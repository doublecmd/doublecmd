(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * Joel Haynie
 * Craig Peterson
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbBzip2Typ.pas                              *}
{*********************************************************}
{* ABBREVIA: TAbBzip2Archive, TAbBzip2Item classes       *}
{*********************************************************}
{* Misc. constants, types, and routines for working      *}
{* with Bzip2 files                                      *}
{*********************************************************}

unit AbBzip2Typ;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbArcTyp, AbTarTyp, AbUtils;

const
  { Default Stream Header for Bzip2s is 'BZhX', where X is the block size setting 1-9 in ASCII }
  { Each block has the following header: '1AY&SY', and are in units of 100kilobytes NOT 100kibiBytes }
  AB_BZIP2_FILE_HEADER  = 'BZh';
  AB_BZIP2_BLOCK_SIZE   = ['1','2','3','4','5','6','7','8','9'];
  AB_BZIP2_BLOCK_HEADER = '1AY&SY'; { Note: $314159265359, BCD for Pi :) }
  { Note that Blocks are bit aligned, as such the only time you will "for sure" see
    the block header is on the start of stream/File }
  AB_BZIP2_FILE_TAIL =#23#114#36#83#133#9#0; { $1772245385090, BCD for sqrt(Pi) :) }
  { This is odd as the blocks are bit allgned so this is a string that is 13*4 bits = 52 bits }

type
  PAbBzip2Header = ^TAbBzip2Header; { File Header }
  TAbBzip2Header = packed record  { SizeOf(TAbBzip2Header) = 10 }
    FileHeader  : array[0..2] of AnsiChar;{ 'BZh';    $42,5A,68 }
    BlockSize   : AnsiChar;               { '1'..'9'; $31-$39 }
    BlockHeader : array[0..5] of AnsiChar;{ '1AY&SY'; $31,41,59,26,53,59 }
  end;

{ The Purpose for this Item is the placeholder for aaAdd and aaDelete Support. }
{ For all intents and purposes we could just use a TAbArchiveItem }
type
  TAbBzip2Item = class(TabArchiveItem);

  TAbBzip2ArchiveState = (gsBzip2, gsTar);

  TAbBzip2Archive = class(TAbTarArchive)
  private
    FBzip2Stream  : TStream;        { stream for Bzip2 file}
    FBzip2Item    : TAbArchiveList; { item in bzip2 (only one, but need polymorphism of class)}
    FTarStream    : TStream;        { stream for possible contained Tar }
    FTarList      : TAbArchiveList; { items in possible contained Tar }
    FTarAutoHandle: Boolean;
    FState        : TAbBzip2ArchiveState;
    FIsBzippedTar : Boolean;

    procedure DecompressToStream(aStream: TStream);
    procedure SetTarAutoHandle(const Value: Boolean);
    procedure SwapToBzip2;
    procedure SwapToTar;

  protected
    { Inherited Abstract functions }
    function CreateItem(const SourceFileName   : string;
                        const ArchiveDirectory : string): TAbArchiveItem; override;
    procedure ExtractItemAt(Index : Integer; const NewName : string); override;
    procedure ExtractItemToStreamAt(Index : Integer; aStream : TStream); override;
    procedure LoadArchive; override;
    procedure SaveArchive; override;
    procedure TestItemAt(Index : Integer); override;
    function GetSupportsEmptyFolders : Boolean; override;

  public {methods}
    constructor CreateFromStream(aStream : TStream; const aArchiveName : string); override;
    destructor  Destroy; override;

    procedure DoSpanningMediaRequest(Sender : TObject; ImageNumber : Integer;
      var ImageName : string; var Abort : Boolean); override;

    { Properties }
    property TarAutoHandle : Boolean
      read FTarAutoHandle write SetTarAutoHandle;

    property IsBzippedTar : Boolean
      read FIsBzippedTar write FIsBzippedTar;
  end;

function VerifyBzip2(Strm : TStream) : TAbArchiveType;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, // Fix inline warnings
{$ENDIF}
  StrUtils, SysUtils,
  AbBzip2, AbExcept, AbVMStrm, AbBitBkt, DCOSUtils, DCClassesUtf8;

{ ****************** Helper functions Not from Classes Above ***************** }
function VerifyHeader(const Header : TAbBzip2Header) : Boolean;
begin
  Result := (Header.FileHeader = AB_BZIP2_FILE_HEADER) and
            (Header.BlockSize in AB_BZIP2_BLOCK_SIZE)  and
            (Header.BlockHeader = AB_BZIP2_BLOCK_HEADER);
end;
{ -------------------------------------------------------------------------- }
function VerifyBzip2(Strm : TStream) : TAbArchiveType;
var
  Hdr : TAbBzip2Header;
  CurPos : int64;
  DecompStream, TarStream: TStream;
begin
  Result := atUnknown;

  CurPos := Strm.Position;
  Strm.Seek(0, soFromBeginning);

  try
    if (Strm.Read(Hdr, SizeOf(Hdr)) = SizeOf(Hdr)) and VerifyHeader(Hdr) then begin
      Result := atBzip2;
      { Check for embedded TAR }
      Strm.Seek(0, soFromBeginning);
      DecompStream := TBZDecompressionStream.Create(Strm);
      try
        TarStream := TMemoryStream.Create;
        try
          TarStream.CopyFrom(DecompStream, 512 * 2);
          TarStream.Seek(0, soFromBeginning);
          if VerifyTar(TarStream) = atTar then
            Result := atBzippedTar;
        finally
          TarStream.Free;
        end;
      finally
        DecompStream.Free;
      end;
    end;
  except
    on EReadError do
      Result := atUnknown;
  end;
  Strm.Position := CurPos; { Return to original position. }
end;


{ ****************************** TAbBzip2Archive ***************************** }
constructor TAbBzip2Archive.CreateFromStream(aStream: TStream;
  const aArchiveName: string);
begin
  inherited CreateFromStream(aStream, aArchiveName);
  FState       := gsBzip2;
  FBzip2Stream := FStream;
  FBzip2Item   := FItemList;
  FTarStream   := TAbVirtualMemoryStream.Create;
  FTarList     := TAbArchiveList.Create(True);
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2Archive.SwapToTar;
begin
  FStream   := FTarStream;
  FItemList := FTarList;
  FState    := gsTar;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2Archive.SwapToBzip2;
begin
  FStream   := FBzip2Stream;
  FItemList := FBzip2Item;
  FState    := gsBzip2;
end;
{ -------------------------------------------------------------------------- }
function TAbBzip2Archive.CreateItem(const SourceFileName   : string;
                                    const ArchiveDirectory : string): TAbArchiveItem;
var
  Bz2Item : TAbBzip2Item;
  FullSourceFileName, FullArchiveFileName: String;
begin
  if IsBzippedTar and TarAutoHandle then begin
    SwapToTar;
    Result := inherited CreateItem(SourceFileName, ArchiveDirectory);
  end
  else begin
    SwapToBzip2;
    Bz2Item := TAbBzip2Item.Create;
    try
      MakeFullNames(SourceFileName, ArchiveDirectory,
                    FullSourceFileName, FullArchiveFileName);

      Bz2Item.FileName := FullArchiveFileName;
      Bz2Item.DiskFileName := FullSourceFileName;

      Result := Bz2Item;
    except
      Result := nil;
      raise;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
destructor TAbBzip2Archive.Destroy;
begin
  SwapToBzip2;
  FTarList.Free;
  FTarStream.Free;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2Archive.ExtractItemAt(Index: Integer;
  const NewName: string);
var
  OutStream : TStream;
begin
  if IsBzippedTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemAt(Index, NewName);
  end
  else begin
    SwapToBzip2;
    OutStream := TFileStreamEx.Create(NewName, fmCreate or fmShareDenyNone);
    try
      try
        ExtractItemToStreamAt(Index, OutStream);
      finally
        OutStream.Free;
      end;
      { Bz2 doesn't store the last modified time or attributes, so don't set them }
    except
      on E : EAbUserAbort do begin
        FStatus := asInvalid;
        if mbFileExists(NewName) then
          mbDeleteFile(NewName);
        raise;
      end else begin
        if mbFileExists(NewName) then
          mbDeleteFile(NewName);
        raise;
      end;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2Archive.ExtractItemToStreamAt(Index: Integer;
  aStream: TStream);
begin
  if IsBzippedTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemToStreamAt(Index, aStream);
  end
  else begin
    SwapToBzip2;
    { Index ignored as there's only one item in a Bz2 }
    DecompressToStream(aStream);
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbBzip2Archive.GetSupportsEmptyFolders : Boolean;
begin
  Result := IsBzippedTar and TarAutoHandle;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2Archive.LoadArchive;
var
  Item: TAbBzip2Item;
  Abort: Boolean;
  ItemName: string;
begin
  if FBzip2Stream.Size = 0 then
    Exit;

  if IsBzippedTar and TarAutoHandle then begin
    { Decompress and send to tar LoadArchive }
    DecompressToStream(FTarStream);
    SwapToTar;
    inherited LoadArchive;
  end
  else begin
    SwapToBzip2;
    Item := TAbBzip2Item.Create;
    Item.Action := aaNone;
    { Filename isn't stored, so constuct one based on the archive name }
    ItemName := ExtractFileName(ArchiveName);
    if ItemName = '' then
      Item.FileName := 'unknown'
    else if AnsiEndsText('.tbz', ItemName) or AnsiEndsText('.tbz2', ItemName) then
      Item.FileName := ChangeFileExt(ItemName, '.tar')
    else
      Item.FileName := ChangeFileExt(ItemName, '');
    Item.DiskFileName := Item.FileName;
    FItemList.Add(Item);
  end;
  DoArchiveProgress(100, Abort);
  FIsDirty := False;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2Archive.SaveArchive;
var
  CompStream: TStream;
  i: Integer;
  CurItem: TAbBzip2Item;
  InputFileStream: TStream;
begin
  if IsBzippedTar and TarAutoHandle then
  begin
    SwapToTar;
    inherited SaveArchive;
    FTarStream.Position := 0;
    FBzip2Stream.Size := 0;
    CompStream := TBZCompressionStream.Create(bs9, FBzip2Stream);
    try
      CompStream.CopyFrom(FTarStream, 0);
    finally
      CompStream.Free;
    end;
  end
  else begin
    { Things we know: There is only one file per archive.}
    { Actions we have to address in SaveArchive: }
    { aaNone & aaMove do nothing, as the file does not change, only the meta data }
    { aaDelete could make a zero size file unless there are two files in the list.}
    { aaAdd, aaStreamAdd, aaFreshen, & aaReplace will be the only ones to take action. }
    SwapToBzip2;
    for i := 0 to pred(Count) do begin
      FCurrentItem := ItemList[i];
      CurItem      := TAbBzip2Item(ItemList[i]);
      case CurItem.Action of
        aaNone, aaMove: Break;{ Do nothing; bz2 doesn't store metadata }
        aaDelete: ; {doing nothing omits file from new stream}
        aaAdd, aaFreshen, aaReplace, aaStreamAdd: begin
          FBzip2Stream.Size := 0;
          CompStream := TBZCompressionStream.Create(bs9, FBzip2Stream);
          try
            if CurItem.Action = aaStreamAdd then
              CompStream.CopyFrom(InStream, 0){ Copy/compress entire Instream to FBzip2Stream }
            else begin
              InputFileStream := TFileStreamEx.Create(CurItem.DiskFileName, fmOpenRead or fmShareDenyWrite );
              try
                CompStream.CopyFrom(InputFileStream, 0);{ Copy/compress entire Instream to FBzip2Stream }
              finally
                InputFileStream.Free;
              end;
            end;
          finally
            CompStream.Free;
          end;
          Break;
        end; { End aaAdd, aaFreshen, aaReplace, & aaStreamAdd }
      end; { End of CurItem.Action Case }
    end; { End Item for loop }
  end; { End Tar Else }
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2Archive.SetTarAutoHandle(const Value: Boolean);
begin
  if Value then
    SwapToTar
  else
    SwapToBzip2;
  FTarAutoHandle := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2Archive.DecompressToStream(aStream: TStream);
const
  BufSize = $F000;
var
  DecompStream: TBZDecompressionStream;
  Buffer: PByte;
  N: Integer;
begin
  DecompStream := TBZDecompressionStream.Create(FBzip2Stream);
  try
    GetMem(Buffer, BufSize);
    try
      N := DecompStream.Read(Buffer^, BufSize);
      while N > 0 do begin
        aStream.WriteBuffer(Buffer^, N);
        N := DecompStream.Read(Buffer^, BufSize);
      end;
    finally
      FreeMem(Buffer, BufSize);
    end;
  finally
    DecompStream.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2Archive.TestItemAt(Index: Integer);
var
  Bzip2Type: TAbArchiveType;
  BitBucket: TAbBitBucketStream;
begin
  if IsBzippedTar and TarAutoHandle then begin
    SwapToTar;
    inherited TestItemAt(Index);
  end
  else begin
    { note Index ignored as there's only one item in a GZip }
    Bzip2Type := VerifyBzip2(FBzip2Stream);
    if not (Bzip2Type in [atBzip2, atBzippedTar]) then
      raise EAbGzipInvalid.Create;// TODO: Add bzip2-specific exceptions }
    BitBucket := TAbBitBucketStream.Create(1024);
    try
      DecompressToStream(BitBucket);
    finally
      BitBucket.Free;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbBzip2Archive.DoSpanningMediaRequest(Sender: TObject;
  ImageNumber: Integer; var ImageName: string; var Abort: Boolean);
begin
  Abort := False;
end;

end.
