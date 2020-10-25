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
 * Alexander Koblov <alexx2000@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbZstdTyp.pas                               *}
{*********************************************************}
{* ABBREVIA: TAbZstdArchive, TAbZstdItem classes         *}
{*********************************************************}
{* Misc. constants, types, and routines for working      *}
{* with zst files                                        *}
{*********************************************************}

unit AbZstdTyp;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbArcTyp, AbTarTyp, AbUtils;

const
  { The first for (4) bytes of the Stream are so called Header }
  { Magic Bytes. They can be used to identify the file type.   }
  AB_ZSTD_FILE_HEADER = #$28#$B5#$2F#$FD;

type
  PAbZstdHeader = ^TAbZstdHeader; { File Header }
  TAbZstdHeader = packed record   { SizeOf(TAbZstdHeader) = 10 }
    MagicNumber : array[0..3] of AnsiChar;{ 28 B5 2F FD }
  end;

{ The Purpose for this Item is the placeholder for aaAdd and aaDelete Support. }
{ For all intents and purposes we could just use a TAbArchiveItem }
type
  TAbZstdItem = class(TabArchiveItem);

  TAbZstdArchiveState = (gsZstd, gsTar);

  TAbZstdArchive = class(TAbTarArchive)
  private
    FZstdStream   : TStream;        { stream for Zstd file}
    FZstdItem     : TAbArchiveList; { item in zstd (only one, but need polymorphism of class)}
    FTarStream    : TStream;        { stream for possible contained Tar }
    FTarList      : TAbArchiveList; { items in possible contained Tar }
    FTarAutoHandle: Boolean;
    FState        : TAbZstdArchiveState;
    FIsZstdTar    : Boolean;

    procedure DecompressToStream(aStream: TStream);
    procedure SetTarAutoHandle(const Value: Boolean);
    procedure SwapToZstd;
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

    property IsZstdTar : Boolean
      read FIsZstdTar write FIsZstdTar;
  end;

function VerifyZstd(Strm : TStream) : TAbArchiveType;

implementation

uses
  SysUtils,
  AbZstd, AbExcept, AbVMStrm, AbBitBkt, DCOSUtils, DCClassesUtf8;

{ ****************** Helper functions Not from Classes Above ***************** }
function VerifyHeader(const Header : TAbZstdHeader) : Boolean;
begin
  Result := CompareByte(Header.MagicNumber, AB_ZSTD_FILE_HEADER, SizeOf(Header.MagicNumber)) = 0;
end;
{ -------------------------------------------------------------------------- }
function VerifyZstd(Strm : TStream) : TAbArchiveType;
var
  Hdr : TAbZstdHeader;
  CurPos, DecompSize : Int64;
  DecompStream, TarStream: TStream;
  Buffer: array[0..Pred(AB_TAR_RECORDSIZE * 4)] of Byte;
begin
  Result := atUnknown;

  CurPos := Strm.Position;
  Strm.Seek(0, soBeginning);

  try
    if (Strm.Read(Hdr, SizeOf(Hdr)) = SizeOf(Hdr)) and VerifyHeader(Hdr) then begin
      Result := atZstd;
      { Check for embedded TAR }
      Strm.Seek(0, soBeginning);
      DecompStream := TZSTDDecompressionStream.Create(Strm);
      try
        TarStream := TMemoryStream.Create;
        try
          DecompSize:= DecompStream.Read(Buffer, SizeOf(Buffer));
          TarStream.Write(Buffer, DecompSize);
          TarStream.Seek(0, soBeginning);
          if VerifyTar(TarStream) = atTar then
            Result := atZstdTar;
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


{ ****************************** TAbZstdArchive ***************************** }
constructor TAbZstdArchive.CreateFromStream(aStream: TStream;
  const aArchiveName: string);
begin
  inherited CreateFromStream(aStream, aArchiveName);
  FState      := gsZstd;
  FZstdStream := FStream;
  FZstdItem   := FItemList;
  FTarStream  := TAbVirtualMemoryStream.Create;
  FTarList    := TAbArchiveList.Create(True);
end;
{ -------------------------------------------------------------------------- }
procedure TAbZstdArchive.SwapToTar;
begin
  FStream   := FTarStream;
  FItemList := FTarList;
  FState    := gsTar;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZstdArchive.SwapToZstd;
begin
  FStream   := FZstdStream;
  FItemList := FZstdItem;
  FState    := gsZstd;
end;
{ -------------------------------------------------------------------------- }
function TAbZstdArchive.CreateItem(const SourceFileName   : string;
                                    const ArchiveDirectory : string): TAbArchiveItem;
var
  ZstdItem : TAbZstdItem;
  FullSourceFileName, FullArchiveFileName: String;
begin
  if IsZstdTar and TarAutoHandle then begin
    SwapToTar;
    Result := inherited CreateItem(SourceFileName, ArchiveDirectory);
  end
  else begin
    SwapToZstd;
    ZstdItem := TAbZstdItem.Create;
    try
      MakeFullNames(SourceFileName, ArchiveDirectory,
                    FullSourceFileName, FullArchiveFileName);

      ZstdItem.FileName := FullArchiveFileName;
      ZstdItem.DiskFileName := FullSourceFileName;

      Result := ZstdItem;
    except
      Result := nil;
      raise;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
destructor TAbZstdArchive.Destroy;
begin
  SwapToZstd;
  FTarList.Free;
  FTarStream.Free;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZstdArchive.ExtractItemAt(Index: Integer;
  const NewName: string);
var
  OutStream : TStream;
begin
  if IsZstdTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemAt(Index, NewName);
  end
  else begin
    SwapToZstd;
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
procedure TAbZstdArchive.ExtractItemToStreamAt(Index: Integer;
  aStream: TStream);
begin
  if IsZstdTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemToStreamAt(Index, aStream);
  end
  else begin
    SwapToZstd;
    { Index ignored as there's only one item in a Bz2 }
    DecompressToStream(aStream);
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbZstdArchive.GetSupportsEmptyFolders : Boolean;
begin
  Result := IsZstdTar and TarAutoHandle;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZstdArchive.LoadArchive;
var
  ItemName: string;
  Item: TAbZstdItem;
  Abort: Boolean = False;
begin
  if FZstdStream.Size = 0 then
    Exit;

  if IsZstdTar and TarAutoHandle then begin
    { Decompress and send to tar LoadArchive }
    DecompressToStream(FTarStream);
    SwapToTar;
    inherited LoadArchive;
  end
  else begin
    SwapToZstd;
    Item := TAbZstdItem.Create;
    Item.Action := aaNone;
    Item.UncompressedSize := ZSTD_FileSize(FZstdStream);
    { Filename isn't stored, so constuct one based on the archive name }
    ItemName := ExtractFileName(ArchiveName);
    if ItemName = '' then
      Item.FileName := 'unknown'
    else
      Item.FileName := ChangeFileExt(ItemName, '');
    Item.DiskFileName := Item.FileName;
    FItemList.Add(Item);
  end;
  DoArchiveProgress(100, Abort);
  FIsDirty := False;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZstdArchive.SaveArchive;
var
  CompStream: TStream;
  i: Integer;
  CurItem: TAbZstdItem;
  UpdateArchive: Boolean;
  TempFileName: String;
  InputFileStream: TStream;
begin
  if IsZstdTar and TarAutoHandle then
  begin
    SwapToTar;
    inherited SaveArchive;
    UpdateArchive := (FZstdStream.Size > 0) and (FZstdStream is TFileStreamEx);
    if UpdateArchive then
    begin
      FreeAndNil(FZstdStream);
      TempFileName := GetTempName(FArchiveName + ExtensionSeparator);
      { Create new archive with temporary name }
      FZstdStream := TFileStreamEx.Create(TempFileName, fmCreate or fmShareDenyWrite);
    end;
    FTarStream.Position := 0;
    CompStream := TZSTDCompressionStream.Create(FZstdStream, 5, FTarStream.Size);
    try
      CompStream.CopyFrom(FTarStream, 0);
    finally
      CompStream.Free;
    end;
    if UpdateArchive then
    begin
      FreeAndNil(FZstdStream);
      { Replace original by new archive }
      if not (mbDeleteFile(FArchiveName) and mbRenameFile(TempFileName, FArchiveName)) then
        RaiseLastOSError;
      { Open new archive }
      FZstdStream := TFileStreamEx.Create(FArchiveName, fmOpenRead or fmShareDenyNone);
    end;
  end
  else begin
    { Things we know: There is only one file per archive.}
    { Actions we have to address in SaveArchive: }
    { aaNone & aaMove do nothing, as the file does not change, only the meta data }
    { aaDelete could make a zero size file unless there are two files in the list.}
    { aaAdd, aaStreamAdd, aaFreshen, & aaReplace will be the only ones to take action. }
    SwapToZstd;
    for i := 0 to pred(Count) do begin
      FCurrentItem := ItemList[i];
      CurItem      := TAbZstdItem(ItemList[i]);
      case CurItem.Action of
        aaNone, aaMove: Break;{ Do nothing; bz2 doesn't store metadata }
        aaDelete: ; {doing nothing omits file from new stream}
        aaAdd, aaFreshen, aaReplace, aaStreamAdd: begin
          FZstdStream.Size := 0;
          if CurItem.Action = aaStreamAdd then
            CurItem.UncompressedSize := InStream.Size
          else begin
            CurItem.UncompressedSize := mbFileSize(CurItem.DiskFileName);
          end;
          CompStream := TZSTDCompressionStream.Create(FZstdStream, 5, CurItem.UncompressedSize);
          try
            if CurItem.Action = aaStreamAdd then
              CompStream.CopyFrom(InStream, 0){ Copy/compress entire Instream to FZstdStream }
            else begin
              InputFileStream := TFileStreamEx.Create(CurItem.DiskFileName, fmOpenRead or fmShareDenyWrite );
              try
                CompStream.CopyFrom(InputFileStream, 0);{ Copy/compress entire Instream to FZstdStream }
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
procedure TAbZstdArchive.SetTarAutoHandle(const Value: Boolean);
begin
  if Value then
    SwapToTar
  else
    SwapToZstd;
  FTarAutoHandle := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZstdArchive.DecompressToStream(aStream: TStream);
const
  BufSize = $F000;
var
  DecompStream: TZSTDDecompressionStream;
  Buffer: PByte;
  N: Integer;
begin
  DecompStream := TZSTDDecompressionStream.Create(FZstdStream);
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
procedure TAbZstdArchive.TestItemAt(Index: Integer);
var
  ZstdType: TAbArchiveType;
  BitBucket: TAbBitBucketStream;
begin
  if IsZstdTar and TarAutoHandle then begin
    SwapToTar;
    inherited TestItemAt(Index);
  end
  else begin
    { note Index ignored as there's only one item in a GZip }
    ZstdType := VerifyZstd(FZstdStream);
    if not (ZstdType in [atZstd, atZstdTar]) then
      raise EAbGzipInvalid.Create; // TODO: Add zstd-specific exceptions }
    BitBucket := TAbBitBucketStream.Create(1024);
    try
      DecompressToStream(BitBucket);
    finally
      BitBucket.Free;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbZstdArchive.DoSpanningMediaRequest(Sender: TObject;
  ImageNumber: Integer; var ImageName: string; var Abort: Boolean);
begin
  Abort := False;
end;

end.
