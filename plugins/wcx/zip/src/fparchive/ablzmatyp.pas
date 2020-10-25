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
{* ABBREVIA: AbLzmaTyp.pas                               *}
{*********************************************************}
{* ABBREVIA: TAbLzmaArchive, TAbLzmaItem classes         *}
{*********************************************************}
{* Misc. constants, types, and routines for working      *}
{* with Lzma files                                       *}
{*********************************************************}

unit AbLzmaTyp;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbArcTyp, AbTarTyp, AbUtils;

type
  PAbLzmaHeader = ^TAbLzmaHeader; { File Header }
  TAbLzmaHeader = packed record   { SizeOf(TAbLzmaHeader) = 13 }
    Properties: array[0..4] of Byte; { LZMA properties }
    UncompressedSize : Int64;        { Uncompressed size }
  end;

{ The Purpose for this Item is the placeholder for aaAdd and aaDelete Support. }
{ For all intents and purposes we could just use a TAbArchiveItem }
type
  TAbLzmaItem = class(TabArchiveItem);

  TAbLzmaArchiveState = (gsLzma, gsTar);

  { TAbLzmaArchive }

  TAbLzmaArchive = class(TAbTarArchive)
  private
    FLzmaStream   : TStream;        { stream for Lzma file}
    FLzmaItem     : TAbArchiveList; { item in lzma (only one, but need polymorphism of class)}
    FTarStream    : TStream;        { stream for possible contained Tar }
    FTarList      : TAbArchiveList; { items in possible contained Tar }
    FTarAutoHandle: Boolean;
    FState        : TAbLzmaArchiveState;
    FIsLzmaTar    : Boolean;

    procedure CompressFromStream(aStream: TStream);
    procedure DecompressToStream(aStream: TStream);
    procedure SetTarAutoHandle(const Value: Boolean);
    procedure SwapToLzma;
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

    property IsLzmaTar : Boolean
      read FIsLzmaTar write FIsLzmaTar;
  end;

function VerifyLzma(Strm : TStream) : TAbArchiveType;

implementation

uses
  StrUtils, SysUtils, Math,
  AbExcept, AbVMStrm, AbBitBkt, ULZMADecoder, ULZMAEncoder, DCOSUtils, DCClassesUtf8;

{ ****************** Helper functions Not from Classes Above ***************** }
function VerifyLzma(Strm : TStream) : TAbArchiveType;
var
  CurPos : Int64;
  TarStream: TStream;
  Hdr : TAbLzmaHeader;
  UncompressedSize: Int64;
  DecompStream: TLZMADecoder;
begin
  Result := atUnknown;

  CurPos := Strm.Position;
  Strm.Seek(0, soBeginning);

  try
    if Strm.Read(Hdr, SizeOf(Hdr)) = SizeOf(Hdr) then
    begin
      TarStream := TMemoryStream.Create;
      try
        DecompStream := TLZMADecoder.Create;
        try
          if Hdr.UncompressedSize <> -1 then
            UncompressedSize:= Min(AB_TAR_RECORDSIZE * 4, Hdr.UncompressedSize)
          else if Strm.Size < AB_TAR_RECORDSIZE * 8 then
            UncompressedSize:= -1
          else begin
            UncompressedSize:= AB_TAR_RECORDSIZE * 4;
          end;
          if DecompStream.SetDecoderProperties(Hdr.Properties) and
             DecompStream.Code(Strm, TarStream, UncompressedSize) then
          begin
            Result := atLzma;
            { Check for embedded TAR }
            TarStream.Seek(0, soBeginning);
            if VerifyTar(TarStream) = atTar then
              Result := atLzmaTar;
          end;
        finally
          DecompStream.Free;
        end;
      finally
       TarStream.Free;
      end;
    end;
  except
    on EReadError do
      Result := atUnknown;
  end;
  Strm.Position := CurPos; { Return to original position. }
end;


{ ****************************** TAbLzmaArchive ***************************** }
constructor TAbLzmaArchive.CreateFromStream(aStream: TStream;
  const aArchiveName: string);
begin
  inherited CreateFromStream(aStream, aArchiveName);
  FState       := gsLzma;
  FLzmaStream  := FStream;
  FLzmaItem    := FItemList;
  FTarStream   := TAbVirtualMemoryStream.Create;
  FTarList     := TAbArchiveList.Create(True);
end;
{ -------------------------------------------------------------------------- }
procedure TAbLzmaArchive.SwapToTar;
begin
  FStream   := FTarStream;
  FItemList := FTarList;
  FState    := gsTar;
end;
{ -------------------------------------------------------------------------- }
procedure TAbLzmaArchive.SwapToLzma;
begin
  FStream   := FLzmaStream;
  FItemList := FLzmaItem;
  FState    := gsLzma;
end;
{ -------------------------------------------------------------------------- }
function TAbLzmaArchive.CreateItem(const SourceFileName   : string;
                                    const ArchiveDirectory : string): TAbArchiveItem;
var
  LzmaItem : TAbLzmaItem;
  FullSourceFileName, FullArchiveFileName: String;
begin
  if IsLzmaTar and TarAutoHandle then begin
    SwapToTar;
    Result := inherited CreateItem(SourceFileName, ArchiveDirectory);
  end
  else begin
    SwapToLzma;
    LzmaItem := TAbLzmaItem.Create;
    try
      MakeFullNames(SourceFileName, ArchiveDirectory,
                    FullSourceFileName, FullArchiveFileName);

      LzmaItem.FileName := FullArchiveFileName;
      LzmaItem.DiskFileName := FullSourceFileName;

      Result := LzmaItem;
    except
      Result := nil;
      raise;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
destructor TAbLzmaArchive.Destroy;
begin
  SwapToLzma;
  FTarList.Free;
  FTarStream.Free;
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbLzmaArchive.ExtractItemAt(Index: Integer;
  const NewName: string);
var
  OutStream : TStream;
begin
  if IsLzmaTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemAt(Index, NewName);
  end
  else begin
    SwapToLzma;
    OutStream := TFileStreamEx.Create(NewName, fmCreate or fmShareDenyNone);
    try
      try
        ExtractItemToStreamAt(Index, OutStream);
      finally
        OutStream.Free;
      end;
      { Lzma doesn't store the last modified time or attributes, so don't set them }
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
procedure TAbLzmaArchive.ExtractItemToStreamAt(Index: Integer;
  aStream: TStream);
begin
  if IsLzmaTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemToStreamAt(Index, aStream);
  end
  else begin
    SwapToLzma;
    { Index ignored as there's only one item in a Lzma }
    DecompressToStream(aStream);
  end;
end;
{ -------------------------------------------------------------------------- }
function TAbLzmaArchive.GetSupportsEmptyFolders : Boolean;
begin
  Result := IsLzmaTar and TarAutoHandle;
end;
{ -------------------------------------------------------------------------- }
procedure TAbLzmaArchive.LoadArchive;
var
  Item: TAbLzmaItem;
  Abort: Boolean;
  ItemName: string;
  Header: TAbLzmaHeader;
begin
  if FLzmaStream.Size = 0 then
    Exit;

  if IsLzmaTar and TarAutoHandle then begin
    { Decompress and send to tar LoadArchive }
    DecompressToStream(FTarStream);
    SwapToTar;
    inherited LoadArchive;
  end
  else begin
    SwapToLzma;
    FStream.Read(Header, SizeOf(Header));
    Item := TAbLzmaItem.Create;
    Item.Action := aaNone;
    if Header.UncompressedSize <> -1 then
      Item.UncompressedSize := Header.UncompressedSize;
    { Filename isn't stored, so constuct one based on the archive name }
    ItemName := ExtractFileName(ArchiveName);
    if ItemName = '' then
      Item.FileName := 'unknown'
    else if AnsiEndsText('.tlz', ItemName) then
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
procedure TAbLzmaArchive.SaveArchive;
var
  I: Integer;
  CurItem: TAbLzmaItem;
  UpdateArchive: Boolean;
  TempFileName: String;
  InputFileStream: TStream;
begin
  if IsLzmaTar and TarAutoHandle then
  begin
    SwapToTar;
    inherited SaveArchive;
    UpdateArchive := (FLzmaStream.Size > 0) and (FLzmaStream is TFileStreamEx);
    if UpdateArchive then
    begin
      FreeAndNil(FLzmaStream);
      TempFileName := GetTempName(FArchiveName + ExtensionSeparator);
      { Create new archive with temporary name }
      FLzmaStream := TFileStreamEx.Create(TempFileName, fmCreate or fmShareDenyWrite);
    end;
    FTarStream.Position := 0;
    CompressFromStream(FTarStream);
    if UpdateArchive then
    begin
      FreeAndNil(FLzmaStream);
      { Replace original by new archive }
      if not (mbDeleteFile(FArchiveName) and mbRenameFile(TempFileName, FArchiveName)) then
        RaiseLastOSError;
      { Open new archive }
      FLzmaStream := TFileStreamEx.Create(FArchiveName, fmOpenRead or fmShareDenyNone);
    end;
  end
  else begin
    { Things we know: There is only one file per archive.}
    { Actions we have to address in SaveArchive: }
    { aaNone & aaMove do nothing, as the file does not change, only the meta data }
    { aaDelete could make a zero size file unless there are two files in the list.}
    { aaAdd, aaStreamAdd, aaFreshen, & aaReplace will be the only ones to take action. }
    SwapToLzma;
    for I := 0 to Pred(Count) do begin
      FCurrentItem := ItemList[I];
      CurItem      := TAbLzmaItem(ItemList[I]);
      case CurItem.Action of
        aaNone, aaMove: Break;{ Do nothing; lzma doesn't store metadata }
        aaDelete: ; {doing nothing omits file from new stream}
        aaAdd, aaFreshen, aaReplace, aaStreamAdd: begin
          FLzmaStream.Size := 0;
          if CurItem.Action = aaStreamAdd then
          begin
            CompressFromStream(InStream); { Copy/compress entire Instream to FLzmaStream }
          end
          else begin
            InputFileStream := TFileStreamEx.Create(CurItem.DiskFileName, fmOpenRead or fmShareDenyWrite);
            try
              CompressFromStream(InputFileStream); { Copy/compress entire Instream to FLzmaStream }
            finally
              InputFileStream.Free;
            end;
          end;
          Break;
        end; { End aaAdd, aaFreshen, aaReplace, & aaStreamAdd }
      end; { End of CurItem.Action Case }
    end; { End Item for loop }
  end; { End Tar Else }
end;
{ -------------------------------------------------------------------------- }
procedure TAbLzmaArchive.SetTarAutoHandle(const Value: Boolean);
begin
  if Value then
    SwapToTar
  else
    SwapToLzma;
  FTarAutoHandle := Value;
end;
{ -------------------------------------------------------------------------- }
procedure TAbLzmaArchive.CompressFromStream(aStream: TStream);
var
  Encoder: TLZMAEncoder;
begin
  Encoder := TLZMAEncoder.Create;
  try
    Encoder.WriteCoderProperties(FLzmaStream);
    FLzmaStream.WriteQWord(NToLE(aStream.Size));
    Encoder.Code(aStream, FLzmaStream, -1, -1);
  finally
    Encoder.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbLzmaArchive.DecompressToStream(aStream: TStream);
var
  Header: TAbLzmaHeader;
  Decoder: TLZMADecoder;
begin
  FLzmaStream.Seek(0, soBeginning);
  if FLzmaStream.Read(Header, SizeOf(Header)) = SizeOf(Header) then
  begin
    Decoder := TLZMADecoder.Create;
    try
      if Decoder.SetDecoderProperties(Header.Properties) and
         Decoder.Code(FLzmaStream, aStream, Header.UncompressedSize) then
      begin
        Exit; { Success }
      end;
    finally
      Decoder.Free;
    end;
  end;
  raise EAbUnhandledType.Create;
end;
{ -------------------------------------------------------------------------- }
procedure TAbLzmaArchive.TestItemAt(Index: Integer);
var
  LzmaType: TAbArchiveType;
  BitBucket: TAbBitBucketStream;
begin
  if IsLzmaTar and TarAutoHandle then begin
    SwapToTar;
    inherited TestItemAt(Index);
  end
  else begin
    { Note Index ignored as there's only one item in a GZip }
    LzmaType := VerifyLzma(FLzmaStream);
    if not (LzmaType in [atLzma, atLzmaTar]) then
      raise EAbGzipInvalid.Create; // TODO: Add lzma-specific exceptions }
    BitBucket := TAbBitBucketStream.Create(1024);
    try
      DecompressToStream(BitBucket);
    finally
      BitBucket.Free;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbLzmaArchive.DoSpanningMediaRequest(Sender: TObject;
  ImageNumber: Integer; var ImageName: string; var Abort: Boolean);
begin
  Abort := False;
end;

end.
