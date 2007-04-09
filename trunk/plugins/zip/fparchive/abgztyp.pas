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
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbGzTyp.pas 3.04                            *}
{*********************************************************}
{* ABBREVIA: TAbGzipArchive, TAbGzipItem classes         *}
{*********************************************************}
{* Misc. constants, types, and routines for working      *}
{* with GZip files                                       *}
{* See: RFC 1952                                         *}
{* "GZIP file format specification version 4.3"          *}
{* for more information on GZip                          *}
{*********************************************************}

{$I AbDefine.inc}

unit AbGzTyp;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  {$IFNDEF NOQT}
  QDialogs,
  {$ENDIF}
  {$ENDIF}
  SysUtils, Classes,

  AbConst, AbExcept, AbUtils, AbArcTyp, AbTarTyp, 
  AbDfBase, AbDfDec, AbDfEnc, AbVMStrm, AbBitBkt, AbSpanSt;

type
  { pre-defined "operating system" (really more FILE system)
    types for the Gzip header }
  TAbGzFileSystem =
    (osFat, osAmiga, osVMS, osUnix, osVM_CMS, osAtariTOS,
    osHPFS, osMacintosh, osZSystem, osCP_M, osTOPS20,
    osNTFS, osQDOS, osAcornRISCOS, osUnknown, osUndefined);

type
  PAbGzHeader = ^TAbGzHeader;
  TAbGzHeader = packed record  { SizeOf(TGzHeader) = 10}
    ID1        : Byte;  { ID Byte, should always be $1F}
    ID2        : Byte;  { ID Byte, should always be $8B}
    CompMethod : Byte;  { compression method used}
    { 0..7 reserved, 8 = deflate, others undefined as of this writing (4/27/2001)}
    Flags      : Byte; { misc flags}
      { Bit 0: FTEXT    compressed file contains text, can be used for}
                      { cross platform line termination translation}
      { Bit 1: FHCRC    header data includes CRC16 for header after}
                      { Extra Data, FileName, and Comment, if any}
      { Bit 2: FEXTRA   header data contains Extra Data, starts immediately}
                      { after header proper}
      { Bit 3: FNAME    header data contains FileName, null terminated}
                      { string starting immediately after Extra Data (if any)}
      { Bit 4: FCOMMENT header data contains Comment, null terminated string}
                      { starting immediately after FileName (if any)}
      { Bits 5..7 are undefined and reserved as of this writing (4/27/2001)}
    ModTime    : LongInt; { File Modification (Creation) time,}
                          { UNIX cdate format}
    XtraFlags  : Byte;   { additional flags}
      { XtraFlags = 2  -- Deflate compressor used maximum compression algorithm}
      { XtraFlags = 4  -- Deflate compressor used fastest algorithm}
    OS         : Byte; { Operating system that created file,}
                       { see GZOsToStr routine for values}
  end;

  TAbGzTailRec = packed record
    CRC32 : LongInt;  { crc for uncompressed data }
    ISize : LongInt;  { size of uncompressed data }
  end;

type
  TAbGzipItem = class(TAbArchiveItem)
  private
    FCRC32: LongInt;
  protected {private}
    FGZHeader : TAbGzHeader;
    FIsText : Boolean;
    FCRC16 : ShortInt;
    FXLen  : ShortInt;
    FExtraField, FFileComment : string;
    FIncludeHeaderCrc: Boolean;

  protected
    function GetExtraField: string;
    function GetFileSystem: TAbGzFileSystem;
    function GetFileComment: string;
    function GetHeaderCRC: Word;
    function GetHasExtraField: Boolean;
    function GetHasFileComment: Boolean;
    function GetHasHeaderCRC: Boolean;
    function GetHasFileName: Boolean;
    function GetIsText: Boolean;

    procedure SetExtraField(const Value: string);
    procedure SetFileComment(Value : string);
    procedure SetFileSystem(const Value: TAbGzFileSystem);
    procedure SetIsText(const Value: Boolean);

    function GetCompressedSize : LongInt; override;
    function GetExternalFileAttributes : LongInt; override;
    function GetIsEncrypted : Boolean; override;
    function GetLastModFileDate : Word; override;
    function GetLastModFileTime : Word; override;
    function GetUncompressedSize : LongInt; override;

    procedure SetCompressedSize(const Value : LongInt); override;
    procedure SetExternalFileAttributes( Value : LongInt ); override;
    procedure SetFileName(Value : string); override;
    procedure SetIsEncrypted(Value : Boolean); override;
    procedure SetLastModFileDate(const Value : Word); override;
    procedure SetLastModFileTime(const Value : Word); override;
    procedure SetUncompressedSize(const Value : LongInt); override;

    procedure SaveGzHeaderToStream(AStream : TStream);
    procedure LoadGzHeaderFromStream(AStream : TStream);
  public
    property CRC32 : LongInt
      read FCRC32 write FCRC32;

    property CompressionMethod : Byte
      read FGZHeader.CompMethod write FGZHeader.CompMethod;

    property ExtraFlags : Byte {Default: 2}
      read FGZHeader.XtraFlags write FGZHeader.XtraFlags;

    property Flags : Byte
      read FGZHeader.Flags write FGZHeader.Flags;

    property FileComment : string
      read GetFileComment write SetFileComment;

    property FileSystem : TAbGzFileSystem {Default: osFat (Windows); osUnix (Linux)}
      read GetFileSystem write SetFileSystem;

    property ExtraField : string
      read GetExtraField write SetExtraField;

    property HeaderCRC : Word
      read GetHeaderCRC;

    property IsEncrypted : Boolean
      read GetIsEncrypted;

    property HasExtraField : Boolean
      read GetHasExtraField;

    property HasFileName : Boolean
      read GetHasFileName;

    property HasFileComment : Boolean
      read GetHasFileComment;

    property HasHeaderCRC : Boolean
      read GetHasHeaderCRC;

    property IsText : Boolean
      read GetIsText write SetIsText;

    property GZHeader : TAbGzHeader
      read FGZHeader write FGZHeader;

    property IncludeHeaderCrc : Boolean
      read FIncludeHeaderCrc write FIncludeHeaderCrc;

    constructor Create;
  end;

  TAbGzipStreamHelper = class(TAbArchiveStreamHelper)
  private
    function GetGzCRC: LongInt;
    function GetFileSize: LongInt;
  protected {private}
    FItem : TAbGzipItem;
    FTail : TAbGzTailRec;
  public
    constructor Create(AStream : TStream);
    destructor Destroy; override;

    procedure ExtractItemData(AStream : TStream); override;
    function FindFirstItem : Boolean; override;
    function FindNextItem : Boolean; override;
    function SeekItem(Index : Integer): Boolean; override;
    procedure SeekToItemData;
    procedure WriteArchiveHeader; override;
    procedure WriteArchiveItem(AStream : TStream); override;
    procedure WriteArchiveTail; override;
    function GetItemCount : Integer; override;
    procedure ReadHeader; override;
    procedure ReadTail; override;

    property CRC : LongInt
      read GetGzCRC;
    property FileSize : LongInt
      read GetFileSize;
    property TailCRC : LongInt
      read FTail.CRC32;
    property TailSize : LongInt
      read FTail.ISize;
  end;

  TAbGzipArchiveState = (gsGzip, gsTar);

  TAbGzipArchive = class(TAbTarArchive)
  private
    FGZStream  : TStream;        { stream for GZip file}
    FGZItem    : TAbArchiveList; { item in Gzip (only one, but need polymorphism of class)}
    FTarStream : TStream;        { stream for possible contained Tar }
    FTarList   : TAbArchiveList; { items in possible contained Tar }
    FTarAutoHandle: Boolean;
    FTarLoaded : Boolean;
    FState     : TAbGzipArchiveState;
    FIsGzippedTar : Boolean;

    procedure SetTarAutoHandle(const Value: Boolean);
    function GetIsGzippedTar: Boolean;
    procedure SwapToGzip;
    procedure SwapToTar;

  protected
    function CreateItem(const FileSpec : string): TAbArchiveItem;
      override;
    procedure ExtractItemAt(Index : Integer; const NewName : string);
      override;
    procedure ExtractItemToStreamAt(Index : Integer; aStream : TStream);
      override;
    procedure LoadArchive;
      override;
    procedure SaveArchive;
      override;
    procedure TestItemAt(Index : Integer);
      override;
    function FixName(Value : string) : string;
      override;

    function GetItem(Index: Integer): TAbGzipItem;                  {!!.03}
    procedure PutItem(Index: Integer; const Value: TAbGzipItem);    {!!.03}
  public {methods}
    constructor Create(FileName : string; Mode : Word);
      override;
    destructor  Destroy;
      override;

    procedure DoSpanningMediaRequest(Sender : TObject; ImageNumber : Integer;
      var ImageName : string; var Abort : Boolean); override;

    property TarAutoHandle : Boolean
      read FTarAutoHandle write SetTarAutoHandle;

    property IsGzippedTar : Boolean
      read GetIsGzippedTar write FIsGzippedTar;

    property Items[Index : Integer] : TAbGzipItem                    {!!.03}
      read GetItem                                                   {!!.03}
      write PutItem; default;                                        {!!.03}
  end;

function VerifyGZip(Strm : TStream) : TAbArchiveType;
function GZOsToStr(OS: Byte) : string;

implementation

const
  { Header Signature Values}
  AB_GZ_HDR_ID1 = $1F;
  AB_GZ_HDR_ID2 = $8B;

  { Test bits for TGzHeader.Flags field }
  AB_GZ_FLAG_FTEXT    = $01;
  AB_GZ_FLAG_FHCRC    = $02;
  AB_GZ_FLAG_FEXTRA   = $04;
  AB_GZ_FLAG_FNAME    = $08;
  AB_GZ_FLAG_FCOMMENT = $10;

  { GZip OS source flags }
  AB_GZ_OS_ID_FAT         = 0;
  AB_GZ_OS_ID_Amiga       = 1;
  AB_GZ_OS_ID_VMS         = 2;
  AB_GZ_OS_ID_Unix        = 3;
  AB_GZ_OS_ID_VM_CMS      = 4;
  AB_GZ_OS_ID_AtariTOS    = 5;
  AB_GZ_OS_ID_HPFS        = 6;
  AB_GZ_OS_ID_Macintosh   = 7;
  AB_GZ_OS_ID_Z_System    = 8;
  AB_GZ_OS_ID_CP_M        = 9;
  AB_GZ_OS_ID_TOPS20      = 10;
  AB_GZ_OS_ID_NTFS        = 11;
  AB_GZ_OS_ID_QDOS        = 12;
  AB_GZ_OS_ID_AcornRISCOS = 13;
  AB_GZ_OS_ID_unknown     = 255;

function GZOsToStr(OS: Byte) : string;
{
Return a descriptive string for TGzHeader.OS field
}
begin
  case OS of
    AB_GZ_OS_ID_FAT         : Result := AbGzOsFat;
    AB_GZ_OS_ID_Amiga       : Result := AbGzOsAmiga;
    AB_GZ_OS_ID_VMS         : Result := AbGzOsVMS;
    AB_GZ_OS_ID_Unix        : Result := AbGzOsUnix;
    AB_GZ_OS_ID_VM_CMS      : Result := AbGzOsVM_CMS;
    AB_GZ_OS_ID_AtariTOS    : Result := AbGzOsAtari;
    AB_GZ_OS_ID_HPFS        : Result := AbGzOsHPFS;
    AB_GZ_OS_ID_Macintosh   : Result := AbGzOsMacintosh;
    AB_GZ_OS_ID_Z_System    : Result := AbGzOsZ_System;
    AB_GZ_OS_ID_CP_M        : Result := AbGzOsCP_M;
    AB_GZ_OS_ID_TOPS20      : Result := AbGzOsTOPS_20;
    AB_GZ_OS_ID_NTFS        : Result := AbGzOsNTFS;
    AB_GZ_OS_ID_QDOS        : Result := AbGzOsQDOS;
    AB_GZ_OS_ID_AcornRISCOS : Result := AbGzOsAcornRISCOS;
    AB_GZ_OS_ID_unknown     : Result := AbGzOsunknown;
  else
    Result := AbGzOsUndefined;
  end;
end;


function VerifyHeader(Header : TAbGzHeader) : Boolean;
begin
  { check id fields and if deflated (only handle deflate anyway)}
  Result := (Header.ID1 = AB_GZ_HDR_ID1) and
     (Header.ID2 = AB_GZ_HDR_ID2) and
     (Header.CompMethod = 8 {deflate});
end;

function VerifyGZip(Strm : TStream) : TAbArchiveType;
var
{  Hdr : TAbGzHeader; }
  GHlp : TAbGzipStreamHelper;
  Hlpr : TAbDeflateHelper;
  PartialTarData : TMemoryStream;
  CurPos : LongInt;
begin
  Result := atUnknown;

  CurPos := Strm.Position;
  Strm.Seek(0, soFromBeginning);

  {prepare for the try..finally}
  Hlpr := nil;
  PartialTarData := nil;

  GHlp := TAbGzipStreamHelper.Create(Strm);
  try
    {create the stream helper and read the item header}
    GHlp.ReadHeader;

    { check id fields and if deflated (only handle deflate anyway)}
    if VerifyHeader(GHlp.FItem.FGZHeader) then begin
      Result := atGZip; { provisional }

      { check if is actually a Gzipped Tar }
      { partial extract contents, verify vs. Tar }
      PartialTarData := TMemoryStream.Create;
      GHlp.SeekToItemData;
      Hlpr := TAbDeflateHelper.Create;
      Hlpr.PartialSize := 512;
      PartialTarData.SetSize(512 * 2);
      Inflate(Strm, PartialTarData, Hlpr);

      {set to beginning of extracted data}
      PartialTarData.Position := 0;

      if (VerifyTar(PartialTarData) = atTar) then
        Result := atGZippedTar;
    end;
  finally
    GHlp.Free;
    Hlpr.Free;
    PartialTarData.Free;
    
    Strm.Position := CurPos;
  end;
end;


{ TAbGzipStreamHelper }

constructor TAbGzipStreamHelper.Create(AStream : TStream);
begin
  inherited Create(AStream);
  FItem := TAbGzipItem.Create;
end;

destructor TAbGzipStreamHelper.Destroy;
begin
  FItem.Free;
  inherited;
end;

procedure SeekToStringEndInStream(AStream: TStream);
{
locate next instance of a null character in a stream
leaves stream positioned just past that,
or at end of stream if not found or null is last byte in stream.
}
const
  BuffSiz = 1024;
var
  Buff   : array [0..BuffSiz-1] of AnsiChar;
  Len, StartPos, DataRead, TotalRead : LongInt;
  Done : Boolean;
begin
{ basically what this is supposed to do is...}
{
  repeat
    AStream.Read(C, 1);
  until (AStream.Position = AStream.Size) or (C = #0);
}
  StartPos := AStream.Position;
  Done := False;
  TotalRead := 0;
  while not Done do begin
    DataRead := AStream.Read(Buff, BuffSiz - 1);

    if DataRead = 0 then
      Done := True
    else begin
      Buff[DataRead] := #0;
      Len := StrLen(Buff);
      if Len < DataRead then begin
        Done := True;
        AStream.Seek(StartPos + TotalRead + Len + 1, soFromBeginning);
      end else
        TotalRead := TotalRead + DataRead;
    end;
  end;
end;

procedure TAbGzipStreamHelper.SeekToItemData;
{find end of header data, including FileName etc.}
begin
  {** Seek to Compressed Data **}
  FStream.Seek(0, soFromBeginning);
  FItem.LoadGzHeaderFromStream(FStream);
end;

procedure TAbGzipStreamHelper.ExtractItemData(AStream: TStream);
var
  Helper : TAbDeflateHelper;
begin
  Helper := TAbDeflateHelper.Create;
  try
    SeekToItemData;
    if (AStream is TAbBitBucketStream) then
      Helper.Options := Helper.Options or dfc_TestOnly;
    FItem.CRC32 := Inflate(FStream, AStream, Helper);
    FItem.UncompressedSize := AStream.Size{Helper.NormalSize};
  finally
    Helper.Free;
  end;
end;

function TAbGzipStreamHelper.FindFirstItem: Boolean;
var
  GZH : TAbGzHeader;
  DataRead : Integer;
begin
  Result := False;
  FStream.Seek(0, soFromBeginning);
  DataRead := FStream.Read(GZH, SizeOf(TAbGzHeader));
  if (DataRead = SizeOf(TAbGzHeader)) and VerifyHeader(GZH) then begin
    FItem.GZHeader := GZH;
    Result := True;
  end;
  FStream.Seek(0, soFromBeginning);
end;

function TAbGzipStreamHelper.FindNextItem: Boolean;
begin
  { only one item in a GZip }
  Result := False;
end;

function TAbGzipStreamHelper.SeekItem(Index: Integer): Boolean;
begin
  if Index > 0 then
    Result := False
  else
    Result := FindFirstItem;
end;

procedure TAbGzipStreamHelper.WriteArchiveHeader;
begin
  FItem.SaveGzHeaderToStream(FStream);
end;

procedure TAbGzipStreamHelper.WriteArchiveItem(AStream: TStream);
var
  Helper : TAbDeflateHelper;
begin
  Helper := TAbDeflateHelper.Create;
  try
    FItem.CRC32 := Deflate(AStream, FStream, Helper);
    FItem.UncompressedSize := AStream.Size;                              {!!.02}
//    FItem.UncompressedSize := FStream.Size{Helper.NormalSize};         {!!.02}
  finally
    Helper.Free;
  end;
end;

procedure TAbGzipStreamHelper.WriteArchiveTail;
var
  Tail : TAbGzTailRec;
begin
  Tail.CRC32 := FItem.CRC32;
  Tail.ISize := FItem.UncompressedSize;
  FStream.Write(Tail, SizeOf(TAbGzTailRec));
end;

function TAbGzipStreamHelper.GetItemCount: Integer;
begin
  { only one item in a gzip }
  Result := 1;
end;

procedure TAbGzipStreamHelper.ReadHeader;
begin
  FItem.LoadGzHeaderFromStream(FStream);
end;

procedure TAbGzipStreamHelper.ReadTail;
begin
  FStream.Read(FTail, SizeOf(TAbGzTailRec));
end;

function TAbGzipStreamHelper.GetGzCRC: LongInt;
begin
  Result := FItem.CRC32;
end;

function TAbGzipStreamHelper.GetFileSize: LongInt;
begin
  Result := FItem.UncompressedSize;
end;

{ TAbGzipItem }

constructor TAbGzipItem.Create;
begin
{ set defaults }
  { Maxium Compression }
  FGzHeader.XtraFlags := 2;

  FFileName := '';
  FFileComment := '';
  FExtraField := '';

  { source OS ID }
{$IFDEF LINUX } {assume EXT2 system }
  FGzHeader.OS := AB_GZ_OS_ID_Unix;
{$ENDIF LINUX }
{$IFDEF MSWINDOWS } {assume FAT system }
  FGzHeader.OS := AB_GZ_OS_ID_FAT;
{$ENDIF MSWINDOWS }

  FIncludeHeaderCrc := False;
end;

function TAbGzipItem.GetCompressedSize: LongInt;
begin
  Result := FCompressedSize;
end;

function TAbGzipItem.GetExternalFileAttributes: LongInt;
begin
  { GZip has no provision for storing attributes }
  Result := 0;
end;

function TAbGzipItem.GetExtraField: string;
begin
  Result := '';
  if HasExtraField then begin
    SetLength(Result, FXLen);
    Move(FExtraField, Result[1], FXLen);
  end;
end;

function TAbGzipItem.GetFileComment: string;
begin
  Result := '';
  if HasFileComment then
    Result := FFileComment;
end;


function TAbGzipItem.GetFileSystem: TAbGzFileSystem;
begin
  case FGzHeader.OS of
    0..13: Result := TAbGzFileSystem(FGzHeader.OS);
    255:   Result := osUnknown;
    else
      Result := osUndefined;
  end; { case }
end;

function TAbGzipItem.GetHeaderCRC: Word;
begin
  Result := 0;
  if HasHeaderCRC then
    Result := FCRC16;
end;

function TAbGzipItem.GetIsEncrypted: Boolean;
begin
{ GZip doesn't support any native encryption }
  Result := False;
end;

function TAbGzipItem.GetHasExtraField: Boolean;
begin
  Result := (FGZHeader.Flags and AB_GZ_FLAG_FEXTRA) = AB_GZ_FLAG_FEXTRA;
end;

function TAbGzipItem.GetHasFileComment: Boolean;
begin
  Result := (FGZHeader.Flags and AB_GZ_FLAG_FCOMMENT) = AB_GZ_FLAG_FCOMMENT;
end;

function TAbGzipItem.GetHasFileName: Boolean;
begin
  Result := (FGZHeader.Flags and AB_GZ_FLAG_FNAME) = AB_GZ_FLAG_FNAME;
end;

function TAbGzipItem.GetHasHeaderCRC: Boolean;
begin
  Result := (FGZHeader.Flags and AB_GZ_FLAG_FHCRC) = AB_GZ_FLAG_FHCRC;
end;

function TAbGzipItem.GetIsText: Boolean;
begin
  Result := (FGZHeader.Flags and AB_GZ_FLAG_FTEXT) = AB_GZ_FLAG_FTEXT;
end;

function TAbGzipItem.GetLastModFileDate: Word;
var
  Rslt : LongInt;
  D : TDateTime;
begin
  { convert to TDateTime }
  D := AbUnixTimeToDateTime(FGZHeader.ModTime);

  { convert to DOS file Date }
  Rslt := DateTimeToFileDate(D);
  Result := LongRec(Rslt).Hi;
end;

function TAbGzipItem.GetLastModFileTime: Word;
var
  Rslt : LongInt;
  D : TDateTime;
begin
  { convert to TDateTime }
  D := AbUnixTimeToDateTime(FGZHeader.ModTime);

  { convert to DOS file Time }
  Rslt := DateTimeToFileDate(D);
  Result := LongRec(Rslt).Lo;
end;

function TAbGzipItem.GetUncompressedSize: LongInt;
begin
  Result := FUncompressedSize;
end;

procedure TAbGzipItem.LoadGzHeaderFromStream(AStream: TStream);
var
  StartPos : LongInt;
  Len      : LongInt;
  LenW     : Word;
  CRC16    : ShortInt;
begin
  AStream.Read(FGzHeader, SizeOf(TAbGzHeader));

  if HasExtraField then begin
    { get length of extra data }
    AStream.Read(LenW, SizeOf(Word));
    SetLength(FExtraField, LenW);
    AStream.Read(FExtraField[1], LenW);
  end
  else
    FExtraField := '';

  { Get Filename, if any }
  if HasFileName then begin
    StartPos := AStream.Position;
    SeekToStringEndInStream(AStream);
    Len := AStream.Position - StartPos - 1;
    AStream.Seek(StartPos, soFromBeginning);
    SetLength(FFileName, Len);
    AStream.Read(FFileName[1], Len + 1);
  end
  else
    FFileName := 'unknown'; 

  { any comment present? }
  if HasFileComment then begin
    StartPos := AStream.Position;
    SeekToStringEndInStream(AStream);
    Len := AStream.Position - StartPos - 1;
    AStream.Position := StartPos;
    SetLength(FFileComment, Len);
    AStream.Read(FFileComment[1], Len + 1);
  end
  else
    FFileComment := '';

  { any 16-bit CRC for header present? }
  if HasHeaderCRC then begin
    AStream.Read(CRC16, SizeOf(CRC16));
    FCRC16 := CRC16;
  end;

  {Assert: stream should now be located at start of compressed data }

  FCompressedSize := AStream.Size - AStream.Position - SizeOf(TAbGzTailRec);

  DiskFileName := FileName;
  AbUnfixName(FDiskFileName);
  Action := aaNone;
  Tagged := False;
end;

procedure TAbGzipItem.SaveGzHeaderToStream(AStream: TStream);
var
  HBuff, HPtr : PAnsiChar;
  HSize, I32 : LongInt;
  I16 : ShortInt;
  LenW  : Word;
begin
  { start with basic header record }
  HSize := SizeOf(TAbGzHeader);

  { default ID fields }
  FGzHeader.ID1 := AB_GZ_HDR_ID1;
  FGzHeader.ID2 := AB_GZ_HDR_ID2;

  { compression method }
  FGzHeader.CompMethod := 8;  { deflate }

  { flags }
  FGzHeader.Flags := 0;

  { provide for the header CRC }
  if IncludeHeaderCRC then begin
    FGzHeader.Flags := AB_GZ_FLAG_FHCRC;
    Inc(HSize, 2);
  end;

  { add Text flag if user has set it }
  if IsText then
    FGzHeader.Flags := FGzHeader.Flags or AB_GZ_FLAG_FTEXT;

  { any Extra Field Present? }
  if FExtraField > '' then begin
    FGzHeader.Flags := FGZHeader.Flags or AB_GZ_FLAG_FEXTRA;
    HSize := HSize + 2 + Length(FExtraField);
  end;

  { any File Name present? }
  if FFileName > '' then begin
    FGzHeader.Flags := FGZHeader.Flags or AB_GZ_FLAG_FNAME;
    HSize := HSize + Length(FFileName) + 1;
  end;

  { any File Comment present? }
  if FFileComment > '' then begin
    FGzHeader.Flags := FGZHeader.Flags or AB_GZ_FLAG_FCOMMENT;
    HSize := HSize + Length(FFileComment) + 1;
  end;

  { build the header plus extra info }
  GetMem(HBuff, HSize);
  try
    HPtr := HBuff;

    { main header data }
    Move(FGzHeader, HPtr^, SizeOf(TAbGzHeader));
    Inc(HPtr, SizeOf(TAbGzHeader));

    { add extra field if any }
    if HasExtraField then begin
      LenW := Length(FExtraField);
      Move(LenW, HPtr^, SizeOf(Word));
      Inc(HPtr, SizeOf(Word));
      Move(FExtraField[1], HPtr^, LenW);
      Inc(HPtr, LenW);
    end;

    { add filename if any (and include final #0 from string) }
    if HasFileName then begin
      Move(FFileName[1], HPtr^, succ(length(FFileName)));
      Inc(HPtr, succ(length(FFileName)));
    end;

    { add file comment if any (and include final #0 from string) }
    if HasFileComment then begin
      Move(FFileComment[1], HPtr^, succ(length(FFileComment)));
      Inc(HPtr, succ(length(FFileComment)));
    end;

    if IncludeHeaderCRC then begin
      { calculate and write the header CRC }
      I16 := LongRec(I32).Lo;
      Move(I16, HPtr^, sizeof(I16));
    end;

    { dump it all to the stream }
    AStream.Write(HBuff^, HSize);
  finally
    FreeMem(HBuff);
  end;
end;

procedure TAbGzipItem.SetCompressedSize(const Value: LongInt);
begin
  FCompressedSize := Value;
end;

procedure TAbGzipItem.SetExternalFileAttributes(Value: LongInt);
begin
  { do nothing }
end;

procedure TAbGzipItem.SetExtraField(const Value: string);
begin
  FExtraField := '';

  if Value > '' then begin
    FExtraField := Value;
    FGzHeader.Flags := FGzHeader.Flags or AB_GZ_FLAG_FEXTRA;
  end
  else begin
    FGzHeader.Flags := FGzHeader.Flags and not AB_GZ_FLAG_FEXTRA;
  end;
end;

procedure TAbGzipItem.SetFileComment(Value: string);
begin
  FFileComment := '';

  if Value > '' then begin
    FFileComment := Value;
    FGzHeader.Flags := FGzHeader.Flags or AB_GZ_FLAG_FCOMMENT;
  end
  else begin
    FGzHeader.Flags := FGzHeader.Flags and not AB_GZ_FLAG_FCOMMENT;
  end;
end;

procedure TAbGzipItem.SetFileName(Value: string);
begin
  if FFileName <> '' then
     FFileName := '';

  if Value > '' then begin
    FFileName := Value { + #0};
    FGzHeader.Flags := FGzHeader.Flags or AB_GZ_FLAG_FNAME;
  end
  else begin
    FGzHeader.Flags := FGzHeader.Flags and not AB_GZ_FLAG_FNAME;
  end;
end;

procedure TAbGzipItem.SetFileSystem(const Value: TAbGzFileSystem);
begin
  if Value = osUnknown then
    FGzHeader.OS := 255
  else
    FGzHeader.OS := Ord(Value);
end;

procedure TAbGzipItem.SetIsEncrypted(Value: Boolean);
begin
  { do nothing }
end;

procedure TAbGzipItem.SetIsText(const Value: Boolean);
begin
  FIsText := Value;
  case FIsText of
    True  : FGzHeader.Flags := FGzHeader.Flags or AB_GZ_FLAG_FTEXT;
    False : FGzHeader.Flags := FGzHeader.Flags and not AB_GZ_FLAG_FTEXT;
  end;
end;

procedure TAbGzipItem.SetLastModFileDate(const Value: Word);
var
  D : TDateTime;
  UT : LongInt;
begin
  UT := FGZHeader.ModTime;

  { keep seconds in current day, discard date's seconds }
  UT := UT mod SecondsInDay;

  { build new date }
  D := EncodeDate(Value shr 9 + 1980, Value shr 5 and 15, Value and 31);

  { add to unix second count }
  UT :=  UT + AbDateTimeToUnixTime(D);

  { store back in header }
  FGZHeader.ModTime := UT;
end;

procedure TAbGzipItem.SetLastModFileTime(const Value: Word);
var
  T : TDateTime;
  UT : LongInt;
begin
  UT := FGZHeader.ModTime;

  { keep seconds in current date, discard day's seconds }
  UT := UT - (UT mod SecondsInDay);

  { build new time }
  T := EncodeTime(Value shr 11, Value shr 5 and 63, Value and 31 shl 1, 0);

  { add to unix second count }
  UT := UT + AbDateTimeToUnixTime(T);

  { store back in header }
  FGZHeader.ModTime := UT;
end;

procedure TAbGzipItem.SetUncompressedSize(const Value: Integer);
begin
  FUncompressedSize := Value;
end;

{ TAbGzipArchive }

constructor TAbGzipArchive.Create(FileName: string; Mode: Word);
begin
  inherited Create(FileName, Mode);
  FTarLoaded := False;
  FState    := gsGzip;
  FGZStream  := FStream;  { save reference to opened file stream }
  FGZItem    := FItemList;
  FTarStream := TAbVirtualMemoryStream.Create;
  FTarList   := TAbArchiveList.Create;
end;

procedure TAbGzipArchive.SwapToTar;
begin
  FStream := FTarStream;
  FItemList := FTarList;
  FState := gsTar;
end;

procedure TAbGzipArchive.SwapToGzip;
begin
  FStream := FGzStream;
  FItemList := FGzItem;
  FState := gsGzip;
end;

function TAbGzipArchive.CreateItem(const FileSpec: string): TAbArchiveItem;
var
  Buff : array [0..511] of Char;
  GzItem : TAbGzipItem;
begin
  if IsGZippedTar and TarAutoHandle then begin
    if FState <> gsTar then
      SwapToTar;
    Result := inherited CreateItem(FileSpec);
  end
  else begin
    SwapToGzip;
    GzItem := TAbGzipItem.Create;
    try
      GzItem.CompressedSize := 0;
      GzItem.CRC32 := 0;
      StrPCopy(Buff, ExpandFileName(FileSpec));
      GzItem.DiskFileName := StrPas(Buff);
      StrPCopy(Buff, FixName(FileSpec));
      GzItem.FileName := StrPas(Buff);
      Result := GzItem;
    except
      Result := nil;
    end;
  end;
end;

destructor TAbGzipArchive.Destroy;
begin
  SwapToGzip;
  FTarList.Free;
  FTarStream.Free;
  inherited Destroy;
end;


procedure TAbGzipArchive.ExtractItemAt(Index: Integer;
  const NewName: string);
var
  OutStream : TFileStream;
  UseName : string;
  CurItem : TAbGzipItem;
{$IFDEF LINUX}                                                           {!!.01}
  FileDateTime  : TDateTime;                                             {!!.01}
  LinuxFileTime : LongInt;                                               {!!.01}
{$ENDIF LINUX}                                                           {!!.01}
begin
  if IsGZippedTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemAt(Index, NewName);
  end
  else begin
    SwapToGzip;
    if Index > 0 then Index := 0; { only one item in a GZip}

    UseName := NewName;
    CurItem := TAbGzipItem(ItemList[Index]);

    { check if path to save to is okay }
    if AbConfirmPath(BaseDirectory, UseName, ExtractOptions, FOnConfirmOverwrite) then
    begin
      OutStream := TFileStream.Create(UseName, fmCreate or fmShareDenyNone);

      try
        try {OutStream}
          ExtractItemToStreamAt(Index, OutStream);
          {$IFDEF MSWINDOWS}
          FileSetDate(OutStream.Handle, (Longint(CurItem.LastModFileDate) shl 16)
            + CurItem.LastModFileTime);
          AbFileSetAttr(UseName, 0); {normal file}                       {!!.01}
          {$ENDIF}
          {$IFDEF LINUX}
          FileDateTime := AbDosFileDateToDateTime(CurItem.LastModFileDate,  {!!.01}
            CurItem.LastModFileTime);                                    {!!.01}
          LinuxFileTime := AbDateTimeToUnixTime(FileDateTime);           {!!.01}
//!! MVC not yet implemented          FileSetDate(UseName, LinuxFileTime);                           {!!.01}
          AbFileSetAttr(UseName, AB_FPERMISSION_GENERIC);                {!!.01}
          {$ENDIF}
        finally {OutStream}
          OutStream.Free;
        end; {OutStream}

      except
        on E : EAbUserAbort do begin
          FStatus := asInvalid;
          if FileExists(UseName) then
            DeleteFile(UseName);
          raise;
        end else begin
          if FileExists(UseName) then
            DeleteFile(UseName);
          raise;
        end;
      end;
    end;
  end;
end;

procedure TAbGzipArchive.ExtractItemToStreamAt(Index: Integer;
  aStream: TStream);
var
  GzHelp  : TAbGzipStreamHelper;
begin
  if IsGzippedTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemToStreamAt(Index, aStream);
  end
  else begin
    SwapToGzip;
    { note Index ignored as there's only one item in a GZip }

    GZHelp := TAbGzipStreamHelper.Create(FGzStream);
    try
      { read GZip Header }
      GzHelp.ReadHeader;

      { extract copy data from GZip}
      GzHelp.ExtractItemData(aStream);

      { Get validation data }
      GzHelp.ReadTail;

      { validate against CRC }
      if GzHelp.FItem.Crc32 <> GzHelp.TailCRC then
        raise EAbGzipBadCRC.Create;

      { validate against file size }
      if GzHelp.FItem.UncompressedSize <> GZHelp.TailSize then
        raise EAbGzipBadFileSize.Create;
    finally
      GzHelp.Free;
    end;
  end;
end;

function TAbGzipArchive.FixName(Value: string): string;
{ fix up fileaname for storage }
begin
  {GZip files Always strip the file path}
  StoreOptions := StoreOptions + [soStripDrive, soStripPath];
  Result := '';
  if Value <> '' then
    Result := ExtractFileName(Value);
end;

function TAbGzipArchive.GetIsGzippedTar: Boolean;
begin
  Result := FIsGzippedTar;
end;

{!!.03 -- Added }
function TAbGzipArchive.GetItem(Index: Integer): TAbGzipItem;
begin
  Result := nil;
  if Index = 0 then
    Result := TAbGzipItem(FItemList.Items[Index]);
end;
{!!.03 -- End Added }

procedure TAbGzipArchive.LoadArchive;
var
  GzHelp       : TAbGzipStreamHelper;
  Item         : TAbGzipItem;
  ItemFound    : Boolean;
  Abort        : Boolean;
  TotalEntries : Integer;
  i            : Integer;
  Progress     : Byte;
begin
  if FGzStream.Size = 0 then
    Exit;

  if IsGzippedTar and TarAutoHandle then begin
    { extract Tar and set stream up }

    GzHelp := TAbGzipStreamHelper.Create(FGzStream);
    try
      if not FTarLoaded then begin
        GzHelp.SeekToItemData;
        GzHelp.ExtractItemData(FTarStream);
        SwapToTar;
        inherited LoadArchive;
        FTarLoaded := True;
      end;
    finally
      GzHelp.Free;
    end;
  end
  else begin
    SwapToGzip;

    { create helper }
    GzHelp := TAbGzipStreamHelper.Create(FGzStream);
    try
      TotalEntries := GzHelp.GetItemCount;

      {build Items list from tar header records}
      i := 0;

      { reset Tar }
      ItemFound := GzHelp.FindFirstItem;

      { while more data in Tar }

      if ItemFound then begin
        Item := TAbGzipItem.Create;
        Item.LoadGzHeaderFromStream(FGzStream);

        FGzStream.Seek(-SizeOf(TAbGzTailRec), soFromEnd);
        GZHelp.ReadTail;
        Item.FCRC32 := GZHelp.FItem.FCRC32;
        Item.UncompressedSize := 
          GZHelp.FItem.UncompressedSize;

        Item.Action := aaNone;
        FItemList.Clear;
        FItemList.Add(Item);
        Inc(i);

        { show progress and allow for aborting }
        Progress := (i * 100) div TotalEntries;
        DoArchiveProgress(Progress, Abort);
        if Abort then begin
          FStatus := asInvalid;
          raise EAbUserAbort.Create;
        end;

        { get the next item }
      end;

      DoArchiveProgress(100, Abort);
      FIsDirty := False;
    finally
      { Clean Up }
      GzHelp.Free;
    end;
  end;
end;

{!!.03 -- Added }
procedure TAbGzipArchive.PutItem(Index: Integer; const Value: TAbGzipItem);
begin
  if Index = 0 then
    FItemList.Items[Index] := Value;
end;
{!!.03 -- End Added }

procedure TAbGzipArchive.SaveArchive;
var
  InGzHelp, OutGzHelp : TAbGzipStreamHelper;
  Abort               : Boolean;
  i                   : Integer;
  NewStream           : TAbVirtualMemoryStream;
  WorkingStream       : TAbVirtualMemoryStream;
  UncompressedStream  : TStream;
  DateTime            : LongInt;
  SaveDir             : string;
  CurItem             : TAbGzipItem;
begin
  {prepare for the try..finally}
  OutGzHelp := nil;
  NewStream := nil;
  WorkingStream := nil;

  try
    InGzHelp := TAbGzipStreamHelper.Create(FGzStream);

    try
      if IsGzippedTar and TarAutoHandle then begin
        { save the Tar data first }
        SwapToTar;
        inherited SaveArchive;

        { update contents of GZip Stream with new Tar }
        FGZStream.Position := 0;
        InGzHelp.ReadHeader;
        FGZStream.Size := 0;
        InGzHelp.WriteArchiveHeader;
        InGzHelp.WriteArchiveItem(FTarStream);
        InGzHelp.WriteArchiveTail;
      end;

      SwapToGzip;

      {init new archive stream}
      NewStream := TAbVirtualMemoryStream.Create;
      OutGzHelp := TAbGzipStreamHelper.Create(NewStream);

      { create helper }
      NewStream.SwapFileDirectory := ExtractFilePath(AbGetTempFile(FTempDir, False));

      {build new archive from existing archive}
      for i := 0 to pred(Count) do begin
        FCurrentItem := ItemList[i];
        CurItem      := TAbGzipItem(ItemList[i]);
        InGzHelp.SeekToItemData;

        case CurItem.Action of
          aaNone, aaMove : begin
          {just copy the file to new stream}
            WorkingStream := TAbVirtualMemoryStream.Create;
            InGzHelp.SeekToItemData;
            InGzHelp.ExtractItemData(WorkingStream);
            WorkingStream.Position := 0;
            CurItem.SaveGzHeaderToStream(NewStream);
            OutGzHelp.WriteArchiveItem(WorkingStream);
          end;

          aaDelete: {doing nothing omits file from new stream} ;

          aaAdd, aaFreshen, aaReplace, aaStreamAdd: begin
            try
              if (CurItem.Action = aaStreamAdd) then begin
              { adding from a stream }
                CurItem.SaveGzHeaderToStream(NewStream);
                CurItem.UncompressedSize := InStream.Size;
                OutGzHelp.WriteArchiveItem(InStream);
              end
              else begin
              { it's coming from a file }
                UncompressedStream := TFileStream.Create(CurItem.DiskFileName,
                    fmOpenRead or fmShareDenyWrite );
                try
                  GetDir(0, SaveDir);
                  try {SaveDir}
                    if (BaseDirectory <> '') then
                      ChDir(BaseDirectory);

                    {Now get the file's attributes}
                    CurItem.ExternalFileAttributes := AbFileGetAttr(CurItem.DiskFileName);

                    CurItem.UncompressedSize := UncompressedStream.Size;
                  finally {SaveDir}
                    ChDir( SaveDir );
                  end; {SaveDir}

                  DateTime := FileAge(CurItem.DiskFileName);
                  CurItem.LastModFileTime := LongRec(DateTime).Lo;
                  CurItem.LastModFileDate := LongRec(DateTime).Hi;

                  CurItem.SaveGzHeaderToStream(NewStream);
                  OutGzHelp.WriteArchiveItem(UncompressedStream);

                finally {UncompressedStream}
                  UncompressedStream.Free;
                end; {UncompressedStream}

              end;
            except
              ItemList[i].Action := aaDelete;
              DoProcessItemFailure(ItemList[i], ptAdd, ecFileOpenError, 0);
            end;
          end;
        end; {case}
      end; { for }
    finally
      InGzHelp.Free;
    end;

    {copy new stream to FStream}
    OutGzHelp.WriteArchiveTail;
    NewStream.Position := 0;
    if (FStream is TMemoryStream) then
      TMemoryStream(FStream).LoadFromStream(NewStream)
    else begin
      { need new stream to write }
      FStream.Free;
      FStream := TAbSpanStream.Create(FArchiveName, fmOpenWrite or fmShareDenyWrite, mtLocal, FSpanningThreshold);
      TAbSpanStream(FStream).OnRequestImage := DoSpanningMediaRequest;   {!!.01}
      TAbSpanStream(FStream).OnArchiveProgress := DoArchiveSaveProgress; {!!.04}
      try
        FStream.CopyFrom(NewStream, NewStream.Size);
        FGZStream := FStream;
      except
        raise EAbException.Create('Unable to create new Spanned stream');
      end;
    end;

    {update Items list}
    for i := pred( Count ) downto 0 do begin
      if ItemList[i].Action = aaDelete then
        FItemList.Delete( i )
      else if ItemList[i].Action <> aaFailed then
        ItemList[i].Action := aaNone;
    end;

    DoArchiveSaveProgress( 100, Abort );                               {!!.04}
    DoArchiveProgress( 100, Abort );
  finally {NewStream}
    WorkingStream.Free;
    OutGzHelp.Free;
    NewStream.Free;
  end;
end;

procedure TAbGzipArchive.SetTarAutoHandle(const Value: Boolean);
begin
  case Value of
    True  : begin
      SwapToTar;
    end;

    False : begin
      SwapToGzip;
    end;
  end;
  FTarAutoHandle := Value;
end;

procedure TAbGzipArchive.TestItemAt(Index: Integer);
var
  SavePos   : LongInt;
  GZType    : TAbArchiveType;
  BitBucket : TAbBitBucketStream;
  GZHelp    : TAbGzipStreamHelper;
begin
  if IsGzippedTar and TarAutoHandle then begin
    inherited TestItemAt(Index);
  end
  else begin
    { note Index ignored as there's only one item in a GZip }
    SavePos := FGzStream.Position;
    GZType := VerifyGZip(FGZStream);
    if not (GZType in [atGZip, atGZippedTar]) then
      raise EAbGzipInvalid.Create;

    BitBucket := nil;
    GZHelp := nil;
    try
      BitBucket := TAbBitBucketStream.Create(1024);
      GZHelp := TAbGzipStreamHelper.Create(FGZStream);

      GZHelp.ExtractItemData(BitBucket);
      GZHelp.ReadTail;

      { validate against CRC }
      if GzHelp.FItem.Crc32 <> GZHelp.TailCRC then
        raise EAbGzipBadCRC.Create;

      { validate against file size }
      if GzHelp.FItem.UncompressedSize <> GZHelp.TailSize then
        raise EAbGzipBadFileSize.Create;

    finally
      GZHelp.Free;
      BitBucket.Free;
    end;

    FGzStream.Position := SavePos;
  end;
end;

procedure TAbGzipArchive.DoSpanningMediaRequest(Sender: TObject;
  ImageNumber: Integer; var ImageName: string; var Abort: Boolean);
begin
  Abort := False;
end;

end.
