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
{* ABBREVIA: AbGzTyp.pas 3.05                            *}
{*********************************************************}
{* ABBREVIA: TAbGzipArchive, TAbGzipItem classes         *}
{*********************************************************}
{* Misc. constants, types, and routines for working      *}
{* with GZip files                                       *}
{* See: RFC 1952                                         *}
{* "GZIP file format specification version 4.3"          *}
{* for more information on GZip                          *}
{* See "algorithm.doc" in Gzip source and "format.txt"   *}
{* on gzip.org for differences from RFC                  *}
{*********************************************************}

{$I AbDefine.inc}

unit AbGzTyp;

interface

uses
  SysUtils, Classes,
  AbConst, AbExcept, AbUtils, AbArcTyp, AbTarTyp,
  AbDfBase, AbDfDec, AbDfEnc, AbVMStrm, AbBitBkt;

type
  { pre-defined "operating system" (really more FILE system)
    types for the Gzip header }
  TAbGzFileSystem =
    (osFat, osAmiga, osVMS, osUnix, osVM_CMS, osAtariTOS,
    osHPFS, osMacintosh, osZSystem, osCP_M, osTOPS20,
    osNTFS, osQDOS, osAcornRISCOS, osVFAT, osMVS, osBeOS,
    osTandem, osTHEOS, osUnknown, osUndefined);

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
      { Bit 1: FCONTINUATION file is a continuation of a multi-part gzip file}
                      { RFC 1952 says this is the header CRC16 flag, but gzip}
                      { reserves it and won't extract the file if this is set}
                      { header data includes part number after header record}
      { Bit 2: FEXTRA   header data contains Extra Data, starts after part}
                      { number (if any)}
      { Bit 3: FNAME    header data contains FileName, null terminated}
                      { string starting immediately after Extra Data (if any)}
                      { RFC 1952 says this is ISO 8859-1 encoded, but gzip}
                      { always uses the system encoding}
      { Bit 4: FCOMMENT header data contains Comment, null terminated string}
                      { starting immediately after FileName (if any)}
      { Bit 5: FENCRYPTED file is encrypted using zip-1.9 encryption }
                      { header data contains a 12-byte encryption header }
                      { starting immediately after Comment.  Documented in}
                      { "algorithm.doc", but unsupported in gzip}
      { Bits 6..7 are undefined and reserved as of this writing (8/25/2009)}
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
    ISize : LongWord;  { size of uncompressed data }
  end;

  TAbGzExtraFieldSubID = array[0..1] of AnsiChar;

type
  TAbGzipExtraField = class(TAbExtraField)
  private
    FGZHeader : PAbGzHeader;
    function GetID(aIndex : Integer): TAbGzExtraFieldSubID;
  protected
    procedure Changed; override;
  public
    constructor Create(aGZHeader : PAbGzHeader);
    procedure Delete(aID : TAbGzExtraFieldSubID);
    function Get(aID : TAbGzExtraFieldSubID;
      out aData : Pointer; out aDataSize : Word) : Boolean;
    procedure Put(aID : TAbGzExtraFieldSubID; const aData; aDataSize : Word);
  public
    property IDs[aIndex : Integer]: TAbGzExtraFieldSubID
      read GetID;
  end;

  TAbGzipItem = class(TAbArchiveItem)
  private
    FGZHeader : TAbGzHeader;
    FExtraField : TAbGzipExtraField;
    FFileComment : AnsiString;

  protected
    function GetFileSystem: TAbGzFileSystem;
    function GetHasExtraField: Boolean;
    function GetHasFileComment: Boolean;
    function GetHasFileName: Boolean;
    function GetIsText: Boolean;

    procedure SetFileComment(const Value : AnsiString);
    procedure SetFileSystem(const Value: TAbGzFileSystem);
    procedure SetIsText(const Value: Boolean);

    function GetExternalFileAttributes : LongWord; override;
    function GetIsEncrypted : Boolean; override;
    function GetLastModDateTime: TDateTime; override;
    function GetLastModFileDate : Word; override;
    function GetLastModFileTime : Word; override;
    function GetSystemSpecificAttributes: LongWord; override;
    function GetSystemSpecificLastModFileTime: Longint; override;

    procedure SetExternalFileAttributes( Value : LongWord ); override;
    procedure SetFileName(const Value : string); override;
    procedure SetIsEncrypted(Value : Boolean); override;
    procedure SetLastModDateTime(const Value : TDateTime); override;
    procedure SetLastModFileDate(const Value : Word); override;
    procedure SetLastModFileTime(const Value : Word); override;
    procedure SetSystemSpecificLastModFileTime(const Value: Longint); override;

    procedure SaveGzHeaderToStream(AStream : TStream);
    procedure LoadGzHeaderFromStream(AStream : TStream);
  public
    property CompressionMethod : Byte
      read FGZHeader.CompMethod;

    property ExtraFlags : Byte {Default: 2}
      read FGZHeader.XtraFlags write FGZHeader.XtraFlags;

    property Flags : Byte
      read FGZHeader.Flags;

    property FileComment : AnsiString
      read FFileComment write SetFileComment;

    property FileSystem : TAbGzFileSystem {Default: osFat (Windows); osUnix (Linux)}
      read GetFileSystem write SetFileSystem;

    property ExtraField : TAbGzipExtraField
      read FExtraField;

    property IsEncrypted : Boolean
      read GetIsEncrypted;

    property HasExtraField : Boolean
      read GetHasExtraField;

    property HasFileName : Boolean
      read GetHasFileName;

    property HasFileComment : Boolean
      read GetHasFileComment;

    property IsText : Boolean
      read GetIsText write SetIsText;

    property GZHeader : TAbGzHeader
      read FGZHeader;

    constructor Create; override;
    destructor Destroy; override;
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
    property TailSize : LongWord
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
    FState     : TAbGzipArchiveState;
    FIsGzippedTar : Boolean;

    procedure SetTarAutoHandle(const Value: Boolean);
    function GetIsGzippedTar: Boolean;
    procedure SwapToGzip;
    procedure SwapToTar;

  protected
    function CreateItem(const SourceFileName   : string;
                        const ArchiveDirectory : string): TAbArchiveItem;
      override;
    procedure ExtractItemAt(Index : Integer; const UseName : string);
      override;
    procedure ExtractItemToStreamAt(Index : Integer; aStream : TStream);
      override;
    procedure LoadArchive;
      override;
    procedure SaveArchive;
      override;
    procedure TestItemAt(Index : Integer);
      override;
    function FixName(const Value : string) : string;
      override;

    function GetItem(Index: Integer): TAbGzipItem;                  {!!.03}
    procedure PutItem(Index: Integer; const Value: TAbGzipItem);    {!!.03}
  public {methods}
    constructor CreateFromStream(aStream : TStream; const aArchiveName : string);
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

uses
  AbResString, DCOSUtils, DCClassesUtf8, DCConvertEncoding;

const
  { Header Signature Values}
  AB_GZ_HDR_ID1 = $1F;
  AB_GZ_HDR_ID2 = $8B;

  { Test bits for TGzHeader.Flags field }
  AB_GZ_FLAG_FTEXT         = $01;
  AB_GZ_FLAG_FCONTINUATION = $02;
  AB_GZ_FLAG_FEXTRA        = $04;
  AB_GZ_FLAG_FNAME         = $08;
  AB_GZ_FLAG_FCOMMENT      = $10;
  AB_GZ_FLAG_FENCRYPTED    = $20;
  AB_GZ_UNSUPPORTED_FLAGS  = $E2;

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
  AB_GZ_OS_ID_VFAT        = 14;
  AB_GZ_OS_ID_MVS         = 15;
  AB_GZ_OS_ID_BEOS        = 16;
  AB_GZ_OS_ID_TANDEM      = 17;
  AB_GZ_OS_ID_THEOS       = 18;
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
    AB_GZ_OS_ID_VFAT        : Result := AbGzOsVFAT;
    AB_GZ_OS_ID_MVS         : Result := AbGzOsMVS;
    AB_GZ_OS_ID_BEOS        : Result := AbGzOsBeOS;
    AB_GZ_OS_ID_TANDEM      : Result := AbGzOsTandem;
    AB_GZ_OS_ID_THEOS       : Result := AbGzOsTHEOS;
    AB_GZ_OS_ID_unknown     : Result := AbGzOsunknown;
  else
    Result := AbGzOsUndefined;
  end;
end;


function VerifyHeader(const Header : TAbGzHeader) : Boolean;
begin
  { check id fields and if deflated (only handle deflate anyway)}
  Result := (Header.ID1 = AB_GZ_HDR_ID1) and
            (Header.ID2 = AB_GZ_HDR_ID2) and
            (Header.CompMethod = 8 {deflate});
end;

function VerifyGZip(Strm : TStream) : TAbArchiveType;
var
  GHlp : TAbGzipStreamHelper = nil;
  Hlpr : TAbDeflateHelper;
  PartialTarData : TMemoryStream;
  CurPos : Int64;
begin
  Result := atUnknown;
  CurPos := Strm.Position;

  try
    {prepare for the try..finally}
    Hlpr := nil;
    PartialTarData := nil;

    try
      Strm.Seek(0, soFromBeginning);
      GHlp := TAbGzipStreamHelper.Create(Strm);

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
  except
    on EFilerError do
      Result := atUnknown;
  end;
end;

{ TAbGzipExtraField }

constructor TAbGzipExtraField.Create(aGZHeader : PAbGzHeader);
begin
  inherited Create;
  FGZHeader := aGZHeader;
end;

procedure TAbGzipExtraField.Changed;
begin
  if Buffer = nil then
    FGzHeader.Flags := FGzHeader.Flags and not AB_GZ_FLAG_FEXTRA
  else
    FGzHeader.Flags := FGzHeader.Flags or AB_GZ_FLAG_FEXTRA;
end;

procedure TAbGzipExtraField.Delete(aID : TAbGzExtraFieldSubID);
begin
  inherited Delete(Word(aID));
end;

function TAbGzipExtraField.GetID(aIndex : Integer): TAbGzExtraFieldSubID;
begin
  Result := TAbGzExtraFieldSubID(inherited IDs[aIndex]);
end;

function TAbGzipExtraField.Get(aID : TAbGzExtraFieldSubID; out aData : Pointer;
  out aDataSize : Word) : Boolean;
begin
  Result := inherited Get(Word(aID), aData, aDataSize);
end;

procedure TAbGzipExtraField.Put(aID : TAbGzExtraFieldSubID; const aData; aDataSize : Word);
begin
  inherited Put(Word(aID), aData, aDataSize);
end;


{ TAbGzipStreamHelper }

constructor TAbGzipStreamHelper.Create(AStream : TStream);
begin
  inherited Create(AStream);
  FItem := TAbGzipItem.Create;
  FItem.LastModDateTime := SysUtils.Now;
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
    FItem.CompressedSize := Helper.CompressedSize;
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
    FItem.FGZHeader := GZH;
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
  // TODO: deflate option: (just slowest/fastest)
  //fast :  Helper.PKZipOption := 's';
  //slow :  Helper.PKZipOption := 'x';
  try
    FItem.CRC32 := Deflate(AStream, FStream, Helper);
    FItem.UncompressedSize := AStream.Size;//Helper.NormalSize
    FItem.CompressedSize := Helper.CompressedSize;
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
  inherited Create;

  { default ID fields }
  FGzHeader.ID1 := AB_GZ_HDR_ID1;
  FGzHeader.ID2 := AB_GZ_HDR_ID2;

  { compression method }
  FGzHeader.CompMethod := 8;  { deflate }

  { Maxium Compression }
  FGzHeader.XtraFlags := 2;

  inherited SetFileName('');
  FFileComment := '';
  FExtraField := TAbGzipExtraField.Create(@FGzHeader);

  { source OS ID }
{$IFDEF LINUX } {assume EXT2 system }
  FGzHeader.OS := AB_GZ_OS_ID_Unix;
{$ENDIF LINUX }
{$IFDEF MSWINDOWS } {assume FAT system }
  FGzHeader.OS := AB_GZ_OS_ID_FAT;
{$ENDIF MSWINDOWS }
end;

destructor TAbGzipItem.Destroy;
begin
  FExtraField.Free;
  inherited;
end;

function TAbGzipItem.GetExternalFileAttributes: LongWord;
begin
  { GZip has no provision for storing attributes }
  Result := AB_FPERMISSION_GENERIC or AB_FMODE_FILE;
end;

function TAbGzipItem.GetFileSystem: TAbGzFileSystem;
begin
  case FGzHeader.OS of
    0..18: Result := TAbGzFileSystem(FGzHeader.OS);
    255:   Result := osUnknown;
    else
      Result := osUndefined;
  end; { case }
end;

function TAbGzipItem.GetIsEncrypted: Boolean;
begin
  Result := (FGZHeader.Flags and AB_GZ_FLAG_FENCRYPTED) = AB_GZ_FLAG_FENCRYPTED;
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

function TAbGzipItem.GetIsText: Boolean;
begin
  Result := (FGZHeader.Flags and AB_GZ_FLAG_FTEXT) = AB_GZ_FLAG_FTEXT;
end;

function TAbGzipItem.GetLastModDateTime : TDateTime;
begin
  // Gzip stores Unix time.
  Result := AbUnixFileTimeToDateTime(FGzHeader.ModTime);
end;

function TAbGzipItem.GetLastModFileDate: Word;
var
  D : TDateTime;
begin
  { convert to TDateTime }
  D := AbUnixFileTimeToDateTime(FGZHeader.ModTime);

  { convert to DOS file Date }
  Result := LongRec(DateTimeToFileDate(D)).Hi;
end;

function TAbGzipItem.GetLastModFileTime: Word;
var
  D : TDateTime;
begin
  { convert to TDateTime }
  D := AbUnixFileTimeToDateTime(FGZHeader.ModTime);

  { convert to DOS file Time }
  Result := LongRec(DateTimeToFileDate(D)).Lo;
end;

function TAbGzipItem.GetSystemSpecificAttributes: LongWord;
begin
  Result := GetExternalFileAttributes;
{$IFDEF MSWINDOWS}
  Result := AbUnix2DosFileAttributes(Result);
{$ENDIF}
end;

function TAbGzipItem.GetSystemSpecificLastModFileTime: Longint;
{$IFDEF MSWINDOWS}
var
  DateTime: TDateTime;
{$ENDIF}
begin
  Result   := FGZHeader.ModTime;

{$IFDEF MSWINDOWS}
  DateTime := AbUnixFileTimeToDateTime(Result);
  Result   := AbDateTimeToDosFileTime(DateTime);
{$ENDIF}
end;

procedure TAbGzipItem.LoadGzHeaderFromStream(AStream: TStream);
var
  StartPos : LongInt;
  Len      : LongInt;
  LenW     : Word;
  AnsiName : AnsiString;
  tempFileName: String;
begin
  AStream.Read(FGzHeader, SizeOf(TAbGzHeader));

  { Skip part number, if any  }
  if (FGzHeader.Flags and AB_GZ_FLAG_FCONTINUATION) = AB_GZ_FLAG_FCONTINUATION then
    AStream.Seek(SizeOf(Word), soCurrent);

  if HasExtraField then begin
    { get length of extra data }
    AStream.Read(LenW, SizeOf(Word));
    FExtraField.LoadFromStream(AStream, LenW);
  end
  else
    FExtraField.Clear;

  { Get Filename, if any }
  if HasFileName then begin
    StartPos := AStream.Position;
    SeekToStringEndInStream(AStream);
    Len := AStream.Position - StartPos - 1;
    AStream.Seek(StartPos, soFromBeginning);
    SetLength(AnsiName, Len);
    AStream.Read(AnsiName[1], Len + 1);
    inherited SetFileName(SysToUtf8(AnsiName));
  end
  else
    inherited SetFileName('unknown');

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


  {Assert: stream should now be located at start of compressed data }
  {If file was compressed with 3.3 spec this will be invalid so use with care}
  CompressedSize := AStream.Size - AStream.Position - SizeOf(TAbGzTailRec);

  tempFileName := FileName;
  AbUnfixName(tempFileName);
  DiskFileName := tempFileName;
  Action := aaNone;
  Tagged := False;
end;

procedure TAbGzipItem.SaveGzHeaderToStream(AStream: TStream);
var
  LenW : Word;
  AnsiName : AnsiString;
begin
  { default ID fields }
  FGzHeader.ID1 := AB_GZ_HDR_ID1;
  FGzHeader.ID2 := AB_GZ_HDR_ID2;

  { compression method }
  FGzHeader.CompMethod := 8;  { deflate }

  { reset unsupported flags }
  FGzHeader.Flags := FGzHeader.Flags and not AB_GZ_UNSUPPORTED_FLAGS;

  { main header data }
  AStream.Write(FGzHeader, SizeOf(TAbGzHeader));

  { add extra field if any }
  if HasExtraField then begin
    LenW := Length(FExtraField.Buffer);
    AStream.Write(LenW, SizeOf(LenW));
    if LenW > 0 then
      AStream.Write(FExtraField.Buffer[0], LenW);
  end;

  { add filename if any (and include final #0 from string) }
  if HasFileName then begin
    AnsiName := Utf8ToSys(FileName);
    AStream.Write(AnsiName[1], Length(AnsiName) + 1);
  end;

  { add file comment if any (and include final #0 from string) }
  if HasFileComment then
    AStream.Write(FFileComment[1], Length(FFileComment) + 1);
end;

procedure TAbGzipItem.SetExternalFileAttributes(Value: LongWord);
begin
  { do nothing }
end;

procedure TAbGzipItem.SetFileComment(const Value: AnsiString);
begin
  FFileComment := Value;
  if FFileComment <> '' then
    FGzHeader.Flags := FGzHeader.Flags or AB_GZ_FLAG_FCOMMENT
  else
    FGzHeader.Flags := FGzHeader.Flags and not AB_GZ_FLAG_FCOMMENT;
end;

procedure TAbGzipItem.SetFileName(const Value: string);
begin
  inherited SetFileName(Value);

  if (Value <> '') and (Value <> 'unknown') then
    FGzHeader.Flags := FGzHeader.Flags or AB_GZ_FLAG_FNAME
  else
    FGzHeader.Flags := FGzHeader.Flags and not AB_GZ_FLAG_FNAME;
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
  if Value then
    FGzHeader.Flags := FGzHeader.Flags or AB_GZ_FLAG_FTEXT
  else
    FGzHeader.Flags := FGzHeader.Flags and not AB_GZ_FLAG_FTEXT;
end;

procedure TAbGzipItem.SetLastModDateTime(const Value : TDateTime);
begin
  // Gzip stores Unix time.
  FGzHeader.ModTime := AbDateTimeToUnixFileTime(Value);
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
  UT :=  UT + AbDateTimeToUnixFileTime(D);

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
  UT := UT + AbDateTimeToUnixFileTime(T);

  { store back in header }
  FGZHeader.ModTime := UT;
end;

procedure TAbGzipItem.SetSystemSpecificLastModFileTime(const Value: Longint);
var
  UnixFileTime: Longint;
{$IFDEF MSWINDOWS}
  DateTime: TDateTime;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  DateTime     := AbDosFileTimeToDateTime(Value);
  UnixFileTime := AbDateTimeToUnixFileTime(DateTime);
{$ELSE}
  UnixFileTime := Value;
{$ENDIF}

  FGZHeader.ModTime := UnixFileTime;
end;

{ TAbGzipArchive }

constructor TAbGzipArchive.CreateFromStream(aStream : TStream;
  const aArchiveName : string);
begin
  inherited CreateFromStream(aStream, aArchiveName);
  FState     := gsGzip;
  FGZStream  := FStream;
  FGZItem    := FItemList;
  FTarStream := TAbVirtualMemoryStream.Create;
  FTarList   := TAbArchiveList.Create(True);
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

function TAbGzipArchive.CreateItem(const SourceFileName   : string;
                                   const ArchiveDirectory : string): TAbArchiveItem;
var
  GzItem : TAbGzipItem;
  FullSourceFileName, FullArchiveFileName: String;
begin
  if IsGZippedTar and TarAutoHandle then begin
    SwapToTar;
    Result := inherited CreateItem(SourceFileName, ArchiveDirectory);
  end
  else begin
    SwapToGzip;
    GzItem := TAbGzipItem.Create;
    try
      MakeFullNames(SourceFileName, ArchiveDirectory,
                    FullSourceFileName, FullArchiveFileName);

      GzItem.FileName := FullArchiveFileName;
      GzItem.DiskFileName := FullSourceFileName;

      Result := GzItem;
    except
      Result := nil;
      raise;
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
  const UseName: string);
var
  OutStream : TStream;
  CurItem : TAbGzipItem;
begin
  if IsGZippedTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemAt(Index, UseName);
  end
  else begin
    SwapToGzip;
    if Index > 0 then Index := 0; { only one item in a GZip}

    CurItem := TAbGzipItem(ItemList[Index]);

    OutStream := TFileStreamEx.Create(UseName, fmCreate or fmShareDenyNone);
    try
      try {OutStream}
        ExtractItemToStreamAt(Index, OutStream);
      finally {OutStream}
        OutStream.Free;
      end; {OutStream}
      // [ 880505 ]  Need to Set Attributes after File is closed {!!.05}
      AbSetFileTime(UseName, CurItem.SystemSpecificLastModFileTime);
      AbFileSetAttr(UseName, CurItem.SystemSpecificAttributes);

    except
      on E : EAbUserAbort do begin
        FStatus := asInvalid;
        if mbFileExists(UseName) then
          mbDeleteFile(UseName);
        raise;
      end else begin
        if mbFileExists(UseName) then
          mbDeleteFile(UseName);
        raise;
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

      {$IFDEF STRICTGZIP}
      { According to
          http://www.gzip.org/zlib/rfc1952.txt

       A compliant gzip compressor should calculate and set the CRC32 and ISIZE.
       However, a compliant decompressor should not check these values.

       If you want to check the the values of the CRC32 and ISIZE in a GZIP file
       when decompressing enable the STRICTGZIP define contained in AbDefine.inc }

      { validate against CRC }
      if GzHelp.FItem.Crc32 <> GzHelp.TailCRC then
        raise EAbGzipBadCRC.Create;

      { validate against file size }
      if GzHelp.FItem.UncompressedSize <> GZHelp.TailSize then
        raise EAbGzipBadFileSize.Create;
      {$ENDIF}
    finally
      GzHelp.Free;
    end;
  end;
end;

function TAbGzipArchive.FixName(const Value: string): string;
{ fix up fileaname for storage }
begin
  if FState = gsTar then
    Result := inherited FixName( Value )
  else begin
    {GZip files Always strip the file path}
    StoreOptions := StoreOptions + [soStripDrive, soStripPath];
    Result := '';
    if Value <> '' then
      Result := ExtractFileName(Value);
  end;
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
  GzHelp : TAbGzipStreamHelper;
  Item   : TAbGzipItem;
  Abort  : Boolean;
begin
  SwapToGzip;
  if FGzStream.Size > 0 then begin
    GzHelp := TAbGzipStreamHelper.Create(FGzStream);
    try
      if GzHelp.FindFirstItem then begin
        Item := TAbGzipItem.Create;
        Item.LoadGzHeaderFromStream(FGzStream);
        FGzStream.Seek(-SizeOf(TAbGzTailRec), soFromEnd);
        GZHelp.ReadTail;
        Item.CRC32 := GZHelp.TailCRC;
        Item.UncompressedSize := GZHelp.TailSize;

        Item.Action := aaNone;
        FGZItem.Add(Item);

        if IsGzippedTar and TarAutoHandle then begin
          { extract Tar and set stream up }
          GzHelp.SeekToItemData;
          GzHelp.ExtractItemData(FTarStream);
          SwapToTar;
          inherited LoadArchive;
        end;
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
  UncompressedStream  : TStream;
  FileTime            : LongInt;
  SaveDir             : string;
  CurItem             : TAbGzipItem;
begin
  {prepare for the try..finally}
  InGzHelp  := nil;
  OutGzHelp := nil;
  NewStream := nil;

  try
    {init new archive stream}
    NewStream := TAbVirtualMemoryStream.Create;
    OutGzHelp := TAbGzipStreamHelper.Create(NewStream);

    { create helper }
    NewStream.SwapFileDirectory := ExtractFilePath(AbGetTempFile(FTempDir, False));

    if IsGzippedTar and TarAutoHandle then begin
      { save the Tar data first }
      SwapToTar;
      inherited SaveArchive;
      FTarStream.Position := 0; // rewind tar stream

      FGZItem.Clear;
      CurItem := TAbGzipItem.Create;
      FGzItem.Add(CurItem);

      { read gzip header }
      FGZStream.Position := 0;
      CurItem.LoadGzHeaderFromStream(FGzStream);
      { Set modtime to time of compression start. }
      if (not CurItem.HasFileName) or (CurItem.FGzHeader.ModTime = 0) then
        CurItem.LastModDateTime := SysUtils.Now;

      { write new gzip stream }
      CurItem.SaveGzHeaderToStream(NewStream);
      OutGzHelp.WriteArchiveItem(FTarStream);
      OutGzHelp.WriteArchiveTail;

      CurItem.CRC32            := OutGzHelp.FItem.CRC32;
      CurItem.UncompressedSize := OutGzHelp.FItem.UncompressedSize;
      CurItem.CompressedSize   := OutGzHelp.FItem.CompressedSize;

      SwapToGzip;
    end

    else  // GZip

    begin

      InGzHelp := TAbGzipStreamHelper.Create(FGzStream);

      SwapToGzip;

      {build new archive from existing archive}
      for i := 0 to pred(Count) do begin
        FCurrentItem := ItemList[i];
        CurItem      := TAbGzipItem(ItemList[i]);

        case CurItem.Action of
          aaNone, aaMove : begin
          {just copy the file to new stream}
            InGzHelp.SeekToItemData;
            CurItem.SaveGzHeaderToStream(NewStream);
            if InGzHelp.FItem.CompressedSize > 0 then
              NewStream.CopyFrom(FGzStream, InGzHelp.FItem.CompressedSize);
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
                try
                  GetDir(0, SaveDir);
                  try {SaveDir}
                    if (BaseDirectory <> '') then
                      ChDir(BaseDirectory);

                    UncompressedStream := TFileStreamEx.Create(CurItem.DiskFileName,
                        fmOpenRead or fmShareDenyWrite );

                    {Now get the file's attributes}
                    //Attrs := AbFileGetAttr(CurItem.DiskFileName);
                    //CurItem.ExternalFileAttributes := Attrs;
                    //CurItem.SystemSpecificAttributes := Attrs;
                    CurItem.UncompressedSize := UncompressedStream.Size;

                  finally {SaveDir}
                    ChDir( SaveDir );
                  end; {SaveDir}

                  FileTime := AbGetFileTime(CurItem.DiskFileName);
                  CurItem.SystemSpecificLastModFileTime := FileTime;

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

      { only write tail if there is a compressed stream,
        i.e., the only item was not deleted }
      if NewStream.Size > 0 then
        OutGzHelp.WriteArchiveTail;
    end; {else GZip}

    {copy new stream to FStream}
    NewStream.Position := 0;
    if (FStream is TMemoryStream) then
      TMemoryStream(FStream).LoadFromStream(NewStream)
    else begin
      { need new stream to write }
      FreeAndNil(FStream);
      FGzStream := nil;
      // GZIP does not support spanning
      FStream := TFileStreamEx.Create(FArchiveName, fmCreate or fmShareDenyWrite);
      try
        FGZStream := FStream;
        if NewStream.Size > 0 then
          FStream.CopyFrom(NewStream, NewStream.Size);
      except
        raise EAbException.Create('Unable to create new file stream');
      end;
    end;

    {update Items list}
    for i := pred( Count ) downto 0 do begin
      if ItemList[i].Action = aaDelete then
        FItemList.Delete( i )
      else if ItemList[i].Action <> aaFailed then
        ItemList[i].Action := aaNone;
    end;

    if IsGzippedTar and TarAutoHandle then
      SwapToTar;

    DoArchiveSaveProgress( 100, Abort );                               {!!.04}
    DoArchiveProgress( 100, Abort );
  finally
    if Assigned(InGzHelp) then
      InGzHelp.Free;
    if Assigned(OutGzHelp) then
      OutGzHelp.Free;
    if Assigned(NewStream) then
      NewStream.Free;
  end;
end;

procedure TAbGzipArchive.SetTarAutoHandle(const Value: Boolean);
begin
  if Value then
    SwapToTar
  else
    SwapToGzip;
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
