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
{* ABBREVIA: AbBZipTyp.pas 3.05                            *}
{*********************************************************}
{* ABBREVIA: TAbBzipArchive, TAbBzipItem classes         *}
{*********************************************************}
{* Taken From http://digilander.libero.it/pellegrinoluna0/  *}
{*********************************************************}

(*  This file is a part of bzip2 and/or libbzip2, a program and
  library for lossless, block-sorting data compression.

  Copyright (C) 1996-2005 Julian R Seward.  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software.  If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.

  3. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.

  4. The name of the author may not be used to endorse or promote
     products derived from this software without specific prior written
     permission.

  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  Julian Seward, Cambridge, UK.
  jseward@bzip.org
  bzip2/libbzip2 version 1.0 of 21 March 2000

  This program is based on (at least) the work of:
     Mike Burrows
     David Wheeler
     Peter Fenwick
     Alistair Moffat
     Radford Neal
     Ian H. Witten
     Robert Sedgewick
     Jon L. Bentley

  For more information on these sources, see the manual. *)
{*********************************************************}

{$I AbDefine.inc}

unit AbBZipTyp;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
// Removed Not needed: [ 871613 ] AB305B - QT dependancy in AbGzTyp
//  {$IFDEF LINUX}
//  QDialogs,
//  {$ENDIF}
  SysUtils, Classes,

  AbConst, AbExcept, AbUtils, AbArcTyp, AbTarTyp,
  AbDfBase, AbDfDec, AbDfEnc, AbVMStrm, AbBitBkt, AbSpanSt;

type
  { pre-defined "operating system" (really more FILE system)
    types for the Gzip header }
  TAbBZipFileSystem =
    (osFat, osAmiga, osVMS, osUnix, osVM_CMS, osAtariTOS,
    osHPFS, osMacintosh, osZSystem, osCP_M, osTOPS20,
    osNTFS, osQDOS, osAcornRISCOS, osUnknown, osUndefined);

type
  PAbBZipHeader = ^TAbBZipHeader;
  TAbBZipHeader = packed record  { SizeOf(TGzHeader) = 10}
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

  TAbBZipTailRec = packed record
    CRC32 : LongInt;  { crc for uncompressed data }
    ISize : LongInt;  { size of uncompressed data }
  end;

type
  TAbBZipItem = class(TAbArchiveItem)
  private
//    FCRC32: LongInt;
  protected {private}
    FBZipHeader : TAbBZipHeader;
    FIsText : Boolean;
    FCRC16 : ShortInt;
    FXLen  : ShortInt;
    FExtraField, FFileComment : string;
    FIncludeHeaderCrc: Boolean;
  protected
    function GetExtraField: string;
    function GetFileSystem: TAbBZipFileSystem;
    function GetFileComment: string;
    function GetHeaderCRC: Word;
    function GetHasExtraField: Boolean;
    function GetHasFileComment: Boolean;
    function GetHasHeaderCRC: Boolean;
    function GetHasFileName: Boolean;
    function GetIsText: Boolean;

    procedure SetExtraField(const Value: string);
    procedure SetFileComment(const Value : string);
    procedure SetFileSystem(const Value: TAbBZipFileSystem);
    procedure SetIsText(const Value: Boolean);

    function GetExternalFileAttributes : LongInt; override;
    function GetIsEncrypted : Boolean; override;
    function GetLastModFileDate : Word; override;
    function GetLastModFileTime : Word; override;
    function GetSystemSpecificAttributes: LongWord; override;

    procedure SetExternalFileAttributes( Value : LongInt ); override;
    procedure SetFileName(const Value : string); override;
    procedure SetIsEncrypted(Value : Boolean); override;
    procedure SetLastModFileDate(const Value : Word); override;
    procedure SetLastModFileTime(const Value : Word); override;

    procedure SaveBzipHeaderToStream(AStream : TStream);
    procedure LoadBZipHeaderFromStream(AStream : TStream);
  public
//    property CRC32 : LongInt
//      read FCRC32 write FCRC32;

    property CompressionMethod : Byte
      read FBZipHeader.CompMethod write FBZipHeader.CompMethod;

    property ExtraFlags : Byte {Default: 2}
      read FBZipHeader.XtraFlags write FBZipHeader.XtraFlags;

    property Flags : Byte
      read FBZipHeader.Flags write FBZipHeader.Flags;

    property FileComment : string
      read GetFileComment write SetFileComment;

    property FileSystem : TAbBZipFileSystem {Default: osFat (Windows); osUnix (Linux)}
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

    property BZipHeader : TAbBZipHeader
      read FBZipHeader write FBZipHeader;

    property IncludeHeaderCrc : Boolean
      read FIncludeHeaderCrc write FIncludeHeaderCrc;

    constructor Create; override;
  end;

  TAbBzipStreamHelper = class(TAbArchiveStreamHelper)
  private
    function GetBzipCRC: LongInt;
    function GetFileSize: LongWord;
  protected {private}
    FItem : TAbBzipItem;
    FTail : TAbBZipTailRec;
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
      read GetBZipCRC;
    property FileSize : LongWord
      read GetFileSize;
    property TailCRC : LongInt
      read FTail.CRC32;
    property TailSize : LongInt
      read FTail.ISize;
  end;

  TAbBzipArchiveState = (gsBzip, gsTar);

  TAbBzipArchive = class(TAbTarArchive)
  private
    FBZipStream  : TStream;        { stream for BZip file}
    FBZipItem    : TAbArchiveList; { item in Bzip (only one, but need polymorphism of class)}
    FTarStream : TStream;        { stream for possible contained Tar }
    FTarList   : TAbArchiveList; { items in possible contained Tar }
    FTarAutoHandle: Boolean;
    FTarLoaded : Boolean;
    FState     : TAbBzipArchiveState;
    FIsBzippedTar : Boolean;

    procedure SetTarAutoHandle(const Value: Boolean);
    function GetIsBzippedTar: Boolean;
    procedure SwapToBzip;
    procedure SwapToTar;

  protected
    function CreateItem(const SourceFileName   : string;
                        const ArchiveDirectory : string): TAbArchiveItem;
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
    function FixName(const Value : string) : string;
      override;

//    function Decompress: integer;
    function GetItem(Index: Integer): TAbBzipItem;                  {!!.03}
    procedure PutItem(Index: Integer; const Value: TAbBzipItem);    {!!.03}
  public {methods}
    constructor Create(const FileName : string; Mode : Word);
      override;
    constructor CreateFromStream(aStream : TStream; const aArchiveName : string); override;
    destructor  Destroy;
      override;

    procedure DoSpanningMediaRequest(Sender : TObject; ImageNumber : Integer;
      var ImageName : string; var Abort : Boolean); override;

    property TarAutoHandle : Boolean
      read FTarAutoHandle write SetTarAutoHandle;

    property IsBzippedTar : Boolean
      read GetIsBzippedTar write FIsBzippedTar;

    property Items[Index : Integer] : TAbBzipItem                    {!!.03}
      read GetItem                                                   {!!.03}
      write PutItem; default;                                        {!!.03}
  end;

function VerifyBZip(Strm : TStream) : TAbArchiveType;
function BZipOsToStr(OS: Byte) : string;

implementation

const
  { Header Signature Values}
  AB_BZIP_HDR_ID1 = $1F;
  AB_BZIP_HDR_ID2 = $8B;

  { Test bits for TGzHeader.Flags field }
  AB_BZIP_FLAG_FTEXT    = $01;
  AB_BZIP_FLAG_FHCRC    = $02;
  AB_BZIP_FLAG_FEXTRA   = $04;
  AB_BZIP_FLAG_FNAME    = $08;
  AB_BZIP_FLAG_FCOMMENT = $10;

  { GZip OS source flags }
  AB_BZIP_OS_ID_FAT         = 0;
  AB_BZIP_OS_ID_Amiga       = 1;
  AB_BZIP_OS_ID_VMS         = 2;
  AB_BZIP_OS_ID_Unix        = 3;
  AB_BZIP_OS_ID_VM_CMS      = 4;
  AB_BZIP_OS_ID_AtariTOS    = 5;
  AB_BZIP_OS_ID_HPFS        = 6;
  AB_BZIP_OS_ID_Macintosh   = 7;
  AB_BZIP_OS_ID_Z_System    = 8;
  AB_BZIP_OS_ID_CP_M        = 9;
  AB_BZIP_OS_ID_TOPS20      = 10;
  AB_BZIP_OS_ID_NTFS        = 11;
  AB_BZIP_OS_ID_QDOS        = 12;
  AB_BZIP_OS_ID_AcornRISCOS = 13;
  AB_BZIP_OS_ID_unknown     = 255;

function BZipOsToStr(OS: Byte) : string;
{
Return a descriptive string for TGzHeader.OS field
}
begin
  case OS of
    AB_BZIP_OS_ID_FAT         : Result := AbGzOsFat;
    AB_BZIP_OS_ID_Amiga       : Result := AbGzOsAmiga;
    AB_BZIP_OS_ID_VMS         : Result := AbGzOsVMS;
    AB_BZIP_OS_ID_Unix        : Result := AbGzOsUnix;
    AB_BZIP_OS_ID_VM_CMS      : Result := AbGzOsVM_CMS;
    AB_BZIP_OS_ID_AtariTOS    : Result := AbGzOsAtari;
    AB_BZIP_OS_ID_HPFS        : Result := AbGzOsHPFS;
    AB_BZIP_OS_ID_Macintosh   : Result := AbGzOsMacintosh;
    AB_BZIP_OS_ID_Z_System    : Result := AbGzOsZ_System;
    AB_BZIP_OS_ID_CP_M        : Result := AbGzOsCP_M;
    AB_BZIP_OS_ID_TOPS20      : Result := AbGzOsTOPS_20;
    AB_BZIP_OS_ID_NTFS        : Result := AbGzOsNTFS;
    AB_BZIP_OS_ID_QDOS        : Result := AbGzOsQDOS;
    AB_BZIP_OS_ID_AcornRISCOS : Result := AbGzOsAcornRISCOS;
    AB_BZIP_OS_ID_unknown     : Result := AbGzOsunknown;
  else
    Result := AbGzOsUndefined;
  end;
end;


function VerifyHeader(const Header : TAbBZipHeader) : Boolean;
begin
  { check id fields and if deflated (only handle deflate anyway)}
  Result := (Header.ID1 = AB_BZIP_HDR_ID1) and
     (Header.ID2 = AB_BZIP_HDR_ID2) and
     (Header.CompMethod = 8 {deflate});
end;

function VerifyBZip(Strm : TStream) : TAbArchiveType;
var
{  Hdr : TAbGzHeader; }
  BHlp : TAbBzipStreamHelper;
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

  BHlp := TAbBzipStreamHelper.Create(Strm);
  try
    {create the stream helper and read the item header}
    BHlp.ReadHeader;

    { check id fields and if deflated (only handle deflate anyway)}
    if VerifyHeader(BHlp.FItem.FBZipHeader) then begin
      Result := atBZip; { provisional }

      { check if is actually a Gzipped Tar }
      { partial extract contents, verify vs. Tar }
      PartialTarData := TMemoryStream.Create;
      BHlp.SeekToItemData;
      Hlpr := TAbDeflateHelper.Create;
//      Hlpr.LogFile := 'c:\gzip.log'; {REMOVE}
      Hlpr.PartialSize := 512;
      PartialTarData.SetSize(512 * 2);
      Inflate(Strm, PartialTarData, Hlpr);

      {set to beginning of extracted data}
      PartialTarData.Position := 0;

      if (VerifyTar(PartialTarData) = atTar) then
        Result := atBZippedTar;
    end;
  finally
    BHlp.Free;
    Hlpr.Free;
    PartialTarData.Free;

    Strm.Position := CurPos;
  end;
end;


{ TAbBzipStreamHelper }

constructor TAbBzipStreamHelper.Create(AStream : TStream);
begin
  inherited Create(AStream);
  FItem := TAbBzipItem.Create;
end;

destructor TAbBzipStreamHelper.Destroy;
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

procedure TAbBzipStreamHelper.SeekToItemData;
{find end of header data, including FileName etc.}
begin
  {** Seek to Compressed Data **}
  FStream.Seek(0, soFromBeginning);
  FItem.LoadBZipHeaderFromStream(FStream);
end;

procedure TAbBzipStreamHelper.ExtractItemData(AStream: TStream);
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

function TAbBzipStreamHelper.FindFirstItem: Boolean;
var
  BZH : TAbBZipHeader;
  DataRead : Integer;
begin
  Result := False;
  FStream.Seek(0, soFromBeginning);
  DataRead := FStream.Read(BZH, SizeOf(TAbBZipHeader));
  if (DataRead = SizeOf(TAbBZipHeader)) and VerifyHeader(BZH) then begin
    FItem.BZipHeader := BZH;
    Result := True;
  end;
  FStream.Seek(0, soFromBeginning);
end;

function TAbBzipStreamHelper.FindNextItem: Boolean;
begin
  { only one item in a GZip }
  Result := False;
end;

function TAbBzipStreamHelper.SeekItem(Index: Integer): Boolean;
begin
  if Index > 0 then
    Result := False
  else
    Result := FindFirstItem;
end;

procedure TAbBzipStreamHelper.WriteArchiveHeader;
begin
  FItem.SaveBZipHeaderToStream(FStream);
end;

procedure TAbBzipStreamHelper.WriteArchiveItem(AStream: TStream);
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

procedure TAbBzipStreamHelper.WriteArchiveTail;
var
  Tail : TAbBZipTailRec;
begin
  Tail.CRC32 := FItem.CRC32;
  Tail.ISize := FItem.UncompressedSize;
  FStream.Write(Tail, SizeOf(TAbBZipTailRec));
end;

function TAbBzipStreamHelper.GetItemCount: Integer;
begin
  { only one item in a gzip }
  Result := 1;
end;

procedure TAbBzipStreamHelper.ReadHeader;
begin
  FItem.LoadBZipHeaderFromStream(FStream);
end;

procedure TAbBzipStreamHelper.ReadTail;
begin
  FStream.Read(FTail, SizeOf(TAbBZipTailRec));
end;

function TAbBzipStreamHelper.GetBZipCRC: LongInt;
begin
  Result := FItem.CRC32;
end;

function TAbBzipStreamHelper.GetFileSize: LongWord;
begin
  Result := FItem.UncompressedSize;
end;

{ TAbBzipItem }

constructor TAbBzipItem.Create;
begin
{ set defaults }
  { Maxium Compression }
  FBZipHeader.XtraFlags := 2;

  FileName := '';
  FFileComment := '';
  FExtraField := '';

  { source OS ID }
{$IFDEF LINUX } {assume EXT2 system }
  FBZipHeader.OS := AB_BZIP_OS_ID_Unix;
{$ENDIF LINUX }
{$IFDEF MSWINDOWS } {assume FAT system }
  FBZipHeader.OS := AB_BZIP_OS_ID_FAT;
{$ENDIF MSWINDOWS }

  FIncludeHeaderCrc := False;
end;

function TAbBzipItem.GetExternalFileAttributes: LongInt;
begin
  { BZip has no provision for storing attributes }
  Result := AB_FPERMISSION_GENERIC or AB_FMODE_FILE;
end;

function TAbBzipItem.GetExtraField: string;
begin
  Result := '';
  if HasExtraField then begin
    SetLength(Result, FXLen);
    Move(FExtraField, Result[1], FXLen);
  end;
end;

function TAbBzipItem.GetFileComment: string;
begin
  Result := '';
  if HasFileComment then
    Result := FFileComment;
end;


function TAbBzipItem.GetFileSystem: TAbBZipFileSystem;
begin
  case FBZipHeader.OS of
    0..13: Result := TAbBZipFileSystem(FBZipHeader.OS);
    255:   Result := osUnknown;
    else
      Result := osUndefined;
  end; { case }
end;

function TAbBzipItem.GetHeaderCRC: Word;
begin
  Result := 0;
  if HasHeaderCRC then
    Result := FCRC16;
end;

function TAbBzipItem.GetIsEncrypted: Boolean;
begin
{ GZip doesn't support any native encryption }
  Result := False;
end;

function TAbBzipItem.GetHasExtraField: Boolean;
begin
  Result := (FBZipHeader.Flags and AB_BZIP_FLAG_FEXTRA) = AB_BZIP_FLAG_FEXTRA;
end;

function TAbBzipItem.GetHasFileComment: Boolean;
begin
  Result := (FBZipHeader.Flags and AB_BZIP_FLAG_FCOMMENT) = AB_BZIP_FLAG_FCOMMENT;
end;

function TAbBzipItem.GetHasFileName: Boolean;
begin
  Result := (FBZipHeader.Flags and AB_BZIP_FLAG_FNAME) = AB_BZIP_FLAG_FNAME;
end;

function TAbBzipItem.GetHasHeaderCRC: Boolean;
begin
  Result := (FBZipHeader.Flags and AB_BZIP_FLAG_FHCRC) = AB_BZIP_FLAG_FHCRC;
end;

function TAbBzipItem.GetIsText: Boolean;
begin
  Result := (FBZipHeader.Flags and AB_BZIP_FLAG_FTEXT) = AB_BZIP_FLAG_FTEXT;
end;

function TAbBzipItem.GetLastModFileDate: Word;
var
  Rslt : LongInt;
  D : TDateTime;
begin
  { convert to TDateTime }
  D := AbUnixTimeToDateTime(FBZIPHeader.ModTime);

  { convert to DOS file Date }
  Rslt := DateTimeToFileDate(D);
  Result := LongRec(Rslt).Hi;
end;

function TAbBzipItem.GetLastModFileTime: Word;
var
  Rslt : LongInt;
  D : TDateTime;
begin
  { convert to TDateTime }
  D := AbUnixTimeToDateTime(FBZipHeader.ModTime);

  { convert to DOS file Time }
  Rslt := DateTimeToFileDate(D);
  Result := LongRec(Rslt).Lo;
end;

function TAbBzipItem.GetSystemSpecificAttributes: LongWord;
begin
  Result := GetExternalFileAttributes;
{$IFDEF MSWINDOWS}
  Result := AbUnix2DosFileAttributes(Result);
{$ENDIF}
end;

procedure TAbBzipItem.LoadBZipHeaderFromStream(AStream: TStream);
var
  StartPos : LongInt;
  Len      : LongInt;
  LenW     : Word;
  CRC16    : ShortInt;
  tempFileName: string;
begin
  AStream.Read(FBZipHeader, SizeOf(TAbBZipHeader));

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
    SetLength(tempFileName, Len);
    AStream.Read(tempFileName[1], Len + 1);
  end
  else
    tempFileName := 'unknown';

  FileName := tempFileName;

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
  {If file was compressed with 3.3 spec this will be invalid so use with care}
  CompressedSize := AStream.Size - AStream.Position - SizeOf(TAbBZipTailRec);

  AbUnfixName(tempFileName);
  DiskFileName := tempFileName;
  Action := aaNone;
  Tagged := False;
end;

procedure TAbBzipItem.SaveBZipHeaderToStream(AStream: TStream);
var
  HBuff, HPtr : PAnsiChar;
  HSize, I32 : LongInt;
  I16 : ShortInt;
  LenW  : Word;
  tempFileName: string;
begin
  { start with basic header record }
  HSize := SizeOf(TAbBZipHeader);

  { default ID fields }
  FBZipHeader.ID1 := AB_BZIP_HDR_ID1;
  FBZipHeader.ID2 := AB_BZIP_HDR_ID2;

  { compression method }
  FBZipHeader.CompMethod := 8;  { deflate }

  { flags }
  FBZipHeader.Flags := 0;

  { provide for the header CRC }
  if IncludeHeaderCRC then begin
    FBzipHeader.Flags := AB_BZIP_FLAG_FHCRC;
    Inc(HSize, 2);
  end;

  { add Text flag if user has set it }
  if IsText then
    FBZipHeader.Flags := FBZipHeader.Flags or AB_BZIP_FLAG_FTEXT;

  { any Extra Field Present? }
  if FExtraField > '' then begin
    FBZipHeader.Flags := FBZipHeader.Flags or AB_BZIP_FLAG_FEXTRA;
    HSize := HSize + 2 + Length(FExtraField);
  end;

  tempFileName := FileName;
  { any File Name present? }
  if tempFileName > '' then begin
    FBZipHeader.Flags := FBZipHeader.Flags or AB_BZIP_FLAG_FNAME;
    HSize := HSize + Length(tempFileName) + 1;
  end;

  { any File Comment present? }
  if FFileComment > '' then begin
    FBZipHeader.Flags := FBZipHeader.Flags or AB_BZIP_FLAG_FCOMMENT;
    HSize := HSize + Length(FFileComment) + 1;
  end;

  { build the header plus extra info }
  GetMem(HBuff, HSize);
  try
    HPtr := HBuff;

    { main header data }
    Move(FBZipHeader, HPtr^, SizeOf(TAbBZipHeader));
    Inc(HPtr, SizeOf(TAbBZipHeader));

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
      Move(tempFileName[1], HPtr^, succ(length(tempFileName)));
      Inc(HPtr, succ(length(tempFileName)));
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


procedure TAbBzipItem.SetExternalFileAttributes(Value: LongInt);
begin
  { do nothing }
end;

procedure TAbBzipItem.SetExtraField(const Value: string);
begin
  FExtraField := '';

  if Value > '' then begin
    FExtraField := Value;
    FBZipHeader.Flags := FBZipHeader.Flags or AB_BZIP_FLAG_FEXTRA;
  end
  else begin
    FBZipHeader.Flags := FBZipHeader.Flags and not AB_BZIP_FLAG_FEXTRA;
  end;
end;

procedure TAbBzipItem.SetFileComment(const Value: string);
begin
  FFileComment := '';

  if Value > '' then begin
    FFileComment := Value;
    FBZipHeader.Flags := FBZipHeader.Flags or AB_BZIP_FLAG_FCOMMENT;
  end
  else begin
    FBZipHeader.Flags := FBZipHeader.Flags and not AB_BZIP_FLAG_FCOMMENT;
  end;
end;

procedure TAbBzipItem.SetFileName(const Value: string);
begin
  if FileName <> '' then
     FileName := '';

  if Value > '' then begin
    FileName := Value { + #0};
    FBZipHeader.Flags := FBZipHeader.Flags or AB_BZIP_FLAG_FNAME;
  end
  else begin
    FBZipHeader.Flags := FBZipHeader.Flags and not AB_BZIP_FLAG_FNAME;
  end;
end;

procedure TAbBzipItem.SetFileSystem(const Value: TAbBZipFileSystem);
begin
  if Value = osUnknown then
    FBZipHeader.OS := 255
  else
    FBZipHeader.OS := Ord(Value);
end;

procedure TAbBzipItem.SetIsEncrypted(Value: Boolean);
begin
  { do nothing }
end;

procedure TAbBzipItem.SetIsText(const Value: Boolean);
begin
  FIsText := Value;
  case FIsText of
    True  : FBZipHeader.Flags := FBZipHeader.Flags or AB_BZIP_FLAG_FTEXT;
    False : FBZipHeader.Flags := FBZipHeader.Flags and not AB_BZIP_FLAG_FTEXT;
  end;
end;

procedure TAbBzipItem.SetLastModFileDate(const Value: Word);
var
  D : TDateTime;
  UT : LongInt;
begin
  UT := FBZipHeader.ModTime;

  { keep seconds in current day, discard date's seconds }
  UT := UT mod SecondsInDay;

  { build new date }
  D := EncodeDate(Value shr 9 + 1980, Value shr 5 and 15, Value and 31);

  { add to unix second count }
  UT :=  UT + AbDateTimeToUnixTime(D);

  { store back in header }
  FBZipHeader.ModTime := UT;
end;

procedure TAbBzipItem.SetLastModFileTime(const Value: Word);
var
  T : TDateTime;
  UT : LongInt;
begin
  UT := FBZipHeader.ModTime;

  { keep seconds in current date, discard day's seconds }
  UT := UT - (UT mod SecondsInDay);

  { build new time }
  T := EncodeTime(Value shr 11, Value shr 5 and 63, Value and 31 shl 1, 0);

  { add to unix second count }
  UT := UT + AbDateTimeToUnixTime(T);

  { store back in header }
  FBZipHeader.ModTime := UT;
end;

{ TAbGzipArchive }

constructor TAbBzipArchive.Create(const FileName: string; Mode: Word);
begin
  inherited Create(FileName, Mode);
  FTarLoaded := False;
  FState    := gsBzip;
// Replaced to use TFileStream instead of TabSpanStream
// This feels like a hack to do this here.
//  FGZStream  := FStream;  { save reference to opened file stream }
  fStream.Free;
  FBZipStream  := TFileStream.Create(FileName,Mode);
  fStream    := FBZipStream;
  FBZipItem    := FItemList;
  FTarStream := TAbVirtualMemoryStream.Create;
  FTarList   := TAbArchiveList.Create;
end;

procedure TAbBzipArchive.SwapToTar;
begin
  FStream := FTarStream;
  FItemList := FTarList;
  FState := gsTar;
end;

procedure TAbBzipArchive.SwapToBzip;
begin
  FStream := FBzipStream;
  FItemList := FBZipItem;
  FState := gsBzip;
end;

function TAbBzipArchive.CreateItem(const SourceFileName   : string;
                                   const ArchiveDirectory : string): TAbArchiveItem;
var
  Buff : array [0..511] of Char;
  BZipItem : TAbBzipItem;
begin
  if IsBZippedTar and TarAutoHandle then begin
    if FState <> gsTar then
      SwapToTar;
    Result := inherited CreateItem(FileSpec);
  end
  else begin
    SwapToBzip;
    BZipItem := TAbBzipItem.Create;
    try
      BZipItem.CompressedSize := 0;
      BZipItem.CRC32 := 0;
      StrPCopy(Buff, ExpandFileName(FileSpec));
      BZipItem.DiskFileName := StrPas(Buff);
      StrPCopy(Buff, FixName(FileSpec));
      BZipItem.FileName := StrPas(Buff);
      Result := BZipItem;
    except
      Result := nil;
    end;
  end;
end;

destructor TAbBzipArchive.Destroy;
begin
  SwapToBzip;
  FTarList.Free;
  FTarStream.Free;
  inherited Destroy;
end;


procedure TAbBzipArchive.ExtractItemAt(Index: Integer;
  const NewName: string);
var
  OutStream : TFileStream;
  UseName : string;
  CurItem : TAbBzipItem;
begin
  if IsBZippedTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemAt(Index, NewName);
  end
  else begin
    SwapToBzip;
    if Index > 0 then Index := 0; { only one item in a GZip}

    UseName := NewName;
    CurItem := TAbBzipItem(ItemList[Index]);

    { check if path to save to is okay }
    if AbConfirmPath(BaseDirectory, UseName, ExtractOptions, FOnConfirmOverwrite) then
    begin
      OutStream := TFileStream.Create(UseName, fmCreate or fmShareDenyNone);

      try
        try {OutStream}
          ExtractItemToStreamAt(Index, OutStream);
        finally {OutStream}
          OutStream.Free;
        end; {OutStream}

        // [ 880505 ]  Need to Set Attributes after File is closed {!!.05}
        AbSetFileDate(UseName, CurItem.SystemSpecificLastModFileTime);
        AbFileSetAttr(UseName, CurItem.SystemSpecificAttributes);

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

procedure TAbBzipArchive.ExtractItemToStreamAt(Index: Integer;
  aStream: TStream);
var
  BZipHelp  : TAbBzipStreamHelper;
begin
  if IsBzippedTar and TarAutoHandle then begin
    SwapToTar;
    inherited ExtractItemToStreamAt(Index, aStream);
  end
  else begin
    SwapToBzip;
    { note Index ignored as there's only one item in a GZip }

    BZipHelp := TAbBzipStreamHelper.Create(FBZipStream);
    try
      { read GZip Header }
      BZipHelp.ReadHeader;

      { extract copy data from BZip}
      BZipHelp.ExtractItemData(aStream);

      { Get validation data }
      BZipHelp.ReadTail;

    finally
      BZipHelp.Free;
    end;
  end;
end;

function TAbBzipArchive.FixName(const Value: string): string;
{ fix up fileaname for storage }
begin
  {BZip files Always strip the file path}
  StoreOptions := StoreOptions + [soStripDrive, soStripPath];
  Result := '';
  if Value <> '' then
    Result := ExtractFileName(Value);
end;

function TAbBzipArchive.GetIsBzippedTar: Boolean;
begin
  Result := FIsBzippedTar;
end;

{!!.03 -- Added }
function TAbBzipArchive.GetItem(Index: Integer): TAbBzipItem;
begin
  Result := nil;
  if Index = 0 then
    Result := TAbBzipItem(FItemList.Items[Index]);
end;
{!!.03 -- End Added }

procedure TAbBzipArchive.LoadArchive;
var
  BZipHelp       : TAbBzipStreamHelper;
  Item         : TAbBzipItem;
  ItemFound    : Boolean;
  Abort        : Boolean;
  TotalEntries : Integer;
  i            : Integer;
  Progress     : Byte;
begin
  if FBZipStream.Size = 0 then
    Exit;

  if IsBzippedTar and TarAutoHandle then begin
    { extract Tar and set stream up }

    BZipHelp := TAbBzipStreamHelper.Create(FBZipStream);
    try
      if not FTarLoaded then begin
        BZipHelp.SeekToItemData;
        BZipHelp.ExtractItemData(FTarStream);
        SwapToTar;
        inherited LoadArchive;
        FTarLoaded := True;
      end;
    finally
      BZipHelp.Free;
    end;
  end
  else begin
    SwapToBzip;

    { create helper }
    BZipHelp := TAbBzipStreamHelper.Create(FBZipStream);
    try
      TotalEntries := BZipHelp.GetItemCount;

      {build Items list from tar header records}
      i := 0;

      { reset Tar }
      ItemFound := BZipHelp.FindFirstItem;

      { while more data in Tar }

      if ItemFound then begin
        Item := TAbBzipItem.Create;
        Item.LoadBZipHeaderFromStream(FBZipStream);

        FBZipStream.Seek(-SizeOf(TAbBZipTailRec), soFromEnd);
        BZipHelp.ReadTail;
        Item.CRC32 := BZipHelp.FTail.CRC32;
        Item.UncompressedSize := BZipHelp.FTail.ISize;
//        Item.CRC32 := GZHelp.FItem.FCRC32;
        Item.UncompressedSize := BZipHelp.FItem.UncompressedSize;

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
      BZipHelp.Free;
    end;
  end;
end;

{!!.03 -- Added }
procedure TAbBzipArchive.PutItem(Index: Integer; const Value: TAbBzipItem);
begin
  if Index = 0 then
    FItemList.Items[Index] := Value;
end;
{!!.03 -- End Added }

procedure TAbBzipArchive.SaveArchive;
var
  InBZipHelp, OutBZipHelp : TAbBzipStreamHelper;
  Abort               : Boolean;
  i                   : Integer;
  NewStream           : TAbVirtualMemoryStream;
  WorkingStream       : TAbVirtualMemoryStream;
  UncompressedStream  : TStream;
  FileTime            : LongInt;
  SaveDir             : string;
  CurItem             : TAbBzipItem;
begin
  {prepare for the try..finally}
  OutBZipHelp := nil;
  NewStream := nil;
  WorkingStream := nil;

  try
    InBZipHelp := TAbBzipStreamHelper.Create(FBzipStream);

    try
      if IsBzippedTar and TarAutoHandle then begin
        { save the Tar data first }
        SwapToTar;
        inherited SaveArchive;

        { update contents of GZip Stream with new Tar }
        FBZipStream.Position := 0;
        InBZipHelp.ReadHeader;
        FBZipStream.Size := 0;
        InBZipHelp.WriteArchiveHeader;
        InBZipHelp.WriteArchiveItem(FTarStream);
        InBZipHelp.WriteArchiveTail;
      end;

      SwapToBzip;

      {init new archive stream}
      NewStream := TAbVirtualMemoryStream.Create;
      OutBZipHelp := TAbBzipStreamHelper.Create(NewStream);

      { create helper }
      NewStream.SwapFileDirectory := ExtractFilePath(AbGetTempFile(FTempDir, False));

      {build new archive from existing archive}
      for i := 0 to pred(Count) do begin
        FCurrentItem := ItemList[i];
        CurItem      := TAbBzipItem(ItemList[i]);
        InBZipHelp.SeekToItemData;

        case CurItem.Action of
          aaNone, aaMove : begin
          {just copy the file to new stream}
            WorkingStream := TAbVirtualMemoryStream.Create;
            InBZipHelp.SeekToItemData;
            InBZipHelp.ExtractItemData(WorkingStream);
            WorkingStream.Position := 0;
            CurItem.SaveBZipHeaderToStream(NewStream);
            OutBZipHelp.WriteArchiveItem(WorkingStream);
          end;

          aaDelete: {doing nothing omits file from new stream} ;

          aaAdd, aaFreshen, aaReplace, aaStreamAdd: begin
            try
              if (CurItem.Action = aaStreamAdd) then begin
              { adding from a stream }
                CurItem.SaveBZipHeaderToStream(NewStream);
                CurItem.UncompressedSize := InStream.Size;
                OutBZipHelp.WriteArchiveItem(InStream);
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

                  FileTime := FileAge(CurItem.DiskFileName);
                  CurItem.SystemSpecificLastModFileTime := FileTime;

                  CurItem.SaveBZipHeaderToStream(NewStream);
                  OutBZipHelp.WriteArchiveItem(UncompressedStream);

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
      InBZipHelp.Free;
    end;

    {copy new stream to FStream}
    OutBZipHelp.WriteArchiveTail;
    NewStream.Position := 0;
    if (FStream is TMemoryStream) then
      TMemoryStream(FStream).LoadFromStream(NewStream)
    else begin
      { need new stream to write }
      FStream.Free;
      // GZIP does not support spanning, removing use of AbSpanStream.
//      FStream := TAbSpanStream.Create(FArchiveName, fmOpenWrite or fmShareDenyWrite, mtLocal, FSpanningThreshold);
      FStream := TFileStream.Create(FArchiveName,fmOpenWrite or fmShareDenyWrite);
//    TAbSpanStream(FStream).OnRequestImage := DoSpanningMediaRequest;   {!!.01}
//    TAbSpanStream(FStream).OnArchiveProgress := DoArchiveSaveProgress; {!!.04}
      try
        if NewStream.Size > 0 then
          FStream.CopyFrom(NewStream, NewStream.Size);
        FBZipStream := FStream;
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
    OutBZipHelp.Free;
    NewStream.Free;
  end;
end;

procedure TAbBzipArchive.SetTarAutoHandle(const Value: Boolean);
begin
  case Value of
    True  : begin
      SwapToTar;
    end;

    False : begin
      SwapToBZip;
    end;
  end;
  FTarAutoHandle := Value;
end;

procedure TAbBzipArchive.TestItemAt(Index: Integer);
var
  SavePos   : LongInt;
  BZipType    : TAbArchiveType;
  BitBucket : TAbBitBucketStream;
  BZipHelp    : TAbBzipStreamHelper;
begin
  if IsBzippedTar and TarAutoHandle then begin
    inherited TestItemAt(Index);
  end
  else begin
    { note Index ignored as there's only one item in a GZip }
    SavePos := FBZipStream.Position;
    BZipType := VerifyBZip(FBZipStream);
    if not (BZipType in [atBZip, atBZippedTar]) then
      raise EAbBZipInvalid.Create;

    BitBucket := nil;
    BZipHelp := nil;
    try
      BitBucket := TAbBitBucketStream.Create(1024);
      BZipHelp := TAbBzipStreamHelper.Create(FBZipStream);

      BZipHelp.ExtractItemData(BitBucket);
      BZipHelp.ReadTail;

      { validate against CRC }
      if BZipHelp.FItem.Crc32 <> BZipHelp.TailCRC then
        raise EAbBzipBadCRC.Create;

      { validate against file size }
      if BZipHelp.FItem.UncompressedSize <> BZipHelp.TailSize then
        raise EAbBzipBadFileSize.Create;

    finally
      BZipHelp.Free;
      BitBucket.Free;
    end;

    FBZipStream.Position := SavePos;
  end;
end;

procedure TAbBzipArchive.DoSpanningMediaRequest(Sender: TObject;
  ImageNumber: Integer; var ImageName: string; var Abort: Boolean);
begin
  Abort := False;
end;

constructor TAbBzipArchive.CreateFromStream(aStream: TStream;
  const aArchiveName: string);
begin
 // [ 858209 ] GZip from stream to stream with TAbGzipArchive renders error
  inherited CreateFromStream(aStream,aArchiveName);
  FTarLoaded := False;
  FState     := gsBZip;
  FBZipItem    := FItemList;
  FTarStream := TAbVirtualMemoryStream.Create;
  FTarList   := TAbArchiveList.Create;
end;

(*
function TAbBzipArchive.BZ2_decompress:ine// ( DState* s )
var
   uc : char;
   retVal,minLen, maxLen : longint;
   bz_stream* strm = s->strm;

   /* stuff that needs to be saved/restored */
   i,j,t.alphaSize,nGroups,nSelectors,EOB,groupNo,groupPos,nextSym,nblockMAX: longint;
   nblock,es,N,curr,zt,zn,zvec,zj,gSel,gMinlen:longint;
   gLimit,gBase,gPerm: pinteger;

   if (s->state == BZ_X_MAGIC_1) {
      /*initialise the save area*/
      s->save_i           = 0;
      s->save_j           = 0;
      s->save_t           = 0;
      s->save_alphaSize   = 0;
      s->save_nGroups     = 0;
      s->save_nSelectors  = 0;
      s->save_EOB         = 0;
      s->save_groupNo     = 0;
      s->save_groupPos    = 0;
      s->save_nextSym     = 0;
      s->save_nblockMAX   = 0;
      s->save_nblock      = 0;
      s->save_es          = 0;
      s->save_N           = 0;
      s->save_curr        = 0;
      s->save_zt          = 0;
      s->save_zn          = 0;
      s->save_zvec        = 0;
      s->save_zj          = 0;
      s->save_gSel        = 0;
      s->save_gMinlen     = 0;
      s->save_gLimit      = NULL;
      s->save_gBase       = NULL;
      s->save_gPerm       = NULL;
   }

   /*restore from the save area*/
   i           = s->save_i;
   j           = s->save_j;
   t           = s->save_t;
   alphaSize   = s->save_alphaSize;
   nGroups     = s->save_nGroups;
   nSelectors  = s->save_nSelectors;
   EOB         = s->save_EOB;
   groupNo     = s->save_groupNo;
   groupPos    = s->save_groupPos;
   nextSym     = s->save_nextSym;
   nblockMAX   = s->save_nblockMAX;
   nblock      = s->save_nblock;
   es          = s->save_es;
   N           = s->save_N;
   curr        = s->save_curr;
   zt          = s->save_zt;
   zn          = s->save_zn;
   zvec        = s->save_zvec;
   zj          = s->save_zj;
   gSel        = s->save_gSel;
   gMinlen     = s->save_gMinlen;
   gLimit      = s->save_gLimit;
   gBase       = s->save_gBase;
   gPerm       = s->save_gPerm;

   retVal = BZ_OK;

   uc := chr(BZ_X_MAGIC_1);
   if uc <> BZ_HDR_B then
      Result := BZ_DATA_ERROR_MAGIC;

   uc := chr(BZ_X_MAGIC_2);
   if uc <> BZ_HDR_Z then
      Result := BZ_DATA_ERROR_MAGIC;

   uc := chr(BZ_X_MAGIC_3);
   if uc <> BZ_HDR_h then
      Result := BZ_DATA_ERROR_MAGIC;

   case s->state of


      GET_BITS(BZ_X_MAGIC_4, s->blockSize100k, 8)
      if (s->blockSize100k < (BZ_HDR_0 + 1) ||
          s->blockSize100k > (BZ_HDR_0 + 9)) RETURN(BZ_DATA_ERROR_MAGIC);
      s->blockSize100k -= BZ_HDR_0;

      if (s->smallDecompress) {
         s->ll16 = BZALLOC( s->blockSize100k * 100000 * sizeof(UInt16) );
         s->ll4  = BZALLOC(
                      ((1 + s->blockSize100k * 100000) >> 1) * sizeof(UChar)
                   );
         if (s->ll16 == NULL || s->ll4 == NULL) RETURN(BZ_MEM_ERROR);
      } else {
         s->tt  = BZALLOC( s->blockSize100k * 100000 * sizeof(Int32) );
         if (s->tt == NULL) RETURN(BZ_MEM_ERROR);
      }

      GET_UCHAR(BZ_X_BLKHDR_1, uc);

      if (uc == 0x17) goto endhdr_2;
      if (uc != 0x31) RETURN(BZ_DATA_ERROR);
      GET_UCHAR(BZ_X_BLKHDR_2, uc);
      if (uc != 0x41) RETURN(BZ_DATA_ERROR);
      GET_UCHAR(BZ_X_BLKHDR_3, uc);
      if (uc != 0x59) RETURN(BZ_DATA_ERROR);
      GET_UCHAR(BZ_X_BLKHDR_4, uc);
      if (uc != 0x26) RETURN(BZ_DATA_ERROR);
      GET_UCHAR(BZ_X_BLKHDR_5, uc);
      if (uc != 0x53) RETURN(BZ_DATA_ERROR);
      GET_UCHAR(BZ_X_BLKHDR_6, uc);
      if (uc != 0x59) RETURN(BZ_DATA_ERROR);

      s->currBlockNo++;
      if (s->verbosity >= 2)
         VPrintf1 ( "\n    [%d: huff+mtf ", s->currBlockNo );

      s->storedBlockCRC = 0;
      GET_UCHAR(BZ_X_BCRC_1, uc);
      s->storedBlockCRC = (s->storedBlockCRC << 8) | ((UInt32)uc);
      GET_UCHAR(BZ_X_BCRC_2, uc);
      s->storedBlockCRC = (s->storedBlockCRC << 8) | ((UInt32)uc);
      GET_UCHAR(BZ_X_BCRC_3, uc);
      s->storedBlockCRC = (s->storedBlockCRC << 8) | ((UInt32)uc);
      GET_UCHAR(BZ_X_BCRC_4, uc);
      s->storedBlockCRC = (s->storedBlockCRC << 8) | ((UInt32)uc);

      GET_BITS(BZ_X_RANDBIT, s->blockRandomised, 1);

      s->origPtr = 0;
      GET_UCHAR(BZ_X_ORIGPTR_1, uc);
      s->origPtr = (s->origPtr << 8) | ((Int32)uc);
      GET_UCHAR(BZ_X_ORIGPTR_2, uc);
      s->origPtr = (s->origPtr << 8) | ((Int32)uc);
      GET_UCHAR(BZ_X_ORIGPTR_3, uc);
      s->origPtr = (s->origPtr << 8) | ((Int32)uc);

      if (s->origPtr < 0)
         RETURN(BZ_DATA_ERROR);
      if (s->origPtr > 10 + 100000*s->blockSize100k) 
         RETURN(BZ_DATA_ERROR);

      /*--- Receive the mapping table ---*/
      for (i = 0; i < 16; i++) {
         GET_BIT(BZ_X_MAPPING_1, uc);
         if (uc == 1) 
            s->inUse16[i] = True; else 
            s->inUse16[i] = False;
      }

      for (i = 0; i < 256; i++) s->inUse[i] = False;

      for (i = 0; i < 16; i++)
         if (s->inUse16[i])
            for (j = 0; j < 16; j++) {
               GET_BIT(BZ_X_MAPPING_2, uc);
               if (uc == 1) s->inUse[i * 16 + j] = True;
            }
      makeMaps_d ( s );
      if (s->nInUse == 0) RETURN(BZ_DATA_ERROR);
      alphaSize = s->nInUse+2;

      /*--- Now the selectors ---*/
      GET_BITS(BZ_X_SELECTOR_1, nGroups, 3);
      if (nGroups < 2 || nGroups > 6) RETURN(BZ_DATA_ERROR);
      GET_BITS(BZ_X_SELECTOR_2, nSelectors, 15);
      if (nSelectors < 1) RETURN(BZ_DATA_ERROR);
      for (i = 0; i < nSelectors; i++) {
         j = 0;
         while (True) {
            GET_BIT(BZ_X_SELECTOR_3, uc);
            if (uc == 0) break;
            j++;
            if (j >= nGroups) RETURN(BZ_DATA_ERROR);
         }
         s->selectorMtf[i] = j;
      }

      /*--- Undo the MTF values for the selectors. ---*/
      {
         UChar pos[BZ_N_GROUPS], tmp, v;
         for (v = 0; v < nGroups; v++) pos[v] = v;
   
         for (i = 0; i < nSelectors; i++) {
            v = s->selectorMtf[i];
            tmp = pos[v];
            while (v > 0) { pos[v] = pos[v-1]; v--; }
            pos[0] = tmp;
            s->selector[i] = tmp;
         }
      }

      /*--- Now the coding tables ---*/
      for (t = 0; t < nGroups; t++) {
         GET_BITS(BZ_X_CODING_1, curr, 5);
         for (i = 0; i < alphaSize; i++) {
            while (True) {
               if (curr < 1 || curr > 20) RETURN(BZ_DATA_ERROR);
               GET_BIT(BZ_X_CODING_2, uc);
               if (uc == 0) break;
               GET_BIT(BZ_X_CODING_3, uc);
               if (uc == 0) curr++; else curr--;
            }
            s->len[t][i] = curr;
         }
      }

      /*--- Create the Huffman decoding tables ---*/
      for (t = 0; t < nGroups; t++) {
         minLen = 32;
         maxLen = 0;
         for (i = 0; i < alphaSize; i++) {
            if (s->len[t][i] > maxLen) maxLen = s->len[t][i];
            if (s->len[t][i] < minLen) minLen = s->len[t][i];
         }
         BZ2_hbCreateDecodeTables ( 
            &(s->limit[t][0]), 
            &(s->base[t][0]), 
            &(s->perm[t][0]), 
            &(s->len[t][0]),
            minLen, maxLen, alphaSize
         );
         s->minLens[t] = minLen;
      }

      /*--- Now the MTF values ---*/

      EOB      = s->nInUse+1;
      nblockMAX = 100000 * s->blockSize100k;
      groupNo  = -1;
      groupPos = 0;

      for (i = 0; i <= 255; i++) s->unzftab[i] = 0;

      /*-- MTF init --*/
      {
         Int32 ii, jj, kk;
         kk = MTFA_SIZE-1;
         for (ii = 256 / MTFL_SIZE - 1; ii >= 0; ii--) {
            for (jj = MTFL_SIZE-1; jj >= 0; jj--) {
               s->mtfa[kk] = (UChar)(ii * MTFL_SIZE + jj);
               kk--;
            }
            s->mtfbase[ii] = kk + 1;
         }
      }
      /*-- end MTF init --*/

      nblock = 0;
      GET_MTF_VAL(BZ_X_MTF_1, BZ_X_MTF_2, nextSym);

      while (True) {

         if (nextSym == EOB) break;

         if (nextSym == BZ_RUNA || nextSym == BZ_RUNB) {

            es = -1;
            N = 1;
            do {
               if (nextSym == BZ_RUNA) es = es + (0+1) * N; else
               if (nextSym == BZ_RUNB) es = es + (1+1) * N;
               N = N * 2;
               GET_MTF_VAL(BZ_X_MTF_3, BZ_X_MTF_4, nextSym);
            }
               while (nextSym == BZ_RUNA || nextSym == BZ_RUNB);

            es++;
            uc = s->seqToUnseq[ s->mtfa[s->mtfbase[0]] ];
            s->unzftab[uc] += es;

            if (s->smallDecompress)
               while (es > 0) {
                  if (nblock >= nblockMAX) RETURN(BZ_DATA_ERROR);
                  s->ll16[nblock] = (UInt16)uc;
                  nblock++;
                  es--;
               }
            else
               while (es > 0) {
                  if (nblock >= nblockMAX) RETURN(BZ_DATA_ERROR);
                  s->tt[nblock] = (UInt32)uc;
                  nblock++;
                  es--;
               };

            continue;

         } else {

            if (nblock >= nblockMAX) RETURN(BZ_DATA_ERROR);

            /*-- uc = MTF ( nextSym-1 ) --*/
            {
               Int32 ii, jj, kk, pp, lno, off;
               UInt32 nn;
               nn = (UInt32)(nextSym - 1);

               if (nn < MTFL_SIZE) {
                  /* avoid general-case expense */
                  pp = s->mtfbase[0];
                  uc = s->mtfa[pp+nn];
                  while (nn > 3) {
                     Int32 z = pp+nn;
                     s->mtfa[(z)  ] = s->mtfa[(z)-1];
                     s->mtfa[(z)-1] = s->mtfa[(z)-2];
                     s->mtfa[(z)-2] = s->mtfa[(z)-3];
                     s->mtfa[(z)-3] = s->mtfa[(z)-4];
                     nn -= 4;
                  }
                  while (nn > 0) { 
                     s->mtfa[(pp+nn)] = s->mtfa[(pp+nn)-1]; nn--; 
                  };
                  s->mtfa[pp] = uc;
               } else {
                  /* general case */
                  lno = nn / MTFL_SIZE;
                  off = nn % MTFL_SIZE;
                  pp = s->mtfbase[lno] + off;
                  uc = s->mtfa[pp];
                  while (pp > s->mtfbase[lno]) { 
                     s->mtfa[pp] = s->mtfa[pp-1]; pp--; 
                  };
                  s->mtfbase[lno]++;
                  while (lno > 0) {
                     s->mtfbase[lno]--;
                     s->mtfa[s->mtfbase[lno]] 
                        = s->mtfa[s->mtfbase[lno-1] + MTFL_SIZE - 1];
                     lno--;
                  }
                  s->mtfbase[0]--;
                  s->mtfa[s->mtfbase[0]] = uc;
                  if (s->mtfbase[0] == 0) {
                     kk = MTFA_SIZE-1;
                     for (ii = 256 / MTFL_SIZE-1; ii >= 0; ii--) {
                        for (jj = MTFL_SIZE-1; jj >= 0; jj--) {
                           s->mtfa[kk] = s->mtfa[s->mtfbase[ii] + jj];
                           kk--;
                        }
                        s->mtfbase[ii] = kk + 1;
                     }
                  }
               }
            }
            /*-- end uc = MTF ( nextSym-1 ) --*/

            s->unzftab[s->seqToUnseq[uc]]++;
            if (s->smallDecompress)
               s->ll16[nblock] = (UInt16)(s->seqToUnseq[uc]); else
               s->tt[nblock]   = (UInt32)(s->seqToUnseq[uc]);
            nblock++;

            GET_MTF_VAL(BZ_X_MTF_5, BZ_X_MTF_6, nextSym);
            continue;
         }
      }

      /* Now we know what nblock is, we can do a better sanity
         check on s->origPtr.
      */
      if (s->origPtr < 0 || s->origPtr >= nblock)
         RETURN(BZ_DATA_ERROR);

      /*-- Set up cftab to facilitate generation of T^(-1) --*/
      s->cftab[0] = 0;
      for (i = 1; i <= 256; i++) s->cftab[i] = s->unzftab[i-1];
      for (i = 1; i <= 256; i++) s->cftab[i] += s->cftab[i-1];
      for (i = 0; i <= 256; i++) {
         if (s->cftab[i] < 0 || s->cftab[i] > nblock) {
            /* s->cftab[i] can legitimately be == nblock */
            RETURN(BZ_DATA_ERROR);
         }
      }

      s->state_out_len = 0;
      s->state_out_ch  = 0;
      BZ_INITIALISE_CRC ( s->calculatedBlockCRC );
      s->state = BZ_X_OUTPUT;
      if (s->verbosity >= 2) VPrintf0 ( "rt+rld" );

      if (s->smallDecompress) {

         /*-- Make a copy of cftab, used in generation of T --*/
         for (i = 0; i <= 256; i++) s->cftabCopy[i] = s->cftab[i];

         /*-- compute the T vector --*/
         for (i = 0; i < nblock; i++) {
            uc = (UChar)(s->ll16[i]);
            SET_LL(i, s->cftabCopy[uc]);
            s->cftabCopy[uc]++;
         }

         /*-- Compute T^(-1) by pointer reversal on T --*/
         i = s->origPtr;
         j = GET_LL(i);
         do {
            Int32 tmp = GET_LL(j);
            SET_LL(j, i);
            i = j;
            j = tmp;
         }
            while (i != s->origPtr);

         s->tPos = s->origPtr;
         s->nblock_used = 0;
         if (s->blockRandomised) {
            BZ_RAND_INIT_MASK;
            BZ_GET_SMALL(s->k0); s->nblock_used++;
            BZ_RAND_UPD_MASK; s->k0 ^= BZ_RAND_MASK; 
         } else {
            BZ_GET_SMALL(s->k0); s->nblock_used++;
         }

      } else {

         /*-- compute the T^(-1) vector --*/
         for (i = 0; i < nblock; i++) {
            uc = (UChar)(s->tt[i] & 0xff);
            s->tt[s->cftab[uc]] |= (i << 8);
            s->cftab[uc]++;
         }

         s->tPos = s->tt[s->origPtr] >> 8;
         s->nblock_used = 0;
         if (s->blockRandomised) {
            BZ_RAND_INIT_MASK;
            BZ_GET_FAST(s->k0); s->nblock_used++;
            BZ_RAND_UPD_MASK; s->k0 ^= BZ_RAND_MASK; 
         } else {
            BZ_GET_FAST(s->k0); s->nblock_used++;
         }

      }

      RETURN(BZ_OK);



    endhdr_2:

      GET_UCHAR(BZ_X_ENDHDR_2, uc);
      if (uc != 0x72) RETURN(BZ_DATA_ERROR);
      GET_UCHAR(BZ_X_ENDHDR_3, uc);
      if (uc != 0x45) RETURN(BZ_DATA_ERROR);
      GET_UCHAR(BZ_X_ENDHDR_4, uc);
      if (uc != 0x38) RETURN(BZ_DATA_ERROR);
      GET_UCHAR(BZ_X_ENDHDR_5, uc);
      if (uc != 0x50) RETURN(BZ_DATA_ERROR);
      GET_UCHAR(BZ_X_ENDHDR_6, uc);
      if (uc != 0x90) RETURN(BZ_DATA_ERROR);

      s->storedCombinedCRC = 0;
      GET_UCHAR(BZ_X_CCRC_1, uc);
      s->storedCombinedCRC = (s->storedCombinedCRC << 8) | ((UInt32)uc);
      GET_UCHAR(BZ_X_CCRC_2, uc);
      s->storedCombinedCRC = (s->storedCombinedCRC << 8) | ((UInt32)uc);
      GET_UCHAR(BZ_X_CCRC_3, uc);
      s->storedCombinedCRC = (s->storedCombinedCRC << 8) | ((UInt32)uc);
      GET_UCHAR(BZ_X_CCRC_4, uc);
      s->storedCombinedCRC = (s->storedCombinedCRC << 8) | ((UInt32)uc);

      s->state = BZ_X_IDLE;
      RETURN(BZ_STREAM_END);

      default: AssertH ( False, 4001 );
   }

   AssertH ( False, 4002 );

   save_state_and_return:

   s->save_i           = i;
   s->save_j           = j;
   s->save_t           = t;
   s->save_alphaSize   = alphaSize;
   s->save_nGroups     = nGroups;
   s->save_nSelectors  = nSelectors;
   s->save_EOB         = EOB;
   s->save_groupNo     = groupNo;
   s->save_groupPos    = groupPos;
   s->save_nextSym     = nextSym;
   s->save_nblockMAX   = nblockMAX;
   s->save_nblock      = nblock;
   s->save_es          = es;
   s->save_N           = N;
   s->save_curr        = curr;
   s->save_zt          = zt;
   s->save_zn          = zn;
   s->save_zvec        = zvec;
   s->save_zj          = zj;
   s->save_gSel        = gSel;
   s->save_gMinlen     = gMinlen;
   s->save_gLimit      = gLimit;
   s->save_gBase       = gBase;
   s->save_gPerm       = gPerm;

   return retVal;   
}
*)
end.
