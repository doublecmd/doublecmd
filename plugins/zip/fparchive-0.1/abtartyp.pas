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
{* ABBREVIA: AbTarTyp.pas 3.04                           *}
{*********************************************************}
{* ABBREVIA: TAbTarArchive, TAbTarItem classes           *}
{*********************************************************}
{* Misc. constants, types, and routines for working      *}
{* with Tar files                                        *}
{*********************************************************}

{$I AbDefine.inc}

unit AbTarTyp;

interface

uses
{$IFDEF MSWINDOWS }
  Windows,
{$ENDIF MSWINDOWS }
  SysUtils, Classes,
  AbUtils, AbVMStrm, AbSpanSt, AbExcept, AbArcTyp;

const
  AB_TAR_RECORDSIZE  = 512;
  AB_TAR_NAMESIZE    = 100;
  AB_TAR_TUSRNAMELEN = 32;
  AB_TAR_TGRPNAMELEN = 32;

{ The checksum field is filled with this while the checksum is computed. }
  AB_TAR_CHKBLANKS = '        ';        { 8 blanks, no null }

  AB_TAR_EMPTY_OCT = '00000000';

{ The magic field is filled with this if uname and gname are valid. }
  AB_TAR_TMAGIC        = 'ustar  '#0;   { 7 chars and a null }

{ The magic field is filled with this if this is a GNU format dump entry }
  AB_TAR_GNUMAGIC      = 'GNUtar '#0;   { 7 chars and a null }

{ The linkflag defines the type of file }
  AB_TAR_LF_OLDNORMAL = #0;   { Normal disk file, Unix compatible }
  AB_TAR_LF_NORMAL    = '0';  { Normal disk file }
  AB_TAR_LF_LINK      = '1';  { Link to previously dumped file }
  AB_TAR_LF_SYMLINK   = '2';  { Symbolic link }
  AB_TAR_LF_CHR       = '3';  { Character special file }
  AB_TAR_LF_BLK       = '4';  { Block special file }
  AB_TAR_LF_DIR       = '5';  { Directory }
  AB_TAR_LF_FIFO      = '6';  { FIFO special file }
  AB_TAR_LF_CONTIG    = '7';  { Contiguous file }

{ Further link types may be defined later. }

{ Bits used in the mode field - values in octal }
  AB_TAR_TSUID   = $0800;   { Set UID on execution }
  AB_TAR_TSGID   = $0400;   { Set GID on execution }
  AB_TAR_TSVTX   = $0200;   { Save text (sticky bit) }

{ File permissions }
  AB_TAR_TUREAD  = $0100;   { read by owner }
  AB_TAR_TUWRITE = $0080;   { write by owner }
  AB_TAR_TUEXEC  = $0040;   { execute/search by owner }
  AB_TAR_TGREAD  = $0020;   { read by group }
  AB_TAR_TGWRITE = $0010;   { write by group }
  AB_TAR_TGEXEC  = $0008;   { execute/search by group }
  AB_TAR_TOREAD  = $0004;   { read by other }
  AB_TAR_TOWRITE = $0002;   { write by other }
  AB_TAR_TOEXEC  = $0001;   { execute/search by other }

type
  Arr8  = array [0..7] of AnsiChar;
  Arr12 = array [0..11] of AnsiChar;
  ArrName = array [0..AB_TAR_NAMESIZE-1] of AnsiChar;

  TAbTarHeaderRec = packed record
    Name     : ArrName;   { filename, null terminated ASCII string }
    Mode     : Arr8;      { file mode (UNIX style, ASCII coded Octal) }
    uid      : Arr8;      { usrid # (UNIX style, ASCII coded Octal) }
    gid      : Arr8;      { grpid # (UNIX style, ASCII coded Octal) }
    Size     : Arr12;     { size of TARred file (ASCII coded Octal) }
    ModTime  : Arr12;     { last file modification (UNIX Date in ASCII coded Octal) }
    ChkSum   : Arr8;      { checksum of header (ASCII coded Octal) }
    LinkFlag : AnsiChar;  { type of item, one of the Link Flag constants from above }
    LinkName : ArrName;   { name of link, null terminated ASCII string }
    Magic    : Arr8;      { identifier, usuall 'ustar' }
    UsrName  : array [0..AB_TAR_TUSRNAMELEN-1] of AnsiChar;
                          { username, null terminated ASCII string }
    GrpName  : array [0..AB_TAR_TGRPNAMELEN-1] of AnsiChar;
                          { groupname, null terminated ASCII string }
    DevMajor : Arr8;      { major device ID (UNIX style, ASCII coded Octal) }
    DevMinor : Arr8;      { minor device ID (UNIX style, ASCII coded Octal) }
  end;


type
  TAbTarItem = class(TAbArchiveItem)
  private
    function GetMagic: string;
  protected {private}
    FTarHeader: TAbTarHeaderRec;
  protected
    function GetDevMajor: Integer;
    function GetDevMinor: Integer;
    function GetGroupID: Integer;
    function GetGroupName: string;
    function GetLinkName: string;
    function GetUserID: Integer;
    function GetUserName: string;

    procedure SetDevMajor(const Value: Integer);
    procedure SetDevMinor(const Value: Integer);
    procedure SetGroupID(const Value: Integer);
    procedure SetGroupName(const Value: string);
    procedure SetLinkName(const Value: string);
    procedure SetUserID(const Value: Integer);
    procedure SetUserName(const Value: string);

    function GetCompressedSize : LongInt; override;
    function GetExternalFileAttributes : LongInt; override;
    function GetFileName : string; override;
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

    procedure SaveTarHeaderToStream(AStream : TStream);
    procedure LoadTarHeaderFromStream(AStream :TStream);

    property Magic : string
      read GetMagic;
  public
    property DevMajor : Integer
      read GetDevMajor write SetDevMajor;
    property DevMinor : Integer
      read GetDevMinor write SetDevMinor;
    property GroupID : Integer
      read GetGroupID write SetGroupID;
    property GroupName : string
      read GetGroupName write SetGroupName;
    property LinkFlag : AnsiChar
      read FTarHeader.LinkFlag write FTarHeader.LinkFlag;
    property LinkName : string
      read GetLinkName write SetLinkName;
    property Mode : LongInt
      read GetExternalFileAttributes write SetExternalFileAttributes;
    property UserID : Integer
      read GetUserID write SetUserID;
    property UserName : string
      read GetUserName write SetUserName;
    property ExternalFileAttributes;
    constructor Create;
  end;


  TAbTarStreamHelper = class(TAbArchiveStreamHelper)
  private
    function SeekItemData(Index: Integer): Boolean;
  protected
    FTarHeader : TAbTarHeaderRec;
  public
    destructor Destroy; override;
    procedure ExtractItemData(AStream : TStream); override;
    function FindFirstItem : Boolean; override;
    function FindNextItem : Boolean; override;
    procedure ReadHeader; override;
    procedure ReadTail; override;
    function SeekItem(Index : Integer): Boolean; override;
    procedure WriteArchiveHeader; override;
    procedure WriteArchiveItem(AStream : TStream); override;
    procedure WriteArchiveTail; override;
    function GetItemCount : Integer; override;
  end;


  TAbTarArchive = class(TAbArchive)
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
    function FixName(Value: string): string;
      override;

    function GetItem(Index: Integer): TAbTarItem;                   {!!.03}
    procedure PutItem(Index: Integer; const Value: TAbTarItem);     {!!.03}
  public {methods}
    constructor Create(FileName : string; Mode : Word);
      override;
    destructor  Destroy;
      override;

    property Items[Index : Integer] : TAbTarItem                    {!!.03}
      read GetItem                                                  {!!.03}
      write PutItem; default;                                       {!!.03}


 end;

function VerifyTar(Strm : TStream) : TAbArchiveType;

implementation

function OctalToInt(const Oct : PAnsiChar; aLen : integer): Integer;
var
  i : integer;
begin
  Result := 0;

  i := 0;
  while (i < aLen) and (Oct[i] = ' ') do
    inc(i);

  if (i = aLen) then
    Exit;

  while (i < aLen) and (Oct[i] in ['0'..'7']) do begin
    Result := (Result * 8) + (Ord(Oct[i]) - Ord('0'));
    inc(i);
  end;

end;

function IntToOctal(Value : Integer): string;
const
  OctDigits  : array[0..7] of AnsiChar = '01234567';
begin
  if Value = 0 then
    Result := '0'
  else begin
    Result := '';
    while Value > 0 do begin
      Result := OctDigits[Value and 7] + Result;
      Value := Value shr 3;
    end;
  end;
end;

function CalcTarHeaderChkSum(TarH : TAbTarHeaderRec): LongInt;
var
  HdrBuffer : PAnsiChar;
  HdrChkSum : LongInt;
  j : Integer;
begin
  { prepare for the checksum calculation }
  HdrBuffer := PAnsiChar(@TarH);                                         {!!.02}
  HdrChkSum := 0;

  {calculate the checksum, a simple sum of the bytes in the header}
  for j := 0 to Pred(SizeOf(TAbTarHeaderRec)) do
    HdrChkSum := HdrChkSum + Ord(HdrBuffer[j]);

  Result := HdrChkSum;
end;

function VerifyHeader(TarH : TAbTarHeaderRec): Boolean;
{ check "Magic" field in Tar Header}
begin
  Result := (TarH.Magic = StrPas(AB_TAR_TMAGIC)) or
     (TarH.Magic = StrPas(AB_TAR_GNUMAGIC));
end;

function VerifyTar(Strm : TStream) : TAbArchiveType;
{ assumes Tar positioned correctly for test of item }
var
  TarHlp : TAbTarStreamHelper;
  TarItem : TAbTarItem;
  TarChkSum : LongInt;
  TarHead : TAbTarHeaderRec;
begin
  { really only verifies that the data read from current stream position
    appears to be tarred data }
  TarHlp := TAbTarStreamHelper.Create(Strm);
  TarItem := TAbTarItem.Create;

  try
    { get current Tar Header }
    TarItem.LoadTarHeaderFromStream(Strm);
    TarHead := TarItem.FTarHeader;

    { verify check sum }
    TarChkSum := OctalToInt(TarItem.FTarHeader.ChkSum, sizeof(TarItem.FTarHeader.ChkSum));
    TarHead.ChkSum := AB_TAR_CHKBLANKS;
    if (CalcTarHeaderChkSum(TarHead) = TarChkSum) or
       VerifyHeader(TarItem.FTarHeader)
    then
      Result := atTar
    else
      result := atUnknown;
  finally
    TarItem.Free;
    TarHlp.Free;
  end;
end;

function PadString(const S : string; Places : Integer) : string;
{
Pads a string (S) with one right space and as many left spaces as
needed to fill Places

If length S greater than Places, just returns S

Some TAR utilities evidently expect Octal numeric fields to be in
this format
}
begin
  if Length(S) >= LongInt(Places) then
    Result := S
  else begin
    Result := S + ' ';
    Result := StringOfChar(' ', Places - Length(Result)) + Result;
  end;
end;


function RoundToTarBlock(Size: Integer): Integer;
begin
  Result := (Size + (AB_TAR_RECORDSIZE - 1)) and
             not (AB_TAR_RECORDSIZE - 1);
end;


function FindFirstTarItem(TarF: TStream; var Item : TAbTarHeaderRec): Integer;
begin
  { reset Tar }
  TarF.Seek(0, soFromBeginning);
  Result := TarF.Read(Item, SizeOf(TAbTarHeaderRec));
end;

function FindNextTarItem(TarF: TStream; var Item : TAbTarHeaderRec): Integer;
var
  Len: Integer;
begin
  { find length of item }
  Len := RoundToTarBlock(OctalToInt(Item.Size, 12));
  { seek past file to next header }
  TarF.Seek((AB_TAR_RECORDSIZE-SizeOf(TAbTarHeaderRec)) + Len, soFromCurrent);
  Result := TarF.Read(Item, SizeOf(TAbTarHeaderRec));
end;


procedure ListTarContents(TarF: TStream; List: TStrings);
{
Get listing of files in Tar
}
var
  TarH : TAbTarHeaderRec;
  FN: string;
begin
  { reset Tar }
  FindFirstTarItem(TarF, TarH);

  { while more data in Tar }
  while TarF.Position < TarF.Size do begin
    { if it's a file }
      if TarH.LinkFlag in [AB_TAR_LF_OLDNORMAL, AB_TAR_LF_NORMAL] then begin
      { add filename to List }
        FN := StrPas(TarH.Name);
        if FN <> '' then
          List.Add(FN);
    end; {if}

    FindNextTarItem(TarF, TarH);
  end; {while}
end;



{ TAbTarItem }

constructor TAbTarItem.Create;
begin
  inherited Create;
  FillChar(FTarHeader, SizeOf(TAbTarHeaderRec), #0);
  { set defaults }
  FTarHeader.Magic := AB_TAR_TMAGIC;
  FileName := '';
  Mode := AB_FPERMISSION_GENERIC;
  UserID := 0;
  GroupID := 0;
  LinkFlag := AB_TAR_LF_NORMAL;
  UserName := '';
  GroupName := '';
  DevMajor := 0;
  DevMinor := 0;
end;


function TAbTarItem.GetCompressedSize: LongInt;
{ TAR includes no internal compression, returns same value as GetUncompressedSize }
begin
  Result := OctalToInt(FTarHeader.Size, SizeOf(FTarHeader.Size));
end;

function TAbTarItem.GetDevMajor: Integer;
begin
  Result := OctalToInt(FTarHeader.DevMajor, SizeOf(FTarHeader.DevMajor));
end;

function TAbTarItem.GetDevMinor: Integer;
begin
  Result := OctalToInt(FTarHeader.DevMinor, SizeOf(FTarHeader.DevMinor));
end;

function TAbTarItem.GetExternalFileAttributes: LongInt;
begin
  Result := OctalToInt(FTarHeader.Mode, SizeOf(FTarHeader.Mode));
end;

function TAbTarItem.GetFileName: string;
begin
  Result := FTarHeader.Name;
end;

function TAbTarItem.GetGroupID: Integer;
begin
  Result := OctalToInt(FTarHeader.gid, sizeof(FTarHeader.gid));
end;

function TAbTarItem.GetGroupName: string;
begin
  Result := FTarHeader.GrpName;
end;

function TAbTarItem.GetIsEncrypted: Boolean;
begin
  { TAR has no native encryption }
  Result := False;
end;

function TAbTarItem.GetLastModFileDate: Word;
var
  UnixDate : Integer;
  D : TDateTime;
begin
  { convert octal string date to unix style integer date }
  UnixDate := OctalToInt(FTarHeader.ModTime, sizeof(FTarHeader.ModTime));

  { convert to TDateTime }
  D := AbUnixTimeToDateTime(UnixDate);

  { convert to DOS file Date }
  UnixDate:=DateTimeToFileDate(D);
  Result := LongRec(UnixDate).Hi;
end;

function TAbTarItem.GetLastModFileTime: Word;
var
  UnixDate : Integer;
  D : TDateTime;
begin
  { convert octal string to unix style integer date }
  UnixDate := OctalToInt(FTarHeader.ModTime, sizeof(FTarHeader.ModTime));

  { convert to TDateTime }
  D := AbUnixTimeToDateTime(UnixDate);

  { convert to DOS file Time }
  UnixDate:=DateTimeToFileDate(D);
  Result := LongRec(UnixDate).Lo;
end;

function TAbTarItem.GetLinkName: string;
begin
  Result := FTarHeader.LinkName;
end;


function TAbTarItem.GetMagic: string;
begin
  Result := FTarHeader.Magic;
end;

function TAbTarItem.GetUncompressedSize: LongInt;
{ TAR includes no internal compression, returns same value as GetCompressedSize }
begin
  Result := OctalToInt(FTarHeader.Size, sizeof(FTarHeader.Size));
end;

function TAbTarItem.GetUserID: Integer;
begin
  Result := OctalToInt(FTarHeader.uid, sizeof(FTarHeader.uid));
end;

function TAbTarItem.GetUserName: string;
begin
  Result := FTarHeader.UsrName;
end;

procedure TAbTarItem.LoadTarHeaderFromStream(AStream: TStream);
begin
  AStream.Read(FTarHeader, SizeOf(TAbTarHeaderRec));
  AStream.Seek(-SizeOf(TAbTarHeaderRec), soFromCurrent);
  FFileName := FTarHeader.Name;
  DiskFileName := FileName;
  AbUnfixName(FDiskFileName);
  Action := aaNone;
  Tagged := False;
end;

procedure TAbTarItem.SaveTarHeaderToStream(AStream: TStream);
var
  j : Integer;
  HdrChkSum : Integer;
  HdrChkStr : string;
  HdrBuffer : PAnsiChar;
  PadSize : Integer;
  PadBuff : array [0..AB_TAR_RECORDSIZE - 1] of byte;
begin
  { TAR says ChkSum field itself is blanked for calc purposes }
  FTarHeader.ChkSum := AB_TAR_CHKBLANKS;

  { prepare for the checksum calculation }
  HdrBuffer := PAnsiChar(@FTarHeader);                                   {!!.02}
  HdrChkSum := 0;

  {calculate the checksum, a simple sum of the bytes in the header}
  for j := 0 to Pred(SizeOf(TAbTarHeaderRec)) do
    HdrChkSum := HdrChkSum + Ord(HdrBuffer[j]);

  {set the checksum in the header}
  HdrChkStr := PadString(IntToOctal(HdrChkSum), SizeOf(Arr8));
  Move(HdrChkStr[1], FTarHeader.ChkSum, Length(HdrChkStr));

  { write header data }
  AStream.Write(FTarHeader, SizeOf(TAbTarHeaderRec));

  { Pad to Next block with zero bytes }
  PadSize := AB_TAR_RECORDSIZE - SizeOf(TAbTarHeaderRec);
  FillChar(PadBuff, PadSize, 0);
  AStream.Write(PadBuff, PadSize);
end;

procedure TAbTarItem.SetCompressedSize(const Value: Integer);
var
  S : string;
begin
  S := PadString(IntToOctal(Value), SizeOf(Arr12));
  Move(S[1], FTarHeader.Size, Length(S));
end;

procedure TAbTarItem.SetDevMajor(const Value: Integer);
var
  S : string;
begin
  S := PadString(IntToOctal(Value), SizeOf(Arr8));
  Move(S[1], FTarHeader.DevMajor, Length(S));
end;

procedure TAbTarItem.SetDevMinor(const Value: Integer);
var
  S : string;
begin
  S := PadString(IntToOctal(Value), SizeOf(Arr8));
  Move(S[1], FTarHeader.DevMinor, Length(S));
end;

procedure TAbTarItem.SetExternalFileAttributes(Value: Integer);
var
  S : string;
begin
  S := PadString(IntToOctal(Value), SizeOf(Arr8));
  Move(S[1], FTarHeader.Mode, Length(S));
end;

procedure TAbTarItem.SetFileName(Value: string);
begin
  StrPCopy(FTarHeader.Name, Value);
end;

procedure TAbTarItem.SetGroupID(const Value: Integer);
var
  S : string;
begin
  S := PadString(IntToOctal(Value), SizeOf(Arr8));
  Move(S[1], FTarHeader.gid, Length(S));
end;

procedure TAbTarItem.SetGroupName(const Value: string);
begin
  StrPCopy(FTarHeader.GrpName, Value);
end;

procedure TAbTarItem.SetIsEncrypted(Value: Boolean);
begin
  { do nothing, TAR has no native encryption }
end;

procedure TAbTarItem.SetLastModFileDate(const Value: Word);
var
  D : TDateTime;
  UT : LongInt;
  DStr : string;
begin
  { get current date from header record }
  UT := OctalToInt(FTarHeader.ModTime, sizeof(FTarHeader.ModTime));

  { keep seconds in current day, discard date's seconds }
  UT := UT mod SecondsInDay;

  { build new date }
  D := EncodeDate(Value shr 9 + 1980, Value shr 5 and 15, Value and 31);

  { add to unix second count }
  UT :=  UT + AbDateTimeToUnixTime(D);

  { store octal string }
  DStr := PadString(IntToOctal(UT), SizeOf(Arr12));
  Move(DStr[1], FTarHeader.ModTime, Length(DStr));
end;

procedure TAbTarItem.SetLastModFileTime(const Value: Word);
var
  T : TDateTime;
  UT : LongInt;
  TStr : string;
begin
  { get current date from header record }
  UT := OctalToInt(FTarHeader.ModTime, sizeof(FTarHeader.ModTime));

  { keep seconds in current date, discard day's seconds }
  UT := UT - (UT mod SecondsInDay);

  { build new time }
  T := EncodeTime(Value shr 11, Value shr 5 and 63, Value and 31 shl 1, 0);

  { add to unix second count }
  UT := UT + AbDateTimeToUnixTime(T);

  { store octal string }
  TStr := PadString(IntToOctal(UT), SizeOf(Arr12));
  Move(TStr[1], FTarHeader.ModTime, Length(TStr));
end;

procedure TAbTarItem.SetLinkName(const Value: string);
begin
  StrPCopy(FTarHeader.LinkName, Value);
end;

procedure TAbTarItem.SetUncompressedSize(const Value: Integer);
var
  S : string;
begin
  S := PadString(IntToOctal(Value), SizeOf(Arr12));
  Move(S[1], FTarHeader.Size, Length(S));
end;

procedure TAbTarItem.SetUserID(const Value: Integer);
var
  S : string;
begin
  S := PadString(IntToOctal(Value), SizeOf(Arr8));
  Move(S[1], FTarHeader.uid, Length(S));
end;

procedure TAbTarItem.SetUserName(const Value: string);
begin
  StrPCopy(FTarHeader.UsrName, Value);
end;

{ TAbTarStreamHelper }

destructor TAbTarStreamHelper.Destroy;
begin
  inherited Destroy;
end;

procedure TAbTarStreamHelper.ExtractItemData(AStream: TStream);
var
  Len : Integer;
begin
  {assumption: the internal stream is positioned at the start of the
               header for the current item}

  { get size of stored data }
  Len := OctalToInt(FTarHeader.Size, sizeof(FTarHeader.Size));

  { locate start of stored data }
  FStream.Seek(SizeOf(TAbTarHeaderRec), soFromCurrent);

  { copy stored data to output }
  AStream.CopyFrom(FStream, Len);

  {reset the stream to the start of the item}
  FStream.Seek(-(SizeOf(TAbTarHeaderRec) + Len), soFromCurrent);
end;


function TAbTarStreamHelper.FindFirstItem: Boolean;
var
  DataRead : LongInt;
  Len: Integer;
begin

  {the first item is found at the start of the stream}
  FStream.Seek(0, soFromBeginning);
  DataRead := FStream.Read(FTarHeader, SizeOf(TAbTarHeaderRec));

  { keep looking til find ordinary file }
  while (DataRead = SizeOf(TAbTarHeaderRec)) and
    not (FTarHeader.LinkFlag in [AB_TAR_LF_OLDNORMAL, AB_TAR_LF_NORMAL]) do begin
    FStream.Seek(-SizeOf(TAbTarHeaderRec), soFromCurrent);

    { advance to next record }
    { find length of current item, rounded up to the TAR block size }
    Len := RoundToTarBlock(OctalToInt(FTarHeader.Size, sizeof(FTarHeader.Size)));

    { seek past file to next header }
    FStream.Seek(AB_TAR_RECORDSIZE + Len, soFromCurrent);
    DataRead := FStream.Read(FTarHeader, SizeOf(TAbTarHeaderRec));
  end;

  FStream.Seek(-SizeOf(TAbTarHeaderRec), soFromCurrent);
  Result := (DataRead = SizeOf(TAbTarHeaderRec)) {and VerifyHeader(FTarHeader)};
end;

function TAbTarStreamHelper.FindNextItem: Boolean;
var
  DataRead : LongInt;
  Len: Integer;
begin
  { find length of current item, rounded up to the TAR block size }
  Len := RoundToTarBlock(OctalToInt(FTarHeader.Size, sizeof(FTarHeader.Size)));

  { seek past file to next header }
  FStream.Seek(AB_TAR_RECORDSIZE + Len, soFromCurrent);
  DataRead := FStream.Read(FTarHeader, SizeOf(TAbTarHeaderRec));

  while (DataRead = SizeOf(TAbTarHeaderRec)) and (StrLen(FTarHeader.Name) > 0) and
    not (FTarHeader.LinkFlag in [AB_TAR_LF_OLDNORMAL, AB_TAR_LF_NORMAL]) do begin
    FStream.Seek(-SizeOf(TAbTarHeaderRec), soFromCurrent);

    { advance to next record }
    { find length of current item, rounded up to the TAR block size }
    Len := RoundToTarBlock(OctalToInt(FTarHeader.Size, sizeof(FTarHeader.Size)));

    { seek past file to next header }
    FStream.Seek(AB_TAR_RECORDSIZE + Len, soFromCurrent);
    DataRead := FStream.Read(FTarHeader, SizeOf(TAbTarHeaderRec));
  end;

  { reset to start of header }
  if (DataRead = SizeOf(TAbTarHeaderRec)) and (StrLen(FTarHeader.Name) > 0) and
    (FTarHeader.LinkFlag in [AB_TAR_LF_OLDNORMAL, AB_TAR_LF_NORMAL]) then
  begin
    FStream.Seek(-SizeOf(TAbTarHeaderRec), soFromCurrent);
    Result := True;
  end else
    Result := False;
end;


function TAbTarStreamHelper.GetItemCount : Integer;
var
  Found : Boolean;
begin
  Result := 0;
  Found := FindFirstItem;
  while Found do begin
    Inc(Result);
    Found := FindNextItem;
  end;
end;


procedure TAbTarStreamHelper.ReadHeader;
begin
  { do nothing }
  { Tar archives have no overall header data }
end;

procedure TAbTarStreamHelper.ReadTail;
begin
  { do nothing }
  { Tar archives have no overall tail data }
end;

function TAbTarStreamHelper.SeekItem(Index: Integer): Boolean;
var
  i : Integer;
begin
  Result := FindFirstItem; { see if can get to first item }
  i := 1;
  while Result and (i < Index) do begin
    Result := FindNextItem;
    Inc(i);
  end;
end;

function TAbTarStreamHelper.SeekItemData(Index: Integer): Boolean;
var
  i : Integer;
begin
  Result := FindFirstItem; { see if can get to first item }
  i := 0;
  while Result and (i < Index) do begin
    Result := FindNextItem;
    Inc(i);
  end;

  { locate start of stored data }
  FStream.Seek(AB_TAR_RECORDSIZE - SizeOf(TAbTarHeaderRec), soFromCurrent);
end;

procedure TAbTarStreamHelper.WriteArchiveHeader;
begin
  { do nothing }
  { Tar archives have no overall header data }
end;

procedure TAbTarStreamHelper.WriteArchiveItem(AStream: TStream);
var
  PadBuff : PAnsiChar;
  PadSize : Integer;
begin
  { transfer actual item data }
  FStream.CopyFrom(AStream, AStream.Size);

  { Pad to Next block }
  PadSize := RoundToTarBlock(AStream.Size) - AStream.Size;
  GetMem(PadBuff, PadSize);
  FillChar(PadBuff^, PadSize, #0);
  FStream.Write(PadBuff^, PadSize);
  FreeMem(PadBuff, PadSize);
end;

procedure TAbTarStreamHelper.WriteArchiveTail;
var
  PadBuff : PAnsiChar;
  PadSize : Integer;
begin
  { append terminating null block }
  PadSize := AB_TAR_RECORDSIZE;
  GetMem(PadBuff, PadSize );
  FillChar(PadBuff^, PadSize, #0);
  FStream.Write(PadBuff^, PadSize);
  FreeMem(PadBuff, PadSize);
end;

{ TAbTarArchive }

constructor TAbTarArchive.Create(FileName: string; Mode: Word);
begin
  inherited Create(FileName, Mode);
end;

destructor TAbTarArchive.Destroy;
begin
  inherited Destroy;
end;


function TAbTarArchive.CreateItem(const FileSpec: string): TAbArchiveItem;
var
  Buff : array [0..255] of AnsiChar;
  Item : TAbTarItem;
begin
  Item := TAbTarItem.Create;
  try
    Item.CompressedSize := 0;
    Item.CRC32 := 0;
    StrPCopy(Buff, ExpandFileName(FileSpec));
    {$IFDEF MSWINDOWS }
    AnsiToOEM(Buff, Buff);
    {$ENDIF MSWINDOWS }
    Item.DiskFileName := Buff;
    StrPCopy(Buff, FixName(FileSpec));
    {$IFDEF MSWINDOWS }
    AnsiToOEM(Buff, Buff);
    {$ENDIF MSWINDOWS }
    Item.FileName := Buff;
  finally
    Result := Item;
  end;
end;


procedure TAbTarArchive.ExtractItemAt(Index: Integer;
  const NewName: string);
var
  OutStream : TFileStream;
  UseName : string;
  CurItem : TAbTarItem;
{$IFDEF LINUX}                                                           {!!.01}
  FileDateTime  : TDateTime;                                             {!!.01}
  LinuxFileTime : LongInt;                                               {!!.01}
{$ENDIF LINUX}                                                           {!!.01}
begin
  UseName := NewName;
  CurItem := TAbTarItem(ItemList[Index]);

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
        AbFileSetAttr(UseName, AbUnix2DosFileAttributes(CurItem.ExternalFileAttributes));
        {$ENDIF}
        {$IFDEF LINUX}
        FileDateTime := AbDosFileDateToDateTime(CurItem.LastModFileDate, {!!.01}
          CurItem.LastModFileTime);                                      {!!.01}
        LinuxFileTime := AbDateTimeToUnixTime(FileDateTime);             {!!.01}
//!! MVC not yet implemented        FileSetDate(UseName, LinuxFileTime);                             {!!.01}
        AbFileSetAttr(UseName, CurItem.ExternalFileAttributes);          {!!.01}
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

procedure TAbTarArchive.ExtractItemToStreamAt(Index: Integer;
  aStream: TStream);
var
  TarHelp : TAbTarStreamHelper;
  Found : Boolean;
begin
  { create helper }
  TarHelp := TAbTarStreamHelper.Create(FStream);

  Found := TarHelp.SeekItemData(Index);

  try {TarHelp}

    if Found {(idx = Index)} then begin
      TarHelp.ExtractItemData(aStream);
    end
    else begin
      raise Exception.Create('Index out of range');
    end;
  finally {TarHelp}
    { Clean Up }
    TarHelp.Free;
  end; {TarHelp}
end;

procedure TAbTarArchive.LoadArchive;
var
  TarHelp      : TAbTarStreamHelper;
  Item         : TAbTarItem;
  ItemFound    : Boolean;
  Abort        : Boolean;
  Confirm      : Boolean;
  TotalEntries : Integer;
  i            : Integer;
  Progress     : Byte;
begin
  { create helper }
  TarHelp := TAbTarStreamHelper.Create(FStream);
  try {TarHelp}
    TotalEntries := TarHelp.GetItemCount;

    {build Items list from tar header records}
    i := 0;

    { reset Tar }
    ItemFound := TarHelp.FindFirstItem;

    { while more data in Tar }
    while (FStream.Position < FStream.Size) and ItemFound do begin
      Item := TAbTarItem.Create;
      try  {Item}
        Item.LoadTarHeaderFromStream(FStream);
        {if it's a file}
        if Item.LinkFlag in [AB_TAR_LF_OLDNORMAL, AB_TAR_LF_NORMAL] then begin
        {create new Item}
          Item.Action := aaNone;
          FItemList.Add(Item);
          Inc(i);
        end { end if }
        else begin
        { unhandled Tar file system entity, notify user, but otherwise ignore }
          if Assigned(FOnConfirmProcessItem) then
            FOnConfirmProcessItem(self, Item, ptFoundUnhandled, Confirm);
        end;

        { show progress and allow for aborting }
        Progress := (i * 100) div TotalEntries;
        DoArchiveProgress(Progress, Abort);
        if Abort then begin
          FStatus := asInvalid;
          raise EAbUserAbort.Create;
        end;

        { get the next item }
        ItemFound := TarHelp.FindNextItem;
      except {Item}
        raise EAbException.Create('Invalid Item');
      end; {Item}
    end; {end while }

    DoArchiveProgress(100, Abort);
    FIsDirty := False;

  finally {TarHelp}
    { Clean Up }
    TarHelp.Free;
  end; {TarHelp}
end;


function TAbTarArchive.FixName(Value: string): string;
{ fixup filename for storage }
begin
  {$IFDEF MSWINDOWS}
  if DOSMode then begin
    {Add the base directory to the filename before converting }
    {the file spec to the short filespec format. }
    if BaseDirectory <> '' then begin
      {Does the filename contain a drive or a leading backslash? }
      if not ((Pos(':', Value) = 2) or (Pos(AbPathDelim, Value) = 1)) then
        {If not, add the BaseDirectory to the filename.}
        Value := BaseDirectory + AbPathDelim + Value;
    end;
    Value := AbGetShortFileSpec( Value );
  end;
  {$ENDIF MSWINDOWS}

  { Should always trip drive info if on a Win/Dos system }
  StoreOptions := StoreOptions + [soStripDrive];

  { strip drive stuff }
  if soStripDrive in StoreOptions then
    AbStripDrive( Value );

  { check for a leading slash }
  if Value[1] = AbPathDelim then
    System.Delete( Value, 1, 1 );

  if soStripPath in StoreOptions then
    Value := ExtractFileName(Value);

  if soRemoveDots in StoreOptions then
    AbStripDots(Value);

  AbFixName(Value);

  Result := Value;
end;

{!!.03 - Added }
function TAbTarArchive.GetItem(Index: Integer): TAbTarItem;
begin
  Result := TAbTarItem(FItemList.Items[Index]);
end;

procedure TAbTarArchive.PutItem(Index: Integer; const Value: TAbTarItem);
begin
  FItemList.Items[Index] := Value;
end;
{!!.03 - End Added }

procedure TAbTarArchive.SaveArchive;
var
  InTarHelp,
  OutTarHelp         : TAbTarStreamHelper;
  Abort              : Boolean;
  i                  : Integer;
  NewStream          : TAbVirtualMemoryStream;
  WorkingStream      : TAbVirtualMemoryStream;
  UncompressedStream : TStream;
  DateTime           : LongInt;
  SaveDir            : string;
  CurItem            : TAbTarItem;
  {$IFDEF LINUX}
  TmpDT              : TDateTime;
  {$ENDIF}
begin
  InTarHelp := TAbTarStreamHelper.Create(FStream);
  try { InTarHelp }

    {init new archive stream}
    NewStream := TAbVirtualMemoryStream.Create;
    OutTarHelp := TAbTarStreamHelper.Create(NewStream);

    try {NewStream/OutTarHelp}

      { create helper }
      NewStream.SwapFileDirectory := ExtractFilePath(AbGetTempFile(FTempDir, False));

      {build new archive from existing archive}
      for i := 0 to pred(Count) do begin
        FCurrentItem := ItemList[i];
        CurItem      := TAbTarItem(ItemList[i]);

        case CurItem.Action of
          aaNone, aaMove : begin
          {just copy the file to new stream}
            WorkingStream := TAbVirtualMemoryStream.Create;
            try
              InTarHelp.SeekItemData(i);
              InTarHelp.ExtractItemData(WorkingStream);
              WorkingStream.Position := 0;
              CurItem.SaveTarHeaderToStream(NewStream);
              OutTarHelp.WriteArchiveItem(WorkingStream);
            finally
              WorkingStream.Free;
            end;
          end;

          aaDelete: {doing nothing omits file from new stream} ;

          aaAdd, aaFreshen, aaReplace, aaStreamAdd: begin
            try
              if (CurItem.Action = aaStreamAdd) then begin
              { adding from a stream }
                CurItem.SaveTarHeaderToStream(NewStream);
                OutTarHelp.WriteArchiveItem(InStream);
              end
              else begin
              { it's coming from a file }
                GetDir(0, SaveDir);
                try {SaveDir}
                  if (BaseDirectory <> '') then
                    ChDir(BaseDirectory);
                  UncompressedStream := TFileStream.Create(CurItem.DiskFileName,
                    fmOpenRead or fmShareDenyWrite );

                  {Now get the file's attributes}
                  CurItem.ExternalFileAttributes :=
                     AbDOS2UnixFileAttributes(AbFileGetAttr(CurItem.DiskFileName));
                  CurItem.UncompressedSize := UncompressedStream.Size;
                finally {SaveDir}
                  ChDir( SaveDir );
                end; {SaveDir}

                try {UncompressedStream}
                  DateTime := FileAge(CurItem.DiskFileName);
                  {$IFDEF LINUX}                                         {!!.01}
                  TmpDT := AbUnixTimeToDateTime(DateTime);               {!!.01}
                  DateTime := AbDateTimeToDosFileDate(TmpDT);            {!!.01}
                  {$ENDIF}                                               {!!.01}

                  CurItem.LastModFileTime := LongRec(DateTime).Lo;
                  CurItem.LastModFileDate := LongRec(DateTime).Hi;

                  CurItem.SaveTarHeaderToStream(NewStream);
                  OutTarHelp.WriteArchiveItem(UncompressedStream);

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
      end;

      {copy new stream to FStream}
      OutTarHelp.WriteArchiveTail;
      NewStream.Position := 0;
      if (FStream is TMemoryStream) then
        TMemoryStream(FStream).LoadFromStream(NewStream)
      else if (FStream is TAbVirtualMemoryStream) then
        TAbVirtualMemoryStream(FStream).CopyFrom(NewStream, NewStream.Size)
      else begin
        { need new stream to write }
        FStream.Free;
        FStream := TAbSpanStream.Create(FArchiveName, fmOpenWrite or fmShareDenyWrite, mtLocal, FSpanningThreshold);
          FStream.CopyFrom(NewStream, NewStream.Size);

        TAbSpanStream(FStream).OnRequestImage := DoSpanningMediaRequest;
        TAbSpanStream(FStream).OnArchiveProgress := DoArchiveSaveProgress; {!!.04}
      end;

      {update Items list}
      for i := pred( Count ) downto 0 do begin
        if ItemList[i].Action = aaDelete then
          FItemList.Delete( i )
        else if ItemList[i].Action <> aaFailed then
          ItemList[i].Action := aaNone;
      end;

      DoArchiveSaveProgress( 100, Abort );                             {!!.04}
      DoArchiveProgress( 100, Abort );
    finally {NewStream/OutTarHelp}
      OutTarHelp.Free;
      NewStream.Free;
    end;

  finally { InTarHelp }
    { Clean Up }
    InTarHelp.Free;
  end;
end;

procedure TAbTarArchive.TestItemAt(Index: Integer);
var
  Hlpr : TAbTarStreamHelper;
begin
  Hlpr := TAbTarStreamHelper.Create(FStream);
  try
    Hlpr.SeekItem(Index);
    if VerifyTar(FStream) <> atTar then
      raise EAbException.Create('Invalid Tar');
  finally
    Hlpr.Free;
  end;
end;

end.
