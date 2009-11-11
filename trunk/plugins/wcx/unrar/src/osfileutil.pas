{
  Functions to convert various OS-specific
  file attributes to others operating systems. (Win/Unix)

  Contains parts of AbUtils from TPAbbrevia toolkit.
}

unit osFileUtil;

{$mode objfpc}{$H+}

interface

  { file attributes }
  function OsDOS2UnixFileAttributes(Attr: LongWord): LongWord;
  function OsUnix2DosFileAttributes(Attr: LongWord): LongWord;

  // -----------------------------------------------------------
  // Use the following functions to convert time Unix<->Windows.
  // TDateTime is assumed to be local time.

  function OsUnixFileTimeToDateTime(UnixTime : LongInt) : TDateTime;
  function OsDateTimeToUnixFileTime(DateTime : TDateTime) : LongInt;

  function OsDosFileTimeToDateTime(DosTime : LongInt) : TDateTime;
  function OsDateTimeToDosFileTime(Value : TDateTime) : LongInt;

  // Below functions work for MS-DOS and Unix time.
  function OsFileTimeToLocalFileTime(FileTime: LongInt): LongInt;
  function OsLocalFileTimeToFileTime(FileTime: LongInt): LongInt;


implementation

uses
  DateUtils, SysUtils
{$IFDEF WINDOWS}
  , Windows
{$ENDIF}
{$IFDEF UNIX}
  , UnixUtil
{$ENDIF}
  ;

{ date and time stuff }
const
  Unix0Date: TDateTime = 25569;      {Date1970 - Date1900}

  SecondsInDay    = 86400;  {Number of seconds in a day}
  SecondsInHour   =  3600;  {Number of seconds in an hour}
  SecondsInMinute =    60;  {Number of seconds in a minute}
  HoursInDay      =    24;  {Number of hours in a day}
  MinutesInHour   =    60;  {Number of minutes in an hour}
  MinutesInDay    =  1440;  {Number of minutes in a day}

{ LINUX File Types and Permissions }
  // These use high 4 bits
  AB_FMODE_SOCKET             = $C000;
  AB_FMODE_FILELINK           = $A000;       // Unix symbolic link
  AB_FMODE_FILEREG            = $8000;       // Unix regular file
  AB_FMODE_BLOCKSPECFILE      = $6000;       // Unix block special
  AB_FMODE_DIR                = $4000;
  AB_FMODE_CHARSPECFILE       = $2000;       // Unix character special
  AB_FMODE_FIFO               = $1000;       // Unix fifo
  AB_FMODE_FILE               = $0000;

  // These use low 12 bits
  AB_FPERMISSION_UID          = $0800; { Set UID on execution }
  AB_FPERMISSION_GID          = $0400; { Set GID on execution }
  AB_FPERMISSION_VTX          = $0200; { Save text (sticky bit) }
  AB_FPERMISSION_OWNERREAD    = $0100; { read by owner }
  AB_FPERMISSION_OWNERWRITE   = $0080; { write by owner }
  AB_FPERMISSION_OWNEREXECUTE = $0040; { execute/search by owner }
  AB_FPERMISSION_GROUPREAD    = $0020; { read by group }
  AB_FPERMISSION_GROUPWRITE   = $0010; { write by group }
  AB_FPERMISSION_GROUPEXECUTE = $0008; { execute/search by group }
  AB_FPERMISSION_OTHERREAD    = $0004; { read by other }
  AB_FPERMISSION_OTHERWRITE   = $0002; { write by other }
  AB_FPERMISSION_OTHEREXECUTE = $0001; { execute/search by other }

{ -------------------------------------------------------------------------- }
function OsUnixFileTimeToDateTime(UnixTime : LongInt) : TDateTime;
{ convert unix date to Delphi TDateTime }
var
  Hrs, Mins, Secs : Word;
  TodaysSecs : LongInt;
{$IFDEF MSWINDOWS}
  DosFileTime : Longint;
{$ENDIF}
begin
{$IFDEF UNIX}
  UnixTime := OsFileTimeToLocalFileTime(UnixTime);
{$ENDIF}

  TodaysSecs := UnixTime mod SecondsInDay;
  Hrs := TodaysSecs div SecondsInHour;
  TodaysSecs := TodaysSecs - (Hrs * SecondsInHour);
  Mins := TodaysSecs div SecondsInMinute;
  Secs := TodaysSecs - (Mins * SecondsInMinute);

  Result := Unix0Date + (UnixTime div SecondsInDay) +
    EncodeTime(Hrs, Mins, Secs, 0);

{$IFDEF MSWINDOWS}
  // Convert universal to local TDateTime.
  DosFileTime := OsDateTimeToDosFileTime(Result);
  DosFileTime := OsFileTimeToLocalFileTime(DosFileTime);
  Result      := OsDosFileTimeToDateTime(DosFileTime);
{$ENDIF}
end;
{ -------------------------------------------------------------------------- }
function OsDateTimeToUnixFileTime(DateTime : TDateTime) : LongInt;
{ convert Delphi TDateTime to unix date }
var
  Hrs, Mins, Secs, MSecs : Word;
  Dt, Tm : TDateTime;
{$IFDEF MSWINDOWS}
  DosFileTime : Longint;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  // Convert local to universal TDateTime.
  DosFileTime := OsDateTimeToDosFileTime(DateTime);
  DosFileTime := OsLocalFileTimeToFileTime(DosFileTime);
  DateTime    := OsDosFileTimeToDateTime(DosFileTime);
{$ENDIF}

  Dt := Trunc(DateTime);
  Tm := DateTime - Dt;
  if Dt < Unix0Date then
    Result := 0
  else
    Result := Trunc(Dt - Unix0Date) * SecondsInDay;

  DecodeTime(Tm, Hrs, Mins, Secs, MSecs);
  Result := Result + (Hrs * SecondsInHour) + (Mins * SecondsInMinute) + Secs;

{$IFDEF UNIX}
  Result := OsLocalFileTimeToFileTime(Result);
{$ENDIF}
end;
{ -------------------------------------------------------------------------- }
function OsDosFileTimeToDateTime(DosTime : LongInt) : TDateTime;
var
  Yr, Mo, Dy : Word;
  Hr, Mn, S  : Word;
  FileDate, FileTime : Word;
begin
  FileDate := LongRec(DosTime).Hi;
  FileTime := LongRec(DosTime).Lo;

  Yr := FileDate shr 9 + 1980;

  Mo := FileDate shr 5 and 15;
  if Mo < 1 then Mo := 1;
  if Mo > 12 then Mo := 12;

  Dy := FileDate and 31;
  if Dy < 1 then Dy := 1;
  if Dy > DaysInAMonth(Yr, Mo) then
    Dy := DaysInAMonth(Yr, Mo);

  Hr := FileTime shr 11;
  if Hr > 23 then Hr := 23;

  Mn := FileTime shr 5 and 63;
  if Mn > 59 then Mn := 59;

  S  := FileTime and 31 shl 1;
  if S > 59 then S := 59;

  Result :=
    EncodeDate(Yr, Mo, Dy) +
    EncodeTime(Hr, Mn, S, 0);
end;

function OsDateTimeToDosFileTime(Value : TDateTime) : LongInt;
var
  Yr, Mo, Dy : Word;
  Hr, Mn, S, MS: Word;
begin
  DecodeDate(Value, Yr, Mo, Dy);
  if (Yr < 1980) or (Yr > 2107) then { outside DOS file date year range }
    Yr := 1980;
  DecodeTime(Value, Hr, Mn, S, MS);

  LongRec(Result).Lo := (S shr 1) or (Mn shl 5) or (Hr shl 11);
  LongRec(Result).Hi := Dy or (Mo shl 5) or ((Yr - 1980) shl 9);
end;

function OsFileTimeToLocalFileTime(FileTime: LongInt): LongInt;
{$IFDEF MSWINDOWS}
var
  WinFileTime     : TFileTime = (dwLowDateTime:0; dwHighDateTime: 0);
  LocalWinFileTime: TFileTime = (dwLowDateTime:0; dwHighDateTime: 0);
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  DosDateTimeToFileTime(LongRec(FileTime).hi, LongRec(FileTime).lo, WinFileTime);
  FileTimeToLocalFileTime(WinFileTime, LocalWinFileTime);
  FileTimeToDosDateTime(LocalWinFileTime, LongRec(FileTime).hi, LongRec(FileTime).lo);
{$ENDIF}
{$IFDEF UNIX}
  FileTime := FileTime + TzSeconds;
{$ENDIF}
  Result := FileTime;
end;

function OsLocalFileTimeToFileTime(FileTime: LongInt): LongInt;
{$IFDEF MSWINDOWS}
var
  WinFileTime, LocalWinFileTime: TFileTime;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  DosDateTimeToFileTime(LongRec(FileTime).hi, LongRec(FileTime).lo, LocalWinFileTime);
  LocalFileTimeToFileTime(LocalWinFileTime, WinFileTime);
  FileTimeToDosDateTime(WinFileTime, LongRec(FileTime).hi, LongRec(FileTime).lo);
{$ENDIF}
{$IFDEF UNIX}
  FileTime := FileTime - Tzseconds;
{$ENDIF}
  Result := FileTime;
end;

{ -------------------------------------------------------------------------- }
function OsDOS2UnixFileAttributes(Attr: LongWord): LongWord;
begin
  Result := { default permissions }
    AB_FPERMISSION_OWNERREAD or
    AB_FPERMISSION_GROUPREAD or
    AB_FPERMISSION_OTHERREAD;

  if (Attr and faReadOnly) = 0 then
    Result := Result or AB_FPERMISSION_OWNERWRITE;

  if (Attr and faDirectory) <> 0 then
  begin
    Result := Result or AB_FMODE_DIR;
    Result := Result or AB_FPERMISSION_OWNEREXECUTE;
  end
  else
    Result := Result or AB_FMODE_FILE;
end;
{ -------------------------------------------------------------------------- }
function OsUnix2DosFileAttributes(Attr: LongWord): LongWord;
begin
  Result := 0;
  case (Attr and $F000) of
    AB_FMODE_FILE, AB_FMODE_FILEREG:
      Result := faArchive; { standard file }

    AB_FMODE_FILELINK:
      Result := Result or faSymLink;

    AB_FMODE_DIR: { directory }
      Result := Result or faDirectory;

    AB_FMODE_FIFO,
    AB_FMODE_CHARSPECFILE,
    AB_FMODE_BLOCKSPECFILE,
    AB_FMODE_SOCKET:
      Result := Result or faSysFile;
  end;

  if (Attr and AB_FPERMISSION_OWNERWRITE) = 0 then
    Result := Result or faReadOnly;
end;

end.

