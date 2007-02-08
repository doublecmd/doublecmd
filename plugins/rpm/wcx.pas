//***************************************************************
// This file is part of RPMWCX, a archiver plugin for
// Windows Commander.
// Copyright (C) 2000 Mandryka Yurij  e-mail:braingroup@hotmail.ru
//***************************************************************

//***************************************************************
// This code based on Christian Ghisler (support@ghisler.com) sources
//***************************************************************

{$A-,I-}
unit wcx;

interface

const
  PK_CAPS_NEW      = 01; //Can create new archives
  PK_CAPS_MODIFY   = 02; //Can modify existing archives
  PK_CAPS_MULTIPLE = 04; //Archive can contain multiple files
  PK_CAPS_DELETE   = 08; //Can delete files
  PK_CAPS_OPTIONS  = 16; //Has options dialog

const
  E_SUCCESS        = 0; //Success
  E_END_ARCHIVE    = 10; //No more files in archive
  E_NO_MEMORY      = 11; //Not enough memory
  E_BAD_DATA       = 12; //Data is bad
  E_BAD_ARCHIVE    = 13; //CRC error in archive data
  E_UNKNOWN_FORMAT = 14; //Archive format unknown
  E_EOPEN          = 15; //Cannot open existing file
  E_ECREATE        = 16; //Cannot create file
  E_ECLOSE         = 17; //Error closing file
  E_EREAD          = 18; //Error reading from file
  E_EWRITE         = 19; //Error writing to file
  E_SMALL_BUF      = 20; //Buffer too small
  E_EABORTED       = 21; //Function aborted by user
  E_NO_FILES       = 22; //No files found
  E_TOO_MANY_FILES = 23; //Too many files to pack
  E_NOT_SUPPORTED  = 24; //Function not supported

const
  PK_SKIP    = 0; //Skip this file
  PK_TEST    = 1; //Test file integrity
  PK_EXTRACT = 2; //Extract to disk

type
  POpenArchiveData = ^TOpenArchiveData;
  TOpenArchiveData = record
    ArcName    : PChar;
    OpenMode   : Integer;
    OpenResult : Integer;
    CmtBuf     : PChar;
    CmtBufSize : Integer;
    CmtSize    : Integer;
    CmtState   : Integer;
  end;{TOpenArchiveData}

  PHeaderData = ^THeaderData;
  THeaderData = record
    ArcName    : array [1..260] of Char;
    FileName   : array [1..260] of Char;
    Flags      : Integer;
    PackSize   : Integer;
    UnpSize    : Integer;
    HostOS     : Integer;
    FileCRC    : Integer;
    FileTime   : Integer;
    UnpVer     : Integer;
    Method     : Integer;
    FileAttr   : Integer;
    CmtBuf     : PChar;
    CmtBufSize : Integer;
    CmtSize    : Integer;
    CmtState   : Integer;
   end;{THeaderData}

type
  TProcessDataProc = function (Addr : PChar; Size : Integer) : Integer; stdcall;
  TChangeVolProc   = function (ArcName : PChar; Mode : Integer) : Integer;

function UnixTimeToDosTime(mtime : Longint; usedaylight : Boolean) : Longint;

implementation

uses
  {$IFDEF WIN32}Windows,{$ENDIF} SysUtils;

function UnixTimeToDosTime(mtime : Longint; usedaylight : Boolean) : Longint;
const
  monthdays:array[1..12] of byte=(31,28,31,30,31,30,31,31,30,31,30,31);
{$IFDEF WIN32}
var
  ft, ftStart, ftStop    : TFileTime;
  st                     : TSystemTime;
  tzinfo                 : TTimeZoneInformation;
  hw, lw, month1, month2 : Word;
  bias, day1, day2       : Integer;
  summer                 : Boolean;
{$ENDIF}
begin
{$IFDEF WIN32}
  ft.dwLowDateTime  := $D53E8000;
  ft.dwHighDateTime := $019DB1DE;
  comp(ft):=comp(ft) + 10000000.0 * (mtime);
  FillChar(tzinfo, sizeof(tzinfo), #0);
  GetTimeZoneInformation(tzinfo);
  bias := tzinfo.bias;
  comp(ft) := comp(ft) - 10000000.0 * 60 * bias;
  FileTimeToSystemTime(ft, st);
  if (tzinfo.StandardDate.wmonth <> 0) and usedaylight then begin
    if tzinfo.StandardDate.wYear = 0 then begin
      month1 := tzinfo.DaylightDate.wMonth;
      month2 := tzinfo.StandardDate.wMonth;
      if (st.wmonth < month1) or (st.wmonth > month2) then summer := False else
      if (st.wmonth > month1) and (st.wmonth < month2) then summer := True else
      if (st.wmonth = month1) then begin
        day1 := (st.wDayOfWeek - ((st.wday - 1) mod 7) + 7) mod 7;
        day2 := (tzinfo.DaylightDate.wDayOfWeek - day1 + 7) mod 7 + 1;
        day2 := day2 + 7 * (tzinfo.DaylightDate.wDay - 1);
        if day2 > monthdays[month1] then dec(day2, 7);
        if st.wDay < day2 then summer := False else
        if st.wDay > day2 then summer := True else
          if st.whour >= tzinfo.DaylightDate.whour
            then summer := True
            else summer := False;
      end
      else begin
        day1 := (st.wDayOfWeek - ((st.wday - 1) mod 7) + 7) mod 7;
        day2 := (tzinfo.StandardDate.wDayOfWeek - day1 + 7) mod 7 + 1;
        day2 := day2 + 7 * (tzinfo.StandardDate.wDay - 1);
        if day2 > monthdays[month1] then Dec(day2, 7);
        if st.wDay < day2 then summer := True else
        if st.wDay > day2 then summer := False else
          if st.whour >= tzinfo.StandardDate.whour
            then summer := False
            else summer := True;
      end;
    end else begin
      SystemTimeToFileTime(tzinfo.DaylightDate, ftStart);
      SystemTimeToFileTime(tzinfo.StandardDate, ftStop);
      summer := (comp(ft) > comp(ftStart)) and (comp(ft) <= comp(ftStop));
    end;
    if summer
      then bias := tzinfo.DaylightBias
      else bias := tzinfo.StandardBias;
    comp(ft) := comp(ft) - 10000000.0 * 60 * bias;
  end;
  FileTimeToDosDateTime(ft, hw, lw);
  Result := 65536 * hw + lw;
{$ENDIF}
end;

end.
