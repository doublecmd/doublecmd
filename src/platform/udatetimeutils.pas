{
   Double Commander
   -------------------------------------------------------------------------
   Date and time functions.

   Copyright (C) 2009 cobines (cobines@gmail.com)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}
unit uDateTimeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTypes
  {$IF DEFINED(MSWINDOWS)}
  , Windows
  {$ELSEIF DEFINED(UNIX)}
  , unixutil
  {$ENDIF}
  ;

function FileTimeToDateTime(FileTime : uTypes.TFileTime) : TDateTime;
function DateTimeToFileTime(DateTime : TDateTime) : uTypes.TFileTime;

{en
   Converts system specific UTC time to local time.
}
function FileTimeToLocalFileTime(const lpFileTime: uTypes.TFileTime;
                                 out lpLocalFileTime: uTypes.TFileTime): LongBool;
{en
   Converts system specific local time to UTC time.
}
function LocalFileTimeToFileTime(const lpLocalFileTime: uTypes.TFileTime;
                                 out lpFileTime: uTypes.TFileTime): LongBool;
{en
   Converts Windows UTC file time to Windows local file time.
   @param(lpFileTime TWinFileTime structure containing the UTC-based file time)
   @param(lpLocalFileTime TWinFileTime structure to receive the converted local file time)
   @returns(The function returns @true if successful, @false otherwise)
}
function WinFileTimeToLocalFileTime(const lpFileTime: TWinFileTime;
                                     out lpLocalFileTime: TWinFileTime): LongBool;
{en
   Converts Windows local file time to Windows UTC file time.
   @param(lpLocalFileTime TWinFileTime structure that specifies the local file time)
   @param(lpFileTime TWinFileTime structure to receive the converted UTC-based file time)
   @returns(The function returns @true if successful, @false otherwise)
}
function WinLocalFileTimeToFileTime(const lpLocalFileTime: TWinFileTime;
                                     out lpFileTime: TWinFileTime): LongBool;
{en
   Converts Windows UTC file time to a file time in TDateTime format.
   @param(ft TWinFileTime structure containing the UTC-based file time)
   @returns(File time in TDateTime format)
}
function WinFileTimeToDateTime(ft : TWinFileTime) : TDateTime;
{en
   Converts a file time in TDateTime format to Windows UTC file time.
   @param(dt File time in TDateTime format)
   @returns(Windows UTC-based file time)
}
function DateTimeToWinFileTime(dt : TDateTime) : TWinFileTime;

{$IFDEF MSWINDOWS}
function WinFileTimeToDateTime(ft : Windows.FILETIME) : TDateTime; inline; overload;

function DosFileTimeToDateTime(const DosTime: TDosFileTime): TDateTime;
function DateTimeToDosFileTime(const DateTime: TDateTime): TDosFileTime;

function WinToDosTime(const WinTime: Windows.FILETIME; var DosTime: TDosFileTime): LongBool; overload;
function DosToWinTime(const DosTime: TDosFileTime; var WinTime: Windows.FILETIME): LongBool; overload;
function WinToDosTime(const WinTime: TWinFileTime; var DosTime: TDosFileTime): LongBool;
function DosToWinTime(const DosTime: TDosFileTime; var WinTime: TWinFileTime): LongBool;
{$ENDIF}

{en
   Converts a month short name to month number.
   @param(ShortMonthName Month short name)
   @param(Default Default month number)
   @returns(Month number)
}
function MonthToNumberDef(const ShortMonthName: String; Default: Word): Word;
{en
   Converts a year short record to year long record if need (10 -> 2010).
   @param(Year Year short record)
   @returns(Year long record)
}
function YearShortToLong(Year: Word): Word;
function TwelveToTwentyFour(Hour: Word; Modifier: AnsiString): Word;

implementation

{$IFDEF UNIX}
uses
  uLng;
{$ENDIF}

const  { Short names of months. }
  ShortMonthNames: TMonthNameArray = ('Jan','Feb','Mar','Apr','May','Jun',
                                      'Jul','Aug','Sep','Oct','Nov','Dec');
{$IFDEF UNIX}
const
  SecsPerHour = SecsPerMin * MinsPerHour;
{$ENDIF}

function FileTimeToDateTime(FileTime : uTypes.TFileTime) : TDateTime;
{$IF DEFINED(MSWINDOWS)}
begin
  Result := WinFileTimeToDateTime(FileTime);
end;
{$ELSEIF DEFINED(UNIX)}
var
  Hrs, Mins, Secs : Word;
  TodaysSecs : uTypes.TFileTime;
begin
  FileTimeToLocalFileTime(FileTime, FileTime);

  TodaysSecs := FileTime mod SecsPerDay;
  Hrs        := Word(TodaysSecs div SecsPerHour);
  TodaysSecs := TodaysSecs - (Hrs * SecsPerHour);
  Mins       := Word(TodaysSecs div SecsPerMin);
  Secs       := Word(TodaysSecs - (Mins * SecsPerMin));

  Result := UnixEpoch +                     // Epoch start +
            (FileTime div SecsPerDay) +     // Number of days +
            EncodeTime(Hrs, Mins, Secs, 0); // Time
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}

function DateTimeToFileTime(DateTime : TDateTime) : uTypes.TFileTime;
{$IF DEFINED(MSWINDOWS)}
begin
  Result := DateTimeToWinFileTime(DateTime);
end;
{$ELSEIF DEFINED(UNIX)}
var
  Hrs, Mins, Secs, MSecs : Word;
  Dt, Tm : TDateTime;
  BigTime: QWord;
begin
  Dt := Trunc(DateTime);
  Tm := DateTime - Dt;
  if Dt < UnixEpoch then
    raise EConvertError.CreateFmt(rsMsgErrDateNotSupported, [DateTimeToStr(DateTime)])
  else
    {$PUSH}{$Q-}
    BigTime := Trunc(Dt - UnixEpoch) * SecsPerDay;
    {$POP}

  DecodeTime(Tm, Hrs, Mins, Secs, MSecs);
  {$PUSH}{$Q-}
  BigTime := BigTime + QWord(Hrs * SecsPerHour) + QWord(Mins * SecsPerMin) + Secs;
  {$POP}

{$IFDEF cpu32}
  if BigTime > High(uTypes.TFileTime) then
    raise EConvertError.CreateFmt(rsMsgErrDateNotSupported, [DateTimeToStr(DateTime)])
  else
{$ENDIF}
  LocalFileTimeToFileTime(BigTime, Result);
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}

function FileTimeToLocalFileTime(const lpFileTime: uTypes.TFileTime;
                                 out lpLocalFileTime: uTypes.TFileTime): LongBool;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.FileTimeToLocalFileTime(Windows.FILETIME(lpFileTime), Windows.FILETIME(lpLocalFileTime));
end;
{$ELSE}
begin
  if lpFileTime < High(uTypes.TFileTime) - TZSeconds then
  begin
    lpLocalFileTime := lpFileTime + TZSeconds;
    Result := True;
  end
  else
  begin
    lpLocalFileTime := High(uTypes.TFileTime);
    Result := False;
  end;
end;
{$ENDIF}

function LocalFileTimeToFileTime(const lpLocalFileTime: uTypes.TFileTime;
                                 out lpFileTime: uTypes.TFileTime): LongBool;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.LocalFileTimeToFileTime(Windows.FILETIME(lpLocalFileTime), Windows.FILETIME(lpFileTime));
end;
{$ELSE}
begin
  if lpLocalFileTime > TZSeconds then
  begin
    lpFileTime := lpLocalFileTime - TZSeconds;
    Result := True;
  end
  else
  begin
    lpFileTime := 0;
    Result := False;
  end;
end;
{$ENDIF}

function WinFileTimeToLocalFileTime(const lpFileTime: TWinFileTime;
                                     out lpLocalFileTime: TWinFileTime): LongBool;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.FileTimeToLocalFileTime(Windows.FILETIME(lpFileTime), Windows.FILETIME(lpLocalFileTime));
end;
{$ELSE}
begin
  Int64(lpLocalFileTime) := Int64(lpFileTime) + 10000000 * Int64(TZSeconds);
  Result := True;
end;
{$ENDIF}

function WinLocalFileTimeToFileTime(const lpLocalFileTime: TWinFileTime;
                                     out lpFileTime: TWinFileTime): LongBool;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.LocalFileTimeToFileTime(Windows.FILETIME(lpLocalFileTime), Windows.FILETIME(lpFileTime));
end;
{$ELSE}
begin
  Int64(lpFileTime) := Int64(lpLocalFileTime) - 10000000 * Int64(TZSeconds);
  Result := True;
end;
{$ENDIF}

function WinFileTimeToDateTime(ft : TWinFileTime) : TDateTime;
begin
  WinFileTimeToLocalFileTime(ft,ft);
  Result := (Int64(ft) / 864000000000.0) - 109205.0;
end;

function DateTimeToWinFileTime(dt : TDateTime) : TWinFileTime;
begin
  Int64(Result) := Round((dt + 109205.0) * 864000000000.0);
  WinLocalFileTimeToFileTime(Result, Result);
end;

{$IFDEF MSWINDOWS}
function WinFileTimeToDateTime(ft : Windows.FILETIME) : TDateTime;
begin
  Result := WinFileTimeToDateTime(TWinFileTime(ft));
end;

function DosFileTimeToDateTime(const DosTime: TDosFileTime): TDateTime;
var
  Date, Time : Word;
begin
  Date := LongWord(DosTime) shr 16;
  Time := LongWord(DosTime) and $ffff;
  Result := ComposeDateTime(EncodeDate((Date shr 9) + 1980,(Date shr 5) and 15, Date and 31),
              EncodeTime(Time shr 11, (Time shr 5) and 63, (Time and 31) shl 1,0));
end;

function DateTimeToDosFileTime(const DateTime: TDateTime): TDosFileTime;
var
  YY,MM,DD,H,m,s,msec : Word;
begin
  Decodedate (DateTime,YY,MM,DD);
  DecodeTime (DateTime,h,m,s,msec);

  If (YY<1980) or (YY>2099) then
    Result:=0
  else
  begin
    Result := LongWord((s shr 1) or (m shl 5) or (h shl 11));
    Result := Result or LongWord(DD shl 16 or (MM shl 21) or (word(YY-1980) shl 25));
  end;
end;

function WinToDosTime(const WinTime: Windows.FILETIME; var DosTime: TDosFileTime): LongBool;
var
  lft : Windows.TFILETIME;
begin
  Result:= Windows.FileTimeToLocalFileTime(Windows.FILETIME(WinTime), lft) and
           Windows.FileTimeToDosDateTime(lft, LongRec(Dostime).Hi, LongRec(DosTime).Lo);
end;

function DosToWinTime(const DosTime: TDosFileTime; var WinTime: Windows.FILETIME): LongBool;
var
  lft : Windows.TFILETIME;
begin
  Result := Windows.DosDateTimeToFileTime(LongRec(DosTime).Hi, LongRec(DosTime).Lo, lft) and
            Windows.LocalFileTimeToFileTime(lft, Windows.FILETIME(WinTime));
end;

function WinToDosTime(const WinTime: TWinFileTime; var DosTime: TDosFileTime): LongBool;
var
  lft : Windows.TFILETIME;
begin
  Result:= Windows.FileTimeToLocalFileTime(Windows.FILETIME(WinTime), lft) and
           Windows.FileTimeToDosDateTime(lft, LongRec(Dostime).Hi, LongRec(DosTime).Lo);
end;

function DosToWinTime(const DosTime: TDosFileTime; var WinTime: TWinFileTime): LongBool;
var
  lft : Windows.TFILETIME;
begin
  Result := Windows.DosDateTimeToFileTime(LongRec(DosTime).Hi, LongRec(DosTime).Lo, lft) and
            Windows.LocalFileTimeToFileTime(lft, Windows.FILETIME(WinTime));
end;
{$ENDIF}

function MonthToNumberDef(const ShortMonthName: String; Default: Word): Word;
var
  I: Word;
begin
  Result:= Default;
  if ShortMonthName = EmptyStr then Exit;
  for I:= 1 to 12 do
  if SameText(ShortMonthName, ShortMonthNames[I]) then
    Exit(I);
end;

function YearShortToLong(Year: Word): Word;
begin
  Result:= Year;
  if (Year < 100) then
    begin
      if (Year < 80) then
        Result:= Year + 2000
      else
        Result:= Year + 1900;
    end;
end;

function TwelveToTwentyFour(Hour: Word; Modifier: AnsiString): Word;
begin
  if Modifier = EmptyStr then Exit(Hour);
  case LowerCase(Modifier[1]) of
    'a':
      begin
        if (Hour = 12) then
          Result:= 0;
      end;
    'p':
      begin
        if (Hour < 12) then
          Result:=  Hour + 12;
      end;
  end;
end;

end.
