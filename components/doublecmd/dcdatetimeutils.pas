{
   Double Commander
   -------------------------------------------------------------------------
   Date and time functions.

   Copyright (C) 2009-2012 Przemysław Nagay (cobines@gmail.com)
   Copyright (C) 2017-2024 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit DCDateTimeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes
  {$IF DEFINED(MSWINDOWS)}
  , Windows
  {$ELSEIF DEFINED(UNIX)}
  , Unix, UnixUtil, DCUnix
  {$ENDIF}
  ;

const
  DATE_TIME_NULL = TDateTime(2958466.0);

function FileTimeToDateTime(FileTime : DCBasicTypes.TFileTime) : TDateTime;
function FileTimeToDateTimeEx(FileTime : DCBasicTypes.TFileTimeEx) : TDateTime;
function DateTimeToFileTime(DateTime : TDateTime) : DCBasicTypes.TFileTime;
function DateTimeToFileTimeEx(DateTime : TDateTime) : DCBasicTypes.TFileTimeEx;

function FileTimeToWinFileTime(FileTime : DCBasicTypes.TFileTime) : TWinFileTime;
function WinFileTimeToFileTimeEx(FileTime: TWinFileTime) : DCBasicTypes.TFileTimeEx;

{en
   Converts system specific UTC time to local time.
}
function FileTimeToLocalFileTime(const FileTime: DCBasicTypes.TFileTime;
                                 out LocalFileTime: DCBasicTypes.TFileTime): LongBool;
{en
   Converts system specific local time to UTC time.
}
function LocalFileTimeToFileTime(const LocalFileTime: DCBasicTypes.TFileTime;
                                 out FileTime: DCBasicTypes.TFileTime): LongBool;
{en
   Converts Windows UTC file time to Windows local file time.
   @param(lpFileTime TWinFileTime structure containing the UTC-based file time)
   @param(lpLocalFileTime TWinFileTime structure to receive the converted local file time)
   @returns(The function returns @true if successful, @false otherwise)
}
function WinFileTimeToLocalFileTime(const FileTime: TWinFileTime;
                                    out LocalFileTime: TWinFileTime): LongBool;
{en
   Converts Windows local file time to Windows UTC file time.
   @param(lpLocalFileTime TWinFileTime structure that specifies the local file time)
   @param(lpFileTime TWinFileTime structure to receive the converted UTC-based file time)
   @returns(The function returns @true if successful, @false otherwise)
}
function WinLocalFileTimeToFileTime(const LocalFileTime: TWinFileTime;
                                    out FileTime: TWinFileTime): LongBool;
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

function DosFileTimeToDateTime(const DosTime: TDosFileTime): TDateTime;
function DateTimeToDosFileTime(const DateTime: TDateTime): TDosFileTime;

{$IFDEF MSWINDOWS}
function VariantTimeToDateTime(VarTime: Double): TDateTime;
function WinFileTimeToDateTime(ft : Windows.FILETIME) : TDateTime; inline; overload;
function WinToDosTime(const WinTime: Windows.FILETIME; var DosTime: TDosFileTime): LongBool; overload;
function DosToWinTime(const DosTime: TDosFileTime; var WinTime: Windows.FILETIME): LongBool; overload;
function WinToDosTime(const WinTime: TWinFileTime; var DosTime: TDosFileTime): LongBool;
function DosToWinTime(const DosTime: TDosFileTime; var WinTime: TWinFileTime): LongBool;
{$ENDIF}

function UnixFileTimeToDateTime(UnixTime: TUnixFileTime) : TDateTime;
{$IFDEF UNIX}
function UnixFileTimeToDateTimeEx(UnixTime: DCBasicTypes.TFileTimeEx) : TDateTime;
{$ENDIF}
function DateTimeToUnixFileTime(DateTime: TDateTime) : TUnixFileTime;
function DateTimeToUnixFileTimeEx(DateTime: TDateTime) : DCBasicTypes.TFileTimeEx;
function UnixFileTimeToFileTime(UnixTime: TUnixFileTime): DCBasicTypes.TFileTime;
function UnixFileTimeToDosTime(UnixTime: TUnixFileTime): TDosFileTime;
function UnixFileTimeToWinTime(UnixTime: TUnixFileTime): TWinFileTime;
function WinFileTimeToUnixTime(WinTime: TWinFileTime) : TUnixFileTime;

function WcxFileTimeToFileTime(WcxTime: LongInt): DCBasicTypes.TFileTime; inline;
function FileTimeToWcxFileTime(FileTime: DCBasicTypes.TFileTime): LongInt; inline;
function WcxFileTimeToDateTime(WcxTime: LongInt): TDateTime;
function UnixFileTimeToWcxTime(UnixTime: TUnixFileTime): LongInt;

function GetTimeZoneBias: LongInt;

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
function FileTimeCompare(SourceTime, TargetTime: TDateTime; NtfsShift: Boolean): Integer;

type
  EDateOutOfRange = class(EConvertError)
  private
    FDateTime: TDateTime;
  public
    constructor Create(ADateTime: TDateTime);
    property DateTime: TDateTime read FDateTime;
  end;

implementation

uses
  DateUtils;

const
  UnixWinEpoch = TWinFileTime($019DB1DED53E8000); // Unix epoch start

const  { Short names of months. }
  ShortMonthNames: TMonthNameArray = ('Jan','Feb','Mar','Apr','May','Jun',
                                      'Jul','Aug','Sep','Oct','Nov','Dec');

{$IF DEFINED(MSWINDOWS)}
var
  WinTimeZoneBias: LongInt;
  TzSpecificLocalTimeToSystemTime: function(lpTimeZoneInformation: PTimeZoneInformation;
                                            lpLocalTime, lpUniversalTime: PSystemtime): BOOL; stdcall;
{$ENDIF}

function AdjustUnixFileTime(const FileTime: DCBasicTypes.TFileTime;
                            out AdjustedFileTime: DCBasicTypes.TFileTime;
                            AdjustValue: Int64): Boolean;
begin
  if AdjustValue < 0 then
  begin
    if FileTime < DCBasicTypes.TFileTime(-AdjustValue) then
    begin
      AdjustedFileTime := 0;
      Result := False;
    end
    else
    begin
      AdjustedFileTime := FileTime - DCBasicTypes.TFileTime(-AdjustValue);
      Result := True;
    end;
  end
  else
  begin
    if High(FileTime) - FileTime < DCBasicTypes.TFileTime(AdjustValue) then
    begin
      AdjustedFileTime := High(FileTime);
      Result := False;
    end
    else
    begin
      AdjustedFileTime := FileTime + DCBasicTypes.TFileTime(AdjustValue);
      Result := True;
    end;
  end;
end;

function AdjustWinFileTime(const FileTime: TWinFileTime;
                           out AdjustedFileTime: TWinFileTime;
                           AdjustValue: Int64): Boolean;
begin
  if AdjustValue < 0 then
  begin
    if FileTime < DCBasicTypes.TWinFileTime(-AdjustValue) then
    begin
      AdjustedFileTime := 0;
      Result := False;
    end
    else
    begin
      AdjustedFileTime := FileTime - DCBasicTypes.TWinFileTime(-AdjustValue);
      Result := True;
    end;
  end
  else
  begin
    if High(FileTime) - FileTime < DCBasicTypes.TWinFileTime(AdjustValue) then
    begin
      AdjustedFileTime := High(FileTime);
      Result := False;
    end
    else
    begin
      AdjustedFileTime := FileTime + DCBasicTypes.TWinFileTime(AdjustValue);
      Result := True;
    end;
  end;
end;

function FileTimeToDateTime(FileTime : DCBasicTypes.TFileTime) : TDateTime; inline;
{$IF DEFINED(MSWINDOWS)}
begin
  Result := WinFileTimeToDateTime(FileTime);
end;
{$ELSEIF DEFINED(UNIX)}
begin
  Result := UnixFileTimeToDateTime(FileTime);
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}

function FileTimeToDateTimeEx(FileTime : DCBasicTypes.TFileTimeEx) : TDateTime;
{$IF DEFINED(MSWINDOWS)}
begin
  Result := WinFileTimeToDateTime(FileTime);
end;
{$ELSEIF DEFINED(UNIX)}
begin
  Result := UnixFileTimeToDateTimeEx(FileTime);
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}

function DateTimeToFileTime(DateTime : TDateTime) : DCBasicTypes.TFileTime; inline;
{$IF DEFINED(MSWINDOWS)}
begin
  Result := DateTimeToWinFileTime(DateTime);
end;
{$ELSEIF DEFINED(UNIX)}
begin
  Result := DateTimeToUnixFileTime(DateTime);
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}

function DateTimeToFileTimeEx(DateTime : TDateTime) : DCBasicTypes.TFileTimeEx; inline;
{$IF DEFINED(MSWINDOWS)}
begin
  Result := DateTimeToWinFileTime(DateTime);
end;
{$ELSEIF DEFINED(UNIX)}
begin
  Result := DateTimeToUnixFileTimeEx(DateTime);
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}

function FileTimeToWinFileTime(FileTime: DCBasicTypes.TFileTime): TWinFileTime;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= TWinFileTime(FileTime)
end;
{$ELSEIF DEFINED(UNIX)}
begin
  Result:= UnixFileTimeToWinTime(TUnixFileTime(FileTime));
end;
{$ENDIF}

function WinFileTimeToFileTimeEx(FileTime: TWinFileTime): DCBasicTypes.TFileTimeEx;
{$IF DEFINED(MSWINDOWS)}
begin
  Result := TFileTimeEx(FileTime);
end;
{$ELSEIF DEFINED(UNIX)}
begin
  Result.Sec:= Int64((FileTime - UnixWinEpoch) div 10000000);
  Result.NanoSec:= Int64((FileTime - UnixWinEpoch) mod 10000000) * 100;
end;
{$ENDIF}

function FileTimeToLocalFileTime(const FileTime: DCBasicTypes.TFileTime;
                                 out LocalFileTime: DCBasicTypes.TFileTime): LongBool;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.FileTimeToLocalFileTime(@Windows.FILETIME(FileTime), @Windows.FILETIME(LocalFileTime));
end;
{$ELSE}
begin
  Result := AdjustUnixFileTime(FileTime, LocalFileTime, Tzseconds);
end;
{$ENDIF}

function LocalFileTimeToFileTime(const LocalFileTime: DCBasicTypes.TFileTime;
                                 out FileTime: DCBasicTypes.TFileTime): LongBool;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.LocalFileTimeToFileTime(@Windows.FILETIME(LocalFileTime), @Windows.FILETIME(FileTime));
end;
{$ELSE}
begin
  Result := AdjustUnixFileTime(LocalFileTime, FileTime, -Tzseconds);
end;
{$ENDIF}

function WinFileTimeToLocalFileTime(const FileTime: TWinFileTime;
                                    out LocalFileTime: TWinFileTime): LongBool;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.FileTimeToLocalFileTime(@Windows.FILETIME(FileTime), @Windows.FILETIME(LocalFileTime));
end;
{$ELSE}
begin
  Result := AdjustWinFileTime(FileTime, LocalFileTime, 10000000 * Int64(TZSeconds));
end;
{$ENDIF}

function WinLocalFileTimeToFileTime(const LocalFileTime: TWinFileTime;
                                    out FileTime: TWinFileTime): LongBool;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.LocalFileTimeToFileTime(@Windows.FILETIME(LocalFileTime), @Windows.FILETIME(FileTime));
end;
{$ELSE}
begin
  Result := AdjustWinFileTime(LocalFileTime, FileTime, -10000000 * Int64(TZSeconds));
end;
{$ENDIF}

function WinFileTimeToDateTime(ft : TWinFileTime) : TDateTime;
{$IF DEFINED(MSWINDOWS)}
var
  lpUniversalTime, lpLocalTime: TSystemTime;
{$ENDIF}
begin
{$IF DEFINED(MSWINDOWS)}
  if (Win32MajorVersion > 5) then
  begin
    FileTimeToSystemTime(@ft, @lpUniversalTime);
    SystemTimeToTzSpecificLocalTime(nil, @lpUniversalTime, @lpLocalTime);
    Result := SystemTimeToDateTime(lpLocalTime);
  end
  else
{$ENDIF}
  begin
    WinFileTimeToLocalFileTime(ft,ft);
    Result := (ft / 864000000000.0) - 109205.0;
  end;
end;

function DateTimeToWinFileTime(dt : TDateTime) : TWinFileTime;
{$IF DEFINED(MSWINDOWS)}
var
  lpUniversalTime, lpLocalTime: TSystemTime;
{$ENDIF}
begin
{$IF DEFINED(MSWINDOWS)}
  if (Win32MajorVersion > 5) then
  begin
    DateTimeToSystemTime(dt, lpLocalTime);
    TzSpecificLocalTimeToSystemTime(nil, @lpLocalTime, @lpUniversalTime);
    SystemTimeToFileTime(@lpUniversalTime, @Result);
  end
  else
{$ENDIF}
  begin
    Result := Round((Extended(dt) + 109205.0) * 864000000000.0);
    WinLocalFileTimeToFileTime(Result, Result);
  end;
end;

function DosFileTimeToDateTime(const DosTime: TDosFileTime): TDateTime;
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

  Result := ComposeDateTime(EncodeDate(Yr, Mo, Dy),
                            EncodeTime(Hr, Mn, S, 0));
end;

function DateTimeToDosFileTime(const DateTime: TDateTime): TDosFileTime;
var
  Yr, Mo, Dy : Word;
  Hr, Mn, S, MS: Word;
begin
  DecodeDate(DateTime, Yr, Mo, Dy);
  if (Yr < 1980) or (Yr > 2107) then // outside DOS file date year range
    Yr := 1980;
  DecodeTime(DateTime, Hr, Mn, S, MS);

  LongRec(Result).Lo := (S shr 1) or (Mn shl 5) or (Hr shl 11);
  LongRec(Result).Hi := Dy or (Mo shl 5) or (Word(Yr - 1980) shl 9);
end;

{$IFDEF MSWINDOWS}
function VariantTimeToDateTime(VarTime: Double): TDateTime;
var
  lpUniversalTime, lpLocalTime: TSystemTime;
begin
  if (Win32MajorVersion > 5) then
  begin
    DateTimeToSystemTime(VarTime, lpUniversalTime);
    SystemTimeToTzSpecificLocalTime(nil, @lpUniversalTime, @lpLocalTime);
    Result := SystemTimeToDateTime(lpLocalTime);
  end
  else begin
    Result := IncMinute(VarTime, -WinTimeZoneBias);
  end;
end;

function WinFileTimeToDateTime(ft : Windows.FILETIME) : TDateTime;
begin
  Result := WinFileTimeToDateTime(TWinFileTime(ft));
end;

function WinToDosTime(const WinTime: Windows.FILETIME; var DosTime: TDosFileTime): LongBool;
var
  lft : Windows.TFILETIME;
begin
  Result:= Windows.FileTimeToLocalFileTime(@Windows.FILETIME(WinTime), @lft) and
           Windows.FileTimeToDosDateTime(@lft, @LongRec(Dostime).Hi, @LongRec(DosTime).Lo);
end;

function DosToWinTime(const DosTime: TDosFileTime; var WinTime: Windows.FILETIME): LongBool;
var
  lft : Windows.TFILETIME;
begin
  Result := Windows.DosDateTimeToFileTime(LongRec(DosTime).Hi, LongRec(DosTime).Lo, @lft) and
            Windows.LocalFileTimeToFileTime(@lft, @Windows.FILETIME(WinTime));
end;

function WinToDosTime(const WinTime: TWinFileTime; var DosTime: TDosFileTime): LongBool;
var
  lft : Windows.TFILETIME;
begin
  Result:= Windows.FileTimeToLocalFileTime(@Windows.FILETIME(WinTime), @lft) and
           Windows.FileTimeToDosDateTime(@lft, @LongRec(Dostime).Hi, @LongRec(DosTime).Lo);
end;

function DosToWinTime(const DosTime: TDosFileTime; var WinTime: TWinFileTime): LongBool;
var
  lft : Windows.TFILETIME;
begin
  Result := Windows.DosDateTimeToFileTime(LongRec(DosTime).Hi, LongRec(DosTime).Lo, @lft) and
            Windows.LocalFileTimeToFileTime(@lft, @Windows.FILETIME(WinTime));
end;
{$ENDIF}


{$IF DEFINED(UNIX)}
function UnixFileTimeToDateTime(UnixTime: TUnixFileTime) : TDateTime;
var
  filetime: DCBasicTypes.TFileTimeEx;
begin
  filetime:= TFileTimeEx.create(UnixTime);
  Result:= UnixFileTimeToDateTimeEx(filetime);
end;

function UnixFileTimeToDateTimeEx(UnixTime: DCBasicTypes.TFileTimeEx) : TDateTime;
var
  ATime: TTimeStruct;
  Milliseconds: Word;
begin
  if (fpLocalTime(@UnixTime.sec, @ATime) = nil) then
    Exit(UnixEpoch);

  ATime.tm_mon += 1;
  ATime.tm_year += 1900;

  if (ATime.tm_year < 1) then
    ATime.tm_year := 1
  else if (ATime.tm_year > 9999) then
    ATime.tm_year := 9999;
  if ATime.tm_sec > 59 then
    ATime.tm_sec := 59;

  if (UnixTime.nanosec > 999000000) then
    Milliseconds := 999
  else begin
    Milliseconds := Round( Extended(UnixTime.nanosec) / (1000.0 * 1000.0) );
  end;

  Result := ComposeDateTime(EncodeDate(ATime.tm_year, ATime.tm_mon, ATime.tm_mday),
                            EncodeTime(ATime.tm_hour, ATime.tm_min, ATime.tm_sec, milliseconds));
end;
{$ELSE}
function UnixFileTimeToDateTime(UnixTime: TUnixFileTime) : TDateTime;
var
  WinFileTime: TWinFileTime;
begin
  WinFileTime:= UnixFileTimeToWinTime(UnixTime);
  Result:= WinFileTimeToDateTime(WinFileTime);
end;
{$ENDIF}

function DateTimeToUnixFileTime(DateTime : TDateTime): TUnixFileTime;
{$IF DEFINED(UNIX)}
var
  AUnixTime: TTime;
  ATime: TTimeStruct;
  Year, Month, Day: Word;
  Hour, Minute, Second, MilliSecond: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Minute, Second, MilliSecond);

  ATime.tm_isdst:= -1;

  ATime.tm_year:= Year - 1900;
  ATime.tm_mon:=  Month - 1;
  ATime.tm_mday:= Day;

  ATime.tm_hour:= Hour;
  ATime.tm_min:= Minute;
  ATime.tm_sec:= Second;

  AUnixTime:= fpMkTime(@ATime);

  if (AUnixTime = -1) then
    Result:= 0
  else begin
    Result:= TUnixFileTime(AUnixTime);
  end;
end;
{$ELSE}
var
  WinFileTime: TWinFileTime;
begin
  WinFileTime:= DateTimeToWinFileTime(DateTime);
  Result:= WinFileTimeToUnixTime(WinFileTime);
end;
{$ENDIF}

function DateTimeToUnixFileTimeEx(DateTime : TDateTime): DCBasicTypes.TFileTimeEx;
{$IF DEFINED(UNIX)}
var
  AUnixTime: TTime;
  ATime: TTimeStruct;
  Year, Month, Day: Word;
  Hour, Minute, Second, MilliSecond: Word;
begin
  if DateTime < UnixEpoch then
    raise EDateOutOfRange.Create(DateTime);

  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Minute, Second, MilliSecond);

  ATime.tm_isdst:= -1;

  ATime.tm_year:= Year - 1900;
  ATime.tm_mon:=  Month - 1;
  ATime.tm_mday:= Day;

  ATime.tm_hour:= Hour;
  ATime.tm_min:= Minute;
  ATime.tm_sec:= Second;

  AUnixTime:= fpMkTime(@ATime);

  if (AUnixTime = -1) then
    Result:= TFileTimeExNull
  else begin
    Result:= TFileTimeEx.create(AUnixTime, MilliSecond*1000*1000);
  end;
end;
{$ELSE}
var
  WinFileTime: TWinFileTime;
begin
  WinFileTime:= DateTimeToWinFileTime(DateTime);
  Result:= WinFileTimeToUnixTime(WinFileTime);
end;
{$ENDIF}

function UnixFileTimeToFileTime(UnixTime: TUnixFileTime): DCBasicTypes.TFileTime; inline;
begin
{$IF DEFINED(MSWINDOWS)}
  Result:= UnixFileTimeToWinTime(UnixTime);
{$ELSE}
  Result:= UnixTime;
{$ENDIF}
end;

function UnixFileTimeToDosTime(UnixTime: TUnixFileTime): TDosFileTime;
begin
  Result := DateTimeToDosFileTime(UnixFileTimeToDateTime(UnixTime));
end;

function UnixFileTimeToWinTime(UnixTime: TUnixFileTime): TWinFileTime;
var
  WinFileTime: TWinFileTime;
begin
  WinFileTime := UnixWinEpoch;
  if not AdjustWinFileTime(WinFileTime, Result, 10000000 * Int64(UnixTime)) then
    Result := WinFileTime;
end;

function WinFileTimeToUnixTime(WinTime: TWinFileTime): TUnixFileTime;
begin
  if (WinTime < UnixWinEpoch) then
    Result:= 0
  else
    Result:= TUnixFileTime((WinTime - UnixWinEpoch) div 10000000);
end;

function WcxFileTimeToFileTime(WcxTime: LongInt): DCBasicTypes.TFileTime;
begin
{$IF DEFINED(MSWINDOWS)}
  DosToWinTime(TDosFileTime(WcxTime), Result);
{$ELSE}
  Result := TFileTime(WcxTime);
{$ENDIF}
end;

function FileTimeToWcxFileTime(FileTime: DCBasicTypes.TFileTime): LongInt;
begin
{$IF DEFINED(MSWINDOWS)}
  WinToDosTime(FileTime, Result);
{$ELSE}
  Result := LongInt(FileTime);
{$ENDIF}
end;

function WcxFileTimeToDateTime(WcxTime: LongInt): TDateTime;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := DosFileTimeToDateTime(WcxTime);
{$ELSEIF DEFINED(UNIX)}
{$PUSH}{$R-}
  Result := FileTimeToDateTime(WcxTime);
{$POP}
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function UnixFileTimeToWcxTime(UnixTime: TUnixFileTime): LongInt;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := UnixFileTimeToDosTime(UnixTime);
{$ELSEIF DEFINED(UNIX)}
{$PUSH}{$R-}
  Result := UnixTime;
{$POP}
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function GetTimeZoneBias: LongInt;
begin
  {$IF DEFINED(MSWINDOWS)}
  Result := WinTimeZoneBias;
  {$ELSEIF DEFINED(UNIX)}
  Result := -Tzseconds div 60;
  {$ELSE}
  Result := 0;
  {$ENDIF}
end;

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
  Result:= Hour;
  if Length(Modifier) > 0 then
  begin
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
end;

function FileTimeCompare(SourceTime, TargetTime: TDateTime; NtfsShift: Boolean): Integer;
const
  TimeDiff = 3100 / MSecsPerDay;
  NtfsDiff:TDateTime = (1/HoursPerDay);
var
  FileTimeDiff,
  NtfsTimeDiff: TDateTime;
begin
  FileTimeDiff:= SourceTime - TargetTime;
  if NtfsShift then
  begin
    NtfsTimeDiff:= FileTimeDiff - NtfsDiff;
    if (NtfsTimeDiff > -TimeDiff) and (NtfsTimeDiff < TimeDiff) then
      Exit(0);
    NtfsTimeDiff:= FileTimeDiff + NtfsDiff;
    if (NtfsTimeDiff > -TimeDiff) and (NtfsTimeDiff < TimeDiff) then
      Exit(0);
  end;
  if (FileTimeDiff > -TimeDiff) and (FileTimeDiff < TimeDiff) then
    Result:= 0
  else if FileTimeDiff > 0 then
    Result:= +1
  else if FileTimeDiff < 0 then
    Result:= -1;
end;

{ EDateOutOfRange }

constructor EDateOutOfRange.Create(ADateTime: TDateTime);
begin
  inherited Create(EmptyStr);
  FDateTime := ADateTime;
end;

{$IF DEFINED(MSWINDOWS)}
initialization
  WinTimeZoneBias := GetLocalTimeOffset;
  if (Win32MajorVersion > 5) then
  begin
    Pointer(TzSpecificLocalTimeToSystemTime):= GetProcAddress(GetModuleHandle(Kernel32),
                                                              'TzSpecificLocalTimeToSystemTime');
  end;
{$ENDIF}

end.

