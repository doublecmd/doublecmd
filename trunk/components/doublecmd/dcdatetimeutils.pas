{
   Double Commander
   -------------------------------------------------------------------------
   Date and time functions.

   Copyright (C) 2009-2012 Przemysław Nagay (cobines@gmail.com)

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

unit DCDateTimeUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes
  {$IF DEFINED(MSWINDOWS)}
  , Windows
  {$ELSEIF DEFINED(UNIX)}
  , unixutil
  {$ENDIF}
  ;

function FileTimeToDateTime(FileTime : DCBasicTypes.TFileTime) : TDateTime;
function DateTimeToFileTime(DateTime : TDateTime) : DCBasicTypes.TFileTime;

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
function WinFileTimeToDateTime(ft : Windows.FILETIME) : TDateTime; inline; overload;
function WinToDosTime(const WinTime: Windows.FILETIME; var DosTime: TDosFileTime): LongBool; overload;
function DosToWinTime(const DosTime: TDosFileTime; var WinTime: Windows.FILETIME): LongBool; overload;
function WinToDosTime(const WinTime: TWinFileTime; var DosTime: TDosFileTime): LongBool;
function DosToWinTime(const DosTime: TDosFileTime; var WinTime: TWinFileTime): LongBool;
{$ENDIF}

function UnixFileTimeToDateTime(UnixTime: TUnixFileTime) : TDateTime;
function DateTimeToUnixFileTime(DateTime: TDateTime) : TUnixFileTime;
function UnixFileTimeToDosTime(UnixTime: TUnixFileTime): TDosFileTime;
function UnixFileTimeToWinTime(UnixTime: TUnixFileTime): TWinFileTime;

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

const  { Short names of months. }
  ShortMonthNames: TMonthNameArray = ('Jan','Feb','Mar','Apr','May','Jun',
                                      'Jul','Aug','Sep','Oct','Nov','Dec');
  SecsPerHour = SecsPerMin * MinsPerHour;

{$IF DEFINED(MSWINDOWS)}
var
  WinTimeZoneBias: LongInt;
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

function FileTimeToDateTime(FileTime : DCBasicTypes.TFileTime) : TDateTime;
{$IF DEFINED(MSWINDOWS)}
begin
  Result := WinFileTimeToDateTime(FileTime);
end;
{$ELSEIF DEFINED(UNIX)}
var
  Hrs, Mins, Secs : Word;
  TodaysSecs : DCBasicTypes.TFileTime;
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

function DateTimeToFileTime(DateTime : TDateTime) : DCBasicTypes.TFileTime;
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
    raise EDateOutOfRange.Create(DateTime)
  else
    {$PUSH}{$Q-}
    BigTime := Trunc(Dt - UnixEpoch) * SecsPerDay;
    {$POP}

  DecodeTime(Tm, Hrs, Mins, Secs, MSecs);
  {$PUSH}{$Q-}
  BigTime := BigTime + QWord(Hrs * SecsPerHour) + QWord(Mins * SecsPerMin) + Secs;
  {$POP}

{$IFDEF cpu32}
  if BigTime > High(DCBasicTypes.TFileTime) then
    raise EDateOutOfRange.Create(DateTime)
  else
{$ENDIF}
  LocalFileTimeToFileTime(BigTime, Result);
end;
{$ELSE}
begin
  Result := 0;
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
begin
  WinFileTimeToLocalFileTime(ft,ft);
  Result := (ft / 864000000000.0) - 109205.0;
end;

function DateTimeToWinFileTime(dt : TDateTime) : TWinFileTime;
begin
  Result := Round((dt + 109205.0) * 864000000000.0);
  WinLocalFileTimeToFileTime(Result, Result);
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

function UnixFileTimeToDateTime(UnixTime: TUnixFileTime) : TDateTime;
var
  Hrs, Mins, Secs : Word;
  TodaysSecs : LongInt;
{$IFDEF MSWINDOWS}
  LocalWinFileTime, WinFileTime: TWinFileTime;
{$ENDIF}
{$IFDEF UNIX}
  LocalUnixTime: TUnixFileTime;
{$ENDIF}
begin
{$IFDEF UNIX}
  if FileTimeToLocalFileTime(UnixTime, LocalUnixTime) then
    UnixTime := LocalUnixTime;
{$ENDIF}

  TodaysSecs := UnixTime mod SecsPerDay;
  Hrs := TodaysSecs div SecsPerHour;
  TodaysSecs := TodaysSecs - (Hrs * SecsPerHour);
  Mins := TodaysSecs div SecsPerMin;
  Secs := TodaysSecs - (Mins * SecsPerMin);

  Result := UnixDateDelta + (UnixTime div SecsPerDay) +
    EncodeTime(Hrs, Mins, Secs, 0);

{$IFDEF MSWINDOWS}
  // Convert universal to local TDateTime.
  WinFileTime := DateTimeToWinFileTime(Result);
  if FileTimeToLocalFileTime(WinFileTime, LocalWinFileTime) then
    WinFileTime := LocalWinFileTime;
  Result := WinFileTimeToDateTime(WinFileTime);
{$ENDIF}
end;

function DateTimeToUnixFileTime(DateTime : TDateTime): TUnixFileTime;
var
  Hrs, Mins, Secs, MSecs : Word;
  Dt, Tm : TDateTime;
{$IFDEF MSWINDOWS}
  LocalWinFileTime, WinFileTime: TWinFileTime;
{$ENDIF}
{$IFDEF UNIX}
  UnixTime: TUnixFileTime;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  // Convert local to universal TDateTime.
  LocalWinFileTime := DateTimeToWinFileTime(Result);
  if LocalFileTimeToFileTime(LocalWinFileTime, WinFileTime) then
    LocalWinFileTime := WinFileTime;
  DateTime := WinFileTimeToDateTime(LocalWinFileTime);
{$ENDIF}

  Dt := Trunc(DateTime);
  Tm := DateTime - Dt;
  if Dt < UnixDateDelta then
    Result := 0
  else
    Result := Trunc(Dt - UnixDateDelta) * SecsPerDay;

  DecodeTime(Tm, Hrs, Mins, Secs, MSecs);
  Result := Result + (Hrs * SecsPerHour) + (Mins * SecsPerMin) + Secs;

{$IFDEF UNIX}
  if LocalFileTimeToFileTime(Result, UnixTime) then
    Result := UnixTime;
{$ENDIF}
end;

function UnixFileTimeToDosTime(UnixTime: TUnixFileTime): TDosFileTime;
begin
  Result := DateTimeToDosFileTime(UnixFileTimeToDateTime(UnixTime));
end;

function UnixFileTimeToWinTime(UnixTime: TUnixFileTime): TWinFileTime;
var
  ft: Windows.TFileTime;
begin
  ft.dwLowDateTime  := $D53E8000; // Unix epoch start
  ft.dwHighDateTime := $019DB1DE;
  if not AdjustWinFileTime(TWinFileTime(ft), Result, 10000000 * Int64(UnixTime)) then
    Result := TWinFileTime(ft);
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

{ EDateOutOfRange }

constructor EDateOutOfRange.Create(ADateTime: TDateTime);
begin
  inherited Create(EmptyStr);
  FDateTime := ADateTime;
end;

{$IF DEFINED(MSWINDOWS)}
procedure InitTimeZoneBias;
var
  TZInfo: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(@TZInfo) of
    TIME_ZONE_ID_UNKNOWN:
      WinTimeZoneBias := TZInfo.Bias;
    TIME_ZONE_ID_STANDARD:
      WinTimeZoneBias := TZInfo.Bias + TZInfo.StandardBias;
    TIME_ZONE_ID_DAYLIGHT:
      WinTimeZoneBias := TZInfo.Bias + TZInfo.DaylightBias;
    else
      WinTimeZoneBias := 0;
  end;
end;

initialization
  InitTimeZoneBias;
{$ENDIF}

end.

