{
   Double commander
   -------------------------------------------------------------------------
   WFX plugin for working with File Transfer Protocol

   Copyright (C) 2009-2018 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

unit FtpUtils;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, WfxPlugin;

const
  cFtpsPort = '990';

function IsIpPrivate(Value: String): Boolean;

function EncodeBase64(Data: AnsiString): AnsiString;
function DecodeBase64(Data: AnsiString): AnsiString;

function RepairConnectionName(Connection: AnsiString): AnsiString;
function ExtractConnectionHost(Connection: AnsiString): AnsiString;
function ExtractConnectionPort(Connection: AnsiString): AnsiString;
function ExtractConnectionProt(Connection: AnsiString): AnsiString;

function FormatMachineTime(const Time: TFileTime): String;
function DecodeMachineTime(const Time: String): TDateTime;

implementation

uses
  Base64, DateUtils, synautil
  {$IFDEF MSWINDOWS}
  , Windows
  {$ELSE}
  , UnixUtil
  {$ENDIF}
  ;

function StrToIp(Value: String): LongWord;
var
  S: String;
  I, X: LongWord;
begin
  Result := 0;
  for X := 0 to 3 do
  begin
    S := Fetch(Value, '.');
    I := StrToIntDef(S, 0);
    Result := (256 * Result) + I;
  end;
end;

function IsIpPrivate(Value: String): Boolean;
var
  Index: Integer;
  Binary: LongWord;
const
  PrivAddr: array [0..4, 0..1] of LongWord = (
    // 10.0.0.0 - 10.255.255.255
    (167772160, 184549375),          // Single Class A network
    // 172.16.0.0 - 172.31.255.255
    (2886729728, 2887778303),        // Contiguous range of 16 Class B blocks
    // 192.168.0.0 - 192.168.255.255
    (3232235520, 3232301055),        // Contiguous range of 256 Class C blocks
    // 169.254.0.0 - 169.254.255.255
    (2851995648, 2852061183),        // Link-local address
    // 127.0.0.0 - 127.255.255.255
    (2130706432, 2147483647)         // Loopback (localhost)
  );
begin
 Binary:= StrToIp(Value);
 for Index:= 0 to 4 do
 begin
   if (Binary >= PrivAddr[Index][0]) and (Binary <= PrivAddr[Index][1]) then
     Exit(True)
 end;
 Result:= False;
end;

function EncodeBase64(Data: AnsiString): AnsiString;
var
  StringStream1,
  StringStream2: TStringStream;
begin
  Result:= EmptyStr;
  if Data = EmptyStr then Exit;
  StringStream1:= TStringStream.Create(Data);
  try
    StringStream1.Position:= 0;
    StringStream2:= TStringStream.Create(EmptyStr);
    try
      with TBase64EncodingStream.Create(StringStream2) do
        try
          CopyFrom(StringStream1, StringStream1.Size);
        finally
          Free;
        end;
      Result:= StringStream2.DataString;
    finally
      StringStream2.Free;
    end;
 finally
   StringStream1.Free;
 end;
end;

function DecodeBase64(Data: AnsiString): AnsiString;
var
  StringStream1,
  StringStream2: TStringStream;
  Base64DecodingStream: TBase64DecodingStream;
begin
  Result:= EmptyStr;
  if Data = EmptyStr then Exit;
  StringStream1:= TStringStream.Create(Data);
  try
    StringStream1.Position:= 0;
    StringStream2:= TStringStream.Create(EmptyStr);
    try
      Base64DecodingStream:= TBase64DecodingStream.Create(StringStream1);
      with StringStream2 do
        try
          CopyFrom(Base64DecodingStream, Base64DecodingStream.Size);
        finally
          Base64DecodingStream.Free;
        end;
      Result:= StringStream2.DataString;
    finally
      StringStream2.Free;
    end;
 finally
   StringStream1.Free;
 end;
end;

function RepairConnectionName(Connection: AnsiString): AnsiString;
var
  Index: Integer;
  DenySym: set of AnsiChar;
begin
  Result:= Connection;
  DenySym:= AllowDirectorySeparators + AllowDriveSeparators + ['<'];
  for Index:= 1 to Length(Result) do
  begin
    if Result[Index] in DenySym then
    begin
      Result[Index]:= '_';
    end;
  end;
end;

function ExtractConnectionHost(Connection: AnsiString): AnsiString;
var
  I: Integer;
begin
  I:= Pos('://', Connection);
  if I > 0 then Delete(Connection, 1, I + 2);
  I:= Pos(':', Connection);
  if I > 0 then
    Result:= Copy(Connection, 1, I - 1)
  else
    Result:= Connection;
end;

function ExtractConnectionPort(Connection: AnsiString): AnsiString;
var
  I, J: Integer;
begin
  Result:= EmptyStr;
  I:= Pos('://', Connection);
  if I > 0 then Delete(Connection, 1, I + 2);
  I:= Pos(':', Connection);
  if I > 0 then
  begin
    J:= Pos('/', Connection);
    if J = 0 then J:= MaxInt;
    Result:= Trim(Copy(Connection, I + 1, J - I - 1));
  end;
end;

function ExtractConnectionProt(Connection: AnsiString): AnsiString;
var
  I: Integer;
begin
 Result:= LowerCase(Connection);
 I:= Pos('://', Result);
 if I = 0 then
   Result:= EmptyStr
 else begin
   Result:= Copy(Result, 1, I - 1);
 end;
end;

function FormatMachineTime(const Time: TFileTime): String;
var
  FileTime: TDateTime;
begin
  FileTime:= (Int64(Time) / 864000000000.0) - 109205.0;
  Result:= FormatDateTime('yyyymmddhhnnss', FileTime);
end;

function DecodeMachineTime(const Time: String): TDateTime;
var
  Year, Month, Day: Word;
  Hour, Minute, Second: Word;
begin
  try
    Year:= StrToIntDef(Copy(Time, 1, 4), 1970);
    Month:= StrToIntDef(Copy(Time, 5, 2), 1);
    Day:= StrToIntDef(Copy(Time, 7, 2), 1);
    Hour:= StrToIntDef(Copy(Time, 9, 2), 0);
    Minute:= StrToIntDef(Copy(Time, 11, 2), 0);
    Second:= StrToIntDef(Copy(Time, 13, 2), 0);
    Result:= EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
    Result:= UniversalTimeToLocal(Result, TimeZoneBias);
  except
    Result:= MinDateTime;
  end;
end;

end.

