{
   Double commander
   -------------------------------------------------------------------------
   WFX plugin for working with File Transfer Protocol

   Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit FtpUtils;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, WfxPlugin;

const
  S_IFMT   = $F000;
  { first-in/first-out (FIFO/pipe)            }
  S_IFIFO  = $1000;
  { character-special file (tty/console)      }
  S_IFCHR  = $2000;
  { directory                                 }
  S_IFDIR  = $4000;
  { blocking device (unused)                  }
  S_IFBLK  = $6000;
  { regular                                   }
  S_IFREG  = $8000;
  { symbolic link (unused)                    }
  S_IFLNK  = $A000;
  { Berkeley socket                           }
  S_IFSOCK = $C000;
  S_IRWXU  = $01C0;
  S_IRUSR  = $0100;
  S_IWUSR  = $0080;
  S_IXUSR  = $0040;
  S_IREAD  = S_IRUSR;
  S_IWRITE = S_IWUSR;
  S_IEXEC  = S_IXUSR;
  { POSIX file modes: group permission...  }
  S_IRWXG  = $0038;
  S_IRGRP  = $0020;
  S_IWGRP  = $0010;
  S_IXGRP  = $0008;
  { POSIX file modes: other permission...  }
  S_IRWXO  = $0007;
  S_IROTH  = $0004;
  S_IWOTH  = $0002;
  S_IXOTH  = $0001;
  { POSIX setuid(), setgid(), and sticky...  }
  S_ISUID  = $0800;
  S_ISGID  = $0400;
  S_ISVTX  = $0200;

const
  ftpProtocol = 'ftp://';

function ModeStr2Mode(const sMode: String): Integer;

function EncodeBase64(Data: AnsiString): AnsiString;
function DecodeBase64(Data: AnsiString): AnsiString;

function ExtractConnectionHost(Connection: AnsiString): AnsiString;
function ExtractConnectionPort(Connection: AnsiString): AnsiString;

function FileTimeToLocalFileTimeEx(const lpFileTime: TFileTime; var lpLocalFileTime: TFileTime): LongBool;
function LocalFileTimeToFileTimeEx(const lpLocalFileTime: TFileTime; var lpFileTime: TFileTime): LongBool;
function FileTimeToDateTime(ft : TFileTime) : TDateTime;
function DateTimeToFileTime(dt : TDateTime) : TFileTime;

implementation

uses
  Base64
  {$IFDEF MSWINDOWS}
  , Windows
  {$ELSE}
  , UnixUtil
  {$ENDIF}
  ;

function ModeStr2Mode(const sMode: String): Integer;
begin
  Result:= 0;

  if Length(sMode) < 10 then Exit;

  if sMode[1] = 'd' then Result:= Result or S_IFDIR;
  if sMode[1] = 'l' then Result:= Result or S_IFLNK;
  if sMode[1] = 's' then Result:= Result or S_IFSOCK;
  if sMode[1] = 'f' then Result:= Result or S_IFIFO;
  if sMode[1] = 'b' then Result:= Result or S_IFBLK;
  if sMode[1] = 'c' then Result:= Result or S_IFCHR;

  if sMode[2] = 'r' then Result:= Result or S_IRUSR;
  if sMode[3] = 'w' then Result:= Result or S_IWUSR;
  if sMode[4] = 'x' then Result:= Result or S_IXUSR;
  if sMode[5] = 'r' then Result:= Result or S_IRGRP;
  if sMode[6] = 'w' then Result:= Result or S_IWGRP;
  if sMode[7] = 'x' then Result:= Result or S_IXGRP;
  if sMode[8] = 'r' then Result:= Result or S_IROTH;
  if sMode[9] = 'w' then Result:= Result or S_IWOTH;
  if sMode[10] = 'x' then Result:= Result or S_IXOTH;

  if sMode[4] = 's' then Result:= Result or S_ISUID;
  if sMode[7] = 's' then Result:= Result or S_ISGID;
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

function ExtractConnectionHost(Connection: AnsiString): AnsiString;
var
  I: Integer;
begin
  if Pos(ftpProtocol, LowerCase(Connection)) <> 0 then
    Delete(Connection, 1, 6);
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
  if Pos(ftpProtocol, LowerCase(Connection)) <> 0 then
    Delete(Connection, 1, 6);
  I:= Pos(':', Connection);
  if I > 0 then
    begin
      J:= Pos('/', Connection);
      if J = 0 then J:= MaxInt;
      Result:= Trim(Copy(Connection, I + 1, J));
    end;
end;

function FileTimeToLocalFileTimeEx(const lpFileTime: TFileTime; var lpLocalFileTime: TFileTime): LongBool;
{$IFDEF MSWINDOWS}
begin
  Result := FileTimeToLocalFileTime(lpFileTime, lpLocalFileTime);
end;
{$ELSE}
begin
  Int64(lpLocalFileTime) := Int64(lpFileTime) + 10000000 * Int64(TZSeconds);
  Result := True;
end;
{$ENDIF}

function LocalFileTimeToFileTimeEx(const lpLocalFileTime: TFileTime; var lpFileTime: TFileTime): LongBool;
{$IFDEF MSWINDOWS}
begin
  Result := LocalFileTimeToFileTime(lpLocalFileTime, lpFileTime);
end;
{$ELSE}
begin
  Int64(lpFileTime) := Int64(lpLocalFileTime) - 10000000 * Int64(TZSeconds);
  Result := True;
end;
{$ENDIF}

function FileTimeToDateTime(ft : TFileTime) : TDateTime;
begin
  FileTimeToLocalFileTimeEx(ft,ft);
  Result := (Int64(ft) / 864000000000.0) - 109205.0;
end;

function DateTimeToFileTime(dt : TDateTime) : TFileTime;
begin
  Int64(Result) := Round((dt + 109205.0) * 864000000000.0);
  LocalFileTimeToFileTimeEx(Result, Result);
end;

end.

