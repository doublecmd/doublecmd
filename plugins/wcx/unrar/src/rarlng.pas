{
  Double Commander
  -------------------------------------------------------------------------
  Rar archiver plugin, language support

  Copyright (C) 2023 Alexander Koblov (alexx2000@mail.ru)

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit RarLng;

{$mode delphi}

interface

uses
  Classes, SysUtils;

resourcestring
  rsMsgButtonCancel = '&Cancel';
  rsMsgButtonExtract = '&Extract';
  rsDictLargeWarning = 'Large dictionary warning';
  rsMsgPasswordEnter = 'Please enter the password:';
  rsDictNotAllowed = '%u GB dictionary exceeds %u GB limit and needs more than %u GB memory to unpack.';
  rsMsgLibraryNotFound = 'Cannot load library %s! Please check your installation.';
  rsMsgExecutableNotFound = 'Cannot find RAR executable!'#10#10'%s'#10#10'Please check the plugin settings.';

  procedure TranslateResourceStrings;

implementation

uses
  UnRARFunc;

function Translate(Name, Value: AnsiString; Hash: LongInt; Arg: Pointer): AnsiString;
var
  ALen: Integer;
begin
  with gStartupInfo do
  begin
    SetLength(Result, MaxSmallint);
    ALen:= TranslateString(Translation, PAnsiChar(Name), PAnsiChar(Value), PAnsiChar(Result), MaxSmallint);
    SetLength(Result, ALen);
  end;
end;

procedure TranslateResourceStrings;
begin
  if Assigned(gStartupInfo.Translation) then
  begin
    SetResourceStrings(@Translate, nil);
  end;
end;

end.

