{
   Double Commander
   -------------------------------------------------------------------------
   Miscellaneous freedesktop.org compatible utility functions

   Copyright (C) 2014 Alexander Koblov (alexx2000@mail.ru)

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

unit uXdg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes;

{en
   Returns a base directory relative to which user-specific data
   files should be written.
}
function GetUserDataDir: UTF8String;
{en
   Returns an ordered list of base directories in which to access
   system-wide application data.
}
function GetSystemDataDirs: TDynamicStringArray;
{en
   Returns a base directory in which to store user-specific application
   configuration information such as user preferences and settings.
}
function GetUserConfigDir: UTF8String;
{en
  Returns an ordered list of base directories in which to access
  system-wide configuration information.
}
function GetSystemConfigDirs: TDynamicStringArray;
{en
   Get desktop file path by desktop base file name.
}
function GetDesktopPath(const DesktopName: UTF8String): UTF8String;

implementation

uses
  DCStrUtils, DCOSUtils, uOSUtils;

function GetUserDataDir: UTF8String;
begin
  Result:= mbGetEnvironmentVariable('XDG_DATA_HOME');
  if Length(Result) = 0 then begin
    Result:= GetHomeDir + '/.local/share';
  end;
end;

function GetSystemDataDirs: TDynamicStringArray;
var
  Value: String;
begin
  Value:= mbGetEnvironmentVariable('XDG_DATA_DIRS');
  if Length(Value) = 0 then
  begin
    Value:= '/usr/local/share/:/usr/share/';
  end;
  Result:= SplitString(Value, PathSeparator);
end;

function GetUserConfigDir: UTF8String;
begin
  Result:= mbGetEnvironmentVariable('XDG_CONFIG_HOME');
  if Length(Result) = 0 then begin
    Result:= GetHomeDir + '/.config';
  end;
end;

function GetSystemConfigDirs: TDynamicStringArray;
var
  Value: String;
begin
  Value:= mbGetEnvironmentVariable('XDG_CONFIG_DIRS');
  if Length(Value) = 0 then
  begin
    Value:= '/etc/xdg';
  end;
  Result:= SplitString(Value, PathSeparator);
end;

function GetDesktopPath(const DesktopName: UTF8String): UTF8String;
const
  PrefixDelim = '-';
var
  Index: Integer;
  HasPrefix: Boolean;
  FileName: UTF8String;
  Path: TDynamicStringArray;

  function DesktopExists(var DesktopPath: UTF8String): Boolean;
  var
    Prefix: PAnsiChar;
  begin
    if mbFileExists(DesktopPath) then
      Exit(True);
    if HasPrefix then
    begin
      Prefix := PAnsiChar(DesktopPath);
      Prefix := strrscan(Prefix, PathDelim);
      Prefix := strscan(Prefix, PrefixDelim);
      while (Prefix <> nil) do
      begin
        Prefix^:= PathDelim;
        if mbFileExists(DesktopPath) then Exit(True);
        Prefix := strscan(Prefix, PrefixDelim);
      end;
    end;
    Result:= False;
  end;

begin
  HasPrefix:= (Pos(PrefixDelim, DesktopName) > 0);
  FileName:= 'applications' + PathDelim + DesktopName;
  // Find in user data directory
  Result:= IncludeTrailingBackslash(GetUserDataDir) + FileName;
  if DesktopExists(Result) then Exit;
  // Find in system data directories
  Path:= GetSystemDataDirs;
  for Index:= Low(Path) to High(Path) do
  begin
    Result:= IncludeTrailingBackslash(Path[Index]) + FileName;
    if DesktopExists(Result) then Exit;
  end;
  Result:= EmptyStr;
end;

end.

