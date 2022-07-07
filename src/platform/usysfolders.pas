{
    Double Commander
    -------------------------------------------------------------------------
    Get system folders.

    Copyright (C) 2006-2022 Alexander Koblov (alexx2000@mail.ru)

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

unit uSysFolders;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

{en
   Get the user home directory
   @returns(The user home directory)
}
function GetHomeDir : String;
{en
   Get the appropriate directory for the application's configuration files
   @returns(The directory for the application's configuration files)
}
function GetAppConfigDir: String;
{en
   Get the appropriate directory for the application's cache files
   @returns(The directory for the application's cache files)
}
function GetAppCacheDir: String;
{en
   Get the appropriate directory for the application's data files
   @returns(The directory for the application's data files)
}
function GetAppDataDir: String;

implementation

uses
  DCOSUtils, DCStrUtils, DCConvertEncoding, LazUTF8
{$IF DEFINED(MSWINDOWS)}
  , Windows, ShlObj, DCWindows
{$ENDIF}
{$IF DEFINED(UNIX)}
  , BaseUnix, Unix, DCUnix
  {$IF DEFINED(DARWIN)}
  , CocoaAll, uMyDarwin
  {$ELSE}
  , uXdg
  {$ENDIF}
{$ENDIF}
  ;

function GetHomeDir : String;
{$IFDEF MSWINDOWS}
begin
  Result:= ExcludeBackPathDelimiter(mbGetEnvironmentVariable('USERPROFILE'));
end;
{$ELSE}
begin
  Result:= ExcludeBackPathDelimiter(SysToUTF8(GetEnvironmentVariable('HOME')));
end;
{$ENDIF}

function GetAppConfigDir: String;
{$IF DEFINED(MSWINDOWS)}
const
  SHGFP_TYPE_CURRENT = 0;
var
  wPath: array[0..MAX_PATH-1] of WideChar;
  wUser: UnicodeString;
  dwLength: DWORD;
begin
  if SUCCEEDED(SHGetFolderPathW(0, CSIDL_APPDATA or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT, @wPath[0])) or
     SUCCEEDED(SHGetFolderPathW(0, CSIDL_LOCAL_APPDATA or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT, @wPath[0])) then
  begin
    Result := UTF16ToUTF8(UnicodeString(wPath));
  end
  else
  begin
    dwLength := UNLEN + 1;
    SetLength(wUser, dwLength);
    if GetUserNameW(PWideChar(wUser), @dwLength) then
    begin
      SetLength(wUser, dwLength - 1);
      Result := GetTempDir + UTF16ToUTF8(wUser);
    end
    else
      Result := EmptyStr;
  end;
  if Result <> '' then
    Result := Result + DirectorySeparator + ApplicationName;
end;
{$ELSEIF DEFINED(DARWIN)}
begin
  Result:= GetHomeDir + '/Library/Preferences/' + ApplicationName;
end;
{$ELSE}
var
  uinfo: PPasswordRecord;
begin
  uinfo:= getpwuid(fpGetUID);
  if (uinfo <> nil) and (uinfo^.pw_dir <> '') then
    Result:= CeSysToUtf8(uinfo^.pw_dir) + '/.config/' + ApplicationName
  else
    Result:= ExcludeTrailingPathDelimiter(SysToUTF8(SysUtils.GetAppConfigDir(False)));
end;
{$ENDIF}

function GetAppCacheDir: String;
{$IF DEFINED(MSWINDOWS)}
var
  APath: array[0..MAX_PATH] of WideChar;
begin
  if SHGetSpecialFolderPathW(0, APath, CSIDL_LOCAL_APPDATA, True) then
    Result:= UTF16ToUTF8(UnicodeString(APath)) + DirectorySeparator + ApplicationName
  else
    Result:= GetAppConfigDir;
end;
{$ELSEIF DEFINED(DARWIN)}
begin
  Result:= NSGetFolderPath(NSCachesDirectory);
end;
{$ELSE}
var
  uinfo: PPasswordRecord;
begin
  uinfo:= getpwuid(fpGetUID);
  if (uinfo <> nil) and (uinfo^.pw_dir <> '') then
    Result:= CeSysToUtf8(uinfo^.pw_dir) + '/.cache/' + ApplicationName
  else
    Result:= GetHomeDir + '/.cache/' + ApplicationName;
end;
{$ENDIF}

function GetAppDataDir: String;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= GetAppCacheDir;
end;
{$ELSEIF DEFINED(DARWIN)}
begin
  Result:= NSGetFolderPath(NSApplicationSupportDirectory);
end;
{$ELSE}
begin
  Result:= IncludeTrailingPathDelimiter(GetUserDataDir) + ApplicationName;
end;
{$ENDIF}

end.

