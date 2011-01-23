{
   Double Commander
   -------------------------------------------------------------------------
   Version information about DC, building tools and running environment.

   Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)
   Copyright (C) 2010       Przemyslaw Nagay (cobines@gmail.com)

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

unit uDCVersion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLVersion;

{$I dcrevision.inc} // Double Commander revision number
{$I revision.inc} // Lazarus revision number

const
  dcVersion   = '0.4.6 alpha';
  dcBuildDate = {$I %DATE%};
  lazVersion  = lcl_version;         // Lazarus version (major.minor.micro)
  lazRevision = RevisionStr;         // Lazarus SVN revision
  fpcVersion  = {$I %FPCVERSION%};   // FPC version (major.minor.micro)
  TargetCPU   = {$I %FPCTARGETCPU%}; // Target CPU of FPC
  TargetOS    = {$I %FPCTARGETOS%};  // Target Operating System of FPC

var
  TargetWS,    // Target WidgetSet of Lazarus
  OSVersion,   // Operating System where DC is run
  WSVersion    // WidgetSet library version where DC is run
  : String;

procedure InitializeVersionInfo;

implementation

uses
  InterfaceBase
  {$IF DEFINED(UNIX)}
  , uOSUtils, uDCUtils, uClassesEx
  {$ENDIF}
  {$IFDEF LCLQT}
  , qt4
  {$ENDIF}
  {$IFDEF LCLGTK2}
  , gtk2
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  ;

const
  LCLPlatform: array[TLCLPlatform] of string = (
      'gtk1',
      'gtk2',
      'win32/win64',
      'wince',
      'carbon',
      'qt4',
      'fpGUI',
      'NoGUI',
      'cocoa'
{$if (lcl_release) > 30}
      , 'android'
{$endif}
    );

{$IF DEFINED(UNIX)}
{en
   Reads file into strings.
   Returns @false if file not found or cannot be read.
}
function GetStringsFromFile(FileName: UTF8String; out sl: TStringListEx): Boolean;
begin
  Result := False;
  sl := nil;
  if mbFileAccess(FileName, fmOpenRead) then
  begin
    sl := TStringListEx.Create;
    try
      sl.LoadFromFile(FileName);
      Result := True;
    except
      on EFilerError do; // Bypass
    end;
  end;
end;

{en
   Reads first line of file into a string.
   Returns @false if file not found or cannot be read.
}
function GetStringFromFile(FileName: UTF8String; out str: String): Boolean;
var
  sl: TStringListEx;
begin
  str := EmptyStr;
  Result := GetStringsFromFile(FileName, sl);
  if Result then
  try
    str := sl.Strings[0];
  finally
    sl.Free;
  end;
end;

function GetOsFromLsbRelease: String;
var
  sl: TStringListEx;
begin
  Result := EmptyStr;

  if GetStringsFromFile('/etc/lsb-release', sl) then
  try
    Result := sl.Values['DISTRIB_DESCRIPTION'];

    if Result <> EmptyStr then
      Result := TrimQuotes(Result)
    else
      Result := sl.Values['DISTRIB_ID'] +
                sl.Values['DISTRIB_RELEASE'] +
                sl.Values['DISTRIB_CODENAME'];
  finally
    sl.Free;
  end;
end;

function GetOsFromProcVersion: String;
var
  i: Integer;
  s: String;
begin
  Result := EmptyStr;

  if GetStringFromFile('/proc/version', s) then
  begin
    // Get first three strings separated by space.

    i := Pos(' ', s);
    if i > 0 then
      Result := Result + Copy(s, 1, i);
    Delete(s, 1, i);

    i := Pos(' ', s);
    if i > 0 then
      Result := Result + Copy(s, 1, i);
    Delete(s, 1, i);

    i := Pos(' ', s);
    if i > 0 then
      Result := Result + Copy(s, 1, i - 1);
    Delete(s, 1, i);
  end;
end;

function GetOsFromIssue: String;
begin
  if not GetStringFromFile('/etc/issue', Result) then
    Result := EmptyStr;
end;

function GetDebianVersion: String;
var
  s: String;
begin
  if GetStringFromFile('/etc/debian_version', s) then
  begin
    Result := 'Debian';
    if s <> EmptyStr then
      Result := Result + ' ' + s;
  end
  else
    Result := EmptyStr;
end;

function GetSuseVersion: String;
begin
  if GetStringFromFile('/etc/SuSE-release', Result) or
     GetStringFromFile('/etc/suse-release', Result) then
  begin
    if Result = EmptyStr then
      Result := 'Suse';
  end
  else
    Result := EmptyStr;
end;

function GetRedHatVersion: String;
begin
  if GetStringFromFile('/etc/redhat-release', Result) then
  begin
    if Result = EmptyStr then
      Result := 'RedHat';
  end
  else
    Result := EmptyStr;
end;

function GetMandrakeVersion: String;
begin
  if GetStringFromFile('/etc/mandrake-release', Result) then
  begin
    if Result = EmptyStr then
      Result := 'Mandrake';
  end
  else
    Result := EmptyStr;
end;
{$ENDIF}

procedure InitializeVersionInfo;
{$IFDEF LCLQT}
const
  qtLibQt4PasRev = '23858';
{$ENDIF}
{$IF DEFINED(MSWINDOWS)}
type
  OSVERSIONINFOEX = record
      dwOSVersionInfoSize : DWORD;
      dwMajorVersion : DWORD;
      dwMinorVersion : DWORD;
      dwBuildNumber : DWORD;
      dwPlatformId : DWORD;
      szCSDVersion : array[0..127] of Char;
      wServicePackMajor : WORD;
      wServicePackMinor : WORD;
      wSuiteMask : WORD;
      wProductType : BYTE;
      wReserved : BYTE;
   end;
const
  VER_NT_WORKSTATION       = $0000001;
  VER_NT_SERVER            = $0000003;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
var
  osvi: OSVERSIONINFOEX;
  si: SYSTEM_INFO;
{$ENDIF}
begin
  TargetWS := LCLPlatform[WidgetSet.LCLPlatform];

  {$IF DEFINED(MSWINDOWS)}
  OSVersion := 'Windows';

  ZeroMemory(@osvi, SizeOf(OSVERSIONINFOEX));
  osvi.dwOSVersionInfoSize := SizeOf(OSVERSIONINFOEX);

  if GetVersionEx(@osvi) then
  begin
    GetSystemInfo(si);

    case osvi.dwPlatformId of
      VER_PLATFORM_WIN32_WINDOWS:
        case osvi.dwMajorVersion of
          4: case osvi.dwMinorVersion of
                0: OSVersion := OSVersion + ' 95';
               10: OSVersion := OSVersion + ' 98';
               90: OSVersion := OSVersion + ' ME';
             end;
        end;

      VER_PLATFORM_WIN32_NT:
        begin
          case osvi.dwMajorVersion of
            3: OSVersion := OSVersion + ' NT 3.5';
            4: OSVersion := OSVersion + ' NT 4';
            5: case osvi.dwMinorVersion of
                 0: OSVersion := OSVersion + ' 2000';
                 1: begin
                      OSVersion := OSVersion + ' XP';
                      if osvi.wSuiteMask = $0000 then
                        OSVersion := OSVersion + ' Home'
                      else if osvi.wSuiteMask = $0200 then
                        OSVersion := OSVersion + ' Professional';
                    end;
                 2: if (osvi.wProductType = VER_NT_WORKSTATION) and
                       (si.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
                    begin
                      OSVersion := OSVersion + ' XP Professional x64'
                    end
                    else if (osvi.wProductType = VER_NT_SERVER) then
                    begin
                      if osvi.wSuiteMask = $8000 then
                        OSVersion := OSVersion + ' Home Server'
                      else
                        OSVersion := OSVersion + ' Server 2003';
                    end;
               end;
            6: case osvi.dwMinorVersion of
                 0: if (osvi.wProductType = VER_NT_WORKSTATION) then
                    begin
                      OSVersion := OSVersion + ' Vista';
                      if osvi.wSuiteMask = $0000 then
                        OSVersion := OSVersion + ' Ultimate'
                      else if osvi.wSuiteMask = $0200 then
                        OSVersion := OSVersion + ' Home';
                    end
                    else if (osvi.wProductType = VER_NT_SERVER) then
                      OSVersion := OSVersion + ' Server 2008';
                 1: if (osvi.wProductType = VER_NT_WORKSTATION) then
                      OSVersion := OSVersion + ' 7'
                    else if (osvi.wProductType = VER_NT_SERVER) then
                      OSVersion := OSVersion + ' Server 2008 R2';
               end;
          end;
        end;
    end;

    // If something detected then add service pack number.
    if OSVersion <> 'Windows' then
    begin
      if osvi.wServicePackMajor > 0 then
      begin
        OSVersion := OSVersion + ' SP' + IntToStr(osvi.wServicePackMajor);
        if osvi.wServicePackMinor > 0 then
          OSVersion := OSVersion + '.' + IntToStr(osvi.wServicePackMinor);
      end;
    end
    else
      OSVersion := OSVersion + ' Build ' + IntToStr(osvi.dwBuildNumber);
  end;
  {$ELSEIF DEFINED(UNIX)}
  // Try using linux standard base.
  OSVersion := GetOsFromLsbRelease;

  // Try some distribution-specific files.
  if OSVersion = EmptyStr then
    OSVersion := GetDebianVersion;
  if OSVersion = EmptyStr then
    OSVersion := GetRedHatVersion;
  if OSVersion = EmptyStr then
    OSVersion := GetSuseVersion;
  if OSVersion = EmptyStr then
    OSVersion := GetMandrakeVersion;

  // Other methods.
  if OSVersion = EmptyStr then
    OSVersion := GetOsFromIssue;
  if OSVersion = EmptyStr then
    OSVersion := GetOsFromProcVersion;

  // Set default names.
  if OSVersion = EmptyStr then
    {$IF DEFINED(LINUX)}
    OSVersion := 'Linux';
    {$ELSEIF DEFINED(DARWIN)}
    OSVersion := 'Darwin';  // MacOS
    {$ELSEIF DEFINED(BSD)}
    OSVersion := 'BSD';
    {$ELSE}
    OSVersion := 'Unix';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF LCLQT}
  {$IF RevisionStr >= qtLibQt4PasRev}
  // Only since Lazarus 0.9.29 QT bindings v2.1.
  WSVersion := 'QT ' + QtVersion + ', libQt4Pas ';
  {$ELSE}
  WSVersion := 'libqt4intf ';
  {$ENDIF}

  WSVersion := WSVersion + IntToStr((QT_VERSION shr 16) and 255) + '.' +
                           IntToStr((QT_VERSION shr  8) and 255) + '.' +
                           IntToStr((QT_VERSION       ) and 255);
  {$ENDIF}

  {$IFDEF LCLGTK2}
  WSVersion := 'GTK ' + IntToStr(gtk_major_version) + '.' +
                        IntToStr(gtk_minor_version) + '.' +
                        IntToStr(gtk_micro_version);
  {$ENDIF}
end;

end.

