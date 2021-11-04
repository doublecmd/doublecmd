{
   Double commander
   -------------------------------------------------------------------------
   Auto-detect default terminal emulator

   Copyright (C) 2021 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uDefaultTerminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  DCOSUtils, DCClassesUtf8, uMyUnix, uGio, uOSUtils;

const
  TERM_KDE = 'konsole';
  TERM_LXQT = 'qterminal';
  TERM_LXDE = 'lxterminal';
  TERM_MATE = 'mate-terminal';
  TERM_XFCE = 'xfce4-terminal';
  TERM_GNOME = 'gnome-terminal';
  TERM_DEBIAN = 'x-terminal-emulator';

function GetPathTerminal(const Exe: String; var Cmd: String): Boolean;
begin
  Result:= ExecutableInSystemPath(Exe);
  if Result then Cmd:= Exe;
end;

function GetKdeTerminal(var Cmd, Params: String): Boolean;
const
  kde5Config = '/.kde/share/config/kdeglobals';
  kde4Config = '/.kde4/share/config/kdeglobals';
var
  I: Integer;
  S: String = '';
  iniCfg: TIniFileEx = nil;
  kdeConfig: array[1..2] of String = (kde4Config, kde5Config);
begin
  for I:= Low(kdeConfig) to High(kdeConfig) do
  begin
    if (Length(S) = 0) and mbFileExists(GetHomeDir + kdeConfig[I]) then
    try
      iniCfg:= TIniFileEx.Create(GetHomeDir + kdeConfig[I]);
      try
        S:= iniCfg.ReadString('General', 'TerminalApplication', EmptyStr);
      finally
        iniCfg.Free;
      end;
    except
      // Skip
    end;
  end;
  Params:= EmptyStr;
  Result:= Length(S) > 0;
  if not Result then begin
    Result:= GetPathTerminal(TERM_KDE, S);
  end;
  if Result then Cmd:= S;
end;

function GetXfceTerminal(var Cmd, Params: String): Boolean;
const
  xfceConfig = '/.config/xfce4/helpers.rc';
var
  S: String = '';
  FileName: String;
begin
  FileName:= GetHomeDir + xfceConfig;
  if mbFileExists(FileName) then
  begin
    with TStringListEx.Create do
    try
      try
        LoadFromFile(FileName);
        S:= Values['TerminalEmulator'];
      except
        // Skip
      end;
    finally
      Free;
    end;
  end;
  Result:= (Length(S) > 0);
  if not Result then begin
    Result:= GetPathTerminal(TERM_XFCE, S);
  end;
  if (S = TERM_XFCE) then
  begin
    Params:= '-x';
  end;
  if Result then Cmd:= S;
end;

function GetLxdeTerminal(var Cmd, Params: String): Boolean;
begin
  Params:= EmptyStr;
  Result:= GetPathTerminal(TERM_LXDE, Cmd);
end;

function GetLxqtTerminal(var Cmd, Params: String): Boolean;
begin
  Params:= EmptyStr;
  Result:= GetPathTerminal(TERM_LXQT, Cmd);
end;

function GetGioTerminal(const Scheme: String; var Cmd, Params: String): Boolean;
begin
  Cmd:= GioGetSetting(Scheme, 'exec');
  Params:= GioGetSetting(Scheme, 'exec-arg');
  Result:= Length(Cmd) > 0;
end;

function GetGnomeTerminal(var Cmd, Params: String): Boolean;
begin
  Result:= GetGioTerminal('org.gnome.desktop.default-applications.terminal', Cmd, Params);
  if not Result then
  begin
    Params:= '-x';
    Result:= GetPathTerminal(TERM_GNOME, Cmd);
  end;
end;

function GetMateTerminal(var Cmd, Params: String): Boolean;
begin
  Result:= GetGioTerminal('org.mate.applications-terminal', Cmd, Params);
  if not Result then
  begin
    Params:= '-x';
    Result:= GetPathTerminal(TERM_MATE, Cmd);
  end;
end;

function GetCinnamonTerminal(var Cmd, Params: String): Boolean;
begin
  Result:= GetGioTerminal('org.cinnamon.desktop.default-applications.terminal', Cmd, Params);
  if not Result then
  begin
    Params:= '-x';
    Result:= GetPathTerminal(TERM_GNOME, Cmd);
  end;
end;

function GetDefaultTerminal(var Cmd, Params: String): Boolean;
begin
  if mbFileExists('/etc/debian_version') then
  begin
    Cmd:= TERM_DEBIAN;
    Exit(True);
  end;
  case DesktopEnv of
    DE_UNKNOWN:
      Result:= False;
    DE_KDE:
      Result:= GetKdeTerminal(Cmd, Params);
    DE_XFCE:
      Result:= GetXfceTerminal(Cmd, Params);
    DE_LXDE:
      Result:=  GetLxdeTerminal(Cmd, Params);
    DE_LXQT:
      Result:=  GetLxqtTerminal(Cmd, Params);
    DE_MATE:
      Result:=  GetMateTerminal(Cmd, Params);
    DE_GNOME:
      Result:=  GetGnomeTerminal(Cmd, Params);
    DE_CINNAMON:
      Result:=  GetCinnamonTerminal(Cmd, Params);
  end;
end;

procedure Initialize;
var
  Cmd: String = '';
  Params: String = '';
begin
  if GetDefaultTerminal(Cmd, Params) then
  begin
    RunTermCmd:= Cmd;
    RunInTermCloseCmd:= Cmd;
    RunInTermStayOpenCmd:= Cmd;
    if (Length(Params) > 0) then
    begin
      RunInTermCloseParams:= StringReplace(RunInTermCloseParams, '-e', Params, []);
      RunInTermStayOpenParams:= StringReplace(RunInTermStayOpenParams, '-e', Params, []);
    end;
  end;
end;

initialization
  Initialize;

end.

