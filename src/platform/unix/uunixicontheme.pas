{
    Double Commander
    -------------------------------------------------------------------------
    Some useful functions for Unix icon theme implementation

    Copyright (C) 2009-2023  Alexander Koblov (alexx2000@mail.ru)

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

unit uUnixIconTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes, uIconTheme;

const
  DEFAULT_THEME_NAME = 'hicolor';

function GetCurrentIconTheme: String;
function GetUnixDefaultTheme: String;
function GetUnixIconThemeBaseDirList: TDynamicStringArray;

implementation

uses
  Laz2_DOM, Laz2_XMLRead, DCClassesUtf8, uMyUnix, DCOSUtils, uOSUtils, uGio,
  uSysFolders, uXdg
{$IF DEFINED(LCLQT5)}
  , Qt5
{$ELSEIF DEFINED(LCLQT6)}
  , Qt6
{$ENDIF}
  ;

{$IF DEFINED(LCLQT5) OR DEFINED(LCLQT6)}
function GetQtIconTheme: String;
var
  AValue: WideString;
begin
  QIcon_themeName(@AValue);
  Result:= UTF8Encode(AValue);
end;
{$ENDIF}

function GetKdeIconTheme: String;
var
  I: Integer;
  FileName: String;
  iniCfg: TIniFileEx;
  kdeConfig: array[1..3] of String =
  (
    'kdeglobals',
    '/.kde/share/config/kdeglobals',
    '/.kde4/share/config/kdeglobals'
  );
begin
  Result:= EmptyStr;
  for I:= Low(kdeConfig) to High(kdeConfig) do
  begin
    if (I > 1) then
      FileName:= GetHomeDir + kdeConfig[I]
    else begin
      FileName:= IncludeTrailingBackslash(GetUserConfigDir) + kdeConfig[I];
    end;
    if mbFileExists(FileName) then
    try
      iniCfg:= TIniFileEx.Create(FileName, fmOpenRead);
      try
        Result:= iniCfg.ReadString('Icons', 'Theme', EmptyStr);
        if (Length(Result) > 0) then Break;
      finally
        iniCfg.Free;
      end;
    except
      // Skip
    end;
  end;
  if Length(Result) = 0 then
    Result:= 'breeze';
end;

function GetGnomeIconTheme: String;
begin
  Result:= GioGetIconTheme('org.gnome.desktop.interface');
  if Length(Result) = 0 then
    Result:= 'Adwaita';
end;

function GetXfceIconTheme: String;
const
  xfceConfig = '/.config/xfce4/xfconf/xfce-perchannel-xml/xsettings.xml';
var
  J, I: Integer;
  ChildNode1,
  ChildNode2: TDOMNode;
  xmlCfg: TXMLDocument = nil;
begin
  Result:= EmptyStr;
  if mbFileExists(GetHomeDir + xfceConfig) then
  try
    ReadXMLFile(xmlCfg, GetHomeDir + xfceConfig);
    try
      for J := 0 to xmlCfg.DocumentElement.ChildNodes.Count -1 do
        begin
          ChildNode1:= xmlCfg.DocumentElement.ChildNodes.Item[J];
          if (ChildNode1.NodeName = 'property') then
            if (ChildNode1.Attributes.Length > 0) and (ChildNode1.Attributes[0].NodeValue = 'Net') then
              for I:= 0 to ChildNode1.ChildNodes.Count - 1 do
                begin
                  ChildNode2 := ChildNode1.ChildNodes.Item[I];
                  if (ChildNode2.NodeName = 'property') then
                    if (ChildNode2.Attributes.Length > 2) and (ChildNode2.Attributes[0].NodeValue = 'IconThemeName') then
                      begin
                        Result:= ChildNode2.Attributes[2].NodeValue;
                        Exit;
                      end;
                end;
        end;
    finally
      xmlCfg.Free;
    end;
  except
    // Skip
  end;
end;

function GetLxdeIconTheme: String;
const
  lxdeConfig1 = '/.config/lxsession/%s/desktop.conf';
  lxdeConfig2 = '/etc/xdg/lxsession/%s/desktop.conf';
var
  I: Integer;
  DesktopSession: String;
  iniCfg: TIniFileEx = nil;
  lxdeConfig: array[1..2] of String = (lxdeConfig1, lxdeConfig2);
begin
  Result:= EmptyStr;
  DesktopSession:= mbGetEnvironmentVariable('DESKTOP_SESSION');
  if Length(DesktopSession) <> 0 then
  begin
    lxdeConfig[1]:= GetHomeDir + Format(lxdeConfig[1], [DesktopSession]);
    lxdeConfig[2]:= Format(lxdeConfig[2], [DesktopSession]);
    for I:= Low(lxdeConfig) to High(lxdeConfig) do
    begin
      if (Length(Result) = 0) and mbFileExists(lxdeConfig[I]) then
      try
        iniCfg:= TIniFileEx.Create(lxdeConfig[I]);
        try
          Result:= iniCfg.ReadString('GTK', 'sNet/IconThemeName', EmptyStr);
        finally
          iniCfg.Free;
        end;
      except
        // Skip
      end;
    end;
  end;
end;

function GetLxqtIconTheme: String;
const
  lxqtConfig = '/.config/lxqt/lxqt.conf';
var
  iniCfg: TIniFileEx;
begin
  Result:= EmptyStr;

  if mbFileExists(GetHomeDir + lxqtConfig) then
  try
    iniCfg:= TIniFileEx.Create(GetHomeDir + lxqtConfig);
    try
      Result:= iniCfg.ReadString('General', 'icon_theme', EmptyStr);
    finally
      iniCfg.Free;
    end;
  except
    // Skip
  end;
end;

function GetMateIconTheme: String; inline;
begin
  Result:= GioGetIconTheme('org.mate.interface');
end;

function GetCinnamonIconTheme: String; inline;
begin
  Result:= GioGetIconTheme('org.cinnamon.desktop.interface');
end;

function GetCurrentIconTheme: String;
begin
  Result:= EmptyStr;
  case DesktopEnv of
    DE_KDE:
      Result:= GetKdeIconTheme;
    DE_GNOME:
      Result:= GetGnomeIconTheme;
    DE_XFCE:
      Result:= GetXfceIconTheme;
    DE_LXDE:
      Result:= GetLxdeIconTheme;
    DE_LXQT:
      Result:= GetLxqtIconTheme;
    DE_MATE:
      Result:= GetMateIconTheme;
    DE_CINNAMON:
      Result:= GetCinnamonIconTheme;
  end;
  if Result = EmptyStr then
  begin
{$IF DEFINED(LCLQT5) OR DEFINED(LCLQT6)}
    Result:= GetQtIconTheme;
    if Result = EmptyStr then
{$ENDIF}
    Result:= DEFAULT_THEME_NAME;
  end;
end;

var
  UnixDefaultTheme: String = DEFAULT_THEME_NAME;
  UnixIconThemesBaseDirList: TDynamicStringArray;

function GetUnixDefaultTheme: String;
begin
  Result:= UnixDefaultTheme;
end;

function GetUnixIconThemeBaseDirList: TDynamicStringArray;
begin
  Result:= UnixIconThemesBaseDirList;
end;

procedure InitIconThemesBaseDirList;
var
  Home: String;
  I: Integer = 1;
begin
  Home := GetHomeDir;
  SetLength(UnixIconThemesBaseDirList, 6);
  UnixIconThemesBaseDirList[0] := Home + '/.icons';
  UnixIconThemesBaseDirList[1] := Home + '/.local/share/icons';
  if DesktopEnv = DE_KDE then
  begin
    I:= 2;
    SetLength(UnixIconThemesBaseDirList, 7);
    UnixIconThemesBaseDirList[2] := Home + '/.kde/share/icons';
  end;
  UnixIconThemesBaseDirList[I + 1] := '/usr/local/share/icons';
  UnixIconThemesBaseDirList[I + 2] := '/usr/local/share/pixmaps';
  UnixIconThemesBaseDirList[I + 3] := '/usr/share/icons';
  UnixIconThemesBaseDirList[I + 4] := '/usr/share/pixmaps';
end;

initialization
  InitIconThemesBaseDirList;

end.

