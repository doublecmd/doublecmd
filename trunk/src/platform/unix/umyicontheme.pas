unit uMyIconTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function GetCurrentIconTheme: String;

implementation

uses
  DOM, XMLRead, IniFiles, uIconTheme, uMyUnix, uOSUtils;

function GetKdeIconTheme: String;
const
  kde3Config = '/.kde/share/config/kdeglobals';
  kde4Config = '/.kde4/share/config/kdeglobals';
var
  kdeConfig: array[1..2] of String = (kde4Config, kde3Config);
  I: Integer;
  iniCfg: TIniFile;
begin
  Result:= EmptyStr;
  iniCfg:= nil;
  for I:= Low(kdeConfig) to High(kdeConfig) do
    if (Result = EmptyStr) and FileExists(GetHomeDir + kdeConfig[I]) then
      try
        iniCfg:= TIniFile.Create(GetHomeDir + kdeConfig[I]);
        Result:= iniCfg.ReadString('Icons', 'Theme', EmptyStr);
      finally
        if Assigned(iniCfg) then
          iniCfg.Free;
      end;
  if (Result = EmptyStr) and DirectoryExists('/usr/share/icons/default.kde4') then
    Result:= 'default.kde4'
  else if (Result = EmptyStr) and DirectoryExists('/usr/share/icons/default.kde') then
    Result:= 'default.kde';
end;

function GetGnomeIconTheme: String;
const
  gnomeConfig = '/.gconf/desktop/gnome/interface/%gconf.xml';
var
  I: Integer;
  xmlCfg: TXMLDocument;
  ChildNode: TDOMNode;
begin
  Result:= EmptyStr;
  xmlCfg:= nil;
  if FileExists(GetHomeDir + gnomeConfig) then
    try
      xmlCfg:= TXMLDocument.Create;
      ReadXMLFile(xmlCfg, GetHomeDir + gnomeConfig);
      for I := 0 to xmlCfg.DocumentElement.ChildNodes.Count -1 do
        begin
          ChildNode := xmlCfg.DocumentElement.ChildNodes.Item[I];
          if (ChildNode.NodeName = 'entry') then
            if (ChildNode.Attributes.Length > 0) and (ChildNode.Attributes[0].NodeValue = 'icon_theme') then
              begin
                Result:= ChildNode.FirstChild.FirstChild.NodeValue;
                Break;
              end;
        end;
    finally
      xmlCfg.Free;
    end;
  if Result = EmptyStr then
    Result:= 'gnome';
end;

function GetXfceIconTheme: String;
const
  xfceConfig = '/.config/xfce4/xfconf/xfce-perchannel-xml/xsettings.xml';
var
  J, I: Integer;
  xmlCfg: TXMLDocument;
  ChildNode1,
  ChildNode2: TDOMNode;
begin
  Result:= EmptyStr;
  xmlCfg:= nil;
  if FileExists(GetHomeDir + xfceConfig) then
    try
      xmlCfg:= TXMLDocument.Create;
      ReadXMLFile(xmlCfg, GetHomeDir + xfceConfig);
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
                        Break;
                      end;
                end;
        end;
    finally
      xmlCfg.Free;
    end;
end;

function GetCurrentIconTheme: String;
begin
  Result:= EmptyStr;
  case GetDesktopEnvironment of
    DE_UNKNOWN:
      Result:= DEFAULT_THEME_NAME;
    DE_KDE:
      Result:= GetKdeIconTheme;
    DE_GNOME:
      Result:= GetGnomeIconTheme;
    DE_XFCE:
      Result:= GetXfceIconTheme;
  end;
  if Result = EmptyStr then
    Result:= DEFAULT_THEME_NAME;
end;

end.

