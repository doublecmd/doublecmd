{
    Double Commander
    -------------------------------------------------------------------------
    Simple implementation of Icon Theme based on FreeDesktop.org specification
    (http://standards.freedesktop.org/icon-theme-spec/icon-theme-spec-latest.html)

    Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

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

unit uIconTheme;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, IniFiles;

type
  TIconDirInfo = record
    IconSize: Integer;
    IconContext: String;
    IconType: String;
    IconMaxSize,
    IconMinSize,
    IconThreshold: Integer;
  end;
  PIconDirInfo = ^TIconDirInfo;

  { TIconDirList }

  TIconDirList = class (TStringList)
  private
    function GetIconDir(Index: Integer): TIconDirInfo;
  public
    destructor Destroy; override;
    function Add(IconDirName: String; IconDirInfo: PIconDirInfo): Integer;
    property Items[Index: Integer]: TIconDirInfo read GetIconDir;
  end;

  { TIconTheme }

  TIconTheme = class
    FIniFile: TIniFile;
    FTheme,
    FThemeName: String;
    FComment: UTF8String;
    FInherits: TStringList;
    FDirectories: TIconDirList;
    function LoadIconDirInfo(const sIconDirName: String): PIconDirInfo;
    function FindIconHelper(aIconName: String; AIconSize: Integer): UTF8String;
  protected
    function LookupIcon(AIconName: String; AIconSize: Integer): UTF8String;
  public
    constructor Create(sThemeName: String);
    destructor Destroy; override;
    function Load(AInherits: TStringList = nil): Boolean;
    function FindIcon(AIconName: String; AIconSize: Integer): UTF8String;
    function DirectoryMatchesSize(SubDirIndex: Integer; AIconSize: Integer): Boolean;
    function DirectorySizeDistance(SubDirIndex: Integer; AIconSize: Integer): Integer;
    property ThemeName: String read FThemeName;
    property Directories: TIconDirList read FDirectories;
  end;

const
  DEFAULT_THEME_NAME: String = 'hicolor';

var
  BaseNameList: array[0..4] of String = ('~/.icons',
                                         '/usr/local/share/icons',
                                         '/usr/local/share/pixmaps',
                                         '/usr/share/icons',
                                         '/usr/share/pixmaps'
                                        );
  IconExtensionList: array[0..1] of String = ('png', 'xpm');

implementation

uses
  LCLProc, StrUtils;

{ TIconTheme }

function LookupFallbackIcon (AIconName: String): UTF8String;
begin
{
  for each directory in $(basename list) {
    for extension in ("png", "svg", "xpm") {
      if exists directory/iconname.extension
        return directory/iconname.extension
    }
  }
  return none
}
end;


function TIconTheme.DirectoryMatchesSize(SubDirIndex: Integer; AIconSize: Integer): Boolean;
begin
  Result:= False;
  // read Type and Size data from subdir
  if SubDirIndex < 0 then Exit;
  with FDirectories.Items[SubDirIndex] do
  begin
    if IconType = 'Fixed' then
      Result:= (IconSize = AIconSize)
    else if IconType = 'Scaled' then
      Result:= (IconMinSize <= AIconSize) and (AIconSize <= IconMaxSize)
    else if IconType = 'Threshold' then
      Result:= ((IconSize - IconThreshold) <= AIconSize) and (AIconSize <= (IconSize + IconThreshold));
  end;
end;


function TIconTheme.DirectorySizeDistance(SubDirIndex: Integer; AIconSize: Integer): Integer;
begin
  Result:= 0;
  // read Type and Size data from subdir
  if SubDirIndex < 0 then Exit;
  with FDirectories.Items[SubDirIndex] do
  begin
    if IconType = 'Fixed' then
      Result:= abs(IconSize - AIconSize)
    else if IconType = 'Scaled' then
      begin
        if AIconSize < IconMinSize then
          Result:= IconMinSize - AIconSize;
        if AIconSize > IconMaxSize then
          Result:= AIconSize - IconMaxSize;
      end
    else if IconType = 'Threshold' then
      begin
        if AIconSize < IconSize - IconThreshold then
          Result:= IconMinSize - AIconSize;
        if AIconSize > IconSize + IconThreshold then
          Result:= AIconSize - IconMaxSize;
      end;
  end;
end;


constructor TIconTheme.Create(sThemeName: String);
begin
  FTheme:= sThemeName;
  FInherits:= nil;
  FDirectories:= nil;
end;

destructor TIconTheme.Destroy;
var
  I: Integer;
begin
  if Assigned(FInherits) then
    begin
      for I:= FInherits.Count - 1 downto 0 do
        begin
          if Assigned(FInherits.Objects[I]) then
            begin
              TIconTheme(FInherits.Objects[I]).Free;
              FInherits.Objects[I]:= nil;
            end;
          FInherits.Delete(I);
        end;
      FreeThenNil(FInherits);
    end;
  FreeThenNil(FDirectories);
  inherited Destroy;
end;

function TIconTheme.Load(AInherits: TStringList): Boolean;
var
 I: Integer;
 sValue: String;
 sElement: String;
 sThemeName: String;
 ParentTheme: TIconTheme;
begin
   Result:= False;
   for I:= Low(BaseNameList) to  High(BaseNameList) do
     begin
       sElement:= BaseNameList[I] + PathDelim + FTheme + PathDelim + 'index.theme';
       if FileExists(sElement) then
         begin
           sThemeName:= sElement;
           Result:= True;
           Break;
         end;
     end;
   // theme not found
   if Result = False then Exit;

   FDirectories:= TIconDirList.Create;
   // list of parent themes
   if Assigned(AInherits) then // if this theme is child
     FInherits:= AInherits
   else // new theme
     FInherits:= TStringList.Create;

   // load theme from file
   FIniFile:= TIniFile.Create(sThemeName);
   FThemeName:= FIniFile.ReadString('Icon Theme', 'Name', EmptyStr);
   FComment:= FIniFile.ReadString('Icon Theme', 'Comment', EmptyStr);

   // read theme directories
   sValue:= FIniFile.ReadString('Icon Theme', 'Directories', EmptyStr);
   WriteLn(sValue);
   repeat
     sElement:= Copy2SymbDel(sValue, ',');
     FDirectories.Add(sElement, LoadIconDirInfo(sElement));
   until sValue = EmptyStr;

   // read parent themes
   sValue:= FIniFile.ReadString('Icon Theme', 'Inherits', EmptyStr);
   if sValue <> EmptyStr then
     repeat
       sElement:= Copy2SymbDel(sValue, ',');
       // skip if already loaded
       if FInherits.IndexOf(sElement) >= 0 then Continue;
       // load parent theme
       ParentTheme:= TIconTheme.Create(sElement);
       FInherits.AddObject(sElement, ParentTheme);
       ParentTheme.Load(FInherits);
     until sValue = EmptyStr;

   // add default theme if needed
   if FInherits.IndexOf(DEFAULT_THEME_NAME) < 0 then
     begin
       ParentTheme:= TIconTheme.Create(DEFAULT_THEME_NAME);
       FInherits.AddObject(DEFAULT_THEME_NAME, ParentTheme);
       ParentTheme.Load(FInherits);
     end;
end;

function TIconTheme.FindIcon(AIconName: String; AIconSize: Integer): UTF8String;
begin
  Result:= FindIconHelper(AIconName, AIconSize);
  if Result = EmptyStr then
    Result:= LookupFallbackIcon(AIconName);
end;

function TIconTheme.LookupIcon(AIconName: String; AIconSize: Integer): UTF8String;
var
  I, J, K: Integer;
  sFileName,
  sClosestFileName: String;
  MinimalSize,
  NewSize: Integer;
begin
  for I:= 0 to FDirectories.Count - 1 do
    for J:= Low(BaseNameList) to  High(BaseNameList) do
      for K:= Low(IconExtensionList) to High(IconExtensionList) do
        if DirectoryMatchesSize(I, AIconSize) then
        begin
          sFileName:= BaseNameList[J] + PathDelim + FTheme + PathDelim + FDirectories[I] + PathDelim + AIconName + '.' + IconExtensionList[K];
          if FileExists(sFileName) then
            begin
    	      Result:= sFileName;
              Exit;
            end;
        end;

  MinimalSize:= MaxInt;
  for I:= 0 to FDirectories.Count - 1 do
    for J:= Low(BaseNameList) to  High(BaseNameList) do
      for K:= Low(IconExtensionList) to High(IconExtensionList) do
        begin
          sFileName:= BaseNameList[J] + PathDelim + FTheme + PathDelim + FDirectories[I] + PathDelim + AIconName + '.' + IconExtensionList[K];
          if FileExists(sFileName) then
            begin
              NewSize:= DirectorySizeDistance(I, AIconSize);
              if NewSize < MinimalSize then
                begin
	          sClosestFileName:= sFileName;
	          MinimalSize:= NewSize;
                end;
            end;
        end;

  if (sClosestFileName <> EmptyStr) then
    Result:= sClosestFileName
  else
    Result:= EmptyStr;
end;

function TIconTheme.LoadIconDirInfo(const sIconDirName: String): PIconDirInfo;
begin
  New(Result);
  with Result^ do
  begin
    IconSize:= FIniFile.ReadInteger(sIconDirName, 'Size', 48);
    IconContext:= FIniFile.ReadString(sIconDirName, 'Context', EmptyStr);
    IconType:= FIniFile.ReadString(sIconDirName, 'Type', 'Threshold');
    IconMaxSize:= FIniFile.ReadInteger(sIconDirName, 'MaxSize', IconSize);
    IconMinSize:= FIniFile.ReadInteger(sIconDirName, 'MinSize', IconSize);
    IconThreshold:= FIniFile.ReadInteger(sIconDirName, 'Threshold', 2);
  end;
end;

function TIconTheme.FindIconHelper(aIconName: String; AIconSize: Integer): UTF8String;
var
  I: Integer;
begin
  Result:= LookupIcon(AIconName, AIconSize);
  if Result <> EmptyStr then
    Exit;
  // find in parent themes
  for I:= 0 to FInherits.Count - 1 do
    begin
      Result:= TIconTheme(FInherits.Objects[I]).LookupIcon(aIconName, AIconSize);
      if Result <> EmptyStr then
        Exit;
    end;
  Result:= EmptyStr;
end;

{ TIconDirList }

function TIconDirList.Add(IconDirName: String; IconDirInfo: PIconDirInfo): Integer;
begin
  Result:= AddObject(IconDirName, TObject(IconDirInfo));
end;

function TIconDirList.GetIconDir(Index: Integer): TIconDirInfo;
begin
  Result:= PIconDirInfo(Objects[Index])^;
end;

destructor TIconDirList.Destroy;
var
  I: Integer;
begin
  for I:= Count - 1 downto 0 do
    begin
      if Assigned(Objects[I]) then
        Dispose(PIconDirInfo(Objects[I]));
      Delete(I);
    end;
  inherited Destroy;
end;

end.
