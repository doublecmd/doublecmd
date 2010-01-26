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
  SysUtils, Classes, IniFiles, StringHashList;

type
  TIconType = (itFixed, itScalable, itThreshold);

  TIconDirInfo = record
    IconSize: Integer;
    //IconContext: String; // currently not used
    IconType: TIconType;
    IconMaxSize,
    IconMinSize,
    IconThreshold: Integer;
    FileListCache: array of TStringHashList;
  end;
  PIconDirInfo = ^TIconDirInfo;

  { TIconDirList }

  TIconDirList = class (TStringList)
  private
    function GetIconDir(Index: Integer): PIconDirInfo;
  public
    destructor Destroy; override;
    function Add(IconDirName: String; IconDirInfo: PIconDirInfo): Integer; reintroduce;
    property Items[Index: Integer]: PIconDirInfo read GetIconDir;
  end;

  { TIconTheme }

  TIconTheme = class
  private
    FTheme,
    FThemeName: String;
    FComment: UTF8String;
    FInherits: TStringList;
    FOwnsInheritsObject: Boolean;
    FDirectories: TIconDirList;
    FBaseNameList: array of String;
    function LoadIconDirInfo(const IniFile: TIniFile; const sIconDirName: String): PIconDirInfo;
    function FindIconHelper(aIconName: String; AIconSize: Integer): UTF8String;
    function LoadThemeWithInherited(AInherits: TStringList): Boolean;
    procedure CacheDirectoryFiles(SubDirIndex: Integer; BaseDirIndex: Integer);
  protected
    function LookupIcon(AIconName: String; AIconSize: Integer): UTF8String;
  public
    constructor Create(sThemeName: String);
    destructor Destroy; override;
    function Load: Boolean;
    function FindIcon(AIconName: String; AIconSize: Integer): UTF8String;
    function DirectoryMatchesSize(SubDirIndex: Integer; AIconSize: Integer): Boolean;
    function DirectorySizeDistance(SubDirIndex: Integer; AIconSize: Integer): Integer;
    class function CutTrailingExtension(const AIconName: String): String;
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
  LCLProc, StrUtils, BaseUnix, uDCUtils;

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
  with FDirectories.Items[SubDirIndex]^ do
  case IconType of
    itFixed:
      Result:= (IconSize = AIconSize);
    itScalable:
      Result:= (IconMinSize <= AIconSize) and (AIconSize <= IconMaxSize);
    itThreshold:
      Result:= ((IconSize - IconThreshold) <= AIconSize) and (AIconSize <= (IconSize + IconThreshold));
  end;
end;


function TIconTheme.DirectorySizeDistance(SubDirIndex: Integer; AIconSize: Integer): Integer;
begin
  Result:= 0;
  // read Type and Size data from subdir
  if SubDirIndex < 0 then Exit;
  with FDirectories.Items[SubDirIndex]^ do
  case IconType of
    itFixed:
      Result:= abs(IconSize - AIconSize);
    itScalable:
      begin
        if AIconSize < IconMinSize then
          Result:= IconMinSize - AIconSize;
        if AIconSize > IconMaxSize then
          Result:= AIconSize - IconMaxSize;
      end;
    itThreshold:
      begin
        if AIconSize < IconSize - IconThreshold then
          Result:= IconMinSize - AIconSize;
        if AIconSize > IconSize + IconThreshold then
          Result:= AIconSize - IconMaxSize;
      end;
  end;
end;


constructor TIconTheme.Create(sThemeName: String);
var
 I, J: Integer;
 sElement: String;
begin
  FTheme:= sThemeName;
  FInherits:= nil;
  FOwnsInheritsObject:= False;
  FDirectories:= nil;
  J:= 0;
  SetLength(FBaseNameList, Length(BaseNameList));
  for I:= Low(BaseNameList) to  High(BaseNameList) do
    begin
      sElement:= BaseNameList[I];
      // use only directories that has this theme
      if DirectoryExists(sElement + PathDelim + FTheme) then
        begin
          FBaseNameList[J]:= sElement;
          Inc(J);
        end;
    end;
  SetLength(FBaseNameList, J);
end;

destructor TIconTheme.Destroy;
var
  I: Integer;
begin
  if FOwnsInheritsObject and Assigned(FInherits) then
    begin
      for I:= FInherits.Count - 1 downto 0 do
        begin
          if Assigned(FInherits.Objects[I]) then
            begin
              TIconTheme(FInherits.Objects[I]).Free;
              FInherits.Objects[I]:= nil;
            end;
        end;
      FreeThenNil(FInherits);
    end;
  FreeThenNil(FDirectories);
  inherited Destroy;
end;

function TIconTheme.Load: Boolean;
var
  I: Integer;
  ParentTheme: TIconTheme;
begin
   Result := LoadThemeWithInherited(FInherits);

   // add default theme if needed
   if FInherits.IndexOf(DEFAULT_THEME_NAME) < 0 then
     begin
       ParentTheme:= TIconTheme.Create(DEFAULT_THEME_NAME);
       I:= FInherits.AddObject(DEFAULT_THEME_NAME, ParentTheme);
       if not ParentTheme.LoadThemeWithInherited(FInherits) then
         begin
           ParentTheme.Free;
           FInherits.Delete(I);
         end;
     end;
end;

function TIconTheme.LoadThemeWithInherited(AInherits: TStringList): Boolean;
var
 I: Integer;
 sValue: String;
 sElement: String;
 sThemeName: String;
 ParentTheme: TIconTheme;
 IniFile: TIniFile = nil;
begin
   Result:= False;
   for I:= Low(FBaseNameList) to  High(FBaseNameList) do
     begin
       sElement:= FBaseNameList[I] + PathDelim + FTheme + PathDelim + 'index.theme';
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
     begin
       FInherits:= TStringList.Create;
       FOwnsInheritsObject:= True;
     end;

   // load theme from file
   IniFile:= TIniFile.Create(sThemeName);
   try
     FThemeName:= IniFile.ReadString('Icon Theme', 'Name', EmptyStr);
     FComment:= IniFile.ReadString('Icon Theme', 'Comment', EmptyStr);

     DebugLn('Loading icon theme ', FThemeName);

     // read theme directories
     sValue:= IniFile.ReadString('Icon Theme', 'Directories', EmptyStr);
     repeat
       sElement:= Copy2SymbDel(sValue, ',');
       FDirectories.Add(sElement, LoadIconDirInfo(IniFile, sElement));
     until sValue = EmptyStr;

     // read parent themes
     sValue:= IniFile.ReadString('Icon Theme', 'Inherits', EmptyStr);
     if sValue <> EmptyStr then
       repeat
         sElement:= Copy2SymbDel(sValue, ',');
         // skip if already loaded
         if FInherits.IndexOf(sElement) >= 0 then Continue;
         // load parent theme
         ParentTheme:= TIconTheme.Create(sElement);
         I:= FInherits.AddObject(sElement, ParentTheme);
         if not ParentTheme.LoadThemeWithInherited(FInherits) then
           begin
             ParentTheme.Free;
             FInherits.Delete(I);
           end;
       until sValue = EmptyStr;

   finally
     FreeAndNil(IniFile);
   end;
end;

function TIconTheme.FindIcon(AIconName: String; AIconSize: Integer): UTF8String;
begin
  Result:= FindIconHelper(AIconName, AIconSize);
{
  if Result = EmptyStr then
    Result:= LookupFallbackIcon(AIconName);
}
end;

function TIconTheme.LookupIcon(AIconName: String; AIconSize: Integer): UTF8String;
var
  I, J, FoundIndex: Integer;
  MinimalSize,
  NewSize: Integer;

  procedure MakeResult; inline;
  begin
    Result:= FBaseNameList[J] + PathDelim + FTheme + PathDelim +
             FDirectories.Strings[I] + PathDelim +
             AIconName + '.' +
             IconExtensionList[Integer(FDirectories.Items[I]^.FileListCache[J].List[FoundIndex]^.Data)];
  end;

begin
  { This is a slightly more optimized version of
    the original algorithm from freedesktop.org. }

  Result:= EmptyStr;
  MinimalSize:= MaxInt;
  for J:= Low(FBaseNameList) to  High(FBaseNameList) do
  begin
    for I:= 0 to FDirectories.Count - 1 do
      begin
        if not Assigned(FDirectories.Items[I]^.FileListCache[J]) then
          CacheDirectoryFiles(I, J);

        FoundIndex:= FDirectories.Items[I]^.FileListCache[J].Find(AIconName);
        if FoundIndex >= 0 then
          begin
            NewSize:= DirectorySizeDistance(I, AIconSize);
            if NewSize = 0 then  // exact match
              begin
                MakeResult;
                Exit;
              end
            else if NewSize < MinimalSize then
              begin
                MakeResult;
                MinimalSize:= NewSize;
              end;
          end;
      end;
  end;
end;

function TIconTheme.LoadIconDirInfo(const IniFile: TIniFile; const sIconDirName: String): PIconDirInfo;
var
  IconTypeStr: String;
  I: Integer;
begin
  New(Result);
  with Result^ do
  begin
    IconSize:= IniFile.ReadInteger(sIconDirName, 'Size', 48);
    //IconContext:= IniFile.ReadString(sIconDirName, 'Context', EmptyStr); // currently not used
    IconTypeStr:= IniFile.ReadString(sIconDirName, 'Type', 'Threshold');
    IconMaxSize:= IniFile.ReadInteger(sIconDirName, 'MaxSize', IconSize);
    IconMinSize:= IniFile.ReadInteger(sIconDirName, 'MinSize', IconSize);
    IconThreshold:= IniFile.ReadInteger(sIconDirName, 'Threshold', 2);

    if IconTypeStr = 'Fixed' then
      IconType:= itFixed
    else if IconTypeStr = 'Scalable' then
      IconType:= itScalable
    else if IconTypeStr = 'Threshold' then
      IconType:= itThreshold
    else
      begin
        Dispose(Result);
        raise Exception.Create('Unsupported icon type');
      end;

    SetLength(FileListCache, Length(FBaseNameList));

    for I:= 0 to Length(FBaseNameList) - 1 do
      FileListCache[I]:= nil;
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

procedure TIconTheme.CacheDirectoryFiles(SubDirIndex: Integer; BaseDirIndex: Integer);
var
  SearchDir, FoundName, FoundExt: String;
  DirPtr: PDir;
  PtrDirEnt: pDirent;
  DirList: TStringHashList;
  I: Integer;
begin
  DirList:= TStringHashList.Create(True);
  FDirectories.Items[SubDirIndex]^.FileListCache[BaseDirIndex]:= DirList;

  SearchDir := FBaseNameList[BaseDirIndex] + PathDelim +
               FTheme + PathDelim +
               FDirectories.Strings[SubDirIndex];

  DirPtr:= BaseUnix.fpOpenDir(PChar(SearchDir));
  if Assigned(DirPtr) then
    begin
      PtrDirEnt:= BaseUnix.fpReadDir(DirPtr^);
      while PtrDirEnt <> nil do
        begin
          FoundName := string(PtrDirEnt^.d_name);
          if (FoundName <> '.') and (FoundName <> '..') then
          begin
            FoundExt := ExtractFileExt(FoundName);
            if Length(FoundExt) > 0 then
            begin
              FoundName := Copy(FoundName, 1, Length(FoundName) - Length(FoundExt));
              Delete(FoundExt, 1, 1); // remove the dot

              // Add only files with supported extensions.
              for I:= Low(IconExtensionList) to High(IconExtensionList) do
                if IconExtensionList[I] = FoundExt then
                begin
                  DirList.Add(FoundName, Pointer(PtrInt(I)));
                  break;
                end;
            end;
          end;
          PtrDirEnt:= BaseUnix.fpReadDir(DirPtr^);
        end;
      BaseUnix.fpCloseDir(DirPtr^);
    end;
end;

class function TIconTheme.CutTrailingExtension(const AIconName: String): String;
var
  I: Integer;
begin
  for I:= Low(IconExtensionList) to High(IconExtensionList) do
    if StrEnds(AIconName, '.' + IconExtensionList[I]) then
      Exit(Copy(AIconName, 1, Length(AIconName) - Length(IconExtensionList[I]) - 1));
  Result := AIconName;
end;

{ TIconDirList }

function TIconDirList.Add(IconDirName: String; IconDirInfo: PIconDirInfo): Integer;
begin
  Result:= AddObject(IconDirName, TObject(IconDirInfo));
end;

function TIconDirList.GetIconDir(Index: Integer): PIconDirInfo;
begin
  Result:= PIconDirInfo(Objects[Index]);
end;

destructor TIconDirList.Destroy;
var
  I, J: Integer;
  IconDirInfo: PIconDirInfo;
begin
  for I:= Count - 1 downto 0 do
    begin
      if Assigned(Objects[I]) then
        begin
          IconDirInfo:= PIconDirInfo(Objects[I]);
          for J := 0 to Length(IconDirInfo^.FileListCache) - 1 do
            IconDirInfo^.FileListCache[J].Free;
          Dispose(IconDirInfo);
        end;
    end;
  inherited Destroy;
end;

end.
