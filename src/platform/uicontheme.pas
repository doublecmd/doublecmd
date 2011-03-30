{
    Double Commander
    -------------------------------------------------------------------------
    Simple implementation of Icon Theme based on FreeDesktop.org specification
    (http://standards.freedesktop.org/icon-theme-spec/icon-theme-spec-latest.html)

    Copyright (C) 2009-2011  Koblov Alexander (Alexx2000@mail.ru)

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
  SysUtils, Classes, StringHashList, uClassesEx;

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
    FBaseDirList: array of String;         //en> List of directories that have this theme's icons.
    FBaseDirListAtCreate: array of String; //en> Base dir list passed to Create
    function LoadIconDirInfo(const IniFile: TIniFileEx; const sIconDirName: String): PIconDirInfo;
    function FindIconHelper(aIconName: String; AIconSize: Integer): UTF8String;
    function LoadThemeWithInherited(AInherits: TStringList): Boolean;
    procedure LoadParentTheme(AThemeName: String);
    procedure CacheDirectoryFiles(SubDirIndex: Integer; BaseDirIndex: Integer);
  protected
    function LookupIcon(AIconName: String; AIconSize: Integer): UTF8String;
  public
    constructor Create(sThemeName: String; BaseDirList: array of String);
    destructor Destroy; override;
    function Load: Boolean;
    function FindIcon(AIconName: String; AIconSize: Integer): UTF8String;
    function DirectoryMatchesSize(SubDirIndex: Integer; AIconSize: Integer): Boolean;
    function DirectorySizeDistance(SubDirIndex: Integer; AIconSize: Integer): Integer;
    class function CutTrailingExtension(const AIconName: String): String;
    property ThemeName: String read FThemeName;
    property Directories: TIconDirList read FDirectories;
  end;

implementation

uses
  LCLProc, StrUtils, uDCUtils, uDebug, uFindEx, uTypes, uOSUtils
  {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
  , uUnixIconTheme
  {$ENDIF}
  ;

const
  IconExtensionList: array[0..1] of String = ('png', 'xpm');

{ TIconTheme }

function LookupFallbackIcon (AIconName: String): UTF8String;
begin
(*
  for each directory in $(basename list) {
    for extension in ("png", "svg", "xpm") {
      if exists directory/iconname.extension
        return directory/iconname.extension
    }
  }
*)
  Result := EmptyStr;
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


constructor TIconTheme.Create(sThemeName: String; BaseDirList: array of String);
var
 I, J: Integer;
 sElement: String;
begin
  FTheme:= sThemeName;
  FInherits:= nil;
  FOwnsInheritsObject:= False;
  FDirectories:= nil;
  J:= 0;
  SetLength(FBaseDirList, Length(BaseDirList));
  SetLength(FBaseDirListAtCreate, Length(BaseDirList));
  for I:= Low(BaseDirList) to High(BaseDirList) do
    begin
      sElement:= BaseDirList[I];
      // use only directories that has this theme
      if mbDirectoryExists(sElement + PathDelim + FTheme) then
        begin
          FBaseDirList[J]:= sElement;
          Inc(J);
        end;
      FBaseDirListAtCreate[I] := sElement; // Remember full base dir list.
    end;
  SetLength(FBaseDirList, J);
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
begin
   Result := LoadThemeWithInherited(FInherits);

   {$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
   // add default theme if needed
   if Result and Assigned(FInherits) then
     LoadParentTheme(DEFAULT_THEME_NAME);
   {$ENDIF}
end;

function TIconTheme.LoadThemeWithInherited(AInherits: TStringList): Boolean;
var
 I: Integer;
 sValue: String;
 sElement: String;
 sThemeName: String;
 IniFile: TIniFileEx = nil;
 IconDirInfo: PIconDirInfo = nil;
begin
   Result:= False;
   for I:= Low(FBaseDirList) to  High(FBaseDirList) do
     begin
       sElement:= FBaseDirList[I] + PathDelim + FTheme + PathDelim + 'index.theme';
       if mbFileExists(sElement) then
         begin
           sThemeName:= sElement;
           Result:= True;
           Break;
         end;
     end;

   // theme not found
   if Result = False then
     begin
       DCDebug('Theme ', FTheme, ' not found.');
       Exit;
     end;

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
   IniFile:= TIniFileEx.Create(sThemeName, fmOpenRead);
   try
     FThemeName:= IniFile.ReadString('Icon Theme', 'Name', EmptyStr);
     FComment:= IniFile.ReadString('Icon Theme', 'Comment', EmptyStr);

     DCDebug('Loading icon theme ', FThemeName);

     // read theme directories
     sValue:= IniFile.ReadString('Icon Theme', 'Directories', EmptyStr);
     repeat
       sElement:= Copy2SymbDel(sValue, ',');
       IconDirInfo:= LoadIconDirInfo(IniFile, sElement);
       if Assigned(IconDirInfo) then FDirectories.Add(sElement, IconDirInfo);
     until sValue = EmptyStr;

     // read parent themes
     sValue:= IniFile.ReadString('Icon Theme', 'Inherits', EmptyStr);
     if sValue <> EmptyStr then
       repeat
         sElement:= Copy2SymbDel(sValue, ',');
         LoadParentTheme(sElement);
       until sValue = EmptyStr;

   finally
     FreeAndNil(IniFile);
   end;
end;

procedure TIconTheme.LoadParentTheme(AThemeName: String);
var
  I: Integer;
  ATheme: TIconTheme;
begin
  if FInherits.IndexOf(AThemeName) < 0 then
    begin
      ATheme:= TIconTheme.Create(AThemeName, FBaseDirListAtCreate);
      I:= FInherits.AddObject(AThemeName, ATheme);
      if not ATheme.LoadThemeWithInherited(FInherits) then
        begin
          ATheme.Free;
          FInherits.Delete(I);
        end;
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
    Result:= FBaseDirList[J] + PathDelim + FTheme + PathDelim +
             FDirectories.Strings[I] + PathDelim +
             AIconName + '.' +
             IconExtensionList[PtrInt(FDirectories.Items[I]^.FileListCache[J].List[FoundIndex]^.Data)];
  end;

begin
  Result:= EmptyStr;

  if not Assigned(FDirectories) then
    Exit;

  { This is a slightly more optimized version of
    the original algorithm from freedesktop.org. }

  MinimalSize:= MaxInt;
  for J:= Low(FBaseDirList) to High(FBaseDirList) do
  begin
    for I:= 0 to FDirectories.Count - 1 do
      begin
        NewSize:= DirectorySizeDistance(I, AIconSize);

        if NewSize < MinimalSize then
          begin
            if not Assigned(FDirectories.Items[I]^.FileListCache[J]) then
              CacheDirectoryFiles(I, J);

            FoundIndex:= FDirectories.Items[I]^.FileListCache[J].Find(AIconName);
            if FoundIndex >= 0 then
              begin
                MakeResult;

                if NewSize = 0 then  // exact match
                  Exit
                else
                  MinimalSize:= NewSize;
              end;
          end;
      end;
  end;
end;

function TIconTheme.LoadIconDirInfo(const IniFile: TIniFileEx; const sIconDirName: String): PIconDirInfo;
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
        DCDebug('Theme directory "%s" has unsupported icon type "%s"', [sIconDirName, IconTypeStr]);
        Exit(nil);
      end;

    SetLength(FileListCache, Length(FBaseDirList));

    for I:= 0 to Length(FBaseDirList) - 1 do
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

  if Assigned(FInherits) then
    begin
      // find in parent themes
      for I:= 0 to FInherits.Count - 1 do
        begin
          Result:= TIconTheme(FInherits.Objects[I]).LookupIcon(aIconName, AIconSize);
          if Result <> EmptyStr then
            Exit;
        end;
    end;

  Result:= EmptyStr;
end;

procedure TIconTheme.CacheDirectoryFiles(SubDirIndex: Integer; BaseDirIndex: Integer);
var
  SearchDir, FoundName, FoundExt: String;
  SearchRec: TSearchRecEx;
  DirList: TStringHashList;
  I: Integer;
begin
  DirList:= TStringHashList.Create(True);
  FDirectories.Items[SubDirIndex]^.FileListCache[BaseDirIndex]:= DirList;

  SearchDir := FBaseDirList[BaseDirIndex] + PathDelim +
               FTheme + PathDelim +
               FDirectories.Strings[SubDirIndex];

  if FindFirstEx(SearchDir + PathDelim + '*', 0, SearchRec) = 0 then
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        FoundExt := ExtractFileExt(SearchRec.Name);
        if Length(FoundExt) > 0 then
        begin
          FoundName := Copy(SearchRec.Name, 1, Length(SearchRec.Name) - Length(FoundExt));
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
    until FindNextEx(SearchRec) <> 0;

  FindCloseEx(SearchRec);
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
