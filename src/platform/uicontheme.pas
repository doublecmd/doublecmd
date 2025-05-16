{
    Double Commander
    -------------------------------------------------------------------------
    Simple implementation of Icon Theme based on FreeDesktop.org specification
    (http://standards.freedesktop.org/icon-theme-spec/icon-theme-spec-latest.html)
    (https://gitlab.gnome.org/GNOME/gtk/blob/main/docs/iconcache.txt)

    Copyright (C) 2009-2025 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit uIconTheme;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, DCOSUtils, DCStringHashListUtf8, DCClassesUtf8;

type

  { TIconCache }

  TIconCache = class
  private
    FFile: TFileMapRec;
    function Validate: Boolean;
    function LoadFromFile(const FileName: String): Boolean;
    function GetDirectoryIndex(const Name: PAnsiChar): Integer;
  public
    destructor Destroy; override;
    function GetIcons(const Directory: String): TStringHashListUtf8;
    class function CreateFrom(const Directory: String): TIconCache;
  end;

type
  TIconType = (itFixed, itScalable, itThreshold);

  TIconDirInfo = record
    IconSize: Integer;
    IconScale: Integer;
    //IconContext: String; // currently not used
    IconType: TIconType;
    IconMaxSize,
    IconMinSize,
    IconThreshold: Integer;
    FileListCache: array of TStringHashListUtf8;
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
  protected
    FTheme,
    FThemeName: String;
    FComment: String;
    FCache: TIconCache;
    FCacheIndex: Integer;
    FDefaultTheme: String;
    FInherits: TStringList;
    FOwnsInheritsObject: Boolean;
    FDirectories: TIconDirList;
    FBaseDirList: array of String;         //en> List of directories that have this theme's icons.
    FBaseDirListAtCreate: array of String; //en> Base dir list passed to Create
    function LoadIconDirInfo(const IniFile: TIniFileEx; const sIconDirName: String): PIconDirInfo;
    function FindIconHelper(aIconName: String; AIconSize, AIconScale: Integer): String;
    function LoadThemeWithInherited(AInherits: TStringList): Boolean;
    procedure LoadParentTheme(AThemeName: String);
    procedure CacheDirectoryFiles(SubDirIndex: Integer; BaseDirIndex: Integer);
  protected
    function LookupIcon(AIconName: String; AIconSize, AIconScale: Integer): String;
    function CreateParentTheme(const sThemeName: String): TIconTheme; virtual;
  public
    constructor Create(sThemeName: String; var BaseDirList: array of String; ADefaultTheme: String = ''); virtual;
    destructor Destroy; override;
    function Load: Boolean; virtual;
    function FindIcon(AIconName: String; AIconSize: Integer; AIconScale: Integer = 1): String;
    function DirectoryMatchesSize(SubDirIndex: Integer; AIconSize, AIconScale: Integer): Boolean;
    function DirectorySizeDistance(SubDirIndex: Integer; AIconSize, AIconScale: Integer): Integer;
    class function CutTrailingExtension(const AIconName: String): String;
    class procedure RegisterExtension(const AExtension: String);
    property ThemeName: String read FThemeName;
    property Directories: TIconDirList read FDirectories;
    property DefaultTheme: String read FDefaultTheme write FDefaultTheme;
  end;

implementation

uses
  LCLProc, StrUtils, uDebug, uFindEx, DCBasicTypes, DCStrUtils;

const
  EXT_IDX_PNG = 0;
  EXT_IDX_XPM = 1;
  EXT_IDX_SVG = 2;

const
  HAS_SUFFIX_XPM = 1;
  HAS_SUFFIX_SVG = 2;
  HAS_SUFFIX_PNG = 4;

var
  IconExtensionList: TDynamicStringArray;

function ReadUInt16(const P: Pointer; Offset: UInt32): UInt16; inline;
begin
  Result:= BEtoN(PUInt16(PByte(P) + Offset)^);
end;

function ReadUInt32(const P: Pointer; Offset: UInt32): UInt32; inline;
begin
  Result:= BEtoN(PUInt32(PByte(P) + Offset)^);
end;

{ TIconCache }

function TIconCache.Validate: Boolean;
var
  I, J: Integer;
  Offset: UInt32;
  ACount: UInt32;
  AVersion: UInt16;
  HashOffset, HashCount: UInt32;
  ImageOffset, ImageCount: UInt32;
  ChainOffset, NameOffset: UInt32;

  function CheckString(S: PAnsiChar): Boolean; inline;
  begin
    Result:= (IndexByte(S^, 1024, 0) > -1);
  end;

  function CheckOffset(AOffset, ASize: UInt32): Boolean; inline;
  begin
    Result:= (AOffset + ASize < FFile.FileSize);
  end;

begin
  Result:= False;

  if FFile.FileSize < 12 then Exit;

  AVersion:= ReadUInt16(FFile.MappedFile, 0);
  if AVersion <> 1 then Exit;
  AVersion:= ReadUInt16(FFile.MappedFile, 2);
  if AVersion <> 0 then Exit;

  Offset:= ReadUInt32(FFile.MappedFile, 8);
  if not CheckOffset(Offset, 4) then Exit;
  ACount:= ReadUInt32(FFile.MappedFile, Offset);

  for I:= 0 to Int32(ACount) - 1 do
  begin
    if not CheckOffset(Offset + 4 + I * 4, 4) then Exit;
    NameOffset:= ReadUInt32(FFile.MappedFile, Offset + 4 + I * 4);
    if not CheckOffset(NameOffset, 0) then Exit;
    CheckString(PAnsiChar(FFile.MappedFile) + NameOffset)
  end;

  HashOffset:= ReadUInt32(FFile.MappedFile, 4);
  if not CheckOffset(HashOffset, 4) then Exit;
  HashCount:= ReadUInt32(FFile.MappedFile, HashOffset);

  for I:= 0 to Int32(HashCount) - 1 do
  begin
    if not CheckOffset(HashOffset + 4 + I * 4, 4) then Exit(False);
    ChainOffset:= ReadUInt32(FFile.MappedFile, HashOffset + 4 + I * 4);

    while (ChainOffset <> $FFFFFFFF) do
    begin
      if not CheckOffset(ChainOffset + 8, 4) then Exit;

      NameOffset:= ReadUInt32(FFile.MappedFile, ChainOffset + 4);
      if not CheckOffset(NameOffset, 0) then Exit(False);
      CheckString(PAnsiChar(FFile.MappedFile) + NameOffset);

      ImageOffset:= ReadUInt32(FFile.MappedFile, ChainOffset + 8);
      if not CheckOffset(ImageOffset, 4) then Exit(False);
      ImageCount:= ReadUInt32(FFile.MappedFile, ImageOffset);

      for J:= 0 to Int32(ImageCount) - 1 do
      begin
        if not CheckOffset(ImageOffset + 4 + J * 8 + 2, 2) then Exit;
      end;

      ChainOffset:= ReadUInt32(FFile.MappedFile, ChainOffset);
    end;
  end;

  Result:= True;
end;

function TIconCache.LoadFromFile(const FileName: String): Boolean;
begin
  Result:= MapFile(FileName, FFile);
  if Result and not Validate then
  begin
    Result:= False;
    UnMapFile(FFile);
  end;
end;

function TIconCache.GetDirectoryIndex(const Name: PAnsiChar): Integer;
var
  Index: Integer;
  Offset: UInt32;
  ACount: UInt32;
  NameOffset: UInt32;
  DirectoryName: PAnsiChar;
begin
  // Directory list offset
  Offset:= ReadUInt32(FFile.MappedFile, 8);
  ACount:= ReadUInt32(FFile.MappedFile, Offset);
  Offset:= Offset + 4;

  for Index:= 0 to Int32(ACount) - 1 do
  begin
    NameOffset:= ReadUInt32(FFile.MappedFile, Offset + Index * 4);
    DirectoryName:= PAnsiChar(FFile.MappedFile) + NameOffset;
    if StrComp(Name, DirectoryName) = 0 then Exit(Index);
  end;
  Result:= -1;
end;

destructor TIconCache.Destroy;
begin
  UnMapFile(FFile);
  inherited Destroy;
end;

function TIconCache.GetIcons(const Directory: String): TStringHashListUtf8;
var
  I, J: Integer;
  Flags: UInt16;
  ExtIdx: IntPtr;
  NameValue: PAnsiChar;
  DirectoryIndex: Integer;
  HashOffset, HashCount: UInt32;
  ImageOffset, ImageCount: UInt32;
  ChainOffset, NameOffset: UInt32;
begin
  DirectoryIndex:= GetDirectoryIndex(PAnsiChar(Directory));

  if DirectoryIndex < 0 then
  begin
    // DCDebug('Not found in cache ', Directory);
    Exit(nil);
  end;

  HashOffset:= ReadUInt32(FFile.MappedFile, 4);
  HashCount:= ReadUInt32(FFile.MappedFile, HashOffset);

  Result:= TStringHashListUtf8.Create(FileNameCaseSensitive);

  for I:= 0 to Int32(HashCount) - 1 do
  begin
    ChainOffset:= ReadUInt32(FFile.MappedFile, HashOffset + 4 + I * 4);

    while (ChainOffset <> $FFFFFFFF) do
    begin
      ImageOffset:= ReadUInt32(FFile.MappedFile, ChainOffset + 8);
      ImageCount:= ReadUInt32(FFile.MappedFile, ImageOffset);

      for J:= 0 to Int32(ImageCount) - 1 do
      begin
        if (DirectoryIndex = ReadUInt16(FFile.MappedFile, ImageOffset + 4 + J * 8)) then
        begin
          Flags:= ReadUInt16(FFile.MappedFile, ImageOffset + 4 + J * 8 + 2);

          if (Flags <> 0) then
          begin
            if (Flags and HAS_SUFFIX_SVG <> 0) then
              ExtIdx:= EXT_IDX_SVG
            else if (Flags and HAS_SUFFIX_PNG <> 0) then
              ExtIdx:= EXT_IDX_PNG
            else if (Flags and HAS_SUFFIX_XPM <> 0) then
              ExtIdx:= EXT_IDX_XPM
            else begin
              Break;
            end;

            NameOffset:= ReadUInt32(FFile.MappedFile, ChainOffset + 4);
            NameValue:= PAnsiChar(FFile.MappedFile) + NameOffset;
            Result.Add(StrPas(NameValue), Pointer(ExtIdx));
          end;

          Break;
        end;
      end;

      ChainOffset:= ReadUInt32(FFile.MappedFile, ChainOffset);
    end;
  end;
end;

class function TIconCache.CreateFrom(const Directory: String): TIconCache;
var
  FileName: String;
  CacheTime: DCBasicTypes.TFileTime;
begin
  Result:= nil;
  FileName:= Directory + PathDelim + 'icon-theme.cache';

  CacheTime:= mbFileAge(FileName);

  if (CacheTime <> DCBasicTypes.TFileTime(-1)) then
  begin
     if (CacheTime < mbFileAge(Directory)) then
     begin
       DCDebug('Icon cache outdated');
     end
     else begin
       Result:= TIconCache.Create;
       if not Result.LoadFromFile(FileName) then
       begin
         FreeAndNil(Result);
       end;
     end;
  end;
end;

{ TIconTheme }

function TIconTheme.DirectoryMatchesSize(SubDirIndex: Integer; AIconSize,
  AIconScale: Integer): Boolean;
begin
  Result:= False;
  // read Type and Size data from subdir
  if SubDirIndex < 0 then Exit;
  with FDirectories.Items[SubDirIndex]^ do
  begin
    if (IconScale <> AIconScale) then
      Exit;
    case IconType of
      itFixed:
        Result:= (IconSize = AIconSize);
      itScalable:
        Result:= (IconMinSize <= AIconSize) and (AIconSize <= IconMaxSize);
      itThreshold:
        Result:= ((IconSize - IconThreshold) <= AIconSize) and (AIconSize <= (IconSize + IconThreshold));
    end;
  end;
end;


function TIconTheme.DirectorySizeDistance(SubDirIndex: Integer; AIconSize,
  AIconScale: Integer): Integer;
begin
  Result:= 0;
  // read Type and Size data from subdir
  if SubDirIndex < 0 then Exit;
  with FDirectories.Items[SubDirIndex]^ do
  case IconType of
    itFixed:
      Result:= abs(IconSize * IconScale - AIconSize * AIconScale);
    itScalable:
      begin
        if AIconSize * AIconScale < IconMinSize * IconScale then
          Result:= IconMinSize * IconScale - AIconSize * AIconScale;
        if AIconSize * AIconScale > IconMaxSize * IconScale then
          Result:= AIconSize * AIconScale - IconMaxSize * IconScale;
      end;
    itThreshold:
      begin
        if AIconSize * AIconScale < (IconSize - IconThreshold) * IconScale then
          Result:= IconMinSize * IconScale - AIconSize * AIconScale;
        if AIconSize * AIconScale > (IconSize + IconThreshold) * IconScale then
          Result:= AIconSize * AIconScale - IconMaxSize * IconScale;
      end;
  end;
end;


constructor TIconTheme.Create(sThemeName: String; var BaseDirList: array of String;
  ADefaultTheme: String);
var
 I, J: Integer;
 sElement: String;
begin
  FTheme:= sThemeName;
  FDefaultTheme:= ADefaultTheme;
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
begin
  if FOwnsInheritsObject then
  begin
    FInherits.Free;
  end;
  FCache.Free;
  FreeAndNil(FDirectories);
  inherited Destroy;
end;

function TIconTheme.Load: Boolean;
var
  ADefault: String;
  ADefaultArray: TDynamicStringArray;
begin
   Result := LoadThemeWithInherited(FInherits);
   if Result and FOwnsInheritsObject then
   begin
     ADefaultArray:= SplitString(FDefaultTheme, PathSeparator);
     for ADefault in ADefaultArray do LoadParentTheme(ADefault);
   end;
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
           DCDebug('Loading icon theme ', FTheme);
           FCache:= TIconCache.CreateFrom(FBaseDirList[I] + PathDelim + FTheme);
           sThemeName:= sElement;
           FCacheIndex:= I;
           Result:= True;
           Break;
         end;
     end;

   // theme not found
   if Result = False then
     begin
       DCDebug('Theme ', FTheme, ' not found');
       Exit;
     end;

   FDirectories:= TIconDirList.Create;
   // list of parent themes
   if Assigned(AInherits) then // if this theme is child
     FInherits:= AInherits
   else // new theme
     begin
       FInherits:= TStringList.Create;
       FInherits.OwnsObjects:= True;
       FOwnsInheritsObject:= True;
     end;

   // load theme from file
   IniFile:= TIniFileEx.Create(sThemeName, fmOpenRead);
   try
     FThemeName:= IniFile.ReadString('Icon Theme', 'Name', EmptyStr);
     FComment:= IniFile.ReadString('Icon Theme', 'Comment', EmptyStr);

     // read theme directories
     sValue:= IniFile.ReadString('Icon Theme', 'Directories', EmptyStr);
     repeat
       sElement:= Copy2SymbDel(sValue, ',');
       IconDirInfo:= LoadIconDirInfo(IniFile, sElement);
       if Assigned(IconDirInfo) then FDirectories.Add(sElement, IconDirInfo);
     until sValue = EmptyStr;

     // load icons from cache
     if Assigned(FCache) then
     begin
       DCDebug('Loading theme icons from cache');

       for I:= 0 to FDirectories.Count - 1 do
       begin
         FDirectories.Items[I]^.FileListCache[FCacheIndex]:= FCache.GetIcons(FDirectories[I]);
       end;
     end;

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
  Index: Integer;
  ATheme: TIconTheme;
begin
  if (FTheme <> AThemeName) and (FInherits.IndexOf(AThemeName) < 0) then
    begin
      ATheme:= CreateParentTheme(AThemeName);
      Index:= FInherits.AddObject(AThemeName, ATheme);
      if not ATheme.LoadThemeWithInherited(FInherits) then
        begin
          FInherits.Delete(Index);
        end;
    end;
end;

function TIconTheme.FindIcon(AIconName: String; AIconSize: Integer;
  AIconScale: Integer): String;
begin
  Result:= FindIconHelper(AIconName, AIconSize, AIconScale);
{
  if Result = EmptyStr then
    Result:= LookupFallbackIcon(AIconName);
}
end;

function TIconTheme.LookupIcon(AIconName: String; AIconSize, AIconScale: Integer): String;
var
  ExtIdx: PtrInt;
  I, J, FoundIndex: Integer;
  MinimalSize, NewSize: Integer;
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
      NewSize:= DirectorySizeDistance(I, AIconSize, AIconScale);

      if (NewSize < MinimalSize) or (NewSize = 0) then
      begin
        if not Assigned(FDirectories.Items[I]^.FileListCache[J]) then
          CacheDirectoryFiles(I, J);

        FoundIndex:= FDirectories.Items[I]^.FileListCache[J].Find(AIconName);

        if FoundIndex >= 0 then
        begin
          ExtIdx:= PtrInt(FDirectories.Items[I]^.FileListCache[J].List[FoundIndex]^.Data);

          Result:= FBaseDirList[J] + PathDelim + FTheme + PathDelim +
                   FDirectories.Strings[I] + PathDelim +
                   AIconName + '.' + IconExtensionList[ExtIdx];

          // Exact match
          if (NewSize = 0) and (AIconScale = FDirectories.Items[I]^.IconScale) then
            Exit
          else
            MinimalSize:= NewSize;
        end;
      end;
    end;
  end;
end;

function TIconTheme.CreateParentTheme(const sThemeName: String): TIconTheme;
begin
  Result:= TIconTheme.Create(sThemeName, FBaseDirListAtCreate);
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
    IconScale:= IniFile.ReadInteger(sIconDirName, 'Scale', 1);
    //IconContext:= IniFile.ReadString(sIconDirName, 'Context', EmptyStr); // currently not used
    IconTypeStr:= IniFile.ReadString(sIconDirName, 'Type', 'Threshold');
    IconMaxSize:= IniFile.ReadInteger(sIconDirName, 'MaxSize', IconSize);
    IconMinSize:= IniFile.ReadInteger(sIconDirName, 'MinSize', IconSize);
    IconThreshold:= IniFile.ReadInteger(sIconDirName, 'Threshold', 2);

    if SameText(IconTypeStr, 'Fixed') then
      IconType:= itFixed
    else if SameText(IconTypeStr, 'Scalable') then
      IconType:= itScalable
    else if SameText(IconTypeStr, 'Threshold') then
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

function TIconTheme.FindIconHelper(aIconName: String; AIconSize,
  AIconScale: Integer): String;
var
  I: Integer;
begin
  Result:= LookupIcon(AIconName, AIconSize, AIconScale);
  if Result <> EmptyStr then
    Exit;

  if Assigned(FInherits) then
    begin
      // find in parent themes
      for I:= 0 to FInherits.Count - 1 do
        begin
          Result:= TIconTheme(FInherits.Objects[I]).LookupIcon(aIconName, AIconSize, AIconScale);
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
  DirList: TStringHashListUtf8;
  I: Integer;
begin
  DirList:= TStringHashListUtf8.Create(True);
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

class procedure TIconTheme.RegisterExtension(const AExtension: String);
var
  I: Integer;
  ExtList: TDynamicStringArray;
begin
  ExtList:= SplitString(AExtension, ';');
  for I:= Low(ExtList) to High(ExtList) do
  begin
    AddString(IconExtensionList, ExtList[I]);
  end;
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

initialization
  AddString(IconExtensionList, 'png'); // EXT_IDX_PNG
  AddString(IconExtensionList, 'xpm'); // EXT_IDX_XPM

end.
