{
    Double Commander
    -------------------------------------------------------------------------
    Handles actions associated with MIME types in .desktop files.

    Based on FreeDesktop.org specifications
    (http://standards.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html)
    (http://www.freedesktop.org/wiki/Specifications/mime-apps-spec)

    Copyright (C) 2009-2010  Przemyslaw Nagay (cobines@gmail.com)
    Copyright (C) 2011-2019  Alexander Koblov (alexx2000@mail.ru)

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

unit uMimeActions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes;

type
  PDesktopFileEntry = ^TDesktopFileEntry;
  TDesktopFileEntry = record
    DesktopFilePath: String;
    MimeType: String;
    DisplayName: String;
    Comment: String;
    ExecWithParams: String; // with %F, %U etc.
    Exec: String;           // % params resolved
    IconName: String;
    Categories: TDynamicStringArray;
    Terminal: Boolean;
    Hidden: Boolean;
  end;

{en
   Needs absolute file names.
   Returns a list of PDesktopFileEntry.
}
function GetDesktopEntries(FileNames: TStringList): TList;
{en
   Needs absolute file names.
   Returns a default application command line.
}
function GetDefaultAppCmd(FileNames: TStringList): String;
{en
   Get desktop entry by desktop file name.
}
function GetDesktopEntry(const FileName: String): PDesktopFileEntry;
{en
   Adds a new action for given mimetype.
   @param(MimeType File mime type)
   @param(DesktopEntry Desktop file name or user command)
   @param(DefaultAction Set as default action for this mime type)
   @returns(The function returns @true if successful, @false otherwise)
}
function AddDesktopEntry(const MimeType, DesktopEntry: String;
                                   DefaultAction: Boolean): Boolean;

function TranslateAppExecToCmdLine(const entry: PDesktopFileEntry;
                                   const fileList: TStringList): String;

implementation

uses
  Unix, BaseUnix, DCClassesUtf8, DCStrUtils, uDCUtils, uGlib2,
  uFileProcs, uIconTheme, uClipboard, DCOSUtils, uKeyFile, uGio, uXdg, uMimeType,
  uDebug, uMyUnix;

type
  TMimeAppsGroup = (magDefault, magAdded, magRemoved);
  TMimeAppsGroupSet = set of TMimeAppsGroup;

const
  MIME_APPS: array[TMimeAppsGroup] of String = (
    'Default Applications',
    'Added Associations',
    'Removed Associations'
  );

type
  TMimeAppsList = record
    Defaults,
    Added,
    Removed: TDynamicStringArray;
  end;

function TranslateAppExecToCmdLine(const entry: PDesktopFileEntry;
                                   const fileList: TStringList): String;
var
  StartPos: Integer = 1;
  CurPos: Integer = 1;
  i: Integer;
  filesAdded: Boolean = False;
begin
  // The .desktop standard does not recommend using % parameters inside quotes
  // in the Exec entry (the behaviour is undefined), so all those parameters
  // can be quoted using any method.
  Result := '';
  while CurPos <= Length(entry^.ExecWithParams) do
  begin
    if entry^.ExecWithParams[CurPos] = '%' then
    begin
      Result := Result + Copy(entry^.ExecWithParams, StartPos, CurPos - StartPos);
      Inc(CurPos);

      if CurPos <= Length(entry^.ExecWithParams) then
        case entry^.ExecWithParams[CurPos] of
          {
          'U':
            begin
              for i := 0 to fileList.Count - 1 do
              begin
                if i <> 0 then
                  Result := Result + ' ';
                Result := Result + QuoteStr(fileScheme + '//' + URIEncode(fileList[i]));
              end;
              filesAdded := True;
            end;
          'u':
            if fileList.Count > 0 then
            begin
              Result := Result + QuoteStr(fileScheme + '//' + URIEncode(fileList[0]));
              filesAdded := True;
            end;
          }
          'F','U':
            begin
              for i := 0 to fileList.Count - 1 do
              begin
                if i <> 0 then
                  Result := Result + ' ';
                Result := Result + QuoteStr(fileList[i]);
              end;
              filesAdded := True;
            end;
          'f','u':
            if fileList.Count > 0 then
            begin
              Result := Result + QuoteStr(fileList[0]);
              filesAdded := True;
            end;
          'N': // deprecated
            begin
              for i := 0 to fileList.Count - 1 do
              begin
                if i <> 0 then
                  Result := Result + ' ';
                Result := Result + QuoteStr(fileList[i]);
              end;
              filesAdded := True;
            end;
          'n': // deprecated
            if fileList.Count > 0 then
            begin
              Result := Result + QuoteStr(fileList[0]);
              filesAdded := True;
            end;
          'D': // deprecated
            begin
              for i := 0 to fileList.Count - 1 do
              begin
                if i <> 0 then
                  Result := Result + ' ';
                Result := Result + QuoteStr(ExtractFilePath(fileList[i]));
              end;
              filesAdded := True;
            end;
          'd': // deprecated
            if fileList.Count > 0 then
            begin
              Result := Result + QuoteStr(ExtractFilePath(fileList[0]));
              filesAdded := True;
            end;
          'i':
            if entry^.IconName <> '' then
              Result := Result + '--icon ' + QuoteStr(entry^.IconName);
          'c':
            Result := Result + QuoteStr(entry^.DisplayName);
          'k':
            Result := Result + QuoteStr(entry^.DesktopFilePath);
          '%':
            Result := Result + '%';
        end;

      Inc(CurPos);
      StartPos := CurPos;
    end
    else
      Inc(CurPos);
  end;

  if (StartPos <> CurPos) then
    Result := Result + Copy(entry^.ExecWithParams, StartPos, CurPos - StartPos);

  if not filesAdded then
  begin
    for i := 0 to fileList.Count - 1 do
      Result := Result + ' ' + QuoteStr(fileList[i]);
  end;
end;

procedure ParseActions(Actions: TDynamicStringArray; var ActionList: TDynamicStringArray);
var
  Action: String;
begin
  for Action in Actions do
  begin
    if Length(GetDesktopPath(Action)) > 0 then
    begin
      if not Contains(ActionList, Action) then
        AddString(ActionList, Action);
    end;
  end;
end;

procedure SetFindPath(var MimeAppsPath: TDynamicStringArray);
const
  APPLICATIONS = 'applications/';
var
  I: Integer;
  Temp: TDynamicStringArray;
begin
  // $XDG_CONFIG_HOME
  AddString(MimeAppsPath, IncludeTrailingBackslash(GetUserConfigDir));
  // $XDG_CONFIG_DIRS
  Temp:= GetSystemConfigDirs;
  for I:= Low(Temp) to High(Temp) do
  begin
    AddString(MimeAppsPath, IncludeTrailingBackslash(Temp[I]));
  end;
  // $XDG_DATA_HOME
  AddString(MimeAppsPath, IncludeTrailingBackslash(GetUserDataDir) + APPLICATIONS);
  // $XDG_DATA_DIRS
  Temp:= GetSystemDataDirs;
  for I:= Low(Temp) to High(Temp) do
  begin
    AddString(MimeAppsPath, IncludeTrailingBackslash(Temp[I]) + APPLICATIONS);
  end;
end;

function ReadMimeAppsList(const MimeType, MimeAppsPath: String; Flags: TMimeAppsGroupSet): TMimeAppsList;
const
  MIME_APPS_LIST = 'mimeapps.list';
var
  J: LongInt;
  FileName: String;
  MimeApps: TKeyFile;
  Actions: TDynamicStringArray;
  MimeAppsFile: TDynamicStringArray;
begin
  // $XDG_CURRENT_DESKTOP
  Actions:= GetCurrentDesktop;
  // Desktop specific configuration
  for J:= Low(Actions) to High(Actions) do
  begin
    AddString(MimeAppsFile, LowerCase(Actions[J]) + '-' + MIME_APPS_LIST);
  end;
  // Common configuration
  AddString(MimeAppsFile, MIME_APPS_LIST);

  for J:= Low(MimeAppsFile) to High(MimeAppsFile) do
  begin
    FileName:= MimeAppsPath + MimeAppsFile[J];
    if mbFileExists(FileName) then
    try
      MimeApps:= TKeyFile.Create(FileName);
      try
        if magDefault in Flags then
        begin
          Actions:= MimeApps.ReadStringList(MIME_APPS[magDefault], MimeType);
          if (Length(Actions) > 0) then ParseActions(Actions, Result.Defaults);
        end;
        if magAdded in Flags then
        begin
          Actions:= MimeApps.ReadStringList(MIME_APPS[magAdded], MimeType);
          if (Length(Actions) > 0) then ParseActions(Actions, Result.Added);
        end;
        if magRemoved in Flags then
        begin
          Actions:= MimeApps.ReadStringList(MIME_APPS[magRemoved], MimeType);
          if (Length(Actions) > 0) then ParseActions(Actions, Result.Removed);
        end;
      finally
        FreeAndNil(MimeApps);
      end;
    except
      // Continue
    end;
  end;
end;

procedure ReadMimeInfoCache(const MimeType, Path: String; out Actions: TDynamicStringArray);
const
  MIME_INFO_CACHE = 'mimeinfo.cache';
var
  MimeCache: TKeyFile;
  FileName: String;
  AValue: TDynamicStringArray;
begin
  FileName:= IncludeTrailingBackslash(Path) + MIME_INFO_CACHE;
  if mbFileExists(FileName) then
  try
    MimeCache:= TKeyFile.Create(FileName);
    try
      AValue:= MimeCache.ReadStringList('MIME Cache', MimeType);
      if (Length(AValue) > 0) then ParseActions(AValue, Actions);
    finally
      FreeAndNil(MimeCache);
    end;
  except
    // Continue
  end;
end;

function GetDesktopEntries(FileNames: TStringList): TList;
var
  Apps: TMimeAppsList;
  Entry: PDesktopFileEntry;
  Path, Action, MimeType: String;
  Actions, MimeTypes: TDynamicStringArray;
  ResultArray, MimeAppsPath: TDynamicStringArray;

  procedure AddAction(const Action: String);
  begin
    Path := GetDesktopPath(Action);
    if Length(Path) > 0 then
    begin
      Entry := GetDesktopEntry(Path);

      if Assigned(Entry) then
      begin
        Entry^.MimeType := MimeType;
        // Set Exec as last because it uses other fields of Entry.
        Entry^.Exec := TranslateAppExecToCmdLine(Entry, Filenames);

        Result.Add(Entry);
      end;
    end;
  end;

begin
  if FileNames.Count = 0 then Exit(nil);

  // Get file mime type
  MimeTypes := GetFileMimeTypes(FileNames[0]);
  if Length(MimeTypes) = 0 then Exit(nil);

  Result := TList.Create;
  SetFindPath(MimeAppsPath);
  SetLength(ResultArray, 0);

  for MimeType in MimeTypes do
  begin
    for Path in MimeAppsPath do
    begin
      // Read actions from mimeapps.list
      Apps:= ReadMimeAppsList(MimeType, Path, [magDefault, magAdded, magRemoved]);

      // Add actions from default group
      for Action in Apps.Defaults do
      begin
        if (not Contains(ResultArray, Action)) and (not Contains(Apps.Removed, Action)) then
          AddString(ResultArray, Action);
      end;

      // Add actions from added group
      for Action in Apps.Added do
      begin
        if (not Contains(ResultArray, Action)) and (not Contains(Apps.Defaults, Action)) then
          AddString(ResultArray, Action);
      end;

      // Read actions from mimeinfo.cache
      ReadMimeInfoCache(MimeType, Path, Actions);

      for Action in Actions do
      begin
        if (not Contains(ResultArray, Action)) and (not Contains(Apps.Removed, Action)) then
        begin
          AddString(ResultArray, Action);
          AddString(Apps.Removed, Action);
        end;
      end;
    end;
  end;
  if HasGio then
  begin
    Actions:= GioMimeTypeGetActions(MimeTypes[0]);
    for Action in Actions do
    begin
      if not Contains(ResultArray, Action) then
        AddString(ResultArray, Action);
    end;
  end;
  // Fill result list
  for Action in ResultArray do
  begin
    AddAction(Action);
  end;
end;

function GetDefaultAppCmd(FileNames: TStringList): String;
var
  I: Integer;
  Action: String;
  Apps: TMimeAppsList;
  MimeType, Path: String;
  Entry: PDesktopFileEntry;
  Actions: TDynamicStringArray;
  MimeTypes: TDynamicStringArray;
  MimeAppsPath: TDynamicStringArray;

  function GetAppExec: String;
  begin
    if Length(Action) > 0 then
    begin
      Path := GetDesktopPath(Action);
      if Length(Path) > 0 then
      begin
        Entry := GetDesktopEntry(Path);

        if Assigned(Entry) then
        begin
          Entry^.MimeType := MimeType;
          // Set Exec as last because it uses other fields of Entry.
          Result := TranslateAppExecToCmdLine(Entry, Filenames);
          Dispose(Entry);
        end;
      end;
    end;
  end;

begin
  Result:= EmptyStr;
  if FileNames.Count = 0 then Exit;

  // Get file mime type
  MimeTypes := GetFileMimeTypes(FileNames[0]);
  if Length(MimeTypes) = 0 then Exit;
  SetFindPath(MimeAppsPath);

  for MimeType in MimeTypes do
  begin
    // Check defaults
    for Path in MimeAppsPath do
    begin
      // Read actions from mimeapps.list
      Apps:= ReadMimeAppsList(MimeType, Path, [magDefault]);
      if Length(Apps.Defaults) > 0 then
      begin
        // First Action is default
        Action:= Apps.Defaults[0];
        Exit(GetAppExec);
      end
    end;
    // Check added
    for Path in MimeAppsPath do
    begin
      // Read actions from mimeapps.list
      Apps:= ReadMimeAppsList(MimeType, Path,  [magAdded]);
      if Length(Apps.Added) > 0 then
      begin
        // First Action is default
        Action:= Apps.Added[0];
        Exit(GetAppExec);
      end;
    end;
    // Check mime info cache
    for Path in MimeAppsPath do
    begin
      // Read actions from mimeinfo.cache
      ReadMimeInfoCache(MimeType, Path, Actions);
      if Length(Actions) > 0 then
      begin
        // Read actions from mimeapps.list
        Apps:= ReadMimeAppsList(MimeType, Path,  [magRemoved]);
        for I:= Low(Actions) to High(Actions) do
        begin
          if not Contains(Apps.Removed, Actions[I]) then
          begin
            Action:= Actions[I];
            Exit(GetAppExec);
          end;
        end;
      end;
    end;
  end; //for
end;

function GetDesktopEntry(const FileName: String): PDesktopFileEntry;
var
  TryExec: String;
  DesktopEntryFile: TKeyFile;
begin
  try
    DesktopEntryFile:= TKeyFile.Create(FileName);
    if not DesktopEntryFile.SectionExists(DESKTOP_GROUP) then
    begin
      DesktopEntryFile.Free;
      Exit(nil);
    end;
    try
      TryExec:= DesktopEntryFile.ReadString(DESKTOP_GROUP, DESKTOP_KEY_TRY_EXEC, EmptyStr);
      if Length(TryExec) > 0 then
      begin
        case GetPathType(TryExec) of
          ptAbsolute: if fpAccess(TryExec, X_OK) <> 0 then Exit(nil);
          ptNone: if not ExecutableInSystemPath(TryExec) then Exit(nil);
        end;
      end;
      New(Result);
      with Result^, DesktopEntryFile do
      begin
        DesktopFilePath := FileName;
        DisplayName     := ReadLocaleString(DESKTOP_GROUP, DESKTOP_KEY_NAME, EmptyStr);
        Comment         := ReadLocaleString(DESKTOP_GROUP, DESKTOP_KEY_COMMENT, EmptyStr);
        ExecWithParams  := ReadString(DESKTOP_GROUP, DESKTOP_KEY_EXEC, EmptyStr);
        IconName        := ReadString(DESKTOP_GROUP, DESKTOP_KEY_ICON, EmptyStr);
        Categories      := ReadStringList(DESKTOP_GROUP, DESKTOP_KEY_CATEGORIES);
        Terminal        := ReadBool(DESKTOP_GROUP, DESKTOP_KEY_TERMINAL, False);
        Hidden          := ReadBool(DESKTOP_GROUP, DESKTOP_KEY_NO_DISPLAY, False);
        {
          Some icon names in .desktop files are specified with an extension,
          even though it is not allowed by the standard unless an absolute path
          to the icon is supplied. We delete this extension here.
        }
        if GetPathType(IconName) = ptNone then
          IconName := TIconTheme.CutTrailingExtension(IconName);
      end;
    finally
      DesktopEntryFile.Free;
    end;
  except
    on E: Exception do
    begin
      Result:= nil;
      WriteLn('GetDesktopEntry: ', E.Message);
    end;
  end;
end;

function AddDesktopEntry(const MimeType, DesktopEntry: String; DefaultAction: Boolean): Boolean;
var
  Value: String;
  CustomFile: String;
  UserDataDir: String;
  DesktopFile: TIniFileEx;
  MimeApps: String = '/mimeapps.list';

  procedure UpdateDesktop(const Group: String);
  begin
    // Read current actions of this mime type
    Value:= DesktopFile.ReadString(Group, MimeType, EmptyStr);
    if (Length(Value) > 0) and (not StrEnds(Value, ';')) then Value += ';';
    if DefaultAction then
    begin
      // Remove chosen action if it exists
      Value:= StringReplace(Value, CustomFile, EmptyStr, [rfReplaceAll]);
      // Save chosen action as first
      DesktopFile.WriteString(Group, MimeType, CustomFile + Value);
    end
    else if (Pos(CustomFile, Value) = 0) then
    begin
      // Save chosen action as last
      DesktopFile.WriteString(Group, MimeType, Value + CustomFile);
    end;
  end;

begin
  CustomFile:= DesktopEntry;
  UserDataDir:= GetUserDataDir;
  mbForceDirectory(UserDataDir + '/applications');
  if (StrEnds(DesktopEntry, '.desktop') = False) then
  begin
    // Create new desktop entry file for user command
    CustomFile:= 'dc_' + ExtractFileName(DesktopEntry) + '_';
    CustomFile:= UserDataDir + '/applications/' + CustomFile;
    CustomFile:= GetTempName(CustomFile) + '.desktop';
    try
      DesktopFile:= TIniFileEx.Create(CustomFile, fmCreate or fmOpenReadWrite);
      try
        DesktopFile.WriteBool(DESKTOP_GROUP, DESKTOP_KEY_NO_DISPLAY, True);
        DesktopFile.WriteString(DESKTOP_GROUP, DESKTOP_KEY_EXEC, DesktopEntry);
        DesktopFile.WriteString(DESKTOP_GROUP, DESKTOP_KEY_MIME_TYPE, MimeType);
        DesktopFile.WriteString(DESKTOP_GROUP, DESKTOP_KEY_NAME, ExtractFileName(DesktopEntry));
        DesktopFile.WriteString(DESKTOP_GROUP, DESKTOP_KEY_TYPE, KEY_FILE_DESKTOP_TYPE_APPLICATION);
        DesktopFile.UpdateFile;
      finally
        DesktopFile.Free;
      end;
      fpSystem('update-desktop-database ' + UserDataDir);
    except
      Exit(False);
    end;
    CustomFile:= ExtractFileName(CustomFile);
  end;
  // Save association in MimeApps
  CustomFile:= CustomFile + ';';
  UserDataDir:= GetUserConfigDir;
  MimeApps:= UserDataDir + MimeApps;
  try
    mbForceDirectory(UserDataDir);
    DesktopFile:= TIniFileEx.Create(MimeApps, fmOpenReadWrite);
    try
      // Update added associations
      UpdateDesktop(MIME_APPS[magAdded]);
      // Set as default action if needed
      if DefaultAction then
      begin
        // Update default applications
        UpdateDesktop(MIME_APPS[magDefault]);
      end;
      DesktopFile.UpdateFile;
      if DesktopEnv = DE_KDE then
        fpSystem('kbuildsycoca5');
    finally
      DesktopFile.Free;
    end;
    Result:= True;
  except
    on E: Exception do
    begin
      Result:= False;
      DCDebug(E.Message);
    end;
  end;
end;

end.
