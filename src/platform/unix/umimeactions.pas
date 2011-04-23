{
    Double Commander
    -------------------------------------------------------------------------
    Handles actions associated with MIME types in .desktop files.

    Based on FreeDesktop.org specifications
    (http://standards.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html)
    (http://www.freedesktop.org/wiki/Specifications/mime-actions-spec)

    Copyright (C) 2009-2010  Przemyslaw Nagay (cobines@gmail.com)
    Copyright (C) 2011  Koblov Alexander (Alexx2000@mail.ru)

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

unit uMimeActions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, // for AnsiString version of StrPas function
  glib2;

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
function GetDefaultAppCmd(FileNames: TStringList): UTF8String;
{en
   Get file MIME type.
   Returns a file MIME type.
}
function GetFileMimeType(const FileName: UTF8String): UTF8String;

implementation

uses
  uClassesEx, uDCUtils, uIconTheme, uClipboard, uOSUtils;

type
  TCDesktopFileEntry = record
    DisplayName: PChar;
    Comment: PChar;
    Exec: PChar;
    IconName: PChar;
    Terminal: gboolean;
    Hidden: gboolean;
  end;

const  
  libmime = 'libmime';

procedure mime_type_init; cdecl; external libmime;
procedure mime_type_finalize; cdecl; external libmime;
function mime_type_get_by_filename(filename: PChar; stat: Pointer) : PChar; cdecl; external libmime;
function mime_type_get_actions(mimeType: PChar): PPChar; cdecl; external libmime;
function mime_type_locate_desktop_file(DirectoryToCheck: PChar; DesktopFileId: PChar): PChar; cdecl; external libmime;
function mime_get_desktop_entry(DesktopFileName: PChar): TCDesktopFileEntry; cdecl; external libmime;

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
          'F':
            begin
              for i := 0 to fileList.Count - 1 do
              begin
                if i <> 0 then
                  Result := Result + ' ';
                Result := Result + QuoteStr(fileList[i]);
              end;
              filesAdded := True;
            end;
          'f':
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

procedure ReadMimeAppsList(const mimeType: String; out Added, Removed: TStringList);
const
  mimeApps1 = '.local/share/applications/mimeapps.list';
  mimeApps2 = '/usr/share/applications/mimeapps.list';
var
  I: LongInt;
  sTemp: String;
  mimeApps: TIniFileEx = nil;
  mimeAppsList: array[1..2] of String = (mimeApps1, mimeApps2);

  function ParseActions(const Actions: String; out ActionList: TStringList): Boolean;
  var
    startIndex,
    finishIndex: LongInt;
    action: String;
    desktopFile: PChar = nil;
  begin
    startIndex:= 1;
    for finishIndex:= 1 to Length(Actions) do
    if (Actions[finishIndex] = ';') then
    begin
      action:= Copy(Actions, startIndex, finishIndex - startIndex);
      desktopFile := mime_type_locate_desktop_file(nil, PChar(action));
      if (desktopFile <> nil) then
      begin
        if (ActionList.IndexOf(action) < 0) then
          ActionList.Add(action);
        g_free(desktopFile);
      end;
      startIndex:= finishIndex + 1;
    end;
  end;

begin
  Added:= TStringList.Create;
  Removed:= TStringList.Create;
  mimeAppsList[1]:= GetHomeDir + mimeAppsList[1];
  for I:= Low(mimeAppsList) to High(mimeAppsList) do
  if (mbFileExists(mimeAppsList[I])) then
  try
    mimeApps:= TIniFileEx.Create(mimeAppsList[I], fmOpenRead or fmShareDenyNone);
    sTemp:= mimeApps.ReadString('Added Associations', mimeType, EmptyStr);
    if (Length(sTemp) <> 0) then ParseActions(sTemp, Added);
    sTemp:= mimeApps.ReadString('Removed Associations', mimeType, EmptyStr);
    if (Length(sTemp) <> 0) then ParseActions(sTemp, Removed);
  finally
    if Assigned(mimeApps) then
      FreeAndNil(mimeApps);
  end;
end;

function GetDesktopEntries(FileNames: TStringList): TList;
var
  mimeType: PChar;
  actions: PPChar;
  desktopFile: PChar;
  i: Integer;
  app: TCDesktopFileEntry;
  Entry: PDesktopFileEntry;
  Added, Removed: TStringList;

  procedure AddAction(action: PChar);
  begin
    desktopFile := mime_type_locate_desktop_file(nil, action);
    app := mime_get_desktop_entry(desktopFile);

    New(Entry);

    Entry^.DesktopFilePath := StrPas(desktopFile);
    Entry^.MimeType := StrPas(mimeType);
    Entry^.DisplayName := StrPas(app.DisplayName);
    Entry^.Comment := StrPas(app.Comment);
    Entry^.ExecWithParams := StrPas(app.Exec);
    Entry^.IconName := StrPas(app.IconName);
    Entry^.Terminal := app.Terminal;
    Entry^.Hidden := app.Hidden;
    // Set Exec as last because it uses other fields of Entry.
    Entry^.Exec := TranslateAppExecToCmdLine(Entry, Filenames);

    {
      Some icon names in .desktop files are specified with an extension,
      even though it is not allowed by the standard unless an absolute path
      to the icon is supplied. We delete this extension here.
    }
    if GetPathType(Entry^.IconName) = ptNone then
      Entry^.IconName := TIconTheme.CutTrailingExtension(Entry^.IconName);

    Result.Add(Entry);

    g_free(desktopFile);
    g_free(app.DisplayName);
    g_free(app.Comment);
    g_free(app.Exec);
    g_free(app.IconName);
  end;

begin
  if FileNames.Count = 0 then
    Exit(nil);

  Result := TList.Create;

  // This string should not be freed.
  mimeType := mime_type_get_by_filename(PChar(FileNames[0]), nil);

  // Retrieve *.desktop identificators
  actions := mime_type_get_actions(mimeType);

  // Read actions from mimeapps.list
  ReadMimeAppsList(mimeType, Added, Removed);

  // Add actions from mimeapps.list
  for i := 0 to Added.Count - 1 do
    AddAction(PChar(Added[i]));

  // If find any actions for this mime
  if actions <> nil then
  begin
    i := 0;
    while (actions[i] <> nil) and (actions[i] <> '') do
    begin
      // Don't add actions where already in mimeapps.list
      if (Added.IndexOf(actions[i]) < 0) and (Removed.IndexOf(actions[i]) < 0) then
        AddAction(actions[i]);

      i := i + 1;
    end;
  end;

  // Free resources
  FreeAndNil(Added);
  FreeAndNil(Removed);
  if (actions <> nil) then
    g_strfreev(actions);
end;

function GetDefaultAppCmd(FileNames: TStringList): UTF8String;
var
  i: Integer = 0;
  mimeType: PChar = nil;
  action: PChar = nil;
  actions: PPChar = nil;
  desktopFile: PChar;
  app: TCDesktopFileEntry;
  Entry: TDesktopFileEntry;
  Added, Removed: TStringList;
begin
  Result:= EmptyStr;

  if FileNames.Count = 0 then Exit;

  // This string should not be freed.
  mimeType := mime_type_get_by_filename(PChar(FileNames[0]), nil);

  // Read actions from mimeapps.list
  ReadMimeAppsList(mimeType, Added, Removed);

  if (Added.Count > 0) then
    begin
      // First action is default
      action:= PChar(Added[0]);
    end
  else
    begin
      // Retrieve *.desktop identificators
      actions := mime_type_get_actions(mimeType);

      // If find any actions for this mime
      if (actions <> nil) then
      repeat
        action := actions[i];
        inc(i);
      until ((action <> nil) or (action <> EmptyStr)) and (Removed.IndexOf(action) < 0);
    end;

  if (action <> nil) then
  begin
    desktopFile := mime_type_locate_desktop_file(nil, action);
    app := mime_get_desktop_entry(desktopFile);

    Entry.DesktopFilePath := StrPas(desktopFile);
    Entry.MimeType := StrPas(mimeType);
    Entry.DisplayName := StrPas(app.DisplayName);
    Entry.Comment := StrPas(app.Comment);
    Entry.ExecWithParams := StrPas(app.Exec);
    Entry.IconName := StrPas(app.IconName);
    Entry.Terminal := app.Terminal;
    Entry.Hidden := app.Hidden;
    // Set Exec as last because it uses other fields of Entry.
    Result := TranslateAppExecToCmdLine(@Entry, Filenames);

    g_free(desktopFile);
    g_free(app.DisplayName);
    g_free(app.Comment);
    g_free(app.Exec);
    g_free(app.IconName);
  end;

  // Free resources
  FreeAndNil(Added);
  FreeAndNil(Removed);
  if (actions <> nil) then
    g_strfreev(actions)
end;

function GetFileMimeType(const FileName: UTF8String): UTF8String;
var
  mimeType: PChar;
begin
  // This string should not be freed.
  mimeType := mime_type_get_by_filename(PChar(FileName), nil);
  Result:= StrPas(mimeType);
end;

initialization
  mime_type_init;

finalization
  mime_type_finalize();

end.
