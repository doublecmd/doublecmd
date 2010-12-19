{
    Double Commander
    -------------------------------------------------------------------------
    Handles actions associated with MIME types in .desktop files.

    Based on FreeDesktop.org specification
    (http://standards.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html)

    Copyright (C) 2009-2010  Przemyslaw Nagay (cobines@gmail.com)

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
  uDCUtils, uIconTheme, uClipboard;

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
function mime_type_get_default_action(mimeType: PChar) : PChar; cdecl; external libmime;
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

function GetDesktopEntries(FileNames: TStringList): TList;
var
  mimeType: PChar;
  actions: PPChar;
  desktopFile: PChar;
  i: Integer;
  app: TCDesktopFileEntry;
  Entry: PDesktopFileEntry;
begin
  if FileNames.Count = 0 then
    Exit(nil);

  Result := TList.Create;

  // This string should not be freed.
  mimeType := mime_type_get_by_filename(PChar(FileNames[0]), nil);

  actions := mime_type_get_actions(mimeType); // retrieve *.desktop identificators

  if actions = nil then
    Exit;  // cannot find any actions for this mime

  i := 0;
  while (actions[i] <> nil) and (actions[i] <> '') do
  begin
    desktopFile := mime_type_locate_desktop_file(nil, actions[i]);
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

    i := i + 1;
  end;

  g_strfreev(actions);
end;

function GetDefaultAppCmd(FileNames: TStringList): UTF8String;
var
  mimeType: PChar;
  action: PChar;
  desktopFile: PChar;
  app: TCDesktopFileEntry;
  Entry: TDesktopFileEntry;
begin
  Result:= EmptyStr;

  if FileNames.Count = 0 then Exit;

  // This string should not be freed.
  mimeType := mime_type_get_by_filename(PChar(FileNames[0]), nil);

  // Get default action for this mime type
  action := mime_type_get_default_action(mimeType);

  if (action <> nil) then
  begin
      desktopFile := mime_type_locate_desktop_file(nil, action);
      if (desktopFile = nil) then Exit; // desktop file not found
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
      g_free(action);
    end;
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
