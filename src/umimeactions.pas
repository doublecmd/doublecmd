unit uMimeActions;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  glib2;

type
  PDesktopFileEntry = ^TDesktopFileEntry;
  TDesktopFileEntry = record
    FileName: String;
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

implementation

type
  PCDesktopFileEntry = ^TCDesktopFileEntry;
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
function translate_app_exec_to_command_line(const app: PCDesktopFileEntry; file_list: PGList): PChar; cdecl; external libmime;

function GetDesktopEntries(FileNames: TStringList): TList;
var
  mimeType: PChar;
  actions: PPChar;
  desktopFile: PChar;
  i, j: Integer;
  app: TCDesktopFileEntry;
  list: PGList = nil;
  exec: PChar;
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
    for j := 0 to FileNames.Count - 1 do
    begin
      list := g_list_append(list, PChar(FileNames[j]));
    end;

    desktopFile := mime_type_locate_desktop_file(nil, actions[i]);
    app := mime_get_desktop_entry(desktopFile);
    exec := translate_app_exec_to_command_line(@app, list);

    New(Entry);

    Entry^.DisplayName := StrPas(app.DisplayName);
    Entry^.Comment := StrPas(app.Comment);
    Entry^.Exec := StrPas(Exec);
    Entry^.IconName := StrPas(app.IconName);
    Entry^.Terminal := app.Terminal;
    Entry^.Hidden := app.Hidden;

    Result.Add(Entry);

    g_free(exec);
    g_free(desktopFile);
    g_free(app.DisplayName);
    g_free(app.Comment);
    g_free(app.Exec);
    g_free(app.IconName);
    g_list_free(list);
    list := nil;

    i := i + 1;
  end;

  g_strfreev(actions);
end;

initialization
  mime_type_init;

finalization
  mime_type_finalize();

end.
