unit uShellContextMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function GetGnomeTemplateMenu(out Items: TStringList): Boolean;

implementation

uses
  LCLProc, IniFiles, uTypes, uFindEx, uDCUtils, uOSUtils;

function GetGnomeTemplateMenu(out Items: TStringList): Boolean;
var
  userDirs: TStringList = nil;
  templateDir: UTF8String;
  searchRec: TSearchRecEx;
begin
  Result:= False;
  try
    userDirs:= TStringList.Create;
    userDirs.LoadFromFile(GetHomeDir + '.config/user-dirs.dirs');
    templateDir:= userDirs.Values['XDG_TEMPLATES_DIR'];
    if Length(templateDir) = 0 then Exit;
    templateDir:= IncludeTrailingPathDelimiter(mbExpandFileName(TrimQuotes(templateDir)));
    if mbDirectoryExists(templateDir) then
    begin
      if FindFirstEx(templateDir, faAnyFile, searchRec) = 0 then
      begin
        Items:= TStringList.Create;
        repeat
          // Skip directories
          if FPS_ISDIR(searchRec.Attr) then Continue;

          Items.Add(ExtractOnlyFileName(searchRec.Name) + '=' + templateDir + searchRec.Name);
        until FindNextEx(searchRec) <> 0;
        Result:= Items.Count > 0;
      end;
      FindCloseEx(searchRec);
    end;
  finally
    if Items.Count = 0 then
      FreeThenNil(Items);
    FreeThenNil(userDirs);
  end;
end;

end.

