unit uGlobsPaths;

interface
var

  gpExePath : String = '';  // executable directory
  gpIniDir : String = '';  // config dir local for user
  gpCfgDir : String = '';  // config dir global for all user
  gpLngDir : String = '';  // path to language *.po files
  gpPixmapPath : String = '';  // path to pixmaps
  
procedure LoadPaths;

implementation
uses
  LCLProc, SysUtils;

function GetAppName : String;
begin
  Result := 'doublecmd';
end;

procedure LoadPaths;
begin
  OnGetApplicationName := @GetAppName;
  gpIniDir := GetAppConfigDir(False);
  if not DirectoryExists(gpIniDir) then
    ForceDirectories(gpIniDir);
  OnGetApplicationName := nil;
  gpIniDir := IncludeTrailingPathDelimiter(gpIniDir);  // add if need path delimiter
  
  gpExePath := ExtractFilePath(ParamStr(0));
  DebugLn('Executable directory: ', gpExePath);

  gpCfgDir := gpExePath;
  gpLngDir := gpExePath + 'language' + DirectorySeparator;
  gpPixmapPath := gpExePath + 'pixmaps' + DirectorySeparator;
end;

end.
