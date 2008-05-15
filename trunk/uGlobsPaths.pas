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
  LCLProc, SysUtils, IniFiles;

function GetAppName : String;
begin
  Result := 'doublecmd';
end;

procedure LoadPaths;
var
  Ini : TIniFile;
begin
  gpExePath := ExtractFilePath(ParamStr(0));
  DebugLn('Executable directory: ', gpExePath);
  
  gpCfgDir := gpExePath;
  
  Ini := TIniFile.Create(gpCfgDir + 'doublecmd.ini');
  if Ini.ReadInteger('Configuration', 'UseIniInProgramDir', 1)  = 1 then // use ini file from program dir
    begin
      gpIniDir := gpCfgDir;
    end
  else  
    begin
      OnGetApplicationName := @GetAppName;
      gpIniDir := GetAppConfigDir(False);
      if not DirectoryExists(gpIniDir) then
        ForceDirectories(gpIniDir);
      OnGetApplicationName := nil;
      gpIniDir := IncludeTrailingPathDelimiter(gpIniDir);  // add if need path delimiter
    end;
  Ini.Free;
	
  gpLngDir := gpExePath + 'language' + DirectorySeparator;
  gpPixmapPath := gpExePath + 'pixmaps' + DirectorySeparator;
end;

end.
