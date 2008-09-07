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
  LCLProc, SysUtils, uClassesEx, uFileProcs, uOSUtils;

function GetAppName : String;
begin
  Result := 'doublecmd';
end;

procedure LoadPaths;
var
  Ini : TIniFileEx;
begin
  gpExePath := ExtractFilePath(ParamStr(0));
  DebugLn('Executable directory: ', gpExePath);
  
  gpCfgDir := gpExePath;
  
  Ini := TIniFileEx.Create(gpCfgDir + 'doublecmd.ini');
  if Ini.ReadInteger('Configuration', 'UseIniInProgramDir', 0)  = 1 then // use ini file from program dir
    begin
      gpIniDir := gpCfgDir;
    end
  else  
    begin
      OnGetApplicationName := @GetAppName;
      gpIniDir := GetAppConfigDir;//(False);
      if not mbDirectoryExists(gpIniDir) then
        ForceDirectory(gpIniDir);
      OnGetApplicationName := nil;
      gpIniDir := IncludeTrailingPathDelimiter(gpIniDir);  // add if need path delimiter
    end;
  Ini.Free;
	
  gpLngDir := gpExePath + 'language' + DirectorySeparator;
  gpPixmapPath := gpExePath + 'pixmaps' + DirectorySeparator;
end;

end.
