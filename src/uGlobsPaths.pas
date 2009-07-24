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
  LCLProc, SysUtils, FileUtil, uClassesEx, uFileProcs, uOSUtils;

function GetAppName : String;
begin
  Result := 'doublecmd';
end;

procedure LoadPaths;
var
  Ini : TIniFileEx;
begin
  OnGetApplicationName := @GetAppName;
  gpExePath := ExtractFilePath(TryReadAllLinks(ParamStrUTF8(0)));
  DebugLn('Executable directory: ', gpExePath);
  
  gpCfgDir := gpExePath;
  
  Ini := TIniFileEx.Create(gpCfgDir + 'doublecmd.ini', fmOpenRead);
  if Ini.ReadInteger('Configuration', 'UseIniInProgramDir', 0)  = 1 then // use ini file from program dir
    begin
      gpIniDir := gpCfgDir;
    end
  else  
    begin
      gpIniDir := GetAppConfigDir;//(False);
      if not mbDirectoryExists(gpIniDir) then
        mbForceDirectory(gpIniDir);
      gpIniDir := IncludeTrailingPathDelimiter(gpIniDir);  // add if need path delimiter
    end;
  Ini.Free;
	
  gpLngDir := gpExePath + 'language' + DirectorySeparator;
  gpPixmapPath := gpExePath + 'pixmaps' + DirectorySeparator;
  // set up environment variables
  mbSetEnvironmentVariable('commander_path', ExcludeTrailingBackslash(gpExePath));
end;

end.
