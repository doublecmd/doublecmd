unit uGlobsPaths;

interface

var
  gpExePath : String = '';  // executable directory
  gpCfgDir : String = '';  // directory from which configuration files are used
  gpGlobalCfgDir : String = '';  // config dir global for all user
  gpLngDir : String = '';  // path to language *.po files
  gpPixmapPath : String = '';  // path to pixmaps
  gpCacheDir : UTF8String = ''; // cache directory
  
procedure LoadPaths;

implementation

uses
  LCLProc, SysUtils, FileUtil, uOSUtils, uFileProcs;

function GetAppName : String;
begin
  Result := 'doublecmd';
end;

procedure LoadPaths;
begin
  OnGetApplicationName := @GetAppName;
  gpExePath := ExtractFilePath(TryReadAllLinks(ParamStrUTF8(0)));
  DebugLn('Executable directory: ', gpExePath);
  
  gpGlobalCfgDir := gpExePath;
  gpCfgDir := GetAppConfigDir;
  if gpCfgDir <> EmptyStr then
    begin
      gpCfgDir := IncludeTrailingPathDelimiter(gpCfgDir);
      if not mbDirectoryExists(gpCfgDir) then
        mbForceDirectory(gpCfgDir);
    end
  else
    DebugLn('Warning: Cannot get user config directory.');

  gpLngDir := gpExePath + 'language' + DirectorySeparator;
  gpPixmapPath := gpExePath + 'pixmaps' + DirectorySeparator;
  gpCacheDir := GetAppCacheDir;

  // set up environment variables
  mbSetEnvironmentVariable('commander_path', ExcludeTrailingBackslash(gpExePath));
end;

end.