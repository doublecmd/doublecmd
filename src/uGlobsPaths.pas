unit uGlobsPaths;

interface

var
  gpExePath : String = '';  // executable directory
  gpCfgDir : String = '';  // directory from which configuration files are used
  gpGlobalCfgDir : String = '';  // config dir global for all user
  gpCmdLineCfgDir : String = ''; // config dir passed on the command line
  gpLngDir : String = '';  // path to language *.po files
  gpPixmapPath : String = '';  // path to pixmaps
  gpCacheDir : UTF8String = ''; // cache directory
  
procedure LoadPaths;

implementation

uses
  SysUtils, FileUtil, uDebug, uOSUtils, uDCUtils;

function GetAppName : String;
begin
  Result := 'doublecmd';
end;

procedure LoadPaths;
begin
  OnGetApplicationName := @GetAppName;
  gpExePath := ExtractFilePath(TryReadAllLinks(ParamStrUTF8(0)));
  DCDebug('Executable directory: ', gpExePath);
  
  gpGlobalCfgDir := gpExePath;
  if gpCmdLineCfgDir <> EmptyStr then
  begin
    if GetPathType(gpCmdLineCfgDir) <> ptAbsolute then
      gpCmdLineCfgDir := IncludeTrailingPathDelimiter(mbGetCurrentDir) + gpCmdLineCfgDir;
    gpCmdLineCfgDir := ExpandAbsolutePath(gpCmdLineCfgDir);
    gpCfgDir := gpCmdLineCfgDir;
  end
  else
  begin
    gpCfgDir := GetAppConfigDir;
    if gpCfgDir = EmptyStr then
    begin
      DCDebug('Warning: Cannot get user config directory.');
      gpCfgDir := gpGlobalCfgDir;
    end;
  end;

  gpCfgDir := IncludeTrailingPathDelimiter(gpCfgDir);
  gpLngDir := gpExePath + 'language' + DirectorySeparator;
  gpPixmapPath := gpExePath + 'pixmaps' + DirectorySeparator;
  gpCacheDir := GetAppCacheDir;

  // set up environment variables
  mbSetEnvironmentVariable('commander_path', ExcludeTrailingBackslash(gpExePath));
end;

end.