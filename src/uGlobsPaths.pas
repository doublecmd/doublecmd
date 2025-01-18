unit uGlobsPaths;

interface

var
  gpExePath : String = '';  // executable directory
  gpCfgDir : String = '';  // directory from which configuration files are used
  gpGlobalCfgDir : String = '';  // config dir global for all user
  gpCmdLineCfgDir : String = ''; // config dir passed on the command line
  gpLngDir : String = '';  // path to language *.po files
  gpPixmapPath : String = '';  // path to pixmaps
  gpHighPath : String = ''; // editor highlighter directory
  gpCacheDir : String = ''; // cache directory
  gpThumbCacheDir : String = ''; // thumbnails cache directory

//Global Configuration Filename
const
  gcfExtensionAssociation : string = 'extassoc.xml';
  
procedure LoadPaths;
procedure UpdateEnvironmentVariable;

implementation

uses
  SysUtils, LazFileUtils, uDebug, DCOSUtils, DCStrUtils, uSysFolders;

var
  gpExeFile: String;

function GetAppName : String;
begin
  Result := 'doublecmd';
end;

procedure UpdateEnvironmentVariable;
begin
  mbSetEnvironmentVariable('COMMANDER_INI', gpCfgDir + 'doublecmd.xml');
  mbSetEnvironmentVariable('DC_CONFIG_PATH', ExcludeTrailingPathDelimiter(gpCfgDir));
  mbSetEnvironmentVariable('COMMANDER_INI_PATH', ExcludeTrailingPathDelimiter(gpCfgDir));
end;

procedure LoadPaths;
begin
  OnGetApplicationName := @GetAppName;

  if gpCmdLineCfgDir <> EmptyStr then
  begin
    if GetPathType(gpCmdLineCfgDir) <> ptAbsolute then
      gpCmdLineCfgDir := IncludeTrailingPathDelimiter(mbGetCurrentDir) + gpCmdLineCfgDir;
    gpCmdLineCfgDir := ExpandAbsolutePath(gpCmdLineCfgDir);
    gpCfgDir := gpCmdLineCfgDir;
  end
  else if mbFileExists(gpGlobalCfgDir + 'doublecmd.inf') then
    gpCfgDir := gpGlobalCfgDir
  else begin
    gpCfgDir := GetAppConfigDir;
    if gpCfgDir = EmptyStr then
    begin
      DCDebug('Warning: Cannot get user config directory.');
      gpCfgDir := gpGlobalCfgDir;
    end;
  end;
  if gpCfgDir <> gpGlobalCfgDir then
    gpCacheDir := GetAppCacheDir
  else begin
    gpCacheDir := gpExePath + 'cache';
  end;
  DCDebug('Executable directory: ', gpExePath);
  DCDebug('Configuration directory: ', gpCfgDir);
  DCDebug('Global configuration directory: ', gpGlobalCfgDir);

  gpCfgDir := IncludeTrailingPathDelimiter(gpCfgDir);
  gpLngDir := gpExePath + 'language' + DirectorySeparator;
  gpPixmapPath := gpExePath + 'pixmaps' + DirectorySeparator;
  gpHighPath:= gpExePath + 'highlighters' + DirectorySeparator;
  gpThumbCacheDir := gpCacheDir + PathDelim + 'thumbnails';

  // set up environment variables
  UpdateEnvironmentVariable;
  mbSetEnvironmentVariable('COMMANDER_EXE', gpExeFile);
  mbSetEnvironmentVariable('COMMANDER_DRIVE', ExtractRootDir(gpExePath));
  mbSetEnvironmentVariable('COMMANDER_PATH', ExcludeTrailingBackslash(gpExePath));
end;

procedure Initialize;
begin
  gpExeFile := ParamStr(0);
  gpExeFile := TryReadAllLinks(gpExeFile);
  gpExePath := ExtractFilePath(gpExeFile);
  gpGlobalCfgDir := gpExePath + 'settings' + DirectorySeparator;
end;

initialization
  Initialize;

end.
