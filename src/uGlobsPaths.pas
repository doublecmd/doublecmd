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
  gpThumbCacheDir : String = ''; // thumbnails cache directory

//Global Configuration Filename
const
  gcfExtensionAssociation : string = 'extassoc.xml';
  
procedure LoadPaths;
procedure UpdateEnvironmentVariable;

implementation

uses
  SysUtils, LazFileUtils, uDebug, uOSUtils, DCOSUtils, DCStrUtils, uSysFolders;

function GetAppName : String;
begin
  Result := 'doublecmd';
end;

procedure UpdateEnvironmentVariable;
begin
  mbSetEnvironmentVariable('COMMANDER_INI', gpCfgDir + 'doublecmd.xml');
end;

procedure LoadPaths;
begin
  OnGetApplicationName := @GetAppName;
  gpExePath := ExtractFilePath(TryReadAllLinks(ParamStrU(0)));
  DCDebug('Executable directory: ', gpExePath);
  
  gpGlobalCfgDir := gpExePath;
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

  gpCfgDir := IncludeTrailingPathDelimiter(gpCfgDir);
  gpLngDir := gpExePath + 'language' + DirectorySeparator;
  gpPixmapPath := gpExePath + 'pixmaps' + DirectorySeparator;
  gpHighPath:= gpExePath + 'highlighters' + DirectorySeparator;
  gpThumbCacheDir := GetAppCacheDir + PathDelim + 'thumbnails';

  // set up environment variables
  UpdateEnvironmentVariable;
  mbSetEnvironmentVariable('COMMANDER_DRIVE', ExtractRootDir(gpExePath));
  mbSetEnvironmentVariable('COMMANDER_PATH', ExcludeTrailingBackslash(gpExePath));
end;

end.
