{
  Notes:
  1. currently implementing:
     DropBox
     Yandex
     One Drive
     Box
     Amazon S3 and Compatible
  2. other cloud drivers will be gradually supported
}

unit uWFXFunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WfxPlugin, Extension,
  uCloudDriver, uCloudRootDriver,
  uWFXPlugin, uWFXConfig, uWFXOptionsCommonRS, uWFXOptionsWindow, uWFXUtil,
  uMiniUtil;

procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); cdecl;
function FsInitW(PluginNr:integer;pProgressProc:tProgressProcW;pLogProc:tLogProcW;pRequestProc:tRequestProcW):integer; cdecl;
function FsFindFirstW(path:pwidechar;var FindData:tWIN32FINDDATAW):thandle; cdecl;
function FsFindNextW(handle:thandle;var FindData:tWIN32FINDDATAW):bool; cdecl;
function FsFindClose(handle:thandle):integer; cdecl;
function FsGetFileW(RemoteName,LocalName:pwidechar;CopyFlags:integer;RemoteInfo:pRemoteInfo):integer; cdecl;
function FsPutFileW(LocalName,RemoteName:pwidechar;CopyFlags:integer):integer; cdecl;
function FsMkDirW(RemoteDir:pwidechar):bool; cdecl;
function FsDeleteFileW(RemoteName:pwidechar):bool; cdecl;
function FsRemoveDirW(RemoteName:pwidechar):bool; cdecl;
function FsRenMovFileW(OldName,NewName:pwidechar;Move,OverWrite:bool;RemoteInfo:pRemoteInfo):integer; cdecl;
function FsExecuteFileW(MainWin:HWND;RemoteName,Verb:pwidechar):integer; cdecl;
function FsGetBackgroundFlags:integer; cdecl;
procedure FsGetDefRootName(DefRootName:pchar;maxlen:integer); cdecl;

implementation

procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); cdecl;
var
  configPath: String;
begin
  try
    configPath:= StartupInfo^.PluginConfDir + 'MacCloud.json';
    if WFXMacCloudPlugin <> nil then begin
      WFXMacCloudPlugin.configPath:= configPath;
      WFXMacCloudPlugin.pluginPath:= StartupInfo^.PluginDir;
      WFXMacCloudPlugin.languageID:= StartupInfo^.LanguageID;
      WFXMacCloudPlugin.TranslateResourceStrings(StartupInfo);
      TWFXCloudDriverConfigManager.initMacCloudDriverManager;
      WFXCloudDriverConfigManager.loadFromCommon( configPath );
      WFXCloudDriverConfigManager.loadFromSecurity;
    end;
  except
    on e: Exception do
      TLogUtil.logError( 'error in ExtensionInitialize(): ' + e.Message );
  end;
end;

function FsInitW(
  PluginNr: Integer;
  pProgressProc: TProgressProcW;
  pLogProc: TLogProcW;
  pRequestProc: TRequestProcW ): Integer; cdecl;
begin
  if Assigned(WFXMacCloudPlugin) then
    WFXMacCloudPlugin.Free;
  WFXMacCloudPlugin:= TWFXMacCloudPlugin.Create( PluginNr, pProgressProc, pLogProc );
  Result:= 0;
end;

function FsFindFirstW(
  path: pwidechar;
  var FindData: TWIN32FINDDATAW ): THandle; cdecl;
var
  parser: TWFXPathParser = nil;
  lister: TCloudDriverLister = nil;
  driver: TCloudDriverBase;

  function doFindFirst: THandle;
  var
    utf8Path: String;
    cloudFile: TCloudFile;
  begin
    utf8Path:= TStringUtil.widecharsToString(path);
    parser:= TWFXPathParser.Create( utf8Path );
    if utf8Path = PathDelim then
      driver:= TCloudRootDriver.Create
    else
      driver:= parser.driver;
    lister:= driver.createLister( parser.driverPath );
    Result:= THandle( lister );
    lister.listFolderBegin;
    cloudFile:= lister.listFolderGetNextFile;
    if cloudFile = nil then
      Exit( wfxInvalidHandle );

    TWFXPluginUtil.cloudFileToWinFindData( cloudFile, FindData );
  end;

begin
  try
    try
      Result:= doFindFirst;
    finally
      FreeAndNil( parser );
    end;
  except
    on e: Exception do begin
      TWFXPluginUtil.exceptionToResult( e );
      Result:= wfxInvalidHandle;
    end;
  end;

  if (Result=wfxInvalidHandle) and Assigned(lister) then
    lister.listFolderEnd;
end;

function FsFindNextW(
  handle: THandle;
  var FindData:tWIN32FINDDATAW ): Bool; cdecl;
var
  lister: TCloudDriverLister;
  cloudFile: TCloudFile;
begin
  try
    lister:= TCloudDriverLister( handle );
    cloudFile:= lister.listFolderGetNextFile;
    if cloudFile = nil then
      Exit( False );

    TWFXPluginUtil.cloudFileToWinFindData( cloudFile, FindData );
    Result:= True;
  except
    on e: Exception do begin
      TWFXPluginUtil.exceptionToResult( e );
      Result:= False;
    end;
  end;
end;

function FsFindClose( handle: THandle ): Integer; cdecl;
var
  lister: TCloudDriverLister;
begin
  Result:= 0;
  try
    lister:= TCloudDriverLister( handle );
    lister.listFolderEnd;
  except
    on e: Exception do begin
      TWFXPluginUtil.exceptionToResult( e );
    end;
  end;
end;

function FsGetFileW(
  RemoteName: pwidechar;
  LocalName: pwidechar;
  CopyFlags: Integer;
  RemoteInfo: pRemoteInfo ): Integer; cdecl;

var
  parser: TWFXPathParser = nil;
  callback: TWFXProgressCallback = nil;

  function doGetFile: Integer;
  var
    serverPath: String;
    localPath: String;
    totalBytes: Integer;
    li: ULARGE_INTEGER;
    exits: Boolean;
  begin
    parser:= TWFXPathParser.Create( TStringUtil.widecharsToString(RemoteName) );
    serverPath:= parser.driverPath;
    if serverPath = EmptyStr then
      raise ENotSupportedException.Create( 'Connection not support copying' );
    localPath:= TStringUtil.widecharsToString( LocalName );
    li.LowPart:= RemoteInfo^.SizeLow;
    li.HighPart:= RemoteInfo^.SizeHigh;
    totalBytes:= li.QuadPart;
    exits:= TFileUtil.exists( localPath );

    TLogUtil.logInformation(
      'FsGetFileW: remote=' + serverPath + ', local=' + localPath +
      ', CopyFlags=' + IntToStr(CopyFlags) + ', size=' + IntToStr(totalBytes) +
      ', LocalFileExists=' + BoolToStr(exits,True) );

    if (CopyFlags and FS_COPYFLAGS_RESUME <> 0) then
      Exit( FS_FILE_NOTSUPPORTED );
    if exits and (CopyFlags and FS_COPYFLAGS_OVERWRITE = 0) then
      Exit( FS_FILE_EXISTS );

    callback:= TWFXProgressCallback.Create(
      RemoteName,
      LocalName,
      totalBytes );
    callback.progress( 0 );
    parser.driver.download( serverPath, localPath, callback );

    Result:= FS_FILE_OK;
  end;

begin
  try
    try
      Result:= doGetFile;
    finally
      FreeAndNil( parser );
      FreeAndNil( callback );
    end;
  except
    on e: Exception do
      Result:= TWFXPluginUtil.exceptionToResult( e );
  end;
end;

function FsPutFileW(
  LocalName: pwidechar;
  RemoteName: pwidechar;
  CopyFlags: Integer ): Integer; cdecl;
const
  FS_EXISTS = FS_COPYFLAGS_EXISTS_SAMECASE or FS_COPYFLAGS_EXISTS_DIFFERENTCASE;
var
  parser: TWFXPathParser = nil;
  callback: TWFXProgressCallback = nil;

  function doPutFile: Integer;
  var
    serverPath: String;
    localPath: String;
    totalBytes: Integer;
    exits: Boolean;
  begin
    parser:= TWFXPathParser.Create( TStringUtil.widecharsToString(RemoteName) );
    serverPath:= parser.driverPath;
    if serverPath = EmptyStr then
      raise ENotSupportedException.Create( 'Connection not support copying' );

    localPath:= TStringUtil.widecharsToString( LocalName );
    totalBytes:= TFileUtil.filesize( localPath );
    exits:= (CopyFlags and FS_EXISTS <> 0);

    TLogUtil.logInformation(
      'FsPutFileW: remote=' + serverPath + ', local=' + localPath +
      ', CopyFlags=' + IntToStr(CopyFlags) + ', size=' + IntToStr(totalBytes) +
      ', RemoteFileExists=' + BoolToStr(exits,True) );

    if (CopyFlags and FS_COPYFLAGS_RESUME <> 0) then
      Exit( FS_FILE_NOTSUPPORTED );
    if exits and (CopyFlags and FS_COPYFLAGS_OVERWRITE = 0) then
      Exit( FS_FILE_EXISTS );

    callback:= TWFXProgressCallback.Create(
      LocalName,
      RemoteName,
      totalBytes );
    callback.progress( 0 );
    parser.driver.upload( serverPath, localPath, callback );

    Result:= FS_FILE_OK;
  end;

begin
  try
    try
      Result:= doPutFile;
    finally
      FreeAndNil( parser );
      FreeAndNil( callback );
    end;
  except
    on e: Exception do
      Result:= TWFXPluginUtil.exceptionToResult( e );
  end;
end;

function FsMkDirW( RemoteDir: pwidechar ): Bool; cdecl;
var
  parser: TWFXPathParser = nil;

  procedure doCreateFolder;
  var
    folderName: String;
  begin
    parser:= TWFXPathParser.Create( TStringUtil.widecharsToString(RemoteDir) );
    if parser.driverPath = EmptyStr then
      folderName:= parser.connectionName
    else
      folderName:= parser.driverPath;
    TCloudRootHelper.getDriver(parser).createFolder( folderName );
  end;

begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then begin
    TLogUtil.logError( 'Not Called in Main Thread!' );
    Exit( False );
  end;

  try
    try
      doCreateFolder;
      Result:= True;
    finally
      FreeAndNil( parser );
    end;
  except
    on e: Exception do begin
      TWFXPluginUtil.exceptionToResult( e );
      Result:= False;
    end;
  end;
end;


function fsDelete( const RemoteName: pwidechar; const isFolder: Boolean ): Boolean;
var
  parser: TWFXPathParser = nil;

  procedure doDelete;
  var
    utf8Path: String;
  begin
    utf8Path:= TStringUtil.widecharsToString(RemoteName);
    parser:= TWFXPathParser.Create( utf8Path );
    if parser.driverPath = EmptyStr then
      utf8Path:= parser.connectionName
    else
      utf8Path:= parser.driverPath;
    TCloudRootHelper.getDriver(parser).delete( utf8Path, isFolder );
  end;

begin
  try
    try
      doDelete;
      Result:= True;
    finally
      FreeAndNil( parser );
    end;
  except
    on e: Exception do begin
      TWFXPluginUtil.exceptionToResult( e );
      Result:= False;
    end;
  end;
end;

function FsDeleteFileW( RemoteName: pwidechar ): Bool; cdecl;
begin
  Result:= fsDelete( RemoteName, False );
end;

function FsRemoveDirW( RemoteName: pwidechar ): Bool; cdecl;
begin
  Result:= fsDelete( RemoteName, True );
end;

function FsRenMovFileW(
  OldName: pwidechar;
  NewName: pwidechar;
  Move, OverWrite: Bool;
  RemoteInfo: pRemoteInfo ): Integer; cdecl;
var
  isFolder: Boolean;
  parserOld: TWFXPathParser = nil;
  parserNew: TWFXPathParser = nil;

  function doCopyOrMove: Integer;
  var
    ret: Boolean;
    driver: TCloudDriverBase;
  begin
    ret:= WFXMacCloudPlugin.progress( oldName, newName, 0 ) = 0;
    if ret then begin
      isFolder:= (RemoteInfo^.Attr AND FILE_ATTRIBUTE_DIRECTORY) <> 0;
      parserOld:= TWFXPathParser.Create( TStringUtil.widecharsToString(OldName) );
      parserNew:= TWFXPathParser.Create( TStringUtil.widecharsToString(NewName) );
      driver:= TCloudRootHelper.getDriver( parserOld );
      if parserOld.driverPath = EmptyStr then begin
        if parserNew.driverPath <> EmptyStr then
          raise ENotSupportedException.Create( 'Connection not support copying' );
        driver.copyOrMove( parserOld.connectionName, parserNew.connectionName, isFolder, Move );
      end else begin
        if parserNew.driverPath = EmptyStr then
          raise ENotSupportedException.Create( 'not support copying/moving to Connection' );
        if parserOld.connection <> parserNew.connection then
          raise ENotSupportedException.Create( 'not support copying/moving between different Connections' );
        driver.copyOrMove( parserOld.driverPath, parserNew.driverPath, isFolder, Move );
      end;
      WFXMacCloudPlugin.progress( oldName, newName, 100 );
      Result:= FS_FILE_OK;
    end else
      Result:= FS_FILE_USERABORT;
  end;

begin
  try
    try
      Result:= doCopyOrMove;
    finally
      FreeAndNil( parserOld );
      FreeAndNil( parserNew );
    end;
  except
    on e: Exception do
      Result:= TWFXPluginUtil.exceptionToResult( e );
  end;
end;

function FsExecuteFileW(
  MainWin: HWND;
  RemoteName: pwidechar;
  Verb: pwidechar ): Integer; cdecl;
var
  parser: TWFXPathParser = nil;

  function doExecute: Integer;
  var
    utf8Path: String;
    utf8Verb: String;
  begin
    Result:= FS_EXEC_OK;
    utf8Path:= TStringUtil.widecharsToString( RemoteName );
    utf8Verb:= TStringUtil.widecharsToString( Verb );
    parser:= TWFXPathParser.Create( utf8Path );

    if parser.driverPath <> EmptyStr then
      Exit( FS_EXEC_YOURSELF );

    if utf8Verb = 'open' then begin
      if parser.connectionName = rsAddNewConnection then begin
        TWFXOptionsUtil.addAndShow;
        TCloudRootHelper.saveConfig;
      end else begin
        Exit( FS_EXEC_SYMLINK );
      end;
    end else if utf8Verb = 'properties' then begin
      TWFXOptionsUtil.show( parser.connectionName );
      TCloudRootHelper.saveConfig;
    end;
  end;

begin
  try
    try
      Result:= doExecute;
    finally
      FreeAndNil( parser );
    end;
  except
    on e: Exception do begin
      TWFXPluginUtil.exceptionToResult( e );
      Result:= FS_EXEC_ERROR;
    end;
  end;

end;

function FsGetBackgroundFlags: Integer; cdecl;
begin
  Result:= BG_DOWNLOAD or BG_UPLOAD{ or BG_ASK_USER};
end;

procedure FsGetDefRootName( DefRootName: pchar; maxlen: Integer ); cdecl;
begin
  strlcopy( DefRootName, 'cloud', maxlen );
end;

end.
