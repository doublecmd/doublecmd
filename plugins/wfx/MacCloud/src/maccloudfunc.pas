{
  Notes:
  1. currently implementing DropBox only
  2. other cloud drivers will be gradually supported
}

unit MacCloudFunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  WfxPlugin, Extension,
  uMacCloudCore, uMacCloudConfig, uMacCloudUtil, uMacCloudOptions,
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

const
  CONST_ADD_NEW_CONNECTION = '<Add New Connection>';

type

  { TCloudRootDriver }

  TCloudRootDriver = class( TCloudDriverBase )
  private
    _list: TFPList;
  public
    procedure listFolderBegin(const path: String); override;
    function listFolderGetNextFile: TCloudFile; override;
    procedure listFolderEnd; override;
  public
    procedure createFolder(const path: String); override;
    procedure delete(const path: String); override;
    procedure copyOrMove(const fromPath: String; const toPath: String; const needToMove: Boolean); override;
  end;

procedure loadConfig( const path: String );
begin
  macCloudDriverConfigManager.loadFromCommon( path );
  macCloudDriverConfigManager.loadFromSecurity;
end;

procedure saveConfig( const path: String = '' );
var
  configPath: String;
begin
  if path = EmptyStr then
    configPath:= macCloudPlugin.configPath
  else
    configPath:= path;
  macCloudDriverConfigManager.saveToCommon( configPath );
  macCloudDriverConfigManager.saveToSecurity;
end;

function getDriver( const parser: TCloudPathParser ): TCloudDriverBase;
begin
  if parser.driverPath = EmptyStr then
    Result:= TCloudRootDriver.Create
  else
    Result:= parser.driver;
end;

procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); cdecl;
var
  configPath: String;
begin
  try
    configPath:= StartupInfo^.PluginConfDir + 'MacCloud.json';
    if macCloudPlugin <> nil then begin
      macCloudPlugin.configPath:= configPath;
      macCloudPlugin.pluginPath:= StartupInfo^.PluginDir;
    end;
    loadConfig( configPath );
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
  if Assigned(macCloudPlugin) then
    macCloudPlugin.Free;
  macCloudPlugin:= TMacCloudPlugin.Create( PluginNr, pProgressProc, pLogProc );
  Result:= 0;
end;

function FsFindFirstW(
  path: pwidechar;
  var FindData: TWIN32FINDDATAW ): THandle; cdecl;
var
  parser: TCloudPathParser = nil;
  driver: TCloudDriverBase;

  function doFindFirst: THandle;
  var
    utf8Path: String;
    cloudFile: TCloudFile;
  begin
    utf8Path:= TStringUtil.widecharsToString(path);
    parser:= TCloudPathParser.Create( utf8Path );
    if utf8Path = PathDelim then
      driver:= TCloudRootDriver.Create
    else
      driver:= parser.driver;
    Result:= THandle( driver );
    driver.listFolderBegin( parser.driverPath );
    cloudFile:= driver.listFolderGetNextFile;
    if cloudFile = nil then
      Exit( wfxInvalidHandle );

    TMacCloudUtil.cloudFileToWinFindData( cloudFile, FindData );
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
      TMacCloudUtil.exceptionToResult( e );
      Result:= wfxInvalidHandle;
    end;
  end;

  if (Result=wfxInvalidHandle) and Assigned(driver) then
    driver.listFolderEnd;
end;

function FsFindNextW(
  handle: THandle;
  var FindData:tWIN32FINDDATAW ): Bool; cdecl;
var
  driver: TCloudDriverBase;
  cloudFile: TCloudFile;
begin
  try
    driver:= TCloudDriverBase( handle );
    cloudFile:= driver.listFolderGetNextFile;
    if cloudFile = nil then
      Exit( False );

    TMacCloudUtil.cloudFileToWinFindData( cloudFile, FindData );
    Result:= True;
  except
    on e: Exception do begin
      TMacCloudUtil.exceptionToResult( e );
      Result:= False;
    end;
  end;
end;

function FsFindClose( handle: THandle ): Integer; cdecl;
var
  driver: TCloudDriverBase;
begin
  Result:= 0;
  try
    driver:= TCloudDriverBase( handle );
    driver.listFolderEnd;
  except
    on e: Exception do begin
      TMacCloudUtil.exceptionToResult( e );
    end;
  end;
end;

function FsGetFileW(
  RemoteName: pwidechar;
  LocalName: pwidechar;
  CopyFlags: Integer;
  RemoteInfo: pRemoteInfo ): Integer; cdecl;

var
  parser: TCloudPathParser = nil;
  callback: TCloudProgressCallback = nil;

  function doGetFile: Integer;
  var
    serverPath: String;
    localPath: String;
    totalBytes: Integer;
    li: ULARGE_INTEGER;
    exits: Boolean;
  begin
    parser:= TCloudPathParser.Create( TStringUtil.widecharsToString(RemoteName) );
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

    callback:= TCloudProgressCallback.Create(
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
      Result:= TMacCloudUtil.exceptionToResult( e );
  end;
end;

function FsPutFileW(
  LocalName: pwidechar;
  RemoteName: pwidechar;
  CopyFlags: Integer ): Integer; cdecl;
const
  FS_EXISTS = FS_COPYFLAGS_EXISTS_SAMECASE or FS_COPYFLAGS_EXISTS_DIFFERENTCASE;
var
  parser: TCloudPathParser = nil;
  callback: TCloudProgressCallback = nil;

  function doPutFile: Integer;
  var
    serverPath: String;
    localPath: String;
    totalBytes: Integer;
    exits: Boolean;
  begin
    parser:= TCloudPathParser.Create( TStringUtil.widecharsToString(RemoteName) );
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

    callback:= TCloudProgressCallback.Create(
      RemoteName,
      LocalName,
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
      Result:= TMacCloudUtil.exceptionToResult( e );
  end;
end;

function FsMkDirW( RemoteDir: pwidechar ): Bool; cdecl;
var
  parser: TCloudPathParser = nil;

  procedure doCreateFolder;
  var
    folderName: String;
  begin
    parser:= TCloudPathParser.Create( TStringUtil.widecharsToString(RemoteDir) );
    if parser.driverPath = EmptyStr then
      folderName:= parser.connectionName
    else
      folderName:= parser.driverPath;
    getDriver(parser).createFolder( folderName );
  end;

begin
  try
    try
      doCreateFolder;
      Result:= True;
    finally
      FreeAndNil( parser );
    end;
  except
    on e: Exception do begin
      TMacCloudUtil.exceptionToResult( e );
      Result:= False;
    end;
  end;
end;

function FsDeleteFileW( RemoteName: pwidechar ): Bool; cdecl;
var
  parser: TCloudPathParser = nil;

  procedure doDelete;
  var
    utf8Path: String;
  begin
    utf8Path:= TStringUtil.widecharsToString(RemoteName);
    parser:= TCloudPathParser.Create( utf8Path );
    if parser.driverPath = EmptyStr then
      utf8Path:= parser.connectionName
    else
      utf8Path:= parser.driverPath;
    getDriver(parser).delete( utf8Path );
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
      TMacCloudUtil.exceptionToResult( e );
      Result:= False;
    end;
  end;
end;

function FsRemoveDirW( RemoteName: pwidechar ): Bool; cdecl;
begin
  Result:= FsDeleteFileW( RemoteName );
end;

function FsRenMovFileW(
  OldName: pwidechar;
  NewName: pwidechar;
  Move, OverWrite: Bool;
  RemoteInfo: pRemoteInfo ): Integer; cdecl;
var
  parserOld: TCloudPathParser = nil;
  parserNew: TCloudPathParser = nil;

  function doCopyOrMove: Integer;
  var
    ret: Boolean;
    driver: TCloudDriverBase;
  begin
    ret:= macCloudPlugin.progress( oldName, newName, 0 ) = 0;
    if ret then begin
      parserOld:= TCloudPathParser.Create( TStringUtil.widecharsToString(OldName) );
      parserNew:= TCloudPathParser.Create( TStringUtil.widecharsToString(NewName) );
      driver:= getDriver( parserOld );
      if parserOld.driverPath = EmptyStr then begin
        if parserNew.driverPath <> EmptyStr then
          raise ENotSupportedException.Create( 'Connection not support copying' );
        driver.copyOrMove( parserOld.connectionName, parserNew.connectionName, True );
      end else begin
        if parserOld.connection <> parserNew.connection then
          raise ENotSupportedException.Create( 'Internal copy/move functions cannot be used between different accounts' );
        driver.copyOrMove( parserOld.driverPath, parserNew.driverPath, Move );
      end;
      macCloudPlugin.progress( oldName, newName, 100 );
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
      Result:= TMacCloudUtil.exceptionToResult( e );
  end;
end;

function FsExecuteFileW(
  MainWin: HWND;
  RemoteName: pwidechar;
  Verb: pwidechar ): Integer; cdecl;
var
  parser: TCloudPathParser = nil;

  function doExecute: Integer;
  var
    utf8Path: String;
    utf8Verb: String;
  begin
    Result:= FS_EXEC_OK;
    utf8Path:= TStringUtil.widecharsToString( RemoteName );
    utf8Verb:= TStringUtil.widecharsToString( Verb );
    parser:= TCloudPathParser.Create( utf8Path );

    if parser.driverPath <> EmptyStr then
      Exit( FS_EXEC_YOURSELF );

    if utf8Verb = 'open' then begin
      if parser.connectionName = CONST_ADD_NEW_CONNECTION then begin
        TCloudOptionsUtil.addAndShow;
        saveConfig;
      end else begin
        Exit( FS_EXEC_SYMLINK );
      end;
    end else if utf8Verb = 'properties' then begin
      TCloudOptionsUtil.show( parser.connectionName );
      saveConfig;
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
      TMacCloudUtil.exceptionToResult( e );
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

{ TCloudRootDriver }

procedure TCloudRootDriver.listFolderBegin(const path: String);
  procedure addNewCommand;
  var
    cloudFile: TCloudFile;
  begin
    cloudFile:= TCloudFile.Create;
    cloudFile.name:= CONST_ADD_NEW_CONNECTION;
    cloudFile.creationTime:= 4.5753942770578702E+004;
    cloudFile.modificationTime:= cloudFile.creationTime;
    _list.Add( cloudFile );
  end;

  procedure addConnections;
  var
    cloudFile: TCloudFile;
    connection: TCloudConnection;
    i: Integer;
  begin
    for i:= 0 to cloudConnectionManager.connections.Count - 1 do begin;
      connection:= TCloudConnection( cloudConnectionManager.connections[i] );
      cloudFile:= TCloudFile.Create;
      cloudFile.name:= connection.name;
      cloudFile.creationTime:= connection.creationTime;
      cloudFile.modificationTime:= connection.modificationTime;
      _list.Add( cloudFile );
    end;
  end;

begin
  _list:= TFPList.Create;
  addNewCommand;
  addConnections;
end;

function TCloudRootDriver.listFolderGetNextFile: TCloudFile;
begin
  if _list.Count > 0 then begin
    Result:= TCloudFile( _list.First );
    _list.Delete( 0 );
  end else begin
    Result:= nil;
  end;
end;

procedure TCloudRootDriver.listFolderEnd;
begin
  FreeAndNil( _list );
  self.Free;
end;

procedure TCloudRootDriver.createFolder(const path: String);
begin
  TCloudOptionsUtil.addAndShow( path );
  saveConfig;
  self.Free;
end;

procedure TCloudRootDriver.delete(const path: String);
var
  connectionName: String absolute path;
begin
  TLogUtil.logInformation( 'Connection Deleted: ' + connectionName );
  cloudConnectionManager.delete( connectionName );
  saveConfig;
  self.Free;
end;

procedure TCloudRootDriver.copyOrMove(const fromPath: String;
  const toPath: String; const needToMove: Boolean);
var
  connectionOldName: String absolute fromPath;
  connectionNewName: String absolute toPath;
  connection: TCloudConnection;
begin
  if NOT needToMove then
    raise ENotSupportedException.Create( 'Connection only support renaming' );
  TLogUtil.logInformation( 'Connection Rename: ' + connectionOldName + ' --> ' + connectionNewName );
  connection:= cloudConnectionManager.get( connectionOldName );
  connection.name:= connectionNewName;
  saveConfig;
  self.Free;
end;

end.
