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
  WfxPlugin,
  uMacCloudCore, uMacCloudUtil,
  uDropBoxClient,
  uMiniUtil;

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
function FsGetBackgroundFlags:integer; cdecl;
procedure FsGetDefRootName(DefRootName:pchar;maxlen:integer); cdecl;

implementation

function client: TCloudDriver;
begin
  Result:= cloudConnectionManager.get('rich').driver;
end;

function FsInitW(
  PluginNr: Integer;
  pProgressProc: TProgressProcW;
  pLogProc: TLogProcW;
  pRequestProc: TRequestProcW ): Integer; cdecl;
begin
  macCloudPlugin:= TMacCloudPlugin.Create( PluginNr, pProgressProc, pLogProc );
  Result:= 0
end;

function FsFindFirstW(
  path:pwidechar;
  var FindData:tWIN32FINDDATAW ): THandle; cdecl;
var
  cloudFile: TCloudFile;
begin
  try
    client.listFolderBegin( TStringUtil.widecharsToString(path) );
    cloudFile:= client.listFolderGetNextFile;
    if cloudFile = nil then
      Exit( wfxInvalidHandle );

    TMacCloudUtil.cloudFileToWinFindData( cloudFile, FindData );
    Result:= 0;
  except
    on e: Exception do begin
      TMacCloudUtil.exceptionToResult( e );
      Result:= wfxInvalidHandle;
    end;
  end;
end;

function FsFindNextW(
  handle: THandle;
  var FindData:tWIN32FINDDATAW ): Bool; cdecl;
var
  cloudFile: TCloudFile;
begin
  try
    cloudFile:= client.listFolderGetNextFile;
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
begin
  Result:= 0;
  try
    client.listFolderEnd;
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
  serverPath: String;
  localPath: String;
  callback: TProgressCallback;
  totalBytes: Integer;
  li: ULARGE_INTEGER;
  exits: Boolean;

  function doGetFile: Integer;
  begin
    serverPath:= TStringUtil.widecharsToString( RemoteName );
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

    callback:= TProgressCallback.Create(
      RemoteName,
      LocalName,
      totalBytes );

    try
      callback.progress( 0 );
      client.download( serverPath, localPath, callback );
    finally
      callback.Free;
    end;

    Result:= FS_FILE_OK;
  end;

begin
  try
    Result:= doGetFile;
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
  serverPath: String;
  localPath: String;
  callback: TProgressCallback;
  totalBytes: Integer;
  exits: Boolean;

  function doPutFile: Integer;
  begin
    serverPath:= TStringUtil.widecharsToString( RemoteName );
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

    callback:= TProgressCallback.Create(
      RemoteName,
      LocalName,
      totalBytes );

    try
      callback.progress( 0 );
      client.upload( serverPath, localPath, callback );
    finally
      callback.Free;
    end;

    Result:= FS_FILE_OK;
  end;

begin
  try
    Result:= doPutFile;
  except
    on e: Exception do
      Result:= TMacCloudUtil.exceptionToResult( e );
  end;
end;

function FsMkDirW( RemoteDir: pwidechar ): Bool; cdecl;
var
  path: String;
begin
  try
    path:= TStringUtil.widecharsToString( RemoteDir );
    client.createFolder( path );
    Result:= True;
  except
    on e: Exception do begin
      TMacCloudUtil.exceptionToResult( e );
      Result:= False;
    end;
  end;
end;

function FsDeleteFileW( RemoteName: pwidechar ): Bool; cdecl;
var
  path: String;
begin
  try
    path:= TStringUtil.widecharsToString( RemoteName );
    client.delete( path );
    Result:= True;
  except
    on e: Exception do begin
      TMacCloudUtil.exceptionToResult( e );
      Result:= False;
    end;
  end;
end;

function FsRemoveDirW( RemoteName: pwidechar ): Bool; cdecl;
var
  path: String;
begin
  try
    path:= TStringUtil.widecharsToString( RemoteName );
    client.delete( path );
    Result:= True;
  except
    on e: Exception do begin
      TMacCloudUtil.exceptionToResult( e );
      Result:= False;
    end;
  end;
end;

function FsRenMovFileW(
  OldName: pwidechar;
  NewName: pwidechar;
  Move, OverWrite: Bool;
  RemoteInfo: pRemoteInfo ): Integer; cdecl;
var
  oldUtf8Path: String;
  newUtf8Path: String;
  ret: Boolean;
begin
  try
    oldUtf8Path:= TStringUtil.widecharsToString( OldName );
    newUtf8Path:= TStringUtil.widecharsToString( NewName );

    ret:= macCloudPlugin.progress( oldName, newName, 0 ) = 0;
    if ret then begin
      client.copyOrMove( oldUtf8Path, newUtf8Path, Move );
      macCloudPlugin.progress( oldName, newName, 100 );
      Result:= FS_FILE_OK
    end else
      Result:= FS_FILE_USERABORT;
  except
    on e: Exception do
      Result:= TMacCloudUtil.exceptionToResult( e );
  end;
end;

function FsGetBackgroundFlags: Integer; cdecl;
begin
  Result:= BG_DOWNLOAD or BG_UPLOAD{ or BG_ASK_USER};
end;

procedure FsGetDefRootName( DefRootName: pchar; maxlen: Integer ); cdecl;
begin
  strlcopy( DefRootName, 'DropBox', maxlen );
end;

procedure init;
var
  driver: TCloudDriver;
  connection: TCloudConnection;
begin
  dropBoxConfig:= TDropBoxConfig.Create( 'ahj0s9xia6i61gh', 'dc2ea085a05ac273a://dropbox/auth' );
  cloudDriverManager.register( TDropBoxClient );

  driver:= cloudDriverManager.createInstance( 'DropBox' );
  connection:= TCloudConnection.Create( 'rich', driver );
  cloudConnectionManager.add( connection );
end;

initialization
  init;

end.
