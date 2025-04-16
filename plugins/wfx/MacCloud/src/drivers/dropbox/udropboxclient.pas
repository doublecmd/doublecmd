{
   Notes:
   1. the most basic DropBox Client
   2. no dependencies on other libraries
}

unit uDropBoxClient;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, DateUtils,
  CocoaAll,
  uCloudDriver, uMiniHttpClient, uMiniUtil;

type

  { TDropBoxListFolderSession }

  TDropBoxListFolderSession = class( TCloudDriverListFolderSession )
  private
    _cursor: String;
  private
    procedure analyseListResult( const jsonString: String );
  protected
    procedure listFolderFirst; override;
    procedure listFolderContinue; override;
  public
    constructor Create( const authSession: TCloudDriverAuthPKCESession; const path: String ); override;
  end;

  { TDropBoxDownloadSession }

  TDropBoxDownloadSession = class( TCloudDriverDownloadSession )
  public
    procedure download; override;
  end;

  { TDropBoxUploadSession }

  TDropBoxUploadSession = class( TCloudDriverUploadSession )
  private
    procedure uploadSmall;
    procedure uploadLarge;
  public
    procedure upload; override;
  end;

  { TDropBoxCreateFolderSession }

  TDropBoxCreateFolderSession = class( TCloudDriverCreateFolderSession )
  public
    procedure createFolder; override;
  end;

  { TDropBoxDeleteSession }

  TDropBoxDeleteSession = class( TCloudDriverDeleteSession )
  public
    procedure delete; override;
  end;

  { TDropBoxCopyMoveSession }

  TDropBoxCopyMoveSession = class( TCloudDriverCopyMoveSession )
  public
    procedure copyOrMove( const needToMove: Boolean ); override;
  end;

  { TDropBoxClient }

  TDropBoxClient = class( TAuthSessionCloudDriver )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
  public
    constructor Create( const config: TCloudDriverConfig );
    function clone: TCloudDriver; override;
  public
    function createLister( const path: String ): TCloudDriverLister; override;
  public
    procedure download(
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback ); override;
    procedure upload(
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback ); override;
  public
    procedure createFolder( const path: String ); override;
    procedure delete( const path: String ); override;
    procedure copyOrMove( const fromPath: String; const toPath: String; const needToMove: Boolean ); override;
  end;

var
  dropBoxConfig: TCloudDriverConfig;

implementation

type
  TDropBoxConstURI = record
    OAUTH2: String;
    TOKEN: String;
    REVOKE_TOKEN: String;
    LIST_FOLDER: String;
    LIST_FOLDER_CONTINUE: String;
    DOWNLOAD: String;
    UPLOAD_SMALL: String;
    UPLOAD_LARGE_START: String;
    UPLOAD_LARGE_APPEND: String;
    UPLOAD_LARGE_FINISH: String;
    CREATE_FOLDER: String;
    DELETE: String;
    COPY: String;
    MOVE: String;
  end;

  TDropBoxConstHeader = record
    AUTH: String;
    ARG: String;
    RESULT: String;
  end;

  TDropBoxConstUpload = record
    LARGE_FILE_SIZE: Integer;
    SESSION_FILE_SIZE: Integer;
  end;

  TDropBoxConst = record
    URI: TDropBoxConstURI;
    HEADER: TDropBoxConstHeader;
    UPLOAD: TDropBoxConstUpload;
  end;

const
  DropBoxConst: TDropBoxConst = (
    URI: (
      OAUTH2: 'https://www.dropbox.com/oauth2/authorize';
      TOKEN: 'https://api.dropbox.com/oauth2/token';
      REVOKE_TOKEN: 'https://api.dropboxapi.com/2/auth/token/revoke';
      LIST_FOLDER:  'https://api.dropboxapi.com/2/files/list_folder';
      LIST_FOLDER_CONTINUE: 'https://api.dropboxapi.com/2/files/list_folder/continue';
      DOWNLOAD: 'https://content.dropboxapi.com/2/files/download';
      UPLOAD_SMALL: 'https://content.dropboxapi.com/2/files/upload';
      UPLOAD_LARGE_START: 'https://content.dropboxapi.com/2/files/upload_session/start';
      UPLOAD_LARGE_APPEND: 'https://content.dropboxapi.com/2/files/upload_session/append_v2';
      UPLOAD_LARGE_FINISH: 'https://content.dropboxapi.com/2/files/upload_session/finish';
      CREATE_FOLDER: 'https://api.dropboxapi.com/2/files/create_folder_v2';
      DELETE: 'https://api.dropboxapi.com/2/files/delete_v2';
      COPY: 'https://api.dropboxapi.com/2/files/copy_v2';
      MOVE: 'https://api.dropboxapi.com/2/files/move_v2';
    );
    HEADER: (
      AUTH: 'Authorization';
      ARG: 'Dropbox-API-Arg';
      RESULT: 'Dropbox-API-Result';
    );
    UPLOAD: (
      LARGE_FILE_SIZE: 1000*1000*150;   // 150M
      SESSION_FILE_SIZE: 1024*1024*120; // 120MiB
    );
  );

// raise the corresponding exception if there are errors
procedure DropBoxClientProcessResult( const cloudDriverResult: TCloudDriverResult );
var
  httpResult: TMiniHttpResult;
  httpError: NSError;
  httpErrorDescription: String;
  dropBoxMessage: String;

  procedure processHttpError;
  begin
    httpResult:= cloudDriverResult.httpResult;
    httpError:= httpResult.error;

    if Assigned(httpError) then begin
      httpErrorDescription:= httpError.localizedDescription.UTF8String;
      case httpError.code of
        2: raise EFileNotFoundException.Create( httpErrorDescription );
        -1001: raise EInOutError.Create( httpErrorDescription );
      end;
    end;
  end;

  procedure processDropBox401Error;
  begin
    if dropBoxMessage.IndexOf('access_token') >= 0 then
      raise ECloudDriverTokenException.Create( dropBoxMessage );
    raise ECloudDriverException.Create( dropBoxMessage );
  end;

  procedure processDropBox409Error;
  begin
    if dropBoxMessage.IndexOf('not_found') >= 0 then
      raise EFileNotFoundException.Create( dropBoxMessage );
    if dropBoxMessage.IndexOf('conflict') >= 0 then
      raise ECloudDriverConflictException.Create( dropBoxMessage );
    raise ECloudDriverPermissionException.Create( dropBoxMessage );
  end;

  procedure processDropBoxError;
  begin
    dropBoxMessage:= cloudDriverResult.resultMessage;

    if (httpResult.resultCode>=200) and (httpResult.resultCode<=299) then
      Exit;
    case httpResult.resultCode of
      401: processDropBox401Error;
      409: processDropBox409Error;
      403: raise ECloudDriverPermissionException.Create( dropBoxMessage );
      429: raise ECloudDriverRateLimitException.Create( dropBoxMessage );
      else raise ECloudDriverException.Create( dropBoxMessage );
    end;
  end;

  procedure logException( const e: Exception );
  var
    message: String;
  begin
    message:= 'DropBox Error';
    if e.Message <> EmptyStr then
      message:= message + ': ' + e.Message;
    TLogUtil.logError( message );
  end;

begin
  try
    processHttpError;
    processDropBoxError;
  except
    on e: Exception do begin
      logException( e );
      raise;
    end;
  end;
end;

{ TDropBoxListFolderSession }

procedure TDropBoxListFolderSession.listFolderFirst;
var
  http: TMiniHttpClient = nil;
  httpResult: TMiniHttpResult;
  cloudDriverResult: TCloudDriverResult = nil;
  body: NSString;
begin
  try
    body:= TJsonUtil.dumps( ['path', _path] );
    http:= TMiniHttpClient.Create( DropBoxConst.URI.LIST_FOLDER, HttpConst.Method.POST );
    _authSession.setAuthHeader( http );
    http.setContentType( HttpConst.ContentType.JSON );
    http.setBody( body );

    cloudDriverResult:= TCloudDriverResult.Create;
    httpResult:= http.connect;
    cloudDriverResult.httpResult:= httpResult;
    cloudDriverResult.resultMessage:= httpResult.body;

    if httpResult.resultCode = 200 then
      analyseListResult( httpResult.body );

    DropBoxClientProcessResult( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TDropBoxListFolderSession.listFolderContinue;
var
  http: TMiniHttpClient = nil;
  httpResult: TMiniHttpResult;
  cloudDriverResult: TCloudDriverResult = nil;
  body: NSString;
begin
  try
    body:= TJsonUtil.dumps( ['cursor', _cursor] );
    http:= TMiniHttpClient.Create( DropBoxConst.URI.LIST_FOLDER_CONTINUE, HttpConst.Method.POST );
    _authSession.setAuthHeader( http );
    http.setContentType( HttpConst.ContentType.JSON );
    http.setBody( body );

    cloudDriverResult:= TCloudDriverResult.Create;
    httpResult:= http.connect;
    cloudDriverResult.httpResult:= httpResult;
    cloudDriverResult.resultMessage:= httpResult.body;

    if httpResult.resultCode = 200 then
      analyseListResult( httpResult.body );

    DropBoxClientProcessResult( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TDropBoxListFolderSession.analyseListResult(const jsonString: String);
var
  json: NSDictionary;
  jsonEntries: NSArray;
  jsonItem: NSDictionary;
  cloudFile: TCloudFile;

  function toDateTime( const key: String ): TDateTime;
  var
    str: String;
  begin
    str:= TJsonUtil.getString( jsonItem, key );
    if str = EmptyStr then
      Result:= 0
    else
      Result:= ISO8601ToDate( str );
  end;

begin
  json:= TJsonUtil.parse( jsonString );
  _cursor:= TJsonUtil.getString( json, 'cursor' );
  _hasMore:= TJsonUtil.getBoolean( json, 'has_more' );
  jsonEntries:= TJsonUtil.getArray( json, 'entries' );
  if jsonEntries = nil then
    Exit;
  for jsonItem in jsonEntries do begin
    cloudFile:= TCloudFile.Create;
    cloudFile.isFolder:= ( TJsonUtil.getString(jsonItem,'.tag')='folder' );
    cloudFile.name:= TJsonUtil.getString( jsonItem, 'name' );
    cloudFile.size:= TJsonUtil.getInteger( jsonItem, 'size' );
    cloudFile.creationTime:= toDateTime( 'client_modified' );
    cloudFile.modificationTime:= toDateTime( 'server_modified' );
    _files.Add( cloudFile );
  end;
end;

constructor TDropBoxListFolderSession.Create( const authSession: TCloudDriverAuthPKCESession; const path: String );
var
  truePath: String;
begin
  if path = PathDelim then
    truePath:= EmptyStr
  else
    truePath:= path;
  Inherited Create( authSession, truePath );
end;

{ TDropBoxDownloadSession }

procedure TDropBoxDownloadSession.download;
var
  http: TMiniHttpClient = nil;
  argJsonString: NSString;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    argJsonString:= TJsonUtil.dumps( ['path', _serverPath], True );
    http:= TMiniHttpClient.Create( DropBoxConst.URI.DOWNLOAD, HttpConst.Method.POST );
    _authSession.setAuthHeader( http );
    http.addHeader( NSSTR(DropBoxConst.HEADER.ARG), argJsonString );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.download( _localPath, _callback );
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.getHeader( DropBoxConst.HEADER.RESULT );

    DropBoxClientProcessResult( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TDropBoxUploadSession }

procedure TDropBoxUploadSession.uploadSmall;
var
  http: TMiniHttpClient = nil;
  argJsonString: NSString;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    argJsonString:= TJsonUtil.dumps( ['path',_serverPath, 'mode','overwrite'], True );
    http:= TMiniHttpClient.Create( DropBoxConst.URI.UPLOAD_SMALL, HttpConst.Method.POST );
    _authSession.setAuthHeader( http );
    http.addHeader( NSSTR(DropBoxConst.HEADER.ARG), argJsonString );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.upload( _localPath, _callback );
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;

    DropBoxClientProcessResult( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TDropBoxUploadSession.uploadLarge;
var
  sessionId: String;
  range: TMiniHttpContentRange;
  offset: Integer;
  sessionSize: Integer;

  procedure uploadStart;
  var
    http: TMiniHttpClient = nil;
    cloudDriverResult: TCloudDriverResult = nil;
    json: NSDictionary;
  begin
    try
      http:= TMiniHttpClient.Create( DropBoxConst.URI.UPLOAD_LARGE_START, HttpConst.Method.POST );
      _authSession.setAuthHeader( http );
      http.setContentType( HttpConst.ContentType.OctetStream );

      cloudDriverResult:= TCloudDriverResult.Create;
      cloudDriverResult.httpResult:= http.connect;
      cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;

      DropBoxClientProcessResult( cloudDriverResult );

      json:= TJsonUtil.parse( cloudDriverResult.resultMessage );
      sessionId:= TJsonUtil.getString( json, 'session_id' );
      if sessionId = EmptyStr then
        raise ECloudDriverException.Create( 'can''t get session_id in TDropBoxUploadSession.uploadLarge()' );
    finally
      FreeAndNil( cloudDriverResult );
      FreeAndNil( http );
    end;
  end;

  procedure uploadAppend( const range: TMiniHttpContentRange );
    function getArgJsonString: NSString;
    var
      jsonCursor: NSMutableDictionary;
    begin
      jsonCursor:= NSMutableDictionary.new;
      TJsonUtil.setString( jsonCursor, 'session_id', sessionId );
      TJsonUtil.setInteger( jsonCursor, 'offset', offset );
      Result:= TJsonUtil.dumps( ['cursor',jsonCursor] );
      jsonCursor.release;
    end;
  var
    http: TMiniHttpClient = nil;
    cloudDriverResult: TCloudDriverResult = nil;
  begin
    try
      http:= TMiniHttpClient.Create( DropBoxConst.URI.UPLOAD_LARGE_APPEND, HttpConst.Method.POST );
      _authSession.setAuthHeader( http );
      http.addHeader( NSSTR(DropBoxConst.HEADER.ARG), getArgJsonString );

      cloudDriverResult:= TCloudDriverResult.Create;
      cloudDriverResult.httpResult:= http.uploadRange(
        _localPath,
        range,
        _callback );
      cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;

      DropBoxClientProcessResult( cloudDriverResult );
    finally
      FreeAndNil( cloudDriverResult );
      FreeAndNil( http );
    end;
  end;

  procedure uploadFinish;
    function getArgJsonString: NSString;
    var
      jsonCursor: NSMutableDictionary;
      jsonCommit: NSMutableDictionary;
    begin
      jsonCursor:= NSMutableDictionary.new;
      TJsonUtil.setString( jsonCursor, 'session_id', sessionId );
      TJsonUtil.setInteger( jsonCursor, 'offset', offset );

      jsonCommit:= NSMutableDictionary.new;
      TJsonUtil.setString( jsonCommit, 'path', _serverPath );
      TJsonUtil.setString( jsonCommit, 'mode', 'overwrite' );

      Result:= TJsonUtil.dumps( ['cursor',jsonCursor, 'commit',jsonCommit] );

      jsonCursor.release;
      jsonCommit.release;
    end;
  var
    http: TMiniHttpClient = nil;
    cloudDriverResult: TCloudDriverResult = nil;
  begin
    try
      http:= TMiniHttpClient.Create( DropBoxConst.URI.UPLOAD_LARGE_FINISH, HttpConst.Method.POST );
      _authSession.setAuthHeader( http );
      http.addHeader( NSSTR(DropBoxConst.HEADER.ARG), getArgJsonString );
      http.setContentType( HttpConst.ContentType.OctetStream );

      cloudDriverResult:= TCloudDriverResult.Create;
      cloudDriverResult.httpResult:= http.connect;
      cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;

      DropBoxClientProcessResult( cloudDriverResult );
    finally
      FreeAndNil( cloudDriverResult );
      FreeAndNil( http );
    end;
  end;

begin
  offset:= 0;
  sessionSize:= 0;
  uploadStart;
  while offset < _localFileSize do begin
    if offset + DropBoxConst.UPLOAD.SESSION_FILE_SIZE > _localFileSize then
      sessionSize:= _localFileSize - offset
    else
      sessionSize:= DropBoxConst.UPLOAD.SESSION_FILE_SIZE;
    range:= TMiniHttpContentRange.Create( offset, offset+sessionSize-1 , _localFileSize );
    uploadAppend( range );
    inc( offset, sessionSize );
    range.Free;
  end;
  uploadFinish;
end;

procedure TDropBoxUploadSession.upload;
begin
  if _localFileSize < DropBoxConst.UPLOAD.LARGE_FILE_SIZE then
    uploadSmall
  else
    uploadLarge;
end;

{ TDropBoxCreateFolderSession }

procedure TDropBoxCreateFolderSession.createFolder;
var
  http: TMiniHttpClient = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  body: NSString;
begin
  try
    body:= TJsonUtil.dumps( ['path', _path] );
    http:= TMiniHttpClient.Create( DropBoxConst.URI.CREATE_FOLDER, HttpConst.Method.POST );
    http.setContentType( HttpConst.ContentType.JSON );
    _authSession.setAuthHeader( http );
    http.setBody( body );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;

    DropBoxClientProcessResult( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TDropBoxDeleteSession }

procedure TDropBoxDeleteSession.delete;
var
  http: TMiniHttpClient = nil;
  body: NSString;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    body:= TJsonUtil.dumps( ['path', _path] );
    http:= TMiniHttpClient.Create( DropBoxConst.URI.DELETE, HttpConst.Method.POST );
    http.setContentType( HttpConst.ContentType.JSON );
    _authSession.setAuthHeader( http );
    http.setBody( body );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;

    DropBoxClientProcessResult( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TDropBoxCopyMoveSession }

procedure TDropBoxCopyMoveSession.copyOrMove( const needToMove: Boolean );
var
  uri: String;
  http: TMiniHttpClient = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  body: NSString;
begin
  try
    if needToMove then
      uri:= DropBoxConst.URI.MOVE
    else
      uri:= DropBoxConst.URI.COPY;

    http:= TMiniHttpClient.Create( uri, HttpConst.Method.POST );
    http.setContentType( HttpConst.ContentType.JSON );
    _authSession.setAuthHeader( http );

    body:= TJsonUtil.dumps( ['from_path', _fromPath, 'to_path', _toPath] );
    http.setBody( body );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;

    DropBoxClientProcessResult( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TDropBoxClient }

class function TDropBoxClient.driverName: String;
begin
  Result:= 'DropBox';
end;

class function TDropBoxClient.createInstance: TCloudDriver;
begin
  Result:= TDropBoxClient.Create( dropBoxConfig );
end;

constructor TDropBoxClient.Create(const config: TCloudDriverConfig);
var
  params: TCloudDriverAuthPKCESessionParams;
begin
  params.config:= config;
  params.resultProcessFunc:= @DropBoxClientProcessResult;
  params.OAUTH2_URI:= DropBoxConst.URI.OAUTH2;
  params.TOKEN_URI:= DropBoxConst.URI.TOKEN;
  params.REVOKE_TOKEN_URI:= DropBoxConst.URI.REVOKE_TOKEN;
  params.AUTH_HEADER:= DropBoxConst.HEADER.AUTH;
  params.AUTH_TYPE:= 'Bearer';
  Inherited Create( config, params );
end;

function TDropBoxClient.clone: TCloudDriver;
var
  newClient: TDropBoxClient;
begin
  newClient:= TDropBoxClient.Create( _config );
  newClient._authSession:= self._authSession.clone( newClient );
  Result:= newClient;
end;

function TDropBoxClient.createLister( const path: String ): TCloudDriverLister;
begin
  Result:= TCloudDriverDefaultLister.Create( TDropBoxListFolderSession, _authSession, path );
end;

procedure TDropBoxClient.download(
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback );
var
  session: TDropBoxDownloadSession = nil;
begin
  try
    session:= TDropBoxDownloadSession.Create( _authSession, serverPath, localPath, callback );
    session.download;
  finally
    FreeAndNil( session );
  end;
end;

procedure TDropBoxClient.upload(
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback);
var
  session: TDropBoxUploadSession = nil;
begin
  try
    session:= TDropBoxUploadSession.Create( _authSession, serverPath, localPath, callback );
    session.upload;
  finally
    FreeAndNil( session );
  end;
end;

procedure TDropBoxClient.createFolder(const path: String);
var
  session: TDropBoxCreateFolderSession = nil;
begin
  try
    session:= TDropBoxCreateFolderSession.Create( _authSession, path );
    session.createFolder;
  finally
    FreeAndNil( session );
  end;
end;

procedure TDropBoxClient.delete(const path: String);
var
  session: TDropBoxDeleteSession = nil;
begin
  try
    session:= TDropBoxDeleteSession.Create( _authSession, path );
    session.delete;
  finally
    FreeAndNil( session );
  end;
end;

procedure TDropBoxClient.copyOrMove(const fromPath: String; const toPath: String;
  const needToMove: Boolean );
var
  session: TDropBoxCopyMoveSession = nil;
begin
  try
    session:= TDropBoxCopyMoveSession.Create( _authSession, fromPath, toPath );
    session.copyOrMove( needToMove );
  finally
    FreeAndNil( session );
  end;
end;

finalization
  FreeAndNil( dropBoxConfig );

end.

