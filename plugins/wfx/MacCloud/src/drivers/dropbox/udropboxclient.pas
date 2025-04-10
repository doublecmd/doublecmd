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
  Classes, SysUtils, syncobjs, DateUtils,
  CocoaAll, uMiniCocoa,
  uMacCloudCore, uMiniHttpClient, uMiniUtil;

type

  { TDropBoxResult }

  TDropBoxResult = class
  public
    httpResult: TMiniHttpResult;
    resultMessage: String;
  end;

  { EDropBoxException }

  EDropBoxException = class( Exception );
  EDropBoxTokenException = class( EDropBoxException );
  EDropBoxConflictException = class( EDropBoxException );
  EDropBoxPermissionException = class( EDropBoxException );
  EDropBoxRateLimitException = class( EDropBoxException );

  { TDropBoxConfig }

  TDropBoxConfig = class
  private
    _clientID: String;
    _listenURI: String;
  public
    constructor Create( const clientID: String; const listenURI: String );
    property clientID: String read _clientID;
    property listenURI: String read _listenURI;
  end;

  { TDropBoxToken }

  TDropBoxToken = class
  private
    _access: String;
    _refresh: String;
    _accessExpirationTime: NSTimeInterval;
  private
    function isValidAccessToken: Boolean;
    function isValidFreshToken: Boolean;
  public
    constructor Create;
    constructor Create( const access: String; const refresh: String; const accessExpirationTime: NSTimeInterval );
    function clone: TDropBoxToken;
  public
    procedure setExpiration( const seconds: Integer );
    procedure invalid;
    property access: String read _access write _access;
    property refresh: String read _refresh write _refresh;
    property accessExpirationTime: NSTimeInterval read _accessExpirationTime write _accessExpirationTime;
  end;

  { TDropBoxAuthPKCESession }

  TDropBoxAuthPKCESession = class
  strict private
    _config: TDropBoxConfig;
    _dropBoxClient: TCloudDriver;
    _codeVerifier: String;
    _state: String;
    _code: String;
    _token: TDropBoxToken;
    _accountID: String;
    _alert: NSAlert;
    _lockObject: TCriticalSection;
  private
    procedure requestAuthorization;
    procedure waitAuthorizationAndPrompt;
    procedure closePrompt;
    procedure requestToken;
    procedure revokeToken;
    procedure refreshToken;
    procedure onRedirect( const url: NSURL );
    function getAccessToken: String;
  public
    constructor Create( const config: TDropBoxConfig; const dropBoxClient: TCloudDriver );
    destructor Destroy; override;
    function clone( const dropBoxClient: TCloudDriver ): TDropBoxAuthPKCESession;
  public
    function authorize: Boolean;
    procedure unauthorize;
    function authorized: Boolean;
    procedure setAuthHeader( http: TMiniHttpClient );
  protected
    procedure setToken( const token: TDropBoxToken );
    function getToken: TDropBoxToken;
  end;

  { TDropBoxListFolderSession }

  TDropBoxListFolderSession = class
  private
    _authSession: TDropBoxAuthPKCESession;
    _path: String;
    _files: TCloudFiles;
    _cursor: String;
    _hasMore: Boolean;
  private
    procedure listFolderFirst;
    procedure listFolderContinue;
    procedure analyseListResult( const jsonString: String );
  public
    constructor Create( const authSession: TDropBoxAuthPKCESession; const path: String );
    destructor Destroy; override;
    function getNextFile: TCloudFile;
  end;

  { TDropBoxDownloadSession }

  TDropBoxDownloadSession = class
  private
    _authSession: TDropBoxAuthPKCESession;
    _serverPath: String;
    _localPath: String;
    _callback: ICloudProgressCallback;
  public
    constructor Create(
      const authSession: TDropBoxAuthPKCESession;
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback );
    procedure download;
  end;

  { TDropBoxUploadSession }

  TDropBoxUploadSession = class
  private
    _authSession: TDropBoxAuthPKCESession;
    _serverPath: String;
    _localPath: String;
    _localFileSize: Integer;
    _callback: ICloudProgressCallback;
  private
    procedure uploadSmall;
    procedure uploadLarge;
  public
    constructor Create(
      const authSession: TDropBoxAuthPKCESession;
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback );
    procedure upload;
  end;

  { TDropBoxCreateFolderSession }

  TDropBoxCreateFolderSession = class
  private
    _authSession: TDropBoxAuthPKCESession;
    _path: String;
  public
    constructor Create( const authSession: TDropBoxAuthPKCESession; const path: String );
    procedure createFolder;
  end;

  { TDropBoxDeleteSession }

  TDropBoxDeleteSession = class
  private
    _authSession: TDropBoxAuthPKCESession;
    _path: String;
  public
    constructor Create( const authSession: TDropBoxAuthPKCESession; const path: String );
    procedure delete;
  end;

  { TDropBoxCopyMoveSession }

  TDropBoxCopyMoveSession = class
  private
    _authSession: TDropBoxAuthPKCESession;
    _fromPath: String;
    _toPath: String;
  public
    constructor Create( const authSession: TDropBoxAuthPKCESession;
      const fromPath: String; const toPath: String );
    procedure copyOrMove( const needToMove: Boolean );
    procedure copy;
    procedure move;
  end;

  { TDropBoxClient }

  TDropBoxClient = class( TCloudDriver )
  private
    _config: TDropBoxConfig;
    _authSession: TDropBoxAuthPKCESession;
    _listFolderSession: TDropBoxListFolderSession;
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
  public
    constructor Create( const config: TDropBoxConfig );
    destructor Destroy; override;
    function clone: TCloudDriver; override;
  public
    function authorize: Boolean; override;
    procedure unauthorize; override;
    function authorized: Boolean; override;
  public
    procedure listFolderBegin( const path: String ); override;
    function  listFolderGetNextFile: TCloudFile; override;
    procedure listFolderEnd; override;
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
  public
    function getToken: TDropBoxToken;
    procedure setToken( const token: TDropBoxToken );
  end;

var
  dropBoxConfig: TDropBoxConfig;

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
procedure DropBoxClientProcessResult( const dropBoxResult: TDropBoxResult );
var
  httpResult: TMiniHttpResult;
  httpError: NSError;
  httpErrorDescription: String;
  dropBoxMessage: String;

  procedure processHttpError;
  begin
    httpResult:= dropBoxResult.httpResult;
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
      raise EDropBoxTokenException.Create( dropBoxMessage );
    raise EDropBoxException.Create( dropBoxMessage );
  end;

  procedure processDropBox409Error;
  begin
    if dropBoxMessage.IndexOf('not_found') >= 0 then
      raise EFileNotFoundException.Create( dropBoxMessage );
    if dropBoxMessage.IndexOf('conflict') >= 0 then
      raise EDropBoxConflictException.Create( dropBoxMessage );
    raise EDropBoxPermissionException.Create( dropBoxMessage );
  end;

  procedure processDropBoxError;
  begin
    dropBoxMessage:= dropBoxResult.resultMessage;

    if (httpResult.resultCode>=200) and (httpResult.resultCode<=299) then
      Exit;
    case httpResult.resultCode of
      401: processDropBox401Error;
      409: processDropBox409Error;
      403: raise EDropBoxPermissionException.Create( dropBoxMessage );
      429: raise EDropBoxRateLimitException.Create( dropBoxMessage );
      else raise EDropBoxException.Create( dropBoxMessage );
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

{ TDropBoxConfig }

constructor TDropBoxConfig.Create( const clientID: String; const listenURI: String );
begin
  _clientID:= clientID;
  _listenURI:= listenURI;
end;

{ TDropBoxToken }

function TDropBoxToken.isValidAccessToken: Boolean;
var
  now: NSDate;
begin
  Result:= False;
  if _access = EmptyStr then
    Exit;
  now:= NSDate.new;
  if now.timeIntervalSince1970 < _accessExpirationTime then
    Result:= True;
  now.release;
end;

function TDropBoxToken.isValidFreshToken: Boolean;
begin
  Result:= _refresh <> EmptyStr;
end;

constructor TDropBoxToken.Create;
begin
end;

constructor TDropBoxToken.Create(const access: String; const refresh: String;
  const accessExpirationTime: NSTimeInterval);
begin
  _access:= access;
  _refresh:= refresh;
  _accessExpirationTime:= accessExpirationTime;
end;

function TDropBoxToken.clone: TDropBoxToken;
begin
  Result:= TDropBoxToken.Create( _access, _refresh, _accessExpirationTime );
end;

procedure TDropBoxToken.setExpiration(const seconds: Integer);
var
  now: NSDate;
  expirationDate: NSDate;
begin
  now:= NSDate.new;
  expirationDate:= now.dateByAddingTimeInterval( seconds - 300 );
  _accessExpirationTime:= expirationDate.timeIntervalSince1970;
  now.release;
end;

procedure TDropBoxToken.invalid;
begin
  _access:= EmptyStr;
  _refresh:= EmptyStr;
end;

{ TDropBoxAuthPKCESession }

procedure TDropBoxAuthPKCESession.requestAuthorization;
var
  queryItems: TQueryItemsDictonary;
  codeChallenge: String;
begin
  _codeVerifier:= TStringUtil.generateRandomString( 43 );
  _state:= TStringUtil.generateRandomString( 10 );
  codeChallenge:= THashUtil.sha256AndBase64( _codeVerifier ) ;

  queryItems:= TQueryItemsDictonary.Create;
  queryItems.Add( 'client_id', _config.clientID );
  queryItems.Add( 'redirect_uri', _config.listenURI );
  queryItems.Add( 'code_challenge', codeChallenge );
  queryItems.Add( 'code_challenge_method', 'S256' );
  queryItems.Add( 'response_type', 'code' );
  queryItems.Add( 'token_access_type', 'offline' );
  queryItems.Add( 'state', _state );
  THttpClientUtil.openInSafari( DropBoxConst.URI.OAUTH2, queryItems );
end;

procedure TDropBoxAuthPKCESession.waitAuthorizationAndPrompt;
begin
  NSApplication(NSAPP).setOpenURLObserver( @self.onRedirect );
  _alert:= NSAlert.new;
  _alert.setMessageText( StringToNSString('Waiting for DropBox authorization') );
  _alert.setInformativeText( StringToNSString('Please login your DropBox account in Safari and authorize Double Commander to access. '#13'The authorization is completed on the DropBox official website, Double Command will not get your password.') );
  _alert.addButtonWithTitle( NSSTR('Cancel') );
  _alert.runModal;
  NSApplication(NSAPP).setOpenURLObserver( nil );
  _alert.release;
  _alert:= nil;
end;

procedure TDropBoxAuthPKCESession.closePrompt;
var
  button: NSButton;
begin
  if _alert = nil then
    Exit;

  button:= NSButton( _alert.buttons.objectAtIndex(0) );
  button.performClick( nil );
end;

procedure TDropBoxAuthPKCESession.requestToken;
var
  http: TMiniHttpClient;
  dropBoxResult: TDropBoxResult;

  procedure doRequest;
  var
    queryItems: TQueryItemsDictonary;
  begin
    queryItems:= TQueryItemsDictonary.Create;
    queryItems.Add( 'client_id', _config.clientID );
    queryItems.Add( 'redirect_uri', _config.listenURI );
    queryItems.Add( 'code', _code );
    queryItems.Add( 'code_verifier', _codeVerifier );
    queryItems.Add( 'grant_type', 'authorization_code' );
    dropBoxResult:= TDropBoxResult.Create;
    dropBoxResult.httpResult:= http.post( DropBoxConst.URI.TOKEN, queryItems );
    dropBoxResult.resultMessage:= dropBoxResult.httpResult.body;

    DropBoxClientProcessResult( dropBoxResult );
  end;

  procedure analyseResult;
  var
    json: NSDictionary;
  begin
    json:= TJsonUtil.parse( dropBoxResult.httpResult.body );
    _token.access:= TJsonUtil.getString( json, 'access_token' );
    _token.refresh:= TJsonUtil.getString( json, 'refresh_token' );
    _token.setExpiration( TJsonUtil.getInteger( json, 'expires_in' ) );
    _accountID:= TJsonUtil.getString( json, 'account_id' );
  end;

begin
  if _code = EmptyStr then
    Exit;

  try
    http:= TMiniHttpClient.Create;
    doRequest;

    if dropBoxResult.httpResult.resultCode <> 200 then
      Exit;
    analyseResult;
    cloudDriverManager.driverUpdated( _dropBoxClient );
  finally
    FreeAndNil( dropBoxResult );
    FreeAndNil( http );
  end;
end;

procedure TDropBoxAuthPKCESession.revokeToken;
var
  http: TMiniHttpClient;
begin
  try
    http:= TMiniHttpClient.Create;
    self.setAuthHeader( http );
    http.post( DropBoxConst.URI.REVOKE_TOKEN, nil );
  finally
    _token.invalid;
    FreeAndNil( http );
  end;
end;

procedure TDropBoxAuthPKCESession.refreshToken;
var
  http: TMiniHttpClient;
  dropBoxResult: TDropBoxResult;

  procedure doRequest;
  var
    queryItems: TQueryItemsDictonary;
  begin
    queryItems:= TQueryItemsDictonary.Create;
    queryItems.Add( 'client_id', _config.clientID );
    queryItems.Add( 'grant_type', 'refresh_token' );
    queryItems.Add( 'refresh_token', _token.refresh );
    dropBoxResult:= TDropBoxResult.Create;
    dropBoxResult.httpResult:= http.post( DropBoxConst.URI.TOKEN, queryItems );
    dropBoxResult.resultMessage:= dropBoxResult.httpResult.body;

    DropBoxClientProcessResult( dropBoxResult );
  end;

  procedure analyseResult;
  var
    json: NSDictionary;
  begin
    json:= TJsonUtil.parse( dropBoxResult.httpResult.body );
    _token.access:= TJsonUtil.getString( json, 'access_token' );
    _token.setExpiration( TJsonUtil.getInteger( json, 'expires_in' ) );
  end;

begin
  try
    http:= TMiniHttpClient.Create;
    doRequest;

    if dropBoxResult.httpResult.resultCode <> 200 then
      Exit;
    analyseResult;
    cloudDriverManager.driverUpdated( _dropBoxClient );
  finally
    FreeAndNil( dropBoxResult );
    FreeAndNil( http );
  end;
end;

procedure TDropBoxAuthPKCESession.onRedirect(const url: NSURL);
var
  components: NSURLComponents;
  state: String;
begin
  components:= NSURLComponents.componentsWithURL_resolvingAgainstBaseURL( url, False );
  state:= THttpClientUtil.queryValue( components, 'state' );
  if state <> _state then
    Exit;
  _code:= THttpClientUtil.queryValue( components, 'code' );
  closePrompt;
end;

function TDropBoxAuthPKCESession.getAccessToken: String;
  procedure checkToken;
  begin
    try
      if NOT _token.isValidAccessToken then begin
        if _token.isValidFreshToken then begin
          self.refreshToken;
        end else begin
          self.authorize;
        end;
      end;
    except
      on e: EDropBoxTokenException do begin
        TLogUtil.logError( 'Token Error: ' + e.ClassName + ': ' + e.Message );
        _token.invalid;
        self.authorize;
      end;
    end;
  end;

begin
  _lockObject.Acquire;
  try
    checkToken;
    Result:= _token.access;
  finally
    _lockObject.Release;
  end;
end;

constructor TDropBoxAuthPKCESession.Create(const config: TDropBoxConfig; const dropBoxClient: TCloudDriver );
begin
  _config:= config;
  _dropBoxClient:= dropBoxClient;
  _token:= TDropBoxToken.Create;
  _lockObject:= TCriticalSection.Create;
end;

destructor TDropBoxAuthPKCESession.Destroy;
begin
  FreeAndNil( _token );
  FreeAndNil( _lockObject );
end;

function TDropBoxAuthPKCESession.clone( const dropBoxClient: TCloudDriver ): TDropBoxAuthPKCESession;
begin
  Result:= TDropBoxAuthPKCESession.Create( _config, dropBoxClient );
  Result._accountID:= self._accountID;
  Result._token:= self._token.clone;
end;

function TDropBoxAuthPKCESession.authorize: Boolean;
begin
  _lockObject.Acquire;
  try
    requestAuthorization;
    TThread.Synchronize( TThread.CurrentThread, @waitAuthorizationAndPrompt );
    requestToken;
    Result:= self.authorized;
  finally
    _codeVerifier:= EmptyStr;
    _state:= EmptyStr;
    _code:= EmptyStr;
    _lockObject.Release;
  end;
end;

procedure TDropBoxAuthPKCESession.unauthorize;
begin
  _lockObject.Acquire;
  try
    revokeToken;
  finally
    _lockObject.Release;
  end;
end;

function TDropBoxAuthPKCESession.authorized: Boolean;
begin
  Result:= (_token.access <> EmptyStr);
end;

procedure TDropBoxAuthPKCESession.setAuthHeader(http: TMiniHttpClient);
var
  access: String;
begin
  access:= self.getAccessToken;
  http.addHeader( DropBoxConst.HEADER.AUTH, 'Bearer ' + access );
end;

procedure TDropBoxAuthPKCESession.setToken(const token: TDropBoxToken);
var
  oldToken: TDropBoxToken;
begin
  oldToken:= _token;
  _token:= token;
  oldToken.Free;
end;

function TDropBoxAuthPKCESession.getToken: TDropBoxToken;
begin
  Result:= _token;
end;

{ TDropBoxListFolderSession }

procedure TDropBoxListFolderSession.listFolderFirst;
var
  http: TMiniHttpClient;
  httpResult: TMiniHttpResult;
  dropBoxResult: TDropBoxResult;
  body: String;
begin
  try
    body:= TJsonUtil.dumps( ['path', _path] );
    http:= TMiniHttpClient.Create;
    _authSession.setAuthHeader( http );
    http.setContentType( HttpConst.ContentType.JSON );
    http.setBody( body );

    dropBoxResult:= TDropBoxResult.Create;
    httpResult:= http.post( DropBoxConst.URI.LIST_FOLDER, nil );
    dropBoxResult.httpResult:= httpResult;
    dropBoxResult.resultMessage:= httpResult.body;

    if Assigned(_files) then
      _files.Free;
    _files:= TCloudFiles.Create;
    if httpResult.resultCode = 200 then
      analyseListResult( httpResult.body );

    DropBoxClientProcessResult( dropBoxResult );
  finally
    FreeAndNil( dropBoxResult );
    FreeAndNil( http );
  end;
end;

procedure TDropBoxListFolderSession.listFolderContinue;
var
  http: TMiniHttpClient;
  httpResult: TMiniHttpResult;
  dropBoxResult: TDropBoxResult;
  body: String;
begin
  try
    body:= TJsonUtil.dumps( ['cursor', _cursor] );
    http:= TMiniHttpClient.Create;
    _authSession.setAuthHeader( http );
    http.setContentType( HttpConst.ContentType.JSON );
    http.setBody( body );

    dropBoxResult:= TDropBoxResult.Create;
    httpResult:= http.post( DropBoxConst.URI.LIST_FOLDER_CONTINUE, nil );
    dropBoxResult.httpResult:= httpResult;
    dropBoxResult.resultMessage:= httpResult.body;

    if httpResult.resultCode = 200 then
      analyseListResult( httpResult.body );

    DropBoxClientProcessResult( dropBoxResult );
  finally
    FreeAndNil( dropBoxResult );
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

constructor TDropBoxListFolderSession.Create( const authSession: TDropBoxAuthPKCESession; const path: String );
begin
  _authSession:= authSession;
  if path <> '/' then
    _path:= path;
end;

destructor TDropBoxListFolderSession.Destroy;
begin
  FreeAndNil( _files );
end;

function TDropBoxListFolderSession.getNextFile: TCloudFile;
  function popFirst: TCloudFile;
  begin
    if _files.Count > 0 then begin
      Result:= TCloudFile( _files.First );
      _files.Delete( 0 );
    end else begin
      Result:= nil;
    end;
  end;

begin
  Result:= popFirst;
  if (Result=nil) and _hasMore then begin
    listFolderContinue;
    Result:= popFirst;
  end;
end;

{ TDropBoxDownloadSession }

constructor TDropBoxDownloadSession.Create(
  const authSession: TDropBoxAuthPKCESession;
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback );
begin
  _authSession:= authSession;
  _serverPath:= serverPath;
  _localPath:= localPath;
  _callback:= callback;
end;

procedure TDropBoxDownloadSession.download;
var
  http: TMiniHttpClient;
  argJsonString: String;
  dropBoxResult: TDropBoxResult;
begin
  try
    argJsonString:= TJsonUtil.dumps( ['path', _serverPath], True );
    http:= TMiniHttpClient.Create;
    _authSession.setAuthHeader( http );
    http.addHeader( DropBoxConst.HEADER.ARG, argJsonString );

    dropBoxResult:= TDropBoxResult.Create;
    dropBoxResult.httpResult:= http.download( DropBoxConst.URI.DOWNLOAD, _localPath, _callback );
    dropBoxResult.resultMessage:= dropBoxResult.httpResult.getHeader( DropBoxConst.HEADER.RESULT );

    DropBoxClientProcessResult( dropBoxResult );
  finally
    FreeAndNil( dropBoxResult );
    FreeAndNil( http );
  end;
end;

{ TDropBoxUploadSession }

constructor TDropBoxUploadSession.Create(
  const authSession: TDropBoxAuthPKCESession; const serverPath: String;
  const localPath: String; const callback: ICloudProgressCallback);
begin
  _authSession:= authSession;
  _serverPath:= serverPath;
  _localPath:= localPath;
  _callback:= callback;
end;

procedure TDropBoxUploadSession.uploadSmall;
var
  http: TMiniHttpClient;
  argJsonString: String;
  dropBoxResult: TDropBoxResult;
begin
  try
    argJsonString:= TJsonUtil.dumps( ['path',_serverPath, 'mode','overwrite'], True );
    http:= TMiniHttpClient.Create;
    _authSession.setAuthHeader( http );
    http.addHeader( DropBoxConst.HEADER.ARG, argJsonString );

    dropBoxResult:= TDropBoxResult.Create;
    dropBoxResult.httpResult:= http.upload( DropBoxConst.URI.UPLOAD_SMALL, _localPath, _callback );
    dropBoxResult.resultMessage:= dropBoxResult.httpResult.body;

    DropBoxClientProcessResult( dropBoxResult );
  finally
    FreeAndNil( dropBoxResult );
    FreeAndNil( http );
  end;
end;

procedure TDropBoxUploadSession.uploadLarge;
var
  sessionId: String;
  offset: Integer;
  sessionSize: Integer;

  procedure uploadStart;
  var
    http: TMiniHttpClient;
    dropBoxResult: TDropBoxResult;
    json: NSDictionary;
  begin
    try
      http:= TMiniHttpClient.Create;
      _authSession.setAuthHeader( http );
      http.setContentType( HttpConst.ContentType.OctetStream );

      dropBoxResult:= TDropBoxResult.Create;
      dropBoxResult.httpResult:= http.post( DropBoxConst.URI.UPLOAD_LARGE_START, nil );
      dropBoxResult.resultMessage:= dropBoxResult.httpResult.body;

      DropBoxClientProcessResult( dropBoxResult );

      json:= TJsonUtil.parse( dropBoxResult.resultMessage );
      sessionId:= TJsonUtil.getString( json, 'session_id' );
      if sessionId = EmptyStr then
        raise EDropBoxException.Create( 'can''t get session_id in TDropBoxUploadSession.uploadLarge()' );
    finally
      FreeAndNil( dropBoxResult );
      FreeAndNil( http );
    end;
  end;

  procedure uploadAppend( range: NSRange );
    function getArgJsonString: String;
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
    http: TMiniHttpClient;
    dropBoxResult: TDropBoxResult;
  begin
    try
      http:= TMiniHttpClient.Create;
      _authSession.setAuthHeader( http );
      http.addHeader( DropBoxConst.HEADER.ARG, getArgJsonString );

      dropBoxResult:= TDropBoxResult.Create;
      dropBoxResult.httpResult:= http.uploadRange(
        DropBoxConst.URI.UPLOAD_LARGE_APPEND,
        _localPath,
        range,
        _callback );
      dropBoxResult.resultMessage:= dropBoxResult.httpResult.body;

      DropBoxClientProcessResult( dropBoxResult );
    finally
      FreeAndNil( dropBoxResult );
      FreeAndNil( http );
    end;
  end;

  procedure uploadFinish;
    function getArgJsonString: String;
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
    http: TMiniHttpClient;
    dropBoxResult: TDropBoxResult;
  begin
    try
      http:= TMiniHttpClient.Create;
      _authSession.setAuthHeader( http );
      http.addHeader( DropBoxConst.HEADER.ARG, getArgJsonString );
      http.setContentType( HttpConst.ContentType.OctetStream );

      dropBoxResult:= TDropBoxResult.Create;
      dropBoxResult.httpResult:= http.post( DropBoxConst.URI.UPLOAD_LARGE_FINISH, nil );
      dropBoxResult.resultMessage:= dropBoxResult.httpResult.body;

      DropBoxClientProcessResult( dropBoxResult );
    finally
      FreeAndNil( dropBoxResult );
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
    uploadAppend( NSMakeRange(offset, sessionSize) );
    inc( offset, sessionSize );
  end;
  uploadFinish;
end;

procedure TDropBoxUploadSession.upload;
begin
  _localFileSize:= TFileUtil.filesize( _localPath );
  if _localFileSize < DropBoxConst.UPLOAD.LARGE_FILE_SIZE then
    uploadSmall
  else
    uploadLarge;
end;

{ TDropBoxCreateFolderSession }

constructor TDropBoxCreateFolderSession.Create(
  const authSession: TDropBoxAuthPKCESession; const path: String );
begin
  _authSession:= authSession;
  _path:= path;
end;

procedure TDropBoxCreateFolderSession.createFolder;
var
  http: TMiniHttpClient;
  dropBoxResult: TDropBoxResult;
  body: String;
begin
  try
    body:= TJsonUtil.dumps( ['path', _path] );
    http:= TMiniHttpClient.Create;
    http.setContentType( HttpConst.ContentType.JSON );
    _authSession.setAuthHeader( http );
    http.setBody( body );

    dropBoxResult:= TDropBoxResult.Create;
    dropBoxResult.httpResult:= http.post( DropBoxConst.URI.CREATE_FOLDER, nil );
    dropBoxResult.resultMessage:= dropBoxResult.httpResult.body;

    DropBoxClientProcessResult( dropBoxResult );
  finally
    FreeAndNil( dropBoxResult );
    FreeAndNil( http );
  end;
end;

{ TDropBoxDeleteSession }

constructor TDropBoxDeleteSession.Create(
  const authSession: TDropBoxAuthPKCESession; const path: String);
begin
  _authSession:= authSession;
  _path:= path;
end;

procedure TDropBoxDeleteSession.delete;
var
  http: TMiniHttpClient;
  body: String;
  dropBoxResult: TDropBoxResult;
begin
  try
    body:= TJsonUtil.dumps( ['path', _path] );
    http:= TMiniHttpClient.Create;
    http.setContentType( HttpConst.ContentType.JSON );
    _authSession.setAuthHeader( http );
    http.setBody( body );

    dropBoxResult:= TDropBoxResult.Create;
    dropBoxResult.httpResult:= http.post( DropBoxConst.URI.DELETE, nil );
    dropBoxResult.resultMessage:= dropBoxResult.httpResult.body;

    DropBoxClientProcessResult( dropBoxResult );
  finally
    FreeAndNil( dropBoxResult );
    FreeAndNil( http );
  end;
end;

{ TDropBoxCopyMoveSession }

constructor TDropBoxCopyMoveSession.Create( const authSession: TDropBoxAuthPKCESession;
  const fromPath: String; const toPath: String );
begin
  _authSession:= authSession;
  _fromPath:= fromPath;
  _toPath:= toPath;
end;

procedure TDropBoxCopyMoveSession.copyOrMove( const needToMove: Boolean );
var
  uri: String;
  http: TMiniHttpClient;
  dropBoxResult: TDropBoxResult;
  body: String;
begin
  try
    http:= TMiniHttpClient.Create;
    http.setContentType( HttpConst.ContentType.JSON );
    _authSession.setAuthHeader( http );

    body:= TJsonUtil.dumps( ['from_path', _fromPath, 'to_path', _toPath] );
    http.setBody( body );

    if needToMove then
      uri:= DropBoxConst.URI.MOVE
    else
      uri:= DropBoxConst.URI.COPY;

    dropBoxResult:= TDropBoxResult.Create;
    dropBoxResult.httpResult:= http.post( uri, nil );
    dropBoxResult.resultMessage:= dropBoxResult.httpResult.body;

    DropBoxClientProcessResult( dropBoxResult );
  finally
    FreeAndNil( dropBoxResult );
    FreeAndNil( http );
  end;
end;

procedure TDropBoxCopyMoveSession.copy;
begin
  copyOrMove( False );
end;

procedure TDropBoxCopyMoveSession.move;
begin
  copyOrMove( True );
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

constructor TDropBoxClient.Create(const config: TDropBoxConfig);
begin
  _config:= config;
  _authSession:= TDropBoxAuthPKCESession.Create( _config, self );
end;

destructor TDropBoxClient.Destroy;
begin
  FreeAndNil( _authSession );
  FreeAndNil( _listFolderSession );
end;

function TDropBoxClient.clone: TCloudDriver;
var
  newClient: TDropBoxClient;
begin
  newClient:= TDropBoxClient.Create( _config );
  newClient._authSession:= self._authSession.clone( self );
  Result:= newClient;
end;

function TDropBoxClient.authorize: Boolean;
begin
  Result:= _authSession.authorize;
end;

procedure TDropBoxClient.unauthorize;
begin
  _authSession.unauthorize;
end;

function TDropBoxClient.authorized: Boolean;
begin
  Result:= _authSession.authorized;
end;

procedure TDropBoxClient.listFolderBegin(const path: String);
begin
  if Assigned(_listFolderSession) then
    _listFolderSession.Free;
  _listFolderSession:= TDropBoxListFolderSession.Create( _authSession, path );
  _listFolderSession.listFolderFirst;
end;

function TDropBoxClient.listFolderGetNextFile: TCloudFile;
begin
  Result:= _listFolderSession.getNextFile;
end;

procedure TDropBoxClient.listFolderEnd;
begin
  FreeAndNil( _listFolderSession );
end;

procedure TDropBoxClient.download(
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback );
var
  session: TDropBoxDownloadSession;
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
  session: TDropBoxUploadSession;
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
  session: TDropBoxCreateFolderSession;
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
  session: TDropBoxDeleteSession;
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
  session: TDropBoxCopyMoveSession;
begin
  try
    session:= TDropBoxCopyMoveSession.Create( _authSession, fromPath, toPath );
    session.copyOrMove( needToMove );
  finally
    FreeAndNil( session );
  end;
end;

function TDropBoxClient.getToken: TDropBoxToken;
begin
  Result:= _authSession.getToken;
end;

procedure TDropBoxClient.setToken(const token: TDropBoxToken);
begin
  _authSession.setToken( token );
end;

finalization
  FreeAndNil( dropBoxConfig );

end.

