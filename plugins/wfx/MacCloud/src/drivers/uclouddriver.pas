unit uCloudDriver;

{$mode ObjFPC}{$H+}
{$interfaces corba}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, syncobjs,
  CocoaAll, uMiniCocoa,
  uMiniHttpClient, uMiniUtil;

type

  { TCloudDriverResult }

  TCloudDriverResult = class
  public
    httpResult: TMiniHttpResult;
    resultMessage: String;
  end;

  { ECloudDriverException }

  ECloudDriverException = class( Exception );
  ECloudDriverAuthException = class( ECloudDriverException );
  ECloudDriverConflictException = class( ECloudDriverException );
  ECloudDriverQuotaException = class( ECloudDriverException );
  ECloudDriverPermissionException = class( ECloudDriverException );
  ECloudDriverRateLimitException = class( ECloudDriverException );
  ECloudDriverNetworkException = class( ECloudDriverException );

  TCloudDriverResultProcessFunc = procedure ( const cloudDriverResult: TCloudDriverResult );

  { TTokenCloudDriverConfig }

  TTokenCloudDriverConfig = class
  private
    _clientID: String;
    _listenURI: String;
  public
    constructor Create( const aClientID: String; const aListenURI: String );
    property clientID: String read _clientID;
    property listenURI: String read _listenURI;
  end;

  TTokenCloudDriverConfigPtr = ^TTokenCloudDriverConfig;

  { TTokenCloudDriverConfigWithSecret }

  TTokenCloudDriverConfigWithSecret = class( TTokenCloudDriverConfig )
  private
    _clientSecret: String;
  public
    constructor Create( const aClientID: String; const aClientSecret: String; const aListenURI: String );
    property clientSecret: String read _clientSecret;
  end;

  { TCloudDriverToken }

  TCloudDriverToken = class
  private
    _access: String;
    _refresh: String;
    _accessExpirationTime: NSTimeInterval;
  private
  public
    constructor Create;
    constructor Create( const access: String; const refresh: String; const accessExpirationTime: NSTimeInterval );
    function clone: TCloudDriverToken;
  public
    procedure setExpiration( const seconds: Integer );
    procedure invalid;
    property access: String read _access write _access;
    property refresh: String read _refresh write _refresh;
    property accessExpirationTime: NSTimeInterval read _accessExpirationTime write _accessExpirationTime;
    function isValidAccessToken: Boolean;
    function isValidRefreshToken: Boolean;
  end;

  TCloudFile = class
  private
    _name: String;
    _size: QWord;
    _creationTime: TDateTime;
    _modificationTime: TDateTime;
    _isFolder: Boolean;
  public
    property name: String read _name write _name;
    property size: QWord read _size write _size;
    property creationTime: TDateTime read _creationTime write _creationTime;
    property modificationTime: TDateTime read _modificationTime write _modificationTime;
    property isFolder: Boolean read _isFolder write _isFolder;
  end;

  TCloudFiles = TFPList;

  ICloudProgressCallback = IMiniHttpDataCallback;

  TCloudDriverLister = class
  public
    procedure listFolderBegin; virtual; abstract;
    function  listFolderGetNextFile: TCloudFile; virtual; abstract;
    procedure listFolderEnd; virtual; abstract;
  end;

  TCloudDriverBase = class
  public
    function createLister( const path: String ): TCloudDriverLister; virtual; abstract;
  public
    procedure createFolder( const path: String ); virtual; abstract;
    procedure delete( const path: String; const isFolder: Boolean ); virtual; abstract;
    procedure copyOrMove( const fromPath: String; const toPath: String;
      const isFolder: Boolean; const needToMove: Boolean ); virtual; abstract;
  end;

  TCloudDriver = class( TCloudDriverBase )
  public
    class function driverName: String; virtual; abstract;
    class function createInstance: TCloudDriver; virtual; abstract;
  public
    function clone: TCloudDriver; virtual; abstract;
  public
    function authorize: Boolean; virtual; abstract;
    procedure unauthorize; virtual; abstract;
    function authorized: Boolean; virtual; abstract;
  public
    procedure download(
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback ); virtual; abstract;
    procedure upload(
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback ); virtual; abstract;
  end;

  TCloudDriverClass = class of TCloudDriver;

  TCloudDriverClasses = TFPList;

  ICloudDriverObserver = interface
    procedure driverUpdated( const driver: TCloudDriver );
  end;

  { TCloudDriverManager }

  TCloudDriverManager = class
  private
    _classes: TCloudDriverClasses;
    _observer: ICloudDriverObserver;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure register( const cloudDriverClass: TCloudDriverClass );
    function find( const name: String ): TCloudDriverClass;
    function createInstance( const name: String ): TCloudDriver;
    function createInstance( const index: Integer ): TCloudDriver;
  public
    procedure driverUpdated( const driver: TCloudDriver );
  public
    property driverClasses: TCloudDriverClasses read _classes;
    property observer: ICloudDriverObserver write _observer;
  end;

  { TCloudDriverOAuth2SessionParams }

  TCloudDriverOAuth2SessionParams = record
    config: TTokenCloudDriverConfig;
    resultProcessFunc: TCloudDriverResultProcessFunc;
    scope: String;
    OAUTH2_URI: String;
    TOKEN_URI: String;
    REVOKE_TOKEN_URI: String;
    AUTH_TYPE: String;
  end;

  { TCloudDriverAuthSession }

  TCloudDriverAuthSession = class
  public
    procedure setAuthHeader( const http: TMiniHttpClient ); virtual; abstract;
    function clone( const driver: TCloudDriver ): TCloudDriverAuthSession; virtual; abstract;
  end;

  { TCloudDriverOAuth2Session }

  TCloudDriverOAuth2Session = class( TCloudDriverAuthSession )
  strict protected
    _driver: TCloudDriver;
    _params: TCloudDriverOAuth2SessionParams;
    _config: TTokenCloudDriverConfig;
    _state: String;
    _code: String;
    _token: TCloudDriverToken;
    _accountID: String;
    _alert: NSAlert;
    _lockObject: TCriticalSection;
  protected
    procedure onAuthorize( const queryItems: TQueryItemsDictonary ); virtual; abstract;
    procedure onRequestToken( const queryItems: TQueryItemsDictonary ); virtual; abstract;
    procedure onRefreshToken( const queryItems: TQueryItemsDictonary ); virtual; abstract;
    procedure onRevokeToken( const http: TMiniHttpClient ); virtual; abstract;
  protected
    procedure requestAuthorization;
    procedure waitAuthorizationAndPrompt;
    procedure closePrompt;
    procedure requestToken;
    procedure revokeToken;
    procedure refreshToken;
    procedure analyseTokenResult( const jsonString: String );
    procedure onRedirect( const url: NSURL );
    function getAccessToken: String;
  public
    constructor Create( const driver: TCloudDriver; const params: TCloudDriverOAuth2SessionParams );
    destructor Destroy; override;
  public
    function authorize: Boolean;
    procedure unauthorize;
    function authorized: Boolean;
    procedure setAuthHeader( const http: TMiniHttpClient ); override;
    procedure setToken( const token: TCloudDriverToken );
    function getToken: TCloudDriverToken;
  end;

  { TCloudDriverOAuth2PKCESession }

  TCloudDriverOAuth2PKCESession = class( TCloudDriverOAuth2Session )
  strict private
    _codeVerifier: String;
  protected
    procedure onAuthorize( const queryItems: TQueryItemsDictonary ); override;
    procedure onRequestToken( const queryItems: TQueryItemsDictonary ); override;
    procedure onRefreshToken( const queryItems: TQueryItemsDictonary ); override;
    procedure onRevokeToken( const http: TMiniHttpClient ); override;
  public
    function clone(const driver: TCloudDriver): TCloudDriverOAuth2Session; override;
  end;

  { TCloudDriverOAuth2SecretSession }

  TCloudDriverOAuth2SecretSession = class( TCloudDriverOAuth2Session )
  protected
    procedure onAuthorize( const queryItems: TQueryItemsDictonary ); override;
    procedure onRequestToken( const queryItems: TQueryItemsDictonary ); override;
    procedure onRefreshToken( const queryItems: TQueryItemsDictonary ); override;
    procedure onRevokeToken( const http: TMiniHttpClient ); override;
  public
    function clone(const driver: TCloudDriver): TCloudDriverOAuth2Session; override;
  end;

  { TCloudDriverListFolderSession }

  TCloudDriverListFolderSession = class
  protected
    _authSession: TCloudDriverAuthSession;
    _path: String;
    _files: TCloudFiles;
    _hasMore: Boolean;
  protected
    procedure listFolderFirst; virtual; abstract;
    procedure listFolderContinue; virtual; abstract;
  public
    constructor Create( const authSession: TCloudDriverAuthSession; const path: String ); virtual;
    destructor Destroy; override;
    function getNextFile: TCloudFile;
  end;

  TCloudDriverListFolderSessionClass = class of TCloudDriverListFolderSession;

  { TCloudDriverDownloadSession }

  TCloudDriverDownloadSession = class
  protected
    _authSession: TCloudDriverAuthSession;
    _serverPath: String;
    _localPath: String;
    _callback: ICloudProgressCallback;
  public
    constructor Create(
      const authSession: TCloudDriverAuthSession;
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback );
    procedure download; virtual; abstract;
  end;

  { TCloudDriverUploadSession }

  TCloudDriverUploadSession = class
  protected
    _authSession: TCloudDriverAuthSession;
    _serverPath: String;
    _localPath: String;
    _localFileSize: Integer;
    _callback: ICloudProgressCallback;
  public
    constructor Create(
      const authSession: TCloudDriverAuthSession;
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback );
    procedure upload; virtual; abstract;
  end;

  { TCloudDriverCreateFolderSession }

  TCloudDriverCreateFolderSession = class
  protected
    _authSession: TCloudDriverAuthSession;
    _path: String;
  public
    constructor Create( const authSession: TCloudDriverAuthSession; const path: String );
    procedure createFolder; virtual; abstract;
  end;

  { TCloudDriverDeleteSession }

  TCloudDriverDeleteSession = class
  protected
    _authSession: TCloudDriverAuthSession;
    _path: String;
    _isFolder: Boolean;
  public
    constructor Create( const authSession: TCloudDriverAuthSession;
      const path: String; const isFolder: Boolean );
    procedure delete; virtual; abstract;
  end;

  { TCloudDriverCopyMoveSession }

  TCloudDriverCopyMoveSession = class
  protected
    _authSession: TCloudDriverAuthSession;
    _fromPath: String;
    _toPath: String;
    _isFolder: Boolean;
  public
    constructor Create(
      const authSession: TCloudDriverAuthSession;
      const fromPath: String;
      const toPath: String;
      const isFolder: Boolean );
    procedure copyOrMove( const needToMove: Boolean ); virtual; abstract;
    procedure copy;
    procedure move;
  end;

  { TCloudDriverDefaultLister }

  TCloudDriverDefaultLister = class( TCloudDriverLister )
  private
    _listFolderSession: TCloudDriverListFolderSession;
  public
    constructor Create(
      const sessionClass: TCloudDriverListFolderSessionClass;
      const authSession: TCloudDriverAuthSession;
      const path: String );
    destructor Destroy; override;
    procedure listFolderBegin; override;
    function  listFolderGetNextFile: TCloudFile; override;
    procedure listFolderEnd; override;
  end;


  { TTokenCloudDriver }

  TTokenCloudDriver = class( TCloudDriver )
  public
    function getToken: TCloudDriverToken; virtual; abstract;
    procedure setToken( const token: TCloudDriverToken ); virtual; abstract;
  end;

  { TOAuth2SessionCloudDriver }

  TOAuth2SessionCloudDriver = class( TTokenCloudDriver )
  protected
    _config: TTokenCloudDriverConfig;
    _authSession: TCloudDriverOAuth2Session;
  public
    constructor Create( const config: TTokenCloudDriverConfig );
    destructor Destroy; override;
  public
    function authorize: Boolean; override;
    procedure unauthorize; override;
    function authorized: Boolean; override;
    function getToken: TCloudDriverToken; override;
    procedure setToken( const token: TCloudDriverToken ); override;
  end;

var
  cloudDriverManager: TCloudDriverManager;

implementation

{ TTokenCloudDriverConfig }

constructor TTokenCloudDriverConfig.Create(
  const aClientID: String;
  const aListenURI: String );
begin
  _clientID:= aClientID;
  _listenURI:= aListenURI;
end;

{ TTokenCloudDriverConfigWithSecret }

constructor TTokenCloudDriverConfigWithSecret.Create(const aClientID: String;
  const aClientSecret: String; const aListenURI: String);
begin
  Inherited Create( aClientID, aListenURI );
  _clientSecret:= aClientSecret;
end;

{ TCloudDriverToken }

function TCloudDriverToken.isValidAccessToken: Boolean;
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

function TCloudDriverToken.isValidRefreshToken: Boolean;
begin
  Result:= _refresh <> EmptyStr;
end;

constructor TCloudDriverToken.Create;
begin
end;

constructor TCloudDriverToken.Create(const access: String; const refresh: String;
  const accessExpirationTime: NSTimeInterval);
begin
  _access:= access;
  _refresh:= refresh;
  _accessExpirationTime:= accessExpirationTime;
end;

function TCloudDriverToken.clone: TCloudDriverToken;
begin
  Result:= TCloudDriverToken.Create( _access, _refresh, _accessExpirationTime );
end;

procedure TCloudDriverToken.setExpiration(const seconds: Integer);
var
  now: NSDate;
  expirationDate: NSDate;
begin
  now:= NSDate.new;
  expirationDate:= now.dateByAddingTimeInterval( seconds - 300 );
  _accessExpirationTime:= expirationDate.timeIntervalSince1970;
  now.release;
end;

procedure TCloudDriverToken.invalid;
begin
  _access:= EmptyStr;
  _refresh:= EmptyStr;
  _accessExpirationTime:= 0;
end;

{ TCloudDriverListFolderSession }

constructor TCloudDriverListFolderSession.Create(
  const authSession: TCloudDriverAuthSession; const path: String);
begin
  _authSession:= authSession;
  _files:= TCloudFiles.Create;
  _path:= path;
end;

destructor TCloudDriverListFolderSession.Destroy;
begin
  FreeAndNil( _files );
end;

function TCloudDriverListFolderSession.getNextFile: TCloudFile;
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

{ TCloudDriverDownloadSession }

constructor TCloudDriverDownloadSession.Create(
  const authSession: TCloudDriverAuthSession;
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback );
begin
  _authSession:= authSession;
  _serverPath:= serverPath;
  _localPath:= localPath;
  _callback:= callback;
end;

{ TCloudDriverUploadSession }

constructor TCloudDriverUploadSession.Create(
  const authSession: TCloudDriverAuthSession;
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback );
begin
  _authSession:= authSession;
  _serverPath:= serverPath;
  _localPath:= localPath;
  _callback:= callback;
  _localFileSize:= TFileUtil.filesize( _localPath );
end;

{ TCloudDriverCreateFolderSession }

constructor TCloudDriverCreateFolderSession.Create(
  const authSession: TCloudDriverAuthSession; const path: String);
begin
  _authSession:= authSession;
  _path:= path;
end;

{ TCloudDriverDeleteSession }

constructor TCloudDriverDeleteSession.Create(
  const authSession: TCloudDriverAuthSession;
  const path: String; const isFolder: Boolean );
begin
  _authSession:= authSession;
  _path:= path;
  _isFolder:= isFolder;
end;

{ TCloudDriverCopyMoveSession }

constructor TCloudDriverCopyMoveSession.Create(
  const authSession: TCloudDriverAuthSession;
  const fromPath: String;
  const toPath: String;
  const isFolder: Boolean );
begin
  _authSession:= authSession;
  _fromPath:= fromPath;
  _toPath:= toPath;
  _isFolder:= isFolder;
end;

procedure TCloudDriverCopyMoveSession.copy;
begin
  copyOrMove( False );
end;

procedure TCloudDriverCopyMoveSession.move;
begin
  copyOrMove( True );
end;

{ TCloudDriverDefaultLister }

constructor TCloudDriverDefaultLister.Create(
  const sessionClass: TCloudDriverListFolderSessionClass;
  const authSession: TCloudDriverAuthSession;
  const path: String);
begin
  _listFolderSession:= sessionClass.Create( authSession, path );
end;

destructor TCloudDriverDefaultLister.Destroy;
begin
  FreeAndNil( _listFolderSession );
end;

procedure TCloudDriverDefaultLister.listFolderBegin;
begin
  _listFolderSession.listFolderFirst;
end;

function TCloudDriverDefaultLister.listFolderGetNextFile: TCloudFile;
begin
  Result:= _listFolderSession.getNextFile;
end;

procedure TCloudDriverDefaultLister.listFolderEnd;
begin
  self.Free;
end;

{ TCloudDriverOAuth2Session }

procedure TCloudDriverOAuth2Session.requestAuthorization;
var
  queryItems: TQueryItemsDictonary;
begin
  _state:= TStringUtil.generateRandomString( 10 );
  queryItems:= TQueryItemsDictonary.Create;
  queryItems.Add( 'client_id', _config.clientID );
  queryItems.Add( 'redirect_uri', _config.listenURI );
  queryItems.Add( 'response_type', 'code' );
  queryItems.Add( 'token_access_type', 'offline' );
  queryItems.Add( 'state', _state );
  if _params.scope <> EmptyStr then
    queryItems.Add( 'scope', _params.scope );
  self.onAuthorize( queryItems );
  THttpClientUtil.openInSafari( _params.OAUTH2_URI, queryItems );
end;

procedure TCloudDriverOAuth2Session.waitAuthorizationAndPrompt;
begin
  NSApplication(NSAPP).setOpenURLObserver( @self.onRedirect );
  _alert:= NSAlert.new;
  _alert.setMessageText( StringToNSString('Waiting for ' + _driver.driverName + ' authorization') );
  _alert.setInformativeText( StringToNSString('Please login your ' + _driver.driverName + ' account in Safari and authorize Double Commander to access. '#13'The authorization is completed on the ' + _driver.driverName + ' official website, Double Command will not get your password.') );
  _alert.addButtonWithTitle( NSSTR('Cancel') );
  _alert.runModal;
  NSApplication(NSAPP).setOpenURLObserver( nil );
  _alert.release;
  _alert:= nil;
end;

procedure TCloudDriverOAuth2Session.closePrompt;
var
  button: NSButton;
begin
  if _alert = nil then
    Exit;

  button:= NSButton( _alert.buttons.objectAtIndex(0) );
  button.performClick( nil );
end;

procedure TCloudDriverOAuth2Session.requestToken;
var
  http: TMiniHttpClient = nil;
  cloudDriverResult: TCloudDriverResult = nil;

  procedure doRequest;
  var
    queryItems: TQueryItemsDictonary;
  begin
    queryItems:= TQueryItemsDictonary.Create;
    queryItems.Add( 'client_id', _config.clientID );
    queryItems.Add( 'redirect_uri', _config.listenURI );
    queryItems.Add( 'code', _code );
    queryItems.Add( 'grant_type', 'authorization_code' );
    self.onRequestToken( queryItems );
    http.setQueryParams( queryItems );
    http.setContentType( HttpConst.ContentType.UrlEncoded );
    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    _params.resultProcessFunc( cloudDriverResult );
  end;

begin
  if _code = EmptyStr then
    Exit;

  try
    http:= TMiniHttpClient.Create( _params.TOKEN_URI, HttpConst.Method.POST );
    doRequest;

    if cloudDriverResult.httpResult.resultCode <> 200 then
      Exit;
    self.analyseTokenResult( cloudDriverResult.httpResult.body );
    cloudDriverManager.driverUpdated( _driver );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TCloudDriverOAuth2Session.revokeToken;
var
  http: TMiniHttpClient = nil;
begin
  try
    http:= TMiniHttpClient.Create( _params.REVOKE_TOKEN_URI, HttpConst.Method.POST );
    self.onRevokeToken( http );
    self.setAuthHeader( http );
    http.connect;
  finally
    _token.invalid;
    FreeAndNil( http );
  end;
end;

procedure TCloudDriverOAuth2Session.refreshToken;
var
  http: TMiniHttpClient = nil;
  cloudDriverResult: TCloudDriverResult = nil;

  procedure doRequest;
  var
    queryItems: TQueryItemsDictonary;
  begin
    queryItems:= TQueryItemsDictonary.Create;
    queryItems.Add( 'client_id', _config.clientID );
    queryItems.Add( 'grant_type', 'refresh_token' );
    queryItems.Add( 'refresh_token', _token.refresh );
    self.onRefreshToken( queryItems );
    http.setQueryParams( queryItems );
    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
  end;

begin
  try
    http:= TMiniHttpClient.Create( _params.TOKEN_URI, HttpConst.Method.POST );
    doRequest;

    if cloudDriverResult.httpResult.resultCode <> 200 then
      raise ECloudDriverAuthException.Create( 'RefreshToken Error' );
    self.analyseTokenResult( cloudDriverResult.httpResult.body );
    cloudDriverManager.driverUpdated( _driver );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TCloudDriverOAuth2Session.analyseTokenResult( const jsonString: String );
var
  json: NSDictionary;
begin
  json:= TJsonUtil.parse( jsonString );
  _token.access:= TJsonUtil.getString( json, 'access_token' );
  _token.refresh:= TJsonUtil.getString( json, 'refresh_token' );
  _token.setExpiration( TJsonUtil.getInteger( json, 'expires_in' ) );
  _accountID:= TJsonUtil.getString( json, 'account_id' );
end;


procedure TCloudDriverOAuth2Session.onRedirect(const url: NSURL);
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

function TCloudDriverOAuth2Session.getAccessToken: String;
  procedure checkToken;
  begin
    try
      if NOT _token.isValidAccessToken then begin
        if _token.isValidRefreshToken then begin
          self.refreshToken;
        end else begin
          self.authorize;
        end;
      end;
    except
      on e: ECloudDriverAuthException do begin
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

constructor TCloudDriverOAuth2Session.Create(
  const driver: TCloudDriver;
  const params: TCloudDriverOAuth2SessionParams );
begin
  _driver:= driver;
  _params:= params;
  _config:= _params.config;
  _token:= TCloudDriverToken.Create;
  _lockObject:= TCriticalSection.Create;
end;

destructor TCloudDriverOAuth2Session.Destroy;
begin
  FreeAndNil( _token );
  FreeAndNil( _lockObject );
end;

function TCloudDriverOAuth2Session.authorize: Boolean;
begin
  _lockObject.Acquire;
  try
    requestAuthorization;
    TThread.Synchronize( TThread.CurrentThread, @waitAuthorizationAndPrompt );
    requestToken;
    Result:= self.authorized;
  finally
    _state:= EmptyStr;
    _code:= EmptyStr;
    _lockObject.Release;
  end;
end;

procedure TCloudDriverOAuth2Session.unauthorize;
begin
  _lockObject.Acquire;
  try
    revokeToken;
  finally
    _lockObject.Release;
  end;
end;

function TCloudDriverOAuth2Session.authorized: Boolean;
begin
  Result:= (_token.access <> EmptyStr);
end;

procedure TCloudDriverOAuth2Session.setAuthHeader( const http: TMiniHttpClient );
var
  access: String;
  authString: NSString;
begin
  access:= self.getAccessToken;
  authString:= StringToNSString( _params.AUTH_TYPE + ' ' + access );
  http.addHeader( HttpConst.Header.Authorization, authString );
end;

procedure TCloudDriverOAuth2Session.setToken(const token: TCloudDriverToken);
var
  oldToken: TCloudDriverToken;
begin
  oldToken:= _token;
  _token:= token;
  oldToken.Free;
end;

function TCloudDriverOAuth2Session.getToken: TCloudDriverToken;
begin
  Result:= _token;
end;

{ TCloudDriverOAuth2PKCESession }

procedure TCloudDriverOAuth2PKCESession.onAuthorize( const queryItems: TQueryItemsDictonary );
var
  codeChallenge: String;
begin
  _codeVerifier:= TStringUtil.generateRandomString( 43 );
  codeChallenge:= THashUtil.sha256AndBase64( _codeVerifier ) ;
  queryItems.Add( 'code_challenge', codeChallenge );
  queryItems.Add( 'code_challenge_method', 'S256' );
end;

procedure TCloudDriverOAuth2PKCESession.onRequestToken( const queryItems: TQueryItemsDictonary );
begin
  queryItems.Add( 'code_verifier', _codeVerifier );
  _codeVerifier:= EmptyStr;
end;

procedure TCloudDriverOAuth2PKCESession.onRefreshToken( const queryItems: TQueryItemsDictonary );
begin
end;

procedure TCloudDriverOAuth2PKCESession.onRevokeToken( const http: TMiniHttpClient );
begin
end;

function TCloudDriverOAuth2PKCESession.clone( const driver: TCloudDriver ): TCloudDriverOAuth2Session;
begin
  Result:= TCloudDriverOAuth2PKCESession.Create( driver, _params );
  Result._accountID:= _accountID;
  Result._token:= _token.clone;
end;

{ TCloudDriverOAuth2SecretSession }

procedure TCloudDriverOAuth2SecretSession.onAuthorize(
  const queryItems: TQueryItemsDictonary);
begin
  queryItems.Add( 'client_id', _config.clientID );
  queryItems.Add( 'token', _token.access );
end;

procedure TCloudDriverOAuth2SecretSession.onRequestToken( const queryItems: TQueryItemsDictonary );
var
  secret: String;
begin
  secret:= TTokenCloudDriverConfigWithSecret(_config).clientSecret;
  queryItems.Add( 'client_secret', secret );
end;

procedure TCloudDriverOAuth2SecretSession.onRefreshToken( const queryItems: TQueryItemsDictonary );
begin
  self.onRequestToken( queryItems );
end;

procedure TCloudDriverOAuth2SecretSession.onRevokeToken( const http: TMiniHttpClient );
var
  queryItems: TQueryItemsDictonary;
  secret: String;
begin
  secret:= TTokenCloudDriverConfigWithSecret(_config).clientSecret;
  queryItems:= TQueryItemsDictonary.Create;
  queryItems.Add( 'client_id', _config.clientID );
  queryItems.Add( 'client_secret', secret );
  queryItems.Add( 'token', _token.access );
  http.setQueryParams( queryItems );
end;

function TCloudDriverOAuth2SecretSession.clone( const driver: TCloudDriver ): TCloudDriverOAuth2Session;
begin
  Result:= TCloudDriverOAuth2SecretSession.Create( driver, _params );
  Result._accountID:= _accountID;
  Result._token:= _token.clone;
end;

{ TCloudDriverManager }

constructor TCloudDriverManager.Create;
begin
  _classes:= TCloudDriverClasses.Create;
end;

destructor TCloudDriverManager.Destroy;
begin
  _classes.Free;
end;

procedure TCloudDriverManager.register( const cloudDriverClass: TCloudDriverClass );
begin
  if _classes.IndexOf(cloudDriverClass) >= 0 then
    Exit;
  _classes.Add( cloudDriverClass );
end;

function TCloudDriverManager.find( const name: String ): TCloudDriverClass;
var
  i: Integer;
  cloudDriverClass: TCloudDriverClass;
begin
  for i:= 0 to _classes.Count - 1 do begin
    cloudDriverClass:= TCloudDriverClass( _classes[i] );
    if cloudDriverClass.driverName <> name then
      continue;
    Exit( cloudDriverClass );
  end;

  raise EArgumentException.Create( 'CloudDriver not found in TCloudDriverManager.find(): ' + name );
end;

function TCloudDriverManager.createInstance( const name: String ): TCloudDriver;
begin
  Result:= self.find(name).createInstance;
end;

function TCloudDriverManager.createInstance(const index: Integer): TCloudDriver;
begin
  Result:= TCloudDriverClass( self.driverClasses[index] ).createInstance;
end;

procedure TCloudDriverManager.driverUpdated(const driver: TCloudDriver);
begin
  if Assigned(_observer) then
    _observer.driverUpdated( driver );
end;

{ TOAuth2SessionCloudDriver }

constructor TOAuth2SessionCloudDriver.Create(
  const config: TTokenCloudDriverConfig );
begin
  _config:= config;
end;

destructor TOAuth2SessionCloudDriver.Destroy;
begin
  FreeAndNil( _authSession );
end;

function TOAuth2SessionCloudDriver.authorize: Boolean;
begin
  Result:= _authSession.authorize;
end;

procedure TOAuth2SessionCloudDriver.unauthorize;
begin
  _authSession.unauthorize;
end;

function TOAuth2SessionCloudDriver.authorized: Boolean;
begin
  Result:= _authSession.authorized;
end;

function TOAuth2SessionCloudDriver.getToken: TCloudDriverToken;
begin
  Result:= _authSession.getToken;
end;

procedure TOAuth2SessionCloudDriver.setToken(const token: TCloudDriverToken);
begin
  _authSession.setToken( token );
end;

end.

