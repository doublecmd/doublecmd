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
  ECloudDriverTokenException = class( ECloudDriverException );
  ECloudDriverConflictException = class( ECloudDriverException );
  ECloudDriverPermissionException = class( ECloudDriverException );
  ECloudDriverRateLimitException = class( ECloudDriverException );

  TCloudDriverResultProcessFunc = procedure ( const cloudDriverResult: TCloudDriverResult );

  { TCloudDriverConfig }

  TCloudDriverConfig = class
  private
    _clientID: String;
    _listenURI: String;
  public
    constructor Create( const clientID: String; const listenURI: String );
    property clientID: String read _clientID;
    property listenURI: String read _listenURI;
  end;

  TCloudDriverConfigPtr = ^TCloudDriverConfig;

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
    function isValidFreshToken: Boolean;
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
    procedure delete( const path: String ); virtual; abstract;
    procedure copyOrMove( const fromPath: String; const toPath: String; const needToMove: Boolean ); virtual; abstract;
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
    function getToken: TCloudDriverToken; virtual; abstract;
    procedure setToken( const token: TCloudDriverToken ); virtual; abstract;
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

  { TCloudDriverAuthPKCESessionParams }

  TCloudDriverAuthPKCESessionParams = record
    config: TCloudDriverConfig;
    resultProcessFunc: TCloudDriverResultProcessFunc;
    OAUTH2_URI: String;
    TOKEN_URI: String;
    REVOKE_TOKEN_URI: String;
    AUTH_HEADER: String;
    AUTH_TYPE: String;
  end;

  { TCloudDriverAuthPKCESession }

  TCloudDriverAuthPKCESession = class
  strict private
    _driver: TCloudDriver;
    _params: TCloudDriverAuthPKCESessionParams;
    _config: TCloudDriverConfig;
    _codeVerifier: String;
    _state: String;
    _code: String;
    _token: TCloudDriverToken;
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
    constructor Create( const driver: TCloudDriver; const params: TCloudDriverAuthPKCESessionParams );
    destructor Destroy; override;
    function clone( const driver: TCloudDriver ): TCloudDriverAuthPKCESession;
  public
    function authorize: Boolean;
    procedure unauthorize;
    function authorized: Boolean;
    procedure setAuthHeader( http: TMiniHttpClient );
    procedure setToken( const token: TCloudDriverToken );
    function getToken: TCloudDriverToken;
  end;

  { TCloudDriverListFolderSession }

  TCloudDriverListFolderSession = class
  protected
    _authSession: TCloudDriverAuthPKCESession;
    _path: String;
    _files: TCloudFiles;
    _hasMore: Boolean;
  protected
    procedure listFolderFirst; virtual; abstract;
    procedure listFolderContinue; virtual; abstract;
  public
    constructor Create( const authSession: TCloudDriverAuthPKCESession; const path: String ); virtual;
    destructor Destroy; override;
    function getNextFile: TCloudFile;
  end;

  TCloudDriverListFolderSessionClass = class of TCloudDriverListFolderSession;

  { TCloudDriverDownloadSession }

  TCloudDriverDownloadSession = class
  protected
    _authSession: TCloudDriverAuthPKCESession;
    _serverPath: String;
    _localPath: String;
    _callback: ICloudProgressCallback;
  public
    constructor Create(
      const authSession: TCloudDriverAuthPKCESession;
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback );
    procedure download; virtual; abstract;
  end;

  { TCloudDriverUploadSession }

  TCloudDriverUploadSession = class
  protected
    _authSession: TCloudDriverAuthPKCESession;
    _serverPath: String;
    _localPath: String;
    _localFileSize: Integer;
    _callback: ICloudProgressCallback;
  public
    constructor Create(
      const authSession: TCloudDriverAuthPKCESession;
      const serverPath: String;
      const localPath: String;
      const callback: ICloudProgressCallback );
    procedure upload; virtual; abstract;
  end;

  { TCloudDriverCreateFolderSession }

  TCloudDriverCreateFolderSession = class
  protected
    _authSession: TCloudDriverAuthPKCESession;
    _path: String;
  public
    constructor Create( const authSession: TCloudDriverAuthPKCESession; const path: String );
    procedure createFolder; virtual; abstract;
  end;

  { TCloudDriverDeleteSession }

  TCloudDriverDeleteSession = class
  protected
    _authSession: TCloudDriverAuthPKCESession;
    _path: String;
  public
    constructor Create( const authSession: TCloudDriverAuthPKCESession; const path: String );
    procedure delete; virtual; abstract;
  end;

  { TCloudDriverCopyMoveSession }

  TCloudDriverCopyMoveSession = class
  protected
    _authSession: TCloudDriverAuthPKCESession;
    _fromPath: String;
    _toPath: String;
  public
    constructor Create(
      const authSession: TCloudDriverAuthPKCESession;
      const fromPath: String;
      const toPath: String );
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
      const authSession: TCloudDriverAuthPKCESession;
      const path: String );
    destructor Destroy; override;
    procedure listFolderBegin; override;
    function  listFolderGetNextFile: TCloudFile; override;
    procedure listFolderEnd; override;
  end;

  { TAuthSessionCloudDriver }

  TAuthSessionCloudDriver = class( TCloudDriver )
  protected
    _config: TCloudDriverConfig;
    _authSession: TCloudDriverAuthPKCESession;
  public
    constructor Create(
      const config: TCloudDriverConfig;
      const authParams: TCloudDriverAuthPKCESessionParams );
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

{ TCloudDriverConfig }

constructor TCloudDriverConfig.Create(
  const clientID: String;
  const listenURI: String );
begin
  _clientID:= clientID;
  _listenURI:= listenURI;
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

function TCloudDriverToken.isValidFreshToken: Boolean;
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
  const authSession: TCloudDriverAuthPKCESession; const path: String);
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
  const authSession: TCloudDriverAuthPKCESession;
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
  const authSession: TCloudDriverAuthPKCESession;
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
  const authSession: TCloudDriverAuthPKCESession; const path: String);
begin
  _authSession:= authSession;
  _path:= path;
end;

{ TCloudDriverDeleteSession }

constructor TCloudDriverDeleteSession.Create(
  const authSession: TCloudDriverAuthPKCESession; const path: String);
begin
  _authSession:= authSession;
  _path:= path;
end;

{ TCloudDriverCopyMoveSession }

constructor TCloudDriverCopyMoveSession.Create(
  const authSession: TCloudDriverAuthPKCESession;
  const fromPath: String;
  const toPath: String );
begin
  _authSession:= authSession;
  _fromPath:= fromPath;
  _toPath:= toPath;
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
  const authSession: TCloudDriverAuthPKCESession;
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

{ TCloudDriverAuthPKCESession }

procedure TCloudDriverAuthPKCESession.requestAuthorization;
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
  THttpClientUtil.openInSafari( _params.OAUTH2_URI, queryItems );
end;

procedure TCloudDriverAuthPKCESession.waitAuthorizationAndPrompt;
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

procedure TCloudDriverAuthPKCESession.closePrompt;
var
  button: NSButton;
begin
  if _alert = nil then
    Exit;

  button:= NSButton( _alert.buttons.objectAtIndex(0) );
  button.performClick( nil );
end;

procedure TCloudDriverAuthPKCESession.requestToken;
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
    queryItems.Add( 'code_verifier', _codeVerifier );
    queryItems.Add( 'grant_type', 'authorization_code' );
    http.setQueryParams( queryItems );
    http.setContentType( HttpConst.ContentType.UrlEncoded );
    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    _params.resultProcessFunc( cloudDriverResult );
  end;

  procedure analyseResult;
  var
    json: NSDictionary;
  begin
    json:= TJsonUtil.parse( cloudDriverResult.httpResult.body );
    _token.access:= TJsonUtil.getString( json, 'access_token' );
    _token.refresh:= TJsonUtil.getString( json, 'refresh_token' );
    _token.setExpiration( TJsonUtil.getInteger( json, 'expires_in' ) );
    _accountID:= TJsonUtil.getString( json, 'account_id' );
  end;

begin
  if _code = EmptyStr then
    Exit;

  try
    http:= TMiniHttpClient.Create( _params.TOKEN_URI, HttpConst.Method.POST );
    doRequest;

    if cloudDriverResult.httpResult.resultCode <> 200 then
      Exit;
    analyseResult;
    cloudDriverManager.driverUpdated( _driver );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TCloudDriverAuthPKCESession.revokeToken;
var
  http: TMiniHttpClient = nil;
begin
  try
    http:= TMiniHttpClient.Create( _params.REVOKE_TOKEN_URI, HttpConst.Method.POST );
    self.setAuthHeader( http );
    http.connect;
  finally
    _token.invalid;
    FreeAndNil( http );
  end;
end;

procedure TCloudDriverAuthPKCESession.refreshToken;
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
    http.setQueryParams( queryItems );
    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    _params.resultProcessFunc( cloudDriverResult );
  end;

  procedure analyseResult;
  var
    json: NSDictionary;
  begin
    json:= TJsonUtil.parse( cloudDriverResult.httpResult.body );
    _token.access:= TJsonUtil.getString( json, 'access_token' );
    _token.setExpiration( TJsonUtil.getInteger( json, 'expires_in' ) );
  end;

begin
  try
    http:= TMiniHttpClient.Create( _params.TOKEN_URI, HttpConst.Method.POST );
    doRequest;

    if cloudDriverResult.httpResult.resultCode <> 200 then
      Exit;
    analyseResult;
    cloudDriverManager.driverUpdated( _driver );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TCloudDriverAuthPKCESession.onRedirect(const url: NSURL);
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

function TCloudDriverAuthPKCESession.getAccessToken: String;
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
      on e: ECloudDriverTokenException do begin
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

constructor TCloudDriverAuthPKCESession.Create(
  const driver: TCloudDriver;
  const params: TCloudDriverAuthPKCESessionParams );
begin
  _driver:= driver;
  _params:= params;
  _config:= _params.config;
  _token:= TCloudDriverToken.Create;
  _lockObject:= TCriticalSection.Create;
end;

destructor TCloudDriverAuthPKCESession.Destroy;
begin
  FreeAndNil( _token );
  FreeAndNil( _lockObject );
end;

function TCloudDriverAuthPKCESession.clone( const driver: TCloudDriver ): TCloudDriverAuthPKCESession;
begin
  Result:= TCloudDriverAuthPKCESession.Create( driver, _params );
  Result._accountID:= _accountID;
  Result._token:= _token.clone;
end;

function TCloudDriverAuthPKCESession.authorize: Boolean;
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

procedure TCloudDriverAuthPKCESession.unauthorize;
begin
  _lockObject.Acquire;
  try
    revokeToken;
  finally
    _lockObject.Release;
  end;
end;

function TCloudDriverAuthPKCESession.authorized: Boolean;
begin
  Result:= (_token.access <> EmptyStr);
end;

procedure TCloudDriverAuthPKCESession.setAuthHeader(http: TMiniHttpClient);
var
  access: String;
begin
  access:= self.getAccessToken;
  http.addHeader( _params.AUTH_HEADER, _params.AUTH_TYPE + ' ' + access );
end;

procedure TCloudDriverAuthPKCESession.setToken(const token: TCloudDriverToken);
var
  oldToken: TCloudDriverToken;
begin
  oldToken:= _token;
  _token:= token;
  oldToken.Free;
end;

function TCloudDriverAuthPKCESession.getToken: TCloudDriverToken;
begin
  Result:= _token;
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

{ TAuthSessionCloudDriver }

constructor TAuthSessionCloudDriver.Create(
  const config: TCloudDriverConfig;
  const authParams: TCloudDriverAuthPKCESessionParams );
begin
  _config:= config;
  _authSession:= TCloudDriverAuthPKCESession.Create( self, authParams );
end;

destructor TAuthSessionCloudDriver.Destroy;
begin
  FreeAndNil( _authSession );
end;

function TAuthSessionCloudDriver.authorize: Boolean;
begin
  Result:= _authSession.authorize;
end;

procedure TAuthSessionCloudDriver.unauthorize;
begin
  _authSession.unauthorize;
end;

function TAuthSessionCloudDriver.authorized: Boolean;
begin
  Result:= _authSession.authorized;
end;

function TAuthSessionCloudDriver.getToken: TCloudDriverToken;
begin
  Result:= _authSession.getToken;
end;

procedure TAuthSessionCloudDriver.setToken(const token: TCloudDriverToken);
begin
  _authSession.setToken( token );
end;

end.

