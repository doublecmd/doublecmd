unit uOAuth2Auth;

{$mode ObjFPC}{$H+}
{$interfaces corba}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, syncobjs,
  CocoaAll, uMiniCocoa,
  uCloudDriver, uOAuth2Core,
  uMiniHttpClient, uMiniUtil;

type
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

  { TCloudDriverOAuth2Session }

  TCloudDriverOAuth2Session = class( TCloudDriverAuthSession )
  strict protected
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
  strict protected
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
  strict protected
    _clientSecret: String;
  protected
    procedure onAuthorize( const queryItems: TQueryItemsDictonary ); override;
    procedure onRequestToken( const queryItems: TQueryItemsDictonary ); override;
    procedure onRefreshToken( const queryItems: TQueryItemsDictonary ); override;
    procedure onRevokeToken( const http: TMiniHttpClient ); override;
  public
    constructor Create(
      const driver: TCloudDriver;
      const params: TCloudDriverOAuth2SessionParams;
      const clientSecret: String );
    function clone(const driver: TCloudDriver): TCloudDriverOAuth2Session; override;
  end;

implementation

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
  _alert.setMessageText( StringToNSString('Waiting for ' + _cloudDriver.driverName + ' authorization') );
  _alert.setInformativeText( StringToNSString('Please login your ' + _cloudDriver.driverName + ' account in Safari and authorize Double Commander to access. '#13'The authorization is completed on the ' + _cloudDriver.driverName + ' official website, Double Command will not get your password.') );
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
    cloudDriverManager.driverUpdated( _cloudDriver );
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
    if NOT _token.isValidAccessToken then
      Exit;
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
    cloudDriverManager.driverUpdated( _cloudDriver );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TCloudDriverOAuth2Session.analyseTokenResult( const jsonString: String );
var
  json: NSDictionary;
  retRefreshToken: String;
  retAccountID: String;
begin
  json:= TJsonUtil.parse( jsonString );
  _token.access:= TJsonUtil.getString( json, 'access_token' );
  retRefreshToken:= TJsonUtil.getString( json, 'refresh_token' );
  if retRefreshToken <> EmptyStr then
    _token.refresh:= retRefreshToken;
  _token.setExpiration( TJsonUtil.getInteger( json, 'expires_in' ) );
  retAccountID:= TJsonUtil.getString( json, 'account_id' );
  if retAccountID <> EmptyStr then
    _accountID:= retAccountID;
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
  Inherited Create( driver );
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
  queryItems.Add( 'token', _token.access );
end;

procedure TCloudDriverOAuth2SecretSession.onRequestToken( const queryItems: TQueryItemsDictonary );
var
  secret: String;
begin
  secret:= _clientSecret;
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
  secret:= _clientSecret;
  queryItems:= TQueryItemsDictonary.Create;
  queryItems.Add( 'client_id', _config.clientID );
  queryItems.Add( 'client_secret', secret );
  queryItems.Add( 'token', _token.access );
  http.setQueryParams( queryItems );
end;

constructor TCloudDriverOAuth2SecretSession.Create(const driver: TCloudDriver;
  const params: TCloudDriverOAuth2SessionParams; const clientSecret: String);
begin
  Inherited Create( driver, params );
  _clientSecret:= clientSecret;
end;

function TCloudDriverOAuth2SecretSession.clone( const driver: TCloudDriver ): TCloudDriverOAuth2Session;
begin
  Result:= TCloudDriverOAuth2SecretSession.Create( driver, _params, _clientSecret );
  Result._accountID:= _accountID;
  Result._token:= _token.clone;
end;

end.

