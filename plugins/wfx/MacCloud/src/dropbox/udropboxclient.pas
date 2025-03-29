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
  Classes, SysUtils, Generics.Collections, DateUtils,
  CocoaAll, uMiniCocoa,
  uMiniHttpClient, uMiniUtil;

type

  { TDropBoxFile }

  TDropBoxFile = class
  private
    _dotTag: String;
    _name: String;
    _size: QWord;
    _clientModified: TDateTime;
    _serverModified: TDateTime;
  public
    function isFolder: Boolean;
  public
    property dotTag: String read _dotTag write _dotTag;
    property name: String read _name write _name;
    property size: QWord read _size write _size;
    property clientModified: TDateTime read _clientModified write _clientModified;
    property serverModified: TDateTime read _serverModified write _serverModified;
  end;

  TDropBoxFiles = specialize TList<TDropBoxFile>;

  IDropBoxProgressCallback = IMiniHttpDataCallback;

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

  TDropBoxToken = record
    access: String;
    refresh: String;
  end;

  { TDropBoxAuthPKCESession }

  TDropBoxAuthPKCESession = class
  private
    _config: TDropBoxConfig;
    _codeVerifier: String;
    _state: String;
    _code: String;
    _token: TDropBoxToken;
    _accountID: String;
    _alert: NSAlert;
  private
    procedure requestAuthorization;
    procedure waitAuthorizationAndPrompt;
    procedure closePrompt;
    function requestToken: Boolean;
    procedure onRedirect( const url: NSURL );
  public
    constructor Create( const config: TDropBoxConfig );
    function authorize: Boolean;
    procedure setAuthHeader( http: TMiniHttpClient );
  public
    property token: TDropBoxToken read _token;
  end;

  { TDropBoxListFolderSession }

  TDropBoxListFolderSession = class
  private
    _authSession: TDropBoxAuthPKCESession;
    _path: String;
    _files: TDropBoxFiles;
    _cursor: String;
    _hasMore: Boolean;
  private
    procedure listFolderFirst;
    procedure listFolderContinue;
    procedure analyseListResult( const jsonString: String );
  public
    constructor Create( const authSession: TDropBoxAuthPKCESession; const path: String );
    destructor Destroy; override;
    function getNextFile: TDropBoxFile;
  end;

  { TDropBoxDownloadSession }

  TDropBoxDownloadSession = class
  private
    _authSession: TDropBoxAuthPKCESession;
    _serverPath: String;
    _localPath: String;
    _callback: IDropBoxProgressCallback;
  public
    constructor Create(
      const authSession: TDropBoxAuthPKCESession;
      const serverPath: String;
      const localPath: String;
      const callback: IDropBoxProgressCallback );
    function download: Boolean;
  end;

  { TDropBoxUploadSession }

  TDropBoxUploadSession = class
  private
    _authSession: TDropBoxAuthPKCESession;
    _serverPath: String;
    _localPath: String;
    _callback: IDropBoxProgressCallback;
  public
    constructor Create(
      const authSession: TDropBoxAuthPKCESession;
      const serverPath: String;
      const localPath: String;
      const callback: IDropBoxProgressCallback );
    function upload: Boolean;
  end;

  { TDropBoxCreateFolderSession }

  TDropBoxCreateFolderSession = class
  private
    _authSession: TDropBoxAuthPKCESession;
    _path: String;
  public
    constructor Create( const authSession: TDropBoxAuthPKCESession; const path: String );
    function createFolder: Boolean;
  end;

  { TDropBoxDeleteSession }

  TDropBoxDeleteSession = class
  private
    _authSession: TDropBoxAuthPKCESession;
    _path: String;
  public
    constructor Create( const authSession: TDropBoxAuthPKCESession; const path: String );
    function delete: Boolean;
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
    function copyOrMove( const needToMove: Boolean ): Boolean;
    function copy: Boolean;
    function move: Boolean;
  end;

  { TDropBoxClient }

  TDropBoxClient = class
  private
    _config: TDropBoxConfig;
    _authSession: TDropBoxAuthPKCESession;
    _listFolderSession: TDropBoxListFolderSession;
  public
    constructor Create( const config: TDropBoxConfig );
    destructor Destroy; override;
  public
    function authorize: Boolean;
  public
    procedure listFolderBegin( const path: String );
    function  listFolderGetNextFile: TDropBoxFile;
    procedure listFolderEnd;
  public
    function download(
      const serverPath: String;
      const localPath: String;
      const callback: IDropBoxProgressCallback ): Boolean;
    function upload(
      const serverPath: String;
      const localPath: String;
      const callback: IDropBoxProgressCallback ): Boolean;
    function createFolder( const path: String ): Boolean;
    function delete(  const path: String ): Boolean;
    function copyOrMove( const fromPath: String; const toPath: String; const needToMove: Boolean ): Boolean;
  end;

implementation

type
  TDropBoxConstURI = record
    OAUTH2: String;
    TOKEN: String;
    LIST_FOLDER: String;
    LIST_FOLDER_CONTINUE: String;
    DOWNLOAD: String;
    UPLOAD_SMALL: String;
    CREATE_FOLDER: String;
    DELETE: String;
    COPY: String;
    MOVE: String;
  end;

  TDropBoxConstHeader = record
    AUTH: String;
    ARG: String
  end;

  TDropBoxConst = record
    URI: TDropBoxConstURI;
    HEADER: TDropBoxConstHeader;
  end;

const
  DropBoxConst: TDropBoxConst = (
    URI: (
      OAUTH2: 'https://www.dropbox.com/oauth2/authorize';
      TOKEN: 'https://api.dropbox.com/oauth2/token';
      LIST_FOLDER:  'https://api.dropboxapi.com/2/files/list_folder';
      LIST_FOLDER_CONTINUE: 'https://api.dropboxapi.com/2/files/list_folder/continue';
      DOWNLOAD: 'https://content.dropboxapi.com/2/files/download';
      UPLOAD_SMALL: 'https://content.dropboxapi.com/2/files/upload';
      CREATE_FOLDER: 'https://api.dropboxapi.com/2/files/create_folder_v2';
      DELETE: 'https://api.dropboxapi.com/2/files/delete_v2';
      COPY: 'https://api.dropboxapi.com/2/files/copy_v2';
      MOVE: 'https://api.dropboxapi.com/2/files/move_v2';
    );
    HEADER: (
      AUTH: 'Authorization';
      ARG: 'Dropbox-API-Arg';
    );
  );

{ TDropBoxFile }

function TDropBoxFile.isFolder: Boolean;
begin
  Result:= _dotTag = 'folder';
end;

{ TDropBoxConfig }

constructor TDropBoxConfig.Create( const clientID: String; const listenURI: String );
begin
  _clientID:= clientID;
  _listenURI:= listenURI;
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

function TDropBoxAuthPKCESession.requestToken: Boolean;
var
  http: TMiniHttpClient;
  requestResult: TMiniClientResult;

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
    requestResult:= http.post( DropBoxConst.URI.TOKEN, queryItems );
  end;

  procedure analyseResult;
  var
    json: NSDictionary;
  begin
    json:= TJsonUtil.parse( requestResult.responseBody );
    _token.access:= TJsonUtil.getString( json, 'access_token' );
    _token.refresh:= TJsonUtil.getString( json, 'refresh_token' );
    _accountID:= TJsonUtil.getString( json, 'account_id' );
  end;

begin
  Result:= False;
  if _code = EmptyStr then
    Exit;

  http:= TMiniHttpClient.Create;
  doRequest;

  if requestResult.statusCode <> 200 then
    Exit;
  analyseResult;
  Result:= True;

  http.Free;
end;

procedure TDropBoxAuthPKCESession.onRedirect(const url: NSURL);
var
  components: NSURLComponents;
  state: String;
begin
  NSApplication(NSAPP).setOpenURLObserver( nil );
  components:= NSURLComponents.componentsWithURL_resolvingAgainstBaseURL( url, False );
  state:= THttpClientUtil.queryValue( components, 'state' );
  if state <> _state then
    Exit;
  _code:= THttpClientUtil.queryValue( components, 'code' );
  closePrompt;
end;

constructor TDropBoxAuthPKCESession.Create(const config: TDropBoxConfig);
begin
  _config:= config;
end;

function TDropBoxAuthPKCESession.authorize: Boolean;
begin
  Result:= False;
  requestAuthorization;
  TThread.Synchronize( TThread.CurrentThread, @waitAuthorizationAndPrompt );
  Result:= requestToken;
end;

procedure TDropBoxAuthPKCESession.setAuthHeader(http: TMiniHttpClient);
begin
  if _token.access = EmptyStr then
    self.authorize;
  http.addHeader( DropBoxConst.HEADER.AUTH, 'Bearer ' + _token.access );
end;

{ TDropBoxListFolderSession }

procedure TDropBoxListFolderSession.listFolderFirst;
var
  http: TMiniHttpClient;
  requestResult: TMiniClientResult;
  body: String;
begin
  body:= TJsonUtil.dumps( ['path', _path] );
  http:= TMiniHttpClient.Create;
  _authSession.setAuthHeader( http );
  http.setContentType( HttpConst.ContentType.JSON );
  http.setBody( body );
  requestResult:= http.post( DropBoxConst.URI.LIST_FOLDER, nil );
  if Assigned(_files) then
    _files.Free;
  _files:= TDropBoxFiles.Create;
  if requestResult.statusCode = 200 then
    analyseListResult( requestResult.responseBody );
  http.Free;
end;

procedure TDropBoxListFolderSession.listFolderContinue;
var
  http: TMiniHttpClient;
  requestResult: TMiniClientResult;
  body: String;
begin
  body:= TJsonUtil.dumps( ['cursor', _cursor] );
  http:= TMiniHttpClient.Create;
  _authSession.setAuthHeader( http );
  http.setContentType( HttpConst.ContentType.JSON );
  http.setBody( body );
  requestResult:= http.post( DropBoxConst.URI.LIST_FOLDER_CONTINUE, nil );
  if requestResult.statusCode = 200 then
    analyseListResult( requestResult.responseBody );
  http.Free;
end;

procedure TDropBoxListFolderSession.analyseListResult(const jsonString: String);
var
  json: NSDictionary;
  jsonEntries: NSArray;
  jsonItem: NSDictionary;
  dbFile: TDropBoxFile;

  function toDateTime( const key: String ): TDateTime;
  var
    str: String;
  begin
    str:= TJsonUtil.getString( json, key );
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
    dbFile:= TDropBoxFile.Create;
    dbFile.dotTag:= TJsonUtil.getString( jsonItem, '.tag' );
    dbFile.name:= TJsonUtil.getString( jsonItem, 'name' );
    dbFile.size:= TJsonUtil.getInteger( jsonItem, 'size' );
    dbFile.clientModified:= toDateTime( 'client_modified' );
    dbFile.serverModified:= toDateTime( 'server_modified' );
    _files.Add( dbFile );
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

function TDropBoxListFolderSession.getNextFile: TDropBoxFile;
  function popFirst: TDropBoxFile;
  begin
    if _files.Count > 0 then begin
      Result:= _files.First;
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
  const callback: IDropBoxProgressCallback );
begin
  _authSession:= authSession;
  _serverPath:= serverPath;
  _localPath:= localPath;
  _callback:= callback;
end;

function TDropBoxDownloadSession.download: Boolean;
var
  http: TMiniHttpClient;
  argJsonString: String;
  requestResult: TMiniClientResult;
begin
  argJsonString:= TJsonUtil.dumps( ['path', _serverPath], True );
  http:= TMiniHttpClient.Create;
  _authSession.setAuthHeader( http );
  http.addHeader( DropBoxConst.HEADER.ARG, argJsonString );
  requestResult:= http.download( DropBoxConst.URI.DOWNLOAD, _localPath, _callback );
  Result:= (requestResult.statusCode = 200);
  http.Free;
end;

{ TDropBoxUploadSession }

constructor TDropBoxUploadSession.Create(
  const authSession: TDropBoxAuthPKCESession; const serverPath: String;
  const localPath: String; const callback: IDropBoxProgressCallback);
begin
  _authSession:= authSession;
  _serverPath:= serverPath;
  _localPath:= localPath;
  _callback:= callback;
end;

function TDropBoxUploadSession.upload: Boolean;
var
  http: TMiniHttpClient;
  argJsonString: String;
  requestResult: TMiniClientResult;
begin
  argJsonString:= TJsonUtil.dumps( ['path', _serverPath], True );
  http:= TMiniHttpClient.Create;
  _authSession.setAuthHeader( http );
  http.addHeader( DropBoxConst.HEADER.ARG, argJsonString );
  requestResult:= http.upload( DropBoxConst.URI.UPLOAD_SMALL, _localPath, _callback );
  Result:= (requestResult.statusCode = 200);
  http.Free;
end;

{ TDropBoxCreateFolderSession }

constructor TDropBoxCreateFolderSession.Create(
  const authSession: TDropBoxAuthPKCESession; const path: String );
begin
  _authSession:= authSession;
  _path:= path;
end;

function TDropBoxCreateFolderSession.createFolder: Boolean;
var
  http: TMiniHttpClient;
  body: String;
  requestResult: TMiniClientResult;
begin
  body:= TJsonUtil.dumps( ['path', _path] );
  http:= TMiniHttpClient.Create;
  http.setContentType( HttpConst.ContentType.JSON );
  _authSession.setAuthHeader( http );
  http.setBody( body );
  requestResult:= http.post( DropBoxConst.URI.CREATE_FOLDER, nil );
  Result:= (requestResult.statusCode = 200);
  http.Free;
end;

{ TDropBoxDeleteSession }

constructor TDropBoxDeleteSession.Create(
  const authSession: TDropBoxAuthPKCESession; const path: String);
begin
  _authSession:= authSession;
  _path:= path;
end;

function TDropBoxDeleteSession.delete: Boolean;
var
  http: TMiniHttpClient;
  body: String;
  requestResult: TMiniClientResult;
begin
  body:= TJsonUtil.dumps( ['path', _path] );
  http:= TMiniHttpClient.Create;
  http.setContentType( HttpConst.ContentType.JSON );
  _authSession.setAuthHeader( http );
  http.setBody( body );
  requestResult:= http.post( DropBoxConst.URI.DELETE, nil );
  Result:= (requestResult.statusCode = 200);
  http.Free;
end;

{ TDropBoxCopyMoveSession }

constructor TDropBoxCopyMoveSession.Create( const authSession: TDropBoxAuthPKCESession;
  const fromPath: String; const toPath: String );
begin
  _authSession:= authSession;
  _fromPath:= fromPath;
  _toPath:= toPath;
end;

function TDropBoxCopyMoveSession.copyOrMove( const needToMove: Boolean ): Boolean;
var
  uri: String;
  http: TMiniHttpClient;
  body: String;
  requestResult: TMiniClientResult;
begin
  http:= TMiniHttpClient.Create;
  http.setContentType( HttpConst.ContentType.JSON );
  _authSession.setAuthHeader( http );

  body:= TJsonUtil.dumps( ['from_path', _fromPath, 'to_path', _toPath] );
  http.setBody( body );

  if needToMove then
    uri:= DropBoxConst.URI.MOVE
  else
    uri:= DropBoxConst.URI.COPY;

  requestResult:= http.post( uri, nil );
  Result:= (requestResult.statusCode = 200);
  http.Free;
end;

function TDropBoxCopyMoveSession.copy: Boolean;
begin
  Result:= copyOrMove( False );
end;

function TDropBoxCopyMoveSession.move: Boolean;
begin
  Result:= copyOrMove( True );
end;

{ TDropBoxClient }

constructor TDropBoxClient.Create(const config: TDropBoxConfig);
begin
  _config:= config;
  _authSession:= TDropBoxAuthPKCESession.Create( _config );
end;

destructor TDropBoxClient.Destroy;
begin
  FreeAndNil( _config );
  FreeAndNil( _authSession );
  FreeAndNil( _listFolderSession );
end;

function TDropBoxClient.authorize: Boolean;
begin
  Result:= _authSession.authorize;
end;

procedure TDropBoxClient.listFolderBegin(const path: String);
begin
  if Assigned(_listFolderSession) then
    _listFolderSession.Free;
  _listFolderSession:= TDropBoxListFolderSession.Create( _authSession, path );
  _listFolderSession.listFolderFirst;
end;

function TDropBoxClient.listFolderGetNextFile: TDropBoxFile;
begin
  Result:= _listFolderSession.getNextFile;
end;

procedure TDropBoxClient.listFolderEnd;
begin
  FreeAndNil( _listFolderSession );
end;

function TDropBoxClient.download(
  const serverPath: String;
  const localPath: String;
  const callback: IDropBoxProgressCallback ): Boolean;
var
  session: TDropBoxDownloadSession;
begin
  session:= TDropBoxDownloadSession.Create( _authSession, serverPath, localPath, callback );
  Result:= session.download;
  session.Free;
end;

function TDropBoxClient.upload(
  const serverPath: String;
  const localPath: String;
  const callback: IDropBoxProgressCallback): Boolean;
var
  session: TDropBoxUploadSession;
begin
  session:= TDropBoxUploadSession.Create( _authSession, serverPath, localPath, callback );
  Result:= session.upload;
  session.Free;
end;

function TDropBoxClient.createFolder(const path: String): Boolean;
var
  session: TDropBoxCreateFolderSession;
begin
  session:= TDropBoxCreateFolderSession.Create( _authSession, path );
  Result:= session.createFolder;
  session.Free;
end;

function TDropBoxClient.delete(const path: String): Boolean;
var
  session: TDropBoxDeleteSession;
begin
  session:= TDropBoxDeleteSession.Create( _authSession, path );
  Result:= session.delete;
  session.Free;
end;

function TDropBoxClient.copyOrMove(const fromPath: String; const toPath: String;
  const needToMove: Boolean ): Boolean;
var
  session: TDropBoxCopyMoveSession;
begin
  session:= TDropBoxCopyMoveSession.Create( _authSession, fromPath, toPath );
  Result:= session.copyOrMove( needToMove );
  session.Free;
end;

end.

