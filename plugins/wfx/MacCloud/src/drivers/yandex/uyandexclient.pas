{
   Notes:
   1. the basic Yandex Client
   2. no dependencies on other libraries
}

unit uYandexClient;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, DateUtils,
  CocoaAll,
  uCloudDriver, uMiniHttpClient, uMiniUtil;

type

  { TCloudDriverListFolderSession }

  TCloudDriverListFolderSession = class
  const
    LIMIT = 100;
  private
    _authSession: TCloudDriverAuthPKCESession;
    _path: String;
    _files: TCloudFiles;
    _offset: Integer;
    _total: Integer;
    _hasMore: Boolean;
  private
    procedure listFolderFirst;
    procedure listFolderContinue;
    procedure analyseListResult( const jsonString: String );
  public
    constructor Create( const authSession: TCloudDriverAuthPKCESession; const path: String );
    destructor Destroy; override;
    function getNextFile: TCloudFile;
  end;

  { TYandexDownloadSession }

  TYandexDownloadSession = class
  private
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
    procedure download;
  end;

  { TYandexUploadSession }

  TYandexUploadSession = class
  private
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
    procedure upload;
  end;

  { TYandexCreateFolderSession }

  TYandexCreateFolderSession = class
  private
    _authSession: TCloudDriverAuthPKCESession;
    _path: String;
  public
    constructor Create( const authSession: TCloudDriverAuthPKCESession; const path: String );
    procedure createFolder;
  end;

  { TYandexDeleteSession }

  TYandexDeleteSession = class
  private
    _authSession: TCloudDriverAuthPKCESession;
    _path: String;
  public
    constructor Create( const authSession: TCloudDriverAuthPKCESession; const path: String );
    procedure delete;
  end;

  { TYandexCopyMoveSession }

  TYandexCopyMoveSession = class
  private
    _authSession: TCloudDriverAuthPKCESession;
    _fromPath: String;
    _toPath: String;
  public
    constructor Create( const authSession: TCloudDriverAuthPKCESession;
      const fromPath: String; const toPath: String );
    procedure copyOrMove( const needToMove: Boolean );
    procedure copy;
    procedure move;
  end;

  { TYandexLister }

  TYandexLister = class( TCloudDriverLister )
  private
    _listFolderSession: TCloudDriverListFolderSession;
  public
    constructor Create( const authSession: TCloudDriverAuthPKCESession; const path: String );
    destructor Destroy; override;
    procedure listFolderBegin; override;
    function  listFolderGetNextFile: TCloudFile; override;
    procedure listFolderEnd; override;
  end;

  { TYandexClient }

  TYandexClient = class( TCloudDriver )
  private
    _config: TCloudDriverConfig;
    _authSession: TCloudDriverAuthPKCESession;
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
  public
    constructor Create( const config: TCloudDriverConfig );
    destructor Destroy; override;
    function clone: TCloudDriver; override;
  public
    function authorize: Boolean; override;
    procedure unauthorize; override;
    function authorized: Boolean; override;
    function getToken: TCloudDriverToken; override;
    procedure setToken( const token: TCloudDriverToken ); override;
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
  yandexConfig: TCloudDriverConfig;

implementation

type
  TYandexConstURI = record
    OAUTH2: String;
    TOKEN: String;
    REVOKE_TOKEN: String;
    RESOURCES: String;
    DOWNLOAD: String;
    UPLOAD: String;
    COPY: String;
    MOVE: String;
  end;

  TYandexConstHeader = record
    AUTH: String;
  end;

  TYandexConst = record
    URI: TYandexConstURI;
    HEADER: TYandexConstHeader;
  end;

const
  YandexConst: TYandexConst = (
    URI: (
      OAUTH2: 'https://oauth.yandex.ru/authorize';
      TOKEN: 'https://oauth.yandex.com/token';
      REVOKE_TOKEN: 'https://oauth.yandex.com/revoke_token';
      RESOURCES:   'https://cloud-api.yandex.net/v1/disk/resources';
      DOWNLOAD: 'https://cloud-api.yandex.net/v1/disk/resources/download';
      UPLOAD:   'https://cloud-api.yandex.net/v1/disk/resources/upload';
      COPY: 'https://cloud-api.yandex.net/v1/disk/resources/copy';
      MOVE: 'https://cloud-api.yandex.net/v1/disk/resources/move';
    );
    HEADER: (
      AUTH: 'Authorization';
    );
  );

// raise the corresponding exception if there are errors
procedure YandexClientResultProcess( const cloudDriverResult: TCloudDriverResult );
var
  httpResult: TMiniHttpResult;
  httpError: NSError;
  httpErrorDescription: String;
  cloudDriverMessage: String;

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

  procedure processCloudDriverError;
  begin
    cloudDriverMessage:= cloudDriverResult.resultMessage;

    if (httpResult.resultCode>=200) and (httpResult.resultCode<=299) then
      Exit;
    raise ECloudDriverException.Create( cloudDriverMessage );
  end;

  procedure logException( const e: Exception );
  var
    message: String;
  begin
    message:= 'Yandex Error';
    if e.Message <> EmptyStr then
      message:= message + ': ' + e.Message;
    TLogUtil.logError( message );
  end;

begin
  try
    processHttpError;
    processCloudDriverError;
  except
    on e: Exception do begin
      logException( e );
      raise;
    end;
  end;
end;

{ TCloudDriverListFolderSession }

procedure TCloudDriverListFolderSession.listFolderFirst;
begin
  listFolderContinue;
end;

procedure TCloudDriverListFolderSession.listFolderContinue;
var
  http: TMiniHttpClient;
  httpResult: TMiniHttpResult = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  queryItems: TQueryItemsDictonary;
begin
  try
    queryItems:= TQueryItemsDictonary.Create;
    queryItems.Add( 'path', _path );
    queryItems.Add( 'offset', IntToStr(_offset) );
    queryItems.Add( 'limit', IntToStr(LIMIT) );
    http:= TMiniHttpClient.Create( YandexConst.URI.RESOURCES, HttpConst.Method.GET );
    http.setQueryParams( queryItems );
    http.setContentType( HttpConst.ContentType.UrlEncoded );
    _authSession.setAuthHeader( http );

    cloudDriverResult:= TCloudDriverResult.Create;
    httpResult:= http.connect;
    cloudDriverResult.httpResult:= httpResult;
    cloudDriverResult.resultMessage:= httpResult.body;

    if httpResult.resultCode = 200 then
      analyseListResult( httpResult.body );

    YandexClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TCloudDriverListFolderSession.analyseListResult(const jsonString: String);
var
  json: NSDictionary;
  jsonEmbedded: NSDictionary;
  jsonItems: NSArray;
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
  jsonEmbedded:= TJsonUtil.getDictionary( json, '_embedded' );
  _total:= TJsonUtil.getInteger( jsonEmbedded, 'total' );
  _offset:= TJsonUtil.getInteger( jsonEmbedded, 'offset' );
  jsonItems:= TJsonUtil.getArray( jsonEmbedded, 'items' );
  if jsonItems = nil then
    Exit;
  for jsonItem in jsonItems do begin
    cloudFile:= TCloudFile.Create;
    cloudFile.isFolder:= ( TJsonUtil.getString(jsonItem,'type')='dir' );
    cloudFile.name:= TJsonUtil.getString( jsonItem, 'name' );
    cloudFile.size:= TJsonUtil.getInteger( jsonItem, 'size' );
    cloudFile.creationTime:= toDateTime( 'created' );
    cloudFile.modificationTime:= toDateTime( 'modified' );
    _files.Add( cloudFile );
    inc( _offset );
  end;
  _hasMore:= ( _offset < _total );
end;

constructor TCloudDriverListFolderSession.Create( const authSession: TCloudDriverAuthPKCESession; const path: String );
begin
  _authSession:= authSession;
  _files:= TCloudFiles.Create;
  _path:= path;
  if _path = EmptyStr then
    _path:= PathDelim;
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

{ TYandexDownloadSession }

constructor TYandexDownloadSession.Create(
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

procedure TYandexDownloadSession.download;
  function getDownloadHref: String;
  var
    http: TMiniHttpClient = nil;
    queryItems: TQueryItemsDictonary;
    cloudDriverResult: TCloudDriverResult = nil;
    json: NSDictionary;
  begin
    try
      queryItems:= TQueryItemsDictonary.Create;
      queryItems.Add( 'path', _serverPath );
      http:= TMiniHttpClient.Create( YandexConst.URI.DOWNLOAD, HttpConst.Method.GET );
      http.setQueryParams( queryItems );
      _authSession.setAuthHeader( http );

      cloudDriverResult:= TCloudDriverResult.Create;
      cloudDriverResult.httpResult:= http.connect;
      cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
      YandexClientResultProcess( cloudDriverResult );

      json:= TJsonUtil.parse( cloudDriverResult.resultMessage );
      Result:= TJsonUtil.getString( json, 'href' );
    finally
      FreeAndNil( cloudDriverResult );
      FreeAndNil( http );
    end;
  end;

  procedure downloadHref( const href : String );
  var
    http: TMiniHttpClient = nil;
    cloudDriverResult: TCloudDriverResult = nil;
  begin
    try
      http:= TMiniHttpClient.Create( href, HttpConst.Method.GET );
      _authSession.setAuthHeader( http );

      cloudDriverResult:= TCloudDriverResult.Create;
      cloudDriverResult.httpResult:= http.download( _localPath, _callback );
      cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
      YandexClientResultProcess( cloudDriverResult );
    finally
      FreeAndNil( cloudDriverResult );
      FreeAndNil( http );
    end;
  end;

var
  href: String;
begin
  href:= getDownloadHref;
  downloadHref( href );
end;

{ TYandexUploadSession }

constructor TYandexUploadSession.Create(
  const authSession: TCloudDriverAuthPKCESession; const serverPath: String;
  const localPath: String; const callback: ICloudProgressCallback);
begin
  _authSession:= authSession;
  _serverPath:= serverPath;
  _localPath:= localPath;
  _callback:= callback;
end;

procedure TYandexUploadSession.upload;
  function getUploadHref: String;
  var
    http: TMiniHttpClient = nil;
    queryItems: TQueryItemsDictonary;
    cloudDriverResult: TCloudDriverResult = nil;
    json: NSDictionary;
  begin
    try
      queryItems:= TQueryItemsDictonary.Create;
      queryItems.Add( 'path', _serverPath );
      http:= TMiniHttpClient.Create( YandexConst.URI.UPLOAD, HttpConst.Method.GET );
      http.setQueryParams( queryItems );
      _authSession.setAuthHeader( http );

      cloudDriverResult:= TCloudDriverResult.Create;
      cloudDriverResult.httpResult:= http.connect;
      cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
      YandexClientResultProcess( cloudDriverResult );

      json:= TJsonUtil.parse( cloudDriverResult.resultMessage );
      Result:= TJsonUtil.getString( json, 'href' );
    finally
      FreeAndNil( cloudDriverResult );
      FreeAndNil( http );
    end;
  end;

  procedure uploadHref( const href: String; const range: TMiniHttpContentRange );
  var
    http: TMiniHttpClient = nil;
    cloudDriverResult: TCloudDriverResult = nil;
  begin
    try
      http:= TMiniHttpClient.Create( href, HttpConst.Method.PUT );
      cloudDriverResult:= TCloudDriverResult.Create;
      cloudDriverResult.httpResult:= http.uploadRange( _localPath, range, _callback );
      cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
      YandexClientResultProcess( cloudDriverResult );
    finally
      FreeAndNil( cloudDriverResult );
      FreeAndNil( http );
    end;
  end;

const
  SESSION_FILE_SIZE = 1024*1024*2; // 2MB
var
  href: String;
  range: TMiniHttpContentRange;
  offset: Integer;
  sessionSize: Integer;
begin
  _localFileSize:= TFileUtil.filesize( _localPath );
  href:= getUploadHref;

  offset:= 0;
  sessionSize:= 0;
  while offset < _localFileSize do begin
    if offset + SESSION_FILE_SIZE > _localFileSize then
      sessionSize:= _localFileSize - offset
    else
      sessionSize:= SESSION_FILE_SIZE;
    range:= TMiniHttpContentRange.Create( offset, offset+sessionSize-1 , _localFileSize );
    uploadHref( href, range );
    inc( offset, sessionSize );
    range.Free;
  end;
end;

{ TYandexCreateFolderSession }

constructor TYandexCreateFolderSession.Create(
  const authSession: TCloudDriverAuthPKCESession; const path: String );
begin
  _authSession:= authSession;
  _path:= path;
end;

procedure TYandexCreateFolderSession.createFolder;
var
  http: TMiniHttpClient = nil;
  queryItems: TQueryItemsDictonary;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    queryItems:= TQueryItemsDictonary.Create;
    queryItems.Add( 'path', _path );
    http:= TMiniHttpClient.Create( YandexConst.URI.RESOURCES, HttpConst.Method.PUTQueryString );
    http.setQueryParams( queryItems );
    _authSession.setAuthHeader( http );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    YandexClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TYandexDeleteSession }

constructor TYandexDeleteSession.Create(
  const authSession: TCloudDriverAuthPKCESession; const path: String);
begin
  _authSession:= authSession;
  _path:= path;
end;

procedure TYandexDeleteSession.delete;
var
  http: TMiniHttpClient = nil;
  queryItems: TQueryItemsDictonary;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    queryItems:= TQueryItemsDictonary.Create;
    queryItems.Add( 'path', _path );
    http:= TMiniHttpClient.Create( YandexConst.URI.RESOURCES, HttpConst.Method.DELETE );
    http.setQueryParams( queryItems );
    _authSession.setAuthHeader( http );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    YandexClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TYandexCopyMoveSession }

constructor TYandexCopyMoveSession.Create( const authSession: TCloudDriverAuthPKCESession;
  const fromPath: String; const toPath: String );
begin
  _authSession:= authSession;
  _fromPath:= fromPath;
  _toPath:= toPath;
end;

procedure TYandexCopyMoveSession.copyOrMove( const needToMove: Boolean );
var
  uri: String;
  http: TMiniHttpClient = nil;
  queryItems: TQueryItemsDictonary;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    if needToMove then
      uri:= YandexConst.URI.MOVE
    else
      uri:= YandexConst.URI.COPY;

    queryItems:= TQueryItemsDictonary.Create;
    queryItems.Add( 'from', _fromPath );
    queryItems.Add( 'path', _toPath );
    queryItems.Add( 'overwrite', 'True' );
    http:= TMiniHttpClient.Create( uri, HttpConst.Method.POSTQueryString );
    http.setQueryParams( queryItems );
    _authSession.setAuthHeader( http );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    YandexClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TYandexCopyMoveSession.copy;
begin
  copyOrMove( False );
end;

procedure TYandexCopyMoveSession.move;
begin
  copyOrMove( True );
end;

{ TYandexLister }

constructor TYandexLister.Create(
  const authSession: TCloudDriverAuthPKCESession;
  const path: String);
begin
  _listFolderSession:= TCloudDriverListFolderSession.Create( authSession, path );
end;

destructor TYandexLister.Destroy;
begin
  FreeAndNil( _listFolderSession );
end;

procedure TYandexLister.listFolderBegin;
begin
  _listFolderSession.listFolderFirst;
end;

function TYandexLister.listFolderGetNextFile: TCloudFile;
begin
  Result:= _listFolderSession.getNextFile;
end;

procedure TYandexLister.listFolderEnd;
begin
  self.Free;
end;

{ TYandexClient }

class function TYandexClient.driverName: String;
begin
  Result:= 'Yandex';
end;

class function TYandexClient.createInstance: TCloudDriver;
begin
  Result:= TYandexClient.Create( yandexConfig );
end;

constructor TYandexClient.Create(const config: TCloudDriverConfig);
var
  params: TCloudDriverAuthPKCESessionParams;
begin
  _config:= config;
  params.config:= config;
  params.resultProcessFunc:= @YandexClientResultProcess;
  params.OAUTH2_URI:= YandexConst.URI.OAUTH2;
  params.TOKEN_URI:= YandexConst.URI.TOKEN;
  params.REVOKE_TOKEN_URI:= YandexConst.URI.REVOKE_TOKEN;
  params.AUTH_HEADER:= YandexConst.HEADER.AUTH;
  params.AUTH_TYPE:= 'OAuth';
  _authSession:= TCloudDriverAuthPKCESession.Create( self, params );
end;

destructor TYandexClient.Destroy;
begin
  FreeAndNil( _authSession );
end;

function TYandexClient.clone: TCloudDriver;
var
  newClient: TYandexClient;
begin
  newClient:= TYandexClient.Create( _config );
  newClient._authSession:= self._authSession.clone( newClient );
  Result:= newClient;
end;

function TYandexClient.authorize: Boolean;
begin
  Result:= _authSession.authorize;
end;

procedure TYandexClient.unauthorize;
begin
  _authSession.unauthorize;
end;

function TYandexClient.authorized: Boolean;
begin
  Result:= _authSession.authorized;
end;

function TYandexClient.createLister( const path: String ): TCloudDriverLister;
begin
  Result:= TYandexLister.Create( _authSession, path );
end;

procedure TYandexClient.download(
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback );
var
  session: TYandexDownloadSession = nil;
begin
  try
    session:= TYandexDownloadSession.Create( _authSession, serverPath, localPath, callback );
    session.download;
  finally
    FreeAndNil( session );
  end;
end;

procedure TYandexClient.upload(
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback);
var
  session: TYandexUploadSession = nil;
begin
  try
    session:= TYandexUploadSession.Create( _authSession, serverPath, localPath, callback );
    session.upload;
  finally
    FreeAndNil( session );
  end;
end;

procedure TYandexClient.createFolder(const path: String);
var
  session: TYandexCreateFolderSession = nil;
begin
  try
    session:= TYandexCreateFolderSession.Create( _authSession, path );
    session.createFolder;
  finally
    FreeAndNil( session );
  end;
end;

procedure TYandexClient.delete(const path: String);
var
  session: TYandexDeleteSession = nil;
begin
  try
    session:= TYandexDeleteSession.Create( _authSession, path );
    session.delete;
  finally
    FreeAndNil( session );
  end;
end;

procedure TYandexClient.copyOrMove(const fromPath: String; const toPath: String;
  const needToMove: Boolean );
var
  session: TYandexCopyMoveSession = nil;
begin
  try
    session:= TYandexCopyMoveSession.Create( _authSession, fromPath, toPath );
    session.copyOrMove( needToMove );
  finally
    FreeAndNil( session );
  end;
end;

function TYandexClient.getToken: TCloudDriverToken;
begin
  Result:= _authSession.getToken;
end;

procedure TYandexClient.setToken(const token: TCloudDriverToken);
begin
  _authSession.setToken( token );
end;

finalization
  FreeAndNil( yandexConfig );

end.

