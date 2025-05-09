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
  uCloudDriver, uOAuth2Core, uOAuth2Auth, uOAuth2Client,
  uMiniHttpClient, uMiniUtil;

type

  { TYandexListFolderSession }

  TYandexListFolderSession = class( TCloudDriverListFolderSession )
  private const
    LIMIT = 100;
  private
    _offset: Integer;
    _total: Integer;
  private
    procedure analyseListResult( const jsonString: String );
  protected
    procedure listFolderFirst; override;
    procedure listFolderContinue; override;
  public
    constructor Create( const authSession: TCloudDriverAuthSession; const path: String ); override;
  end;

  { TYandexDownloadSession }

  TYandexDownloadSession = class( TCloudDriverDownloadSession )
  public
    procedure download; override;
  end;

  { TYandexUploadSession }

  TYandexUploadSession = class( TCloudDriverUploadSession )
  public
    procedure upload; override;
  end;

  { TYandexCreateFolderSession }

  TYandexCreateFolderSession = class( TCloudDriverCreateFolderSession )
  public
    procedure createFolder; override;
  end;

  { TYandexDeleteSession }

  TYandexDeleteSession = class( TCloudDriverDeleteSession )
  public
    procedure delete; override;
  end;

  { TYandexCopyMoveSession }

  TYandexCopyMoveSession = class( TCloudDriverCopyMoveSession )
  public
    procedure copyOrMove( const needToMove: Boolean ); override;
  end;

  { TYandexClient }

  TYandexClient = class( TOAuth2SessionCloudDriver )
  public
    class function driverName: String; override;
    class function createInstance: TCloudDriver; override;
  public
    constructor Create( const config: TTokenCloudDriverConfig );
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
    procedure delete( const path: String; const isFolder: Boolean ); override;
    procedure copyOrMove( const fromPath: String; const toPath: String;
      const isFolder: Boolean; const needToMove: Boolean ); override;
  end;

var
  yandexConfig: TTokenCloudDriverConfig;

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

  TYandexConst = record
    URI: TYandexConstURI;
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
    case httpResult.resultCode of
      401:
        raise ECloudDriverAuthException.Create( cloudDriverMessage );
      403, 507:
        raise ECloudDriverQuotaException.Create( cloudDriverMessage );
      404:
        raise EFileNotFoundException.Create( cloudDriverMessage );
      409:
        raise ECloudDriverConflictException.Create( cloudDriverMessage );
      429:
        raise ECloudDriverRateLimitException.Create( cloudDriverMessage );
      else
        raise ECloudDriverException.Create( cloudDriverMessage );
    end;
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

{ TYandexListFolderSession }

procedure TYandexListFolderSession.listFolderFirst;
begin
  listFolderContinue;
end;

procedure TYandexListFolderSession.listFolderContinue;
var
  http: TMiniHttpClient = nil;
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

procedure TYandexListFolderSession.analyseListResult(const jsonString: String);
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

constructor TYandexListFolderSession.Create( const authSession: TCloudDriverAuthSession; const path: String );
var
  truePath: String;
begin
  if path = EmptyStr then
    truePath:= PathDelim
  else
    truePath:= path;
  Inherited Create( authSession, truePath );
end;

{ TYandexDownloadSession }

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

  procedure uploadHref( const href: String; const range: TMiniContentRange );
  var
    http: TMiniHttpClient = nil;
    cloudDriverResult: TCloudDriverResult = nil;
  begin
    try
      http:= TMiniHttpClient.Create( href, HttpConst.Method.PUT );
      http.setContentRange( range );
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
  range: TMiniContentRange;
  offset: Integer;
  sessionSize: Integer;
begin
  href:= getUploadHref;

  offset:= 0;
  sessionSize:= 0;
  while offset < _localFileSize do begin
    if offset + SESSION_FILE_SIZE > _localFileSize then
      sessionSize:= _localFileSize - offset
    else
      sessionSize:= SESSION_FILE_SIZE;
    range:= TMiniContentRange.Create( offset, offset+sessionSize-1 , _localFileSize );
    uploadHref( href, range );
    inc( offset, sessionSize );
    range.Free;
  end;
end;

{ TYandexCreateFolderSession }

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

{ TYandexClient }

class function TYandexClient.driverName: String;
begin
  Result:= 'Yandex';
end;

class function TYandexClient.createInstance: TCloudDriver;
begin
  Result:= TYandexClient.Create( yandexConfig );
end;

constructor TYandexClient.Create(const config: TTokenCloudDriverConfig);
var
  params: TCloudDriverOAuth2SessionParams;
begin
  Inherited Create( config );
  params.config:= config;
  params.resultProcessFunc:= @YandexClientResultProcess;
  params.scope:= EmptyStr;
  params.OAUTH2_URI:= YandexConst.URI.OAUTH2;
  params.TOKEN_URI:= YandexConst.URI.TOKEN;
  params.REVOKE_TOKEN_URI:= YandexConst.URI.REVOKE_TOKEN;
  params.AUTH_TYPE:= 'OAuth';
  _authSession:= TCloudDriverOAuth2PKCESession.Create( self, params );
end;

function TYandexClient.clone: TCloudDriver;
var
  newClient: TYandexClient;
begin
  newClient:= TYandexClient.Create( _config );
  newClient._authSession.Free;
  newClient._authSession:= TCloudDriverOAuth2Session( _authSession.clone(newClient) );
  Result:= newClient;
end;

function TYandexClient.createLister( const path: String ): TCloudDriverLister;
var
  session: TCloudDriverListFolderSession;
begin
  session:=  TYandexListFolderSession.Create( _authSession, path );
  Result:= TCloudDriverDefaultLister.Create( session );
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

procedure TYandexClient.delete( const path: String; const isFolder: Boolean );
var
  session: TYandexDeleteSession = nil;
begin
  try
    session:= TYandexDeleteSession.Create( _authSession, path, isFolder );
    session.delete;
  finally
    FreeAndNil( session );
  end;
end;

procedure TYandexClient.copyOrMove(const fromPath: String; const toPath: String;
  const isFolder: Boolean; const needToMove: Boolean );
var
  session: TYandexCopyMoveSession = nil;
begin
  try
    session:= TYandexCopyMoveSession.Create( _authSession, fromPath, toPath, isFolder );
    session.copyOrMove( needToMove );
  finally
    FreeAndNil( session );
  end;
end;

finalization
  FreeAndNil( yandexConfig );

end.

