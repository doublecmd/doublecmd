{
   Notes:
   1. the basic One Drive Client
   2. no dependencies on other libraries
}

unit uOneDriveClient;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, DateUtils,
  CocoaAll,
  uCloudDriver, uOAuth2Core, uOAuth2Auth, uOAuth2Client,
  uMiniHttpClient, uMiniUtil;

type

  { TOneDriveListFolderSession }

  TOneDriveListFolderSession = class( TCloudDriverListFolderSession )
  private
    _nextLink: String;
  private
    procedure analyseListResult( const jsonString: String );
  protected
    procedure listFolderFirst; override;
    procedure listFolderContinue; override;
  public
    constructor Create( const authSession: TCloudDriverAuthSession; const path: String ); override;
  end;

  { TOneDriveDownloadSession }

  TOneDriveDownloadSession = class( TCloudDriverDownloadSession )
  public
    procedure download; override;
  end;

  { TOneDriveUploadSession }

  TOneDriveUploadSession = class( TCloudDriverUploadSession )
  public
    procedure upload; override;
  end;

  { TOneDriveCreateFolderSession }

  TOneDriveCreateFolderSession = class( TCloudDriverCreateFolderSession )
  public
    procedure createFolder; override;
  end;

  { TOneDriveDeleteSession }

  TOneDriveDeleteSession = class( TCloudDriverDeleteSession )
  public
    procedure delete; override;
  end;

  { TOneDriveCopyMoveSession }

  TOneDriveCopyMoveSession = class( TCloudDriverCopyMoveSession )
  public
    procedure copyOrMove( const needToMove: Boolean ); override;
  end;

  { TOneDriveClient }

  TOneDriveClient = class( TOAuth2SessionCloudDriver )
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
  oneDriveConfig: TTokenCloudDriverConfig;

implementation

type
  TOneDriveConstURI = record
    OAUTH2: String;
    TOKEN: String;
    REVOKE_TOKEN: String;
    RESOURCES: String;
  end;

  TOneDriveConst = record
    URI: TOneDriveConstURI;
  end;

const
  OneDriveConst: TOneDriveConst = (
    URI: (
      OAUTH2: 'https://login.microsoftonline.com/common/oauth2/v2.0/authorize';
      TOKEN: 'https://login.microsoftonline.com/common/oauth2/v2.0/token';
      REVOKE_TOKEN: 'https://login.microsoftonline.com/common/oauth2/v2.0/logout';
      RESOURCES:   'https://graph.microsoft.com/v1.0';
    );
  );

function toOneDriveResourceItemPath( const path: String; const action: String = '' ): String;
begin
  Result:= '/me/drive/root';
  if path = PathDelim then begin
    if action = EmptyStr then
      Result:= Result + ':/'
    else
      Result:= Result + '/' + action;
  end else begin
    Result:= Result + ':' + path;
    if action <> EmptyStr then
      Result:= Result + ':/' + action;
  end;
end;

function toOneDriveResourceItemUrlEncode( const path: String; const action: String = '' ): String;
begin
  Result:= toOneDriveResourceItemPath( path, action );
  Result:= THttpClientUtil.urlEncode( Result );
end;

// raise the corresponding exception if there are errors
procedure OneDriveClientResultProcess( const cloudDriverResult: TCloudDriverResult );
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
      403:
        raise ECloudDriverPermissionException.Create( cloudDriverMessage );
      404:
        raise EFileNotFoundException.Create( cloudDriverMessage );
      409:
        raise ECloudDriverConflictException.Create( cloudDriverMessage );
      429, 509:
        raise ECloudDriverRateLimitException.Create( cloudDriverMessage );
      507:
        raise ECloudDriverQuotaException.Create( cloudDriverMessage );
      else
        raise ECloudDriverException.Create( cloudDriverMessage );
    end;
  end;

  procedure logException( const e: Exception );
  var
    message: String;
  begin
    message:= 'OneDrive Error';
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

{ TOneDriveListFolderSession }

procedure TOneDriveListFolderSession.listFolderFirst;
begin
  listFolderContinue;
end;

procedure TOneDriveListFolderSession.listFolderContinue;
var
  http: TMiniHttpClient = nil;
  httpResult: TMiniHttpResult = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  urlString: String;
begin
  try
    urlString:= OneDriveConst.URI.RESOURCES + toOneDriveResourceItemUrlEncode(_path,'children');
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.GET );
    http.setContentType( HttpConst.ContentType.UrlEncoded );
    _authSession.setAuthHeader( http );

    cloudDriverResult:= TCloudDriverResult.Create;
    httpResult:= http.connect;
    cloudDriverResult.httpResult:= httpResult;
    cloudDriverResult.resultMessage:= httpResult.body;

    if httpResult.resultCode = 200 then
      analyseListResult( httpResult.body );

    OneDriveClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TOneDriveListFolderSession.analyseListResult(const jsonString: String);
var
  json: NSDictionary;
  jsonItems: NSArray;
  jsonItem: NSDictionary;
  jsonFolder: NSDictionary;
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
  jsonItems:= TJsonUtil.getArray( json, 'value' );
  if jsonItems = nil then
    Exit;
  for jsonItem in jsonItems do begin
    cloudFile:= TCloudFile.Create;
    jsonFolder:= TJsonUtil.getDictionary( jsonItem, 'folder' );
    cloudFile.isFolder:= Assigned( jsonFolder );
    cloudFile.name:= TJsonUtil.getString( jsonItem, 'name' );
    cloudFile.size:= TJsonUtil.getInteger( jsonItem, 'size' );
    cloudFile.creationTime:= toDateTime( 'createdDateTime' );
    cloudFile.modificationTime:= toDateTime( 'lastModifiedDateTime' );
    _files.Add( cloudFile );
  end;
  _nextLink:= TJsonUtil.getString( json, '@odata.nextLink' );
  _hasMore:= (_nextLink <> EmptyStr);
end;

constructor TOneDriveListFolderSession.Create( const authSession: TCloudDriverAuthSession; const path: String );
var
  truePath: String;
begin
  if path = EmptyStr then begin
    truePath:= PathDelim
  end else begin
    truePath:= path;
    if (truePath<>PathDelim) and truePath.EndsWith( PathDelim ) then
      truePath:= truePath.Substring( 0, truePath.Length-1 );
  end;
  Inherited Create( authSession, truePath );
end;

{ TOneDriveDownloadSession }

procedure TOneDriveDownloadSession.download;
  function getDownloadHref: String;
  var
    urlString: String;
    http: TMiniHttpClient = nil;
    cloudDriverResult: TCloudDriverResult = nil;
    json: NSDictionary;
  begin
    try
      urlString:= OneDriveConst.URI.RESOURCES + toOneDriveResourceItemUrlEncode(_serverPath);
      http:= TMiniHttpClient.Create( urlString, HttpConst.Method.GET );
      _authSession.setAuthHeader( http );

      cloudDriverResult:= TCloudDriverResult.Create;
      cloudDriverResult.httpResult:= http.connect;
      cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
      OneDriveClientResultProcess( cloudDriverResult );

      json:= TJsonUtil.parse( cloudDriverResult.resultMessage );
      Result:= TJsonUtil.getString( json, '@microsoft.graph.downloadUrl' );
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
      OneDriveClientResultProcess( cloudDriverResult );
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

{ TOneDriveUploadSession }

procedure TOneDriveUploadSession.upload;
  function getUploadHref: String;
  var
    urlString: String;
    http: TMiniHttpClient = nil;
    cloudDriverResult: TCloudDriverResult = nil;
    json: NSDictionary;
  begin
    try
      urlString:= OneDriveConst.URI.RESOURCES + toOneDriveResourceItemUrlEncode(_serverPath, 'createUploadSession');
      http:= TMiniHttpClient.Create( urlString, HttpConst.Method.POST );
      _authSession.setAuthHeader( http );

      cloudDriverResult:= TCloudDriverResult.Create;
      cloudDriverResult.httpResult:= http.connect;
      cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
      OneDriveClientResultProcess( cloudDriverResult );

      json:= TJsonUtil.parse( cloudDriverResult.resultMessage );
      Result:= TJsonUtil.getString( json, 'uploadUrl' );
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
      OneDriveClientResultProcess( cloudDriverResult );
    finally
      FreeAndNil( cloudDriverResult );
      FreeAndNil( http );
    end;
  end;

const
  SESSION_FILE_SIZE = 1024*32*1024; // 32MB
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

{ TOneDriveCreateFolderSession }

procedure TOneDriveCreateFolderSession.createFolder;
var
  http: TMiniHttpClient = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  urlString: String;
  body: NSString;
  parentPath: String;
  fileName: String;
begin
  try
    parentPath:= TFileUtil.parentPath( _path );
    fileName:= TFileUtil.filename( _path );
    body:= TJsonUtil.dumps( ['name', filename, 'folder', NSDictionary.new.autorelease] );
    urlString:= OneDriveConst.URI.RESOURCES + toOneDriveResourceItemUrlEncode(parentPath, 'children' );
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.POST );
    _authSession.setAuthHeader( http );
    http.setContentType( HttpConst.ContentType.JSON );
    http.setBody( body );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    OneDriveClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TOneDriveDeleteSession }

procedure TOneDriveDeleteSession.delete;
var
  http: TMiniHttpClient = nil;
  urlString: String;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    urlString:= OneDriveConst.URI.RESOURCES + toOneDriveResourceItemUrlEncode(_path);
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.DELETE );
    _authSession.setAuthHeader( http );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    OneDriveClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TOneDriveCopyMoveSession }

procedure TOneDriveCopyMoveSession.copyOrMove( const needToMove: Boolean );
var
  http: TMiniHttpClient = nil;
  urlString: String;
  method: TMiniHttpMethod;
  body: NSString;
  parentPath: String;
  fileName: String;
  jsonParent: NSMutableDictionary;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    parentPath:= TFileUtil.parentPath( _toPath );
    fileName:= TFileUtil.filename( _toPath );
    jsonParent:= NSMutableDictionary.new;
    TJsonUtil.setString( jsonParent, 'path', toOneDriveResourceItemPath(parentPath) );
    body:= TJsonUtil.dumps( ['name', filename, 'parentReference', jsonParent] );
    jsonParent.release;
    if needToMove then begin
      urlString:= OneDriveConst.URI.RESOURCES + toOneDriveResourceItemUrlEncode(_fromPath);
      method:= HttpConst.Method.PATCH;
    end else begin
      urlString:= OneDriveConst.URI.RESOURCES + toOneDriveResourceItemUrlEncode(_fromPath,'copy');
      method:= HttpConst.Method.POST;
    end;

    http:= TMiniHttpClient.Create( urlString, method );
    _authSession.setAuthHeader( http );
    http.setContentType( HttpConst.ContentType.JSON );
    http.setBody( body );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    OneDriveClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TOneDriveClient }

class function TOneDriveClient.driverName: String;
begin
  Result:= 'OneDrive';
end;

class function TOneDriveClient.createInstance: TCloudDriver;
begin
  Result:= TOneDriveClient.Create( oneDriveConfig );
end;

constructor TOneDriveClient.Create(const config: TTokenCloudDriverConfig);
var
  params: TCloudDriverOAuth2SessionParams;
begin
  Inherited Create( config );
  params.config:= config;
  params.resultProcessFunc:= @OneDriveClientResultProcess;
  params.scope:= 'user.read+files.readwrite.all+offline_access';
  params.OAUTH2_URI:= OneDriveConst.URI.OAUTH2;
  params.TOKEN_URI:= OneDriveConst.URI.TOKEN;
  params.REVOKE_TOKEN_URI:= OneDriveConst.URI.REVOKE_TOKEN;
  params.AUTH_TYPE:= 'Bearer';
  _authSession:= TCloudDriverOAuth2PKCESession.Create( self, params );
end;

function TOneDriveClient.clone: TCloudDriver;
var
  newClient: TOneDriveClient;
begin
  newClient:= TOneDriveClient.Create( _config );
  newClient._authSession.Free;
  newClient._authSession:= TCloudDriverOAuth2Session( _authSession.clone(newClient) );
  Result:= newClient;
end;

function TOneDriveClient.createLister( const path: String ): TCloudDriverLister;
var
  session: TCloudDriverListFolderSession;
begin
  session:=  TOneDriveListFolderSession.Create( _authSession, path );
  Result:= TCloudDriverDefaultLister.Create( session );
end;

procedure TOneDriveClient.download(
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback );
var
  session: TOneDriveDownloadSession = nil;
begin
  try
    session:= TOneDriveDownloadSession.Create( _authSession, serverPath, localPath, callback );
    session.download;
  finally
    FreeAndNil( session );
  end;
end;

procedure TOneDriveClient.upload(
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback);
var
  session: TOneDriveUploadSession = nil;
begin
  try
    session:= TOneDriveUploadSession.Create( _authSession, serverPath, localPath, callback );
    session.upload;
  finally
    FreeAndNil( session );
  end;
end;

procedure TOneDriveClient.createFolder(const path: String);
var
  session: TOneDriveCreateFolderSession = nil;
begin
  try
    session:= TOneDriveCreateFolderSession.Create( _authSession, path );
    session.createFolder;
  finally
    FreeAndNil( session );
  end;
end;

procedure TOneDriveClient.delete( const path: String; const isFolder: Boolean );
var
  session: TOneDriveDeleteSession = nil;
begin
  try
    session:= TOneDriveDeleteSession.Create( _authSession, path, isFolder );
    session.delete;
  finally
    FreeAndNil( session );
  end;
end;

procedure TOneDriveClient.copyOrMove(const fromPath: String; const toPath: String;
  const isFolder: Boolean; const needToMove: Boolean );
var
  session: TOneDriveCopyMoveSession = nil;
begin
  try
    session:= TOneDriveCopyMoveSession.Create( _authSession, fromPath, toPath, isFolder );
    session.copyOrMove( needToMove );
  finally
    FreeAndNil( session );
  end;
end;

finalization
  FreeAndNil( oneDriveConfig );

end.

