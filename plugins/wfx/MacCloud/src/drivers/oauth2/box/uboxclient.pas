{
   Notes:
   1. the basic Box Client
   2. no dependencies on other libraries
}

unit uBoxClient;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, DateUtils,
  CocoaAll, uMiniCocoa,
  uCloudDriver, uOAuth2Core, uOAuth2Auth, uOAuth2Client,
  uBoxClientUtil,
  uMiniHttpClient, uMiniUtil;

type

  { TBoxListFolderSession }

  TBoxListFolderSession = class( TCloudDriverListFolderSession )
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

  { TBoxDownloadSession }

  TBoxDownloadSession = class( TCloudDriverDownloadSession )
  public
    procedure download; override;
  end;

  { TBoxUploadSession }

  TBoxUploadSession = class( TCloudDriverUploadSession )
  private
    procedure uploadSmall;
    procedure uploadLarge;
  public
    procedure upload; override;
  end;

  { TBoxCreateFolderSession }

  TBoxCreateFolderSession = class( TCloudDriverCreateFolderSession )
  public
    procedure createFolder; override;
  end;

  { TBoxDeleteSession }

  TBoxDeleteSession = class( TCloudDriverDeleteSession )
  public
    procedure delete; override;
  end;

  { TBoxCopyMoveSession }

  TBoxCopyMoveSession = class( TCloudDriverCopyMoveSession )
  public
    procedure copyOrMove( const needToMove: Boolean ); override;
  end;

  { TBoxClient }

  TBoxClient = class( TOAuth2SessionCloudDriver )
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
  boxConfig: TTokenCloudDriverConfig;

implementation

const
  BOX_CLIENT_SECRET = '*';

{ TBoxListFolderSession }

procedure TBoxListFolderSession.listFolderFirst;
begin
  listFolderContinue;
end;

procedure TBoxListFolderSession.listFolderContinue;
var
  http: TMiniHttpClient = nil;
  httpResult: TMiniHttpResult = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  urlString: String;
  queryItems: TQueryItemsDictonary;
  folderID: String;
begin
  try
    folderID:= TBoxClientUtil.pathToFolderID( _authSession, _path );
    urlString:= BoxConst.URI.FOLDERS + '/' + folderID + '/items';
    queryItems:= TQueryItemsDictonary.Create;
    queryItems.Add( 'offset', IntToStr(_offset) );
    queryItems.Add( 'limit', IntToStr(LIMIT) );
    queryItems.Add( 'fields', 'type,name,size,created_at,modified_at' );
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.GET );
    http.setQueryParams( queryItems );
    http.setContentType( HttpConst.ContentType.UrlEncoded );
    _authSession.setAuthHeader( http );

    cloudDriverResult:= TCloudDriverResult.Create;
    httpResult:= http.connect;
    cloudDriverResult.httpResult:= httpResult;
    cloudDriverResult.resultMessage:= httpResult.body;

    if httpResult.resultCode = 200 then
      analyseListResult( httpResult.body );

    BoxClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TBoxListFolderSession.analyseListResult(const jsonString: String);
var
  json: NSDictionary;
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
  _total:= TJsonUtil.getInteger( json, 'total_count' );
  _offset:= TJsonUtil.getInteger( json, 'offset' );
  jsonItems:= TJsonUtil.getArray( json, 'entries' );
  if jsonItems = nil then
    Exit;
  for jsonItem in jsonItems do begin
    cloudFile:= TCloudFile.Create;
    cloudFile.isFolder:= ( TJsonUtil.getString(jsonItem,'type')='folder' );
    cloudFile.name:= TJsonUtil.getString( jsonItem, 'name' );
    cloudFile.size:= TJsonUtil.getInteger( jsonItem, 'size' );
    cloudFile.creationTime:= toDateTime( 'created_at' );
    cloudFile.modificationTime:= toDateTime( 'modified_at' );
    _files.Add( cloudFile );
    inc( _offset );
  end;
  _hasMore:= ( _offset < _total );
end;

constructor TBoxListFolderSession.Create( const authSession: TCloudDriverAuthSession; const path: String );
var
  truePath: String;
begin
  if path = PathDelim then
    truePath:= EmptyStr
  else
    truePath:= path;
  Inherited Create( authSession, truePath );
end;

{ TBoxDownloadSession }

procedure TBoxDownloadSession.download;
var
  http: TMiniHttpClient = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  fileID: String;
  urlString: String;
begin
  try
    fileID:= TBoxClientUtil.pathToFileID( _authSession, _serverPath );
    urlString:= BoxConst.URI.FILES + '/' + fileID + '/content';
    http:= TMiniHttpClient.Create( urlString , HttpConst.Method.GET );
    _authSession.setAuthHeader( http );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.download( _localPath, _callback );
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;

    BoxClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TBoxUploadSession }

procedure TBoxUploadSession.uploadSmall;
var
  http: TMiniHttpClient = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  boxPath: TBoxPathComponents = nil;
  urlString: String;
  fileID: String;
  attribContentDisposition: NSString;
  fileContentDisposition: NSString;
  fileData: NSData;
  dataArray: TNSDataArray;
begin
  try
    attribContentDisposition:= NSSTR('Content-Disposition: form-data; name="attributes"');
    boxPath:= TBoxPathComponents.Create( _authSession, _serverPath );
    fileContentDisposition:= NSSTR('Content-Disposition: form-data; name="file"; filename=""');
    fileData:= TFileUtil.allContent( _localPath );
    dataArray:= [
      attribContentDisposition, boxPath.toJsonString,
      fileContentDisposition, fileData ];

    fileID:= TBoxClientUtil.pathToFileID( _authSession, _serverPath, False );;
    if fileID = EmptyStr then
      urlString:= BoxConst.URI.UPLOAD_SMALL + '/content'
    else
      urlString:= BoxConst.URI.UPLOAD_SMALL + '/' + fileID + '/content';
    http:= TMiniHttpClient.Create( urlString , HttpConst.Method.POST );
    _authSession.setAuthHeader( http );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.postMultiPart( dataArray, _callback );
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;

    BoxClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
    FreeAndNil( boxPath );
  end;
end;

procedure TBoxUploadSession.uploadLarge;
var
  sessionId: String;
  range: TMiniContentRange;
  offset: Integer;
  sessionSize: Integer;
  uploadPartSize: Integer;
  uploadURI: String;
  uploadFinishURI: String;
  uploadParts: NSMutableArray;

  procedure uploadStart;
  var
    http: TMiniHttpClient = nil;
    cloudDriverResult: TCloudDriverResult = nil;
    boxPath: TBoxPathComponents = nil;
    body: NSString;
    json: NSDictionary;
    jsonEndpoints: NSDictionary;
  begin
    try
      boxPath:= TBoxPathComponents.Create( _authSession, _serverPath );
      body:= TJsonUtil.dumps([
        'folder_id', boxPath.parentID,
        'file_name', boxPath.filename,
        'file_size', _localFileSize ]);

      http:= TMiniHttpClient.Create( BoxConst.URI.UPLOAD_LARGE, HttpConst.Method.POST );
      _authSession.setAuthHeader( http );
      http.setContentType( HttpConst.ContentType.JSON );
      http.setBody( body );

      cloudDriverResult:= TCloudDriverResult.Create;
      cloudDriverResult.httpResult:= http.connect;
      cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
      BoxClientResultProcess( cloudDriverResult );

      json:= TJsonUtil.parse( cloudDriverResult.resultMessage );
      sessionId:= TJsonUtil.getString( json, 'id' );
      uploadPartSize:= TJsonUtil.getInteger( json, 'part_size' );
      jsonEndpoints:= TJsonUtil.getDictionary( json, 'session_endpoints' );
      uploadURI:= TJsonUtil.getString( jsonEndpoints, 'upload_part' );
      uploadFinishURI:= TJsonUtil.getString( jsonEndpoints, 'commit' );

      if sessionId = EmptyStr then
        raise ECloudDriverException.Create( 'can''t get session_id in TBoxUploadSession.uploadLarge.uploadStart()' );
    finally
      FreeAndNil( cloudDriverResult );
      FreeAndNil( http );
      FreeAndNil( boxPath );
    end;
  end;

  procedure uploadAppend( const range: TMiniContentRange );
  var
    http: TMiniHttpClient = nil;
    cloudDriverResult: TCloudDriverResult = nil;
    digest: String;
    json: NSDictionary;
    jsonPart: NSDictionary;
  begin
    try
      digest:= TFileUtil.sha1( _localPath, range );
      http:= TMiniHttpClient.Create( uploadURI, HttpConst.Method.PUT );
      http.setContentRange( range );
      http.addHeader( 'digest', 'sha=' + digest );
      _authSession.setAuthHeader( http );

      cloudDriverResult:= TCloudDriverResult.Create;
      cloudDriverResult.httpResult:= http.uploadRange(
        _localPath,
        range,
        _callback );
      cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
      BoxClientResultProcess( cloudDriverResult );

      json:= TJsonUtil.parse( cloudDriverResult.resultMessage );
      jsonPart:= TJsonUtil.getDictionary( json, 'part' );
      uploadParts.addObject( jsonPart );
    finally
      FreeAndNil( cloudDriverResult );
      FreeAndNil( http );
    end;
  end;

  procedure uploadFinish;
  var
    http: TMiniHttpClient = nil;
    cloudDriverResult: TCloudDriverResult = nil;
    digest: String;
  begin
    try
      digest:= TFileUtil.sha1( _localPath );
      http:= TMiniHttpClient.Create( uploadFinishURI, HttpConst.Method.POST );
      http.addHeader( 'digest', 'sha=' + digest );
      _authSession.setAuthHeader( http );
      http.setContentType( HttpConst.ContentType.JSON );
      http.setBody( TJsonUtil.dumps( ['parts', uploadParts] ) );

      cloudDriverResult:= TCloudDriverResult.Create;
      cloudDriverResult.httpResult:= http.connect;
      cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;

      BoxClientResultProcess( cloudDriverResult );
    finally
      FreeAndNil( cloudDriverResult );
      FreeAndNil( http );
    end;
  end;

begin
  try
    offset:= 0;
    uploadParts:= NSMutableArray.new;
    uploadStart;
    while offset < _localFileSize do begin
      if offset + uploadPartSize > _localFileSize then
        sessionSize:= _localFileSize - offset
      else
        sessionSize:= uploadPartSize;
      range:= TMiniContentRange.Create( offset, offset+sessionSize-1 , _localFileSize );
      uploadAppend( range );
      inc( offset, sessionSize );
      range.Free;
    end;
    uploadFinish;
  finally
    uploadParts.release;
  end;
end;

procedure TBoxUploadSession.upload;
begin
  if _localFileSize <= BoxConst.UPLOAD.LARGE_FILE_SIZE then
    uploadSmall
  else
    uploadLarge;
end;

{ TBoxCreateFolderSession }

procedure TBoxCreateFolderSession.createFolder;
var
  http: TMiniHttpClient = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  boxPath: TBoxPathComponents = nil;
begin
  try
    boxPath:= TBoxPathComponents.Create( _authSession, _path );
    http:= TMiniHttpClient.Create( BoxConst.URI.FOLDERS, HttpConst.Method.POST );
    http.setContentType( HttpConst.ContentType.JSON );
    _authSession.setAuthHeader( http );
    http.setBody( boxPath.toJsonString );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    BoxClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
    FreeAndNil( boxPath );
  end;
end;

{ TBoxDeleteSession }

procedure TBoxDeleteSession.delete;
var
  http: TMiniHttpClient = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  urlString: String;
begin
  try
    if _isFolder then begin
      urlString:= BoxConst.URI.FOLDERS + '/' + TBoxClientUtil.pathToFolderID(_authSession,_path);
    end else begin
      urlString:= BoxConst.URI.FILES + '/' + TBoxClientUtil.pathToFileID(_authSession,_path);
    end;
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.DELETE );
    http.setContentType( HttpConst.ContentType.JSON );
    _authSession.setAuthHeader( http );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    BoxClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TBoxCopyMoveSession }

procedure TBoxCopyMoveSession.copyOrMove( const needToMove: Boolean );
var
  http: TMiniHttpClient = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  boxPath: TBoxPathComponents = nil;
  urlString: String;
  method: TMiniHttpMethod;
begin
  try
    boxPath:= TBoxPathComponents.Create( _authSession, _toPath );
    if _isFolder then begin
      urlString:= BoxConst.URI.FOLDERS + '/' + TBoxClientUtil.pathToFolderID(_authSession,_fromPath);
    end else begin
      urlString:= BoxConst.URI.FILES + '/' + TBoxClientUtil.pathToFileID(_authSession,_fromPath);
    end;
    if needToMove then begin
      method:= HttpConst.Method.PUT;
    end else begin
      urlString:= urlString + '/copy';
      method:= HttpConst.Method.POST;
    end;

    http:= TMiniHttpClient.Create( urlString, method );
    http.setContentType( HttpConst.ContentType.JSON );
    _authSession.setAuthHeader( http );
    http.setBody( boxPath.toJsonString );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    BoxClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
    FreeAndNil( boxPath );
  end;
end;

{ TBoxClient }

class function TBoxClient.driverName: String;
begin
  Result:= 'Box';
end;

class function TBoxClient.createInstance: TCloudDriver;
begin
  Result:= TBoxClient.Create( boxConfig );
end;

constructor TBoxClient.Create(const config: TTokenCloudDriverConfig);
var
  params: TCloudDriverOAuth2SessionParams;
begin
  Inherited Create( config );
  params.config:= config;
  params.resultProcessFunc:= @BoxClientResultProcess;
  params.scope:= EmptyStr;
  params.OAUTH2_URI:= BoxConst.URI.OAUTH2;
  params.TOKEN_URI:= BoxConst.URI.TOKEN;
  params.REVOKE_TOKEN_URI:= BoxConst.URI.REVOKE_TOKEN;
  params.AUTH_TYPE:= 'Bearer';
  _authSession:= TCloudDriverOAuth2SecretSession.Create( self, params, BOX_CLIENT_SECRET );
end;

function TBoxClient.clone: TCloudDriver;
var
  newClient: TBoxClient;
begin
  newClient:= TBoxClient.Create( _config );
  newClient._authSession.Free;
  newClient._authSession:= TCloudDriverOAuth2Session( _authSession.clone(newClient) );
  Result:= newClient;
end;

function TBoxClient.createLister( const path: String ): TCloudDriverLister;
var
  session: TCloudDriverListFolderSession;
begin
  session:=  TBoxListFolderSession.Create( _authSession, path );
  Result:= TCloudDriverDefaultLister.Create( session );
end;

procedure TBoxClient.download(
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback );
var
  session: TBoxDownloadSession = nil;
begin
  try
    session:= TBoxDownloadSession.Create( _authSession, serverPath, localPath, callback );
    session.download;
  finally
    FreeAndNil( session );
  end;
end;

procedure TBoxClient.upload(
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback);
var
  session: TBoxUploadSession = nil;
begin
  try
    session:= TBoxUploadSession.Create( _authSession, serverPath, localPath, callback );
    session.upload;
  finally
    FreeAndNil( session );
  end;
end;

procedure TBoxClient.createFolder(const path: String);
var
  session: TBoxCreateFolderSession = nil;
begin
  try
    session:= TBoxCreateFolderSession.Create( _authSession, path );
    session.createFolder;
  finally
    FreeAndNil( session );
  end;
end;

procedure TBoxClient.delete( const path: String; const isFolder: Boolean );
var
  session: TBoxDeleteSession = nil;
begin
  try
    session:= TBoxDeleteSession.Create( _authSession, path, isFolder );
    session.delete;
  finally
    FreeAndNil( session );
  end;
end;

procedure TBoxClient.copyOrMove(const fromPath: String; const toPath: String;
  const isFolder: Boolean; const needToMove: Boolean );
var
  session: TBoxCopyMoveSession = nil;
begin
  try
    session:= TBoxCopyMoveSession.Create( _authSession, fromPath, toPath, isFolder );
    session.copyOrMove( needToMove );
  finally
    FreeAndNil( session );
  end;
end;

finalization
  FreeAndNil( boxConfig );

end.

