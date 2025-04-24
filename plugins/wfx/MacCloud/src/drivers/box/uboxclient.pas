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
  uCloudDriver, uMiniHttpClient, uMiniUtil;

type

  { TBoxPathToIDSession }

  TBoxPathToIDSession = class
  private
    _authSession: TCloudDriverOAuth2Session;
  private
    function pathToID( const path: String; const uri: String; const raiseException: Boolean ): String;
  public
    constructor Create( const authSession: TCloudDriverOAuth2Session );
    function pathToFolderID( const path: String; const raiseException: Boolean = True ): String;
    function pathToFileID( const path: String; const raiseException: Boolean = True ): String;
  end;

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
    constructor Create( const authSession: TCloudDriverOAuth2Session; const path: String ); override;
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
  private
    procedure doAction( const needToMove: Boolean );
  public
    procedure copyOrMove( const needToMove: Boolean ); override;
  end;

  { TBoxClient }

  TBoxClient = class( TOAuth2SessionCloudDriver )
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
    procedure delete( const path: String; const isFolder: Boolean ); override;
    procedure copyOrMove( const fromPath: String; const toPath: String;
      const isFolder: Boolean; const needToMove: Boolean ); override;
  end;

var
  boxConfig: TCloudDriverConfig;

implementation

type
  TBoxConstURI = record
    OAUTH2: String;
    TOKEN: String;
    REVOKE_TOKEN: String;
    FOLDERS: String;
    FILES: String;
    UPLOAD_SMALL: String;
    UPLOAD_LARGE: String;
  end;

  TBoxConstHeader = record
    AUTH: String;
    ARG: String;
    RESULT: String;
  end;

  TBoxConstUpload = record
    LARGE_FILE_SIZE: Integer;
  end;

  TBoxConst = record
    URI: TBoxConstURI;
    HEADER: TBoxConstHeader;
    UPLOAD: TBoxConstUpload;
  end;

const
  BoxConst: TBoxConst = (
    URI: (
      OAUTH2:       'https://account.box.com/api/oauth2/authorize';
      TOKEN:        'https://api.box.com/oauth2/token';
      REVOKE_TOKEN: 'https://api.box.com/oauth2/revoke';
      FOLDERS:      'https://api.box.com/2.0/folders';
      FILES:        'https://api.box.com/2.0/files';
      UPLOAD_SMALL: 'https://upload.box.com/api/2.0/files';
      UPLOAD_LARGE: 'https://upload.box.com/api/2.0/files/upload_sessions';
    );
    HEADER: (
      AUTH: 'Authorization';
      ARG: 'Dropbox-API-Arg';
      RESULT: 'Dropbox-API-Result';
    );
    UPLOAD: (
      LARGE_FILE_SIZE:   1000*1000*20;  // 20MB
    );
  );

// raise the corresponding exception if there are errors
procedure BoxClientResultProcess( const cloudDriverResult: TCloudDriverResult );
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
        2:
          raise EFileNotFoundException.Create( httpErrorDescription );
        -1001:
          raise EInOutError.Create( httpErrorDescription );
        else
          raise ECloudDriverNetworkException.Create( httpErrorDescription );
      end;
    end;
  end;

  procedure processBoxError;
  begin
    cloudDriverMessage:= cloudDriverResult.resultMessage;

    if (httpResult.resultCode>=200) and (httpResult.resultCode<=299) then
      Exit;

    case httpResult.resultCode of
      401: raise ECloudDriverTokenException.Create( cloudDriverMessage );
      else raise ECloudDriverException.Create( cloudDriverMessage );
    end;
  end;

  procedure logException( const e: Exception );
  var
    message: String;
  begin
    message:= 'Box Error';
    if e.Message <> EmptyStr then
      message:= message + ': ' + e.Message;
    TLogUtil.logError( message );
  end;

begin
  try
    processHttpError;
    processBoxError;
  except
    on e: Exception do begin
      logException( e );
      raise;
    end;
  end;
end;

{ TBoxPathToIDSession }

constructor TBoxPathToIDSession.Create( const authSession: TCloudDriverOAuth2Session);
begin
  _authSession:= authSession;
end;

function TBoxPathToIDSession.pathToFolderID(
  const path: String;
  const raiseException: Boolean ): String;
var
  truePath: String;
begin
  if (path=EmptyStr) or (path=PathDelim) then
    Exit( '0' );
  truePath:= path;
  if truePath.EndsWith( PathDelim ) then
    truePath:= truePath.Substring( 0, truePath.Length-1 );
  Result:= pathToID( truePath, BoxConst.URI.FOLDERS, raiseException );
end;

function TBoxPathToIDSession.pathToFileID(
  const path: String;
  const raiseException: Boolean ): String;
begin
  Result:= pathToID( path, BoxConst.URI.FILES, raiseException );
end;

function TBoxPathToIDSession.pathToID(
  const path: String;
  const uri: String;
  const raiseException: Boolean ): String;

  function getIDByApi: NSArray;
  var
    http: TMiniHttpClient = nil;
    httpResult: TMiniHttpResult = nil;
    cloudDriverResult: TCloudDriverResult = nil;
    queryItems: TQueryItemsDictonary;
    json: NSDictionary;
  begin
    try
      queryItems:= TQueryItemsDictonary.Create;
      queryItems.Add( 'path', path );
      http:= TMiniHttpClient.Create( uri, HttpConst.Method.GET );
      http.setQueryParams( queryItems );
      http.setContentType( HttpConst.ContentType.UrlEncoded );
      _authSession.setAuthHeader( http );

      cloudDriverResult:= TCloudDriverResult.Create;
      httpResult:= http.connect;
      cloudDriverResult.httpResult:= httpResult;
      cloudDriverResult.resultMessage:= httpResult.body;
      BoxClientResultProcess( cloudDriverResult );

      json:= TJsonUtil.parse( cloudDriverResult.resultMessage );
      Result:= TJsonUtil.getArray( json, 'entries' );
    finally
      FreeAndNil( cloudDriverResult );
      FreeAndNil( http );
    end;
  end;
var
  jsonItems: NSArray;
  jsonItem: NSDictionary;
begin
  jsonItems:= getIDByApi;
  if jsonItems.count = 1 then begin
    jsonItem:= NSDictionary( jsonItems.objectAtIndex(0) );
    Result:= TJsonUtil.getString( jsonItem, 'id' );
  end;
  if raiseException and (Result=EmptyStr) then
    raise EFileNotFoundException.Create( 'Box Error, Path Not Found: ' + path );
end;

function BoxPathToFolderID(
  const authSession: TCloudDriverOAuth2Session;
  const path: String;
  const raiseException: Boolean = True ): String;
var
  session: TBoxPathToIDSession = nil;
begin
  try
    session:= TBoxPathToIDSession.Create( authSession );
    Result:= session.pathToFolderID( path, raiseException );
  finally
    session.Free;
  end;
end;

function BoxPathToFileID(
  const authSession: TCloudDriverOAuth2Session;
  const path: String;
  const raiseException: Boolean = True ): String;
var
  session: TBoxPathToIDSession = nil;
begin
  try
    session:= TBoxPathToIDSession.Create( authSession );
    Result:= session.pathToFileID( path, raiseException );
  finally
    session.Free;
  end;
end;

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
    folderID:= BoxPathToFolderID( _authSession, _path );
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

constructor TBoxListFolderSession.Create( const authSession: TCloudDriverOAuth2Session; const path: String );
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
    fileID:= BoxPathToFileID( _authSession, _serverPath );
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

  function getAttributesString: NSString;
  var
    jsonParent: NSMutableDictionary;
    parentID: String;
    parentPath: String;
    filename: String;
  begin
    parentPath:= TFileUtil.parentPath( _serverPath );
    filename:= TFileUtil.filename( _serverPath );
    parentID:= BoxPathToFolderID( _authSession, parentPath );
    jsonParent:= NSMutableDictionary.new;
    TJsonUtil.setString( jsonParent, 'id', parentID );
    Result:= TJsonUtil.dumps( ['name',filename, 'parent',jsonParent] );
    jsonParent.release;
  end;

var
  http: TMiniHttpClient = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  urlString: String;
  fileID: String;
  attribContentDisposition: NSString;
  attribString: NSString;
  fileContentDisposition: NSString;
  fileData: NSData;
  dataArray: TNSDataArray;
begin
  try
    attribContentDisposition:= NSSTR('Content-Disposition: form-data; name="attributes"');
    attribString:= getAttributesString;
    fileContentDisposition:= NSSTR('Content-Disposition: form-data; name="file"; filename=""');
    fileData:= TFileUtil.allContent( _localPath );
    dataArray:= [
      attribContentDisposition, attribString,
      fileContentDisposition, fileData ];

    fileID:= BoxPathToFileID( _authSession, _serverPath, False );;
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
    body: NSString;
    parentPath: String;
    parentID: String;
    filename: String;
    json: NSDictionary;
    jsonEndpoints: NSDictionary;
  begin
    try
      parentPath:= TFileUtil.parentPath( _serverPath );
      filename:= TFileUtil.filename( _serverPath );
      parentID:= BoxPathToFolderID( _authSession, parentPath );
      body:= TJsonUtil.dumps([
        'folder_id', parentID,
        'file_name', filename,
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
  body: NSString;
  parentID: String;
  parentPath: String;
  filename: String;
  jsonParent: NSMutableDictionary;
begin
  try
    jsonParent:= NSMutableDictionary.new;
    parentPath:= TFileUtil.parentPath( _path );
    filename:= TFileUtil.filename( _path );
    parentID:= BoxPathToFolderID( _authSession, parentPath );
    TJsonUtil.setString( jsonParent, 'id', parentID );
    body:= TJsonUtil.dumps( ['name',filename, 'parent',jsonParent] );
    jsonParent.release;

    http:= TMiniHttpClient.Create( BoxConst.URI.FOLDERS, HttpConst.Method.POST );
    http.setContentType( HttpConst.ContentType.JSON );
    _authSession.setAuthHeader( http );
    http.setBody( body );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    BoxClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
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
      urlString:= BoxConst.URI.FOLDERS + '/' + BoxPathToFolderID(_authSession,_path);
    end else begin
      urlString:= BoxConst.URI.FILES + '/' + BoxPathToFileID(_authSession,_path);
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

procedure TBoxCopyMoveSession.doAction(const needToMove: Boolean);
var
  urlString: String;
  http: TMiniHttpClient = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  body: NSString;
  json: NSMutableDictionary;
  jsonParent: NSMutableDictionary;
  toParentPath: String;
  toParentID: String;
  toFilename: String;
  method: TMiniHttpMethod;
begin
  try
    toParentPath:= TFileUtil.parentPath( _toPath );
    toFilename:= TFileUtil.filename( _toPath );
    json:= NSMutableDictionary.new;
    if _isFolder then begin
      urlString:= BoxConst.URI.FOLDERS + '/' + BoxPathToFolderID(_authSession,_fromPath);
    end else begin
      urlString:= BoxConst.URI.FILES + '/' + BoxPathToFileID(_authSession,_fromPath);
    end;
    if needToMove then begin
      method:= HttpConst.Method.PUT;
    end else begin
      urlString:= urlString + '/copy';
      method:= HttpConst.Method.POST;
    end;

    json:= NSMutableDictionary.new;
    TJsonUtil.setString( json, 'name', toFilename );
    toParentID:= BoxPathToFolderID( _authSession, toParentPath );
    jsonParent:= NSMutableDictionary.new;
    TJsonUtil.setString( jsonParent, 'id', toParentID );
    TJsonUtil.setDictionary( json, 'parent', jsonParent );
    jsonParent.release;

    body:= TJsonUtil.dumps( json );
    json.release;

    http:= TMiniHttpClient.Create( urlString, method );
    http.setContentType( HttpConst.ContentType.JSON );
    _authSession.setAuthHeader( http );
    http.setBody( body );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;

    BoxClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TBoxCopyMoveSession.copyOrMove( const needToMove: Boolean );
begin
  doAction( needToMove );
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

constructor TBoxClient.Create(const config: TCloudDriverConfig);
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
  params.AUTH_HEADER:= BoxConst.HEADER.AUTH;
  params.AUTH_TYPE:= 'Bearer';
  _authSession:= TCloudDriverOAuth2SecretSession.Create( self, params );
end;

function TBoxClient.clone: TCloudDriver;
var
  newClient: TBoxClient;
begin
  newClient:= TBoxClient.Create( _config );
  newClient._authSession:= self._authSession.clone( newClient );
  Result:= newClient;
end;

function TBoxClient.createLister( const path: String ): TCloudDriverLister;
begin
  Result:= TCloudDriverDefaultLister.Create( TBoxListFolderSession, _authSession, path );
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

