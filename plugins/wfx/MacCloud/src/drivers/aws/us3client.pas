{
   Notes:
   1. the basic S3 Client
   2. TS3Client is the parent class of many Cloud Drivers that follow the S3 protocol.
   3. it's also an abstract class and requires subclasses to provide the specific parameters.
   4. no dependencies on other libraries
}

unit uS3Client;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, syncobjs, Contnrs, DateUtils,
  CocoaAll,
  uCloudDriver, uAWSCore, uAWSAuth,
  uMiniHttpClient, uMiniUtil;

type

  TS3AccessKey = TAWSAccessKey;

  TS3Config = TAWSCredentialConfig;

  TS3ConfigPtr = ^TS3Config;

  { TS3Bucket }

  TS3Bucket = class
  public
    connectionData: TAWSConnectionData;
    creationTime: TDateTime;
  public
    function clone: TS3Bucket;
  end;

  { TS3Buckets }

  TS3Buckets = class
  strict private
    _items: TFPObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function clone: TS3Buckets;
  public
    procedure add( const bucket: TS3Bucket );
    function get( const name: String ): TS3Bucket; overload;
    function get( const index: Integer ): TS3Bucket; overload; inline;
    function Count: Integer; inline;
    property items[const index: Integer]: TS3Bucket read get; default;
  end;

  { TS3GetAllBucketsSession }

  TS3GetAllBucketsSession = class
  protected
    _authSession: TCloudDriverAuthSession;
  private
    function analyseListResult( const listString: String ): TS3Buckets;
  protected
    procedure constructBucket( const bucket: TS3Bucket; const xmlBucket: NSXMLElement ); virtual; abstract;
    function getConnectionDataOfService: TAWSConnectionData; virtual; abstract;
    function getEndPointOfRegion( const region: String ): String; virtual; abstract;
  public
    constructor Create( const authSession: TCloudDriverAuthSession );
    function listBuckets: TS3Buckets;
  end;

  { TS3GetAllBucketsWithRegionFunctionSession }

  TS3GetAllBucketsWithRegionFunctionSession = class( TS3GetAllBucketsSession )
  protected
    function getRegionOfBucket( const name: String ): String;
  end;

  { TS3BucketsLister }

  TS3BucketsLister = class( TCloudDriverLister )
  private
    _buckets: TS3Buckets;
    _currentIndex: Integer;
  public
    constructor Create( const buckets: TS3Buckets );
    destructor Destroy; override;
    procedure listFolderBegin; override;
    function  listFolderGetNextFile: TCloudFile; override;
    procedure listFolderEnd; override;
  end;

  { TS3ListFolderSession }

  TS3ListFolderSession = class( TCloudDriverListFolderSession )
  private
    _connectionData: TAWSConnectionData;
    _continuationToken: String;
  private
    procedure analyseListResult( const listString: String );
  protected
    procedure listFolderFirst; override;
    procedure listFolderContinue; override;
  public
    constructor Create(
      const authSession: TCloudDriverAuthSession;
      const connectionData: TAWSConnectionData;
      const path: String ); overload;
  end;

  { TS3DownloadSession }

  TS3DownloadSession = class( TCloudDriverDownloadSession )
  public
    procedure download; override;
  end;

  { TS3UploadSession }

  TS3UploadSession = class( TCloudDriverUploadSession )
  public
    procedure upload; override;
  end;

  { TS3CreateFolderSession }

  TS3CreateFolderSession = class( TCloudDriverCreateFolderSession )
  public
    procedure createFolder; override;
  end;

  { TS3DeleteSession }

  TS3DeleteSession = class( TCloudDriverDeleteSession )
  public
    procedure delete; override;
  end;

  { TS3CopyMoveSession }

  TS3CopyMoveSession = class( TCloudDriverCopyMoveSession )
  private
    procedure doCopyFile;
  public
    procedure copyOrMove( const needToMove: Boolean ); override;
  end;

  { TS3PathParser }

  TS3PathParser = class
  private
    _bucketName: String;
    _bucketPath: String;
    _connetionData: TAWSConnectionData;
  public
    constructor Create( const fullpath: String );
    property bucketName: String read _bucketName;
    property bucketPath: String read _bucketPath;
    property connetionData: TAWSConnectionData read _connetionData;
  end;

  { TS3Client }

  TS3Client = class( TAWSCloudDriver )
  strict protected
    _lockObject: TCriticalSection;
  protected
    _authSession: TAWSAuthSession;
    _buckets: TS3Buckets;
  protected
    function autoBuildBuckets: TS3Buckets; virtual; abstract;
    function getConnectionDataOfBucket( const name: String ): TAWSConnectionData;
  public
    constructor Create;
    destructor Destroy; override;
    function clone: TCloudDriver; override;
  public
    function getAllBuckets: TS3Buckets;
    function createLister( const path: String ): TCloudDriverLister; override;
  public
    function authorize: Boolean; override;
    procedure unauthorize; override;
    function authorized: Boolean; override;
    function getAccessKey: TAWSAccessKey; override;
    procedure setAccessKey(const accessKey: TAWSAccessKey); override;
    function getDefaultConnectionData: TAWSConnectionData; override;
    procedure setDefaultConnectionData(const connectionData: TAWSConnectionData); override;
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

implementation

var
  s3CloudDriverConfig: TS3Config;

procedure s3BuckPathHelper(
  const authSession: TCloudDriverAuthSession;
  const path: String;
  var bucketPath: String;
  var connectionData: TAWSConnectionData );
var
  parser: TS3PathParser = nil;
  driver: TS3Client;
begin
  try
    parser:= TS3PathParser.Create( path );
    driver:= TS3Client( authSession.cloudDriver );
    bucketPath:= parser.bucketPath;
    connectionData:= driver.getConnectionDataOfBucket( parser.bucketName );
  finally
    FreeAndNil( parser );
  end;
end;

// raise the corresponding exception if there are errors
procedure S3ClientResultProcess( const cloudDriverResult: TCloudDriverResult );
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

  procedure processS3Error;
  begin
    cloudDriverMessage:= cloudDriverResult.resultMessage;

    if (httpResult.resultCode>=200) and (httpResult.resultCode<=299) then
      Exit;

    case httpResult.resultCode of
      403: raise ECloudDriverAuthException.Create( cloudDriverMessage );
      else raise ECloudDriverException.Create( cloudDriverMessage );
    end;
  end;

  procedure logException( const e: Exception );
  var
    message: String;
  begin
    message:= 'S3 Error';
    if e.Message <> EmptyStr then
      message:= message + ': ' + e.Message;
    TLogUtil.logError( message );
  end;

begin
  try
    processHttpError;
    processS3Error;
  except
    on e: Exception do begin
      logException( e );
      raise;
    end;
  end;
end;

{ TS3ListFolderSession }

procedure TS3ListFolderSession.analyseListResult( const listString: String );
var
  prefixLength: Integer;
  xml: NSXMLElement;

  procedure constructFiles;
  var
    cloudFile: TCloudFile;
    xmlContents: NSArray;
    xmlContent: NSXMLElement;
    name: String;
    size: Integer;
    modificationTime: TDateTime;
  begin
    xmlContents:= xml.elementsForName( NSSTR('Contents') );
    for xmlContent in xmlContents do begin
      name:= TXmlUtil.getString( xmlContent, 'Key' );
      name:= name.Substring( prefixLength );
      if name = EmptyStr then
        continue;
      size:= TXmlUtil.getInteger( xmlContent, 'Size' );
      modificationTime:= ISO8601ToDate( TXmlUtil.getString(xmlContent,'LastModified') );
      cloudFile:= TCloudFile.Create;
      cloudFile.name:= name;
      cloudFile.size:= size;
      cloudFile.modificationTime:= modificationTime;
      _files.Add( cloudFile );
    end;
  end;

  procedure constructFolders;
  var
    cloudFile: TCloudFile;
    xmlCommonPrefixes: NSArray;
    xmlCommonPrefix: NSXMLElement;
    name: String;
  begin
    xmlCommonPrefixes:= xml.elementsForName( NSSTR('CommonPrefixes') );
    for xmlCommonPrefix in xmlCommonPrefixes do begin
      name:= TXmlUtil.getString( xmlCommonPrefix, 'Prefix' );
      name:= name.Substring( prefixLength, name.Length - prefixLength - 1 );
      cloudFile:= TCloudFile.Create;
      cloudFile.name:= name;
      cloudFile.isFolder:= True;
      _files.Add( cloudFile );
    end;
  end;

begin
  prefixLength:= _path.Length;
  xml:= TXmlUtil.parse( listString );
  constructFiles;
  constructFolders;
  _continuationToken:= TXmlUtil.getString( xml, 'NextContinuationToken' );
  _hasMore:= _continuationToken <> EmptyStr;
end;

procedure TS3ListFolderSession.listFolderFirst;
begin
  listFolderContinue;
end;

procedure TS3ListFolderSession.listFolderContinue;
var
  http: TMiniHttpClient = nil;
  httpResult: TMiniHttpResult = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  urlString: String;
  queryItems: TQueryItemsDictonary;
begin
  try
    urlString:= 'https://' + _connectionData.bucketName + '.' + _connectionData.endPoint + '/?';
    queryItems:= TQueryItemsDictonary.Create;
    queryItems.Add( 'list-type', '2' );
    queryItems.Add( 'delimiter', '/' );
    queryItems.Add( 'prefix', _path );
    if _continuationToken <> EmptyStr then
      queryItems.Add( 'continuation-token', _continuationToken);
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.GET );
    http.setQueryParams( queryItems );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    TAWSAuthSession(_authSession).setAuthHeader( http, _connectionData );

    cloudDriverResult:= TCloudDriverResult.Create;
    httpResult:= http.connect;
    cloudDriverResult.httpResult:= httpResult;
    cloudDriverResult.resultMessage:= httpResult.body;

    if httpResult.resultCode = 200 then
      analyseListResult( httpResult.body );

    S3ClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

constructor TS3ListFolderSession.Create(
  const authSession: TCloudDriverAuthSession;
  const connectionData: TAWSConnectionData;
  const path: String );
var
  truePath: String;
begin
  _connectionData:= connectionData;
  truePath:= path;
  if truePath.StartsWith( '/' ) then
    truePath:= truePath.Substring( 1 );
  if truePath <> EmptyStr then begin
    if NOT truePath.EndsWith( '/' ) then
      truePath:= truePath + '/';
  end;
  Inherited Create( authSession, truePath );
end;

{ TS3DownloadSession }

procedure TS3DownloadSession.download;
var
  http: TMiniHttpClient = nil;
  bucketPath: String;
  connectionData: TAWSConnectionData;
  urlString: String;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    s3BuckPathHelper( _authSession, _serverPath, bucketPath, connectionData );
    if bucketPath = EmptyStr then
      raise ENotSupportedException.Create( 'Does not support downloading without S3 bucket' );
    urlString:= 'https://' + connectionData.bucketName + '.' + connectionData.endPoint + THttpClientUtil.urlEncode(bucketPath);
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.GET );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    TAWSAuthSession(_authSession).setAuthHeader( http, connectionData );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.download( _localPath, _callback );
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    S3ClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TS3UploadSession }

procedure TS3UploadSession.upload;
var
  http: TMiniHttpClient = nil;
  bucketPath: String;
  connectionData: TAWSConnectionData;
  urlString: String;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    s3BuckPathHelper( _authSession, _serverPath, bucketPath, connectionData );
    if bucketPath = EmptyStr then
      raise ENotSupportedException.Create( 'Does not support uploading without S3 bucket' );
    urlString:= 'https://' + connectionData.bucketName + '.' + connectionData.endPoint + THttpClientUtil.urlEncode(bucketPath);
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.PUT );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    TAWSAuthSession(_authSession).setAuthHeader( http, connectionData );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.upload( _localPath, _callback );
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    S3ClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TS3CreateFolderSession }

procedure TS3CreateFolderSession.createFolder;
var
  http: TMiniHttpClient = nil;
  bucketPath: String;
  connectionData: TAWSConnectionData;
  urlString: String;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    s3BuckPathHelper( _authSession, _path, bucketPath, connectionData );
    if bucketPath = EmptyStr then
      raise ENotSupportedException.Create( 'Does not support creating S3 buckets' );
    urlString:= 'https://' + connectionData.bucketName + '.' + connectionData.endPoint + THttpClientUtil.urlEncode(bucketPath+'/');
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.PUT );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    TAWSAuthSession(_authSession).setAuthHeader( http, connectionData );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    S3ClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TS3DeleteSession }

procedure TS3DeleteSession.delete;
var
  http: TMiniHttpClient = nil;
  bucketPath: String;
  connectionData: TAWSConnectionData;
  urlString: String;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    s3BuckPathHelper( _authSession, _path, bucketPath, connectionData );
    if bucketPath = EmptyStr then
      raise ENotSupportedException.Create( 'Does not support deleting S3 bucket' );
    urlString:= 'https://' + connectionData.bucketName + '.' + connectionData.endPoint + THttpClientUtil.urlEncode(bucketPath);
    if _isFolder then
      urlString:= urlString + '/';
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.DELETE );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    TAWSAuthSession(_authSession).setAuthHeader( http, connectionData );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    S3ClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TS3CopyMoveSession }

procedure TS3CopyMoveSession.doCopyFile;
var
  http: TMiniHttpClient = nil;
  toBucketPath: String;
  toConnectionData: TAWSConnectionData;
  fromBucketPath: String;
  fromConnectionData: TAWSConnectionData;
  urlString: String;
  cloudDriverResult: TCloudDriverResult = nil;
  sourceHeaderString: String;
begin
  try
    s3BuckPathHelper( _authSession, _toPath, toBucketPath, toConnectionData );
    s3BuckPathHelper( _authSession, _fromPath, fromBucketPath, fromConnectionData );
    if (toBucketPath=EmptyStr) or (fromBucketPath=EmptyStr) then
      raise ENotSupportedException.Create( 'Does not support coping S3 bucket' );
    urlString:= 'https://' + toConnectionData.bucketName + '.' + toConnectionData.endPoint + THttpClientUtil.urlEncode(toBucketPath);
    sourceHeaderString:= '/' + fromConnectionData.bucketName + THttpClientUtil.urlEncode(fromBucketPath);
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.PUT );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    http.addHeader( AWSConst.HEADER.COPY_SOURCE, sourceHeaderString );
    TAWSAuthSession(_authSession).setAuthHeader( http, fromConnectionData );

    cloudDriverResult:= TCloudDriverResult.Create;
    cloudDriverResult.httpResult:= http.connect;
    cloudDriverResult.resultMessage:= cloudDriverResult.httpResult.body;
    S3ClientResultProcess( cloudDriverResult );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

procedure TS3CopyMoveSession.copyOrMove(const needToMove: Boolean);
begin
  if _isFolder then
    raise ENotSupportedException.Create( 'Rename Folder not supported' );

  self.doCopyFile;

  if needToMove then
    _authSession.cloudDriver.delete( _fromPath, False );
end;

{ TS3PathParser }

constructor TS3PathParser.Create(const fullpath: String);
var
  i: Integer;
begin
  i:= fullpath.IndexOf( PathDelim , 1 );
  if i < 0 then begin
    _bucketName:= fullpath.Substring( 1 );
  end else begin
    _bucketName:= fullpath.Substring( 1, i-1 );
    _bucketPath:= fullpath.Substring( i );
    if _bucketPath = EmptyStr then
      _bucketPath:= PathDelim;
  end;
end;

{ TS3Bucket }

function TS3Bucket.clone: TS3Bucket;
begin
  Result:= TS3Bucket.Create;
  Result.connectionData:= self.connectionData;
  Result.creationTime:= self.creationTime;
end;

{ TS3Buckets }

constructor TS3Buckets.Create;
begin
  _items:= TFPObjectList.Create;
end;

destructor TS3Buckets.Destroy;
begin
  FreeAndNil( _items );
end;

procedure TS3Buckets.add( const bucket: TS3Bucket );
begin
  if (bucket.connectionData.bucketName=EmptyStr) or
     (bucket.connectionData.region=EmptyStr) or
     (bucket.connectionData.endPoint=EmptyStr)
  then begin
    bucket.Free;
    Exit;
  end;

  _items.Add( bucket );
end;

function TS3Buckets.get( const name: String ): TS3Bucket;
var
  bucket: TS3Bucket;
  i: Integer;
begin
  for i:=0 to _items.Count-1 do begin
    bucket:= TS3Bucket( _items[i] );
    if bucket.connectionData.bucketName = name then begin
      Result:= bucket;
      Exit;
    end;
  end;
  Result:= nil;
end;

function TS3Buckets.get(const index: Integer): TS3Bucket;
begin
  Result:= TS3Bucket( _items[index] );
end;

function TS3Buckets.Count: Integer;
begin
  Result:= _items.Count;
end;

function TS3Buckets.clone: TS3Buckets;
var
  i: Integer;
begin
  Result:= TS3Buckets.Create;
  for i:=0 to _items.Count-1 do begin
    Result.add( self.get(i).clone );
  end;
end;

{ TS3GetAllBucketsSession }

function TS3GetAllBucketsSession.analyseListResult( const listString: String ): TS3Buckets;
var
  xml: NSXMLElement;
  xmlBuckets: NSXMLElement;
  xmlBucketArray: NSArray;
  xmlBucket: NSXMLElement;
  bucket: TS3Bucket;
  buckets: TS3Buckets;
begin
  buckets:= TS3Buckets.Create;
  xml:= TXmlUtil.parse( listString );
  xmlBuckets:=  TXMLUtil.getElement( xml, 'Buckets' );
  xmlBucketArray:= xmlBuckets.elementsForName( NSSTR('Bucket') );
  for xmlBucket in xmlBucketArray do begin
    bucket:= TS3Bucket.Create;
    bucket.connectionData.bucketName:= TXmlUtil.getString( xmlBucket, 'Name' );
    bucket.creationTime:= ISO8601ToDate( TXmlUtil.getString( xmlBucket, 'CreationDate' ) );
    self.constructBucket( bucket, xmlBucket );
    buckets.Add( bucket );
  end;
  Result:= buckets;
end;

constructor TS3GetAllBucketsSession.Create( const authSession: TCloudDriverAuthSession );
begin
  _authSession:= authSession;
end;

function TS3GetAllBucketsSession.listBuckets: TS3Buckets;
var
  connectionData: TAWSConnectionData;
  http: TMiniHttpClient = nil;
  httpResult: TMiniHttpResult = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  urlString: String;
begin
  Result:= nil;
  try
    connectionData:= self.getConnectionDataOfService;
    urlString:= 'https://' + connectionData.endPoint + '/';
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.GET );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    TAWSAuthSession(_authSession).setAuthHeader( http, connectionData );

    cloudDriverResult:= TCloudDriverResult.Create;
    httpResult:= http.connect;
    cloudDriverResult.httpResult:= httpResult;
    cloudDriverResult.resultMessage:= httpResult.body;

    if httpResult.resultCode = 200 then begin
      Result:= analyseListResult( httpResult.body );
    end else begin
      TLogUtil.logError( 'Error in TS3GetAllBucketsSession.listBuckets(): ' + cloudDriverResult.resultMessage );
    end;
  finally
    if Result = nil then
      Result:= TS3Buckets.Create;
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TS3GetAllBucketsWithRegionFunctionSession }

function TS3GetAllBucketsWithRegionFunctionSession.getRegionOfBucket( const name: String ): String;
var
  connectionData: TAWSConnectionData;
  endPoint: String;
  http: TMiniHttpClient = nil;
  httpResult: TMiniHttpResult = nil;
  cloudDriverResult: TCloudDriverResult = nil;
  urlString: String;
begin
  try
    connectionData:= self.getConnectionDataOfService;
    endPoint:= self.getEndPointOfRegion( connectionData.region );
    urlString:= 'https://' + name + '.' + endPoint + '/';
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.HEAD );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    _authSession.setAuthHeader( http );

    cloudDriverResult:= TCloudDriverResult.Create;
    httpResult:= http.connect;
    cloudDriverResult.httpResult:= httpResult;
    cloudDriverResult.resultMessage:= httpResult.body;
    Result:= httpResult.getHeader( AWSConst.HEADER.BUCKET_REGION );
  finally
    FreeAndNil( cloudDriverResult );
    FreeAndNil( http );
  end;
end;

{ TS3BucketsLister }

constructor TS3BucketsLister.Create( const buckets: TS3Buckets );
begin
  _buckets:= buckets;
end;

destructor TS3BucketsLister.Destroy;
begin
  FreeAndNil( _buckets );
end;

procedure TS3BucketsLister.listFolderBegin;
begin
end;

function TS3BucketsLister.listFolderGetNextFile: TCloudFile;
var
  bucket: TS3Bucket;
begin
  if _currentIndex >= _buckets.Count then
    Exit( nil );
  bucket:= _buckets[_currentIndex];
  Result:= TCloudFile.Create;
  Result.name:= bucket.connectionData.bucketName;
  Result.creationTime:= bucket.creationTime;
  Result.isFolder:= True;
  inc( _currentIndex );
end;

procedure TS3BucketsLister.listFolderEnd;
begin
  self.Free;
end;

{ TS3Client }

constructor TS3Client.Create;
var
  params: TAWSAuthSessionParams;
begin
  params:= Default( TAWSAuthSessionParams );
  params.config:= s3CloudDriverConfig;
  _authSession:= TAWSAuthSession.Create( self, params );
  _lockObject:= TCriticalSection.Create;
end;

destructor TS3Client.Destroy;
begin
  FreeAndNil( _authSession );
  FreeAndNil( _buckets );
  FreeAndNil( _lockObject );
end;

function TS3Client.getConnectionDataOfBucket( const name: String ): TAWSConnectionData;
var
  bucket: TS3Bucket;
begin
  _lockObject.Acquire;
  try
    self.getAllBuckets;
    bucket:= _buckets.get( name );
    if bucket <> nil then
      Result:= bucket.connectionData
    else
      Result:= self.getDefaultConnectionData;
  finally
    _lockObject.Release;
  end;
end;

function TS3Client.clone: TCloudDriver;
var
  newClient: TS3Client;
begin
  newClient:= TS3Client( self.createInstance );
  newClient._authSession.Free;
  newClient._authSession:= TAWSAuthSession( _authSession.clone(newClient) );
  Result:= newClient;
end;

function TS3CLient.getAllBuckets: TS3Buckets;
  procedure createBuckets;
  var
    bucket: TS3Bucket;
  begin
    if _buckets <> nil then
      Exit;
    _buckets:= self.autoBuildBuckets;
    if _buckets.Count > 0 then
      Exit;

    bucket:= TS3Bucket.Create;
    bucket.connectionData:= self.getDefaultConnectionData;
    _buckets.add( bucket );
  end;
begin
  _lockObject.Acquire;
  try
    createBuckets;
    Result:= _buckets;
  finally
    _lockObject.Release;
  end;
end;

function TS3Client.createLister(const path: String): TCloudDriverLister;
var
  parser: TS3PathParser;
  connectionData: TAWSConnectionData;
  listFolderSession: TCloudDriverListFolderSession;
begin
  parser:= TS3PathParser.Create( path );
  _lockObject.Acquire;
  try
    self.getAllBuckets;
    if Path = EmptyStr then begin
      Result:= TS3BucketsLister.Create( _buckets.clone );
    end else begin
      connectionData:= _buckets.get(parser.bucketName).connectionData;
      listFolderSession:= TS3ListFolderSession.Create( _authSession, connectionData, parser.bucketPath );
      Result:= TCloudDriverDefaultLister.Create( listFolderSession );
    end;
  finally
    FreeAndNil( parser );
    _lockObject.Release;
  end;
end;

function TS3Client.authorize: Boolean;
begin
  Result:= True;
end;

procedure TS3Client.unauthorize;
begin
end;

function TS3Client.authorized: Boolean;
begin
  Result:= True;
end;

function TS3Client.getAccessKey: TAWSAccessKey;
begin
  Result:= _authSession.accessKey;
end;

procedure TS3Client.setAccessKey(const accessKey: TAWSAccessKey);
begin
  _authSession.accessKey:= accessKey;
  _lockObject.Acquire;
  try
    FreeAndNil( _buckets );
  finally
    _lockObject.Release;
  end;
end;

function TS3Client.getDefaultConnectionData: TAWSConnectionData;
begin
  Result:= _authSession.defaultConnectionData;
end;

procedure TS3Client.setDefaultConnectionData(const connectionData: TAWSConnectionData);
begin
  _authSession.defaultConnectionData:= connectionData;
end;

procedure TS3Client.download(
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback);
var
  session: TCloudDriverDownloadSession = nil;
begin
  try
    session:= TS3DownloadSession.Create( _authSession, serverPath, localPath, callback );
    session.download;
  finally
    FreeAndNil( session );
  end;
end;

procedure TS3Client.upload(
  const serverPath: String;
  const localPath: String;
  const callback: ICloudProgressCallback);
var
  session: TCloudDriverUploadSession = nil;
begin
  try
    session:= TS3UploadSession.Create( _authSession, serverPath, localPath, callback );
    session.upload;
  finally
    FreeAndNil( session );
  end;
end;

procedure TS3Client.createFolder(const path: String);
var
  session: TCloudDriverCreateFolderSession = nil;
begin
  try
    session:= TS3CreateFolderSession.Create( _authSession, path );
    session.createFolder;
  finally
    FreeAndNil( session );
  end;
end;

procedure TS3Client.delete(const path: String; const isFolder: Boolean);
var
  session: TS3DeleteSession = nil;
begin
  try
    session:= TS3DeleteSession.Create( _authSession, path, isFolder );
    session.delete;
  finally
    FreeAndNil( session );
  end;
end;

procedure TS3Client.copyOrMove(const fromPath: String; const toPath: String;
  const isFolder: Boolean; const needToMove: Boolean);
var
  session: TS3CopyMoveSession = nil;
begin
  try
    session:= TS3CopyMoveSession.Create( _authSession, fromPath, toPath, isFolder );
    session.copyOrMove( needToMove );
  finally
    FreeAndNil( session );
  end;
end;

initialization
  s3CloudDriverConfig:= TS3Config.Create(
    'AWS4-HMAC-SHA256',
    'AWS4',
    's3',
    'aws4_request' );

finalization
  FreeAndNil( s3CloudDriverConfig );

end.

