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
  Classes, SysUtils, DateUtils,
  CocoaAll,
  uCloudDriver, uAWSCore, uAWSAuth,
  uMiniHttpClient, uMiniUtil;

type

  TS3AccessKey = TAWSAccessKey;

  TS3Config = TAWSConfig;

  TS3AuthSessionParams = TAWSAuthSessionParams;

  { TS3ListFolderSession }

  TS3ListFolderSession = class( TCloudDriverListFolderSession )
  private
    _params: TS3AuthSessionParams;
    _continuationToken: String;
  private
    procedure analyseListResult( const listString: String );
  protected
    procedure listFolderFirst; override;
    procedure listFolderContinue; override;
  public
    constructor Create( const authSession: TCloudDriverAuthSession; const path: String ); override;
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

  { TS3Client }

  TS3Client = class( TAWSCloudDriver )
  protected
    _config: TS3Config;
    _authSession: TAWSAuthSession;
  public
    constructor Create( const config: TS3Config );
    destructor Destroy; override;
  public
    function createLister( const path: String ): TCloudDriverLister; override;
  public
    function authorize: Boolean; override;
    procedure unauthorize; override;
    function authorized: Boolean; override;
    function getAccessKey: TAWSAccessKey; override;
    procedure setAccessKey(const accessKey: TAWSAccessKey); override;
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
    urlString:= 'https://' + _params.defaultBucket + '.' + _params.endPoint + '/?';
    queryItems:= TQueryItemsDictonary.Create;
    queryItems.Add( 'list-type', '2' );
    queryItems.Add( 'delimiter', '/' );
    queryItems.Add( 'prefix', _path );
    if _continuationToken <> EmptyStr then
      queryItems.Add( 'continuation-token', _continuationToken);
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.GET );
    http.setQueryParams( queryItems );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    _authSession.setAuthHeader( http );

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

constructor TS3ListFolderSession.Create( const authSession: TCloudDriverAuthSession; const path: String );
var
  truePath: String;
begin
  _params:= TAWSAuthSession(authSession).params;
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
  params: TS3AuthSessionParams;
  urlString: String;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    params:= TAWSAuthSession(_authSession).params;
    urlString:= 'https://' + params.defaultBucket + '.' + params.endPoint + THttpClientUtil.urlEncode(_serverPath);
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.GET );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    _authSession.setAuthHeader( http );

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
  params: TS3AuthSessionParams;
  urlString: String;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    params:= TAWSAuthSession(_authSession).params;
    urlString:= 'https://' + params.defaultBucket + '.' + params.endPoint + THttpClientUtil.urlEncode(_serverPath);
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.PUT );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    _authSession.setAuthHeader( http );

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
  params: TS3AuthSessionParams;
  urlString: String;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    params:= TAWSAuthSession(_authSession).params;
    urlString:= 'https://' + params.defaultBucket + '.' + params.endPoint + THttpClientUtil.urlEncode(_path+'/');
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.PUT );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    _authSession.setAuthHeader( http );

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
  params: TS3AuthSessionParams;
  urlString: String;
  cloudDriverResult: TCloudDriverResult = nil;
begin
  try
    params:= TAWSAuthSession(_authSession).params;
    urlString:= 'https://' + params.defaultBucket + '.' + params.endPoint + THttpClientUtil.urlEncode(_path);
    if _isFolder then
      urlString:= urlString + '/';
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.DELETE );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    _authSession.setAuthHeader( http );

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
  params: TS3AuthSessionParams;
  urlString: String;
  cloudDriverResult: TCloudDriverResult = nil;
  sourceHeaderString: String;
begin
  try
    params:= TAWSAuthSession(_authSession).params;
    urlString:= 'https://' + params.defaultBucket + '.' + params.endPoint + THttpClientUtil.urlEncode(_toPath);
    sourceHeaderString:= '/' + params.defaultBucket + THttpClientUtil.urlEncode(_fromPath);
    http:= TMiniHttpClient.Create( urlString, HttpConst.Method.PUT );
    http.addHeader( AWSConst.HEADER.CONTENT_SHA256, AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
    http.addHeader( AWSConst.HEADER.COPY_SOURCE, sourceHeaderString );
    _authSession.setAuthHeader( http );

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
  if needToMove then
    raise ENotSupportedException.Create( 'Move / Rename not supported' );
  if _isFolder then
    raise ENotSupportedException.Create( 'Copy Folder not supported' );
  self.doCopyFile;
end;

{ TS3Client }

constructor TS3Client.Create( const config: TS3Config );
begin
  _config:= config;
end;

destructor TS3Client.Destroy;
begin
  FreeAndNil( _authSession );
end;

function TS3Client.createLister(const path: String): TCloudDriverLister;
begin
  Result:= TCloudDriverDefaultLister.Create( TS3ListFolderSession, _authSession, path );
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

end.

