unit uBoxClientUtil;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  uCloudDriver, uMiniHttpClient, uMiniUtil;

type

  { TBoxPathComponents }

  TBoxPathComponents = class
  private
    _fullPath: String;
    _filename: String;
    _parentPath: String;
    _parentID: String;
  public
    constructor Create( const authSession: TCloudDriverAuthSession; const path: String );
    function toJsonString: NSString;
  public
    property filename: String read _filename;
    property parentPath: String read _parentPath;
    property parentID: String read _parentID;
  end;

  { TBoxClientUtil }

  TBoxClientUtil = class
  public
    class function pathToFolderID(
      const authSession: TCloudDriverAuthSession;
      const path: String;
      const raiseException: Boolean = True ): String;
    class function pathToFileID(
        const authSession: TCloudDriverAuthSession;
        const path: String;
        const raiseException: Boolean = True ): String;
  end;

  { TBoxPathToIDSession }

  TBoxPathToIDSession = class
  private
    _authSession: TCloudDriverAuthSession;
  private
    function pathToID( const path: String; const uri: String; const raiseException: Boolean ): String;
  public
    constructor Create( const authSession: TCloudDriverAuthSession );
    function pathToFolderID( const path: String; const raiseException: Boolean = True ): String;
    function pathToFileID( const path: String; const raiseException: Boolean = True ): String;
  end;

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
      ARG: 'Dropbox-API-Arg';
      RESULT: 'Dropbox-API-Result';
    );
    UPLOAD: (
      LARGE_FILE_SIZE:   1000*1000*20;  // 20MB
    );
  );

procedure BoxClientResultProcess( const cloudDriverResult: TCloudDriverResult );

implementation

{ TBoxPathToIDSession }

constructor TBoxPathToIDSession.Create( const authSession: TCloudDriverAuthSession);
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

{ TBoxPathComponents }

constructor TBoxPathComponents.Create(
  const authSession: TCloudDriverAuthSession; const path: String);
begin
  _fullPath:= path;
  _parentPath:= TFileUtil.parentPath( _fullPath );
  _filename:= TFileUtil.filename( _fullPath );
  _parentID:= TBoxClientUtil.pathToFolderID( authSession, _parentPath );
end;

function TBoxPathComponents.toJsonString: NSString;
var
  jsonParent: NSMutableDictionary;
begin
  jsonParent:= NSMutableDictionary.new;
  TJsonUtil.setString( jsonParent, 'id', _parentID );
  Result:= TJsonUtil.dumps( ['name',_filename, 'parent',jsonParent] );
  jsonParent.release;
end;

class function TBoxClientUtil.pathToFolderID(
  const authSession: TCloudDriverAuthSession;
  const path: String;
  const raiseException: Boolean ): String;
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

class function TBoxClientUtil.pathToFileID(
  const authSession: TCloudDriverAuthSession;
  const path: String;
  const raiseException: Boolean ): String;
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
      401: raise ECloudDriverAuthException.Create( cloudDriverMessage );
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

end.

