unit uAWSAuth;

{$mode ObjFPC}{$H+}
{$interfaces corba}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, uMiniCocoa,
  uCloudDriver,
  uAWSCore,
  uMiniHttpClient, uMiniUtil;

type

  { TAWSAuthSessionParams }

  TAWSAuthSessionParams = record
    config: TAWSCredentialConfig;
    defaultConnectionData: TAWSConnectionData;
  end;

  { TAWSSigner }

  TAWSSigner = class
  strict private
    _params: TAWSAuthSessionParams;
    _config: TAWSCredentialConfig;
    _connectionData: TAWSConnectionData;
    _accessKey: TAWSAccessKey;
    _request: NSMutableURLRequest;
  protected
    constructor Create(
      const params: TAWSAuthSessionParams;
      const accessKey: TAWSAccessKey;
      const connectionData: TAWSConnectionData;
      const request: NSMutableURLRequest );
  protected
    function buildDateYYYYMMDDString: NSString;
    function buildTimeStampString: NSString;
    function buildAuthString: NSString;
    function buildSignature: NSString;
    function buildSigningString: NSString;
    function buildSigningKey: NSData;
    function buildScopeString: NSString;
    function buildCredentialString: NSString;
    function buildHmacAlgorithmString: NSString;
  end;

  { TAWSAuthSession }

  TAWSAuthSession = class( TCloudDriverAuthSession )
  strict protected
    _params: TAWSAuthSessionParams;
    _accessKey: TAWSAccessKey;
  private
    procedure addNeededHeader( const request: NSMutableURLRequest );
  public
    constructor Create( const driver: TCloudDriver; const params: TAWSAuthSessionParams );
    destructor Destroy; override;
    procedure setAccessKey( const accessKey: TAWSAccessKey );
    function clone( const driver: TCloudDriver ): TCloudDriverAuthSession; override;
    procedure setAuthHeader(const http: TMiniHttpClient); override;
    procedure setAuthHeader( const http: TMiniHttpClient; const connectionData: TAWSConnectionData ); virtual; overload;
  public
    property params: TAWSAuthSessionParams read _params;
    property accessKey: TAWSAccessKey read _accessKey write setAccessKey;
    property defaultConnectionData: TAWSConnectionData read _params.defaultConnectionData write _params.defaultConnectionData;
  end;

implementation

type

  { TCanonicalRequestUtil }

  TCanonicalRequestUtil = class
  strict private
    class function buildCanonicalHeadersString( const request: NSMutableURLRequest ): NSString;
    class function buildHttpVerbString( const request: NSMutableURLRequest ): NSString;
    class function buildCanonicalURIString( const request: NSMutableURLRequest ): NSString;
    class function buildCanonicalQueryString( const request: NSMutableURLRequest ): NSString;
    class function buildHashedPayloadString: NSString;
  protected
    class function buildSignedHeadersString( const request: NSMutableURLRequest ): NSString;
    class function build( const request: NSMutableURLRequest ): NSString;
  end;

{ TAWSAuthSession }

procedure TAWSAuthSession.addNeededHeader( const request: NSMutableURLRequest );
  procedure addDateHeaderIfNeeded;
  var
    nowString: NSString;
  begin
    if request.valueForHTTPHeaderField(NSSTR(AWSConst.Header.Date)) <> nil then
      Exit;
    nowString:= TDateTimeUtil.nowToISO8601;
    request.addValue_forHTTPHeaderField( nowString, NSSTR(AWSConst.Header.Date) );
  end;

  procedure addHostHeaderIfNeeded;
  begin
    if request.valueForHTTPHeaderField(HttpConst.Header.Host) <> nil then
      Exit;
    request.addValue_forHTTPHeaderField( request.URL.host, HttpConst.Header.Host );
  end;

  procedure addTokenHeaderIfNeeded;
  begin
    if _accessKey.token = EmptyStr then
      Exit;
    request.addValue_forHTTPHeaderField(
      StringToNSString(_accessKey.token),
      NSSTR(AWSConst.Header.SECURITY_TOKEN) );
  end;

begin
  addDateHeaderIfNeeded;
  addHostHeaderIfNeeded;
  addTokenHeaderIfNeeded;
end;

constructor TAWSAuthSession.Create(
  const driver: TCloudDriver;
  const params: TAWSAuthSessionParams);
begin
  Inherited Create( driver );
  _params:= params;
  _accessKey:= TAWSAccessKey.Create( '', '', '' );
end;

destructor TAWSAuthSession.Destroy;
begin
  FreeAndNil( _accessKey );
end;

function TAWSAuthSession.clone( const driver: TCloudDriver ): TCloudDriverAuthSession;
var
  session: TAWSAuthSession;
begin
  session:= TAWSAuthSession.Create( driver, _params );
  session.setAccessKey( _accessKey.clone );
  Result:= session;
end;

procedure TAWSAuthSession.setAuthHeader(const http: TMiniHttpClient);
begin
  self.setAuthHeader( http, self.defaultConnectionData );
end;

procedure TAWSAuthSession.setAuthHeader(
  const http: TMiniHttpClient;
  const connectionData: TAWSConnectionData );
var
  request: NSMutableURLRequest;
  authString: NSString;
  signer: TAWSSigner = nil;
begin
  try
    request:= http.request;
    self.addNeededHeader( request );
    signer:= TAWSSigner.Create( params, _accessKey, connectionData, request );
    authString:= signer.buildAuthString;
    http.addHeader( HttpConst.Header.Authorization, authString );
  finally
    FreeAndNil( signer );
  end;
end;

procedure TAWSAuthSession.setAccessKey(const accessKey: TAWSAccessKey);
var
  oldAccessKey: TAWSAccessKey;
begin
  oldAccessKey:= _accessKey;
  _accessKey:= accessKey;
  oldAccessKey.Free;
end;

{ TAWSSigner }

constructor TAWSSigner.Create(
  const params: TAWSAuthSessionParams;
  const accessKey: TAWSAccessKey;
  const connectionData: TAWSConnectionData;
  const request: NSMutableURLRequest );
begin
  _params:= params;
  _config:= _params.config;
  _connectionData:= connectionData;
  _accessKey:= accessKey;
  _request:= request;
end;

function TAWSSigner.buildDateYYYYMMDDString: NSString;
begin
  Result:= self.buildTimeStampString.substringToIndex( 8 );
end;

function TAWSSigner.buildTimeStampString: NSString;
begin
  Result:= NSString( _request.allHTTPHeaderFields.objectForKey(NSSTR(AWSConst.Header.Date)) );
end;

function TAWSSigner.buildAuthString: NSString;
var
  hmacAlgorithm: NSString;
  credential: NSString;
  signedHeader: NSString;
  signature: NSString;
begin
  hmacAlgorithm:= buildHmacAlgorithmString;
  credential:= buildCredentialString;
  signedHeader:= TCanonicalRequestUtil.buildSignedHeadersString( _request );
  signature:= buildSignature;
  Result:= NSString.stringWithFormat( NSSTR('%@ Credential=%@,SignedHeaders=%@,Signature=%@'),
    hmacAlgorithm,
    credential,
    signedHeader,
    signature );
end;

function TAWSSigner.buildSignature: NSString;
var
  signingkeyData: NSData;
  signingString: NSString;
begin
  signingkeyData:= buildSigningKey;
  signingString:= buildSigningString;
  Result:= THashUtil.HmacSha256AndHexStr( signingkeyData, signingString );
end;

function TAWSSigner.buildSigningString: NSString;
var
  canonicalRequestString: NSString;
  canonicalRequestSha256HexString: NSString;
begin
  canonicalRequestString:= TCanonicalRequestUtil.build( _request );
  canonicalRequestSha256HexString:= THashUtil.sha256AndHexStr( canonicalRequestString );
  Result:= NSString.stringWithFormat( NSSTR('%@'#10'%@'#10'%@'#10'%@'),
    buildHmacAlgorithmString,
    buildTimeStampString,
    buildScopeString,
    canonicalRequestSha256HexString );
end;

function TAWSSigner.buildSigningKey: NSData;
var
  dateString: NSString;
  keyString: NSString;
  key: NSData;
  dateKey: NSData;
  dateRegionKey: NSData;
  dateRegionServiceKey: NSData;
begin
  dateString:= buildDateYYYYMMDDString;
  keyString:= StringToNSString( _config.prefix + _accessKey.secret );
  key:= keyString.dataUsingEncoding( NSUTF8StringEncoding );
  dateKey:= THashUtil.HmacSha256( key, dateString );
  dateRegionKey:= THashUtil.HmacSha256( dateKey, NSSTR(_connectionData.region) );
  dateRegionServiceKey:= THashUtil.HmacSha256( dateRegionKey, NSSTR(_config.service) );
  Result:= THashUtil.HmacSha256( dateRegionServiceKey, NSSTR(_config.request) );
end;

function TAWSSigner.buildHmacAlgorithmString: NSString;
begin
  Result:= NSSTR( _config.versionAlgorithm );
end;

function TAWSSigner.buildScopeString: NSString;
var
  signDate: NSString;
  signRegion: NSString;
  signService: NSString;
  signProduct: NSString;
begin
  signDate:= buildDateYYYYMMDDString;
  signService:= NSSTR( _config.service );
  signRegion:= NSSTR( _connectionData.region );
  signProduct:= NSSTR( _config.request );
  Result:= NSString.stringWithFormat( NSSTR('%@/%@/%@/%@'),
    signDate,
    signRegion,
    signService,
    signProduct );
end;

function TAWSSigner.buildCredentialString: NSString;
begin
  Result:= NSString.stringWithFormat( NSSTR('%@/%@'),
    NSSTR( _accessKey.id ),
    buildScopeString );
end;

{ TCanonicalRequest }

class function TCanonicalRequestUtil.build( const request: NSMutableURLRequest): NSString;
begin
  Result:= NSString.stringWithFormat( NSSTR('%@'#10'%@'#10'%@'#10'%@'#10'%@'#10'%@'),
    buildHttpVerbString(request),
    buildCanonicalURIString(request),
    buildCanonicalQueryString(request),
    buildCanonicalHeadersString(request),
    buildSignedHeadersString(request),
    buildHashedPayloadString);
end;

class function TCanonicalRequestUtil.buildHttpVerbString( const request: NSMutableURLRequest): NSString;
begin
  Result:= request.HTTPMethod;
end;

class function TCanonicalRequestUtil.buildCanonicalURIString( const request: NSMutableURLRequest): NSString;
var
  urlComponents: NSURLComponents;
begin
  urlComponents:= NSURLComponents.componentsWithURL_resolvingAgainstBaseURL( request.URL, False );
  Result:= urlComponents.percentEncodedPath;
end;

class function TCanonicalRequestUtil.buildCanonicalQueryString( const request: NSMutableURLRequest ): NSString;
  function createQueryDictionary: NSDictionary;
  var
    urlComponents: NSURLComponents;
    queryDictionary: NSMutableDictionary;
    queryItems: NSArray;
    queryItem: NSURLQueryItem;
    queryItemValue: NSString;
  begin
    urlComponents:= NSURLComponents.componentsWithURL_resolvingAgainstBaseURL( request.URL, False );
    queryItems:= urlComponents.queryItems;
    queryDictionary:= NSMutableDictionary.new;
    for queryItem in queryItems do begin
      queryItemValue:= NSString( queryItem.value );
      if queryItemValue = nil then
        queryItemValue:= NSString.string_;
      queryDictionary.setObject_forKey(
        THttpClientUtil.awsUriEncode( queryItemValue ),
        THttpClientUtil.awsUriEncode( queryItem.name ) );
    end;
    Result:= queryDictionary;
  end;
var
  headerStrings: NSMutableArray;
  queryDictionary: NSDictionary;
  itemNames: NSArray;
  itemName: NSString;
  itemValue: NSObject;
  itemString: NSString;
begin
  queryDictionary:= createQueryDictionary;
  itemNames:= queryDictionary.allKeys.sortedArrayUsingSelector( ObjcSelector('compare:') );
  headerStrings:= NSMutableArray.new;
  for itemName in itemNames do begin
    itemValue:= queryDictionary.objectForKey(itemName);
    itemString:= NSString.stringWithFormat( NSSTR('%@=%@'),
      itemName,
      itemValue );
    headerStrings.addObject( itemString );
  end;
  Result:= headerStrings.componentsJoinedByString( NSSTR('&') );
  headerStrings.release;
  queryDictionary.release;
end;

class function TCanonicalRequestUtil.buildCanonicalHeadersString( const request: NSMutableURLRequest ): NSString;
var
  headers: NSDictionary;
  sortedKeys: NSArray;
  headerKey: NSString;
  headerValue: NSString;
  headerString: NSMutableString;
begin
  headers:= request.allHTTPHeaderFields;
  sortedKeys:= headers.allKeys.sortedArrayUsingSelector( ObjcSelector('caseInsensitiveCompare:') );
  headerString:= NSMutableString.new.autorelease;
  for headerKey in sortedKeys do begin
    headerValue:= NSString( headers.objectForKey(headerKey) );
    headerValue:= headerValue.stringByTrimmingCharactersInSet( NSCharacterSet.whitespaceAndNewlineCharacterSet );
    headerString.appendString( headerKey.lowercaseString );
    headerString.appendString( NSSTR(':') );
    headerString.appendString( headerValue );
    headerString.appendString( NSSTR(#10) );
  end;
  Result:= headerString;
end;

class function TCanonicalRequestUtil.buildSignedHeadersString( const request: NSMutableURLRequest ): NSString;
var
  headers: NSDictionary;
  sortedKeys: NSArray;
  headerStrings: NSMutableArray;
  headerKey: NSString;
begin
  headers:= request.allHTTPHeaderFields;
  sortedKeys:= headers.allKeys.sortedArrayUsingSelector( ObjcSelector('caseInsensitiveCompare:') );
  headerStrings:= NSMutableArray.new;
  for headerKey in sortedKeys do begin
    headerStrings.addObject( headerKey.lowercaseString );
  end;
  Result:= headerStrings.componentsJoinedByString( NSSTR(';') );
  headerStrings.release;
end;

class function TCanonicalRequestUtil.buildHashedPayloadString: NSString;
begin
  Result:= NSSTR( AWSConst.HEADER.CONTENT_SHA256_DEFAULT_VALUE );
end;

end.

