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

  { TAWSAuthSession }

  TAWSAuthSession = class( TCloudDriverAuthSession )
  strict protected
    _config: TAWSConfig;
  private
    procedure addNeededHeader( const request: NSMutableURLRequest );
  public
    constructor Create( const config: TAWSConfig );
    function clone( const driver: TCloudDriver ): TCloudDriverAuthSession; override;
    procedure setAuthHeader( const http: TMiniHttpClient ); override;
  public
    property config: TAWSConfig read _config;
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

  { TSignUtil }

  TSignUtil = class
  strict private
    class function buildDateYYYYMMDDString( const request: NSMutableURLRequest ): NSString;
    class function buildTimeStampString( const request: NSMutableURLRequest ): NSString;
  protected
    class function buildAuthString(
      const config: TAWSConfig;
      const request: NSMutableURLRequest ): NSString;
    class function buildSignature(
      const config: TAWSConfig;
      const request: NSMutableURLRequest ): NSString;
    class function buildSigningString(
      const config: TAWSConfig;
      const request: NSMutableURLRequest ): NSString;
    class function buildSigningKey(
      const config: TAWSConfig;
      const request: NSMutableURLRequest ): NSData;
    class function buildScopeString(
      const config: TAWSConfig;
      const request: NSMutableURLRequest ): NSString;
    class function buildCredentialString(
      const config: TAWSConfig;
      const request: NSMutableURLRequest ): NSString;
    class function buildHmacAlgorithmString( const config: TAWSConfig ): NSString;
  end;

{ TAWSAuthSession }

procedure TAWSAuthSession.addNeededHeader( const request: NSMutableURLRequest );
  procedure addDateHeaderIfNeeded;
  var
    nowString: NSString;
  begin
    if request.valueForHTTPHeaderField(HttpConst.Header.Date) <> nil then
      Exit;
    nowString:= TDateTimeUtil.nowToISO8601;
    request.addValue_forHTTPHeaderField( nowString, HttpConst.Header.Date );
  end;

  procedure addHostHeaderIfNeeded;
  begin
    if request.valueForHTTPHeaderField(HttpConst.Header.Host) <> nil then
      Exit;
    request.addValue_forHTTPHeaderField( request.URL.host, HttpConst.Header.Host );
  end;
begin
  addDateHeaderIfNeeded;
  addHostHeaderIfNeeded;
end;

constructor TAWSAuthSession.Create(const config: TAWSConfig);
begin
  _config:= config;
  _config.credentialPrefixAndSecret:= _config.credentialPrefix + _config.accessKeySecret;
end;

function TAWSAuthSession.clone( const driver: TCloudDriver ): TCloudDriverAuthSession;
begin
  Result:= TAWSAuthSession.Create( _config );
end;

procedure TAWSAuthSession.setAuthHeader( const http: TMiniHttpClient );
var
  request: NSMutableURLRequest;
  authString: NSString;
begin
  request:= http.request;
  self.addNeededHeader( request );
  authString:= TSignUtil.buildAuthString( _config, request );
  http.addHeader( HttpConst.Header.Authorization, authString );
end;

{ TSigningKey }

class function TSignUtil.buildAuthString(
  const config: TAWSConfig;
  const request: NSMutableURLRequest ): NSString;
var
  hmacAlgorithm: NSString;
  credential: NSString;
  signedHeader: NSString;
  signature: NSString;
begin
  hmacAlgorithm:= buildHmacAlgorithmString( config );
  credential:= buildCredentialString( config, request );
  signedHeader:= TCanonicalRequestUtil.buildSignedHeadersString( request );
  signature:= buildSignature( config, request );
  Result:= NSString.stringWithFormat( NSSTR('%@ Credential=%@,SignedHeaders=%@,Signature=%@'),
    hmacAlgorithm,
    credential,
    signedHeader,
    signature );
end;

class function TSignUtil.buildSignature(
  const config: TAWSConfig;
  const request: NSMutableURLRequest ): NSString;
var
  signingkeyData: NSData;
  signingString: NSString;
begin
  signingkeyData:= buildSigningKey( config, request );
  signingString:= buildSigningString( config, request );
  Result:= THashUtil.HmacSha256AndHexStr( signingkeyData, signingString );
end;

class function TSignUtil.buildSigningString(
  const config: TAWSConfig;
  const request: NSMutableURLRequest ): NSString;
var
  canonicalRequestString: NSString;
  canonicalRequestSha256HexString: NSString;
begin
  canonicalRequestString:= TCanonicalRequestUtil.build(request);
  canonicalRequestSha256HexString:= THashUtil.sha256AndHexStr( canonicalRequestString );
  Result:= NSString.stringWithFormat( NSSTR('%@'#10'%@'#10'%@'#10'%@'),
    buildHmacAlgorithmString( config ),
    buildTimeStampString(request),
    buildScopeString(config,request),
    canonicalRequestSha256HexString );
end;

class function TSignUtil.buildSigningKey(
  const config: TAWSConfig;
  const request: NSMutableURLRequest ): NSData;
var
  dateString: NSString;
  keyString: NSString;
  key: NSData;
  dateKey: NSData;
  dateRegionKey: NSData;
  dateRegionServiceKey: NSData;
begin
  dateString:= self.buildDateYYYYMMDDString(request);
  keyString:= StringToNSString( config.credentialPrefixAndSecret );
  key:= keyString.dataUsingEncoding( NSUTF8StringEncoding );
  dateKey:= THashUtil.HmacSha256( key, dateString );
  dateRegionKey:= THashUtil.HmacSha256( dateKey, NSSTR(config.credentialRegion) );
  dateRegionServiceKey:= THashUtil.HmacSha256( dateRegionKey, NSSTR(config.credentialService) );
  Result:= THashUtil.HmacSha256( dateRegionServiceKey, NSSTR(config.credentialRequest) );
end;

class function TSignUtil.buildDateYYYYMMDDString( const request: NSMutableURLRequest ): NSString;
begin
  Result:= self.buildTimeStampString(request).substringToIndex( 8 );
end;

class function TSignUtil.buildTimeStampString( const request: NSMutableURLRequest ): NSString;
begin
  Result:= NSString( request.allHTTPHeaderFields.objectForKey(HttpConst.Header.Date) );
end;

class function TSignUtil.buildHmacAlgorithmString( const config: TAWSConfig ): NSString;
begin
  Result:= NSSTR( config.credentialVersionAlgorithm );
end;

class function TSignUtil.buildScopeString(
  const config: TAWSConfig;
  const request: NSMutableURLRequest ): NSString;
var
  signDate: NSString;
  signRegion: NSString;
  signService: NSString;
  signProduct: NSString;
begin
  signDate:= buildDateYYYYMMDDString( request );
  signService:= NSSTR( config.credentialService );
  signRegion:= NSSTR( config.credentialRegion );
  signProduct:= NSSTR( config.credentialRequest );
  Result:= NSString.stringWithFormat( NSSTR('%@/%@/%@/%@'),
    signDate,
    signRegion,
    signService,
    signProduct );
end;

class function TSignUtil.buildCredentialString(
  const config: TAWSConfig;
  const request: NSMutableURLRequest ): NSString;
begin
  Result:= NSString.stringWithFormat( NSSTR('%@/%@'),
    NSSTR( config.accessKeyID),
    TSignUtil.buildScopeString(config,request) );
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

