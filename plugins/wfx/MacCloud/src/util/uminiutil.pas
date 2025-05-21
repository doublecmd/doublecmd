{
   Notes:
   1. contains only the most basic util
   2. make full use of Cocoa native facilities:
      json serialization/deserialization
      xml
      key chains
      hash
      string conversion
      file tools
   3. the purpose is to avoid dependencies on other libraries
}

unit uMiniUtil;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, DateUtils,
  MacOSAll, CocoaAll, uMiniCocoa;

type

  { TMiniContentRange }

  TMiniContentRange = class
  private
    _first: Integer;
    _last: Integer;
    _total: Integer;
  public
    constructor Create( const first: Integer; const last: Integer; const total: Integer );
    function isNeeded: Boolean;
    function ToString: ansistring; override;
    function length: Integer;
    function toNSRange: NSRange;
    property first: Integer read _first;
    property last: Integer read _last;
  end;

  { TFakeStringDictonary }

  TFakeStringDictonary = class
  private
    _keys: TStringList;
    _values: TStringList;
  private
    function getKey( const index: Integer ): String;
    function getValue( const index: Integer ): String;
    function getCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add( const key: String; const value: String );
    property Count: Integer read getCount;
    property keys[ const index: Integer ]: String read getKey;
    property values[ const index: Integer ]: String read getValue;
  end;

  TQueryItemsDictonary = TFakeStringDictonary;

  { THttpClientUtil }

  THttpClientUtil = class
  private const
    AWS_URI_ENCODE_ALLOW_CHAR = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-._~';
  private class var
    AWS_URI_ENCODE_CHARACTER_SET: NSCharacterSet;
    PATH_ENCODE_CHARACTER_SET: NSCharacterSet;
  public
    class function toQueryItems( const lclItems: TQueryItemsDictonary ): NSArray;
    class function toNSString( const lclItems: TQueryItemsDictonary ): NSString;
    class procedure openInSafari( const urlPart: String; lclItems: TQueryItemsDictonary );
    class function queryValue( components: NSURLComponents; const name: String ): String;
    class function urlEncode( const urlString: String ): String;
    class function awsUriEncode( const uriString: NSString ): NSString;
  end;

  { THashUtil }

  THashUtil = class
  public
    class function sha256AndBase64( const input: String ): String;
    class function sha256AndHexStr( const input: NSString ): NSString;
    class function sha1AndBase64( const inputStream: NSinputStream ): String;
    class function HmacSha256( const keyData: NSData; const text: NSString ): NSData;
    class function HmacSha256AndHexStr( const keyData: NSData; const text: NSString ): NSString;
  end;

  { TStringUtil }

  TStringUtil = class
  public
    class function generateRandomString( const length: Integer ): String;
    class function widecharsToString(const p: pwidechar): String;
    class procedure stringToWidechars(const buffer: pwidechar; const s: String; const maxLength: Integer);
    class function hexstr( const buf: pbyte; const length: Integer ): NSString;
    class function hexstr( const data: NSData ): NSString;
    class function removeSpace( const s: NSString ): String;
  end;

  { TJsonUtil }

  TJsonUtil = class
  public
    class function dumps(
      const json: NSDictionary;
      const ensureAscii: Boolean = False;
      const options: Integer = NSJSONWritingWithoutEscapingSlashes ): NSString;
    class function dumps(
      const Elements: Array of Const;
      const ensureAscii: Boolean = False;
      const options: Integer = NSJSONWritingWithoutEscapingSlashes ): NSString;
    class function parse( const jsonString: String ): NSDictionary; overload;
    class function parse( const jsonString: NSString ): NSDictionary; overload;
    class procedure setString( const json: NSMutableDictionary; const key: String; const value: String );
    class procedure setBoolean( const json: NSMutableDictionary; const key: String; const value: Boolean );
    class procedure setInteger( const json: NSMutableDictionary; const key: String; const value: Integer );
    class procedure setDateTime( const json: NSMutableDictionary; const key: String; const value: TDateTime );
    class procedure setArray( const json: NSMutableDictionary; const key: String; const value: NSArray );
    class procedure setDictionary( const json: NSMutableDictionary; const key: String; const value: NSDictionary );
    class function getNSString( const json: NSDictionary; const key: String ): NSString;
    class function getString( const json: NSDictionary; const key: String ): String;
    class function getBoolean( const json: NSDictionary; const key: String ): Boolean;
    class function getInteger( const json: NSDictionary; const key: String ): Integer;
    class function getDateTime( const json: NSDictionary; const key: String ): TDateTime;
    class function getArray( const json: NSDictionary; const key: String ): NSArray;
    class function getDictionary( const json: NSDictionary; const key: String ): NSDictionary;
  end;

  { TXMLUtil }

  TXmlUtil = class
  public
    class function parse( const xmlString: String ): NSXMLElement;
    class function parse( const xmlString: NSString ): NSXMLElement;
    class function getElement( const xml: NSXMLElement; const name: String ): NSXMLElement;
    class function getNSString( const xml: NSXMLElement; const name: String ): NSString;
    class function getString( const xml: NSXMLElement; const name: String ): String;
    class function getInteger( const xml: NSXMLElement; const name: String ): Integer;
  end;

  { TSecUtil }

  TSecUtil = class
  public
    class procedure saveValue( const service: String; const account: String; const value: NSString ); overload;
    class function getValue( const service: String; const account: String ): String;
  end;

  { TFileUtil }

  TFileUtil = class
  public
    class function exists( const path: String ): Boolean;
    class function filesize( const path: String ): Integer;
    class function parentPath( const path: String ): String;
    class function filename( const path: String ): String;
    class function extension( const path: String ): String;
    class function pathWithLanguageID( const path: String; const languageID: String ): String;
    class function allContent( const path: String ): NSData;
    class function contentAsUTF8String( const path: String ): NSString;
    class function sha1( const path: String ): String; overload;
    class function sha1( const path: String; const range:TMiniContentRange ): String; overload;
  end;

  { TDateTimeUtil }

  TDateTimeUtil = class
  public
    class function dateTimeToISO8601( const date: NSDate ): NSString;
    class function nowToISO8601: NSString;
  end;

  TLogProc = procedure ( const MsgType: Integer; const message: String ) of object;

  { TLogUtil }

  TLogUtil = class
  private class var
    _logProc: TLogProc;
  public
    class procedure setLogProc( const logProc: TLogProc );
    class procedure log( const MsgType: Integer; const message: String );
    class procedure logInformation( const message: String );
    class procedure logError( const message: String );
  end;

implementation

type
  CC_LONG = uint32;

const
  CC_SHA1_BLOCK_BYTES = 64;
  CC_SHA1_BLOCK_LONG = 16;

  CC_SHA1_DIGEST_LENGTH = 20;
  CC_SHA256_DIGEST_LENGTH = 32;

type
  CC_SHA1_CTX = record
    h0,h1,h2,h3,h4: CC_LONG;
    Nl,Nh: CC_LONG;
    data: Array[1..CC_SHA1_BLOCK_LONG] of CC_LONG;
    num: Integer;
  end;

  CC_SHA1_CTX_PTR = ^CC_SHA1_CTX;

  CCHmacAlgorithm = NSInteger;

  function CC_SHA256(data: Pointer; len: LongWord; md: Pointer): Pointer; cdecl; external;
  procedure CC_SHA1_Init(c: CC_SHA1_CTX_PTR); cdecl; external;
  procedure CC_SHA1_Update(c: CC_SHA1_CTX_PTR; const data: Pointer; len: CC_LONG ); cdecl; external;
  procedure CC_SHA1_Final(md: pchar; c: CC_SHA1_CTX_PTR); cdecl; external;

  procedure CCHmac( algorithm: CCHmacAlgorithm; key: Pointer; keyLength: Integer;
    data: Pointer; dataLength: Integer; macOut: Pointer ); cdecl; external;

const
  kCCHmacAlgSHA256 = 2;

type
  TConsoleLogger = class
    procedure logProc( const MsgType: Integer; const message: String );
  end;

var
  consoleLogger: TConsoleLogger;

{ THashUtil }

class function THashUtil.sha256AndBase64(const input: String): String;
var
  sha256Buf: array [1..CC_SHA256_DIGEST_LENGTH] of Byte;
  sha256Data: NSData;
  base64String: NSString;
begin
  CC_SHA256( @input[1], input.Length, @sha256Buf );
  sha256Data:= NSData.dataWithBytes_length( @sha256Buf, CC_SHA256_DIGEST_LENGTH );
  base64String:= sha256Data.base64EncodedStringWithOptions( 0 );
  base64String:= base64String.stringByReplacingOccurrencesOfString_withString( NSSTR('/'), NSSTR('_') );
  base64String:= base64String.stringByReplacingOccurrencesOfString_withString( NSSTR('+'), NSSTR('-') );
  base64String:= base64String.stringByReplacingOccurrencesOfString_withString( NSSTR('='), NSString.string_ );
  Result:= base64String.UTF8String;
end;

class function THashUtil.sha256AndHexStr(const input: NSString): NSString;
var
  sha256Buf: array [1..CC_SHA256_DIGEST_LENGTH] of Byte;
begin
  CC_SHA256( input.cString, input.cStringLength, @sha256Buf );
  TStringUtil.hexstr( @sha256Buf, CC_SHA256_DIGEST_LENGTH );
end;

class function THashUtil.sha1AndBase64(const inputStream: NSinputStream): String;
var
  ctx: CC_SHA1_CTX;
  sha1Buf: array [1..CC_SHA1_DIGEST_LENGTH] of Byte;
  sha1Data: NSData;
  base64String: NSString;

  dataBuf: array [1..1024*1024] of Byte;
  length: Integer;
begin
  CC_SHA1_Init( @ctx );
  while inputStream.hasBytesAvailable do begin
    length:= inputStream.read_maxLength( @dataBuf, sizeof(dataBuf) );
    CC_SHA1_Update( @ctx, @dataBuf, length );
  end;
  CC_SHA1_Final( @sha1Buf, @ctx);

  sha1Data:= NSData.dataWithBytes_length( @sha1Buf, CC_SHA1_DIGEST_LENGTH );
  base64String:= sha1Data.base64EncodedStringWithOptions( 0 );
  Result:= base64String.UTF8String;
end;

class function THashUtil.HmacSha256( const keyData: NSData; const text: NSString ): NSData;
var
  hmacBuf: array [1..CC_SHA256_DIGEST_LENGTH] of Byte;
  textData: NSData;
begin
  textData:= text.dataUsingEncoding( NSUTF8StringEncoding );
  CCHmac( kCCHmacAlgSHA256,
    keyData.bytes, keyData.length,
    textData.bytes, textData.length,
    @hmacBuf );
  Result:= NSData.dataWithBytes_length( @hmacBuf, CC_SHA256_DIGEST_LENGTH );
end;

class function THashUtil.HmacSha256AndHexStr(
  const keyData: NSData; const text: NSString): NSString;
var
  hmacData: NSData;
begin
  hmacData:= THashUtil.HmacSha256( keyData, text );
  Result:= TStringUtil.hexstr( hmacData );
end;

{ TStringUtil }

class function TStringUtil.generateRandomString(const length: Integer): String;
const
  samples = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
var
  randomString: TAnsiStringBuilder;
  randomValue: Integer;
  i: Integer;
begin
  randomString:= TAnsiStringBuilder.Create( length );
  for i:=0 to length-1 do begin
    randomValue:= random(samples.Length) + 1;
    randomString.Append( samples[randomValue] );
  end;
  Result:= randomString.ToString;
  randomString.Free;
end;

class function TStringUtil.widecharsToString(const p: pwidechar): String;
var
  str: NSString;
begin
  str:= NSString.stringWithCharacters_length( @p[0], StrLen(p) );
  Result:= str.UTF8String;
end;

class procedure TStringUtil.stringToWidechars(
  const buffer: pwidechar;
  const s: String;
  const maxLength: Integer );
var
  str: NSString;
  range: NSRange;
  usedLength: Integer;
begin
  str:= StringToNSString( s );
  range.location:= 0;
  range.length:= str.length;
  str.getBytes_maxLength_usedLength_encoding_options_range_remainingRange(
    pchar(buffer),
    maxLength - 2,
    @usedLength,
    NSUnicodeStringEncoding,
    0,
    range,
    nil );
  buffer[usedLength div 2]:= #0;
end;

class function TStringUtil.hexstr( const buf: pbyte; const length: Integer ): NSString;
var
  hexString: NSMutableString;
  i: Integer;
begin
  hexString:= NSMutableString.new.autorelease;
  for i:=0 to length-1 do begin
    hexString.appendFormat( NSSTR('%02x'), buf[i] );
  end;
  Result:= hexString;
end;

class function TStringUtil.hexstr(const data: NSData): NSString;
begin
  Result:= TStringUtil.hexstr( data.bytes, data.length );
end;

class function TStringUtil.removeSpace(const s: NSString): String;
begin
  Result:= s.stringByReplacingOccurrencesOfString_withString( NSSTR(' '), NSString.string_ ).UTF8String;
end;

{ TJsonUtil }

class function TJsonUtil.dumps(
  const json: NSDictionary;
  const ensureAscii: Boolean = False;
  const options: Integer = NSJSONWritingWithoutEscapingSlashes ): NSString;

  function escapeUnicode( oldString: NSString ): NSString;
  var
    newString: NSMutableString;
    c: unichar;
    i: Integer;
  begin
    newString:= NSMutableString.new;
    newString.autorelease;
    for i:=0 to oldString.length-1 do begin
      c:= oldString.characterAtIndex( i );
      if c > $7F then
        newString.appendFormat( NSSTR('\u%04x'), c )
      else
        newString.appendFormat( NSSTR('%C'), c );
    end;
    Result:= newString;
  end;

var
  jsonData: NSData;
  jsonString: NSString;
  error: NSError;
begin
  error:= nil;
  jsonData:= NSJSONSerialization.dataWithJSONObject_options_error( json, options, @error );
  if error <> nil then
    Result:= nil
  else begin
    jsonString:= NSString.alloc.initWithData_encoding( jsonData, NSUTF8StringEncoding );
    jsonString.autorelease;
    if ensureAscii then
      jsonString:= escapeUnicode( jsonString );
    Result:= jsonString;
  end;
end;

class function TJsonUtil.dumps(
  const Elements: array of const;
  const ensureAscii: Boolean = False;
  const options: Integer = NSJSONWritingWithoutEscapingSlashes ): NSString;
var
  i: integer;
  json: NSMutableDictionary;
  key: NSString;
  value: NSObject;

  function varToNSObject( const Element : TVarRec ): id;
  begin
    Result:= nil;
    With Element do
      case VType of
        vtString     : Result:= StringToNSString( VString^ );
        vtAnsiString : begin
          if VPChar = nil then
            Result:= NSString.string_
          else
            Result:= NSString.stringWithUTF8String( VPChar );
        end;
        vtBoolean    : Result:= NSNumber.numberWithBool( VBoolean );
        vtInteger    : Result:= NSNumber.numberWithLongLong( VInteger );
        vtInt64      : Result:= NSNumber.numberWithLongLong( VInt64^ );
        vtQWord      : Result:= NSNumber.numberWithLongLong( VQWord^ );
        vtPointer    : Result:= NSObject( VPointer );
      end;
  end;

begin
  Result:= nil;

  if ((High(Elements)-Low(Elements)) mod 2)=0 then
    Exit;

  json:= NSMutableDictionary.new;
  i:= Low(Elements);
  while i<= High(Elements) do begin
    key:= varToNSObject( Elements[i] );
    inc( i );
    value:= varToNSObject( Elements[i] );
    inc( i );
    json.setValue_forKey( value, key );
  end;

  Result:= self.dumps( json, ensureAscii, options );
  json.release;
end;

class function TJsonUtil.parse(const jsonString: String): NSDictionary;
begin
  Result:= self.parse( StringToNSString(jsonString) );
end;

class function TJsonUtil.parse(const jsonString: NSString): NSDictionary;
var
  jsonData: NSData;
  error: NSError = nil;
begin
  jsonData:= jsonString.dataUsingEncoding( NSUTF8StringEncoding );
  Result:= NSJSONSerialization.JSONObjectWithData_options_error( jsonData, 0, @error );
  if error <> nil then
    raise EArgumentException.Create( 'error in TJsonUtil.parse(): ' +
      error.localizedDescription.UTF8String + ': ' + jsonString.UTF8String );
end;

class procedure TJsonUtil.setString(const json: NSMutableDictionary; const key: String;
  const value: String);
begin
  json.setObject_forKey( StringToNSString(value), StringToNSString(key) );
end;

class procedure TJsonUtil.setBoolean(const json: NSMutableDictionary;
  const key: String; const value: Boolean);
begin
  json.setObject_forKey( NSNumber.numberWithBool(value), StringToNSString(key) );
end;

class procedure TJsonUtil.setInteger(const json: NSMutableDictionary;
  const key: String; const value: Integer);
begin
  json.setObject_forKey( NSNumber.numberWithLongLong(value), StringToNSString(key) );
end;

class procedure TJsonUtil.setDateTime(const json: NSMutableDictionary;
  const key: String; const value: TDateTime);
begin
  json.setObject_forKey( NSNumber.numberWithDouble(value), StringToNSString(key) );
end;

class procedure TJsonUtil.setArray(const json: NSMutableDictionary; const key: String;
  const value: NSArray);
begin
  json.setObject_forKey( value, StringToNSString(key) );
end;

class procedure TJsonUtil.setDictionary(const json: NSMutableDictionary;
  const key: String; const value: NSDictionary);
begin
  json.setObject_forKey( value , StringToNSString(key) );
end;

class function TJsonUtil.getNSString( const json: NSDictionary; const key: String ): NSString;
begin
  Result:= NSString( json.objectForKey( StringToNSString(key) ) );
end;

class function TJsonUtil.getString( const json: NSDictionary; const key: String ): String;
begin
  Result:= TJsonUtil.getNSString(json,key).UTF8String;
end;

class function TJsonUtil.getBoolean(const json: NSDictionary; const key: String
  ): Boolean;
begin
  Result:= NSNumber( json.objectForKey( StringToNSString(key) ) ).boolValue;
end;

class function TJsonUtil.getInteger(const json: NSDictionary; const key: String
  ): Integer;
begin
  Result:= NSNumber( json.objectForKey( StringToNSString(key) ) ).longLongValue;
end;

class function TJsonUtil.getDateTime(const json: NSDictionary; const key: String
  ): TDateTime;
begin
  Result:= NSNumber( json.objectForKey( StringToNSString(key) ) ).doubleValue;
end;

class function TJsonUtil.getArray(const json: NSDictionary; const key: String
  ): NSArray;
begin
  Result:= NSArray( json.objectForKey( StringToNSString(key) ) );
end;

class function TJsonUtil.getDictionary(const json: NSDictionary;
  const key: String): NSDictionary;
begin
  Result:= NSDictionary( json.objectForKey( StringToNSString(key) ) );
end;

{ TXmlUtil }

class function TXmlUtil.parse(const xmlString: String): NSXMLElement;
begin
  Result:= self.parse( StringToNSString(xmlString) );
end;

class function TXmlUtil.parse(const xmlString: NSString): NSXMLElement;
var
  doc: NSXMLDocument = nil;
  error: NSError = nil;
begin
  doc:= NSXMLDocument.alloc.initWithXMLString_options_error(
    xmlString,
    0,
    @error );
  if error <> nil then
    raise EArgumentException.Create( 'error in TXmlUtil.parse(): ' +
      error.localizedDescription.UTF8String + ': ' + xmlString.UTF8String );
  Result:= doc.rootElement;
  doc.autorelease;
end;

class function TXmlUtil.getElement( const xml: NSXMLElement; const name: String ): NSXMLElement;
var
  elements: NSArray;
begin
  elements:= xml.elementsForName( StringToNSString(name) );
  Result:= NSXMLElement( elements.firstObject );
end;

class function TXmlUtil.getNSString( const xml: NSXMLElement; const name: String ): NSString;
var
  elements: NSArray;
begin
  elements:= xml.elementsForName( StringToNSString(name) );
  Result:= elements.firstObject.stringValue;
end;

class function TXmlUtil.getString( const xml: NSXMLElement; const name: String ): String;
begin
  Result:= TXmlUtil.getNSString(xml,name).utf8String;
end;

class function TXmlUtil.getInteger(const xml: NSXMLElement; const name: String ): Integer;
begin
  Result:= TXmlUtil.getNSString(xml,name).longLongValue;
end;

{ TSecUtil }

class procedure TSecUtil.saveValue(
  const service: String;
  const account: String;
  const value: NSString );
var
  appID: NSString;
  data: NSData;
  attributes: NSMutableDictionary;
  status: OSStatus;
begin
  appID:= NSBundle.mainBundle.bundleIdentifier;
  data:= value.dataUsingEncoding(NSUTF8StringEncoding);
  attributes:= NSMutableDictionary.new;
  attributes.setObject_forKey( kSecClassGenericPassword , kSecClass );
  attributes.setObject_forKey( appID, kSecAttrLabel );
  attributes.setObject_forKey( StringToNSString(service) , kSecAttrService );
  attributes.setObject_forKey( StringToNSString(account) , kSecAttrAccount );
  attributes.setObject_forKey( data , kSecValueData );
  SecItemDelete( attributes );
  status:= SecItemAdd( attributes, nil );
  attributes.release;
  if status <> errSecSuccess then
    TLogUtil.logError( 'SecItemAdd() error in TSecUtil.saveValue(): ' + IntToStr(status) );
end;

class function TSecUtil.getValue(
  const service: String;
  const account: String ): String;
var
  attributes: NSMutableDictionary;
  status: OSStatus;
  item: NSDictionary;
  data: NSData;
  value: NSString;
begin
  attributes:= NSMutableDictionary.new;
  attributes.setObject_forKey( kSecClassGenericPassword , kSecClass );
  attributes.setObject_forKey( StringToNSString(service), kSecAttrService );
  attributes.setObject_forKey( StringToNSString(account), kSecAttrAccount );
  attributes.setObject_forKey( kSecMatchLimitOne, kSecMatchLimit );
  attributes.setObject_forKey( NSNumber.numberWithBool(True), kSecReturnAttributes );
  attributes.setObject_forKey( NSNumber.numberWithBool(True), kSecReturnData );
  status:= SecItemCopyMatching( attributes, @item );
  if status <> errSecSuccess then
    TLogUtil.logError( 'SecItemCopyMatching() error in TSecUtil.getValue(): ' + IntToStr(status) );
  attributes.release;
  data:= NSData( item.objectForKey(kSecValueData) );
  value:= NSString.alloc.initWithData_encoding(data, NSUTF8StringEncoding);
  Result:= value.UTF8String;
  value.release;
end;

{ TConsoleLogger }

procedure TConsoleLogger.logProc( const MsgType: Integer; const message: String );
begin
  Writeln( 'DefaultLogger: ', message );
end;

{ TFileUtil }

class function TFileUtil.exists(const path: String): Boolean;
var
  cocoaPath: NSString;
begin
  cocoaPath:= StringToNSString( path );
  Result:= NSFileManager.defaultManager.fileExistsAtPath( cocoaPath );
end;

class function TFileUtil.filesize(const path: String): Integer;
var
  fileAttributes: NSDictionary;
  filesizeNumber: NSNumber;
begin
  fileAttributes:= NSFileManager.defaultManager.attributesOfItemAtPath_error(
    StringToNSString(path),
    nil );
  filesizeNumber:= fileAttributes.objectForKey( NSFileSize );
  Result:= filesizeNumber.longLongValue;
end;

class function TFileUtil.parentPath(const path: String): String;
var
  nsPath: NSString;
begin
  nsPath:= StringToNSString( path );
  Result:= nsPath.stringByDeletingLastPathComponent.UTF8String;
end;

class function TFileUtil.filename(const path: String): String;
var
  nsPath: NSString;
begin
  nsPath:= StringToNSString( path );
  Result:= nsPath.lastPathComponent.UTF8String;
end;

class function TFileUtil.extension(const path: String): String;
begin
  Result:= StringToNSString(path).pathExtension.UTF8String;
end;

class function TFileUtil.pathWithLanguageID( const path: String; const languageID: String ): String;
var
  basePath: NSString;
  languagePath: String;
begin
  basePath:= StringToNSString( path );
  languagePath:= basePath.stringByDeletingPathExtension.
           stringByAppendingString( StringToNSString(languageID) ).
           stringByAppendingPathExtension( basePath.pathExtension ).UTF8String;
  if TFileUtil.exists( languagePath ) then
    Result:= languagePath
  else
    Result:= path;
end;

class function TFileUtil.allContent(const path: String): NSData;
begin
  Result:= NSData.dataWithContentsOfFile( StringToNSString(path) );
end;

class function TFileUtil.contentAsUTF8String(const path: String): NSString;
var
  data: NSData;
begin
  data:= TFileUtil.allContent( path );
  Result:= NSString.alloc.initWithData_encoding( data, NSUTF8StringEncoding );
  Result.autorelease;
end;

class function TFileUtil.sha1(const path: String): String;
var
  range: TMiniContentRange;
  size: Integer;
begin
  size:= TFileUtil.filesize( path );
  range:= TMiniContentRange.Create( 0, size-1, size );
  Result:= self.sha1( path, range );
  range.Free;
end;

class function TFileUtil.sha1(const path: String; const range: TMiniContentRange): String;
var
  nsPath: NSString;
  inputStream: NSInputStream;
begin
  nsPath:= StringToNSString(path);
  if range.isNeeded then
    inputStream:= NSFileRangeInputStream.alloc.initWithFileAtPath_Range( nsPath, range.toNSRange )
  else
    inputStream:= NSInputStream.inputStreamWithFileAtPath( nsPath );
  inputStream.open;
  Result:= THashUtil.sha1AndBase64( inputStream );
  inputStream.close;
end;

{ TDateTimeUtil }

class function TDateTimeUtil.dateTimeToISO8601( const date: NSDate ): NSString;
var
  formater: NSDateFormatter;
begin
  formater:= NSDateFormatter.new;
  formater.setDateFormat( NSSTR('yyyyMMdd''T''HHmmss''Z''') );
  formater.setTimeZone( NSTimeZone.timeZoneForSecondsFromGMT(0) );
  Result:= formater.stringFromDate( date );
  formater.release;
end;

class function TDateTimeUtil.nowToISO8601: NSString;
var
  nowDate: NSDate;
begin
  nowDate:= NSDate.new;
  Result:= TDateTimeUtil.dateTimeToISO8601( nowDate );
  nowDate.release;
end;

{ TLogUtil }

class procedure TLogUtil.setLogProc( const logProc: TLogProc );
begin
  _logProc:= logProc;
end;

class procedure TLogUtil.log( const MsgType: Integer; const message: String );
begin
  _logProc( MsgType, message );
end;

class procedure TLogUtil.logInformation( const message: String );
begin
  log( 3, message );
end;

class procedure TLogUtil.logError(const message: String);
begin
  log( 6, message );
end;

{ TFakeStringDictonary }

constructor TFakeStringDictonary.Create;
begin
  _keys:= TStringList.Create;
  _values:= TStringList.Create;
end;

destructor TFakeStringDictonary.Destroy;
begin
  FreeAndNil( _keys );
  FreeAndNil( _values );
end;

procedure TFakeStringDictonary.Add(const key: String; const value: String);
begin
  _keys.Add( key );
  _values.Add( value );
end;

function TFakeStringDictonary.getKey( const index: Integer ): String;
begin
  Result:= _keys[index];
end;

function TFakeStringDictonary.getValue( const index: Integer ): String;
begin
  Result:= _values[index];
end;

function TFakeStringDictonary.getCount: Integer;
begin
  Result:= _keys.Count;
end;

{ THttpClientUtil }

class function THttpClientUtil.toQueryItems(
  const lclItems: TQueryItemsDictonary ): NSArray;
var
  cocoaItem: NSURLQueryItem;
  cocoaItems: NSMutableArray;
  i: Integer;
begin
  cocoaItems:= NSMutableArray.arrayWithCapacity( lclItems.Count );
  for i:=0 to lclItems.Count-1 do begin
    cocoaItem:= NSURLQueryItem.queryItemWithName_value(
      StringToNSString( lclItems.keys[i] ),
      StringToNSString( lclItems.values[i] ) );
    cocoaItems.addObject( cocoaItem );
  end;
  Result:= cocoaItems;
end;

class function THttpClientUtil.toNSString(const lclItems: TQueryItemsDictonary ): NSString;
var
  components: NSURLComponents;
  cocoaItems: NSArray;
begin
  components:= NSURLComponents.new;
  cocoaItems:= THttpClientUtil.toQueryItems( lclItems );
  components.setQueryItems( cocoaItems );
  Result:= components.query;
  components.release;
end;

class procedure THttpClientUtil.openInSafari(
  const urlPart: String; lclItems: TQueryItemsDictonary );
var
  urlComponents: NSURLComponents;
begin
  urlComponents:= NSURLComponents.componentsWithString( StringToNSString(urlPart) );
  urlComponents.setQueryItems( toQueryItems(lclItems) );
  NSWorkspace.sharedWorkspace.openURLs_withAppBundleIdentifier_options_additionalEventParamDescriptor_launchIdentifiers(
    NSArray.arrayWithObject( urlComponents.url ),
    NSSTR('com.apple.Safari'),
    0,
    nil,
    nil );
  FreeAndNil( lclItems );
end;

class function THttpClientUtil.queryValue(components: NSURLComponents;
  const name: String): String;
var
  cocoaName: NSString;
  queryItem: NSURLQueryItem;
begin
  cocoaName:= StringToNSString( name );
  for queryItem in components.queryItems do begin
    if queryItem.name.isEqualToString( cocoaName ) then begin
      Result:= queryItem.value.UTF8String;
      Exit;
    end;
  end;
  Result:= EmptyStr;
end;

class function THttpClientUtil.urlEncode(const urlString: String): String;
var
  nsUrlString: NSString;
begin
  nsUrlString:= StringToNSString(urlString).stringByAddingPercentEncodingWithAllowedCharacters(
    PATH_ENCODE_CHARACTER_SET );
  Result:= nsUrlString.UTF8String;
end;

class function THttpClientUtil.awsUriEncode( const uriString: NSString ): NSString;
begin
  Result:= uriString.stringByAddingPercentEncodingWithAllowedCharacters( AWS_URI_ENCODE_CHARACTER_SET );
end;

{ TMiniContentRange }

constructor TMiniContentRange.Create(const first: Integer;
  const last: Integer; const total: Integer);
begin
  _first:= first;
  _last:= last;
  _total:= total;
end;

function TMiniContentRange.isNeeded: Boolean;
begin
  Result:= (_first>0) OR (_last<_total-1);
end;

function TMiniContentRange.ToString: ansistring;
var
  range: String;
begin
  range:= IntToStr(_first) + '-' + IntToStr(_last);
  Result:= 'bytes ' + range + '/' + IntToStr(_total);
end;

function TMiniContentRange.length: Integer;
begin
  Result:= _last - _first + 1;
end;

function TMiniContentRange.toNSRange: NSRange;
begin
  Result.location:= _first;
  Result.length:= self.length;
end;

function createPathEncodeCharacterSet: NSCharacterSet;
var
  cs: NSMutableCharacterSet;
begin
  cs:= NSMutableCharacterSet.new;
  cs.formUnionWithCharacterSet( NSCharacterSet.URLPathAllowedCharacterSet );
  cs.removeCharactersInString( NSSTR('+') );
  Result:= cs;
end;

initialization
  consoleLogger:= TConsoleLogger.Create;
  TLogUtil.setLogProc( @consoleLogger.logProc );

  THttpClientUtil.AWS_URI_ENCODE_CHARACTER_SET:= NSCharacterSet.characterSetWithCharactersInString(
    NSSTR(THttpClientUtil.AWS_URI_ENCODE_ALLOW_CHAR) ).retain;

  THttpClientUtil.PATH_ENCODE_CHARACTER_SET:= createPathEncodeCharacterSet;

finalization
  FreeAndNil( consoleLogger );

end.

