{
   Notes:
   1. contains only the most basic util
   2. make full use of Cocoa native facilities:
      json serialization/deserialization
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
  Classes, SysUtils,
  MacOSAll, CocoaAll, uMiniCocoa;

type

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
  public
    class function toQueryItems( const lclItems: TQueryItemsDictonary ): NSArray;
    class procedure openInSafari( const urlPart: String; lclItems: TQueryItemsDictonary );
    class function queryValue( components: NSURLComponents; const name: String ): String;
  end;

  { THashUtil }

  THashUtil = class
  public
    class function sha256AndBase64( const input: String ): String;
  end;

  { TStringUtil }

  TStringUtil = class
  public
    class function generateRandomString( const length: Integer ): String;
    class function widecharsToString(const p: pwidechar): String;
    class procedure stringToWidechars(const buffer: pwidechar; const s: String; const maxLength: Integer);
  end;

  { TJsonUtil }

  TJsonUtil = class
  public
    class function dumps(
      const Elements: Array of Const;
      const ensureAscii: Boolean = False;
      const options: Integer = NSJSONWritingWithoutEscapingSlashes ): String;
    class function parse( const jsonString: String ): NSDictionary; overload;
    class function parse( const jsonString: NSString ): NSDictionary; overload;
    class procedure setString( const json: NSMutableDictionary; const key: String; const value: String );
    class procedure setBoolean( const json: NSMutableDictionary; const key: String; const value: Boolean );
    class procedure setInteger( const json: NSMutableDictionary; const key: String; const value: Integer );
    class procedure setDateTime( const json: NSMutableDictionary; const key: String; const value: TDateTime );
    class procedure setArray( const json: NSMutableDictionary; const key: String; const value: NSArray );
    class procedure setDictionary( const json: NSMutableDictionary; const key: String; const value: NSDictionary );
    class function getString( const json: NSDictionary; const key: String ): String;
    class function getBoolean( const json: NSDictionary; const key: String ): Boolean;
    class function getInteger( const json: NSDictionary; const key: String ): Integer;
    class function getDateTime( const json: NSDictionary; const key: String ): TDateTime;
    class function getArray( const json: NSDictionary; const key: String ): NSArray;
    class function getDictionary( const json: NSDictionary; const key: String ): NSDictionary;
  end;

  { TSecUtil }

  TSecUtil = class
  public
    class procedure saveValue( const service: String; const account: String; const value: String ); overload;
    class function getValue( const service: String; const account: String ): String;
  end;

  { TFileUtil }

  TFileUtil = class
  public
    class function exists( const path: String ): Boolean;
    class function filesize( const path: String ): Integer;
    class function parentPath( const path: String ): String;
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

const
  CC_SHA256_DIGEST_LENGTH = 32;

  function CC_SHA256(data: Pointer; len: LongWord; md: Pointer): Pointer; cdecl; external;

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

{ TJsonUtil }

class function TJsonUtil.dumps(
  const Elements: array of const;
  const ensureAscii: Boolean = False;
  const options: Integer = NSJSONWritingWithoutEscapingSlashes ): String;
var
  i: integer;
  jsonData: NSData;
  jsonString: NSString;
  json: NSMutableDictionary;
  key: NSString;
  value: NSObject;
  error: NSError;

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

  function escapeUnicode( oldString: NSString ): NSString;
  var
    newString: NSMutableString;
    c: unichar;
    i: Integer;
  begin
    newString:= NSMutableString.new;
    for i:=0 to oldString.length-1 do begin
      c:= oldString.characterAtIndex( i );
      if c > $7F then
        newString.appendFormat( NSSTR('\u%04x'), c )
      else
        newString.appendFormat( NSSTR('%C'), c );
    end;
    Result:= newString;
    newString.autorelease;
  end;

begin
  Result:= EmptyStr;

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

  error:= nil;
  jsonData:= NSJSONSerialization.dataWithJSONObject_options_error( json, options, @error );
  if error <> nil then
    Result:= EmptyStr
  else begin
    jsonString:= NSString.alloc.initWithData_encoding( jsonData, NSUTF8StringEncoding );
    if ensureAscii then
      jsonString:= escapeUnicode( jsonString );
    Result:= jsonString.UTF8String;
  end;

  json.release;
end;

class function TJsonUtil.parse(const jsonString: String): NSDictionary;
begin
  Result:= self.parse( StringToNSString(jsonString) );
end;

class function TJsonUtil.parse(const jsonString: NSString): NSDictionary;
var
  jsonData: NSData;
  error: NSError;
begin
  jsonData:= jsonString.dataUsingEncoding( NSUTF8StringEncoding );
  error:= nil;
  Result:= NSJSONSerialization.JSONObjectWithData_options_error( jsonData, 0, @error );
  if error <> nil then
    raise EArgumentException.Create( 'error in TJsonUtil.parse(): ' + error.localizedDescription.UTF8String );
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

class function TJsonUtil.getString(const json: NSDictionary; const key: String ): String;
begin
  Result:= NSString( json.objectForKey( StringToNSString(key) ) ).UTF8String;
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

{ TSecUtil }

class procedure TSecUtil.saveValue(
  const service: String;
  const account: String;
  const value: String );
var
  appID: NSString;
  data: NSData;
  attributes: NSMutableDictionary;
  status: OSStatus;
begin
  appID:= NSBundle.mainBundle.bundleIdentifier;
  data:= StringToNSString(value).dataUsingEncoding(NSUTF8StringEncoding);
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

initialization
  consoleLogger:= TConsoleLogger.Create;
  TLogUtil.setLogProc( @consoleLogger.logProc );

finalization
  FreeAndNil( consoleLogger );

end.

