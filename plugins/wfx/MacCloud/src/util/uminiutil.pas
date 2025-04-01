{
   Notes:
   1. contains only the most basic util
   2. make full use of Cocoa native facilities:
      json serialization/deserialization
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
  Classes, SysUtils, Generics.Collections,
  CocoaAll, uMiniCocoa;

type

  TQueryItemsDictonary = specialize TDictionary<string, string>;

  { THttpUtil }

  THttpUtil = class
  public
    class function toQueryItems( const lclItems: TQueryItemsDictonary ): NSArray;
    class procedure openInSafari( const urlPart: String; lclItems: TQueryItemsDictonary );
    class function queryValue( components: NSURLComponents; const name: String ): String;
  end;

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
    class function dumps( const Elements: Array of Const; const ensureAscii: Boolean = False ): String;
    class function parse( const jsonString: String ): NSDictionary;
    class procedure setString( const json: NSMutableDictionary; const key: String; const value: String );
    class procedure setBoolean( const json: NSMutableDictionary; const key: String; const value: Boolean );
    class procedure setInteger( const json: NSMutableDictionary; const key: String; const value: Integer );
    class procedure setArray( const json: NSMutableDictionary; const key: String; const value: NSArray );
    class function getString( const json: NSDictionary; const key: String ): String;
    class function getBoolean( const json: NSDictionary; const key: String ): Boolean;
    class function getInteger( const json: NSDictionary; const key: String ): Integer;
    class function getArray( const json: NSDictionary; const key: String ): NSArray;
  end;

  { TFileUtil }

  TFileUtil = class
  public
    class function exists( const path: String ): Boolean;
    class function filesize( const path: String ): Integer;
  end;

  TLogProc = procedure ( const MsgType: Integer; const message: String );

  { TLogUtil }

  TLogUtil = class
  private class var
    _logProc: TLogProc;
    _pluginNumber: Integer;
  public
    class procedure setLogProc( const logProc: TLogProc );
    class procedure log( const MsgType: Integer; const message: String );
    class procedure logInformation( const message: String );
  end;

implementation

const
  CC_SHA256_DIGEST_LENGTH = 32;

  function CC_SHA256(data: Pointer; len: LongWord; md: Pointer): Pointer; cdecl; external;


{ THttpUtil }

class function THttpUtil.toQueryItems( const lclItems: TQueryItemsDictonary ): NSArray;
var
  lclItem: TQueryItemsDictonary.TDictionaryPair;
  cocoaItem: NSURLQueryItem;
  cocoaItems: NSMutableArray;
begin
  cocoaItems:= NSMutableArray.arrayWithCapacity( lclItems.Count );
  for lclItem in lclItems do begin
    cocoaItem:= NSURLQueryItem.queryItemWithName_value(
      StringToNSString(lclItem.Key),
      StringToNSString(lclItem.Value) );
    cocoaItems.addObject( cocoaItem );
  end;
  Result:= cocoaItems;
end;

class procedure THttpUtil.openInSafari(
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

class function THttpUtil.queryValue(components: NSURLComponents;
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
  const ensureAscii: Boolean = False ): String;
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
  jsonData:= NSJSONSerialization.dataWithJSONObject_options_error( json, 1 shl 3, @error );
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
var
  jsonData: NSData;
  error: NSError;
begin
  error:= nil;
  jsonData:= StringToNSString(jsonString).dataUsingEncoding( NSUTF8StringEncoding );
  Result:= NSJSONSerialization.JSONObjectWithData_options_error( jsonData, 0, @error );
  if error <> nil then
    Result:= nil;
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

class procedure TJsonUtil.setArray(const json: NSMutableDictionary; const key: String;
  const value: NSArray);
begin
  json.setObject_forKey( value, StringToNSString(key) );
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

class function TJsonUtil.getArray(const json: NSDictionary; const key: String
  ): NSArray;
begin
  Result:= NSArray( json.objectForKey( StringToNSString(key) ) );
end;

procedure defaultLogProc( const MsgType: Integer; const message: String );
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

{ THttpClientUtil }

class function THttpClientUtil.toQueryItems(
  const lclItems: TQueryItemsDictonary ): NSArray;
var
  lclItem: TQueryItemsDictonary.TDictionaryPair;
  cocoaItem: NSURLQueryItem;
  cocoaItems: NSMutableArray;
begin
  cocoaItems:= NSMutableArray.arrayWithCapacity( lclItems.Count );
  for lclItem in lclItems do begin
    cocoaItem:= NSURLQueryItem.queryItemWithName_value(
      StringToNSString(lclItem.Key),
      StringToNSString(lclItem.Value) );
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
  TLogUtil.setLogProc( @defaultLogProc );

end.

