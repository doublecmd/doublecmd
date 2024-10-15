unit uDarwinFinderModel;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, LCLType,
  sqldb, SQLite3Conn,
  uDebug,
  CocoaAll, CocoaConst, Cocoa_Extra;

type

  { TFinderTag }

  TFinderTag = objcclass( NSObject )
  private
    _name: NSString;
    _colorIndex: NSInteger;
    _isUserDefined: Boolean;
  public
    class function tagWithParams( const name: NSString; const colorIndex: Integer;
      const isUserDefined: Boolean ): TFinderTag; message 'tagWithParams:name:colorIndex:';
    procedure dealloc; override;

    function name: NSString; message 'tag_name';
    function colorIndex: NSInteger; message 'tag_colorIndex';
    function isUserDefined: Boolean; message 'tag_isUserDefined';
    function color: NSColor; message 'tag_color';
  end;

  { TFinderTags }

  TFinderTags = class
  private class var
    _tags: NSDictionary;
  public
    class function tags: NSDictionary;
    class procedure update;
    class function getTagOfName( tagName: NSString ): TFinderTag;
  end;

  { TFinderTagNSColors }

  TFinderTagNSColors = Array of NSColor;

  { uDarwinFinderModelUtil }

  uDarwinFinderModelUtil = class
  strict private class var
    _rectFinderTagNSColors: TFinderTagNSColors;
    _dotFinderTagNSColors: TFinderTagNSColors;
    _favoriteTags: NSArray;
  private
    class function getAllTags: NSDictionary;
    class function getFavoriteTags: NSArray; static;
    class function getTagsData_macOS12: NSDictionary;
    class function getTagsData_macOS11: NSDictionary;
    class function doGetAllTags( const tagDictionary: NSDictionary ): NSDictionary;
    class function getTagsDataFromDatabase: TBytes;
    class function getFavoriteTagNames: NSArray;
    class procedure initFinderTagNSColors;
  public
    class function getTagNamesOfFile( const url: NSURL ): NSArray;
    class procedure setTagNamesOfFile( const url: NSURL; const tagNames: NSArray );
    class procedure addTagForFile( const url: NSURL; const tagName: NSString );
    class procedure removeTagForFile( const url: NSURL; const tagName: NSString );
  public
    class property rectFinderTagNSColors: TFinderTagNSColors read _rectFinderTagNSColors;
    class property dotFinderTagNSColors: TFinderTagNSColors read _dotFinderTagNSColors;
    class property favoriteTags: NSArray read getFavoriteTags;
  end;

implementation

const
  NEW_FINDER_TAGS_DATABASE_PATH  = '/Library/SyncedPreferences/com.apple.kvs/com.apple.KeyValueService-Production.sqlite';
  OLD_FINDER_TAGS_FILE_PATH      = '/Library/SyncedPreferences/com.apple.finder.plist';
  FAVORITE_FINDER_TAGS_FILE_PATH = '/Library/Preferences/com.apple.finder.plist';

{ TFinderTag }

class function TFinderTag.tagWithParams( const name: NSString; const colorIndex: Integer;
  const isUserDefined: Boolean): TFinderTag;
begin
  Result:= TFinderTag.new;
  Result._name:= name.retain;
  if (colorIndex>=0) and (colorIndex<length(uDarwinFinderModelUtil.rectFinderTagNSColors)) then
    Result._colorIndex:= colorIndex;
  Result._isUserDefined:= isUserDefined;
  Result.autorelease;
end;

procedure TFinderTag.dealloc;
begin
  _name.release;
  Inherited;
end;

function TFinderTag.name: NSString;
begin
  Result:= _name;
end;

function TFinderTag.colorIndex: NSInteger;
begin
  Result:= _colorIndex;
end;

function TFinderTag.isUserDefined: Boolean;
begin
  Result:= _isUserDefined;
end;

function TFinderTag.color: NSColor;
begin
  Result:= uDarwinFinderModelUtil.rectFinderTagNSColors[ _colorIndex ];
end;

{ TFinderTags }

class function TFinderTags.tags: NSDictionary;
begin
  Result:= _tags;
end;

class procedure TFinderTags.update;
var
  newTags: NSDictionary;
begin
  newTags:= uDarwinFinderModelUtil.getAllTags;
  if newTags = nil then
    Exit;
  if _tags <> nil then
    _tags.release;
  _tags:= newTags;
  _tags.retain;
end;

class function TFinderTags.getTagOfName( tagName: NSString ): TFinderTag;
begin
  Result:= nil;
  if _tags = nil then
    self.update;
  if _tags = nil then
    Exit;
  Result:= _tags.objectForKey( tagName );
end;

{ uDarwinFinderModelUtil }

class function uDarwinFinderModelUtil.getTagNamesOfFile(const url: NSURL
  ): NSArray;
var
  ret: Boolean;
  tagNames: NSArray;
  tagColor: NSColor;
begin
  Result:= nil;
  ret:= url.getResourceValue_forKey_error( @tagNames, NSURLTagNamesKey, nil );
  if ret then
    Result:= tagNames;
end;

class procedure uDarwinFinderModelUtil.setTagNamesOfFile(const url: NSURL;
  const tagNames: NSArray);
begin
  url.setResourceValue_forKey_error( tagNames, NSURLTagNamesKey, nil );
end;

class procedure uDarwinFinderModelUtil.addTagForFile(const url: NSURL;
  const tagName: NSString );
var
  tagNames: NSArray;
  newTagNames: NSMutableArray;
begin
  tagNames:= uDarwinFinderModelUtil.getTagNamesOfFile( url );
  newTagNames:= NSMutableArray.arrayWithArray( tagNames );
  newTagNames.addObject( tagName );
  uDarwinFinderModelUtil.setTagNamesOfFile( url, newTagNames );
end;

class procedure uDarwinFinderModelUtil.removeTagForFile(const url: NSURL;
  const tagName: NSString);
var
  tagNames: NSArray;
  newTagNames: NSMutableArray;
begin
  tagNames:= uDarwinFinderModelUtil.getTagNamesOfFile( url );
  newTagNames:= NSMutableArray.arrayWithArray( tagNames );
  newTagNames.removeObject( tagName );
  uDarwinFinderModelUtil.setTagNamesOfFile( url, newTagNames );
end;

class function uDarwinFinderModelUtil.getAllTags: NSDictionary;
var
  tagDictionary: NSDictionary;
begin
  Result:= nil;

  try
    if NSAppKitVersionNumber >= NSAppKitVersionNumber12_0 then
      tagDictionary:= getTagsData_macOS12
    else
      tagDictionary:= getTagsData_macOS11;

    Result:= doGetAllTags( tagDictionary );
  except
    // it is suitable for just recording exception and handling it silently
    on e: Exception do begin
      DCDebug( 'Exception in uDarwinFinderUtil.getAllTags(): ', e.ToString );
    end;
  end;
end;

class function uDarwinFinderModelUtil.getFavoriteTagNames: NSArray;
var
  path: NSString;
  plistData: NSData;
  plistProperties: id;
begin
  Result:= nil;
  path:= NSHomeDirectory.stringByAppendingString( NSSTR(FAVORITE_FINDER_TAGS_FILE_PATH) );

  plistData:= NSData.dataWithContentsOfFile( path );
  if plistData = nil then
    Exit;

  plistProperties:= NSPropertyListSerialization.propertyListWithData_options_format_error(
    plistData, NSPropertyListImmutable, nil, nil );
  if plistProperties = nil then
    Exit;

  Result:= plistProperties.valueForKeyPath( NSSTR('FavoriteTagNames') );
end;

class function uDarwinFinderModelUtil.getFavoriteTags: NSArray;
var
  tagNames: NSArray;
  tagName: NSString;
  tags: NSMutableArray;
  tag: TFinderTag;
begin
  tagNames:= uDarwinFinderModelUtil.getFavoriteTagNames;
  tags:= NSMutableArray.alloc.initWithCapacity( tagNames.count );
  for tagName in tagNames do begin
    if tagName.length = 0 then
      continue;
    tag:= TFinderTags.getTagOfName( tagName );
    tags.addObject( tag );
  end;

  if tags.count > 0 then begin
    _favoriteTags.release;
    _favoriteTags:= tags;
  end;

  Result:= _favoriteTags;
end;

class function uDarwinFinderModelUtil.getTagsData_macOS12: NSDictionary;
var
  plistBytes: TBytes;
  plistData: NSData;
begin
  Result:= nil;
  plistBytes:= uDarwinFinderModelUtil.getTagsDataFromDatabase;
  if plistBytes = nil then
    Exit;

  plistData:= NSData.dataWithBytes_length( @plistBytes[0], Length(plistBytes) );
  if plistData = nil then
    Exit;

  Result:= NSPropertyListSerialization.propertyListWithData_options_format_error(
    plistData, NSPropertyListImmutable, nil, nil );
end;

class function uDarwinFinderModelUtil.getTagsData_macOS11: NSDictionary;
var
  path: NSString;
  plistData: NSData;
  plistProperties: id;
begin
  Result:= nil;
  path:= NSHomeDirectory.stringByAppendingString( NSSTR(OLD_FINDER_TAGS_FILE_PATH) );

  plistData:= NSData.dataWithContentsOfFile( path );
  if plistData = nil then
    Exit;

  plistProperties:= NSPropertyListSerialization.propertyListWithData_options_format_error(
    plistData, NSPropertyListImmutable, nil, nil );
  if plistProperties = nil then
    Exit;

  Result:= plistProperties.valueForKeyPath( NSSTR('values.FinderTagDict.value') );
end;

class function uDarwinFinderModelUtil.getTagsDataFromDatabase: TBytes;
var
  connection: TSQLConnection = nil;
  transaction: TSQLTransaction = nil;
  query: TSQLQuery = nil;
  databasePath: String;
begin
  Result:= nil;
  try
    connection:= TSQLite3Connection.Create( nil );
    transaction:= TSQLTransaction.Create( connection );
    connection.Transaction:= transaction;
    databasePath:= NSHomeDirectory.UTF8String + NEW_FINDER_TAGS_DATABASE_PATH;
    connection.DatabaseName:= databasePath;

    query:= TSQLQuery.Create( nil );
    query.SQL.Text:= 'select ZPLISTDATAVALUE from ZSYDMANAGEDKEYVALUE where ZKEY="FinderTagDict"';
    query.Database:= connection;
    query.Open;
    Result:= query.FieldByName('ZPLISTDATAVALUE').AsBytes;

    query.Close;
    connection.Close;
  finally
    if query <> nil then
      query.Free;
    if transaction <> nil then
      transaction.Free;
    if connection <> nil then
      connection.Free;
  end;
end;

class function uDarwinFinderModelUtil.doGetAllTags( const tagDictionary: NSDictionary ): NSDictionary;
var
  plistTagArray: NSArray;

  plistTagItem: NSDictionary;
  plistTagName: NSString;
  plistTagColorNumber: NSNumber;
  plistTagUserDefined: NSNumber;

  allFinderTagDict: NSMutableDictionary;
  tag: TFinderTag;
begin
  Result:= nil;
  if tagDictionary = nil then
    Exit;

  plistTagArray:= tagDictionary.valueForKeyPath( NSSTR('FinderTags') );
  if plistTagArray = nil then
    Exit;

  allFinderTagDict:= NSMutableDictionary.dictionaryWithCapacity( plistTagArray.count  );
  for plistTagItem in plistTagArray do begin
    plistTagName:= plistTagItem.valueForKey( NSSTR('n') );
    plistTagColorNumber:= plistTagItem.valueForKey( NSSTR('l') );
    plistTagUserDefined:= plistTagItem.valueForKey( NSSTR('p') );
    tag:= TFinderTag.tagWithParams(
      plistTagName,
      plistTagColorNumber.integerValue,
      plistTagUserDefined.boolValue );
    allFinderTagDict.setValue_forKey( tag, plistTagName );
  end;

  Result:= allFinderTagDict;
end;

class procedure uDarwinFinderModelUtil.initFinderTagNSColors;
begin
  _rectFinderTagNSColors:= [
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.656, 0.656, 0.656, 0.5 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.656, 0.656, 0.656, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.699, 0.836, 0.266, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.746, 0.547, 0.844, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.340, 0.629, 0.996, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.934, 0.852, 0.266, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.980, 0.383, 0.348, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.961, 0.660, 0.254, 1 ).retain
  ];

  _dotFinderTagNSColors:= [
    NSColor.textColor,
    NSColor.grayColor,
    NSColor.greenColor,
    NSColor.purpleColor,
    NSColor.blueColor,
    NSColor.yellowColor,
    NSColor.redColor,
    NSColor.orangeColor
  ];
end;

initialization
  uDarwinFinderModelUtil.initFinderTagNSColors;

end.

