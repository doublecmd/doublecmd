unit uDarwinFinderModel;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, LCLType,
  sqldb, SQLite3Conn, syncobjs,
  uLog, uDebug,
  MacOSAll, CocoaAll, CocoaConst, CocoaUtils, Cocoa_Extra,
  uDarwinUtil;

type

  { TFinderTag }

  TFinderTag = objcclass( NSObject )
  private
    _name: NSString;
    _colorIndex: NSInteger;
    _isShowingInSidebar: Boolean;
    _isUserDefined: Boolean;
  public
    class function tagWithParams( const name: NSString; const colorIndex: Integer;
      const isShowingInSidebar: Boolean; const isUserDefined: Boolean ): TFinderTag;
      message 'tagWithParams:name:colorIndex:isShowingInSidebar:';
    procedure dealloc; override;

    function name: NSString; message 'tag_name';
    function colorIndex: NSInteger; message 'tag_colorIndex';
    function isShowingInSidebar: Boolean; message 'tag_isShowingInSidebar';
    function isUserDefined: Boolean; message 'tag_isUserDefined';
    function editorColor: NSColor; message 'tag_editorColor';
    function menuColor: NSColor; message 'tag_menuColor';
  end;

  { TFinderTags }

  TFinderTags = class
  private class var
    _lockObject: TCriticalSection;
    _tags: NSDictionary;
  public
    class constructor Create;

    class function tags: NSDictionary;
    class procedure update;
    class function getTagOfName( tagName: NSString ): TFinderTag;
  end;

  { TFinderTagNSColors }

  TFinderTagNSColors = Array of NSColor;

  TMacOSSearchResultHandler = procedure ( const searchName: String; const files: TStringArray ) of object;

  TFinderFavoriteTagMenuItemState = ( selectionAll, selectionNone, selectionMixed );

  { TDarwinFinderModelUtil }

  TDarwinFinderModelUtil = class
  strict private class var
    _editorFinderTagNSColors: TFinderTagNSColors;
    _menuFinderTagNSColors: TFinderTagNSColors;
    _decorationFinderTagNSColors: TFinderTagNSColors;
    _favoriteTags: NSArray;
  private
    class function getAllTags: NSDictionary;
    class function getFavoriteTags: NSArray; static;
    class function getTagsData_macOS12: NSDictionary;
    class function getTagsData_macOS11: NSDictionary;
    class function doGetAllTags( const tagDictionary: NSDictionary ): NSDictionary;
    class function getTagsDataFromDatabase: TBytes;
    class procedure initFinderTagNSColors;
    class procedure doSearchFiles(
      const searchName: NSString;
      const handler: TMacOSSearchResultHandler;
      const predicate: NSPredicate;
      const scopes: NSArray );
  public
    class function getFavoriteTagNames: NSArray;
    class function getSidebarTagNames: NSArray;
  public
    class function getTagStateForFiles( const tagName: NSString ; const urls: NSArray ):
      TFinderFavoriteTagMenuItemState;
    class function getTagNamesOfFile( const url: NSURL ): NSArray;
    class function getTagNamesOfFiles( const urls: NSArray ): NSArray;
    class procedure setTagNamesOfFile( const url: NSURL; const tagNames: NSArray );
    class procedure addTagForFile( const url: NSURL; const tagName: NSString );
    class procedure addTagForFiles( const urls: NSArray; const tagName: NSString );
    class procedure removeTagForFile( const url: NSURL; const tagName: NSString );
    class procedure removeTagForFiles( const urls: NSArray; const tagName: NSString );
  public
    class procedure searchFilesForTagName( const tagName: NSString; const handler: TMacOSSearchResultHandler );
    class procedure searchFilesForTagNames( const tagNames: NSArray; const handler: TMacOSSearchResultHandler );
    class procedure searchFilesBySavedSearch( const path: NSString; const handler: TMacOSSearchResultHandler );
    class procedure searchFilesBySavedSearch( const path: String; const handler: TMacOSSearchResultHandler );
  public
    class property editorFinderTagNSColors: TFinderTagNSColors read _editorFinderTagNSColors;
    class property menuFinderTagNSColors: TFinderTagNSColors read _menuFinderTagNSColors;
    class property decorationFinderTagNSColors: TFinderTagNSColors read _decorationFinderTagNSColors;
    class property favoriteTags: NSArray read getFavoriteTags;
  end;

implementation

const
  FINDER_TAGS_DATABASE_NAME         = '/Data/com.apple.kvs/com.apple.KeyValueService-Production.sqlite';
  FINDER_TAGS_DATABASE_UUID_PATH    = '/Library/Daemon Containers';
  FINDER_TAGS_DATABASE_STATIC_PATH  = '/Library/SyncedPreferences/com.apple.kvs/com.apple.KeyValueService-Production.sqlite';
  FINDER_TAGS_FILE_PATH_11minus     = '/Library/SyncedPreferences/com.apple.finder.plist';
  FAVORITE_FINDER_TAGS_FILE_PATH    = '/Library/Preferences/com.apple.finder.plist';

var
  NSSTR_FINDER_TAGS_DATABASE_NAME: NSString;
  NSSTR_FINDER_TAGS_DATABASE_UUID_PATH: NSString;
  FINDER_TAGS_DATABASE_PATH_12to13: String;

{ TFinderTag }

class function TFinderTag.tagWithParams( const name: NSString; const colorIndex: Integer;
  const isShowingInSidebar: Boolean; const isUserDefined: Boolean ): TFinderTag;
begin
  Result:= TFinderTag.new;
  Result._name:= name.retain;
  if (colorIndex>=0) and (colorIndex<length(TDarwinFinderModelUtil.editorFinderTagNSColors)) then
    Result._colorIndex:= colorIndex;
  Result._isShowingInSidebar:= isShowingInSidebar;
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

function TFinderTag.isShowingInSidebar: Boolean;
begin
  Result:= _isShowingInSidebar;
end;

function TFinderTag.isUserDefined: Boolean;
begin
  Result:= _isUserDefined;
end;

function TFinderTag.editorColor: NSColor;
begin
  Result:= TDarwinFinderModelUtil.editorFinderTagNSColors[ _colorIndex ];
end;

function TFinderTag.menuColor: NSColor;
begin
  Result:= TDarwinFinderModelUtil.menuFinderTagNSColors[ _colorIndex ];
end;

{ TFinderTags }

class constructor TFinderTags.Create;
begin
  _lockObject:= TCriticalSection.Create;
end;

class function TFinderTags.tags: NSDictionary;
begin
  Result:= _tags;
end;

class procedure TFinderTags.update;
var
  newTags: NSDictionary;
begin
  newTags:= TDarwinFinderModelUtil.getAllTags;
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

  _lockObject.Acquire;
  try
    if _tags = nil then
      self.update;

    if _tags = nil then
      Exit;
    Result:= _tags.objectForKey( tagName );
  finally
    _lockObject.Release;
  end;
end;

{ TMacOSQueryHandler }

type
  TMacOSQueryHandler = objcclass( NSObject )
  private
    _queryName: NSString;
    _query: NSMetadataQuery;
    _handler: TMacOSSearchResultHandler;
    procedure initalGatherComplete( sender: id ); message 'initalGatherComplete:';
  public
    function initWithName( name: NSString ): id; message 'doublecmd_initWithName:';
    procedure dealloc; override;
  end;

procedure TMacOSQueryHandler.initalGatherComplete(sender: id);
var
  item: NSMetadataItem;
  path: NSString;
  files: TStringArray;
  i: Integer;
  count: Integer;
begin
  _query.stopQuery;
  NSNotificationCenter.defaultCenter.removeObserver_name_object(
    self,
    NSMetadataQueryDidFinishGatheringNotification,
    _query );

  files:= nil;
  count:= _query.resultCount;
  if count > 0 then begin
    SetLength( files, count );
    for i:=0 to count-1 do begin
      item:= NSMetadataItem( _query.results.objectAtIndex(i) );
      path:= item.valueForAttribute( NSString(kMDItemPath) );
      files[i]:= path.UTF8String;
    end;
  end;

  _handler( _queryName.UTF8String, files );

  self.release;
end;

function TMacOSQueryHandler.initWithName(name: NSString): id;
begin
  _queryName:= name;
  _queryName.retain;
  Result:= self;
end;

procedure TMacOSQueryHandler.dealloc;
begin
  _queryName.release;
  _query.release;
end;

{ TDarwinFinderModelUtil }

class function TDarwinFinderModelUtil.getTagNamesOfFile(const url: NSURL
  ): NSArray;
var
  ret: Boolean;
  tagNames: NSArray;
  error: NSError = nil;
begin
  Result:= nil;
  ret:= url.getResourceValue_forKey_error( @tagNames, NSURLTagNamesKey, @error );
  if ret then
    Result:= tagNames
  else
    logDarwinError( 'TDarwinFinderModelUtil.getTagNamesOfFile', error );
end;

class function TDarwinFinderModelUtil.getTagNamesOfFiles(const urls: NSArray
  ): NSArray;
var
  url: NSURL;
  tagNames: NSMutableOrderedSet;
begin
  tagNames:= NSMutableOrderedSet.new;
  for url in urls do begin
    tagNames.addObjectsFromArray( getTagNamesOfFile(url) );
  end;
  Result:= NSArray.arrayWithArray( tagNames.array_ );
  tagNames.release;
end;

class procedure TDarwinFinderModelUtil.setTagNamesOfFile(const url: NSURL;
  const tagNames: NSArray);
var
  error: NSError = nil;
  ok: Boolean;
begin
  ok:= url.setResourceValue_forKey_error( tagNames, NSURLTagNamesKey, @error );
  if NOT ok then
    logDarwinError( 'TDarwinFinderModelUtil.setTagNamesOfFile', error );
end;

class procedure TDarwinFinderModelUtil.addTagForFile(const url: NSURL;
  const tagName: NSString );
var
  tagNames: NSArray;
  newTagNames: NSMutableArray;
begin
  tagNames:= TDarwinFinderModelUtil.getTagNamesOfFile( url );
  if tagNames.containsObject(tagName) then
    Exit;
  newTagNames:= NSMutableArray.arrayWithArray( tagNames );
  newTagNames.addObject( tagName );
  TDarwinFinderModelUtil.setTagNamesOfFile( url, newTagNames );
end;

class procedure TDarwinFinderModelUtil.addTagForFiles(const urls: NSArray;
  const tagName: NSString);
var
  url: NSURL;
begin
  for url in urls do begin
    addTagForFile( url, tagName );
  end;
end;

class procedure TDarwinFinderModelUtil.removeTagForFile(const url: NSURL;
  const tagName: NSString);
var
  tagNames: NSArray;
  newTagNames: NSMutableArray;
begin
  tagNames:= TDarwinFinderModelUtil.getTagNamesOfFile( url );
  newTagNames:= NSMutableArray.arrayWithArray( tagNames );
  newTagNames.removeObject( tagName );
  TDarwinFinderModelUtil.setTagNamesOfFile( url, newTagNames );
end;

class procedure TDarwinFinderModelUtil.removeTagForFiles(const urls: NSArray;
  const tagName: NSString);
var
  url: NSURL;
begin
  for url in urls do begin
    removeTagForFile( url, tagName );
  end;
end;

class procedure TDarwinFinderModelUtil.doSearchFiles(
  const searchName: NSString;
  const handler: TMacOSSearchResultHandler;
  const predicate: NSPredicate;
  const scopes: NSArray);
var
  queryHandler: TMacOSQueryHandler;
  query: NSMetadataQuery;
begin
  // release in initalGatherComplete()
  query:= NSMetadataQuery.new;
  // release in initalGatherComplete()
  queryHandler:= TMacOSQueryHandler.alloc.initWithName( searchName );
  queryHandler._query:= query;
  queryHandler._handler:= handler;
  NSNotificationCenter.defaultCenter.addObserver_selector_name_object(
    queryHandler,
    objcselector('initalGatherComplete:'),
    NSMetadataQueryDidFinishGatheringNotification,
    query );

  query.setPredicate( predicate );
  if scopes <> nil then
    query.setSearchScopes( scopes );
  query.startQuery;
end;

class procedure TDarwinFinderModelUtil.searchFilesBySavedSearch(
  const path: NSString; const handler: TMacOSSearchResultHandler);
var
  searchName: NSString;
  predicate: NSPredicate;
  rawQuery: NSString = nil;
  searchScopes: NSArray = nil;

  procedure analyseSavedSearch;
  var
    plistData: NSData;
    plistProperties: id;
    error: NSError = nil;
  begin
    plistData:= NSData.dataWithContentsOfFile( path );
    if plistData = nil then
      raise EInOutError.Create( 'savedSearch File Read Error: ' + path.UTF8String );

    plistProperties:= NSPropertyListSerialization.propertyListWithData_options_format_error(
      plistData, NSPropertyListImmutable, nil, @error );
    if plistProperties = nil then begin
      logDarwinError( 'TDarwinFinderModelUtil.analyseSavedSearch', error );
      raise EFormatError.Create( 'savedSearch File Content Error: ' + path.UTF8String );
    end;

    plistProperties:= plistProperties.valueForKeyPath( NSSTR('RawQueryDict') );
    if plistProperties = nil then
      raise EFormatError.Create( 'savedSearch File Content Error: ' + path.UTF8String );

    rawQuery:= plistProperties.valueForKeyPath( NSSTR('RawQuery') );
    if rawQuery = nil then
      raise EFormatError.Create( 'savedSearch File Raw Query Not Found: ' + path.UTF8String );

    searchScopes:= plistProperties.valueForKeyPath( NSSTR('SearchScopes') );
  end;

begin
  searchName:= path.lastPathComponent.stringByDeletingPathExtension;
  analyseSavedSearch;
  predicate:= NSPredicate.predicateFromMetadataQueryString( rawQuery );
  self.doSearchFiles( searchName, handler, predicate, searchScopes );
end;

class procedure TDarwinFinderModelUtil.searchFilesBySavedSearch(
  const path: String; const handler: TMacOSSearchResultHandler);
begin
  TDarwinFinderModelUtil.searchFilesBySavedSearch( StrToNSString(path), handler );
end;

class procedure TDarwinFinderModelUtil.searchFilesForTagNames(
  const tagNames: NSArray; const handler: TMacOSSearchResultHandler );

  function toString: NSString;
  var
    tagName: NSString;
    name: NSMutableString;
  begin
    name:= NSMutableString.new;
    for tagName in tagNames do begin
      name.appendString( tagName );
      name.appendString( NSSTR('|') );
    end;
    Result:= name.substringToIndex( name.length-1 );
    name.release;
  end;

  function formatString: NSString;
  var
    format: NSMutableString;
    count: Integer;
    i: Integer;
  begin
    format:= NSMutableString.new;
    count:= tagNames.count;
    for i:=1 to count do begin
      format.appendString( NSSTR('(kMDItemUserTags == %@) && ') );
    end;
    Result:= format.substringToIndex( format.length-4 );
    format.release;
  end;

var
  searchName: NSString;
  predicate: NSPredicate;
begin
  if tagNames.count = 0 then
    Exit;

  searchName:= toString();
  predicate:= NSPredicate.predicateWithFormat_argumentArray( formatString(), tagNames );
  self.doSearchFiles( searchName, handler, predicate, nil );
end;

class procedure TDarwinFinderModelUtil.searchFilesForTagName(
  const tagName: NSString; const handler: TMacOSSearchResultHandler);
var
  tagNames: NSArray;
begin
  tagNames:= NSArray.arrayWithObject( tagName );
  TDarwinFinderModelUtil.searchFilesForTagNames( tagNames, handler );
end;

class function TDarwinFinderModelUtil.getAllTags: NSDictionary;
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
      LogWrite( 'Exception in uDarwinFinderUtil.getAllTags(): ' + e.ToString, lmtError );
    end;
  end;
end;

class function TDarwinFinderModelUtil.getFavoriteTagNames: NSArray;
var
  path: NSString;
  plistData: NSData;
  plistProperties: id;
  error: NSError = nil;
begin
  Result:= nil;
  path:= NSHomeDirectory.stringByAppendingString( NSSTR(FAVORITE_FINDER_TAGS_FILE_PATH) );

  plistData:= NSData.dataWithContentsOfFile( path );
  if plistData = nil then
    Exit;

  plistProperties:= NSPropertyListSerialization.propertyListWithData_options_format_error(
    plistData, NSPropertyListImmutable, nil, @error );
  if plistProperties = nil then begin
    logDarwinError( 'TDarwinFinderModelUtil.getFavoriteTagNames', error );
    Exit;
  end;

  Result:= plistProperties.valueForKeyPath( NSSTR('FavoriteTagNames') );
end;

class function TDarwinFinderModelUtil.getSidebarTagNames: NSArray;
var
  tagNames: NSMutableArray;
  tag: TFinderTag;
begin
  TFinderTags.update;
  tagNames:= NSMutableArray.arrayWithCapacity( 16 );
  for tag in TFinderTags.tags.allValues do begin
    if tag.isShowingInSidebar then
      tagNames.addObject( tag.name );
  end;
  Result:= tagNames;
end;

class function TDarwinFinderModelUtil.getTagStateForFiles(
  const tagName: NSString ; const urls: NSArray ): TFinderFavoriteTagMenuItemState;
var
  tagNames: NSArray;
  url: NSURL;
  matchCount: Integer;
begin
  matchCount:= 0;
  for url in urls do begin
    tagNames:= getTagNamesOfFile( url );
    if tagNames.containsObject(tagName) then
      matchCount:= matchCount + 1;
  end;
  if matchCount = 0 then
    Result:= selectionNone
  else if matchCount = urls.count then
    Result:= selectionAll
  else
    Result:= selectionMixed;
end;

class function TDarwinFinderModelUtil.getFavoriteTags: NSArray;
var
  tagNames: NSArray;
  tagName: NSString;
  tags: NSMutableArray;
  tag: TFinderTag;
begin
  tagNames:= TDarwinFinderModelUtil.getFavoriteTagNames;
  tags:= NSMutableArray.alloc.initWithCapacity( tagNames.count );
  for tagName in tagNames do begin
    if tagName.length = 0 then
      continue;
    tag:= TFinderTags.getTagOfName( tagName );
    if tag = nil then
      continue;
    tags.addObject( tag );
  end;

  if tags.count > 0 then begin
    _favoriteTags.release;
    _favoriteTags:= tags;
  end;

  Result:= _favoriteTags;
end;

class function TDarwinFinderModelUtil.getTagsData_macOS12: NSDictionary;
var
  plistBytes: TBytes;
  plistData: NSData;
  error: NSError = nil;
begin
  Result:= nil;
  plistBytes:= TDarwinFinderModelUtil.getTagsDataFromDatabase;
  if plistBytes = nil then
    Exit;

  plistData:= NSData.dataWithBytes_length( @plistBytes[0], Length(plistBytes) );
  if plistData = nil then
    Exit;

  Result:= NSPropertyListSerialization.propertyListWithData_options_format_error(
    plistData, NSPropertyListImmutable, nil, @error );
  if Result = nil then
    logDarwinError( 'TDarwinFinderModelUtil.getTagsData_macOS12', error );
end;

class function TDarwinFinderModelUtil.getTagsData_macOS11: NSDictionary;
var
  path: NSString;
  plistData: NSData;
  plistProperties: id;
  error: NSError = nil;
begin
  Result:= nil;
  path:= NSHomeDirectory.stringByAppendingString( NSSTR(FINDER_TAGS_FILE_PATH_11minus) );

  plistData:= NSData.dataWithContentsOfFile( path );
  if plistData = nil then
    Exit;

  plistProperties:= NSPropertyListSerialization.propertyListWithData_options_format_error(
    plistData, NSPropertyListImmutable, nil, @error );
  if plistProperties = nil then begin
    logDarwinError( 'TDarwinFinderModelUtil.getTagsData_macOS11', error );
    Exit;
  end;

  Result:= plistProperties.valueForKeyPath( NSSTR('values.FinderTagDict.value') );
end;

class function TDarwinFinderModelUtil.getTagsDataFromDatabase: TBytes;
  function getDatabaseUUIDPath: String;
  var
    manager: NSFileManager;
    subPaths: NSArray;
    uuid: NSString;
    databasePath: NSString;
    error: NSError = nil;
  begin
    manager:= NSFileManager.defaultManager;
    subPaths:= manager.contentsOfDirectoryAtPath_error( NSSTR_FINDER_TAGS_DATABASE_UUID_PATH, @error );
    if subPaths = nil then begin
      logDarwinError( 'TDarwinFinderModelUtil.getTagsDataFromDatabase', error );
      Exit;
    end;

    for uuid in subPaths do begin
      databasePath:= NSSTR_FINDER_TAGS_DATABASE_UUID_PATH.stringByAppendingPathComponent(uuid).stringByAppendingPathComponent(NSSTR_FINDER_TAGS_DATABASE_NAME);
      if manager.fileExistsAtPath(databasePath) then begin
        Result:= databasePath.UTF8String;
        Exit;
      end;
    end;
    Result:= EmptyStr;
  end;

  function getDatabasePath: String;
  begin
    if NSAppKitVersionNumber < NSAppKitVersionNumber14_0 then
      Result:= FINDER_TAGS_DATABASE_PATH_12to13
    else
      Result:= getDatabaseUUIDPath;
  end;

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
    databasePath:= getDatabasePath;
    if databasePath = EmptyStr then
      Exit;
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

class function TDarwinFinderModelUtil.doGetAllTags( const tagDictionary: NSDictionary ): NSDictionary;
var
  plistTagArray: NSArray;

  plistTagItem: NSDictionary;
  plistTagName: NSString;
  plistTagColorNumber: NSNumber;
  plistShowingInSidebar: NSNumber;
  plistTagUserDefined: NSNumber;
  showingInSidebar: Boolean;

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
    plistShowingInSidebar:= plistTagItem.valueForKey( NSSTR('v') );
    plistTagUserDefined:= plistTagItem.valueForKey( NSSTR('p') );

    showingInSidebar:= True;
    if plistShowingInSidebar <> nil then
      showingInSidebar:= plistShowingInSidebar.boolValue;

    tag:= TFinderTag.tagWithParams(
      plistTagName,
      plistTagColorNumber.integerValue,
      showingInSidebar,
      plistTagUserDefined.boolValue );

    allFinderTagDict.setValue_forKey( tag, plistTagName );
  end;

  Result:= allFinderTagDict;
end;

class procedure TDarwinFinderModelUtil.initFinderTagNSColors;
begin
  _editorFinderTagNSColors:= [
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.656, 0.656, 0.656, 0.5 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.588, 0.588, 0.612, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.427, 0.800, 0.431, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.698, 0.424, 0.835, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.329, 0.533, 0.941, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.925, 0.784, 0.373, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.902, 0.384, 0.373, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.925, 0.627, 0.345, 1 ).retain
  ];

  _menuFinderTagNSColors:= [
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.400, 0.400, 0.400, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.541, 0.541, 0.561, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.263, 0.788, 0.306, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.631, 0.333, 0.784, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.169, 0.455, 0.957, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.957, 0.773, 0.165, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.949, 0.263, 0.255, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.953, 0.569, 0.161, 1 ).retain
  ];

  _decorationFinderTagNSColors:= [
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.000, 0.000, 0.000, 0 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.580, 0.576, 0.596, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.341, 0.827, 0.349, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.694, 0.349, 0.875, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.231, 0.494, 0.996, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 1.000, 0.812, 0.263, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 1.000, 0.306, 0.294, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 1.000, 0.604, 0.243, 1 ).retain
  ];
end;

procedure initNSSTR;
begin
  NSSTR_FINDER_TAGS_DATABASE_NAME:= NSSTR( FINDER_TAGS_DATABASE_NAME );
  NSSTR_FINDER_TAGS_DATABASE_UUID_PATH:= NSHomeDirectory.stringByAppendingPathComponent( NSSTR(FINDER_TAGS_DATABASE_UUID_PATH) ).retain;
  FINDER_TAGS_DATABASE_PATH_12to13:= NSHomeDirectory.UTF8String + FINDER_TAGS_DATABASE_STATIC_PATH;
end;

initialization
  initNSSTR;
  TDarwinFinderModelUtil.initFinderTagNSColors;

end.

