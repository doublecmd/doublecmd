unit uDarwinFinder;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, LCLType,
  sqldb, SQLite3Conn,
  MacOSAll, CocoaAll, CocoaUtils;

type

  { uDarwinFinderUtil }

  uDarwinFinderUtil = class
  public
    class procedure popoverFileTags(
      const path: String; const positioningView: NSView; const edge: NSRectEdge );
  private
    class function getTagNamesOfFile( const url: NSUrl ): NSArray;
    class procedure setTagNamesOfFile( const url: NSUrl; const tagNames: NSArray );
  private
    class function getAllTags: NSDictionary;
    class function getAllTagsFromPlist( const plistBytes: TBytes ): NSDictionary;
    class function getTagsPlistFromDatabase: TBytes;
  end;

implementation

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
    class procedure update;
    class function getTagOfName( tagName: NSString ): TFinderTag;
  end;

type
  TFinderTagNSColors = Array of NSColor;

var
  defaultFinderTagNSColors: TFinderTagNSColors;

type

  { NSTokenAttachmentCell }

  NSTokenAttachmentCell = objcclass external ( NSTextAttachmentCell )
  end;

  { TCocoaTokenAttachmentCell }

  TCocoaTokenAttachmentCell = objcclass( NSTokenAttachmentCell )
    procedure drawWithFrame_inView(cellFrame: NSRect; controlView_: NSView); override;
    function cellSizeForBounds(aRect: NSRect): NSSize; override;
    function titleRectForBounds(theRect: NSRect): NSRect; override;
  end;

  { TCocoaTokenFieldCell }

  TCocoaTokenFieldCell = objcclass( NSTokenFieldCell )
    function setUpTokenAttachmentCell( aCell: NSTokenAttachmentCell;
      anObject: id ): NSTokenAttachmentCell;
      message 'setUpTokenAttachmentCell:forRepresentedObject:';
  end;

  { TFinderTagsEditorPanel }

  TFinderTagsEditorPanel = objcclass( NSObject,
    NSPopoverDelegateProtocol,
    NSTokenFieldDelegateProtocol )
  private
    _url: NSUrl;
    _tagsTokenField: NSTokenField;
    _cancel: Boolean;
  public
    class function editorWithPath( const path: NSString): id; message 'editorWithPath:';
    procedure dealloc; override;
    procedure showPopover( const sender: NSView ; const edge: NSRectEdge );
      message 'showPopover:sender:';
  private
    function control_textView_doCommandBySelector (control: NSControl; textView: NSTextView; commandSelector: SEL): ObjCBOOL;
    procedure popoverWillClose(notification: NSNotification);
    procedure popoverDidClose (notification: NSNotification);
  end;

{ TFinderTag }

class function TFinderTag.tagWithParams( const name: NSString; const colorIndex: Integer;
  const isUserDefined: Boolean): TFinderTag;
begin
  Result:= TFinderTag.new;
  Result._name:= name.retain;
  if (colorIndex>=0) and (colorIndex<length(defaultFinderTagNSColors)) then
    Result._colorIndex:= colorIndex;
  Result._isUserDefined:= isUserDefined;
  Result.autorelease;
end;

procedure TFinderTag.dealloc;
begin
  _name.release;
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
  Result:= defaultFinderTagNSColors[ _colorIndex ];
end;

{ TFinderTags }

class procedure TFinderTags.update;
var
  newTags: NSDictionary;
begin
  newTags:= uDarwinFinderUtil.getAllTags;
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

{ TCocoaTokenAttachmentCell }

procedure TCocoaTokenAttachmentCell.drawWithFrame_inView(cellFrame: NSRect;
  controlView_: NSView);
var
  finderTag: TFinderTag;
  tagRect: NSRect;
  path: NSBezierPath;
begin
  finderTag:= TFinderTags.getTagOfName( self.stringValue );
  if finderTag <> nil then begin
    tagRect:= self.drawingRectForBounds( cellFrame );
    path:= NSBezierPath.bezierPathWithRoundedRect_xRadius_yRadius(
           tagRect,
           2,
           2 );
    finderTag.color.set_;
    path.fill;
  end;
  drawInteriorWithFrame_inView( cellFrame, controlView_ );
end;

function TCocoaTokenAttachmentCell.cellSizeForBounds(aRect: NSRect): NSSize;
var
  preferedWidth: CGFloat;
begin
  Result:= inherited;
  preferedWidth:= self.attributedStringValue.size.width + 14;
  if Result.width > preferedWidth then
    Result.width:= preferedWidth;
end;

function TCocoaTokenAttachmentCell.titleRectForBounds(theRect: NSRect): NSRect;
begin
  Result:= theRect;
  Result.origin.x:= Result.origin.x + 2;
end;

{ TCocoaTokenFieldCell }

function TCocoaTokenFieldCell.setUpTokenAttachmentCell(
  aCell: NSTokenAttachmentCell; anObject: id): NSTokenAttachmentCell;
begin
  Result:= TCocoaTokenAttachmentCell.alloc.initTextCell( NSString(anObject) );
  Result.setRepresentedObject( anObject );
  Result.autorelease;
end;

{ TFinderTagsEditorPanel }

class function TFinderTagsEditorPanel.editorWithPath( const path: NSString ): id;
var
  panel: TFinderTagsEditorPanel;
begin
  panel:= TFinderTagsEditorPanel.new;
  panel._url:= NSURL.fileURLWithPath( path );
  panel._url.retain;
  Result:= panel;

  TFinderTags.update;
end;

procedure TFinderTagsEditorPanel.dealloc;
begin
  _tagsTokenField.release;
  _url.release;
end;

procedure TFinderTagsEditorPanel.popoverDidClose(notification: NSNotification);
begin
  self.release;
end;

procedure TFinderTagsEditorPanel.showPopover( const sender: NSView; const edge: NSRectEdge );
var
  contentRect: NSRect;
  popover: NSPopover;
  controller: NSViewController;
  contentView: NSView;
  tagNameArray: NSArray;
begin
  contentRect.origin.x:= 0;
  contentRect.origin.y:= 0;
  contentRect.size.Width:= 260;
  contentRect.size.Height:= 108;
  contentView:= NSView.alloc.initWithFrame( contentRect );
  controller:= NSViewController.new;
  controller.setView( contentView );

  tagNameArray:= uDarwinFinderUtil.getTagNamesOfFile( _url );
  contentRect:= NSInsetRect( contentRect, 6, 6 );
  NSTokenField.setCellClass( TCocoaTokenFieldCell );
  _tagsTokenField:= NSTokenField.alloc.initWithFrame( contentRect );
  _tagsTokenField.setDelegate( self );
  _tagsTokenField.setObjectValue( tagNameArray );
  _tagsTokenField.setFocusRingType( NSFocusRingTypeNone );
  contentView.addSubview( _tagsTokenField );

  popover:= NSPopover.new;
  popover.setContentViewController( controller );
  popover.setDelegate( self );
  popover.setBehavior( NSPopoverBehaviorTransient );

  popover.showRelativeToRect_ofView_preferredEdge(
    sender.bounds,
    sender,
    edge );

  NSControlMoveCaretToTheEnd( _tagsTokenField );

  contentView.release;
  controller.release;
  popover.release;
end;

function TFinderTagsEditorPanel.control_textView_doCommandBySelector(
  control: NSControl; textView: NSTextView; commandSelector: SEL): ObjCBOOL;
begin
  if commandSelector = ObjCSelector('cancelOperation:') then
    _cancel:= True;
  Result:= False;
end;

procedure TFinderTagsEditorPanel.popoverWillClose(notification: NSNotification);
var
  tagNameArray: NSArray;
begin
  if _cancel then
    Exit;
  tagNameArray:= _tagsTokenField.objectValue;
  uDarwinFinderUtil.setTagNamesOfFile( _url, tagNameArray );
end;

{ uDarwinFinderUtil }

class procedure uDarwinFinderUtil.popoverFileTags(
  const path: String; const positioningView: NSView ; const edge: NSRectEdge );
var
  panel: TFinderTagsEditorPanel;
begin
  panel:= TFinderTagsEditorPanel.editorWithPath( StrToNSString(path) );
  panel.showPopover( positioningView, edge );
end;

class function uDarwinFinderUtil.getTagNamesOfFile( const url: NSUrl ): NSArray;
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

class procedure uDarwinFinderUtil.setTagNamesOfFile( const url: NSUrl;
  const tagNames: NSArray);
begin
  url.setResourceValue_forKey_error( tagNames, NSURLTagNamesKey, nil );
end;

const
  NEW_FINDER_TAGS_DATABASE_PATH = '/Library/SyncedPreferences/com.apple.kvs/com.apple.KeyValueService-Production.sqlite';

class function uDarwinFinderUtil.getAllTags: NSDictionary;
var
  plistBytes: TBytes;
begin
  Result:= nil;
  plistBytes:= uDarwinFinderUtil.getTagsPlistFromDatabase;
  if plistBytes <> nil then
    Result:= uDarwinFinderUtil.getAllTagsFromPlist( plistBytes );
end;

class function uDarwinFinderUtil.getTagsPlistFromDatabase: TBytes;
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

class function uDarwinFinderUtil.getAllTagsFromPlist( const plistBytes: TBytes ): NSDictionary;
var
  plistData: NSData;
  plistProperties: id;
  plistTagArray: NSArray;

  plistTagItem: NSDictionary;
  plistTagName: NSString;
  plistTagColorNumber: NSNumber;
  plistTagUserDefined: NSNumber;

  allFinderTagDict: NSMutableDictionary;
  tag: TFinderTag;
begin
  Result:= nil;
  plistData:= NSData.dataWithBytes_length( @plistBytes[0], Length(plistBytes) );
  if plistData = nil then
    Exit;

  plistProperties:= NSPropertyListSerialization.propertyListWithData_options_format_error(
    plistData, NSPropertyListImmutable, nil, nil );

  if plistProperties = nil then
    Exit;

  plistTagArray:= plistProperties.valueForKeyPath( NSSTR('FinderTags') );
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

procedure initFinderTagNSColors;
begin
  defaultFinderTagNSColors:= [
    NSColor.windowBackgroundColor,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.656, 0.656, 0.656, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.699, 0.836, 0.266, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.746, 0.547, 0.844, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.340, 0.629, 0.996, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.934, 0.852, 0.266, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.980, 0.383, 0.348, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.961, 0.660, 0.254, 1 ).retain
  ];
end;

initialization
  initFinderTagNSColors;

end.

