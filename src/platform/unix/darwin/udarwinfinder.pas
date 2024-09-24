unit uDarwinFinder;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, LCLType,
  sqldb, SQLite3Conn,
  MacOSAll, CocoaAll, CocoaConst, CocoaUtils, Cocoa_Extra;

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
  private
    class procedure drawTagName( tagName: NSString; color: NSColor; rect: NSRect );
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
  rectFinderTagNSColors: TFinderTagNSColors;
  dotFinderTagNSColors: TFinderTagNSColors;

type

  { NSTokenAttachmentCell }

  NSTokenAttachmentCell = objcclass external ( NSTextAttachmentCell )
  end;

  { TCocoaTokenAttachmentCell }

  TCocoaTokenAttachmentCell = objcclass( NSTokenAttachmentCell )
  private
    function isSelected: Boolean; message 'token_isSelected';
  public
    procedure drawInteriorWithFrame_inView (cellFrame: NSRect; controlView_: NSView); override;
    procedure drawWithFrame_inView(cellFrame: NSRect; controlView_: NSView); override;
    function cellSizeForBounds(aRect: NSRect): NSSize; override;
    function drawingRectForBounds(theRect: NSRect): NSRect; override;
    function titleRectForBounds(theRect: NSRect): NSRect; override;
  end;

  { TCocoaTokenFieldCell }

  TCocoaTokenFieldCell = objcclass( NSTokenFieldCell )
    function setUpTokenAttachmentCell( aCell: NSTokenAttachmentCell;
      anObject: id ): NSTokenAttachmentCell;
      message 'setUpTokenAttachmentCell:forRepresentedObject:';
  end;

  { TCocoaTokenFieldDelegateProtocol }

  TCocoaTokenFieldDelegateProtocol = objcprotocol( NSTokenFieldDelegateProtocol )
    procedure updateFilter( substring: NSString ); message 'updateFilter:';
  end;

  { TCocoaTokenField }

  TCocoaTokenField = objcclass( NSTokenField, NSTextViewDelegateProtocol )
  private
    _selectedTokenObjects: NSMutableSet;
  public
    procedure dealloc; override;
    procedure textViewDidChangeSelection(notification: NSNotification);
    procedure textDidChange(notification: NSNotification); override;
  public
    procedure insertTokenNameReplaceEditingString( const tokenName: NSString );
      message 'insertTokenNameReplaceEditingString:';
    function editingStringRange: NSRange; message 'token_editingStringRange';
    function isSelectedTokenObject( const anObject: id ): Boolean;
      message 'isSelectedTokenObject:';
  end;

  { TFinderTagsListView }

  TFinderTagsListView = objcclass( NSTableView )
    function init: id; override;
    procedure drawRow_clipRect (row: NSInteger; clipRect: NSRect); override;
    function acceptsFirstResponder: ObjCBOOL; override;
  private
    procedure onTagSelected( sender: id ); message 'onTagSelected:';
  end;

  { TFinderTagsEditorPanel }

  TFinderTagsEditorPanel = objcclass( NSObject,
    NSPopoverDelegateProtocol,
    TCocoaTokenFieldDelegateProtocol,
    NSTableViewDataSourceProtocol )
  private
    _url: NSUrl;
    _tagsTokenField: TCocoaTokenField;
    _filterListView: NSTableView;
    _filterTagNames: NSMutableArray;
    _cancel: Boolean;
  public
    class function editorWithPath( const path: NSString): id; message 'editorWithPath:';
    function initWithPath( const path: NSString): id; message 'initWithPath:';
    procedure dealloc; override;
    procedure showPopover( const sender: NSView ; const edge: NSRectEdge );
      message 'showPopover:sender:';
  public
    function numberOfRowsInTableView (tableView: NSTableView): NSInteger;
    function tableView_objectValueForTableColumn_row (
      tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): id;
  public
    procedure insertCurrentFilterToken; message 'insertCurrentFilterToken';
    procedure updateFilter( substring: NSString );
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
  if (colorIndex>=0) and (colorIndex<length(rectFinderTagNSColors)) then
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
  Result:= rectFinderTagNSColors[ _colorIndex ];
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

function TCocoaTokenAttachmentCell.isSelected: Boolean;
begin
  Result:= TCocoaTokenField(controlView).isSelectedTokenObject( self.objectValue );
end;

procedure TCocoaTokenAttachmentCell.drawInteriorWithFrame_inView(
  cellFrame: NSRect; controlView_: NSView);
var
  color: NSColor;
  titleRect: NSRect;
begin
  if self.isSelected then
    color:= NSColor.textBackgroundColor
  else
    color:= NSColor.textColor;

  titleRect:= titleRectForBounds( cellFrame );

  uDarwinFinderUtil.drawTagName( NSString(self.objectValue), color, titleRect );
end;

procedure TCocoaTokenAttachmentCell.drawWithFrame_inView(cellFrame: NSRect;
  controlView_: NSView);
var
  finderTag: TFinderTag;
  color: NSColor;
  drawingRect: NSRect;
  path: NSBezierPath;
begin
  finderTag:= TFinderTags.getTagOfName( self.stringValue );
  if finderTag <> nil then
    color:= finderTag.color
  else
    color:= rectFinderTagNSColors[0];

  drawingRect:= self.drawingRectForBounds( cellFrame );
  path:= NSBezierPath.bezierPathWithRoundedRect_xRadius_yRadius(
         drawingRect,
         2,
         2 );

  if self.isSelected then begin
    NSColor.selectedTextBackgroundColor.set_;
    NSRectFill( cellFrame );
  end;

  color.set_;
  path.fill;

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
  Result.height:= Result.height + 4;
end;

function TCocoaTokenAttachmentCell.drawingRectForBounds(theRect: NSRect
  ): NSRect;
begin
  Result.origin.x:= theRect.origin.x + 2;
  Result.origin.y:= theRect.origin.y + 2;
  Result.size.width:= theRect.size.width - 4;
  Result.size.height:= theRect.size.height - 4;
end;

function TCocoaTokenAttachmentCell.titleRectForBounds(theRect: NSRect): NSRect;
begin
  Result.origin.x:= theRect.origin.x + 7;
  Result.origin.y:= theRect.origin.y + self.font.pointSize + 2;
  Result.size.width:= theRect.size.width - 14;
  Result.size.height:= theRect.size.Height - 4;
end;

{ TCocoaTokenFieldCell }

function TCocoaTokenFieldCell.setUpTokenAttachmentCell(
  aCell: NSTokenAttachmentCell; anObject: id): NSTokenAttachmentCell;
begin
  Result:= TCocoaTokenAttachmentCell.alloc.initTextCell( NSString(anObject) );
  Result.setControlView( self.controlView );
  Result.setRepresentedObject( anObject );
  Result.autorelease;
end;

procedure TCocoaTokenField.dealloc;
begin
  if _selectedTokenObjects <> nil then
    _selectedTokenObjects.release;
  inherited;
end;

{ TCocoaTokenField }

procedure TCocoaTokenField.textViewDidChangeSelection( notification: NSNotification);
var
  range: NSRange;
  beginIndex: NSUInteger;
  endIndex: NSUInteger;
  i: NSUInteger;
  tokenNames: NSArray;
  editor: NSText;
begin
  if _selectedTokenObjects = nil then
    _selectedTokenObjects:= NSMutableSet.new
  else
    _selectedTokenObjects.removeAllObjects;

  editor:= self.currentEditor;
  if editor = nil then
    Exit;

  editor.setNeedsDisplay_( True );

  range:= self.currentEditor.selectedRange;
  if range.length = 0 then
    Exit;

  beginIndex:= range.location;
  endIndex:= range.location + range.length - 1;

  if editor.string_.characterAtIndex(beginIndex) <> NSAttachmentCharacter then
    Exit;

  tokenNames:= NSArray( objectValue );
  for i:= beginIndex to endIndex do begin
    _selectedTokenObjects.addObject( tokenNames.objectAtIndex(i) );
  end;
end;

function TCocoaTokenField.isSelectedTokenObject( const anObject: id ): Boolean;
begin
  Result:= _selectedTokenObjects.containsObject( anObject );
end;

procedure TCocoaTokenField.textDidChange(notification: NSNotification);
var
  filterString: NSString;
  range: NSRange;
begin
  Inherited;

  filterString:= nil;
  range:= self.editingStringRange;
  if range.location <> NSNotFound then
    filterString:= self.currentEditor.string_.substringWithRange( range );
  TCocoaTokenFieldDelegateProtocol(self.delegate).updateFilter( filterString );
end;

procedure TCocoaTokenField.insertTokenNameReplaceEditingString(
  const tokenName: NSString);
var
  editor: NSTextView;
begin
  editor:= NSTextView( self.currentEditor );
  editor.insertText_replacementRange( tokenName, self.editingStringRange );
end;

function TCocoaTokenField.editingStringRange: NSRange;
var
  editor: NSText;
  editorString: NSString;
  caretIndex: NSUInteger;

  function findLastToken: NSUInteger;
  var
    tokenRange: NSRange;
    tokenIndex: NSUInteger;
  begin
    tokenRange.location:= 0;
    tokenRange.length:= caretIndex;
    tokenRange:= editorString.rangeOfString_options_range(
      NSSTR_ATTACHMENT_CHARACTER, NSBackwardsSearch, tokenRange );
    tokenIndex:= tokenRange.location;
    if tokenIndex = NSNotFound then
      tokenIndex:= 0
    else
      tokenIndex:= tokenIndex + 1;
    Result:= tokenIndex;
  end;

  function findNextToken: NSUInteger;
  var
    tokenRange: NSRange;
    tokenIndex: NSUInteger;
  begin
    tokenRange.location:= caretIndex;
    tokenRange.length:= editorString.length - caretIndex;
    tokenRange:= editorString.rangeOfString_options_range(
      NSSTR_ATTACHMENT_CHARACTER, 0, tokenRange );
    tokenIndex:= tokenRange.location;
    if tokenIndex = NSNotFound then
      tokenIndex:= editorString.length;
    Result:= tokenIndex;
  end;

  function calcRange( beginIndex: NSUInteger; endIndex: NSUInteger ): NSRange;
  begin
    if endIndex > beginIndex then begin
      Result.location:= beginIndex;
      Result.length:= endIndex - beginIndex;
    end else begin
      Result.location:= NSNotFound;
      Result.length:= 0;
    end;
  end;
begin
  editor:= self.currentEditor;
  editorString:= editor.string_;
  caretIndex:= editor.selectedRange.location;
  Result:= calcRange( findLastToken, findNextToken );
end;

{ TFinderTagsListView }

function TFinderTagsListView.init: id;
begin
  Result:= inherited init;
  self.setTarget( self );
  self.setAction( objcselector('onTagSelected:') ) ;
  self.setDoubleAction( self.action ) ;
end;

procedure TFinderTagsListView.drawRow_clipRect(row: NSInteger; clipRect: NSRect
  );
var
  cellRect: NSRect;
  tagName: NSString;

  procedure drawSelectedBackground;
  var
    rect: NSRect;
    path: NSBezierPath;
  begin
    if NOT self.isRowSelected(row) then
      Exit;
    NSColor.alternateSelectedControlColor.set_;
    rect:= self.rectOfRow( row );
    rect:= NSInsetRect( rect, 10, 0 );
    path:= NSBezierPath.bezierPathWithRoundedRect_xRadius_yRadius( rect, 4, 4 );
    path.fill;
  end;

  procedure drawTagColor;
  var
    finderTag: TFinderTag;
    rect: NSRect;
    path: NSBezierPath;
  begin
    rect:= cellRect;
    rect.size.width:= rect.size.height;
    rect:= NSInsetRect( rect, 5, 5 );
    finderTag:= TFinderTags.getTagOfName( tagName );
    dotFinderTagNSColors[finderTag.colorIndex].set_;
    if finderTag.colorIndex <> 0 then begin
      path:= NSBezierPath.bezierPathWithOvalInRect( rect );
      path.fill;
    end else begin
      rect:= NSInsetRect( rect, 0.5, 0.5 );
      path:= NSBezierPath.bezierPathWithOvalInRect( rect );
      path.stroke;
    end;
  end;

  procedure drawTagName;
  var
    color: NSColor;
    rect: NSRect;
  begin
    if self.isRowSelected(row) then begin
      color:= NSColor.controlColor;
    end else begin
      color:= NSColor.textColor;
    end;

    rect:= cellRect;
    rect.origin.x:= rect.origin.x + 20;
    rect.size.width:= rect.size.width - 20;
    rect.origin.y:= rect.origin.y + 14;

    uDarwinFinderUtil.drawTagName( tagName, color, rect );
  end;

begin
  tagName:= NSTableViewDataSourceProtocol(self.datasource).tableView_objectValueForTableColumn_row(
    self, nil, row );
  cellRect:= frameOfCellAtColumn_row( 0, row );

  drawSelectedBackground;
  drawTagColor;
  drawTagName;
end;

function TFinderTagsListView.acceptsFirstResponder: ObjCBOOL;
begin
  Result:= False;
end;

procedure TFinderTagsListView.onTagSelected(sender: id);
begin
  TFinderTagsEditorPanel(self.dataSource).insertCurrentFilterToken;
end;

{ TFinderTagsEditorPanel }

class function TFinderTagsEditorPanel.editorWithPath( const path: NSString ): id;
begin
  Result:= TFinderTagsEditorPanel.alloc.initWithPath( path );
end;

function TFinderTagsEditorPanel.initWithPath( const path: NSString ): id;
begin
  _url:= NSURL.fileURLWithPath( path );
  _url.retain;
  _filterTagNames:= NSMutableArray.new;
  Result:= self;

  TFinderTags.update;
end;

procedure TFinderTagsEditorPanel.dealloc;
begin
  _tagsTokenField.release;
  _url.release;
  _filterTagNames.release;
  Inherited;
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
  scrollView: NSScrollView;
  column: NSTableColumn;
begin
  contentRect.origin.x:= 0;
  contentRect.origin.y:= 0;
  contentRect.size.Width:= 262;
  contentRect.size.Height:= 300;
  contentView:= NSView.alloc.initWithFrame( contentRect );
  controller:= NSViewController.new;
  controller.setView( contentView );

  tagNameArray:= uDarwinFinderUtil.getTagNamesOfFile( _url );
  contentRect:= NSInsetRect( contentRect, 6, 6 );
  contentRect.origin.x:= 6;
  contentRect.origin.y:= 190;
  contentRect.size.Width:= 250;
  contentRect.size.Height:= 100;
  NSTokenField.setCellClass( TCocoaTokenFieldCell );
  _tagsTokenField:= TCocoaTokenField.alloc.initWithFrame( contentRect );
  _tagsTokenField.setDelegate( NSTokenFieldDelegateProtocol(self) );
  _tagsTokenField.setObjectValue( tagNameArray );
  _tagsTokenField.setFocusRingType( NSFocusRingTypeNone );
  contentView.addSubview( _tagsTokenField );

  contentRect.origin.x:= 0;
  contentRect.origin.y:= 14;
  contentRect.size.Width:= 262;
  contentRect.size.Height:= 170;
  scrollView:= NSScrollView.alloc.initWithFrame( contentRect );
  _filterListView:= TFinderTagsListView.new;
  _filterListView.setIntercellSpacing( NSZeroSize );
  column:= NSTableColumn.new;
  column.setWidth( 256 );
  _filterListView.addTableColumn( column );
  column.release;
  _filterListView.setStyle( NSTableViewStyleAutomatic );
  _filterListView.setRowHeight( 20 );
  _filterListView.setFocusRingType( NSFocusRingTypeNone );
  _filterListView.setDataSource( self );
  _filterListView.setHeaderView( nil );
  _filterListView.setBackgroundColor( NSColor.clearColor );
  scrollView.setDocumentView( _filterListView );
  scrollView.setAutohidesScrollers( True );
  scrollView.setHasVerticalScroller( True );
  scrollView.setDrawsBackground( False );
  contentView.addSubview( scrollView );

  popover:= NSPopover.new;
  popover.setContentViewController( controller );
  popover.setDelegate( self );
  popover.setBehavior( NSPopoverBehaviorTransient );

  popover.showRelativeToRect_ofView_preferredEdge(
    sender.bounds,
    sender,
    edge );

  NSControlMoveCaretToTheEnd( _tagsTokenField );
  self.updateFilter( nil );

  scrollView.release;
  contentView.release;
  controller.release;
  popover.release;
end;

function TFinderTagsEditorPanel.numberOfRowsInTableView(tableView: NSTableView
  ): NSInteger;
begin
  Result:= _filterTagNames.count;
end;

function TFinderTagsEditorPanel.tableView_objectValueForTableColumn_row(
  tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): id;
begin
  Result:= _filterTagNames.objectAtIndex( row );
end;

procedure TFinderTagsEditorPanel.insertCurrentFilterToken;
begin
  _tagsTokenField.currentEditor.doCommandBySelector( ObjCSelector('insertNewline:') );
end;

procedure TFinderTagsEditorPanel.updateFilter( substring: NSString );
var
  tagName: NSString;
  usedTagNames: NSArray;
begin
  _filterTagNames.removeAllObjects;
  usedTagNames:= _tagsTokenField.objectValue;
  for tagName in TFinderTags._tags do begin
    if (substring<>nil) and (NOT tagName.containsString(substring)) then
      continue;
    if usedTagNames.containsObject(tagName) then
      continue;
    _filterTagNames.addObject( tagName );
  end;

  _filterListView.reloadData;
end;

function TFinderTagsEditorPanel.control_textView_doCommandBySelector(
  control: NSControl; textView: NSTextView; commandSelector: SEL): ObjCBOOL;

  procedure insertTokenName;
  var
    tokenName: NSString;
  begin
    if _filterListView.selectedRow < 0 then
      Exit;
    tokenName:= _filterTagNames.objectAtIndex(_filterListView.selectedRow);
    _tagsTokenField.insertTokenNameReplaceEditingString( tokenName );
  end;

begin
  Result:= False;
  if commandSelector = ObjCSelector('cancelOperation:') then begin
    _cancel:= True;
  end else if commandSelector = ObjCSelector('insertNewline:') then begin
    insertTokenName;
  end else if (commandSelector=ObjCSelector('moveDown:')) or
              (commandSelector=ObjCSelector('moveUp:')) then begin
    _filterListView.keyDown( NSApp.currentEvent );
    Result:= True;
  end;
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

class procedure uDarwinFinderUtil.drawTagName( tagName: NSString; color: NSColor; rect: NSRect );
var
  attributes: NSMutableDictionary;
  ps: NSMutableParagraphStyle;
begin
  ps:= NSMutableParagraphStyle.new;
  ps.setLineBreakMode( NSLineBreakByTruncatingTail );

  attributes:= NSMutableDictionary.new;
  attributes.setValue_forKey( color, NSForegroundColorAttributeName );
  attributes.setValue_forKey( ps, NSParagraphStyleAttributeName );

  tagName.drawWithRect_options_attributes( rect, 0, attributes );

  ps.release;
  attributes.release;
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
  rectFinderTagNSColors:= [
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.656, 0.656, 0.656, 0.5 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.656, 0.656, 0.656, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.699, 0.836, 0.266, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.746, 0.547, 0.844, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.340, 0.629, 0.996, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.934, 0.852, 0.266, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.980, 0.383, 0.348, 1 ).retain,
    NSColor.colorWithCalibratedRed_green_blue_alpha( 0.961, 0.660, 0.254, 1 ).retain
  ];

  dotFinderTagNSColors:= [
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
  initFinderTagNSColors;

end.

