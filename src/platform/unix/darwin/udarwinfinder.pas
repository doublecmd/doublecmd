unit uDarwinFinder;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, LCLType,
  sqldb, SQLite3Conn,
  MacOSAll, CocoaAll, CocoaConst, CocoaTextEdits, CocoaUtils;

const
  TAG_POPOVER_WIDTH = 228.0;
  TAG_POPOVER_HEIGHT = 303.0;
  TAG_POPOVER_PADDING = 6.0;

  TAG_LABEL_FONT_SIZE = 12.0;
  TAG_LIST_FONT_SIZE = 12.0;
  TAG_TOKEN_FONT_SIZE = 11.0;

  TAG_TOKEN_HORZ_EXTENSION = 4.0;
  TAG_TOKEN_COL_SPACING = 2.0;
  TAG_TOKEN_LINE_SPACING = 1.0;

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
    class procedure drawTagName( const tagName: NSString;
      const fontSize: CGFloat; const color: NSColor; const rect: NSRect );
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
    function isSelected: Boolean; message 'doublecmd_isSelected';
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
    procedure tokenField_onUpdate( editingString: NSString = nil );
      message 'doublecmd_tokenField_onUpdate:';
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
    function intrinsicContentSize: NSSize; override;
    procedure insertTokenNameReplaceEditingString( const tokenName: NSString );
      message 'doublecmd_insertTokenNameReplaceEditingString:';
    function editingString: NSString;
      message 'doublecmd_editingString';
    function editingStringRange: NSRange;
      message 'doublecmd_editingStringRange';
    function isSelectedTokenObject( const anObject: id ): Boolean;
      message 'doublecmd_isSelectedTokenObject:';
  end;

  { TFinderTagsListView }

  TFinderTagsListView = objcclass( NSTableView )
    function init: id; override;
    procedure drawRow_clipRect (row: NSInteger; clipRect: NSRect); override;
    function acceptsFirstResponder: ObjCBOOL; override;
  private
    procedure onTagSelected( sender: id ); message 'doublecmd_onTagSelected:';
  end;

  { TFinderTagsEditorPanel }

  TFinderTagsEditorPanel = objcclass( NSViewController,
    NSPopoverDelegateProtocol,
    TCocoaTokenFieldDelegateProtocol,
    NSTableViewDataSourceProtocol )
  private
    _url: NSUrl;
    _popover: NSPopover;
    _pathLabel: NSTextField;
    _tagsTokenField: TCocoaTokenField;
    _filterListView: NSTableView;
    _filterTagNames: NSMutableArray;
    _cancel: Boolean;
  public
    class function editorWithPath( const path: NSString): id; message 'doublecmd_editorWithPath:';
    function initWithPath( const path: NSString): id; message 'doublecmd_initWithPath:';
    procedure dealloc; override;
    procedure showPopover( const sender: NSView ; const edge: NSRectEdge );
      message 'doublecmd_showPopover:sender:';
  public
    procedure loadView; override;
  public
    function numberOfRowsInTableView (tableView: NSTableView): NSInteger;
    function tableView_objectValueForTableColumn_row (
      tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): id;
  public
    procedure insertCurrentFilterToken; message 'doublecmd_insertCurrentFilterToken';
    procedure tokenField_onUpdate( editingString: NSString = nil );
  private
    function control_textView_doCommandBySelector (control: NSControl; textView: NSTextView; commandSelector: SEL): ObjCBOOL;
    procedure popoverWillClose(notification: NSNotification);
    procedure popoverDidClose (notification: NSNotification);
    procedure updateFilter( substring: NSString ); message 'doublecmd_updateFilter:';
    procedure updateLayout; message 'doublecmd_updateLayout';
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

  uDarwinFinderUtil.drawTagName( NSString(self.objectValue),
    TAG_TOKEN_FONT_SIZE, color, titleRect );
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
         3,
         3 );

  color.set_;
  path.fill;

  drawInteriorWithFrame_inView( cellFrame, controlView_ );
end;

function TCocoaTokenAttachmentCell.cellSizeForBounds(aRect: NSRect): NSSize;
var
  stringSize: NSSize;
  preferedWidth: CGFloat;
  maxWidth: CGFloat;
begin
  maxWidth:= self.controlView.bounds.size.width - 8;
  stringSize:= self.attributedStringValue.size;
  preferedWidth:= TAG_TOKEN_HORZ_EXTENSION + stringSize.width + TAG_TOKEN_HORZ_EXTENSION + TAG_TOKEN_COL_SPACING;
  if preferedWidth > maxWidth then
    preferedWidth:= maxWidth;

  Result.width:= preferedWidth;
  Result.height:= stringSize.height + TAG_TOKEN_LINE_SPACING;
end;

function TCocoaTokenAttachmentCell.drawingRectForBounds(theRect: NSRect
  ): NSRect;
begin
  Result.origin.x:= theRect.origin.x;
  Result.origin.y:= theRect.origin.y;
  Result.size.width:= theRect.size.width - TAG_TOKEN_COL_SPACING;
  Result.size.height:= theRect.size.height - TAG_TOKEN_LINE_SPACING;
end;

function TCocoaTokenAttachmentCell.titleRectForBounds(theRect: NSRect): NSRect;
begin
  Result.origin.x:= theRect.origin.x + TAG_TOKEN_HORZ_EXTENSION ;
  Result.origin.y:= theRect.origin.y + theRect.size.height - 4;
  Result.size.width:= theRect.size.width - TAG_TOKEN_HORZ_EXTENSION * 2;
  Result.size.height:= theRect.size.Height;
end;

{ TCocoaTokenFieldCell }

function TCocoaTokenFieldCell.setUpTokenAttachmentCell(
  aCell: NSTokenAttachmentCell; anObject: id): NSTokenAttachmentCell;
begin
  Result:= TCocoaTokenAttachmentCell.alloc.initTextCell( NSString(anObject) );
  Result.setFont( NSFont.systemFontOfSize(TAG_TOKEN_FONT_SIZE) );
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
  cocoaDelegate: TCocoaTokenFieldDelegateProtocol;
begin
  Inherited;

  cocoaDelegate:= TCocoaTokenFieldDelegateProtocol( self.delegate );
  cocoaDelegate.tokenField_onUpdate( self.editingString );
end;

function TCocoaTokenField.intrinsicContentSize: NSSize;
var
  rect: NSRect;
begin
  rect:= self.bounds;
  rect.size.width:= rect.size.width - 7;
  rect.size.height:= 1000;
  Result:= self.cell.cellSizeForBounds( rect );
  Result.width:= self.frame.size.width;
end;

procedure TCocoaTokenField.insertTokenNameReplaceEditingString(
  const tokenName: NSString);
var
  tokenNames: NSMutableArray;
  newTokenRange: NSRange;

  procedure removeEditingToken;
  begin
    if newTokenRange.length > 0 then
      tokenNames.removeObjectAtIndex( newTokenRange.location );
  end;

  procedure removeDuplicateToken;
  var
    i: NSUInteger;
  begin
    i:= 0;
    while i < tokenNames.count do begin
      if tokenName.isEqualTo( tokenNames.objectAtIndex(i) ) then begin
        tokenNames.removeObjectAtIndex( i );
        if i < newTokenRange.location then
          dec( newTokenRange.location );
      end else begin
        inc( i );
      end;
    end;
  end;

  procedure insertNewToken;
  begin
    tokenNames.insertObject_atIndex( tokenName, newTokenRange.location );
  end;

  procedure setCaretLocation( location: NSUInteger );
  var
    range: NSRange;
  begin
    range.location:= location;
    range.length:= 0;
    self.currentEditor.setSelectedRange( range );
  end;

begin
  tokenNames:= NSMutableArray.arrayWithArray( self.objectValue );
  newTokenRange:= self.editingStringRange;
  removeEditingToken;
  removeDuplicateToken;
  insertNewToken;
  self.setObjectValue( tokenNames );
  setCaretLocation( newTokenRange.location + 1 );
end;

function TCocoaTokenField.editingString: NSString;
var
  editorString: NSString;
begin
  editorString:= self.currentEditor.string_;
  Result:= editorString.substringWithRange( self.editingStringRange );
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
    Result.location:= beginIndex;
    Result.length:= endIndex - beginIndex;
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
  self.setAction( objcselector('doublecmd_onTagSelected:') ) ;
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
    path:= NSBezierPath.bezierPathWithRoundedRect_xRadius_yRadius( rect, 5, 5 );
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
      color:= NSColor.alternateSelectedControlTextColor;
    end else begin
      color:= NSColor.textColor;
    end;

    rect:= cellRect;
    rect.origin.x:= rect.origin.x + 20;
    rect.size.width:= rect.size.width - 20;
    rect.origin.y:= rect.origin.y + TAG_LIST_FONT_SIZE + 2;

    uDarwinFinderUtil.drawTagName( tagName, TAG_LIST_FONT_SIZE, color, rect );
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
var
  panel: TFinderTagsEditorPanel;
begin
  panel:= TFinderTagsEditorPanel.alloc.initWithPath( path );
  panel.loadView;
  Result:= panel;
end;

function TFinderTagsEditorPanel.initWithPath( const path: NSString ): id;
begin
  Inherited init;

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
  _popover.release;
  Inherited;
end;

procedure TFinderTagsEditorPanel.popoverDidClose(notification: NSNotification);
begin
  self.release;
end;

procedure TFinderTagsEditorPanel.showPopover( const sender: NSView; const edge: NSRectEdge );
begin
  self.view.setFrameSize( NSMakeSize(TAG_POPOVER_WIDTH, TAG_POPOVER_HEIGHT) );

  _tagsTokenField.setObjectValue( uDarwinFinderUtil.getTagNamesOfFile(_url) );
  _tagsTokenField.setFrameSize( NSMakeSize(TAG_POPOVER_WIDTH-TAG_POPOVER_PADDING*2,0) );

  _popover.showRelativeToRect_ofView_preferredEdge(
    sender.bounds,
    sender,
    edge );

  NSControlMoveCaretToTheEnd( _tagsTokenField );
  self.tokenField_onUpdate;
end;

procedure TFinderTagsEditorPanel.loadView;
var
  contentView: NSView;
  scrollView: NSScrollView;
begin
  contentView:= NSView.new;

  _pathLabel:= NSTextField.new;
  _pathLabel.setHidden( False );
  _pathLabel.setStringValue( _url.lastPathComponent  );
  _pathLabel.setAlignment( NSTextAlignmentCenter );
  _pathLabel.setLineBreakMode( NSLineBreakByTruncatingMiddle );
  _pathLabel.setEditable( False );
  _pathLabel.setBordered( False );
  _pathLabel.setBackgroundColor( NSColor.clearColor );
  _pathLabel.setFocusRingType( NSFocusRingTypeNone );
  contentView.addSubview( _pathLabel );

  NSTokenField.setCellClass( TCocoaTokenFieldCell );
  _tagsTokenField:= TCocoaTokenField.new;
  _tagsTokenField.setDelegate( NSTokenFieldDelegateProtocol(self) );
  _tagsTokenField.setFont( NSFont.systemFontOfSize(TAG_TOKEN_FONT_SIZE+1) );
  _tagsTokenField.setFocusRingType( NSFocusRingTypeNone );
  contentView.addSubview( _tagsTokenField );

  scrollView:= NSScrollView.alloc.initWithFrame( NSZeroRect );
  _filterListView:= TFinderTagsListView.new;
  _filterListView.setIntercellSpacing( NSZeroSize );
  _filterListView.addTableColumn( NSTableColumn.new.autorelease );
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
  scrollView.release;

  _popover:= NSPopover.new;
  _popover.setContentViewController( self );
  _popover.setDelegate( self );
  _popover.setBehavior( NSPopoverBehaviorTransient );

  self.setView( contentView );
  contentView.release;
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

procedure TFinderTagsEditorPanel.tokenField_onUpdate( editingString: NSString = nil );
begin
  self.updateFilter( editingString );
  self.updateLayout;
end;

procedure TFinderTagsEditorPanel.updateFilter( substring: NSString );
var
  tagName: NSString;
  usedTagNames: NSMutableArray;
  editingRange: NSRange;
begin
  _filterTagNames.removeAllObjects;
  usedTagNames:= NSMutableArray.arrayWithArray( _tagsTokenField.objectValue );
  editingRange:= _tagsTokenField.editingStringRange;
  if editingRange.length > 0 then
    usedTagNames.removeObjectAtIndex( editingRange.location );

  for tagName in TFinderTags._tags do begin
    if (substring.length>0) and (NOT tagName.containsString(substring)) then
      continue;
    if usedTagNames.containsObject(tagName) then
      continue;
    _filterTagNames.addObject( tagName );
  end;

  _filterListView.reloadData;
  _filterListView.deselectAll(nil);
end;

procedure TFinderTagsEditorPanel.updateLayout;
var
  pathLabelFrame: NSRect;
  tagsTokenFieldFrame: NSRect;
  filterListFrame: NSRect;
  popoverHeight: CGFloat;
begin
  popoverHeight:= _popover.contentSize.height;

  pathLabelFrame.size.width:= TAG_POPOVER_WIDTH - TAG_POPOVER_PADDING * 2;
  pathLabelFrame.size.height:= TAG_LABEL_FONT_SIZE + 3;
  pathLabelFrame.origin.x:= TAG_POPOVER_PADDING;
  pathLabelFrame.origin.y:= popoverHeight - ( pathLabelFrame.size.height + TAG_POPOVER_PADDING );
  _pathLabel.setFrame( pathLabelFrame );

  tagsTokenFieldFrame.size:= _tagsTokenField.intrinsicContentSize;
  if tagsTokenFieldFrame.size.height > 190 then
    tagsTokenFieldFrame.size.height:= 190;
  tagsTokenFieldFrame.origin.x:= TAG_POPOVER_PADDING;
  tagsTokenFieldFrame.origin.y:= pathLabelFrame.origin.y - ( tagsTokenFieldFrame.size.height + TAG_POPOVER_PADDING );

  filterListFrame.origin.x:= 0;
  filterListFrame.origin.y:= 10;
  filterListFrame.size.width:= TAG_POPOVER_WIDTH;
  filterListFrame.size.height:= tagsTokenFieldFrame.origin.y - ( filterListFrame.origin.y + TAG_POPOVER_PADDING );

  _tagsTokenField.setFrame( tagsTokenFieldFrame );
  _filterListView.enclosingScrollView.setFrame( filterListFrame );
  _filterListView.sizeToFit;
end;

function TFinderTagsEditorPanel.control_textView_doCommandBySelector(
  control: NSControl; textView: NSTextView; commandSelector: SEL): ObjCBOOL;

  function tryInsertTokenName: Boolean;
  var
    tokenName: NSString;
  begin
    Result:= False;
    if _filterListView.selectedRow >= 0 then
      tokenName:= _filterTagNames.objectAtIndex(_filterListView.selectedRow)
    else
      tokenName:= _tagsTokenField.editingString;
    if tokenName.length = 0 then
      Exit;
    _tagsTokenField.insertTokenNameReplaceEditingString( tokenName );
    self.tokenField_onUpdate;
    Result:= True;
  end;

  procedure closePopover;
  begin
    _popover.close;
  end;

  procedure handleEnter;
  begin
    if tryInsertTokenName = false then
      closePopover;
  end;

begin
  Result:= False;
  if commandSelector = ObjCSelector('cancelOperation:') then begin
    _cancel:= True;
  end else if commandSelector = ObjCSelector('insertNewline:') then begin
    handleEnter;
    Result:= true;
  end else if (commandSelector = ObjCSelector('moveDown:')) or
              (commandSelector = ObjCSelector('moveUp:')) then begin
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

class procedure uDarwinFinderUtil.drawTagName( const tagName: NSString;
  const fontSize: CGFloat; const color: NSColor; const rect: NSRect );
var
  attributes: NSMutableDictionary;
  ps: NSMutableParagraphStyle;
  font: NSFont;
begin
  ps:= NSMutableParagraphStyle.new;
  ps.setLineBreakMode( NSLineBreakByTruncatingTail );

  font:= NSFont.systemFontOfSize( fontSize );

  attributes:= NSMutableDictionary.new;
  attributes.setValue_forKey( font, NSFontAttributeName );
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

