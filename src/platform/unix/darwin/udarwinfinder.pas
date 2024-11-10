unit uDarwinFinder;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, LCLType, Menus,
  uLng,
  uDarwinFinderModel,
  MacOSAll, CocoaAll, CocoaConst, CocoaTextEdits, CocoaUtils, Cocoa_Extra;

const
  FINDER_FAVORITE_TAGS_MENU_ITEM_SIZE = 20.0;
  FINDER_FAVORITE_TAGS_MENU_ITEM_SPACING = 4.0;

  FINDER_TAGS_MENU_ROUND_SIZE = 12;

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
  TFinderTagMenuRoundImages = Array of NSImage;

  TFinderEditorCloseHandler = procedure ( const cancel: Boolean; const tagNames: NSArray ) of object;

  { uDarwinFinderUtil }

  uDarwinFinderUtil = class
  strict private class var
    _menuTagRoundImages: TFinderTagMenuRoundImages;
  public
    class procedure popoverFileTagsEditor(
      const path: String; onClose: TFinderEditorCloseHandler;
      const positioningView: NSView; const edge: NSRectEdge );
    class procedure popoverTagsSelector(
      const title: String; onClose: TFinderEditorCloseHandler;
      const positioningView: NSView; const edge: NSRectEdge );

    class function attachFinderTagsMenu( const path: String;
      const lclMenu: TPopupMenu; const menuIndex: Integer ): Boolean;
    class procedure attachSearchForTagsMenu( const lclMenu: TMenu );
  private
    class procedure drawTagName( const tagName: NSString;
      const fontSize: CGFloat; const color: NSColor; const rect: NSRect );
    class procedure initMenuRoundNSImages;
  end;

implementation

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
    _popover: NSPopover;
    _titleLabel: NSTextField;
    _tagsTokenField: TCocoaTokenField;
    _filterListView: NSTableView;
    _filterTagNames: NSMutableArray;
    _cancel: Boolean;
    _onClose: TFinderEditorCloseHandler;
  public
    class function editorWithPath( const path: NSString ): id; message 'doublecmd_editorWithPath:';
    class function selectorWithTitle( const titleString: NSString ): id; message 'doublecmd_editorWithTitle:';
    function init: id; override;
    procedure dealloc; override;
    procedure showPopover( const sender: NSView ; const edge: NSRectEdge );
      message 'doublecmd_showPopover:sender:';
  public
    procedure loadView; override;
    procedure setTitle(newValue: NSString); override;
    procedure setTagNames( tagNames: NSArray ); message 'doublecmd_setTagNames:';
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

  TFinderFavoriteTagsMenuView = objcclass;

  { TFinderFavoriteTagMenuItem }

  TFinderFavoriteTagMenuItem = objcclass( NSView )
  private
    _finderTag: TFinderTag;
    _trackingArea: NSTrackingArea;
    _using: Boolean;
    _hover: Boolean;
  public
    procedure setFinderTag( const finderTag: TFinderTag ); message 'doublecmd_setFinderTag:';
    function finderTag: TFinderTag; message 'doublecmd_finderTag';
    procedure setUsing( const using: Boolean ); message 'doublecmd_setUsing:';
    function using: Boolean; message 'doublecmd_using';
  public
    procedure dealloc; override;
    procedure updateTrackingAreas; override;
    procedure drawRect(dirtyRect: NSRect); override;
    procedure mouseEntered(theEvent: NSEvent); override;
    procedure mouseExited(theEvent: NSEvent); override;
    procedure mouseUp(theEvent: NSEvent); override;
  private
    function actionHandler: TFinderFavoriteTagsMenuView;
      message 'doublecmd_actionHandler';
  end;

  { TFinderFavoriteTagsMenuView }

  TFinderFavoriteTagsMenuView = objcclass( NSView )
  private
    _lclMenu: TPopupMenu;
    _lclMateMenuItem: TMenuItem;
    _favoriteTags: NSArray;
    _url: NSURL;
  public
    procedure setLclMenu( const lclMenu: TPopupMenu; const lclMateMenuItem: TMenuItem );
      message 'doublecmd_setLclMenu:lclMenu:';
    procedure setPath( const path: NSString );
      message 'doublecmd_setPath:';
    procedure setFavoriteTags( const favoriteTags: NSArray );
      message 'doublecmd_setFavoriteTags:';
  private
    procedure onTagMenuItemHoverChanged( tagMenuItem: TFinderFavoriteTagMenuItem );
      message 'doublecmd_onTagMenuItemHoverChanged:';
    procedure onTagMenuItemSelected( tagMenuItem: TFinderFavoriteTagMenuItem );
      message 'doublecmd_onTagMenuItemSelected:';
  public
    procedure dealloc; override;
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
    color:= uDarwinFinderModelUtil.rectFinderTagNSColors[0];

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
      if tokenName.localizedCaseInsensitiveCompare(tokenNames.objectAtIndex(i)) = NSOrderedSame then begin
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
  newStyle: Boolean;

  procedure drawSelectedBackground;
  var
    rect: NSRect;
    path: NSBezierPath;
  begin
    if NOT self.isRowSelected(row) then
      Exit;
    NSColor.alternateSelectedControlColor.set_;
    rect:= self.rectOfRow( row );
    if newStyle then begin
      rect:= NSInsetRect( rect, 10, 0 );
      path:= NSBezierPath.bezierPathWithRoundedRect_xRadius_yRadius( rect, 5, 5 );
      path.fill;
    end else begin
      NSRectFill( rect );
    end;
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
    if NOT newStyle then
      rect.origin.x:= rect.origin.x + 10;
    finderTag:= TFinderTags.getTagOfName( tagName );
    uDarwinFinderModelUtil.dotFinderTagNSColors[finderTag.colorIndex].set_;
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
    if newStyle then begin
      rect.origin.x:= rect.origin.x + 20;
      rect.size.width:= rect.size.width - 20;
    end else begin
      rect.origin.x:= rect.origin.x + 30;
      rect.size.width:= rect.size.width - 40;
    end;
    rect.origin.y:= rect.origin.y + TAG_LIST_FONT_SIZE + 2;

    uDarwinFinderUtil.drawTagName( tagName, TAG_LIST_FONT_SIZE, color, rect );
  end;

begin
  newStyle:= True;
  if NSAppKitVersionNumber < NSAppKitVersionNumber11_0 then
    newStyle:= false
  else if self.style = NSTableViewStylePlain then
    newStyle:= false;

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
  url: NSURL;
begin
  url:= NSURL.fileURLWithPath( path );
  // release in popoverDidClose()
  panel:= TFinderTagsEditorPanel.new;
  panel.loadView;
  panel.setTitle( url.lastPathComponent );
  panel.setTagNames( uDarwinFinderModelUtil.getTagNamesOfFile(url) );
  Result:= panel;
end;

class function TFinderTagsEditorPanel.selectorWithTitle( const titleString: NSString ): id;
var
  panel: TFinderTagsEditorPanel;
begin
  // release in popoverDidClose()
  panel:= TFinderTagsEditorPanel.new;
  panel.loadView;
  panel.setTitle( titleString );
  Result:= panel;
end;

function TFinderTagsEditorPanel.init: id;
begin
  Inherited init;

  _filterTagNames:= NSMutableArray.new;
  Result:= self;

  TFinderTags.update;
end;

procedure TFinderTagsEditorPanel.dealloc;
begin
  _titleLabel.release;
  _tagsTokenField.release;
  _filterTagNames.release;
  _popover.release;
  Inherited;
end;

procedure TFinderTagsEditorPanel.popoverDidClose(notification: NSNotification);
begin
  _popover.setDelegate( nil );
  _popover.setContentViewController( nil );
  self.release;
end;

procedure TFinderTagsEditorPanel.showPopover( const sender: NSView; const edge: NSRectEdge );
begin
  self.view.setFrameSize( NSMakeSize(TAG_POPOVER_WIDTH, TAG_POPOVER_HEIGHT) );

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

  _titleLabel:= NSTextField.new;
  _titleLabel.setHidden( False );
  _titleLabel.setAlignment( NSTextAlignmentCenter );
  _titleLabel.setLineBreakMode( NSLineBreakByTruncatingMiddle );
  _titleLabel.setEditable( False );
  _titleLabel.setBordered( False );
  _titleLabel.setBackgroundColor( NSColor.clearColor );
  _titleLabel.setFocusRingType( NSFocusRingTypeNone );
  contentView.addSubview( _titleLabel );

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

procedure TFinderTagsEditorPanel.setTitle(newValue: NSString);
begin
  _titleLabel.setStringValue( newValue );
end;

procedure TFinderTagsEditorPanel.setTagNames(tagNames: NSArray);
begin
  _tagsTokenField.setObjectValue( tagNames );
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

  for tagName in TFinderTags.tags do begin
    if (substring.length>0) and (NOT tagName.localizedCaseInsensitiveContainsString(substring)) then
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
  _titleLabel.setFrame( pathLabelFrame );

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
  tagNames: NSArray;
begin
  if _onClose = nil then
    Exit;

  tagNames:= nil;
  if NOT _cancel then
    tagNames:= _tagsTokenField.objectValue;

  _onClose( _cancel, tagNames );
end;

{ uDarwinFinderUtil }

class procedure uDarwinFinderUtil.popoverFileTagsEditor(
  const path: String; onClose: TFinderEditorCloseHandler;
  const positioningView: NSView ; const edge: NSRectEdge );
var
  panel: TFinderTagsEditorPanel;
begin
  panel:= TFinderTagsEditorPanel.editorWithPath( StrToNSString(path) );
  panel._onClose:= onClose;
  panel.showPopover( positioningView, edge );
end;

class procedure uDarwinFinderUtil.popoverTagsSelector(
  const title: String; onClose: TFinderEditorCloseHandler;
  const positioningView: NSView; const edge: NSRectEdge);
var
  panel: TFinderTagsEditorPanel;
begin
  panel:= TFinderTagsEditorPanel.selectorWithTitle( StrToNSString(title) );
  panel._onClose:= onClose;
  panel.showPopover( positioningView, edge );
end;

class function uDarwinFinderUtil.attachFinderTagsMenu( const path: String;
  const lclMenu: TPopupMenu; const menuIndex: Integer ): Boolean;
var
  menuView: TFinderFavoriteTagsMenuView;
  cocoaItem: NSMenuItem;
  favoriteTags: NSArray;
begin
  Result:= False;

  favoriteTags:= uDarwinFinderModelUtil.favoriteTags;
  if favoriteTags = nil then
    Exit;

  menuView:= TFinderFavoriteTagsMenuView.alloc.initWithFrame(
    NSMakeRect( 0, 0,
      200,
      FINDER_FAVORITE_TAGS_MENU_ITEM_SIZE + FINDER_FAVORITE_TAGS_MENU_ITEM_SPACING*2 ) );
  menuView.setLclMenu( lclMenu, lclMenu.Items[menuIndex+1] );
  menuView.setPath( StrToNSString(path) );
  menuView.setFavoriteTags( favoriteTags );

  cocoaItem:= NSMenuItem( lclMenu.Items[menuIndex].Handle );
  cocoaItem.setView( menuView );

  menuView.release;
  Result:= True;
end;

class procedure uDarwinFinderUtil.attachSearchForTagsMenu(const lclMenu: TMenu);
  procedure setColorImage( const lclMenuItem: TMenuItem );
  var
    cocoaItem: NSMenuItem;
    tag: TFinderTag;
  begin
    cocoaItem:= NSMenuItem( lclMenuItem.Handle );
    tag:= TFinderTags.getTagOfName( cocoaItem.title );
    if tag = nil then
      Exit;
    cocoaItem.setImage( _menuTagRoundImages[tag.colorIndex] );
  end;

var
  i: Integer;
  count: Integer;
begin
  count:= lclMenu.Items.Count;
  for i:=0 to count-1 do begin
    setColorImage( lclMenu.Items[i] );
  end;
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

class procedure uDarwinFinderUtil.initMenuRoundNSImages;
var
  imageSize: NSSize;
  colors: TFinderTagNSColors;
  count: Integer;
  i: Integer;

  procedure drawImageContent( const color: NSColor );
  var
    rect: NSRect;
    path: NSBezierPath;
  begin
    color.set_;
    rect.origin:= NSZeroPoint;
    rect.size:= imageSize;
    path:= NSBezierPath.bezierPathWithOvalInRect( rect );
    path.fill;
  end;

  function createOneColorImage( const color: NSColor ): NSImage;
  var
    image: NSImage;
    imageRep: NSBitmapImageRep;
    context: NSGraphicsContext;
  begin
    imageRep:= NSBitmapImageRep.alloc.initWithBitmapDataPlanes_pixelsWide_pixelsHigh__colorSpaceName_bytesPerRow_bitsPerPixel(
      nil,
      FINDER_TAGS_MENU_ROUND_SIZE, FINDER_TAGS_MENU_ROUND_SIZE,
      8, 4,
      True, False,
      NSCalibratedRGBColorSpace,
      0, 0 );
    context:= NSGraphicsContext.graphicsContextWithBitmapImageRep( imageRep );
    NSGraphicsContext.classSaveGraphicsState;
    NSGraphicsContext.setCurrentContext( context );
    drawImageContent( color );
    image:= NSImage.alloc.initWithSize( imageSize );
    image.addRepresentation( imageRep );
    Result:= image;
    NSGraphicsContext.classRestoreGraphicsState;
  end;

begin
  imageSize:= NSMakeSize( FINDER_TAGS_MENU_ROUND_SIZE, FINDER_TAGS_MENU_ROUND_SIZE );
  colors:= uDarwinFinderModelUtil.rectFinderTagNSColors;
  count:= Length( colors );
  SetLength( _menuTagRoundImages, count );
  for i:= 0 to count-1 do begin
    _menuTagRoundImages[i]:= createOneColorImage( colors[i] );
  end;
end;

{ TFinderFavoriteTagMenuItemControl }

procedure TFinderFavoriteTagMenuItem.setFinderTag(
  const finderTag: TFinderTag);
begin
  _finderTag:= finderTag;
end;

function TFinderFavoriteTagMenuItem.finderTag: TFinderTag;
begin
  Result:= _finderTag;
end;

procedure TFinderFavoriteTagMenuItem.setUsing(const using: Boolean);
begin
  _using:= using;
end;

function TFinderFavoriteTagMenuItem.using: Boolean;
begin
  Result:= _using;
end;

procedure TFinderFavoriteTagMenuItem.dealloc;
begin
  if Assigned(_trackingArea) then begin
    self.removeTrackingArea( _trackingArea );
    _trackingArea.release;
  end;
end;

procedure TFinderFavoriteTagMenuItem.updateTrackingAreas;
const
  options: NSTrackingAreaOptions = NSTrackingMouseEnteredAndExited
                                or NSTrackingActiveAlways;
begin
  if Assigned(_trackingArea) then begin
    self.removeTrackingArea( _trackingArea );
    _trackingArea.release;
  end;

  _trackingArea:= NSTrackingArea.alloc.initWithRect_options_owner_userInfo(
    self.bounds,
    options,
    self,
    nil );
  self.addTrackingArea( _trackingArea );
end;

procedure TFinderFavoriteTagMenuItem.drawRect(dirtyRect: NSRect);
  procedure drawCircle;
  var
    rect: NSRect;
    path: NSBezierPath;
  begin
    rect:= NSMakeRect( 0, 0, FINDER_FAVORITE_TAGS_MENU_ITEM_SIZE, FINDER_FAVORITE_TAGS_MENU_ITEM_SIZE );
    if NOT _hover then
      rect:= NSInsetRect( rect, 2, 2 );

    _finderTag.color.set_;
    path:= NSBezierPath.bezierPathWithOvalInRect( rect );
    path.fill;
  end;

  procedure drawState;
  var
    stateString: NSString;
    stateRect: NSRect;
    stateFontSize: CGFloat;
    attributes: NSMutableDictionary;
  begin
    stateRect:= NSMakeRect( 5, 6, FINDER_FAVORITE_TAGS_MENU_ITEM_SIZE, FINDER_FAVORITE_TAGS_MENU_ITEM_SIZE );
    stateFontSize:= 11;
    if _hover then begin
      if _using then begin
        stateString:= StrToNSString( 'x' );
        stateRect.origin.x:= stateRect.origin.x + 1;
        stateFontSize:= 14;
      end else begin
        stateString:= StrToNSString( '+' );
        stateFontSize:= 15;
      end;
    end else begin
      if _using then
        stateString:= StrToNSString( '✓' )
      else
        stateString:= nil;
    end;

    if stateString = nil then
      Exit;

    attributes:= NSMutableDictionary.new;
    attributes.setValue_forKey( NSFont.systemFontOfSize(stateFontSize), NSFontAttributeName );
    attributes.setValue_forKey( NSColor.whiteColor, NSForegroundColorAttributeName );

    stateString.drawWithRect_options_attributes( stateRect, 0, attributes );

    attributes.release;
  end;

begin
  drawCircle;
  drawState;
end;

procedure TFinderFavoriteTagMenuItem.mouseEntered(theEvent: NSEvent);
begin
  inherited mouseEntered(theEvent);
  _hover:= True;
  self.setNeedsDisplay_( True );
  self.superview.setNeedsDisplay_( True );
  self.actionHandler.onTagMenuItemHoverChanged( self );
end;

procedure TFinderFavoriteTagMenuItem.mouseExited(theEvent: NSEvent);
begin
  inherited mouseExited(theEvent);
  _hover:= False;
  self.setNeedsDisplay_( True );
  self.superview.setNeedsDisplay_( True );
  self.actionHandler.onTagMenuItemHoverChanged( nil );
end;

procedure TFinderFavoriteTagMenuItem.mouseUp(theEvent: NSEvent);
begin
  self.actionHandler.onTagMenuItemSelected( self );
end;

function TFinderFavoriteTagMenuItem.actionHandler: TFinderFavoriteTagsMenuView;
begin
  Result:= TFinderFavoriteTagsMenuView( self.superview );
end;

{ TFinderFavoriteTagsMenuView }

procedure TFinderFavoriteTagsMenuView.setLclMenu( const lclMenu: TPopupMenu;
  const lclMateMenuItem: TMenuItem );
begin
  _lclMenu:= lclMenu;
  _lclMateMenuItem:= lclMateMenuItem;
end;

procedure TFinderFavoriteTagsMenuView.setPath(const path: NSString);
begin
  _url:= NSURL.alloc.initFileURLWithPath( path );
end;

procedure TFinderFavoriteTagsMenuView.setFavoriteTags(const favoriteTags: NSArray
  );
var
  finderTag: TFinderTag;
  itemControl: TFinderFavoriteTagMenuItem;
  itemRect: NSRect;
  fileTagNames: NSArray;

  function createItemControl: TFinderFavoriteTagMenuItem;
  var
    using: Boolean;
  begin
    using:= fileTagNames.containsObject( finderTag.name );
    Result:= TFinderFavoriteTagMenuItem.alloc.initWithFrame( itemRect );
    Result.setFinderTag( finderTag );
    Result.setUsing( using );
  end;

  procedure createSubviews;
  begin
    fileTagNames:= uDarwinFinderModelUtil.getTagNamesOfFile( _url );
    itemRect:= NSMakeRect(
      16,
      FINDER_FAVORITE_TAGS_MENU_ITEM_SPACING,
      FINDER_FAVORITE_TAGS_MENU_ITEM_SIZE + FINDER_FAVORITE_TAGS_MENU_ITEM_SPACING,
      FINDER_FAVORITE_TAGS_MENU_ITEM_SIZE );
    for finderTag in _favoriteTags do begin
      itemControl:= createItemControl;
      self.addSubview( itemControl );
      itemControl.release;
      itemRect.origin.x:= itemRect.origin.x + FINDER_FAVORITE_TAGS_MENU_ITEM_SIZE + FINDER_FAVORITE_TAGS_MENU_ITEM_SPACING;
    end;
  end;

begin
  _favoriteTags:= favoriteTags;
  createSubviews;
end;

procedure TFinderFavoriteTagsMenuView.onTagMenuItemHoverChanged(
  tagMenuItem: TFinderFavoriteTagMenuItem );
var
  mateCaption: String;
begin
  if tagMenuItem <> nil then begin
    mateCaption:= tagMenuItem.finderTag.name.UTF8String;
    if tagMenuItem.using then
      mateCaption:= Format( rsMenuMacOSRemoveFinderTag, [mateCaption] )
    else
      mateCaption:= Format( rsMenuMacOSAddFinderTag, [mateCaption] );
  end
  else
    mateCaption:= rsMenuMacOSEditFinderTags;
  _lclMateMenuItem.Caption:= mateCaption;
end;

procedure TFinderFavoriteTagsMenuView.onTagMenuItemSelected(tagMenuItem: TFinderFavoriteTagMenuItem);
var
  tagName: NSString;
begin
  tagName:= tagMenuItem.finderTag.name;
  Writeln( tagName.utf8string );
  if tagMenuItem.using then
    uDarwinFinderModelUtil.removeTagForFile( _url, tagName )
  else
    uDarwinFinderModelUtil.addTagForFile( _url, tagName );
  NSMenu(_lclMenu.Handle).cancelTracking;
end;

procedure TFinderFavoriteTagsMenuView.dealloc;
begin
  Inherited;
  _url.release;
end;

initialization
  uDarwinFinderUtil.initMenuRoundNSImages;

end.

