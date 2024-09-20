unit uDarwinFinder;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, LCLType,
  MacOSAll, CocoaAll, CocoaUtils;

type

  { uDarwinFinderUtil }

  uDarwinFinderUtil = class
  public
    class procedure popoverFileTags(
      const path: String; const positioningView: NSView; const edge: NSRectEdge );
  private
    class function getTagNamesOfFile( const path: String ): NSArray;
  private
    class function getAllTags: NSDictionary;
  private
    class procedure popoverTags( const tagArray: NSArray; const positioningView: NSView; const edge: NSRectEdge );
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
  end;

  { TCocoaTokenFieldCell }

  TCocoaTokenFieldCell = objcclass( NSTokenFieldCell )
    function setUpTokenAttachmentCell( aCell: NSTokenAttachmentCell;
      anObject: id ): NSTokenAttachmentCell;
      message 'setUpTokenAttachmentCell:forRepresentedObject:';
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
  titleRect: NSRect;
begin
  finderTag:= TFinderTags.getTagOfName( self.stringValue );
  if finderTag <> nil then begin
    titleRect:= self.drawingRectForBounds( cellFrame );
    finderTag.color.set_;
    NSRectFill( titleRect );
  end;
  drawInteriorWithFrame_inView( cellFrame, controlView_ );
end;

{ TCocoaTokenFieldCell }

function TCocoaTokenFieldCell.setUpTokenAttachmentCell(
  aCell: NSTokenAttachmentCell; anObject: id): NSTokenAttachmentCell;
begin
  Result:= TCocoaTokenAttachmentCell.alloc.initTextCell( NSString(anObject) );
  Result.setRepresentedObject( anObject );
  Result.autorelease;
end;

{ uDarwinFinderUtil }

class procedure uDarwinFinderUtil.popoverFileTags(
  const path: String; const positioningView: NSView ; const edge: NSRectEdge );
var
  tagArray: NSArray;
begin
  tagArray:= uDarwinFinderUtil.getTagNamesOfFile( path );
  uDarwinFinderUtil.popoverTags( tagArray, positioningView, edge );
end;

class procedure uDarwinFinderUtil.popoverTags(
  const tagArray: NSArray; const positioningView: NSView ; const edge: NSRectEdge );
var
  contentRect: NSRect;
  popover: NSPopover;
  controller: NSViewController;
  contentView: NSView;
  tagsTokenField: NSTokenField;
begin
  contentRect.origin.x:= 0;
  contentRect.origin.y:= 0;
  contentRect.size.Width:= 250;
  contentRect.size.Height:= 100;
  contentView:= NSView.alloc.initWithFrame( contentRect );
  controller:= NSViewController.new;
  controller.setView( contentView );

  contentRect:= NSInsetRect( contentRect, 10, 10 );
  NSTokenField.setCellClass( TCocoaTokenFieldCell );
  tagsTokenField:= NSTokenField.alloc.initWithFrame( contentRect );
  tagsTokenField.setObjectValue( tagArray );
  tagsTokenField.setFocusRingType( NSFocusRingTypeNone );
  contentView.addSubview( tagsTokenField );

  popover:= NSPopover.new;
  popover.setContentViewController( controller );
  popover.setBehavior( NSPopoverBehaviorTransient );

  popover.showRelativeToRect_ofView_preferredEdge(
    positioningView.bounds,
    positioningView,
    edge );

  NSControlMoveCaretToTheEnd( tagsTokenField );

  tagsTokenField.release;
  contentView.release;
  controller.release;
  popover.release;
end;

class function uDarwinFinderUtil.getTagNamesOfFile( const path: String ): NSArray;
var
  url: NSUrl;
  success: Boolean;
  tags: NSArray;
begin
  Result:= nil;
  url:= NSUrl.fileURLWithPath( StrToNSString(path) );
  success:= url.getResourceValue_forKey_error( @tags, NSURLTagNamesKey, nil );
  if success then
    Result:= tags;
end;

class function uDarwinFinderUtil.getAllTags: NSDictionary;
begin
  Result:= nil;
end;

procedure initFinderTagNSColors;
begin
  defaultFinderTagNSColors:= [
    NSColor.windowBackgroundColor,
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

