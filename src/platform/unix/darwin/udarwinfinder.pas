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
  public
    class procedure popoverTags(
      const tagArray: NSArray; const positioningView: NSView; const edge: NSRectEdge );
    class function getTagsOfFile(
      const path: String ): NSArray;
  end;

implementation

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

class procedure uDarwinFinderUtil.popoverFileTags(
  const path: String; const positioningView: NSView ; const edge: NSRectEdge );
var
  tagArray: NSArray;
begin
  tagArray:= uDarwinFinderUtil.getTagsOfFile( path );
  uDarwinFinderUtil.popoverTags( tagArray, positioningView, edge );
end;

class function uDarwinFinderUtil.getTagsOfFile( const path: String ): NSArray;
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

end.

