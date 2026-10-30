unit uDarwinPanel;

{$mode delphi}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  Controls, Forms,
  CocoaAll, QuickLookUI, CocoaUtils, CocoaPrivate,
  uDarwinApplication, uDarwinFinderModel, uDarwinFinder;

type

  { TDarwinPanelUtil }

  TDarwinPanelUtil = class
  public
    class procedure showQuickLook;
    class procedure updateQuickLook;
    class function hideQuickLook: Boolean;
    class procedure showEditFinderTags( const Sender: id; const control: TWinControl );
    class procedure showSharingService;
    class procedure showAirDrop;
  end;

implementation

type

  { TQLPreviewPanelMate }

  TQLPreviewPanelMate = objcclass( NSObject, QLPreviewPanelDataSourceProtocol, QLPreviewPanelDelegateProtocol )
  private
    _urlArray: NSArray;
  public
    function numberOfPreviewItemsInPreviewPanel (panel: QLPreviewPanel): NSInteger;
    function previewPanel_previewItemAtIndex (panel: QLPreviewPanel; index: NSInteger): QLPreviewItemProtocol;
    function previewPanel_handleEvent (panel: QLPreviewPanel; event: NSEvent): ObjCBOOL;
      message 'previewPanel:handleEvent:';
  public
    function initWithItems( urlArray: NSArray ): id; message 'setUrlArray:';
    procedure dealloc; override;
  end;

function TQLPreviewPanelMate.numberOfPreviewItemsInPreviewPanel(
  panel: QLPreviewPanel): NSInteger;
begin
  Result:= _urlArray.count;
end;

function TQLPreviewPanelMate.previewPanel_previewItemAtIndex(panel: QLPreviewPanel;
  index: NSInteger): QLPreviewItemProtocol;
begin
  Result:= QLPreviewItemProtocol( _urlArray.objectAtIndex(index) );
end;

var
  // the file panel that gets the keys while the preview is shown
  quickLookFocusView: NSView = nil;

procedure setQuickLookFocusView( const responder: NSResponder );
begin
  if Assigned(quickLookFocusView) then
    quickLookFocusView.release;
  quickLookFocusView:= nil;
  if Assigned(responder) and responder.isKindOfClass(NSView) then begin
    quickLookFocusView:= NSView(responder);
    quickLookFocusView.retain;
  end;
end;

// The panel makes itself the key window when shown, and again when its content
// is loaded. Pass the keys on to the file panel, so its cursor keeps moving and
// the preview follows it. Escape is left to the panel to close it.
function TQLPreviewPanelMate.previewPanel_handleEvent(panel: QLPreviewPanel;
  event: NSEvent): ObjCBOOL;
const
  kVK_Escape = 53;
var
  callback: ICommonCallback;
begin
  Result:= False;
  if not ((event.type_ = NSKeyDown) or (event.type_ = NSKeyUp)) then
    Exit;
  if event.keyCode = kVK_Escape then
    Exit;
  if (quickLookFocusView = nil) or (quickLookFocusView.window = nil) then
    Exit;

  // the main window loses its first responder while the panel is shown,
  // restore it and make the main window key again, so that the file panel
  // counts as focused for the hotkeys (e.g. Tab) and the next keys go
  // directly to it
  quickLookFocusView.window.makeFirstResponder( quickLookFocusView );
  quickLookFocusView.window.makeKeyWindow;

  // LCL handles the keys in TCocoaApplication.sendEvent, before Cocoa does,
  // but the event was sent to the panel. Do the same for the file panel.
  callback:= quickLookFocusView.lclGetCallback;
  if Assigned(callback) then begin
    if callback.handleEventBeforeCocoa( event ) then
      quickLookFocusView.keyDown( event );
    callback.handleEventAfterCocoa( event );
  end;
  // the keys may move the focus, e.g. Tab to the other file panel
  if Assigned(quickLookFocusView.window) then
    setQuickLookFocusView( quickLookFocusView.window.firstResponder );
  Result:= True;
end;

function TQLPreviewPanelMate.initWithItems(urlArray: NSArray): id;
begin
  Result:= Inherited init;
  _urlArray:= urlArray;
  _urlArray.retain;
end;

procedure TQLPreviewPanelMate.dealloc;
begin
  _urlArray.release;
  Inherited;
end;

var
  quickLookMate: TQLPreviewPanelMate = nil;

procedure setQuickLookItems( const panel: QLPreviewPanel; const urlArray: NSArray );
var
  mate: TQLPreviewPanelMate;
begin
  mate:= TQLPreviewPanelMate.alloc.initWithItems( urlArray );
  panel.setDataSource( mate );
  panel.setDelegate( mate );
  // the panel retains neither its data source nor its delegate
  if Assigned(quickLookMate) then
    quickLookMate.release;
  quickLookMate:= mate;
end;

function isQuickLookVisible: Boolean;
begin
  Result:= Assigned(quickLookMate) and
           QLPreviewPanel.sharedPreviewPanelExists and
           QLPreviewPanel.sharedPreviewPanel.isVisible;
end;

class procedure TDarwinPanelUtil.showQuickLook;
var
  lclArray: TStringArray;
  panel: QLPreviewPanel;
begin
  // toggle, like Space in Finder
  if hideQuickLook then
    Exit;

  lclArray:= TDCCocoaApplication(NSApp).serviceMenuGetFilenames;
  if lclArray = nil then
    Exit;

  if Assigned(NSApp.keyWindow) then
    setQuickLookFocusView( NSApp.keyWindow.firstResponder )
  else
    setQuickLookFocusView( nil );

  panel:= QLPreviewPanel.sharedPreviewPanel;
  setQuickLookItems( panel, TCocoaCollectionUtil.urlArrayToNSArray(lclArray) );
  panel.makeKeyAndOrderFront( nil );
end;

class function TDarwinPanelUtil.hideQuickLook: Boolean;
begin
  Result:= isQuickLookVisible;
  if Result then
    QLPreviewPanel.sharedPreviewPanel.orderOut( nil );
end;

class procedure TDarwinPanelUtil.updateQuickLook;
var
  lclArray: TStringArray;
  urlArray: NSArray;
  panel: QLPreviewPanel;
begin
  if not isQuickLookVisible then
    Exit;

  lclArray:= TDCCocoaApplication(NSApp).serviceMenuGetFilenames;
  if lclArray = nil then
    Exit;

  urlArray:= TCocoaCollectionUtil.urlArrayToNSArray( lclArray );
  if urlArray.isEqualToArray( quickLookMate._urlArray ) then
    Exit;

  panel:= QLPreviewPanel.sharedPreviewPanel;
  setQuickLookItems( panel, urlArray );
  panel.reloadData;
end;

type

  { TFinderTagsEditorPanelHandler }

  TFinderTagsEditorPanelHandler = class
  private
    _urls: NSArray;
    _oldTagNames: NSArray;
  public
    constructor Create( const paths: TStringArray );
    destructor Destroy; override;
    procedure onClose( const cancel: Boolean; const newTagNames: NSArray );
  end;

constructor TFinderTagsEditorPanelHandler.Create( const paths: TStringArray );
begin
  _urls:= TCocoaCollectionUtil.urlArrayToNSArray( paths );
  _urls.retain;
  _oldTagNames:= TDarwinFinderModelUtil.getTagNamesOfFiles( _urls );
  _oldTagNames.retain;
end;

destructor TFinderTagsEditorPanelHandler.Destroy;
begin
  _oldTagNames.release;
  _urls.release;
end;

procedure TFinderTagsEditorPanelHandler.onClose( const cancel: Boolean; const newTagNames: NSArray );

  procedure processRemovedTags;
  var
    removedTagNames: NSMutableSet;
    tagName: NSString;
  begin
    removedTagNames:= NSMutableSet.setWithArray( _oldTagNames );
    removedTagNames.minusSet( NSSet.setWithArray(newTagNames) );
    for tagName in removedTagNames do begin
      TDarwinFinderModelUtil.removeTagForFiles( _urls, tagName );
    end;
  end;

  procedure processAddedTags;
  var
    addedTagNames: NSMutableSet;
    tagName: NSString;
  begin
    addedTagNames:= NSMutableSet.setWithArray( newTagNames );
    addedTagNames.minusSet( NSSet.setWithArray(_oldTagNames) );
    for tagName in addedTagNames do begin
      TDarwinFinderModelUtil.addTagForFiles( _urls, tagName );
    end;
  end;

begin
  if cancel then
    Exit;
  if newTagNames.isEqualToArray(_oldTagNames) then
    Exit;

  if _urls.count = 1 then begin
    TDarwinFinderModelUtil.setTagNamesOfFile( NSURL(_urls.objectAtIndex(0)), newTagNames );
  end else begin
    processRemovedTags;
    processAddedTags;
  end;
end;

class procedure TDarwinPanelUtil.showEditFinderTags(const Sender: id;
  const control: TWinControl);
var
  tagItem: NSToolBarItem absolute Sender;
  filenames: TStringArray;
  view: NSView;
  handler: TFinderTagsEditorPanelHandler;
begin
  filenames:= TDCCocoaApplication(NSApp).serviceMenuGetFilenames;
  if length(filenames) = 0 then
    Exit;

  view:= nil;
  if Assigned(tagItem) then
    view:= tagItem.valueForKey( NSSTR('_itemViewer') );
  if (view=nil) or (view.window=nil) then
    view:= NSView( control.Handle );

  handler:= TFinderTagsEditorPanelHandler.Create( filenames );
  TDarwinFinderUtil.popoverFileTagsEditor( filenames, handler.onClose, view , NSMaxYEdge );
end;

// Sharing Service
class procedure TDarwinPanelUtil.showSharingService;
var
  picker: NSSharingServicePicker;
  cocoaArray: NSArray;
  lclArray: TStringArray;
  point: TPoint;
  popupNSRect: NSRect;
  control: TWinControl;
begin
  if not TDCCocoaApplication(NSApp).serviceMenuIsReady then
    exit;

  lclArray:= TDCCocoaApplication(NSApp).serviceMenuGetFilenames;
  if lclArray = nil then
    Exit;

  cocoaArray:= TCocoaCollectionUtil.urlArrayToNSArray( lclArray );

  control:= Screen.ActiveControl;
  point:= control.ScreenToClient( Mouse.CursorPos );
  popupNSRect.origin.x:= point.X;
  popupNSRect.origin.y:= point.Y;
  popupNSRect.size:= NSMakeSize( 1, 1 );

  picker:= NSSharingServicePicker.alloc.initWithItems( cocoaArray );
  picker.showRelativeToRect_ofView_preferredEdge( popupNSRect, NSView(control.handle) , NSMaxXEdge );
  picker.release;
end;

class procedure TDarwinPanelUtil.showAirDrop;
var
  service: NSSharingService;
  lclArray: TStringArray;
  cocoaArray: NSArray;
begin
  lclArray:= TDCCocoaApplication(NSApp).serviceMenuGetFilenames;
  if lclArray = nil then
    Exit;

  cocoaArray:= TCocoaCollectionUtil.urlArrayToNSArray( lclArray );
  service:= NSSharingService.sharingServiceNamed( NSSharingServiceNameSendViaAirDrop );
  service.performWithItems( cocoaArray );
end;


end.

