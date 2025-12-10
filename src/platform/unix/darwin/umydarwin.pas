{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains specific DARWIN functions.

   Copyright (C) 2016-2024 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA

   Notes:
   1. TDarwinAarch64Statfs is the workaround for the bug of FPC.
      TDarwinAarch64Statfs and the related codes can be removed after FPC 3.3.1
      see also: https://gitlab.com/freepascal.org/fpc/source/-/issues/39873
}

unit uMyDarwin;

{$mode delphi}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, UnixType,
  InterfaceBase, Menus, Controls, Forms,
  uFileProperty, uDisplayFile, uFileView, uColumnsFileView,
  uLng,
  MacOSAll, CocoaAll, QuickLookUI,
  CocoaUtils, CocoaInt, CocoaPrivate, CocoaConst, CocoaMenus, Cocoa_Extra,
  uDarwinApplication, uDarwinFSWatch, uDarwinFinder, uDarwinFinderModel, uDarwinUtil;

const
  FINDER_FAVORITE_TAGS_MENU_ITEM_CAPTION = #$EF#$BF#$BC'FinderFavoriteTags';

procedure onMainMenuCreate( menu: NSMenu );

type

  { TMacosServiceMenuHelper }

  TMacosServiceMenuHelper = class
  private
    oldMenuPopupHandler: TNotifyEvent;
    serviceSubMenuCaption: String;
    tagFilePaths: TStringArray;
    procedure attachSystemMenu( Sender: TObject );
    procedure attachServicesMenu( Sender: TObject );
    procedure attachFinderTagsMenu( Sender: TObject );
    procedure privilegeAction( Sender: TObject );
  public
    procedure PopUp( const menu: TPopupMenu; const caption: String; const paths: TStringArray );
  end;

procedure showQuickLookPanel;
procedure showEditFinderTagsPanel( const Sender: id; const control: TWinControl );

// MacOS Sharing
procedure showMacOSSharingServiceMenu;
procedure showMacOSAirDropDialog;

var
  MacosServiceMenuHelper: TMacosServiceMenuHelper;

type
  
  { TDarwinFileViewDrawHelper }

  TDarwinFileViewDrawHelper = class
    procedure onDrawCell(Sender: TFileView; aCol, aRow: Integer;
      aRect: TRect; focused: Boolean; aFile: TDisplayFile);
    procedure drawTagsAsDecoration(
      const colors: TFileFinderTagPrimaryColors; const drawRect: TRect; const focused: Boolean );
  end;

var
  DarwinFileViewDrawHelper: TDarwinFileViewDrawHelper;

implementation

uses
  DynLibs;

procedure onMainMenuCreate( menu: NSMenu );
var
  lclForm: TObject;
  keyWindow: NSWindow;
begin
  lclForm:= nil;
  keyWindow:= NSApplication(NSApp).keyWindow;
  if keyWindow <> nil then
    lclForm:= keyWindow.lclGetTarget;
  if (lclForm=nil) or (lclForm.ClassName='TfrmMain') then
    AttachEditMenu( menu, menu.numberOfItems, CocoaConst.NSSTR_EDIT_MENU );
end;

procedure TMacosServiceMenuHelper.attachSystemMenu(Sender: TObject);
begin
  self.attachServicesMenu( Sender );
  self.attachFinderTagsMenu( Sender );
end;

procedure TMacosServiceMenuHelper.attachServicesMenu( Sender: TObject );
var
  menu: TPopupMenu Absolute Sender;
  servicesItem: TMenuItem;
  subMenu: TCocoaMenu;
begin
  // call the previous OnMenuPopupHandler and restore it
  if Assigned(oldMenuPopupHandler) then oldMenuPopupHandler( Sender );
  OnMenuPopupHandler:= oldMenuPopupHandler;
  oldMenuPopupHandler:= nil;

  // attach the Services Sub Menu by calling NSApplication.setServicesMenu()
  servicesItem:= menu.Items.Find(serviceSubMenuCaption);
  if servicesItem<>nil then
  begin
    subMenu:= TCocoaMenu.alloc.initWithTitle(NSString.string_);
    TCocoaMenuItem(servicesItem.Handle).setSubmenu( subMenu );
    subMenu.release;
    NSApp.setServicesMenu( NSMenu(servicesItem.Handle) );
  end;
end;

procedure TMacosServiceMenuHelper.attachFinderTagsMenu( Sender: TObject );
var
  menu: TPopupMenu Absolute Sender;
  menuItem: TMenuItem;
  menuIndex: Integer;
  success: Boolean;
begin
  menuIndex:= menu.Items.IndexOfCaption( FINDER_FAVORITE_TAGS_MENU_ITEM_CAPTION );
  if menuIndex < 0 then
    Exit;

  success:= uDarwinFinderUtil.attachFinderTagsMenu( self.tagFilePaths, menu, menuIndex );
  if success then
    Exit;

  menuItem:= menu.Items[menuIndex];
  menuItem.Caption:= rsMenuMacOSGrantPermissionToSupportFinderTags;
  menuItem.OnClick:= self.privilegeAction;
end;

procedure TMacosServiceMenuHelper.privilegeAction(Sender: TObject);
begin
  TDarwinApplicationUtil.openSystemSecurityPreferences_PrivacyAllFiles;
end;

procedure TMacosServiceMenuHelper.PopUp( const menu: TPopupMenu;
  const caption: String; const paths: TStringArray );
begin
  // because the menu item handle will be destroyed in TPopupMenu.PopUp()
  // we can only call NSApplication.setServicesMenu() in OnMenuPopupHandler()
  oldMenuPopupHandler:= OnMenuPopupHandler;
  OnMenuPopupHandler:= attachSystemMenu;
  serviceSubMenuCaption:= caption;
  tagFilePaths:= paths;
  menu.PopUp();
end;

{ TDarwinFileViewDrawHelper }

procedure TDarwinFileViewDrawHelper.onDrawCell(Sender: TFileView; aCol, aRow: Integer;
  aRect: TRect; focused: Boolean; aFile: TDisplayFile);
var
  macOSProperty: TFileMacOSSpecificProperty;
begin
  if (Sender is TColumnsFileView) and (aCol<>0) then
    Exit;

  macOSProperty:= aFile.FSFile.MacOSSpecificProperty;
  if macOSProperty = nil then
    Exit;

  drawTagsAsDecoration( macOSProperty.FinderTagPrimaryColors, aRect, focused );
end;

procedure TDarwinFileViewDrawHelper.drawTagsAsDecoration(
  const colors: TFileFinderTagPrimaryColors; const drawRect: TRect;
  const focused: Boolean);
var
  i: Integer;
  colorIndex: Integer;
  color: NSColor;
  tagRect: NSRect;
  path: NSBezierPath;
begin
  tagRect.size.width:= 11;
  tagRect.size.height:= 11;
  tagRect.origin.x:= drawRect.Right - 17;
  tagRect.origin.y:= drawRect.Top + (drawRect.Height-tagRect.size.height)/2;

  for i:=0 to 2 do begin
    colorIndex:= colors.indexes[i];
    if colorIndex < 0 then
      break;
    color:= uDarwinFinderModelUtil.decorationFinderTagNSColors[colorIndex];
    color.set_;
    path:= NSBezierPath.bezierPathWithOvalInRect( tagRect );
    path.fill;
    if focused then
      NSColor.alternateSelectedControlTextColor.set_
    else
      NSColor.textBackgroundColor.set_;
    path.stroke;
    tagRect.origin.x:= tagRect.origin.x - 5;
  end;
end;

procedure showMacOSSharingServiceMenu;
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

  cocoaArray:= UrlArrayFromLCLToNS( lclArray );

  control:= Screen.ActiveControl;
  point:= control.ScreenToClient( Mouse.CursorPos );
  popupNSRect.origin.x:= point.X;
  popupNSRect.origin.y:= point.Y;
  popupNSRect.size:= NSMakeSize( 1, 1 );

  picker:= NSSharingServicePicker.alloc.initWithItems( cocoaArray );
  picker.showRelativeToRect_ofView_preferredEdge( popupNSRect, NSView(control.handle) , NSMaxXEdge );
  picker.release;
end;

procedure showMacOSAirDropDialog;
var
  service: NSSharingService;
  lclArray: TStringArray;
  cocoaArray: NSArray;
begin
  lclArray:= TDCCocoaApplication(NSApp).serviceMenuGetFilenames;
  if lclArray = nil then
    Exit;

  cocoaArray:= UrlArrayFromLCLToNS( lclArray );
  service:= NSSharingService.sharingServiceNamed( NSSharingServiceNameSendViaAirDrop );
  service.performWithItems( cocoaArray );
end;

procedure Initialize;
begin
  MacosServiceMenuHelper:= TMacosServiceMenuHelper.Create;
  DarwinFileViewDrawHelper:= TDarwinFileViewDrawHelper.Create;
end;

procedure Finalize;
begin
  FreeAndNil( MacosServiceMenuHelper );
end;

type
  
  { TDCQLPreviewPanelMate }

  TDCQLPreviewPanelMate = objcclass( NSObject, QLPreviewPanelDataSourceProtocol )
  private
    _urlArray: NSArray;
  public
    function numberOfPreviewItemsInPreviewPanel (panel: QLPreviewPanel): NSInteger;
    function previewPanel_previewItemAtIndex (panel: QLPreviewPanel; index: NSInteger): QLPreviewItemProtocol;
  public
    function initWithItems( urlArray: NSArray ): id; message 'setUrlArray:';
    procedure dealloc; override;
  end;

function TDCQLPreviewPanelMate.numberOfPreviewItemsInPreviewPanel(
  panel: QLPreviewPanel): NSInteger;
begin
  Result:= _urlArray.count;
end;

function TDCQLPreviewPanelMate.previewPanel_previewItemAtIndex(panel: QLPreviewPanel;
  index: NSInteger): QLPreviewItemProtocol;
begin
  Result:= QLPreviewItemProtocol( _urlArray.objectAtIndex(index) );
end;

function TDCQLPreviewPanelMate.initWithItems(urlArray: NSArray): id;
begin
  Result:= Inherited init;
  _urlArray:= urlArray;
  _urlArray.retain;
end;

procedure TDCQLPreviewPanelMate.dealloc;
begin
  _urlArray.release;
  Inherited;
end;

procedure showQuickLookPanel;
var
  lclArray: TStringArray;
  mate: TDCQLPreviewPanelMate;
  panel: QLPreviewPanel;
begin
  lclArray:= TDCCocoaApplication(NSApp).serviceMenuGetFilenames;
  if lclArray = nil then
    Exit;

  mate:= TDCQLPreviewPanelMate.alloc.initWithItems( UrlArrayFromLCLToNS(lclArray) );
  panel:= QLPreviewPanel.sharedPreviewPanel;
  panel.setDataSource( mate );
  panel.makeKeyAndOrderFront( nil );
  mate.release;
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
  _urls:= UrlArrayFromLCLToNS( paths );
  _urls.retain;
  _oldTagNames:= uDarwinFinderModelUtil.getTagNamesOfFiles( _urls );
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
      uDarwinFinderModelUtil.removeTagForFiles( _urls, tagName );
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
      uDarwinFinderModelUtil.addTagForFiles( _urls, tagName );
    end;
  end;

begin
  if cancel then
    Exit;
  if _urls.count = 1 then begin
    uDarwinFinderModelUtil.setTagNamesOfFile( NSURL(_urls.objectAtIndex(0)), newTagNames );
  end else begin
    processRemovedTags;
    processAddedTags;
  end;
end;

procedure showEditFinderTagsPanel( const Sender: id; const control: TWinControl );
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
  uDarwinFinderUtil.popoverFileTagsEditor( filenames, handler.onClose, view , NSMaxYEdge );
end;

initialization
  Initialize;

finalization
  Finalize;

end.
