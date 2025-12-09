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

function getMacOSDefaultTerminal(): String;

procedure FixMacFormatSettings;

function NSGetTempPath: String;

function NSGetFolderPath(Folder: NSSearchPathDirectory): String;

function GetFileDescription(const FileName: String): String;

function ResolveAliasFile(const FileName: String): String;

procedure openSystemSecurityPreferences_PrivacyAllFiles;

procedure openNewInstance();

// Workarounds for FPC RTL Bug
// copied from ptypes.inc and modified fstypename only
{$if defined(cpuarm) or defined(cpuaarch64) or defined(iphonesim)}
     { structure used on iPhoneOS and available on Mac OS X 10.6 and later }

const MFSTYPENAMELEN = 16;

type TDarwinAarch64Statfs = record
          bsize : cuint32;
          iosize : cint32;
          blocks : cuint64;
          bfree : cuint64;
          bavail : cuint64;
          files : cuint64;
          ffree : cuint64;
          fsid : fsid_t;
          owner : uid_t;
          ftype : cuint32;
          fflags : cuint32;
          fssubtype : cuint32;
          fstypename : array[0..(MFSTYPENAMELEN)-1] of char;
          mountpoint : array[0..(PATH_MAX)-1] of char;
          mntfromname : array[0..(PATH_MAX)-1] of char;
          reserved: array[0..7] of cuint32;
     end;

type TDarwinStatfs = TDarwinAarch64Statfs;

{$else}

type TDarwinStatfs = TStatFs;

{$endif}

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

procedure performMacOSService( serviceName: String );

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
  openSystemSecurityPreferences_PrivacyAllFiles;
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

procedure performMacOSService( serviceName: String );
var
  pboard: NSPasteboard;
  ok: Boolean;
begin
  pboard:= NSPasteboard.pasteboardWithUniqueName;
  ok:= TDCCocoaApplication(NSApp).writeSelectionToPasteboard_types(
    pboard , nil );
  if ok then
    NSPerformService( NSSTR(serviceName), pboard );
end;

procedure FixMacFormatSettings;
var
  S: String;
  ALocale: CFLocaleRef;
begin
  ALocale:= CFLocaleCopyCurrent;
  if Assigned(ALocale) then
  begin
    S:= CFStringToStr(CFLocaleGetValue(ALocale, kCFLocaleGroupingSeparator));
    if Length(S) = 0 then
    begin
      DefaultFormatSettings.ThousandSeparator:= #0;
    end;
    CFRelease(ALocale);
  end;
end;

function NSGetTempPath: String;
begin
  Result:= IncludeTrailingBackslash(NSTemporaryDirectory.UTF8String);
end;

function getMacOSDefaultTerminal(): String;
begin
  Result:= NSStringToString( NSWorkspace.sharedWorkspace.fullPathForApplication( NSStr('terminal') ) );
end;

function NSGetFolderPath(Folder: NSSearchPathDirectory): String;
var
  Path: NSArray;
begin
  Path:= NSFileManager.defaultManager.URLsForDirectory_inDomains(Folder, NSUserDomainMask);
  if Path.count > 0 then
  begin
    Result:= IncludeTrailingBackslash(NSURL(Path.objectAtIndex(0)).path.UTF8String) + ApplicationName;
  end;
end;

function GetFileDescription(const FileName: String): String;
var
  Error: NSError;
  WS: NSWorkspace;
  FileType: NSString;
  FileNameRef: CFStringRef;
begin
  WS:= NSWorkspace.sharedWorkspace;
  FileNameRef:= StringToCFStringRef(FileName);
  if (FileNameRef = nil) then Exit(EmptyStr);
  FileType:= WS.typeOfFile_error(NSString(FileNameRef), @Error);
  if (FileType = nil) then
    Result:= Error.localizedDescription.UTF8String
  else begin
    Result:= WS.localizedDescriptionForType(FileType).UTF8String;
  end;
  CFRelease(FileNameRef);
end;

function ResolveAliasFile(const FileName: String): String;
var
  ASource: NSURL;
  ATarget: NSURL;
begin
  Result:= EmptyStr;
  ASource:= NSURL.fileURLWithPath(StringToNSString(FileName));
  ATarget:= NSURL(NSURL.URLByResolvingAliasFileAtURL_options_error(ASource, NSURLBookmarkResolutionWithoutUI, nil));
  if Assigned(ATarget) then Result:= ATarget.fileSystemRepresentation;
end;

procedure openSystemSecurityPreferences_PrivacyAllFiles;
const
  Privacy_AllFiles = 'x-apple.systempreferences:com.apple.preference.security?Privacy_AllFiles';
var
  url: NSURL;
begin
  url:= NSURL.URLWithString( NSSTR(Privacy_AllFiles) );
  NSWorkspace.sharedWorkspace.openURL( url );
end;

procedure openNewInstance();
begin
  NSWorkspace.sharedWorkspace.launchApplicationAtURL_options_configuration_error(
    NSBundle.mainBundle.bundleURL,
    NSWorkspaceLaunchNewInstance,
    nil,
    nil);
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
