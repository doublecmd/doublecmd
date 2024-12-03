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
{$linkframework DiskArbitration}

interface

uses
  Classes, SysUtils, UnixType,
  InterfaceBase, Menus, Controls, Forms,
  uFileProperty, uFileSourceProperty, uDisplayFile, uFileView, uColumnsFileView,
  uLng,
  Cocoa_Extra, MacOSAll, CocoaAll, QuickLookUI,
  CocoaUtils, CocoaInt, CocoaPrivate, CocoaConst, CocoaMenus,
  uDarwinFSWatch, uDarwinFinder, uDarwinFinderModel;

const
  FINDER_FAVORITE_TAGS_MENU_ITEM_CAPTION = #$EF#$BF#$BC'FinderFavoriteTags';

// Darwin Util Function
function StringToNSString(const S: String): NSString;
function StringToCFStringRef(const S: String): CFStringRef;
function NSArrayToList(const theArray:NSArray): TStringList;
function ListToNSArray(const list:TStrings): NSArray;
function ListToNSUrlArray(const list:TStrings): NSArray;

procedure onMainMenuCreate( menu: NSMenu );

procedure setMacOSAppearance( mode:Integer );

function getMacOSDefaultTerminal(): String;

procedure FixMacFormatSettings;

function NSGetTempPath: String;

function NSGetFolderPath(Folder: NSSearchPathDirectory): String;

function GetFileDescription(const FileName: String): String;
function MountNetworkDrive(const serverAddress: String): Boolean;

function GetVolumeName(const Device: String): String;

procedure openSystemSecurityPreferences_PrivacyAllFiles;

function unmountAndEject(const path: String): Boolean;

procedure openNewInstance();

function getMacOSDisplayNameFromPath(const path: String): String;

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

// MacOS Simple File Sytem Watcher (only one watchPath)

{ TSimpleDarwinFSWatcher }

TSimpleDarwinFSWatcher = class( TThread )
private
  _monitor: TDarwinFSWatcher;
  _callback: TDarwinFSWatchCallBack;
  _event: TDarwinFSWatchEvent;
protected
  procedure Execute; override;
  procedure handleEvent( event:TDarwinFSWatchEvent );
  procedure doSyncCallback;
public
  procedure stop();
  constructor Create( const path:String; const callback:TDarwinFSWatchCallBack );
  destructor Destroy; override;
end;

// MacOS Service Integration
type TNSServiceProviderCallBack = Procedure( filenames:TStringList ) of object;
type TNSServiceMenuIsReady = Function(): Boolean of object;
type TNSServiceMenuGetFilenames = Function(): TStringArray of object;

type TDCCocoaApplication = objcclass(TCocoaApplication)
  function validRequestorForSendType_returnType (sendType: NSString; returnType: NSString): id; override;
  function writeSelectionToPasteboard_types (pboard: NSPasteboard; types: NSArray): ObjCBOOL; message 'writeSelectionToPasteboard:types:';
  procedure observeValueForKeyPath_ofObject_change_context( keyPath: NSString; object_: id; change: NSDictionary; context: pointer); override;
public
  serviceMenuIsReady: TNSServiceMenuIsReady;
  serviceMenuGetFilenames: TNSServiceMenuGetFilenames;
end;

type TNSServiceProvider = objcclass(NSObject)
private
  onOpenWithNewTab: TNSServiceProviderCallBack;
public
  procedure openWithNewTab( pboard:NSPasteboard; userData:NSString; error:NSStringPtr ); message 'openWithNewTab:userData:error:';
end;

type 

  { TMacosServiceMenuHelper }

  TMacosServiceMenuHelper = class
  private
    oldMenuPopupHandler: TNotifyEvent;
    serviceSubMenuCaption: String;
    tagFilePath: String;
    procedure attachSystemMenu( Sender: TObject );
    procedure attachServicesMenu( Sender: TObject );
    procedure attachFinderTagsMenu( Sender: TObject );
    procedure privilegeAction( Sender: TObject );
  public
    procedure PopUp( const menu: TPopupMenu; const caption: String; const path: String );
  end;

procedure InitNSServiceProvider(
  serveCallback: TNSServiceProviderCallBack;
  isReadyFunc: TNSServiceMenuIsReady;
  getFilenamesFunc: TNSServiceMenuGetFilenames );

procedure performMacOSService( serviceName: String );

procedure showQuickLookPanel;
procedure showEditFinderTagsPanel( const Sender: id; const control: TWinControl );
function getMacOSFinderTagFileProperty( const path: String ): TFileFinderTagProperty;

// MacOS Sharing
procedure showMacOSSharingServiceMenu;
procedure showMacOSAirDropDialog;

// MacOS Theme
type TNSThemeChangedHandler = Procedure() of object;

procedure InitNSThemeChangedObserver( handler: TNSThemeChangedHandler );

var
  HasMountURL: Boolean = False;
  NSServiceProvider: TNSServiceProvider;
  MacosServiceMenuHelper: TMacosServiceMenuHelper;
  NSThemeChangedHandler: TNSThemeChangedHandler;

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

{ TSimpleDarwinFSWatcher }

procedure TSimpleDarwinFSWatcher.Execute;
begin
  _monitor.start();
end;

procedure TSimpleDarwinFSWatcher.handleEvent( event:TDarwinFSWatchEvent );
begin
  _event:= event;
  Synchronize( doSyncCallback );
end;

procedure TSimpleDarwinFSWatcher.doSyncCallback;
begin
  _callback( _event );
  _event:= nil;
end;

procedure TSimpleDarwinFSWatcher.stop();
begin
  _monitor.terminate();
end;

constructor TSimpleDarwinFSWatcher.Create(
  const path:String;
  const callback:TDarwinFSWatchCallBack );
begin
  Inherited Create( false );
  _callback:= callback;
  _monitor:= TDarwinFSWatcher.create( handleEvent );
  _monitor.addPath( path );
end;

destructor TSimpleDarwinFSWatcher.Destroy;
begin
  _monitor.terminate;
  FreeAndNil( _monitor );
  inherited;
end;


procedure setMacOSAppearance( mode:Integer );
var
  appearance: NSAppearance;
begin
  if not NSApp.respondsToSelector( ObjCSelector('appearance') ) then
    exit;

  case mode of
    0,1:
      appearance:= nil;
    2:
      appearance:= NSAppearance.appearanceNamed( NSSTR_DARK_NAME );
    3:
      appearance:= NSAppearance.appearanceNamed( NSAppearanceNameAqua );
  end;
  NSApp.setAppearance( appearance );
  NSAppearance.setCurrentAppearance( appearance );
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

  success:= uDarwinFinderUtil.attachFinderTagsMenu( self.tagFilePath, menu, menuIndex );
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
  const caption: String; const path: String );
begin
  // because the menu item handle will be destroyed in TPopupMenu.PopUp()
  // we can only call NSApplication.setServicesMenu() in OnMenuPopupHandler()
  oldMenuPopupHandler:= OnMenuPopupHandler;
  OnMenuPopupHandler:= attachSystemMenu;
  serviceSubMenuCaption:= caption;
  tagFilePath:= path;
  menu.PopUp();
end;

{ TDarwinFileViewDrawHelper }

procedure TDarwinFileViewDrawHelper.onDrawCell(Sender: TFileView; aCol, aRow: Integer;
  aRect: TRect; focused: Boolean; aFile: TDisplayFile);
var
  tagProperty: TFileFinderTagProperty;
begin
  if (Sender is TColumnsFileView) and (aCol<>0) then
    Exit;

  tagProperty:= aFile.FSFile.FinderTagProperty;
  if tagProperty = nil then
    Exit;

  drawTagsAsDecoration( tagProperty.Colors, aRect, focused );
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
    color:= uDarwinFinderModelUtil.rectFinderTagNSColors[colorIndex];
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


procedure InitNSServiceProvider(
  serveCallback: TNSServiceProviderCallBack;
  isReadyFunc: TNSServiceMenuIsReady;
  getFilenamesFunc: TNSServiceMenuGetFilenames );
var
  DCApp: TDCCocoaApplication;
  sendTypes: NSArray;
  returnTypes: NSArray;
begin
  DCApp:= TDCCocoaApplication( NSApp );

  // MacOS Service menu incoming setup
  if not Assigned(NSServiceProvider) then
  begin
    NSServiceProvider:= TNSServiceProvider.alloc.init;
    DCApp.setServicesProvider( NSServiceProvider );
    NSUpdateDynamicServices;
  end;
  NSServiceProvider.onOpenWithNewTab:= serveCallback;

  // MacOS Service menu outgoing setup
  sendTypes:= NSArray.arrayWithObject(NSFilenamesPboardType);
  returnTypes:= nil;
  DCApp.serviceMenuIsReady:= isReadyFunc;
  DCApp.serviceMenuGetFilenames:= getFilenamesFunc;
  DCApp.registerServicesMenuSendTypes_returnTypes( sendTypes, returnTypes );
end;

procedure TNSServiceProvider.openWithNewTab( pboard:NSPasteboard; userData:NSString; error:NSStringPtr );
var
  filenameArray{, lClasses}: NSArray;
  filenameList: TStringList;
begin
  filenameArray := pboard.propertyListForType(NSFilenamesPboardType);
  if filenameArray <> nil then
  begin
    if Assigned(onOpenWithNewTab) then
    begin
      filenameList:= NSArrayToList( filenameArray );
      onOpenWithNewTab( filenameList );
      FreeAndNil( filenameList );
    end;
  end;
end;

function TDCCocoaApplication.validRequestorForSendType_returnType (sendType: NSString; returnType: NSString): id;
var
  isSendTypeMatch: ObjcBool;
  isReturnTypeMatch: ObjcBool;
begin
  Result:= nil;
  if not NSFilenamesPboardType.isEqualToString(sendType) then exit;
  if returnType<>nil then exit;
  if self.serviceMenuIsReady() then Result:=self;
end;

function TDCCocoaApplication.writeSelectionToPasteboard_types( pboard: NSPasteboard; types: NSArray): ObjCBOOL;
var
  lclArray: TStringArray;
  cocoaArray: NSArray;
begin
  Result:= false;
  lclArray:= self.serviceMenuGetFilenames();
  if lclArray=nil then exit;

  cocoaArray:= StringArrayFromLCLToNS( lclArray );
  pboard.declareTypes_owner( NSArray.arrayWithObject(NSFileNamesPboardType), nil );
  pboard.setPropertyList_forType( cocoaArray, NSFileNamesPboardType );
  Result:= true;
end;

procedure TDCCocoaApplication.observeValueForKeyPath_ofObject_change_context(
  keyPath: NSString; object_: id; change: NSDictionary; context: pointer);
begin
  Inherited observeValueForKeyPath_ofObject_change_context( keyPath, object_, change, context );
  if keyPath.isEqualToString(NSSTR('effectiveAppearance')) then
  begin
    NSAppearance.setCurrentAppearance( self.appearance );
    if Assigned(NSThemeChangedHandler) then NSThemeChangedHandler;
  end;
end;

procedure InitNSThemeChangedObserver( handler: TNSThemeChangedHandler );
begin
  if Assigned(NSThemeChangedHandler) then exit;

  NSApp.addObserver_forKeyPath_options_context(
    NSApp, NSSTR('effectiveAppearance'), 0, nil );

  NSThemeChangedHandler:= handler;
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
  popupNSRect.size:= NSZeroSize;

  picker:= NSSharingServicePicker.alloc.initWithItems( cocoaArray );
  picker.showRelativeToRect_ofView_preferredEdge( popupNSRect, NSView(control.handle) , NSMinYEdge );
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

function NSArrayToList(const theArray:NSArray): TStringList;
var
  i: Integer;
  list : TStringList;
begin
  list := TStringList.Create;
  for i := 0 to theArray.Count-1 do
  begin
    list.Add( NSStringToString( theArray.objectAtIndex(i) ) );
  end;
  Result := list;
end;

function ListToNSArray(const list:TStrings): NSArray;
var
  theArray: NSMutableArray;
  item: String;
begin
  theArray := NSMutableArray.arrayWithCapacity( list.Count );
  for item in list do begin
    theArray.addObject( StringToNSString(item) );
  end;
  Result := theArray;
end;

function ListToNSUrlArray(const list:TStrings): NSArray;
var
  theArray: NSMutableArray;
  item: String;
  url: NSUrl;
begin
  theArray:= NSMutableArray.arrayWithCapacity( list.Count );
  for item in list do begin
    url:= NSUrl.fileURLWithPath( StringToNSString(item) );
    theArray.addObject( url );
  end;
  Result:= theArray;
end;

function CFStringToStr(AString: CFStringRef): String;
var
  Str: Pointer;
  StrSize: CFIndex;
  StrRange: CFRange;
begin
  if AString = nil then
  begin
    Result:= EmptyStr;
    Exit;
  end;
  // Try the quick way first
  Str:= CFStringGetCStringPtr(AString, kCFStringEncodingUTF8);
  if Str <> nil then
    Result:= PAnsiChar(Str)
  else begin
    // if that doesn't work this will
    StrRange.location:= 0;
    StrRange.length:= CFStringGetLength(AString);

    CFStringGetBytes(AString, StrRange, kCFStringEncodingUTF8,
                     Ord('?'), False, nil, 0, StrSize{%H-});
    SetLength(Result, StrSize);

    if StrSize > 0 then
    begin
      CFStringGetBytes(AString, StrRange, kCFStringEncodingUTF8,
                       Ord('?'), False, @Result[1], StrSize, StrSize);
    end;
  end;
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

function StringToNSString(const S: String): NSString;
begin
  Result:= NSString(NSString.stringWithUTF8String(PAnsiChar(S)));
end;

function StringToCFStringRef(const S: String): CFStringRef;
begin
  Result:= CFStringCreateWithCString(nil, PAnsiChar(S), kCFStringEncodingUTF8);
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

function GetVolumeName(const Device: String): String;
var
  ADisk: DADiskRef;
  AName: CFStringRef;
  ASession: DASessionRef;
  ADescription: CFDictionaryRef;
begin
  Result:= EmptyStr;
  ASession:= DASessionCreate(kCFAllocatorDefault);
  if Assigned(ASession) then
  begin
    ADisk:= DADiskCreateFromBSDName(kCFAllocatorDefault, ASession, PAnsiChar(Device));
    if Assigned(ADisk) then
    begin
      ADescription:= DADiskCopyDescription(ADisk);
      if Assigned(ADescription) then
      begin
        AName:= CFDictionaryGetValue(ADescription, kDADiskDescriptionVolumeNameKey);
        if (AName = nil) then AName:= CFDictionaryGetValue(ADescription, kDADiskDescriptionMediaNameKey);
        if Assigned(AName) then
        begin
          Result:= CFStringToStr(AName);
        end;
        CFRelease(ADescription);
      end;
      CFRelease(ADisk);
    end;
    CFRelease(ASession);
  end;
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

function unmountAndEject(const path: String): Boolean;
begin
  Result:= NSWorkspace.sharedWorkspace.unmountAndEjectDeviceAtPath( StringToNSString(path) );
end;

procedure openNewInstance();
begin
  NSWorkspace.sharedWorkspace.launchApplicationAtURL_options_configuration_error(
    NSBundle.mainBundle.bundleURL,
    NSWorkspaceLaunchNewInstance,
    nil,
    nil);
end;


var
  NetFS: TLibHandle = NilHandle;
  CoreServices: TLibHandle = NilHandle;

var
  FSMountServerVolumeSync: function(url: CFURLRef; mountDir: CFURLRef; user: CFStringRef; password: CFStringRef;
    mountedVolumeRefNum: FSVolumeRefNumPtr; flags: OptionBits): OSStatus; stdcall;
  NetFSMountURLSync: function(_url: CFURLRef; _mountpath: CFURLRef; _user: CFStringRef; _passwd: CFStringRef;
    _open_options: CFMutableDictionaryRef; _mount_options: CFMutableDictionaryRef; _mountpoints: CFArrayRefPtr): Int32; cdecl;

function MountNetworkDrive(const serverAddress: String): Boolean;
var
  sharePath: NSURL;
  mountPoints: CFArrayRef = nil;
begin
  sharePath:= NSURL.URLWithString(StringToNSString(serverAddress));
  if Assigned(NetFSMountURLSync) then
    Result:= NetFSMountURLSync(CFURLRef(sharePath), nil, nil, nil, nil, nil, @mountPoints) = 0
  else begin
    Result:= FSMountServerVolumeSync(CFURLRef(sharePath), nil, nil, nil, nil, 0) = noErr;
  end;
end;

procedure Initialize;
begin
  NetFS:= LoadLibrary('/System/Library/Frameworks/NetFS.framework/NetFS');
  if (NetFS <> NilHandle) then
  begin
    @NetFSMountURLSync:= GetProcAddress(NetFS, 'NetFSMountURLSync');
  end;
  CoreServices:= LoadLibrary('/System/Library/Frameworks/CoreServices.framework/CoreServices');
  if (CoreServices <> NilHandle) then
  begin
    @FSMountServerVolumeSync:= GetProcAddress(CoreServices, 'FSMountServerVolumeSync');
  end;
  HasMountURL:= Assigned(NetFSMountURLSync) or Assigned(FSMountServerVolumeSync);
  MacosServiceMenuHelper:= TMacosServiceMenuHelper.Create;
  DarwinFileViewDrawHelper:= TDarwinFileViewDrawHelper.Create;
end;

procedure Finalize;
begin
  if (NetFS <> NilHandle) then FreeLibrary(NetFS);
  if (CoreServices <> NilHandle) then FreeLibrary(CoreServices);
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
    _path: String;
  public
    constructor Create( const path: String );
    procedure onClose( const cancel: Boolean; const tagNames: NSArray );
  end;

constructor TFinderTagsEditorPanelHandler.Create( const path: String );
begin
  _path:= path;
end;

procedure TFinderTagsEditorPanelHandler.onClose( const cancel: Boolean; const tagNames: NSArray );
var
  url: NSURL;
begin
  if cancel then
    Exit;
  url:= NSURL.fileURLWithPath( StrToNSString(_path) );
  uDarwinFinderModelUtil.setTagNamesOfFile( url, tagNames );
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

  handler:= TFinderTagsEditorPanelHandler.Create( filenames[0] );
  uDarwinFinderUtil.popoverFileTagsEditor( filenames[0], handler.onClose, view , NSMaxYEdge );
end;

function getMacOSFinderTagFileProperty( const path: String ): TFileFinderTagProperty;
var
  url: NSURL;
  tagNames: NSArray;
  tagName: NSString;

  function toPrimaryColors: TFileFinderTagPrimaryColors;
  var
    tag: TFinderTag;
    iSource: NSUInteger;
    iDest: Integer;
    colorIndex: Integer;
  begin
    iSource:= 0;
    if tagNames.count > 3 then
      iSource:= tagNames.count - 3;
    for iDest:=0 to 2 do begin
      colorIndex:= -1;
      if iSource < tagNames.count then begin
        tagName:= NSString( tagNames.objectAtIndex(iSource) );
        tag:= TFinderTags.getTagOfName( tagName );
        colorIndex:= tag.colorIndex;
      end;
      Result.indexes[iDest]:= colorIndex;
      inc( iSource );
    end;
  end;

begin
  Result:= nil;
  url:= NSURL.fileURLWithPath( StrToNSString(path) );
  tagNames:= uDarwinFinderModelUtil.getTagNamesOfFile( url );

  if tagNames = nil then
    Exit;

  Result:= TFileFinderTagProperty.Create;
  Result.Colors:= toPrimaryColors;
end;

function getMacOSDisplayNameFromPath(const path: String): String;
var
  cocoaPath: NSString;
  displayName: NSString;
begin
  cocoaPath:= StringToNSString(path).stringByStandardizingPath;
  displayName:= NSFileManager.defaultManager.displayNameAtPath( cocoaPath );
  Result:= displayName.UTF8String;
end;

initialization
  Initialize;

finalization
  Finalize;

end.
