unit uDarwinApplication;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, Menus, uLng,
  Controls, Forms, ExtCtrls, LazLoggerBase, uLog,
  MacOSAll, CocoaAll,
  CocoaInt, CocoaPrivate, CocoaThemes, Cocoa_Extra, CocoaMenus, CocoaUtils, CocoaConst,
  uDarwinUtil, uDarwinFinder;

const
  FINDER_FAVORITE_TAGS_MENU_ITEM_CAPTION = #$EF#$BF#$BC'FinderFavoriteTags';

type
  // MacOS Service Integration
  TDarwinServiceProviderCallBack = Procedure( filenames:TStringList ) of object;
  TDarwinServiceMenuIsReadyFunc = Function(): Boolean of object;
  TDarwinServiceMenuGetFilenamesFunc = Function(): TStringArray of object;

  { TDarwinServiceProvider }

  TDarwinServiceProvider = objcclass(NSObject)
  public
    onOpenWithNewTab: TDarwinServiceProviderCallBack;
  public
    procedure openWithNewTab( pboard:NSPasteboard; userData:NSString; error:NSStringPtr ); message 'openWithNewTab:userData:error:';
  end;

  { TDCCocoaApplication }

  TDCCocoaApplication = objcclass(TCocoaApplication)
    function validRequestorForSendType_returnType (sendType: NSString; returnType: NSString): id; override;
    function writeSelectionToPasteboard_types (pboard: NSPasteboard; types: NSArray): ObjCBOOL; message 'writeSelectionToPasteboard:types:';
  private
    _serviceProvider: TDarwinServiceProvider;
  public
    serviceMenuIsReady: TDarwinServiceMenuIsReadyFunc;
    serviceMenuGetFilenames: TDarwinServiceMenuGetFilenamesFunc;
  end;

  { TDarwinApplicationUtil }

  TDarwinApplicationUtil = class
  public
    class procedure initServiceProvider(
      const serveCallback: TDarwinServiceProviderCallBack;
      const isReadyFunc: TDarwinServiceMenuIsReadyFunc;
      const getFilenamesFunc: TDarwinServiceMenuGetFilenamesFunc );
    class procedure popUpMenuWithServiceSubmenu( const menu: TPopupMenu; const caption: String; const paths: TStringArray );
    class procedure performService( const serviceName: String );
    class procedure openSystemSecurityPreferences_PrivacyAllFiles;
  public
    class procedure setTheme( const mode: Integer );
  public
    class procedure fixFormatSettings;
    class procedure openNewInstance;
  end;

procedure darwinOnMainMenuCreate( menu: NSMenu );

implementation

{ TDCCocoaApplication }

function TDCCocoaApplication.validRequestorForSendType_returnType (sendType: NSString; returnType: NSString): id;
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

{ TDarwinServiceProvider }

procedure TDarwinServiceProvider.openWithNewTab( pboard:NSPasteboard; userData:NSString; error:NSStringPtr );
var
  filenameArray: NSArray;
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

{ TDarwinApplicationUtil }

class procedure TDarwinApplicationUtil.setTheme(const mode: Integer);
var
  appearance: NSAppearance;
begin
  if not NSApp.respondsToSelector( ObjCSelector('appearance') ) then
    exit;

  case mode of
    0, 1:
      appearance:= nil;
    2:
      appearance:= NSAppearance.appearanceNamed( NSSTR_DARK_NAME );
    3:
      appearance:= NSAppearance.appearanceNamed( NSAppearanceNameAqua );
  end;
  NSApp.setAppearance( appearance );

  if appearance = nil then
    appearance:= NSApp.effectiveAppearance;
  NSAppearance.setCurrentAppearance( appearance );
  TCocoaThemeServices.darwinThemeChangedNotify;
end;

class procedure TDarwinApplicationUtil.fixFormatSettings;
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

class procedure TDarwinApplicationUtil.openNewInstance;
begin
  NSWorkspace.sharedWorkspace.launchApplicationAtURL_options_configuration_error(
    NSBundle.mainBundle.bundleURL,
    NSWorkspaceLaunchNewInstance,
    nil,
    nil);
end;

class procedure TDarwinApplicationUtil.initServiceProvider(
  const serveCallback: TDarwinServiceProviderCallBack;
  const isReadyFunc: TDarwinServiceMenuIsReadyFunc;
  const getFilenamesFunc: TDarwinServiceMenuGetFilenamesFunc);
var
  DCApp: TDCCocoaApplication Absolute NSApp;
  sendTypes: NSArray;
  returnTypes: NSArray;
begin
  // MacOS Service menu incoming setup
  if not Assigned(DCApp._serviceProvider) then
  begin
    DCApp._serviceProvider:= TDarwinServiceProvider.alloc.init;
    DCApp.setServicesProvider( DCApp._serviceProvider );
    NSUpdateDynamicServices;
  end;
  DCApp._serviceProvider.onOpenWithNewTab:= serveCallback;

  // MacOS Service menu outgoing setup
  sendTypes:= NSArray.arrayWithObject(NSFilenamesPboardType);
  returnTypes:= nil;
  DCApp.serviceMenuIsReady:= isReadyFunc;
  DCApp.serviceMenuGetFilenames:= getFilenamesFunc;
  DCApp.registerServicesMenuSendTypes_returnTypes( sendTypes, returnTypes );
end;

type
  TDarwinServiceMenuManager = class
  private
    oldMenuPopupHandler: TNotifyEvent;
    serviceSubMenuCaption: String;
    tagFilePaths: TStringArray;
    procedure attachSystemMenu( Sender: TObject );
    procedure attachServicesMenu( Sender: TObject );
    procedure attachFinderTagsMenu( Sender: TObject );
    procedure privilegeAction( Sender: TObject );
  end;

class procedure TDarwinApplicationUtil.popUpMenuWithServiceSubmenu(const menu: TPopupMenu;
  const caption: String; const paths: TStringArray);
var
  menuManager: TDarwinServiceMenuManager;
begin
  menuManager:= TDarwinServiceMenuManager.Create;

  // because the menu item handle will be destroyed in TPopupMenu.PopUp()
  // we can only call NSApplication.setServicesMenu() in OnMenuPopupHandler()
  menuManager.oldMenuPopupHandler:= OnMenuPopupHandler;
  OnMenuPopupHandler:= @menuManager.attachSystemMenu;
  menuManager.serviceSubMenuCaption:= caption;
  menuManager.tagFilePaths:= paths;
  menu.PopUp();

  menuManager.Free;
end;

class procedure TDarwinApplicationUtil.performService(const serviceName: String
  );
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

class procedure TDarwinApplicationUtil.openSystemSecurityPreferences_PrivacyAllFiles;
const
  Privacy_AllFiles = 'x-apple.systempreferences:com.apple.preference.security?Privacy_AllFiles';
var
  url: NSURL;
begin
  url:= NSURL.URLWithString( NSSTR(Privacy_AllFiles) );
  NSWorkspace.sharedWorkspace.openURL( url );
end;

procedure TDarwinServiceMenuManager.attachSystemMenu(Sender: TObject);
begin
  self.attachServicesMenu( Sender );
  self.attachFinderTagsMenu( Sender );
end;

procedure TDarwinServiceMenuManager.attachServicesMenu( Sender: TObject );
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

procedure TDarwinServiceMenuManager.attachFinderTagsMenu( Sender: TObject );
var
  menu: TPopupMenu Absolute Sender;
  menuItem: TMenuItem;
  menuIndex: Integer;
  success: Boolean;
begin
  menuIndex:= menu.Items.IndexOfCaption( FINDER_FAVORITE_TAGS_MENU_ITEM_CAPTION );
  if menuIndex < 0 then
    Exit;

  success:= TDarwinFinderUtil.attachFinderTagsMenu( self.tagFilePaths, menu, menuIndex );
  if success then
    Exit;

  menuItem:= menu.Items[menuIndex];
  menuItem.Caption:= rsMenuMacOSGrantPermissionToSupportFinderTags;
  menuItem.OnClick:= @self.privilegeAction;
end;

procedure TDarwinServiceMenuManager.privilegeAction(Sender: TObject);
begin
  TDarwinApplicationUtil.openSystemSecurityPreferences_PrivacyAllFiles;
end;

procedure darwinOnMainMenuCreate( menu: NSMenu );
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

type
  
  TDebugTimer = class( TTimer )
    procedure log( Sender: TObject );
  end;

procedure TDebugTimer.log(Sender: TObject);
var
  form: TForm;
  control: TControl;

  DCApp: TDCCocoaApplication Absolute NSApp;
  window: NSWindow;
  responder: NSResponder;
begin
  form:= Screen.ActiveForm;
  control:= form.ActiveControl;
  LogWrite( '>>> ' + TimeToStr(now) );
  LogWrite( 'LCL:' );
  LogWrite( '  ' + DbgsName(form) );
  LogWrite( '  ' + DbgsName(control) );

  window:= DCApp.keyWindow;
  responder:= window.firstResponder;
  LogWrite( 'COCOA:' );
  LogWrite( '  ' + window.className.UTF8String + ': ' + DbgsName(window.lclGetTarget) );
  LogWrite( '  ' + responder.className.UTF8String + ': ' + DbgsName(responder.lclGetTarget)  );

  LogWrite( '' );
end;

var
  debugTimer: TDebugTimer;

initialization
  debugTimer:= TDebugTimer.Create( nil );
  debugTimer.Interval:= 10*1000;
  debugTimer.OnTimer:= @debugTimer.log;
  debugTimer.Enabled:= True;


end.

