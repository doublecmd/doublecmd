unit uDarwinApplication;

{$mode delphi}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, CocoaInt, Cocoa_Extra, CocoaUtils,
  uDarwinUtil;

type
  // MacOS Service Integration
  TDarwinServiceProviderCallBack = Procedure( filenames:TStringList ) of object;
  TDarwinServiceMenuIsReadyFunc = Function(): Boolean of object;
  TDarwinServiceMenuGetFilenamesFunc = Function(): TStringArray of object;

  TDarwinThemeChangedHandler = Procedure() of object;

  TDarwinServiceProvider = objcclass(NSObject)
  public
    onOpenWithNewTab: TDarwinServiceProviderCallBack;
  public
    procedure openWithNewTab( pboard:NSPasteboard; userData:NSString; error:NSStringPtr ); message 'openWithNewTab:userData:error:';
  end;

  TDCCocoaApplication = objcclass(TCocoaApplication)
    function validRequestorForSendType_returnType (sendType: NSString; returnType: NSString): id; override;
    function writeSelectionToPasteboard_types (pboard: NSPasteboard; types: NSArray): ObjCBOOL; message 'writeSelectionToPasteboard:types:';
    procedure observeValueForKeyPath_ofObject_change_context(keyPath: NSString;
      object_: id; change: NSDictionary; context: pointer); override;
  private
    _serviceProvider: TDarwinServiceProvider;
    _themeChangedHandler: TDarwinThemeChangedHandler;
  public
    serviceMenuIsReady: TDarwinServiceMenuIsReadyFunc;
    serviceMenuGetFilenames: TDarwinServiceMenuGetFilenamesFunc;
  end;

  TDarwinApplicationUtil = class
  public
    class procedure initServiceProvider(
      serveCallback: TDarwinServiceProviderCallBack;
      isReadyFunc: TDarwinServiceMenuIsReadyFunc;
      getFilenamesFunc: TDarwinServiceMenuGetFilenamesFunc );

    class procedure initThemeChangedObserver( handler: TDarwinThemeChangedHandler );
  end;

implementation

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

procedure TDCCocoaApplication.observeValueForKeyPath_ofObject_change_context(
  keyPath: NSString; object_: id; change: NSDictionary; context: pointer);
begin
  Inherited observeValueForKeyPath_ofObject_change_context( keyPath, object_, change, context );
  if keyPath.isEqualToString(NSSTR('effectiveAppearance')) then
  begin
    NSAppearance.setCurrentAppearance( self.appearance );
    if Assigned(_themeChangedHandler) then _themeChangedHandler;
  end;
end;

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

class procedure TDarwinApplicationUtil.initServiceProvider(
  serveCallback: TDarwinServiceProviderCallBack;
  isReadyFunc: TDarwinServiceMenuIsReadyFunc;
  getFilenamesFunc: TDarwinServiceMenuGetFilenamesFunc );
var
  DCApp: TDCCocoaApplication;
  sendTypes: NSArray;
  returnTypes: NSArray;
begin
  DCApp:= TDCCocoaApplication( NSApp );

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

class procedure TDarwinApplicationUtil.initThemeChangedObserver( handler: TDarwinThemeChangedHandler );
var
  DCApp: TDCCocoaApplication Absolute NSApp;
begin
  DCApp:= TDCCocoaApplication( NSApp );
  if Assigned(DCApp._themeChangedHandler) then exit;

  NSApp.addObserver_forKeyPath_options_context(
    NSApp, NSSTR('effectiveAppearance'), 0, nil );

  DCApp._themeChangedHandler:= handler;
end;

end.

