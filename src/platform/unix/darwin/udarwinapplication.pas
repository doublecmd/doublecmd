unit uDarwinApplication;

{$mode delphi}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, CocoaInt, Cocoa_Extra, CocoaUtils,
  uDarwinUtil;

// MacOS Service Integration
type
  TNSServiceProviderCallBack = Procedure( filenames:TStringList ) of object;
  TNSServiceMenuIsReady = Function(): Boolean of object;
  TNSServiceMenuGetFilenames = Function(): TStringArray of object;

  TNSThemeChangedHandler = Procedure() of object;

  TNSServiceProvider = objcclass(NSObject)
  public
    onOpenWithNewTab: TNSServiceProviderCallBack;
  public
    procedure openWithNewTab( pboard:NSPasteboard; userData:NSString; error:NSStringPtr ); message 'openWithNewTab:userData:error:';
  end;

  TDCCocoaApplication = objcclass(TCocoaApplication)
    function validRequestorForSendType_returnType (sendType: NSString; returnType: NSString): id; override;
    function writeSelectionToPasteboard_types (pboard: NSPasteboard; types: NSArray): ObjCBOOL; message 'writeSelectionToPasteboard:types:';
    procedure observeValueForKeyPath_ofObject_change_context(keyPath: NSString;
      object_: id; change: NSDictionary; context: pointer); override;
  public
    serviceMenuIsReady: TNSServiceMenuIsReady;
    serviceMenuGetFilenames: TNSServiceMenuGetFilenames;
  end;

procedure InitNSServiceProvider(
  serveCallback: TNSServiceProviderCallBack;
  isReadyFunc: TNSServiceMenuIsReady;
  getFilenamesFunc: TNSServiceMenuGetFilenames );

procedure InitNSThemeChangedObserver( handler: TNSThemeChangedHandler );

var
  NSServiceProvider: TNSServiceProvider;
  NSThemeChangedHandler: TNSThemeChangedHandler;

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
    if Assigned(NSThemeChangedHandler) then NSThemeChangedHandler;
  end;
end;

procedure TNSServiceProvider.openWithNewTab( pboard:NSPasteboard; userData:NSString; error:NSStringPtr );
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

procedure InitNSThemeChangedObserver( handler: TNSThemeChangedHandler );
begin
  if Assigned(NSThemeChangedHandler) then exit;

  NSApp.addObserver_forKeyPath_options_context(
    NSApp, NSSTR('effectiveAppearance'), 0, nil );

  NSThemeChangedHandler:= handler;
end;

end.

