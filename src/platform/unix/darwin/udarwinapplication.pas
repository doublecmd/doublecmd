unit uDarwinApplication;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, fgl,
  CocoaAll, CocoaInt, Cocoa_Extra, CocoaUtils,
  uDarwinUtil;

type
  // MacOS Service Integration
  TDarwinServiceProviderCallBack = Procedure( filenames:TStringList ) of object;
  TDarwinServiceMenuIsReadyFunc = Function(): Boolean of object;
  TDarwinServiceMenuGetFilenamesFunc = Function(): TStringArray of object;

  TDarwinThemeObserver = Procedure() of object;
  TDarwinThemeObservers = specialize TFPGList<TDarwinThemeObserver>;

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
    procedure observeValueForKeyPath_ofObject_change_context(keyPath: NSString;
      object_: id; change: NSDictionary; context_: pointer); override;
  private
    _serviceProvider: TDarwinServiceProvider;
  public
    serviceMenuIsReady: TDarwinServiceMenuIsReadyFunc;
    serviceMenuGetFilenames: TDarwinServiceMenuGetFilenamesFunc;
  end;

  { TDarwinApplicationUtil }

  TDarwinApplicationUtil = class
  private
    class var _themeObservers: TDarwinThemeObservers;
  private
    class procedure init;
    class procedure deinit;
    class procedure themeNotify;
  public
    class procedure initServiceProvider(
      serveCallback: TDarwinServiceProviderCallBack;
      isReadyFunc: TDarwinServiceMenuIsReadyFunc;
      getFilenamesFunc: TDarwinServiceMenuGetFilenamesFunc );

    class procedure addThemeObserver( const observer: TDarwinThemeObserver );
  end;

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

procedure TDCCocoaApplication.observeValueForKeyPath_ofObject_change_context(
  keyPath: NSString; object_: id; change: NSDictionary; context_: pointer);
begin
  Inherited observeValueForKeyPath_ofObject_change_context( keyPath, object_, change, context_ );
  if keyPath.isEqualToString(NSSTR('effectiveAppearance')) then
  begin
    NSAppearance.setCurrentAppearance( self.appearance );
    TDarwinApplicationUtil.themeNotify;
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

{ TDarwinApplicationUtil }

class procedure TDarwinApplicationUtil.init;
begin
  _themeObservers:= TDarwinThemeObservers.Create;
end;

class procedure TDarwinApplicationUtil.deinit;
begin
  FreeAndNil( _themeObservers );
end;

class procedure TDarwinApplicationUtil.themeNotify;
var
  i: Integer;
begin
  if _themeObservers = nil then
    Exit;
  for i:= 0 to _themeObservers.Count-1 do begin
    _themeObservers[i]();
  end;
end;

class procedure TDarwinApplicationUtil.addThemeObserver( const observer: TDarwinThemeObserver );
begin
  _themeObservers.Add( observer );
end;

class procedure TDarwinApplicationUtil.initServiceProvider(
  serveCallback: TDarwinServiceProviderCallBack;
  isReadyFunc: TDarwinServiceMenuIsReadyFunc;
  getFilenamesFunc: TDarwinServiceMenuGetFilenamesFunc );
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

initialization
  TDarwinApplicationUtil.init;

finalization
  TDarwinApplicationUtil.deinit;

end.

