unit uiCloudDriverUtil;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, fgl, Graphics,
  CocoaAll, CocoaUtils,
  uDCUtils, uMyDarwin,
  uiCloudDriverConfig;

type

  { TiCloudApp }

  TiCloudApp = class
  public
    appName: String;
    displayName: String;
    contentCount: Integer;
    icon: NSImage;
  public
    destructor Destroy; override;
  end;

  TiCloudApps = specialize TFPGObjectList<TiCloudApp>;

  { iCloudDriverUtil }

  iCloudDriverUtil = class
  private
    class function getPlistAppIconNames( const path: String ): NSArray;
  public
    class function createAppImage(const appName: String): NSImage;
    class function createAllApps: TiCloudApps;
    class function getAppFullPath(const appName: String): String;
  end;

implementation

{ TiCloudApp }

destructor TiCloudApp.Destroy;
begin
  icon.release;
end;

{ iCloudDriverUtil }

class function iCloudDriverUtil.getPlistAppIconNames(const path: String ): NSArray;
var
  plistPath: NSString;
  plistData: NSData;
  plistProperties: id;
begin
  Result:= nil;
  plistPath:= StrToNSString( uDCUtils.ReplaceTilde(path) );

  plistData:= NSData.dataWithContentsOfFile( plistPath );
  if plistData = nil then
    Exit;

  plistProperties:= NSPropertyListSerialization.propertyListWithData_options_format_error(
    plistData, NSPropertyListImmutable, nil, nil );
  if plistProperties = nil then
    Exit;

  Result:= plistProperties.valueForKeyPath( NSSTR('BRContainerIcons') );
end;

class function iCloudDriverUtil.createAppImage(const appName: String): NSImage;
var
  appImage: NSImage;
  appFileName: String;
  appPlistPath: String;
  appResourcePath: NSString;
  appIconNames: NSArray;
  appIconName: NSString;
  appIconPath: NSString;
begin
  Result:= nil;

  appFileName:= appName.Replace( '~', '.' );
  appPlistPath:= iCloudDriverConfig.path.container + '/' + appFileName + '.plist';
  appIconNames:= getPlistAppIconNames( appPlistPath );
  if appIconNames = nil then
    Exit;

  appResourcePath:= StrToNSString( uDCUtils.ReplaceTilde(iCloudDriverConfig.path.container) + '/' + appFileName + '/' );

  appImage:= NSImage.new;
  for appIconName in appIconNames do begin
    appIconPath:= appResourcePath.stringByAppendingString(appIconName);
    appIconPath:= appIconPath.stringByAppendingString( NSSTR('.png') );
    appImage.addRepresentation( NSImageRep.imageRepWithContentsOfFile(appIconPath) );
  end;
  Result:= appImage;
end;

class function iCloudDriverUtil.createAllApps: TiCloudApps;
var
  manager: NSFileManager;
  filesInBasePath: NSArray;
  appName: NSString;
  appPath: NSString;
  appBasePath: NSString;
  app: TiCloudApp;

  function pass( appName: NSString ): Boolean;
  var
    driverName: NSString;
  begin
    Result:= True;
    if appName.hasPrefix( NSSTR('.') ) then
      Exit;
    driverName:= NSSTR(iCloudDriverConfig.path.driver);
    driverName:= driverName.lastPathComponent;
    if appName.isEqualToString(driverName) then
      Exit;
    Result:= False;
  end;

  function contentCountOfApp( appPath: NSString ): Integer;
  var
    filesOfApp: NSArray;
  begin
    appPath:= appPath.stringByAppendingString( NSSTR('/Documents') );
    filesOfApp:= manager.contentsOfDirectoryAtPath_error( appPath, nil );
    Result:= filesOfApp.count;
  end;

begin
  Result:= TiCloudApps.Create;
  appBasePath:= NSSTR( IncludeTrailingPathDelimiter(uDCUtils.ReplaceTilde(iCloudDriverConfig.path.base)) );
  manager:= NSFileManager.defaultManager;
  filesInBasePath:= manager.contentsOfDirectoryAtPath_error( appBasePath, nil );
  for appName in filesInBasePath do begin
    if pass(appName) then
      continue;
    appPath:= appBasePath.stringByAppendingString( appName );
    app:= TiCloudApp.Create;
    app.appName:= appName.UTF8String;
    app.displayName:= getMacOSDisplayNameFromPath( appPath.UTF8String );
    app.contentCount:= contentCountOfApp( appPath );
    app.icon:= createAppImage( app.appName );
    Result.Add( app );
  end;
end;

class function iCloudDriverUtil.getAppFullPath(const appName: String): String;
begin
  Result:= IncludeTrailingPathDelimiter(uDCUtils.ReplaceTilde(iCloudDriverConfig.path.base))
         + appName + '/Documents';
end;

end.

