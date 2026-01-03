unit uiCloudDriveUtil;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, fgl, Graphics,
  CocoaAll,
  uDCUtils, uDarwinFile, uDarwinUtil, uiCloudDriveConfig;

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

  { iCloudDriveUtil }

  iCloudDriveUtil = class
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

{ iCloudDriveUtil }

class function iCloudDriveUtil.getPlistAppIconNames(const path: String ): NSArray;
var
  plistPath: String;
  plistData: NSData;
  plistProperties: id;
  error: NSError = nil;
begin
  Result:= nil;
  plistPath:= uDCUtils.ReplaceTilde( path );

  plistData:= TDarwinFileUtil.dataWithContentsOfFile( plistPath, 'iCloudDriveUtil.getPlistAppIconNames()' );
  if plistData = nil then
    Exit;

  plistProperties:= NSPropertyListSerialization.propertyListWithData_options_format_error(
    plistData, NSPropertyListImmutable, nil, @error );
  if plistProperties = nil then begin
    logDarwinError( 'iCloudDriveUtil.getPlistAppIconNames', error );
    Exit;
  end;

  Result:= plistProperties.valueForKeyPath( NSSTR('BRContainerIcons') );
end;

class function iCloudDriveUtil.createAppImage(const appName: String): NSImage;
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
  appPlistPath:= iCloudDriveConfig.path.container + '/' + appFileName + '.plist';
  appIconNames:= getPlistAppIconNames( appPlistPath );
  if appIconNames = nil then
    Exit;

  appResourcePath:= StringToNSString( uDCUtils.ReplaceTilde(iCloudDriveConfig.path.container) + '/' + appFileName + '/' );

  appImage:= NSImage.new;
  for appIconName in appIconNames do begin
    appIconPath:= appResourcePath.stringByAppendingString(appIconName);
    appIconPath:= appIconPath.stringByAppendingString( NSSTR('.png') );
    appImage.addRepresentation( NSImageRep.imageRepWithContentsOfFile(appIconPath) );
  end;
  Result:= appImage;
end;

class function iCloudDriveUtil.createAllApps: TiCloudApps;
var
  manager: NSFileManager;
  filesInBasePath: NSArray;
  appName: NSString;
  appPath: NSString;
  appBasePath: NSString;
  app: TiCloudApp;
  error: NSError = nil;

  function pass( appName: NSString ): Boolean;
  var
    driveName: NSString;
  begin
    Result:= True;
    if appName.hasPrefix( NSSTR('.') ) then
      Exit;
    driveName:= NSSTR(iCloudDriveConfig.path.drive);
    driveName:= driveName.lastPathComponent;
    if appName.isEqualToString(driveName) then
      Exit;
    Result:= False;
  end;

  function contentCountOfApp( appPath: NSString ): Integer;
  var
    filesOfApp: NSArray;
    error: NSError = nil;
  begin
    appPath:= appPath.stringByAppendingString( NSSTR('/Documents') );
    filesOfApp:= manager.contentsOfDirectoryAtPath_error( appPath, @error );
    if filesOfApp = nil then
      logDarwinError( 'iCloudDriveUtil.createAllApps.contentCountOfApp', error );
    Result:= filesOfApp.count;
  end;

begin
  Result:= TiCloudApps.Create;
  appBasePath:= NSSTR( IncludeTrailingPathDelimiter(uDCUtils.ReplaceTilde(iCloudDriveConfig.path.base)) );
  manager:= NSFileManager.defaultManager;
  filesInBasePath:= manager.contentsOfDirectoryAtPath_error( appBasePath, @error );
  if filesInBasePath = nil then begin
    logDarwinError( 'iCloudDriveUtil.createAllApps', error );
    Exit;
  end;

  for appName in filesInBasePath do begin
    if pass(appName) then
      continue;
    appPath:= appBasePath.stringByAppendingString( appName );
    app:= TiCloudApp.Create;
    app.appName:= appName.UTF8String;
    app.displayName:= TDarwinFileUtil.getDisplayName( appPath.UTF8String );
    app.contentCount:= contentCountOfApp( appPath );
    app.icon:= createAppImage( app.appName );
    Result.Add( app );
  end;
end;

class function iCloudDriveUtil.getAppFullPath(const appName: String): String;
begin
  Result:= IncludeTrailingPathDelimiter(uDCUtils.ReplaceTilde(iCloudDriveConfig.path.base))
         + appName + '/Documents';
end;

end.

