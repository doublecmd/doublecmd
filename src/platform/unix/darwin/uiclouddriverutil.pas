unit uiCloudDriverUtil;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll, CocoaUtils,
  uDCUtils,
  uiCloudDriverConfig;

type
  
  { iCloudDriverUtil }

  iCloudDriverUtil = class
  private
    class function getPlistAppIconNames( const path: String ): NSArray;
  public
    class function createAppImage(const appName: String): NSImage;
  end;

implementation

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

end.

