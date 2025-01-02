unit uiCloudDriver;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, Menus,
  uFile, uDisplayFile, uFileSource, uMountedFileSource,
  uDCUtils, uMyDarwin,
  CocoaAll, CocoaUtils, Cocoa_Extra;

type
  { TiCloudDriverFileSource }

  TiCloudDriverFileSource = class( TMountedFileSource )
  private
    _appIcons: NSMutableDictionary;
    _files: TFiles;
  private
    procedure addAppIcon( const path: String; const appName: String );
    procedure downloadAction(Sender: TObject);
  public
    class function isSeedFile(aFile: TFile): Boolean;
    class function isSeedFiles(aFiles: TFiles): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure mountAppPoint( const appName: String );
    function getAppIconByPath( const path: String ): NSImage;
    procedure download( const aFile: TFile );
    procedure download( const files: TFiles );
    function getDefaultPointForPath( const path: String ): String; override;
  public
    function GetUIHandler: TFileSourceUIHandler; override;

    function GetRootDir(sPath : String): String; override;
    function IsSystemFile(aFile: TFile): Boolean; override;
    function IsPathAtRoot(Path: String): Boolean; override;
    function GetDisplayFileName(aFile: TFile): String; override;
    function QueryContextMenu(AFiles: TFiles; var AMenu: TPopupMenu): Boolean; override;
  end;

implementation

const
  iCLOUD_PATH = '~/Library/Mobile Documents';
  iCLOUD_DRIVER_PATH = iCLOUD_PATH + '/com~apple~CloudDocs';
  iCLOUD_CONTAINER_PATH = '~/Library/Application Support/CloudDocs/session/containers';

type
  
  { TiCloudDriverUIHandler }

  TiCloudDriverUIHandler = class( TFileSourceUIHandler )
    procedure draw( var params: TFileSourceUIParams ); override;
    procedure click( var params: TFileSourceUIParams); override;
  end;

var
  iCloudDriverUIProcessor: TiCloudDriverUIHandler;

{ TiCloudDriverUIHandler }

procedure TiCloudDriverUIHandler.draw( var params: TFileSourceUIParams );
var
  graphicsContext: NSGraphicsContext;

  procedure drawOverlayAppIcon;
  var
    image: NSImage;
    destRect: NSRect;
    fs: TiCloudDriverFileSource;
  begin
    fs:= params.fs as TiCloudDriverFileSource;
    if fs.GetCurrentWorkingDirectory <> fs.GetRootDir then
      Exit;

    image:= fs.getAppIconByPath( params.displayFile.FSFile.Path );
    if image = nil then
      Exit;

    destRect:= RectToNSRect( params.iconRect );
    destRect.origin.y:= destRect.origin.y + params.iconRect.Height/16;
    destRect:= NSInsetRect( destRect, params.iconRect.Width/4, params.iconRect.Height/4 );

    image.drawInRect_fromRect_operation_fraction_respectFlipped_hints(
      destRect,
      NSZeroRect,
      NSCompositeSourceOver,
      1,
      True,
      nil );
  end;

  procedure drawDownloadIcon;
  var
    image: NSImage;
    destRect: NSRect;
  begin
    if NOT TiCloudDriverFileSource.isSeedFile(params.displayFile.FSFile) then
      Exit;

    image:= NSImage.imageWithSystemSymbolName_accessibilityDescription(
      NSSTR('icloud.and.arrow.down'), nil );

    if image = nil then
      Exit;

    destRect.size:= image.size;
    destRect.origin.x:= params.drawingRect.Right - Round(image.size.width) - 8;
    destRect.origin.y:= params.drawingRect.Top + (params.drawingRect.Height-Round(image.size.height))/2;
    params.drawingRect.Right:= Round(destRect.origin.x) - 4;

    image.drawInRect_fromRect_operation_fraction_respectFlipped_hints(
      destRect,
      NSZeroRect,
      NSCompositeSourceOver,
      0.5,
      True,
      nil );
  end;

begin
  if params.col <> 0 then
    Exit;

  NSGraphicsContext.classSaveGraphicsState;
  try
    graphicsContext := NSGraphicsContext.graphicsContextWithCGContext_flipped(
      NSGraphicsContext.currentContext.CGContext,
      True );
    NSGraphicsContext.setCurrentContext( graphicsContext );

    drawOverlayAppIcon;
    drawDownloadIcon;
  finally
    NSGraphicsContext.classRestoreGraphicsState;
  end;
end;

procedure TiCloudDriverUIHandler.click(var params: TFileSourceUIParams);
var
  aFile: TFile;
begin
  if params.col <> 0 then
    Exit;

  aFile:= params.displayFile.FSFile;
  if NOT TiCloudDriverFileSource.isSeedFile(aFile) then
    Exit;

  if params.x < params.drawingRect.Right - 28 then
    Exit;

  (params.fs as TiCloudDriverFileSource).download( aFile );
end;

{ TiCloudDriverFileSource }

constructor TiCloudDriverFileSource.Create;
begin
  inherited Create;
  _appIcons:= NSMutableDictionary.new;
end;

destructor TiCloudDriverFileSource.Destroy;
begin
  _appIcons.release;
  FreeAndNil( _files );
  inherited Destroy;
end;

procedure TiCloudDriverFileSource.addAppIcon( const path: String; const appName: String );
  function getPlistAppIconNames( const path: String ): NSArray;
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

  function createAppImage: NSImage;
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
    appPlistPath:= iCLOUD_CONTAINER_PATH + '/' + appFileName + '.plist';
    appIconNames:= getPlistAppIconNames( appPlistPath );
    if appIconNames = nil then
      Exit;

    appResourcePath:= StrToNSString( uDCUtils.ReplaceTilde(iCLOUD_CONTAINER_PATH) + '/' + appFileName + '/' );

    appImage:= NSImage.new;
    for appIconName in appIconNames do begin
      appIconPath:= appResourcePath.stringByAppendingString(appIconName);
      appIconPath:= appIconPath.stringByAppendingString( NSSTR('.png') );
      appImage.addRepresentation( NSImageRep.imageRepWithContentsOfFile(appIconPath) );
    end;
    Result:= appImage;
  end;

var
  image: NSImage;
begin
  image:= createAppImage;
  if image = nil then
    Exit;
  _appIcons.setValue_forKey( image, StrToNSString(path) );
  image.release;
end;

procedure TiCloudDriverFileSource.mountAppPoint( const appName: String );
var
  path: String;
begin
  path:= uDCUtils.ReplaceTilde(iCLOUD_PATH) + '/' + appName + '/Documents/';
  self.mount( path );
  self.addAppIcon( path, appName );
end;

function TiCloudDriverFileSource.getAppIconByPath(const path: String): NSImage;
begin
  Result:= _appIcons.valueForKey( StrToNSString(path) );
end;

procedure TiCloudDriverFileSource.download( const aFile: TFile );
var
  isSeed: Boolean;
  manager: NSFileManager;
  url: NSUrl;
begin
  manager:= NSFileManager.defaultManager;
  isSeed:= isSeedFile( aFile );
  url:= NSUrl.fileURLWithPath( StrToNSString(aFile.FullPath) );
  if isSeed then
    manager.startDownloadingUbiquitousItemAtURL_error( url, nil )
  else
    manager.evictUbiquitousItemAtURL_error( url, nil );
end;

procedure TiCloudDriverFileSource.download( const files: TFiles );
var
  isSeed: Boolean;
  i: Integer;
  manager: NSFileManager;
  url: NSUrl;
begin
  manager:= NSFileManager.defaultManager;
  isSeed:= isSeedFiles( files );
  for i:= 0 to files.Count-1 do begin
    url:= NSUrl.fileURLWithPath( StrToNSString(files[i].FullPath) );
    if isSeed then
      manager.startDownloadingUbiquitousItemAtURL_error( url, nil )
    else
      manager.evictUbiquitousItemAtURL_error( url, nil );
  end;
end;

function TiCloudDriverFileSource.GetUIHandler: TFileSourceUIHandler;
begin
  Result:= iCloudDriverUIProcessor;
end;

procedure TiCloudDriverFileSource.downloadAction(Sender: TObject);
begin
  if _files = nil then
    Exit;
  download( _files );
  FreeAndNil( _files );
end;

class function TiCloudDriverFileSource.isSeedFile(aFile: TFile): Boolean;
begin
  Result:= False;
  if NOT aFile.Name.StartsWith( '.' ) then
    Exit;
  if NOT aFile.Name.EndsWith( '.icloud', True ) then
    Exit;
  Result:= True;
end;

class function TiCloudDriverFileSource.isSeedFiles(aFiles: TFiles): Boolean;
begin
  Result:= isSeedFile( aFiles[0] );
end;

function TiCloudDriverFileSource.getDefaultPointForPath(const path: String): String;
begin
  Result:= getMacOSDisplayNameFromPath( path );
end;

function TiCloudDriverFileSource.GetRootDir(sPath: String): String;
var
  path: String;
  displayName: String;
begin
  path:= uDCUtils.ReplaceTilde( iCLOUD_DRIVER_PATH );
  displayName:= getMacOSDisplayNameFromPath( path );
  Result:= PathDelim + PathDelim + PathDelim + displayName + PathDelim;
end;

function TiCloudDriverFileSource.IsSystemFile(aFile: TFile): Boolean;
begin
  Result:= inherited;
  if Result then
    Result:= NOT isSeedFile( aFile );
end;

function TiCloudDriverFileSource.IsPathAtRoot(Path: String): Boolean;
var
  iCloudPath: String;
  testPath: String;
begin
  Result:= inherited;
  if NOT Result then begin
    iCloudPath:= uDCUtils.ReplaceTilde( iCLOUD_DRIVER_PATH );
    testPath:= ExcludeTrailingPathDelimiter( Path );
    Result:= ( testPath=iCloudPath );
  end;
end;

function TiCloudDriverFileSource.GetDisplayFileName(aFile: TFile): String;
begin
  if aFile.Name = '..' then
    Result:= Inherited
  else
    Result:= getMacOSDisplayNameFromPath( aFile.FullPath );
end;

function TiCloudDriverFileSource.QueryContextMenu(AFiles: TFiles; var AMenu: TPopupMenu): Boolean;
var
  menuItem: TMenuItem;
begin
  Result:= False;
  if AFiles.Count = 0 then
    Exit;

  FreeAndNil( _files );
  _files:= AFiles.clone;

  menuItem:= TMenuItem.Create( AMenu );
  if isSeedFile(AFiles[0]) then
    menuItem.Caption:= 'Download Now'
  else
    menuItem.Caption:= 'Remove Download';
  MenuItem.OnClick:= @self.downloadAction;
  AMenu.Items.Insert(0, menuItem);
  menuItem:= TMenuItem.Create( AMenu );
  menuItem.Caption:= '-';
  AMenu.Items.Insert(1, menuItem);

  Result:= True;
end;

initialization
  iCloudDriverUIProcessor:= TiCloudDriverUIHandler.Create;

finalization
  FreeAndNil( iCloudDriverUIProcessor );

end.

