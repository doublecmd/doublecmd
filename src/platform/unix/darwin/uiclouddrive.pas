unit uiCloudDrive;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, syncobjs, fgl, LazMethodList,
  Menus, Forms, Dialogs, System.UITypes,
  uiCloudDriveConfig, uiCloudDriveUtil,
  uFile, uDisplayFile,
  uFileSource, uFileSourceOperationTypes, uFileSourceManager,
  uFileSourceWatcher, uMountedFileSource, uVfsModule,
  uDCUtils, uLng, uGlobs,
  uDarwinFSWatch, uDarwinSimpleFSWatch, uDarwinDC,
  uDarwinFile, uDarwinImage,
  CocoaAll, CocoaUtils, CocoaThemes;

type

  { TiCloudDriveFileSource }

  TiCloudDriveFileSource = class( TMountedFileSource )
  private
    _appIcons: NSMutableDictionary;
  private
    procedure addAppIcon( const path: String; const appName: String );
    procedure downloadAction(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    class function IsSupportedPath(const Path: String): Boolean; override;

    procedure mountAppPoint( const appName: String );
    function getAppIconByPath( const path: String ): NSImage;
    function getDefaultPointForPath( const path: String ): String; override;
  public
    class function GetFileSource: TiCloudDriveFileSource;

    function GetWatcher: TFileSourceWatcher; override;
    function GetProcessor: TFileSourceProcessor; override;
    function GetUIHandler: TFileSourceUIHandler; override;
    class function GetMainIcon(out Path: String): Boolean; override;

    function GetRootDir(sPath : String): String; override;
    function IsSystemFile(aFile: TFile): Boolean; override;
    function IsPathAtRoot(Path: String): Boolean; override;
    function GetDisplayFileName(aFile: TFile): String; override;
    function QueryContextMenu(AFiles: TFiles; var AMenu: TPopupMenu): Boolean; override;
  end;

implementation

type

  TWatcherItem = class
  public
    path: String;
    filter: TFSWatchFilter;
    eventHandler: TFSWatcherEvent;
    userData: Pointer;
  end;

  TWatcherItems = specialize TFPGObjectList<TWatcherItem>;

  { TiCloudDriveWatcher }

  TiCloudDriveWatcher = class( TDefaultFileSourceWatcher )
  private
    _lockObject: TCriticalSection;
  private
    _watcher: TSimpleDarwinFSWatcher;
    _watcherItems: TWatcherItems;
    _currentItem: TWatcherItem;
    _currentFSEvent: TFSWatcherEventData;
  private
    procedure createWatcher;
    procedure destroyWatcher;
    function findWatch(const path: String; const event: TFSWatcherEvent): Integer;
  private
    function toFileSourceEvent( event: TDarwinFSWatchEvent;
      var fileSourceEvent: TFSWatcherEventData ): Boolean;
    procedure handleEventInMainThread;
    procedure handleEvent( event: TDarwinFSWatchEvent );
  public
    function canWatch(const path: String): Boolean; override;
    function addWatch(const path: String; const filter: TFSWatchFilter;
      const event: TFSWatcherEvent; const UserData: Pointer=nil): Boolean; override;
    procedure removeWatch(const path: String; const event: TFSWatcherEvent); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;
  
  { TiCloudDriveProcessor }

  TiCloudDriveProcessor = class( TMountedFileSourceProcessor )
  public
    procedure consultOperation(var params: TFileSourceConsultParams); override;
  end;

  { TiCloudDriveUIHandler }

  TiCloudDriveUIHandler = class( TFileSourceUIHandler, ICocoaThemeObserver )
  private
    _downloadImage: NSImage;
  private
    procedure createImages;
    procedure releaseImages;
    procedure onThemeChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure draw( var params: TFileSourceUIParams ); override;
    function click( var params: TFileSourceUIParams): Boolean; override;
  end;

  { TSeedFileUtil }

  TSeedFileUtil = class
  private
    class procedure doDownloadDirectory( const path: NSString );
    class procedure doDownload( const path: NSString );
    class procedure download( const aFile: TFile );
    class procedure evict( const aFile: TFile );
  public
    class function isSeedFile( const aFile: TFile ): Boolean;
    class function isSeedFiles( const aFiles: TFiles): Boolean;
    class procedure downloadOrEvict( const fs: IFileSource; const aFile: TFile );
    class procedure downloadOrEvict( const fs: IFileSource; const aFiles: TFiles );
  public
    class function toSeedFilePath( const aFile: TFile ): String;
    class function toNormalFilePath( const aFile: TFile ): String;
  end;

var
  iCloudDriveWatcher: TiCloudDriveWatcher;
  iCloudDriveProcessor: TiCloudDriveProcessor;
  iCloudDriveUIProcessor: TiCloudDriveUIHandler;

{ TiCloudDriveProcessor }

procedure TiCloudDriveProcessor.consultOperation( var params: TFileSourceConsultParams );

  procedure confirmIfSeedFiles;
  var
    dlgResult: TModalResult;
  begin
    if params.phase <> TFileSourceConsultPhase.source then
      Exit;
    if NOT TSeedFileUtil.isSeedFiles(params.files) then
      Exit;
    dlgResult:= MessageDlg(
      rsiCloudDriveCopySeedFileConfirmDlgTitle,
      rsiCloudDriveCopySeedFileConfirmDlgMessage,
      mtConfirmation,
      [mbCancel, mbYes],
      0 );

    if dlgResult <> mrCancel then
      Exit;

    params.consultResult:= fscrCancel;
    params.handled:= True;
  end;

begin
  if params.operationType = fsoCopy then
    confirmIfSeedFiles;

  if params.handled then
    Exit;

  inherited consultOperation(params);
end;

{ TiCloudDriveWatcher }

procedure TiCloudDriveWatcher.createWatcher;
begin
  if _watcher <> nil then
    Exit;

  _watcher:= TSimpleDarwinFSWatcher.Create(
    uDCUtils.ReplaceTilde( iCloudDriveConfig.path.base ),
    @handleEvent );
  _watcher.monitor.watchSubtree:= True;
  _watcher.Start;
end;

procedure TiCloudDriveWatcher.destroyWatcher;
begin
  if _watcher = nil then
    Exit;

  _watcher.stop();
  FreeAndNil( _watcher );
end;

function TiCloudDriveWatcher.findWatch(const path: String; const event: TFSWatcherEvent): Integer;
var
  i: Integer;
  item: TWatcherItem;
begin
  _lockObject.Acquire;
  try
    for i:= 0 to _watcherItems.Count-1 do begin
      item:= _watcherItems[i];
      if item.path<>path then
        continue;
      if NOT SameMethod(TMethod(item.eventHandler), TMethod(event)) then
        continue;
      Exit( i );
    end;
    Result:= -1;
  finally
    _lockObject.Release;
  end;
end;

function TiCloudDriveWatcher.toFileSourceEvent(event: TDarwinFSWatchEvent;
  var fileSourceEvent: TFSWatcherEventData ): Boolean;
begin
  Result:= TDarwinFSWatcherUtil.convertToFileSourceEvent( event, fileSourceEvent );
  if Result = false then
    Exit;

  if TiCloudDriveFileSource.GetFileSource.getMountPointFromPath(event.fullPath)<>nil then begin
    fileSourceEvent.Path:= event.fullPath;
    fileSourceEvent.FileName:= '';
  end else begin
    fileSourceEvent.Path:= ExtractFilePath( event.fullPath );
  end;
end;

procedure TiCloudDriveWatcher.handleEventInMainThread;
begin
  _currentItem.eventHandler( _currentFSEvent );
end;

procedure TiCloudDriveWatcher.handleEvent(event: TDarwinFSWatchEvent);
var
  ok: Boolean;
  virtualPath: String;
  item: TWatcherItem;
  fileSourceEvent: TFSWatcherEventData;
begin
  virtualPath:= TiCloudDriveFileSource.GetFileSource.GetVirtualPath( event.fullPath );
  virtualPath:= ExtractFilePath( ExcludeTrailingPathDelimiter(virtualPath) );
  ok:= Self.toFileSourceEvent( event, fileSourceEvent );
  if NOT ok then
    Exit;

  _lockObject.Acquire;
  try
    for item in _watcherItems do begin
      if virtualPath = item.path then begin
        if not Application.Terminated then begin
          _currentItem:= item;
          _currentFSEvent:= fileSourceEvent;
          _currentFSEvent.UserData:= item.userData;
          _watcher.Synchronize( _watcher, @handleEventInMainThread );
        end;
      end;
    end;
  finally
    _lockObject.Release;
  end;
end;

function TiCloudDriveWatcher.canWatch(const path: String): Boolean;
begin
  Result:= True;
end;

function TiCloudDriveWatcher.addWatch(const path: String;
  const filter: TFSWatchFilter; const event: TFSWatcherEvent;
  const UserData: Pointer): Boolean;
var
  watcherItem: TWatcherItem;
begin
  Result:= True;
  _lockObject.Acquire;
  try
    if self.findWatch(path,event) >= 0 then
      Exit;

    watcherItem:= TWatcherItem.Create;
    watcherItem.path:= path;
    watcherItem.filter:= filter;
    watcherItem.eventHandler:= event;
    watcherItem.userData:= UserData;
    _watcherItems.Add( watcherItem );
    createWatcher;
  finally
    _lockObject.Release;
  end;
end;

procedure TiCloudDriveWatcher.removeWatch(const path: String; const event: TFSWatcherEvent);
var
  index: Integer;
begin
  _lockObject.Acquire;
  try
    index:= self.findWatch( path, event );
    if index < 0 then
      Exit;

    _watcherItems.Delete( index );
    if _watcherItems.count = 0 then
      destroyWatcher;
  finally
    _lockObject.Release;
  end;
end;

constructor TiCloudDriveWatcher.Create;
begin
  _lockObject:= TCriticalSection.Create;;
  _watcherItems:= TWatcherItems.Create;
end;

destructor TiCloudDriveWatcher.Destroy;
begin
  destroyWatcher;
  FreeAndNil( _watcherItems );
  FreeAndNil( _lockObject );
end;

{ TSeedFileUtil }

class procedure TSeedFileUtil.doDownloadDirectory(const path: NSString);
var
  manager: NSFileManager;
  files: NSArray;
  name: NSString;
begin
  manager:= NSFileManager.defaultManager;
  files:= manager.contentsOfDirectoryAtPath_error(
    path, nil );
  for name in files do begin
    doDownload( path.stringByAppendingPathComponent(name) );
  end;
end;

class procedure TSeedFileUtil.doDownload(const path: NSString);
var
  manager: NSFileManager;
  isDirectory: ObjCBOOL;
  url: NSURL;
begin
  manager:= NSFileManager.defaultManager;
  manager.fileExistsAtPath_isDirectory( path, @isDirectory );
  if isDirectory then begin
    doDownloadDirectory( path );
  end else begin
    url:= NSUrl.fileURLWithPath( path );
    manager.startDownloadingUbiquitousItemAtURL_error( url, nil );
  end;
end;

class procedure TSeedFileUtil.download(const aFile: TFile);
var
  path: NSString;
begin
  path:= StrToNSString( aFile.FullPath );
  doDownload( path );
end;

class procedure TSeedFileUtil.evict(const aFile: TFile);
var
  url: NSUrl;
  manager: NSFileManager;
begin
  url:= NSUrl.fileURLWithPath( StrToNSString(aFile.FullPath) );
  manager:= NSFileManager.defaultManager;
  manager.evictUbiquitousItemAtURL_error( url, nil );
end;

class function TSeedFileUtil.isSeedFile(const aFile: TFile): Boolean;
begin
  if aFile.MacOSSpecificProperty <> nil then
    Result:= aFile.MacOSSpecificProperty.IsiCloudSeedFile
  else
    Result:= False;
end;

class function TSeedFileUtil.isSeedFiles(const aFiles: TFiles): Boolean;
var
  i: Integer;
begin
  Result:= False;
  for i:=0 to aFiles.Count-1 do begin
    Result:= isSeedFile( aFiles[i] );
    if Result then
      Exit;
  end;
end;

class procedure TSeedFileUtil.downloadOrEvict(const fs: IFileSource;
  const aFile: TFile);
var
  newPath: String;
  params: TFileSourceEventParams;
begin
  if isSeedFile( aFile ) then begin
    download( aFile );
    newPath:= toNormalFilePath( aFile );
  end else begin
    evict( aFile );
    newPath:= toSeedFilePath( aFile );
  end;

  params.fs:= fs;
  params.eventType:= TFileSourceEventType.queryActive;
  fs.eventNotify( params );
  if params.resultDisplayFile.FSFile.FullPath <> aFile.FullPath then
    Exit;

  params.eventType:= TFileSourceEventType.relocation;
  params.newPath:= newPath;
  fs.eventNotify( params );
end;

class procedure TSeedFileUtil.downloadOrEvict(const fs: IFileSource;
  const aFiles: TFiles);
var
  isSeed: Boolean;
  i: Integer;
  oldPath: String;
  newPath: String;
  params: TFileSourceEventParams;
begin
  oldPath:= EmptyStr;
  newPath:= EmptyStr;

  params.fs:= fs;
  params.eventType:= TFileSourceEventType.queryActive;
  fs.eventNotify( params );
  if params.resultDisplayFile <> nil then
    oldPath:= params.resultDisplayFile.FSFile.FullPath;

  isSeed:= isSeedFiles( aFiles );
  for i:= 0 to aFiles.Count-1 do begin
    if isSeed then begin
      download( aFiles[i] );
      if oldPath = aFiles[i].FullPath then
        newPath:= toNormalFilePath( aFiles[i] );
    end else begin
      evict( aFiles[i] );
      if oldPath = aFiles[i].FullPath then
        newPath:= toSeedFilePath( aFiles[i] );
    end;
  end;

  if newPath = EmptyStr then
    Exit;

  params.eventType:= TFileSourceEventType.relocation;
  params.newPath:= newPath;
  fs.eventNotify( params );
end;

class function TSeedFileUtil.toSeedFilePath(const aFile: TFile): String;
var
  name: String;
begin
  name:= aFile.Name;
  Result:= aFile.Path + '.' + name + '.icloud';
end;

class function TSeedFileUtil.toNormalFilePath(const aFile: TFile): String;
var
  name: String;
begin
  name:= aFile.NameNoExt;
  name:= name.Substring( 1 );
  Result:= aFile.Path + name;
end;

{ TiCloudDriveUIHandler }

procedure TiCloudDriveUIHandler.createImages;
var
  tempImage: NSImage;
begin
  _downloadImage.release;
  tempImage:= NSImage.alloc.initWithContentsOfFile( StrToNSString(mbExpandFileName(iCloudDriveConfig.icon.download)) );
  tempImage.setSize( NSMakeSize(16,16) );
  if TCocoaThemeServices.isDark then begin
    _downloadImage:= TDarwinImageUtil.invertColor( tempImage );
  end else begin
    _downloadImage:= tempImage;
  end;
  _downloadImage.retain;
  tempImage.release;
end;

procedure TiCloudDriveUIHandler.releaseImages;
begin
  _downloadImage.release;
  _downloadImage:= nil;
end;

procedure TiCloudDriveUIHandler.onThemeChanged;
begin
  self.releaseImages;
end;

constructor TiCloudDriveUIHandler.Create;
begin
  Inherited;
  TCocoaThemeServices.addObserver( self );
end;

destructor TiCloudDriveUIHandler.Destroy;
begin
  TCocoaThemeServices.removeObserver( self );
  self.releaseImages;
  Inherited;
end;

procedure TiCloudDriveUIHandler.draw( var params: TFileSourceUIParams );
var
  graphicsContext: NSGraphicsContext;

  procedure drawOverlayAppIcon;
  var
    image: NSImage;
    destRect: NSRect;
    fs: TiCloudDriveFileSource;
  begin
    fs:= params.fs as TiCloudDriveFileSource;
    image:= fs.getAppIconByPath( params.displayFile.FSFile.FullPath );
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
    destRect: NSRect;
  begin
    if NOT TSeedFileUtil.isSeedFile(params.displayFile.FSFile) then
      Exit;

    if _downloadImage = nil then
      createImages;

    destRect.size:= _downloadImage.size;
    destRect.origin.x:= params.drawingRect.Right - Round(_downloadImage.size.width) - 8;
    destRect.origin.y:= params.drawingRect.Top + (params.drawingRect.Height-Round(_downloadImage.size.height))/2;
    params.drawingRect.Right:= Round(destRect.origin.x) - 4;

    _downloadImage.drawInRect_fromRect_operation_fraction_respectFlipped_hints(
      destRect,
      NSZeroRect,
      NSCompositeSourceOver,
      0.5,
      True,
      nil );
  end;

begin
  if params.multiColumns AND (params.col<>0) then
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

function TiCloudDriveUIHandler.click(var params: TFileSourceUIParams): Boolean;
var
  aFile: TFile;
begin
  Result:= False;

  if params.multiColumns AND (params.col<>0) then
    Exit;

  aFile:= params.displayFile.FSFile;
  if NOT TSeedFileUtil.isSeedFile(aFile) then
    Exit;

  if params.x < params.drawingRect.Right - 28 then
    Exit;

  TSeedFileUtil.downloadOrEvict( params.fs, aFile );

  Result:= True;
end;

{ TiCloudDriveFileSource }

constructor TiCloudDriveFileSource.Create;
  procedure addApps;
  var
    i: Integer;
    app: TiCloudDriveConfigAppItem;
  begin
    for i:=0 to Length(iCloudDriveConfig.apps)-1 do begin
      app:= iCloudDriveConfig.apps[i];
      self.mountAppPoint( app.app );
    end;
  end;
begin
  inherited Create;

  FCurrentAddress:= iCloudDriveConfig.scheme;
  _appIcons:= NSMutableDictionary.new;
  addApps;
  self.mount( iCloudDriveConfig.path.drive, '/' );
end;

class function TiCloudDriveFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= Path.StartsWith( iCloudDriveConfig.scheme );
end;

destructor TiCloudDriveFileSource.Destroy;
begin
  _appIcons.release;
  inherited Destroy;
end;

procedure TiCloudDriveFileSource.addAppIcon( const path: String; const appName: String );
var
  image: NSImage;
begin
  image:= iCloudDriveUtil.createAppImage( appName );
  if image = nil then
    Exit;
  _appIcons.setValue_forKey( image, StrToNSString(path) );
  image.release;
end;

procedure TiCloudDriveFileSource.mountAppPoint( const appName: String );
var
  path: String;
begin
  path:= uDCUtils.ReplaceTilde(iCloudDriveConfig.path.base) + '/' + appName + '/Documents/';
  self.mount( path );
  self.addAppIcon( path, appName );
end;

function TiCloudDriveFileSource.getAppIconByPath(const path: String): NSImage;
begin
  Result:= _appIcons.valueForKey( StrToNSString(path) );
end;

function TiCloudDriveFileSource.GetUIHandler: TFileSourceUIHandler;
begin
  Result:= iCloudDriveUIProcessor;
end;

class function TiCloudDriveFileSource.GetMainIcon(out Path: String): Boolean;
begin
  Path:= iCloudDriveConfig.icon.main;
  Result:= True;
end;

procedure TiCloudDriveFileSource.downloadAction(Sender: TObject);
var
  item: TMenuItem absolute Sender;
  files: TFiles;
begin
  files:= TFiles( item.Tag );
  if files = nil then
    Exit;
  TSeedFileUtil.downloadOrEvict( Self, files );
end;

function TiCloudDriveFileSource.getDefaultPointForPath(const path: String): String;
begin
  Result:= TDarwinFileUtil.getDisplayName( path );
end;

class function TiCloudDriveFileSource.GetFileSource: TiCloudDriveFileSource;
var
  aFileSource: IFileSource;
begin
  aFileSource := FileSourceManager.Find(TiCloudDriveFileSource, iCloudDriveConfig.scheme );
  if not Assigned(aFileSource) then
    Result:= TiCloudDriveFileSource.Create
  else
    Result:= aFileSource as TiCloudDriveFileSource;
end;

function TiCloudDriveFileSource.GetWatcher: TFileSourceWatcher;
begin
  Result:= iCloudDriveWatcher;
end;

function TiCloudDriveFileSource.GetProcessor: TFileSourceProcessor;
begin
  Result:= iCloudDriveProcessor;
end;

function TiCloudDriveFileSource.GetRootDir(sPath: String): String;
var
  path: String;
  displayName: String;
begin
  path:= uDCUtils.ReplaceTilde( iCloudDriveConfig.path.drive );
  displayName:= TDarwinFileUtil.getDisplayName( path );
  Result:= PathDelim + displayName + PathDelim;
end;

function TiCloudDriveFileSource.IsSystemFile(aFile: TFile): Boolean;
begin
  Result:= inherited;
  if Result then
    Result:= NOT TSeedFileUtil.isSeedFile( aFile );
end;

function TiCloudDriveFileSource.IsPathAtRoot(Path: String): Boolean;
var
  iCloudPath: String;
  testPath: String;
begin
  Result:= inherited;
  if NOT Result then begin
    iCloudPath:= uDCUtils.ReplaceTilde( iCloudDriveConfig.path.drive );
    testPath:= ExcludeTrailingPathDelimiter( Path );
    Result:= ( testPath=iCloudPath );
  end;
end;

function TiCloudDriveFileSource.GetDisplayFileName(aFile: TFile): String;
begin
  if aFile.Name = '..' then
    Result:= Inherited
  else
    Result:= TDarwinFileUtil.getDisplayName( aFile.FullPath );
end;

function TiCloudDriveFileSource.QueryContextMenu(AFiles: TFiles; var AMenu: TPopupMenu): Boolean;
var
  menuItem: TMenuItem;
begin
  Result:= False;
  if AFiles.Count = 0 then
    Exit;

  menuItem:= TMenuItem.Create( AMenu );

  if TSeedFileUtil.isSeedFiles(AFiles) then
    menuItem.Caption:= rsMnuiCloudDriveDownloadNow
  else
    menuItem.Caption:= rsMnuiCloudDriveRemoveDownload;
  menuItem.OnClick:= @self.downloadAction;
  menuItem.Tag:= PtrInt( AFiles );
  AMenu.Items.Insert(0, menuItem);

  menuItem:= TMenuItem.Create( AMenu );
  menuItem.Caption:= '-';
  AMenu.Items.Insert(1, menuItem);

  Result:= True;
end;

initialization
  iCloudDriveWatcher:= TiCloudDriveWatcher.Create;
  iCloudDriveProcessor:= TiCloudDriveProcessor.Create;
  iCloudDriveUIProcessor:= TiCloudDriveUIHandler.Create;
  RegisterVirtualFileSource( 'iCloud', TiCloudDriveFileSource, True );

finalization
  FreeAndNil( iCloudDriveWatcher );
  FreeAndNil( iCloudDriveProcessor );
  FreeAndNil( iCloudDriveUIProcessor );

end.

