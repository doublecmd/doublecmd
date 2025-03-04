unit uiCloudDriver;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, syncobjs, fgl, LazMethodList,
  Menus, Forms, Dialogs, System.UITypes,
  uiCloudDriverConfig, uiCloudDriverUtil,
  uFile, uDisplayFile,
  uFileSource, uFileSourceOperationTypes, uFileSourceManager,
  uFileSourceWatcher, uMountedFileSource, uVfsModule,
  uDCUtils, uLng, uGlobs,
  uMyDarwin, uDarwinFSWatch,
  CocoaAll, CocoaUtils;

type

  { TiCloudDriverFileSource }

  TiCloudDriverFileSource = class( TMountedFileSource )
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
    class function GetFileSource: TiCloudDriverFileSource;

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

  { TiCloudDriverWatcher }

  TiCloudDriverWatcher = class( TDefaultFileSourceWatcher )
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
    function toFileSourceEventCommon( event: TDarwinFSWatchEvent;
      var fileSourceEvent: TFSWatcherEventData ): Boolean;
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
  
  { TiCloudDriverProcessor }

  TiCloudDriverProcessor = class( TMountedFileSourceProcessor )
  public
    procedure consultOperation(var params: TFileSourceConsultParams); override;
  end;

  { TiCloudDriverUIHandler }

  TiCloudDriverUIHandler = class( TFileSourceUIHandler )
    procedure draw( var params: TFileSourceUIParams ); override;
    procedure click( var params: TFileSourceUIParams); override;
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
  iCloudDriverWatcher: TiCloudDriverWatcher;
  iCloudDriverProcessor: TiCloudDriverProcessor;
  iCloudDriverUIProcessor: TiCloudDriverUIHandler;
  iCloudArrowDownImage: NSImage;

{ TiCloudDriverProcessor }

procedure TiCloudDriverProcessor.consultOperation( var params: TFileSourceConsultParams );

  procedure confirmIfSeedFiles;
  var
    dlgResult: TModalResult;
  begin
    if params.phase <> TFileSourceConsultPhase.source then
      Exit;
    if NOT TSeedFileUtil.isSeedFiles(params.files) then
      Exit;
    dlgResult:= MessageDlg(
      rsiCloudDriverCopySeedFileConfirmDlgTitle,
      rsiCloudDriverCopySeedFileConfirmDlgMessage,
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

{ TiCloudDriverWatcher }

procedure TiCloudDriverWatcher.createWatcher;
begin
  if _watcher <> nil then
    Exit;

  _watcher:= TSimpleDarwinFSWatcher.Create(
    uDCUtils.ReplaceTilde( iCloudDriverConfig.path.base ),
    @handleEvent );
  _watcher.monitor.watchSubtree:= True;
  _watcher.Start;
end;

procedure TiCloudDriverWatcher.destroyWatcher;
begin
  if _watcher = nil then
    Exit;

  _watcher.stop();
  FreeAndNil( _watcher );
end;

function TiCloudDriverWatcher.findWatch(const path: String; const event: TFSWatcherEvent): Integer;
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

// todo: refactor with TFileSystemWatcherImpl.handleFSEvent(event:TDarwinFSWatchEvent);
function TiCloudDriverWatcher.toFileSourceEventCommon(event: TDarwinFSWatchEvent;
  var fileSourceEvent: TFSWatcherEventData ): Boolean;
begin
  Result:= False;
  if [watch_file_name_change, watch_attributes_change] * gWatchDirs = [] then exit;
  if event.isDropabled then exit;
///  if (ecChildChanged in event.categories) and (not isWatchSubdir(event.watchPath) ) then exit;

  fileSourceEvent.Path := event.watchPath;
  fileSourceEvent.FileName := EmptyStr;
  fileSourceEvent.NewFileName := EmptyStr;
  fileSourceEvent.OriginalEvent := event;
  fileSourceEvent.EventType := fswUnknownChange;

  if TDarwinFSWatchEventCategory.ecRootChanged in event.categories then begin
    fileSourceEvent.EventType := fswSelfDeleted;
  end else if event.fullPath.Length >= event.watchPath.Length+2 then begin
    // 1. file-level update only valid if there is a FileName,
    //    otherwise keep directory-level update
    // 2. the order of the following judgment conditions must be preserved
    if (not (watch_file_name_change in gWatchDirs)) and
       ([ecStructChanged, ecAttribChanged] * event.categories = [ecStructChanged])
         then exit;
    if (not (watch_attributes_change in gWatchDirs)) and
       ([ecStructChanged, ecAttribChanged] * event.categories = [ecAttribChanged])
         then exit;

    fileSourceEvent.FileName := ExtractFileName( event.fullPath );

    if TDarwinFSWatchEventCategory.ecRemoved in event.categories then
      fileSourceEvent.EventType := fswFileDeleted
    else if TDarwinFSWatchEventCategory.ecRenamed in event.categories then begin
      if ExtractFilePath(event.fullPath)=ExtractFilePath(event.renamedPath) then begin
        // fswFileRenamed only when FileName and NewFileName in the same dir
        // otherwise keep fswUnknownChange
        fileSourceEvent.EventType := fswFileRenamed;
        fileSourceEvent.NewFileName := ExtractFileName( event.renamedPath );
      end;
    end else if TDarwinFSWatchEventCategory.ecCreated in event.categories then
      fileSourceEvent.EventType := fswFileCreated
    else if TDarwinFSWatchEventCategory.ecAttribChanged in event.categories then
      fileSourceEvent.EventType := fswFileChanged
    else
      exit;
  end;

  Result:= True;
end;

function TiCloudDriverWatcher.toFileSourceEvent(event: TDarwinFSWatchEvent;
  var fileSourceEvent: TFSWatcherEventData ): Boolean;
begin
  Result:= Self.toFileSourceEventCommon( event, fileSourceEvent );
  if Result = false then
    Exit;

  if TiCloudDriverFileSource.GetFileSource.getMountPointFromPath(event.fullPath)<>nil then begin
    fileSourceEvent.Path:= event.fullPath;
    fileSourceEvent.FileName:= '';
  end else begin
    fileSourceEvent.Path:= ExtractFilePath( event.fullPath );
  end;
end;

procedure TiCloudDriverWatcher.handleEventInMainThread;
begin
  _currentItem.eventHandler( _currentFSEvent );
end;

procedure TiCloudDriverWatcher.handleEvent(event: TDarwinFSWatchEvent);
var
  ok: Boolean;
  virtualPath: String;
  item: TWatcherItem;
  fileSourceEvent: TFSWatcherEventData;
begin
  virtualPath:= TiCloudDriverFileSource.GetFileSource.GetVirtualPath( event.fullPath );
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

function TiCloudDriverWatcher.canWatch(const path: String): Boolean;
begin
  Result:= True;
end;

function TiCloudDriverWatcher.addWatch(const path: String;
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

procedure TiCloudDriverWatcher.removeWatch(const path: String; const event: TFSWatcherEvent);
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

constructor TiCloudDriverWatcher.Create;
begin
  _lockObject:= TCriticalSection.Create;;
  _watcherItems:= TWatcherItems.Create;
end;

destructor TiCloudDriverWatcher.Destroy;
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

    if iCloudArrowDownImage = nil then begin
      iCloudArrowDownImage:= NSImage.alloc.initWithContentsOfFile( StrToNSString(mbExpandFileName(iCloudDriverConfig.icon.download)) );
      iCloudArrowDownImage.setSize( NSMakeSize(16,16) );
    end;

    destRect.size:= iCloudArrowDownImage.size;
    destRect.origin.x:= params.drawingRect.Right - Round(iCloudArrowDownImage.size.width) - 8;
    destRect.origin.y:= params.drawingRect.Top + (params.drawingRect.Height-Round(iCloudArrowDownImage.size.height))/2;
    params.drawingRect.Right:= Round(destRect.origin.x) - 4;

    iCloudArrowDownImage.drawInRect_fromRect_operation_fraction_respectFlipped_hints(
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

procedure TiCloudDriverUIHandler.click(var params: TFileSourceUIParams);
var
  aFile: TFile;
begin
  if params.multiColumns AND (params.col<>0) then
    Exit;

  aFile:= params.displayFile.FSFile;
  if NOT TSeedFileUtil.isSeedFile(aFile) then
    Exit;

  if params.x < params.drawingRect.Right - 28 then
    Exit;

  TSeedFileUtil.downloadOrEvict( params.fs, aFile );
end;

{ TiCloudDriverFileSource }

constructor TiCloudDriverFileSource.Create;
  procedure addApps;
  var
    i: Integer;
    app: TiCloudDriverConfigAppItem;
  begin
    for i:=0 to Length(iCloudDriverConfig.apps)-1 do begin
      app:= iCloudDriverConfig.apps[i];
      self.mountAppPoint( app.app );
    end;
  end;
begin
  inherited Create;

  FCurrentAddress:= iCloudDriverConfig.scheme;
  _appIcons:= NSMutableDictionary.new;
  addApps;
  self.mount( iCloudDriverConfig.path.driver, '/' );
end;

class function TiCloudDriverFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= Path.StartsWith( iCloudDriverConfig.scheme );
end;

destructor TiCloudDriverFileSource.Destroy;
begin
  _appIcons.release;
  inherited Destroy;
end;

procedure TiCloudDriverFileSource.addAppIcon( const path: String; const appName: String );
var
  image: NSImage;
begin
  image:= iCloudDriverUtil.createAppImage( appName );
  if image = nil then
    Exit;
  _appIcons.setValue_forKey( image, StrToNSString(path) );
  image.release;
end;

procedure TiCloudDriverFileSource.mountAppPoint( const appName: String );
var
  path: String;
begin
  path:= uDCUtils.ReplaceTilde(iCloudDriverConfig.path.base) + '/' + appName + '/Documents/';
  self.mount( path );
  self.addAppIcon( path, appName );
end;

function TiCloudDriverFileSource.getAppIconByPath(const path: String): NSImage;
begin
  Result:= _appIcons.valueForKey( StrToNSString(path) );
end;

function TiCloudDriverFileSource.GetUIHandler: TFileSourceUIHandler;
begin
  Result:= iCloudDriverUIProcessor;
end;

class function TiCloudDriverFileSource.GetMainIcon(out Path: String): Boolean;
begin
  Path:= iCloudDriverConfig.icon.main;
  Result:= True;
end;

procedure TiCloudDriverFileSource.downloadAction(Sender: TObject);
var
  item: TMenuItem absolute Sender;
  files: TFiles;
begin
  files:= TFiles( item.Tag );
  if files = nil then
    Exit;
  TSeedFileUtil.downloadOrEvict( Self, files );
end;

function TiCloudDriverFileSource.getDefaultPointForPath(const path: String): String;
begin
  Result:= getMacOSDisplayNameFromPath( path );
end;

class function TiCloudDriverFileSource.GetFileSource: TiCloudDriverFileSource;
var
  aFileSource: IFileSource;
begin
  aFileSource := FileSourceManager.Find(TiCloudDriverFileSource, iCloudDriverConfig.scheme );
  if not Assigned(aFileSource) then
    Result:= TiCloudDriverFileSource.Create
  else
    Result:= aFileSource as TiCloudDriverFileSource;
end;

function TiCloudDriverFileSource.GetWatcher: TFileSourceWatcher;
begin
  Result:= iCloudDriverWatcher;
end;

function TiCloudDriverFileSource.GetProcessor: TFileSourceProcessor;
begin
  Result:= iCloudDriverProcessor;
end;

function TiCloudDriverFileSource.GetRootDir(sPath: String): String;
var
  path: String;
  displayName: String;
begin
  path:= uDCUtils.ReplaceTilde( iCloudDriverConfig.path.driver );
  displayName:= getMacOSDisplayNameFromPath( path );
  Result:= PathDelim + displayName + PathDelim;
end;

function TiCloudDriverFileSource.IsSystemFile(aFile: TFile): Boolean;
begin
  Result:= inherited;
  if Result then
    Result:= NOT TSeedFileUtil.isSeedFile( aFile );
end;

function TiCloudDriverFileSource.IsPathAtRoot(Path: String): Boolean;
var
  iCloudPath: String;
  testPath: String;
begin
  Result:= inherited;
  if NOT Result then begin
    iCloudPath:= uDCUtils.ReplaceTilde( iCloudDriverConfig.path.driver );
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

  menuItem:= TMenuItem.Create( AMenu );

  if TSeedFileUtil.isSeedFiles(AFiles) then
    menuItem.Caption:= rsMnuiCloudDriverDownloadNow
  else
    menuItem.Caption:= rsMnuiCloudDriverRemoveDownload;
  menuItem.OnClick:= @self.downloadAction;
  menuItem.Tag:= PtrInt( AFiles );
  AMenu.Items.Insert(0, menuItem);

  menuItem:= TMenuItem.Create( AMenu );
  menuItem.Caption:= '-';
  AMenu.Items.Insert(1, menuItem);

  Result:= True;
end;

initialization
  iCloudDriverWatcher:= TiCloudDriverWatcher.Create;
  iCloudDriverProcessor:= TiCloudDriverProcessor.Create;
  iCloudDriverUIProcessor:= TiCloudDriverUIHandler.Create;
  RegisterVirtualFileSource( 'iCloud', TiCloudDriverFileSource, True );

finalization
  FreeAndNil( iCloudDriverWatcher );
  FreeAndNil( iCloudDriverProcessor );
  FreeAndNil( iCloudDriverUIProcessor );
  iCloudArrowDownImage.release;

end.

