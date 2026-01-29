unit udarwinfileviewhistory;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  SysUtils, Classes, Graphics,
  uFile, uFileSource, uFileSystemFileSource, uSearchResultFileSource, uiCloudDrive,
  uFileView, uPixMapManager, uDCUtils, uGlobs,
  uDarwinUtil, uDarwinImage, uDarwinFile,
  CocoaAll;

type

  TGotoHistoryAction = procedure ( const fsIndex: Integer; const pathIndex: Integer );

  { TDarwinFileViewHistoryUtil }

  TDarwinFileViewHistoryUtil = class
  private
    class function calcTag(const fsIndex: Integer; const pathIndex: Integer): Integer; inline;
    class function getIcon(const fs: IFileSource; const path: String): NSImage;
    class function getDisplayName(const fs: IFileSource; const path: String): String;
  public
    class function createHistoryMenu(
      const direction: Boolean;
      const popupView: NSView;
      const fileView: TFileView;
      const onAction: TGotoHistoryAction;
      const firstCount: Integer ): NSMenu;
  end;

implementation

type

  { THistoryMenuDelegate }

  THistoryMenuDelegate = objcclass( NSObject, NSMenuDelegateProtocol )
  private
    loaded: Boolean;
  private
    procedure menuNeedsUpdate (menu: NSMenu);
  end;

  { THistoryMenu }

  THistoryMenu = objcclass( NSMenu )
  public
    procedure dealloc; override;
  private
    direction: Boolean;
    popupView: NSView;
    fileView: TFileView;
    onAction: TGotoHistoryAction;
    firstCount: Integer;

    procedure addMenuItem(
      const fsIndex: Integer;
      const pathIndex: Integer ); message 'dcAddMenuItem::';
    procedure addShowAllMenuItem; message 'dcAddShowAllMenuItem';

    procedure loadBackwardMenu( const maxMenuCount: Integer ); message 'loadBackwardMenu:';
    procedure loadForwardMenu( const maxMenuCount: Integer ); message 'loadForwardMenu:';

    procedure dcItemAction( const sender: id ); message 'dcItemAction:';
    procedure dcFirstLoad; message 'dcFirstLoad';
    procedure dcShowAll( const sender: id ); message 'dcShowAll:';
  end;

{ THistoryMenuDelgate }

procedure THistoryMenuDelegate.menuNeedsUpdate(menu: NSMenu);
begin
  if self.loaded then
    Exit;
  THistoryMenu(menu).dcFirstLoad;
  self.loaded:= True
end;

{ THistoryMenu }

procedure THistoryMenu.dealloc;
begin
  self.delegate.release;
  self.setDelegate( nil );
  Inherited;
end;

procedure THistoryMenu.addMenuItem(
  const fsIndex: Integer;
  const pathIndex: Integer );
var
  fs: IFileSource;
  menuItem: NSMenuItem;
  tag: Integer;
  path: String;
  displayName: String;
  image: NSImage;
begin
  fs:= self.fileView.FileSources[fsIndex];
  tag:= TDarwinFileViewHistoryUtil.calcTag( fsIndex, pathIndex );
  path:= self.fileView.Path[fsIndex, pathIndex];

  displayName:= TDarwinFileViewHistoryUtil.getDisplayName( fs, path );
  image:= TDarwinFileViewHistoryUtil.getIcon( fs, path );

  menuItem:= NSMenuItem.new;
  menuItem.setTitle( StringToNSString(displayName) );
  menuItem.setImage( image );
  menuItem.setTag( tag );
  menuItem.setTarget( self );
  menuItem.setAction( ObjCSelector('dcItemAction:') );

  self.addItem( menuItem );
  menuItem.release;
end;

procedure THistoryMenu.addShowAllMenuItem;
var
  menuItem: NSMenuItem;
  image: NSImage;
begin
  image:= darwinImageCacheForPath.getNSImageForFileContent(
    mbExpandFileName('$COMMANDER_PATH/pixmaps/macOS/chevron-down-2.png'),
    gIconsInMenusSize,
    True );
  menuItem:= NSMenuItem.new;
  menuItem.setTitle( NSString.string_ );
  menuItem.setImage( image );
  menuItem.setTarget( self );
  menuItem.setAction( ObjCSelector('dcShowAll:') );

  self.addItem( menuItem );
  menuItem.release;
end;

procedure THistoryMenu.dcItemAction(const sender: id);
var
  item: NSMenuItem Absolute sender;
  fsIndex: Integer;
  pathIndex: Integer;
begin
  if NOT NSObject(sender).isKindOfClass(NSMenuItem) then
    Exit;
  fsIndex:= item.tag >> 16;
  pathIndex := item.tag and $FFFF;
  onAction( fsIndex, pathIndex );
end;

procedure THistoryMenu.dcFirstLoad;
begin
  if self.direction then
    self.loadBackwardMenu( self.firstCount )
  else
    self.loadForwardMenu( self.firstCount );
end;

procedure THistoryMenu.dcShowAll(const sender: id);
var
  menu: NSMenu;
begin
  menu:= TDarwinFileViewHistoryUtil.createHistoryMenu(
    self.direction,
    nil,
    self.fileView,
    self.onAction,
    MaxInt );
  menu.popUpMenuPositioningItem_atLocation_inView(
    nil, NSMakePoint(0,2), self.popupView );
  menu.release;
end;

procedure THistoryMenu.loadBackwardMenu( const maxMenuCount: Integer );
var
  fsIndex: Integer;
  count: Integer = 0;

  procedure addHistory;
  var
    pathIndex: Integer;
  begin
    fsIndex:= self.fileView.CurrentFileSourceIndex;
    for pathIndex:= self.fileView.CurrentPathIndex-1 downto 0 do begin
      if count >= maxMenuCount then
        Exit;
      self.addMenuItem( fsIndex, pathIndex );
      inc( count );
    end;

    Dec( fsIndex );
    while fsIndex >= 0 do begin
      for pathIndex:= self.fileView.PathsCount[fsIndex]-1 downto 0 do begin
        if count >= maxMenuCount then
          Exit;
        self.addMenuItem( fsIndex, pathIndex );
        inc( count );
      end;
      Dec( fsIndex );
    end;
  end;

begin
  addHistory;
  if fsIndex >= 0 then
    self.addShowAllMenuItem;
end;

procedure THistoryMenu.loadForwardMenu( const maxMenuCount: Integer );
var
  fsIndex: Integer;
  count: Integer = 0;

  procedure addHistory;
  var
    pathIndex: Integer;
  begin
    fsIndex:= self.fileView.CurrentFileSourceIndex;
    for pathIndex:= self.fileView.CurrentPathIndex+1 to fileView.PathsCount[fsIndex]-1 do begin
      if count >= maxMenuCount then
        Exit;
      self.addMenuItem( fsIndex, pathIndex );
      inc( count );
    end;

    Inc( fsIndex );
    while fsIndex < self.fileView.FileSourcesCount do begin
      for pathIndex:= 0 to self.fileView.PathsCount[fsIndex]-1 do begin
        if count >= maxMenuCount then
          Exit;
        self.addMenuItem( fsIndex, pathIndex );
        inc( count );
      end;
      Inc( fsIndex );
    end;
  end;

begin
  addHistory;
  if fsIndex < self.fileView.FileSourcesCount then
    self.addShowAllMenuItem;
end;

{ TDarwinFileViewHistoryUtil }

class function TDarwinFileViewHistoryUtil.calcTag(
  const fsIndex: Integer;
  const pathIndex: Integer ): Integer;
begin
  Result := (fsIndex << 16) or pathIndex;
end;

class function TDarwinFileViewHistoryUtil.getIcon(
  const fs: IFileSource;
  const path: String ): NSImage;
var
  bitmap: TBitmap;
begin
  bitmap:= fs.GetCustomIcon( path, gIconsInMenusSize );

  if Assigned(bitmap) then begin
    Result:= TDarwinImageUtil.toNSImage( bitmap );
    Result.setSize( NSMakeSize(gIconsInMenusSize,gIconsInMenusSize) );
    bitmap.Free;
    Exit;
  end;

  if fs.IsClass(TFileSystemFileSource) then begin
    Result:= TDarwinImageUtil.getFileIconWithSize( fs.GetRealPath(path), gIconsInMenusSize );
    Exit;
  end;

  Result:= TDarwinImageUtil.getFileIconWithSize( '/usr', gIconsInMenusSize );
end;

class function TDarwinFileViewHistoryUtil.getDisplayName(
  const fs: IFileSource;
  const path: String ): String;
var
  tempFile: TFile;
begin
  if fs.IsClass(TSearchResultFileSource) then begin
    tempFile:= TFile.Create( path );
    Result:= fs.GetDisplayFileName( tempFile );
    tempFile.Free;
    Exit;
  end;

  if fs.IsClass(TFileSystemFileSource) then begin
    Result:= TDarwinFileUtil.getDisplayName( path );
    Exit;
  end;

  Result:= path;
end;

class function TDarwinFileViewHistoryUtil.createHistoryMenu(
  const direction: Boolean;
  const popupView: NSView;
  const fileView: TFileView;
  const onAction: TGotoHistoryAction;
  const firstCount: Integer ): NSMenu;
var
  menu: THistoryMenu;
  delegate: THistoryMenuDelegate;
begin
  menu:= THistoryMenu.new;
  menu.direction:= direction;
  menu.popupView:= popupView;
  menu.fileView:= fileView;
  menu.onAction:= onAction;
  menu.firstCount:= firstCount;

  delegate:= THistoryMenuDelegate.new;
  menu.setDelegate( delegate );

  Result:= menu;
end;

end.

