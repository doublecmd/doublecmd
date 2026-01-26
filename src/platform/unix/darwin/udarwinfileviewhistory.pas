unit udarwinfileviewhistory;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  SysUtils, Classes, Graphics,
  uFile, uFileSource, uFileSystemFileSource, uSearchResultFileSource, uiCloudDrive,
  uFileView, uPixMapManager,
  uGlobs,
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
    class procedure addMenuItem(
      const menu: NSMenu;
      const fileView: TFileView;
      const fsIndex: Integer;
      const pathIndex: Integer );
  public
    class function createBackwardMenu(
      const fileView: TFileView;
      const onAction: TGotoHistoryAction ): NSMenu;
    class function createForwardMenu(
      const fileView: TFileView;
      const onAction: TGotoHistoryAction ): NSMenu;
  end;

implementation

type
  
  { TMenuItem }

  THistoryMenu = objcclass( NSMenu )
  private
    onAction: TGotoHistoryAction;
    procedure dcItemAction( const sender: id ); message 'dcItemAction:';
  end;

{ TMenuItem }

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
  Writeln( '++++ click: ', item.tag );
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

class procedure TDarwinFileViewHistoryUtil.addMenuItem(
  const menu: NSMenu;
  const fileView: TFileView;
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
  fs:= fileView.FileSources[fsIndex];
  tag:= calcTag(fsIndex,pathIndex);
  path:= fileView.Path[fsIndex, pathIndex];

  displayName:= TDarwinFileViewHistoryUtil.getDisplayName( fs, path );
  image:= TDarwinFileViewHistoryUtil.getIcon( fs, path );

  menuItem:= NSMenuItem.new;
  menuItem.setTitle( StringToNSString(displayName) );
  menuItem.setImage( image );
  menuItem.setTag( tag );
  menuItem.setTarget( menu );
  menuItem.setAction( ObjCSelector('dcItemAction:') );

  menu.addItem( menuItem );
  menuItem.release;
end;

class function TDarwinFileViewHistoryUtil.createBackwardMenu(
  const fileView: TFileView;
  const onAction: TGotoHistoryAction ): NSMenu;
var
  fsIndex: Integer;
  pathIndex: Integer;
  menu: THistoryMenu;
begin
  menu:= THistoryMenu.new;
  menu.onAction:= onAction;

  fsIndex:= fileView.CurrentFileSourceIndex;
  for pathIndex:= fileView.CurrentPathIndex-1 downto 0 do
    addMenuItem( menu, fileView, fsIndex, pathIndex );

  Dec( fsIndex );
  while fsIndex >= 0 do begin
    for pathIndex:= fileView.PathsCount[fsIndex]-1 downto 0 do
      addMenuItem( menu, fileView, fsIndex, pathIndex );
    Dec( fsIndex );
  end;

  if menu.numberOfItems = 0 then begin
    menu.release;
    Result:= nil;
  end else begin
    Result:= menu;
  end;
end;

class function TDarwinFileViewHistoryUtil.createForwardMenu(
  const fileView: TFileView;
  const onAction: TGotoHistoryAction ): NSMenu;
var
  fsIndex: Integer;
  pathIndex: Integer;
  menu: THistoryMenu;
begin
  menu:= THistoryMenu.new;
  menu.onAction:= onAction;

  fsIndex:= fileView.CurrentFileSourceIndex;
  for pathIndex:= fileView.CurrentPathIndex+1 to fileView.PathsCount[fsIndex]-1 do
    addMenuItem( menu, fileView, fsIndex, pathIndex );

  Inc( fsIndex );
  while fsIndex < fileView.FileSourcesCount do begin
    for pathIndex:= 0 to fileView.PathsCount[fsIndex]-1 do
      addMenuItem( menu, fileView, fsIndex, pathIndex );
    Inc( fsIndex );
  end;

  if menu.numberOfItems = 0 then begin
    menu.release;
    Result:= nil;
  end else begin
    Result:= menu;
  end;
end;

end.

