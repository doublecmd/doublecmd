unit uiCloudDriver;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Menus,
  uFile, uMountedFileSource,
  uDCUtils, uMyDarwin,
  CocoaAll, CocoaUtils;

type
  { TiCloudDriverFileSource }

  TiCloudDriverFileSource = class(TMountedFileSource)
  private
    _files: TFiles;
  private
    procedure downloadAction(Sender: TObject);
  public
    class function isSeedFile(aFile: TFile): Boolean;
    class function isSeedFiles(aFiles: TFiles): Boolean;
  public
    destructor Destroy; override;

    function getDefaultPointForPath(const path: String): String; override;
    function GetRootDir(sPath : String): String; override;
    function IsSystemFile(aFile: TFile): Boolean; override;
    function IsPathAtRoot(Path: String): Boolean; override;
    function GetDisplayFileName(aFile: TFile): String; override;
    function QueryContextMenu(AFiles: TFiles; var AMenu: TPopupMenu): Boolean; override;
  end;

implementation

const
  iCLOUD_DRIVER_PATH = '~/Library/Mobile Documents/com~apple~CloudDocs';

{ TiCloudDriverFileSource }

destructor TiCloudDriverFileSource.Destroy;
begin
  FreeAndNil( _files );
  inherited Destroy;
end;

procedure TiCloudDriverFileSource.downloadAction(Sender: TObject);
var
  isSeed: Boolean;
  i: Integer;
  manager: NSFileManager;
  url: NSUrl;
begin
  if _files = nil then
    Exit;

  manager:= NSFileManager.defaultManager;
  isSeed:= isSeedFiles( _files );
  for i:= 0 to _files.Count-1 do begin
    url:= NSUrl.fileURLWithPath( StrToNSString(_files[i].FullPath) );
    if isSeed then
      manager.startDownloadingUbiquitousItemAtURL_error( url, nil )
    else
      manager.evictUbiquitousItemAtURL_error( url, nil );
  end;

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

end.

