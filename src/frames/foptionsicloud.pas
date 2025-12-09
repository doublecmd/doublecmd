unit fOptionsiCloud;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Graphics,
  fOptionsFrame, uPixMapManager,
  fMain, uFileSystemFileSource, uFileViewNotebook,
  uiCloudDrive, uiCloudDriveConfig, uiCloudDriveUtil;

type

  { TfrmOptionsiCloud }

  TfrmOptionsiCloud = class(TOptionsEditor)
    appsImageList: TImageList;
    appsListView: TListView;
    procedure appsListViewDblClick(Sender: TObject);
  public
    apps: TiCloudApps;
  public
    class function GetTitle: String; override;
    class function GetIconIndex: Integer; override;
  private
    function isEnabledApp( const appName: String ): Boolean;
  public
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end;

implementation

{$R *.lfm}

{ TfrmOptionsiCloud }

procedure TfrmOptionsiCloud.appsListViewDblClick(Sender: TObject);
var
  fs: IFileSystemFileSource;
  path: String;
  notebook: TFileViewNotebook;
  newPage: TFileViewPage;
begin
  notebook:= frmMain.ActiveNotebook;
  newPage:= notebook.NewPage(Notebook.ActiveView);

  fs:= TFileSystemFileSource.GetFileSource;
  path:= iCloudDriveUtil.getAppFullPath( String(appsListView.Selected.Data) );
  newPage.FileView.AddFileSource( fs, path );
  notebook.PageIndex:= NewPage.PageIndex;
end;

class function TfrmOptionsiCloud.GetTitle: String;
begin
  Result:= 'iCloud';
end;

class function TfrmOptionsiCloud.GetIconIndex: Integer;
begin
  Result:= 43;
end;

function TfrmOptionsiCloud.isEnabledApp(const appName: String): Boolean;
var
  i: Integer;
begin
  for i:= 0 to Length(iCloudDriveConfig.apps)-1 do begin
    if iCloudDriveConfig.apps[i].app = appName then
      Exit( True );
  end;
  Result:= False;
end;

procedure TfrmOptionsiCloud.Load;
var
  app: TiCloudApp;
  item: TListItem;
  icon: TBitmap;
begin
  inherited Load;
  self.apps:= iCloudDriveUtil.createAllApps;
  for app in self.apps do begin
    item:= self.appsListView.Items.Add;
    item.Caption:= app.displayName;
    item.Data:= Pointer( app.appName );
    item.SubItems.Add( IntToStr(app.contentCount) );

    if isEnabledApp(app.appName) then
      item.Checked:= True;

    if app.icon = nil then
      continue;

    icon:= uPixMapManager.NSImageToTBitmap(
      getBestNSImageWithSize(app.icon, self.appsImageList.Width) );
    item.ImageIndex:= self.appsImageList.Count;
    self.appsImageList.Add( icon, nil );
  end;

  self.appsListView.SortColumn:= 1;
  self.appsListView.SortDirection:= sdDescending;
end;

function TfrmOptionsiCloud.Save: TOptionsEditorSaveFlags;
var
  iAllApps: Integer;
  iEnabledApps: Integer;
  item: TiCloudDriveConfigAppItem;
begin
  SetLength( iCloudDriveConfig.apps, appsListView.Items.Count );
  iEnabledApps:= 0;
  for iAllApps:=0 to appsListView.Items.Count-1 do begin
    if NOT appsListView.Items[iAllApps].Checked then
      continue;

    item.name:= appsListView.Items[iAllApps].Caption;
    item.app:= String( appsListView.Items[iAllApps].Data );
    iCloudDriveConfig.apps[iEnabledApps]:= item;
    inc( iEnabledApps );
  end;
  SetLength( iCloudDriveConfig.apps, iEnabledApps );
  iCloudDriveConfigUtil.save;

  Result:= [oesfNeedsRestart];
  FreeAndNil( self.apps );
end;

end.

