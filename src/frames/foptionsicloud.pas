unit fOptionsiCloud;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Graphics,
  fOptionsFrame, uPixMapManager,
  uiCloudDriverUtil;

type

  { TfrmOptionsiCloud }

  TfrmOptionsiCloud = class(TOptionsEditor)
    appsImageList: TImageList;
    appsListView: TListView;
  public
    apps: TiCloudApps;
  public
    class function GetTitle: String; override;
    class function GetIconIndex: Integer; override;
  public
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end;

implementation

{$R *.lfm}

{ TfrmOptionsiCloud }

class function TfrmOptionsiCloud.GetTitle: String;
begin
  Result:= 'iCloud';
end;

class function TfrmOptionsiCloud.GetIconIndex: Integer;
begin
  Result:= 43;
end;

procedure TfrmOptionsiCloud.Load;
var
  app: TiCloudApp;
  item: TListItem;
  icon: TBitmap;
begin
  inherited Load;
  self.apps:= iCloudDriverUtil.createAllApps;
  for app in self.apps do begin
    item:= self.appsListView.Items.Add;
    item.Caption:= app.displayName;
    item.SubItems.Add( IntToStr(app.contentCount) );

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
begin
  Result:= [oesfNeedsRestart];
  FreeAndNil( self.apps );
end;

end.

