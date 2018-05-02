unit dmCommonData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs; 

type

  { TdmComData }

  TdmComData = class(TDataModule)
    ilEditorImages: TImageList;
    ImageList: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure LoadEditorImageList;
  public
    { public declarations }
  end; 

var
  dmComData: TdmComData;

implementation

uses
  LCLVersion, uGraphics, uPixMapManager;

{$R *.lfm}

{ TdmComData }

procedure TdmComData.DataModuleCreate(Sender: TObject);
begin
  if Assigned(PixMapManager) then
  begin
    LoadEditorImageList;
  end;
end;

procedure TdmComData.LoadEditorImageList;
begin
{$if lcl_fullversion >= 1070000}
  ilEditorImages.Width:= AdjustIconSize(ilEditorImages.Width, 96);
  ilEditorImages.Height:= AdjustIconSize(ilEditorImages.Height, 96);
{$endif}
  ilEditorImages.LoadThemeIcon(0, 'document-new');
  ilEditorImages.LoadThemeIcon(1, 'document-open');
  ilEditorImages.LoadThemeIcon(2, 'document-save');
  ilEditorImages.LoadThemeIcon(3, 'document-save-as');
  ilEditorImages.LoadThemeIcon(4, 'document-properties');
  ilEditorImages.LoadThemeIcon(5, 'edit-cut');
  ilEditorImages.LoadThemeIcon(6, 'edit-copy');
  ilEditorImages.LoadThemeIcon(7, 'edit-paste');
  ilEditorImages.LoadThemeIcon(8, 'edit-undo');
  ilEditorImages.LoadThemeIcon(9, 'edit-redo');
  ilEditorImages.LoadThemeIcon(10, 'edit-find');
  ilEditorImages.LoadThemeIcon(11, 'edit-find-replace');
  ilEditorImages.LoadThemeIcon(12, 'application-exit');
  ilEditorImages.LoadThemeIcon(13, 'help-about');
  ilEditorImages.LoadThemeIcon(14, 'edit-delete');
  ilEditorImages.LoadThemeIcon(15, 'edit-select-all');
  ilEditorImages.LoadThemeIcon(16, 'go-jump');
  ilEditorImages.LoadThemeIcon(17, 'view-refresh');
end;

end.

