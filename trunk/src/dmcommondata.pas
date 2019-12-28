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
  LoadThemeIcon(ilEditorImages, 0, 'document-new');
  LoadThemeIcon(ilEditorImages, 1, 'document-open');
  LoadThemeIcon(ilEditorImages, 2, 'document-save');
  LoadThemeIcon(ilEditorImages, 3, 'document-save-as');
  LoadThemeIcon(ilEditorImages, 4, 'document-properties');
  LoadThemeIcon(ilEditorImages, 5, 'edit-cut');
  LoadThemeIcon(ilEditorImages, 6, 'edit-copy');
  LoadThemeIcon(ilEditorImages, 7, 'edit-paste');
  LoadThemeIcon(ilEditorImages, 8, 'edit-undo');
  LoadThemeIcon(ilEditorImages, 9, 'edit-redo');
  LoadThemeIcon(ilEditorImages, 10, 'edit-find');
  LoadThemeIcon(ilEditorImages, 11, 'edit-find-replace');
  LoadThemeIcon(ilEditorImages, 12, 'application-exit');
  LoadThemeIcon(ilEditorImages, 13, 'help-about');
  LoadThemeIcon(ilEditorImages, 14, 'edit-delete');
  LoadThemeIcon(ilEditorImages, 15, 'edit-select-all');
  LoadThemeIcon(ilEditorImages, 16, 'go-jump');
  LoadThemeIcon(ilEditorImages, 17, 'view-refresh');
end;

end.

