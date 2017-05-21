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
  uPixMapManager;

{$R *.lfm}

{ TdmComData }

procedure TdmComData.DataModuleCreate(Sender: TObject);
begin
  LoadEditorImageList;
end;

procedure TdmComData.LoadEditorImageList;
begin
  ilEditorImages.Width:= AdjustIconSize(ilEditorImages.Width, 96);
  ilEditorImages.Height:= AdjustIconSize(ilEditorImages.Height, 96);
  with PixMapManager do
  begin
    ilEditorImages.Replace(0, GetThemeIcon('document-new', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(1, GetThemeIcon('document-open', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(2, GetThemeIcon('document-save', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(3, GetThemeIcon('document-save-as', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(4, GetThemeIcon('document-properties', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(5, GetThemeIcon('edit-cut', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(6, GetThemeIcon('edit-copy', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(7, GetThemeIcon('edit-paste', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(8, GetThemeIcon('edit-undo', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(9, GetThemeIcon('edit-redo', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(10, GetThemeIcon('edit-find', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(11, GetThemeIcon('edit-find-replace', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(12, GetThemeIcon('system-log-out', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(13, GetThemeIcon('help-browser', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(14, GetThemeIcon('edit-delete', ilEditorImages.Width) , nil);
    ilEditorImages.Replace(15, GetThemeIcon('edit-select-all', ilEditorImages.Width) , nil);
    // ilEditorImages.Replace(16, GetThemeIcon('edit-goto', ilEditorImages.Width) , nil);
  end;
end;

end.

