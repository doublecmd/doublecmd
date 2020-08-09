{
   Double Commander
   -------------------------------------------------------------------------
   General icons loaded at launch based on screen resolution

   Copyright (C) 2009-2020 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit dmCommonData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs; 

type

  { TdmComData }

  TdmComData = class(TDataModule)
    ilEditorImages: TImageList;
    ilViewerImages: TImageList;
    ImageList: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure LoadViewerImageList;
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
    LoadViewerImageList;
    LoadEditorImageList;
  end;
end;

procedure TdmComData.LoadViewerImageList;
begin
  LoadThemeIcon(ilViewerImages, 0, 'view-refresh');
  LoadThemeIcon(ilViewerImages, 1, 'go-previous');
  LoadThemeIcon(ilViewerImages, 2, 'go-next');
  LoadThemeIcon(ilViewerImages, 3, 'edit-copy');
  LoadThemeIcon(ilViewerImages, 4, 'edit-cut');
  LoadThemeIcon(ilViewerImages, 5, 'edit-delete');
  LoadThemeIcon(ilViewerImages, 6, 'zoom-in');
  LoadThemeIcon(ilViewerImages, 7, 'zoom-out');

  LoadThemeIcon(ilViewerImages, 8, 'object-rotate-left');
  LoadThemeIcon(ilViewerImages, 9, 'object-rotate-right');
  LoadThemeIcon(ilViewerImages, 10, 'object-flip-horizontal');

  LoadThemeIcon(ilViewerImages, 11, 'media-playback-pause');
  LoadThemeIcon(ilViewerImages, 12, 'media-playback-start');
  LoadThemeIcon(ilViewerImages, 13, 'media-skip-backward');
  LoadThemeIcon(ilViewerImages, 14, 'media-skip-forward');

  LoadThemeIcon(ilViewerImages, 15, 'image-crop');

  LoadThemeIcon(ilViewerImages, 16, 'image-red-eye');

  LoadThemeIcon(ilViewerImages, 17, 'draw-freehand');
  LoadThemeIcon(ilViewerImages, 18, 'draw-rectangle');
  LoadThemeIcon(ilViewerImages, 19, 'draw-ellipse');

  LoadThemeIcon(ilViewerImages, 20, 'edit-undo');
  LoadThemeIcon(ilViewerImages, 21, 'document-edit');

  LoadThemeIcon(ilViewerImages, 22, 'view-fullscreen');

  LoadThemeIcon(ilViewerImages, 23, 'draw-path');

  LoadThemeIcon(ilViewerImages, 24, 'document-page-setup');

  LoadThemeIcon(ilViewerImages, 25, 'view-restore');

  LoadThemeIcon(ilViewerImages, 26, 'camera-photo');
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
  LoadThemeIcon(ilEditorImages, 18, 'mr-config');
  LoadThemeIcon(ilEditorImages, 19, 'mr-editor');
  LoadThemeIcon(ilEditorImages, 20, 'mr-filename');
  LoadThemeIcon(ilEditorImages, 21, 'mr-extension');
  LoadThemeIcon(ilEditorImages, 22, 'mr-counter');
  LoadThemeIcon(ilEditorImages, 23, 'mr-date');
  LoadThemeIcon(ilEditorImages, 24, 'mr-time');
  LoadThemeIcon(ilEditorImages, 25, 'mr-plugin');
  LoadThemeIcon(ilEditorImages, 26, 'view-file');
  LoadThemeIcon(ilEditorImages, 27, 'mr-pathtools');
  LoadThemeIcon(ilEditorImages, 28, 'mr-rename');
  LoadThemeIcon(ilEditorImages, 29, 'mr-clearfield');
  LoadThemeIcon(ilEditorImages, 30, 'mr-presets');
  LoadThemeIcon(ilEditorImages, 31, 'mr-savepreset');
  LoadThemeIcon(ilEditorImages, 32, 'mr-deletepreset');
  LoadThemeIcon(ilEditorImages, 33, 'mr-droppresets');
end;

end.

