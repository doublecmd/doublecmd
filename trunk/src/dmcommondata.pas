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
    procedure LoadImages(Images: TImageList; ANames: TStringArray);
  public
    { public declarations }
  end; 

var
  dmComData: TdmComData;

implementation

uses
  LCLVersion, Graphics, uPixMapManager;

{$R *.lfm}

const
  ViewerNames: array[0..26] of String = (
    'view-refresh',
    'go-previous',
    'go-next',
    'edit-copy',
    'edit-cut',
    'edit-delete',
    'zoom-in',
    'zoom-out',
    'object-rotate-left',
    'object-rotate-right',
    'object-flip-horizontal',
    'media-playback-pause',
    'media-playback-start',
    'media-skip-backward',
    'media-skip-forward',
    'image-crop',
    'image-red-eye',
    'draw-freehand',
    'draw-rectangle',
    'draw-ellipse',
    'edit-undo',
    'document-edit',
    'view-fullscreen',
    'draw-path',
    'document-page-setup',
    'view-restore',
    'camera-photo'
  );

  EditorNames: array[0..44] of String = (
    'document-new',
    'document-open',
    'document-save',
    'document-save-as',
    'document-properties',
    'edit-cut',
    'edit-copy',
    'edit-paste',
    'edit-undo',
    'edit-redo',
    'edit-find',
    'edit-find-replace',
    'application-exit',
    'help-about',
    'edit-delete',
    'edit-select-all',
    'go-jump',
    'view-refresh',
    'mr-config',
    'mr-editor',
    'mr-filename',
    'mr-extension',
    'mr-counter',
    'mr-date',
    'mr-time',
    'mr-plugin',
    'view-file',
    'mr-pathtools',
    'mr-rename',
    'mr-clearfield',
    'mr-presets',
    'mr-savepreset',
    'mr-deletepreset',
    'mr-droppresets',
    'document-save-alt',
    'document-save-as-alt',
    'go-next',
    'go-bottom',
    'go-down',
    'go-up',
    'go-top',
    'process-stop',
    'copy-right-to-left',
    'copy-left-to-right',
    'choose-encoding'
  );

{ TdmComData }

procedure TdmComData.DataModuleCreate(Sender: TObject);
begin
  if Assigned(PixMapManager) then
  begin
    LoadImages(ilViewerImages, ViewerNames);
    LoadImages(ilEditorImages, EditorNames);
  end;
end;

procedure TdmComData.LoadImages(Images: TImageList; ANames: TStringArray);
var
  AName: String;
  ASize16, ASize24: Integer;
  ABitmap16, ABitmap24: TBitmap;
begin
  Images.Clear;
  ASize16:= 16; // AdjustIconSize(16, 96);
  ASize24:= 24; // AdjustIconSize(24, 96);
  Images.RegisterResolutions([ASize16, ASize24]);
  for AName in ANames do
  begin
    ABitmap16:= PixMapManager.GetThemeIcon(AName, ASize16);
    if (ABitmap16 = nil) then ABitmap16:= TBitmap.Create;
    ABitmap24:= PixMapManager.GetThemeIcon(AName, ASize24);
    if (ABitmap24 = nil) then ABitmap24:= TBitmap.Create;
    Images.AddMultipleResolutions([ABitmap16, ABitmap24]);
    ABitmap16.Free;
    ABitmap24.Free;
  end;
end;

end.

