{
   Double Commander
   -------------------------------------------------------------------------
   General icons loaded at launch based on screen resolution

   Copyright (C) 2009-2025 Alexander Koblov (alexx2000@mail.ru)

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
    procedure LoadImages(Images: TImageList; const ANames: array of String);
  public
    { public declarations }
  end; 

var
  dmComData: TdmComData;

implementation

uses
  LCLVersion, Graphics, uPixMapManager, uGlobs;

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

procedure TdmComData.LoadImages(Images: TImageList; const ANames: array of String);
var
  I: Integer;
  AName: String;
  AResolutions: array of Integer;
  ABitmaps: array of TCustomBitmap;
begin
  Images.Clear;
  SetLength(ABitmaps, 3);
  SetLength(AResolutions, 3);

  AResolutions[0]:= 16; // AdjustIconSize(16, 96);
  AResolutions[1]:= 24; // AdjustIconSize(24, 96);
  AResolutions[2]:= 32; // AdjustIconSize(32, 96);

  if gToolIconsSize >= 48 then
  begin
    SetLength(ABitmaps, 4);
    SetLength(AResolutions, 4);
    AResolutions[3]:= gToolIconsSize;
  end;

  Images.RegisterResolutions(AResolutions);

  for AName in ANames do
  begin
    for I:= 0 to High(AResolutions) do
    begin
      ABitmaps[I]:= PixMapManager.GetThemeIcon(AName, AResolutions[I]);
      if (ABitmaps[I] = nil) then ABitmaps[I]:= TBitmap.Create;
    end;

    Images.AddMultipleResolutions(ABitmaps);

    for I:= 0 to High(ABitmaps) do
    begin
      ABitmaps[I].Free;
    end;
  end;
end;

end.

