{
   Double Commander
   -------------------------------------------------------------------------
   Icons options page

   Copyright (C) 2006-2011  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit fOptionsIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, fOptionsFrame;

type

  { TfrmOptionsIcons }

  TfrmOptionsIcons = class(TOptionsEditor)
    cbIconsShowOverlay: TCheckBox;
    cbIconsSize: TComboBox;
    gbIconsSize: TGroupBox;
    gbShowIconsMode: TGroupBox;
    imgIconExample: TImage;
    pnlIconExample: TPanel;
    rbIconsShowAll: TRadioButton;
    rbIconsShowAllAndExe: TRadioButton;
    rbIconsShowNone: TRadioButton;
    rbIconsShowStandard: TRadioButton;
    procedure cbIconsSizeChange(Sender: TObject);
    procedure rbIconsShowNoneChange(Sender: TObject);
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end; 

implementation

{$R *.lfm}

uses
  Graphics, uPixMapManager, uGlobs, uLng;

{ TfrmOptionsIcons }

procedure TfrmOptionsIcons.cbIconsSizeChange(Sender: TObject);
var
  bmpTemp: TBitmap;
  iSize: Integer;
begin
  case cbIconsSize.ItemIndex of
    0: iSize:= 16;
    1: iSize:= 22;
    2: iSize:= 32;
  end;
  bmpTemp:= PixmapManager.GetDefaultDriveIcon(iSize, pnlIconExample.Color);
  imgIconExample.Picture.Bitmap.Assign(bmpTemp);
  FreeAndNil(bmpTemp);
end;

procedure TfrmOptionsIcons.rbIconsShowNoneChange(Sender: TObject);
begin
  cbIconsSize.Enabled := not rbIconsShowNone.Checked;
end;

class function TfrmOptionsIcons.GetIconIndex: Integer;
begin
  Result := 16;
end;

class function TfrmOptionsIcons.GetTitle: String;
begin
  Result := rsOptionsEditorIcons;
end;

procedure TfrmOptionsIcons.Load;
begin
  case gShowIconsNew of
    sim_none: rbIconsShowNone.Checked:= True;
    sim_standart: rbIconsShowStandard.Checked:= True;
    sim_all: rbIconsShowAll.Checked:= True;
    sim_all_and_exe: rbIconsShowAllAndExe.Checked := True;
  end;
  cbIconsShowOverlay.Checked:= gIconOverlays;
  cbIconsSize.Text := IntToStr(gIconsSizeNew) + 'x' + IntToStr(gIconsSizeNew);
  cbIconsSizeChange(nil);
end;

function TfrmOptionsIcons.Save: TOptionsEditorSaveFlags;
var
  SelectedShowIcons: TShowIconsMode = sim_none;
  SelectedIconsSize: Integer;
  iSize: Integer;
begin
  Result := [];

  if rbIconsShowNone.Checked then
    SelectedShowIcons := sim_none
  else if rbIconsShowStandard.Checked then
    SelectedShowIcons := sim_standart
  else if rbIconsShowAll.Checked then
    SelectedShowIcons := sim_all
  else if rbIconsShowAllAndExe.Checked then
    SelectedShowIcons := sim_all_and_exe;

  case cbIconsSize.ItemIndex of
    0: SelectedIconsSize := 16;
    1: SelectedIconsSize := 22;
    2: SelectedIconsSize := 32;
  else SelectedIconsSize := gIconsSizeNew;
  end;

  if (gIconsSizeNew <> SelectedIconsSize) or
     (gShowIconsNew <> SelectedShowIcons) then
  begin
    Include(Result, oesfNeedsRestart);
  end;

  gIconsSizeNew := SelectedIconsSize;
  gShowIconsNew := SelectedShowIcons;
  gIconOverlays := cbIconsShowOverlay.Checked;
end;

end.

