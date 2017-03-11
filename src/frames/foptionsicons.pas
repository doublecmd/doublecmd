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
    cbDiskIconsSize: TComboBox;
    cbIconsShowOverlay: TCheckBox;
    cbIconsExclude: TCheckBox;
    cbIconsInMenusSize: TComboBox;
    cbIconsInMenus: TCheckBox;
    cbIconsOnButtons: TCheckBox;
    cbIconsSize: TComboBox;
    edtIconsExcludeDirs: TEdit;
    gbIconsSize: TGroupBox;
    gbShowIconsMode: TGroupBox;
    gbDisableSpecialIcons: TGroupBox;
    gbIconsInMenus: TGroupBox;
    gbIconsOnButtons: TGroupBox;
    imgDiskIconExample: TImage;
    imgIconExample: TImage;
    lblDiskPanel: TLabel;
    lblFilePanel: TLabel;
    Panel4: TPanel;
    pnlImage: TPanel;
    pnlLabel: TPanel;
    rbIconsShowAll: TRadioButton;
    rbIconsShowAllAndExe: TRadioButton;
    rbIconsShowNone: TRadioButton;
    rbIconsShowStandard: TRadioButton;
    procedure cbDiskIconsSizeChange(Sender: TObject);
    procedure cbIconsExcludeChange(Sender: TObject);
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
  Forms, Graphics, uPixMapManager, uGlobs, uLng;

{ TfrmOptionsIcons }

procedure TfrmOptionsIcons.cbIconsSizeChange(Sender: TObject);
var
  iSize: Integer;
  bmpTemp: TBitmap;
begin
  case cbIconsSize.ItemIndex of
    0: iSize:= 16;
    1: iSize:= 22;
    2: iSize:= 32;
    3: iSize:= 48;
  end;
  bmpTemp:= PixmapManager.GetFolderIcon(iSize, pnlImage.Color);
  imgIconExample.Picture.Assign(bmpTemp);
  FreeAndNil(bmpTemp);
end;

procedure TfrmOptionsIcons.cbIconsExcludeChange(Sender: TObject);
begin
  edtIconsExcludeDirs.Enabled:= cbIconsExclude.Checked;
end;

procedure TfrmOptionsIcons.cbDiskIconsSizeChange(Sender: TObject);
var
  iSize: Integer;
  bmpTemp: TBitmap;
begin
  case cbDiskIconsSize.ItemIndex of
    0: iSize:= 16;
    1: iSize:= 22;
    2: iSize:= 32;
    3: iSize:= 48;
  end;
  bmpTemp:= PixmapManager.GetDefaultDriveIcon(iSize, pnlImage.Color);
  imgDiskIconExample.Picture.Assign(bmpTemp);
  FreeAndNil(bmpTemp);
end;

procedure TfrmOptionsIcons.rbIconsShowNoneChange(Sender: TObject);
begin
  cbIconsSize.Enabled := not rbIconsShowNone.Checked;
  cbIconsShowOverlay.Enabled := not rbIconsShowNone.Checked;
  gbDisableSpecialIcons.Enabled := not rbIconsShowNone.Checked;
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
  cbIconsExclude.Checked:= gIconsExclude;
  cbIconsInMenus.Checked := gIconsInMenus;
  edtIconsExcludeDirs.Text:= gIconsExcludeDirs;
  edtIconsExcludeDirs.Enabled:= gIconsExclude;
  cbIconsSize.Text := IntToStr(gIconsSizeNew) + 'x' + IntToStr(gIconsSizeNew);
  cbDiskIconsSize.Text := IntToStr(gDiskIconsSizeNew) + 'x' + IntToStr(gDiskIconsSizeNew);
  cbIconsInMenusSize.Text := IntToStr(gIconsInMenusSizeNew) + 'x' + IntToStr(gIconsInMenusSizeNew);
  cbIconsSizeChange(nil);
  cbDiskIconsSizeChange(nil);
  cbIconsOnButtons.Checked := Application.ShowButtonGlyphs = sbgAlways;
end;

function TfrmOptionsIcons.Save: TOptionsEditorSaveFlags;
var
  SelectedShowIcons: TShowIconsMode = sim_none;
  SelectedIconsSize: Integer;
  SelectedDiskIconsSize: Integer;
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
    3: SelectedIconsSize := 48;
  else SelectedIconsSize := gIconsSizeNew;
  end;

  case cbDiskIconsSize.ItemIndex of
    0: SelectedDiskIconsSize := 16;
    1: SelectedDiskIconsSize := 22;
    2: SelectedDiskIconsSize := 32;
    3: SelectedDiskIconsSize := 48;
  else SelectedDiskIconsSize := gDiskIconsSizeNew;
  end;

  case cbIconsInMenusSize.ItemIndex of
    0: gIconsInMenusSizeNew := 16;
    1: gIconsInMenusSizeNew := 22;
    2: gIconsInMenusSizeNew := 32;
  end;

  if (gIconsSizeNew <> SelectedIconsSize) or
     (gShowIconsNew <> SelectedShowIcons) or
     (gDiskIconsSizeNew <> SelectedDiskIconsSize) or
     (gIconsInMenusSizeNew <> gIconsInMenusSize) then
  begin
    Include(Result, oesfNeedsRestart);
  end;

  if cbIconsInMenus.Checked <> gIconsInMenus then Include(Result, oesfNeedsRestart); //Main page menu's are created only at startup so we need to restart.

  gIconsSizeNew := SelectedIconsSize;
  gShowIconsNew := SelectedShowIcons;
  gDiskIconsSizeNew := SelectedDiskIconsSize;
  gIconOverlays := cbIconsShowOverlay.Checked;
  gIconsExclude := cbIconsExclude.Checked;
  gIconsExcludeDirs := edtIconsExcludeDirs.Text;
  gIconsInMenus := cbIconsInMenus.Checked;

  if cbIconsOnButtons.Checked then
  begin
    if Application.ShowButtonGlyphs <> sbgAlways then Include(Result, oesfNeedsRestart);
    Application.ShowButtonGlyphs := sbgAlways;
  end
  else begin
    if Application.ShowButtonGlyphs <> sbgNever then Include(Result, oesfNeedsRestart);
    Application.ShowButtonGlyphs := sbgNever;
  end;
end;

end.

