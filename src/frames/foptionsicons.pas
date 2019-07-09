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
    cbIconsOnButtons: TCheckBox;
    cbIconsShowOverlay: TCheckBox;
    cbIconsExclude: TCheckBox;
    cbIconsInMenusSize: TComboBox;
    cbIconsInMenus: TCheckBox;
    cbIconsSize: TComboBox;
    chkShowHiddenDimmed: TCheckBox;
    cmbIconTheme: TComboBox;
    edtIconsExcludeDirs: TEdit;
    gbIconsSize: TGroupBox;
    gbShowIconsMode: TGroupBox;
    gbDisableSpecialIcons: TGroupBox;
    gbShowIcons: TGroupBox;
    gbIconTheme: TGroupBox;
    imgDiskIconExample: TImage;
    imgIconExample: TImage;
    lblDiskPanel: TLabel;
    lblFilePanel: TLabel;
    pnlComboBox: TPanel;
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
  private
    procedure FillIconThemes(const Path: String);
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end; 

implementation

{$R *.lfm}

uses
  Forms, Graphics, FileUtil, DCOSUtils, uPixMapManager, uGlobs, uLng, uOSUtils,
  uGlobsPaths;

{ TfrmOptionsIcons }

procedure TfrmOptionsIcons.cbIconsSizeChange(Sender: TObject);
var
  iSize: Integer;
  bmpTemp: TBitmap;
begin
  if cbIconsSize.ItemIndex < 0 then Exit;
  iSize:= PtrInt(cbIconsSize.Items.Objects[cbIconsSize.ItemIndex]);
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
  if cbDiskIconsSize.ItemIndex < 0 then Exit;
  iSize:= PtrInt(cbDiskIconsSize.Items.Objects[cbDiskIconsSize.ItemIndex]);
  bmpTemp:= PixmapManager.GetDefaultDriveIcon(iSize, pnlImage.Color);
  imgDiskIconExample.Picture.Assign(bmpTemp);
  FreeAndNil(bmpTemp);
end;

procedure TfrmOptionsIcons.rbIconsShowNoneChange(Sender: TObject);
begin
  cbIconsSize.Enabled := not rbIconsShowNone.Checked;
  cbIconsShowOverlay.Enabled := not rbIconsShowNone.Checked;
  chkShowHiddenDimmed.Enabled := not rbIconsShowNone.Checked;
  gbDisableSpecialIcons.Enabled := not rbIconsShowNone.Checked;
end;

procedure TfrmOptionsIcons.FillIconThemes(const Path: String);
var
  I: Integer;
  ADirectories: TStringList;
begin
  ADirectories:= FindAllDirectories(Path, False);
  for I:= 0 to ADirectories.Count - 1 do
  begin
    if mbFileExists(ADirectories[I] + PathDelim + 'index.theme') then
      cmbIconTheme.Items.Add(ExtractFileName(ADirectories[I]));
  end;
  ADirectories.Free;
end;

class function TfrmOptionsIcons.GetIconIndex: Integer;
begin
  Result := 16;
end;

class function TfrmOptionsIcons.GetTitle: String;
begin
  Result := rsOptionsEditorIcons;
end;

procedure TfrmOptionsIcons.Init;
var
  I: Integer;
  AIconSize: String;
begin
  inherited Init;
  for I:= Low(ICON_SIZES) to High(ICON_SIZES) do
  begin
    AIconSize:= IntToStr(ICON_SIZES[I]) + 'x' + IntToStr(ICON_SIZES[I]);
    cbIconsSize.Items.AddObject(AIconSize, TObject(PtrInt(ICON_SIZES[I])));
  end;
  for I:= Low(ICON_SIZES) to High(ICON_SIZES) - 1 do
  begin
    AIconSize:= IntToStr(ICON_SIZES[I]) + 'x' + IntToStr(ICON_SIZES[I]);
    cbDiskIconsSize.Items.AddObject(AIconSize, TObject(PtrInt(ICON_SIZES[I])));
  end;
  TStringList(cmbIconTheme.Items).Duplicates:= dupIgnore;
  if not gUseConfigInProgramDir then begin
    FillIconThemes(IncludeTrailingBackslash(GetAppDataDir) + 'pixmaps');
  end;
  FillIconThemes(gpPixmapPath);
end;

procedure TfrmOptionsIcons.Load;
begin
  case gShowIconsNew of
    sim_none: rbIconsShowNone.Checked:= True;
    sim_standart: rbIconsShowStandard.Checked:= True;
    sim_all: rbIconsShowAll.Checked:= True;
    sim_all_and_exe: rbIconsShowAllAndExe.Checked := True;
  end;
  cmbIconTheme.Text:= gIconTheme;
  chkShowHiddenDimmed.Checked:= gShowHiddenDimmed;
  cbIconsShowOverlay.Checked:= gIconOverlays;
  cbIconsExclude.Checked:= gIconsExclude;
  cbIconsInMenus.Checked := gIconsInMenus;
  edtIconsExcludeDirs.Text:= gIconsExcludeDirs;
  edtIconsExcludeDirs.Enabled:= gIconsExclude;
  cbIconsSize.Text := IntToStr(gIconsSizeNew) + 'x' + IntToStr(gIconsSizeNew);
  cbDiskIconsSize.Text := IntToStr(gDiskIconsSize) + 'x' + IntToStr(gDiskIconsSize);
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

  if cbIconsSize.ItemIndex < 0 then
    SelectedIconsSize := gIconsSizeNew
  else begin
    SelectedIconsSize := PtrInt(cbIconsSize.Items.Objects[cbIconsSize.ItemIndex])
  end;

  if cbDiskIconsSize.ItemIndex < 0 then
    SelectedDiskIconsSize := gDiskIconsSize
  else begin
    SelectedDiskIconsSize := PtrInt(cbDiskIconsSize.Items.Objects[cbDiskIconsSize.ItemIndex])
  end;

  case cbIconsInMenusSize.ItemIndex of
    0: gIconsInMenusSizeNew := 16;
    1: gIconsInMenusSizeNew := 24;
    2: gIconsInMenusSizeNew := 32;
  end;

  if (gIconsSizeNew <> SelectedIconsSize) or
     (gShowIconsNew <> SelectedShowIcons) or
     (gIconsInMenusSizeNew <> gIconsInMenusSize) then
  begin
    Include(Result, oesfNeedsRestart);
  end;

  if cbIconsInMenus.Checked <> gIconsInMenus then Include(Result, oesfNeedsRestart); //Main page menu's are created only at startup so we need to restart.

  gIconsSizeNew := SelectedIconsSize;
  gShowIconsNew := SelectedShowIcons;
  gDiskIconsSize := SelectedDiskIconsSize;
  gIconOverlays := cbIconsShowOverlay.Checked;
  gIconsExclude := cbIconsExclude.Checked;
  gIconsExcludeDirs := edtIconsExcludeDirs.Text;
  gIconsInMenus := cbIconsInMenus.Checked;
  gShowHiddenDimmed := chkShowHiddenDimmed.Checked;

  if cbIconsOnButtons.Checked then
  begin
    if Application.ShowButtonGlyphs <> sbgAlways then Include(Result, oesfNeedsRestart);
    Application.ShowButtonGlyphs := sbgAlways;
  end
  else begin
    if Application.ShowButtonGlyphs <> sbgNever then Include(Result, oesfNeedsRestart);
    Application.ShowButtonGlyphs := sbgNever;
  end;

  if cmbIconTheme.Text <> gIconTheme then
  begin
    gIconTheme:= cmbIconTheme.Text;
    Include(Result, oesfNeedsRestart);
  end;
end;

end.

