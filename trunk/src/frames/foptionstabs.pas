{
   Double Commander
   -------------------------------------------------------------------------
   Tabs options page

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

unit fOptionsTabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ComCtrls, fOptionsFrame;

type

  { TfrmOptionsTabs }

  TfrmOptionsTabs = class(TOptionsEditor)
    cbTabsActivateOnClick: TCheckBox;
    cbTabsAlwaysVisible: TCheckBox;
    cbTabsConfirmCloseAll: TCheckBox;
    cbTabsLimitOption: TCheckBox;
    cbTabsLockedAsterisk: TCheckBox;
    cbTabsMultiLines: TCheckBox;
    cbTabsOpenForeground: TCheckBox;
    cbTabsOpenNearCurrent: TCheckBox;
    cbTabsShowCloseButton: TCheckBox;
    cmbTabsPosition: TComboBox;
    edtTabsLimitLength: TEdit;
    gbTabs: TGroupBox;
    lblChar: TLabel;
    lblTabsPosition: TLabel;
  private
    FPageControl: TPageControl; // For checking Tabs capabilities
  protected
    procedure Init; override;
  public
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end; 

implementation

{$R *.lfm}

uses
  uDCUtils, uLng, uGlobs;

{ TfrmOptionsTabs }

procedure TfrmOptionsTabs.Init;
begin
  ParseLineToList(rsOptTabsPosition, cmbTabsPosition.Items);
  FPageControl := TPageControl.Create(Self);
end;

procedure TfrmOptionsTabs.Load;
begin
  cbTabsAlwaysVisible.Checked := (tb_always_visible in gDirTabOptions) and gDirectoryTabs;
  cbTabsLimitOption.Checked := tb_text_length_limit in gDirTabOptions;
  cbTabsConfirmCloseAll.Checked:= tb_confirm_close_all in gDirTabOptions;
  cbTabsOpenForeground.Checked:= tb_open_new_in_foreground in gDirTabOptions;
  cbTabsOpenNearCurrent.Checked:= tb_open_new_near_current in gDirTabOptions;
  cbTabsLockedAsterisk.Checked:= tb_show_asterisk_for_locked in gDirTabOptions;
  cbTabsActivateOnClick.Checked:= tb_activate_panel_on_click in gDirTabOptions;
  cbTabsMultiLines.Visible:= (nbcMultiline in FPageControl.GetCapabilities);
  if cbTabsMultiLines.Visible then
     cbTabsMultiLines.Checked:= tb_multiple_lines in gDirTabOptions;
  cbTabsShowCloseButton.Visible:= (nbcShowCloseButtons in FPageControl.GetCapabilities);
  if cbTabsShowCloseButton.Visible then
    cbTabsShowCloseButton.Checked:= tb_show_close_button in gDirTabOptions;
  edtTabsLimitLength.Text:= IntToStr(gDirTabLimit);
  case gDirTabPosition of
    tbpos_top:    cmbTabsPosition.ItemIndex := 0;
    tbpos_bottom: cmbTabsPosition.ItemIndex := 1;
    else          cmbTabsPosition.ItemIndex := 0;
  end;
end;

function TfrmOptionsTabs.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gDirTabOptions := [];  // Reset tab options
  if cbTabsAlwaysVisible.Checked then
    gDirTabOptions := gDirTabOptions + [tb_always_visible];
  if cbTabsMultiLines.Checked then
    gDirTabOptions := gDirTabOptions + [tb_multiple_lines];
  if cbTabsLimitOption.Checked then
    gDirTabOptions := gDirTabOptions + [tb_text_length_limit];
  if cbTabsConfirmCloseAll.Checked then
    gDirTabOptions := gDirTabOptions + [tb_confirm_close_all];
  if cbTabsOpenForeground.Checked then
    gDirTabOptions := gDirTabOptions + [tb_open_new_in_foreground];
  if cbTabsOpenNearCurrent.Checked then
    gDirTabOptions := gDirTabOptions + [tb_open_new_near_current];
  if cbTabsLockedAsterisk.Checked then
    gDirTabOptions := gDirTabOptions + [tb_show_asterisk_for_locked];
  if cbTabsActivateOnClick.Checked then
    gDirTabOptions := gDirTabOptions + [tb_activate_panel_on_click];
  if cbTabsShowCloseButton.Checked then
    gDirTabOptions := gDirTabOptions + [tb_show_close_button];

  gDirTabLimit := StrToIntDef(edtTabsLimitLength.Text, 32);

  case cmbTabsPosition.ItemIndex of
    0: gDirTabPosition := tbpos_top;
    1: gDirTabPosition := tbpos_bottom;
  end;
end;

initialization
  RegisterOptionsEditor(optedFolderTabs, TfrmOptionsTabs);

end.

