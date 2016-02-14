{
   Double Commander
   -------------------------------------------------------------------------
   Tabs options page

   Copyright (C) 2006-2016  Koblov Alexander (Alexx2000@mail.ru)

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
  Classes, SysUtils, StdCtrls, ComCtrls, ExtCtrls, fOptionsFrame;

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
    cbTabsActionOnDoubleClick: TComboBox;
    edtTabsLimitLength: TEdit;
    gbTabs: TGroupBox;
    lblTabsActionOnDoubleClick: TLabel;
    lblChar: TLabel;
    lblTabsPosition: TLabel;
    cbKeepRenamedNameBackToNormal: TCheckBox;
    cbTabsConfirmCloseLocked: TCheckBox;
    cbTabsReuseTabWhenPossible: TCheckBox;
    cbTabsShowDriveLetter: TCheckBox;
    cbTabsCloseDuplicateWhenClosing: TCheckBox;
  private
    FPageControl: TPageControl; // For checking Tabs capabilities
    FLastLoadedOptionSignature: dword;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    function CanWeClose(var WillNeedUpdateWindowView: boolean): boolean; override;
  end;

implementation

{$R *.lfm}

uses
  Forms, fOptions, uShowMsg, uComponentsSignature, DCStrUtils, uLng, uGlobs;

{ TfrmOptionsTabs }

procedure TfrmOptionsTabs.Init;
begin
  ParseLineToList(rsOptTabsPosition, cmbTabsPosition.Items);
  ParseLineToList(rsTabsActionOnDoubleClickChoices, cbTabsActionOnDoubleClick.Items);
  FPageControl := TPageControl.Create(Self);
end;

class function TfrmOptionsTabs.GetIconIndex: Integer;
begin
  Result := 9;
end;

class function TfrmOptionsTabs.GetTitle: String;
begin
  Result := rsOptionsEditorFolderTabs;
end;

procedure TfrmOptionsTabs.Load;
begin
  {$IFDEF MSWINDOWS}
  cbTabsShowDriveLetter.Visible:=True;
  {$ENDIF}
  cbTabsAlwaysVisible.Checked := (tb_always_visible in gDirTabOptions) and gDirectoryTabs;
  cbTabsLimitOption.Checked := tb_text_length_limit in gDirTabOptions;
  cbTabsConfirmCloseAll.Checked := tb_confirm_close_all in gDirTabOptions;
  cbTabsConfirmCloseLocked.Checked := tb_confirm_close_locked_tab in gDirTabOptions;
  cbTabsCloseDuplicateWhenClosing.Checked := tb_close_duplicate_when_closing in gDirTabOptions;
  cbTabsOpenForeground.Checked := tb_open_new_in_foreground in gDirTabOptions;
  cbTabsOpenNearCurrent.Checked := tb_open_new_near_current in gDirTabOptions;
  cbTabsReuseTabWhenPossible.Checked := tb_reusing_tab_when_possible in gDirTabOptions;
  cbTabsLockedAsterisk.Checked := tb_show_asterisk_for_locked in gDirTabOptions;
  cbKeepRenamedNameBackToNormal.Checked := tb_keep_renamed_when_back_normal in gDirTabOptions;
  cbTabsActivateOnClick.Checked := tb_activate_panel_on_click in gDirTabOptions;
  cbTabsShowDriveLetter.Checked := tb_show_drive_letter in gDirTabOptions;
  cbTabsActionOnDoubleClick.ItemIndex := Integer(gDirTabActionOnDoubleClick);
  cbTabsActionOnDoubleClick.Refresh;

  cbTabsMultiLines.Visible := (nbcMultiline in FPageControl.GetCapabilities);
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

  Application.ProcessMessages;
  FLastLoadedOptionSignature := ComputeSignatureBasedOnComponent(Self, $00000000);
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
  if cbTabsConfirmCloseLocked.Checked then
    gDirTabOptions := gDirTabOptions + [tb_confirm_close_locked_tab];
  if cbTabsCloseDuplicateWhenClosing.Checked then
    gDirTabOptions := gDirTabOptions + [tb_close_duplicate_when_closing];
  if cbTabsOpenForeground.Checked then
    gDirTabOptions := gDirTabOptions + [tb_open_new_in_foreground];
  if cbTabsOpenNearCurrent.Checked then
    gDirTabOptions := gDirTabOptions + [tb_open_new_near_current];
  if cbTabsReuseTabWhenPossible.Checked then
    gDirTabOptions := gDirTabOptions + [tb_reusing_tab_when_possible];
  if cbTabsLockedAsterisk.Checked then
    gDirTabOptions := gDirTabOptions + [tb_show_asterisk_for_locked];
  if cbKeepRenamedNameBackToNormal.Checked then
    gDirTabOptions := gDirTabOptions + [tb_keep_renamed_when_back_normal];
  if cbTabsActivateOnClick.Checked then
    gDirTabOptions := gDirTabOptions + [tb_activate_panel_on_click];
  if cbTabsShowDriveLetter.Checked then
    gDirTabOptions := gDirTabOptions + [tb_show_drive_letter];
  if cbTabsShowCloseButton.Checked then
    gDirTabOptions := gDirTabOptions + [tb_show_close_button];

  gDirTabActionOnDoubleClick := TTabsOptionsDoubleClick(cbTabsActionOnDoubleClick.ItemIndex);

  gDirTabLimit := StrToIntDef(edtTabsLimitLength.Text, 32);

  case cmbTabsPosition.ItemIndex of
    0: gDirTabPosition := tbpos_top;
    1: gDirTabPosition := tbpos_bottom;
  end;

  FLastLoadedOptionSignature := ComputeSignatureBasedOnComponent(Self, $00000000);
end;

{ TfrmOptionsTabs.CanWeClose }
function TfrmOptionsTabs.CanWeClose(var WillNeedUpdateWindowView: boolean): boolean;
var
  Answer: TMyMsgResult;
begin
  Result := (FLastLoadedOptionSignature = ComputeSignatureBasedOnComponent(Self, $00000000));

  if not Result then
  begin
    ShowOptions(TfrmOptionsTabs);
    Answer := MsgBox(rsMsgTabsOptionsModifiedWantToSave, [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
    case Answer of
      mmrYes:
      begin
        Save;
        WillNeedUpdateWindowView := True;
        Result := True;
      end;

      mmrNo: Result := True;
      else
        Result := False;
    end;
  end;
end;

end.

