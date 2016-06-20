{
   Double Commander
   -------------------------------------------------------------------------
   Tabs "Extra" options page

   Copyright (C) 2016 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit fOptionsTabsExtra;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ComCtrls, ExtCtrls, fOptionsFrame;
type

  { TfrmOptionsTabsExtra }

  TfrmOptionsTabsExtra = class(TOptionsEditor)
    cbDefaultExistingTabsToKeep: TComboBox;
    cbDefaultSaveDirHistory: TComboBox;
    cbDefaultTargetPanelLeftSaved: TComboBox;
    cbDefaultTargetPanelRightSaved: TComboBox;
    cbGoToConfigAfterReSave: TCheckBox;
    cbGoToConfigAfterSave: TCheckBox;
    cbUseFavoriteTabsExtraOptions: TCheckBox;
    gbTabs: TGroupBox;
    gbDefaultTabSavedRestoration: TGroupBox;
    lblDefaultExistingTabsToKeep: TLabel;
    lblFavoriteTabsSaveDirHistory: TLabel;
    lblDefaultTargetPanelLeftSaved: TLabel;
    lblDefaultTargetPanelRightSaved: TLabel;
    rgWhereToAdd: TRadioGroup;
    procedure cbUseFavoriteTabsExtraOptionsChange(Sender: TObject);
  private
    FPageControl: TPageControl; // For checking Tabs capabilities
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Forms,

  //DC
  fOptions, DCStrUtils, uLng, uGlobs, ufavoritetabs, fOptionsFavoriteTabs;

{ TfrmOptionsTabsExtra }

procedure TfrmOptionsTabsExtra.cbUseFavoriteTabsExtraOptionsChange(Sender: TObject);
var
  Options: IOptionsDialog = nil;
  Editor: TOptionsEditor = nil;
begin
  gFavoriteTabsUseRestoreExtraOptions := TCheckBox(Sender).Checked;
  gbDefaultTabSavedRestoration.Enabled := TCheckBox(Sender).Checked;
  if not TCheckBox(Sender).Checked then
    lblFavoriteTabsSaveDirHistory.Caption := rsMsgFavoriteTabsSimpleMode
  else
    lblFavoriteTabsSaveDirHistory.Caption := rsMsgFavoriteTabsExtraMode;

  // Le't be dynamic and update possible already displayed Favorite Tabs Configuration frame.
  Options := GetOptionsForm;
  if Options <> nil then // and it will be since we're here! :-)
    Editor := Options.GetEditor(TfrmOptionsFavoriteTabs);
  if Editor <> nil then
    TfrmOptionsFavoriteTabs(Editor).gpSavedTabsRestorationAction.Visible := gFavoriteTabsUseRestoreExtraOptions;
end;

{ TfrmOptionsTabsExtra.Init }
procedure TfrmOptionsTabsExtra.Init;
begin
  FPageControl := TPageControl.Create(Self);
  ParseLineToList(rsOptFavoriteTabsWhereToAddInList, rgWhereToAdd.Items);
  ParseLineToList(rsFavTabsPanelSideSelection,cbDefaultTargetPanelLeftSaved.Items);
  ParseLineToList(rsFavTabsPanelSideSelection,cbDefaultTargetPanelRightSaved.Items);
  ParseLineToList(rsFavTabsPanelSideSelection,cbDefaultExistingTabsToKeep.Items);
  ParseLineToList(rsFavTabsSaveDirHistory,cbDefaultSaveDirHistory.Items);
end;

class function TfrmOptionsTabsExtra.GetIconIndex: integer;
begin
  Result := 38;
end;

class function TfrmOptionsTabsExtra.GetTitle: string;
begin
  Result := rsOptionsEditorFolderTabsExtra;
end;

procedure TfrmOptionsTabsExtra.Load;
begin
  cbUseFavoriteTabsExtraOptions.Checked := gFavoriteTabsUseRestoreExtraOptions;
  cbUseFavoriteTabsExtraOptionsChange(cbUseFavoriteTabsExtraOptions);
  cbDefaultTargetPanelLeftSaved.ItemIndex := integer(gDefaultTargetPanelLeftSaved);
  cbDefaultTargetPanelRightSaved.ItemIndex := integer(gDefaultTargetPanelRightSaved);
  cbDefaultExistingTabsToKeep.ItemIndex := integer(gDefaultExistingTabsToKeep);
  if gFavoriteTabsSaveDirHistory then cbDefaultSaveDirHistory.ItemIndex := 1 else cbDefaultSaveDirHistory.ItemIndex := 0;
  rgWhereToAdd.ItemIndex := integer(gWhereToAddNewFavoriteTabs);
  cbGoToConfigAfterSave.Checked := gFavoriteTabsGoToConfigAfterSave;
  cbGoToConfigAfterReSave.Checked := gFavoriteTabsGoToConfigAfterReSave;

  Application.ProcessMessages;
end;

function TfrmOptionsTabsExtra.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gFavoriteTabsUseRestoreExtraOptions := cbUseFavoriteTabsExtraOptions.Checked;
  gDefaultTargetPanelLeftSaved := TTabsConfigLocation(cbDefaultTargetPanelLeftSaved.ItemIndex);
  gDefaultTargetPanelRightSaved := TTabsConfigLocation(cbDefaultTargetPanelRightSaved.ItemIndex);
  gDefaultExistingTabsToKeep := TTabsConfigLocation(cbDefaultExistingTabsToKeep.ItemIndex);
  gFavoriteTabsSaveDirHistory := (cbDefaultSaveDirHistory.ItemIndex = 1);
  gWhereToAddNewFavoriteTabs := TPositionWhereToAddFavoriteTabs(rgWhereToAdd.ItemIndex);
  gFavoriteTabsGoToConfigAfterSave := cbGoToConfigAfterSave.Checked;
  gFavoriteTabsGoToConfigAfterReSave := cbGoToConfigAfterReSave.Checked;
end;

end.



