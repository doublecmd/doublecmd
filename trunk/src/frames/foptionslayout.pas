{
   Double Commander
   -------------------------------------------------------------------------
   Layout options page

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

unit fOptionsLayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls,
  fOptionsFrame;

type

  { TfrmOptionsLayout }

  TfrmOptionsLayout = class(TOptionsEditor)
    cbFlatDiskPanel: TCheckBox;
    cbFlatInterface: TCheckBox;
    cbFlatToolBar: TCheckBox;
    cbFreespaceInd: TCheckBox;
    cbLogWindow: TCheckBox;
    cbPanelOfOperations: TCheckBox;
    cbProgInMenuBar: TCheckBox;
    cbShowCmdLine: TCheckBox;
    cbShowCurDir: TCheckBox;
    cbShowDiskPanel: TCheckBox;
    cbShowDriveFreeSpace: TCheckBox;
    cbShowDriveMenuButton: TCheckBox;
    cbShowKeysPanel: TCheckBox;
    cbShowMainMenu: TCheckBox;
    cbShowMainToolBar: TCheckBox;
    cbShowStatusBar: TCheckBox;
    cbShowTabHeader: TCheckBox;
    cbShowTabs: TCheckBox;
    cbTermWindow: TCheckBox;
    cbTwoDiskPanels: TCheckBox;
    gbScreenLayout: TGroupBox;
    procedure cbShowDiskPanelChange(Sender: TObject);
    procedure cbTermWindowChange(Sender: TObject);
  public
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end;

implementation

{$R *.lfm}

uses
  uGlobs;

{ TfrmOptionsLayout }

procedure TfrmOptionsLayout.cbShowDiskPanelChange(Sender: TObject);
begin
  cbTwoDiskPanels.Enabled := cbShowDiskPanel.Checked;
  cbFlatDiskPanel.Enabled := cbShowDiskPanel.Checked;
end;

procedure TfrmOptionsLayout.cbTermWindowChange(Sender: TObject);
begin
  if cbTermWindow.Checked then
    begin
      cbShowCmdLine.Tag:= Integer(cbShowCmdLine.Checked);
      cbShowCmdLine.Checked:= True;
      cbShowCmdLine.Enabled:= False;
    end
  else
    begin
      cbShowCmdLine.Checked:= Boolean(cbShowCmdLine.Tag);
      cbShowCmdLine.Enabled:= True;
    end;
end;

procedure TfrmOptionsLayout.Load;
begin
  cbShowMainMenu.Checked := gMainMenu;
  cbShowMainToolBar.Checked := gButtonBar;
  cbFlatToolBar.Checked := gToolBarFlat;
  cbShowDiskPanel.Checked := gDriveBar1;
  cbTwoDiskPanels.Checked := gDriveBar2;
  cbFlatDiskPanel.Checked := gDriveBarFlat;
  cbShowDriveMenuButton.Checked := gDriveMenuButton;
  cbShowTabs.Checked := gDirectoryTabs;
  cbShowCurDir.Checked := gCurDir;
  cbShowTabHeader.Checked := gTabHeader;
  cbShowStatusBar.Checked := gStatusBar;
  cbShowCmdLine.Checked := gCmdLine or gTermWindow;
  cbShowCmdLine.Enabled:= not gTermWindow;
  cbShowKeysPanel.Checked := gKeyButtons;
  cbFlatInterface.Checked := gInterfaceFlat;
  cbLogWindow.Checked := gLogWindow;
  cbTermWindow.Checked := gTermWindow;
  cbShowDriveFreeSpace.Checked := gDriveFreeSpace;
  cbFreespaceInd.Checked := gDriveInd;
  cbProgInMenuBar.Checked := gProgInMenuBar;
  cbPanelOfOperations.Checked := gPanelOfOp;
end;

function TfrmOptionsLayout.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gMainMenu := cbShowMainMenu.Checked;
  gButtonBar := cbShowMainToolBar.Checked;
  gToolBarFlat := cbFlatToolBar.Checked;
  gDriveBar1 := cbShowDiskPanel.Checked;
  gDriveBar2 := cbTwoDiskPanels.Checked;
  gDriveBarFlat := cbFlatDiskPanel.Checked;
  gDriveMenuButton := cbShowDriveMenuButton.Checked;
  gDirectoryTabs := cbShowTabs.Checked;
  gCurDir := cbShowCurDir.Checked;
  gTabHeader := cbShowTabHeader.Checked;
  gStatusBar := cbShowStatusBar.Checked;
  gCmdLine := cbShowCmdLine.Checked;
  gKeyButtons := cbShowKeysPanel.Checked;
  gInterfaceFlat := cbFlatInterface.Checked;
  gLogWindow := cbLogWindow.Checked;
  gTermWindow := cbTermWindow.Checked;
  gDriveFreeSpace := cbShowDriveFreeSpace.Checked;
  gDriveInd := cbFreespaceInd.Checked;
  gProgInMenuBar := cbProgInMenuBar.Checked;
  gPanelOfOp := cbPanelOfOperations.Checked;
end;

initialization
  RegisterOptionsEditor(optedLayout, TfrmOptionsLayout);

end.

