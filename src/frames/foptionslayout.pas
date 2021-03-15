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
    cbFreespaceInd: TCheckBox;
    cbLogWindow: TCheckBox;
    cbPanelOfOperations: TCheckBox;
    cbProgInMenuBar: TCheckBox;
    cbShowCmdLine: TCheckBox;
    cbShowCurDir: TCheckBox;
    cbShowDiskPanel: TCheckBox;
    cbShowDriveFreeSpace: TCheckBox;
    cbShowDrivesListButton: TCheckBox;
    cbShowKeysPanel: TCheckBox;
    cbShowMainMenu: TCheckBox;
    cbShowMainToolBar: TCheckBox;
    cbShowStatusBar: TCheckBox;
    cbShowTabHeader: TCheckBox;
    cbShowTabs: TCheckBox;
    cbTermWindow: TCheckBox;
    cbTwoDiskPanels: TCheckBox;
    cbShowShortDriveFreeSpace: TCheckBox;
    cbUppercaseDriveLetter: TCheckBox;
    cbShowColonAfterDrive: TCheckBox;
    chkShowMiddleToolBar: TCheckBox;
    gbScreenLayout: TGroupBox;
    procedure cbShowDiskPanelChange(Sender: TObject);
    procedure cbShowDriveFreeSpaceChange(Sender: TObject);
  protected
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  uGlobs, uLng;

{ TfrmOptionsLayout }

procedure TfrmOptionsLayout.cbShowDiskPanelChange(Sender: TObject);
begin
  cbTwoDiskPanels.Enabled := cbShowDiskPanel.Checked;
  cbFlatDiskPanel.Enabled := cbShowDiskPanel.Checked;
end;

procedure TfrmOptionsLayout.cbShowDriveFreeSpaceChange(Sender: TObject);
begin
  cbShowShortDriveFreeSpace.Enabled:= cbShowDriveFreeSpace.Checked;
  if not(cbShowDriveFreeSpace.Checked) then cbShowShortDriveFreeSpace.Checked:= false;
end;

class function TfrmOptionsLayout.GetIconIndex: Integer;
begin
  Result := 7;
end;

class function TfrmOptionsLayout.GetTitle: String;
begin
  Result := rsOptionsEditorLayout;
end;

procedure TfrmOptionsLayout.Load;
begin
  cbShowMainMenu.Checked := gMainMenu;
  cbShowMainToolBar.Checked := gButtonBar;
  chkShowMiddleToolBar.Checked := gMiddleToolBar;
  cbShowDiskPanel.Checked := gDriveBar1;
  cbTwoDiskPanels.Checked := gDriveBar2;
  cbFlatDiskPanel.Checked := gDriveBarFlat;
  cbShowDrivesListButton.Checked := gDrivesListButton;
  cbShowTabs.Checked := gDirectoryTabs;
  cbShowCurDir.Checked := gCurDir;
  cbShowTabHeader.Checked := gTabHeader;
  cbShowStatusBar.Checked := gStatusBar;
  cbShowCmdLine.Checked := gCmdLine;
  cbShowKeysPanel.Checked := gKeyButtons;
  cbFlatInterface.Checked := gInterfaceFlat;
  cbLogWindow.Checked := gLogWindow;
  cbTermWindow.Checked := gTermWindow;
  cbShowDriveFreeSpace.Checked := gDriveFreeSpace;
  cbFreespaceInd.Checked := gDriveInd;
  cbProgInMenuBar.Checked := gProgInMenuBar;
  cbPanelOfOperations.Checked := gPanelOfOp;
  cbShowShortDriveFreeSpace.Checked:= gShortFormatDriveInfo;
  cbUppercaseDriveLetter.Checked := gUppercaseDriveLetter;
  cbShowColonAfterDrive.Checked := gShowColonAfterDrive;
end;

function TfrmOptionsLayout.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gMainMenu := cbShowMainMenu.Checked;
  gButtonBar := cbShowMainToolBar.Checked;
  gMiddleToolBar := chkShowMiddleToolBar.Checked;
  gDriveBar1 := cbShowDiskPanel.Checked;
  gDriveBar2 := cbTwoDiskPanels.Checked;
  gDriveBarFlat := cbFlatDiskPanel.Checked;
  gDrivesListButton := cbShowDrivesListButton.Checked;
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
  gShortFormatDriveInfo := cbShowShortDriveFreeSpace.Checked;
  gUppercaseDriveLetter := cbUppercaseDriveLetter.Checked;
  gShowColonAfterDrive := cbShowColonAfterDrive.Checked;
end;

end.

