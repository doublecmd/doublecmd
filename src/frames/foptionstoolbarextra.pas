{
   Double Commander
   -------------------------------------------------------------------------
   Toolbar configuration for extra options page

   Copyright (C) 2019 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsToolbarExtra;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Buttons, Menus, EditBtn,

  //DC
  fOptionsFrame;

type
  { TfrmOptionsToolbarExtra }
  TfrmOptionsToolbarExtra = class(TOptionsEditor)
    btnPathToBeRelativeToAll: TButton;
    btnPathToBeRelativeToHelper: TSpeedButton;
    cbToolbarFilenameStyle: TComboBox;
    ckbToolbarIcons: TCheckBox;
    ckbToolbarCommand: TCheckBox;
    ckbToolbarStartPath: TCheckBox;
    dePathToBeRelativeTo: TDirectoryEdit;
    gbToolbarOptionsExtra: TGroupBox;
    lblApplySettingsFor: TLabel;
    lbPathToBeRelativeTo: TLabel;
    lbToolbarFilenameStyle: TLabel;
    pmPathToBeRelativeToHelper: TPopupMenu;
    procedure btnPathToBeRelativeToAllClick(Sender: TObject);
    procedure btnPathToBeRelativeToHelperClick(Sender: TObject);
    procedure cbToolbarFilenameStyleChange(Sender: TObject);
  private
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

  //DC
  uGlobs, uLng, DCStrUtils, fOptions, fOptionsToolbar, uSpecialDir;

procedure TfrmOptionsToolbarExtra.Init;
begin
  ParseLineToList(rsPluginFilenameStyleList, cbToolbarFilenameStyle.Items);
end;

procedure TfrmOptionsToolbarExtra.Load;
begin
  cbToolbarFilenameStyle.ItemIndex := integer(gToolbarFilenameStyle);
  cbToolbarFilenameStyleChange(cbToolbarFilenameStyle);
  dePathToBeRelativeTo.Text := gToolbarPathToBeRelativeTo;
  ckbToolbarIcons.Checked := tpmeIcon in gToolbarPathModifierElements;
  ckbToolbarCommand.Checked := tpmeCommand in gToolbarPathModifierElements;
  ckbToolbarStartPath.Checked := tpmeStartingPath in gToolbarPathModifierElements;
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathToBeRelativeToHelper, mp_PATHHELPER, nil);
end;

function TfrmOptionsToolbarExtra.Save: TOptionsEditorSaveFlags;
begin
  gToolbarFilenameStyle := TConfigFilenameStyle(cbToolbarFilenameStyle.ItemIndex);
  gToolbarPathToBeRelativeTo := dePathToBeRelativeTo.Text;
  gToolbarPathModifierElements := [];
  if ckbToolbarIcons.Checked then gToolbarPathModifierElements := gToolbarPathModifierElements + [tpmeIcon];
  if ckbToolbarCommand.Checked then gToolbarPathModifierElements := gToolbarPathModifierElements + [tpmeCommand];
  if ckbToolbarStartPath.Checked then gToolbarPathModifierElements := gToolbarPathModifierElements + [tpmeStartingPath];
  Result := [];
end;

class function TfrmOptionsToolbarExtra.GetIconIndex: integer;
begin
  Result := 32;
end;

class function TfrmOptionsToolbarExtra.GetTitle: string;
begin
  Result := rsOptionsEditorToolbarExtra;
end;

procedure TfrmOptionsToolbarExtra.cbToolbarFilenameStyleChange(Sender: TObject);
begin
  lbPathToBeRelativeTo.Visible := (TConfigFilenameStyle(cbToolbarFilenameStyle.ItemIndex) = TConfigFilenameStyle.pfsRelativeToFollowingPath);
  dePathToBeRelativeTo.Visible := lbPathToBeRelativeTo.Visible;
  btnPathToBeRelativeToHelper.Visible := lbPathToBeRelativeTo.Visible;
end;

procedure TfrmOptionsToolbarExtra.btnPathToBeRelativeToAllClick(Sender: TObject);
var
  Options: IOptionsDialog;
  Editor: TOptionsEditor;
begin
  Self.SaveSettings; //Call "SaveSettings" instead of just "Save" to get option signature set right away do we don't bother user for that page when close.
  Options := ShowOptions(TfrmOptionsToolbar);
  Editor := Options.GetEditor(TfrmOptionsToolbar);
  TfrmOptionsToolbar(Editor).ScanToolbarForFilenameAndPath(TfrmOptionsToolbar(Editor).TopToolbar);
  TfrmOptionsToolbar(Editor).RefrechCurrentButton;
  ShowOptions(TfrmOptionsToolbar);
end;

procedure TfrmOptionsToolbarExtra.btnPathToBeRelativeToHelperClick(Sender: TObject);
begin
  dePathToBeRelativeTo.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(dePathToBeRelativeTo, pfPATH);
  pmPathToBeRelativeToHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

end.
