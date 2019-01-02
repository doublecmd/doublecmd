{
   Double Commander
   -------------------------------------------------------------------------
   DirectoryHotlist configuration for extra options page

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

unit fOptionsDirectoryHotlistExtra;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Buttons, Menus, EditBtn,

  //DC
  fOptionsFrame;

type
  { TfrmOptionsDirectoryHotlistExtra }
  TfrmOptionsDirectoryHotlistExtra = class(TOptionsEditor)
    btnPathToBeRelativeToAll: TButton;
    btnPathToBeRelativeToHelper: TSpeedButton;
    cbDirectoryHotlistFilenameStyle: TComboBox;
    ckbDirectoryHotlistSource: TCheckBox;
    ckbDirectoryHotlistTarget: TCheckBox;
    dePathToBeRelativeTo: TDirectoryEdit;
    gbDirectoryHotlistOptionsExtra: TGroupBox;
    lblApplySettingsFor: TLabel;
    lbPathToBeRelativeTo: TLabel;
    lbDirectoryHotlistFilenameStyle: TLabel;
    pmPathToBeRelativeToHelper: TPopupMenu;
    procedure btnPathToBeRelativeToAllClick(Sender: TObject);
    procedure btnPathToBeRelativeToHelperClick(Sender: TObject);
    procedure cbDirectoryHotlistFilenameStyleChange(Sender: TObject);
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
  uGlobs, uLng, DCStrUtils, fOptions, fOptionsDirectoryHotlist, uSpecialDir;

procedure TfrmOptionsDirectoryHotlistExtra.Init;
begin
  ParseLineToList(rsPluginFilenameStyleList, cbDirectoryHotlistFilenameStyle.Items);
end;

procedure TfrmOptionsDirectoryHotlistExtra.Load;
begin
  cbDirectoryHotlistFilenameStyle.ItemIndex := integer(gHotDirFilenameStyle);
  cbDirectoryHotlistFilenameStyleChange(cbDirectoryHotlistFilenameStyle);
  dePathToBeRelativeTo.Text := gHotDirPathToBeRelativeTo;
  ckbDirectoryHotlistSource.Checked := hdpmSource in gHotDirPathModifierElements;
  ckbDirectoryHotlistTarget.Checked := hdpmTarget in gHotDirPathModifierElements;
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathToBeRelativeToHelper, mp_PATHHELPER, nil);
end;

function TfrmOptionsDirectoryHotlistExtra.Save: TOptionsEditorSaveFlags;
begin
  gHotDirFilenameStyle := TConfigFilenameStyle(cbDirectoryHotlistFilenameStyle.ItemIndex);
  gHotDirPathToBeRelativeTo := dePathToBeRelativeTo.Text;
  gHotDirPathModifierElements := [];
  if ckbDirectoryHotlistSource.Checked then gHotDirPathModifierElements := gHotDirPathModifierElements + [hdpmSource];
  if ckbDirectoryHotlistTarget.Checked then gHotDirPathModifierElements := gHotDirPathModifierElements + [hdpmTarget];
  Result := [];
end;

class function TfrmOptionsDirectoryHotlistExtra.GetIconIndex: integer;
begin
  Result := 33;
end;

class function TfrmOptionsDirectoryHotlistExtra.GetTitle: string;
begin
  Result := rsOptionsEditorDirectoryHotlistExtra;
end;

procedure TfrmOptionsDirectoryHotlistExtra.cbDirectoryHotlistFilenameStyleChange(Sender: TObject);
begin
  lbPathToBeRelativeTo.Visible := (TConfigFilenameStyle(cbDirectoryHotlistFilenameStyle.ItemIndex) = TConfigFilenameStyle.pfsRelativeToFollowingPath);
  dePathToBeRelativeTo.Visible := lbPathToBeRelativeTo.Visible;
  btnPathToBeRelativeToHelper.Visible := lbPathToBeRelativeTo.Visible;
end;

procedure TfrmOptionsDirectoryHotlistExtra.btnPathToBeRelativeToAllClick(Sender: TObject);
var
  Options: IOptionsDialog;
  Editor: TOptionsEditor;
begin
  Self.SaveSettings; //Call "SaveSettings" instead of just "Save" to get option signature set right away do we don't bother user for that page when close.
  Options := ShowOptions(TfrmOptionsDirectoryHotlist);
  Editor := Options.GetEditor(TfrmOptionsDirectoryHotlist);
  TfrmOptionsDirectoryHotlist(Editor).ScanHotDirForFilenameAndPath;
  ShowOptions(TfrmOptionsDirectoryHotlist);
end;

procedure TfrmOptionsDirectoryHotlistExtra.btnPathToBeRelativeToHelperClick(Sender: TObject);
begin
  dePathToBeRelativeTo.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(dePathToBeRelativeTo, pfPATH);
  pmPathToBeRelativeToHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

end.
