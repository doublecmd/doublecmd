{
    Double Commander
    -------------------------------------------------------------------------
    Extra File Associations Configuration

    Copyright (C) 2016-2019 Alexander Koblov (alexx2000@mail.ru)

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


unit fOptionsFileAssocExtra;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fOptionsFrame, StdCtrls, ExtCtrls, Buttons, EditBtn, Menus;
  
type

  { TfrmOptionsFileAssocExtra }

  TfrmOptionsFileAssocExtra = class(TOptionsEditor)
    btnPathToBeRelativeToAll: TButton;
    btnPathToBeRelativeToHelper: TSpeedButton;
    cbOfferToAddToFileAssociations: TCheckBox;
    cbExecuteViaShell: TCheckBox;
    cbExtendedContextMenu: TCheckBox;
    cbOpenSystemWithTerminalClose: TCheckBox;
    cbOpenSystemWithTerminalStayOpen: TCheckBox;
    cbIncludeConfigFileAssoc: TCheckBox;
    cbFileAssocFilenameStyle: TComboBox;
    ckbFileAssocCommand: TCheckBox;
    ckbFileAssocIcons: TCheckBox;
    ckbFileAssocStartPath: TCheckBox;
    dePathToBeRelativeTo: TDirectoryEdit;
    gbExtendedContextMenuOptions: TGroupBox;
    gbToolbarOptionsExtra: TGroupBox;
    lblApplySettingsFor: TLabel;
    lbPathToBeRelativeTo: TLabel;
    lbFileAssocFilenameStyle: TLabel;
    pmPathToBeRelativeToHelper: TPopupMenu;
    procedure btnPathToBeRelativeToAllClick(Sender: TObject);
    procedure btnPathToBeRelativeToHelperClick(Sender: TObject);
    procedure cbExtendedContextMenuChange(Sender: TObject);
    procedure cbFileAssocFilenameStyleChange(Sender: TObject);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetTitle: string; override;
    class function GetIconIndex: integer; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Controls,

  //DC
  DCStrUtils, uGlobs, uLng, uSpecialDir, fOptions, fOptionsFileAssoc;

{TfrmOptionsFileAssocExtra}

procedure TfrmOptionsFileAssocExtra.Init;
begin
  ParseLineToList(rsPluginFilenameStyleList, cbFileAssocFilenameStyle.Items);
end;

{ TfrmOptionsFileAssocExtra.GetTitle }
class function TfrmOptionsFileAssocExtra.GetTitle: string;
begin
  Result := rsOptionsEditorFileAssicExtra;
end;

{ TfrmOptionsFileAssocExtra.GetIconIndex }
class function TfrmOptionsFileAssocExtra.GetIconIndex: integer;
begin
  Result := 36;
end;

{ TfrmOptionsFileAssocExtra.cbExtendedContextMenuChange }
procedure TfrmOptionsFileAssocExtra.cbExtendedContextMenuChange(Sender: TObject);
begin
  gbExtendedContextMenuOptions.Enabled := TCheckbox(Sender).Checked;
end;

{ TfrmOptionsFileAssocExtra.Load }
procedure TfrmOptionsFileAssocExtra.Load;
begin
  cbOfferToAddToFileAssociations.Checked := gOfferToAddToFileAssociations;
  cbExtendedContextMenu.Checked := gExtendedContextMenu;
  cbOpenSystemWithTerminalStayOpen.Checked := gExecuteViaTerminalStayOpen;
  cbOpenSystemWithTerminalClose.Checked := gExecuteViaTerminalClose;
  cbExecuteViaShell.Checked := gOpenExecuteViaShell;
  cbIncludeConfigFileAssoc.Checked := gIncludeFileAssociation;
  cbExtendedContextMenuChange(cbExtendedContextMenu);
  cbFileAssocFilenameStyle.ItemIndex := integer(gFileAssocFilenameStyle);
  cbFileAssocFilenameStyleChange(cbFileAssocFilenameStyle);
  dePathToBeRelativeTo.Text := gFileAssocPathToBeRelativeTo;
  ckbFileAssocIcons.Checked := fameIcon in gFileAssocPathModifierElements;
  ckbFileAssocCommand.Checked := fameCommand in gFileAssocPathModifierElements;
  ckbFileAssocStartPath.Checked := fameStartingPath in gFileAssocPathModifierElements;
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathToBeRelativeToHelper, mp_PATHHELPER, nil);

end;

{ TfrmOptionsFileAssocExtra.Save }
function TfrmOptionsFileAssocExtra.Save: TOptionsEditorSaveFlags;
begin
  gOfferToAddToFileAssociations := cbOfferToAddToFileAssociations.Checked;
  gExtendedContextMenu := cbExtendedContextMenu.Checked;
  gExecuteViaTerminalStayOpen := cbOpenSystemWithTerminalStayOpen.Checked;
  gExecuteViaTerminalClose := cbOpenSystemWithTerminalClose.Checked;
  gOpenExecuteViaShell := cbExecuteViaShell.Checked;
  gIncludeFileAssociation := cbIncludeConfigFileAssoc.Checked;
  gFileAssocFilenameStyle := TConfigFilenameStyle(cbFileAssocFilenameStyle.ItemIndex);
  gFileAssocPathToBeRelativeTo := dePathToBeRelativeTo.Text;
  gFileAssocPathModifierElements := [];
  if ckbFileAssocIcons.Checked then gFileAssocPathModifierElements := gFileAssocPathModifierElements + [fameIcon];
  if ckbFileAssocCommand.Checked then gFileAssocPathModifierElements := gFileAssocPathModifierElements + [fameCommand];
  if ckbFileAssocStartPath.Checked then gFileAssocPathModifierElements := gFileAssocPathModifierElements + [fameStartingPath];
  Result := [];
end;

{ TfrmOptionsFileAssocExtra.btnPathToBeRelativeToHelperClick }
procedure TfrmOptionsFileAssocExtra.btnPathToBeRelativeToHelperClick(Sender: TObject);
begin
  dePathToBeRelativeTo.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(dePathToBeRelativeTo, pfPATH);
  pmPathToBeRelativeToHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmOptionsFileAssocExtra.cbFileAssocFilenameStyleChange }
procedure TfrmOptionsFileAssocExtra.cbFileAssocFilenameStyleChange(Sender: TObject);
begin
  lbPathToBeRelativeTo.Visible := (TConfigFilenameStyle(cbFileAssocFilenameStyle.ItemIndex) = TConfigFilenameStyle.pfsRelativeToFollowingPath);
  dePathToBeRelativeTo.Visible := lbPathToBeRelativeTo.Visible;
  btnPathToBeRelativeToHelper.Visible := lbPathToBeRelativeTo.Visible;
end;

{ TfrmOptionsFileAssocExtra.btnPathToBeRelativeToAllClick }
procedure TfrmOptionsFileAssocExtra.btnPathToBeRelativeToAllClick(Sender: TObject);
var
  Options: IOptionsDialog;
  Editor: TOptionsEditor;
begin
  Self.SaveSettings; //Call "SaveSettings" instead of just "Save" to get option signature set right away do we don't bother user for that page when close.
  Options := ShowOptions(TfrmOptionsFileAssoc);
  Editor := Options.GetEditor(TfrmOptionsFileAssoc);
  TfrmOptionsFileAssoc(Editor).ScanFileAssocForFilenameAndPath;
  ShowOptions(TfrmOptionsFileAssoc);
end;


end.
