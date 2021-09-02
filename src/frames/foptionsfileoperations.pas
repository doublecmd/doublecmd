{
   Double Commander
   -------------------------------------------------------------------------
   File operations options page

   Copyright (C) 2006-2021 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsFileOperations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Spin, ExtCtrls, KASComboBox, DividerBevel,
  fOptionsFrame;

type

  { TfrmOptionsFileOperations }

  TfrmOptionsFileOperations = class(TOptionsEditor)
    bvlConfirmations: TDividerBevel;
    cbDeleteToTrash: TCheckBox;
    cbDropReadOnlyFlag: TCheckBox;
    cbProcessComments: TCheckBox;
    cbRenameSelOnlyName: TCheckBox;
    cbShowCopyTabSelectPanel: TCheckBox;
    cbSkipFileOpError: TCheckBox;
    cbProgressKind: TComboBoxAutoWidth;
    cbCopyConfirmation: TCheckBox;
    cbMoveConfirmation: TCheckBox;
    cbDeleteConfirmation: TCheckBox;
    cbDeleteToTrashConfirmation: TCheckBox;
    cbVerifyChecksumConfirmation: TCheckBox;
    cbTestArchiveConfirmation: TCheckBox;
    cmbTypeOfDuplicatedRename: TComboBoxAutoWidth;
    edtBufferSize: TEdit;
    edtHashBufferSize: TEdit;
    gbUserInterface: TGroupBox;
    gbExecutingOperations: TGroupBox;
    lblHashBufferSize: TLabel;
    lblTypeOfDuplicatedRename: TLabel;
    lblBufferSize: TLabel;
    lblProgressKind: TLabel;
    lblWipePassNumber: TLabel;
    seWipePassNumber: TSpinEdit;
    procedure cbDeleteToTrashChange(Sender: TObject);
  private
    FLoading: Boolean;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    constructor Create(TheOwner: TComponent); override;
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  DCStrUtils, uGlobs, uLng, fOptionsHotkeys;

{ TfrmOptionsFileOperations }

class function TfrmOptionsFileOperations.GetIconIndex: Integer;
begin
  Result := 8;
end;

class function TfrmOptionsFileOperations.GetTitle: String;
begin
  Result := rsOptionsEditorFileOperations;
end;

procedure TfrmOptionsFileOperations.Init;
begin
  ParseLineToList(rsOptFileOperationsProgressKind, cbProgressKind.Items);
  ParseLineToList(rsOptTypeOfDuplicatedRename, cmbTypeOfDuplicatedRename.Items);
end;

procedure TfrmOptionsFileOperations.cbDeleteToTrashChange(Sender: TObject);
var
  HotkeysEditor: TOptionsEditor;
begin
  if not FLoading then
  begin
    HotkeysEditor := OptionsDialog.GetEditor(TfrmOptionsHotkeys);
    if Assigned(HotkeysEditor) then
      (HotkeysEditor as TfrmOptionsHotkeys).AddDeleteWithShiftHotkey(cbDeleteToTrash.Checked);
  end;
end;

procedure TfrmOptionsFileOperations.Load;
begin
  FLoading := True;

  edtBufferSize.Text               := IntToStr(gCopyBlockSize div 1024);
  edtHashBufferSize.Text           := IntToStr(gHashBlockSize div 1024);
  cbSkipFileOpError.Checked        := gSkipFileOpError;
  cbDropReadOnlyFlag.Checked       := gDropReadOnlyFlag;
  seWipePassNumber.Value           := gWipePassNumber;
  cbProcessComments.Checked        := gProcessComments;
  cbShowCopyTabSelectPanel.Checked := gShowCopyTabSelectPanel;
  cbDeleteToTrash.Checked          := gUseTrash;
  cbRenameSelOnlyName.Checked      := gRenameSelOnlyName;

  case gFileOperationsProgressKind of
    fopkSeparateWindow:           cbProgressKind.ItemIndex := 0;
    fopkSeparateWindowMinimized:  cbProgressKind.ItemIndex := 1;
    fopkOperationsPanel:          cbProgressKind.ItemIndex := 2;
  end;

  cbCopyConfirmation.Checked           := focCopy in gFileOperationsConfirmations;
  cbMoveConfirmation.Checked           := focMove in gFileOperationsConfirmations;
  cbDeleteConfirmation.Checked         := focDelete in gFileOperationsConfirmations;
  cbTestArchiveConfirmation.Checked    := focTestArchive in gFileOperationsConfirmations;
  cbDeleteToTrashConfirmation.Checked  := focDeleteToTrash in gFileOperationsConfirmations;
  cbVerifyChecksumConfirmation.Checked := focVerifyChecksum in gFileOperationsConfirmations;
  cmbTypeOfDuplicatedRename.ItemIndex  := Integer(gTypeOfDuplicatedRename);

  FLoading := False;
end;

function TfrmOptionsFileOperations.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gCopyBlockSize          := StrToIntDef(edtBufferSize.Text, gCopyBlockSize div 1024) * 1024;
  gHashBlockSize          := StrToIntDef(edtHashBufferSize.Text, gHashBlockSize div 1024) * 1024;
  gSkipFileOpError        := cbSkipFileOpError.Checked;
  gDropReadOnlyFlag       := cbDropReadOnlyFlag.Checked;
  gWipePassNumber         := seWipePassNumber.Value;
  gProcessComments        := cbProcessComments.Checked;
  gShowCopyTabSelectPanel := cbShowCopyTabSelectPanel.Checked;
  gUseTrash               := cbDeleteToTrash.Checked;
  gRenameSelOnlyName      := cbRenameSelOnlyName.Checked;

  case cbProgressKind.ItemIndex of
    0: gFileOperationsProgressKind := fopkSeparateWindow;
    1: gFileOperationsProgressKind := fopkSeparateWindowMinimized;
    2: gFileOperationsProgressKind := fopkOperationsPanel;
  end;

  gFileOperationsConfirmations := [];
  if cbCopyConfirmation.Checked then
    Include(gFileOperationsConfirmations, focCopy);
  if cbMoveConfirmation.Checked then
    Include(gFileOperationsConfirmations, focMove);
  if cbDeleteConfirmation.Checked then
    Include(gFileOperationsConfirmations, focDelete);
  if cbTestArchiveConfirmation.Checked then
    Include(gFileOperationsConfirmations, focTestArchive);
  if cbDeleteToTrashConfirmation.Checked then
    Include(gFileOperationsConfirmations, focDeleteToTrash);
  if cbVerifyChecksumConfirmation.Checked then
    Include(gFileOperationsConfirmations, focVerifyChecksum);
  gTypeOfDuplicatedRename := tDuplicatedRename(cmbTypeOfDuplicatedRename.ItemIndex);
end;

constructor TfrmOptionsFileOperations.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLoading := False;
end;

end.

