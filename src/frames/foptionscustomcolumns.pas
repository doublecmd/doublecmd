{
   Double Commander
   -------------------------------------------------------------------------
   Custom columns options page

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Copyright (C) 2008-2016 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsCustomColumns;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  ComCtrls, Controls, Classes, SysUtils, StdCtrls, ExtCtrls, Forms, ColorBox,
  Buttons, Spin, Grids, Menus, Dialogs,
  //DC
  uColumns, fOptionsFrame
  {$IFDEF COLUMNSFILEVIEW_VTV}
  , uColumnsFileViewVtv
  {$ELSE}
  , uColumnsFileView
  {$ENDIF}
  ;

type

  { TfrmOptionsCustomColumns }
  TfrmOptionsCustomColumns = class(TOptionsEditor)
    btnAllAllowOverColor: TButton;
    btnAllBackColor: TButton;
    btnAllBackColor2: TButton;
    btnAllCursorColor: TButton;
    btnAllCursorText: TButton;
    btnAllFont: TButton;
    btnAllForeColor: TButton;
    btnAllInactiveCursorColor: TButton;
    btnAllInactiveMarkColor: TButton;
    btnAllMarkColor: TButton;
    btnAllUseInactiveSelColor: TButton;
    btnAllUseInvertedSelection: TButton;
    btnBackColor: TButton;
    btnBackColor2: TButton;
    btnCursorBorderColor: TButton;
    btnGotoSetDefault: TButton;
    btnResetCursorBorder: TButton;
    btnResetFrameCursor: TButton;
    btnSaveAsConfigColumns: TButton;
    btnCursorColor: TButton;
    btnCursorText: TButton;
    btnDeleteConfigColumns: TButton;
    btnFont: TBitBtn;
    btnForeColor: TButton;
    btnInactiveCursorColor: TButton;
    btnInactiveMarkColor: TButton;
    btnMarkColor: TButton;
    btnNext: TButton;
    btnNewConfig: TButton;
    btnPrev: TButton;
    btnRenameConfigColumns: TButton;
    btnResetAllowOverColor: TButton;
    btnResetBackColor: TButton;
    btnResetBackColor2: TButton;
    btnResetCursorColor: TButton;
    btnResetCursorText: TButton;
    btnResetFont: TButton;
    btnResetForeColor: TButton;
    btnResetInactiveCursorColor: TButton;
    btnResetInactiveMarkColor: TButton;
    btnResetMarkColor: TButton;
    btnResetUseInactiveSelColor: TButton;
    btnResetUseInvertedSelection: TButton;
    btnSaveConfigColumns: TButton;
    cbAllowOverColor: TCheckBox;
    cbApplyChangeForAllColumns: TCheckBox;
    cbBackColor: TColorBox;
    cbBackColor2: TColorBox;
    cbConfigColumns: TComboBox;
    cbCursorBorder: TCheckBox;
    cbCursorBorderColor: TColorBox;
    cbCursorColor: TColorBox;
    cbCursorText: TColorBox;
    cbForeColor: TColorBox;
    cbInactiveCursorColor: TColorBox;
    cbInactiveMarkColor: TColorBox;
    cbMarkColor: TColorBox;
    cbUseFrameCursor: TCheckBox;
    cbUseInactiveSelColor: TCheckBox;
    cbUseInvertedSelection: TCheckBox;
    chkUseCustomView: TCheckBox;
    cmbFileSystem: TComboBox;
    dlgcolor: TColorDialog;
    dlgfont: TFontDialog;
    edtFont: TEdit;
    lblFileSystem: TLabel;
    lblBackColor: TLabel;
    lblBackColor2: TLabel;
    lblConfigColumns: TLabel;
    lblCurrentColumn: TLabel;
    lblCursorColor: TLabel;
    lblCursorText: TLabel;
    lblFontName: TLabel;
    lblFontSize: TLabel;
    lblForeColor: TLabel;
    lblInactiveCursorColor: TLabel;
    lblInactiveMarkColor: TLabel;
    lblMarkColor: TLabel;
    lblPreviewTop: TLabel;
    lblWorkingColumn: TLabel;
    miAddColumn: TMenuItem;
    pnlCommon: TPanel;
    pnlCustomColumnsViewSettings: TPanel;
    pmFields: TPopupMenu;
    pmStringGrid: TPopupMenu;
    pnlActualCont: TPanel;
    pnlConfigColumns: TPanel;
    pnlGeneralColumnsViewSettings: TPanel;
    pnlLeft: TPanel;
    pnlPreviewCont: TPanel;
    pnlRight: TPanel;
    sneFontSize: TSpinEdit;
    spGridArea: TSplitter;
    spltBetweenPanels: TSplitter;
    stgColumns: TStringGrid;
    procedure btnGotoSetDefaultClick(Sender: TObject);
    procedure cmbFileSystemChange(Sender: TObject);
    procedure FillFileSystemList;
    procedure FillColumnsList;
    procedure cbConfigColumnsChange(Sender: TObject);
    procedure btnSaveConfigColumnsClick(Sender: TObject);
    procedure btnDeleteConfigColumnsClick(Sender: TObject);
    procedure UpdatePageInfoFromColumnClass;
    procedure UpdateColumnClass;
    procedure stgColumnsSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
    procedure stgColumnsKeyDown(Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
    procedure stgColumnsMouseDown(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: integer);
    procedure stgColumnsMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure CreateEditingControls;
    procedure EditorKeyDown(Sender: TObject; var Key: word; {%H-}Shift: TShiftState);
    procedure AddNewField;
    procedure miAddColumnClick(Sender: TObject);
    procedure stgSetSelectionAsHintToUser;
    procedure stgColumnsEditingDone(Sender: TObject);
    procedure MenuFieldsClick(Sender: TObject);
    procedure EditorSaveResult(Sender: TObject);
    procedure CustomSomethingChanged(Sender: TObject);
    procedure LoadCustColumn(const Index: integer);
    procedure chkUseCustomViewChange(Sender: TObject);
    procedure cbCursorBorderChange(Sender: TObject);
    procedure cbCursorBorderColorChange(Sender: TObject);
    procedure btnCursorBorderColorClick(Sender: TObject);
    procedure btnResetCursorBorderClick(Sender: TObject);
    procedure cbUseFrameCursorChange(Sender: TObject);
    procedure btnResetFrameCursorClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure cbApplyChangeForAllColumnsChange(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure sneFontSizeChange(Sender: TObject);
    procedure btnResetFontClick(Sender: TObject);
    procedure btnAllForeColorClick(Sender: TObject);
    procedure cbForeColorChange(Sender: TObject);
    procedure btnForeColorClick(Sender: TObject);
    procedure btnResetForeColorClick(Sender: TObject);
    procedure cbBackColorChange(Sender: TObject);
    procedure btnBackColorClick(Sender: TObject);
    procedure btnResetBackColorClick(Sender: TObject);
    procedure cbBackColor2Change(Sender: TObject);
    procedure btnBackColor2Click(Sender: TObject);
    procedure btnResetBackColor2Click(Sender: TObject);
    procedure cbMarkColorChange(Sender: TObject);
    procedure btnMarkColorClick(Sender: TObject);
    procedure btnResetMarkColorClick(Sender: TObject);
    procedure cbCursorColorChange(Sender: TObject);
    procedure btnCursorColorClick(Sender: TObject);
    procedure btnResetCursorColorClick(Sender: TObject);
    procedure cbCursorTextChange(Sender: TObject);
    procedure btnCursorTextClick(Sender: TObject);
    procedure btnResetCursorTextClick(Sender: TObject);
    procedure cbInactiveCursorColorChange(Sender: TObject);
    procedure btnInactiveCursorColorClick(Sender: TObject);
    procedure btnResetInactiveCursorColorClick(Sender: TObject);
    procedure cbInactiveMarkColorChange(Sender: TObject);
    procedure btnInactiveMarkColorClick(Sender: TObject);
    procedure btnResetInactiveMarkColorClick(Sender: TObject);
    procedure cbUseInvertedSelectionChange(Sender: TObject);
    procedure btnResetUseInvertedSelectionClick(Sender: TObject);
    procedure cbUseInactiveSelColorChange(Sender: TObject);
    procedure btnResetUseInactiveSelColorClick(Sender: TObject);
    procedure cbAllowOvercolorChange(Sender: TObject);
    procedure btnResetAllowOverColorClick(Sender: TObject);
    procedure pnlLeftEnter(Sender: TObject);
    procedure pnlRightEnter(Sender: TObject);
    procedure OnColumnResized(Sender: TObject; ColumnIndex: integer; ColumnNewsize: integer);

    {Editors}
    procedure SpinEditExit(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure BitBtnDeleteFieldClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure ComboBoxXSelect(Sender: TObject);
    procedure UpDownXClick(Sender: TObject; {%H-}Button: TUDBtnType);
    procedure UpDownXChanging(Sender: TObject; var {%H-}AllowChange: boolean);

  private
    ColPrm: TColPrm;
    ColumnClass: TPanelColumnsClass;
    PreviewLeftPanel: TColumnsFileView;
    PreviewRightPanel: TColumnsFileView;

    updWidth: TSpinEdit;
    cbbAlign: TComboBox;
    edtField: TEdit;
    btnAdd: TButton;
    btnDel: TBitBtn;
    updMove: TUpDown;

    bColumnConfigLoaded: boolean;
    FUpdating: boolean;
    ColumnClassOwnership: boolean;
    IndexRaw: integer;
    FCellValue: string;

  protected
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
    procedure Done; override;

  public
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
    function IsSignatureComputedFromAllWindowComponents: Boolean; override;
    function ExtraOptionsSignature(CurrentSignature:dword):dword; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  strutils, Graphics, LCLType,
  //DC
  DCStrUtils, fOptions, uShowMsg, uDebug, uFileFunctions, DCOSUtils,
  uFileSystemFileSource, uDCUtils, uGlobs, uLng, fMain,
  fOptionsFilePanelsColors;

{ TfrmOptionsCustomColumns }

type
  THackStringGrid = class(TCustomStringGrid)
  end;

{ TfrmOptionsCustomColumns.Load }
procedure TfrmOptionsCustomColumns.Load;
begin
  //1. Init some flags
  bColumnConfigLoaded := False;
  FUpdating := False;
  ColumnClassOwnership := True;

  //2. Create some objects we need for this page.
  ColPrm := nil;
  ColumnClass := TPanelColumnsClass.Create;
  lblPreviewTop.Caption := rsMsgPanelPreview;
  CreateEditingControls;

  //3. Load stuff for our preview
  lblWorkingColumn.Caption := rsConfCustHeader + ':';
  PreviewLeftPanel := TColumnsFileView.Create(pnlLeft, TFileSystemFileSource.Create, mbGetCurrentDir);
  PreviewLeftPanel.OnColumnResized := @Self.OnColumnResized;
  PreviewLeftPanel.JustForColorPreviewSetActiveState(True);
  PreviewRightPanel := TColumnsFileView.Create(pnlRight, TFileSystemFileSource.Create, mbGetCurrentDir);
  PreviewRightPanel.OnColumnResized := @Self.OnColumnResized;
  PreviewRightPanel.JustForColorPreviewSetActiveState(False);

  //4. Load our list of columns set.
  FillFileSystemList;
  FillColumnsList;

  //5. Select the one we currently have in the active panel if possible. User won't be lost and it's the most pertinent thing to do.
  if frmMain.ActiveNotebook.ActiveView.ClassNameIs('TColumnsFileView') then
  begin
    cbConfigColumns.ItemIndex := cbConfigColumns.Items.IndexOf(TColumnsFileView(frmMain.ActiveNotebook.ActiveView).ActiveColm);
    pnlLeft.Width := frmMain.ActiveNotebook.Width;
  end;
  if (cbConfigColumns.ItemIndex = -1) and (cbConfigColumns.Items.Count > 0) then
    cbConfigColumns.ItemIndex := 0;

  //6. We have mostly loaded what needed to be load.
  bColumnConfigLoaded := True;

  //7. Now let's show what we've got for that view.
  cbConfigColumnsChange(cbConfigColumns);

  //8. Local action
  cbApplyChangeForAllColumns.Checked := gCustomColumnsChangeAllColumns;
end;

{ TfrmOptionsCustomColumns.Save }
function TfrmOptionsCustomColumns.Save: TOptionsEditorSaveFlags;
begin
  gCustomColumnsChangeAllColumns := cbApplyChangeForAllColumns.Checked;
  btnSaveConfigColumnsClick(btnSaveConfigColumns);
  Result := [];
end;

{ TfrmOptionsCustomColumns.Done }
procedure TfrmOptionsCustomColumns.Done;
var
  i: integer;
begin
  if (ColumnClassOwnership = True) and Assigned(ColumnClass) then
    FreeAndNil(ColumnClass);

  if Assigned(PreviewLeftPanel) then
    FreeAndNil(PreviewLeftPanel);

  if Assigned(PreviewRightPanel) then
    FreeAndNil(PreviewRightPanel);

  // Free TColPrm objects assigned to each row.
  for i := 0 to stgColumns.RowCount - 1 do
  begin
    if Assigned(stgColumns.Objects[6, i]) then
    begin
      (stgColumns.Objects[6, i] as TColPrm).Free;
      stgColumns.Objects[6, i] := nil;
    end;
  end;
end;

{ TfrmOptionsCustomColumns.GetIconIndex }
class function TfrmOptionsCustomColumns.GetIconIndex: integer;
begin
  Result := 30;
end;

{ TfrmOptionsCustomColumns.GetTitle }
class function TfrmOptionsCustomColumns.GetTitle: string;
begin
  Result := rsOptionsEditorCustomColumns;
end;

{ TfrmOptionsCustomColumns.IsSignatureComputedFromAllWindowComponents }
function TfrmOptionsCustomColumns.IsSignatureComputedFromAllWindowComponents: Boolean;
begin
  result := False;
end;

{ TfrmOptionsCustomColumns.ExtraOptionsSignature }
function TfrmOptionsCustomColumns.ExtraOptionsSignature(CurrentSignature:dword):dword;
begin
  result := ColumnClass.GetSignature(CurrentSignature);
end;

{ TfrmOptionsCustomColumns.FillColumnsList }
procedure TfrmOptionsCustomColumns.FillColumnsList;
var
  Index: Integer;
begin
  cbConfigColumns.Clear;
  for Index:= 0 to ColSet.Items.Count - 1 do
  begin
    if SameText(TPanelColumnsClass(ColSet.Items.Objects[Index]).FileSystem, cmbFileSystem.Text) then
    begin
      cbConfigColumns.Items.AddObject(ColSet.Items[Index], TObject(PtrInt(Index)));
    end;
  end;
end;

{ TfrmOptionsCustomColumns.btnGotoSetDefaultClick }
procedure TfrmOptionsCustomColumns.btnGotoSetDefaultClick(Sender: TObject);
begin
  ShowOptions(TfrmOptionsFilePanelsColors);
end;

procedure TfrmOptionsCustomColumns.cmbFileSystemChange(Sender: TObject);
begin
  FillColumnsList;
  if cbConfigColumns.Items.Count > 0 then
  begin
    cbConfigColumns.ItemIndex:= 0;
    cbConfigColumnsChange(cbConfigColumns);
  end
  else begin
    stgColumns.RowCount:= 1;
    btnRenameConfigColumns.Enabled:= False;
    btnDeleteConfigColumns.Enabled:= False;
  end;
  pnlActualCont.Enabled:= cbConfigColumns.Items.Count > 0;
  btnSaveAsConfigColumns.Enabled:= pnlActualCont.Enabled;
  cbConfigColumns.Enabled:= pnlActualCont.Enabled;
end;

procedure TfrmOptionsCustomColumns.FillFileSystemList;
var
  Index: Integer;
begin
  cmbFileSystem.Clear;
  cmbFileSystem.Items.Add(FS_GENERAL);
  for Index:= 0 to gWFXPlugins.Count - 1 do
  begin
    cmbFileSystem.Items.Add(gWFXPlugins.Name[Index]);
  end;
  cmbFileSystem.ItemIndex:= 0;
end;

{ TfrmOptionsCustomColumns.cbConfigColumnsChange }
procedure TfrmOptionsCustomColumns.cbConfigColumnsChange(Sender: TObject);
begin
  if bColumnConfigLoaded then
  begin
    ColumnClass.Assign(ColSet.GetColumnSet(PtrInt(cbConfigColumns.Items.Objects[cbConfigColumns.ItemIndex])));
    LastLoadedOptionSignature := ComputeCompleteOptionsSignature;
    cbConfigColumns.Enabled := True;
    btnSaveConfigColumns.Enabled := False;
    btnRenameConfigColumns.Enabled := True;
    btnNewConfig.Enabled := True;
    UpdatePageInfoFromColumnClass;
  end;
end;

{ TfrmOptionsCustomColumns.btnSaveConfigColumnsClick }
procedure TfrmOptionsCustomColumns.btnSaveConfigColumnsClick(Sender: TObject);
var
  Index: PtrInt;
  SuggestedCustomColumnsName: String;
  ColumnClassForConfig: TPanelColumnsClass;
begin
  // We won't free that one obviously because it's the one that will now be in global application system memory
  ColumnClassForConfig := TPanelColumnsClass.Create;
  if cbConfigColumns.Items.Count > 0 then
  begin
    UpdateColumnClass;
    ColumnClassForConfig.Assign(ColumnClass);
    Index:= PtrInt(cbConfigColumns.Items.Objects[cbConfigColumns.ItemIndex]);
  end;

  case TComponent(Sender).tag of
    1: // Save.
    begin
      ColSet.DeleteColumnSet(Index);
      Colset.Insert(Index, ColumnClassForConfig);
      cbConfigColumnsChange(cbConfigColumns);
    end;

    2: // Save as.
    begin
      SuggestedCustomColumnsName := ColumnClassForConfig.Name + '(' + GetDateTimeInStrEZSortable(now) + ')';
      ShowInputQuery(rsOptionsEditorCustomColumns, rsMenuConfigureEnterCustomColumnName, SuggestedCustomColumnsName);
      if (SuggestedCustomColumnsName = '') or (cbConfigColumns.Items.indexof(SuggestedCustomColumnsName) <> -1) then
        SuggestedCustomColumnsName := ColumnClassForConfig.Name + '(' + GetDateTimeInStrEZSortable(now) + ')';
      ColumnClassForConfig.Name := SuggestedCustomColumnsName;
      ColSet.Add(ColumnClassForConfig);
      FillColumnsList;
      cbConfigColumns.ItemIndex := cbConfigColumns.Items.IndexOf(ColumnClassForConfig.Name);
      cbConfigColumnsChange(cbConfigColumns);
    end;

    3: // New.
    begin
      FreeAndNil(ColumnClassForConfig);
      ColumnClassForConfig := TPanelColumnsClass.Create;
      ColumnClassForConfig.AddDefaultEverything;
      ColumnClassForConfig.FileSystem := cmbFileSystem.Text;
      ColumnClassForConfig.Name := ColumnClassForConfig.Name + ' (' + GetDateTimeInStrEZSortable(now) + ')';
      ColSet.Add(ColumnClassForConfig);
      cmbFileSystemChange(cmbFileSystem);
      cbConfigColumns.ItemIndex := cbConfigColumns.Items.IndexOf(ColumnClassForConfig.Name);
      cbConfigColumnsChange(cbConfigColumns);
    end;

    4: // Rename.
    begin
      SuggestedCustomColumnsName := cbConfigColumns.Items.Strings[cbConfigColumns.ItemIndex];
      if ShowInputQuery(rsOptionsEditorCustomColumns, rsMenuConfigureEnterCustomColumnName, SuggestedCustomColumnsName) then
      begin
        if (SuggestedCustomColumnsName <> '') then
        begin
          if cbConfigColumns.Items.indexof(SuggestedCustomColumnsName) = -1 then
          begin
            ColumnClassForConfig.Name := SuggestedCustomColumnsName;
            ColSet.DeleteColumnSet(Index);
            Colset.Insert(Index, ColumnClassForConfig);
            FillColumnsList;
            cbConfigColumns.ItemIndex := cbConfigColumns.Items.IndexOf(ColumnClassForConfig.Name);
            cbConfigColumnsChange(cbConfigColumns);
          end
          else
          begin
            msgError(rsMenuConfigureColumnsAlreadyExists);
          end;
        end;
      end;
    end;
  end;
end;

{ TfrmOptionsCustomColumns.btnDeleteConfigColumnsClick }
procedure TfrmOptionsCustomColumns.btnDeleteConfigColumnsClick(Sender: TObject);
begin
  if cbConfigColumns.ItemIndex = -1 then Exit;
  if (cbConfigColumns.Items.Count = 1) and (cmbFileSystem.ItemIndex = 0) then
    Exit;
  ColSet.DeleteColumnSet(PtrInt(cbConfigColumns.Items.Objects[cbConfigColumns.ItemIndex]));
  cmbFileSystemChange(cmbFileSystem);
end;

{ TfrmOptionsCustomColumns.UpdatePageInfoFromColumnClass }
// ***Important routine.
// Take the initial info from the ColumnClass and organize the form's components to reflect that.
procedure TfrmOptionsCustomColumns.UpdatePageInfoFromColumnClass;
var
  I: integer;
begin
  PreviewLeftPanel.ActiveColmSlave := ColumnClass;
  PreviewLeftPanel.isSlave := True;
  PreviewRightPanel.ActiveColmSlave := ColumnClass;
  PreviewRightPanel.isSlave := True;

  if ColumnClass.ColumnsCount > 0 then
  begin
    stgColumns.RowCount := ColumnClass.ColumnsCount + 1;

    for i := 0 to ColumnClass.ColumnsCount - 1 do
    begin
      stgColumns.Cells[1, i + 1] := ColumnClass.GetColumnTitle(i);
      stgColumns.Cells[2, i + 1] := IntToStr(ColumnClass.GetColumnWidth(i));
      stgColumns.Cells[3, i + 1] := ColumnClass.GetColumnAlignString(i);
      stgColumns.Cells[4, i + 1] := ColumnClass.GetColumnFuncString(i);
      stgColumns.Objects[6, i + 1] := ColumnClass.GetColumnPrm(i);
    end;
  end
  else
  begin
    stgColumns.RowCount := 1;
    AddNewField;
  end;

  PreviewLeftPanel.UpdateColumnsView;
  PreviewRightPanel.UpdateColumnsView;

  FUpdating := True;
  chkUseCustomView.Checked := ColumnClass.CustomView;
  chkUseCustomViewChange(chkUseCustomView);
  cbCursorBorder.Checked := ColumnClass.UseCursorBorder;
  SetColorInColorBox(cbCursorBorderColor, ColumnClass.CursorBorderColor);
  cbUseFrameCursor.Checked := ColumnClass.UseFrameCursor;
  FUpdating := False;

  // Localize StringGrid header
  stgColumns.Cells[0, 0] := rsSimpleWordColumnSingular;
  stgColumns.Cells[1, 0] := rsConfColCaption;
  stgColumns.Cells[2, 0] := rsConfColWidth;
  stgColumns.Cells[3, 0] := rsConfColAlign;
  stgColumns.Cells[4, 0] := rsConfColFieldCont;
  stgColumns.Cells[5, 0] := rsConfColMove;
  stgColumns.Cells[6, 0] := rsConfColDelete;

  LoadCustColumn(0);
end;

{ TfrmOptionsCustomColumns.UpdateColumnClass }
// ***Important routine. Convert the current form components into the current working "ColumnClass".
// ***It is not saved to file yet, but if we do, it will be that one!
procedure TfrmOptionsCustomColumns.UpdateColumnClass;
var
  i, indx: integer;
  Tit, FuncString: string;
  Wid: integer;
  Ali: TAlignment;
begin
  // Save fields
  ColumnClass.Clear;

  for i := 1 to stgColumns.RowCount - 1 do
  begin
    with stgColumns do
    begin
      Tit := Cells[1, i];
      Wid := StrToInt(Cells[2, i]);
      Ali := StrToAlign(Cells[3, i]);
      FuncString := Cells[4, i];
    end;
    indx := ColumnClass.Add(Tit, FuncString, Wid, Ali);
    if stgColumns.Objects[6, i] <> nil then
      ColumnClass.SetColumnPrm(Indx, TColPrm(stgColumns.Objects[6, i]));
  end;

  ColumnClass.FileSystem := cmbFileSystem.Text;
  ColumnClass.CustomView := chkUseCustomView.Checked;
  ColumnClass.UseCursorBorder := cbCursorBorder.Checked;
  ColumnClass.CursorBorderColor := cbCursorBorderColor.Selected;
  ColumnClass.UseFrameCursor := cbUseFrameCursor.Checked;
  ColumnClass.Name := cbConfigColumns.Items.Strings[cbConfigColumns.ItemIndex];

  if LastLoadedOptionSignature = ComputeCompleteOptionsSignature then
  begin
    cbConfigColumns.Enabled := True;
    cbConfigColumns.Hint := '';
    btnSaveConfigColumns.Enabled := False;
    btnRenameConfigColumns.Enabled := True;
    btnNewConfig.Enabled := True;
  end
  else
  begin
    cbConfigColumns.Enabled := False;
    cbConfigColumns.Hint := rsMenuConfigureColumnsSaveToChange;
    btnSaveConfigColumns.Enabled := True;
    btnRenameConfigColumns.Enabled := False;
    btnNewConfig.Enabled := False;
  end;

  PreviewLeftPanel.UpdateColumnsView;
  PreviewLeftPanel.Reload;
  PreviewRightPanel.UpdateColumnsView;
  PreviewRightPanel.Reload;
end;

{ TfrmOptionsCustomColumns.stgColumnsSelectEditor }
procedure TfrmOptionsCustomColumns.stgColumnsSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
begin
  // Hide '+' button in other columns than 4th (Field contents).
  if (aCol <> 4) and btnAdd.Visible then
    btnAdd.Hide;

  try
    FUpdating := True;
    case aCol of
      0: // Just the arrow pointing the "active" columns
      begin
        Editor := nil;
      end;

      2: // Width
      begin
        with updWidth do
        begin
          Left := (Sender as TStringGrid).CellRect(aCol, aRow).Left;
          Top := (Sender as TStringGrid).CellRect(aCol, aRow).Top;
          Height := (Sender as TStringGrid).RowHeights[aRow];
          Width := (Sender as TStringGrid).ColWidths[aCol];
          Value := StrToInt((Sender as TStringGrid).Cells[aCol, aRow]);
        end;
        Editor := updWidth;
      end;

      3: // Columns alignment
      begin
        with cbbAlign do
        begin
          Width := (Sender as TStringGrid).ColWidths[aCol];
          Left := (Sender as TStringGrid).CellRect(aCol, aRow).Left;
          Top := (Sender as TStringGrid).CellRect(aCol, aRow).Top;
          Height := (Sender as TStringGrid).RowHeights[aRow];
          ItemIndex := Items.IndexOf((Sender as TStringGrid).Cells[aCol, aRow]);
        end;
        Editor := cbbAlign;
      end;

      4: // Field contents
      begin
        with btnAdd do
        begin
          Width := 20;
          Left := (Sender as TStringGrid).CellRect(aCol, aRow).Right - Width;
          Top := (Sender as TStringGrid).CellRect(aCol, aRow).Top;
          Height := (Sender as TStringGrid).RowHeights[aRow];
          Tag := aRow;
          Show;
        end;

        with edtField do
        begin
          Width := (Sender as TStringGrid).ColWidths[aCol];
          Left := (Sender as TStringGrid).CellRect(aCol, aRow).Left;
          Top := (Sender as TStringGrid).CellRect(aCol, aRow).Top;
          Height := (Sender as TStringGrid).RowHeights[aRow];
          Text := (Sender as TStringGrid).Cells[aCol, aRow];
        end;
        Editor := edtField;
      end;

      5: // Move columns
      begin
        with updMove do
        begin
          Height := stgColumns.RowHeights[aRow];
          Width := stgColumns.ColWidths[aCol] - 2;
          Min := -((Sender as TStringGrid).RowCount - 1);
          Max := -1;
          Position := -aRow;
          Left := (Sender as TStringGrid).CellRect(aCol, aRow).Right - Width;
          Top := (Sender as TStringGrid).CellRect(aCol, aRow).Top;
        end;
        Editor := updMove;
      end;

      6: // Delete columns
      begin
        // Only show delete button if there is more than one column.
        if (stgColumns.RowCount - stgColumns.FixedRows) > 1 then
        begin
          with btnDel do
          begin
            Height := stgColumns.RowHeights[aRow];
            Width := stgColumns.ColWidths[aCol] - 2;
            Left := (Sender as TStringGrid).CellRect(aCol, aRow).Right - Width;
            Top := (Sender as TStringGrid).CellRect(aCol, aRow).Top;
          end;
          Editor := btnDel;
        end
        else
          Editor := nil;
      end;
    end;

  finally
    if Assigned(Editor) then
    begin
      Editor.Tag := aRow;
      Editor.Hint := IntToStr(aCol);
      if not stgColumns.EditorMode then
        FCellValue := stgColumns.Cells[aCol, aRow];
    end;
    FUpdating := False;
  end;
end;

{ TfrmOptionsCustomColumns.stgColumnsKeyDown }
procedure TfrmOptionsCustomColumns.stgColumnsKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_DOWN:
      if (stgColumns.Row = stgColumns.RowCount - 1) then
      begin
        AddNewField;
      end;
    VK_ESCAPE:
      if (stgColumns.EditorMode) then
      begin
        stgColumns.Cells[stgColumns.Col, stgColumns.Row] := FCellValue;
        stgColumns.EditorMode := False;
        Key := 0;
      end;
  end;
end;

{ TfrmOptionsCustomColumns.stgColumnsMouseDown }
procedure TfrmOptionsCustomColumns.stgColumnsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Col: integer = 0;
  Row: integer = 0;
begin
  if Y < stgColumns.GridHeight then
  begin
    // Clicked on a cell, allow editing.
    stgColumns.Options := stgColumns.Options + [goEditing];

    // Select clicked column in customize colors panel.
    stgColumns.MouseToCell(X, Y, Col, Row);
    LoadCustColumn(Row - stgColumns.FixedRows);
  end
  else
  begin
    // Clicked not on a cell, disable editing.
    stgColumns.Options := stgColumns.Options - [goEditing];

    if btnAdd.Visible then
      btnAdd.Hide;
  end;
end;

{ TfrmOptionsCustomColumns.stgColumnsMouseMove }
procedure TfrmOptionsCustomColumns.stgColumnsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  iCol: integer;
  StringGrid: THackStringGrid absolute Sender;
begin
  if (StringGrid.fGridState = gsColSizing) then
  begin
    if StringGrid.EditorMode then
      with StringGrid.Editor do
      begin
        iCol := StrToInt(Hint);
        Width := StringGrid.ColWidths[iCol];
        Left := StringGrid.CellRect(iCol, StringGrid.Row).Left;
      end;
    if btnAdd.Visible then
      btnAdd.Left := StringGrid.CellRect(4, StringGrid.Row).Right - btnAdd.Width;
  end;
end;

{ TfrmOptionsCustomColumns.CreateEditingControls }
procedure TfrmOptionsCustomColumns.CreateEditingControls;
begin
  // Editing controls are created with no parent-control.
  // TCustomGrid handles their visibility when they are assigned to Editor property.

  btnDel := TBitBtn.Create(Self);
  with btnDel do
  begin
    //   Glyph.Assign(btnCancel.Glyph);
    Caption := 'Delete';
    OnClick := @BitBtnDeleteFieldClick;
  end;

  cbbAlign := TComboBox.Create(Self);
  with cbbAlign do
  begin
    Style := csDropDownList;
    AddItem('<-', nil);
    AddItem('->', nil);
    AddItem('=', nil);
    OnSelect := @ComboBoxXSelect;
    OnKeyDown := @EditorKeyDown;
  end;

  edtField := TEdit.Create(Self);
  with edtField do
  begin
    OnExit := @EditExit;
    OnKeyDown := @EditorKeyDown;
  end;

  updMove := TUpDown.Create(Self);
  with updMove do
  begin
    OnChanging := @UpDownXChanging;
    OnClick := @UpDownXClick;
  end;

  updWidth := TSpinEdit.Create(Self);
  with updWidth do
  begin
    MinValue := 0;
    MaxValue := 1000;
    OnKeyDown := @EditorKeyDown;
    OnChange := @SpinEditChange;
    OnExit := @SpinEditExit;
  end;

  // Add button displayed in 'Field contents'.
  btnAdd := TButton.Create(Self);
  with btnAdd do
  begin
    Visible := False;
    Parent := stgColumns; // set Parent, because this control is shown manually in stgColumns
    Caption := '+';
    OnClick := @btnAddClick;
  end;
end;

{ TfrmOptionsCustomColumns.EditorKeyDown }
procedure TfrmOptionsCustomColumns.EditorKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        EditorSaveResult(Sender);
        stgColumns.EditorMode := False;
        Key := 0;
       end;
    VK_ESCAPE:
      begin
        stgColumns.EditorMode := False;
        stgColumns.Cells[stgColumns.Col, stgColumns.Row] := FCellValue;
        UpdateColumnClass;
        Key := 0;
      end;
  end;
end;

{ TfrmOptionsCustomColumns.AddNewField }
procedure TfrmOptionsCustomColumns.AddNewField;
begin
  stgColumns.RowCount := stgColumns.RowCount + 1;
  stgColumns.Cells[1, stgColumns.RowCount - 1] := EmptyStr;
  stgColumns.Cells[2, stgColumns.RowCount - 1] := '50';
  stgColumns.Cells[3, stgColumns.RowCount - 1] := '<-';
  stgColumns.Cells[4, stgColumns.RowCount - 1] := '';
  stgColumns.Objects[6, stgColumns.RowCount - 1] := TColPrm.Create;

  UpdateColumnClass;
end;

{ TfrmOptionsCustomColumns.miAddColumnClick }
procedure TfrmOptionsCustomColumns.miAddColumnClick(Sender: TObject);
begin
  AddNewField;
end;

{ TfrmOptionsCustomColumns.SpinEditExit }
procedure TfrmOptionsCustomColumns.SpinEditExit(Sender: TObject);
begin
  EditorSaveResult(Sender);
end;

{ TfrmOptionsCustomColumns.SpinEditChange }
procedure TfrmOptionsCustomColumns.SpinEditChange(Sender: TObject);
begin
  EditorSaveResult(Sender);
end;

{ TfrmOptionsCustomColumns.EditExit }
procedure TfrmOptionsCustomColumns.EditExit(Sender: TObject);
begin
  EditorSaveResult(Sender);
end;

{ TfrmOptionsCustomColumns.BitBtnDeleteFieldClick }
procedure TfrmOptionsCustomColumns.BitBtnDeleteFieldClick(Sender: TObject);
var
  RowNr: integer;
begin
  RowNr := (Sender as TBitBtn).Tag;

  // Free TColPrm object assigned to the row.
  if Assigned(stgColumns.Objects[6, RowNr]) then
  begin
    (stgColumns.Objects[6, RowNr] as TColPrm).Free;
    stgColumns.Objects[6, RowNr] := nil;
  end;

  stgColumns.DeleteColRow(False, RowNr);
  EditorSaveResult(Sender);

  if RowNr = stgColumns.RowCount then
    // The last row was deleted, load previous column.
    LoadCustColumn(RowNr - stgColumns.FixedRows - 1)
  else
    // Load next column (RowNr will point to it after deleting).
    LoadCustColumn(RowNr - stgColumns.FixedRows);
end;

{ TfrmOptionsCustomColumns.btnAddAddClick }
procedure TfrmOptionsCustomColumns.btnAddClick(Sender: TObject);
var
  Point: TPoint;
begin
  // Fill column fields menu
  FillContentFieldMenu(pmFields.Items, @MenuFieldsClick, cmbFileSystem.Text);
  // Show popup menu
  Point.x := (Sender as TButton).Left - 25;
  Point.y := (Sender as TButton).Top + (Sender as TButton).Height + 40;
  Point := ClientToScreen(Point);
  pmFields.PopUp(Point.X, Point.Y);
end;

{ TfrmOptionsCustomColumns.ComboBoxXSelect }
procedure TfrmOptionsCustomColumns.ComboBoxXSelect(Sender: TObject);
begin
  EditorSaveResult(Sender);
end;

{ TfrmOptionsCustomColumns.UpDownXClick }
procedure TfrmOptionsCustomColumns.UpDownXClick(Sender: TObject; Button: TUDBtnType);
begin
  stgColumns.ExchangeColRow(False, updMove.Tag, abs(updMove.Position));
  with updMove do
  begin
    Left := stgColumns.CellRect(5, abs(updMove.Position)).Right - Width;
    Top := stgColumns.CellRect(5, abs(updMove.Position)).Top;
  end;
  EditorSaveResult(Sender);
  LoadCustColumn(abs(updMove.Position) - 1);
end;

{ TfrmOptionsCustomColumns.UpDownXChanging }
procedure TfrmOptionsCustomColumns.UpDownXChanging(Sender: TObject; var AllowChange: boolean);
begin
  updMove.tag := abs(updMove.Position);
  EditorSaveResult(Sender);
end;

{ TfrmOptionsCustomColumns.stgSetSelectionAsHintToUser }
procedure TfrmOptionsCustomColumns.stgSetSelectionAsHintToUser;
var
  CellToSelect: TGridRect;
begin
  CellToSelect.Left := 1; // Column for the name.
  CellToSelect.Right := 1;
  CellToSelect.Top := IndexRaw + 1; // Actual column of the view. This will give a visual hint to current column edited.
  CellToSelect.Bottom := IndexRaw + 1;
  stgColumns.Options := stgColumns.Options + [goRangeSelect, goSelectionActive]; // So we can change current grid selection.
  stgColumns.Selection := CellToSelect;
  stgColumns.Options := stgColumns.Options - [goRangeSelect, goSelectionActive]; // To place it back like original author wanted.
  stgColumns.SetFocus;
end;

{ TfrmOptionsCustomColumns.stgColumnsEditingDone }
procedure TfrmOptionsCustomColumns.stgColumnsEditingDone(Sender: TObject);
begin
  EditorSaveResult(Sender);
end;

{ TfrmOptionsCustomColumns.MenuFieldsClick }
procedure TfrmOptionsCustomColumns.MenuFieldsClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
begin
  if Length(stgColumns.Cells[1, btnAdd.Tag]) = 0 then
  begin
    if MenuItem.Tag = 0 then
      stgColumns.Cells[1, btnAdd.Tag] := Copy(MenuItem.Caption, 1, Pos('(', MenuItem.Caption) - 3)
    else
      stgColumns.Cells[1, btnAdd.Tag] := MenuItem.Caption;
  end;
  case MenuItem.Tag of
    0:
    begin
      stgColumns.Cells[4, btnAdd.Tag] := stgColumns.Cells[4, btnAdd.Tag] + '[DC().' + MenuItem.Hint + '{}] ';
    end;
    1:
    begin
      stgColumns.Cells[4, btnAdd.Tag] := stgColumns.Cells[4, btnAdd.Tag] + '[Plugin(' + MenuItem.Parent.Caption + ').' + MenuItem.Caption + '{}] ';
    end;
    2:
    begin
      stgColumns.Cells[4, btnAdd.Tag] := stgColumns.Cells[4, btnAdd.Tag] + '[Plugin(' + MenuItem.Parent.Parent.Caption + ').' + MenuItem.Parent.Caption + '{' + MenuItem.Caption + '}] ';
    end;
    3:
    begin
      stgColumns.Cells[4, btnAdd.Tag] := stgColumns.Cells[4, btnAdd.Tag] + '[DC().' + MenuItem.Parent.Hint + '{' + MenuItem.Hint + '}] ';
    end;
  end;
  EditorSaveResult(Sender);
end;

{ TfrmOptionsCustomColumns.EditorSaveResult }
procedure TfrmOptionsCustomColumns.EditorSaveResult(Sender: TObject);
begin
  if not FUpdating then
  begin
    if Sender is TSpinEdit then
      stgColumns.Cells[2, (Sender as TSpinEdit).Tag] := IntToStr(updWidth.Value);
    if Sender is TComboBox then
      stgColumns.Cells[3, (Sender as TComboBox).Tag] := (Sender as TComboBox).Text;
    if Sender is TEdit then
      stgColumns.Cells[4, (Sender as TEdit).Tag] := (Sender as TEdit).Text;

    UpdateColumnClass;
  end;
end;

{ TfrmOptionsCustomColumns.CustomSomethingChanged }
procedure TfrmOptionsCustomColumns.CustomSomethingChanged(Sender: TObject);
begin
  if cbApplyChangeForAllColumns.Checked then
    btnAllForeColorClick(Sender)
  else
    EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.LoadCustColumn }
procedure TfrmOptionsCustomColumns.LoadCustColumn(const Index: integer);
var
  InnerUpdateStateToBeRestored: boolean;
  iRow: integer;
begin
  if (Index >= stgColumns.RowCount - 1) or (Index < 0) then
    exit;

  IndexRaw := Index;
  ColPrm := TColPrm(stgColumns.Objects[6, IndexRaw + 1]);

  InnerUpdateStateToBeRestored := FUpdating;
  FUpdating := True;

  for iRow := 1 to pred(stgColumns.RowCount) do
    stgColumns.Cells[0, iRow] := strutils.ifthen(iRow = (IndexRaw + 1), '--->', EmptyStr);

  lblCurrentColumn.Caption := ColumnClass.GetColumnTitle(IndexRaw);
  edtFont.Text := ColumnClass.GetColumnFontName(IndexRaw);
  sneFontSize.Value := ColumnClass.GetColumnFontSize(IndexRaw);
  SetColorInColorBox(cbForeColor, ColumnClass.GetColumnTextColor(IndexRaw));
  SetColorInColorBox(cbBackColor, ColumnClass.GetColumnBackground(IndexRaw));
  SetColorInColorBox(cbBackColor2, ColumnClass.GetColumnBackground2(IndexRaw));
  SetColorInColorBox(cbMarkColor, ColumnClass.GetColumnMarkColor(IndexRaw));
  SetColorInColorBox(cbCursorColor, ColumnClass.GetColumnCursorColor(IndexRaw));
  SetColorInColorBox(cbCursorText, ColumnClass.GetColumnCursorText(IndexRaw));
  SetColorInColorBox(cbInactiveCursorColor, ColumnClass.GetColumnInactiveCursorColor(IndexRaw));
  SetColorInColorBox(cbInactiveMarkColor, ColumnClass.GetColumnInactiveMarkColor(IndexRaw));
  cbAllowOverColor.Checked := ColumnClass.GetColumnOvercolor(IndexRaw);
  cbUseInvertedSelection.Checked := ColumnClass.GetColumnUseInvertedSelection(IndexRaw);
  cbUseInactiveSelColor.Checked := ColumnClass.GetColumnUseInactiveSelColor(IndexRaw);
  FUpdating := InnerUpdateStateToBeRestored;
end;

{ TfrmOptionsCustomColumns.chkUseCustomViewChange }
procedure TfrmOptionsCustomColumns.chkUseCustomViewChange(Sender: TObject);
begin
  pnlCommon.Visible:= chkUseCustomView.Checked;
  pnlCustomColumnsViewSettings.Visible := chkUseCustomView.Checked;
  btnGotoSetDefault.Visible := not chkUseCustomView.Checked;
  EditorSaveResult(nil);
  if chkUsecustomView.Checked then
  begin
    LoadCustColumn(0);

    cbCursorBorder.Checked:= gUseCursorBorder;
    cbCursorBorderChange(cbCursorBorder);
    SetColorInColorBox(cbCursorBorderColor, gCursorBorderColor);
    cbUseFrameCursor.Checked:= gUseFrameCursor;
    cbUseFrameCursorChange(cbUseFrameCursor);
  end;
end;

{ TfrmOptionsCustomColumns.cbCursorBorderChange }
procedure TfrmOptionsCustomColumns.cbCursorBorderChange(Sender: TObject);
begin
  cbCursorBorderColor.Enabled := cbCursorBorder.Checked and cbCursorBorder.Enabled;
  btnCursorBorderColor.Enabled := cbCursorBorderColor.Enabled;
  btnResetCursorBorder.Enabled:= cbCursorBorderColor.Enabled;
  if cbCursorBorder.Checked and cbCursorBorder.Enabled then
    cbCursorBorderColor.Font.Color := clDefault
  else
    cbCursorBorderColor.Font.Color := clInactiveCaption;
  EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.cbCursorBorderColorChange }
procedure TfrmOptionsCustomColumns.cbCursorBorderColorChange(Sender: TObject);
begin
  if Assigned(ColPrm) then
  begin
    EditorSaveResult(nil);
  end;
end;

{ TfrmOptionsCustomColumns.btnCursorBorderColorClick }
procedure TfrmOptionsCustomColumns.btnCursorBorderColorClick(Sender: TObject);
begin
  dlgcolor.Color := cbCursorBorderColor.Selected;
  if dlgcolor.Execute then
  begin
    SetColorInColorBox(cbCursorBorderColor, dlgcolor.Color);
    EditorSaveResult(nil);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetCursorBorderClick }
procedure TfrmOptionsCustomColumns.btnResetCursorBorderClick(Sender: TObject);
begin
  cbCursorBorder.Checked := gUseCursorBorder;
  SetColorInColorBox(cbCursorBorderColor, gCursorBorderColor);
  EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.cbUseFrameCursorChange }
procedure TfrmOptionsCustomColumns.cbUseFrameCursorChange(Sender: TObject);
begin
  btnResetFrameCursor.Enabled := cbUseFrameCursor.Checked;
  cbCursorBorder.Enabled := not cbUseFrameCursor.Checked;
  lblCursorText.Enabled := not cbUseFrameCursor.Checked;
  cbCursorText.Enabled := not cbUseFrameCursor.Checked;
  btnCursorText.Enabled := not cbUseFrameCursor.Checked;
  btnResetCursorText.Enabled := not cbUseFrameCursor.Checked;
  btnAllCursorText.Enabled := not cbUseFrameCursor.Checked and not cbApplyChangeForAllColumns.Checked;
  btnResetCursorBorder.Enabled := not cbUseFrameCursor.Checked;

  if not cbUseFrameCursor.Checked then
    cbCursorText.Font.Color := clDefault
  else
    cbCursorText.Font.Color := clInactiveCaption;
  cbCursorBorderChange(cbCursorBorder);

  EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.btnResetFrameCursorClick }
procedure TfrmOptionsCustomColumns.btnResetFrameCursorClick(Sender: TObject);
begin
  cbUseFrameCursor.Checked := gUseFrameCursor;
  EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.btnPrevClick }
procedure TfrmOptionsCustomColumns.btnPrevClick(Sender: TObject);
begin
  if IndexRaw > 0 then
    LoadCustColumn(IndexRaw - 1)
  else
    LoadCustColumn(stgColumns.RowCount - 2);
  stgSetSelectionAsHintToUser;
end;

{ TfrmOptionsCustomColumns.btnNextClick }
procedure TfrmOptionsCustomColumns.btnNextClick(Sender: TObject);
begin
  if IndexRaw < (stgColumns.RowCount - 2) then
    LoadCustColumn(IndexRaw + 1)
  else
    LoadCustColumn(0);
  stgSetSelectionAsHintToUser;
end;

{ TfrmOptionsCustomColumns.cbApplyChangeForAllColumnsChange }
procedure TfrmOptionsCustomColumns.cbApplyChangeForAllColumnsChange(Sender: TObject);
begin
  if cbApplyChangeForAllColumns.Checked then
  begin
    btnAllBackColor.Enabled := False;
    btnAllBackColor2.Enabled := False;
    btnAllCursorColor.Enabled := False;
    btnAllCursorText.Enabled := False;
    btnAllFont.Enabled := False;
    btnAllInactiveCursorColor.Enabled := False;
    btnAllInactiveMarkColor.Enabled := False;
    btnAllMarkColor.Enabled := False;
    btnAllForeColor.Enabled := False;
    btnAllAllowOverColor.Enabled := False;
    btnAllUseInvertedSelection.Enabled := False;
    btnAllUseInactiveSelColor.Enabled := False;
  end
  else
  begin
    btnAllBackColor.Enabled := True;
    btnAllBackColor2.Enabled := True;
    btnAllCursorColor.Enabled := True;
    btnAllCursorText.Enabled := True;
    btnAllFont.Enabled := True;
    btnAllMarkColor.Enabled := True;
    btnAllForeColor.Enabled := True;
    btnAllAllowOverColor.Enabled := True;
    btnAllUseInvertedSelection.Enabled := True;
    btnAllUseInactiveSelColor.Enabled := True;
    btnAllInactiveCursorColor.Enabled := cbUseInactiveSelColor.Checked;
    btnAllInactiveMarkColor.Enabled := cbUseInactiveSelColor.Checked;
  end;
end;

{ TfrmOptionsCustomColumns.btnFontClick }
procedure TfrmOptionsCustomColumns.btnFontClick(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    with TColPrm(stgColumns.Objects[6, IndexRaw + 1]) do
    begin
      dlgfont.Font.Name := FontName;
      dlgfont.Font.Size := FontSize;
      dlgfont.Font.Style := FontStyle;

      if dlgfont.Execute then
      begin
        edtFont.Text := dlgfont.Font.Name;
        sneFontSize.Value := dlgfont.Font.Size;
        FontName := dlgfont.Font.Name;
        FontSize := dlgfont.Font.Size;
        FontStyle := dlgfont.Font.Style;
        CustomSomethingChanged(Sender);
      end;
    end;
  end;
end;

{ TfrmOptionsCustomColumns.sneFontSizeChange }
procedure TfrmOptionsCustomColumns.sneFontSizeChange(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    TColPrm(stgColumns.Objects[6, IndexRaw + 1]).FontSize := sneFontSize.Value;
    CustomSomethingChanged(Sender);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetFontClick }
procedure TfrmOptionsCustomColumns.btnResetFontClick(Sender: TObject);
begin
  TColPrm(stgColumns.Objects[6, IndexRaw + 1]).FontName := gFonts[dcfMain].Name;
  TColPrm(stgColumns.Objects[6, IndexRaw + 1]).FontSize := gFonts[dcfMain].Size;
  TColPrm(stgColumns.Objects[6, IndexRaw + 1]).FontStyle := gFonts[dcfMain].Style;
  edtFont.Text := gFonts[dcfMain].Name;
  sneFontSize.Value := gFonts[dcfMain].Size;
  CustomSomethingChanged(Sender);
end;

{ TfrmOptionsCustomColumns.btnAllForeColorClick }
procedure TfrmOptionsCustomColumns.btnAllForeColorClick(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to pred(stgColumns.RowCount) do
    case TComponent(Sender).tag of
      0:
      begin
        TColPrm(stgColumns.Objects[6, i]).FontName := TColPrm(stgColumns.Objects[6, IndexRaw + 1]).FontName;
        TColPrm(stgColumns.Objects[6, i]).FontSize := TColPrm(stgColumns.Objects[6, IndexRaw + 1]).FontSize;
        TColPrm(stgColumns.Objects[6, i]).FontStyle := TColPrm(stgColumns.Objects[6, IndexRaw + 1]).FontStyle;
      end;
      1: TColPrm(stgColumns.Objects[6, i]).TextColor := cbForeColor.Selected;
      2: TColPrm(stgColumns.Objects[6, i]).Background := cbBackColor.Selected;
      3: TColPrm(stgColumns.Objects[6, i]).Background2 := cbBackColor2.Selected;
      4: TColPrm(stgColumns.Objects[6, i]).MarkColor := cbMarkColor.Selected;
      5: TColPrm(stgColumns.Objects[6, i]).CursorColor := cbCursorColor.Selected;
      6: TColPrm(stgColumns.Objects[6, i]).CursorText := cbCursorText.Selected;
      7: TColPrm(stgColumns.Objects[6, i]).InactiveCursorColor := cbInactiveCursorColor.Selected;
      8: TColPrm(stgColumns.Objects[6, i]).InactiveMarkColor := cbInactiveMarkColor.Selected;
      9: TColPrm(stgColumns.Objects[6, i]).UseInvertedSelection := cbUseInvertedSelection.Checked;
      10: TColPrm(stgColumns.Objects[6, i]).UseInactiveSelColor := cbUseInactiveSelColor.Checked;
      11: TColPrm(stgColumns.Objects[6, i]).Overcolor := cbAllowOverColor.Checked;
    end;
  UpdateColumnClass;
end;

{ TfrmOptionsCustomColumns.cbForeColorChange }
procedure TfrmOptionsCustomColumns.cbForeColorChange(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    ColPrm.TextColor := (Sender as TColorBox).Selected;
    CustomSomethingChanged(Sender);
  end;
end;

{ TfrmOptionsCustomColumns.btnForeColorClick }
procedure TfrmOptionsCustomColumns.btnForeColorClick(Sender: TObject);
begin
  dlgcolor.Color := cbForeColor.Selected;
  if dlgcolor.Execute then
  begin
    SetColorInColorBox(cbForeColor, dlgcolor.Color);

    TColPrm(stgColumns.Objects[6, IndexRaw + 1]).TextColor := cbForeColor.Selected;
    EditorSaveResult(nil);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetForeColorClick }
procedure TfrmOptionsCustomColumns.btnResetForeColorClick(Sender: TObject);
begin
  TColPrm(stgColumns.Objects[6, IndexRaw + 1]).TextColor := gForeColor;
  SetColorInColorBox(cbForeColor, gForeColor);
  EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.cbBackColorChange }
procedure TfrmOptionsCustomColumns.cbBackColorChange(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    ColPrm.Background := (Sender as TColorBox).Selected;
    CustomSomethingChanged(Sender);
  end;
end;

{ TfrmOptionsCustomColumns.btnBackColorClick }
procedure TfrmOptionsCustomColumns.btnBackColorClick(Sender: TObject);
begin
  dlgcolor.Color := cbBackColor.Selected;
  if dlgcolor.Execute then
  begin
    SetColorInColorBox(cbBackColor, dlgcolor.Color);
    TColPrm(stgColumns.Objects[6, IndexRaw + 1]).Background := cbBackColor.Selected;
    EditorSaveResult(nil);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetBackColorClick }
procedure TfrmOptionsCustomColumns.btnResetBackColorClick(Sender: TObject);
begin
  TColPrm(stgColumns.Objects[6, IndexRaw + 1]).Background := gBackColor;
  SetColorInColorBox(cbBackColor, gBackColor);
  EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.cbBackColor2Change }
procedure TfrmOptionsCustomColumns.cbBackColor2Change(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    ColPrm.Background2 := (Sender as TColorBox).Selected;
    CustomSomethingChanged(Sender);
  end;
end;

{ TfrmOptionsCustomColumns.btnBackColor2Click }
procedure TfrmOptionsCustomColumns.btnBackColor2Click(Sender: TObject);
begin
  dlgcolor.Color := cbBackColor2.Selected;
  if dlgcolor.Execute then
  begin
    SetColorInColorBox(cbBackColor2, dlgcolor.Color);
    TColPrm(stgColumns.Objects[6, IndexRaw + 1]).Background2 := cbBackColor2.Selected;
    EditorSaveResult(nil);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetBackColor2Click }
procedure TfrmOptionsCustomColumns.btnResetBackColor2Click(Sender: TObject);
begin
  TColPrm(stgColumns.Objects[6, IndexRaw + 1]).Background2 := gBackColor2;
  SetColorInColorBox(cbBackColor2, gBackColor2);
  EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.cbMarkColorChange }
procedure TfrmOptionsCustomColumns.cbMarkColorChange(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    ColPrm.MarkColor := (Sender as TColorBox).Selected;
    CustomSomethingChanged(Sender);
  end;
end;

{ TfrmOptionsCustomColumns.btnMarkColorClick }
procedure TfrmOptionsCustomColumns.btnMarkColorClick(Sender: TObject);
begin
  dlgcolor.Color := cbMarkColor.Selected;
  if dlgcolor.Execute then
  begin
    SetColorInColorBox(cbMarkColor, dlgcolor.Color);
    TColPrm(stgColumns.Objects[6, IndexRaw + 1]).MarkColor := cbMarkColor.Selected;
    EditorSaveResult(nil);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetMarkColorClick }
procedure TfrmOptionsCustomColumns.btnResetMarkColorClick(Sender: TObject);
begin
  TColPrm(stgColumns.Objects[6, IndexRaw + 1]).MarkColor := gMarkColor;
  SetColorInColorBox(cbMarkColor, gMarkColor);
  EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.cbCursorColorChange }
procedure TfrmOptionsCustomColumns.cbCursorColorChange(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    ColPrm.CursorColor := (Sender as TColorBox).Selected;
    CustomSomethingChanged(Sender);
  end;
end;

{ TfrmOptionsCustomColumns.btnCursorColorClick }
procedure TfrmOptionsCustomColumns.btnCursorColorClick(Sender: TObject);
begin
  dlgcolor.Color := cbCursorColor.Selected;
  if dlgcolor.Execute then
  begin
    SetColorInColorBox(cbCursorColor, dlgcolor.Color);
    TColPrm(stgColumns.Objects[6, IndexRaw + 1]).CursorColor := cbCursorColor.Selected;
    EditorSaveResult(nil);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetCursorColorClick }
procedure TfrmOptionsCustomColumns.btnResetCursorColorClick(Sender: TObject);
begin
  TColPrm(stgColumns.Objects[6, IndexRaw + 1]).CursorColor := gCursorColor;
  SetColorInColorBox(cbCursorColor, gCursorColor);
  EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.cbCursorTextChange }
procedure TfrmOptionsCustomColumns.cbCursorTextChange(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    ColPrm.CursorText := (Sender as TColorBox).Selected;
    CustomSomethingChanged(Sender);
  end;
end;

{ TfrmOptionsCustomColumns.btnCursorTextClick }
procedure TfrmOptionsCustomColumns.btnCursorTextClick(Sender: TObject);
begin
  dlgcolor.Color := cbCursorText.Selected;
  if dlgcolor.Execute then
  begin
    SetColorInColorBox(cbCursorText, dlgcolor.Color);
    TColPrm(stgColumns.Objects[6, IndexRaw + 1]).CursorText := cbCursorText.Selected;
    EditorSaveResult(nil);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetCursorTextClick }
procedure TfrmOptionsCustomColumns.btnResetCursorTextClick(Sender: TObject);
begin
  TColPrm(stgColumns.Objects[6, IndexRaw + 1]).CursorText := gCursorText;
  SetColorInColorBox(cbCursorText, gCursorText);
  EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.cbInactiveCursorColorChange }
procedure TfrmOptionsCustomColumns.cbInactiveCursorColorChange(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    ColPrm.InactiveCursorColor := (Sender as TColorBox).Selected;
    CustomSomethingChanged(Sender);
  end;
end;

{ TfrmOptionsCustomColumns.btnInactiveCursorColorClick }
procedure TfrmOptionsCustomColumns.btnInactiveCursorColorClick(Sender: TObject);
begin
  dlgcolor.Color := cbInactiveCursorColor.Selected;
  if dlgcolor.Execute then
  begin
    SetColorInColorBox(cbInactiveCursorColor, dlgcolor.Color);
    TColPrm(stgColumns.Objects[6, IndexRaw + 1]).InactiveCursorColor := cbInactiveCursorColor.Selected;
    EditorSaveResult(nil);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetInactiveCursorColorClick }
procedure TfrmOptionsCustomColumns.btnResetInactiveCursorColorClick(Sender: TObject);
begin
  TColPrm(stgColumns.Objects[6, IndexRaw + 1]).InactiveCursorColor := gInactiveCursorColor;
  SetColorInColorBox(cbInactiveCursorColor, gInactiveCursorColor);
  EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.cbInactiveMarkColorChange }
procedure TfrmOptionsCustomColumns.cbInactiveMarkColorChange(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    ColPrm.InactiveMarkColor := (Sender as TColorBox).Selected;
    CustomSomethingChanged(Sender);
  end;
end;

{ TfrmOptionsCustomColumns.btnInactiveMarkColorClick }
procedure TfrmOptionsCustomColumns.btnInactiveMarkColorClick(Sender: TObject);
begin
  dlgcolor.Color := cbInactiveMarkColor.Selected;
  if dlgcolor.Execute then
  begin
    SetColorInColorBox(cbInactiveMarkColor, dlgcolor.Color);
    TColPrm(stgColumns.Objects[6, IndexRaw + 1]).InactiveMarkColor := cbInactiveMarkColor.Selected;
    EditorSaveResult(nil);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetInactiveMarkColorClick }
procedure TfrmOptionsCustomColumns.btnResetInactiveMarkColorClick(Sender: TObject);
begin
  TColPrm(stgColumns.Objects[6, IndexRaw + 1]).InactiveMarkColor := gInactiveMarkColor;
  SetColorInColorBox(cbInactiveMarkColor, gInactiveMarkColor);
  EditorSaveResult(nil);
end;

{ TfrmOptionsCustomColumns.cbUseInvertedSelectionChange }
procedure TfrmOptionsCustomColumns.cbUseInvertedSelectionChange(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    TColPrm(stgColumns.Objects[6, IndexRaw + 1]).UseInvertedSelection := cbUseInvertedSelection.Checked;
    CustomSomethingChanged(Sender);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetUseInvertedSelectionClick }
procedure TfrmOptionsCustomColumns.btnResetUseInvertedSelectionClick(Sender: TObject);
begin
  cbUseInvertedSelection.Checked := gUseInvertedSelection;
  cbUseInvertedSelectionChange(cbUseInvertedSelection);
end;

{ TfrmOptionsCustomColumns.cbUseInactiveSelColorChange }
procedure TfrmOptionsCustomColumns.cbUseInactiveSelColorChange(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    lblInactiveCursorColor.Enabled := cbUseInactiveSelColor.Checked;
    cbInactiveCursorColor.Enabled := cbUseInactiveSelColor.Checked;
    btnInactiveCursorColor.Enabled := cbUseInactiveSelColor.Checked;
    btnResetInactiveCursorColor.Enabled := cbUseInactiveSelColor.Checked;
    btnAllInactiveCursorColor.Enabled := cbUseInactiveSelColor.Checked;
    lblInactiveMarkColor.Enabled := cbUseInactiveSelColor.Checked;
    cbInactiveMarkColor.Enabled := cbUseInactiveSelColor.Checked;
    btnInactiveMarkColor.Enabled := cbUseInactiveSelColor.Checked;
    btnResetInactiveMarkColor.Enabled := cbUseInactiveSelColor.Checked;
    btnAllInactiveMarkColor.Enabled := cbUseInactiveSelColor.Checked;
    TColPrm(stgColumns.Objects[6, IndexRaw + 1]).UseInactiveSelColor := cbUseInactiveSelColor.Checked;
    CustomSomethingChanged(Sender);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetUseInactiveSelColorClick }
procedure TfrmOptionsCustomColumns.btnResetUseInactiveSelColorClick(Sender: TObject);
begin
  cbUseInactiveSelColor.Checked := gUSeInactiveSelColor;
  cbUseInactiveSelColorChange(cbUseInactiveSelColor);
end;

{ TfrmOptionsCustomColumns.cbAllowOvercolorChange }
procedure TfrmOptionsCustomColumns.cbAllowOvercolorChange(Sender: TObject);
begin
  if Assigned(ColPrm) and (not FUpdating) then
  begin
    TColPrm(stgColumns.Objects[6, IndexRaw + 1]).Overcolor := cbAllowOverColor.Checked;
    CustomSomethingChanged(Sender);
  end;
end;

{ TfrmOptionsCustomColumns.btnResetAllowOverColorClick }
procedure TfrmOptionsCustomColumns.btnResetAllowOverColorClick(Sender: TObject);
begin
  cbAllowOverColor.Checked := gAllowOverColor;
  cbAllowOvercolorChange(cbAllowOverColor);
end;

{ TfrmOptionsCustomColumns.pnlLeftEnter }
procedure TfrmOptionsCustomColumns.pnlLeftEnter(Sender: TObject);
begin
  PreviewRightPanel.JustForColorPreviewSetActiveState(False);
  PreviewLeftPanel.JustForColorPreviewSetActiveState(True);
end;

{ TfrmOptionsCustomColumns.pnlRightEnter }
procedure TfrmOptionsCustomColumns.pnlRightEnter(Sender: TObject);
begin
  PreviewLeftPanel.JustForColorPreviewSetActiveState(False);
  PreviewRightPanel.JustForColorPreviewSetActiveState(True);
end;

{ TfrmOptionsCustomColumns.OnColumnResized }
procedure TfrmOptionsCustomColumns.OnColumnResized(Sender: TObject; ColumnIndex: integer; ColumnNewsize: integer);
begin
  if ColumnIndex < pred(stgColumns.RowCount) then
  begin
    stgColumns.Cells[2, 1 + ColumnIndex] := IntToStr(ColumnNewSize);
    EditorSaveResult(Sender); //To like everywhere here, but it's not absolutely necessary...
  end;
end;

end.

