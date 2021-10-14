{
   Double Commander
   -------------------------------------------------------------------------
   Tooltips options page

   Copyright (C) 2011-2021 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsToolTips;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, StdCtrls, Buttons,
  Menus, ExtCtrls, Dialogs, fOptionsFrame, uInfoToolTip;

type

  { TfrmOptionsToolTips }

  TfrmOptionsToolTips = class(TOptionsEditor)
    pnlGeneralToolTipsOptions: TPanel;
    pnlShowTooltip: TPanel;
    chkShowToolTip: TCheckBox;
    pnlToolTipsListbox: TPanel;
    lblToolTipsListBox: TLabel;
    lsbCustomFields: TListBox;
    splToolTips: TSplitter;
    pnlConfigurationToolTips: TPanel;
    pnlTooltipButtons: TPanel;
    btnApplyToolTipsFileType: TBitBtn;
    btnAddToolTipsFileType: TBitBtn;
    btnCopyToolTipsFileType: TBitBtn;
    btnRenameToolTipsFileType: TBitBtn;
    btnDeleteToolTipsFileType: TBitBtn;
    btnTooltipOther: TBitBtn;
    pnlActualToolTipsConfiguration: TPanel;
    bvlToolTips1: TDividerBevel;
    lblFieldsMask: TLabel;
    edtFieldsMask: TEdit;
    btnFieldsSearchTemplate: TBitBtn;
    lblFieldsList: TLabel;
    memFieldsList: TMemo;
    btnFieldsList: TButton;
    pmFields: TPopupMenu;
    bvlToolTips2: TDividerBevel;
    lblTooltipShowingMode: TLabel;
    cbTooltipShowingMode: TComboBox;
    lblTooltipHidingDelay: TLabel;
    cbToolTipHideTimeOut: TComboBox;
    pmTooltipOther: TPopupMenu;
    miToolTipsFileTypeDiscardModification: TMenuItem;
    miSeparator1: TMenuItem;
    miToolTipsFileTypeSortFileType: TMenuItem;
    miSeparator2: TMenuItem;
    miToolTipsFileTypeExport: TMenuItem;
    miToolTipsFileTypeImport: TMenuItem;
    OpenTooltipFileTypeDialog: TOpenDialog;
    SaveTooltipFileTypeDialog: TSaveDialog;
    procedure FillListBoxWithToolTipsList;
    procedure SetActiveButtonsBasedOnToolTipsQuantity;
    procedure LoadMemoWithThisHint(sHint: string);
    procedure LoadThisHintWithThisMemo(var sHint: string);
    procedure ActualSaveCurrentToolTips;
    procedure edtAnyChange({%H-}Sender: TObject);
    procedure SetConfigurationState(bConfigurationSaved: boolean);
    procedure chkShowToolTipChange(Sender: TObject);
    procedure lsbCustomFieldsSelectionChange({%H-}Sender: TObject; {%H-}User: boolean);
    procedure lsbCustomFieldsDragOver({%H-}Sender, {%H-}Source: TObject; {%H-}X, {%H-}Y: integer; {%H-}State: TDragState; var Accept: boolean);
    procedure lsbCustomFieldsDragDrop({%H-}Sender, {%H-}Source: TObject; {%H-}X, Y: integer);
    procedure btnApplyToolTipsFileTypeClick({%H-}Sender: TObject);
    procedure btnAddToolTipsFileTypeClick({%H-}Sender: TObject);
    procedure btnCopyToolTipsFileTypeClick({%H-}Sender: TObject);
    procedure btnRenameToolTipsFileTypeClick({%H-}Sender: TObject);
    procedure btnDeleteToolTipsFileTypeClick({%H-}Sender: TObject);
    procedure btnTooltipOtherClick({%H-}Sender: TObject);
    procedure miToolTipsFileTypeDiscardModificationClick({%H-}Sender: TObject);
    procedure miToolTipsFileTypeSortFileTypeClick({%H-}Sender: TObject);
    procedure miToolTipsFileTypeExportClick({%H-}Sender: TObject);
    procedure miToolTipsFileTypeImportClick({%H-}Sender: TObject);
    procedure miPluginClick(Sender: TObject);
    procedure btnFieldsListClick({%H-}Sender: TObject);
    procedure btnFieldsSearchTemplateClick({%H-}Sender: TObject);
    function isUniqueFileType(paramNewName: string): boolean;
    procedure ClearData;
  private
    bCurrentlyLoadingSettings, bCurrentlyFilling: boolean;
    FFileInfoToolTipTemp: TFileInfoToolTip;
  protected
    procedure Init; override;
    procedure Load; override;
    procedure Done; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
    function IsSignatureComputedFromAllWindowComponents: boolean; override;
    function ExtraOptionsSignature(CurrentSignature: dword): dword; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  StrUtils, LCLProc,

  //DC
  DCStrUtils, uShowMsg, uComponentsSignature, fMaskInputDlg, uLng, uGlobs,
  uSearchTemplate, uFileFunctions;

const
  CONFIG_NOTSAVED = False;
  CONFIG_SAVED = True;

var
  iLastDisplayedIndex: integer = -1;

{ TfrmOptionsToolTips }

{ TfrmOptionsToolTips.Init }
procedure TfrmOptionsToolTips.Init;
begin
  FFileInfoToolTipTemp := TFileInfoToolTip.Create;
  bCurrentlyLoadingSettings := True;
  bCurrentlyFilling := True;
  ParseLineToList(rsToolTipModeList, cbTooltipShowingMode.Items);
  ParseLineToList(rsToolTipHideTimeOutList, cbToolTipHideTimeOut.Items);
  OpenTooltipFileTypeDialog.Filter := ParseLineToFileFilter([rsFilterDCToolTipFiles, '*.tooltip', rsFilterAnyFiles, '*.*']);
  SaveTooltipFileTypeDialog.Filter := OpenTooltipFileTypeDialog.Filter;
end;

{ TfrmOptionsToolTips.Load }
procedure TfrmOptionsToolTips.Load;
begin
  bCurrentlyLoadingSettings := True;
  try
    chkShowToolTip.Checked := gShowToolTip;
    cbTooltipShowingMode.ItemIndex := integer(gShowToolTipMode);
    cbToolTipHideTimeOut.ItemIndex := integer(gToolTipHideTimeOut);
    FFileInfoToolTipTemp.Assign(gFileInfoToolTip);
    FillListBoxWithToolTipsList;
  finally
    bCurrentlyLoadingSettings := False;
  end;
end;

{ TfrmOptionsToolTips.Done }
procedure TfrmOptionsToolTips.Done;
begin
  if lsbCustomFields.ItemIndex <> -1 then
    if lsbCustomFields.ItemIndex < FFileInfoToolTipTemp.HintItemList.Count then
      iLastDisplayedIndex := lsbCustomFields.ItemIndex;

  FreeThenNil(FFileInfoToolTipTemp);
end;

{ TfrmOptionsToolTips.Save }
function TfrmOptionsToolTips.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
  if not lsbCustomFields.Enabled then
    ActualSaveCurrentToolTips;
  gShowToolTip := chkShowToolTip.Checked;
  gShowToolTipMode := TToolTipMode(cbTooltipShowingMode.ItemIndex);
  gToolTipHideTimeOut := TToolTipHideTimeOut(cbToolTipHideTimeOut.ItemIndex);
  gFileInfoToolTip.Assign(FFileInfoToolTipTemp);
  SetConfigurationState(CONFIG_SAVED);
  LastLoadedOptionSignature := ComputeCompleteOptionsSignature;
end;

{ TfrmOptionsToolTips.GetIconIndex }
class function TfrmOptionsToolTips.GetIconIndex: integer;
begin
  Result := 19;
end;

{ TfrmOptionsToolTips.GetTitle }
class function TfrmOptionsToolTips.GetTitle: string;
begin
  Result := rsOptionsEditorTooltips;
end;

{ TfrmOptionsToolTips.IsSignatureComputedFromAllWindowComponents }
function TfrmOptionsToolTips.IsSignatureComputedFromAllWindowComponents: boolean;
begin
  Result := False;
end;

{ TfrmOptionsToolTips.ExtraOptionsSignature }
function TfrmOptionsToolTips.ExtraOptionsSignature(CurrentSignature: dword): dword;
begin
  if not lsbCustomFields.Enabled then //If currently our Listbox is disabled, it's because we did at least one modification...
    Result := (LastLoadedOptionSignature xor $01) //...so let's make sure the reported signature for the whole thing is affected.
  else
  begin
    CurrentSignature := ComputeSignatureSingleComponent(chkShowToolTip, CurrentSignature);
    CurrentSignature := ComputeSignatureSingleComponent(cbTooltipShowingMode, CurrentSignature);
    CurrentSignature := ComputeSignatureSingleComponent(cbToolTipHideTimeOut, CurrentSignature);
    Result := FFileInfoToolTipTemp.ComputeSignature(CurrentSignature);
  end;
end;

{ TfrmOptionsToolTips.FillListBoxWithToolTipsList }
procedure TfrmOptionsToolTips.FillListBoxWithToolTipsList;
var
  I, iRememberIndex: integer;
begin
  bCurrentlyFilling := True;
  try
    iRememberIndex := lsbCustomFields.ItemIndex;
    lsbCustomFields.Clear;
    for I := 0 to pred(FFileInfoToolTipTemp.HintItemList.Count) do
      lsbCustomFields.Items.Add(FFileInfoToolTipTemp.HintItemList[I].Name);
    if lsbCustomFields.Items.Count > 0 then
    begin
      if (iRememberIndex <> -1) and (iRememberIndex < lsbCustomFields.Items.Count) then
        lsbCustomFields.ItemIndex := iRememberIndex
      else
      if (iLastDisplayedIndex <> -1) and (iLastDisplayedIndex < lsbCustomFields.Items.Count) then
        lsbCustomFields.ItemIndex := iLastDisplayedIndex
      else
        lsbCustomFields.ItemIndex := 0;
    end;
    SetActiveButtonsBasedOnToolTipsQuantity;
    btnApplyToolTipsFileType.Enabled := False;
    lsbCustomFieldsSelectionChange(lsbCustomFields, False);
  finally
    bCurrentlyFilling := False;
  end;
end;

{ TfrmOptionsToolTips.SetActiveButtonsBasedOnToolTipsQuantity }
procedure TfrmOptionsToolTips.SetActiveButtonsBasedOnToolTipsQuantity;
begin
  btnAddToolTipsFileType.Enabled := lsbCustomFields.Enabled;

  btnCopyToolTipsFileType.Enabled := ((lsbCustomFields.Items.Count > 0) and (lsbCustomFields.Enabled));
  btnRenameToolTipsFileType.Enabled := btnCopyToolTipsFileType.Enabled;
  btnDeleteToolTipsFileType.Enabled := btnCopyToolTipsFileType.Enabled;

  miToolTipsFileTypeSortFileType.Enabled := ((lsbCustomFields.Items.Count > 1) and (lsbCustomFields.Enabled));
  miToolTipsFileTypeExport.Enabled := btnCopyToolTipsFileType.Enabled;
end;

{ TfrmOptionsToolTips.LoadMemoWithThisHint }
//To be backward compatible with past versions and existing config, let's keep the "/n" separator for each line.
//[Plugin(&lt;Exif&gt;).Width{}]\nGenre:[Plugin(audioinfo).Genre{}]
procedure TfrmOptionsToolTips.LoadMemoWithThisHint(sHint: string);
var
  iStartingPoint, iPosDelimiter: integer;
begin
  memFieldsList.Clear;
  iStartingPoint := 1;
  repeat
    iPosDelimiter := PosEx('\n', LowerCase(sHint), iStartingPoint);
    if iPosDelimiter <> 0 then
    begin
      memFieldsList.Lines.Add(copy(sHint, iStartingPoint, (iPosDelimiter - iStartingPoint)));
      iStartingPoint := iPosDelimiter + 2;
    end;
  until iPosDelimiter = 0;
  if iStartingPoint < length(sHint) then
    memFieldsList.Lines.Add(RightStr(sHint, succ(length(sHint) - iStartingPoint)));
  memFieldsList.SelStart := 0;
end;

{ TfrmOptionsToolTips.LoadThisHintWithThisMemo }
procedure TfrmOptionsToolTips.LoadThisHintWithThisMemo(var sHint: string);
var
  iIndexLine: integer;
begin
  sHint := '';
  for iIndexLine := 0 to pred(memFieldsList.Lines.Count) do
    sHint := sHint + memFieldsList.Lines.Strings[iIndexLine] + IfThen(iIndexLine < pred(memFieldsList.Lines.Count), '\n', '');
end;

{ TfrmOptionsToolTips.ActualSaveCurrentToolTips }
procedure TfrmOptionsToolTips.ActualSaveCurrentToolTips;
begin
  if lsbCustomFields.ItemIndex <> -1 then
  begin
    FFileInfoToolTipTemp.HintItemList[lsbCustomFields.ItemIndex].Name := lsbCustomFields.Items.Strings[lsbCustomFields.ItemIndex];
    FFileInfoToolTipTemp.HintItemList[lsbCustomFields.ItemIndex].Mask := edtFieldsMask.Text;
    LoadThisHintWithThisMemo(FFileInfoToolTipTemp.HintItemList[lsbCustomFields.ItemIndex].Hint);
  end;
end;

{ TfrmOptionsToolTips.edtAnyChange }
procedure TfrmOptionsToolTips.edtAnyChange(Sender: TObject);
begin
  if not bCurrentlyLoadingSettings then
    if lsbCustomFields.Enabled then
      SetConfigurationState(CONFIG_NOTSAVED);
end;

{ TfrmOptionsToolTips.SetConfigurationState }
procedure TfrmOptionsToolTips.SetConfigurationState(bConfigurationSaved: boolean);
begin
  if lsbCustomFields.Enabled <> bConfigurationSaved then
  begin
    chkShowToolTip.Enabled := bConfigurationSaved;
    btnApplyToolTipsFileType.Enabled := not bConfigurationSaved;
    lsbCustomFields.Enabled := bConfigurationSaved;
    btnAddToolTipsFileType.Enabled := bConfigurationSaved;
    btnCopyToolTipsFileType.Enabled := bConfigurationSaved;
    btnRenameToolTipsFileType.Enabled := bConfigurationSaved;
    btnDeleteToolTipsFileType.Enabled := bConfigurationSaved;
    miToolTipsFileTypeDiscardModification.Enabled := not bConfigurationSaved;
    miToolTipsFileTypeSortFileType.Enabled := bConfigurationSaved;
    miToolTipsFileTypeExport.Enabled := bConfigurationSaved;
    miToolTipsFileTypeImport.Enabled := bConfigurationSaved;
    lsbCustomFields.Hint := IfThen(bConfigurationSaved = CONFIG_SAVED, EmptyStr, rsOptTooltipConfigureSaveToChange);
  end;
end;

{ TfrmOptionsToolTips.chkShowToolTipChange }
procedure TfrmOptionsToolTips.chkShowToolTipChange(Sender: TObject);
begin
  pnlConfigurationToolTips.Enabled := TCheckBox(Sender).Checked;
  pnlToolTipsListbox.Enabled := pnlConfigurationToolTips.Enabled;
end;

{ lsbCustomFieldsSelectionChange }
procedure TfrmOptionsToolTips.lsbCustomFieldsSelectionChange(Sender: TObject; User: boolean);
begin
  bCurrentlyLoadingSettings := True;
  pnlActualToolTipsConfiguration.Enabled:= lsbCustomFields.ItemIndex <> -1;
  if pnlActualToolTipsConfiguration.Enabled then
  begin
    edtFieldsMask.Text := FFileInfoToolTipTemp.HintItemList[lsbCustomFields.ItemIndex].Mask;
    LoadMemoWithThisHint(FFileInfoToolTipTemp.HintItemList[lsbCustomFields.ItemIndex].Hint);
  end;
  bCurrentlyLoadingSettings := False;
end;

{ TfrmOptionsToolTips.lsbCustomFieldsDragOver }
procedure TfrmOptionsToolTips.lsbCustomFieldsDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
begin
  Accept := True;
end;

{ TfrmOptionsToolTips.lsbCustomFieldsDragDrop }
procedure TfrmOptionsToolTips.lsbCustomFieldsDragDrop(Sender, Source: TObject; X, Y: integer);
var
  SrcIndex, DestIndex: integer;
begin
  SrcIndex := lsbCustomFields.ItemIndex;
  if SrcIndex = -1 then
    Exit;
  DestIndex := lsbCustomFields.GetIndexAtY(Y);
  if (DestIndex < 0) or (DestIndex >= lsbCustomFields.Count) then
    DestIndex := lsbCustomFields.Count - 1;

  lsbCustomFields.Items.Move(SrcIndex, DestIndex);
  FFileInfoToolTipTemp.HintItemList.Move(SrcIndex, DestIndex);
  lsbCustomFields.ItemIndex := DestIndex;
  lsbCustomFieldsSelectionChange(lsbCustomFields, False);
end;

{ TfrmOptionsToolTips.btnApplyToolTipsFileTypeClick }
procedure TfrmOptionsToolTips.btnApplyToolTipsFileTypeClick(Sender: TObject);
begin
  Save;
  if lsbCustomFields.CanFocus then
    lsbCustomFields.SetFocus;
end;

{TfrmOptionsToolTips.btnCopyToolTipsFileTypeClick }
procedure TfrmOptionsToolTips.btnCopyToolTipsFileTypeClick(Sender: TObject);
var
  sCurrentSelectedName, sNewName: string;
  iIndexCopy, iPosOpenPar, iNewInsertedPosition: integer;
  ANewHintItem: THintItem;
begin
  if lsbCustomFields.ItemIndex < 0 then
    Exit;

  sCurrentSelectedName := lsbCustomFields.Items.Strings[lsbCustomFields.ItemIndex];
  if LastDelimiter(')', sCurrentSelectedName) = length(sCurrentSelectedName) then
  begin
    iPosOpenPar := LastDelimiter('(', sCurrentSelectedName);
    if (iPosOpenPar > 0) and (iPosOpenPar > (length(sCurrentSelectedName) - 4)) then
      sCurrentSelectedName := LeftStr(sCurrentSelectedName, pred(pred(iPosOpenPar)));
  end;

  iIndexCopy := 2;
  while lsbCustomFields.Items.IndexOf(Format('%s (%d)', [sCurrentSelectedName, iIndexCopy])) <> -1 do
    Inc(iIndexCopy);
  sNewName := Format('%s (%d)', [sCurrentSelectedName, iIndexCopy]);

  ANewHintItem := FFileInfoToolTipTemp.HintItemList[lsbCustomFields.ItemIndex].Clone;
  //Let's place our copy right after the original one.
  iNewInsertedPosition := succ(lsbCustomFields.ItemIndex);
  if iNewInsertedPosition < FFileInfoToolTipTemp.HintItemList.Count then
  begin
    lsbCustomFields.Items.Insert(iNewInsertedPosition, sNewName);
    FFileInfoToolTipTemp.HintItemList.Insert(iNewInsertedPosition, ANewHintItem);
  end
  else
  begin
    lsbCustomFields.Items.Add(sNewName);
    FFileInfoToolTipTemp.HintItemList.Add(ANewHintItem);
  end;
  lsbCustomFields.ItemIndex := iNewInsertedPosition;
  SetActiveButtonsBasedOnToolTipsQuantity;
  if edtFieldsMask.CanFocus then
    edtFieldsMask.SetFocus;
end;

{ TfrmOptionsToolTips.btnAddToolTipsFileTypeClick }
procedure TfrmOptionsToolTips.btnAddToolTipsFileTypeClick(Sender: TObject);
var
  sName: string;
  ANewHintItem: THintItem;
begin
  sName := EmptyStr;
  if InputQuery(rsOptAddingToolTipFileType, rsOptToolTipFileType, sName) then
  begin
    if sName <> EmptyStr then
    begin
      if isUniqueFileType(sName) then
      begin
        ANewHintItem := THintItem.Create;
        ANewHintItem.Name := sName;
        FFileInfoToolTipTemp.HintItemList.Add(ANewHintItem);
        lsbCustomFields.ItemIndex := lsbCustomFields.Items.Add(sName);
        lsbCustomFieldsSelectionChange(lsbCustomFields, False);
        ClearData;
        SetActiveButtonsBasedOnToolTipsQuantity;
        if edtFieldsMask.CanFocus then
          edtFieldsMask.SetFocus;
      end;
    end;
  end;
end;

{ TfrmOptionsToolTips.btnRenameToolTipsFileTypeClick }
procedure TfrmOptionsToolTips.btnRenameToolTipsFileTypeClick(Sender: TObject);
var
  sNewName: string;
begin
  if lsbCustomFields.ItemIndex < 0 then
    Exit;
  sNewName := lsbCustomFields.Items.Strings[lsbCustomFields.ItemIndex];
  if InputQuery(rsOptRenamingToolTipFileType, rsOptToolTipsFileTypeName, sNewName) then
  begin
    if isUniqueFileType(sNewName) then
      if lsbCustomFields.Items.IndexOf(sNewName) = -1 then
      begin
        lsbCustomFields.Items.Strings[lsbCustomFields.ItemIndex] := sNewName;
        FFileInfoToolTipTemp.HintItemList[lsbCustomFields.ItemIndex].Name := sNewName;
      end
      else
      begin
        msgError(Format(rsOptToolTipFileTypeAlreadyExists, [sNewName]));
      end;
  end;
end;

{ TfrmOptionsToolTips.btnDeleteToolTipsFileTypeClick }
procedure TfrmOptionsToolTips.btnDeleteToolTipsFileTypeClick(Sender: TObject);
var
  iIndexDelete: longint;
begin
  iIndexDelete := lsbCustomFields.ItemIndex;
  if (iIndexDelete < 0) then
    Exit;

  if MsgBox(Format(rsOptToolTipFileTypeConfirmDelete, [lsbCustomFields.Items.Strings[lsbCustomFields.ItemIndex]]), [msmbYes, msmbCancel], msmbCancel, msmbCancel) = mmrYes then
  begin
    bCurrentlyFilling := True;
    try
      lsbCustomFields.Items.Delete(iIndexDelete);
      FFileInfoToolTipTemp.HintItemList.Delete(iIndexDelete);

      if lsbCustomFields.Items.Count > 0 then
      begin
        if iIndexDelete >= FFileInfoToolTipTemp.HintItemList.Count then
          lsbCustomFields.ItemIndex := pred(FFileInfoToolTipTemp.HintItemList.Count)
        else
          lsbCustomFields.ItemIndex := iIndexDelete;
      end
      else
      begin
        ClearData;
      end;
      lsbCustomFieldsSelectionChange(lsbCustomFields, False);

      SetActiveButtonsBasedOnToolTipsQuantity;
      if edtFieldsMask.CanFocus then
        edtFieldsMask.SetFocus;

    finally
      bCurrentlyFilling := False;
    end;
  end;
end;

{ TfrmOptionsToolTips.btnTooltipOtherClick }
procedure TfrmOptionsToolTips.btnTooltipOtherClick(Sender: TObject);
var
  pWantedPos: TPoint;
begin
  pWantedPos := btnTooltipOther.ClientToScreen(Point(btnTooltipOther.Width div 2, btnTooltipOther.Height - 5)); // Position this way instead of using mouse cursor since it will work for keyboard user.
  pmTooltipOther.PopUp(pWantedPos.X, pWantedPos.Y);
end;

{ TfrmOptionsToolTips.miToolTipsFileTypeDiscardModificationClick }
procedure TfrmOptionsToolTips.miToolTipsFileTypeDiscardModificationClick(Sender: TObject);
begin
  FFileInfoToolTipTemp.Assign(gFileInfoToolTip);
  FillListBoxWithToolTipsList;
  SetConfigurationState(CONFIG_SAVED);
  SetActiveButtonsBasedOnToolTipsQuantity;
end;

{ TfrmOptionsToolTips.miToolTipsFileTypeSortFileTypeClick }
procedure TfrmOptionsToolTips.miToolTipsFileTypeSortFileTypeClick(Sender: TObject);
begin
  if FFileInfoToolTipTemp.HintItemList.Count > 0 then
  begin
    FFileInfoToolTipTemp.Sort;
    FillListBoxWithToolTipsList;
  end;
end;

{ TfrmOptionsToolTips.miToolTipsFileTypeExportClick }
procedure TfrmOptionsToolTips.miToolTipsFileTypeExportClick(Sender: TObject);
var
  slValueList, slOutputIndexSelected: TStringList;
  ExportedFileInfoToolTipTemp: TFileInfoToolTip;
  iIndex, iExportedIndex: integer;
begin
  if FFileInfoToolTipTemp.HintItemList.Count > 0 then
  begin
    slValueList := TStringList.Create;
    slOutputIndexSelected := TStringList.Create;
    try
      for iIndex := 0 to pred(FFileInfoToolTipTemp.HintItemList.Count) do
        slValueList.Add(FFileInfoToolTipTemp.HintItemList[iIndex].Name);

      if ShowInputMultiSelectListBox(rsOptToolTipFileTypeExportCaption, rsOptToolTipFileTypeExportPrompt, slValueList, slOutputIndexSelected) then
      begin
        ExportedFileInfoToolTipTemp := TFileInfoToolTip.Create;
        try
          for iIndex := 0 to pred(slOutputIndexSelected.Count) do
          begin
            iExportedIndex := StrToIntDef(slOutputIndexSelected.Strings[iIndex], -1);
            if iExportedIndex <> -1 then
              ExportedFileInfoToolTipTemp.HintItemList.Add(FFileInfoToolTipTemp.HintItemList[iExportedIndex].Clone);
          end;

          if ExportedFileInfoToolTipTemp.HintItemList.Count > 0 then
          begin
            SaveTooltipFileTypeDialog.DefaultExt := '*.tooltip';
            SaveTooltipFileTypeDialog.FilterIndex := 1;
            SaveTooltipFileTypeDialog.Title := rsOptToolTipFileTypeWhereToSave;
            SaveTooltipFileTypeDialog.FileName := rsOptToolTipFileTypeDefaultExportFilename;
            if SaveTooltipFileTypeDialog.Execute then
            begin
              ExportedFileInfoToolTipTemp.SaveToFile(SaveTooltipFileTypeDialog.FileName);
              msgOK(Format(rsOptToolTipFileTypeExportDone, [ExportedFileInfoToolTipTemp.HintItemList.Count, SaveTooltipFileTypeDialog.FileName]));
            end;
          end;
        finally
          ExportedFileInfoToolTipTemp.Free;
        end;
      end;
    finally
      slOutputIndexSelected.Free;
      slValueList.Free;
    end;
  end;
end;

{ TfrmOptionsToolTips.miToolTipsFileTypeImportClick}
procedure TfrmOptionsToolTips.miToolTipsFileTypeImportClick(Sender: TObject);
var
  ImportedFileInfoToolTipTemp: TFileInfoToolTip;
  slValueList, slOutputIndexSelected: TStringList;
  iIndex, iImportedIndex, iNbImported: integer;
begin
  OpenTooltipFileTypeDialog.DefaultExt := '*.tooltip';
  OpenTooltipFileTypeDialog.FilterIndex := 1;
  OpenTooltipFileTypeDialog.Title := rsOptToolTipFileTypeImportFile;
  if OpenTooltipFileTypeDialog.Execute then
  begin
    ImportedFileInfoToolTipTemp := TFileInfoToolTip.Create;
    try
      ImportedFileInfoToolTipTemp.LoadFromFile(OpenTooltipFileTypeDialog.FileName);
      if ImportedFileInfoToolTipTemp.HintItemList.Count > 0 then
      begin
        slValueList := TStringList.Create;
        slOutputIndexSelected := TStringList.Create;
        try
          for iIndex := 0 to pred(ImportedFileInfoToolTipTemp.HintItemList.Count) do
            slValueList.Add(ImportedFileInfoToolTipTemp.HintItemList[iIndex].Name);
          if ShowInputMultiSelectListBox(rsOptToolTipFileTypeImportCaption, rsOptToolTipFileTypeImportPrompt, slValueList, slOutputIndexSelected) then
          begin
            iNbImported := 0;
            for iIndex := 0 to pred(slOutputIndexSelected.Count) do
            begin
              iImportedIndex := StrToIntDef(slOutputIndexSelected.Strings[iIndex], -1);
              if iImportedIndex <> -1 then
              begin
                FFileInfoToolTipTemp.HintItemList.Add(ImportedFileInfoToolTipTemp.HintItemList[iImportedIndex].Clone);
                lsbCustomFields.Items.add(FFileInfoToolTipTemp.HintItemList[pred(FFileInfoToolTipTemp.HintItemList.Count)].Name);
                Inc(iNbImported);
              end;
            end;
            lsbCustomFields.ItemIndex := lsbCustomFields.Items.Count - 1;
            if iNbImported > 0 then
            begin
              SetActiveButtonsBasedOnToolTipsQuantity;
              msgOK(Format(rsOptToolTipFileTypeImportDone, [iNbImported, OpenTooltipFileTypeDialog.FileName]));
            end;
          end;
        finally
          slOutputIndexSelected.Free;
          slValueList.Free;
        end;
      end;
    finally
      ImportedFileInfoToolTipTemp.Free;
    end;
  end;
end;

{ TfrmOptionsToolTips.miPluginClick }
procedure TfrmOptionsToolTips.miPluginClick(Sender: TObject);
var
  sMask: string;
  MenuItem: TMenuItem absolute Sender;
begin
  case MenuItem.Tag of
    0: sMask := '[DC().' + MenuItem.Hint + '{}]';
    1: sMask := '[Plugin(' + MenuItem.Parent.Caption + ').' + MenuItem.Hint + '{}]';
    2: sMask := '[Plugin(' + MenuItem.Parent.Parent.Caption + ').' + MenuItem.Parent.Hint + '{' + MenuItem.Hint + '}]';
    3: sMask := '[DC().' + MenuItem.Parent.Hint + '{' + MenuItem.Hint + '}] ';
    else
      sMask := EmptyStr;
  end;

  if sMask <> EmptyStr then
  begin
    memFieldsList.SelText := sMask;
    if memFieldsList.CanFocus then
      memFieldsList.SetFocus;
  end;
end;

{ TfrmOptionsToolTips.btnFieldsListClick }
procedure TfrmOptionsToolTips.btnFieldsListClick(Sender: TObject);
begin
  FillContentFieldMenu(pmFields.Items, @miPluginClick);
  pmFields.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
end;

{ TfrmOptionsToolTips.btnFieldsSearchTemplateClick }
procedure TfrmOptionsToolTips.btnFieldsSearchTemplateClick(Sender: TObject);
var
  sMask: string;
  bTemplate: boolean;
begin
  sMask := '';
  if ShowMaskInputDlg(rsMarkPlus, rsMaskInput, glsMaskHistory, sMask) then
  begin
    bTemplate := IsMaskSearchTemplate(sMask);
    edtFieldsMask.Text := sMask;
    edtFieldsMask.Enabled := not bTemplate;
  end;
end;

{ TfrmOptionsToolTips.isUniqueFileType }
function TfrmOptionsToolTips.isUniqueFileType(paramNewName: string): boolean;
begin
  Result := (lsbCustomFields.Items.IndexOf(paramNewName) = -1);
  if not Result then
    msgError(Format(rsOptToolTipFileTypeAlreadyExists, [paramNewName]));
end;

{ TfrmOptionsToolTips.ClearData }
procedure TfrmOptionsToolTips.ClearData;
begin
  bCurrentlyLoadingSettings := True;
  edtFieldsMask.Text := EmptyStr;
  memFieldsList.Clear;
  bCurrentlyLoadingSettings := False;
end;

end.
