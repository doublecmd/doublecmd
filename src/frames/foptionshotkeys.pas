{
   Double Commander
   -------------------------------------------------------------------------
   Hotkeys options page

   Copyright (C) 2006-2016 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsHotkeys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Grids, fOptionsFrame,
  fOptionsHotkeysEditHotkey, uHotkeyManager, DCBasicTypes, Controls, Buttons,
  Menus, ActnList, uGlobs;

type
  { TfrmOptionsHotkeys }
  TfrmOptionsHotkeys = class(TOptionsEditor)
    btnDeleteHotKey: TButton;
    btnAddHotKey: TButton;
    btnEditHotkey: TButton;
    edtFilter: TEdit;
    lblCommands: TLabel;
    lbFilter: TLabel;
    lblSCFiles: TLabel;
    lblCategories: TLabel;
    lbSCFilesList: TComboBox;
    lbxCategories: TComboBox;
    pnlHotkeyButtons: TPanel;
    stgCommands: TStringGrid;
    stgHotkeys: TStringGrid;
    btnFileAction: TSpeedButton;
    lblSortOrder: TLabel;
    cbCommandSortOrder: TComboBox;
    alMainActionList: TActionList;
    actAddHotKey: TAction;
    actEditHotKey: TAction;
    actDeleteHotKey: TAction;
    actPopupFileRelatedMenu: TAction;
    actSortByCommand: TAction;
    actSortByHotKeysGrouped: TAction;
    actSortByHotKeysOnePerLine: TAction;
    actPreviousCategory: TAction;
    actNextCategory: TAction;
    pmShortcutMenu: TPopupMenu;
    actSaveNow: TAction;
    actRename: TAction;
    actCopy: TAction;
    actDelete: TAction;
    actRestoreDefault: TAction;
    miSaveNow: TMenuItem;
    miCopy: TMenuItem;
    miRename: TMenuItem;
    miDelete: TMenuItem;
    miRestoreDefault: TMenuItem;
    miSeparator1: TMenuItem;
    miCategories: TMenuItem;
    miPreviousCategory: TMenuItem;
    miNextCategory: TMenuItem;
    miSortOrder: TMenuItem;
    miSortByCommand: TMenuItem;
    miSortByHotKeysOnePerLine: TMenuItem;
    miSortByHotKeysGrouped: TMenuItem;
    miCommands: TMenuItem;
    miAddHotKey: TMenuItem;
    miEditHotKey: TMenuItem;
    miDeleteHotKey: TMenuItem;
    procedure actRenameExecute(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure lbSCFilesListChange(Sender: TObject);
    procedure lbxCategoriesChange(Sender: TObject);
    procedure stgCommandsDblClick(Sender: TObject);
    procedure stgCommandsDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; {%H-}aState: TGridDrawState);
    procedure stgCommandsResize(Sender: TObject);
    procedure stgCommandsSelectCell(Sender: TObject; {%H-}aCol, aRow: integer; var {%H-}CanSelect: boolean);
    procedure stgHotkeysDblClick(Sender: TObject);
    procedure stgHotkeysKeyDown(Sender: TObject; var Key: word;
    {%H-}Shift: TShiftState);
    procedure stgHotkeysResize(Sender: TObject);
    procedure stgHotkeysSelectCell(Sender: TObject; {%H-}aCol, aRow: integer; var {%H-}CanSelect: boolean);
    procedure stgCommandsCompareCells(Sender: TObject; ACol, ARow, BCol, BRow: integer; var Result: integer);
    procedure stgCommandsHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
    procedure cbCommandSortOrderChange(Sender: TObject);
    function isOkToContinueRegardingModifiedOrNot: boolean;
    function GetANewSetupName(var ASetupName: string): boolean;
    procedure actAddHotKeyExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteHotKeyExecute(Sender: TObject);
    procedure actEditHotKeyExecute(Sender: TObject);
    procedure actNextCategoryExecute(Sender: TObject);
    procedure actPopupFileRelatedMenuExecute(Sender: TObject);
    procedure actPreviousCategoryExecute(Sender: TObject);
    procedure actRestoreDefaultExecute(Sender: TObject);
    procedure actSaveNowExecute(Sender: TObject);
    procedure actAdjustSortOrderExecute(Sender: TObject);
  private
    FEditForm: TfrmEditHotkey;
    FHotkeysAutoColWidths: array of integer;
    FHotkeysAutoGridWidth: integer;
    FHotkeysCategories: TStringList; // Untranslated
    FUpdatingShortcutsFiles: boolean;
    FModified: boolean;
    procedure AutoSizeCommandsGrid;
    procedure AutoSizeHotkeysGrid;
    procedure ClearHotkeysGrid;
    procedure DeleteHotkeyFromGrid(aHotkey: string);
    function GetSelectedCommand: string;
    {en
       Refreshes all hotkeys from the Commands grid
    }
    procedure UpdateHotkeys(HMForm: THMForm);
    procedure UpdateHotkeysForCommand(HMForm: THMForm; RowNr: integer);
    procedure FillSCFilesList;
    {en
       Return hotkeys assigned for command for the form and its controls.
    }
    procedure GetHotKeyList(HMForm: THMForm; Command: string; HotkeysList: THotkeys);
    {en
       Fill hotkey grid with all hotkeys assigned to a command
    }
    procedure FillHotkeyList(sCommand: string);
    {en
       Fill Commands grid with all commands available for the selected category.
       @param(Filter
              If not empty string then shows only commands containing Filter string.)
    }
    procedure FillCommandList(Filter: string);
    procedure FillCategoriesList;
    {en
       Retrieves untranslated form name.
    }
    function GetSelectedForm: string;
    procedure SelectHotkey(Hotkey: THotkey);
    procedure ShowEditHotkeyForm(EditMode: boolean; aHotkeyRow: integer);
    procedure ShowEditHotkeyForm(EditMode: boolean; const AForm: string; const ACommand: string; const AHotkey: THotkey; const AControls: TDynamicStringArray);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddDeleteWithShiftHotkey(UseTrash: boolean);
    class function GetIconIndex: integer; override;
    class function GetTitle: string; override;
    function IsSignatureComputedFromAllWindowComponents: boolean; override;
    procedure DeleteHotkey;
    procedure DeleteAllHotkeys;
    procedure TryToSelectThatCategory(sCategory: string);
  end;

function GetSortableShortcutName(sToSort: string): string;

implementation

{$R *.lfm}

uses
  fMain,
  Graphics, Forms, LCLType, Dialogs, LazUTF8, LCLVersion,
  uFindEx, uGlobsPaths, uLng, uKeyboard, uFormCommands, DCStrUtils,
  DCOSUtils, uShowMsg;

const
  stgCmdCommandIndex = 0;
  stgCmdHotkeysIndex = 1;
  stgCmdDescriptionIndex = 2;

type
  PHotkeyItem = ^THotkeyItem;
  THotkeyItem = record
    Hotkey: THotkey;
    Controls: TDynamicStringArray;
  end;

procedure DestroyHotkeyItem(HotkeyItem: PHotkeyItem);
begin
  if Assigned(HotkeyItem) then
  begin
    HotkeyItem^.Hotkey.Free;
    Dispose(HotkeyItem);
  end;
end;

{ MyStrcompare }
// Used to help "HotkeysToString" function to have the returned shortcut strings
// sorted in such way that "Fx" are first, "ALT-Fx" second, etc. when there are
// more than one shortcut per command.
function MyStrcompare(List: TStringList; Index1, Index2: integer): integer;
begin
  Result := CompareText(GetSortableShortcutName(List.Strings[Index1]), GetSortableShortcutName(List.Strings[Index2]));
end;

{ HotkeysToString }
// Converts hotkeys list to string.
function HotkeysToString(const Hotkeys: THotkeys): string;
var
  sCurrent: string;
  i: integer;
  sList: TStringList;
begin
  Result := '';
  sList := TStringList.Create;
  try
    sList.CaseSensitive := True;
    for i := 0 to Hotkeys.Count - 1 do
    begin
      sCurrent := ShortcutsToText(Hotkeys[i].Shortcuts);
      if sList.IndexOf(sCurrent) < 0 then
        sList.Add(sCurrent);
    end;

    sList.CustomSort(@MyStrcompare);

    for i := 0 to pred(sList.Count) do
      AddStrWithSep(Result, sList.Strings[i], ';');

  finally
    sList.Free;
  end;
end;

function CompareCategories(List: TStringList; Index1, Index2: integer): integer;
begin
{$IF LCL_FULLVERSION >= 093100}
  Result := UTF8CompareText(List.Strings[Index1], List.Strings[Index2]);
{$ELSE}
  Result := WideCompareText(UTF8Decode(List.Strings[Index1]), UTF8Decode(List.Strings[Index2]));
{$ENDIF}
end;

{ TfrmOptionsHotkeys }

{ TfrmOptionsHotkeys.edtFilterChange }
procedure TfrmOptionsHotkeys.edtFilterChange(Sender: TObject);
{< filtering active commands list}
begin
  if lbxCategories.ItemIndex = -1 then Exit;
  FillCommandList(edtFilter.Text);
end;

{ TfrmOptionsHotkeys.lbSCFilesListChange }
procedure TfrmOptionsHotkeys.lbSCFilesListChange(Sender: TObject);
begin
  if not FUpdatingShortcutsFiles then
  begin
    if not isOkToContinueRegardingModifiedOrNot then
    begin
      if gNameSCFile <> lbSCFilesList.Items[lbSCFilesList.ItemIndex] then
        lbSCFilesList.ItemIndex := lbSCFilesList.Items.indexof(gNameSCFile);
    end
    else
    begin
      if (lbSCFilesList.ItemIndex >= 0) then
      begin
        gNameSCFile := lbSCFilesList.Items[lbSCFilesList.ItemIndex];
        HotMan.Load(gpCfgDir + gNameSCFile);
        FModified := False;
        FillCategoriesList;
        lbxCategoriesChange(lbxCategories);
      end;
    end;
  end;
end;

{ TfrmOptionsHotkeys.lbxCategoriesChange }
procedure TfrmOptionsHotkeys.lbxCategoriesChange(Sender: TObject);
begin
  if lbxCategories.ItemIndex = -1 then Exit;

  edtFilter.Clear;
  FillCommandList('');
end;

{ TfrmOptionsHotkeys.stgCommandsDblClick }
procedure TfrmOptionsHotkeys.stgCommandsDblClick(Sender: TObject);
begin
  // add hot key
  ShowEditHotkeyForm(False, GetSelectedForm, GetSelectedCommand, nil, nil);
end;

{ TfrmOptionsHotkeys.stgCommandsDrawCell }
procedure TfrmOptionsHotkeys.stgCommandsDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  OffsetY: integer;
begin
  if aCol = stgCmdHotkeysIndex then
  begin
    if aRow > 0 then
    begin
      with Sender as TStringGrid do
      begin
        if Cells[aCol, aRow] <> '' then
        begin
          OffsetY := (DefaultRowHeight - Canvas.TextHeight(Cells[aCol, aRow])) div 2;
          if not (gdSelected in aState) then Canvas.Font.Color := clRed else Canvas.Font.Color := clWhite;
          Canvas.TextOut(aRect.Left + 3, aRect.Top + OffsetY, Cells[aCol, aRow]);
        end;
      end;
    end;
  end;
end;

{ TfrmOptionsHotkeys.stgCommandsResize }
procedure TfrmOptionsHotkeys.stgCommandsResize(Sender: TObject);
begin
  AutoSizeCommandsGrid;
end;

{ TfrmOptionsHotkeys.stgCommandsSelectCell }
procedure TfrmOptionsHotkeys.stgCommandsSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
// < find hotkeys for command
var
  sCommand: string;
begin
  // clears all controls
  actAddHotKey.Enabled := False;
  actEditHotkey.Enabled := False;
  actDeleteHotKey.Enabled := False;
  ClearHotkeysGrid;

  if aRow >= stgCommands.FixedRows then
  begin
    sCommand := stgCommands.Cells[stgCmdCommandIndex, aRow];
    FillHotkeyList(sCommand);
    actAddHotKey.Enabled := True;
  end;
end;

{ TfrmOptionsHotkeys.stgHotkeysDblClick }
procedure TfrmOptionsHotkeys.stgHotkeysDblClick(Sender: TObject);
begin
  ShowEditHotkeyForm(True, stgHotkeys.Row);
end;

{ TfrmOptionsHotkeys.stgHotkeysKeyDown }
procedure TfrmOptionsHotkeys.stgHotkeysKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_DELETE then DeleteHotkey;
end;

{ TfrmOptionsHotkeys.stgHotkeysResize }
procedure TfrmOptionsHotkeys.stgHotkeysResize(Sender: TObject);
begin
  AutoSizeHotkeysGrid;
end;

{ TfrmOptionsHotkeys.stgHotkeysSelectCell }
procedure TfrmOptionsHotkeys.stgHotkeysSelectCell(Sender: TObject; aCol, aRow: integer; var CanSelect: boolean);
var
  aEnabled: boolean;
begin
  aEnabled := aRow >= stgHotkeys.FixedRows;
  actEditHotkey.Enabled := aEnabled;
  actDeleteHotKey.Enabled := aEnabled;
end;

{ TfrmOptionsHotkeys.AutoSizeCommandsGrid }
procedure TfrmOptionsHotkeys.AutoSizeCommandsGrid;
begin
  stgCommands.AutoSizeColumns;
end;

{ TfrmOptionsHotkeys.AutoSizeHotkeysGrid }
procedure TfrmOptionsHotkeys.AutoSizeHotkeysGrid;
var
  Diff: integer = 0;
  i: integer;
begin
  with stgHotkeys do
  begin
    if Length(FHotkeysAutoColWidths) = ColCount then
    begin
      if ClientWidth > FHotkeysAutoGridWidth then
        Diff := (ClientWidth - FHotkeysAutoGridWidth) div 3;
      for i := 0 to ColCount - 1 do
        ColWidths[i] := FHotkeysAutoColWidths[i] + Diff;
    end;
  end;
end;

{ TfrmOptionsHotkeys.actAddHotKeyExecute }
procedure TfrmOptionsHotkeys.actAddHotKeyExecute(Sender: TObject);
begin
  ShowEditHotkeyForm(False, GetSelectedForm, GetSelectedCommand, nil, nil);
end;

{ TfrmOptionsHotkeys.DeleteHotkeyFromGrid }
procedure TfrmOptionsHotkeys.DeleteHotkeyFromGrid(aHotkey: string);
var
  i: integer;
begin
  for i := stgHotkeys.FixedRows to stgHotkeys.RowCount - 1 do
    if stgHotkeys.Cells[0, i] = aHotkey then
    begin
      DestroyHotkeyItem(PHotkeyItem(stgHotkeys.Objects[0, i]));
      stgHotkeys.DeleteColRow(False, i);
      Break;
    end;
end;

{ TfrmOptionsHotkeys.UpdateHotkeys }
procedure TfrmOptionsHotkeys.UpdateHotkeys(HMForm: THMForm);
var
  i: integer;
begin
  if cbCommandSortOrder.ItemIndex = 0 then
  begin
    for i := Self.stgCommands.FixedRows to Self.stgCommands.RowCount - 1 do
      Self.UpdateHotkeysForCommand(HMForm, i);
  end
  else
  begin
    FillCommandList(edtFilter.Text);
  end;
end;

{ TfrmOptionsHotkeys.UpdateHotkeysForCommand }
procedure TfrmOptionsHotkeys.UpdateHotkeysForCommand(HMForm: THMForm; RowNr: integer);
var
  Hotkeys: THotkeys;
begin
  Hotkeys := THotkeys.Create(False);
  try
    GetHotKeyList(HMForm, stgCommands.Cells[stgCmdCommandIndex, RowNr], Hotkeys);
    stgCommands.Cells[stgCmdHotkeysIndex, RowNr] := HotkeysToString(Hotkeys);
  finally
    Hotkeys.Free;
  end;
end;

{ TfrmOptionsHotkeys.FillSCFilesList }
procedure TfrmOptionsHotkeys.FillSCFilesList;
var
  SR: TSearchRecEx;
  Res, iItem: integer;
  slSCFileList: TStringList;
begin
  FUpdatingShortcutsFiles := True;

  slSCFileList := TStringList.Create;
  try
    slSCFileList.Sorted := True;

    Res := FindFirstEx(gpCfgDir + '*.scf', 0, SR);
    try
      while Res = 0 do
      begin
        slSCFileList.Add(Sr.Name);
        Res := FindNextEx(SR);
      end;
    finally
      FindCloseEx(SR);
    end;

    lbSCFilesList.Items.Clear;
    for iItem := 0 to pred(slSCFileList.Count) do
      lbSCFilesList.Items.Add(slSCFileList.Strings[iItem]);
    iItem := lbSCFilesList.Items.IndexOf(gNameSCFile);
    if iItem <> -1 then lbSCFilesList.ItemIndex := iItem
    else if lbSCFilesList.Items.Count > 0 then lbSCFilesList.ItemIndex := 0;

  finally
    FreeAndNil(slSCFileList);
  end;

  FUpdatingShortcutsFiles := False;
end;

{ TfrmOptionsHotkeys.GetHotKeyList }
procedure TfrmOptionsHotkeys.GetHotKeyList(HMForm: THMForm; Command: string; HotkeysList: THotkeys);
  procedure AddHotkeys(hotkeys: THotkeys);
  var
    i: integer;
  begin
    for i := 0 to hotkeys.Count - 1 do
    begin
      if hotkeys[i].Command = Command then
        HotkeysList.Add(hotkeys[i]);
    end;
  end;

var
  i: integer;
begin
  AddHotkeys(HMForm.Hotkeys);
  for i := 0 to HMForm.Controls.Count - 1 do
    AddHotkeys(HMForm.Controls[i].Hotkeys);
end;

{ TfrmOptionsHotkeys.ClearHotkeysGrid }
procedure TfrmOptionsHotkeys.ClearHotkeysGrid;
var
  i: integer;
begin
  for i := stgHotkeys.FixedRows to stgHotkeys.RowCount - 1 do
    DestroyHotkeyItem(PHotkeyItem(stgHotkeys.Objects[0, i]));
  stgHotkeys.RowCount := stgHotkeys.FixedRows;
end;

{ TfrmOptionsHotkeys.FillHotkeyList }
procedure TfrmOptionsHotkeys.FillHotkeyList(sCommand: string);
  function SetObject(RowNr: integer; AHotkey: THotkey): PHotkeyItem;
  var
    HotkeyItem: PHotkeyItem;
  begin
    New(HotkeyItem);
    stgHotkeys.Objects[0, RowNr] := TObject(HotkeyItem);
    HotkeyItem^.Hotkey := AHotkey.Clone;
    Result := HotkeyItem;
  end;

var
  HMForm: THMForm;
  HMControl: THMControl;
  iHotKey, iControl, iGrid: integer;
  hotkey: THotkey;
  found: boolean;
  HotkeyItem: PHotkeyItem;
begin
  ClearHotkeysGrid;

  if (sCommand = EmptyStr) or (lbxCategories.ItemIndex = -1) then
    Exit;

  HMForm := HotMan.Forms.Find(GetSelectedForm);
  if not Assigned(HMForm) then
    Exit;

  stgHotkeys.BeginUpdate;
  try
    // add hotkeys from form
    for iHotKey := 0 to HMForm.Hotkeys.Count - 1 do
    begin
      hotkey := HMForm.Hotkeys[iHotKey];
      if hotkey.Command <> sCommand then
        continue;

      stgHotkeys.RowCount := stgHotkeys.RowCount + 1;
      stgHotkeys.Cells[0, stgHotkeys.RowCount - 1] := ShortcutsToText(hotkey.Shortcuts);
      stgHotkeys.Cells[1, stgHotkeys.RowCount - 1] := ArrayToString(hotkey.Params);
      SetObject(stgHotkeys.RowCount - 1, hotkey);
    end;

    // add hotkeys from controls
    for iControl := 0 to HMForm.Controls.Count - 1 do
    begin
      HMControl := HMForm.Controls[iControl];
      for iHotKey := 0 to HMControl.Hotkeys.Count - 1 do
      begin
        hotkey := HMControl.Hotkeys[iHotKey];
        if hotkey.Command <> sCommand then
          continue;

        // search for hotkey in grid and add control name to list
        found := False;
        for iGrid := stgHotkeys.FixedRows to stgHotkeys.RowCount - 1 do
        begin
          HotkeyItem := PHotkeyItem(stgHotkeys.Objects[0, iGrid]);
          if HotkeyItem^.Hotkey.SameShortcuts(hotkey.Shortcuts) and HotkeyItem^.Hotkey.SameParams(hotkey.Params) then
          begin
            stgHotkeys.Cells[2, iGrid] := stgHotkeys.Cells[2, iGrid] + HMControl.Name + ';';
            HotkeyItem := PHotkeyItem(stgHotkeys.Objects[0, iGrid]);
            AddString(HotkeyItem^.Controls, HMControl.Name);
            found := True;
            break;
          end; { if }
        end; { for }

        // add new row for hotkey
        if not found then
        begin
          stgHotkeys.RowCount := stgHotkeys.RowCount + 1;
          stgHotkeys.Cells[0, stgHotkeys.RowCount - 1] := ShortcutsToText(hotkey.Shortcuts);
          stgHotkeys.Cells[1, stgHotkeys.RowCount - 1] := ArrayToString(hotkey.Params);
          stgHotkeys.Cells[2, stgHotkeys.RowCount - 1] := HMControl.Name + ';';
          HotkeyItem := SetObject(stgHotkeys.RowCount - 1, hotkey);
          AddString(HotkeyItem^.Controls, HMControl.Name);
        end; { if }
      end; { for }
    end; { for }
  finally
    stgHotkeys.EndUpdate;
  end;

  stgHotkeys.AutoSizeColumns;
  SetLength(FHotkeysAutoColWidths, stgHotkeys.ColCount);
  for iHotKey := 0 to stgHotkeys.ColCount - 1 do
    FHotkeysAutoColWidths[iHotKey] := stgHotkeys.ColWidths[iHotKey];
  FHotkeysAutoGridWidth := stgHotkeys.GridWidth;
  AutoSizeHotkeysGrid;
end;

{ TfrmOptionsHotkeys.FillCommandList }
// We will scan the hotkeys and fill progressively the list "slCommandsForGrid", "slDescriptionsFroGrid" and "slHotKeyForGrid".
// Then we output to actual grid the element of the list.
// Then we sort the grid.
// Fill stgCommands with commands and descriptions
procedure TfrmOptionsHotkeys.FillCommandList(Filter: string);
var
  lcFilter: string;
  FilterParts: TStringList;
  slCommandsForGrid, slDescriptionsForGrid, slHotKeyForGrid: TStringList;

  procedure AddOrFilterOut(const Command, HotKeys, Description: string);

    function CheckHotKeys: Boolean;
    var
      lcHotKeys: string;
      i: integer;
    begin
      lcHotKeys := UTF8LowerCase(HotKeys);
      for i := 0 to pred(FilterParts.Count) do // Get filter parts split by '+' character
      begin
        if FilterParts[i] = '' then
          Continue;
        if Length(FilterParts[i]) = 1 then // Heurstics to make filtering more handy
        begin
          if FilterParts[i][1] in ['c','s','a','m'] then // Ctrl Shift Alt Meta first letters
            Result := Pos('+' + FilterParts[i] + ';', '+' + lcHotKeys + ';') <> 0
          else // other single letters
            Result := Pos('+' + FilterParts[i], '+' + lcHotKeys) <> 0;
        end
        else // plain substring search for two or more letters
          Result := Pos(FilterParts[i], lcHotKeys) <> 0;
        if not Result then
          Break;
      end;
    end;

  begin
    if (lcFilter = '') or
      (Pos(lcFilter, UTF8LowerCase(Command)) <> 0) or
      (Pos(lcFilter, UTF8LowerCase(Description)) <> 0) or
      ((HotKeys <> '') and CheckHotKeys) then
    begin
      slCommandsForGrid.Add(Command);
      slHotKeyForGrid.Add(HotKeys);
      slDescriptionsForGrid.Add(Description);
    end;
  end;

var
  slTmp: THotkeys;
  slAllCommands: TStringList;
  i, j: integer;
  HMForm: THMForm;
  sForm: string;
  CommandsFormClass: TComponentClass;
  CommandsForm: TComponent = nil;
  CommandsFormCreated: boolean = False;
  CommandsIntf: IFormCommands;
begin
  sForm := GetSelectedForm;
  CommandsFormClass := TFormCommands.GetCommandsForm(sForm);
  if not Assigned(CommandsFormClass) or not Supports(CommandsFormClass, IFormCommands) then
  begin
    stgCommands.Clean;
    Exit;
  end;

  // Find an instance of the form to retrieve action list (for descriptions).
  for i := 0 to Screen.CustomFormCount - 1 do
    if Screen.CustomForms[i].ClassType = CommandsFormClass then
    begin
      CommandsForm := Screen.CustomForms[i];
      Break;
    end;

  // If not found create an instance temporarily.
  if not Assigned(CommandsForm) then
  begin
    CommandsForm := CommandsFormClass.Create(Application);
    CommandsFormCreated := True;
  end;

  CommandsIntf := CommandsForm as IFormCommands;

  slAllCommands := TStringList.Create;
  slCommandsForGrid := TStringList.Create;
  slHotKeyForGrid := TStringList.Create;
  slDescriptionsForGrid := TStringList.Create;
  slTmp := THotkeys.Create(False);
  HMForm := HotMan.Forms.Find(sForm);

  // 1. Get all the "cm_" commands and store them in our list "slAllCommands".
  CommandsIntf.GetCommandsList(slAllCommands);

  // 2. Prepare filter to use in the next step.
  lcFilter := UTF8LowerCase(Filter);
  FilterParts := TStringList.Create;
  FilterParts.Delimiter := '+';
  FilterParts.DelimitedText := lcFilter;

  // 3. Based on all the commands we got, populate "equally" our three string list of commands, hotkeys and descrition used to fill our grid.
  for i := 0 to pred(slAllCommands.Count) do
  begin
    if Assigned(HMForm) then
    begin
      slTmp.Clear;
      GetHotKeyList(HMForm, slAllCommands.Strings[i], slTmp);
      if (THotKeySortOrder(cbCommandSortOrder.ItemIndex) = hksoByHotKeyOnePerRow) and (slTmp.Count > 0) then
      begin
        for j := 0 to pred(slTmp.Count) do
        begin
          AddOrFilterOut(
            slAllCommands.Strings[i],
            ShortcutsToText(slTmp[j].Shortcuts),
            CommandsIntf.GetCommandCaption(slAllCommands.Strings[i], cctLong));
        end;
      end
      else
      begin
        AddOrFilterOut(
          slAllCommands.Strings[i],
          HotkeysToString(slTmp),
          CommandsIntf.GetCommandCaption(slAllCommands.Strings[i], cctLong));
      end;
    end
    else
    begin
      AddOrFilterOut(
        slAllCommands.Strings[i],
        '',
        CommandsIntf.GetCommandCaption(slAllCommands.Strings[i], cctLong));
    end;
  end;

  // 4. Add to list NAMES of columns.
  slCommandsForGrid.Insert(0, rsOptHotkeysCommand);
  slHotKeyForGrid.Insert(0, rsOptHotkeysHotkeys);
  slDescriptionsForGrid.Insert(0, rsOptHotkeysDescription);

  // 5. Set stringgrid rows count.
  stgCommands.RowCount := slCommandsForGrid.Count;

  // 6. Copy to grid our created list.
  stgCommands.BeginUpdate;
  stgCommands.Clean;
  stgCommands.Cols[stgCmdCommandIndex].Assign(slCommandsForGrid);
  stgCommands.Cols[stgCmdHotkeysIndex].Assign(slHotKeyForGrid);
  stgCommands.Cols[stgCmdDescriptionIndex].Assign(slDescriptionsForGrid);

  // 7. Sort our grid according to our wish
  case THotKeySortOrder(cbCommandSortOrder.ItemIndex) of
    hksoByCommand: stgCommands.SortColRow(True, stgCmdCommandIndex);
    hksoByHotKeyGrouped, hksoByHotKeyOnePerRow: stgCommands.SortColRow(True, stgCmdHotkeysIndex);
  end;

  // 8. We have finished playing in element of the grid.
  stgCommands.EndUpdate;

  // 9. Resize the columns to fit with the text now in all the cells.
  AutoSizeCommandsGrid;

  stgCommands.Row := 0; // needs for call select function for refresh hotkeylist

  slAllCommands.Free;
  slCommandsForGrid.Free;
  slHotKeyForGrid.Free;
  slDescriptionsForGrid.Free;
  slTmp.Free;
  FilterParts.Free;

  if CommandsFormCreated then
    CommandsForm.Free;
end;

{ TfrmOptionsHotkeys.FillCategoriesList }
procedure TfrmOptionsHotkeys.FillCategoriesList;
var
  i, MainIndex, Diff: integer;
  Translated: TStringList;
begin
  Translated := TStringList.Create;
  try
    TFormCommands.GetCategoriesList(FHotkeysCategories, Translated);

    if FHotkeysCategories.Count > 0 then
    begin
      // Remove Main category so that it can be put to the top after sorting the rest.
      MainIndex := FHotkeysCategories.IndexOf('Main');
      if (MainIndex >= 0) and (Translated[MainIndex] = rsHotkeyCategoryMain) then
      begin
        FHotkeysCategories.Delete(MainIndex);
        Translated.Delete(MainIndex);
        Diff := 1; // Account for Main category being at the top.
      end
      else
      begin
        MainIndex := -1;
        Diff := 0;
      end;

      // Assign indexes to FHotkeysCategories (untranslated).
      for i := 0 to Translated.Count - 1 do
        Translated.Objects[i] := TObject(i + Diff);

      Translated.CustomSort(@CompareCategories);

      if MainIndex >= 0 then
      begin
        FHotkeysCategories.InsertObject(0, 'Main', TObject(0));
        Translated.InsertObject(0, rsHotkeyCategoryMain, TObject(0));
      end;

      lbxCategories.Items.Assign(Translated);
      lbxCategories.ItemIndex := 0;
    end
    else
      lbxCategories.Items.Clear;
  finally
    Translated.Free;
  end;
end;

{ TfrmOptionsHotkeys.GetSelectedForm }
function TfrmOptionsHotkeys.GetSelectedForm: string;
var
  Index: integer;
begin
  Index := lbxCategories.ItemIndex;
  if (Index >= 0) and (Index < FHotkeysCategories.Count) then
    Result := FHotkeysCategories[PtrUInt(lbxCategories.Items.Objects[Index])]
  else
    Result := EmptyStr;
end;

{ TfrmOptionsHotkeys.GetIconIndex }
class function TfrmOptionsHotkeys.GetIconIndex: integer;
begin
  Result := 5;
end;

{ TfrmOptionsHotkeys.GetSelectedCommand }
function TfrmOptionsHotkeys.GetSelectedCommand: string;
begin
  if stgCommands.Row >= stgCommands.FixedRows then
    Result := stgCommands.Cells[stgCmdCommandIndex, stgCommands.Row]
  else
    Result := EmptyStr;
end;

{ TfrmOptionsHotkeys.GetTitle }
class function TfrmOptionsHotkeys.GetTitle: string;
begin
  Result := rsOptionsEditorHotKeys;
end;

{ TfrmOptionsHotkeys.IsSignatureComputedFromAllWindowComponents }
function TfrmOptionsHotkeys.IsSignatureComputedFromAllWindowComponents: boolean;
begin
  Result := False;
end;

{ TfrmOptionsHotkeys.DeleteAllHotkeys }
// "ClearAllHotkeys" is a private procedure of "HotMan", so let's clear hotkeys manually by calling the same code
procedure TfrmOptionsHotkeys.DeleteAllHotkeys;
var
  iForm, iControl: integer;
begin
  for iForm := 0 to pred(HotMan.Forms.Count) do
  begin
    HotMan.Forms[iForm].Hotkeys.Clear;
    for iControl := 0 to pred(HotMan.Forms[iForm].Controls.Count) do
      HotMan.Forms[iForm].Controls[iControl].Hotkeys.Clear;
  end;
end;

{ TfrmOptionsHotkeys.DeleteHotkey }
procedure TfrmOptionsHotkeys.DeleteHotkey;
var
  i: integer;
  sCommand: string;
  HMForm: THMForm;
  HMControl: THMControl;
  hotkey: THotkey;
  HotkeyItem: PHotkeyItem;
  RememberSelectionGridRect: TGridRect;
  bCanSelect: boolean;
begin
  if stgHotkeys.Row >= stgHotkeys.FixedRows then
  begin
    RememberSelectionGridRect := stgCommands.Selection;

    HotkeyItem := PHotkeyItem(stgHotkeys.Objects[0, stgHotkeys.Row]);
    sCommand := GetSelectedCommand;
    HMForm := HotMan.Forms.Find(GetSelectedForm);
    if Assigned(HMForm) then
    begin
      for i := 0 to HMForm.Controls.Count - 1 do
      begin
        HMControl := HMForm.Controls[i];
        if Assigned(HMControl) then
        begin
          hotkey := HMControl.Hotkeys.FindByContents(HotkeyItem^.Hotkey);
          if Assigned(hotkey) then
            HMControl.Hotkeys.Remove(hotkey);
        end;
      end;

      hotkey := HMForm.Hotkeys.FindByContents(HotkeyItem^.Hotkey);
      if Assigned(hotkey) then
        HMForm.Hotkeys.Remove(hotkey);

      // refresh lists
      Self.UpdateHotkeys(HMForm);
      Self.FillHotkeyList(sCommand);
      FModified := True;

      if stgCommands.CanFocus then stgCommands.SetFocus;
      stgCommands.Row := RememberSelectionGridRect.Top;
      bCanSelect := True;
      stgCommandsSelectCell(stgCommands, stgCommands.Selection.Left, stgCommands.Selection.Top, bCanSelect);
    end;
  end;
end;

{ TfrmOptionsHotkeys.Init }
procedure TfrmOptionsHotkeys.Init;
begin
  FModified := False;
  ParseLineToList(rsHotkeySortOrder, cbCommandSortOrder.Items);
  stgCommands.FocusRectVisible := False;
  stgCommands.SortOrder := soAscending; // Default initial sort ascending
  stgHotkeys.FocusRectVisible := False;
  // Localize Hotkeys.
  // stgCommands is localized in FillCommandList.
  stgHotkeys.Columns.Items[0].Title.Caption := rsOptHotkeysHotkey;
  stgHotkeys.Columns.Items[1].Title.Caption := rsOptHotkeysParameters;
  btnFileAction.Caption := '';
end;

{ TfrmOptionsHotkeys.Load }
procedure TfrmOptionsHotkeys.Load;
begin
  cbCommandSortOrder.ItemIndex := integer(gHotKeySortOrder);
  FillSCFilesList;
  FillCategoriesList;
  lbxCategoriesChange(lbxCategories);
end;

{ TfrmOptionsHotkeys.Save }
function TfrmOptionsHotkeys.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  // Save hotkeys file name.
  if lbSCFilesList.ItemIndex >= 0 then
    gNameSCFile := lbSCFilesList.Items[lbSCFilesList.ItemIndex];

  HotMan.Save(gpCfgDir + gNameSCFile);
end;

{ TfrmOptionsHotkeys.SelectHotkey }
procedure TfrmOptionsHotkeys.SelectHotkey(Hotkey: THotkey);
var
  HotkeyItem: PHotkeyItem;
  i: integer;
begin
  for i := stgHotkeys.FixedRows to stgHotkeys.RowCount - 1 do
  begin
    HotkeyItem := PHotkeyItem(stgHotkeys.Objects[0, i]);
    if Assigned(HotkeyItem) and HotkeyItem^.Hotkey.SameAs(Hotkey) then
    begin
      stgHotkeys.Row := i;
      Break;
    end;
  end;
end;

{ TfrmOptionsHotkeys.ShowEditHotkeyForm }
procedure TfrmOptionsHotkeys.ShowEditHotkeyForm(EditMode: boolean; aHotkeyRow: integer);
var
  HotkeyItem: PHotkeyItem;
begin
  HotkeyItem := PHotkeyItem(stgHotkeys.Objects[0, aHotkeyRow]);
  if Assigned(HotkeyItem) then
    ShowEditHotkeyForm(EditMode,
      GetSelectedForm,
      HotkeyItem^.Hotkey.Command,
      HotkeyItem^.Hotkey,
      HotkeyItem^.Controls);
end;

{ TfrmOptionsHotkeys.ShowEditHotkeyForm }
procedure TfrmOptionsHotkeys.ShowEditHotkeyForm(EditMode: boolean; const AForm: string; const ACommand: string; const AHotkey: THotkey; const AControls: TDynamicStringArray);
var
  HMForm: THMForm;
  Hotkey: THotkey = nil;
begin
  if AForm <> EmptyStr then
  begin
    if not Assigned(FEditForm) then
      FEditForm := TfrmEditHotkey.Create(Self);

    if FEditForm.Execute(EditMode, AForm, ACommand, AHotkey, AControls) then
    begin
      HMForm := HotMan.Forms.FindOrCreate(AForm);

      // refresh hotkey lists
      Self.UpdateHotkeys(HMForm);
      Self.FillHotkeyList(ACommand);

      Hotkey := FEditForm.CloneNewHotkey;
      try
        // Select the new shortcut in the hotkeys table.
        SelectHotkey(Hotkey);
      finally
        Hotkey.Free;
      end;

      FModified := True;
    end;
  end;
end;

{ TfrmOptionsHotkeys.Create }
constructor TfrmOptionsHotkeys.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FHotkeysCategories := TStringList.Create;
end;

{ TfrmOptionsHotkeys.Destroy }
destructor TfrmOptionsHotkeys.Destroy;
begin
  inherited Destroy;
  FHotkeysCategories.Free;
end;

{ TfrmOptionsHotkeys.AddDeleteWithShiftHotkey }
procedure TfrmOptionsHotkeys.AddDeleteWithShiftHotkey(UseTrash: boolean);
  procedure ReverseShift(Hotkey: THotkey; out Shortcut: TShortCut; out TextShortcut: string);
  var
    ShiftState: TShiftState;
  begin
    Shortcut := TextToShortCutEx(Hotkey.Shortcuts[0]);
    ShiftState := ShortcutToShiftEx(Shortcut);
    if ssShift in ShiftState then
      ShiftState := ShiftState - [ssShift]
    else
      ShiftState := ShiftState + [ssShift];
    ShortCut := KeyToShortCutEx(Shortcut, ShiftState);
    TextShortcut := ShortCutToTextEx(Shortcut);
  end;

  function ConfirmFix({%H-}Hotkey: THotkey; const Msg: string): boolean;
  begin
    Result := QuestionDlg(rsOptHotkeysCannotSetShortcut, Msg, mtConfirmation, [mrYes, rsOptHotkeysFixParameter, 'isdefault', mrCancel], 0) = mrYes;
  end;

  function FixOverrides(Hotkey: THotkey; const OldTrashParam: string; NewTrashParam: boolean; ShouldUseTrash: boolean): boolean;
  begin
    if Contains(Hotkey.Params, OldTrashParam) or NewTrashParam then
    begin
      Result := ConfirmFix(Hotkey, Format(rsOptHotkeysDeleteTrashCanOverrides, [Hotkey.Shortcuts[0]]));
      if Result then
      begin
        DeleteString(Hotkey.Params, OldTrashParam);
        if ShouldUseTrash then
          SetValue(Hotkey.Params, 'trashcan', 'setting')
        else
          SetValue(Hotkey.Params, 'trashcan', 'reversesetting');
      end;
    end
    else
      Result := True;
  end;

  procedure FixReversedShortcut(Hotkey: THotkey; NonReversedHotkey: THotkey; const ParamsToDelete: array of string; const AllowedOldParam: string; const NewTrashParam: string; HasTrashCan: boolean; TrashStr: string);
  var
    sDelete: string;
  begin
    if ContainsOneOf(Hotkey.Params, ParamsToDelete) or (HasTrashCan and (TrashStr <> NewTrashParam)) then
      if not ConfirmFix(Hotkey, Format(rsOptHotkeysDeleteTrashCanParameterExists, [Hotkey.Shortcuts[0], NonReversedHotkey.Shortcuts[0]])) then
        Exit;

    for sDelete in ParamsToDelete do
      DeleteString(Hotkey.Params, sDelete);
    if not Contains(Hotkey.Params, AllowedOldParam) then
      SetValue(Hotkey.Params, 'trashcan', NewTrashParam);
  end;

  procedure AddShiftShortcut(Hotkeys: THotkeys);
  var
    i, j: integer;
    Shortcut: TShortCut;
    TextShortcut: string;
    NewParams: array of string;
    HasTrashCan, HasTrashBool, NormalTrashSetting: boolean;
    TrashStr: string;
    TrashBoolValue: boolean;
    CheckedShortcuts: TDynamicStringArray;
    ReversedHotkey: THotkey;
    CountBeforeAdded: integer;
    SetShortcut: boolean;
  begin
    SetLength(CheckedShortcuts, 0);
    CountBeforeAdded := Hotkeys.Count;
    for i := 0 to CountBeforeAdded - 1 do
    begin
      if (Hotkeys[i].Command = 'cm_Delete') and (Length(Hotkeys[i].Shortcuts) > 0) then
      begin
        if Length(Hotkeys[i].Shortcuts) > 1 then
        begin
          MessageDlg(rsOptHotkeysCannotSetShortcut,
            Format(rsOptHotkeysShortcutForDeleteIsSequence, [ShortcutsToText(Hotkeys[i].Shortcuts)]),
            mtWarning, [mbOK], 0);
          Continue;
        end;

        if not Contains(CheckedShortcuts, Hotkeys[i].Shortcuts[0]) then
        begin
          ReversedHotkey := nil;
          SetShortcut := True;
          ReverseShift(Hotkeys[i], Shortcut, TextShortcut);
          AddString(CheckedShortcuts, TextShortcut);

          // Check if shortcut with reversed shift already exists.
          for j := 0 to CountBeforeAdded - 1 do
          begin
            if ArrBegins(Hotkeys[j].Shortcuts, [TextShortcut], False) then
            begin
              if Hotkeys[j].Command <> Hotkeys[i].Command then
              begin
                if QuestionDlg(rsOptHotkeysCannotSetShortcut, Format(rsOptHotkeysShortcutForDeleteAlreadyAssigned, [Hotkeys[i].Shortcuts[0], TextShortcut, Hotkeys[j].Command]),
                  mtConfirmation, [mrYes, rsOptHotkeysChangeShortcut, 'isdefault', mrCancel], 0) = mrYes then
                begin
                  Hotkeys[j].Command := Hotkeys[i].Command;
                end
                else
                  SetShortcut := False;
              end;

              ReversedHotkey := Hotkeys[j];
              Break;
            end;
          end;

          if not SetShortcut then
            Continue;

          // Fix parameters of original hotkey if needed.
          HasTrashCan := GetParamValue(Hotkeys[i].Params, 'trashcan', TrashStr);
          HasTrashBool := HasTrashCan and GetBoolValue(TrashStr, TrashBoolValue);
          if not FixOverrides(Hotkeys[i], 'recycle', HasTrashBool and TrashBoolValue, UseTrash) then
            Continue;
          if not FixOverrides(Hotkeys[i], 'norecycle', HasTrashBool and not TrashBoolValue, not UseTrash) then
            Continue;

          // Reverse trash setting for reversed hotkey.
          NewParams := Copy(Hotkeys[i].Params);
          HasTrashCan := GetParamValue(NewParams, 'trashcan', TrashStr); // Could have been added above so check again
          if Contains(NewParams, 'recyclesettingrev') then
          begin
            DeleteString(NewParams, 'recyclesettingrev');
            NormalTrashSetting := True;
          end
          else if Contains(NewParams, 'recyclesetting') then
          begin
            DeleteString(NewParams, 'recyclesetting');
            NormalTrashSetting := False;
          end
          else if HasTrashCan and (TrashStr = 'reversesetting') then
            NormalTrashSetting := True
          else
            NormalTrashSetting := False;

          if Assigned(ReversedHotkey) then
          begin
            HasTrashCan := GetParamValue(ReversedHotkey.Params, 'trashcan', TrashStr);

            if NormalTrashSetting then
            begin
              FixReversedShortcut(ReversedHotkey, Hotkeys[i],
                ['recyclesettingrev', 'recycle', 'norecycle'],
                'recyclesetting', 'setting', HasTrashCan, TrashStr);
            end
            else
            begin
              FixReversedShortcut(ReversedHotkey, Hotkeys[i],
                ['recyclesetting', 'recycle', 'norecycle'],
                'recyclesettingrev', 'reversesetting', HasTrashCan, TrashStr);
            end;
          end
          else if QuestionDlg(rsOptHotkeysSetDeleteShortcut, Format(rsOptHotkeysAddDeleteShortcutLong, [TextShortcut]), mtConfirmation, [mrYes, rsOptHotkeysAddShortcutButton, 'isdefault', mrCancel], 0) = mrYes then
          begin
            if NormalTrashSetting then
              TrashStr := 'setting'
            else
              TrashStr := 'reversesetting';
            SetValue(NewParams, 'trashcan', TrashStr);

            Hotkeys.Add([TextShortcut], NewParams, Hotkeys[i].Command);
          end;
        end;
      end;
    end;
  end;

var
  HMForm: THMForm;
  I: integer;
begin
  HMForm := HotMan.Forms.Find('Main');
  if Assigned(HMForm) then
  begin
    AddShiftShortcut(HMForm.Hotkeys);
    for I := 0 to HMForm.Controls.Count - 1 do
      AddShiftShortcut(HMForm.Controls[i].Hotkeys);
    // Refresh hotkeys list.
    if GetSelectedCommand = 'cm_Delete' then
      Self.FillHotkeyList('cm_Delete');
  end;
end;

{ TfrmOptionsHotkeys.TryToSelectThatCategory }
procedure TfrmOptionsHotkeys.TryToSelectThatCategory(sCategory: string);
var
  iCategoryIndex: integer;
begin
  iCategoryIndex := lbxCategories.Items.IndexOf(sCategory);
  if iCategoryIndex <> -1 then
  begin
    lbxCategories.ItemIndex := iCategoryIndex;
    lbxCategoriesChange(lbxCategories);
  end;
end;

{ TfrmOptionsHotkeys.cbCommandSortOrderChange }
procedure TfrmOptionsHotkeys.cbCommandSortOrderChange(Sender: TObject);
begin
  if THotKeySortOrder(cbCommandSortOrder.ItemIndex) <> gHotKeySortOrder then
  begin
    if (THotKeySortOrder(cbCommandSortOrder.ItemIndex) = hksoByHotKeyOnePerRow) or (gHotKeySortOrder = hksoByHotKeyOnePerRow) then
      FillCommandList(edtFilter.Text)
    else
      stgCommands.SortColRow(True, cbCommandSortOrder.ItemIndex); //hksoByCommand=0=column0=command hksoByHotKeyGrouped=1=column1=hotkey
  end;
  gHotKeySortOrder := THotKeySortOrder(cbCommandSortOrder.ItemIndex);
end;

{ TfrmOptionsHotkeys.isOkToContinueRegardingModifiedOrNot }
function TfrmOptionsHotkeys.isOkToContinueRegardingModifiedOrNot: boolean;
var
  Answer: TMyMsgResult;
begin
  Result := True;
  if FModified then
  begin
    Answer := MsgBox(Format(rsHotKeyFileSaveModified, [gNameSCFile]), [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
    case Answer of
      mmrYes: HotMan.Save(gpCfgDir + gNameSCFile);
      mmrCancel: Result := False;
    end;
  end;
end;

{ TfrmOptionsHotkeys.GetANewSetupName }
function TfrmOptionsHotkeys.GetANewSetupName(var ASetupName: string): boolean;
var
  sSuggestedName: string;
  Answer: TMyMsgResult = mmrCancel;
begin
  Result := False;

  repeat
    sSuggestedName := ASetupName;
    if InputQuery(rsHotKeyFileNewName, rsHotKeyFileInputNewName, sSuggestedName) then
    begin
      Result := not mbFileExists(gpCfgDir + sSuggestedName);
      if not Result then
      begin
        Answer := MsgBox(rsHotKeyFileAlreadyExists, [msmbYes, msmbNo, msmbCancel], msmbCancel, msmbCancel);
        Result := (Answer = mmrYes);
      end;
    end;
  until (Result) or (Answer = mmrCancel);

  if Result then ASetupName := sSuggestedName;
end;

{ GetSortableShortcutName }
// Will return a string representing the shortcut.
// The string will have a prefix that it will help to sort it in such way that the shortcut will appear in this order, from the first to the last:
//   -:Fx (with F9 arranged to be shown prior F10)
//   -:ALT+Fx
//   -:CTRL+Fx
//   -:SHIFT+Fx
//   -:CTRL+SHIFT+Fx
//   -:Single letter stuff
//   -:CTRL+Letter
//   -:SHIFT+Letter
//   -:CTRL+SHIFT+Letter
function GetSortableShortcutName(sToSort: string): string;
var
  posSemiColon, i: integer;
  sFollowing: string;
  sAbsolute: string;
  sShifted: string;
  posPlus1, posPlus2: integer;
  icombine: integer = 0;
  isFxKey: boolean;
  iPrefix: word;
begin
  if length(sToSort) > 1 then
    if sToSort[length(sToSort)] = '+' then sToSort[length(sToSort)] := ',';

  //1o) We get the first shortcut string in case there are many.
  posSemiColon := pos(';', sToSort);
  if posSemiColon <> 0 then sToSort := leftstr(sToSort, pred(posSemiColon));

  //2o) Make sure we're in uppercase
  sToSort := UpperCase(sToSort);

  //3o) Let's arrange things so F9 will be coded F09 so it will easily be sortable prior F10 instead of being after.
  i := 1;
  while (i <= length(sToSort)) do
  begin
    if pos(sToSort[i], '0123456789') <> 0 then
    begin
      sFollowing := copy(sToSort, succ(i), 1);
      if pos(sFollowing, '0123456789') = 0 then sToSort := copy(sToSort, 1, pred(i)) + '0' + rightstr(sToSort, (length(sToSort) - pred(i)));
      Inc(i);
    end;
    Inc(i);
  end;

  //4o) Let's see if we have combined keys (CTRL+..., SHIFT+..., CTRL+SHIFT+....)
  posPlus1 := pos('+', sToSort);
  posPlus2 := UTF8Pos('+', sToSort, succ(PosPlus1));
  if posPlus1 <> 0 then
    if posPlus2 = 0 then iCombine := 1
    else iCombine := 2;

  //5o) Let's extract the unshifted absolute keys
  case iCombine of
    0: sAbsolute := sToSort;
    1: sAbsolute := copy(sToSort, succ(posPlus1), (length(sToSort) - posPlus1));
    2: sAbsolute := copy(sToSort, succ(posPlus2), (length(sToSort) - posPlus2));
  end;

  case iCombine of
    0: sShifted := '';
    1: sShifted := copy(sToSort, 1, pred(posPlus1));
    2: sShifted := copy(sToSort, 1, pred(posPlus2));
  end;

  isFxKey := (pos('F', sAbsolute) = 1) and (length(sAbsolute) > 1);

  iPrefix := 0;
  if (not isFxKey) then iPrefix := iPrefix or $100; //Make sure if it's a "Fx" key, it appear FIRST
  if length(sAbsolute) > 1 then iPrefix := iPrefix or $01;

  case iCombine of
    0:
    begin
    end;

    1:
    begin
      if pos('ALT', sShifted) = 1 then iPrefix := (iPrefix or $02)
      else if pos('CTRL', sShifted) = 1 then iPrefix := (iPrefix or $04)
      else if pos('SHIFT', sShifted) = 1 then iPrefix := (iPrefix or $08);
    end;

    2:
    begin
      if pos('CTRL+ALT', sShifted) = 1 then iPrefix := (iPrefix or $10) else
      if pos('CTRL+SHIFT', sShifted) = 1 then iPrefix := (iPrefix or $20) else
      if pos('SHIFT+ALT', sShifted) = 1 then iPrefix := (iPrefix or $40);
    end;
  end;

  Result := Format('%4.4d%s', [iPrefix, sAbsolute]);
end;

{ TfrmOptionsHotkeys.stgCommandsCompareCells }
// Add a word about "iSecondLevelSort"
procedure TfrmOptionsHotkeys.stgCommandsCompareCells(Sender: TObject; ACol, ARow, BCol, BRow: integer; var Result: integer);
var
  sText1, sText2: string;
  iSecondLevelSort: boolean = False;
begin
  if ACol and $80 <> 0 then
  begin
    iSecondLevelSort := True;
    ACol := Acol and $7F;
  end;

  sText1 := TStringGrid(Sender).Cells[ACol, ARow];
  sText2 := TStringGrid(Sender).Cells[BCol, BRow];

  if aCol = stgCmdHotkeysIndex then
  begin
    if (sText1 = '') then
    begin
      if (sText2 = '') then
      begin
        if not iSecondLevelSort then
          stgCommandsCompareCells(Sender, stgCmdCommandIndex or $80, ARow, stgCmdCommandIndex, BRow, Result)
        else
          Result := 0;
      end
      else
        Result := 1;
    end
    else
    begin
      if (sText2 = '') then
      begin
        Result := -1;
      end
      else
      begin
        sText1 := GetSortableShortcutName(sText1);
        sText2 := GetSortableShortcutName(sText2);
        case TStringGrid(Sender).SortOrder of
          soAscending: Result := CompareText(sText1, sText2);
          soDescending: Result := CompareText(sText2, sText1);
        end;
      end;
    end;
  end
  else
  begin
    case TStringGrid(Sender).SortOrder of
      soAscending: Result := CompareText(sText1, sText2);
      soDescending: Result := CompareText(sText2, sText1);
    end;

    if (Result = 0) and (not iSecondLevelSort) then
      stgCommandsCompareCells(Sender, stgCmdHotkeysIndex or $80, ARow, stgCmdHotkeysIndex, BRow, Result);
  end;
end;

{ TfrmOptionsHotkeys.stgCommandsHeaderClick }
procedure TfrmOptionsHotkeys.stgCommandsHeaderClick(Sender: TObject; IsColumn: boolean; Index: integer);
var
  iInitialIndex: integer;
begin
  iInitialIndex := cbCommandSortOrder.ItemIndex;

  if (isColumn) then
  begin
    if (Index = stgCmdCommandIndex) and (THotKeySortOrder(cbCommandSortOrder.ItemIndex) <> hksoByCommand) then
      cbCommandSortOrder.ItemIndex := integer(hksoByCommand)
    else
    if (Index = stgCmdHotkeysIndex) then
    begin
      if (THotKeySortOrder(cbCommandSortOrder.ItemIndex) = hksoByCommand) then
        cbCommandSortOrder.ItemIndex := integer(hksoByHotKeyGrouped)
      else
        cbCommandSortOrder.ItemIndex := 3 - cbCommandSortOrder.ItemIndex;
    end;
  end;

  if iInitialIndex <> cbCommandSortOrder.ItemIndex then
    cbCommandSortOrderChange(cbCommandSortOrder);
end;

{ TfrmOptionsHotkeys.actSaveNowExecute }
procedure TfrmOptionsHotkeys.actSaveNowExecute(Sender: TObject);
begin
  HotMan.Save(gpCfgDir + gNameSCFile);
  FModified := False;
end;

{ TfrmOptionsHotkeys.actAdjustSortOrderExecute }
procedure TfrmOptionsHotkeys.actAdjustSortOrderExecute(Sender: TObject);
begin
  cbCommandSortOrder.ItemIndex := TComponent(Sender).Tag;
  cbCommandSortOrderChange(cbCommandSortOrder);
end;

{ RemoveSCFextension }
function RemoveSCFextension(sBaseName: string): string;
begin
  Result := StringReplace(sBaseName, '.scf', '', [rfIgnoreCase, rfReplaceAll]);
end;

{ TfrmOptionsHotkeys.actRenameExecute }
procedure TfrmOptionsHotkeys.actRenameExecute(Sender: TObject);
begin
  actCopyExecute(Sender);
end;

{ TfrmOptionsHotkeys.actCopyExecute }
procedure TfrmOptionsHotkeys.actCopyExecute(Sender: TObject);
var
  sSetupName, sOldFilename: string;
begin
  if isOkToContinueRegardingModifiedOrNot then
  begin
    sSetupName := RemoveSCFextension(Format(rsHotKeyFileCopyOf, [gNameSCFile]));
    if GetANewSetupName(sSetupName) then
    begin
      sOldFilename := gNameSCFile;
      gNameSCFile := RemoveSCFextension(sSetupName) + '.scf';
      HotMan.Save(gpCfgDir + gNameSCFile);
      if TAction(Sender).Tag = 1 then mbDeletefile(gpCfgDir + sOldFilename);
      FillSCFilesList;
      FillCategoriesList;
      lbxCategoriesChange(lbxCategories);
    end;
  end;
end;

{ TfrmOptionsHotkeys.actDeleteExecute }
procedure TfrmOptionsHotkeys.actDeleteExecute(Sender: TObject);
begin
  if lbSCFilesList.Items.Count > 1 then
  begin
    if MsgBox(Format(rsHotKeyFileConfirmErasure, [RemoveSCFextension(lbSCFilesList.Text)]), [msmbYes, msmbCancel], msmbCancel, msmbCancel) = mmrYes then
    begin
      if mbFileExists(gpCfgDir + lbSCFilesList.Items[lbSCFilesList.ItemIndex]) then
        mbDeleteFile(gpCfgDir + lbSCFilesList.Items[lbSCFilesList.ItemIndex]);
      FillSCFilesList;
      FillCategoriesList;
      lbxCategoriesChange(lbxCategories);
    end;
  end
  else
  begin
    MsgError(rsHotKeyFileMustKeepOne);
  end;
end;

{ TfrmOptionsHotkeys.actRestoreDefaultExecute }
procedure TfrmOptionsHotkeys.actRestoreDefaultExecute(Sender: TObject);
begin
  if isOkToContinueRegardingModifiedOrNot then
  begin
    if MsgBox(rsHotKeyFileConfirmDefault, [msmbYes, msmbCancel], msmbCancel, msmbCancel) = mmrYes then
    begin
      DeleteAllHotkeys;
      LoadDefaultHotkeyBindings;
      HotMan.Save(gpCfgDir + gNameSCFile);
      HotMan.Load(gpCfgDir + gNameSCFile);
      FillCategoriesList;
      lbxCategoriesChange(lbxCategories);
    end;
  end;
end;

{ TfrmOptionsHotkeys.actDeleteHotKeyExecute }
procedure TfrmOptionsHotkeys.actDeleteHotKeyExecute(Sender: TObject);
begin
  DeleteHotkey;
end;

{ TfrmOptionsHotkeys.actEditHotKeyExecute }
procedure TfrmOptionsHotkeys.actEditHotKeyExecute(Sender: TObject);
begin
  ShowEditHotkeyForm(True, stgHotkeys.Row);
end;

{ TfrmOptionsHotkeys.actNextCategoryExecute }
procedure TfrmOptionsHotkeys.actNextCategoryExecute(Sender: TObject);
begin
  if lbxCategories.ItemIndex < pred(lbxCategories.Items.Count) then
    lbxCategories.ItemIndex := lbxCategories.ItemIndex + 1
  else
    lbxCategories.ItemIndex := 0;
  lbxCategoriesChange(lbxCategories);
end;

{ TfrmOptionsHotkeys.actPopupFileRelatedMenuExecute }
procedure TfrmOptionsHotkeys.actPopupFileRelatedMenuExecute(Sender: TObject);
var
  TargetPopUpMenuPos: TPoint;
begin
  TargetPopUpMenuPos := Self.ClientToScreen(Classes.Point(btnFileAction.Left + (btnFileAction.Width div 2), btnFileAction.Height + (btnFileAction.Height div 2)));
  pmShortCutMenu.PopUp(TargetPopUpMenuPos.x, TargetPopUpMenuPos.y);
end;

{ TfrmOptionsHotkeys.actPreviousCategoryExecute }
procedure TfrmOptionsHotkeys.actPreviousCategoryExecute(Sender: TObject);
begin
  if lbxCategories.ItemIndex > 0 then
    lbxCategories.ItemIndex := lbxCategories.ItemIndex - 1
  else
    lbxCategories.ItemIndex := pred(lbxCategories.Items.Count);
  lbxCategoriesChange(lbxCategories);
end;

end.

