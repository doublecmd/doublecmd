{
   Double Commander
   -------------------------------------------------------------------------
   Hotkeys options page

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

unit fOptionsHotkeys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Grids,
  fOptionsFrame, uHotkeyManager;

type

  { TfrmOptionsHotkeys }

  TfrmOptionsHotkeys = class(TOptionsEditor)
    btClearHotKey: TButton;
    btSetHotKey: TButton;
    cgHKControls: TCheckGroup;
    edtHotKey: TEdit;
    edtFilter: TEdit;
    edtParam: TEdit;
    lblCommands: TLabel;
    lbFilter: TLabel;
    lblHotKey: TLabel;
    lblParam: TLabel;
    lblSCFiles: TLabel;
    lblHotKeyConflict: TLabel;
    lbSCFilesList: TListBox;
    lblCategories: TLabel;
    lbxCategories: TListBox;
    pnlHotkeyButtons: TPanel;
    stgCommands: TStringGrid;
    stgHotkeys: TStringGrid;
    procedure btClearHotKeyClick(Sender: TObject);
    procedure btSetHotKeyClick(Sender: TObject);
    procedure cgHKControlsItemClick(Sender: TObject; Index: Integer);
    procedure edtFilterChange(Sender: TObject);
    procedure edtHotKeyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtHotKeyKeyPress(Sender: TObject; var Key: char);
    procedure lbSCFilesListSelectionChange(Sender: TObject; User: boolean);
    procedure lbxCategoriesSelectionChange(Sender: TObject; User: boolean);
    procedure stgCommandsResize(Sender: TObject);
    procedure stgCommandsSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure stgHotkeysResize(Sender: TObject);
    procedure stgHotkeysSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  private
    FHotkeysAutoColWidths: array of Integer;
    FHotkeysAutoGridWidth: Integer;
    FHotkeysCategories: TStringList; // Untranslated
    procedure AutoSizeCommandsGrid;
    procedure AutoSizeHotkeysGrid;
    procedure DeleteHotkeyFromGrid(aHotkey: String);
    {en
       Refreshes all hotkeys from the Commands grid
    }
    procedure UpdateHotkeys(HMForm: THMForm);
    procedure UpdateHotkeysForCommand(HMForm: THMForm; RowNr: Integer);
    procedure FillSCFilesList;
    {en
       Return hotkeys assigned for command for the form and its controls.
    }
    procedure GetHotKeyList(HMForm: THMForm; Command: String; HotkeysList: TStringList);
    {en
       Check if combination of pressed hotkey and checked controls are already in use.
       Conflicting hotkeys are deleted if DeleteConflicts parameter is true.
    }
    procedure CheckHotKeyConflicts(DeleteConflicts: Boolean = false);
    {en
       Fill hotkey grid with all hotkeys assigned to a command
    }
    procedure FillHotkeyList(sCommand: String);
    {en
       Add form controls to HKControl checkbox list
    }
    procedure FillHKControlList();
    {en
       Fill Commands grid with all commands available for the selected category.
       @param(Filter
              If not empty string then shows only commands containing Filter string.)
    }
    procedure FillCommandList(Filter: String);
    procedure FillCategoriesList;
    {en
       Retrieves untranslated category.
    }
    function GetSelectedCategory: String;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  Forms, Controls, Dialogs, LCLProc,
  uFindEx, uGlobs, uGlobsPaths, uLng, uTypes, uKeyboard, uFormCommands;

const
  stgCmdCommandIndex = 0;
  stgCmdHotkeysIndex = 1;
  stgCmdCommentIndex = 2;

function StListToStr(separator:string; const lStList:TStringList; duplicates: boolean = true):string;
//< convert stringlist to string
var
  sLast: String;
  i: Integer;
begin
  Result:='';
  if lStList.Count>0 then
  begin
    if not duplicates then
      lStList.Sort;

    sLast := lStList[0];

    Result:=lStList[0]+separator;
    for i:=1 to lStList.Count-1 do
    begin
      if not duplicates and (lStList[i] = sLast) then
        continue;

      sLast := lStList[i];

      Result:=Result+lStList[i]+separator;
    end;
  end;
end;

function CompareCategories(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := UTF8CompareText(List.Strings[Index1], List.Strings[Index2]);
end;

{ TfrmOptionsHotkeys }

procedure TfrmOptionsHotkeys.btClearHotKeyClick(Sender: TObject);
var
  i: Integer;
  sShortCut: String;
  sCommand: String;
  HMForm: THMForm;
  HMControl: THMControl;
  hotkey: THotkey;
begin
  if lbxCategories.ItemIndex=-1 then Exit;
  sShortCut := stgHotkeys.Cells[0, stgHotkeys.Row];
  sCommand := stgCommands.Cells[stgCmdCommandIndex, stgCommands.Row];
  HMForm := HotMan.Forms.Find(GetSelectedCategory);
  if Assigned(HMForm) then
  begin
    for i := 0 to HMForm.Controls.Count - 1 do
    begin
      HMControl := HMForm.Controls[i];
      if Assigned(HMControl) then
      begin
        hotkey := HMControl.Hotkeys.Find(sShortCut);
        if Assigned(hotkey) and (hotkey.Command = sCommand) then
          HMControl.Hotkeys.Remove(hotkey);
      end;
    end;

    hotkey := HMForm.Hotkeys.Find(sShortCut);
    if Assigned(hotkey) and (hotkey.Command = sCommand) then
      HMForm.Hotkeys.Remove(hotkey);

    // refresh lists
    Self.UpdateHotkeys(HMForm);
    Self.FillHotkeyList(sCommand);
  end;
end;

procedure TfrmOptionsHotkeys.btSetHotKeyClick(Sender: TObject);
var
  i: Integer;
  sCommand: string;
  sShortCut, sParam: String;
  HMForm: THMForm;
  HMControl: THMControl;
  hotkey: THotkey;
  isFormHotkey: Boolean;

begin
// ToDo: Black list HotKey which can't use

  if lbxCategories.ItemIndex=-1 then Exit;
  if stgCommands.Row<1 then Exit;

  sShortCut := edtHotKey.Text;

  // check for invalid hotkey
  if sShortCut = EmptyStr then
    Exit;

  sParam := edtParam.Text;
  sCommand := stgCommands.Cells[stgCmdCommandIndex, stgCommands.Row];

  if (lblHotKeyConflict.Caption <> EmptyStr) then
  begin
    if (MessageDlg(rsOptHotkeysShortCutUsed,                                     // delete command on assigned shortcut
                   Format(rsOptHotkeysShortCutUsedText1,                         // if another was applied
                          [sShortCut]) + LineEnding +
                   Format(rsOptHotkeysShortCutUsedText2,
                          [sCommand]),
                   mtConfirmation, mbYesNo, 0) = mrYes) then
      CheckHotKeyConflicts(true)
    else
      Exit;
  end;

  HMForm := HotMan.Forms.FindOrCreate(GetSelectedCategory);
  isFormHotkey := true;
  for i := 0 to Self.cgHKControls.Items.Count - 1 do
  begin
    HMControl := THMControl(Self.cgHKControls.Items.Objects[i]);
    if not Assigned(HMControl) then
      continue;

    // delete previous hotkey if exists
    hotkey := HMControl.Hotkeys.Find(sShortCut);
    if Assigned(hotkey) and (hotkey.Command = sCommand) then
      HMControl.Hotkeys.Remove(hotkey);

    // add new hotkey
    if Self.cgHKControls.Checked[i] then
    begin
      isFormHotkey := false;

      HMControl.Hotkeys.Add(sShortCut, sCommand, sParam);
    end;
  end;

  // delete previous hotkey if exists
  hotkey := HMForm.Hotkeys.Find(sShortCut);
  if Assigned(hotkey) and (hotkey.Command = sCommand) then
    HMForm.Hotkeys.Remove(hotkey);

  if isFormHotkey then
    HMForm.Hotkeys.Add(sShortCut, sCommand, sParam);

  // refresh hotkey lists
  Self.UpdateHotkeys(HMForm);
  Self.FillHotkeyList(sCommand);

  // Select the new shortcut in the hotkeys table.
  stgHotkeys.Row := stgHotkeys.Cols[0].IndexOf(sShortCut);
end;

procedure TfrmOptionsHotkeys.cgHKControlsItemClick(Sender: TObject; Index: Integer);
begin
  CheckHotKeyConflicts();
end;

procedure TfrmOptionsHotkeys.edtFilterChange(Sender: TObject);
{< filtering active commands list}
begin
  if lbxCategories.ItemIndex=-1 then Exit;
  edtHotKey.Clear;
  FillCommandList(edtFilter.Text);
end;

procedure TfrmOptionsHotkeys.edtHotKeyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ShortCut: TShortCut;
begin
  ShortCut := KeyToShortCutEx(Key,GetKeyShiftStateEx);
  edtHotKey.Text := ShortCutToTextEx(ShortCut);
  Key := 0;
  btSetHotKey.Enabled := (edtHotKey.Text <> '');
  lblHotKeyConflict.Caption:='';
  btClearHotKey.Enabled := (edtHotKey.Text <> '');
  cgHKControls.Enabled := btSetHotKey.Enabled;

  CheckHotKeyConflicts();
end;

procedure TfrmOptionsHotkeys.edtHotKeyKeyPress(Sender: TObject; var Key: char);
begin
  Key := #0;
  edtHotKey.Text := '';
  btSetHotKey.Enabled := False;
end;

procedure TfrmOptionsHotkeys.lbSCFilesListSelectionChange(Sender: TObject; User: boolean);
begin
  if lbSCFilesList.ItemIndex >= 0 then
  begin
    HotMan.Load(gpCfgDir + lbSCFilesList.Items[lbSCFilesList.ItemIndex]);
    FillCategoriesList;
  end;
end;

procedure TfrmOptionsHotkeys.lbxCategoriesSelectionChange(Sender: TObject; User: boolean);
begin
  if lbxCategories.ItemIndex=-1 then Exit;

  Self.FillHKControlList();

  edtFilter.Clear;
  FillCommandList('');
end;

procedure TfrmOptionsHotkeys.stgCommandsResize(Sender: TObject);
begin
  AutoSizeCommandsGrid;
end;

procedure TfrmOptionsHotkeys.stgCommandsSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  // < find hotkeys for command
var
  selcmd: String;
  HMForm: THMForm;
begin
  // clears all controls
  btSetHotKey.Enabled := False;
  btClearHotKey.Enabled := False;
  edtHotKey.Clear;
  lblHotKeyConflict.Caption:='';
  cgHKControls.Enabled := False;
  stgHotkeys.RowCount := stgHotkeys.FixedRows;
  if aRow<1 then
    Exit;

  HMForm := HotMan.Forms.Find(GetSelectedCategory);
  if not Assigned(HMForm) then
    Exit;

  selcmd:=stgCommands.Cells[stgCmdCommandIndex,aRow];// get selected command

  FillHotkeyList(selcmd);
end;

procedure TfrmOptionsHotkeys.stgHotkeysResize(Sender: TObject);
begin
  AutoSizeHotkeysGrid;
end;

procedure TfrmOptionsHotkeys.stgHotkeysSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var
  controlList: TStringList;
  i, j: Integer;
begin
  if aRow < stgHotkeys.FixedRows then
    Exit;

  edtHotKey.Text := stgHotkeys.Cells[0, aRow];
  edtParam.Text := stgHotkeys.Cells[1, aRow];
  btSetHotKey.Enabled := true;
  btClearHotKey.Enabled := true;
  lblHotKeyConflict.Caption:='';
  cgHKControls.Enabled := true;

  // check objects to which hotkey applies
  controlList := TStringList.Create();
  try
    controlList.Delimiter := ';';
    controlList.StrictDelimiter := True;
    controlList.DelimitedText := stgHotkeys.Cells[2, aRow];

    for i := 0 to Self.cgHKControls.Items.Count - 1 do
    begin
      Self.cgHKControls.Checked[i] := false;

      // last string of list is always be empty, so [Count - 2]
      for j := 0 to controlList.Count - 2 do
        if Self.cgHKControls.Items[i] = controlList[j] then
        begin
          Self.cgHKControls.Checked[i] := true;
          break;
        end;
    end;

  finally
    FreeAndNil(controlList);
  end;
end;

procedure TfrmOptionsHotkeys.AutoSizeCommandsGrid;
begin
  with stgCommands do
  begin
    AutoSizeColumns;
    if ClientWidth > GridWidth then
      ColWidths[stgCmdCommentIndex] := ColWidths[stgCmdCommentIndex] + (ClientWidth - GridWidth);
  end;
end;

procedure TfrmOptionsHotkeys.AutoSizeHotkeysGrid;
var
  Diff: Integer = 0;
  i: Integer;
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

procedure TfrmOptionsHotkeys.DeleteHotkeyFromGrid(aHotkey: String);
var
  i: Integer;
begin
  for i := stgHotkeys.FixedRows to stgHotkeys.RowCount - 1 do
    if stgHotkeys.Cells[0, i] = aHotkey then
    begin
      stgHotkeys.DeleteColRow(False, i);
      Break;
    end;
end;

procedure TfrmOptionsHotkeys.UpdateHotkeys(HMForm: THMForm);
var
  i: Integer;
begin
  for i := Self.stgCommands.FixedRows to Self.stgCommands.RowCount - 1 do
    Self.UpdateHotkeysForCommand(HMForm, i);
end;

procedure TfrmOptionsHotkeys.UpdateHotkeysForCommand(HMForm: THMForm; RowNr: Integer);
var
  lslHotKeys: TStringList;
begin
  lslHotKeys:=TStringList.Create;
  GetHotKeyList(HMForm, stgCommands.Cells[stgCmdCommandIndex,RowNr],lslHotKeys);
  stgCommands.Cells[stgCmdHotkeysIndex,RowNr]:=StListToStr(';',lslHotKeys,false);
  lslHotKeys.Free;
end;

procedure TfrmOptionsHotkeys.FillSCFilesList;
var
  SR : TSearchRecEx;
  Res : Integer;
begin
  lbSCFilesList.Items.Clear;
  Res := FindFirstEx(gpCfgDir + '*.scf', faAnyFile, SR);
  while Res = 0 do
  begin
    Res:= lbSCFilesList.Items.Add(Sr.Name);
    if Sr.Name = gNameSCFile then
      lbSCFilesList.Selected[Res] := True;
    Res := FindNextEx(SR);
  end;
  FindCloseEx(SR);
end;

procedure TfrmOptionsHotkeys.GetHotKeyList(HMForm: THMForm; Command: String; HotkeysList: TStringList);
  procedure AddHotkeys(hotkeys: THotkeys);
  var
    i: Integer;
  begin
    for i := 0 to hotkeys.Count - 1 do
    begin
      if hotkeys[i].Command = Command then
        HotkeysList.AddObject(hotkeys[i].Shortcut, hotkeys[i]);
    end;
  end;
var
  i: Integer;
begin
  AddHotkeys(HMForm.Hotkeys);
  for i := 0 to HMForm.Controls.Count - 1 do
    AddHotkeys(HMForm.Controls[i].Hotkeys);
end;

procedure TfrmOptionsHotkeys.CheckHotKeyConflicts(DeleteConflicts: Boolean);
  procedure AddConflictHint(ACommand, AName: String);
  var
    s: String = '';
  begin
    if lblHotKeyConflict.Hint <> '' then
      s := LineEnding;
    lblHotKeyConflict.Hint := lblHotKeyConflict.Hint + s +
        Format(rsOptHotkeysUsedBy, [ACommand, AName]);
  end;
var
  HMForm: THMForm;
  HMControl: THMControl;
  sShortCut: String;
  hotkey: THotkey;
  i, count: Integer;
  isFormHotKey: Boolean;
  sCommand: String;
begin
  lblHotKeyConflict.Caption := EmptyStr;
  lblHotKeyConflict.Hint := EmptyStr;

  HMForm := HotMan.Forms.Find(GetSelectedCategory);
  if not Assigned(HMForm) then
    Exit;

  sShortCut := edtHotKey.Text;
  sCommand := stgCommands.Cells[stgCmdCommandIndex, stgCommands.Row];

  count := 0;
  isFormHotKey := true;
  // search if any checked control has same hotkey assigned somewhere else
  for i := 0 to cgHKControls.Items.Count - 1 do
  begin
    if not cgHKControls.Checked[i] then
      continue;

    isFormHotKey := false;

    HMControl := THMControl(cgHKControls.Items.Objects[i]);
    if not Assigned(HMControl) then
      continue;

    hotkey := HMControl.Hotkeys.Find(sShortCut);
    if Assigned(hotkey) and (hotkey.command <> sCommand) then
    begin
      Inc(count);

      if DeleteConflicts then
        HMControl.Hotkeys.Remove(hotkey)
      else
        AddConflictHint(hotkey.Command, HMControl.Name);
    end;
  end;

  if isFormHotKey then
  begin
    hotkey := HMForm.Hotkeys.Find(sShortCut);
    if Assigned(hotkey) and (hotkey.command <> sCommand) then
    begin
      Inc(count);

      if DeleteConflicts then
        HMForm.Hotkeys.Remove(hotkey)
      else
        AddConflictHint(hotkey.Command, HMForm.Name);
    end;
  end;

  // show full message if only one conflict, else show a generic message
  if count = 1 then
    lblHotKeyConflict.Caption := lblHotKeyConflict.Hint
  else if count > 1 then
    lblHotKeyConflict.Caption := rsOptHotkeysShortCutUsed + ' [..]';

  lblHotKeyConflict.Visible := count > 0;
end;

procedure TfrmOptionsHotkeys.FillHotkeyList(sCommand: String);
var
  HMForm: THMForm;
  HMControl: THMControl;
  iHotKey, iControl, iGrid: Integer;
  hotkey: THotkey;
  found: Boolean;
begin
  Self.stgHotkeys.RowCount := Self.stgHotkeys.FixedRows;

  if sCommand = EmptyStr then
    Exit;

  if Self.lbxCategories.ItemIndex = -1 then
    Exit;

  HMForm := HotMan.Forms.Find(GetSelectedCategory);
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
      stgHotkeys.Cells[0, stgHotkeys.RowCount - 1] := hotkey.ShortCut;
      stgHotkeys.Cells[1, stgHotkeys.RowCount - 1] := hotkey.Params;
    end;

    // add hotkeys from controls
    for iControl := 0 to HMForm.Controls.Count - 1  do
    begin
      HMControl := HMForm.Controls[iControl];
      for iHotKey := 0 to HMControl.Hotkeys.Count - 1 do
      begin
        hotkey := HMControl.Hotkeys[iHotKey];
        if hotkey.Command <> sCommand then
          continue;

        // search for hotkey in grid and add control name to list
        found := false;
        for iGrid := stgHotkeys.FixedRows to stgHotkeys.RowCount - 1 do
        begin
          if stgHotkeys.Cells[0, iGrid] = hotkey.ShortCut then
          begin
            stgHotkeys.Cells[2, iGrid] := stgHotkeys.Cells[2, iGrid] + HMControl.Name + ';';
            found := true;
            break;
          end; { if }
        end; { for }

        // add new row for hotkey
        if not found then
        begin
          stgHotkeys.RowCount := stgHotkeys.RowCount + 1;
          stgHotkeys.Cells[0, stgHotkeys.RowCount - 1] := hotkey.ShortCut;
          stgHotkeys.Cells[1, stgHotkeys.RowCount - 1] := hotkey.Params;
          stgHotkeys.Cells[2, stgHotkeys.RowCount - 1] := HMControl.Name + ';';
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

procedure TfrmOptionsHotkeys.FillHKControlList;
var
  HMForm: THMForm;
  i: Integer;
  ControlsList: TStringList;
begin
  ControlsList := TStringList.Create;
  try
    HMForm := HotMan.Forms.Find(GetSelectedCategory);
    if Assigned(HMForm) then
    begin
      for i := 0 to HMForm.Controls.Count - 1 do
        ControlsList.AddObject(HMForm.Controls[i].Name, HMForm.Controls[i]);
    end;
    ControlsList.Sort;
    cgHKControls.Items.Assign(ControlsList);
    cgHKControls.Visible := cgHKControls.Items.Count <> 0;
  finally
    ControlsList.Free;
  end;
end;

procedure TfrmOptionsHotkeys.FillCommandList(Filter: String);
//< fill stgCommands by commands and comments
var
  slTmp, slAllCommands, slComments, slHotKey: TStringList;
  slFiltered: TStringList = nil;
  lstr:   String;
  i:      Integer;
  HMForm: THMForm;
  sForm:  String;
  CommandsFormClass: TComponentClass;
  CommandsForm: TComponent = nil;
  CommandsFormCreated: Boolean = False;
  CommandsIntf: IFormCommands;
begin
  sForm := GetSelectedCategory;
  CommandsFormClass := TFormCommands.GetCommandsForm(sForm);
  if not Assigned(CommandsFormClass) or
     not Supports(CommandsFormClass, IFormCommands) then
  begin
    stgCommands.Clean;
    Exit;
  end;

  // Find an instance of the form to retrieve action list (for comments).
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
  slComments    := TStringList.Create;
  slHotKey      := TStringList.Create;
  slTmp         := TStringList.Create;
  HMForm        := HotMan.Forms.Find(sForm);

  CommandsIntf.GetCommandsList(slAllCommands);

  if Filter <> '' then // if filter not empty
  begin
    slFiltered := TStringList.Create;
    lstr := UTF8LowerCase(Filter);
    for i := 0 to slAllCommands.Count - 1 do // for all command
      // if filtered text find in command or comment then add to filteredlist
      if (UTF8Pos(lstr, UTF8LowerCase(slAllCommands.Strings[i])) <> 0) or
         (UTF8Pos(lstr, UTF8LowerCase(CommandsIntf.GetCommandCaption(slAllCommands.Strings[i]))) <> 0) then
      begin
        slFiltered.Add(slAllCommands[i]);
      end;
  end
  else // filter empty -> assign all commands to filtered list
  begin
    slFiltered    := slAllCommands;
    slAllCommands := nil;
  end;

  // sort filtered items
  slFiltered.Sort;
  for i := 0 to slFiltered.Count - 1 do
  begin // for all filtered items do
    // get comment for command and add to slComments list
    slComments.Add(CommandsIntf.GetCommandCaption(slFiltered.Strings[i]));

    // getting list of assigned hot key
    if Assigned(HMForm) then
    begin
      slTmp.Clear;
      GetHotKeyList(HMForm, slFiltered.Strings[i], slTmp);
      slHotKey.Add(StListToStr(';', slTmp, false)); //add to hotkey list created string
    end
    else
      slHotKey.Add('');
  end;

  // add to list NAMES of columns
  slFiltered.Insert(0, rsOptHotkeysCommand);
  slComments.Insert(0, rsOptHotkeysComment);
  slHotKey.Insert(0, rsOptHotkeysHotkeys);
  //set stringgrid rows count
  stgCommands.RowCount := slFiltered.Count;
  // copy to string grid created lists
  stgCommands.BeginUpdate;
  stgCommands.Clean;
  stgCommands.Cols[stgCmdCommandIndex].Assign(slFiltered);
  stgCommands.Cols[stgCmdHotkeysIndex].Assign(slHotKey);
  stgCommands.Cols[stgCmdCommentIndex].Assign(slComments);
  stgCommands.EndUpdate;
  AutoSizeCommandsGrid;

  stgCommands.Row := 0; // needs for call select function for refresh hotkeylist

  slHotKey.Free;
  slAllCommands.Free;
  slComments.Free;
  slFiltered.Free;
  slTmp.Free;

  if CommandsFormCreated then
    CommandsForm.Free;
end;

procedure TfrmOptionsHotkeys.FillCategoriesList;
var
  i, MainIndex, Diff: Integer;
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

function TfrmOptionsHotkeys.GetSelectedCategory: String;
var
  Index: Integer;
begin
  Index := lbxCategories.ItemIndex;
  if (Index >= 0) and (Index < FHotkeysCategories.Count) then
    Result := FHotkeysCategories[PtrUInt(lbxCategories.Items.Objects[Index])]
  else
    Result := EmptyStr;
end;

class function TfrmOptionsHotkeys.GetIconIndex: Integer;
begin
  Result := 5;
end;

class function TfrmOptionsHotkeys.GetTitle: String;
begin
  Result := rsOptionsEditorHotKeys;
end;

procedure TfrmOptionsHotkeys.Init;
begin
  stgCommands.FocusRectVisible := False;
  stgHotkeys.FocusRectVisible := False;
  // Localize Hotkeys.
  // stgCommands is localized in FillCommandList.
  stgHotkeys.Columns.Items[0].Title.Caption := rsOptHotkeysHotkey;
  stgHotkeys.Columns.Items[1].Title.Caption := rsOptHotkeysParameters;
end;

procedure TfrmOptionsHotkeys.Load;
begin
  FillSCFilesList;
end;

function TfrmOptionsHotkeys.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  // Save hotkeys file name.
  if lbSCFilesList.ItemIndex >= 0 then
    gNameSCFile := lbSCFilesList.Items[lbSCFilesList.ItemIndex];
end;

constructor TfrmOptionsHotkeys.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FHotkeysCategories := TStringList.Create;
end;

destructor TfrmOptionsHotkeys.Destroy;
begin
  inherited Destroy;
  FHotkeysCategories.Free;
end;

end.

