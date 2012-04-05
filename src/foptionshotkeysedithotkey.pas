{
   Double Commander
   -------------------------------------------------------------------------
   Editor for hotkeys

   Copyright (C) 2012       Przemyslaw Nagay (cobines@gmail.com)

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

unit fOptionsHotkeysEditHotkey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons,
  uHotkeyManager, uTypes;

type

  { TfrmEditHotkey }

  TfrmEditHotkey = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnShowCommandHelp: TButton;
    cgHKControls: TCheckGroup;
    lblShortcuts: TLabel;
    lblHotKeyConflict: TLabel;
    lblParameters: TLabel;
    edtParameters: TMemo;
    pnlShortcuts: TPanel;
    btnAddShortcut: TSpeedButton;
    btnRemoveShortcut: TSpeedButton;
    procedure btnAddShortcutClick(Sender: TObject);
    procedure btnRemoveShortcutClick(Sender: TObject);
    procedure btnShowCommandHelpClick(Sender: TObject);
    procedure cgHKControlsItemClick(Sender: TObject; Index: integer);
    procedure edtShortcutKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtShortcutKeyPress(Sender: TObject; var Key: char);
    procedure edtShortcutKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCommand: String;
    FControls: TDynamicStringArray;
    FShortcutsEditors: Integer;
    FForm: String;
    FForms, FFormsTranslated: TStringList;
    function ApplyHotkey: Boolean;
    procedure AddShortcutEditor;
    {en
       Check if combination of pressed hotkey and checked controls are already in use.
       Conflicting hotkeys are deleted if DeleteConflicts parameter is true.
    }
    procedure CheckHotKeyConflicts(DeleteConflicts: Boolean = false);
    procedure FillHKControlList;
    function GetShortcutsEditorsCount: Integer;
    function GetParameters: TDynamicStringArray;
    function GetShortcuts: TDynamicStringArray;
    function GetTranslatedControlName(const AName: String): String;
    function GetTranslatedFormName(const AName: String): String;
    procedure RemoveLastShortcutEditor;
    procedure SetBitmapOrCaption(Button: TSpeedButton; const AIconName, ACaption: String);
    procedure SetCommand(NewCommand: String);
    procedure SetControls(const NewControls: TDynamicStringArray);
    procedure SetHotkey(Hotkey: THotkey);
    procedure SetParameters(const NewParameters: TDynamicStringArray);
    procedure SetShortcuts(const NewShortcuts: TDynamicStringArray);
  public
    destructor Destroy; override;
    function Execute(EditMode: Boolean;
                     Form: String;
                     Command: String;
                     Hotkey: THotkey;
                     AControls: TDynamicStringArray): Boolean;
    function CloneNewHotkey: THotkey;
  end;

implementation

{$R *.lfm}

uses
  HelpIntfs, LCLType, uKeyboard, uLng, uGlobs, uFormCommands, uDCUtils,
  uPixMapManager;

const
  MaxShortcutSequenceLength = 5;

{ TfrmEditHotkey }

procedure TfrmEditHotkey.AddShortcutEditor;
var
  EditControl: TEdit;
begin
  if GetShortcutsEditorsCount < MaxShortcutSequenceLength then
  begin
    EditControl := TEdit.Create(Self);
    EditControl.Parent := pnlShortcuts;
    EditControl.OnKeyDown  := @edtShortcutKeyDown;
    EditControl.OnKeyPress := @edtShortcutKeyPress;
    EditControl.OnKeyUp    := @edtShortcutKeyUp;
  end;
end;

function TfrmEditHotkey.ApplyHotkey: Boolean;
var
  i: Integer;
  Shortcuts, Params: array of String;
  HMForm: THMForm;
  HMControl: THMControl;
  hotkey: THotkey;
  isFormHotkey: Boolean;
begin
  Result := False;

  Shortcuts := GetShortcuts;
  // check for invalid hotkey
  if Length(Shortcuts) = 0 then
    Exit;

  Params := GetParameters;

  if (lblHotKeyConflict.Caption <> EmptyStr) then
  begin
    if (MessageDlg(rsOptHotkeysShortCutUsed,                                     // delete command on assigned shortcut
                   Format(rsOptHotkeysShortCutUsedText1,                         // if another was applied
                          [ShortcutsToText(Shortcuts)]) + LineEnding +
                   Format(rsOptHotkeysShortCutUsedText2,
                          [FCommand]),
                   mtConfirmation, mbYesNo, 0) = mrYes) then
      CheckHotKeyConflicts(true)
    else
      Exit;
  end;

  HMForm := HotMan.Forms.FindOrCreate(FForm);
  isFormHotkey := true;
  for i := 0 to cgHKControls.Items.Count - 1 do
  begin
    HMControl := THMControl(cgHKControls.Items.Objects[i]);
    if not Assigned(HMControl) then
      continue;

    // delete previous hotkey if exists
    hotkey := HMControl.Hotkeys.Find(Shortcuts);
    if Assigned(hotkey) and (hotkey.Command = FCommand) then
      HMControl.Hotkeys.Remove(hotkey);

    // add new hotkey
    if cgHKControls.Checked[i] then
    begin
      isFormHotkey := false;
      HMControl.Hotkeys.Add(Shortcuts, Params, FCommand);
    end;
  end;

  // delete previous hotkey if exists
  hotkey := HMForm.Hotkeys.Find(Shortcuts);
  if Assigned(hotkey) and (hotkey.Command = FCommand) then
    HMForm.Hotkeys.Remove(hotkey);

  if isFormHotkey then
    HMForm.Hotkeys.Add(Shortcuts, Params, FCommand);

  Result := True;
end;

procedure TfrmEditHotkey.btnAddShortcutClick(Sender: TObject);
begin
  AddShortcutEditor;
end;

procedure TfrmEditHotkey.btnRemoveShortcutClick(Sender: TObject);
begin
  RemoveLastShortcutEditor;
end;

procedure TfrmEditHotkey.btnShowCommandHelpClick(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('', edtParameters.HelpKeyword);
end;

procedure TfrmEditHotkey.cgHKControlsItemClick(Sender: TObject; Index: integer);
begin
  CheckHotKeyConflicts;
end;

procedure TfrmEditHotkey.CheckHotKeyConflicts(DeleteConflicts: Boolean);
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
  Shortcuts: TDynamicStringArray;
  hotkey: THotkey;
  i, count: Integer;
  isFormHotKey: Boolean;
begin
  lblHotKeyConflict.Caption := EmptyStr;
  lblHotKeyConflict.Hint := EmptyStr;

  HMForm := HotMan.Forms.Find(FForm);
  if not Assigned(HMForm) then
    Exit;

  Shortcuts := GetShortcuts;

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

    hotkey := HMControl.Hotkeys.Find(Shortcuts);
    if Assigned(hotkey) and (hotkey.command <> FCommand) then
    begin
      Inc(count);

      if DeleteConflicts then
        HMControl.Hotkeys.Remove(hotkey)
      else
        AddConflictHint(hotkey.Command, GetTranslatedControlName(HMControl.Name));
    end;
  end;

  if isFormHotKey then
  begin
    hotkey := HMForm.Hotkeys.Find(Shortcuts);
    if Assigned(hotkey) and (hotkey.command <> FCommand) then
    begin
      Inc(count);

      if DeleteConflicts then
        HMForm.Hotkeys.Remove(hotkey)
      else
        AddConflictHint(hotkey.Command, GetTranslatedFormName(HMForm.Name));
    end;
  end;

  // show full message if only one conflict, else show a generic message
  if count = 1 then
    lblHotKeyConflict.Caption := lblHotKeyConflict.Hint
  else if count > 1 then
    lblHotKeyConflict.Caption := rsOptHotkeysShortCutUsed + ' [..]';

  lblHotKeyConflict.Visible := count > 0;
end;

function TfrmEditHotkey.CloneNewHotkey: THotkey;
begin
  Result := THotkey.Create;
  Result.Shortcuts := GetShortcuts;
  Result.Params    := GetParameters;
  Result.Command   := FCommand;
end;

destructor TfrmEditHotkey.Destroy;
begin
  inherited Destroy;
  FForms.Free;
  FFormsTranslated.Free;
end;

procedure TfrmEditHotkey.edtShortcutKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ShortCut: TShortCut;
  sShortCut: String;
  EditControl: TEdit;
begin
  ShortCut := KeyToShortCutEx(Key, GetKeyShiftStateEx);
  sShortCut := ShortCutToTextEx(ShortCut);
  EditControl := Sender as TEdit;

  // Allow closing the dialog if Escape pressed twice.
  if (ShortCut <> VK_ESCAPE) or (EditControl.Text <> sShortCut) then
  begin
    EditControl.Text := sShortCut;
    Key := 0;
    btnOK.Enabled := sShortCut <> '';
    lblHotKeyConflict.Caption := '';

    CheckHotKeyConflicts;
  end;
end;

procedure TfrmEditHotkey.edtShortcutKeyPress(Sender: TObject; var Key: char);
var
  EditControl: TEdit;
begin
  EditControl := Sender as TEdit;
  EditControl.Text := '';
  btnOK.Enabled := False;
  Key := #0;
end;

procedure TfrmEditHotkey.edtShortcutKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ShortCut: TShortCut;
  sShortCut: String;
  EditControl: TEdit;
begin
  ShortCut := KeyToShortCutEx(Key, GetKeyShiftStateEx);
  sShortCut := ShortCutToTextEx(ShortCut);
  EditControl := Sender as TEdit;

  // Select next shortcut editor.
  if (ShortCut <> VK_ESCAPE) and (sShortCut <> '') and (EditControl.Text = sShortCut) then
    pnlShortcuts.SelectNext(EditControl, True, True);
end;

function TfrmEditHotkey.Execute(
  EditMode: Boolean;
  Form: String;
  Command: String;
  Hotkey: THotkey;
  AControls: TDynamicStringArray): Boolean;
begin
  FForm := Form;
  SetHotkey(Hotkey);
  SetCommand(Command);
  SetControls(AControls);

  if EditMode then
    Caption := Format(rsOptHotkeysEditHotkey, [Command])
  else
    Caption := Format(rsOptHotkeysAddHotkey, [Command]);

  if ShowModal = mrOK then
    Result := ApplyHotkey
  else
    Result := False;
end;

procedure TfrmEditHotkey.FillHKControlList;
var
  HMForm: THMForm;
  i: Integer;
  ControlsList: TStringList;
begin
  ControlsList := TStringList.Create;
  try
    HMForm := HotMan.Forms.Find(FForm);
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

procedure TfrmEditHotkey.FormCreate(Sender: TObject);
begin
  lblHotKeyConflict.Color := clHighlight;
  lblHotKeyConflict.Font.Color := clHighlightText;

  FForms := TStringList.Create;
  FFormsTranslated := TStringList.Create;
  TFormCommands.GetCategoriesList(FForms, FFormsTranslated);

  SetBitmapOrCaption(btnAddShortcut, 'list-add', '+');
  SetBitmapOrCaption(btnRemoveShortcut, 'list-remove', '-');

  AddShortcutEditor;
end;

procedure TfrmEditHotkey.FormShow(Sender: TObject);
var
  EditControl: TEdit;
begin
  if pnlShortcuts.ControlCount > 0 then
  begin
    EditControl := pnlShortcuts.Controls[0] as TEdit;
    EditControl.SetFocus;
    EditControl.SelStart := Length(EditControl.Text);
    EditControl.SelLength := 0;
  end;
end;

function TfrmEditHotkey.GetParameters: TDynamicStringArray;
var
  Lines: Integer;
  i: Integer;
begin
  Lines := edtParameters.Lines.Count;
  if Lines > 0 then
  begin
    if edtParameters.Lines.Strings[Lines-1] = '' then
      Dec(Lines);
    SetLength(Result, Lines);
    for i := 0 to Lines - 1 do
      Result[i] := edtParameters.Lines.Strings[i];
  end;
end;

function TfrmEditHotkey.GetShortcuts: TDynamicStringArray;
var
  i: Integer;
  EditControl: TEdit;
begin
  Result := nil;
  for i := 0 to pnlShortcuts.ControlCount - 1 do
  begin
    EditControl := pnlShortcuts.Controls[i] as TEdit;
    if EditControl.Text <> '' then
      AddString(Result, EditControl.Text);
  end;
end;

function TfrmEditHotkey.GetShortcutsEditorsCount: Integer;
begin
  Result := pnlShortcuts.ControlCount;
end;

function TfrmEditHotkey.GetTranslatedControlName(const AName: String): String;
begin
  // TODO: Translate controls names.
  Result := AName;
end;

function TfrmEditHotkey.GetTranslatedFormName(const AName: String): String;
var
  i: Integer;
begin
  i := FForms.IndexOf(AName);
  if i >= 0 then
    Result := FFormsTranslated.Strings[i]
  else
    Result := AName;
end;

procedure TfrmEditHotkey.RemoveLastShortcutEditor;
begin
  if pnlShortcuts.ControlCount > 1 then
    pnlShortcuts.Controls[pnlShortcuts.ControlCount - 1].Free;
end;

procedure TfrmEditHotkey.SetBitmapOrCaption(Button: TSpeedButton; const AIconName, ACaption: String);
var
  Bmp: TBitmap = nil;
  IconIndex: PtrInt;
begin
  IconIndex := PixMapManager.GetIconByName(AIconName);
  if IconIndex <> -1 then
    Bmp := PixMapManager.GetBitmap(IconIndex);

  if Assigned(Bmp) then
  begin
    Button.Glyph  := Bmp;
    Button.Height := gIconsSize;
    Button.Width  := gIconsSize;
    Bmp.Free;
  end
  else
  begin
    Button.Caption := ACaption;
  end;
end;

procedure TfrmEditHotkey.SetCommand(NewCommand: String);
begin
  FCommand := NewCommand;
  btnShowCommandHelp.Caption := Format(rsShowHelpFor, [FCommand]);
  edtParameters.HelpKeyword := '/cmds.html#' + FCommand;
end;

procedure TfrmEditHotkey.SetControls(const NewControls: TDynamicStringArray);
var
  sControl: String;
  i: Integer;
begin
  FControls := NewControls;

  FillHKControlList;

  // Mark controls to which hotkey applies.
  for i := 0 to cgHKControls.Items.Count - 1 do
  begin
    cgHKControls.Checked[i] := False;

    for sControl in FControls do
      if cgHKControls.Items[i] = sControl then
      begin
        cgHKControls.Checked[i] := True;
        Break;
      end;
  end;
end;

procedure TfrmEditHotkey.SetHotkey(Hotkey: THotkey);
begin
  if Assigned(Hotkey) then
  begin
    SetShortcuts(Hotkey.Shortcuts);
    SetParameters(Hotkey.Params);
  end
  else
  begin
    SetShortcuts(nil);
    SetParameters(nil);
  end;
end;

procedure TfrmEditHotkey.SetParameters(const NewParameters: TDynamicStringArray);
var
  Param: String;
begin
  edtParameters.Clear;
  for Param in NewParameters do
    edtParameters.Lines.Add(Param);
end;

procedure TfrmEditHotkey.SetShortcuts(const NewShortcuts: TDynamicStringArray);
var
  Index: Integer;
  EditControl: TEdit;
  Shortcut: String;
begin
  if Assigned(NewShortcuts) then
  begin
    while pnlShortcuts.ControlCount < Length(NewShortcuts) do
      AddShortcutEditor;
    while pnlShortcuts.ControlCount > Length(NewShortcuts) do
      RemoveLastShortcutEditor;

    Index := 0;
    for Shortcut in NewShortcuts do
    begin
      EditControl := pnlShortcuts.Controls[Index] as TEdit;
      EditControl.Text := Shortcut;
      Inc(Index);
    end;
  end;
end;

end.

