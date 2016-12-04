{
   Double Commander
   -------------------------------------------------------------------------
   Editor for hotkeys

   Copyright (C) 2012  Przemyslaw Nagay (cobines@gmail.com)
   Copyright (C) 2016  Alexander Koblov (alexx2000@mail.ru)

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
  ExtCtrls, Buttons, Menus, uHotkeyManager, DCBasicTypes;

type

  TEditHotkeyOption = (ehoHideParams);
  TEditHotkeyOptions = set of TEditHotkeyOption;

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
    pmWithAllShortcuts: TPopupMenu;
    btnSelectFromList: TSpeedButton;
    procedure btnAddShortcutClick(Sender: TObject);
    procedure btnRemoveShortcutClick(Sender: TObject);
    procedure btnShowCommandHelpClick(Sender: TObject);
    procedure cgHKControlsItemClick(Sender: TObject; {%H-}Index: integer);
    procedure edtShortcutKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure edtShortcutKeyPress(Sender: TObject; var Key: char);
    procedure edtShortcutKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChangeEnterBehaviorClick(Sender: TObject);
    procedure ShortcutHelperClick(Sender: TObject);
    procedure PopulateHelperMenu;
    procedure btnSelectFromListClick(Sender: TObject);
  private
    FCommand: String;
    FEditMode: Boolean;
    FForm: String;
    FForms, FFormsTranslated: TStringList;
    FOldHotkey: THotkey;
    FOptions: TEditHotkeyOptions;
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
                     AControls: TDynamicStringArray;
                     Options: TEditHotkeyOptions = []): Boolean;
    function CloneNewHotkey: THotkey;
  end;

implementation

{$R *.lfm}

uses
  LCLType, dmHelpManager, uKeyboard, uLng, uGlobs, uFormCommands, DCStrUtils,
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
    EditControl.Font.Color:=clRed;
    EditControl.Parent := pnlShortcuts;
    EditControl.OnKeyDown  := @edtShortcutKeyDown;
    EditControl.OnKeyPress := @edtShortcutKeyPress;
    EditControl.OnKeyUp    := @edtShortcutKeyUp;
  end;
end;

function TfrmEditHotkey.ApplyHotkey: Boolean;
  procedure UpdateHotkey(ShouldBePresent: Boolean;
                         HotkeyOld, HotkeyNew: THotkey;
                         Hotkeys: THotkeys);
  var
    hotkey: THotkey;
  begin
    if FEditMode then
    begin
      hotkey := Hotkeys.Find(HotkeyOld.Shortcuts);
      if Assigned(hotkey) and (hotkey.Command = FCommand) then
      begin
        if ShouldBePresent then
        begin
          hotkey.Assign(HotkeyNew);
          Hotkeys.UpdateHotkey(hotkey);
        end
        else if hotkey.SameParams(HotkeyOld.Params) then
          Hotkeys.Remove(hotkey);
      end
      else if ShouldBePresent then
        Hotkeys.Add(HotkeyNew.Shortcuts, HotkeyNew.Params, HotkeyNew.Command);
    end
    else if ShouldBePresent then
    begin
      // Overwrite old hotkey in Add mode too.
      hotkey := Hotkeys.Find(HotkeyNew.Shortcuts);
      if Assigned(hotkey) and (hotkey.Command = FCommand) then
      begin
        hotkey.Assign(HotkeyNew);
        Hotkeys.UpdateHotkey(hotkey);
      end
      else
        Hotkeys.Add(HotkeyNew.Shortcuts, HotkeyNew.Params, HotkeyNew.Command);
    end;
  end;

var
  i: Integer;
  HMForm: THMForm;
  HMControl: THMControl;
  NewHotkey: THotkey;
  IsFormHotkey: Boolean;
begin
  Result := False;

  NewHotkey := CloneNewHotkey;
  try
    // check for invalid hotkey
    if Length(NewHotkey.Shortcuts) = 0 then
      Exit;

    if (lblHotKeyConflict.Caption <> EmptyStr) then
    begin
      if (MessageDlg(rsOptHotkeysShortCutUsed,                                     // delete command on assigned shortcut
                     Format(rsOptHotkeysShortCutUsedText1,                         // if another was applied
                            [ShortcutsToText(NewHotkey.Shortcuts)]) + LineEnding +
                     Format(rsOptHotkeysShortCutUsedText2,
                            [NewHotkey.Command]),
                     mtConfirmation, mbYesNo, 0) = mrYes) then
        CheckHotKeyConflicts(True)
      else
        Exit;
    end;

    HMForm := HotMan.Forms.FindOrCreate(FForm);
    IsFormHotkey := True;
    for i := 0 to cgHKControls.Items.Count - 1 do
    begin
      HMControl := THMControl(cgHKControls.Items.Objects[i]);
      if Assigned(HMControl) then
      begin
        if cgHKControls.Checked[i] then
          IsFormHotkey := False;

        UpdateHotkey(cgHKControls.Checked[i],
                     FOldHotkey, NewHotkey, HMControl.Hotkeys);
      end;
    end;

    UpdateHotkey(IsFormHotkey,
                 FOldHotkey, NewHotkey, HMForm.Hotkeys);

    Result := True;
  finally
    NewHotkey.Free;
  end;
end;

procedure TfrmEditHotkey.btnAddShortcutClick(Sender: TObject);
begin
  AddShortcutEditor;
  if TEdit(pnlShortcuts.Controls[pred(pnlShortcuts.ControlCount)]).CanFocus then
    TEdit(pnlShortcuts.Controls[pred(pnlShortcuts.ControlCount)]).SetFocus;
end;

procedure TfrmEditHotkey.btnRemoveShortcutClick(Sender: TObject);
begin
  RemoveLastShortcutEditor;
end;

procedure TfrmEditHotkey.btnShowCommandHelpClick(Sender: TObject);
begin
  ShowHelpForKeywordWithAnchor(edtParameters.HelpKeyword);
end;

procedure TfrmEditHotkey.cgHKControlsItemClick(Sender: TObject; Index: integer);
begin
  CheckHotKeyConflicts;
end;

procedure TfrmEditHotkey.CheckHotKeyConflicts(DeleteConflicts: Boolean);
var
  ConflictsCount: Integer;
  ShortConflicts, LongConflicts: String;

  procedure AddCommandConflict(Hotkey: THotkey; const AName: String);
  var
    sConflict: String;
  begin
    sConflict := Format(rsOptHotkeysUsedBy, [Hotkey.Command, AName]);
    AddStrWithSep(ShortConflicts, sConflict, LineEnding);
    AddStrWithSep(LongConflicts, sConflict, LineEnding);
  end;

  procedure AddParamsConflict(Hotkey: THotkey);
  var
    sConflict: String;
    Param: String;
  begin
    sConflict := rsOptHotkeysUsedWithDifferentParams;
    AddStrWithSep(ShortConflicts, sConflict, LineEnding);
    if Length(Hotkey.Params) > 0 then
    begin
      sConflict := sConflict + ':';
      for Param in Hotkey.Params do
        AddStrWithSep(sConflict, ' ' + Param, LineEnding);
    end;
    AddStrWithSep(LongConflicts, sConflict, LineEnding);
  end;

  procedure CheckHotkey(Hotkeys: THotkeys; const AObjectName: String; HotkeyToSearch: THotkey);
  var
    Hotkey: THotkey;
  begin
    Hotkey := Hotkeys.Find(HotkeyToSearch.Shortcuts);
    if Assigned(Hotkey) then
    begin
      if Hotkey.Command <> FCommand then
      begin
        Inc(ConflictsCount);
        if DeleteConflicts then
          Hotkeys.Remove(Hotkey)
        else
          AddCommandConflict(Hotkey, GetTranslatedControlName(AObjectName));
      end
      else if not Hotkey.SameParams(HotkeyToSearch.Params) then
      begin
        Inc(ConflictsCount);
        if DeleteConflicts then
          Hotkeys.Remove(Hotkey)
        else
          AddParamsConflict(Hotkey);
      end;
    end;
  end;

var
  HMForm: THMForm;
  HMControl: THMControl;
  i: Integer;
  IsFormHotKey: Boolean;
  Hotkey: THotkey;
begin
  lblHotKeyConflict.Caption := EmptyStr;
  lblHotKeyConflict.Hint := EmptyStr;

  HMForm := HotMan.Forms.Find(FForm);
  if not Assigned(HMForm) then
    Exit;

  Hotkey := CloneNewHotkey;
  try
    ConflictsCount := 0;
    if Length(Hotkey.Shortcuts) > 0 then
    begin
      IsFormHotKey := True;
      // search if any checked control has same hotkey assigned somewhere else
      for i := 0 to cgHKControls.Items.Count - 1 do
      begin
        if cgHKControls.Checked[i] then
        begin
          IsFormHotKey := False;
          HMControl := THMControl(cgHKControls.Items.Objects[i]);
          if Assigned(HMControl) then
            CheckHotkey(HMControl.Hotkeys, HMControl.Name, Hotkey);
        end;
      end;

      if IsFormHotKey then
        CheckHotkey(HMForm.Hotkeys, HMForm.Name, Hotkey);

      lblHotKeyConflict.Caption := ShortConflicts;
      lblHotKeyConflict.Hint := LongConflicts;
    end;

    lblHotKeyConflict.Visible := ConflictsCount > 0;
  finally
    Hotkey.Free;
  end;
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
  FOldHotkey.Free;
end;

procedure TfrmEditHotkey.edtShortcutKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ShortCut: TShortCut;
  sShortCut: String;
  EditControl: TEdit;
begin
  if (Key<>VK_RETURN) or (not gUseEnterToCloseHotKeyEditor) then
  begin
    ShortCut := KeyToShortCutEx(Key, GetKeyShiftStateEx);
    sShortCut := ShortCutToTextEx(ShortCut);
    EditControl := Sender as TEdit;

    // Allow closing the dialog if Escape pressed twice.
    if (ShortCut <> VK_ESCAPE) or (EditControl.Text <> sShortCut) then
    begin
      EditControl.Text := sShortCut;
      Key := 0;
      btnOK.Enabled := GetShortcuts <> nil;
      lblHotKeyConflict.Caption := '';

      CheckHotKeyConflicts;
    end;
  end;
end;

procedure TfrmEditHotkey.edtShortcutKeyPress(Sender: TObject; var Key: char);
var
  EditControl: TEdit;
begin
  if (Key <> Char(VK_RETURN)) or (not gUseEnterToCloseHotKeyEditor) then
  begin
    EditControl := Sender as TEdit;
    EditControl.Text := '';
    btnOK.Enabled := GetShortcuts <> nil;
    Key := #0;
  end;
end;

procedure TfrmEditHotkey.edtShortcutKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ShortCut: TShortCut;
  sShortCut: String;
  EditControl: TEdit;
begin
  if (Key <> VK_RETURN) or (not gUseEnterToCloseHotKeyEditor) then
  begin
    ShortCut := KeyToShortCutEx(Key, GetKeyShiftStateEx);
    sShortCut := ShortCutToTextEx(ShortCut);
    EditControl := Sender as TEdit;

    // Select next shortcut editor.
    if (ShortCut <> VK_ESCAPE) and (sShortCut <> '') and (EditControl.Text = sShortCut) then
      pnlShortcuts.SelectNext(EditControl, True, True);
  end;
end;

function TfrmEditHotkey.Execute(
  EditMode: Boolean;
  Form: String;
  Command: String;
  Hotkey: THotkey;
  AControls: TDynamicStringArray;
  Options: TEditHotkeyOptions = []): Boolean;
begin
  FEditMode := EditMode;
  FForm := Form;
  FOptions := Options;

  SetHotkey(Hotkey);
  SetCommand(Command);
  SetControls(AControls);
  PopulateHelperMenu;

  if EditMode then
    Caption := Format(rsOptHotkeysEditHotkey, [Command])
  else
    Caption := Format(rsOptHotkeysAddHotkey, [Command]);

  lblParameters.Visible := not (ehoHideParams in Options);
  edtParameters.Visible := not (ehoHideParams in Options);
  btnShowCommandHelp.Visible := not (ehoHideParams in Options);
  btnOK.Enabled := GetShortcuts <> nil;
  lblHotKeyConflict.Caption := '';
  lblHotKeyConflict.Hint    := '';
  lblHotKeyConflict.Visible := False;

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
begin
  Result := GetArrayFromStrings(edtParameters.Lines);
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
  FillHKControlList;

  // Mark controls to which hotkey applies.
  for i := 0 to cgHKControls.Items.Count - 1 do
  begin
    cgHKControls.Checked[i] := False;

    for sControl in NewControls do
      if cgHKControls.Items[i] = sControl then
      begin
        cgHKControls.Checked[i] := True;
        Break;
      end;
  end;
end;

procedure TfrmEditHotkey.SetHotkey(Hotkey: THotkey);
begin
  FreeAndNil(FOldHotkey);
  if Assigned(Hotkey) then
  begin
    FOldHotkey := Hotkey.Clone;
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
begin
  SetStringsFromArray(edtParameters.Lines, NewParameters);
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
  end
  else
  begin
    while pnlShortcuts.ControlCount > 1 do
      RemoveLastShortcutEditor;
    if pnlShortcuts.ControlCount > 0 then
    begin
      EditControl := pnlShortcuts.Controls[0] as TEdit;
      EditControl.Clear;
    end;
  end;
end;

{ TfrmEditHotkey.ShortcutHelperClick }
procedure TfrmEditHotkey.ShortcutHelperClick(Sender: TObject);
var
  EditControl:TEdit=nil;
  iSeeker:integer;
begin
  for iSeeker:=0 to pred(pnlShortcuts.ControlCount) do
    if TEdit(pnlShortcuts.Controls[iSeeker]).Focused then EditControl:=TEdit(pnlShortcuts.Controls[iSeeker]);

  if (EditControl=nil) AND (pnlShortcuts.ControlCount>0) then
    EditControl:=TEdit(pnlShortcuts.Controls[pred(pnlShortcuts.ControlCount)]);

  if EditControl<>nil then
  begin
    EditControl.Text:=TMenuItem(Sender).Caption;
    btnOK.Enabled := GetShortcuts <> nil;
    lblHotKeyConflict.Caption := '';
    CheckHotKeyConflicts;

    // Select next shortcut editor.
    pnlShortcuts.SelectNext(EditControl, True, True);
  end;
end;

{ TfrmEditHotKey.PopulateHelperMenu }
procedure TfrmEditHotKey.PopulateHelperMenu;
const
  STD_PREFIX=6;
  CommandPrefix:array[0..pred(STD_PREFIX)] of string =('','Alt+','Ctrl+','Shift+','Ctrl+Shift+','Shift+Alt+');
var
  ASubMenu:TMenuItem;
  AMenuItem:TMenuItem;
  sMaybeSC:string;
  iPrefix,iFunction:integer;
  HMForm: THMForm;
  Hotkeys: THotkeys;
  i,j:integer;
  slAllShortcuts:TStringList;
begin
  slAllShortcuts:=TStringList.Create;
  try
    slAllShortcuts.Sorted:=True;
    slAllShortcuts.Duplicates:=dupIgnore;

    //1. Clear any previous menu entries.
    pmWithAllShortcuts.Items.Clear;

    //2. Scan to get all the shortcuts in a TStringList
    HMForm := HotMan.Forms.Find(FForm);
    if not Assigned(HMForm) then Exit;
    Hotkeys := HMForm.Hotkeys;
    for i:=0 to pred(Hotkeys.Count) do
      for j:=0 to pred(length(Hotkeys.Items[i].Shortcuts)) do
        slAllShortcuts.Add(Hotkeys.Items[i].Shortcuts[j]);

    //3. Begin to populate menu with the "F" fonction keys
    for iPrefix:=0 to pred(STD_PREFIX) do
    begin
      ASubMenu:=TMenuItem.Create(pmWithAllshortcuts);
      ASubMenu.Caption := CommandPrefix[iPrefix]+'Fx...';
      pmWithAllShortcuts.Items.Add(ASubMenu);

      for iFunction:=1 to 12 do
      begin
        sMaybeSC:=Format('%sF%d',[CommandPrefix[iPrefix],iFunction]);
        if slAllShortcuts.IndexOf(sMaybeSC)=-1 then
        begin
          AMenuItem:=TMenuItem.Create(pmWithAllShortcuts);
          AMenuItem.Caption:=sMaybeSC;
          AMenuItem.Enabled:=(slAllShortcuts.IndexOf(sMaybeSC)=-1);
          if AMenuItem.Enabled then AMenuItem.OnClick:=@ShortcutHelperClick;
          ASubMenu.Add(AMenuItem);
        end;
      end;
    end;

    //4. Then a little separator
    ASubMenu:=TMenuItem.Create(pmWithAllshortcuts);
    ASubMenu.Caption:='-';
    pmWithAllShortcuts.Items.Add(ASubMenu);

    //5. Continue to populate with the "letter" fonction keys
    for iPrefix:=2 to pred(STD_PREFIX) do
    begin
      ASubMenu:=TMenuItem.Create(pmWithAllshortcuts);
      ASubMenu.Caption:=CommandPrefix[iPrefix]+rsSimpleWordLetter;
      pmWithAllShortcuts.Items.Add(ASubMenu);

      for iFunction:=0 to pred(26) do
      begin
        sMaybeSC:=Format('%s%s',[CommandPrefix[iPrefix],AnsiChar(ord('A')+iFunction)]);
        if slAllShortcuts.IndexOf(sMaybeSC)=-1 then
        begin
          AMenuItem:=TMenuItem.Create(pmWithAllShortcuts);
          AMenuItem.Caption:=sMaybeSC;
          AMenuItem.Enabled:=(slAllShortcuts.IndexOf(sMaybeSC)=-1);
          if AMenuItem.Enabled then AMenuItem.OnClick:=@ShortcutHelperClick;
          ASubMenu.Add(AMenuItem);
        end;
      end;
    end;

    //6. Little separator
    ASubMenu:=TMenuItem.Create(pmWithAllshortcuts);
    ASubMenu.Caption:='-';
    pmWithAllShortcuts.Items.Add(ASubMenu);

    //7. Option for the "Enter"
    ASubMenu := TMenuItem.Create(pmWithAllshortcuts);
    ASubMenu.Caption := rsHotKeyNoSCEnter;
    ASubMenu.Checked := gUseEnterToCloseHotKeyEditor;
    ASubMenu.OnClick := @ChangeEnterBehaviorClick;
    pmWithAllShortcuts.Items.Add(ASubMenu);

  finally
    FreeAndNil(slAllShortcuts);
  end;
end;

procedure TfrmEditHotkey.btnSelectFromListClick(Sender: TObject);
begin
  pmWithAllShortcuts.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
end;

{ TfrmEditHotkey.ChangeEnterBehaviorClick }
procedure TfrmEditHotkey.ChangeEnterBehaviorClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  gUseEnterToCloseHotKeyEditor := TMenuItem(Sender).Checked;
end;

end.

