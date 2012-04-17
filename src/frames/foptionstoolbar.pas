{
   Double Commander
   -------------------------------------------------------------------------
   Toolbar configuration options page

   Copyright (C) 2006-2010  Koblov Alexander (Alexx2000@mail.ru)
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

unit fOptionsToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Buttons, fOptionsFrame, KASToolBar, KASToolItems,
  uFormCommands, uHotkeyManager, DCBasicTypes,
  fOptionsHotkeysEditHotkey, DCXmlConfig;

type

  { TfrmOptionsToolbar }

  TfrmOptionsToolbar = class(TOptionsEditor)
    btnInsertButton: TButton;
    btnCloneButton: TButton;
    btnDeleteButton: TButton;
    btnOpenFile: TButton;
    btnEditHotkey: TButton;
    btnRemoveHotkey: TButton;
    cbInternalCommand: TComboBox;
    cbFlatButtons: TCheckBox;
    edtExternalParameters: TEdit;
    edtExternalCommand: TEdit;
    lblHotkeyValue: TLabel;
    edtStartPath: TEdit;
    edtToolTip: TEdit;
    gbGroupBox: TGroupBox;
    edtIconFileName: TEdit;
    lblInternalParameters: TLabel;
    lblBarSize: TLabel;
    lblBarSizeValue: TLabel;
    lblInternalCommand: TLabel;
    lblExternalCommand: TLabel;
    lblHotkey: TLabel;
    lblIconFile: TLabel;
    lblIconSize: TLabel;
    lblIconSizeValue: TLabel;
    lblExternalParameters: TLabel;
    lblStartPath: TLabel;
    lblToolTip: TLabel;
    edtInternalParameters: TMemo;
    OpenDialog: TOpenDialog;
    pnlEditControls: TPanel;
    pnlFullToolbarButtons: TPanel;
    pnlEditToolbar: TPanel;
    pnlToolbarButtons: TPanel;
    rgToolItemType: TRadioGroup;
    btnOpenIcon: TButton;
    sboxToolbars: TScrollBox;
    trbBarSize: TTrackBar;
    trbIconSize: TTrackBar;
    procedure btnEditHotkeyClick(Sender: TObject);
    procedure btnInsertButtonClick(Sender: TObject);
    procedure btnRemoveHotKeyClick(Sender: TObject);
    procedure btnCloneButtonClick(Sender: TObject);
    procedure btnDeleteButtonClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure cbInternalCommandSelect(Sender: TObject);
    procedure cbFlatButtonsChange(Sender: TObject);
    procedure edtIconFileNameChange(Sender: TObject);
    procedure ToolbarDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ToolbarDragDrop(Sender, Source: TObject; X, Y: Integer);
    function ToolbarLoadButtonGlyph(ToolItem: TKASToolItem; iIconSize: Integer;
      clBackColor: TColor): TBitmap;
    procedure ToolbarToolButtonClick(Sender: TObject);
    procedure ToolbarToolButtonDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ToolbarToolButtonDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean; NumberOfButton: Integer);
    procedure ToolbarToolButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolbarToolButtonMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; NumberOfButton: Integer);
    procedure ToolbarToolButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOpenIconClick(Sender: TObject);
    function ToolbarToolItemShortcutsHint(ToolItem: TKASNormalItem): String;
    procedure rgToolItemTypeSelectionChanged(Sender: TObject);
    procedure trbBarSizeChange(Sender: TObject);
    procedure trbIconSizeChange(Sender: TObject);
  private
    FCurrentButton: TKASToolButton;
    FEditForm: TfrmEditHotkey;
    FFormCommands: IFormCommands;
    FToolButtonMouseX, FToolButtonMouseY, FToolDragButtonNumber: Integer; // For dragging
    FUpdatingButtonType: Boolean;
    FUpdatingIconText: Boolean;
    function AddNewSubToolbar(ToolItem: TKASMenuItem): TKASToolBar;
    procedure ApplyEditControls;
    procedure CloseToolbarsBelowCurrentButton;
    procedure CloseToolbar(Index: Integer);
    function CreateToolbar(Items: TKASToolBarItems): TKASToolBar;
    class function FindHotkey(NormalItem: TKASNormalItem; Hotkeys: THotkeys): THotkey;
    class function FindHotkey(NormalItem: TKASNormalItem): THotkey;
    function GetTopToolbar: TKASToolBar;
    procedure LoadCurrentButton;
    procedure LoadToolbar(ToolBar: TKASToolBar; Config: TXmlConfig; RootNode: TXmlNode);
    procedure MarkCurrentToolbar;
    procedure PressButtonDown(Button: TKASToolButton);
    procedure UpdateIcon(Icon: String);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetShortcuts(NormalItem: TKASNormalItem): TDynamicStringArray;
    class function GetTitle: String; override;
    procedure SelectButton(ButtonNumber: Integer);
  end;

implementation

{$R *.lfm}

uses
  LCLVersion, Toolwin,
  DCStrUtils, uGlobs, uLng, uOSForms, uDCUtils, uPixMapManager,
  uKASToolItemsExtended,
  fMain;

const
  cHotKeyCommand = 'cm_ExecuteToolbarItem';

{ TfrmOptionsToolbar }

class function TfrmOptionsToolbar.GetIconIndex: Integer;
begin
  Result := 32;
end;

class function TfrmOptionsToolbar.GetShortcuts(NormalItem: TKASNormalItem): TDynamicStringArray;
var
  Hotkey: THotkey;
begin
  Hotkey := FindHotkey(NormalItem);
  if Assigned(Hotkey) then
    Result := Hotkey.Shortcuts
  else
    Result := nil;
end;

class function TfrmOptionsToolbar.GetTitle: String;
begin
  Result := rsOptionsEditorToolbar;
end;

function TfrmOptionsToolbar.GetTopToolbar: TKASToolBar;
begin
  if sboxToolbars.ControlCount > 0 then
    Result := sboxToolbars.Controls[0] as TKASToolBar
  else
    Result := nil;
end;

procedure TfrmOptionsToolbar.Init;
var
  ToolBar: TKASToolBar;
begin
  FFormCommands := frmMain as IFormCommands;
  FFormCommands.GetCommandsList(cbInternalCommand.Items);
  cbInternalCommand.Sorted := True;
  FUpdatingButtonType := True;
  ParseLineToList(rsOptToolbarButtonType, rgToolItemType.Items);
  FUpdatingButtonType := False;
  FToolDragButtonNumber := -1;
  {$IF LCL_FULLVERSION >= 093100}
  rgToolItemType.OnSelectionChanged := @rgToolItemTypeSelectionChanged;
  {$ELSE}
  rgToolItemType.OnClick := @rgToolItemTypeSelectionChanged;
  {$ENDIF}
  ToolBar := CreateToolbar(nil);
  if Assigned(ToolBar) then
    // Put first one on top so that any other toolbars
    // created before Show are put below it.
    ToolBar.Top := 0;
end;

procedure TfrmOptionsToolbar.Load;
var
  ToolBarNode: TXmlNode;
  ToolBar: TKASToolBar;
begin
  trbBarSize.Position   := gToolBarButtonSize div 2;
  trbIconSize.Position  := gToolBarIconSize div 2;
  cbFlatButtons.Checked := gToolBarFlat;

  lblBarSizeValue.Caption  := IntToStr(trbBarSize.Position*2);
  lblIconSizeValue.Caption := IntToStr(trbIconSize.Position*2);

  FCurrentButton := nil;
  CloseToolbarsBelowCurrentButton;

  ToolBar := GetTopToolbar;
  ToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MainToolbar', False);
  LoadToolbar(ToolBar, gConfig, ToolBarNode);
  if ToolBar.ButtonCount > 0 then
    PressButtonDown(ToolBar.Buttons[0]);
end;

procedure TfrmOptionsToolbar.LoadCurrentButton;
var
  ToolItem: TKASToolItem;
  NormalItem: TKASNormalItem;
  CommandItem: TKASCommandItem;
  ProgramItem: TKASProgramItem;
  EnableNormal, EnableCommand, EnableProgram: Boolean;
  ButtonTypeIndex: Integer = -1;
  ShortcutsHint: String;
begin
  EnableNormal  := False;
  EnableCommand := False;
  EnableProgram := False;

  DisableAutoSizing;
  try
    CloseToolbarsBelowCurrentButton;

    if Assigned(FCurrentButton) then
    begin
      ToolItem := FCurrentButton.ToolItem;
      if ToolItem is TKASSeparatorItem then
        ButtonTypeIndex := 0;
      if ToolItem is TKASNormalItem then
      begin
        EnableNormal := True;
        NormalItem := TKASNormalItem(ToolItem);
        FUpdatingIconText := True;
        edtIconFileName.Text := NormalItem.Icon;
        FUpdatingIconText := False;
        edtToolTip.Text := NormalItem.Hint;
        ShortcutsHint := NormalItem.GetShortcutsHint;
        if ShortcutsHint = '' then
          lblHotkeyValue.Caption := rsOptHotkeysNoHotkey
        else
          lblHotkeyValue.Caption := ShortcutsHint;
        btnRemoveHotkey.Enabled := ShortcutsHint <> '';
      end;
      if ToolItem is TKASCommandItem then
      begin
        ButtonTypeIndex := 1;
        EnableCommand := True;
        CommandItem := TKASCommandItem(ToolItem);
        cbInternalCommand.Text := CommandItem.Command;
        SetStringsFromArray(edtInternalParameters.Lines, CommandItem.Params);
      end;
      if ToolItem is TKASProgramItem then
      begin
        ButtonTypeIndex := 2;
        EnableProgram := True;
        ProgramItem := TKASProgramItem(ToolItem);
        edtExternalCommand.Text := ProgramItem.Command;
        edtExternalParameters.Text := ProgramItem.Params;
        edtStartPath.Text := ProgramItem.StartPath;
      end;
      if ToolItem is TKASMenuItem then
      begin
        ButtonTypeIndex := 3;
        AddNewSubToolbar(TKASMenuItem(ToolItem));
      end;
    end;

    FUpdatingButtonType := True;
    rgToolItemType.ItemIndex := ButtonTypeIndex;
    FUpdatingButtonType := False;

    lblIconFile.Visible           := EnableNormal;
    edtIconFileName.Visible       := EnableNormal;
    btnOpenIcon.Visible           := EnableNormal;
    lblToolTip.Visible            := EnableNormal;
    edtToolTip.Visible            := EnableNormal;
    lblInternalCommand.Visible    := EnableCommand;
    cbInternalCommand.Visible     := EnableCommand;
    lblInternalParameters.Visible := EnableCommand;
    edtInternalParameters.Visible := EnableCommand;
    lblExternalCommand.Visible    := EnableProgram;
    edtExternalCommand.Visible    := EnableProgram;
    lblExternalParameters.Visible := EnableProgram;
    edtExternalParameters.Visible := EnableProgram;
    lblStartPath.Visible          := EnableProgram;
    edtStartPath.Visible          := EnableProgram;
    btnOpenFile.Visible           := EnableProgram;
    lblHotkey.Visible             := EnableNormal;
    lblHotkeyValue.Visible        := EnableNormal;
    btnEditHotkey.Visible         := EnableNormal;
    btnRemoveHotkey.Visible       := EnableNormal;
    btnCloneButton.Visible        := Assigned(FCurrentButton);
    btnDeleteButton.Visible       := Assigned(FCurrentButton);
    rgToolItemType.Visible        := Assigned(FCurrentButton);

    MarkCurrentToolbar;
  finally
    EnableAutoSizing;
  end;
end;

procedure TfrmOptionsToolbar.LoadToolbar(ToolBar: TKASToolBar; Config: TXmlConfig; RootNode: TXmlNode);
var
  ToolBarLoader: TKASToolBarExtendedLoader;
begin
  ToolBarLoader := TKASToolBarExtendedLoader.Create;
  try
    if Assigned(RootNode) then
      ToolBar.LoadConfiguration(Config, RootNode, ToolBarLoader);
  finally
    ToolBarLoader.Free;
  end;
end;

procedure TfrmOptionsToolbar.MarkCurrentToolbar;
var
  MarkToolBar, ToolBar, PrevToolBar: TKASToolBar;
  i: Integer;
begin
  if Assigned(FCurrentButton) then
    MarkToolBar := FCurrentButton.ToolBar
  else
    MarkToolBar := GetTopToolbar;

  if Assigned(MarkToolBar) then
  begin
    DisableAutoSizing;
    try
      PrevToolBar := nil;
      for i := 0 to sboxToolbars.ControlCount - 1 do
      begin
        ToolBar := sboxToolbars.Controls[i] as TKASToolBar;
        if ToolBar = MarkToolBar then
        begin
          if Assigned(PrevToolBar) then
          begin
            PrevToolBar.EdgeBorders := [ebBottom];
            PrevToolBar.EdgeInner   := esLowered;
            PrevToolBar.EdgeOuter   := esLowered;
          end;
          ToolBar.EdgeInner   := esRaised;
          ToolBar.EdgeOuter   := esRaised;
          ToolBar.EdgeBorders := [ebTop, ebBottom];
        end
        else if PrevToolBar = MarkToolBar then
        begin
          ToolBar.EdgeInner   := esLowered;
          ToolBar.EdgeOuter   := esLowered;
          ToolBar.EdgeBorders := [ebTop];
        end
        else
        begin
          ToolBar.EdgeInner   := esNone;
          ToolBar.EdgeOuter   := esNone;
          ToolBar.EdgeBorders := [];
        end;
        PrevToolBar := ToolBar;
      end;
      PrevToolBar.EdgeOuter   := esRaised;
      PrevToolBar.EdgeBorders := [ebBottom];
    finally
      EnableAutoSizing;
    end;
  end;
end;

procedure TfrmOptionsToolbar.PressButtonDown(Button: TKASToolButton);
begin
  FUpdatingButtonType := True;
  Button.Click;
  FUpdatingButtonType := False;
end;

procedure TfrmOptionsToolbar.rgToolItemTypeSelectionChanged(Sender: TObject);
var
  ToolBar: TKASToolBar;
  ToolItem: TKASToolItem = nil;
  NewButton: TKASToolButton;
begin
  if not FUpdatingButtonType and Assigned(FCurrentButton) then
  begin
    case rgToolItemType.ItemIndex of
      0: ToolItem := TKASSeparatorItem.Create;
      1: ToolItem := TKASCommandItem.Create;
      2: ToolItem := TKASProgramItem.Create;
      3: ToolItem := TKASMenuItem.Create;
    end;
    if Assigned(ToolItem) then
    begin
      ToolBar := FCurrentButton.ToolBar;
      // Copy what you can from previous button type.
      ToolItem.Assign(FCurrentButton.ToolItem);
      NewButton := ToolBar.InsertButton(FCurrentButton, ToolItem);
      ToolBar.RemoveButton(FCurrentButton);
      FCurrentButton := NewButton;
      PressButtonDown(NewButton);
    end;
  end;
end;

function TfrmOptionsToolbar.Save: TOptionsEditorSaveFlags;
var
  ToolBarNode: TXmlNode;
  ToolBar: TKASToolBar;
begin
  ApplyEditControls;

  gToolBarFlat       := cbFlatButtons.Checked;
  gToolBarButtonSize := trbBarSize.Position * 2;
  gToolBarIconSize   := trbIconSize.Position * 2;

  ToolBar := GetTopToolbar;
  if Assigned(ToolBar) then
  begin
    ToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MainToolbar', True);
    gConfig.ClearNode(ToolBarNode);
    Toolbar.SaveConfiguration(gConfig, ToolBarNode);
  end;

  Result := [];
end;

procedure TfrmOptionsToolbar.btnOpenIconClick(Sender: TObject);
var
  sFileName: String;
begin
  sFileName := GetCmdDirFromEnvVar(edtIconFileName.Text);
  if ShowOpenIconDialog(Self, sFileName) then
    edtIconFileName.Text := sFileName;
end;

function TfrmOptionsToolbar.CreateToolbar(Items: TKASToolBarItems): TKASToolBar;
begin
  Result := TKASToolBar.Create(sboxToolbars);
  Result.AutoSize                := True;
  Result.Constraints.MinHeight   := 24;
  Result.Flat                    := cbFlatButtons.Checked;
  Result.GlyphSize               := trbIconSize.Position * 2;
  Result.RadioToolBar            := True;
  Result.SetButtonSize(trbBarSize.Position * 2, trbBarSize.Position * 2);
  Result.ShowDividerAsButton     := True;
  Result.OnDragOver              := @ToolbarDragOver;
  Result.OnDragDrop              := @ToolbarDragDrop;
  Result.OnLoadButtonGlyph       := @ToolbarLoadButtonGlyph;
  Result.OnToolButtonClick       := @ToolbarToolButtonClick;
  Result.OnToolButtonMouseDown   := @ToolbarToolButtonMouseDown;
  Result.OnToolButtonMouseUp     := @ToolbarToolButtonMouseUp;
  Result.OnToolButtonMouseMove   := @ToolbarToolButtonMouseMove;
  Result.OnToolButtonDragDrop    := @ToolbarToolButtonDragDrop;
  Result.OnToolButtonDragOver    := @ToolbarToolButtonDragOver;
  Result.OnToolItemShortcutsHint := @ToolbarToolItemShortcutsHint;
  Result.BorderSpacing.Bottom    := 2;
  Result.EdgeInner   := esNone;
  Result.EdgeOuter   := esNone;
  Result.EdgeBorders := [];
  Result.Top := MaxInt; // So that it is put under all existing toolbars (because of Align=alTop).

  Result.UseItems(Items);
  Result.Parent := sboxToolbars;
end;

function TfrmOptionsToolbar.AddNewSubToolbar(ToolItem: TKASMenuItem): TKASToolBar;
begin
  Result := CreateToolbar(ToolItem.SubItems);
  if Result.ButtonCount = 0 then
    Result.AddButton(TKASCommandItem.Create);
end;

procedure TfrmOptionsToolbar.ApplyEditControls;
var
  ToolItem: TKASToolItem;
  NormalItem: TKASNormalItem;
  CommandItem: TKASCommandItem;
  ProgramItem: TKASProgramItem;
begin
  if Assigned(FCurrentButton) then
  begin
    ToolItem := FCurrentButton.ToolItem;
    if ToolItem is TKASNormalItem then
    begin
      NormalItem := TKASNormalItem(ToolItem);
      NormalItem.Icon := edtIconFileName.Text;
      NormalItem.Hint := edtToolTip.Text;
    end;
    if ToolItem is TKASCommandItem then
    begin
      CommandItem := TKASCommandItem(ToolItem);
      CommandItem.Command := cbInternalCommand.Text;
      CommandItem.Params := GetArrayFromStrings(edtInternalParameters.Lines);
    end;
    if ToolItem is TKASProgramItem then
    begin
      ProgramItem := TKASProgramItem(ToolItem);
      ProgramItem.Command   := edtExternalCommand.Text;
      ProgramItem.Params    := edtExternalParameters.Text;
      ProgramItem.StartPath := edtStartPath.Text;
    end;
  end;
end;

(*Add new button on tool bar*)
procedure TfrmOptionsToolbar.btnInsertButtonClick(Sender: TObject);
var
  ToolBar: TKASToolBar;
begin
  if Assigned(FCurrentButton) then
  begin
    ApplyEditControls;
    ToolBar := FCurrentButton.ToolBar;
  end
  else
    ToolBar := GetTopToolbar;

  if Assigned(ToolBar) then
  begin
    FCurrentButton := ToolBar.InsertButton(FCurrentButton, TKASCommandItem.Create);
    PressButtonDown(FCurrentButton);
  end;
end;

procedure TfrmOptionsToolbar.btnRemoveHotKeyClick(Sender: TObject);
  procedure RemoveHotkey(Hotkeys: THotkeys; NormalItem: TKASNormalItem);
  var
    Hotkey: THotkey;
  begin
    Hotkey := FindHotkey(NormalItem, Hotkeys);
    Hotkeys.Remove(Hotkey);
  end;
var
  HMForm: THMForm;
  ToolItem: TKASToolItem;
  NormalItem: TKASNormalItem;
  I: Integer;
begin
  ToolItem := FCurrentButton.ToolItem;
  if ToolItem is TKASNormalItem then
  begin
    NormalItem := TKASNormalItem(ToolItem);
    HMForm := HotMan.Forms.Find('Main');
    if Assigned(HMForm) then
    begin
      RemoveHotkey(HMForm.Hotkeys, NormalItem);
      for I := 0 to HMForm.Controls.Count - 1 do
        RemoveHotkey(HMForm.Controls[I].Hotkeys, NormalItem);
    end;
    LoadCurrentButton;
  end;
end;

(*Clone selected button on tool bar*)
procedure TfrmOptionsToolbar.btnCloneButtonClick(Sender: TObject);
var
  SourceItem: TKASToolItem;
  Button: TKASToolButton;
begin
  if Assigned(FCurrentButton) then
  begin
    ApplyEditControls;
    SourceItem := FCurrentButton.ToolItem;
    Button := FCurrentButton.ToolBar.InsertButton(FCurrentButton, SourceItem.Clone);
    PressButtonDown(Button);
  end;
end;

(*Remove current button*)
procedure TfrmOptionsToolbar.btnDeleteButtonClick(Sender: TObject);
var
  NextButton: Integer;
  ToolBar: TKASToolBar;
begin
  if Assigned(FCurrentButton) then
  begin
    ToolBar := FCurrentButton.ToolBar;
    NextButton := FCurrentButton.Tag;
    Toolbar.RemoveButton(FCurrentButton);
    FCurrentButton := nil;
    if Toolbar.ButtonCount > 0 then
    begin
      // Select next button or the last one.
      if NextButton >= Toolbar.ButtonCount then
        NextButton := Toolbar.ButtonCount - 1;
      PressButtonDown(Toolbar.Buttons[NextButton]);
    end
    else
    begin
      LoadCurrentButton;
    end;
  end;
end;

procedure TfrmOptionsToolbar.btnEditHotkeyClick(Sender: TObject);
var
  HMForm: THMForm;
  TemplateHotkey, Hotkey: THotkey;
  ToolItem: TKASToolItem;
  NormalItem: TKASNormalItem;
  AControls: TDynamicStringArray = nil;
  I: Integer;
begin
  if not Assigned(FEditForm) then
    FEditForm := TfrmEditHotkey.Create(Self);

  ToolItem := FCurrentButton.ToolItem;
  if ToolItem is TKASNormalItem then
  begin
    NormalItem := TKASNormalItem(ToolItem);
    TemplateHotkey := THotkey.Create;
    try
      TemplateHotkey.Command := cHotKeyCommand;
      SetValue(TemplateHotkey.Params, 'ToolItemID', NormalItem.ID);

      HMForm := HotMan.Forms.Find('Main');
      if Assigned(HMForm) then
      begin
        Hotkey := FindHotkey(NormalItem, HMForm.Hotkeys);
        if Assigned(Hotkey) then
          TemplateHotkey.Shortcuts := Hotkey.Shortcuts;
        for I := 0 to HMForm.Controls.Count - 1 do
        begin
          Hotkey := FindHotkey(NormalItem, HMForm.Controls[I].Hotkeys);
          if Assigned(Hotkey) then
          begin
            TemplateHotkey.Shortcuts := Hotkey.Shortcuts;
            AddString(AControls, HMForm.Controls[I].Name);
          end;
        end;
      end;

      if FEditForm.Execute(True, 'Main', cHotKeyCommand, TemplateHotkey, AControls, [ehoHideParams]) then
      begin
        LoadCurrentButton;
      end;
    finally
      TemplateHotkey.Free;
    end;
  end;
end;

procedure TfrmOptionsToolbar.btnOpenFileClick(Sender: TObject);
begin
  OpenDialog.DefaultExt:= EmptyStr;
  OpenDialog.Filter:= EmptyStr;
  if OpenDialog.Execute then
    begin
      edtExternalCommand.Text := OpenDialog.FileName;
      edtStartPath.Text       := ExtractFilePath(OpenDialog.FileName);
      edtIconFileName.Text    := OpenDialog.FileName;
      edtToolTip.Text         := ExtractOnlyFileName(OpenDialog.FileName);
    end;
end;

procedure TfrmOptionsToolbar.cbInternalCommandSelect(Sender: TObject);
var
  Command: String;
begin
  Command := cbInternalCommand.Items[cbInternalCommand.ItemIndex];
  edtToolTip.Text := FFormCommands.GetCommandCaption(Command, cctLong);
  edtInternalParameters.HelpKeyword := '/cmds.html#' + Command;
end;

procedure TfrmOptionsToolbar.CloseToolbarsBelowCurrentButton;
var
  CloseFrom: Integer = 1;
  i: Integer;
begin
  if Assigned(FCurrentButton) then
  begin
    for i := 0 to sboxToolbars.ControlCount - 1 do
      if sboxToolbars.Controls[i] = FCurrentButton.ToolBar then
      begin
        CloseFrom := i + 1;
        Break;
      end;
  end;
  for i := sboxToolbars.ControlCount - 1 downto CloseFrom do
    CloseToolbar(i);
end;

procedure TfrmOptionsToolbar.CloseToolbar(Index: Integer);
begin
  if Index > 0 then
    sboxToolbars.Controls[Index].Free;
end;

procedure TfrmOptionsToolbar.cbFlatButtonsChange(Sender: TObject);
var
  i: Integer;
  ToolBar: TKASToolBar;
begin
  for i := 0 to sboxToolbars.ControlCount - 1 do
  begin
    ToolBar := sboxToolbars.Controls[i] as TKASToolBar;
    ToolBar.Flat := cbFlatButtons.Checked;
  end;
end;

procedure TfrmOptionsToolbar.edtIconFileNameChange(Sender: TObject);
begin
  if not FUpdatingIconText then
    UpdateIcon(edtIconFileName.Text);
end;

class function TfrmOptionsToolbar.FindHotkey(NormalItem: TKASNormalItem; Hotkeys: THotkeys): THotkey;
var
  i: Integer;
  ToolItemID: String;
begin
  for i := 0 to Hotkeys.Count - 1 do
  begin
    Result := Hotkeys.Items[i];
    if (Result.Command = cHotKeyCommand) and
       (GetParamValue(Result.Params, 'ToolItemID', ToolItemID)) and
       (ToolItemID = NormalItem.ID) then
       Exit;
  end;
  Result := nil;
end;

class function TfrmOptionsToolbar.FindHotkey(NormalItem: TKASNormalItem): THotkey;
var
  HMForm: THMForm;
  i: Integer;
begin
  HMForm := HotMan.Forms.Find('Main');
  if Assigned(HMForm) then
  begin
    Result := FindHotkey(NormalItem, HMForm.Hotkeys);
    if not Assigned(Result) then
    begin
      for i := 0 to HMForm.Controls.Count - 1 do
      begin
        Result := FindHotkey(NormalItem, HMForm.Controls[i].Hotkeys);
        if Assigned(Result) then
          Break;
      end;
    end;
  end
  else
    Result := nil;
end;

procedure TfrmOptionsToolbar.trbBarSizeChange(Sender: TObject);
var
  ToolBar: TKASToolBar;
  i: Integer;
begin
  DisableAutoSizing;
  try
    lblBarSizeValue.Caption := IntToStr(trbBarSize.Position*2);
    trbIconSize.Position    := trbBarSize.Position - (trbBarSize.Position div 5);
    for i := 0 to sboxToolbars.ControlCount - 1 do
    begin
      ToolBar := sboxToolbars.Controls[i] as TKASToolBar;
      ToolBar.SetButtonSize(trbBarSize.Position * 2, trbBarSize.Position * 2);
    end;
  finally
    EnableAutoSizing;
  end;
end;

procedure TfrmOptionsToolbar.trbIconSizeChange(Sender: TObject);
var
  ToolBar: TKASToolBar;
  i: Integer;
begin
  DisableAutoSizing;
  try
    lblIconSizeValue.Caption := IntToStr(trbIconSize.Position * 2);
    for i := 0 to sboxToolbars.ControlCount - 1 do
    begin
      ToolBar := sboxToolbars.Controls[i] as TKASToolBar;
      ToolBar.GlyphSize := trbIconSize.Position * 2;
    end;
  finally
    EnableAutoSizing;
  end;
end;

procedure TfrmOptionsToolbar.UpdateIcon(Icon: String);
var
  ToolItem: TKASToolItem;
  NormalItem: TKASNormalItem;
begin
  // Refresh icon on the toolbar.
  ToolItem := FCurrentButton.ToolItem;
  if ToolItem is TKASNormalItem then
  begin
    NormalItem := TKASNormalItem(ToolItem);
    NormalItem.Icon := Icon;
    FCurrentButton.ToolBar.UpdateIcon(FCurrentButton);
  end;
end;

procedure TfrmOptionsToolbar.ToolbarDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  // Drag to a different toolbar.
  Accept := (Source is TKASToolButton) and (TKASToolButton(Source).ToolBar <> Sender);
end;

procedure TfrmOptionsToolbar.ToolbarDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SourceButton: TKASToolButton;
  TargetToolbar: TKASToolBar;
begin
  if Source is TKASToolButton then
  begin
    SourceButton  := Source as TKASToolButton;
    TargetToolbar := Sender as TKASToolBar;
    if SourceButton.ToolBar <> TargetToolBar then
      SourceButton.ToolBar.MoveButton(SourceButton, TargetToolbar, nil);
  end;
end;

function TfrmOptionsToolbar.ToolbarLoadButtonGlyph(ToolItem: TKASToolItem;
  iIconSize: Integer; clBackColor: TColor): TBitmap;
begin
  if ToolItem is TKASSeparatorItem then  // Paint 'separator' icon
    begin
      Result := TBitmap.Create;
      Result.Transparent := True;
      Result.TransparentColor := clFuchsia;
      Result.SetSize(iIconSize, iIconSize);
      Result.Canvas.Brush.Color:= clFuchsia;
      Result.Canvas.FillRect(Rect(0,0,iIconSize,iIconSize));
      Result.Canvas.Brush.Color:= clBtnText;
      Result.Canvas.RoundRect(Rect(Round(iIconSize * 0.4), 2, Round(iIconSize * 0.6), iIconSize - 2),iIconSize div 8,iIconSize div 4);
    end
  else if ToolItem is TKASNormalItem then
    Result := PixMapManager.LoadBitmapEnhanced(TKASNormalItem(ToolItem).Icon, iIconSize, True, clBackColor)
  else
    Result := nil;
end;

(*Select button on panel*)
procedure TfrmOptionsToolbar.ToolbarToolButtonClick(Sender: TObject);
var
  ClickedButton: TKASToolButton;
begin
  ClickedButton := Sender as TKASToolButton;

  if not FUpdatingButtonType then
    ApplyEditControls;

  if Assigned(FCurrentButton) then
  begin
    // If current toolbar has changed depress the previous button.
    if FCurrentButton.ToolBar <> ClickedButton.ToolBar then
      FCurrentButton.Down := False;
  end;

  FCurrentButton := ClickedButton;
  LoadCurrentButton;
end;

procedure TfrmOptionsToolbar.ToolbarToolButtonDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  SourceButton, TargetButton: TKASToolButton;
begin
  if Source is TKASToolButton then
  begin
    SourceButton := Source as TKASToolButton;
    TargetButton := Sender as TKASToolButton;
    // Drop to a different toolbar.
    if SourceButton.ToolBar <> TargetButton.ToolBar then
    begin
      SourceButton.ToolBar.MoveButton(SourceButton, TargetButton.ToolBar, TargetButton);
    end;
  end;
end;

(* Move button if it is dragged*)
procedure TfrmOptionsToolbar.ToolbarToolButtonDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean; NumberOfButton: Integer);
var
  SourceButton, TargetButton: TKASToolButton;
begin
  if Source is TKASToolButton then
  begin
    SourceButton := Source as TKASToolButton;
    TargetButton := Sender as TKASToolButton;
    // Move on the same toolbar.
    if SourceButton.ToolBar = TargetButton.ToolBar then
    begin
      if FToolDragButtonNumber <> TargetButton.Tag then
      begin
        SourceButton.ToolBar.MoveButton(SourceButton.Tag, TargetButton.Tag);
        FToolDragButtonNumber := TargetButton.Tag;
        Accept := True;
      end;
    end;
  end;
end;

(* Do not start drag in here, because oterwise button wouldn't be pushed down*)
procedure TfrmOptionsToolbar.ToolbarToolButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FToolButtonMouseX := X;
  FToolButtonMouseY := Y;
end;

(* Start dragging only if mbLeft if pressed and mouse moved.*)
procedure TfrmOptionsToolbar.ToolbarToolButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; NumberOfButton: Integer);
var
  Button: TKASToolButton;
begin
  if Sender is TKASToolButton then
  begin
    if (ssLeft in Shift) and (FToolDragButtonNumber = -1) then
      if (abs(FToolButtonMouseX-X)>10) or (abs(FToolButtonMouseY-Y)>10) then
      begin
        Button := TKASToolButton(Sender);
        FToolDragButtonNumber := NumberOfButton;
        Button.Toolbar.Buttons[NumberOfButton].BeginDrag(False, 5);
      end;
  end;
end;

(* End button drag*)
procedure TfrmOptionsToolbar.ToolbarToolButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FToolDragButtonNumber := -1;
end;

function TfrmOptionsToolbar.ToolbarToolItemShortcutsHint(ToolItem: TKASNormalItem): String;
begin
  Result := ShortcutsToText(GetShortcuts(ToolItem));
end;

procedure TfrmOptionsToolbar.SelectButton(ButtonNumber: Integer);
var
  ToolBar: TKASToolBar;
begin
  if sboxToolbars.ControlCount > 0 then
  begin
    ToolBar := sboxToolbars.Controls[0] as TKASToolBar;
    if (ButtonNumber >= 0) and (ButtonNumber < Toolbar.ButtonCount) then
    begin
      FCurrentButton := Toolbar.Buttons[ButtonNumber];
      PressButtonDown(FCurrentButton);
    end;
  end;
end;

end.

