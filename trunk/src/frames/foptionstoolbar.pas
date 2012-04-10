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
  ComCtrls, ExtCtrls, Buttons, fOptionsFrame, KASToolBar, KASToolItems, uFormCommands;

type

  { TfrmOptionsToolbar }

  TfrmOptionsToolbar = class(TOptionsEditor)
    btnInsertButton: TButton;
    btnClearHotKey: TButton;
    btnCloneButton: TButton;
    btnDeleteButton: TButton;
    btnOpenFile: TButton;
    cbInternalCommand: TComboBox;
    cbFlatButtons: TCheckBox;
    edtHotKeys: TEdit;
    edtExternalParameters: TEdit;
    edtExternalCommand: TEdit;
    edtStartPath: TEdit;
    edtToolTip: TEdit;
    gbGroupBox: TGroupBox;
    edtIconFileName: TEdit;
    ktbBar: TKASToolBar;
    lblInternalParameters: TLabel;
    lblBarSize: TLabel;
    lblBarSizeValue: TLabel;
    lblInternalCommand: TLabel;
    lblExternalCommand: TLabel;
    lblHotKeys: TLabel;
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
    procedure btnInsertButtonClick(Sender: TObject);
    procedure btnClearHotKeyClick(Sender: TObject);
    procedure btnCloneButtonClick(Sender: TObject);
    procedure btnDeleteButtonClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure cbInternalCommandSelect(Sender: TObject);
    procedure cbFlatButtonsChange(Sender: TObject);
    procedure edtIconFileNameChange(Sender: TObject);
    procedure ktbBarClick(Sender: TObject);
    function ktbBarLoadButtonGlyph(ToolItem: TKASToolItem; iIconSize: Integer;
      clBackColor: TColor): TBitmap;
    procedure ktbBarToolButtonClick(Sender: TObject);
    procedure ktbBarToolButtonDragDrop(Sender, Source: TObject; X, Y: Integer;
      NumberOfButton: Integer);
    procedure ktbBarToolButtonDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean; NumberOfButton: Integer);
    procedure ktbBarToolButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; NumberOfButton: Integer);
    procedure ktbBarToolButtonMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; NumberOfButton: Integer);
    procedure ktbBarToolButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; NumberOfButton: Integer);
    procedure btnOpenIconClick(Sender: TObject);
    procedure rgToolItemTypeSelectionChanged(Sender: TObject);
    procedure sboxToolbarsClick(Sender: TObject);
    procedure trbBarSizeChange(Sender: TObject);
    procedure trbIconSizeChange(Sender: TObject);
  private
    FCurrentButton: TKASToolButton;
    FFormCommands: IFormCommands;
    FUpdatingIconText: Boolean;
    FUpdatingButtonType: Boolean;
    ToolButtonMouseX, ToolButtonMouseY, ToolDragButtonNumber: integer; // For dragging
    procedure ApplyEditControls;
    procedure LoadCurrentButton;
    procedure UpdateIcon(Icon: String);
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    procedure SelectButton(ButtonNumber: Integer);
  end;

implementation

{$R *.lfm}

uses
  DCStrUtils, uGlobs, uLng, DCXmlConfig, uOSForms, uDCUtils, uPixMapManager,
  uKASToolItemsExtended, uHotkeyManager,
  fMain;

{ TfrmOptionsToolbar }

class function TfrmOptionsToolbar.GetIconIndex: Integer;
begin
  Result := 32;
end;

class function TfrmOptionsToolbar.GetTitle: String;
begin
  Result := rsOptionsEditorToolbar;
end;

procedure TfrmOptionsToolbar.Init;
begin
  FFormCommands := frmMain as IFormCommands;
  FFormCommands.GetCommandsList(cbInternalCommand.Items);
  cbInternalCommand.Sorted := True;
  FUpdatingButtonType := True;
  ParseLineToList(rsOptToolbarButtonType, rgToolItemType.Items);
  FUpdatingButtonType := False;
end;

procedure TfrmOptionsToolbar.Load;
var
  ToolBarLoader: TKASToolBarExtendedLoader;
  ToolBarNode: TXmlNode;
begin
  trbBarSize.Position   := gToolBarButtonSize div 2;
  trbIconSize.Position  := gToolBarIconSize div 2;
  cbFlatButtons.Checked := gToolBarFlat;

  lblBarSizeValue.Caption  := IntToStr(trbBarSize.Position*2);
  lblIconSizeValue.Caption := IntToStr(trbIconSize.Position*2);

  ktbBar.GlyphSize := gToolBarIconSize;
  ktbBar.SetButtonSize(gToolBarButtonSize, gToolBarButtonSize);
  ktbBar.Clear;
  ToolBarLoader := TKASToolBarExtendedLoader.Create;
  try
    ToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MainToolbar', False);
    if Assigned(ToolBarNode) then
    begin
      ktbBar.LoadConfiguration(gConfig, ToolBarNode, ToolBarLoader);
      if ktbBar.ButtonCount > 0 then
        ktbBar.Buttons[0].Click;
    end;
  finally
    ToolBarLoader.Free;
  end;
end;

procedure TfrmOptionsToolbar.LoadCurrentButton;
var
  ToolItem: TKASToolItem;
  NormalItem: TKASNormalItem;
  CommandItem: TKASCommandItem;
  ProgramItem: TKASProgramItem;
  EnableNormal, EnableCommand, EnableProgram: Boolean;
  ButtonTypeIndex: Integer = -1;
begin
  EnableNormal  := False;
  EnableCommand := False;
  EnableProgram := False;

  DisableAutoSizing;
  try
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
        edtHotKeys.Text := ShortcutsToText(NormalItem.Shortcuts);
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
        ButtonTypeIndex := 3;
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
    lblHotKeys.Visible            := False;//EnableNormal;
    edtHotKeys.Visible            := False;//EnableNormal;
    btnClearHotKey.Visible        := False;//EnableNormal;
    btnClearHotKey.Enabled        := Length(edtHotKeys.Text) <> 0;
    btnCloneButton.Visible        := Assigned(FCurrentButton);
    btnDeleteButton.Visible       := Assigned(FCurrentButton);
    rgToolItemType.Visible        := Assigned(FCurrentButton);
  finally
    EnableAutoSizing;
  end;
end;

procedure TfrmOptionsToolbar.rgToolItemTypeSelectionChanged(Sender: TObject);
var
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
      // Copy what you can from previous button type.
      ToolItem.Assign(FCurrentButton.ToolItem);
      NewButton := ktbBar.InsertButton(FCurrentButton, ToolItem);
      ktbBar.RemoveButton(FCurrentButton);
      FCurrentButton := NewButton;
      NewButton.Click;
    end;
  end;
end;

function TfrmOptionsToolbar.Save: TOptionsEditorSaveFlags;
var
  ToolBarNode: TXmlNode;
begin
  ApplyEditControls;

  gToolBarFlat       := cbFlatButtons.Checked;
  gToolBarButtonSize := trbBarSize.Position * 2;
  gToolBarIconSize   := trbIconSize.Position * 2;

  ToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MainToolbar', True);
  gConfig.ClearNode(ToolBarNode);
  ktbBar.SaveConfiguration(gConfig, ToolBarNode);

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
      //NormalItem.Shortcuts := edtHotKeys.Text;
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
begin
  ApplyEditControls;
  FCurrentButton := ktbBar.InsertButton(FCurrentButton, TKASCommandItem.Create);
  FCurrentButton.Click;
end;

procedure TfrmOptionsToolbar.btnClearHotKeyClick(Sender: TObject);
var
  HMForm: THMForm;
  Hotkey: THotkey;
  ToolItem: TKASToolItem;
  NormalItem: TKASNormalItem;
begin
  edtHotKeys.Text:= EmptyStr;
  btnClearHotKey.Enabled:= False;
  ToolItem := FCurrentButton.ToolItem;
  if ToolItem is TKASNormalItem then
  begin
    NormalItem := TKASNormalItem(ToolItem);
    HMForm := HotMan.Forms.Find('Main');
    if Assigned(HMForm) then
    begin
      Hotkey := HMForm.Hotkeys.Find(NormalItem.Shortcuts);
      HMForm.Hotkeys.Remove(Hotkey);
    end;
    NormalItem.Shortcuts := nil;
  end;
end;

(*Clone selected button on tool bar*)
procedure TfrmOptionsToolbar.btnCloneButtonClick(Sender: TObject);
var
  SourceItem: TKASToolItem;
  Button: TKASToolButton;
begin
  ApplyEditControls;
  SourceItem := FCurrentButton.ToolItem;
  Button := ktbBar.InsertButton(FCurrentButton, SourceItem.Clone);
  Button.Click;
end;

(*Remove current button*)
procedure TfrmOptionsToolbar.btnDeleteButtonClick(Sender: TObject);
var
  NextButton: Integer;
begin
  if Assigned(FCurrentButton) then
  begin
    NextButton := FCurrentButton.Tag;
    ktbBar.RemoveButton(FCurrentButton);
    FCurrentButton := nil;
    LoadCurrentButton;
    if ktbBar.ButtonCount > 0 then
    begin
      // Select next button or the last one.
      if NextButton >= ktbBar.ButtonCount then
        NextButton := ktbBar.ButtonCount - 1;
      ktbBar.Buttons[NextButton].Click;
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

procedure TfrmOptionsToolbar.cbFlatButtonsChange(Sender: TObject);
begin
  ktbBar.Flat := cbFlatButtons.Checked;
end;

procedure TfrmOptionsToolbar.edtIconFileNameChange(Sender: TObject);
begin
  if not FUpdatingIconText then
    UpdateIcon(edtIconFileName.Text);
end;

procedure TfrmOptionsToolbar.trbBarSizeChange(Sender: TObject);
begin
  lblBarSizeValue.Caption:=IntToStr(trbBarSize.Position*2);
  trbIconSize.Position:= trbBarSize.Position - (trbBarSize.Position div 5);
  ktbBar.SetButtonSize(trbBarSize.Position*2,trbBarSize.Position*2);
end;

procedure TfrmOptionsToolbar.trbIconSizeChange(Sender: TObject);
begin
  lblIconSizeValue.Caption := IntToStr(trbIconSize.Position*2);
  ktbBar.GlyphSize := trbIconSize.Position*2;
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
    ktbBar.UpdateIcon(FCurrentButton);
  end;
end;

procedure TfrmOptionsToolbar.ktbBarClick(Sender: TObject);
begin
  sboxToolbarsClick(Sender);
end;

function TfrmOptionsToolbar.ktbBarLoadButtonGlyph(ToolItem: TKASToolItem;
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
procedure TfrmOptionsToolbar.ktbBarToolButtonClick(Sender: TObject);
begin
  FCurrentButton := Sender as TKASToolButton;
  LoadCurrentButton;
end;

(* Select button after it is dragged*)
procedure TfrmOptionsToolbar.ktbBarToolButtonDragDrop(Sender, Source: TObject;
  X, Y: Integer; NumberOfButton: Integer);
begin
  ktbBarToolButtonClick(Sender);
end;

(* Move button if it is dragged*)
procedure TfrmOptionsToolbar.ktbBarToolButtonDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean; NumberOfButton: Integer);
begin
  if not (Source is TSpeedButton) then exit;
  if (ToolDragButtonNumber <> (Sender as TSpeedButton).Tag) then
    begin
      ktbBar.MoveButton((Source as TSpeedButton).Tag, (Sender as TSpeedButton).Tag);
      ToolDragButtonNumber := (Sender as TSpeedButton).Tag;
      Accept:=True;
    end;
end;

(* Do not start drag in here, because oterwise button wouldn't be pushed down*)
procedure TfrmOptionsToolbar.ktbBarToolButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  NumberOfButton: Integer);
begin
  ApplyEditControls;
  ToolButtonMouseX:=X;
  ToolButtonMouseY:=Y;
end;

(* Start dragging only if mbLeft if pressed and mouse moved.*)
procedure TfrmOptionsToolbar.ktbBarToolButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; NumberOfButton: Integer);
begin
  if (ssLeft in Shift) and (ToolDragButtonNumber = -1) then
    if (abs(ToolButtonMouseX-X)>10) or (abs(ToolButtonMouseY-Y)>10) then
    begin
      ToolDragButtonNumber:=NumberOfButton;
      ktbBar.Buttons[NumberOfButton].BeginDrag(false,5);
    end;
end;

(* End button drag*)
procedure TfrmOptionsToolbar.ktbBarToolButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  NumberOfButton: Integer);
begin
  ToolDragButtonNumber := -1;
end;

// Deselect any selected button
procedure TfrmOptionsToolbar.sboxToolbarsClick(Sender: TObject);
begin
  if Assigned(FCurrentButton) then
  begin
    ApplyEditControls;
    if Assigned(FCurrentButton) then
      FCurrentButton.Down := False;
    FCurrentButton := nil;
    LoadCurrentButton;
  end;
end;

procedure TfrmOptionsToolbar.SelectButton(ButtonNumber: Integer);
var
  i: Integer;
begin
  if (ButtonNumber >= 0) and (ButtonNumber < ktbBar.ButtonCount) then
  begin
    FCurrentButton := ktbBar.Buttons[ButtonNumber];
    FCurrentButton.Click;
  end;
end;

end.

