{
   Double Commander
   -------------------------------------------------------------------------
   Toolbar configuration options page

   Copyright (C) 2012      Przemyslaw Nagay (cobines@gmail.com)
   Copyright (C) 2006-2017 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Buttons, Menus, fOptionsFrame, KASToolBar, KASToolItems,
  uFormCommands, uHotkeyManager, DCBasicTypes,
  fOptionsHotkeysEditHotkey, DCXmlConfig;

type

  { TfrmOptionsToolbar }

  TfrmOptionsToolbar = class(TOptionsEditor)
    btnInsertButton: TButton;
    btnCloneButton: TButton;
    btnDeleteButton: TButton;
    btnParametersHelper: TSpeedButton;
    btnSuggestionTooltip: TButton;
    btnOpenFile: TButton;
    btnEditHotkey: TButton;
    btnOpenCmdDlg: TButton;
    btnRelativeStartPath: TSpeedButton;
    btnStartPath: TButton;
    btnRelativeIconFileName: TSpeedButton;
    btnRemoveHotkey: TButton;
    cbInternalCommand: TComboBox;
    cbFlatButtons: TCheckBox;
    edtExternalParameters: TEdit;
    edtExternalCommand: TEdit;
    lblHelpOnInternalCommand: TLabel;
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
    miSrcRplIconNames: TMenuItem;
    miSrcRplCommands: TMenuItem;
    miSrcRplParameters: TMenuItem;
    miSrcRplStartPath: TMenuItem;
    miSrcRplClickSeparator: TMenuItem;
    miSrcRplAllOfAll: TMenuItem;
    miSearchAndReplace: TMenuItem;
    miExportCurrent: TMenuItem;
    miImportAllDCCommands: TMenuItem;
    miAddSeparatorSubMenu: TMenuItem;
    miExternalCommandFirstElement: TMenuItem;
    miSubToolBarFirstElement: TMenuItem;
    miInternalCommandPriorCurrent: TMenuItem;
    miExternalCommandPriorCurrent: TMenuItem;
    miSubToolBarPriorCurrent: TMenuItem;
    miInternalCommandAfterCurrent: TMenuItem;
    miExternalCommandAfterCurrent: TMenuItem;
    miSubToolBarAfterCurrent: TMenuItem;
    miInternalCommandLastElement: TMenuItem;
    miExternalCommandLastElement: TMenuItem;
    miAddInternalCommandSubMenu: TMenuItem;
    miSubToolBarLastElement: TMenuItem;
    miAddExternalCommandSubMenu: TMenuItem;
    miAddSubToolBarSubMenu: TMenuItem;
    miSeparatorFirstItem: TMenuItem;
    miSeparatorPriorCurrent: TMenuItem;
    miSeparatorAfterCurrent: TMenuItem;
    miSeparatorLastElement: TMenuItem;
    miInternalCommandFirstElement: TMenuItem;
    OpenDialog: TOpenDialog;
    pmPathHelper: TPopupMenu;
    pnlEditControls: TPanel;
    pnlFullToolbarButtons: TPanel;
    pnlEditToolbar: TPanel;
    pnlToolbarButtons: TPanel;
    pmInsertButtonMenu: TPopupMenu;
    pmparameteresHelper: TPopupMenu;
    ReplaceDialog: TReplaceDialog;
    rgToolItemType: TRadioGroup;
    btnOpenIcon: TSpeedButton;
    pnToolbars: TPanel;
    btnRelativeExternalCommand: TSpeedButton;
    trbBarSize: TTrackBar;
    trbIconSize: TTrackBar;
    miImportSeparator: TMenuItem;
    SaveDialog: TSaveDialog;
    cbReportErrorWithCommands: TCheckBox;
    btnOther: TButton;
    pmOtherClickToolbar: TPopupMenu;
    miAddAllCmds: TMenuItem;
    miSeparator1: TMenuItem;
    miExport: TMenuItem;
    miExportTop: TMenuItem;
    miExportTopToDCBar: TMenuItem;
    miExportSeparator1: TMenuItem;
    miExportTopToTCIniKeep: TMenuItem;
    miExportTopToTCIniNoKeep: TMenuItem;
    miExportSeparator2: TMenuItem;
    miExportTopToTCBarKeep: TMenuItem;
    miExportTopToTCBarNoKeep: TMenuItem;
    miExportCurrentToDCBar: TMenuItem;
    miExportSeparator3: TMenuItem;
    miExportCurrentToTCIniKeep: TMenuItem;
    miExportCurrentToTCIniNoKeep: TMenuItem;
    miExportSeparator4: TMenuItem;
    miExportCurrentToTCBarKeep: TMenuItem;
    miExportCurrentToTCBarNoKeep: TMenuItem;
    miImport: TMenuItem;
    miImportDCBAR: TMenuItem;
    miImportDCBARReplaceTop: TMenuItem;
    miSeparator8: TMenuItem;
    miImportDCBARAddTop: TMenuItem;
    miImportDCBARAddMenuTop: TMenuItem;
    miSeparator9: TMenuItem;
    miImportDCBARAddCurrent: TMenuItem;
    miImportDCBARAddMenuCurrent: TMenuItem;
    miImportSeparator2: TMenuItem;
    miImportTCINI: TMenuItem;
    miImportTCINIReplaceTop: TMenuItem;
    miSeparator6: TMenuItem;
    miImportTCINIAddTop: TMenuItem;
    miImportTCINIAddMenuTop: TMenuItem;
    miSeparator7: TMenuItem;
    miImportTCINIAddCurrent: TMenuItem;
    miImportTCINIAddMenuCurrent: TMenuItem;
    miImportTCBAR: TMenuItem;
    miImportTCBARReplaceTop: TMenuItem;
    miSeparator10: TMenuItem;
    miImportTCBARAddTop: TMenuItem;
    miImportTCBARAddMenuTop: TMenuItem;
    miSeparator11: TMenuItem;
    miImportTCBARAddCurrent: TMenuItem;
    miImportTCBARAddMenuCurrent: TMenuItem;
    miSeparator2: TMenuItem;
    miBackup: TMenuItem;
    miExportTopToBackup: TMenuItem;
    miImportBackup: TMenuItem;
    miImportBackupReplaceTop: TMenuItem;
    miSeparator13: TMenuItem;
    miImportBackupAddTop: TMenuItem;
    miImportBackupAddMenuTop: TMenuItem;
    miSeparator14: TMenuItem;
    miImportBackupAddCurrent: TMenuItem;
    miImportBackupAddMenuCurrent: TMenuItem;
    procedure btnEditHotkeyClick(Sender: TObject);
    procedure btnInsertButtonClick(Sender: TObject);
    procedure btnOpenCmdDlgClick(Sender: TObject);
    procedure btnParametersHelperClick(Sender: TObject);
    procedure btnRelativeExternalCommandClick(Sender: TObject);
    procedure btnRelativeIconFileNameClick(Sender: TObject);
    procedure btnRelativeStartPathClick(Sender: TObject);
    procedure btnRemoveHotKeyClick(Sender: TObject);
    procedure btnCloneButtonClick(Sender: TObject);
    procedure btnDeleteButtonClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnStartPathClick(Sender: TObject);
    procedure btnSuggestionTooltipClick(Sender: TObject);
    procedure cbInternalCommandSelect(Sender: TObject);
    procedure cbFlatButtonsChange(Sender: TObject);
    procedure edtIconFileNameChange(Sender: TObject);
    procedure lblHelpOnInternalCommandClick(Sender: TObject);
    procedure miAddAllCmdsClick(Sender: TObject);
    procedure miInsertButtonClick(Sender: TObject);
    procedure miSrcRplClick(Sender: TObject);
    procedure ToolbarDragOver(Sender, Source: TObject; {%H-}X, {%H-}Y: Integer;
      {%H-}State: TDragState; var Accept: Boolean);
    procedure ToolbarDragDrop(Sender, Source: TObject; {%H-}X, {%H-}Y: Integer);
    function ToolbarLoadButtonGlyph(ToolItem: TKASToolItem; iIconSize: Integer;
      clBackColor: TColor): TBitmap;
    procedure ToolbarToolButtonClick(Sender: TObject);
    procedure ToolbarToolButtonDragDrop(Sender, Source: TObject; {%H-}X, {%H-}Y: Integer);
    procedure ToolbarToolButtonDragOver(Sender, Source: TObject; {%H-}X, {%H-}Y: Integer;
      {%H-}State: TDragState; var Accept: Boolean; {%H-}NumberOfButton: Integer);
    procedure ToolbarToolButtonMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure ToolbarToolButtonMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; NumberOfButton: Integer);
    procedure ToolbarToolButtonMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure btnOpenIconClick(Sender: TObject);
    function ToolbarToolItemShortcutsHint(ToolItem: TKASNormalItem): String;
    procedure rgToolItemTypeSelectionChanged(Sender: TObject);
    procedure trbBarSizeChange(Sender: TObject);
    procedure trbIconSizeChange(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    function ComputeToolbarsSignature(Seed:dword=$00000000): dword;
    procedure btnOtherClick(Sender: TObject);
    procedure miExportToAnythingClick(Sender: TObject);
    procedure miImportFromAnythingClick(Sender: TObject);

  private
    FUpdateHotKey: Boolean;
    FCurrentButton: TKASToolButton;
    FEditForm: TfrmEditHotkey;
    FFormCommands: IFormCommands;
    FToolButtonMouseX, FToolButtonMouseY, FToolDragButtonNumber: Integer; // For dragging
    FUpdatingButtonType: Boolean;
    FUpdatingIconText: Boolean;
    bFirstTimeDrawn: boolean;
    function AddNewSubToolbar(ToolItem: TKASMenuItem; bIncludeButtonOnNewBar:boolean=True): TKASToolBar;
    procedure ApplyEditControls;
    procedure CloseToolbarsBelowCurrentButton;
    procedure CloseToolbar(Index: Integer);
    function CreateToolbar(Items: TKASToolBarItems): TKASToolBar;
    class function FindHotkey(NormalItem: TKASNormalItem; Hotkeys: THotkeys): THotkey;
    class function FindHotkey(NormalItem: TKASNormalItem): THotkey;
    function GetTopToolbar: TKASToolBar;
    procedure LoadCurrentButton;
    procedure LoadToolbar(ToolBar: TKASToolBar; Config: TXmlConfig; RootNode: TXmlNode; ConfigurationLoadType: TTypeOfConfigurationLoad);
    procedure PressButtonDown(Button: TKASToolButton);
    procedure UpdateIcon(Icon: String);
    procedure DisplayAppropriateControls(EnableNormal, EnableCommand, EnableProgram: boolean);

  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetShortcuts(NormalItem: TKASNormalItem): TDynamicStringArray;
    class function GetTitle: String; override;
    function IsSignatureComputedFromAllWindowComponents: Boolean; override;
    function ExtraOptionsSignature(CurrentSignature:dword):dword; override;
    procedure SelectButton(ButtonNumber: Integer);
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  crc, LazUTF8, LCLVersion, Toolwin,

  //DC
  {$IFDEF MSWINDOWS}
  uTotalCommander,
  {$ENDIF}
  uComponentsSignature, fEditSearch, fMainCommandsDlg, uFileProcs, uDebug,
  DCOSUtils, uShowMsg, DCStrUtils, uGlobs, uLng, uOSForms, uDCUtils,
  uPixMapManager, uKASToolItemsExtended, fMain, uSpecialDir, dmHelpManager,
  uGlobsPaths;

const
  cHotKeyCommand = 'cm_ExecuteToolbarItem';

  { Constants used with export/import }
  MASK_ACTION_WITH_WHAT = $03;
  ACTION_WITH_WINCMDINI = $00;
  ACTION_WITH_TC_TOOLBARFILE = $01;
  ACTION_WITH_DC_TOOLBARFILE = $02;
  ACTION_WITH_BACKUP = $03;

  MASK_ACTION_TOOLBAR = $30;
  ACTION_WITH_MAIN_TOOLBAR = $0;
  IMPORT_IN_MAIN_TOOLBAR_TO_NEW_SUB_BAR = $1;
  ACTION_WITH_CURRENT_BAR = $2;
  IMPORT_IN_CURRENT_BAR_TO_NEW_SUB_BAR = $3;

  MASK_FLUSHORNOT_EXISTING = $80;
  ACTION_FLUSH_EXISTING = $80;
  MASK_IMPORT_DESTIONATION = $30;

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

{ TfrmOptionsToolbar.IsSignatureComputedFromAllWindowComponents }
function TfrmOptionsToolbar.IsSignatureComputedFromAllWindowComponents: Boolean;
begin
  Result := False;
end;

{ TfrmOptionsToolbar.ExtraOptionsSignature }
function TfrmOptionsToolbar.ExtraOptionsSignature(CurrentSignature:dword):dword;
begin
  Result := ComputeToolbarsSignature(CurrentSignature);
  Result := ComputeSignatureSingleComponent(trbBarSize, Result);
  Result := ComputeSignatureSingleComponent(trbIconSize, Result);
  Result := ComputeSignatureSingleComponent(cbFlatButtons, Result);
  Result := ComputeSignatureSingleComponent(cbReportErrorWithCommands, Result);
end;

function TfrmOptionsToolbar.GetTopToolbar: TKASToolBar;
begin
  if pnToolbars.ControlCount > 0 then
    Result := pnToolbars.Controls[0] as TKASToolBar
  else
    Result := nil;
end;

procedure TfrmOptionsToolbar.Init;
var
  ToolBar: TKASToolBar;
begin
  bFirstTimeDrawn := True;
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
  {$IFNDEF MSWINDOWS}
  miExportSeparator1.free;
  miExportTopToTCIniKeep.free;
  miExportTopToTCIniNoKeep.free;
  miExportSeparator2.free;
  miExportTopToTCBarKeep.free;
  miExportTopToTCBarNoKeep.free;
  miExportSeparator3.free;
  miExportCurrentToTCIniKeep.free;
  miExportCurrentToTCIniNoKeep.free;
  miExportSeparator4.free;
  miExportCurrentToTCBarKeep.free;
  miExportCurrentToTCBarNoKeep.free;
  miImportSeparator.free;
  miImportTCINI.free;
  miImportTCBAR.free;
  {$ENDIF}
  trbBarSize.Position   := gToolBarButtonSize div 2;
  trbIconSize.Position  := gToolBarIconSize div 2;
  cbFlatButtons.Checked := gToolBarFlat;
  cbReportErrorWithCommands.Checked := gToolbarReportErrorWithCommands;

  lblBarSizeValue.Caption  := IntToStr(trbBarSize.Position*2);
  lblIconSizeValue.Caption := IntToStr(trbIconSize.Position*2);

  FCurrentButton := nil;
  CloseToolbarsBelowCurrentButton;

  ToolBar := GetTopToolbar;
  ToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MainToolbar', False);
  LoadToolbar(ToolBar, gConfig, ToolBarNode, tocl_FlushCurrentToolbarContent);
  if ToolBar.ButtonCount > 0 then
    PressButtonDown(ToolBar.Buttons[0]);
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper,mp_PATHHELPER,nil);
  gSupportForVariableHelperMenu.PopulateMenuWithVariableHelper(pmparameteresHelper,edtExternalParameters);

  FUpdateHotKey := False;
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
        edtToolTip.Text:=StringReplace(NormalItem.Hint, #$0A, '\n', [rfReplaceAll]);

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

    DisplayAppropriateControls(EnableNormal, EnableCommand, EnableProgram);
  finally
    EnableAutoSizing;
  end;

  //Let's display the menuitem related with a subtoolbar only if current selected toolbar is a subtoolbar.
  miExportCurrent.Enabled := Assigned(FCurrentButton) and (FCurrentButton.ToolBar.Tag > 1);
  {$IFDEF MSWINDOWS}
  miImportTCINIAddCurrent.Enabled := miExportCurrent.Enabled;
  miImportTCINIAddMenuCurrent.Enabled := miExportCurrent.Enabled;
  miImportTCBARAddCurrent.Enabled := miExportCurrent.Enabled;
  miImportTCBARAddMenuCurrent.Enabled := miExportCurrent.Enabled;
  {$ENDIF}
  miImportDCBARAddCurrent.Enabled := miExportCurrent.Enabled;
  miImportDCBARAddMenuCurrent.Enabled := miExportCurrent.Enabled;
  miImportBackupAddCurrent.Enabled := miExportCurrent.Enabled;
  miImportBackupAddMenuCurrent.Enabled := miExportCurrent.Enabled;
end;

procedure TfrmOptionsToolbar.DisplayAppropriateControls(EnableNormal, EnableCommand, EnableProgram: boolean);
begin
  lblIconFile.Visible := EnableNormal;
  edtIconFileName.Visible := EnableNormal;
  btnOpenIcon.Visible := EnableNormal;
  btnRelativeIconFileName.Visible := EnableNormal;
  lblToolTip.Visible := EnableNormal;
  edtToolTip.Visible := EnableNormal;
  btnSuggestionTooltip.Visible := EnableNormal;
  lblInternalCommand.Visible := EnableCommand;
  cbInternalCommand.Visible := EnableCommand;
  btnOpenCmdDlg.Visible := EnableCommand;
  lblHelpOnInternalCommand.Visible := EnableCommand;
  lblInternalParameters.Visible := EnableCommand;
  edtInternalParameters.Visible := EnableCommand;
  lblExternalCommand.Visible := EnableProgram;
  edtExternalCommand.Visible := EnableProgram;
  lblExternalParameters.Visible := EnableProgram;
  edtExternalParameters.Visible := EnableProgram;
  btnParametersHelper.Visible := EnableProgram;

  lblStartPath.Visible := EnableProgram;
  edtStartPath.Visible := EnableProgram;
  btnOpenFile.Visible := EnableProgram;
  btnRelativeExternalCommand.Visible := EnableProgram;
  btnStartPath.Visible := EnableProgram;
  btnRelativeStartPath.Visible := EnableProgram;
  lblHotkey.Visible := EnableNormal;
  lblHotkeyValue.Visible := EnableNormal;
  btnEditHotkey.Visible := EnableNormal;
  btnRemoveHotkey.Visible := EnableNormal;
  btnCloneButton.Visible := Assigned(FCurrentButton);
  btnDeleteButton.Visible := Assigned(FCurrentButton);
  rgToolItemType.Visible := Assigned(FCurrentButton);
end;

procedure TfrmOptionsToolbar.LoadToolbar(ToolBar: TKASToolBar; Config: TXmlConfig; RootNode: TXmlNode; ConfigurationLoadType: TTypeOfConfigurationLoad);
var
  ToolBarLoader: TKASToolBarExtendedLoader;
begin
  ToolBarLoader := TKASToolBarExtendedLoader.Create(FFormCommands);
  try
    if Assigned(RootNode) then
      ToolBar.LoadConfiguration(Config, RootNode, ToolBarLoader, ConfigurationLoadType);
  finally
    ToolBarLoader.Free;
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
      1: ToolItem := TKASCommandItem.Create(FFormCommands);
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
  gToolbarReportErrorWithCommands := cbReportErrorWithCommands.Checked;
  gToolBarButtonSize := trbBarSize.Position * 2;
  gToolBarIconSize   := trbIconSize.Position * 2;

  ToolBar := GetTopToolbar;
  if Assigned(ToolBar) then
  begin
    ToolBarNode := gConfig.FindNode(gConfig.RootNode, 'Toolbars/MainToolbar', True);
    gConfig.ClearNode(ToolBarNode);
    Toolbar.SaveConfiguration(gConfig, ToolBarNode);
  end;

  if FUpdateHotKey then
  begin
    FUpdateHotKey := False;
    HotMan.Save(gpCfgDir + gNameSCFile);
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
  Result := TKASToolBar.Create(pnToolbars);
  Result.AutoSize                := True;
  Result.Anchors := [akTop, akLeft, akRight];
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
  Result.EdgeInner := esRaised;
  Result.EdgeOuter := esLowered;
  Result.EdgeBorders := [ebBottom];

  Result.Top := MaxSmallInt; // So that it is put under all existing toolbars (because of Align=alTop).

  Result.UseItems(Items);
  Result.Parent := pnToolbars;
  Result.Tag := pnToolbars.ComponentCount;
end;

function TfrmOptionsToolbar.AddNewSubToolbar(ToolItem: TKASMenuItem; bIncludeButtonOnNewBar:boolean=True): TKASToolBar;
begin
  Result := CreateToolbar(ToolItem.SubItems);
  if bIncludeButtonOnNewBar then
    if Result.ButtonCount = 0 then
      Result.AddButton(TKASCommandItem.Create(FFormCommands));
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
      NormalItem.Hint := StringReplace(edtToolTip.Text, '\n', #$0A, [rfReplaceAll]);
      NormalItem.Text := EmptyStr;
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
  pmInsertButtonMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmOptionsToolbar.btnOpenCmdDlgClick }
procedure TfrmOptionsToolbar.btnOpenCmdDlgClick(Sender: TObject);
var
  WantedCommand: String = '';
  IndexMaybe:longint;
begin
  if cbInternalCommand.ItemIndex=-1 then
    begin
      cbInternalCommand.ItemIndex:=0;
      cbInternalCommandSelect(cbInternalCommand);
    end;

  if ShowMainCommandDlgForm(cbInternalCommand.Items.Strings[cbInternalCommand.ItemIndex],WantedCommand) then
  begin
    IndexMaybe:=cbInternalCommand.Items.IndexOf(WantedCommand);
    if IndexMaybe<>-1 then
    begin
      cbInternalCommand.ItemIndex:=IndexMaybe;
      cbInternalCommandSelect(cbInternalCommand);
    end;
  end;
end;

procedure TfrmOptionsToolbar.btnParametersHelperClick(Sender: TObject);
begin
  pmparameteresHelper.PopUp;
end;

procedure TfrmOptionsToolbar.miInsertButtonClick(Sender: TObject);
var
  ToolBar: TKASToolBar;
  ToolItem: TKASToolItem = nil;
  WhereToAdd:longint;
  IndexWhereToAdd:longint;
begin
  if Assigned(FCurrentButton) then
  begin
    ApplyEditControls;
    ToolBar := FCurrentButton.ToolBar;
  end
  else
  begin
    ToolBar := GetTopToolbar;
  end;

  if Assigned(ToolBar) then
  begin
    with Sender as TComponent do
    begin
      case ((tag shr 4) and $0F) of
        1: ToolItem := TKASSeparatorItem.Create;
        2: ToolItem := TKASCommandItem.Create(FFormCommands);
        3: ToolItem := TKASProgramItem.Create;
        4: ToolItem := TKASMenuItem.Create;
      end;

      WhereToAdd:=tag and $0F;
    end;

    IndexWhereToAdd:=0;
    if (ToolBar.ButtonCount=0) then IndexWhereToAdd:=-1;
    if (IndexWhereToAdd=0) AND  (WhereToAdd=4) then IndexWhereToAdd:=-1;
    if Assigned(FCurrentButton) then
    begin
      if (IndexWhereToAdd=0) AND  (WhereToAdd=3) AND (FCurrentButton.Tag=pred(ToolBar.ButtonCount)) then IndexWhereToAdd:=-1;
      if (IndexWhereToAdd=0) AND  (WhereToAdd=3) then IndexWhereToAdd:=(FCurrentButton.Tag+1);
      if (IndexWhereToAdd=0) AND  (WhereToAdd=2) then IndexWhereToAdd:=FCurrentButton.Tag;
    end;

    if IndexWhereToAdd=-1 then
    begin
      //We simply add the button at the end
      FCurrentButton := ToolBar.AddButton(ToolItem);
    end
    else
    begin
      //We add the button *after* the current selected button
      FCurrentButton := ToolBar.InsertButton(IndexWhereToAdd, ToolItem);
    end;
    PressButtonDown(FCurrentButton);

    //Let's speed up process if we can pre-open requester according to what was just inserted as new button
    with Sender as TComponent do
    begin
      case ((tag shr 4) and $0F) of
        2: btnOpenCmdDlgClick(btnOpenCmdDlg);
        3: btnOpenFileClick(btnOpenFile);
      end;
    end;
  end;
end;

{ TfrmOptionsToolbar.miSrcRplClick }
procedure TfrmOptionsToolbar.miSrcRplClick(Sender: TObject);
const
  SaRMASK_ICON = $01;
  SaRMASK_COMMAND = $02;
  SaRMASK_PARAMS = $04;
  SaRMASK_STARTPATH = $08;
var
  ActionDispatcher, NbOfReplacement:integer;
  sSearchText, sReplaceText:string;
  ReplaceFlags: TReplaceFlags;

  function ReplaceIfNecessary(sWorkingText:string):string;
  begin
    result := StringReplace(sWorkingText, sSearchText, sReplaceText, ReplaceFlags);
    if result<>sWorkingText then inc(NbOfReplacement);
  end;

  procedure PossiblyRecursiveSearchAndReplaceInThisButton(ToolItem: TKASToolItem);
  var
    IndexItem, IndexParam: integer;
  begin
    if ToolItem is TKASSeparatorItem then
    begin
    end;

    if ToolItem is TKASCommandItem then
    begin
      if (ActionDispatcher AND SaRMASK_ICON) <> 0 then TKASCommandItem(ToolItem).Icon:=ReplaceIfNecessary(TKASCommandItem(ToolItem).Icon);
      if (ActionDispatcher AND SaRMASK_PARAMS) <> 0 then
        for IndexParam:=0 to pred(length(TKASCommandItem(ToolItem).Params)) do
          TKASCommandItem(ToolItem).Params[IndexParam]:=ReplaceIfNecessary(TKASCommandItem(ToolItem).Params[IndexParam]);
    end;

    if ToolItem is TKASProgramItem then
    begin
      if (ActionDispatcher AND SaRMASK_ICON) <> 0 then TKASProgramItem(ToolItem).Icon:=ReplaceIfNecessary(TKASProgramItem(ToolItem).Icon);
      if (ActionDispatcher AND SaRMASK_COMMAND) <> 0 then TKASProgramItem(ToolItem).Command:=ReplaceIfNecessary(TKASProgramItem(ToolItem).Command);
      if (ActionDispatcher AND SaRMASK_STARTPATH) <> 0 then TKASProgramItem(ToolItem).StartPath:=ReplaceIfNecessary(TKASProgramItem(ToolItem).StartPath);
      if (ActionDispatcher AND SaRMASK_PARAMS) <> 0 then TKASProgramItem(ToolItem).Params:=ReplaceIfNecessary(TKASProgramItem(ToolItem).Params);
    end;

    if ToolItem is TKASMenuItem then
    begin
      if (ActionDispatcher AND SaRMASK_ICON) <> 0 then TKASMenuItem(ToolItem).Icon:=ReplaceIfNecessary(TKASMenuItem(ToolItem).Icon);
      for IndexItem := 0 to pred(TKASMenuItem(ToolItem).SubItems.Count) do
        PossiblyRecursiveSearchAndReplaceInThisButton(TKASMenuItem(ToolItem).SubItems[IndexItem]);
    end;
  end;

var
  //Placed intentionnally *AFTER* above routine to make sure these variable names are not used in above possibly recursive routines.
  IndexButton: integer;
  Toolbar: TKASToolbar;
  EditSearchOptionToOffer: TEditSearchDialogOption = [];
  EditSearchOptionReturned: TEditSearchDialogOption = [];
begin
  with Sender as TComponent do ActionDispatcher:=tag;

  ApplyEditControls;
  Application.ProcessMessages;

  if ((ActionDispatcher AND SaRMASK_ICON) <>0) AND (edtIconFileName.Visible) AND (edtIconFileName.Text<>'') then sSearchText:=edtIconFileName.Text else
    if ((ActionDispatcher AND SaRMASK_COMMAND) <>0) AND (edtExternalCommand.Visible) AND (edtExternalCommand.Text<>'') then sSearchText:=edtExternalCommand.Text else
      if ((ActionDispatcher AND SaRMASK_PARAMS) <>0) AND (edtExternalParameters.Visible) AND (edtExternalParameters.Text<>'') then sSearchText:=edtExternalParameters.Text else
        if ((ActionDispatcher AND SaRMASK_STARTPATH) <>0) AND (edtStartPath.Visible) AND (edtStartPath.Text<>'') then sSearchText:=edtStartPath.Text else
          if ((ActionDispatcher AND SaRMASK_PARAMS) <>0) AND (edtInternalParameters.Visible) AND (edtInternalParameters.Lines.Count>0) then sSearchText:=edtInternalParameters.Lines.Strings[0] else
            sSearchText:='';
  sReplaceText:=sSearchText;

  {$IFDEF MSWINDOWS}
  EditSearchOptionToOffer:=EditSearchOptionToOffer+[eswoCaseSensitiveUnchecked];
  {$ELSE}
  EditSearchOptionToOffer:=EditSearchOptionToOffer+[eswoCaseSensitiveChecked];
  {$ENDIF}

  if GetSimpleSearchAndReplaceString(self, EditSearchOptionToOffer, sSearchText, sReplaceText, EditSearchOptionReturned, glsSearchPathHistory, glsReplacePathHistory) then
  begin
    NbOfReplacement:=0;
    ReplaceFlags:=[rfReplaceAll];
    if eswoCaseSensitiveUnchecked in EditSearchOptionReturned then ReplaceFlags := ReplaceFlags + [rfIgnoreCase];
    Toolbar := GetTopToolbar;

    //Let's scan the current bar!
    for IndexButton := 0 to pred(Toolbar.ButtonCount) do
    begin
      PossiblyRecursiveSearchAndReplaceInThisButton(Toolbar.Buttons[IndexButton].ToolItem);
      ToolBar.UpdateIcon(Toolbar.Buttons[IndexButton]);
    end;

    if NbOfReplacement=0 then
    begin
      msgOk(rsZeroReplacement);
    end
    else
    begin
      if ToolBar.ButtonCount > 0 then
        PressButtonDown(ToolBar.Buttons[0]);
      msgOk(format(rsXReplacements,[NbOfReplacement]));
    end;
  end;
end;

procedure TfrmOptionsToolbar.btnRelativeExternalCommandClick(Sender: TObject);
begin
  edtExternalCommand.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(edtExternalCommand,pfFILE);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TfrmOptionsToolbar.btnRelativeIconFileNameClick(Sender: TObject);
begin
  edtIconFileName.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(edtIconFileName,pfFILE);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TfrmOptionsToolbar.btnRelativeStartPathClick(Sender: TObject);
begin
  edtStartPath.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(edtStartPath,pfPATH);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
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
  if Assigned(FCurrentButton) then
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
      FUpdateHotKey:= True;
    end;
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

    if FCurrentButton.Tag < pred(FCurrentButton.ToolBar.ButtonCount) then
      Button := FCurrentButton.ToolBar.InsertButton((FCurrentButton.Tag + 1), SourceItem.Clone)
    else
      Button := FCurrentButton.ToolBar.AddButton(SourceItem.Clone);

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
  if Assigned(FCurrentButton) then
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
          FUpdateHotKey:= True;
        end;
      finally
        TemplateHotkey.Free;
      end;
    end;
  end;
end;

procedure TfrmOptionsToolbar.btnOpenFileClick(Sender: TObject);
begin
  OpenDialog.DefaultExt:= EmptyStr;
  OpenDialog.Filter:= EmptyStr;
  if edtExternalCommand.Text<>'' then OpenDialog.InitialDir:=ExtractFilePath(edtExternalCommand.Text);
  if OpenDialog.Execute then
    begin
      edtExternalCommand.Text := OpenDialog.FileName;
      edtStartPath.Text       := ExtractFilePath(OpenDialog.FileName);
      edtIconFileName.Text    := OpenDialog.FileName;
      edtToolTip.Text         := ExtractOnlyFileName(OpenDialog.FileName);
    end;
end;

procedure TfrmOptionsToolbar.btnStartPathClick(Sender: TObject);
var
  MaybeResultingOutputPath:string;
begin
  MaybeResultingOutputPath:=edtStartPath.Text;
  if MaybeResultingOutputPath='' then MaybeResultingOutputPath:=frmMain.ActiveFrame.CurrentPath;
  if SelectDirectory(rsSelectDir, MaybeResultingOutputPath, MaybeResultingOutputPath, False) then
    edtStartPath.Text:=MaybeResultingOutputPath;
end;

{ TfrmOptionsToolbar.btnSuggestionTooltipClick }
procedure TfrmOptionsToolbar.btnSuggestionTooltipClick(Sender: TObject);
var
  sSuggestion : string;
  iLineIndex, pOriginalSuggestion : integer;
begin
  sSuggestion:=EmptyStr;

  case rgToolItemType.ItemIndex of
    1: //Internal command: Idea is to keep the existing one for the single first line, then add systematically the parameters.
    begin
      pOriginalSuggestion:=pos('\n',edtToolTip.Text);
      if pOriginalSuggestion<>0 then
        sSuggestion:=leftstr(edtToolTip.Text,pred(pOriginalSuggestion))+'\n----'
      else
        sSuggestion:=edtToolTip.Text+'\n----';

      if edtInternalParameters.Lines.Count>0 then
      begin
        for iLineIndex:=0 to pred(edtInternalParameters.Lines.Count) do
          sSuggestion:=sSuggestion+'\n'+edtInternalParameters.Lines.Strings[iLineIndex];
      end;
    end;

    2://External command: Idea is to keep the existing one for the first line, then add systematically command, parameters and start path, one per line.
    begin
      pOriginalSuggestion:=pos(('\n----\n'+StringReplace(lblExternalCommand.Caption, '&', '', [rfReplaceAll])),edtToolTip.Text);
      if pOriginalSuggestion<>0 then
        sSuggestion:=leftstr(edtToolTip.Text,pred(pOriginalSuggestion))+'\n----\n'
      else
        sSuggestion:=edtToolTip.Text+'\n----\n';

      sSuggestion:=sSuggestion+StringReplace(lblExternalCommand.Caption, '&', '', [rfReplaceAll])+' '+edtExternalCommand.Text;
      if edtExternalParameters.Text<>EmptyStr then sSuggestion:=sSuggestion+'\n'+StringReplace(lblExternalParameters.Caption, '&', '', [rfReplaceAll])+' '+edtExternalParameters.Text;
      if edtStartPath.Text<>EmptyStr then sSuggestion:=sSuggestion+'\n'+StringReplace(lblStartPath.Caption, '&', '', [rfReplaceAll])+' '+edtStartPath.Text;
    end;
  end;

  if sSuggestion<>EmptyStr then edtToolTip.Text:=sSuggestion;
end;

procedure TfrmOptionsToolbar.cbInternalCommandSelect(Sender: TObject);
var
  Command: String;
begin
  Command := cbInternalCommand.Items[cbInternalCommand.ItemIndex];
  edtToolTip.Text := FFormCommands.GetCommandCaption(Command, cctLong);
  edtIconFileName.Text := UTF8LowerCase(Command);
end;

procedure TfrmOptionsToolbar.CloseToolbarsBelowCurrentButton;
var
  CloseFrom: Integer = 1;
  i: Integer;
begin
  if Assigned(FCurrentButton) then
  begin
    for i := 0 to pnToolbars.ControlCount - 1 do
      if pnToolbars.Controls[i] = FCurrentButton.ToolBar then
      begin
        CloseFrom := i + 1;
        Break;
      end;
  end;
  for i := pnToolbars.ControlCount - 1 downto CloseFrom do
    CloseToolbar(i);
end;

procedure TfrmOptionsToolbar.CloseToolbar(Index: Integer);
begin
  if Index > 0 then
    pnToolbars.Controls[Index].Free;
end;

procedure TfrmOptionsToolbar.cbFlatButtonsChange(Sender: TObject);
var
  i: Integer;
  ToolBar: TKASToolBar;
begin
  for i := 0 to pnToolbars.ControlCount - 1 do
  begin
    ToolBar := pnToolbars.Controls[i] as TKASToolBar;
    ToolBar.Flat := cbFlatButtons.Checked;
  end;
end;

procedure TfrmOptionsToolbar.edtIconFileNameChange(Sender: TObject);
begin
  if not FUpdatingIconText then
    UpdateIcon(edtIconFileName.Text);
end;

procedure TfrmOptionsToolbar.lblHelpOnInternalCommandClick(Sender: TObject);
begin
  ShowHelpForKeywordWithAnchor('/cmds.html#' + cbInternalCommand.Text);
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
    for i := 0 to pnToolbars.ControlCount - 1 do
    begin
      ToolBar := pnToolbars.Controls[i] as TKASToolBar;
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
    for i := 0 to pnToolbars.ControlCount - 1 do
    begin
      ToolBar := pnToolbars.Controls[i] as TKASToolBar;
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
  if Assigned(FCurrentButton) then
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
    begin
      if (SourceButton = FCurrentButton) then FCurrentButton := nil;
      SourceButton.ToolBar.MoveButton(SourceButton, TargetToolbar, nil);
    end;
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
    Result := PixMapManager.LoadBitmapEnhanced(TKASNormalItem(ToolItem).Icon, iIconSize, True, clBackColor, nil)
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
  if pnToolbars.ControlCount > 0 then
  begin
    ToolBar := pnToolbars.Controls[0] as TKASToolBar;
    if (ButtonNumber >= 0) and (ButtonNumber < Toolbar.ButtonCount) then
    begin
      FCurrentButton := Toolbar.Buttons[ButtonNumber];
      PressButtonDown(FCurrentButton);
    end;
  end;
end;

{ TfrmOptionsToolbar.FrameEnter }
procedure TfrmOptionsToolbar.FrameEnter(Sender: TObject);
begin
  //Tricky pass to don't have the "pnlEditToolbar" being continously resized depending on the button task we're going through.
  //The idea is to have system arrange for the "CommandItem", which is the taller size one, then freeze size there and keep this way.
  if bFirstTimeDrawn then
  begin
    bFirstTimeDrawn := False;
    DisplayAppropriateControls(True, True, False);
    Application.ProcessMessages;
    pnlEditToolbar.AutoSize := False;
    LoadCurrentButton;
  end;
end;

{ TfrmOptionsToolbar.ComputeToolbarsSignature }
// Routine tries to pickup all char chain from element of toolbar toolbar and compute a unique CRC32.
// This CRC32 will bea kind of signature of the toolbar.
// We compute the CRC32 at the start of edition and at the end.
// If they are different, it's a sign that toolbars have been modified.
// It's not "perfect" since it might happen that two different combinaisons will
// give the same CRC32 but odds are very good that it will be a different one.
function TfrmOptionsToolbar.ComputeToolbarsSignature(Seed:dword): dword;
const
  CONSTFORTOOLITEM: array[1..4] of byte = ($23, $35, $28, $DE);

  procedure RecursiveGetSignature(ToolItem: TKASToolItem; var Result: dword);
  var
    IndexToolItem: longint;
    sInnerParam: string;
  begin
    if ToolItem is TKASSeparatorItem then
      Result := crc32(Result, @CONSTFORTOOLITEM[1], 1);
    if ToolItem is TKASCommandItem then
    begin
      Result := crc32(Result, @CONSTFORTOOLITEM[2], 1);
      if length(TKASCommandItem(ToolItem).Icon) > 0 then
        Result := crc32(Result, @TKASCommandItem(ToolItem).Icon[1], length(TKASCommandItem(ToolItem).Icon));
      if length(TKASCommandItem(ToolItem).Hint) > 0 then
        Result := crc32(Result, @TKASCommandItem(ToolItem).Hint[1], length(TKASCommandItem(ToolItem).Hint));
      if length(TKASCommandItem(ToolItem).Command) > 0 then
        Result := crc32(Result, @TKASCommandItem(ToolItem).Command[1], length(TKASCommandItem(ToolItem).Command));
      for sInnerParam in TKASCommandItem(ToolItem).Params do
        Result := crc32(Result, @sInnerParam[1], length(sInnerParam));
    end;
    if ToolItem is TKASProgramItem then
    begin
      Result := crc32(Result, @CONSTFORTOOLITEM[3], 1);
      if length(TKASProgramItem(ToolItem).Icon) > 0 then
        Result := crc32(Result, @TKASProgramItem(ToolItem).Icon[1], length(TKASProgramItem(ToolItem).Icon));
      if length(TKASProgramItem(ToolItem).Hint) > 0 then
        Result := crc32(Result, @TKASProgramItem(ToolItem).Hint[1], length(TKASProgramItem(ToolItem).Hint));
      if length(TKASProgramItem(ToolItem).Command) > 0 then
        Result := crc32(Result, @TKASProgramItem(ToolItem).Command[1], length(TKASProgramItem(ToolItem).Command));
      if length(TKASProgramItem(ToolItem).Params) > 0 then
        Result := crc32(Result, @TKASProgramItem(ToolItem).Params[1], length(TKASProgramItem(ToolItem).Params));
      if length(TKASProgramItem(ToolItem).StartPath) > 0 then
        Result := crc32(Result, @TKASProgramItem(ToolItem).StartPath[1], length(TKASProgramItem(ToolItem).StartPath));
    end;
    if ToolItem is TKASMenuItem then
    begin
      Result := crc32(Result, @CONSTFORTOOLITEM[4], 1);
      if length(TKASMenuItem(ToolItem).Icon) > 0 then
        Result := crc32(Result, @TKASMenuItem(ToolItem).Icon[1], length(TKASMenuItem(ToolItem).Icon));
      if length(TKASMenuItem(ToolItem).Hint) > 0 then
        Result := crc32(Result, @TKASMenuItem(ToolItem).Hint[1], length(TKASMenuItem(ToolItem).Hint));

      for IndexToolItem := 0 to pred(TKASMenuItem(ToolItem).SubItems.Count) do
        RecursiveGetSignature(TKASMenuItem(ToolItem).SubItems[IndexToolItem], Result);
    end;
  end;

var
  IndexButton: longint;
  Toolbar: TKASToolBar;
begin
  ApplyEditControls;
  Toolbar := GetTopToolbar;
  Result := Seed;
  for IndexButton := 0 to pred(Toolbar.ButtonCount) do
    RecursiveGetSignature(Toolbar.Buttons[IndexButton].ToolItem, Result);
end;

{ TfrmOptionsToolbar.btnExportClick }
procedure TfrmOptionsToolbar.btnOtherClick(Sender: TObject);
begin
  pmOtherClickToolbar.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmOptionsToolbar.miImportAllDCCommandsClick }
// Will add on the top toolbar a button giving access to a sub menu with ALL the internal DC internal commands.
// This submenu will contain submenus entries, one per internal command category.
// This is mainly to help to validate run-time that each command has its own icon and so on.

procedure TfrmOptionsToolbar.miAddAllCmdsClick(Sender: TObject);
var
  slListCommands: TStringList;
  AToolbarConfig: TXmlConfig;
  ToolBarNode, RowNode, AllDCCommandsSubMenuNode, SubMenuNode, CommandCategoryNode, CommandNode: TXmlNode;
  MenuItemsNode: TXmlNode = nil; // We should preinitialize that one.
  IndexCommand: integer;
  bFlagCategoryTitle: boolean = False;
  sCmdName, sHintName, sHotKey, sCategory: string;
  ATopToolBar: TKASToolBar;
begin
  slListCommands := TStringList.Create;
  try
    // 1. Recuperate the list of all the DC internal commands.
    FFormCommands.GetCommandsListForACommandCategory(slListCommands, '(' + rsSimpleWordAll + ')', csLegacy);

    // 2. Create our XML structure to hold all our tree of sub menu and commands.
    AToolbarConfig := TXmlConfig.Create;
    try
      ToolBarNode := AToolbarConfig.FindNode(AToolbarConfig.RootNode, 'Toolbars/MainToolbar', True);
      AToolbarConfig.ClearNode(ToolBarNode);
      RowNode := AToolbarConfig.AddNode(ToolBarNode, 'Row');

      AllDCCommandsSubMenuNode := AToolbarConfig.AddNode(RowNode, 'Menu');
      AToolbarConfig.AddValue(AllDCCommandsSubMenuNode, 'ID', GuidToString(DCGetNewGUID));
      AToolbarConfig.AddValue(AllDCCommandsSubMenuNode, 'Icon', 'cm_doanycmcommand');
      AToolbarConfig.AddValue(AllDCCommandsSubMenuNode, 'Hint', rsMsgAllDCIntCmds);

      CommandCategoryNode := AToolbarConfig.AddNode(AllDCCommandsSubMenuNode, 'MenuItems');

      for IndexCommand := 0 to pred(slListCommands.Count) do
      begin
        FFormCommands.ExtractCommandFields(slListCommands.Strings[IndexCommand], sCategory, sCmdName, sHintName, sHotKey, bFlagCategoryTitle);

        if not bFlagCategoryTitle then
        begin
          if MenuItemsNode <> nil then
          begin
            CommandNode := AToolbarConfig.AddNode(MenuItemsNode, 'Command');
            AToolbarConfig.AddValue(CommandNode, 'ID', GuidToString(DCGetNewGUID));
            AToolbarConfig.AddValue(CommandNode, 'Icon', UTF8LowerCase(sCmdName));
            AToolbarConfig.AddValue(CommandNode, 'Command', sCmdName);
            AToolbarConfig.AddValue(CommandNode, 'Hint', sHintName);
          end;
        end
        else
        begin
          SubMenuNode := AToolbarConfig.AddNode(CommandCategoryNode, 'Menu');
          AToolbarConfig.AddValue(SubMenuNode, 'ID', GuidToString(DCGetNewGUID));
          AToolbarConfig.AddValue(SubMenuNode, 'Hint', sCmdName);
          //Let's take icon of first command of the category for the subtoolbar icon for this "new" category
          FFormCommands.ExtractCommandFields(slListCommands.Strings[IndexCommand + 1], sCategory, sCmdName, sHintName, sHotKey, bFlagCategoryTitle);
          AToolbarConfig.AddValue(SubMenuNode, 'Icon', UTF8LowerCase(sCmdName));
          MenuItemsNode := AToolbarConfig.AddNode(SubMenuNode, 'MenuItems');
        end;
      end;

      // 3. Now, we import our structure and at once, bang! we'll have added our bar and sub ones.
      ATopToolBar := GetTopToolbar;
      ToolBarNode := AToolbarConfig.FindNode(AToolbarConfig.RootNode, 'Toolbars/MainToolbar', False);
      if ToolBarNode <> nil then
      begin
        LoadToolbar(ATopToolBar, AToolbarConfig, ToolBarNode, tocl_AddToCurrentToolbarContent);
        if ATopToolBar.ButtonCount > 0 then
          PressButtonDown(ATopToolBar.Buttons[pred(ATopToolBar.ButtonCount)]); //Let's press the last added button since user might wants to complement what he just added
      end;

    finally
      FreeAndNil(AToolbarConfig);
    end;
  finally
    slListCommands.Free;
  end;
end;


{ TfrmOptionsToolbar.miExportToAnythingClick }
procedure TfrmOptionsToolbar.miExportToAnythingClick(Sender: TObject);
var
  ToolbarConfig: TXmlConfig;
  FlagKeepGoing: boolean = False;
  BackupPath: string;
  ToolBarNode: TXmlNode;
  ToolBar: TKASToolBar;
  InnerResult: boolean = False;
  ActionDispatcher: integer;

begin
  with Sender as TComponent do
    ActionDispatcher := tag;

  //1. Make we got an invalid name from the start
  SaveDialog.Filename := '';

  //2. Let's determine from which which level of toolbar we need to export
  ToolBar := GetTopToolbar;
  if (ActionDispatcher and MASK_ACTION_TOOLBAR) = ACTION_WITH_CURRENT_BAR then
  begin
    if Assigned(FCurrentButton) then
    begin
      ApplyEditControls;
      ToolBar := FCurrentButton.ToolBar;
    end;
  end;

  if Assigned(ToolBar) then
  begin
    //3. Let's get a filename for the export
    case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
      ACTION_WITH_DC_TOOLBARFILE:
      begin
        SaveDialog.DefaultExt := '*.toolbar';
        SaveDialog.FilterIndex := 1;
        SaveDialog.Title := rsMsgDCToolbarWhereToSave;
        SaveDialog.FileName := 'New DC Toolbar filename';
        FlagKeepGoing := SaveDialog.Execute;
      end;

      ACTION_WITH_BACKUP:
      begin
        BackupPath := IncludeTrailingPathDelimiter(mbExpandFileName(EnvVarConfigPath)) + 'Backup';
        if mbForceDirectory(BackupPath) then
        begin
          SaveDialog.Filename := BackupPath + DirectorySeparator + 'Backup_' + GetDateTimeInStrEZSortable(now) + '.toolbar';
          FlagKeepGoing := True;
        end;
      end;

      {$IFDEF MSWINDOWS}
      ACTION_WITH_WINCMDINI:
      begin
        if areWeInSituationToPlayWithTCFiles then
        begin
          SaveDialog.Filename := sTotalCommanderMainbarFilename;
          FlagKeepGoing := True;
        end;
      end;

      ACTION_WITH_TC_TOOLBARFILE:
      begin
        SaveDialog.DefaultExt := '*.BAR';
        SaveDialog.FilterIndex := 2;
        SaveDialog.Title := rsMsgTCToolbarWhereToSave;
        SaveDialog.FileName := 'New TC Toolbar filename';
        SaveDialog.InitialDir := ExcludeTrailingPathDelimiter(gTotalCommanderToolbarPath);
        FlagKeepGoing := SaveDialog.Execute;
        if FlagKeepGoing then
          FlagKeepGoing := areWeInSituationToPlayWithTCFiles;
      end;
      {$ENDIF}
    end;

    //4. Let's do the actual exportation
    if FlagKeepGoing and (SaveDialog.Filename <> '') then
    begin
      case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
        //If it's DC format, let's save the XML in regular fashion.
        ACTION_WITH_DC_TOOLBARFILE, ACTION_WITH_BACKUP:
        begin
          ToolbarConfig := TXmlConfig.Create(SaveDialog.Filename);
          try
            ToolBarNode := ToolbarConfig.FindNode(ToolbarConfig.RootNode, 'Toolbars/MainToolbar', True);
            ToolbarConfig.ClearNode(ToolBarNode);
            ToolBar.SaveConfiguration(ToolbarConfig, ToolBarNode);
            InnerResult := ToolbarConfig.Save;
          finally
            FreeAndNil(ToolbarConfig);
          end;
        end;

        {$IFDEF MSWINDOWS}
        //If it's TC format, we first create the necessary .BAR files.
        //If requested, we also update the Wincmd.ini file.
        ACTION_WITH_WINCMDINI, ACTION_WITH_TC_TOOLBARFILE:
        begin
          ExportDCToolbarsToTC(Toolbar,SaveDialog.Filename,((ActionDispatcher and MASK_FLUSHORNOT_EXISTING) = ACTION_FLUSH_EXISTING), ((actionDispatcher and MASK_ACTION_WITH_WHAT) = ACTION_WITH_WINCMDINI) );
          InnerResult := True;
        end;
        {$ENDIF}
      end;
    end;

    if InnerResult then
      msgOK(Format(rsMsgToolbarSaved, [SaveDialog.Filename]));
  end;
end;

{ TfrmOptionsToolbar.miImportFromAnythingClick }
// We can import elements to DC toolbar...
//   FROM...
//     -a previously exported DC .toolbar file
//     -a previously backuped DC .toolbar file
//     -the TC toolbar and subtoolbar right from the main toolbar in TC
//     -a specified TC toolbar file
//   TO...
//     -replace the top toolbar in DC
//     -extend the top toolbar in DC
//     -a subtoolbar of the top toolbar in DC
//     -replace the current selected toolbar in DC
//     -extend the current selected toolbar in DC
//     -a subtoolbar of the current selected in DC
procedure TfrmOptionsToolbar.miImportFromAnythingClick(Sender: TObject);
var
  ActionDispatcher: longint;
  FlagKeepGoing: boolean = False;
  BackupPath, ImportedToolbarHint: string;
  ImportDestination: byte;
  ToolBar: TKASToolBar;
  LocalKASMenuItem: TKASMenuItem;
  ToolbarConfig: TXmlConfig;
  ToolBarNode: TXmlNode;
begin
  with Sender as TComponent do
    ActionDispatcher := tag;

  //1o) Make sure we got the the filename to import into "OpenDialog.Filename" variable.
  case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
    {$IFDEF MSWINDOWS}
    ACTION_WITH_WINCMDINI:
    begin
      if areWeInSituationToPlayWithTCFiles then
      begin
        OpenDialog.Filename := sTotalCommanderMainbarFilename;
        ImportedToolbarHint := rsDefaultImportedTCToolbarHint;
        FlagKeepGoing := True;
      end;
    end;

    ACTION_WITH_TC_TOOLBARFILE:
    begin
      if areWeInSituationToPlayWithTCFiles then
      begin
        OpenDialog.DefaultExt := '*.BAR';
        OpenDialog.FilterIndex := 3;
        OpenDialog.Title := rsMsgToolbarLocateTCToolbarFile;
        ImportedToolbarHint := rsDefaultImportedTCToolbarHint;
        FlagKeepGoing := OpenDialog.Execute;
      end;
    end;
    {$ENDIF}

    ACTION_WITH_DC_TOOLBARFILE:
    begin
      OpenDialog.DefaultExt := '*.toolbar';
      OpenDialog.FilterIndex := 1;
      OpenDialog.Title := rsMsgToolbarLocateDCToolbarFile;
      ImportedToolbarHint := rsDefaultImportedDCToolbarHint;
      FlagKeepGoing := OpenDialog.Execute;
    end;

    ACTION_WITH_BACKUP:
    begin
      BackupPath := IncludeTrailingPathDelimiter(mbExpandFileName(EnvVarConfigPath)) + 'Backup';
      if mbForceDirectory(BackupPath) then
      begin
        OpenDialog.DefaultExt := '*.toolbar';
        OpenDialog.FilterIndex := 1;
        OpenDialog.InitialDir := ExcludeTrailingPathDelimiter(BackupPath);
        OpenDialog.Title := rsMsgToolbarRestoreWhat;
        ImportedToolbarHint := rsDefaultImportedDCToolbarHint;
        FlagKeepGoing := OpenDialog.Execute;
      end;
    end;
  end;

  //2o) If we got something valid, let's attempt to import it!
  if FlagKeepGoing then
  begin
    //3o) Let's make "Toolbar" hold the toolbar where to import in.
    ImportDestination := (ActionDispatcher and MASK_IMPORT_DESTIONATION);
    ImportDestination := ImportDestination shr 4;

    case ImportDestination of
      ACTION_WITH_MAIN_TOOLBAR:
      begin
        ToolBar := GetTopToolbar;
      end;

      ACTION_WITH_CURRENT_BAR:
      begin
        if Assigned(FCurrentButton) then
          Toolbar := FCurrentButton.ToolBar;
        if Toolbar = nil then
          ToolBar := GetTopToolbar;
      end;

      IMPORT_IN_MAIN_TOOLBAR_TO_NEW_SUB_BAR, IMPORT_IN_CURRENT_BAR_TO_NEW_SUB_BAR:
      begin
        case ImportDestination of
          IMPORT_IN_MAIN_TOOLBAR_TO_NEW_SUB_BAR:
          begin
            FCurrentButton := nil;
            ToolBar := GetTopToolbar;
            CloseToolbarsBelowCurrentButton;
          end;

          IMPORT_IN_CURRENT_BAR_TO_NEW_SUB_BAR:
          begin
            if Assigned(FCurrentButton) then
              Toolbar := FCurrentButton.ToolBar;
            if Toolbar = nil then
              ToolBar := GetTopToolbar;
          end;
        end;

        if FCurrentButton <> nil then
          FCurrentButton.Down := False;
        LocalKASMenuItem := TKASMenuItem.Create;
        LocalKASMenuItem.Icon := 'cm_configtoolbars';
        LocalKASMenuItem.Hint := ImportedToolbarHint;
        FCurrentButton := ToolBar.AddButton(LocalKASMenuItem);
        Toolbar := AddNewSubToolbar(LocalKASMenuItem, False);
      end;
    end;

    //4o) Let's attempt the actual import
    case (ActionDispatcher and MASK_ACTION_WITH_WHAT) of
      {$IFDEF MSWINDOWS}
      ACTION_WITH_WINCMDINI, ACTION_WITH_TC_TOOLBARFILE:
      begin
        ToolbarConfig := TXmlConfig.Create;
        try
          ConvertTCToolbarToDCXmlConfig(OpenDialog.FileName, ToolbarConfig);

          ToolBarNode := ToolbarConfig.FindNode(ToolbarConfig.RootNode, 'Toolbars/MainToolbar', False);
          if ToolBarNode <> nil then
          begin
            FCurrentButton := nil;
            if (ActionDispatcher and MASK_FLUSHORNOT_EXISTING) = ACTION_FLUSH_EXISTING then
              LoadToolbar(ToolBar, ToolbarConfig, ToolBarNode, tocl_FlushCurrentToolbarContent)
            else
              LoadToolbar(ToolBar, ToolbarConfig, ToolBarNode, tocl_AddToCurrentToolbarContent);

            if ToolBar.ButtonCount > 0 then
              PressButtonDown(ToolBar.Buttons[pred(ToolBar.ButtonCount)]); //Let's press the last added button since user might wants to complement what he just added
          end;
        finally
          FreeAndNil(ToolbarConfig);
        end;
      end;
      {$ENDIF}

      ACTION_WITH_DC_TOOLBARFILE, ACTION_WITH_BACKUP:
      begin
        ToolbarConfig := TXmlConfig.Create(OpenDialog.FileName, True);
        try
          ToolBarNode := ToolbarConfig.FindNode(ToolbarConfig.RootNode, 'Toolbars/MainToolbar', False);
          if ToolBarNode <> nil then
          begin
            FCurrentButton := nil;
            if (ActionDispatcher and MASK_FLUSHORNOT_EXISTING) = ACTION_FLUSH_EXISTING then
              LoadToolbar(ToolBar, ToolbarConfig, ToolBarNode, tocl_FlushCurrentToolbarContent)
            else
              LoadToolbar(ToolBar, ToolbarConfig, ToolBarNode, tocl_AddToCurrentToolbarContent);

            if ToolBar.ButtonCount > 0 then
              PressButtonDown(ToolBar.Buttons[pred(ToolBar.ButtonCount)]); //Let's press the last added button since user might wants to complement what he just added
          end;
        finally
          FreeAndNil(ToolbarConfig);
        end;
      end;
    end;
  end;
end;

end.
