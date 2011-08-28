{
   Double Commander
   -------------------------------------------------------------------------
   Implementing of Options dialog

   Copyright (C) 2006-2011  Koblov Alexander (Alexx2000@mail.ru)

   contributors:

   Radek Cervinka  <radek.cervinka@centrum.cz>

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

unit fOptions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, Spin, ColorBox,
  EditBtn, Grids, uGlobs, fOptionsFrame, uHotkeyManager;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    btnAutoConfig: TBitBtn;
    btnMultiArcAdd: TBitBtn;
    btnMultiArcDelete: TBitBtn;
    btnMultiArcRename: TBitBtn;
    btnMultiArcApply: TBitBtn;
    btnAddSel: TButton;
    btnAddSelWithPath: TButton;
    btnConfigApply: TBitBtn;
    btnConfigEdit: TBitBtn;
    btnSelLogFnt: TButton;
    btnSelEditFnt: TButton;
    btnSelMainFnt: TButton;
    btnSelViewFnt: TButton;
    btSetHotKey: TButton;
    btClearHotKey: TButton;
    btnNewColumnsSet: TButton;
    btnEditColumnsSet: TButton;
    btnDelColumnsSet: TButton;
    btnCopyColumnsSet: TButton;
    btnSelViewerBookFnt: TButton;
    btnBackViewerColor: TButton;
    btnFontViewerColor: TButton;
    cbSortCaseSensitive: TCheckBox;
    cbOnlyOnce: TCheckBox;
    cbDropReadOnlyFlag: TCheckBox;
    cbIconsShowOverlay: TCheckBox;
    cbIconsSize: TComboBox;
    cbLynxLike: TCheckBox;
    cbRenameSelOnlyName: TCheckBox;
    cbShowCopyTabSelectPanel: TCheckBox;
    cbShortFileSizeFormat: TCheckBox;
    cbTabsShowCloseButton: TCheckBox;
    cbFlatInterface: TCheckBox;
    cbFlatToolBar: TCheckBox;
    cbTabsAlwaysVisible: TCheckBox;
    cbTabsMultiLines: TCheckBox;
    cbTabsLimitOption: TCheckBox;
    cbLogWindow: TCheckBox;
    cbDirHistory: TCheckBox;
    cbCmdLineHistory: TCheckBox;
    cbFileMaskHistory: TCheckBox;
    cbSelectionByMouse: TCheckBox;
    cbTabsOpenForeground: TCheckBox;
    cbMinimizeToTray: TCheckBox;
    cbDateTimeFormat: TComboBox;
    cbbFileSystem: TComboBox;
    cbMouseMode: TComboBox;
    cbCutTextToColWidth: TCheckBox;
    cbProcessComments: TCheckBox;
    cbTabsConfirmCloseAll: TCheckBox;
    cbTabsLockedAsterisk: TCheckBox;
    cbTermWindow: TCheckBox;
    cbGridVertLine: TCheckBox;
    cbGridHorzLine: TCheckBox;
    cbShowWarningMessages: TCheckBox;
    cbShowSystemFiles: TCheckBox;
    cbSpaceMovesDown: TCheckBox;
    cbDirBrackets: TCheckBox;
    cbTabsActivateOnClick: TCheckBox;
    cbDeleteToTrash: TCheckBox;
    cbAlwaysShowTrayIcon: TCheckBox;
    cbSkipFileOpError: TCheckBox;
    cbShowDialogOnDragDrop: TCheckBox;
    cbListFilesInThread: TCheckBox;
    cbLoadIconsSeparately: TCheckBox;
    cbWatchExcludeDirs: TCheckBox;
    cbTabsOpenNearCurrent: TCheckBox;
    cbToolsRunInTerminal: TCheckBox;
    cbToolsKeepTerminalOpen: TCheckBox;
    cbToolsUseExternalProgram: TCheckBox;
    cbFreespaceInd: TCheckBox;
    cbProgInMenuBar: TCheckBox;
    cbPanelOfOperations: TCheckBox;
    cbShowDriveFreeSpace: TCheckBox;
    cbSaveThubnails: TCheckBox;
    cbShowMainMenu: TCheckBox;
    cbPartialNameSearch: TCheckBox;
    cgHKControls: TCheckGroup;
    chkMultiArcDebug: TCheckBox;
    chkMultiArcOutput: TCheckBox;
    chkSearchReplaceHistory: TCheckBox;
    chkSaveConfiguration: TCheckBox;
    chkMultiArcEnabled: TCheckBox;
    chkAutoFillColumns: TCheckBox;
    chkIgnoreEnable: TCheckBox;
    cmbTabsPosition: TComboBox;
    cmbAutoSizeColumn: TComboBox;
    cbSortMethod: TComboBox;
    cbBackgroundColorViewerBook: TColorBox;
    cbFontColorViewerBook: TColorBox;
    dlgFnt: TFontDialog;
    edHotKey: TEdit;
    edtViewerBookFont: TEdit;
    edtArchiveListEnd: TEdit;
    edtArchiveListStart: TEdit;
    edtArchiveAdd: TEdit;
    edtArchiveExtension: TEdit;
    edtArchiveExtract: TEdit;
    edtArchiveList: TEdit;
    edtDescription: TEdit;
    edtLogFont: TEdit;
    edtToolsParameters: TEdit;
    edtFilter: TEdit;
    edtParam: TEdit;
    edtTabsLimitLength: TEdit;
    edtCopyBufferSize: TEdit;
    edtEditorFontSize: TSpinEdit;
    edtMainFontSize: TSpinEdit;
    edtRunTerm: TEdit;
    edtRunInTerm: TEdit;
    edtMainFont: TEdit;
    edtEditorFont: TEdit;
    edtViewerFont: TEdit;
    edtViewerFontSize: TSpinEdit;
    cbLogFile: TCheckBox;
    edtLogFontSize: TSpinEdit;
    fneArchiver: TFileNameEdit;
    fneToolsPath: TFileNameEdit;
    fneSaveIn: TFileNameEdit;
    gbArchiverOptions: TGroupBox;
    gbExactNameMatch: TGroupBox;
    fneLogFileName: TFileNameEdit;
    gbLogFile: TGroupBox;
    gbLogFileOp: TGroupBox;
    gbLogFileStatus: TGroupBox;
    gbMisc1: TGroupBox;
    gbMisc2: TGroupBox;
    gbScreenLayout: TGroupBox;
    cbFlatDiskPanel: TCheckBox;
    cbShowMainToolBar: TCheckBox;
    cbShowCmdLine: TCheckBox;
    cbShowCurDir: TCheckBox;
    cbShowDiskPanel: TCheckBox;
    cbShowDriveMenuButton: TCheckBox;
    cbShowKeysPanel: TCheckBox;
    cbShowStatusBar: TCheckBox;
    cbShowTabHeader: TCheckBox;
    cbShowTabs: TCheckBox;
    cbTwoDiskPanels: TCheckBox;
    gbCopyBufferSize: TGroupBox;
    gbGeneralOptions: TGroupBox;
    gbDateTimeFormat: TGroupBox;
    gbTabs: TGroupBox;
    gbFileSearch: TGroupBox;
    gbLocConfigFiles: TGroupBox;
    gbSaveOnExit: TGroupBox;
    gbMisc3: TGroupBox;
    gbMisc4: TGroupBox;
    gbShowGrid: TGroupBox;
    gbExtended: TGroupBox;
    gbShowIconsMode: TGroupBox;
    gbAutoRefreshEnable: TGroupBox;
    gbAutoRefreshDisable: TGroupBox;
    gbSorting: TGroupBox;
    gbViewerExample: TGroupBox;
    gbViewerBookMode: TGroupBox;
    grpQuickSearchFilterKeys: TGroupBox;
    lblCmdLineConfigDir: TLabel;
    lblSCFiles: TLabel;
    lblNumberColumnsViewer: TLabel;
    lblFontColorViewerBook: TLabel;
    lblBackgroundColorViewerBook: TLabel;
    lblViewerBookFont: TLabel;
    lblSortMethod: TLabel;
    lblArchiveListEnd: TLabel;
    lblArchiveListStart: TLabel;
    lblArchiveAdd: TLabel;
    lblArchiveExtension: TLabel;
    lblArchiveExtract: TLabel;
    lblArchiveList: TLabel;
    lblArchiveListFormat: TLabel;
    lblArchiver: TLabel;
    lblDescription: TLabel;
    lblToolsPath: TLabel;
    lblToolsParameters: TLabel;
    lblAutoSizeColumn: TLabel;
    lblQuickSearch: TLabel;
    lblQuickFilter: TLabel;
    lblSaveIn: TLabel;
    lblLogFont: TLabel;
    lbxMultiArc: TListBox;
    lbSCFilesList: TListBox;
    memArchiveListFormat: TMemo;
    memIgnoreList: TMemo;
    pgToolTips: TPage;
    pbViewerBook: TPaintBox;
    pnlHotkeyButtons: TPanel;
    pnlMultiArcButtons: TPanel;
    pcArchiverCommands: TPageControl;
    pgArchivers: TPage;
    pgIgnoreList: TPage;
    pnlArchiverCommands: TPanel;
    rbScrollPageByPage: TRadioButton;
    rbScrollLineByLine: TRadioButton;
    rbScrollLineByLineCursor: TRadioButton;
    sbxMultiArc: TScrollBox;
    pnlQuickSearch: TPanel;
    pnlQuickFilter: TPanel;
    rbAltLetterQF: TRadioButton;
    rbCtrlAltLetterQF: TRadioButton;
    rbLetterQF: TRadioButton;
    rbNoneQF: TRadioButton;
    rbIconsShowAllAndExe: TRadioButton;
    rbIconsShowAll: TRadioButton;
    rbIconsShowNone: TRadioButton;
    rbIconsShowStandard: TRadioButton;
    pnlIconExample: TPanel;
    imgIconExample: TImage;
    lblTabsPosition: TLabel;
    lbPressedHotKeyCommand: TLabel;
    lbFilter: TLabel;
    ledDriveBlackList: TLabeledEdit;
    lbcategory: TLabel;
    lbcommands: TLabel;
    lblParam: TLabel;
    lblWipePassNumber: TLabel;
    lblMouseMode: TLabel;
    lblConfigColumns: TLabel;
    lbtypes: TLabel;
    lbxCategories: TListBox;
    lstColumnsSets: TListBox;
    pgIcons: TPage;
    pgAutoRefresh: TPage;
    pgMisc: TPage;
    pnlButtons: TPanel;
    pgColumns: TPage;
    gbScrolling: TGroupBox;
    rbCtrlAltLetterQS: TRadioButton;
    rbAltLetterQS: TRadioButton;
    rbNoneQS: TRadioButton;
    cbExactBeginning: TCheckBox;
    cbExactEnding: TCheckBox;
    rbLetterQS: TRadioButton;
    ilTreeView: TImageList;
    lblChar: TLabel;
    lblDateTimeExample: TLabel;
    lblDateTimeFormat: TLabel;
    lblCopyBufferSize: TLabel;
    lblEditorFont: TLabel;
    lblHotKey: TLabel;
    lblMainFont: TLabel;
    lblRunTerm: TLabel;
    lblRunInTerm: TLabel;
    lblViewerFont: TLabel;
    lngList: TListBox;
    cbLogArcOp: TCheckBox;
    cbLogCpMvLn: TCheckBox;
    cbLogDelete: TCheckBox;
    cbLogErrors: TCheckBox;
    cbLogDirOp: TCheckBox;
    cbLogVFS: TCheckBox;
    cbLogInfo: TCheckBox;
    cbLogSuccess: TCheckBox;
    nbNotebook: TNotebook;
    odOpenDialog: TOpenDialog;
    optColorDialog: TColorDialog;
    pgQuickSearch: TPage;
    pgConfigStorage: TPage;
    pgLogFile: TPage;
    pgTabs: TPage;
    pgFileOp: TPage;
    pgLayout: TPage;
    pgPlugins: TPage;
    pnlCaption: TPanel;
    Panel3: TPanel;
    Panel1: TPanel;
    btnOK: TBitBtn;
    btnApply: TBitBtn;
    btnCancel: TBitBtn;
    pgBehav: TPage;
    pgColor: TPage;
    pgFonts: TPage;
    pgHotKey: TPage;
    pgLng: TPage;
    pgTools: TPage;
    rbProgramDir: TRadioButton;
    rbUserHomeDir: TRadioButton;
    rbUseMmapInSearch: TRadioButton;
    rbUseStreamInSearch: TRadioButton;
    seWipePassNumber: TSpinEdit;
    seWheelScrollLines: TSpinEdit;
    edtViewerBookFontSize: TSpinEdit;
    seNumberColumnsViewer: TSpinEdit;
    splMultiArc: TSplitter;
    splOptionsSplitter: TSplitter;
    stgCommands: TStringGrid;
    gbIconsSize: TGroupBox;
    stgHotkeys: TStringGrid;
    stgTools: TStringGrid;
    stgArchiverCommands: TStringGrid;
    tbArchiverAdditional: TTabSheet;
    tbArchiverGeneral: TTabSheet;
    tvTreeView: TTreeView;
    cbWatchFileNameChange: TCheckBox;
    edtWatchExcludeDirs: TEdit;
    cbWatchOnlyForeground: TCheckBox;
    cbWatchAttributesChange: TCheckBox;
    procedure btnAddSelClick(Sender: TObject);
    procedure btnAddSelWithPathClick(Sender: TObject);
    procedure btnAutoConfigClick(Sender: TObject);
    procedure btnBackViewerColorClick(Sender: TObject);
    procedure btnConfigApplyClick(Sender: TObject);
    procedure btnConfigEditClick(Sender: TObject);
    procedure btnFontViewerColorClick(Sender: TObject);
    procedure btnMultiArcAddClick(Sender: TObject);
    procedure btnMultiArcApplyClick(Sender: TObject);
    procedure btnMultiArcDeleteClick(Sender: TObject);
    procedure btnMultiArcRenameClick(Sender: TObject);
    procedure btnSelViewerBookFntClick(Sender: TObject);
    procedure cbAlwaysShowTrayIconChange(Sender: TObject);
    procedure cbIconsSizeChange(Sender: TObject);
    procedure cbToolsKeepTerminalOpenChange(Sender: TObject);
    procedure cbToolsRunInTerminalChange(Sender: TObject);
    procedure cbToolsUseExternalProgramChange(Sender: TObject);
    procedure cbWatchExcludeDirsChange(Sender: TObject);
    procedure cgHKControlsItemClick(Sender: TObject; Index: integer);
    procedure chkIgnoreEnableChange(Sender: TObject);
    procedure chkMultiArcEnabledChange(Sender: TObject);
    procedure chkSaveConfigurationChange(Sender: TObject);
    procedure edtToolsParametersChange(Sender: TObject);
    procedure edtViewerBookFontSizeChange(Sender: TObject);
    procedure fneToolsPathAcceptFileName(Sender: TObject; var Value: String);
    procedure fneToolsPathChange(Sender: TObject);
    procedure lbSCFilesListSelectionChange(Sender: TObject; User: boolean);
    procedure lbxMultiArcSelectionChange(Sender: TObject; User: boolean);
    procedure OnAutoRefreshOptionChanged(Sender: TObject);
    procedure edHotKeyKeyPress(Sender: TObject; var Key: char);
    procedure btClearHotKeyClick(Sender: TObject);
    procedure btnCopyColumnsSetClick(Sender: TObject);
    procedure btnDelColumnsSetClick(Sender: TObject);
    procedure btnEditColumnsSetClick(Sender: TObject);
    procedure btnNewColumnsSetClick(Sender: TObject);
    procedure cbColorBoxChange(Sender: TObject);
    procedure cbDateTimeFormatChange(Sender: TObject);
    procedure cbShowDiskPanelChange(Sender: TObject);
    procedure cbTermWindowChange(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure edtEditorFontSizeChange(Sender: TObject);
    procedure edtMainFontSizeChange(Sender: TObject);
    procedure edtViewerFontSizeChange(Sender: TObject);
    procedure edtLogFontSizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSetHotKeyClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnSelEditFntClick(Sender: TObject);
    procedure btnSelMainFntClick(Sender: TObject);
    procedure btnSelViewFntClick(Sender: TObject);
    procedure btnSelLogFntClick(Sender: TObject);
    procedure edHotKeyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxCategoriesSelectionChange(Sender: TObject; User: boolean);
    procedure nbNotebookPageChanged(Sender: TObject);
    procedure pbViewerBookPaint(Sender: TObject);
    procedure pgBehavResize(Sender: TObject);
    procedure rbIconsShowNoneChange(Sender: TObject);
    procedure rbQuickSearchFilterKeyChange(Sender: TObject);
    procedure seNumberColumnsViewerChange(Sender: TObject);
    procedure stgArchiverCommandsPrepareCanvas(Sender: TObject; aCol,
      aRow: Integer; aState: TGridDrawState);
    procedure stgCommandsResize(Sender: TObject);
    procedure stgCommandsSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure stgHotkeysSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure stgToolsSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure tbArchiverAdditionalShow(Sender: TObject);
    procedure tvTreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    FOptionsEditorList: TOptionsEditorList;
    tmpExternalTools: TExternalToolsOptions;
    FUpdatingTools: Boolean;

    procedure DeleteHotkeyFromGrid(aHotkey: String);
    {en
       Refreshes all hotkeys from the Commands grid
    }
    procedure UpdateHotkeys(HMForm: THMForm);
    procedure UpdateHotkeysForCommand(HMForm: THMForm; RowNr: Integer);
    procedure ShowExternalToolOptions(ExtTool: TExternalTool);
    procedure FillSCFilesList;
    procedure CreateOptionsEditorList;
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
  public
    procedure FillLngListBox;
    procedure FillFontLists;
    procedure FillColumnsList;
    procedure FillCommandsPage;
    procedure FillIgnoreList(bWithFullPath: Boolean);
    procedure FillArchiverList;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure FillCommandList(lstFilter:string);// fill stringgrid
  end;

implementation

{$R *.lfm}

uses
  uLng, uGlobsPaths, uPixMapManager, fMain, LCLProc, LCLVersion,
  uColorExt, uDCUtils, uOSUtils, fColumnsSetConf, uShowMsg, uShowForm,
  uTypes, StrUtils, uFindEx, uKeyboard,
  fMaskInputDlg, uSearchTemplate, uMultiArc, uFile, uDebug,
  fOptionsPlugins, fOptionsToolTips, fOptionsColors;

const
  stgCmdCommandIndex = 0;
  stgCmdCommentIndex = 1;
  stgCmdHotkeysIndex = 2;

  stgArchiveTitle                = 0;
  stgArchiveDelete               = 1;
  stgArchiveTest                 = 2;
  stgArchiveExtractWithoutPath   = 3;
  stgArchiveSelfExtract          = 4;
  stgArchiveID                   = 5;
  stgArchiveIDPos                = 6;
  stgArchiveIDSeekRange          = 7;
  stgArchivePasswordQuery        = 8;
  stgArchiveFormMode             = 9;

const
  // Tools page: what tool is displayed in each row.
  ExtToolFromRow: array[0..2] of TExternalTool = (etViewer, etEditor, etDiffer);

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

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  FUpdatingTools := False;

  // Localize some ComboBox
  ParseLineToList(rsOptMouseSelectionButton, cbMouseMode.Items);
  ParseLineToList(rsOptAutoSizeColumn, cmbAutoSizeColumn.Items);
  ParseLineToList(rsOptTabsPosition, cmbTabsPosition.Items);
  ParseLineToList(rsOptSortMethod, cbSortMethod.Items);

  // Show configuration directory
  if gpCmdLineCfgDir = '' then
  begin
    rbProgramDir.Caption:= rbProgramDir.Caption + ' - [' + IncludeTrailingPathDelimiter(gpGlobalCfgDir) + ']';
    rbUserHomeDir.Caption:= rbUserHomeDir.Caption + ' - [' + IncludeTrailingPathDelimiter(GetAppConfigDir) + ']';
  end
  else
  begin
    rbProgramDir.Visible := False;
    rbProgramDir.Enabled := False;
    rbUserHomeDir.Visible := False;
    rbUserHomeDir.Enabled := False;
    lblCmdLineConfigDir.Visible := True;
    lblCmdLineConfigDir.Caption := lblCmdLineConfigDir.Caption + ' - [' + IncludeTrailingPathDelimiter(gpCmdLineCfgDir) + ']';
  end;

  // tvTreeView localization
  with tvTreeView.Items do
    begin
      Item[0].Text := rsOptLanguage;
      Item[1].Text := rsOptBehav;
      Item[2].Text := rsOptTools;
      Item[3].Text := rsOptFonts;
      Item[4].Text := rsOptColors;
      Item[5].Text := rsOptHotKeys;
      Item[6].Text := rsOptPlugins;
      Item[7].Text := rsOptLayout;
      Item[8].Text := rsOptFileOp;
      Item[9].Text := rsOptFolderTabs;
      Item[10].Text := rsOptLog;
      Item[11].Text := rsOptConfig;
      Item[12].Text := rsOptQuickSearch;
      Item[13].Text := rsOptColumns;
      Item[14].Text := rsOptMiscellaneous;
      Item[15].Text := rsOptAutoRefresh;
      Item[16].Text := rsOptIcons;
      Item[17].Text := rsOptIgnoreList;
      Item[18].Text := rsOptArchivers;
      Item[19].Text := rsOptTooltips;
    end;
  tvTreeView.Items.Item[0].Selected:= True;

  // Set QuickFilter radio buttons captions same as QuickSearch.
  rbCtrlAltLetterQF.Caption := rbCtrlAltLetterQS.Caption;
  rbAltLetterQF.Caption     := rbAltLetterQS.Caption;
  rbLetterQF.Caption        := rbLetterQS.Caption;
  rbNoneQF.Caption          := rbNoneQS.Caption;

  // Disable focus rectangle on tools grid.
  stgTools.FocusRectVisible := False;
  // Localize tools names.
  stgTools.Cells[0, stgTools.FixedRows + 0] := rsToolViewer;
  stgTools.Cells[0, stgTools.FixedRows + 1] := rsToolEditor;
  stgTools.Cells[0, stgTools.FixedRows + 2] := rsToolDiffer;
  // Enable/disable tools controls.
  FUpdatingTools := True;
  cbToolsUseExternalProgramChange(nil);
  FUpdatingTools := False;

  // Localize Hotkeys.
  // stgCommands is localized in FillCommandList.
  stgHotkeys.Columns.Items[0].Title.Caption := rsOptHotkeysHotkey;
  stgHotkeys.Columns.Items[1].Title.Caption := rsOptHotkeysParameters;

  // Localize additional archiver commands.
  stgArchiverCommands.Cells[0, stgArchiveTitle] := rsOptArchiveParam;
  stgArchiverCommands.Cells[1, stgArchiveTitle] := rsOptArchiveValue;
  stgArchiverCommands.Cells[0, stgArchiveDelete] := rsOptArchiveDelete;
  stgArchiverCommands.Cells[0, stgArchiveTest] := rsOptArchiveTest;
  stgArchiverCommands.Cells[0, stgArchiveExtractWithoutPath] := rsOptArchiveExtractWithoutPath;
  stgArchiverCommands.Cells[0, stgArchiveSelfExtract] := rsOptArchiveSelfExtract;
  stgArchiverCommands.Cells[0, stgArchiveID] := rsOptArchiveID;
  stgArchiverCommands.Cells[0, stgArchiveIDPos] := rsOptArchiveIDPos;
  stgArchiverCommands.Cells[0, stgArchiveIDSeekRange] := rsOptArchiveIDSeekRange;
  stgArchiverCommands.Cells[0, stgArchivePasswordQuery] := rsOptArchivePasswordQuery;
  stgArchiverCommands.Cells[0, stgArchiveFormMode] := rsOptArchiveFormMode;

  // Create and fill options editor list
  CreateOptionsEditorList;

  // Load all configuration
  LoadConfig;

  // Initialize property storage
  InitPropStorage(Self);
  // Let not warning on which page save form
  nbNotebook.PageIndex := 0;

  // Below needed until after we switch to Lazarus 0.9.31.
  nbNotebook.TabStop := True;
  {$if (lcl_release) < 31}
  nbNotebook.ShowTabs := False;
  nbNotebook.OnPageChanged := @nbNotebookPageChanged;
  {$endif}

  gbViewerBookMode.Enabled := not (cbToolsUseExternalProgram.Checked);
  FillSCFilesList;
end;

procedure TfrmOptions.btSetHotKeyClick(Sender: TObject);
var
  i: integer;
  sCommand: string;
  lslHotKeys:TStringList;
  sShortCut, sParam: String;
  HMForm: THMForm;
  HMControl: THMControl;
  hotkey: THotkey;
  isFormHotkey: Boolean;

begin
// ToDo: Black list HotKey which can't use

  if lbxCategories.ItemIndex=-1 then exit;
  if stgCommands.Row<1 then exit;

  sShortCut := edHotKey.Text;

  // check for invalid hotkey
  if sShortCut = EmptyStr then
    exit;

  sParam := edtParam.Text;
  sCommand := stgCommands.Cells[stgCmdCommandIndex, stgCommands.Row];

  if (lbPressedHotKeyCommand.Caption <> EmptyStr) then
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

  HMForm := HotMan.Forms.FindOrCreate(lbxCategories.Items[lbxCategories.ItemIndex]);
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

procedure TfrmOptions.cbColorBoxChange(Sender: TObject);
begin
  pbViewerBook.Repaint;
end;

procedure TfrmOptions.cbDateTimeFormatChange(Sender: TObject);
begin
  lblDateTimeExample.Caption:= FormatDateTime(cbDateTimeFormat.Text, Now);
end;

procedure TfrmOptions.cbShowDiskPanelChange(Sender: TObject);
begin
  cbTwoDiskPanels.Enabled := cbShowDiskPanel.Checked;
  cbFlatDiskPanel.Enabled := cbShowDiskPanel.Checked;
end;

procedure TfrmOptions.cbTermWindowChange(Sender: TObject);
begin
  if cbTermWindow.Checked then
    begin
      cbShowCmdLine.Tag:= Integer(cbShowCmdLine.Checked);
      cbShowCmdLine.Checked:= True;
      cbShowCmdLine.Enabled:= False;
    end
  else
    begin
      cbShowCmdLine.Checked:= Boolean(cbShowCmdLine.Tag);
      cbShowCmdLine.Enabled:= True;
    end;
end;

procedure TfrmOptions.edtFilterChange(Sender: TObject);
{< filtering active commands list
}
begin
  if lbxCategories.ItemIndex=-1 then exit;
  edHotKey.Clear;

  FillCommandList(edtFilter.Text);
end;

procedure TfrmOptions.edtEditorFontSizeChange(Sender: TObject);
begin
  edtEditorFont.Font.Size := edtEditorFontSize.Value;
end;

procedure TfrmOptions.edtMainFontSizeChange(Sender: TObject);
begin
  edtMainFont.Font.Size := edtMainFontSize.Value;
end;

procedure TfrmOptions.edtViewerFontSizeChange(Sender: TObject);
begin
  edtViewerFont.Font.Size := edtViewerFontSize.Value;
end;

procedure TfrmOptions.edtLogFontSizeChange(Sender: TObject);
begin
  edtLogFont.Font.Size := edtLogFontSize.Value;
end;

procedure TfrmOptions.edtViewerBookFontSizeChange(Sender: TObject);
begin
  edtViewerBookFont.Font.Size := edtViewerBookFontSize.Value;
end;

procedure TfrmOptions.FillLngListBox;
var
  fr:TSearchRecEx;
  iIndex:Integer;
  sLangName : String;
begin
  lngList.Clear;
  DCDebug('Language dir: ' + gpLngDir);
  if FindFirstEx(gpLngDir+'*.po', faAnyFile, fr)<>0 then
  begin
    FindCloseEx(fr);
    Exit;
  end;
  repeat
    sLangName := GetLanguageName(gpLngDir + fr.Name);
    lngList.Items.Add(Format('%s = (%s)', [fr.Name, sLangName]));
  until FindNextEx(fr)<>0;
  
  FindCloseEx(fr);

  iIndex:=lngList.Items.IndexOfName(gPOFileName + #32);
  if iIndex>=0 then
    lngList.Selected[iIndex]:=True;
end;

procedure TfrmOptions.btnOKClick(Sender: TObject);
begin
  // save all configuration
  SaveConfig;
  // write to config file
  SaveGlobs;
end;

procedure TfrmOptions.btnApplyClick(Sender: TObject);
begin
  // save all configuration
  SaveConfig;
  // write to config file
  SaveGlobs;
end;

procedure TfrmOptions.btnSelEditFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name  := edtEditorFont.Text;
  dlgFnt.Font.Size  := edtEditorFontSize.Value;
  dlgFnt.Font.Style := edtEditorFont.Font.Style;
  if dlgFnt.Execute then
    begin
      edtEditorFont.Text       := dlgFnt.Font.Name;
      edtEditorFontSize.Value  := dlgFnt.Font.Size;
      edtEditorFont.Font.Style := dlgFnt.Font.Style;
    end;
end;

procedure TfrmOptions.btnSelMainFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name  := edtMainFont.Text;
  dlgFnt.Font.Size  := edtMainFontSize.Value;
  dlgFnt.Font.Style := edtMainFont.Font.Style;
  if dlgFnt.Execute then
    begin
      edtMainFont.Text       := dlgFnt.Font.Name;
      edtMainFontSize.Value  := dlgFnt.Font.Size;
      edtMainFont.Font.Style := dlgFnt.Font.Style;
    end;
end;

procedure TfrmOptions.btnSelViewFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name  := edtViewerFont.Text;
  dlgFnt.Font.Size  := edtViewerFontSize.Value;
  dlgFnt.Font.Style := edtViewerFont.Font.Style;
  if dlgFnt.Execute then
    begin
      edtViewerFont.Text       := dlgFnt.Font.Name;
      edtViewerFontSize.Value  := dlgFnt.Font.Size;
      edtViewerFont.Font.Style := dlgFnt.Font.Style;
    end;
end;

procedure TfrmOptions.btnSelLogFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name  := edtLogFont.Text;
  dlgFnt.Font.Size  := edtLogFontSize.Value;
  dlgFnt.Font.Style := edtLogFont.Font.Style;
  if dlgFnt.Execute then
    begin
      edtLogFont.Text       := dlgFnt.Font.Name;
      edtLogFontSize.Value  := dlgFnt.Font.Size;
      edtLogFont.Font.Style := dlgFnt.Font.Style;
    end;
end;

procedure TfrmOptions.btnSelViewerBookFntClick(Sender: TObject);
begin
  dlgFnt.Font.Name  := edtViewerBookFont.Text;
  dlgFnt.Font.Size  := edtViewerBookFontSize.Value;
  dlgFnt.Font.Style := edtViewerBookFont.Font.Style;
  if dlgFnt.Execute then
    begin
      edtViewerBookFont.Text       := dlgFnt.Font.Name;
      edtViewerBookFontSize.Value  := dlgFnt.Font.Size;
      edtViewerBookFont.Font.Style := dlgFnt.Font.Style;
    end;
end;

procedure TfrmOptions.FillFontLists;
begin
  edtMainFont.Text   := gFonts[dcfMain].Name;
  edtEditorFont.Text := gFonts[dcfEditor].Name;
  edtViewerFont.Text := gFonts[dcfViewer].Name;
  edtLogFont.Text    := gFonts[dcfLog].Name;
  edtViewerBookFont.Text := gFonts[dcfViewerBook].Name;

  edtMainFontSize.Value   := gFonts[dcfMain].Size;
  edtEditorFontSize.Value := gFonts[dcfEditor].Size;
  edtViewerFontSize.Value := gFonts[dcfViewer].Size;
  edtLogFontSize.Value    := gFonts[dcfLog].Size;
  edtViewerBookFontSize.Value := gFonts[dcfViewerBook].Size;

  FontOptionsToFont(gFonts[dcfMain], edtMainFont.Font);
  FontOptionsToFont(gFonts[dcfEditor], edtEditorFont.Font);
  FontOptionsToFont(gFonts[dcfViewer], edtViewerFont.Font);
  FontOptionsToFont(gFonts[dcfLog], edtLogFont.Font);
  FontOptionsToFont(gFonts[dcfViewerBook], edtViewerBookFont.Font);
end;

procedure TfrmOptions.CheckHotKeyConflicts(DeleteConflicts: Boolean);
var
  HMForm: THMForm;
  HMControl: THMControl;
  sShortCut: String;
  hotkey: THotkey;
  i, count: Integer;
  isFormHotKey: Boolean;
  sCommand: String;
begin
  lbPressedHotKeyCommand.Caption := EmptyStr;
  lbPressedHotKeyCommand.Hint := EmptyStr;

  HMForm := HotMan.Forms.Find(lbxCategories.Items[lbxCategories.ItemIndex]);
  if not Assigned(HMForm) then
    Exit;

  sShortCut := edHotKey.Text;
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
        lbPressedHotKeyCommand.Hint := lbPressedHotKeyCommand.Hint + Format(rsOptHotkeysUsedBy, [hotkey.Command, HMControl.Name]) + LineEnding;
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
        lbPressedHotKeyCommand.Hint := lbPressedHotKeyCommand.Hint + Format(rsOptHotkeysUsedBy, [hotkey.Command, HMForm.Name]) + LineEnding;
    end;
  end;

  // show full message if only one conflict, else show a generic message
  if count = 1 then
    lbPressedHotKeyCommand.Caption := lbPressedHotKeyCommand.Hint
  else if count > 1 then
    lbPressedHotKeyCommand.Caption := rsOptHotkeysShortCutUsed + ' [..]';

  lbPressedHotKeyCommand.Visible := count > 0;
end;

procedure TfrmOptions.edHotKeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ShortCut: TShortCut;
begin
  ShortCut := KeyToShortCutEx(Key,GetKeyShiftStateEx);
  edHotKey.Text := ShortCutToTextEx(ShortCut);
  Key := 0;
  btSetHotKey.Enabled := (edHotKey.Text <> '');
  lbPressedHotKeyCommand.Caption:='';
  btClearHotKey.Enabled := (edHotKey.Text <> '');
  cgHKControls.Enabled := btSetHotKey.Enabled;

  CheckHotKeyConflicts();
end;

procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  FreeThenNil(FOptionsEditorList);
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
//Load specified page or 0
  tvTreeView.Items.Item[Self.Tag].Selected:=true;
  nbNotebook.PageIndex := Self.Tag;

end;

procedure TfrmOptions.pgBehavResize(Sender: TObject);
var
  iWidth: Integer;
begin
  iWidth:= (pgBehav.Width div 2) - 26;
  gbMisc2.Width:= iWidth;
end;

procedure TfrmOptions.rbIconsShowNoneChange(Sender: TObject);
begin
   cbIconsSize.Enabled := not rbIconsShowNone.Checked;
end;

procedure TfrmOptions.rbQuickSearchFilterKeyChange(Sender: TObject);
begin
  rbCtrlAltLetterQF.Enabled := not rbCtrlAltLetterQS.Checked;
  rbAltLetterQF.Enabled     := not rbAltLetterQS.Checked;
  rbLetterQF.Enabled        := not rbLetterQS.Checked;
  rbCtrlAltLetterQS.Enabled := not rbCtrlAltLetterQF.Checked;
  rbAltLetterQS.Enabled     := not rbAltLetterQF.Checked;
  rbLetterQS.Enabled        := not rbLetterQF.Checked;
end;

procedure TfrmOptions.seNumberColumnsViewerChange(Sender: TObject);
begin
  pbViewerBook.Repaint;
end;

procedure TfrmOptions.stgArchiverCommandsPrepareCanvas(Sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if aRow = 0 then
    stgArchiverCommands.Canvas.Brush.Color:= stgArchiverCommands.FixedColor
  else
    stgArchiverCommands.Canvas.Brush.Color:= stgArchiverCommands.Color;
end;

procedure TfrmOptions.stgCommandsResize(Sender: TObject);
begin
  stgCommands.ColWidths[stgCmdHotkeysIndex] := stgCommands.Width
                                             - stgCommands.ColWidths[stgCmdCommandIndex]
                                             - stgCommands.ColWidths[stgCmdCommentIndex];
end;

procedure TfrmOptions.stgCommandsSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
  // < find hotkeys for command
var st:TStringList;
    selcmd: String;
    HMForm: THMForm;
begin

     // clears all controls
     btSetHotKey.Enabled :=false;
     btClearHotKey.Enabled :=false;
     edHotKey.Clear;
     lbPressedHotKeyCommand.Caption:='';
     cgHKControls.Enabled := false;
     stgHotkeys.RowCount := stgHotkeys.FixedRows;
     if aRow<1 then exit;

     HMForm := HotMan.Forms.Find(lbxCategories.Items[lbxCategories.ItemIndex]);
     if not Assigned(HMForm) then
       exit;

     selcmd:=stgCommands.Cells[stgCmdCommandIndex,aRow];// get selected command

     FillHotkeyList(selcmd);
end;

procedure TfrmOptions.stgHotkeysSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var
  controlList: TStringList;
  i, j: Integer;
begin
  if aRow < stgHotkeys.FixedRows then
    exit;

  edHotKey.Text := stgHotkeys.Cells[0, aRow];
  edtParam.Text := stgHotkeys.Cells[1, aRow];
  btSetHotKey.Enabled := true;
  btClearHotKey.Enabled := true;
  lbPressedHotKeyCommand.Caption:='';
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

procedure TfrmOptions.stgToolsSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  aRow := aRow - stgTools.FixedRows;
  if (aRow >= 0) and (aRow < SizeOf(ExtToolFromRow)) then
  begin
    FUpdatingTools := True;
    ShowExternalToolOptions(ExtToolFromRow[aRow]);
    FUpdatingTools := False;
  end;
end;

procedure TfrmOptions.tbArchiverAdditionalShow(Sender: TObject);
var
  I, J: LongInt;
  iWidth: LongInt = 0;
begin
  for I:= 0 to stgArchiverCommands.RowCount - 1 do
  begin
    J:= stgArchiverCommands.Canvas.TextWidth(stgArchiverCommands.Cells[0, I]);
    if J > iWidth then iWidth:= J;
  end;
  stgArchiverCommands.ColWidths[0]:= iWidth + 12;
end;

procedure TfrmOptions.btnFontViewerColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbFontColorViewerBook.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbFontColorViewerBook, optColorDialog.Color);
  end;
end;

procedure TfrmOptions.btnMultiArcAddClick(Sender: TObject);
var
  sName: UTF8String;
  MultiArcItem: TMultiArcItem;
begin
  if InputQuery(Caption, rsOptArchiveTypeName, sName) then
    begin
      MultiArcItem:= TMultiArcItem.Create;
      lbxMultiArc.Items.AddObject(sName, MultiArcItem);
      gMultiArcList.Add(sName, MultiArcItem);
      lbxMultiArc.ItemIndex:= lbxMultiArc.Count - 1;
      pcArchiverCommands.Enabled:= (lbxMultiArc.Count <> 0);
      chkMultiArcEnabled.Enabled:= (lbxMultiArc.Count <> 0);
    end;
end;

procedure TfrmOptions.btnMultiArcApplyClick(Sender: TObject);
begin
  if lbxMultiArc.ItemIndex < 0 then Exit;
  with TMultiArcItem(lbxMultiArc.Items.Objects[lbxMultiArc.ItemIndex]), stgArchiverCommands do
  begin
    FDescription:= edtDescription.Text;
    FArchiver:= fneArchiver.FileName;
    FExtension:= edtArchiveExtension.Text;
    FList:= edtArchiveList.Text;
    FStart:= edtArchiveListStart.Text;
    FEnd:= edtArchiveListEnd.Text;
    FFormat.Assign(memArchiveListFormat.Lines);
    FExtract:= edtArchiveExtract.Text;
    FAdd:= edtArchiveAdd.Text;
    FDelete:= Cells[1, stgArchiveDelete];
    FTest:= Cells[1, stgArchiveTest];
    FExtractWithoutPath:= Cells[1, stgArchiveExtractWithoutPath];
    FAddSelfExtract:= Cells[1, stgArchiveSelfExtract];
    FID:= Cells[1, stgArchiveID];
    FIDPos:= Cells[1, stgArchiveIDPos];
    FIDSeekRange:= Cells[1, stgArchiveIDSeekRange];
    FPasswordQuery:= Cells[1, stgArchivePasswordQuery];
    FFormMode:= StrToIntDef(Cells[1, stgArchiveFormMode], 0);
    FOutput:= chkMultiArcOutput.Checked;
    FDebug:= chkMultiArcDebug.Checked;
  end;
end;

procedure TfrmOptions.btnMultiArcDeleteClick(Sender: TObject);
var
  I: Integer;
begin
  if lbxMultiArc.ItemIndex < 0 then Exit;
  I:= lbxMultiArc.ItemIndex;
  lbxMultiArc.Items.Delete(I);
  gMultiArcList.Delete(I);
  lbxMultiArc.ItemIndex:= lbxMultiArc.Count - 1;
  pcArchiverCommands.Enabled:= (lbxMultiArc.Count <> 0);
  chkMultiArcEnabled.Enabled:= (lbxMultiArc.Count <> 0);
end;

procedure TfrmOptions.btnMultiArcRenameClick(Sender: TObject);
var
  sNewName: UTF8String;
begin
  if lbxMultiArc.ItemIndex < 0 then Exit;
  sNewName:= lbxMultiArc.Items[lbxMultiArc.ItemIndex];
  if InputQuery(Caption, rsOptArchiveTypeName, sNewName) then
    begin
      lbxMultiArc.Items[lbxMultiArc.ItemIndex]:= sNewName;
      gMultiArcList.Names[lbxMultiArc.ItemIndex]:= sNewName;
    end;
end;

procedure TfrmOptions.FillColumnsList;
begin
 lstColumnsSets.Clear;
 If ColSet.Items.Count>0 then
   begin
     lstColumnsSets.Items.AddStrings(ColSet.Items);
   end;
end;

procedure TfrmOptions.FillCommandsPage;
begin
  actions.GetCategoriesList(lbxCategories.Items);
  if lbxCategories.Items.Count>0 then
    lbxCategories.ItemIndex:=0;
  stgcommands.AutoSizeColumns;
end;

procedure TfrmOptions.FillIgnoreList(bWithFullPath: Boolean);
var
  I: Integer;
  SelectedFiles: TFiles;
begin
  SelectedFiles := frmMain.ActiveFrame.CloneSelectedFiles;
  try
    for I:= 0 to SelectedFiles.Count - 1 do
      if bWithFullPath then
        memIgnoreList.Lines.Add(SelectedFiles[I].FullPath)
      else
        memIgnoreList.Lines.Add(SelectedFiles[I].Name);
  finally
    FreeAndNil(SelectedFiles);
  end;
end;

procedure TfrmOptions.FillArchiverList;
var
  I: Integer;
begin
  for I:= 0 to gMultiArcList.Count - 1 do
    lbxMultiArc.Items.AddObject(gMultiArcList.Names[I], gMultiArcList[I]);
  pcArchiverCommands.Enabled:= (lbxMultiArc.Count <> 0);
  chkMultiArcEnabled.Enabled:= (lbxMultiArc.Count <> 0);
  if lbxMultiArc.Count > 0 then
    lbxMultiArc.ItemIndex:= 0;
end;

procedure TfrmOptions.FillCommandList(lstFilter: String);
//< fill stgCommands by commands and comments
var
  slTmp, slAllCommands, slComments, slHotKey: TStringList;
  slFiltered: TStringList = nil;
  lstr:   String;
  i:      Integer;
  HMForm: THMForm;
  sForm:  String;
begin
  slAllCommands := TStringList.Create;
  slComments    := TStringList.Create;
  slHotKey      := TStringList.Create;
  slTmp         := TStringList.Create;
  sForm         := lbxCategories.items.Strings[lbxCategories.ItemIndex];
  HMForm        := HotMan.Forms.Find(sForm);

  Actions.GetCommandsByCategory(sForm, slAllCommands);
  if lstFilter <> '' then // if filter not empty
  begin
    slFiltered := TStringList.Create;
    lstr := UTF8LowerCase(edtFilter.Text);
    for i := 0 to slAllCommands.Count - 1 do // for all command
      // if filtered text find in command or comment then add to filteredlist
      if (UTF8Pos(lstr, UTF8LowerCase(slAllCommands.Strings[i])) <> 0) or
         (UTF8Pos(lstr, UTF8LowerCase(Actions.GetCommandCaption(slAllCommands.Strings[i]))) <> 0) then
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
    slComments.Add(Actions.GetCommandCaption(slFiltered.Strings[i]));

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
  slFiltered.Insert(0, rsOptHotkeysCommands);
  slComments.Insert(0, rsOptHotkeysComments);
  slHotKey.Insert(0, rsOptHotkeysHotkeys);
  //set stringgrid rows count
  stgCommands.RowCount := slFiltered.Count;
  // copy to string grid created lists
  stgCommands.Cols[stgCmdCommandIndex].Assign(slFiltered);
  stgCommands.Cols[stgCmdCommentIndex].Assign(slComments);
  stgCommands.Cols[stgCmdHotkeysIndex].Assign(slHotKey);

  stgCommands.Row := 0; // needs for call select function for refresh hotkeylist

  slHotKey.Free;
  slAllCommands.Free;
  slComments.Free;
  slFiltered.Free;
  slTmp.Free;
end;

procedure TfrmOptions.FillHotkeyList(sCommand: String);
var
  HMForm: THMForm;
  HMControl: THMControl;
  iHotKey, iControl, iGrid: Integer;
  hotkey: THotkey;
  found: Boolean;
begin
  Self.stgHotkeys.RowCount := Self.stgHotkeys.FixedRows;

  if sCommand = EmptyStr then
    exit;

  if Self.lbxCategories.ItemIndex = -1 then
    exit;

  HMForm := HotMan.Forms.Find(lbxCategories.Items[lbxCategories.ItemIndex]);
  if not Assigned(HMForm) then
    exit;

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
end;

procedure TfrmOptions.FillHKControlList();
var
  HMForm: THMForm;
  i: Integer;
begin
  cgHKControls.Items.Clear;
  HMForm := HotMan.Forms.Find(lbxCategories.items.Strings[lbxCategories.ItemIndex]);
  if Assigned(HMForm) then
  begin
    for i := 0 to HMForm.Controls.Count - 1 do
      cgHKControls.Items.AddObject(HMForm.Controls[i].Name, HMForm.Controls[i]);
  end;
  cgHKControls.Visible := cgHKControls.Items.Count <> 0;
end;

procedure TfrmOptions.GetHotKeyList(HMForm: THMForm; Command: String; HotkeysList: TStringList);
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

procedure TfrmOptions.FillSCFilesList;
var
  SR : TSearchRecEx;
  Res : Integer;
begin
  lbSCFilesList.Items.Clear;
  Res := FindFirstEx(gpCfgDir + '*.scf', faAnyFile, SR);
  while Res = 0 do
  begin
    Res:= lbSCFilesList.Items.Add(Sr.Name);
    if Sr.Name = gNameSCFile then lbSCFilesList.Selected[Res]:=true;
    Res := FindNextEx(SR);
  end;
  FindCloseEx(SR);
  lbSCFilesList.OnSelectionChange := @lbSCFilesListSelectionChange;
end;

procedure TfrmOptions.CreateOptionsEditorList;
var
  I: LongInt;
  aOptionsEditor: TOptionsEditor;
begin
  FOptionsEditorList:= TOptionsEditorList.Create;
  for I:= 0 to OptionsEditorClassList.Count - 1 do
  begin
    aOptionsEditor:= OptionsEditorClassList[I].OptionsEditorClass.Create(Self);
    aOptionsEditor.Parent:= nbNotebook.Page[Integer(OptionsEditorClassList[I].OptionsEditorType)];
    FOptionsEditorList.Add(aOptionsEditor);
  end;
end;

procedure TfrmOptions.lbxCategoriesSelectionChange(Sender: TObject; User: boolean);
begin
  if lbxCategories.ItemIndex=-1 then exit;

  Self.FillHKControlList();

  edtFilter.Clear;
  FillCommandList('');
end;

procedure TfrmOptions.nbNotebookPageChanged(Sender: TObject);
begin 
  // temporally this is hack for bug http://www.freepascal.org/mantis/view.php?id=9635
  nbNotebook.Page[nbNotebook.PageIndex].Height := nbNotebook.Height - 8;
  nbNotebook.Page[nbNotebook.PageIndex].Height := nbNotebook.Height - 8;
end;

procedure TfrmOptions.pbViewerBookPaint(Sender: TObject);
var
  i, numb, x, y: integer;
  sStr: String;
begin
  sStr:= 'Text';
  with pbViewerBook.Canvas do
  begin
    Font.Name := edtViewerBookFont.Text;
    Font.Size := edtViewerBookFontSize.Value;
    x:= TextWidth(sStr);
    y:= TextHeight(sStr);
    pbViewerBook.Width := (x + 10) * seNumberColumnsViewer.Value;
    Brush.Color := cbBackgroundColorViewerBook.Selected;
    Font.Color := cbFontColorViewerBook.Selected;
    FillRect(0, 0, pbViewerBook.Width, pbViewerBook.Height);
    for i:= 0 to seNumberColumnsViewer.Value - 1 do
    begin
      for numb:= 0 to 1 do
      TextOut(i * (x + 5) + 5, y * numb + 4, sStr);
    end;
  end;
end;

procedure TfrmOptions.cbAlwaysShowTrayIconChange(Sender: TObject);
begin
  // Force minimizing to tray when tray icon is always shown.
  cbMinimizeToTray.Enabled:= not cbAlwaysShowTrayIcon.Checked;
end;

procedure TfrmOptions.cbIconsSizeChange(Sender: TObject);
var
  bmpTemp: TBitmap;
  iSize: Integer;
begin
  case cbIconsSize.ItemIndex of
    0: iSize:= 16;
    1: iSize:= 22;
    2: iSize:= 32;
  end;
  bmpTemp:= PixmapManager.GetDefaultDriveIcon(iSize, pnlIconExample.Color);
  imgIconExample.Picture.Bitmap.Assign(bmpTemp);
  FreeThenNil(bmpTemp);
end;

procedure TfrmOptions.cbToolsKeepTerminalOpenChange(Sender: TObject);
var
  aRow: Integer;
begin
  if not FUpdatingTools then
  begin
    aRow := stgTools.Row - stgTools.FixedRows;
    if (aRow >= 0) and (aRow < SizeOf(ExtToolFromRow)) then
      tmpExternalTools[ExtToolFromRow[aRow]].KeepTerminalOpen := cbToolsKeepTerminalOpen.Checked;
  end;
end;

procedure TfrmOptions.cbToolsRunInTerminalChange(Sender: TObject);
var
  aRow: Integer;
begin
  cbToolsKeepTerminalOpen.Enabled := cbToolsRunInTerminal.Checked;

  if not FUpdatingTools then
  begin
    aRow := stgTools.Row - stgTools.FixedRows;
    if (aRow >= 0) and (aRow < SizeOf(ExtToolFromRow)) then
      tmpExternalTools[ExtToolFromRow[aRow]].RunInTerminal := cbToolsRunInTerminal.Checked;
  end;
end;

procedure TfrmOptions.cbToolsUseExternalProgramChange(Sender: TObject);
var
  aRow: Integer;
begin
  lblToolsPath.Enabled            := cbToolsUseExternalProgram.Checked;
  fneToolsPath.Enabled            := cbToolsUseExternalProgram.Checked;
  lblToolsParameters.Enabled      := cbToolsUseExternalProgram.Checked;
  edtToolsParameters.Enabled      := cbToolsUseExternalProgram.Checked;
  cbToolsRunInTerminal.Enabled    := cbToolsUseExternalProgram.Checked;
  cbToolsKeepTerminalOpen.Enabled := cbToolsUseExternalProgram.Checked;
  gbViewerBookMode.Enabled        := not (cbToolsUseExternalProgram.Checked);
  lblBackgroundColorViewerBook.Enabled := not (cbToolsUseExternalProgram.Checked);
  lblNumberColumnsViewer.Enabled  := not (cbToolsUseExternalProgram.Checked);
  lblFontColorViewerBook.Enabled  := not (cbToolsUseExternalProgram.Checked);
  if not FUpdatingTools then
  begin
    aRow := stgTools.Row - stgTools.FixedRows;
    if (aRow >= 0) and (aRow < SizeOf(ExtToolFromRow)) then
      tmpExternalTools[ExtToolFromRow[aRow]].Enabled := cbToolsUseExternalProgram.Checked;
  end;
end;

procedure TfrmOptions.cbWatchExcludeDirsChange(Sender: TObject);
begin
  edtWatchExcludeDirs.Enabled := cbWatchExcludeDirs.Checked;
end;

procedure TfrmOptions.cgHKControlsItemClick(Sender: TObject; Index: integer);
begin
  CheckHotKeyConflicts();
end;

procedure TfrmOptions.chkIgnoreEnableChange(Sender: TObject);
begin
  memIgnoreList.Enabled:= chkIgnoreEnable.Checked;
  fneSaveIn.Enabled:= chkIgnoreEnable.Checked;
  btnAddSelWithPath.Enabled:= chkIgnoreEnable.Checked;
  btnAddSel.Enabled:= chkIgnoreEnable.Checked;
end;

procedure TfrmOptions.chkMultiArcEnabledChange(Sender: TObject);
begin
  if lbxMultiArc.ItemIndex < 0 then Exit;
  with TMultiArcItem(lbxMultiArc.Items.Objects[lbxMultiArc.ItemIndex]) do
  FEnabled:= chkMultiArcEnabled.Checked;
end;

procedure TfrmOptions.chkSaveConfigurationChange(Sender: TObject);
begin
  cbDirHistory.Enabled:= chkSaveConfiguration.Checked;
  cbCmdLineHistory.Enabled:= chkSaveConfiguration.Checked;
  cbFileMaskHistory.Enabled:= chkSaveConfiguration.Checked;
  chkSearchReplaceHistory.Enabled:= chkSaveConfiguration.Checked;
end;

procedure TfrmOptions.edtToolsParametersChange(Sender: TObject);
var
  aRow: Integer;
begin
  if not FUpdatingTools then
  begin
    aRow := stgTools.Row - stgTools.FixedRows;
    if (aRow >= 0) and (aRow < SizeOf(ExtToolFromRow)) then
      tmpExternalTools[ExtToolFromRow[aRow]].Parameters := edtToolsParameters.Text;
  end;
end;

procedure TfrmOptions.fneToolsPathAcceptFileName(Sender: TObject;
  var Value: String);
begin
  Value:= SetCmdDirAsEnvVar(Value);
end;

procedure TfrmOptions.fneToolsPathChange(Sender: TObject);
var
  aRow: Integer;
begin
  if not FUpdatingTools then
  begin
    aRow := stgTools.Row - stgTools.FixedRows;
    if (aRow >= 0) and (aRow < SizeOf(ExtToolFromRow)) then
      // Use fneToolsPath.Caption because Filename is one letter behind when typing manually.
      tmpExternalTools[ExtToolFromRow[aRow]].Path := fneToolsPath.Caption;
  end;
end;

procedure TfrmOptions.lbSCFilesListSelectionChange(Sender: TObject;
  User: boolean);
begin
  HotMan.Load(gpCfgDir + lbSCFilesList.GetSelectedText);
  FillCommandsPage;
end;

procedure TfrmOptions.lbxMultiArcSelectionChange(Sender: TObject; User: boolean);
begin
  if lbxMultiArc.ItemIndex < 0 then
  with stgArchiverCommands do
    begin
      edtDescription.Text:= EmptyStr;
      fneArchiver.FileName:= EmptyStr;
      edtArchiveExtension.Text:= EmptyStr;
      edtArchiveList.Text:= EmptyStr;
      edtArchiveListStart.Text:= EmptyStr;
      edtArchiveListEnd.Text:= EmptyStr;
      memArchiveListFormat.Lines.Clear;
      edtArchiveExtract.Text:= EmptyStr;
      edtArchiveAdd.Text:= EmptyStr;
      Cells[1, stgArchiveDelete]:= EmptyStr;
      Cells[1, stgArchiveTest]:= EmptyStr;
      Cells[1, stgArchiveExtractWithoutPath]:= EmptyStr;
      Cells[1, stgArchiveSelfExtract]:= EmptyStr;
      Cells[1, stgArchiveID]:= EmptyStr;
      Cells[1, stgArchiveIDPos]:= EmptyStr;
      Cells[1, stgArchiveIDSeekRange]:= EmptyStr;
      Cells[1, stgArchivePasswordQuery]:= EmptyStr;
      Cells[1, stgArchiveFormMode]:= EmptyStr;
      chkMultiArcOutput.Checked:= False;
      chkMultiArcDebug.Checked:= False;
      chkMultiArcEnabled.Checked:= False;
      pcArchiverCommands.Enabled:= (lbxMultiArc.Count <> 0);
      chkMultiArcEnabled.Enabled:= (lbxMultiArc.Count <> 0);
    end
  else
    with TMultiArcItem(lbxMultiArc.Items.Objects[lbxMultiArc.ItemIndex]), stgArchiverCommands  do
    begin
      edtDescription.Text:= FDescription;
      fneArchiver.FileName:= FArchiver;
      edtArchiveExtension.Text:= FExtension;
      edtArchiveList.Text:= FList;
      edtArchiveListStart.Text:= FStart;
      edtArchiveListEnd.Text:= FEnd;
      memArchiveListFormat.Lines.Assign(FFormat);
      edtArchiveExtract.Text:= FExtract;
      edtArchiveAdd.Text:= FAdd;
      Cells[1, stgArchiveDelete]:= FDelete;
      Cells[1, stgArchiveTest]:= FTest;
      Cells[1, stgArchiveExtractWithoutPath]:= FExtractWithoutPath;
      Cells[1, stgArchiveSelfExtract]:= FAddSelfExtract;
      Cells[1, stgArchiveID]:= FID;
      Cells[1, stgArchiveIDPos]:= FIDPos;
      Cells[1, stgArchiveIDSeekRange]:= FIDSeekRange;
      Cells[1, stgArchivePasswordQuery]:= FPasswordQuery;
      Cells[1, stgArchiveFormMode]:= IntToStr(FFormMode);
      chkMultiArcOutput.Checked:= FOutput;
      chkMultiArcDebug.Checked:= FDebug;
      chkMultiArcEnabled.Checked:= FEnabled;
  end;
end;

procedure TfrmOptions.OnAutoRefreshOptionChanged(Sender: TObject);
begin
  gbAutoRefreshDisable.Enabled := cbWatchFileNameChange.Checked or
                                  cbWatchAttributesChange.Checked;
end;

procedure TfrmOptions.edHotKeyKeyPress(Sender: TObject; var Key: char);
begin
  Key := #0;
  edHotKey.Text := '';
  btSetHotKey.Enabled := False;
end;

procedure TfrmOptions.btnAddSelClick(Sender: TObject);
begin
  FillIgnoreList(False);
end;

procedure TfrmOptions.btnAddSelWithPathClick(Sender: TObject);
begin
  FillIgnoreList(True);
end;

procedure TfrmOptions.btnAutoConfigClick(Sender: TObject);
begin
  gMultiArcList.AutoConfigure;
  lbxMultiArcSelectionChange(lbxMultiArc, True);
end;

procedure TfrmOptions.btnBackViewerColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbBackgroundColorViewerBook.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbBackgroundColorViewerBook, optColorDialog.Color);
  end;
end;

procedure TfrmOptions.btnConfigApplyClick(Sender: TObject);
begin
  gConfig.Load; // force reloading config from file
  LoadGlobs;
  LoadConfig;
  btnConfigApply.Enabled:= False;
end;

procedure TfrmOptions.btnConfigEditClick(Sender: TObject);
begin
  if Assigned(gIni) then
    ShowEditorByGlob(gpCfgDir + 'doublecmd.ini')
  else
    ShowEditorByGlob(gpCfgDir + 'doublecmd.xml');
  btnConfigApply.Enabled:= True;
end;

procedure TfrmOptions.btnDelColumnsSetClick(Sender: TObject);
begin
  if lstColumnsSets.ItemIndex=-1 then exit;
  if lstColumnsSets.Count=1 then exit;
  ColSet.DeleteColumnSet(lstColumnsSets.Items[lstColumnsSets.ItemIndex]);
  FillColumnsList;
end;

procedure TfrmOptions.btnEditColumnsSetClick(Sender: TObject);
var
  frmColumnsSetConf: TfColumnsSetConf;
begin
  //TODO: may be it would be better to show error message?
  if lstColumnsSets.ItemIndex=-1 then exit;
  
  frmColumnsSetConf := TfColumnsSetConf.Create(nil);
  try
    {EDIT Set}
    frmColumnsSetConf.edtNameofColumnsSet.Text:=lstColumnsSets.Items[lstColumnsSets.ItemIndex];
    frmColumnsSetConf.lbNrOfColumnsSet.Caption:=IntToStr(lstColumnsSets.ItemIndex+1);
    frmColumnsSetConf.Tag:=lstColumnsSets.ItemIndex;
    frmColumnsSetConf.SetColumnsClass(ColSet.GetColumnSet(lstColumnsSets.Items[lstColumnsSets.ItemIndex]));
    {EDIT Set}
    frmColumnsSetConf.ShowModal;
    FillColumnsList;
  finally
    FreeAndNil(frmColumnsSetConf);
  end;
end;

procedure TfrmOptions.btClearHotKeyClick(Sender: TObject);
var st:TStringList;
    i:integer;
    lstr: String;
    lslHotKeys: TStringList;
    sShortCut: String;
    sCommand: String;
    HMForm: THMForm;
    HMControl: THMControl;
    hotkey: THotkey;
begin
  if lbxCategories.ItemIndex=-1 then exit;
  sShortCut := stgHotkeys.Cells[0, stgHotkeys.Row];
  sCommand := stgCommands.Cells[stgCmdCommandIndex, stgCommands.Row];
  HMForm := HotMan.Forms.Find(lbxCategories.Items[lbxCategories.ItemIndex]);
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

procedure TfrmOptions.btnCopyColumnsSetClick(Sender: TObject);
var
  s: string;
begin
  if lstColumnsSets.ItemIndex <> -1 then
  begin
    s := lstColumnsSets.Items[lstColumnsSets.ItemIndex];
    ColSet.CopyColumnSet(s, s + '_Copy');
    FillColumnsList;
  end;
end;

procedure TfrmOptions.btnNewColumnsSetClick(Sender: TObject);
var
  frmColumnsSetConf: TfColumnsSetConf;
begin
  frmColumnsSetConf := TfColumnsSetConf.Create(nil);
  try
    // Create new Set
    frmColumnsSetConf.edtNameofColumnsSet.Text:='New Columns'+inttostr(ColSet.count);
    frmColumnsSetConf.lbNrOfColumnsSet.Caption:=IntToStr(lstColumnsSets.Count+1);
    frmColumnsSetConf.Tag:=-1;
    frmColumnsSetConf.SetColumnsClass(nil);
    frmColumnsSetConf.ShowModal;
    FillColumnsList;
  finally
    FreeAndNil(frmColumnsSetConf);
  end;
end;

procedure TfrmOptions.tvTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  //DebugLN('Page index == ' + IntToStr(Node.Index));
  nbNotebook.PageIndex := tvTreeView.Selected.ImageIndex; // temporally image index
  pnlCaption.Caption := tvTreeView.Selected.Text;
end;

procedure TfrmOptions.LoadConfig;
var
  I: LongInt;
begin
  { Layout page }
  cbShowMainMenu.Checked := gMainMenu;
  cbShowMainToolBar.Checked := gButtonBar;
  cbFlatToolBar.Checked := gToolBarFlat;
  cbShowDiskPanel.Checked := gDriveBar1;
  cbTwoDiskPanels.Checked := gDriveBar2;
  cbFlatDiskPanel.Checked := gDriveBarFlat;
  cbShowDriveMenuButton.Checked := gDriveMenuButton;
  cbShowTabs.Checked := gDirectoryTabs;
  cbShowCurDir.Checked := gCurDir;
  cbShowTabHeader.Checked := gTabHeader;
  cbShowStatusBar.Checked := gStatusBar;
  cbShowCmdLine.Checked := gCmdLine or gTermWindow;
  cbShowCmdLine.Enabled:= not gTermWindow;
  cbShowKeysPanel.Checked := gKeyButtons;
  cbFlatInterface.Checked := gInterfaceFlat;
  cbLogWindow.Checked := gLogWindow;
  cbTermWindow.Checked := gTermWindow;
  cbShowDriveFreeSpace.Checked := gDriveFreeSpace;
  cbFreespaceInd.Checked := gDriveInd;
  cbProgInMenuBar.Checked := gProgInMenuBar;
  cbPanelOfOperations.Checked := gPanelOfOp;

  { Behaviours page }
  edtRunInTerm.Text:= gRunInTerm;
  edtRunTerm.Text:=gRunTerm;
  cbOnlyOnce.Checked:= gOnlyOneAppInstance;
  cbLynxLike.Checked:=gLynxLike;
  cbShortFileSizeFormat.Checked:=gShortFileSizeFormat;

  cbSelectionByMouse.Checked:=gMouseSelectionEnabled;
  cbMouseMode.ItemIndex := gMouseSelectionButton;

  chkAutoFillColumns.Checked:= gAutoFillColumns;
  cmbAutoSizeColumn.ItemIndex:= gAutoSizeColumn;

  case gScrollMode of
    smLineByLineCursor:
      rbScrollLineByLineCursor.Checked:= True;
    smLineByLine:
      rbScrollLineByLine.Checked:= True;
    smPageByPage:
      rbScrollPageByPage.Checked:= True;
    else
      rbScrollLineByLine.Checked:= True;
  end;
  seWheelScrollLines.Value:= gWheelScrollLines;

  cbMinimizeToTray.Checked:= gMinimizeToTray;
  cbMinimizeToTray.Enabled:= not gAlwaysShowTrayIcon;
  cbAlwaysShowTrayIcon.Checked:= gAlwaysShowTrayIcon;
  cbDateTimeFormat.Text:= gDateTimeFormat;
  lblDateTimeExample.Caption:= FormatDateTime(gDateTimeFormat, Now);
  cbRenameSelOnlyName.Checked:= gRenameSelOnlyName;
  cbCutTextToColWidth.Checked:= gCutTextToColWidth;
  ledDriveBlackList.Text:= gDriveBlackList;
  cbShowSystemFiles.Checked:= gShowSystemFiles;
  cbListFilesInThread.Checked:= gListFilesInThread;
  cbLoadIconsSeparately.Checked:= gLoadIconsSeparately;

  { Tools page }
  tmpExternalTools := gExternalTools;
  seNumberColumnsViewer.Value := gColCount;

  { Colors }
  SetColorInColorBox(cbBackgroundColorViewerBook,gBookBackgroundColor);
  SetColorInColorBox(cbFontColorViewerBook,gBookFontColor);

  { File operations }
  edtCopyBufferSize.Text:= IntToStr(gCopyBlockSize div 1024);
  cbSkipFileOpError.Checked:= gSkipFileOpError;
  cbDropReadOnlyFlag.Checked := gDropReadOnlyFlag;
  rbUseMmapInSearch.Checked := gUseMmapInSearch;
  cbPartialNameSearch.Checked := gPartialNameSearch;
  seWipePassNumber.Value:= gWipePassNumber;
  cbProcessComments.Checked:= gProcessComments;
  cbShowCopyTabSelectPanel.Checked:=gShowCopyTabSelectPanel;
  cbDeleteToTrash.Checked:= gUseTrash;
  cbShowDialogOnDragDrop.Checked := gShowDialogOnDragDrop;
  cbSaveThubnails.Checked := gSaveThumb;

  { Log file }
  cbLogFile.Checked := gLogFile;
  fneLogFileName.FileName := gLogFileName;
  cbLogCpMvLn.Checked := (log_cp_mv_ln in gLogOptions);
  cbLogDelete.Checked := (log_delete in gLogOptions);
  cbLogDirOp.Checked := (log_dir_op in gLogOptions);
  cbLogArcOp.Checked := (log_arc_op in gLogOptions);
  cbLogVFS.Checked := (log_vfs_op in gLogOptions);
  cbLogSuccess.Checked := (log_success in gLogOptions);
  cbLogErrors.Checked := (log_errors in gLogOptions);
  cbLogInfo.Checked := (log_info in gLogOptions);

  { Folder tabs }
  cbTabsAlwaysVisible.Checked := (tb_always_visible in gDirTabOptions) and gDirectoryTabs;
  cbTabsLimitOption.Checked := tb_text_length_limit in gDirTabOptions;
  cbTabsConfirmCloseAll.Checked:= tb_confirm_close_all in gDirTabOptions;
  cbTabsOpenForeground.Checked:= tb_open_new_in_foreground in gDirTabOptions;
  cbTabsOpenNearCurrent.Checked:= tb_open_new_near_current in gDirTabOptions;
  cbTabsLockedAsterisk.Checked:= tb_show_asterisk_for_locked in gDirTabOptions;
  cbTabsActivateOnClick.Checked:= tb_activate_panel_on_click in gDirTabOptions;
  cbTabsMultiLines.Visible:= (nbcMultiline in pcArchiverCommands.GetCapabilities);
  if cbTabsMultiLines.Visible then
     cbTabsMultiLines.Checked:= tb_multiple_lines in gDirTabOptions;
  cbTabsShowCloseButton.Visible:= (nbcShowCloseButtons in pcArchiverCommands.GetCapabilities);
  if cbTabsShowCloseButton.Visible then
    cbTabsShowCloseButton.Checked:= tb_show_close_button in gDirTabOptions;
  edtTabsLimitLength.Text:= IntToStr(gDirTabLimit);
  case gDirTabPosition of
    tbpos_top:    cmbTabsPosition.ItemIndex := 0;
    tbpos_bottom: cmbTabsPosition.ItemIndex := 1;
    else          cmbTabsPosition.ItemIndex := 0;
  end;

  { Configuration storage }
  if gUseConfigInProgramDirNew then
    rbProgramDir.Checked := True
  else
    rbUserHomeDir.Checked := True;
  chkSaveConfiguration.Checked:= gSaveConfiguration;
  chkSearchReplaceHistory.Checked:= gSaveSearchReplaceHistory;
  cbDirHistory.Checked := gSaveDirHistory;
  cbCmdLineHistory.Checked := gSaveCmdLineHistory;
  cbFileMaskHistory.Checked := gSaveFileMaskHistory;
  { Quick Search page}
  if gQuickSearch then
    begin
      if (gQuickSearchMode = [ssCtrl, ssAlt]) then
        rbCtrlAltLetterQS.Checked := True
      else if (gQuickSearchMode = [ssAlt]) then
        rbAltLetterQS.Checked := True
      else if gQuickSearchMode = [] then
        rbLetterQS.Checked := True;
    end
  else
    rbNoneQS.Checked := True;

  if gQuickFilter then
    begin
      if (gQuickFilterMode = [ssCtrl, ssAlt]) then
        rbCtrlAltLetterQF.Checked := True
      else if (gQuickFilterMode = [ssAlt]) then
        rbAltLetterQF.Checked := True
      else if gQuickFilterMode = [] then
        rbLetterQF.Checked := True;
    end
  else
    rbNoneQF.Checked := True;

  cbExactBeginning.Checked := gQuickSearchMatchBeginning;
  cbExactEnding.Checked := gQuickSearchMatchEnding;

  { Misc page }
  cbGridVertLine.Checked:= gGridVertLine;
  cbGridHorzLine.Checked:= gGridHorzLine;
  cbShowWarningMessages.Checked:= gShowWarningMessages;
  cbSpaceMovesDown.Checked:= gSpaceMovesDown;
  cbDirBrackets.Checked:= gDirBrackets;
  cbSortCaseSensitive.Checked:= gSortCaseSensitive;
  if gSortNatural then cbSortMethod.ItemIndex:= 1;

  { Auto refresh }
  cbWatchFileNameChange.Checked := (watch_file_name_change in gWatchDirs);
  cbWatchAttributesChange.Checked := (watch_attributes_change in gWatchDirs);
  cbWatchOnlyForeground.Checked := (watch_only_foreground in gWatchDirs);
  cbWatchExcludeDirs.Checked := (watch_exclude_dirs in gWatchDirs);
  edtWatchExcludeDirs.Text := gWatchDirsExclude;
  OnAutoRefreshOptionChanged(nil);
  cbWatchExcludeDirsChange(nil);

  { Icons sizes in file panels }
  case gShowIconsNew of
    sim_none: rbIconsShowNone.Checked:= True;
    sim_standart: rbIconsShowStandard.Checked:= True;
    sim_all: rbIconsShowAll.Checked:= True;
    sim_all_and_exe: rbIconsShowAllAndExe.Checked := True;
  end;
  cbIconsShowOverlay.Checked:= gIconOverlays;
  cbIconsSize.Text := IntToStr(gIconsSizeNew) + 'x' + IntToStr(gIconsSizeNew);
  cbIconsSizeChange(nil);

  { Ignore list page }
  chkIgnoreEnable.Checked:= gIgnoreListFileEnabled;
  fneSaveIn.FileName:= gIgnoreListFile;
  memIgnoreList.Lines.Assign(glsIgnoreList);
  chkIgnoreEnableChange(chkIgnoreEnable);

  FillLngListBox;
  FillFontLists;

  FillColumnsList;
  // fill commands
  FillCommandsPage;
  // fill archiver list
  FillArchiverList;

  { Load options to frames }
  for I:= 0 to FOptionsEditorList.Count - 1 do
    FOptionsEditorList[I].Load;
end;

procedure TfrmOptions.SaveConfig;
var
  I: LongInt;
begin
  { Layout page }
  gMainMenu := cbShowMainMenu.Checked;
  gButtonBar := cbShowMainToolBar.Checked;
  gToolBarFlat := cbFlatToolBar.Checked;
  gDriveBar1 := cbShowDiskPanel.Checked;
  gDriveBar2 := cbTwoDiskPanels.Checked;
  gDriveBarFlat := cbFlatDiskPanel.Checked;
  gDriveMenuButton := cbShowDriveMenuButton.Checked;
  gDirectoryTabs := cbShowTabs.Checked;
  gCurDir := cbShowCurDir.Checked;
  gTabHeader := cbShowTabHeader.Checked;
  gStatusBar := cbShowStatusBar.Checked;
  gCmdLine := cbShowCmdLine.Checked;
  gKeyButtons := cbShowKeysPanel.Checked;
  gInterfaceFlat := cbFlatInterface.Checked;
  gLogWindow := cbLogWindow.Checked;
  gTermWindow := cbTermWindow.Checked;
  gDriveFreeSpace := cbShowDriveFreeSpace.Checked;
  gDriveInd := cbFreespaceInd.Checked;
  gProgInMenuBar := cbProgInMenuBar.Checked;
  gPanelOfOp := cbPanelOfOperations.Checked;

  { Behaviour page }
  gRunInTerm:=edtRunInTerm.Text;
  gRunTerm:= edtRunTerm.Text;
  gOnlyOneAppInstance:=cbOnlyOnce.Checked;
  gLynxLike:=cbLynxLike.Checked;
  gShortFileSizeFormat:=cbShortFileSizeFormat.Checked;

  if rbScrollLineByLineCursor.Checked then
    gScrollMode:= smLineByLineCursor
  else if rbScrollLineByLine.Checked then
    gScrollMode:= smLineByLine
  else if rbScrollPageByPage.Checked then
    gScrollMode:= smPageByPage;
  gWheelScrollLines:= seWheelScrollLines.Value;

  gMinimizeToTray:= cbMinimizeToTray.Checked;
  gAlwaysShowTrayIcon:= cbAlwaysShowTrayIcon.Checked;
  gDateTimeFormat := cbDateTimeFormat.Text;
  gRenameSelOnlyName:= cbRenameSelOnlyName.Checked;
  gCutTextToColWidth:= cbCutTextToColWidth.Checked;
  gDriveBlackList:= ledDriveBlackList.Text;
  gShowSystemFiles:= cbShowSystemFiles.Checked;
  gListFilesInThread:= cbListFilesInThread.Checked;
  gLoadIconsSeparately:= cbLoadIconsSeparately.Checked;
  
  gMouseSelectionEnabled := cbSelectionByMouse.Checked;
  gMouseSelectionButton := cbMouseMode.ItemIndex;

  gAutoFillColumns:= chkAutoFillColumns.Checked;
  gAutoSizeColumn:= cmbAutoSizeColumn.ItemIndex;

  { Tools page }
  gExternalTools := tmpExternalTools;
  gColCount := seNumberColumnsViewer.Value;

  { Fonts }
  with gFonts[dcfMain] do
  begin
    Name  := edtMainFont.Text;
    Size  := edtMainFontSize.Value;
    Style := edtMainFont.Font.Style;
  end;

  with gFonts[dcfEditor] do
  begin
    Name  := edtEditorFont.Text;
    Size  := edtEditorFontSize.Value;
    Style := edtEditorFont.Font.Style;
  end;

  with gFonts[dcfViewer] do
  begin
    Name  := edtViewerFont.Text;
    Size  := edtViewerFontSize.Value;
    Style := edtViewerFont.Font.Style;
  end;

  with gFonts[dcfLog] do
  begin
    Name  := edtLogFont.Text;
    Size  := edtLogFontSize.Value;
    Style := edtLogFont.Font.Style;
  end;

  with gFonts[dcfViewerBook] do
  begin
    Name  := edtViewerBookFont.Text;
    Size  := edtViewerBookFontSize.Value;
    Style := edtViewerBookFont.Font.Style;
  end;

  { Colors }
  gBookBackgroundColor := cbBackgroundColorViewerBook.Selected;
  gBookFontColor := cbFontColorViewerBook.Selected;

  { File operations }
  gCopyBlockSize := StrToIntDef(edtCopyBufferSize.Text, gCopyBlockSize) * 1024;
  gSkipFileOpError:= cbSkipFileOpError.Checked;
  gDropReadOnlyFlag := cbDropReadOnlyFlag.Checked;
  gUseMmapInSearch := rbUseMmapInSearch.Checked;
  gPartialNameSearch := cbPartialNameSearch.Checked;
  gWipePassNumber:= seWipePassNumber.Value;
  gProcessComments:= cbProcessComments.Checked;
  gShowCopyTabSelectPanel:=cbShowCopyTabSelectPanel.Checked;
  gUseTrash:= cbDeleteToTrash.Checked;
  gShowDialogOnDragDrop := cbShowDialogOnDragDrop.Checked;
  gSaveThumb := cbSaveThubnails.Checked;

  { Log file }
  gLogFile := cbLogFile.Checked;
  gLogFileName := fneLogFileName.FileName;
  gLogOptions := []; // Reset log options
  if cbLogCpMvLn.Checked then
    Include(gLogOptions, log_cp_mv_ln);
  if cbLogDelete.Checked then
    Include(gLogOptions, log_delete);
  if cbLogDirOp.Checked then
    Include(gLogOptions, log_dir_op);
  if cbLogArcOp.Checked then
    Include(gLogOptions, log_arc_op);
  if cbLogVFS.Checked then
    Include(gLogOptions, log_vfs_op);
  if cbLogSuccess.Checked then
    Include(gLogOptions, log_success);
  if cbLogErrors.Checked then
    Include(gLogOptions, log_errors);
  if cbLogInfo.Checked then
    Include(gLogOptions, log_info);
    
  { Folder tabs }
  gDirTabOptions := [];  // Reset tab options
  if cbTabsAlwaysVisible.Checked then
    gDirTabOptions := gDirTabOptions + [tb_always_visible];
  if cbTabsMultiLines.Checked then
    gDirTabOptions := gDirTabOptions + [tb_multiple_lines];
  if cbTabsLimitOption.Checked then
    gDirTabOptions := gDirTabOptions + [tb_text_length_limit];
  if cbTabsConfirmCloseAll.Checked then
    gDirTabOptions := gDirTabOptions + [tb_confirm_close_all];
  if cbTabsOpenForeground.Checked then
    gDirTabOptions := gDirTabOptions + [tb_open_new_in_foreground];
  if cbTabsOpenNearCurrent.Checked then
    gDirTabOptions := gDirTabOptions + [tb_open_new_near_current];
  if cbTabsLockedAsterisk.Checked then
    gDirTabOptions := gDirTabOptions + [tb_show_asterisk_for_locked];
  if cbTabsActivateOnClick.Checked then
    gDirTabOptions := gDirTabOptions + [tb_activate_panel_on_click];
  if cbTabsShowCloseButton.Checked then
    gDirTabOptions := gDirTabOptions + [tb_show_close_button];

  gDirTabLimit := StrToIntDef(edtTabsLimitLength.Text, 32);

  case cmbTabsPosition.ItemIndex of
    0: gDirTabPosition := tbpos_top;
    1: gDirTabPosition := tbpos_bottom;
  end;

  { Configuration storage }
  gUseConfigInProgramDirNew := rbProgramDir.Checked;
  gSaveConfiguration := chkSaveConfiguration.Checked;
  gSaveSearchReplaceHistory := chkSearchReplaceHistory.Checked;
  gSaveDirHistory := cbDirHistory.Checked;
  gSaveCmdLineHistory := cbCmdLineHistory.Checked;
  gSaveFileMaskHistory := cbFileMaskHistory.Checked;

  { Quick Search page}
  gQuickSearch := not rbNoneQS.Checked;
  
  if rbCtrlAltLetterQS.Checked then
    gQuickSearchMode := [ssCtrl, ssAlt];
  if rbAltLetterQS.Checked then
    gQuickSearchMode := [ssAlt];
  if rbLetterQS.Checked then
    gQuickSearchMode := [];

  gQuickFilter := not rbNoneQF.Checked;

  if rbCtrlAltLetterQF.Checked then
    gQuickFilterMode := [ssCtrl, ssAlt];
  if rbAltLetterQF.Checked then
    gQuickFilterMode := [ssAlt];
  if rbLetterQF.Checked then
    gQuickFilterMode := [];

  gQuickSearchMatchBeginning := cbExactBeginning.Checked;
  gQuickSearchMatchEnding := cbExactEnding.Checked;

  { Misc page }
  gGridVertLine:= cbGridVertLine.Checked;
  gGridHorzLine:= cbGridHorzLine.Checked;
  gShowWarningMessages:= cbShowWarningMessages.Checked;
  gSpaceMovesDown:= cbSpaceMovesDown.Checked;
  gDirBrackets:= cbDirBrackets.Checked;
  gSortCaseSensitive:= cbSortCaseSensitive.Checked;
  gSortNatural:= (cbSortMethod.ItemIndex = 1);

  { Auto refresh }
  gWatchDirs := []; // Reset watch options
  if cbWatchFileNameChange.Checked then
    Include(gWatchDirs, watch_file_name_change);
  if cbWatchAttributesChange.Checked then
    Include(gWatchDirs, watch_attributes_change);
  if cbWatchOnlyForeground.Checked then
    Include(gWatchDirs, watch_only_foreground);
  if cbWatchExcludeDirs.Checked then
    Include(gWatchDirs, watch_exclude_dirs);
  gWatchDirsExclude:= edtWatchExcludeDirs.Text;

  { Icons }
  if rbIconsShowNone.Checked then
    gShowIconsNew:= sim_none
  else if rbIconsShowStandard.Checked then
    gShowIconsNew:= sim_standart
  else if rbIconsShowAll.Checked then
    gShowIconsNew:= sim_all
  else if rbIconsShowAllAndExe.Checked then
    gShowIconsNew:= sim_all_and_exe;
  gIconOverlays:= cbIconsShowOverlay.Checked;

  { Ignore list page }
  gIgnoreListFileEnabled:= chkIgnoreEnable.Checked;
  gIgnoreListFile:= fneSaveIn.FileName;
  glsIgnoreList.Assign(memIgnoreList.Lines);

//-------------------------------------------------
  if (gIconsSizeNew <> StrToInt(Copy(cbIconsSize.Text, 1, 2))) or
     (gShowIconsNew <> gShowIcons) or ((lngList.ItemIndex>-1) and
     (Trim(gPOFileName) <> Trim(lngList.Items.Names[lngList.ItemIndex]))) then
    begin
      gIconsSizeNew:= StrToInt(Copy(cbIconsSize.Text, 1, 2)); // new file panel icons size
      if lngList.ItemIndex > -1 then
        gPOFileName:= Trim(lngList.Items.Names[lngList.ItemIndex]); // new language file
      msgOk(rsMsgRestartForApplyChanges);
    end;
//-------------------------------------------------

  frmMain.UpdateWindowView;
  frmMain.Repaint; // for panels repaint

  {save hot keys file}
  gNameSCFile := lbSCFilesList.GetSelectedText;

  { Save options from frames }
  for I:= 0 to FOptionsEditorList.Count - 1 do
    FOptionsEditorList[I].Save;
end;

procedure TfrmOptions.DeleteHotkeyFromGrid(aHotkey: String);
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

procedure TfrmOptions.UpdateHotkeys(HMForm: THMForm);
var
  i: Integer;
begin
  for i := Self.stgCommands.FixedRows to Self.stgCommands.RowCount - 1 do
    Self.UpdateHotkeysForCommand(HMForm, i);
end;

procedure TfrmOptions.UpdateHotkeysForCommand(HMForm: THMForm; RowNr: Integer);
var
  lslHotKeys:TStringList;
begin
  lslHotKeys:=TStringList.Create;
  GetHotKeyList(HMForm, stgCommands.Cells[stgCmdCommandIndex,RowNr],lslHotKeys);
  stgCommands.Cells[stgCmdHotkeysIndex,RowNr]:=StListToStr(';',lslHotKeys,false);
  lslHotKeys.Free;
end;

procedure TfrmOptions.ShowExternalToolOptions(ExtTool: TExternalTool);
begin
  with tmpExternalTools[ExtTool] do
  begin
    cbToolsUseExternalProgram.Checked := Enabled;
    fneToolsPath.FileName             := Path;
    edtToolsParameters.Text           := Parameters;
    cbToolsRunInTerminal.Checked      := RunInTerminal;
    cbToolsKeepTerminalOpen.Checked   := KeepTerminalOpen;
  end;
  gbViewerBookMode.Visible := (ExtTool = etViewer);
end;

end.
