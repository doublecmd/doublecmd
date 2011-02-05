{
   Double Commander
   -------------------------------------------------------------------------
   Implementing of Options dialog

   Copyright (C) 2006-2009  Koblov Alexander (Alexx2000@mail.ru)

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
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, Spin, ColorBox,
  EditBtn, Grids, uDSXModule, uWCXModule, uWDXModule,
  uWFXmodule, uWLXModule, uGlobs;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    bbtnAddCategory: TBitBtn;
    bbtnApplyCategory: TBitBtn;
    bbtnDeleteCategory: TBitBtn;
    btnAutoConfig: TBitBtn;
    btnMultiArcAdd: TBitBtn;
    btnMultiArcDelete: TBitBtn;
    btnMultiArcRename: TBitBtn;
    btnMultiArcApply: TBitBtn;
    btnAddSel: TButton;
    btnAddSelWithPath: TButton;
    btnCategoryColor: TButton;
    btnConfigApply: TBitBtn;
    btnConfigEdit: TBitBtn;
    btnConfigPlugin: TBitBtn;
    btnSearchTemplate: TBitBtn;
    btnSelLogFnt: TButton;
    btnTweakPlugin: TBitBtn;
    btnRemovePlugin: TBitBtn;
    btnEnablePlugin: TBitBtn;
    btnAddPlugin: TBitBtn;
    btnSelEditFnt: TButton;
    btnSelMainFnt: TButton;
    btnSelViewFnt: TButton;
    btSetHotKey: TButton;
    btnForeColor: TButton;
    btnBackColor: TButton;
    btClearHotKey: TButton;
    btnBackColor2: TButton;
    btnMarkColor: TButton;
    btnCursorColor: TButton;
    btnCursorText: TButton;
    btnNewColumnsSet: TButton;
    btnEditColumnsSet: TButton;
    btnDelColumnsSet: TButton;
    btnCopyColumnsSet: TButton;
    btnSelViewerBookFnt: TButton;
    btnBackViewerColor: TButton;
    btnFontViewerColor: TButton;
    cbBackColor: TColorBox;
    cBackGrndLabel: TLabel;
    cbSortCaseSensitive: TCheckBox;
    cbCategoryColor: TColorBox;
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
    cbbUseInvertedSelection: TCheckBox;
    cbMinimizeToTray: TCheckBox;
    cbBackColor2: TColorBox;
    cbMarkColor: TColorBox;
    cbCursorColor: TColorBox;
    cbCursorText: TColorBox;
    cbTextColor: TColorBox;
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
    cbbUseFrameCursor: TCheckBox;
    cbShowDriveFreeSpace: TCheckBox;
    cbSaveThubnails: TCheckBox;
    chkSearchReplaceHistory: TCheckBox;
    chkSaveConfiguration: TCheckBox;
    chkMultiArcEnabled: TCheckBox;
    chkAutoFillColumns: TCheckBox;
    chkIgnoreEnable: TCheckBox;
    chkMultiArcOutput: TCheckBox;
    chkMultiArcDebug: TCheckBox;
    cmbTabsPosition: TComboBox;
    cmbAutoSizeColumn: TComboBox;
    cbSortMethod: TComboBox;
    cbBackgroundColorViewerBook: TColorBox;
    cbFontColorViewerBook: TColorBox;
    cTextLabel: TLabel;
    dlgFnt: TFontDialog;
    edHotKey: TEdit;
    edtViewerBookFont: TEdit;
    edtArchiveListEnd: TEdit;
    edtArchiveListStart: TEdit;
    edtArchiveDelete: TEdit;
    edtArchiveMultiVolume: TEdit;
    edtArchiveSelfExtract: TEdit;
    edtArchiveAdd: TEdit;
    edtArchiveExtension: TEdit;
    edtArchiveExtract: TEdit;
    edtArchiveList: TEdit;
    edtArchiveTest: TEdit;
    edtDescription: TEdit;
    edtLogFont: TEdit;
    edtToolsParameters: TEdit;
    edtCategoryAttr: TEdit;
    edtCategoryMask: TEdit;
    edtCategoryName: TEdit;
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
    gbFileTypesColors: TGroupBox;
    gbLogFile: TGroupBox;
    gbLogFileOp: TGroupBox;
    gbLogFileStatus: TGroupBox;
    gbMisc1: TGroupBox;
    gbExample: TGroupBox;
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
    gbShowToolTip: TGroupBox;
    gbSorting: TGroupBox;
    gbViewerExample: TGroupBox;
    gbViewerBookMode: TGroupBox;
    grpQuickSearchFilterKeys: TGroupBox;
    lblNumberColumnsViewer: TLabel;
    lblFontColorViewerBook: TLabel;
    lblBackgroundColorViewerBook: TLabel;
    lblViewerBookFont: TLabel;
    lblSortMethod: TLabel;
    lblArchiveListEnd: TLabel;
    lblArchiveListStart: TLabel;
    lblArchiveDelete: TLabel;
    lblArchiveMultiVolume: TLabel;
    lblArchiveSelfExtract: TLabel;
    lblArchiveAdd: TLabel;
    lblArchiveExtension: TLabel;
    lblArchiveExtract: TLabel;
    lblArchiveList: TLabel;
    lblArchiveListFormat: TLabel;
    lblArchiver: TLabel;
    lblArchiveTest: TLabel;
    lblDescription: TLabel;
    lblToolsPath: TLabel;
    lblToolsParameters: TLabel;
    lblInactivePanelBrightness: TLabel;
    lblAutoSizeColumn: TLabel;
    lblQuickSearch: TLabel;
    lblQuickFilter: TLabel;
    lblSaveIn: TLabel;
    lblLogFont: TLabel;
    lbxMultiArc: TListBox;
    memArchiveListFormat: TMemo;
    memIgnoreList: TMemo;
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
    rbToolTipNone: TRadioButton;
    rbToolTipOnlyLarge: TRadioButton;
    rbToolTipAllFiles: TRadioButton;
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
    lbCategories: TListBox;
    lblCategoryAttr: TLabel;
    lblCategoryColor: TLabel;
    lblCategoryMask: TLabel;
    lblCategoryName: TLabel;
    lbPressedHotKeyCommand: TLabel;
    lbFilter: TLabel;
    ledDriveBlackList: TLabeledEdit;
    lblDSXDescription: TLabel;
    lblWLXDescription: TLabel;
    lblWCXDescription: TLabel;
    lbcategory: TLabel;
    lbcommands: TLabel;
    lblParam: TLabel;
    lblWipePassNumber: TLabel;
    lblMouseMode: TLabel;
    lblConfigColumns: TLabel;
    lblWDXDescription: TLabel;
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
    lblWFXDescription: TLabel;
    lblBackground2: TLabel;
    lblMarkColor: TLabel;
    lblCursorColor: TLabel;
    lblCursorText: TLabel;
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
    pbExample: TPaintBox;
    pcPluginsTypes: TPageControl;
    pgFileTypesColors: TPage;
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
    stgPlugins: TStringGrid;
    stgCommands: TStringGrid;
    gbIconsSize: TGroupBox;
    stgHotkeys: TStringGrid;
    stgTools: TStringGrid;
    tbArchiverAdditional: TTabSheet;
    tbArchiverGeneral: TTabSheet;
    tbInactivePanelBrightness: TTrackBar;
    tsWLX: TTabSheet;
    tsDSX: TTabSheet;
    tsWDX: TTabSheet;
    tsWCX: TTabSheet;
    tsWFX: TTabSheet;
    tvTreeView: TTreeView;
    cbWatchFileNameChange: TCheckBox;
    edtWatchExcludeDirs: TEdit;
    cbWatchOnlyForeground: TCheckBox;
    cbWatchAttributesChange: TCheckBox;
    procedure bbtnAddCategoryClick(Sender: TObject);
    procedure bbtnApplyCategoryClick(Sender: TObject);
    procedure bbtnDeleteCategoryClick(Sender: TObject);
    procedure btnAddSelClick(Sender: TObject);
    procedure btnAddSelWithPathClick(Sender: TObject);
    procedure btnAutoConfigClick(Sender: TObject);
    procedure btnBackViewerColorClick(Sender: TObject);
    procedure btnConfigApplyClick(Sender: TObject);
    procedure btnConfigEditClick(Sender: TObject);
    procedure btnConfigPluginClick(Sender: TObject);
    procedure btnDSXAddClick(Sender: TObject);
    procedure btnEnablePluginClick(Sender: TObject);
    procedure btnFontViewerColorClick(Sender: TObject);
    procedure btnMultiArcAddClick(Sender: TObject);
    procedure btnMultiArcApplyClick(Sender: TObject);
    procedure btnMultiArcDeleteClick(Sender: TObject);
    procedure btnMultiArcRenameClick(Sender: TObject);
    procedure btnSearchTemplateClick(Sender: TObject);
    procedure btnSelViewerBookFntClick(Sender: TObject);
    procedure cbAlwaysShowTrayIconChange(Sender: TObject);
    procedure cbbUseFrameCursorChange(Sender: TObject);
    procedure cbFontColorViewerBookChange(Sender: TObject);
    procedure cbIconsSizeChange(Sender: TObject);
    procedure cbToolsKeepTerminalOpenChange(Sender: TObject);
    procedure cbToolsRunInTerminalChange(Sender: TObject);
    procedure cbToolsUseExternalProgramChange(Sender: TObject);
    procedure cbWatchExcludeDirsChange(Sender: TObject);
    procedure chkIgnoreEnableChange(Sender: TObject);
    procedure chkMultiArcEnabledChange(Sender: TObject);
    procedure chkSaveConfigurationChange(Sender: TObject);
    procedure edtToolsParametersChange(Sender: TObject);
    procedure edtViewerBookFontSizeChange(Sender: TObject);
    procedure fneToolsPathAcceptFileName(Sender: TObject; var Value: String);
    procedure fneToolsPathChange(Sender: TObject);
    procedure lbxMultiArcSelectionChange(Sender: TObject; User: boolean);
    procedure OnAutoRefreshOptionChanged(Sender: TObject);
    procedure edHotKeyKeyPress(Sender: TObject; var Key: char);
    procedure btnWDXAddClick(Sender: TObject);
    procedure btnWFXAddClick(Sender: TObject);
    procedure btnWLXAddClick(Sender: TObject);
    procedure btClearHotKeyClick(Sender: TObject);
    procedure btnBackColor2Click(Sender: TObject);
    procedure btnCopyColumnsSetClick(Sender: TObject);
    procedure btnCursorColorClick(Sender: TObject);
    procedure btnCursorTextClick(Sender: TObject);
    procedure btnCategoryColorClick(Sender: TObject);
    procedure btnDelColumnsSetClick(Sender: TObject);
    procedure btnEditColumnsSetClick(Sender: TObject);
    procedure btnMarkColorClick(Sender: TObject);
    procedure btnNewColumnsSetClick(Sender: TObject);
    procedure btnWCXAddClick(Sender: TObject);
    procedure btnForeColorClick(Sender: TObject);
    procedure btnBackColorClick(Sender: TObject);
    procedure btnRemovePluginClick(Sender: TObject);
    procedure btnTweakPluginClick(Sender: TObject);
    procedure cbbUseInvertedSelectionChange(Sender: TObject);
    procedure cbCategoryColorChange(Sender: TObject);
    procedure cbColorBoxChange(Sender: TObject);
    procedure cbDateTimeFormatChange(Sender: TObject);
    procedure cbShowDiskPanelChange(Sender: TObject);
    procedure cbTermWindowChange(Sender: TObject);
    procedure cbTextColorChange(Sender: TObject);
    procedure cbColorBoxDropDown(Sender: TObject);
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
    procedure edHotKeyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbCategoriesClick(Sender: TObject);
    procedure lbCategoriesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lbxCategoriesSelectionChange(Sender: TObject; User: boolean);
    procedure nbNotebookPageChanged(Sender: TObject);
    procedure pbViewerBookPaint(Sender: TObject);
    procedure pbExamplePaint(Sender: TObject);
    procedure pcPluginsTypesChange(Sender: TObject);
    procedure pgBehavResize(Sender: TObject);
    procedure rbIconsShowNoneChange(Sender: TObject);
    procedure rbQuickSearchFilterKeyChange(Sender: TObject);
    procedure seNumberColumnsViewerChange(Sender: TObject);
    procedure stgCommandsResize(Sender: TObject);
    procedure stgCommandsSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure stgHotkeysSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure stgPluginsBeforeSelection(Sender: TObject; aCol, aRow: Integer);
    procedure stgToolsSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure tsDSXShow(Sender: TObject);
    procedure tsWCXShow(Sender: TObject);
    procedure tsWDXShow(Sender: TObject);
    procedure tsWFXShow(Sender: TObject);
    procedure tsWLXShow(Sender: TObject);
    procedure tvTreeViewChange(Sender: TObject; Node: TTreeNode);

  private
    tmpExternalTools: TExternalToolsOptions;
    FUpdatingTools: Boolean;

    procedure DeleteHotkeyFromGrid(aHotkey: String);
    procedure ShowExternalToolOptions(ExtTool: TExternalTool);

  public
    procedure FillLngListBox;
    procedure FillFontLists;
    procedure FillFileColorsList;
    procedure FillColumnsList;
    procedure FillCommandsPage;
    procedure FillIgnoreList(bWithFullPath: Boolean);
    procedure FillArchiverList;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure SetColorInColorBox(const lcbColorBox:TColorBox;const lColor:TColor);
    procedure FillCommandList(lstFilter:string);// fill stringgrid
    // return assigned hotkey for command
    function  getHotKeyListByCommand(command:string; const res:TStringList):integer;
  end;

var
  tmpDSXPlugins: TDSXModuleList;
  tmpWCXPlugins: TWCXModuleList;
  tmpWDXPlugins: TWDXModuleList;
  tmpWFXPlugins: TWFXModuleList;
  tmpWLXPlugins: TWLXModuleList;

implementation

{$R *.lfm}

uses
  uLng, uGlobsPaths, uPixMapManager, fMain, LCLProc, LCLVersion,
  uColorExt, uDCUtils, uOSUtils, fColumnsSetConf, uShowMsg, uShowForm,
  fTweakPlugin, uhotkeymanger, uTypes, StrUtils, uFindEx, uKeyboard,
  fMaskInputDlg, uSearchTemplate, uMultiArc;

const
     stgCmdCommandIndex=0;
     stgCmdCommentIndex=1;
     stgCmdHotkeysIndex=2;

const
  // Tools page: what tool is displayed in each row.
  ExtToolFromRow: array[0..2] of TExternalTool = (etViewer, etEditor, etDiffer);

function StListToStr(separator:string; const lStList:TStringList):string;
//< convert stringlist to string
var
  i: Integer;
begin
 Result:='';
 if lStList.Count>0 then
 begin
  Result:=lStList[0]+separator;
  for i:=1 to lStList.Count-1 do Result:=Result+lStList[i]+separator;
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
  rbProgramDir.Caption:= rbProgramDir.Caption + ' - [' + gpGlobalCfgDir + ']';
  rbUserHomeDir.Caption:= rbUserHomeDir.Caption + ' - [' + GetAppConfigDir + PathDelim + ']';
  // tvTreeView localization
  with tvTreeView.Items do
    begin
      Item[0].Text := rsOptLanguage;
      Item[1].Text := rsOptBehav;
      Item[2].Text := rsOptTools;
      Item[3].Text := rsOptFonts;
      Item[4].Text := rsOptColors;
      Item[5].Text := rsOptFilePanels;
      Item[6].Text := rsOptFileTypes;
      Item[7].Text := rsOptHotKeys;
      Item[8].Text := rsOptPlugins;
      Item[9].Text := rsOptLayout;
      Item[10].Text := rsOptFileOp;
      Item[11].Text := rsOptFolderTabs;
      Item[12].Text := rsOptLog;
      Item[13].Text := rsOptConfig;
      Item[14].Text := rsOptQuickSearch;
      Item[15].Text := rsOptColumns;
      Item[16].Text := rsOptMiscellaneous;
      Item[17].Text := rsOptAutoRefresh;
      Item[18].Text := rsOptIcons;
      Item[19].Text := rsOptIgnoreList;
      Item[20].Text := rsOptArchivers
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

  // Localize plugins.
  stgPlugins.Columns.Items[0].Title.Caption := rsOptPluginsActive;
  stgPlugins.Columns.Items[1].Title.Caption := rsOptPluginsName;
  stgPlugins.Columns.Items[2].Title.Caption := rsOptPluginsRegisteredFor;
  stgPlugins.Columns.Items[3].Title.Caption := rsOptPluginsFileName;

  // create plugins lists
  tmpDSXPlugins:= TDSXModuleList.Create;
  tmpWCXPlugins:= TWCXModuleList.Create;
  tmpWDXPlugins:= TWDXModuleList.Create;
  tmpWFXPlugins:= TWFXModuleList.Create;
  tmpWLXPlugins:= TWLXModuleList.Create;
  // load all configuration
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
end;

procedure TfrmOptions.btSetHotKeyClick(Sender: TObject);
var i: integer;
    Cat:string;
    st:TStringList;
    sOldCommand, sCommand: string;
    lslHotKeys:TStringList;
    sShortCut, sParam: String;
procedure lAddHotKey;
//< local function for add hot key,
begin
    HotMan.AddHotKey(sShortCut,
                     stgCommands.Cells[stgCmdCommandIndex,stgCommands.Row],
                     sParam,
                     frmMain);

    stgHotkeys.RowCount := stgHotkeys.RowCount + 1;
    stgHotkeys.Cells[0, stgHotkeys.RowCount - 1] := sShortCut;
    stgHotkeys.Cells[1, stgHotkeys.RowCount - 1] := sParam;

    edtParam.Text:='';
    edHotKey.Text:='';
    btSetHotKey.Enabled:=false;
    btClearHotKey.Enabled:=false;

    // refresh selected cell with hotkey
    lslHotKeys:=TStringList.Create;
    // set new hotkey string to stringgrid
    getHotKeyListByCommand(stgCommands.Cells[stgCmdCommandIndex,stgCommands.Row],lslHotKeys);
    stgCommands.Cells[stgCmdHotkeysIndex, stgCommands.Row]:=StListToStr(';',lslHotKeys);
    lslHotKeys.Free;

    // Select the new shortcut in the hotkeys table.
    stgHotkeys.Row := stgHotkeys.Cols[0].IndexOf(sShortCut);
end;
begin
// ToDo: Black list HotKey which can't use
//TODO: Realize full version of hotkey's using. Allow to bind hotkeys to any controls.

 if lbxCategories.ItemIndex=-1 then exit;
 if stgCommands.Row<1 then exit;

 sShortCut := edHotKey.Text;
 sParam := edtParam.Text;
 sCommand := stgCommands.Cells[stgCmdCommandIndex, stgCommands.Row];

 cat:=lbxCategories.Items[lbxCategories.ItemIndex];
 if cat='Main' then
 begin
  i:=HotMan.GetHotKeyIndex(sShortCut);
  if i=-1 then
  begin
   lAddHotKey;
  end
  else
   begin
     st:=TStringList.Create;
     HotMan.GetControlsListBy(sShortCut,st);

     if st.IndexOf('frmMain')>-1 then
       begin
          // Shortcut already used.

          HotMan.GetCommandsListBy(sShortCut,st);
          sOldCommand := Copy(st[0], pos('=',st[0]) + 1, Length(st[0]) - pos('=', st[0]));

          // Delete the old shortcut.
          // If it was assigned to a different command then ask user for confirmation.
          if (sOldCommand = sCommand) or
             (MessageDlg(rsOptHotkeysShortCutUsed,
                         Format(rsOptHotkeysShortCutUsedText1,
                                [sShortCut, sOldCommand]) + LineEnding +
                         Format(rsOptHotkeysShortCutUsedText2,
                                [sCommand]),
                         mtConfirmation, mbYesNo, 0) = mrYes) then
          begin
            //**  delete hotkey
            HotMan.DeleteHotKey(sShortCut,frmMain);
            lbPressedHotKeyCommand.Caption:='';// clear message "used by ..."
            // delete hotkey from hotkeylist
            DeleteHotkeyFromGrid(sShortCut);

            // find row in stringgrid where need delete hotkey
            i:=stgCommands.Cols[stgCmdCommandIndex].IndexOf(sOldCommand);
            if i>0 then
            begin
             lslHotKeys:=TStringList.Create;
             getHotKeyListByCommand(sOldCommand, lslHotKeys);
             stgCommands.Cells[stgCmdHotkeysIndex,i]:=StListToStr(';',lslHotKeys);
             lslHotKeys.Free;
            end;
            //** add
            lAddHotKey;
          end;
       end // end if st.IndexOf('frmMain')>-1
       else
         begin // add hotkey
         lAddHotKey;
         end; //end else if st.IndexOf('frmMain')>-1
     st.free;
   end; //end else if i=-1; i:=HotMan.GetHotKeyIndex(sShortCut);
 end; // end if cat='Main'

end;

procedure TfrmOptions.btnForeColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbTextColor.Color;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbTextColor,optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptions.btnBackColorClick(Sender: TObject);
begin
   optColorDialog.Color:= cbBackColor.Color;
   if optColorDialog.Execute then
   begin
    SetColorInColorBox(cbBackColor,optColorDialog.Color);
    pbExample.Repaint;
   end;
end;

procedure TfrmOptions.cbbUseInvertedSelectionChange(Sender: TObject);
begin
  pbExample.Repaint;
end;

procedure TfrmOptions.cbColorBoxChange(Sender: TObject);
begin
  (Sender as TColorBox).Color := (Sender as TColorBox).Selected;
  pbExample.Repaint;
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

procedure TfrmOptions.cbTextColorChange(Sender: TObject);
begin
  cbTextColor.Color := cbTextColor.Selected;
end;

procedure TfrmOptions.cbColorBoxDropDown(Sender: TObject);
begin
  (Sender as TColorBox).Color := clWindow;
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
  DebugLn('Language dir: ' + gpLngDir);
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

procedure TfrmOptions.edHotKeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//<
var
  i: LongInt;
  st: TStringList;
  sShortCut: String;
  vShortCut: TShortCut;
begin
  vShortCut := ShortCutEx(Key,GetKeyShiftStateEx);
  sShortCut := ShortCutToTextEx(vShortCut);
  edHotKey.Text := sShortCut;
  Key := 0;
  btSetHotKey.Enabled := (edHotKey.Text <> '');
  lbPressedHotKeyCommand.Caption:='';

  // find hotkey
  i:=HotMan.GetHotKeyIndex(sShortCut);
  if i<>-1 then
  begin
   st:=TStringList.Create;
   if HotMan.GetControlsListBy(sShortCut,st)>0 then
   begin
     HotMan.GetCommandsListBy(sShortCut,st);
     lbPressedHotKeyCommand.Caption := rsOptHotkeysUsedBy + ' ' + st[0];
     btClearHotKey.Enabled := (edHotKey.Text <> '');
   end;
   st.free;
  end;

end;

procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  if Assigned(tmpDSXPlugins) then
    FreeAndNil(tmpDSXPlugins);
  if Assigned(tmpWCXPlugins) then
    FreeAndNil(tmpWCXPlugins);
  if Assigned(tmpWDXPlugins) then
    FreeAndNil(tmpWDXPlugins);
  if Assigned(tmpWFXPlugins) then
    FreeAndNil(tmpWFXPlugins);
  if Assigned(tmpWLXPlugins) then
    FreeAndNil(tmpWLXPlugins);
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
//Load specified page or 0
  tvTreeView.Items.Item[Self.Tag].Selected:=true;
  nbNotebook.PageIndex := Self.Tag;

end;

procedure TfrmOptions.pbExamplePaint(Sender: TObject);
var
  h, I : integer;
  sText : String;
  iTextTop, iTextLeft : Integer;
  Rect : TRect;
begin
  h := pbExample.Height div 6;
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := pbExample.Width;
  Rect.Bottom := h;

  for I := 1 to 6 do
  with pbExample.Canvas do
  begin
    case I of
    1:
      begin
       if  cbbUseInvertedSelection.Checked then
         begin
            Brush.Color := cbMarkColor.Color;
            Font.Color := cbTextColor.Color;
            sText := rsOptExampleMark;
         end
       else
         begin
            Brush.Color := cbBackColor.Color;
            Font.Color := cbMarkColor.Color;
            sText := rsOptExampleMark;
         end;
      end;
    2:
      begin
       if  cbbUseInvertedSelection.Checked then
         begin
            Brush.Color := cbMarkColor.Color;
            Font.Color := cbTextColor.Color;
            sText := rsOptExampleMark;
         end
       else
         begin
            Brush.Color := cbBackColor2.Color;
            Font.Color := cbMarkColor.Color;
            sText := rsOptExampleMark;
         end;
      end;
    3:
      begin
        Brush.Color := cbBackColor.Color;
        Font.Color := cbTextColor.Color;
        sText := rsOptExampleText;
      end;
    4:
      begin
        Brush.Color := cbBackColor2.Color;
        Font.Color := cbTextColor.Color;
        sText := rsOptExampleText;
      end;
    5:
      begin
        if cbbUseFrameCursor.Checked then
         begin
          Pen.Color := cbCursorColor.Color;
          Brush.Color := cbBackColor.Color;
         end
          else  Brush.Color := cbCursorColor.Color;

        Font.Color := cbCursorText.Color;
        sText := rsOptExampleCursor;
      end;
    6:
      begin
       if  cbbUseInvertedSelection.Checked then
         begin
           if cbbUseFrameCursor.Checked then
              begin
                Pen.Color := cbCursorColor.Color;
                Brush.Color := cbBackColor.Color;
                Font.Color := cbMarkColor.Color;
              end
           else
             begin
               Brush.Color := cbCursorColor.Color;
               Font.Color :=InvertColor(cbCursorText.Color);
             end;
            sText := rsOptExampleMarkCursor;
         end
       else
         begin
           if cbbUseFrameCursor.Checked then
              begin
                Pen.Color := cbCursorColor.Color;
                Brush.Color := cbBackColor.Color;
                Font.Color := cbMarkColor.Color;
              end
           else
             begin
               Brush.Color := cbCursorColor.Color;
               Font.Color := cbMarkColor.Color;
             end;
            sText := rsOptExampleMarkCursor;
         end;
      end;
    end; // case
    
    Font.Style := edtMainFont.Font.Style;
    Font.Size := edtMainFont.Font.Size;
    Font.Name := edtMainFont.Font.Name;

    iTextTop := Rect.Top + (h div 2) - (TextHeight(sText) div 2);
    iTextLeft := Rect.Left + (pbExample.Width div 2) - (TextWidth(sText) div 2);
    FillRect(Rect);
    if (cbbUseFrameCursor.Checked) and ((i=5) or (i=6)) then
      begin
         Pen.Color:=cbCursorColor.Color;
         Line(Rect.Left, Rect.Top, Rect.Right, Rect.Top);
         Line(Rect.Left, Rect.Bottom - 1, Rect.Right, Rect.Bottom - 1);
      end;
    TextOut(iTextLeft, iTextTop, sText);
    Rect.Top := Rect.Bottom;
    Rect.Bottom := h * (I + 1);
  end; // for
end;

procedure TfrmOptions.pcPluginsTypesChange(Sender: TObject);
begin
  if stgPlugins.RowCount > stgPlugins.FixedRows then
    stgPluginsBeforeSelection(stgPlugins, 0, stgPlugins.FixedRows);
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
    i: Integer;
begin

     // clears all controls
     btSetHotKey.Enabled :=false;
     btClearHotKey.Enabled :=false;
     edHotKey.Clear;
     lbPressedHotKeyCommand.Caption:='';
     stgHotkeys.RowCount := stgHotkeys.FixedRows;
     if aRow<1 then exit;

     selcmd:=stgCommands.Cells[stgCmdCommandIndex,aRow];// get selected command
     if selcmd<>'' then
     begin
       st:=TStringList.Create;
       getHotKeyListByCommand(selcmd,st);
       stgHotkeys.RowCount := stgHotkeys.RowCount + st.Count;
       for i := 0 to st.Count - 1 do
       begin
         stgHotkeys.Cells[0, stgHotkeys.FixedRows + i] := st.Strings[i];
         stgHotkeys.Cells[1, stgHotkeys.FixedRows + i] := THotkeyInfoClass(st.Objects[i]).AParams;
       end;
       st.Free;
     end;
end;

procedure TfrmOptions.stgHotkeysSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  edHotKey.Text := stgHotkeys.Cells[0, aRow];
  edtParam.Text := stgHotkeys.Cells[1, aRow];
  btSetHotKey.Enabled := true;
  btClearHotKey.Enabled := true;
  lbPressedHotKeyCommand.Caption:='';
end;

{ Plugins }

procedure TfrmOptions.stgPluginsBeforeSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  if stgPlugins.Cells[0, aRow] = '+' then
    btnEnablePlugin.Caption:= rsOptDisable
  else if stgPlugins.Cells[0, aRow] = '-' then
    btnEnablePlugin.Caption:= rsOptEnable;

  btnEnablePlugin.Enabled:= (stgPlugins.Cells[0, aRow] <> '');
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

procedure TfrmOptions.btnEnablePluginClick(Sender: TObject);
var
  sExt,
  sExts: String;
  iPluginIndex: Integer;
  bEnabled: Boolean;
begin
  if stgPlugins.Row < stgPlugins.FixedRows then Exit;
  if pcPluginsTypes.ActivePage.Name = 'tsWCX' then
    begin
      sExts:= stgPlugins.Cells[2, stgPlugins.Row];
      sExt:= Copy2SpaceDel(sExts);
      repeat
        iPluginIndex:= tmpWCXPlugins.Find(stgPlugins.Cells[3, stgPlugins.Row], sExt);
        if iPluginIndex <> -1 then
        begin
          bEnabled:= not tmpWCXPlugins.Enabled[iPluginIndex];
          tmpWCXPlugins.Enabled[iPluginIndex]:= bEnabled;
        end;
        sExt:= Copy2SpaceDel(sExts);
      until sExt = '';
      stgPlugins.Cells[0, stgPlugins.Row]:= IfThen(bEnabled, string('+'), string('-'));
      btnEnablePlugin.Caption:= IfThen(bEnabled, rsOptDisable, rsOptEnable);
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWFX' then
    begin
      bEnabled:= not tmpWFXPlugins.Enabled[stgPlugins.Row - stgPlugins.FixedRows];
      stgPlugins.Cells[0, stgPlugins.Row]:= IfThen(bEnabled, string('+'), string('-'));
      tmpWFXPlugins.Enabled[stgPlugins.Row - stgPlugins.FixedRows]:= bEnabled;
      btnEnablePlugin.Caption:= IfThen(bEnabled, rsOptDisable, rsOptEnable);
    end;
end;

procedure TfrmOptions.btnFontViewerColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbFontColorViewerBook.Color;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbFontColorViewerBook,optColorDialog.Color);
    pbExample.Repaint;
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
  with TMultiArcItem(lbxMultiArc.Items.Objects[lbxMultiArc.ItemIndex]) do
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
    FDelete:= edtArchiveDelete.Text;
    FTest:= edtArchiveTest.Text;
    FAddMultiVolume:= edtArchiveMultiVolume.Text;
    FAddSelfExtract:= edtArchiveSelfExtract.Text;
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

procedure TfrmOptions.btnRemovePluginClick(Sender: TObject);
var
  sExt,
  sExts: String;
  iPluginIndex: Integer;
begin
  if stgPlugins.Row < stgPlugins.FixedRows then Exit; // no plugins

  if pcPluginsTypes.ActivePage.Name = 'tsDSX' then
    begin
      tmpDSXPlugins.DeleteItem(stgPlugins.Row - stgPlugins.FixedRows);
      stgPlugins.DeleteColRow(False, stgPlugins.Row);
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWCX' then
    begin
      sExts:= stgPlugins.Cells[2, stgPlugins.Row];
      sExt:= Copy2SpaceDel(sExts);
      repeat
        iPluginIndex:= tmpWCXPlugins.Find(stgPlugins.Cells[3, stgPlugins.Row], sExt);
        if iPluginIndex <> -1 then
          tmpWCXPlugins.Delete(iPluginIndex);
        sExt:= Copy2SpaceDel(sExts);
      until sExt = '';
      stgPlugins.DeleteColRow(False, stgPlugins.Row);
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWDX' then
    begin
      tmpWDXPlugins.DeleteItem(stgPlugins.Row - stgPlugins.FixedRows);
      stgPlugins.DeleteColRow(False, stgPlugins.Row);
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWFX' then
    begin
      tmpWFXPlugins.Delete(stgPlugins.Row - stgPlugins.FixedRows);
      stgPlugins.DeleteColRow(False, stgPlugins.Row);
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWLX' then
    begin
      tmpWLXPlugins.DeleteItem(stgPlugins.Row - stgPlugins.FixedRows);
      stgPlugins.DeleteColRow(False, stgPlugins.Row);
    end
end;

procedure TfrmOptions.btnTweakPluginClick(Sender: TObject);
var
  ptPluginType: TPluginType;
  iPluginIndex: Integer;
begin
  iPluginIndex:= stgPlugins.Row - stgPlugins.FixedRows;
  if pcPluginsTypes.ActivePage.Name = 'tsDSX' then
    ptPluginType:= ptDSX
  else if pcPluginsTypes.ActivePage.Name = 'tsWCX' then
    begin
      ptPluginType:= ptWCX;
      // get plugin index
      iPluginIndex:= tmpWCXPlugins.Find(stgPlugins.Cells[3, stgPlugins.Row],
                                        Copy2Space(stgPlugins.Cells[2, stgPlugins.Row]));
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWDX' then
    ptPluginType:= ptWDX
  else if pcPluginsTypes.ActivePage.Name = 'tsWFX' then
    ptPluginType:= ptWFX
  else if pcPluginsTypes.ActivePage.Name = 'tsWLX' then
    ptPluginType:= ptWLX;

  if iPluginIndex < 0 then Exit;
  if ShowTweakPluginDlg(ptPluginType, iPluginIndex) then
    pcPluginsTypes.ActivePage.OnShow(pcPluginsTypes.ActivePage); // update info in plugin list
end;

procedure TfrmOptions.btnConfigPluginClick(Sender: TObject);
var
  WCXmodule: TWCXmodule;
  WFXmodule: TWFXmodule;
  PluginFileName: String;
begin
  if stgPlugins.Row < stgPlugins.FixedRows then Exit; // no plugins

  PluginFileName := GetCmdDirFromEnvVar(stgPlugins.Cells[3, stgPlugins.Row]);

  if pcPluginsTypes.ActivePage.Name = 'tsWCX' then
    begin
      WCXmodule := TWCXmodule.Create;
      DebugLn('TWCXmodule created');
      try
        if WCXmodule.LoadModule(PluginFileName) then
         begin
           DebugLn('WCXModule Loaded');
           WCXmodule.VFSConfigure(stgPlugins.Handle);
           DebugLn('Dialog executed');
           WCXModule.UnloadModule;
           DebugLn('WCX Module Unloaded');
         end
         else
           msgError(rsMsgErrEOpen + ': ' + PluginFileName);
      finally
        WCXmodule.Free;
        DebugLn('WCX Freed');
      end;
    end
  else if pcPluginsTypes.ActivePage.Name = 'tsWFX' then
    begin
      WFXmodule := TWFXmodule.Create;
      DebugLn('TWFXmodule created');
      try
        if WFXmodule.LoadModule(PluginFileName) then
         begin
           DebugLn('WFXModule Loaded');
           WfxModule.VFSInit(0);
           WFXmodule.VFSConfigure(stgPlugins.Handle);
           DebugLn('Dialog executed');
           WFXModule.UnloadModule;
           DebugLn('WFX Module Unloaded');
         end
         else
           msgError(rsMsgErrEOpen + ': ' + PluginFileName);
      finally
        WFXmodule.Free;
        DebugLn('WFX Freed');
      end;
    end;
end;

{ DSX plugins }

procedure TfrmOptions.btnDSXAddClick(Sender: TObject);
var
  I, J: Integer;
  sPluginName : String;
begin
  odOpenDialog.Filter := 'Search plugins (*.dsx)|*.dsx';
  if odOpenDialog.Execute then
    begin
      sPluginName := ExtractFileName(odOpenDialog.FileName);
      Delete(sPluginName,length(sPluginName)-4,4);
      I:= tmpDSXPlugins.Add(sPluginName,odOpenDialog.FileName,'');

      stgPlugins.RowCount:= stgPlugins.RowCount + 1;
      J:= stgPlugins.RowCount - stgPlugins.FixedRows;
      stgPlugins.Cells[1, J]:= tmpDSXPlugins.GetDsxModule(I).Name;
      stgPlugins.Cells[2, J]:= tmpDSXPlugins.GetDsxModule(I).Descr;
      stgPlugins.Cells[3, J]:= SetCmdDirAsEnvVar(tmpDSXPlugins.GetDsxModule(I).FileName);
    end;
end;

procedure TfrmOptions.tsDSXShow(Sender: TObject);
var i:integer;
begin
  btnAddPlugin.OnClick:= @btnDSXAddClick;
  stgPlugins.RowCount:= tmpDSXPlugins.Count + stgPlugins.FixedRows;
  for i:=0 to tmpDSXPlugins.Count-1 do
    begin
    stgPlugins.Cells[1, I + stgPlugins.FixedRows]:= tmpDSXPlugins.GetDsxModule(i).Name;
    stgPlugins.Cells[2, I + stgPlugins.FixedRows]:= tmpDSXPlugins.GetDsxModule(i).Descr;
    stgPlugins.Cells[3, I + stgPlugins.FixedRows]:= SetCmdDirAsEnvVar(tmpDSXPlugins.GetDsxModule(i).FileName);
    end;
end;

{ WCX plugins }

procedure TfrmOptions.btnWCXAddClick(Sender: TObject);
var
  J, iPluginIndex, iFlags: Integer;
  sExt : String;
  sExts : String;
  sExtsTemp : String;
  sPluginName : String;
  sAlreadyAssignedExts : String;
  WCXmodule : TWCXmodule;
begin
  odOpenDialog.Filter := 'Archive plugins (*.wcx)|*.wcx';
  if odOpenDialog.Execute then
    begin
      WCXmodule := TWCXmodule.Create;
      try
        if WCXmodule.LoadModule(odOpenDialog.FileName) then
          begin
            iFlags := WCXmodule.GetPluginCapabilities;
            WCXModule.UnloadModule;
          end
        else
          iFlags := 0;

        sPluginName := SetCmdDirAsEnvVar(odOpenDialog.FileName);
        if InputQuery(rsOptEnterExt, Format(rsOptAssocPluginWith, [odOpenDialog.FileName]), sExts) then
          begin
            sExtsTemp := sExts;
            sExts := '';
            sAlreadyAssignedExts := '';
            sExt:= Copy2SpaceDel(sExtsTemp);
            repeat
              iPluginIndex:= tmpWCXPlugins.Find(sPluginName, sExt);
              if iPluginIndex <> -1 then
                begin
                  AddStrWithSep(sAlreadyAssignedExts, sExt);
                end
              else
                begin
                  tmpWCXPlugins.AddObject(sExt + '=' + IntToStr(iFlags) + ',' + sPluginName, TObject(True));
                  AddStrWithSep(sExts, sExt);
                end;
              sExt:= Copy2SpaceDel(sExtsTemp);
            until sExt = '';

            if sAlreadyAssignedExts <> '' then
              MessageDlg(Format(rsOptPluginAlreadyAssigned, [odOpenDialog.FileName]) +
                         LineEnding + sAlreadyAssignedExts, mtWarning, [mbOK], 0);

            if sExts <> '' then
              begin
                stgPlugins.RowCount:= stgPlugins.RowCount + 1; // Add new row
                J:= stgPlugins.RowCount-1;
                stgPlugins.Cells[0, J]:= '+'; // Enabled
                stgPlugins.Cells[1, J]:= ExtractOnlyFileName(odOpenDialog.FileName);
                stgPlugins.Cells[2, J]:= sExts;
                stgPlugins.Cells[3, J]:= sPluginName;
              end;
          end;
      finally
        WCXmodule.Free;
      end;
    end;
end;

procedure TfrmOptions.tsWCXShow(Sender: TObject);
var
  I,
  iIndex: Integer;
  sFileName,
  sExt: String;
  iRow: Integer;
begin
  btnAddPlugin.OnClick:= @btnWCXAddClick;
  stgPlugins.RowCount:= stgPlugins.FixedRows;

  // Clear column with extensions
  stgPlugins.Clean(2, stgPlugins.FixedRows, 2, stgPlugins.RowCount, [gzNormal]);

  for I := 0 to tmpWCXPlugins.Count - 1 do
  begin
    // get associated extension
    sExt := tmpWCXPlugins.Ext[I];

    //get file name
    sFileName:= tmpWCXPlugins.FileName[I];

    iIndex:= stgPlugins.Cols[3].IndexOf(sFileName);
    if iIndex < 0 then
      begin
        stgPlugins.RowCount:= stgPlugins.RowCount + 1;
        iRow := stgPlugins.RowCount - 1;
        stgPlugins.Cells[1, iRow]:= ExtractOnlyFileName(sFileName);
        stgPlugins.Cells[2, iRow]:= sExt + #32;

        if tmpWCXPlugins.Enabled[I] then // enabled
          begin
            stgPlugins.Cells[3, iRow]:= sFileName;
            stgPlugins.Cells[0, iRow]:= '+';
          end
        else // disabled
          begin
            stgPlugins.Cells[3, iRow]:= sFileName;
            stgPlugins.Cells[0, iRow]:= '-';
          end;
      end
    else
      begin
        stgPlugins.Cells[2, iIndex]:= stgPlugins.Cells[2, iIndex] + sExt + #32;
      end;
  end;
  if stgPlugins.RowCount > stgPlugins.FixedRows then
    stgPluginsBeforeSelection(stgPlugins, 0, stgPlugins.FixedRows);
end;

{ WDX plugins }

procedure TfrmOptions.btnWDXAddClick(Sender: TObject);
var
  I, J: Integer;
  sPluginName : String;
begin
  odOpenDialog.Filter := 'Content plugins (*.wdx; *.lua)|*.wdx;*.lua';
  if odOpenDialog.Execute then
    begin
      sPluginName := ExtractOnlyFileName(odOpenDialog.FileName);
      I:= tmpWDXPlugins.Add(sPluginName, odOpenDialog.FileName, '');

      tmpWDXPlugins.LoadModule(sPluginName);
      tmpWDXPlugins.GetWdxModule(sPluginName).DetectStr:=tmpWDXPlugins.GetWdxModule(sPluginName).CallContentGetDetectString;

      stgPlugins.RowCount:= stgPlugins.RowCount + 1;
      J:= stgPlugins.RowCount-1;
      stgPlugins.Cells[1, J]:= tmpWDXPlugins.GetWdxModule(I).Name;
      stgPlugins.Cells[2, J]:= tmpWDXPlugins.GetWdxModule(I).DetectStr;
      stgPlugins.Cells[3, J]:= SetCmdDirAsEnvVar(tmpWDXPlugins.GetWdxModule(I).FileName);
    end;
end;

procedure TfrmOptions.tsWDXShow(Sender: TObject);
var i:integer;
begin
  btnAddPlugin.OnClick:= @btnWDXAddClick;
  stgPlugins.RowCount:= tmpWDXPlugins.Count + stgPlugins.FixedRows;
  for i:=0 to tmpWDXPlugins.Count-1 do
    begin
    stgPlugins.Cells[1, I + stgPlugins.FixedRows]:= tmpWDXPlugins.GetWdxModule(i).Name;
    stgPlugins.Cells[2, I + stgPlugins.FixedRows]:= tmpWDXPlugins.GetWdxModule(i).DetectStr;
    stgPlugins.Cells[3, I + stgPlugins.FixedRows]:= SetCmdDirAsEnvVar(tmpWDXPlugins.GetWdxModule(i).FileName);
    end;
end;

{ WFX plugins }

procedure TfrmOptions.btnWFXAddClick(Sender: TObject);
var
  I, J: Integer;
  WfxModule : TWFXmodule;
  sPluginName,
  sRootName: UTF8String;
begin
  odOpenDialog.Filter := 'File system plugins (*.wfx)|*.wfx';
  if odOpenDialog.Execute then
  begin
  DebugLn('Dialog executed');
    WfxModule := TWfxModule.Create;
    DebugLn('TWFXmodule created');
    if WfxModule.LoadModule(odOpenDialog.FileName) then
     begin
       DebugLn('WFXModule Loaded');
       sRootName:= WfxModule.VFSRootName;
       if sRootName <> EmptyStr then
        sPluginName := sRootName + '=' + SetCmdDirAsEnvVar(odOpenDialog.FileName)
       else
         begin
           DebugLn('WFX alternate name');
           sRootName:= ExtractFileName(odOpenDialog.FileName);
           sRootName:= Copy(sRootName, 1, Pos('.', sRootName) - 1);
           sPluginName := sRootName + '=' + SetCmdDirAsEnvVar(odOpenDialog.FileName)
         end;
     end
    else
    begin
      DebugLn('Module not loaded');
      sPluginName := ExtractFileName(odOpenDialog.FileName) +'=' + SetCmdDirAsEnvVar(odOpenDialog.FileName);
    end;

  DebugLn('WFX sPluginName='+sPluginName);
  I:= tmpWFXPlugins.AddObject(sPluginName, TObject(True));
  stgPlugins.RowCount:= tmpWFXPlugins.Count + 1;
  J:= stgPlugins.RowCount-1;
  stgPlugins.Cells[0, J]:= '+';
  stgPlugins.Cells[1, J]:= tmpWFXPlugins.Name[I];
  stgPlugins.Cells[2, J]:= EmptyStr;
  stgPlugins.Cells[3, J]:= tmpWFXPlugins.FileName[I];
  DebugLn('WFX Item Added');
  WFXModule.UnloadModule;
  DebugLn('WFX Module Unloaded');
  WFXmodule.Free;
  DebugLn('WFX Freed');
  end;
end;

procedure TfrmOptions.tsWFXShow(Sender: TObject);
var
  I, iRow: Integer;
begin
  btnAddPlugin.OnClick:= @btnWFXAddClick;
  stgPlugins.RowCount:= tmpWFXPlugins.Count + stgPlugins.FixedRows;
  for I:= 0 to tmpWFXPlugins.Count - 1 do
  begin
    iRow := I + stgPlugins.FixedRows;
    if tmpWFXPlugins.Enabled[I] then
      begin
        stgPlugins.Cells[1, iRow]:= tmpWFXPlugins.Name[I];
        stgPlugins.Cells[3, iRow]:= tmpWFXPlugins.FileName[I];
        stgPlugins.Cells[0, iRow]:= '+';
      end
    else
      begin
        stgPlugins.Cells[1, iRow]:= tmpWFXPlugins.Name[I];
        stgPlugins.Cells[3, iRow]:= tmpWFXPlugins.FileName[I];
        stgPlugins.Cells[0, iRow]:= '-';
      end;
    stgPlugins.Cells[2, iRow]:= '';
  end;
end;

{ WLX Plugins }

procedure TfrmOptions.btnWLXAddClick(Sender: TObject);
var
  I, J: Integer;
  sPluginName : String;
begin
  odOpenDialog.Filter := 'Viewer plugins (*.wlx)|*.wlx';
  if odOpenDialog.Execute then
    begin
      sPluginName := ExtractOnlyFileName(odOpenDialog.FileName);
      I:= tmpWLXPlugins.Add(sPluginName,odOpenDialog.FileName,'');

      tmpWLXPlugins.LoadModule(sPluginName);
      tmpWLXPlugins.GetWlxModule(sPluginName).DetectStr:=tmpWLXPlugins.GetWlxModule(sPluginName).CallListGetDetectString;

      stgPlugins.RowCount:= stgPlugins.RowCount + 1;
      J:= stgPlugins.RowCount-1;
      stgPlugins.Cells[1, J]:= tmpWLXPlugins.GetWlxModule(I).Name;
      stgPlugins.Cells[2, J]:= tmpWLXPlugins.GetWlxModule(I).DetectStr;
      stgPlugins.Cells[3, J]:= SetCmdDirAsEnvVar(tmpWLXPlugins.GetWlxModule(I).FileName);
    end;
end;

procedure TfrmOptions.tsWLXShow(Sender: TObject);
var
  i: Integer;
begin
  btnAddPlugin.OnClick:= @btnWLXAddClick;
  stgPlugins.RowCount:= tmpWLXPlugins.Count + stgPlugins.FixedRows;
  for i:=0 to tmpWLXPlugins.Count-1 do
    begin
    stgPlugins.Cells[1, I + stgPlugins.FixedRows]:= tmpWLXPlugins.GetWlxModule(i).Name;
    stgPlugins.Cells[2, I + stgPlugins.FixedRows]:= tmpWLXPlugins.GetWlxModule(i).DetectStr;
    stgPlugins.Cells[3, I + stgPlugins.FixedRows]:= SetCmdDirAsEnvVar(tmpWLXPlugins.GetWlxModule(i).FileName);
    end;
end;

{ File lbtypes category color }

procedure TfrmOptions.FillFileColorsList;
var
  sCategoryName : String;
  I : Integer;
begin

  for I := 0 to gColorExt.MaskItemList.Count - 1 do
    begin
      sCategoryName := TMaskItem(gColorExt.MaskItemList.Items[I]).sName;
      lbCategories.Items.AddObject(sCategoryName,TMaskItem(gColorExt.MaskItemList.Items[I]));
    end; // for
    
    
    if lbCategories.Count > 0 then
      lbCategories.ItemIndex := 0
    else
      begin
        edtCategoryName.Enabled := False;
        edtCategoryMask.Enabled := False;
        edtCategoryAttr.Enabled := False;
        cbCategoryColor.Enabled := False;
        btnCategoryColor.Enabled := False;
        bbtnDeleteCategory.Enabled := False;
        bbtnApplyCategory.Enabled := False;
      end;
    lbCategoriesClick(lbCategories);
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
  begin
    lbxCategories.ItemIndex:=0;
    lbxCategoriesSelectionChange(nil,false);
  end;
  stgcommands.AutoSizeColumns;
end;

procedure TfrmOptions.FillIgnoreList(bWithFullPath: Boolean);
var
  I: Integer;
begin
  with frmMain.ActiveFrame.SelectedFiles do
  begin
    for I:= 0 to Count - 1 do
      if bWithFullPath then
        memIgnoreList.Lines.Add(Items[I].FullPath)
      else
        memIgnoreList.Lines.Add(Items[I].Name);
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

procedure TfrmOptions.FillCommandList(lstFilter:string);
//< fill stgCommands by commands and comments
var
   slTmp,slFiltered,slAllCommands,slComments,slHotKey:TStringList;
   lstr: String;
   i: Integer;

begin
    slAllCommands:=TStringList.Create();
    slFiltered:=TStringList.Create();
    slHotKey:=TStringList.Create();
    slTmp:=TStringList.Create();

    Actions.GetCommandsByCategory(lbxCategories.items.Strings[lbxCategories.ItemIndex],slAllCommands);
    if lstFilter<>'' then // if filter not empty
    begin
      lstr:=edtFilter.Text;
      for i:=0 to slAllCommands.Count-1 do // for all command
      // if filtered text find in command or comment then add to filteredlist
         if (UTF8Pos(UTF8LowerCase(lstr),UTF8LowerCase(slAllCommands.Strings[i]))<>0)
         or (UTF8Pos(UTF8LowerCase(lstr),UTF8LowerCase(Actions.GetCommandCaption(slAllCommands.Strings[i])))<>0)
             then  slFiltered.Add(slAllCommands[i]);

    end
    else // filter empty -> copy to filtered list all command
        slFiltered.Assign(slAllCommands);

    // sort filtered items
    slFiltered.Sort;
    slAllCommands.Clear;
    slComments:=slAllCommands; // rename
    for i:=0 to slFiltered.Count -1 do
    begin // for all filtered items do
     // get comment for command and add to slComments list
     slComments.Add(Actions.GetCommandCaption(slFiltered.Strings[i]));

     slTmp.Clear;
     // getting list of assigned hot key
     getHotKeyListByCommand(slFiltered.Strings[i],slTmp);
     slHotKey.add(StListToStr(';',slTmp)); //add to hotkey list created string
    end;
    // add to list NAMES of columns
    slFiltered.Insert(0, rsOptHotkeysCommands);
    slComments.Insert(0, rsOptHotkeysComments);
    slHotKey.Insert(0, rsOptHotkeysHotkeys);
    //set stringgrid rows count
    stgCommands.RowCount:=slFiltered.Count;
    // copy to string grid created lists
    stgCommands.Cols[stgCmdCommandIndex].Assign(slFiltered);
    stgCommands.Cols[stgCmdCommentIndex].Assign(slAllCommands);
    stgCommands.Cols[2].Assign(slHotKey);

    stgCommands.Row:=0; // needs for call select function for refresh hotkeylist

    slHotKey.Free;
    slAllCommands.free;
    slFiltered.free;
    slTmp.Free;
end;

function TfrmOptions.getHotKeyListByCommand(command: string; const res:TStringList):INTEGER;
var
  i,j: Integer;
  lstl:TStringList;
begin
  lstl:=TStringList.Create;
  try
    for i:=0 to HotMan.HotkeyList.Count - 1 do
    begin
      HotMan.GetControlsListBy(HotMan.HotkeyList[i],lstl);
      for j:=0 to lstl.Count-1 do
      begin
        if Assigned(lstl.Objects[j]) then
          if command = THotkeyInfoClass(lstl.Objects[j]).ACommand then
            Res.AddObject(HotMan.HotkeyList[i], lstl.Objects[j]);
      end; // for j
    end; // for i
  finally
    FreeAndNil(lstl);
  end;
  Result:=res.Count;
end;

procedure TfrmOptions.lbxCategoriesSelectionChange(Sender: TObject; User: boolean);
begin
  if lbxCategories.ItemIndex=-1 then exit;
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
  i, numb: integer;
  sStr: String;
begin
  sStr:= 'Text';
  pbViewerBook.Width := (pbViewerBook.Canvas.TextWidth(sStr)+10)*seNumberColumnsViewer.Value;
  pbViewerBook.Canvas.Font.Name := edtViewerBookFont.Text;
  with pbViewerBook.Canvas do
  begin
    Brush.Color := cbBackgroundColorViewerBook.Color;
    Font.Color := cbFontColorViewerBook.Color;
    Font.Height:= pbViewerBook.Height div 3 - 2;
    FillRect(0,0,pbViewerBook.Width,pbViewerBook.Height);
    for i:=0 to seNumberColumnsViewer.Value-1 do
    begin
      for numb:=0 to 2 do
      TextOut(i*(pbViewerBook.Canvas.TextWidth(sStr)+5)+3,(Font.Height+2)*numb+2,sStr);
    end;
  end;
end;

{ File type color }

procedure TfrmOptions.cbCategoryColorChange(Sender: TObject);
begin
  (Sender as TColorBox).Color := (Sender as TColorBox).Selected;
end;

procedure TfrmOptions.lbCategoriesClick(Sender: TObject);
var
  MaskItem : TMaskItem;
  bEnabled: Boolean;
begin

  if (lbCategories.Count > 0) and (Assigned(lbCategories.Items.Objects[lbCategories.ItemIndex])) then
    begin
      edtCategoryName.Text := lbCategories.Items[lbCategories.ItemIndex];
      MaskItem := TMaskItem(lbCategories.Items.Objects[lbCategories.ItemIndex]);

      edtCategoryMask.Text := MaskItem.sExt;
      SetColorInColorBox(cbCategoryColor,MaskItem.cColor);
      bEnabled:= Pos('>', MaskItem.sExt) <> 1;
      edtCategoryMask.Enabled:= bEnabled;
      edtCategoryAttr.Enabled:= bEnabled;
      edtCategoryAttr.Text := MaskItem.sModeStr;
    end
  else
    begin
      if lbCategories.Count = 0 then
        edtCategoryName.Text := ''
      else
        edtCategoryName.Text := lbCategories.Items[lbCategories.ItemIndex];
      edtCategoryMask.Text := '*';
      edtCategoryAttr.Text := '';
      cbCategoryColor.ItemIndex := -1;
      cbCategoryColor.Color := clWindow;
      cbCategoryColor.Selected := cbCategoryColor.Color;
    end;
end;

procedure TfrmOptions.lbCategoriesDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  With (Control as TListBox) do
   begin
     Canvas.FillRect(ARect);
     if (not Selected[Index]) and Assigned(Items.Objects[Index]) then
       Canvas.Font.Color:=TMaskItem(Items.Objects[Index]).cColor
     else
       Canvas.Font.Color:=gCursorText;

     Canvas.TextOut(ARect.Left+2,ARect.Top+1,Items[Index]);
   end;
end;

procedure TfrmOptions.btnSearchTemplateClick(Sender: TObject);
var
  sMask: UTF8String;
  bTemplate: Boolean;
begin
  if ShowMaskInputDlg(rsMarkPlus, rsMaskInput, glsMaskHistory, sMask) then
    begin
      bTemplate:= IsMaskSearchTemplate(sMask);
      edtCategoryMask.Text:= sMask;
      if bTemplate then edtCategoryAttr.Text:= EmptyStr;
      edtCategoryMask.Enabled:= not bTemplate;
      edtCategoryAttr.Enabled:= not bTemplate;
    end;
end;

procedure TfrmOptions.cbAlwaysShowTrayIconChange(Sender: TObject);
begin
  // Force minimizing to tray when tray icon is always shown.
  cbMinimizeToTray.Enabled:= not cbAlwaysShowTrayIcon.Checked;
end;

procedure TfrmOptions.cbbUseFrameCursorChange(Sender: TObject);
begin
  pbExample.Repaint;
end;

procedure TfrmOptions.cbFontColorViewerBookChange(Sender: TObject);
begin

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

procedure TfrmOptions.lbxMultiArcSelectionChange(Sender: TObject; User: boolean);
begin
  if lbxMultiArc.ItemIndex < 0 then
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
      edtArchiveDelete.Text:= EmptyStr;
      edtArchiveTest.Text:= EmptyStr;
      edtArchiveMultiVolume.Text:= EmptyStr;
      edtArchiveSelfExtract.Text:= EmptyStr;
      chkMultiArcOutput.Checked:= False;
      chkMultiArcDebug.Checked:= False;
      chkMultiArcEnabled.Checked:= False;
      pcArchiverCommands.Enabled:= (lbxMultiArc.Count <> 0);
      chkMultiArcEnabled.Enabled:= (lbxMultiArc.Count <> 0);
    end
  else
    with TMultiArcItem(lbxMultiArc.Items.Objects[lbxMultiArc.ItemIndex]) do
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
      edtArchiveDelete.Text:= FDelete;
      edtArchiveTest.Text:= FTest;
      edtArchiveMultiVolume.Text:= FAddMultiVolume;
      edtArchiveSelfExtract.Text:= FAddSelfExtract;
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

procedure TfrmOptions.bbtnAddCategoryClick(Sender: TObject);
var
  iIndex : Integer;
begin
  if lbCategories.Count = 0 then
    begin
      edtCategoryName.Enabled := True;
      edtCategoryMask.Enabled := True;
      edtCategoryAttr.Enabled := True;
      cbCategoryColor.Enabled := True;
      btnCategoryColor.Enabled := True;
      bbtnDeleteCategory.Enabled := True;
      bbtnApplyCategory.Enabled := True;
    end;
  iIndex := lbCategories.Items.AddObject('', nil);
  lbCategories.ItemIndex := iIndex;
  edtCategoryName.Text := '';
  edtCategoryMask.Text := '*';
  edtCategoryAttr.Text := '';
  cbCategoryColor.ItemIndex := -1;
  cbCategoryColor.Color := clWindow;
end;

procedure TfrmOptions.bbtnApplyCategoryClick(Sender: TObject);
var
  MaskItem : TMaskItem;
  I, iCount : Integer;
begin
  if bbtnDeleteCategory.Tag = 0 then // if we add or change category
    begin
      lbCategories.Items[lbCategories.ItemIndex] := edtCategoryName.Text;
      MaskItem := TMaskItem.Create;
      MaskItem.sName := edtCategoryName.Text;
      MaskItem.cColor := cbCategoryColor.Color;
      if edtCategoryMask.Text = '' then
        edtCategoryMask.Text := '*'; // because we load colors from ini by mask
      MaskItem.sExt := edtCategoryMask.Text;
      MaskItem.sModeStr := edtCategoryAttr.Text;
      lbCategories.Items.Objects[lbCategories.ItemIndex] := MaskItem;
      if lbCategories.ItemIndex >= gColorExt.MaskItemList.Count then
        gColorExt.MaskItemList.Add(MaskItem)
      else
        begin
          TMaskItem(gColorExt.MaskItemList.Items[lbCategories.ItemIndex]).Free;
          gColorExt.MaskItemList.Items[lbCategories.ItemIndex] := MaskItem;
        end;

    end
  else  // if we delete category
    begin
      iCount := gColorExt.MaskItemList.Count - 1;
      for I := iCount downto 0 do  // delete old categories
        begin
          gColorExt.MaskItemList.Delete(I);
        end;
      iCount := lbCategories.Count;
      for I := 0 to iCount - 1 do  //write new categories
        begin
          gColorExt.MaskItemList.Add(lbCategories.Items.Objects[I]);
        end;
    end; // delete category

    bbtnDeleteCategory.Tag := 0;
end;

procedure TfrmOptions.bbtnDeleteCategoryClick(Sender: TObject);
begin
  lbCategories.Items.Delete(lbCategories.ItemIndex);
  bbtnDeleteCategory.Tag := 1; // show that we delete category
  if lbCategories.Count > 0 then
    lbCategories.ItemIndex := 0;
  lbCategoriesClick(lbCategories);
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
  optColorDialog.Color:= cbBackgroundColorViewerBook.Color;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbBackgroundColorViewerBook,optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptions.btnCategoryColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbCategoryColor.Color;
  if optColorDialog.Execute then
    SetColorInColorBox(cbCategoryColor,optColorDialog.Color);
end;

{ /File type color }

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

{/ File lbtypes category color }

procedure TfrmOptions.btClearHotKeyClick(Sender: TObject);
var st:TStringList;
    cat:string;
    i:integer;
    lstr: String;
    lslHotKeys: TStringList;
    sShortCut: String;
begin

 //TODO: delete hotkey.
 //TODO:New interface for hotkeys

 if lbxCategories.ItemIndex=-1 then exit;
 cat:=lbxCategories.Items[lbxCategories.ItemIndex];
 sShortCut := stgHotkeys.Cells[0, stgHotkeys.Row];
 if cat='Main' then
 begin
    i:=HotMan.GetHotKeyIndex(sShortCut);
    if i=-1 then exit; // no hotkey in hotkey manager

    st:=TStringList.Create;
     if HotMan.GetControlsListBy(sShortCut,st)>0 then
       begin
         // get command assigned for sShortCut
         HotMan.GetCommandsListBy(sShortCut,st);

         HotMan.DeleteHotKey(sShortCut,frmMain);
         edtParam.Text:='';
         edHotKey.Text:='';
         btClearHotKey.Enabled:=false;
         btSetHotKey.Enabled:=false;
         lbPressedHotKeyCommand.Caption:='';

         // if hotkey in hotkeylist, delete him
         DeleteHotkeyFromGrid(sShortCut);

         // if exist assigned command for sShortCut then refresh stringgrid
         if st.Count>0 then
         begin
              lstr:=copy(st[0],pos('=',st[0])+1,Length(st[0])-pos('=',st[0]));
              i:=stgCommands.Cols[0].IndexOf(lstr);
              if i>0 then
              begin
                   lslHotKeys:=TStringList.Create;
                   getHotKeyListByCommand(stgCommands.Cells[stgCmdCommandIndex,i],lslHotKeys);
                   stgCommands.Cells[2,i]:=StListToStr(';',lslHotKeys);
                   lslHotKeys.Free;
              end;
         end;
       end;
    st.free;
 end;
end;

procedure TfrmOptions.btnBackColor2Click(Sender: TObject);
begin
  optColorDialog.Color:= cbBackColor2.Color;
  if optColorDialog.Execute then
   begin
    SetColorInColorBox(cbBackColor2,optColorDialog.Color);
    pbExample.Repaint;
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

procedure TfrmOptions.btnCursorColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbCursorColor.Color;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbCursorColor,optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptions.btnCursorTextClick(Sender: TObject);
begin
  optColorDialog.Color:= cbCursorText.Color;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbCursorText,optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptions.btnMarkColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbMarkColor.Color;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbMarkColor,optColorDialog.Color);
    pbExample.Repaint;
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
  if tvTreeView.Selected.ImageIndex = 20 then // special for "Colors" item
    begin
      nbNotebook.PageIndex := 4;
      pnlCaption.Caption := tvTreeView.Items.Item[5].Text;
    end
  else
    begin
      nbNotebook.PageIndex := tvTreeView.Selected.ImageIndex; // temporally image index
      pnlCaption.Caption := tvTreeView.Selected.Text;
    end;
end;

procedure TfrmOptions.LoadConfig;
begin
  { Layout page }
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
  SetColorInColorBox(cbTextColor,gForeColor);
  SetColorInColorBox(cbBackColor,gBackColor);
  SetColorInColorBox(cbBackColor2,gBackColor2);
  SetColorInColorBox(cbMarkColor,gMarkColor);
  SetColorInColorBox(cbCursorColor,gCursorColor);
  SetColorInColorBox(cbCursorText,gCursorText);
  SetColorInColorBox(cbBackgroundColorViewerBook,gBookBackgroundColor);
  SetColorInColorBox(cbFontColorViewerBook,gBookFontColor);
  cbbUseInvertedSelection.Checked:=gUseInvertedSelection;
  cbbUseFrameCursor.Checked:=gUseFrameCursor;
  tbInactivePanelBrightness.Position:=gInactivePanelBrightness;

  { File operations }
  edtCopyBufferSize.Text:= IntToStr(gCopyBlockSize div 1024);
  cbSkipFileOpError.Checked:= gSkipFileOpError;
  cbDropReadOnlyFlag.Checked := gDropReadOnlyFlag;
  rbUseMmapInSearch.Checked := gUseMmapInSearch;
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
  cbTabsMultiLines.Visible:= (nbcMultiline in pcPluginsTypes.GetCapabilities);
  if cbTabsMultiLines.Visible then
     cbTabsMultiLines.Checked:= tb_multiple_lines in gDirTabOptions;
  cbTabsShowCloseButton.Visible:= (nbcShowCloseButtons in pcPluginsTypes.GetCapabilities);
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
  rbToolTipAllFiles.Checked:= (stm_show_for_all in gShowToolTipMode);
  rbToolTipOnlyLarge.Checked:= (stm_only_large_name in gShowToolTipMode);
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
  FillFileColorsList;


   FillColumnsList;
   // fill commands
   FillCommandsPage;
   // fill archiver list
   FillArchiverList;

  { Fill plugins lists }
  tmpDSXPlugins.Assign(gDSXPlugins);
  tmpWCXPlugins.Assign(gWCXPlugins);
  tmpWDXPlugins.Assign(gWDXPlugins);
  tmpWFXPlugins.Assign(gWFXPlugins);
  tmpWLXPlugins.Assign(gWLXPlugins);
end;

procedure TfrmOptions.SaveConfig;
begin
  { Layout page }
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
  gForeColor := cbTextColor.Color;
  gBackColor := cbBackColor.Color; // background color
  gBackColor2 := cbBackColor2.Color;
  gMarkColor := cbMarkColor.Color;
  gCursorColor := cbCursorColor.Color;
  gCursorText := cbCursorText.Color;
  gBookBackgroundColor := cbBackgroundColorViewerBook.Color;
  gBookFontColor := cbFontColorViewerBook.Color;
  gUseInvertedSelection:=cbbUseInvertedSelection.Checked;
  gInactivePanelBrightness:=tbInactivePanelBrightness.Position;
  gUseFrameCursor:=cbbUseFrameCursor.Checked;

  { File operations }
  gCopyBlockSize := StrToIntDef(edtCopyBufferSize.Text, gCopyBlockSize) * 1024;
  gSkipFileOpError:= cbSkipFileOpError.Checked;
  gDropReadOnlyFlag := cbDropReadOnlyFlag.Checked;
  gUseMmapInSearch := rbUseMmapInSearch.Checked;
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
  gShowToolTipMode:= []; // Reset tool tip show mode
  if rbToolTipAllFiles.Checked then
    Include(gShowToolTipMode, stm_show_for_all);
  if rbToolTipOnlyLarge.Checked then
    Include(gShowToolTipMode, stm_only_large_name);
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
  frmMain.SaveShortCuts;
  
  { Set plugins lists }
  gDSXPlugins.Assign(tmpDSXPlugins);
  gWCXPlugins.Assign(tmpWCXPlugins);
  gWDXPlugins.Assign(tmpWDXPlugins);
  gWFXPlugins.Assign(tmpWFXPlugins);
  gWLXPlugins.Assign(tmpWLXPlugins);
end;

procedure TfrmOptions.SetColorInColorBox(const lcbColorBox: TColorBox;
  const lColor: TColor);
//< setelect in lcbColorBox lColor if lColor in lcbColorBox else
// add to lcbColorBox lColor and select him
var
  i: LongInt;
begin
     if(lcbColorBox=nil) then exit; // if lcbColorBox not exist;

     with lcbColorBox do
     begin
       //search lColor in colorbox colorlist
       for i:=0 to Items.Count-1 do
        if Colors[i]=lColor then //find color
         begin
       // select color
           Selected:=lColor;
       // set colorbox color to lColor
           Color:=lColor;
           exit;
         end;//  if for

       //add items to colorbox list
       Items.Objects[Items.Add('$'+HexStr(lColor,8))]:=TObject(PtrInt(lColor));
       Color:=lColor;
       Selected:=lColor;
     end; // with
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
