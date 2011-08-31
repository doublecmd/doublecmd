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
  EditBtn, Grids, uGlobs, fOptionsFrame;

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
    btnNewColumnsSet: TButton;
    btnEditColumnsSet: TButton;
    btnDelColumnsSet: TButton;
    btnCopyColumnsSet: TButton;
    btnBackViewerColor: TButton;
    btnFontViewerColor: TButton;
    cbSortCaseSensitive: TCheckBox;
    cbDropReadOnlyFlag: TCheckBox;
    cbIconsShowOverlay: TCheckBox;
    cbIconsSize: TComboBox;
    cbRenameSelOnlyName: TCheckBox;
    cbShowCopyTabSelectPanel: TCheckBox;
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
    cbTabsOpenForeground: TCheckBox;
    cbbFileSystem: TComboBox;
    cbProcessComments: TCheckBox;
    cbTabsConfirmCloseAll: TCheckBox;
    cbTabsLockedAsterisk: TCheckBox;
    cbGridVertLine: TCheckBox;
    cbGridHorzLine: TCheckBox;
    cbShowWarningMessages: TCheckBox;
    cbSpaceMovesDown: TCheckBox;
    cbDirBrackets: TCheckBox;
    cbTabsActivateOnClick: TCheckBox;
    cbDeleteToTrash: TCheckBox;
    cbSkipFileOpError: TCheckBox;
    cbShowDialogOnDragDrop: TCheckBox;
    cbWatchExcludeDirs: TCheckBox;
    cbTabsOpenNearCurrent: TCheckBox;
    cbToolsRunInTerminal: TCheckBox;
    cbToolsKeepTerminalOpen: TCheckBox;
    cbToolsUseExternalProgram: TCheckBox;
    cbSaveThubnails: TCheckBox;
    cbShowMainMenu: TCheckBox;
    cbPartialNameSearch: TCheckBox;
    chkMultiArcDebug: TCheckBox;
    chkMultiArcOutput: TCheckBox;
    chkSearchReplaceHistory: TCheckBox;
    chkSaveConfiguration: TCheckBox;
    chkMultiArcEnabled: TCheckBox;
    chkIgnoreEnable: TCheckBox;
    cmbTabsPosition: TComboBox;
    cbSortMethod: TComboBox;
    edtArchiveListEnd: TEdit;
    edtArchiveListStart: TEdit;
    edtArchiveAdd: TEdit;
    edtArchiveExtension: TEdit;
    edtArchiveExtract: TEdit;
    edtArchiveList: TEdit;
    edtDescription: TEdit;
    edtToolsParameters: TEdit;
    edtTabsLimitLength: TEdit;
    edtCopyBufferSize: TEdit;
    cbLogFile: TCheckBox;
    fneArchiver: TFileNameEdit;
    fneToolsPath: TFileNameEdit;
    fneSaveIn: TFileNameEdit;
    gbArchiverOptions: TGroupBox;
    gbExactNameMatch: TGroupBox;
    fneLogFileName: TFileNameEdit;
    gbLogFile: TGroupBox;
    gbLogFileOp: TGroupBox;
    gbLogFileStatus: TGroupBox;
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
    gbTabs: TGroupBox;
    gbFileSearch: TGroupBox;
    gbLocConfigFiles: TGroupBox;
    gbSaveOnExit: TGroupBox;
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
    lblNumberColumnsViewer: TLabel;
    lblFontColorViewerBook: TLabel;
    lblBackgroundColorViewerBook: TLabel;
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
    lblQuickSearch: TLabel;
    lblQuickFilter: TLabel;
    lblSaveIn: TLabel;
    lbxMultiArc: TListBox;
    memArchiveListFormat: TMemo;
    memIgnoreList: TMemo;
    pgToolTips: TPage;
    pbViewerBook: TPaintBox;
    pnlMultiArcButtons: TPanel;
    pcArchiverCommands: TPageControl;
    pgArchivers: TPage;
    pgIgnoreList: TPage;
    pnlArchiverCommands: TPanel;
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
    lbcategory: TLabel;
    lblWipePassNumber: TLabel;
    lblConfigColumns: TLabel;
    lstColumnsSets: TListBox;
    pgIcons: TPage;
    pgAutoRefresh: TPage;
    pgMisc: TPage;
    pnlButtons: TPanel;
    pgColumns: TPage;
    rbCtrlAltLetterQS: TRadioButton;
    rbAltLetterQS: TRadioButton;
    rbNoneQS: TRadioButton;
    cbExactBeginning: TCheckBox;
    cbExactEnding: TCheckBox;
    rbLetterQS: TRadioButton;
    ilTreeView: TImageList;
    lblChar: TLabel;
    lblCopyBufferSize: TLabel;
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
    seNumberColumnsViewer: TSpinEdit;
    splMultiArc: TSplitter;
    splOptionsSplitter: TSplitter;
    gbIconsSize: TGroupBox;
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
    procedure btnConfigApplyClick(Sender: TObject);
    procedure btnConfigEditClick(Sender: TObject);
    procedure btnMultiArcAddClick(Sender: TObject);
    procedure btnMultiArcApplyClick(Sender: TObject);
    procedure btnMultiArcDeleteClick(Sender: TObject);
    procedure btnMultiArcRenameClick(Sender: TObject);
    procedure cbIconsSizeChange(Sender: TObject);
    procedure cbWatchExcludeDirsChange(Sender: TObject);
    procedure chkIgnoreEnableChange(Sender: TObject);
    procedure chkMultiArcEnabledChange(Sender: TObject);
    procedure chkSaveConfigurationChange(Sender: TObject);
    procedure lbxMultiArcSelectionChange(Sender: TObject; User: boolean);
    procedure OnAutoRefreshOptionChanged(Sender: TObject);
    procedure btnCopyColumnsSetClick(Sender: TObject);
    procedure btnDelColumnsSetClick(Sender: TObject);
    procedure btnEditColumnsSetClick(Sender: TObject);
    procedure btnNewColumnsSetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure nbNotebookPageChanged(Sender: TObject);
    procedure rbIconsShowNoneChange(Sender: TObject);
    procedure rbQuickSearchFilterKeyChange(Sender: TObject);
    procedure stgArchiverCommandsPrepareCanvas(Sender: TObject; aCol,
      aRow: Integer; aState: TGridDrawState);
    procedure tbArchiverAdditionalShow(Sender: TObject);
    procedure tvTreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    FOptionsEditorList: TOptionsEditorList;
    procedure CreateOptionsEditorList;
  public
    procedure FillColumnsList;
    procedure FillIgnoreList(bWithFullPath: Boolean);
    procedure FillArchiverList;
    procedure LoadConfig;
    procedure SaveConfig;
  end;

implementation

{$R *.lfm}

uses
  uLng, uGlobsPaths, uPixMapManager, fMain, LCLProc, LCLVersion,
  uColorExt, uDCUtils, uOSUtils, fColumnsSetConf, uShowMsg, uShowForm,
  uTypes, StrUtils, uFindEx, uKeyboard,
  fMaskInputDlg, uSearchTemplate, uMultiArc, uFile, uDebug,
  fOptionsPlugins, fOptionsToolTips, fOptionsColors, fOptionsLanguage,
  fOptionsBehaviour, fOptionsTools, fOptionsHotkeys, fOptionsLayout,
  fOptionsFonts;

const
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

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  // Localize some ComboBox
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

procedure TfrmOptions.stgArchiverCommandsPrepareCanvas(Sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if aRow = 0 then
    stgArchiverCommands.Canvas.Brush.Color:= stgArchiverCommands.FixedColor
  else
    stgArchiverCommands.Canvas.Brush.Color:= stgArchiverCommands.Color;
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

procedure TfrmOptions.CreateOptionsEditorList;
var
  I: LongInt;
  aOptionsEditor: TOptionsEditor;
begin
  FOptionsEditorList:= TOptionsEditorList.Create;
  for I:= 0 to OptionsEditorClassList.Count - 1 do
  begin
    aOptionsEditor:= OptionsEditorClassList[I].OptionsEditorClass.Create(Self);
    aOptionsEditor.Align := alClient;
    aOptionsEditor.Parent:= nbNotebook.Page[Integer(OptionsEditorClassList[I].OptionsEditorType)];
    FOptionsEditorList.Add(aOptionsEditor);
  end;
end;

procedure TfrmOptions.nbNotebookPageChanged(Sender: TObject);
begin 
  // temporally this is hack for bug http://www.freepascal.org/mantis/view.php?id=9635
  nbNotebook.Page[nbNotebook.PageIndex].Height := nbNotebook.Height - 8;
  nbNotebook.Page[nbNotebook.PageIndex].Height := nbNotebook.Height - 8;
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

  FillColumnsList;
  // fill archiver list
  FillArchiverList;

  { Load options to frames }
  for I:= 0 to FOptionsEditorList.Count - 1 do
    FOptionsEditorList[I].Load;
end;

procedure TfrmOptions.SaveConfig;
var
  I: LongInt;
  NeedsRestart: Boolean = False;
begin
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
  cbRenameSelOnlyName.Checked:= gRenameSelOnlyName;
  gRenameSelOnlyName:= cbRenameSelOnlyName.Checked;

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
     (gShowIconsNew <> gShowIcons) then
    begin
      gIconsSizeNew:= StrToInt(Copy(cbIconsSize.Text, 1, 2)); // new file panel icons size
      NeedsRestart := True;
    end;

  { Save options from frames }
  for I:= 0 to FOptionsEditorList.Count - 1 do
    if oesfNeedsRestart in FOptionsEditorList[I].Save then
      NeedsRestart := True;

  if NeedsRestart then
    msgOk(rsMsgRestartForApplyChanges);

  frmMain.UpdateWindowView;
end;

end.
