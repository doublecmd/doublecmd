{
   Double Commander
   -------------------------------------------------------------------------
   Directories synchronization utility (specially for DC)

   Copyright (C) 2013 Anton Panferov (ast.a_s@mail.ru)
   Copyright (C) 2014-2026 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit fSyncDirsDlg;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, IntegerList, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ComCtrls, Grids, Menus, ActnList, EditBtn,
  DCOSUtils,
  uFileView, uFileSource, uFileSourceCopyOperation, uFile, uFileSourceOperation,
  uFileSourceOperationMessageBoxesUI, uFormCommands, uHotkeyManager, uClassesEx,
  uFileSourceOperationOptions, uFileSourceDeleteOperation, KASProgressBar,
  uMasks, uSearchTemplate,
  uSyncDirsModel, uSyncDirsService;

const
  HotkeysCategory = 'Synchronize Directories';

type
  { TDrawGrid }

  TDrawGrid = class(Grids.TDrawGrid)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  { TSyncDirsDlgState }

  TSyncDirsDlgState = (
    sddsWaiting,

    // "Compare" button
    // running in Main Thread
    // update statistics of FFilteredCount in Status Bar
    // need to call Application.ProcessMessages()
    sddsScanning,

    // Compare with "by Content" checkbox
    // running in Background Thread, but Block other operations in Main Thread
    // it is not possible to perform other operations at the same time
    // update statistics of FFilteredCount in Status Bar
    // update statistics of FileSize in Progress Bar
    // NO need to call Application.ProcessMessages()
    sddsCheckingContent,

    // "Synchronize" button
    // running in Main Thread
    // update statistics of FOperation in Progress Bar (FileSize and FileCount)
    // need to call Application.ProcessMessages()
    sddsSynchronizing,

    // "Delete left / Delete right / Delete Both" in Popup Context Menu
    // running in Main Thread
    // update statistics of FOperation in Progress Bar (FileCount only)
    // need to call Application.ProcessMessages()
    sddsDeletingFiles,

    // "Copy file properties" in Popup Context Menu
    // running in Main Thread
    // update statistics of FileCount in Progress Bar (FileCount only)
    // need to call Application.ProcessMessages()
    sddsCopyingProperties
  );

  { TSyncDirsDlgStateHelper }

  TSyncDirsDlgStateHelper = class
  private
    _state: TSyncDirsDlgState;
    _running: Boolean;

    constructor Create;
    procedure enter(const state: TSyncDirsDlgState);
    procedure stop;
    procedure reset;

    function isInState(const state: TSyncDirsDlgState): Boolean;
    function isRunningInState(const state: TSyncDirsDlgState): Boolean;
    function isRunning: Boolean;
  end;

  { TSyncDirsDlgProgressHelper }

  TSyncDirsDlgProgressHelper = class
  private
    isSize: Boolean;

    panelControl: TPanel;
    labelControl: TLabel;
    barControl: TKASProgressBar;

    caption: String;
    predefinedCaption: String;
    total: Int64;
    done: Int64;

    constructor Create(const aIsSize: Boolean; const aPanelControl: TPanel; const aLabelControl: TLabel; const aBarControl: TKASProgressBar);

    procedure open(const aCaption: String; const aTotal: Int64);
    procedure close;
    procedure update(const aDone: Int64);
    procedure updateBarControl;
  end;

  { TfrmSyncDirsDlg }

  TfrmSyncDirsDlg = class(
    TForm,
    IFormCommands,
    ISyncDirsFileProcessorWithUI,
    ISyncDirsCheckContentThreadCallback,
    ISyncDirsTreeBuilderCallback,
    ISyncDirsCommonCallback )

    actDeleteLeft: TAction;
    actDeleteRight: TAction;
    actDeleteBoth: TAction;
    actCopyPropertiesLeftToRight: TAction;
    actCopyPropertiesRightToLeft: TAction;
    actSelectDeleteLeft: TAction;
    actSelectDeleteRight: TAction;
    actSelectDeleteBoth: TAction;
    actSelectCopyReverse: TAction;
    actSelectClear: TAction;
    actSelectCopyLeftToRight: TAction;
    actSelectCopyRightToLeft: TAction;
    actSelectCopyDefault: TAction;
    ActionList: TActionList;
    btnAbort: TBitBtn;
    btnCompare: TButton;
    btnSynchronize: TButton;
    btnClose: TButton;
    chkEmptyDir: TCheckBox;
    chkAsymmetric: TCheckBox;
    chkSubDirs: TCheckBox;
    chkByContent: TCheckBox;
    chkIgnoreDate: TCheckBox;
    chkOnlySelected: TCheckBox;
    cbExtFilter: TComboBox;
    edPath1: TDirectoryEdit;
    edPath2: TDirectoryEdit;
    HeaderDG: TDrawGrid;
    lblFileSizeProgress: TLabel;
    lblFileCountProgress: TLabel;
    MainDrawGrid: TDrawGrid;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    LeftPanel1: TPanel;
    LeftPanel2: TPanel;
    miCopyPropertiesRightToLeft: TMenuItem;
    miCopyPropertiesLeftToRight: TMenuItem;
    miSeparator4: TMenuItem;
    miDeleteBoth: TMenuItem;
    miDeleteRight: TMenuItem;
    miDeleteLeft: TMenuItem;
    miSeparator3: TMenuItem;
    miSelectDeleteLeft: TMenuItem;
    miSelectDeleteRight: TMenuItem;
    miSelectDeleteBoth: TMenuItem;
    miSeparator2: TMenuItem;
    miSelectCopyReverse: TMenuItem;
    miSeparator1: TMenuItem;
    miSelectCopyLeftToRight: TMenuItem;
    miSelectCopyRightToLeft: TMenuItem;
    miSelectCopyDefault: TMenuItem;
    miSelectClear: TMenuItem;
    MenuItemCompare: TMenuItem;
    MenuItemViewRight: TMenuItem;
    MenuItemViewLeft: TMenuItem;
    pnlFilter: TPanel;
    pnlProgress: TPanel;
    pnlFileSizeProgress: TPanel;
    pnlFileCountProgress: TPanel;
    pmGridMenu: TPopupMenu;
    pbFileSizeProgress: TKASProgressBar;
    pbFileCountProgress: TKASProgressBar;
    sbCopyRight: TSpeedButton;
    sbEqual: TSpeedButton;
    sbNotEqual: TSpeedButton;
    sbUnknown: TSpeedButton;
    sbCopyLeft: TSpeedButton;
    sbDuplicates: TSpeedButton;
    sbSingles: TSpeedButton;
    btnSearchTemplate: TSpeedButton;
    StatusBar1: TStatusBar;
    Timer: TTimer;
    TopPanel: TPanel;
    procedure actExecute(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSearchTemplateClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure btnSynchronizeClick(Sender: TObject);
    procedure edPath1AcceptDirectory(Sender: TObject; var Value: String);
    procedure RestoreProperties(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MainDrawGridDblClick(Sender: TObject);
    procedure MainDrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure MainDrawGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MainDrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HeaderDGHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure HeaderDGHeaderSizing(sender: TObject; const IsColumn: boolean;
      const aIndex, aSize: Integer);
    procedure FilterSpeedButtonClick(Sender: TObject);
    procedure MenuItemViewClick(Sender: TObject);
    procedure pmGridMenuPopup(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FCommands: TFormCommands;
    FIniPropStorage: TIniPropStorageEx;
  private
    { private declarations }
    FState: TSyncDirsDlgStateHelper;
    FSortService: TSyncDirsSortService;
    FFullTree: TTwoLevelTree;
    FFilteredList: TFlatDirFileList;
    FSortIndex: Integer;
    FSortDesc: Boolean;
    FCompareOption: TSyncDirsCompareOption;
    FMaskList: TMaskList;
    FTemplate: TSearchTemplate;
    FSelectedFiles: TDoubleFiles;
    FFileSources: TDoubleFileSources;
    FCmpFileSources: TDoubleFileSources;
    FCmpFilePaths: TDoublePaths;
    FAddresses: TDoublePaths;
    hCols: array [0..6] of record Left, Width: Integer end;
    FFilteredCount: TSyncDirsFlatCount;
    FOperation: TFileSourceOperation;
    FileExistsOption: TFileSourceOperationOptionFileExists;
    SymLinkOption: TFileSourceOperationOptionSymLink;
    VerifyOption: Boolean;
    FCopyStatistics: TFileSourceCopyOperationStatistics;
    FDeleteStatistics: TFileSourceDeleteOperationStatistics;
    FFileSourceOperationMessageBoxesUI: TFileSourceOperationMessageBoxesUI;

    FFileSizeProgressHelper: TSyncDirsDlgProgressHelper;
    FFileCountProgressHelper: TSyncDirsDlgProgressHelper;

    function createCompareOption: TSyncDirsCompareOption;
    function createFilterFlags: TFilterFlags;
    function createSelectionIndexes: TIntegerList;

    procedure toggleSelectionAction;
    procedure setSelectionAction(const newAction: TSyncRecState);

    procedure doCopyProperties(const targetSide: TDoubleSide);

    procedure Compare;
    procedure FillFoundItemsDG;
    procedure InitVisibleItems;
    procedure RecalcHeaderCols;
    procedure ScanDirs;
    procedure SetSortIndex(AValue: Integer);
    procedure UpdateStatusBar;
    procedure EnableControls(AEnabled: Boolean);
    procedure DeleteSelectedFiles(ALeft, ARight: Boolean);

  private
    function fileProcessorWithUICopyFiles(
      const sourceFS: IFileSource;
      const targetFS: IFileSource;
      var files: TFiles;
      const targetPath: String): Boolean;
    function fileProcessorWithUIDeleteFiles(
      const FileSource: IFileSource;
      var Files: TFiles): Boolean;
    function fileProcessorWithUICopyProperties(
      const targetFS: IFileSource;
      const sourceFile: TFile;
      const targetFile: TFile;
      const attrs: TCopyAttributesOptions ): Boolean;
    function fileProcessorWithUIDeleteFile(
      const FileSource: IFileSource;
      const f: TFile): Boolean;

  private
    FCheckContentThread: TSyncDirsCheckContentThread;
    FCheckContentThreadTimerCount: Integer;
  private
    procedure onCheckContentThreadStart;
    procedure onCheckContentThreadFinish;
    procedure onCheckContentThreadReapplyFilter;
    procedure onCheckContentThreadCountUpdated( const totalInc: Integer; const equalInc: Integer; const notEqInc: Integer );

    procedure checkContentThreadStart;
    procedure checkContentThreadStop;
    procedure checkContentThreadUpdateStat(const doneBytes: Int64; const totalBytes: Int64);

  private
    function treeBuilderMaskFilt( const f: TFile ): Boolean;
    procedure onTreeBuilderUpdateProgress( const percent: Integer );

  private
    function commonCheckRunning(const processMessages: Boolean): Boolean;

  private
    property leftFS: IFileSource read FFileSources[dsLeft] write FFileSources[dsLeft];
    property rightFS: IFileSource read FFileSources[dsRight] write FFileSources[dsRight];
    property leftAddress: String read FAddresses[dsLeft] write FAddresses[dsLeft];
    property rightAddress: String read FAddresses[dsRight] write FAddresses[dsRight];

    property SortIndex: Integer read FSortIndex write SetSortIndex;
    property Commands: TFormCommands read FCommands implements IFormCommands;
  protected
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
                                 const AXProportion, AYProportion: Double); override;
  public
    { public declarations }
    constructor Create(AOwner: TComponent;
      FileView1, FileView2: TFileView); reintroduce;
    destructor Destroy; override;
  public
    procedure CopyToClipboard;
  published
    procedure cm_SelectClear(const {%H-}Params:array of string);
    procedure cm_SelectDeleteLeft(const {%H-}Params:array of string);
    procedure cm_SelectDeleteRight(const {%H-}Params:array of string);
    procedure cm_SelectDeleteBoth(const {%H-}Params:array of string);
    procedure cm_SelectCopyDefault(const {%H-}Params:array of string);
    procedure cm_SelectCopyReverse(const {%H-}Params:array of string);
    procedure cm_SelectCopyLeftToRight(const {%H-}Params:array of string);
    procedure cm_SelectCopyRightToLeft(const {%H-}Params:array of string);

    procedure cm_DeleteLeft(const {%H-}Params:array of string);
    procedure cm_DeleteRight(const {%H-}Params:array of string);
    procedure cm_DeleteBoth(const {%H-}Params:array of string);

    procedure cm_CopyPropertiesLeftToRight(const {%H-}Params:array of string);
    procedure cm_CopyPropertiesRightToLeft(const {%H-}Params:array of string);
  end;

resourcestring
  rsComparingPercent = 'Comparing... %d%% (ESC to cancel)';
  rsLeftToRightCopy = 'Left to Right: Copy %d files, total size: %s (%s)';
  rsRightToLeftCopy = 'Right to Left: Copy %d files, total size: %s (%s)';
  rsDeleteLeft = 'Left: Delete %d file(s)';
  rsDeleteRight = 'Right: Delete %d file(s)';
  rsFilesFound = 'Files found: %d  (Identical: %d, Different: %d, '
    + 'Unique left: %d, Unique right: %d)';

procedure ShowSyncDirsDlg(FileView1, FileView2: TFileView);

implementation

uses
  fMain, uDebug, fDiffer, fSyncDirsPerformDlg, fSyncDirsSelectPropertiesDlg,
  uGlobs, LCLType, LazUTF8, LazFileUtils,
  uFileSystemFileSource, DCDateTimeUtils,
  uDCUtils, uFileSourceOperationTypes, uShowForm, uAdministrator,
  uOSUtils, uLng, Math, uClipboard, fMaskInputDlg,
  LCLVersion, uTypes, uFileSystemCopyOperation, uFileSystemDeleteOperation, uFindFiles,
  uFileSourceManager, uFileSourceProperty, uShowMsg;

{$R *.lfm}

const
  GRID_COLUMN_FMT = 'HeaderDG_Column%d_Width';

procedure ShowSyncDirsDlg(FileView1, FileView2: TFileView);
  function isSupported: Boolean;
  var
    leftFS: IFileSource;
    rightFS: IFileSource;
  begin
    Result:= False;
    leftFS:= FileView1.FileSource;
    rightFS:= FileView2.FileSource;
    if NOT (fspSynchronizable in leftFS.GetProperties) then
      Exit;
    if NOT (fspSynchronizable in rightFS.GetProperties) then
      Exit;
    if NOT TSyncDirsUtil.supportsSyncDirs(leftFS,rightFS) then
      Exit;
    Result:= True;
  end;

begin
  if not Assigned(FileView1) then
    raise Exception.Create('ShowSyncDirsDlg: FileView1=nil');
  if not Assigned(FileView2) then
    raise Exception.Create('ShowSyncDirsDlg: FileView2=nil');
  if NOT isSupported then begin
    msgWarning(rsMsgErrNotSupported);
    Exit;
  end;

  with TfrmSyncDirsDlg.Create(Application, FileView1, FileView2) do
    Show;
end;

{ TDrawGrid }

procedure TDrawGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  C, R: Integer;
begin
  if Button <> mbRight then
    inherited MouseDown(Button, Shift, X, Y)
  else begin
    MouseToCell(X, Y, {%H-}C, {%H-}R);
    if (R >= 0) and (R < RowCount) then
    begin
      if not IsCellSelected[Col, R] then begin
        self.Row:= R;
        self.ClearSelections;
      end else begin
        C:= Row;
        PInteger(@Row)^:= R;
        InvalidateRow(C);
        InvalidateRow(R);
      end;
    end;
  end;
end;

{ TSyncDirsDlgStateHelper }

constructor TSyncDirsDlgStateHelper.Create;
begin
  self.reset;
end;

procedure TSyncDirsDlgStateHelper.enter(const state: TSyncDirsDlgState);
begin
  _state:= state;
  _running:= True;
end;

procedure TSyncDirsDlgStateHelper.stop;
begin
  _running:= False;
end;

procedure TSyncDirsDlgStateHelper.reset;
begin
  _state:= sddsWaiting;
  _running:= True;
end;

function TSyncDirsDlgStateHelper.isInState(const state: TSyncDirsDlgState): Boolean;
begin
  Result:= (_state=state);
end;

function TSyncDirsDlgStateHelper.isRunningInState(const state: TSyncDirsDlgState): Boolean;
begin
  Result:= _running AND self.isInState(state);
end;

function TSyncDirsDlgStateHelper.isRunning: Boolean;
begin
  Result:= _running AND NOT self.isInState(sddsWaiting);
end;

{ TSyncDirsDlgProgressHelper }

constructor TSyncDirsDlgProgressHelper.Create(
  const aIsSize: Boolean;
  const aPanelControl: TPanel;
  const aLabelControl: TLabel;
  const aBarControl: TKASProgressBar);
begin
  self.isSize:= aIsSize;
  self.panelControl:= aPanelControl;
  self.labelControl:= aLabelControl;
  self.barControl:= aBarControl;
end;

procedure TSyncDirsDlgProgressHelper.open(const aCaption: String; const aTotal: Int64);
begin
  self.labelControl.Caption:= aCaption;
  self.caption:= aCaption;
  self.predefinedCaption:= EmptyStr;
  self.total:= aTotal;
  self.done:= 0;
  self.updateBarControl;
  self.panelControl.Show;
end;

procedure TSyncDirsDlgProgressHelper.close;
begin
  self.caption:= EmptyStr;
  self.panelControl.Hide;
end;

procedure TSyncDirsDlgProgressHelper.update(const aDone: Int64);
begin
  self.done:= aDone;
  self.updateBarControl;
end;

procedure TSyncDirsDlgProgressHelper.updateBarControl;
var
  barText : String;
  labelText : String;
begin
  if self.caption = EmptyStr then
    Exit;

  if self.isSize then begin
    {
      not all FileSources can provide precise statistical information,
      nor can they accurately calculate the generated size, for example, MultiArchive/7z.
      so this check has been added to prevent reporting an erroneous progress exceeding 100%
    }
    if self.done > self.total then
      self.total:= self.done + 1;
    barText := cnvFormatFileSize(self.done, uoscOperation) + '/' + cnvFormatFileSize(self.total, uoscOperation);
  end else begin
    {
      1. defensively prevent exceeding 100%
      2. for "Delete all empty directories"
    }
    if self.done > self.total then
      self.total:= self.done;
    barText := IntToStrTS(self.done) + '/' + IntToStrTS(self.total);
  end;
  self.barControl.SetProgress(self.done, self.total, barText);

  {$IFDEF LCLCOCOA}
  if self.predefinedCaption <> EmptyStr then begin
    labelText := self.predefinedCaption
  end else if self.total > 0 then begin
    labelText := self.caption + ': ' + barText + ' (' + FloatToStrF((self.done / self.total) * 100, ffFixed, 0, 0) + '%)'
  end else begin
    labelText := self.caption;
  end;
  self.labelControl.Caption := labelText;
  {$ENDIF}
end;

{ TfrmSyncDirsDlg }

procedure TfrmSyncDirsDlg.actExecute(Sender: TObject);
var
  cmd: string;
begin
  cmd := (Sender as TAction).Name;
  cmd := 'cm_' + Copy(cmd, 4, Length(cmd) - 3);
  Commands.ExecuteCommand(cmd, []);
end;

procedure TfrmSyncDirsDlg.btnCloseClick(Sender: TObject);
begin
  Close
end;

procedure TfrmSyncDirsDlg.btnSearchTemplateClick(Sender: TObject);
var
  sMask: String;
  bTemplate: Boolean;
begin
  sMask:= cbExtFilter.Text;
  if ShowMaskInputDlg(rsMarkPlus, rsMaskInput, glsSyncMaskHistory, sMask) then
  begin
    bTemplate:= IsMaskSearchTemplate(sMask);
    cbExtFilter.Enabled:= not bTemplate;
    cbExtFilter.Text:= sMask;
  end;
end;

procedure TfrmSyncDirsDlg.btnAbortClick(Sender: TObject);
begin
  if Assigned(FOperation) then
    FOperation.Stop
  else
    FState.stop;
end;

procedure TfrmSyncDirsDlg.btnCompareClick(Sender: TObject);
begin
  if not IsMaskSearchTemplate(cbExtFilter.Text) then
    InsertFirstItem(Trim(cbExtFilter.Text), cbExtFilter);
  StatusBar1.Panels[0].Text := Format(rsComparingPercent, [0]);
  Compare;
end;

procedure TfrmSyncDirsDlg.btnSynchronizeClick(Sender: TObject);
  function getAddressPath(const side: TDoubleSide): String;
  begin
    Result:= FCmpFileSources[side].CurrentAddress + FCmpFilePaths[side];
  end;
var
  synchronizer: TSyncDirsSynchronizer;
  syncCount: TSyncDirsSyncCount;
  syncFlags: TSyncDirsSyncFlags;
  fileSizeVisible: Boolean;
  fileCountVisible: Boolean;
begin
  synchronizer:= TSyncDirsSynchronizer.Create( self, self, FFilteredList, FCmpFileSources, FCmpFilePaths );
  syncCount:= synchronizer.count;
  syncFlags:= [];

  FCopyStatistics.DoneBytes:= 0;
  FDeleteStatistics.DoneFiles:= 0;
  FCopyStatistics.TotalBytes:= syncCount.copySize;
  FDeleteStatistics.TotalFiles:= syncCount.deleteCount;

  with TfrmSyncDirsPerformDlg.Create(Self) do
  try
    edLeftPath.Text := getAddressPath(dsLeft);
    edRightPath.Text := getAddressPath(dsRight);;
    if syncCount.copyToLeftCount > 0 then
    begin
      chkRightToLeft.Enabled := True;
      chkRightToLeft.Checked := True;
      edLeftPath.Enabled := True;
    end;
    if syncCount.copyToRightCount > 0 then
    begin
      chkLeftToRight.Enabled := True;
      chkLeftToRight.Checked := True;
      edRightPath.Enabled := True;
    end;
    chkDeleteLeft.Enabled := syncCount.deleteLeftCount > 0;
    chkDeleteLeft.Checked := chkDeleteLeft.Enabled;
    chkDeleteRight.Enabled := syncCount.deleteRightCount > 0;
    chkDeleteRight.Checked := chkDeleteRight.Enabled;
    chkDeleteLeft.Caption := Format(rsDeleteLeft, [syncCount.deleteLeftCount]);
    chkDeleteRight.Caption := Format(rsDeleteRight, [syncCount.deleteRightCount]);
    chkLeftToRight.Caption :=
      Format(rsLeftToRightCopy, [syncCount.copyToRightCount, cnvFormatFileSize(syncCount.copyToRightSize, fsfFloat, gFileSizeDigits), IntToStrTS(syncCount.copyToRightSize)]);
    chkRightToLeft.Caption :=
      Format(rsRightToLeftCopy, [syncCount.copyToLeftCount, cnvFormatFileSize(syncCount.copyToLeftSize, fsfFloat, gFileSizeDigits), IntToStrTS(syncCount.copyToLeftSize)]);
    chkVerify.Visible := TSyncDirsUtil.supportsVerify(FCmpFileSources);
    chkVerify.Checked := gOperationOptionVerify;

    if ShowModal = mrOk then
    begin
      FState.enter(sddsSynchronizing);
      EnableControls(False);
      SymLinkOption:= fsooslNone;
      if chkConfirmOverwrites.Checked then
        FileExistsOption := fsoofeNone
      else begin
        FileExistsOption := fsoofeOverwrite;
      end;
      VerifyOption := chkVerify.Checked;

      if chkRightToLeft.Checked then
        Include( syncFlags, sfCopyToLeft );
      if chkLeftToRight.Checked then
        Include( syncFlags, sfCopyToRight );
      if chkDeleteLeft.Checked then
        Include( syncFlags, sfDeleteLeft );
      if chkDeleteRight.Checked then
        Include( syncFlags, sfDeleteRight );
      if chkDeleteLeftAllEmptyDirs.Checked then
        Include( syncFlags, sfDeleteLeftAllEmptyDirs );
      if chkDeleteRightAllEmptyDirs.Checked then
        Include( syncFlags, sfDeleteRightAllEmptyDirs );

      fileSizeVisible:= (sfCopyToLeft in syncFlags) or (sfCopyToRight in syncFlags);
      fileCountVisible:= (sfDeleteLeft in syncFlags) or (sfDeleteRight in syncFlags) or
                         (sfDeleteLeftAllEmptyDirs in syncFlags) or (sfDeleteRightAllEmptyDirs in syncFlags);

      if fileSizeVisible then
        FFileSizeProgressHelper.open(rsOperCopying, syncCount.copySize)
      else
        FFileSizeProgressHelper.close;
      if fileCountVisible then
        FFileCountProgressHelper.open(rsOperDeleting, syncCount.deleteCount)
      else
        FFileCountProgressHelper.close;

      synchronizer.sync( syncFlags );

      EnableControls(True);
      btnCompare.Click;
    end;
  finally
    if FState.isInState(sddsSynchronizing) then begin
      FFileSizeProgressHelper.close;
      FFileCountProgressHelper.close;
      FState.reset;
    end;
    synchronizer.Free;
    Free;
  end;
end;

procedure TfrmSyncDirsDlg.edPath1AcceptDirectory(Sender: TObject; var Value: String);
var
  side: TDoubleSide;
begin
  if Sender = edPath1 then
    side:= dsLeft
  else
    side:= dsRight;
  FFileSources[side] := TFileSystemFileSource.GetFileSource;
  FAddresses[side] := '';
end;

procedure TfrmSyncDirsDlg.MainDrawGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  c, r: Integer;
begin
  if Button <> mbLeft then
    Exit;

  MainDrawGrid.MouseToCell(X, Y, c, r);
  if (r < 0) or (r >= FFilteredList.Count)
  or (x - 2 < hCols[3].Left)
  or (x - 2 > hCols[3].Left + hCols[3].Width)
  then
    Exit;

  toggleSelectionAction;
end;

procedure TfrmSyncDirsDlg.RestoreProperties(Sender: TObject);
var
  Index: Integer;
begin
  with HeaderDG.Columns do
  begin
    for Index := 0 to Count - 1 do
      Items[Index].Width:= StrToIntDef(FIniPropStorage.StoredValue[Format(GRID_COLUMN_FMT, [Index])], Items[Index].Width);
  end;
  RecalcHeaderCols;
end;

procedure TfrmSyncDirsDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Index: Integer;
begin
  checkContentThreadStop;
  CloseAction := caFree;
  { settings }
  gSyncDirsEmptyDirs            := chkEmptyDir.Checked;
  gSyncDirsSubdirs              := chkSubDirs.Checked;
  gSyncDirsAsymmetric           := chkAsymmetric.Checked and gSyncDirsAsymmetricSave;
  gSyncDirsIgnoreDate           := chkIgnoreDate.Checked;
  gSyncDirsShowFilterCopyRight  := sbCopyRight.Down;
  gSyncDirsShowFilterEqual      := sbEqual.Down;
  gSyncDirsShowFilterNotEqual   := sbNotEqual.Down;
  gSyncDirsShowFilterUnknown    := sbUnknown.Down;
  gSyncDirsShowFilterCopyLeft   := sbCopyLeft.Down;
  gSyncDirsShowFilterDuplicates := sbDuplicates.Down;
  gSyncDirsShowFilterSingles    := sbSingles.Down;
  if gSyncDirsFileMaskSave = True then
  begin
    if not IsMaskSearchTemplate(cbExtFilter.Text) then
      gSyncDirsFileMask         := cbExtFilter.Text;
  end;
  if chkByContent.Enabled then
    gSyncDirsByContent          := chkByContent.Checked;
  glsSyncMaskHistory.Assign(cbExtFilter.Items);

  with HeaderDG.Columns do
  begin
    for Index := 0 to Count - 1 do
      FIniPropStorage.StoredValue[Format(GRID_COLUMN_FMT, [Index])]:= IntToStr(Items[Index].Width);
  end;
end;

procedure TfrmSyncDirsDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Assigned(FOperation) then
  begin
    FOperation.Stop;
    CanClose := False;
  end
  else if FState.isRunningInState(sddsScanning) then
  begin
    FState.reset;
    CanClose := False;
  end
  else if FState.isRunningInState(sddsCheckingContent) then
  begin
    CanClose := False;
    checkContentThreadStop;
  end;
end;

procedure TfrmSyncDirsDlg.FormCreate(Sender: TObject);
var
  Index: Integer;
  HMSync: THMForm;
begin
  // Initialize property storage
  FIniPropStorage := InitPropStorage(Self);
  FIniPropStorage.OnRestoreProperties:= @RestoreProperties;
  for Index := 0 to HeaderDG.Columns.Count - 1 do
  begin
    FIniPropStorage.StoredValues.Add.DisplayName:= Format(GRID_COLUMN_FMT, [Index]);
  end;

  {$IFDEF LCLCOCOA}
  pnlProgress.Color:=clBtnHighlight;
  {$ENDIF}
  MainDrawGrid.SelectedColor:= gColors.SyncDirs^.SelectedColor;

  FFileSizeProgressHelper:= TSyncDirsDlgProgressHelper.Create(
    True,
    pnlFileSizeProgress,
    lblFileSizeProgress,
    pbFileSizeProgress);
  FFileCountProgressHelper:= TSyncDirsDlgProgressHelper.Create(
    False,
    pnlFileCountProgress,
    lblFileCountProgress,
    pbFileCountProgress);

  { settings }
  chkEmptyDir.Checked    := gSyncDirsEmptyDirs;
  chkSubDirs.Checked     := gSyncDirsSubdirs;
  chkAsymmetric.Checked  := gSyncDirsAsymmetric;
  chkByContent.Checked   := gSyncDirsByContent and chkByContent.Enabled;
  chkIgnoreDate.Checked  := gSyncDirsIgnoreDate;
  sbCopyRight.Down       := gSyncDirsShowFilterCopyRight;
  sbEqual.Down           := gSyncDirsShowFilterEqual;
  sbNotEqual.Down        := gSyncDirsShowFilterNotEqual;
  sbUnknown.Down         := gSyncDirsShowFilterUnknown;
  sbCopyLeft.Down        := gSyncDirsShowFilterCopyLeft;
  sbDuplicates.Down      := gSyncDirsShowFilterDuplicates;
  sbSingles.Down         := gSyncDirsShowFilterSingles;
  if gSyncDirsFileMaskSave = False then
  begin
    Index := glsSyncMaskHistory.IndexOf(gSyncDirsFileMask);
    if Index <> -1 then
      glsSyncMaskHistory.Move(Index, 0)
    else
      glsSyncMaskHistory.Insert(0, gSyncDirsFileMask);
  end;
  cbExtFilter.Items.Assign(glsSyncMaskHistory);
  cbExtFilter.Text       := gSyncDirsFileMask;

  HMSync := HotMan.Register(Self, HotkeysCategory);
  HMSync.RegisterActionList(ActionList);
  FCommands := TFormCommands.Create(Self, ActionList);

{$IFDEF DARWIN}
  self.BorderIcons:= self.BorderIcons - [biMinimize];
{$ENDIF}
end;

procedure TfrmSyncDirsDlg.FormResize(Sender: TObject);
begin
  pbFileSizeProgress.Width:= ClientWidth div 3;
  pbFileCountProgress.Width:= pbFileSizeProgress.Width;
end;

procedure TfrmSyncDirsDlg.MainDrawGridDblClick(Sender: TObject);
var
  r, x: Integer;
  rec: TSyncRec;
begin
  r := MainDrawGrid.Row;
  if (r < 0) or (r >= FFilteredList.Count) then Exit;
  x := MainDrawGrid.ScreenToClient(Mouse.CursorPos).X;
  if (x > hCols[3].Left) and (x < hCols[3].Left + hCols[3].Width) then Exit;
  rec := FFilteredList.fileSyncRec(r);
  if rec.isDir or NOT rec.hasFilesOnBothSides or (rec.state = srsEqual) then
    Exit;
  PrepareToolData(self.leftFS, rec.leftFile, self.rightFS, rec.rightFile, @ShowDifferByGlobList);
end;

procedure TfrmSyncDirsDlg.MainDrawGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
const
  LEFT_FILE_INDENTATION = 8;
var
  rec: TSyncRec;
  x: Integer;
  s: string;
begin
  if (FFilteredList = nil) or (aRow >= FFilteredList.Count) then Exit;
  with MainDrawGrid.Canvas do
  begin
    rec := FFilteredList.fileSyncRec(aRow);
    if rec.isDir then
    begin
      if gdSelected in aState then begin
        Brush.Color:= gColors.SyncDirs^.DirSelectedColor
      end else begin
        {$IFDEF DARWIN}
        Brush.Color := clInfoBk;
        {$ELSE}
        Brush.Color := clBtnFace;
        {$ENDIF}
      end;
      FillRect(aRect);
      Font.Bold := True;
      Font.Color := clWindowText;
      with hCols[0] do
        TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
          Left + 2, aRect.Top + 2, FFilteredList.path(aRow));
    end else begin
      with gColors.SyncDirs^ do
      begin
        case rec.state of
        srsNotEq:       Font.Color := UnknownColor;
        srsCopyToLeft:    Font.Color := RightColor;
        srsCopyToRight:   Font.Color := LeftColor;
        srsDeleteLeft:  Font.Color := LeftColor;
        srsDeleteRight: Font.Color := RightColor;
        else Font.Color := clWindowText;
        end;
      end;
      if rec.hasLeftFile then
      begin
        with hCols[0] do
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            Left + 2 + LEFT_FILE_INDENTATION, aRect.Top + 2, FFilteredList.path(aRow));
        s := IntToStrTS(rec.leftFile.Size);
        with hCols[1] do begin
          x := Left + Width - 8 - TextWidth(s);
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            x, aRect.Top + 2, s);
        end;
        s := FormatDateTime(gDateTimeFormatSync, rec.leftFile.ModificationTime);
        with hCols[2] do
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            Left + 2, aRect.Top + 2, s)
      end;
      if rec.hasRightFile then
      begin
        TextOut(hCols[6].Left + 2, aRect.Top + 2, FFilteredList.path(aRow));
        s := IntToStrTS(rec.rightFile.Size);
        with hCols[5] do begin
          x := Left + Width - 8 - TextWidth(s);
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            x, aRect.Top + 2, s);
        end;
        s := FormatDateTime(gDateTimeFormatSync, rec.rightFile.ModificationTime);
        with hCols[4] do
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            Left + 2, aRect.Top + 2, s)
      end;
    end;
    if rec.isFile or (rec.state<>srsDoNothing) then begin
      ImageList1.Draw(MainDrawGrid.Canvas,
        hCols[3].Left + (hCols[3].Width - ImageList1.Width) div 2 - 2,
        (aRect.Top + aRect.Bottom - ImageList1.Height - 1) div 2, Ord(rec.action));
    end;
  end;
end;

procedure TfrmSyncDirsDlg.MainDrawGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ASelection: TGridRect;
begin
  case Key of
    VK_SPACE:
      begin
        toggleSelectionAction;
        Key:= 0;
      end;
    VK_A:
      if (Shift = [ssModifier]) then
      begin
        ASelection.Top:= 0;
        ASelection.Left:= 0;
        ASelection.Right:= MainDrawGrid.ColCount - 1;
        ASelection.Bottom:= MainDrawGrid.RowCount - 1;
        MainDrawGrid.Selection:= ASelection;
        Key:= 0;
      end;
    VK_C,
    VK_INSERT:
      if (Shift = [ssModifier]) then
      begin
        CopyToClipboard;
        Key:= 0;
      end;
  end;
end;

procedure TfrmSyncDirsDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key <> VK_ESCAPE then
    Exit;

  Key := 0;
  if FState.isRunningInState(sddsScanning) then
    FState.reset
  else if FState.isRunning then
    FState.stop
  else
    Close;
end;

procedure TfrmSyncDirsDlg.HeaderDGHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
begin
  if (Index <> 3) and (Index <= 6) then
    SortIndex := Index;
end;

procedure TfrmSyncDirsDlg.HeaderDGHeaderSizing(sender: TObject;
  const IsColumn: boolean; const aIndex, aSize: Integer);
begin
  RecalcHeaderCols;
  MainDrawGrid.Invalidate;
end;

procedure TfrmSyncDirsDlg.FilterSpeedButtonClick(Sender: TObject);
begin
  FillFoundItemsDG;
end;

procedure TfrmSyncDirsDlg.MenuItemViewClick(Sender: TObject);
var
  r: Integer;
  f: TFile = nil;
  rec: TSyncRec;
begin
  r := MainDrawGrid.Row;
  if (r < 0) or (r >= FFilteredList.Count) then Exit;
  rec := FFilteredList.fileSyncRec(r);
  if rec.isFile then
  begin
    if Sender = MenuItemViewLeft then
      f := rec.leftFile
    else if Sender = MenuItemViewRight then begin
      f := rec.rightFile;
    end;
    if Assigned(f) then ShowViewerByGlob(f.FullPath);
  end;
end;

procedure TfrmSyncDirsDlg.pmGridMenuPopup(Sender: TObject);
  procedure calcSelection;
  var
    fromIndex: Integer;
    toIndex: Integer;
  begin
    if MainDrawGrid.HasMultiSelection then
      Exit;
    if MainDrawGrid.Selection.Height>0 then
      Exit;

    fromIndex:= MainDrawGrid.Row;
    toIndex:= FFilteredList.lastFileInCurrentDir( fromIndex );
    MainDrawGrid.Selection:= TGridRect.Create(0,fromIndex,3,toIndex);
  end;

  procedure enableMenuItems;
  var
    indexes: TIntegerList;
    hasBothWithinSameRec: Boolean;
    leftCount: Integer;
    rightCount: Integer;
    hasLeft: Boolean;
    hasRight: Boolean;
    hasBoth: Boolean;
    enableCopyProperties: Boolean;
  begin
    indexes:= self.createSelectionIndexes;
    FFilteredList.countLeftRight(indexes, leftCount, rightCount);
    hasBothWithinSameRec:= FFilteredList.countBothSides(indexes) > 0;
    indexes.Free;

    hasLeft:= (leftCount > 0);
    hasRight:= (rightCount > 0);
    hasBoth:= hasLeft AND hasRight;

    enableCopyProperties:= hasBothWithinSameRec;
    if enableCopyProperties then
      enableCopyProperties:= TSyncDirsUtil.supportsCopyProperties(FFileSources);

    miSelectCopyLeftToRight.Enabled:= hasLeft;
    miSelectCopyRightToLeft.Enabled:= hasRight;
    MenuItemViewLeft.Enabled:= hasLeft;
    MenuItemViewRight.Enabled:= hasRight;
    MenuItemCompare.Enabled:= hasBothWithinSameRec;
    miCopyPropertiesLeftToRight.Enabled:= enableCopyProperties;
    miCopyPropertiesRightToLeft.Enabled:= enableCopyProperties;
    miSelectDeleteLeft.Enabled := hasLeft;
    miSelectDeleteRight.Enabled := hasRight;
    miSelectDeleteBoth.Enabled := hasBoth;
    miDeleteLeft.Enabled := hasLeft;
    miDeleteRight.Enabled := hasRight;
    miDeleteBoth.Enabled := hasBoth;
  end;
begin
  calcSelection;
  enableMenuItems;
end;

procedure TfrmSyncDirsDlg.TimerTimer(Sender: TObject);
var
  CopyStatistics: TFileSourceCopyOperationStatistics;
  DeleteStatistics: TFileSourceDeleteOperationStatistics;
begin
  if Assigned(FOperation) then begin
    if (FOperation is TFileSourceCopyOperation) then begin
      CopyStatistics:= TFileSourceCopyOperation(FOperation).RetrieveStatistics;
      FFileSizeProgressHelper.done:= FCopyStatistics.DoneBytes + CopyStatistics.DoneBytes;
    end else if (FOperation is TFileSourceDeleteOperation) then begin
      DeleteStatistics:= TFileSourceDeleteOperation(FOperation).RetrieveStatistics;
      FFileCountProgressHelper.done:= FDeleteStatistics.DoneFiles + DeleteStatistics.DoneFiles;
    end;
  end;

  if FState.isRunningInState(sddsCheckingContent) then begin
    CopyStatistics:= FCheckContentThread.RetrieveStatistics;
    checkContentThreadUpdateStat(CopyStatistics.DoneBytes, CopyStatistics.TotalBytes);
  end;

  FFileSizeProgressHelper.updateBarControl;
  FFileCountProgressHelper.updateBarControl;
end;

function TfrmSyncDirsDlg.createCompareOption: TSyncDirsCompareOption;
var
  flags: TSyncDirsCompareFlags;
begin
  flags:= [];
  if self.chkOnlySelected.Checked then
    Include( flags, TSyncDirsCompareFlag.cfOnlySelected );
  if self.chkEmptyDir.Checked then
    Include( flags, TSyncDirsCompareFlag.cfEmptyDirs );
  if self.chkAsymmetric.Checked then
    Include( flags, TSyncDirsCompareFlag.cfAsymmetric );
  if self.chkSubDirs.Checked then
    Include( flags, TSyncDirsCompareFlag.cfSubdirs );
  if self.chkByContent.Checked then
    Include( flags, TSyncDirsCompareFlag.cfByContent );
  if self.chkIgnoreDate.Checked then
    Include( flags, TSyncDirsCompareFlag.cfIgnoreDate );

  if TSyncDirsUtil.isBothFileSystemFileSource(FFileSources) then begin
    if gNtfsHourTimeDelay and NtfsHourTimeDelay(self.edPath1.Text, self.edPath2.Text) then
      Include( flags, TSyncDirsCompareFlag.cfNtfsShift );
  end;

  Result:= TSyncDirsCompareOption.Create( flags );
end;

function TfrmSyncDirsDlg.createFilterFlags: TFilterFlags;
begin
  Result:= [];
  if self.sbCopyRight.Down then
    Include( Result, TSyncDirsFilterFlag.ffCopyRight );
  if self.sbCopyLeft.Down then
    Include( Result, TSyncDirsFilterFlag.ffCopyLeft );
  if self.sbEqual.Down then
    Include( Result, TSyncDirsFilterFlag.ffEqual );
  if self.sbNotEqual.Down then
    Include( Result, TSyncDirsFilterFlag.ffNotEqual );
  if self.sbUnknown.Down then
    Include( Result, TSyncDirsFilterFlag.ffUnknown );
  if self.sbDuplicates.Down then
    Include( Result, TSyncDirsFilterFlag.ffDuplicate );
  if self.sbSingles.Down then
    Include( Result, TSyncDirsFilterFlag.ffSingle );
end;

function TfrmSyncDirsDlg.createSelectionIndexes: TIntegerList;
var
  i: Integer;
begin
  Result:= TIntegerList.Create;
  for i:= 0 to self.MainDrawGrid.RowCount-1 do begin
    if MainDrawGrid.IsCellSelected[0,i] then
      Result.Add( i );
  end;
end;

procedure TfrmSyncDirsDlg.SetSortIndex(AValue: Integer);
  function getSortIndicator: String;
  begin
    {$IF DEFINED(MSWINDOWS) or DEFINED(DARWIN)}
    if FSortDesc then Result:= '↓' else Result:= '↑';
    {$ELSE}
    if FSortDesc then Result:= '↑' else Result:= '↓';
    {$ENDIF}
  end;

var
  s: string;
begin
  FSortService.sortIndex := AValue;
  if AValue = FSortIndex then
  begin
    s := HeaderDG.Columns[AValue].Title.Caption;
    UTF8Delete(s, 1, 1);
    FSortDesc := not FSortDesc;
    FSortService.sortDesc := FSortDesc;
    s := getSortIndicator() + s;
    HeaderDG.Columns[AValue].Title.Caption := s;
    FSortService.sortTree(FFullTree);
    FillFoundItemsDG;
  end else begin
    if FSortIndex >= 0 then
    begin
      s := HeaderDG.Columns[FSortIndex].Title.Caption;
      UTF8Delete(s, 1, 1);
      HeaderDG.Columns[FSortIndex].Title.Caption := s;
    end;
    FSortIndex := AValue;
    FSortDesc := False;
    FSortService.sortDesc := FSortDesc;
    with HeaderDG.Columns[FSortIndex].Title do
      Caption := getSortIndicator() + Caption;
    FSortService.sortTree(FFullTree);
    FillFoundItemsDG;
  end;
end;

procedure TfrmSyncDirsDlg.Compare;
begin
  TopPanel.Enabled := False;
  try
    FFullTree.Clear;
    FCompareOption.Free;
    FCompareOption:= self.createCompareOption;
    MainDrawGrid.RowCount := 0;
    ScanDirs;
    MainDrawGrid.SetFocus;
  finally
    TopPanel.Enabled := NOT FState.isRunningInState(sddsCheckingContent);
  end;
end;

procedure TfrmSyncDirsDlg.FillFoundItemsDG;
begin
  InitVisibleItems;
  MainDrawGrid.ColCount := 1;
  MainDrawGrid.RowCount := FFilteredList.Count;
  MainDrawGrid.Invalidate;
  FFilteredCount:= FFilteredList.flatCount;
  UpdateStatusBar;
  if FFullTree.Count > 0 then
  begin
    btnCompare.Default := False;
    btnSynchronize.Enabled := True;
    btnSynchronize.Default := True;
  end else begin
    btnCompare.Default := True;
    btnSynchronize.Enabled := False;
    btnSynchronize.Default := False;
  end;
end;

procedure TfrmSyncDirsDlg.InitVisibleItems;
begin
  TSyncDirsUtil.filterFlatListWithFlags(FFullTree, FFilteredList, self.createFilterFlags);
end;

procedure TfrmSyncDirsDlg.RecalcHeaderCols;
var
  i, l: Integer;
begin
  l := 0;
  for i := 0 to 6 do
    with hCols[i] do
    begin
      Left := l;
      Width := HeaderDG.Columns[i].Width;
      l := l + Width;
    end;
end;

procedure TfrmSyncDirsDlg.ScanDirs;
var
  builder: TSyncDirsTreeBuilder = nil;
  basePaths: TDoublePaths;

  procedure setBaseDir(const text: String; const side: TDoubleSide);
  var
    path: String;
    address: String;
  begin
    path:= AppendPathDelim(text);
    address:= FAddresses[side];
    if (address <> '') and (Copy(path, 1, Length(address)) = address) then
      Delete(path, 1, Length(address));
    basePaths[side]:= path;
  end;

begin
  FState.enter(sddsScanning);
  try
    if IsMaskSearchTemplate(cbExtFilter.Text) then
    begin
      FMaskList := nil;
      FTemplate:= gSearchTemplateList.TemplateByName[cbExtFilter.Text];
    end
    else begin
      FTemplate := nil;
      if cbExtFilter.Text <> EmptyStr then
        FMaskList := TMaskList.Create(cbExtFilter.Text)
      else
        FMaskList := TMaskList.Create( '*' );
    end;
    FCmpFileSources := FFileSources;
    setBaseDir(edPath1.Text, dsLeft);
    setBaseDir(edPath2.Text, dsRight);
    FCmpFilePaths := basePaths;

    builder:= TSyncDirsTreeBuilder.Create( self, FSortService, FCompareOption, FFileSources, basePaths );
    builder.build( FFullTree, FSelectedFiles );

    FillFoundItemsDG;
    if NOT FState.isRunningInState(sddsScanning) then
      Exit;
    if (FFullTree.Count > 0) and (cfByContent in FCompareOption.flags) then
      checkContentThreadStart;
  finally
    FreeAndNil(builder);
    FreeAndNil(FMaskList);
    FTemplate := nil;
    if FState.isInState(sddsScanning) then
      FState.reset;
  end;
end;

procedure TfrmSyncDirsDlg.UpdateStatusBar;
var
  s: string;
begin
  s := Format(rsFilesFound, [FFilteredCount.total, FFilteredCount.equal, FFilteredCount.notEqual, FFilteredCount.leftUnique, FFilteredCount.rightUnique]);
  if FState.isRunningInState(sddsCheckingContent) then
    s := s + ' ...';
  StatusBar1.Panels[0].Text := s;
end;

procedure TfrmSyncDirsDlg.toggleSelectionAction;
begin
  setSelectionAction(srsNextAction);
end;

procedure TfrmSyncDirsDlg.EnableControls(AEnabled: Boolean);
begin
  edPath1.Enabled:= AEnabled;
  edPath2.Enabled:= AEnabled;
  TopPanel.Enabled:= AEnabled;
  HeaderDG.Enabled:= AEnabled;
  pnlFilter.Enabled:= AEnabled;
  MainDrawGrid.Enabled:= AEnabled;
  pnlProgress.Visible:= not AEnabled;
  Timer.Enabled:= not AEnabled;
end;

procedure TfrmSyncDirsDlg.setSelectionAction(const newAction: TSyncRecState);
var
  indexes: TIntegerList;
begin
  indexes:= self.createSelectionIndexes;
  MainDrawGrid.BeginUpdate;
  try
    FFilteredList.setNewAction( indexes, newAction );
  finally
    indexes.Free;
    MainDrawGrid.EndUpdate;
  end;
end;

procedure TfrmSyncDirsDlg.doCopyProperties(const targetSide: TDoubleSide);
  function selectProperties: TCopyAttributesOptions;
  var
    dialog: TfrmSyncDirsSelectPropertiesDlg;
  begin
    Result:= [];
    dialog:= TfrmSyncDirsSelectPropertiesDlg.Create(nil);
    dialog.chkTime.Checked:= gOperationOptionCopyTime;
    dialog.chkAttributes.Checked:= gOperationOptionCopyAttributes;
    dialog.chkPermissions.Checked:= gOperationOptionCopyOwnership or gOperationOptionCopyPermissions;
    dialog.chkXAttributes.Checked:= gOperationOptionCopyXattributes;
    if dialog.ShowModal = mrOK then begin
      if dialog.chkTime.Checked then
        Include(Result, caoCopyTime);
      if dialog.chkAttributes.Checked then
        Include(Result, caoCopyAttributes);
      if dialog.chkPermissions.Checked then
        Result:= Result + [caoCopyPermissions, caoCopyOwnership];
      if dialog.chkXAttributes.Checked then
        Include(Result, caoCopyXattributes);
    end;
    dialog.Free;
  end;

var
  commonService: TSyncDirsCommonService;
  indexes: TIntegerList = nil;
  attrs: TCopyAttributesOptions;
begin
  attrs:= selectProperties;
  if attrs = [] then
    Exit;

  commonService:= TSyncDirsCommonService.Create(self, self, FFilteredList, FCmpFileSources);
  try
    indexes:= self.createSelectionIndexes;

    FState.enter(sddsCopyingProperties);
    FFileSizeProgressHelper.close;
    FFileCountProgressHelper.open(rsOperSettingProperty, FFilteredList.countBothSides(indexes));
    EnableControls(False);

    commonService.copyProperties(indexes, targetSide, attrs);

    self.MainDrawGrid.Invalidate;
    EnableControls(True);
  finally
    if FState.isInState(sddsCopyingProperties) then begin
      FFileCountProgressHelper.close;
      FState.reset;
    end;
    commonService.Free;
    indexes.Free;
  end;
end;

procedure TfrmSyncDirsDlg.onCheckContentThreadStart;
begin
  Timer.Enabled:= True;
  HeaderDG.Enabled:= False;
  GroupBox1.Enabled:= False;
  MainDrawGrid.Enabled:= False;
  FFileSizeProgressHelper.open(rsDiffComparing, 100);
  FFileCountProgressHelper.close;
  pnlProgress.Visible:= True;
end;

procedure TfrmSyncDirsDlg.onCheckContentThreadFinish;
begin
  Timer.Enabled:= False;
  FState.reset;
  if Assigned(FCheckContentThread) then begin
    FCheckContentThread.FreeOnTerminate:= True;
    FCheckContentThread:= nil;
  end;
  UpdateStatusBar;
  HeaderDG.Enabled:= True;
  TopPanel.Enabled:= True;
  GroupBox1.Enabled:= True;
  MainDrawGrid.Enabled:= True;
  pnlProgress.Visible:= False;
  MainDrawGrid.SetFocus;
end;

procedure TfrmSyncDirsDlg.onCheckContentThreadReapplyFilter;
begin
  FillFoundItemsDG;
end;

procedure TfrmSyncDirsDlg.onCheckContentThreadCountUpdated(
  const totalInc: Integer;
  const equalInc: Integer;
  const notEqInc: Integer);
begin
  Inc( FFilteredCount.total, totalInc );
  Inc( FFilteredCount.equal, equalInc );
  Inc( FFilteredCount.notEqual, notEqInc );
end;

procedure TfrmSyncDirsDlg.checkContentThreadStart;
begin
  if FState.isRunningInState(sddsCheckingContent) then
    Exit;
  FState.enter(sddsCheckingContent);
  FCheckContentThread := TSyncDirsCheckContentThread.Create(FFullTree, self.createFilterFlags, Self);
  FCheckContentThreadTimerCount := 0;
end;

procedure TfrmSyncDirsDlg.checkContentThreadStop;
begin
  if NOT FState.isInState(sddsCheckingContent) then
    Exit;
  FCheckContentThread.Terminate;
  FCheckContentThread.WaitFor;    // onCheckContentThreadFinish() will be called
end;

procedure TfrmSyncDirsDlg.checkContentThreadUpdateStat(
  const doneBytes: Int64;
  const totalBytes: Int64 );
begin
  {$IFDEF LCLCOCOA}
  if totalBytes > 0 then
    FFileSizeProgressHelper.predefinedCaption := Format(rsComparingPercent, [doneBytes*100 div totalBytes])
  else
    FFileSizeProgressHelper.predefinedCaption := Format(rsComparingPercent, [0]);
  {$ENDIF}
  FFileSizeProgressHelper.total := totalBytes;
  FFileSizeProgressHelper.done := doneBytes;

  Inc(FCheckContentThreadTimerCount);
  if (FCheckContentThreadTimerCount mod 5 = 0) then begin
    MainDrawGrid.Invalidate;
    UpdateStatusBar;
  end;
end;

function TfrmSyncDirsDlg.treeBuilderMaskFilt(const f: TFile): Boolean;
begin
  if f.IsDirectory or f.IsLinkToDirectory then begin
    Result:= (FTemplate = nil) or (CheckDirectoryName(FTemplate.FileChecks, f.Name));
  end else begin
    Result:= False;
    if Assigned(FTemplate) then
      Result:= FTemplate.CheckFile(f);
    if NOT Result then
      Result:= ((FMaskList = nil) or FMaskList.Matches(f.Name));
  end;
end;

procedure TfrmSyncDirsDlg.onTreeBuilderUpdateProgress(const percent: Integer);
begin
  StatusBar1.Panels[0].Text:= Format(rsComparingPercent, [percent]);
end;

function TfrmSyncDirsDlg.commonCheckRunning(const processMessages: Boolean): Boolean;
begin
  Result:= FState.isRunning;
  if processMessages then
    Application.ProcessMessages;
end;

procedure TfrmSyncDirsDlg.DeleteSelectedFiles(ALeft, ARight: Boolean);
var
  commonService: TSyncDirsCommonService;
  Message: String;
  indexes: TIntegerList = nil;
  leftCount: Integer;
  rightCount: Integer;
  fullyDone: Boolean;
begin
  commonService:= TSyncDirsCommonService.Create(self, self, FFilteredList, FCmpFileSources);
  try
    indexes:= self.createSelectionIndexes;
    FFilteredList.countLeftRight(indexes, leftCount, rightCount);

    ALeft:= ALeft and (leftCount > 0);
    ARight:= ARight and (rightCount > 0);

    if (ALeft = False) and (ARight = False) then Exit;

    FDeleteStatistics.DoneFiles:= 0;
    FDeleteStatistics.TotalFiles:= 0;

    Message:= EmptyStr;
    if ALeft then begin
      FDeleteStatistics.TotalFiles+= leftCount;
      Message:= Format(rsVarLeftPanel + ': ' + rsMsgDelFlDr, [leftCount]) + LineEnding;
    end;
    if ARight then begin
      FDeleteStatistics.TotalFiles+= rightCount;
      Message+= Format(rsVarRightPanel + ': ' + rsMsgDelFlDr, [rightCount]) + LineEnding;
    end;

    if MessageDlg(Message, mtWarning, [mbYes, mbNo], 0, mbYes) = mrYes then begin
      FState.enter(sddsDeletingFiles);
      FFileSizeProgressHelper.close;
      FFileCountProgressHelper.open(rsOperDeleting, FDeleteStatistics.TotalFiles);
      EnableControls(False);

      fullyDone:= commonService.delete(indexes, ALeft, ARight);
      if fullyDone then
        self.FillFoundItemsDG;

      EnableControls(True);
      if NOT fullyDone then
        btnCompare.Click;
    end;
  finally
    if FState.isInState(sddsDeletingFiles) then begin
      FFileCountProgressHelper.close;
      FState.reset;
    end;
    commonService.Free;
    indexes.Free;
  end;
end;

function TfrmSyncDirsDlg.fileProcessorWithUICopyFiles(
  const sourceFS: IFileSource;
  const targetFS: IFileSource;
  var files: TFiles;
  const targetPath: String): Boolean;
var
  supported: Boolean = False;

  procedure operationHandle( const operation: TFileSourceOperation; const state: TFileSourceOperationState );
  begin
    supported:= True;
    case state of
      fsosStarting: begin
        operation.Elevate:= ElevateAction;
        TFileSourceCopyOperation(operation).SymLinkOption := SymLinkOption;
        TFileSourceCopyOperation(operation).FileExistsOption := FileExistsOption;
        if operation is TFileSystemCopyOperation then
          TFileSystemCopyOperation(operation).Verify := VerifyOption;
        operation.AddUserInterface(FFileSourceOperationMessageBoxesUI);
        FOperation:= operation;
      end;
      fsosStopped: begin
        SymLinkOption := TFileSourceCopyOperation(operation).SymLinkOption;
        FileExistsOption := TFileSourceCopyOperation(operation).FileExistsOption;
        FCopyStatistics.DoneBytes+= TFileSourceCopyOperation(operation).RetrieveStatistics.TotalBytes;
        FFileSizeProgressHelper.update(FCopyStatistics.DoneBytes);
        FOperation:= nil;
      end;
    end;
  end;

begin
  Result:= TSyncDirsUtil.copyFiles(sourceFS, targetFS, files, targetPath, @operationHandle );
  if NOT Result then begin
    if NOT supported then
      MessageDlg(rsMsgErrNotSupported, mtError, [mbOK], 0);
  end;
end;

function TfrmSyncDirsDlg.fileProcessorWithUIDeleteFiles(
  const FileSource: IFileSource;
  var Files: TFiles ): Boolean;
var
  supported: Boolean = False;

  procedure operationHandle( const operation: TFileSourceOperation; const state: TFileSourceOperationState );
  begin
    supported:= True;
    case state of
      fsosStarting: begin
        if (operation is TFileSystemDeleteOperation) then begin
          TFileSystemDeleteOperation(operation).Recycle:= gUseTrash;
        end;
        operation.Elevate:= ElevateAction;
        operation.AddUserInterface(FFileSourceOperationMessageBoxesUI);
        FOperation:= operation;
      end;
      fsosStopped: begin
        FDeleteStatistics.DoneFiles+= TFileSourceDeleteOperation(operation).RetrieveStatistics.TotalFiles;
        FFileCountProgressHelper.update(FDeleteStatistics.DoneFiles);
        FOperation:= nil;
      end;
    end;
  end;
begin
  Result:= TSyncDirsUtil.deleteFiles(FileSource, Files, @operationHandle);
  if NOT Result then begin
    if NOT supported then
      MessageDlg(rsMsgErrNotSupported, mtError, [mbOK], 0);
  end;
end;

function TfrmSyncDirsDlg.fileProcessorWithUICopyProperties(
  const targetFS: IFileSource;
  const sourceFile: TFile;
  const targetFile: TFile;
  const attrs: TCopyAttributesOptions ): Boolean;
var
  errMsg: String;
  askResult: TModalResult;
begin
  Result:= TSyncDirsUtil.copyProperties(targetFS, sourceFile, targetFile, attrs);
  Inc( FFileCountProgressHelper.done );
  if NOT Result then begin
    errMsg:= Format(rsMsgErrSetAttribute, [targetFile.FullPath]);
    askResult:= MessageDlg(errMsg, mtError, [mbIgnore, mbAbort], 0, mbAbort);
    Result:= (askResult = mrIgnore);
  end;
end;

function TfrmSyncDirsDlg.fileProcessorWithUIDeleteFile(
  const FileSource: IFileSource;
  const f: TFile ): Boolean;
var
  files: TFiles;
begin
  files := TFiles.Create(EmptyStr);
  files.OwnsObjects:= False;
  files.Add(f);
  Result:= fileProcessorWithUIDeleteFiles(FileSource, files);
  files.Free;
end;

procedure TfrmSyncDirsDlg.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
                                             const AXProportion, AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  RecalcHeaderCols;
end;

constructor TfrmSyncDirsDlg.Create(AOwner: TComponent; FileView1,
  FileView2: TFileView);
begin
  inherited Create(AOwner);
  FState := TSyncDirsDlgStateHelper.Create;
  FSortService := TSyncDirsSortService.Create;
  FFullTree := TTwoLevelTree.Create;
  FFilteredList := TFlatDirFileList.Create(FFullTree);
  self.leftFS := FileView1.FileSource;
  self.rightFS := FileView2.FileSource;
  self.leftAddress := FileView1.CurrentAddress;
  self.rightAddress := FileView2.CurrentAddress;
  with FileView1 do begin
    edPath1.Text := self.leftAddress + CurrentPath;
{$if lcl_fullversion >= 4990000}
    edPath1.DialogOptionsEx:= [ofShowsFilePackagesSwitch];
{$endif}
  end;
  with FileView2 do begin
    edPath2.Text := self.rightAddress + CurrentPath;
{$if lcl_fullversion >= 4990000}
    edPath2.DialogOptionsEx:= [ofShowsFilePackagesSwitch];
{$endif}
  end;
  RecalcHeaderCols;
  MainDrawGrid.DoubleBuffered := True;
  MainDrawGrid.Font.Bold := True;
  FSortIndex := -1;
  SortIndex := 0;
  MainDrawGrid.RowCount := 0;
  FSelectedFiles[dsLeft]:= FileView1.CloneSelectedFiles;
  FSelectedFiles[dsRight]:= FileView2.CloneSelectedFiles;
  chkOnlySelected.Enabled := ((FSelectedFiles[dsLeft].Count>0) or (FSelectedFiles[dsRight].Count>0)) and
                             (FileView1.FlatView = False) and
                             (FileView2.FlatView = False);
  chkOnlySelected.Checked := chkOnlySelected.Enabled;
  // ---------------------------------------------------------------------------
  chkByContent.Enabled := TSyncDirsUtil.supportsCompareByContent(FFileSources);
  chkAsymmetric.Enabled := fsoDelete in FileView2.FileSource.GetOperationsTypes;
  // ---------------------------------------------------------------------------
  actDeleteLeft.Enabled := fsoDelete in FileView1.FileSource.GetOperationsTypes;
  actDeleteRight.Enabled := fsoDelete in FileView2.FileSource.GetOperationsTypes;
  actDeleteBoth.Enabled := actDeleteLeft.Enabled and actDeleteRight.Enabled;
  // ---------------------------------------------------------------------------
  FFileSourceOperationMessageBoxesUI := TFileSourceOperationMessageBoxesUI.Create;
end;

destructor TfrmSyncDirsDlg.Destroy;
begin
  HotMan.UnRegister(Self);
  FFileSourceOperationMessageBoxesUI.Free;
  FSelectedFiles[dsLeft].Free;
  FSelectedFiles[dsRight].Free;
  FFilteredList.Free;
  FFullTree.Free;
  FSortService.Free;
  FCompareOption.Free;
  FFileSizeProgressHelper.Free;
  FFileCountProgressHelper.Free;
  FState.Free;
  inherited Destroy;
end;

procedure TfrmSyncDirsDlg.CopyToClipboard;
var
  indexes: TIntegerList = nil;
  sl: TStringList = nil;
begin
  try
    indexes:= self.createSelectionIndexes;
    sl:= TSyncDirsUtil.selectionToStringList(FFilteredList, indexes, FCompareOption);
    ClipboardSetText(sl.Text);
  finally
    FreeAndNil(sl);
    FreeAndNil(indexes);
  end;
end;

procedure TfrmSyncDirsDlg.cm_SelectClear(const Params: array of string);
begin
  setSelectionAction(srsDoNothing);
end;

procedure TfrmSyncDirsDlg.cm_SelectDeleteLeft(const Params: array of string);
begin
  setSelectionAction(srsDeleteLeft);
end;

procedure TfrmSyncDirsDlg.cm_SelectDeleteRight(const Params: array of string);
begin
  setSelectionAction(srsDeleteRight);
end;

procedure TfrmSyncDirsDlg.cm_SelectDeleteBoth(const Params: array of string);
begin
  setSelectionAction(srsDeleteBoth);
end;

procedure TfrmSyncDirsDlg.cm_SelectCopyDefault(const Params: array of string);
begin
  setSelectionAction(srsUnknown);
end;

procedure TfrmSyncDirsDlg.cm_SelectCopyReverse(const Params: array of string);
begin
  setSelectionAction(srsNotEq);
end;

procedure TfrmSyncDirsDlg.cm_SelectCopyLeftToRight(const Params: array of string);
begin
  setSelectionAction(srsCopyToRight);
end;

procedure TfrmSyncDirsDlg.cm_SelectCopyRightToLeft(const Params: array of string);
begin
  setSelectionAction(srsCopyToLeft);
end;

procedure TfrmSyncDirsDlg.cm_DeleteLeft(const Params: array of string);
begin
  DeleteSelectedFiles(True, False);
end;

procedure TfrmSyncDirsDlg.cm_DeleteRight(const Params: array of string);
begin
  DeleteSelectedFiles(False, True);
end;

procedure TfrmSyncDirsDlg.cm_DeleteBoth(const Params: array of string);
begin
  DeleteSelectedFiles(True, True);
end;

procedure TfrmSyncDirsDlg.cm_CopyPropertiesLeftToRight(const Params: array of string);
begin
  self.doCopyProperties(dsRight);
end;

procedure TfrmSyncDirsDlg.cm_CopyPropertiesRightToLeft(const Params: array of string);
begin
  self.doCopyProperties(dsLeft);
end;

initialization
  TFormCommands.RegisterCommandsForm(TfrmSyncDirsDlg, HotkeysCategory, @rsHotkeyCategorySyncDirs);

end.

