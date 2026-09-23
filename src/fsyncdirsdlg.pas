{
   Double Commander
   -------------------------------------------------------------------------
   Directories synchronization utility (specially for DC)

   Copyright (C) 2013 Anton Panferov (ast.a_s@mail.ru)
   Copyright (C) 2014-2024 Alexander Koblov (alexx2000@mail.ru)

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
  ExtCtrls, Buttons, ComCtrls, Grids, Menus, ActnList, EditBtn, DCClassesUtf8,
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

  { TfrmSyncDirsDlg }

  TfrmSyncDirsDlg = class(
    TForm,
    IFormCommands,
    ISyncDirsFileProcessorWithUI,
    ISyncDirsCheckContentThreadCallback,
    ISyncDirsTreeBuilderCallback,
    ISyncDirsSynchronizerCallback )

    actDeleteLeft: TAction;
    actDeleteRight: TAction;
    actDeleteBoth: TAction;
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
    lblProgress: TLabel;
    lblProgressDelete: TLabel;
    MainDrawGrid: TDrawGrid;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    LeftPanel1: TPanel;
    LeftPanel2: TPanel;
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
    pnlCopyProgress: TPanel;
    pnlDeleteProgress: TPanel;
    pmGridMenu: TPopupMenu;
    ProgressBar: TKASProgressBar;
    ProgressBarDelete: TKASProgressBar;
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
    procedure MainDrawGridMouseDown(Sender: TObject; Button: TMouseButton;
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
    FCancel: Boolean;
    FScanning: Boolean;
    FService: TSyncDirsService;
    FFullTree: TTwoLevelTree;
    FFilteredList: TFlatDirFileList;
    FSortIndex: Integer;
    FSortDesc: Boolean;
    FCompareOption: TSyncDirsCompareOption;
    FMaskList: TMaskList;
    FTemplate: TSearchTemplate;
    FSelectedItems: TStringListEx;
    FFileSourceL, FFileSourceR: IFileSource;
    FCmpFileSourceL, FCmpFileSourceR: IFileSource;
    FCmpFilePathL, FCmpFilePathR: string;
    FAddressL, FAddressR: string;
    hCols: array [0..6] of record Left, Width: Integer end;
    Ftotal, Fequal, Fnoneq, FuniqueL, FuniqueR: Integer;
    FOperation: TFileSourceOperation;
    FileExistsOption: TFileSourceOperationOptionFileExists;
    SymLinkOption: TFileSourceOperationOptionSymLink;
    FCopyStatistics: TFileSourceCopyOperationStatistics;
    FDeleteStatistics: TFileSourceDeleteOperationStatistics;
    FFileSourceOperationMessageBoxesUI: TFileSourceOperationMessageBoxesUI;

    function createCompareOption: TSyncDirsCompareOption;
    function createFilterFlags: TFilterFlags;
    function createSelectionIndexes: TIntegerList;

    procedure toggleSelectionAction;
    procedure setSelectionAction(const newAction: TSyncRecState);

    procedure Compare;
    procedure FillFoundItemsDG;
    procedure InitVisibleItems;
    procedure RecalcHeaderCols;
    procedure ScanDirs;
    procedure SetSortIndex(AValue: Integer);
    procedure UpdateStatusBar;
    procedure EnableControls(AEnabled: Boolean);
    procedure DeleteFiles(ALeft, ARight: Boolean);
    procedure UpdateList(ALeft, ARight: TFiles; ARemoveLeft, ARemoveRight: Boolean);
    procedure SetProgressBytes(AProgressBar: TKASProgressBar; CurrentBytes: Int64; TotalBytes: Int64);
    procedure SetProgressFiles(AProgressBar: TKASProgressBar; CurrentFiles: Int64; TotalFiles: Int64);


  private
    function fileProcessorWithUICopyFiles(
      const src: IFileSource;
      const dst: IFileSource;
      const fs: TFiles;
      const Dest: String): Boolean;
    function fileProcessorWithUIDeleteFiles(
      const FileSource: IFileSource;
      var Files: TFiles): Boolean;
    function fileProcessorWithUIDeleteFile(
      const FileSource: IFileSource;
      const f: TFile): Boolean;

  private
    FCheckContentThread: TSyncDirsCheckContentThread;
    FCheckContentThreadTimerCount: Integer;
    FCheckContentThreadComparing: Boolean;
  private
    procedure onCheckContentThreadStart;
    procedure onCheckContentThreadFinish;
    procedure onCheckContentThreadReapplyFilter;
    procedure onCheckContentThreadCountUpdated( const equalInc: Integer; const notEqInc: Integer );

    procedure checkContentThreadStart;
    procedure checkContentThreadStop;
    procedure checkContentThreadUpdateGrid;
    procedure checkContentThreadSetProgressBytes(const AProgressBar: TKASProgressBar; const CurrentBytes: Int64; const TotalBytes: Int64);

  private
    function treeBuilderCheckRunning( const processMessages: Boolean ): Boolean;
    function treeBuilderMaskFilt( const f: TFile ): Boolean;
    function treeBuilderSelectedFilt( const filename: String ): Boolean;
    procedure onTreeBuilderUpdateProgress( const percent: Integer );

  private
    function synchronizerCheckRunning: Boolean;

  private
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
  fMain, uDebug, fDiffer, fSyncDirsPerformDlg, uGlobs, LCLType, LazUTF8, LazFileUtils,
  uFileSystemFileSource, DCDateTimeUtils,
  uDCUtils, uFileSourceOperationTypes, uShowForm, uAdministrator,
  uOSUtils, uLng, Math, uClipboard, fMaskInputDlg,
  LCLVersion, uTypes, uFileSystemDeleteOperation, uFindFiles,
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
    if NOT TSyncDirsFileUtil.supportsSyncDirs(leftFS,rightFS) then
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
  else if FCheckContentThreadComparing then
    checkContentThreadStop
  else begin
    pnlProgress.Hide;
  end;
end;

procedure TfrmSyncDirsDlg.btnCompareClick(Sender: TObject);
begin
  if not IsMaskSearchTemplate(cbExtFilter.Text) then
    InsertFirstItem(Trim(cbExtFilter.Text), cbExtFilter);
  StatusBar1.Panels[0].Text := Format(rsComparingPercent, [0]);
  checkContentThreadStop;
  Compare;
end;

procedure TfrmSyncDirsDlg.btnSynchronizeClick(Sender: TObject);

  procedure removeAsymmetricRightEmptyDirs;
    function isEmptyDir(const fs: IFileSource; const path: String): Boolean;
    var
      files: TFiles;
      f: TFile;
      i: Integer;
    begin
      Result:= False;
      files:= fs.GetFiles(path);
      try
        for i:= 0 to files.Count-1 do begin
          f:= files[i];
          if (f.Name<>'.') and (f.Name<>'..') then
            Exit;
        end;
        Result:= True;
      finally
        files.Free;
      end;
    end;

  var
    i: Integer;
    syncRec: TFileSyncRec;
  begin
    if NOT (cfAsymmetric in FCompareOption.flags) then
      Exit;
    if NOT (cfEmptyDirs in FCompareOption.flags) then
      Exit;

    for i:= FFilteredList.Count-1 downto 0 do begin
      syncRec:= FFilteredList.fileSyncRec(i);
      if NOT syncRec.isDir then
        continue;
      if TDirSyncRec(syncRec).noFile then
        continue;
      if NOT Assigned(syncRec.rightFile) then
        continue;
      if Assigned(syncRec.leftFile) then
        continue;
      if isEmptyDir(FCmpFileSourceR,syncRec.rightFile.FullPath) then
        fileProcessorWithUIDeleteFile(FCmpFileSourceR, syncRec.rightFile);
    end;
  end;

var
  synchronizer: TSyncDirsSynchronizer;
  syncCount: TSyncDirsSyncCount;
  syncFlags: TSyncDirsSyncFlags;
begin
  synchronizer:= TSyncDirsSynchronizer.Create( self, self, FFilteredList );
  synchronizer.leftFS:= FCmpFileSourceL;
  synchronizer.rightFS:= FCmpFileSourceR;
  synchronizer.leftBasePath:= FCmpFilePathL;
  synchronizer.rightBasePath:= FCmpFilePathR;
  syncCount:= synchronizer.count;
  syncFlags:= [];

  FCopyStatistics.DoneBytes:= 0;
  FDeleteStatistics.DoneFiles:= 0;
  FCopyStatistics.TotalBytes:= syncCount.copySize;
  FDeleteStatistics.TotalFiles:= syncCount.deleteCount;

  with TfrmSyncDirsPerformDlg.Create(Self) do
  try
    edLeftPath.Text := FCmpFileSourceL.CurrentAddress + FCmpFilePathL;
    edRightPath.Text := FCmpFileSourceR.CurrentAddress + FCmpFilePathR;
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
    if ShowModal = mrOk then
    begin
      EnableControls(False);
      SymLinkOption:= fsooslNone;
      if chkConfirmOverwrites.Checked then
        FileExistsOption := fsoofeNone
      else begin
        FileExistsOption := fsoofeOverwrite;
      end;

      if chkRightToLeft.Checked then
        Include( syncFlags, sfCopyToLeft );
      if chkLeftToRight.Checked then
        Include( syncFlags, sfCopyToRight );
      if chkDeleteLeft.Checked then
        Include( syncFlags, sfDeleteLeft );
      if chkDeleteRight.Checked then
        Include( syncFlags, sfDeleteRight );

      lblProgress.Caption := rsOperCopying;
      lblProgressDelete.Caption := rsOperDeleting;
      ProgressBar.Position:=0;
      ProgressBarDelete.Position:=0;
      pnlCopyProgress.Visible:= (sfCopyToLeft in syncFlags) or (sfCopyToRight in syncFlags);
      pnlDeleteProgress.Visible:= (sfDeleteLeft in syncFlags) or (sfDeleteRight in syncFlags);

      synchronizer.sync( syncFlags );
      removeAsymmetricRightEmptyDirs;

      EnableControls(True);
      btnCompare.Click;
    end;
  finally
    synchronizer.Free;
    Free;
  end;
end;

procedure TfrmSyncDirsDlg.edPath1AcceptDirectory(Sender: TObject;
  var Value: String);
begin
  if Sender = edPath1 then
  begin
    FFileSourceL := TFileSystemFileSource.GetFileSource;
    FAddressL := '';
  end
  else if Sender = edPath2 then
  begin
    FFileSourceR := TFileSystemFileSource.GetFileSource;
    FAddressR := '';
  end;
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

procedure TfrmSyncDirsDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
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
  else if FScanning then
  begin
    FCancel := True;
    CanClose := False;
  end
  else if FCheckContentThreadComparing then
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

  lblProgress.Caption    := rsOperCopying;
  lblProgressDelete.Caption   := rsOperDeleting;
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
  ProgressBar.Width:= ClientWidth div 3;
  ProgressBarDelete.Width:= ProgressBar.Width;
end;

procedure TfrmSyncDirsDlg.MainDrawGridDblClick(Sender: TObject);
var
  r, x: Integer;
  sr: TFileSyncRec;
begin
  r := MainDrawGrid.Row;
  if (r < 0) or (r >= FFilteredList.Count) then Exit;
  x := MainDrawGrid.ScreenToClient(Mouse.CursorPos).X;
  if (x > hCols[3].Left) and (x < hCols[3].Left + hCols[3].Width) then Exit;
  sr := FFilteredList.fileSyncRec(r);
  if sr.isDir
  or not Assigned(sr.rightFile) or not Assigned(sr.leftFile) or (sr.state = srsEqual)
  then
    Exit;
  PrepareToolData(FFileSourceL, sr.leftFile, FFileSourceR, sr.rightFile, @ShowDifferByGlobList);
end;

procedure TfrmSyncDirsDlg.MainDrawGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
const
  LEFT_FILE_INDENTATION = 8;
var
  r: TFileSyncRec;
  x: Integer;
  s: string;
begin
  if (FFilteredList = nil) or (aRow >= FFilteredList.Count) then Exit;
  with MainDrawGrid.Canvas do
  begin
    r := FFilteredList.fileSyncRec(aRow);
    if r.isDir then
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
        case r.state of
        srsNotEq:       Font.Color := UnknownColor;
        srsCopyToLeft:    Font.Color := RightColor;
        srsCopyToRight:   Font.Color := LeftColor;
        srsDeleteLeft:  Font.Color := LeftColor;
        srsDeleteRight: Font.Color := RightColor;
        else Font.Color := clWindowText;
        end;
      end;
      if Assigned(r.leftFile) then
      begin
        with hCols[0] do
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            Left + 2 + LEFT_FILE_INDENTATION, aRect.Top + 2, FFilteredList.path(aRow));
        s := IntToStrTS(r.leftFile.Size);
        with hCols[1] do begin
          x := Left + Width - 8 - TextWidth(s);
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            x, aRect.Top + 2, s);
        end;
        s := FormatDateTime(gDateTimeFormatSync, r.leftFile.ModificationTime);
        with hCols[2] do
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            Left + 2, aRect.Top + 2, s)
      end;
      if Assigned(r.rightFile) then
      begin
        TextOut(hCols[6].Left + 2, aRect.Top + 2, FFilteredList.path(aRow));
        s := IntToStrTS(r.rightFile.Size);
        with hCols[5] do begin
          x := Left + Width - 8 - TextWidth(s);
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            x, aRect.Top + 2, s);
        end;
        s := FormatDateTime(gDateTimeFormatSync, r.rightFile.ModificationTime);
        with hCols[4] do
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            Left + 2, aRect.Top + 2, s)
      end;
    end;
    if NOT r.isDir or (r.state<>srsDoNothing) then begin
      ImageList1.Draw(MainDrawGrid.Canvas,
        hCols[3].Left + (hCols[3].Width - ImageList1.Width) div 2 - 2,
        (aRect.Top + aRect.Bottom - ImageList1.Height - 1) div 2, Ord(r.action));
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
      toggleSelectionAction;
    VK_A:
    begin
      if (Shift = [ssModifier]) then
      begin
        ASelection.Top:= 0;
        ASelection.Left:= 0;
        ASelection.Right:= MainDrawGrid.ColCount - 1;
        ASelection.Bottom:= MainDrawGrid.RowCount - 1;
        MainDrawGrid.Selection:= ASelection;
      end;
    end;
    VK_C:
      if (Shift = [ssModifier]) then
      begin
        CopyToClipboard;
      end;
    VK_INSERT:
      if (Shift = [ssModifier]) then
      begin
        CopyToClipboard;
      end;
  end;
end;

procedure TfrmSyncDirsDlg.MainDrawGridMouseDown(Sender: TObject;
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

  MainDrawGrid.Row:= r;
  MainDrawGrid.ClearSelections;
  toggleSelectionAction;
end;

procedure TfrmSyncDirsDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    if FScanning then
      FCancel := True
    else if FCheckContentThreadComparing then
      checkContentThreadStop
    else
      Close;
  end;
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
  FillFoundItemsDG
end;

procedure TfrmSyncDirsDlg.MenuItemViewClick(Sender: TObject);
var
  r: Integer;
  f: TFile = nil;
  sr: TFileSyncRec;
begin
  r := MainDrawGrid.Row;
  if (r < 0) or (r >= FFilteredList.Count) then Exit;
  sr := FFilteredList.fileSyncRec(r);
  if NOT sr.isDir then
  begin
    if Sender = MenuItemViewLeft then
      f := sr.leftFile
    else if Sender = MenuItemViewRight then begin
      f := sr.rightFile;
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

begin
  miSelectDeleteLeft.Visible := not (cfAsymmetric in FCompareOption.flags);
  miSelectDeleteBoth.Visible := not (cfAsymmetric in FCompareOption.flags);
  calcSelection;
end;

procedure TfrmSyncDirsDlg.TimerTimer(Sender: TObject);
var
  CopyStatistics: TFileSourceCopyOperationStatistics;
  DeleteStatistics: TFileSourceDeleteOperationStatistics;
begin
  if Assigned(FOperation) then
  begin
    if (FOperation is TFileSourceCopyOperation) then
    begin
      CopyStatistics:= TFileSourceCopyOperation(FOperation).RetrieveStatistics;
      SetProgressBytes(ProgressBar, FCopyStatistics.DoneBytes +
                       CopyStatistics.DoneBytes, FCopyStatistics.TotalBytes);
    end
    else if (FOperation is TFileSourceDeleteOperation) then
    begin
      DeleteStatistics:= TFileSourceDeleteOperation(FOperation).RetrieveStatistics;
      SetProgressFiles(ProgressBarDelete, FDeleteStatistics.DoneFiles +
                       DeleteStatistics.DoneFiles, FDeleteStatistics.TotalFiles);
    end;
  end
  else if Assigned(FCheckContentThread) then
  begin
    Inc(FCheckContentThreadTimerCount);
    CopyStatistics:= FCheckContentThread.RetrieveStatistics;
    if (FCheckContentThreadTimerCount mod 5 = 0) then checkContentThreadUpdateGrid;
    checkContentThreadSetProgressBytes(ProgressBar, CopyStatistics.DoneBytes, CopyStatistics.TotalBytes);
  end;
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

  if (FFileSourceL.IsClass(TFileSystemFileSource)) and (FFileSourceR.IsClass(TFileSystemFileSource)) then begin
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
  FService.sortIndex := AValue;
  if AValue = FSortIndex then
  begin
    s := HeaderDG.Columns[AValue].Title.Caption;
    UTF8Delete(s, 1, 1);
    FSortDesc := not FSortDesc;
    FService.sortDesc := FSortDesc;
    s := getSortIndicator() + s;
    HeaderDG.Columns[AValue].Title.Caption := s;
    FService.sortTree(FFullTree);
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
    FService.sortDesc := FSortDesc;
    with HeaderDG.Columns[FSortIndex].Title do
      Caption := getSortIndicator() + Caption;
    FService.sortTree(FFullTree);
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
    TopPanel.Enabled := not FCheckContentThreadComparing;
  end;
end;

procedure TfrmSyncDirsDlg.FillFoundItemsDG;

  procedure CalcStat;
  var
    i: Integer;
    r: TFileSyncRec;
  begin
    Ftotal := 0;
    Fequal := 0;
    Fnoneq := 0;
    FuniqueL := 0;
    FuniqueR := 0;
    for i := 0 to FFilteredList.Count - 1 do
    begin
      r := FFilteredList.fileSyncRec(i);
      if NOT r.isDir then
      begin
        Inc(Ftotal);
        if Assigned(r.leftFile) and not Assigned(r.rightFile) then Inc(FuniqueL) else
        if Assigned(r.rightFile) and not Assigned(r.leftFile) then Inc(FuniqueR);
        if r.state = srsEqual then Inc(Fequal) else
        if r.state = srsNotEq then Inc(Fnoneq) else
        if Assigned(r.leftFile) and Assigned(r.rightFile) then Inc(Fnoneq);
      end;
    end;
  end;

begin
  InitVisibleItems;
  MainDrawGrid.ColCount := 1;
  MainDrawGrid.RowCount := FFilteredList.Count;
  MainDrawGrid.Invalidate;
  CalcStat;
  UpdateStatusBar;
  if FFilteredList.Count > 0 then
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
  FFullTree.filterFlatListWithFlags(FFilteredList, self.createFilterFlags);
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
  BaseDirL: String;
  BaseDirR: String;
begin
  FScanning := True;
  try
    FCancel := False;
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
    FCmpFileSourceL := FFileSourceL;
    FCmpFileSourceR := FFileSourceR;
    BaseDirL := AppendPathDelim(edPath1.Text);
    if (FAddressL <> '') and (Copy(BaseDirL, 1, Length(FAddressL)) = FAddressL) then
      Delete(BaseDirL, 1, Length(FAddressL));
    BaseDirR := AppendPathDelim(edPath2.Text);
    if (FAddressR <> '') and (Copy(BaseDirR, 1, Length(FAddressR)) = FAddressR) then
      Delete(BaseDirR, 1, Length(FAddressR));
    FCmpFilePathL := BaseDirL;
    FCmpFilePathR := BaseDirR;

    builder:= TSyncDirsTreeBuilder.Create( self, FService, FCompareOption );
    builder.baseDirL:= BaseDirL;
    builder.baseDirR:= BaseDirR;
    builder.fileSourceL:= FFileSourceL;
    builder.fileSourceR:= FFileSourceR;
    builder.build( FFullTree );

    FillFoundItemsDG;
    if FCancel then
      Exit;
    if (FFullTree.Count > 0) and (cfByContent in FCompareOption.flags) then
      checkContentThreadStart;
  finally
    FreeAndNil(builder);
    FreeAndNil(FMaskList);
    FTemplate := nil;
    FScanning := False;
  end;
end;

procedure TfrmSyncDirsDlg.UpdateStatusBar;
var s: string;
begin
  s := Format(rsFilesFound, [Ftotal, Fequal, Fnoneq, FuniqueL, FuniqueR]);
  if Assigned(FCheckContentThread)
  and not FCheckContentThread.Done then
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

procedure TfrmSyncDirsDlg.onCheckContentThreadStart;
begin
  Timer.Enabled:= True;
  HeaderDG.Enabled:= False;
  GroupBox1.Enabled:= False;
  MainDrawGrid.Enabled:= False;
  pnlCopyProgress.Visible:= True;
  ProgressBar.SetProgress(0, 100);
  pnlDeleteProgress.Visible:= False;
  lblProgress.Caption:= rsDiffComparing;
  pnlProgress.Visible:= True;
end;

procedure TfrmSyncDirsDlg.onCheckContentThreadFinish;
begin
  FCheckContentThreadComparing:= False;
  Timer.Enabled:= False;
  HeaderDG.Enabled:= True;
  TopPanel.Enabled:= True;
  GroupBox1.Enabled:= True;
  MainDrawGrid.Enabled:= True;
  pnlProgress.Visible:= False;
end;

procedure TfrmSyncDirsDlg.onCheckContentThreadReapplyFilter;
begin
  FillFoundItemsDG;
  UpdateStatusBar;
end;

procedure TfrmSyncDirsDlg.onCheckContentThreadCountUpdated(
  const equalInc: Integer;
  const notEqInc: Integer);
begin
  Inc( Fequal, equalInc );
  Inc( Fnoneq, notEqInc );
end;

procedure TfrmSyncDirsDlg.checkContentThreadStart;
begin
  if Assigned(FCheckContentThread) then
    Exit;
  FCheckContentThread := TSyncDirsCheckContentThread.Create(FFullTree, Self);
  FCheckContentThreadTimerCount := 0;
  FCheckContentThreadComparing := True;
end;

procedure TfrmSyncDirsDlg.checkContentThreadStop;
begin
  if Assigned(FCheckContentThread) then
  begin
    with FCheckContentThread do
    begin
      Terminate;
      WaitFor;
    end;
    FreeAndNil(FCheckContentThread);
  end;
end;

procedure TfrmSyncDirsDlg.checkContentThreadUpdateGrid;
begin
  MainDrawGrid.Invalidate;
  UpdateStatusBar;
end;

procedure TfrmSyncDirsDlg.checkContentThreadSetProgressBytes(
  const AProgressBar: TKASProgressBar;
  const CurrentBytes: Int64;
  const TotalBytes: Int64 );
var
  BarText : String;
  CaptionText : String;
begin
  BarText := cnvFormatFileSize(CurrentBytes, uoscOperation) + '/' + cnvFormatFileSize(TotalBytes, uoscOperation);
  AProgressBar.SetProgress(CurrentBytes, TotalBytes, BarText );

  {$IFDEF LCLCOCOA}
  if TotalBytes > 0 then
    CaptionText := Format(rsComparingPercent, [CurrentBytes*100 div TotalBytes])
  else
    CaptionText := Format(rsComparingPercent, [0]);
  lblProgress.Caption := CaptionText;
  {$ENDIF}
end;

function TfrmSyncDirsDlg.treeBuilderCheckRunning(const processMessages: Boolean): Boolean;
begin
  if processMessages then
    Application.ProcessMessages;
  Result:= NOT FCancel;
end;

function TfrmSyncDirsDlg.treeBuilderMaskFilt(const f: TFile): Boolean;
begin
  if f.IsDirectory or f.IsLinkToDirectory then begin
    Result:= (FTemplate = nil) or (CheckDirectoryName(FTemplate.FileChecks, f.Name));
  end else begin
    Result:= (FTemplate = nil) or FTemplate.CheckFile(f);
    if NOT Result then
      Result:= ((FMaskList = nil) or FMaskList.Matches(f.Name));
  end;
end;

function TfrmSyncDirsDlg.treeBuilderSelectedFilt(const filename: String): Boolean;
begin
  Result:= FSelectedItems.IndexOf(filename) >= 0;
end;

procedure TfrmSyncDirsDlg.onTreeBuilderUpdateProgress(const percent: Integer);
begin
  StatusBar1.Panels[0].Text:= Format(rsComparingPercent, [percent]);
end;

function TfrmSyncDirsDlg.synchronizerCheckRunning: Boolean;
begin
  Result:= pnlProgress.Visible;
end;

procedure TfrmSyncDirsDlg.DeleteFiles(ALeft, ARight: Boolean);

  procedure countSelectedDeletableItems(var leftCount: Integer; var rightCount: Integer);
  var
    i: Integer;
    rec: TFileSyncRec;
  begin
    leftCount:= 0;
    rightCount:= 0;
    for i:= 0 to FFilteredList.Count-1 do begin
      if NOT MainDrawGrid.IsCellSelected[0,i] then
        continue;
      rec:= FFilteredList.fileSyncRec(i);
      if rec.isDir and NOT (cfEmptyDirs in FCompareOption.flags) then
        continue;
      if Assigned(rec.leftFile) then
        Inc( leftCount );
      if Assigned(rec.rightFile) then
        Inc( rightCount );
    end;
  end;

var
  Message: String;
  ALeftList: TFiles = nil;
  ARightList: TFiles = nil;
  leftCount: Integer;
  rightCount: Integer;

begin
  try
    Message:= EmptyStr;
    countSelectedDeletableItems( leftCount, rightCount );

    ALeft:= ALeft and (leftCount > 0);
    ARight:= ARight and (rightCount > 0);

    if (ALeft = False) and (ARight = False) then Exit;

    FDeleteStatistics.DoneFiles:= 0;
    FDeleteStatistics.TotalFiles:= 0;

    if ALeft then
    begin
      FDeleteStatistics.TotalFiles+= leftCount;
      Message:= Format(rsVarLeftPanel + ': ' + rsMsgDelFlDr, [leftCount]) + LineEnding;
    end;

    if ARight then
    begin
      FDeleteStatistics.TotalFiles+= rightCount;
      Message+= Format(rsVarRightPanel + ': ' + rsMsgDelFlDr, [rightCount]) + LineEnding;
    end;

    if MessageDlg(Message, mtWarning, [mbYes, mbNo], 0, mbYes) = mrYes then
    begin
      EnableControls(False);
      pnlCopyProgress.Visible:= False;
      pnlDeleteProgress.Visible:= True;

      if ALeft then
        ALeftList:= TFiles.Create(EmptyStr);
      if ARight then
        ARightList:= TFiles.Create(EmptyStr);
      UpdateList(ALeftList, ARightList, ALeft, ARight);

      if ALeft then fileProcessorWithUIDeleteFiles(FCmpFileSourceL, ALeftList);
      if ARight then fileProcessorWithUIDeleteFiles(FCmpFileSourceR, ARightList);
      EnableControls(True);
    end;
  finally
    ALeftList.Free;
    ARightList.Free;
  end;
end;

function TfrmSyncDirsDlg.fileProcessorWithUICopyFiles(
  const src: IFileSource;
  const dst: IFileSource;
  const fs: TFiles;
  const Dest: String): Boolean;

  procedure operationHandle( const operation: TFileSourceOperation; const state: TFileSourceOperationState );
  begin
    case state of
      fsosStarting: begin
        operation.Elevate:= ElevateAction;
        TFileSourceCopyOperation(operation).SymLinkOption := SymLinkOption;
        TFileSourceCopyOperation(operation).FileExistsOption := FileExistsOption;
        operation.AddUserInterface(FFileSourceOperationMessageBoxesUI);
        FOperation:= operation;
      end;
      fsosStopped: begin
        SymLinkOption := TFileSourceCopyOperation(operation).SymLinkOption;
        FileExistsOption := TFileSourceCopyOperation(operation).FileExistsOption;
        FCopyStatistics.DoneBytes+= TFileSourceCopyOperation(operation).RetrieveStatistics.TotalBytes;
        SetProgressBytes(ProgressBar, FCopyStatistics.DoneBytes, FCopyStatistics.TotalBytes);
        FOperation:= nil;
      end;
    end;
  end;

begin
  Result:= TSyncDirsFileUtil.copyFiles(src, dst, fs, Dest, @operationHandle );
  if NOT Result then
    MessageDlg(rsMsgErrNotSupported, mtError, [mbOK], 0);
end;

function TfrmSyncDirsDlg.fileProcessorWithUIDeleteFiles(
  const FileSource: IFileSource;
  var Files: TFiles ): Boolean;

  procedure operationHandle( const operation: TFileSourceOperation; const state: TFileSourceOperationState );
  begin
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
        SetProgressFiles(ProgressBarDelete, FDeleteStatistics.DoneFiles, FDeleteStatistics.TotalFiles);
        FOperation:= nil;
      end;
    end;
  end;
begin
  Result:= TSyncDirsFileUtil.deleteFiles(FileSource, Files, @operationHandle);
  if NOT Result then
    MessageDlg(rsMsgErrNotSupported, mtError, [mbOK], 0);
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

{
  when deleting an item in FilterList, FullTree will be synchronized
  the change via marking rather than actual deletion.

  if an item is deleted from FilteredList during the process,
  the SyncRec.state of that item will be marked as srsDeleted.

  since FilteredList and FullTree share the SyncRec, accessing
  the SyncRec.state of the item via FullTree also yields srcDeleted.

  it eliminates the need to actually delete these items from FullTree.
}
procedure TfrmSyncDirsDlg.UpdateList(ALeft, ARight: TFiles; ARemoveLeft, ARemoveRight: Boolean);

  procedure doRemoveItem(const index: Integer);
  var
    rec: TFileSyncRec;
  begin
    rec:= FFilteredList.fileSyncRec(index);

    if ARemoveLeft and rec.isDeletable(True) then begin
      ALeft.Add(rec.leftFile.Clone);
      FFilteredList.removeLeft( index );
    end;

    if ARemoveRight and rec.isDeletable(False) then begin
      ARight.Add(rec.rightFile.Clone);
      FFilteredList.removeRight( index );
    end;

    if Assigned(rec.leftFile) or Assigned(rec.rightFile) then
      rec.updateState
    else begin
      // don't call MainDrawGrid.DeleteRow() here, it may cause MainDrawGrid.Row changed
      // then cause MainDrawGrid.Selection and MainDrawGrid.IsCellSelected() changed
      FFilteredList.FullyDelete(index);
    end;
  end;

  procedure processMultiSelection;
  var
    i: Integer;
  begin
    for i:= FFilteredList.Count-1 downto 0 do begin
      if MainDrawGrid.IsCellSelected[0,i] then
        doRemoveItem( i );
    end;
  end;

begin
  if (ARemoveLeft=False) and (ARemoveRight=False) then
    Exit;

  MainDrawGrid.BeginUpdate;
  try
    if MainDrawGrid.HasMultiSelection or (MainDrawGrid.Selection.Height>0) then begin
      processMultiSelection;
    end else begin
      doRemoveItem(MainDrawGrid.Row);
    end;
    self.InitVisibleItems;
  finally
    MainDrawGrid.RowCount := FFilteredList.Count;
    MainDrawGrid.EndUpdate;
  end;
end;

procedure TfrmSyncDirsDlg.SetProgressBytes(AProgressBar: TKASProgressBar;
  CurrentBytes: Int64; TotalBytes: Int64);
var
  BarText : String;
  CaptionText : String;
begin
  BarText := cnvFormatFileSize(CurrentBytes, uoscOperation) + '/' + cnvFormatFileSize(TotalBytes, uoscOperation);
  AProgressBar.SetProgress(CurrentBytes, TotalBytes, BarText );

  {$IFDEF LCLCOCOA}
  if TotalBytes > 0 then
    CaptionText := rsOperCopying + ': ' + BarText + ' (' + FloatToStrF((CurrentBytes / TotalBytes) * 100, ffFixed, 0, 0) + '%)'
  else
    CaptionText := rsOperCopying;
  lblProgress.Caption := CaptionText;
  {$ENDIF}
end;

procedure TfrmSyncDirsDlg.SetProgressFiles(AProgressBar: TKASProgressBar;
  CurrentFiles: Int64; TotalFiles: Int64);
var
  BarText : String;
  CaptionText : String;
begin
  BarText := IntToStrTS(CurrentFiles) + '/' + IntToStrTS(TotalFiles);
  AProgressBar.SetProgress(CurrentFiles, TotalFiles, BarText );

  {$IFDEF LCLCOCOA}
  if TotalFiles > 0 then
    CaptionText := rsOperDeleting + ': ' + BarText + ' (' + FloatToStrF((CurrentFiles / TotalFiles) * 100, ffFixed, 0, 0) + '%)'
  else
    CaptionText := rsOperDeleting;
  lblProgressDelete.Caption := CaptionText;
  {$ENDIF}
end;

procedure TfrmSyncDirsDlg.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
                                             const AXProportion, AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  RecalcHeaderCols;
end;

constructor TfrmSyncDirsDlg.Create(AOwner: TComponent; FileView1,
  FileView2: TFileView);
var
  Index: Integer;
  AFiles: TFiles;
begin
  inherited Create(AOwner);
  FService := TSyncDirsService.Create;
  FFullTree := TTwoLevelTree.Create;
  FFilteredList := TFlatDirFileList.Create;
  FFileSourceL := FileView1.FileSource;
  FFileSourceR := FileView2.FileSource;
  FAddressL := FileView1.CurrentAddress;
  FAddressR := FileView2.CurrentAddress;
  with FileView1 do begin
    edPath1.Text := FAddressL + CurrentPath;
{$if lcl_fullversion >= 4990000}
    edPath1.DialogOptionsEx:= [ofShowsFilePackagesSwitch];
{$endif}
  end;
  with FileView2 do begin
    edPath2.Text := FAddressR + CurrentPath;
{$if lcl_fullversion >= 4990000}
    edPath2.DialogOptionsEx:= [ofShowsFilePackagesSwitch];
{$endif}
  end;
  RecalcHeaderCols;
  MainDrawGrid.DoubleBuffered := True;
  MainDrawGrid.Font.Bold := True;
  FSortIndex := -1;
  SortIndex := 0;
  FScanning := False;
  MainDrawGrid.RowCount := 0;
  // ---------------------------------------------------------------------------
  FSelectedItems := TStringListEx.Create;
  FSelectedItems.Sorted := True;
  FSelectedItems.Duplicates := dupIgnore;
  FSelectedItems.CaseSensitive := FileNameCaseSensitive;
  // Get selected items from active panel
  AFiles := FileView1.CloneSelectedFiles;
  for Index := 0 to AFiles.Count - 1 do
  begin
    FSelectedItems.Add(AFiles[Index].Name);
  end;
  AFiles.Free;
  // Get selected items from passive panel
  AFiles := FileView2.CloneSelectedFiles;
  for Index := 0 to AFiles.Count - 1 do
  begin
    FSelectedItems.Add(AFiles[Index].Name);
  end;
  AFiles.Free;
  // ---------------------------------------------------------------------------
  chkOnlySelected.Enabled := (FSelectedItems.Count > 0) and
                             (FileView1.FlatView = False) and
                             (FileView2.FlatView = False);
  chkOnlySelected.Checked := chkOnlySelected.Enabled;
  // ---------------------------------------------------------------------------
  chkByContent.Enabled := FFileSourceL.IsClass(TFileSystemFileSource) and
                          FFileSourceR.IsClass(TFileSystemFileSource);
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
  FFilteredList.Free;
  FSelectedItems.Free;
  FFullTree.Free;
  FService.Free;
  FCompareOption.Free;
  inherited Destroy;
end;

procedure TfrmSyncDirsDlg.CopyToClipboard;
var
  indexes: TIntegerList = nil;
  sl: TStringList = nil;
begin
  try
    indexes:= self.createSelectionIndexes;
    sl:= FService.selectionToStringList(FFilteredList, indexes, FCompareOption);
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
  DeleteFiles(True, False);
end;

procedure TfrmSyncDirsDlg.cm_DeleteRight(const Params: array of string);
begin
  DeleteFiles(False, True);
end;

procedure TfrmSyncDirsDlg.cm_DeleteBoth(const Params: array of string);
begin
  DeleteFiles(True, True);
end;

initialization
  TFormCommands.RegisterCommandsForm(TfrmSyncDirsDlg, HotkeysCategory, @rsHotkeyCategorySyncDirs);

end.

