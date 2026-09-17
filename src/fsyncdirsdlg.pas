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

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ComCtrls, Grids, Menus, ActnList, EditBtn, DCClassesUtf8,
  uFileView, uFileSource, uFileSourceCopyOperation, uFile, uFileSourceOperation,
  uFileSourceOperationMessageBoxesUI, uFormCommands, uHotkeyManager, uClassesEx,
  uFileSourceDeleteOperation, KASProgressBar,
  uSyncDirsModel;

const
  HotkeysCategory = 'Synchronize Directories';

const
  SYNC_REC_STATE_SYMBOL: array[TSyncRecState] of String = (
    '?',
    '=',
    '!=',
    '<-',
    '->',
    'X_',
    '_X',
    'XX',
    '',

    'ERR(NextAction)',
    'ERR(NoAction)',
    'ERR(DEL)'
  );

type
  { TDrawGrid }

  TDrawGrid = class(Grids.TDrawGrid)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  { TfrmSyncDirsDlg }

  TfrmSyncDirsDlg = class(TForm, IFormCommands)
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
    FComparing: Boolean;
    FFullTree: TTwoLevelTree;
    FFilteredList: TFlatDirFileList;
    FSortIndex: Integer;
    FSortDesc: Boolean;
    FCompareOption: TCompareOption;
    FSelectedItems: TStringListEx;
    FFileSourceL, FFileSourceR: IFileSource;
    FCmpFileSourceL, FCmpFileSourceR: IFileSource;
    FCmpFilePathL, FCmpFilePathR: string;
    FAddressL, FAddressR: string;
    hCols: array [0..6] of record Left, Width: Integer end;
    CheckContentThread: TObject;
    Ftotal, Fequal, Fnoneq, FuniqueL, FuniqueR: Integer;
    FOperation: TFileSourceOperation;
    FCopyStatistics: TFileSourceCopyOperationStatistics;
    FDeleteStatistics: TFileSourceDeleteOperationStatistics;
    FFileSourceOperationMessageBoxesUI: TFileSourceOperationMessageBoxesUI;

    function createCompareOption: TCompareOption;
    function createFilterFlags: TFilterFlags;

    procedure Compare;
    procedure FillFoundItemsDG;
    procedure InitVisibleItems;
    procedure RecalcHeaderCols;
    procedure ScanDirs;
    procedure SetSortIndex(AValue: Integer);
    procedure SortFoundItems;
    procedure SortFoundItems(sl: TStringList);
    procedure UpdateStatusBar;
    procedure StopCheckContentThread;
    procedure toggleSelectionActions;
    procedure EnableControls(AEnabled: Boolean);
    procedure SetSyncRecState(AState: TSyncRecState);
    procedure DeleteFiles(ALeft, ARight: Boolean);
    function DeleteFiles(FileSource: IFileSource; var Files: TFiles): Boolean;
    function DeleteFile(FileSource: IFileSource; const f: TFile): Boolean;
    procedure UpdateList(ALeft, ARight: TFiles; ARemoveLeft, ARemoveRight: Boolean);
    procedure SetProgressBytes(AProgressBar: TKASProgressBar; CurrentBytes: Int64; TotalBytes: Int64);
    procedure SetProgressFiles(AProgressBar: TKASProgressBar; CurrentFiles: Int64; TotalFiles: Int64);
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
  uFileSystemFileSource, uFileSourceOperationOptions, DCDateTimeUtils, SyncObjs,
  uDCUtils, uFileSourceUtil, uFileSourceOperationTypes, uShowForm, uAdministrator,
  uOSUtils, uLng, uMasks, Math, uClipboard, IntegerList, fMaskInputDlg, uSearchTemplate,
  LCLVersion, SysConst, DCStrUtils, DCOSUtils, uTypes, uFileSystemDeleteOperation, uFindFiles,
  uFileSourceManager, uFileSourceProperty, uShowMsg;

{$R *.lfm}

const
  GRID_COLUMN_FMT = 'HeaderDG_Column%d_Width';

type

  { TCheckContentThread }

  TCheckContentThread = class(TThread)
  private
    FDone: Boolean;
    FTimer: Integer;
    FOwner: TfrmSyncDirsDlg;
    FMutex: TCriticalSection;
    FStatistics: TFileSourceCopyOperationStatistics;
  private
    procedure DoStart;
    procedure DoFinish;
    procedure UpdateGrid;
    procedure ReapplyFilter;
  protected
    procedure Execute; override;
    function RetrieveStatistics: TFileSourceCopyOperationStatistics;
    procedure UpdateStatistics(var NewStatistics: TFileSourceCopyOperationStatistics);
    procedure SetProgressBytes(AProgressBar: TKASProgressBar; CurrentBytes: Int64; TotalBytes: Int64);
  public
    constructor Create(Owner: TfrmSyncDirsDlg);
    destructor Destroy; override;
    property Done: Boolean read FDone;
  end;

function consultCopyOperation(var params: TFileSourceConsultParams): Boolean;
begin
  Result:= False;
  params.operationType:= fsoCopy;
  FileSourceManager.consultOperation(params);
  if params.consultResult <> fscrSuccess then
    Exit;
  if params.operationTemp then
    Exit;
  Result:= True;
end;

function consultAndConfirmCopyOperation(var params: TFileSourceConsultParams): Boolean;
begin
  Result:= False;
  if consultCopyOperation(params) then
    FileSourceManager.confirmOperation(params);
  if params.consultResult <> fscrSuccess then
    Exit;
  if params.operationTemp then
    Exit;
  Result:= True;
end;

function supportsSyncDirs(
  const sourceFS: IFileSource;
  const targetFS: IFileSource ): Boolean;
var
  params: TFileSourceConsultParams;
begin
  params:= Default(TFileSourceConsultParams);
  params.sourceFS:= sourceFS;
  params.targetFS:= targetFS;
  Result:= consultCopyOperation(params);
end;

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
    if NOT supportsSyncDirs(leftFS,rightFS) then
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
      if not IsCellSelected[Col, R] then
        MoveExtend(False, Col, R, False)
      else begin
        C:= Row;
        PInteger(@Row)^:= R;
        InvalidateRow(C);
        InvalidateRow(R);
      end;
    end;
  end;
end;

{ TCheckContentThread }

procedure TCheckContentThread.DoStart;
begin
  with FOwner do
  begin
    Timer.Enabled:= True;
    HeaderDG.Enabled:= False;
    GroupBox1.Enabled:= False;
    MainDrawGrid.Enabled:= False;
    pnlCopyProgress.Visible:= True;
    ProgressBar.SetProgress(0, 100);
    pnlDeleteProgress.Visible:= False;
    lblProgress.Caption:= rsDiffComparing;
  end;
  FOwner.pnlProgress.Visible:= True;
end;

procedure TCheckContentThread.DoFinish;
begin
  FOwner.FComparing:= False;
  FOwner.Timer.Enabled:= False;
  FOwner.HeaderDG.Enabled:= True;
  FOwner.TopPanel.Enabled:= True;
  FOwner.GroupBox1.Enabled:= True;
  FOwner.MainDrawGrid.Enabled:= True;
  FOwner.pnlProgress.Visible:= False;
end;

procedure TCheckContentThread.UpdateGrid;
begin
  FOwner.MainDrawGrid.Invalidate;
  FOwner.UpdateStatusBar;
end;

procedure TCheckContentThread.ReapplyFilter;
begin
  FOwner.FillFoundItemsDG;
  FOwner.UpdateStatusBar;
end;

procedure TCheckContentThread.Execute;
const
  BUF_LEN = 1024 * 1024;
var
  Buffer1, Buffer2: PByte;
  Statistics: TFileSourceCopyOperationStatistics;

  function CompareFiles(const FileName1, FileName2: String; Size: Int64): Boolean;
  var
    DoneBytes, Count: Int64;
    File1, File2: TFileStreamEx;
  begin
    File1 := TFileStreamEx.Create(FileName1, fmOpenRead or fmShareDenyWrite);
    try
      File2 := TFileStreamEx.Create(FileName2, fmOpenRead or fmShareDenyWrite);
      try
        DoneBytes := 0;

        repeat
          if Size - DoneBytes <= BUF_LEN then
            Count := Size - DoneBytes
          else begin
            Count := BUF_LEN;
          end;

          File1.ReadBuffer(Buffer1^, Count);
          File2.ReadBuffer(Buffer2^, Count);

          if (Count <> BUF_LEN) then
            Result := CompareByte(Buffer1^, Buffer2^, Count) = 0
          else begin
            Result := CompareDWord(Buffer1^, Buffer2^, Count div SizeOf(Dword)) = 0;
          end;

          Statistics.DoneBytes += Count;
          DoneBytes := DoneBytes + Count;

          UpdateStatistics(Statistics);

        until Terminated or not Result or (DoneBytes >= Size);
      finally
        File2.Free;
      end;
    finally
      File1.Free;
    end;
  end;

var
  isEqual: Boolean;
  dirIndex, fileIndex: Integer;
  rec: TFileSyncRec;
begin
  Synchronize(@DoStart);
  Buffer1:= GetMem(BUF_LEN);
  Buffer2:= GetMem(BUF_LEN);
  try
    if (Buffer1 = nil) or (Buffer2 = nil) then
      raise EOutOfMemory.Create(SOutOfMemory);

    with FOwner do
    begin
      Statistics.DoneBytes:= 0;
      Statistics.TotalBytes:= 0;
      for dirIndex := 0 to FFullTree.Count - 1 do
      begin
        for fileIndex := 0 to FFullTree.dirItem(dirIndex).fileCount - 1 do
        begin
          if Terminated then Exit;
          rec := FFullTree.fileSyncRec(dirIndex, fileIndex);
          if NOT rec.isDir and (rec.state = srsUnknown) then
          begin
            Statistics.TotalBytes+= rec.leftFile.Size;
          end;
        end;
      end;
      UpdateStatistics(Statistics);
    end;

    with FOwner do
    for dirIndex := 0 to FFullTree.Count - 1 do
    begin
      for fileIndex := 0 to FFullTree.dirItem(dirIndex).fileCount - 1 do
      begin
        if Terminated then Exit;
        rec := FFullTree.fileSyncRec(dirIndex, fileIndex);
        if NOT rec.isDir and (rec.state = srsUnknown) then
        begin
          try
            isEqual:= CompareFiles(rec.leftFile.FullPath, rec.rightFile.FullPath, rec.leftFile.Size);
            if Terminated then Exit;
            if isEqual then
            begin
              Inc(Fequal);
              Dec(Fnoneq);
              rec.state := srsEqual
            end
            else begin
              rec.state := srsNotEq;
            end;
            if rec.action = srsUnknown then
            begin
              rec.action := rec.state;
            end;
          except
            on E: Exception do
              DCDebug('[SyncDirs::CmpContentThread] ' + E.Message);
          end;
        end;
      end;
    end;
    FDone := True;
    Synchronize(@ReapplyFilter);
  finally
    Synchronize(@DoFinish);
    if Assigned(Buffer1) then FreeMem(Buffer1);
    if Assigned(Buffer2) then FreeMem(Buffer2);
  end;
end;

function TCheckContentThread.RetrieveStatistics: TFileSourceCopyOperationStatistics;
begin
  FMutex.Acquire;
  try
    Result := Self.FStatistics;
  finally
    FMutex.Release;
  end;
end;

procedure TCheckContentThread.UpdateStatistics(var NewStatistics: TFileSourceCopyOperationStatistics);
begin
  FMutex.Acquire;
  try
    FStatistics := NewStatistics;
  finally
    FMutex.Release;
  end;
end;

procedure TCheckContentThread.SetProgressBytes(AProgressBar: TKASProgressBar;
  CurrentBytes: Int64; TotalBytes: Int64);
var
  BarText : String;
begin
  BarText := cnvFormatFileSize(CurrentBytes, uoscOperation) + '/' + cnvFormatFileSize(TotalBytes, uoscOperation);
  AProgressBar.SetProgress(CurrentBytes, TotalBytes, BarText );
end;

constructor TCheckContentThread.Create(Owner: TfrmSyncDirsDlg);
begin
  FOwner := Owner;
  FMutex := TCriticalSection.Create;
  inherited Create(False);
end;

destructor TCheckContentThread.Destroy;
begin
  inherited Destroy;
  FMutex.Free;
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
  else if FComparing then
    StopCheckContentThread
  else begin
    pnlProgress.Hide;
  end;
end;

procedure TfrmSyncDirsDlg.btnCompareClick(Sender: TObject);
begin
  if not IsMaskSearchTemplate(cbExtFilter.Text) then
    InsertFirstItem(Trim(cbExtFilter.Text), cbExtFilter);
  StatusBar1.Panels[0].Text := Format(rsComparingPercent, [0]);
  StopCheckContentThread;
  Compare;
end;

procedure TfrmSyncDirsDlg.btnSynchronizeClick(Sender: TObject);
var
  FileExistsOption: TFileSourceOperationOptionFileExists;
  SymLinkOption: TFileSourceOperationOptionSymLink = fsooslNone;

  function CopyFiles(src, dst: IFileSource; fs: TFiles; Dest: string): Boolean;
  var
    params: TFileSourceConsultParams;
  begin
    fs.Path:= fs[0].Path;

    params:= Default(TFileSourceConsultParams);
    params.sourceFS:= src;
    params.targetFS:= dst;
    params.files:= fs;
    params.targetPath:= Dest;
    if NOT consultAndConfirmCopyOperation(params) then
    begin
      MessageDlg(rsMsgErrNotSupported, mtError, [mbOK], 0);
      Exit(False);
    end;

    // Create destination directory
    Dst.CreateDirectory(ExcludeBackPathDelimiter(Dest));

    // Determine operation type
    case params.resultOperationType of
      fsoCopy:
        begin
          // Copy within the same file source.
          FOperation := params.resultFS.CreateCopyOperation(
                        params.files,
                        params.resultTargetPath ) as TFileSourceCopyOperation;
        end;
      fsoCopyOut:
        begin
          // CopyOut to filesystem.
          FOperation := params.resultFS.CreateCopyOutOperation(
                         Dst,
                         params.files,
                         params.resultTargetPath) as TFileSourceCopyOperation;
        end;
      fsoCopyIn:
        begin
          // CopyIn from filesystem.
          FOperation := params.resultFS.CreateCopyInOperation(
                         Src,
                         params.files,
                         params.resultTargetPath) as TFileSourceCopyOperation;
        end;
    end;
    if not Assigned(FOperation) then
    begin
      MessageDlg(rsMsgErrNotSupported, mtError, [mbOK], 0);
      Exit(False);
    end;
    FOperation.Elevate:= ElevateAction;
    TFileSourceCopyOperation(FOperation).SymLinkOption := SymLinkOption;
    TFileSourceCopyOperation(FOperation).FileExistsOption := FileExistsOption;
    FOperation.AddUserInterface(FFileSourceOperationMessageBoxesUI);
    try
      FOperation.Execute;
      Result := FOperation.Result = fsorFinished;
      SymLinkOption := TFileSourceCopyOperation(FOperation).SymLinkOption;
      FileExistsOption := TFileSourceCopyOperation(FOperation).FileExistsOption;
      FCopyStatistics.DoneBytes+= TFileSourceCopyOperation(FOperation).RetrieveStatistics.TotalBytes;
      SetProgressBytes(ProgressBar, FCopyStatistics.DoneBytes, FCopyStatistics.TotalBytes);
    finally
      FreeAndNil(FOperation);
    end;
  end;

  procedure processDir(const syncRec: TFileSyncRec);
  begin
    case syncRec.action of
      srsCopyToRight:
        CreateDirectoryFromFile(
          FCmpFileSourceR,
          FCmpFilePathR + syncRec.relPath,
          FCmpFileSourceL,
          syncRec.leftFile);
      srsCopyToLeft:
        CreateDirectoryFromFile(
          FCmpFileSourceL,
          FCmpFilePathL + syncRec.relPath,
          FCmpFileSourceR,
          syncRec.rightFile);
      srsDeleteRight:
        DeleteFile(FCmpFileSourceR, syncRec.rightFile);
      srsDeleteLeft:
        DeleteFile(FCmpFileSourceL, syncRec.leftFile);
    end;
  end;

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
      if TDirSyncRec(syncRec).isEmpty then
        continue;
      if NOT Assigned(syncRec.rightFile) then
        continue;
      if Assigned(syncRec.leftFile) then
        continue;
      if isEmptyDir(FCmpFileSourceR,syncRec.rightFile.FullPath) then
        DeleteFile(FCmpFileSourceR, syncRec.rightFile);
    end;
  end;

var
  i,
  DeleteLeftCount, DeleteRightCount,
  CopyLeftCount, CopyRightCount: Integer;
  CopyLeftSize, CopyRightSize: Int64;
  fsr: TFileSyncRec;
  DeleteLeft, DeleteRight,
  CopyLeft, CopyRight: Boolean;
  DeleteLeftFiles, DeleteRightFiles,
  CopyLeftFiles, CopyRightFiles: TFiles;
  Dest: string;
begin
  DeleteLeftCount := 0; DeleteRightCount := 0;
  CopyLeftCount := 0; CopyRightCount := 0;
  CopyLeftSize := 0;  CopyRightSize := 0;

  for i := 0 to FFilteredList.Count - 1 do begin
    fsr := FFilteredList.fileSyncRec(i);
    case fsr.action of
      srsCopyToLeft:
        begin
          Inc(CopyLeftCount);
          Inc(CopyLeftSize, fsr.rightFile.Size);
        end;
      srsCopyToRight:
        begin
          Inc(CopyRightCount);
          Inc(CopyRightSize, fsr.leftFile.Size);
        end;
      srsDeleteLeft:
        begin
          Inc(DeleteLeftCount);
        end;
      srsDeleteRight:
        begin
          Inc(DeleteRightCount);
        end;
      srsDeleteBoth:
        begin
          Inc(DeleteLeftCount);
          Inc(DeleteRightCount);
        end;
    end;
  end;
  FCopyStatistics.DoneBytes:= 0;
  FDeleteStatistics.DoneFiles:= 0;
  FCopyStatistics.TotalBytes:= CopyLeftSize + CopyRightSize;
  FDeleteStatistics.TotalFiles:= DeleteLeftCount + DeleteRightCount;

  with TfrmSyncDirsPerformDlg.Create(Self) do
  try
    edLeftPath.Text := FCmpFileSourceL.CurrentAddress + FCmpFilePathL;
    edRightPath.Text := FCmpFileSourceR.CurrentAddress + FCmpFilePathR;
    if CopyLeftCount > 0 then
    begin
      chkRightToLeft.Enabled := True;
      chkRightToLeft.Checked := True;
      edLeftPath.Enabled := True;
    end;
    if CopyRightCount > 0 then
    begin
      chkLeftToRight.Enabled := True;
      chkLeftToRight.Checked := True;
      edRightPath.Enabled := True;
    end;
    chkDeleteLeft.Enabled := DeleteLeftCount > 0;
    chkDeleteLeft.Checked := chkDeleteLeft.Enabled;
    chkDeleteRight.Enabled := DeleteRightCount > 0;
    chkDeleteRight.Checked := chkDeleteRight.Enabled;
    chkDeleteLeft.Caption := Format(rsDeleteLeft, [DeleteLeftCount]);
    chkDeleteRight.Caption := Format(rsDeleteRight, [DeleteRightCount]);
    chkLeftToRight.Caption :=
      Format(rsLeftToRightCopy, [CopyRightCount, cnvFormatFileSize(CopyRightSize, fsfFloat, gFileSizeDigits), IntToStrTS(CopyRightSize)]);
    chkRightToLeft.Caption :=
      Format(rsRightToLeftCopy, [CopyLeftCount, cnvFormatFileSize(CopyLeftSize, fsfFloat, gFileSizeDigits), IntToStrTS(CopyLeftSize)]);
    if ShowModal = mrOk then
    begin
      EnableControls(False);
      if chkConfirmOverwrites.Checked then
        FileExistsOption := fsoofeNone
      else begin
        FileExistsOption := fsoofeOverwrite;
      end;
      CopyLeft := chkRightToLeft.Checked;
      CopyRight := chkLeftToRight.Checked;
      DeleteLeft := chkDeleteLeft.Checked;
      DeleteRight := chkDeleteRight.Checked;

      lblProgress.Caption := rsOperCopying;
      lblProgressDelete.Caption := rsOperDeleting;
      ProgressBar.Position:=0;
      ProgressBarDelete.Position:=0;
      pnlCopyProgress.Visible:= CopyLeft or CopyRight;
      pnlDeleteProgress.Visible:= DeleteLeft or DeleteRight;

      i := 0;
      while i < FFilteredList.Count do
      begin
        CopyLeftFiles := TFiles.Create('');
        CopyRightFiles := TFiles.Create('');
        DeleteLeftFiles := TFiles.Create('');
        DeleteRightFiles := TFiles.Create('');
        fsr := FFilteredList.fileSyncRec(i);
        if fsr.isDir then begin
          processDir(fsr);
          i := i + 1;
          continue;
        end;

        repeat
          Dest := fsr.relPath;
          case fsr.action of
            srsCopyToRight:
              if CopyRight then CopyRightFiles.Add(fsr.leftFile.Clone);
            srsCopyToLeft:
              if CopyLeft then CopyLeftFiles.Add(fsr.rightFile.Clone);
            srsDeleteRight:
              if DeleteRight then DeleteRightFiles.Add(fsr.rightFile.Clone);
            srsDeleteLeft:
              if DeleteLeft then DeleteLeftFiles.Add(fsr.leftFile.Clone);
            srsDeleteBoth:
              begin
                if DeleteRight then DeleteRightFiles.Add(fsr.rightFile.Clone);
                if DeleteLeft then DeleteLeftFiles.Add(fsr.leftFile.Clone);
              end;
          end;
          i := i + 1;
          if i < FFilteredList.Count then
            fsr := FFilteredList.fileSyncRec(i);
        until (i = FFilteredList.Count) or fsr.isDir;

        if CopyLeftFiles.Count > 0 then
        begin
          if not CopyFiles(FCmpFileSourceR, FCmpFileSourceL, CopyLeftFiles,
            FCmpFilePathL + Dest) then Break;
        end else CopyLeftFiles.Free;
        if CopyRightFiles.Count > 0 then
        begin
          if not CopyFiles(FCmpFileSourceL, FCmpFileSourceR, CopyRightFiles,
            FCmpFilePathR + Dest) then Break;
        end else CopyRightFiles.Free;
        if DeleteLeftFiles.Count > 0 then
        begin
          if not DeleteFiles(FCmpFileSourceL, DeleteLeftFiles) then Break;
        end
        else DeleteLeftFiles.Free;
        if DeleteRightFiles.Count > 0 then
        begin
          if not DeleteFiles(FCmpFileSourceR, DeleteRightFiles) then Break;
        end
        else DeleteRightFiles.Free;
        if not pnlProgress.Visible then Break;
      end;

      removeAsymmetricRightEmptyDirs;

      EnableControls(True);
      btnCompare.Click;
    end;
  finally
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
  StopCheckContentThread;
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
  else if FComparing then
  begin
    CanClose := False;
    StopCheckContentThread;
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
        Brush.Color:= gColors.SyncDirs^.DirSelectedColor;
        FillRect(aRect);
      end;
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
      toggleSelectionActions;
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
  MainDrawGrid.MouseToCell(X, Y, c, r);
  if (r < 0) or (r >= FFilteredList.Count)
  or (x - 2 < hCols[3].Left)
  or (x - 2 > hCols[3].Left + hCols[3].Width)
  then
    Exit;
  toggleSelectionActions;
end;

procedure TfrmSyncDirsDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    if FScanning then
      FCancel := True
    else if FComparing then
      StopCheckContentThread
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
  else if Assigned(CheckContentThread) then
  begin
    with TCheckContentThread(CheckContentThread) do
    begin
      Inc(FTimer);
      CopyStatistics:= RetrieveStatistics;
      if (FTimer mod 5 = 0) then UpdateGrid;
      SetProgressBytes(ProgressBar, CopyStatistics.DoneBytes, CopyStatistics.TotalBytes);
    end;
  end;
end;

function TfrmSyncDirsDlg.createCompareOption: TCompareOption;
var
  flags: TCompareFlags;
begin
  flags:= [];
  if self.chkOnlySelected.Checked then
    Include( flags, TCompareFlag.cfOnlySelected );
  if self.chkEmptyDir.Checked then
    Include( flags, TCompareFlag.cfEmptyDirs );
  if self.chkAsymmetric.Checked then
    Include( flags, TCompareFlag.cfAsymmetric );
  if self.chkSubDirs.Checked then
    Include( flags, TCompareFlag.cfSubdirs );
  if self.chkByContent.Checked then
    Include( flags, TCompareFlag.cfByContent );
  if self.chkIgnoreDate.Checked then
    Include( flags, TCompareFlag.cfIgnoreDate );

  if (FFileSourceL.IsClass(TFileSystemFileSource)) and (FFileSourceR.IsClass(TFileSystemFileSource)) then begin
    if gNtfsHourTimeDelay and NtfsHourTimeDelay(self.edPath1.Text, self.edPath2.Text) then
      Include( flags, TCompareFlag.cfNtfsShift );
  end;

  Result:= TCompareOption.Create( flags );
end;

function TfrmSyncDirsDlg.createFilterFlags: TFilterFlags;
begin
  Result:= [];
  if self.sbCopyRight.Down then
    Include( Result, TFilterFlag.ffCopyRight );
  if self.sbCopyLeft.Down then
    Include( Result, TFilterFlag.ffCopyLeft );
  if self.sbEqual.Down then
    Include( Result, TFilterFlag.ffEqual );
  if self.sbNotEqual.Down then
    Include( Result, TFilterFlag.ffNotEqual );
  if self.sbUnknown.Down then
    Include( Result, TFilterFlag.ffUnknown );
  if self.sbDuplicates.Down then
    Include( Result, TFilterFlag.ffDuplicate );
  if self.sbSingles.Down then
    Include( Result, TFilterFlag.ffSingle );
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
  if AValue = FSortIndex then
  begin
    s := HeaderDG.Columns[AValue].Title.Caption;
    UTF8Delete(s, 1, 1);
    FSortDesc := not FSortDesc;
    s := getSortIndicator() + s;
    HeaderDG.Columns[AValue].Title.Caption := s;
    SortFoundItems;
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
    with HeaderDG.Columns[FSortIndex].Title do
      Caption := getSortIndicator() + Caption;
    SortFoundItems;
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
    TopPanel.Enabled := not FComparing;
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
  MaskList: TMaskList;
  Template: TSearchTemplate;
  LeftFirst: Boolean = True;
  RightFirst: Boolean = True;
  BaseDirL, BaseDirR: string;

  procedure ScanDir(
    dir: string;
    const leftParentDirs: TStringList;
    const rightParentDirs: TStringList);

    procedure ProcessOneSide(dirItem: TTwoLevelTreeDirItem; dirs: TStringList; var ASide: Boolean; sideLeft: Boolean);
    var
      fs: TFiles;
      i, j: Integer;
      f: TFile;
      r: TFileSyncRec;
      fn: String;
      dirFullPath: String;
      dirSyncRec: TDirSyncRec;
      currentFileSource: IFileSource;
    begin
      dirSyncRec := dirItem.dirSyncRec;
      if sideLeft then begin
        currentFileSource := FFileSourceL;
        dirFullPath := BaseDirL + dir;
      end else begin
        currentFileSource := FFileSourceR;
        dirFullPath := BaseDirR + dir;
      end;
      fs := currentFileSource.GetFiles(dirFullPath);
      if (cfOnlySelected in FCompareOption.flags) and ASide then
      begin
        ASide:= False;
        for I:= fs.Count - 1 downto 0 do
        begin
          if FSelectedItems.IndexOf(fs[I].Name) < 0 then
            fs.Delete(I);
        end;
      end;
      try
        for i := 0 to fs.Count - 1 do
        begin
          f := fs.Items[i];
          if f.Name = EmptyStr then
            f.Name := currentFileSource.GetDisplayFileName(f);
          fn := NormalizeFileName(f.Name);
          if f.IsDirectory or f.IsLinkToDirectory then
          begin
            if (f.NameNoExt <> '.') and (f.NameNoExt <> '..') then
            begin
              if (Template = nil) or (CheckDirectoryName(Template.FileChecks, f.Name)) then
                dirs.AddObject(fn, f.Clone);  // dirs don't own Object
            end;
          end
          else if (Template = nil) or Template.CheckFile(f) then
          begin
            if ((MaskList = nil) or MaskList.Matches(f.Name)) then
            begin
              j := dirItem.indexOfFile(fn);
              if j < 0 then
                r := TFileSyncRec.Create(FCompareOption, dir)
              else
                r := dirItem.fileSyncRec(j);
              if sideLeft then
              begin
                r.leftFile := f.Clone;
                r.updateState;
              end else begin
                r.rightFile := f.Clone;
                r.updateState;
                if (cfByContent in FCompareOption.flags) and (r.state = srsEqual) and (r.rightFile.Size > 0) then
                begin
                  r.action := srsUnknown;
                  r.state := srsUnknown;
                end;
              end;
              dirItem.addFile(fn, r);
              dirSyncRec.incChildrenCount(sideLeft);
            end;
          end;
        end;
      finally
        fs.Free;
      end;
    end;

    procedure setDirSyncRecFile(dirSyncRec: TDirSyncRec);
    var
      i: Integer;
      currentDirPart: String;
    begin
      currentDirPart:= GetLastDir(dir);
      i:= leftParentDirs.IndexOf(currentDirPart);
      if i >= 0 then
        dirSyncRec.leftFile:= TFile(leftParentDirs.Objects[i]);    // owns file
      i:= rightParentDirs.IndexOf(currentDirPart);
      if i >= 0 then
        dirSyncRec.rightFile:= TFile(rightParentDirs.Objects[i]);   // owns file
    end;

  var
    i, j, tot: Integer;
    dirItem: TTwoLevelTreeDirItem;
    dirsLeft, dirsRight: TStringListEx;
    d: string;
    dirSyncRec: TDirSyncRec;
  begin
    i := FFullTree.indexOfDir(dir);
    if i < 0 then begin
      dirSyncRec := TDirSyncRec.Create(FCompareOption, dir);
      dirItem := TTwoLevelTreeDirItem.Create(dirSyncRec);
      FFullTree.addDir(dir, dirItem);
    end else begin
      dirItem := FFullTree.dirItem(i);
      dirSyncRec := dirItem.dirSyncRec;
    end;

    if dir <> '' then begin
      setDirSyncRecFile(dirSyncRec);
      dir := AppendPathDelim(dir);
    end;

    dirsLeft := TStringListEx.Create;
    dirsLeft.CaseSensitive := FileNameCaseSensitive;
    dirsLeft.Sorted := True;
    dirsRight := TStringListEx.Create;
    dirsRight.CaseSensitive := FileNameCaseSensitive;
    dirsRight.Sorted := True;
    try
      Application.ProcessMessages;
      if FCancel then Exit;
      ProcessOneSide(dirItem, dirsLeft, LeftFirst, True);
      ProcessOneSide(dirItem, dirsRight, RightFirst, False);
      dirSyncRec.updateState;
      SortFoundItems(dirItem.files);
      if not (cfSubdirs in FCompareOption.flags) then Exit;
      tot := dirsLeft.Count + dirsRight.Count;
      for i := 0 to dirsLeft.Count - 1 do
      begin
        if dir = '' then
          StatusBar1.Panels[0].Text :=
            Format(rsComparingPercent, [i * 100 div tot]);
        d := dirsLeft[i];
        ScanDir(dir + d, dirsLeft, dirsRight);
        if FCancel then Exit;
        j := dirsRight.IndexOf(d);
        if j >= 0 then
        begin
          dirsRight.Delete(j);
          Dec(tot);
        end
      end;
      for i := 0 to dirsRight.Count - 1 do
      begin
        if dir = '' then
          StatusBar1.Panels[0].Text :=
            Format(rsComparingPercent, [(dirsLeft.Count + i) * 100 div tot]);
        d := dirsRight[i];
        ScanDir(dir + d, dirsLeft, dirsRight);
        if FCancel then Exit;
      end;
    finally
      dirsLeft.Free;
      dirsRight.Free;
    end;
  end;

begin
  FScanning := True;
  try
  FCancel := False;
  FCmpFileSourceL := FFileSourceL;
  FCmpFileSourceR := FFileSourceR;
  BaseDirL := AppendPathDelim(edPath1.Text);
  if IsMaskSearchTemplate(cbExtFilter.Text) then
  begin
    MaskList := nil;
    Template:= gSearchTemplateList.TemplateByName[cbExtFilter.Text];
  end
  else begin
    Template := nil;
    if cbExtFilter.Text <> EmptyStr then
      MaskList := TMaskList.Create(cbExtFilter.Text)
    else
      MaskList := TMaskList.Create( '*' );
  end;
  if (FAddressL <> '') and (Copy(BaseDirL, 1, Length(FAddressL)) = FAddressL) then
    Delete(BaseDirL, 1, Length(FAddressL));
  BaseDirR := AppendPathDelim(edPath2.Text);
  if (FAddressR <> '') and (Copy(BaseDirR, 1, Length(FAddressR)) = FAddressR) then
    Delete(BaseDirR, 1, Length(FAddressR));
  FCmpFilePathL := BaseDirL;
  FCmpFilePathR := BaseDirR;
  ScanDir('', nil, nil);
  MaskList.Free;
  FillFoundItemsDG;
  if FCancel then Exit;
  if (FFullTree.Count > 0) and (cfByContent in FCompareOption.flags) then
  begin
    CheckContentThread := TCheckContentThread.Create(Self);
    FComparing := True;
  end;
  finally
  FScanning := False;
  end;
end;

procedure TfrmSyncDirsDlg.SortFoundItems;
var
  i: Integer;
begin
  if FSortIndex < 0 then Exit;
  for i := 0 to FFullTree.Count - 1 do
    SortFoundItems( FFullTree.dirItem(i).files );
end;

procedure TfrmSyncDirsDlg.SortFoundItems(sl: TStringList);

  function CompareFn(sl: TStringList; i, j: Integer): Integer;
  var
    r1, r2: TFileSyncRec;
  begin
    if FSortIndex in [1..5] then
    begin
      r1 := TFileSyncRec(sl.Objects[i]);
      r2 := TFileSyncRec(sl.Objects[j]);
    end;
    case FSortIndex of
    0:
      Result := mbCompareStr(sl[i], sl[j]);
    1:
      if (Assigned(r1.leftFile) < Assigned(r2.leftFile))
      or Assigned(r2.leftFile) and (r1.leftFile.Size < r2.leftFile.Size) then
        Result := -1
      else
      if (Assigned(r1.leftFile) > Assigned(r2.leftFile))
      or Assigned(r1.leftFile) and (r1.leftFile.Size > r2.leftFile.Size) then
        Result := 1
      else
        Result := 0;
    2:
      if (Assigned(r1.leftFile) < Assigned(r2.leftFile))
      or Assigned(r2.leftFile)
      and (r1.leftFile.ModificationTime < r2.leftFile.ModificationTime) then
        Result := -1
      else
      if (Assigned(r1.leftFile) > Assigned(r2.leftFile))
      or Assigned(r1.leftFile)
      and (r1.leftFile.ModificationTime > r2.leftFile.ModificationTime) then
        Result := 1
      else
        Result := 0;
    4:
      if (Assigned(r1.rightFile) < Assigned(r2.rightFile))
      or Assigned(r2.rightFile)
      and (r1.rightFile.ModificationTime < r2.rightFile.ModificationTime) then
        Result := -1
      else
      if (Assigned(r1.rightFile) > Assigned(r2.rightFile))
      or Assigned(r1.rightFile)
      and (r1.rightFile.ModificationTime > r2.rightFile.ModificationTime) then
        Result := 1
      else
        Result := 0;
    5:
      if (Assigned(r1.rightFile) < Assigned(r2.rightFile))
      or Assigned(r2.rightFile) and (r1.rightFile.Size < r2.rightFile.Size) then
        Result := -1
      else
      if (Assigned(r1.rightFile) > Assigned(r2.rightFile))
      or Assigned(r1.rightFile) and (r1.rightFile.Size > r2.rightFile.Size) then
        Result := 1
      else
        Result := 0;
    6:
      Result := mbCompareStr(sl[i], sl[j]);
    end;
    if FSortDesc then
      Result := -Result;
  end;

  procedure QuickSort(L, R: Integer; sl: TStringList);
  var
    Pivot, vL, vR: Integer;
  begin
    if R - L <= 1 then begin // a little bit of time saver
      if L < R then
        if CompareFn(sl, L, R) > 0 then
          sl.Exchange(L, R);
      Exit;
    end;

    vL := L;
    vR := R;

    Pivot := L + Random(R - L); // they say random is best

    while vL < vR do begin
      while (vL < Pivot) and (CompareFn(sl, vL, Pivot) <= 0) do
        Inc(vL);

      while (vR > Pivot) and (CompareFn(sl, vR, Pivot) > 0) do
        Dec(vR);

      sl.Exchange(vL, vR);

      if Pivot = vL then // swap pivot if we just hit it from one side
        Pivot := vR
      else if Pivot = vR then
        Pivot := vL;
    end;

    if Pivot - 1 >= L then
      QuickSort(L, Pivot - 1, sl);
    if Pivot + 1 <= R then
      QuickSort(Pivot + 1, R, sl);
  end;

begin
  QuickSort(0, sl.Count - 1, sl);
end;

procedure TfrmSyncDirsDlg.UpdateStatusBar;
var s: string;
begin
  s := Format(rsFilesFound, [Ftotal, Fequal, Fnoneq, FuniqueL, FuniqueR]);
  if Assigned(CheckContentThread)
  and not TCheckContentThread(CheckContentThread).Done then
    s := s + ' ...';
  StatusBar1.Panels[0].Text := s;
end;

procedure TfrmSyncDirsDlg.StopCheckContentThread;
begin
  if Assigned(CheckContentThread) then
  begin
    with TCheckContentThread(CheckContentThread) do
    begin
      Terminate;
      WaitFor;
    end;
    FreeAndNil(CheckContentThread);
  end;
end;

procedure TfrmSyncDirsDlg.toggleSelectionActions;
begin
  SetSyncRecState(srsNextAction);
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

procedure TfrmSyncDirsDlg.SetSyncRecState(AState: TSyncRecState);
var
  _handled: TIntegerList = nil;

  procedure addHandled( const i: Integer );
  begin
    _handled.Add( i );
  end;

  function isHandled( const i : Integer ): Boolean;
  begin
    Result:= _handled.IndexOf(i) >= 0;
  end;

  procedure doUpdateAction(const index: Integer; NewAction: TSyncRecState);
  var
    rec: TFileSyncRec;
  begin
    if isHandled(index) then
      Exit;

    addHandled(index);

    rec:= FFilteredList.fileSyncRec(index);
    case NewAction of
      srsUnknown:
        NewAction:= rec.state;
      srsNotEq:
        begin
          if (rec.action = srsCopyToLeft) and Assigned(rec.leftFile) then
              NewAction:= srsCopyToRight
          else if (rec.action = srsCopyToRight) and Assigned(rec.rightFile) then
              NewAction:= srsCopyToLeft
          else
            NewAction:= rec.action
        end;
      srsCopyToLeft:
        begin
          if not Assigned(rec.rightFile) then
            NewAction:= srsDoNothing;
        end;
      srsCopyToRight:
        begin
          if not Assigned(rec.leftFile) then
            NewAction:= srsDoNothing;
        end;
      srsDeleteLeft:
        begin
          if not Assigned(rec.leftFile) then
            NewAction:= srsDoNothing;
        end;
      srsDeleteRight:
        begin
          if not Assigned(rec.rightFile) then
            NewAction:= srsDoNothing;
        end;
      srsDeleteBoth:
        begin
          if not Assigned(rec.leftFile) then
            NewAction:= srsDeleteRight;
          if not Assigned(rec.rightFile) then
            NewAction:= srsDeleteLeft;
        end;
      srsNextAction:
        NewAction:= rec.getNextAction;
    end;
    rec.action:= NewAction;
    MainDrawGrid.InvalidateRow(index);
  end;

  procedure checkAncestorsDirs(index: Integer; const NewAction: TSyncRecState);
  var
    rec: TFileSyncRec;
    basePath: String;
  begin
    if NOT (cfEmptyDirs in FCompareOption.flags) then
      Exit;

    rec:= FFilteredList.fileSyncRec(index);
    basePath:= IncludeTrailingPathDelimiter(rec.relPath);

    Dec(index);
    while index >= 0 do begin
      rec := FFilteredList.fileSyncRec(index);
      if rec.relPath = EmptyStr then
        break;
      if NOT PathIsInPath(basePath, rec.relPath) then
        break;
      if rec.isDir then begin
        if rec.state = srsDoNothing then
          break;
        doUpdateAction(index, NewAction);
      end;
      Dec(index);
    end;
  end;

  procedure uncheckDescendantsDirsAndFiles(index: Integer; const NewAction: TSyncRecState);
  var
    rec: TFileSyncRec;
    basePath: String;
  begin
    rec:= FFilteredList.fileSyncRec(index);
    basePath:= IncludeTrailingPathDelimiter(rec.relPath);
    Inc(index);
    if NOT (cfEmptyDirs in FCompareOption.flags) then begin
      while index < FFilteredList.Count do
      begin
        rec:= FFilteredList.fileSyncRec(index);
        if rec.isDir then
          break;
        doUpdateAction(index, NewAction);
        Inc(index);
      end;
    end else begin
      while index < FFilteredList.Count do
      begin
        rec:= FFilteredList.fileSyncRec(index);
        if NOT PathIsInPath(rec.relPath, basePath) then
          break;
        doUpdateAction(index, NewAction);
        Inc(index);
      end;
    end;
  end;

  procedure processOnlyOneSelection(const index: Integer);
  var
    rec: TFileSyncRec;
  begin
    if (index < 0) or (index >= FFilteredList.Count) then
      Exit;

    if isHandled(index) then
      Exit;

    rec:= FFilteredList.fileSyncRec(index);
    if rec.state = srsDoNothing then
      Exit;

    doUpdateAction(index, AState);

    case rec.action of
      srsCopyToLeft,
      srsCopyToRight:
        checkAncestorsDirs(index, rec.action);
      srsDeleteLeft,
      srsDeleteRight,
      srsDeleteBoth,
      srsDoNothing:
        if rec.isDir then
          uncheckDescendantsDirsAndFiles(index, rec.action);
    end;
  end;

  procedure processMultiSelection;
  var
    i: Integer;
  begin
    for i:= 0 to FFilteredList.Count-1 do begin
      if MainDrawGrid.IsCellSelected[0,i] then begin
        processOnlyOneSelection( i );
      end;
    end;
  end;

begin
  _handled:= TIntegerList.Create;

  if MainDrawGrid.HasMultiSelection or (MainDrawGrid.Selection.Height>0) then
    processMultiSelection
  else
    processOnlyOneSelection( MainDrawGrid.Row );

  _handled.Free;
end;

procedure TfrmSyncDirsDlg.DeleteFiles(ALeft, ARight: Boolean);

  procedure countSelectedVisibleItems(var leftCount: Integer; var rightCount: Integer);
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
    countSelectedVisibleItems( leftCount, rightCount );

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

      if ALeft then DeleteFiles(FCmpFileSourceL, ALeftList);
      if ARight then DeleteFiles(FCmpFileSourceR, ARightList);
      EnableControls(True);
    end;
  finally
    ALeftList.Free;
    ARightList.Free;
  end;
end;

function TfrmSyncDirsDlg.DeleteFiles(FileSource: IFileSource; var Files: TFiles): Boolean;
begin
  Files.Path := Files[0].Path;
  FOperation:= FileSource.CreateDeleteOperation(Files);
  if not Assigned(FOperation) then
  begin
    MessageDlg(rsMsgErrNotSupported, mtError, [mbOK], 0);
    Exit(False);
  end;
  if (FOperation is TFileSystemDeleteOperation) then
  begin
    TFileSystemDeleteOperation(FOperation).Recycle:= gUseTrash;
  end;
  FOperation.Elevate:= ElevateAction;
  FOperation.AddUserInterface(FFileSourceOperationMessageBoxesUI);
  try
    FOperation.Execute;
    Result := FOperation.Result = fsorFinished;
    FDeleteStatistics.DoneFiles+= TFileSourceDeleteOperation(FOperation).RetrieveStatistics.TotalFiles;
    SetProgressFiles(ProgressBarDelete, FDeleteStatistics.DoneFiles, FDeleteStatistics.TotalFiles);
  finally
    FreeAndNil(FOperation);
  end;
end;

function TfrmSyncDirsDlg.DeleteFile(FileSource: IFileSource; const f: TFile
  ): Boolean;
var
  files: TFiles;
begin
  files := TFiles.Create(EmptyStr);
  files.OwnsObjects:= False;
  files.Add(f);
  Result:= DeleteFiles(FileSource, files);
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
  FCompareOption.Free;
  inherited Destroy;
end;

procedure TfrmSyncDirsDlg.CopyToClipboard;
  procedure FillRowList(RowList: TIntegerList);
  var
    i: Integer;
  begin
    for i:= 0 to MainDrawGrid.RowCount-1 do begin
      if MainDrawGrid.IsCellSelected[0,i] then
        RowList.Add( i );
    end;
  end;

  procedure PrintRow(sl: TStringList; R: Integer);
  var
    s: string;
    SyncRec: TFileSyncRec;
  begin
    SyncRec := FFilteredList.fileSyncRec(R);
    if SyncRec.isDir then
    begin
      s := FFilteredList.path(R);
      if cfEmptyDirs in FCompareOption.flags then begin
        if SyncRec.state <> srsDoNothing then
          s := s + #9#9#9 + SYNC_REC_STATE_SYMBOL[SyncRec.action];
      end;
    end
    else
    begin
      if Assigned(SyncRec.leftFile) then
      begin
        s := FFilteredList.path(R) + #9 +
             IntToStrTS(SyncRec.leftFile.Size) + #9 +
             FormatDateTime(gDateTimeFormatSync, SyncRec.leftFile.ModificationTime);
      end
      else
      begin
        s := #9#9;
      end;
      s := s + #9 + SYNC_REC_STATE_SYMBOL[SyncRec.action] + #9;
      if Assigned(SyncRec.rightFile) then
      begin
        s := s +
             FormatDateTime(gDateTimeFormatSync, SyncRec.rightFile.ModificationTime) + #9 +
             IntToStrTS(SyncRec.rightFile.Size) + #9 +
             FFilteredList.path(R);
      end;
    end;
    sl.Add(s);
  end;

var
  sl: TStringList;
  RowList: TIntegerList;
  I: Integer;
begin
  sl := TStringList.Create;
  RowList := TIntegerList.Create;
  try
    FillRowList(RowList);
    for I := 0 to RowList.Count - 1 do
    begin
      PrintRow(sl, RowList[I]);
    end;
    ClipboardSetText(sl.Text);
  finally
    FreeAndNil(sl);
    FreeAndNil(RowList);
  end;
end;

procedure TfrmSyncDirsDlg.cm_SelectClear(const Params: array of string);
begin
  SetSyncRecState(srsDoNothing);
end;

procedure TfrmSyncDirsDlg.cm_SelectDeleteLeft(const Params: array of string);
begin
  SetSyncRecState(srsDeleteLeft);
end;

procedure TfrmSyncDirsDlg.cm_SelectDeleteRight(const Params: array of string);
begin
  SetSyncRecState(srsDeleteRight);
end;

procedure TfrmSyncDirsDlg.cm_SelectDeleteBoth(const Params: array of string);
begin
  SetSyncRecState(srsDeleteBoth);
end;

procedure TfrmSyncDirsDlg.cm_SelectCopyDefault(const Params: array of string);
begin
  SetSyncRecState(srsUnknown);
end;

procedure TfrmSyncDirsDlg.cm_SelectCopyReverse(const Params: array of string);
begin
  SetSyncRecState(srsNotEq);
end;

procedure TfrmSyncDirsDlg.cm_SelectCopyLeftToRight(const Params: array of string);
begin
  SetSyncRecState(srsCopyToRight);
end;

procedure TfrmSyncDirsDlg.cm_SelectCopyRightToLeft(const Params: array of string);
begin
  SetSyncRecState(srsCopyToLeft);
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

