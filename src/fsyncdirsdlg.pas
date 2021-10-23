{
   Double Commander
   -------------------------------------------------------------------------
   Directories synchronization utility (specially for DC)

   Copyright (C) 2013 Anton Panferov (ast.a_s@mail.ru)
   Copyright (C) 2014-2020 Alexander Koblov (alexx2000@mail.ru)

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
  ExtCtrls, Buttons, ComCtrls, Grids, Menus, ActnList, EditBtn, LazUTF8Classes,
  uFileView, uFileSource, uFileSourceCopyOperation, uFile, uFileSourceOperation,
  uFileSourceOperationMessageBoxesUI, uFormCommands, uHotkeyManager, uClassesEx,
  uFileSourceDeleteOperation, KASProgressBar;

const
  HotkeysCategory = 'Synchronize Directories';

type

  TSyncRecState = (srsUnknown, srsEqual, srsNotEq, srsCopyLeft, srsCopyRight, srsDeleteLeft,
    srsDeleteRight, srsDeleteBoth, srsDoNothing);

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
    FFoundItems: TStringListUtf8;
    FVisibleItems: TStringListUtf8;
    FSortIndex: Integer;
    FSortDesc: Boolean;
    FNtfsShift: Boolean;
    FFileExists: TSyncRecState;
    FSelectedItems: TStringListUtf8;
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
    procedure ClearFoundItems;
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
    procedure UpdateSelection(R: Integer);
    procedure EnableControls(AEnabled: Boolean);
    procedure SetSyncRecState(AState: TSyncRecState);
    procedure DeleteFiles(ALeft, ARight: Boolean);
    function DeleteFiles(FileSource: IFileSource; var Files: TFiles): Boolean;
    procedure UpdateList(ALeft, ARight: TFiles; ARemoveLeft, ARemoveRight: Boolean);
    procedure SetProgressBytes(AProgressBar: TKASProgressBar; CurrentBytes: Int64; TotalBytes: Int64);
    procedure SetProgressFiles(AProgressBar: TKASProgressBar; CurrentFiles: Int64; TotalFiles: Int64);
  private
    property SortIndex: Integer read FSortIndex write SetSortIndex;
    property Commands: TFormCommands read FCommands implements IFormCommands;
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
  DCClassesUtf8, uFileSystemFileSource, uFileSourceOperationOptions, DCDateTimeUtils,
  uDCUtils, uFileSourceUtil, uFileSourceOperationTypes, uShowForm, uAdministrator,
  uOSUtils, uLng, uMasks, Math, uClipboard, IntegerList, fMaskInputDlg, uSearchTemplate,
  StrUtils, uTypes, uFileSystemDeleteOperation;

{$R *.lfm}

const
  GRID_COLUMN_FMT = 'HeaderDG_Column%d_Width';

type

  { TFileSyncRec }

  TFileSyncRec = class
  private
    FRelPath: string;
    FState: TSyncRecState;
    FAction: TSyncRecState;
    FFileR, FFileL: TFile;
    FForm: TfrmSyncDirsDlg;
  public
    constructor Create(AForm: TfrmSyncDirsDlg; RelPath: string);
    destructor Destroy; override;
    procedure UpdateState(ignoreDate: Boolean);
  end;

  { TCheckContentThread }

  TCheckContentThread = class(TThread)
  private
    FOwner: TfrmSyncDirsDlg;
    FDone: Boolean;
    procedure UpdateGrid;
    procedure ReapplyFilter;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TfrmSyncDirsDlg);
    property Done: Boolean read FDone;
  end;

procedure ShowSyncDirsDlg(FileView1, FileView2: TFileView);
begin
  if not Assigned(FileView1) then
    raise Exception.Create('ShowSyncDirsDlg: FileView1=nil');
  if not Assigned(FileView2) then
    raise Exception.Create('ShowSyncDirsDlg: FileView2=nil');
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

  function CompareFiles(fn1, fn2: String; len: Int64): Boolean;
  const
    BUFLEN = 1024 * 32;
  var
    fs1, fs2: TFileStreamEx;
    buf1, buf2: array [1..BUFLEN] of Byte;
    i, j: Int64;
  begin
    fs1 := TFileStreamEx.Create(fn1, fmOpenRead or fmShareDenyWrite);
    try
      fs2 := TFileStreamEx.Create(fn2, fmOpenRead or fmShareDenyWrite);
      try
        i := 0;
        repeat
          if len - i <= BUFLEN then
            j := len - i
          else
            j := BUFLEN;
          fs1.Read(buf1, j);
          fs2.Read(buf2, j);
          i := i + j;
          Result := CompareMem(@buf1, @buf2, j);
        until Terminated or not Result or (i >= len);
      finally
        fs2.Free;
      end;
    finally
      fs1.Free;
    end;
  end;

var
  i, j: Integer;
  r: TFileSyncRec;
begin
  with FOwner do
    for i := 0 to FFoundItems.Count - 1 do
      for j := 0 to TStringList(FFoundItems.Objects[i]).Count - 1 do
      begin
        if Terminated then Exit;
          r := TFileSyncRec(TStringList(FFoundItems.Objects[i]).Objects[j]);
        if Assigned(r) and (r.FState = srsUnknown) then
        begin
          try
            if CompareFiles(r.FFileL.FullPath, r.FFileR.FullPath, r.FFileL.Size) then
            begin
              Inc(Fequal);
              r.FState := srsEqual
            end
            else
              r.FState := srsNotEq;
            if r.FAction = srsUnknown then
              r.FAction := r.FState;
            if j mod 20 = 0 then
              Synchronize(@UpdateGrid);
          except
            on e: Exception do
              DCDebug('[SyncDirs::CmpContentThread] ' + e.Message);
          end;
        end;
      end;
  FDone := True;
  Synchronize(@ReapplyFilter);
end;

constructor TCheckContentThread.Create(Owner: TfrmSyncDirsDlg);
begin
  FOwner := Owner;
  inherited Create(False);
end;

constructor TFileSyncRec.Create(AForm: TfrmSyncDirsDlg; RelPath: string);
begin
  FForm:= AForm;
  FRelPath := RelPath;
end;

destructor TFileSyncRec.Destroy;
begin
  FreeAndNil(FFileL);
  FreeAndNil(FFileR);
  inherited Destroy;
end;

procedure TFileSyncRec.UpdateState(ignoreDate: Boolean);
var
  FileTimeDiff: Integer;
begin
  FState := srsNotEq;
  if Assigned(FFileR) and not Assigned(FFileL) then
    FState := FForm.FFileExists
  else
  if not Assigned(FFileR) and Assigned(FFileL) then
    FState := srsCopyRight
  else begin
    FileTimeDiff := FileTimeCompare(FFileL.ModificationTime, FFileR.ModificationTime, FForm.FNtfsShift);
    if ((FileTimeDiff = 0) or ignoreDate) and (FFileL.Size = FFileR.Size) then
      FState := srsEqual
    else
    if not ignoreDate then
      if FileTimeDiff > 0 then
        FState := srsCopyRight
      else
      if FileTimeDiff < 0 then
        FState := srsCopyLeft;
  end;
  if FForm.chkAsymmetric.Checked and (FState = srsCopyLeft) then
    FAction := srsDoNothing
  else begin
    FAction := FState;
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
  if ShowMaskInputDlg(rsMarkPlus, rsMaskInput, glsMaskHistory, sMask) then
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
  OperationType: TFileSourceOperationType;
  FileExistsOption: TFileSourceOperationOptionFileExists;
  SymLinkOption: TFileSourceOperationOptionSymLink = fsooslNone;

  function CopyFiles(src, dst: IFileSource; fs: TFiles; Dest: string): Boolean;
  begin
    if not GetCopyOperationType(Src, Dst, OperationType) then
    begin
      MessageDlg(rsMsgErrNotSupported, mtError, [mbOK], 0);
      Exit(False);
    end
    else begin
      Fs.Path:= fs[0].Path;
      // Create destination directory
      Dst.CreateDirectory(Dest);
      // Determine operation type
      case OperationType of
        fsoCopy:
          begin
            // Copy within the same file source.
            FOperation := Src.CreateCopyOperation(
                           Fs,
                           Dest) as TFileSourceCopyOperation;
          end;
        fsoCopyOut:
          begin
            // CopyOut to filesystem.
            FOperation := Src.CreateCopyOutOperation(
                           Dst,
                           Fs,
                           Dest) as TFileSourceCopyOperation;
          end;
        fsoCopyIn:
          begin
            // CopyIn from filesystem.
            FOperation := Dst.CreateCopyInOperation(
                           Src,
                           Fs,
                           Dest) as TFileSourceCopyOperation;
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
  for i := 0 to FVisibleItems.Count - 1 do
    if Assigned(FVisibleItems.Objects[i]) then
    begin
      fsr := TFileSyncRec(FVisibleItems.Objects[i]);
      case fsr.FAction of
      srsCopyLeft:
        begin
          Inc(CopyLeftCount);
          Inc(CopyLeftSize, fsr.FFileR.Size);
        end;
      srsCopyRight:
        begin
          Inc(CopyRightCount);
          Inc(CopyRightSize, fsr.FFileL.Size);
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
    if (CopyLeftCount > 0) and
        GetCopyOperationType(FFileSourceR, FFileSourceL, OperationType) then
    begin
      chkRightToLeft.Enabled := True;
      chkRightToLeft.Checked := True;
      edLeftPath.Enabled := True;
    end;
    if (CopyRightCount > 0) and
        GetCopyOperationType(FFileSourceL, FFileSourceR, OperationType) then
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
      Format(rsLeftToRightCopy, [CopyRightCount, cnvFormatFileSize(CopyRightSize, fsfFloat, gFileSizeDigits), Numb2USA(IntToStr(CopyRightSize))]);
    chkRightToLeft.Caption :=
      Format(rsRightToLeftCopy, [CopyLeftCount, cnvFormatFileSize(CopyLeftSize, fsfFloat, gFileSizeDigits), Numb2USA(IntToStr(CopyLeftSize))]);
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
      pnlCopyProgress.Visible:= CopyLeft or CopyRight;
      pnlDeleteProgress.Visible:= DeleteLeft or DeleteRight;
      i := 0;
      while i < FVisibleItems.Count do
      begin
        CopyLeftFiles := TFiles.Create('');
        CopyRightFiles := TFiles.Create('');
        DeleteLeftFiles := TFiles.Create('');
        DeleteRightFiles := TFiles.Create('');
        if FVisibleItems.Objects[i] <> nil then
          repeat
            fsr := TFileSyncRec(FVisibleItems.Objects[i]);
            Dest := fsr.FRelPath;
            case fsr.FAction of
            srsCopyRight:
              if CopyRight then CopyRightFiles.Add(fsr.FFileL.Clone);
            srsCopyLeft:
              if CopyLeft then CopyLeftFiles.Add(fsr.FFileR.Clone);
            srsDeleteRight:
              if DeleteRight then DeleteRightFiles.Add(fsr.FFileR.Clone);
            srsDeleteLeft:
              if DeleteLeft then DeleteLeftFiles.Add(fsr.FFileL.Clone);
            srsDeleteBoth:
              begin
                if DeleteRight then DeleteRightFiles.Add(fsr.FFileR.Clone);
                if DeleteLeft then DeleteLeftFiles.Add(fsr.FFileL.Clone);
              end;
            end;
            i := i + 1;
          until (i = FVisibleItems.Count) or (FVisibleItems.Objects[i] = nil);
        i := i + 1;
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
  gSyncDirsSubdirs              := chkSubDirs.Checked;
  gSyncDirsAsymmetric           := chkAsymmetric.Checked and gSyncDirsAsymmetricSave;
  gSyncDirsIgnoreDate           := chkIgnoreDate.Checked;
  gSyncDirsShowFilterCopyRight  := sbCopyRight.Down;
  gSyncDirsShowFilterEqual      := sbEqual.Down;
  gSyncDirsShowFilterNotEqual   := sbNotEqual.Down;
  gSyncDirsShowFilterCopyLeft   := sbCopyLeft.Down;
  gSyncDirsShowFilterDuplicates := sbDuplicates.Down;
  gSyncDirsShowFilterSingles    := sbSingles.Down;
  if not IsMaskSearchTemplate(cbExtFilter.Text) then
    gSyncDirsFileMask           := cbExtFilter.Text;
  if chkByContent.Enabled then
    gSyncDirsByContent          := chkByContent.Checked;
  glsMaskHistory.Assign(cbExtFilter.Items);

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

  lblProgress.Caption    := rsOperCopying;
  lblProgressDelete.Caption   := rsOperDeleting;
  { settings }
  chkSubDirs.Checked     := gSyncDirsSubdirs;
  chkAsymmetric.Checked  := gSyncDirsAsymmetric;
  chkByContent.Checked   := gSyncDirsByContent and chkByContent.Enabled;
  chkIgnoreDate.Checked  := gSyncDirsIgnoreDate;
  sbCopyRight.Down       := gSyncDirsShowFilterCopyRight;
  sbEqual.Down           := gSyncDirsShowFilterEqual;
  sbNotEqual.Down        := gSyncDirsShowFilterNotEqual;
  sbCopyLeft.Down        := gSyncDirsShowFilterCopyLeft;
  sbDuplicates.Down      := gSyncDirsShowFilterDuplicates;
  sbSingles.Down         := gSyncDirsShowFilterSingles;
  cbExtFilter.Items.Assign(glsMaskHistory);
  cbExtFilter.Text       := gSyncDirsFileMask;

  HMSync := HotMan.Register(Self, HotkeysCategory);
  HMSync.RegisterActionList(ActionList);
  FCommands := TFormCommands.Create(Self, ActionList);
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
  if (r < 0) or (r >= FVisibleItems.Count) then Exit;
  x := MainDrawGrid.ScreenToClient(Mouse.CursorPos).X;
  if (x > hCols[3].Left) and (x < hCols[3].Left + hCols[3].Width) then Exit;
  sr := TFileSyncRec(FVisibleItems.Objects[r]);
  if not Assigned(sr)
  or not Assigned(sr.FFileR) or not Assigned(sr.FFileL) or (sr.FState = srsEqual)
  then
    Exit;
  PrepareToolData(FFileSourceL, sr.FFileL, FFileSourceR, sr.FFileR, @ShowDifferByGlobList);
end;

procedure TfrmSyncDirsDlg.MainDrawGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  r: TFileSyncRec;
  x: Integer;
  s: string;
begin
  if (FVisibleItems = nil) or (aRow >= FVisibleItems.Count) then Exit;
  with MainDrawGrid.Canvas do
  begin
    r := TFileSyncRec(FVisibleItems.Objects[aRow]);
    if r = nil then
    begin
      Brush.Color := clBtnFace;
      FillRect(aRect);
      Font.Bold := True;
      Font.Color := clWindowText;
      with hCols[0] do
        TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
          Left + 2, aRect.Top + 2, FVisibleItems[aRow]);
    end else begin
      case r.FState of
      srsNotEq:       Font.Color := gSyncUnknownColor;
      srsCopyLeft:    Font.Color := gSyncRightColor;
      srsCopyRight:   Font.Color := gSyncLeftColor;
      srsDeleteLeft:  Font.Color := gSyncLeftColor;
      srsDeleteRight: Font.Color := gSyncRightColor;
      else Font.Color := clWindowText;
      end;
      if Assigned(r.FFileL) then
      begin
        with hCols[0] do
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            Left + 2, aRect.Top + 2, FVisibleItems[aRow]);
        s := IntToStr(r.FFileL.Size);
        with hCols[1] do begin
          x := Left + Width - 8 - TextWidth(s);
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            x, aRect.Top + 2, s);
        end;
        s := DateTimeToStr(r.FFileL.ModificationTime);
        with hCols[2] do
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            Left + 2, aRect.Top + 2, s)
      end;
      if Assigned(r.FFileR) then
      begin
        TextOut(hCols[6].Left + 2, aRect.Top + 2, FVisibleItems[aRow]);
        s := IntToStr(r.FFileR.Size);
        with hCols[5] do begin
          x := Left + Width - 8 - TextWidth(s);
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            x, aRect.Top + 2, s);
        end;
        s := DateTimeToStr(r.FFileR.ModificationTime);
        with hCols[4] do
          TextRect(Rect(Left, aRect.Top, Left + Width, aRect.Bottom),
            Left + 2, aRect.Top + 2, s)
      end;
      ImageList1.Draw(MainDrawGrid.Canvas,
        hCols[3].Left + (hCols[3].Width - ImageList1.Width) div 2 - 2,
        (aRect.Top + aRect.Bottom - ImageList1.Height - 1) div 2, Ord(r.FAction));
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
      UpdateSelection(MainDrawGrid.Row);
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
  if (r < 0) or (r >= FVisibleItems.Count)
  or (x - 2 < hCols[3].Left)
  or (x - 2 > hCols[3].Left + hCols[3].Width)
  then
    Exit;
  UpdateSelection(R);
end;

procedure TfrmSyncDirsDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    if FScanning then
      FCancel := True
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
  if (r < 0) or (r >= FVisibleItems.Count) then Exit;
  sr := TFileSyncRec(FVisibleItems.Objects[r]);
  if Assigned(sr) then
  begin
    if Sender = MenuItemViewLeft then
      f := sr.FFileL
    else if Sender = MenuItemViewRight then begin
      f := sr.FFileR;
    end;
    if Assigned(f) then ShowViewerByGlob(f.FullPath);
  end;
end;

procedure TfrmSyncDirsDlg.pmGridMenuPopup(Sender: TObject);
begin
  miSelectDeleteLeft.Visible := not chkAsymmetric.Checked;
  miSelectDeleteBoth.Visible := not chkAsymmetric.Checked;
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
  end;
end;

procedure TfrmSyncDirsDlg.SetSortIndex(AValue: Integer);
var
  s: string;
begin
  if AValue = FSortIndex then
  begin
    s := HeaderDG.Columns[AValue].Title.Caption;
    UTF8Delete(s, 1, 1);
    FSortDesc := not FSortDesc;
    if FSortDesc then s := '↑' + s else s := '↓' + s;
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
      Caption := '↓' + Caption;
    SortFoundItems;
    FillFoundItemsDG;
  end;
end;

procedure TfrmSyncDirsDlg.ClearFoundItems;
var
  i, j: Integer;
begin
  for i := 0 to FFoundItems.Count - 1 do
    with TStringList(FFoundItems.Objects[i]) do
    begin
      for j := 0 to Count - 1 do
        Objects[j].Free;
      Clear;
    end;
  FFoundItems.Clear;
end;

procedure TfrmSyncDirsDlg.Compare;
begin
  TopPanel.Enabled := False;
  try
    ClearFoundItems;
    MainDrawGrid.RowCount := 0;
    ScanDirs;
    FillFoundItemsDG;
    MainDrawGrid.SetFocus;
  finally
    TopPanel.Enabled := True;
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
    for i := 0 to FVisibleItems.Count - 1 do
    begin
      r := TFileSyncRec(FVisibleItems.Objects[i]);
      if Assigned(r) then
      begin
        Inc(Ftotal);
        if Assigned(r.FFileL) and not Assigned(r.FFileR) then Inc(FuniqueL) else
        if Assigned(r.FFileR) and not Assigned(r.FFileL) then Inc(FuniqueR);
        if r.FState = srsEqual then Inc(Fequal) else
        if r.FState = srsNotEq then Inc(Fnoneq) else
        if Assigned(r.FFileL) and Assigned(r.FFileR) then Inc(Fnoneq);
      end;
    end;
  end;

begin
  InitVisibleItems;
  MainDrawGrid.ColCount := 1;
  MainDrawGrid.RowCount := FVisibleItems.Count;
  MainDrawGrid.Invalidate;
  CalcStat;
  UpdateStatusBar;
  if FVisibleItems.Count > 0 then
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
var
  i, j: Integer;
  AFilter: record
    copyLeft, copyRight, eq, neq: Boolean;
    dup, single: Boolean;
  end;
  r: TFileSyncRec;

begin
  if Assigned(FVisibleItems) then
    FVisibleItems.Clear
  else begin
    FVisibleItems := TStringListUtf8.Create;
    FVisibleItems.CaseSensitive := FileNameCaseSensitive;
  end;
  { init filter }
  with AFilter do
  begin
    copyLeft := sbCopyLeft.Down;
    copyRight := sbCopyRight.Down;
    eq := sbEqual.Down;
    neq := sbNotEqual.Down;
    dup := sbDuplicates.Down;
    single := sbSingles.Down;
  end;
  for i := 0 to FFoundItems.Count - 1 do
  begin
    if FFoundItems[i] <> '' then
      FVisibleItems.Add(AppendPathDelim(FFoundItems[i]));
    with TStringList(FFoundItems.Objects[i]) do
      for j := 0 to Count - 1 do
      begin
        { check filter }
        r := TFileSyncRec(Objects[j]);
        if ((Assigned(r.FFileL) <> Assigned(r.FFileR)) and AFilter.single or
           (Assigned(r.FFileL) = Assigned(r.FFileR)) and AFilter.dup)
           and
           ((r.FState = srsCopyLeft) and AFilter.copyLeft or
            (r.FState = srsCopyRight) and AFilter.copyRight or
            (r.FState = srsDeleteLeft) and AFilter.copyRight or
            (r.FState = srsDeleteRight) and AFilter.copyLeft or
            (r.FState = srsEqual) and AFilter.eq or
            (r.FState = srsNotEq) and AFilter.neq or
            (r.FState = srsUnknown))
        then
          FVisibleItems.AddObject(Strings[j], Objects[j]);
      end;
  end;
  { remove empty dirs after filtering }
  for i := FVisibleItems.Count - 1 downto 0 do
    if (FVisibleItems.Objects[i] = nil)
    and ((i + 1 >= FVisibleItems.Count) or (FVisibleItems.Objects[i + 1] = nil)) then
      FVisibleItems.Delete(i);
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
  ignoreDate, Subdirs, ByContent: Boolean;

  procedure ScanDir(dir: string);

    procedure ProcessOneSide(it, dirs: TStringList; var ASide: Boolean; sideLeft: Boolean);
    var
      fs: TFiles;
      i, j: Integer;
      f: TFile;
      r: TFileSyncRec;
    begin
      if sideLeft then
        fs := FFileSourceL.GetFiles(BaseDirL + dir)
      else begin
        fs := FFileSourceR.GetFiles(BaseDirR + dir);
      end;
      if chkOnlySelected.Checked and ASide then
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
          if (Template = nil) or Template.CheckFile(f) then
          begin
            if f.IsDirectory then
            begin
              if (f.NameNoExt <> '.') and (f.NameNoExt <> '..') then
                dirs.Add(f.Name);
            end
            else if ((MaskList = nil) or MaskList.Matches(f.Name)) then
            begin
              j := it.IndexOf(f.Name);
              if j < 0 then
                r := TFileSyncRec.Create(Self, dir)
              else
                r := TFileSyncRec(it.Objects[j]);
              if sideLeft then
              begin
                r.FFileL := f.Clone;
                r.UpdateState(ignoreDate);
              end else begin
                r.FFileR := f.Clone;
                r.UpdateState(ignoreDate);
                if ByContent and (r.FState = srsEqual) and (r.FFileR.Size > 0) then
                begin
                  r.FAction := srsUnknown;
                  r.FState := srsUnknown;
                end;
              end;
              it.AddObject(f.Name, r);
            end;
          end;
        end;
      finally
        fs.Free;
      end;
    end;

  var
    i, j, tot: Integer;
    it: TStringList;
    dirsLeft, dirsRight: TStringListUtf8;
    d: string;
  begin
    i := FFoundItems.IndexOf(dir);
    if i < 0 then
    begin
      it := TStringListUtf8.Create;
      it.CaseSensitive := FileNameCaseSensitive;
      it.Sorted := True;
      FFoundItems.AddObject(dir, it);
    end else
      it := TStringList(FFoundItems.Objects[i]);
    if dir <> '' then dir := AppendPathDelim(dir);
    dirsLeft := TStringListUtf8.Create;
    dirsLeft.CaseSensitive := FileNameCaseSensitive;
    dirsLeft.Sorted := True;
    dirsRight := TStringListUtf8.Create;
    dirsRight.CaseSensitive := FileNameCaseSensitive;
    dirsRight.Sorted := True;
    try
      Application.ProcessMessages;
      if FCancel then Exit;
      ProcessOneSide(it, dirsLeft, LeftFirst, True);
      ProcessOneSide(it, dirsRight, RightFirst, False);
      SortFoundItems(it);
      if not Subdirs then Exit;
      tot := dirsLeft.Count + dirsRight.Count;
      for i := 0 to dirsLeft.Count - 1 do
      begin
        if dir = '' then
          StatusBar1.Panels[0].Text :=
            Format(rsComparingPercent, [i * 100 div tot]);
        d := dirsLeft[i];
        ScanDir(dir + d);
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
        ScanDir(dir + d);
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
    MaskList := TMaskList.Create(cbExtFilter.Text);
  end;
  if (FAddressL <> '') and (Copy(BaseDirL, 1, Length(FAddressL)) = FAddressL) then
    Delete(BaseDirL, 1, Length(FAddressL));
  BaseDirR := AppendPathDelim(edPath2.Text);
  if (FAddressR <> '') and (Copy(BaseDirR, 1, Length(FAddressR)) = FAddressR) then
    Delete(BaseDirR, 1, Length(FAddressR));
  FCmpFilePathL := BaseDirL;
  FCmpFilePathR := BaseDirR;
  ignoreDate := chkIgnoreDate.Checked;
  Subdirs := chkSubDirs.Checked;
  ByContent := chkByContent.Checked;
  if chkAsymmetric.Checked then
    FFileExists:= srsDeleteRight
  else begin
    FFileExists:= srsCopyLeft;
  end;
  ScanDir('');
  MaskList.Free;
  if FCancel then Exit;
  if (FFoundItems.Count > 0) and chkByContent.Checked then
    CheckContentThread := TCheckContentThread.Create(Self);
  finally
  FScanning := False;
  end;
end;

procedure TfrmSyncDirsDlg.SortFoundItems;
var
  i: Integer;
begin
  if FSortIndex < 0 then Exit;
  for i := 0 to FFoundItems.Count - 1 do
    SortFoundItems(TStringList(FFoundItems.Objects[i]));
end;

procedure TfrmSyncDirsDlg.SortFoundItems(sl: TStringList);

  function CompareFn(sl: TStringList; i, j: Integer): Integer;
  var
    r1, r2: TFileSyncRec;
  begin
    r1 := TFileSyncRec(sl.Objects[i]);
    r2 := TFileSyncRec(sl.Objects[j]);
    case FSortIndex of
    0:
      Result := UTF8CompareStr(sl[i], sl[j]);
    1:
      if (Assigned(r1.FFileL) < Assigned(r2.FFileL))
      or Assigned(r2.FFileL) and (r1.FFileL.Size < r2.FFileL.Size) then
        Result := -1
      else
      if (Assigned(r1.FFileL) > Assigned(r2.FFileL))
      or Assigned(r1.FFileL) and (r1.FFileL.Size > r2.FFileL.Size) then
        Result := 1
      else
        Result := 0;
    2:
      if (Assigned(r1.FFileL) < Assigned(r2.FFileL))
      or Assigned(r2.FFileL)
      and (r1.FFileL.ModificationTime < r2.FFileL.ModificationTime) then
        Result := -1
      else
      if (Assigned(r1.FFileL) > Assigned(r2.FFileL))
      or Assigned(r1.FFileL)
      and (r1.FFileL.ModificationTime > r2.FFileL.ModificationTime) then
        Result := 1
      else
        Result := 0;
    4:
      if (Assigned(r1.FFileR) < Assigned(r2.FFileR))
      or Assigned(r2.FFileR)
      and (r1.FFileR.ModificationTime < r2.FFileR.ModificationTime) then
        Result := -1
      else
      if (Assigned(r1.FFileR) > Assigned(r2.FFileR))
      or Assigned(r1.FFileR)
      and (r1.FFileR.ModificationTime > r2.FFileR.ModificationTime) then
        Result := 1
      else
        Result := 0;
    5:
      if (Assigned(r1.FFileR) < Assigned(r2.FFileR))
      or Assigned(r2.FFileR) and (r1.FFileR.Size < r2.FFileR.Size) then
        Result := -1
      else
      if (Assigned(r1.FFileR) > Assigned(r2.FFileR))
      or Assigned(r1.FFileR) and (r1.FFileR.Size > r2.FFileR.Size) then
        Result := 1
      else
        Result := 0;
    6:
      Result := UTF8CompareStr(sl[i], sl[j]);
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

procedure TfrmSyncDirsDlg.UpdateSelection(R: Integer);
var
  sr: TFileSyncRec;
  ca: TSyncRecState;
begin
  sr := TFileSyncRec(FVisibleItems.Objects[r]);
  if not Assigned(sr) or (sr.FState = srsEqual) then Exit;
  ca := sr.FAction;
  case ca of
  srsNotEq:
    ca := srsCopyRight;
  srsCopyRight:
    if Assigned(sr.FFileR) then
      ca := srsCopyLeft
    else
      ca := srsDoNothing;
  srsCopyLeft:
    if Assigned(sr.FFileL) then
      ca := srsNotEq
    else
      ca := srsDoNothing;
  srsDeleteRight:
    if not chkAsymmetric.Checked then
      ca := sr.FState
    else
      ca := srsDoNothing;
  srsDeleteLeft:
    ca := sr.FState;
  srsDeleteBoth:
    ca := sr.FState;
  srsDoNothing:
    if Assigned(sr.FFileL) then
      ca := srsCopyRight
    else
      ca := FFileExists;
  end;
  sr.FAction := ca;
  MainDrawGrid.InvalidateRow(r);
end;

procedure TfrmSyncDirsDlg.EnableControls(AEnabled: Boolean);
begin
  edPath1.Enabled:= AEnabled;
  edPath2.Enabled:= AEnabled;
  TopPanel.Enabled:= AEnabled;
  pnlFilter.Enabled:= AEnabled;
  MainDrawGrid.Enabled:= AEnabled;
  pnlProgress.Visible:= not AEnabled;
  Timer.Enabled:= not AEnabled;
end;

procedure TfrmSyncDirsDlg.SetSyncRecState(AState: TSyncRecState);
var
  R, Y: Integer;
  Selection: TGridRect;
  SyncRec: TFileSyncRec;

  procedure UpdateAction(NewAction: TSyncRecState);
  begin
    case NewAction of
      srsUnknown:
        NewAction:= SyncRec.FState;
      srsNotEq:
        begin
          if (SyncRec.FAction = srsCopyLeft) and Assigned(SyncRec.FFileL) then
              NewAction:= srsCopyRight
          else if (SyncRec.FAction = srsCopyRight) and Assigned(SyncRec.FFileR) then
              NewAction:= srsCopyLeft
          else
            NewAction:= SyncRec.FAction
        end;
      srsCopyLeft:
        begin
          if not Assigned(SyncRec.FFileR) then
            NewAction:= srsDoNothing;
        end;
      srsCopyRight:
        begin
          if not Assigned(SyncRec.FFileL) then
            NewAction:= srsDoNothing;
        end;
      srsDeleteLeft:
        begin
          if not Assigned(SyncRec.FFileL) then
            NewAction:= srsDoNothing;
        end;
      srsDeleteRight:
        begin
          if not Assigned(SyncRec.FFileR) then
            NewAction:= srsDoNothing;
        end;
      srsDeleteBoth:
        begin
          if not Assigned(SyncRec.FFileL) then
            NewAction:= srsDeleteRight;
          if not Assigned(SyncRec.FFileR) then
            NewAction:= srsDeleteLeft;
        end;
    end;
    SyncRec.FAction:= NewAction;
    MainDrawGrid.InvalidateRow(R);
  end;

begin
  Selection:= MainDrawGrid.Selection;
  if (MainDrawGrid.HasMultiSelection) or (Selection.Bottom <> Selection.Top) then
  begin
    for Y:= 0 to MainDrawGrid.SelectedRangeCount - 1 do
    begin
      Selection:= MainDrawGrid.SelectedRange[Y];
      for R := Selection.Top to Selection.Bottom do
      begin
        SyncRec := TFileSyncRec(FVisibleItems.Objects[R]);
        if Assigned(SyncRec) then UpdateAction(AState);
      end;
    end;
    Exit;
  end;
  R := MainDrawGrid.Row;
  if (R < 0) or (R >= FVisibleItems.Count) then Exit;
  SyncRec := TFileSyncRec(FVisibleItems.Objects[r]);
  if Assigned(SyncRec) then
  begin
    UpdateAction(AState);
  end
  else begin
    Inc(R);
    while R < FVisibleItems.Count do
    begin
      SyncRec := TFileSyncRec(FVisibleItems.Objects[R]);
      if (SyncRec = nil) then Break;
      UpdateAction(AState);
      Inc(R);
    end;
  end;
end;

procedure TfrmSyncDirsDlg.DeleteFiles(ALeft, ARight: Boolean);
var
  Message: String;
  ALeftList: TFiles;
  ARightList: TFiles;
begin
  if not ALeft then
    ALeftList:= nil
  else begin
    ALeftList:= TFiles.Create(EmptyStr);
  end;

  if not ARight then
    ARightList:= nil
  else begin
    ARightList:= TFiles.Create(EmptyStr);
  end;

  try
    Message:= EmptyStr;
    UpdateList(ALeftList, ARightList, False, False);

    ALeft:= ALeft and (ALeftList.Count > 0);
    ARight:= ARight and (ARightList.Count > 0);

    if (ALeft = False) and (ARight = False) then Exit;

    FDeleteStatistics.DoneFiles:= 0;
    FDeleteStatistics.TotalFiles:= 0;

    if ALeft then
    begin
      FDeleteStatistics.TotalFiles+= ALeftList.Count;
      Message:= Format(rsVarLeftPanel + ': ' + rsMsgDelFlDr, [ALeftList.Count]) + LineEnding;
    end;

    if ARight then
    begin
      FDeleteStatistics.TotalFiles+= ARightList.Count;
      Message+= Format(rsVarRightPanel + ': ' + rsMsgDelFlDr, [ARightList.Count]) + LineEnding;
    end;

    if MessageDlg(Message, mtWarning, [mbYes, mbNo], 0, mbYes) = mrYes then
    begin
      EnableControls(False);
      pnlCopyProgress.Visible:= False;
      pnlDeleteProgress.Visible:= True;
      if ALeft then DeleteFiles(FCmpFileSourceL, ALeftList);
      if ARight then DeleteFiles(FCmpFileSourceR, ARightList);
      UpdateList(nil, nil, ALeft, ARight);
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

procedure TfrmSyncDirsDlg.UpdateList(ALeft, ARight: TFiles; ARemoveLeft,
  ARemoveRight: Boolean);
var
  R, Y: Integer;
  ARemove: Boolean;
  Selection: TGridRect;
  SyncRec: TFileSyncRec;

  procedure AddRemoveItem;
  begin
    if Assigned(ALeft) and Assigned(SyncRec.FFileL) then
      ALeft.Add(SyncRec.FFileL.Clone);

    if Assigned(ARight) and Assigned(SyncRec.FFileR) then
      ARight.Add(SyncRec.FFileR.Clone);

    if ARemove then
    begin
      if ARemoveLeft and Assigned(SyncRec.FFileL) then
        FreeAndNil(SyncRec.FFileL);
      if ARemoveRight and Assigned(SyncRec.FFileR) then
        FreeAndNil(SyncRec.FFileR);

      if Assigned(SyncRec.FFileL) or Assigned(SyncRec.FFileR) then
        SyncRec.UpdateState(chkIgnoreDate.Checked)
      else begin
        MainDrawGrid.DeleteRow(R);
        FVisibleItems.Delete(R);
      end;
    end;
  end;

begin
  Selection:= MainDrawGrid.Selection;
  ARemove:= ARemoveLeft or ARemoveRight;
  if (MainDrawGrid.HasMultiSelection) or (Selection.Bottom <> Selection.Top) then
  begin
    if ARemove then MainDrawGrid.BeginUpdate;
    for Y:= 0 to MainDrawGrid.SelectedRangeCount - 1 do
    begin
      Selection:= MainDrawGrid.SelectedRange[Y];
      for R := Selection.Bottom downto Selection.Top do
      begin
        SyncRec := TFileSyncRec(FVisibleItems.Objects[R]);
        if Assigned(SyncRec) then AddRemoveItem;
      end;
    end;
    if ARemove then MainDrawGrid.EndUpdate;
    Exit;
  end;
  R := MainDrawGrid.Row;
  if (R < 0) or (R >= FVisibleItems.Count) then Exit;
  SyncRec := TFileSyncRec(FVisibleItems.Objects[r]);
  if ARemove then MainDrawGrid.BeginUpdate;
  if Assigned(SyncRec) then
  begin
    AddRemoveItem;
  end
  else begin
    Y:= R;
    Inc(R);
    while R < FVisibleItems.Count do
    begin
      if (FVisibleItems.Objects[R] = nil) then Break;
      Inc(R);
    end;
    Dec(R);
    while R > Y do
    begin
      SyncRec := TFileSyncRec(FVisibleItems.Objects[R]);
      AddRemoveItem;
      Dec(R);
    end;
  end;
  if ARemove then MainDrawGrid.EndUpdate;
end;

procedure TfrmSyncDirsDlg.SetProgressBytes(AProgressBar: TKASProgressBar;
  CurrentBytes: Int64; TotalBytes: Int64);
begin
  AProgressBar.SetProgress(CurrentBytes, TotalBytes,
                           cnvFormatFileSize(CurrentBytes, uoscOperation) + '/' +
                           cnvFormatFileSize(TotalBytes, uoscOperation)
                           );
end;

procedure TfrmSyncDirsDlg.SetProgressFiles(AProgressBar: TKASProgressBar;
  CurrentFiles: Int64; TotalFiles: Int64);
begin
  AProgressBar.SetProgress(CurrentFiles, TotalFiles,
                           cnvFormatFileSize(CurrentFiles, uoscNoUnit) + '/' +
                           cnvFormatFileSize(TotalFiles, uoscNoUnit)
                           );
end;

constructor TfrmSyncDirsDlg.Create(AOwner: TComponent; FileView1,
  FileView2: TFileView);
var
  Index: Integer;
  AFiles: TFiles;
begin
  inherited Create(AOwner);
  FFoundItems := TStringListUtf8.Create;
  FFoundItems.CaseSensitive := FileNameCaseSensitive;
  FFoundItems.Sorted := True;
  FFileSourceL := FileView1.FileSource;
  FFileSourceR := FileView2.FileSource;
  FAddressL := FileView1.CurrentAddress;
  FAddressR := FileView2.CurrentAddress;
  with FileView1 do
    edPath1.Text := FAddressL + CurrentPath;
  with FileView2 do
    edPath2.Text := FAddressR + CurrentPath;
  RecalcHeaderCols;
  MainDrawGrid.DoubleBuffered := True;
  MainDrawGrid.Font.Bold := True;
  FSortIndex := -1;
  SortIndex := 0;
  FScanning := False;
  FSortDesc := False;
  MainDrawGrid.RowCount := 0;
  // ---------------------------------------------------------------------------
  FSelectedItems := TStringListUtf8.Create;
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
  if (FFileSourceL.IsClass(TFileSystemFileSource)) and (FFileSourceR.IsClass(TFileSystemFileSource)) then
  begin
    FNtfsShift := gNtfsHourTimeDelay and NtfsHourTimeDelay(FileView1.CurrentPath, FileView2.CurrentPath);
  end;
end;

destructor TfrmSyncDirsDlg.Destroy;
begin
  HotMan.UnRegister(Self);
  FFileSourceOperationMessageBoxesUI.Free;
  FVisibleItems.Free;
  FSelectedItems.Free;
  if Assigned(FFoundItems) then
  begin
    ClearFoundItems;
    FFoundItems.Free;
  end;
  inherited Destroy;
end;

procedure TfrmSyncDirsDlg.CopyToClipboard;
var
  sl: TStringList;
  RowList: TIntegerList;
  I: Integer;

  procedure FillRowList(RowList: TIntegerList);
  var
    R, Y: Integer;
    Selection: TGridRect;
  begin
    Selection := MainDrawGrid.Selection;
    if (MainDrawGrid.HasMultiSelection) or (Selection.Bottom <> Selection.Top) then
    begin
      for Y:= 0 to MainDrawGrid.SelectedRangeCount - 1 do
      begin
        Selection:= MainDrawGrid.SelectedRange[Y];
        for R := Selection.Top to Selection.Bottom do
        begin
          if RowList.IndexOf(R) = -1 then
          begin
            RowList.Add(R);
          end;
        end;
      end;
    end
    else
    begin
      R := MainDrawGrid.Row;
      if RowList.IndexOf(R) = -1 then
      begin
        RowList.Add(R);
      end;
    end;
    RowList.Sort;
  end;

  procedure PrintRow(R: Integer);
  var
    s: string;
    SyncRec: TFileSyncRec;
  begin
    s := '';
    SyncRec := TFileSyncRec(FVisibleItems.Objects[R]);
    if not Assigned(SyncRec) then
    begin
      s := s + FVisibleItems[R];
    end
    else
    begin
      if Assigned(SyncRec.FFileL) then
      begin
        s := s + FVisibleItems[R];
        s := s + #9;
        s := s + IntToStr(SyncRec.FFileL.Size);
        s := s + #9;
        s := s + DateTimeToStr(SyncRec.FFileL.ModificationTime);
      end;
      if Length(s) <> 0 then
        s := s + #9;
      case SyncRec.FState of
        srsUnknown:
          s := s + '?';
        srsEqual:
          s := s + '=';
        srsNotEq:
          s := s + '!=';
        srsCopyLeft:
          s := s + '<-';
        srsCopyRight:
          s := s + '->';
      end;
      if Length(s) <> 0 then
        s := s + #9;
      if Assigned(SyncRec.FFileR) then
      begin
        s := s + DateTimeToStr(SyncRec.FFileR.ModificationTime);
        s := s + #9;
        s := s + IntToStr(SyncRec.FFileR.Size);
        s := s + #9;
        s := s + FVisibleItems[R];
      end;
    end;
    sl.Add(s);
  end;
begin
  sl := TStringList.Create;
  RowList := TIntegerList.Create;
  try
    FillRowList(RowList);
    for I := 0 to RowList.Count - 1 do
    begin
      PrintRow(RowList[I]);
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
  SetSyncRecState(srsCopyRight);
end;

procedure TfrmSyncDirsDlg.cm_SelectCopyRightToLeft(const Params: array of string);
begin
  SetSyncRecState(srsCopyLeft);
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

