{
   Seksi Commander
   ----------------------------
   Implementing of progress dialog for file operation

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:

   Copyright (C) 2008-2010  Koblov Alexander (Alexx2000@mail.ru)
}

unit fFileOpDlg;

{$mode objfpc}{$H+}

interface

uses
  LResources,
  SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls, Buttons, ExtCtrls,
  uOperationsManager, uFileSourceOperation, uFileSourceOperationUI, fViewOperations;

type

  TFileOpDlgLook = set of (fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb);


  { TfrmFileOp }

  TfrmFileOp = class(TForm)
    btnPauseStart: TBitBtn;
    btnWorkInBackground: TButton;
    lblFrom: TLabel;
    lblTo: TLabel;
    lblFileNameTo: TLabel;
    pbSecond: TProgressBar;
    pbFirst: TProgressBar;
    lblFileNameFrom: TLabel;
    lblEstimated: TLabel;
    btnCancel: TBitBtn;
    btnToQueue: TSpeedButton;
    btnOpHome: TSpeedButton;
    bthOpEnd: TSpeedButton;
    procedure bthOpEndClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOpHomeClick(Sender: TObject);
    procedure btnPauseStartClick(Sender: TObject);
    procedure btnToQueueClick(Sender: TObject);
    procedure btnWorkInBackgroundClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
    FOperationHandle: TOperationHandle;
    FUpdateTimer: TTimer;  //<en Timer for updating statistics.
    FUserInterface: TFileSourceOperationUI;
    FStopOperationOnClose: Boolean;

    procedure OnUpdateTimer(Sender: TObject);

    procedure InitializeControls(FileOpDlgLook: TFileOpDlgLook);
    procedure SetPauseGlyph;
    procedure SetPlayGlyph;
    procedure UpdatePauseStartButton(OperationState: TFileSourceOperationState);
    procedure SetProgress(ProgressBar: TProgressBar; CurrentValue: Int64; MaxValue: Int64; BarText: String = '');
    procedure SetProgressBytes(ProgressBar: TProgressBar; CurrentBytes: Int64; TotalBytes: Int64);
    procedure SetSpeedAndTime(Operation: TFileSourceOperation; RemainingTime: TDateTime; Speed: String);

    procedure InitializeCopyOperation(Operation: TFileSourceOperation);
    procedure InitializeMoveOperation(Operation: TFileSourceOperation);
    procedure InitializeDeleteOperation(Operation: TFileSourceOperation);
    procedure InitializeWipeOperation(Operation: TFileSourceOperation);
    procedure InitializeCalcChecksumOperation(Operation: TFileSourceOperation);
    procedure InitializeTestArchiveOperation(Operation: TFileSourceOperation);
    procedure UpdateCopyOperation(Operation: TFileSourceOperation);
    procedure UpdateMoveOperation(Operation: TFileSourceOperation);
    procedure UpdateDeleteOperation(Operation: TFileSourceOperation);
    procedure UpdateWipeOperation(Operation: TFileSourceOperation);
    procedure UpdateCalcChecksumOperation(Operation: TFileSourceOperation);
    procedure UpdateTestArchiveOperation(Operation: TFileSourceOperation);

  public


    // Change to override later.
    constructor Create(OperationHandle: TOperationHandle); overload;
    destructor Destroy; override;

    function CloseQuery: Boolean; override;

    procedure ToggleProgressBarStyle;

  end;

implementation

{$R *.lfm}

uses
   dmCommonData, uLng, uDCUtils,
   uFileSourceOperationTypes,
   uFileSourceCopyOperation,
   uFileSourceMoveOperation,
   uFileSourceDeleteOperation,
   uFileSourceWipeOperation,
   uFileSourceCalcChecksumOperation,
   uFileSourceTestArchiveOperation,
   uFileSourceOperationMessageBoxesUI
   {$IFDEF LCLGTK2}
   , Gtk2
   {$ENDIF}
   {$IFDEF LCLQT}
   , qt4, qtwidgets
   {$ENDIF}
   ;

procedure TfrmFileOp.btnCancelClick(Sender: TObject);
var
  Operation: TFileSourceOperation;
begin
  Operation := OperationsManager.GetOperationByHandle(FOperationHandle);
  if Assigned(Operation) then
  begin
    Operation.Stop;
  end;
  ModalResult:= mrCancel;
end;

procedure TfrmFileOp.bthOpEndClick(Sender: TObject);
begin
  OperationsManager.MoveOperation(indexFocus, OperationsManager.OperationsCount-1);
  indexFocus:= OperationsManager.OperationsCount-1;
end;

procedure TfrmFileOp.btnOpHomeClick(Sender: TObject);
begin
  OperationsManager.MoveOperation(indexFocus, 0);
  indexFocus:= 0;
end;

procedure TfrmFileOp.btnPauseStartClick(Sender: TObject);
var
  Operation: TFileSourceOperation;
begin
  Operation := OperationsManager.GetOperationByHandle(FOperationHandle);
  if Assigned(Operation) then
  begin
    if Operation.State in [fsosNotStarted, fsosPaused] then
    begin
      Operation.Start;
      SetPauseGlyph;
    end
    else if Operation.State in [fsosRunning, fsosWaitingForConnection] then
    begin
      Operation.Pause;
      SetPlayGlyph;
      OperationsManager.CheckQueuedOperations;
    end;
  end;
end;

procedure TfrmFileOp.btnToQueueClick(Sender: TObject);
var
Operation: TFileSourceOperation;
begin

  Operation := OperationsManager.GetOperationByHandle(FOperationHandle);
  if Assigned(Operation) then
  begin
    OperationsManager.SetFormCreate (FOperationHandle, false);
    OperationsManager.InQueue(FOperationHandle, true);
  end;
  FStopOperationOnClose := False;
  Close;
  OperationsManager.CheckQueuedOperations;
end;

procedure TfrmFileOp.btnWorkInBackgroundClick(Sender: TObject);
var
Operation: TFileSourceOperation;
begin
  Operation := OperationsManager.GetOperationByHandle(FOperationHandle);
  if Assigned(Operation) then   OperationsManager.SetFormCreate (FOperationHandle, false);
  FStopOperationOnClose := False;
  Close;
  OperationsManager.CheckQueuedOperations;
end;

procedure TfrmFileOp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmFileOp.FormCreate(Sender: TObject);
var
  Operation: TFileSourceOperation;
begin
  pbFirst.DoubleBuffered:= True;
  pbSecond.DoubleBuffered:= True;
  Self.DoubleBuffered:= True;

  {$IFDEF LCLGTK2}
  // Have to disable LCLGTK2 default progress bar text
  // set in TGtk2WSProgressBar.UpdateProgressBarText.
  pbFirst.BarShowText := False;
  pbSecond.BarShowText := False;
  {$ENDIF}

  Operation := OperationsManager.GetOperationByHandle(FOperationHandle);
  if Assigned(Operation) then
  begin

    case Operation.ID of

      fsoCopy, fsoCopyIn, fsoCopyOut:
        InitializeCopyOperation(Operation);
      fsoMove:
        InitializeMoveOperation(Operation);
      fsoDelete:
        InitializeDeleteOperation(Operation);
      fsoWipe:
        InitializeWipeOperation(Operation);
      fsoCalcChecksum:
        InitializeCalcChecksumOperation(Operation);
      fsoTestArchive:
        InitializeTestArchiveOperation(Operation);

      else
        begin
          Caption := 'Unknown operation';
          InitializeControls([fodl_first_pb]);
        end;
    end;

    UpdatePauseStartButton(Operation.State);
  end
  else
  begin
    Caption := 'Invalid operation';
  end;

  FUpdateTimer := TTimer.Create(Self);
  FUpdateTimer.Interval := 100;
  FUpdateTimer.OnTimer := @OnUpdateTimer;
  FUpdateTimer.Enabled := True;

  OperationsManager.SetFormCreate (FOperationHandle, true);
end;


constructor TfrmFileOp.Create(OperationHandle: TOperationHandle);
var
  Operation: TFileSourceOperation;
begin
  FOperationHandle := OperationHandle;

//  if not Assigned(frmFileOp(FOperationHandle)) then
//  begin


  inherited Create(Application);

  AutoSize := True;

  FStopOperationOnClose := True;

  Operation := OperationsManager.GetOperationByHandle(FOperationHandle);
  if Assigned(Operation) then
  begin
    FUserInterface := TFileSourceOperationMessageBoxesUI.Create;
    Operation.AddUserInterface(FUserInterface);
  end
  else
    FUserInterface := nil;

end;

destructor TfrmFileOp.Destroy;
var
  Operation: TFileSourceOperation;
begin
  inherited Destroy;

  if Assigned(FUserInterface) then
  begin
    Operation := OperationsManager.GetOperationByHandle(FOperationHandle);
    if Assigned(Operation) then
      Operation.RemoveUserInterface(FUserInterface);

    FreeAndNil(FUserInterface);
  end;
end;

function TfrmFileOp.CloseQuery: Boolean;
var
  Operation: TFileSourceOperation;
begin
  Result := True;

  if FStopOperationOnClose then
  begin
    Operation := OperationsManager.GetOperationByHandle(FOperationHandle);
    if Assigned(Operation) and (Operation.State <> fsosStopped) then
    begin
      Result := False;
      Operation.Stop;
    end
  end;
end;

procedure TfrmFileOp.OnUpdateTimer(Sender: TObject);
var
  Operation: TFileSourceOperation;
  NewCaption: String;
begin
  Operation := OperationsManager.GetOperationByHandle(FOperationHandle);
  if Assigned(Operation) and (Operation.State <> fsosStopped) then
  begin
    case Operation.ID of

      fsoCopy, fsoCopyIn, fsoCopyOut:
        UpdateCopyOperation(Operation);
      fsoMove:
        UpdateMoveOperation(Operation);
      fsoDelete:
        UpdateDeleteOperation(Operation);
      fsoWipe:
        UpdateWipeOperation(Operation);
      fsoCalcChecksum:
        UpdateCalcChecksumOperation(Operation);
      fsoTestArchive:
        UpdateTestArchiveOperation(Operation);

      else
      begin
        // Operation not currently supported for display.
        // Only show general progress.
        pbFirst.Position := Operation.Progress;
      end;
    end;

    UpdatePauseStartButton(Operation.State);

    NewCaption := IntToStr(Operation.Progress) + '% ' + Hint;
    if Operation.State <> fsosRunning then
      NewCaption := NewCaption + ' [' + FileSourceOperationStateText[Operation.State] + ']';
    Caption := NewCaption;
  end
  else
  begin
    // Operation has finished.
    // if CloseOnFinish then
    Close;
    // if BeepOnFinish then Beep;
  end;
end;

procedure TfrmFileOp.InitializeControls(FileOpDlgLook: TFileOpDlgLook);
begin
  lblFrom.Visible         := fodl_from_lbl in FileOpDlgLook;
  lblFileNameFrom.Visible := fodl_from_lbl in FileOpDlgLook;
  lblTo.Visible           := fodl_to_lbl in FileOpDlgLook;
  lblFileNameTo.Visible   := fodl_to_lbl in FileOpDlgLook;
  pbFirst.Visible         := fodl_first_pb in FileOpDlgLook;
  pbSecond.Visible        := fodl_second_pb in FileOpDlgLook;

  lblFileNameFrom.Caption := '';
  lblFileNameTo.Caption := '';
  lblEstimated.Caption := #32;

  Hint := Caption;
end;

procedure TfrmFileOp.SetPauseGlyph;
begin
  dmComData.ImageList.GetBitmap(1, btnPauseStart.Glyph);
end;

procedure TfrmFileOp.SetPlayGlyph;
begin
  dmComData.ImageList.GetBitmap(0, btnPauseStart.Glyph);
end;

procedure TfrmFileOp.UpdatePauseStartButton(OperationState: TFileSourceOperationState);
begin
  case OperationState of
    fsosNotStarted, fsosStopped, fsosPaused:
      begin
        btnPauseStart.Enabled := True;
        SetPlayGlyph;
      end;

    fsosStarting, fsosStopping, fsosPausing, fsosWaitingForFeedback:
      begin
        btnPauseStart.Enabled := False;
      end;

    fsosRunning, fsosWaitingForConnection:
      begin
        btnPauseStart.Enabled := True;
        SetPauseGlyph;
      end;

    else
      btnPauseStart.Enabled := False;
  end;
end;

procedure TfrmFileOp.SetProgress(ProgressBar: TProgressBar; CurrentValue: Int64; MaxValue: Int64; BarText: String);
{$IFDEF LCLGTK2}
var
  wText: String;
{$ENDIF}
{$IFDEF LCLQT}
var
  wText: WideString;
{$ENDIF}
begin
  if MaxValue <> 0 then
    ProgressBar.Position := (CurrentValue * 100) div MaxValue
  else
    ProgressBar.Position := 0;

{$IFDEF LCLGTK2}
{
  %v - the current progress value.
  %l - the lower bound for the progress value.
  %u - the upper bound for the progress value.
  %p - the current progress percentage.
}
  if BarText <> '' then
    wText := BarText + ' (%p%%)'
  else
    wText := '%p%%';
  gtk_progress_set_format_string(PGtkProgress(ProgressBar.Handle), PChar(wText));
  // Have to reset 'show_text' every time because LCLGTK2 will set it according to BarShowText.
  gtk_progress_set_show_text(PGtkProgress(ProgressBar.Handle), True);
{$ENDIF}
{$IFDEF LCLQT}
{
  %p - is replaced by the percentage completed.
  %v - is replaced by the current value.
  %m - is replaced by the total number of steps.
}
  if BarText <> '' then
    wText := WideString(BarText) + ' (%p%)'
  else
    wText := '%p%';
  QProgressBar_setFormat(QProgressBarH(TQtProgressBar(ProgressBar.Handle).Widget), @wText);
  //QProgressBar_setTextVisible(QProgressBarH(TQtProgressBar(ProgressBar.Handle).Widget), True);
{$ENDIF}
end;

procedure TfrmFileOp.SetProgressBytes(ProgressBar: TProgressBar; CurrentBytes: Int64; TotalBytes: Int64);
begin
  SetProgress(ProgressBar, CurrentBytes, TotalBytes,
    cnvFormatFileSize(CurrentBytes, True) + 'B/' +
    cnvFormatFileSize(TotalBytes, True) + 'B');
end;

procedure TfrmFileOp.SetSpeedAndTime(Operation: TFileSourceOperation; RemainingTime: TDateTime; Speed: String);
var
  sEstimated: String;
begin
  if Operation.State <> fsosRunning then
    sEstimated := #32
  else
  begin
    sEstimated := FormatDateTime('HH:MM:SS', RemainingTime);
    sEstimated := Format(rsDlgSpeedTime, [Speed, sEstimated]);
  end;

  lblEstimated.Caption := sEstimated;
end;

procedure TfrmFileOp.InitializeCopyOperation(Operation: TFileSourceOperation);
begin
  Caption := rsDlgCp;
  InitializeControls([fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb]);
end;

procedure TfrmFileOp.InitializeMoveOperation(Operation: TFileSourceOperation);
begin
  Caption := rsDlgMv;
  InitializeControls([fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb]);
end;

procedure TfrmFileOp.InitializeDeleteOperation(Operation: TFileSourceOperation);
begin
  Caption := rsDlgDel;
  InitializeControls([fodl_from_lbl, fodl_first_pb]);
  lblFrom.Caption := rsDlgDeleting;
end;

procedure TfrmFileOp.InitializeWipeOperation(Operation: TFileSourceOperation);
begin
  Caption := rsDlgWipe;
  InitializeControls([fodl_from_lbl, fodl_first_pb, fodl_second_pb]);
  lblFrom.Caption := rsDlgDeleting;
end;

procedure TfrmFileOp.InitializeCalcChecksumOperation(Operation: TFileSourceOperation);
begin
  Caption := rsDlgCheckSumCalc;
  InitializeControls([fodl_from_lbl, fodl_first_pb, fodl_second_pb]);
  lblFrom.Visible := False;
end;

procedure TfrmFileOp.InitializeTestArchiveOperation(Operation: TFileSourceOperation);
begin
  Caption := rsDlgTest;
  InitializeControls([fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb]);
end;

procedure TfrmFileOp.UpdateCopyOperation(Operation: TFileSourceOperation);
var
  CopyOperation: TFileSourceCopyOperation;
  CopyStatistics: TFileSourceCopyOperationStatistics;
begin
  CopyOperation := Operation as TFileSourceCopyOperation;
  CopyStatistics := CopyOperation.RetrieveStatistics;

  with CopyStatistics do
  begin
    lblFileNameFrom.Caption := CurrentFileFrom;
    lblFileNameTo.Caption := CurrentFileTo;

    SetProgressBytes(pbFirst, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(pbSecond, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, True) + 'B');
  end;
end;

procedure TfrmFileOp.UpdateMoveOperation(Operation: TFileSourceOperation);
var
  MoveOperation: TFileSourceMoveOperation;
  MoveStatistics: TFileSourceMoveOperationStatistics;
begin
  MoveOperation := Operation as TFileSourceMoveOperation;
  MoveStatistics := MoveOperation.RetrieveStatistics;

  with MoveStatistics do
  begin
    lblFileNameFrom.Caption := CurrentFileFrom;
    lblFileNameTo.Caption := CurrentFileTo;

    SetProgressBytes(pbFirst, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(pbSecond, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, True) + 'B');
  end;
end;

procedure TfrmFileOp.UpdateDeleteOperation(Operation: TFileSourceOperation);
var
  DeleteOperation: TFileSourceDeleteOperation;
  DeleteStatistics: TFileSourceDeleteOperationStatistics;
begin
  DeleteOperation := Operation as TFileSourceDeleteOperation;
  DeleteStatistics := DeleteOperation.RetrieveStatistics;

  with DeleteStatistics do
  begin
    lblFileNameFrom.Caption := CurrentFile;

    SetProgress(pbFirst, DoneFiles, TotalFiles,
      cnvFormatFileSize(DoneFiles, True) + '/' +
      cnvFormatFileSize(TotalFiles, True));
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(FilesPerSecond, True));
  end;
end;

procedure TfrmFileOp.UpdateWipeOperation(Operation: TFileSourceOperation);
var
  WipeOperation: TFileSourceWipeOperation;
  WipeStatistics: TFileSourceWipeOperationStatistics;
begin
  WipeOperation := Operation as TFileSourceWipeOperation;
  WipeStatistics := WipeOperation.RetrieveStatistics;

  with WipeStatistics do
  begin
    lblFileNameFrom.Caption := CurrentFile;

    SetProgressBytes(pbFirst, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(pbSecond, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, True) + 'B');
  end;
end;

procedure TfrmFileOp.UpdateCalcChecksumOperation(Operation: TFileSourceOperation);
var
  CalcChecksumOperation: TFileSourceCalcChecksumOperation;
  CalcChecksumStatistics: TFileSourceCalcChecksumOperationStatistics;
begin
  CalcChecksumOperation := Operation as TFileSourceCalcChecksumOperation;
  CalcChecksumStatistics := CalcChecksumOperation.RetrieveStatistics;

  with CalcChecksumStatistics do
  begin
    lblFileNameFrom.Caption := CurrentFile;

    SetProgressBytes(pbFirst, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(pbSecond, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, True) + 'B');
  end;
end;

procedure TfrmFileOp.UpdateTestArchiveOperation(Operation: TFileSourceOperation);
var
  TestArchiveOperation: TFileSourceTestArchiveOperation;
  TestArchiveStatistics: TFileSourceTestArchiveOperationStatistics;
begin
  TestArchiveOperation := Operation as TFileSourceTestArchiveOperation;
  TestArchiveStatistics := TestArchiveOperation.RetrieveStatistics;

  with TestArchiveStatistics do
  begin
    lblFileNameFrom.Caption := ArchiveFile;
    lblFileNameTo.Caption := CurrentFile;

    SetProgressBytes(pbFirst, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(pbSecond, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, True) + 'B');
  end;
end;

procedure TfrmFileOp.ToggleProgressBarStyle;
begin
  if (pbFirst.Style = pbstMarquee) and (pbSecond.Style = pbstMarquee) then
    begin
      pbFirst.Style:= pbstNormal;
      pbSecond.Style:= pbstNormal;
    end
  else
    begin
      pbFirst.Style:= pbstMarquee;
      pbSecond.Style:= pbstMarquee;
    end;
end;

end.
