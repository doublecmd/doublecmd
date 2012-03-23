{
   Seksi Commander
   ----------------------------
   Implementing of progress dialog for file operation

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:

   Copyright (C) 2008-2011  Koblov Alexander (Alexx2000@mail.ru)
}

unit fFileOpDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls, Buttons, ExtCtrls,
  KASProgressBar, uOperationsManager, uFileSourceOperation,
  uFileSourceOperationUI;

type

  TFileOpDlgLook = set of (fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb);

  TOperationProgressWindowEvent =
    (opwevOpened,
     opwevClosed);

  TOperationProgressWindowEvents = set of TOperationProgressWindowEvent;

  TOperationProgressWindowEventProc = procedure(OperationHandle: TOperationHandle;
                                                Event: TOperationProgressWindowEvent) of object;

  { TfrmFileOp }

  TfrmFileOp = class(TForm)
    btnCancel: TBitBtn;
    btnPauseStart: TBitBtn;
    btnViewOperations: TButton;
    btnMinimizeToPanel: TButton;
    lblEstimated: TLabel;
    lblFileNameFrom: TLabel;
    lblFileNameTo: TLabel;
    lblFrom: TLabel;
    lblTo: TLabel;
    pbFirst: TKASProgressBar;
    pbSecond: TKASProgressBar;
    pnlButtons: TPanel;
    pnlClient: TPanel;
    pnlFrom: TPanel;
    pnlTo: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnMinimizeToPanelClick(Sender: TObject);
    procedure btnPauseStartClick(Sender: TObject);
    procedure btnViewOperationsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    function GetProgressBarStyle: TProgressBarStyle;
    procedure SetProgressBarStyle(const AValue: TProgressBarStyle);

  private
    { Private declarations }
    FOperationHandle: TOperationHandle;
    FUpdateTimer: TTimer;  //<en Timer for updating statistics.
    FUserInterface: TFileSourceOperationUI;
    FStopOperationOnClose: Boolean;

    procedure OnUpdateTimer(Sender: TObject);

    procedure InitializeControls(FileOpDlgLook: TFileOpDlgLook);
    procedure NotifyEvents(Events: TOperationProgressWindowEvents);
    procedure SetPauseGlyph;
    procedure SetPlayGlyph;
    procedure UpdatePauseStartButton(OperationState: TFileSourceOperationState);
    procedure SetProgressBytes(ProgressBar: TKASProgressBar; CurrentBytes: Int64; TotalBytes: Int64);
    procedure SetSpeedAndTime(Operation: TFileSourceOperation; RemainingTime: TDateTime; Speed: String);

    procedure InitializeCopyOperation(Operation: TFileSourceOperation);
    procedure InitializeMoveOperation(Operation: TFileSourceOperation);
    procedure InitializeDeleteOperation(Operation: TFileSourceOperation);
    procedure InitializeWipeOperation(Operation: TFileSourceOperation);
    procedure InitializeSplitOperation(Operation: TFileSourceOperation);
    procedure InitializeCombineOperation(Operation: TFileSourceOperation);
    procedure InitializeCalcChecksumOperation(Operation: TFileSourceOperation);
    procedure InitializeTestArchiveOperation(Operation: TFileSourceOperation);
    procedure InitializeCalcStatisticsOperation(Operation: TFileSourceOperation);
    procedure UpdateCopyOperation(Operation: TFileSourceOperation);
    procedure UpdateMoveOperation(Operation: TFileSourceOperation);
    procedure UpdateDeleteOperation(Operation: TFileSourceOperation);
    procedure UpdateWipeOperation(Operation: TFileSourceOperation);
    procedure UpdateSplitOperation(Operation: TFileSourceOperation);
    procedure UpdateCombineOperation(Operation: TFileSourceOperation);
    procedure UpdateCalcStatisticsOperation(Operation: TFileSourceOperation);
    procedure UpdateCalcChecksumOperation(Operation: TFileSourceOperation);
    procedure UpdateTestArchiveOperation(Operation: TFileSourceOperation);

  public
    constructor Create(OperationHandle: TOperationHandle); reintroduce;
    destructor Destroy; override;

    function CloseQuery: Boolean; override;

    class procedure AddEventsListener(Events: TOperationProgressWindowEvents;
                                      FunctionToCall: TOperationProgressWindowEventProc);
    class procedure RemoveEventsListener(Events: TOperationProgressWindowEvents;
                                         FunctionToCall: TOperationProgressWindowEventProc);

    class function IsOpenedFor(AOperationHandle: TOperationHandle): Boolean;
    class procedure ShowFor(AOperationHandle: TOperationHandle);

    property ProgressBarStyle: TProgressBarStyle read GetProgressBarStyle write SetProgressBarStyle;
  end;

implementation

{$R *.lfm}

uses
   dmCommonData, uLng, uDCUtils,
   fViewOperations,
   uFileSourceOperationTypes,
   uFileSourceCopyOperation,
   uFileSourceMoveOperation,
   uFileSourceDeleteOperation,
   uFileSourceWipeOperation,
   uFileSourceSplitOperation,
   uFileSourceCombineOperation,
   uFileSourceCalcChecksumOperation,
   uFileSourceCalcStatisticsOperation,
   uFileSourceTestArchiveOperation,
   uFileSourceOperationMessageBoxesUI
   ;

type
  PEventsListItem = ^TEventsListItem;
  TEventsListItem = record
    EventFunction: TOperationProgressWindowEventProc;
  end;

var
  OpenedForms: TFPList;
  OpenedHandles: TFPList;
  EventsListeners: array[TOperationProgressWindowEvent] of TFPList;

procedure Initialize;
var
  Event: TOperationProgressWindowEvent;
begin
  OpenedForms := TFPList.Create;
  OpenedHandles := TFPList.Create;
  for Event := Low(EventsListeners) to High(EventsListeners) do
    EventsListeners[Event] := TFPList.Create;
end;

procedure Finalize;
var
  Event: TOperationProgressWindowEvent;
  Item: PEventsListItem;
begin
  for Event := Low(EventsListeners) to High(EventsListeners) do
  begin
    for Item in EventsListeners[Event] do
      Dispose(Item);
    FreeAndNil(EventsListeners[Event]);
  end;
  OpenedForms.Free;
  OpenedHandles.Free;
end;

procedure TfrmFileOp.btnCancelClick(Sender: TObject);
var
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) then
    OpManItem.Operation.Stop;
  ModalResult:= mrCancel;
end;

procedure TfrmFileOp.btnMinimizeToPanelClick(Sender: TObject);
begin
  FStopOperationOnClose := False;
  Close;
end;

procedure TfrmFileOp.btnPauseStartClick(Sender: TObject);
var
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) then
  begin
    if OpManItem.Operation.State in [fsosNotStarted, fsosPaused] then
    begin
      OpManItem.Operation.Start;
      SetPauseGlyph;
    end
    else if OpManItem.Operation.State in [fsosRunning, fsosWaitingForConnection] then
    begin
      OpManItem.Operation.Pause;
      SetPlayGlyph;
    end;
  end;
end;

procedure TfrmFileOp.btnViewOperationsClick(Sender: TObject);
begin
  ShowOperationsViewer(FOperationHandle);
end;

procedure TfrmFileOp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  CloseAction:= caFree;
  i := OpenedHandles.Remove(Pointer(FOperationHandle));
  if i >= 0 then
    OpenedForms.Delete(i);
  NotifyEvents([opwevClosed]);
end;

procedure TfrmFileOp.FormCreate(Sender: TObject);
var
  OpManItem: TOperationsManagerItem;
begin
  pbFirst.DoubleBuffered:= True;
  pbSecond.DoubleBuffered:= True;
  Self.DoubleBuffered:= True;

  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) then
  begin
    ProgressBarStyle:= pbstMarquee;

    case OpManItem.Operation.ID of

      fsoCopy, fsoCopyIn, fsoCopyOut:
        InitializeCopyOperation(OpManItem.Operation);
      fsoMove:
        InitializeMoveOperation(OpManItem.Operation);
      fsoDelete:
        InitializeDeleteOperation(OpManItem.Operation);
      fsoWipe:
        InitializeWipeOperation(OpManItem.Operation);
      fsoSplit:
        InitializeSplitOperation(OpManItem.Operation);
      fsoCombine:
        InitializeCombineOperation(OpManItem.Operation);
      fsoCalcChecksum:
        InitializeCalcChecksumOperation(OpManItem.Operation);
      fsoTestArchive:
        InitializeTestArchiveOperation(OpManItem.Operation);
      fsoCalcStatistics:
        InitializeCalcStatisticsOperation(OpManItem.Operation);

      else
        begin
          InitializeControls([fodl_first_pb]);
        end;
    end;

    Caption := OpManItem.Operation.GetDescription([fsoddJob]);
    Hint := Caption;
    UpdatePauseStartButton(OpManItem.Operation.State);

    FUpdateTimer := TTimer.Create(Self);
    FUpdateTimer.Interval := 100;
    FUpdateTimer.OnTimer := @OnUpdateTimer;
    FUpdateTimer.Enabled := True;

    if OpenedHandles.IndexOf(Pointer(FOperationHandle)) < 0 then
    begin
      OpenedForms.Add(Self);
      OpenedHandles.Add(Pointer(FOperationHandle));
      NotifyEvents([opwevOpened]);
    end;
  end;
end;

constructor TfrmFileOp.Create(OperationHandle: TOperationHandle);
var
  OpManItem: TOperationsManagerItem;
begin
  FOperationHandle := InvalidOperationHandle;

  inherited Create(Application);

  AutoSize := True;

  OpManItem := OperationsManager.GetItemByHandle(OperationHandle);
  if Assigned(OpManItem) then
  begin
    FUserInterface := TFileSourceOperationMessageBoxesUI.Create;
    OpManItem.Operation.AddUserInterface(FUserInterface);
    FStopOperationOnClose := True;
    FOperationHandle := OperationHandle;
  end
  else
  begin
    FUserInterface := nil;
    Close;
  end;
end;

destructor TfrmFileOp.Destroy;
var
  OpManItem: TOperationsManagerItem;
begin
  inherited Destroy;

  if Assigned(FUserInterface) then
  begin
    OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
    if Assigned(OpManItem) then
      OpManItem.Operation.RemoveUserInterface(FUserInterface);

    FreeAndNil(FUserInterface);
  end;
end;

function TfrmFileOp.CloseQuery: Boolean;
var
  OpManItem: TOperationsManagerItem;
begin
  Result := True;

  if FStopOperationOnClose then
  begin
    OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
    if Assigned(OpManItem) then
      OpManItem.Operation.Stop;
  end;
end;

class procedure TfrmFileOp.AddEventsListener(Events: TOperationProgressWindowEvents; FunctionToCall: TOperationProgressWindowEventProc);
var
  Item: PEventsListItem;
  Event: TOperationProgressWindowEvent;
begin
  for Event in Events do
  begin
    New(Item);
    Item^.EventFunction := FunctionToCall;
    EventsListeners[Event].Add(Item);
  end;
end;

class procedure TfrmFileOp.RemoveEventsListener(Events: TOperationProgressWindowEvents; FunctionToCall: TOperationProgressWindowEventProc);
var
  Item: PEventsListItem;
  Event: TOperationProgressWindowEvent;
  i: Integer;
begin
  for Event in Events do
  begin
    for i := 0 to EventsListeners[Event].Count - 1 do
    begin
      Item := PEventsListItem(EventsListeners[Event].Items[i]);
      if Item^.EventFunction = FunctionToCall then
      begin
        EventsListeners[Event].Delete(i);
        Dispose(Item);
        Break;  // break from one for only
      end;
    end;
  end;
end;

class function TfrmFileOp.IsOpenedFor(AOperationHandle: TOperationHandle): Boolean;
begin
  Result := OpenedHandles.IndexOf(Pointer(AOperationHandle)) >= 0;
end;

class procedure TfrmFileOp.ShowFor(AOperationHandle: TOperationHandle);
var
  OperationDialog: TfrmFileOp;
  Index: Integer;
begin
  if AOperationHandle <> InvalidOperationHandle then
  begin
    Index := OpenedHandles.IndexOf(Pointer(AOperationHandle));
    if Index < 0 then
    begin
      OperationDialog := TfrmFileOp.Create(AOperationHandle);
      OperationDialog.Show;
    end
    else
    begin
      OperationDialog := TfrmFileOp(OpenedForms[Index]);
      OperationDialog.ShowOnTop;
    end;
  end;
end;

procedure TfrmFileOp.OnUpdateTimer(Sender: TObject);
var
  OpManItem: TOperationsManagerItem;
  NewCaption: String;
begin
  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) and (OpManItem.Operation.State <> fsosStopped) then
  begin
    if (OpManItem.Operation.State = fsosRunning) and (ProgressBarStyle = pbstMarquee) then
    begin
      ProgressBarStyle:= pbstNormal;
    end;

    case OpManItem.Operation.ID of

      fsoCopy, fsoCopyIn, fsoCopyOut:
        UpdateCopyOperation(OpManItem.Operation);
      fsoMove:
        UpdateMoveOperation(OpManItem.Operation);
      fsoDelete:
        UpdateDeleteOperation(OpManItem.Operation);
      fsoWipe:
        UpdateWipeOperation(OpManItem.Operation);
      fsoSplit:
        UpdateSplitOperation(OpManItem.Operation);
      fsoCombine:
        UpdateCombineOperation(OpManItem.Operation);
      fsoCalcChecksum:
        UpdateCalcChecksumOperation(OpManItem.Operation);
      fsoCalcStatistics:
        UpdateCalcStatisticsOperation(OpManItem.Operation);
      fsoTestArchive:
        UpdateTestArchiveOperation(OpManItem.Operation);

      else
      begin
        // Operation not currently supported for display.
        // Only show general progress.
        pbFirst.Position := Round(OpManItem.Operation.Progress * pbFirst.Max);
      end;
    end;

    UpdatePauseStartButton(OpManItem.Operation.State);

    NewCaption := FloatToStrF(OpManItem.Operation.Progress * 100, ffFixed, 0, 0) + '% ' + Hint;
    if OpManItem.Operation.State <> fsosRunning then
      NewCaption := NewCaption + ' [' + FileSourceOperationStateText[OpManItem.Operation.State] + ']';
    Caption := NewCaption;
  end
  else
  begin
    // Operation has finished.
    // if CloseOnFinish then
    Close;
    // if BeepOnFinish then Beep;
    FStopOperationOnClose := False;
  end;
end;

procedure TfrmFileOp.InitializeControls(FileOpDlgLook: TFileOpDlgLook);
begin
  lblFrom.Visible         := fodl_from_lbl in FileOpDlgLook;
  lblFileNameFrom.Visible := lblFrom.Visible;

  lblTo.Visible           := fodl_to_lbl in FileOpDlgLook;
  lblFileNameTo.Visible   := lblTo.Visible;

  pbFirst.Visible         := fodl_first_pb in FileOpDlgLook;
  pbSecond.Visible        := fodl_second_pb in FileOpDlgLook;

  if (fodl_second_pb in FileOpDlgLook) then
    pbSecond.ShowInTaskbar:= True
  else
    pbFirst.ShowInTaskbar:= True;

  lblFileNameFrom.Caption := '';
  lblFileNameTo.Caption := '';
  lblEstimated.Caption := #32;
end;

procedure TfrmFileOp.NotifyEvents(Events: TOperationProgressWindowEvents);
var
  Item: PEventsListItem;
  Event: TOperationProgressWindowEvent;
begin
  for Event in Events do
    for Item in EventsListeners[Event] do
      Item^.EventFunction(FOperationHandle, Event);
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

procedure TfrmFileOp.SetProgressBytes(ProgressBar: TKASProgressBar; CurrentBytes: Int64; TotalBytes: Int64);
begin
  ProgressBar.SetProgress(CurrentBytes, TotalBytes,
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
      if RemainingTime > 0 then
        begin
          sEstimated := FormatDateTime('HH:MM:SS', RemainingTime);
          sEstimated := Format(rsDlgSpeedTime, [Speed, sEstimated]);
        end
      else
        sEstimated := Format(rsDlgSpeed, [Speed]);
    end;

  lblEstimated.Caption := sEstimated;
end;

procedure TfrmFileOp.InitializeCopyOperation(Operation: TFileSourceOperation);
begin
  InitializeControls([fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb]);
end;

procedure TfrmFileOp.InitializeMoveOperation(Operation: TFileSourceOperation);
begin
  InitializeControls([fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb]);
end;

procedure TfrmFileOp.InitializeDeleteOperation(Operation: TFileSourceOperation);
begin
  InitializeControls([fodl_from_lbl, fodl_first_pb]);
  lblFrom.Caption := rsOperDeleting;
end;

procedure TfrmFileOp.InitializeCalcStatisticsOperation(Operation: TFileSourceOperation);
begin
  InitializeControls([fodl_from_lbl]);
  lblFrom.Caption := rsOperCalculatingStatictics;
end;

procedure TfrmFileOp.InitializeWipeOperation(Operation: TFileSourceOperation);
begin
  InitializeControls([fodl_from_lbl, fodl_first_pb, fodl_second_pb]);
  lblFrom.Caption := rsOperWiping;
end;

procedure TfrmFileOp.InitializeSplitOperation(Operation: TFileSourceOperation);
begin
  InitializeControls([fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb]);
end;

procedure TfrmFileOp.InitializeCombineOperation(Operation: TFileSourceOperation);
begin
  InitializeControls([fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb]);
end;

procedure TfrmFileOp.InitializeCalcChecksumOperation(Operation: TFileSourceOperation);
begin
  InitializeControls([fodl_from_lbl, fodl_first_pb, fodl_second_pb]);
  lblFrom.Visible := False;
end;

procedure TfrmFileOp.InitializeTestArchiveOperation(Operation: TFileSourceOperation);
begin
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
    lblFileNameFrom.Caption := MinimizeFilePath(CurrentFileFrom, lblFileNameFrom.Canvas, lblFileNameFrom.Width);
    lblFileNameTo.Caption := MinimizeFilePath(CurrentFileTo, lblFileNameTo.Canvas, lblFileNameTo.Width);

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
    lblFileNameFrom.Caption := MinimizeFilePath(CurrentFileFrom, lblFileNameFrom.Canvas, lblFileNameFrom.Width);
    lblFileNameTo.Caption := MinimizeFilePath(CurrentFileTo, lblFileNameTo.Canvas, lblFileNameTo.Width);

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
    lblFileNameFrom.Caption := MinimizeFilePath(CurrentFile, lblFileNameFrom.Canvas, lblFileNameFrom.Width);

    pbFirst.SetProgress(DoneFiles, TotalFiles,
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
    lblFileNameFrom.Caption := MinimizeFilePath(CurrentFile, lblFileNameFrom.Canvas, lblFileNameFrom.Width);

    SetProgressBytes(pbFirst, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(pbSecond, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, True) + 'B');
  end;
end;

procedure TfrmFileOp.UpdateSplitOperation(Operation: TFileSourceOperation);
var
  SplitOperation: TFileSourceSplitOperation;
  SplitStatistics: TFileSourceSplitOperationStatistics;
begin
  SplitOperation := Operation as TFileSourceSplitOperation;
  SplitStatistics := SplitOperation.RetrieveStatistics;

  with SplitStatistics do
  begin
    lblFileNameFrom.Caption := MinimizeFilePath(CurrentFileFrom, lblFileNameFrom.Canvas, lblFileNameFrom.Width);
    lblFileNameTo.Caption := MinimizeFilePath(CurrentFileTo, lblFileNameTo.Canvas, lblFileNameTo.Width);

    SetProgressBytes(pbFirst, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(pbSecond, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, True) + 'B');
  end;
end;

procedure TfrmFileOp.UpdateCombineOperation(Operation: TFileSourceOperation);
var
  CombineOperation: TFileSourceCombineOperation;
  CombineStatistics: TFileSourceCombineOperationStatistics;
begin
  CombineOperation := Operation as TFileSourceCombineOperation;
  CombineStatistics := CombineOperation.RetrieveStatistics;

  with CombineStatistics do
  begin
    lblFileNameFrom.Caption := MinimizeFilePath(CurrentFileFrom, lblFileNameFrom.Canvas, lblFileNameFrom.Width);
    lblFileNameTo.Caption := MinimizeFilePath(CurrentFileTo, lblFileNameTo.Canvas, lblFileNameTo.Width);

    SetProgressBytes(pbFirst, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(pbSecond, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, True) + 'B');
  end;
end;

procedure TfrmFileOp.UpdateCalcStatisticsOperation(Operation: TFileSourceOperation);
var
  CalcStatisticsOperation: TFileSourceCalcStatisticsOperation;
  CalcStatisticsOperationStatistics: TFileSourceCalcStatisticsOperationStatistics;
begin
  CalcStatisticsOperation := Operation as TFileSourceCalcStatisticsOperation;
  CalcStatisticsOperationStatistics := CalcStatisticsOperation.RetrieveStatistics;

  with CalcStatisticsOperationStatistics do
  begin
    lblFileNameFrom.Caption := MinimizeFilePath(CurrentFile, lblFileNameFrom.Canvas, lblFileNameFrom.Width);
    SetSpeedAndTime(Operation, 0, cnvFormatFileSize(FilesPerSecond, True));
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
    lblFileNameFrom.Caption := MinimizeFilePath(CurrentFile, lblFileNameFrom.Canvas, lblFileNameFrom.Width);

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
    lblFileNameFrom.Caption := MinimizeFilePath(ArchiveFile, lblFileNameFrom.Canvas, lblFileNameFrom.Width);
    lblFileNameTo.Caption := MinimizeFilePath(CurrentFile, lblFileNameTo.Canvas, lblFileNameTo.Width);

    SetProgressBytes(pbFirst, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(pbSecond, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, True) + 'B');
  end;
end;

function TfrmFileOp.GetProgressBarStyle: TProgressBarStyle;
begin
  if (pbFirst.Style = pbstMarquee) and (pbSecond.Style = pbstMarquee) then
    Result:= pbstMarquee
  else
    Result:= pbstNormal;
end;

procedure TfrmFileOp.SetProgressBarStyle(const AValue: TProgressBarStyle);
begin
  pbFirst.Style:= AValue;
  pbSecond.Style:= AValue;
end;

initialization
  Initialize;

finalization
  Finalize;

end.
