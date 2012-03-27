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
    lblCurrentOperationText: TLabel;
    lblQueue: TLabel;
    lblEstimated: TLabel;
    lblFileNameFrom: TLabel;
    lblFileNameTo: TLabel;
    lblFrom: TLabel;
    lblCurrentOperation: TLabel;
    lblTo: TLabel;
    pbQueue: TKASProgressBar;
    pnlQueue: TPanel;
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
    FOperationHandle: TOperationHandle;
    FQueueIdentifier: TOperationsManagerQueueIdentifier;
    FUpdateTimer: TTimer;  //<en Timer for updating statistics.
    FUserInterface: TFileSourceOperationUI;
    FStopOperationOnClose: Boolean;

    procedure OnUpdateTimer(Sender: TObject);

    function  AddToOpenedForms(OpManItem: TOperationsManagerItem): Boolean;
    procedure CloseDialog;
    procedure FinalizeOperation;
    function  GetFirstOperationHandle(QueueIdentifier: TOperationsManagerQueueIdentifier): TOperationHandle;
    procedure InitializeControls(OpManItem: TOperationsManagerItem; FileOpDlgLook: TFileOpDlgLook);
    function  InitializeOperation: Boolean;
    procedure NotifyEvents(Events: TOperationProgressWindowEvents);
    procedure RemoveFromOpenedForms;
    procedure SetPauseGlyph;
    procedure SetPlayGlyph;
    procedure UpdateOperation(OpManItem: TOperationsManagerItem);
    procedure UpdatePauseStartButton(OpManItem: TOperationsManagerItem);
    procedure SetProgressBytes(ProgressBar: TKASProgressBar; CurrentBytes: Int64; TotalBytes: Int64);
    procedure SetSpeedAndTime(Operation: TFileSourceOperation; RemainingTime: TDateTime; Speed: String);

    procedure InitializeCopyOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeMoveOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeDeleteOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeWipeOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeSplitOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeCombineOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeCalcChecksumOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeTestArchiveOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeCalcStatisticsOperation(OpManItem: TOperationsManagerItem);
    procedure UpdateCopyOperation(Operation: TFileSourceOperation);
    procedure UpdateMoveOperation(Operation: TFileSourceOperation);
    procedure UpdateDeleteOperation(Operation: TFileSourceOperation);
    procedure UpdateWipeOperation(Operation: TFileSourceOperation);
    procedure UpdateSplitOperation(Operation: TFileSourceOperation);
    procedure UpdateCombineOperation(Operation: TFileSourceOperation);
    procedure UpdateCalcStatisticsOperation(Operation: TFileSourceOperation);
    procedure UpdateCalcChecksumOperation(Operation: TFileSourceOperation);
    procedure UpdateTestArchiveOperation(Operation: TFileSourceOperation);

    class function GetOpenedForm(AOperationHandle: TOperationHandle): TfrmFileOp;
    class function GetOpenedForm(AQueueIdentifier: TOperationsManagerQueueIdentifier): TfrmFileOp;

  public
    constructor Create(OperationHandle: TOperationHandle); reintroduce;
    constructor Create(QueueIdentifier: TOperationsManagerQueueIdentifier); reintroduce;
    destructor Destroy; override;

    function CloseQuery: Boolean; override;

    class procedure AddEventsListener(Events: TOperationProgressWindowEvents;
                                      FunctionToCall: TOperationProgressWindowEventProc);
    class procedure RemoveEventsListener(Events: TOperationProgressWindowEvents;
                                         FunctionToCall: TOperationProgressWindowEventProc);

    class function IsOpenedFor(AOperationHandle: TOperationHandle): Boolean;
    class function IsOpenedFor(AQueueIdentifier: TOperationsManagerQueueIdentifier): Boolean;
    class procedure ShowFor(AOperationHandle: TOperationHandle);
    class procedure ShowFor(AQueueIdentifier: TOperationsManagerQueueIdentifier);

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

  POpenedForm = ^TOpenedForm;
  TOpenedForm = record
    Form: TfrmFileOp;
    OperationHandle: TOperationHandle;
    QueueIdentifier: TOperationsManagerQueueIdentifier;
  end;

var
  OpenedForms: TFPList;
  EventsListeners: array[TOperationProgressWindowEvent] of TFPList;

procedure Initialize;
var
  Event: TOperationProgressWindowEvent;
begin
  OpenedForms := TFPList.Create;
  for Event := Low(EventsListeners) to High(EventsListeners) do
    EventsListeners[Event] := TFPList.Create;
end;

procedure Finalize;
var
  Event: TOperationProgressWindowEvent;
  Item: PEventsListItem;
  OpenedForm: POpenedForm;
begin
  for Event := Low(EventsListeners) to High(EventsListeners) do
  begin
    for Item in EventsListeners[Event] do
      Dispose(Item);
    FreeAndNil(EventsListeners[Event]);
  end;
  for OpenedForm in OpenedForms do
    Dispose(OpenedForm);
  OpenedForms.Free;
end;

procedure TfrmFileOp.btnCancelClick(Sender: TObject);
var
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) then
  begin
    if OpManItem.Queue.IsFree then
      OpManItem.Operation.Stop
    else
      OpManItem.Queue.Stop;
    ModalResult:= mrCancel;
  end;
end;

procedure TfrmFileOp.btnMinimizeToPanelClick(Sender: TObject);
begin
  CloseDialog;
end;

procedure TfrmFileOp.btnPauseStartClick(Sender: TObject);
var
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) then
  begin
    if OpManItem.Queue.IsFree then
      OpManItem.Operation.TogglePause
    else
      OpManItem.Queue.TogglePause;
    UpdatePauseStartButton(OpManItem);
  end;
end;

procedure TfrmFileOp.btnViewOperationsClick(Sender: TObject);
var
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) then
  begin
    if OpManItem.Queue.IsFree then
      ShowOperationsViewer(OpManItem.Handle)
    else
      ShowOperationsViewer(OpManItem.Queue.Identifier);
  end;
end;

procedure TfrmFileOp.CloseDialog;
begin
  FStopOperationOnClose := False;
  Close;
end;

procedure TfrmFileOp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
  FinalizeOperation;
end;

procedure TfrmFileOp.FormCreate(Sender: TObject);
begin
  pbFirst.DoubleBuffered:= True;
  pbSecond.DoubleBuffered:= True;
  pbQueue.DoubleBuffered:= True;
  Self.DoubleBuffered:= True;

  FUpdateTimer := TTimer.Create(Self);
  FUpdateTimer.Interval := 100;
  FUpdateTimer.OnTimer := @OnUpdateTimer;
  FUpdateTimer.Enabled := False;

  if not InitializeOperation then
    CloseDialog;
end;

function TfrmFileOp.GetFirstOperationHandle(QueueIdentifier: TOperationsManagerQueueIdentifier): TOperationHandle;
var
  Queue: TOperationsManagerQueue;
begin
  Queue := OperationsManager.QueueByIdentifier[QueueIdentifier];
  if Assigned(Queue) and (Queue.Count > 0) then
    Result := Queue.Items[0].Handle
  else
    Result := InvalidOperationHandle;
end;

class function TfrmFileOp.GetOpenedForm(AOperationHandle: TOperationHandle): TfrmFileOp;
var
  Index: Integer;
  Item: POpenedForm;
begin
  for Index := 0 to OpenedForms.Count - 1 do
  begin
    Item := OpenedForms[Index];
    if Item^.OperationHandle = AOperationHandle then
      Exit(Item^.Form);
  end;
  Result := nil;
end;

class function TfrmFileOp.GetOpenedForm(AQueueIdentifier: TOperationsManagerQueueIdentifier): TfrmFileOp;
var
  Index: Integer;
  Item: POpenedForm;
begin
  for Index := 0 to OpenedForms.Count - 1 do
  begin
    Item := OpenedForms[Index];
    if Item^.QueueIdentifier = AQueueIdentifier then
      Exit(Item^.Form);
  end;
  Result := nil;
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
    FStopOperationOnClose := True;
    FOperationHandle := OperationHandle;
  end
  else
  begin
    CloseDialog;
  end;
end;

constructor TfrmFileOp.Create(QueueIdentifier: TOperationsManagerQueueIdentifier);
begin
  Create(GetFirstOperationHandle(QueueIdentifier));
end;

destructor TfrmFileOp.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FUserInterface);
end;

procedure TfrmFileOp.FinalizeOperation;
var
  OpManItem: TOperationsManagerItem;
begin
  FUpdateTimer.Enabled := False;

  if FOperationHandle <> InvalidOperationHandle then
  begin
    if Assigned(FUserInterface) then
    begin
      OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
      if Assigned(OpManItem) then
        OpManItem.Operation.RemoveUserInterface(FUserInterface);
    end;

    RemoveFromOpenedForms;
    FOperationHandle := InvalidOperationHandle;
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

function TfrmFileOp.AddToOpenedForms(OpManItem: TOperationsManagerItem): Boolean;
var
  Index: Integer;
  Item: POpenedForm;
  Found: Boolean = False;
begin
  for Index := 0 to OpenedForms.Count - 1 do
  begin
    Item := OpenedForms[Index];
    // Check if another form is not already opened for the operation or queue.
    if (Item^.OperationHandle = FOperationHandle) or
       (not OpManItem.Queue.IsFree and (Item^.QueueIdentifier = FQueueIdentifier)) then
    begin
      Exit(False);
    end
    else if Item^.Form = Self then
    begin
      Found := True;
      Break;
    end;
  end;

  if not Found then
  begin
    New(Item);
    OpenedForms.Add(Item);
  end;

  Item^.Form := Self;
  Item^.OperationHandle := FOperationHandle;
  Item^.QueueIdentifier := FQueueIdentifier;
  NotifyEvents([opwevOpened]);
  Result := True;
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

procedure TfrmFileOp.RemoveFromOpenedForms;
var
  i: Integer;
  Item: POpenedForm;
begin
  for i := 0 to OpenedForms.Count - 1 do
  begin
    Item := OpenedForms[i];
    if Item^.Form = Self then
    begin
      Dispose(Item);
      OpenedForms.Delete(i);
      NotifyEvents([opwevClosed]);
      Break;
    end;
  end;
end;

class function TfrmFileOp.IsOpenedFor(AOperationHandle: TOperationHandle): Boolean;
begin
  Result := Assigned(GetOpenedForm(AOperationHandle));
end;

class function TfrmFileOp.IsOpenedFor(AQueueIdentifier: TOperationsManagerQueueIdentifier): Boolean;
begin
  Result := Assigned(GetOpenedForm(AQueueIdentifier));
end;

class procedure TfrmFileOp.ShowFor(AOperationHandle: TOperationHandle);
var
  OperationDialog: TfrmFileOp;
begin
  OperationDialog := GetOpenedForm(AOperationHandle);
  if Assigned(OperationDialog) then
    OperationDialog.ShowOnTop
  else
  begin
    OperationDialog := TfrmFileOp.Create(AOperationHandle);
    OperationDialog.Show;
  end;
end;

class procedure TfrmFileOp.ShowFor(AQueueIdentifier: TOperationsManagerQueueIdentifier);
var
  OperationDialog: TfrmFileOp;
begin
  OperationDialog := GetOpenedForm(AQueueIdentifier);
  if Assigned(OperationDialog) then
    OperationDialog.ShowOnTop
  else
  begin
    OperationDialog := TfrmFileOp.Create(AQueueIdentifier);
    OperationDialog.Show;
  end;
end;

procedure TfrmFileOp.OnUpdateTimer(Sender: TObject);
var
  OpManItem: TOperationsManagerItem;
  Queue: TOperationsManagerQueue;
begin
  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);

  // Check if operation did not change queues.
  if Assigned(OpManItem) and (OpManItem.Queue.Identifier <> FQueueIdentifier) then
  begin
    Queue := OperationsManager.QueueByIdentifier[FQueueIdentifier];
    FinalizeOperation;
    if Assigned(Queue) and not Queue.IsFree then
    begin
      // Queue was begin followed - switch to next operation from that queue.
      FQueueIdentifier := Queue.Identifier;
      FOperationHandle := GetFirstOperationHandle(Queue.Identifier);
    end
    else
    begin
      // Follow the operation to new queue either because previously followed
      // queue was free-operations queue or old queue was destroyed.
      FQueueIdentifier := OpManItem.Queue.Identifier;
      FOperationHandle := OpManItem.Handle;
    end;
    if not InitializeOperation then
    begin
      CloseDialog;
      Exit;
    end;
    OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  end;

  // Check if first operation in the queue hasn't changed.
  if Assigned(OpManItem) and
     not OpManItem.Queue.IsFree and
     (GetFirstOperationHandle(FQueueIdentifier) <> FOperationHandle) then
  begin
    FinalizeOperation;
    FOperationHandle := GetFirstOperationHandle(FQueueIdentifier);
    if not InitializeOperation then
    begin
      CloseDialog;
      Exit;
    end;
    OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  end;

  if Assigned(OpManItem) then
  begin
    UpdateOperation(OpManItem);
  end
  else // Operation was destroyed
  begin
    Queue := OperationsManager.QueueByIdentifier[FQueueIdentifier];
    if not Assigned(Queue) or Queue.IsFree then
    begin
      // Single operation was being followed and it has finished
      // or all operations in queue have finished - close window.
      CloseDialog;
    end
    else
    begin
      // Queue was begin followed - switch to next operation from that queue.
      FOperationHandle := GetFirstOperationHandle(FQueueIdentifier);
    end;
  end;
end;

procedure TfrmFileOp.InitializeControls(OpManItem: TOperationsManagerItem; FileOpDlgLook: TFileOpDlgLook);
var
  ShowQueue: Boolean;
begin
  ShowQueue := not OpManItem.Queue.IsFree;

  pnlQueue.Visible := ShowQueue;

  lblFrom.Visible         := fodl_from_lbl in FileOpDlgLook;
  lblFileNameFrom.Visible := lblFrom.Visible;

  lblTo.Visible           := fodl_to_lbl in FileOpDlgLook;
  lblFileNameTo.Visible   := lblTo.Visible;

  pbFirst.Visible         := fodl_first_pb in FileOpDlgLook;
  pbSecond.Visible        := fodl_second_pb in FileOpDlgLook;

  pbQueue.ShowInTaskbar  := ShowQueue;
  pbFirst.ShowInTaskbar  :=
    not ShowQueue and ([fodl_first_pb, fodl_second_pb] * FileOpDlgLook = [fodl_first_pb]);
  pbSecond.ShowInTaskbar :=
    not ShowQueue and (fodl_second_pb in FileOpDlgLook);

  if ShowQueue then
    lblQueue.Caption := rsDlgQueue + ' ' + IntToStr(OpManItem.Queue.Identifier);

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

procedure TfrmFileOp.InitializeCopyOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb]);
end;

procedure TfrmFileOp.InitializeMoveOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb]);
end;

function TfrmFileOp.InitializeOperation: Boolean;
var
  OpManItem: TOperationsManagerItem;
begin
  Result := False;

  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);

  if Assigned(OpManItem) and not OpManItem.Queue.IsFree then
  begin
    FOperationHandle := GetFirstOperationHandle(OpManItem.Queue.Identifier);
    OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  end;

  if Assigned(OpManItem) then
  begin
    FQueueIdentifier := OpManItem.Queue.Identifier;

    if AddToOpenedForms(OpManItem) then
    begin
      if not Assigned(FUserInterface) then
        FUserInterface := TFileSourceOperationMessageBoxesUI.Create;
      OpManItem.Operation.AddUserInterface(FUserInterface);

      ProgressBarStyle:= pbstMarquee;

      case OpManItem.Operation.ID of

        fsoCopy, fsoCopyIn, fsoCopyOut:
          InitializeCopyOperation(OpManItem);
        fsoMove:
          InitializeMoveOperation(OpManItem);
        fsoDelete:
          InitializeDeleteOperation(OpManItem);
        fsoWipe:
          InitializeWipeOperation(OpManItem);
        fsoSplit:
          InitializeSplitOperation(OpManItem);
        fsoCombine:
          InitializeCombineOperation(OpManItem);
        fsoCalcChecksum:
          InitializeCalcChecksumOperation(OpManItem);
        fsoTestArchive:
          InitializeTestArchiveOperation(OpManItem);
        fsoCalcStatistics:
          InitializeCalcStatisticsOperation(OpManItem);

        else
          begin
            InitializeControls(OpManItem, [fodl_first_pb]);
          end;
      end;

      UpdatePauseStartButton(OpManItem);
      Caption := EmptyStr;
      FUpdateTimer.Enabled := True;
      Result := True;
    end;
  end;
  if not Result then
    FOperationHandle := InvalidOperationHandle;
end;

procedure TfrmFileOp.InitializeDeleteOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_first_pb]);
end;

procedure TfrmFileOp.InitializeCalcStatisticsOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl]);
end;

procedure TfrmFileOp.InitializeWipeOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_first_pb, fodl_second_pb]);
end;

procedure TfrmFileOp.InitializeSplitOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb]);
end;

procedure TfrmFileOp.InitializeCombineOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb]);
end;

procedure TfrmFileOp.InitializeCalcChecksumOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_first_pb, fodl_second_pb]);
end;

procedure TfrmFileOp.InitializeTestArchiveOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_to_lbl, fodl_first_pb, fodl_second_pb]);
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

procedure TfrmFileOp.UpdateOperation(OpManItem: TOperationsManagerItem);
var
  NewCaption: String;
begin
  // Proceed with update.
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

  if not OpManItem.Queue.IsFree then
    pbQueue.Position := Round(OpManItem.Queue.Progress * pbQueue.Max);

  UpdatePauseStartButton(OpManItem);

  if OpManItem.Queue.IsFree then
  begin
    NewCaption := GetProgressString(OpManItem.Operation.Progress) + ' '
                  + OpManItem.Operation.GetDescription(fsoddJob)
                  + GetOperationStateString(OpManItem.Operation.State);
  end
  else
  begin
    NewCaption := GetProgressString(OpManItem.Queue.Progress) + ' '
                  + rsDlgQueue + ' ' + IntToStr(OpManItem.Queue.Identifier);

    if OpManItem.Queue.Paused then
      NewCaption := NewCaption + GetOperationStateString(fsosPaused)
    else
      NewCaption := NewCaption + ': ' + OpManItem.Operation.GetDescription(fsoddJob);

    lblCurrentOperationText.Caption :=
      OpManItem.Operation.GetDescription(fsoddJob) + ' ' +
      GetProgressString(OpManItem.Operation.Progress);
  end;

  Caption := NewCaption;
end;

procedure TfrmFileOp.UpdatePauseStartButton(OpManItem: TOperationsManagerItem);
begin
  if OpManItem.Queue.IsFree then
  begin
    case OpManItem.Operation.State of
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
  end
  else
  begin
    btnPauseStart.Enabled := True;
    if OpManItem.Queue.Paused then
      SetPlayGlyph
    else
      SetPauseGlyph;
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
