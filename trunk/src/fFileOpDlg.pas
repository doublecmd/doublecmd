{
   Double Commander
   -------------------------------------------------------------------------
   Window displaying progress for file source operations and queues.

   Copyright (C) 2008-2018  Alexander Koblov (alexx2000@mail.ru)
   Copyright (C) 2012       Przemysław Nagay (cobines@gmail.com)

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

unit fFileOpDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls, Buttons, ExtCtrls,
  KASProgressBar, uOperationsManager, uFileSourceOperation,
  uFileSourceOperationUI, uOSForms;

type

  TFileOpDlgLook = set of (fodl_from_lbl, fodl_to_lbl, fodl_current_pb, fodl_total_pb);

  TOperationProgressWindowEvent =
    (opwevOpened,
     opwevClosed);

  TOperationProgressWindowEvents = set of TOperationProgressWindowEvent;

  TOperationProgressWindowEventProc = procedure(OperationHandle: TOperationHandle;
                                                Event: TOperationProgressWindowEvent) of object;

  TOperationProgressWindowOption =
    (opwoIfExistsBringToFront,
     opwoStartMinimized);

  TOperationProgressWindowOptions = set of TOperationProgressWindowOption;

  { TfrmFileOp }

  TfrmFileOp = class(TModalDialog)
    btnCancel: TBitBtn;
    btnPauseStart: TBitBtn;
    btnViewOperations: TBitBtn;
    btnMinimizeToPanel: TBitBtn;
    lblFileCount: TLabel;
    lblCurrentOperationText: TLabel;
    lblEstimated: TLabel;
    lblFileNameFrom: TLabel;
    lblFileNameTo: TLabel;
    lblFrom: TLabel;
    lblCurrentOperation: TLabel;
    lblTo: TLabel;
    pnlQueue: TPanel;
    pbCurrent: TKASProgressBar;
    pbTotal: TKASProgressBar;
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
    function  GetProgressBarStyle: TProgressBarStyle;
    procedure InitializeControls(OpManItem: TOperationsManagerItem; FileOpDlgLook: TFileOpDlgLook);
    function  InitializeOperation: Boolean;
    procedure NotifyEvents(Events: TOperationProgressWindowEvents);
    procedure RemoveFromOpenedForms;
    procedure SetPauseGlyph;
    procedure SetPlayGlyph;
    procedure SetLabelCaption(ALabel: TLabel; const AText: String);
    procedure UpdateOperation(OpManItem: TOperationsManagerItem);
    procedure UpdatePauseStartButton(OpManItem: TOperationsManagerItem);
    procedure SetProgressBarStyle(const AValue: TProgressBarStyle);
    procedure SetProgressCount(Operation: TFileSourceOperation; DoneFiles: Int64; TotalFiles: Int64);
    procedure SetProgressBytes(Operation: TFileSourceOperation; ProgressBar: TKASProgressBar; CurrentBytes: Int64; TotalBytes: Int64);
    procedure SetProgressFiles(Operation: TFileSourceOperation; ProgressBar: TKASProgressBar; CurrentFiles: Int64; TotalFiles: Int64);
    procedure SetSpeedAndTime(Operation: TFileSourceOperation; RemainingTime: TDateTime; Speed: String);
    procedure StopOperationOrQueue;

    procedure InitializeCopyOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeMoveOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeDeleteOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeWipeOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeSplitOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeCombineOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeCalcChecksumOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeTestArchiveOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeCalcStatisticsOperation(OpManItem: TOperationsManagerItem);
    procedure InitializeSetFilePropertyOperation(OpManItem: TOperationsManagerItem);
    procedure UpdateCopyOperation(Operation: TFileSourceOperation);
    procedure UpdateMoveOperation(Operation: TFileSourceOperation);
    procedure UpdateDeleteOperation(Operation: TFileSourceOperation);
    procedure UpdateWipeOperation(Operation: TFileSourceOperation);
    procedure UpdateSplitOperation(Operation: TFileSourceOperation);
    procedure UpdateCombineOperation(Operation: TFileSourceOperation);
    procedure UpdateCalcStatisticsOperation(Operation: TFileSourceOperation);
    procedure UpdateCalcChecksumOperation(Operation: TFileSourceOperation);
    procedure UpdateTestArchiveOperation(Operation: TFileSourceOperation);
    procedure UpdateSetFilePropertyOperation(Operation: TFileSourceOperation);

    class function GetOpenedForm(AOperationHandle: TOperationHandle): TfrmFileOp;
    class function GetOpenedForm(AQueueIdentifier: TOperationsManagerQueueIdentifier): TfrmFileOp;
    class procedure ShowExistingWindow(AWindow: TfrmFileOp; Options: TOperationProgressWindowOptions);
    class procedure ShowNewWindow(AWindow: TfrmFileOp; Options: TOperationProgressWindowOptions);

    property ProgressBarStyle: TProgressBarStyle read GetProgressBarStyle write SetProgressBarStyle;

  public
    constructor Create(OperationHandle: TOperationHandle); reintroduce;
    constructor Create(QueueIdentifier: TOperationsManagerQueueIdentifier); reintroduce;
    destructor Destroy; override;

    procedure ExecuteModal; override;
    function ShowModal: Integer; override;

    function CloseQuery: Boolean; override;

    class procedure AddEventsListener(Events: TOperationProgressWindowEvents;
                                      FunctionToCall: TOperationProgressWindowEventProc);
    class procedure RemoveEventsListener(Events: TOperationProgressWindowEvents;
                                         FunctionToCall: TOperationProgressWindowEventProc);

    class function IsOpenedFor(AOperationHandle: TOperationHandle): Boolean;
    class function IsOpenedFor(AQueueIdentifier: TOperationsManagerQueueIdentifier): Boolean;
    class procedure ShowFor(AOperationHandle: TOperationHandle; Options: TOperationProgressWindowOptions);
    class procedure ShowFor(AQueueIdentifier: TOperationsManagerQueueIdentifier; Options: TOperationProgressWindowOptions);
  end;

implementation

{$R *.lfm}

uses
   dmCommonData, uLng, uDCUtils, LCLVersion,
   fViewOperations,
   uFileSourceOperationMisc,
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
   uFileSourceSetFilePropertyOperation,
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

procedure Terminate;
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
begin
  StopOperationOrQueue;
  ModalResult:= mrCancel;
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
  pbCurrent.DoubleBuffered:= True;
  pbTotal.DoubleBuffered:= True;
  Self.DoubleBuffered:= True;

  FUpdateTimer := TTimer.Create(Self);
  FUpdateTimer.Interval := 100;
  FUpdateTimer.OnTimer := @OnUpdateTimer;
  FUpdateTimer.Enabled := False;

  if not InitializeOperation then
    CloseDialog;

  // Workaround: TWinControl.WMSize loop detected
  Constraints.MaxWidth:= Screen.Width;
  Constraints.MaxHeight:= Screen.Height;
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

procedure TfrmFileOp.ExecuteModal;
var
  OpManItem: TOperationsManagerItem;
begin
  if FOperationHandle <> InvalidOperationHandle then
  begin
    if Assigned(FUserInterface) then
    begin
      OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
      if Assigned(OpManItem) then OpManItem.Operation.Execute;
    end;
  end;
end;

function TfrmFileOp.ShowModal: Integer;
begin
  BorderStyle:= bsDialog;
  ShowInTaskBar:= stNever;
  btnViewOperations.Visible:= False;
  btnMinimizeToPanel.Visible:= False;
  Result:= inherited ShowModal;
  // Workaround http://doublecmd.sourceforge.net/mantisbt/view.php?id=1323
{$IF DEFINED(DARWIN) and DEFINED(LCLQT)}
  Visible:= True; Visible:= False;
{$ENDIF}
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
begin
  Result := True;

  if FStopOperationOnClose then
    StopOperationOrQueue;
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

class procedure TfrmFileOp.ShowFor(AOperationHandle: TOperationHandle; Options: TOperationProgressWindowOptions);
var
  OperationDialog: TfrmFileOp;
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(AOperationHandle);
  if Assigned(OpManItem) then
  begin
    if not OpManItem.Queue.IsFree then
    begin
      ShowFor(OpManItem.Queue.Identifier, Options);
    end
    else
    begin
      OperationDialog := GetOpenedForm(AOperationHandle);
      if Assigned(OperationDialog) then
        ShowExistingWindow(OperationDialog, Options)
      else
      begin
        OperationDialog := TfrmFileOp.Create(AOperationHandle);
        ShowNewWindow(OperationDialog, Options);
      end;
    end;
  end;
end;

class procedure TfrmFileOp.ShowFor(AQueueIdentifier: TOperationsManagerQueueIdentifier; Options: TOperationProgressWindowOptions);
var
  OperationDialog: TfrmFileOp;
begin
  OperationDialog := GetOpenedForm(AQueueIdentifier);
  if Assigned(OperationDialog) then
  begin
    ShowExistingWindow(OperationDialog, Options);
  end
  else
  begin
    OperationDialog := TfrmFileOp.Create(AQueueIdentifier);
    ShowNewWindow(OperationDialog, Options);
  end;
end;

class procedure TfrmFileOp.ShowExistingWindow(AWindow: TfrmFileOp; Options: TOperationProgressWindowOptions);
begin
  if opwoIfExistsBringToFront in Options then
    AWindow.ShowOnTop;
end;

class procedure TfrmFileOp.ShowNewWindow(AWindow: TfrmFileOp; Options: TOperationProgressWindowOptions);
begin
  if opwoStartMinimized in Options then
  begin
    {$IF lcl_fullversion >= 093100}
    // Workaround for bug in Lazarus 0.9.31 #0021603.
    AWindow.Visible := True;
    AWindow.WindowState := wsMinimized;
    {$ELSE}
    AWindow.WindowState := wsMinimized;
    AWindow.Visible := True;
    {$ENDIF}
  end
  else
    AWindow.Show;
end;

procedure TfrmFileOp.StopOperationOrQueue;
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
      OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
      OpManItem.Operation.AddUserInterface(FUserInterface);
    end;
  end;
end;

procedure TfrmFileOp.InitializeControls(OpManItem: TOperationsManagerItem; FileOpDlgLook: TFileOpDlgLook);
begin
  pnlQueue.Visible := not OpManItem.Queue.IsFree;

  lblFrom.Visible         := fodl_from_lbl in FileOpDlgLook;
  lblFileNameFrom.Visible := lblFrom.Visible;

  lblTo.Visible           := fodl_to_lbl in FileOpDlgLook;
  lblFileNameTo.Visible   := lblTo.Visible;

  pbCurrent.Visible       := fodl_current_pb in FileOpDlgLook;
  pbTotal.Visible         := fodl_total_pb in FileOpDlgLook;

  pbCurrent.ShowInTaskbar := [fodl_current_pb, fodl_total_pb] * FileOpDlgLook = [fodl_current_pb];
  pbTotal.ShowInTaskbar   := fodl_total_pb in FileOpDlgLook;

  lblFileNameFrom.Caption := '';
  lblFileNameTo.Caption   := '';
  lblEstimated.Caption    := #32;
  lblFileCount.Caption    := EmptyStr;
end;

procedure TfrmFileOp.NotifyEvents(Events: TOperationProgressWindowEvents);
var
  Item: PEventsListItem;
  Event: TOperationProgressWindowEvent;
  i: Integer;
begin
  for Event in Events do
  begin
    for i := 0 to EventsListeners[Event].Count - 1 do
    begin
      Item := EventsListeners[Event].Items[i];
      Item^.EventFunction(FOperationHandle, Event);
    end;
  end;
end;

procedure TfrmFileOp.SetPauseGlyph;
begin
  dmComData.ImageList.GetBitmap(1, btnPauseStart.Glyph);
end;

procedure TfrmFileOp.SetPlayGlyph;
begin
  dmComData.ImageList.GetBitmap(0, btnPauseStart.Glyph);
end;

procedure TfrmFileOp.SetLabelCaption(ALabel: TLabel; const AText: String);
begin
  ALabel.Hint := AText;
  ALabel.Caption := MinimizeFilePath(AText, ALabel.Canvas, ALabel.Width);
end;

procedure TfrmFileOp.SetProgressBytes(Operation: TFileSourceOperation;
  ProgressBar: TKASProgressBar; CurrentBytes: Int64; TotalBytes: Int64);
begin
  if (CurrentBytes = -1) then
    ProgressBar.Style := pbstMarquee
  else begin
    if Operation.State = fsosRunning then ProgressBar.Style := pbstNormal;
    ProgressBar.SetProgress(CurrentBytes, TotalBytes,
                            cnvFormatFileSize(CurrentBytes, uoscOperation) + '/' +
                            cnvFormatFileSize(TotalBytes, uoscOperation)
                            );
  end;
end;

procedure TfrmFileOp.SetProgressFiles(Operation: TFileSourceOperation;
  ProgressBar: TKASProgressBar; CurrentFiles: Int64; TotalFiles: Int64);
begin
  if (CurrentFiles = -1) then
    ProgressBar.Style := pbstMarquee
  else begin
    if Operation.State = fsosRunning then ProgressBar.Style := pbstNormal;
    ProgressBar.SetProgress(CurrentFiles, TotalFiles,
                            cnvFormatFileSize(CurrentFiles, uoscNoUnit) + '/' +
                            cnvFormatFileSize(TotalFiles, uoscNoUnit)
                            );
  end;
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
          // Normal view, less than 24 hours of estimated time
          if RemainingTime < 1.0 then
            sEstimated := FormatDateTime('HH:MM:SS', RemainingTime)
          else begin
            sEstimated := IntToStr(Trunc(RemainingTime)) + '``' + FormatDateTime('HH:MM:SS', RemainingTime);
          end;
          sEstimated := Format(rsDlgSpeedTime, [Speed, sEstimated]);
        end
      else
        sEstimated := Format(rsDlgSpeed, [Speed]);
    end;

  lblEstimated.Caption := sEstimated;
end;

procedure TfrmFileOp.InitializeCopyOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_to_lbl, fodl_current_pb, fodl_total_pb]);
end;

procedure TfrmFileOp.InitializeMoveOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_to_lbl, fodl_current_pb, fodl_total_pb]);
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
        fsoSetFileProperty:
          InitializeSetFilePropertyOperation(OpManItem);

        else
          begin
            InitializeControls(OpManItem, [fodl_total_pb]);
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
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_total_pb]);
end;

procedure TfrmFileOp.InitializeCalcStatisticsOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl]);
end;

procedure TfrmFileOp.InitializeSetFilePropertyOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_total_pb]);
end;

procedure TfrmFileOp.InitializeWipeOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_current_pb, fodl_total_pb]);
end;

procedure TfrmFileOp.InitializeSplitOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_to_lbl, fodl_current_pb, fodl_total_pb]);
end;

procedure TfrmFileOp.InitializeCombineOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_to_lbl, fodl_current_pb, fodl_total_pb]);
end;

procedure TfrmFileOp.InitializeCalcChecksumOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_current_pb, fodl_total_pb]);
end;

procedure TfrmFileOp.InitializeTestArchiveOperation(OpManItem: TOperationsManagerItem);
begin
  InitializeControls(OpManItem, [fodl_from_lbl, fodl_to_lbl, fodl_current_pb, fodl_total_pb]);
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
    SetLabelCaption(lblFileNameFrom, CurrentFileFrom);
    SetLabelCaption(lblFileNameTo, CurrentFileTo);

    SetProgressCount(Operation, DoneFiles, TotalFiles);
    SetProgressBytes(Operation, pbCurrent, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(Operation, pbTotal, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, uoscOperation));
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
    SetLabelCaption(lblFileNameFrom, CurrentFileFrom);
    SetLabelCaption(lblFileNameTo, CurrentFileTo);

    SetProgressCount(Operation, DoneFiles, TotalFiles);
    SetProgressBytes(Operation, pbCurrent, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(Operation, pbTotal, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, uoscOperation));
  end;
end;

procedure TfrmFileOp.UpdateOperation(OpManItem: TOperationsManagerItem);
var
  NewCaption: String;
begin
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
    fsoSetFileProperty:
      UpdateSetFilePropertyOperation(OpManItem.Operation);

    else
    begin
      // Operation not currently supported for display.
      // Only show general progress.
      pbTotal.Position := Round(OpManItem.Operation.Progress * pbTotal.Max);
    end;
  end;

  UpdatePauseStartButton(OpManItem);

  if OpManItem.Queue.IsFree then
  begin
    NewCaption := GetProgressString(OpManItem.Operation.Progress) + ' '
                  + OpManItem.Operation.GetDescription(fsoddJob)
                  + GetOperationStateString(OpManItem.Operation.State);
  end
  else
  begin
    if OpManItem.Queue.Paused then
      NewCaption := '[' + IntToStr(OpManItem.Queue.Count) + '] '
                    + OpManItem.Queue.GetDescription(False)
                    + GetOperationStateString(fsosPaused)
    else
      NewCaption := '[' + IntToStr(OpManItem.Queue.Count) + '] '
                    + GetProgressString(OpManItem.Operation.Progress) + ' '
                    + OpManItem.Operation.GetDescription(fsoddJob) + ' - '
                    + OpManItem.Queue.GetDescription(False);

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
    SetLabelCaption(lblFileNameFrom, CurrentFile);

    SetProgressFiles(Operation, pbTotal, DoneFiles, TotalFiles);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(FilesPerSecond, uoscNoUnit));
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
    SetLabelCaption(lblFileNameFrom, CurrentFile);

    SetProgressBytes(Operation, pbCurrent, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(Operation, pbTotal, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, uoscOperation));
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
    SetLabelCaption(lblFileNameFrom ,CurrentFileFrom);
    SetLabelCaption(lblFileNameTo, CurrentFileTo);

    SetProgressBytes(Operation, pbCurrent, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(Operation, pbTotal, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, uoscOperation));
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
    SetLabelCaption(lblFileNameFrom, CurrentFileFrom);
    SetLabelCaption(lblFileNameTo, CurrentFileTo);

    SetProgressBytes(Operation, pbCurrent, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(Operation, pbTotal, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, uoscOperation));
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
    SetLabelCaption(lblFileNameFrom, CurrentFile);
    SetSpeedAndTime(Operation, 0, cnvFormatFileSize(FilesPerSecond, uoscNoUnit));
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
    SetLabelCaption(lblFileNameFrom, CurrentFile);

    SetProgressBytes(Operation, pbCurrent, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(Operation, pbTotal, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, uoscOperation));
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
    SetLabelCaption(lblFileNameFrom, ArchiveFile);
    SetLabelCaption(lblFileNameTo, CurrentFile);

    SetProgressBytes(Operation, pbCurrent, CurrentFileDoneBytes, CurrentFileTotalBytes);
    SetProgressBytes(Operation, pbTotal, DoneBytes, TotalBytes);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(BytesPerSecond, uoscOperation));
  end;
end;

procedure TfrmFileOp.UpdateSetFilePropertyOperation(Operation: TFileSourceOperation);
var
  SetOperation: TFileSourceSetFilePropertyOperation;
  SetStatistics: TFileSourceSetFilePropertyOperationStatistics;
begin
  SetOperation := Operation as TFileSourceSetFilePropertyOperation;
  SetStatistics := SetOperation.RetrieveStatistics;

  with SetStatistics do
  begin
    SetLabelCaption(lblFileNameFrom, CurrentFile);

    SetProgressFiles(Operation, pbTotal, DoneFiles, TotalFiles);
    SetSpeedAndTime(Operation, RemainingTime, cnvFormatFileSize(FilesPerSecond, uoscNoUnit));
  end;
end;

function TfrmFileOp.GetProgressBarStyle: TProgressBarStyle;
begin
  if (pbCurrent.Style = pbstMarquee) and (pbTotal.Style = pbstMarquee) then
    Result:= pbstMarquee
  else
    Result:= pbstNormal;
end;

procedure TfrmFileOp.SetProgressBarStyle(const AValue: TProgressBarStyle);
begin
  pbCurrent.Style:= AValue;
  pbTotal.Style:= AValue;
end;

procedure TfrmFileOp.SetProgressCount(Operation: TFileSourceOperation;
  DoneFiles: Int64; TotalFiles: Int64);
begin
  if (DoneFiles < 0) or (TotalFiles = 0) then
    lblFileCount.Caption := EmptyStr
  else begin
    lblFileCount.Caption := IntToStr(DoneFiles) + ' / ' + IntToStr(TotalFiles);
  end;
end;

initialization
  Initialize;

finalization
  Terminate;

end.
