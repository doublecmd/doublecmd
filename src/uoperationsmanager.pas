{
    Double Commander
    -------------------------------------------------------------------------
    Manager that maintains a list of running file source operations
    and manages queues of operations.

    Copyright (C) 2010-2012  Przemys�aw Nagay (cobines@gmail.com)

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

unit uOperationsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uOperationThread, uFileSourceOperation;

type
  {en Handle to OperationsManager's operation.}
  TOperationHandle = type Longint;
  {en Identifier of OperationsManager's queue.}
  TOperationsManagerQueueIdentifier = type Longint;

const
  InvalidOperationHandle = TOperationHandle(0);
  FreeOperationsQueueId = 0;
  ModalQueueId = Pred(FreeOperationsQueueId);
  SingleQueueId = Succ(FreeOperationsQueueId);

type
  TOperationsManagerQueue = class;

  { TOperationsManagerItem }

  TOperationsManagerItem = class
  strict private
    FHandle       : TOperationHandle;
    FOperation    : TFileSourceOperation;
    FQueue        : TOperationsManagerQueue;
    FThread       : TOperationThread;
    FVisible      : Boolean;

  private
    function RemoveFromQueue: Boolean;
    {en
       Inserts the item into new queue at specific position.
       @param(NewQueue
              To which queue to insert the item. If it is the same queue in which
              the item is currently nothing is performed.)
       @param(TargetOperation
              Handle to another operation in NewQueue near which the item should be placed.)
       @param(PlaceBefore
              If @true then places item before TargetOperation.
              If @false then places item after TargetOperation.)
    }
    procedure SetQueue(NewQueue: TOperationsManagerQueue;
                       TargetOperation: TOperationHandle;
                       PlaceBefore: Boolean);
  public
    constructor Create(AHandle: TOperationHandle;
                       AOperation: TFileSourceOperation;
                       AThread: TOperationThread);
    destructor Destroy; override;

    procedure Start;
    {en
       Moves the item and places it before or after another operation.
       @param(TargetOperation
              Handle to another operation where item should be moved.
              If handle belongs to operation from a different queue then
              the item is moved to that queue and placed before or after the operation.)
       @param(PlaceBefore
              If @true then places item before TargetOperation.
              If @false then places item after TargetOperation.)
    }
    procedure Move(TargetOperation: TOperationHandle; PlaceBefore: Boolean);
    procedure MoveToBottom;
    function  MoveToNewQueue: TOperationsManagerQueueIdentifier;
    procedure MoveToTop;
    procedure SetQueue(NewQueue: TOperationsManagerQueue; InsertAtFront: Boolean = False);

    property Handle: TOperationHandle read FHandle;
    property Operation: TFileSourceOperation read FOperation;
    property Queue: TOperationsManagerQueue read FQueue;
    property Thread: TOperationThread read FThread;
    property Visible: Boolean read FVisible write FVisible;
  end;

  { TOperationsManagerQueue }

  TOperationsManagerQueue = class
  strict private
    FIdentifier: TOperationsManagerQueueIdentifier;
    FList: TFPList;
    FPaused: Boolean;
    function GetIndexByHandle(Handle: TOperationHandle): Integer;
    function GetItem(Index: Integer): TOperationsManagerItem;
    function GetItemByHandle(Handle: TOperationHandle): TOperationsManagerItem;
    function GetOperationsCount: Integer;
  private
    {en
       Inserts new item into the queue.
       @param(InsertAt
              If -1 (default) it adds to the back of the queue.
              If 0 inserts at the front.
              If 0 < InsertAt < Count it inserts at specific position.)
    }
    function Insert(Item: TOperationsManagerItem; InsertAt: Integer = -1): Integer;
    {en
       Inserts new item into the queue.
       @param(InsertAtFront
              If @true then inserts at the front,
              if @false then inserts at the back.)
    }
    function Insert(Item: TOperationsManagerItem;
                    TargetOperation: TOperationHandle;
                    PlaceBefore: Boolean): Integer;
    {en
       Moves item within the queue.
       @param(SourceItem
              Which item should be moved.)
       @param(TargetItem
              SourceItem is moved placed either before or after TargetItem.
              If TargetItem is @nil then SourceItem is moved to the front if
              PlaceBefore is @true and to the back if PlaceBefore is @false.)
       @param(PlaceBefore
              If @true then SourceItem is placed before TargetItem.
              If @false then SourceItem is placed after TargetItem.)
    }
    procedure Move(SourceItem, TargetItem: TOperationsManagerItem; PlaceBefore: Boolean);
    function Remove(Item: TOperationsManagerItem): Boolean;
  public
    constructor Create(AIdentifier: TOperationsManagerQueueIdentifier);
    destructor Destroy; override;

    function GetDescription(IncludeCount: Boolean): String;
    {en
       Returns @true if this queue is a free operations queue.
    }
    function IsFree: Boolean; inline;
    procedure Pause;
    procedure Stop;
    procedure TogglePause;
    procedure UnPause;

    property Count: Integer read GetOperationsCount;
    property Identifier: TOperationsManagerQueueIdentifier read FIdentifier;
    property Items[Index: Integer]: TOperationsManagerItem read GetItem;
    property ItemByHandle[Handle: TOperationHandle]: TOperationsManagerItem read GetItemByHandle;
    property Paused: Boolean read FPaused;
  end;

  TOperationManagerEvent =
    (omevOperationAdded,
     omevOperationRemoved,
     omevOperationMoved);

  TOperationManagerEvents = set of TOperationManagerEvent;

  TOperationManagerEventNotify = procedure(Item: TOperationsManagerItem;
                                           Event: TOperationManagerEvent) of object;

  {en
     Manages file source operations.
     Executes them, stores threads, allows querying active operations
     (meaning operations being executed).
  }

  { TOperationsManager }

  TOperationsManager = class
  private
    FLastUsedHandle: TOperationHandle;
    FEventsListeners: array[TOperationManagerEvent] of TFPList;
    FQueues: TFPList;

    procedure ThreadTerminatedEvent(Sender: TObject);

    function GetItemByOperation(Operation: TFileSourceOperation): TOperationsManagerItem;
    function GetItemByIndex(Index: Integer): TOperationsManagerItem;
    function GetOperationsCount: Integer;
    function GetNextUnusedHandle: TOperationHandle;
    function GetQueueByIndex(Index: Integer): TOperationsManagerQueue;
    function GetQueueByIdentifier(Identifier: TOperationsManagerQueueIdentifier): TOperationsManagerQueue;
    function GetQueuesCount: Integer;

    function MoveToNewQueue(Item: TOperationsManagerItem): TOperationsManagerQueueIdentifier;
    procedure MoveToQueue(Item: TOperationsManagerItem; QueueIdentifier: TOperationsManagerQueueIdentifier);

    {en
       Notifies all listeners that an event has occurred (or multiple events).
    }
    procedure NotifyEvents(Item: TOperationsManagerItem; Events: TOperationManagerEvents);

  public
    constructor Create;
    destructor Destroy; override;

    {en
       Adds an operation to the manager.
       @param(ShowProgress
              If @true automatically shows progress window.)
    }
    function AddOperation(Operation: TFileSourceOperation;
                          ShowProgress: Boolean = True): TOperationHandle;

    {en
       Adds an operation to the manager.
       @param(QueueIdentifier
              Specifies to which queue to put the operation.)
       @param(InsertAtFrontOfQueue
              If @true inserts the operation at the front of the queue,
              if @false inserts at the back of the queue.)
       @param(ShowProgress
              If @true automatically shows progress window.)
    }
    function AddOperation(Operation: TFileSourceOperation;
                          QueueIdentifier: TOperationsManagerQueueIdentifier;
                          InsertAtFrontOfQueue: Boolean;
                          ShowProgress: Boolean = True): TOperationHandle;

    {en
       Adds an operation to the manager.
       Execute operation in the main thread and show progress in modal window.
    }
    function AddOperationModal(Operation: TFileSourceOperation): TOperationHandle;

    {en
       Operations retrieved this way can be safely used from the main GUI thread.
       But they should not be stored for longer use, because they
       may be destroyed by the Operations Manager when they finish.
       Operation handle can always be used to query OperationsManager if the
       operation item is still alive.
    }
    function GetItemByHandle(Handle: TOperationHandle): TOperationsManagerItem;
    function GetOrCreateQueue(Identifier: TOperationsManagerQueueIdentifier): TOperationsManagerQueue;
    function GetNewQueueIdentifier: TOperationsManagerQueueIdentifier;

    procedure PauseAll;
    procedure StopAll;
    procedure UnPauseAll;
    function  AllProgressPoint: Double;

    {en
       Adds a function to call on specific events.
    }
    procedure AddEventsListener(Events: TOperationManagerEvents;
                                FunctionToCall: TOperationManagerEventNotify);

    {en
       Removes a registered function callback for events.
    }
    procedure RemoveEventsListener(Events: TOperationManagerEvents;
                                   FunctionToCall: TOperationManagerEventNotify);

    property OperationsCount: Integer read GetOperationsCount;
    property QueuesCount: Integer read GetQueuesCount;
    property QueueByIndex[Index: Integer]: TOperationsManagerQueue read GetQueueByIndex;
    property QueueByIdentifier[Identifier: TOperationsManagerQueueIdentifier]: TOperationsManagerQueue read GetQueueByIdentifier;
  end;

var
  OperationsManager: TOperationsManager = nil;

implementation

uses
  uDebug, uLng, uFileSourceOperationMisc, uFileSourceProperty, uFileSource;

type
  PEventsListItem = ^TEventsListItem;
  TEventsListItem = record
    EventFunction: TOperationManagerEventNotify;
  end;

{ TOperationsManagerItem }

constructor TOperationsManagerItem.Create(AHandle: TOperationHandle; AOperation: TFileSourceOperation; AThread: TOperationThread);
begin
  FHandle := AHandle;
  FOperation := AOperation;
  FThread := AThread;
  FVisible := True;
end;

destructor TOperationsManagerItem.Destroy;
begin
  inherited Destroy;
  FOperation.Free;
end;

procedure TOperationsManagerItem.Start;
begin
  if (FThread = nil) then
  begin
    FThread := TOperationThread.Create(True, Operation);

    if Assigned(FThread.FatalException) then
      raise FThread.FatalException;

    // Set OnTerminate event so that we can cleanup when thread finishes.
    // Or instead of this create a timer for each thread and do:
    //  Thread.WaitFor  (or WaitForThreadTerminate(Thread.ThreadID))
    FThread.OnTerminate := @OperationsManager.ThreadTerminatedEvent;

    FThread.Start;
  end;
  Operation.Start;
end;

procedure TOperationsManagerItem.Move(TargetOperation: TOperationHandle; PlaceBefore: Boolean);
var
  TargetItem: TOperationsManagerItem;
begin
  TargetItem := OperationsManager.GetItemByHandle(TargetOperation);
  if Assigned(TargetItem) then
  begin
    if Queue = TargetItem.Queue then
      Queue.Move(Self, TargetItem, PlaceBefore)
    else
      SetQueue(TargetItem.Queue, TargetOperation, PlaceBefore);
  end;
end;

procedure TOperationsManagerItem.MoveToBottom;
begin
  Queue.Move(Self, nil, False);
end;

function TOperationsManagerItem.MoveToNewQueue: TOperationsManagerQueueIdentifier;
begin
  Result := OperationsManager.MoveToNewQueue(Self);
end;

procedure TOperationsManagerItem.MoveToTop;
begin
  Queue.Move(Self, nil, True);
end;

function TOperationsManagerItem.RemoveFromQueue: Boolean;
begin
  Result := Queue.Remove(Self);
  if Queue.Count = 0 then
  begin
    OperationsManager.FQueues.Remove(Queue);
    Queue.Free;
  end;
  FQueue := nil;
end;

procedure TOperationsManagerItem.SetQueue(NewQueue: TOperationsManagerQueue; InsertAtFront: Boolean);
begin
  if (Queue <> NewQueue) and Assigned(NewQueue) then
  begin
    if not Assigned(Queue) or RemoveFromQueue then
    begin
      FQueue := NewQueue;
      if InsertAtFront then
        NewQueue.Insert(Self, 0)  // Insert at the front of the queue.
      else
        NewQueue.Insert(Self);    // Add at the back of the queue.

      OperationsManager.NotifyEvents(Self, [omevOperationMoved]);
    end;
  end;
end;

procedure TOperationsManagerItem.SetQueue(NewQueue: TOperationsManagerQueue;
                                          TargetOperation: TOperationHandle;
                                          PlaceBefore: Boolean);
begin
  if (Queue <> NewQueue) and Assigned(NewQueue) then
  begin
    if not Assigned(Queue) or RemoveFromQueue then
    begin
      FQueue := NewQueue;
      NewQueue.Insert(Self, TargetOperation, PlaceBefore);
      OperationsManager.NotifyEvents(Self, [omevOperationMoved]);
    end;
  end;
end;

{ TOperationsManagerQueue }

function TOperationsManagerQueue.GetIndexByHandle(Handle: TOperationHandle): Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if TOperationsManagerItem(Items[Result]).Handle = Handle then
      Exit;
  end;
  Result := -1;
end;

function TOperationsManagerQueue.GetItem(Index: Integer): TOperationsManagerItem;
begin
  Result := TOperationsManagerItem(FList.Items[Index]);
end;

function TOperationsManagerQueue.GetItemByHandle(Handle: TOperationHandle): TOperationsManagerItem;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Result := Items[Index];
    if Result.Handle = Handle then
      Exit;
  end;
  Result := nil;
end;

function TOperationsManagerQueue.GetOperationsCount: Integer;
begin
  Result := FList.Count;
end;

constructor TOperationsManagerQueue.Create(AIdentifier: TOperationsManagerQueueIdentifier);
begin
  FList := TFPList.Create;
  FIdentifier := AIdentifier;
end;

destructor TOperationsManagerQueue.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  for i := 0 to FList.Count - 1 do
    Items[i].Free;
  FList.Free;
end;

function TOperationsManagerQueue.GetDescription(IncludeCount: Boolean): String;
begin
  Result := rsDlgQueue + ' #' + IntToStr(Identifier);
  if IncludeCount then
    Result := Result + ' [' + IntToStr(Count) + ']';
end;

procedure TOperationsManagerQueue.Move(SourceItem, TargetItem: TOperationsManagerItem; PlaceBefore: Boolean);
var
  FromIndex, ToIndex: Integer;
  ShouldMove: Boolean = False;
begin
  FromIndex := GetIndexByHandle(SourceItem.Handle);
  if FromIndex >= 0 then
  begin
    if not Assigned(TargetItem) then
    begin
      if PlaceBefore then
        ToIndex := 0
      else
        ToIndex := FList.Count - 1;
      ShouldMove := True;
    end
    else
    begin
      ToIndex := GetIndexByHandle(TargetItem.Handle);
      if ToIndex >= 0 then
      begin
        if PlaceBefore then
        begin
          if FromIndex < ToIndex then
            Dec(ToIndex);
        end
        else
        begin
          if FromIndex > ToIndex then
            Inc(ToIndex);
        end;
        ShouldMove := True;
      end;
    end;
  end;

  if ShouldMove and (FromIndex <> ToIndex) then
  begin
    if (not Paused) and ((FromIndex = 0) or (ToIndex = 0)) and not IsFree then
      Items[0].Operation.Pause;

    FList.Move(FromIndex, ToIndex);

    if (not Paused) and ((FromIndex = 0) or (ToIndex = 0)) and not IsFree then
      Items[0].Start;

    OperationsManager.NotifyEvents(SourceItem, [omevOperationMoved]);
  end;
end;

function TOperationsManagerQueue.Insert(Item: TOperationsManagerItem; InsertAt: Integer): Integer;
begin
  if InsertAt = -1 then
    InsertAt := FList.Count
  else
  begin
    if (not Paused) and (InsertAt = 0) and not IsFree then
      Items[0].Operation.Pause;
  end;

  FList.Insert(InsertAt, Item);
  Result := InsertAt;

  if (not Paused) and (IsFree or (InsertAt = 0)) then
  begin
    Item.Start;
  end
  else
    Item.Operation.Pause;
end;

function TOperationsManagerQueue.Insert(Item: TOperationsManagerItem; TargetOperation: TOperationHandle; PlaceBefore: Boolean): Integer;
begin
  Result := GetIndexByHandle(TargetOperation);
  if Result >= 0 then
  begin
    if not PlaceBefore then
      Inc(Result);
    Insert(Item, Result);
  end;
end;

function TOperationsManagerQueue.IsFree: Boolean;
begin
  Result := (FIdentifier = FreeOperationsQueueId) or (FIdentifier = ModalQueueId);
end;

procedure TOperationsManagerQueue.Pause;
var
  Index: Integer;
begin
  if IsFree then
  begin
    for Index := 0 to Count - 1 do
      Items[Index].Operation.Pause;
  end
  else
  begin
    FPaused := True;
    if Count > 0 then
      Items[0].Operation.Pause;
  end;
end;

function TOperationsManagerQueue.Remove(Item: TOperationsManagerItem): Boolean;
var
  Index: Integer;
begin
  Index := FList.Remove(Item);
  Result := Index <> -1;
  if Result and
     (not Paused) and
     (not IsFree) and
     (Index = 0) and (Count > 0) then
  begin
    Items[0].Start;
  end;
end;

procedure TOperationsManagerQueue.Stop;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Operation.Stop;
end;

procedure TOperationsManagerQueue.TogglePause;
begin
  if Paused then
    UnPause
  else
    Pause;
end;

procedure TOperationsManagerQueue.UnPause;
var
  Index: Integer;
begin
  if IsFree then
  begin
    for Index := 0 to Count - 1 do
      Items[Index].Start;
  end
  else
  begin
    if Count > 0 then
      Items[0].Start;
    FPaused := False;
  end;
end;

{ TOperationsManager }

constructor TOperationsManager.Create;
var
  Event: TOperationManagerEvent;
begin
  FQueues := TFPList.Create;
  FLastUsedHandle := InvalidOperationHandle;

  for Event := Low(FEventsListeners) to High(FEventsListeners) do
    FEventsListeners[Event] := TFPList.Create;

  inherited Create;
end;

destructor TOperationsManager.Destroy;
var
  i: Integer;
  Event: TOperationManagerEvent;
begin
  inherited Destroy;

  for Event := Low(FEventsListeners) to High(FEventsListeners) do
  begin
    for i := 0 to FEventsListeners[Event].Count - 1 do
      Dispose(PEventsListItem(FEventsListeners[Event].Items[i]));

    FreeAndNil(FEventsListeners[Event]);
  end;

  if QueuesCount > 0 then
    DCDebug('Warning: Destroying Operations Manager with active operations!');

  FreeAndNil(FQueues);
end;

function TOperationsManager.AddOperation(Operation: TFileSourceOperation; ShowProgress: Boolean): TOperationHandle;
begin
  if fspListOnMainThread in (Operation.FileSource as IFileSource).Properties then
    Result := AddOperationModal(Operation)
  else begin
    Result := AddOperation(Operation, FreeOperationsQueueId, False, ShowProgress);
  end;
end;

function TOperationsManager.AddOperation(
  Operation: TFileSourceOperation;
  QueueIdentifier: TOperationsManagerQueueIdentifier;
  InsertAtFrontOfQueue: Boolean;
  ShowProgress: Boolean = True): TOperationHandle;
var
  Item: TOperationsManagerItem;
begin
  if QueueIdentifier = ModalQueueId then
  begin
    Result:= AddOperationModal(Operation);
    Exit;
  end;

  Result := InvalidOperationHandle;

  if Assigned(Operation) then
  begin
    Item := TOperationsManagerItem.Create(GetNextUnusedHandle, Operation, nil);
    if Assigned(Item) then
    try
      Operation.PreventStart;

      Result := Item.Handle;

      Item.Visible := ShowProgress;

      Item.SetQueue(GetOrCreateQueue(QueueIdentifier), InsertAtFrontOfQueue);

      NotifyEvents(Item, [omevOperationAdded]);

      if ShowProgress then ShowOperation(Item);

    except
      Item.Free;
    end;
  end;
end;

function TOperationsManager.AddOperationModal(Operation: TFileSourceOperation): TOperationHandle;
var
  Thread: TOperationThread;
  Item: TOperationsManagerItem;
begin
  Result := InvalidOperationHandle;

  if Assigned(Operation) then
  begin
    Thread := TOperationThread.Create(True, Operation);
    if Assigned(Thread) then
    begin
      if Assigned(Thread.FatalException) then
        raise Thread.FatalException;

      Item := TOperationsManagerItem.Create(GetNextUnusedHandle, Operation, Thread);
      if Assigned(Item) then
      try
        Operation.PreventStart;

        Result := Item.Handle;

        Item.SetQueue(GetOrCreateQueue(ModalQueueId), False);

        NotifyEvents(Item, [omevOperationAdded]);

        ShowOperationModal(Item);

        ThreadTerminatedEvent(Thread);

      except
        Item.Free;
      end;
    end;
  end;
end;

function TOperationsManager.GetOperationsCount: Integer;
var
  QueueIndex: Integer;
  Queue: TOperationsManagerQueue;
begin
  Result := 0;
  for QueueIndex := 0 to QueuesCount - 1 do
  begin
    Queue := QueueByIndex[QueueIndex];
    Inc(Result, Queue.Count);
  end;
end;

function TOperationsManager.GetQueuesCount: Integer;
begin
  Result := FQueues.Count;
end;

function TOperationsManager.MoveToNewQueue(Item: TOperationsManagerItem): TOperationsManagerQueueIdentifier;
var
  NewQueueId: TOperationsManagerQueueIdentifier;
  NewQueue: TOperationsManagerQueue;
begin
  for NewQueueId := Succ(FreeOperationsQueueId) to MaxInt do
  begin
    if not Assigned(QueueByIdentifier[NewQueueId]) then
    begin
      NewQueue := GetOrCreateQueue(NewQueueId);
      Item.SetQueue(NewQueue);
      Exit(NewQueueId);
    end;
  end;
end;

function TOperationsManager.GetItemByHandle(Handle: TOperationHandle): TOperationsManagerItem;
var
  QueueIndex: Integer;
  Queue: TOperationsManagerQueue;
begin
  if Handle <> InvalidOperationHandle then
  begin
    for QueueIndex := 0 to QueuesCount - 1 do
    begin
      Queue  := QueueByIndex[QueueIndex];
      Result := Queue.ItemByHandle[Handle];
      if Assigned(Result) then
        Exit;
    end;
  end;
  Result := nil;
end;

function TOperationsManager.GetItemByOperation(Operation: TFileSourceOperation): TOperationsManagerItem;
var
  OperIndex, QueueIndex: Integer;
  Item: TOperationsManagerItem;
  Queue: TOperationsManagerQueue;
begin
  for QueueIndex := 0 to QueuesCount - 1 do
  begin
    Queue := QueueByIndex[QueueIndex];
    for OperIndex := 0 to Queue.Count - 1 do
    begin
      Item := Queue.Items[OperIndex];
      if Item.Operation = Operation then
        Exit(Item);
    end;
  end;
  Result := nil;
end;

function TOperationsManager.GetItemByIndex(Index: Integer): TOperationsManagerItem;
var
  OperIndex, QueueIndex: Integer;
  Queue: TOperationsManagerQueue;
  Counter: Integer;
begin
  Counter := 0;
  for QueueIndex := 0 to QueuesCount - 1 do
  begin
    Queue := QueueByIndex[QueueIndex];
    for OperIndex := 0 to Queue.Count - 1 do
    begin
      if Counter = Index then
        Exit(Queue.Items[OperIndex]);
      Inc(Counter);
    end;
  end;
  Result := nil;
end;

function TOperationsManager.GetOrCreateQueue(Identifier: TOperationsManagerQueueIdentifier): TOperationsManagerQueue;
begin
  Result := QueueByIdentifier[Identifier];
  if not Assigned(Result) then
  begin
    Result := TOperationsManagerQueue.Create(Identifier);
    FQueues.Add(Result);
  end;
end;

function TOperationsManager.GetNewQueueIdentifier: TOperationsManagerQueueIdentifier;
var
  NewQueueId: TOperationsManagerQueueIdentifier;
begin
  for NewQueueId := Succ(FreeOperationsQueueId) to MaxInt do
  begin
    if not Assigned(QueueByIdentifier[NewQueueId]) then
    begin
      Exit(NewQueueId);
    end;
  end;
end;

procedure TOperationsManager.MoveToQueue(Item: TOperationsManagerItem; QueueIdentifier: TOperationsManagerQueueIdentifier);
var
  Queue: TOperationsManagerQueue;
begin
  Queue := GetOrCreateQueue(QueueIdentifier);
  Item.SetQueue(Queue);
end;

function TOperationsManager.GetNextUnusedHandle: TOperationHandle;
begin
  // Handles are consecutively incremented.
  // Even if they overflow there is little probability that
  // there will be that many operations.
  Result := InterLockedIncrement(FLastUsedHandle);
  if Result = InvalidOperationHandle then
    Result := InterLockedIncrement(FLastUsedHandle);
end;

function TOperationsManager.GetQueueByIndex(Index: Integer): TOperationsManagerQueue;
begin
  if (Index >= 0) and (Index < FQueues.Count) then
    Result := TOperationsManagerQueue(FQueues.Items[Index])
  else
    Result := nil;
end;

function TOperationsManager.GetQueueByIdentifier(Identifier: TOperationsManagerQueueIdentifier): TOperationsManagerQueue;
var
  i: Integer;
  Queue: TOperationsManagerQueue;
begin
  for i := 0 to FQueues.Count - 1 do
  begin
    Queue := QueueByIndex[i];
    if Queue.Identifier = Identifier then
      Exit(Queue);
  end;
  Result := nil;
end;

procedure TOperationsManager.ThreadTerminatedEvent(Sender: TObject);
var
  Thread: TOperationThread;
  Item: TOperationsManagerItem;
  OperIndex, QueueIndex: Integer;
  Queue: TOperationsManagerQueue;
begin
  // This function is executed from the GUI thread (through Synchronize).

  Thread := Sender as TOperationThread;

  // Search the terminated thread in the operations list.
  for QueueIndex := 0 to QueuesCount - 1 do
  begin
    Queue := QueueByIndex[QueueIndex];
    for OperIndex := 0 to Queue.Count - 1 do
    begin
      Item := TOperationsManagerItem(Queue.Items[OperIndex]);
      if Item.Thread = Thread then
      begin
        Item.RemoveFromQueue;

        NotifyEvents(Item, [omevOperationRemoved]);

        // Here the operation should not be used anymore
        // (by the thread and by any operations viewer).
        Item.Free;

        Exit;
      end;
    end;
  end;
end;

procedure TOperationsManager.PauseAll;
var
  i: Integer;
begin
  for i := 0 to QueuesCount - 1 do
    OperationsManager.QueueByIndex[i].Pause;
end;

procedure TOperationsManager.StopAll;
var
  i: Integer;
begin
  for i := 0 to QueuesCount - 1 do
    OperationsManager.QueueByIndex[i].Stop;
end;

procedure TOperationsManager.UnPauseAll;
var
  i: Integer;
begin
  for i := 0 to QueuesCount - 1 do
    OperationsManager.QueueByIndex[i].UnPause;
end;

function TOperationsManager.AllProgressPoint: Double;
var
  Item: TOperationsManagerItem;
  i: Integer;
begin
  Result := 0;
  if OperationsManager.OperationsCount <> 0 then
  begin
    for i := 0 to OperationsCount - 1 do
    begin
      Item := OperationsManager.GetItemByIndex(i);
      if Assigned(Item) then
        Result := Result + Item.Operation.Progress;  // calculate allProgressBar
    end;
    Result := Result / OperationsManager.OperationsCount;  // ���������� ������� ��������
  end;
end;

procedure TOperationsManager.AddEventsListener(Events: TOperationManagerEvents;
                                               FunctionToCall: TOperationManagerEventNotify);
var
  Item: PEventsListItem;
  Event: TOperationManagerEvent;
begin
  for Event := Low(TOperationManagerEvent) to High(TOperationManagerEvent) do
  begin
    if Event in Events then
    begin
      Item := New(PEventsListItem);
      Item^.EventFunction := FunctionToCall;
      FEventsListeners[Event].Add(Item);
    end;
  end;
end;

procedure TOperationsManager.RemoveEventsListener(Events: TOperationManagerEvents;
                                                  FunctionToCall: TOperationManagerEventNotify);
var
  Item: PEventsListItem;
  Event: TOperationManagerEvent;
  i: Integer;
begin
  for Event := Low(TOperationManagerEvent) to High(TOperationManagerEvent) do
  begin
    if Event in Events then
    begin
      for i := 0 to FEventsListeners[Event].Count - 1 do
      begin
        Item := PEventsListItem(FEventsListeners[Event].Items[i]);
        if Item^.EventFunction = FunctionToCall then
        begin
          FEventsListeners[Event].Delete(i);
          Dispose(Item);
          break;  // break from one for only
        end;
      end;
    end;
  end;
end;

procedure TOperationsManager.NotifyEvents(Item: TOperationsManagerItem; Events: TOperationManagerEvents);
var
  EventItem: PEventsListItem;
  Event: TOperationManagerEvent;
  i: Integer;
begin
  for Event := Low(TOperationManagerEvent) to High(TOperationManagerEvent) do
  begin
    if Event in Events then
    begin
      // Call each listener function.
      for i := 0 to FEventsListeners[Event].Count - 1 do
      begin
        EventItem := PEventsListItem(FEventsListeners[Event].Items[i]);
        EventItem^.EventFunction(Item, Event);
      end;
    end;
  end;
end;

initialization

  OperationsManager := TOperationsManager.Create;

finalization

  FreeAndNil(OperationsManager);

end.

