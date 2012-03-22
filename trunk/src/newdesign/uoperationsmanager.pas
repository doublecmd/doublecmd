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
  SingleQueueId = 1; // TODO: Hard-coded for now

type
  TOperationsManagerQueue = class;

  { TOperationsManagerItem }

  TOperationsManagerItem = class
  strict private
    FHandle       : TOperationHandle;
    FOperation    : TFileSourceOperation;
    FPauseRunning : Boolean;
    FQueue        : TOperationsManagerQueue;
    FThread       : TOperationThread;

  public
    constructor Create(AHandle: TOperationHandle;
                       AOperation: TFileSourceOperation;
                       AThread: TOperationThread);
    destructor Destroy; override;

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
    property PauseRunning: Boolean read FPauseRunning write FPauseRunning;
    property Queue: TOperationsManagerQueue read FQueue;
    property Thread: TOperationThread read FThread;
  end;

  { TOperationsManagerQueue }

  TOperationsManagerQueue = class
  strict private
    FList: TFPList;
    FIdentifier: TOperationsManagerQueueIdentifier;
    function GetIndexByHandle(Handle: TOperationHandle): Integer;
    function GetItem(Index: Integer): TOperationsManagerItem;
    function GetItemByHandle(Handle: TOperationHandle): TOperationsManagerItem;
    function GetOperationsCount: Integer;
    procedure RunNextOperation(Index: Integer);
    procedure RunOperation(Item: TOperationsManagerItem);
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
    function Insert(Item: TOperationsManagerItem; InsertAtFront: Boolean): Integer;
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

    property Count: Integer read GetOperationsCount;
    property Items[Index: Integer]: TOperationsManagerItem read GetItem;
    property ItemByHandle[Handle: TOperationHandle]: TOperationsManagerItem read GetItemByHandle;
    property Identifier: TOperationsManagerQueueIdentifier read FIdentifier;
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
    procedure StartOperation(Item: TOperationsManagerItem);

    {en
       Notifies all listeners that an event has occurred (or multiple events).
    }
    procedure NotifyEvents(Item: TOperationsManagerItem; Events: TOperationManagerEvents);

  public
    constructor Create;
    destructor Destroy; override;

    {en
       Adds an operation to the manager.
       @param(QueueNumber
              Specifies to which queue to put the operation.
              If QueueNumber is FreeOperationsQueue the operation is not put into any queue (a free operation).)
    }
    function AddOperation(Operation: TFileSourceOperation;
                          QueueIdentifier: TOperationsManagerQueueIdentifier = FreeOperationsQueueId;
                          InsertAtFrontOfQueue: Boolean = False): TOperationHandle;

    {en
       Operations retrieved this way can be safely used from the main GUI thread.
       But they should not be stored for longer use, because they
       may be destroyed by the Operations Manager when they finish.
       Operation handle can always be used to query OperationsManager if the
       operation item is still alive.
    }
    function GetItemByHandle(Handle: TOperationHandle): TOperationsManagerItem;
    function GetOrCreateQueue(Identifier: TOperationsManagerQueueIdentifier): TOperationsManagerQueue;

    procedure CancelAll;
    procedure PauseRunning;
    procedure StartRunning;
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
end;

destructor TOperationsManagerItem.Destroy;
begin
  inherited Destroy;
  FOperation.Free;
end;

procedure TOperationsManagerItem.Move(TargetOperation: TOperationHandle; PlaceBefore: Boolean);
var
  TargetItem: TOperationsManagerItem;
begin
  TargetItem := OperationsManager.GetItemByHandle(TargetOperation);
  if Assigned(TargetItem) then
  begin
    SetQueue(TargetItem.Queue);
    TargetItem.Queue.Move(Self, TargetItem, PlaceBefore);
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

procedure TOperationsManagerItem.SetQueue(NewQueue: TOperationsManagerQueue; InsertAtFront: Boolean);
begin
  if (Queue <> NewQueue) and Assigned(NewQueue) then
  begin
    if not Assigned(Queue) or Queue.Remove(Self) then
    begin
      FQueue := NewQueue;
      NewQueue.Insert(Self, InsertAtFront);
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

procedure TOperationsManagerQueue.RunNextOperation(Index: Integer);
var
  Item: TOperationsManagerItem;
begin
  if Count > 0 then
  begin
    if Index = 0 then
    begin
      Item := Items[0];
      if Item.Operation.State in [fsosNotStarted, fsosPaused] then
        RunOperation(Item);
    end;
  end;
end;

procedure TOperationsManagerQueue.RunOperation(Item: TOperationsManagerItem);
begin
  OperationsManager.StartOperation(Item);
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

  if ShouldMove then
  begin
    FList.Move(FromIndex, ToIndex);
    RunNextOperation(ToIndex);
    OperationsManager.NotifyEvents(SourceItem, [omevOperationMoved]);
  end;
end;

function TOperationsManagerQueue.Insert(Item: TOperationsManagerItem; InsertAt: Integer): Integer;
begin
  if InsertAt = -1 then
    InsertAt := FList.Count;
  FList.Insert(InsertAt, Item);
  Result := InsertAt;
  if FIdentifier = FreeOperationsQueueId then
    RunOperation(Item)
  else
    RunNextOperation(InsertAt);
end;

function TOperationsManagerQueue.Insert(Item: TOperationsManagerItem; InsertAtFront: Boolean): Integer;
begin
  if InsertAtFront then
    Result := Insert(Item, 0)  // Insert at the front of the queue.
  else
    Result := Insert(Item);    // Add at the back of the queue.
end;

function TOperationsManagerQueue.Remove(Item: TOperationsManagerItem): Boolean;
var
  Index: Integer;
begin
  Index := FList.Remove(Item);
  Result := Index <> -1;
  if Result and (FIdentifier <> FreeOperationsQueueId) then
    RunNextOperation(Index);
end;

{ TOperationsManager }

constructor TOperationsManager.Create;
var
  Event: TOperationManagerEvent;
begin
  FQueues := TFPList.Create;
  FLastUsedHandle := 0;

  for Event := Low(FEventsListeners) to High(FEventsListeners) do
    FEventsListeners[Event] := TFPList.Create;

  inherited Create;
end;

destructor TOperationsManager.Destroy;
var
  i: Integer;
  OperIndex, QueueIndex: Integer;
  Item: TOperationsManagerItem;
  Event: TOperationManagerEvent;
  Queue: TOperationsManagerQueue;
begin
  inherited Destroy;

  // If any operations still exist, remove listeners as we're destroying the object.
  for QueueIndex := 0 to QueuesCount - 1 do
  begin
    Queue := QueueByIndex[QueueIndex];
    for OperIndex := 0 to Queue.Count - 1 do
    begin
      Item := Queue.Items[OperIndex];
    end;
    Queue.Free;
  end;

  for Event := Low(FEventsListeners) to High(FEventsListeners) do
  begin
    for i := 0 to FEventsListeners[Event].Count - 1 do
      Dispose(PEventsListItem(FEventsListeners[Event].Items[i]));

    FreeAndNil(FEventsListeners[Event]);
  end;

  FreeAndNil(FQueues);
end;

function TOperationsManager.AddOperation(
  Operation: TFileSourceOperation;
  QueueIdentifier: TOperationsManagerQueueIdentifier = FreeOperationsQueueId;
  InsertAtFrontOfQueue: Boolean = False): TOperationHandle;
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
        Item.PauseRunning := False;

        Operation.PreventStart;

        Result := Item.Handle;

        // Set OnTerminate event so that we can cleanup when thread finishes.
        // Or instead of this create a timer for each thread and do:
        //  Thread.WaitFor  (or WaitForThreadTerminate(Thread.ThreadID))
        Thread.OnTerminate := @ThreadTerminatedEvent;

        Item.SetQueue(GetOrCreateQueue(QueueIdentifier), InsertAtFrontOfQueue);

        NotifyEvents(Item, [omevOperationAdded]);

        Thread.Resume;
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
        Queue.Remove(Item);

        if Queue.Count = 0 then
        begin
          FQueues.Remove(Queue);
          Queue.Free;
        end;

        NotifyEvents(Item, [omevOperationRemoved]);

        // Here the operation should not be used anymore
        // (by the thread and by any operations viewer).
        Item.Free;

        Exit;
      end;
    end;
  end;
end;

procedure TOperationsManager.StartOperation(Item: TOperationsManagerItem);
begin
  Item.Operation.Start;
end;

procedure TOperationsManager.CancelAll;
var
  Item: TOperationsManagerItem;
  i: Integer;
begin
  // Cancel all operations
  for i := 0 to OperationsCount - 1 do
  begin
    Item := OperationsManager.GetItemByIndex(i);
    if Assigned(Item) then
      Item.Operation.Stop;
  end;
end;

procedure TOperationsManager.PauseRunning;
var
  Item: TOperationsManagerItem;
  i: Integer;
       //true - operation was runnig
begin
  for i := 0 to OperationsCount - 1 do
  begin
    Item := OperationsManager.GetItemByIndex(i);
    if Assigned(Item) then
      begin
        if Item.Operation.State = fsosRunning then
          begin
            Item.PauseRunning := True; //«апоминаем строку которую приостановили
            Item.Operation.Pause;
          end;
      end;
  end;
end;

procedure TOperationsManager.StartRunning;
var
  Item: TOperationsManagerItem;
  I: Integer;
  StartOp: Boolean = False;
begin
  for I := 0 to OperationsCount - 1 do
  begin
    Item := OperationsManager.GetItemByIndex(i);
    if Assigned(Item) then
      begin
        if Item.PauseRunning = True  then //¬споминаем остановленную операцию и запускаем
          begin
            Item.Operation.Start;
            Item.PauseRunning := False;      // —брасываем пам€ть
            StartOp:= True;                                                  //ѕометка, что есть запущенна€ операци€
          end;
      end;
  end;
  if not StartOp then
  begin
    Item := OperationsManager.GetItemByIndex(0);
    if Assigned(Item) then
      Item.Operation.Start;        //если нет до этого запущенных, то запускаем первую
  end;
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
    Result := Result / OperationsManager.OperationsCount;  // ѕоказываем средний прогресс
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

