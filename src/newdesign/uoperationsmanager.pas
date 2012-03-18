unit uOperationsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLng, uOperationThread, uFileSourceOperation;

type

  {en Handle to OperationsManager's operation.}
  TOperationHandle = Longint;

const
  InvalidOperationHandle = TOperationHandle(0);

type

  {en
     Possible options when adding a new operation.
  }
  TOperationStartingState =
    (ossInvalid,
     ossManualStart,    //<en Don't start automatically. Must be explicitly started.
     ossAutoStart,      //<en Start automatically, regardless if any operations are currently running.
     ossQueueFirst,     //<en Don't start automatically,
                        //<en unless there are no other operations working.
                        //<en Will be started when there are no operation running.
                        //<en This option will put the operation to the head of the queue.
     ossQueueLast,      //<en Same as ossQueueFirst, but this option will put the
                        //<en operation to the back of the queue.
     ossQueueIn         //<en insert operation to queue.
    );

const
  OperationStartingStateText: array[TOperationStartingState] of string =
    ('', rsOperManualStart, rsOperAutoStart, rsOperQueue, rsOperQueue, rsOperQueue);

type

  TOperationsManagerItem = class
  private
    FThread       : TOperationThread;
    FOperation    : TFileSourceOperation;
    FHandle       : TOperationHandle;
    FStartingState: TOperationStartingState;
    FForm         : Boolean;
    FPauseRunning : Boolean;
  public
    property Form: Boolean read FForm write FForm;
    property Handle: TOperationHandle read FHandle;
    property Operation: TFileSourceOperation read FOperation;
    property PauseRunning: Boolean read FPauseRunning write FPauseRunning;
    property Thread: TOperationThread read FThread;
  end;

  TOperationManagerEvent =
    (omevOperationAdded,
     omevOperationRemoved,
     omevOperationStarted,
     omevOperationFinished);

  TOperationManagerEvents = set of TOperationManagerEvent;

  TOperationManagerEventNotify = procedure(Operation: TFileSourceOperation;
                                           Event: TOperationManagerEvent) of object;

  {en
     Manages file source operations.
     Executes them, stores threads, allows querying active operations
     (meaning operations being executed).
  }

  { TOperationsManager }

  TOperationsManager = class
  private
    FOperations: TFPList;         //<en List of TOperationsManagerItem
    FLastUsedHandle: TOperationHandle;
    FEventsListeners: array[TOperationManagerEvent] of TFPList;

    procedure ThreadTerminatedEvent(Sender: TObject);

    function GetOperationsCount: Integer;

    function GetNextUnusedHandle: TOperationHandle;

    {en
       Returns @true if there is at least one operation currently running.
    }
    function AreOperationsRunning: Boolean;

    procedure StartOperation(Item: TOperationsManagerItem);

    procedure AddOperationListeners(Operation: TFileSourceOperation);
    procedure RemoveOperationListeners(Operation: TFileSourceOperation);

    procedure OperationStateChangedEvent(Operation: TFileSourceOperation;
                                         State: TFileSourceOperationState);

    {en
       Notifies all listeners that an event has occurred (or multiple events).
    }
    procedure NotifyEvents(Operation: TFileSourceOperation; Events: TOperationManagerEvents);

  public
    constructor Create;
    destructor Destroy; override;

    {en
       Checks if there is any queued operation and if all other operations
       are stopped, then the next queued operations is started.
    }
    procedure CheckQueuedOperations;

    {en
       Add or remove operation from queue by Handle
    }

    procedure InQueue (Handle: TOperationHandle; setQueue: boolean);

    function AddOperation(Operation: TFileSourceOperation;
                          StartingState: TOperationStartingState): TOperationHandle;

    {en
       Operations retrieved this way can be safely used from the main GUI thread.
       But they should not be stored for longer use, because they
       may be destroyed by the Operations Manager when they finish.
       Operation handle can always be used to safely query OperationsManager
       for a specific operation.
       Also OperationExists function can be used to query OperationsManager
       if the given pointer to a operation is still registered (and thus not
       yet destroyed).
    }
    function GetItemByHandle(Handle: TOperationHandle): TOperationsManagerItem;
    function GetItemByOperation(Operation: TFileSourceOperation): TOperationsManagerItem;
    function GetItemByIndex(Index: Integer): TOperationsManagerItem;

    {en
       Changes the Item's (and thus operation's) position in the list.
       It is used to change the order of execution of queued operations.
       @param(FromIndex is an index in the operations list of the Item that should be moved.)
       @param(ToIndex is an index in the operations list where the Item should be moved to.)
    }
    procedure MoveOperation(FromIndex: Integer; ToIndex: Integer);

    procedure CancelAll;
    procedure StartAll;
    procedure PauseAll;
    procedure PauseRunning;
    procedure StartRunning;
    function  AllProgressPoint: Double;

    {en
       This function is used to check if the pointer to an operation is still
       valid. If an operation is registered in OperationsManager the function
       returns @true.
       @param(Operation is the pointer which should be checked.)
    }
    function OperationExists(Operation: TFileSourceOperation): Boolean;

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
  end;

var
  OperationsManager: TOperationsManager = nil;

implementation

type
  PEventsListItem = ^TEventsListItem;
  TEventsListItem = record
    EventFunction: TOperationManagerEventNotify;
  end;

constructor TOperationsManager.Create;
var
  Event: TOperationManagerEvent;
begin
  FOperations := TFPList.Create;
  FLastUsedHandle := 0;

  for Event := Low(FEventsListeners) to High(FEventsListeners) do
    FEventsListeners[Event] := TFPList.Create;

  inherited Create;
end;

destructor TOperationsManager.Destroy;
var
  i: Integer;
  Item: TOperationsManagerItem;
  Event: TOperationManagerEvent;
begin
  inherited Destroy;

  // If any operations still exist, remove listeners as we're destroying the object.
  for i := 0 to FOperations.Count - 1 do
  begin
    Item := TOperationsManagerItem(FOperations.Items[i]);
    RemoveOperationListeners(Item.Operation);
    Item.Free;
  end;

  for Event := Low(FEventsListeners) to High(FEventsListeners) do
  begin
    for i := 0 to FEventsListeners[Event].Count - 1 do
      Dispose(PEventsListItem(FEventsListeners[Event].Items[i]));

    FreeAndNil(FEventsListeners[Event]);
  end;

  FreeAndNil(FOperations);
end;

function TOperationsManager.AddOperation(Operation: TFileSourceOperation;
                                         StartingState: TOperationStartingState): TOperationHandle;
var
  Thread: TOperationThread;
  Item: TOperationsManagerItem;
begin
  Result := InvalidOperationHandle;

  if Assigned(Operation) then
  begin
    Item := TOperationsManagerItem.Create;
    if Assigned(Item) then
    try
      Thread := TOperationThread.Create(True, Operation);

      if Assigned(Thread) then
      begin
        if Assigned(Thread.FatalException) then
          raise Thread.FatalException;

        Item.FOperation := Operation;
        Item.FThread := Thread;
        Item.FHandle := GetNextUnusedHandle;
        Item.FStartingState := StartingState;
        Item.FForm := True;
        Item.FPauseRunning := False;

        if StartingState = ossQueueFirst then
          FOperations.Insert(0, Item)  // Insert at the top of the queue.
        else
          FOperations.Add(Item);       // Add at the back of the queue.

        AddOperationListeners(Operation);

        Result := Item.Handle;

        // Set OnTerminate event so that we can cleanup when thread finishes.
        // Or instead of this create a timer for each thread and do:
        //  Thread.WaitFor  (or WaitForThreadTerminate(Thread.ThreadID))
        Thread.OnTerminate := @ThreadTerminatedEvent;

        NotifyEvents(Operation, [omevOperationAdded]);

        case StartingState of
          ossAutoStart:
            begin
              StartOperation(Item);
            end;

          ossQueueFirst, ossQueueLast, ossQueueIn:
            begin
              if not AreOperationsRunning then
                StartOperation(Item)
              else
              begin
                // It will be started later when currently running operations finish.
                Operation.PreventStart;
              end;
            end;

          else
            // It will be started by some user trigger.
            Operation.PreventStart;
        end;

        Thread.Resume;
      end
      else
        Item.Free;

    except
      Item.Free;
    end;
  end;
end;

function TOperationsManager.GetOperationsCount: Integer;
begin
  Result := FOperations.Count;
end;

function TOperationsManager.GetItemByHandle(Handle: TOperationHandle): TOperationsManagerItem;
var
  Item: TOperationsManagerItem = nil;
  i: Integer;
begin
  Result := nil;

  if (Handle <> InvalidOperationHandle) then
  begin
    // Search for operation identified by given handle.
    for i := 0 to FOperations.Count - 1 do
    begin
      Item := TOperationsManagerItem(FOperations.Items[i]);
      if Item.Handle = Handle then
      begin
        Result := Item;
        break;
      end;
    end;
  end;
end;

function TOperationsManager.GetItemByOperation(Operation: TFileSourceOperation): TOperationsManagerItem;
var
  Item: TOperationsManagerItem;
  i: Integer;
begin
  Result := nil;

  for i := 0 to FOperations.Count - 1 do
  begin
    Item := TOperationsManagerItem(FOperations.Items[i]);
    if Item.Operation = Operation then
    begin
      Result := Item;
      Exit;
    end
  end;
end;

function TOperationsManager.GetItemByIndex(Index: Integer): TOperationsManagerItem;
begin
  if (Index >= 0) and (Index < FOperations.Count) then
    Result := TOperationsManagerItem(FOperations.Items[Index])
  else
    Result := nil;
end;

procedure TOperationsManager.InQueue (Handle: TOperationHandle; setQueue: boolean);
var
  Item: TOperationsManagerItem = nil;
begin
  Item := GetItemByHandle(Handle);
  if Assigned(Item) then
  begin
    if setQueue = true then
      Item.FStartingState := ossQueueIn
    else
      Item.FStartingState := ossManualStart;
  end;
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

procedure TOperationsManager.ThreadTerminatedEvent(Sender: TObject);
var
  Thread: TOperationThread;
  Item: TOperationsManagerItem = nil;
  Index: Integer = -1;
begin
  // This function is executed from the GUI thread (through Synchronize).

  Thread := Sender as TOperationThread;

  // Search the terminated thread in the operations list.
  for Index := 0 to FOperations.Count - 1 do
  begin
    Item := TOperationsManagerItem(FOperations.Items[Index]);
    if Item.Thread = Thread then
    begin
      NotifyEvents(Item.Operation, [omevOperationFinished]);

      FOperations.Delete(Index);

      NotifyEvents(Item.Operation, [omevOperationRemoved]);

      Item.FThread := nil;  // Thread frees itself automatically on terminate.

      // Here the operation should not be used anymore
      // (by the thread and by any operations viewer).
      FreeAndNil(Item.FOperation);

      Item.Free;

      Break;
    end;
  end;

  CheckQueuedOperations;
end;

procedure TOperationsManager.CheckQueuedOperations;
var
  i: Integer;
  Item: TOperationsManagerItem = nil;
begin
  // Should a queued operation start when there are paused operations?
  // How about operations that are waiting for input from user?

  if not AreOperationsRunning then
  begin
    for i := 0 to FOperations.Count - 1 do
    begin
      Item := TOperationsManagerItem(FOperations.Items[i]);

      if (Item.FStartingState in [ossQueueFirst, ossQueueLast, ossQueueIn]) then

      //  and  (Item.Operation.State = fsosNotStarted)
      begin
        StartOperation(Item);
        Exit;
      end;
    end;
  end;
end;

function TOperationsManager.AreOperationsRunning: Boolean;
var
  Item: TOperationsManagerItem = nil;
  Index: Integer = -1;
begin
  // Search for a running operation.
  for Index := 0 to FOperations.Count - 1 do
  begin
    Item := TOperationsManagerItem(FOperations.Items[Index]);
           // (Item.Operation.State in [fsosNotStarted, fsosStopped])
    if Item.Operation.State = fsosRunning then
      Exit(True);  // There is an operation running.
  end;
  Result := False;
end;

procedure TOperationsManager.StartOperation(Item: TOperationsManagerItem);
begin
  Item.FStartingState := ossManualStart; // Reset state.
  Item.Operation.Start;

  NotifyEvents(Item.Operation, [omevOperationStarted]);
end;

procedure TOperationsManager.MoveOperation(FromIndex: Integer; ToIndex: Integer);
var
  Item: TOperationsManagerItem = nil;
begin
  if (FromIndex >= 0) and (FromIndex < FOperations.Count) and
     (ToIndex >= 0) and (ToIndex < FOperations.Count) then
  begin
    Item := TOperationsManagerItem(FOperations.Items[FromIndex]);

    // This has to be in exactly this order: first delete then insert.
    FOperations.Delete(FromIndex);
    FOperations.Insert(ToIndex, Item);
  end;
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

procedure TOperationsManager.StartAll;
var
  Item: TOperationsManagerItem;
  i: Integer;
begin
  // Start all operations
  for i := 0 to OperationsCount - 1 do
  begin
    Item := OperationsManager.GetItemByIndex(i);
    if Assigned(Item) then
      Item.Operation.Start;
  end;
end;

procedure TOperationsManager.PauseAll;
var
  Item: TOperationsManagerItem;
  i: Integer;
begin
  // Pause all operations
  for i := 0 to OperationsCount do
  begin
    Item := OperationsManager.GetItemByIndex(i);
    if Assigned(Item) then
      Item.Operation.Pause;
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

function TOperationsManager.OperationExists(Operation: TFileSourceOperation): Boolean;
var
  Item: TOperationsManagerItem = nil;
begin
  Item := GetItemByOperation(Operation);
  Result := Assigned(Item);
end;

procedure TOperationsManager.AddOperationListeners(Operation: TFileSourceOperation);
begin
  Operation.AddStateChangedListener([fsosStarting], @OperationStateChangedEvent);
end;

procedure TOperationsManager.RemoveOperationListeners(Operation: TFileSourceOperation);
begin
  Operation.RemoveStateChangedListener(fsosAllStates, @OperationStateChangedEvent);
end;

procedure TOperationsManager.OperationStateChangedEvent(Operation: TFileSourceOperation;
                                                        State: TFileSourceOperationState);
var
  Item: TOperationsManagerItem;
begin
  Item := GetItemByOperation(Operation);
  if Assigned(Item) then
  begin
    if State = fsosStarting then
    begin
      // Remove 'queue' flag, because the operation was manually started by the user.
      Item.FStartingState := ossManualStart;
      // Listener is not needed anymore.
      Operation.RemoveStateChangedListener(fsosAllStates, @OperationStateChangedEvent);
    end;
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

procedure TOperationsManager.NotifyEvents(Operation: TFileSourceOperation;
                                          Events: TOperationManagerEvents);
var
  Item: PEventsListItem;
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
        Item := PEventsListItem(FEventsListeners[Event].Items[i]);
        Item^.EventFunction(Operation, Event);
      end;
    end;
  end;
end;

initialization

  OperationsManager := TOperationsManager.Create;

finalization

  FreeAndNil(OperationsManager);

end.

