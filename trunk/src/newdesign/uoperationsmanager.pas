unit uOperationsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LCLIntf, syncobjs, uLng,
  uOperationThread, uFileSourceOperation, lclproc;

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
     ossDontStart,      //<en Don't start automatically.
     ossAutoStart,      //<en Start automatically.
     ossQueue,          //<en Don't start automatically,
                        //<en unless there are no other operations working.
     ossAutoQueue);     //<en If there are no operations running - autostart.
                        //<en If there are operations running - queue.

const
  OperationStartingStateText: array[TOperationStartingState] of string =
    ('', rsOperDontStart, rsOperAutoStart, rsOperQueue, '');

type

  POperationsManagerEntry = ^TOperationsManagerEntry;
  TOperationsManagerEntry = record
    Thread       : TOperationThread;
    Operation    : TFileSourceOperation;
    Handle       : TOperationHandle;
    StartingState: TOperationStartingState;
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
  TOperationsManager = class
  private
    FOperations: TFPList;         //<en List of TOperationsManagerEntry
    FLastUsedHandle: TOperationHandle;
    FEventsListeners: array[TOperationManagerEvent] of TFPList;

    procedure ThreadTerminatedEvent(Sender: TObject);

    function GetOperationsCount: Integer;

    function GetNextUnusedHandle: TOperationHandle;

    function GetEntryByHandle(Handle: TOperationHandle): POperationsManagerEntry;
    function GetEntryByOperation(Operation: TFileSourceOperation): POperationsManagerEntry;

    {en
       Checks if there is any queued operation and if all other operations
       are stopped, then the next queued operations is started.
    }
    procedure CheckQueuedOperations;

    {en
       Returns @true if there is at least one operation currently running.
    }
    function AreOperationsRunning: Boolean;

    procedure StartOperation(Entry: POperationsManagerEntry);

    procedure AddOperationListeners(Operation: TFileSourceOperation);
    procedure RemoveOperationListeners(Operation: TFileSourceOperation);

    procedure OperationStateChangedEvent(Operation: TFileSourceOperation;
                                         Event: TFileSourceOperationEvent);

    {en
       Notifies all listeners that an event has occurred (or multiple events).
    }
    procedure NotifyEvents(Operation: TFileSourceOperation; Events: TOperationManagerEvents);

  public
    constructor Create;
    destructor Destroy; override;

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
    function GetOperationByIndex(Index: Integer): TFileSourceOperation;
    function GetOperationByHandle(Handle: TOperationHandle): TFileSourceOperation;
    function GetHandleById(Index: Integer): TOperationHandle;
    function GetStartingState(Handle: TOperationHandle): TOperationStartingState;

    {en
       Changes the entry's (and thus operation's) position in the list.
       It is used to change the order of execution of queued operations.
       @param(FromIndex is an index in the operations list of the entry that should be moved.)
       @param(ToIndex is an index in the operations list where the entry should be moved to.)
    }
    procedure MoveOperation(FromIndex: Integer; ToIndex: Integer);

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
  PEventsListEntry = ^TEventsListEntry;
  TEventsListEntry = record
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
  Entry: POperationsManagerEntry;
  Event: TOperationManagerEvent;
begin
  inherited Destroy;

  // If any operations still exist, remove listeners as we're destroying the object.
  for i := 0 to FOperations.Count - 1 do
  begin
    Entry := POperationsManagerEntry(FOperations.Items[i]);
    RemoveOperationListeners(Entry^.Operation);
  end;

  for Event := Low(FEventsListeners) to High(FEventsListeners) do
  begin
    for i := 0 to FEventsListeners[Event].Count - 1 do
      Dispose(PEventsListEntry(FEventsListeners[Event].Items[i]));

    FreeAndNil(FEventsListeners[Event]);
  end;

  FreeAndNil(FOperations);
end;

function TOperationsManager.AddOperation(Operation: TFileSourceOperation;
                                         StartingState: TOperationStartingState): TOperationHandle;
var
  Thread: TOperationThread;
  Entry: POperationsManagerEntry;
begin
  Result := InvalidOperationHandle;

  if Assigned(Operation) then
  begin
    Entry := New(POperationsManagerEntry);
    if Assigned(Entry) then
    try
      Thread := TOperationThread.Create(True, Operation);

      if Assigned(Thread) then
      begin
        if Assigned(Thread.FatalException) then
          raise Thread.FatalException;

        Entry^.Operation := Operation;
        Entry^.Thread := Thread;
        Entry^.Handle := GetNextUnusedHandle;
        Entry^.StartingState := StartingState;

        FOperations.Add(Entry);

        AddOperationListeners(Operation);

        Result := Entry^.Handle;

        // Set OnTerminate event so that we can cleanup when thread finishes.
        // Or instead of this create a timer for each thread and do:
        //  Thread.WaitFor  (or WaitForThreadTerminate(Thread.ThreadID))
        Thread.OnTerminate := @ThreadTerminatedEvent;

        NotifyEvents(Operation, [omevOperationAdded]);

        case StartingState of
          ossAutoStart:
            begin
              StartOperation(Entry);
            end;

          ossAutoQueue:
            begin
              if not AreOperationsRunning then
                StartOperation(Entry)
              else
              begin
                Entry^.StartingState := ossQueue; // change to Queued
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
        Dispose(Entry);

    except
      Dispose(Entry);
    end;
  end;
end;

function TOperationsManager.GetOperationsCount: Integer;
begin
  Result := FOperations.Count;
end;

function TOperationsManager.GetEntryByHandle(Handle: TOperationHandle): POperationsManagerEntry;
var
  Entry: POperationsManagerEntry = nil;
  i: Integer;
begin
  Result := nil;

  if (Handle <> InvalidOperationHandle) then
  begin
    // Search for operation identified by given handle.
    for i := 0 to FOperations.Count - 1 do
    begin
      Entry := POperationsManagerEntry(FOperations.Items[i]);
      if Entry^.Handle = Handle then
      begin
        Result := Entry;
        break;
      end;
    end;
  end;
end;

function TOperationsManager.GetEntryByOperation(Operation: TFileSourceOperation): POperationsManagerEntry;
var
  Entry: POperationsManagerEntry;
  i: Integer;
begin
  Result := nil;

  for i := 0 to FOperations.Count - 1 do
  begin
    Entry := POperationsManagerEntry(FOperations.Items[i]);
    if Entry^.Operation = Operation then
    begin
      Result := Entry;
      Exit;
    end
  end;
end;

function TOperationsManager.GetOperationByIndex(Index: Integer): TFileSourceOperation;
var
  Entry: POperationsManagerEntry = nil;
begin
  if (Index >= 0) and (Index < FOperations.Count) then
  begin
    Entry := POperationsManagerEntry(FOperations.Items[Index]);
    if Assigned(Entry^.Operation) then
      Result := Entry^.Operation;
  end
  else
    Result := nil;
end;

function TOperationsManager.GetOperationByHandle(Handle: TOperationHandle): TFileSourceOperation;
var
  Entry: POperationsManagerEntry = nil;
begin
  Entry := GetEntryByHandle(Handle);
  if Assigned(Entry) then
    Result := Entry^.Operation
  else
    Result := nil;
end;

function TOperationsManager.GetHandleById(Index: Integer): TOperationHandle;
var
  Entry: POperationsManagerEntry = nil;
begin
  if (Index >= 0) and (Index < FOperations.Count) then
  begin
    Entry := POperationsManagerEntry(FOperations.Items[Index]);
    Result := Entry^.Handle;
  end
  else
    Result := InvalidOperationHandle;
end;

function TOperationsManager.GetStartingState(Handle: TOperationHandle): TOperationStartingState;
var
  Entry: POperationsManagerEntry = nil;
begin
  Entry := GetEntryByHandle(Handle);
  if Assigned(Entry) then
    Result := Entry^.StartingState
  else
    Result := ossInvalid;
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
  Entry: POperationsManagerEntry = nil;
  Index: Integer = -1;
begin
  // This function is executed from the GUI thread (through Synchronize).

  Thread := Sender as TOperationThread;

  // Search the terminated thread in the operations list.
  for Index := 0 to FOperations.Count - 1 do
  begin
    Entry := POperationsManagerEntry(FOperations.Items[Index]);
    if Entry^.Thread = Thread then
    begin
      break;
    end;
  end;

  if Assigned(Entry) then
  begin
    NotifyEvents(Entry^.Operation, [omevOperationFinished]);

    FOperations.Delete(Index);

    NotifyEvents(Entry^.Operation, [omevOperationRemoved]);

    Entry^.Thread := nil;  // Thread frees himself automatically on terminate.

    // Here the operation should not be used anymore
    // (by the thread and by any operations viewer).
    FreeAndNil(Entry^.Operation);

    Dispose(Entry);
  end;

  CheckQueuedOperations;
end;

procedure TOperationsManager.CheckQueuedOperations;
var
  i: Integer;
  Entry: POperationsManagerEntry = nil;
begin
  // Should a queued operation start when there are paused operations?
  // How about operations that are waiting for input from user?

  if not AreOperationsRunning then
  begin
    for i := 0 to FOperations.Count - 1 do
    begin
      Entry := POperationsManagerEntry(FOperations.Items[i]);

      if Entry^.StartingState = ossQueue then
      begin
        StartOperation(Entry);
        Exit;
      end;
    end;
  end;
end;

function TOperationsManager.AreOperationsRunning: Boolean;
var
  Entry: POperationsManagerEntry = nil;
  Index: Integer = -1;
begin
  // Search for a running operation.
  for Index := 0 to FOperations.Count - 1 do
  begin
    Entry := POperationsManagerEntry(FOperations.Items[Index]);

    if not (Entry^.Operation.State in [fsosNotStarted, fsosStopped]) then
      Exit(True);  // There is an operation running.
  end;
  Result := False;
end;

procedure TOperationsManager.StartOperation(Entry: POperationsManagerEntry);
begin
  Entry^.StartingState := ossDontStart; // Reset state.
  Entry^.Operation.Start;

  NotifyEvents(Entry^.Operation, [omevOperationStarted]);
end;

procedure TOperationsManager.MoveOperation(FromIndex: Integer; ToIndex: Integer);
var
  Entry: POperationsManagerEntry = nil;
begin
  if (FromIndex >= 0) and (FromIndex < FOperations.Count) and
     (ToIndex >= 0) and (ToIndex < FOperations.Count) then
  begin
    Entry := POperationsManagerEntry(FOperations.Items[FromIndex]);

    // This has to be in exactly this order: first delete then insert.
    FOperations.Delete(FromIndex);
    FOperations.Insert(ToIndex, Entry);
  end;
end;

function TOperationsManager.OperationExists(Operation: TFileSourceOperation): Boolean;
var
  Entry: POperationsManagerEntry = nil;
begin
  Entry := GetEntryByOperation(Operation);
  Result := Assigned(Entry);
end;

procedure TOperationsManager.AddOperationListeners(Operation: TFileSourceOperation);
begin
  Operation.AddEventsListener([fsoevStateChanged], @Self.OperationStateChangedEvent);
end;

procedure TOperationsManager.RemoveOperationListeners(Operation: TFileSourceOperation);
begin
  Operation.RemoveEventsListener([fsoevStateChanged], @Self.OperationStateChangedEvent);
end;

procedure TOperationsManager.OperationStateChangedEvent(Operation: TFileSourceOperation;
                                                        Event: TFileSourceOperationEvent);
var
  Entry: POperationsManagerEntry;
begin
  Entry := GetEntryByOperation(Operation);
  if Assigned(Entry) then
  begin
    if Operation.State <> fsosNotStarted then
    begin
      // Remove queue/auto-queue flag, because the operation is now not in NotStarted state.
      Entry^.StartingState := ossDontStart;
    end;
  end;
end;

procedure TOperationsManager.AddEventsListener(Events: TOperationManagerEvents;
                                               FunctionToCall: TOperationManagerEventNotify);
var
  Entry: PEventsListEntry;
  Event: TOperationManagerEvent;
begin
  for Event := Low(TOperationManagerEvent) to High(TOperationManagerEvent) do
  begin
    if Event in Events then
    begin
      Entry := New(PEventsListEntry);
      Entry^.EventFunction := FunctionToCall;
      FEventsListeners[Event].Add(Entry);
    end;
  end;
end;

procedure TOperationsManager.RemoveEventsListener(Events: TOperationManagerEvents;
                                                  FunctionToCall: TOperationManagerEventNotify);
var
  Entry: PEventsListEntry;
  Event: TOperationManagerEvent;
  i: Integer;
begin
  for Event := Low(TOperationManagerEvent) to High(TOperationManagerEvent) do
  begin
    if Event in Events then
    begin
      for i := 0 to FEventsListeners[Event].Count - 1 do
      begin
        Entry := PEventsListEntry(FEventsListeners[Event].Items[i]);
        if Entry^.EventFunction = FunctionToCall then
        begin
          FEventsListeners[Event].Delete(i);
          Dispose(Entry);
          break;  // break from one for only
        end;
      end;
    end;
  end;
end;

procedure TOperationsManager.NotifyEvents(Operation: TFileSourceOperation;
                                          Events: TOperationManagerEvents);
var
  Entry: PEventsListEntry;
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
        Entry := PEventsListEntry(FEventsListeners[Event].Items[i]);
        Entry^.EventFunction(Operation, Event);
      end;
    end;
  end;
end;

initialization

  OperationsManager := TOperationsManager.Create;

finalization

  FreeAndNil(OperationsManager);

end.

