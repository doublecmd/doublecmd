unit uOperationsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LCLIntf, syncobjs,
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
    (ossDontStart,      //<en Don't start automatically.
     ossAutoStart,      //<en Start automatically.
     ossQueue);         //<en Don't start automatically,
                        //<en unless there are no other operations working.

  POperationsManagerEntry = ^TOperationsManagerEntry;
  TOperationsManagerEntry = record
    Thread       : TOperationThread;
    Operation    : TFileSourceOperation;
    Handle       : TOperationHandle;
    StartingState: TOperationStartingState;
  end;

  TOperationManagerEvent = procedure(Operation: TFileSourceOperation) of object;

  {en
     Manages file source operations.
     Executes them, stores threads, allows querying active operations
     (meaning operations being executed).
  }
  TOperationsManager = class
  private
    FOperations: TFPList;         //<en List of TOperationsManagerEntry
    FLock: TCriticalSection;
    FLastUsedHandle: TOperationHandle;

    // Events follow.
    // (do this with multiple listeners, so many viewers can look through active operations).

    FOnOperationAdded   : TOperationManagerEvent;
    FOnOperationRemoved : TOperationManagerEvent;

    // These for are for current operation state.
    // Or make one event: OperationStateChanged and caller will query operation for state?
    FOnOperationStarted : TOperationManagerEvent;
    FOnOperationFinished: TOperationManagerEvent;  // Where to put reason of stopping?
    FOnOperationPaused  : TOperationManagerEvent;
    FOnOperationResumed : TOperationManagerEvent;

    procedure ThreadTerminatedEvent(Sender: TObject);

    function GetOperationsCount: Integer;

    function GetNextUnusedHandle: TOperationHandle;

  public
    constructor Create;
    destructor Destroy; override;

    function AddOperation(Operation: TFileSourceOperation;
                          StartingState: TOperationStartingState): TOperationHandle;

    {en
       Operations retrieved this way can be safely used from the main GUI thread.
       But they should not be stored for longer use, because they
       may be destroyed by the Operations Manager when they finish.
    }
    function GetOperationByIndex(Index: Integer): TFileSourceOperation;
    function GetOperationByHandle(Handle: TOperationHandle): TFileSourceOperation;

    function GetHandleById(Index: Integer): TOperationHandle;

    property OperationsCount: Integer read GetOperationsCount;

    // Events.
    property OnOperationAdded   : TOperationManagerEvent read FOnOperationAdded write FOnOperationAdded;
    property OnOperationRemoved : TOperationManagerEvent read FOnOperationRemoved write FOnOperationRemoved;
    property OnOperationStarted : TOperationManagerEvent read FOnOperationStarted write FOnOperationStarted;
    property OnOperationFinished: TOperationManagerEvent read FOnOperationFinished write FOnOperationFinished;
  end;

var
  OperationsManager: TOperationsManager = nil;

implementation

constructor TOperationsManager.Create;
begin
  FOperations := TFPList.Create;
  FLock := TCriticalSection.Create;
  FLastUsedHandle := 0;

  FOnOperationAdded    := nil;
  FOnOperationRemoved  := nil;
  FOnOperationStarted  := nil;
  FOnOperationFinished := nil;
  FOnOperationPaused   := nil;
  FOnOperationResumed  := nil;

  inherited Create;
end;

destructor TOperationsManager.Destroy;
begin
  inherited Destroy;

  FreeAndNil(FLock);
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

        Result := Entry^.Handle;

        // Set OnTerminate event so that we can cleanup when thread finishes.
        // Or instead of this create a timer for each thread and do:
        //  Thread.WaitFor  (or WaitForThreadTerminate(Thread.ThreadID))
        Thread.OnTerminate := @ThreadTerminatedEvent;

        if Assigned(FOnOperationAdded) then
          FOnOperationAdded(Operation);

        if StartingState = ossAutoStart then
        begin
          Thread.Resume;

          if Assigned(FOnOperationStarted) then
            FOnOperationStarted(Operation);
        end;
        // else it will be started by some user trigger
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
        Result := Entry^.Operation;
        break;
      end;
    end;
  end;
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
    if Assigned(FOnOperationFinished) then
      FOnOperationFinished(Entry^.Operation);

    FOperations.Delete(Index);

    if Assigned(FOnOperationRemoved) then
      FOnOperationRemoved(Entry^.Operation);

    Entry^.Thread := nil;  // Thread frees himself automatically on terminate.

    // Here the operation should not be used anymore
    // (by the thread and by any operations viewer).
    FreeAndNil(Entry^.Operation);

    Dispose(Entry);
  end;
end;

initialization

  OperationsManager := TOperationsManager.Create;

finalization

  FreeAndNil(OperationsManager);

end.

