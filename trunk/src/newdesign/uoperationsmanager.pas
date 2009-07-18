unit uOperationsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LCLIntf, syncobjs,
  uOperationThread, uFileSourceOperation;

type

  {en Handle to OperationsManager's operation.}
  TOperationHandle = Longint;

const
  InvalidOperationHandle = TOperationHandle(0);

type

  POperationsManagerEntry = ^TOperationsManagerEntry;
  TOperationsManagerEntry = record
    Thread   : TOperationThread;
    Operation: TFileSourceOperation;
    Handle   : TOperationHandle;
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
    FNextUnusedHandle: TOperationHandle;

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
    function GetOperationByIndex(Index: Integer): TFileSourceOperation;
    function GetOperationByHandle(Handle: TOperationHandle): TFileSourceOperation;

    function GetNextUnusedHandle: TOperationHandle;

  public
    constructor Create;
    destructor Destroy; override;

    function AddOperation(Operation: TFileSourceOperation;
                          StartImmediately: Boolean): TOperationHandle;

    property OperationByIndex[Index: Integer]: TFileSourceOperation read GetOperationByIndex;
    property OperationByHandle[Handle: TOperationHandle]: TFileSourceOperation read GetOperationByHandle;
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
  FNextUnusedHandle := 1;   // Start from 1.

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
                                         StartImmediately: Boolean): TOperationHandle;
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

        FOperations.Add(Entry);

        Result := Entry^.Handle;

        // Set OnTerminate event so that we can cleanup when thread finishes.
        // Or instead of this create a timer for each thread and do:
        //  Thread.WaitFor  (or WaitForThreadTerminate(Thread.ThreadID))
        Thread.OnTerminate := @ThreadTerminatedEvent;

        if Assigned(FOnOperationAdded) then
          FOnOperationAdded(Operation);

        if StartImmediately then
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
    if Assigned(Entry) and Assigned(Entry^.Operation) then
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

function TOperationsManager.GetNextUnusedHandle: TOperationHandle;
begin
  // Handles are consecutively incremented.
  // Even if they overflow there is little probability that
  // there will be that many operations.
  Result := InterLockedIncrement(FNextUnusedHandle);
  if Result = InvalidOperationHandle then
    Result := InterLockedIncrement(FNextUnusedHandle);
end;

procedure TOperationsManager.ThreadTerminatedEvent(Sender: TObject);
var
  Thread: TOperationThread;
  Entry: POperationsManagerEntry = nil;
  i: Integer;
begin
  // This function is executed from the main thread (through Synchronize).

  Thread := Sender as TOperationThread;

  // Search the terminated in the operations list.
  for i := 0 to FOperations.Count - 1 do
  begin
    Entry := POperationsManagerEntry(FOperations.Items[i]);
    if Entry^.Thread = Thread then
    begin
      FOperations.Delete(i);
      break;
    end;
  end;

  if Assigned(Entry) then
  begin
    if Assigned(FOnOperationFinished) then
      FOnOperationFinished(Entry^.Operation);

    FreeAndNil(Entry^.Thread);

    if Assigned(FOnOperationRemoved) then
      FOnOperationRemoved(Entry^.Operation);

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

