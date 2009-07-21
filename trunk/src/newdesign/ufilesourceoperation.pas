unit uFileSourceOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, uLng,
  uFileSourceOperationTypes;

const

  guidIFileSourceOperationUI = '{3BC28C8B-8A3E-4F71-8828-52483B8C057A}';

type

  TFileSourceOperationState =
    (fsosNotStarted,  //<en before operation has started
     fsosStarting,    //<en responded to Start command
     fsosRunning,
     fsosPausing,     //<en responded to Pause command
     fsosPaused,
     fsosWaitingForFeedback, //<en waiting for a response from a user through the assigned UI
     fsosStopping,    //<en responded to Stop command
     fsosStopped);    //<en finished due to Stop command or on its own

  TFileSourceOperationStopReason =
    (fssrFinished,        //<en normal finish
     fssrAborted);        //<en aborted due to Stop command (by user)

const
  FileSourceOperationStateText: array[TFileSourceOperationState] of string =
    (rsOperNotStarted, rsOperStarting, rsOperRunning, rsOperPausing,
     rsOperPaused, rsOperWaitingForFeedback, rsOperStopping, rsOperStopped);

  FileSourceOperationStopReasonText: array[TFileSourceOperationStopReason] of string =
    (rsOperFinished, rsOperAborted);

type

  TFileSourceOperation = class;

  TFileSourceOperationEvent =
    (fsoevStateChanged);   //<en State has changed (paused, started, stopped, etc.)

  TFileSourceOperationEvents = set of TFileSourceOperationEvent;

  TFileSourceOperationEventNotify =
      procedure(Operation: TFileSourceOperation;
                Event: TFileSourceOperationEvent) of object;

  {en
     Base class for each file source operation.
  }
  TFileSourceOperation = class
  private
    {
       Progress and StopReason don't need synchronization, because they
       are written to from a single thread only (operation running thread) and
       just read from other threads.
       DesiredState works the other way around. It is written to only by the
       GUI thread and read from only by the operation running thread, so also
       no synchronization needed.
       State must by synchronized as its written to by both threads.
    }

    {en
       General progress of the operation (0 - 100 %).
       Specific statistics are returned by the individual operations.
    }
    FProgress: Integer;
    FDesiredState: TFileSourceOperationState;
    FStopReason: TFileSourceOperationStopReason;

    FState: TFileSourceOperationState;
    FStateLock: TCriticalSection;

    {en
       This event is used to wait for start and wait for unpausing.
    }
    FPauseEvent: PRTLEvent;

    {en
       An array of lists of listeners for each event type.
       Does not have to be synchronized as it is read and written to from GUI thread only.
    }
    FEventsListeners: array[TFileSourceOperationEvent] of TFPList;

    {en
       The thread that runs this operation.
       It is used for synchronizing events (notifying of them from the main thread).
    }
    FThread: TThread;

    {en
       Set of events that occurred and listeners have to be notified about.
       Access must be synchronized with FEventsLock.
    }
    FEventsToNotify: TFileSourceOperationEvents;
    FEventsLock: TCriticalSection; // used to synchronize access to FEventsToNotify
    {en
       How many calls to CallEventsListeners are scheduled.
       It is only modified from the main thread.
    }
    FScheduledEventsListenersCalls: Integer;

    {en
       Before finishing, the operation waits for this event, so that it doesn't
       finish until all scheduled calls to CallEventsListeners are made.
    }
    FNoEventsListenersCallsScheduledEvent: PRTLEvent;

    {en
       This function must be run from the GUI thread.
       It posts a function to the application message queue that will call
       all the needed event functions.
    }
    procedure QueueEventsListeners;
    procedure CallEventsListeners(Data: PtrInt);

  protected

    procedure UpdateProgress(NewProgress: Integer);
    procedure UpdateState(NewState: TFileSourceOperationState);
    function GetDesiredState: TFileSourceOperationState;

    {en
       Must be called from the controller thread (GUI).
    }
    procedure DoWaitForStart;
    {en
       Must be called from the operation thread.
    }
    procedure DoPause;
    {en
       Must be called from the controller thread (GUI).
    }
    procedure DoUnPause;

    {en
       This should be set to the correct file operation type in each concrete descendant.
       We rely on this when making a decision based on operation type. This way
       it's easier to maintain different sorts of things we can do with operations
       and statistics, without having to include knowledge of those things
       in the operations classes hierarchy.
    }
    function GetID: TFileSourceOperationType; virtual abstract;

    procedure Initialize; virtual abstract;
    function  ExecuteStep: Boolean; virtual abstract;
    procedure Finalize; virtual abstract;

    {en
       Notifies all listeners that an event has occurred (or multiple events).
       This function will be called from the operation thread.
    }
    procedure NotifyEvents(Events: TFileSourceOperationEvents);

  public
    constructor Create; virtual;
    destructor Destroy; override;

    {en
       Executes operation.
    }
    procedure fExecute; virtual abstract;

    procedure Execute; virtual;

    {
      The Start/Pause/Stop functions only have sense if the operation
      is run in a separate thread (most probably via OperationsManager).
    }

    {en
       Tries to start/resume operation.
       First immediately sets state to fsosStarting and after it is started to fsosRunning.
    }
    procedure Start;
    {en
       Tries to put operation into a paused state.
       First immediately sets state to fsosPausing and after it is paused to fsosPaused.
    }
    procedure Pause;
    {en
       Tries to stop operation.
       First immediately sets state to fsosStopping and after it is stopped to fsosStopped.
    }
    procedure Stop;
    {en
       Prevents auto start of the operation on Execute.
       Makes sense only before first call to Execute.
    }
    procedure PreventStart;

    {en
       Sets the thread assigned to this operation.
    }
    procedure AssignThread(Thread: TThread);

    {en
       Adds a function to call on specific events.
    }
    procedure AddEventsListener(Events: TFileSourceOperationEvents;
                                FunctionToCall: TFileSourceOperationEventNotify);

    {en
       Removes a registered function callback for events.
    }
    procedure RemoveEventsListener(Events: TFileSourceOperationEvents;
                                   FunctionToCall: TFileSourceOperationEventNotify);

    property Progress: Integer read FProgress;
    property ID: TFileSourceOperationType read GetID;
    property State: TFileSourceOperationState read FState;
  end;

  {en
     Interface used by each operation to communicate
     with the main application and the user.
     The operations are able to use different interfaces for communicating
     (message boxes, logging in edit controls or file with no questions,
      through console, ...).
     When operation has no communication interface it runs silent by
     any options it has set (for example: overwrite all or skip all).
  }
  IFileSourceOperationUI = interface [guidIFileSourceOperationUI]

    // While copying file exists. Ask what to do:
    //function FileExists: what to do;

    //         FileReadOnly
    //         AskOverwrite
    //         etc.

  end;

implementation

uses
  Forms;

type
  PEventsListEntry = ^TEventsListEntry;
  TEventsListEntry = record
    EventFunction: TFileSourceOperationEventNotify;
  end;

constructor TFileSourceOperation.Create;
var
  Event: TFileSourceOperationEvent;
begin
  FState := fsosNotStarted;
  FDesiredState := fsosRunning;  // set for auto-start unless prevented by PreventStart
  FStopReason := fssrFinished;
  FProgress := 0;
  FPauseEvent := RTLEventCreate;
  FStateLock := TCriticalSection.Create;
  FEventsLock := TCriticalSection.Create;
  FThread := nil;
  FEventsToNotify := [];
  FScheduledEventsListenersCalls := 0;
  FNoEventsListenersCallsScheduledEvent := RTLEventCreate;
  // Set at start because we don't have any calls scheduled at this time.
  RTLeventSetEvent(FNoEventsListenersCallsScheduledEvent);

  for Event := Low(FEventsListeners) to High(FEventsListeners) do
    FEventsListeners[Event] := TFPList.Create;

  inherited Create;
end;

destructor TFileSourceOperation.Destroy;
var
  Event: TFileSourceOperationEvent;
  i: Integer;
begin
  inherited Destroy;

  for Event := Low(FEventsListeners) to High(FEventsListeners) do
  begin
    for i := 0 to FEventsListeners[Event].Count - 1 do
      Dispose(PEventsListEntry(FEventsListeners[Event].Items[i]));

    FreeAndNil(FEventsListeners[Event]);
  end;

  RTLeventdestroy(FPauseEvent);
  RTLeventdestroy(FNoEventsListenersCallsScheduledEvent);
  FreeAndNil(FStateLock);
  FreeAndNil(FEventsLock);
end;

procedure TFileSourceOperation.Execute;
begin
  UpdateState(fsosNotStarted);
  UpdateProgress(0);
  FStopReason := fssrFinished;

  if GetDesiredState <> fsosRunning then
  begin
    DoWaitForStart;
  end;

  UpdateState(fsosStarting);

  Initialize;

  UpdateState(fsosRunning);

  // Main loop.
  while True do
  begin
    case GetDesiredState of
      fsosPaused:
        begin
          UpdateState(fsosPaused);
          DoPause;
          UpdateState(fsosRunning);
        end;

      fsosStopped:
        begin
          Finalize;
          FStopReason := fssrAborted;
          UpdateState(fsosStopped);
          Exit;
        end;
    end;

    if ExecuteStep = False then
      Break;  // Operation has finished.
  end;

  UpdateState(fsosStopped);
  UpdateProgress(100);

  // Wait until all the scheduled calls to events listeners have been processed
  // by the main thread (otherwise the calls can be made to a freed memory location).
  RTLeventWaitFor(FNoEventsListenersCallsScheduledEvent);
end;

procedure TFileSourceOperation.UpdateProgress(NewProgress: Integer);
begin
  FProgress := NewProgress;
end;

procedure TFileSourceOperation.UpdateState(NewState: TFileSourceOperationState);
begin
  FStateLock.Acquire;
  try
    FState := NewState;
  finally
    FStateLock.Release;
  end;

  NotifyEvents([fsoevStateChanged]);
end;

function TFileSourceOperation.GetDesiredState: TFileSourceOperationState;
begin
  Result := FDesiredState;
end;

procedure TFileSourceOperation.DoWaitForStart;
begin
  RTLeventResetEvent(FPauseEvent);
  RTLeventWaitFor(FPauseEvent); // wait indefinitely
end;

procedure TFileSourceOperation.DoPause;
begin
  UpdateState(fsosPaused);
  RTLeventResetEvent(FPauseEvent);
  RTLeventWaitFor(FPauseEvent); // wait indefinitely
  UpdateState(fsosRunning);
end;

procedure TFileSourceOperation.DoUnPause;
begin
  RTLeventSetEvent(FPauseEvent);
end;

procedure TFileSourceOperation.Start;
begin
  if State in [fsosNotStarted, fsosPaused] then
  begin
    UpdateState(fsosStarting);
    DoUnPause;

    FDesiredState := fsosRunning;
  end;
end;

procedure TFileSourceOperation.Pause;
begin
  if State = fsosRunning then
  begin
    UpdateState(fsosPausing);
    FDesiredState := fsosPaused;
  end;
end;

procedure TFileSourceOperation.Stop;
begin
  if State in [fsosNotStarted, fsosPaused, fsosRunning] then
  begin
    UpdateState(fsosStopping);
    DoUnPause;

    FDesiredState := fsosStopped;
  end;
end;

procedure TFileSourceOperation.PreventStart;
begin
  FDesiredState := fsosNotStarted;
end;

procedure TFileSourceOperation.AssignThread(Thread: TThread);
begin
  FThread := Thread;
end;

procedure TFileSourceOperation.AddEventsListener(Events: TFileSourceOperationEvents;
                                                 FunctionToCall: TFileSourceOperationEventNotify);
var
  Entry: PEventsListEntry;
  Event: TFileSourceOperationEvent;
begin
  for Event := Low(TFileSourceOperationEvent) to High(TFileSourceOperationEvent) do
  begin
    if Event in Events then
    begin
      Entry := New(PEventsListEntry);
      Entry^.EventFunction := FunctionToCall;
      FEventsListeners[Event].Add(Entry);
    end;
  end;
end;

procedure TFileSourceOperation.RemoveEventsListener(Events: TFileSourceOperationEvents;
                                                    FunctionToCall: TFileSourceOperationEventNotify);
var
  Entry: PEventsListEntry;
  Event: TFileSourceOperationEvent;
  i: Integer;
begin
  for Event := Low(TFileSourceOperationEvent) to High(TFileSourceOperationEvent) do
  begin
    if Event in Events then
    begin
      for i := 0 to FEventsListeners[Event].Count - 1 do
      begin
        Entry := PEventsListEntry(FEventsListeners[Event].Items[i]);
        if Entry^.EventFunction = FunctionToCall then
        begin
          FEventsListeners[Event].Delete(i);
          break;  // break from one for only
        end;
      end;
    end;
  end;
end;

procedure TFileSourceOperation.NotifyEvents(Events: TFileSourceOperationEvents);
begin
  // Add events to set of events to notify about.
  FEventsLock.Acquire; // must be done under the same lock as in CallEventsListeners
  try
    FEventsToNotify := FEventsToNotify + Events;

    // If there is already a scheduled (queued) call to CallEventsListeners,
    // it will also notify about the new events set above, so we don't have to
    // schedule another one. This must be done under lock, because it must be
    // synchronized with adding new events to FEventsToNotify.
    if FScheduledEventsListenersCalls > 0 then
      Exit;

  finally
    FEventsLock.Release;
  end;

  // NotifyEvents() is run from the operation thread so we cannot call event
  // listeners directly, because they may update the GUI. Instead they will be
  // queued to be called by the main thread (through application message queue).
  // We don't want to call events listeners through Synchronize because the
  // operation thread would be suspended while the listeners are executing and
  // it would slow down executing of the operation.
  if Assigned(FThread) then
    TThread.Synchronize(FThread, @Self.QueueEventsListeners); // queueing must be from the GUI thread
  // Notifying of events not through a thread not supported for now.
end;

procedure TFileSourceOperation.QueueEventsListeners;
begin
  // While this function is executing the operation thread is suspended,
  // so only queue function and quickly exit.
  Application.QueueAsyncCall(@Self.CallEventsListeners, 0);

  Inc(FScheduledEventsListenersCalls, 1); // don't have to do under lock, operation thread is suspended here
  RTLeventResetEvent(FNoEventsListenersCallsScheduledEvent);
end;

procedure TFileSourceOperation.CallEventsListeners(Data: PtrInt);
var
  Event: TFileSourceOperationEvent;
  Entry: PEventsListEntry;
  i: Integer;
  LocalEventsToNotify: TFileSourceOperationEvents;
begin
  // This function is run from the main thread, so we are sure that
  // while it is executing no registered listener is destroyed.

  FEventsLock.Acquire; // must be done under the same lock as in NotifyEvents
  try
    LocalEventsToNotify := FEventsToNotify; // read events to notify
    FEventsToNotify := [];                  // clear events to notify
    Dec(FScheduledEventsListenersCalls, 1); // must be under lock too as it needs to be
                                            // synchronized with clearing FEventsToNotify
  finally
    FEventsLock.Release;
  end;

  if LocalEventsToNotify <> [] then
  begin
    for Event := Low(TFileSourceOperationEvent) to High(TFileSourceOperationEvent) do
    begin
      if Event in LocalEventsToNotify then // Check if this event occurred.
      begin
        // Call each listener function.
        for i := 0 to FEventsListeners[Event].Count - 1 do
        begin
          Entry := PEventsListEntry(FEventsListeners[Event].Items[i]);
          Entry^.EventFunction(Self, Event);
        end;
      end;
    end;
  end;

  if FScheduledEventsListenersCalls = 0 then
    // After setting this event the operation object (Self) may already
    // be destroyed, so it must be the last thing to do.
    RTLeventSetEvent(FNoEventsListenersCallsScheduledEvent);
end;

end.

