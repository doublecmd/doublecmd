unit uFileSourceOperation;

{$mode objfpc}{$H+}

// If defined causes to synchronize executing callback for events.
// This ensures that the callbacks always have the operation in the current state,
// which might be safer, but it is slower because the operation must wait
// until all callbacks are executed.
// If undefined then all events are sent asynchronously, which is faster.
// However, it may result in those events to be reported much later after
// they have happened in the operation and the operation state might
// not be valid anymore.
//{$DEFINE fsoSynchronizeEvents}

// If defined it will only send one event and will not send more
// until that event is processed (so some events may be lost).
// This normally shouldn't be defined.
//{$DEFINE fsoSendOnlyCurrentState}

//{$DEFINE debugFileSourceOperation}

interface

uses
  Classes, SysUtils, syncobjs, uLng,
  uFileSourceOperationOptionsUI,
  uFileSourceOperationTypes,
  uFileSourceOperationUI;

type

  TFileSourceOperationState =
    (fsosNotStarted,  //<en before operation has started
     fsosStarting,    //<en responded to Start command
     fsosRunning,
     fsosPausing,     //<en responded to Pause command
     fsosPaused,
     fsosWaitingForFeedback, //<en waiting for a response from a user through the assigned UI
     fsosWaitingForConnection, //<en waiting for an available connection to TFileSource
     fsosStopping,    //<en responded to Stop command
     fsosStopped);    //<en finished due to Stop command or on its own

  TFileSourceOperationStates = set of TFileSourceOperationState;

const
  fsosAllStates  = [Low(TFileSourceOperationState) .. High(TFileSourceOperationState)];

type
  TFileSourceOperationResult =
    (fsorFinished,    //<en operation has finished successfully
     fsorAborted);    //<en operation has been aborted by user

const
  FileSourceOperationStateText: array[TFileSourceOperationState] of string =
    (rsOperNotStarted, rsOperStarting, rsOperRunning, rsOperPausing,
     rsOperPaused, rsOperWaitingForFeedback, rsOperWaitingForConnection,
     rsOperStopping, rsOperStopped);

  FileSourceOperationResultText: array[TFileSourceOperationResult] of string =
    (rsOperFinished, rsOperAborted);

type

  TFileSourceOperation = class;

  TFileSourceOperationStateChangedNotify =
      procedure(Operation: TFileSourceOperation;
                State: TFileSourceOperationState) of object;

  TAskQuestionFunction =
      function(Msg: String; Question: String;
               PossibleResponses: array of TFileSourceOperationUIResponse;
               DefaultOKResponse: TFileSourceOperationUIResponse;
               DefaultCancelResponse: TFileSourceOperationUIResponse
             ) : TFileSourceOperationUIResponse of object;
  TAbortOperationFunction = procedure of object;
  TCheckOperationStateFunction = procedure of object;

  TFileSourceOperationClass = class of TFileSourceOperation;
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
    FProgress: Double;
    FDesiredState: TFileSourceOperationState;
    FOperationResult: TFileSourceOperationResult;

    FState: TFileSourceOperationState;
    FStateLock: TCriticalSection;

    {en
       This event is used to wait for start and wait for unpausing.
    }
    FPauseEvent: PRTLEvent;

    {en
       This event is used to wait for an available connection to TFileSource.
    }
    FConnectionAvailableEvent: PRTLEvent;

    {en
       A list of listeners of state-changed event.
       Must be synchronized using FEventsLock.
    }
    FStateChangedEventListeners: TFPList;
    {en
       Used to synchronize access to:
       - FStateChangedEventListeners
       - FScheduledEventsListenersCalls
       - FNoEventsListenersCallsScheduledEvent
    }
    FEventsLock: TCriticalSection;

    {en
       The thread that runs this operation.
       It is used for synchronizing events (notifying of them from the main thread).
    }
    FThread: TThread;

{$IFNDEF fsoSynchronizeEvents}
    {en
       How many events are scheduled to execute.
    }
    FScheduledEventsListenersCalls: Integer;

    {en
       Before finishing, the operation waits for this event, so that it doesn't
       finish until all scheduled calls to CallEventsListeners are made.
    }
    FNoEventsListenersCallsScheduledEvent: PRTLEvent;
{$ENDIF}

    {en
       List of assigned user interfaces that operation can use to ask user questions.
       Don't access this list from the operation thread.
    }
    FUserInterfaces: TFPList; // of TFileSourceOperationUI

    {en
       Event used to notify operation thread that an UI was assigned,
       so it can wake up and ask questions.
    }
    FUserInterfaceAssignedEvent: PRTLEvent;

    // Parameters for UI question.
    // Used to pass from operation thread to GUI thread.
    FUIMessage: String;
    FUIQuestion: String;
    FUIPossibleResponses: array of TFileSourceOperationUIResponse;
    FUIDefaultOKResponse: TFileSourceOperationUIResponse;
    FUIDefaultCancelResponse: TFileSourceOperationUIResponse;
    FUIResponse: TFileSourceOperationUIResponse;
    FTryAskQuestionResult: Boolean;

    {en
       Used to determine whether the operation has started or not.
    }
    FOperationInitialized : Boolean;

    {en
       Last start time (when operation started or resumed after pause).
    }
    FStartTime: TDateTime;

    {en
       File source on which this operation is executed.
    }
    FFileSource: IInterface;

    {en
       File source connection.
    }
    FConnection: TObject;

    // This function is called from main thread.
{$IFDEF fsoSynchronizeEvents}
    procedure CallEventsListeners;
{$ELSE}
    procedure CallEventsListeners(Data: Pointer);
{$ENDIF}

    // This should be run from GUI thread only.
    procedure TryAskQuestion;

    procedure UpdateState(NewState: TFileSourceOperationState);
    function GetState: TFileSourceOperationState;
    procedure UpdateStartTime(NewStartTime: TDateTime);

    {en
       Must be called from the operation thread.
    }
    procedure DoPause;
    {en
       Must be called from the controller thread (GUI).
    }
    procedure DoUnPause;

    procedure DoWaitForConnection;

    {en
       Pauses the operation until it is notified that a connection is available.
    }
    procedure WaitForConnection;

    {en
       Reloads any file sources changed by the operation.
    }
    procedure ReloadFileSources;

  protected
    {en
       If @true a connection is requested from file source before the operation
       starts. By default this is @true if file source has fspUsesConnections
       property, but this variable may be changed on a per-operation basis
       in the operation's constructor.
    }
    FNeedsConnection: Boolean;

    procedure UpdateProgress(NewProgress: Double);
    function GetDesiredState: TFileSourceOperationState;

    {en
       Reloads changed file sources.
       It is called from main thread.
    }
    procedure DoReloadFileSources; virtual;

    {en
       Retrieves an available connection from the file source (TFileSourceConnection).
    }
    function GetConnection: TObject; virtual;

    {en
       This should be set to the correct file operation type in each concrete descendant.
       We rely on this when making a decision based on operation type. This way
       it's easier to maintain different sorts of things we can do with operations
       and statistics, without having to include knowledge of those things
       in the operations classes hierarchy.
    }
    function GetID: TFileSourceOperationType; virtual abstract;

    procedure Initialize; virtual;
    procedure MainExecute; virtual abstract;
    procedure Finalize; virtual;

    {en
       Notifies all listeners that operation has changed its state.
       This function can be called from the operation thread or from the main thread.
       Don't call it under the FEventsLock lock.
    }
    procedure NotifyStateChanged(NewState: TFileSourceOperationState);

    {en
       General function to ask questions from operations.
       It is run from the operation thread and is thread-safe.
       The function stops executing the operation until the question can be
       asked and the response from the user is received. While the operation
       is waiting for a response it may be aborted by the user in which case
       the function will throw EFileSourceOperationAborting exception.
       The most recently (last) assigned user interface is used to ask the question.
    }
    function AskQuestion(
               Msg: String; Question: String;
               PossibleResponses: array of TFileSourceOperationUIResponse;
               DefaultOKResponse: TFileSourceOperationUIResponse;
               DefaultCancelResponse: TFileSourceOperationUIResponse
             ) : TFileSourceOperationUIResponse;

    {en
       Remember statistics at start time (used for estimating remaining time).
    }
    procedure UpdateStatisticsAtStartTime; virtual abstract;

    {en
       This function does some checks on the current and desired state of the
       operation:
       If the desired state is fsosPaused it pauses the thread.
       If the desired state is fsosStopped it throws EFileSourceOperationAborting
       exception.

       The function should be run from the operation thread
       from the most repeated points.
    }
    procedure CheckOperationState;

    class procedure RaiseAbortOperation;

    property Thread: TThread read FThread;

  public
    constructor Create(const aFileSource: IInterface); virtual;
    destructor Destroy; override;

    {en
       Executes operation.
    }
    procedure Execute;

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
       Notifies the operation that possibly a connection is available from
       the file source, but it does not guarantee it. The operation should
       ask the file source for the connection.
       Usually will be called from the file source.
    }
    procedure ConnectionAvailableNotify;

    {en
       Sets the thread assigned to this operation.
    }
    procedure AssignThread(AThread: TThread);

    {en
       Adds a function to call when the operation's state changes.
       @param(States
              The function will be called if the operation changes its state
              to one of these states.)
    }
    procedure AddStateChangedListener(
                States: TFileSourceOperationStates;
                FunctionToCall: TFileSourceOperationStateChangedNotify);

    {en
       Removes a registered function callback for state-changed event.
    }
    procedure RemoveStateChangedListener(
                States: TFileSourceOperationStates;
                FunctionToCall: TFileSourceOperationStateChangedNotify);

    // These functions are run from the GUI thread.
    procedure AddUserInterface(UserInterface: TFileSourceOperationUI);
    procedure RemoveUserInterface(UserInterface: TFileSourceOperationUI);
    {en
       Returns graphical interface class for user to set operation options.
    }
    class function GetOptionsUIClass: TFileSourceOperationOptionsUIClass; virtual;
    class function GetOperationClass: TFileSourceOperationClass;

    property Progress: Double read FProgress;
    property ID: TFileSourceOperationType read GetID;
    property State: TFileSourceOperationState read GetState;
    property StartTime: TDateTime read FStartTime;
    property Result: TFileSourceOperationResult read FOperationResult;
    property FileSource: IInterface read FFileSource;
  end;

  EFileSourceOperationAborting = class(Exception)
  public
    constructor Create; reintroduce;
  end;

implementation

uses
  Forms, uFileSource, uFileSourceProperty, uDebug, uExceptions
  {$IFNDEF fsoSynchronizeEvents}
  , uGuiMessageQueue
  {$ENDIF}
  ;

type
  PStateChangedEventEntry = ^TStateChangedEventEntry;
  TStateChangedEventEntry = record
    FunctionToCall: TFileSourceOperationStateChangedNotify;
    States        : TFileSourceOperationStates;
  end;

  PUserInterfacesEntry = ^TUserInterfacesEntry;
  TUserInterfacesEntry = record
    UserInterface: TFileSourceOperationUI;
  end;

constructor TFileSourceOperation.Create(const aFileSource: IInterface);
begin
  FState := fsosNotStarted;
  FDesiredState := fsosRunning;  // set for auto-start unless prevented by PreventStart
  FOperationResult := fsorFinished;
  FProgress := 0;
  FPauseEvent := RTLEventCreate;
  FConnectionAvailableEvent := RTLEventCreate;
  FStateLock := TCriticalSection.Create;
  FEventsLock := TCriticalSection.Create;
  FThread := nil;
  FConnection := nil;
  FOperationInitialized := False;

{$IFNDEF fsoSynchronizeEvents}
  FScheduledEventsListenersCalls := 0;

  FNoEventsListenersCallsScheduledEvent := RTLEventCreate;
  // Set at start because we don't have any calls scheduled at this time.
  RTLeventSetEvent(FNoEventsListenersCallsScheduledEvent);
{$ENDIF}

  FUserInterfaces := TFPList.Create;
  FUserInterfaceAssignedEvent := RTLEventCreate;
  // Reset at start because we have no interface assigned.
  RTLeventResetEvent(FUserInterfaceAssignedEvent);

  FStateChangedEventListeners := TFPList.Create;

  FStartTime := 0;

  FFileSource := aFileSource;

  FNeedsConnection := (fspUsesConnections in (FileSource as IFileSource).Properties);

  inherited Create;
end;

destructor TFileSourceOperation.Destroy;
var
  i: Integer;
begin
  inherited Destroy;

  // Remove operation from the queue of operations waiting for a connection
  // (it can still be there if it was aborted while waiting).
  (FileSource as IFileSource).RemoveOperationFromQueue(Self);

  for i := 0 to FStateChangedEventListeners.Count - 1 do
    Dispose(PStateChangedEventEntry(FStateChangedEventListeners.Items[i]));
  FreeAndNil(FStateChangedEventListeners);

  for i := 0 to FUserInterfaces.Count - 1 do
    Dispose(PUserInterfacesEntry(FUserInterfaces.Items[i]));
  FreeAndNil(FUserInterfaces);

  // Just to be sure - set all events when we're destroying the object
  // in case the thread is still waiting (this should normally not happen).
  RTLeventSetEvent(FPauseEvent);
  RTLeventSetEvent(FConnectionAvailableEvent);
{$IFNDEF fsoSynchronizeEvents}
  RTLeventSetEvent(FNoEventsListenersCallsScheduledEvent);
{$ENDIF}
  RTLeventSetEvent(FUserInterfaceAssignedEvent);

  RTLeventdestroy(FPauseEvent);
  RTLeventdestroy(FConnectionAvailableEvent);
{$IFNDEF fsoSynchronizeEvents}
  RTLeventdestroy(FNoEventsListenersCallsScheduledEvent);
{$ENDIF}
  RTLeventdestroy(FUserInterfaceAssignedEvent);

  FreeAndNil(FStateLock);
  FreeAndNil(FEventsLock);
end;

procedure TFileSourceOperation.Initialize;
begin
  // Override in descendant classes.
end;

procedure TFileSourceOperation.Finalize;
begin
  // Override in descendant classes.
end;

procedure TFileSourceOperation.Execute;
begin
  try
{$IFDEF debugFileSourceOperation}
   DCDebug('Op: ', hexStr(Self), ' ', FormatDateTime('nnss.zzzz', Now), ': Start operation ', ClassName);
{$ENDIF}

    UpdateProgress(0);
    FOperationResult := fsorAborted;

    try
      // Wait for manual start if not started automatically.
      if GetDesiredState <> fsosRunning then
        DoPause;  // wait for start command

      // Check if wasn't aborted while paused.
      CheckOperationState;

      if FNeedsConnection then
      begin
        // Wait for connection to file source.
        while True do
        begin
          FConnection := GetConnection;

          if Assigned(FConnection) then
            break;

          if State <> fsosWaitingForConnection then
            UpdateState(fsosWaitingForConnection);

          DoWaitForConnection;

          // Allow pausing and aborting the operation.
          CheckOperationState;
        end;
      end;

      // Initialize.

      UpdateState(fsosStarting);

      Initialize;
      FOperationInitialized := True;

      UpdateStartTime(SysUtils.Now);
      UpdateState(fsosRunning);

{$IFDEF debugFileSourceOperation}
      DCDebug('Op: ', hexStr(Self), ' ', FormatDateTime('nnss.zzzz', Now), ': Before main execute');
{$ENDIF}

      MainExecute;

{$IFDEF debugFileSourceOperation}
      DCDebug('Op: ', hexStr(Self), ' ', FormatDateTime('nnss.zzzz', Now), ': After main execute');
{$ENDIF}

      FOperationResult := fsorFinished;
    except
      on EFileSourceOperationAborting do
        begin
          FOperationResult := fsorAborted;
        end;
    end;

    if FOperationInitialized then
      Finalize;

    UpdateProgress(1);

  finally
    UpdateState(fsosStopped);

{$IFDEF debugFileSourceOperation}
    DCDebug('Op: ', hexStr(self), ' ', FormatDateTime('nnss.zzzz', Now), ': Operation finished ', ClassName);
{$ENDIF}

{$IFNDEF fsoSynchronizeEvents}
    // Wait until all the scheduled calls to events listeners have been processed
    // by the main thread (otherwise the calls can be made to a freed memory location).
    RTLeventWaitFor(FNoEventsListenersCallsScheduledEvent);

{$IFDEF debugFileSourceOperation}
    DCDebug('Op: ', hexStr(self), ' ', FormatDateTime('nnss.zzzz', Now), ': After wait for events');
{$ENDIF}
{$ENDIF}
  end;

  // It is best to reload after the operation and all events are finished.
  if FOperationInitialized then
    ReloadFileSources;
end;

procedure TFileSourceOperation.UpdateProgress(NewProgress: Double);
begin
  FProgress := NewProgress;
end;

procedure TFileSourceOperation.UpdateState(NewState: TFileSourceOperationState);
begin
  FStateLock.Acquire;
  try
    if FState = NewState then
      Exit;
    FState := NewState;
  finally
    FStateLock.Release;
  end;

{$IFDEF debugFileSourceOperation}
  DCDebug('Op: ', hexStr(self), ' ', FormatDateTime('nnss.zzzz', Now), ': Updated state to ', IntToStr(Integer(NewState)));
{$ENDIF}
  NotifyStateChanged(NewState);
end;

function TFileSourceOperation.GetDesiredState: TFileSourceOperationState;
begin
  Result := FDesiredState;
end;

function TFileSourceOperation.GetState: TFileSourceOperationState;
begin
  FStateLock.Acquire;
  try
    Result := FState;
  finally
    FStateLock.Release;
  end;
end;

procedure TFileSourceOperation.DoPause;
begin
  RTLeventResetEvent(FPauseEvent);
  RTLeventWaitFor(FPauseEvent); // wait indefinitely
end;

procedure TFileSourceOperation.DoUnPause;
begin
  RTLeventSetEvent(FPauseEvent);
end;

procedure TFileSourceOperation.DoWaitForConnection;
begin
  RTLeventResetEvent(FConnectionAvailableEvent);
  RTLeventWaitFor(FConnectionAvailableEvent); // wait indefinitely
end;

procedure TFileSourceOperation.WaitForConnection;
begin
  UpdateState(fsosWaitingForConnection);

  DoWaitForConnection;

  UpdateStartTime(SysUtils.Now);
  UpdateState(fsosRunning);
end;

procedure TFileSourceOperation.ConnectionAvailableNotify;
begin
  RTLeventSetEvent(FConnectionAvailableEvent);
end;

function TFileSourceOperation.GetConnection: TObject;
begin
  Result := (FileSource as IFileSource).GetConnection(Self);
end;

procedure TFileSourceOperation.CheckOperationState;
begin
  case GetDesiredState of
    fsosPaused:
      begin
        UpdateState(fsosPaused);
        DoPause;

        // Check if the operation was unpaused because it is being aborted.
        if GetDesiredState = fsosStopped then
          RaiseAbortOperation;

        UpdateStartTime(SysUtils.Now);
        if FOperationInitialized then
          UpdateState(fsosRunning)
        else
          UpdateState(fsosStarting);
      end;

    fsosStopped:  // operation was asked to stop (via Stop function)
      begin
        RaiseAbortOperation;
      end;

    // else: we're left with fsosRunning
  end;
end;

procedure TFileSourceOperation.UpdateStartTime(NewStartTime: TDateTime);
begin
  FStartTime := NewStartTime;
  UpdateStatisticsAtStartTime;
end;

procedure TFileSourceOperation.Start;
begin
  FStateLock.Acquire;
  try
    if FState in [fsosNotStarted, fsosPaused] then
      FState := fsosStarting
    else
      Exit;
  finally
    FStateLock.Release;
  end;

  NotifyStateChanged(fsosStarting);
  FDesiredState := fsosRunning;
  DoUnPause;
end;

procedure TFileSourceOperation.Pause;
begin
  FStateLock.Acquire;
  try
    if FState in [fsosStarting, fsosRunning, fsosWaitingForConnection] then
      FState := fsosPausing
    else
      Exit;
  finally
    FStateLock.Release;
  end;

  NotifyStateChanged(fsosPausing);
  FDesiredState := fsosPaused;

  // Also set "Connection available" event in case the operation is waiting
  // for a connection and the user wants to pause it
  // (this must be after setting desired state).
  ConnectionAvailableNotify;
end;

procedure TFileSourceOperation.Stop;
begin
  FStateLock.Acquire;
  try
    if not (FState in [fsosStopping, fsosStopped]) then
      FState := fsosStopping
    else
      Exit;
  finally
    FStateLock.Release;
  end;

  NotifyStateChanged(fsosStopping);
  FDesiredState := fsosStopped;

  DoUnPause;

  // Also set "Connection available" event in case the operation is waiting
  // for a connection and the user wants to abort it
  // (this must be after setting desired state).
  ConnectionAvailableNotify;

  // The operation may be waiting for the user's response.
  // Wake it up then, because it is being aborted
  // (this must be after setting state to Stopping).
  RTLeventSetEvent(FUserInterfaceAssignedEvent);
end;

procedure TFileSourceOperation.PreventStart;
begin
  FDesiredState := fsosNotStarted;
end;

procedure TFileSourceOperation.AssignThread(AThread: TThread);
begin
  FThread := AThread;
end;

procedure TFileSourceOperation.AddStateChangedListener(
            States: TFileSourceOperationStates;
            FunctionToCall: TFileSourceOperationStateChangedNotify);
var
  Entry: PStateChangedEventEntry;
  i: Integer;
begin
  FEventsLock.Acquire;
  try
    // Check if this function isn't already added.
    for i := 0 to FStateChangedEventListeners.Count - 1 do
    begin
      Entry := PStateChangedEventEntry(FStateChangedEventListeners.Items[i]);
      if Entry^.FunctionToCall = FunctionToCall then
      begin
        // Add states to listen for.
        Entry^.States := Entry^.States + States;
        Exit;
      end;
    end;

    // Add new listener.
    Entry := New(PStateChangedEventEntry);
    Entry^.FunctionToCall := FunctionToCall;
    Entry^.States := States;
    FStateChangedEventListeners.Add(Entry);
  finally
    FEventsLock.Release;
  end;
end;

procedure TFileSourceOperation.RemoveStateChangedListener(
             States: TFileSourceOperationStates;
             FunctionToCall: TFileSourceOperationStateChangedNotify);
var
  Entry: PStateChangedEventEntry;
  i: Integer;
begin
  FEventsLock.Acquire;
  try
    for i := 0 to FStateChangedEventListeners.Count - 1 do
    begin
      Entry := PStateChangedEventEntry(FStateChangedEventListeners.Items[i]);
      if Entry^.FunctionToCall = FunctionToCall then
      begin
        // Remove listening for states.
        Entry^.States := Entry^.States - States;

        // If all states removed - remove the callback function itself.
        if Entry^.States = [] then
        begin
          FStateChangedEventListeners.Delete(i);
          Dispose(Entry);
        end;

        break;
      end;
    end;
  finally
    FEventsLock.Release;
  end;
end;

procedure TFileSourceOperation.NotifyStateChanged(NewState: TFileSourceOperationState);
var
  i: Integer;
  found: Boolean = False;
begin
  FEventsLock.Acquire;
  try
{$IFNDEF fsoSynchronizeEvents}
{$IFDEF fsoSendOnlyCurrentState}
    // If we only want to notify about the current state, first check
    // if there already isn't scheduled (queued) a call to CallEventsListeners.
    if FScheduledEventsListenersCalls > 0 then
      Exit;
{$ENDIF}
{$ENDIF}

    // Check if there is at least one listener that wants the new state.
    for i := 0 to FStateChangedEventListeners.Count - 1 do
    begin
      if NewState in PStateChangedEventEntry(FStateChangedEventListeners.Items[i])^.States then
      begin
        found := True;
        break;
      end;
    end;

    if not found then
      Exit;

{$IFNDEF fsoSynchronizeEvents}
    // This must be under the same lock as in CallEventsListeners.
    InterLockedIncrement(FScheduledEventsListenersCalls);
    RTLeventResetEvent(FNoEventsListenersCallsScheduledEvent);
{$ENDIF}
  finally
    FEventsLock.Release;
  end;

{$IFDEF debugFileSourceOperation}
  DCDebug('Op: ', hexStr(self), ' ', FormatDateTime('nnss.zzzz', Now), ': Before notify events');
{$ENDIF}

  if GetCurrentThreadID <> MainThreadID then
    // NotifyStateChanged() is run from the operation thread so we cannot
    // call event listeners directly, because they may update the GUI.
{$IFDEF fsoSynchronizeEvents}
    // Call listeners through Synchronize.
    TThread.Synchronize(FThread, @CallEventsListeners)
{$ELSE}
    // Schedule listeners through asynchronous message queue.
    GuiMessageQueue.QueueMethod(@CallEventsListeners, Pointer(PtrUInt(NewState)))
{$ENDIF}
  else
  begin
    // The function was called from main thread - call directly.

    if GetCurrentThreadID <> MainThreadID then
    begin
      // The operation runs in a thread.
      // Handle exceptions for the GUI thread because it controls the operation
      // and in case of error the operation may be left in infinite waiting state.
      try
{$IFDEF fsoSynchronizeEvents}
        CallEventsListeners;
{$ELSE}
        CallEventsListeners(Pointer(PtrUInt(NewState)));
{$ENDIF}
      except
        on Exception do
          begin
            WriteExceptionToErrorFile;
            DCDebug(ExceptionToString);
            ShowExceptionDialog;
          end;
      end;
    end
    else
    begin
{$IFDEF fsoSynchronizeEvents}
        CallEventsListeners;
{$ELSE}
        CallEventsListeners(Pointer(PtrUInt(NewState)));
{$ENDIF}
    end;
  end;

{$IFDEF debugFileSourceOperation}
  DCDebug('Op: ', hexStr(self), ' ', FormatDateTime('nnss.zzzz', Now), ': After notify events');
{$ENDIF}
end;

{$IFDEF fsoSynchronizeEvents}
procedure TFileSourceOperation.CallEventsListeners;
{$ELSE}
procedure TFileSourceOperation.CallEventsListeners(Data: Pointer);
{$ENDIF}
var
  Entry: PStateChangedEventEntry;
  i: Integer;
  aState: TFileSourceOperationState;
  FunctionsToCall: array of TFileSourceOperationStateChangedNotify;
  FunctionsCount: Integer = 0;
begin
{$IFDEF debugFileSourceOperation}
  DCDebug('Op: ', hexStr(self), ' ', FormatDateTime('nnss.zzzz', Now), ': Before call events');
{$ENDIF}

{$IFDEF fsoSynchronizeEvents}
  aState := Self.State;
{$ELSE}
 {$IFDEF fsoSendOnlyCurrentState}
  aState := Self.State;
 {$ELSE}
  aState := TFileSourceOperationState(PtrUInt(Data));
 {$ENDIF}

  InterLockedDecrement(FScheduledEventsListenersCalls);
{$ENDIF}

  // First the listeners functions must be copied under lock before calling them,
  // because any function called may attempt to add/remove listeners from the list.
  FEventsLock.Acquire;
  try
    SetLength(FunctionsToCall, FStateChangedEventListeners.Count);

    for i := 0 to FStateChangedEventListeners.Count - 1 do
    begin
      Entry := PStateChangedEventEntry(FStateChangedEventListeners.Items[i]);
      // Check if the listener wants this state.
      if (aState in Entry^.States) then
      begin
        FunctionsToCall[FunctionsCount] := Entry^.FunctionToCall;
        Inc(FunctionsCount, 1);
      end;
    end;

  finally
    FEventsLock.Release;
  end;

  // Call each listener function (not under lock).
  for i := 0 to FunctionsCount - 1 do
    FunctionsToCall[i](Self, aState);

{$IFNDEF fsoSynchronizeEvents}
  FEventsLock.Acquire;
  try
    // This must be under the same lock as in NotifyStateChanged.
    if FScheduledEventsListenersCalls = 0 then
      RTLeventSetEvent(FNoEventsListenersCallsScheduledEvent);
  finally
    FEventsLock.Release;
  end;
{$ENDIF}

{$IFDEF debugFileSourceOperation}
  DCDebug('Op: ', hexStr(Self), ' ', FormatDateTime('nnss.zzzz', Now), ': After call events');
{$ENDIF}
end;

procedure TFileSourceOperation.AddUserInterface(UserInterface: TFileSourceOperationUI);
var
  Entry: PUserInterfacesEntry;
begin
  Entry := New(PUserInterfacesEntry);
  Entry^.UserInterface := UserInterface;
  FUserInterfaces.Add(Entry);

  // Notify a possibly waiting operation thread that an UI was assigned.
  RTLeventSetEvent(FUserInterfaceAssignedEvent);
end;

procedure TFileSourceOperation.RemoveUserInterface(UserInterface: TFileSourceOperationUI);
var
  Entry: PUserInterfacesEntry;
  i: Integer;
begin
  for i := 0 to FUserInterfaces.Count - 1 do
  begin
    Entry := PUserInterfacesEntry(FUserInterfaces.Items[i]);
    if Entry^.UserInterface = UserInterface then
    begin
      FUserInterfaces.Delete(i);
      Dispose(Entry);
      break;
    end;
  end;

  if FUserInterfaces.Count = 0 then
    // Last interface was removed - reset event so that operation
    // thread will wait for an UI if it wants to ask a question.
    RTLeventResetEvent(FUserInterfaceAssignedEvent);
end;

class function TFileSourceOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result := nil;
end;

class function TFileSourceOperation.GetOperationClass: TFileSourceOperationClass;
begin
  Result := Self;
end;

function TFileSourceOperation.AskQuestion(
             Msg: String; Question: String;
             PossibleResponses: array of TFileSourceOperationUIResponse;
             DefaultOKResponse: TFileSourceOperationUIResponse;
             DefaultCancelResponse: TFileSourceOperationUIResponse): TFileSourceOperationUIResponse;
var
  i: Integer;
  bStateChanged: Boolean = False;
  OldState: TFileSourceOperationState;
begin
  FStateLock.Acquire;
  try
    if FState in [fsosStopping, fsosStopped] then
      RaiseAbortOperation
    else
    begin
      OldState := FState;
      FState := fsosWaitingForFeedback;
    end;
  finally
    FStateLock.Release;
  end;

  NotifyStateChanged(fsosWaitingForFeedback);

  // Set up parameters through variables because
  // we cannot pass them via Synchronize call to TryAskQuestion.
  FUIMessage := Msg;
  FUIQuestion := Question;
  SetLength(FUIPossibleResponses, Length(PossibleResponses));
  for i := 0 to Length(PossibleResponses) - 1 do
    FUIPossibleResponses[i] := PossibleResponses[i];
  FUIDefaultOKResponse := DefaultOKResponse;
  FUIDefaultCancelResponse := DefaultCancelResponse;

  if GetCurrentThreadID <> MainThreadID then
  begin
    while True do
    begin
      TThread.Synchronize(FThread, @TryAskQuestion);

      // Check result of TryAskQuestion.
      if FTryAskQuestionResult = False then
      begin
        // There is no UI assigned - wait until it is assigned.
        RTLeventWaitFor(FUserInterfaceAssignedEvent);

        // Check why the event was set.
        // It is either because an UI was assigned or because the operation is being aborted.
        if State in [fsosStopping, fsosStopped] then
        begin
          // The operation is being aborted.
          RaiseAbortOperation;
          break;
        end;
        // else we got an UI assigned - retry asking question
      end
      else
      begin
        // Received answer from the user.
        Result := FUIResponse;
        break;
      end;
    end;
  end
  else
  begin
    // The operation is probably run from main thread - call directly.
    TryAskQuestion;

    if FTryAskQuestionResult = False then
      // There is no UI assigned - assume default OK answer.
      Result := DefaultOKResponse
    else
      Result := FUIResponse;
  end;

  FStateLock.Acquire;
  try
    // Check, if the state is still the same as before asking question.
    if FState = fsosWaitingForFeedback then
    begin
      UpdateStartTime(SysUtils.Now);
      FState := OldState;
      bStateChanged := True;
    end;
  finally
    FStateLock.Release;
  end;

  if bStateChanged then
    NotifyStateChanged(OldState);
end;

procedure TFileSourceOperation.TryAskQuestion;
var
  Entry: PUserInterfacesEntry;
begin
  // This is run from GUI thread.

  FTryAskQuestionResult := False; // We have no answer yet.

  if FUserInterfaces.Count > 0 then
  begin
    // Get the UI that was most recently added.
    Entry := PUserInterfacesEntry(FUserInterfaces.Last);

    if Assigned(Entry) then
    begin
      FUIResponse := Entry^.UserInterface.AskQuestion(
                        FUIMessage,
                        FUIQuestion,
                        FUIPossibleResponses,
                        FUIDefaultOKResponse,
                        FUIDefaultCancelResponse);

      FTryAskQuestionResult := True;  // We do have an answer now.
    end;
  end;
  // else We have no UIs assigned - cannot ask question.
end;

procedure TFileSourceOperation.ReloadFileSources;
begin
  TThread.Synchronize(FThread, @DoReloadFileSources); // Calls virtual function
end;

procedure TFileSourceOperation.DoReloadFileSources;
begin
  // Nothing by default.
end;

class procedure TFileSourceOperation.RaiseAbortOperation;
begin
  raise EFileSourceOperationAborting.Create;
end;

constructor EFileSourceOperationAborting.Create;
begin
  inherited Create('aborting file source operation');
end;

end.

