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

constructor TFileSourceOperation.Create;
begin
  FState := fsosNotStarted;
  FDesiredState := fsosRunning;  // set for auto-start unless prevented by PreventStart
  FStopReason := fssrFinished;
  FProgress := 0;
  FPauseEvent := RTLEventCreate;
  FStateLock := TCriticalSection.Create;

  inherited Create;
end;

destructor TFileSourceOperation.Destroy;
begin
  inherited Destroy;

  RTLeventdestroy(FPauseEvent);
  FreeAndNil(FStateLock);
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

end.

