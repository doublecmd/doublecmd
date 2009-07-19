unit uFileSourceOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
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


  {en
     Base class for each file source operation.
  }
  TFileSourceOperation = class
  private
    FProgress: Integer;

    {en
       Should return a general progress (0-100) of the operation.
       Specific statistics are returned by the individual operations.

       It is only read from the main thread and written to by operation
       executing thread, thus probably doesn't need synchronization.
    }
    function GetProgress: Integer;

  protected
    FState: TFileSourceOperationState;             // both must be synchronized
    FStopReason: TFileSourceOperationStopReason;

    procedure UpdateProgress(NewProgress: Integer);

    {en
       This should be set to the correct file operation type in each concrete descendant.
       We rely on this when making a decision based on operation type. This way
       it's easier to maintain different sorts of things we can do with operations
       and statistics, without having to include knowledge of those things
       in the operations classes hierarchy.
    }
    function GetID: TFileSourceOperationType; virtual abstract;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    {en
       Tries to execute operation.
       First immediately sets state to fsosStarting and after it is started to fsosRunning.
    }
    procedure Execute; virtual abstract;

    {en
       Tries to put operation into a paused state.
       First immediately sets state to fsosPausing and after it is paused to fsosPaused.
    }
    procedure Pause; virtual abstract;
    {en
       Tries to stop operation.
       First immediately sets state to fsosStopping and after it is stopped to fsosStopped.
    }
    procedure Stop; virtual abstract;

    property Progress: Integer read GetProgress;
    property ID: TFileSourceOperationType read GetID;
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

    {en If the operation should terminate.}
    function Terminate: Boolean;

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
  FStopReason := fssrFinished;
  FProgress := 0;

  inherited Create;
end;

destructor TFileSourceOperation.Destroy;
begin
  inherited Destroy;
end;

function TFileSourceOperation.GetProgress: Integer;
begin
  // Doesn't need synchronization.
  Result := FProgress;
end;

procedure TFileSourceOperation.UpdateProgress(NewProgress: Integer);
begin
  // Doesn't need synchronization.
  FProgress := NewProgress;
end;

end.

