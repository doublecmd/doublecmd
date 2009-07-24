unit uOperationThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation, LCLProc;

type

  {en
     Thread executing a file source operation.
  }
  TOperationThread = class(TThread)
  private
    FOperation: TFileSourceOperation;
    FExceptionMessage: String;

  protected
    procedure Execute; override;

    procedure ShowException;

  public
    {en
       Creates a new thread for executing an operation.
       @param(CreateSuspended if @true the thread is not immediately started on creation.)
       @param(Operation is the file source operation that will be executed.)
    }
    constructor Create(CreateSuspended: Boolean; Operation: TFileSourceOperation); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs;

constructor TOperationThread.Create(CreateSuspended: Boolean; Operation: TFileSourceOperation);
begin
  FreeOnTerminate := True;

  FOperation := Operation;

  inherited Create(CreateSuspended, DefaultStackSize);

  FOperation.AssignThread(Self); // This may be executed from the thread already.
end;

destructor TOperationThread.Destroy;
begin
  inherited Destroy;
end;

procedure TOperationThread.Execute;
var
  FrameCount: integer;
  Frames: PPointer;
  FrameNumber:Integer;
begin
  try
    FOperation.Execute;
  except
    on e: Exception do
    begin
      FExceptionMessage := 'Unhandled exception ' + e.ClassName + ': ' + e.Message + LineEnding
                         + 'Stack trace:' + LineEnding
                         + BackTraceStrFunc(ExceptAddr) + LineEnding;

      FrameCount:=ExceptFrameCount;
      Frames:=ExceptFrames;
      for FrameNumber := 0 to FrameCount-1 do
        FExceptionMessage := FExceptionMessage + BackTraceStrFunc(Frames[FrameNumber]) + LineEnding;

      if IsConsole then
        DebugLn(FExceptionMessage);

      Synchronize(@ShowException);
    end;
  end;
end;

procedure TOperationThread.ShowException;
begin
  MessageDlg('Double Commander error', FExceptionMessage, mtError, [mbOK], 0);
end;

end.

