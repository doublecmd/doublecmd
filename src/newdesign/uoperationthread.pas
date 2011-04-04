unit uOperationThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation;

type

  {en
     Thread executing a file source operation.
  }
  TOperationThread = class(TThread)
  private
    FOperation: TFileSourceOperation;

  protected
    procedure Execute; override;

  public
    {en
       Creates a new thread for executing an operation.
       @param(CreateSuspended if @true the thread is not immediately started on creation.)
       @param(Operation is the file source operation that will be executed.)
    }
    constructor Create(CreateSuspended: Boolean; Operation: TFileSourceOperation); reintroduce;
  end;

implementation

uses
  uDebug, uExceptions;

constructor TOperationThread.Create(CreateSuspended: Boolean; Operation: TFileSourceOperation);
begin
  FreeOnTerminate := True;
  FOperation := Operation;
  FOperation.AssignThread(Self);

  inherited Create(CreateSuspended, DefaultStackSize);
end;

procedure TOperationThread.Execute;
begin
  try
    FOperation.Execute;
  except
    on e: Exception do
      HandleException(e, Self);
  end;
end;

end.

