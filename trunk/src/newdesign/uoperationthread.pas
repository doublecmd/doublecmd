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

    function CheckTerminated: Boolean;

  protected
    procedure Execute; override;

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

constructor TOperationThread.Create(CreateSuspended: Boolean; Operation: TFileSourceOperation);
begin
  FreeOnTerminate := True;

  FOperation := Operation;
  //FOperation.UI.Terminate := @CheckTerminated;

  inherited Create(CreateSuspended, DefaultStackSize);
end;

destructor TOperationThread.Destroy;
begin
  inherited Destroy;
end;

procedure TOperationThread.Execute;
begin
  FOperation.Execute;
end;

function TOperationThread.CheckTerminated: Boolean;
begin
  Result := Terminated;
end;

end.

