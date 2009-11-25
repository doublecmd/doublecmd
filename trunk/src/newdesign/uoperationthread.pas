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
    FExceptionMessage: String;
    FExceptionBackTrace: String;

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
  LCLProc, uExceptions, uGlobs;

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
begin
  try
    FOperation.Execute;
  except
    on e: Exception do
    begin
      FExceptionMessage := e.Message;
      FExceptionBackTrace := ExceptionToString;

      if FExceptionBackTrace <> EmptyStr then
        DebugLn(FExceptionBackTrace);

      Synchronize(@ShowException);
    end;
  end;
end;

procedure TOperationThread.ShowException;
begin
  WriteExceptionToFile(gErrorFile, FExceptionBackTrace);
  ShowExceptionDialog(FExceptionMessage);
end;

end.

