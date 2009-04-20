unit uFakeThread;

{$mode objfpc}{$H+}

interface

{
  dummy thread class

  This is workaround for current FPC error with threads
  
  This class replace TThread implementation (and
  disable multible threads, but interface stay same,
  and in future only few changes and threads will work)
  
  In FPC 1.9.4 threads works, but Synchronize is not enabled
}

uses
  Classes, SysUtils; 
type


  TFakeThread =class
  protected
    FTerminated:Boolean;
  
  public
    constructor Create(CreateSuspended: Boolean);
    
    procedure Synchronize(Method: TThreadMethod);
    procedure Terminate;
    procedure Suspend;
    procedure Resume;
    function WaitFor: Integer;
    procedure Execute; virtual; abstract;
    
    property Terminated:Boolean read FTerminated;
{    FreeOnTerminate: Boolean;
    OnTerminate:TNotifyEvent;}
  end;


implementation
uses
  Forms;
{ TFakeThread }

constructor TFakeThread.Create(CreateSuspended: Boolean);
begin
  if not CreateSuspended then
     Resume;
end;

procedure TFakeThread.Synchronize(Method: TThreadMethod);
begin
  Method();
  Application.ProcessMessages;
end;

procedure TFakeThread.Terminate;
begin
  FTerminated:=True;
end;

procedure TFakeThread.Suspend;
begin
 // not implemented
end;

procedure TFakeThread.Resume;
begin
  Execute;
end;

function TFakeThread.WaitFor: Integer;
begin
  Terminate;
  while not FTerminated do
    Sleep(1);
end;

end.

