{
   Double Commander
   -------------------------------------------------------------------------
   Executing functions in a thread.

   Copyright (C) 2009 cobines (cobines@gmail.com)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}
unit uFunctionThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TFunctionThread = class(TThread)
  private
    FFunctionToCall: TThreadMethod;
    FExceptionMessage: String;
    FExceptionBackTrace: String;
    FWaitEvent: PRTLEvent;
    FFunctionFinished: TNotifyEvent;

  protected
    procedure Execute; override;

    procedure ShowException;
    procedure DoFunctionFinished;
    procedure CallFunctionFinished;

  public
    constructor Create(CreateSuspended: Boolean); reintroduce;
    destructor Destroy; override;

    procedure AssignFunction(AFunctionToCall: TThreadMethod);
    procedure Finish;

    property OnFunctionFinished: TNotifyEvent read FFunctionFinished write FFunctionFinished;
  end;

implementation

uses
  LCLProc, uExceptions;

constructor TFunctionThread.Create(CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;

  FWaitEvent := RTLEventCreate;
  FFunctionToCall := nil;

  inherited Create(CreateSuspended, DefaultStackSize);
end;

destructor TFunctionThread.Destroy;
begin
  RTLeventdestroy(FWaitEvent);

  inherited Destroy;
end;

procedure TFunctionThread.AssignFunction(AFunctionToCall: TThreadMethod);
begin
  if Assigned(AFunctionToCall) then
  begin
    if Assigned(FFunctionToCall) then
      raise Exception.Create('A function is already running in this thread');

    FFunctionToCall := AFunctionToCall;
    RTLeventSetEvent(FWaitEvent);
  end;
end;

procedure TFunctionThread.Finish;
begin
  Terminate;
  RTLeventSetEvent(FWaitEvent);
end;

procedure TFunctionThread.Execute;
begin
  while not Terminated do
  begin
    RTLeventResetEvent(FWaitEvent);

    if Assigned(FFunctionToCall) then
    begin
      try
        FFunctionToCall();
        DoFunctionFinished;
        FFunctionToCall := nil;
      except
        on e: Exception do
        begin
          FFunctionToCall := nil;

          FExceptionMessage := e.Message;
          FExceptionBackTrace := ExceptionToString;

          if FExceptionBackTrace <> EmptyStr then
            DebugLn(FExceptionBackTrace);

          Synchronize(@ShowException);
        end;
      end;
    end
    else
    begin
      RTLeventWaitFor(FWaitEvent);
    end;
  end;
end;

procedure TFunctionThread.DoFunctionFinished;
begin
  if Assigned(FFunctionFinished) then
    Synchronize(@CallFunctionFinished);
end;

procedure TFunctionThread.CallFunctionFinished;
begin
  FFunctionFinished(Self);
end;

procedure TFunctionThread.ShowException;
begin
  WriteExceptionToErrorFile(FExceptionBackTrace);
  ShowExceptionDialog(FExceptionMessage);
end;

end.

