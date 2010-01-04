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
  Classes, SysUtils, syncobjs;

type
  TFunctionThreadMethod = procedure(Params: Pointer) of object;

  PFunctionThreadItem = ^TFunctionThreadItem;
  TFunctionThreadItem = record
    Method: TFunctionThreadMethod;
    Params: Pointer;
  end;

  TFunctionThread = class(TThread)
  private
    FFunctionsToCall: TFPList;
    FExceptionMessage: String;
    FExceptionBackTrace: String;
    FWaitEvent: PRTLEvent;
    FLock: TCriticalSection;
    FFinished: Boolean;
    FFinishedEvent: PRTLEvent;

  protected
    procedure Execute; override;

    procedure ShowException;

  public
    constructor Create(CreateSuspended: Boolean); reintroduce;
    destructor Destroy; override;

    procedure QueueFunction(AFunctionToCall: TFunctionThreadMethod; AParams: Pointer = nil);
    procedure Finish;

    class procedure WaitForWithSynchronize(AThread: TFunctionThread; WaitTimeMs: Cardinal = 0);

    property Finished: Boolean read FFinished;
    property FinishedEvent: PRTLEvent read FFinishedEvent;
  end;

implementation

uses
  LCLProc, uExceptions;

constructor TFunctionThread.Create(CreateSuspended: Boolean);
begin
  FreeOnTerminate := False;

  FWaitEvent := RTLEventCreate;
  FFunctionsToCall := TFPList.Create;
  FLock := TCriticalSection.Create;
  FFinished := False;
  FFinishedEvent := RTLEventCreate;

  inherited Create(CreateSuspended, DefaultStackSize);
end;

destructor TFunctionThread.Destroy;
var
  i: Integer;
begin
  RTLeventdestroy(FWaitEvent);

  FLock.Acquire;
  for i := 0 to FFunctionsToCall.Count - 1 do
    Dispose(PFunctionThreadItem(FFunctionsToCall[i]));
  FLock.Release;

  FreeThenNil(FFunctionsToCall);
  FreeThenNil(FLock);

  RTLeventSetEvent(FFinishedEvent);
  RTLeventdestroy(FFinishedEvent);

  inherited Destroy;
end;

procedure TFunctionThread.QueueFunction(AFunctionToCall: TFunctionThreadMethod; AParams: Pointer);
var
  pItem: PFunctionThreadItem;
begin
  if (not Terminated) and Assigned(AFunctionToCall) then
  begin
    New(pItem);
    pItem^.Method := AFunctionToCall;
    pItem^.Params := AParams;

    FLock.Acquire;
    try
      FFunctionsToCall.Add(pItem);
    finally
      FLock.Release;
    end;

    RTLeventSetEvent(FWaitEvent);
  end;
end;

procedure TFunctionThread.Finish;
begin
  Terminate;
  RTLeventSetEvent(FWaitEvent);
end;

procedure TFunctionThread.Execute;
var
  pItem: PFunctionThreadItem;
begin
  while (not Terminated) {or (FFunctionsToCall.Count > 0)} do
  begin
    RTLeventResetEvent(FWaitEvent);

    pItem := nil;

    FLock.Acquire;
    try
      if FFunctionsToCall.Count > 0 then
      begin
        pItem := PFunctionThreadItem(FFunctionsToCall[0]);
        FFunctionsToCall.Delete(0);
      end;
    finally
      FLock.Release;
    end;

    if Assigned(pItem) then
    begin
      try
        pItem^.Method(pItem^.Params);
        Dispose(pItem);
      except
        on e: Exception do
        begin
          Dispose(pItem);
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

  FFinished := True;
  RTLeventSetEvent(FFinishedEvent);
end;

procedure TFunctionThread.ShowException;
begin
  WriteExceptionToErrorFile(FExceptionBackTrace);
  ShowExceptionDialog(FExceptionMessage);
end;

class procedure TFunctionThread.WaitForWithSynchronize(AThread: TFunctionThread; WaitTimeMs: Cardinal);
var
  Waited: Cardinal = 0;
begin
  AThread.Finish;

  while not AThread.Finished do
  begin
    RTLeventWaitFor(AThread.FinishedEvent, 100);
    if AThread.Finished then
      Break;

    if WaitTimeMs > 0 then
    begin
      Inc(Waited, 100);
      if Waited >= WaitTimeMs then
      begin
        AThread.FreeOnTerminate := True;
        Exit;
      end;
    end;

    CheckSynchronize;
  end;

  // Can now safely wait without checking for Synchronize.
  WaitForThreadTerminate(AThread.Handle, WaitTimeMs - Waited);
  AThread.Free;
end;

end.

