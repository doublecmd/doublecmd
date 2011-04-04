{
   Double Commander
   -------------------------------------------------------------------------
   Executing functions in a thread.

   Copyright (C) 2009-2011 Przemysław Nagay (cobines@gmail.com)

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
    FWaitEvent: PRTLEvent;
    FLock: TCriticalSection;
    FFinished: Boolean;

  protected
    procedure Execute; override;

  public
    constructor Create(CreateSuspended: Boolean); reintroduce;
    destructor Destroy; override;

    procedure QueueFunction(AFunctionToCall: TFunctionThreadMethod; AParams: Pointer = nil);
    procedure Finish;

    class procedure Finalize(var AThread: TFunctionThread);

    property Finished: Boolean read FFinished;
  end;

implementation

uses
  LCLProc, uDebug, uExceptions;

constructor TFunctionThread.Create(CreateSuspended: Boolean);
begin
  FWaitEvent := RTLEventCreate;
  FFunctionsToCall := TFPList.Create;
  FLock := TCriticalSection.Create;
  FFinished := False;
  FreeOnTerminate := False;

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
  try
    while (not Terminated) or (FFunctionsToCall.Count > 0) do
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
            HandleException(e, Self);
          end;
        end;
      end
      else
      begin
        RTLeventWaitFor(FWaitEvent);
      end;
    end;
  finally
    FFinished := True;
  end;
end;

class procedure TFunctionThread.Finalize(var AThread: TFunctionThread);
begin
  AThread.Finish;
{$IF (fpc_version<2) or ((fpc_version=2) and (fpc_release<5))}
  If (MainThreadID=GetCurrentThreadID) then
    while not AThread.Finished do
      CheckSynchronize(100);
{$ENDIF}
  AThread.WaitFor;
  AThread.Free;
  AThread := nil;
end;

end.

