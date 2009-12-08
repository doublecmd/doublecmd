{
   Double Commander
   -------------------------------------------------------------------------
   Thread-safe asynchronous call queue.
   It allows queueing methods that should be called by GUI thread.

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
unit uGuiMessageQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

type
  TGuiMessageProc = procedure (Data: Pointer) of object;

  PMessageQueueItem = ^TMessageQueueItem;
  TMessageQueueItem = record
    Method: TGuiMessageProc;
    Data  : Pointer;
    Next  : PMessageQueueItem;
  end;

  TGuiMessageQueueThread = class(TThread)
  private
    FWakeThreadEvent: PRTLEvent;
    FMessageQueue: PMessageQueueItem;
    FMessageQueueLastItem: PMessageQueueItem;
    FMessageQueueLock: TCriticalSection;

    procedure CallMethods;

  public
    constructor Create(CreateSuspended: Boolean = False); reintroduce;
    destructor Destroy; override;
    procedure Terminate;
    procedure Execute; override;

    {en
       @param(AllowDuplicates
              If @false then if the queue already has AMethod with
              AData parameter then it is not queued for a second time.
              If @true then the same methods with the same parameters
              are allowed to exists multiple times in the queue.)
    }
    procedure QueueMethod(AMethod: TGuiMessageProc; AData: Pointer;
                          AllowDuplicates: Boolean = True);
  end;

  procedure InitializeGuiMessageQueue;
  procedure FinalizeGuiMessageQueue;

var
  GuiMessageQueue: TGuiMessageQueueThread;

implementation

constructor TGuiMessageQueueThread.Create(CreateSuspended: Boolean = False);
begin
  FWakeThreadEvent := RTLEventCreate;
  FMessageQueue := nil;
  FMessageQueueLastItem := nil;
  FMessageQueueLock := TCriticalSection.Create;

  inherited Create(CreateSuspended, DefaultStackSize);
  FreeOnTerminate := True;
end;

destructor TGuiMessageQueueThread.Destroy;
var
  item: PMessageQueueItem;
begin
  // Make sure the thread is not running anymore.
  Terminate;

  FMessageQueueLock.Acquire;
  while Assigned(FMessageQueue) do
  begin
    item := FMessageQueue^.Next;
    Dispose(FMessageQueue);
    FMessageQueue := item;
  end;
  FMessageQueueLock.Release;

  RTLeventdestroy(FWakeThreadEvent);
  FreeAndNil(FMessageQueueLock);

  inherited Destroy;
end;

procedure TGuiMessageQueueThread.Terminate;
begin
  inherited Terminate;
  // Wake after setting Terminate to True.
  RTLeventSetEvent(FWakeThreadEvent);
end;

procedure TGuiMessageQueueThread.Execute;
begin
  while not Terminated do
  begin
    if Assigned(FMessageQueue) then
      // Call some methods.
      Synchronize(@CallMethods)
    else
      // Wait for messages.
      RTLeventWaitFor(FWakeThreadEvent);
  end;
end;

procedure TGuiMessageQueueThread.QueueMethod(AMethod: TGuiMessageProc; AData: Pointer;
                                             AllowDuplicates: Boolean = True);
var
  item: PMessageQueueItem;
begin
  FMessageQueueLock.Acquire;
  try
    if AllowDuplicates = False then
    begin
      // Search the queue for this method and parameter.
      item := FMessageQueue;
      while Assigned(item) do
      begin
        if (item^.Method = AMethod) and (item^.Data = AData) then
          Exit;
        item := item^.Next;
      end;
    end;

    New(item);
    item^.Method := AMethod;
    item^.Data   := AData;
    item^.Next   := nil;

    if not Assigned(FMessageQueue) then
      FMessageQueue := item
    else
      FMessageQueueLastItem^.Next := item;

    FMessageQueueLastItem := item;
    RTLeventSetEvent(FWakeThreadEvent);

  finally
    FMessageQueueLock.Release;
  end;
end;

procedure TGuiMessageQueueThread.CallMethods;
var
  MaxMessages: Integer = 10; // Call this maximum methods per one Synchronize.
  item: PMessageQueueItem;
begin
  while Assigned(FMessageQueue) and (MaxMessages > 0) do
  begin
    // Call method with parameter.
    FMessageQueue^.Method(FMessageQueue^.Data);

    FMessageQueueLock.Acquire;
    try
      item := FMessageQueue^.Next;
      Dispose(FMessageQueue);
      FMessageQueue := item;

      // If queue is empty then reset wait event (must be done under lock).
      if not Assigned(FMessageQueue) then
        RTLeventResetEvent(FWakeThreadEvent);
    finally
      FMessageQueueLock.Release;
    end;

    Dec(MaxMessages, 1);
  end;
end;

// ----------------------------------------------------------------------------

procedure InitializeGuiMessageQueue;
begin
  GuiMessageQueue := TGuiMessageQueueThread.Create(False);
end;

procedure FinalizeGuiMessageQueue;
begin
  GuiMessageQueue.Terminate;
  WaitForThreadTerminate(GuiMessageQueue.ThreadID, 10000); // wait max 10 seconds
end;

initialization
  InitializeGuiMessageQueue;

finalization
  FinalizeGuiMessageQueue;

end.

