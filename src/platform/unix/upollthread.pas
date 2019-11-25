unit uPollThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Unix, BaseUnix;

procedure AddPoll(fd: cint; events: cshort; handler: TNotifyEvent; CloseOnDestroy: Boolean = True);
procedure RemovePoll(fd: cint);

implementation

uses
  DCUnix;

type

  { TPollRecord }

  TPollRecord = record
    handler: TNotifyEvent;
    CloseOnDestroy: Boolean;
  end;

  { TPollThread }

  TPollThread = class(TThread)
  private
    FCount: Cardinal;
    FEventPipe: TFilDes;
    FDesc: array of tpollfd;
    FHandler: array of TPollRecord;
  protected
    procedure Refresh;
    procedure Execute; override;
    procedure Clear(Sender: TObject);
  public
    procedure AddPoll(fd: cint; events: cshort; handler: TNotifyEvent; CloseOnDestroy: Boolean = True);

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

var
  Mutex: TRTLCriticalSection;
  PollThread: TPollThread = nil;

procedure Print(const sMessage: String);
begin
  WriteLn('PollThread: ', sMessage);
end;

procedure AddPoll(fd: cint; events: cshort; handler: TNotifyEvent;
  CloseOnDestroy: Boolean);
begin
  EnterCriticalSection(Mutex);
  try
    if not Assigned(PollThread) then begin
      PollThread:= TPollThread.Create;
    end;
    PollThread.AddPoll(fd, events, handler, CloseOnDestroy);
    Print('AddPoll ' + IntToStr(fd));
  finally
    LeaveCriticalSection(Mutex);
  end;
end;

procedure RemovePoll(fd: cint);
var
  Index: Integer;
begin
  EnterCriticalSection(Mutex);
  try
    for Index:= 0 to PollThread.FCount - 1 do
    begin
      if PollThread.FDesc[Index].fd = fd then
      begin
        PollThread.FDesc[Index].events:= 0;
        Break;
      end;
    end;
    Print('RemovePoll ' + IntToStr(fd));
  finally
    LeaveCriticalSection(Mutex);
  end;
end;

{ TPollThread }

procedure TPollThread.Clear(Sender: TObject);
var
  Symbol: Byte = 0;
begin
  // Clear pipe
  while FileRead(FEventPipe[0], Symbol, 1) <> -1 do;
end;

procedure TPollThread.Refresh;
var
  Symbol: Byte = 0;
begin
  FileWrite(FEventPipe[1], Symbol, 1);
end;

procedure TPollThread.Execute;
var
  i: cint;
  ret: cint;
begin
  while not Terminated do
  begin
    repeat
      ret:= fpPoll(@FDesc[0], FCount, -1);
    until (ret <> -1) or (fpGetErrNo <> ESysEINTR);
    if (ret = -1) then
    begin
      Print(SysErrorMessage(fpGetErrNo));
      Exit;
    end;
    for i := 0 to FCount - 1 do
    begin
      if (FDesc[i].events and FDesc[i].revents <> 0) then
      begin
        FHandler[i].handler(Self);
      end;
    end;
  end;
end;

procedure TPollThread.AddPoll(fd: cint; events: cshort;
  handler: TNotifyEvent; CloseOnDestroy: Boolean);
var
  NewLength: Integer;
begin
  NewLength:= FCount + 1;
  SetLength(FDesc, NewLength);
  SetLength(FHandler, NewLength);

  FDesc[FCount].fd:= fd;
  FDesc[FCount].events:= events;

  FHandler[FCount].handler:= handler;
  FHandler[FCount].CloseOnDestroy:= CloseOnDestroy;

  InterLockedIncrement(FCount);

  if FCount = 2 then
  begin
    Start;
    Print('Start polling');
  end;

  Refresh;
end;

constructor TPollThread.Create;
begin
  inherited Create(True);
  // Create pipe for user triggered fake event
  FEventPipe[0] := -1;
  FEventPipe[1] := -1;
  if fpPipe(FEventPipe) < 0 then
    Print(SysErrorMessage(fpGetErrNo))
  else begin
    // Set both ends of pipe non blocking
    FileCloseOnExec(FEventPipe[0]); FileCloseOnExec(FEventPipe[1]);
    FpFcntl(FEventPipe[0], F_SetFl, FpFcntl(FEventPipe[0], F_GetFl) or O_NONBLOCK);
    FpFcntl(FEventPipe[1], F_SetFl, FpFcntl(FEventPipe[1], F_GetFl) or O_NONBLOCK);
  end;
  Self.AddPoll(FEventPipe[0], POLLIN, @Clear, True);
end;

destructor TPollThread.Destroy;
var
  Index: Integer;
begin
  Terminate;
  Refresh;

  inherited Destroy;

  // Close both ends of pipe
  if FEventPipe[1] <> -1 then
  begin
    FileClose(FEventPipe[1]);
    FEventPipe[1] := -1;
  end;
  for Index:= 0 to FCount - 1 do
  begin
    if FHandler[Index].CloseOnDestroy then
      FileClose(FDesc[Index].fd);
  end;

  Print('Finish polling');
end;

initialization
  InitCriticalSection(Mutex);

finalization
  PollThread.Free;
  DoneCriticalSection(Mutex);

end.

