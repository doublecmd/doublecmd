unit uMountWatcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Unix, BaseUnix, CTypes;

type

  { TMountWatcher }

  TMountWatcher = class
  private
    FOnMountEvent: TNotifyEvent;
  protected
    procedure DoMountEvent;
    procedure Handler(Sender: TObject);
    procedure ShowMessage(const Message: String);
  public
    procedure Start;
    property OnMountEvent: TNotifyEvent read FOnMountEvent write FOnMountEvent;
  end;

implementation

uses
  RtlConsts, DCOSUtils, uDebug, uPollThread;

{ TMountWatcher }

procedure TMountWatcher.DoMountEvent;
begin
  if Assigned(FOnMountEvent) then
    FOnMountEvent(Self);
end;

procedure TMountWatcher.Handler(Sender: TObject);
begin
  Sleep(1000);
  TThread.Synchronize(nil, @DoMountEvent);
  ShowMessage('DoMountEvent');
end;

procedure TMountWatcher.ShowMessage(const Message: String);
begin
  DCDebug(ClassName + ': ' + Message);
end;

procedure TMountWatcher.Start;
var
  fd: cint;
begin
  fd:= mbFileOpen('/proc/self/mounts', fmOpenRead);
  if (fd = feInvalidHandle) then
    ShowMessage(Format(SFOpenError, ['/proc/self/mounts']))
  else begin
    AddPoll(fd, POLLERR, @Handler, True);
  end;
end;

end.

