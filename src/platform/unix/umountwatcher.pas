unit uMountWatcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Unix, BaseUnix, CTypes;

type

  { TMountWatcher }

  TMountWatcher = class(TThread)
  private
    FOnMountEvent: TNotifyEvent;
    procedure DoMountEvent;
    procedure ShowMessage(const Message: String);
  protected
    procedure Execute; override;
  public
    property OnMountEvent: TNotifyEvent read FOnMountEvent write FOnMountEvent;
  end;

implementation

uses
  RtlConsts, uDebug;

{ TMountWatcher }

procedure TMountWatcher.DoMountEvent;
begin
  if Assigned(FOnMountEvent) then
    FOnMountEvent(Self);
end;

procedure TMountWatcher.ShowMessage(const Message: String);
begin
  DCDebug(ClassName + ': ' + Message);
end;

procedure TMountWatcher.Execute;
var
  fd: cint;
  ret: cint;
  fds: TFDSet;
begin
  fd:= fpOpen('/proc/self/mounts', O_RDONLY);
  if (fd = feInvalidHandle) then
    ShowMessage(Format(SFOpenError, ['/proc/self/mounts']))
  else try
    while not Terminated do
    begin
      fpFD_ZERO(fds);
      fpFD_SET(fd, fds);
      ret:= fpSelect(fd + 1, nil, nil, @fds, 20);
      if (ret = 0) then Continue;
      if (ret = -1) then
      begin
        ShowMessage('fpSelect failed');
        Break;
      end;
      if (ret > 0) and (fpFD_ISSET(fd, fds) = 1) then
      begin
        Sleep(1000);
        Synchronize(@DoMountEvent);
        ShowMessage('DoMountEvent');
      end;
    end;
  finally
    fpClose(fd);
  end;
end;

end.

