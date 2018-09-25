unit uNetworkThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, JwaWinNetWk, Windows;

type

  { TNetworkThread }

  TNetworkThread = class(TThread)
  private
    FWaitFinish: TSimpleEvent;
    FWaitConnect: TSimpleEvent;
    NetResource: TNetResourceW;
  protected
    procedure Execute; override;
  public
    constructor Create(lpLocalName, lpRemoteName: LPWSTR; dwType: DWORD); reintroduce;
    destructor Destroy; override;
  public
    class function Connect(lpLocalName, lpRemoteName: LPWSTR; dwType: DWORD): Integer;
  end;

implementation

uses
  uMyWindows;

{ TNetworkThread }

procedure TNetworkThread.Execute;
begin
  // Function WNetAddConnection2W works very slow
  // when the final character is a backslash ('\')
  ReturnValue:= WNetAddConnection2W(@NetResource, nil, nil, CONNECT_INTERACTIVE);
  FWaitConnect.SetEvent;
  FWaitFinish.WaitFor(INFINITE);
end;

constructor TNetworkThread.Create(lpLocalName, lpRemoteName: LPWSTR; dwType: DWORD);
begin
  inherited Create(True);
  FreeOnTerminate:= True;
  FWaitFinish:= TSimpleEvent.Create;
  FWaitConnect:= TSimpleEvent.Create;
  ZeroMemory(@NetResource, SizeOf(TNetResourceW));
  if Assigned(lpLocalName) then begin
    NetResource.lpLocalName:= StrNew(lpLocalName);
  end;
  if Assigned(lpRemoteName) then begin
    NetResource.lpRemoteName:= StrNew(lpRemoteName);
  end;
  NetResource.dwType:= dwType;
end;

destructor TNetworkThread.Destroy;
begin
  FWaitFinish.Free;
  FWaitConnect.Free;
  StrDispose(NetResource.lpLocalName);
  StrDispose(NetResource.lpRemoteName);
  inherited Destroy;
end;

class function TNetworkThread.Connect(lpLocalName, lpRemoteName: LPWSTR; dwType: DWORD): Integer;
var
  Timeout: Integer = 10000;
begin
  with TNetworkThread.Create(lpLocalName, lpRemoteName, dwType) do
  begin
    Start;
    try
      while True do
      begin
        if (Timeout = 0) then Exit(WAIT_TIMEOUT);
        if (GetAsyncKeyStateEx(VK_ESCAPE)) then Exit(ERROR_CANCELLED);
        if (FWaitConnect.WaitFor(1) <> wrTimeout) then Exit(ReturnValue);
        Dec(Timeout);
      end;
    finally
      FWaitFinish.SetEvent;
    end;
  end;
end;

end.

