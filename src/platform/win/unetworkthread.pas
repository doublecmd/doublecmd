unit uNetworkThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, JwaWinNetWk, Windows, Forms, Graphics, uDrive;

type

  { TDriveIcon }

  TDriveIcon = class
  public
    Drive: TDrive;
    Bitmap: TBitmap;
    destructor Destroy; override;
  end;

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
    class function Connect(lpLocalName, lpRemoteName: LPWSTR; dwType: DWORD; CheckOperationState: TThreadMethod = nil): Integer;
  end;

  { TNetworkDriveLoader }

  TNetworkDriveLoader = class(TThread)
  private
    FDrive: TDrive;
    FIconSize: Integer;
    FBackColor: TColor;
    FCallback: TDataEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(ADrive: PDrive; AIconSize: Integer; ABackColor: TColor; ACallback: TDataEvent); reintroduce;
  end;

implementation

uses
   uMyWindows, uPixMapManager;

{ TDriveIcon }

destructor TDriveIcon.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

{ TNetworkDriveLoader }

procedure TNetworkDriveLoader.Execute;
var
  AIcon: TDriveIcon;
  AData: PtrInt absolute AIcon;
begin
  AIcon:= TDriveIcon.Create;
  AIcon.Drive:= FDrive;
  AIcon.Bitmap:= PixMapManager.GetDriveIcon(@FDrive, FIconSize, FBackColor);

  Application.QueueAsyncCall(FCallback, AData);
end;

constructor TNetworkDriveLoader.Create(ADrive: PDrive; AIconSize: Integer;
  ABackColor: TColor; ACallback: TDataEvent);
begin
  FDrive:= ADrive^;
  FIconSize:= AIconSize;
  FBackColor:= ABackColor;
  FCallback:= ACallback;
  inherited Create(True);
  FreeOnTerminate:= True;
end;

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

class function TNetworkThread.Connect(lpLocalName, lpRemoteName: LPWSTR;
  dwType: DWORD; CheckOperationState: TThreadMethod): Integer;
begin
  with TNetworkThread.Create(lpLocalName, lpRemoteName, dwType) do
  begin
    Start;
    try
      while True do
      begin
        if Assigned(CheckOperationState) then CheckOperationState
        else if (GetAsyncKeyStateEx(VK_ESCAPE)) then Exit(ERROR_CANCELLED);
        if (FWaitConnect.WaitFor(1) <> wrTimeout) then Exit(ReturnValue);
      end;
    finally
      FWaitFinish.SetEvent;
    end;
  end;
end;

end.

