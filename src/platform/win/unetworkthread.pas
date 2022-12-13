unit uNetworkThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, JwaWinNetWk, Windows, Forms, Graphics, Dialogs,
  StdCtrls, ComCtrls, Buttons, uDrive, uOSForms;

type

  { TDriveIcon }

  TDriveIcon = class
  public
    Drive: TDrive;
    Bitmap: TBitmap;
    destructor Destroy; override;
  end;

  { TNetworkForm }

  TNetworkForm = class(TModalDialog)
    lblPrompt: TLabel;
    btnAbort: TBitBtn;
    pbConnect: TProgressBar;
  private
    FThread: TThread;
  public
    constructor Create(TheOwner: TComponent; AThread: TThread; APath: PWideChar); reintroduce;
    procedure ExecuteModal; override;
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
    class function Connect(lpLocalName, lpRemoteName: LPWSTR; dwType: DWORD): Integer; overload;
    class function Connect(lpLocalName, lpRemoteName: LPWSTR; dwType: DWORD; CheckOperationState: TThreadMethod): Integer; overload;
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
  Math, InterfaceBase, Controls, fMain, uMyWindows, uPixMapManager, uLng;

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

{ TNetworkForm }

constructor TNetworkForm.Create(TheOwner: TComponent; AThread: TThread;
  APath: PWideChar);
begin
  FThread:= AThread;
  inherited CreateNew(TheOwner);

  AutoSize:= True;
  BorderStyle:= bsDialog;
  Caption:= Application.Title;
  Position:= poOwnerFormCenter;
  ChildSizing.TopBottomSpacing:= 6;
  ChildSizing.LeftRightSpacing:= 6;

  lblPrompt := TLabel.Create(Self);
  with lblPrompt do
  begin
    Parent:= Self;
    AutoSize:= True;
    Caption:= rsOperWaitingForConnection + ' ' + UTF8Encode(UnicodeString(APath));
  end;
  pbConnect:= TProgressBar.Create(Self);
  with pbConnect do
  begin
    Parent:= Self;
    Style:= pbstMarquee;
    AnchorToNeighbour(akTop, 6, lblPrompt);
    Constraints.MinWidth:= Math.Max(280, Screen.Width div 4);
  end;
  btnAbort:= TBitBtn.Create(Self);
  with btnAbort do
  begin
    Parent:= Self;
    Kind:= bkAbort;
    Default:= True;
    Cancel:= True;
    AutoSize:= True;
    AnchorHorizontalCenterTo(Self);
    AnchorToNeighbour(akTop, 12, pbConnect);
  end;
end;

procedure TNetworkForm.ExecuteModal;
begin
  repeat
    WidgetSet.AppProcessMessages;

    if Application.Terminated then
    begin
      ModalResult:= mrCancel;
    end;

    if ModalResult <> 0 then
    begin
      CloseModal;
      if ModalResult <> 0 then Break;
    end;

    with TNetworkThread(FThread) do
    begin
      if (FWaitConnect.WaitFor(1) <> wrTimeout) then
      begin
        ModalResult:= mrOK;
        Break;
      end;
    end;

    Application.Idle(True);
  until False;
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
  dwType: DWORD): Integer;
var
  AStartTime: QWord;
  AThread: TNetworkThread;
begin
  AThread:= TNetworkThread.Create(lpLocalName, lpRemoteName, dwType);
  with AThread do
  begin
    Start;
    AStartTime:= GetTickCount64;
    try
      while True do
      begin
        if (GetTickCount64 - AStartTime > 3000) then
        begin
          with TNetworkForm.Create(frmMain, AThread, lpRemoteName) do
          try
            if (ShowModal = mrOK) then
              Exit(ReturnValue)
            else begin
              Exit(ERROR_CANCELLED);
            end;
          finally
            Free;
          end;
        end;
        if (GetAsyncKeyStateEx(VK_ESCAPE)) then Exit(ERROR_CANCELLED);
        if (FWaitConnect.WaitFor(1) <> wrTimeout) then Exit(ReturnValue);
      end;
    finally
      FWaitFinish.SetEvent;
    end;
  end;
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

