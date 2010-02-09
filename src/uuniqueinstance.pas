unit uUniqueInstance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleIPC;

type

  TOnUniqueInstanceMessage = procedure (Sender: TObject; Params: array of UTF8String; ParamCount: Integer) of object;

  { TUniqueInstance }

  TUniqueInstance = class
  private
    FInstanceName: UTF8String;
    FServerIPC: TSimpleIPCServer;
    FClientIPC: TSimpleIPCClient;
    FOnMessage: TOnUniqueInstanceMessage;

    procedure OnNative(Sender: TObject);

    procedure CreateServer;
    procedure CreateClient;
  public
    constructor Create(aInstanceName: String);
    destructor Destroy; override;

    function IsRunInstance: Boolean;
    procedure SendParams;
    procedure SendString(aStr: UTF8String);

    procedure RunListen;
    procedure StopListen;

    property OnMessage: TOnUniqueInstanceMessage read FOnMessage write FOnMessage;
  end;

function IsUniqueInstance(aInstanceName: String): Boolean;

{en
   Returns @true if current application instance is allowed to run.
   Returns @false if current instance should not be run.
}
function IsInstanceAllowed: Boolean;

var
  UniqueInstance: TUniqueInstance = nil;

implementation

uses
  StrUtils, FileUtil, uGlobs;

const
  Separator = '|';

{ TUniqueInstance }

procedure TUniqueInstance.OnNative(Sender: TObject);
var
  sTemp: UTF8String;
  sTempArray: array of UTF8String;
  mtMsgCount: TMessageType;
  I: Integer;
begin
  if Assigned(FOnMessage) then
    begin
      mtMsgCount:= FServerIPC.MsgType;
      sTemp:= FServerIPC.StringMessage;
      SetLength(sTempArray, mtMsgCount);
      for I:= 0 to mtMsgCount - 1 do
        sTempArray[I] := Copy2SymbDel(sTemp, Separator);
      FOnMessage(Self, sTempArray, mtMsgCount);
      SetLength(sTempArray, 0);
    end;
end;

procedure TUniqueInstance.CreateServer;
begin
  if FServerIPC = nil then
    begin
      FServerIPC:= TSimpleIPCServer.Create(nil);
      FServerIPC.OnMessage:= @OnNative;
    end;
  if FClientIPC <> nil then
    FreeAndNil(FClientIPC);
end;

procedure TUniqueInstance.CreateClient;
begin
  if FClientIPC = nil then
    FClientIPC:= TSimpleIPCClient.Create(nil);
end;

function TUniqueInstance.IsRunInstance: Boolean;
begin
  CreateClient;
  FClientIPC.ServerID:= FInstanceName;
  Result:= FClientIPC.ServerRunning;
end;

procedure TUniqueInstance.SendParams;
var
 sTemp: UTF8String;
 I: Integer;
begin
  CreateClient;
  FClientIPC.ServerID:= FInstanceName;
  if not FClientIPC.ServerRunning then Exit;
  sTemp:= EmptyStr;
  for I:= 1 to ParamCount do
    sTemp:= sTemp + Separator + SysToUTF8(ParamStr(I));
  try
    FClientIPC.Connect;
    FClientIPC.SendStringMessage(ParamCount, sTemp);
  finally
    FClientIPC.Disconnect;
  end;
end;

procedure TUniqueInstance.SendString(aStr: UTF8String);
begin
  CreateClient;
  FClientIPC.ServerID:= FInstanceName;
  if not FClientIPC.ServerRunning then Exit;
  try
    FClientIPC.Connect;
    FClientIPC.SendStringMessage(mtString, aStr + Separator);
  finally
    FClientIPC.Disconnect;
  end;
end;

procedure TUniqueInstance.RunListen;
begin
  CreateServer;
  FServerIPC.ServerID:= FInstanceName;
  FServerIPC.Global:= True;
  FServerIPC.StartServer;
end;

procedure TUniqueInstance.StopListen;
begin
  if FServerIPC = nil then Exit;
  FServerIPC.StopServer;
end;

constructor TUniqueInstance.Create(aInstanceName: String);
begin
  FInstanceName:= aInstanceName;
end;

destructor TUniqueInstance.Destroy;
begin
  if Assigned(FClientIPC) then
    FreeAndNil(FClientIPC);
  if Assigned(FServerIPC) then
    FreeAndNil(FServerIPC);
  inherited Destroy;
end;


function IsUniqueInstance(aInstanceName: String): Boolean;
begin
  Result:= True;
  UniqueInstance:= TUniqueInstance.Create(aInstanceName);
  if UniqueInstance.IsRunInstance then
   begin
    UniqueInstance.SendString('ShowMainForm');
    Exit(False);
   end;
  UniqueInstance.RunListen;
end;

function IsInstanceAllowed: Boolean;
begin
  Result := (not gOnlyOneAppInstance) or IsUniqueInstance(ApplicationName);
end;

finalization
  if Assigned(UniqueInstance) then
    begin
      UniqueInstance.StopListen;
      FreeAndNil(UniqueInstance);
    end;
end.

