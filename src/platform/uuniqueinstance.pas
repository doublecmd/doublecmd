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
    FHandle: THandle;
    FInstanceName: UTF8String;
    FServerIPC: TSimpleIPCServer;
    FClientIPC: TSimpleIPCClient;
    FOnMessage: TOnUniqueInstanceMessage;

    procedure OnNative(Sender: TObject);

    procedure CreateServer;
    procedure CreateClient;

    function IsRunning: Boolean;
    procedure DisposeMutex;
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
  {$IF DEFINED(MSWINDOWS)}
  Windows,
  {$ELSEIF DEFINED(UNIX)}
  ipc,
  {$ENDIF}
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

function TUniqueInstance.IsRunning: Boolean;
{$IF DEFINED(MSWINDOWS)}
var
  MutexName: AnsiString;
begin
  Result:= False;
  MutexName:= ExtractFileName(ParamStr(0));
  FHandle:= OpenMutex(MUTEX_MODIFY_STATE, False, PAnsiChar(MutexName));
  if FHandle = 0 then
    FHandle:= CreateMutex(nil, True, PAnsiChar(MutexName))
  else
    begin
      if WaitForSingleObject(FHandle, 0) <> WAIT_ABANDONED then
        Result:= True;
    end;
end;
{$ELSEIF DEFINED(UNIX)}
const
  SEM_PERM = 6 shl 6 { 0600 };
var
  semkey: TKey;
  status: longint = 0;
  arg: tsemun;

  function semlock(semid: longint): boolean;
  var
    p_buf: tsembuf;
  begin
    p_buf.sem_num := 0;
    p_buf.sem_op := 1;
    p_buf.sem_flg := SEM_UNDO;
    Result:= semop(semid, @p_buf, 1) = 0;
  end;

begin
  Result := False;
  semkey := ftok(PAnsiChar(ParamStr(0)), 0);
  FHandle := semget(semkey, 1, SEM_PERM or IPC_CREAT or IPC_EXCL);
  if FHandle = -1 then
    begin
      FHandle := semget(semkey, 1, 0);
      status := semctl(FHandle, 0, SEM_GETVAL, arg);
      if status = 1 then
        Result := True
      else
        semlock(FHandle);
    end
  else
    begin
      arg.val := 0;
      status := semctl(FHandle, 0, SEM_SETVAL, arg);
      semlock(FHandle);
    end;
end;
{$ENDIF}

procedure TUniqueInstance.DisposeMutex;
{$IF DEFINED(MSWINDOWS)}
begin
  ReleaseMutex(FHandle);
end;
{$ELSEIF DEFINED(UNIX)}
var
  arg: tsemun;
begin
  semctl(FHandle, 0, IPC_RMID, arg);
end;
{$ENDIF}

function TUniqueInstance.IsRunInstance: Boolean;
begin
  CreateClient;
  FClientIPC.ServerID:= FInstanceName;
  Result:= IsRunning and FClientIPC.ServerRunning;
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
    sTemp:= sTemp + SysToUTF8(ParamStr(I)) + Separator;
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
  DisposeMutex;
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
    UniqueInstance.SendParams;
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

