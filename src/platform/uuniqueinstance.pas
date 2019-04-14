unit uUniqueInstance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimpleIPC, uCmdLineParams, RegExpr;

type

  TOnUniqueInstanceMessage = procedure (Sender: TObject; Params: TCommandLineParams) of object;

  { TUniqueInstance }

  TUniqueInstance = class
  private
    FHandle: THandle;
    FInstanceName: String;
    FServerIPC: TSimpleIPCServer;
    FClientIPC: TSimpleIPCClient;
    FOnMessage: TOnUniqueInstanceMessage;
    FServernameByUser: String;
    {$IF DEFINED(UNIX)}
    FMyProgramCreateSemaphore:Boolean;
    {$ENDIF}

    procedure OnNative(Sender: TObject);
    procedure OnMessageQueued(Sender: TObject);

    procedure CreateServer;
    procedure CreateClient;

    function IsRunning: Boolean;
    procedure DisposeMutex;
  public
    constructor Create(aInstanceName: String; aServernameByUser: String);
    destructor Destroy; override;

    function IsRunInstance: Boolean;
    procedure SendParams;

    procedure RunListen;
    procedure StopListen;
    function isAnotherDCRunningWhileIamRunning:boolean;

    property OnMessage: TOnUniqueInstanceMessage read FOnMessage write FOnMessage;
    property ServernameByUser: String read FServernameByUser;
  end;


procedure InitInstance;
procedure InitInstanceWithName(aInstanceName: String; aServernameByUser: String);
function IsUniqueInstance: Boolean;
function WantsToBeClient: Boolean;

{en
   Returns @true if current application instance is allowed to run.
   Returns @false if current instance should not be run.
}
function IsInstanceAllowed: Boolean;

var
  UniqueInstance: TUniqueInstance = nil;
  FIsUniqueInstance: Boolean = False;

implementation

uses
  {$IF DEFINED(MSWINDOWS)}
  Windows,
  {$ELSEIF DEFINED(UNIX)}
  ipc, baseunix, uPipeServer, uXdg,
  {$ENDIF}
  Forms, StrUtils, FileUtil, uGlobs, uDebug;

{$IF DEFINED(DARWIN)}
const
  SEM_GETVAL = 5; // Return the value of semval (READ)
  SEM_SETVAL = 8; // Set the value of semval to arg.val (ALTER)
{$ENDIF}

{ TUniqueInstance }

procedure TUniqueInstance.OnNative(Sender: TObject);
var
  Params: TCommandLineParams;
begin
  if Assigned(FOnMessage) then
    begin
      FServerIPC.MsgData.Seek(0, soFromBeginning);
      FServerIPC.MsgData.ReadBuffer(Params, FServerIPC.MsgType);
      FOnMessage(Self, Params);
    end;
end;

procedure TUniqueInstance.OnMessageQueued(Sender: TObject);
begin
  {$IF (FPC_FULLVERSION >= 030001)}
  FServerIPC.ReadMessage;
  {$ENDIF}
end;

procedure TUniqueInstance.CreateServer;
begin
  if FServerIPC = nil then
    begin
      FServerIPC:= TSimpleIPCServer.Create(nil);
      FServerIPC.OnMessage:= @OnNative;
      {$IF DEFINED(MSWINDOWS) and (FPC_FULLVERSION >= 030001)}
      FServerIPC.OnMessageQueued:= @OnMessageQueued;
      {$ENDIF}
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

  function id: byte;
  var
    UserID: LongRec;
  begin
    Result := 0;
    UserID := LongRec(fpGetUID);
    Result := Result xor UserID.Bytes[0];
    Result := Result xor UserID.Bytes[1];
    Result := Result xor UserID.Bytes[2];
    Result := Result xor UserID.Bytes[3];
  end;

  function semlock(semid: longint): boolean;
  // increase special Value in semaphore structure (value decreases automatically
  // when program completed incorrectly)
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
  semkey := ftok(PAnsiChar(ParamStr(0)), id);
  // try create semapore for semkey
  // If semflg specifies both IPC_CREAT and IPC_EXCL and a semaphore set already
  // exists for semkey, then semget() return -1 and errno set to EEXIST
  FHandle := semget(semkey, 1, SEM_PERM or IPC_CREAT or IPC_EXCL);

  // if semaphore exists
  if FHandle = -1 then
    begin
      // get semaphore id
      FHandle := semget(semkey, 1, 0);
      // get special Value from semaphore structure
      status := semctl(FHandle, 0, SEM_GETVAL, arg);

      if status = 1 then
      // There is other running copy of the program
        begin
          Result := True;
          // Not to release semaphore when exiting from the program
          FMyProgramCreateSemaphore := false;
        end
      else
      begin
      // Other copy of the program has created a semaphore but has been completed incorrectly
      // increase special Value in semaphore structure (value decreases automatically
      // when program completed incorrectly)
        semlock(FHandle);

      // its one copy of program running, release semaphore when exiting from the program
        FMyProgramCreateSemaphore := true;
      end;
    end
  else
    begin
      // its one copy of program running, release semaphore when exiting from the program
      FMyProgramCreateSemaphore := true;
      // set special Value in semaphore structure to 0
      arg.val := 0;
      status := semctl(FHandle, 0, SEM_SETVAL, arg);
      // increase special Value in semaphore structure (value decreases automatically
      // when program completed incorrectly)
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
  // If my copy of the program created a semaphore then released it
  if FMyProgramCreateSemaphore then
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
  Stream: TMemoryStream = nil;
begin
  CreateClient;
  FClientIPC.ServerID:= FInstanceName;
  if not FClientIPC.ServerRunning then
    Exit;

  Stream:= TMemoryStream.Create;
  Stream.WriteBuffer(CommandLineParams, SizeOf(TCommandLineParams));
  try
    FClientIPC.Connect;
    Stream.Seek(0, soFromBeginning);
    FClientIPC.SendMessage(SizeOf(TCommandLineParams), Stream);
  finally
    Stream.Free;
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

function TUniqueInstance.isAnotherDCRunningWhileIamRunning:boolean;
var
  LocalClientIPC: TSimpleIPCClient;
  IndexInstance:integer;

function GetServerIdNameToCheck:string;
begin
  Result:= ApplicationName;
  if IndexInstance > 1 then Result+= '-' + IntToStr(IndexInstance);
{$IF DEFINED(UNIX)}
  Result:= IncludeTrailingBackslash(GetUserRuntimeDir) + Result + '.pipe';
{$ENDIF}
end;

begin
  Result:=True;

  if IsRunning then
  begin
    FServerIPC.Active:=False;
    try
      LocalClientIPC:=TSimpleIPCClient.Create(nil);
      IndexInstance:=1;
      Result:=FALSE;
      repeat
        LocalClientIPC.ServerID:=GetServerIdNameToCheck;
        Result:=LocalClientIPC.ServerRunning;
        inc(IndexInstance);
      until Result OR (IndexInstance>=10);
    finally
      FServerIPC.Active:=True;
    end;
  end;
end;

constructor TUniqueInstance.Create(aInstanceName: String; aServernameByUser: String);
begin
  FInstanceName:= aInstanceName;
  FServernameByUser:= aServernameByUser;
  if Length(FServernameByUser) > 0 then
    FInstanceName+= '-' + FServernameByUser;
{$IF DEFINED(UNIX)}
  FInstanceName:= IncludeTrailingBackslash(GetUserRuntimeDir) + FInstanceName + '.pipe';
{$ENDIF}
end;

destructor TUniqueInstance.Destroy;
begin
  if Assigned(FClientIPC) then
    FreeAndNil(FClientIPC);
  if Assigned(FServerIPC) then
    FreeAndNil(FServerIPC);
  inherited Destroy;
end;



{en 
  Initializes instance with given name (currently this is ApplicationName),
  and user-provided servername (typically, an empty string).

  If there is no already existing instance, then create it.
  If there is already existing instance, and the current one is a client,
    then send params to the server (i.e. to the existing instance)
  If there is already existing instance, and the current one is not a client,
    (i.e. gOnlyOneAppInstance is false and no --client/-c options were given),
    then user-provided servername is altered: firstly, just add a trailing number '2'.
    If there is already some trailing number, then increase it by 1, until
    we found a servername that isn't busy yet, and then create instance
    with this servername.
}
procedure InitInstanceWithName(aInstanceName: String; aServernameByUser: String);

  {en
    If a given servername doesn't contain a trailing number, then
    add a trailing number '2'; otherwise increase existing number
    and return resulting string.
  }
  function GetNextServername(CurServername: String): String;
  var
    SNameRegExp: TRegExpr;
    CurNumber: Integer;
  begin
    SNameRegExp := TRegExpr.Create();
    try
      SNameRegExp.Expression := '(\d+)$';
      if SNameRegExp.Exec(CurServername) then
      begin
        //-- there is existing trailing number, so, increase it by 1
        CurNumber := StrToInt(SNameRegExp.Match[1]) + 1;
        Result := ReplaceRegExpr(SNameRegExp.Expression, CurServername, IntToStr(CurNumber), False);
      end
      else
        //-- there is no trailing number, so, add a trailing number '2'
        Result := CurServername + '2';

    finally
      SNameRegExp.Free;
    end;

  end;

begin
  FIsUniqueInstance := True;

  //-- determine if the instance with the same name already exists
  UniqueInstance := TUniqueInstance.Create(aInstanceName, aServernameByUser);
  if UniqueInstance.IsRunInstance then
    //-- it does exist, so, set flag that instance is not unique
    FIsUniqueInstance := False;

  //-- if instance is allowed (i.e. is not a client), then find unique
  //   servername
  if IsInstanceAllowed then
  begin
    while UniqueInstance.IsRunInstance do
    begin
      UniqueInstance.Free;
      aServernameByUser := GetNextServername(aServernameByUser);
      UniqueInstance := TUniqueInstance.Create(aInstanceName, aServernameByUser);
    end;

    //-- unique servername is found, so, run it as a server and set 
    //   FIsUniqueInstance flag
    UniqueInstance.RunListen;
    FIsUniqueInstance := True
  end
  else
    //-- if this instance is not allowed (i.e. it's a client), then send params to the server.
    UniqueInstance.SendParams;

end;

{en 
  Initialize instance with an application name and user-provided servername
  (see detailed comment for InitInstanceWithName)
}
procedure InitInstance;
begin
  InitInstanceWithName(ApplicationName, CommandLineParams.Servername);
end;



function IsUniqueInstance: Boolean;
begin
   Result := FIsUniqueInstance;
end;

function WantsToBeClient: Boolean;
begin
   Result := (gOnlyOneAppInstance or CommandLineParams.Client);
end;


function IsInstanceAllowed: Boolean;
begin
  Result := (not WantsToBeClient) or IsUniqueInstance;
end;

finalization
  if Assigned(UniqueInstance) then
    begin
      UniqueInstance.StopListen;
      FreeAndNil(UniqueInstance);
    end;
end.

