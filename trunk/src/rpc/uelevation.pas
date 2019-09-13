unit uElevation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uClientServer, uService, uWorker;

type

  { TMasterProxy }

  TMasterProxy = class
  private
    FClient: TBaseTransport;
  public
    function Execute: LongBool;
  public
    constructor Create(const AName: String);
    class function Instance: TMasterProxy;
  end;

  { TWorkerProxy }

  TWorkerProxy = class
  private
    FClient: TBaseTransport;
    function ProcessObject(ACommand: UInt32; const ObjectName: String): LongBool;
    function ProcessObject(ACommand: UInt32; const ObjectName: String; Mode: Integer): THandle;
  public
    function FileOpen(const FileName: String; Mode: Integer): THandle; inline;
    function FileCreate(const FileName: String; Mode: Integer): THandle; inline;
    function DeleteFile(const FileName: String): LongBool; inline;
    function RenameFile(const OldName, NewName: String): LongBool;
    function CreateDirectory(const Directory: String): LongBool; inline;
    function RemoveDirectory(const Directory: String): LongBool; inline;
  public
    constructor Create;
    class function Instance: TWorkerProxy;
  end;


procedure StartMasterServer;
procedure StartWorkerServer(const AName: String);

procedure CreateWorkerProxy();
procedure CreateMasterProxy(const AName: String);

var
  MasterService: TMasterService;
  WorkerService: TWorkerService;

implementation

uses
  uOSUtils, uDebug;

const
  MasterAddress = 'doublecmd-master-';
  WorkerAddress = 'doublecmd-worker-';

var
  MasterProxy: TMasterProxy = nil;
  WorkerProxy: TWorkerProxy = nil;

procedure StartMasterServer;
var
  Address: String;
begin
  Address:= MasterAddress + IntToStr(GetProcessID);
  MasterService := TMasterService.Create(Address);
  MasterService.Start;
end;

procedure StartWorkerServer(const AName: String);
var
  Address: String;
begin
  Address:= WorkerAddress + AName;
  WorkerService := TWorkerService.Create(Address);
  WorkerService.ProcessID:= StrToDWord(AName);
  WorkerService.Start;
end;

procedure CreateMasterProxy(const AName: String);
begin
  MasterProxy:= TMasterProxy.Create(AName);
  if not MasterProxy.Execute then Halt;
end;

procedure CreateWorkerProxy();
begin

end;

{ TMasterProxy }

function TMasterProxy.Execute: LongBool;
var
  Stream: TMemoryStream;
begin
  Result:= False;
  Stream:= TMemoryStream.Create;
  try
    // Write header
    Stream.WriteDWord(UInt32(RPC_Execute));
    Stream.WriteDWord(SizeOf(SizeUInt));
    // Write process identifier
    Stream.WriteBuffer(GetProcessID, SizeOf(SizeUInt));
    // Send command
    FClient.WriteBuffer(Stream.Memory^, Stream.Size);
    // Receive command result
    FClient.ReadBuffer(Result, SizeOf(Result));
  finally
    Stream.Free;
  end;
end;

constructor TMasterProxy.Create(const AName: String);
begin
  FClient:= TPipeTransport.Create(MasterAddress + AName);
end;

class function TMasterProxy.Instance: TMasterProxy;
begin
  Result:= MasterProxy;
end;

{ TWorkerProxy }

function TWorkerProxy.ProcessObject(ACommand: UInt32; const ObjectName: String): LongBool;
var
  Stream: TMemoryStream;
begin
  Result:= False;
  Stream:= TMemoryStream.Create;
  try
    // Write header
    Stream.WriteDWord(ACommand);
    Stream.Seek(SizeOf(UInt32), soFromCurrent);
    // Write arguments
    Stream.WriteAnsiString(ObjectName);
    // Write data size
    Stream.Seek(SizeOf(UInt32), soFromBeginning);
    Stream.WriteDWord(Stream.Size - SizeOf(UInt32) * 2);
    // Send command
    FClient.WriteBuffer(Stream.Memory^, Stream.Size);
    // Receive command result
    FClient.ReadBuffer(Result, SizeOf(Result));
  finally
    Stream.Free;
  end;
end;

function TWorkerProxy.ProcessObject(ACommand: UInt32; const ObjectName: String;
  Mode: Integer): THandle;
var
  Stream: TMemoryStream;
begin
  Result:= feInvalidHandle;
  Stream:= TMemoryStream.Create;
  try
    // Write header
    Stream.WriteDWord(ACommand);
    Stream.Seek(SizeOf(UInt32), soFromCurrent);
    // Write arguments
    Stream.WriteAnsiString(ObjectName);
    Stream.WriteDWord(Mode);
    // Write data size
    Stream.Seek(SizeOf(UInt32), soFromBeginning);
    Stream.WriteDWord(Stream.Size - SizeOf(UInt32) * 2);
    // Send command
    FClient.WriteBuffer(Stream.Memory^, Stream.Size);
    // Receive command result
    FClient.ReadHandle(Result);
  finally
    Stream.Free;
  end;
end;

function TWorkerProxy.FileOpen(const FileName: String; Mode: Integer): THandle;
begin
  Result:= ProcessObject(RPC_FileOpen, FileName, Mode);
end;

function TWorkerProxy.FileCreate(const FileName: String; Mode: Integer): THandle;
begin
  Result:= ProcessObject(RPC_FileCreate, FileName, Mode);
end;

function TWorkerProxy.DeleteFile(const FileName: String): LongBool;
begin
  Result:= ProcessObject(RPC_DeleteFile, FileName);
end;

function TWorkerProxy.RenameFile(const OldName, NewName: String): LongBool;
var
  Stream: TMemoryStream;
begin
  Result:= False;
  Stream:= TMemoryStream.Create;
  try
    // Write header
    Stream.WriteDWord(RPC_RenameFile);
    Stream.Seek(SizeOf(UInt32), soFromCurrent);
    // Write arguments
    Stream.WriteAnsiString(OldName);
    Stream.WriteAnsiString(NewName);
    // Write data size
    Stream.Seek(SizeOf(UInt32), soFromBeginning);
    Stream.WriteDWord(Stream.Size - SizeOf(UInt32) * 2);
    // Send command
    FClient.WriteBuffer(Stream.Memory^, Stream.Size);
    // Receive command result
    FClient.ReadBuffer(Result, SizeOf(Result));
  finally
    Stream.Free;
  end;
end;

function TWorkerProxy.CreateDirectory(const Directory: String): LongBool;
begin
  Result:= ProcessObject(RPC_CreateDirectory, Directory);
end;

function TWorkerProxy.RemoveDirectory(const Directory: String): LongBool;
begin
  Result:= ProcessObject(RPC_RemoveDirectory, Directory);
end;

constructor TWorkerProxy.Create;
begin
  FClient:= TPipeTransport.Create(WorkerAddress + IntToStr(GetProcessID));
end;

class function TWorkerProxy.Instance: TWorkerProxy;
var
  AProxy: PPointer;
  AThread: TThread;
begin
  if GetCurrentThreadId = MainThreadID then
    AProxy:= @WorkerProxy
  else begin
    AThread:= TThread.CurrentThread;
    AProxy:= @AThread.FatalException;
  end;
  if (AProxy^ = nil) then
  begin
    AProxy^:= TWorkerProxy.Create;
  end;
  Result:= TWorkerProxy(AProxy^);
  if MasterService.ClientCount = 0 then
  begin
    ExecCmdAdmin(ParamStrU(0), ['--service', IntToStr(GetProcessID)]);
    MasterService.Wait;
    Result.FClient.Disconnect;
  end;
end;

procedure Initialize;
begin
  if ParamCount > 0 then
  begin
    if ParamStr(1) = '--service' then
    begin
      DCDebug('Start worker server');
      StartWorkerServer(ParamStr(2));
      CreateMasterProxy(ParamStr(2));
      ReadLn;
      Halt;
    end;
  end;
  StartMasterServer;
  CreateWorkerProxy;
end;

initialization
  Initialize;

end.

