unit uElevation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes, DCOSUtils,
  uClientServer, uService, uWorker, uFindEx, uFileCopyEx;

type

  { TMasterProxy }

  TMasterProxy = class
  private
    FClient: TBaseTransport;
  public
    function Execute: LongBool;
  public
    constructor Create(const AName: String);
    destructor Destroy; override;
    class function Instance: TMasterProxy;
  end;

  { TWorkerProxy }

  TWorkerProxy = class
  private
    FClient: TBaseTransport;
    procedure ReadSearchRec(Data: TMemoryStream; var SearchRec: TSearchRecEx);
    function ProcessObject(ACommand: UInt32; const ObjectName: String): LongBool;
    function ProcessObject(ACommand: UInt32; const OldName, NewName: String): LongBool;
    function ProcessObject(ACommand: UInt32; const ObjectName: String; Attr: UInt32): LongBool;
    function ProcessObject(ACommand: UInt32; const ObjectName: String; Mode: Integer): THandle;
  public
    function Terminate: Boolean;
    function FileExists(const FileName: String): LongBool; inline;
    function FileGetAttr(const FileName: String; FollowLink: LongBool): TFileAttrs; inline;
    function FileGetAttr(const FileName: String; out Attr: TFileAttributeData): LongBool;
    function FileSetAttr(const FileName: String; Attr: TFileAttrs): LongBool; inline;
    function FileSetTime(const FileName: String;
                            ModificationTime: DCBasicTypes.TFileTime;
                            CreationTime    : DCBasicTypes.TFileTime;
                            LastAccessTime  : DCBasicTypes.TFileTime): LongBool;
    function FileSetReadOnly(const FileName: String; ReadOnly: Boolean): LongBool; inline;
    function FileCopyAttr(const sSrc, sDst: String;
                          Options: TCopyAttributesOptions): TCopyAttributesOptions;
    function FileOpen(const FileName: String; Mode: Integer): THandle; inline;
    function FileCreate(const FileName: String; Mode: Integer): THandle; inline;
    function FileCopy(const Source, Target: String; Options: UInt32;
                      UpdateProgress: TFileCopyProgress; UserData: Pointer): LongBool;
    function DeleteFile(const FileName: String): LongBool; inline;
    function RenameFile(const OldName, NewName: String): LongBool; inline;
    function FindFirst(const Path: String; Flags: UInt32; out SearchRec: TSearchRecEx): Integer;
    function FindNext(var SearchRec: TSearchRecEx): Integer;
    procedure FindClose(var SearchRec: TSearchRecEx);
    function CreateHardLink(const Path, LinkName: String): LongBool; inline;
    function CreateSymbolicLink(const Path, LinkName: String): LongBool; inline;
    function CreateDirectory(const Directory: String): LongBool; inline;
    function RemoveDirectory(const Directory: String): LongBool; inline;
    function DirectoryExists(const Directory: String): LongBool; inline;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TWorkerProxy;
  end;


procedure StartMasterServer;
procedure StartWorkerServer(const AName: String);

procedure CreateWorkerProxy();
procedure CreateMasterProxy(const AName: String);

var
  MasterService: TMasterService = nil;
  WorkerService: TWorkerService = nil;

implementation

uses
  SyncObjs, LazUtf8, uSuperUser, uDebug;

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
  if not MasterProxy.Execute then WorkerService.Event.SetEvent;
end;

procedure CreateWorkerProxy;
begin
  WorkerProxy:= TWorkerProxy.Create;
end;

var
  Mutex: TRTLCriticalSection;
  WorkerProcess: UIntPtr = 0;

function WaitProcessThread({%H-}Parameter: Pointer): PtrInt;
begin
  Result:= 0;
  WaitProcess(WorkerProcess);
  WorkerProcess:= 0;
  MasterService.Event.SetEvent;
  EndThread(Result);
end;

function ElevateSelf: Boolean;
begin
  WorkerProcess:= ExecCmdAdmin(ParamStrUtf8(0), ['--service', IntToStr(GetProcessID)]);
  Result := (WorkerProcess > 0);
  if Result then BeginThread(@WaitProcessThread);
end;

{ TMasterProxy }

function TMasterProxy.Execute: LongBool;
var
  Stream: TMemoryStream;
begin
  Result:= False;
  try
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
  except
    on E: Exception do DCDebug(E.Message);
  end;
end;

constructor TMasterProxy.Create(const AName: String);
begin
  FClient:= TPipeTransport.Create(MasterAddress + AName);
end;

destructor TMasterProxy.Destroy;
begin
  inherited Destroy;
  FClient.Free;
end;

class function TMasterProxy.Instance: TMasterProxy;
begin
  Result:= MasterProxy;
end;

{ TWorkerProxy }

procedure TWorkerProxy.ReadSearchRec(Data: TMemoryStream;
  var SearchRec: TSearchRecEx);
begin
  Data.ReadBuffer((@SearchRec.PlatformTime)^, SizeOf(SearchRec.PlatformTime));
  Data.ReadBuffer((@SearchRec.LastAccessTime)^, SizeOf(SearchRec.LastAccessTime));
  Data.ReadBuffer(SearchRec.Time, SizeOf(TFileTime));
  Data.ReadBuffer(SearchRec.Size, SizeOf(Int64));
  Data.ReadBuffer(SearchRec.Attr, SizeOf(TFileAttrs));
  SearchRec.Name:= Data.ReadAnsiString;
end;

function TWorkerProxy.ProcessObject(ACommand: UInt32; const ObjectName: String): LongBool;
var
  LastError: Integer;
  Stream: TMemoryStream;
begin
  Result:= False;
  try
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
      FClient.ReadBuffer(LastError, SizeOf(LastError));
      SetLastOSError(LastError);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do DCDebug(E.Message);
  end;
end;

function TWorkerProxy.ProcessObject(ACommand: UInt32; const OldName,
  NewName: String): LongBool;
var
  LastError: Integer;
  Stream: TMemoryStream;
begin
  Result:= False;
  try
    Stream:= TMemoryStream.Create;
    try
      // Write header
      Stream.WriteDWord(ACommand);
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
      FClient.ReadBuffer(LastError, SizeOf(LastError));
      SetLastOSError(LastError);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do DCDebug(E.Message);
  end;
end;

function TWorkerProxy.ProcessObject(ACommand: UInt32; const ObjectName: String;
  Attr: UInt32): LongBool;
var
  LastError: Integer;
  Stream: TMemoryStream;
begin
  Result:= False;
  try
    Stream:= TMemoryStream.Create;
    try
      // Write header
      Stream.WriteDWord(ACommand);
      Stream.Seek(SizeOf(UInt32), soFromCurrent);
      // Write arguments
      Stream.WriteAnsiString(ObjectName);
      Stream.WriteDWord(Attr);
      // Write data size
      Stream.Seek(SizeOf(UInt32), soFromBeginning);
      Stream.WriteDWord(Stream.Size - SizeOf(UInt32) * 2);
      // Send command
      FClient.WriteBuffer(Stream.Memory^, Stream.Size);
      // Receive command result
      FClient.ReadBuffer(Result, SizeOf(Result));
      FClient.ReadBuffer(LastError, SizeOf(LastError));
      SetLastOSError(LastError);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do DCDebug(E.Message);
  end;
end;

function TWorkerProxy.ProcessObject(ACommand: UInt32; const ObjectName: String;
  Mode: Integer): THandle;
var
  Stream: TMemoryStream;
begin
  Result:= feInvalidHandle;
  try
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
  except
    on E: Exception do DCDebug(E.Message);
  end;
end;

function TWorkerProxy.Terminate: Boolean;
var
  Stream: TMemoryStream;
begin
  Result:= False;
  try
    Stream:= TMemoryStream.Create;
    try
      // Write header
      Stream.WriteDWord(RPC_Terminate);
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
  except
    on E: Exception do DCDebug(E.Message);
  end;
end;

function TWorkerProxy.FileExists(const FileName: String): LongBool;
begin
  Result:= ProcessObject(RPC_FileExists, FileName);
end;

function TWorkerProxy.FileGetAttr(const FileName: String; FollowLink: LongBool): TFileAttrs;
begin
  Result:= TFileAttrs(ProcessObject(RPC_FileGetAttr, FileName, UInt32(FollowLink)));
end;

function TWorkerProxy.FileGetAttr(const FileName: String; out
  Attr: TFileAttributeData): LongBool;
var
  LastError: Integer;
  Stream: TMemoryStream;
begin
  Result:= False;
  try
    Stream:= TMemoryStream.Create;
    try
      // Write header
      Stream.WriteDWord(RPC_FileGetAttr);
      Stream.Seek(SizeOf(UInt32), soFromCurrent);
      // Write arguments
      Stream.WriteAnsiString(FileName);
      Stream.WriteDWord(maxSmallint);
      // Write data size
      Stream.Seek(SizeOf(UInt32), soFromBeginning);
      Stream.WriteDWord(Stream.Size - SizeOf(UInt32) * 2);
      // Send command
      FClient.WriteBuffer(Stream.Memory^, Stream.Size);
      // Receive command result
      FClient.ReadBuffer(Result, SizeOf(Result));
      FClient.ReadBuffer(LastError, SizeOf(LastError));
      FClient.ReadBuffer(Attr, SizeOf(TFileAttributeData));
      SetLastOSError(LastError);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do DCDebug(E.Message);
  end;
end;

function TWorkerProxy.FileSetAttr(const FileName: String; Attr: TFileAttrs): LongBool;
begin
  Result:= ProcessObject(RPC_FileSetAttr, FileName, Attr);
end;

function TWorkerProxy.FileSetTime(const FileName: String;
  ModificationTime: DCBasicTypes.TFileTime;
  CreationTime: DCBasicTypes.TFileTime; LastAccessTime: DCBasicTypes.TFileTime
  ): LongBool;
var
  LastError: Integer;
  Stream: TMemoryStream;
begin
  Result:= False;
  try
    Stream:= TMemoryStream.Create;
    try
      // Write header
      Stream.WriteDWord(RPC_FileSetTime);
      Stream.Seek(SizeOf(UInt32), soFromCurrent);
      // Write arguments
      Stream.WriteAnsiString(FileName);
      Stream.WriteQWord(ModificationTime);
      Stream.WriteQWord(CreationTime);
      Stream.WriteQWord(LastAccessTime);
      // Write data size
      Stream.Seek(SizeOf(UInt32), soFromBeginning);
      Stream.WriteDWord(Stream.Size - SizeOf(UInt32) * 2);
      // Send command
      FClient.WriteBuffer(Stream.Memory^, Stream.Size);
      // Receive command result
      FClient.ReadBuffer(Result, SizeOf(Result));
      FClient.ReadBuffer(LastError, SizeOf(LastError));
      SetLastOSError(LastError);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do DCDebug(E.Message);
  end;
end;

function TWorkerProxy.FileSetReadOnly(const FileName: String; ReadOnly: Boolean): LongBool;
begin
  Result:= ProcessObject(RPC_FileSetReadOnly, FileName, UInt32(ReadOnly));
end;

function TWorkerProxy.FileCopyAttr(const sSrc, sDst: String;
  Options: TCopyAttributesOptions): TCopyAttributesOptions;
var
  LastError: Integer;
  Stream: TMemoryStream;
begin
  Result:= Options;
  try
    Stream:= TMemoryStream.Create;
    try
      // Write header
      Stream.WriteDWord(RPC_FileCopyAttr);
      Stream.Seek(SizeOf(UInt32), soFromCurrent);
      // Write arguments
      Stream.WriteAnsiString(sSrc);
      Stream.WriteAnsiString(sDst);
      Stream.WriteDWord(UInt32(Options));
      // Write data size
      Stream.Seek(SizeOf(UInt32), soFromBeginning);
      Stream.WriteDWord(Stream.Size - SizeOf(UInt32) * 2);
      // Send command
      FClient.WriteBuffer(Stream.Memory^, Stream.Size);
      // Receive command result
      FClient.ReadBuffer(Result, SizeOf(Result));
      FClient.ReadBuffer(LastError, SizeOf(LastError));
      SetLastOSError(LastError);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do DCDebug(E.Message);
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

function TWorkerProxy.FileCopy(const Source, Target: String; Options: UInt32;
  UpdateProgress: TFileCopyProgress; UserData: Pointer): LongBool;
var
  LastError: Integer;
  Stream: TMemoryStream;
  TotalBytes, DoneBytes: Int64;
begin
  Result:= False;
  try
    Stream:= TMemoryStream.Create;
    try
      // Write header
      Stream.WriteDWord(RPC_FileCopy);
      Stream.Seek(SizeOf(UInt32), soFromCurrent);
      // Write arguments
      Stream.WriteAnsiString(Source);
      Stream.WriteAnsiString(Target);
      Stream.WriteBuffer(Options, SizeOf(Options));
      // Write data size
      Stream.Seek(SizeOf(UInt32), soFromBeginning);
      Stream.WriteDWord(Stream.Size - SizeOf(UInt32) * 2);
      // Send command
      FClient.WriteBuffer(Stream.Memory^, Stream.Size);
      repeat
        FClient.ReadBuffer(LastError, SizeOf(LastError));
        // Receive progress info
        if (LastError = 1) then
        begin
          FClient.ReadBuffer(TotalBytes, SizeOf(TotalBytes));
          FClient.ReadBuffer(DoneBytes, SizeOf(DoneBytes));
          Result:= UpdateProgress(TotalBytes, DoneBytes, UserData);
          FClient.WriteBuffer(Result, SizeOf(Result));
        end;
      until (LastError <> 1);
      // Receive command result
      FClient.ReadBuffer(Result, SizeOf(Result));
      FClient.ReadBuffer(LastError, SizeOf(LastError));
      SetLastOSError(LastError);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do DCDebug(E.Message);
  end;
end;

function TWorkerProxy.DeleteFile(const FileName: String): LongBool;
begin
  Result:= ProcessObject(RPC_DeleteFile, FileName);
end;

function TWorkerProxy.RenameFile(const OldName, NewName: String): LongBool;
begin
  Result:= ProcessObject(RPC_RenameFile, OldName, NewName);
end;

function TWorkerProxy.FindFirst(const Path: String; Flags: UInt32; out
  SearchRec: TSearchRecEx): Integer;
var
  ASize: UInt32 = 0;
  Stream: TMemoryStream;
  Data: TMemoryStream absolute SearchRec.FindHandle;
begin
  Result:= -1;
  try
    Stream:= TMemoryStream.Create;
    try
      // Write header
      Stream.WriteDWord(RPC_FindFirst);
      Stream.Seek(SizeOf(UInt32), soFromCurrent);
      // Write arguments
      Stream.WriteAnsiString(Path);
      Stream.WriteDWord(Flags);
      // Write data size
      Stream.Seek(SizeOf(UInt32), soFromBeginning);
      Stream.WriteDWord(Stream.Size - SizeOf(UInt32) * 2);
      // Send command
      FClient.WriteBuffer(Stream.Memory^, Stream.Size);
      // Receive command result
      FClient.ReadBuffer(Result, SizeOf(Result));
      FClient.ReadBuffer(ASize, SizeOf(ASize));
      if ASize > 0 then
      begin
        Data:= TMemoryStream.Create;
        Data.Size:= ASize;
        FClient.ReadBuffer(Data.Memory^, ASize);
        Data.Seek(SizeOf(Pointer), soBeginning);
        ReadSearchRec(Data, SearchRec);
      end;
    finally
      Stream.Free;
    end;
  except
    on E: Exception do DCDebug(E.Message);
  end;
end;

function TWorkerProxy.FindNext(var SearchRec: TSearchRecEx): Integer;
var
  ASize: UInt32 = 0;
  Stream: TMemoryStream;
  Data: TMemoryStream absolute SearchRec.FindHandle;
begin
  Result:= -1;
  try
    if Data.Position < Data.Size then
    begin
      Result:= 0;
      ReadSearchRec(Data, SearchRec);
    end
    else begin
      Stream:= TMemoryStream.Create;
      try
        // Write header
        Stream.WriteDWord(RPC_FindNext);
        Stream.WriteDWord(SizeOf(Pointer));
        // Write arguments
        Stream.WriteBuffer(Data.Memory^, SizeOf(Pointer));
        // Send command
        FClient.WriteBuffer(Stream.Memory^, Stream.Size);
        // Receive command result
        FClient.ReadBuffer(Result, SizeOf(Result));
        FClient.ReadBuffer(ASize, SizeOf(ASize));
        if ASize > 0 then
        begin
          Data.Size:= ASize;
          FClient.ReadBuffer(Data.Memory^, ASize);
          Data.Seek(SizeOf(Pointer), soBeginning);
          ReadSearchRec(Data, SearchRec);
        end;
      finally
        Stream.Free;
      end;
    end;
  except
    on E: Exception do DCDebug(E.Message);
  end;
end;

procedure TWorkerProxy.FindClose(var SearchRec: TSearchRecEx);
var
  Stream: TMemoryStream;
  Data: TMemoryStream absolute SearchRec.FindHandle;
begin
  try
    Stream:= TMemoryStream.Create;
    try
      // Write header
      Stream.WriteDWord(RPC_FindClose);
      Stream.WriteDWord(SizeOf(Pointer));
      // Write arguments
      Stream.WriteBuffer(Data.Memory^, SizeOf(Pointer));
      // Send command
      FClient.WriteBuffer(Stream.Memory^, Stream.Size);
    finally
      Data.Free;
      Stream.Free;
    end;
  except
    on E: Exception do DCDebug(E.Message);
  end;
end;

function TWorkerProxy.CreateHardLink(const Path, LinkName: String): LongBool;
begin
  Result:= ProcessObject(RPC_CreateHardLink, Path, LinkName);
end;

function TWorkerProxy.CreateSymbolicLink(const Path, LinkName: String): LongBool;
begin
  Result:= ProcessObject(RPC_CreateSymbolicLink, Path, LinkName);
end;

function TWorkerProxy.CreateDirectory(const Directory: String): LongBool;
begin
  Result:= ProcessObject(RPC_CreateDirectory, Directory);
end;

function TWorkerProxy.RemoveDirectory(const Directory: String): LongBool;
begin
  Result:= ProcessObject(RPC_RemoveDirectory, Directory);
end;

function TWorkerProxy.DirectoryExists(const Directory: String): LongBool;
begin
  Result:= ProcessObject(RPC_DirectoryExists, Directory);
end;

constructor TWorkerProxy.Create;
begin
  FClient:= TPipeTransport.Create(WorkerAddress + IntToStr(GetProcessID));
end;

destructor TWorkerProxy.Destroy;
begin
  DCDebug('TWorkerProxy.Destroy');
  inherited Destroy;
  FClient.Free;
end;

class function TWorkerProxy.Instance: TWorkerProxy;
var
  AProxy: PPointer;
  AThread: TThread;
begin
  if GetCurrentThreadId = MainThreadID then
    Result:= WorkerProxy
  else begin
    AThread:= TThread.CurrentThread;
    AProxy:= @AThread.FatalException;
    if (AProxy^ = nil) then
    begin
      AProxy^:= TWorkerProxy.Create;
    end;
    Result:= TWorkerProxy(AProxy^);
  end;

  EnterCriticalSection(Mutex);
  try
    if MasterService.ClientCount = 0 then
    begin
      MasterService.Event.ResetEvent;
      if ElevateSelf then begin
        MasterService.Event.WaitFor(60000);
      end;
      Result.FClient.Disconnect;
    end;
  finally
    LeaveCriticalSection(Mutex);
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
      WorkerService.Event.WaitFor(INFINITE);
      WorkerService.Free;
      Halt;
    end;
  end;
  if not AdministratorPrivileges then
  begin
    InitCriticalSection(Mutex);
    StartMasterServer;
    CreateWorkerProxy;
  end;
end;

procedure Finalize;
begin
  if WorkerProcess > 0 then begin
    WorkerProxy.Terminate;
  end;
  if Assigned(MasterService) then
  begin
    DoneCriticalSection(Mutex);
    MasterService.Free;
  end;
end;

initialization
  Initialize;

finalization
  Finalize;

end.

