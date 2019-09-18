unit uWorker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, uService;

const
  RPC_Execute     = 1;

type

  { TMasterService }

  TMasterService = class(TBaseService)
  private
    FEvent: TEvent;
  public
    constructor Create(const AName: String); override;
    procedure ProcessRequest(ATransport: TBaseTransport; ACommand: Int32; ARequest: TStream); override;
    property Event: TEvent read FEvent;
  end;

const
  RPC_Terminate  = 0;
  RPC_FileOpen   = 1;
  RPC_FileCreate = 2;
  RPC_DeleteFile = 3;
  RPC_RenameFile = 4;
  RPC_FileExists = 9;
  RPC_FileGetAttr = 10;
  RPC_FileSetAttr = 11;
  RPC_FileSetTime = 12;

  RPC_CreateHardLink = 8;
  RPC_CreateSymbolicLink = 7;

  RPC_CreateDirectory = 5;
  RPC_RemoveDirectory = 6;

type

  { TWorkerService }

  TWorkerService = class(TBaseService)
  public
    constructor Create(const AName: String); override;
    procedure ProcessRequest(ATransport: TBaseTransport; ACommand: Int32; ARequest: TStream); override;
  end;

var
  WorkerProcessId: SizeUInt = 0;

implementation

uses
  DCBasicTypes, DCOSUtils, uDebug;

{ TMasterService }

constructor TMasterService.Create(const AName: String);
begin
  inherited Create(AName);
  Self.FVerifyChild:= True;
  FEvent:= TEvent.Create(nil, False, False, '');
end;

procedure TMasterService.ProcessRequest(ATransport: TBaseTransport; ACommand: Int32;
  ARequest: TStream);
var
  Result: LongBool = True;
begin
  case ACommand of
    RPC_Execute:
      begin
        ARequest.ReadBuffer(WorkerProcessId, SizeOf(SizeUInt));
        ATransport.WriteBuffer(Result, SizeOf(Result));
        FEvent.SetEvent;
      end;
  end;
end;

{ TWorkerService }

constructor TWorkerService.Create(const AName: String);
begin
  inherited Create(AName);
  Self.FVerifyParent:= True;
end;

procedure TWorkerService.ProcessRequest(ATransport: TBaseTransport; ACommand: Int32;
  ARequest: TStream);
var
  Mode: Integer;
  Handle: THandle;
  NewName: String;
  FileName: String;
  Result: LongBool;
  Attr: TFileAttrs;
  LastError: Integer;
  CreationTime: TFileTime;
  LastAccessTime: TFileTime;
  ModificationTime: TFileTime;
begin
  case ACommand of
  RPC_DeleteFile:
    begin
      FileName:= ARequest.ReadAnsiString;
      DCDebug('DeleteFile ', FileName);
      Result:= mbDeleteFile(FileName);
      LastError:= GetLastOSError;
      ATransport.WriteBuffer(Result, SizeOf(Result));
      ATransport.WriteBuffer(LastError, SizeOf(LastError));
    end;
  RPC_FileExists:
    begin
      FileName:= ARequest.ReadAnsiString;
      DCDebug('FileExists ', FileName);
      Result:= mbFileExists(FileName);
      LastError:= GetLastOSError;
      ATransport.WriteBuffer(Result, SizeOf(Result));
      ATransport.WriteBuffer(LastError, SizeOf(LastError));
    end;
  RPC_FileGetAttr:
    begin
      FileName:= ARequest.ReadAnsiString;
      DCDebug('FileGetAttr ', FileName);
      Result:= LongBool(mbFileGetAttr(FileName));
      LastError:= GetLastOSError;
      ATransport.WriteBuffer(Result, SizeOf(Result));
      ATransport.WriteBuffer(LastError, SizeOf(LastError));
    end;
  RPC_FileSetAttr:
    begin
      FileName:= ARequest.ReadAnsiString;
      Attr:= ARequest.ReadDWord;
      DCDebug('FileSetAttr ', FileName);
      Result:= LongBool(mbFileSetAttr(FileName, Attr));
      LastError:= GetLastOSError;
      ATransport.WriteBuffer(Result, SizeOf(Result));
      ATransport.WriteBuffer(LastError, SizeOf(LastError));
    end;
  RPC_FileSetTime:
    begin
      FileName:= ARequest.ReadAnsiString;
      ModificationTime:= ARequest.ReadQWord;
      CreationTime:= ARequest.ReadQWord;
      LastAccessTime:= ARequest.ReadQWord;
      DCDebug('FileSetTime ', FileName);
      Result:= mbFileSetTime(FileName, ModificationTime, CreationTime, LastAccessTime);
      LastError:= GetLastOSError;
      ATransport.WriteBuffer(Result, SizeOf(Result));
      ATransport.WriteBuffer(LastError, SizeOf(LastError));
    end;
  RPC_FileOpen:
    begin
      FileName:= ARequest.ReadAnsiString;
      Mode:= ARequest.ReadDWord;
      DCDebug('FileOpen ', FileName);
      Handle:= mbFileOpen(FileName, Mode);
      ATransport.WriteHandle(Handle);
    end;
  RPC_FileCreate:
    begin
      FileName:= ARequest.ReadAnsiString;
      Mode:= ARequest.ReadDWord;
      DCDebug('FileCreate ', FileName);
      Handle:= mbFileCreate(FileName, Mode);
      ATransport.WriteHandle(Handle);
    end;
  RPC_RenameFile:
    begin
      FileName:= ARequest.ReadAnsiString;
      NewName:= ARequest.ReadAnsiString;
      DCDebug('RenameFile ', FileName);
      Result:= mbRenameFile(FileName, NewName);
      LastError:= GetLastOSError;
      ATransport.WriteBuffer(Result, SizeOf(Result));
      ATransport.WriteBuffer(LastError, SizeOf(LastError));
    end;
  RPC_CreateHardLink:
    begin
      FileName:= ARequest.ReadAnsiString;
      NewName:= ARequest.ReadAnsiString;
      DCDebug('CreateHardLink ', NewName);
      Result:= CreateHardLink(FileName, NewName);
      LastError:= GetLastOSError;
      ATransport.WriteBuffer(Result, SizeOf(Result));
      ATransport.WriteBuffer(LastError, SizeOf(LastError));
    end;
  RPC_CreateSymbolicLink:
    begin
      FileName:= ARequest.ReadAnsiString;
      NewName:= ARequest.ReadAnsiString;
      DCDebug('CreateSymbolicLink ', NewName);
      Result:= CreateSymLink(FileName, NewName);
      LastError:= GetLastOSError;
      ATransport.WriteBuffer(Result, SizeOf(Result));
      ATransport.WriteBuffer(LastError, SizeOf(LastError));
    end;
  RPC_CreateDirectory:
    begin
      FileName:= ARequest.ReadAnsiString;
      DCDebug('CreateDirectory ', FileName);
      Result:= mbCreateDir(FileName);
      LastError:= GetLastOSError;
      ATransport.WriteBuffer(Result, SizeOf(Result));
      ATransport.WriteBuffer(LastError, SizeOf(LastError));
    end;
  RPC_RemoveDirectory:
    begin
      FileName:= ARequest.ReadAnsiString;
      DCDebug('RemoveDirectory ', FileName);
      Result:= mbRemoveDir(FileName);
      LastError:= GetLastOSError;
      ATransport.WriteBuffer(Result, SizeOf(Result));
      ATransport.WriteBuffer(LastError, SizeOf(LastError));
    end;
  RPC_Terminate: Halt;
  end;
end;

end.

