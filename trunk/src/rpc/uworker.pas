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
  public
    constructor Create(const AName: String); override;
    procedure ProcessRequest(ATransport: TBaseTransport; ACommand: Int32; ARequest: TStream); override;
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
  RPC_FileSetReadOnly = 14;
  RPC_FileCopyAttr = 15;

  RPC_FileCopy = 19;

  RPC_FindFirst = 16;
  RPC_FindNext = 17;
  RPC_FindClose = 18;

  RPC_CreateHardLink = 8;
  RPC_CreateSymbolicLink = 7;

  RPC_CreateDirectory = 5;
  RPC_RemoveDirectory = 6;
  RPC_DirectoryExists = 13;

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
  DCBasicTypes, DCOSUtils, uFindEx, uDebug, uFileCopyEx;

function FileCopyProgress(TotalBytes, DoneBytes: Int64; UserData: Pointer): LongBool;
var
  Code: UInt32 = 1;
  ATransport: TBaseTransport absolute UserData;
begin
  ATransport.WriteBuffer(Code, SizeOf(UInt32));
  ATransport.WriteBuffer(TotalBytes, SizeOf(TotalBytes));
  ATransport.WriteBuffer(DoneBytes, SizeOf(DoneBytes));
  ATransport.ReadBuffer(Result, SizeOf(Result));
end;

{ TMasterService }

constructor TMasterService.Create(const AName: String);
begin
  inherited Create(AName);
  Self.FVerifyChild:= True;
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
const
  FIND_MAX = 512;
var
  Mode: Integer;
  Index: Integer;
  Handle: THandle;
  Options: UInt32;
  NewName: String;
  FileName: String;
  Result: LongBool;
  Attr: TFileAttrs;
  LastError: Integer;
  Data: TMemoryStream;
  SearchRec: PSearchRecEx;
  CreationTime: TFileTime;
  LastAccessTime: TFileTime;
  ModificationTime: TFileTime;
  FileAttr: TFileAttributeData;

  procedure WriteSearchRec(Data: TMemoryStream; SearchRec: PSearchRecEx);
  begin
    Data.WriteBuffer(SearchRec^.PlatformTime, SizeOf(SearchRec^.PlatformTime));
    Data.WriteBuffer(SearchRec^.LastAccessTime, SizeOf(SearchRec^.LastAccessTime));
    Data.WriteBuffer(SearchRec^.Time, SizeOf(TFileTime));
    Data.WriteBuffer(SearchRec^.Size, SizeOf(Int64));
    Data.WriteBuffer(SearchRec^.Attr, SizeOf(TFileAttrs));
    Data.WriteAnsiString(SearchRec^.Name);
  end;

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
      Mode:= ARequest.ReadDWord;
      DCDebug('FileGetAttr ', FileName);
      case Mode of
        Ord(LongBool(False)):
          Result:= LongBool(mbFileGetAttr(FileName));
        Ord(LongBool(True)):
          Result:= LongBool(mbFileGetAttrNoLinks(FileName));
        maxSmallint:
          Result:= LongBool(mbFileGetAttr(FileName, FileAttr));
      end;
      LastError:= GetLastOSError;
      ATransport.WriteBuffer(Result, SizeOf(Result));
      ATransport.WriteBuffer(LastError, SizeOf(LastError));
      if (Mode = maxSmallint) then ATransport.WriteBuffer(FileAttr, SizeOf(FileAttr));
    end;
  RPC_FileSetAttr:
    begin
      FileName:= ARequest.ReadAnsiString;
      Attr:= ARequest.ReadDWord;
      DCDebug('FileSetAttr ', FileName);
      Result:= mbFileSetAttr(FileName, Attr);
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
  RPC_FileSetReadOnly:
    begin
      FileName:= ARequest.ReadAnsiString;
      Attr:= ARequest.ReadDWord;
      DCDebug('FileSetReadOnly ', FileName);
      Result:= mbFileSetReadOnly(FileName, Boolean(Attr));
      LastError:= GetLastOSError;
      ATransport.WriteBuffer(Result, SizeOf(Result));
      ATransport.WriteBuffer(LastError, SizeOf(LastError));
    end;
  RPC_FileCopyAttr:
    begin
      FileName:= ARequest.ReadAnsiString;
      NewName:= ARequest.ReadAnsiString;
      Attr:= ARequest.ReadDWord;
      DCDebug('FileCopyAttr ', NewName);
      Result:= LongBool(mbFileCopyAttr(FileName, NewName, TCopyAttributesOptions(Attr)));
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
  RPC_FindFirst:
  begin
    Index:= 0;
    FileName:= ARequest.ReadAnsiString;
    Mode:= ARequest.ReadDWord;
    New(SearchRec);
    LastError:= FindFirstEx(FileName, Mode, SearchRec^);
    if LastError = 0 then
    begin
      Data:= TMemoryStream.Create;
      Data.WriteBuffer(SearchRec, SizeOf(SearchRec));
      repeat
        Inc(Index);
        WriteSearchRec(Data, SearchRec);
      until not ((Index < FIND_MAX) and (FindNextEx(SearchRec^) = 0));
      Index:= Data.Size;
    end;
    ATransport.WriteBuffer(LastError, SizeOf(LastError));
    ATransport.WriteBuffer(Index, SizeOf(Index));
    if Index > 0 then begin
      ATransport.WriteBuffer(Data.Memory^, Index);
      Data.Free;
    end;
  end;
  RPC_FindNext:
  begin
    Index:= 0;
    ARequest.ReadBuffer(SearchRec, SizeOf(SearchRec));
    LastError:= FindNextEx(SearchRec^);
    if LastError = 0 then
    begin
      Data:= TMemoryStream.Create;
      Data.WriteBuffer(SearchRec, SizeOf(SearchRec));
      repeat
        Inc(Index);
        WriteSearchRec(Data, SearchRec);
      until not ((Index < FIND_MAX) and (FindNextEx(SearchRec^) = 0));
      Index:= Data.Size;
    end;
    ATransport.WriteBuffer(LastError, SizeOf(LastError));
    ATransport.WriteBuffer(Index, SizeOf(Index));
    if Index > 0 then begin
      ATransport.WriteBuffer(Data.Memory^, Index);
      Data.Free;
    end;
  end;
  RPC_FindClose:
  begin
    ARequest.ReadBuffer(SearchRec, SizeOf(SearchRec));
    FindCloseEx(SearchRec^);
    Dispose(SearchRec);
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
  RPC_DirectoryExists:
  begin
    FileName:= ARequest.ReadAnsiString;
    DCDebug('DirectoryExists ', FileName);
    Result:= mbDirectoryExists(FileName);
    LastError:= GetLastOSError;
    ATransport.WriteBuffer(Result, SizeOf(Result));
    ATransport.WriteBuffer(LastError, SizeOf(LastError));
  end;
  RPC_FileCopy:
  begin
    FileName:= ARequest.ReadAnsiString;
    NewName:= ARequest.ReadAnsiString;
    Options:= ARequest.ReadDWord;
    DCDebug('FileCopy ', FileName);
    Result:= FileCopyEx(FileName, NewName, Options, @FileCopyProgress, ATransport);
    LastError:= GetLastOSError;
    Index:= 0;
    ATransport.WriteBuffer(Index, SizeOf(Index));
    ATransport.WriteBuffer(Result, SizeOf(Result));
    ATransport.WriteBuffer(LastError, SizeOf(LastError));
  end;
  RPC_Terminate: FEvent.SetEvent;
  end;
end;

end.

