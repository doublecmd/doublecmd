unit uFileUnlock;

{$mode delphi}{$R-}

interface

uses
  Classes, SysUtils;

type
  TProcessInfo = record
    ProcessId: DWORD;
    FileHandle: THandle;
    ApplicationName: String;
    ExecutablePath: String;
  end;

  TProcessInfoArray = array of TProcessInfo;

function TerminateProcess(ProcessId: DWORD): Boolean;
function FileUnlock(ProcessId: DWORD; hFile: THandle): Boolean;
function GetFileInUseProcessFast(const FileName: String; out ProcessInfo: TProcessInfoArray): Boolean;
function GetFileInUseProcessSlow(const FileName: String; LastError: Integer; var ProcessInfo: TProcessInfoArray): Boolean;

implementation

uses
  JwaWinType, JwaNative, JwaNtStatus, JwaPsApi, Windows, DCWindows;

const
  RstrtMgr = 'RstrtMgr.dll';

const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;

  CCH_RM_MAX_SVC_NAME = 63;
  CCH_RM_MAX_APP_NAME = 255;
  RM_SESSION_KEY_LEN = SizeOf(TGUID);
  CCH_RM_SESSION_KEY = RM_SESSION_KEY_LEN * 2;

type
  TRMAppType = (
    RmUnknownApp  = 0,
    RmMainWindow  = 1,
    RmOtherWindow = 2,
    RmService     = 3,
    RmExplorer    = 4,
    RmConsole     = 5,
    RmCritical    = 1000
  );

  PRMUniqueProcess = ^TRMUniqueProcess;
  TRMUniqueProcess = record
    dwProcessId: DWORD;
    ProcessStartTime: TFileTime;
  end;

  PRMProcessInfo = ^TRMProcessInfo;
  TRMProcessInfo = record
    Process: TRMUniqueProcess;
    strAppName: array[0..CCH_RM_MAX_APP_NAME] of WideChar;
    strServiceShortName: array[0..CCH_RM_MAX_SVC_NAME] of WideChar;
    ApplicationType: TRMAppType;
    AppStatus: ULONG;
    TSSessionId: DWORD;
    bRestartable: BOOL;
  end;

  PSystemHandleInformationFx = ^TSystemHandleInformationFx;
  TSystemHandleInformationFx = record
    Count: ULONG;
    Handle: array[0..0] of TSystemHandleInformation;
  end;

  TSystemHandleTableEntryInfoEx = record
    Object_: PVOID;
    ProcessId: ULONG_PTR;
    Handle: ULONG_PTR;
    GrantedAccess: ULONG;
    CreatorBackTraceIndex: USHORT;
    ObjectTypeNumber: USHORT;
    HandleAttributes: ULONG;
    Reserved: ULONG;
  end;

  PSystemHandleInformationEx = ^TSystemHandleInformationEx;
  TSystemHandleInformationEx = record
    Count: ULONG_PTR;
    Reserved: ULONG_PTR;
    Handle: array[0..0] of TSystemHandleTableEntryInfoEx;
  end;

var
  RmStartSession: function (pSessionHandle: LPDWORD; dwSessionFlags: DWORD; strSessionKey: LPWSTR): DWORD; stdcall;
  RmEndSession: function (dwSessionHandle: DWORD): DWORD; stdcall;

  RmRegisterResources: function(dwSessionHandle: DWORD; nFiles: UINT; rgsFileNames: LPPWSTR; nApplications: UINT; rgApplications: PRMUniqueProcess; nServices: UINT; rgsServiceNames: LPPWSTR): DWORD; stdcall;
  RmGetList: function(dwSessionHandle: DWORD; pnProcInfoNeeded: PUINT; pnProcInfo: PUINT; rgAffectedApps: PRMProcessInfo; lpdwRebootReasons: LPDWORD): DWORD; stdcall;

  QueryFullProcessImageNameW: function(hProcess: HANDLE; dwFlags: DWORD; lpExeName: LPWSTR; lpdwSize: PDWORD): BOOL; stdcall;

  GetFinalPathNameByHandleW: function(hFile: HANDLE; lpszFilePath: LPWSTR; cchFilePath: DWORD; dwFlags: DWORD): DWORD; stdcall;

  NtQueryObject: function(ObjectHandle : HANDLE; ObjectInformationClass : OBJECT_INFORMATION_CLASS; ObjectInformation : PVOID; ObjectInformationLength : ULONG; ReturnLength : PULONG): NTSTATUS; stdcall;

var
  RstrtMgrLib: HMODULE = 0;
  GetFileName: function(hFile: HANDLE): UnicodeString;

function _wcsnicmp(const s1, s2: pwidechar; count: ptruint): integer; cdecl; external 'msvcrt.dll';

function GetFileHandleList(out SystemInformation : PSystemHandleInformationEx): Boolean;
const
  MEM_SIZE = SizeOf(TSystemHandleInformationEx);
var
  Index: Integer;
  Status: NTSTATUS;
  SystemInformationLength : ULONG = MEM_SIZE;
  SystemInformationOld : PSystemHandleInformationFx;
begin
  if CheckWin32Version(5, 1) then
  begin
    SystemInformation:= GetMem(MEM_SIZE);
    repeat
      Status:= NtQuerySystemInformation(TSystemInformationClass(64), SystemInformation,
                                        SystemInformationLength, @SystemInformationLength);
      if Status = STATUS_INFO_LENGTH_MISMATCH then
      begin
        SystemInformationLength+= SizeOf(TSystemHandleTableEntryInfoEx) * 100;
        ReAllocMem(SystemInformation, SystemInformationLength);
      end;
    until Status <> STATUS_INFO_LENGTH_MISMATCH;
    Result:= (Status = STATUS_SUCCESS);
    if not Result then FreeMem(SystemInformation);
  end
  else begin
    SystemInformationOld:= GetMem(MEM_SIZE);
    repeat
      Status:= NtQuerySystemInformation(SystemHandleInformation, SystemInformationOld,
                                        SystemInformationLength, @SystemInformationLength);
      if Status = STATUS_INFO_LENGTH_MISMATCH then
      begin
        SystemInformationLength+= SizeOf(TSystemHandleInformation) * 100;
        ReAllocMem(SystemInformationOld, SystemInformationLength);
      end;
    until Status <> STATUS_INFO_LENGTH_MISMATCH;
    Result:= (Status = STATUS_SUCCESS);
    if Result then
    begin
      SystemInformation:= GetMem(SystemInformationOld.Count *
                                 SizeOf(TSystemHandleTableEntryInfoEx) +
                                 SizeOf(TSystemHandleInformationEx));
      for Index := 0 to SystemInformationOld.Count - 1 do
      begin
        with SystemInformation.Handle[Index] do
        begin
          Handle:= SystemInformationOld.Handle[Index].Handle;
          Object_:= SystemInformationOld.Handle[Index].Object_;
          ProcessId:= SystemInformationOld.Handle[Index].ProcessId;
          GrantedAccess:= SystemInformationOld.Handle[Index].GrantedAccess;
          ObjectTypeNumber:= SystemInformationOld.Handle[Index].ObjectTypeNumber;
        end;
      end;
      SystemInformation.Count:= SystemInformationOld.Count;
    end;
    FreeMem(SystemInformationOld);
  end;
end;

function GetFileNameOld(hFile: HANDLE): UnicodeString;
const
  MAX_SIZE = SizeOf(TObjectNameInformation) + MAXWORD;
var
  ReturnLength : ULONG;
  ObjectInformation : PObjectNameInformation;
begin
  ObjectInformation:= GetMem(MAX_SIZE);
  if (NtQueryObject(hFile, ObjectNameInformation, ObjectInformation, MAXWORD, @ReturnLength) <> STATUS_SUCCESS) then
    Result:= EmptyWideStr
  else begin
    SetLength(Result, ObjectInformation^.Name.Length div SizeOf(WideChar));
    Move(ObjectInformation^.Name.Buffer^, Result[1], ObjectInformation^.Name.Length);
  end;
  FreeMem(ObjectInformation);
end;

function GetFileNameNew(hFile: HANDLE): UnicodeString;
begin
  SetLength(Result, maxSmallint + 1);
  SetLength(Result, GetFinalPathNameByHandleW(hFile, PWideChar(Result), maxSmallint, 0));
end;

var
  FileHandleType: ULONG;

function GetFileHandleType: ULONG;
var
  Index: DWORD;
  Handle: THandle;
  ProcessId: DWORD;
  SystemInformation : PSystemHandleInformationEx;
begin
  Handle:= FileOpen('NUL', fmOpenRead or fmShareDenyNone);
  if Handle <> feInvalidHandle then
  begin
    if GetFileHandleList(SystemInformation) then
    begin
      ProcessId:= GetCurrentProcessId;
      for Index:= 0 to SystemInformation^.Count - 1 do
      begin
        if (SystemInformation^.Handle[Index].Handle = Handle) and (SystemInformation^.Handle[Index].ProcessId = ProcessId) then
        begin
          Result:= SystemInformation^.Handle[Index].ObjectTypeNumber;
          Break;
        end;
      end;
      FreeMem(SystemInformation);
    end;
    FileClose(Handle);
  end;
end;

function GetProcessFileName(hProcess: HANDLE): UnicodeString;
var
  dwSize: DWORD;
begin
  if (Win32MajorVersion < 6) then
  begin
    SetLength(Result, maxSmallint + 1);
    SetLength(Result, GetModuleFileNameExW(hProcess, 0, PWideChar(Result), maxSmallint));
  end
  else begin
    dwSize:= maxSmallint;
    SetLength(Result, dwSize + 1);
    if QueryFullProcessImageNameW(hProcess, 0, PWideChar(Result), @dwSize) then
    begin
      SetLength(Result, dwSize);
    end;
  end;
end;

function GetModuleFileName(hProcess, hModule: HANDLE): UnicodeString;
begin
  SetLength(Result, maxSmallint + 1);
  SetLength(Result, GetModuleFileNameExW(hProcess, hModule, PWideChar(Result), maxSmallint));
end;

function GetNativeName(const FileName: String; out NativeName: UnicodeString): Boolean;
var
  hFile: HANDLE;
begin
  hFile := CreateFileW(PWideChar(UTF16LongName(FileName)), FILE_READ_ATTRIBUTES,
                       FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                       nil, OPEN_EXISTING, 0, 0);
  Result:= (hFile <> INVALID_HANDLE_VALUE);
  if Result then begin
    NativeName:= GetFileName(hFile);
    CloseHandle(hFile);
  end;
end;

function CheckHandleType(hFile: HANDLE): Boolean;
var
  hFileMap: HANDLE;
begin
  hFileMap:= CreateFileMappingW(hFile, nil, PAGE_READONLY, 0, 1, nil);
  Result:= (hFileMap <> 0);
  if Result then
    CloseHandle(hFileMap)
  else begin
    Result:= (GetLastError <> ERROR_BAD_EXE_FORMAT);
  end;
end;

procedure AddLock(var ProcessInfo: TProcessInfoArray; ProcessId: DWORD; Process, FileHandle: HANDLE);
var
  Index: Integer;
begin
  for Index:= 0 to High(ProcessInfo) do
  begin
    if (ProcessInfo[Index].ProcessId = ProcessId) then
    begin
      if (ProcessInfo[Index].FileHandle = 0) and (FileHandle <> 0) then
      begin
        ProcessInfo[Index].FileHandle:= FileHandle;
        Exit;
      end;
    end;
  end;
  Index:= Length(ProcessInfo);
  SetLength(ProcessInfo, Index + 1);
  ProcessInfo[Index].ProcessId:= ProcessId;
  ProcessInfo[Index].FileHandle:= FileHandle;
  ProcessInfo[Index].ExecutablePath:= UTF8Encode(GetProcessFileName(Process));
end;

procedure GetModuleInUseProcess(const FileName: String; var ProcessInfo: TProcessInfoArray);
var
  I, J: Integer;
  hProcess: HANDLE;
  cbNeeded: DWORD = 0;
  AFileName, AOpenName: UnicodeString;
  dwProcessList: array[0..4095] of DWORD;
  hModuleList: array [0..4095] of HMODULE;
begin
  if EnumProcesses(@dwProcessList[0], SizeOf(dwProcessList), cbNeeded) then
  begin
    AFileName:= UTF8Decode(FileName);
    for I:= 0 to (cbNeeded div SizeOf(DWORD)) do
    begin
      hProcess:= OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, dwProcessList[I]);
      if (hProcess <> 0) then
      begin
        if EnumProcessModules(hProcess, @hModuleList[0], SizeOf(hModuleList), cbNeeded) then
        begin
          for J:= 0 to (cbNeeded div SizeOf(HMODULE)) do
          begin
            AOpenName:= GetModuleFileName(hProcess, hModuleList[J]);
            if (Length(AOpenName) = Length(AFileName)) then
            begin
              if (_wcsnicmp(PWideChar(AOpenName), PWideChar(AFileName), Length(AFileName)) = 0) then
              begin
                AddLock(ProcessInfo, dwProcessList[I], hProcess, 0);
                Break;
              end;
            end;
          end;
        end;
        CloseHandle(hProcess);
      end;
    end;
  end;
end;

procedure GetFileInUseProcess(const FileName: String; var ProcessInfo: TProcessInfoArray);
var
  hFile: HANDLE;
  Index: Integer;
  ALength: Integer;
  hProcess: HANDLE;
  hCurrentProcess: HANDLE;
  AFileName, AOpenName: UnicodeString;
  SystemInformation : PSystemHandleInformationEx;
begin
  if GetNativeName(FileName, AFileName) and GetFileHandleList(SystemInformation) then
  begin
    ALength:= Length(AFileName);
    hCurrentProcess:= GetCurrentProcess;
    for Index:= 0 to SystemInformation^.Count - 1 do
    begin
      if (SystemInformation^.Handle[Index].ObjectTypeNumber = FileHandleType) then
      begin
        hProcess:= OpenProcess(PROCESS_DUP_HANDLE or PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, SystemInformation^.Handle[Index].ProcessId);
        if (hProcess <> 0) then
        begin
          if DuplicateHandle(hProcess, SystemInformation^.Handle[Index].Handle, hCurrentProcess, @hFile, 0, False, DUPLICATE_SAME_ACCESS) then
          begin
            if CheckHandleType(hFile) then
            begin
              AOpenName:= GetFileName(hFile);
              if Length(AOpenName) >= ALength then
              begin
                if (_wcsnicmp(PWideChar(AOpenName), PWideChar(AFileName), ALength) = 0) then
                begin
                  if (Length(AOpenName) = ALength) or (AOpenName[ALength + 1] = PathDelim) then
                    AddLock(ProcessInfo, SystemInformation^.Handle[Index].ProcessId, hProcess, SystemInformation^.Handle[Index].Handle);
                end;
              end;
            end;
            CloseHandle(hFile);
          end;
          CloseHandle(hProcess);
        end;
      end;
    end;
    FreeMem(SystemInformation);
  end;
end;

function GetFileInUseProcessFast(const FileName: String; out ProcessInfo: TProcessInfoArray): Boolean;
const
  MAX_CNT = 64;
var
  I: Integer;
  dwReason: DWORD;
  dwSession: DWORD;
  hProcess: HANDLE;
  nProcInfoNeeded: UINT;
  rgsFileNames: PWideChar;
  nProcInfo: UINT = MAX_CNT;
  ftCreation, ftDummy: TFileTime;
  szSessionKey: array[0..CCH_RM_SESSION_KEY] of WideChar;
  rgAffectedApps: array[0..MAX_CNT - 1] of TRMProcessInfo;
begin
  if (RstrtMgrLib = 0) then Exit(False);
  ZeroMemory(@szSessionKey[0], SizeOf(szSessionKey));
  Result:= (RmStartSession(@dwSession, 0, szSessionKey) = ERROR_SUCCESS);
  if Result then
  try
    rgsFileNames:= PWideChar(UTF8Decode(FileName));
    Result:= (RmRegisterResources(dwSession, 1, @rgsFileNames, 0, nil, 0, nil) = ERROR_SUCCESS) and
             (RmGetList(dwSession, @nProcInfoNeeded, @nProcInfo, rgAffectedApps, @dwReason) = ERROR_SUCCESS);
    if Result then
    begin
      Result:= (nProcInfo > 0);
      SetLength(ProcessInfo, nProcInfo);
      for I:= 0 to nProcInfo - 1 do
      begin
        ProcessInfo[I].ProcessId:= rgAffectedApps[I].Process.dwProcessId;
        ProcessInfo[I].ApplicationName:= UTF8Encode(UnicodeString(rgAffectedApps[I].strAppName));

        hProcess:= OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, rgAffectedApps[I].Process.dwProcessId);
        if hProcess <> 0 then
        try
          if GetProcessTimes(hProcess, ftCreation, ftDummy, ftDummy, ftDummy) and
             (CompareFileTime(@rgAffectedApps[I].Process.ProcessStartTime, @ftCreation) = 0) then
          begin
            ProcessInfo[I].ExecutablePath:= UTF8Encode(GetProcessFileName(hProcess));
          end;
        finally
          CloseHandle(hProcess);
        end;
      end;
    end;
  finally
    RmEndSession(dwSession);
  end;
end;

function GetFileInUseProcessSlow(const FileName: String; LastError: Integer; var ProcessInfo: TProcessInfoArray): Boolean;
begin
  if (Win32MajorVersion < 6) and (LastError = ERROR_ACCESS_DENIED) then
  begin
    GetModuleInUseProcess(FileName, ProcessInfo)
  end;
  if (LastError = ERROR_SHARING_VIOLATION) then
  begin
    GetFileInUseProcess(FileName, ProcessInfo);
  end;
  Result:= (Length(ProcessInfo) > 0);
end;

function TerminateProcess(ProcessId: DWORD): Boolean;
var
  hProcess: HANDLE;
begin
  hProcess:= OpenProcess(SYNCHRONIZE or PROCESS_TERMINATE, False, ProcessId);
  Result:= (hProcess <> 0);
  if Result then
  begin
    Result:= Windows.TerminateProcess(hProcess, 1);
    CloseHandle(hProcess);
  end;
end;

function FileUnlock(ProcessId: DWORD; hFile: THandle): Boolean;
var
  hProcess: HANDLE;
  hDuplicate: HANDLE;
begin
  Result:= False;
  hProcess:= OpenProcess(PROCESS_DUP_HANDLE or PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessId);
  if (hProcess <> 0) then
  begin
    if (DuplicateHandle(hProcess, hFile, GetCurrentProcess, @hDuplicate, 0, False, DUPLICATE_SAME_ACCESS or DUPLICATE_CLOSE_SOURCE)) then
    begin
      Result:= CloseHandle(hDuplicate);
    end;
   CloseHandle(hProcess);
  end;
end;

procedure GetFileHandleTypeThread({%H-}Parameter : Pointer);
begin
  FileHandleType:= GetFileHandleType;
end;

procedure Initialize;
var
  SystemDirectory: UnicodeString;
begin
  if Win32MajorVersion < 6 then
  begin
    GetFileName:= @GetFileNameOld;
    @NtQueryObject:= GetProcAddress(GetModuleHandleW(ntdll), 'NtQueryObject');
  end
  else begin
    SetLength(SystemDirectory, maxSmallint + 1);
    SetLength(SystemDirectory, GetSystemDirectoryW(Pointer(SystemDirectory), maxSmallint));
    RstrtMgrLib:= LoadLibraryW(PWideChar(SystemDirectory + PathDelim + RstrtMgr));
    if RstrtMgrLib <> 0 then
    begin
      @RmStartSession := GetProcAddress(RstrtMgrLib, 'RmStartSession');
      @RmEndSession := GetProcAddress(RstrtMgrLib, 'RmEndSession');

      @RmRegisterResources := GetProcAddress(RstrtMgrLib, 'RmRegisterResources');
      @RmGetList := GetProcAddress(RstrtMgrLib, 'RmGetList');
    end;
    GetFileName:= @GetFileNameNew;
    @QueryFullProcessImageNameW:= GetProcAddress(GetModuleHandleW(Kernel32), 'QueryFullProcessImageNameW');
    @GetFinalPathNameByHandleW:= GetProcAddress(GetModuleHandleW(Kernel32), 'GetFinalPathNameByHandleW');
  end;
  TThread.ExecuteInThread(@GetFileHandleTypeThread, nil);
end;

initialization
  Initialize;

end.

