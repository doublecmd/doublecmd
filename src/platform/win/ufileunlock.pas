unit uFileUnlock;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TProcessInfo = record
    ProcessId: DWORD;
    ApplicationName: String;
    ExecutablePath: String;
  end;

  TProcessInfoArray = array of TProcessInfo;

function GetFileInUseProcess(const FileName: String; LastError: Integer; out ProcessInfo: TProcessInfoArray): Boolean;

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

  PSystemHandleInformationEx = ^TSystemHandleInformationEx;
  TSystemHandleInformationEx = record
    Count: ULONG;
    Handle: array[0..0] of TSystemHandleInformation;
  end;

var
  RmStartSession: function (pSessionHandle: LPDWORD; dwSessionFlags: DWORD; strSessionKey: LPWSTR): DWORD; stdcall;
  RmEndSession: function (dwSessionHandle: DWORD): DWORD; stdcall;

  RmRegisterResources: function(dwSessionHandle: DWORD; nFiles: UINT; rgsFileNames: LPPWSTR; nApplications: UINT; rgApplications: PRMUniqueProcess; nServices: UINT; rgsServiceNames: LPPWSTR): DWORD; stdcall;
  RmGetList: function(dwSessionHandle: DWORD; pnProcInfoNeeded: PUINT; pnProcInfo: PUINT; rgAffectedApps: PRMProcessInfo; lpdwRebootReasons: LPDWORD): DWORD; stdcall;

  QueryFullProcessImageNameW: function(hProcess: HANDLE; dwFlags: DWORD; lpExeName: LPWSTR; lpdwSize: PDWORD): BOOL; stdcall;

  GetFinalPathNameByHandleW: function(hFile: HANDLE; lpszFilePath: LPWSTR; cchFilePath: DWORD; dwFlags: DWORD): DWORD; stdcall;

var
  RstrtMgrLib: HMODULE = 0;
  GetFileName: function(hFile: HANDLE): UnicodeString;

function _wcsnicmp(const s1, s2: pwidechar; count: ptruint): integer; cdecl; external 'msvcrt.dll';

function GetFileHandleList(out SystemInformation : PSystemHandleInformationEx): Boolean;
const
  MEM_SIZE = SizeOf(TSystemHandleInformationEx);
var
  Status: NTSTATUS;
  SystemInformationLength : ULONG = MEM_SIZE;
begin
  SystemInformation:= GetMem(MEM_SIZE);
  repeat
    Status:= NtQuerySystemInformation(SystemHandleInformation, SystemInformation,
                                      SystemInformationLength, @SystemInformationLength);
    if Status = STATUS_INFO_LENGTH_MISMATCH then begin
      ReAllocMem(SystemInformation, SystemInformationLength + SizeOf(TSystemHandleInformation) * 100)
     end;
  until Status <> STATUS_INFO_LENGTH_MISMATCH;
  Result:= (Status = STATUS_SUCCESS);
  if not Result then FreeMem(SystemInformation);
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
        if (SystemInformation^.Handle[Index].Handle = USHORT(Handle)) and (SystemInformation^.Handle[Index].ProcessId = ProcessId) then
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

function GetModuleFileName(hProcess: HANDLE): UnicodeString;
begin
  SetLength(Result, maxSmallint + 1);
  SetLength(Result, GetModuleFileNameExW(hProcess, 0, PWideChar(Result), maxSmallint));
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

function GetFileInUseProcessOld(const FileName: String; var ProcessInfo: TProcessInfoArray): Boolean;
var
  hFile: HANDLE;
  Index: Integer;
  hProcess: HANDLE;
  AFileName, AOpenName: UnicodeString;
  SystemInformation : PSystemHandleInformationEx;
begin
  if GetNativeName(FileName, AFileName) and GetFileHandleList(SystemInformation) then
  begin
    for Index:= 0 to SystemInformation^.Count - 1 do
    begin
      if (SystemInformation^.Handle[Index].ObjectTypeNumber = FileHandleType) then
      begin
        { Query the object name (unless it has an access of
          0x0012019f, on which NtQueryObject could hang. }
        if (SystemInformation^.Handle[Index].GrantedAccess = $0012019f) then
        begin
          Continue;
        end;
        hProcess:= OpenProcess(PROCESS_DUP_HANDLE or PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, SystemInformation^.Handle[Index].ProcessId);
        if (hProcess <> 0) then
        begin
          if DuplicateHandle(hProcess, SystemInformation^.Handle[Index].Handle, GetCurrentProcess, @hFile, 0, False, DUPLICATE_SAME_ACCESS) then
          begin
            AOpenName:= GetFileName(hFile);
            if (_wcsnicmp(PWideChar(AOpenName), PWideChar(AFileName), Length(AFileName)) = 0) then
            begin
              SetLength(ProcessInfo, Length(ProcessInfo) + 1);
              ProcessInfo[High(ProcessInfo)].ProcessId:= SystemInformation^.Handle[Index].ProcessId;
              ProcessInfo[High(ProcessInfo)].ExecutablePath:= UTF8Encode(GetModuleFileName(hProcess));
            end;
            CloseHandle(hFile);
          end;
          CloseHandle(hProcess);
        end;
      end;
    end;
    FreeMem(SystemInformation);
  end;
  Result:= (Length(ProcessInfo) > 0);
end;

function GetFileInUseProcessNew(const FileName: String; out ProcessInfo: TProcessInfoArray): Boolean;
const
  MAX_CNT = 5;
var
  I: Integer;
  dwSize: DWORD;
  dwReason: DWORD;
  dwSession: DWORD;
  hProcess: HANDLE;
  nProcInfoNeeded: UINT;
  rgsFileNames: PWideChar;
  nProcInfo: UINT = MAX_CNT;
  usExecutable: UnicodeString;
  ftCreation, ftExit, ftKernel, ftUser: TFileTime;
  szSessionKey: array[0..CCH_RM_SESSION_KEY] of WideChar;
  rgAffectedApps: array[0..MAX_CNT - 1] of TRMProcessInfo;
begin
  if (RstrtMgrLib = 0) then Exit(False);
  FillChar(szSessionKey, SizeOf(szSessionKey), 0);
  Result:= (RmStartSession(@dwSession, 0, szSessionKey) = ERROR_SUCCESS);
  if Result then
  try
    rgsFileNames:= PWideChar(UTF8Decode(FileName));
    Result:= (RmRegisterResources(dwSession, 1, @rgsFileNames, 0, nil, 0, nil) = ERROR_SUCCESS) and
             (RmGetList(dwSession, @nProcInfoNeeded, @nProcInfo, rgAffectedApps, @dwReason) = ERROR_SUCCESS);
    if Result then
    begin
      SetLength(ProcessInfo, nProcInfo);
      for I:= 0 to nProcInfo - 1 do
      begin
        ProcessInfo[I].ProcessId:= rgAffectedApps[I].Process.dwProcessId;
        ProcessInfo[I].ApplicationName:= UTF8Encode(UnicodeString(rgAffectedApps[I].strAppName));

        hProcess:= OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, rgAffectedApps[I].Process.dwProcessId);
        if hProcess <> 0 then
        try
          if GetProcessTimes(hProcess, ftCreation, ftExit, ftKernel, ftUser) and
             (CompareFileTime(@rgAffectedApps[I].Process.ProcessStartTime, @ftCreation) = 0) then
          begin
            dwSize:= maxSmallint;
            SetLength(usExecutable, dwSize + 1);
            if QueryFullProcessImageNameW(hProcess, 0, PWideChar(usExecutable), @dwSize) then
            begin
              SetLength(usExecutable, dwSize);
              ProcessInfo[I].ExecutablePath:= UTF8Encode(usExecutable);
            end;
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

function GetFileInUseProcess(const FileName: String; LastError: Integer; out
  ProcessInfo: TProcessInfoArray): Boolean;
begin
  if (LastError <> ERROR_SHARING_VIOLATION) then Exit(False);
  if Win32MajorVersion < 6 then
    Result:= GetFileInUseProcessOld(FileName, ProcessInfo)
  else begin
    Result:= GetFileInUseProcessNew(FileName, ProcessInfo)
  end;
end;

procedure GetFileHandleTypeThread(Parameter : Pointer);
begin
  FileHandleType:= GetFileHandleType;
end;

procedure Initialize;
var
  SystemDirectory: UnicodeString;
begin
  if Win32MajorVersion < 6 then
    GetFileName:= @GetFileNameOld
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

