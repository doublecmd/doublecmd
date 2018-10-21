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
  Windows;

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

var
  RmStartSession: function (pSessionHandle: LPDWORD; dwSessionFlags: DWORD; strSessionKey: LPWSTR): DWORD; stdcall;
  RmEndSession: function (dwSessionHandle: DWORD): DWORD; stdcall;

  RmRegisterResources: function(dwSessionHandle: DWORD; nFiles: UINT; rgsFileNames: LPPWSTR; nApplications: UINT; rgApplications: PRMUniqueProcess; nServices: UINT; rgsServiceNames: LPPWSTR): DWORD; stdcall;
  RmGetList: function(dwSessionHandle: DWORD; pnProcInfoNeeded: PUINT; pnProcInfo: PUINT; rgAffectedApps: PRMProcessInfo; lpdwRebootReasons: LPDWORD): DWORD; stdcall;

  QueryFullProcessImageNameW: function(hProcess: HANDLE; dwFlags: DWORD; lpExeName: LPWSTR; lpdwSize: PDWORD): BOOL; stdcall;

var
  RstrtMgrLib: HMODULE = 0;

procedure Initialize;
var
  SystemDirectory: UnicodeString;
begin
  if CheckWin32Version(6, 0) then
  begin
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
    @QueryFullProcessImageNameW:= GetProcAddress(GetModuleHandleW(Kernel32), 'QueryFullProcessImageNameW');
  end;
end;

function GetFileInUseProcess(const FileName: String; LastError: Integer; out
  ProcessInfo: TProcessInfoArray): Boolean;
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
  if (LastError <> ERROR_SHARING_VIOLATION) then Exit(False);
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

initialization
  Initialize;

end.

