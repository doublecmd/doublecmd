unit uProcessInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JwaWinNT, Windows;

function GetParentProcessId(ProcessId: DWORD): DWORD;
function GetProcessFileName(hProcess: HANDLE): UnicodeString;
function GetProcessFileNameEx(ProcessId: DWORD): UnicodeString;
function GetTokenUserSID(hToken: HANDLE; out SID: TBytes): Boolean;
function GetProcessUserSID(hProcess: HANDLE; out SID: TBytes): Boolean;

implementation

uses
  JwaTlHelp32;

var
  GetProcessImageFileNameW: function(hProcess: HANDLE; lpImageFileName: LPWSTR;
                                     nSize: DWORD): DWORD; stdcall;

function GetParentProcessId(ProcessId: DWORD): DWORD;
var
  hSnapshot : THandle;
  ProcessEntry : TProcessEntry32;
begin
  Result := 0;
  hSnapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnapshot <> INVALID_HANDLE_VALUE then
  begin
    ProcessEntry.dwSize := SizeOf(TProcessEntry32);
    if Process32First(hSnapshot, ProcessEntry) then
    begin
      repeat
        if ProcessEntry.th32ProcessID = ProcessId then
        begin
          Result:= ProcessEntry.th32ParentProcessID;
          Break;
        end;
      until not Process32Next(hSnapshot, ProcessEntry);
    end;
    CloseHandle(hSnapshot);
  end;
end;

function GetProcessFileName(hProcess: HANDLE): UnicodeString;
begin
  SetLength(Result, maxSmallint + 1);
  SetLength(Result, GetProcessImageFileNameW(hProcess, PWideChar(Result), maxSmallint));
end;

function GetProcessFileNameEx(ProcessId: DWORD): UnicodeString;
const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
var
  hProcess: HANDLE;
begin
  Result:= EmptyWideStr;
  hProcess:= OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, ProcessId);
  if hProcess <> 0 then
  try
    Result:= GetProcessFileName(hProcess);
  finally
    CloseHandle(hProcess);
  end;
end;

function GetTokenUserSID(hToken: HANDLE; out SID: TBytes): Boolean;
var
  ReturnLength: DWORD = 0;
  TokenInformation: array [0..SECURITY_MAX_SID_SIZE] of Byte;
  UserToken: TTokenUser absolute TokenInformation;
begin
  Result:= GetTokenInformation(hToken, TokenUser, @TokenInformation,
                               SizeOf(TokenInformation), ReturnLength);
  if Result then
  begin
    SetLength(SID, GetLengthSid(UserToken.User.Sid));
    CopySid(Length(SID), PSID(@SID[0]), UserToken.User.Sid);
  end;
end;

function GetProcessUserSID(hProcess: HANDLE; out SID: TBytes): Boolean;
var
  hToken: HANDLE = 0;
begin
  Result:= OpenProcessToken(hProcess, TOKEN_QUERY, hToken);
  if Result then
  begin
    Result:= GetTokenUserSID(hToken, SID);
    CloseHandle(hToken);
  end;
end;

initialization
  Pointer(GetProcessImageFileNameW):= GetProcAddress(GetModuleHandle('psapi.dll'),
                                                     'GetProcessImageFileNameW');
end.
