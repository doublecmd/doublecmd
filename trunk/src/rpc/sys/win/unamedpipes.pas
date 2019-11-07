unit uNamedPipes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PTrustee = ^TTrustee;
  TTrustee = record
    pMultipleTrustee: PTrustee;
    MultipleTrusteeOperation: DWORD;
    TrusteeForm: DWORD;
    TrusteeType: DWORD;
    ptstrName: PAnsiChar;
  end;

  TExplicitAccess = record
    grfAccessPermissions: DWORD;
    grfAccessMode: DWORD;
    grfInheritance: DWORD;
    Trustee: TTrustee;
  end;

function VerifyChild(hPipe: THandle): Boolean;
function VerifyParent(hPipe: THandle): Boolean;
function GetCurrentUserSID(var SID: TBytes): Boolean;

implementation

uses
  JwaWinNT, Windows, uProcessInfo, uDebug;

var
  GetNamedPipeClientProcessId: function(Pipe: HANDLE; ClientProcessId: PULONG): BOOL; stdcall;

function VerifyChild(hPipe: HANDLE): Boolean;
var
  ClientProcessId: ULONG;
begin
  if GetNamedPipeClientProcessId(hPipe, @ClientProcessId) then
  begin
    // Allow to connect from child process and same executable only
    if GetCurrentProcessId = GetParentProcessId(ClientProcessId) then
    begin
      DCDebug('My: ', GetProcessFileName(GetCurrentProcess));
      DCDebug('Client: ', GetProcessFileNameEx(ClientProcessId));

      if UnicodeSameText(GetProcessFileName(GetCurrentProcess), GetProcessFileNameEx(ClientProcessId)) then
        Exit(True);
    end;
  end;
  Result:= False;
end;

function VerifyParent(hPipe: HANDLE): Boolean;
var
  ClientProcessId: ULONG;
begin
  if GetNamedPipeClientProcessId(hPipe, @ClientProcessId) then
  begin
    // Allow to connect from parent process and same executable only
    if ClientProcessId = GetParentProcessId(GetCurrentProcessId) then
    begin

      DCDebug('My: ', GetProcessFileName(GetCurrentProcess));
      DCDebug('Client: ', GetProcessFileNameEx(ClientProcessId));

      if UnicodeSameText(GetProcessFileName(GetCurrentProcess), GetProcessFileNameEx(ClientProcessId)) then
        Exit(True);
    end;
  end;
  Result:= False;
end;

function GetCurrentUserSID(var SID: TBytes): Boolean;
var
  ReturnLength: DWORD = 0;
  TokenHandle: HANDLE = INVALID_HANDLE_VALUE;
  TokenInformation: array [0..SECURITY_MAX_SID_SIZE] of Byte;
  UserToken: TTokenUser absolute TokenInformation;
begin
  Result:= OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, TokenHandle);
  if not Result then
  begin
    if GetLastError = ERROR_NO_TOKEN then
      Result:= OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle);
  end;
  if Result then
  begin
    Result:= GetTokenInformation(TokenHandle, TokenUser,
                                 @TokenInformation, SizeOf(TokenInformation), ReturnLength);
    CloseHandle(TokenHandle);
    if Result then
    begin
      SetLength(SID, GetLengthSid(UserToken.User.Sid));
      CopySid(ReturnLength, PSID(@SID[0]), UserToken.User.Sid);
    end;
  end;
end;

initialization
  Pointer(GetNamedPipeClientProcessId):= GetProcAddress(GetModuleHandleW(kernel32),
                                                        'GetNamedPipeClientProcessId');

end.

