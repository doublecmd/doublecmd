unit uNamedPipes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function VerifyChild(hPipe: THandle): Boolean;
function VerifyParent(hPipe: THandle): Boolean;

implementation

uses
  Windows, uProcessInfo, uDebug;

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

initialization
  Pointer(GetNamedPipeClientProcessId):= GetProcAddress(GetModuleHandleW(kernel32),
                                                        'GetNamedPipeClientProcessId');

end.

