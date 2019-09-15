unit uSuperUser;

{$mode objfpc}{$H+}

interface

procedure WaitProcess(Process: UIntPtr);
function TerminateProcess(Process: UIntPtr): Boolean;
function ElevationRequired(LastError: Integer = 0): Boolean;
function ExecCmdAdmin(const Exe: String; Args: array of String; sStartPath: String = ''): UIntPtr;

implementation

uses
  SysUtils
{$IF DEFINED(MSWINDOWS)}
  , Windows, DCOSUtils, ShellApi, uMyWindows
{$ELSEIF DEFINED(UNIX)}
  , Unix, BaseUnix, DCUnix
  {$IF DEFINED(DARWIN)}
  , DCStrUtils,
  {$ENDIF}
{$ENDIF}
  ;

var
  AdministratorPrivileges: Boolean;

procedure WaitProcess(Process: UIntPtr);
{$IF DEFINED(MSWINDOWS)}
begin
  WaitForSingleObject(Process, INFINITE);
  CloseHandle(Process);
end;
{$ELSE}
var
  Status : cInt = 0;
begin
  while (FpWaitPid(Process, @Status, 0) = -1) and (fpgeterrno() = ESysEINTR) do;
end;
{$ENDIF}

function ElevationRequired(LastError: Integer = 0): Boolean;
{$IF DEFINED(MSWINDOWS)}
begin
  if AdministratorPrivileges then Exit(False);
  if LastError = 0 then LastError:= GetLastError;
  Result:= (LastError = ERROR_ACCESS_DENIED) or (LastError = ERROR_PRIVILEGE_NOT_HELD) or (LastError = ERROR_INVALID_OWNER);
end;
{$ELSE}
begin
  if AdministratorPrivileges then Exit(False);
  if LastError = 0 then LastError:= GetLastOSError;
  Result:= (LastError = ESysEPERM) or (LastError = ESysEACCES);
end;
{$ENDIF}

function TerminateProcess(Process: UIntPtr): Boolean;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= Windows.TerminateProcess(Process, 1);
end;
{$ELSE}
begin
  Result:= fpKill(Process, SIGTERM) = 0;
end;
{$ENDIF}

{$IF DEFINED(UNIX)}

function ExecuteCommand(Command: String; Args: TStringArray; StartPath: String): UIntPtr;
var
  ProcessId : TPid;
begin
  ProcessId := fpFork;

  if ProcessId = 0 then
    begin
      { Set the close-on-exec flag to all }
      FileCloseOnExecAll;

      { Set child current directory }
      if Length(StartPath) > 0 then fpChdir(StartPath);

      { The child does the actual exec, and then exits }
      if FpExecLP(Command, Args) = -1 then
        Writeln(Format('Execute error %d: %s', [fpgeterrno, SysErrorMessage(fpgeterrno)]));

      { If the FpExecLP fails, we return an exitvalue of 127, to let it be known }
      fpExit(127);
    end
  else if ProcessId = -1 then         { Fork failed }
    begin
      WriteLn('Fork failed: ' + Command, LineEnding, SysErrorMessage(fpgeterrno));
    end;

  if ProcessId < 0 then
    Result := 0
  else
    Result := ProcessId;
end;

{$ELSEIF DEFINED(MSWINDOWS)}

function MaybeQuoteIfNotQuoted(const S: String): String;
begin
  if (Pos(' ', S) <> 0) and (pos('"', S) = 0) then
    Result := '"' + S + '"'
  else
    Result := S;
end;

{$ENDIF}

function ExecCmdAdmin(const Exe: String; Args: array of String; sStartPath: String): UIntPtr;
{$IF DEFINED(MSWINDOWS)}
var
  Index: Integer;
  AParams: String;
  lpExecInfo: TShellExecuteInfoW;
begin
  AParams := EmptyStr;
  for Index := Low(Args) to High(Args) do
    AParams += MaybeQuoteIfNotQuoted(Args[Index]) + ' ';

  if sStartPath = EmptyStr then
    sStartPath:= mbGetCurrentDir;

  ZeroMemory(@lpExecInfo, SizeOf(lpExecInfo));
  lpExecInfo.cbSize:= SizeOf(lpExecInfo);
  lpExecInfo.fMask:= SEE_MASK_NOCLOSEPROCESS;
  lpExecInfo.lpFile:= PWideChar(UTF8Decode(Exe));
  lpExecInfo.lpDirectory:= PWideChar(UTF8Decode(sStartPath));
  lpExecInfo.lpParameters:= PWideChar(UTF8Decode(AParams));
  lpExecInfo.lpVerb:= 'runas';

  if ShellExecuteExW(@lpExecInfo) then
    Result:= lpExecInfo.hProcess
  else
    Result:= 0;
end;
{$ELSEIF DEFINED(DARWIN)}
var
  Index: Integer;
  ACommand: String;
  AParams: TStringArray;
begin
  ACommand:= EscapeNoQuotes(Exe);
  for Index := Low(Args) to High(Args) do
    ACommand += ' ' + EscapeNoQuotes(Args[Index]);

  SetLength(AParams, 7);
  AParams[0]:= '-e';
  AParams[1]:= 'on run argv';
  AParams[2]:= '-e';
  AParams[3]:= 'do shell script (item 1 of argv) with administrator privileges';
  AParams[4]:= '-e';
  AParams[5]:='end run';
  AParams[6]:= ACommand;

  Result:= ExecuteCommand('/usr/bin/osascript', AParams, sStartPath);
end;
{$ELSE}
var
  Index: Integer;
  AParams: TStringArray;
begin
  SetLength(AParams, Length(Args) + 1);
  for Index := Low(Args) to High(Args) do
    AParams[Index + 1]:= Args[Index];
  AParams[0] := Exe;

  Result:= ExecuteCommand('/usr/bin/pkexec', AParams, sStartPath);
end;
{$ENDIF}

initialization
{$IF DEFINED(UNIX)}
  AdministratorPrivileges:= (fpGetUID = 0);
{$ELSE}
  AdministratorPrivileges:= (Win32MajorVersion < 6) or IsUserAdmin;
{$ENDIF}

end.

