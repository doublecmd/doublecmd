unit uSuperUser;

{$mode objfpc}{$H+}

interface

procedure WaitProcess(Process: UIntPtr);
function AdministratorPrivileges: Boolean; inline;
function TerminateProcess(Process: UIntPtr): Boolean;
function ElevationRequired(LastError: Integer = 0): Boolean;
function ExecCmdAdmin(const Exe: String; Args: array of String; sStartPath: String = ''): UIntPtr;

implementation

uses
  SysUtils
{$IF DEFINED(MSWINDOWS)}
  , Types, Windows, DCOSUtils, ShellApi, DCConvertEncoding, uMyWindows
{$ELSEIF DEFINED(UNIX)}
  , Classes, Unix, BaseUnix, DCUnix, Dialogs, SyncObjs, Process, un_process
  {$IF DEFINED(DARWIN)}
  , DCStrUtils
  {$ENDIF}
{$ENDIF}
  ;

var
  FAdministratorPrivileges: Boolean;

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
  if FAdministratorPrivileges then Exit(False);
  if LastError = 0 then LastError:= GetLastError;
  Result:= (LastError = ERROR_ACCESS_DENIED) or (LastError = ERROR_PRIVILEGE_NOT_HELD) or (LastError = ERROR_INVALID_OWNER) or (LastError = ERROR_NOT_ALL_ASSIGNED);
end;
{$ELSE}
begin
  if FAdministratorPrivileges then Exit(False);
  if LastError = 0 then LastError:= GetLastOSError;
  Result:= (LastError = ESysEPERM) or (LastError = ESysEACCES);
end;
{$ENDIF}

function AdministratorPrivileges: Boolean;
begin
  Result:= FAdministratorPrivileges;
end;

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

const
  SYS_PATH: array[0..1] of String = ('/usr/bin/', '/usr/local/bin/');

resourcestring
  rsMsgPasswordEnter = 'Please enter the password:';

type
  TSuperProgram = (spNone, spSudo, spPkexec);

  { TSuperUser }

  TSuperUser = class(TThread)
  private
    FPrompt: String;
    FCtl: TExProcess;
    FMessage: String;
    FArgs: TStringArray;
    FEvent: TSimpleEvent;
  private
    procedure Ready;
    procedure RequestPassword;
    procedure OnReadLn(Str: String);
    procedure OnQueryString(Str: String);
  protected
    procedure Execute; override;
  public
    constructor Create(Args: TStringArray; const StartPath: String);
    destructor Destroy; override;
  end;

var
  SuperExe: String;
  SuperProgram: TSuperProgram;

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

function FindExecutable(const FileName: String; out FullName: String): Boolean;
var
  Index: Integer;
begin
  for Index:= Low(SYS_PATH) to High(SYS_PATH) do
  begin
    FullName:= SYS_PATH[Index] + FileName;
    if fpAccess(FullName, X_OK) = 0 then
      Exit(True);
  end;
  Result:= False;
end;

function ExecuteSudo(Args: TStringArray; const StartPath: String): UIntPtr;
begin
  with TSuperUser.Create(Args, StartPath) do
  begin
    Start;
    FEvent.WaitFor(INFINITE);
    Result:= FCtl.Process.ProcessHandle;
  end;
end;

{ TSuperUser }

procedure TSuperUser.Ready;
begin
  FEvent.SetEvent;
  Yield;
  FreeOnTerminate:= True;
  FCtl.OnOperationProgress:= nil;
end;

procedure TSuperUser.RequestPassword;
var
  S: String = '';
begin
  if Length(FMessage) = 0 then begin
    FMessage:= rsMsgPasswordEnter
  end;
  if not InputQuery('Double Commander', FMessage, True, S) then
    FCtl.Stop
  else begin
    S:= S + LineEnding;
    FCtl.Process.Input.Write(S[1], Length(S));
  end;
  FMessage:= EmptyStr;
end;

procedure TSuperUser.OnReadLn(Str: String);
begin
  FMessage:= Str;
end;

procedure TSuperUser.OnQueryString(Str: String);
begin
  Synchronize(@RequestPassword)
end;

procedure TSuperUser.Execute;
var
  GUID : TGUID;
  Index: Integer;
begin
  CreateGUID(GUID);
  FPrompt:= GUIDToString(GUID);
  FCtl.Process.Options:= FCtl.Process.Options + [poStderrToOutPut];
  FCtl.Process.Executable:= SuperExe;
  FCtl.Process.Parameters.Add('-S');
  FCtl.Process.Parameters.Add('-k');
  FCtl.Process.Parameters.Add('-p');
  FCtl.Process.Parameters.Add(FPrompt);
  for Index:= 0 to High(FArgs) do begin
    FCtl.Process.Parameters.Add(FArgs[Index]);
  end;
  FCtl.QueryString:= FPrompt;
  FCtl.OnQueryString:= @OnQueryString;
  FCtl.OnOperationProgress:= @Ready;
  fCtl.OnProcessExit:= @Ready;
  FCtl.OnReadLn:= @OnReadLn;
  FCtl.Execute;
end;

constructor TSuperUser.Create(Args: TStringArray; const StartPath: String);
begin
  inherited Create(True);
  FCtl:= TExProcess.Create(EmptyStr);
  FCtl.Process.CurrentDirectory:= StartPath;
  FEvent:= TSimpleEvent.Create;
  FArgs:= Args;
end;

destructor TSuperUser.Destroy;
begin
  inherited Destroy;
  FEvent.Free;
  FCtl.Free;
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
  lpExecInfo.lpFile:= PWideChar(CeUtf8ToUtf16(Exe));
  lpExecInfo.lpDirectory:= PWideChar(CeUtf8ToUtf16(sStartPath));
  lpExecInfo.lpParameters:= PWideChar(CeUtf8ToUtf16(AParams));
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

  case SuperProgram of
    spSudo:   Result:= ExecuteSudo(AParams, sStartPath);
    spPkexec: Result:= ExecuteCommand(SuperExe, AParams, sStartPath);
  end;
end;
{$ENDIF}

initialization
{$IF DEFINED(DARWIN)}
  FAdministratorPrivileges:= True;
{$ELSEIF DEFINED(UNIX)}
  {$IFDEF LINUX}
  if FindExecutable('pkexec', SuperExe) then
    SuperProgram:= spPkexec
  else
  {$ENDIF}
  if FindExecutable('sudo', SuperExe) then
    SuperProgram:= spSudo
  else begin
    SuperProgram:= spNone;
  end;
  FAdministratorPrivileges:= (fpGetUID = 0) or (SuperProgram = spNone);
{$ELSE}
  FAdministratorPrivileges:= (IsUserAdmin <> dupError);
{$ENDIF}

end.

