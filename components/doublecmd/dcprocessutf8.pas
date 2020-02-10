{
  Based on process.inc from the Free Component Library (FCL)
  Copyright (c) 1999-2008 by the Free Pascal development team

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit DCProcessUtf8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IF DEFINED(MSWINDOWS)}
  , Process, Windows, Pipes
  {$ELSEIF DEFINED(UNIX)}
  , BaseUnix, Process, UTF8Process, DCUnix
  {$ENDIF}
  ;

type
  { TProcessUtf8 }
  {$IF DEFINED(UNIX)}
  TProcessUtf8 = class(UTF8Process.TProcessUTF8)
  private
    procedure DoForkEvent(Sender : TObject);
  public
    constructor Create(AOwner : TComponent); override;
    procedure Execute; override;
    function Resume : Integer; override;
    function Suspend : Integer; override;
    function Terminate (AExitCode : Integer): Boolean; override;
  end;
  {$ELSEIF DEFINED(MSWINDOWS)}
  TProcessUtf8 = class(TProcess)
  public
    procedure Execute; override;
  end;
  {$ENDIF}

implementation

{$IF DEFINED(UNIX)}

{ TProcessUtf8 }

procedure TProcessUtf8.DoForkEvent(Sender: TObject);
begin
  FileCloseOnExecAll;
  if (poNewProcessGroup in Options) then
    if (setpgid(0, 0) < 0) then fpExit(127);
end;

constructor TProcessUtf8.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IF (FPC_FULLVERSION >= 30000)}
  OnForkEvent:= @DoForkEvent;
  {$ELSE}
  OnForkEvent:= @FileCloseOnExecAll;
  {$ENDIF}
end;

procedure TProcessUtf8.Execute;
begin
  inherited Execute;
  if (poNewProcessGroup in Options) then
    PInteger(@ProcessId)^:= -ProcessId;
end;

function TProcessUtf8.Resume: Integer;
begin
  if fpKill(ProcessId, SIGCONT) <> 0 then
    Result:= -1
  else
    Result:= 0;
end;

function TProcessUtf8.Suspend: Integer;
begin
  if fpKill(ProcessId, SIGSTOP) <> 0 then
    Result:= -1
  else
    Result:= 1;
end;

function TProcessUtf8.Terminate(AExitCode: Integer): Boolean;
begin
  Result:= fpKill(ProcessId, SIGTERM) = 0;
  if Result then
  begin
    if Running then
      Result:= fpKill(ProcessId, SIGKILL) = 0;
  end;
  if Result then WaitOnExit;
end;

{$ELSEIF DEFINED(MSWINDOWS)}

{$WARN SYMBOL_DEPRECATED OFF}

{$IF FPC_FULLVERSION < 30000}
type
  TStartupInfoW = TStartupInfo;
{$ENDIF}

resourcestring
  SNoCommandLine = 'Cannot execute empty command-line';
  SErrCannotExecute = 'Failed to execute %s : %d';

const
  PriorityConstants: array [TProcessPriority] of Cardinal =
    (HIGH_PRIORITY_CLASS, IDLE_PRIORITY_CLASS,
    NORMAL_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS
{$IF FPC_FULLVERSION >= 30200}
    , BELOW_NORMAL_PRIORITY_CLASS,ABOVE_NORMAL_PRIORITY_CLASS
{$ENDIF}
    );

function GetStartupFlags(P: TProcess): Cardinal;
begin
  with P do
  begin
    Result := 0;
    if poUsePipes in Options then
      Result := Result or Startf_UseStdHandles;
    if suoUseShowWindow in StartupOptions then
      Result := Result or startf_USESHOWWINDOW;
    if suoUSESIZE in StartupOptions then
      Result := Result or startf_usesize;
    if suoUsePosition in StartupOptions then
      Result := Result or startf_USEPOSITION;
    if suoUSECOUNTCHARS in StartupOptions then
      Result := Result or startf_usecountchars;
    if suoUsefIllAttribute in StartupOptions then
      Result := Result or startf_USEFILLATTRIBUTE;
  end;
end;

function GetCreationFlags(P: TProcess): Cardinal;
begin
  with P do
  begin
    Result := 0;
    if poNoConsole in Options then
      Result := Result or Detached_Process;
    if poNewConsole in Options then
      Result := Result or Create_new_console;
    if poNewProcessGroup in Options then
      Result := Result or CREATE_NEW_PROCESS_GROUP;
    if poRunSuspended in Options then
      Result := Result or Create_Suspended;
    if poDebugProcess in Options then
      Result := Result or DEBUG_PROCESS;
    if poDebugOnlyThisProcess in Options then
      Result := Result or DEBUG_ONLY_THIS_PROCESS;
    if poDefaultErrorMode in Options then
      Result := Result or CREATE_DEFAULT_ERROR_MODE;
    Result := Result or PriorityConstants[Priority];
  end;
end;

function StringsToPWideChars(List: TStrings): Pointer;
var
  I: Integer;
  EnvBlock: WideString;
begin
  EnvBlock := '';
  for I := 0 to List.Count - 1 do
    EnvBlock := EnvBlock + UTF8Decode(List[I]) + #0;
  EnvBlock := EnvBlock + #0;
  GetMem(Result, Length(EnvBlock) * SizeOf(Widechar));
  CopyMemory(Result, @EnvBlock[1], Length(EnvBlock) * SizeOf(Widechar));
end;

procedure InitProcessAttributes(P: TProcess; var PA: TSecurityAttributes);
begin
  FillChar(PA, SizeOf(PA), 0);
  PA.nLength := SizeOf(PA);
end;

procedure InitThreadAttributes(P: TProcess; var TA: TSecurityAttributes);
begin
  FillChar(TA, SizeOf(TA), 0);
  TA.nLength := SizeOf(TA);
end;

procedure InitStartupInfo(P: TProcess; var SI: TStartupInfoW);
const
  SWC: array [TShowWindowOptions] of Cardinal =
    (0, SW_HIDE, SW_Maximize, SW_Minimize, SW_Restore, SW_Show,
    SW_ShowDefault, SW_ShowMaximized, SW_ShowMinimized,
    SW_showMinNOActive, SW_ShowNA, SW_ShowNoActivate, SW_ShowNormal);
begin
  FillChar(SI, SizeOf(SI), 0);
  with SI do
  begin
    dwFlags := GetStartupFlags(P);
    if P.ShowWindow <> swoNone then
      dwFlags := dwFlags or Startf_UseShowWindow
    else
      dwFlags := dwFlags and not Startf_UseShowWindow;
    wShowWindow := SWC[P.ShowWindow];
    if (poUsePipes in P.Options) then
    begin
      dwFlags := dwFlags or Startf_UseStdHandles;
    end;
    if P.FillAttribute <> 0 then
    begin
      dwFlags := dwFlags or Startf_UseFillAttribute;
      dwFillAttribute := P.FillAttribute;
    end;
    dwXCountChars := P.WindowColumns;
    dwYCountChars := P.WindowRows;
    dwYsize := P.WindowHeight;
    dwXsize := P.WindowWidth;
    dwy := P.WindowTop;
    dwX := P.WindowLeft;
  end;
end;

{ The handles that are to be passed to the child process must be
  inheritable. On the other hand, only non-inheritable handles
  allow the sending of EOF when the write-end is closed. This
  function is used to duplicate the child process's ends of the
  handles into inheritable ones, leaving the parent-side handles
  non-inheritable.
}
function DuplicateHandleFP(var Handle: THandle): Boolean;
var
  oldHandle: THandle;
begin
  oldHandle := Handle;
  Result := DuplicateHandle(GetCurrentProcess(), oldHandle,
    GetCurrentProcess(), @Handle, 0, True, DUPLICATE_SAME_ACCESS);
  if Result then
    Result := CloseHandle(oldHandle);
end;


procedure CreatePipes(var HI, HO, HE: THandle; var SI: TStartupInfoW;
  CE: Boolean; APipeBufferSize: Cardinal);
begin
  CreatePipeHandles(SI.hStdInput, HI, APipeBufferSize);
  DuplicateHandleFP(SI.hStdInput);
  CreatePipeHandles(HO, Si.hStdOutput, APipeBufferSize);
  DuplicateHandleFP(Si.hStdOutput);
  if CE then
  begin
    CreatePipeHandles(HE, SI.hStdError, APipeBufferSize);
    DuplicateHandleFP(SI.hStdError);
  end
  else
  begin
    SI.hStdError := SI.hStdOutput;
    HE := HO;
  end;
end;

function MaybeQuote(const S: String): String;
begin
  if (Pos(' ', S) <> 0) then
    Result := '"' + S + '"'
  else
    Result := S;
end;

function MaybeQuoteIfNotQuoted(const S: String): String;
begin
  if (Pos(' ', S) <> 0) and (pos('"', S) = 0) then
    Result := '"' + S + '"'
  else
    Result := S;
end;

{ TProcessUtf8 }

procedure TProcessUtf8.Execute;
var
  I: Integer;
  PName, PDir, PCommandLine: PWideChar;
  FEnv: Pointer;
  FCreationFlags: Cardinal;
  FProcessAttributes: TSecurityAttributes;
  FThreadAttributes: TSecurityAttributes;
  FProcessInformation: TProcessInformation;
  FStartupInfo: TStartupInfoW;
  HI, HO, HE: THandle;
  Cmd: String;
begin
  InheritHandles := True;
  PName := nil;
  PCommandLine := nil;
  PDir := nil;

  if (ApplicationName = '') and (CommandLine = '') and (Executable = '') then
    raise EProcess.Create(SNoCommandline);
  if (ApplicationName <> '') then
  begin
    PName := PWideChar(UTF8Decode(ApplicationName));
    PCommandLine := PWideChar(UTF8Decode(CommandLine));
  end
  else if (CommandLine <> '') then
    PCommandLine := PWideChar(UTF8Decode(CommandLine))
  else if (Executable <> '') then
  begin
    Cmd := MaybeQuoteIfNotQuoted(Executable);
    for I := 0 to Parameters.Count - 1 do
      Cmd := Cmd + ' ' + MaybeQuoteIfNotQuoted(Parameters[I]);
    PCommandLine := PWideChar(UTF8Decode(Cmd));
  end;
  if CurrentDirectory <> '' then
    PDir := PWideChar(UTF8Decode(CurrentDirectory));
  if Environment.Count <> 0 then
    FEnv := StringsToPWideChars(Environment)
  else
    FEnv := nil;
  try
    FCreationFlags := GetCreationFlags(Self);
    InitProcessAttributes(Self, FProcessAttributes);
    InitThreadAttributes(Self, FThreadAttributes);
    InitStartupInfo(Self, FStartUpInfo);
    if poUsePipes in Options then
      CreatePipes(HI, HO, HE, FStartupInfo, not (poStdErrToOutPut in Options),
        PipeBufferSize);
    try
      if not CreateProcessW(PName, PCommandLine, @FProcessAttributes,
        @FThreadAttributes, InheritHandles, FCreationFlags, FEnv,
        PDir, FStartupInfo, FProcessInformation) then
        raise EProcess.CreateFmt(SErrCannotExecute, [CommandLine, GetLastError]);
      PHandle(@ProcessHandle)^ := FProcessInformation.hProcess;
      PHandle(@ThreadHandle)^ := FProcessInformation.hThread;
      PInteger(@ProcessID)^ := FProcessINformation.dwProcessID;
    finally
      if poUsePipes in Options then
      begin
        FileClose(FStartupInfo.hStdInput);
        FileClose(FStartupInfo.hStdOutput);
        if not (poStdErrToOutPut in Options) then
          FileClose(FStartupInfo.hStdError);
        CreateStreams(HI, HO, HE);
      end;
    end;
    FRunning := True;
  finally
    if FEnv <> nil then
      FreeMem(FEnv);
  end;
  if not (csDesigning in ComponentState) and // This would hang the IDE !
    (poWaitOnExit in Options) and not (poRunSuspended in Options) then
    WaitOnExit;
end;

{$ENDIF}

end.

