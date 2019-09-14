unit uSuperUser;

{$mode objfpc}{$H+}

interface

function ExecCmdAdmin(const Exe: String; Args: array of String; sStartPath: String = ''): Boolean;

implementation

uses
  SysUtils
{$IF DEFINED(MSWINDOWS)}
  , Windows, DCOSUtils
{$ELSEIF DEFINED(UNIX)}
  , Unix, BaseUnix, DCUnix
  {$IF DEFINED(DARWIN)}
  , DCStrUtils,
  {$ENDIF}
{$ENDIF}
  ;

{$IF DEFINED(UNIX)}

function WaitForPidThread(Parameter : Pointer): PtrInt;
var
  Status : cInt = 0;
  PID: PtrInt absolute Parameter;
begin
  while (FpWaitPid(PID, @Status, 0) = -1) and (fpgeterrno() = ESysEINTR) do;
  WriteLn('Process ', PID, ' finished, exit status ', Status);
  Result:= Status; EndThread(Result);
end;

function ExecuteCommand(Command: String; Args: TStringArray; StartPath: String): Boolean;
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
    end
  else if ProcessId > 0 then          { Parent }
    begin
      {$PUSH}{$WARNINGS OFF}{$HINTS OFF}
      BeginThread(@WaitForPidThread, Pointer(PtrInt(ProcessId)));
      {$POP}
    end;

  Result := (ProcessId > 0);
end;

{$ENDIF}

function ExecCmdAdmin(const Exe: String; Args: array of String; sStartPath: String): Boolean;
{$IF DEFINED(MSWINDOWS)}
var
  Index: Integer;
  AParams: String;
begin
  AParams := EmptyStr;
  for Index := Low(Args) to High(Args) do
    AParams += '"' + Args[Index] + '" ';

  if sStartPath = EmptyStr then
    sStartPath:= mbGetCurrentDir;

  Result:= ShellExecuteW(0, 'runas', PWideChar(UTF8Decode(Exe)),
                         PWideChar(UTF8Decode(AParams)),
                         PWideChar(UTF8Decode(sStartPath)), SW_SHOW) > 32;
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

end.

