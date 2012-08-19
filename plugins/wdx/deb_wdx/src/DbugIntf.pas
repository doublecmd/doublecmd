unit DbugIntf;

interface

uses
  Windows, Dialogs; // We need "Dialogs" for TMsgDlgType

procedure SendBoolean(const Identifier: string; const Value: Boolean);
procedure SendDateTime(const Identifier: string; const Value: TDateTime);
procedure SendDebugEx(const Msg: string; MType: TMsgDlgType);
procedure SendDebug(const Msg: string);
procedure SendDebugClear;
procedure SendInteger(const Identifier: string; const Value: Integer);
procedure SendMethodEnter(const MethodName: string);
procedure SendMethodExit(const MethodName: string);
procedure SendSeparator;
procedure SendDebugFmt(const Msg: string; const Args: array of const);
procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TMsgDlgType);
function StartDebugWin: hWnd;

implementation

uses
  Messages,
  SysUtils,
  Registry,
  Forms; // We need "Forms" for the Application object

threadvar
  MsgPrefix: AnsiString;

const
  chrClearCommand = #3;

var
  PastFailedAttemptToStartDebugWin: Boolean = False;

function StartDebugWin: hWnd;
var
  DebugFilename: string;
  Buf: array[0..MAX_PATH + 1] of Char;
  si: TStartupInfo;
  pi: TProcessInformation;
begin
  MsgPrefix := '';

  Result := 0;
  if PastFailedAttemptToStartDebugWin then
    Exit;

  with TRegIniFile.Create('\Software\GExperts') do
  try
    DebugFilename := ReadString('Debug', 'FilePath', '');
  finally
    Free;
  end;

  if Trim(DebugFileName) = '' then
  begin
    GetModuleFileName(HINSTANCE, Buf, SizeOf(Buf)-1);
    DebugFileName := ExtractFilePath(StrPas(Buf))+'GDebug.exe';
  end;

  if (Trim(DebugFilename) = '') or not FileExists(DebugFilename) then
  begin
    PastFailedAttemptToStartDebugWin := True;
    Exit;
  end;

  FillChar(si, SizeOf(si), #0);
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_SHOW;
  if not CreateProcess(PChar(DebugFilename), nil,
                       nil, nil,
                       False, 0, nil, nil,
                       si, pi) then
  begin
    PastFailedAttemptToStartDebugWin := True;
    Exit;
  end;

  try
    WaitForInputIdle(pi.hProcess, 3 * 1000); // wait for 3 seconds to get idle
  finally
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
  end;

  Result := FindWindow('TfmDebug', nil);
end;

procedure SendDebugEx(const Msg: string; MType: TMsgDlgType);
var
  CDS: TCopyDataStruct;
  DebugWin: hWnd;
  MessageString: string;
{$IFDEF LINUX}
const
  MTypeStr: array[TMsgDlgType] of string =
    ('Waring: ', 'Error: ', 'Information: ', 'Confirmation: ', 'Custom: ');
{$ENDIF LINUX}
begin
{$IFDEF LINUX}
  Writeln('GX: ' + MTypeStr[MType] + Msg);
{$ENDIF LINUX}
{$IFNDEF LINUX}
  DebugWin := FindWindow('TfmDebug', nil);

  if DebugWin = 0 then
    DebugWin := StartDebugWin;

  if DebugWin <> 0 then
  begin
    MessageString := MsgPrefix + Msg;
    CDS.cbData := Length(MessageString) + 4;
    CDS.dwData := 0;
    if Msg = chrClearCommand then
      CDS.lpData := PChar(chrClearCommand+Char(Ord(MType) + 1)+ MessageString +#0)
    else
      CDS.lpData := PChar(#1+Char(Ord(MType) + 1)+ MessageString +#0);
    SendMessage(DebugWin, WM_COPYDATA, WParam(Application.Handle), LParam(@CDS));
  end;
{$ENDIF not LINUX}
end;

procedure SendDebug(const Msg: string);
begin
  SendDebugEx(Msg, mtInformation);
end;

procedure SendDebugFmt(const Msg: string; const Args: array of const);
begin
  SendDebugEx(Format(Msg, Args), mtInformation);
end;

procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TMsgDlgType);
begin
  SendDebugEx(Format(Msg, Args), MType);
end;

procedure SendDebugClear;
begin
  SendDebug(chrClearCommand);
end;

const
  Indentation = '    ';

procedure SendMethodEnter(const MethodName: string);
begin
  MsgPrefix := MsgPrefix + Indentation;  
  SendDebugEx('Entering ' + MethodName, mtInformation);
end;

procedure SendMethodExit(const MethodName: string);
begin
  SendDebugEx('Exiting ' + MethodName, mtInformation);
  Delete(MsgPrefix, 1, Length(Indentation));  
end;

procedure SendSeparator;
const
  SeparatorString = '------------------------------';
begin
  SendDebugEx(SeparatorString, mtInformation);
end;

procedure SendBoolean(const Identifier: string; const Value: Boolean);
begin
  // Note: We deliberately leave "True" and "False" as
  // hard-coded string constants, since these are
  // technical terminology which should not be localised.
  if Value then
    SendDebugEx(Identifier + '= True', mtInformation)
  else
    SendDebugEx(Identifier + '= False', mtInformation);
end;

procedure SendInteger(const Identifier: string; const Value: Integer);
begin
  SendDebugEx(Format('%s = %d', [Identifier, Value]), mtInformation);
end;

procedure SendDateTime(const Identifier: string; const Value: TDateTime);
begin
  SendDebugEx(Identifier + '=' + DateTimeToStr(Value), mtInformation);
end;

end.

