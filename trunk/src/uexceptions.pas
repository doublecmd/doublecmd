unit uExceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function ExceptionToString: String;
procedure WriteExceptionToFile(const aFileName: UTF8String; const ExceptionText: String = '');
procedure WriteExceptionToErrorFile(const ExceptionText: String = ''); inline;
procedure ShowExceptionDialog(const ExceptionText: String = '');

implementation

uses
  Forms, Controls, Dialogs, LCLProc, LCLStrConsts, uLng, uGlobs;

function ExceptionToString: String;
var
  FrameCount: Integer;
  FrameNumber: Integer;
  Frames: PPointer;
begin
  Result := 'Unhandled exception:';

  if Assigned(ExceptObject) and (ExceptObject is Exception) then
  begin
    Result := Result + ' ' +
              Exception(ExceptObject).ClassName + ': ' +
              Exception(ExceptObject).Message;
  end;

  Result := Result + LineEnding +
            '  Stack trace:' + LineEnding +
            BackTraceStrFunc(ExceptAddr) + LineEnding;

  FrameCount := ExceptFrameCount;
  Frames := ExceptFrames;
  for FrameNumber := 0 to FrameCount - 1 do
    Result := Result + BackTraceStrFunc(Frames[FrameNumber]) + LineEnding;
end;

procedure WriteExceptionToFile(const aFileName: UTF8String; const ExceptionText: String);
var
  f: System.Text;
begin
  if aFileName <> EmptyStr then
  begin
    AssignFile(f, aFileName);
    if not FileExists(aFileName) then
      Rewrite(f)
    else
      Append(f);

    if TextRec(f).mode <> fmClosed then
    begin
      WriteLn(f, '-------- ', FormatDateTime('dd-mm-yyyy, hh:nn:ss', SysUtils.Now), ' --------');

      if ExceptionText = EmptyStr then
      begin
        if Assigned(ExceptObject) and (ExceptObject is Exception) then
          WriteLn(f, 'Unhandled exception: ',
                     Exception(ExceptObject).ClassName, ': ',
                     Exception(ExceptObject).Message)
        else
          WriteLn(f, 'Unhandled exception');
        WriteLn(f, '  Stack trace:');

        System.DumpExceptionBackTrace(f);
      end
      else
        WriteLn(f, ExceptionText);

      CloseFile(f);
    end;
  end;
end;

procedure WriteExceptionToErrorFile(const ExceptionText: String = '');
begin
  WriteExceptionToFile(gErrorFile, ExceptionText);
end;

procedure ShowExceptionDialog(const ExceptionText: String = '');
// Based on TApplication.ShowException.
var
  Msg: string;
  MsgResult: Integer;
begin
  if AppNoExceptionMessages in Application.Flags then exit;

  if ExceptionText = EmptyStr then
  begin
    if Assigned(ExceptObject) and (ExceptObject is Exception) then
      Msg := Exception(ExceptObject).Message
    else
      Msg := '';
  end
  else
    Msg := ExceptionText;

  if FindInvalidUTF8Character(PChar(Msg), Length(Msg), False) > 0 then
    Msg := AnsiToUtf8(Msg);
  if (Msg <> '') and (Msg[length(Msg)] = LineEnding) then Delete(Msg, Length(Msg), 1);

  with Application do
  if (not Terminated) and (Application <> nil) and (AppInitialized in Flags) then
  begin
    DisableIdleHandler;
    try
      MsgResult := MessageDlg(
        Application.Title + ' - ' + rsMtError,
        rsMtError + ':' + LineEnding + Msg + LineEnding + LineEnding +
        Format(rsUnhandledExceptionMessage,
          [LineEnding + gErrorFile + LineEnding + LineEnding,
           StringReplace(rsMbIgnore, '&', '', [rfReplaceAll]),
           StringReplace(rsMbAbort, '&', '', [rfReplaceAll])]),
        mtError, [mbIgnore, mbAbort], 0, mbIgnore);

    finally
      EnableIdleHandler;
    end;
    if MsgResult = mrAbort then
    begin
      Flags := Flags + [AppNoExceptionMessages];
      Halt;
    end;
  end;
end;

end.

