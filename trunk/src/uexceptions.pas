unit uExceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function ExceptionToString: String;
procedure WriteExceptionToFile(const aFileName: UTF8String; const ExceptionText: String = '');
procedure WriteExceptionToErrorFile(const ExceptionText: String = ''); inline;
procedure ShowExceptionDialog(const ExceptionText: String = '');
procedure ShowException(e: Exception);
{en
   Log exception to file, show on console and show message dialog.
   Can be called from other threads.
}
procedure HandleException(e: Exception; AThread: TThread = nil);

implementation

uses
  Forms, Controls, Dialogs, LCLProc, LCLStrConsts, syncobjs,
  uDebug, uLng, uGlobs, uDCVersion, DCOSUtils;

type
  THandleException = class
  private
    FHandleExceptionLock: TCriticalSection;
    FHandleExceptionMessage: String;
    FHandleExceptionBackTrace: String;
    procedure ShowException;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure HandleException(e: Exception; AThread: TThread = nil);
  end;

var
  HandleExceptionObj: THandleException;

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
  if (aFileName <> EmptyStr) and not mbDirectoryExists(aFileName) then
  begin
    AssignFile(f, aFileName);
    if not mbFileExists(aFileName) then
      Rewrite(f)
    else
      Append(f);

    if TextRec(f).mode <> fmClosed then
    begin
      WriteLn(f, '--------------- ',
                 FormatDateTime('dd-mm-yyyy, hh:nn:ss', SysUtils.Now),
                 ' ---------------');
      WriteLn(f, '| DC v', dcVersion, ' Rev. ', dcRevision,
                 ' -- ', TargetCPU + '-' + TargetOS + '-' + TargetWS);
      if WSVersion <> EmptyStr then
        WriteLn(f, '| ', OSVersion, ' -- ', WSVersion)
      else
        WriteLn(f, '| ', OSVersion);

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

      // Make one empty line.
      WriteLn(f);

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

procedure ShowException(e: Exception);
begin
  MessageDlg(Application.Title, rsMsgLogError + LineEnding + e.Message, mtError, [mbOK], 0);
end;

procedure HandleException(e: Exception; AThread: TThread);
begin
  HandleExceptionObj.HandleException(e, AThread);
end;

constructor THandleException.Create;
begin
  FHandleExceptionLock := TCriticalSection.Create;
end;

destructor THandleException.Destroy;
begin
  inherited;
  FreeAndNil(FHandleExceptionLock);
end;

procedure THandleException.HandleException(e: Exception; AThread: TThread);
var
  BackTrace: String;
begin
  if MainThreadID = GetCurrentThreadId then
  begin
    BackTrace := ExceptionToString;
    DCDebug(BackTrace);
    WriteExceptionToErrorFile(BackTrace);
    ShowExceptionDialog(e.Message);
  end
  else
  begin
    FHandleExceptionLock.Acquire;
    try
      FHandleExceptionMessage := e.Message;
      FHandleExceptionBackTrace := ExceptionToString;

      if FHandleExceptionBackTrace <> EmptyStr then
        DCDebug(FHandleExceptionBackTrace);

      TThread.Synchronize(AThread, @ShowException);

    finally
      FHandleExceptionLock.Release;
    end;
  end;
end;

procedure THandleException.ShowException;
begin
  WriteExceptionToErrorFile(FHandleExceptionBackTrace);
  ShowExceptionDialog(FHandleExceptionMessage);
end;

initialization
  HandleExceptionObj := THandleException.Create;

finalization
  FreeAndNil(HandleExceptionObj);

end.

