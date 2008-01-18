unit uLog;

{$mode objfpc}{$H+}

interface

type
  TLogMsgType = (lmtInfo, lmtSuccess, lmtError);

procedure logWrite(const sText:String; LogMsgType : TLogMsgType = lmtInfo);


implementation
uses
  SysUtils, LCLProc, fMain, uGlobs;

procedure logWrite(const sText:String; LogMsgType : TLogMsgType);
var
  LogFile : TextFile;
begin
  if Assigned(fMain.frmMain) and gLogWindow then // if write log to window
  with fMain.frmMain.seLogWindow do
    begin
      CaretY := Lines.AddObject(sText, TObject(LogMsgType)) + 1;
    end;

  if gLogFile then // if write log to file
    begin
      AssignFile(LogFile, gLogFileName);
      try
        if FileExists(gLogFileName) then
          Append(LogFile)
        else
          Rewrite(LogFile);
        WriteLn(LogFile, Format('%s %s', [DateTimeToStr(Now), sText]));

        DebugLn(Format('%s %s',[DateTimeToStr(Now), sText]));

        CloseFile(LogFile);
      except
        on E:Exception do
          DebugLn('Error writing to log: ' + E.Message);
      end;
  end; // gLogWriteFile
end;

end.
