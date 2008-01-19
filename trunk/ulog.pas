{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains log write functions.

    Copyright (C) 2008  Koblov Alexander (Alexx2000@mail.ru)

    contributors:

    Radek Cervinka  <radek.cervinka@centrum.cz>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uLog;

{$mode objfpc}{$H+}

interface
uses
  Classes;
  
type
  TLogMsgType = (lmtInfo, lmtSuccess, lmtError);
  
type
  { TLogWriteThread }

  TLogWriteThread = class
  private
    FCriticalSection: TRTLCriticalSection;
    procedure LogWriteInTheThread;
  protected
    FThread : TThread;
    FMsg : String;
    FLogMsgType : TLogMsgType;
  public
    constructor Create(Thread : TThread);
    destructor Destroy;override;
    procedure WriteLog(const sText:String; LogMsgType : TLogMsgType);
  end;
  
procedure logWrite(const sText:String; LogMsgType : TLogMsgType = lmtInfo);
procedure logWrite(Thread : TThread; const sText:String; LogMsgType : TLogMsgType = lmtInfo);

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

procedure logWrite(Thread: TThread; const sText: String; LogMsgType: TLogMsgType);
var
  LogWriteThread : TLogWriteThread;
begin
  try
    LogWriteThread := TLogWriteThread.Create(Thread);
    LogWriteThread.WriteLog(sText, LogMsgType);
  finally
    LogWriteThread.Free;
  end;
end;

{ TLogWriteThread }

procedure TLogWriteThread.LogWriteInTheThread;
begin
   EnterCriticalsection(FCriticalSection);
   logWrite(FMsg, FLogMsgType);
   LeaveCriticalsection(FCriticalSection);
end;

constructor TLogWriteThread.Create(Thread: TThread);
begin
  FThread := Thread;
  InitCriticalSection(FCriticalSection);
end;

destructor TLogWriteThread.Destroy;
begin
  FMsg := '';
  DoneCriticalsection(FCriticalSection);
  inherited Destroy;
end;

procedure TLogWriteThread.WriteLog(const sText: String; LogMsgType: TLogMsgType);
begin
  FMsg := sText;
  FLogMsgType := LogMsgType;
  FThread.Synchronize(FThread, @LogWriteInTheThread);
end;

end.
