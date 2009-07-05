{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains log write functions.

    Copyright (C) 2008-2009  Koblov Alexander (Alexx2000@mail.ru)

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
    FThread: TThread;
    FMsg: String;
    FLogMsgType: TLogMsgType;
    FForce,
    FLogFile: Boolean;
  public
    constructor Create(Thread: TThread);
    destructor Destroy; override;
    procedure WriteLog(const sText: String; LogMsgType: TLogMsgType; bForce, bLogFile: Boolean);
  end;

procedure ShowLogWindow(bShow: Boolean);
procedure logWrite(const sText: String; LogMsgType: TLogMsgType = lmtInfo; bForce: Boolean = False; bLogFile: Boolean = True);
procedure logWrite(Thread: TThread; const sText: String; LogMsgType: TLogMsgType = lmtInfo; bForce: Boolean = False; bLogFile: Boolean = True);

implementation
uses
  SysUtils, LCLProc, fMain, uGlobs, uFileProcs, uOSUtils;

procedure ShowLogWindow(bShow: Boolean);
begin
  if Assigned(fMain.frmMain) then
  with fMain.frmMain do
  begin
    LogSplitter.Visible:= bShow;
    seLogWindow.Visible:= bShow;
  end;
end;

procedure logWrite(const sText: String; LogMsgType: TLogMsgType; bForce, bLogFile: Boolean);
var
  hLogFile: Integer;
  LogMsgTypeObject: TObject absolute LogMsgType;
begin
  if (not gLogWindow) and bForce then
    ShowLogWindow(True);
  if Assigned(fMain.frmMain) and (gLogWindow or bForce) then // if write log to window
  with fMain.frmMain.seLogWindow do
    begin
      CaretY:= Lines.AddObject(sText, LogMsgTypeObject) + 1;
    end;

  if gLogFile and bLogFile then // if write log to file
    try
      if mbFileExists(gLogFileName) then
        hLogFile:= mbFileOpen(gLogFileName, fmOpenReadWrite)
      else
        hLogFile:= mbFileCreate(gLogFileName);

      FileSeek(hLogFile, 0, soFromEnd);
      FileWriteLn(hLogFile, Format('%s %s', [DateTimeToStr(Now), sText]));

      DebugLn(Format('%s %s',[DateTimeToStr(Now), sText]));

      FileClose(hLogFile);
    except
      on E: Exception do
        DebugLn('Error writing to log: ' + E.Message);
    end; // gLogWriteFile
end;

procedure logWrite(Thread: TThread; const sText: String; LogMsgType: TLogMsgType; bForce, bLogFile: Boolean);
var
  LogWriteThread: TLogWriteThread;
begin
  if Assigned (Thread) then
    try
      LogWriteThread:= TLogWriteThread.Create(Thread);
      LogWriteThread.WriteLog(sText, LogMsgType, bForce, bLogFile);
    finally
      LogWriteThread.Free;
    end
  else
    logWrite(sText, LogMsgType, bForce, bLogFile);
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
  FThread:= Thread;
  InitCriticalSection(FCriticalSection);
end;

destructor TLogWriteThread.Destroy;
begin
  FMsg:= '';
  DoneCriticalsection(FCriticalSection);
  inherited Destroy;
end;

procedure TLogWriteThread.WriteLog(const sText: String; LogMsgType: TLogMsgType; bForce, bLogFile: Boolean);
begin
  FMsg:= sText;
  FLogMsgType:= LogMsgType;
  FForce:= bForce;
  FLogFile:= bLogFile;
  FThread.Synchronize(FThread, @LogWriteInTheThread);
end;

end.
