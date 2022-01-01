{
  Double Commander
  -------------------------------------------------------------------------
  Unix pseudoterminal device implementation

  Copyright (C) 2021 Alexander Koblov (alexx2000@mail.ru)

  Permission is hereby granted, free of charge, to any person obtaining
  a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

unit VTEmuPty;

{$mode delphi}

interface

uses
  Classes, SysUtils, BaseUnix, TermIO, InitC, VTEmuCtl;

// Under Linux and BSD forkpty is situated in libutil.so library
{$IF NOT DEFINED(DARWIN)}
  {$LINKLIB util}
{$ENDIF}

type

  { TPtyDevice }

  TPtyDevice = class(TCustomPtyDevice)
  private
    Fpty: LongInt;
    FThread: TThread;
    FChildPid: THandle;
    FEventPipe: TFilDes;
    FLength, FCols, FRows: Integer;
    FBuffer: array[0..8191] of AnsiChar;
  protected
    procedure ReadySync;
    procedure ReadThread;
    procedure SetConnected(AValue: Boolean); override;
    function CreatePseudoConsole(const cmd: String): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function WriteStr(const Str: string): Integer; override;
    function SetCurrentDir(const Path: String): Boolean; override;
    function SetScreenSize(aCols, aRows: Integer): Boolean; override;
  end;

implementation

uses
  Errors, DCOSUtils, DCStrUtils, DCUnix;

type
  Pwinsize = ^winsize;
  Ptermios = ^termios;

function forkpty(__amaster: Plongint; __name: Pchar; __termp: Ptermios; __winp: Pwinsize): longint;cdecl;external clib name 'forkpty';
function execl(__path: Pchar; __arg: Pchar): longint;cdecl;varargs;external clib name 'execl';

{ TPtyDevice }

procedure TPtyDevice.SetConnected(AValue: Boolean);
var
  AShell: String;
  Symbol: Byte = 0;
begin
  if FConnected = AValue then Exit;
  FConnected:= AValue;

  if FConnected then
  begin
    AShell:= mbGetEnvironmentVariable('SHELL');
    if Length(AShell) = 0 then AShell:= '/bin/sh';
    FConnected:= CreatePseudoConsole(AShell);
    if FConnected then
    begin
      FThread:= TThread.ExecuteInThread(ReadThread);
    end;
  end
  else begin
    if FChildPid > 0 then
    begin
      FpKill(FChildPid, SIGTERM);
    end;
    FileWrite(FEventPipe[1], Symbol, 1);
  end;
end;

procedure TPtyDevice.ReadySync;
begin
  if Assigned(FOnRxBuf) then
    FOnRxBuf(Self, FBuffer, FLength);
end;

procedure TPtyDevice.ReadThread;
var
  ret: cint;
  symbol: byte = 0;
  fds: array[0..1] of tpollfd;
begin
  fds[0].fd:= FEventPipe[0];
  fds[0].events:= POLLIN;

  fds[1].fd:= Fpty;
  fds[1].events:= POLLIN;

  while FConnected do
  begin
    repeat
      ret:= fpPoll(@fds[0], 2, -1);
    until (ret <> -1) or (fpGetErrNo <> ESysEINTR);
    if (ret = -1) then
    begin
      WriteLn(SysErrorMessage(fpGetErrNo));
      Break;
    end;
    if (fds[0].events and fds[0].revents <> 0) then
    begin
      while FileRead(fds[0].fd, symbol, 1) <> -1 do;
      Break;
    end;
    if (fds[1].events and fds[1].revents <> 0) then
    begin
      FLength:= FileRead(Fpty, FBuffer, SizeOf(FBuffer));
      if (FLength > 0) then TThread.Synchronize(FThread, ReadySync);
    end;
  end;
end;

function TPtyDevice.CreatePseudoConsole(const cmd: String): Boolean;
var
  ws: TWinSize;
begin
  ws.ws_row:= FRows;
  ws.ws_col:= FCols;
  ws.ws_xpixel:= 0;
  ws.ws_ypixel:= 0;

  FChildPid:= forkpty(@Fpty, nil, nil, @ws);

  if FChildPid = 0 then
  begin
    FileCloseOnExecAll;
    setenv('TERM', 'xterm-256color', 1);
    execl(PAnsiChar(cmd), PAnsiChar(cmd), nil);
    Errors.PError('execl() failed. Command: '+ cmd, cerrno);
    fpExit(127);
  end;
  Result:= (FChildPid > 0);
end;

constructor TPtyDevice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if fpPipe(FEventPipe) < 0 then
    WriteLn(SysErrorMessage(fpGetErrNo))
  else begin
    // Set both ends of pipe non blocking
    FileCloseOnExec(FEventPipe[0]); FileCloseOnExec(FEventPipe[1]);
    FpFcntl(FEventPipe[0], F_SetFl, FpFcntl(FEventPipe[0], F_GetFl) or O_NONBLOCK);
    FpFcntl(FEventPipe[1], F_SetFl, FpFcntl(FEventPipe[1], F_GetFl) or O_NONBLOCK);
  end;
end;

destructor TPtyDevice.Destroy;
begin
  SetConnected(False);
  inherited Destroy;
  FileClose(FEventPipe[0]);
  FileClose(FEventPipe[1]);
end;

function TPtyDevice.WriteStr(const Str: string): Integer;
begin
  Result:= FileWrite(Fpty, Pointer(Str)^, Length(Str));
end;

function TPtyDevice.SetCurrentDir(const Path: String): Boolean;
begin
  Result:= WriteStr(' cd ' + EscapeNoQuotes(Path) + #13) > 0;
end;

function TPtyDevice.SetScreenSize(aCols, aRows: Integer): Boolean;
var
  ws: TWinSize;
begin
  ws.ws_row:= aRows;
  ws.ws_col:= aCols;
  ws.ws_xpixel:= 0;
  ws.ws_ypixel:= 0;

  Result:= FpIOCtl(Fpty,TIOCSWINSZ,@ws) = 0;

  if Result then
  begin
    FCols:= aCols;
    FRows:= aRows;
  end;
end;

end.

