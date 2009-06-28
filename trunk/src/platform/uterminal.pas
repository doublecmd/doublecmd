{
   Double Commander
   -------------------------------------------------------------------------
   Terminal emulator abstract class

   Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

   Based on terminal emulator implementation
   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)

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

unit uTerminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCmdBox;

type
   Cint = Integer;

   { TTerminal }

   TTerminal = class
   protected
     FChildPid: THandle;
     Fpty: LongInt;
   public
     {en
        Read info from pty
     }
     function Read_pty(var Output: UTF8String; const TimeOut: LongInt = 10): LongInt; virtual; abstract;
     {en
        Create new pty and start cmd
     }
     function Fork_pty(const RowCount, ColCount: Integer; const Command: UTF8String; const Params: UTF8String = ''): THandle; virtual; abstract;
     {en
         Write string to pty
     }
     function Write_pty(const Input: UTF8String): Boolean; virtual; abstract;
     //---------------------
     function SendBreak_pty(): Boolean; virtual; abstract; // ^C
     function SendSignal_pty(Sig: Cint): Boolean; virtual; abstract;
     function SetScreenSize(ColCount, RowCount: Integer): Boolean; virtual; abstract;
     //---------------------
     function KillShell: LongInt; virtual; abstract;
     function CSI_GetTaskId(const buf:UTF8string):integer; virtual; abstract; //get index of sequence in CSILast list
     //---------------------}
     property ShellPid: THandle read FChildPid;
     property PtyPid: LongInt read Fpty;
   end;

 { TConsoleThread }

  TConsoleThread = class(TThread)
  protected
    FLock: System.TRTLCriticalSection;
    FTerm: TTerminal;
    FBuf: UTF8String;
    FRowsCount,
    FColsCount: Integer;
    FOut: TCmdBox;
    FShell: String;
  public
    property Terminal: TTerminal read FTerm;
    property RowsCount: Integer read FRowsCount write FRowsCount;
    property ColsCount: Integer read FColsCount write FColsCount;
    property CmdBox: TCmdBox read FOut write FOut;
    property Shell: String read FShell write FShell;
  end;

function CreateConsoleThread: TConsoleThread;

implementation

uses
{$IF DEFINED(WINDOWS)}
  uWinTerm;
{$ELSEIF DEFINED(UNIX)}
  uUnixTerm;
{$ENDIF}

function CreateConsoleThread: TConsoleThread;
{$IF DEFINED(WINDOWS)}
begin
  Result:= TWinConThread.Create;
end;
{$ELSEIF DEFINED(UNIX)}
begin
  Result:= TUnixConThread.Create;
end;
{$ENDIF}

end.

