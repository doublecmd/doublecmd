{
   Double commander
   -------------------------------------------------------------------------
   This unit contains Unix specific functions

   Copyright (C) 2015-2017 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit DCUnix;

{$mode objfpc}{$H+}

interface

uses
  InitC, BaseUnix;

const
{$IF DEFINED(LINUX)}
  FD_CLOEXEC = 1;
  O_CLOEXEC  = &02000000;
{$ELSEIF DEFINED(FREEBSD)}
  O_CLOEXEC  = &04000000;
{$ELSEIF DEFINED(NETBSD)}
  O_CLOEXEC  = $00400000;
{$ELSE}
  O_CLOEXEC  = 0;
{$ENDIF}

{en
   Set the close-on-exec flag to all
}
procedure FileCloseOnExecAll;
{en
   Set the close-on-exec (FD_CLOEXEC) flag
}
procedure FileCloseOnExec(Handle: System.THandle); inline;
{en
   Change owner and group of a file (does not follow symbolic links)
   @param(path Full path to file)
   @param(owner User ID)
   @param(group Group ID)
   @returns(On success, zero is returned. On error, -1 is returned, and errno is set appropriately)
}
function fpLChown(path : String; owner : TUid; group : TGid): cInt;

function FileLock(Handle: System.THandle; Mode: cInt): System.THandle;

implementation

uses
  SysUtils, Unix, DCConvertEncoding;

{$IF DEFINED(BSD)}
type rlim_t = Int64;
{$ENDIF}

const
  {$IF DEFINED(LINUX)}
  _SC_OPEN_MAX  = 4;
  RLIM_INFINITY = rlim_t(-1);
  {$ELSEIF DEFINED(BSD)}
  _SC_OPEN_MAX  = 5;
  RLIM_INFINITY = rlim_t(High(QWord) shr 1);
  {$ENDIF}

function sysconf(name: cint): clong; cdecl; external clib;
function lchown(path : PChar; owner : TUid; group : TGid): cInt; cdecl; external clib name 'lchown';

procedure FileCloseOnExecAll;
var
  fd: cint;
  p: TRLimit;
  fd_max: rlim_t = RLIM_INFINITY;
begin
  if (FpGetRLimit(RLIMIT_NOFILE, @p) = 0) and (p.rlim_cur <> RLIM_INFINITY) then
    fd_max:= p.rlim_cur
  else begin
    {$IF DECLARED(_SC_OPEN_MAX)}
    fd_max:= sysconf(_SC_OPEN_MAX);
    {$ENDIF}
  end;
  if fd_max = RLIM_INFINITY then
    fd_max:= High(Byte);
  for fd:= 3 to cint(fd_max) do
    FileCloseOnExec(fd);
end;

procedure FileCloseOnExec(Handle: System.THandle);
begin
{$IF DECLARED(FD_CLOEXEC)}
  FpFcntl(Handle, F_SETFD, FpFcntl(Handle, F_GETFD) or FD_CLOEXEC);
{$ENDIF}
end;

function fpLChown(path: String; owner: TUid; group: TGid): cInt;
begin
  Result := lchown(PAnsiChar(CeUtf8ToSys(path)), owner, group);
  if Result = -1 then fpseterrno(fpgetCerrno);
end;

function FileLock(Handle: System.THandle; Mode: cInt): System.THandle;
var
  lockop: cint;
  lockres: cint;
  lockerr: cint;
begin
  Result:= Handle;
  case (Mode and $F0) of
    fmShareCompat,
    fmShareExclusive:
      lockop:= LOCK_EX or LOCK_NB;
    fmShareDenyWrite:
      lockop:= LOCK_SH or LOCK_NB;
    else
      Exit;
  end;
  repeat
    lockres:= fpFlock(Handle, lockop);
  until (lockres = 0) or (fpgeterrno <> ESysEIntr);
  lockerr:= fpgeterrno;
  {
    Only return an error if locks are working and the file was already
    locked. Not if locks are simply unsupported (e.g., on Angstrom Linux
    you always get ESysNOLCK in the default configuration)
  }
  if (lockres <> 0) and ((lockerr = ESysEAGAIN) or (lockerr = ESysEDEADLK)) then
  begin
    Result:= -1;
    FileClose(Handle);
  end;
end;

end.

