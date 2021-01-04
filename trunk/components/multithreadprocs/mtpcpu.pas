{
 **********************************************************************
  This file is part of the Free Pascal run time library.

  See the file COPYING.FPC, included in this distribution,
  for details about the license.
 **********************************************************************

  System depending code for light weight threads.

  Copyright (C) 2008 Mattias Gaertner mattias@freepascal.org

}
unit MTPCPU;

{$mode objfpc}{$H+}
{$inline on}

interface

{$IF defined(windows)}
uses Windows;
{$ELSEIF defined(freebsd) or defined(darwin)}
uses ctypes, sysctl;
{$ELSEIF defined(linux)}
{$linklib c}
uses ctypes;
{$ENDIF}

function GetSystemThreadCount: integer;

procedure CallLocalProc(AProc, Frame: Pointer; Param1: PtrInt;
  Param2, Param3: Pointer); inline;

implementation

{$IFDEF Linux}
const _SC_NPROCESSORS_ONLN = 83;
function sysconf(i: cint): clong; cdecl; external name 'sysconf';
{$ENDIF}

function GetSystemThreadCount: integer;
// returns a good default for the number of threads on this system
{$IF defined(windows)}
//returns total number of processors available to system including logical hyperthreaded processors
var
  i: Integer;
  ProcessAffinityMask, SystemAffinityMask: DWORD_PTR;
  Mask: DWORD;
  SystemInfo: SYSTEM_INFO;
begin
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask)
  then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := DWord(1) shl i;
      if (ProcessAffinityMask and Mask)<>0 then
        inc(Result);
    end;
  end else begin
    //can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(SystemInfo);
    Result := SystemInfo.dwNumberOfProcessors;
  end;
end;
{$ELSEIF defined(UNTESTEDsolaris)}
  begin
    t = sysconf(_SC_NPROC_ONLN);
  end;
{$ELSEIF defined(freebsd) or defined(darwin)}
type
  PSysCtl = {$IF FPC_FULLVERSION>=30200}pcint{$ELSE}pchar{$ENDIF};
var
  mib: array[0..1] of cint;
  len: csize_t;
  t: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := HW_NCPU;
  len := sizeof(t);
  fpsysctl(PSysCtl(@mib), 2, @t, @len, Nil, 0);
  Result:=t;
end;
{$ELSEIF defined(linux)}
  begin
    Result:=sysconf(_SC_NPROCESSORS_ONLN);
  end;

{$ELSE}
  begin
    Result:=1;
  end;
{$ENDIF}

procedure CallLocalProc(AProc, Frame: Pointer; Param1: PtrInt;
  Param2, Param3: Pointer); inline;
type
  PointerLocal = procedure(_EBP: Pointer; Param1: PtrInt;
                           Param2, Param3: Pointer);
begin
  PointerLocal(AProc)(Frame, Param1, Param2, Param3);
end;

end.

