unit uProcessInfo;

{$mode objfpc}{$H+}
{$packrecords c}

interface

uses
  Classes, SysUtils, BaseUnix;

function GetProcessUserId(ProcessId: pid_t): uid_t;
function GetParentProcessId(ProcessId: pid_t): pid_t;
function GetProcessFileName(ProcessId: pid_t): String;

implementation

uses
  InitC
{$IF DEFINED(FREEBSD)}
  , FreeBSD, SysCtl
{$ENDIF}
  ;

{$IF DEFINED(LINUX)}

function GetProcessUserId(ProcessId: pid_t): uid_t;
var
  Info: TStat;
begin
  if fpStat(Format('/proc/%d', [ProcessId]), Info) < 0 then
    Result:= 0
  else
    Result:= Info.st_uid;
end;

function GetParentProcessId(ProcessId: pid_t): pid_t;
var
  pid: pid_t;
  hFile: THandle;
  ABuffer: String;
  comm, state: String;
begin
  hFile:= FileOpen(Format('/proc/%d/stat', [ProcessId]), fmOpenRead or fmShareDenyNone);
  if hFile = feInvalidHandle then (Exit(-1));
  SetLength(ABuffer, MAX_PATH);
  Result:= FileRead(hFile, ABuffer[1], MAX_PATH);
  if (Result >= 0) then
  begin
    SetLength(ABuffer, Result);
    SScanf(ABuffer, '%d %s %s %d', [@pid, @comm, @state, @Result]);
  end;
  FileClose(hFile);
end;

function GetProcessFileName(ProcessId: pid_t): String;
begin
  Result:= fpReadLink(Format('/proc/%d/exe', [ProcessId]));
end;

{$ELSEIF DEFINED(DARWIN)}

const
  MAXCOMLEN = 16;
  PROC_PIDPATHINFO = 11;
  PROC_PIDT_SHORTBSDINFO = 13;

type
  Tproc_bsdshortinfo = record
    pbsi_pid:    UInt32;                             // Process identifier
    pbsi_ppid:   UInt32;                             // Parent process identifier
    pbsi_pgid:   UInt32;
    pbsi_status: UInt32;
    pbsi_comm:   array[0..MAXCOMLEN-1] of AnsiChar;  // Process name
    pbsi_flags:  UInt32;
    pbsi_uid:    uid_t;
    pbsi_gid:    gid_t;
    pbsi_ruid:   uid_t;
    pbsi_rgid:   gid_t;
    pbsi_svuid:  uid_t;
    pbsi_svgid:  gid_t;
    pbsi_rfu:    UInt32;
  end;

function proc_pidinfo(pid: cint; flavor: cint; arg: cuint64; buffer: pointer; buffersize: cint): cint; cdecl; external 'proc';

function GetProcessUserId(ProcessId: pid_t): uid_t;
var
  ret: cint;
  info: Tproc_bsdshortinfo;
begin
  ret:= proc_pidinfo(ProcessId, PROC_PIDT_SHORTBSDINFO, 0, @info, SizeOf(Tproc_bsdshortinfo));
  if (ret = SizeOf(Tproc_bsdshortinfo)) then
    Result:= info.pbsi_ruid
  else
    Result:= 0;
end;

function GetParentProcessId(ProcessId: pid_t): pid_t;
var
  ret: cint;
  info: Tproc_bsdshortinfo;
begin
  ret:= proc_pidinfo(ProcessId, PROC_PIDT_SHORTBSDINFO, 0, @info, SizeOf(Tproc_bsdshortinfo));
  if (ret = SizeOf(Tproc_bsdshortinfo)) then
    Result:= info.pbsi_ppid
  else
    Result:= -1;
end;

function GetProcessFileName(ProcessId: pid_t): String;
begin
  SetLength(Result, MAX_PATH + 1);
  if proc_pidinfo(ProcessId, PROC_PIDPATHINFO, 0, Pointer(Result), MAX_PATH) < 0 then
    SetLength(Result, 0);
end;

{$ELSEIF DEFINED(FREEBSD)}

type
  Tkinfo_proc = record
    ki_structsize:     cint;
    ki_layout:         cint;
    ki_args:           pointer;
    ki_paddr:          pointer;
    ki_addr:           pointer;
    ki_tracep:         pointer;
    ki_textvp:         pointer;
    ki_fd:             pointer;
    ki_vmspace:        pointer;
    ki_wchan:          pointer;
    ki_pid:            pid_t;    // Process identifier
    ki_ppid:           pid_t;    // Parent process identifier
    ki_pgid:           pid_t;
    ki_tpgid:          pid_t;
    ki_sid:            pid_t;
    ki_tsid:           pid_t;
    ki_jobc:           cshort;
    ki_spare_short1:   cshort;
    ki_tdev_freebsd11: cuint32;
    ki_siglist:        sigset_t;
    ki_sigmask:        sigset_t;
    ki_sigignore:      sigset_t;
    ki_sigcatch:       sigset_t;
    ki_uid:            uid_t;    // Effective user id
    ki_ruid:           uid_t;    // Real user id
    ki_reserved:   array[0..4095] of byte;
  end;

function GetProcessUserId(ProcessId: pid_t): uid_t;
var
  length: csize_t;
  info: Tkinfo_proc;
  mib: array[0..3] of cint = (CTL_KERN, KERN_PROC, KERN_PROC_PID, 0);
begin
  mib[3] := ProcessId;
  length := SizeOf(Tkinfo_proc);
  if (FPsysctl(@mib, 4, @info, @length, nil, 0) < 0) then Exit(0);
  if (length = 0) then Exit(0);
  Result:= info.ki_ruid;
end;

function GetParentProcessId(ProcessId: pid_t): pid_t;
var
  length: csize_t;
  info: Tkinfo_proc;
  mib: array[0..3] of cint = (CTL_KERN, KERN_PROC, KERN_PROC_PID, 0);
begin
  mib[3] := ProcessId;
  length := SizeOf(Tkinfo_proc);
  if (FPsysctl(@mib, 4, @info, @length, nil, 0) < 0) then Exit(-1);
  if (length = 0) then Exit(-1);
  Result:= info.ki_ppid;
end;

function GetProcessFileName(ProcessId: pid_t): String;
begin
  kernproc_getpath(ProcessId, Result);
end;

{$ELSE}

function GetProcessUserId(ProcessId: pid_t): uid_t;
begin
  Result:= 0;
end;

function GetParentProcessId(ProcessId: pid_t): pid_t;
begin
  Result:= -1;
end;

function GetProcessFileName(ProcessId: pid_t): String;
begin
  Result:= EmptyStr;
end;

{$ENDIF}

end.
