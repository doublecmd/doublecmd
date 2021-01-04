unit uLocalSockets;

{$mode objfpc}{$H+}
{$packrecords c}

interface

uses
  Classes, SysUtils, BaseUnix, Sockets;

function VerifyChild(Handle: THandle): Boolean;
function VerifyParent(Handle: THandle): Boolean;

procedure SetSocketClientProcessId({%H-}fd: cint);
function GetSocketClientProcessId(fd: cint): pid_t;

function SendHandle(sock: cint; fd: cint): Boolean;
function RecvHandle(sock: cint): cint;

function SocketDirectory: String;

implementation

uses
  InitC, uProcessInfo, uDebug;

const
  SCM_RIGHTS = $01;  //* Transfer file descriptors.  */

type
  msglen_t = {$IFDEF BSD}cint{$ELSE}size_t{$ENDIF};

  Pmsghdr = ^msghdr;
  msghdr = record
     msg_name : pointer;
     msg_namelen : socklen_t;
     msg_iov : piovec;
     msg_iovlen : msglen_t;
     msg_control : pointer;
     msg_controllen : msglen_t;
     msg_flags : cInt;
  end;

  Pcmsghdr = ^cmsghdr;
  cmsghdr = record
    cmsg_len   : msglen_t;
    cmsg_level : cInt;
    cmsg_type  : cInt;
  end;

function sendmsg(__fd: cInt; __message: pmsghdr; __flags: cInt): ssize_t; cdecl; external clib name 'sendmsg';
function recvmsg(__fd: cInt; __message: pmsghdr; __flags: cInt): ssize_t; cdecl; external clib name 'recvmsg';

{$IF DEFINED(LINUX)}

type
  ucred = record
    pid : pid_t;
    uid : uid_t;
    gid : gid_t;
  end;

{$ELSEIF DEFINED(DARWIN)}

const
  MSG_NOSIGNAL = $20000;
  LOCAL_PEERPID = $002; //* retrieve peer pid */
  SOL_LOCAL = 0; //* Level number of get/setsockopt for local domain sockets */

{$ELSEIF DEFINED(BSD)}

const
  SCM_CREDS = $03; //* process creds (struct cmsgcred) */

type
  Pcmsgcred = ^cmsgcred;
  cmsgcred = record
    cmcred_pid: pid_t;  //* PID of sending process */
    cmcred_uid: uid_t;  //* real UID of sending process */
    cmcred_euid: uid_t;  //* effective UID of sending process */
    cmcred_gid: gid_t;  //* real GID of sending process */
    cmcred_ngroups: cshort;  //* number or groups */
    cmcred_groups: array[0..15] of gid_t;  //* groups */
  end;

{$ENDIF}

const
{$IF DEFINED(DARWIN)}
  ALIGN_BYTES = csize_t(SizeOf(cuint32) - 1);
{$ELSE}
  ALIGN_BYTES = csize_t(SizeOf(csize_t) - 1);
{$ENDIF}

function CMSG_ALIGN(len: csize_t): csize_t; inline;
begin
  Result:= (((len) + ALIGN_BYTES) and (not (ALIGN_BYTES)));
end;

function CMSG_SPACE(len: csize_t): csize_t; inline;
begin
  Result:= (CMSG_ALIGN(len) + CMSG_ALIGN(SizeOf(cmsghdr)));
end;

function CMSG_LEN(len: csize_t): csize_t; inline;
begin
  Result:= (CMSG_ALIGN(SizeOf(cmsghdr)) + (len));
end;

function CMSG_DATA(cmsg: Pcmsghdr): PByte; inline;
{$IF DEFINED(BSD)}
begin
  Result:= PByte(cmsg) + CMSG_ALIGN(SizeOf(cmsghdr));
end;
{$ELSE}
begin
  Result:= PByte(cmsg + 1)
end;
{$ENDIF}

function SendMessage(__fd: cInt; __message: pmsghdr; __flags: cInt): ssize_t;
begin
  repeat
    Result:= sendmsg(__fd, __message, __flags);
  until (Result <> -1) or (fpgetCerrno <> ESysEINTR);
end;

function RecvMessage(__fd: cInt; __message: pmsghdr; __flags: cInt): ssize_t;
begin
  repeat
    Result:= recvmsg(__fd, __message, __flags);
  until (Result <> -1) or (fpgetCerrno <> ESysEINTR);
end;

procedure SetSocketClientProcessId(fd: cint);
{$IF DEFINED(LINUX) OR DEFINED(DARWIN)}
begin

end;
{$ELSE}
var
  buf: Byte;
  iov: iovec;
  msg: msghdr;
  cmsga: Pcmsghdr;
  nbytes: ssize_t;
  data: array[Byte] of Byte;
begin
  cmsga := Pcmsghdr(@data[0]);
  {*
   * The backend doesn't care what we send here, but it wants
   * exactly one character to force recvmsg() to block and wait
   * for us.
   *}
  buf := 0;
  iov.iov_base := @buf;
  iov.iov_len := 1;

  cmsga^.cmsg_len := CMSG_LEN(SizeOf(cmsgcred));
  cmsga^.cmsg_level := SOL_SOCKET;
  cmsga^.cmsg_type := SCM_CREDS;
  {*
  * cmsg.cred will get filled in with the correct information
  * by the kernel when this message is sent.
  *}
  msg.msg_name := nil;
  msg.msg_namelen := 0;
  msg.msg_iov := @iov;
  msg.msg_iovlen := 1;
  msg.msg_control := cmsga;
  msg.msg_controllen := CMSG_SPACE(SizeOf(cmsgcred));
  msg.msg_flags := MSG_NOSIGNAL;

  nbytes := SendMessage(fd, @msg, MSG_NOSIGNAL);
  if (nbytes = -1) then
    DCDebug('SendMessage: ', SysErrorMessage(fpgetCerrno));
end;
{$ENDIF}

function GetSocketClientProcessId(fd: cint): pid_t;
{$IF DEFINED(LINUX)}
var
  cred: ucred;
  ALength: TSockLen;
begin
  ALength:= SizeOf(ucred);
  if (fpgetsockopt(fd, SOL_SOCKET, SO_PEERCRED, @cred, @ALength) = -1) then
    Exit(-1);
  Result:= cred.pid;
end;
{$ELSEIF DEFINED(DARWIN)}
var
  ALength: TSockLen;
begin
  ALength:= SizeOf(Result);
  if (fpgetsockopt(fd, SOL_LOCAL, LOCAL_PEERPID, @Result, @ALength) = -1) then
    Exit(-1);
end;
{$ELSE}
var
  buf: Byte;
  iov: iovec;
  msg: msghdr;
  cmsga: Pcmsghdr;
  nbytes: ssize_t;
  data: array[Byte] of Byte;
begin
  cmsga := Pcmsghdr(@data[0]);

  msg.msg_name := nil;
  msg.msg_namelen := 0;
  msg.msg_iov := @iov;
  msg.msg_iovlen := 1;
  msg.msg_control := cmsga;
  msg.msg_controllen := CMSG_SPACE(SizeOf(cmsgcred));
  msg.msg_flags := MSG_NOSIGNAL;
  {*
  * The one character which is received here is not meaningful;
  * its purposes is only to make sure that recvmsg() blocks
  * long enough for the other side to send its credentials.
  *}
  iov.iov_base := @buf;
  iov.iov_len := 1;

  nbytes := RecvMessage(fd, @msg, MSG_NOSIGNAL);
  if (nbytes = -1) then
    DCDebug('RecvMessage: ', SysErrorMessage(fpgetCerrno));

  Result:= Pcmsgcred(CMSG_DATA(cmsga))^.cmcred_pid;
end;
{$ENDIF}

function SendHandle(sock: cint; fd: cint): Boolean;
var
  buf: Byte;
  iov: iovec;
  msg: msghdr;
  cmsga: Pcmsghdr;
  nbytes: ssize_t;
  data: array[Byte] of Byte;
begin
  cmsga := Pcmsghdr(@data[0]);
  {*
   * The backend doesn't care what we send here, but it wants
   * exactly one character to force recvmsg() to block and wait
   * for us.
   *}
  buf := 0;
  iov.iov_base := @buf;
  iov.iov_len := 1;

  cmsga^.cmsg_len := CMSG_LEN(SizeOf(fd));
  cmsga^.cmsg_level := SOL_SOCKET;
  cmsga^.cmsg_type := SCM_RIGHTS;
  {*
  * cmsg.cred will get filled in with the correct information
  * by the kernel when this message is sent.
  *}
  msg.msg_name := nil;
  msg.msg_namelen := 0;
  msg.msg_iov := @iov;
  msg.msg_iovlen := 1;
  msg.msg_control := cmsga;
  msg.msg_controllen := CMSG_SPACE(SizeOf(fd));
  msg.msg_flags := MSG_NOSIGNAL;

  Move(fd, CMSG_DATA(cmsga)^, SizeOf(fd));

  nbytes := SendMessage(sock, @msg, MSG_NOSIGNAL);
  if (nbytes = -1) then
    DCDebug('SendHandle: ', SysErrorMessage(fpgetCerrno));

  FileClose(fd);

  Result:= (nbytes > 0)
end;

function RecvHandle(sock: cint): cint;
var
  buf: Byte;
  iov: iovec;
  msg: msghdr;
  cmsga: Pcmsghdr;
  nbytes: ssize_t;
  data: array[Byte] of Byte;
begin
  cmsga := Pcmsghdr(@data[0]);

  msg.msg_name := nil;
  msg.msg_namelen := 0;
  msg.msg_iov := @iov;
  msg.msg_iovlen := 1;
  msg.msg_control := cmsga;
  msg.msg_controllen := CMSG_SPACE(SizeOf(Result));
  msg.msg_flags := MSG_NOSIGNAL;
  {*
  * The one character which is received here is not meaningful;
  * its purposes is only to make sure that recvmsg() blocks
  * long enough for the other side to send its credentials.
  *}
  iov.iov_base := @buf;
  iov.iov_len := 1;

  nbytes := RecvMessage(sock, @msg, MSG_NOSIGNAL);
  if (nbytes = -1) then
    DCDebug('RecvHandle: ', SysErrorMessage(fpgetCerrno));

  Move(CMSG_DATA(cmsga)^, Result, SizeOf(Result));
end;

function CheckParent(ProcessId, ParentId: pid_t): Boolean;
begin
  DCDebug(['ProcessId: ', ProcessId]);
  while (ProcessId <> ParentId) and (ProcessId > 1) do
  begin
    ProcessId:= GetParentProcessId(ProcessId);
    DCDebug(['ProcessId: ', ProcessId]);
  end;
  Result:= (ProcessId = ParentId);
end;

function VerifyChild(Handle: THandle): Boolean;
var
  ProcessId: pid_t;
begin
  DCDebug('VerifyChild');
  ProcessId:= GetSocketClientProcessId(Handle);
  DCDebug(['Credentials from socket: pid=', ProcessId]);
  Result:= CheckParent(ProcessId, GetProcessId);{ and
           (GetProcessFileName(ProcessId) = GetProcessFileName(GetProcessId));}

  DCDebug(['VerifyChild: ', Result]);
end;

function VerifyParent(Handle: THandle): Boolean;
var
  ProcessId: pid_t;
begin
  DCDebug('VerifyParent');
  ProcessId:= GetSocketClientProcessId(Handle);
  DCDebug(['Credentials from socket: pid=', ProcessId]);
  Result:= CheckParent(FpGetppid, ProcessId) and
           (GetProcessFileName(ProcessId) = GetProcessFileName(GetProcessId));
  DCDebug(['VerifyParent: ', Result]);
end;

function SocketDirectory: String;
var
  Stat: TStat;
  UserID: TUid;
begin
  UserID:= fpGetUID;
  if UserID = 0 then begin
    UserID:= GetProcessUserId(StrToInt(ParamStr(2)));
  end;
  Result:= GetTempDir + ApplicationName + '-' + IntToStr(UserID);
  // Verify directory owner
  if not DirectoryExists(Result) then
  begin
    if fpMkDir(Result, &700) <> 0 then
      RaiseLastOSError;
  end
  else begin
    if fpStat(Result, Stat) <> 0 then
      RaiseLastOSError;
    if (Stat.st_uid <> UserID) and (fpChown(Result, UserID, Stat.st_gid) < 0) then
      RaiseLastOSError;
    if ((Stat.st_mode and $0FFF) <> &700) and (fpChmod(Result, &700) < 0) then
      RaiseLastOSError;
  end;
  Result += PathDelim;
end;

end.

