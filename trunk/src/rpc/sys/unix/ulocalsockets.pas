unit uLocalSockets;

{$mode objfpc}{$H+}
{$packrecords c}

interface

uses
  Classes, SysUtils, BaseUnix, Sockets;

function VerifyChild(Handle: THandle): Boolean;
function VerifyParent(Handle: THandle): Boolean;

procedure SetSocketClientProcessId(fd: cint);
function GetSocketClientProcessId(fd: cint): pid_t;

function SendHandle(sock: cint; fd: cint): Boolean;
function RecvHandle(sock: cint): cint;

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
  SCM_CREDS = 	$03;		//* process creds (struct cmsgcred) */

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

  cmsg = record
    hdr: cmsghdr;
    cred: cmsgcred;
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
  cmsga: cmsg;
  nbytes: ssize_t;
begin
  {*
   * The backend doesn't care what we send here, but it wants
   * exactly one character to force recvmsg() to block and wait
   * for us.
   *}
   buf := 0;
   iov.iov_base := @buf;
   iov.iov_len := 1;

   cmsga.hdr.cmsg_len := sizeof(cmsg);
   cmsga.hdr.cmsg_level := SOL_SOCKET;
   cmsga.hdr.cmsg_type := SCM_CREDS;
   {*
   * cmsg.cred will get filled in with the correct information
   * by the kernel when this message is sent.
   *}
   msg.msg_name := nil;
   msg.msg_namelen := 0;
   msg.msg_iov := @iov;
   msg.msg_iovlen := 1;
   msg.msg_control := @cmsga;
   msg.msg_controllen := sizeof(cmsga);
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
  cmsga: cmsg;
  nbytes: ssize_t;
begin
  msg.msg_name := nil;
  msg.msg_namelen := 0;
  msg.msg_iov := @iov;
  msg.msg_iovlen := 1;
  msg.msg_control := @cmsga;
  msg.msg_controllen := sizeof(cmsga);
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

  Result:= cmsga.cred.cmcred_pid;
end;
{$ENDIF}

type
  fdmsg = record
    hdr: cmsghdr;
    fd: cint;
  end;

function SendHandle(sock: cint; fd: cint): Boolean;
var
  buf: Byte;
  iov: iovec;
  msg: msghdr;
  cmsga: fdmsg;
  nbytes: ssize_t;
begin
  {*
   * The backend doesn't care what we send here, but it wants
   * exactly one character to force recvmsg() to block and wait
   * for us.
   *}
   buf := 0;
   iov.iov_base := @buf;
   iov.iov_len := 1;

   cmsga.hdr.cmsg_len := sizeof(fdmsg);
   cmsga.hdr.cmsg_level := SOL_SOCKET;
   cmsga.hdr.cmsg_type := SCM_RIGHTS;
   {*
   * cmsg.cred will get filled in with the correct information
   * by the kernel when this message is sent.
   *}
   msg.msg_name := nil;
   msg.msg_namelen := 0;
   msg.msg_iov := @iov;
   msg.msg_iovlen := 1;
   msg.msg_control := @cmsga;
   msg.msg_controllen := sizeof(cmsga);
   msg.msg_flags := MSG_NOSIGNAL;

   cmsga.fd:= fd;

  nbytes := SendMessage(sock, @msg, MSG_NOSIGNAL);
  if (nbytes = -1) then
    DCDebug('SendHandle: ', SysErrorMessage(fpgetCerrno));

  FileClose(fd);
end;

function RecvHandle(sock: cint): cint;
var
  buf: Byte;
  iov: iovec;
  msg: msghdr;
  cmsga: fdmsg;
  nbytes: ssize_t;
begin
  msg.msg_name := nil;
  msg.msg_namelen := 0;
  msg.msg_iov := @iov;
  msg.msg_iovlen := 1;
  msg.msg_control := @cmsga;
  msg.msg_controllen := sizeof(cmsga);
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

  Result:= cmsga.fd;
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

end.

