unit libssh;

{$mode delphi}
{$packrecords c}

interface

uses
  Classes, SysUtils, CTypes;

const
  //* Hash Types */
  LIBSSH2_HOSTKEY_HASH_MD5   = 1;
  LIBSSH2_HOSTKEY_HASH_SHA1  = 2;

  //* Disconnect Codes (defined by SSH protocol) */
  SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT    = 1;
  SSH_DISCONNECT_PROTOCOL_ERROR                 = 2;
  SSH_DISCONNECT_KEY_EXCHANGE_FAILED            = 3;
  SSH_DISCONNECT_RESERVED                       = 4;
  SSH_DISCONNECT_MAC_ERROR                      = 5;
  SSH_DISCONNECT_COMPRESSION_ERROR              = 6;
  SSH_DISCONNECT_SERVICE_NOT_AVAILABLE          = 7;
  SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED = 8;
  SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE        = 9;
  SSH_DISCONNECT_CONNECTION_LOST                = 10;
  SSH_DISCONNECT_BY_APPLICATION                 = 11;
  SSH_DISCONNECT_TOO_MANY_CONNECTIONS           = 12;
  SSH_DISCONNECT_AUTH_CANCELLED_BY_USER         = 13;
  SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE = 14;
  SSH_DISCONNECT_ILLEGAL_USER_NAME              = 15;

  { Error Codes (defined by libssh2)  }
  LIBSSH2_ERROR_NONE = 0;
  LIBSSH2_ERROR_SOCKET_NONE = -(1);
  LIBSSH2_ERROR_BANNER_RECV = -(2);
  LIBSSH2_ERROR_BANNER_SEND = -(3);
  LIBSSH2_ERROR_INVALID_MAC = -(4);
  LIBSSH2_ERROR_KEX_FAILURE = -(5);
  LIBSSH2_ERROR_ALLOC = -(6);
  LIBSSH2_ERROR_SOCKET_SEND = -(7);
  LIBSSH2_ERROR_KEY_EXCHANGE_FAILURE = -(8);
  LIBSSH2_ERROR_TIMEOUT = -(9);
  LIBSSH2_ERROR_HOSTKEY_INIT = -(10);
  LIBSSH2_ERROR_HOSTKEY_SIGN = -(11);
  LIBSSH2_ERROR_DECRYPT = -(12);
  LIBSSH2_ERROR_SOCKET_DISCONNECT = -(13);
  LIBSSH2_ERROR_PROTO = -(14);
  LIBSSH2_ERROR_PASSWORD_EXPIRED = -(15);
  LIBSSH2_ERROR_FILE = -(16);
  LIBSSH2_ERROR_METHOD_NONE = -(17);
  LIBSSH2_ERROR_AUTHENTICATION_FAILED = -(18);
  LIBSSH2_ERROR_PUBLICKEY_UNRECOGNIZED = LIBSSH2_ERROR_AUTHENTICATION_FAILED;
  LIBSSH2_ERROR_PUBLICKEY_UNVERIFIED = -(19);
  LIBSSH2_ERROR_CHANNEL_OUTOFORDER = -(20);
  LIBSSH2_ERROR_CHANNEL_FAILURE = -(21);
  LIBSSH2_ERROR_CHANNEL_REQUEST_DENIED = -(22);
  LIBSSH2_ERROR_CHANNEL_UNKNOWN = -(23);
  LIBSSH2_ERROR_CHANNEL_WINDOW_EXCEEDED = -(24);
  LIBSSH2_ERROR_CHANNEL_PACKET_EXCEEDED = -(25);
  LIBSSH2_ERROR_CHANNEL_CLOSED = -(26);
  LIBSSH2_ERROR_CHANNEL_EOF_SENT = -(27);
  LIBSSH2_ERROR_SCP_PROTOCOL = -(28);
  LIBSSH2_ERROR_ZLIB = -(29);
  LIBSSH2_ERROR_SOCKET_TIMEOUT = -(30);
  LIBSSH2_ERROR_SFTP_PROTOCOL = -(31);
  LIBSSH2_ERROR_REQUEST_DENIED = -(32);
  LIBSSH2_ERROR_METHOD_NOT_SUPPORTED = -(33);
  LIBSSH2_ERROR_INVAL = -(34);
  LIBSSH2_ERROR_INVALID_POLL_TYPE = -(35);
  LIBSSH2_ERROR_PUBLICKEY_PROTOCOL = -(36);
  LIBSSH2_ERROR_EAGAIN = -(37);
  LIBSSH2_ERROR_BUFFER_TOO_SMALL = -(38);
  LIBSSH2_ERROR_BAD_USE = -(39);
  LIBSSH2_ERROR_COMPRESS = -(40);
  LIBSSH2_ERROR_OUT_OF_BOUNDARY = -(41);
  LIBSSH2_ERROR_AGENT_PROTOCOL = -(42);
  LIBSSH2_ERROR_SOCKET_RECV = -(43);
  LIBSSH2_ERROR_ENCRYPT = -(44);
  LIBSSH2_ERROR_BAD_SOCKET = -(45);
  LIBSSH2_ERROR_KNOWN_HOSTS = -(46);

  //* Flags for open_ex() */
  _LIBSSH2_SFTP_OPENFILE                        = 0;
  _LIBSSH2_SFTP_OPENDIR                         = 1;

  //* Flags for rename_ex() */
  LIBSSH2_SFTP_RENAME_OVERWRITE                 = $00000001;
  LIBSSH2_SFTP_RENAME_ATOMIC                    = $00000002;
  LIBSSH2_SFTP_RENAME_NATIVE                    = $00000004;

  //* Flags for stat_ex() */
  _LIBSSH2_SFTP_STAT                            = 0;
  _LIBSSH2_SFTP_LSTAT                           = 1;
  _LIBSSH2_SFTP_SETSTAT                         = 2;

  //* Flags for symlink_ex() */
  _LIBSSH2_SFTP_SYMLINK                         = 0;
  _LIBSSH2_SFTP_READLINK                        = 1;
  _LIBSSH2_SFTP_REALPATH                        = 2;

  //* SFTP attribute flag bits */
  LIBSSH2_SFTP_ATTR_SIZE                        = $00000001;
  LIBSSH2_SFTP_ATTR_UIDGID                      = $00000002;
  LIBSSH2_SFTP_ATTR_PERMISSIONS                 = $00000004;
  LIBSSH2_SFTP_ATTR_ACMODTIME                   = $00000008;
  LIBSSH2_SFTP_ATTR_EXTENDED                    = $80000000;

  //* File mode */
  //* Read, write, execute/search by owner */
  LIBSSH2_SFTP_S_IRWXU        = 448;     //* RWX mask for owner */
  LIBSSH2_SFTP_S_IRUSR        = 256;     //* R for owner */
  LIBSSH2_SFTP_S_IWUSR        = 128;     //* W for owner */
  LIBSSH2_SFTP_S_IXUSR        = 64;      //* X for owner */
  //* Read, write, execute/search by group */
  LIBSSH2_SFTP_S_IRWXG        = 56;      //* RWX mask for group */
  LIBSSH2_SFTP_S_IRGRP        = 32;      //* R for group */
  LIBSSH2_SFTP_S_IWGRP        = 16;      //* W for group */
  LIBSSH2_SFTP_S_IXGRP        = 8;       //* X for group */
  //* Read, write, execute/search by others */
  LIBSSH2_SFTP_S_IRWXO        = 7;       //* RWX mask for other */
  LIBSSH2_SFTP_S_IROTH        = 4;       //* R for other */
  LIBSSH2_SFTP_S_IWOTH        = 2;       //* W for other */
  LIBSSH2_SFTP_S_IXOTH        = 1;       //* X for other */

  //* SFTP File Transfer Flags -- (e.g. flags parameter to sftp_open()) */
  LIBSSH2_FXF_READ            = $00000001;
  LIBSSH2_FXF_WRITE           = $00000002;
  LIBSSH2_FXF_APPEND          = $00000004;
  LIBSSH2_FXF_CREAT           = $00000008;
  LIBSSH2_FXF_TRUNC           = $00000010;
  LIBSSH2_FXF_EXCL            = $00000020;

type
  //* Session API */
  PLIBSSH2_SESSION = type Pointer;
  //* SFTP API */
  PLIBSSH2_SFTP = type Pointer;
  PLIBSSH2_SFTP_HANDLE = type Pointer;
  PLIBSSH2_SFTP_ATTRIBUTES = ^LIBSSH2_SFTP_ATTRIBUTES;
  LIBSSH2_SFTP_ATTRIBUTES = record
    flags: culong;
    filesize: cuint64;
    uid, gid: culong;
    permissions: culong;
    atime, mtime: culong;
  end;
  PLIBSSH2_SFTP_STATVFS = ^_LIBSSH2_SFTP_STATVFS;
  _LIBSSH2_SFTP_STATVFS = record
      f_bsize: cuint64;    //* file system block size */
      f_frsize: cuint64;   //* fragment size */
      f_blocks: cuint64;   //* size of fs in f_frsize units */
      f_bfree: cuint64;    //* # free blocks */
      f_bavail: cuint64;   //* # free blocks for non-root */
      f_files: cuint64;    //* # inodes */
      f_ffree: cuint64;    //* # free inodes */
      f_favail: cuint64;   //* # free inodes for non-root */
      f_fsid: cuint64;     //* file system ID */
      f_flag: cuint64;     //* mount flags */
      f_namemax: cuint64;  //* maximum filename length */
  end;
  PLIBSSH2_USERAUTH_KBDINT_PROMPT = ^LIBSSH2_USERAUTH_KBDINT_PROMPT;
  LIBSSH2_USERAUTH_KBDINT_PROMPT = record
      text: PAnsiChar;
      length: cuint;
      echo: cuchar;
  end;
  PLIBSSH2_USERAUTH_KBDINT_RESPONSE = ^LIBSSH2_USERAUTH_KBDINT_RESPONSE;
  LIBSSH2_USERAUTH_KBDINT_RESPONSE = record
    text: PAnsiChar;
    length: cuint;
  end;
  //* Malloc callbacks */
  LIBSSH2_ALLOC_FUNC = function(count: csize_t; abstract: Pointer): Pointer; cdecl;
  LIBSSH2_REALLOC_FUNC = function(ptr: Pointer; count: csize_t; abstract: Pointer): Pointer; cdecl;
  LIBSSH2_FREE_FUNC = procedure(ptr: Pointer; abstract: Pointer); cdecl;
  //* Callbacks for special SSH packets */
  LIBSSH2_PASSWD_CHANGEREQ_FUNC = procedure(session: PLIBSSH2_SESSION; var newpw: PAnsiChar;
                                            var newpw_len: cint; abstract: Pointer); cdecl;
  //* 'keyboard-interactive' authentication callback */
  LIBSSH2_USERAUTH_KBDINT_RESPONSE_FUNC = procedure(const name: PAnsiChar; name_len: cint;
                                            const instruction: PAnsiChar; instruction_len: cint;
                                            num_prompts: cint; const prompts: PLIBSSH2_USERAUTH_KBDINT_PROMPT;
                                            responses: PLIBSSH2_USERAUTH_KBDINT_RESPONSE; abstract: PPointer); cdecl;

var
  //* Session API */
  libssh2_session_init_ex: function(my_alloc: LIBSSH2_ALLOC_FUNC;
                                    my_free: LIBSSH2_FREE_FUNC;
                                    my_realloc: LIBSSH2_REALLOC_FUNC;
                                    abstract: Pointer): PLIBSSH2_SESSION; cdecl;
  libssh2_session_handshake: function(session: PLIBSSH2_SESSION; sock: cint): cint; cdecl;
  libssh2_hostkey_hash: function(session: PLIBSSH2_SESSION; hash_type: cint): PAnsiChar; cdecl;
  libssh2_session_disconnect_ex: function(session: PLIBSSH2_SESSION;
                                          reason: cint;
                                          const description: PAnsiChar;
                                          const lang: PAnsiChar): cint; cdecl;
  libssh2_session_free: function(session: PLIBSSH2_SESSION): cint; cdecl;
  libssh2_session_set_blocking: procedure(session: PLIBSSH2_SESSION; blocking: cint); cdecl;
  libssh2_session_last_errno: function(session: PLIBSSH2_SESSION): cint; cdecl;
  libssh2_session_set_timeout: procedure(session: PLIBSSH2_SESSION; timeout: clong); cdecl;
  //* Userauth API */
  libssh2_userauth_list: function(session: PLIBSSH2_SESSION;
                                  const username: PAnsiChar; username_len: cuint): PAnsiChar; cdecl;
  libssh2_userauth_password_ex: function(session: PLIBSSH2_SESSION;
                                         const username: PAnsiChar;
                                         username_len: cuint;
                                         const password: PAnsiChar;
                                         password_len: cuint;
                                         passwd_change_cb: LIBSSH2_PASSWD_CHANGEREQ_FUNC): cint; cdecl;
  libssh2_userauth_keyboard_interactive_ex: function(session: PLIBSSH2_SESSION;
                                                     const username: PAnsiChar;
                                                     username_len: cuint;
                                                     response_callback: LIBSSH2_USERAUTH_KBDINT_RESPONSE_FUNC): cint; cdecl;
  //* SFTP API */
  libssh2_sftp_init: function(session: PLIBSSH2_SESSION): PLIBSSH2_SFTP; cdecl;
  libssh2_sftp_shutdown: function(sftp: PLIBSSH2_SFTP): cint; cdecl;
  libssh2_sftp_last_error: function(sftp: PLIBSSH2_SFTP): culong; cdecl;
  //* File / Directory Ops */
  libssh2_sftp_open_ex: function(sftp: PLIBSSH2_SFTP;
                                 const filename: PAnsiChar;
                                 filename_len: cint; flags: culong;
                                 mode: clong; open_type: cint): PLIBSSH2_SFTP_HANDLE; cdecl;
  libssh2_sftp_read: function(handle: PLIBSSH2_SFTP_HANDLE;
                              buffer: PAnsiChar; buffer_maxlen: csize_t): ptrint; cdecl;
  libssh2_sftp_write: function(handle: PLIBSSH2_SFTP_HANDLE;
                               buffer: PByte; count: csize_t): ptrint; cdecl;
  libssh2_sftp_readdir_ex: function(handle: PLIBSSH2_SFTP_HANDLE;
                                    buffer: PAnsiChar; buffer_maxlen: csize_t;
                                   longentry: PAnsiChar; longentry_maxlen: csize_t;
                                    attrs: PLIBSSH2_SFTP_ATTRIBUTES): cint; cdecl;
  libssh2_sftp_close_handle: function(handle: PLIBSSH2_SFTP_HANDLE): cint; cdecl;
  libssh2_sftp_seek64: procedure(handle: PLIBSSH2_SFTP_HANDLE; offset: cuint64); cdecl;
  //* Miscellaneous Ops */
  libssh2_sftp_rename_ex: function(sftp: PLIBSSH2_SFTP;
                                   const source_filename: PAnsiChar;
                                   srouce_filename_len: cuint;
                                   const dest_filename: PAnsiChar;
                                   dest_filename_len: cuint;
                                   flags: clong): cint; cdecl;
  libssh2_sftp_unlink_ex: function(sftp: PLIBSSH2_SFTP;
                                   const filename: PAnsiChar;
                                   filename_len: cuint): cint; cdecl;
  libssh2_sftp_statvfs: function(sftp: PLIBSSH2_SFTP;
                                 const path: PAnsiChar;
                                 path_len: csize_t;
                                 st: PLIBSSH2_SFTP_STATVFS): cint; cdecl;
  libssh2_sftp_mkdir_ex: function(sftp: PLIBSSH2_SFTP;
                                  const path: PAnsiChar;
                                  path_len: cuint; mode: clong): cint; cdecl;
  libssh2_sftp_rmdir_ex: function(sftp: PLIBSSH2_SFTP;
                                  const path: PAnsiChar;
                                  path_len: cuint): cint; cdecl;
  libssh2_sftp_stat_ex: function(sftp: PLIBSSH2_SFTP;
                                 const path: PAnsiChar;
                                 path_len: cuint;
                                 stat_type: cint;
                                 attrs: PLIBSSH2_SFTP_ATTRIBUTES): cint; cdecl;
  libssh2_sftp_symlink_ex: function(sftp: PLIBSSH2_SFTP;
                                    const path: PAnsiChar;
                                    path_len: cuint;
                                    target: PAnsiChar;
                                    target_len: cuint; link_type: cint): cint; cdecl;

  //* Inline functions */
  function libssh2_session_init: PLIBSSH2_SESSION; inline;
  function libssh2_session_disconnect(session: PLIBSSH2_SESSION; const description: PAnsiChar): cint; inline;
  function libssh2_userauth_password(session: PLIBSSH2_SESSION; const username: PAnsiChar; const password: PAnsiChar): cint; inline;
  function libssh2_userauth_keyboard_interactive(session: PLIBSSH2_SESSION; const username: PAnsiChar; response_callback: LIBSSH2_USERAUTH_KBDINT_RESPONSE_FUNC): cint; inline;
  function libssh2_sftp_open(sftp: PLIBSSH2_SFTP; const filename: PAnsiChar; flags: culong; mode: clong): PLIBSSH2_SFTP_HANDLE; inline;
  function libssh2_sftp_opendir(sftp: PLIBSSH2_SFTP; const path: PAnsiChar): PLIBSSH2_SFTP_HANDLE; inline;
  function libssh2_sftp_close(handle: PLIBSSH2_SFTP_HANDLE): cint; inline;
  function libssh2_sftp_closedir(handle: PLIBSSH2_SFTP_HANDLE): cint; inline;
  function libssh2_sftp_rename(sftp: PLIBSSH2_SFTP; const sourcefile: PAnsiChar; const destfile: PAnsiChar): cint; inline;
  function libssh2_sftp_unlink(sftp: PLIBSSH2_SFTP; const filename: PAnsiChar): cint; inline;
  function libssh2_sftp_mkdir(sftp: PLIBSSH2_SFTP; const path: PAnsiChar; mode: clong): cint; inline;
  function libssh2_sftp_rmdir(sftp: PLIBSSH2_SFTP; const path: PAnsiChar): cint; inline;
  function libssh2_sftp_stat(sftp: PLIBSSH2_SFTP; const path: PAnsiChar; attrs: PLIBSSH2_SFTP_ATTRIBUTES): cint; inline;
  function libssh2_sftp_lstat(sftp: PLIBSSH2_SFTP; const path: PAnsiChar; attrs: PLIBSSH2_SFTP_ATTRIBUTES): cint; inline;
  function libssh2_sftp_setstat(sftp: PLIBSSH2_SFTP; const path: PAnsiChar; attrs: PLIBSSH2_SFTP_ATTRIBUTES): cint; inline;
  function libssh2_sftp_symlink(sftp: PLIBSSH2_SFTP; const orig: PAnsiChar; linkpath: PAnsiChar): cint; inline;
  function libssh2_sftp_readlink(sftp: PLIBSSH2_SFTP; const path: PAnsiChar; target: PAnsiChar; maxlen: cuint): cint; inline;
  function libssh2_sftp_realpath(sftp: PLIBSSH2_SFTP; const path: PAnsiChar; target: PAnsiChar; maxlen: cuint): cint; inline;

implementation

uses
  DynLibs;

function libssh2_session_init: PLIBSSH2_SESSION;
begin
  Result:= libssh2_session_init_ex(nil, nil, nil, nil);
end;

function libssh2_session_disconnect(session: PLIBSSH2_SESSION; const description: PAnsiChar): cint;
begin
  Result:= libssh2_session_disconnect_ex(session, SSH_DISCONNECT_BY_APPLICATION,
                                         description, '');
end;

function libssh2_userauth_password(session: PLIBSSH2_SESSION;
  const username: PAnsiChar; const password: PAnsiChar): cint;
begin
  Result:= libssh2_userauth_password_ex(session, username, strlen(username),
                                        password, strlen(password), nil);
end;

function libssh2_userauth_keyboard_interactive(session: PLIBSSH2_SESSION;
  const username: PAnsiChar;
  response_callback: LIBSSH2_USERAUTH_KBDINT_RESPONSE_FUNC): cint;
begin
  Result:= libssh2_userauth_keyboard_interactive_ex(session, username, strlen(username), response_callback);
end;

function libssh2_sftp_open(sftp: PLIBSSH2_SFTP; const filename: PAnsiChar;
  flags: culong; mode: clong): PLIBSSH2_SFTP_HANDLE;
begin
  Result:= libssh2_sftp_open_ex(sftp, filename, strlen(filename), flags, mode, _LIBSSH2_SFTP_OPENFILE);
end;

function libssh2_sftp_opendir(sftp: PLIBSSH2_SFTP; const path: PAnsiChar): PLIBSSH2_SFTP_HANDLE;
begin
  Result:= libssh2_sftp_open_ex(sftp, path, strlen(path), 0, 0, _LIBSSH2_SFTP_OPENDIR);
end;

function libssh2_sftp_close(handle: PLIBSSH2_SFTP_HANDLE): cint;
begin
  Result:= libssh2_sftp_close_handle(handle);
end;

function libssh2_sftp_closedir(handle: PLIBSSH2_SFTP_HANDLE): cint;
begin
  Result:= libssh2_sftp_close_handle(handle);
end;

function libssh2_sftp_rename(sftp: PLIBSSH2_SFTP; const sourcefile: PAnsiChar; const destfile: PAnsiChar): cint;
begin
  Result:= libssh2_sftp_rename_ex(sftp, sourcefile, strlen(sourcefile),
                                  destfile, strlen(destfile),
                                  LIBSSH2_SFTP_RENAME_OVERWRITE or
                                  LIBSSH2_SFTP_RENAME_ATOMIC or
                                  LIBSSH2_SFTP_RENAME_NATIVE);
end;

function libssh2_sftp_unlink(sftp: PLIBSSH2_SFTP; const filename: PAnsiChar): cint;
begin
  Result:= libssh2_sftp_unlink_ex(sftp, filename, strlen(filename));
end;

function libssh2_sftp_mkdir(sftp: PLIBSSH2_SFTP; const path: PAnsiChar; mode: clong): cint;
begin
  Result:= libssh2_sftp_mkdir_ex(sftp, path, strlen(path), mode);
end;

function libssh2_sftp_rmdir(sftp: PLIBSSH2_SFTP; const path: PAnsiChar): cint;
begin
  Result:= libssh2_sftp_rmdir_ex(sftp, path, strlen(path));
end;

function libssh2_sftp_stat(sftp: PLIBSSH2_SFTP; const path: PAnsiChar;
  attrs: PLIBSSH2_SFTP_ATTRIBUTES): cint;
begin
  Result:= libssh2_sftp_stat_ex(sftp, path, strlen(path), _LIBSSH2_SFTP_STAT, attrs);
end;

function libssh2_sftp_lstat(sftp: PLIBSSH2_SFTP; const path: PAnsiChar;
  attrs: PLIBSSH2_SFTP_ATTRIBUTES): cint;
begin
  Result:= libssh2_sftp_stat_ex(sftp, path, strlen(path), _LIBSSH2_SFTP_LSTAT, attrs);
end;

function libssh2_sftp_setstat(sftp: PLIBSSH2_SFTP; const path: PAnsiChar;
  attrs: PLIBSSH2_SFTP_ATTRIBUTES): cint;
begin
  repeat
    Result:= libssh2_sftp_stat_ex(sftp, path, strlen(path), _LIBSSH2_SFTP_SETSTAT, attrs);
    Sleep(1);
  until Result <> LIBSSH2_ERROR_EAGAIN;
end;

function libssh2_sftp_symlink(sftp: PLIBSSH2_SFTP; const orig: PAnsiChar;
  linkpath: PAnsiChar): cint;
begin
  Result:= libssh2_sftp_symlink_ex(sftp, orig, strlen(orig), linkpath, strlen(linkpath), _LIBSSH2_SFTP_SYMLINK);
end;

function libssh2_sftp_readlink(sftp: PLIBSSH2_SFTP; const path: PAnsiChar;
  target: PAnsiChar; maxlen: cuint): cint;
begin
  Result:= libssh2_sftp_symlink_ex(sftp, path, strlen(path), target, maxlen, _LIBSSH2_SFTP_READLINK)
end;

function libssh2_sftp_realpath(sftp: PLIBSSH2_SFTP; const path: PAnsiChar;
  target: PAnsiChar; maxlen: cuint): cint;
begin
  Result:= libssh2_sftp_symlink_ex(sftp, path, strlen(path), target, maxlen, _LIBSSH2_SFTP_REALPATH);
end;

var
  libssh2: TLibHandle = NilHandle;

function SafeGetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : Pointer;
begin
  Result:= GetProcedureAddress(Lib, ProcName);
  if (Result = nil) then raise Exception.Create(EmptyStr);
end;

procedure Initialize;
begin
  libssh2:= LoadLibrary('libssh2.dll');
  if (libssh2 <> NilHandle) then
  try
    //* Session API */
    libssh2_session_init_ex:= SafeGetProcAddress(libssh2, 'libssh2_session_init_ex');
    libssh2_session_handshake:= SafeGetProcAddress(libssh2, 'libssh2_session_handshake');
    libssh2_hostkey_hash:= SafeGetProcAddress(libssh2, 'libssh2_hostkey_hash');
    libssh2_session_disconnect_ex:= SafeGetProcAddress(libssh2, 'libssh2_session_disconnect_ex');
    libssh2_session_free:= SafeGetProcAddress(libssh2, 'libssh2_session_free');
    libssh2_session_set_blocking:= SafeGetProcAddress(libssh2, 'libssh2_session_set_blocking');
    libssh2_session_last_errno:= SafeGetProcAddress(libssh2, 'libssh2_session_last_errno');
    libssh2_session_set_timeout:= SafeGetProcAddress(libssh2, 'libssh2_session_set_timeout');
    //* Userauth API */
    libssh2_userauth_list:= SafeGetProcAddress(libssh2, 'libssh2_userauth_list');
    libssh2_userauth_password_ex:= SafeGetProcAddress(libssh2, 'libssh2_userauth_password_ex');
    libssh2_userauth_keyboard_interactive_ex:= SafeGetProcAddress(libssh2, 'libssh2_userauth_keyboard_interactive_ex');
    //* SFTP API */
    libssh2_sftp_init:= SafeGetProcAddress(libssh2, 'libssh2_sftp_init');
    libssh2_sftp_shutdown:= SafeGetProcAddress(libssh2, 'libssh2_sftp_shutdown');
    libssh2_sftp_last_error:= SafeGetProcAddress(libssh2, 'libssh2_sftp_last_error');
    //* File / Directory Ops */
    libssh2_sftp_open_ex:= SafeGetProcAddress(libssh2, 'libssh2_sftp_open_ex');
    libssh2_sftp_read:= SafeGetProcAddress(libssh2, 'libssh2_sftp_read');
    libssh2_sftp_write:= SafeGetProcAddress(libssh2, 'libssh2_sftp_write');
    libssh2_sftp_readdir_ex:= SafeGetProcAddress(libssh2, 'libssh2_sftp_readdir_ex');
    libssh2_sftp_close_handle:= SafeGetProcAddress(libssh2, 'libssh2_sftp_close_handle');
    libssh2_sftp_seek64:= SafeGetProcAddress(libssh2, 'libssh2_sftp_seek64');
    //* Miscellaneous Ops */
    libssh2_sftp_rename_ex:= SafeGetProcAddress(libssh2, 'libssh2_sftp_rename_ex');
    libssh2_sftp_unlink_ex:= SafeGetProcAddress(libssh2, 'libssh2_sftp_unlink_ex');
    libssh2_sftp_statvfs:= SafeGetProcAddress(libssh2, 'libssh2_sftp_statvfs');
    libssh2_sftp_mkdir_ex:= SafeGetProcAddress(libssh2, 'libssh2_sftp_mkdir_ex');
    libssh2_sftp_rmdir_ex:= SafeGetProcAddress(libssh2, 'libssh2_sftp_rmdir_ex');
    libssh2_sftp_stat_ex:= SafeGetProcAddress(libssh2, 'libssh2_sftp_stat_ex');
    libssh2_sftp_symlink_ex:= SafeGetProcAddress(libssh2, 'libssh2_sftp_symlink_ex');

  except
  end;
end;

initialization
  Initialize;

finalization
  if (libssh2 <> NilHandle) then FreeLibrary(libssh2);

end.

