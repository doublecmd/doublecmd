{
   Double commander
   -------------------------------------------------------------------------
   This unit contains Unix specific functions

   Copyright (C) 2015-2023 Alexander Koblov (alexx2000@mail.ru)

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

   Notes:
   1. TDarwinStat64 is the workaround for the bug of BaseUnix.Stat in FPC.
      on MacOS with x86_64, Stat64 should be used instead of Stat.
      and lstat64() should be called instead of lstat().
}

unit DCUnix;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$packrecords c}

interface

uses
  InitC, BaseUnix, UnixType, DCBasicTypes, SysUtils;

const
{$IF DEFINED(LINUX)}
  FD_CLOEXEC = 1;
  O_CLOEXEC  = &02000000;
{$ELSEIF DEFINED(FREEBSD)}
  O_CLOEXEC  = &04000000;
{$ELSEIF DEFINED(NETBSD)}
  O_CLOEXEC  = $00400000;
{$ELSEIF DEFINED(HAIKU)}
  FD_CLOEXEC = 1;
  O_CLOEXEC  = $00000040;
{$ELSE}
  O_CLOEXEC  = 0;
{$ENDIF}

{$IF DEFINED(LINUX)}
  {$I dclinuxmagic.inc}
{$ENDIF}

type
{$IF DEFINED(LINUX)}
  TUnixTime =
    {$IF DEFINED(CPUAARCH64)}
      Int64
    {$ELSEIF DEFINED(CPUMIPS)}
      LongInt
    {$ELSE}UIntPtr{$ENDIF};
  TUnixMode =
    {$IF DEFINED(CPUPOWERPC)}
      LongInt
    {$ELSE}Cardinal{$ENDIF};
{$ELSE}
  TUnixTime = TTime;
  TUnixMode = TMode;
{$ENDIF}

type
  PTimeStruct = ^TTimeStruct;
  TTimeStruct = record
    tm_sec:    cint;      //* Seconds.      [0-60] (1 leap second)
    tm_min:    cint;      //* Minutes.      [0-59]
    tm_hour:   cint;      //* Hours.        [0-23]
    tm_mday:   cint;      //* Day.          [1-31]
    tm_mon:    cint;      //* Month.        [0-11]
    tm_year:   cint;      //* Year - 1900.
    tm_wday:   cint;      //* Day of week.  [0-6]
    tm_yday:   cint;      //* Days in year. [0-365]
    tm_isdst:  cint;      //* DST.          [-1/0/1]
    tm_gmtoff: clong;     //* Seconds east of UTC.
    tm_zone:   pansichar; //* Timezone abbreviation.
  end;

type
  //en Password file entry record
  passwd = record
    pw_name: PChar;    //en< user name
    pw_passwd: PChar;  //en< user password
    pw_uid: uid_t;     //en< user ID
    pw_gid: gid_t;     //en< group ID
{$IF DEFINED(BSD)}
    pw_change: time_t; //en< password change time
    pw_class: PChar;   //en< user access class
{$ENDIF}
{$IF NOT DEFINED(HAIKU)}
    pw_gecos: PChar;   //en< real name
{$ENDIF}
    pw_dir: PChar;     //en< home directory
    pw_shell: PChar;   //en< shell program
{$IF DEFINED(HAIKU)}
    pw_gecos: PChar;   //en< real name
{$ENDIF}
{$IF DEFINED(BSD)}
    pw_expire: time_t; //en< account expiration
    pw_fields: cint;   //en< internal: fields filled in
{$ENDIF}
  end;
  TPasswordRecord = passwd;
  PPasswordRecord = ^TPasswordRecord;
  //en Group file entry record
  group = record
    gr_name: PChar;   //en< group name
    gr_passwd: PChar; //en< group password
    gr_gid: gid_t;    //en< group ID
    gr_mem: ^PChar;   //en< group members
  end;
  TGroupRecord = group;
  PGroupRecord = ^TGroupRecord;

type
{$IF DEFINED(DARWIN)}
  TDarwinStat64 = record { the types are real}
       st_dev        : dev_t;             // inode's device
       st_mode       : mode_t;            // inode protection mode
       st_nlink      : nlink_t;           // number of hard links
       st_ino        : cuint64;           // inode's number
       st_uid        : uid_t;             // user ID of the file's owner
       st_gid        : gid_t;             // group ID of the file's group
       st_rdev       : dev_t;             // device type
       st_atime      : time_t;            // time of last access
       st_atimensec  : clong;             // nsec of last access
       st_mtime      : time_t;            // time of last data modification
       st_mtimensec  : clong;             // nsec of last data modification
       st_ctime      : time_t;            // time of last file status change
       st_ctimensec  : clong;             // nsec of last file status change
       st_birthtime  : time_t;            // File creation time
       st_birthtimensec : clong;          // nsec of file creation time
       st_size       : off_t;             // file size, in bytes
       st_blocks     : cint64;            // blocks allocated for file
       st_blksize    : cuint32;           // optimal blocksize for I/O
       st_flags      : cuint32;           // user defined flags for file
       st_gen        : cuint32;           // file generation number
       st_lspare     : cint32;
       st_qspare     : array[0..1] Of cint64;
  end;

  TDCStat = TDarwinStat64;
{$ELSE}
  TDCStat = BaseUnix.Stat;
{$ENDIF}

  TDCStatHelper = record Helper for TDCStat
  Public
    function birthtime: TFileTimeEx; inline;
    function mtime:     TFileTimeEx; inline;
    function atime:     TFileTimeEx; inline;
    function ctime:     TFileTimeEx; inline;
  end;

Function DC_fpLstat( const path:RawByteString; var Info:TDCStat ): cint; inline;

// nanoseconds supported
function DC_FileSetTime(const FileName: String;
                        const mtime    : TFileTimeEx;
                        const birthtime: TFileTimeEx;
                        const atime    : TFileTimeEx ): Boolean;


{en
   Set the close-on-exec flag to all
}
procedure FileCloseOnExecAll;
{en
   Set the close-on-exec (FD_CLOEXEC) flag
}
procedure FileCloseOnExec(Handle: System.THandle); inline;
{en
   Find mount point of file system where file is located
   @param(FileName File name)
   @returns(Mount point of file system)
}
function FindMountPointPath(const FileName: String): String;
{en
   Change owner and group of a file (does not follow symbolic links)
   @param(path Full path to file)
   @param(owner User ID)
   @param(group Group ID)
   @returns(On success, zero is returned. On error, -1 is returned, and errno is set appropriately)
}
function fpLChown(path : String; owner : TUid; group : TGid): cInt;
{en
   Set process group ID for job control
}
function setpgid(pid, pgid: pid_t): cint; cdecl; external clib;
{en
   The getenv() function searches the environment list to find the
   environment variable name, and returns a pointer to the corresponding
   value string.
}
function getenv(name: PAnsiChar): PAnsiChar; cdecl; external clib;
{en
   Change or add an environment variable
   @param(name Environment variable name)
   @param(value Environment variable value)
   @param(overwrite Overwrite environment variable if exist)
   @returns(The function returns zero on success, or -1 if there was
            insufficient space in the environment)
}
function setenv(const name, value: PAnsiChar; overwrite: cint): cint; cdecl; external clib;
{en
   Remove an environment variable
   @param(name Environment variable name)
   @returns(The function returns zero on success, or -1 on error)
}
function unsetenv(const name: PAnsiChar): cint; cdecl; external clib;
{en
   Get password file entry
   @param(uid User ID)
   @returns(The function returns a pointer to a structure containing the broken-out
            fields of the record in the password database that matches the user ID)
}
function getpwuid(uid: uid_t): PPasswordRecord; cdecl; external clib;
{en
   Get password file entry
   @param(name User name)
   @returns(The function returns a pointer to a structure containing the broken-out
            fields of the record in the password database that matches the user name)
}
function getpwnam(const name: PChar): PPasswordRecord; cdecl; external clib;
{en
   Get group file entry
   @param(gid Group ID)
   @returns(The function returns a pointer to a structure containing the broken-out
            fields of the record in the group database that matches the group ID)
}
function getgrgid(gid: gid_t): PGroupRecord; cdecl; external clib;
{en
   Get group file entry
   @param(name Group name)
   @returns(The function returns a pointer to a structure containing the broken-out
            fields of the record in the group database that matches the group name)
}
function getgrnam(name: PChar): PGroupRecord; cdecl; external clib;

function FileLock(Handle: System.THandle; Mode: cInt): System.THandle;

function fpMkTime(tm: PTimeStruct): TTime;
function fpLocalTime(timer: PTime; tp: PTimeStruct): PTimeStruct;

{$IF DEFINED(LINUX)}
function fpFDataSync(fd: cint): cint;
function fpCloneFile(src_fd, dst_fd: cint): Boolean;
function fpFAllocate(fd: cint; mode: cint; offset, len: coff_t): cint;

function mbFileGetXattr(const FileName: String): TStringArray;
function mbFileCopyXattr(const Source, Target: String): Boolean;
{$ENDIF}

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
function fnmatch(const pattern: PAnsiChar; const str: PAnsiChar; flags: cint): cint; cdecl; external clib;
{$ENDIF}

implementation

uses
  Unix, DCConvertEncoding, LazUTF8
{$IFDEF DARWIN}
  , DCDarwin
{$ENDIF}
  ;

{$IF not DEFINED(LINUX)}
function TDCStatHelper.birthtime: TFileTimeEx;
begin
  Result.sec:= st_birthtime;
  Result.nanosec:= st_birthtimensec;
end;

function TDCStatHelper.mtime: TFileTimeEx;
begin
  Result.sec:= st_mtime;
  Result.nanosec:= st_mtimensec;
end;

function TDCStatHelper.atime: TFileTimeEx;
begin
  Result.sec:= st_atime;
  Result.nanosec:= st_atimensec;
end;

function TDCStatHelper.ctime: TFileTimeEx;
begin
  Result.sec:= st_ctime;
  Result.nanosec:= st_ctimensec;
end;
{$ELSE}
function TDCStatHelper.birthtime: TFileTimeEx;
begin
  Result:= TFileTimeExNull;
end;

function TDCStatHelper.mtime: TFileTimeEx;
begin
  Result.sec:= Int64(st_mtime);
  Result.nanosec:= Int64(st_mtime_nsec);
end;

function TDCStatHelper.atime: TFileTimeEx;
begin
  Result.sec:= Int64(st_atime);
  Result.nanosec:= Int64(st_atime_nsec);
end;

function TDCStatHelper.ctime: TFileTimeEx;
begin
  Result.sec:= Int64(st_ctime);
  Result.nanosec:= Int64(st_ctime_nsec);
end;
{$ENDIF}


{$IF DEFINED(DARWIN)}

Function fpLstat64( path:pchar; Info:pstat ): cint; cdecl; external clib name 'lstat64';

Function DC_fpLstat( const path:RawByteString; var Info:TDCStat ): cint; inline;
var
  SystemPath: RawByteString;
begin
  SystemPath:=ToSingleByteFileSystemEncodedFileName( path );
  Result:= fpLstat64( pchar(SystemPath), @info );
end;

{$ELSE}

Function DC_fpLstat( const path:RawByteString; var Info:TDCStat ): cint; inline;
begin
  Result:= fpLstat( path, info );
end;

{$ENDIF}

function fputimes( path:pchar; times:Array of UnixType.timeval ): cint; cdecl; external clib name 'utimes';

function DC_FileSetTime(const FileName: String;
                        const mtime    : TFileTimeEx;
                        const birthtime: TFileTimeEx;
                        const atime    : TFileTimeEx ): Boolean;
var
  timevals: Array[0..1] of UnixType.timeval;
begin
  Result:= false;

  // last access time
  timevals[0].tv_sec:= atime.sec;
  timevals[0].tv_usec:= round( Extended(atime.nanosec) / 1000.0 );
  // last modification time
  timevals[1].tv_sec:= mtime.sec;
  timevals[1].tv_usec:= round( Extended(mtime.nanosec) / 1000.0 );
  if fputimes(pchar(UTF8ToSys(FileName)), timevals) <> 0 then exit;

  {$IF not DEFINED(DARWIN)}
  Result:= true;
  {$ELSE}
  Result:= MacosFileSetCreationTime( FileName, birthtime );
  {$ENDIF}
end;



{$IF DEFINED(BSD)}
type rlim_t = Int64;
{$ENDIF}

const
  {$IF DEFINED(LINUX)}
  _SC_OPEN_MAX  = 4;
  FICLONE       = $40049409;
  RLIM_INFINITY = rlim_t(-1);
  {$ELSEIF DEFINED(BSD)}
  _SC_OPEN_MAX  = 5;
  RLIM_INFINITY = rlim_t(High(QWord) shr 1);
  {$ELSEIF DEFINED(HAIKU)}
  _SC_OPEN_MAX  = 20;
  RLIMIT_NOFILE = 4;
  RLIM_INFINITY = $ffffffff;
  {$ENDIF}

procedure tzset(); cdecl; external clib;
function sysconf(name: cint): clong; cdecl; external clib;
function mktime(tp: PTimeStruct): TTime; cdecl; external clib;
function localtime_r(timer: PTime; tp: PTimeStruct): PTimeStruct; cdecl; external clib;
function lchown(path : PChar; owner : TUid; group : TGid): cInt; cdecl; external clib;
{$IF DEFINED(LINUX)}
function fdatasync(fd: cint): cint; cdecl; external clib;
function fallocate(fd: cint; mode: cint; offset, len: coff_t): cint; cdecl; external clib;

function lremovexattr(const path, name: PAnsiChar): cint; cdecl; external clib;
function llistxattr(const path: PAnsiChar; list: PAnsiChar; size: csize_t): ssize_t; cdecl; external clib;
function lgetxattr(const path, name: PAnsiChar; value: Pointer; size: csize_t): ssize_t; cdecl; external clib;
function lsetxattr(const path, name: PAnsiChar; const value: Pointer; size: csize_t; flags: cint): cint; cdecl; external clib;
{$ENDIF}

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

function FindMountPointPath(const FileName: String): String;
var
  I, J: LongInt;
  sTemp: String;
  recStat: Stat;
  st_dev: QWord;
begin
  // Set root directory as mount point by default
  Result:= PathDelim;
  // Get stat info for original file
  if (fpLStat(FileName, recStat) < 0) then Exit;
  // Save device ID of original file
  st_dev:= recStat.st_dev;
  J:= Length(FileName);
  for I:= J downto 1 do
  begin
    if FileName[I] = PathDelim then
    begin
      if (I = 1) then
        sTemp:= PathDelim
      else
        sTemp:= Copy(FileName, 1, I - 1);
      // Stat for current directory
      if (fpLStat(sTemp, recStat) < 0) then Continue;
      // If it is a link then checking link destination
      if fpS_ISLNK(recStat.st_mode) then
      begin
        sTemp:= fpReadlink(sTemp);
        Result:= FindMountPointPath(sTemp);
        Exit;
      end;
      // Check device ID
      if (recStat.st_dev <> st_dev) then
      begin
        Result:= Copy(FileName, 1, J);
        Exit;
      end;
      J:= I;
    end;
  end;
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
{$IFDEF LINUX}
  Sbfs: TStatFS;
{$ENDIF}
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
{$IFDEF LINUX}
  if (fpFStatFS(Handle, @Sbfs) = 0) then
  begin
    case UInt32(Sbfs.fstype) of
      SMB_SUPER_MAGIC,
      SMB2_MAGIC_NUMBER,
      CIFS_MAGIC_NUMBER: Exit;
    end;
  end;
{$ENDIF}
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

function fpMkTime(tm: PTimeStruct): TTime;
begin
  Result := mktime(tm);
  if (Result = TTime(-1)) then fpseterrno(fpgetCerrno);
end;

function fpLocalTime(timer: PTime; tp: PTimeStruct): PTimeStruct;
begin
  Result := localtime_r(timer, tp);
  if (Result = nil) then fpseterrno(fpgetCerrno);
end;

{$IF DEFINED(LINUX)}
function fpFDataSync(fd: cint): cint;
begin
  Result := fdatasync(fd);
  if Result = -1 then fpseterrno(fpgetCerrno);
end;

function fpCloneFile(src_fd, dst_fd: cint): Boolean;
var
  ASource: Pointer absolute src_fd;
begin
  Result:= (FpIOCtl(dst_fd, FICLONE, ASource) = 0);
end;

function fpFAllocate(fd: cint; mode: cint; offset, len: coff_t): cint;
begin
  Result := fallocate(fd, mode, offset, len);
  if Result = -1 then fpseterrno(fpgetCerrno);
end;

function mbFileGetXattr(const FileName: String): TStringArray;
var
  AList: String;
  ALength: ssize_t;
  AFileName: String;
begin
  SetLength(AList, MaxSmallint);
  Result:= Default(TStringArray);
  AFileName:= CeUtf8ToSys(FileName);
  ALength:= llistxattr(PAnsiChar(AFileName), Pointer(AList), Length(AList));
  if (ALength < 0) then
  begin
    if (fpgetCerrno <> ESysERANGE) then
    begin
      fpseterrno(fpgetCerrno);
      Exit;
    end
    else begin
      ALength:= llistxattr(PAnsiChar(AFileName), nil, 0);
      if ALength < 0 then
      begin
        fpseterrno(fpgetCerrno);
        Exit;
      end;
      SetLength(AList, ALength);
      ALength:= llistxattr(PAnsiChar(AFileName), Pointer(AList), ALength);
      if ALength < 0 then
      begin
        fpseterrno(fpgetCerrno);
        Exit;
      end;
    end;
  end;
  if (ALength > 0) then
  begin
    SetLength(AList, ALength - 1);
    Result:= AList.Split(#0);
  end;
end;

function mbFileCopyXattr(const Source, Target: String): Boolean;
var
  Value: String;
  Index: Integer;
  ALength: ssize_t;
  Names: TStringArray;
  ASource, ATarget: String;
begin
  Result:= True;
  ASource:= CeUtf8ToSys(Source);
  ATarget:= CeUtf8ToSys(Target);
  // Remove attributes from target
  Names:= mbFileGetXattr(Target);
  for Index:= 0 to High(Names) do
  begin
    lremovexattr(PAnsiChar(ATarget), PAnsiChar(Names[Index]));
  end;
  SetLength(Value, MaxSmallint);
  Names:= mbFileGetXattr(Source);
  for Index:= 0 to High(Names) do
  begin
    ALength:= lgetxattr(PAnsiChar(ASource), PAnsiChar(Names[Index]), Pointer(Value), Length(Value));
    if (ALength < 0) then
    begin
      if (fpgetCerrno <> ESysERANGE) then
      begin
        fpseterrno(fpgetCerrno);
        Exit(False);
      end
      else begin
        ALength:= lgetxattr(PAnsiChar(ASource), PAnsiChar(Names[Index]), nil, 0);
        if ALength < 0 then
        begin
          fpseterrno(fpgetCerrno);
          Exit(False);
        end;
        SetLength(Value, ALength);
        ALength:= lgetxattr(PAnsiChar(ASource), PAnsiChar(Names[Index]), Pointer(Value), Length(Value));
        if ALength < 0 then
        begin
          fpseterrno(fpgetCerrno);
          Exit(False);
        end;
      end;
    end;
    if (lsetxattr(PAnsiChar(ATarget), PAnsiChar(Names[Index]), Pointer(Value), ALength, 0) < 0) then
    begin
      fpseterrno(fpgetCerrno);
      Exit(fpgeterrno = ESysEOPNOTSUPP);
    end;
  end;
end;
{$ENDIF}

initialization
  tzset();

end.

