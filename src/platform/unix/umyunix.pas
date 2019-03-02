{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains specific UNIX functions.

    Copyright (C) 2008-2016 Alexander Koblov (alexx2000@mail.ru)

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

unit uMyUnix;

{$mode objfpc}{$H+}
{$packrecords c}

{$IF NOT DEFINED(LINUX)}
{$DEFINE FPC_USE_LIBC}
{$ENDIF}

interface

uses
  Classes, SysUtils, BaseUnix, CTypes, DCBasicTypes, uDrive;

const
  libc = 'c';
  _PATH_FSTAB = '/etc/fstab';
  _PATH_MOUNTED = '/etc/mtab';

type
  TDesktopEnvironment = (
    DE_UNKNOWN  = 0,
    DE_KDE      = 1,
    DE_GNOME    = 2,
    DE_XFCE     = 3,
    DE_LXDE     = 4,
    DE_MATE     = 5,
    DE_CINNAMON = 6
  );

const
  DesktopName: array[TDesktopEnvironment] of String = (
    'Unknown',
    'KDE',
    'GNOME',
    'Xfce',
    'LXDE',
    'MATE',
    'Cinnamon'
  );

type
  PIOFILE = Pointer;
  PFILE = PIOFILE;
  //en Mount entry record
  mntent = record
    mnt_fsname: PChar;   //en< name of mounted file system
    mnt_dir: PChar;      //en< file system path prefix
    mnt_type: PChar;     //en< mount type
    mnt_opts: PChar;     //en< mount options
    mnt_freq: LongInt;   //en< dump frequency in days
    mnt_passno: LongInt; //en< pass number on parallel fsck
  end;
  TMountEntry = mntent;
  PMountEntry = ^TMountEntry;

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
    pw_gecos: PChar;   //en< real name
    pw_dir: PChar;     //en< home directory
    pw_shell: PChar;   //en< shell program
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

{$IFDEF LINUX}
{en
   Opens the file system description file
   @param(filename File system description file)
   @param(mode Type of access)
   @returns(The function returns a file pointer to file system description file)
}
function setmntent(const filename: PChar; const mode: PChar): PFILE; cdecl; external libc name 'setmntent';
{en
   Reads the next line from  the file system description file
   @param(stream File pointer to file system description file)
   @returns(The function returns a pointer to a structure containing the
            broken out fields from a line in the file)
}
function getmntent(stream: PFILE): PMountEntry; cdecl; external libc name 'getmntent';
{en
   Closes the file system description file
   @param(stream File pointer to file system description file)
   @returns(The function always returns 1)
}
function endmntent(stream: PFILE): LongInt; cdecl; external libc name 'endmntent';
{$ENDIF}
{en
   Get password file entry
   @param(uid User ID)
   @returns(The function returns a pointer to a structure containing the broken-out
            fields of the record in the password database that matches the user ID)
}
function getpwuid(uid: uid_t): PPasswordRecord; cdecl; external libc name 'getpwuid';
{en
   Get password file entry
   @param(name User name)
   @returns(The function returns a pointer to a structure containing the broken-out
            fields of the record in the password database that matches the user name)
}
function getpwnam(const name: PChar): PPasswordRecord; cdecl; external libc name 'getpwnam';
{en
   Get group file entry
   @param(gid Group ID)
   @returns(The function returns a pointer to a structure containing the broken-out
            fields of the record in the group database that matches the group ID)
}
function getgrgid(gid: gid_t): PGroupRecord; cdecl; external libc name 'getgrgid';
{en
   Get group file entry
   @param(name Group name)
   @returns(The function returns a pointer to a structure containing the broken-out
            fields of the record in the group database that matches the group name)
}
function getgrnam(name: PChar): PGroupRecord; cdecl; external libc name 'getgrnam';
{en
   The getenv() function searches the environment list to find the
   environment variable name, and returns a pointer to the corresponding
   value string.
}
function getenv(name: PAnsiChar): PAnsiChar; cdecl; external libc name 'getenv';
{en
   Change or add an environment variable
   @param(name Environment variable name)
   @param(value Environment variable value)
   @param(overwrite Overwrite environment variable if exist)
   @returns(The function returns zero on success, or -1 if there was
            insufficient space in the environment)
}
function setenv(const name, value: PChar; overwrite: LongInt): LongInt; cdecl; external libc name 'setenv';

function fpSystemStatus(Command: string): cint;

function GetDesktopEnvironment: TDesktopEnvironment;
function FileIsLinkToFolder(const FileName: String; out LinkTarget: String): Boolean;
{en
   Checks if file is executable or script
   @param(FileName File name)
   @returns(The function returns @true if successful, @false otherwise)
}
function FileIsUnixExecutable(const Filename: String): Boolean;
{en
   Find mount point of file system where file is located
   @param(FileName File name)
   @returns(Mount point of file system)
}
function FindMountPointPath(const FileName: String): String;
function ExecutableInSystemPath(const FileName: String): Boolean;
function GetDefaultAppCmd(const FileName: String): String;
function GetFileMimeType(const FileName: String): String;
{en
   Fix separators in case they are broken UTF-8 characters
   (FPC takes only first byte as it doesn't support Unicode).
}
procedure FixDateTimeSeparators;

function MountDrive(Drive: PDrive): Boolean;
function UnmountDrive(Drive: PDrive): Boolean;
function EjectDrive(Drive: PDrive): Boolean;

function ExecuteCommand(Command: String; Args: TDynamicStringArray; StartPath: String): Boolean;

{$IF DEFINED(BSD)}
const
  MNT_WAIT = 1; // synchronously wait for I/O to complete
  MNT_NOWAIT = 2; // start all I/O, but do not wait for it
  MNT_LAZY = 3; // push data not written by filesystem syncer
  MNT_SUSPEND = 4; // suspend file system after sync

type
  TFSTab = record
    fs_spec: PChar; // block special device name
    fs_file: PChar; // file system path prefix
    fs_vfstype: PChar; // file system type, ufs, nfs
    fs_mntops: PChar; // mount options ala -o
    fs_type: PChar; // FSTAB_* from fs_mntops
    fs_freq: longint; // dump frequency, in days
    fs_passno: longint; // pass number on parallel fsc
  end;
  PFSTab = ^TFSTab;
  PStatFS = ^TStatFS;

{$IF DEFINED(DARWIN)}
function getfsstat(buf: pstatfs; bufsize: cint; flags: cint): cint; cdecl; external libc name 'getfsstat';
{$ELSE}
function getfsstat(struct_statfs: PStatFS; const buffsize: int64; const int_flags: integer): integer;
{$ENDIF}
function getfsent(): PFSTab; cdecl; external libc name 'getfsent';
procedure endfsent(); cdecl; external libc name 'endfsent';
{$ENDIF}

var
  DesktopEnv: TDesktopEnvironment = DE_UNKNOWN;

implementation

uses
  URIParser, Unix, Process, LazUTF8, DCOSUtils, DCClassesUtf8, DCStrUtils,
  DCUnix, uDCUtils, uOSUtils
{$IF (NOT DEFINED(FPC_USE_LIBC)) or (DEFINED(BSD) AND NOT DEFINED(DARWIN))}
  , SysCall
{$ENDIF}
{$IF NOT DEFINED(DARWIN)}
  , uMimeActions, uMimeType, uGVolume
{$ENDIF}
{$IFDEF LINUX}
  , uUDisks
{$ENDIF}
  ;

{$IF DEFINED(BSD) AND NOT DEFINED(DARWIN)}
function getfsstat(struct_statfs: PStatFS; const buffsize: int64; const int_flags: integer): integer;
{$IF DEFINED(FREEBSD) AND ((fpc_version<2) OR ((fpc_version=2) AND (fpc_release<5)))}
const
  syscall_nr_getfsstat = 18; // was not defined before fpc 2.5.1
{$ENDIF}
begin
  Result := do_syscall(syscall_nr_getfsstat, TSysParam(struct_statfs), TSysParam(buffsize), TSysParam(int_flags));
end;
{$ENDIF}

function fpSystemStatus(Command: string): cint;
begin
  Result := fpSystem(UTF8ToSys(Command));
  if wifexited(Result) then
    Result := wexitStatus(Result);
end;

{$IFDEF LINUX}

var
  HavePMount: Boolean = False;
  HaveUDisksCtl: Boolean = False;

procedure CheckPMount;
begin
  // Check pumount first because Puppy Linux has another tool named pmount
  HavePMount := (fpSystemStatus('pumount --version > /dev/null 2>&1') = 0) and
                (fpSystemStatus('pmount --version > /dev/null 2>&1') = 0);
end;

procedure CheckUDisksCtl;
begin
  HaveUDisksCtl := (fpSystemStatus('udisksctl help > /dev/null 2>&1') = 0);
end;

{$ENDIF LINUX}

function GetDesktopEnvironment: TDesktopEnvironment;
var
  I: Integer;
  DesktopSession: String;
const
  EnvVariable: array[0..1] of String = ('XDG_CURRENT_DESKTOP', 'DESKTOP_SESSION');
begin
  Result:= DE_UNKNOWN;
  for I:= Low(EnvVariable) to High(EnvVariable) do
  begin
    DesktopSession:= GetEnvironmentVariable(EnvVariable[I]);
    if Length(DesktopSession) = 0 then Continue;
    DesktopSession:= LowerCase(DesktopSession);
    if Pos('kde', DesktopSession) <> 0 then
      Exit(DE_KDE);
    if Pos('gnome', DesktopSession) <> 0 then
      Exit(DE_GNOME);
    if Pos('xfce', DesktopSession) <> 0 then
      Exit(DE_XFCE);
    if Pos('lxde', DesktopSession) <> 0 then
      Exit(DE_LXDE);
    if Pos('mate', DesktopSession) <> 0 then
      Exit(DE_MATE);
    if Pos('cinnamon', DesktopSession) <> 0 then
      Exit(DE_CINNAMON);
  end;
  if GetEnvironmentVariable('KDE_FULL_SESSION') <> '' then
    Exit(DE_KDE);
  if GetEnvironmentVariable('GNOME_DESKTOP_SESSION_ID') <> '' then
    Exit(DE_GNOME);
  if GetEnvironmentVariable('_LXSESSION_PID') <> '' then
    Exit(DE_LXDE);
  if fpSystemStatus('pgrep xfce4-session > /dev/null') = 0 then
    Exit(DE_XFCE);
end;

function FileIsLinkToFolder(const FileName: String; out LinkTarget: String): Boolean;
var
  StatInfo: BaseUnix.Stat;
  iniDesktop: TIniFileEx = nil;
begin
  Result:= False;
  try
    iniDesktop:= TIniFileEx.Create(FileName, fmOpenRead);
    try
      if iniDesktop.ReadString('Desktop Entry', 'Type', EmptyStr) = 'Link' then
      begin
        LinkTarget:= iniDesktop.ReadString('Desktop Entry', 'URL', EmptyStr);
        if not URIToFilename(LinkTarget, LinkTarget) then Exit;
        if fpLStat(UTF8ToSys(LinkTarget), StatInfo) <> 0 then Exit;
        Result:= FPS_ISDIR(StatInfo.st_mode);
      end;
    finally
      FreeAndNil(iniDesktop);
    end;
  except
    // Ignore
  end;
end;

function FileIsUnixExecutable(const FileName: String): Boolean;
var
  Info : Stat;
  dwSign : LongWord;
  fsExeScr : TFileStreamEx = nil;
begin
  // First check FileName is not a directory and then check if executable
  Result:= (fpStat(UTF8ToSys(FileName), Info) <> -1) and FPS_ISREG(Info.st_mode) and
           (BaseUnix.fpAccess(UTF8ToSys(FileName), BaseUnix.X_OK) = 0);
  if Result and (Info.st_size >= SizeOf(dwSign)) then
  try
    fsExeScr := TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      dwSign := fsExeScr.ReadDWord;
      // ELF or #!
      Result := ((dwSign = NtoBE($7F454C46)) or (Lo(dwSign) = NtoBE($2321)));
    finally
      fsExeScr.Free;
    end;
  except
    Result:= False;
  end;
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
  if (fpLStat(UTF8ToSys(FileName), recStat) < 0) then Exit;
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
      if (fpLStat(UTF8ToSys(sTemp), recStat) < 0) then Continue;
      // If it is a link then checking link destination
      if fpS_ISLNK(recStat.st_mode) then
      begin
        sTemp:= ReadSymLink(sTemp);
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

function ExecutableInSystemPath(const FileName: String): Boolean;
var
  I: Integer;
  Path: String;
  Value: TDynamicStringArray;
begin
  Path:= GetEnvironmentVariable('PATH');
  Value:= SplitString(Path, PathSeparator);
  for I:= Low(Value) to High(Value) do
  begin
    if fpAccess(IncludeTrailingPathDelimiter(Value[I]) + FileName, X_OK) = 0 then
      Exit(True);
  end;
  Result:= False;
end;

function GetDefaultAppCmd(const FileName: String): String;
{$IF NOT DEFINED(DARWIN)}
var
  Filenames: TStringList;
begin
  Filenames:= TStringList.Create;
  Filenames.Add(FileName);
  Result:= uMimeActions.GetDefaultAppCmd(Filenames);
  if Length(Result) = 0 then
  Result:= 'xdg-open ' + QuoteStr(FileName);
  FreeAndNil(Filenames);
end;
{$ELSE}
begin
  Result:= 'xdg-open ' + QuoteStr(FileName);
end;
{$ENDIF}

function GetFileMimeType(const FileName: String): String;
{$IF NOT DEFINED(DARWIN)}
begin
  Result:= uMimeType.GetFileMimeType(FileName);
end;
{$ELSE}
begin
  Result:= EmptyStr;
end;
{$ENDIF}

procedure FixDateTimeSeparators;
var
  TimeEnv: String;
begin
  TimeEnv := GetEnvironmentVariable('LC_TIME');
  if TimeEnv = EmptyStr then
    TimeEnv := GetEnvironmentVariable('LC_ALL');
  if TimeEnv = EmptyStr then
    TimeEnv := GetEnvironmentVariable('LANG');
  if TimeEnv <> EmptyStr then
  begin
    TimeEnv := upcase(TimeEnv);
    if StrEnds(TimeEnv, 'UTF-8') or StrEnds(TimeEnv, 'UTF8') then
    with FormatSettings do
    begin
      if Ord(DateSeparator) > $7F then
        DateSeparator := '/';
      if Ord(TimeSeparator) > $7F then
        TimeSeparator := ':';
    end;
  end;
end;

function Mount(const Path: String; Timeout: Integer): Boolean;
var
  Message: String;
  Handler: TMethod;
  Process: TProcess;
  Index: Integer = 0;

  procedure ProcessForkEvent{$IF (FPC_FULLVERSION >= 30000)}(Self, Sender : TObject){$ENDIF};
  begin
    if (setpgid(0, 0) < 0) then fpExit(127);
  end;

begin
  Process:= TProcess.Create(nil);
  try
    Handler.Data:= Process;
    Process.Executable:= 'mount';
    Process.Parameters.Add(Path);
    Handler.Code:= @ProcessForkEvent;
    {$IF (FPC_FULLVERSION >= 30000)}
    Process.OnForkEvent:= TProcessForkEvent(Handler);
    {$ELSE}
    Process.OnForkEvent:= TProcessForkEvent(@ProcessForkEvent);
    {$ENDIF}
    Process.Options:= Process.Options + [poUsePipes, poStderrToOutPut];
    try
      Process.Execute;
      while Process.Running do
      begin
        Inc(Index);
        Sleep(100);
        if (Index > Timeout) then
        begin
          Process.Terminate(-1);
          fpKill(-Process.Handle, SIGTERM);
          Exit(False);
        end;
        Process.Input.Write(#13#10, 2);
        if (Process.Output.NumBytesAvailable > 0) then
        begin
          SetLength(Message, Process.Output.NumBytesAvailable);
          Process.Output.Read(Message[1], Length(Message));
          Write(Message);
        end;
      end;
      {$IF (FPC_FULLVERSION >= 30000)}
      Result:= (Process.ExitCode = 0);
      {$ELSE}
      Result:= (Process.ExitStatus = 0);
      {$ENDIF}
    except
      Result:= False;
    end;
  finally
    Process.Free;
  end;
end;

function MountDrive(Drive: PDrive): Boolean;
{$IFDEF LINUX}
var
  Index: Integer;
  MountPath: String = '';
{$ENDIF}
begin
  if not Drive^.IsMounted then
  begin
{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
    Result := False;
    // If Path is not empty "mount" can mount it because it has a destination path from fstab.
    if Drive^.Path <> EmptyStr then
{$ENDIF}
      Result := Mount(Drive^.Path, 300);
{$IF DEFINED(LINUX)}
    if not Result and HaveUDisksCtl then
    begin
      Result:= RunCommand('udisksctl', ['mount', '-b', Drive^.DeviceId], MountPath);
      if Result then
      begin
        Write(MountPath);
        Index:= Pos(' at ', MountPath);
        if Index > 0  then
        begin
          Inc(Index, 4);
          Drive^.Path:= Copy(MountPath, Index, Length(MountPath) - Index - 1);
        end;
      end
    end;
    if not Result and uUDisks.Initialize then
    begin
      try
        Result := uUDisks.Mount(DeviceFileToUDisksObjectPath(Drive^.DeviceId), EmptyStr, nil, MountPath);
      except
        on E: Exception do
        begin
          Result := False;
          WriteLn(E.Message);
        end;
      end;
      if Result then
        Drive^.Path := MountPath;
      uUDisks.Finalize;
    end;
    if not Result and HavePMount and Drive^.IsMediaRemovable then
      Result := fpSystemStatus('pmount ' + Drive^.DeviceId) = 0;
{$ELSE IF DEFINED(DARWIN)}
    if not Result then
      Result := fpSystemStatus('diskutil mount ' + Drive^.DeviceId) = 0;
{$ENDIF}
  end
  else
    Result := True;
end;

function UnmountDrive(Drive: PDrive): Boolean;
begin
  if Drive^.IsMounted then
  begin
{$IF NOT DEFINED(DARWIN)}
    if Drive^.DriveType = dtSpecial then
    begin
      Exit(uGVolume.Unmount(Drive^.Path));
    end;
{$ENDIF}
{$IF DEFINED(LINUX)}
    Result := False;
    if HaveUDisksCtl then
      Result := fpSystemStatus('udisksctl unmount -b ' + Drive^.DeviceId) = 0;
    if not Result and uUDisks.Initialize then
    begin
      Result := uUDisks.Unmount(DeviceFileToUDisksObjectPath(Drive^.DeviceId), nil);
      uUDisks.Finalize;
    end;
    if not Result and HavePMount and Drive^.IsMediaRemovable then
      Result := fpSystemStatus('pumount ' + Drive^.DeviceId) = 0;
    if not Result then
{$ELSE IF DEFINED(DARWIN)}
    Result := fpSystemStatus('diskutil unmount ' + Drive^.DeviceId) = 0;
    if not Result then
{$ENDIF}
    Result := fpSystemStatus('umount ' + Drive^.Path) = 0;
  end
  else
    Result := True;
end;

function EjectDrive(Drive: PDrive): Boolean;
begin
{$IF DEFINED(DARWIN)}
  Result := fpSystemStatus('diskutil eject ' + Drive^.DeviceId) = 0;
  if not Result then
{$ENDIF}
  Result := fpSystemStatus('eject ' + Drive^.DeviceId) = 0;
end;

{en
  Waits for a child process to finish and collects its exit status,
  causing it to be released by the system (prevents defunct processes).

  Instead of the wait-thread we could just ignore or handle SIGCHLD signal
  for the process, but this way we don't interfere with the signal handling.
  The downside is that there's a thread for every child process running.

  Another method is to periodically do a cleanup, for example from OnIdle
  or OnTimer event. Remember PIDs of spawned child processes and when
  cleaning call FpWaitpid(PID, nil, WNOHANG) on each PID. Downside is they
  are not released immediately after the child process finish (may be relevant
  if we want to display exit status to the user).
}
function WaitForPidThread(Parameter : Pointer): PtrInt;
var
  Status : cInt = 0;
  PID: PtrInt absolute Parameter;
begin
  while (FpWaitPid(PID, @Status, 0) = -1) and (fpgeterrno() = ESysEINTR) do;
  WriteLn('Process ', PID, ' finished, exit status ', Status);
  Result:= Status; EndThread(Result);
end;

function ExecuteCommand(Command: String; Args: TDynamicStringArray; StartPath: String): Boolean;
var
  pid : TPid;
begin
  {$IFDEF DARWIN}
  // If we run application bundle (*.app) then
  // execute it by 'open -a' command (see 'man open' for details)
  if StrEnds(Command, '.app') then
  begin
    SetLength(Args, Length(Args) + 2);
    for pid := High(Args) downto Low(Args) + 2 do
      Args[pid]:= Args[pid - 2];
    Args[0] := '-a';
    Args[1] := Command;
    Command := 'open';
  end;
  {$ENDIF}

  pid := fpFork;

  if pid = 0 then
    begin
      { Set the close-on-exec flag to all }
      FileCloseOnExecAll;

      { Set child current directory }
      if Length(StartPath) > 0 then fpChdir(StartPath);

      { The child does the actual exec, and then exits }
      if FpExecLP(Command, Args) = -1 then
        Writeln(Format('Execute error %d: %s', [fpgeterrno, SysErrorMessage(fpgeterrno)]));

      { If the FpExecLP fails, we return an exitvalue of 127, to let it be known }
      fpExit(127);
    end
  else if pid = -1 then         { Fork failed }
    begin
      WriteLn('Fork failed: ' + Command, LineEnding, SysErrorMessage(fpgeterrno));
    end
  else if pid > 0 then          { Parent }
    begin
      {$PUSH}{$WARNINGS OFF}{$HINTS OFF}
      BeginThread(@WaitForPidThread, Pointer(PtrInt(pid)));
      {$POP}
    end;

  Result := (pid > 0);
end;

{$IF NOT DEFINED(DARWIN)}
initialization
  DesktopEnv := GetDesktopEnvironment;
  {$IFDEF LINUX}
    CheckPMount;
    CheckUDisksCtl;
  {$ENDIF}
{$ENDIF}

end.

