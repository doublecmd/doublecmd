{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains specific UNIX functions.

    Copyright (C) 2008-2023 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
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
    DE_CINNAMON = 6,
    DE_LXQT     = 7
  );

const
  DesktopName: array[TDesktopEnvironment] of String = (
    'Unknown',
    'KDE',
    'GNOME',
    'Xfce',
    'LXDE',
    'MATE',
    'Cinnamon',
    'LXQt'
  );

{$IF DEFINED(LINUX)}
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

function fpSystemStatus(Command: string): cint;

function GetDesktopEnvironment: TDesktopEnvironment;
function FileIsLinkToFolder(const FileName: String; out LinkTarget: String): Boolean;
{en
   Checks if file is executable or script
   @param(FileName File name)
   @returns(The function returns @true if successful, @false otherwise)
}
function FileIsUnixExecutable(const Filename: String): Boolean;
function FindExecutableInSystemPath(var FileName: String): Boolean;
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
{$IF NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
  , libfontconfig, uMimeActions, uMimeType, uGVolume
{$ENDIF}
{$IFDEF DARWIN}
  , uMyDarwin
{$ENDIF}
{$IFDEF LINUX}
  , uUDisks2
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

procedure CheckPMount;
begin
  // Check pumount first because Puppy Linux has another tool named pmount
  HavePMount := (fpSystemStatus('pumount --version > /dev/null 2>&1') = 0) and
                (fpSystemStatus('pmount --version > /dev/null 2>&1') = 0);
end;

{$ENDIF LINUX}

function GetDesktopEnvironment: TDesktopEnvironment;
var
  I: Integer;
  DesktopSession: String;
const
  EnvVariable: array[0..2] of String = ('XDG_CURRENT_DESKTOP',
                                        'XDG_SESSION_DESKTOP',
                                        'DESKTOP_SESSION');
begin
  Result:= DE_UNKNOWN;
  for I:= Low(EnvVariable) to High(EnvVariable) do
  begin
    DesktopSession:= GetEnvironmentVariable(EnvVariable[I]);
    if Length(DesktopSession) = 0 then Continue;
    DesktopSession:= LowerCase(DesktopSession);
    if Pos('kde', DesktopSession) <> 0 then
      Exit(DE_KDE);
    if Pos('plasma', DesktopSession) <> 0 then
      Exit(DE_KDE);
    if Pos('gnome', DesktopSession) <> 0 then
      Exit(DE_GNOME);
    if Pos('xfce', DesktopSession) <> 0 then
      Exit(DE_XFCE);
    if Pos('lxde', DesktopSession) <> 0 then
      Exit(DE_LXDE);
    if Pos('lxqt', DesktopSession) <> 0 then
      Exit(DE_LXQT);
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

function FindExecutableInSystemPath(var FileName: String): Boolean;
var
  I: Integer;
  Path, FullName: String;
  Value: TDynamicStringArray;
begin
  Path:= GetEnvironmentVariable('PATH');
  Value:= SplitString(Path, PathSeparator);
  for I:= Low(Value) to High(Value) do
  begin
    FullName:= IncludeTrailingPathDelimiter(Value[I]) + FileName;
    if fpAccess(FullName, X_OK) = 0 then
    begin
      FileName:= FullName;
      Exit(True);
    end;
  end;
  Result:= False;
end;

function ExecutableInSystemPath(const FileName: String): Boolean;
var
  FullName: String;
begin
  FullName:= FileName;
  Result:= FindExecutableInSystemPath(FullName);
end;

function GetDefaultAppCmd(const FileName: String): String;
{$IF NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
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
{$ELSEIF DEFINED(HAIKU)}
begin
  Result:= '/bin/open ' + QuoteStr(FileName);
end;
{$ELSE}
begin
  Result:= 'xdg-open ' + QuoteStr(FileName);
end;
{$ENDIF}

function GetFileMimeType(const FileName: String): String;
{$IF NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
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
    if not Result and HasUDisks2 then
    begin
      Result:= uUDisks2.Mount(Drive^.DeviceId, MountPath);
      if Result then
      begin
        Drive^.Path:= MountPath;
        WriteLn(Drive^.DeviceId, ' -> ', MountPath);
      end
    end;
    if not Result and HavePMount and Drive^.IsMediaRemovable then
      Result := fpSystemStatus('pmount ' + Drive^.DeviceId) = 0;
{$ELSEIF DEFINED(DARWIN)}
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
{$IF NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
    if Drive^.DriveType = dtSpecial then
    begin
      Exit(uGVolume.Unmount(Drive^.Path));
    end;
{$ENDIF}
{$IF DEFINED(LINUX)}
    Result := False;
    if HasUDisks2 then
      Result := uUDisks2.Unmount(Drive^.DeviceId);
    if not Result and HavePMount and Drive^.IsMediaRemovable then
      Result := fpSystemStatus('pumount ' + Drive^.DeviceId) = 0;
    if not Result then
{$ELSEIF DEFINED(DARWIN)}
    Result := unmountAndEject( Drive^.Path );
    if not Result then
{$ENDIF}
    Result := fpSystemStatus('umount ' + Drive^.Path) = 0;
  end
  else
    Result := True;
end;

function EjectDrive(Drive: PDrive): Boolean;
begin
{$IF NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
  Result:= uGVolume.Eject(Drive^.Path);
  if not Result then
{$ENDIF}

{$IF DEFINED(LINUX)}
  Result := False;
  if HasUDisks2 then
    Result := uUDisks2.Eject(Drive^.DeviceId);
  if not Result then
{$ELSEIF DEFINED(DARWIN)}
  Result := unmountAndEject( Drive^.Path );
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

{$IF NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
function GetFontName(const AName: String): String;
var
  Res: TFcResult;
  AFont: PFcPattern;
  AFontName: PFcChar8;
  APattern: PFcPattern;
begin
  Result:= AName;
  APattern:= FcNameParse(PFcChar8(AName));
  if Assigned(APattern) then
  begin
    FcConfigSubstitute(nil, APattern, FcMatchPattern);
    FcDefaultSubstitute(APattern);
    AFont:= FcFontMatch(nil, APattern, @Res);
    if Assigned(AFont) then
    begin
      AFontName:= FcPatternFormat(AFont, '%{fullname}');
      if Assigned(AFontName) then
      begin
        Result:= StrPas(AFontName);
        FcStrFree(AFontName);
      end;
      FcPatternDestroy(AFont);
    end;
    FcPatternDestroy(APattern);
  end;
end;

initialization
  DesktopEnv := GetDesktopEnvironment;
  {$IFDEF LINUX}
    CheckPMount;
  {$ENDIF}
  if (LoadFontConfigLib('libfontconfig.so.1', False) > 0) then
  begin
    MonoSpaceFont:= GetFontName(MonoSpaceFont);
    UnLoadFontConfigLib;
  end;
{$ENDIF}

end.

