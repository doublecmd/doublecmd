{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains platform depended functions.

    Copyright (C) 2006-2012  Koblov Alexander (Alexx2000@mail.ru)

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

unit uOSUtils;
 
{$mode delphi}{$H+}

interface

uses
    SysUtils, Classes, LCLType, uDrive, DCBasicTypes
    {$IF DEFINED(UNIX)}
    , uFileAttributes
      {$IFDEF DARWIN}
      , MacOSAll
      {$ENDIF}
    {$ENDIF}
    ;
    
const
  {$IF DEFINED(MSWINDOWS)}
  faFolder = faDirectory;
  RunTerm = 'cmd.exe';  // default terminal
  RunInTerm = 'cmd.exe /K'; // default run in terminal command
  fmtRunInTerm = '%s "%s"';
  fmtRunInShell = '%s /C "%s"';
  fmtCommandPath = '%s>';
  MonoSpaceFont = 'Fixedsys';
  {$ELSEIF DEFINED(UNIX)}
  faFolder = S_IFDIR;
  {$IFDEF DARWIN)}
  RunTerm = '/Applications/Utilities/Terminal.app';  // default terminal
  RunInTerm = ''; // default run in terminal command
  fmtRunInTerm = '';
  {$ELSE}
  RunTerm = 'xterm';  // default terminal
  RunInTerm = 'xterm -e sh -c'; // default run in terminal command
  fmtRunInTerm = '%s ''%s ; echo -n Press ENTER to exit... ; read a''';
  {$ENDIF}
  fmtRunInShell = '%s -c ''%s''';
  fmtCommandPath = '[%s]$:';
  MonoSpaceFont = 'Monospace';
  {$ENDIF}

type
  EInvalidCommandLine = class(Exception);
  EInvalidQuoting = class(EInvalidCommandLine)
    constructor Create; reintroduce;
  end;

function FileIsLinkToFolder(const FileName: UTF8String; out LinkTarget: UTF8String): Boolean;
function ExecCmdFork(sCmdLine:String; bTerm : Boolean = False; sTerm : String = ''; bKeepTerminalOpen: Boolean = True):Boolean;
{en
   Opens a file or URL in the user's preferred application
   @param(URL File name or URL)
   @returns(The function returns @true if successful, @false otherwise)
}
function ShellExecute(URL: UTF8String): Boolean;
function GetDiskFreeSpace(const Path : String; out FreeSize, TotalSize : Int64) : Boolean;
{en
   Get maximum file size for a mounted file system
   @param(Path The pathname of any file within the mounted file  system)
   @returns(The maximum file size for a mounted file system)
}
function GetDiskMaxFileSize(const Path : UTF8String) : Int64;
{en
   Create a hard link to a file
   @param(Path Name of file)
   @param(LinkName Name of hard link)
   @returns(The function returns @true if successful, @false otherwise)
}
function CreateHardLink(const Path, LinkName: String) : Boolean;
{en
   Create a symbolic link
   @param(Path Name of file)
   @param(LinkName Name of symbolic link)
   @returns(The function returns @true if successful, @false otherwise)
}
function CreateSymLink(const Path, LinkName: string) : Boolean;
{en
   Read destination of symbolic link
   @param(LinkName Name of symbolic link)
   @returns(The file name/path the symbolic link name is pointing to.
            The path may be relative to link's location.)
}
function ReadSymLink(const LinkName : String) : String;
{en
   Reads the concrete file's name that the link points to.
   If the link points to a link then it's resolved recursively
   until a valid file name that is not a link is found.
   @param(LinkName Name of symbolic link (absolute path))
   @returns(The absolute filename the symbolic link name is pointing to,
            or an empty string when the link is invalid or
            the file it points to does not exist.)
}
function mbReadAllLinks(const PathToLink : String) : String;
{en
   Get the user home directory
   @returns(The user home directory)
}
function GetHomeDir : String;
{en
   Get the appropriate directory for the application's configuration files
   @returns(The directory for the application's configuration files)
}
function GetAppConfigDir: String;
{en
   Get the appropriate directory for the application's cache files
   @returns(The directory for the application's cache files)
}
function GetAppCacheDir: UTF8String;
function GetTempFolder: String;
{en
   Get the system specific self extracting archive extension
   @returns(Self extracting archive extension)
}
function GetSfxExt: String;

function IsAvailable(Drive: PDrive; TryMount: Boolean = True) : Boolean;
function GetShell : String;
{en
   Formats a string which will execute Command via shell.
}
function FormatShell(const Command: String): String;
{en
   Formats a string which will execute Command in a terminal.
}
function FormatTerminal(Command: String; bKeepTerminalOpen: Boolean): String;
{en
   Same as mbFileGetAttr, but dereferences any encountered links.
}
function mbFileGetAttrNoLinks(const FileName: UTF8String): TFileAttrs;
{en
   Convert file name to system encoding, if name can not be represented in
   current locale then use short file name under Windows.
}
function mbFileNameToSysEnc(const LongPath: UTF8String): String;
function mbGetEnvironmentVariable(const sName: UTF8String): UTF8String;
function mbSetEnvironmentVariable(const sName, sValue: UTF8String): Boolean;
{en
   Extract the root directory part of a file name.
   @returns(Drive letter under Windows and mount point under Unix)
}
function ExtractRootDir(const FileName: UTF8String): UTF8String;

procedure FixFormIcon(Handle: LCLType.HWND);
procedure HideConsoleWindow;
procedure FixDateNamesToUTF8;

implementation

uses
  FileUtil, uDCUtils, DCOSUtils, DCStrUtils, uGlobs, uLng
  {$IF DEFINED(MSWINDOWS)}
  , JwaWinCon, Windows, uNTFSLinks, uMyWindows, JwaWinNetWk, uShlObjAdditional
  , shlobj
  {$ENDIF}
  {$IF DEFINED(UNIX)}
  , BaseUnix, Unix, uMyUnix, dl
    {$IF NOT DEFINED(DARWIN)}
  , uGio
    {$ENDIF}
  {$ENDIF}
  ;

{$IFDEF UNIX}
type
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
  TWaitForPidThread = class(TThread)
  private
    FPID: TPid;
  protected
    procedure Execute; override;
  public
    constructor Create(WaitForPid: TPid); overload;
  end;

  constructor TWaitForPidThread.Create(WaitForPid: TPid);
  begin
    inherited Create(True);
    FPID := WaitForPid;
    FreeOnTerminate := True;
  end;

  procedure TWaitForPidThread.Execute;
  begin
    while (FpWaitPid(FPID, nil, 0) = -1) and (fpgeterrno() = ESysEINTR) do;
  end;
{$ENDIF}

function FileIsLinkToFolder(const FileName: UTF8String; out
  LinkTarget: UTF8String): Boolean;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= False;
  if LowerCase(ExtractOnlyFileExt(FileName)) = 'lnk' then
    Result:= SHFileIsLinkToFolder(FileName, LinkTarget);
end;
{$ELSEIF DEFINED(UNIX)}
begin
  Result:= False;
  if LowerCase(ExtractOnlyFileExt(FileName)) = 'desktop' then
    Result:= uMyUnix.FileIsLinkToFolder(FileName, LinkTarget);
end;
{$ENDIF}

(* Execute external commands *)

function ExecCmdFork(sCmdLine:String; bTerm : Boolean; sTerm : String; bKeepTerminalOpen: Boolean) : Boolean;
{$IFDEF UNIX}
var
  Command : String;
  pid : LongInt;
  Args : TDynamicStringArray;
  WaitForPidThread: TWaitForPidThread;
begin
  if bTerm then
    sCmdLine := FormatTerminal(sCmdLine, bKeepTerminalOpen);

  SplitCmdLine(UTF8ToSys(sCmdLine), Command, Args);
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
  if Command = EmptyStr then Exit(False);

  pid := fpFork;

  if pid = 0 then
    begin
      { The child does the actual exec, and then exits }
      if FpExecLP(Command, Args) = -1 then
        Writeln(Format('Execute error %d: %s', [fpgeterrno, SysErrorMessageUTF8(fpgeterrno)]));

      { If the FpExecLP fails, we return an exitvalue of 127, to let it be known }
      fpExit(127);
    end
  else if pid = -1 then         { Fork failed }
    begin
      raise Exception.Create('Fork failed: ' + Command);
    end
  else if pid > 0 then          { Parent }
    begin
      WaitForPidThread := TWaitForPidThread.Create(pid);
      WaitForPidThread.Resume;
    end;

  Result := (pid > 0);
end;
{$ELSE}
var
  sFileName,
  sParams: String;
  wFileName,
  wParams,
  wWorkDir: WideString;  
begin
  wWorkDir:= UTF8Decode(mbGetCurrentDir);

  if bTerm then
    begin
      if sTerm = '' then sTerm := RunInTerm;
      sCmdLine := Format(fmtRunInTerm, [sTerm, sCmdLine]);
    end;
    
  SplitCmdLine(sCmdLine, sFileName, sParams);
  sFileName:= NormalizePathDelimiters(sFileName);
  wFileName:= UTF8Decode(sFileName);
  wParams:= UTF8Decode(sParams);
  Result := (ShellExecuteW(0, nil, PWChar(wFileName), PWChar(wParams), PWChar(wWorkDir), SW_SHOW) > 32);
end;
{$ENDIF}

function ShellExecute(URL: UTF8String): Boolean;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= ExecCmdFork(Format('"%s"', [URL]));
  if Result = False then
      Result:= ExecCmdFork('rundll32 shell32.dll OpenAs_RunDLL ' + URL);
end;
{$ELSEIF DEFINED(DARWIN)}
var
  theFileNameCFRef: CFStringRef = nil;
  theFileNameUrlRef: CFURLRef = nil;
  theFileNameFSRef: FSRef;
begin
  Result:= False;
  try
    theFileNameCFRef:= CFStringCreateWithFileSystemRepresentation(nil, PAnsiChar(URL));
    theFileNameUrlRef:= CFURLCreateWithFileSystemPath(nil, theFileNameCFRef, kCFURLPOSIXPathStyle, False);
    if (CFURLGetFSRef(theFileNameUrlRef, theFileNameFSRef)) then
      begin
        Result:= (LSOpenFSRef(theFileNameFSRef, nil) = noErr);
      end;
  finally
    if Assigned(theFileNameCFRef) then
      CFRelease(theFileNameCFRef);
    if Assigned(theFileNameUrlRef) then
      CFRelease(theFileNameUrlRef);
  end;
end;
{$ELSE}
var
  DesktopEnv: Cardinal;
  sCmdLine: UTF8String;
begin
  Result:= False;
  sCmdLine:= EmptyStr;
  if FileIsUnixExecutable(URL) then
    begin
      if GetPathType(URL) <> ptAbsolute then
        sCmdLine := './';
      sCmdLine:= sCmdLine + QuoteStr(URL);
    end
  else
    begin
      DesktopEnv:= GetDesktopEnvironment;
      if (DesktopEnv = DE_KDE) and (FindDefaultExecutablePath('kioclient') <> EmptyStr) then
        sCmdLine:= 'kioclient exec ' + QuoteStr(URL) // Under KDE use "kioclient" to open files
      else if HasGio then
        Result:= GioOpen(URL) // Under GNOME, Xfce, Unity and LXDE use "GIO" to open files
      else
        begin
          if GetPathType(URL) = ptAbsolute then
            sCmdLine:= URL
          else
            begin
              sCmdLine := IncludeTrailingPathDelimiter(mbGetCurrentDir);
              sCmdLine:= GetAbsoluteFileName(sCmdLine, URL)
            end;
          sCmdLine:= GetDefaultAppCmd(sCmdLine);
        end;
    end;
  if Length(sCmdLine) <> 0 then
    Result:= ExecCmdFork(sCmdLine);
end;
{$ENDIF}

(* Get Disk Free Space *)

function GetDiskFreeSpace(const Path : String; out FreeSize, TotalSize : Int64) : Boolean;
{$IFDEF UNIX}
var
  sbfs: TStatFS;
begin
    Result:= (fpStatFS(PChar(UTF8ToSys(Path)), @sbfs) = 0);
    if not Result then Exit;
    FreeSize := (Int64(sbfs.bavail)*sbfs.bsize);
{$IF DEFINED(CPU32) or (FPC_VERSION>2) or ((FPC_VERSION=2) and ((FPC_RELEASE>2) or ((FPC_RELEASE=2) and (FPC_PATCH>=3))))}
    TotalSize := (Int64(sbfs.blocks)*sbfs.bsize);
{$ENDIF}
end;
{$ELSE}
var
  wPath: WideString;
begin
  wPath:= UTF8Decode(Path);
  Result:= GetDiskFreeSpaceExW(PWChar(wPath), FreeSize, TotalSize, nil);
end;
{$ENDIF}

function GetDiskMaxFileSize(const Path: UTF8String): Int64;
{$IFDEF UNIX}
const
  MSDOS_SUPER_MAGIC = $4d44;
var
  sbfs: TStatFS;
begin
  Result := High(Int64);
  if (fpStatFS(PChar(UTF8ToSys(Path)), @sbfs) = 0) then
  begin
    {$IFDEF BSD}
    if (sbfs.ftype = MSDOS_SUPER_MAGIC) then
    {$ELSE}
    if (sbfs.fstype = MSDOS_SUPER_MAGIC) then
    {$ENDIF}
      Result:= $FFFFFFFF; // 4 Gb
  end;
end;
{$ELSE}
var
 lpVolumeNameBuffer,
 lpFileSystemNameBuffer  : array [0..255] of WideChar;
 lpMaximumComponentLength,
 lpFileSystemFlags     : DWORD;
begin
 Result := High(Int64);
 if GetVolumeInformationW(PWideChar(UTF8Decode(ExtractFileDrive(Path)) + PathDelim),
                         lpVolumeNameBuffer, SizeOf(lpVolumeNameBuffer),
                         nil,
                         lpMaximumComponentLength,
                         lpFileSystemFlags,
                         lpFileSystemNameBuffer, SizeOf(lpFileSystemNameBuffer)) then
  begin
    if SameText(lpFileSystemNameBuffer, 'FAT') then
      Result:= $80000000 // 2 Gb
    else if SameText(lpFileSystemNameBuffer, 'FAT32') then
      Result:= $FFFFFFFF; // 4 Gb
  end;
end;
{$ENDIF}

function CreateHardLink(const Path, LinkName: String) : Boolean;
{$IFDEF MSWINDOWS}
var
  wsPath, wsLinkName: WideString;
begin
  Result:= True;
  try
    wsPath:= UTF8Decode(Path);
    wsLinkName:= UTF8Decode(LinkName);
    Result:= uNTFSLinks.CreateHardlink(wsPath, wsLinkName);
  except
    Result:= False;
  end;
end;
{$ELSE}
begin
  Result := (fplink(PChar(UTF8ToSys(Path)),PChar(UTF8ToSys(LinkName)))=0);
end;
{$ENDIF}

function CreateSymLink(const Path, LinkName: string) : Boolean;
{$IFDEF MSWINDOWS}
var
  wsPath, wsLinkName: WideString;
begin
  Result := True;
  try
    wsPath:= UTF8Decode(Path);
    wsLinkName:= UTF8Decode(LinkName);
    Result:= uNTFSLinks.CreateSymlink(wsPath, wsLinkName);
  except
    Result := False;
  end;
end;
{$ELSE}
begin
  Result := (fpsymlink(PChar(UTF8ToSys(Path)),PChar(UTF8ToSys(LinkName)))=0);
end;
{$ENDIF}

(* Get symlink target *)

function ReadSymLink(const LinkName : String) : String;
{$IFDEF MSWINDOWS}
var
  wsLinkName,
  wsTarget: WideString;
  LinkType: TReparsePointType;
begin
  try
    wsLinkName:= UTF8Decode(LinkName);
    if uNTFSLinks.GetSymlinkInfo(wsLinkName, wsTarget, LinkType) then
      Result := UTF8Encode(wsTarget)
    else
      Result := '';
  except
    Result := '';
  end;
end;
{$ELSE}
begin
  Result := SysToUTF8(fpReadlink(UTF8ToSys(LinkName)));
end;
{$ENDIF}

function mbReadAllLinks(const PathToLink: String) : String;
var
  Attrs: TFileAttrs;
  LinkTargets: TStringList;  // A list of encountered filenames (for detecting cycles)

  function mbReadAllLinksRec(const PathToLink: String): String;
  begin
    Result := ReadSymLink(PathToLink);
    if Result <> '' then
    begin
      if GetPathType(Result) <> ptAbsolute then
        Result := GetAbsoluteFileName(ExtractFilePath(PathToLink), Result);

      if LinkTargets.IndexOf(Result) >= 0 then
      begin
        // Link already encountered - links form a cycle.
        Result := '';
{$IFDEF UNIX}
        fpseterrno(ESysELOOP);
{$ENDIF}
        Exit;
      end;

      Attrs := mbFileGetAttr(Result);
      if (Attrs <> faInvalidAttributes) then
      begin
        if FPS_ISLNK(Attrs) then
        begin
          // Points to a link - read recursively.
          LinkTargets.Add(Result);
          Result := mbReadAllLinksRec(Result);
        end;
        // else points to a file/dir
      end
      else
      begin
        Result := '';  // Target of link doesn't exist
{$IFDEF UNIX}
        fpseterrno(ESysENOENT);
{$ENDIF}
      end;
    end;
  end;

begin
  LinkTargets := TStringList.Create;
  try
    Result := mbReadAllLinksRec(PathToLink);
  finally
    FreeAndNil(LinkTargets);
  end;
end;

(* Return home directory*)

function GetHomeDir : String;
{$IFDEF MSWINDOWS}
var
  iSize: Integer;
  wHomeDir: WideString;
begin
  iSize:= GetEnvironmentVariableW('USERPROFILE', nil, 0);
  if iSize > 0 then
    begin
      SetLength(wHomeDir, iSize);
      GetEnvironmentVariableW('USERPROFILE', PWChar(wHomeDir), iSize);
    end;
  Delete(wHomeDir, iSize, 1);
  Result:= ExcludeBackPathDelimiter(UTF8Encode(wHomeDir));
end;
{$ELSE}
begin
  Result:= ExcludeBackPathDelimiter(SysToUTF8(GetEnvironmentVariable('HOME')));
end;
{$ENDIF}

function GetShell : String;
{$IFDEF MSWINDOWS}
var
  iSize: Integer;
  wShell: WideString;
begin
  iSize:= GetEnvironmentVariableW('ComSpec', nil, 0);
  if iSize > 0 then
    begin
      SetLength(wShell, iSize);
      GetEnvironmentVariableW('ComSpec', PWChar(wShell), iSize);
    end;
  Delete(wShell, iSize, 1);
  Result:= UTF8Encode(wShell);
end;
{$ELSE}
begin
  Result:= SysToUTF8(GetEnvironmentVariable('SHELL'));
end;
{$ENDIF}

function FormatShell(const Command: String): String;
begin
{$IF DEFINED(UNIX)}
  Result := Format('%s -c %s', [GetShell, QuoteSingle(Command)]);
{$ELSEIF DEFINED(MSWINDOWS)}
  Result := Format('%s /C %s', [GetShell, QuoteDouble(Command)]);
{$ENDIF}
end;

function FormatTerminal(Command: String; bKeepTerminalOpen: Boolean): String;
begin
{$IF DEFINED(UNIX)}
  if bKeepTerminalOpen then
    Command := Command + '; echo -n Press ENTER to exit... ; read a';
  Result := Format('%s %s', [gRunInTerm, QuoteSingle(Command)]);
{$ELSEIF DEFINED(MSWINDOWS)}
  // TODO: See if keeping terminal window open can be implemented on Windows.
  Result := Format('%s %s', [gRunInTerm, QuoteDouble(Command)]);
{$ENDIF}
end;

function GetAppConfigDir: String;
{$IF DEFINED(MSWINDOWS)}
const
  SHGFP_TYPE_CURRENT = 0;
var
  wPath: array[0..MAX_PATH-1] of WideChar;
  wUser: WideString;
  dwLength: DWORD;
begin
  if SUCCEEDED(SHGetFolderPathW(0, CSIDL_APPDATA or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT, @wPath[0])) or
     SUCCEEDED(SHGetFolderPathW(0, CSIDL_LOCAL_APPDATA or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT, @wPath[0])) then
  begin
    Result := UTF8Encode(WideString(wPath));
  end
  else
  begin
    dwLength := UNLEN + 1;
    SetLength(wUser, dwLength);
    if GetUserNameW(PWideChar(wUser), @dwLength) then
    begin
      SetLength(wUser, dwLength - 1);
      Result := GetTempDir + UTF8Encode(wUser);
    end
    else
      Result := EmptyStr;
  end;
  if Result <> '' then
    Result := Result + DirectorySeparator + ApplicationName;
end;
{$ELSEIF DEFINED(DARWIN)}
begin
  Result:= GetHomeDir + '/Library/Preferences/' + ApplicationName;
end;
{$ELSE}
var
  uinfo: PPasswordRecord;
begin
  uinfo:= getpwuid(fpGetUID);
  if (uinfo <> nil) and (uinfo^.pw_dir <> '') then
    Result:= SysToUTF8(uinfo^.pw_dir) + '/.config/' + ApplicationName
  else
    Result:= ExcludeTrailingPathDelimiter(SysToUTF8(SysUtils.GetAppConfigDir(False)));
end;
{$ENDIF}

function GetAppCacheDir: UTF8String;
{$IF DEFINED(MSWINDOWS)}
var
  APath: array[0..MAX_PATH] of WideChar;
begin
  if SHGetSpecialFolderPathW(0, APath, CSIDL_LOCAL_APPDATA, True) then
    Result:= UTF8Encode(WideString(APath)) + DirectorySeparator + ApplicationName
  else
    Result:= GetAppConfigDir;
end;
{$ELSEIF DEFINED(DARWIN)}
begin
  Result:= GetHomeDir + '/Library/Caches/' + ApplicationName;
end;
{$ELSE}
var
  uinfo: PPasswordRecord;
begin
  uinfo:= getpwuid(fpGetUID);
  if (uinfo <> nil) and (uinfo^.pw_dir <> '') then
    Result:= SysToUTF8(uinfo^.pw_dir) + '/.cache/' + ApplicationName
  else
    Result:= GetHomeDir + '/.cache/' + ApplicationName;
end;
{$ENDIF}

function GetTempFolder: String;
begin
  Result:= GetTempDir + '_dc';
  if not mbDirectoryExists(Result) then
    mbCreateDir(Result);
  Result:= Result + PathDelim;
end;

function GetSfxExt: String;
{$IFDEF MSWINDOWS}
begin
  Result:= '.exe';
end;
{$ELSE}
begin
  Result:= '.run';
end;
{$ENDIF}

function IsAvailable(Drive: PDrive; TryMount: Boolean): Boolean;
{$IF DEFINED(MSWINDOWS)}
var
  Drv: String;
  DriveLabel: String;
  NetResource: TNetResourceW;
  wsLocalName, wsRemoteName: WideString;
begin
  Drv:= ExtractFileDrive(Drive^.Path) + PathDelim;

  // Try to close CD/DVD drive
  if (GetDriveType(PChar(Drv)) = DRIVE_CDROM) and
     TryMount and (not mbDriveReady(Drv)) then
    begin
       DriveLabel:= mbGetVolumeLabel(Drv, False);
       mbCloseCD(Drv);
       if mbDriveReady(Drv) then
         mbWaitLabelChange(Drv, DriveLabel);
    end;
  // Try to connect to mapped network drive
  if (Drive^.DriveType = dtNetwork) and
     TryMount and (not mbDriveReady(Drv)) then
    begin
      wsLocalName  := UTF8Decode(ExtractFileDrive(Drive^.Path));
      wsRemoteName := UTF8Decode(Drive^.DriveLabel);
      FillChar(NetResource, SizeOf(NetResource), #0);
      NetResource.dwType:= RESOURCETYPE_DISK;
      NetResource.lpLocalName:= PWideChar(wsLocalName);
      NetResource.lpRemoteName:= PWideChar(wsRemoteName);
      WNetAddConnection2W(NetResource, nil, nil, CONNECT_INTERACTIVE);
    end;
  Result:= mbDriveReady(Drv);
end;
{$ELSEIF DEFINED(DARWIN)}
begin
  // Because we show under Mac OS X only mounted volumes
  Result:= True;
end;
{$ELSEIF DEFINED(LINUX)}
var
  mtab: PIOFile;
  pme: PMountEntry;
begin
  Result:= False;
  mtab:= setmntent(_PATH_MOUNTED,'r');
  if not Assigned(mtab) then exit;
  pme:= getmntent(mtab);
  while (pme <> nil) do
  begin
    if SysToUTF8(pme.mnt_dir) = Drive^.Path then
    begin
      Result:= True;
      Break;
    end;
    pme:= getmntent(mtab);
  end;
  endmntent(mtab);

  if not Result and TryMount then
    Result := MountDrive(Drive);
end;
{$ELSE}
begin
  Result:= True;
end;
{$ENDIF}

function mbFileGetAttrNoLinks(const FileName: UTF8String): TFileAttrs;
{$IFDEF UNIX}
var
  Info: BaseUnix.Stat;
begin
  if fpStat(UTF8ToSys(FileName), Info) >= 0 then
    Result := Info.st_mode
  else
    Result := faInvalidAttributes;
end;
{$ELSE}
var
  LinkTarget: UTF8String;
begin
  LinkTarget := mbReadAllLinks(FileName);
  if LinkTarget <> '' then
    Result := mbFileGetAttr(LinkTarget)
  else
    Result := faInvalidAttributes;
end;
{$ENDIF}

function mbFileNameToSysEnc(const LongPath: UTF8String): String;
{$IFDEF MSWINDOWS}
begin
  Result:= UTF8ToSys(LongPath);
  if Pos('?', Result) <> 0 then
    mbGetShortPathName(LongPath, Result);
end;
{$ELSE}
begin
  Result:= UTF8ToSys(LongPath);
end;
{$ENDIF}

function mbGetEnvironmentVariable(const sName: UTF8String): UTF8String;
{$IFDEF MSWINDOWS}
var
  wsName: WideString;
  smallBuf: array[0..1023] of WideChar;
  largeBuf: PWideChar;
  dwResult: DWORD;
begin
  Result := EmptyStr;
  wsName := UTF8Decode(sName);
  dwResult := GetEnvironmentVariableW(PWideChar(wsName), @smallBuf[0], Length(smallBuf));
  if dwResult > Length(smallBuf) then
  begin
    // Buffer not large enough.
    largeBuf := GetMem(SizeOf(WideChar) * dwResult);
    if Assigned(largeBuf) then
    try
      dwResult := GetEnvironmentVariableW(PWideChar(wsName), largeBuf, dwResult);
      if dwResult > 0 then
        Result := UTF8Encode(WideString(largeBuf));
    finally
      FreeMem(largeBuf);
    end;
  end
  else if dwResult > 0 then
    Result := UTF8Encode(WideString(smallBuf));
end;
{$ELSE}
begin
  Result:= SysToUTF8(GetEnvironmentVariable(UTF8ToSys(sName)));
end;
{$ENDIF}

function mbSetEnvironmentVariable(const sName, sValue: UTF8String): Boolean;
{$IFDEF MSWINDOWS}
var
  wsName,
  wsValue: WideString;
begin
  wsName:= UTF8Decode(sName);
  wsValue:= UTF8Decode(sValue);
  Result:= SetEnvironmentVariableW(PWideChar(wsName), PWideChar(wsValue));
end;
{$ELSE}
begin
  Result:= (setenv(PChar(UTF8ToSys(sName)), PChar(UTF8ToSys(sValue)), 1) = 0);
end;
{$ENDIF}

function ExtractRootDir(const FileName: UTF8String): UTF8String;
{$IFDEF UNIX}
begin
  Result:= ExcludeTrailingPathDelimiter(FindMountPointPath(ExcludeTrailingPathDelimiter(FileName)));
end;
{$ELSE}
begin
  Result:= ExtractFileDrive(FileName);
end;
{$ENDIF}

procedure FixFormIcon(Handle: LCLType.HWND);
begin
  // Workaround for Lazarus issue 0018484.
  // Any form that sets its own icon should call this in FormCreate.
{$IFDEF WINDOWS}
  Windows.SetClassLong(Handle, GCL_HICONSM, 0);
  Windows.SetClassLong(Handle, GCL_HICON, 0);
{$ENDIF}
end;

procedure HideConsoleWindow;
begin
{$IFDEF WINDOWS}
  if isConsole then ShowWindow(GetConsoleWindow, SW_HIDE);
{$ENDIF}
end;

procedure FixDateNamesToUTF8;
var
  i: Integer;
begin
  for i := Low(ShortMonthNames) to High(ShortMonthNames) do
    ShortMonthNames[i] := SysToUTF8(ShortMonthNames[i]);
  for i := Low(ShortDayNames) to High(ShortDayNames) do
    ShortDayNames[i] := SysToUTF8(ShortDayNames[i]);
  for i := Low(LongMonthNames) to High(LongMonthNames) do
    LongMonthNames[i] := SysToUTF8(LongMonthNames[i]);
  for i := Low(LongDayNames) to High(LongDayNames) do
    LongDayNames[i] := SysToUTF8(LongDayNames[i]);
end;

{ EInvalidQuoting }

constructor EInvalidQuoting.Create;
begin
  inherited Create(rsMsgInvalidQuoting);
end;

end.
