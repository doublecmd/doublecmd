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
    SysUtils, Classes, LCLType, uClassesEx, uTypes, uDrive
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
  TFileMapRec = record
    FileHandle : System.THandle;
    FileSize : Int64;
{$IFDEF MSWINDOWS}
    MappingHandle : System.THandle;
{$ENDIF}
    MappedFile : Pointer;
  end;

  EInvalidCommandLine = class(Exception);
  EInvalidQuoting = class(EInvalidCommandLine)
    constructor Create; reintroduce;
  end;

const
  faInvalidAttributes: TFileAttrs = TFileAttrs(-1);

{en
   Is file a directory
   @param(iAttr File attributes)
   @returns(@true if file is a directory, @false otherwise)
}
function FPS_ISDIR(iAttr: TFileAttrs) : Boolean;
{en
   Is file a symbolic link
   @param(iAttr File attributes)
   @returns(@true if file is a symbolic link, @false otherwise)
}
function FPS_ISLNK(iAttr: TFileAttrs) : Boolean;
{en
   Is file executable
   @param(sFileName File name)
   @returns(@true if file is executable, @false otherwise)
}
function FileIsExeLib(const sFileName : String) : Boolean;
{en
   Copies a file attributes (attributes, date/time, owner & group, permissions).
   @param(sSrc String expression that specifies the name of the file to be copied)
   @param(sDst String expression that specifies the target file name)
   @param(bDropReadOnlyFlag Drop read only attribute if @true)
   @returns(The function returns @true if successful, @false otherwise)
}
function FileIsReadOnly(iAttr: TFileAttrs): Boolean;
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

{en
   Returns path to a temporary name. It ensures that returned path doesn't exist,
   i.e., there is no filesystem entry by that name.
   If it could not create a unique temporary name then it returns empty string.

   @param(PathPrefix
          This parameter is added at the beginning of each path that is tried.
          The directories in this path are not created if they don't exist.
          If it is empty then the system temporary directory is used.
          For example:
            If PathPrefix is '/tmp/myfile' then files '/tmp/myfileXXXXXX' are tried.
            The path '/tmp' must already exist.)
}
function GetTempName(PathPrefix: String): String;
{en
   Get the system specific self extracting archive extension
   @returns(Self extracting archive extension)
}
function GetSfxExt: String;

function IsAvailable(Drive: PDrive; TryMount: Boolean = True) : Boolean;

(* File mapping/unmapping routines *)
{en
   Create memory map of a file
   @param(sFileName Name of file to mapping)
   @param(FileMapRec TFileMapRec structure)
   @returns(The function returns @true if successful, @false otherwise)
}
function MapFile(const sFileName : UTF8String; out FileMapRec : TFileMapRec) : Boolean;
{en
   Unmap previously mapped file
   @param(FileMapRec TFileMapRec structure)
}
procedure UnMapFile(var FileMapRec : TFileMapRec);

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
   Convert from console to UTF8 encoding.
}
function ConsoleToUTF8(const Str: AnsiString): UTF8String;

{ File handling functions}
function mbFileOpen(const FileName: UTF8String; Mode: Longint): System.THandle;
function mbFileCreate(const FileName: UTF8String): System.THandle; overload;
function mbFileCreate(const FileName: UTF8String; ShareMode: Longint): System.THandle; overload;
function mbFileCreate(const FileName: UTF8String; ShareMode: Longint; Rights: Longint): System.THandle; overload;
function mbFileAge(const FileName: UTF8String): uTypes.TFileTime;
// On success returns True.
function mbFileGetTime(const FileName: UTF8String;
                       var ModificationTime: uTypes.TFileTime;
                       var CreationTime    : uTypes.TFileTime;
                       var LastAccessTime  : uTypes.TFileTime): Boolean;
// On success returns True.
function mbFileSetTime(const FileName: UTF8String;
                       ModificationTime: uTypes.TFileTime;
                       CreationTime    : uTypes.TFileTime = 0;
                       LastAccessTime  : uTypes.TFileTime = 0): Boolean;
{en
   Checks if a given file exists - it can be a real file or a link to a file,
   but it can be opened and read from.
   Even if the result is @false, we can't be sure a file by that name can be created,
   because there may still exist a directory or link by that name.
}
function mbFileExists(const FileName: UTF8String): Boolean;
function mbFileAccess(const FileName: UTF8String; Mode: Integer): Boolean;
function mbFileGetAttr(const FileName: UTF8String): TFileAttrs;
function mbFileSetAttr (const FileName: UTF8String; Attr: TFileAttrs) : LongInt;
{en
   Same as mbFileGetAttr, but dereferences any encountered links.
}
function mbFileGetAttrNoLinks(const FileName: UTF8String): TFileAttrs;
function mbFileCopyAttr(const sSrc, sDst: UTF8String; bDropReadOnlyFlag : Boolean): Boolean;
// Returns True on success.
function mbFileSetReadOnly(const FileName: UTF8String; ReadOnly: Boolean): Boolean;
function mbDeleteFile(const FileName: UTF8String): Boolean;

function mbRenameFile(const OldName: UTF8String; NewName: UTF8String): Boolean;
function mbFileSize(const FileName: UTF8String): Int64;
function FileFlush(Handle: System.THandle): Boolean;
{ Directory handling functions}
function mbGetCurrentDir: UTF8String;
function mbSetCurrentDir(const NewDir: UTF8String): Boolean;
{en
   Checks if a given directory exists - it may be a real directory or a link to directory.
   Even if the result is @false, we can't be sure a directory by that name can be created,
   because there may still exist a file or link by that name.
}
function mbDirectoryExists(const Directory : UTF8String) : Boolean;
function mbCreateDir(const NewDir: UTF8String): Boolean;
function mbRemoveDir(const Dir: UTF8String): Boolean;
{en
   Checks if any file system entry exists at given path.
   It can be file, directory, link, etc. (links are not followed).
}
function mbFileSystemEntryExists(const Path: UTF8String): Boolean;
{en
   Convert file name to system encoding, if name can not be represented in
   current locale then use short file name under Windows.
}
function mbFileNameToSysEnc(const LongPath: UTF8String): String;
function mbCompareFileNames(const FileName1, FileName2: UTF8String): Boolean;
{ Other functions }
function mbGetEnvironmentString(Index : Integer) : UTF8String;
function mbGetEnvironmentVariable(const sName: UTF8String): UTF8String;
function mbSetEnvironmentVariable(const sName, sValue: UTF8String): Boolean;
function mbLoadLibrary(const Name: UTF8String): TLibHandle;
function mbSysErrorMessage(ErrorCode: Integer): UTF8String;
function HasPathInvalidCharacters(Path: UTF8String): Boolean;
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
  FileUtil, uDebug, uDCUtils, uGlobs, uLng
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

{$IF DEFINED(MSWINDOWS)}
const
  AccessModes: array[0..2] of DWORD  = (
                GENERIC_READ,
                GENERIC_WRITE,
                GENERIC_READ or GENERIC_WRITE);
  ShareModes: array[0..4] of DWORD = (
               0,
               0,
               FILE_SHARE_READ,
               FILE_SHARE_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE);
{$ELSEIF DEFINED(UNIX)}
const
  AccessModes: array[0..2] of LongInt  = (
                O_RdOnly,
                O_WrOnly,
                O_RdWr);
{$ENDIF}

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


function SetModeReadOnly(mode: TMode; ReadOnly: Boolean): TMode;
begin
  mode := mode and not (S_IWUSR or S_IWGRP or S_IWOTH);
  if ReadOnly = False then
  begin
    if (mode AND S_IRUSR) = S_IRUSR then
      mode := mode or S_IWUSR;
    if (mode AND S_IRGRP) = S_IRGRP then
      mode := mode or S_IWGRP;
    if (mode AND S_IROTH) = S_IROTH then
      mode := mode or S_IWOTH;
  end;
  Result := mode;
end;

{$ENDIF}

(*Is Directory*)

function  FPS_ISDIR(iAttr: TFileAttrs) : Boolean; inline;
{$IFDEF MSWINDOWS}
begin
  Result := (iAttr and FILE_ATTRIBUTE_DIRECTORY <> 0);
end;
{$ELSE}
begin
  Result := BaseUnix.FPS_ISDIR(iAttr);
end;
{$ENDIF}

(*Is Link*)

function FPS_ISLNK(iAttr: TFileAttrs) : Boolean; inline;
{$IFDEF MSWINDOWS}
begin
  Result := (iAttr and FILE_ATTRIBUTE_REPARSE_POINT <> 0);
end;
{$ELSE}
begin
  Result := BaseUnix.FPS_ISLNK(iAttr);
end;
{$ENDIF}

function FileIsExeLib(const sFileName : String) : Boolean;
var
  fsExeLib : TFileStreamEx;
{$IFDEF MSWINDOWS}
  Sign : Word;
{$ELSE}
  Sign : DWord;
{$ENDIF}
begin
  Result := False;
  if mbFileExists(sFileName) and (mbFileSize(sFileName) >= SizeOf(Sign)) then
    begin
      fsExeLib := TFileStreamEx.Create(sFileName, fmOpenRead or fmShareDenyNone);
      try
        {$IFDEF MSWINDOWS}
        Sign := fsExeLib.ReadWord;
        Result := (Sign = $5A4D);
        {$ELSE}
        Sign := fsExeLib.ReadDWord;
        Result := (Sign = $464C457F);
        {$ENDIF}
      finally
        fsExeLib.Free;
      end;
    end;
end;

function FileIsReadOnly(iAttr: TFileAttrs): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result:= (iAttr and faReadOnly) <> 0;
end;
{$ELSE}
begin
  Result:= (((iAttr AND S_IRUSR) = S_IRUSR) and ((iAttr AND S_IWUSR) <> S_IWUSR));
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

function mbFileCopyAttr(const sSrc, sDst: UTF8String; bDropReadOnlyFlag : Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  Attr : TFileAttrs;
  ModificationTime, CreationTime, LastAccessTime: uTypes.TFileTime;
begin
  Attr := mbFileGetAttr(sSrc);
  if Attr <> faInvalidAttributes then
  begin
    if bDropReadOnlyFlag and ((Attr and faReadOnly) <> 0) then
      Attr := (Attr and not faReadOnly);
    Result := (mbFileSetAttr(sDst, Attr) = 0);
  end
  else
    Result := False;

  if mbFileGetTime(sSrc, ModificationTime, CreationTime, LastAccessTime) then
    Result := mbFileSetTime(sDst, ModificationTime, CreationTime, LastAccessTime) and Result
  else
    Result := False;
end;
{$ELSE}  // *nix
var
  StatInfo : BaseUnix.Stat;
  utb : BaseUnix.TUTimBuf;
  mode : TMode;
begin
  Result := fpLStat(PChar(UTF8ToSys(sSrc)), StatInfo) >= 0;
  if Result then
  begin
    if FPS_ISLNK(StatInfo.st_mode) then
    begin
      // Only group/owner can be set for links.
      if fpLChown(PChar(UTF8ToSys(sDst)), StatInfo.st_uid, StatInfo.st_gid)=-1 then
      begin
        DCDebug(Format('chown (%s) failed',[sDst]));
        Result := False;
      end;
    end
    else
    begin
    // file time
      utb.actime  := StatInfo.st_atime;  // last access time
      utb.modtime := StatInfo.st_mtime;  // last modification time
      Result := (fputime(PChar(UTF8ToSys(sDst)), @utb) = 0) and Result;

    // owner & group
      if fpChown(PChar(UTF8ToSys(sDst)), StatInfo.st_uid, StatInfo.st_gid)=-1 then
      begin
        DCDebug(Format('chown (%s) failed',[sDst]));
        Result := False;
      end;
    // mode
      mode := StatInfo.st_mode;
      if bDropReadOnlyFlag then
        mode := SetModeReadOnly(mode, False);
      if fpChmod(PChar(UTF8ToSys(sDst)), mode) = -1 then
      begin
        DCDebug(Format('chmod (%s) failed',[sDst]));
        Result := False;
      end;
    end;
  end;
end;
{$ENDIF}

(* Execute external commands *)

function ExecCmdFork(sCmdLine:String; bTerm : Boolean; sTerm : String; bKeepTerminalOpen: Boolean) : Boolean;
{$IFDEF UNIX}
var
  Command : String;
  pid : LongInt;
  Args : TOpenStringArray;
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
  DCDebug('File: ' + sFileName + ' Params: ' + sParams + ' WorkDir: ' + wWorkDir);
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

function GetTempName(PathPrefix: String): String;
const
  MaxTries = 100;
var
  TryNumber: Integer = 0;
begin
  if PathPrefix = '' then
    PathPrefix := GetTempDir;
  repeat
    Result := PathPrefix + IntToStr(System.Random(MaxInt)); // or use CreateGUID()
    Inc(TryNumber);
    if TryNumber = MaxTries then
      Exit('');
  until not mbFileSystemEntryExists(Result);
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
      FillChar(NetResource, SizeOf(NetResource), #0);
      NetResource.dwType:= RESOURCETYPE_DISK;
      NetResource.lpLocalName:= PWideChar(UTF8Decode(ExtractFileDrive(Drive^.Path)));
      NetResource.lpRemoteName:= PWideChar(UTF8Decode(Drive^.DriveLabel));
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

function MapFile(const sFileName : UTF8String; out FileMapRec : TFileMapRec) : Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := False;
  with FileMapRec do
    begin
      MappedFile := nil;
      MappingHandle := 0;
      FileHandle := feInvalidHandle;

      FileSize := mbFileSize(sFileName);
      if FileSize = 0 then Exit;   // Cannot map empty files

      FileHandle := mbFileOpen(sFileName, fmOpenRead);
      if FileHandle = feInvalidHandle then Exit;

      MappingHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
      if MappingHandle <> 0 then
      begin
        MappedFile := MapViewOfFile(MappingHandle, FILE_MAP_READ, 0, 0, 0);
        if not Assigned(MappedFile) then
        begin
          UnMapFile(FileMapRec);
          Exit;
        end;
      end
      else
        begin
          UnMapFile(FileMapRec);
          Exit;
        end;
    end;
  Result := True;
end;
{$ELSE}
var
  StatInfo: BaseUnix.Stat;
begin
  Result:= False;
  with FileMapRec do
    begin
      MappedFile := nil;
      FileHandle:= fpOpen(PChar(UTF8ToSys(sFileName)), O_RDONLY);

      if FileHandle = feInvalidHandle then Exit;
      if fpfstat(FileHandle, StatInfo) <> 0 then
        begin
          UnMapFile(FileMapRec);
          Exit;
        end;

      FileSize := StatInfo.st_size;
      if FileSize = 0 then // Cannot map empty files
      begin
        UnMapFile(FileMapRec);
        Exit;
      end;

      MappedFile:= fpmmap(nil,FileSize,PROT_READ, MAP_PRIVATE{SHARED},FileHandle,0 );
      if MappedFile = MAP_FAILED then
        begin
          MappedFile := nil;
          UnMapFile(FileMapRec);
          Exit;
        end;
    end;
  Result := True;
end;
{$ENDIF}

procedure UnMapFile(var FileMapRec : TFileMapRec);
{$IFDEF MSWINDOWS}
begin
  with FileMapRec do
    begin
      if Assigned(MappedFile) then
      begin
        UnmapViewOfFile(MappedFile);
        MappedFile := nil;
      end;

      if MappingHandle <> 0 then
      begin
        CloseHandle(MappingHandle);
        MappingHandle := 0;
      end;

      if FileHandle <> feInvalidHandle then
      begin
        FileClose(FileHandle);
        FileHandle := feInvalidHandle;
      end;
    end;
end;
{$ELSE}
begin
  with FileMapRec do
    begin
      if FileHandle <> feInvalidHandle then
      begin
        fpClose(FileHandle);
        FileHandle := feInvalidHandle;
      end;

      if Assigned(MappedFile) then
      begin
        fpmunmap(MappedFile,FileSize);
        MappedFile := nil;
      end;
    end;
end;
{$ENDIF}  

function ConsoleToUTF8(const Str: AnsiString): UTF8String;
{$IFDEF MSWINDOWS}
var
  Dst: PChar;
{$ENDIF}
begin
  Result:= Str;
  {$IFDEF MSWINDOWS}
  Dst:= AllocMem((Length(Result) + 1) * SizeOf(Char));
  if OEMToChar(PChar(Result), Dst) then
    Result:= SysToUTF8(Dst);
  FreeMem(Dst);
  {$ENDIF}
end;

function mbFileOpen(const FileName: UTF8String; Mode: Longint): System.THandle;
{$IFDEF MSWINDOWS}
begin
  Result:= CreateFileW(PWideChar(UTF8Decode(FileName)), AccessModes[Mode and 3],
                       ShareModes[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL, 0);
end;
{$ELSE}
begin
  Result:= fpOpen(UTF8ToSys(FileName), AccessModes[Mode and 3]);
end;
{$ENDIF}

function mbFileCreate(const FileName: UTF8String): System.THandle;
{$IFDEF MSWINDOWS}
begin
  Result := mbFileCreate(FileName, fmShareDenyWrite, 0);
end;
{$ELSE}
begin
  Result:= fpOpen(UTF8ToSys(FileName), O_Creat or O_RdWr or O_Trunc);
end;
{$ENDIF}

function mbFileCreate(const FileName: UTF8String; ShareMode: Longint): System.THandle;
{$IFDEF MSWINDOWS}
begin
  Result:= mbFileCreate(FileName, ShareMode, 0);
end;
{$ELSE}
begin
  {$IF (FPC_VERSION > 2) or ((FPC_VERSION = 2) and (FPC_RELEASE >= 5))}
  Result:= FileCreate(UTF8ToSys(FileName), ShareMode, 438); // 438 = 666 octal
  {$ELSE}
  Result:= FileCreate(UTF8ToSys(FileName), 438); // 438 = 666 octal
  {$ENDIF}
end;
{$ENDIF}

function mbFileCreate(const FileName: UTF8String; ShareMode: Longint; Rights: Longint): System.THandle;
{$IFDEF MSWINDOWS}
begin
  Result:= CreateFileW(PWideChar(UTF8Decode(FileName)), GENERIC_READ or GENERIC_WRITE,
                       ShareModes[(ShareMode and $F0) shr 4], nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;
{$ELSE}
begin
  {$IF (FPC_VERSION > 2) or ((FPC_VERSION = 2) and (FPC_RELEASE >= 5))}
  Result:= FileCreate(UTF8ToSys(FileName), ShareMode, Rights);
  {$ELSE}
  Result:= FileCreate(UTF8ToSys(FileName), Rights);
  {$ENDIF}
end;
{$ENDIF}

function mbFileAge(const FileName: UTF8String): uTypes.TFileTime;
{$IFDEF MSWINDOWS}
var
  Handle: System.THandle;
  FindData: TWin32FindDataW;
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(FileName);
  Handle := FindFirstFileW(PWChar(wFileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
        Exit(uTypes.TWinFileTime(FindData.ftLastWriteTime));
    end;
  Result:= uTypes.TFileTime(-1);
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  Result:= uTypes.TFileTime(-1);
  if fpStat(UTF8ToSys(FileName), Info) >= 0 then
{$PUSH}{$R-}
    Result := Info.st_mtime;
{$POP}
end;
{$ENDIF}

function mbFileGetTime(const FileName: UTF8String;
                       var ModificationTime: uTypes.TFileTime;
                       var CreationTime    : uTypes.TFileTime;
                       var LastAccessTime  : uTypes.TFileTime): Boolean;
{$IFDEF MSWINDOWS}
var
  Handle: System.THandle;
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(FileName);
  Handle := CreateFileW(PWChar(wFileName),
                        FILE_READ_ATTRIBUTES,
                        FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                        nil,
                        OPEN_EXISTING,
                        FILE_FLAG_BACKUP_SEMANTICS,  // needed for opening directories
                        0);

  if Handle <> INVALID_HANDLE_VALUE then
    begin
      Result := Windows.GetFileTime(Handle,
                                    @CreationTime,
                                    @LastAccessTime,
                                    @ModificationTime);
      CloseHandle(Handle);
    end
  else
    Result := False;
end;
{$ELSE}
var
  StatInfo : BaseUnix.Stat;
begin
  Result := fpLStat(PChar(UTF8ToSys(FileName)), StatInfo) >= 0;
  if Result then
  begin
    LastAccessTime   := StatInfo.st_atime;
    ModificationTime := StatInfo.st_mtime;
    CreationTime     := StatInfo.st_ctime;
  end;
end;
{$ENDIF}

function mbFileSetTime(const FileName: UTF8String;
                       ModificationTime: uTypes.TFileTime;
                       CreationTime    : uTypes.TFileTime = 0;
                       LastAccessTime  : uTypes.TFileTime = 0): Boolean;
{$IFDEF MSWINDOWS}
var
  Handle: System.THandle;
  wFileName: WideString;
  PWinModificationTime: Windows.LPFILETIME = nil;
  PWinCreationTime: Windows.LPFILETIME = nil;
  PWinLastAccessTime: Windows.LPFILETIME = nil;
begin
  wFileName:= UTF8Decode(FileName);
  Handle := CreateFileW(PWChar(wFileName),
                        FILE_WRITE_ATTRIBUTES,
                        FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                        nil,
                        OPEN_EXISTING,
                        FILE_FLAG_BACKUP_SEMANTICS,  // needed for opening directories
                        0);

  if Handle <> INVALID_HANDLE_VALUE then
    begin
      if ModificationTime <> 0 then
      begin
        PWinModificationTime := @ModificationTime;
      end;
      if CreationTime <> 0 then
      begin
        PWinCreationTime := @CreationTime;
      end;
      if LastAccessTime <> 0 then
      begin
        PWinLastAccessTime := @LastAccessTime;
      end;

      Result := Windows.SetFileTime(Handle,
                                    PWinCreationTime,
                                    PWinLastAccessTime,
                                    PWinModificationTime);
      CloseHandle(Handle);
    end
  else
    Result := False;
end;
{$ELSE}
var
  t: TUTimBuf;
begin
  t.actime := LastAccessTime;
  t.modtime := ModificationTime;
  Result := (fputime(PChar(UTF8ToSys(FileName)), @t) <> -1);
end;
{$ENDIF}

function mbFileExists(const FileName: UTF8String) : Boolean;
{$IFDEF MSWINDOWS}
var
  Attr: Dword;
  wFileName: WideString;
begin
  Result:=False;
  wFileName:= UTF8Decode(FileName);
  Attr:= GetFileAttributesW(PWChar(wFileName));
  if Attr <> DWORD(-1) then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) = 0;
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  Result:= False;
  // Can use fpStat, because link to an existing filename can be opened as if it were a real file.
  if fpStat(UTF8ToSys(FileName), Info) >= 0 then
    Result:= fpS_ISREG(Info.st_mode);
end;
{$ENDIF}

function mbFileAccess(const FileName: UTF8String; Mode: Integer): Boolean;
{$IFDEF MSWINDOWS}
const
  AccessMode: array[0..2] of DWORD  = (
                GENERIC_READ,
                GENERIC_WRITE,
                GENERIC_READ or GENERIC_WRITE);
var
  hFile: System.THandle;
  wFileName: WideString;
  dwDesiredAccess: DWORD;
  dwShareMode: DWORD = 0;
begin
  Result:= False;
  wFileName:= UTF8Decode(FileName);
  dwDesiredAccess := AccessMode[Mode and 3];
  if dwDesiredAccess = GENERIC_READ then
    dwShareMode := FILE_SHARE_READ;
  hFile:= CreateFileW(PWChar(wFileName), dwDesiredAccess, dwShareMode,
                      nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile <> INVALID_HANDLE_VALUE then
    begin
      Result:= True;
      FileClose(hFile);
    end;
end;
{$ELSE}
const
  AccessMode: array[0..2] of LongInt  = (
                R_OK,
                W_OK,
                R_OK or W_OK);
begin
  Result:= fpAccess(UTF8ToSys(FileName), AccessMode[Mode and 3]) = 0;
end;
{$ENDIF}

{$IFOPT R+}
{$DEFINE uOSUtilsRangeCheckOn}
{$R-}
{$ENDIF}

function mbFileGetAttr(const FileName: UTF8String): TFileAttrs;
{$IFDEF MSWINDOWS}
var
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(FileName);
  Result := GetFileAttributesW(PWChar(wFileName));
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  Result:= faInvalidAttributes;
  if fpLStat(UTF8ToSys(FileName), @Info) >= 0 then
    Result:= Info.st_mode;
end;
{$ENDIF}

function mbFileSetAttr(const FileName: UTF8String; Attr: TFileAttrs): LongInt;
{$IFDEF MSWINDOWS}
var
  wFileName: WideString;
begin
  Result:= 0;
  wFileName:= UTF8Decode(FileName);
  if not SetFileAttributesW(PWChar(wFileName), Attr) then
    Result:= GetLastError;
end;
{$ELSE}
begin
  Result:= fpchmod(PChar(UTF8ToSys(FileName)), Attr);
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

{$IFDEF uOSUtilsRangeCheckOn}
{$R+}
{$UNDEF uOSUtilsRangeCheckOn}
{$ENDIF}

function mbFileSetReadOnly(const FileName: UTF8String; ReadOnly: Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  iAttr: DWORD;
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(FileName);
  iAttr := GetFileAttributesW(PWChar(wFileName));
  if iAttr = DWORD(-1) then Exit(False);
  if ReadOnly then
    iAttr:= iAttr or faReadOnly
  else
    iAttr:= iAttr and not faReadOnly;
  Result:= SetFileAttributesW(PWChar(wFileName), iAttr) = True;
end;
{$ELSE}
var
  StatInfo: BaseUnix.Stat;
  mode: TMode;
begin
  if fpStat(PChar(UTF8ToSys(FileName)), StatInfo) <> 0 then Exit(False);
  mode := SetModeReadOnly(StatInfo.st_mode, ReadOnly);
  Result:= fpchmod(PChar(UTF8ToSys(FileName)), mode) = 0;
end;
{$ENDIF}

function mbDeleteFile(const FileName: UTF8String): Boolean;
{$IFDEF MSWINDOWS}
var
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(FileName);
  Result:= Windows.DeleteFileW(PWChar(wFileName));
end;
{$ELSE}
begin
  Result:= fpUnLink(UTF8ToSys(FileName)) = 0;
end;
{$ENDIF}

function mbRenameFile(const OldName: UTF8String; NewName: UTF8String): Boolean;
{$IFDEF MSWINDOWS}
var
  wOldName,
  wNewName: WideString;
begin
  wOldName:= UTF8Decode(OldName);
  wNewName:= UTF8Decode(NewName);
  Result:= MoveFileExW(PWChar(wOldName), PWChar(wNewName), MOVEFILE_REPLACE_EXISTING);
end;
{$ELSE}
var
  tmpFileName: UTF8String;
  OldFileStat, NewFileStat: stat;
begin
  if GetPathType(NewName) <> ptAbsolute then
    NewName := ExtractFilePath(OldName) + NewName;

  if OldName = NewName then
    Exit(True);

  if fpLstat(UTF8ToSys(OldName), OldFileStat) <> 0 then
    Exit(False);

  // Check if target file exists.
  if fpLstat(UTF8ToSys(NewName), NewFileStat) = 0 then
  begin
    // Check if source and target are the same files (same inode and same device).
    if (OldFileStat.st_ino = NewFileStat.st_ino) and
       (OldFileStat.st_dev = NewFileStat.st_dev) then
    begin
      // Check number of links.
      // If it is 1 then source and target names most probably differ only
      // by case on a case-insensitive filesystem. Direct rename() in such case
      // fails on Linux, so we use a temporary file name and rename in two stages.
      // If number of links is more than 1 then it's enough to simply unlink
      // the source file, since both files are technically identical.
      // (On Linux rename() returns success but doesn't do anything
      // if renaming a file to its hard link.)
      // We cannot use st_nlink for directories because it means "number of
      // subdirectories"; hard links to directories are not supported on Linux
      // or Windows anyway (on MacOSX they are). Therefore we always treat
      // directories as if they were a single link and rename them using temporary name.

      if (NewFileStat.st_nlink = 1) or BaseUnix.fpS_ISDIR(NewFileStat.st_mode) then
      begin
        tmpFileName := GetTempName(OldName);

        if FpRename(UTF8ToSys(OldName), UTF8ToSys(tmpFileName)) = 0 then
        begin
          if fpLstat(UTF8ToSys(NewName), NewFileStat) = 0 then
          begin
            // We have renamed the old file but the new file name still exists,
            // so this wasn't a single file on a case-insensitive filesystem
            // accessible by two names that differ by case.

            FpRename(UTF8ToSys(tmpFileName), UTF8ToSys(OldName));  // Restore old file.
{$IFDEF DARWIN}
            // If it's a directory with multiple hard links then simply unlink the source.
            if BaseUnix.fpS_ISDIR(NewFileStat.st_mode) and (NewFileStat.st_nlink > 1) then
              Result := (fpUnLink(UTF8ToSys(OldName)) = 0)
            else
{$ENDIF}
            Result := False;
          end
          else if FpRename(UTF8ToSys(tmpFileName), UTF8ToSys(NewName)) = 0 then
          begin
            Result := True;
          end
          else
          begin
            FpRename(UTF8ToSys(tmpFileName), UTF8ToSys(OldName));  // Restore old file.
            Result := False;
          end;
        end
        else
          Result := False;
      end
      else
      begin
        // Multiple links - simply unlink the source file.
        Result := (fpUnLink(UTF8ToSys(OldName)) = 0);
      end;

      Exit;
    end;
  end;
  Result := FpRename(UTF8ToSys(OldName), UTF8ToSys(NewName)) = 0;
end;
{$ENDIF}

function mbFileSize(const FileName: UTF8String): Int64;
{$IFDEF MSWINDOWS}
var
  Handle: System.THandle;
  FindData: TWin32FindDataW;
  wFileName: WideString;
begin
  Result:= 0;
  wFileName:= UTF8Decode(FileName);
  Handle := FindFirstFileW(PWideChar(wFileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
        Int64Rec(Result).Lo:= FindData.nFileSizeLow;
        Int64Rec(Result).Hi:= FindData.nFileSizeHigh;
      end;
    end;
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  Result:= 0;
  if fpStat(UTF8ToSys(FileName), Info) >= 0 then
    Result:= Info.st_size;
end;
{$ENDIF}

function FileFlush(Handle: System.THandle): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result:= FlushFileBuffers(Handle);
end;
{$ELSE}  
begin
  Result:= (fpfsync(Handle) = 0);
end;  
{$ENDIF}
  
function mbGetCurrentDir: UTF8String;
{$IFDEF MSWINDOWS}
var
  iSize: Integer;
  wsDir: WideString;
begin
  Result:= '';
  iSize:= GetCurrentDirectoryW(0, nil);
  if iSize > 0 then
    begin
      SetLength(wsDir, iSize);
      GetCurrentDirectoryW(iSize, PWideChar(wsDir));
      wsDir:= PWideChar(wsDir);
      Result:= UTF8Encode(wsDir);
    end;
end;
{$ELSE}
begin
  GetDir(0, Result);
  Result := SysToUTF8(Result);
end;
{$ENDIF}

function mbSetCurrentDir(const NewDir: UTF8String): Boolean;
{$IFDEF MSWINDOWS}
var
  wNewDir: WideString;
  NetResource: TNetResourceW;
begin
  wNewDir:= UTF8Decode(NewDir);
  if Pos('\\', wNewDir) = 1 then
    begin
      wNewDir:= ExcludeTrailingBackslash(wNewDir);
      FillChar(NetResource, SizeOf(NetResource), #0);
      NetResource.dwType:= RESOURCETYPE_ANY;
      NetResource.lpRemoteName:= PWideChar(wNewDir);
      WNetAddConnection2W(NetResource, nil, nil, CONNECT_INTERACTIVE);
    end;
  Result:= SetCurrentDirectoryW(PWChar(wNewDir));
end;
{$ELSE}
begin
  Result:= fpChDir(PChar(UTF8ToSys(NewDir))) = 0;
end;
{$ENDIF}

function mbDirectoryExists(const Directory: UTF8String) : Boolean;
{$IFDEF MSWINDOWS}
var
  Attr:Dword;
  wDirectory: WideString;
begin
  Result:= False;
  wDirectory:= UTF8Decode(Directory);
  Attr:= GetFileAttributesW(PWChar(wDirectory));
  if Attr <> DWORD(-1) then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) > 0;
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  Result:= False;
  // We can use fpStat here instead of fpLstat, so that True is returned
  // when target is a directory or a link to an existing directory.
  // Note that same behaviour would be achieved by passing paths
  // that end with path delimiter to fpLstat.
  // Paths with links can be used the same way as if they were real directories.
  if fpStat(UTF8ToSys(Directory), Info) >= 0 then
    Result:= fpS_ISDIR(Info.st_mode);
end;
{$ENDIF}

function mbCreateDir(const NewDir: UTF8String): Boolean;
{$IFDEF MSWINDOWS}
var
  wNewDir: WideString;
begin
  wNewDir:= UTF8Decode(NewDir);
  Result:= CreateDirectoryW(PWChar(wNewDir), nil);
end;
{$ELSE}
begin
  Result:= fpMkDir(PChar(UTF8ToSys(NewDir)), $1FF) = 0; // $1FF = &0777
end;
{$ENDIF}

function mbRemoveDir(const Dir: UTF8String): Boolean;
{$IFDEF MSWINDOWS}
var
  wDir: WideString;
begin
  wDir:= UTF8Decode(Dir);
  Result:= RemoveDirectoryW(PWChar(wDir));
end;
{$ELSE}
begin
  Result:= fpRmDir(PChar(UTF8ToSys(Dir))) = 0;
end;
{$ENDIF}

function mbFileSystemEntryExists(const Path: UTF8String): Boolean;
begin
  Result := mbFileGetAttr(Path) <> faInvalidAttributes;
end;

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

function mbCompareFileNames(const FileName1, FileName2: UTF8String): Boolean; inline;
{$IF DEFINED(WINDOWS) OR DEFINED(DARWIN)}
begin
  Result:= (WideCompareText(UTF8Decode(FileName1), UTF8Decode(FileName2)) = 0);
end;
{$ELSE}
begin
  Result:= (WideCompareStr(UTF8Decode(FileName1), UTF8Decode(FileName2)) = 0);
end;
{$ENDIF}

function mbGetEnvironmentString(Index: Integer): UTF8String;
{$IFDEF MSWINDOWS}
var
  hp, p: PWideChar;
begin
  Result:= '';
  p:= GetEnvironmentStringsW;
  hp:= p;
  if (hp <> nil) then
    begin
      while (hp^ <> #0) and (Index > 1) do
        begin
          Dec(Index);
          hp:= hp + lstrlenW(hp) + 1;
        end;
      if (hp^ <> #0) then
        Result:= UTF8Encode(WideString(hp));
    end;
  FreeEnvironmentStringsW(p);
end;
{$ELSE}
begin
  Result:= SysToUTF8(GetEnvironmentString(Index));
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

function mbLoadLibrary(const Name: UTF8String): TLibHandle;
{$IFDEF MSWINDOWS}
var
  wsName: WideString;
begin
  wsName:= UTF8Decode(Name);
  Result:= LoadLibraryW(PWideChar(wsName));
end;
{$ELSE}
begin
  {$PUSH}{$HINTS OFF}
  Result:= TLibHandle(dlopen(PChar(UTF8ToSys(Name)), RTLD_LAZY));
  {$POP}
end;
{$ENDIF}

function mbSysErrorMessage(ErrorCode: Integer): UTF8String;
begin
  Result :=
{$IFDEF WINDOWS}
            UTF8Encode(SysErrorMessage(ErrorCode));
{$ELSE}
            SysToUTF8(SysErrorMessage(ErrorCode));
{$ENDIF}
end;

function HasPathInvalidCharacters(Path: UTF8String): Boolean;
begin
  Result := ContainsOneOf(Path, '*?');
end;

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
