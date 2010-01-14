{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains platform depended functions.

    Copyright (C) 2006-2009  Koblov Alexander (Alexx2000@mail.ru)

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
    SysUtils, Classes, LCLProc, uClassesEx, uTypes
    {$IF DEFINED(MSWINDOWS)}
    , Windows, ShellApi, uNTFSLinks, uMyWindows, JwaWinNetWk
    {$ELSEIF DEFINED(UNIX)}
    , BaseUnix, Unix, UnixType, dl
      {$IFDEF DARWIN}
      , MacOSAll
      {$ELSE}
      , libhal, dbus
      {$ENDIF}
    {$ENDIF};
    
const
  {$IFDEF MSWINDOWS}
  faFolder = faDirectory;
  faSymLink   = $00000400;
  RunTerm = 'cmd.exe';  // default terminal
  RunInTerm = 'cmd.exe /K'; // default run in terminal command
  fmtRunInTerm = '%s "%s"';
  fmtRunInShell = '%s /C "%s"';
  ShieldChar = '/';
  MonoSpaceFont = 'Fixedsys';
  {$ELSE}
  faFolder = S_IFDIR;
  faSymLink   = $00000040;
  RunTerm = 'xterm';  // default terminal
  RunInTerm = 'xterm -e sh -c'; // default run in terminal command
  fmtRunInTerm = '%s ''%s ; echo -n Press ENTER to exit... ; read a''';
  fmtRunInShell = '%s -c ''%s''';
  ShieldChar = '\';
  MonoSpaceFont = 'Monospace';
  {$ENDIF}

type
   TDriveType = (dtUnknown, dtNoDrive, dtRemovable, dtFixed, dtNetwork, dtCDROM,
                 dtRAM, dtFloppy, dtFlash);
    
  TDrive = record
    Name,
    Path,
    DriveLabel :String;
    DriveType : TDriveType;
  end;
  PDrive = ^TDrive;

type
  TFileMapRec = record
    FileHandle : THandle;
    FileSize : Int64;
{$IFDEF MSWINDOWS}
    MappingHandle : THandle;
{$ENDIF}
    MappedFile : PChar;
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
function FileCopyAttr(const sSrc, sDst:String; bDropReadOnlyFlag : Boolean):Boolean;
function ExecCmdFork(sCmdLine:String; bTerm : Boolean = False; sTerm : String = ''):Boolean;
{en
   Opens a file or URL in the user's preferred application
   @param(URL File name or URL)
   @returns(The function returns @true if successful, @false otherwise)
}
function ShellExecute(URL: String): Boolean;
function GetDiskFreeSpace(Path : String; out FreeSize, TotalSize : Int64) : Boolean;
{en
   Create a hard link to a file
   @param(Path Name of file)
   @param(LinkName Name of hard link)
   @returns(The function returns @true if successful, @false otherwise)
}
function CreateHardLink(Path, LinkName: String) : Boolean;
{en
   Create a symbolic link
   @param(Path Name of file)
   @param(LinkName Name of symbolic link)
   @returns(The function returns @true if successful, @false otherwise)
}
function CreateSymLink(Path, LinkName: string) : Boolean;
{en
   Read destination of symbolic link
   @param(LinkName Name of symbolic link)
   @returns(The file name/path the symbolic link name is pointing to.
            The path may be relative to link's location.)
}
function ReadSymLink(LinkName : String) : String;
{en
   Reads the concrete file's name that the link points to.
   If the link points to a link then it's resolved recursively
   until a valid file name that is not a link is found.
   @param(LinkName Name of symbolic link (absolute path))
   @returns(The absolute filename the symbolic link name is pointing to,
            or an empty string when the link is invalid or
            the file it points to does not exist.)
}
function mbReadAllLinks(PathToLink : String) : String;
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

function IsAvailable(Path : String) : Boolean;
function GetAllDrives : TList;
{en
   Destroys drives list created by GetAllDrives.
}
procedure DestroyDrivesList(var ADrivesList: TList);

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

{ File handling functions}
function mbFileOpen(const FileName: UTF8String; Mode: Integer): THandle;
function mbFileCreate(const FileName: UTF8String): THandle; overload;
function mbFileCreate(const FileName: UTF8String; Mode: Integer): THandle; overload;
function mbFileAge(const FileName: UTF8String): uTypes.TFileTime;
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
// Returns True on success.
function mbFileSetReadOnly(const FileName: UTF8String; ReadOnly: Boolean): Boolean;
function mbDeleteFile(const FileName: UTF8String): Boolean;

// 30.04.2009 - this function move files and folders to trash can (Windows implemented).
// 12.05.2009 - added implementation for Linux via gvfs-trash.
function mbDeleteToTrash(const FileName: UTF8String): Boolean;
// 14.05.2009 - this funtion checks 'gvfs-trash' BEFORE deleting. Need for various linux disributives.
function mbCheckTrash(sPath: UTF8String): Boolean;
// ----------------
function mbRenameFile(const OldName, NewName : UTF8String): Boolean;
function mbFileSize(const FileName: UTF8String): Int64;
function FileFlush(Handle: THandle): Boolean;
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
{ Other functions }
function mbCompareText(const s1, s2: UTF8String): PtrInt;
function mbGetEnvironmentString(Index : Integer) : UTF8String;
function mbSetEnvironmentVariable(const sName, sValue: UTF8String): Boolean;
function mbLoadLibrary(Name: UTF8String): TLibHandle;
function mbSysErrorMessage(ErrorCode: Integer): UTF8String;

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
// create all Hal object
procedure CreateHal;
// procedure where we get message add or remove
procedure LibHalDeviceAdded(ctx: PLibHalContext; const udi: PChar);cdecl;
procedure LibHalDeviceRemoved(ctx: PLibHalContext; const udi: PChar);cdecl;
// dispach message and
function CheckHalMsg: Boolean;
// create all Hal object
procedure FreeHal;

var
  DcDbus : PDBusConnection;
  DcCtx : PLibHalContext;
  DeviceWasChange : Boolean;
  DeviceList : TStringList = nil;

{$ENDIF}

implementation

uses
  FileUtil, uDCUtils
  // 30.04.2009 - для удаления в корзину
  {$IFDEF MSWINDOWS}
  , Win32Int, InterfaceBase
  {$ELSE}
  , uMyUnix
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
    FpWaitPid(FPID, nil, 0);
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

function  FPS_ISDIR(iAttr: TFileAttrs) : Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := (iAttr and faDirectory <> 0);
end;
{$ELSE}
begin
  Result := BaseUnix.FPS_ISDIR(iAttr);
end;
{$ENDIF}

(*Is Link*)

function FPS_ISLNK(iAttr: TFileAttrs) : Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := (iAttr and faSymLink <> 0);
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
  wSign : Word;
{$ELSE}
  dwSign : DWord;
{$ENDIF}
begin
  Result := False;
  if mbFileExists(sFileName) and (mbFileSize(sFileName) > 0) then
    begin
      fsExeLib := TFileStreamEx.Create(sFileName, fmOpenRead or fmShareDenyNone);
      try
        {$IFDEF MSWINDOWS}
        wSign := fsExeLib.ReadWord;
        Result := (wSign = $5A4D);
        {$ELSE}
        dwSign := fsExeLib.ReadDWord;
        Result := (dwSign = $464C457F);
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

function FileCopyAttr(const sSrc, sDst:String; bDropReadOnlyFlag : Boolean):Boolean;
{$IFDEF MSWINDOWS}
var
  iAttr : TFileAttrs;
  ft : Windows.TFileTime;
  Handle: THandle;
begin
  iAttr := mbFileGetAttr(sSrc);
  //---------------------------------------------------------
  Handle:= mbFileOpen(sSrc, fmOpenRead or fmShareDenyNone);
  GetFileTime(Handle,nil,nil,@ft);
  FileClose(Handle);
  //---------------------------------------------------------
  if bDropReadOnlyFlag and ((iAttr and faReadOnly) <> 0) then
    iAttr := (iAttr and not faReadOnly);
  Result := (mbFileSetAttr(sDst, iAttr) = 0);
  //---------------------------------------------------------
  Handle:= mbFileOpen(sDst, fmOpenReadWrite or fmShareExclusive);
  Result := SetFileTime(Handle, nil, nil, @ft);
  FileClose(Handle);
end;
{$ELSE}  // *nix
var
  StatInfo : BaseUnix.Stat;
  utb : BaseUnix.PUTimBuf;
  mode : TMode;
begin
  fpLStat(PChar(sSrc), StatInfo);

  if FPS_ISLNK(StatInfo.st_mode) then
  begin
    // Can only set group/owner for links.
    if fpLChown(PChar(sDst),StatInfo.st_uid, StatInfo.st_gid)=-1 then
    begin
      // development messages
      DebugLN(Format('chown (%s) failed',[sDst]));
    end;
  end
  else
  begin
  // file time
    new(utb);
    utb^.actime:=StatInfo.st_atime;  // last access time
    utb^.modtime:=StatInfo.st_mtime; // last modification time
    fputime(PChar(sDst),utb);
    dispose(utb);

  // owner & group
    if fpChown(PChar(sDst),StatInfo.st_uid, StatInfo.st_gid)=-1 then
    begin
      // development messages
      DebugLN(Format('chown (%s) failed',[sDst]));
    end;
    // mod
    mode := StatInfo.st_mode;
    if bDropReadOnlyFlag then
      mode := SetModeReadOnly(mode, False);
    if fpChmod(PChar(sDst), mode) = -1 then
    begin
      // development messages
      DebugLN(Format('chmod (%s) failed',[sDst]));
    end;
  end;

  Result:=True;
end;
{$ENDIF}

(* Execute external commands *)

function ExecCmdFork(sCmdLine:String; bTerm : Boolean; sTerm : String) : Boolean;
{$IFDEF UNIX}
var
  sTempStr : String;
  x, pid : LongInt;
  Command : String;
  Args : TOpenStringArray;
  WaitForPidThread: TWaitForPidThread;
begin
  sTempStr := Trim(sCmdLine);

  if bTerm then
    begin
      x := 1;
      while x <= Length(sTempStr) do begin
        if (sTempStr[x] in [{'"',} '''']) and ((x = 1) or (sTempStr[x - 1] <> ShieldChar)) then
          Insert(ShieldChar, sTempStr, x);
        Inc(x);
      end;
      if sTerm = '' then sTerm := RunInTerm;
      sTempStr := Format(fmtRunInTerm, [sTerm, sTempStr]);
    end;

  SplitArgs(Args, sTempStr);

  if Length(Args) = 0 then Exit;

  Command := Args[0];

  // remove command from args (0th element)
  for x := 1 to Length(Args) - 1 do
    Args[x-1] := RemoveQuotation(Args[x]);
  SetLength(Args, Length(Args) - 1);

  pid := fpFork;

  if pid = 0 then
    begin
      { The child does the actual exec, and then exits }
      FpExecLP(Command, Args);
      { If the FpExecVP fails, we return an exitvalue of 127, to let it be known }
      fpExit(127);
    end
  else if pid = -1 then         { Fork failed }
    begin
      raise Exception.Create('Fork failed: ' + sCmdLine);
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
  DebugLn('File: ' + sFileName + ' Params: ' + sParams + ' WorkDir: ' + wWorkDir);
  wFileName:= UTF8Decode(sFileName);
  wParams:= UTF8Decode(sParams);
  Result := (ShellExecuteW(0, 'open', PWChar(wFileName), PWChar(wParams), PWChar(wWorkDir), SW_SHOW) > 32);
end;
{$ENDIF}

function ShellExecute(URL: String): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result:= ExecCmdFork(Format('"%s"', [URL]));
  if Result = False then
      Result:= ExecCmdFork('rundll32 shell32.dll OpenAs_RunDLL ' + URL);
end;
{$ELSE}
var
  DesktopEnv: Cardinal;
  sCmdLine: String;
begin
  Result:= False;
  sCmdLine:= '';
  DesktopEnv:= GetDesktopEnvironment;
  case DesktopEnv of
  DE_UNKNOWN:
    begin
      if FileIsExecutable(URL) then
      begin
        if GetPathType(URL) <> ptAbsolute then
          sCmdLine := './';
        sCmdLine:= sCmdLine + QuoteStr(URL);
      end
      else
        sCmdLine:= 'xdg-open ' + QuoteStr(URL);
    end;
  DE_KDE:
    sCmdLine:= 'kfmclient exec ' + QuoteStr(URL);
  DE_GNOME:
    sCmdLine:= 'gnome-open ' + QuoteStr(URL);
  DE_XFCE:
    sCmdLine:= 'exo-open ' + QuoteStr(URL);
  end;
  if sCmdLine <> '' then
    Result:= ExecCmdFork(sCmdLine);
end;
{$ENDIF}

(* Get Disk Free Space *)

function GetDiskFreeSpace(Path : String; out FreeSize, TotalSize : Int64) : Boolean;
{$IFDEF UNIX}
var
  sbfs: TStatFS;
begin
    Result:= (fpStatFS(PChar(Path), @sbfs) = 0);
    if not Result then Exit;
    FreeSize := (Int64(sbfs.bavail)*sbfs.bsize);
{$IF DEFINED(CPU32) or (FPC_VERSION>2) or ((FPC_VERSION=2) and ((FPC_RELEASE>2) or ((FPC_RELEASE=2) and (FPC_PATCH>=3))))}
    TotalSize := (Int64(sbfs.blocks)*sbfs.bsize);
{$ENDIF}
end;
{$ELSE}
var
  wPath: WideString;
  OldErrorMode: Word;
begin
  wPath:= UTF8Decode(Path);
  OldErrorMode:= SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  Result:= GetDiskFreeSpaceExW(PWChar(wPath), FreeSize, TotalSize, nil);
  SetErrorMode(OldErrorMode);
end;
{$ENDIF}



function CreateHardLink(Path, LinkName: String) : Boolean;
{$IFDEF MSWINDOWS}
var
  wPath, wLinkName: WideString;
begin
  Result:= True;
  try
    wPath:= UTF8Decode(Path);
    wLinkName:= UTF8Decode(LinkName);
    uNTFSLinks.CreateHardlink(wPath, wLinkName);
  except
    Result:= False;
  end;
end;
{$ELSE}
begin
  Result := (fplink(PChar(@Path[1]),PChar(@LinkName[1]))=0);
end;
{$ENDIF}

function CreateSymLink(Path, LinkName: string) : Boolean;
{$IFDEF MSWINDOWS}
var
  wPath, wLinkName: WideString;
begin
  Result := True;
  try
    wPath:= UTF8Decode(Path);
    wLinkName:= UTF8Decode(LinkName);
    uNTFSLinks.CreateSymlink(wPath, wLinkName);
  except
    Result := False;
  end;
end;
{$ELSE}
begin
  Result := (fpsymlink(PChar(@Path[1]),PChar(@LinkName[1]))=0);
end;
{$ENDIF}

(* Get symlink target *)

function ReadSymLink(LinkName : String) : String;
{$IFDEF MSWINDOWS}
var
  wLinkName,
  wTarget: WideString;
  LinkType: TReparsePointType;
begin
  try
    wLinkName:= UTF8Decode(LinkName);
    if uNTFSLinks.FGetSymlinkInfo(wLinkName, wTarget, LinkType) then
      Result := UTF8Encode(wTarget)
    else
      Result := '';
  except
    Result := '';
  end;
end;
{$ELSE}
begin
  Result := fpReadlink(LinkName);
end;
{$ENDIF}

function mbReadAllLinks(PathToLink: String) : String;
var
  Attrs: TFileAttrs;
  LinkTargets: TStringList;  // A list of encountered filenames (for detecting cycles)

  function mbReadAllLinksRec(PathToLink: String): String;
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
  Result:= UTF8Encode(wHomeDir) + DirectorySeparator;
end;
{$ELSE}
begin
  Result:= GetEnvironmentVariable('HOME')+DirectorySeparator;
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
  Result:= GetEnvironmentVariable('SHELL');
end;
{$ENDIF}

function GetAppConfigDir: String;
{$IFDEF MSWINDOWS}
var
  iSize: Integer;
  wDir: WideString;
begin
  iSize:= GetEnvironmentVariableW('APPDATA', nil, 0);
  if iSize > 0 then
    begin
      SetLength(wDir, iSize);
      GetEnvironmentVariableW('APPDATA', PWChar(wDir), iSize);
    end;
  Delete(wDir, iSize, 1);
  Result:= UTF8Encode(wDir) + DirectorySeparator + ApplicationName;
end;
{$ELSE}
var
  uinfo: PPasswordRecord;
begin
  uinfo:= getpwuid(fpGetUID);
  if (uinfo <> nil) and (uinfo^.pw_dir <> '') then
    Result:= uinfo^.pw_dir + '/.config/' + ApplicationName
  else
    Result:= SysUtils.GetAppConfigDir(False);
end;
{$ENDIF}

function IsAvailable(Path: String): Boolean;
{$IF DEFINED(MSWINDOWS)}
var
  Drv: String;
  DriveLabel: string;
begin
  Drv:= ExtractFileDrive(Path) + PathDelim;

  { Close CD/DVD }
  if (GetDriveType(PChar(Drv)) = DRIVE_CDROM) and
     (not mbDriveReady(Drv)) then
    begin
       DriveLabel:= mbGetVolumeLabel(Drv, False);
       mbCloseCD(Drv);
       if mbDriveReady(Drv) then
         mbWaitLabelChange(Drv, DriveLabel);
    end;
  Result:= mbDriveReady(Drv);
end;
{$ELSEIF DEFINED(DARWIN)}
begin
  // Because we show under Mac OS X only mounted volumes
  Result:= True;
end;
{$ELSE}
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
    if pme.mnt_dir = Path then
    begin
      Result:= True;
      Break;
    end;
    pme:= getmntent(mtab);
  end;
  endmntent(mtab);
end;
{$ENDIF}

(*Return a list of drives in system*)

function GetAllDrives : TList;
{$IF DEFINED(MSWINDOWS)}
var
  Drive : PDrive;
  DriveNum: Integer;
  DriveBits: set of 0..25;
begin
  Result := TList.Create;
  { fill list }
  Integer(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
  begin
    if not (DriveNum in DriveBits) then Continue;
    New(Drive);
    Result.Add(Drive);
    with Drive^ do
    begin
     Name := Char(DriveNum + Ord('a')) + ':\';
     Path := Name;
     DriveType := TDriveType(GetDriveType(PChar(Name)));
     if DriveType = dtRemovable then
       begin
         if (Path[1] in ['a'..'b']) or (Path[1] in ['A'..'B']) then
           DriveType := dtFloppy
         else
           DriveType := dtFlash;
       end;
     case DriveType of
     dtFloppy:
       DriveLabel:= Path;
     dtNetwork:
       DriveLabel:= mbGetRemoteFileName(Path);
     else
       DriveLabel:= mbGetVolumeLabel(Name, True);
     end;
    end;
  end;
end;
{$ELSEIF DEFINED(DARWIN)}
var
  Drive : PDrive;
  osResult: OSStatus;
  volumeIndex: ItemCount;
  actualVolume: FSVolumeRefNum;
  volumeName: HFSUniStr255;
  volumeInfo: FSVolumeInfo;
  volumeParms: GetVolParmsInfoBuffer;
  pb: HParamBlockRec;
  volNameAsCFString: CFStringRef;
  volNameAsCString: array[0..255] of char;
begin
  Result := TList.Create;
  osResult:= noErr;
  // Iterate across all mounted volumes using FSGetVolumeInfo. This will return nsvErr
  // (no such volume) when volumeIndex becomes greater than the number of mounted volumes.
  volumeIndex:= 1;
  while (osResult = noErr) or (osResult <> nsvErr) do
    begin
      FillByte(volumeInfo, SizeOf(volumeInfo), 0);

      // We're mostly interested in the volume reference number (actualVolume)
      osResult:= FSGetVolumeInfo(kFSInvalidVolumeRefNum,
                                 volumeIndex,
                                 @actualVolume,
                                 kFSVolInfoFSInfo,
                                 @volumeInfo,
                                 @volumeName,
                                 nil);

      if (osResult = noErr) then
        begin
          // Use the volume reference number to retrieve the volume parameters. See the documentation
          // on PBHGetVolParmsSync for other possible ways to specify a volume.
          pb.ioNamePtr := nil;
          pb.ioVRefNum := actualVolume;
          pb.ioBuffer := @volumeParms;
          pb.ioReqCount := SizeOf(volumeParms);

          // A version 4 GetVolParmsInfoBuffer contains the BSD node name in the vMDeviceID field.
          // It is actually a char * value. This is mentioned in the header CoreServices/CarbonCore/Files.h.
          osResult := PBHGetVolParmsSync(@pb);
          if (osResult <> noErr) then
            begin
              WriteLn(stderr, 'PBHGetVolParmsSync returned %d\n', osResult);
            end
          else
            begin
              // The following code is just to convert the volume name from a HFSUniCharStr to
              // a plain C string so we can use it. It'd be preferable to
              // use CoreFoundation to work with the volume name in its Unicode form.

              volNameAsCFString := CFStringCreateWithCharacters(kCFAllocatorDefault,
                                                                volumeName.unicode,
                                                                volumeName.length);

              // If the conversion to a C string fails, then skip this volume.
              if (not CFStringGetCString(volNameAsCFString,
                                         volNameAsCString,
                                         SizeOf(volNameAsCString),
                                         kCFStringEncodingUTF8)) then
                begin
                  CFRelease(volNameAsCFString);
                  Inc(volumeIndex);
                  Continue;
                end;

              CFRelease(volNameAsCFString);

              //---------------------------------------------------------------
              New(Drive);
              with Drive^ do
              begin
                // The volume is local if vMServerAdr is 0. Network volumes won't have a BSD node name.
                if (volumeParms.vMServerAdr = 0) then
                  begin
                    Drive^.DriveType:= dtFixed;
                    WriteLn(Format('Volume "%s" (vRefNum %d), BSD node /dev/%s',
                        [volNameAsCString, actualVolume, PChar(volumeParms.vMDeviceID)]));
                  end
                else
                  begin
                    Drive^.DriveType:= dtNetwork;
                    WriteLn(Format('Volume "%s" (vRefNum %d)', [volNameAsCString, actualVolume]));
                  end;
                Name:= volNameAsCString;
                Path:= '/Volumes/' + volNameAsCString;
                DriveLabel:= volNameAsCString;
              end;
              Result.Add(Drive);
              //---------------------------------------------------------------
              end;
          Inc(volumeIndex);
        end;
    end; // while
end;
{$ELSE}
  function CheckMountEntry(DriveList: TList; MountEntry: PMountEntry): Boolean;
  var
    J: Integer;
    MountPoint: String;
  begin
    Result:= False;
    with MountEntry^ do
    begin
      // check filesystem
      if (mnt_fsname = 'proc') then Exit;

      // check mount dir
      if (mnt_dir = '') or
         (mnt_dir = '/') or
         (mnt_dir = 'none') or
         (mnt_dir = '/proc') or
         (mnt_dir = '/dev/pts') then Exit;

      // check file system type
      if (mnt_type = 'ignore') or
         (mnt_type = 'tmpfs') or
         (mnt_type = 'proc') or
         (mnt_type = 'swap') or
         (mnt_type = 'sysfs') or
         (mnt_type = 'devpts') or
         (mnt_type = 'fusectl') or
         (mnt_type = 'securityfs') or
         (mnt_type = 'binfmt_misc') or
         (mnt_type = 'fuse.gvfs-fuse-daemon') or
         (mnt_type = 'fuse.truecrypt') or
         (mnt_type = 'nfsd') or
         (mnt_type = 'usbfs') or
         (mnt_type = 'rpc_pipefs') then Exit;

      MountPoint := ExcludeTrailingPathDelimiter(mnt_dir);

      // if already added
      for J:= 0 to DriveList.Count - 1 do
        if PDrive(DriveList.Items[J])^.Path = MountPoint then
          Exit;
    end;
    Result:= True;
  end;
var
  Drive : PDrive;
  fstab: PIOFile;
  pme: PMountEntry;
  MntEntFileList: array[1..2] of PChar = (_PATH_FSTAB, _PATH_MOUNTED);
  I: Integer;
begin
  Result := TList.Create;
  for I:= Low(MntEntFileList) to High(MntEntFileList) do
  try
  fstab:= setmntent(MntEntFileList[I],'r');
  if not Assigned(fstab) then exit;
  pme:= getmntent(fstab);
  while (pme <> nil) do
  begin
    if CheckMountEntry(Result, pme) then
       begin
         New(Drive);
         with Drive^ do
         begin
           Path := StrPas(pme.mnt_dir);
           Path := ExcludeTrailingPathDelimiter(Path);
           DriveLabel := Path;
           Name := ExtractFileName(Path);
           if Name = '' then
             Name := Path;

           {$IFDEF DEBUG}
           DebugLn('Adding drive "' + Name + '" with mount point "' + Path + '"');
           {$ENDIF}

           // TODO more correct detect icons by drive type on Linux
           if (Pos('ISO9660', UpperCase(pme.mnt_type)) > 0) or (Pos('CDROM', UpperCase(pme.mnt_dir)) > 0) or
              (Pos('CDRW', UpperCase(pme.mnt_dir)) > 0) or (Pos('DVD', UpperCase(pme.mnt_dir)) > 0) then DriveType := dtCDROM else
           if (Pos('FLOPPY', UpperCase(pme.mnt_dir)) > 0) then DriveType := dtFloppy else
           if (Pos('ZIP', UpperCase(pme.mnt_type)) > 0) or (Pos('USB', UpperCase(pme.mnt_dir)) > 0) or
              (Pos('CAMERA', UpperCase(pme.mnt_dir)) > 0) then DriveType := dtFlash else
           if (Pos('NFS', UpperCase(pme.mnt_type)) > 0) or (Pos('SMB', UpperCase(pme.mnt_type)) > 0) or
              (Pos('NETW', UpperCase(pme.mnt_dir)) > 0) then DriveType := dtNetwork else
            DriveType := dtFixed;
         end;
         Result.Add(Drive);
       end;
    pme:= getmntent(fstab);
  end;
  endmntent(fstab);
  except
    DebugLn('Error with ', MntEntFileList[I]);
  end;
end;
{$ENDIF}

procedure DestroyDrivesList(var ADrivesList: TList);
var
  i : Integer;
begin
  if Assigned(ADrivesList) then
  begin
    for i := 0 to ADrivesList.Count - 1 do
      if Assigned(ADrivesList.Items[i]) then
        Dispose(PDrive(ADrivesList.Items[i]));
    FreeAndNil(ADrivesList);
  end;
end;


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
      FileHandle:= fpOpen(PChar(sFileName), O_RDONLY);

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
      if PtrInt(MappedFile) = -1 then
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

function mbFileOpen(const FileName: UTF8String; Mode: Integer): THandle;
{$IFDEF MSWINDOWS}
const
  AccessMode: array[0..2] of DWORD  = (
                GENERIC_READ,
                GENERIC_WRITE,
                GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of DWORD = (
               0,
               0,
               FILE_SHARE_READ,
               FILE_SHARE_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE);
var
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(FileName);
  Result:= CreateFileW(PWChar(wFileName), AccessMode[Mode and 3],
                       ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL, 0);
end;
{$ELSE}
const
  AccessMode: array[0..2] of LongInt  = (
                O_RdOnly,
                O_WrOnly,
                O_RdWr);
begin
  Result:= fpOpen(FileName, AccessMode[Mode and 3]);
end;
{$ENDIF}

function mbFileCreate(const FileName: UTF8String): THandle;
{$IFDEF MSWINDOWS}
var
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(FileName);
  Result:= CreateFileW(PWChar(wFileName), GENERIC_READ or GENERIC_WRITE,
                       0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;
{$ELSE}
begin
  Result:= fpOpen(FileName, O_Creat or O_RdWr or O_Trunc);
end;
{$ENDIF}

function mbFileCreate(const FileName: UTF8String; Mode: Integer): THandle;
{$IFDEF MSWINDOWS}
begin
  Result:= mbFileCreate(FileName);
end;
{$ELSE}
begin
  Result:= fpOpen(FileName, O_Creat or O_RdWr or O_Trunc, Mode);
end;
{$ENDIF}

function mbFileAge(const FileName: UTF8String): uTypes.TFileTime;
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
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
  if fpStat(FileName, Info) >= 0 then
{$PUSH}{$R-}
    Result := Info.st_mtime;
{$POP}
end;
{$ENDIF}

function mbFileSetTime(const FileName: UTF8String;
                       ModificationTime: uTypes.TFileTime;
                       CreationTime    : uTypes.TFileTime = 0;
                       LastAccessTime  : uTypes.TFileTime = 0): Boolean;
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
  wFileName: WideString;
  PWinModificationTime: Windows.LPFILETIME = nil;
  PWinCreationTime: Windows.LPFILETIME = nil;
  PWinLastAccessTime: Windows.LPFILETIME = nil;
begin
  wFileName:= UTF8Decode(FileName);
  Handle := CreateFileW(PWChar(wFileName),
                        FILE_WRITE_ATTRIBUTES,
                        FILE_SHARE_READ or FILE_SHARE_WRITE,
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
  Result := (fputime(PChar(pointer(FileName)), @t) <> -1);
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
  if fpStat(FileName, Info) >= 0 then
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
  hFile: THandle;
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
  Result:= fpAccess(FileName, AccessMode[Mode and 3]) = 0;
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
  if fpLStat(FileName, @Info) >= 0 then
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
  Result:= fpchmod(PChar(FileName), Attr);
end;
{$ENDIF}

function mbFileGetAttrNoLinks(const FileName: UTF8String): TFileAttrs;
{$IFDEF UNIX}
var
  Info: BaseUnix.Stat;
begin
  if fpStat(FileName, Info) >= 0 then
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
  if fpStat(PChar(FileName), StatInfo) <> 0 then Exit(False);
  mode := SetModeReadOnly(StatInfo.st_mode, ReadOnly);
  Result:= fpchmod(PChar(FileName), mode) = 0;
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
  Result:= fpUnLink(FileName) >= 0;
end;
{$ENDIF}

// 30.04.2009 ---------------------------------------------------------------------
function mbDeleteToTrash(const FileName: UTF8String): Boolean;
{$IFDEF MSWINDOWS}
var
  wFileName: WideString;
  FileOp: TSHFileOpStructW;
begin
  wFileName:= UTF8Decode(FileName);
  wFileName:= wFileName + #0;
  FillChar(FileOp, SizeOf(FileOp), 0);
  FileOp.Wnd := TWin32Widgetset(Widgetset).AppHandle;
  FileOp.wFunc := FO_DELETE;
  FileOp.pFrom := PWideChar(wFileName);
  // удаляем без подтвержения
  FileOp.fFlags := FOF_ALLOWUNDO or FOF_NOERRORUI or FOF_SILENT or FOF_NOCONFIRMATION;
  Result := (SHFileOperationW(@FileOp) = 0) and (not FileOp.fAnyOperationsAborted);
end;
{$ELSE}
// 12.05.2009 - implementation via 'gvfs-trash' application
var f: textfile;
    s: string;
begin
  // Open pipe to gvfs-trash to read the output in case of error.
  // The errors are written to stderr hence "2>&1" is needed because popen only catches stdout.
  if popen(f, _PATH_GVFS_TRASH + #32 + QuoteStr(FileName) + ' 2>&1', 'r') = 0 then
  begin
    readln(f,s);
    Result := not StrBegins(s, 'Error trashing');
    pclose(f);
  end
  else
   Result := False;
end;
{$ENDIF}
// --------------------------------------------------------------------------------

// 14.05.2009 ---------------------------------------------------------------------
function mbCheckTrash(sPath: UTF8String): Boolean;
{$IFDEF UNIX}
begin
  // Checking gvfs-trash.
  Result := mbFileExists(_PATH_GVFS_TRASH);
end;
{$ELSE}
const
  wsRoot: WideString = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\BitBucket\';
var
  Key: HKEY;
  Value: DWORD;
  ValueSize: LongInt;
begin
  Result:= False;
  if not mbDirectoryExists(sPath) then Exit;
  ValueSize:= SizeOf(DWORD);
  if RegOpenKeyExW(HKEY_LOCAL_MACHINE, PWideChar(wsRoot), 0, KEY_READ, Key) = ERROR_SUCCESS then
    begin
      if RegQueryValueExW(Key, 'UseGlobalSettings', nil, nil, @Value, @ValueSize) <> ERROR_SUCCESS then
        Value:= 1; // use global settings by default
      if (Value = 1) then
        begin
          if RegQueryValueExW(Key, 'NukeOnDelete', nil, nil, @Value, @ValueSize) <> ERROR_SUCCESS then
            Value:= 0; // delete to trash by default
          Result:= (Value = 0);
          RegCloseKey(Key);
        end
      else
        begin
          RegCloseKey(Key);
          if RegOpenKeyExW(HKEY_LOCAL_MACHINE, PWideChar(wsRoot + sPath[1]), 0, KEY_READ, Key) = ERROR_SUCCESS then
            begin
              if RegQueryValueExW(Key, 'NukeOnDelete', nil, nil, @Value, @ValueSize) <> ERROR_SUCCESS then
                Value:= 0; // delete to trash by default
              Result:= (Value = 0);
              RegCloseKey(Key);
            end;
        end;
    end;
end;
{$ENDIF}

// --------------------------------------------------------------------------------

function mbRenameFile(const OldName, NewName: UTF8String): Boolean;
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
begin
  Result:= BaseUnix.FpRename(OldNAme, NewName) >= 0;
end;
{$ENDIF}

function mbFileSize(const FileName: UTF8String): Int64;
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
  FindData: TWin32FindDataW;
  wFileName: WideString;
begin
  Result:= 0;
  wFileName:= UTF8Decode(FileName);
  Handle := FindFirstFileW(PWChar(wFileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
        Result:= (Int64(FindData.nFileSizeHigh) * MAXDWORD)+FindData.nFileSizeLow;
    end;
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  Result:= 0;
  if fpStat(FileName, Info) >= 0 then
    Result:= Info.st_size;
end;
{$ENDIF}

function FileFlush(Handle: THandle): Boolean;  
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
  Result:= fpChDir(PChar(NewDir)) = 0;
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
  if fpStat(Directory, Info) >= 0 then
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
  Result:= fpMkDir(PChar(NewDir), $FFF) = 0;
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
  Result:= fpRmDir(PChar(Dir)) = 0;
end;
{$ENDIF}

function mbCompareText(const s1, s2: UTF8String): PtrInt; inline;
{$IFDEF MSWINDOWS}
begin
  Result:= WideCompareText(UTF8Decode(s1), UTF8Decode(s2));
end;
{$ELSE}
begin
  Result:= StrComp(PChar(UTF8LowerCase(s1)), PChar(UTF8LowerCase(s2)));
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
  Result:= GetEnvironmentString(Index);
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
  Result:= (setenv(PChar(sName), PChar(sValue), 1) = 0);
end;
{$ENDIF}

function mbLoadLibrary(Name: UTF8String): TLibHandle;
{$IFDEF MSWINDOWS}
var
  wsName: WideString;
begin
  wsName:= UTF8Decode(Name);
  Result:= LoadLibraryW(PWideChar(wsName));
end;
{$ELSE}
begin
  Result:= TLibHandle(dlopen(PChar(Name), RTLD_LAZY));
end;
{$ENDIF}

function mbSysErrorMessage(ErrorCode: Integer): UTF8String;
begin
  Result :=
{$IFDEF WINDOWS}
            UTF8Encode(SysErrorMessage(ErrorCode));
{$ELSE}
            SysErrorMessage(ErrorCode);
{$ENDIF}
end;

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}
// ************************** HAL section ***********************  //

procedure CheckBlockDev(ctx: PLibHalContext; const udi: PChar; error: PDBusError; const CreateArray: boolean = False);
var cap : PChar;
var s : string;
begin
{  possible value what we can get (you can see that volume.label it will be mount point)
               for more key see "hal-device | less"
  volume.label = 'ATTID'  (string)
  volume.mount_point = '/media/ATTID'  (string)
  volume.is_mounted = true  (bool)
  volume.is_mounted_read_only = false  (bool)
  volume.linux.is_device_mapper = false  (bool)
  volume.is_disc = false  (bool)
  block.device = '/dev/sdc1'  (string)
  volume.is_partition = true  (bool)
  block.major = 8  (0x8)  (int)
  volume.partition.number = 1  (0x1)  (int)
  block.minor = 33  (0x21)  (int)
  volume.block_size = 512  (0x200)  (int)
  block.is_volume = true  (bool)
}
  // exit if bad udi
  if udi = nil then exit;

  // exit if not block dev
  if(libhal_device_property_exists(ctx, udi, PChar('block.device'), nil) = 0) then
    exit;

  // it is our dev
  if not CreateArray then
  begin
    DeviceWasChange := True;
  end;
//  DebugLn(udi);
  // add to array
  DeviceList.Add(udi);

  // get dev name
  cap := libhal_device_get_property_string(ctx, udi, Pchar('block.device'), nil);
  s := 'Dev '+cap;
  libhal_free_string(cap);

  // get mount point
  if(libhal_device_property_exists(ctx, udi, PChar('volume.mount_point'), nil) <> 0) then
  begin
    cap := libhal_device_get_property_string(ctx, udi, Pchar('volume.mount_point'), nil);
    s := s+' with mount point  '+cap;
    libhal_free_string(cap);
  end;

//  if not CreateArray then
//    DebugLn('udi = ',udi,s);

end;

procedure CreateBlokDevArr;
var  halDevices : PPChar;
     numDevices,i : Integer;
     udi : PChar;
begin
  // nil DeviceList;
  DeviceList.Clear;

  // Get List of all dev
  halDevices := libhal_get_all_devices(DcCtx, @numDevices, nil);
  if halDevices = nil then
  begin
    DebugLn('Cannot get device list');
  end;
//  DebugLn('Number of devices: ' + IntToStr(numDevices));

  // show info for all dev
  for i := 0 to numDevices do
  begin
    udi := (halDevices + i)^;
    CheckBlockDev(DcCtx, udi, nil, True);
  end;

  // free mem
  libhal_free_string_array(halDevices);
end;

procedure CreateHal;
begin
  // start program we must set global variable

  // create new HAL
  DcCtx := libhal_ctx_new;

  if DcCtx = nil then
  begin
    DebugLn('Cannot initialize HAL context');
    exit;
  end;

  // new error object
  // dbus_error_init(@MyError);
  // can do it if we will want new more aboute error

  // new connect with D-Bus
  DcDbus := dbus_bus_get_private(DBUS_BUS_SYSTEM, nil);

  // say to HAL use our D-Bus
  libhal_ctx_set_dbus_connection(DcCtx, DcDbus);

  // init HAL
  if (libhal_ctx_init(DcCtx, nil) = 0) then
  begin
    DebugLn('Cannot initialize HAL');
    exit;
    //warn ("libhal_ctx_init failed: %s", dbus_error.message);
    //   dbus_error_free (&dbus_error);
    //   libhal_ctx_free (ctx);
    //   return 1;
  end;



  if libhal_ctx_set_device_added(DcCtx, @LibHalDeviceAdded) = 0 then
    DebugLn('Cannot register LibHalDeviceAdded');

  if libhal_ctx_set_device_removed(DcCtx, @LibHalDeviceRemoved) = 0 then
    DebugLn('Cannot register LibHalDeviceRemoved');

  DeviceList := TStringList.Create;
  CreateBlokDevArr;
end;

procedure LibHalDeviceAdded(ctx: PLibHalContext; const udi: PChar); cdecl;
begin
//  DebugLn('add dev  ',udi);
//  sleep(1500); // if we dont do it we don`t see new dev
  CheckBlockDev(ctx,udi,nil); // it return value 2 time on one flash like /dev/sda /dev/sda1
end;

procedure LibHalDeviceRemoved(ctx: PLibHalContext; const udi: PChar); cdecl;
begin
  // here we will when somebody remove flash or CD or may be other
  // when it will CD it will one time
  // when it will be flash it will 7 time or more

  // here we can know only UDI
  // UDI here don`t have mount point and dev name

//  DebugLn('remove dev  ', udi);
//  ShowDevInfo(ctx,udi,@MyError); // don`t work
  if DeviceList.IndexOf(udi) > -1 then
  begin
    DeviceWasChange := True;
    DeviceList.Delete(DeviceList.IndexOf(udi));
    //CreateBlokDevArr; or we can re create DeviceList
  end;
end;

function CheckHalMsg: Boolean;
begin
  Result := False;
  // for one disk i must dispatch ~9 msg try do it 3 times
  dbus_connection_read_write_dispatch(DcDbus, 1);
  dbus_connection_read_write_dispatch(DcDbus, 1);
  dbus_connection_read_write_dispatch(DcDbus, 1);
  if DeviceWasChange then
  begin
    Result := True;
    DeviceWasChange := False;
  end;
end;

procedure FreeHal;
begin
  libhal_ctx_free(DcCtx);
//  dbus_connection_close(DcDbus);
//  it not right becose wee don do dbus_connection_open
//  how i must close dbus_bus_get_private i dont know

  FreeAndNil(DeviceList);
end;

// ************************** HAL section End ***********************  //

{$ENDIF}

{$IF DEFINED(UNIX) AND NOT DEFINED(DARWIN)}

initialization
  CreateHal;

finalization
  FreeHal;

{$ENDIF}

end.