{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains platform depended functions.

    Copyright (C) 2006-2019 Alexander Koblov (alexx2000@mail.ru)

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

unit uOSUtils;
 
{$mode delphi}

{$IFDEF DARWIN}
{$modeswitch objectivec1}
{$ENDIF}

interface

uses
    SysUtils, Classes, LCLType, uDrive, DCBasicTypes, uFindEx
    {$IF DEFINED(UNIX)}
    , DCFileAttributes
      {$IFDEF DARWIN}
      , MacOSAll
      {$ENDIF}
    {$ENDIF}
    ;
    
const
  CnstUserCommand = '{command}';

  {$IF DEFINED(MSWINDOWS)}
  faFolder = faDirectory;
  ReversePathDelim = '/';
  RunTermCmd = 'cmd.exe';  // default terminal
  RunTermParams = '';
  RunInTermStayOpenCmd = 'cmd.exe'; // default run in terminal command AND Stay open after command
  RunInTermStayOpenParams = '/K {command}';
  RunInTermCloseCmd = 'cmd.exe'; // default run in terminal command AND Close after command
  RunInTermCloseParams = '/C {command}';
  fmtCommandPath = '%s>';
  MonoSpaceFont = 'Courier New';
  {$ELSEIF DEFINED(UNIX)}
  faFolder = S_IFDIR;
  ReversePathDelim = '\';
  {$IFDEF DARWIN)}
  RunTermCmd = '/Applications/Utilities/Terminal.app %D';  // default terminal
  RunTermParams = '';
  RunInTerm = ''; // default run in terminal command
  RunInTermStayOpenCmd = '%COMMANDER_PATH%/scripts/terminal.sh'; // default run in terminal command AND Stay open after command
  RunInTermStayOpenParams = '''{command}''';
  RunInTermCloseCmd = ''; // default run in terminal command AND Close after command
  RunInTermCloseParams = '';
  MonoSpaceFont = 'Monaco';
  {$ELSE}
  RunTermCmd = 'xterm';  // default terminal
  RunTermParams = '';
  RunInTermStayOpenCmd = 'xterm'; // default run in terminal command AND Stay open after command
  RunInTermStayOpenParams = '-e sh -c ''{command}; echo -n Press ENTER to exit... ; read a''';
  RunInTermCloseCmd = 'xterm'; // default run in terminal command AND Close after command
  RunInTermCloseParams = '-e sh -c {command}';
  MonoSpaceFont = 'Monospace';
  {$ENDIF}
  fmtCommandPath = '[%s]$:';
  {$ENDIF}

  termStayOpen=True;
  termClose=False;
type
  tTerminalEndindMode = boolean;

  EInvalidCommandLine = class(Exception);
  EInvalidQuoting = class(EInvalidCommandLine)
    constructor Create; reintroduce;
  end;

{$IF DEFINED(MSWINDOWS) and DEFINED(FPC_HAS_CPSTRING)}
  NativeString = UnicodeString;
{$ELSE}
  NativeString = String;
{$ENDIF}

function NtfsHourTimeDelay(const SourceName, TargetName: String): Boolean;
function FileIsLinkToFolder(const FileName: String; out LinkTarget: String): Boolean;
function FileIsLinkToDirectory(const FileName: String; Attr: TFileAttrs): Boolean;
{en
   Execute command line
}
function ExecCmdFork(sCmd: String): Boolean;
{en
   Execute external commands
   @param(sCmd The executable)
   @param(sParams The optional parameters)
   @param(sStartPath The initial working directory)
   @param(bShowCommandLinePriorToExecute Flag indicating if we want the user to be prompted at the very last
                                         seconds prior to launch execution by offering a dialog window where
                                         he can adjust/confirm the three above parameters.)
   @param(bTerm Flag indicating if it should be launch through terminal)
   @param(bKeepTerminalOpen Value indicating the type of terminal to use (closed at the end, remain opened, etc.))
}
function ExecCmdFork(sCmd: String; sParams: String; sStartPath: String = ''; bShowCommandLinePriorToExecute: Boolean = False;
                     bTerm: Boolean = False; bKeepTerminalOpen: tTerminalEndindMode = termStayOpen): Boolean;
{en
   Opens a file or URL in the user's preferred application
   @param(URL File name or URL)
   @returns(The function returns @true if successful, @false otherwise)
}
function ShellExecute(URL: String): Boolean;
function GetDiskFreeSpace(const Path : String; out FreeSize, TotalSize : Int64) : Boolean;
{en
   Get maximum file size for a mounted file system
   @param(Path The pathname of any file within the mounted file  system)
   @returns(The maximum file size for a mounted file system)
}
function GetDiskMaxFileSize(const Path : String) : Int64;
{en
   Reads the concrete file's name that the link points to.
   If the link points to a link then it's resolved recursively
   until a valid file name that is not a link is found.
   @param(PathToLink Name of symbolic link (absolute path))
   @returns(The absolute filename the symbolic link name is pointing to,
            or an empty string when the link is invalid or
            the file it points to does not exist.)
}
function mbReadAllLinks(const PathToLink : String) : String;
{en
   If PathToLink points to a link then it returns file that the link points to (recursively).
   If PathToLink does not point to a link then PathToLink value is returned.
}
function mbCheckReadLinks(const PathToLink : String) : String;
function mbFileGetAttr(const FileName: String; out Attr: TSearchRecEx): Boolean; overload;
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
function GetAppCacheDir: String;
function GetAppDataDir: String;
function GetTempFolder: String;

{ Similar to "GetTempFolder" but that we can unilaterally delete at the end when closin application}
function GetTempFolderDeletableAtTheEnd: String;
procedure DeleteTempFolderDeletableAtTheEnd;

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
procedure FormatTerminal(var sCmd: String; var sParams: String; bKeepTerminalOpen: tTerminalEndindMode);
{en
   Same as mbFileGetAttr, but dereferences any encountered links.
}
function mbFileGetAttrNoLinks(const FileName: String): TFileAttrs;
{en
   Convert file name to system encoding, if name can not be represented in
   current locale then use short file name under Windows.
}
function mbFileNameToSysEnc(const LongPath: String): String;
{en
   Converts file name to native representation
}
function mbFileNameToNative(const FileName: String): NativeString; inline;
function mbGetEnvironmentVariable(const sName: String): String;
function mbSetEnvironmentVariable(const sName, sValue: String): Boolean;
{en
   Extract the root directory part of a file name.
   @returns(Drive letter under Windows and mount point under Unix)
}
function ExtractRootDir(const FileName: String): String;

procedure FixFormIcon(Handle: LCLType.HWND);
procedure HideConsoleWindow;
procedure FixDateNamesToUTF8;

function ParamStrU(Param: Integer): String; overload;
function ParamStrU(const Param: String): String; overload;

{en
  Get the current username of the current session
}
function GetCurrentUserName : String;

{en
  Get the current machine name
}
function GetComputerNetName: String;

implementation

uses
  StrUtils, uFileProcs, FileUtil, uDCUtils, DCOSUtils, DCStrUtils, uGlobs, uLng,
  fConfirmCommandLine, uLog, DCConvertEncoding, LazUTF8
  {$IF DEFINED(MSWINDOWS)}
  , JwaWinCon, Windows, uMyWindows, JwaWinNetWk,
    uShlObjAdditional, ShlObj, DCWindows, uNetworkThread
  {$ENDIF}
  {$IF DEFINED(UNIX)}
  , BaseUnix, Unix, uMyUnix, dl
    {$IF DEFINED(DARWIN)}
  , CocoaAll, uMyDarwin
    {$ELSE}
  , uGio, uClipboard, uXdg, uKde
    {$ENDIF}
  {$ENDIF}
  ;

function FileIsLinkToFolder(const FileName: String; out
  LinkTarget: String): Boolean;
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

function ExecCmdFork(sCmd, sParams, sStartPath:String; bShowCommandLinePriorToExecute, bTerm : Boolean; bKeepTerminalOpen: tTerminalEndindMode) : Boolean;
{$IFDEF UNIX}
var
  Args : TDynamicStringArray;
  bFlagKeepGoing: boolean = True;
begin
  result:=False;

  if bTerm then
    FormatTerminal(sCmd, sParams, bKeepTerminalOpen);

  if bShowCommandLinePriorToExecute then
    bFlagKeepGoing:= ConfirmCommandLine(sCmd, sParams, sStartPath);

  if bFlagKeepGoing then
  begin
    if (log_commandlineexecution in gLogOptions) then logWrite(rsMsgLogExtCmdLaunch+': '+rsSimpleWordFilename+'='+sCmd+' / '+rsSimpleWordParameter+'='+sParams+' / '+rsSimpleWordWorkDir+'='+sStartPath);

    if sCmd = EmptyStr then Exit(False);

    sCmd := UTF8ToSys(sCmd);
    SplitCommandArgs(UTF8ToSys(sParams), Args);

    Result := ExecuteCommand(sCmd, Args, sStartPath);

    if (log_commandlineexecution in gLogOptions) then
    begin
      if Result then
        logWrite(rsMsgLogExtCmdResult + ': ' + rsSimpleWordResult + '=' + rsSimpleWordSuccessExcla + ' / ' + rsSimpleWordFilename + '=' + sCmd + ' / ' + rsSimpleWordParameter + '=' + sParams + ' / ' + rsSimpleWordWorkDir + '=' + sStartPath)
      else
        logWrite(rsMsgLogExtCmdResult + ': ' + rsSimpleWordResult + '=' + rsSimpleWordFailedExcla + ' / ' + rsSimpleWordFilename + '=' + sCmd + ' / ' + rsSimpleWordParameter + '=' + sParams + ' / ' + rsSimpleWordWorkDir + '=' + sStartPath);
    end;
  end
  else
  begin
    Result := True;
  end;
end;
{$ELSE}
var
  wFileName, wParams, wStartPath: WideString;
  bFlagKeepGoing: boolean = True;
  ExecutionResult:HINST;
begin
  sStartPath:=RemoveQuotation(sStartPath);

  if sStartPath='' then
    sStartPath:=mbGetCurrentDir;

  sCmd:= NormalizePathDelimiters(sCmd);

  if bTerm then
    begin
      sCmd := ConcatenateStrWithSpace(sCmd,sParams);
      if bKeepTerminalOpen = termStayOpen then
        begin
          sParams:=StringReplace(gRunInTermStayOpenParams, '{command}', QuoteFilenameIfNecessary(sCmd) , [rfIgnoreCase]);
          sCmd := gRunInTermStayOpenCmd;
        end
        else
        begin
          sParams:=StringReplace(gRunInTermCloseParams, '{command}', QuoteFilenameIfNecessary(sCmd) , [rfIgnoreCase]);
          sCmd := gRunInTermCloseCmd;
        end;
    end;

  if bShowCommandLinePriorToExecute then
    bFlagKeepGoing:= ConfirmCommandLine(sCmd, sParams, sStartPath);

  if bFlagKeepGoing then
  begin
    wFileName:= UTF8Decode(sCmd);
    wParams:= UTF8Decode(sParams);
    wStartPath:= UTF8Decode(sStartPath);

    if (log_commandlineexecution in gLogOptions) then logWrite(rsMsgLogExtCmdLaunch+': '+rsSimpleWordFilename+'='+sCmd+' / '+rsSimpleWordParameter+'='+sParams+' / '+rsSimpleWordWorkDir+'='+sStartPath);

    ExecutionResult:=ShellExecuteW(0, nil, PWChar(wFileName), PWChar(wParams), PWChar(wStartPath), SW_SHOW);

    if (log_commandlineexecution in gLogOptions) then logWrite(rsMsgLogExtCmdResult + ': ' + rsSimpleWordResult + '=' + ifThen((ExecutionResult > 32), rsSimpleWordSuccessExcla, IntToStr(ExecutionResult) + ':' + SysErrorMessage(ExecutionResult)) + ' / ' + rsSimpleWordFilename + '=' + sCmd + ' / ' + rsSimpleWordParameter + '=' + sParams + ' / ' + rsSimpleWordWorkDir + '=' + sStartPath);

    Result := (ExecutionResult > 32);
  end
  else
  begin
    result:=True; //User abort, so let's fake all things completed.
  end;
end;
{$ENDIF}

function FileIsLinkToDirectory(const FileName: String; Attr: TFileAttrs): Boolean;
{$IFDEF UNIX}
var
  Info: BaseUnix.Stat;
begin
  Result:= FPS_ISLNK(Attr) and (fpStat(UTF8ToSys(FileName), Info) >= 0) and FPS_ISDIR(Info.st_mode);
end;
{$ELSE}
begin
  Result:= FPS_ISLNK(Attr) and FPS_ISDIR(Attr);
end;
{$ENDIF}

function ExecCmdFork(sCmd: String): Boolean;
{$IFDEF UNIX}
var
  Command: String;
  Args : TDynamicStringArray;
begin
  SplitCmdLine(sCmd, Command, Args);

  if (log_commandlineexecution in gLogOptions) then logWrite(rsMsgLogExtCmdLaunch + ': ' + rsSimpleWordCommand + '=' + sCmd);

  Result:= ExecuteCommand(Command, Args, EmptyStr);

  if (log_commandlineexecution in gLogOptions) then
  begin
    if Result then
      logWrite(rsMsgLogExtCmdResult + ': ' + rsSimpleWordResult + '=' + rsSimpleWordSuccessExcla + ' / ' + rsSimpleWordCommand + '=' + sCmd)
    else
      logWrite(rsMsgLogExtCmdResult + ': ' + rsSimpleWordResult + '=' + rsSimpleWordFailedExcla + ' / ' + rsSimpleWordCommand + '=' + sCmd);
  end;
end;
{$ELSE}
var
  sFileName,
  sParams: String;
  ExecutionResult: HINST;
  wsStartPath: UnicodeString;
begin
  SplitCmdLine(sCmd, sFileName, sParams);
  wsStartPath:= UTF8Decode(mbGetCurrentDir());
  sFileName:= NormalizePathDelimiters(sFileName);

  if (log_commandlineexecution in gLogOptions) then
    logWrite(rsMsgLogExtCmdLaunch + ': ' + rsSimpleWordFilename + '=' + sCmd + ' / ' + rsSimpleWordParameter + '=' + sParams);

  ExecutionResult := ShellExecuteW(0, nil, PWideChar(UTF8Decode(sFileName)), PWideChar(UTF8Decode(sParams)), PWideChar(wsStartPath), SW_SHOW);

  if (log_commandlineexecution in gLogOptions) then
  begin
    logWrite(rsMsgLogExtCmdResult + ': ' + rsSimpleWordResult + '=' + IfThen((ExecutionResult > 32), rsSimpleWordSuccessExcla, IntToStr(ExecutionResult) + ':' + SysErrorMessage(ExecutionResult)) + ' / ' + rsSimpleWordFilename + '=' + sCmd + ' / ' + rsSimpleWordParameter + '=' + sParams);
  end;

  Result := (ExecutionResult > 32);
end;
{$ENDIF}

function ShellExecute(URL: String): Boolean;
{$IF DEFINED(MSWINDOWS)}
var
  Return: HINST;
  wsFileName: UnicodeString;
  wsStartPath: UnicodeString;
begin
  URL:= NormalizePathDelimiters(URL);
  wsFileName:= UTF8Decode(QuoteDouble(URL));
  wsStartPath:= UTF8Decode(mbGetCurrentDir());
  Return:= ShellExecuteW(0, nil, PWideChar(wsFileName), nil, PWideChar(wsStartPath), SW_SHOWNORMAL);
  if Return = SE_ERR_NOASSOC then
    Result:= ExecCmdFork('rundll32 shell32.dll OpenAs_RunDLL ' + URL)
  else
    Result:= Return > 32;
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
  sCmdLine: String;
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
      if (DesktopEnv = DE_KDE) and (HasKdeOpen = True) then
        Result:= KioOpen(URL) // Under KDE use "kioclient" to open files
      else if HasGio and (DesktopEnv <> DE_XFCE) then
        Result:= GioOpen(URL) // Under GNOME, Unity and LXDE use "GIO" to open files
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
  Result:= (fpStatFS(PAnsiChar(CeUtf8ToSys(Path)), @sbfs) = 0);
  if not Result then Exit;
  FreeSize := (Int64(sbfs.bavail) * sbfs.bsize);
  TotalSize := (Int64(sbfs.blocks) * sbfs.bsize);
end;
{$ELSE}
var
  wPath: UnicodeString;
begin
  FreeSize := 0;
  TotalSize := 0;
  wPath:= UTF16LongName(Path);
  Result:= GetDiskFreeSpaceExW(PWideChar(wPath), FreeSize, TotalSize, nil);
end;
{$ENDIF}

function GetDiskMaxFileSize(const Path: String): Int64;
{$IFDEF UNIX}
const
  MSDOS_SUPER_MAGIC = $4d44;
var
  sbfs: TStatFS;
begin
  Result := High(Int64);
  if (fpStatFS(PAnsiChar(CeUtf8ToSys(Path)), @sbfs) = 0) then
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
 lpFileSystemNameBuffer: array [0..255] of WideChar;
 lpMaximumComponentLength: DWORD = 0;
 lpFileSystemFlags: DWORD = 0;
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

function NtfsHourTimeDelay(const SourceName, TargetName: String): Boolean;
{$IFDEF MSWINDOWS}
var
 lpDummy: DWORD = 0;
 lpSourceFileSystem,
 lpTargetFileSystem: array [0..MAX_PATH] of WideChar;
begin
 Result:= False;
 if GetVolumeInformationW(PWideChar(UTF8Decode(ExtractFileDrive(SourceName)) + PathDelim),
                          nil, 0, nil, lpDummy, lpDummy, lpSourceFileSystem, MAX_PATH) and
    GetVolumeInformationW(PWideChar(UTF8Decode(ExtractFileDrive(TargetName)) + PathDelim),
                          nil, 0, nil, lpDummy, lpDummy, lpTargetFileSystem, MAX_PATH) then
  begin
    Result:= (SameText(lpSourceFileSystem, 'FAT32') and SameText(lpTargetFileSystem, 'NTFS')) or
             (SameText(lpTargetFileSystem, 'FAT32') and SameText(lpSourceFileSystem, 'NTFS'))
  end;
end;
{$ELSE}
begin
  Result:= False;
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

function mbCheckReadLinks(const PathToLink : String): String;
var
  Attrs: TFileAttrs;
begin
  Attrs := mbFileGetAttr(PathToLink);
  if (Attrs <> faInvalidAttributes) and FPS_ISLNK(Attrs) then
    Result := mbReadAllLinks(PathToLink)
  else
    Result := PathToLink;
end;

function mbFileGetAttr(const FileName: String; out Attr: TSearchRecEx): Boolean;
{$IFDEF MSWINDOWS}
var
  FileInfo: Windows.TWin32FileAttributeData;
begin
  Result:= GetFileAttributesExW(PWideChar(UTF16LongName(FileName)),
                                GetFileExInfoStandard, @FileInfo);
  if Result then
  begin
    Attr.Time:= TWinFileTime(FileInfo.ftLastWriteTime);
    Int64Rec(Attr.Size).Lo:= FileInfo.nFileSizeLow;
    Int64Rec(Attr.Size).Hi:= FileInfo.nFileSizeHigh;
    Attr.Attr:= FileInfo.dwFileAttributes;
  end;
end;
{$ELSE}
var
  StatInfo: BaseUnix.Stat;
begin
  Result:= fpLStat(UTF8ToSys(FileName), StatInfo) >= 0;
  if Result then
  begin
    Attr.Time:= StatInfo.st_mtime;
    Attr.Size:= StatInfo.st_size;
    Attr.Attr:= StatInfo.st_mode;
  end;
end;
{$ENDIF}

function GetHomeDir : String;
{$IFDEF MSWINDOWS}
var
  iSize: Integer;
  wHomeDir: UnicodeString;
begin
  iSize:= GetEnvironmentVariableW('USERPROFILE', nil, 0);
  if iSize > 0 then
    begin
      SetLength(wHomeDir, iSize);
      GetEnvironmentVariableW('USERPROFILE', PWChar(wHomeDir), iSize);
    end;
  Delete(wHomeDir, iSize, 1);
  Result:= ExcludeBackPathDelimiter(UTF16ToUTF8(wHomeDir));
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
  wShell: UnicodeString;
begin
  iSize:= GetEnvironmentVariableW('ComSpec', nil, 0);
  if iSize > 0 then
    begin
      SetLength(wShell, iSize);
      GetEnvironmentVariableW('ComSpec', PWChar(wShell), iSize);
    end;
  Delete(wShell, iSize, 1);
  Result:= UTF16ToUTF8(wShell);
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

procedure FormatTerminal(var sCmd: String; var sParams: String; bKeepTerminalOpen: tTerminalEndindMode);
var
  sConfigParam:string;
begin
{$IF DEFINED(UNIX)}
  sParams := ConcatenateStrWithSpace(sCmd, sParams);

  if bKeepTerminalOpen = termStayOpen then
  begin
    sCmd := gRunInTermStayOpenCmd;
    sConfigParam := gRunInTermStayOpenParams;
  end
  else
  begin
    sCmd := gRunInTermCloseCmd;
    sConfigParam := gRunInTermCloseParams;
  end;

  sCmd := ReplaceEnvVars(sCmd);

  if Pos(CnstUserCommand, sConfigParam) <> 0 then
    sParams := StringReplace(sConfigParam, CnstUserCommand, sParams , [rfIgnoreCase])
  else
    sParams := ConcatenateStrWithSpace(sConfigParam, sParams);

{$ELSEIF DEFINED(MSWINDOWS)}
//  if bKeepTerminalOpen then
//    Result := Format('%s %s', [gRunInTermStayOpenCmd, QuoteDouble(Command)])
//  else
//    Result := Format('%s %s', [gRunInTermCloseCmd, QuoteDouble(Command)]);
  sParams := ConcatenateStrWithSpace(sCmd, sParams);

  if bKeepTerminalOpen = termStayOpen then
  begin
    sCmd := gRunInTermStayOpenCmd;
    sConfigParam := gRunInTermStayOpenParams;
  end
  else
  begin
    sCmd := gRunInTermCloseCmd;
    sConfigParam := gRunInTermCloseParams;
  end;

  if pos(CnstUserCommand,sConfigParam)<>0 then
    sParams := StringReplace(sConfigParam, CnstUserCommand, sParams , [rfIgnoreCase])
  else
    sParams:=ConcatenateStrWithSpace(sConfigParam, sParams);
{$ENDIF}
end;

function GetAppConfigDir: String;
{$IF DEFINED(MSWINDOWS)}
const
  SHGFP_TYPE_CURRENT = 0;
var
  wPath: array[0..MAX_PATH-1] of WideChar;
  wUser: UnicodeString;
  dwLength: DWORD;
begin
  if SUCCEEDED(SHGetFolderPathW(0, CSIDL_APPDATA or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT, @wPath[0])) or
     SUCCEEDED(SHGetFolderPathW(0, CSIDL_LOCAL_APPDATA or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT, @wPath[0])) then
  begin
    Result := UTF16ToUTF8(WideString(wPath));
  end
  else
  begin
    dwLength := UNLEN + 1;
    SetLength(wUser, dwLength);
    if GetUserNameW(PWideChar(wUser), @dwLength) then
    begin
      SetLength(wUser, dwLength - 1);
      Result := GetTempDir + UTF16ToUTF8(wUser);
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
    Result:= CeSysToUtf8(uinfo^.pw_dir) + '/.config/' + ApplicationName
  else
    Result:= ExcludeTrailingPathDelimiter(SysToUTF8(SysUtils.GetAppConfigDir(False)));
end;
{$ENDIF}

function GetAppCacheDir: String;
{$IF DEFINED(MSWINDOWS)}
var
  APath: array[0..MAX_PATH] of WideChar;
begin
  if SHGetSpecialFolderPathW(0, APath, CSIDL_LOCAL_APPDATA, True) then
    Result:= UTF16ToUTF8(UnicodeString(APath)) + DirectorySeparator + ApplicationName
  else
    Result:= GetAppConfigDir;
end;
{$ELSEIF DEFINED(DARWIN)}
begin
  Result:= NSGetFolderPath(NSCachesDirectory);
end;
{$ELSE}
var
  uinfo: PPasswordRecord;
begin
  uinfo:= getpwuid(fpGetUID);
  if (uinfo <> nil) and (uinfo^.pw_dir <> '') then
    Result:= CeSysToUtf8(uinfo^.pw_dir) + '/.cache/' + ApplicationName
  else
    Result:= GetHomeDir + '/.cache/' + ApplicationName;
end;
{$ENDIF}

function GetAppDataDir: String;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= GetAppCacheDir;
end;
{$ELSEIF DEFINED(DARWIN)}
begin
  Result:= NSGetFolderPath(NSApplicationSupportDirectory);
end;
{$ELSE}
begin
  Result:= IncludeTrailingPathDelimiter(GetUserDataDir) + ApplicationName;
end;
{$ENDIF}

function GetTempFolder: String;
begin
  Result:= GetTempDir + '_dc';
  if not mbDirectoryExists(Result) then
    mbCreateDir(Result);
  Result:= Result + PathDelim;
end;

function GetTempFolderDeletableAtTheEnd: String;
begin
  Result:= GetTempDir + '_dc~~~';
  if not mbDirectoryExists(Result) then
    mbCreateDir(Result);
  Result:= Result + PathDelim;
end;

procedure DeleteTempFolderDeletableAtTheEnd;
var
  TempFolderName:string;
begin
  TempFolderName:= GetTempDir + '_dc~~~';
  if mbDirectoryExists(TempFolderName) then
    DelTree(TempFolderName);
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
  wsLocalName, wsRemoteName: WideString;
begin
  Drv:= ExtractFileDrive(Drive^.Path) + PathDelim;

  // Try to close CD/DVD drive
  if (Drive^.DriveType = dtOptical) and
     TryMount and (not mbDriveReady(Drv)) then
    begin
       DriveLabel:= mbGetVolumeLabel(Drv, False);
       mbCloseCD(Drv);
       if mbDriveReady(Drv) then
         mbWaitLabelChange(Drv, DriveLabel);
    end
  // Try to connect to mapped network drive
  else if (Drive^.DriveType = dtNetwork) and
     TryMount and (not mbDriveReady(Drv)) then
    begin
      wsLocalName  := UTF8Decode(ExtractFileDrive(Drive^.Path));
      wsRemoteName := UTF8Decode(Drive^.DriveLabel);
      TNetworkThread.Connect(PWideChar(wsLocalName), PWideChar(wsRemoteName), RESOURCETYPE_DISK);
    end
  // Try to unlock BitLocker Drive
  else if TryMount then begin
    mbDriveUnlock(Drive^.Path);
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
    if CeSysToUtf8(pme.mnt_dir) = Drive^.Path then
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

function mbFileGetAttrNoLinks(const FileName: String): TFileAttrs;
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
  LinkTarget: String;
begin
  LinkTarget := mbReadAllLinks(FileName);
  if LinkTarget <> '' then
    Result := mbFileGetAttr(LinkTarget)
  else
    Result := faInvalidAttributes;
end;
{$ENDIF}

function mbFileNameToSysEnc(const LongPath: String): String;
{$IFDEF MSWINDOWS}
begin
  Result:= CeUtf8ToSys(LongPath);
  if Pos('?', Result) <> 0 then
    mbGetShortPathName(LongPath, Result);
end;
{$ELSE}
begin
  Result:= CeUtf8ToSys(LongPath);
end;
{$ENDIF}

function mbGetEnvironmentVariable(const sName: String): String;
{$IFDEF MSWINDOWS}
var
  wsName: UnicodeString;
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
        Result := UTF16ToUTF8(UnicodeString(largeBuf));
    finally
      FreeMem(largeBuf);
    end;
  end
  else if dwResult > 0 then
    Result := UTF16ToUTF8(UnicodeString(smallBuf));
end;
{$ELSE}
begin
  Result:= CeSysToUtf8(getenv(PAnsiChar(CeUtf8ToSys(sName))));
end;
{$ENDIF}

function mbSetEnvironmentVariable(const sName, sValue: String): Boolean;
{$IFDEF MSWINDOWS}
var
  wsName,
  wsValue: UnicodeString;
begin
  wsName:= UTF8Decode(sName);
  wsValue:= UTF8Decode(sValue);
  Result:= SetEnvironmentVariableW(PWideChar(wsName), PWideChar(wsValue));
end;
{$ELSE}
begin
  Result:= (setenv(PAnsiChar(CeUtf8ToSys(sName)), PAnsiChar(CeUtf8ToSys(sValue)), 1) = 0);
end;
{$ENDIF}

function ExtractRootDir(const FileName: String): String;
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
  with DefaultFormatSettings do
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
end;

function ParamStrU(Param: Integer): String;
{$IFDEF UNIX}
begin
  Result:= SysToUTF8(ObjPas.ParamStr(Param));
end;
{$ELSE}
begin
  if (Param >= 0) and (Param < argc) then
    Result:= StrPas(argv[Param])
  else
    Result:= EmptyStr;
end;
{$ENDIF}

function ParamStrU(const Param: String): String;
{$IFDEF UNIX}
begin
  Result:= SysToUTF8(Param);
end;
{$ELSE}
begin
  Result:= Param;
end;
{$ENDIF}

{ EInvalidQuoting }

constructor EInvalidQuoting.Create;
begin
  inherited Create(rsMsgInvalidQuoting);
end;

{ GetCurrentUserName }
function GetCurrentUserName : String;
{$IF DEFINED(MSWINDOWS)}
var
  wsUserName    : UnicodeString;
  dwUserNameLen : DWORD = UNLEN + 1;
begin
  SetLength(wsUserName, dwUserNameLen);
  if GetUserNameW(PWideChar(wsUserName), dwUserNameLen) then
  begin
    SetLength(wsUserName, dwUserNameLen - 1);
    Result := UTF16ToUTF8(wsUserName);
  end
  else
    Result := 'Unknown';
end;
{$ELSEIF DEFINED(UNIX)}
begin
  Result:= SysToUTF8(GetEnvironmentVariable('USER'));
end;
{$ENDIF}

{ GetComputerNetName }
function GetComputerNetName: String;
{$IF DEFINED(MSWINDOWS)}
var
  Size: DWORD = MAX_PATH;
  Buffer: array[0..Pred(MAX_PATH)] of WideChar;
begin
  if GetComputerNameW(Buffer, Size) then
    Result := UTF16ToUTF8(UnicodeString(Buffer))
  else
    Result := ''
end;
{$ELSEIF DEFINED(UNIX)}
begin
  Result:= SysToUTF8(GetHostName);
end;
{$ENDIF}

function mbFileNameToNative(const FileName: String): NativeString;
{$IF DEFINED(MSWINDOWS) and DEFINED(FPC_HAS_CPSTRING)}
begin
  Result:= UTF16LongName(FileName);
end;
{$ELSE}
begin
  Result:= Utf8ToSys(FileName);
end;
{$ENDIF}

end.
