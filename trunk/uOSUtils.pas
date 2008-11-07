{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains platform depended functions.

    Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)

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
 
interface

uses
    SysUtils, Classes, LCLProc, uDCUtils, uFindEx, uClassesEx
    {$IFDEF MSWINDOWS}
    , Windows, ShellApi, uNTFSLinks, uMyWindows, JwaWinNetWk
    {$ELSE}
    , BaseUnix, Unix, UnixType, UnixUtil, uMyUnix
    {$ENDIF};
    
{$mode delphi}{$H+}
    
const
  {$IFDEF MSWINDOWS}
  faFolder = faDirectory;
  faSymLink   = $00000400;
  RunTerm = 'cmd.exe';  // default terminal
  RunInTerm = 'cmd.exe /K'; // default run in terminal command
  fmtRunInTerm = '%s "%s"';
  ShieldChar = '/';
  {$ELSE}
  faFolder = S_IFDIR;
  faSymLink   = $00000040;
  RunTerm = 'xterm';  // default terminal
  RunInTerm = 'xterm -e sh -c'; // default run in terminal command
  fmtRunInTerm = '%s ''%s ; echo -n Press ENTER to exit... ; read''';
  ShieldChar = '\';
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

{$IFDEF MSWINDOWS}
  FILETIME = Windows.FILETIME;
{$ELSE}
  FILETIME = record
    dwLowDateTime : DWORD;
    dwHighDateTime : DWORD;
  end;
{$ENDIF}
  LPFILETIME = ^FILETIME;
  _FILETIME = FILETIME;
  TFILETIME = FILETIME;
  PFILETIME = ^FILETIME;

type
  TFileMapRec = record
    FileHandle : THandle;
    FileSize : Int64;
    MappingHandle : THandle;
    MappedFile : PChar;
  end;
{en
   Is file a directory
   @param(iAttr File attributes)
   @returns(@true if file is a directory, @false otherwise)
}
function FPS_ISDIR(iAttr:Cardinal) : Boolean;
{en
   Is file a symbolic link
   @param(iAttr File attributes)
   @returns(@true if file is a symbolic link, @false otherwise)
}
function FPS_ISLNK(iAttr:Cardinal) : Boolean;
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
function FileIsReadOnly(iAttr:Cardinal): Boolean;
function FileCopyAttr(const sSrc, sDst:String; bDropReadOnlyFlag : Boolean):Boolean;
function ExecCmdFork(sCmdLine:String; bTerm : Boolean = False; sTerm : String = ''):Boolean;
function GetDiskFreeSpace(Path : String; var FreeSize, TotalSize : Int64) : Boolean;
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
   @returns(The file the symbolic link name is pointing to)
}
function ReadSymLink(LinkName : String) : String;
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

(* File mapping/unmapping routines *)
{en
   Create memory map of a file
   @param(sFileName Name of file to mapping)
   @param(FileMapRec TFileMapRec structure)
   @returns(The function returns @true if successful, @false otherwise)
}
function MapFile(const sFileName : String; var FileMapRec : TFileMapRec) : Boolean;
{en
   Unmap previously mapped file
   @param(FileMapRec TFileMapRec structure)
}
procedure UnMapFile(FileMapRec : TFileMapRec);

(* Date/Time routines *)
{en
   Converts a file time based on the Coordinated Universal Time (UTC) to a local file time
   @param(lpFileTime TFileTime structure containing the UTC-based file time)
   @param(lpLocalFileTime TFileTime structure to receive the converted local file time)
   @returns(The function returns @true if successful, @false otherwise)
}
function FileTimeToLocalFileTimeEx(const lpFileTime: TFileTime; var lpLocalFileTime: TFileTime): LongBool;
{en
   Converts a local file time to a file time based on the Coordinated Universal Time (UTC)
   @param(lpLocalFileTime TFileTime structure that specifies the local file time)
   @param(lpFileTime TFileTime structure to receive the converted UTC-based file time)
   @returns(The function returns @true if successful, @false otherwise)
}
function LocalFileTimeToFileTimeEx(const lpLocalFileTime: TFileTime; var lpFileTime: TFileTime): LongBool;
{en
   Converts a file time based on the Coordinated Universal Time (UTC) to a TDateTime format
   @param(ft TFileTime structure containing the UTC-based file time)
   @returns(File time in TDateTime format)
}
function FileTimeToDateTime(ft : TFileTime) : TDateTime;
{en
   Converts a file time in TDateTime format to a file time based on the Coordinated Universal Time (UTC)
   @param(dt File time in TDateTime format)
   @returns(UTC-based file time)
}
function DateTimeToFileTime(dt : TDateTime) : TFileTime;

function GetShell : String;

{ File handling functions}
function mbFileOpen(const FileName: UTF8String; Mode: Integer): Integer;
function mbFileCreate(const FileName: UTF8String): Integer;overload;
function mbFileCreate(const FileName: UTF8String; Mode: Integer): Integer;overload;
function mbFileAge(const FileName: UTF8String): Longint;
function mbFileExists(const FileName: UTF8String): Boolean;
function mbFileAccess(const FileName: UTF8String; Mode: Integer): Boolean;
function mbFileGetAttr(const FileName: UTF8String): LongInt;
function mbFileSetAttr (const FileName: UTF8String; Attr: LongInt) : LongInt;
function mbFileSetReadOnly(const FileName: UTF8String; ReadOnly: Boolean): Boolean;
function mbDeleteFile(const FileName: UTF8String): Boolean;
function mbRenameFile(const OldName, NewName : UTF8String): Boolean;
function mbFileSize(const FileName: UTF8String): Int64;
function FileFlush(Handle: Integer): Boolean;
{ Directory handling functions}
function mbGetCurrentDir: UTF8String;
function mbSetCurrentDir(const NewDir: UTF8String): Boolean;
function mbDirectoryExists(const Directory : UTF8String) : Boolean;
function mbCreateDir(const NewDir: UTF8String): Boolean;
function mbRemoveDir(const Dir: UTF8String): Boolean;

implementation
   
(*Is Directory*)

function  FPS_ISDIR(iAttr:Cardinal) : Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := Boolean(iAttr and faDirectory);
end;
{$ELSE}
begin
  Result := BaseUnix.FPS_ISDIR(iAttr);
end;
{$ENDIF}

(*Is Link*)

function FPS_ISLNK(iAttr:Cardinal) : Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := Boolean(iAttr and faSymLink);
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
  if mbFileExists(sFileName) then
    begin
      fsExeLib := TFileStreamEx.Create(sFileName, fmOpenRead or fmShareDenyNone);
      {$IFDEF MSWINDOWS}
      wSign := fsExeLib.ReadWord;
      Result := (wSign = $5A4D);
      {$ELSE}
      dwSign := fsExeLib.ReadDWord;
      Result := (dwSign = $464C457F);
      {$ENDIF}
      fsExeLib.Free;
    end;
end;

function FileIsReadOnly(iAttr: Cardinal): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result:= Boolean(iAttr and faReadOnly);
end;
{$ELSE}
begin
  Result:= (((iAttr AND S_IRUSR) = S_IRUSR) and ((iAttr AND S_IWUSR) <> S_IWUSR));
end;
{$ENDIF}

function FileCopyAttr(const sSrc, sDst:String; bDropReadOnlyFlag : Boolean):Boolean;
{$IFDEF MSWINDOWS}
var
  iAttr : LongInt;
  ft : TFileTime;
  Handle: THandle;
begin
  iAttr := mbFileGetAttr(sSrc);
  //---------------------------------------------------------
  Handle:= mbFileOpen(sSrc, fmOpenRead or fmShareDenyNone);
  GetFileTime(Handle,nil,nil,@ft);
  FileClose(Handle);
  //---------------------------------------------------------
  if bDropReadOnlyFlag and Boolean(iAttr and faReadOnly) then
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
  mode : dword;
begin
  fpStat(PChar(sSrc), StatInfo);
//  DebugLN(AttrToStr(stat.st_mode));  // file time
  new(utb);
  utb^.actime:=StatInfo.st_atime;  //last access time // maybe now
  utb^.modtime:=StatInfo.st_mtime; // last modification time
  fputime(PChar(sDst),utb);
  dispose(utb);
// end file

// owner & group
  if fpChown(PChar(sDst),StatInfo.st_uid, StatInfo.st_gid)=-1 then
  begin
    // development messages
    DebugLN(Format('chown (%s) failed',[sSrc]));
  end;
  // mod
  mode := StatInfo.st_mode;
  if bDropReadOnlyFlag and ((mode AND S_IRUSR) = S_IRUSR) and ((mode AND S_IWUSR) <> S_IWUSR) then
    mode := (mode or S_IWUSR);
  if fpChmod(PChar(sDst), mode) = -1 then
  begin
    // development messages
    DebugLN(Format('chmod (%s) failed',[sSrc]));
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
  Args : TOpenStringArray;
  pArgs : PPCharArray;
begin
  sTempStr := Trim(sCmdLine);
  SplitArgs(Args, sTempStr);
  
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
      SplitArgs(Args, sTempStr);
    end;
  if Length(Args) = 0 then Exit;
    for x := 0 to Length(Args) - 1 do Args[x] := RemoveQuotation(Args[x]);

  if Length(Args) > 0 then
    begin
      pArgs := AllocMem((Length(Args) + 1) * SizeOf(PChar));
      for x := 0 to Length(Args) - 1 do
        pArgs[x] := PChar(Args[x]);
      pArgs[Length(Args)] := nil;
    end;

  pid := fpFork;

  if pid = 0 then
    begin
      { The child does the actual exec, and then exits }
      FpExecVP(Args[0], PPChar(@pArgs[0]));
      { If the FpExecVP fails, we return an exitvalue of 127, to let it be known }
      fpExit(127);
    end
  else
    if pid = -1 then         { Fork failed }
      begin
        raise Exception.Create('Fork failed: ' + sCmdLine);
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
  DebugLN('File: ' + sFileName + ' Params: ' + sParams + ' WorkDir: ' + wWorkDir);
  wFileName:= UTF8Decode(sFileName);
  wParams:= UTF8Decode(sParams);
  Result := (ShellExecuteW(0, 'open', PWChar(wFileName), PWChar(wParams), PWChar(wWorkDir), SW_SHOW) > 32);
end;
{$ENDIF}

(* Get Disk Free Space *)

function GetDiskFreeSpace(Path : String; var FreeSize, TotalSize : Int64) : Boolean;
{$IFDEF UNIX}
var
  sbfs: TStatFS;
begin
    fpStatFS(PChar(Path), @sbfs);
    FreeSize := (Int64(sbfs.bavail)*sbfs.bsize);
{$IFDEF CPU32} // TODO: Fix it
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
begin
  Result:= SysUtils.GetAppConfigDir(False);
end;
{$ENDIF}

function IsAvailable(Path: String): Boolean;
{$IFDEF MSWINDOWS}
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
var
  Drive : PDrive;

{$IFDEF MSWINDOWS}
  DriveNum: Integer;
  DriveBits: set of 0..25;
begin
  Result := TList.Create;
  { fill list }
  Integer(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
  begin
  New(Drive);
    with Drive^ do
    begin
     if not (DriveNum in DriveBits) then Continue;
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
     if (DriveType <> dtFloppy) and (DriveType <> dtNetwork) then
       DriveLabel := mbGetVolumeLabel(Name, True);
    end;
  Result.Add(Drive);
  end;

end;

{$ELSE}
  fstab: PIOFile;
  pme: PMountEntry;
begin
  Result := TList.Create;
  fstab:= setmntent(_PATH_FSTAB,'r');
  if not Assigned(fstab) then exit;
  pme:= getmntent(fstab);
  while (pme <> nil) do
  begin
    if (pme.mnt_dir <> '/') and (pme.mnt_dir <> 'none') and
       (pme.mnt_dir <> 'swap') and (pme.mnt_dir <> '/proc') and 
       (pme.mnt_dir <> '/dev/pts') then
       begin
         New(Drive);
         with Drive^ do
         begin
           Name := ExtractFileName(pme.mnt_dir);
           Path := pme.mnt_dir;
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
end;
{$ENDIF}

function MapFile(const sFileName : String; var FileMapRec : TFileMapRec) : Boolean;
{$IFDEF MSWINDOWS}
begin
  Result := False;
  with FileMapRec do
    begin
      FileHandle := mbFileOpen(sFileName, fmOpenRead);
      if FileHandle <= 0 then Exit;
      FileSize := GetFileSize(FileHandle, nil);

      MappingHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);

      if MappingHandle <> 0 then
        MappedFile := MapViewOfFile(MappingHandle, FILE_MAP_READ, 0, 0, 0)
      else
        begin
          MappedFile := nil;
          FileClose(FileHandle);
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
      FileHandle:= fpOpen(PChar(sFileName), O_RDONLY);

      if FileHandle = -1 then Exit;
      if fpfstat(FileHandle, StatInfo) <> 0 then
        begin
          fpClose(FileHandle);
          Exit;
        end;

      FileSize:= StatInfo.st_size;
      MappedFile:= fpmmap(nil,FileSize,PROT_READ, MAP_PRIVATE{SHARED},FileHandle,0 );
      if PtrInt(MappedFile) = -1 then
        begin
          MappedFile := nil;
          fpClose(FileHandle);
          Exit;
        end;
    end;
  Result := True;
end;
{$ENDIF}

procedure UnMapFile(FileMapRec : TFileMapRec);
{$IFDEF MSWINDOWS}
begin
  with FileMapRec do
    begin
      if Assigned(MappedFile) then
        UnmapViewOfFile(MappedFile);

      if MappingHandle <> 0 then
        CloseHandle(MappingHandle);

      if FileHandle >= 0 then
       FileClose(FileHandle);
    end;
end;
{$ELSE}
begin
  with FileMapRec do
    begin
      if FileHandle >= 0 then
        fpClose(FileHandle);

      if Assigned(MappedFile) then
        fpmunmap(MappedFile,FileSize);
    end;
end;
{$ENDIF}  

(* Date/Time routines *)

function FileTimeToLocalFileTimeEx(const lpFileTime: TFileTime; var lpLocalFileTime: TFileTime): LongBool;
{$IFDEF MSWINDOWS}
begin
  Result := FileTimeToLocalFileTime(lpFileTime, lpLocalFileTime);
end;
{$ELSE}
begin
  Int64(lpLocalFileTime) := Int64(lpFileTime) + 10000000 * Int64(TZSeconds);
end;
{$ENDIF}

function LocalFileTimeToFileTimeEx(const lpLocalFileTime: TFileTime; var lpFileTime: TFileTime): LongBool;
{$IFDEF MSWINDOWS}
begin
  Result := LocalFileTimeToFileTime(lpLocalFileTime, lpFileTime);
end;
{$ELSE}
begin
  Int64(lpFileTime) := Int64(lpLocalFileTime) - 10000000 * Int64(TZSeconds);
end;
{$ENDIF}

function FileTimeToDateTime(ft : TFileTime) : TDateTime;
begin
  FileTimeToLocalFileTimeEx(ft,ft);
  Result := (Int64(ft) / 864000000000.0) - 109205.0;
end;

function DateTimeToFileTime(dt : TDateTime) : TFileTime;
begin
  Int64(Result) := Round((dt + 109205.0) * 864000000000.0);
  LocalFileTimeToFileTimeEx(Result, Result);
end;

function mbFileOpen(const FileName: UTF8String; Mode: Integer): Integer;
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

function mbFileCreate(const FileName: UTF8String): Integer;
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

function mbFileCreate(const FileName: UTF8String; Mode: Integer): Integer;
{$IFDEF MSWINDOWS}
begin
  Result:= FileCreate(FileName);
end;
{$ELSE}
begin
  Result:= fpOpen(FileName, O_Creat or O_RdWr or O_Trunc, Mode);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function WinToDosTime (var Wtime: TFileTime; var DTime: LongInt): LongBool;
var
  lft : TFileTime;
begin
  Result:= FileTimeToLocalFileTime(WTime,lft) and
                FileTimeToDosDateTime(lft,Longrec(Dtime).Hi,LongRec(DTIME).lo);
end;
{$ELSE}
function UnixToWinAge(UnixAge: time_t): LongInt;
var
  Y,M,D,hh,mm,ss : word;
begin
  EpochToLocal(UnixAge,y,m,d,hh,mm,ss);
  Result:= DateTimeToFileDate(EncodeDate(y,m,d) + EncodeTime(hh,mm,ss,0));
end;
{$ENDIF}

function mbFileAge(const FileName: UTF8String): Longint;
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
        If WinToDosTime(FindData.ftLastWriteTime,Result) then
          Exit;
    end;
  Result:= -1;
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  Result:= -1;
  if fpStat(FileName, Info) >= 0 then
    Result:=UnixToWinAge(Info.st_mtime);
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
  if Attr <> $ffffffff then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) = 0;
end;
{$ELSE}
begin
  Result:= fpAccess(FileName, F_OK) = 0;
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
begin
  Result:= False;
  wFileName:= UTF8Decode(FileName);
  hFile:= CreateFileW(PWChar(wFileName), AccessMode[Mode and 3],
                       0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile <> 0 then
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

function mbFileGetAttr(const FileName: UTF8String): Longint;
{$IFDEF MSWINDOWS}
var
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(FileName);
  Result:= GetFileAttributesW(PWChar(wFileName));
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  Result:= -1;
  if fpLStat(FileName, @Info) >= 0 then
    Result:= Info.st_mode;
end;
{$ENDIF}

function mbFileSetAttr(const FileName: UTF8String; Attr: LongInt): LongInt;
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

function mbFileSetReadOnly(const FileName: UTF8String; ReadOnly: Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  iAttr: LongInt;
begin
  iAttr:= mbFileGetAttr(FileName);
  if iAttr = -1 then Exit(False);
  if ReadOnly then
    iAttr:= iAttr and faReadOnly
  else
    iAttr:= iAttr and not faReadOnly;
  Result:= mbFileSetAttr(FileName, iAttr) = 0;
end;
{$ELSE}
var
  StatInfo: BaseUnix.Stat;
  mode: dword;
begin
  if fpStat(PChar(FileName), StatInfo) <> 0 then Exit(False);
  mode:= StatInfo.st_mode;
  if ReadOnly then
    mode := mode and not (S_IWUSR or S_IWGRP or S_IWOTH)
  else
    mode:=  mode or (S_IWUSR or S_IWGRP or S_IWOTH);
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

function mbRenameFile(const OldName, NewName: UTF8String): Boolean;
{$IFDEF MSWINDOWS}
var
  wOldName,
  wNewName: WideString;
begin
  wOldName:= UTF8Decode(OldName);
  wNewName:= UTF8Decode(NewName);
  Result:= MoveFileW(PWChar(wOldName), PWChar(wNewName));
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
        Result:= (FindData.nFileSizeHigh * MAXDWORD)+FindData.nFileSizeLow;
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

function FileFlush(Handle: Integer): Boolean;  
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
  wDir: WideString;
begin
  Result:= '';
  iSize:= GetCurrentDirectoryW(0, nil);
  if iSize > 0 then
    begin
      SetLength(wDir, iSize);
      GetCurrentDirectoryW(iSize, PWChar(wDir));
      Result:= UTF8Encode(wDir);
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
  if Attr <> $ffffffff then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) > 0;
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  Result:= False;
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

end.
