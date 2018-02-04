{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains platform dependent functions dealing with operating system.

    Copyright (C) 2006-2017 Alexander Koblov (alexx2000@mail.ru)

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

unit DCOSUtils;
 
{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, DynLibs, DCClassesUtf8, DCBasicTypes;

const
  fmOpenSync    = $10000;
  fmOpenDirect  = $20000;
  fmOpenNoATime = $40000;

type
  TFileMapRec = record
    FileHandle : System.THandle;
    FileSize : Int64;
{$IFDEF MSWINDOWS}
    MappingHandle : System.THandle;
{$ENDIF}
    MappedFile : Pointer;
  end;

  TCopyAttributesOption = (caoCopyAttributes,
                           caoCopyTime,
                           caoCopyOwnership,
                           caoCopyPermissions,
                           caoRemoveReadOnlyAttr);
  TCopyAttributesOptions = set of TCopyAttributesOption;

const
  faInvalidAttributes: TFileAttrs = TFileAttrs(-1);
  CopyAttributesOptionCopyAll = [caoCopyAttributes, caoCopyTime, caoCopyOwnership];

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
function FileIsReadOnly(iAttr: TFileAttrs): Boolean; inline;

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

(* File mapping/unmapping routines *)
{en
   Create memory map of a file
   @param(sFileName Name of file to mapping)
   @param(FileMapRec TFileMapRec structure)
   @returns(The function returns @true if successful, @false otherwise)
}
function MapFile(const sFileName : String; out FileMapRec : TFileMapRec) : Boolean;
{en
   Unmap previously mapped file
   @param(FileMapRec TFileMapRec structure)
}
procedure UnMapFile(var FileMapRec : TFileMapRec);

{en
   Convert from console to UTF8 encoding.
}
function ConsoleToUTF8(const Str: AnsiString): String;

{ File handling functions}
function mbFileOpen(const FileName: String; Mode: LongWord): System.THandle;
function mbFileCreate(const FileName: String): System.THandle; overload; inline;
function mbFileCreate(const FileName: String; Mode: LongWord): System.THandle; overload; inline;
function mbFileCreate(const FileName: String; Mode, Rights: LongWord): System.THandle; overload;
function mbFileAge(const FileName: String): DCBasicTypes.TFileTime;
function mbFileSame(const FirstName, SecondName: String): Boolean;
// On success returns True.
function mbFileGetTime(const FileName: String;
                       var ModificationTime: DCBasicTypes.TFileTime;
                       var CreationTime    : DCBasicTypes.TFileTime;
                       var LastAccessTime  : DCBasicTypes.TFileTime): Boolean;
// On success returns True.
function mbFileSetTime(const FileName: String;
                       ModificationTime: DCBasicTypes.TFileTime;
                       CreationTime    : DCBasicTypes.TFileTime = 0;
                       LastAccessTime  : DCBasicTypes.TFileTime = 0): Boolean;
{en
   Checks if a given file exists - it can be a real file or a link to a file,
   but it can be opened and read from.
   Even if the result is @false, we can't be sure a file by that name can be created,
   because there may still exist a directory or link by that name.
}
function mbFileExists(const FileName: String): Boolean;
function mbFileAccess(const FileName: String; Mode: Word): Boolean;
function mbFileGetAttr(const FileName: String): TFileAttrs; overload;
function mbFileSetAttr(const FileName: String; Attr: TFileAttrs) : LongInt;
function mbFileGetAttr(const FileName: String; out Attr: TSearchRec): Boolean; overload;
{en
   If any operation in Options is performed and does not succeed it is included
   in the result set. If all performed operations succeed the function returns empty set.
   For example for Options=[caoCopyTime, caoCopyOwnership] setting ownership
   doesn't succeed then the function returns [caoCopyOwnership].
}
function mbFileCopyAttr(const sSrc, sDst: String; Options: TCopyAttributesOptions): TCopyAttributesOptions;
// Returns True on success.
function mbFileSetReadOnly(const FileName: String; ReadOnly: Boolean): Boolean;
function mbDeleteFile(const FileName: String): Boolean;

function mbRenameFile(const OldName: String; NewName: String): Boolean;
function mbFileSize(const FileName: String): Int64;
function FileFlush(Handle: System.THandle): Boolean;
{ Directory handling functions}
function mbGetCurrentDir: String;
function mbSetCurrentDir(const NewDir: String): Boolean;
{en
   Checks if a given directory exists - it may be a real directory or a link to directory.
   Even if the result is @false, we can't be sure a directory by that name can be created,
   because there may still exist a file or link by that name.
}
function mbDirectoryExists(const Directory : String) : Boolean;
function mbCreateDir(const NewDir: String): Boolean;
function mbRemoveDir(const Dir: String): Boolean;
{en
   Checks if any file system entry exists at given path.
   It can be file, directory, link, etc. (links are not followed).
}
function mbFileSystemEntryExists(const Path: String): Boolean;
function mbCompareFileNames(const FileName1, FileName2: String): Boolean;
function mbSameFile(const FileName1, FileName2: String): Boolean;
{ Other functions }
function mbGetEnvironmentString(Index : Integer) : String;
{en
   Expands environment-variable strings and replaces
   them with the values defined for the current user
}
function mbExpandEnvironmentStrings(const FileName: String): String;
function mbSysErrorMessage: String; overload; inline;
function mbSysErrorMessage(ErrorCode: Integer): String; overload;
{en
   Get current module name
}
function mbGetModuleName(Address: Pointer = nil): String;
function mbLoadLibrary(const Name: String): TLibHandle;
function SafeGetProcAddress(Lib: TLibHandle; const ProcName: AnsiString): Pointer;

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

implementation

uses
{$IF DEFINED(MSWINDOWS)}
  Windows, JwaWinNetWk, DCDateTimeUtils, DCWindows, DCNtfsLinks,
{$ENDIF}
{$IF DEFINED(UNIX)}
  BaseUnix, Unix, dl, DCUnix,
{$ENDIF}
  DCConvertEncoding, DCStrUtils, LazUTF8;

{$IFDEF UNIX}
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
  OpenFlags: array[0..3] of DWORD  = (
                0,
                FILE_FLAG_WRITE_THROUGH,
                FILE_FLAG_NO_BUFFERING,
                FILE_FLAG_WRITE_THROUGH or FILE_FLAG_NO_BUFFERING);

var
  CurrentDirectory: String;
{$ELSEIF DEFINED(UNIX)}
const

{$IF NOT DECLARED(O_SYNC)}
  O_SYNC   = 0;
{$ENDIF}

{$IF NOT DECLARED(O_DIRECT)}
  O_DIRECT = 0;
{$ENDIF}

  AccessModes: array[0..2] of cInt  = (
                O_RdOnly,
                O_WrOnly,
                O_RdWr);
  OpenFlags: array[0..3] of cInt  = (
                0,
                O_SYNC,
                O_DIRECT,
                O_SYNC or O_DIRECT);
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
  try
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
  except
    Result := False;
  end;
end;

function FileIsReadOnly(iAttr: TFileAttrs): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result:= (iAttr and (faReadOnly or faHidden or faSysFile)) <> 0;
end;
{$ELSE}
begin
  Result:= (((iAttr AND S_IRUSR) = S_IRUSR) and ((iAttr AND S_IWUSR) <> S_IWUSR));
end;
{$ENDIF}

function mbFileCopyAttr(const sSrc, sDst: String; Options: TCopyAttributesOptions): TCopyAttributesOptions;
{$IFDEF MSWINDOWS}
var
  Attr : TFileAttrs;
  ModificationTime, CreationTime, LastAccessTime: DCBasicTypes.TFileTime;
begin
  Result := [];

  if caoCopyAttributes in Options then
  begin
    Attr := mbFileGetAttr(sSrc);
    if Attr <> faInvalidAttributes then
    begin
      if (caoRemoveReadOnlyAttr in Options) and ((Attr and faReadOnly) <> 0) then
        Attr := (Attr and not faReadOnly);
      if mbFileSetAttr(sDst, Attr) <> 0 then
        Include(Result, caoCopyAttributes);
    end
    else
      Include(Result, caoCopyAttributes);
  end;

  if caoCopyTime in Options then
  begin
    if not (mbFileGetTime(sSrc, ModificationTime, CreationTime, LastAccessTime) and
            mbFileSetTime(sDst, ModificationTime, CreationTime, LastAccessTime)) then
      Include(Result, caoCopyTime);
  end;

  if caoCopyPermissions in Options then
  begin
    if not CopyNtfsPermissions(sSrc, sDst) then
    begin
      Include(Result, caoCopyPermissions);
    end;
  end;
end;
{$ELSE}  // *nix
var
  StatInfo : BaseUnix.Stat;
  utb : BaseUnix.TUTimBuf;
  mode : TMode;
begin
  if fpLStat(UTF8ToSys(sSrc), StatInfo) >= 0 then
  begin
    Result := [];
    if FPS_ISLNK(StatInfo.st_mode) then
    begin
      if caoCopyOwnership in Options then
      begin
        // Only group/owner can be set for links.
        if fpLChown(sDst, StatInfo.st_uid, StatInfo.st_gid) = -1 then
        begin
          Include(Result, caoCopyOwnership);
        end;
      end;
    end
    else
    begin
      if caoCopyTime in Options then
      begin
        utb.actime  := time_t(StatInfo.st_atime);  // last access time
        utb.modtime := time_t(StatInfo.st_mtime);  // last modification time
        if fputime(UTF8ToSys(sDst), @utb) <> 0 then
          Include(Result, caoCopyTime);
      end;

      if caoCopyOwnership in Options then
      begin
        if fpChown(PChar(UTF8ToSys(sDst)), StatInfo.st_uid, StatInfo.st_gid) = -1 then
        begin
          Include(Result, caoCopyOwnership);
        end;
      end;

      if caoCopyAttributes in Options then
      begin
        mode := StatInfo.st_mode;
        if caoRemoveReadOnlyAttr in Options then
          mode := SetModeReadOnly(mode, False);
        if fpChmod(UTF8ToSys(sDst), mode) = -1 then
        begin
          Include(Result, caoCopyAttributes);
        end;
      end;
    end;
  end
  else
    Result := Options;
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

function MapFile(const sFileName : String; out FileMapRec : TFileMapRec) : Boolean;
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
      FileHandle:= mbFileOpen(sFileName, fmOpenRead);

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

function ConsoleToUTF8(const Str: AnsiString): String;
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

function mbFileOpen(const FileName: String; Mode: LongWord): System.THandle;
{$IFDEF MSWINDOWS}
const
  ft: TFileTime = ( dwLowDateTime: $FFFFFFFF; dwHighDateTime: $FFFFFFFF; );
begin
  Result:= CreateFileW(PWideChar(UTF16LongName(FileName)),
                       AccessModes[Mode and 3] or ((Mode and fmOpenNoATime) shr 10),
                       ShareModes[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL, OpenFlags[(Mode shr 16) and 3]);
  if (Mode and fmOpenNoATime <> 0) then
  begin
    if (Result <> feInvalidHandle) then
      SetFileTime(Result, nil, @ft, @ft)
    else if GetLastError = ERROR_ACCESS_DENIED then
      Result := mbFileOpen(FileName, Mode and not fmOpenNoATime);
  end;
end;
{$ELSE}
begin
  repeat
    Result:= fpOpen(UTF8ToSys(FileName), AccessModes[Mode and 3] or
                    OpenFlags[(Mode shr 16) and 3] or O_CLOEXEC);
  until (Result <> -1) or (fpgeterrno <> ESysEINTR);
  if Result <> feInvalidHandle then
  begin
    FileCloseOnExec(Result);
    Result:= FileLock(Result, Mode and $FF);
  end;
end;
{$ENDIF}

function mbFileCreate(const FileName: String): System.THandle;
begin
  Result:= mbFileCreate(FileName, fmShareDenyWrite);
end;

function mbFileCreate(const FileName: String; Mode: LongWord): System.THandle;
begin
  Result:= mbFileCreate(FileName, Mode, 438); // 438 = 666 octal
end;

function mbFileCreate(const FileName: String; Mode, Rights: LongWord): System.THandle;
{$IFDEF MSWINDOWS}
begin
  Result:= CreateFileW(PWideChar(UTF16LongName(FileName)), GENERIC_READ or GENERIC_WRITE,
                       ShareModes[(Mode and $F0) shr 4], nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL,
                       OpenFlags[(Mode shr 16) and 3]);
end;
{$ELSE}
begin
  repeat
    Result:= fpOpen(UTF8ToSys(FileName), O_Creat or O_RdWr or O_Trunc or
                    OpenFlags[(Mode shr 16) and 3] or O_CLOEXEC, Rights);
  until (Result <> -1) or (fpgeterrno <> ESysEINTR);
  if Result <> feInvalidHandle then
  begin
    FileCloseOnExec(Result);
    Result:= FileLock(Result, Mode and $FF);
  end;
end;
{$ENDIF}

function mbFileAge(const FileName: String): DCBasicTypes.TFileTime;
{$IFDEF MSWINDOWS}
var
  Handle: System.THandle;
  FindData: TWin32FindDataW;
begin
  Handle := FindFirstFileW(PWideChar(UTF16LongName(FileName)), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
        Exit(DCBasicTypes.TWinFileTime(FindData.ftLastWriteTime));
    end;
  Result:= DCBasicTypes.TFileTime(-1);
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  Result:= DCBasicTypes.TFileTime(-1);
  if fpStat(UTF8ToSys(FileName), Info) >= 0 then
{$PUSH}{$R-}
    Result := Info.st_mtime;
{$POP}
end;
{$ENDIF}

function mbFileSame(const FirstName, SecondName: String): Boolean;
{$IFDEF MSWINDOWS}
var
  Handle: System.THandle;
  lpFirstFileInfo,
  lpSecondFileInfo: TByHandleFileInformation;
begin
  // Read first file info
  Handle:= CreateFileW(PWideChar(UTF16LongName(FirstName)), FILE_READ_ATTRIBUTES,
                       FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                       nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if Handle = INVALID_HANDLE_VALUE then Exit(False);
  Result:= GetFileInformationByHandle(Handle, lpFirstFileInfo);
  CloseHandle(Handle);
  if not Result then Exit;
  // Read second file info
  Handle:= CreateFileW(PWideChar(UTF16LongName(SecondName)), FILE_READ_ATTRIBUTES,
                       FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                       nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if Handle = INVALID_HANDLE_VALUE then Exit(False);
  Result:= GetFileInformationByHandle(Handle, lpSecondFileInfo);
  CloseHandle(Handle);
  if not Result then Exit;
  // Compare file info
  Result:= CompareByte(lpFirstFileInfo, lpSecondFileInfo,
                       SizeOf(TByHandleFileInformation)) = 0;
end;
{$ELSE}
var
  FirstStat,
  SecondStat: BaseUnix.Stat;
begin
  // Read first file info
  if fpStat(UTF8ToSys(FirstName), FirstStat) < 0 then Exit(False);
  // Read second file info
  if fpStat(UTF8ToSys(SecondName), SecondStat) < 0 then Exit(False);
  // Compare file info
  Result:= (FirstStat.st_dev = SecondStat.st_dev) and
           (FirstStat.st_ino = SecondStat.st_ino);
end;
{$ENDIF}

function mbFileGetTime(const FileName: String;
                       var ModificationTime: DCBasicTypes.TFileTime;
                       var CreationTime    : DCBasicTypes.TFileTime;
                       var LastAccessTime  : DCBasicTypes.TFileTime): Boolean;
{$IFDEF MSWINDOWS}
var
  Handle: System.THandle;
begin
  Handle := CreateFileW(PWideChar(UTF16LongName(FileName)),
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
  Result := fpLStat(UTF8ToSys(FileName), StatInfo) >= 0;
  if Result then
  begin
    LastAccessTime   := StatInfo.st_atime;
    ModificationTime := StatInfo.st_mtime;
    CreationTime     := StatInfo.st_ctime;
  end;
end;
{$ENDIF}

function mbFileSetTime(const FileName: String;
                       ModificationTime: DCBasicTypes.TFileTime;
                       CreationTime    : DCBasicTypes.TFileTime = 0;
                       LastAccessTime  : DCBasicTypes.TFileTime = 0): Boolean;
{$IFDEF MSWINDOWS}
var
  Handle: System.THandle;
  PWinModificationTime: Windows.LPFILETIME = nil;
  PWinCreationTime: Windows.LPFILETIME = nil;
  PWinLastAccessTime: Windows.LPFILETIME = nil;
begin
  Handle := CreateFileW(PWideChar(UTF16LongName(FileName)),
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
  CurrentModificationTime, CurrentCreationTime, CurrentLastAccessTime: DCBasicTypes.TFileTime;
begin
  if mbFileGetTime(FileName,CurrentModificationTime, CurrentCreationTime, CurrentLastAccessTime) then
  begin
    if LastAccessTime<>0 then t.actime := time_t(LastAccessTime) else t.actime := time_t(CurrentLastAccessTime);
    if ModificationTime<>0 then t.modtime := time_t(ModificationTime) else t.modtime := time_t(CurrentModificationTime);
    Result := (fputime(UTF8ToSys(FileName), @t) <> -1);
  end
  else
  begin
    Result:=False;
  end;
end;
{$ENDIF}

function mbFileExists(const FileName: String) : Boolean;
{$IFDEF MSWINDOWS}
var
  Attr: DWORD;
begin
  Attr:= GetFileAttributesW(PWideChar(UTF16LongName(FileName)));
  if Attr <> DWORD(-1) then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) = 0
  else
    Result:=False;
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  // Can use fpStat, because link to an existing filename can be opened as if it were a real file.
  if fpStat(UTF8ToSys(FileName), Info) >= 0 then
    Result:= fpS_ISREG(Info.st_mode)
  else
    Result:= False;
end;
{$ENDIF}

function mbFileAccess(const FileName: String; Mode: Word): Boolean;
{$IFDEF MSWINDOWS}
const
  AccessMode: array[0..2] of DWORD  = (
                GENERIC_READ,
                GENERIC_WRITE,
                GENERIC_READ or GENERIC_WRITE);
var
  hFile: System.THandle;
  dwDesiredAccess: DWORD;
  dwShareMode: DWORD = 0;
begin
  dwDesiredAccess := AccessMode[Mode and 3];
  if Mode = fmOpenRead then // If checking Read mode no sharing mode given
    Mode := Mode or fmShareDenyNone;
  dwShareMode := ShareModes[(Mode and $F0) shr 4];
  hFile:= CreateFileW(PWideChar(UTF16LongName(FileName)), dwDesiredAccess, dwShareMode,
                      nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  Result := hFile <> INVALID_HANDLE_VALUE;
  if Result then
    FileClose(hFile);
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

function mbFileGetAttr(const FileName: String): TFileAttrs;
{$IFDEF MSWINDOWS}
begin
  Result := GetFileAttributesW(PWideChar(UTF16LongName(FileName)));
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  if fpLStat(UTF8ToSys(FileName), @Info) >= 0 then
    Result:= Info.st_mode
  else
    Result:= faInvalidAttributes;
end;
{$ENDIF}

function mbFileSetAttr(const FileName: String; Attr: TFileAttrs): LongInt;
{$IFDEF MSWINDOWS}
begin
  if SetFileAttributesW(PWideChar(UTF16LongName(FileName)), Attr) then
    Result:= 0
  else
    Result:= GetLastError;
end;
{$ELSE}
begin
  Result:= fpchmod(UTF8ToSys(FileName), Attr);
end;
{$ENDIF}

function mbFileGetAttr(const FileName: String; out Attr: TSearchRec): Boolean;
{$IFDEF MSWINDOWS}
var
  FileInfo: Windows.TWin32FileAttributeData;
begin
  Result:= GetFileAttributesExW(PWideChar(UTF16LongName(FileName)),
                                GetFileExInfoStandard, @FileInfo);
  if Result then
  begin
    WinToDosTime(FileInfo.ftLastWriteTime, Attr.Time);
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

{$IFDEF uOSUtilsRangeCheckOn}
{$R+}
{$UNDEF uOSUtilsRangeCheckOn}
{$ENDIF}

function mbFileSetReadOnly(const FileName: String; ReadOnly: Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  iAttr: DWORD;
  wFileName: UnicodeString;
begin
  wFileName:= UTF16LongName(FileName);
  iAttr := GetFileAttributesW(PWideChar(wFileName));
  if iAttr = DWORD(-1) then Exit(False);
  if ReadOnly then
    iAttr:= iAttr or faReadOnly
  else
    iAttr:= iAttr and not (faReadOnly or faHidden or faSysFile);
  Result:= SetFileAttributesW(PWideChar(wFileName), iAttr) = True;
end;
{$ELSE}
var
  StatInfo: BaseUnix.Stat;
  mode: TMode;
begin
  if fpStat(UTF8ToSys(FileName), StatInfo) <> 0 then Exit(False);
  mode := SetModeReadOnly(StatInfo.st_mode, ReadOnly);
  Result:= fpchmod(UTF8ToSys(FileName), mode) = 0;
end;
{$ENDIF}

function mbDeleteFile(const FileName: String): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result:= Windows.DeleteFileW(PWideChar(UTF16LongName(FileName)));
  if not Result then Result:= (GetLastError = ERROR_FILE_NOT_FOUND);
end;
{$ELSE}
begin
  Result:= fpUnLink(UTF8ToSys(FileName)) = 0;
  if not Result then Result:= (fpgetErrNo = ESysENOENT);
end;
{$ENDIF}

function mbRenameFile(const OldName: String; NewName: String): Boolean;
{$IFDEF MSWINDOWS}
var
  wOldName,
  wNewName: UnicodeString;
begin
  wNewName:= UTF16LongName(NewName);
  wOldName:= UTF16LongName(OldName);
  Result:= MoveFileExW(PWChar(wOldName), PWChar(wNewName), MOVEFILE_REPLACE_EXISTING);
end;
{$ELSE}
var
  tmpFileName: String;
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

function mbFileSize(const FileName: String): Int64;
{$IFDEF MSWINDOWS}
var
  Handle: System.THandle;
  FindData: TWin32FindDataW;
begin
  Result:= 0;
  Handle := FindFirstFileW(PWideChar(UTF16LongName(FileName)), FindData);
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

function FileFlush(Handle: System.THandle): Boolean; inline;
{$IFDEF MSWINDOWS}
begin
  Result:= FlushFileBuffers(Handle);
end;
{$ELSE}  
begin
  Result:= (fpfsync(Handle) = 0);
end;  
{$ENDIF}
  
function mbGetCurrentDir: String;
{$IFDEF MSWINDOWS}
var
  dwSize: DWORD;
  wsDir: UnicodeString;
begin
  if Length(CurrentDirectory) > 0 then
    Result:= CurrentDirectory
  else
  begin
    dwSize:= GetCurrentDirectoryW(0, nil);
    if dwSize = 0 then
      Result:= EmptyStr
    else begin
      SetLength(wsDir, dwSize + 1);
      SetLength(wsDir, GetCurrentDirectoryW(dwSize, PWideChar(wsDir)));
      Result:= UTF16ToUTF8(wsDir);
    end;
  end;
end;
{$ELSE}
begin
  GetDir(0, Result);
  Result := SysToUTF8(Result);
end;
{$ENDIF}

function mbSetCurrentDir(const NewDir: String): Boolean;
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
  wsNewDir: UnicodeString;
  FindData: TWin32FindDataW;
  NetResource: TNetResourceW;
begin
  // Function WNetAddConnection2W works very slow
  // when the final character is a backslash ('\')
  wsNewDir:= UTF8Decode(ExcludeTrailingPathDelimiter(NewDir));
  if Pos('\\', wsNewDir) = 1 then
  begin
    FillChar(NetResource, SizeOf(NetResource), #0);
    NetResource.dwType:= RESOURCETYPE_ANY;
    NetResource.lpRemoteName:= PWideChar(wsNewDir);
    WNetAddConnection2W(NetResource, nil, nil, CONNECT_INTERACTIVE);
  end;
  wsNewDir:= UTF16LongName(IncludeTrailingBackslash(NewDir)) + '*';
  Handle:= FindFirstFileW(PWideChar(wsNewDir), FindData);
  Result:= (Handle <> INVALID_HANDLE_VALUE) or (GetLastError = ERROR_FILE_NOT_FOUND);
  if (Handle <> INVALID_HANDLE_VALUE) then FindClose(Handle);
  if Result then CurrentDirectory:= NewDir;
end;
{$ELSE}
begin
  Result:= fpChDir(UTF8ToSys(NewDir)) = 0;
end;
{$ENDIF}

function mbDirectoryExists(const Directory: String) : Boolean;
{$IFDEF MSWINDOWS}
var
  Attr: DWORD;
begin
  Attr:= GetFileAttributesW(PWideChar(UTF16LongName(Directory)));
  if Attr <> DWORD(-1) then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) > 0
  else
    Result:= False;
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  // We can use fpStat here instead of fpLstat, so that True is returned
  // when target is a directory or a link to an existing directory.
  // Note that same behaviour would be achieved by passing paths
  // that end with path delimiter to fpLstat.
  // Paths with links can be used the same way as if they were real directories.
  if fpStat(UTF8ToSys(Directory), Info) >= 0 then
    Result:= fpS_ISDIR(Info.st_mode)
  else
    Result:= False;
end;
{$ENDIF}

function mbCreateDir(const NewDir: String): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result:= CreateDirectoryW(PWideChar(UTF16LongName(NewDir)), nil);
end;
{$ELSE}
begin
  Result:= fpMkDir(UTF8ToSys(NewDir), $1FF) = 0; // $1FF = &0777
end;
{$ENDIF}

function mbRemoveDir(const Dir: String): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result:= RemoveDirectoryW(PWideChar(UTF16LongName(Dir)));
  if not Result then Result:= (GetLastError = ERROR_FILE_NOT_FOUND);
end;
{$ELSE}
begin
  Result:= fpRmDir(UTF8ToSys(Dir)) = 0;
  if not Result then Result:= (fpgetErrNo = ESysENOENT);
end;
{$ENDIF}

function mbFileSystemEntryExists(const Path: String): Boolean;
begin
  Result := mbFileGetAttr(Path) <> faInvalidAttributes;
end;

function mbCompareFileNames(const FileName1, FileName2: String): Boolean; inline;
{$IF DEFINED(WINDOWS) OR DEFINED(DARWIN)}
begin
  Result:= (WideCompareText(UTF8Decode(FileName1), UTF8Decode(FileName2)) = 0);
end;
{$ELSE}
begin
  Result:= (WideCompareStr(UTF8Decode(FileName1), UTF8Decode(FileName2)) = 0);
end;
{$ENDIF}

function mbSameFile(const FileName1, FileName2: String): Boolean;
{$IF DEFINED(MSWINDOWS)}
var
  FileHandle1, FileHandle2: System.THandle;
  FileInfo1, FileInfo2: BY_HANDLE_FILE_INFORMATION;
begin
  Result := mbCompareFileNames(FileName1, FileName2);

  if not Result then
  begin
    FileHandle1 := CreateFileW(PWideChar(UTF16LongName(FileName1)), FILE_READ_ATTRIBUTES,
        FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
        nil, OPEN_EXISTING, 0, 0);

    if FileHandle1 <> INVALID_HANDLE_VALUE then
    begin
      FileHandle2 := CreateFileW(PWideChar(UTF16LongName(FileName2)), FILE_READ_ATTRIBUTES,
          FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
          nil, OPEN_EXISTING, 0, 0);

      if FileHandle2 <> INVALID_HANDLE_VALUE then
      begin
        if GetFileInformationByHandle(FileHandle1, FileInfo1) and
           GetFileInformationByHandle(FileHandle2, FileInfo2) then
        begin
          // Check if both files have the same index on the same volume.
          // This check is valid only while both files are open.
          Result := (FileInfo1.dwVolumeSerialNumber = FileInfo2.dwVolumeSerialNumber) and
                    (FileInfo1.nFileIndexHigh       = FileInfo2.nFileIndexHigh) and
                    (FileInfo1.nFileIndexLow        = FileInfo2.nFileIndexLow);
        end;
        CloseHandle(FileHandle2);
      end;
      CloseHandle(FileHandle1);
    end
  end;
end;
{$ELSEIF DEFINED(UNIX)}
var
  File1Stat, File2Stat: stat;
begin
  Result := mbCompareFileNames(FileName1, FileName2) or
            (
              (fpLstat(UTF8ToSys(FileName1), File1Stat) = 0) and
              (fpLstat(UTF8ToSys(FileName2), File2Stat) = 0) and
              (File1Stat.st_ino = File2Stat.st_ino) and
              (File1Stat.st_dev = File2Stat.st_dev)
            );
end;
{$ENDIF}

function mbGetEnvironmentString(Index: Integer): String;
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
        Result:= UTF16ToUTF8(UnicodeString(hp));
    end;
  FreeEnvironmentStringsW(p);
end;
{$ELSE}
begin
  Result:= SysToUTF8(GetEnvironmentString(Index));
end;
{$ENDIF}

function mbExpandEnvironmentStrings(const FileName: String): String;
{$IF DEFINED(MSWINDOWS)}
var
  dwSize: DWORD;
  wsResult: UnicodeString;
begin
  SetLength(wsResult, MaxSmallInt + 1);
  dwSize:= ExpandEnvironmentStringsW(PWideChar(UTF8Decode(FileName)), PWideChar(wsResult), MaxSmallInt);
  if (dwSize = 0) or (dwSize > MaxSmallInt) then
    Result:= FileName
  else begin
    SetLength(wsResult, dwSize - 1);
    Result:= UTF16ToUTF8(wsResult);
  end;
end;
{$ELSE}
var
  Index: Integer = 1;
  EnvCnt, EqualPos: Integer;
  EnvVar, EnvName, EnvValue: String;
begin
  Result:= FileName;
  EnvCnt:= GetEnvironmentVariableCount;
  while (Index <= EnvCnt) and (Pos('$', Result) > 0) do
  begin
    EnvVar:= mbGetEnvironmentString(Index);
    EqualPos:= Pos('=', EnvVar);
    if EqualPos = 0 then Continue;
    EnvName:= Copy(EnvVar, 1, EqualPos - 1);
    EnvValue:= Copy(EnvVar, EqualPos + 1, MaxInt);
    Result:= StringReplace(Result, '$' + EnvName, EnvValue, [rfReplaceAll, rfIgnoreCase]);
    Inc(Index);
  end;
end;
{$ENDIF}

function mbSysErrorMessage: String;
begin
  Result := mbSysErrorMessage(GetLastOSError);
end;

function mbSysErrorMessage(ErrorCode: Integer): String;
begin
  Result := SysErrorMessage(ErrorCode);
{$IF (FPC_FULLVERSION < 30004)}
  Result := CeSysToUtf8(Result);
{$ENDIF}
end;

function mbGetModuleName(Address: Pointer): String;
const
  Dummy: Boolean = False;
{$IFDEF UNIX}
var
  dlinfo: dl_info;
begin
  if Address = nil then Address:= @Dummy;
  FillChar({%H-}dlinfo, SizeOf(dlinfo), #0);
  if dladdr(Address, @dlinfo) = 0 then
    Result:= EmptyStr
  else begin
    Result:= CeSysToUtf8(dlinfo.dli_fname);
  end;
end;
{$ELSE}
var
  ModuleName: UnicodeString;
  lpBuffer: TMemoryBasicInformation;
begin
  if Address = nil then Address:= @Dummy;
  if VirtualQuery(Address, @lpBuffer, SizeOf(lpBuffer)) <> SizeOf(lpBuffer) then
    Result:= EmptyStr
  else begin
    SetLength(ModuleName, MAX_PATH + 1);
    SetLength(ModuleName, GetModuleFileNameW({%H-}THandle(lpBuffer.AllocationBase),
                                             PWideChar(ModuleName), MAX_PATH));
    Result:= UTF16ToUTF8(ModuleName);
  end;
end;
{$ENDIF}

function mbLoadLibrary(const Name: String): TLibHandle;
{$IFDEF MSWINDOWS}
begin
  Result:= LoadLibraryW(PWideChar(UTF8Decode(Name)));
end;
{$ELSE}
begin
  Result:= TLibHandle(dlopen(PChar(UTF8ToSys(Name)), RTLD_LAZY));
end;
{$ENDIF}

function SafeGetProcAddress(Lib: TLibHandle; const ProcName: AnsiString): Pointer;
begin
  Result:= GetProcedureAddress(Lib, ProcName);
  if (Result = nil) then raise Exception.Create(ProcName);
end;

function CreateHardLink(const Path, LinkName: String) : Boolean;
{$IFDEF MSWINDOWS}
var
  wsPath, wsLinkName: UnicodeString;
begin
  wsPath:= UTF16LongName(Path);
  wsLinkName:= UTF16LongName(LinkName);
  Result:= DCNtfsLinks.CreateHardlink(wsPath, wsLinkName);
end;
{$ELSE}
begin
  Result := (fplink(PAnsiChar(CeUtf8ToSys(Path)),PAnsiChar(CeUtf8ToSys(LinkName)))=0);
end;
{$ENDIF}

function CreateSymLink(const Path, LinkName: string) : Boolean;
{$IFDEF MSWINDOWS}
var
  wsPath, wsLinkName: UnicodeString;
begin
  wsPath:= UTF8Decode(Path);
  wsLinkName:= UTF16LongName(LinkName);
  Result:= DCNtfsLinks.CreateSymlink(wsPath, wsLinkName);
end;
{$ELSE}
begin
  Result := (fpsymlink(PAnsiChar(CeUtf8ToSys(Path)), PAnsiChar(CeUtf8ToSys(LinkName)))=0);
end;
{$ENDIF}

function ReadSymLink(const LinkName : String) : String;
{$IFDEF MSWINDOWS}
var
  wsLinkName, wsTarget: UnicodeString;
begin
  wsLinkName:= UTF16LongName(LinkName);
  if DCNtfsLinks.ReadSymLink(wsLinkName, wsTarget) then
    Result := UTF16ToUTF8(wsTarget)
  else
    Result := EmptyStr;
end;
{$ELSE}
begin
  Result := SysToUTF8(fpReadlink(UTF8ToSys(LinkName)));
end;
{$ENDIF}

end.
