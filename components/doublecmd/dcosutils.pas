{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains platform dependent functions dealing with operating system.

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

unit DCOSUtils;
 
{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, DCClassesUtf8, DCBasicTypes;
    
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
                           caoRemoveReadOnlyAttr);
  TCopyAttributesOptions = set of TCopyAttributesOption;

const
  faInvalidAttributes: TFileAttrs = TFileAttrs(-1);
  CopyAttributesOptionCopyAll = [caoCopyAttributes, caoCopyTime, caoCopyOwnership];

// From Lazarus LCL.
// Once switched to Lazarus 1.0 we can use package LazUtils instead and remove those.
function UTF8ToSys(const s: string): string;// as UTF8ToAnsi but more independent of widestringmanager
function SysToUTF8(const s: string): string;// as AnsiToUTF8 but more independent of widestringmanager

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
function MapFile(const sFileName : UTF8String; out FileMapRec : TFileMapRec) : Boolean;
{en
   Unmap previously mapped file
   @param(FileMapRec TFileMapRec structure)
}
procedure UnMapFile(var FileMapRec : TFileMapRec);

{en
   Convert from console to UTF8 encoding.
}
function ConsoleToUTF8(const Str: AnsiString): UTF8String;

{ File handling functions}
function mbFileOpen(const FileName: UTF8String; Mode: Word): System.THandle;
function mbFileCreate(const FileName: UTF8String): System.THandle; overload;
function mbFileCreate(const FileName: UTF8String; ShareMode: Longint): System.THandle; overload;
function mbFileCreate(const FileName: UTF8String; ShareMode: Longint; Rights: Longint): System.THandle; overload;
function mbFileAge(const FileName: UTF8String): DCBasicTypes.TFileTime;
// On success returns True.
function mbFileGetTime(const FileName: UTF8String;
                       var ModificationTime: DCBasicTypes.TFileTime;
                       var CreationTime    : DCBasicTypes.TFileTime;
                       var LastAccessTime  : DCBasicTypes.TFileTime): Boolean;
// On success returns True.
function mbFileSetTime(const FileName: UTF8String;
                       ModificationTime: DCBasicTypes.TFileTime;
                       CreationTime    : DCBasicTypes.TFileTime = 0;
                       LastAccessTime  : DCBasicTypes.TFileTime = 0): Boolean;
{en
   Checks if a given file exists - it can be a real file or a link to a file,
   but it can be opened and read from.
   Even if the result is @false, we can't be sure a file by that name can be created,
   because there may still exist a directory or link by that name.
}
function mbFileExists(const FileName: UTF8String): Boolean;
function mbFileAccess(const FileName: UTF8String; Mode: Word): Boolean;
function mbFileGetAttr(const FileName: UTF8String): TFileAttrs;
function mbFileSetAttr (const FileName: UTF8String; Attr: TFileAttrs) : LongInt;
{en
   If any operation in Options is performed and does not succeed it is included
   in the result set. If all performed operations succeed the function returns empty set.
   For example for Options=[caoCopyTime, caoCopyOwnership] setting ownership
   doesn't succeed then the function returns [caoCopyOwnership].
}
function mbFileCopyAttr(const sSrc, sDst: UTF8String; Options: TCopyAttributesOptions): TCopyAttributesOptions;
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
function mbCompareFileNames(const FileName1, FileName2: UTF8String): Boolean;
{ Other functions }
function mbGetEnvironmentString(Index : Integer) : UTF8String;
function mbSysErrorMessage(ErrorCode: Integer): UTF8String;
function mbLoadLibrary(const Name: UTF8String): TLibHandle;

implementation

{$IF DEFINED(MSWINDOWS)}
uses
  Windows, JwaWinNetWk;
{$ENDIF}
{$IF DEFINED(UNIX)}
  {$IF DEFINED(DARWIN)}
    {$DEFINE FPC_USE_LIBC}
  {$ENDIF}
uses
  {$IF NOT DEFINED(FPC_USE_LIBC)}
  SysCall,
  {$ENDIF}
  BaseUnix, Unix, dl, DCStrUtils;
{$ENDIF}

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
{$ELSEIF DEFINED(UNIX)}
const
  AccessModes: array[0..2] of LongInt  = (
                O_RdOnly,
                O_WrOnly,
                O_RdWr);

function fpLChown(path : pChar; owner : TUid; group : TGid): cInt; {$IFDEF FPC_USE_LIBC}cdecl; external 'c' name 'lchown';{$ENDIF}
{$IFNDEF FPC_USE_LIBC}
begin
  fpLChown:=do_syscall(syscall_nr_lchown,TSysParam(path),TSysParam(owner),TSysParam(group));
end;
{$ENDIF}
{$ENDIF}

//////////
// From Lazarus, remove once switched to Lazarus 1.0, use LazUtils instead.
var
  FNeedRTLAnsi: boolean = false;
  FNeedRTLAnsiValid: boolean = false;

function NeedRTLAnsi: boolean;
{$IFDEF WinCE}
// CP_UTF8 is missing in the windows unit of the Windows CE RTL
const
  CP_UTF8 = 65001;
{$ENDIF}
{$IFNDEF Windows}
var
  Lang: String;
  i: LongInt;
  Encoding: String;
{$ENDIF}
begin
  if FNeedRTLAnsiValid then
    exit(FNeedRTLAnsi);
  {$IFDEF Windows}
  FNeedRTLAnsi:=GetACP<>CP_UTF8;
  {$ELSE}
  FNeedRTLAnsi:=false;
  Lang := SysUtils.GetEnvironmentVariable('LC_ALL');
  if lang = '' then
  begin
    Lang := SysUtils.GetEnvironmentVariable('LC_MESSAGES');
    if Lang = '' then
    begin
      Lang := SysUtils.GetEnvironmentVariable('LANG');
    end;
  end;
  i:=System.Pos('.',Lang);
  if (i>0) then begin
    Encoding:=copy(Lang,i+1,length(Lang)-i);
    FNeedRTLAnsi:=(SysUtils.CompareText(Encoding,'UTF-8')<>0)
              and (SysUtils.CompareText(Encoding,'UTF8')<>0);
  end;
  {$ENDIF}
  FNeedRTLAnsiValid:=true;
  Result:=FNeedRTLAnsi;
end;

procedure SetNeedRTLAnsi(NewValue: boolean);
begin
  FNeedRTLAnsi:=NewValue;
  FNeedRTLAnsiValid:=true;
end;

function IsASCII(const s: string): boolean; inline;
var
  i: Integer;
begin
  for i:=1 to length(s) do if ord(s[i])>127 then exit(false);
  Result:=true;
end;

function UTF8ToSys(const s: string): string;
begin
  if NeedRTLAnsi and (not IsASCII(s)) then
    Result:=UTF8ToAnsi(s)
  else
    Result:=s;
end;

function SysToUTF8(const s: string): string;
begin
  if NeedRTLAnsi and (not IsASCII(s)) then
  begin
    Result:=AnsiToUTF8(s);
    {$ifdef FPC_HAS_CPSTRING}
    // prevent UTF8 codepage appear in the strings - we don't need codepage
    // conversion magic in LCL code
    SetCodePage(RawByteString(Result), StringCodePage(s), False);
    {$endif}
  end
  else
    Result:=s;
end;
//////////

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

function mbFileCopyAttr(const sSrc, sDst: UTF8String; Options: TCopyAttributesOptions): TCopyAttributesOptions;
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
end;
{$ELSE}  // *nix
var
  StatInfo : BaseUnix.Stat;
  utb : BaseUnix.TUTimBuf;
  mode : TMode;
begin
  if fpLStat(PChar(UTF8ToSys(sSrc)), StatInfo) >= 0 then
  begin
    Result := [];
    if FPS_ISLNK(StatInfo.st_mode) then
    begin
      if caoCopyOwnership in Options then
      begin
        // Only group/owner can be set for links.
        if fpLChown(PChar(UTF8ToSys(sDst)), StatInfo.st_uid, StatInfo.st_gid) = -1 then
        begin
          Include(Result, caoCopyOwnership);
        end;
      end;
    end
    else
    begin
      if caoCopyTime in Options then
      begin
        utb.actime  := StatInfo.st_atime;  // last access time
        utb.modtime := StatInfo.st_mtime;  // last modification time
        if fputime(PChar(UTF8ToSys(sDst)), @utb) <> 0 then
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
        if fpChmod(PChar(UTF8ToSys(sDst)), mode) = -1 then
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

function mbFileOpen(const FileName: UTF8String; Mode: Word): System.THandle;
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

function mbFileAge(const FileName: UTF8String): DCBasicTypes.TFileTime;
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

function mbFileGetTime(const FileName: UTF8String;
                       var ModificationTime: DCBasicTypes.TFileTime;
                       var CreationTime    : DCBasicTypes.TFileTime;
                       var LastAccessTime  : DCBasicTypes.TFileTime): Boolean;
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
                       ModificationTime: DCBasicTypes.TFileTime;
                       CreationTime    : DCBasicTypes.TFileTime = 0;
                       LastAccessTime  : DCBasicTypes.TFileTime = 0): Boolean;
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
  wFileName:= UTF8Decode(FileName);
  Attr:= GetFileAttributesW(PWChar(wFileName));
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

function mbFileAccess(const FileName: UTF8String; Mode: Word): Boolean;
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
  wFileName:= UTF8Decode(FileName);
  dwDesiredAccess := AccessMode[Mode and 3];
  if Mode = fmOpenRead then // If checking Read mode no sharing mode given
    Mode := Mode or fmShareDenyNone;
  dwShareMode := ShareModes[(Mode and $F0) shr 4];
  hFile:= CreateFileW(PWChar(wFileName), dwDesiredAccess, dwShareMode,
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
  if fpLStat(UTF8ToSys(FileName), @Info) >= 0 then
    Result:= Info.st_mode
  else
    Result:= faInvalidAttributes;
end;
{$ENDIF}

function mbFileSetAttr(const FileName: UTF8String; Attr: TFileAttrs): LongInt;
{$IFDEF MSWINDOWS}
var
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(FileName);
  if SetFileAttributesW(PWChar(wFileName), Attr) then
    Result:= 0
  else
    Result:= GetLastError;
end;
{$ELSE}
begin
  Result:= fpchmod(PChar(UTF8ToSys(FileName)), Attr);
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
  wsNewDir: WideString;
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
  // MSDN says that the final character must be a backslash ('\').
  wsNewDir:= wsNewDir + DirectorySeparator;
  Result:= SetCurrentDirectoryW(PWideChar(wsNewDir));
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
  wDirectory:= UTF8Decode(Directory);
  Attr:= GetFileAttributesW(PWChar(wDirectory));
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

function mbSysErrorMessage(ErrorCode: Integer): UTF8String;
begin
  Result :=
{$IFDEF WINDOWS}
            UTF8Encode(SysErrorMessage(ErrorCode));
{$ELSE}
            SysToUTF8(SysErrorMessage(ErrorCode));
{$ENDIF}
end;

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
  Result:= TLibHandle(dlopen(PChar(UTF8ToSys(Name)), RTLD_LAZY));
end;
{$ENDIF}

end.
