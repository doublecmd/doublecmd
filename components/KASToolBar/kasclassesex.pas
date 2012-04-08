{
   Double commander
   -------------------------------------------------------------------------
   This module contains classes with UTF8 file names support.

   Copyright (C) 2008-2009  Koblov Alexander (Alexx2000@mail.ru)

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

unit KASClassesEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  faInvalidAttributes: Cardinal = Cardinal(-1);

type
  { TFileStreamEx class }

  TFileStreamEx = class(THandleStream)
  private
    FFileName: UTF8String;
  public
    constructor Create(const AFileName: UTF8String; Mode: Word);
    destructor Destroy; override;
    property FileName : UTF8String read FFileName;
  end; 

  function mbFileAccess(const FileName: UTF8String; Mode: Integer): Boolean;
  function mbFileCreate(const FileName: UTF8String): THandle;
  function mbFileOpen(const FileName: UTF8String; Mode: Integer): THandle;
  function mbFileExists(const FileName: UTF8String): Boolean;
  function mbDeleteFile(const FileName: UTF8String): Boolean;
  function mbFileGetAttr(const FileName: UTF8String): Cardinal;
  function mbFileSetAttr(const FileName: UTF8String; Attr: Cardinal) : LongInt;
  function mbCreateDir(const NewDir: UTF8String): Boolean;
  function mbFileSize(const FileName: UTF8String): Int64;
  function mbRenameFile(const OldName: UTF8String; NewName: UTF8String): Boolean;
  function mbFileSystemEntryExists(const Path: UTF8String): Boolean;
  function GetTempName(PathPrefix: String): String;


implementation

uses
  RtlConsts
  {$IF DEFINED(MSWINDOWS)}
  , Windows
  {$ELSEIF DEFINED(UNIX)}
  , BaseUnix
  {$ENDIF}
  ;

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
{$ENDIF}

function mbFileOpen(const FileName: UTF8String; Mode: Integer): THandle;
{$IFDEF MSWINDOWS}
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

function mbFileCreate(const FileName: UTF8String): THandle;
{$IFDEF MSWINDOWS}
var
  wFileName: WideString;
begin
  wFileName:= UTF8Decode(FileName);
  Result:= CreateFileW(PWChar(wFileName), GENERIC_READ or GENERIC_WRITE,
                       FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;
{$ELSE}
begin
  Result:= fpOpen(FileName, O_Creat or O_RdWr or O_Trunc);
end;
{$ENDIF}

{ TFileStreamEx }

constructor TFileStreamEx.Create(const AFileName: UTF8String; Mode: Word);
var
  H: System.THandle;
begin
{$IF (FPC_VERSION > 2) or ((FPC_VERSION=2) and (FPC_RELEASE >= 5))}
  if (Mode and fmCreate) <> 0 then
{$ELSE}
  if Mode = fmCreate then
{$ENDIF}
    begin
      H:= mbFileCreate(AFileName);
      if H = feInvalidHandle then
        raise EFCreateError.CreateFmt(SFCreateError, [AFileName])
      else
        inherited Create(H);
    end
  else
    begin 
      H:= mbFileOpen(AFileName, Mode);
      if H = feInvalidHandle then
        raise EFOpenError.CreateFmt(SFOpenError, [AFilename])
      else
        inherited Create(H);
    end;
  FFileName:= AFileName;
end;

destructor TFileStreamEx.Destroy;
begin
  inherited Destroy;
  // Close handle after destroying the base object, because it may use Handle in Destroy.
  if Handle >= 0 then
    FileClose(Handle);
end;

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
  Result:= fpUnLink(FileName) = 0;
end;
{$ENDIF}

function mbFileGetAttr(const FileName: UTF8String): Cardinal;
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
  Result:= fpMkDir(PChar(NewDir), $1FF) = 0; // $1FF = &0777
end;
{$ENDIF}

function mbFileSetAttr(const FileName: UTF8String; Attr: Cardinal): LongInt;
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

function mbFileSize(const FileName: UTF8String): Int64;
{$IFDEF MSWINDOWS}
var
  Handle: System.THandle;
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

function mbFileSystemEntryExists(const Path: UTF8String): Boolean;
begin
  Result := mbFileGetAttr(Path) <> faInvalidAttributes;
end;

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

end.
