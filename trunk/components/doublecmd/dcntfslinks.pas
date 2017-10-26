{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains functions to work with hard and symbolic links
    on the NTFS file system.

    Copyright (C) 2012-2017 Alexander Koblov (alexx2000@mail.ru)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit DCNtfsLinks;

{$mode delphi}

interface

uses
  Windows, SysUtils;

const
  // CreateSymbolicLink flags
  SYMBOLIC_LINK_FLAG_FILE      = 0;
  SYMBOLIC_LINK_FLAG_DIRECTORY = 1;
  // CreateFile flags
  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;
  // DeviceIoControl control codes
  FSCTL_SET_REPARSE_POINT      = $000900A4;
  FSCTL_GET_REPARSE_POINT      = $000900A8;
  FSCTL_DELETE_REPARSE_POINT   = $000900AC;

const
  REPARSE_DATA_HEADER_SIZE = 8;
  MOUNT_POINT_HEADER_SIZE  = 8;
  FILE_DOES_NOT_EXIST = DWORD(-1);
  wsLongFileNamePrefix = UnicodeString('\\?\');
  wsNativeFileNamePrefix = UnicodeString('\??\');

type
  {$packrecords c}
  TSymbolicLinkReparseBuffer = record
    SubstituteNameOffset: USHORT;
    SubstituteNameLength: USHORT;
    PrintNameOffset: USHORT;
    PrintNameLength: USHORT;
    Flags: ULONG;
    PathBuffer: array[0..0] of WCHAR;
   end;

  TMountPointReparseBuffer = record
    SubstituteNameOffset: USHORT;
    SubstituteNameLength: USHORT;
    PrintNameOffset: USHORT;
    PrintNameLength: USHORT;
    PathBuffer: array[0..0] of WCHAR;
  end;

  TGenericReparseBuffer = record
    DataBuffer: array[0..0] of UCHAR;
  end;

  REPARSE_DATA_BUFFER = record
    ReparseTag: ULONG;
    ReparseDataLength: USHORT;
    Reserved: USHORT;
    case Integer of
    0: (SymbolicLinkReparseBuffer: TSymbolicLinkReparseBuffer);
    1: (MountPointReparseBuffer: TMountPointReparseBuffer);
    2: (GenericReparseBuffer: TGenericReparseBuffer);
  end;
  TReparseDataBuffer = REPARSE_DATA_BUFFER;
  PReparseDataBuffer = ^REPARSE_DATA_BUFFER;
  {$packrecords default}

{en
   Creates a symbolic link.
   This function is only supported on the NTFS file system.
   On Windows 2000/XP it works for directories only
   On Windows Vista/Seven it works for directories and files
   (for files it works only with Administrator rights)
   @param(AFileName The name of the existing file)
   @param(ALinkName The name of the symbolic link)
   @returns(The function returns @true if successful, @false otherwise)
}
function CreateSymLink(const ATargetName, ALinkName: UnicodeString): Boolean;
{en
   Established a hard link beetwen an existing file and new file. This function
   is only supported on the NTFS file system, and only for files, not directories.
   @param(AFileName The name of the existing file)
   @param(ALinkName The name of the new hard link)
   @returns(The function returns @true if successful, @false otherwise)
}
function CreateHardLink(const AFileName, ALinkName: UnicodeString): Boolean;
{en
   Reads a symbolic link target.
   This function is only supported on the NTFS file system.
   @param(aSymlinkFileName The name of the symbolic link)
   @param(aTargetFileName The name of the target file/directory)
   @returns(The function returns @true if successful, @false otherwise)
}
function ReadSymLink(const aSymlinkFileName: UnicodeString; out aTargetFileName: UnicodeString): Boolean;

implementation

const
  ERROR_DIRECTORY_NOT_SUPPORTED = 336;

type
  TCreateSymbolicLinkW = function(
    pwcSymlinkFileName,
    pwcTargetFileName: PWideChar;
    dwFlags: DWORD): BOOL; stdcall;

  TCreateHardLinkW = function (
    lpFileName,
    lpExistingFileName: LPCWSTR;
    lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;

var
  HasNewApi: Boolean = False;
  MayCreateSymLink: Boolean = False;

function _CreateHardLink_New(AFileName : UnicodeString; ALinkName: UnicodeString): Boolean;
var
  hLib: THandle;
  CreateHardLinkW: TCreateHardLinkW;
begin
  Result:= False;

  hLib:= GetModuleHandle('kernel32.dll');
  if hLib = 0 then
    begin
      Assert(False, 'Can not load library "kernel32.dll"');
      Exit;
    end;

  CreateHardLinkW:= TCreateHardLinkW(GetProcAddress(hLib, 'CreateHardLinkW'));
  if not Assigned(CreateHardLinkW) then
    begin
      Assert(False, 'Can not get function address for "CreateHardLinkW"');
      Exit;
    end;

  Result:= CreateHardLinkW(PWideChar(ALinkName), PWideChar(AFileName), nil);
end;

function _CreateHardLink_Old(aExistingFileName, aFileName: UnicodeString): Boolean;
var
  hFile: THandle;
  lpBuffer: TWin32StreamId;
  wcFileName: array[0..MAX_PATH] of WideChar;
  dwNumberOfBytesWritten: DWORD = 0;
  lpContext: LPVOID = nil;
  lpFilePart: LPWSTR = nil;
begin
  Result:= GetFullPathNameW(PWideChar(aFileName), MAX_PATH, wcFileName, lpFilePart) > 0;
  if Result then
  begin
    hFile:= CreateFileW(PWideChar(aExistingFileName),
                        GENERIC_READ or GENERIC_WRITE,
                        FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                        nil, OPEN_EXISTING, 0, 0);
    Result:= (hFile <> INVALID_HANDLE_VALUE);
  end;
  if Result then
  try
    ZeroMemory(@lpBuffer, SizeOf(TWin32StreamId));
    with lpBuffer do
    begin
      dwStreamId:= BACKUP_LINK;
      Size.LowPart:= (Length(aFileName) + 1) * SizeOf(WideChar);
    end;
    // Write stream header
    Result:= BackupWrite(hFile,
                         @lpBuffer,
                         SizeOf(TWin32StreamId) - SizeOf(PWideChar),
                         dwNumberOfBytesWritten,
                         False,
                         False,
                         lpContext);
    if not Result then Exit;
    // Write file name buffer
    Result:= BackupWrite(hFile,
                         @wcFileName,
                         lpBuffer.Size.LowPart,
                         dwNumberOfBytesWritten,
                         False,
                         False,
                         lpContext);
    if not Result then Exit;
    // Finish write operation
    Result:= BackupWrite(hFile,
                         nil,
                         0,
                         dwNumberOfBytesWritten,
                         True,
                         False,
                         lpContext);
  finally
    CloseHandle(hFile);
  end;
end;

function CreateHardLink(const AFileName, ALinkName: UnicodeString): Boolean;
var
  dwAttributes: DWORD;
begin
  dwAttributes := Windows.GetFileAttributesW(PWideChar(AFileName));
  if dwAttributes = FILE_DOES_NOT_EXIST then Exit(False);
  if (dwAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
  begin
    SetLastError(ERROR_DIRECTORY_NOT_SUPPORTED);
    Exit(False);
  end;

  dwAttributes := Windows.GetFileAttributesW(PWideChar(ALinkName));
  if dwAttributes <> FILE_DOES_NOT_EXIST then
  begin
    SetLastError(ERROR_FILE_EXISTS);
    Exit(False);
  end;

  if HasNewApi then
    Result:= _CreateHardLink_New(AFileName, ALinkName)
  else
    Result:= _CreateHardLink_Old(AFileName, ALinkName)
end;

function _CreateSymLink_New(const ATargetFileName, ASymlinkFileName: UnicodeString; dwFlags: DWORD): boolean;
var
  hLib: THandle;
  CreateSymbolicLinkW: TCreateSymbolicLinkW;
begin
  Result:= False;

  hLib:= GetModuleHandle('kernel32.dll');
  if hLib = 0 then
    begin
      Assert(False, 'Can not load library "kernel32.dll"');
      Exit;
    end;

  CreateSymbolicLinkW:= TCreateSymbolicLinkW(GetProcAddress(hLib, 'CreateSymbolicLinkW'));
  if not Assigned(CreateSymbolicLinkW) then
    begin
      Assert(False, 'Can not get function address for "CreateSymbolicLinkW"');
      Exit;
    end;

  Result:= CreateSymbolicLinkW(PWideChar(ASymlinkFileName), PWideChar(ATargetFileName), dwFlags);
end;

function _CreateSymLink_Old(aTargetFileName, aSymlinkFileName: UnicodeString): Boolean;
var
  hDevice: THandle;
  lpInBuffer: PReparseDataBuffer;
  nInBufferSize,
  dwPathBufferSize: DWORD;
  wsNativeFileName: UnicodeString;
  lpBytesReturned: DWORD = 0;
begin
  Result:= CreateDirectoryW(PWideChar(aSymlinkFileName), nil);
  if Result then
  try
    hDevice:= CreateFileW(PWideChar(aSymlinkFileName),
                          GENERIC_WRITE, 0, nil, OPEN_EXISTING,
                          FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0);
    if hDevice = INVALID_HANDLE_VALUE then Exit;
    if Pos(wsLongFileNamePrefix, aTargetFileName) <> 1 then
      wsNativeFileName:= wsNativeFileNamePrefix + aTargetFileName
    else begin
      wsNativeFileName:= wsNativeFileNamePrefix + Copy(aTargetFileName, 5, MaxInt);
    end;
    // File name length with trailing zero and zero for empty PrintName
    dwPathBufferSize:= Length(wsNativeFileName) * SizeOf(WideChar) + 4;
    nInBufferSize:= REPARSE_DATA_HEADER_SIZE + MOUNT_POINT_HEADER_SIZE + dwPathBufferSize;
    lpInBuffer:= GetMem(nInBufferSize);
    ZeroMemory(lpInBuffer, nInBufferSize);
    with lpInBuffer^, lpInBuffer^.MountPointReparseBuffer do
    begin
      ReparseTag:= IO_REPARSE_TAG_MOUNT_POINT;
      ReparseDataLength:= MOUNT_POINT_HEADER_SIZE + dwPathBufferSize;
      SubstituteNameLength:= Length(wsNativeFileName) * SizeOf(WideChar);
      PrintNameOffset:= SubstituteNameOffset + SubstituteNameLength + SizeOf(WideChar);
      CopyMemory(@PathBuffer[0], @wsNativeFileName[1], SubstituteNameLength);
    end;
    Result:= DeviceIoControl(hDevice,                  // handle to file or directory
                             FSCTL_SET_REPARSE_POINT,  // dwIoControlCode
                             lpInBuffer,               // input buffer
                             nInBufferSize,            // size of input buffer
                             nil,                      // lpOutBuffer
                             0,                        // nOutBufferSize
                             lpBytesReturned,          // lpBytesReturned
                             nil);                     // OVERLAPPED structure

    FreeMem(lpInBuffer);
    CloseHandle(hDevice);
  finally
    if not Result then RemoveDirectoryW(PWideChar(aSymlinkFileName));
  end;
end;

function CreateSymLink(const ATargetName, ALinkName: UnicodeString): Boolean;
var
  dwAttributes: DWORD;
  lpFilePart: LPWSTR = nil;
  AFileName, AFullPathName: UnicodeString;
begin
  Result:= False;
  if (Length(ATargetName) > 1) and CharInSet(ATargetName[2], [':', '\']) then
    AFullPathName:= ATargetName
  else begin
    SetLength(AFullPathName, MaxSmallint);
    AFileName:= ExtractFilePath(ALinkName) + ATargetName;
    dwAttributes:= GetFullPathNameW(PWideChar(AFileName), MaxSmallint, PWideChar(AFullPathName), lpFilePart);
    if dwAttributes > 0 then
      SetLength(AFullPathName, dwAttributes)
    else begin
      AFullPathName:= ATargetName;
    end;
  end;
  dwAttributes:= Windows.GetFileAttributesW(PWideChar(AFullPathName));
  if dwAttributes = FILE_DOES_NOT_EXIST then Exit;
  if HasNewApi = False then
  begin
    if (dwAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
      Result:= _CreateSymLink_Old(AFullPathName, ALinkName)
    else
      SetLastError(ERROR_NOT_SUPPORTED);
  end
  else begin
    if (dwAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      Result:= _CreateSymLink_New(ATargetName, ALinkName, SYMBOLIC_LINK_FLAG_FILE)
    else begin
      if not MayCreateSymLink then
        Result:= _CreateSymLink_Old(AFullPathName, ALinkName)
      else begin
        Result:= _CreateSymLink_New(ATargetName, ALinkName, SYMBOLIC_LINK_FLAG_DIRECTORY);
      end;
    end;
  end;
end;

function ReadSymLink(const aSymlinkFileName: UnicodeString; out aTargetFileName: UnicodeString): Boolean;
var
  hDevice: THandle;
  dwFileAttributes: DWORD;
  caOutBuffer: array[0..4095] of Byte;
  lpOutBuffer: TReparseDataBuffer absolute caOutBuffer;
  pwcTargetFileName: PWideChar;
  lpBytesReturned: DWORD = 0;
  dwFlagsAndAttributes: DWORD;
begin
  dwFileAttributes:= GetFileAttributesW(PWideChar(aSymlinkFileName));
  Result:= dwFileAttributes <> FILE_DOES_NOT_EXIST;
  if Result then
  begin
    if (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      dwFlagsAndAttributes:= FILE_FLAG_OPEN_REPARSE_POINT
    else
      dwFlagsAndAttributes:= FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT;
    // Open reparse point
    hDevice:= CreateFileW(PWideChar(aSymlinkFileName),
                          0, FILE_SHARE_READ or FILE_SHARE_WRITE,
                          nil, OPEN_EXISTING, dwFlagsAndAttributes, 0);
    Result:= hDevice <> INVALID_HANDLE_VALUE;
    if not Result then Exit;
    Result:= DeviceIoControl(hDevice,                  // handle to file or directory
                             FSCTL_GET_REPARSE_POINT,  // dwIoControlCode
                             nil,                      // input buffer
                             0,                        // size of input buffer
                             @caOutBuffer,             // lpOutBuffer
                             SizeOf(caOutBuffer),      // nOutBufferSize
                             lpBytesReturned,          // lpBytesReturned
                             nil);                     // OVERLAPPED structure
    CloseHandle(hDevice);
    if Result then
    begin
      case lpOutBuffer.ReparseTag of
      IO_REPARSE_TAG_SYMLINK:
        with lpOutBuffer.SymbolicLinkReparseBuffer do
        begin
          pwcTargetFileName:= @PathBuffer[0];
          pwcTargetFileName:= pwcTargetFileName + SubstituteNameOffset div SizeOf(WideChar);
          SetLength(aTargetFileName, SubstituteNameLength div SizeOf(WideChar));
          CopyMemory(PWideChar(aTargetFileName), pwcTargetFileName, SubstituteNameLength);
        end;
      IO_REPARSE_TAG_MOUNT_POINT:
        with lpOutBuffer.MountPointReparseBuffer do
        begin
          pwcTargetFileName:= @PathBuffer[0];
          pwcTargetFileName:= pwcTargetFileName + SubstituteNameOffset div SizeOf(WideChar);
          SetLength(aTargetFileName, SubstituteNameLength div SizeOf(WideChar));
          CopyMemory(PWideChar(aTargetFileName), pwcTargetFileName, SubstituteNameLength);
        end;
      end;
      if Pos(wsNativeFileNamePrefix, aTargetFileName) = 1 then
        Delete(aTargetFileName, 1, Length(wsNativeFileNamePrefix));
    end;
  end;
end;

function MayCreateSymbolicLink: Boolean;
const
  SE_CREATE_SYMBOLIC_LINK_NAME = 'SeCreateSymbolicLinkPrivilege';
var
  I: Integer;
  hProcess: HANDLE;
  dwLength: DWORD = 0;
  seCreateSymbolicLink: LUID = 0;
  TokenInformation: array [0..1023] of Byte;
  Privileges: TTokenPrivileges absolute TokenInformation;
begin
  hProcess:= GetCurrentProcess();
  if (OpenProcessToken(hProcess, TOKEN_READ, hProcess)) then
  try
    if (LookupPrivilegeValueW(nil, SE_CREATE_SYMBOLIC_LINK_NAME, seCreateSymbolicLink)) then
    begin
      if (GetTokenInformation(hProcess, TokenPrivileges, @Privileges, SizeOf(TokenInformation), dwLength)) then
      begin
        {$PUSH}{$R-}
        for I:= 0 to Int32(Privileges.PrivilegeCount) - 1 do
        begin
          if Privileges.Privileges[I].Luid = seCreateSymbolicLink then
            Exit(True);
        end;
        {$POP}
      end;
    end;
  finally
    CloseHandle(hProcess);
  end;
  Result:= False;
end;

initialization
  MayCreateSymLink:= MayCreateSymbolicLink;
  HasNewApi:= (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6);

end.
