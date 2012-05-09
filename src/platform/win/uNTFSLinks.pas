{
  This unit contains functions for work with hard and symbolic links
  on the NTFS file system.

  Based on:

   //====================================================================
   //  **** UBPFD *********** by kladovka.net.ru ****
   //
   //  xlink - Исходный код утилиты, которая создает hard и symbolic links
   //  почти как в unix.
   //
   //  Author:      Alex Konshin, akonshin@earthlink.net, Boston, USA
   //  Copyright:   http://home.earthlink.net/~akonshin/files/xlink.zip
   //  Date:        30.12.2002
   //====================================================================

   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

   //====================================================================
   //  Junction creation and listing utility, based on Junction.c source
   //  by Mark Russinovich, http://www.sysinternals.com. Thanks Mark!
   //
   //  Note: targets of some rare reparse point types are not recognized,
   //  as in Mark's source.
   //
   //  (C) Alexey Torgashin, http://alextpp.narod.ru, atorg@yandex.ru
   //  18.02.06 - initial version
   //====================================================================
}

unit uNTFSLinks;

{$mode delphi}

interface

uses
  Windows, SysUtils;

type
   TReparsePointType = (
    slUnknown,
    slJunction,
    slMountPoint,
    slSymLink,
    slHSM,
    slSIS,
    slDFS
    );

const
  FILE_DOES_NOT_EXIST = DWORD(-1);
  SReparsePointType: array[TReparsePointType] of PAnsiChar = (
    'Unknown point type',
    'Junction',
    'Mount Point',
    'Symbolic Link',
    'Hierarchical Storage Management point',
    'Single Instance Store point',
    'Distributed File System point'
    );

{en
   Creates a symbolic link. This function is only supported on the NTFS file system.
   On Windows 2000/XP it works for directories only
   On Windows Vista/Seven it works for directories and files
   (for files it works only with Administrator rights)
   @param(AFileName The name of the existing file)
   @param(ALinkName The name of the symbolic link)
   @returns(The function returns @true if successful, @false otherwise)
}
function CreateSymLink(ATargetName, ALinkName: WideString): Boolean;
{en
   Established a hard link beetwen an existing file and new file. This function
   is only supported on the NTFS file system, and only for files, not directories.
   @param(AFileName The name of the existing file)
   @param(ALinkName The name of the new hard link)
   @returns(The function returns @true if successful, @false otherwise)
}
function CreateHardLink(AFileName, ALinkName: WideString): Boolean;

function GetSymlinkInfo(const fn: WideString; out Target: WideString; out LinkType: TReparsePointType): Boolean;
function DriveSupportsSymlinks(const fn: WideString): boolean;

implementation

uses
  LCLProc, uDebug;

//-------------------------------------------------------------

type
  int64rec = packed record
    lo: LongWord;
    hi: LongInt;
  end;

//-------------------------------------------------------------

function HasNewApi: Boolean;
begin
  Result:= (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6);
end;

//-------------------------------------------------------------

type
  TCreateHardLinkW = function (
    lpFileName,
    lpExistingFileName: LPCWSTR;
    lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;

//-------------------------------------------------------------

function _CreateHardLink_New(AFileName : WideString; ALinkName: WideString): Boolean;
var
  hLib: THandle;
  CreateHardLinkW: TCreateHardLinkW;
begin
  Result:= False;

  hLib:= GetModuleHandle('kernel32.dll');
  if hLib = 0 then
    begin
      DCDebug('Can not load library "kernel32.dll"');
      Exit;
    end;

  CreateHardLinkW:= TCreateHardLinkW(GetProcAddress(hLib, 'CreateHardLinkW'));
  if not Assigned(CreateHardLinkW) then
    begin
      DCDebug('Can not get function address for "CreateHardLinkW"');
      Exit;
    end;

  Result:= CreateHardLinkW(PWideChar(ALinkName), PWideChar(AFileName), nil);
end;

function _CreateHardLink_Old(AFileName : WideString; ALinkName: WideString): Boolean;
var
  aLinkFullName: Array[0..MAX_PATH] of WideChar;
  pwFilePart: LPWSTR;
  hFileSource: THandle;
  rStreamId: WIN32_STREAM_ID;
  cbPathLen, dwStreamHeaderSize, dwBytesWritten: DWORD;
  lpContext: Pointer;
begin
  Result:= False;
  hFileSource :=
    Windows.CreateFileW(
      PWideChar(AFileName),
      GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
      nil,
      OPEN_EXISTING,
      0,
      0
    );

  if hFileSource=INVALID_HANDLE_VALUE then 
    raise Exception.Create('Can''t open file "' + AFileName + '"');

  try
    cbPathLen := Windows.GetFullPathNameW(PWideChar(ALinkName), MAX_PATH,
      aLinkFullName, pwFilePart);
    if cbPathLen <= 0 then
      raise Exception.Create('Invalid link name "' + ALinkName + '"');

    cbPathLen := (cbPathLen + 1) * SizeOf(WideChar);

    lpContext := nil;

    rStreamId.dwStreamId := BACKUP_LINK;
    rStreamId.dwStreamAttributes := 0;
    rStreamId.dwStreamNameSize := 0;
    int64rec(rStreamId.Size).hi := 0;
    int64rec(rStreamId.Size).lo := cbPathLen;
    dwStreamHeaderSize := PChar(@rStreamId.cStreamName)-PChar(@rStreamId)
      +LongInt(rStreamId.dwStreamNameSize);

    if not BackupWrite(
        hFileSource,
        Pointer(@rStreamId), // buffer to write
        dwStreamHeaderSize, // number of bytes to write
        dwBytesWritten,
        False, // don't abort yet
        False, // don't process security
        lpContext
      ) then RaiseLastOSError;

    if not BackupWrite(
        hFileSource,
        Pointer(@aLinkFullName), // buffer to write
        cbPathLen, // number of bytes to write
        dwBytesWritten,
        False, // don't abort yet
        False, // don't process security
        lpContext
      ) then RaiseLastOSError;

    // free context
    if not BackupWrite(
        hFileSource,
        nil, // buffer to write
        0, // number of bytes to write
        dwBytesWritten,
        True, // abort
        False, // don't process security
        lpContext
      ) then RaiseLastOSError;

    Result:= True;
  finally
    CloseHandle(hFileSource);
  end;
end;

function CreateHardLink(AFileName, ALinkName: WideString): Boolean;
var
  dwAttributes: DWORD;
begin
  dwAttributes := Windows.GetFileAttributesW(PWideChar(AFileName));
  if dwAttributes = FILE_DOES_NOT_EXIST then
    raise Exception.Create('File "' + AFileName + '" does not exist.');
  if (dwAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
    raise Exception.Create('Can''t create hardlink for directory (file "' + AFileName + '").');

  dwAttributes := Windows.GetFileAttributesW(PWideChar(ALinkName));
  if dwAttributes <> FILE_DOES_NOT_EXIST then
    raise Exception.Create('File "' + ALinkName + '" already exists.');

  if HasNewApi then
    Result:= _CreateHardLink_New(AFileName, ALinkName)
  else
    Result:= _CreateHardLink_Old(AFileName, ALinkName)
end;

//-------------------------------------------------------------

const
  FILE_DEVICE_FILE_SYSTEM = $0009;
  // Define the method codes for how buffers are passed for I/O and FS controls
  METHOD_BUFFERED = 0;
  METHOD_IN_DIRECT = 1;
  METHOD_OUT_DIRECT = 2;
  METHOD_NEITHER = 3;

  // Define the access check value for any access
  FILE_ANY_ACCESS = 0;

  FSCTL_SET_REPARSE_POINT = (FILE_DEVICE_FILE_SYSTEM shl 16) or
    (FILE_ANY_ACCESS shl 14) or (41 shl 2) or (METHOD_BUFFERED);
  FSCTL_GET_REPARSE_POINT = (FILE_DEVICE_FILE_SYSTEM shl 16) or
    (FILE_ANY_ACCESS shl 14) or (42 shl 2) or (METHOD_BUFFERED);
  FSCTL_DELETE_REPARSE_POINT = (FILE_DEVICE_FILE_SYSTEM shl 16) or
    (FILE_ANY_ACCESS shl 14) or (43 shl 2) or (METHOD_BUFFERED);

  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;


  REPARSE_MOUNTPOINT_HEADER_SIZE = 8;

  MAX_REPARSE_SIZE = 17000;
  MAX_NAME_LENGTH = 1024;

  FILE_SUPPORTS_REPARSE_POINTS = $00000080;

type
  REPARSE_MOUNTPOINT_DATA_BUFFER = packed record
    ReparseTag : DWORD;
    ReparseDataLength : DWORD;
    Reserved : Word;
    ReparseTargetLength : Word;
    ReparseTargetMaximumLength : Word;
    Reserved1 : Word;
    ReparseTarget : Array [0..0] of WChar;
  end;
  TReparseMountpointDataBuffer = REPARSE_MOUNTPOINT_DATA_BUFFER;
  PReparseMountpointDataBuffer = ^TReparseMountpointDataBuffer;

  REPARSE_DATA_BUFFER = packed record
    ReparseTag: DWORD;
    ReparseDataLength: Word;
    Reserved: Word;
    SubstituteNameOffset: Word;
    SubstituteNameLength: Word;
    PrintNameOffset: Word;
    PrintNameLength: Word;
    PathBuffer: array[0..0] of WideChar;
  end;
  TReparseDataBuffer = REPARSE_DATA_BUFFER;
  PReparseDataBuffer = ^TReparseDataBuffer;

const
  SYMBOLIC_LINK_FLAG_FILE      = 0;
  SYMBOLIC_LINK_FLAG_DIRECTORY = 1;
  wsNativeFileNamePrefix : WideString = '\??\';

type
  TGetFinalPathNameByHandleW = function(
    hFile: THandle;
    pwcFilePath: PWideChar;
    cchFilePath: DWORD;
    dwFlags: DWORD): DWORD; stdcall;

type
  TCreateSymbolicLinkW = function(
    pwcSymlinkFileName,
    pwcTargetFileName: PWideChar;
    dwFlags: DWORD): BOOL; stdcall;

//-------------------------------------------------------------

function _CreateSymLink_New(const ATargetFileName, ASymlinkFileName: WideString): boolean;
var
  hLib: THandle;
  CreateSymbolicLinkW: TCreateSymbolicLinkW;
begin
  Result:= False;

  hLib:= GetModuleHandle('kernel32.dll');
  if hLib = 0 then
    begin
      DCDebug('Can not load library "kernel32.dll"');
      Exit;
    end;

  CreateSymbolicLinkW:= TCreateSymbolicLinkW(GetProcAddress(hLib, 'CreateSymbolicLinkW'));
  if not Assigned(CreateSymbolicLinkW) then
    begin
      DCDebug('Can not get function address for "CreateSymbolicLinkW"');
      Exit;
    end;

  Result:= CreateSymbolicLinkW(PWideChar(ASymlinkFileName), PWideChar(ATargetFileName), SYMBOLIC_LINK_FLAG_FILE);
end;

function _CreateSymLink_Old(ATargetName, ALinkName: WideString): Boolean;
var
  hLink : THandle;
  pReparseInfo : PReparseMountPointDataBuffer;
  BufSize : Integer;
  pwcLinkFileName : PWideChar;
  wTargetNativeFileName : WideString;
  dwBytesReturned : DWORD;
  dwAttributes : DWORD;
  bDirectoryCreated : Boolean;
begin
  Result := False;
  pReparseInfo := nil;
  hLink := INVALID_HANDLE_VALUE;
  bDirectoryCreated := False;

  try
    pwcLinkFileName:= PWideChar(ALinkName);
    dwAttributes := Windows.GetFileAttributesW(pwcLinkFileName);
    if dwAttributes <> FILE_DOES_NOT_EXIST then Exit;

    if not Windows.CreateDirectoryW(pwcLinkFileName, nil) then
      Exit
    else
      bDirectoryCreated := True;

    try
      hLink := Windows.CreateFileW(pwcLinkFileName, GENERIC_WRITE, 0, nil,
          OPEN_EXISTING,
          FILE_FLAG_OPEN_REPARSE_POINT or FILE_FLAG_BACKUP_SEMANTICS, 0);

      if hLink = INVALID_HANDLE_VALUE then RaiseLastOSError;

      //--------------------------------------------------------
      wTargetNativeFileName:= wsNativeFileNamePrefix + ATargetName;
      BufSize:= (Length(wsNativeFileNamePrefix) + Length(ATargetName) + 1) * 2 + REPARSE_MOUNTPOINT_HEADER_SIZE + 12;
      GetMem(pReparseInfo, BufSize);
      FillChar(pReparseInfo^, BufSize, 0);

      with pReparseInfo^ do
      begin
        Move(wTargetNativeFileName[1], ReparseTarget, (Length(wTargetNativeFileName) + 1) * 2);
        ReparseTag:= IO_REPARSE_TAG_MOUNT_POINT;
        ReparseTargetLength:= Length(wTargetNativeFileName) * 2;
        ReparseTargetMaximumLength:= ReparseTargetLength + 2;
        ReparseDataLength:= ReparseTargetLength + 12;
      end;
      //--------------------------------------------------------

      dwBytesReturned := 0;
      if not DeviceIoControl( hLink, FSCTL_SET_REPARSE_POINT, pReparseInfo,
              pReparseInfo^.ReparseDataLength + REPARSE_MOUNTPOINT_HEADER_SIZE,
              nil, 0, dwBytesReturned, nil ) then RaiseLastOSError;

    except
      if bDirectoryCreated then RemoveDirectoryW(pwcLinkFileName);
      raise;
    end;

    Result := True;
  finally
    if hLink <> INVALID_HANDLE_VALUE then Windows.CloseHandle(hLink);
    if pReparseInfo <> nil then System.FreeMem(pReparseInfo);
  end;
end;

function CreateSymLink(ATargetName, ALinkName: WideString): Boolean;
var
  dwAttributes : DWORD;
begin
  Result:= False;
  dwAttributes := Windows.GetFileAttributesW(PWideChar(ATargetName));
  if (dwAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
    Result:= _CreateSymLink_Old(ATargetName, ALinkName)
  else if HasNewApi then
    Result:= _CreateSymLink_New(ATargetName, ALinkName);
end;

//-------------------------------------------------------------

function GetSymlinkInfo(const fn: WideString; out Target: WideString; out LinkType: TReparsePointType): Boolean;
var
  attr: DWORD;
  h: THandle;
  reparseBuffer: array[0..MAX_REPARSE_SIZE-1] of char;
  reparseInfo: PReparseDataBuffer;
  reparseData: pointer;
  //reparseData1,
  reparseData2: PWideChar;
  //name1,
  name2: array[0..MAX_NAME_LENGTH-1] of WideChar;
  returnedLength: DWORD;
  control: boolean;
begin
  Result:= false;
  Target:= '';
  LinkType:= slUnknown;

  attr:= GetFileAttributesW(PWideChar(fn));
  if (attr and FILE_ATTRIBUTE_REPARSE_POINT)=0 then Exit;

  if (attr and FILE_ATTRIBUTE_DIRECTORY)<>0 then
    h:= CreateFileW(PWideChar(fn), 0,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING,
      FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0)
  else
    h:= CreateFileW(PWideChar(fn), 0,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING,
      FILE_FLAG_OPEN_REPARSE_POINT, 0);

  if h = INVALID_HANDLE_VALUE then
    begin
      DCDebug('CreateFileW failed');
      Exit;
    end;

  reparseInfo:= @reparseBuffer;
  control:= DeviceIoControl(h, FSCTL_GET_REPARSE_POINT,
                            nil, 0, reparseInfo, SizeOf(reparseBuffer),
                            returnedLength, nil);
  CloseHandle(h);

  if not control then
    begin
      DCDebug('DeviceIoControl failed');
      Exit;
    end;

  case reparseInfo^.ReparseTag of
    IO_REPARSE_TAG_MOUNT_POINT:
      begin
        reparseData:= @reparseInfo.PathBuffer;

        {
        FillChar(name1, SizeOf(name1), 0);
        reparseData1:= reparseData + reparseInfo.PrintNameOffset;
        lstrcpynW(name1, reparseData1, reparseInfo.PrintNameLength);
        }

        FillChar(name2, SizeOf(name2), 0);
        reparseData2:= reparseData + reparseInfo.SubstituteNameOffset;
        lstrcpynW(name2, reparseData2, reparseInfo.SubstituteNameLength);

        Target:= name2;
        if Pos(wsNativeFileNamePrefix, Target) = 1 then
          Delete(Target, 1, Length(wsNativeFileNamePrefix));

        if Pos(':', Target) > 0 then
          LinkType:= slJunction
        else
          LinkType:= slMountPoint;

        Result:= True;
      end;

    IO_REPARSE_TAG_SYMLINK:
      begin
        reparseData:= @reparseInfo.PathBuffer;

        {
        FillChar(name1, SizeOf(name1), 0);
        reparseData1:= reparseData + reparseInfo.PrintNameOffset;
        lstrcpynW(name1, reparseData1, reparseInfo.PrintNameLength);
        }

        FillChar(name2, SizeOf(name2), 0);
        reparseData2:= reparseData + reparseInfo.SubstituteNameOffset + 4;
        lstrcpynW(name2, reparseData2, (reparseInfo.SubstituteNameLength div 2) + 1);

        Target:= name2;
        if Pos(wsNativeFileNamePrefix, Target) = 1 then
          Delete(Target, 1, Length(wsNativeFileNamePrefix));

        LinkType:= slSymLink;

        Result:= true;
      end;

    IO_REPARSE_TAG_HSM:
      begin
        LinkType:= slHSM;
        Result:= true;
      end;

    IO_REPARSE_TAG_SIS:
      begin
        LinkType:= slSIS;
        Result:= true;
      end;

    IO_REPARSE_TAG_DFS:
      begin
        LinkType:= slDFS;
        Result:= true;
      end;
  end;
end;

//-------------------------------------------------------------

function DriveSupportsSymlinks(const fn: WideString): Boolean;
var
  disk: WideString;
  buf1, buf2: array[0..50] of WideChar;
  Serial, NameLen, Flags: DWORD;
begin
  Result:= False;
  if (fn = '') or (Pos(':\', fn) <> 2) then Exit;
  disk := fn[1] + ':\';
  FillChar(buf1, SizeOf(buf1), 0);
  FillChar(buf2, SizeOf(buf2), 0);
  if GetVolumeInformationW(PWideChar(disk), @buf1, SizeOf(buf1),
    @Serial, NameLen, Flags, @buf2, SizeOf(buf2)) then
    Result:= (Flags and FILE_SUPPORTS_REPARSE_POINTS) <> 0;
end;

end.
