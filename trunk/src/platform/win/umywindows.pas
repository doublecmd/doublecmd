{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains specific WINDOWS functions.

    Copyright (C) 2006-2019 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uMyWindows;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, SysUtils, JwaWinBase, Windows;

const
  // STORAGE_BUS_TYPE
  BusTypeUnknown = $00;
  BusTypeUsb     = $07;
  BusTypeSd      = $0C;
  BusTypeMmc     = $0D;

procedure ShowWindowEx(hWnd: HWND);
function FindMainWindow(ProcessId: DWORD): HWND;
function GetMenuItemText(hMenu: HMENU; uItem: UINT; fByPosition: LongBool): UnicodeString;
function GetMenuItemType(hMenu: HMENU; uItem: UINT; fByPosition: LongBool): UINT;
function InsertMenuItemEx(hMenu, SubMenu: HMENU; Caption: PWideChar; Position, ItemID,  ItemType : UINT; Bitmap:Graphics.TBitmap = nil): boolean;
function RegReadKey(ARoot: HKEY; const APath, AName: UnicodeString; out AValue: UnicodeString): Boolean;
{en
   Extracts volume GUID from a volume GUID path
}
function ExtractVolumeGUID(const VolumeName: UnicodeString): UnicodeString;
{en
   Retrieves a volume GUID path for the volume that is associated with the specified
   volume mount point (drive letter, volume GUID path, or mounted folder)
   @param(Path The string that contains the path of a mounted folder or a drive letter)
   @returns(Volume GUID path)
}
function GetMountPointVolumeName(const Path: UnicodeString): UnicodeString;
{en
   Checks readiness of a drive
   @param(sDrv  String specifying the root directory of a file system volume)
   @returns(The function returns @true if drive is ready, @false otherwise)
}
function mbDriveReady(const sDrv: String): Boolean;
{en
   Get the label of a file system volume
   @param(sDrv  String specifying the root directory of a file system volume)
   @param(bVolReal @true if it a real file system volume)
   @returns(The function returns volume label)
}
function mbGetVolumeLabel(const sDrv: String; const bVolReal: Boolean): String;
{en
   Set the label of a file system volume
   @param(sRootPathName  String specifying the root directory of a file system volume)
   @param(sVolumeName String specifying a new name for the volume)
   @returns(The function returns @true if successful, @false otherwise)
}
function mbSetVolumeLabel(sRootPathName, sVolumeName: String): Boolean;
{en
   Wait for change disk label
   @param(sDrv  String specifying the root directory of a file system volume)
   @param(sCurLabel Current volume label)
}
procedure mbWaitLabelChange(const sDrv: String; const sCurLabel: String);
{en
   Close CD/DVD drive
   @param(sDrv  String specifying the root directory of a drive)
}
procedure mbCloseCD(const sDrv: String);
function mbDriveBusType(Drive: AnsiChar): UInt32;
{en
   Get physical drive serial number
}
function mbGetDriveSerialNumber(Drive: AnsiChar): String;
procedure mbDriveUnlock(const sDrv: String);
{en
   Get remote file name by local file name
   @param(sLocalName String specifying the local file name)
   @returns(The function returns remote file name)
}
function mbGetRemoteFileName(const sLocalName: String): String;
{en
   Retrieves the short path form of the specified path
   @param(sLongPath The path string)
   @param(sShortPath A string to receive the short form of the path that sLongPath specifies)
   @returns(The function returns @true if successful, @false otherwise)
}
function mbGetShortPathName(const sLongPath: String; var sShortPath: AnsiString): Boolean;
{en
   Retrieves owner of the file (user and group).
   Both user and group contain computer name.
   @param(sPath Absolute path to the file. May be UNC path.)
   @param(sUser Returns user name of the file.)
   @param(sGroup Returns primary group of the file.)
}
function GetFileOwner(const sPath: String; out sUser, sGroup: String): Boolean;
{en
   Retrieves a description of file's type.
   @param(sPath Absolute path to the file.)
}
function GetFileDescription(const sPath: String): String;
{en
   Retrieves file system name of the volume that sRootPath points to.
   @param(sRootPath Root directory of the volume, for example C:\)
}
function mbGetFileSystem(const sRootPath: String): String;
{en
   Retrieves the actual number of bytes of disk storage used to store a specified file.
   @param(FileName The name of the file.)
}
function mbGetCompressedFileSize(const FileName: String): Int64;
{en
   Retrieves the time the file was changed.
}
function mbGetFileChangeTime(const FileName: String; out ChangeTime: TFileTime): Boolean;
{en
   Determines whether a key is up or down at the time the function is called,
   and whether the key was pressed after a previous call to GetAsyncKeyStateEx.
}
function GetAsyncKeyStateEx(vKey: Integer): Boolean;
{en
   This routine returns @true if the caller's
   process is a member of the Administrators local group.
   @returns(The function returns @true if caller has Administrators local group, @false otherwise)
}
function IsUserAdmin: TDuplicates;
{en
   This routine returns @true if the caller's process is running in the remote desktop session
}
function RemoteSession: Boolean;
{en
   Creates windows shortcut file (.lnk)
}
procedure CreateShortcut(const Target, Shortcut: String);
{en
   Extract file attributes from find data record.
   Removes reparse point attribute if a reparse point tag is not a name surrogate.
   @param(FindData Find data record from FindFirstFile/FindNextFile function.)
}
function ExtractFileAttributes(const FindData: TWin32FindDataW): DWORD;

procedure InitErrorMode;

procedure UpdateEnvironment;

procedure FixCommandLineToUTF8;

implementation

uses
  ShellAPI, MMSystem, JwaWinNetWk, JwaWinUser, JwaNative, JwaVista, LazUTF8,
  ActiveX, ShlObj, ComObj, DCWindows, uShlObjAdditional;

var
  Wow64DisableWow64FsRedirection: function(OldValue: PPointer): BOOL; stdcall;
  Wow64RevertWow64FsRedirection: function(OldValue: Pointer): BOOL; stdcall;

type
  PHandleData = ^THandleData;
  THandleData = record
    ProcessId: DWORD;
    WindowHandle: HWND;
end;

function IsMainWindow(Handle: HWND): Boolean;
begin
  Result:= (GetWindow(Handle, GW_OWNER) = 0) and IsWindowVisible(Handle);
end;

function EnumWindowsCallback(Handle: HWND; lParam: LPARAM): BOOL; stdcall;
var
  ProcessId: DWORD = 0;
  Data: PHandleData absolute lParam;
begin
  GetWindowThreadProcessId(Handle, @ProcessId);
  Result:= (Data^.ProcessId <> ProcessId) or (not IsMainWindow(Handle));
  if not Result then Data^.WindowHandle:= Handle;
end;

procedure ShowWindowEx(hWnd: HWND);
var
  Placement: TWindowPlacement;
begin
  ZeroMemory(@Placement, SizeOf(TWindowPlacement));
  Placement.length:= SizeOf(TWindowPlacement);
  GetWindowPlacement(hWnd, Placement);

  case (Placement.showCmd) of
    SW_SHOWMAXIMIZED: ShowWindow(hWnd, SW_SHOWMAXIMIZED);
    SW_SHOWMINIMIZED: ShowWindow(hWnd, SW_RESTORE);
    else ShowWindow(hWnd, SW_NORMAL);
  end;

  SetForegroundWindow(hWnd);
end;

function FindMainWindow(ProcessId: DWORD): HWND;
var
  Data: THandleData;
begin
  Data.WindowHandle:= 0;
  Data.ProcessId:= ProcessId;
  EnumWindows(@EnumWindowsCallback, {%H-}LPARAM(@Data));
  Result:= Data.WindowHandle;
end;

function GetMenuItemText(hMenu: HMENU; uItem: UINT; fByPosition: LongBool): UnicodeString;
var
  miiw: TMenuItemInfoW;
  wca: array[0..Pred(MAX_PATH)] of WideChar;
begin
  Result:= EmptyWideStr;
  FillChar(miiw, SizeOf(TMenuItemInfoW), 0);
  with miiw do
  begin
    cbSize:= SizeOf(TMenuItemInfoW);
    fMask:= MIIM_FTYPE or MIIM_STRING;
    dwTypeData:= @wca[0];
    cch:= MAX_PATH;
  end;
  if GetMenuItemInfoW(hMenu, uItem, fByPosition, miiw) then
  begin
    Result:= miiw.dwTypeData;
  end;
end;

function GetMenuItemType(hMenu: HMENU; uItem: UINT; fByPosition: LongBool): UINT;
var
  miiw: TMenuItemInfoW;
begin
  Result:= 0;
  FillChar(miiw, SizeOf(TMenuItemInfoW), 0);
  with miiw do
  begin
    cbSize:= SizeOf(TMenuItemInfoW);
    fMask:= MIIM_FTYPE;
  end;
  if GetMenuItemInfoW(hMenu, uItem, fByPosition, miiw) then
  begin
    Result:= miiw.fType;
  end;
end;

function InsertMenuItemEx(hMenu, SubMenu: HMENU; Caption: PWideChar;
                         Position, ItemID,  ItemType : UINT; Bitmap:Graphics.TBitmap): boolean;
var
  mi: TMenuItemInfoW;
begin
  FillChar(mi, SizeOf(mi), 0);
   with mi do
   begin
      cbSize := SizeOf(mi);

      case ItemType of
        MFT_SEPARATOR:
          begin
            fMask := MIIM_STATE or MIIM_TYPE or MIIM_ID;
          end;
        MFT_STRING:
          begin
            fMask := MIIM_BITMAP or MIIM_STRING or MIIM_SUBMENU or MIIM_ID;
            if BitMap<>nil then hbmpItem:=Bitmap.Handle;
          end;
      end;

      fType := ItemType;
      fState := MFS_ENABLED;
      wID := ItemID;
      hSubMenu := SubMenu;
      dwItemData := 0;
      dwTypeData := Caption;
      cch := SizeOf(Caption);
   end;
   Result := InsertMenuItemW(hMenu, Position, True, mi);
end;

function RegReadKey(ARoot: HKEY; const APath, AName: UnicodeString; out AValue: UnicodeString): Boolean;
var
  AKey: HKEY = 0;
  dwSize: DWORD = MaxSmallint;
begin
  Result:= RegOpenKeyExW(ARoot, PWideChar(APath), 0, KEY_READ, AKey) = ERROR_SUCCESS;
  if Result then
  begin
    SetLength(AValue, MaxSmallint);
    Result:= RegQueryValueExW(AKey, PWideChar(AName), nil, nil, PByte(AValue), @dwSize) = ERROR_SUCCESS;
    if Result then
    begin
      dwSize:= dwSize div SizeOf(WideChar);
      if (dwSize > 0) and (AValue[dwSize] = #0) then Dec(dwSize);
      SetLength(AValue, dwSize);
    end;
    RegCloseKey(AKey);
  end;
end;

function DisplayName(const wsDrv: WideString): WideString;
var
  SFI: TSHFileInfoW;
begin
  FillChar(SFI, SizeOf(SFI), 0);
  SHGetFileInfoW(PWChar(wsDrv), 0, SFI, SizeOf(SFI), SHGFI_DISPLAYNAME);
  Result:= SFI.szDisplayName;

  if Pos('(', Result) <> 0 then
    SetLength(Result, Pos('(', Result) - 2);
end;

function ExtractVolumeGUID(const VolumeName: UnicodeString): UnicodeString;
var
  I, J: LongInt;
begin
  I:= Pos('{', VolumeName);
  J:= Pos('}', VolumeName);
  if (I = 0) or (J = 0) then Exit(EmptyWideStr);
  Result:= Copy(VolumeName, I, J - I + 1);
end;

function GetMountPointVolumeName(const Path: UnicodeString): UnicodeString;
const
  MAX_VOLUME_NAME = 50;
var
  wsPath: UnicodeString;
  wsVolumeName: array[0..Pred(MAX_VOLUME_NAME)] of WideChar;
begin
  FillByte(wsVolumeName, MAX_VOLUME_NAME, 0);
  wsPath:= IncludeTrailingPathDelimiter(Path);
  if not GetVolumeNameForVolumeMountPointW(PWideChar(wsPath), wsVolumeName, MAX_VOLUME_NAME) then
    Result:= EmptyWideStr
  else
    Result:= UnicodeString(wsVolumeName);
end;

(* Drive ready *)

function mbDriveReady(const sDrv: String): Boolean;
var
  NotUsed: DWORD;
  wsDrv: WideString;
begin
  wsDrv:= UTF8Decode(sDrv);
  Result:= GetVolumeInformationW(PWChar(wsDrv), nil, 0, nil, NotUsed, NotUsed, nil, 0);
end;

(* Disk label *)

function mbGetVolumeLabel(const sDrv: String; const bVolReal: Boolean): String;
var
  WinVer: Byte;
  DriveType, NotUsed: DWORD;
  Buf: array [0..MAX_PATH - 1] of WideChar;
  wsDrv,
  wsResult: UnicodeString;
begin
  Result:= '';
  wsDrv:= UTF8Decode(sDrv);
  WinVer:= LOBYTE(LOWORD(GetVersion));
  DriveType:= GetDriveTypeW(PWChar(wsDrv));

  if (WinVer <= 4) and (DriveType <> DRIVE_REMOVABLE) or bVolReal then
    begin // Win9x, Me, NT <= 4.0
      Buf[0]:= #0;
      GetVolumeInformationW(PWChar(wsDrv), Buf, DWORD(SizeOf(Buf)), nil,
                            NotUsed, NotUsed, nil, 0);
      wsResult:= Buf;

      if bVolReal and (WinVer >= 5) and (Result <> '') and
         (DriveType <> DRIVE_REMOVABLE) then // Win2k, XP and higher
        wsResult:= DisplayName(wsDrv)
      else if (Result = '') and (not bVolReal) then
        wsResult:= '<none>';
    end
  else
    wsResult:= DisplayName(wsDrv);
  Result:= UTF16ToUTF8(wsResult);
end;

(* Wait for change disk label *)

function mbSetVolumeLabel(sRootPathName, sVolumeName: String): Boolean;
var
  wsRootPathName,
  wsVolumeName: UnicodeString;
begin
  wsRootPathName:= UTF8Decode(sRootPathName);
  wsVolumeName:= UTF8Decode(sVolumeName);
  Result:= SetVolumeLabelW(PWChar(wsRootPathName), PWChar(wsVolumeName));
end;

procedure mbWaitLabelChange(const sDrv: String; const sCurLabel: String);
var
  st1, st2: String;
begin
  if mbGetVolumeLabel(sDrv, True) = '' then
    Exit;
  st1:= TrimLeft(sCurLabel);
  st2:= st1;
  while st1 = st2 do
    st2:= mbGetVolumeLabel(sDrv, FALSE);
end;

(* Close CD/DVD *)

procedure mbCloseCD(const sDrv: String);
var
  OpenParms: MCI_OPEN_PARMSA;
begin
  FillChar(OpenParms, SizeOf(OpenParms), 0);
  OpenParms.lpstrDeviceType:= 'CDAudio';
  OpenParms.lpstrElementName:= PAnsiChar(ExtractFileDrive(sDrv));
  mciSendCommandA(0, MCI_OPEN, MCI_OPEN_TYPE or MCI_OPEN_ELEMENT, DWORD_PTR(@OpenParms));
  mciSendCommandA(OpenParms.wDeviceID, MCI_SET, MCI_SET_DOOR_CLOSED, 0);
  mciSendCommandA(OpenParms.wDeviceID, MCI_CLOSE, MCI_OPEN_TYPE or MCI_OPEN_ELEMENT, DWORD_PTR(@OpenParms));
end;

const
  IOCTL_STORAGE_QUERY_PROPERTY = $2D1400;

type
  STORAGE_PROPERTY_QUERY = record
    PropertyId: DWORD;
    QueryType: DWORD;
    AdditionalParameters: array[0..0] of Byte;
  end;

  STORAGE_DEVICE_DESCRIPTOR = record
    Version: DWORD;
    Size: DWORD;
    DeviceType: Byte;
    DeviceTypeModifier: Byte;
    RemovableMedia: Boolean;
    CommandQueueing: Boolean;
    VendorIdOffset: DWORD;
    ProductIdOffset: DWORD;
    ProductRevisionOffset: DWORD;
    SerialNumberOffset: DWORD;
    BusType: DWORD;
    RawPropertiesLength: DWORD;
    RawDeviceProperties: array[0..0] of Byte;
  end;

function mbDriveBusType(Drive: AnsiChar): UInt32;
var
  Dummy: DWORD;
  Handle: THandle;
  Query: STORAGE_PROPERTY_QUERY;
  Descr: STORAGE_DEVICE_DESCRIPTOR;
  VolumePath: UnicodeString = '\\.\X:';
begin
  Result := BusTypeUnknown;
  VolumePath[5] := WideChar(Drive);

  Handle := CreateFileW(PWideChar(VolumePath), 0,
    FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, 0, 0);

  if (Handle <> INVALID_HANDLE_VALUE) then
  begin
    ZeroMemory(@Query, SizeOf(STORAGE_PROPERTY_QUERY));

    if (DeviceIoControl(Handle, IOCTL_STORAGE_QUERY_PROPERTY,
                        @Query, SizeOf(STORAGE_PROPERTY_QUERY),
                        @Descr, SizeOf(STORAGE_DEVICE_DESCRIPTOR),
                        @Dummy, nil)) then
    begin
      Result := Descr.BusType;
    end;

    CloseHandle(Handle);
  end;
end;

function mbGetDriveSerialNumber(Drive: AnsiChar): String;
var
  Handle: THandle;
  dwBytesReturned: DWORD;
  Query: STORAGE_PROPERTY_QUERY;
  ABuffer: array[0..4095] of Byte;
  VolumePath: UnicodeString = '\\.\X:';
  Descr: STORAGE_DEVICE_DESCRIPTOR absolute ABuffer;
begin
  Result:= EmptyStr;
  VolumePath[5] := WideChar(Drive);
  Handle:= CreateFileW(PWideChar(VolumePath), 0,
                      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

  if Handle <> INVALID_HANDLE_VALUE then
  begin
    ZeroMemory(@ABuffer[0], SizeOf(ABuffer));
    ZeroMemory(@Query, SizeOf(STORAGE_PROPERTY_QUERY));

    if DeviceIoControl(Handle, IOCTL_STORAGE_QUERY_PROPERTY,
                       @Query, SizeOf(STORAGE_PROPERTY_QUERY),
                       @ABuffer[0], SizeOf(ABuffer), @dwBytesReturned, nil) then
    begin
      if (Descr.SerialNumberOffset > 0) then
        Result := StrPas(PAnsiChar(@ABuffer[0] + Descr.SerialNumberOffset));
    end;

    CloseHandle(Handle);
  end;
end;

function IsWow64: BOOL;
const
  Wow64Process: TDuplicates = dupIgnore;
var
  IsWow64Process: function(hProcess: HANDLE; Wow64Process: PBOOL): BOOL; stdcall;
begin
  if (Wow64Process = dupIgnore) then
  begin
    Result:= False;
    Pointer(IsWow64Process):= GetProcAddress(GetModuleHandle(Kernel32), 'IsWow64Process');
    if (IsWow64Process <> nil) then IsWow64Process(GetCurrentProcess, @Result);
    if Result then Wow64Process:= dupAccept else Wow64Process:= dupError;
  end;
  Result:= (Wow64Process = dupAccept);
end;

function Wow64DisableRedirection(OldValue: PPointer): BOOL;
begin
  if (IsWow64 = False) then
    Result:= True
  else begin
    Result:= Wow64DisableWow64FsRedirection(OldValue);
  end;
end;

function Wow64RevertRedirection(OldValue: Pointer): BOOL;
begin
  if (IsWow64 = False) then
    Result:= True
  else begin
    Result:= Wow64RevertWow64FsRedirection(OldValue);
  end;
end;

procedure ShellExecuteThread(Parameter : Pointer);
var
  Result: DWORD = 0;
  OldValue: Pointer = nil;
  Status : BOOL absolute Result;
  lpExecInfo: LPShellExecuteInfoW absolute Parameter;
begin
  if Wow64DisableRedirection(@OldValue) then
  begin
    CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
    Status:= ShellExecuteExW(lpExecInfo);
    CoUninitialize();
    Wow64RevertRedirection(OldValue);
  end;
  EndThread(Result);
end;

procedure mbDriveUnlock(const sDrv: String);
const
  FVE_E_LOCKED_VOLUME = HRESULT($80310000);
  FVE_E_VOLUME_NOT_BOUND = HRESULT($80310017);
var
  Msg: TMSG;
  LastError: HRESULT;
  ShellThread: TThread;
  wsDrive: UnicodeString;
  lpExecInfo: TShellExecuteInfoW;
begin
  wsDrive:= UTF8Decode(sDrv);
  if not GetDiskFreeSpaceExW(PWideChar(wsDrive), nil, nil, nil) then
  begin
    LastError:= GetLastError;
    if (LastError = FVE_E_LOCKED_VOLUME) or (LastError = FVE_E_VOLUME_NOT_BOUND) then
    begin
      ZeroMemory(@lpExecInfo, SizeOf(lpExecInfo));
      lpExecInfo.cbSize:= SizeOf(lpExecInfo);
      lpExecInfo.fMask:= SEE_MASK_NOCLOSEPROCESS;
      lpExecInfo.lpFile:= PWideChar(wsDrive);
      lpExecInfo.lpVerb:= 'unlock-bde';
      ShellThread:= TThread.ExecuteInThread(@ShellExecuteThread, @lpExecInfo);
      if (ShellThread.WaitFor <> 0) and (lpExecInfo.hProcess <> 0) then
      begin
        while (WaitForSingleObject(lpExecInfo.hProcess, 100) = WAIT_TIMEOUT) do
        begin
          if (GetAsyncKeyStateEx(VK_ESCAPE)) then
          begin
            TerminateProcess(lpExecInfo.hProcess, 1);
            Break;
          end;
          PeekMessageW({%H-}Msg, 0, 0, 0, PM_REMOVE);
        end;
        {
        if GetExitCodeProcess(lpExecInfo.hProcess, @LastError) then
          Result:= (LastError = 0);
        }
        CloseHandle(lpExecInfo.hProcess);
      end;
    end;
  end;
end;

function mbGetRemoteFileName(const sLocalName: String): String;
var
  dwResult,
  lpBufferSize: DWORD;
  wsLocalName: UnicodeString;
  lpBuffer: PUniversalNameInfoW;
begin
  Result:= sLocalName;
  wsLocalName:= UTF8Decode(sLocalName);
  lpBufferSize:= SizeOf(TUniversalNameInfoW);
  GetMem(lpBuffer, lpBufferSize);
  try
    dwResult:= WNetGetUniversalNameW(PWideChar(wsLocalName), UNIVERSAL_NAME_INFO_LEVEL, lpBuffer, lpBufferSize);
    if dwResult = ERROR_MORE_DATA then
      begin
        lpBuffer:= ReallocMem(lpBuffer, lpBufferSize);
        dwResult:= WNetGetUniversalNameW(PWideChar(wsLocalName), UNIVERSAL_NAME_INFO_LEVEL, lpBuffer, lpBufferSize);
      end;
    if dwResult = NO_ERROR then
      Result:= UTF16ToUTF8(UnicodeString(lpBuffer^.lpUniversalName));
  finally
    FreeMem(lpBuffer);
  end;
end;

function mbGetShortPathName(const sLongPath: String; var sShortPath: AnsiString): Boolean;
var
  wsLongPath,
  wsShortPath: UnicodeString;
  cchBuffer: DWORD;
begin
  Result:= False;
  wsLongPath:= UTF16LongName(sLongPath);
  cchBuffer:= GetShortPathNameW(PWideChar(wsLongPath), nil, 0);
  if cchBuffer = 0 then Exit;
  SetLength(wsShortPath, cchBuffer);
  cchBuffer:= GetShortPathNameW(PWideChar(wsLongPath), PWideChar(wsShortPath), cchBuffer);
  if cchBuffer <> 0 then
  begin
    sShortPath:= AnsiString(wsShortPath);
    Result:= True;
  end;
end;

function GetFileOwner(const sPath: String; out sUser, sGroup: String): Boolean;
var
  wsMachineName: UnicodeString;

  function SidToDisplayString(sid: PSID; sidType: SID_NAME_USE): String;
  var
    pName: PWideChar = nil;
    pDomain: PWideChar = nil;
    NameLen: DWORD = 0;
    DomainLen: DWORD = 0;
  begin
    // We're expecting insufficient buffer error here.
    if (LookupAccountSidW(PWideChar(wsMachineName), sid,
                          nil, @NameLen,
                          nil, @DomainLen,
                          @SidType) = False) and
       (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
    begin
      pName := Getmem(NameLen * SizeOf(WideChar));
      pDomain := Getmem(DomainLen * SizeOf(WideChar));

      if Assigned(pName) and Assigned(pDomain) and
         LookupAccountSidW(PWideChar(wsMachineName), sid,
                           pName, @NameLen,
                           pDomain, @DomainLen,
                           @SidType) then
      begin
        if pDomain[0] <> #0 then
          Result := UTF16ToUTF8(UnicodeString(pDomain) + PathDelim + UnicodeString(pName))
        else
          Result := UTF16ToUTF8(UnicodeString(pName));
      end
      else
        Result := EmptyStr;

      Freemem(pName);
      Freemem(pDomain);
    end
    else
      Result := EmptyStr;
  end;

  // From UNC name extracts computer name.
  function GetMachineName(wPathName: LPCWSTR): WideString;
  var
    lpMachineName,
    lpMachineNameNext: PWideChar;
  begin
    lpMachineName := PathFindNextComponentW(wPathName);

    if Assigned(lpMachineName) then
    begin
      lpMachineNameNext := PathFindNextComponentW(lpMachineName);

      if Assigned(lpMachineNameNext) then
        SetString(Result, lpMachineName, lpMachineNameNext - lpMachineName - 1)
      else
        Result := lpMachineName;
    end
    else
      Result := EmptyWideStr;
  end;

var
  wszUNCPathName: array[0..32767] of WideChar;
  wsPathName: WideString;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR = nil;
  pOwnerSid: PSID = nil;
  pUNI: PUniversalNameInfoW;
  bDefault: Boolean;
  dwBufferSize: DWORD = 0;
  dwSizeNeeded: DWORD = 0;
begin
  Result := False;

  if Length(sPath) = 0 then
    Exit;

  try
    wsPathName := UTF8Decode(sPath);

    // Check if the path is to remote share and get remote machine name.

    if PathIsUNCW(PWideChar(wsPathName)) then
    begin
      // Path is in full UNC format.
      wsMachineName := GetMachineName(PWideChar(wsPathName));
    end
    else
    begin
      // Check if local path is mapped to network share.
      dwBufferSize := SizeOf(wszUNCPathName);
      pUNI := PUniversalNameInfoW(@wszUNCPathName[0]);
      if WNetGetUniversalNameW(PWideChar(wsPathName),
           UNIVERSAL_NAME_INFO_LEVEL, pUNI, dwBufferSize) = NO_ERROR then
      begin
        wsMachineName := GetMachineName(pUNI^.lpUniversalName);
      end;
      // else not a network share, no network connection, etc.
    end;

    { Get security descriptor. }

    // We're expecting insufficient buffer error here.
    if (GetFileSecurityW(PWideChar(wsPathName),
                         OWNER_SECURITY_INFORMATION or GROUP_SECURITY_INFORMATION,
                         nil, 0, @dwSizeNeeded) <> False) or
       (GetLastError <> ERROR_INSUFFICIENT_BUFFER) or
       (dwSizeNeeded = 0) then
    begin
      Exit;
    end;

    pSecurityDescriptor := GetMem(dwSizeNeeded);
    if not Assigned(pSecurityDescriptor) then
      Exit;

    if not GetFileSecurityW(PWideChar(wsPathName),
                            OWNER_SECURITY_INFORMATION or GROUP_SECURITY_INFORMATION,
                            pSecurityDescriptor,
                            dwSizeNeeded, @dwSizeNeeded) then
    begin
      Exit;
    end;

    { Get Owner and Group. }

    if GetSecurityDescriptorOwner(pSecurityDescriptor, pOwnerSid, @bDefault) then
      sUser := SidToDisplayString(pOwnerSid, SidTypeUser)
    else
      sUser := EmptyStr;

    if GetSecurityDescriptorGroup(pSecurityDescriptor, pOwnerSid, @bDefault) then
      sGroup := SidToDisplayString(pOwnerSid, SidTypeGroup)
    else
      sGroup := EmptyStr;

    Result := True;

  finally
    if Assigned(pSecurityDescriptor) then
      Freemem(pSecurityDescriptor);
  end;
end;

function GetFileDescription(const sPath: String): String;
var
  SFI: TSHFileInfoW;
begin
  FillChar(SFI, SizeOf(SFI), 0);
  if SHGetFileInfoW(PWideChar(UTF8Decode(sPath)), 0, SFI, SizeOf(SFI), SHGFI_TYPENAME) <> 0 then
    Result := UTF16ToUTF8(UnicodeString(SFI.szTypeName))
  else
    Result := EmptyStr;
end;

function mbGetFileSystem(const sRootPath: String): String;
var
  Buf: array [0..MAX_PATH] of WideChar;
  NotUsed: DWORD = 0;
begin
  // Available since Windows XP.
  if ((Win32MajorVersion > 5) or ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) and
     GetVolumeInformationW(PWideChar(UTF8Decode(sRootPath)), nil, 0, nil,
                           NotUsed, NotUsed, Buf, SizeOf(Buf)) then
  begin
    Result:= UTF16ToUTF8(UnicodeString(Buf));
  end
  else
    Result := EmptyStr;
end;

function mbGetCompressedFileSize(const FileName: String): Int64;
begin
  Int64Rec(Result).Lo:= GetCompressedFileSizeW(PWideChar(UTF16LongName(FileName)), @Int64Rec(Result).Hi);
end;

function mbGetFileChangeTime(const FileName: String; out ChangeTime: TFileTime): Boolean;
var
  Handle: System.THandle;
  IoStatusBlock : TIoStatusBlock;
  FileInformation: TFileBasicInformation;
begin
  Handle:= CreateFileW(PWideChar(UTF16LongName(FileName)), FILE_READ_ATTRIBUTES,
                       FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                       nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if Handle = INVALID_HANDLE_VALUE then Exit(False);
  Result:= NtQueryInformationFile(Handle, @IoStatusBlock, @FileInformation,
                                  SizeOf(FileInformation), FileBasicInformation) = 0;
  CloseHandle(Handle);
  ChangeTime:= TFileTime(FileInformation.ChangeTime);
end;

function GetAsyncKeyStateEx(vKey: Integer): Boolean;
var
  Handle: HWND;
  dwProcessId: DWORD = 0;
begin
  if (GetAsyncKeyState(vKey) < 0) then
  begin
    Handle:= GetForegroundWindow;
    if (Handle <> 0) then
    begin
      GetWindowThreadProcessId(Handle, @dwProcessId);
      Exit(GetCurrentProcessId = dwProcessId);
    end;
  end;
  Result:= False;
end;

function IsUserAdmin: TDuplicates;
var
  Success: Boolean;
  ReturnLength: DWORD = 0;
  TokenHandle: HANDLE = INVALID_HANDLE_VALUE;
  TokenInformation: array [0..1023] of Byte;
  ElevationType: JwaVista.TTokenElevationType absolute TokenInformation;
begin
  if (Win32MajorVersion < 6) then Exit(dupIgnore);
  Success:= OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, TokenHandle);
  if not Success then
  begin
    if GetLastError = ERROR_NO_TOKEN then
      Success:= OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle);
  end;
  if Success then
  begin
    Success:= GetTokenInformation(TokenHandle, Windows.TTokenInformationClass(TokenElevationType),
                                 @TokenInformation, SizeOf(TokenInformation), ReturnLength);
    CloseHandle(TokenHandle);
    if Success then
    begin
      case ElevationType of
        TokenElevationTypeDefault: Result:= dupIgnore;  // The token does not have a linked token. (UAC disabled)
        TokenElevationTypeFull:    Result:= dupAccept;  // The token is an elevated token. (Administrator)
        TokenElevationTypeLimited: Result:= dupError;   // The token is a limited token. (User)
      end;
    end;
  end;
  if not Success then Result:= dupError;
end;

function RemoteSession: Boolean;
const
  GLASS_SESSION_ID = 'GlassSessionId';
  TERMINAL_SERVER_KEY = 'SYSTEM\CurrentControlSet\Control\Terminal Server\';
var
  dwType: DWORD;
  lResult: LONG;
  AKey: HKEY = 0;
  dwGlassSessionId, cbGlassSessionId, dwCurrentSessionId: DWORD;
  ProcessIdToSessionId: function(dwProcessId: DWORD; pSessionId: PDWORD): BOOL; stdcall;
begin
  Result:= False;
  if (GetSystemMetrics(SM_REMOTESESSION) <> 0) then
  begin
    Result:= True;
  end
  else if (Win32MajorVersion > 5) then
  begin
    Pointer(ProcessIdToSessionId):= GetProcAddress(GetModuleHandle(Kernel32), 'ProcessIdToSessionId');
    if Assigned(ProcessIdToSessionId) then
    begin
      lResult:= RegOpenKeyEx(HKEY_LOCAL_MACHINE, TERMINAL_SERVER_KEY, 0, KEY_READ, AKey);
      if (lResult = ERROR_SUCCESS) then
      begin
        cbGlassSessionId:= SizeOf(dwGlassSessionId);
        lResult:= RegQueryValueEx(AKey, GLASS_SESSION_ID, nil, @dwType, @dwGlassSessionId, @cbGlassSessionId);
        if (lResult = ERROR_SUCCESS) then
        begin
          if (ProcessIdToSessionId(GetCurrentProcessId(), @dwCurrentSessionId)) then
          begin
            Result:= (dwCurrentSessionId <> dwGlassSessionId);
          end;
        end;
        RegCloseKey(AKey);
      end;
    end;
  end;
end;

procedure CreateShortcut(const Target, Shortcut: String);
var
  IObject: IUnknown;
  ISLink: IShellLinkW;
  IPFile: IPersistFile;
  LinkName: WideString;
  TargetArguments: WideString;
begin
  TargetArguments:= EmptyWideStr;

  { Creates an instance of IShellLink }
  IObject := CreateComObject(CLSID_ShellLink);
  IPFile := IObject as IPersistFile;
  ISLink := IObject as IShellLinkW;

  OleCheckUTF8(ISLink.SetPath(PWideChar(UTF8Decode(Target))));
  OleCheckUTF8(ISLink.SetArguments(PWideChar(TargetArguments)));
  OleCheckUTF8(ISLink.SetWorkingDirectory(PWideChar(UTF8Decode(ExtractFilePath(Target)))));

  { Get the desktop location }
  LinkName := UTF8Decode(Shortcut);
  if LowerCase(ExtractFileExt(LinkName)) <> '.lnk' then
    LinkName := LinkName + '.lnk';

  { Create the link }
  OleCheckUTF8(IPFile.Save(PWideChar(LinkName), False));
end;

function ExtractFileAttributes(const FindData: TWin32FindDataW): DWORD; inline;
begin
  // If a reparse point tag is not a name surrogate then remove reparse point attribute
  // Fixes bug: http://doublecmd.sourceforge.net/mantisbt/view.php?id=531
  if (FindData.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0) and
     (FindData.dwReserved0 and $20000000 = 0) then
    Result:= FindData.dwFileAttributes - FILE_ATTRIBUTE_REPARSE_POINT
  else
    Result:= FindData.dwFileAttributes;
end;

procedure InitErrorMode;
begin
  SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
end;

procedure UpdateEnvironment;
var
  dwSize: DWORD;
  ASysPath: UnicodeString;
  AUserPath: UnicodeString;
  APath: UnicodeString = '';
begin
  // System environment
  if RegReadKey(HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\Control\Session Manager\Environment', 'Path', ASysPath) then
  begin
    APath := ASysPath;
    if (Length(APath) > 0) and (APath[Length(APath)] <> PathSeparator) then APath += PathSeparator;
  end;
  // User environment
  if RegReadKey(HKEY_CURRENT_USER, 'Environment', 'Path', AUserPath) then
  begin
    APath := APath + AUserPath;
    if (Length(APath) > 0) and (APath[Length(APath)] <> PathSeparator) then APath += PathSeparator;
  end;
  // Update path environment variable
  if Length(APath) > 0 then
  begin
    SetLength(ASysPath, MaxSmallInt + 1);
    dwSize:= ExpandEnvironmentStringsW(PWideChar(APath), PWideChar(ASysPath), MaxSmallInt);
    if (dwSize = 0) or (dwSize > MaxSmallInt) then
      ASysPath:= APath
    else begin
      SetLength(ASysPath, dwSize - 1);
    end;
    SetEnvironmentVariableW('Path', PWideChar(ASysPath));
  end;
end;

procedure FixCommandLineToUTF8;
var
  I, nArgs: Integer;
  sTemp: String;
  szArgList: PPWideChar;
  pwcCommandLine: PWideChar;
  lpFileName: array[0..Pred(MaxSmallInt)] of WideChar;
begin
{$IF DEFINED(FPC_HAS_CPSTRING)}
  if DefaultSystemCodePage = CP_UTF8 then Exit;
{$ENDIF}
  pwcCommandLine:= GetCommandLineW();
  for I:= 0 to lstrlenW(pwcCommandLine) - 1 do
  begin
    if (pwcCommandLine[I] = PathDelim) and (pwcCommandLine[I + 1] = '"') then
    begin
      pwcCommandLine[I]:= '"';
      pwcCommandLine[I + 1]:= #32;
    end;
  end;
  szArgList:= CommandLineToArgvW(pwcCommandLine, @nArgs);
  if Assigned(szArgList) then
  begin
    if (nArgs > argc) then
    begin
      SysReAllocMem(argv, nArgs * SizeOf(Pointer));
      FillChar(argv[argc], (nArgs - argc) * Sizeof(Pointer), #0);
      argc:= nArgs;
    end;
    // Special case for ParamStr(0)
    I:= GetModuleFileNameW(0, lpFileName, MaxSmallInt);
    lpFileName[I]:= #0; // to be safe
    sTemp:= UTF16ToUTF8(UnicodeString(lpFileName));
    SysReAllocMem(argv[0], Length(sTemp) + 1);
    StrPCopy(argv[0], sTemp);
    // Process all other parameters
    for I:= 1 to nArgs - 1 do
    begin
      sTemp:= UTF16ToUTF8(UnicodeString(szArgList[I]));
      SysReAllocMem(argv[I], Length(sTemp) + 1);
      StrPCopy(argv[I], sTemp);
    end;
    LocalFree(HLOCAL(szArgList));
  end;
end;

initialization
  if (IsWow64) then begin
    Pointer(Wow64DisableWow64FsRedirection):= GetProcAddress(GetModuleHandle(Kernel32), 'Wow64DisableWow64FsRedirection');
    Pointer(Wow64RevertWow64FsRedirection):= GetProcAddress(GetModuleHandle(Kernel32), 'Wow64RevertWow64FsRedirection');
  end;

end.

