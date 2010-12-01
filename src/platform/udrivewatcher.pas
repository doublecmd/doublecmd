{
   Double Commander
   -------------------------------------------------------------------------
   Enumerating and monitoring drives in the system.

   Copyright (C) 2006-2010  Koblov Alexander (Alexx2000@mail.ru)
   Copyright (C) 2010  Przemyslaw Nagay (cobines@gmail.com)

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

unit uDriveWatcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LCLType, uDrive;

type
  TDriveWatcherEvent = (dweDriveAdded,
                        dweDriveRemoved,
                        dweDriveChanged);

  TDriveWatcherEventNotify = procedure(EventType: TDriveWatcherEvent;
                                       const ADrive: PDrive) of object;

  TDriveWatcherObserverList = specialize TFPGList<TDriveWatcherEventNotify>;

  TDriveWatcher = class
    class procedure Initialize(Handle: HWND);
    class procedure Finalize;
    class procedure AddObserver(Func: TDriveWatcherEventNotify);
    class procedure RemoveObserver(Func: TDriveWatcherEventNotify);
    class function GetDrivesList: TDrivesList;
  end;

implementation

uses
  {$IFDEF UNIX}
  Unix, uMyUnix, LCLProc
   {$IFDEF LINUX}
   , inotify, uUDisks, uFileSystemWatcher, uDCUtils
   {$ENDIF}
   {$IFDEF DARWIN}
   , MacOSAll
 	 {$ENDIF}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  uMyWindows, Windows, JwaDbt
  {$ENDIF}
  ;

{$IFDEF LINUX}
type
  TFakeClass = class
  public
    procedure OnWatcherNotifyEvent(Sender: TObject; NotifyData: PtrInt);
    procedure OnUDisksNotify(Reason: TUDisksMethod; const ObjectPath: UTF8String);
  end;
{$ENDIF}

var
  FObservers: TDriveWatcherObserverList = nil;
  InitializeCounter: Integer = 0;
  {$IFDEF LINUX}
  EtcDirWatcher: TFileSystemWatcher = nil;
  FakeClass: TFakeClass = nil;
  IsUDisksAvailable: Boolean = False;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  OldWProc: WNDPROC;
  {$ENDIF}

procedure DoDriveAdded(const ADrive: PDrive);
var
  i: Integer;
begin
  for i := 0 to FObservers.Count - 1 do
    FObservers[i](dweDriveAdded, ADrive);
end;

procedure DoDriveRemoved(const ADrive: PDrive);
var
  i: Integer;
begin
  for i := 0 to FObservers.Count - 1 do
    FObservers[i](dweDriveRemoved, ADrive);
end;

procedure DoDriveChanged(const ADrive: PDrive);
var
  i: Integer;
begin
  for i := 0 to FObservers.Count - 1 do
    FObservers[i](dweDriveChanged, ADrive);
end;

{$IFDEF MSWINDOWS}
function MyWndProc(hWnd: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  ADrive: PDrive = nil;
begin
  case uiMsg of
    WM_DEVICECHANGE:
      case wParam of
        DBT_DEVICEARRIVAL:
          DoDriveAdded(ADrive);
        DBT_DEVICEREMOVECOMPLETE:
          DoDriveRemoved(ADrive);
      end;
  end; // case
  Result := CallWindowProc(OldWProc, hWnd, uiMsg, wParam, lParam);
end;

procedure SetMyWndProc(Handle : HWND);
begin
  {$PUSH}{$HINTS OFF}
  OldWProc := WNDPROC(SetWindowLongPtr(Handle, GWL_WNDPROC, LONG_PTR(@MyWndProc)));
  {$POP}
end;
{$ENDIF}

class procedure TDriveWatcher.Initialize(Handle: HWND);
begin
  Inc(InitializeCounter);
  if InitializeCounter > 1 then
    // Already initialized.
    Exit;

  FObservers := TDriveWatcherObserverList.Create;

  {$IFDEF LINUX}
  FakeClass := TFakeClass.Create;

  if uUDisks.Initialize then
  begin
    IsUDisksAvailable := True;
    uUDisks.AddObserver(@FakeClass.OnUDisksNotify);
  end
  else
  begin
    DebugLn('Detecting devices through /etc/mtab.');
    EtcDirWatcher:= TFileSystemWatcher.Create(nil, '/etc', [wfFileNameChange]);
    EtcDirWatcher.OnWatcherNotifyEvent:= @FakeClass.OnWatcherNotifyEvent;
    EtcDirWatcher.Active:= True;
  end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  SetMyWndProc(Handle);
  {$ENDIF}
end;

class procedure TDriveWatcher.Finalize;
begin
  Dec(InitializeCounter);
  if InitializeCounter <> 0 then
    // Don't finalize yet.
    Exit;

  {$IFDEF LINUX}
  if IsUDisksAvailable then
  begin
    uUDisks.RemoveObserver(@FakeClass.OnUDisksNotify);
    uUDisks.Finalize;
    IsUDisksAvailable := False;
  end;
  if Assigned(EtcDirWatcher) then
    FreeAndNil(EtcDirWatcher);
  if Assigned(FakeClass) then
    FreeAndNil(FakeClass);
  {$ENDIF}

  if Assigned(FObservers) then
    FreeAndNil(FObservers);
end;

class procedure TDriveWatcher.AddObserver(Func: TDriveWatcherEventNotify);
begin
  if FObservers.IndexOf(Func) < 0 then
    FObservers.Add(Func);
end;

class procedure TDriveWatcher.RemoveObserver(Func: TDriveWatcherEventNotify);
begin
  FObservers.Remove(Func);
end;

{$IFDEF LINUX}
function BeginsWithString(const patterns: array of string; const strings: array of string): Boolean;
var
  i, j: Integer;
begin
  for i := Low(strings) to High(strings) do
  begin
    for j := Low(patterns) to High(patterns) do
      if StrBegins(strings[i], patterns[j]) then
        Exit(True);
  end;
  Result := False;
end;

function IsPartOfString(const patterns: array of string; const str: string): Boolean;
var
  I: Integer;
begin
  for I := Low(patterns) to High(patterns) do
    if Pos(patterns[I], str) > 0 then
      Exit(True);
  Result := False;
end;

function IsStringInArray(const search: string; strings: array of string): Boolean;
var
  i: Integer;
begin
  for i := Low(strings) to High(strings) do
  begin
    if strings[i] = search then
      Exit(True);
  end;
  Result := False;
end;

function UDisksGetDevices(out Devices, UUIDs: TStringArray): Boolean;
var
  i: Integer;
begin
  Result := uUDisks.EnumerateDevices(Devices);
  if Result then
  begin
    SetLength(UUIDs, Length(Devices));
    for i := 0 to Length(Devices) - 1 do
    begin
      if not GetObjectProperty(Devices[i], 'IdUuid', UUIDs[i]) then
        Exit(False);
    end;
  end;
end;

function UDisksDeviceToDrive(const ObjectPath: UTF8String; out Drive: PDrive): Boolean;
var
  DeviceFile: UTF8String;
  DeviceIsDrive,
  DeviceIsSystemInternal,
  DeviceIsPartition,
  DeviceIsMounted,
  DeviceIsRemovable,   // If contains removable media.
  DeviceIsOpticalDisc, // If is an optical drive and optical disk is inserted.
  DeviceIsMediaAvailable,
  DriveIsMediaEjectable: Boolean;
  DeviceMountPaths: TStringArray;
  DriveConnectionInterface,
  DriveMedia: UTF8String; // Type of media currently in the drive.
  DriveMediaCompatibility: TStringArray; // Possible media types.
  IdUsage,
  IdType,
  IdVersion,
  IdLabel: UTF8String;
begin
  // Description of properties:
  // http://hal.freedesktop.org/docs/udisks/Device.html

  Result :=
    GetObjectProperty(ObjectPath, 'DeviceIsDrive', DeviceIsDrive) and
    GetObjectProperty(ObjectPath, 'DeviceIsSystemInternal', DeviceIsSystemInternal) and
    GetObjectProperty(ObjectPath, 'DeviceIsPartition', DeviceIsPartition) and
    GetObjectProperty(ObjectPath, 'DeviceIsMounted', DeviceIsMounted) and
    GetObjectProperty(ObjectPath, 'DeviceIsRemovable', DeviceIsRemovable) and
    GetObjectProperty(ObjectPath, 'DeviceIsOpticalDisc', DeviceIsOpticalDisc) and
    GetObjectProperty(ObjectPath, 'DeviceIsMediaAvailable', DeviceIsMediaAvailable) and
    GetObjectProperty(ObjectPath, 'DeviceFile', DeviceFile);

  if Result and DeviceIsMounted then
    Result := GetObjectProperty(ObjectPath, 'DeviceMountPaths', DeviceMountPaths);

  if Result and DeviceIsDrive then
  begin
    Result := GetObjectProperty(ObjectPath, 'DriveIsMediaEjectable', DriveIsMediaEjectable) and
              GetObjectProperty(ObjectPath, 'DriveConnectionInterface', DriveConnectionInterface) and
              GetObjectProperty(ObjectPath, 'DriveMedia', DriveMedia) and
              GetObjectProperty(ObjectPath, 'DriveMediaCompatibility', DriveMediaCompatibility);
  end;

  if Result then
  begin
    Result := GetObjectProperty(ObjectPath, 'IdUsage', IdUsage) and
              GetObjectProperty(ObjectPath, 'IdType', IdType) and
              GetObjectProperty(ObjectPath, 'IdVersion', IdVersion) and
              GetObjectProperty(ObjectPath, 'IdLabel', IdLabel);
  end;

  if Result then
  begin
    New(Drive);
    with Drive^ do
    begin
      DeviceId := DeviceFile;
      if DeviceIsMounted and (Length(DeviceMountPaths) > 0) then
      begin
        Path := DeviceMountPaths[0];
        DisplayName := ExtractFileName(Path);
      end
      else
      begin
        Path := EmptyStr;
        DisplayName := ExtractFileName(DeviceFile);
      end;
      DriveLabel := IdLabel;

      if DeviceIsPartition then
        DriveType := dtHardDisk
      else if DeviceIsDrive then
        begin
          if BeginsWithString(['flash'], DriveMediaCompatibility) then
            DriveType := dtFlash
          else if BeginsWithString(['floppy'], DriveMediaCompatibility) then
            DriveType := dtFloppy
          else if BeginsWithString(['optical'], DriveMediaCompatibility) then
            DriveType := dtOptical
          else
            DriveType := dtUnknown;
        end
      else
        DriveType := dtUnknown;

      IsMediaAvailable := DeviceIsMediaAvailable;
      IsMediaEjectable := DeviceIsDrive and DriveIsMediaEjectable;
      IsMounted := DeviceIsMounted;
    end;
  end;
end;
{$ENDIF}

class function TDriveWatcher.GetDrivesList: TDrivesList;
{$IF DEFINED(MSWINDOWS)}
var
  Drive : PDrive;
  DriveNum: Integer;
  DriveBits: set of 0..25;
  WinDriveType: UINT;
  DrivePath: String;
begin
  Result := TDrivesList.Create;
  { fill list }
  DWORD(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
  begin
    if not (DriveNum in DriveBits) then Continue;
    DrivePath := Char(DriveNum + Ord('a')) + ':\';
    WinDriveType := GetDriveType(PChar(DrivePath));
    if WinDriveType = DRIVE_NO_ROOT_DIR then Continue;
    New(Drive);
    Result.Add(Drive);
    with Drive^ do
    begin
      DeviceId := EmptyStr;
      Path := DrivePath;
      DisplayName := Path;
      IsMediaAvailable := True;
      IsMediaEjectable := False;
      IsMounted := True;

      case WinDriveType of
        DRIVE_REMOVABLE:
          begin
            if Path[1] in ['a'..'b'] then
              DriveType := dtFloppy
            else
              DriveType := dtFlash;
            IsMediaEjectable := True;
          end;
        DRIVE_FIXED:
          DriveType := dtHardDisk;
        DRIVE_REMOTE:
          DriveType := dtNetwork;
        DRIVE_CDROM:
          begin
            DriveType := dtOptical;
            IsMediaEjectable := True;
          end;
        DRIVE_RAMDISK:
          DriveType := dtRamDisk;
        else
          DriveType := dtUnknown;
      end;

      if IsMediaAvailable then
      begin
        case DriveType of
          dtFloppy:
            DriveLabel := EmptyStr;
          dtNetwork:
            DriveLabel := mbGetRemoteFileName(Path);
          else
            DriveLabel := mbGetVolumeLabel(Path, True);
        end;
      end
      else
        DriveLabel := EmptyStr;
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
  Result := TDrivesList.Create;
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
                    DriveType:= dtHardDisk;
                    WriteLn(Format('Volume "%s" (vRefNum %d), BSD node /dev/%s',
                        [volNameAsCString, actualVolume, PChar(volumeParms.vMDeviceID)]));
                  end
                else
                  begin
                    DriveType:= dtNetwork;
                    WriteLn(Format('Volume "%s" (vRefNum %d)', [volNameAsCString, actualVolume]));
                  end;
                DeviceId:= EmptyStr;
                DisplayName:= volNameAsCString;
                Path:= '/Volumes/' + volNameAsCString;
                DriveLabel:= volNameAsCString;
                IsMediaAvailable:= True;
                IsMediaEjectable:= False;
                IsMounted:= True;
              end;
              Result.Add(Drive);
              //---------------------------------------------------------------
              end;
          Inc(volumeIndex);
        end;
    end; // while
end;
{$ELSE}
  function CheckMountEntry(MountEntry: PMountEntry): Boolean;
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
         (mnt_type = 'none') or
         (mnt_type = 'cgroup') or
         (mnt_type = 'cpuset') or
         (mnt_type = 'tmpfs') or
         (mnt_type = 'proc') or
         (mnt_type = 'swap') or
         (mnt_type = 'sysfs') or
         (mnt_type = 'debugfs') or
         (mnt_type = 'devtmpfs') or
         (mnt_type = 'devpts') or
         (mnt_type = 'fusectl') or
         (mnt_type = 'securityfs') or
         (mnt_type = 'binfmt_misc') or
         (mnt_type = 'fuse.gvfs-fuse-daemon') or
         (mnt_type = 'fuse.truecrypt') or
         (mnt_type = 'nfsd') or
         (mnt_type = 'usbfs') or
         (mnt_type = 'rpc_pipefs') then Exit;
    end;
    Result:= True;
  end;
  function UDisksGetDeviceObjectByUUID(const UUID: String; const Devices, UUIDs: TStringArray): String;
  var
    i: Integer;
  begin
    for i := Low(UUIDs) to High(UUIDs) do
      if UUIDs[i] = UUID then
        Exit(Devices[i]);
    Result := EmptyStr;
  end;

var
  AddedDevices: TStringList = nil;
  AddedMountPoints: TStringList = nil;
  HaveUDisksDevices: Boolean = False;

  function CheckDevice(const Device: String): Boolean;
  begin
    // If UDisks is available name=value pair should have been handled,
    // so we are free to check the device name. Otherwise don't check it
    // if it is a known name=value pair.
    Result := HaveUDisksDevices or not StrBegins(Device, 'UUID=');
  end;

  // Checks if device on some mount point hasn't been added yet.
  function CanAddDevice(const Device, MountPoint: String): Boolean;
  var
    Idx: Integer;
  begin
    Idx := AddedMountPoints.IndexOf(MountPoint);
    Result := (Idx < 0) or
              (CheckDevice(Device) and
               CheckDevice(AddedDevices[Idx]) and
               (AddedDevices[Idx] <> Device));
  end;

  function GetDrive(const DrivesList: TDrivesList;
                    const Device, MountPoint: String): PDrive;
  var
    i: Integer;
  begin
    for i := 0 to DrivesList.Count - 1 do
    begin
      // If UDisks not available only check mount points.
      if (DrivesList[i]^.Path = MountPoint) and
         (not CheckDevice(Device) or
          not CheckDevice(DrivesList[i]^.DeviceId) or
          (DrivesList[i]^.DeviceId = Device)) then
        Exit(DrivesList[i]);
    end;
    Result := nil;
  end;

const
  MntEntFileList: array[1..2] of PChar = (_PATH_FSTAB, _PATH_MOUNTED);
var
  Drive : PDrive = nil;
  ExistingDrive : PDrive;
  fstab: PIOFile;
  pme: PMountEntry;
  I: Integer;
  UDisksDevicesList: TStringArray;
  UDisksUUIDsList: TStringArray;
  UDisksDeviceObject: UTF8String;
  DeviceFile: String;
  MountPoint: String;
  HandledByUDisks: Boolean = False;
begin
  Result := TDrivesList.Create;
  try
    AddedDevices := TStringList.Create;
    AddedMountPoints := TStringList.Create;

    if IsUDisksAvailable then
      HaveUDisksDevices := UDisksGetDevices(UDisksDevicesList, UDisksUUIDsList);

    // Storage devices have to be in fstab or mtab and reported by UDisks.
    for I:= Low(MntEntFileList) to High(MntEntFileList) do
    begin
      fstab:= setmntent(MntEntFileList[I],'r');
      if not Assigned(fstab) then Continue;
      pme:= getmntent(fstab);
      while (pme <> nil) do
      begin
        if CheckMountEntry(pme) then
        begin
          DeviceFile := StrPas(pme^.mnt_fsname);
          MountPoint := ExcludeTrailingPathDelimiter(StrPas(pme^.mnt_dir));

          if HaveUDisksDevices then
          begin
            // Handle "/dev/" and "UUID=" through UDisks if available.
            if StrBegins(DeviceFile, 'UUID=') then
            begin
              UDisksDeviceObject := UDisksGetDeviceObjectByUUID(
                  Copy(DeviceFile, 6, MaxInt), UDisksDevicesList, UDisksUUIDsList);
              if UDisksDeviceObject <> EmptyStr then
                DeviceFile := '/dev/' + ExtractFileName(UDisksDeviceObject);
              HandledByUDisks := True;
            end
            else if StrBegins(DeviceFile, '/dev/') then
            begin
              UDisksDeviceObject := UDisksDevicePathPrefix + Copy(DeviceFile, 6, MaxInt);
              HandledByUDisks := True;
            end
            else
              HandledByUDisks := False;

            // Don't add the device if it's not listed by UDisks.
            if HandledByUDisks and
               IsStringInArray(UDisksDeviceObject, UDisksDevicesList) and
               CanAddDevice(DeviceFile, MountPoint) and
               UDisksDeviceToDrive(UDisksDeviceObject, Drive) then
            begin
              // Drive object has been created.
              Drive^.Path := MountPoint;
              Drive^.DisplayName := ExtractFileName(Drive^.Path);
            end;
          end;

          // Add by entry in fstab/mtab.
          if not HandledByUDisks then
          begin
            if CanAddDevice(DeviceFile, MountPoint) then
            begin
              New(Drive);
              with Drive^ do
              begin
                DeviceId := DeviceFile;
                Path := MountPoint;
                DisplayName := ExtractFileName(Path);
                DriveLabel := Path;

                if IsPartOfString(['ISO9660', 'CDROM', 'CDRW', 'DVD'], UpperCase(pme^.mnt_type)) then
                  DriveType := dtOptical else
                if IsPartOfString(['FLOPPY'], UpperCase(pme^.mnt_type)) then
                  DriveType := dtFloppy else
                if IsPartOfString(['ZIP', 'USB', 'CAMERA'], UpperCase(pme^.mnt_type)) then
                  DriveType := dtFlash else
                if IsPartOfString(['NFS', 'SMB', 'NETW', 'CIFS'], UpperCase(pme^.mnt_type)) then
                  DriveType := dtNetwork
                else
                  DriveType := dtHardDisk;

                IsMediaAvailable:= True;
                IsMediaEjectable:= (DriveType = dtOptical);
                IsMounted:= False; // Checked via mtab below.
              end;
            end
            // Mark drive as mounted if found in mtab.
            else if MntEntFileList[I] = _PATH_MOUNTED then
            begin
              ExistingDrive := GetDrive(Result, DeviceFile, MountPoint);
              if Assigned(ExistingDrive) then
                ExistingDrive^.IsMounted := True;
            end;
          end;

          // If drive object has been created add it to the list.
          if Assigned(Drive) then
          begin
            Result.Add(Drive);
            Drive := nil;
            AddedDevices.Add(DeviceFile);
            AddedMountPoints.Add(MountPoint);

            {$IFDEF DEBUG}
            DebugLn('Adding drive "' + DeviceFile + '" with mount point "' + MountPoint + '"');
            {$ENDIF}
          end;
        end;
        pme:= getmntent(fstab);
      end;
      endmntent(fstab);
    end;
  finally
    if Assigned(AddedDevices) then
      AddedDevices.Free;
    if Assigned(AddedMountPoints) then
      AddedMountPoints.Free;
    if Assigned(Drive) then
      Dispose(Drive);
  end;
end;
{$ENDIF}

{$IFDEF LINUX}
procedure TFakeClass.OnWatcherNotifyEvent(Sender: TObject; NotifyData: PtrInt);
var
  ev: pinotify_event absolute NotifyData;
  ADrive: PDrive = nil;
begin
  if (ev^.mask = IN_DELETE) and (Pos('mtab', PChar(@ev^.name)) = 1) then
    DoDriveChanged(ADrive);
end;

procedure TFakeClass.OnUDisksNotify(Reason: TUDisksMethod; const ObjectPath: UTF8String);
var
  ADrive: PDrive = nil;
begin
  UDisksDeviceToDrive(ObjectPath, ADrive);
  try
    case Reason of
      UDisks_DeviceAdded:
        DoDriveAdded(ADrive);
      UDisks_DeviceRemoved:
        DoDriveRemoved(ADrive);
      UDisks_DeviceChanged:
        DoDriveChanged(ADrive);
    end;
  finally
    if Assigned(ADrive) then
      Dispose(ADrive);
  end;
end;
{$ENDIF}

end.

