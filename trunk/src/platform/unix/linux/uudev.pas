{
   Double Commander
   -------------------------------------------------------------------------
   Interface to UDev service via libudev.

   Copyright (C) 2014 Alexander Koblov (alexx2000@mail.ru)

   Based on udisks-1.0.4/src/device.c

   Copyright (C) 2008 David Zeuthen <david@fubar.dk>

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

unit uUDev;

{$mode delphi}
{$assertions on}

interface

uses
  Classes, SysUtils, CTypes, Unix, BaseUnix, uUDisks;

function Initialize: Boolean;
procedure Finalize;

procedure AddObserver(Func: TUDisksDeviceNotify);
procedure RemoveObserver(Func: TUDisksDeviceNotify);

function EnumerateDevices(out DevicesInfos: TUDisksDevicesInfos): Boolean;
function GetDeviceInfo(const ObjectPath: String; out Info: TUDisksDeviceInfo): Boolean; overload;

var
  HasUdev: Boolean = False;

implementation

uses
  DynLibs, DCOSUtils, uDebug, uPollThread;

type

  { TMonitorObject }

  TMonitorObject = class
  private
    FAction: String;
    FDevicePath: String;
  private
    procedure ReceiveDevice;
    procedure Handler(Sender: TObject);
  public
    constructor Create;
  end;

type
  Pudev = ^Tudev;
  Tudev = record end;
  Pudev_device = ^Tudev_device;
  Tudev_device = record end;
  Pudev_monitor = ^Tudev_monitor;
  Tudev_monitor = record end;
  Pudev_enumerate = ^Tudev_enumerate;
  Tudev_enumerate = record end;
  Pudev_list_entry = ^Tudev_list_entry;
  Tudev_list_entry = record end;

var
  // udev — libudev context
  udev_new: function(): Pudev; cdecl;
  udev_unref: function(udev: Pudev): Pudev; cdecl;
  // udev_list — list operation
  udev_list_entry_get_next: function(list_entry: Pudev_list_entry): Pudev_list_entry; cdecl;
  udev_list_entry_get_name: function(list_entry: Pudev_list_entry): PAnsiChar; cdecl;
  // udev_device — kernel sys devices
  udev_device_unref: procedure(udev_device: Pudev_device); cdecl;
  udev_device_new_from_syspath: function(udev: Pudev; const syspath: PAnsiChar): Pudev_device;  cdecl;
  udev_device_get_devnode: function(udev_device: Pudev_device): PAnsiChar; cdecl;
  udev_device_get_devtype: function(udev_device: Pudev_device): PAnsiChar; cdecl;
  udev_device_get_syspath: function(udev_device: Pudev_device): PAnsiChar; cdecl;
  udev_device_get_action: function(udev_device: Pudev_device): PAnsiChar; cdecl;
  udev_device_get_property_value: function(udev_device: Pudev_device;
                                           const key: PAnsiChar): PAnsiChar; cdecl;
  udev_device_get_sysattr_value: function(udev_device: Pudev_device;
                                          const sysattr: PAnsiChar): PAnsiChar; cdecl;
  // udev_monitor — device event source
  udev_monitor_unref: procedure(udev_monitor: Pudev_monitor); cdecl;
  udev_monitor_new_from_netlink: function(udev: Pudev; const name: PAnsiChar): Pudev_monitor; cdecl;

  udev_monitor_filter_add_match_subsystem_devtype: function(udev_monitor: Pudev_monitor;
                                                            const subsystem: PAnsiChar;
                                                            const devtype: PAnsiChar): cint; cdecl;
  udev_monitor_enable_receiving: function(udev_monitor: Pudev_monitor): cint; cdecl;
  udev_monitor_get_fd: function(udev_monitor: Pudev_monitor): cint; cdecl;
  udev_monitor_receive_device: function(udev_monitor: Pudev_monitor): Pudev_device; cdecl;
  // udev_enumerate — lookup and sort sys devices
  udev_enumerate_new: function(udev: Pudev): Pudev_enumerate; cdecl;
  udev_enumerate_unref: function(udev_enumerate: Pudev_enumerate): Pudev_enumerate; cdecl;
  udev_enumerate_add_match_subsystem: function(udev_enumerate: Pudev_enumerate;
                                               const subsystem: PAnsiChar): cint; cdecl;
  udev_enumerate_scan_devices: function(udev_enumerate: Pudev_enumerate): cint; cdecl;
  udev_enumerate_get_list_entry: function(udev_enumerate: Pudev_enumerate): Pudev_list_entry; cdecl;

const
  LibraryName = 'libudev.so.%d';

var
  udev: Pudev = nil;
  libudev: TLibHandle = NilHandle;
  udev_monitor: Pudev_monitor = nil;
  udev_monitor_object: TMonitorObject = nil;
  observers: TUDisksObserverList = nil;

const
  UDEV_DEVICE_TYPE_DISK = 'disk';
  UDEV_DEVICE_TYPE_PARTITION = 'partition';

  drive_media_mapping: array [0..29, 0..1] of String =
    (
      ( 'ID_DRIVE_FLASH', 'flash' ),
      ( 'ID_DRIVE_FLASH_CF', 'flash_cf' ),
      ( 'ID_DRIVE_FLASH_MS', 'flash_ms' ),
      ( 'ID_DRIVE_FLASH_SM', 'flash_sm' ),
      ( 'ID_DRIVE_FLASH_SD', 'flash_sd' ),
      ( 'ID_DRIVE_FLASH_SDHC', 'flash_sdhc' ),
      ( 'ID_DRIVE_FLASH_MMC', 'flash_mmc' ),
      ( 'ID_DRIVE_FLOPPY', 'floppy' ),
      ( 'ID_DRIVE_FLOPPY_ZIP', 'floppy_zip' ),
      ( 'ID_DRIVE_FLOPPY_JAZ', 'floppy_jaz' ),
      ( 'ID_CDROM', 'optical_cd' ),
      ( 'ID_CDROM_CD_R', 'optical_cd_r' ),
      ( 'ID_CDROM_CD_RW', 'optical_cd_rw' ),
      ( 'ID_CDROM_DVD', 'optical_dvd' ),
      ( 'ID_CDROM_DVD_R', 'optical_dvd_r' ),
      ( 'ID_CDROM_DVD_RW', 'optical_dvd_rw' ),
      ( 'ID_CDROM_DVD_RAM', 'optical_dvd_ram' ),
      ( 'ID_CDROM_DVD_PLUS_R', 'optical_dvd_plus_r' ),
      ( 'ID_CDROM_DVD_PLUS_RW', 'optical_dvd_plus_rw' ),
      ( 'ID_CDROM_DVD_PLUS_R_DL', 'optical_dvd_plus_r_dl' ),
      ( 'ID_CDROM_DVD_PLUS_RW_DL', 'optical_dvd_plus_rw_dl' ),
      ( 'ID_CDROM_BD', 'optical_bd' ),
      ( 'ID_CDROM_BD_R', 'optical_bd_r' ),
      ( 'ID_CDROM_BD_RE', 'optical_bd_re' ),
      ( 'ID_CDROM_HDDVD', 'optical_hddvd' ),
      ( 'ID_CDROM_HDDVD_R', 'optical_hddvd_r' ),
      ( 'ID_CDROM_HDDVD_RW', 'optical_hddvd_rw' ),
      ( 'ID_CDROM_MO', 'optical_mo' ),
      ( 'ID_CDROM_MRW', 'optical_mrw' ),
      ( 'ID_CDROM_MRW_W', 'optical_mrw_w' )
    );

procedure Print(const sMessage: String);
begin
  WriteLn('UDev: ', sMessage);
end;

procedure Load;
var
  Version: Integer;
begin
  for Version:= 1 downto 0 do
  begin
    libudev:= LoadLibrary(Format(LibraryName, [Version]));
    if libudev <> NilHandle then Break;
  end;
  HasUdev:= libudev <> NilHandle;
  if HasUdev then
  try
    // udev — libudev context
    udev_new:= SafeGetProcAddress(libudev, 'udev_new');
    udev_unref:= SafeGetProcAddress(libudev, 'udev_unref');
    // udev_list — list operation
    udev_list_entry_get_next:= SafeGetProcAddress(libudev, 'udev_list_entry_get_next');
    udev_list_entry_get_name:= SafeGetProcAddress(libudev, 'udev_list_entry_get_name');
    // udev_device — kernel sys devices
    udev_device_unref:= SafeGetProcAddress(libudev, 'udev_device_unref');
    udev_device_new_from_syspath:= SafeGetProcAddress(libudev, 'udev_device_new_from_syspath');
    udev_device_get_devnode:= SafeGetProcAddress(libudev, 'udev_device_get_devnode');
    udev_device_get_devtype:= SafeGetProcAddress(libudev, 'udev_device_get_devtype');
    udev_device_get_syspath:= SafeGetProcAddress(libudev, 'udev_device_get_syspath');
    udev_device_get_action:= SafeGetProcAddress(libudev, 'udev_device_get_action');
    udev_device_get_property_value:= SafeGetProcAddress(libudev, 'udev_device_get_property_value');
    udev_device_get_sysattr_value:= SafeGetProcAddress(libudev, 'udev_device_get_sysattr_value');
    // udev_monitor — device event source
    udev_monitor_unref:= SafeGetProcAddress(libudev, 'udev_monitor_unref');
    udev_monitor_new_from_netlink:= SafeGetProcAddress(libudev, 'udev_monitor_new_from_netlink');

    udev_monitor_filter_add_match_subsystem_devtype:= SafeGetProcAddress(libudev, 'udev_monitor_filter_add_match_subsystem_devtype');
    udev_monitor_enable_receiving:= SafeGetProcAddress(libudev, 'udev_monitor_enable_receiving');

    udev_monitor_get_fd:= SafeGetProcAddress(libudev, 'udev_monitor_get_fd');
    udev_monitor_receive_device:= SafeGetProcAddress(libudev, 'udev_monitor_receive_device');
    // udev_enumerate — lookup and sort sys devices
    udev_enumerate_new:= SafeGetProcAddress(libudev, 'udev_enumerate_new');
    udev_enumerate_unref:= SafeGetProcAddress(libudev, 'udev_enumerate_unref');
    udev_enumerate_add_match_subsystem:= SafeGetProcAddress(libudev, 'udev_enumerate_add_match_subsystem');
    udev_enumerate_scan_devices:= SafeGetProcAddress(libudev, 'udev_enumerate_scan_devices');
    udev_enumerate_get_list_entry:= SafeGetProcAddress(libudev, 'udev_enumerate_get_list_entry');

    // Create the udev object
    udev:= udev_new();
    if udev = nil then Raise Exception.Create('Can''t create udev');
  except
    on E: Exception do
    begin
      HasUdev:= False;
      UnloadLibrary(libudev);
      Print(E.Message);
    end;
  end;
end;

procedure Free;
begin
  if Assigned(udev) then udev_unref(udev);
  if libudev <> NilHandle then UnloadLibrary(libudev);
end;

function GetDeviceProperty(const Device: Pudev_device;
                           const PropertyName: String;
                           out Value: Boolean): Boolean; overload;
var
  pacValue: PAnsiChar;
begin
  pacValue:= udev_device_get_property_value(Device, PAnsiCHar(PropertyName));
  Result:= Assigned(pacValue);
  if (Result = False) then
    Value:= False
  else
    Value:= StrToBool(pacValue);
end;

function GetDeviceProperty(const Device: Pudev_device;
                           const PropertyName: String;
                           out Value: String): Boolean; overload;
var
  pacValue: PAnsiChar;
begin
  pacValue:= udev_device_get_property_value(Device, PAnsiCHar(PropertyName));
  Result:= Assigned(pacValue);
  if (Result = False) then
    Value:= EmptyStr
  else
    Value:= StrPas(pacValue);
end;

function GetDeviceAttribute(const Device: Pudev_device;
                            const AttributeName: String;
                            out Value: String): Boolean; overload;
var
  pacValue: PAnsiChar;
begin
  pacValue:= udev_device_get_sysattr_value(Device, PAnsiCHar(AttributeName));
  Result:= Assigned(pacValue);
  if (Result = False) then
    Value:= EmptyStr
  else
    Value:= StrPas(pacValue);
end;

function GetDeviceAttribute(const Device: Pudev_device;
                            const AttributeName: String;
                            out Value: Boolean): Boolean; overload;
var
  S: String;
begin
  Result:= GetDeviceAttribute(Device, AttributeName, S);
  if Result then Result:= TryStrToBool(S, Value);
end;

function GetDeviceAttribute(const SystemPath: String;
                            const AttributeName: String;
                            out Value: Boolean): Boolean; overload;
var
  S: AnsiChar;
  Handle: THandle;
  FileName: String;
begin
  FileName:= IncludeTrailingBackslash(SystemPath) + AttributeName;
  Handle:= mbFileOpen(FileName, fmOpenRead or fmShareDenyNone);
  Result:= Handle <> feInvalidHandle;
  if Result then
  begin
    Result:= FileRead(Handle, S, SizeOf(S)) > 0;
    if Result then Result:= TryStrToBool(S, Value);
    FileClose(Handle);
  end;
end;

function DecodeString(const EncodedString: String): String;
var
  Finish: Integer;
  Index: Integer = 1;
  StartIndex: Integer = 1;
begin
  Result:= EmptyStr;
  Finish:= Length(EncodedString);

  while Index <= Finish - 3 do
  begin
    if EncodedString[Index] <> '\' then
      Inc(Index)
    else begin
      if EncodedString[Index + 1] <> 'x' then
      begin
        Print('**** NOTE: malformed encoded string: ' + EncodedString);
        Exit(EncodedString);
      end;
      Result:= Result + Copy(EncodedString, StartIndex, Index - StartIndex)
                      + Chr(StrToInt('$' + Copy(EncodedString, Index + 2, 2)));
      Index:= Index + 4;
      StartIndex:= Index;
    end;
  end;

  Result:= Result + Copy(EncodedString, StartIndex, Finish - StartIndex + 1);
end;

procedure UpdateDriveConnectionInterface(SystemPath: PAnsiChar; var Info: TUDisksDeviceInfo);
var
  Path,
  Connection: String;
begin
  Path:= IncludeTrailingPathDelimiter(SystemPath);
  repeat
    Connection:= fpReadLink(Path + 'subsystem');
    Connection:= ExtractFileName(ExcludeTrailingPathDelimiter(Connection));
    if Connection = 'usb' then
    begin
      // Both the interface and the device will be 'usb'.
      // However only the device will have the 'speed' property.
      if mbFileExists(Path + 'speed') then
      begin
        Info.DriveConnectionInterface:= Connection;
        Break;
      end;
    end;
    Path:= ExtractFilePath(ExcludeTrailingPathDelimiter(Path));
  until (Length(Path) = 0) or (CompareStr(Path, '/sys/devices/') = 0);
end;

procedure GetDeviceInfo(SystemPath: PAnsiChar; Device: Pudev_device; out Info: TUDisksDeviceInfo); overload;
var
  I: Integer;
  Value: String;
begin
  with Info do
  begin
    DeviceFile:= udev_device_get_devnode(Device);

    DeviceObjectPath:= SystemPath;

    GetDeviceProperty(Device, 'ID_BUS', DriveConnectionInterface);

    GetDeviceProperty(Device, 'ID_FS_USAGE', IdUsage);
    GetDeviceProperty(Device, 'ID_FS_TYPE', IdType);
    GetDeviceProperty(Device, 'ID_FS_VERSION', IdVersion);
    GetDeviceProperty(Device, 'ID_FS_UUID', IdUuid);

    GetDeviceProperty(Device, 'ID_FS_LABEL_ENC', IdLabel);
    if Length(IdLabel) > 0 then
      IdLabel:= DecodeString(IdLabel)
    else
      GetDeviceProperty(Device, 'ID_FS_LABEL', IdLabel);

    if not GetDeviceProperty(Device, 'ID_DRIVE_EJECTABLE', DriveIsMediaEjectable) then
    begin
      DriveIsMediaEjectable:= FALSE;
      DriveIsMediaEjectable:= DriveIsMediaEjectable or (udev_device_get_property_value(Device, 'ID_CDROM' ) <> nil);
      DriveIsMediaEjectable:= DriveIsMediaEjectable or (udev_device_get_property_value(Device, 'ID_DRIVE_FLOPPY_ZIP' ) <> nil);
      DriveIsMediaEjectable:= DriveIsMediaEjectable or (udev_device_get_property_value(Device, 'ID_DRIVE_FLOPPY_JAZ' ) <> nil);
    end;

    GetDeviceProperty(Device, 'UDISKS_SYSTEM_INTERNAL', DeviceIsSystemInternal);
    GetDeviceProperty(Device, 'UDISKS_AUTOMOUNT_HINT', DeviceAutomountHint);

    if not GetDeviceProperty(Device, 'UDISKS_IGNORE', DevicePresentationHide) then
      GetDeviceProperty(Device, 'UDISKS_PRESENTATION_HIDE', DevicePresentationHide);

    if not GetDeviceProperty(Device, 'UDISKS_NAME', DevicePresentationName) then
      GetDeviceProperty(Device, 'UDISKS_PRESENTATION_NAME', DevicePresentationName);

    if not GetDeviceProperty(Device, 'UDISKS_ICON_NAME', DevicePresentationIconName) then
      GetDeviceProperty(Device, 'UDISKS_PRESENTATION_ICON_NAME', DevicePresentationIconName);

    GetDeviceProperty(Device, 'ID_DRIVE_DETACHABLE', DriveCanDetach);

    Value:= udev_device_get_devtype(Device);
    DeviceIsDrive:= (Value = UDEV_DEVICE_TYPE_DISK);
    DeviceIsPartition:= (Value = UDEV_DEVICE_TYPE_PARTITION);
    if DeviceIsDrive then
    begin
      if not GetDeviceProperty(Device, 'UDISKS_PARTITION_TABLE', DeviceIsPartitionTable) then
      begin
        DeviceIsPartitionTable:= (udev_device_get_property_value(Device, 'ID_PART_TABLE_TYPE' ) <> nil);
      end;
    end
    else if DeviceIsPartition then
    begin
      if not GetDeviceProperty(Device, 'UDISKS_PARTITION_SLAVE', PartitionSlave) then
      begin
        if DeviceObjectPath[Length(DeviceObjectPath)] in ['0'..'9'] then
        begin
          PartitionSlave:= ExtractFileDir(DeviceObjectPath);
          GetDeviceAttribute(PartitionSlave, 'removable', DeviceIsRemovable);
        end;
      end;
    end;

    if not DeviceIsRemovable then
    begin
      GetDeviceAttribute(Device, 'removable', DeviceIsRemovable);
    end;

    UpdateDriveConnectionInterface(SystemPath, Info);

    DeviceIsMediaAvailable:= (Length(IdUsage) > 0) or (Length(IdType) > 0) or
                             (Length(IdUuid) > 0) or (Length(IdLabel) > 0);

    if not DeviceIsMediaAvailable then
    begin
      GetDeviceProperty(Device, 'ID_CDROM_MEDIA', DeviceIsMediaAvailable);
    end;

    for I:= Low(drive_media_mapping) to High(drive_media_mapping) do
    begin
      if Assigned(udev_device_get_property_value(Device, PAnsiChar(drive_media_mapping[I, 0]))) then
      begin
        SetLength(DriveMediaCompatibility, Length(DriveMediaCompatibility) + 1);
        DriveMediaCompatibility[High(DriveMediaCompatibility)]:= drive_media_mapping[I, 1];
      end;
    end;
    {
    WriteLn('Device: ', DeviceFile);
    WriteLn('   Devtype: ', Value);
    WriteLn('   IdType: ', IdType);
    WriteLn('   IdLabel: ', IdLabel );
    WriteLn('   IdVersion: ', IdVersion );
    WriteLn('   IdUsage: ', IdUsage );
    WriteLn('   IdUuid: ', IdUuid );
    WriteLn('   DriveIsMediaEjectable: ', DriveIsMediaEjectable );
    WriteLn('   DeviceIsSystemInternal: ', DeviceIsSystemInternal );
    WriteLn('   DeviceIsPartitionTable: ', DeviceIsPartitionTable );
    WriteLn('   DevicePresentationHide: ', DevicePresentationHide );
    WriteLn('   DevicePresentationName: ', DevicePresentationName );
    WriteLn('   DevicePresentationIconName: ', DevicePresentationIconName );
    WriteLn('   DeviceAutomountHint: ', DeviceAutomountHint );
    WriteLn('   DriveCanDetach: ', DriveCanDetach );
    WriteLn('   PartitionSlave: ', PartitionSlave );
    WriteLn('   DeviceIsRemovable: ', DeviceIsRemovable );
    WriteLn('   DriveConnectionInterface: ', DriveConnectionInterface );
    }
  end;
end;

function EnumerateDevices(out DevicesInfos: TUDisksDevicesInfos): Boolean;
var
  path: PAnsiChar;
  device: Pudev_device;
  devices: Pudev_list_entry;
  enumerate: Pudev_enumerate;
begin
  SetLength(DevicesInfos, 0);
  // Create a list of the devices in the 'block' subsystem
  enumerate:= udev_enumerate_new(udev);
  udev_enumerate_add_match_subsystem(enumerate, 'block');
  udev_enumerate_scan_devices(enumerate);
  devices:= udev_enumerate_get_list_entry(enumerate);
  while devices <> nil do
  begin
    // Get the filename of the /sys entry for the device
    // and create a udev_device object (dev) representing it
    path:= udev_list_entry_get_name(devices);
    device:= udev_device_new_from_syspath(udev, path);

    if Assigned(device) then
    begin
      SetLength(DevicesInfos, Length(DevicesInfos) + 1);
      GetDeviceInfo(path, device, DevicesInfos[High(DevicesInfos)]);
      udev_device_unref(Device);
    end;

    devices:= udev_list_entry_get_next(devices);
  end;
  // Free the enumerator object
  udev_enumerate_unref(enumerate);

  Result:= Length(DevicesInfos) > 0;
end;

function GetDeviceInfo(const ObjectPath: String; out Info: TUDisksDeviceInfo): Boolean;
var
  Device: Pudev_device;
begin
  Device:= udev_device_new_from_syspath(udev, PAnsiChar(ObjectPath));
  Result:= Assigned(Device);
  if Result then
  begin
    GetDeviceInfo(PAnsiChar(ObjectPath), Device, Info);
    udev_device_unref(Device);
  end;
end;

function Initialize: Boolean;
var
  Return: cint;
begin
  // Set up a monitor to monitor block devices
  udev_monitor:= udev_monitor_new_from_netlink(udev, 'udev');
  Result:= Assigned(udev_monitor);
  if Result then
  try
    Return:= udev_monitor_filter_add_match_subsystem_devtype(udev_monitor, 'block', nil);
    Assert(Return = 0, 'udev_monitor_filter_add_match_subsystem_devtype');
    Return:= udev_monitor_enable_receiving(udev_monitor);
    Assert(Return = 0, 'udev_monitor_enable_receiving');
    observers:= TUDisksObserverList.Create;
    udev_monitor_object:= TMonitorObject.Create;
  except
    Result:= False;
    udev_monitor_unref(udev_monitor);
    udev_monitor:= nil;
  end;
end;

procedure Finalize;
begin
  FreeAndNil(udev_monitor_object);
  FreeAndNil(observers);
  udev_monitor_unref(udev_monitor);
end;

procedure AddObserver(Func: TUDisksDeviceNotify);
begin
  if Observers.IndexOf(Func) < 0 then
    Observers.Add(Func);
end;

procedure RemoveObserver(Func: TUDisksDeviceNotify);
begin
  Observers.Remove(Func);
end;

{ TMonitorThread }

procedure TMonitorObject.ReceiveDevice;
var
  I: Integer;
  Method: TUDisksMethod;
begin
  if FAction = 'add' then
    Method:= UDisks_DeviceAdded
  else if FAction = 'remove' then
    Method:= UDisks_DeviceRemoved
  else if FAction = 'change' then
    Method:= UDisks_DeviceChanged
  else
    Method:= UDisks_DeviceChanged;

  Print('Device ' + FAction + ': ' + FDevicePath);
  for I := 0 to Observers.Count - 1 do
    Observers[I](Method, FDevicePath);
end;

procedure TMonitorObject.Handler(Sender: TObject);
var
  device: Pudev_device;
begin
  // Make the call to ReceiveDevice the device
  // select() ensured that this will not block
  device:= udev_monitor_receive_device(udev_monitor);
  if Assigned(device) then
  begin
    FAction:= udev_device_get_action(device);
    FDevicePath:= udev_device_get_syspath(device);
    TThread.Synchronize(nil, ReceiveDevice);
    udev_device_unref(device);
  end;
end;

constructor TMonitorObject.Create;
var
  fd: cint;
begin
  // Get the file descriptor (fd) for the monitor
  // This fd will get passed to poll()
  fd := udev_monitor_get_fd(udev_monitor);

  AddPoll(fd, POLLIN, Handler, False);

  Print('Begin monitoring');
end;

initialization
  Load;

finalization
  Free;

end.

