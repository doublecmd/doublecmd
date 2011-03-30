unit uHal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libhal, dbus;

// create all Hal object
function CreateHal: Boolean;
// procedure where we get message add or remove
procedure LibHalDeviceAdded(ctx: PLibHalContext; const udi: PChar);cdecl;
procedure LibHalDeviceRemoved(ctx: PLibHalContext; const udi: PChar);cdecl;
procedure LibHalDeviceNewCapability(ctx: PLibHalContext; const udi: PChar; const capability: PChar);cdecl;
procedure LibHalDeviceLostCapability(ctx: PLibHalContext; const udi: PChar; const capability: PChar);cdecl;
procedure LibHalDevicePropertyModified(ctx: PLibHalContext; const udi: PChar; const key: PChar;
                                       is_removed, is_added: dbus_bool_t);cdecl;
procedure LibHalDeviceCondition(ctx: PLibHalContext; const udi: PChar;
                                const condition_name: PChar;
                                const condition_delta: PChar); cdecl;

// dispach message and
function CheckHalMsg: Boolean;
// create all Hal object
procedure FreeHal;
function IsHal: Boolean;

implementation

uses
  LCLProc;

var
  DcDbus : PDBusConnection = nil;
  DBusConnectionOpen : Boolean = False;
  DcHalCtx : PLibHalContext = nil;
  HalConnectionOpen : Boolean = False;
  DeviceWasChanged : Boolean = False;
  DeviceList : TStringList = nil;


procedure PrintError(const sMessage: String; pError: PDBusError);
begin
  if Assigned(pError) and (dbus_error_is_set(pError) <> 0) then
    DCDebug(sMessage + ': ' + pError^.name + ' ' + pError^.message)
  else
    DCDebug(sMessage);
end;

procedure CheckBlockDev(ctx: PLibHalContext; const udi: PChar; const CreateArray: boolean = False);
var
  cap : PChar;
  s : string;
  DcDbusError : DBusError;
  IsBlockDevice, HasMountPoint: Boolean;
begin
{  possible value what we can get (you can see that volume.label it will be mount point)
               for more key see "hal-device | less"
  volume.label = 'ATTID'  (string)
  volume.mount_point = '/media/ATTID'  (string)
  volume.is_mounted = true  (bool)
  volume.is_mounted_read_only = false  (bool)
  volume.linux.is_device_mapper = false  (bool)
  volume.is_disc = false  (bool)
  block.device = '/dev/sdc1'  (string)
  volume.is_partition = true  (bool)
  block.major = 8  (0x8)  (int)
  volume.partition.number = 1  (0x1)  (int)
  block.minor = 33  (0x21)  (int)
  volume.block_size = 512  (0x200)  (int)
  block.is_volume = true  (bool)
}
  // exit if bad udi
  if udi = nil then exit;

  // exit if not block dev
  dbus_error_init(@DcDbusError);
  IsBlockDevice := libhal_device_property_exists(ctx, udi, PChar('block.device'), @DcDbusError) <> 0;
  if dbus_error_is_set(@DcDbusError) <> 0 then
  begin
    PrintError('Cannot get block.device property for ' + udi, @DcDbusError);
    dbus_error_free(@DcDbusError);
    Exit;
  end;
  if not IsBlockDevice then
    Exit;

  // it is our dev
  if not CreateArray then
  begin
    DeviceWasChanged := True;
  end;

  s := StringReplace(StrPas(udi), '/org/freedesktop/Hal/devices/', '', [rfIgnoreCase]);

  // add to array
  DeviceList.Add(udi);

  // get dev name
  cap := libhal_device_get_property_string(ctx, udi, Pchar('block.device'), nil);
  s := s + ' is ' + cap;
  libhal_free_string(cap);

  // get mount point
  dbus_error_init(@DcDbusError);
  HasMountPoint := libhal_device_property_exists(ctx, udi, PChar('volume.mount_point'), @DcDbusError) <> 0;
  if dbus_error_is_set(@DcDbusError) <> 0 then
  begin
    PrintError('Cannot get volume.mount_point property for ' + udi, @DcDbusError);
    dbus_error_free(@DcDbusError);
    Exit;
  end;
  if HasMountPoint then
  begin
    cap := libhal_device_get_property_string(ctx, udi, Pchar('volume.mount_point'), nil);
    s := s + ' with mount point ' + cap;
    libhal_free_string(cap);
  end;

  //DCDebug(s);
end;

function CreateBlokDevArr: Boolean;
var
  halDevices : PPChar = nil;
  numDevices,i : Integer;
  udi : PChar;
  DcDbusError : DBusError;
begin
  Result:= False;

  // nil DeviceList;
  DeviceList.Clear;

  // Get List of all dev
  dbus_error_init(@DcDbusError);
  halDevices := libhal_get_all_devices(DcHalCtx, @numDevices, @DcDbusError);
  if (not Assigned(halDevices)) or (dbus_error_is_set(@DcDbusError) <> 0) then
  begin
    PrintError('Cannot get device list', @DcDbusError);
    dbus_error_free(@DcDbusError);
    Exit;
  end;
  //DCDebug('Number of devices: ' + IntToStr(numDevices));

  // show info for all dev
  for i := 0 to numDevices - 1 do
  begin
    udi := (halDevices + i)^;
    CheckBlockDev(DcHalCtx, udi, True);
  end;

  // free mem
  libhal_free_string_array(halDevices);

  Result:= True;
end;

function CreateHal: Boolean;
var
  DcDbusError : DBusError;
begin
  Result := False;

  DCDebug('Initializing HAL');

  // create new HAL
  DcHalCtx := libhal_ctx_new;

  if DcHalCtx = nil then
  begin
    DCDebug('Cannot create HAL context');
    Exit;
  end;

  // new connection with D-Bus
  dbus_error_init(@DcDbusError);
  DcDbus := dbus_bus_get_private(DBUS_BUS_SYSTEM, @DcDbusError);
  if (not Assigned(DcDbus)) or (dbus_error_is_set(@DcDbusError) <> 0) then
  begin
    PrintError('Cannot get DBUS connection', @DcDbusError);
    FreeHal;
    dbus_error_free(@DcDbusError);
    Exit;
  end;

  DBusConnectionOpen := True;

  // Disable exiting the application when it is disconnected from DBUS.
  dbus_connection_set_exit_on_disconnect(DcDbus, 0);

  // Set dbus connection for the HAL context.
  if libhal_ctx_set_dbus_connection(DcHalCtx, DcDbus) = 0 then
  begin
    DCDebug('Cannot set DBUS connection for HAL context');
    FreeHal;
    Exit;
  end;

  // init HAL
  dbus_error_init(@DcDbusError);
  if (libhal_ctx_init(DcHalCtx, @DcDbusError) = 0) or
     (dbus_error_is_set(@DcDbusError) <> 0) then
  begin
    PrintError('Cannot initialize HAL', @DcDbusError);
    FreeHal;
    dbus_error_free(@DcDbusError);
    Exit;
  end;

  HalConnectionOpen := True;

  if libhal_ctx_set_device_added(DcHalCtx, @LibHalDeviceAdded) = 0 then
    DCDebug('Cannot register LibHalDeviceAdded');

  if libhal_ctx_set_device_removed(DcHalCtx, @LibHalDeviceRemoved) = 0 then
    DCDebug('Cannot register LibHalDeviceRemoved');

  if libhal_ctx_set_device_new_capability(DcHalCtx, @LibHalDeviceNewCapability) = 0 then
    DCDebug('Cannot register LibHalDeviceNewCapability');

  if libhal_ctx_set_device_lost_capability(DcHalCtx, @LibHalDeviceLostCapability) = 0 then
    DCDebug('Cannot register LibHalDeviceLostCapability');

  if libhal_ctx_set_device_property_modified(DcHalCtx, @LibHalDevicePropertyModified) = 0 then
    DCDebug('Cannot register LibHalDevicePropertyModified');

  if libhal_ctx_set_device_condition(DcHalCtx, @LibHalDeviceCondition) = 0 then
    DCDebug('Cannot register LibHalDeviceCondition');

  // Watch all properties.
  dbus_error_init(@DcDbusError);
  if (libhal_device_property_watch_all(DcHalCtx, @DcDbusError) = 0) or
     (dbus_error_is_set(@DcDbusError) <> 0) then
  begin
    PrintError('Cannot watch all properties', @DcDbusError);
    FreeHal;
    dbus_error_free(@DcDbusError);
    Exit;
  end;

  DeviceList := TStringList.Create;
  if not CreateBlokDevArr then
  begin
    FreeHal;
    Exit;
  end;

  DCDebug('HAL initialized OK');
  Result := True;
end;

procedure LibHalDeviceAdded(ctx: PLibHalContext; const udi: PChar); cdecl;
begin
  DCDebug('HAL: new device added: ',udi);
//  sleep(1500); // if we dont do it we don`t see new dev
  CheckBlockDev(ctx,udi); // it return value 2 time on one flash like /dev/sda /dev/sda1
end;

procedure LibHalDeviceRemoved(ctx: PLibHalContext; const udi: PChar); cdecl;
begin
  // here we will when somebody remove flash or CD or may be other
  // when it will CD it will one time
  // when it will be flash it will 7 time or more

  // here we can know only UDI
  // UDI here don`t have mount point and dev name

//  DCDebug('remove dev  ', udi);
  if DeviceList.IndexOf(udi) > -1 then
  begin
    DeviceWasChanged := True;
    DeviceList.Delete(DeviceList.IndexOf(udi));
    //CreateBlokDevArr; or we can re create DeviceList
  end;
  DCDebug('HAL: Device was removed: ', udi);
end;

procedure LibHalDeviceNewCapability(ctx: PLibHalContext; const udi: PChar; const capability: PChar);cdecl;
begin
  WriteLn('LibHalDeviceNewCapability');
  DeviceWasChanged := True;
end;

procedure LibHalDeviceLostCapability(ctx: PLibHalContext; const udi: PChar; const capability: PChar);cdecl;
begin
  WriteLn('LibHalDeviceLostCapability');
  DeviceWasChanged := True;
end;

procedure LibHalDevicePropertyModified(ctx: PLibHalContext; const udi: PChar; const key: PChar;
                                       is_removed, is_added: dbus_bool_t);cdecl;
begin
  WriteLn('LibHalDevicePropertyModified');
  DeviceWasChanged := True;
end;

procedure LibHalDeviceCondition(ctx: PLibHalContext; const udi: PChar;
                                const condition_name: PChar;
                                const condition_delta: PChar); cdecl;
begin
  WriteLn('LibHalDeviceCondition');
  DeviceWasChanged := True;
end;

function CheckHalMsg: Boolean;
begin
  if IsHal and DBusConnectionOpen then
  begin
    // for one disk i must dispatch ~9 msg try do it 3 times
    if dbus_connection_read_write_dispatch(DcDbus, 1) = 0 then
      DBusConnectionOpen := False
    else if dbus_connection_read_write_dispatch(DcDbus, 1) = 0 then
      DBusConnectionOpen := False
    else if dbus_connection_read_write_dispatch(DcDbus, 1) = 0 then
      DBusConnectionOpen := False;

    Result := DeviceWasChanged;
    DeviceWasChanged := False;
  end
  else
    Result := False;
end;

procedure FreeHal;
begin
  if Assigned(DcHalCtx) then
  begin
    if HalConnectionOpen then
    begin
      DCDebug('Shutting down HAL');
      libhal_ctx_shutdown(DcHalCtx, nil);
      HalConnectionOpen := False;
    end;
    libhal_ctx_free(DcHalCtx);
    DcHalCtx := nil;
  end;
  if Assigned(DcDbus) then
  begin
    if DBusConnectionOpen then
    begin
      DCDebug('Closing DBUS connection');
      dbus_connection_close(DcDbus);
      DBusConnectionOpen := False;
    end;
    dbus_connection_unref(DcDbus);
    DcDbus := nil;
  end;
  FreeThenNil(DeviceList);
end;

function IsHal: Boolean;
begin
  Result := Assigned(DcHalCtx) and Assigned(DcDbus);
end;

initialization
  CreateHal;

finalization
  FreeHal;

end.

