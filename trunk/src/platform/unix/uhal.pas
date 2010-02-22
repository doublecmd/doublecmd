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
    DebugLn(sMessage + ': ' + pError^.name + ' ' + pError^.message)
  else
    DebugLn(sMessage);
end;

procedure CheckBlockDev(ctx: PLibHalContext; const udi: PChar; error: PDBusError; const CreateArray: boolean = False);
var cap : PChar;
var s : string;
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
  if(libhal_device_property_exists(ctx, udi, PChar('block.device'), nil) = 0) then
    exit;

  // it is our dev
  if not CreateArray then
  begin
    DeviceWasChanged := True;
  end;
//  DebugLn(udi);
  // add to array
  DeviceList.Add(udi);

  // get dev name
  cap := libhal_device_get_property_string(ctx, udi, Pchar('block.device'), nil);
  s := 'Dev '+cap;
  libhal_free_string(cap);

  // get mount point
  if(libhal_device_property_exists(ctx, udi, PChar('volume.mount_point'), nil) <> 0) then
  begin
    cap := libhal_device_get_property_string(ctx, udi, Pchar('volume.mount_point'), nil);
    s := s+' with mount point  '+cap;
    libhal_free_string(cap);
  end;

//  if not CreateArray then
//    DebugLn('udi = ',udi,s);

end;

procedure CreateBlokDevArr;
var  halDevices : PPChar;
     numDevices,i : Integer;
     udi : PChar;
begin
  // nil DeviceList;
  DeviceList.Clear;

  // Get List of all dev
  halDevices := libhal_get_all_devices(DcHalCtx, @numDevices, nil);
  if halDevices = nil then
  begin
    DebugLn('Cannot get device list');
  end;
//  DebugLn('Number of devices: ' + IntToStr(numDevices));

  // show info for all dev
  for i := 0 to numDevices do
  begin
    udi := (halDevices + i)^;
    CheckBlockDev(DcHalCtx, udi, nil, True);
  end;

  // free mem
  libhal_free_string_array(halDevices);
end;

function CreateHal: Boolean;
var
  DcDbusError : DBusError;
begin
  Result := False;

  // create new HAL
  DcHalCtx := libhal_ctx_new;

  if DcHalCtx = nil then
  begin
    DebugLn('Cannot initialize HAL context');
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
    DebugLn('Cannot register LibHalDeviceAdded');

  if libhal_ctx_set_device_removed(DcHalCtx, @LibHalDeviceRemoved) = 0 then
    DebugLn('Cannot register LibHalDeviceRemoved');

  DeviceList := TStringList.Create;
  CreateBlokDevArr;

  Result := True;
end;

procedure LibHalDeviceAdded(ctx: PLibHalContext; const udi: PChar); cdecl;
begin
//  DebugLn('add dev  ',udi);
//  sleep(1500); // if we dont do it we don`t see new dev
  CheckBlockDev(ctx,udi,nil); // it return value 2 time on one flash like /dev/sda /dev/sda1
end;

procedure LibHalDeviceRemoved(ctx: PLibHalContext; const udi: PChar); cdecl;
begin
  // here we will when somebody remove flash or CD or may be other
  // when it will CD it will one time
  // when it will be flash it will 7 time or more

  // here we can know only UDI
  // UDI here don`t have mount point and dev name

//  DebugLn('remove dev  ', udi);
  if DeviceList.IndexOf(udi) > -1 then
  begin
    DeviceWasChanged := True;
    DeviceList.Delete(DeviceList.IndexOf(udi));
    //CreateBlokDevArr; or we can re create DeviceList
  end;
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
  if Assigned(DcDbus) then
  begin
    if DBusConnectionOpen then
    begin
      dbus_connection_close(DcDbus);
      DBusConnectionOpen := False;
    end;
    dbus_connection_unref(DcDbus);
    DcDbus := nil;
  end;
  if Assigned(DcHalCtx) then
  begin
    if HalConnectionOpen then
    begin
      libhal_ctx_shutdown(DcHalCtx, nil);
      HalConnectionOpen := False;
    end;
    libhal_ctx_free(DcHalCtx);
    DcHalCtx := nil;
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

