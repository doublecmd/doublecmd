unit uDarwinIO;

{
  APFS bootable specifications, there are two Volumns in the same Grouop:
    System Volumn: ReadOnly, No FSEvent
    Data Volumn:   Writable, MNT_DONTBROWSE, FSEvent
  for bootable APFS volumes, we need to combine the Name of the System Volume and
  the path of the Data Volume by the GroupUUID.
}

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}
{$linkframework IOKit}

interface

uses
  Classes, SysUtils,
  MacOSAll, CocoaAll, CocoaUtils,
  uMyDarwin;

type
  natural_t = UInt32;
  mach_port_t = natural_t;
  io_object_t = mach_port_t;
  io_iterator_t = io_object_t;
  p_io_iterator_t = ^io_iterator_t;
  io_connect_t = io_object_t;
  io_service_t = io_object_t;
  io_registry_entry_t = io_object_t;
  p_io_registry_entry_t = ^io_object_t;

  kern_return_t = integer;
  IOOptionBits = UInt32;
  io_name_t = array of char;

function IOServiceGetMatchingServices(
  mainPort: mach_port_t;
  matching: NSDictionary;
  existing: p_io_iterator_t ): kern_return_t; cdecl; external;

function IORegistryEntryGetParentEntry(
  entry: io_registry_entry_t;
  const plane: io_name_t;
  parent: p_io_registry_entry_t ): kern_return_t; cdecl; external;

function IORegistryEntryCreateCFProperty(
  entry: io_registry_entry_t;
  key: NSString;
  allocator: CFAllocatorRef;
  options: IOOptionBits ): CFTypeRef; cdecl; external;

function IORegistryEntryCreateCFProperties(
  entry: io_registry_entry_t;
  properties: CFMutableDictionaryRefPtr;
  allocator: CFAllocatorRef;
  options: IOOptionBits ):  kern_return_t; cdecl; external;

function IOServiceMatching( const name: pchar ): NSDictionary; cdecl; external;

function IOIteratorNext( iterator: io_iterator_t ): io_object_t; cdecl; external;

var
  kIOMasterPortDefault: mach_port_t; cvar; external;

const
  kIOServicePlane = 'IOService';

type
  PDarwinStatfs = ^TDarwinStatfs;

  { TDarwinIOVolumns }

  TDarwinIOVolumns = class
  private
    _volumns: NSArray;
    _pStatfs: PDarwinStatfs;
    _statfsCount: Integer;
  private
    function createVolumns: NSArray;
    function getDeviceID( const fs: PDarwinStatfs ): NSString;
    function getVolumnByDeviceID( const deviceID: NSString ): NSDictionary;
    function getGroupUUIDByDeviceID( const deviceID: NSString ): NSString;
    function getApfsDataDeviceIDByGroupUUID( const groupUUID: NSString ): NSString;
    function getStatfsByDeviceID( const deviceID: NSString ): PDarwinStatfs;
  public
    constructor Create( const pStatfs: PDarwinStatfs; const statfsCount: Integer );
    destructor Destroy; override;
    function getPath( const fs: PDarwinStatfs ): String;
    function getDisplayName( const fs: PDarwinStatfs ): String;
    function isRemovable( const fs: PDarwinStatfs ): Boolean;
  end;

implementation

const
  ROLE_SYSTEM_MASK = $01;
  ROLE_DATA_MASK   = $40;

var
  BsdName_KEY: NSString;
  VolGroupUUID_KEY: NSString;
  RoleValue_KEY: NSString;
  Removable_KEY: NSString;
  NULL_UUID: NSString;

{ TDarwinIOVolumns }

function TDarwinIOVolumns.createVolumns: NSArray;
var
  ioIterator: io_iterator_t;
  ioServiceObject: io_object_t;
  ioVolumnObject: io_object_t;
  ret: integer;
  volumnProperties: NSMutableDictionary;
  volumns: NSMutableArray;

  bsdName: CFTypeRef;
  groupUUID: CFTypeRef;
  mntFromName: CFTypeRef;
  roleValue: CFTypeRef;
  removable: CFTypeRef;
begin
  Result:= nil;

  ret:= IOServiceGetMatchingServices(
    kIOMasterPortDefault,
    IOServiceMatching( 'IOMediaBSDClient' ),
    @ioIterator );
  if ret <> 0 then
    Exit;

  volumns:= NSMutableArray.new;

  repeat
    ioServiceObject:= IOIteratorNext( ioIterator );
    if ioServiceObject = 0 then
      break;
    ret:= IORegistryEntryGetParentEntry( ioServiceObject, kIOServicePlane, @ioVolumnObject );
    if ret <> 0 then
      break;
    volumnProperties:= NSMutableDictionary.new;
    bsdName:= IORegistryEntryCreateCFProperty( ioVolumnObject, BsdName_KEY, kCFAllocatorDefault, 0 );
    volumnProperties.setValue_forKey( bsdName , BsdName_KEY );
    groupUUID:= IORegistryEntryCreateCFProperty( ioVolumnObject, VolGroupUUID_KEY, kCFAllocatorDefault, 0 );
    volumnProperties.setValue_forKey( groupUUID , VolGroupUUID_KEY );
    roleValue:= IORegistryEntryCreateCFProperty( ioVolumnObject, RoleValue_KEY, kCFAllocatorDefault, 0 );
    volumnProperties.setValue_forKey( roleValue , RoleValue_KEY );
    removable:= IORegistryEntryCreateCFProperty( ioVolumnObject, Removable_KEY, kCFAllocatorDefault, 0 );
    volumnProperties.setValue_forKey( removable , Removable_KEY );
    volumns.addObject( volumnProperties );
    volumnProperties.release;
  until False;

  Result:= volumns;
end;

function TDarwinIOVolumns.getDeviceID(const fs: PDarwinStatfs): NSString;
var
  deviceID: String;
begin
  deviceID:= ExtractFileName( fs^.mntfromname );
  Result:= StrToNSString( deviceID );
end;

function TDarwinIOVolumns.getVolumnByDeviceID(const deviceID: NSString
  ): NSDictionary;
var
  volumn: NSDictionary;
begin
  Result:= nil;
  for volumn in _volumns do begin
    if NOT deviceID.isEqual( volumn.valueForKey(BsdName_KEY) ) then
      continue;
    Result:= volumn;
    break;
  end;
end;

function TDarwinIOVolumns.getGroupUUIDByDeviceID(const deviceID: NSString
  ): NSString;
var
  volumn: NSDictionary;
begin
  Result:= nil;
  for volumn in _volumns do begin
    if NOT deviceID.isEqual( volumn.valueForKey(BsdName_KEY) ) then
      continue;
    Result:= NSString( volumn.valueForKey(VolGroupUUID_KEY) );
    if Result.isEqualToString(NULL_UUID) then
      Result:= nil;
    break;
  end;
end;

function TDarwinIOVolumns.getApfsDataDeviceIDByGroupUUID(
  const groupUUID: NSString): NSString;
var
  volumn: NSDictionary;
  roleValue: NSUInteger;
begin
  Result:= nil;
  for volumn in _volumns do begin
    if NOT groupUUID.isEqual( volumn.valueForKey(VolGroupUUID_KEY) ) then
      continue;
    roleValue:= NSNumber( volumn.valueForKey(RoleValue_KEY) ).unsignedIntValue;
    if (roleValue and ROLE_DATA_MASK) = 0  then
      continue;
    Result:= NSString( volumn.valueForKey(BsdName_KEY) );
    break;
  end;
end;

function TDarwinIOVolumns.getStatfsByDeviceID(const deviceID: NSString
  ): PDarwinStatfs;
var
  fs: PDarwinStatfs;
  i: Integer;
  deviceIDStr: String;
begin
  Result:= nil;
  fs:= _pStatfs;
  deviceIDStr:= deviceID.UTF8String;
  for i:= 0 to _statfsCount-1 do begin
    if ExtractFileName(fs^.mntfromname) = deviceIDStr then begin
      Result:= fs;
      break;
    end;
    inc( fs );
  end;
end;

constructor TDarwinIOVolumns.Create(const pStatfs: PDarwinStatfs;
  const statfsCount: Integer);
begin
  _volumns:= createVolumns;
  _pStatfs:= pStatfs;
  _statfsCount:= statfsCount;
end;

destructor TDarwinIOVolumns.Destroy;
begin
  _volumns.release;
end;

function TDarwinIOVolumns.getPath(const fs: PDarwinStatfs): String;
var
  deviceID: NSString;
  dataFs: PDarwinStatfs;
  groupUUID: NSString;
  dataDeviceID: NSString;
begin
  Result:= fs^.mountpoint;
  if Result = PathDelim then
    Exit;

  dataFs:= nil;
  deviceID:= self.getDeviceID( fs );
  groupUUID:= self.getGroupUUIDByDeviceID( deviceID );
  if groupUUID <> nil then begin
    dataDeviceID:= self.getApfsDataDeviceIDByGroupUUID( groupUUID );
    if dataDeviceID <> nil then
      dataFs:= self.getStatfsByDeviceID( dataDeviceID );
  end;
  if Assigned(dataFs) then
    Result:= dataFS^.mountpoint;
end;

function TDarwinIOVolumns.getDisplayName(const fs: PDarwinStatfs): String;
begin
  if fs^.mountpoint = PathDelim then begin
    Result:= getMacOSDisplayNameFromPath( PathDelim );
    if Result = EmptyStr then
      Result:= 'System';
  end else begin
    Result:= fs^.mountpoint;
  end;
end;

function TDarwinIOVolumns.isRemovable(const fs: PDarwinStatfs): Boolean;
var
  deviceID: NSString;
  volumn: NSDictionary;
  removable: NSNumber;
begin
  deviceID:= self.getDeviceID( fs );
  volumn:= self.getVolumnByDeviceID( deviceID );
  removable:= NSNumber( volumn.valueForKey(Removable_KEY) );
  Result:= ( removable.integerValue <> 0 );
end;

initialization
  BsdName_KEY:= NSSTR( 'BSD Name' );
  VolGroupUUID_KEY:= NSSTR( 'VolGroupUUID' );
  RoleValue_KEY:= NSSTR( 'RoleValue' );
  Removable_KEY:= NSSTR( 'Removable' );
  NULL_UUID:= NSSTR('00000000-0000-0000-0000-000000000000');

end.

