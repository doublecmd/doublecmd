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
   , inotify, uUDisks, uFileSystemWatcher
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
  TDriveWatchMethod = (dwmNone, dwmUDisks, dwmTabFiles);

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
  WatchMethod: TDriveWatchMethod = dwmNone;
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
  OldWProc := WNDPROC(SetWindowLong(Handle, GWL_WNDPROC, LONG_PTR(@MyWndProc)));
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
    WatchMethod := dwmUDisks;
    uUDisks.AddObserver(@FakeClass.OnUDisksNotify);
  end
  else
  begin
    Writeln('Detecting devices through /etc/mtab.');
    WatchMethod := dwmTabFiles;
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
  if WatchMethod = dwmUDisks then
  begin
    uUDisks.RemoveObserver(@FakeClass.OnUDisksNotify);
    uUDisks.Finalize;
  end;
  if Assigned(EtcDirWatcher) then
    FreeAndNil(EtcDirWatcher);
  if Assigned(FakeClass) then
    FreeAndNil(FakeClass);
  WatchMethod := dwmNone;
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
function ContainsString(const search: array of string; strings: array of string): Boolean;
var
  i, j: Integer;
begin
  for i := Low(strings) to High(strings) do
  begin
    for j := Low(search) to High(search) do
      if strings[i] = search[j] then
        Exit(True);
  end;
  Result := False;
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
      Name := ExtractFileName(DeviceFile);
      if DeviceIsMounted and (Length(DeviceMountPaths) > 0) then
        Path := DeviceMountPaths[0]
      else
        Path := EmptyStr;
      DriveLabel := IdLabel;

      if DeviceIsPartition then
        DriveType := dtFixed
      else if DeviceIsDrive and ContainsString(
          ['flash',
           'flash_cf',
           'flash_ms',
           'flash_sm',
           'flash_sd',
           'flash_sdhc',
           'flash_mmc'], DriveMediaCompatibility) then
          DriveType := dtFlash
      else if DeviceIsDrive and ContainsString(
          ['floppy',
           'floppy_zip',
           'floppy_jaz'], DriveMediaCompatibility) then
          DriveType := dtFloppy
      else if DeviceIsDrive and ContainsString(
          ['optical'], DriveMediaCompatibility) then
          DriveType := dtCDROM
      else if DeviceIsRemovable then
        DriveType := dtRemovable
      else
        DriveType := dtUnknown;
    end;
  end;
end;

function GetDrivesList_UDisks: TDrivesList;
var
  DevicesList: TStringArray;
  i: Integer;
  Drive: PDrive;
begin
  if uUDisks.EnumerateDevices(DevicesList) then
  begin
    Result := TDrivesList.Create;

    for i := Low(DevicesList) to High(DevicesList) do
    begin
      if UDisksDeviceToDrive(DevicesList[i], Drive) then
        Result.Add(Drive);
    end;
  end
  else
    Result := nil;
end;

function GetDrivesList_TabFiles: TDrivesList;
  function CheckMountEntry(DriveList: TDrivesList; MountEntry: PMountEntry): Boolean;
  var
    J: Integer;
    MountPoint: String;
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

      MountPoint := ExcludeTrailingPathDelimiter(mnt_dir);

      // if already added
      for J:= 0 to DriveList.Count - 1 do
        if DriveList[J]^.Path = MountPoint then
          Exit;
    end;
    Result:= True;
  end;
  function IsInString(const patterns: array of string; const str: string): Boolean;
  var
    I: Integer;
  begin
    for I := Low(patterns) to High(patterns) do
      if Pos(patterns[I], str) > 0 then
        Exit(True);
    Result := False;
  end;
var
  Drive : PDrive;
  fstab: PIOFile;
  pme: PMountEntry;
  MntEntFileList: array[1..2] of PChar = (_PATH_FSTAB, _PATH_MOUNTED);
  I: Integer;
begin
  Result := TDrivesList.Create;
  for I:= Low(MntEntFileList) to High(MntEntFileList) do
  try
  fstab:= setmntent(MntEntFileList[I],'r');
  if not Assigned(fstab) then exit;
  pme:= getmntent(fstab);
  while (pme <> nil) do
  begin
    if CheckMountEntry(Result, pme) then
       begin
         New(Drive);
         with Drive^ do
         begin
           Path := StrPas(pme^.mnt_dir);
           Path := ExcludeTrailingPathDelimiter(Path);
           DriveLabel := Path;
           Name := ExtractFileName(Path);
           if Name = '' then
             Name := Path;

           {$IFDEF DEBUG}
           DebugLn('Adding drive "' + Name + '" with mount point "' + Path + '"');
           {$ENDIF}

           // TODO more correct detect icons by drive type on Linux
           if IsInString(['ISO9660', 'CDROM', 'CDRW', 'DVD'], UpperCase(pme^.mnt_type)) then
             DriveType := dtCDROM else
           if IsInString(['FLOPPY'], UpperCase(pme^.mnt_type)) then
             DriveType := dtFloppy else
           if IsInString(['ZIP', 'USB', 'CAMERA'], UpperCase(pme^.mnt_type)) then
             DriveType := dtFlash else
           if IsInString(['NFS', 'SMB', 'NETW'], UpperCase(pme^.mnt_type)) then
             DriveType := dtNetwork
           else
             DriveType := dtFixed;
         end;
         Result.Add(Drive);
       end;
    pme:= getmntent(fstab);
  end;
  endmntent(fstab);
  except
    DebugLn('Error with ', MntEntFileList[I]);
  end;
end;
{$ENDIF}

class function TDriveWatcher.GetDrivesList: TDrivesList;
{$IF DEFINED(MSWINDOWS)}
var
  Drive : PDrive;
  DriveNum: Integer;
  DriveBits: set of 0..25;
begin
  Result := TDrivesList.Create;
  { fill list }
  Integer(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
  begin
    if not (DriveNum in DriveBits) then Continue;
    New(Drive);
    Result.Add(Drive);
    with Drive^ do
    begin
     Name := Char(DriveNum + Ord('a')) + ':\';
     Path := Name;
     DriveType := TDriveType(GetDriveType(PChar(Name)));
     if DriveType = dtRemovable then
       begin
         if (Path[1] in ['a'..'b']) or (Path[1] in ['A'..'B']) then
           DriveType := dtFloppy
         else
           DriveType := dtFlash;
       end;
     case DriveType of
     dtFloppy:
       DriveLabel:= Path;
     dtNetwork:
       DriveLabel:= mbGetRemoteFileName(Path);
     else
       DriveLabel:= mbGetVolumeLabel(Name, True);
     end;
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
                    DriveType:= dtFixed;
                    WriteLn(Format('Volume "%s" (vRefNum %d), BSD node /dev/%s',
                        [volNameAsCString, actualVolume, PChar(volumeParms.vMDeviceID)]));
                  end
                else
                  begin
                    DriveType:= dtNetwork;
                    WriteLn(Format('Volume "%s" (vRefNum %d)', [volNameAsCString, actualVolume]));
                  end;
                Name:= volNameAsCString;
                Path:= '/Volumes/' + volNameAsCString;
                DriveLabel:= volNameAsCString;
              end;
              Result.Add(Drive);
              //---------------------------------------------------------------
              end;
          Inc(volumeIndex);
        end;
    end; // while
end;
{$ELSE}
begin
  Result := nil;
  case WatchMethod of
    dwmUDisks:   Result := GetDrivesList_UDisks;
    dwmTabFiles: Result := GetDrivesList_TabFiles;
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

