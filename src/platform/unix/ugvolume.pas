{
   Double Commander
   -------------------------------------------------------------------------
   Interface to GVolumeMonitor

   Copyright (C) 2014-2019 Alexander Koblov (alexx2000@mail.ru)

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

unit uGVolume;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDrive;

type
  TGVolumeSignal = (GVolume_Added, GVolume_Removed, GVolume_Changed);
  TGVolumeNotify = procedure(Signal: TGVolumeSignal; Drive: PDrive) of object;

function Initialize: Boolean;
procedure Finalize;

procedure AddObserver(Func: TGVolumeNotify);
procedure RemoveObserver(Func: TGVolumeNotify);

function Unmount(const Path: String): Boolean;
function EnumerateVolumes(DrivesList: TDrivesList): Boolean;

implementation

uses
  typinfo, fgl, uGLib2, uGio2, uGObject2, uShowMsg;

type
  TGVolumeObserverList = specialize TFPGList<TGVolumeNotify>;

var
  VolumeMonitor: PGVolumeMonitor = nil;
  Observers: TGVolumeObserverList = nil;

procedure Print(const Message: String);
begin
  WriteLn('GVolumeMonitor: ', Message);
end;

function ReadString(Volume: PGVolume; const Kind: Pgchar): String;
var
  Value: PAnsiChar;
begin
  Value:= g_volume_get_identifier(Volume, Kind);
  if Value = nil then
    Result:= EmptyStr
  else begin
    Result:= StrPas(Value);
    g_free(Value);
  end;
end;

function VolumeToDrive(Volume: PGVolume): PDrive;
var
  GFile: PGFile;
  GMount: PGMount;
  Name, Path: Pgchar;
begin
  Result:= nil;
  GMount:= g_volume_get_mount(Volume);
  if Assigned(GMount) then
    g_object_unref(PGObject(GMount))
  else begin
    GFile:= g_volume_get_activation_root(Volume);
    if Assigned(GFile) then
    begin
      if not g_file_has_uri_scheme(GFile, 'file') then
      begin
        Path:= g_file_get_uri(GFile);
        if Assigned(Path) then
        begin
          New(Result);
          Result^.IsMounted:= True;
          Result^.DriveType:= dtSpecial;
          Result^.IsMediaAvailable:= True;
          Result^.IsMediaEjectable:= g_volume_can_eject(Volume);
          Result^.DeviceId:= ReadString(Volume, VOLUME_IDENTIFIER_KIND_UNIX_DEVICE);
          Result^.DriveLabel:= ReadString(Volume, VOLUME_IDENTIFIER_KIND_LABEL);
          Name:= g_volume_get_name(Volume);
          if (Name = nil) then
            Result^.DisplayName:= ExtractFileName(Result^.DeviceId)
          else
          begin
            Result^.DisplayName := StrPas(Name);
            g_free(Name);
          end;
          Name:= g_uri_unescape_string(Path, nil);
          if (Name = nil) then
            Result^.Path:= StrPas(Path)
          else begin
            Result^.Path:= StrPas(Name);
            g_free(Name);
          end;
          g_free(Path);
        end;
      end;
      g_object_unref(PGObject(GFile));
    end;
  end;
end;

function MountToDrive(Mount: PGMount): PDrive;
var
  GFile: PGFile;
  Name, Path: Pgchar;
begin
  Result:= nil;
  if not g_mount_is_shadowed(Mount) then
  begin
    GFile:= g_mount_get_root(Mount);
    if Assigned(GFile) then
    begin
      if not g_file_has_uri_scheme(GFile, 'file') then
      begin
        Path:= g_file_get_uri(GFile);
        if Assigned(Path) then
        begin
          New(Result);
          Result^.IsMounted:= True;
          Result^.DriveType:= dtSpecial;
          Result^.IsMediaAvailable:= True;
          Result^.IsMediaEjectable:= g_mount_can_eject(Mount);
          Name:= g_mount_get_name(Mount);
          if (Name = nil) then
            Result^.DisplayName:= ExtractFileName(Result^.Path)
          else
          begin
            Result^.DisplayName := StrPas(Name);
            g_free(Name);
          end;
          Name:= g_uri_unescape_string(Path, nil);
          if (Name = nil) then
            Result^.Path:= StrPas(Path)
          else begin
            Result^.Path:= StrPas(Name);
            g_free(Name);
          end;
          g_free(Path);
        end;
      end;
      g_object_unref(PGObject(GFile));
    end;
  end;
end;

procedure VolumeEvent(volume_monitor: PGVolumeMonitor; volume: PGVolume; user_data: gpointer); cdecl;
var
  Drive: PDrive;
  Index: Integer;
  VolumeEvent: TGVolumeSignal absolute user_data;
begin
  Drive:= VolumeToDrive(volume);
  if Assigned(Drive) then
  begin
    Print(GetEnumName(TypeInfo(TGVolumeSignal), PtrInt(VolumeEvent)) + ': ' + Drive^.Path);
    for Index:= 0 to Observers.Count - 1 do
      Observers[Index](VolumeEvent, Drive);
  end;
end;

procedure MountEvent(volume_monitor: PGVolumeMonitor; mount: PGMount; user_data: gpointer); cdecl;
var
  Drive: PDrive;
  Index: Integer;
  VolumeEvent: TGVolumeSignal absolute user_data;
begin
  Drive:= MountToDrive(mount);
  if Assigned(Drive) then
  begin
    Print(GetEnumName(TypeInfo(TGVolumeSignal), PtrInt(VolumeEvent)) + ': ' + Drive^.Path);
    for Index:= 0 to Observers.Count - 1 do
      Observers[Index](VolumeEvent, Drive);
  end;
end;

procedure AddObserver(Func: TGVolumeNotify);
begin
  if Observers.IndexOf(Func) < 0 then
    Observers.Add(Func);
end;

procedure RemoveObserver(Func: TGVolumeNotify);
begin
  Observers.Remove(Func);
end;

function Initialize: Boolean;
begin
  VolumeMonitor:= g_volume_monitor_get();
  Result:= Assigned(VolumeMonitor);
  if Result then
  begin
    Observers:= TGVolumeObserverList.Create;

    g_signal_connect_data(VolumeMonitor, 'volume-added', TGCallback(@VolumeEvent),
                          gpointer(PtrInt(GVolume_Added)), nil, G_CONNECT_AFTER);
    g_signal_connect_data(VolumeMonitor, 'volume-changed', TGCallback(@VolumeEvent),
                          gpointer(PtrInt(GVolume_Changed)), nil, G_CONNECT_AFTER);
    g_signal_connect_data(VolumeMonitor, 'volume-removed', TGCallback(@VolumeEvent),
                          gpointer(PtrInt(GVolume_Removed)), nil, G_CONNECT_AFTER);

    g_signal_connect_data(VolumeMonitor, 'mount-added', TGCallback(@MountEvent),
                          gpointer(PtrInt(GVolume_Added)), nil, G_CONNECT_AFTER);
    g_signal_connect_data(VolumeMonitor, 'mount-changed', TGCallback(@MountEvent),
                          gpointer(PtrInt(GVolume_Changed)), nil, G_CONNECT_AFTER);
    g_signal_connect_data(VolumeMonitor, 'mount-removed', TGCallback(@MountEvent),
                          gpointer(PtrInt(GVolume_Removed)), nil, G_CONNECT_AFTER);
  end;
end;

procedure Finalize;
begin
  if Assigned(VolumeMonitor) then
  begin
    FreeAndNil(Observers);
    g_object_unref(VolumeMonitor);
    VolumeMonitor:= nil;
  end;
end;

procedure FinishUnmount(source_object: PGObject; res: PGAsyncResult; user_data: gpointer); cdecl;
var
  AError: PGError = nil;
begin
   if not g_mount_unmount_with_operation_finish(PGMount(source_object), res, @AError) then
   begin
     msgError(nil, AError^.message);
     g_error_free(AError);
   end;
   g_object_unref(source_object);
end;

function Unmount(const Path: String): Boolean;
var
  AFile: PGFile;
  AMount: PGMount;
begin
  AFile:= g_file_new_for_uri(Pgchar(Path));
  AMount:= g_file_find_enclosing_mount(AFile, nil, nil);
  Result:= Assigned(AMount);
  if Result then begin
    g_mount_unmount_with_operation(AMount, G_MOUNT_UNMOUNT_NONE, nil, nil, @FinishUnmount, nil);
  end;
  g_object_unref(PGObject(AFile));
end;

function EnumerateVolumes(DrivesList: TDrivesList): Boolean;
var
  Drive: PDrive;
  GMount: PGMount;
  GVolume: PGVolume;
  VolumeList: PGList;
  VolumeTemp: PGList;
begin
  Result:= False;
  VolumeList:= g_volume_monitor_get_volumes(VolumeMonitor);
  if Assigned(VolumeList) then
  begin
    Result:= True;
    VolumeTemp:= VolumeList;
    while Assigned(VolumeTemp) do
    begin
      GVolume:= VolumeTemp^.data;
      Drive:= VolumeToDrive(GVolume);

      if (Assigned(Drive)) then
      begin
        DrivesList.Add(Drive);
        // WriteLn('GVolume: ', Drive^.Path);
      end;

      g_object_unref(PGObject(GVolume));
      VolumeTemp:= VolumeTemp^.next;
    end;
    g_list_free(VolumeList);
  end;

  VolumeList:= g_volume_monitor_get_mounts(VolumeMonitor);
  if Assigned(VolumeList) then
  begin
    Result:= True;
    VolumeTemp:= VolumeList;
    while Assigned(VolumeTemp) do
    begin
      GMount:= VolumeTemp^.data;
      Drive:= MountToDrive(GMount);

      if (Assigned(Drive)) then
      begin
        DrivesList.Add(Drive);
        // WriteLn('GMount: ', Drive^.Path);
      end;

      g_object_unref(PGObject(GMount));
      VolumeTemp:= VolumeTemp^.next;
    end;
    g_list_free(VolumeList);
  end;
end;

end.

