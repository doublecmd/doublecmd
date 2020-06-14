{
   Double Commander
   -------------------------------------------------------------------------
   Interface to UDisks2 service via libudisks2.

   Copyright (C) 2020 Alexander Koblov (alexx2000@mail.ru)

   Based on udisks-2.8.4/tools/udisksctl.c

   Copyright (C) 2007-2010 David Zeuthen <zeuthen@gmail.com>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uUDisks2;

{$mode delphi}

interface

uses
  SysUtils;

function Mount(const ObjectPath: String; out MountPath: String): Boolean;
function Unmount(const ObjectPath: String): Boolean;

var
  HasUDisks2: Boolean = False;

implementation

uses
  Classes, DynLibs, DCOSUtils, uGio2, uGObject2, uGlib2;

type
  PUDisksBlock = Pointer;
  PUDisksObject = Pointer;
  PUDisksClient = Pointer;
  PUDisksFilesystem = Pointer;

var
  udisks_block_get_device: function(object_: PUDisksBlock): Pgchar; cdecl;
  udisks_block_get_symlinks: function(object_: PUDisksBlock): PPgchar; cdecl;
  udisks_object_peek_block: function(object_: PUDisksObject): PUDisksBlock; cdecl;
  udisks_object_peek_filesystem: function(object_: PUDisksObject): PUDisksFilesystem; cdecl;
  udisks_client_get_object_manager: function(client: PUDisksClient): PGDBusObjectManager; cdecl;
  udisks_client_new_sync: function(cancellable: PGCancellable; error: PPGError): PUDisksClient; cdecl;
  udisks_filesystem_call_unmount_sync: function(proxy: PUDisksFilesystem; arg_options: PGVariant; cancellable: PGCancellable; error: PPGError): gboolean; cdecl;
  udisks_filesystem_call_mount_sync: function(proxy: PUDisksFilesystem; arg_options: PGVariant; out_mount_path: PPgchar; cancellable: PGCancellable; error: PPGError): gboolean; cdecl;

procedure Print(const sMessage: String);
begin
  WriteLn('UDisks2: ', sMessage);
end;

procedure PrintError(AError: PGError);
begin
  Print(AError^.message);
  g_error_free(AError);
end;

function DeviceFileToUDisksObject(Client: PUDisksClient; Device: Pgchar): PUDisksObject;
var
  Symlinks: PPgchar;
  L, Objects: PGList;
  Block: PUDisksBlock;
  Object_: PUDisksObject;
begin
  Result:= nil;
  Objects:= g_dbus_object_manager_get_objects(udisks_client_get_object_manager(Client));
  if Assigned(Objects) then
  try
    L:= Objects;
    while (L <> nil) do
    begin
      Object_:= PUDisksObject(L^.data);
      Block:= udisks_object_peek_block(Object_);
      if Assigned(Block) then
      begin
        if (g_strcmp0(udisks_block_get_device(Block), Device) = 0) then
        begin
          Result:= PUDisksObject(g_object_ref(PGObject(Object_)));
          Exit;
        end;
        Symlinks:= udisks_block_get_symlinks(Block);
        while (Symlinks <> nil) and (Symlinks^ <> nil) do
        begin
          if (g_strcmp0(Symlinks^, Device) = 0) then
          begin
            Result:= PUDisksObject(g_object_ref(PGObject(Object_)));
            Exit;
          end;
          Inc(Symlinks);
        end;
      end;
      L:= L^.next;
    end;
  finally
    g_list_free_full(Objects, TGDestroyNotify(@g_object_unref));
  end;
end;

function MountUnmount(const ObjectPath: String; Mount: Boolean; MountPath: PString): Boolean;
var
  mount_path: Pgchar;
  options: PGVariant;
  AError: PGError = nil;
  object_: PUDisksObject;
  builder: TGVariantBuilder;
  client: PUDisksClient = nil;
  filesystem: PUDisksFilesystem;
begin
  client := udisks_client_new_sync (nil, @AError);
  if (client = nil) then
  begin
    PrintError(AError);
    Exit(False);
  end;
  object_:= DeviceFileToUDisksObject(client, Pgchar(ObjectPath));
  Result:= Assigned(object_);
  if Result then
  begin
    filesystem:= udisks_object_peek_filesystem(object_);
    Result:= Assigned(filesystem);
    if Result then
    begin
      g_variant_builder_init(@builder, PGVariantType(PAnsiChar('a{sv}')));
      options:= g_variant_builder_end (@builder);
      g_variant_ref_sink(options);

      if not Mount then
      begin
        Result:= udisks_filesystem_call_unmount_sync(filesystem, options,
                                                     nil, @AError);
        if not Result then PrintError(AError);
      end
      else begin
        Result:= udisks_filesystem_call_mount_sync(filesystem, options,
                                                   @mount_path, nil, @AError);
        if not Result then
          PrintError(AError)
        else begin
          MountPath^:= StrPas(mount_path);
          g_free(mount_path);
        end;
      end;
      g_variant_unref(options);
    end;
    g_object_unref(PGObject(object_));
  end;
  g_object_unref(PGObject(client));
end;

function Mount(const ObjectPath: String; out MountPath: String): Boolean;
begin
  Result:= MountUnmount(ObjectPath, True, @MountPath);
end;

function Unmount(const ObjectPath: String): Boolean;
begin
  Result:= MountUnmount(ObjectPath, False, nil);
end;

function CheckUDisks({%H-}Parameter : Pointer): PtrInt;
var
  AClient: PGObject;
  AError: PGError = nil;
begin
  Result:= 0;
  AClient := udisks_client_new_sync (nil, @AError);
  HasUDisks2:= Assigned(AClient);
  if HasUDisks2 then
    g_object_unref(AClient)
  else begin
    PrintError(AError);
  end;
  EndThread;
end;

var
  libudisks2_so_0: TLibHandle;

procedure Initialize;
begin
  libudisks2_so_0:= SafeLoadLibrary('libudisks2.so.0');
  if (libudisks2_so_0 <> NilHandle) then
  try
    @udisks_block_get_device:= SafeGetProcAddress(libudisks2_so_0, 'udisks_block_get_device');
    @udisks_block_get_symlinks:= SafeGetProcAddress(libudisks2_so_0, 'udisks_block_get_symlinks');
    @udisks_object_peek_block:= SafeGetProcAddress(libudisks2_so_0, 'udisks_object_peek_block');
    @udisks_object_peek_filesystem:= SafeGetProcAddress(libudisks2_so_0, 'udisks_object_peek_filesystem');
    @udisks_client_get_object_manager:= SafeGetProcAddress(libudisks2_so_0, 'udisks_client_get_object_manager');
    @udisks_client_new_sync:= SafeGetProcAddress(libudisks2_so_0, 'udisks_client_new_sync');
    @udisks_filesystem_call_unmount_sync:= SafeGetProcAddress(libudisks2_so_0, 'udisks_filesystem_call_unmount_sync');
    @udisks_filesystem_call_mount_sync:= SafeGetProcAddress(libudisks2_so_0, 'udisks_filesystem_call_mount_sync');

    BeginThread(@CheckUDisks);
  except
    on E: Exception do
    begin
      UnloadLibrary(libudisks2_so_0);
      libudisks2_so_0:= NilHandle;
      Print(E.Message);
    end;
  end;
end;

initialization
  Initialize;

finalization
  if (libudisks2_so_0 <> NilHandle) then
    UnloadLibrary(libudisks2_so_0);

end.

