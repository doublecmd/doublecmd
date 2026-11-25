{
   Double Commander
   -------------------------------------------------------------------------
   K Desktop Environment integration unit

   Copyright (C) 2014-2026 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uKde;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMyUnix, uDrive;

const
  KMTP_MIME_DIRECTORY = 'inode/directory';

type
  TKmtpDeviceInfo = record
    ObjectPath: String;
    Udi: String;
    FriendlyName: String;
  end;
  TKmtpDeviceArray = array of TKmtpDeviceInfo;

  TKmtpStorageInfo = record
    ObjectPath: String;
    Description: String;
    MaxCapacity: UInt64;
    FreeSpace: UInt64;
  end;
  TKmtpStorageArray = array of TKmtpStorageInfo;

  TKmtpFileInfo = record
    ItemId: UInt32;
    ParentId: UInt32;
    StorageId: UInt32;
    FileName: String;
    FileSize: UInt64;
    ModDate: Int64;
    FileType: String;
  end;
  TKmtpFileArray = array of TKmtpFileInfo;

  TKmtpTransferProgressCallback = function(Transferred, Total: UInt64): Boolean of object;

function KioOpen(const URL: String): Boolean;

function IsKmtpActive: Boolean;
procedure KmtpInitialize(AOnChanged: TNotifyEvent);
procedure KmtpFinalize;
function KmtpEnumerateVolumes(DrivesList: TDrivesList): Boolean;
function KmtpResolvePath(var APath: String): Boolean;

function EncodeMtpHost(const AName: String): String;
function DecodeMtpHost(const AHost: String): String;

function KmtpListDevices: TKmtpDeviceArray;
function KmtpListStorages(const ADevicePath: String): TKmtpStorageArray;
function KmtpGetStorageSpace(const AStoragePath: String; out AFreeSpace, AMaxCapacity: UInt64): Boolean;
function KmtpGetFilesAndFolders(const AStoragePath, ARelPath: String;
  out AFiles: TKmtpFileArray; out AExistsCode: Integer): Boolean;
function KmtpGetFileMetadata(const AStoragePath, ARelPath: String; out AInfo: TKmtpFileInfo): Boolean;
function KmtpCreateFolder(const AStoragePath, ARelPath: String): Boolean;
function KmtpDeleteObject(const AStoragePath, ARelPath: String): Boolean;
function KmtpSetFileName(const AStoragePath, ARelPath, ANewName: String): Boolean;
function KmtpTransferFileFD(const AStoragePath, ARelPath, ALocalFile: String;
  AIsUpload: Boolean; ATotalBytes: UInt64; AProgress: TKmtpTransferProgressCallback;
  out AErrorMsg: String): Boolean;

var
  HasKdeOpen: Boolean = False;

implementation

uses
  BaseUnix, CTypes, LazLogger, syncobjs,
  DCConvertEncoding, DCOSUtils, DCStrUtils, DCUnix,
  uDCUtils, uGlobs, uOSUtils, uTrash, uDebug, uClipboard,
  uGLib2, uGio2, uGObject2, uGio;

const
  KMTPD_BUS_NAME          = 'org.kde.kmtpd5';
  KMTPD_DAEMON_PATH       = '/modules/kmtpd';
  KMTPD_DAEMON_INTERFACE  = 'org.kde.kmtp.Daemon';
  KMTPD_DEVICE_INTERFACE  = 'org.kde.kmtp.Device';
  KMTPD_STORAGE_INTERFACE = 'org.kde.kmtp.Storage';
  DBUS_PROPERTIES_IFACE   = 'org.freedesktop.DBus.Properties';
  KMTPD_TIMEOUT           = 30000;

type
  PKmtpTransferState = ^TKmtpTransferState;
  TKmtpTransferState = record
    Transferred: UInt64;
    Total: UInt64;
    ResultCode: gint32;
    Finished: Boolean;
  end;

var
  KdeVersion: String;
  KdeOpen: String = 'kioclient';

  KmtpLock: TCriticalSection = nil;
  KmtpDBusConn: PGDBusConnection = nil;
  KmtpInitialized: Boolean = False;
  KmtpUnavailable: Boolean = False;
  KmtpDevicesSubId: guint = 0;
  OnKmtpChanged: TNotifyEvent = nil;

function KioOpen(const URL: String): Boolean;
begin
  Result:= ExecCmdFork(KdeOpen + ' exec ' + QuoteStr(URL));
end;

function FileTrash(const FileName: String): Boolean;
begin
  try
    Result:= ExecuteProcess(KdeOpen, ['--noninteractive', 'move', FileName, 'trash:/']) = 0;
  except
    on E: Exception do
    begin
      Result:= False;
      DebugLn('FileTrash: ', E.Message);
    end;
  end;
end;

function EncodeMtpHost(const AName: String): String;
const
  HEX_CHARS: array[0..15] of AnsiChar = '0123456789ABCDEF';
var
  I: Integer;
  Ch: AnsiChar;
begin
  Result := '';
  for I := 1 to Length(AName) do
  begin
    Ch := AName[I];
    if Ch in ['%', '/', '?', '#', '@', ':', '[', ']'] then
      Result := Result + '%' + HEX_CHARS[Ord(Ch) shr 4] + HEX_CHARS[Ord(Ch) and $0F]
    else
      Result := Result + Ch;
  end;
end;

function DecodeMtpHost(const AHost: String): String;
begin
  Result := URIDecode(AHost);
end;

function GvfsSupportsMtp: Boolean;
var
  GVfs: PGVfs;
  Schemes: PPgchar;
begin
  Result := False;
  GVfs := g_vfs_get_default();
  if GVfs = nil then
    Exit;
  Schemes := g_vfs_get_supported_uri_schemes(GVfs);
  if Schemes = nil then
    Exit;
  while Schemes^ <> nil do
  begin
    if StrComp(Schemes^, 'mtp') = 0 then
      Exit(True);
    Inc(Schemes);
  end;
end;

function IsKmtpActive: Boolean;
begin
  if KmtpUnavailable or (not HasGio) then
    Exit(False);
  if (not Assigned(g_bus_get_sync)) or
     (not Assigned(g_dbus_connection_call_sync)) or
     (not Assigned(g_dbus_connection_call_with_unix_fd_list_sync)) or
     (not Assigned(g_dbus_connection_signal_subscribe)) or
     (not Assigned(g_dbus_connection_signal_unsubscribe)) then
    Exit(False);
  // Keep GNOME/COSMIC/GTK GVfs behavior unchanged when GVfs handles mtp://
  if (DesktopEnv <> DE_KDE) and GvfsSupportsMtp then
    Exit(False);
  Result := True;
end;

procedure OnKmtpDevicesChanged(connection: PGDBusConnection;
  sender_name: Pgchar; object_path: Pgchar; interface_name: Pgchar;
  signal_name: Pgchar; parameters: PGVariant; user_data: gpointer); cdecl;
begin
  if Assigned(OnKmtpChanged) then
    OnKmtpChanged(nil);
end;

function EnsureKmtpDBusConn: PGDBusConnection;
var
  AError: PGError = nil;
begin
  if not IsKmtpActive then
    Exit(nil);

  KmtpLock.Acquire;
  try
    if KmtpUnavailable then
      Exit(nil);
    if KmtpDBusConn = nil then
    begin
      KmtpDBusConn := g_bus_get_sync(G_BUS_TYPE_SESSION, nil, @AError);
      if Assigned(AError) then
      begin
        DCDebug('KMTP: Cannot connect to session bus: ', StrPas(AError^.message));
        g_error_free(AError);
        KmtpUnavailable := True;
        Exit(nil);
      end;
    end;
    if Assigned(KmtpDBusConn) and KmtpInitialized and (KmtpDevicesSubId = 0) then
    begin
      KmtpDevicesSubId := g_dbus_connection_signal_subscribe(
        KmtpDBusConn, nil, KMTPD_DAEMON_INTERFACE, 'devicesChanged',
        KMTPD_DAEMON_PATH, nil, G_DBUS_SIGNAL_FLAGS_NONE,
        @OnKmtpDevicesChanged, nil, nil);
    end;
    Result := KmtpDBusConn;
  finally
    KmtpLock.Release;
  end;
end;

function KmtpCallSync(const AObjectPath, AInterfaceName, AMethodName: String;
  AParams: PGVariant; out AErrorMsg: String): PGVariant;
var
  Conn: PGDBusConnection;
  AError: PGError = nil;
  RemoteErr: Pgchar;
begin
  Result := nil;
  AErrorMsg := '';
  Conn := EnsureKmtpDBusConn;
  if Conn = nil then
    Exit;

  Result := g_dbus_connection_call_sync(
    Conn,
    KMTPD_BUS_NAME,
    Pgchar(AObjectPath),
    Pgchar(AInterfaceName),
    Pgchar(AMethodName),
    AParams,
    nil,
    G_DBUS_CALL_FLAGS_NONE,
    KMTPD_TIMEOUT,
    nil,
    @AError);

  if Assigned(AError) then
  begin
    AErrorMsg := StrPas(AError^.message);
    if Assigned(g_dbus_error_is_remote_error) and Assigned(g_dbus_error_get_remote_error) and
       g_dbus_error_is_remote_error(AError) then
    begin
      RemoteErr := g_dbus_error_get_remote_error(AError);
      if Assigned(RemoteErr) then
      begin
        if (StrComp(RemoteErr, 'org.freedesktop.DBus.Error.ServiceUnknown') = 0) or
           (StrComp(RemoteErr, 'org.freedesktop.DBus.Error.NameHasNoOwner') = 0) then
          KmtpUnavailable := True;
        g_free(RemoteErr);
      end;
    end
    else if (Pos('ServiceUnknown', AErrorMsg) > 0) or
            (Pos('The name is not activatable', AErrorMsg) > 0) then
      KmtpUnavailable := True;
    g_error_free(AError);
  end;
end;

function KmtpGetPropertyString(const AObjectPath, AInterfaceName, APropName: String): String;
var
  Res, Child, Val: PGVariant;
  ErrMsg: String;
  S: Pgchar;
begin
  Result := '';
  Res := KmtpCallSync(AObjectPath, DBUS_PROPERTIES_IFACE, 'Get',
    g_variant_new('(ss)', [PAnsiChar(AInterfaceName), PAnsiChar(APropName)]), ErrMsg);
  if Res = nil then
    Exit;
  try
    Child := g_variant_get_child_value(Res, 0);
    if Assigned(Child) then
    try
      Val := g_variant_get_variant(Child);
      if Assigned(Val) then
      try
        S := g_variant_get_string(Val, nil);
        if Assigned(S) then
          Result := StrPas(S);
      finally
        g_variant_unref(Val);
      end;
    finally
      g_variant_unref(Child);
    end;
  finally
    g_variant_unref(Res);
  end;
end;

function KmtpGetPropertyUInt64(const AObjectPath, AInterfaceName, APropName: String; out AValue: UInt64): Boolean;
var
  Res, Child, Val: PGVariant;
  ErrMsg: String;
begin
  Result := False;
  AValue := 0;
  Res := KmtpCallSync(AObjectPath, DBUS_PROPERTIES_IFACE, 'Get',
    g_variant_new('(ss)', [PAnsiChar(AInterfaceName), PAnsiChar(APropName)]), ErrMsg);
  if Res = nil then
    Exit;
  try
    Child := g_variant_get_child_value(Res, 0);
    if Assigned(Child) then
    try
      Val := g_variant_get_variant(Child);
      if Assigned(Val) then
      try
        AValue := g_variant_get_uint64(Val);
        Result := True;
      finally
        g_variant_unref(Val);
      end;
    finally
      g_variant_unref(Child);
    end;
  finally
    g_variant_unref(Res);
  end;
end;

function KmtpListObjectPaths(const AObjectPath, AInterfaceName, AMethodName: String): TStringArray;
var
  Res, Arr, Elem: PGVariant;
  Count, I: SizeInt;
  ErrMsg: String;
  S: Pgchar;
begin
  Result := nil;
  Res := KmtpCallSync(AObjectPath, AInterfaceName, AMethodName, nil, ErrMsg);
  if Res = nil then
    Exit;
  try
    if g_variant_n_children(Res) = 0 then
      Exit;
    Arr := g_variant_get_child_value(Res, 0);
    if Assigned(Arr) then
    try
      Count := SizeInt(g_variant_n_children(Arr));
      if Count > 0 then
      begin
        SetLength(Result, Count);
        for I := 0 to Count - 1 do
        begin
          Elem := g_variant_get_child_value(Arr, gsize(I));
          if Assigned(Elem) then
          try
            S := g_variant_get_string(Elem, nil);
            if Assigned(S) then
              Result[I] := StrPas(S);
          finally
            g_variant_unref(Elem);
          end;
        end;
      end;
    finally
      g_variant_unref(Arr);
    end;
  finally
    g_variant_unref(Res);
  end;
end;

function KmtpListDevices: TKmtpDeviceArray;
var
  Paths: TStringArray;
  I: Integer;
begin
  Result := nil;
  if not IsKmtpActive then
    Exit;
  Paths := KmtpListObjectPaths(KMTPD_DAEMON_PATH, KMTPD_DAEMON_INTERFACE, 'listDevices');
  SetLength(Result, Length(Paths));
  for I := 0 to High(Paths) do
  begin
    Result[I].ObjectPath := Paths[I];
    Result[I].Udi := KmtpGetPropertyString(Paths[I], KMTPD_DEVICE_INTERFACE, 'udi');
    Result[I].FriendlyName := KmtpGetPropertyString(Paths[I], KMTPD_DEVICE_INTERFACE, 'friendlyName');
  end;
end;

function KmtpListStorages(const ADevicePath: String): TKmtpStorageArray;
var
  Paths: TStringArray;
  I: Integer;
begin
  Result := nil;
  if ADevicePath = '' then
    Exit;
  Paths := KmtpListObjectPaths(ADevicePath, KMTPD_DEVICE_INTERFACE, 'listStorages');
  SetLength(Result, Length(Paths));
  for I := 0 to High(Paths) do
  begin
    Result[I].ObjectPath := Paths[I];
    Result[I].Description := KmtpGetPropertyString(Paths[I], KMTPD_STORAGE_INTERFACE, 'description');
    KmtpGetPropertyUInt64(Paths[I], KMTPD_STORAGE_INTERFACE, 'maxCapacity', Result[I].MaxCapacity);
    KmtpGetPropertyUInt64(Paths[I], KMTPD_STORAGE_INTERFACE, 'freeSpaceInBytes', Result[I].FreeSpace);
  end;
end;

function KmtpGetStorageSpace(const AStoragePath: String; out AFreeSpace, AMaxCapacity: UInt64): Boolean;
begin
  AFreeSpace := 0;
  AMaxCapacity := 0;
  if AStoragePath = '' then
    Exit(False);
  KmtpGetPropertyUInt64(AStoragePath, KMTPD_STORAGE_INTERFACE, 'freeSpaceInBytes', AFreeSpace);
  KmtpGetPropertyUInt64(AStoragePath, KMTPD_STORAGE_INTERFACE, 'maxCapacity', AMaxCapacity);
  Result := (AMaxCapacity > 0);
end;

function ParseKmtpFileTuple(ATuple: PGVariant): TKmtpFileInfo;
var
  Field: PGVariant;
  S: Pgchar;
begin
  Result := Default(TKmtpFileInfo);
  if (ATuple = nil) or (g_variant_n_children(ATuple) < 7) then
    Exit;

  Field := g_variant_get_child_value(ATuple, 0);
  if Assigned(Field) then begin Result.ItemId := g_variant_get_uint32(Field); g_variant_unref(Field); end;

  Field := g_variant_get_child_value(ATuple, 1);
  if Assigned(Field) then begin Result.ParentId := g_variant_get_uint32(Field); g_variant_unref(Field); end;

  Field := g_variant_get_child_value(ATuple, 2);
  if Assigned(Field) then begin Result.StorageId := g_variant_get_uint32(Field); g_variant_unref(Field); end;

  Field := g_variant_get_child_value(ATuple, 3);
  if Assigned(Field) then
  begin
    S := g_variant_get_string(Field, nil);
    if Assigned(S) then Result.FileName := StrPas(S);
    g_variant_unref(Field);
  end;

  Field := g_variant_get_child_value(ATuple, 4);
  if Assigned(Field) then begin Result.FileSize := g_variant_get_uint64(Field); g_variant_unref(Field); end;

  Field := g_variant_get_child_value(ATuple, 5);
  if Assigned(Field) then begin Result.ModDate := g_variant_get_int64(Field); g_variant_unref(Field); end;

  Field := g_variant_get_child_value(ATuple, 6);
  if Assigned(Field) then
  begin
    S := g_variant_get_string(Field, nil);
    if Assigned(S) then Result.FileType := StrPas(S);
    g_variant_unref(Field);
  end;
end;

function KmtpGetFilesAndFolders(const AStoragePath, ARelPath: String;
  out AFiles: TKmtpFileArray; out AExistsCode: Integer): Boolean;
var
  Res, Arr, Elem, ExistsV: PGVariant;
  Count, I: SizeInt;
  ErrMsg: String;
begin
  Result := False;
  AFiles := nil;
  AExistsCode := 1;
  Res := KmtpCallSync(AStoragePath, KMTPD_STORAGE_INTERFACE, 'getFilesAndFolders',
    g_variant_new('(s)', [PAnsiChar(ARelPath)]), ErrMsg);
  if Res = nil then
    Exit;
  try
    if g_variant_n_children(Res) < 2 then
      Exit;

    ExistsV := g_variant_get_child_value(Res, 1);
    if Assigned(ExistsV) then
    try
      AExistsCode := g_variant_get_int32(ExistsV);
    finally
      g_variant_unref(ExistsV);
    end;

    Arr := g_variant_get_child_value(Res, 0);
    if Assigned(Arr) then
    try
      Count := SizeInt(g_variant_n_children(Arr));
      if Count > 0 then
      begin
        SetLength(AFiles, Count);
        for I := 0 to Count - 1 do
        begin
          Elem := g_variant_get_child_value(Arr, gsize(I));
          if Assigned(Elem) then
          try
            AFiles[I] := ParseKmtpFileTuple(Elem);
          finally
            g_variant_unref(Elem);
          end;
        end;
      end;
      Result := True;
    finally
      g_variant_unref(Arr);
    end;
  finally
    g_variant_unref(Res);
  end;
end;

function KmtpGetFileMetadata(const AStoragePath, ARelPath: String; out AInfo: TKmtpFileInfo): Boolean;
var
  Res, TupleV: PGVariant;
  ErrMsg: String;
begin
  Result := False;
  AInfo := Default(TKmtpFileInfo);
  if (ARelPath = '') or (ARelPath = '/') then
    Exit;
  Res := KmtpCallSync(AStoragePath, KMTPD_STORAGE_INTERFACE, 'getFileMetadata',
    g_variant_new('(s)', [PAnsiChar(ARelPath)]), ErrMsg);
  if Res = nil then
    Exit;
  try
    TupleV := g_variant_get_child_value(Res, 0);
    if Assigned(TupleV) then
    try
      AInfo := ParseKmtpFileTuple(TupleV);
      Result := (AInfo.ItemId <> 0);
    finally
      g_variant_unref(TupleV);
    end;
  finally
    g_variant_unref(Res);
  end;
end;

function KmtpCreateFolder(const AStoragePath, ARelPath: String): Boolean;
var
  Res, Child: PGVariant;
  ErrMsg: String;
begin
  Result := False;
  Res := KmtpCallSync(AStoragePath, KMTPD_STORAGE_INTERFACE, 'createFolder',
    g_variant_new('(s)', [PAnsiChar(ARelPath)]), ErrMsg);
  if Res = nil then
    Exit;
  try
    Child := g_variant_get_child_value(Res, 0);
    if Assigned(Child) then
    try
      Result := g_variant_get_uint32(Child) <> 0;
    finally
      g_variant_unref(Child);
    end;
  finally
    g_variant_unref(Res);
  end;
end;

function KmtpDeleteObject(const AStoragePath, ARelPath: String): Boolean;
var
  Res, Child: PGVariant;
  ErrMsg: String;
begin
  Result := False;
  Res := KmtpCallSync(AStoragePath, KMTPD_STORAGE_INTERFACE, 'deleteObject',
    g_variant_new('(s)', [PAnsiChar(ARelPath)]), ErrMsg);
  if Res = nil then
    Exit;
  try
    Child := g_variant_get_child_value(Res, 0);
    if Assigned(Child) then
    try
      Result := g_variant_get_int32(Child) = 0;
    finally
      g_variant_unref(Child);
    end;
  finally
    g_variant_unref(Res);
  end;
end;

function KmtpSetFileName(const AStoragePath, ARelPath, ANewName: String): Boolean;
var
  Res, Child: PGVariant;
  ErrMsg: String;
begin
  Result := False;
  Res := KmtpCallSync(AStoragePath, KMTPD_STORAGE_INTERFACE, 'setFileName',
    g_variant_new('(ss)', [PAnsiChar(ARelPath), PAnsiChar(ANewName)]), ErrMsg);
  if Res = nil then
    Exit;
  try
    Child := g_variant_get_child_value(Res, 0);
    if Assigned(Child) then
    try
      Result := g_variant_get_int32(Child) = 0;
    finally
      g_variant_unref(Child);
    end;
  finally
    g_variant_unref(Res);
  end;
end;

procedure OnKmtpCopyProgress(connection: PGDBusConnection;
  sender_name: Pgchar; object_path: Pgchar; interface_name: Pgchar;
  signal_name: Pgchar; parameters: PGVariant; user_data: gpointer); cdecl;
var
  State: PKmtpTransferState absolute user_data;
  V: PGVariant;
begin
  if (State = nil) or (parameters = nil) then
    Exit;
  if g_variant_n_children(parameters) >= 2 then
  begin
    V := g_variant_get_child_value(parameters, 0);
    if Assigned(V) then begin State^.Transferred := g_variant_get_uint64(V); g_variant_unref(V); end;
    V := g_variant_get_child_value(parameters, 1);
    if Assigned(V) then begin State^.Total := g_variant_get_uint64(V); g_variant_unref(V); end;
  end;
end;

procedure OnKmtpCopyFinished(connection: PGDBusConnection;
  sender_name: Pgchar; object_path: Pgchar; interface_name: Pgchar;
  signal_name: Pgchar; parameters: PGVariant; user_data: gpointer); cdecl;
var
  State: PKmtpTransferState absolute user_data;
  V: PGVariant;
begin
  if State = nil then
    Exit;
  if Assigned(parameters) and (g_variant_n_children(parameters) >= 1) then
  begin
    V := g_variant_get_child_value(parameters, 0);
    if Assigned(V) then
    begin
      State^.ResultCode := g_variant_get_int32(V);
      g_variant_unref(V);
    end;
  end;
  State^.Finished := True;
end;

function KmtpTransferFileFD(const AStoragePath, ARelPath, ALocalFile: String;
  AIsUpload: Boolean; ATotalBytes: UInt64; AProgress: TKmtpTransferProgressCallback;
  out AErrorMsg: String): Boolean;
var
  Conn: PGDBusConnection;
  Ctx: PGMainContext;
  Handle: cint;
  FdIdx: gint32;
  FDList: PGUnixFDList;
  Res, Child: PGVariant;
  AError: PGError = nil;
  SubProgress: guint = 0;
  SubFinished: guint = 0;
  MethodName: PAnsiChar;
  State: TKmtpTransferState;
  DispatchRes: gint32;
begin
  Result := False;
  AErrorMsg := '';
  Conn := EnsureKmtpDBusConn;
  if Conn = nil then
  begin
    AErrorMsg := 'KMTP D-Bus daemon is not available';
    Exit;
  end;

  if AIsUpload then
  begin
    repeat
      Handle := fpOpen(CeUtf8ToSys(ALocalFile), O_RDONLY or O_CLOEXEC);
    until (Handle <> -1) or (fpgeterrno <> ESysEINTR);
    MethodName := 'sendFileFromFileDescriptor';
  end
  else
  begin
    repeat
      Handle := fpOpen(CeUtf8ToSys(ALocalFile), O_WRONLY or O_CREAT or O_TRUNC or O_CLOEXEC, $1B6);
    until (Handle <> -1) or (fpgeterrno <> ESysEINTR);
    MethodName := 'getFileToFileDescriptor';
  end;

  if Handle < 0 then
  begin
    AErrorMsg := SysErrorMessage(fpgeterrno);
    Exit;
  end;

  FDList := g_unix_fd_list_new();
  FdIdx := g_unix_fd_list_append(FDList, Handle, @AError);
  fpClose(Handle);
  if (FdIdx < 0) or Assigned(AError) then
  begin
    if Assigned(AError) then
    begin
      AErrorMsg := StrPas(AError^.message);
      g_error_free(AError);
    end
    else
      AErrorMsg := 'Failed to append file descriptor';
    g_object_unref(PGObject(FDList));
    Exit;
  end;

  State := Default(TKmtpTransferState);
  State.Total := ATotalBytes;
  State.ResultCode := -1;

  Ctx := g_main_context_new();
  g_main_context_push_thread_default(Ctx);
  try
    SubProgress := g_dbus_connection_signal_subscribe(
      Conn, nil, KMTPD_STORAGE_INTERFACE, 'copyProgress',
      PAnsiChar(AStoragePath), nil, G_DBUS_SIGNAL_FLAGS_NONE,
      @OnKmtpCopyProgress, @State, nil);

    SubFinished := g_dbus_connection_signal_subscribe(
      Conn, nil, KMTPD_STORAGE_INTERFACE, 'copyFinished',
      PAnsiChar(AStoragePath), nil, G_DBUS_SIGNAL_FLAGS_NONE,
      @OnKmtpCopyFinished, @State, nil);

    Res := g_dbus_connection_call_with_unix_fd_list_sync(
      Conn,
      KMTPD_BUS_NAME,
      PAnsiChar(AStoragePath),
      KMTPD_STORAGE_INTERFACE,
      MethodName,
      g_variant_new('(hs)', [FdIdx, PAnsiChar(ARelPath)]),
      nil,
      G_DBUS_CALL_FLAGS_NONE,
      KMTPD_TIMEOUT,
      FDList,
      nil,
      nil,
      @AError);

    if Assigned(AError) then
    begin
      AErrorMsg := StrPas(AError^.message);
      g_error_free(AError);
      Exit(False);
    end;

    if Res = nil then
    begin
      AErrorMsg := 'Empty D-Bus reply from kmtpd';
      Exit(False);
    end;

    DispatchRes := -1;
    try
      Child := g_variant_get_child_value(Res, 0);
      if Assigned(Child) then
      begin
        DispatchRes := g_variant_get_int32(Child);
        g_variant_unref(Child);
      end;
    finally
      g_variant_unref(Res);
    end;

    if DispatchRes <> 0 then
    begin
      AErrorMsg := Format('KMTP transfer failed to start (code %d)', [DispatchRes]);
      Exit(False);
    end;

    while not State.Finished do
    begin
      while g_main_context_pending(Ctx) do
        g_main_context_iteration(Ctx, False);

      if Assigned(AProgress) then
      begin
        if not AProgress(State.Transferred, State.Total) then
          Exit(False);
      end;

      if not State.Finished then
        Sleep(10);
    end;

    Result := (State.ResultCode = 0);
    if Result then
    begin
      if Assigned(AProgress) then
        AProgress(ATotalBytes, ATotalBytes);
    end
    else
      AErrorMsg := Format('KMTP transfer failed (code %d)', [State.ResultCode]);
  finally
    if SubProgress <> 0 then
      g_dbus_connection_signal_unsubscribe(Conn, SubProgress);
    if SubFinished <> 0 then
      g_dbus_connection_signal_unsubscribe(Conn, SubFinished);
    g_main_context_pop_thread_default(Ctx);
    g_main_context_unref(Ctx);
    g_object_unref(PGObject(FDList));
    if (not Result) and (not AIsUpload) and mbFileExists(ALocalFile) then
      mbDeleteFile(ALocalFile);
  end;
end;

procedure KmtpInitialize(AOnChanged: TNotifyEvent);
begin
  OnKmtpChanged := AOnChanged;
  KmtpInitialized := True;
  if IsKmtpActive then
    EnsureKmtpDBusConn;
end;

procedure KmtpFinalize;
begin
  KmtpLock.Acquire;
  try
    KmtpInitialized := False;
    OnKmtpChanged := nil;
    if Assigned(KmtpDBusConn) then
    begin
      if (KmtpDevicesSubId <> 0) and Assigned(g_dbus_connection_signal_unsubscribe) then
      begin
        g_dbus_connection_signal_unsubscribe(KmtpDBusConn, KmtpDevicesSubId);
        KmtpDevicesSubId := 0;
      end;
      g_object_unref(PGObject(KmtpDBusConn));
      KmtpDBusConn := nil;
    end;
  finally
    KmtpLock.Release;
  end;
end;

function KmtpEnumerateVolumes(DrivesList: TDrivesList): Boolean;
var
  Devices: TKmtpDeviceArray;
  I, J: Integer;
  DrivePath: String;
  AlreadyExists: Boolean;
  Drive: PDrive;
begin
  Result := False;
  if not IsKmtpActive then
    Exit;

  Devices := KmtpListDevices;
  if Length(Devices) = 0 then
    Exit;

  for I := 0 to High(Devices) do
  begin
    if Devices[I].FriendlyName = '' then
      Continue;

    DrivePath := 'mtp://' + EncodeMtpHost(Devices[I].FriendlyName) + '/';
    AlreadyExists := False;
    for J := 0 to DrivesList.Count - 1 do
    begin
      if (DrivesList[J]^.Path = DrivePath) or
         ((Devices[I].Udi <> '') and (DrivesList[J]^.DeviceId = Devices[I].Udi)) then
      begin
        AlreadyExists := True;
        Break;
      end;
    end;

    if not AlreadyExists then
    begin
      New(Drive);
      FillChar(Drive^, SizeOf(TDrive), 0);
      Drive^.DisplayName := Devices[I].FriendlyName;
      Drive^.DriveLabel := Devices[I].FriendlyName;
      Drive^.DeviceId := Devices[I].Udi;
      Drive^.Path := DrivePath;
      Drive^.DriveType := dtSpecial;
      Drive^.IsMounted := True;
      Drive^.IsMediaAvailable := True;
      DrivesList.Add(Drive);
      Result := True;
    end;
  end;
end;

function KmtpResolvePath(var APath: String): Boolean;
var
  Udi, Rest, HostPart: String;
  PosIdx, SlashPos: Integer;
  Devices: TKmtpDeviceArray;
  I: Integer;
  MatchedName: String;
begin
  Result := False;
  if not IsKmtpActive then
    Exit;

  // Handle kio-fuse mtp udi paths, e.g. /run/user/1000/kio-fuse-.../mtp/udi=/org/kde/solid/...
  PosIdx := Pos('/mtp/udi=/', APath);
  if PosIdx > 0 then
    Udi := Copy(APath, PosIdx + Length('/mtp/udi='), MaxInt)
  else if StrBegins(APath, 'mtp:udi=') then
    Udi := Copy(APath, Length('mtp:udi=') + 1, MaxInt)
  else if StrBegins(APath, 'mtp://udi=') then
    Udi := Copy(APath, Length('mtp://udi=') + 1, MaxInt)
  else
    Udi := '';

  if Udi <> '' then
  begin
    Udi := ExcludeTrailingPathDelimiter(Udi);
    if StrEnds(Udi, '/.') then
      Delete(Udi, Length(Udi) - 1, 2);
    Udi := ExcludeTrailingPathDelimiter(Udi);

    Devices := KmtpListDevices;
    MatchedName := '';
    for I := 0 to High(Devices) do
    begin
      if (Devices[I].Udi = Udi) or (Pos(Devices[I].Udi + '/', Udi) = 1) or
         (Pos(Udi + '/', Devices[I].Udi) = 1) then
      begin
        MatchedName := Devices[I].FriendlyName;
        Break;
      end;
    end;
    if (MatchedName = '') and (Length(Devices) = 1) then
      MatchedName := Devices[0].FriendlyName;

    if MatchedName <> '' then
      APath := 'mtp://' + EncodeMtpHost(MatchedName) + '/'
    else
      APath := 'mtp:///';
    Exit(True);
  end;

  if (APath = 'mtp:') or (APath = 'mtp:/') or (APath = 'mtp://') or (APath = 'mtp:///') then
  begin
    Devices := KmtpListDevices;
    if (Length(Devices) = 1) and (Devices[0].FriendlyName <> '') then
      APath := 'mtp://' + EncodeMtpHost(Devices[0].FriendlyName) + '/'
    else
      APath := 'mtp:///';
    Exit(True);
  end;

  if StrBegins(APath, 'mtp:/') and (not StrBegins(APath, 'mtp://')) then
  begin
    Rest := Copy(APath, 6, MaxInt);
    SlashPos := Pos('/', Rest);
    if SlashPos = 0 then
      APath := 'mtp://' + EncodeMtpHost(Rest) + '/'
    else
    begin
      HostPart := Copy(Rest, 1, SlashPos - 1);
      APath := 'mtp://' + EncodeMtpHost(HostPart) + Copy(Rest, SlashPos, MaxInt);
    end;
    Exit(True);
  end;

  if StrBegins(APath, 'mtp://') then
  begin
    Rest := Copy(APath, 7, MaxInt);
    if Pos('/', Rest) = 0 then
      APath := APath + '/';
    Exit(True);
  end;
end;

procedure Initialize;
begin
  if (DesktopEnv = DE_KDE) then
  begin
    KdeVersion:= GetEnvironmentVariable('KDE_SESSION_VERSION');
    if KdeVersion = '5' then KdeOpen:= 'kioclient5';
    HasKdeOpen:= FindExecutableInSystemPath(KdeOpen);
    // if HasKdeOpen then FileTrashUtf8:= @FileTrash;
  end;
end;

initialization
  KmtpLock := TCriticalSection.Create;
  RegisterInitialization(@Initialize);

finalization
  KmtpFinalize;
  FreeAndNil(KmtpLock);

end.
