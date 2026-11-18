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
  Classes, SysUtils, URIParser, DCStrUtils, uMyUnix, uDrive, uFile, uFileProperty,
  uFileSource, uFileSourceProperty, uRealFileSource, uFileSourceOperation,
  uFileSourceOperationTypes;

type
  TKmtpChangedNotify = procedure;

  { IKmtpFileSource }

  IKmtpFileSource = interface(IRealFileSource)
    ['{8B7C2D4E-4A91-4B3C-9F1E-2C6D8E4A1B7F}']
    function ResolveTarget(const APath: String;
                           out ADevicePath, AStoragePath, ARelPath: String;
                           out ALevel: Integer): Boolean;
    function GetDeviceName: String;
  end;

  { TKmtpFileSource }

  TKmtpFileSource = class(TRealFileSource, IKmtpFileSource)
  private
    FDeviceName: String;
  protected
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;
  public
    constructor Create; override;
    constructor Create(const URI: TURI); override;

    class function IsSupportedPath(const Path: String): Boolean; override;
    class function CreateFile(const APath: String): TFile; override;

    function ResolveTarget(const APath: String;
                           out ADevicePath, AStoragePath, ARelPath: String;
                           out ALevel: Integer): Boolean;
    function GetDeviceName: String;

    function GetPathType(sPath: String): TPathType; override;
    function GetFileSystem: String; override;
    function GetProperties: TFileSourceProperties; override;
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetRetrievableFileProperties: TFilePropertiesTypes; override;
    function GetOperationsTypes: TFileSourceOperationTypes; override;

    function FileSystemEntryExists(const Path: String; const Options: TFileSourceExistsOptions): TFileSourceExistsResult; override;
    function CreateDirectory(const Path: String): Boolean; override;
    function GetFreeSpace(Path: String; out FreeSize, TotalSize: Int64): Boolean; override;
    procedure Reload(const PathsToReload: TPathsArray); override;

    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCopyInOperation(SourceFileSource: IFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: IFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; override;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; override;
    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation; override;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation; override;
  end;

function KioOpen(const URL: String): Boolean;

procedure KmtpInitialize(AOnChanged: TKmtpChangedNotify);
procedure KmtpFinalize;
function KmtpEnumerateVolumes(DrivesList: TDrivesList): Boolean;
function KmtpResolvePath(var APath: String): Boolean;

var
  HasKdeOpen: Boolean = False;

implementation

uses
  BaseUnix, CTypes, Forms, Dialogs, DateUtils, Math, LazLogger, syncobjs,
  DCBasicTypes, DCConvertEncoding, DCDateTimeUtils, DCFileAttributes, DCOSUtils,
  DCUnix, uDCUtils, uGlobs, uOSUtils, uTrash, uLog, uLng, uShowMsg, uDebug,
  uFindEx, uFileProcs,
  uGLib2, uGio2, uGObject2, uGio, uGioFileSource, uClipboard,
  uFileSourceListOperation, uFileSourceCopyOperation, uFileSourceDeleteOperation,
  uFileSourceCreateDirectoryOperation, uFileSourceExecuteOperation,
  uFileSourceSetFilePropertyOperation, uFileSourceCalcStatisticsOperation,
  uFileSourceOperationOptions, uFileSourceOperationUI, uFileSourceTreeBuilder,
  uFileSystemUtil, uTempFileSystemFileSource, fFileProperties;

const
  KMTPD_BUS_NAME         = 'org.kde.kmtpd5';
  KMTPD_DAEMON_PATH      = '/modules/kmtpd';
  KMTPD_DAEMON_INTERFACE = 'org.kde.kmtp.Daemon';
  KMTPD_DEVICE_INTERFACE = 'org.kde.kmtp.Device';
  KMTPD_STORAGE_INTERFACE = 'org.kde.kmtp.Storage';
  DBUS_PROPERTIES_IFACE  = 'org.freedesktop.DBus.Properties';
  KMTPD_TIMEOUT          = 30000;
  KMTP_MIME_DIRECTORY    = 'inode/directory';

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

  PKmtpTransferState = ^TKmtpTransferState;
  TKmtpTransferState = record
    Transferred: UInt64;
    Total: UInt64;
    ResultCode: gint32;
    Finished: Boolean;
  end;

  TKmtpTransferProgressCallback = function(Transferred, Total: UInt64): Boolean of object;

var
  KdeVersion: String;
  KdeOpen: String = 'kioclient';

  KmtpLock: TCriticalSection = nil;
  KmtpDBusConn: PGDBusConnection = nil;
  KmtpInitialized: Boolean = False;
  KmtpUnavailable: Boolean = False;
  KmtpDevicesSubId: guint = 0;
  OnKmtpChanged: TKmtpChangedNotify = nil;

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

function SplitKmtpSegments(const APath: String): TStringArray;
var
  I, StartPos, Len: Integer;
  Count: Integer = 0;
  Part: String;
begin
  Result := nil;
  Len := Length(APath);
  I := 1;
  while I <= Len do
  begin
    while (I <= Len) and (APath[I] = '/') do
      Inc(I);
    if I > Len then
      Break;
    StartPos := I;
    while (I <= Len) and (APath[I] <> '/') do
      Inc(I);
    Part := Copy(APath, StartPos, I - StartPos);
    if (Part <> '') and (Part <> '.') then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := Part;
      Inc(Count);
    end;
  end;
end;

function JoinKmtpStoragePath(const Segments: TStringArray; StartIndex: Integer): String;
var
  I: Integer;
begin
  Result := '';
  for I := StartIndex to High(Segments) do
    Result := Result + '/' + Segments[I];
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
  if (DesktopEnv <> DE_KDE) and TGioFileSource.IsSupportedPath('mtp://') then
    Exit(False);
  Result := True;
end;

procedure OnKmtpDevicesChanged(connection: PGDBusConnection;
  sender_name: Pgchar; object_path: Pgchar; interface_name: Pgchar;
  signal_name: Pgchar; parameters: PGVariant; user_data: gpointer); cdecl;
begin
  if Assigned(OnKmtpChanged) then
    OnKmtpChanged();
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

function FindDeviceByName(const AName: String; out ADevice: TKmtpDeviceInfo): Boolean;
var
  Devices: TKmtpDeviceArray;
  I: Integer;
begin
  Result := False;
  ADevice := Default(TKmtpDeviceInfo);
  Devices := KmtpListDevices;
  for I := 0 to High(Devices) do
  begin
    if SameText(Devices[I].FriendlyName, AName) then
    begin
      ADevice := Devices[I];
      Exit(True);
    end;
  end;
end;

function FindStorageByDesc(const ADevicePath, ADesc: String; out AStorage: TKmtpStorageInfo): Boolean;
var
  Storages: TKmtpStorageArray;
  I: Integer;
begin
  Result := False;
  AStorage := Default(TKmtpStorageInfo);
  Storages := KmtpListStorages(ADevicePath);
  for I := 0 to High(Storages) do
  begin
    if SameText(Storages[I].Description, ADesc) then
    begin
      AStorage := Storages[I];
      Exit(True);
    end;
  end;
end;

function MakeKmtpFileFromInfo(const APath: String; const AInfo: TKmtpFileInfo): TFile;
begin
  Result := TKmtpFileSource.CreateFile(APath);
  Result.Name := AInfo.FileName;
  if AInfo.FileType = KMTP_MIME_DIRECTORY then
  begin
    Result.Size := 0;
    Result.Attributes := S_IFDIR or $01ED;
  end
  else
  begin
    Result.Size := Int64(AInfo.FileSize);
    Result.Attributes := S_IFREG or $01A4;
  end;
  if AInfo.ModDate > 0 then
    Result.ModificationTime := UnixFileTimeToDateTime(TUnixFileTime(AInfo.ModDate));
end;

procedure KmtpInitialize(AOnChanged: TKmtpChangedNotify);
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

type

  { TKmtpTreeBuilder }

  TKmtpTreeBuilder = class(TFileSourceTreeBuilder)
  private
    FKmtpFileSource: IKmtpFileSource;
  protected
    procedure AddLinkTarget(aFile: TFile; CurrentNode: TFileTreeNode); override;
    procedure AddFilesInDirectory(srcPath: String; CurrentNode: TFileTreeNode); override;
  public
    constructor Create(AKmtpFileSource: IKmtpFileSource;
                       AskQuestionFunction: TAskQuestionFunction;
                       CheckOperationStateFunction: TCheckOperationStateFunction);
  end;

  { TKmtpListOperation }

  TKmtpListOperation = class(TFileSourceListOperation)
  private
    FKmtpFileSource: IKmtpFileSource;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

  { TKmtpCopyInOperation }

  TKmtpCopyInOperation = class(TFileSourceCopyInOperation)
  private
    FKmtpFileSource: IKmtpFileSource;
    FSourceFilesTree: TFileTree;
    FStatistics: TFileSourceCopyOperationStatistics;
    FRenameNameMask, FRenameExtMask: String;
    FRenamingFiles, FRenamingRootDir: Boolean;
    FRootDir: TFile;
    FOldDoneBytes: Int64;
    FSkipAnyError: Boolean;
    function OnProgress(Transferred, Total: UInt64): Boolean;
    procedure LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
    function EnsureTargetFolder(const AStoragePath, ARelPath: String): Boolean;
    function CheckTargetDirExists(const AStoragePath, ARelPath, AFullTarget: String): TFileSourceOperationOptionDirectoryExists;
    function CheckTargetFileExists(aSourceFile: TFile; const AStoragePath: String;
      var ARelPath, AFullTarget: String): TFileSourceOperationOptionFileExists;
    function ProcessNode(aNode: TFileTreeNode; const CurrentTargetPath: String): Boolean;
    function ProcessDirectory(aNode: TFileTreeNode; const AbsoluteTargetFileName: String): Boolean;
    function ProcessFile(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
    procedure CountStatistics(aNode: TFileTreeNode);
  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure MainExecute; override;
  end;

  { TKmtpCopyOutOperation }

  TKmtpCopyOutOperation = class(TFileSourceCopyOutOperation)
  private
    FKmtpFileSource: IKmtpFileSource;
    FSourceFilesTree: TFileTree;
    FStatistics: TFileSourceCopyOperationStatistics;
    FRenameNameMask, FRenameExtMask: String;
    FRenamingFiles, FRenamingRootDir: Boolean;
    FRootDir: TFile;
    FOldDoneBytes: Int64;
    FSkipAnyError: Boolean;
    FCurrentFile: TFile;
    FCurrentTargetFilePath: String;
    function OnProgress(Transferred, Total: UInt64): Boolean;
    procedure LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
    procedure QuestionActionHandler(Action: TFileSourceOperationUIAction);
    function TargetExists(aNode: TFileTreeNode; var AbsoluteTargetFileName: String): TFileSystemOperationTargetExistsResult;
    function DirExists(aFile: TFile; const AbsoluteTargetFileName: String; AllowCopyInto: Boolean): TFileSourceOperationOptionDirectoryExists;
    function FileExists(aFile: TFile; var AbsoluteTargetFileName: String): TFileSourceOperationOptionFileExists;
    function ProcessNode(aNode: TFileTreeNode; const CurrentTargetPath: String): Boolean;
    function ProcessDirectory(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
    function ProcessFile(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
    procedure CountStatistics(aNode: TFileTreeNode);
  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure MainExecute; override;
  end;

  { TKmtpDeleteOperation }

  TKmtpDeleteOperation = class(TFileSourceDeleteOperation)
  private
    FKmtpFileSource: IKmtpFileSource;
    FFullFilesTreeToDelete: TFiles;
    FStatistics: TFileSourceDeleteOperationStatistics;
    FSkipErrors: Boolean;
    procedure FillAndCount;
    function ProcessFile(aFile: TFile): Boolean;
    function ShowError(const sMessage: String): TFileSourceOperationUIResponse;
    procedure LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToDelete: TFiles); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure MainExecute; override;
  end;

  { TKmtpCreateDirectoryOperation }

  TKmtpCreateDirectoryOperation = class(TFileSourceCreateDirectoryOperation)
  public
    procedure MainExecute; override;
  end;

  { TKmtpExecuteOperation }

  TKmtpExecuteOperation = class(TFileSourceExecuteOperation)
  public
    procedure MainExecute; override;
  end;

  { TKmtpCalcStatisticsOperation }

  TKmtpCalcStatisticsOperation = class(TFileSourceCalcStatisticsOperation)
  private
    FKmtpFileSource: IKmtpFileSource;
    FStatistics: TFileSourceCalcStatisticsOperationStatistics;
    procedure ProcessFile(aFile: TFile);
    procedure ProcessSubDirs(const srcPath: String);
  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFiles: TFiles); override;
    procedure Initialize; override;
    procedure MainExecute; override;
  end;

  { TKmtpSetFilePropertyOperation }

  TKmtpSetFilePropertyOperation = class(TFileSourceSetFilePropertyOperation)
  private
    FKmtpFileSource: IKmtpFileSource;
    FFullFilesTree: TFiles;
    FStatistics: TFileSourceSetFilePropertyOperationStatistics;
  protected
    function SetNewProperty(aFile: TFile; aTemplateProperty: TFileProperty): TSetFilePropertyResult; override;
  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theTargetFiles: TFiles;
                       var theNewProperties: TFileProperties); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure MainExecute; override;
  end;

{ TKmtpTreeBuilder }

constructor TKmtpTreeBuilder.Create(AKmtpFileSource: IKmtpFileSource;
  AskQuestionFunction: TAskQuestionFunction;
  CheckOperationStateFunction: TCheckOperationStateFunction);
begin
  inherited Create(AskQuestionFunction, CheckOperationStateFunction);
  FKmtpFileSource := AKmtpFileSource;
end;

procedure TKmtpTreeBuilder.AddLinkTarget(aFile: TFile; CurrentNode: TFileTreeNode);
begin
  AddFile(aFile, CurrentNode);
end;

procedure TKmtpTreeBuilder.AddFilesInDirectory(srcPath: String; CurrentNode: TFileTreeNode);
var
  DevPath, StorPath, RelPath: String;
  Level, I, ExistsCode: Integer;
  Storages: TKmtpStorageArray;
  Files: TKmtpFileArray;
  AFile: TFile;
begin
  if not FKmtpFileSource.ResolveTarget(srcPath, DevPath, StorPath, RelPath, Level) then
    Exit;

  if Level = 1 then
  begin
    Storages := KmtpListStorages(DevPath);
    for I := 0 to High(Storages) do
    begin
      CheckOperationState;
      AFile := TKmtpFileSource.CreateFile(srcPath);
      AFile.Name := Storages[I].Description;
      AFile.Size := 0;
      AFile.Attributes := S_IFDIR or $01ED;
      AddItem(AFile, CurrentNode);
    end;
  end
  else if Level >= 2 then
  begin
    if KmtpGetFilesAndFolders(StorPath, RelPath, Files, ExistsCode) and (ExistsCode = 0) then
    begin
      for I := 0 to High(Files) do
      begin
        CheckOperationState;
        if (Files[I].FileName = '') or (Files[I].FileName = '.') or (Files[I].FileName = '..') then
          Continue;
        AFile := MakeKmtpFileFromInfo(srcPath, Files[I]);
        AddItem(AFile, CurrentNode);
      end;
    end;
  end;
end;

{ TKmtpListOperation }

constructor TKmtpListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FKmtpFileSource := aFileSource as IKmtpFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TKmtpListOperation.MainExecute;
var
  DevPath, StorPath, RelPath: String;
  Level, I, ExistsCode: Integer;
  Devices: TKmtpDeviceArray;
  Storages: TKmtpStorageArray;
  KmtpFiles: TKmtpFileArray;
  AFile: TFile;
begin
  FFiles.Clear;
  if not FKmtpFileSource.ResolveTarget(Path, DevPath, StorPath, RelPath, Level) then
    Exit;

  case Level of
    0:
      begin
        Devices := KmtpListDevices;
        for I := 0 to High(Devices) do
        begin
          CheckOperationState;
          if Devices[I].FriendlyName = '' then
            Continue;
          AFile := TKmtpFileSource.CreateFile(Path);
          AFile.Name := Devices[I].FriendlyName;
          AFile.Size := 0;
          AFile.Attributes := S_IFDIR or $01ED;
          FFiles.Add(AFile);
        end;
      end;
    1:
      begin
        Storages := KmtpListStorages(DevPath);
        for I := 0 to High(Storages) do
        begin
          CheckOperationState;
          AFile := TKmtpFileSource.CreateFile(Path);
          AFile.Name := Storages[I].Description;
          AFile.Size := 0;
          AFile.Attributes := S_IFDIR or $01ED;
          FFiles.Add(AFile);
        end;
      end;
    2:
      begin
        if KmtpGetFilesAndFolders(StorPath, RelPath, KmtpFiles, ExistsCode) and (ExistsCode = 0) then
        begin
          for I := 0 to High(KmtpFiles) do
          begin
            CheckOperationState;
            if (KmtpFiles[I].FileName = '') or (KmtpFiles[I].FileName = '.') or (KmtpFiles[I].FileName = '..') then
              Continue;
            AFile := MakeKmtpFileFromInfo(Path, KmtpFiles[I]);
            FFiles.Add(AFile);
          end;
        end;
      end;
  end;
end;

{ TKmtpCopyInOperation }

constructor TKmtpCopyInOperation.Create(aSourceFileSource: IFileSource;
  aTargetFileSource: IFileSource; var theSourceFiles: TFiles; aTargetPath: String);
begin
  FKmtpFileSource := aTargetFileSource as IKmtpFileSource;
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
  FFileExistsOption := gOperationOptionFileExists;
  FDirExistsOption := gOperationOptionDirectoryExists;
end;

destructor TKmtpCopyInOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSourceFilesTree);
end;

procedure TKmtpCopyInOperation.Initialize;
var
  TreeBuilder: TFileSystemTreeBuilder;
  aFile: TFile;
begin
  FStatistics := RetrieveStatistics;
  TreeBuilder := TFileSystemTreeBuilder.Create(@AskQuestion, @CheckOperationState);
  try
    TreeBuilder.SymLinkOption := fsooslFollow;
    TreeBuilder.BuildFromFiles(SourceFiles);
    FSourceFilesTree := TreeBuilder.ReleaseTree;
    FStatistics.TotalFiles := TreeBuilder.FilesCount;
    FStatistics.TotalBytes := TreeBuilder.FilesSize;
  finally
    FreeAndNil(TreeBuilder);
  end;

  SplitFileMask(RenameMask, FRenameNameMask, FRenameExtMask);
  FRenamingFiles := (RenameMask <> '*.*') and (RenameMask <> '');
  FRenamingRootDir := False;
  if (FSourceFilesTree.SubNodesCount = 1) and FRenamingFiles then
  begin
    aFile := FSourceFilesTree.SubNodes[0].TheFile;
    if (aFile.IsDirectory or aFile.IsLinkToDirectory) and (not ContainsWildcards(RenameMask)) then
    begin
      FRenamingFiles := False;
      FRenamingRootDir := True;
      FRootDir := aFile;
    end;
  end;
end;

procedure TKmtpCopyInOperation.MainExecute;
begin
  ProcessNode(FSourceFilesTree, FTargetPath);
end;

function TKmtpCopyInOperation.OnProgress(Transferred, Total: UInt64): Boolean;
begin
  if State = fsosStopping then
    Exit(False);
  FStatistics.CurrentFileDoneBytes := Int64(Transferred);
  if Total > 0 then
    FStatistics.CurrentFileTotalBytes := Int64(Total);
  FStatistics.DoneBytes := FOldDoneBytes + Int64(Transferred);
  UpdateStatistics(FStatistics);
  CheckOperationState;
  Result := True;
end;

procedure TKmtpCopyInOperation.LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
begin
  case logMsgType of
    lmtError:   if not (log_errors in gLogOptions) then Exit;
    lmtInfo:    if not (log_info in gLogOptions) then Exit;
    lmtSuccess: if not (log_success in gLogOptions) then Exit;
  end;
  if logOptions <= gLogOptions then
    logWrite(Thread, sMessage, logMsgType);
end;

function TKmtpCopyInOperation.EnsureTargetFolder(const AStoragePath, ARelPath: String): Boolean;
var
  Meta: TKmtpFileInfo;
begin
  if (ARelPath = '') or (ARelPath = '/') then
    Exit(True);
  if KmtpGetFileMetadata(AStoragePath, ARelPath, Meta) then
    Exit(Meta.FileType = KMTP_MIME_DIRECTORY);
  Result := KmtpCreateFolder(AStoragePath, ARelPath);
end;

function TKmtpCopyInOperation.CheckTargetDirExists(const AStoragePath, ARelPath,
  AFullTarget: String): TFileSourceOperationOptionDirectoryExists;
var
  Meta: TKmtpFileInfo;
begin
  if not KmtpGetFileMetadata(AStoragePath, ARelPath, Meta) then
    Exit(fsoodeCopyInto);
  if FDirExistsOption <> fsoodeNone then
    Exit(FDirExistsOption);
  case AskQuestion(Format(rsMsgFolderExistsRwrt, [AFullTarget]), '',
                   [fsourCopyInto, fsourCopyIntoAll, fsourSkip, fsourSkipAll, fsourCancel],
                   fsourCopyInto, fsourSkip) of
    fsourCopyInto:    Result := fsoodeCopyInto;
    fsourCopyIntoAll: begin FDirExistsOption := fsoodeCopyInto; Result := fsoodeCopyInto; end;
    fsourSkip:        Result := fsoodeSkip;
    fsourSkipAll:     begin FDirExistsOption := fsoodeSkip; Result := fsoodeSkip; end;
    else              RaiseAbortOperation;
  end;
end;

function TKmtpCopyInOperation.CheckTargetFileExists(aSourceFile: TFile; const AStoragePath: String;
  var ARelPath, AFullTarget: String): TFileSourceOperationOptionFileExists;
const
  Responses: array[0..8] of TFileSourceOperationUIResponse =
    (fsourOverwrite, fsourSkip, fsourRenameSource, fsourOverwriteAll,
     fsourSkipAll, fsourOverwriteOlder, fsourOverwriteSmaller,
     fsourCancel, fsourOverwriteLarger);
var
  Meta: TKmtpFileInfo;
  Answer: Boolean;
  Msg, NewName: String;

  function OverwriteOlder: TFileSourceOperationOptionFileExists;
  begin
    if CompareDateTime(RecodeMilliSecond(aSourceFile.ModificationTime, 0),
                       UnixFileTimeToDateTime(TUnixFileTime(Meta.ModDate))) = GreaterThanValue then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

begin
  while KmtpGetFileMetadata(AStoragePath, ARelPath, Meta) do
  begin
    case FFileExistsOption of
      fsoofeNone:
        begin
          Answer := True;
          Msg := rsMsgFileExistsOverwrite + LineEnding + AFullTarget + LineEnding +
                 Format(rsMsgFileExistsFileInfo, [IntToStrTS(Int64(Meta.FileSize)),
                        DateTimeToStr(UnixFileTimeToDateTime(TUnixFileTime(Meta.ModDate)))]) + LineEnding +
                 LineEnding + rsMsgFileExistsWithFile + LineEnding + aSourceFile.FullPath + LineEnding +
                 Format(rsMsgFileExistsFileInfo, [IntToStrTS(aSourceFile.Size), DateTimeToStr(aSourceFile.ModificationTime)]);
          case AskQuestion(Msg, '', Responses, fsourOverwrite, fsourSkip) of
            fsourOverwrite:        Exit(fsoofeOverwrite);
            fsourSkip:             Exit(fsoofeSkip);
            fsourOverwriteAll:     begin FFileExistsOption := fsoofeOverwrite; Exit(fsoofeOverwrite); end;
            fsourSkipAll:          begin FFileExistsOption := fsoofeSkip; Exit(fsoofeSkip); end;
            fsourOverwriteOlder:   begin FFileExistsOption := fsoofeOverwriteOlder; Exit(OverwriteOlder); end;
            fsourOverwriteSmaller:
              begin
                FFileExistsOption := fsoofeOverwriteSmaller;
                if aSourceFile.Size > Int64(Meta.FileSize) then Exit(fsoofeOverwrite) else Exit(fsoofeSkip);
              end;
            fsourOverwriteLarger:
              begin
                FFileExistsOption := fsoofeOverwriteLarger;
                if aSourceFile.Size < Int64(Meta.FileSize) then Exit(fsoofeOverwrite) else Exit(fsoofeSkip);
              end;
            fsourRenameSource:
              begin
                NewName := ExtractFileName(AFullTarget);
                Answer := ShowInputQuery(Thread, Application.Title, rsEditNewFileName, NewName);
                if Answer and (NewName <> '') then
                begin
                  AFullTarget := ExtractFilePath(AFullTarget) + NewName;
                  ARelPath := ExtractFilePath(ARelPath) + NewName;
                  Continue;
                end;
              end;
            else
              RaiseAbortOperation;
          end;
        end;
      fsoofeOverwriteOlder:   Exit(OverwriteOlder);
      fsoofeOverwriteSmaller: if aSourceFile.Size > Int64(Meta.FileSize) then Exit(fsoofeOverwrite) else Exit(fsoofeSkip);
      fsoofeOverwriteLarger:  if aSourceFile.Size < Int64(Meta.FileSize) then Exit(fsoofeOverwrite) else Exit(fsoofeSkip);
      else
        Exit(FFileExistsOption);
    end;
  end;
  Result := fsoofeOverwrite;
end;

function TKmtpCopyInOperation.ProcessNode(aNode: TFileTreeNode; const CurrentTargetPath: String): Boolean;
var
  I: Integer;
  SubNode: TFileTreeNode;
  aFile: TFile;
  TargetName: String;
begin
  Result := True;
  for I := 0 to aNode.SubNodesCount - 1 do
  begin
    SubNode := aNode.SubNodes[I];
    aFile := SubNode.TheFile;

    if FRenamingRootDir and (aFile = FRootDir) then
      TargetName := CurrentTargetPath + RenameMask
    else if FRenamingFiles then
      TargetName := CurrentTargetPath + ApplyRenameMask(SourceFileSource, aFile, FRenameNameMask, FRenameExtMask)
    else
      TargetName := CurrentTargetPath + aFile.Name;

    FStatistics.CurrentFileFrom := aFile.FullPath;
    FStatistics.CurrentFileTo := TargetName;
    FStatistics.CurrentFileTotalBytes := aFile.Size;
    FStatistics.CurrentFileDoneBytes := 0;
    UpdateStatistics(FStatistics);

    if aFile.IsDirectory then
    begin
      if not ProcessDirectory(SubNode, TargetName) then
        Result := False;
    end
    else
    begin
      if not ProcessFile(SubNode, TargetName) then
        Result := False;
    end;
    CheckOperationState;
  end;
end;

function TKmtpCopyInOperation.ProcessDirectory(aNode: TFileTreeNode; const AbsoluteTargetFileName: String): Boolean;
var
  DevPath, StorPath, RelPath: String;
  Level: Integer;
  Meta: TKmtpFileInfo;
begin
  if (not FKmtpFileSource.ResolveTarget(AbsoluteTargetFileName, DevPath, StorPath, RelPath, Level)) or
     (Level < 2) or (StorPath = '') then
  begin
    if not FSkipAnyError then
      if AskQuestion(rsMsgErrNotSupported, '', [fsourSkip, fsourAbort], fsourSkip, fsourAbort) = fsourAbort then
        RaiseAbortOperation;
    CountStatistics(aNode);
    Exit(False);
  end;

  if KmtpGetFileMetadata(StorPath, RelPath, Meta) then
  begin
    if CheckTargetDirExists(StorPath, RelPath, AbsoluteTargetFileName) = fsoodeSkip then
    begin
      CountStatistics(aNode);
      Exit(False);
    end;
  end
  else if not EnsureTargetFolder(StorPath, RelPath) then
  begin
    LogMessage(rsMsgLogError + Format(rsMsgErrForceDir, [AbsoluteTargetFileName]), [log_vfs_op], lmtError);
    if not FSkipAnyError then
      if AskQuestion(Format(rsMsgErrForceDir, [AbsoluteTargetFileName]), '', [fsourSkip, fsourAbort], fsourSkip, fsourAbort) = fsourAbort then
        RaiseAbortOperation;
    CountStatistics(aNode);
    Exit(False);
  end;

  Result := ProcessNode(aNode, IncludeTrailingPathDelimiter(AbsoluteTargetFileName));
end;

function TKmtpCopyInOperation.ProcessFile(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
var
  DevPath, StorPath, RelPath, ErrMsg: String;
  Level: Integer;
  Meta: TKmtpFileInfo;
  Opt: TFileSourceOperationOptionFileExists;
begin
  Result := False;
  FOldDoneBytes := FStatistics.DoneBytes;

  if (not FKmtpFileSource.ResolveTarget(AbsoluteTargetFileName, DevPath, StorPath, RelPath, Level)) or
     (Level < 2) or (StorPath = '') or (RelPath = '') then
  begin
    if not FSkipAnyError then
      if AskQuestion(rsMsgErrNotSupported, '', [fsourSkip, fsourAbort], fsourSkip, fsourAbort) = fsourAbort then
        RaiseAbortOperation;
    Exit(False);
  end;

  Opt := CheckTargetFileExists(aNode.TheFile, StorPath, RelPath, AbsoluteTargetFileName);
  if Opt = fsoofeSkip then
  begin
    FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
    FStatistics.DoneBytes := FOldDoneBytes + aNode.TheFile.Size;
    UpdateStatistics(FStatistics);
    Exit(True);
  end;

  if KmtpGetFileMetadata(StorPath, RelPath, Meta) then
    KmtpDeleteObject(StorPath, RelPath);

  repeat
    Result := KmtpTransferFileFD(StorPath, RelPath, aNode.TheFile.FullPath, True,
                                 UInt64(Max(Int64(0), aNode.TheFile.Size)), @OnProgress, ErrMsg);
    if Result then
      Break;
    if State = fsosStopping then
      RaiseAbortOperation;
    if FSkipAnyError then
      Break;
    case AskQuestion(ErrMsg, '', [fsourRetry, fsourSkip, fsourSkipAll, fsourCancel], fsourRetry, fsourCancel) of
      fsourRetry:   ;
      fsourSkip:    Break;
      fsourSkipAll: begin FSkipAnyError := True; Break; end;
      else          RaiseAbortOperation;
    end;
  until False;

  if Result then
    LogMessage(Format(rsMsgLogSuccess + rsMsgLogCopy, [aNode.TheFile.FullPath + ' -> ' + AbsoluteTargetFileName]), [log_vfs_op], lmtSuccess)
  else
    LogMessage(Format(rsMsgLogError + rsMsgLogCopy, [aNode.TheFile.FullPath + ' -> ' + AbsoluteTargetFileName]) + LineEnding + ErrMsg, [log_vfs_op], lmtError);

  FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
  FStatistics.DoneBytes := FOldDoneBytes + aNode.TheFile.Size;
  UpdateStatistics(FStatistics);
end;

procedure TKmtpCopyInOperation.CountStatistics(aNode: TFileTreeNode);
  procedure CountNode(N: TFileTreeNode);
  var
    I: Integer;
  begin
    if N.TheFile.IsDirectory then
      for I := 0 to N.SubNodesCount - 1 do
        CountNode(N.SubNodes[I])
    else
    begin
      FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
      FStatistics.DoneBytes := FStatistics.DoneBytes + N.TheFile.Size;
    end;
  end;
begin
  CountNode(aNode);
  UpdateStatistics(FStatistics);
end;

{ TKmtpCopyOutOperation }

constructor TKmtpCopyOutOperation.Create(aSourceFileSource: IFileSource;
  aTargetFileSource: IFileSource; var theSourceFiles: TFiles; aTargetPath: String);
begin
  FKmtpFileSource := aSourceFileSource as IKmtpFileSource;
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
  FFileExistsOption := gOperationOptionFileExists;
  FDirExistsOption := gOperationOptionDirectoryExists;
end;

destructor TKmtpCopyOutOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSourceFilesTree);
end;

procedure TKmtpCopyOutOperation.Initialize;
var
  TreeBuilder: TKmtpTreeBuilder;
  aFile: TFile;
begin
  FStatistics := RetrieveStatistics;
  TreeBuilder := TKmtpTreeBuilder.Create(FKmtpFileSource, @AskQuestion, @CheckOperationState);
  try
    TreeBuilder.SymLinkOption := SymLinkOption;
    TreeBuilder.BuildFromFiles(SourceFiles);
    FSourceFilesTree := TreeBuilder.ReleaseTree;
    FStatistics.TotalFiles := TreeBuilder.FilesCount;
    FStatistics.TotalBytes := TreeBuilder.FilesSize;
  finally
    FreeAndNil(TreeBuilder);
  end;

  SplitFileMask(RenameMask, FRenameNameMask, FRenameExtMask);
  FRenamingFiles := (RenameMask <> '*.*') and (RenameMask <> '');
  FRenamingRootDir := False;
  if (FSourceFilesTree.SubNodesCount = 1) and FRenamingFiles then
  begin
    aFile := FSourceFilesTree.SubNodes[0].TheFile;
    if (aFile.IsDirectory or aFile.IsLinkToDirectory) and (not ContainsWildcards(RenameMask)) then
    begin
      FRenamingFiles := False;
      FRenamingRootDir := True;
      FRootDir := aFile;
    end;
  end;
end;

procedure TKmtpCopyOutOperation.MainExecute;
begin
  ProcessNode(FSourceFilesTree, FTargetPath);
end;

function TKmtpCopyOutOperation.OnProgress(Transferred, Total: UInt64): Boolean;
begin
  if State = fsosStopping then
    Exit(False);
  FStatistics.CurrentFileDoneBytes := Int64(Transferred);
  if Total > 0 then
    FStatistics.CurrentFileTotalBytes := Int64(Total);
  FStatistics.DoneBytes := FOldDoneBytes + Int64(Transferred);
  UpdateStatistics(FStatistics);
  CheckOperationState;
  Result := True;
end;

procedure TKmtpCopyOutOperation.LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
begin
  case logMsgType of
    lmtError:   if not (log_errors in gLogOptions) then Exit;
    lmtInfo:    if not (log_info in gLogOptions) then Exit;
    lmtSuccess: if not (log_success in gLogOptions) then Exit;
  end;
  if logOptions <= gLogOptions then
    logWrite(Thread, sMessage, logMsgType);
end;

procedure TKmtpCopyOutOperation.QuestionActionHandler(Action: TFileSourceOperationUIAction);
begin
  if Action = fsouaCompare then
    ShowCompareFilesUI(FCurrentFile, FCurrentTargetFilePath);
end;

function TKmtpCopyOutOperation.DirExists(aFile: TFile; const AbsoluteTargetFileName: String;
  AllowCopyInto: Boolean): TFileSourceOperationOptionDirectoryExists;
var
  PossibleResponses: array of TFileSourceOperationUIResponse = nil;
  DefaultOkResponse: TFileSourceOperationUIResponse;

  procedure AddResponse(R: TFileSourceOperationUIResponse);
  begin
    SetLength(PossibleResponses, Length(PossibleResponses) + 1);
    PossibleResponses[High(PossibleResponses)] := R;
  end;

begin
  if FDirExistsOption <> fsoodeNone then
    Exit(FDirExistsOption);

  if AllowCopyInto then
  begin
    AddResponse(fsourCopyInto);
    AddResponse(fsourCopyIntoAll);
    DefaultOkResponse := fsourCopyInto;
  end
  else
    DefaultOkResponse := fsourSkip;
  AddResponse(fsourSkip);
  AddResponse(fsourSkipAll);
  AddResponse(fsourCancel);

  case AskQuestion(Format(rsMsgFolderExistsRwrt, [AbsoluteTargetFileName]), '',
                   PossibleResponses, DefaultOkResponse, fsourSkip) of
    fsourCopyInto:    Result := fsoodeCopyInto;
    fsourCopyIntoAll: begin FDirExistsOption := fsoodeCopyInto; Result := fsoodeCopyInto; end;
    fsourSkip:        Result := fsoodeSkip;
    fsourSkipAll:     begin FDirExistsOption := fsoodeSkip; Result := fsoodeSkip; end;
    else              RaiseAbortOperation;
  end;
end;

function TKmtpCopyOutOperation.FileExists(aFile: TFile;
  var AbsoluteTargetFileName: String): TFileSourceOperationOptionFileExists;
const
  Responses: array[0..9] of TFileSourceOperationUIResponse =
    (fsourOverwrite, fsourSkip, fsourRenameSource, fsourOverwriteAll,
     fsourSkipAll, fsourOverwriteOlder, fsourOverwriteSmaller,
     fsourCancel, fsouaCompare, fsourOverwriteLarger);
var
  Answer: Boolean;
  Msg, NewName: String;
  TargetStat: TSearchRecEx;

  function TargetSize: Int64;
  begin
    Result := TargetStat.Size;
  end;

  function OverwriteOlder: TFileSourceOperationOptionFileExists;
  begin
    if CompareDateTime(RecodeMilliSecond(aFile.ModificationTime, 0),
                       FileTimeToDateTime(TargetStat.Time)) = GreaterThanValue then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

begin
  if FindFirstEx(AbsoluteTargetFileName, 0, TargetStat) = 0 then
    FindCloseEx(TargetStat)
  else
  begin
    TargetStat.Size := 0;
    TargetStat.Time := 0;
  end;

  case FFileExistsOption of
    fsoofeNone:
      repeat
        Answer := True;
        Msg := rsMsgFileExistsOverwrite + LineEnding + AbsoluteTargetFileName + LineEnding +
               Format(rsMsgFileExistsFileInfo, [IntToStrTS(TargetSize),
                      DateTimeToStr(FileTimeToDateTime(TargetStat.Time))]) + LineEnding +
               LineEnding + rsMsgFileExistsWithFile + LineEnding + aFile.FullPath + LineEnding +
               Format(rsMsgFileExistsFileInfo, [IntToStrTS(aFile.Size), DateTimeToStr(aFile.ModificationTime)]);
        FCurrentFile := aFile;
        FCurrentTargetFilePath := AbsoluteTargetFileName;
        case AskQuestion(Msg, '', Responses, fsourOverwrite, fsourSkip, @QuestionActionHandler) of
          fsourOverwrite:        Exit(fsoofeOverwrite);
          fsourSkip:             Exit(fsoofeSkip);
          fsourOverwriteAll:     begin FFileExistsOption := fsoofeOverwrite; Exit(fsoofeOverwrite); end;
          fsourSkipAll:          begin FFileExistsOption := fsoofeSkip; Exit(fsoofeSkip); end;
          fsourOverwriteOlder:   begin FFileExistsOption := fsoofeOverwriteOlder; Exit(OverwriteOlder); end;
          fsourOverwriteSmaller:
            begin
              FFileExistsOption := fsoofeOverwriteSmaller;
              if aFile.Size > TargetSize then Exit(fsoofeOverwrite) else Exit(fsoofeSkip);
            end;
          fsourOverwriteLarger:
            begin
              FFileExistsOption := fsoofeOverwriteLarger;
              if aFile.Size < TargetSize then Exit(fsoofeOverwrite) else Exit(fsoofeSkip);
            end;
          fsourRenameSource:
            begin
              NewName := ExtractFileName(AbsoluteTargetFileName);
              Answer := ShowInputQuery(Thread, Application.Title, rsEditNewFileName, NewName);
              if Answer and (NewName <> '') then
              begin
                AbsoluteTargetFileName := ExtractFilePath(AbsoluteTargetFileName) + NewName;
                Exit(fsoofeAutoRenameSource);
              end;
            end;
          else
            RaiseAbortOperation;
        end;
      until not Answer;
    fsoofeOverwriteOlder:   Result := OverwriteOlder;
    fsoofeOverwriteSmaller: if aFile.Size > TargetSize then Result := fsoofeOverwrite else Result := fsoofeSkip;
    fsoofeOverwriteLarger:  if aFile.Size < TargetSize then Result := fsoofeOverwrite else Result := fsoofeSkip;
    else
      Result := FFileExistsOption;
  end;
end;

function TKmtpCopyOutOperation.TargetExists(aNode: TFileTreeNode;
  var AbsoluteTargetFileName: String): TFileSystemOperationTargetExistsResult;
begin
  repeat
    if mbDirectoryExists(AbsoluteTargetFileName) then
    begin
      if DirExists(aNode.TheFile, AbsoluteTargetFileName, aNode.TheFile.IsDirectory) = fsoodeSkip then
        Exit(fsoterSkip)
      else
        Exit(fsoterAddToTarget);
    end
    else if mbFileExists(AbsoluteTargetFileName) then
    begin
      case FileExists(aNode.TheFile, AbsoluteTargetFileName) of
        fsoofeSkip:             Exit(fsoterSkip);
        fsoofeAutoRenameSource: Result := fsoterRenamed;
        fsoofeOverwrite:        Exit(fsoterDeleted);
        else                    Exit(fsoterSkip);
      end;
    end
    else
      Exit(fsoterNotExists);
  until Result <> fsoterRenamed;
end;

function TKmtpCopyOutOperation.ProcessNode(aNode: TFileTreeNode; const CurrentTargetPath: String): Boolean;
var
  I: Integer;
  SubNode: TFileTreeNode;
  aFile: TFile;
  TargetName: String;
begin
  Result := True;
  for I := 0 to aNode.SubNodesCount - 1 do
  begin
    SubNode := aNode.SubNodes[I];
    aFile := SubNode.TheFile;

    if FRenamingRootDir and (aFile = FRootDir) then
      TargetName := CurrentTargetPath + RenameMask
    else if FRenamingFiles then
      TargetName := CurrentTargetPath + ApplyRenameMask(FKmtpFileSource as IFileSource, aFile, FRenameNameMask, FRenameExtMask)
    else
      TargetName := CurrentTargetPath + aFile.Name;

    FStatistics.CurrentFileFrom := aFile.FullPath;
    FStatistics.CurrentFileTo := TargetName;
    FStatistics.CurrentFileTotalBytes := aFile.Size;
    FStatistics.CurrentFileDoneBytes := 0;
    UpdateStatistics(FStatistics);

    if aFile.IsDirectory then
    begin
      if not ProcessDirectory(SubNode, TargetName) then
        Result := False;
    end
    else
    begin
      if not ProcessFile(SubNode, TargetName) then
        Result := False;
    end;
    CheckOperationState;
  end;
end;

function TKmtpCopyOutOperation.ProcessDirectory(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
begin
  case TargetExists(aNode, AbsoluteTargetFileName) of
    fsoterSkip:
      begin
        CountStatistics(aNode);
        Exit(False);
      end;
    fsoterNotExists:
      begin
        if not mbForceDirectory(AbsoluteTargetFileName) then
        begin
          LogMessage(rsMsgLogError + Format(rsMsgErrForceDir, [AbsoluteTargetFileName]), [log_vfs_op], lmtError);
          if not FSkipAnyError then
            if AskQuestion(Format(rsMsgErrForceDir, [AbsoluteTargetFileName]), '', [fsourSkip, fsourAbort], fsourSkip, fsourAbort) = fsourAbort then
              RaiseAbortOperation;
          CountStatistics(aNode);
          Exit(False);
        end;
      end;
    fsoterAddToTarget: ;
  end;

  Result := ProcessNode(aNode, IncludeTrailingPathDelimiter(AbsoluteTargetFileName));
end;

function TKmtpCopyOutOperation.ProcessFile(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
var
  DevPath, StorPath, RelPath, ErrMsg: String;
  Level: Integer;
  FileTime: TFileTime;
begin
  Result := False;
  FOldDoneBytes := FStatistics.DoneBytes;

  if (not FKmtpFileSource.ResolveTarget(aNode.TheFile.FullPath, DevPath, StorPath, RelPath, Level)) or
     (Level < 2) or (StorPath = '') or (RelPath = '') then
  begin
    if not FSkipAnyError then
      if AskQuestion(rsMsgErrNotSupported, '', [fsourSkip, fsourAbort], fsourSkip, fsourAbort) = fsourAbort then
        RaiseAbortOperation;
    Exit(False);
  end;

  if TargetExists(aNode, AbsoluteTargetFileName) = fsoterSkip then
  begin
    FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
    FStatistics.DoneBytes := FOldDoneBytes + aNode.TheFile.Size;
    UpdateStatistics(FStatistics);
    Exit(True);
  end;

  repeat
    Result := KmtpTransferFileFD(StorPath, RelPath, AbsoluteTargetFileName, False,
                                 UInt64(Max(Int64(0), aNode.TheFile.Size)), @OnProgress, ErrMsg);
    if Result then
    begin
      if (caoCopyTime in CopyAttributesOptions) and (aNode.TheFile.ModificationTime > 0) then
      begin
        FileTime := DateTimeToFileTime(aNode.TheFile.ModificationTime);
        mbFileSetTime(AbsoluteTargetFileName, FileTime, 0, 0);
      end;
      Break;
    end;
    if State = fsosStopping then
      RaiseAbortOperation;
    if FSkipAnyError then
      Break;
    case AskQuestion(ErrMsg, '', [fsourRetry, fsourSkip, fsourSkipAll, fsourCancel], fsourRetry, fsourCancel) of
      fsourRetry:   ;
      fsourSkip:    Break;
      fsourSkipAll: begin FSkipAnyError := True; Break; end;
      else          RaiseAbortOperation;
    end;
  until False;

  if Result then
    LogMessage(Format(rsMsgLogSuccess + rsMsgLogCopy, [aNode.TheFile.FullPath + ' -> ' + AbsoluteTargetFileName]), [log_vfs_op], lmtSuccess)
  else
    LogMessage(Format(rsMsgLogError + rsMsgLogCopy, [aNode.TheFile.FullPath + ' -> ' + AbsoluteTargetFileName]) + LineEnding + ErrMsg, [log_vfs_op], lmtError);

  FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
  FStatistics.DoneBytes := FOldDoneBytes + aNode.TheFile.Size;
  UpdateStatistics(FStatistics);
end;

procedure TKmtpCopyOutOperation.CountStatistics(aNode: TFileTreeNode);
  procedure CountNode(N: TFileTreeNode);
  var
    I: Integer;
  begin
    if N.TheFile.IsDirectory then
      for I := 0 to N.SubNodesCount - 1 do
        CountNode(N.SubNodes[I])
    else
    begin
      FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
      FStatistics.DoneBytes := FStatistics.DoneBytes + N.TheFile.Size;
    end;
  end;
begin
  CountNode(aNode);
  UpdateStatistics(FStatistics);
end;

{ TKmtpDeleteOperation }

constructor TKmtpDeleteOperation.Create(aTargetFileSource: IFileSource;
  var theFilesToDelete: TFiles);
begin
  FKmtpFileSource := aTargetFileSource as IKmtpFileSource;
  FSkipErrors := False;
  FFullFilesTreeToDelete := nil;
  inherited Create(aTargetFileSource, theFilesToDelete);
end;

destructor TKmtpDeleteOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FFullFilesTreeToDelete);
end;

procedure TKmtpDeleteOperation.FillAndCount;
var
  I: Integer;
  aFile: TFile;

  procedure FillRec(const srcPath: String);
  var
    DevPath, StorPath, RelPath: String;
    Level, J, ExistsCode: Integer;
    Files: TKmtpFileArray;
    ChildFile: TFile;
  begin
    if (not FKmtpFileSource.ResolveTarget(srcPath, DevPath, StorPath, RelPath, Level)) or (Level < 2) then
      Exit;
    if KmtpGetFilesAndFolders(StorPath, RelPath, Files, ExistsCode) and (ExistsCode = 0) then
    begin
      for J := 0 to High(Files) do
      begin
        if (Files[J].FileName = '') or (Files[J].FileName = '.') or (Files[J].FileName = '..') then
          Continue;
        ChildFile := MakeKmtpFileFromInfo(srcPath, Files[J]);
        FFullFilesTreeToDelete.Add(ChildFile);
        if ChildFile.IsDirectory then
        begin
          Inc(FStatistics.TotalFiles);
          FillRec(srcPath + ChildFile.Name + DirectorySeparator);
        end
        else
        begin
          Inc(FStatistics.TotalFiles);
          Inc(FStatistics.TotalBytes, ChildFile.Size);
        end;
      end;
    end;
  end;

begin
  FStatistics.TotalFiles := 0;
  FStatistics.TotalBytes := 0;
  FFullFilesTreeToDelete := TFiles.Create(FilesToDelete.Path);
  for I := 0 to FilesToDelete.Count - 1 do
  begin
    aFile := FilesToDelete[I];
    FFullFilesTreeToDelete.Add(aFile.Clone);
    if aFile.IsDirectory then
    begin
      Inc(FStatistics.TotalFiles);
      FillRec(aFile.FullPath + DirectorySeparator);
    end
    else
    begin
      Inc(FStatistics.TotalFiles);
      Inc(FStatistics.TotalBytes, aFile.Size);
    end;
  end;
end;

procedure TKmtpDeleteOperation.Initialize;
begin
  FStatistics := RetrieveStatistics;
  FillAndCount;
end;

procedure TKmtpDeleteOperation.MainExecute;
var
  I: Integer;
  aFile: TFile;
begin
  for I := FFullFilesTreeToDelete.Count - 1 downto 0 do
  begin
    aFile := FFullFilesTreeToDelete[I];
    FStatistics.CurrentFile := aFile.FullPath;
    UpdateStatistics(FStatistics);

    ProcessFile(aFile);

    FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
    FStatistics.DoneBytes := FStatistics.DoneBytes + aFile.Size;
    UpdateStatistics(FStatistics);
    CheckOperationState;
  end;
end;

function TKmtpDeleteOperation.ShowError(const sMessage: String): TFileSourceOperationUIResponse;
begin
  if gSkipFileOpError or FSkipErrors then
    Exit(fsourSkip);
  Result := AskQuestion(sMessage, '', [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort], fsourRetry, fsourAbort);
  case Result of
    fsourSkipAll: FSkipErrors := True;
    fsourAbort:   RaiseAbortOperation;
  end;
end;

procedure TKmtpDeleteOperation.LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
begin
  case logMsgType of
    lmtError:   if not (log_errors in gLogOptions) then Exit;
    lmtInfo:    if not (log_info in gLogOptions) then Exit;
    lmtSuccess: if not (log_success in gLogOptions) then Exit;
  end;
  if logOptions <= gLogOptions then
    logWrite(Thread, sMessage, logMsgType);
end;

function TKmtpDeleteOperation.ProcessFile(aFile: TFile): Boolean;
var
  DevPath, StorPath, RelPath, Msg: String;
  Level: Integer;
begin
  Result := False;
  if (not FKmtpFileSource.ResolveTarget(aFile.FullPath, DevPath, StorPath, RelPath, Level)) or
     (Level < 2) or (StorPath = '') or (RelPath = '') then
    Exit(False);

  repeat
    Result := KmtpDeleteObject(StorPath, RelPath);
    if Result then
    begin
      if aFile.IsDirectory then
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogRmDir, [aFile.FullPath]), [log_vfs_op], lmtSuccess)
      else
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogDelete, [aFile.FullPath]), [log_vfs_op], lmtSuccess);
      Exit(True);
    end;

    if aFile.IsDirectory then
      Msg := Format(rsMsgLogError + rsMsgLogRmDir, [aFile.FullPath])
    else
      Msg := Format(rsMsgLogError + rsMsgLogDelete, [aFile.FullPath]);

    LogMessage(Msg, [log_vfs_op], lmtError);
  until ShowError(Msg) <> fsourRetry;
end;

{ TKmtpCreateDirectoryOperation }

procedure TKmtpCreateDirectoryOperation.MainExecute;
begin
  if (FileSource as IFileSource).CreateDirectory(AbsolutePath) then
  begin
    if (log_vfs_op in gLogOptions) and (log_success in gLogOptions) then
      logWrite(Thread, Format(rsMsgLogSuccess + rsMsgLogMkDir, [AbsolutePath]), lmtSuccess);
  end
  else
  begin
    if (log_vfs_op in gLogOptions) and (log_errors in gLogOptions) then
      logWrite(Thread, Format(rsMsgLogError + rsMsgLogMkDir, [AbsolutePath]), lmtError);
  end;
end;

{ TKmtpExecuteOperation }

procedure TKmtpExecuteOperation.MainExecute;
var
  AFiles: TFiles;
begin
  if Verb = 'properties' then
  begin
    AFiles := TFiles.Create(CurrentPath);
    try
      AFiles.Add(ExecutableFile.Clone);
      ShowFileProperties(FileSource as IFileSource, AFiles);
    finally
      AFiles.Free;
    end;
    FExecuteOperationResult := fseorSuccess;
    Exit;
  end;

  if not ExecutableFile.IsDirectory then
    FExecuteOperationResult := fseorYourSelf
  else
    FExecuteOperationResult := fseorError;
end;

{ TKmtpCalcStatisticsOperation }

constructor TKmtpCalcStatisticsOperation.Create(aTargetFileSource: IFileSource;
  var theFiles: TFiles);
begin
  FKmtpFileSource := aTargetFileSource as IKmtpFileSource;
  inherited Create(aTargetFileSource, theFiles);
end;

procedure TKmtpCalcStatisticsOperation.Initialize;
begin
  FStatistics := RetrieveStatistics;
end;

procedure TKmtpCalcStatisticsOperation.MainExecute;
var
  I: Integer;
begin
  for I := 0 to Files.Count - 1 do
  begin
    ProcessFile(Files[I]);
    CheckOperationState;
  end;
end;

procedure TKmtpCalcStatisticsOperation.ProcessFile(aFile: TFile);
begin
  FStatistics.CurrentFile := aFile.FullPath;
  UpdateStatistics(FStatistics);

  if aFile.IsDirectory then
  begin
    Inc(FStatistics.Directories);
    ProcessSubDirs(aFile.FullPath + DirectorySeparator);
  end
  else
  begin
    Inc(FStatistics.Files);
    FStatistics.Size := FStatistics.Size + aFile.Size;
    if aFile.ModificationTime > 0 then
    begin
      if (FStatistics.OldestFile = 0) or (aFile.ModificationTime < FStatistics.OldestFile) then
        FStatistics.OldestFile := aFile.ModificationTime;
      if aFile.ModificationTime > FStatistics.NewestFile then
        FStatistics.NewestFile := aFile.ModificationTime;
    end;
  end;
  UpdateStatistics(FStatistics);
end;

procedure TKmtpCalcStatisticsOperation.ProcessSubDirs(const srcPath: String);
var
  DevPath, StorPath, RelPath: String;
  Level, I, ExistsCode: Integer;
  Storages: TKmtpStorageArray;
  SubFiles: TKmtpFileArray;
  AFile: TFile;
begin
  if not FKmtpFileSource.ResolveTarget(srcPath, DevPath, StorPath, RelPath, Level) then
    Exit;

  if Level = 1 then
  begin
    Storages := KmtpListStorages(DevPath);
    for I := 0 to High(Storages) do
    begin
      CheckOperationState;
      AFile := TKmtpFileSource.CreateFile(srcPath);
      AFile.Name := Storages[I].Description;
      AFile.Attributes := S_IFDIR or $01ED;
      try
        ProcessFile(AFile);
      finally
        AFile.Free;
      end;
    end;
  end
  else if Level >= 2 then
  begin
    if KmtpGetFilesAndFolders(StorPath, RelPath, SubFiles, ExistsCode) and (ExistsCode = 0) then
    begin
      for I := 0 to High(SubFiles) do
      begin
        CheckOperationState;
        if (SubFiles[I].FileName = '') or (SubFiles[I].FileName = '.') or (SubFiles[I].FileName = '..') then
          Continue;
        AFile := MakeKmtpFileFromInfo(srcPath, SubFiles[I]);
        try
          ProcessFile(AFile);
        finally
          AFile.Free;
        end;
      end;
    end;
  end;
end;

{ TKmtpSetFilePropertyOperation }

constructor TKmtpSetFilePropertyOperation.Create(aTargetFileSource: IFileSource;
  var theTargetFiles: TFiles; var theNewProperties: TFileProperties);
begin
  FKmtpFileSource := aTargetFileSource as IKmtpFileSource;
  FFullFilesTree := nil;
  inherited Create(aTargetFileSource, theTargetFiles, theNewProperties);
  FSupportedProperties := [fpName];
end;

destructor TKmtpSetFilePropertyOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TKmtpSetFilePropertyOperation.Initialize;
begin
  FStatistics := RetrieveStatistics;
  FFullFilesTree := TargetFiles;
  FStatistics.TotalFiles := FFullFilesTree.Count;
end;

procedure TKmtpSetFilePropertyOperation.MainExecute;
var
  I: Integer;
  aFile, aTemplateFile: TFile;
begin
  for I := 0 to FFullFilesTree.Count - 1 do
  begin
    aFile := FFullFilesTree[I];
    FStatistics.CurrentFile := aFile.FullPath;
    UpdateStatistics(FStatistics);

    if Assigned(TemplateFiles) and (I < TemplateFiles.Count) then
      aTemplateFile := TemplateFiles[I]
    else
      aTemplateFile := nil;

    SetProperties(I, aFile, aTemplateFile);

    FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
    UpdateStatistics(FStatistics);
    CheckOperationState;
  end;
end;

function TKmtpSetFilePropertyOperation.SetNewProperty(aFile: TFile;
  aTemplateProperty: TFileProperty): TSetFilePropertyResult;
var
  DevPath, StorPath, RelPath, NewName: String;
  Level: Integer;
begin
  if aTemplateProperty.GetID <> fpName then
    Exit(sfprSkipped);

  NewName := (aTemplateProperty as TFileNameProperty).Value;
  if NewName = aFile.Name then
    Exit(sfprSkipped);

  if (not FKmtpFileSource.ResolveTarget(aFile.FullPath, DevPath, StorPath, RelPath, Level)) or
     (Level < 2) or (StorPath = '') or (RelPath = '') then
    Exit(sfprError);

  if KmtpSetFileName(StorPath, RelPath, NewName) then
    Result := sfprSuccess
  else
    Result := sfprError;
end;

{ TKmtpFileSource }

constructor TKmtpFileSource.Create;
begin
  inherited Create;
  FDeviceName := '';
  FCurrentAddress := 'mtp://';
end;

constructor TKmtpFileSource.Create(const URI: TURI);
begin
  inherited Create(URI);
  FCurrentAddress := FURI.Protocol + '://' + FURI.Host;
  FDeviceName := DecodeMtpHost(FURI.Host);
end;

function TKmtpFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  Result := True;
end;

class function TKmtpFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result := StrBegins(Path, 'mtp://') and IsKmtpActive;
end;

class function TKmtpFileSource.CreateFile(const APath: String): TFile;
begin
  Result := inherited CreateFile(APath);
  with Result do
  begin
    AttributesProperty := TFileAttributesProperty.CreateOSAttributes;
    SizeProperty := TFileSizeProperty.Create;
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create;
    TypeProperty := TFileTypeProperty.Create;
    CommentProperty := TFileCommentProperty.Create;
  end;
end;

function TKmtpFileSource.GetDeviceName: String;
begin
  Result := FDeviceName;
end;

function TKmtpFileSource.ResolveTarget(const APath: String;
  out ADevicePath, AStoragePath, ARelPath: String; out ALevel: Integer): Boolean;
var
  CleanPath: String;
  Segments: TStringArray;
  Device: TKmtpDeviceInfo;
  Storage: TKmtpStorageInfo;
  DevIdx: Integer;
begin
  Result := False;
  ADevicePath := '';
  AStoragePath := '';
  ARelPath := '';
  ALevel := 0;

  CleanPath := APath;
  if StrBegins(CleanPath, FCurrentAddress) then
    Delete(CleanPath, 1, Length(FCurrentAddress));

  Segments := SplitKmtpSegments(CleanPath);

  if FDeviceName <> '' then
  begin
    if not FindDeviceByName(FDeviceName, Device) then
      Exit(False);
    ADevicePath := Device.ObjectPath;
    if Length(Segments) = 0 then
    begin
      ALevel := 1;
      Exit(True);
    end;
    if not FindStorageByDesc(ADevicePath, Segments[0], Storage) then
      Exit(False);
    AStoragePath := Storage.ObjectPath;
    ARelPath := JoinKmtpStoragePath(Segments, 1);
    ALevel := 2;
    Result := True;
  end
  else
  begin
    if Length(Segments) = 0 then
    begin
      ALevel := 0;
      Exit(True);
    end;
    if not FindDeviceByName(Segments[0], Device) then
      Exit(False);
    ADevicePath := Device.ObjectPath;
    if Length(Segments) = 1 then
    begin
      ALevel := 1;
      Exit(True);
    end;
    DevIdx := 1;
    if not FindStorageByDesc(ADevicePath, Segments[DevIdx], Storage) then
      Exit(False);
    AStoragePath := Storage.ObjectPath;
    ARelPath := JoinKmtpStoragePath(Segments, DevIdx + 1);
    ALevel := 2;
    Result := True;
  end;
end;

function TKmtpFileSource.GetPathType(sPath: String): TPathType;
begin
  if Pos('://', sPath) > 0 then
    Result := ptAbsolute
  else
    Result := inherited GetPathType(sPath);
end;

function TKmtpFileSource.GetFileSystem: String;
begin
  if FDeviceName <> '' then
    Result := FDeviceName
  else
    Result := 'MTP';
end;

function TKmtpFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspVirtual, fspSynchronizable];
end;

function TKmtpFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := [fpName, fpSize, fpAttributes, fpModificationTime];
end;

function TKmtpFileSource.GetRetrievableFileProperties: TFilePropertiesTypes;
begin
  Result := [fpName, fpSize, fpAttributes, fpModificationTime];
end;

function TKmtpFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList, fsoCopyIn, fsoCopyOut, fsoDelete,
             fsoCreateDirectory, fsoExecute, fsoCalcStatistics, fsoSetFileProperty];
end;

function TKmtpFileSource.FileSystemEntryExists(const Path: String;
  const Options: TFileSourceExistsOptions): TFileSourceExistsResult;
var
  DevPath, StorPath, RelPath: String;
  Level: Integer;
  Meta: TKmtpFileInfo;
  Exists: Boolean;
begin
  Result := TFileSourceExistsResult.notExist;
  if Options = [] then
    Exit;

  if not ResolveTarget(Path, DevPath, StorPath, RelPath, Level) then
    Exit;

  if (Level < 2) or (RelPath = '') then
  begin
    if (Options = [TFileSourceExistsOption.needFile]) then
      Exit(TFileSourceExistsResult.notExist)
    else
      Exit(TFileSourceExistsResult.exists);
  end;

  if not KmtpGetFileMetadata(StorPath, RelPath, Meta) then
    Exit;

  if Options = [TFileSourceExistsOption.needFile] then
    Exists := (Meta.FileType <> KMTP_MIME_DIRECTORY)
  else if Options = [TFileSourceExistsOption.needDir] then
    Exists := (Meta.FileType = KMTP_MIME_DIRECTORY)
  else
    Exists := True;

  if Exists then
    Result := TFileSourceExistsResult.exists;
end;

function TKmtpFileSource.CreateDirectory(const Path: String): Boolean;
var
  CleanPath: String;
  Segments: TStringArray;
  Device: TKmtpDeviceInfo;
  Storage: TKmtpStorageInfo;
  CurRel, DevName, StorDesc: String;
  StartIdx, I: Integer;
  Meta: TKmtpFileInfo;
begin
  Result := False;
  CleanPath := Path;
  if StrBegins(CleanPath, FCurrentAddress) then
    Delete(CleanPath, 1, Length(FCurrentAddress));

  Segments := SplitKmtpSegments(CleanPath);
  if FDeviceName <> '' then
  begin
    if Length(Segments) < 2 then
      Exit(False);
    DevName := FDeviceName;
    StorDesc := Segments[0];
    StartIdx := 1;
  end
  else
  begin
    if Length(Segments) < 3 then
      Exit(False);
    DevName := Segments[0];
    StorDesc := Segments[1];
    StartIdx := 2;
  end;

  if not FindDeviceByName(DevName, Device) then
    Exit(False);
  if not FindStorageByDesc(Device.ObjectPath, StorDesc, Storage) then
    Exit(False);

  CurRel := '';
  for I := StartIdx to High(Segments) do
  begin
    CurRel := CurRel + '/' + Segments[I];
    if KmtpGetFileMetadata(Storage.ObjectPath, CurRel, Meta) then
    begin
      if Meta.FileType <> KMTP_MIME_DIRECTORY then
        Exit(False);
    end
    else
    begin
      if not KmtpCreateFolder(Storage.ObjectPath, CurRel) then
        Exit(False);
    end;
  end;
  Result := True;
end;

function TKmtpFileSource.GetFreeSpace(Path: String; out FreeSize, TotalSize: Int64): Boolean;
var
  DevPath, StorPath, RelPath: String;
  Level, I: Integer;
  Val: UInt64;
  Storages: TKmtpStorageArray;
begin
  Result := False;
  FreeSize := 0;
  TotalSize := 0;
  if not ResolveTarget(Path, DevPath, StorPath, RelPath, Level) then
    Exit;

  if (Level >= 2) and (StorPath <> '') then
  begin
    if KmtpGetPropertyUInt64(StorPath, KMTPD_STORAGE_INTERFACE, 'freeSpaceInBytes', Val) then
      FreeSize := Int64(Val);
    if KmtpGetPropertyUInt64(StorPath, KMTPD_STORAGE_INTERFACE, 'maxCapacity', Val) then
      TotalSize := Int64(Val);
    Result := (TotalSize > 0);
  end
  else if (Level = 1) and (DevPath <> '') then
  begin
    Storages := KmtpListStorages(DevPath);
    for I := 0 to High(Storages) do
    begin
      Inc(FreeSize, Int64(Storages[I].FreeSpace));
      Inc(TotalSize, Int64(Storages[I].MaxCapacity));
    end;
    Result := (TotalSize > 0);
  end;
end;

procedure TKmtpFileSource.Reload(const PathsToReload: TPathsArray);
var
  Index: Integer;
  PathList: TPathsArray;
begin
  SetLength(PathList, Length(PathsToReload));
  for Index := Low(PathsToReload) to High(PathsToReload) do
    PathList[Index] := StringReplace(PathsToReload[Index], FCurrentAddress, '', []);
  inherited Reload(PathList);
end;

function TKmtpFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
begin
  Result := TKmtpListOperation.Create(Self, TargetPath);
end;

function TKmtpFileSource.CreateCopyInOperation(SourceFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
begin
  Result := TKmtpCopyInOperation.Create(SourceFileSource, Self, SourceFiles, TargetPath);
end;

function TKmtpFileSource.CreateCopyOutOperation(TargetFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
begin
  Result := TKmtpCopyOutOperation.Create(Self, TargetFileSource, SourceFiles, TargetPath);
end;

function TKmtpFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
begin
  Result := TKmtpDeleteOperation.Create(Self, FilesToDelete);
end;

function TKmtpFileSource.CreateCreateDirectoryOperation(BasePath: String;
  DirectoryPath: String): TFileSourceOperation;
begin
  Result := TKmtpCreateDirectoryOperation.Create(Self, BasePath, DirectoryPath);
end;

function TKmtpFileSource.CreateExecuteOperation(var ExecutableFile: TFile;
  BasePath, Verb: String): TFileSourceOperation;
begin
  Result := TKmtpExecuteOperation.Create(Self, ExecutableFile, BasePath, Verb);
end;

function TKmtpFileSource.CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation;
begin
  Result := TKmtpCalcStatisticsOperation.Create(Self, theFiles);
end;

function TKmtpFileSource.CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
  var theNewProperties: TFileProperties): TFileSourceOperation;
begin
  Result := TKmtpSetFilePropertyOperation.Create(Self, theTargetFiles, theNewProperties);
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
