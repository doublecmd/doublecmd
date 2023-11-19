unit uFlatpak;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uMyUnix;

function FlatpakOpen(const FileName: String; Ask: Boolean32): Boolean;

implementation

uses
  BaseUnix, DCOSUtils, DCUnix, uTrash, uDebug, uGLib2, uGObject2, uGio2;

const
  PORTAL_BUS_NAME = 'org.freedesktop.portal.Desktop';
  PORTAL_OBJECT_PATH = '/org/freedesktop/portal/desktop';
  PORTAL_INTERFACE_TRASH = 'org.freedesktop.portal.Trash';
  PORTAL_INTERFACE_OPENURI = 'org.freedesktop.portal.OpenURI';

var
  PortalBusName: String;
  DBusConn: PGDBusConnection = nil;

procedure PrintError(AError: PGError);
begin
  DCDebug(AError^.message);
  g_error_free(AError);
end;

function FlatpakOpen(const FileName: String; Ask: Boolean32): Boolean;
var
  Res: PGVariant;
  Handle: THandle;
  FDList: PGUnixFDList;
  AError: PGError = nil;
  Options: TGVariantBuilder;
begin
  if (DBusConn = nil) then Exit(False);
  Handle:= mbFileOpen(FileName, fmOpenRead or fmShareDenyNone);

  if Handle < 0 then Exit(False);

  FDList:= g_unix_fd_list_new_from_array(@Handle, 1);
  g_variant_builder_init(@Options, PGVariantType(Pgchar('a{sv}')));
  g_variant_builder_add(@Options, '{sv}', ['ask', g_variant_new_boolean(Ask)]);

  Res:= g_dbus_connection_call_with_unix_fd_list_sync(DBusConn,
                                                      Pgchar(PortalBusName),
                                                      PORTAL_OBJECT_PATH,
                                                      PORTAL_INTERFACE_OPENURI,
                                                      'OpenFile',
                                                      g_variant_new('(sha{sv})', ['', 0, @Options]),
                                                      nil,
                                                      G_DBUS_CALL_FLAGS_NONE,
                                                      -1,
                                                      FDList,
                                                      nil,
                                                      nil,
                                                      @AError);

  g_object_unref(PGObject(FDList));

  if Assigned(AError) then
  begin
    PrintError(AError);
  end;

  Result:= Assigned(Res);
  if Result then g_variant_unref(Res);
end;

function FileTrash(const FileName: String): Boolean;
var
  Answer: guint;
  Ret: PGVariant;
  Handle: THandle;
  FDList: PGUnixFDList;
  AError: PGError = nil;
begin
  repeat
    Handle:= fpOpen(FileName, O_PATH or O_CLOEXEC);
  until (Handle <> -1) or (fpgeterrno <> ESysEINTR);

  if Handle < 0 then Exit(False);

  FDList:= g_unix_fd_list_new_from_array(@Handle, 1);

  Ret:= g_dbus_connection_call_with_unix_fd_list_sync(DBusConn,
                                                      Pgchar(PortalBusName),
                                                      PORTAL_OBJECT_PATH,
                                                      PORTAL_INTERFACE_TRASH,
                                                      'TrashFile',
                                                      g_variant_new('(h)', [0]),
                                                      nil,
                                                      G_DBUS_CALL_FLAGS_NONE,
                                                      -1,
                                                      FDList,
                                                      nil,
                                                      nil,
                                                      @AError);

  g_object_unref(PGObject(FDList));

  if Assigned(AError) then
  begin
    PrintError(AError);
  end;

  if (Ret = nil) then
    Result:= False
  else begin
    g_variant_get(Ret, '(u)', [@Answer]);
    Result:= (Answer = 1);
    g_variant_unref(Ret);
  end;
end;

procedure Initialize;
var
  AError: PGError = nil;
begin
  if (DesktopEnv = DE_FLATPAK) then
  begin
    PortalBusName:= GetEnvironmentVariable('LIBPORTAL_PORTAL_BUS_NAME');
    if (Length(PortalBusName) = 0) then PortalBusName:= PORTAL_BUS_NAME;
    DBusConn:= g_bus_get_sync(G_BUS_TYPE_SESSION, nil, @AError);
    if (DBusConn = nil) then
      PrintError(AError)
    else begin
      FileTrashUtf8:= @FileTrash;
    end;
  end;
end;

procedure Finalize;
begin
  if Assigned(DBusConn) then g_object_unref(PGObject(DBusConn));
end;

initialization
  Initialize;

finalization
  Finalize;

end.

