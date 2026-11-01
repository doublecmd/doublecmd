{
    Double Commander
    -------------------------------------------------------------------------
    Support for the org.freedesktop.portal.FileTransfer portal.

    A sandboxed application cannot open a file by its path on the host, so a
    plain text/uri-list hands it a name it can show but no data it can read.
    The portal solves that: the source exports the files through the document
    portal and offers only a transfer key, which the target redeems for paths
    that are readable inside its own sandbox.

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

unit uFileTransferPortal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  {en
     Toolkits offer this format next to text/uri-list. Its content is the
     transfer key returned by StartTransfer, followed by a zero byte.
  }
  portalFileTransferMime = 'application/vnd.portal.filetransfer';

{en
   Exports the files through the document portal and returns the transfer key
   to offer under portalFileTransferMime. Returns an empty string when the
   portal is not available, in which case the file list is simply dragged the
   way it always was.
}
function FileTransferRegister(FileNames: TStringList): String;

{en
   Turns a transfer key received from another application into file names that
   this application is allowed to open.
}
function FileTransferRetrieve(const Key: String; FileNames: TStringList): Boolean;

implementation

uses
  BaseUnix, DCUnix, uGlib2, uGObject2, uGio2, uDebug;

const
{$IF DEFINED(LINUX)}
  // A path descriptor is all the portal needs, and unlike a read-only one it
  // does not require permission to read the file.
  OpenFlags = O_PATH or O_CLOEXEC;
{$ELSE}
  OpenFlags = O_RDONLY or O_CLOEXEC;
{$ENDIF}

  PORTAL_BUS_NAME    = 'org.freedesktop.portal.Documents';
  PORTAL_OBJECT_PATH = '/org/freedesktop/portal/documents';
  PORTAL_INTERFACE   = 'org.freedesktop.portal.FileTransfer';

  {en
     The drag is started from inside a mouse event handler, so the portal is
     never allowed to block the user interface for the default D-Bus timeout.
  }
  PORTAL_TIMEOUT = 1000;

  {en
     The session bus limits how many file descriptors fit in one message.
  }
  MaxFilesPerMessage = 16;

var
  Initialized: Boolean = False;
  Unavailable: Boolean = False;
  DBusConn: PGDBusConnection = nil;
  {en
     Key of the previous transfer, closed when the next one is started.
  }
  LastKey: String = '';

procedure PrintError(var AError: PGError);
begin
  if Assigned(AError) then
  begin
    DCDebug('FileTransfer portal: ', StrPas(AError^.message));
    g_error_free(AError);
    AError:= nil;
  end;
end;

function Connection: PGDBusConnection;
var
  AError: PGError = nil;
begin
  if not Initialized then
  begin
    Initialized:= True;
    // GIO is loaded dynamically, so the functions may simply not be there.
    if Assigned(g_bus_get_sync) and Assigned(g_dbus_connection_call_sync) and
       Assigned(g_dbus_connection_call_with_unix_fd_list_sync) then
    begin
      DBusConn:= g_bus_get_sync(G_BUS_TYPE_SESSION, nil, @AError);
      PrintError(AError);
    end;
  end;
  if Unavailable then
    Result:= nil
  else begin
    Result:= DBusConn;
  end;
end;

procedure StopTransfer(const Key: String);
var
  Res: PGVariant;
  AError: PGError = nil;
begin
  if (Length(Key) = 0) or (DBusConn = nil) then Exit;

  Res:= g_dbus_connection_call_sync(DBusConn, PORTAL_BUS_NAME,
                                    PORTAL_OBJECT_PATH, PORTAL_INTERFACE,
                                    'StopTransfer',
                                    g_variant_new('(s)', [PAnsiChar(Key)]),
                                    nil, G_DBUS_CALL_FLAGS_NONE,
                                    PORTAL_TIMEOUT, nil, @AError);

  // A transfer that the target has already picked up closes itself, so an
  // error here is the normal case and not worth reporting.
  if Assigned(AError) then g_error_free(AError);
  if Assigned(Res) then g_variant_unref(Res);
end;

function StartTransfer: String;
var
  Res, Child: PGVariant;
  AError: PGError = nil;
  Options: TGVariantBuilder;
begin
  Result:= '';

  g_variant_builder_init(@Options, PGVariantType(Pgchar('a{sv}')));
  // Read only access is enough, the target is receiving a copy of the files.
  g_variant_builder_add(@Options, '{sv}', ['writable', g_variant_new_boolean(False)]);
  // Let the portal close the transfer once the files have been retrieved.
  g_variant_builder_add(@Options, '{sv}', ['autostop', g_variant_new_boolean(True)]);

  Res:= g_dbus_connection_call_sync(DBusConn, PORTAL_BUS_NAME,
                                    PORTAL_OBJECT_PATH, PORTAL_INTERFACE,
                                    'StartTransfer',
                                    g_variant_new('(a{sv})', [@Options]),
                                    nil, G_DBUS_CALL_FLAGS_NONE,
                                    PORTAL_TIMEOUT, nil, @AError);

  if Assigned(AError) then
  begin
    PrintError(AError);
    // Nothing is listening, do not pay the timeout again on every drag.
    Unavailable:= True;
    Exit;
  end;

  if Assigned(Res) then
  begin
    Child:= g_variant_get_child_value(Res, 0);
    Result:= StrPas(g_variant_get_string(Child, nil));
    g_variant_unref(Child);
    g_variant_unref(Res);
  end;
end;

function AddFiles(const Key: String; FileNames: TStringList;
                  AFrom, ATo: Integer): Boolean;
var
  Res: PGVariant;
  Index: Integer;
  Handle: cint;
  Count: Integer = 0;
  FDList: PGUnixFDList;
  AError: PGError = nil;
  Fds, Options: TGVariantBuilder;
begin
  FDList:= g_unix_fd_list_new();
  g_variant_builder_init(@Fds, PGVariantType(Pgchar('ah')));

  for Index:= AFrom to ATo do
  begin
    repeat
      Handle:= fpOpen(FileNames[Index], OpenFlags);
    until (Handle <> -1) or (fpgeterrno <> ESysEINTR);

    if (Handle < 0) then Continue;

    // The list duplicates the descriptor, so ours has to be closed either way.
    if (g_unix_fd_list_append(FDList, Handle, @AError) < 0) then
      PrintError(AError)
    else begin
      g_variant_builder_add_value(@Fds, g_variant_new_handle(Count));
      Inc(Count);
    end;

    fpClose(Handle);
  end;

  if (Count = 0) then
  begin
    g_variant_builder_clear(@Fds);
    g_object_unref(PGObject(FDList));
    Exit(False);
  end;

  g_variant_builder_init(@Options, PGVariantType(Pgchar('a{sv}')));

  Res:= g_dbus_connection_call_with_unix_fd_list_sync(DBusConn,
          PORTAL_BUS_NAME, PORTAL_OBJECT_PATH, PORTAL_INTERFACE, 'AddFiles',
          g_variant_new('(saha{sv})', [PAnsiChar(Key), @Fds, @Options]),
          nil, G_DBUS_CALL_FLAGS_NONE, PORTAL_TIMEOUT,
          FDList, nil, nil, @AError);

  g_object_unref(PGObject(FDList));

  PrintError(AError);

  Result:= Assigned(Res);
  if Result then g_variant_unref(Res);
end;

function FileTransferRegister(FileNames: TStringList): String;
var
  AFrom, ATo: Integer;
begin
  Result:= '';
  if (Connection = nil) or (FileNames.Count = 0) then Exit;

  // Close the transfer before this one, in case the target never picked it up.
  // Doing it here instead of when the drag ends avoids racing with a target
  // that retrieves the files only after the drop has been accepted.
  if (Length(LastKey) > 0) then
  begin
    StopTransfer(LastKey);
    LastKey:= '';
  end;

  Result:= StartTransfer;
  if (Length(Result) = 0) then Exit;

  AFrom:= 0;
  while (AFrom < FileNames.Count) do
  begin
    ATo:= AFrom + MaxFilesPerMessage - 1;
    if (ATo > FileNames.Count - 1) then ATo:= FileNames.Count - 1;

    if not AddFiles(Result, FileNames, AFrom, ATo) then
    begin
      StopTransfer(Result);
      Exit(EmptyStr);
    end;

    AFrom:= ATo + 1;
  end;

  LastKey:= Result;
end;

function FileTransferRetrieve(const Key: String; FileNames: TStringList): Boolean;
var
  Index: Integer;
  Count: Integer;
  Res, List, Child: PGVariant;
  AError: PGError = nil;
  Options: TGVariantBuilder;
begin
  Result:= False;
  if (Connection = nil) or (Length(Key) = 0) then Exit;

  g_variant_builder_init(@Options, PGVariantType(Pgchar('a{sv}')));

  Res:= g_dbus_connection_call_sync(DBusConn, PORTAL_BUS_NAME,
                                    PORTAL_OBJECT_PATH, PORTAL_INTERFACE,
                                    'RetrieveFiles',
                                    g_variant_new('(sa{sv})', [PAnsiChar(Key), @Options]),
                                    nil, G_DBUS_CALL_FLAGS_NONE,
                                    PORTAL_TIMEOUT, nil, @AError);

  PrintError(AError);
  if (Res = nil) then Exit;

  List:= g_variant_get_child_value(Res, 0);
  Count:= Integer(g_variant_n_children(List));

  for Index:= 0 to Count - 1 do
  begin
    Child:= g_variant_get_child_value(List, gsize(Index));
    FileNames.Add(StrPas(g_variant_get_string(Child, nil)));
    g_variant_unref(Child);
  end;

  g_variant_unref(List);
  g_variant_unref(Res);

  Result:= (FileNames.Count > 0);
end;

end.
