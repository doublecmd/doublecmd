unit uFileManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFile;

function ShowItemProperties(const Files: TFiles): Boolean;

implementation

uses
  DBus, URIParser, uDebug;

const
  DBUS_TIMEOUT_INFINITE = Integer($7fffffff);

const
  FileManagerAddress    = 'org.freedesktop.FileManager1';
  FileManagerObjectPath = '/org/freedesktop/FileManager1';
  FileManagerInterface  = 'org.freedesktop.FileManager1';

var
  DBusError: DBus.DBusError;
  DBusConn: DBus.PDBusConnection;
  FileManagerAvailable: Boolean = True;

procedure Print(const sMessage: String);
begin
  DCDebug('FileManager: ', sMessage);
end;

function CheckError(const sMessage: String; pError: PDBusError): Boolean;
begin
  if (dbus_error_is_set(pError) = 0) then
    Result:= False
  else begin
    Print(sMessage + ': ' + pError^.name + ' ' + pError^.message);
    dbus_error_free(pError);
    Result:= True;
  end;
end;

function SendMessage(AMessage: PDBusMessage): Boolean;
var
  AReply: PDBusMessage;
begin
  dbus_error_init (@DBusError);
  AReply := dbus_connection_send_with_reply_and_block(DBusConn, AMessage, DBUS_TIMEOUT_INFINITE, @DBusError);

  if CheckError('Error sending message', @DBusError) then
    Result:= False
  else if Assigned(AReply) then
  begin
    Result:= True;
    dbus_message_unref(AReply);
  end
  else begin
    Result:= False;
    Print('Reply not received');
  end;
end;

function ShowItemProperties(const Files: TFiles): Boolean;
var
  AFile: String;
  Index: Integer;
  StringPtr: PAnsiChar;
  AMessage: PDBusMessage;
  argsIter, arrayIter: DBusMessageIter;
begin
  if (DBusConn = nil) then Exit(False);
  if (not FileManagerAvailable) then Exit(False);

  {
    <method name='ShowItemProperties'>
      <arg type='as' name='URIs' direction='in'/>
      <arg type='s' name='StartupId' direction='in'/>
    </method>
  }

  AMessage:= dbus_message_new_method_call(FileManagerAddress,
                                          FileManagerObjectPath,
                                          FileManagerInterface,
                                          'ShowItemProperties');
  if not Assigned(AMessage) then
  begin
    Print('Cannot create message "FilesystemMount"');
    Result:= False;
  end
  else begin
    dbus_message_iter_init_append(AMessage, @argsIter);

    Result:= (dbus_message_iter_open_container(@argsIter, DBUS_TYPE_ARRAY, PAnsiChar(DBUS_TYPE_STRING_AS_STRING), @arrayIter) <> 0);

    if Result then
    begin
      for Index := 0 to Files.Count - 1 do
      begin
        AFile:= FilenameToURI(Files[Index].FullPath);
        StringPtr:= PAnsiChar(AFile);

        if dbus_message_iter_append_basic(@arrayIter, DBUS_TYPE_STRING, @StringPtr) = 0 then
        begin
          Result:= False;
          Break;
        end;
      end;

      if dbus_message_iter_close_container(@argsIter, @arrayIter) = 0 then
        Result:= False;
    end;

    if Result then
    begin
      StringPtr:= '';
      Result:= (dbus_message_iter_append_basic(@argsIter, DBUS_TYPE_STRING, @StringPtr) <> 0);
    end;

    if not Result then
    begin
      Print('Cannot append arguments');
    end
    else begin
      Result:= SendMessage(AMessage);
    end;
    dbus_message_unref(AMessage);
  end;
  FileManagerAvailable:= Result;
end;

procedure Initialize;
begin
  dbus_error_init(@DBusError);
  DBusConn:= dbus_bus_get(DBUS_BUS_SESSION, @DBusError);

  if CheckError('Cannot acquire connection to DBUS session bus', @DBusError) then
    Exit;

  if Assigned(DBusConn) then
  begin
    dbus_connection_set_exit_on_disconnect(DBusConn, 0);
  end;
end;

procedure Finalize;
begin
  if Assigned(DBusConn) then dbus_connection_unref(DBusConn);
end;

initialization
  Initialize;

finalization
  Finalize;

end.

