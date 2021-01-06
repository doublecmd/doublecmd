unit uRabbitVCS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Graphics, uPixMapManager;

const
  RabbitVCSAddress   = 'org.google.code.rabbitvcs.RabbitVCS.Checker';
  RabbitVCSObject    = '/org/google/code/rabbitvcs/StatusChecker';
  RabbitVCSInterface = 'org.google.code.rabbitvcs.StatusChecker';

type
  TVcsStatus = (vscNormal, vscModified, vscAdded, vscDeleted, vscIgnored,
                vscReadOnly, vscLocked, vscUnknown, vscMissing, vscReplaced,
                vscComplicated, vscCalculating, vscError, vscUnversioned);

const
  VcsStatusText: array[TVcsStatus] of String =
                                         (
                                         'normal',
                                         'modified',
                                         'added',
                                         'deleted',
                                         'ignored',
                                         'locked',
                                         'locked',
                                         'unknown',
                                         'missing',
                                         'replaced',
                                         'complicated',
                                         'calculating',
                                         'error',
                                         'unversioned'
                                         );

  VcsStatusEmblems: array[TVcsStatus] of String =
                                         (
                                         'emblem-rabbitvcs-normal',
                                         'emblem-rabbitvcs-modified',
                                         'emblem-rabbitvcs-added',
                                         'emblem-rabbitvcs-deleted',
                                         'emblem-rabbitvcs-ignored',
                                         'emblem-rabbitvcs-locked',
                                         'emblem-rabbitvcs-locked',
                                         'emblem-rabbitvcs-unknown',
                                         'emblem-rabbitvcs-complicated',
                                         'emblem-rabbitvcs-modified',
                                         'emblem-rabbitvcs-complicated',
                                         'emblem-rabbitvcs-calculating',
                                         'emblem-rabbitvcs-error',
                                         'emblem-rabbitvcs-unversioned'
                                         );

{en
   Requests a status check from the underlying status checker.
}
function CheckStatus(Path: String; Recurse: Boolean32 = False;
                     Invalidate: Boolean32 = True; Summary: Boolean32 = False): string;

procedure FillRabbitMenu(Menu: TPopupMenu; Paths: TStringList);

var
  RabbitVCS: Boolean = False;

implementation

uses
  dbus, fpjson, jsonparser, jsonscanner, unix, baseunix,
  DCUnix, DCClassesUtf8, uGlobs, uGlobsPaths, uMyUnix, uPython
{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
  , uGObject2
{$ENDIF}
  ;

const
  MODULE_NAME = 'rabbit-vcs';

var
  error: DBusError;
  RabbitGtk3: Boolean;
  conn: PDBusConnection = nil;
  PythonModule: PPyObject = nil;
  ShellContextMenu: PPyObject = nil;
  PythonVersion: array[Byte] of AnsiChar;

procedure Initialize(Self: TObject); forward;

procedure Print(const sMessage: String);
begin
  WriteLn('RabbitVCS: ', sMessage);
end;

function CheckError(const sMessage: String; pError: PDBusError): Boolean;
begin
  if (dbus_error_is_set(pError) <> 0) then
  begin
    Print(sMessage + ': ' + pError^.name + ' ' + pError^.message);
    dbus_error_free(pError);
    Result := True;
  end
  else
    Result := False;
end;

function CheckRabbit: Boolean;
var
  service_exists: dbus_bool_t;
begin
  dbus_error_init(@error);
  // Check if RabbitVCS service is running
  service_exists := dbus_bus_name_has_owner(conn, RabbitVCSAddress, @error);
  if CheckError('Cannot query RabbitVCS on DBUS', @error) then
    Result:= False
  else
    Result:= (service_exists <> 0);
end;

function CheckService: Boolean;
var
  pyValue: PPyObject;
begin
  Result:= CheckRabbit;
  if Result then
    Print('Service found running')
  else begin
    // Try to start RabbitVCS service
    pyValue:= PythonRunFunction(PythonModule, 'StartService');
    Py_XDECREF(pyValue);
    Result:= CheckRabbit;
    if Result then Print('Service successfully started');
  end;
end;

function CheckStatus(Path: String; Recurse: Boolean32;
                     Invalidate: Boolean32; Summary: Boolean32): string;
var
  Return: Boolean;
  StringPtr: PAnsiChar;
  JAnswer : TJSONObject;
  VcsStatus: TVcsStatus;
  message: PDBusMessage;
  argsIter: DBusMessageIter;
  pending: PDBusPendingCall;
  arrayIter: DBusMessageIter;
begin
  if not RabbitVCS then Exit;

  // Create a new method call and check for errors
  message := dbus_message_new_method_call(RabbitVCSAddress,   // target for the method call
                                          RabbitVCSObject,    // object to call on
                                          RabbitVCSInterface, // interface to call on
                                          'CheckStatus');     // method name
  if (message = nil) then
  begin
    Print('Cannot create message "CheckStatus"');
    Exit;
  end;

  try
    // Append arguments
    StringPtr:= PAnsiChar(Path);
    dbus_message_iter_init_append(message, @argsIter);

    if not RabbitGtk3 then
      Return:= (dbus_message_iter_append_basic(@argsIter, DBUS_TYPE_STRING, @StringPtr) <> 0)
    else begin
      Return:= (dbus_message_iter_open_container(@argsIter, DBUS_TYPE_ARRAY, DBUS_TYPE_BYTE_AS_STRING, @arrayIter) <> 0);
      Return:= Return and (dbus_message_iter_append_fixed_array(@arrayIter, DBUS_TYPE_BYTE, @StringPtr, Length(Path)) <> 0);
      Return:= Return and (dbus_message_iter_close_container(@argsIter, @arrayIter) <> 0);
    end;

    Return:= Return and (dbus_message_iter_append_basic(@argsIter, DBUS_TYPE_BOOLEAN, @Recurse) <> 0);
    Return:= Return and (dbus_message_iter_append_basic(@argsIter, DBUS_TYPE_BOOLEAN, @Invalidate) <> 0);
    Return:= Return and (dbus_message_iter_append_basic(@argsIter, DBUS_TYPE_BOOLEAN, @Summary) <> 0);

    if not Return then
    begin
      Print('Cannot append arguments');
      Exit;
    end;

    // Send message and get a handle for a reply
    if (dbus_connection_send_with_reply(conn, message, @pending, -1) = 0) then
    begin
      Print('Error sending message');
      Exit;
    end;

    if (pending = nil) then
    begin
      Print('Pending call is null');
      Exit;
    end;

    dbus_connection_flush(conn);

  finally
    dbus_message_unref(message);
  end;

  // Block until we recieve a reply
  dbus_pending_call_block(pending);
  // Get the reply message
  message := dbus_pending_call_steal_reply(pending);
  // Free the pending message handle
  dbus_pending_call_unref(pending);

  if (message = nil) then
  begin
    Print('Reply is null');
    Exit;
  end;

  try
    // Read the parameters
    if (dbus_message_iter_init(message, @argsIter) <> 0) then
    begin
      if (dbus_message_iter_get_arg_type(@argsIter) = DBUS_TYPE_STRING) then
      begin
        dbus_message_iter_get_basic(@argsIter, @StringPtr);

        with TJSONParser.Create(StrPas(StringPtr), [joUTF8]) do
        try
          JAnswer:= Parse as TJSONObject;
          try
            Result:= JAnswer.Strings['content'];
            if Result = 'unknown' then Exit(EmptyStr);
          except
            Exit(EmptyStr);
          end;
          JAnswer.Free;
        finally
          Free;
        end;

        for VcsStatus:= Low(TVcsStatus) to High(VcsStatus) do
        begin
          if (VcsStatusText[VcsStatus] = Result) then
          begin
            Result:= VcsStatusEmblems[VcsStatus];
            Break;
          end;
        end;
      end;
    end;
  finally
    dbus_message_unref(message);
  end;
end;

procedure MenuClickHandler(Self, Sender: TObject);
var
  pyMethod, pyArgs: PPyObject;
  MenuItem: TMenuItem absolute Sender;
begin
  if Assigned(ShellContextMenu) then
  begin
    pyMethod:= PyString_FromString('Execute');
    pyArgs:= PyString_FromString(PAnsiChar(MenuItem.Hint));
    PyObject_CallMethodObjArgs(ShellContextMenu, pyMethod, pyArgs, nil);
    Py_XDECREF(pyArgs);
    Py_XDECREF(pyMethod);
  end;
end;

procedure FillRabbitMenu(Menu: TPopupMenu; Paths: TStringList);
var
  Handler: TMethod;
  pyMethod, pyValue: PPyObject;

  procedure SetBitmap(Item: TMenuItem; const IconName: String);
  var
    bmpTemp: TBitmap;
  begin
    bmpTemp:= PixMapManager.LoadBitmapEnhanced(IconName, 16, True, clMenu);
    if Assigned(bmpTemp) then
    begin
      Item.Bitmap.Assign(bmpTemp);
      FreeAndNil(bmpTemp);
    end;
  end;

  procedure BuildMenu(pyMenu: PPyObject; BaseItem: TMenuItem);
  var
    Index: Integer;
    IconName: String;
    MenuItem: TMenuItem;
    pyItem, pyObject: PPyObject;
  begin
    for Index:= 0 to PyList_Size(pyMenu) - 1 do
    begin
      pyItem:= PyList_GetItem(pyMenu, Index);
      MenuItem:= TMenuItem.Create(BaseItem);
      pyObject:= PyObject_GetAttrString(pyItem, 'label');
      MenuItem.Caption:= PyStringToString(pyObject);
      if MenuItem.Caption <> '-' then
      begin
        pyObject:= PyObject_GetAttrString(pyItem, 'identifier');
        MenuItem.Hint:= PyStringToString(pyObject);
        if Length(MenuItem.Hint) > 0 then begin
          MenuItem.OnClick:= TNotifyEvent(Handler);
        end;
        pyObject:= PyObject_GetAttrString(pyItem, 'icon');
        IconName:= PyStringToString(pyObject);
        if Length(IconName) > 0 then SetBitmap(MenuItem, IconName);
      end;
      pyObject:= PyObject_GetAttrString(pyItem, 'menu');
      if Assigned(pyObject) and (PyList_Size(pyObject) > 0) then
      begin
        BuildMenu(pyObject, MenuItem);
        Py_DECREF(pyObject);
      end;
      BaseItem.Add(MenuItem);
    end;
  end;

begin
  if not RabbitVCS then Exit;
  Py_XDECREF(ShellContextMenu);
  ShellContextMenu:= PythonRunFunction(PythonModule, 'GetContextMenu', Paths);
  if Assigned(ShellContextMenu) then
  begin
    Handler.Data:= Menu;
    Handler.Code:= @MenuClickHandler;
    pyMethod:= PyString_FromString('GetMenu');
    pyValue:= PyObject_CallMethodObjArgs(ShellContextMenu, pyMethod, nil);
    if Assigned(pyValue) then
    begin
      BuildMenu(pyValue, Menu.Items);
      Py_DECREF(pyValue);
    end;
    Py_XDECREF(pyMethod);
  end;
end;

function FindPython(const Path: String): String;
const
  Debian = '/usr/share/python3/debian_defaults';
begin
  Result:= ExtractFileName(Path);
  Result:= StringReplace(Result, 'python', '', []);
  if Length(Result) > 0 then
  begin
    if Result[1] = '2' then Exit;
    if fpAccess(Debian, F_OK) = 0 then
    try
      with TIniFileEx.Create(Debian, fmOpenRead) do
      try
        Result:= ReadString('DEFAULT', 'default-version', Result);
        Result:= StringReplace(Trim(Result), 'python', '', []);
      finally
        Free;
      end;
    except
      // Ignore
    end;
  end;
end;

function CheckPackage({%H-}Parameter : Pointer): PtrInt;
const
  Path = '/usr/lib';
var
  DirPtr: pDir;
  Handler: TMethod;
  Directory: String;
  DirEntPtr: pDirent;
  PackageFormat: String;
begin
  if fpAccess('/etc/debian_version', F_OK) = 0 then
    PackageFormat:= 'dist-packages/rabbitvcs'
  else begin
    PackageFormat:= 'site-packages/rabbitvcs';
  end;
  DirPtr:= fpOpenDir(Path);
  if Assigned(DirPtr) then
  try
    DirEntPtr:= fpReadDir(DirPtr^);
    while DirEntPtr <> nil do
    begin
      if (DirEntPtr^.d_name <> '..') and (DirEntPtr^.d_name <> '.') then
      begin
        if FNMatch('python*', DirEntPtr^.d_name, 0) = 0 then
        begin
          Directory:= Path + PathDelim + DirEntPtr^.d_name;
          if (fpAccess(Directory + PathDelim + PackageFormat, F_OK) = 0) then
          begin
            PythonVersion:= FindPython(Directory);
            if Length(PythonVersion) > 0 then
            begin
              Handler.Data:= nil;
              Handler.Code:= @Initialize;
              TThread.Synchronize(nil, TThreadMethod(Handler));
              Exit(0);
            end;
          end;
        end;
      end;
      DirEntPtr:= fpReadDir(DirPtr^);
    end;
  finally
    fpCloseDir(DirPtr^);
  end;
  Result:= 1;
end;

function CheckVersion: Boolean;
var
  ATemp: AnsiString;
  pyModule: PPyObject;
  pyVersion: PPyObject;
  AVersion: TStringArray;
  Major, Minor, Micro: Integer;
{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
  GtkWidget: TGType;
  GtkClass, Gtk3: Pointer;
{$ENDIF}
begin
  Result:= False;
  pyModule:= PythonLoadModule('rabbitvcs');
  if Assigned(pyModule) then
  begin
    pyVersion:= PythonRunFunction(pyModule, 'package_version');
    if Assigned(pyVersion) then
    begin
      ATemp:= PyStringToString(pyVersion);
      AVersion:= ATemp.Split(['.']);
      Print('Version ' + ATemp);
      if (Length(AVersion) > 1) then
      begin
        Major:= StrToIntDef(AVersion[0], 0);
        Minor:= StrToIntDef(AVersion[1], 0);
        if (Length(AVersion) > 2) then
          Micro:= StrToIntDef(AVersion[2], 0)
        else begin
          Micro:= 0;
        end;
        // RabbitVCS migrated to GTK3 from version 0.17.1
        RabbitGtk3:= (Major > 0) or (Minor > 17) or ((Minor = 17) and (Micro > 0));
{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
        // Check GTK platform theme plugin
        GtkWidget:= g_type_from_name('GtkWidget');
        Result:= (GtkWidget = 0);
        if not Result then
        begin
          GtkClass:= g_type_class_ref(GtkWidget);
          // Property 'expand' since GTK 3.0
          Gtk3:= g_object_class_find_property(GtkClass, 'expand');
          // RabbitVCS GTK version should be same as Qt platform theme plugin GTK version
          Result:= (RabbitGtk3 = Assigned(Gtk3));
        end;
{$ELSEIF DEFINED(LCLGTK2)}
        Result:= not RabbitGTK3;
{$ELSEIF DEFINED(LCLGTK3)}
        Result:= RabbitGTK3;
{$ELSE}
        Result:= True
{$ENDIF}
      end;
    end;
  end;
end;

procedure Initialize(Self: TObject);
begin
  dbus_error_init(@error);
  conn := dbus_bus_get(DBUS_BUS_SESSION, @error);
  if CheckError('Cannot acquire connection to DBUS session bus', @error) then
    Exit;
  if PythonInitialize(PythonVersion) then
  begin
    if not CheckVersion then Exit;
    Print('Python version ' + PythonVersion);
    PythonAddModulePath(gpExePath + 'scripts');
    PythonModule:= PythonLoadModule(MODULE_NAME);
    RabbitVCS:= Assigned(PythonModule) and CheckService;
  end;
end;

procedure Finalize;
begin
  PythonFinalize;
  if Assigned(conn) then dbus_connection_unref(conn);
end;

initialization
  BeginThread(@CheckPackage);

finalization
  Finalize;

end.

