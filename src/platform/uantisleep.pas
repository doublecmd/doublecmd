unit uAntiSleep;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure AllowSleepMode(Allow: Boolean);

implementation

{$IF DEFINED(MSWINDOWS)}
uses JwaWinNT, JwaWinBase;
{$ELSEIF DEFINED(LINUX)}
uses dbus, LazLogger, BaseUnix;
{$ELSEIF DEFINED(DARWIN)}
uses MacOSAll;
{$ENDIF}

{$IF DEFINED(MSWINDOWS)}
threadvar
  Count: Integer;

procedure DisableSleep;
const
  ES_AWAYMODE_REQUIRED = $00000040;
var
  esFlags: EXECUTION_STATE = ES_CONTINUOUS or ES_SYSTEM_REQUIRED;
begin
  Inc(Count);
  if (Count = 1) then
  begin
    if Win32MajorVersion > 5 then begin
      esFlags:= esFlags or ES_AWAYMODE_REQUIRED;
    end;
    SetThreadExecutionState(esFlags);
  end;
end;

procedure EnableSleep;
begin
  if (Count > 0) then
  begin
    Dec(Count);
    if (Count = 0) then
    begin
      SetThreadExecutionState(ES_CONTINUOUS);
    end;
  end;
end;
{$ELSEIF DEFINED(DARWIN)}
const
  kIOPMNullAssertionID = 0;

type
  IOPMAssertionID = UInt32;
  PIOPMAssertionID = ^IOPMAssertionID;

  {$push}{$packenum 4}
  IOPMAssertionLevel =
  (
    kIOPMAssertionLevelOff = 0,
    kIOPMAssertionLevelOn  = 255
  );
  {$pop}

const
  kIOPMAssertPreventUserIdleSystemSleep = 'PreventUserIdleSystemSleep';

function IOPMAssertionCreateWithName(AssertionType: CFStringRef; AssertionLevel: IOPMAssertionLevel; AssertionName: CFStringRef; AssertionID: PIOPMAssertionID): IOReturn; mwpascal; external name '_IOPMAssertionCreateWithName';
function IOPMAssertionRelease(AssertionID: IOPMAssertionID): IOReturn; mwpascal; external name '_IOPMAssertionRelease';

var
  Count: Integer = 0;
  Mutex: System.TRTLCriticalSection;
  AssertionID: IOPMAssertionID = kIOPMNullAssertionID;

procedure DisableSleep;
var
  Res: IOReturn;
  AssertID: IOPMAssertionID = kIOPMNullAssertionID;
begin
  System.EnterCriticalSection(Mutex);
  try
    Inc(Count);
    if (Count = 1) then
    begin
      Res:= IOPMAssertionCreateWithName(CFSTR(kIOPMAssertPreventUserIdleSystemSleep), kIOPMAssertionLevelOn, CFSTR('Double Commander'), @AssertID);
      if (Res = kIOReturnSuccess) then AssertionID:= AssertID;
    end;
  finally
    System.LeaveCriticalSection(Mutex);
  end;
end;

procedure EnableSleep;
begin
  System.EnterCriticalSection(Mutex);
  try
    if (Count > 0) then
    begin
      Dec(Count);
      if (Count = 0) then
      begin
        if (AssertionID <> kIOPMNullAssertionID) then
        begin
          IOPMAssertionRelease(AssertionID);
          AssertionID:= kIOPMNullAssertionID;
        end;
      end;
    end;
  finally
    System.LeaveCriticalSection(Mutex);
  end;
end;

procedure Initialize;
begin
  System.InitCriticalSection(Mutex);
end;

procedure Finalize;
begin
  System.DoneCriticalSection(Mutex);
end;
{$ELSEIF DEFINED(LINUX)}
const
  LOGIN1_ADDRESS = 'org.freedesktop.login1';
  LOGIN1_PATH = '/org/freedesktop/login1';
  LOGIN1_INTERFACE = 'org.freedesktop.login1.Manager';

var
  conn: PDBusConnection = nil;
  inhibit_fd: THandle = feInvalidHandle;

var
  Count: Integer = 0;
  Mutex: System.TRTLCriticalSection;

procedure Print(const sMessage: String);
begin
  DebugLn('Inhibit: ', sMessage);
end;

function CheckError(const sMessage: String; pError: PDBusError): Boolean;
begin
  if (dbus_error_is_set(pError) = 0) then
    Result := False
  else begin
    Print(sMessage + ': ' + pError^.name + ' ' + pError^.message);
    dbus_error_free(pError);
    Result := True;
  end;
end;

procedure DisableSleep;
var
  res: Boolean;
  err: DBusError;
  iter: DBusMessageIter;
  message, reply: PDBusMessage;
  what, who, why, mode: PAnsiChar;
begin
  System.EnterCriticalSection(Mutex);
  try
    Inc(Count);
    if (Count = 1) then
    begin
      dbus_error_init(@err);

      message := dbus_message_new_method_call(LOGIN1_ADDRESS, LOGIN1_PATH, LOGIN1_INTERFACE, 'Inhibit');

      if message = nil then
      begin
        Print('Cannot create message');
        Exit;
      end;

      try
        what := 'idle';
        who := 'Double Commander';
        why := 'Operation in progress';
        mode := 'block';

        dbus_message_iter_init_append(message, @iter);
        res:= dbus_message_iter_append_basic(@iter, DBUS_TYPE_STRING, @what) <> 0;
        res:= res and (dbus_message_iter_append_basic(@iter, DBUS_TYPE_STRING, @who) <> 0);
        res:= res and (dbus_message_iter_append_basic(@iter, DBUS_TYPE_STRING, @why) <> 0);
        res:= res and (dbus_message_iter_append_basic(@iter, DBUS_TYPE_STRING, @mode) <> 0);

        if not res then
        begin
          Print('Cannot append arguments');
          Exit;
        end;

        reply:= dbus_connection_send_with_reply_and_block(conn, message, 1000, @err);

        if CheckError('Send message', @err) then Exit;

        dbus_message_iter_init(reply, @iter);
        dbus_message_iter_get_basic(@iter, @inhibit_fd);
        dbus_message_unref(reply);
      finally
        dbus_message_unref(message);
      end;
    end;
  finally
    System.LeaveCriticalSection(Mutex);
  end;
end;

procedure EnableSleep;
var
  Res: Integer;
begin
  System.EnterCriticalSection(Mutex);
  try
    if (Count > 0) then
    begin
      Dec(Count);
      if (Count = 0) then
      begin
        if inhibit_fd <> feInvalidHandle then
        begin
          repeat
            Res:= fpClose(inhibit_fd);
          until (Res <> -1) or (fpGetErrNo <> ESysEINTR);

          if (Res < 0) then
          begin
            Print(SysErrorMessage(fpGetErrNo));
          end;
          inhibit_fd:= feInvalidHandle;
        end;
      end;
    end;
  finally
    System.LeaveCriticalSection(Mutex);
  end;
end;

procedure Initialize;
var
  error: DBusError;
begin
  dbus_error_init(@error);
  System.InitCriticalSection(Mutex);
  conn:= dbus_bus_get(DBUS_BUS_SYSTEM, @error);
  CheckError('Cannot acquire connection to DBUS system bus', @error);
end;

procedure Finalize;
begin
  System.DoneCriticalSection(Mutex);
  if Assigned(conn) then dbus_connection_unref(conn);
end;
{$ELSE}
procedure DisableSleep;
begin
end;

procedure EnableSleep;
begin
end;
{$ENDIF}

procedure AllowSleepMode(Allow: Boolean);
begin
  if Allow then
    EnableSleep
  else begin
    DisableSleep;
  end;
end;

{$IF DEFINED(DARWIN) OR DEFINED(LINUX)}
initialization
  Initialize;

finalization
  Finalize;
{$ENDIF}

end.

