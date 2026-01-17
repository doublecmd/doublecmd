unit uAntiSleep;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure AllowSleepMode(Allow: Boolean);

implementation

{$IF DEFINED(MSWINDOWS)}
uses JwaWinNT, JwaWinBase;
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
  Dec(Count);
  if (Count = 0) then
  begin
    SetThreadExecutionState(ES_CONTINUOUS);
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
    Dec(Count);
    if (Count = 0) then
    begin
      if (AssertionID <> kIOPMNullAssertionID) then
      begin
        IOPMAssertionRelease(AssertionID);
        AssertionID:= kIOPMNullAssertionID;
      end;
    end;
  finally
    System.LeaveCriticalSection(Mutex);
  end;
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

{$IF DEFINED(DARWIN)}
initialization
  System.InitCriticalSection(Mutex);

finalization
  System.DoneCriticalSection(Mutex);
{$ENDIF}

end.

