unit uDarwinFNKey;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  MacOSAll, CocoaAll,
  CocoaEvent, Cocoa_Extra;

type

  // force F1, F2, etc. to be used as standard function keys
  { TDarwinFNKeyTap }

  TDarwinFNKeyTap = class( TCocoaEventTap )
  public
    procedure createTap; override;
  end;

implementation

{ TDarwinFNKeyTap }

function createKeyEvent(
  keyCode: CGKeyCode;
  flags: CGEventFlags;
  isKeyDown: Boolean ): CGEventRef;
const
  MASK = NSShiftKeyMask or NSControlKeyMask or NSAlternateKeyMask or NSCommandKeyMask;
var
  keyDownBool: CBool;
begin
  flags:= flags and MASK;
  if isKeyDown then
    keyDownBool:= 1
  else
    keyDownBool:= 0;
  Result:= CGEventCreateKeyboardEvent( nil, keyCode, keyDownBool );
  CGEventSetFlags( Result, flags );
end;

function FNKeyKeyEventHandler( eventRef: CGEventRef; isKeyDown: Boolean ): CGEventRef;
var
  keyCode: CGKeyCode;
  flags: CGEventFlags;
begin
  Result:= eventRef;
  keyCode:= CGEventGetIntegerValueField( eventRef, kCGKeyboardEventKeycode );
  flags:= CGEventGetFlags( eventRef );

  case keyCode of
    144:
      keyCode:= kVK_F1;
    145:
      keyCode:= kVK_F2;
    160:
      keyCode:= kVK_F3;
    128, 129, 177:
      keyCode:= kVK_F4;
    176, 179:
      keyCode:= kVK_F5;
    178, 180:
      keyCode:= kVK_F6;
    131:
      keyCode:= kVK_F7;
    130:
      keyCode:= kVK_F8;
    132:
      keyCode:= kVK_F9;
    else
      keyCode:= 0;
  end;

  if keyCode <> 0 then
    Result:= createKeyEvent( keyCode, flags, isKeyDown );
end;

function FNKeySystemEventHandler( eventRef: CGEventRef ): CGEventRef;
var
  event: NSEvent;
  keyCode: CGKeyCode;
  flags: CGEventFlags;
  isKeyDown: Boolean;
begin
  Result:= eventRef;
  event:= NSEvent.eventWithCGEvent( eventRef );
  if event.subtype <> NX_SUBTYPE_AUX_CONTROL_BUTTONS then
    Exit;
  keyCode:= (event.data1 and $FFFF0000) >> 16;
  flags:= event.modifierFlags;
  isKeyDown:= (event.data1 and $FF00)=$0A00;

  case keyCode of
    NX_KEYTYPE_BRIGHTNESS_DOWN:
      keyCode:= kVK_F1;
    NX_KEYTYPE_BRIGHTNESS_UP:
      keyCode:= kVK_F2;
    NX_KEYTYPE_ILLUMINATION_DOWN:
      keyCode:= kVK_F5;
    NX_KEYTYPE_ILLUMINATION_UP:
      keyCode:= kVK_F6;
    NX_KEYTYPE_REWIND:
      keyCode:= kVK_F7;
    NX_KEYTYPE_PLAY:
      keyCode:= kVK_F8;
    NX_KEYTYPE_FAST:
      keyCode:= kVK_F9;
    NX_KEYTYPE_MUTE:
      keyCode:= kVK_F10;
    NX_KEYTYPE_SOUND_DOWN:
      keyCode:= kVK_F11;
    NX_KEYTYPE_SOUND_UP:
      keyCode:= kVK_F12;
    else
      keyCode:= 0;
  end;

  if keyCode <> 0 then
    Result:= createKeyEvent( keyCode, flags, isKeyDown );
end;

function FNKeyEventHandler(
  proxy: CGEventTapProxy;
  eventType: CGEventType;
  event: CGEventRef;
  userInfo: UnivPtr ): CGEventRef; MWPascal;
begin
  Result:= event;

  if NOT (NSApp.isActive and Assigned(NSApp.keyWindow)) then
    Exit;

  if (eventType=NSKeyDown) or (eventType=NSKeyUp) then begin
    Result:= FNKeyKeyEventHandler( event, eventType=NSKeyDown );
  end else begin
    Result:= FNKeySystemEventHandler( event );
  end;
end;

procedure TDarwinFNKeyTap.createTap;
var
  runLoopSource: CFRunLoopSourceRef;
begin
  if self.hasTap then
    Exit;

  TCocoaEventTapUtil.promptAuthorization;

  _port:= CGEventTapCreate(
    kCGHIDEventTap,
    kCGHeadInsertEventTap,
    kCGEventTapOptionDefault,
    NSKeyDownMask or NSKeyUpMask or NSSystemDefinedMask,
    @FNKeyEventHandler,
    nil );

  if NOT Assigned(_port) then
    Exit;

  runLoopSource:= CFMachPortCreateRunLoopSource( nil, _port, 0 );
  CFRunLoopAddSource(
    CFRunLoopGetCurrent(),
    runLoopSource,
    kCFRunLoopCommonModes );
  CFRelease( runLoopSource );
end;

end.
