{
  This unit handles anything regarding keyboard and keys.
  It is heavily dependent on operating system and widget set.
  For MSWINDOWS and Unix GTK1/2, QT.
}

unit uKeyboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType;


type
  TMenuKeyCap = (mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns,
    mkcDel, mkcShift, mkcCtrl, mkcAlt, mkcWin);

const
  SmkcBkSp = 'BkSp';
  SmkcTab = 'Tab';
  SmkcEsc = 'Esc';
  SmkcEnter = 'Enter';
  SmkcSpace = 'Space';
  SmkcPgUp = 'PgUp';
  SmkcPgDn = 'PgDn';
  SmkcEnd = 'End';
  SmkcHome = 'Home';
  SmkcLeft = 'Left';
  SmkcUp = 'Up';
  SmkcRight = 'Right';
  SmkcDown = 'Down';
  SmkcIns = 'Ins';
  SmkcDel = 'Del';
  SmkcShift = 'Shift+';
  SmkcCtrl = 'Ctrl+';
  SmkcAlt = 'Alt+';
  SmkcWin = 'WinKey+';

  MenuKeyCaps: array[TMenuKeyCap] of string = (
    SmkcBkSp, SmkcTab, SmkcEsc, SmkcEnter, SmkcSpace, SmkcPgUp,
    SmkcPgDn, SmkcEnd, SmkcHome, SmkcLeft, SmkcUp, SmkcRight,
    SmkcDown, SmkcIns, SmkcDel, SmkcShift, SmkcCtrl, SmkcAlt,
    SmkcWin);


  {en Retrieves current modifiers state of the keyboard. }
  function GetKeyShiftStateEx: TShiftState;

  {en
     Tries to translate virtual key (VK_..) into a valid UTF8 character,
     taking into account modifiers state.
     @param(Key
            Virtual key code.)
     @param(ShiftState
            Keyboard modifiers that should be taken into account
            when determining the character.)
  }
  function VirtualKeyToUTF8Char(Key: Byte; ShiftState: TShiftState = []): TUTF8Char;

  {en
     Returns text description of a virtual key trying to take into account
     given modifiers state.
     For keys that have characters assigned it usually returns that character,
     for others some textual description.
     @param(Key
            Virtual key code.)
     @param(ShiftState
            Keyboard modifiers that should be taken into account
            when determining the description.)
     @return(UTF8 character assigned to Key or an empty string.)
  }
  function VirtualKeyToText(Key: Byte; ShiftState: TShiftState = []): string;

{$IFDEF MSWINDOWS}
  {en
     If a virtual key with any modifiers produces valid ANSI or UNICODE character,
     that character is returned in UTF8 encoding.
     @param(Key
            Virtual key code.)
     @param(ExcludeShiftState
            Which modifiers should not be taken into account when
            determining possible character.)
     @return(UTF8 character assigned to Key or an empty string.)
  }
  function GetInternationalCharacter(Key: Word;
                                     ExcludeShiftState: TShiftState = []): TUTF8Char;
{$ENDIF}

  {en
     Initializes keyboard module.
     Should be called after Application.Initialize and with the main form created.
  }
  procedure InitializeKeyboard;
  procedure CleanupKeyboard;

  {en
     Should be called whenever a keyboard layout modification is detected.
  }
  procedure OnKeyboardLayoutChanged;

implementation

uses
  LCLProc, LCLIntf
{$IF DEFINED(MSWINDOWS)}
  , Windows, Win32Proc
{$ENDIF}
{$IF DEFINED(UNIX)}
  , XLib, X
{$ENDIF}
{$IF DEFINED(LCLGTK)}
  , Gdk, GLib
  , GtkProc
{$ENDIF}
{$IF DEFINED(LCLGTK2)}
  , Gdk2, GLib2, GtkExtra
  , GtkProc
{$ENDIF}
{$IF DEFINED(UNIX) and DEFINED(LCLQT)}
  , qt4, qtwidgets
  , xutil, KeySym
  , Forms  // for Application.MainForm
{$ENDIF}
  ;

{$IF DEFINED(UNIX)}
  {$IF DEFINED(LCLGTK)}
var
  XDisplay: PDisplay = nil;
  {$ELSEIF DEFINED(LCLGTK2)}
var
  XDisplay: PGdkDisplay = nil;
  {$ELSEIF DEFINED(LCLQT)}
var
  XDisplay: PDisplay = nil;
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
var
  // True, if the current keyboard layout's right Alt key is mapped as AltGr.
  HasKeyboardAltGrKey : Boolean = False;
{$ENDIF}

{$IF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
var
  // This is set to a virtual key number that AltGr is mapped on.
  VK_ALTGR: Byte = VK_UNDEFINED;
  {$IF DEFINED(LCLGTK2)}
  KeysChangesSignalHandlerId : gulong = 0;
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(UNIX) and DEFINED(LCLQT)}
type
  TKeyboardLayoutChangedHook = class
  private
    EventHook: QObject_hookH;

    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; // called by QT

  public
    constructor Create(QObject: QObjectH);
    destructor Destroy; override;
  end;

var
  // Used to catch "keyboard layout modified" event.
  KeyboardLayoutChangedHook: TKeyboardLayoutChangedHook = nil;

  ShiftMask : Cardinal = 0;
  AltGrMask : Cardinal = 0;
{$ENDIF}


{$IF DEFINED(LCLGTK)}
function XKeycodeToKeysym(para1:PDisplay; para2:TKeyCode; index:integer):TKeySym;cdecl;external libX11;
{$ENDIF}

{$IF DEFINED(UNIX) and DEFINED(LCLQT)}
{en
   Retrieves the character and respective modifiers state
   for the given keysym and given level.
}
procedure XKeysymToUTF8Char(XKeysym: TKeySym; ShiftLevel: Cardinal;
                            out ShiftState: TShiftState; out KeyChar: TUTF8Char);
var
  XKeycode: TKeyCode;
  XKeyEvent: TXKeyEvent;
  KeySymChars: array[0..16] of Char;
  KeySymCharLen: Integer;
  Level: Integer;
begin
  KeyChar := '';
  ShiftState := [];

  XKeycode := XKeysymToKeycode(XDisplay, XKeysym);

  if XKeycode <> 0 then
  begin
    // 4 levels - two groups of two characters each (unshifted/shifted).
    // AltGr is usually the group switch.
    for Level := 0 to 3 do
    begin
      if XKeysym = XKeycodeToKeysym(XDisplay, XKeyCode, Level) then
      begin
        // Init dummy XEvent to retrieve the char corresponding to the keycode.
        FillChar(XKeyEvent, SizeOf(XKeyEvent), 0);
        XKeyEvent._Type := KeyPress;
        XKeyEvent.Display := XDisplay;
        XKeyEvent.Same_Screen := True;
        XKeyEvent.KeyCode := XKeyCode;

        case ShiftLevel of
          0: XKeyEvent.State := 0;                      // 1st group
          1: XKeyEvent.State := ShiftMask;              // 1st group
          2: XKeyEvent.State := AltGrMask;              // 2nd group
          3: XKeyEvent.State := AltGrMask or ShiftMask; // 2nd group
        else
             XKeyEvent.State := 0;
        end;

        // Retrieve the character for this KeySym.
        KeySymCharLen := XLookupString(@XKeyEvent, KeySymChars, SizeOf(KeySymChars), nil, nil);

        // Delete ending zero.
        if (KeySymCharLen > 0) and (KeySymChars[KeySymCharLen - 1] = #0) then
          Dec(KeySymCharLen);

        if KeySymCharLen > 0 then
        begin
          SetString(KeyChar, KeySymChars, KeySymCharLen);

          // Get modifier keys of the found keysym.
          case Level of
            0: ShiftState := [];
            1: ShiftState := [ssShift];
            2: ShiftState := [ssAltGr];
            3: ShiftState := [ssShift, ssAltGr];
          end;
        end;

        Exit;
      end
    end;
  end;
end;
{$ENDIF}

function GetKeyShiftStateEx: TShiftState;
  function IsKeyDown(Key: Integer): Boolean;
  begin
    Result := (GetKeyState(Key) and $8000)<>0;
  end;
begin
  Result:=[];

{$IFDEF MSWINDOWS}
  if HasKeyboardAltGrKey then
  begin
    // Windows maps AltGr as Ctrl+Alt combination, so if AltGr is pressed,
    // it cannot be detected if Ctrl is pressed too. Therefore if AltGr
    // is pressed we don't include Ctrl in the result. Unless Left Alt is also
    // pressed - then we do include it under the assumption that the user
    // pressed Ctrl+Left Alt. The limitation is that a combination of
    // LeftAlt + AltGr is reported as [ssCtrl, ssAlt, ssAltGr].
    if IsKeyDown(VK_LCONTROL) and
       ((not IsKeyDown(VK_RMENU)) or IsKeyDown(VK_LMENU)) then
         Include(Result,ssCtrl);
    if IsKeyDown(VK_RMENU) then
      Include(Result,ssAltGr);
  end
  else
{$ENDIF}
  begin
    if IsKeyDown(VK_RMENU) or IsKeyDown(VK_MENU) then
      Include(Result,ssAlt);
    if IsKeyDown(VK_LCONTROL) or IsKeyDown(VK_CONTROL) then
      Include(Result,ssCtrl);
  end;

{$IF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  if (VK_ALTGR <> VK_UNDEFINED) and IsKeyDown(VK_ALTGR) then
    Include(Result,ssAltGr);
{$ENDIF}

{$IF DEFINED(UNIX) and DEFINED(LCLQT)}
  // QtGroupSwitchModifier is only recognized on X11.
  if (QApplication_keyboardModifiers and QtGroupSwitchModifier) > 0 then
    Include(Result,ssAltGr);
{$ENDIF}

  if IsKeyDown(VK_RCONTROL) then
    Include(Result,ssCtrl);
  if IsKeyDown(VK_LMENU) then
    Include(Result,ssAlt);

  if IsKeyDown(VK_SHIFT) then
    Include(Result,ssShift);
  if IsKeyDown(VK_LWIN) or IsKeyDown(VK_RWIN) then
    Include(Result,ssSuper);

  if (GetKeyState(VK_CAPITAL) and $1)<>0 then  // Caps-lock toggled
    Include(Result,ssCaps);
end;

function VirtualKeyToUTF8Char(Key: Byte; ShiftState: TShiftState): TUTF8Char;

{$IF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2) or DEFINED(LCLQT))}
  function ShiftStateToXModifierLevel(ShiftState: TShiftState): Cardinal;
  begin
    Result := 0;
    if ssShift in ShiftState then Result := Result or 1;
    if ssAltGr in ShiftState then Result := Result or 2;
  end;
{$ENDIF}

var
{$IF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  KeyInfo: TVKeyInfo;
{$ENDIF}
  ShiftedChar: Boolean;
{$IF DEFINED(UNIX) and DEFINED(LCLQT)}
  KeyChar:TUTF8Char;
  KeySym: TKeySym;
  TempShiftState: TShiftState;
{$ENDIF}
begin
  Result := '';

  // Upper case if either caps-lock is toggled or shift pressed.
  ShiftedChar := (ssCaps in ShiftState) xor (ssShift in ShiftState);

{$IF DEFINED(MSWINDOWS)}

  Result := GetInternationalCharacter(Key, GetKeyShiftStateEx - ShiftState);

{$ELSEIF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}

  KeyInfo := GtkProc.GetVKeyInfo(Key);

  // KeyInfo.KeyChar contains characters according to modifiers:
  // [0] - unshifted            [2] - unshifted + AltGr
  // [1] - shifted              [3] - shifted + AltGr
  // Caps-lock is handled below with ShiftedChar variable.

  Result := KeyInfo.KeyChar[ShiftStateToXModifierLevel(ShiftState)];

{$ELSEIF DEFINED(UNIX) and DEFINED(LCLQT)}

  // For QT we'll use Xlib to get text for a key.

  KeySym := 0;
  case Key of
    VK_0..VK_9:         Result := Char(Ord('0') + Key - VK_0);
    VK_A..VK_Z:         Result := Char(Ord('A') + Key - VK_A);
    VK_NUMPAD0..
      VK_NUMPAD9:       Result := Char(Ord('0') + Key - VK_NUMPAD0);

    VK_MULTIPLY:        KeySym := XK_KP_Multiply;
    VK_ADD:             KeySym := XK_KP_Add;
    VK_SUBTRACT:        KeySym := XK_KP_Subtract;
    VK_DIVIDE:          KeySym := XK_KP_Divide;

    // These VKs might only work for US-layout keyboards.
    VK_OEM_PLUS:        KeySym := XK_plus;
    VK_OEM_MINUS:       KeySym := XK_minus;
    VK_OEM_COMMA:       KeySym := XK_comma;
    VK_OEM_PERIOD:      KeySym := XK_period;
    VK_SEPARATOR:       KeySym := XK_comma;
    VK_DECIMAL:         KeySym := XK_period;
    VK_OEM_1:           KeySym := XK_semicolon;
    VK_OEM_3:           KeySym := XK_quoteleft;
    VK_OEM_4:           KeySym := XK_bracketleft;
    VK_OEM_5:           KeySym := XK_backslash;
    VK_OEM_6:           KeySym := XK_bracketright;
    VK_OEM_7:           KeySym := XK_apostrophe;

    // Some additional keys for QT not mapped in TQtWidget.QtKeyToLCLKey.
    // Based on QT sources: src/gui/kernel/qkeymapper_x11.cpp.
    QtKey_Bar:          KeySym := XK_bar;
    QtKey_Underscore:   KeySym := XK_underscore;
    QtKey_Question:     KeySym := XK_question;
    QtKey_AsciiCircum:  KeySym := XK_asciicircum;

    // $C1 - $DA not used VK space
    // Some of these keys (not translated in QtKeyToLCLKey) are on international keyboards.
    QtKey_Aacute:       KeySym := XK_aacute;
    QtKey_Acircumflex:  KeySym := XK_acircumflex;
    QtKey_Atilde:       KeySym := XK_atilde;
    QtKey_Adiaeresis:   KeySym := XK_adiaeresis;
    QtKey_Aring:        KeySym := XK_aring;
    QtKey_AE:           KeySym := XK_ae;
    QtKey_Ccedilla:     KeySym := XK_ccedilla;
    QtKey_Egrave:       KeySym := XK_egrave;
    QtKey_Eacute:       KeySym := XK_eacute;
    QtKey_Ecircumflex:  KeySym := XK_ecircumflex;
    QtKey_Ediaeresis:   KeySym := XK_ediaeresis;
    QtKey_Igrave:       KeySym := XK_igrave;
    QtKey_Iacute:       KeySym := XK_iacute;
    QtKey_Icircumflex:  KeySym := XK_icircumflex;
    QtKey_Idiaeresis:   KeySym := XK_idiaeresis;
    QtKey_ETH:          KeySym := XK_eth;
    QtKey_Ntilde:       KeySym := XK_ntilde;
    QtKey_Ograve:       KeySym := XK_ograve;
    QtKey_Oacute:       KeySym := XK_oacute;
    QtKey_Ocircumflex:  KeySym := XK_ocircumflex;
    QtKey_Otilde:       KeySym := XK_otilde;
    QtKey_Odiaeresis:   KeySym := XK_odiaeresis;
    QtKey_multiply:     KeySym := XK_multiply;
    QtKey_Ooblique:     KeySym := XK_ooblique;
    QtKey_Ugrave:       KeySym := XK_ugrave;
    QtKey_Uacute:       KeySym := XK_uacute;
  end;

  if KeySym <> 0 then
  begin
    // Get character for a key with the given keysym
    // and with given modifiers applied.
    // Don't care about modifiers state, because we already have it.
    XKeysymToUTF8Char(KeySym, ShiftStateToXModifierLevel(ShiftState),
                      TempShiftState, KeyChar);
    Result := KeyChar;
  end;

{$ELSE}

{$ENDIF}

  // Make upper case if either caps-lock is toggled or shift pressed.
  if Result <> '' then
  begin
  if ShiftedChar then
      Result := UTF8UpperCase(Result)
    else
      Result := UTF8LowerCase(Result);
  end;
end;

function VirtualKeyToText(Key: Byte; ShiftState: TShiftState): string;
var
  Name: string;
{$IF DEFINED(UNIX) and DEFINED(LCLQT)}
  KeyChar: TUTF8Char;
  KeySym: TKeySym;
  TempShiftState: TShiftState;
{$ENDIF}
begin

{$IF DEFINED(UNIX) and DEFINED(LCLQT)}
  // Overwrite behaviour for some keys in QT.
  KeySym := 0;
  case Key of
    QtKey_Bar:         KeySym := XK_bar;                 // VK_F13
    QtKey_Underscore:  KeySym := XK_underscore;          // VK_SLEEP

    // '+' (XK_plus) and 'numpad +' (XK_KP_Add) are both reported as VK_ADD (QtKey_Plus)
    VK_ADD:            KeySym := XK_KP_Add;
    // '*' (XK_multiply) and 'numpad *' (XK_KP_Multiply) are both reported as VK_MULTIPLY (QtKey_Asterisk)
    VK_MULTIPLY:       KeySym := XK_KP_Multiply;
  end;

  if KeySym <> 0 then
  begin
    // Get base character for a key with the given keysym.
    // Don't care about modifiers state, because we already have it.
    XKeysymToUTF8Char(KeySym, 0, TempShiftState, KeyChar);
    Name := KeyChar;
  end
  else
{$ENDIF}

  case Key of
    VK_BACK:
      Name := MenuKeyCaps[mkcBkSp];
    VK_TAB:
      Name := MenuKeyCaps[mkcTab];
    VK_RETURN:
      Name := MenuKeyCaps[mkcEnter];
    VK_ESCAPE:
      Name := MenuKeyCaps[mkcEsc];
    VK_SPACE..VK_DOWN:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + Key - VK_SPACE)];
    VK_INSERT:
      Name := MenuKeyCaps[mkcIns];
    VK_DELETE:
      Name := MenuKeyCaps[mkcDel];
    VK_0..VK_9:
      Name := Chr(Key - VK_0 + Ord('0'));
    VK_A..VK_Z:
      Name := Chr(Key - VK_A + Ord('A'));
    VK_NUMPAD0..VK_NUMPAD9:
      Name := Chr(Key - VK_NUMPAD0 + Ord('0'));
    VK_F1..VK_F24:
      Name := 'F' + IntToStr(Key - VK_F1 + 1);
  else
     Name := VirtualKeyToUTF8Char(Key, []);
  end;

  Result := '';
  if Name <> '' then
  begin
    if ssShift in ShiftState then Result := Result + MenuKeyCaps[mkcShift];
    if ssCtrl  in ShiftState then Result := Result + MenuKeyCaps[mkcCtrl];
    if ssAlt   in ShiftState then Result := Result + MenuKeyCaps[mkcAlt];
    if ssSuper in ShiftState then Result := Result + MenuKeyCaps[mkcWin];
    Result := Result + Name;
  end;
end;

{$IF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
procedure UpdateGtkAltGrVirtualKeyCode;
var
  VKNr: Byte;
  KeyInfo: TVKeyInfo;
{$IFDEF LCLGTK2}
  GdkKey: TGdkKeymapKey = (KeyCode: 0; Group: 0; Level: 0);
{$ENDIF}
  KeyVal: guint;
begin
  VK_ALTGR := VK_UNDEFINED;

  // Search all virtual keys for a scancode of AltGraph.
  for VKNr := Low(Byte) to High(Byte) do
  begin
    KeyInfo := GtkProc.GetVKeyInfo(VKNr);

    if (KeyInfo.KeyCode[True] = 0) and // not extended
       (KeyInfo.KeyCode[False] <> 0) then
    begin
{$IFDEF LCLGTK}
      KeyVal := XKeycodetoKeysym(XDisplay, KeyInfo.KeyCode[False], 0);

      if KeyVal = GDK_ISO_Level3_Shift then  // AltGraph
{$ELSE}
      GdkKey.keycode := KeyInfo.keycode[False];

      KeyVal := gdk_keymap_lookup_key(
                    gdk_keymap_get_for_display(XDisplay),
                    @GdkKey);

      if KeyVal = GDK_KEY_ISO_Level3_Shift then  // AltGraph
{$ENDIF}
      begin
        VK_ALTGR := VKNr;
        Exit;
      end;
    end;
  end;
end;
{$ENDIF}


{$IFDEF MSWINDOWS}
function GetInternationalCharacter(Key: Word;
                                   ExcludeShiftState: TShiftState): TUTF8Char;
var
  KeyboardState: array [0..255] of byte;
  wideChars: widestring;
  asciiChar: AnsiChar;
  IntResult: LongInt;

  function IsKeyDown(Key: Byte): Boolean;
  begin
    Result := (KeyboardState[Key] and $80)<>0;
  end;

begin
  Result := '';

  SetLength(wideChars, 16);  // should be enough

  Windows.GetKeyboardState(KeyboardState);

  // Exclude not wanted modifiers.
  if ssCtrl in ExcludeShiftState then
  begin
    KeyboardState[VK_RCONTROL] := 0;
    if (not HasKeyboardAltGrKey) or
       (ssAltGr in ExcludeShiftState) or
       (not IsKeyDown(VK_RMENU))  // if AltGr not pressed
    then
      KeyboardState[VK_LCONTROL] := 0;
  end;

  if ssAlt in ExcludeShiftState then
  begin
    KeyboardState[VK_LMENU] := 0;
    if (not HasKeyboardAltGrKey) then
      KeyboardState[VK_RMENU] := 0;
  end;

  if ssAltGr in ExcludeShiftState then
  begin
    KeyboardState[VK_RMENU] := 0;
    if not IsKeyDown(VK_LMENU) then // if Left Alt not pressed
      KeyboardState[VK_LCONTROL] := 0;
  end;

  if ssCaps in ExcludeShiftState then
    KeyboardState[VK_CAPITAL] := 0;

  if ssShift in ExcludeShiftState then
  begin
    KeyboardState[VK_LSHIFT] := 0;
    KeyboardState[VK_RSHIFT] := 0;
    KeyboardState[VK_SHIFT] := 0;
  end;

  if (not IsKeyDown(VK_LCONTROL)) and (not IsKeyDown(VK_RCONTROL)) then
    KeyboardState[VK_CONTROL] := 0;
  if (not IsKeyDown(VK_LMENU)) and (not IsKeyDown(VK_RMENU)) then
    KeyboardState[VK_MENU] := 0;

  if Win32Proc.UnicodeEnabledOS then
    begin
      IntResult := Windows.ToUnicode(Key, 0, @KeyboardState, PWChar(wideChars),
                                     Length(wideChars), 0);
      if IntResult = 1 then
        Result := UTF8Copy(UTF16ToUTF8(wideChars), 1, 1);
    end
  else
    begin
      IntResult := Windows.ToAscii(Key, 0, @KeyboardState, @asciiChar, 0);
      if IntResult = 1 then
        Result := AnsiToUtf8(string(asciiChar));
    end;
end;

procedure UpdateKeyboardLayoutAltGrFlag;

  type
    PKBDTABLES = ^KBDTABLES;
    KBDTABLES = record // not packed
      pCharModifers: Pointer;
      pVkToWCharTable: Pointer;
      pDeadKey: Pointer;
      pKeyNames: Pointer;
      pKeyNamesExt: Pointer;
      pKeyNamesDead: Pointer;
      pUsVscToVk: Pointer;
      MaxVscToVk: Byte;
      pVSCToVk_E0: Pointer;
      pVSCToVk_E1: Pointer;
      LocalFlags: DWORD;    // <-- we only need this
      LgMaxD: Byte;
      cbLgEntry: Byte;
      pLigature: Pointer;
    end;

  const
    KBDTABLE_VERSION = 1;
    // Flags
    KLLF_ALTGR = 1;
    //KLLF_SHIFTLOCK = 2;
    //KLLF_LRM_RLM = 4;

  function GetKeyboardLayoutFileName: WideString;
  var
    KeyHandle: HKEY;
    KeyboardLayoutName: array [0..KL_NAMELENGTH-1] of WChar;
    RegistryKey  : WideString = 'SYSTEM\CurrentControlSet\Control\Keyboard Layouts\';
    RegistryValue: WideString = 'Layout File';
    BytesNeeded: DWORD;
  begin
    Result := '';
    // Get current keyboard layout ID.
    if GetKeyboardLayoutNameW(KeyboardLayoutName) then
    begin
      RegistryKey := RegistryKey + PWChar(KeyboardLayoutName);

      // Read corresponding layout dll name from registry.
      if (RegOpenKeyExW(HKEY_LOCAL_MACHINE, PWChar(RegistryKey), 0,
                        KEY_QUERY_VALUE, @KeyHandle) = ERROR_SUCCESS)
         and (KeyHandle <> 0) then
      begin
        if RegQueryValueExW(KeyHandle, PWChar(RegistryValue), nil, nil,
                            nil, @BytesNeeded) = ERROR_SUCCESS then
        begin
          SetLength(Result, BytesNeeded div SizeOf(WChar));
          if RegQueryValueExW(KeyHandle, PWChar(RegistryValue), nil, nil,
                              PByte(PWChar(Result)), @BytesNeeded) = ERROR_SUCCESS then
          begin
            Result := Result + #0; // end with zero to be sure
          end;
        end;

        RegCloseKey(KeyHandle);
      end;
    end;
  end;

  function GetKeyboardLayoutAltGrFlag(LayoutDllFileName: WideString): Boolean;
  type
    TKbdLayerDescriptor = function: PKBDTABLES; stdcall;
  var
    Handle: HMODULE;
    KbdLayerDescriptor: TKbdLayerDescriptor;
    Tables: PKBDTABLES;
  begin
    Result := False;
    // Load the keyboard layout dll.
    Handle := LoadLibraryW(PWChar(LayoutDllFileName));
    if Handle <> 0 then
    begin
      KbdLayerDescriptor := TKbdLayerDescriptor(GetProcAddress(Handle, 'KbdLayerDescriptor'));
      if Assigned(KbdLayerDescriptor) then
      begin
        // Get the layout tables.
        Tables := KbdLayerDescriptor();
        if Assigned(Tables) and (HIWORD(Tables^.LocalFlags) = KBDTABLE_VERSION) then
        begin
          // Read AltGr flag.
          Result := Boolean(Tables^.LocalFlags and KLLF_ALTGR);
        end;
      end;

      FreeLibrary(Handle);
    end;
  end;

var
  FileName: WideString;
begin
  HasKeyboardAltGrKey := False;

  FileName := GetKeyboardLayoutFileName;
  if FileName <> '' then
    HasKeyboardAltGrKey := GetKeyboardLayoutAltGrFlag(FileName);
end;
{$ENDIF}

{$IF DEFINED(UNIX) and DEFINED(LCLQT)}
procedure UpdateModifiersMasks;
var
  Map: PXModifierKeymap;
  KeyCode: PKeyCode;
  KeySym: TKeySym;
  ModifierNr, l, Level: Integer;
begin
  ShiftMask := 0;
  AltGrMask := 0;

  if Assigned(XDisplay) then
  begin
    Map := XGetModifierMapping(XDisplay);
    if Assigned(Map) then
    begin
      KeyCode := Map^.modifiermap;

      for ModifierNr := 0 to 7 do // Xlib uses up to 8 modifiers.
      begin
        // Scan through possible keycodes for each modifier.
        // We're looking for the keycodes assigned to Shift and AltGr.
        for l := 1 to Map^.max_keypermod do
        begin
          if KeyCode^ <> 0 then // Omit zero keycodes.
          begin
            for Level := 0 to 3 do  // Check group 1 and group 2 (each has 2 keysyms)
            begin
              // Translate each keycode to keysym and check
              // if this is the modifier we are looking for.
              KeySym := XKeycodeToKeysym(XDisplay, KeyCode^, Level);

              // If found, assign mask according the the modifier number
              // (Shift by default should be the first modifier).
              case KeySym of
                XK_Mode_switch:
                  AltGrMask := 1 shl ModifierNr;

                XK_Shift_L,
                XK_Shift_R:
                  ShiftMask := 1 shl ModifierNr;
              end;
            end;
          end;

          Inc(KeyCode);
        end;
      end;

      XFreeModifiermap(Map);
    end;
  end;
end;
{$ENDIF}

procedure OnKeyboardLayoutChanged;
begin
{$IFDEF MSWINDOWS}
  UpdateKeyboardLayoutAltGrFlag;
{$ENDIF}
{$IF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  UpdateGtkAltGrVirtualKeyCode;
{$ENDIF}
{$IF DEFINED(UNIX) and DEFINED(LCLQT)}
  UpdateModifiersMasks;
{$ENDIF}
end;

{$IF DEFINED(UNIX) and DEFINED(LCLQT)}
constructor TKeyboardLayoutChangedHook.Create(QObject: QObjectH);
var
  Method: TMethod;
begin
  EventHook := QObject_hook_create(QObject);
  if Assigned(EventHook) then
  begin
    TEventFilterMethod(Method) := @EventFilter;
    QObject_hook_hook_events(EventHook, Method);
  end;
end;

destructor TKeyboardLayoutChangedHook.Destroy;
begin
  if Assigned(EventHook) then
  begin
    QObject_hook_destroy(EventHook);
    EventHook := nil;
  end;
end;

function TKeyboardLayoutChangedHook.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False; // Don't filter any events.

  // Somehow this event won't be sent to the window,
  // unless the user first presses a key inside it.
  if QEvent_type(Event) = QEventKeyboardLayoutChange then
  begin
    OnKeyboardLayoutChanged;
  end;
end;
{$ENDIF}

{$IF DEFINED(UNIX)}
{$IF DEFINED(LCLGTK)}
function EventHandler(GdkXEvent: PGdkXEvent; GdkEvent: PGdkEvent;
                      Data: gpointer): TGdkFilterReturn; cdecl;
var
  XEvent: xlib.PXEvent;
  XMappingEvent: PXMappingEvent;
begin
  Result := GDK_FILTER_CONTINUE;  // Don't filter any events.

  XEvent := xlib.PXEvent(GdkXEvent);

  case XEvent^._type of
    MappingNotify{, 112}:
      begin
        XMappingEvent := PXMappingEvent(XEvent);
        case XMappingEvent^.request of
          MappingModifier,
          MappingKeyboard:
          begin
            XRefreshKeyboardMapping(XMappingEvent);
            OnKeyboardLayoutChanged;
          end;
          // Don't care about MappingPointer.
        end;
      end;
  end;
end;
{$ELSEIF DEFINED(LCLGTK2)}
procedure KeysChangedSignalHandler(keymap: PGdkKeymap; Data: gpointer); cdecl;
begin
  OnKeyboardLayoutChanged;
end;
{$ENDIF}
{$ENDIF}

procedure UnhookKeyboardLayoutChanged;
begin
{$IF DEFINED(UNIX) and DEFINED(LCLQT)}

  if Assigned(KeyboardLayoutChangedHook) then
    FreeAndNil(KeyboardLayoutChangedHook);

{$ELSEIF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}

  {$IF DEFINED(LCLGTK)}

  gdk_window_remove_filter(nil, @EventHandler, nil);

  {$ELSEIF DEFINED(LCLGTK2)}

  if (KeysChangesSignalHandlerId <> 0)
  and g_signal_handler_is_connected(gdk_keymap_get_for_display(XDisplay),
                                    KeysChangesSignalHandlerId) then
  begin
    g_signal_handler_disconnect(gdk_keymap_get_for_display(XDisplay),
                                KeysChangesSignalHandlerId);
    KeysChangesSignalHandlerId := 0;
  end;

  {$ENDIF}

{$ENDIF}
end;

procedure HookKeyboardLayoutChanged;
begin
  UnhookKeyboardLayoutChanged;

  // On Unix (X server) the event for changing keyboard layout
  // is sent twice (on QT, GTK1 and GTK2).

{$IF DEFINED(UNIX) and DEFINED(LCLQT)}

  KeyboardLayoutChangedHook := KeyboardLayoutChangedHook.Create(
                               TQtWidget(Application.MainForm.Handle).TheObject);

{$ELSEIF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}

  // On GTK1 XLib's MappingNotify event is used to detect keyboard mapping changes.
  // On GTK2 however (at least on my system), an event of type 112 instead of 34
  // (which is a correct value for MappingNotify) is received, yet max value for
  // an event is 35. So, on GTK2 a GdkKeymap signal is used instead.

  {$IF DEFINED(LCLGTK)}

  gdk_window_add_filter(nil, @EventHandler, nil); // Filter events for all windows.

  {$ELSEIF DEFINED(LCLGTK2)}

  // Connect to GdkKeymap object for the given display.
  KeysChangesSignalHandlerId :=
      g_signal_connect(gdk_keymap_get_for_display(XDisplay),
                       'keys-changed',
                       TGCallback(@KeysChangedSignalHandler), nil);

  {$ENDIF}

{$ENDIF}
end;

procedure InitializeKeyboard;
begin
  OnKeyboardLayoutChanged;
  HookKeyboardLayoutChanged;
end;

procedure CleanupKeyboard;
begin
  UnhookKeyboardLayoutChanged;
end;


initialization
{$IF DEFINED(UNIX)}
  // Get connection to X server.
  {$IF DEFINED(LCLGTK)}
  XDisplay := gdk_display;
  {$ELSEIF DEFINED(LCLGTK2)}
  XDisplay := gdk_display_get_default;
  {$ELSEIF DEFINED(LCLQT)}
  XDisplay := XOpenDisplay(nil);
  {$ENDIF}
{$ENDIF}


finalization
{$IF DEFINED(UNIX) and DEFINED(LCLQT)}
  XCloseDisplay(XDisplay);
{$ENDIF}

end.

