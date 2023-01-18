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
  TMenuKeyCap = (mkcClear, mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns,
    mkcDel, mkcShift, mkcCtrl, mkcAlt, mkcWin, mkcNumDivide, mkcNumMultiply,
    mkcNumAdd, mkcNumSubstract);

const
  SmkcClear = 'Clear';
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
  SmkcNumDivide = 'Num/';
  SmkcNumMultiply = 'Num*';
  SmkcNumAdd = 'Num+';
  SmkcNumSubstract = 'Num-';
  SmkcSuper = {$IF DEFINED(DARWIN)}SmkcWin{$ELSE}SmkcCtrl{$ENDIF};

  MenuKeyCaps: array[TMenuKeyCap] of string = (
    SmkcClear, SmkcBkSp, SmkcTab, SmkcEsc, SmkcEnter, SmkcSpace, SmkcPgUp,
    SmkcPgDn, SmkcEnd, SmkcHome, SmkcLeft, SmkcUp, SmkcRight, SmkcDown,
    SmkcIns, SmkcDel, SmkcShift, SmkcCtrl, SmkcAlt, SmkcWin,
    SmkcNumDivide, SmkcNumMultiply, SmkcNumAdd, SmkcNumSubstract);

  // Modifiers that can be used for shortcuts (non-toggable).
  KeyModifiersShortcut = [ssShift, ssAlt, ssCtrl, ssMeta, ssSuper, ssHyper, ssAltGr];
  // Modifiers that change meaning of entered text (case, non-ASCII characters).
  KeyModifiersText = [ssShift, ssAltGr, ssCaps];
  // Modifiers that can be used for shortcuts without taking into account text modifiers.
  KeyModifiersShortcutNoText = KeyModifiersShortcut - KeyModifiersText;

  {en Retrieves current modifiers state of the keyboard. }
  function GetKeyShiftStateEx: TShiftState;

  function KeyToShortCutEx(Key: Word; Shift: TShiftState): TShortCut;
  function ModifiersTextToShortcutEx(const ModifiersText: String; out ModLength: Integer): TShortCut;
  {en Changes order of modifiers in text to always be the same. }
  function NormalizeModifiers(ShortCutText: String): String;
  function ShiftToShortcutEx(ShiftState: TShiftState): TShortCut;
  function ShiftToTextEx(ShiftState: TShiftState): String;
  function ShortcutToShiftEx(Shortcut: TShortCut): TShiftState;
  function ShortCutToTextEx(ShortCut: TShortCut): String;
  function TextToShortCutEx(const ShortCutText: String): TShortCut;

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

  function IsShortcutConflictingWithOS(Shortcut: String): Boolean;

  {en
     Initializes keyboard module.
     Should be called after Application.Initialize.
  }
  procedure InitializeKeyboard;
  procedure CleanupKeyboard;
  {en
     Should be called after main form has been created.
  }
  procedure HookKeyboardLayoutChanged;

  {en
     Should be called whenever a keyboard layout modification is detected.
  }
  procedure OnKeyboardLayoutChanged;

{$IFDEF MSWINDOWS}
var
  // True, if the current keyboard layout's right Alt key is mapped as AltGr.
  HasKeyboardAltGrKey : Boolean = False;
{$ENDIF}

implementation

{$IF DEFINED(UNIX) AND NOT (DEFINED(DARWIN) OR DEFINED(HAIKU))}
{$DEFINE X11}
{$ENDIF}

uses
  LCLProc, LCLIntf, LazUTF8
{$IF DEFINED(MSWINDOWS)}
  , Windows
{$ENDIF}
{$IF DEFINED(LCLGTK)}
  , Gdk, GLib
  , GtkProc
  , XLib, X
{$ENDIF}
{$IF DEFINED(LCLGTK2)}
  , Gdk2, GLib2, Gtk2Extra
  , Gtk2Proc
{$ENDIF}
{$IF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
  {$IF DEFINED(LCLQT)}
  , qt4, qtwidgets
  {$ELSEIF DEFINED(LCLQT5)}
  , qt5, qtwidgets
  {$ELSEIF DEFINED(LCLQT6)}
  , qt6, qtwidgets
  {$ENDIF}
  , XLib, X
  , xutil, KeySym
  , Forms  // for Application.MainForm
{$ENDIF}
  ;

type
  TModifiersMap = record
    Shift:    TShiftStateEnum;
    Shortcut: TShortCut;
    Text:     TMenuKeyCap;
  end;

const
  ModifiersMap: array [0..3] of TModifiersMap =
   ((Shift: ssCtrl;  Shortcut: scCtrl;  Text: mkcCtrl),
    (Shift: ssShift; Shortcut: scShift; Text: mkcShift),
    (Shift: ssAlt;   Shortcut: scAlt;   Text: mkcAlt),
    (Shift: ssMeta;  Shortcut: scMeta;  Text: mkcWin)
    );

{$IF DEFINED(X11)}
var
  {$IF DEFINED(LCLGTK)}
  XDisplay: PDisplay = nil;
  {$ELSEIF DEFINED(LCLGTK2)}
  XDisplay: PGdkDisplay = nil;
  {$ELSEIF (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
  XDisplay: PDisplay = nil;
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
var
  // This is set to a virtual key number that AltGr is mapped on.
  VK_ALTGR: Byte = VK_UNDEFINED;
  {$IF DEFINED(LCLGTK2)}
  KeysChangesSignalHandlerId : gulong = 0;
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
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

var
  VKToCharArray: array[Low(Byte)..High(Byte)] of String;

{$IF DEFINED(LCLGTK)}
function XKeycodeToKeysym(para1:PDisplay; para2:TKeyCode; index:integer):TKeySym;cdecl;external libX11;
{$ENDIF}

procedure CacheVKToChar;
var
  Key: Byte;
begin
  for Key := Low(VKToCharArray) to High(VKToCharArray) do
    case Key of
      VK_BACK:
        VKToCharArray[Key] := MenuKeyCaps[mkcBkSp];
      VK_TAB:
        VKToCharArray[Key] := MenuKeyCaps[mkcTab];
      VK_CLEAR:
        VKToCharArray[Key] := MenuKeyCaps[mkcClear];
      VK_RETURN:
        VKToCharArray[Key] := MenuKeyCaps[mkcEnter];
      VK_ESCAPE:
        VKToCharArray[Key] := MenuKeyCaps[mkcEsc];
      VK_SPACE..VK_DOWN:
        VKToCharArray[Key] := MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + Key - VK_SPACE)];
      VK_INSERT:
        VKToCharArray[Key] := MenuKeyCaps[mkcIns];
      VK_DELETE:
        VKToCharArray[Key] := MenuKeyCaps[mkcDel];
      VK_0..VK_9:
        VKToCharArray[Key] := Chr(Key - VK_0 + Ord('0'));
      VK_A..VK_Z:
        VKToCharArray[Key] := Chr(Key - VK_A + Ord('A'));
      VK_NUMPAD0..VK_NUMPAD9:
        VKToCharArray[Key] := Chr(Key - VK_NUMPAD0 + Ord('0'));
      VK_DIVIDE:
        VKToCharArray[Key] := MenuKeyCaps[mkcNumDivide];
      VK_MULTIPLY:
        VKToCharArray[Key] := MenuKeyCaps[mkcNumMultiply];
      VK_SUBTRACT:
        VKToCharArray[Key] := MenuKeyCaps[mkcNumSubstract];
      VK_ADD:
        VKToCharArray[Key] := MenuKeyCaps[mkcNumAdd];
      VK_F1..VK_F24:
        VKToCharArray[Key] := 'F' + IntToStr(Key - VK_F1 + 1);
      VK_LCL_MINUS:
        VKToCharArray[Key] := '-';
      VK_LCL_EQUAL:
        VKToCharArray[Key] := '=';
      VK_LCL_OPEN_BRACKET:
        VKToCharArray[Key] := '[';
      VK_LCL_CLOSE_BRACKET:
        VKToCharArray[Key] := ']';
      VK_LCL_BACKSLASH:
        VKToCharArray[Key] := '\';
      VK_LCL_SEMI_COMMA:
        VKToCharArray[Key] := ';';
      VK_LCL_QUOTE:
        VKToCharArray[Key] := '''';
      VK_LCL_COMMA:
        VKToCharArray[Key] := ',';
      VK_LCL_POINT:
        VKToCharArray[Key] := '.';
      VK_LCL_SLASH:
        VKToCharArray[Key] := '/';
      VK_LCL_TILDE:
        VKToCharArray[Key] := '`';
    else
      VKToCharArray[Key] := VirtualKeyToUTF8Char(Key, []);
    end;
end;

{$IF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
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
        XKeyEvent.Same_Screen := TBool(1); // True
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

{$IF DEFINED(LCLGTK2) and DEFINED(X11)}
  function GetKeyState(nVirtKey: Integer): Smallint;
  var
    Mask, State: TGdkModifierType;
  begin
    Result := LCLIntf.GetKeyState(nVirtKey);
    case nVirtKey of
      VK_SHIFT, VK_LSHIFT, VK_RSHIFT       : Mask := GDK_SHIFT_MASK;
      VK_CONTROL, VK_LCONTROL, VK_RCONTROL : Mask := GDK_CONTROL_MASK;
      else                                   Exit;
    end;
    State := -1;
    gdk_window_get_pointer(nil, nil, nil, @State);
    if (State <> -1) and (State and Mask = 0) then Result := 0;
  end;
{$ENDIF}

  function IsKeyDown(Key: Integer): Boolean;
  begin
    Result := (GetKeyState(Key) and $8000) <> 0;
  end;

  procedure GetMouseButtonState;
  var
    bSwapButton: Boolean;
  begin
    bSwapButton:= GetSystemMetrics(SM_SWAPBUTTON) <> 0;
    if IsKeyDown(VK_LBUTTON) then
    begin
      if bSwapButton then
        Include(Result, ssRight)
      else
        Include(Result, ssLeft);
    end;
    if IsKeyDown(VK_RBUTTON) then
    begin
      if bSwapButton then
        Include(Result, ssLeft)
      else
        Include(Result, ssRight);
    end;
  end;

begin
  Result:=[];

  GetMouseButtonState;

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

{$IF DEFINED(X11) and DEFINED(LCLQT)}
  // QtGroupSwitchModifier is only recognized on X11.
  if (QApplication_keyboardModifiers and QtGroupSwitchModifier) > 0 then
    Include(Result,ssAltGr);
{$ENDIF}

  if IsKeyDown(VK_RCONTROL) then
    Include(Result, ssCtrl);
  if IsKeyDown(VK_LMENU) then
    Include(Result, ssAlt);

  if IsKeyDown(VK_SHIFT) then
    Include(Result, ssShift);
  if IsKeyDown(VK_LWIN) or IsKeyDown(VK_RWIN) then
    Include(Result, ssMeta);

  if (GetKeyState(VK_CAPITAL) and $1) <> 0 then  // Caps-lock toggled
    Include(Result, ssCaps);
end;

function KeyToShortCutEx(Key: Word; Shift: TShiftState): TShortCut;
begin
  Result := (Key and $FF) or ShiftToShortcutEx(Shift);
end;

function ModifiersTextToShortcutEx(const ModifiersText: String; out ModLength: Integer): TShortCut;
var
  StartPos: Integer;
  i: Integer = 0;
  Found: Boolean = True;

  function CompareFront(const Front: String): Boolean;
  begin
    if (Front <> '') and (StartPos + length(Front) - 1 <= length(ModifiersText)) and
      (AnsiStrLIComp(@ModifiersText[StartPos], PChar(Front), Length(Front)) = 0) then
    begin
      Result := True;
      Inc(StartPos, length(Front));
    end
    else
      Result := False;
  end;

begin
  Result   := 0;
  StartPos := 1;
  while Found do
  begin
    Found := False;
    for i := Low(ModifiersMap) to High(ModifiersMap) do
    begin
      if CompareFront(MenuKeyCaps[ModifiersMap[i].Text]) then
      begin
        Result := Result or ModifiersMap[i].Shortcut;
        Found := True;
        Break;
      end;
    end;
  end;
  ModLength := StartPos - 1;
end;

function NormalizeModifiers(ShortCutText: String): String;
var
  ModLength: Integer;
  Shortcut:  TShortCut;
begin
  Shortcut := ModifiersTextToShortcutEx(ShortCutText, ModLength);
  Result := ShiftToTextEx(ShortcutToShiftEx(Shortcut)) +
            Copy(ShortCutText, ModLength + 1, MaxInt);
end;

function ShiftToShortcutEx(ShiftState: TShiftState): TShortCut;
var
  i: Integer;
begin
  Result := 0;
  for i := Low(ModifiersMap) to High(ModifiersMap) do
  begin
    if ModifiersMap[i].Shift in ShiftState then
      Inc(Result, ModifiersMap[i].Shortcut);
  end;
end;

function ShiftToTextEx(ShiftState: TShiftState): String;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := Low(ModifiersMap) to High(ModifiersMap) do
  begin
    if ModifiersMap[i].Shift in ShiftState then
      Result := Result + MenuKeyCaps[ModifiersMap[i].Text];
  end;
end;

function ShortcutToShiftEx(Shortcut: TShortCut): TShiftState;
var
  i: Integer;
begin
  Result := [];
  for i := Low(ModifiersMap) to High(ModifiersMap) do
  begin
    if Shortcut and ModifiersMap[i].Shortcut <> 0 then
      Include(Result, ModifiersMap[i].Shift);
  end;
end;

function ShortCutToTextEx(ShortCut: TShortCut): String;
begin
  Result := VirtualKeyToText(Byte(ShortCut and $FF), ShortcutToShiftEx(ShortCut));
end;

function TextToShortCutEx(const ShortCutText: String): TShortCut;
var
  Key:       TShortCut;
  Shift:     TShortCut;
  Name:      String;
  StartPos:  Integer;
begin
  Result := 0;
  Shift  := ModifiersTextToShortcutEx(ShortCutText, StartPos);
  Inc(StartPos);

  // Get text for the key if anything left in the string.
  if StartPos <= Length(ShortCutText) then
  begin
    { Copy range from table in ShortCutToText }
    for Key := $08 to $FF do
    begin
      Name := VirtualKeyToText(Key);
      if (Name <> '') and (length(Name) = length(ShortCutText) - StartPos + 1) and
        (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Name), length(Name)) = 0) then
      begin
        Exit(Key or Shift);
      end;
    end;
  end;
end;

function VirtualKeyToUTF8Char(Key: Byte; ShiftState: TShiftState): TUTF8Char;

{$IF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2) or DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
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
{$IF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
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

  KeyInfo := GetVKeyInfo(Key);

  // KeyInfo.KeyChar contains characters according to modifiers:
  // [0] - unshifted            [2] - unshifted + AltGr
  // [1] - shifted              [3] - shifted + AltGr
  // Caps-lock is handled below with ShiftedChar variable.

  Result := KeyInfo.KeyChar[ShiftStateToXModifierLevel(ShiftState)];

{$ELSEIF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}

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
{$IF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
  KeyChar: TUTF8Char;
  KeySym: TKeySym;
  TempShiftState: TShiftState;
{$ENDIF}
begin

{$IF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
  // Overwrite behaviour for some keys in QT.
  case Key of
    QtKey_Bar:         KeySym := XK_bar;                 // VK_F13
    QtKey_Underscore:  KeySym := XK_underscore;          // VK_SLEEP
    QtKey_QuoteLeft:   KeySym := XK_quoteleft;
    else               KeySym := 0;
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

  Name := VKToCharArray[Key];

  if Name <> '' then
    Result := ShiftToTextEx(ShiftState) + Name
  else
    Result := '';
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
    KeyInfo := GetVKeyInfo(VKNr);

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

  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
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

{$IF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
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
{$IF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
  UpdateModifiersMasks;
{$ENDIF}
  CacheVKToChar;
end;

{$IF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
constructor TKeyboardLayoutChangedHook.Create(QObject: QObjectH);
begin
  EventHook := QObject_hook_create(QObject);
  if Assigned(EventHook) then
  begin
    QObject_hook_hook_events(EventHook, @EventFilter);
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
{$IF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}

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

{$IF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}

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

function IsShortcutConflictingWithOS(Shortcut: String): Boolean;
const
  KEY_HIGH = {$IF DEFINED(DARWIN)}28{$ELSE}27{$ENDIF};
const
  ConflictingShortcuts: array [0..KEY_HIGH] of String =
    (SmkcBkSp,                           // Delete previous character
     SmkcDel,                            // Delete next character
     SmkcLeft,                           // Move cursor left
     SmkcRight,                          // Move cursor right
     SmkcSpace,                          // Space
{$IF DEFINED(DARWIN)}
     SmkcWin + SmkcSpace,                // Spotlight (Mac OS X)
{$ENDIF DARWIN}
     SmkcWin,                            // Context menu
     SmkcShift + 'F10',                  // Context menu
     SmkcShift + SmkcDel,                // Cut text
     SmkcShift + SmkcIns,                // Paste text
     SmkcShift + SmkcHome,               // Select to beginning
     SmkcShift + SmkcEnd,                // Select to end
     SmkcShift + SmkcLeft,               // Select previous character
     SmkcShift + SmkcRight,              // Select next character
     SmkcSuper + 'A',                    // Select all
     SmkcSuper + 'C',                    // Copy text
     SmkcSuper + 'V',                    // Paste text
     SmkcSuper + 'X',                    // Cut text
     SmkcSuper + 'Z',                    // Undo
     SmkcSuper + SmkcBkSp,               // Delete previous word
     SmkcSuper + SmkcDel,                // Delete next word
     SmkcSuper + SmkcIns,                // Copy text
     SmkcSuper + SmkcHome,               // Move to beginning
     SmkcSuper + SmkcEnd,                // Move to end
     SmkcSuper + SmkcLeft,               // Move to beginning of word
     SmkcSuper + SmkcRight,              // Move to end of word
     SmkcSuper + SmkcShift + 'Z',        // Redo
     SmkcSuper + SmkcShift + SmkcLeft,   // Select to beginning of word
     SmkcSuper + SmkcShift + SmkcRight); // Select to end of word
var
  i: Integer;
begin
  for i := Low(ConflictingShortcuts) to High(ConflictingShortcuts) do
    if Shortcut = ConflictingShortcuts[i] then
      Exit(True);
  Result := False;
end;

procedure InitializeKeyboard;
begin
  OnKeyboardLayoutChanged;
end;

procedure CleanupKeyboard;
begin
  UnhookKeyboardLayoutChanged;
end;

{$IF DEFINED(X11)}
initialization
  // Get connection to X server.
  {$IF DEFINED(LCLGTK)}
  XDisplay := gdk_display;
  {$ELSEIF DEFINED(LCLGTK2)}
  XDisplay := gdk_display_get_default;
  {$ELSEIF (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
  XDisplay := XOpenDisplay(nil);
  {$ENDIF}
{$ENDIF}

{$IF DEFINED(X11) and (DEFINED(LCLQT) or DEFINED(LCLQT5) OR DEFINED(LCLQT6))}
finalization
  XCloseDisplay(XDisplay);
{$ENDIF}

end.

