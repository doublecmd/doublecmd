unit uKeyboard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType;

  { Retrieves current modifiers state of the keyboard. }
  function GetKeyShiftStateEx: TShiftState;

  {en
     Tries to translate virtual key (VK_..) into a valid UTF8 character,
     taking into account modifiers state.
     @param(Key
            Virtual key code.)
     @param(ShiftState
            Current keyboard modifiers that should be taken into account
            when determining the character.)
  }
  function VirtualKeyToUTF8Char(Key: Word; ShiftState: TShiftState): TUTF8Char;

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

  procedure UpdateKeyboardLayoutAltGrFlag;
{$ENDIF}

{$IF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  procedure UpdateGtkAltGrVirtualKeyCode;
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
{$ENDIF}

implementation

uses
  LCLProc, LCLIntf
{$IF DEFINED(MSWINDOWS)}
  , Windows, Win32Proc
{$ENDIF}
{$IF DEFINED(LCLGTK)}
  , Gdk, GLib, XLib, X
  , GtkProc
{$ENDIF}
{$IF DEFINED(LCLGTK2)}
  , Gdk2, GLib2, GtkExtra
  , GtkProc
{$ENDIF}
{$IF DEFINED(UNIX) and DEFINED(LCLQT)}
  , qt4
{$ENDIF}
  ;

{$IF DEFINED(LCLGTK)}
function XKeycodeToKeysym(para1:PDisplay; para2:TKeyCode; index:integer):TKeySym;cdecl;external libX11;
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

function VirtualKeyToUTF8Char(Key: Word; ShiftState: TShiftState): TUTF8Char;
var
{$IF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  KeyInfo: TVKeyInfo;
{$ENDIF}
  ShiftedChar: Boolean;
begin
  Result := '';

  Key := Byte(Key and $FF);

  // Upper case if either caps-lock is toggled or shift pressed.
  ShiftedChar := (ssCaps in ShiftState) xor (ssShift in ShiftState);

{$IF DEFINED(MSWINDOWS)}
  Result := GetInternationalCharacter(Key, GetKeyShiftStateEx - ShiftState);
{$ELSEIF DEFINED(UNIX) and (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  KeyInfo := GtkProc.GetVKeyInfo(Key);

  if ShiftedChar then
  begin
    if ssAltGr in ShiftState then
      Result := KeyInfo.KeyChar[3]    // shifted + AltGr
    else
      Result := KeyInfo.KeyChar[1];   // shifted
  end
  else
  begin
    if ssAltGr in ShiftState then
      Result := KeyInfo.KeyChar[2]    // unshifted + AltGr
    else
      Result := KeyInfo.KeyChar[0];   // unshifted
  end;
{$ELSE}
  // Allow only keys we know how to translate, because we have no keyboard mapping.
  case Key of
    VK_0..VK_9:
      Result := Char(Ord('0') + Key - VK_0);
    VK_A..VK_Z:
      Result := Char(Ord('A') + Key - VK_A);
    VK_NUMPAD0..VK_NUMPAD9:
      Result := Char(Ord('0') + Key - VK_NUMPAD0);
    VK_OEM_PLUS:
      Result := '+';
    VK_OEM_COMMA:
      Result := ',';
    VK_OEM_MINUS:
      Result := '-';
    VK_OEM_PERIOD:
      Result := '.';
  end;
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
      KeyVal := XKeycodetoKeysym(gdk_display, KeyInfo.KeyCode[False], 0);

      if KeyVal = GDK_ISO_Level3_Shift then  // AltGraph
{$ELSE}
      GdkKey.keycode := KeyInfo.keycode[False];

      KeyVal := gdk_keymap_lookup_key(
                    gdk_keymap_get_for_display(gdk_display_get_default),
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


{$IFDEF MSWINDOWS}
initialization
  UpdateKeyboardLayoutAltGrFlag;
{$ENDIF}

end.

