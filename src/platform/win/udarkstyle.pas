{
  Double Commander
  -------------------------------------------------------------------------
  Dark mode support unit (Windows 10 + Qt5).

  Copyright (C) 2019 Richard Yu
  Copyright (C) 2019-2020 Alexander Koblov (alexx2000@mail.ru)

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

unit uDarkStyle;

{$mode delphi}

interface

uses
  Classes, SysUtils, Windows;

var
  g_buildNumber: DWORD = 0;
  g_darkModeEnabled: bool = false;
  g_darkModeSupported: bool = false;

procedure ApplyDarkStyle;
procedure RefreshTitleBarThemeColor(hWnd: HWND);
function AllowDarkModeForWindow(hWnd: HWND; allow: bool): bool;

implementation

uses
  UxTheme, JwaWinUser, Qt5;

type
  // Insider 18334
  PreferredAppMode =
  (
    Default,
    AllowDark,
    ForceDark,
    ForceLight,
    Max
  );

  PWindowCompositionAttribData = ^TWindowCompositionAttribData;
  TWindowCompositionAttribData = record
    dwAttrib: DWORD;
    pvData: PVOID;
    cbData: SIZE_T;
  end;

var
  RtlGetNtVersionNumbers: procedure(major, minor, build: LPDWORD); stdcall;
  _SetWindowCompositionAttribute: function(hWnd: HWND; data: PWINDOWCOMPOSITIONATTRIBDATA): BOOL; stdcall;
  // 1809 17763
  _ShouldAppsUseDarkMode: function(): bool; stdcall; // ordinal 132
  _AllowDarkModeForWindow: function(hWnd: HWND; allow: bool): bool; stdcall; // ordinal 133
  _AllowDarkModeForApp: function(allow: bool): bool; stdcall; // ordinal 135, removed since 18334
  _RefreshImmersiveColorPolicyState: procedure(); stdcall; // ordinal 104
  _IsDarkModeAllowedForWindow: function(hWnd: HWND): bool; stdcall; // ordinal 137
  // Insider 18334
  _SetPreferredAppMode: function(appMode: PreferredAppMode): PreferredAppMode; stdcall; // ordinal 135, since 18334

function AllowDarkModeForWindow(hWnd: HWND; allow: bool): bool;
begin
  if (g_darkModeSupported) then
    Result:= _AllowDarkModeForWindow(hWnd, allow)
  else
    Result:= false;
end;

function IsHighContrast(): bool;
var
  highContrast: HIGHCONTRASTW;
begin
  highContrast.cbSize:= SizeOf(HIGHCONTRASTW);
  if (SystemParametersInfoW(SPI_GETHIGHCONTRAST, SizeOf(highContrast), @highContrast, 0)) then
    Result:= (highContrast.dwFlags and HCF_HIGHCONTRASTON <> 0)
  else
    Result:= false;
end;

procedure RefreshTitleBarThemeColor(hWnd: HWND);
const
  WCA_USEDARKMODECOLORS = 26;
var
  dark: BOOL;
  data: TWindowCompositionAttribData;
begin
  dark:= (_IsDarkModeAllowedForWindow(hWnd) and
	  _ShouldAppsUseDarkMode() and not IsHighContrast());

  if (g_buildNumber < 18362) then
    SetPropW(hWnd, 'UseImmersiveDarkModeColors', THandle(dark))
  else if Assigned(_SetWindowCompositionAttribute) then
  begin
    data.pvData:= @dark;
    data.cbData:= SizeOf(dark);
    data.dwAttrib:= WCA_USEDARKMODECOLORS;
    _SetWindowCompositionAttribute(hWnd, @data);
  end;
end;

procedure AllowDarkModeForApp(allow: bool);
begin
  if Assigned(_AllowDarkModeForApp) then
    _AllowDarkModeForApp(allow)
  else if Assigned(_SetPreferredAppMode) then
  begin
    if (allow) then
      _SetPreferredAppMode(AllowDark)
    else
      _SetPreferredAppMode(Default);
  end;
end;

procedure ApplyDarkStyle;
const
  StyleName: WideString = 'Fusion';
var
  AColor: TQColor;
  APalette: QPaletteH;

  function QColor(R: Integer; G: Integer; B: Integer; A: Integer = 255): PQColor;
  begin
    Result:= @AColor;
    QColor_fromRgb(Result, R, G, B, A);
  end;

begin
  if not g_darkModeEnabled then Exit;

  QApplication_setStyle(QStyleFactory_create(@StyleName));

  APalette:= QPalette_Create();

  // Modify palette to dark
  QPalette_setColor(APalette, QPaletteWindow, QColor(53, 53, 53));
  QPalette_setColor(APalette, QPaletteWindowText, QColor(255, 255, 255));
  QPalette_setColor(APalette, QPaletteDisabled, QPaletteWindowText, QColor(127, 127, 127));
  QPalette_setColor(APalette, QPaletteBase, QColor(42, 42, 42));
  QPalette_setColor(APalette, QPaletteAlternateBase, QColor(66, 66, 66));
  QPalette_setColor(APalette, QPaletteToolTipBase, QColor(255, 255, 255));
  QPalette_setColor(APalette, QPaletteToolTipText, QColor(53, 53, 53));
  QPalette_setColor(APalette, QPaletteText, QColor(255, 255, 255));
  QPalette_setColor(APalette, QPaletteDisabled, QPaletteText, QColor(127, 127, 127));
  QPalette_setColor(APalette, QPaletteDark, QColor(35, 35, 35));
  QPalette_setColor(APalette, QPaletteShadow, QColor(20, 20, 20));
  QPalette_setColor(APalette, QPaletteButton, QColor(53, 53, 53));
  QPalette_setColor(APalette, QPaletteButtonText, QColor(255, 255, 255));
  QPalette_setColor(APalette, QPaletteDisabled, QPaletteButtonText, QColor(127, 127, 127));
  QPalette_setColor(APalette, QPaletteBrightText, QColor(255, 0, 0));
  QPalette_setColor(APalette, QPaletteLink, QColor(42, 130, 218));
  QPalette_setColor(APalette, QPaletteHighlight, QColor(42, 130, 218));
  QPalette_setColor(APalette, QPaletteDisabled, QPaletteHighlight, QColor(80, 80, 80));
  QPalette_setColor(APalette, QPaletteHighlightedText, QColor(255, 255, 255));
  QPalette_setColor(APalette, QPaletteDisabled, QPaletteHighlightedText, QColor(127, 127, 127));

  QApplication_setPalette(APalette);
end;

const
  LOAD_LIBRARY_SEARCH_SYSTEM32 = $800;

function CheckBuildNumber(buildNumber: DWORD): Boolean; inline;
begin
  Result := (buildNumber = 17763) or // 1809
            (buildNumber = 18362) or // 1903
            (buildNumber = 18363);   // 1909
end;

procedure InitDarkMode();
var
  hUxtheme: HMODULE;
  major, minor: DWORD;
begin
  @RtlGetNtVersionNumbers := GetProcAddress(GetModuleHandleW('ntdll.dll'), 'RtlGetNtVersionNumbers');
  if Assigned(RtlGetNtVersionNumbers) then
  begin
    RtlGetNtVersionNumbers(@major, @minor, @g_buildNumber);
    g_buildNumber:= g_buildNumber and not $F0000000;
    if (major = 10) and (minor = 0) and CheckBuildNumber(g_buildNumber) then
    begin
      hUxtheme := LoadLibraryExW('uxtheme.dll', 0, LOAD_LIBRARY_SEARCH_SYSTEM32);
      if (hUxtheme <> 0) then
      begin
        @_RefreshImmersiveColorPolicyState := GetProcAddress(hUxtheme, MAKEINTRESOURCEA(104));
        @_ShouldAppsUseDarkMode := GetProcAddress(hUxtheme, MAKEINTRESOURCEA(132));
        @_AllowDarkModeForWindow := GetProcAddress(hUxtheme, MAKEINTRESOURCEA(133));

        if (g_buildNumber < 18362) then
	  @_AllowDarkModeForApp := GetProcAddress(hUxtheme, MAKEINTRESOURCEA(135))
        else
	  @_SetPreferredAppMode := GetProcAddress(hUxtheme, MAKEINTRESOURCEA(135));

        @_IsDarkModeAllowedForWindow := GetProcAddress(hUxtheme, MAKEINTRESOURCEA(137));

        @_SetWindowCompositionAttribute := GetProcAddress(GetModuleHandleW(user32), 'SetWindowCompositionAttribute');

        if  Assigned(_RefreshImmersiveColorPolicyState) and
	    Assigned(_ShouldAppsUseDarkMode) and
	    Assigned(_AllowDarkModeForWindow) and
	    (Assigned(_AllowDarkModeForApp) or Assigned(_SetPreferredAppMode)) and
	    Assigned(_IsDarkModeAllowedForWindow) then
        begin
          g_darkModeSupported := true;
          AllowDarkModeForApp(true);
          _RefreshImmersiveColorPolicyState();
          g_darkModeEnabled := _ShouldAppsUseDarkMode() and not IsHighContrast();
        end;
      end;
    end;
  end;
end;

initialization
  InitDarkMode;

end.

