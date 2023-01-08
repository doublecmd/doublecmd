{
  Double Commander
  -------------------------------------------------------------------------
  Windows dark style widgetset implementation

  Copyright (C) 2021-2023 Alexander Koblov (alexx2000@mail.ru)

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uWin32WidgetSetDark;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  LCLVersion;

procedure ApplyDarkStyle;

implementation

uses
  Classes, SysUtils, Win32Int, WSLCLClasses, Forms, Windows, Win32Proc, Menus,
  Controls, LCLType, Win32WSComCtrls, ComCtrls, LMessages, Win32WSStdCtrls,
  WSStdCtrls, Win32WSControls, StdCtrls, WSControls, Graphics, Themes, LazUTF8,
  UxTheme, Win32Themes, ExtCtrls, WSMenus, JwaWinGDI, FPImage, Math, uDarkStyle,
  WSComCtrls, CommCtrl, uImport, WSForms, Win32WSButtons, Buttons, Win32Extra,
  Win32WSForms, Win32WSSpin, Spin, Win32WSMenus, Dialogs, GraphUtil,
  Generics.Collections, TmSchema, InterfaceBase;

type
  TWinControlDark = class(TWinControl);
  TCustomGroupBoxDark = class(TCustomGroupBox);

type

    { TWin32WSWinControlDark }

    TWin32WSWinControlDark = class(TWin32WSWinControl)
    published
      class function CreateHandle(const AWinControl: TWinControl;
            const AParams: TCreateParams): HWND; override;
    end;

    { TWin32WSStatusBarDark }

    TWin32WSStatusBarDark = class(TWin32WSStatusBar)
    published
      class function CreateHandle(const AWinControl: TWinControl;
            const AParams: TCreateParams): HWND; override;
    end;

    { TWin32WSCustomComboBoxDark }

    TWin32WSCustomComboBoxDark = class(TWin32WSCustomComboBox)
    published
      class function CreateHandle(const AWinControl: TWinControl;
            const AParams: TCreateParams): HWND; override;
      class function GetDefaultColor(const AControl: TControl;
            const ADefaultColorType: TDefaultColorType): TColor; override;
    end;

    { TWin32WSCustomMemoDark }

    TWin32WSCustomMemoDark = class(TWin32WSCustomMemo)
    published
      class function CreateHandle(const AWinControl: TWinControl;
            const AParams: TCreateParams): HWND; override;
    end;

    { TWin32WSCustomListBoxDark }

    TWin32WSCustomListBoxDark = class(TWin32WSCustomListBox)
    published
      class function CreateHandle(const AWinControl: TWinControl;
            const AParams: TCreateParams): HWND; override;
    end;

    { TWin32WSScrollBoxDark }

    TWin32WSScrollBoxDark = class(TWin32WSScrollBox)
    published
      class function CreateHandle(const AWinControl: TWinControl;
            const AParams: TCreateParams): HWND; override;
    end;

    { TWin32WSCustomFormDark }

    TWin32WSCustomFormDark = class(TWin32WSCustomForm)
    published
      class function CreateHandle(const AWinControl: TWinControl;
            const AParams: TCreateParams): HWND; override;
    end;

    { TWin32WSTrackBarDark }

    TWin32WSTrackBarDark = class(TWin32WSTrackBar)
    published
      class function CreateHandle(const AWinControl: TWinControl;
            const AParams: TCreateParams): HWND; override;
      class procedure DefaultWndHandler(const AWinControl: TWinControl;
         var AMessage); override;
    end;

    { TWin32WSPopupMenuDark }

    TWin32WSPopupMenuDark = class(TWin32WSPopupMenu)
    published
      class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
    end;

const
  ID_SUB_SCROLLBOX   = 1;
  ID_SUB_LISTBOX     = 2;
  ID_SUB_COMBOBOX    = 3;
  ID_SUB_STATUSBAR   = 4;
  ID_SUB_TRACKBAR    = 5;

const
  themelib = 'uxtheme.dll';

const
  VSCLASS_DARK_EDIT      = 'DarkMode_CFD::Edit';
  VSCLASS_DARK_TAB       = 'BrowserTab::Tab';
  VSCLASS_DARK_BUTTON    = 'DarkMode_Explorer::Button';
  VSCLASS_DARK_COMBOBOX  = 'DarkMode_CFD::Combobox';
  VSCLASS_DARK_SCROLLBAR = 'DarkMode_Explorer::ScrollBar';
  VSCLASS_PROGRESS_INDER = 'Indeterminate::Progress';

const
  MDL_MENU_SUBMENU     = #$EE#$A5#$B0; // $E970

  MDL_RADIO_FILLED     = #$EE#$A8#$BB; // $EA3B
  MDL_RADIO_CHECKED    = #$EE#$A4#$95; // $E915
  MDL_RADIO_OUTLINE    = #$EE#$A8#$BA; // $EA3A

  MDL_CHECKBOX_FILLED  = #$EE#$9C#$BB; // $E73B
  MDL_CHECKBOX_CHECKED = #$EE#$9C#$BE; // $E73E
  MDL_CHECKBOX_GRAYED  = #$EE#$9C#$BC; // $E73C
  MDL_CHECKBOX_OUTLINE = #$EE#$9C#$B9; // $E739

type
  TThemeClassMap = specialize TDictionary<HTHEME, String>;

var
  Theme: TThemeData;
  ThemeClass: TThemeClassMap;
  OldUpDownWndProc: Windows.WNDPROC;
  CustomFormWndProc: Windows.WNDPROC;
  SysColor: array[0..COLOR_ENDCOLORS] of TColor;
  SysColorBrush: array[0..COLOR_ENDCOLORS] of HBRUSH;
  DefSubclassProc: function(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
  SetWindowSubclass: function(hWnd: HWND; pfnSubclass: SUBCLASSPROC; uIdSubclass: UINT_PTR; dwRefData: DWORD_PTR): BOOL; stdcall;

procedure EnableDarkStyle(Window: HWND);
begin
  AllowDarkModeForWindow(Window, True);
  SetWindowTheme(Window, 'DarkMode_Explorer', nil);
  SendMessageW(Window, WM_THEMECHANGED, 0, 0);
end;

procedure AllowDarkStyle(var Window: HWND);
begin
  if (Window <> 0) then
  begin
    AllowDarkModeForWindow(Window, True);
    Window:= 0;
  end;
end;

function HSVToColor(H, S, V: Double): TColor;
var
  R, G, B: Integer;
begin
  HSVtoRGB(H, S, V, R, G, B);
  R := Min(MAXBYTE, R);
  G := Min(MAXBYTE, G);
  B := Min(MAXBYTE, B);
  Result:= RGBToColor(R, G, B);
end;

function Darker(Color: TColor; Factor: Integer): TColor; forward;

function Lighter(Color: TColor; Factor: Integer): TColor;
var
  H, S, V: Double;
begin
  // Invalid factor
  if (Factor <= 0) then
    Exit(Color);
  // Makes color darker
  if (Factor < 100) then begin
    Exit(darker(Color, 10000 div Factor));
  end;

  ColorToHSV(Color, H, S, V);

  V:= (Factor * V) / 100;
  if (V > High(Word)) then
  begin
    // Overflow, adjust saturation
    S -= V - High(Word);
    if (S < 0) then
      S := 0;
    V:= High(Word);
  end;

  Result:= HSVToColor(H, S, V);
end;

function Darker(Color: TColor; Factor: Integer): TColor;
var
  H, S, V: Double;
begin
  // Invalid factor
  if (Factor <= 0) then
    Exit(Color);
  // Makes color lighter
  if (Factor < 100) then
    Exit(lighter(Color, 10000 div Factor));

  ColorToHSV(Color, H, S, V);
  V := (V * 100) / Factor;

  Result:= HSVToColor(H, S, V);
end;

{
  Fill rectangle gradient
}
function FillGradient(hDC: HDC; Start, Finish: TColor; ARect: TRect; dwMode: ULONG): Boolean;
var
  cc: TFPColor;
  gRect: GRADIENT_RECT;
  vert: array[0..1] of TRIVERTEX;
begin
  cc:= TColorToFPColor(Start);

  vert[0].x      := ARect.Left;
  vert[0].y      := ARect.Top;
  vert[0].Red    := cc.red;
  vert[0].Green  := cc.green;
  vert[0].Blue   := cc.blue;
  vert[0].Alpha  := cc.alpha;

  cc:= TColorToFPColor(ColorToRGB(Finish));

  vert[1].x      := ARect.Right;
  vert[1].y      := ARect.Bottom;
  vert[1].Red    := cc.red;
  vert[1].Green  := cc.green;
  vert[1].Blue   := cc.blue;
  vert[1].Alpha  := cc.alpha;

  gRect.UpperLeft  := 0;
  gRect.LowerRight := 1;
  Result:= JwaWinGDI.GradientFill(hDC, vert, 2, @gRect, 1, dwMode);
end;

function GetNonClientMenuBorderRect(Window: HWND): TRect;
var
  R, W: TRect;
begin
  GetClientRect(Window, @R);
  // Map to screen coordinate space
  MapWindowPoints(Window, 0, @R, 2);
  GetWindowRect(Window, @W);
  OffsetRect(R, -W.Left, -W.Top);
  Result:= Classes.Rect(R.Left, R.Top - 1, R.Right, R.Top);
end;

{
  Set menu background color
}
procedure SetMenuBackground(Menu: HMENU);
var
  MenuInfo: TMenuInfo;
begin
  MenuInfo:= Default(TMenuInfo);
  MenuInfo.cbSize:= SizeOf(MenuInfo);
  MenuInfo.fMask:= MIM_BACKGROUND or MIM_APPLYTOSUBMENUS;
  MenuInfo.hbrBack:= CreateSolidBrush(RGBToColor(45, 45, 45));
  SetMenuInfo(Menu, @MenuInfo);
end;

{
  Set control colors
}
procedure SetControlColors(Control: TControl; Canvas: HDC);
var
  Color: TColor;
begin
  // Set background color
  Color:= Control.Color;
  if Color = clDefault then
  begin
    Color:= Control.GetDefaultColor(dctBrush);
  end;
  SetBkColor(Canvas, ColorToRGB(Color));

  // Set text color
  Color:= Control.Font.Color;
  if Color = clDefault then
  begin
    Color:= Control.GetDefaultColor(dctFont);
  end;
  SetTextColor(Canvas, ColorToRGB(Color));
end;

{ TWin32WSUpDownControlDark }

procedure DrawUpDownArrow(Window: HWND; Canvas: TCanvas; ARect: TRect; AType: TUDAlignButton);
var
  j: integer;
  ax, ay, ah, aw: integer;

  procedure Calculate(var a, b: Integer);
  var
    tmp: Double;
  begin
    tmp:= Double(a + 1) / 2;
    if (tmp > b) then
    begin
      a:= 2 * b - 1;
      b:= (a + 1) div 2;
    end
    else begin
      b:= Round(tmp);
      a:= 2 * b - 1;
    end;
    b:= Max(b, 3);
    a:= Max(a, 5);
  end;

begin
  aw:= ARect.Width div 2;
  ah:= ARect.Height div 2;

  if IsWindowEnabled(Window) then
    Canvas.Pen.Color:= clBtnText
  else begin
    Canvas.Pen.Color:= clGrayText;
  end;
  if (AType in [udLeft, udRight]) then
    Calculate(ah, aw)
  else begin
    Calculate(aw, ah);
  end;
  ax:= ARect.Left + (ARect.Width - aw) div 2;
  ay:= ARect.Top + (ARect.Height - ah) div 2;

  case AType of
    udLeft:
      begin
        for j:= 0 to ah div 2 do
        begin
          Canvas.MoveTo(ax + aw - j - 2, ay + j);
          Canvas.LineTo(ax + aw - j - 2, ay + ah - j - 1);
        end;
      end;
    udRight:
      begin
        for j:= 0 to ah div 2 do
        begin
          Canvas.MoveTo(ax + j, ay + j);
          Canvas.LineTo(ax + j, ay + ah - j - 1);
        end;
      end;
    udTop:
      begin
        for j:= 0 to aw div 2 do
        begin
          Canvas.MoveTo(ax + j, ay + ah - j - 1);
          Canvas.LineTo(ax + aw - j, ay + ah - j - 1);
        end;
      end;
    udBottom:
    begin
      for j:= 0 to aw div 2 do
      begin
        Canvas.MoveTo(ax + j, ay + j);
        Canvas.LineTo(ax + aw - j, ay + j);
      end;
    end;
  end;
end;

function UpDownWndProc(Window: HWND; Msg: UINT; wParam: Windows.WPARAM; lParam: Windows.LPARAM): LRESULT; stdcall;
var
  DC: HDC;
  L, R: TRect;
  rcDst: TRect;
  ARect: TRect;
  PS: PAINTSTRUCT;
  LCanvas : TCanvas;
  LButton, RButton: TUDAlignButton;
begin
  case Msg of
    WM_PAINT:
    begin
      DC := BeginPaint(Window, @ps);
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle:= DC;

        GetClientRect(Window, @ARect);

        LCanvas.Brush.Color:= SysColor[COLOR_BTNFACE];
        LCanvas.FillRect(ps.rcPaint);

        L:= ARect;
        R:= ARect;

        if (GetWindowLongPtr(Window, GWL_STYLE) and UDS_HORZ <> 0) then
        begin
          LButton:= udLeft;
          RButton:= udRight;
          R.Left:= R.Width div 2;
          L.Right:= L.Right - L.Width div 2;
        end
        else begin
          LButton:= udTop;
          RButton:= udBottom;
          R.Top:= R.Height div 2;
          L.Bottom:= L.Bottom - L.Height div 2;
        end;

        if (IntersectRect(rcDst, L, PS.rcPaint)) then
        begin
          LCanvas.Pen.Color:= RGBToColor(38, 38, 38);
          LCanvas.RoundRect(L, 4, 4);
          InflateRect(L, -1, -1);
          LCanvas.Pen.Color:= RGBToColor(92, 92, 92);
          LCanvas.RoundRect(L, 4, 4);
          DrawUpDownArrow(Window, LCanvas, L, LButton);
        end;

        if (IntersectRect(rcDst, R, PS.rcPaint)) then
        begin
          LCanvas.Pen.Color:= RGBToColor(38, 38, 38);
          LCanvas.RoundRect(R, 4, 4);
          InflateRect(R, -1, -1);
          LCanvas.Pen.Color:= RGBToColor(92, 92, 92);
          LCanvas.RoundRect(R, 4, 4);
          DrawUpDownArrow(Window, LCanvas, R, RButton);
        end;
      finally
        LCanvas.Handle:= 0;
        LCanvas.Free;
      end;
      EndPaint(Window, @ps);
      Result:= 0;
    end;
  WM_ERASEBKGND:
    begin
      Exit(1);
    end;
    else begin
      Result:= CallWindowProc(OldUpDownWndProc, Window, Msg, WParam, LParam);
    end;
  end;
end;

{ TWin32WSTrackBarDark }

function TrackBarWindowProc(Window: HWND; Msg: UINT; wParam: Windows.WPARAM; lParam: Windows.LPARAM; uISubClass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
begin
  if Msg = WM_ERASEBKGND then
    Result := 1
  else
    Result := DefSubclassProc(Window, Msg, WParam, LParam);
end;

class function TWin32WSTrackBarDark.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
begin
  AWinControl.Color:= SysColor[COLOR_BTNFACE];
  Result:= inherited CreateHandle(AWinControl, AParams);
  SetWindowSubclass(Result, @TrackBarWindowProc, ID_SUB_TRACKBAR, 0);
end;

class procedure TWin32WSTrackBarDark.DefaultWndHandler(
  const AWinControl: TWinControl; var AMessage);
var
  NMHdr: PNMHDR;
  NMCustomDraw: PNMCustomDraw;
begin
  with TLMessage(AMessage) do
    case Msg of
      CN_NOTIFY:
        begin
          NMHdr := PNMHDR(LParam);
          if NMHdr^.code = NM_CUSTOMDRAW then
          begin
            NMCustomDraw:= PNMCustomDraw(LParam);
            case NMCustomDraw^.dwDrawStage of
              CDDS_PREPAINT:
              begin
                Result := CDRF_NOTIFYITEMDRAW;
              end;
              CDDS_ITEMPREPAINT:
              begin
                case NMCustomDraw^.dwItemSpec of
                  TBCD_CHANNEL:
                    begin
                      Result:= CDRF_SKIPDEFAULT;
                      SelectObject(NMCustomDraw^.hdc, GetStockObject(DC_PEN));
                      SetDCPenColor(NMCustomDraw^.hdc, SysColor[COLOR_BTNSHADOW]);
                      SelectObject(NMCustomDraw^.hdc, GetStockObject(DC_BRUSH));
                      SetDCBrushColor(NMCustomDraw^.hdc, SysColor[COLOR_BTNFACE]);
                      with NMCustomDraw^.rc do
                        RoundRect(NMCustomDraw^.hdc, Left, Top, Right, Bottom, 6, 6);
                    end;
                  else begin
                    Result:= CDRF_DODEFAULT;
                  end;
                end;
              end;
            end;
          end;
        end
      else
        inherited DefaultWndHandler(AWinControl, AMessage);
    end;
end;

{ TWin32WSScrollBoxDark }

function ScrollBoxWindowProc(Window: HWND; Msg: UINT; wParam: Windows.WPARAM; lParam: Windows.LPARAM; uISubClass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
var
  DC: HDC;
  R, W: TRect;
  Delta: Integer;
begin
  Result:= DefSubclassProc(Window, Msg, WParam, LParam);

  if Msg = WM_NCPAINT then
  begin
    GetClientRect(Window, @R);
    MapWindowPoints(Window, 0, @R, 2);
    GetWindowRect(Window, @W);
    Delta:= Abs(W.Top - R.Top);

    DC:= GetWindowDC(Window);
    ExcludeClipRect(DC, Delta, Delta, W.Width - Delta, W.Height - Delta);
    SelectObject(DC, GetStockObject(DC_PEN));
    SelectObject(DC, GetStockObject(DC_BRUSH));
    SetDCPenColor(DC, SysColor[COLOR_BTNSHADOW]);
    SetDCBrushColor(DC, SysColor[COLOR_BTNHIGHLIGHT]);
    Rectangle(DC, 0, 0, W.Width, W.Height);
    ReleaseDC(Window, DC);
  end;
end;

class function TWin32WSScrollBoxDark.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
begin
  Result:= inherited CreateHandle(AWinControl, AParams);
  if TScrollBox(AWinControl).BorderStyle = bsSingle then begin
    SetWindowSubclass(Result, @ScrollBoxWindowProc, ID_SUB_SCROLLBOX, 0);
  end;
  EnableDarkStyle(Result);
end;

{ TWin32WSPopupMenuDark }

class procedure TWin32WSPopupMenuDark.Popup(const APopupMenu: TPopupMenu;
  const X, Y: integer);
begin
  SetMenuBackground(APopupMenu.Handle);

  inherited Popup(APopupMenu, X, Y);
end;

{ TWin32WSWinControlDark }

class function TWin32WSWinControlDark.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  P: TCreateParams;
begin
  P:= AParams;
  if (AWinControl is TCustomTreeView) then
  begin
    AWinControl.Color:= SysColor[COLOR_WINDOW];
    with TCustomTreeView(AWinControl) do
    begin
      ExpandSignType:= tvestPlusMinus;
      TreeLineColor:= SysColor[COLOR_GRAYTEXT];
      ExpandSignColor:= SysColor[COLOR_GRAYTEXT];
    end;
  end;
  P.ExStyle:= p.ExStyle and not WS_EX_CLIENTEDGE;
  TWinControlDark(AWinControl).BorderStyle:= bsNone;
  Result:= inherited CreateHandle(AWinControl, P);
  EnableDarkStyle(Result);
end;

{ TWin32WSCustomFormDark }

function FormWndProc2(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  DC: HDC;
  R: TRect;
begin
  case Msg of
    WM_NCACTIVATE,
    WM_NCPAINT:
    begin
      Result:= CallWindowProc(CustomFormWndProc, Window, Msg, wParam, lParam);

      DC:= GetWindowDC(Window);
      R:= GetNonclientMenuBorderRect(Window);
      FillRect(DC, R, GetSysColorBrush(COLOR_WINDOW));
      ReleaseDC(Window, DC);
    end;
    WM_SHOWWINDOW:
    begin
      AllowDarkModeForWindow(Window, True);
      RefreshTitleBarThemeColor(Window);
    end
    else begin
      Result:= CallWindowProc(CustomFormWndProc, Window, Msg, wParam, lParam);
    end;
  end;
end;

class function TWin32WSCustomFormDark.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  Info: PWin32WindowInfo;
begin
  AWinControl.DoubleBuffered:= True;
  AWinControl.Color:= SysColor[COLOR_BTNFACE];
  AWinControl.Brush.Color:= SysColor[COLOR_BTNFACE];

  Result:= inherited CreateHandle(AWinControl, AParams);

  Info:= GetWin32WindowInfo(Result);

  Info^.DefWndProc:= @WindowProc;

  CustomFormWndProc:= Windows.WNDPROC(SetWindowLongPtr(Result, GWL_WNDPROC, LONG_PTR(@FormWndProc2)));

  AWinControl.Color:= SysColor[COLOR_BTNFACE];
  AWinControl.Font.Color:= SysColor[COLOR_BTNTEXT];
end;

{ TWin32WSCustomListBoxDark }

function ListBoxWindowProc2(Window: HWND; Msg: UINT; wParam: Windows.WPARAM; lParam: Windows.LPARAM; uISubClass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
var
  PS: TPaintStruct;
begin
  if Msg = WM_PAINT then
  begin
    if SendMessage(Window, LB_GETCOUNT, 0, 0) = 0 then
    begin
      BeginPaint(Window, @ps);
      // ListBox:= TCustomListBox(GetWin32WindowInfo(Window)^.WinControl);
      // Windows.FillRect(DC, ps.rcPaint, ListBox.Brush.Reference.Handle);
      EndPaint(Window, @ps);
    end;
  end;
  Result:= DefSubclassProc(Window, Msg, WParam, LParam);
end;

class function TWin32WSCustomListBoxDark.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  P: TCreateParams;
begin
  P:= AParams;
  P.ExStyle:= P.ExStyle and not WS_EX_CLIENTEDGE;
  TCustomListBox(AWinControl).BorderStyle:= bsNone;
  Result:= inherited CreateHandle(AWinControl, P);
  EnableDarkStyle(Result);
  SetWindowSubclass(Result, @ListBoxWindowProc2, ID_SUB_LISTBOX, 0);
  TCustomListBox(AWinControl).Color:= SysColor[COLOR_WINDOW];
  AWinControl.Font.Color:= SysColor[COLOR_WINDOWTEXT];
end;

{ TWin32WSCustomMemoDark }

class function TWin32WSCustomMemoDark.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  P: TCreateParams;
begin
  P:= AParams;
  TCustomEdit(AWinControl).BorderStyle:= bsNone;
  P.ExStyle:= P.ExStyle and not WS_EX_CLIENTEDGE;

  AWinControl.Color:= SysColor[COLOR_WINDOW];
  AWinControl.Font.Color:= SysColor[COLOR_WINDOWTEXT];

  Result:= inherited CreateHandle(AWinControl, P);

  EnableDarkStyle(Result);
end;

{ TWin32WSCustomComboBoxDark }

function ComboBoxWindowProc(Window:HWND; Msg:UINT; wParam:Windows.WPARAM;lparam:Windows.LPARAM;uISubClass : UINT_PTR;dwRefData:DWORD_PTR):LRESULT; stdcall;
var
  DC: HDC;
  ComboBox: TCustomComboBox;
begin
  case Msg of
    WM_CTLCOLORLISTBOX:
    begin
      ComboBox:= TCustomComboBox(GetWin32WindowInfo(Window)^.WinControl);
      DC:= HDC(wParam);
      SetControlColors(ComboBox, DC);
      Exit(LResult(ComboBox.Brush.Reference.Handle));
    end;
  end;
  Result:= DefSubclassProc(Window, Msg, wParam, lParam);
end;

class function TWin32WSCustomComboBoxDark.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  Info: TComboboxInfo;
begin
  AWinControl.Color:= SysColor[COLOR_BTNFACE];
  AWinControl.Font.Color:= SysColor[COLOR_BTNTEXT];

  Result:= inherited CreateHandle(AWinControl, AParams);

  Info.cbSize:= SizeOf(Info);
  Win32Extra.GetComboBoxInfo(Result, @Info);
  EnableDarkStyle(Info.hwndList);

  AllowDarkModeForWindow(Result, True);

  SetWindowSubclass(Result, @ComboBoxWindowProc, ID_SUB_COMBOBOX, 0);
end;

class function TWin32WSCustomComboBoxDark.GetDefaultColor(
  const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor;
const
  DefColors: array[TDefaultColorType] of TColor = (
  { dctBrush } clBtnFace,
  { dctFont  } clBtnText
  );
begin
  Result:= DefColors[ADefaultColorType];
end;

{ TWin32WSStatusBarDark }

function StatusBarWndProc(Window: HWND; Msg: UINT; wParam: Windows.WPARAM; lParam: Windows.LPARAM; uISubClass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
var
  DC: HDC;
  X: Integer;
  Index: Integer;
  PS: TPaintStruct;
  LCanvas: TCanvas;
  APanel: TStatusPanel;
  StatusBar: TStatusBar;
  Info: PWin32WindowInfo;
begin
  Info:= GetWin32WindowInfo(Window);
  if (Info = nil) or (Info^.WinControl = nil) then
  begin
    Result:= CallDefaultWindowProc(Window, Msg, WParam, LParam);
    Exit;
  end;

  if Msg = WM_PAINT then
  begin
    StatusBar:= TStatusBar(Info^.WinControl);

    TWin32WSStatusBar.DoUpdate(StatusBar);

    DC:= BeginPaint(Window, @ps);

    LCanvas:= TCanvas.Create;
    try
      LCanvas.Handle:= DC;
      LCanvas.Brush.Color:= SysColor[COLOR_BTNFACE];
      LCanvas.FillRect(ps.rcPaint);

      X:= 1;
      LCanvas.Font.Color:= clWhite;
      for Index:= 0 to StatusBar.Panels.Count - 1 do
      begin
        APanel:= StatusBar.Panels[Index];
        LCanvas.TextOut(X, 1, APanel.Text);
        X+= APanel.Width;
        {
        LCanvas.Pen.Color:=  Darker(RGBToColor(53, 53, 53), 120);
        LCanvas.Line(x-1, ps.rcPaint.Top, x-1, ps.rcPaint.Bottom);

        LCanvas.Pen.Color:=  Lighter(RGBToColor(53, 53, 53), 154);
        LCanvas.Line(x-2, ps.rcPaint.Top, x-2, ps.rcPaint.Bottom);
        }
      end;
    finally
      LCanvas.Handle:= 0;
      LCanvas.Free;
    end;
    EndPaint(Window, @ps);
    Result:= 0;
  end
  else
    Result:= DefSubclassProc(Window, Msg, WParam, LParam);
end;

class function TWin32WSStatusBarDark.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
begin
  Result:= inherited CreateHandle(AWinControl, AParams);
  SetWindowSubclass(Result, @StatusBarWndProc, ID_SUB_STATUSBAR, 0);
end;

{
  Forward declared functions
}
function InterceptOpenThemeData(hwnd: hwnd; pszClassList: LPCWSTR): hTheme; stdcall; forward;
procedure DrawButton(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; pClipRect: PRECT); forward;

{
  Draws text using the color and font defined by the visual style
}
function DrawThemeTextDark(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR; iCharCount: Integer;
  dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall;
var
  OldColor: COLORREF;
  Index, Element: TThemedElement;
begin
  for Index:= Low(TThemedElement) to High(TThemedElement) do
  begin
    if Theme[Index] = hTheme then
    begin
      Element:= Index;
      Break;
    end;
  end;

  if Element = teToolTip then
    OldColor:= SysColor[COLOR_INFOTEXT]
  else begin
    OldColor:= SysColor[COLOR_BTNTEXT];
  end;
  OldColor:= SetTextColor(hdc, OldColor);
  SetBkMode(hdc, TRANSPARENT);

  DrawTextExW(hdc, pszText, iCharCount, @pRect, dwTextFlags, nil);

  SetTextColor(hdc, OldColor);

  Result:= S_OK;
end;

{
  Draws the border and fill defined by the visual style for the specified control part
}
function DrawThemeBackgroundDark(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
  pClipRect: PRECT): HRESULT; stdcall;
var
  LRect: TRect;
  AColor: TColor;
  LCanvas: TCanvas;
  AStyle: TTextStyle;
  Index, Element: TThemedElement;
begin
  for Index:= Low(TThemedElement) to High(TThemedElement) do
  begin
    if Theme[Index] = hTheme then
    begin
      Element:= Index;
      Break;
    end;
  end;

  if Element = teHeader then
  begin
    if iPartId in [HP_HEADERITEM, HP_HEADERITEMRIGHT] then
    begin
      LCanvas:= TCanvas.Create;
      try
        LCanvas.Handle:= hdc;
        AColor:= SysColor[COLOR_BTNFACE];

        if iStateId in [HIS_HOT, HIS_SORTEDHOT, HIS_ICONHOT, HIS_ICONSORTEDHOT] then
          FillGradient(hdc, Lighter(AColor, 174), Lighter(AColor, 166), pRect, GRADIENT_FILL_RECT_V)
        else
          FillGradient(hdc, Lighter(AColor, 124), Lighter(AColor, 116), pRect, GRADIENT_FILL_RECT_V);

        if (iPartId <> HP_HEADERITEMRIGHT) then
        begin
          LCanvas.Pen.Color:= Lighter(AColor, 104);
          LCanvas.Line(pRect.Right-1, pRect.Top, pRect.Right-1, pRect.Bottom);

          LCanvas.Pen.Color:= Lighter(AColor, 158);
          LCanvas.Line(pRect.Right - 2, pRect.Top, pRect.Right - 2, pRect.Bottom);
        end;
        // Top line
        LCanvas.Pen.Color:= Lighter(AColor, 164);
        LCanvas.Line(pRect.Left, pRect.Top, pRect.Right, pRect.Top);
        // Bottom line
        LCanvas.Pen.Color:= Darker(AColor, 140);
        LCanvas.Line(pRect.Left, pRect.Bottom - 1, pRect.Right, pRect.Bottom - 1);
      finally
        LCanvas.Handle:= 0;
        LCanvas.Free;
      end;
    end;
  end

  else if Element = teMenu then
  begin
    if iPartId in [MENU_BARBACKGROUND, MENU_POPUPITEM, MENU_POPUPGUTTER,
                   MENU_POPUPSUBMENU, MENU_POPUPSEPARATOR, MENU_POPUPCHECK,
                   MENU_POPUPCHECKBACKGROUND] then
    begin
      LCanvas:= TCanvas.Create;
      try
        LCanvas.Handle:= hdc;

        if not (iPartId in [MENU_POPUPSUBMENU, MENU_POPUPCHECK, MENU_POPUPCHECKBACKGROUND]) then
        begin
          if iStateId = MDS_HOT then
            LCanvas.Brush.Color:= SysColor[COLOR_MENUHILIGHT]
          else begin
            LCanvas.Brush.Color:= RGBToColor(45, 45, 45);
          end;
          LCanvas.FillRect(pRect);
        end;

        if iPartId = MENU_POPUPCHECK then
        begin
          AStyle:= LCanvas.TextStyle;
          AStyle.Layout:= tlCenter;
          AStyle.Alignment:= taCenter;
          LCanvas.Brush.Style:= bsClear;
          LCanvas.Font.Name:= 'Segoe MDL2 Assets';
          LCanvas.Font.Color:= RGBToColor(212, 212, 212);
          LCanvas.TextRect(pRect, 0, 0, MDL_CHECKBOX_CHECKED, AStyle);
        end;

        if iPartId = MENU_POPUPSEPARATOR then
        begin
         LRect:= pRect;
         LCanvas.Pen.Color:= RGBToColor(112, 112, 112);
         LRect.Top:= LRect.Top + (LRect.Height div 2);
         LRect.Bottom:= LRect.Top;

         LCanvas.Line(LRect);
        end;

        if (iPartId = MENU_POPUPCHECKBACKGROUND) then
        begin
          LRect:= pRect;
          InflateRect(LRect, -1, -1);
          LCanvas.Pen.Color:= RGBToColor(45, 45, 45);
          LCanvas.Brush.Color:= RGBToColor(81, 81, 81);
          LCanvas.RoundRect(LRect, 6, 6);
        end;

        if iPartId = MENU_POPUPSUBMENU then
        begin
          LCanvas.Brush.Style:= bsClear;
          LCanvas.Font.Name:= 'Segoe MDL2 Assets';
          LCanvas.Font.Color:= RGBToColor(111, 111, 111);
          LCanvas.TextOut(pRect.Left, pRect.Top, MDL_MENU_SUBMENU);
        end;
      finally
        LCanvas.Handle:= 0;
        LCanvas.Free;
      end;
    end;
  end

  else if Element = teToolBar then
  begin
    if iPartId in [TP_BUTTON] then
    begin
      LCanvas:= TCanvas.Create;
      try
        LCanvas.Handle:= hdc;
        AColor:= SysColor[COLOR_BTNFACE];

        if iStateId = TS_HOT then
          LCanvas.Brush.Color:= Lighter(AColor, 116)
        else if iStateId = TS_PRESSED then
          LCanvas.Brush.Color:= Darker(AColor, 116)
        else begin
          LCanvas.Brush.Color:= AColor;
        end;
        LCanvas.FillRect(pRect);

       if iStateId <> TS_NORMAL then
       begin
         if iStateId = TS_CHECKED then
         begin
           LRect:= pRect;
           InflateRect(LRect, -2, -2);
           LCanvas.Brush.Color:= Lighter(AColor, 146);
           LCanvas.FillRect(LRect);
         end;

         LCanvas.Pen.Color:= Darker(AColor, 140);
         LCanvas.RoundRect(pRect, 6, 6);

         LRect:= pRect;

         LCanvas.Pen.Color:= Lighter(AColor, 140);
         InflateRect(LRect, -1, -1);
         LCanvas.RoundRect(LRect, 6, 6);
       end;
      finally
        LCanvas.Handle:= 0;
        LCanvas.Free;
      end;
    end;
  end

  else if Element = teButton then
  begin
    DrawButton(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
  end;

  Result:= S_OK;
end;

var
  __CreateWindowExW: function(dwExStyle: DWORD; lpClassName: LPCWSTR; lpWindowName: LPCWSTR; dwStyle: DWORD; X: longint; Y: longint; nWidth: longint; nHeight: longint; hWndParent: HWND; hMenu: HMENU; hInstance: HINST; lpParam: LPVOID): HWND; stdcall;

function _DrawEdge(hdc: HDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall;
var
  Original: HGDIOBJ;
  ClientRect: TRect;
  ColorDark, ColorLight: TColorRef;

  procedure DrawLine(X1, Y1, X2, Y2: Integer);
  begin
    MoveToEx(hdc, X1, Y1, nil);
    LineTo(hdc, X2, Y2);
  end;

  procedure InternalDrawEdge(Outer: Boolean; const R: TRect);
  var
    X1, Y1, X2, Y2: Integer;
    ColorLeftTop, ColorRightBottom: TColor;
  begin
    X1:= R.Left;
    Y1:= R.Top;
    X2:= R.Right;
    Y2:= R.Bottom;

    ColorLeftTop:= clNone;
    ColorRightBottom:= clNone;

    if Outer then
    begin
      if Edge and BDR_RAISEDOUTER <> 0 then
      begin
        ColorLeftTop:= ColorLight;
        ColorRightBottom:= ColorDark;
      end
      else if Edge and BDR_SUNKENOUTER <> 0 then
      begin
        ColorLeftTop:= ColorDark;
        ColorRightBottom:= ColorLight;
      end;
    end
    else
    begin
      if Edge and BDR_RAISEDINNER <> 0 then
      begin
        ColorLeftTop:= ColorLight;
        ColorRightBottom:= ColorDark;
      end
      else if Edge and BDR_SUNKENINNER <> 0 then
      begin
        ColorLeftTop:= ColorDark;
        ColorRightBottom:= ColorLight;
      end;
    end;

    SetDCPenColor(hdc, ColorLeftTop);

    if grfFlags and BF_LEFT <> 0 then
      DrawLine(X1, Y1, X1, Y2);
    if grfFlags and BF_TOP <> 0 then
      DrawLine(X1, Y1, X2, Y1);

    SetDCPenColor(hdc, ColorRightBottom);

    if grfFlags and BF_RIGHT <> 0 then
      DrawLine(X2, Y1, X2, Y2);
    if grfFlags and BF_BOTTOM <> 0 then
      DrawLine(X1, Y2, X2, Y2);
  end;

begin
  Result:= False;
  if IsRectEmpty(qrc) then
    Exit;

  ClientRect:= qrc;
  Dec(ClientRect.Right, 1);
  Dec(ClientRect.Bottom, 1);
  Original:= SelectObject(hdc, GetStockObject(DC_PEN));
  try
    ColorDark:= SysColor[COLOR_BTNSHADOW];
    ColorLight:= SysColor[COLOR_BTNHIGHLIGHT];

    if Edge and (BDR_SUNKENOUTER or BDR_RAISEDOUTER) <> 0 then
    begin
      InternalDrawEdge(True, ClientRect);
    end;

    InflateRect(ClientRect, -1, -1);

    if Edge and (BDR_SUNKENINNER or BDR_RAISEDINNER) <> 0 then
    begin
      InternalDrawEdge(False, ClientRect);
      InflateRect(ClientRect, -1, -1);
    end;

    Inc(ClientRect.Right);
    Inc(ClientRect.Bottom);

    if grfFlags and BF_ADJUST <> 0 then
    begin
      qrc:= ClientRect;
    end;

    Result:= True;
  finally
    SelectObject(hdc, Original);
  end;
end;

{
  Retrieves the current color of the specified display element
}
function GetSysColorDark(nIndex: longint): DWORD; stdcall;
begin
  if (nIndex >= 0) and (nIndex <= COLOR_ENDCOLORS) then
    Result:= SysColor[nIndex]
  else begin
    Result:= 0;
  end;
end;

{
  Retrieves a handle identifying a logical brush that corresponds to the specified color index
}
function GetSysColorBrushDark(nIndex: longint): HBRUSH; stdcall;
begin
  if (nIndex >= 0) and (nIndex <= COLOR_ENDCOLORS) then
  begin
    if (SysColorBrush[nIndex] = 0) then
    begin
      SysColorBrush[nIndex]:= CreateSolidBrush(SysColor[nIndex]);
    end;
    Result:= SysColorBrush[nIndex];
  end
  else begin
    Result:= CreateSolidBrush(GetSysColorDark(nIndex));
  end;
end;

const
  ClassNameW: PWideChar = 'TCustomForm';
  ClassNameTC: PWideChar = 'TTOTAL_CMD'; // for compatibility with plugins

function _CreateWindowExW(dwExStyle: DWORD; lpClassName: LPCWSTR; lpWindowName: LPCWSTR; dwStyle: DWORD; X: longint; Y: longint; nWidth: longint; nHeight: longint; hWndParent: HWND; hMenu: HMENU; hInstance: HINST; lpParam: LPVOID): HWND; stdcall;
var
  AParams: PNCCreateParams absolute lpParam;
begin
  if Assigned(AParams) and (AParams^.WinControl is TCustomForm) then
  begin
    if (hWndParent = 0) and AParams^.WinControl.ClassNameIs('TfrmMain') then
      lpClassName:= ClassNameTC
    else begin
      lpClassName:= ClassNameW;
    end;
  end else begin
    dwExStyle:= dwExStyle or WS_EX_CONTEXTHELP;
  end;
  Result:= __CreateWindowExW(dwExStyle, lpClassName, lpWindowName, dwStyle, X, Y, nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam);
end;

function TaskDialogIndirectDark(const pTaskConfig: PTASKDIALOGCONFIG; pnButton: PInteger; pnRadioButton: PInteger; pfVerificationFlagChecked: PBOOL): HRESULT; stdcall;
const
  BTN_USER = $1000;
var
  Idx: Integer;
  Index: Integer;
  Button: TDialogButton;
  Buttons: TDialogButtons;
  DlgType: Integer = idDialogInfo;
begin
  with pTaskConfig^ do
  begin
    if (pszMainIcon = TD_INFORMATION_ICON) then
      DlgType:= idDialogInfo
    else if (pszMainIcon = TD_WARNING_ICON) then
      DlgType:= idDialogWarning
    else if (pszMainIcon = TD_ERROR_ICON) then
      DlgType:= idDialogError
    else if (pszMainIcon = TD_SHIELD_ICON) then
      DlgType:= idDialogShield
    else if (dwFlags and TDF_USE_HICON_MAIN <> 0) then
    begin
      if (hMainIcon = Windows.LoadIcon(0, IDI_QUESTION)) then
        DlgType:= idDialogConfirm;
    end;

    Buttons:= TDialogButtons.Create(TDialogButton);
    try
      for Index:= 0 to cButtons - 1 do
      begin
        Button:= Buttons.Add;
        Idx:= pButtons[Index].nButtonID;
        Button.ModalResult:= (Idx + BTN_USER);
        Button.Default:= (Idx = nDefaultButton);
        Button.Caption:= UTF8Encode(UnicodeString(pButtons[Index].pszButtonText));
      end;

      Result:= DefaultQuestionDialog(UTF8Encode(UnicodeString(pszWindowTitle)),
                                     UTF8Encode(UnicodeString(pszContent)), DlgType, Buttons, 0);

      if Assigned(pnButton) then
      begin
        if (Result < BTN_USER) then
          pnButton^:= Result
        else begin
          pnButton^:= Result - BTN_USER;
        end;
      end;
    finally
      Buttons.Free;
    end;
  end;
  Result:= S_OK;
end;

procedure SubClassUpDown;
var
  Window: HWND;
begin
  Window:= CreateWindowW(UPDOWN_CLASSW, nil, 0, 0, 0, 200, 20, 0, 0, HINSTANCE, nil);
  OldUpDownWndProc:= Windows.WNDPROC(GetClassLongPtr(Window, GCLP_WNDPROC));

  SetClassLongPtr(Window, GCLP_WNDPROC, LONG_PTR(@UpDownWndProc));
  DestroyWindow(Window);
end;

procedure ScreenFormEvent(Self, Sender: TObject; Form: TCustomForm);
begin
  if Assigned(Form.Menu) then
  begin
    Form.Menu.OwnerDraw:= True;
    SetMenuBackground(GetMenu(Form.Handle));
    Form.Menu.OwnerDraw:= False;
  end;
end;

{
  Override several widgetset controls
}
procedure ApplyDarkStyle;
var
  Handler: TMethod;
  Index: TThemedElement;
begin
  if not g_darkModeEnabled then
    Exit;

  SubClassUpDown;

  OpenThemeData:= @InterceptOpenThemeData;

  DefBtnColors[dctFont]:= SysColor[COLOR_BTNTEXT];
  DefBtnColors[dctBrush]:= SysColor[COLOR_BTNFACE];

  Handler.Code:= @ScreenFormEvent;
  Screen.AddHandlerFormVisibleChanged(TScreenFormEvent(Handler), True);

  with TWinControl.Create(nil) do Free;
  RegisterWSComponent(TWinControl, TWin32WSWinControlDark);

  WSComCtrls.RegisterStatusBar;
  RegisterWSComponent(TStatusBar, TWin32WSStatusBarDark);

  WSStdCtrls.RegisterCustomComboBox;
  RegisterWSComponent(TCustomComboBox, TWin32WSCustomComboBoxDark);

  WSStdCtrls.RegisterCustomEdit;

  WSStdCtrls.RegisterCustomMemo;
  RegisterWSComponent(TCustomMemo, TWin32WSCustomMemoDark);

  WSStdCtrls.RegisterCustomListBox;
  RegisterWSComponent(TCustomListBox, TWin32WSCustomListBoxDark);

  WSForms.RegisterScrollingWinControl;

  WSForms.RegisterCustomForm;
  RegisterWSComponent(TCustomForm, TWin32WSCustomFormDark);

  WSMenus.RegisterMenu;
  WSMenus.RegisterPopupMenu;
  RegisterWSComponent(TPopupMenu, TWin32WSPopupMenuDark);

  WSForms.RegisterScrollBox;
  RegisterWSComponent(TScrollBox, TWin32WSScrollBoxDark);

  RegisterCustomTrackBar;
  RegisterWSComponent(TCustomTrackBar, TWin32WSTrackBarDark);

  DrawThemeText:= @DrawThemeTextDark;
  DrawThemeBackground:= @DrawThemeBackgroundDark;

  for Index:= Low(TThemedElement) to High(TThemedElement) do
  begin
    Theme[Index]:= TWin32ThemeServices(ThemeServices).Theme[Index];
  end;

  DefaultWindowInfo.DefWndProc:= @WindowProc;

  TaskDialogIndirect:= @TaskDialogIndirectDark;
end;

function FormWndProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  Info: PWin32WindowInfo;
begin
  if Msg = WM_CREATE then
  begin
    AllowDarkModeForWindow(Window, True);
    RefreshTitleBarThemeColor(Window);
  end
  else if (Msg = WM_SETFONT) then
  begin
    Info := GetWin32WindowInfo(Window);
    if Assigned(Info) then
    begin
     Info^.DefWndProc:= @WindowProc;
    end;
    Result:= CallWindowProc(@WindowProc, Window, Msg, WParam, LParam);
    Exit;
  end;
  Result:= DefWindowProc(Window, Msg, WParam, LParam);
end;

var
  TrampolineOpenThemeData: function(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall =  nil;
  TrampolineDrawThemeText: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR; iCharCount: Integer;
                                    dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall = nil;
  TrampolineDrawThemeBackground: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; pClipRect: Pointer): HRESULT; stdcall =  nil;

procedure DrawCheckBox(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
  pClipRect: PRECT);
var
  LCanvas: TCanvas;
  AStyle: TTextStyle;
begin
  LCanvas:= TCanvas.Create;
  try
    LCanvas.Handle:= HDC;

    LCanvas.Brush.Color:= clBtnFace;
    LCanvas.FillRect(pRect);

    AStyle:= LCanvas.TextStyle;
    AStyle.Layout:= tlCenter;
    AStyle.ShowPrefix:= True;

    // Fill checkbox rect
    LCanvas.Font.Name:= 'Segoe MDL2 Assets';
    LCanvas.Font.Color:= SysColor[COLOR_WINDOW];
    LCanvas.TextRect(pRect, 0, 0, MDL_CHECKBOX_FILLED, AStyle);

    // Draw checkbox border
    if iStateId in [CBS_UNCHECKEDHOT, CBS_MIXEDHOT, CBS_CHECKEDHOT] then
      LCanvas.Font.Color:= SysColor[COLOR_HIGHLIGHT]
    else begin
      LCanvas.Font.Color:= RGBToColor(192, 192, 192);
    end;
    LCanvas.TextRect(pRect, 0, 0, MDL_CHECKBOX_OUTLINE, AStyle);

    // Draw checkbox state
    if iStateId in [CBS_MIXEDNORMAL, CBS_MIXEDHOT,
                    CBS_MIXEDPRESSED, CBS_MIXEDDISABLED] then
    begin
      LCanvas.Font.Color:= RGBToColor(120, 120, 120);
      LCanvas.TextRect(pRect, 0, 0, MDL_CHECKBOX_GRAYED, AStyle);
    end
    else if iStateId in [CBS_CHECKEDNORMAL, CBS_CHECKEDHOT,
                         CBS_CHECKEDPRESSED, CBS_CHECKEDDISABLED] then
    begin
      LCanvas.Font.Color:= RGBToColor(192, 192, 192);
      LCanvas.TextRect(pRect, 0, 0, MDL_CHECKBOX_CHECKED, AStyle);
    end;
  finally
    LCanvas.Handle:= 0;
    LCanvas.Free;
  end;
end;

procedure DrawRadionButton(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
  pClipRect: PRECT);
var
  LCanvas: TCanvas;
  AStyle: TTextStyle;
begin
  LCanvas:= TCanvas.Create;
  try
    LCanvas.Handle:= hdc;

    LCanvas.Brush.Color:= SysColor[COLOR_BTNFACE];
    LCanvas.FillRect(pRect);

    AStyle:= LCanvas.TextStyle;
    AStyle.Layout:= tlCenter;
    AStyle.ShowPrefix:= True;

    // Draw radio circle
    LCanvas.Font.Name:= 'Segoe MDL2 Assets';
    LCanvas.Font.Color:= SysColor[COLOR_WINDOW];
    LCanvas.TextRect(pRect, 0, 0, MDL_RADIO_FILLED, AStyle);

    // Draw radio button state
    if iStateId in [RBS_CHECKEDNORMAL, RBS_CHECKEDHOT,
                    RBS_CHECKEDPRESSED, RBS_CHECKEDDISABLED] then
    begin
      LCanvas.Font.Color:= RGBToColor(192, 192, 192);
      LCanvas.TextRect(pRect, 0, 0, MDL_RADIO_CHECKED, AStyle );
    end;

    // Set outline circle color
    if iStateId in [RBS_UNCHECKEDPRESSED, RBS_CHECKEDPRESSED] then
      LCanvas.Font.Color:= RGBToColor(83, 160, 237)
    else if iStateId in [RBS_UNCHECKEDHOT, RBS_CHECKEDHOT] then
      LCanvas.Font.Color:= SysColor[COLOR_HIGHLIGHT]
    else begin
      LCanvas.Font.Color:= RGBToColor(192, 192, 192);
    end;
    // Draw outline circle
    LCanvas.TextRect(pRect, 0, 0, MDL_RADIO_OUTLINE, AStyle);
  finally
    LCanvas.Handle:= 0;
    LCanvas.Free;
  end;
end;

procedure DrawGroupBox(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
  pClipRect: PRECT);
var
  LCanvas: TCanvas;
begin
  LCanvas:= TCanvas.Create;
  try
    LCanvas.Handle:= HDC;

    // Draw border
    LCanvas.Brush.Style:= bsClear;
    LCanvas.Pen.Color:= SysColor[COLOR_BTNHIGHLIGHT];
    LCanvas.RoundRect(pRect, 10, 10);
  finally
    LCanvas.Handle:= 0;
    LCanvas.Free;
  end;
end;

procedure DrawButton(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
  pClipRect: PRECT);
begin
  case iPartId of
    BP_PUSHBUTTON: TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
    BP_RADIOBUTTON: DrawRadionButton(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
    BP_CHECKBOX: DrawCheckBox(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
    BP_GROUPBOX: DrawGroupBox(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
  end;
end;

procedure DrawTabControl(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
  pClipRect: PRECT);
var
  ARect: TRect;
  AColor: TColor;
  ALight: TColor;
  LCanvas: TCanvas;
begin
  LCanvas:= TCanvas.Create;
  try
    LCanvas.Handle:= hdc;

    AColor:= SysColor[COLOR_BTNFACE];
    ALight:= Lighter(AColor, 160);

    case iPartId of
      TABP_TOPTABITEM,
      TABP_TOPTABITEMLEFTEDGE,
      TABP_TOPTABITEMBOTHEDGE,
      TABP_TOPTABITEMRIGHTEDGE:
      begin
        ARect:= pRect;
        // Fill tab inside
        if (iStateId <> TIS_SELECTED) then
        begin
          if iStateId <> TIS_HOT then
            LCanvas.Brush.Color:= Lighter(AColor, 117)
          else begin
            LCanvas.Brush.Color:= Lighter(AColor, 200);
          end;
        end
        else begin
          Dec(ARect.Bottom);
          InflateRect(ARect, -1, -1);
          LCanvas.Brush.Color:= Lighter(AColor, 176);
        end;
        LCanvas.FillRect(ARect);
        LCanvas.Pen.Color:= ALight;

        if iPartId in [TABP_TOPTABITEMLEFTEDGE, TABP_TOPTABITEMBOTHEDGE] then
        begin
          // Draw left border
          LCanvas.Line(pRect.Left, pRect.Top, pRect.Left, pRect.Bottom);
        end;

        if (iStateId <> TIS_SELECTED) then
        begin
          // Draw right border
          LCanvas.Line(pRect.Right - 1, pRect.Top, pRect.Right - 1, pRect.Bottom)
        end
        else begin
          // Draw left border
          if (iPartId = TABP_TOPTABITEM) then
          begin
            LCanvas.Line(pRect.Left, pRect.Top, pRect.Left, pRect.Bottom - 1);
          end;
          // Draw right border
          LCanvas.Line(pRect.Right - 1, pRect.Top, pRect.Right - 1, pRect.Bottom - 1);
        end;
        // Draw top border
        LCanvas.Line(pRect.Left, pRect.Top, pRect.Right, pRect.Top);
      end;
      TABP_PANE:
      begin
        // Draw tab pane border
        LCanvas.Brush.Color:= AColor;
        LCanvas.Pen.Color:= ALight;
        LCanvas.Rectangle(pRect);
      end;
    end;
  finally
    LCanvas.Handle:= 0;
    LCanvas.Free;
  end;
end;

procedure DrawProgressBar(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
  pClipRect: PRECT);
begin
  if not (iPartId in [PP_TRANSPARENTBAR, PP_TRANSPARENTBARVERT]) then
    TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect)
  else begin
    SelectObject(hdc, GetStockObject(DC_PEN));
    SetDCPenColor(hdc, SysColor[COLOR_BTNSHADOW]);
    SelectObject(hdc, GetStockObject(DC_BRUSH));
    SetDCBrushColor(hdc, SysColor[COLOR_BTNFACE]);
    with pRect do Rectangle(hdc, Left, Top, Right, Bottom);
  end;
end;

function InterceptOpenThemeData(hwnd: hwnd; pszClassList: LPCWSTR): hTheme; stdcall;
var
  P: LONG_PTR;
begin
  if (hwnd <> 0) then
  begin
    P:= GetWindowLongPtr(hwnd, GWL_EXSTYLE);

    if (P and WS_EX_CONTEXTHELP = 0) or (lstrcmpiW(pszClassList, VSCLASS_MONTHCAL) = 0) then
    begin
      Result:= TrampolineOpenThemeData(hwnd, pszClassList);
      Exit;
    end;
  end;

  if lstrcmpiW(pszClassList, VSCLASS_TAB) = 0 then
  begin
    AllowDarkStyle(hwnd);
    pszClassList:= PWideChar(VSCLASS_DARK_TAB);
  end
  else if lstrcmpiW(pszClassList, VSCLASS_BUTTON) = 0 then
  begin
    AllowDarkStyle(hwnd);
    pszClassList:= PWideChar(VSCLASS_DARK_BUTTON);
  end
  else if lstrcmpiW(pszClassList, VSCLASS_EDIT) = 0 then
  begin
    AllowDarkStyle(hwnd);
    pszClassList:= PWideChar(VSCLASS_DARK_EDIT);
  end
  else if lstrcmpiW(pszClassList, VSCLASS_COMBOBOX) = 0 then
  begin
    AllowDarkStyle(hwnd);
    pszClassList:= PWideChar(VSCLASS_DARK_COMBOBOX);
  end
  else if lstrcmpiW(pszClassList, VSCLASS_SCROLLBAR) = 0 then
  begin
    AllowDarkStyle(hwnd);
    pszClassList:= PWideChar(VSCLASS_DARK_SCROLLBAR);
  end;

  Result:= TrampolineOpenThemeData(hwnd, pszClassList);
  ThemeClass.AddOrSetValue(Result, pszClassList);
end;

function InterceptDrawThemeText(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR; iCharCount: Integer;
  dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall;
var
  OldColor: COLORREF;
  ClassName: String;
begin
  if ThemeClass.TryGetValue(hTheme, ClassName) then
  begin
    if SameText(ClassName, VSCLASS_DARK_COMBOBOX) or SameText(ClassName, VSCLASS_DARK_EDIT) then
    begin
      Result:= TrampolineDrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect);
      Exit;
    end;

    if SameText(ClassName, VSCLASS_TOOLTIP) then
      OldColor:= SysColor[COLOR_INFOTEXT]
    else begin
      OldColor:= SysColor[COLOR_BTNTEXT];
    end;

    if SameText(ClassName, VSCLASS_DARK_BUTTON) then
    begin
      if (iPartId = BP_CHECKBOX) and (iStateId in [CBS_UNCHECKEDDISABLED, CBS_CHECKEDDISABLED, CBS_MIXEDDISABLED]) then
        OldColor:= SysColor[COLOR_GRAYTEXT]
      else if (iPartId = BP_RADIOBUTTON) and (iStateId in [RBS_UNCHECKEDDISABLED, RBS_CHECKEDDISABLED]) then
        OldColor:= SysColor[COLOR_GRAYTEXT]
      else if (iPartId = BP_GROUPBOX) and (iStateId = GBS_DISABLED) then
        OldColor:= SysColor[COLOR_GRAYTEXT]
      else if (iPartId = BP_PUSHBUTTON) then
      begin
        Result:= TrampolineDrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect);
        Exit;
      end;
    end;

    OldColor:= SetTextColor(hdc, OldColor);
    SetBkMode(hdc, TRANSPARENT);

    DrawTextExW(hdc, pszText, iCharCount, @pRect, dwTextFlags, nil);

    SetTextColor(hdc, OldColor);

    Exit(S_OK);
  end;
  Result:= TrampolineDrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect);
end;

function InterceptDrawThemeBackground(hTheme: hTheme; hdc: hdc; iPartId, iStateId: Integer; const pRect: TRect;
    pClipRect: Pointer): HRESULT; stdcall;
var
  Index: Integer;
  ClassName: String;
begin
  if ThemeClass.TryGetValue(hTheme, ClassName) then
  begin
    Index:= SaveDC(hdc);
    try
      if SameText(ClassName, VSCLASS_DARK_BUTTON) then
      begin
        DrawButton(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
      end
      else if SameText(ClassName, VSCLASS_DARK_TAB) then
      begin
        DrawTabControl(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
      end
      else if SameText(ClassName, VSCLASS_PROGRESS) or SameText(ClassName, VSCLASS_PROGRESS_INDER) then
      begin
        DrawProgressBar(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
      end
      else begin
        Result:= TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
      end;
    finally
      RestoreDC(hdc, Index);
    end;
    Exit(S_OK);
  end;
  Result:= TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect);
end;

function DrawThemeEdgeDark(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pDestRect: TRect; uEdge,
  uFlags: UINT; pContentRect: PRECT): HRESULT; stdcall;
var
  ARect: TRect;
begin
  ARect:= pDestRect;
  _DrawEdge(hdc, ARect, uEdge, uFlags);
  if (uFlags and DFCS_ADJUSTRECT <> 0) and (pContentRect <> nil) then
    pContentRect^ := ARect;
  Result:= S_OK;
end;

function GetThemeSysColorDark(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall;
begin
  Result:= GetSysColor(iColorId);
end;

function GetThemeSysColorBrushDark(hTheme: HTHEME; iColorId: Integer): HBRUSH; stdcall;
begin
  Result:= GetSysColorBrush(iColorId);
end;

var
  DeleteObjectOld: function(ho: HGDIOBJ): WINBOOL; stdcall;

function __DeleteObject(ho: HGDIOBJ): WINBOOL; stdcall;
var
  Index: Integer;
begin
  for Index:= 0 to High(SysColorBrush) do
  begin
    if SysColorBrush[Index] = ho then Exit(True);
  end;
  Result:= DeleteObjectOld(ho);
end;

procedure InitializeColors;
begin
  SysColor[COLOR_SCROLLBAR]               := RGBToColor(53, 53, 53);
  SysColor[COLOR_BACKGROUND]              := RGBToColor(53, 53, 53);
  SysColor[COLOR_ACTIVECAPTION]           := RGBToColor(42, 130, 218);
  SysColor[COLOR_INACTIVECAPTION]         := RGBToColor(53, 53, 53);
  SysColor[COLOR_MENU]                    := RGBToColor(42, 42, 42);
  SysColor[COLOR_WINDOW]                  := RGBToColor(42, 42, 42);
  SysColor[COLOR_WINDOWFRAME]             := RGBToColor(20, 20, 20);
  SysColor[COLOR_MENUTEXT]                := RGBToColor(255, 255, 255);
  SysColor[COLOR_WINDOWTEXT]              := RGBToColor(255, 255, 255);
  SysColor[COLOR_CAPTIONTEXT]             := RGBToColor(255, 255, 255);
  SysColor[COLOR_ACTIVEBORDER]            := RGBToColor(53, 53, 53);
  SysColor[COLOR_INACTIVEBORDER]          := RGBToColor(53, 53, 53);
  SysColor[COLOR_APPWORKSPACE]            := RGBToColor(53, 53, 53);
  SysColor[COLOR_HIGHLIGHT]               := RGBToColor(42, 130, 218);
  SysColor[COLOR_HIGHLIGHTTEXT]           := RGBToColor(255, 255, 255);
  SysColor[COLOR_BTNFACE]                 := RGBToColor(53, 53, 53);
  SysColor[COLOR_BTNSHADOW]               := RGBToColor(35, 35, 35);
  SysColor[COLOR_GRAYTEXT]                := RGBToColor(160, 160, 160);
  SysColor[COLOR_BTNTEXT]                 := RGBToColor(255, 255, 255);
  SysColor[COLOR_INACTIVECAPTIONTEXT]     := RGBToColor(255, 255, 255);
  SysColor[COLOR_BTNHIGHLIGHT]            := RGBToColor(66, 66, 66);
  SysColor[COLOR_3DDKSHADOW]              := RGBToColor(20, 20, 20);
  SysColor[COLOR_3DLIGHT]                 := RGBToColor(40, 40, 40);
  SysColor[COLOR_INFOTEXT]                := RGBToColor(53, 53, 53);
  SysColor[COLOR_INFOBK]                  := RGBToColor(255, 255, 255);
  SysColor[COLOR_HOTLIGHT]                := RGBToColor(66, 66, 66);
  SysColor[COLOR_GRADIENTACTIVECAPTION]   := GetSysColor(COLOR_GRADIENTACTIVECAPTION);
  SysColor[COLOR_GRADIENTINACTIVECAPTION] := GetSysColor(COLOR_GRADIENTINACTIVECAPTION);
  SysColor[COLOR_MENUHILIGHT]             := RGBToColor(42, 130, 218);
  SysColor[COLOR_MENUBAR]                 := RGBToColor(42, 42, 42);
  SysColor[COLOR_FORM]                    := RGBToColor(53, 53, 53);
end;

function WinRegister(ClassName: PWideChar): Boolean;
var
  WindowClassW: WndClassW;
begin
  ZeroMemory(@WindowClassW, SizeOf(WndClassW));
  with WindowClassW do
  begin
    Style := CS_DBLCLKS;
    LPFnWndProc := @FormWndProc;
    hInstance := System.HInstance;
    hIcon := Windows.LoadIcon(MainInstance, 'MAINICON');
    if hIcon = 0 then
     hIcon := Windows.LoadIcon(0, IDI_APPLICATION);
    hCursor := Windows.LoadCursor(0, IDC_ARROW);
    LPSzClassName := ClassName;
  end;
  Result := Windows.RegisterClassW(@WindowClassW) <> 0;
end;

procedure Initialize;
var
  hModule, hUxTheme: THandle;
  pLibrary, pFunction: PPointer;
  pImpDesc: PIMAGE_DELAYLOAD_DESCRIPTOR;
begin
  if not g_darkModeEnabled then
    Exit;

  InitializeColors;

  WinRegister(ClassNameW);
  WinRegister(ClassNameTC);

  ThemeClass:= TThemeClassMap.Create;

  hModule:= GetModuleHandle(gdi32);
  Pointer(DeleteObjectOld):= GetProcAddress(hModule, 'DeleteObject');

  hModule:= GetModuleHandle(comctl32);
  Pointer(DefSubclassProc):= GetProcAddress(hModule, 'DefSubclassProc');
  Pointer(SetWindowSubclass):= GetProcAddress(hModule, 'SetWindowSubclass');

  // Override several system functions
  pLibrary:= FindImportLibrary(MainInstance, user32);
  if Assigned(pLibrary) then
  begin
    hModule:= GetModuleHandle(user32);

    pFunction:= FindImportFunction(pLibrary, GetProcAddress(hModule, 'CreateWindowExW'));
    if Assigned(pFunction) then
    begin
      Pointer(__CreateWindowExW):= ReplaceImportFunction(pFunction, @_CreateWindowExW);
    end;
    pFunction:= FindImportFunction(pLibrary, GetProcAddress(hModule, 'DrawEdge'));
    if Assigned(pFunction) then
    begin
      ReplaceImportFunction(pFunction, @_DrawEdge);
    end;
    pFunction:= FindImportFunction(pLibrary, GetProcAddress(hModule, 'GetSysColor'));
    if Assigned(pFunction) then
    begin
      ReplaceImportFunction(pFunction, @GetSysColorDark);
    end;
    pFunction:= FindImportFunction(pLibrary, GetProcAddress(hModule, 'GetSysColorBrush'));
    if Assigned(pFunction) then
    begin
      ReplaceImportFunction(pFunction, @GetSysColorBrushDark);
    end;
  end;
  pLibrary:= FindImportLibrary(MainInstance, gdi32);
  if Assigned(pLibrary) then
  begin
    hModule:= GetModuleHandle(gdi32);
    pFunction:= FindImportFunction(pLibrary, Pointer(DeleteObjectOld));
    if Assigned(pFunction) then
    begin
      ReplaceImportFunction(pFunction, @__DeleteObject);
    end;
  end;

  hModule:= GetModuleHandle(comctl32);
  pImpDesc:= FindDelayImportLibrary(hModule, themelib);
  if Assigned(pImpDesc) then
  begin
    hUxTheme:= GetModuleHandle(themelib);
    Pointer(TrampolineOpenThemeData):= GetProcAddress(hUxTheme, 'OpenThemeData');
    Pointer(TrampolineDrawThemeText):= GetProcAddress(hUxTheme, 'DrawThemeText');
    Pointer(TrampolineDrawThemeBackground):= GetProcAddress(hUxTheme, 'DrawThemeBackground');

    ReplaceDelayImportFunction(hModule, pImpDesc, 'OpenThemeData', @InterceptOpenThemeData);
    ReplaceDelayImportFunction(hModule, pImpDesc, 'DrawThemeText', @InterceptDrawThemeText);
    ReplaceDelayImportFunction(hModule, pImpDesc, 'DrawThemeBackground', @InterceptDrawThemeBackground);

    ReplaceDelayImportFunction(hModule, pImpDesc, 'DrawThemeEdge', @DrawThemeEdgeDark);
  end;

  pLibrary:= FindImportLibrary(hModule, gdi32);
  if Assigned(pLibrary) then
  begin
    pFunction:= FindImportFunction(pLibrary, Pointer(DeleteObjectOld));
    if Assigned(pFunction) then
    begin
      ReplaceImportFunction(pFunction, @__DeleteObject);
    end;
  end;

  hModule:= GetModuleHandle(comctl32);
  pLibrary:= FindImportLibrary(hModule, user32);
  if Assigned(pLibrary) then
  begin
    hModule:= GetModuleHandle(user32);

    pFunction:= FindImportFunction(pLibrary, GetProcAddress(hModule, 'DrawEdge'));
    if Assigned(pFunction) then
    begin
      ReplaceImportFunction(pFunction, @_DrawEdge);
    end;
  end;
end;

initialization
  Initialize;

end.

