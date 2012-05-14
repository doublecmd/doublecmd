unit DelphiCompat;

{ Delphi Compatibility Unit

  Copyright (C) 2007 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

}

{$mode objfpc}{$H+}
{.$define DEBUG_DELPHICOMPAT}

interface

uses
  LMessages, Types, LCLType, Classes;

const
  //Messages
  WM_GETDLGCODE = LM_GETDLGCODE;
  WM_ERASEBKGND = LM_ERASEBKGND;
  WM_VSCROLL = LM_VSCROLL;
  WM_HSCROLL = LM_HSCROLL;
  WM_CHAR = LM_CHAR;
  WM_KEYDOWN = LM_KEYDOWN;
  WM_KEYUP = LM_KEYUP;
  WM_KILLFOCUS = LM_KILLFOCUS;
  WM_SIZE = LM_SIZE;
  WM_LBUTTONDBLCLK = LM_LBUTTONDBLCLK;
  WM_LBUTTONDOWN = LM_LBUTTONDOWN;

type
  //TWM* types
  TMessage = TLMessage;
  TWMHScroll = TLMHScroll;
  TWMVScroll = TLMVScroll;
  TWMChar = TLMChar;
  TWMKeyDown = TLMKeyDown;
  TWMKeyUp = TLMKeyUp;
  TWMKillFocus = TLMKillFocus;
  TWMSize = TLMSize;
  TWMLButtonDblClk = TLMLButtonDblClk;
  TWMMeasureItem = TLMMeasureItem;
  TWMDrawItem = TLMDrawItems;
  
function BeginDeferWindowPos(nNumWindows: LongInt):THandle;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
function CopyImage(hImage: THandle; uType:LongWord; cxDesired, cyDesired: LongInt; fuFlags:LongWord):THandle;

function DeferWindowPos(hWinPosInfo, hWnd, hWndInsertAfter:THandle; x, y, cx, cy:longint; uFlags:LongWord):THandle;

function EndDeferWindowPos(hWinPosInfo:THandle):Boolean;

function GdiFlush: Boolean;
function GetACP:LongWord;
function GetBkColor(DC:HDC):COLORREF;
function GetDCEx(hWnd:HWND; hrgnClip:HRGN; flags:DWORD):HDC;
function GetKeyboardLayout(dwLayout:DWORD):THandle;
function GetKeyboardState(lpKeyState:PBYTE):BOOLEAN;
function GetLocaleInfo(Locale, LCType:LongWord; lpLCData:PChar; cchData:longint):longint;
function GetRandomRgn(DC: HDC; Rgn: HRGN; iNum: Integer): Integer; stdcall;
function GetTextAlign(hDC:HDC): LongWord;
function GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; out Size: TSize): Boolean;
function GetWindowDC(hWnd:HWND):HDC;

function ImageList_DragShowNolock(fShow: Boolean): Boolean;

function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints; cPoints: UINT): Integer;
function MultiByteToWideChar(CodePage, dwFlags:DWORD; lpMultiByteStr:PChar; cchMultiByte:longint; lpWideCharStr:PWideChar;cchWideChar:longint):longint;

function OffsetRgn(hrgn:HRGN; nxOffset, nYOffset:longint):longint;

function ScrollDC(DC:HDC; dx:longint; dy:longint; var lprcScroll:TRECT; var lprcClip:TRECT;hrgnUpdate:HRGN; lprcUpdate:PRECT):Boolean;
function SetBrushOrgEx(DC:HDC; nXOrg, nYOrg:longint; lppt:PPOINT):Boolean;

function ToAscii(uVirtKey, uScanCode:LongWord; lpKeyState: PByte; lpChar: PWord; uFlags:LongWord): LongInt;

implementation


uses
{$i uses.inc}
  LCLProc, Controls
  {$ifdef DEBUG_DELPHICOMPAT}
  ,multiloglcl, filechannel
  {$endif}
  ;

{$ifdef DEBUG_DELPHICOMPAT}
const
  //Logger  classes
  lcInfo = 0;
  lcStack = 1;

var
  Logger: TLCLLogger;
{$endif}


{$i delphicompat.inc}

end.
