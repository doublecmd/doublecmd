
{ This file is part of Delphi Compatibility Unit

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

function BeginDeferWindowPos(nNumWindows: longint): THandle;
begin
  Result:=Windows.BeginDeferWindowPos(nNumWindows);
end;

function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc,
  YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop);
end;

function CopyImage(hImage: THANDLE; uType: LongWord; cxDesired,
  cyDesired: LongInt; fuFlags: LongWord): THandle;
begin
  Result := Windows.CopyImage(hImage,uType,cxDesired,cyDesired,fuFlags);
end;

function CreatePatternBrush(hbmp: HBITMAP): HBRUSH;
begin
  Result := Windows.CreatePatternBrush(hbmp);
end;

function DeferWindowPos(hWinPosInfo, hWnd, hWndInsertAfter: THandle;
  x, y, cx, cy: longint; uFlags: LongWord): THandle;
begin
  Result := Windows.DeferWindowPos(hWinPosInfo,hWnd,hWndInsertAfter,x,y,cx,cy,uFlags);
end;

function DrawFrameControl(DC: HDC; const Rect: TRect; uType, uState: LongWord): Boolean;
begin
  Result := Windows.DrawFrameControl(DC,Rect,uType,uState);
end;

function EndDeferWindowPos(hWinPosInfo: THandle): Boolean;
begin
  Result:=Windows.EndDeferWindowPos(hWinPosInfo);
end;

function GdiFlush: Boolean;
begin
  Result := Windows.GdiFlush;
end;

function GetACP: LongWord;
begin
  Result := Windows.GetACP;
end;

function GetBkColor(DC: HDC): LCLType.COLORREF;
begin
  Result := Windows.GetBkColor(DC);
end;

function GetDCEx(hWnd: HWND; hrgnClip: HRGN; flags: DWORD): HDC;
begin
  Result := Windows.GetDCEx(hWnd,hrgnClip,flags);
end;

function GetKeyboardLayout(dwLayout: DWORD): THandle;
begin
  Result := Windows.GetKeyboardLayout(dwLayout);
end;

function GetKeyboardState(lpKeyState: PBYTE): BOOLEAN;
begin
  Result := Windows.GetKeyboardState(lpKeyState);
end;

function GetLocaleInfo(Locale, LCType: LongWord; lpLCData: PChar;
  cchData: longint): longint;
begin
  Result := Windows.GetLocaleInfo(Locale,LCType,lpLCData,cchData);
end;

function GetRandomRgn(DC: HDC; Rgn: HRGN; iNum: Integer): Integer; stdcall; external 'GDI32.DLL';

function GetTextAlign(hDC: HDC): LongWord;
begin
  Result := Windows.GetTextAlign(hDC);
end;

function GetTextExtentExPoint(DC: LCLType.HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: ObjPas.PInteger;
  var Size: TSize): BOOL;
begin
  Result := Windows.GetTextExtentExPoint(DC, Str, Count, MaxWidth, MaxCount, PartialWidths,  Size);
end;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; out Size: TSize): Boolean;
begin
  Result := Windows.GetTextExtentPointW(DC, Str, Count, Size);
end;

function GetWindowDC(hWnd: HWND): HDC;
begin
  Result := Windows.GetWindowDC(hWnd);
end;

function ImageList_DragShowNolock(fShow: Boolean): Boolean;
begin
  Result := CommCtrl.ImageList_DragShowNolock(fShow);
end;

function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints; cPoints: UINT
  ): Integer;
begin
  Result:=Windows.MapWindowPoints(hWndFrom,hWndTo,lpPoints,cPoints);
end;

function MultiByteToWideChar(CodePage, dwFlags: DWORD; lpMultiByteStr: PChar;
  cchMultiByte: longint; lpWideCharStr: PWideChar; cchWideChar: longint
  ): longint;
begin
  Result := Windows.MultiByteToWideChar(CodePage,dwFlags,lpMultiByteStr,cchMultiByte,lpWideCharStr,cchWideChar);
end;

function OffsetRgn(hrgn: HRGN; nxOffset, nYOffset: longint): longint;
begin
  Result := Windows.OffsetRgn(hrgn,nxOffset,nYOffset);
end;

function SetBrushOrgEx(DC: LCLType.HDC; nXOrg, nYOrg: longint; lppt: Types.PPoint): Boolean;
begin
  Result := Windows.SetBrushOrgEx(DC,nXOrg,nYOrg,lppt);
end;

function ScrollDC(DC: LCLType.HDC; dx: longint; dy: longint; var lprcScroll: Types.TRect;
  var lprcClip: Types.TRect; hrgnUpdate: LCLType.HRGN; lprcUpdate: Types.PRect): Boolean;
begin
  Result := Windows.ScrollDC(DC, dx, dy, lprcScroll, lprcClip, hrgnUpdate, lprcUpdate);
end;

function ToAscii(uVirtKey, uScanCode: LongWord; lpKeyState: PBYTE;
  lpChar: PWORD; uFlags: LongWord): longint;
begin
  Result := Windows.ToAscii(uVirtKey,uScanCode,lpKeyState,lpChar,uFlags);
end;
