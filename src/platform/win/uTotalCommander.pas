{
    Double Commander
    -------------------------------------------------------------------------
    Creates Total Commander fake window (some plugins don't work without it)

    Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uTotalCommander;

{$MODE DELPHI}

{.$DEFINE DEBUG}

interface

uses
  Windows;

procedure CreateTotalCommanderWindow(hWindow: HWND);  

implementation

var
  wcFakeWndClass: TWndClassEx;
  hMainWindow,
  hFakeWindow: HWND;

function WindowProc(hWnd: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  // resend message to real main window
  SendMessage(hMainWindow, uiMsg, wParam, lParam);
  {$IFDEF DEBUG}
  WriteLn(uiMsg);
  {$ENDIF}
  Result:= DefWindowProc(hWnd, uiMsg, wParam, lParam);
end;

procedure CreateTotalCommanderWindow(hWindow: HWND);
begin
  hMainWindow:= hWindow;
  FillByte(wcFakeWndClass, SizeOf(wcFakeWndClass), 0);
  wcFakeWndClass.cbSize:= SizeOf (wcFakeWndClass);
  wcFakeWndClass.Style:= CS_HREDRAW or CS_VREDRAW;
  wcFakeWndClass.lpfnWndProc:= @WindowProc;
  wcFakeWndClass.hInstance:= hInstance;
  wcFakeWndClass.hbrBackground:= Color_BtnFace + 12;
  wcFakeWndClass.lpszMenuName:= nil;
  wcFakeWndClass.lpszClassName:= 'TTOTAL_CMD';
  RegisterClassEx(wcFakeWndClass);
  // Create Total Commander fake window
  hFakeWindow:= CreateWindowEx(0, 'TTOTAL_CMD', 'Double Commander', WS_OVERLAPPEDWINDOW, 100, 100, 300, 300, 0, 0, hInstance, nil);
  {$IFDEF DEBUG}
  // Show window (for debugging only)
  ShowWindow(hFakeWindow, SW_SHOW);
  {$ENDIF}
end;
  
end.