{
   Double commander
   -------------------------------------------------------------------------
   Setup unique window class name for main form

   Copyright (C) 2016-2019 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
}

unit uDClass;

{$mode objfpc}{$H+}

interface

implementation

uses
  Classes, SysUtils, Win32Int, WSLCLClasses, Forms, Windows, Win32Proc,
  Controls, LCLType, fMain, Win32WSControls, uImport;

const
  ClassNameW: PWideChar = 'TTOTAL_CMD'; // for compatibility with plugins

function WinRegister: Boolean;
var
  WindowClassW: WndClassW;
begin
  ZeroMemory(@WindowClassW, SizeOf(WndClassW));
  with WindowClassW do
  begin
    Style := CS_DBLCLKS;
    LPFnWndProc := @WindowProc;
    hInstance := System.HInstance;
    hIcon := Windows.LoadIcon(MainInstance, 'MAINICON');
    if hIcon = 0 then
     hIcon := Windows.LoadIcon(0, IDI_APPLICATION);
    hCursor := Windows.LoadCursor(0, IDC_ARROW);
    LPSzClassName := ClassNameW;
  end;
  Result := Windows.RegisterClassW(@WindowClassW) <> 0;
end;

var
  __GetProp: function(hWnd: HWND; lpString: LPCSTR): HANDLE; stdcall;
  __SetProp: function(hWnd: HWND; lpString: LPCSTR; hData: HANDLE): WINBOOL; stdcall;
  __CreateWindowExW: function(dwExStyle: DWORD; lpClassName: LPCWSTR; lpWindowName: LPCWSTR; dwStyle: DWORD; X: longint; Y: longint; nWidth: longint; nHeight: longint; hWndParent: HWND; hMenu: HMENU; hInstance: HINST; lpParam: LPVOID): HWND; stdcall;

function _GetProp(hWnd: HWND; lpString: LPCSTR): HANDLE; stdcall;
var
  Atom: UIntPtr absolute lpString;
begin
  if (Atom > MAXWORD) and (lpString = 'WinControl') then
    Result:= __GetProp(hWnd, 'WinControlDC')
  else
    Result:= __GetProp(hWnd, lpString);
end;

function _SetProp(hWnd: HWND; lpString: LPCSTR; hData: HANDLE): WINBOOL; stdcall;
var
  Atom: UIntPtr absolute lpString;
begin
  if (Atom > MAXWORD) and (lpString = 'WinControl') then
    Result:= __SetProp(hWnd, 'WinControlDC', hData)
  else
    Result:= __SetProp(hWnd, lpString, hData);
end;

function _CreateWindowExW(dwExStyle: DWORD; lpClassName: LPCWSTR; lpWindowName: LPCWSTR; dwStyle: DWORD; X: longint; Y: longint; nWidth: longint; nHeight: longint; hWndParent: HWND; hMenu: HMENU; hInstance: HINST; lpParam: LPVOID): HWND; stdcall;
var
  AParams: PNCCreateParams absolute lpParam;
begin
  if (hWndParent = 0) and Assigned(AParams) and (AParams^.WinControl is TfrmMain) then lpClassName:= ClassNameW;
  Result:= __CreateWindowExW(dwExStyle, lpClassName, lpWindowName, dwStyle, X, Y, nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam);
end;

procedure Initialize;
var
  hModule: THandle;
  pLibrary, pFunction: PPointer;
begin
  WinRegister;

  pLibrary:= FindImportLibrary(MainInstance, user32);
  if Assigned(pLibrary) then
  begin
    hModule:= GetModuleHandle(user32);

    pFunction:= FindImportFunction(pLibrary, GetProcAddress(hModule, 'CreateWindowExW'));
    if Assigned(pFunction) then
    begin
      Pointer(__CreateWindowExW):= ReplaceImportFunction(pFunction, @_CreateWindowExW);
    end;

    // Prevent plugins written in Lazarus from crashing by changing the name for
    // GetProp/SetProp to store control data from 'WinControl' to 'WinControlDC'

    pFunction:= FindImportFunction(pLibrary, GetProcAddress(hModule, 'GetPropA'));
    if Assigned(pFunction) then
    begin
      Pointer(__GetProp):= ReplaceImportFunction(pFunction, @_GetProp);
    end;

    pFunction:= FindImportFunction(pLibrary, GetProcAddress(hModule, 'SetPropA'));
    if Assigned(pFunction) then
    begin
      Pointer(__SetProp):= ReplaceImportFunction(pFunction, @_SetProp);
    end;
  end;
  Windows.GlobalDeleteAtom(WindowInfoAtom);
  WindowInfoAtom := Windows.GlobalAddAtom('WindowInfoDC');
end;

initialization
  Initialize;

finalization
  Windows.UnregisterClassW(PWideChar(ClassNameW), System.HInstance);

end.

