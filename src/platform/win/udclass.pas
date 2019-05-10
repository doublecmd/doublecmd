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
  Controls, WSForms, Win32WSForms, LCLType, fMain;

const
  ClassNameW: UnicodeString = 'DClass'#0;

type

  { TWin32WSCustomFormEx }

  TWin32WSCustomFormEx = class(TWin32WSCustomForm)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

{ TWin32WSCustomFormEx }

class function TWin32WSCustomFormEx.CreateHandle(const AWinControl: TWinControl;
                                                 const AParams: TCreateParams): HWND;
var
  AClass: String;
  AMainForm: Boolean;
begin
  AMainForm := AWinControl is TfrmMain;
  if AMainForm then
  begin
    AClass := ClsName;
    ClsName := String(ClassNameW);
  end;
  Result := inherited CreateHandle(AWinControl, AParams);
  if AMainForm then ClsName := AClass;
end;

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
    LPSzClassName := PWideChar(ClassNameW);
  end;
  Result := Windows.RegisterClassW(@WindowClassW) <> 0;
end;

procedure Initialize;
begin
  WinRegister;
  // Replace TCustomForm widgetset class
  RegisterCustomForm;
  RegisterWSComponent(TCustomForm, TWin32WSCustomFormEx);
end;

initialization
  Initialize;

finalization
  Windows.UnregisterClassW(PWideChar(ClassNameW), System.HInstance);

end.

