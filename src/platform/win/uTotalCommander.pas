{
    Double Commander
    -------------------------------------------------------------------------
    Creates Total Commander fake window (some plugins don't work without it)

    Copyright (C) 2009-2014 Alexander Koblov (alexx2000@mail.ru)

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
function ConvertTCStringToString(TCString: ansistring): string;
function ConvertStringToTCString(sString: string): ansistring;

implementation

uses
  LCLVersion, Forms, JwaDbt, SysUtils, LCLProc, uDebug;

var
  wcFakeWndClass: TWndClassEx;
  //hMainWindow,
  {$IFDEF DEBUG}
  hFakeWindow: HWND;
  {$ENDIF}

function WindowProc(hWnd: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  {
    Resend message to DoubleCommander main window.

    Disabled currently, because it may interfere with LCL, especially since the fake
    TotalCmd window is also a main app window (WS_OVERLAPPEDWINDOW). May be enabled
    in future if any plugins need this, but following messages should be skipped
    because they are known to cause conflict:
    - WM_ACTIVATEAPP
      Confuses LCL about which main form (window) is currently active and
      it stops calling OnExit events for controls (see TWinControl.WMKillFocus).
  }
  //SendMessage(hMainWindow, uiMsg, wParam, lParam);

  {$IF (lcl_fullversion >= 1020000)}
  if (uiMsg = WM_DEVICECHANGE) and (wParam = DBT_DEVNODES_CHANGED) and (lParam = 0) then
  begin
    Screen.UpdateMonitors; // Refresh monitor list
    DCDebug('WM_DEVICECHANGE:DBT_DEVNODES_CHANGED');
  end;
  {$ENDIF}

  {$IFDEF DEBUG}
  WriteLn(uiMsg);
  {$ENDIF}
  Result:= DefWindowProc(hWnd, uiMsg, wParam, lParam);
end;

procedure CreateTotalCommanderWindow(hWindow: HWND);
begin
//  hMainWindow:= hWindow;
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
  {$IFDEF DEBUG}
  hFakeWindow:=
  {$ENDIF}
  CreateWindowEx(0, 'TTOTAL_CMD', 'Double Commander', WS_OVERLAPPEDWINDOW, 100, 100, 300, 300, 0, 0, hInstance, nil);
  {$IFDEF DEBUG}
  // Show window (for debugging only)
  ShowWindow(hFakeWindow, SW_SHOW);
  {$ENDIF}
end;


//Test have been made with string from site http://stackoverflow.com/questions/478201/how-to-test-an-application-for-correct-encoding-e-g-utf-8
//Note: If you ever "think" to change or modify this routine, make sure to test the following:
//1o) Make a directory with utf-8 special characters, a path like this: "Card-♠♣♥♦"
//2o) Then, go with TC and add it as a favorite.
//3o) Then, exit it to make sure it is saved in its wndcmd.ini file
//4o) Then, go in the hotlist of DC and do an import from TC file
//5o) Make sure the path you've created has really been imported and it's NOT written "cd Card-♠♣♥♦\" or things like that.
//6o) Make sure you can also GO TO this folder from the popup menu of hotlist.
//7o) After that, repeat the step one through six with a path called "français", or "Esta frase está en español" and really take the time to do it.
//8o) Really take the time to do step 7 with the suggested two folder mentionned here.
//
//In its "wincmd", TC is using AnsiString for character that don't need UTF-8 string.
//In other words, there ARE NOT all in UTF8 format.
//He add the identifier "AnsiChar($EF) + AnsiChar($BB) + AnsiChar($BF)" at the beginning of each value that requires that the following needs to be interpret as UTF8 string.
//So we cannot systematically convert the string. Some are using code between 128 and 255 that needs to be interpert as what it was in ANSI.
//ALSO, lettings the $EF $BB $BF in the "string" make the string to be displayble "normally" in Lazarus, yes...
//...but when it's time to do things like "pos(...", "copy(...", the $EF $BB $BF are there, taken into acocunt, even when doing a print of the string we don't see see them!
//
//Anyway. If you ever modify the following thinking it shouldn't be like this or there is a better way or whatever, please, take the time to do the test written after your modifications
//
function ConvertTCStringToString(TCString: ansistring): string;
begin
  Result := TCString;
  if length(Result) >= 3 then
  begin
    if ((TCString[1] = AnsiChar($EF)) and (TCString[2] = AnsiChar($BB)) and (TCString[3] = AnsiChar($BF))) then
    begin
      Result := copy(Result, 4, (length(Result) - 3));
    end
    else
    begin
      Result := AnsiToUtf8(Result);
    end;
  end;
end;

//TC is not systematically adding the $EF $BB $BF to each string, but let's play safe and always it since he take them in account when using it anyway.
function ConvertStringToTCString(sString: string): ansistring;
begin
  Result := AnsiChar($EF) + AnsiChar($BB) + AnsiChar($BF) + sString;
end;

end.

