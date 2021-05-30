{
   Double commander
   -------------------------------------------------------------------------
   Explorer preview plugin

   Copyright (C) 2021 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

library Preview;

{$mode objfpc}{$H+}

uses
  SysUtils, JwaWinUser, Windows, Messages, WlxPlugin, uPreviewHandler;

const
  WH_KEYBOARD_LL = 13;
  PREVIEW_HANDLER = UnicodeString('IPreviewHandler');

type
  TPreviewData = class
    Handler: IPreviewHandler;
  end;

var
  hhk: HHOOK;
  Count: Integer = 0;
  ProcessIdWide: PWideChar;
  ProcessIdAtom: ATOM absolute ProcessIdWide;

procedure DLL_Entry_Hook(lpReserved : PtrInt);
begin
  GlobalDeleteAtom(ProcessIdAtom);
end;

function LowLevelKeyboardProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  ParentWin: HWND;
  ProcessId: HANDLE;
  Msg: PKbDllHookStruct absolute lParam;

  function IsKeyUp(Key: Integer): Boolean; inline;
  begin
    Result := (GetKeyState(Key) and $8000) = 0;
  end;

begin
  if (nCode = HC_ACTION) then
  begin
    if (wParam = WM_KEYDOWN) and (Msg^.vkCode = VK_ESCAPE) then
    begin
      ParentWin:= Windows.GetAncestor(GetForegroundWindow, GA_ROOT);
      ProcessId:= Windows.GetPropW(ParentWin, ProcessIdWide);
      if (ProcessId <> 0) and (ProcessId = GetProcessID) then
      begin
        if IsKeyUp(VK_MENU) and IsKeyUp(VK_CONTROL) and IsKeyUp(VK_MENU) then
        begin
          PostMessage(ParentWin, WM_SYSCOMMAND, SC_CLOSE, 0);
        end;
      end;
    end;
  end;
  Result:= CallNextHookEx(hhk, nCode, wParam, lParam);
end;

function WindowProc(hWnd: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  ARect: TRect;
  AData: TPreviewData;
  AHandle: THandle absolute AData;
begin
  if (uiMsg = WM_SETFOCUS) then
  begin
    AHandle:= GetWindowLongPtr(hWnd, GWLP_USERDATA);
    if (AHandle <> 0) then AData.Handler.SetFocus();
  end
  else if (uiMsg = WM_SIZE) then
  begin
    AHandle:= GetWindowLongPtr(hWnd, GWLP_USERDATA);
    if (AHandle <> 0) then
    begin
      ARect.Top:= 0;
      ARect.Left:= 0;
      ARect.Width:= LOWORD(lParam);
      ARect.Height:= HIWORD(lParam);
      AData.Handler.SetRect(@ARect);
    end;
  end;
  Result := DefWindowProc(hWnd, uiMsg, wParam, lParam);
end;

function ListLoadW(ParentWin: HWND; FileToLoad: PWideChar; ShowFlags: Integer): HWND; stdcall;
var
  ARect: TRect;
  AData: TPreviewData;
  WindowClassW: TWndClassW;
  AHandler: IPreviewHandler;
  AHandle: THandle absolute AData;
begin
  AHandler:= GetPreviewHandler(FileToLoad);
  if (AHandler = nil) then Exit(wlxInvalidHandle);

  ZeroMemory(@WindowClassW, SizeOf(WndClassW));
  with WindowClassW do
  begin
    Style := CS_DBLCLKS;
    lpfnWndProc := @WindowProc;
    cbWndExtra := SizeOf(Pointer);
    hCursor := LoadCursor(0, IDC_ARROW);
    lpszClassName := PREVIEW_HANDLER;
    hInstance := GetModuleHandleW(nil);
  end;
  Windows.RegisterClassW(WindowClassW);

  Result:= CreateWindowW(PREVIEW_HANDLER, 'PreviewHandler', WS_CHILD or WS_VISIBLE, 0, 0, 640,
                         480, ParentWin, 0, WindowClassW.hInstance, nil);

  if (Result <> wlxInvalidHandle) then
  begin
    AData:= TPreviewData.Create;
    AData.Handler:= AHandler;
    SetPropW(ParentWin, ProcessIdWide, GetProcessID);
    SetWindowLongPtr(Result, GWLP_USERDATA, AHandle);
    ARect:= TRect.Create(0, 0, 640, 480);
    AHandler.SetWindow(Result, ARect);
    AData.Handler.DoPreview();
    Inc(Count);
    if (Count = 1) then begin
      hhk:= SetWindowsHookExW(WH_KEYBOARD_LL, @LowLevelKeyboardProc, WindowClassW.hInstance, 0);
    end;
  end;
end;

procedure ListCloseWindow(ListWin: HWND); stdcall;
var
  AData: TPreviewData;
  AHandle: THandle absolute AData;
begin
  AHandle:= GetWindowLongPtr(ListWin, GWLP_USERDATA);
  DestroyWindow(ListWin);
  if Assigned(AData) then
  begin
    AData.Handler.Unload();
    AData.Free;
    Dec(Count);
    if Count = 0 then begin
      UnhookWindowsHookEx(hhk);
    end;
  end;
end;

procedure ListGetDetectString(DetectString: PAnsiChar; MaxLen: Integer); stdcall;
begin
  StrLCopy(DetectString, '(EXT="HTM")|(EXT="HTML")|(EXT="MHT")|(EXT="MHTML")', MaxLen);
end;

procedure ListSetDefaultParams(dps: PListDefaultParamStruct); stdcall;
begin
  Dll_Process_Detach_Hook:= @DLL_Entry_Hook;
  ProcessIdAtom := GlobalAddAtomW(PREVIEW_HANDLER);
end;

exports
  ListLoadW,
  ListCloseWindow,
  ListGetDetectString,
  ListSetDefaultParams;

end.

