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
  SysUtils,
  Windows,
  Messages,
  WlxPlugin,
  uPreviewHandler;

type
  TPreviewData = class
    Handler: IPreviewHandler;
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
    lpszClassName := 'IPreviewHandler';
    hInstance := GetModuleHandleW(nil);
  end;
  Windows.RegisterClassW(WindowClassW);

  Result:= CreateWindowW('IPreviewHandler', 'PreviewHandler', WS_CHILD or WS_VISIBLE, 0, 0, 640,
                         480, ParentWin, 0, WindowClassW.hInstance, nil);

  if (Result <> wlxInvalidHandle) then
  begin
    AData:= TPreviewData.Create;
    AData.Handler:= AHandler;
    SetWindowLongPtr(Result, GWLP_USERDATA, AHandle);
    ARect:= TRect.Create(0, 0, 640, 480);
    AHandler.SetWindow(Result, ARect);
    AData.Handler.DoPreview();
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
  end;
end;

procedure ListGetDetectString(DetectString: PAnsiChar; MaxLen: Integer); stdcall;
begin
  StrLCopy(DetectString, '(EXT="HTM")|(EXT="HTML")|(EXT="MHT")|(EXT="MHTML")', MaxLen);
end;

exports
  ListLoadW,
  ListCloseWindow,
  ListGetDetectString;

end.

