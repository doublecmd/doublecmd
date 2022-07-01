{
   Double commander
   -------------------------------------------------------------------------
   Rich Text Format plugin

   Copyright (C) 2022 Alexander Koblov (alexx2000@mail.ru)

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

library richview;

{$mode objfpc}{$H+}

uses
  RichEdit, Windows, Messages, WlxPlugin;

type
{$push}{$packrecords 4}
  TEditStream = record
    dwCookie : DWORD_PTR;
    dwError : DWORD;
    pfnCallback : EDITSTREAMCALLBACK;
  end;
{$pop}

var
  RichEditClass: PWideChar;

function LoadLibraryX(const AName: PWideChar): HMODULE;
begin
  Result:= LoadLibraryExW(AName, 0, LOAD_LIBRARY_SEARCH_SYSTEM32);
end;

function RichEditWndProc(hWnd: HWND; uiMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  AWndProc: Windows.WNDPROC;
  AHandle: LONG_PTR absolute AWndProc;
begin
  if (uiMsg = WM_KEYDOWN) then
  begin
    if (wParam = VK_ESCAPE) then
    begin
      PostMessage(GetParent(hWnd), WM_SYSCOMMAND, SC_CLOSE, 0);
    end;
  end;
  AHandle := GetWindowLongPtr(hWnd, GWLP_USERDATA);
  if Assigned(AWndProc) then
    Result := CallWindowProc(AWndProc, hWnd, uiMsg, wParam, lParam)
  else begin
    Result := DefWindowProc(hWnd, uiMsg, wParam, lParam);
  end;
end;

function EditStreamCallback(dwCookie: DWORD_PTR; lpBuff: LPBYTE; cb: LONG; pcb: PLONG): DWORD; stdcall;
var
  AFile: THandle absolute dwCookie;
begin
  if (ReadFile(AFile, lpBuff^, cb, PDWORD(pcb)^, nil)) then
    Exit(0);

  Result:= DWORD(-1);
end;

function ListLoadW(ParentWin: HWND; FileToLoad: PWideChar; ShowFlags: Integer): HWND; stdcall;
var
  ARect: TRect;
  AFile: THandle;
  AResult: Boolean;
  hInstance: HMODULE;
  AWndProc: LONG_PTR;
  AStream: TEditStream;
begin
  if (RichEditClass = nil) then Exit(wlxInvalidHandle);

  AFile:= CreateFileW(FileToLoad, GENERIC_READ, FILE_SHARE_READ, nil,
                      OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);

  if (AFile = INVALID_HANDLE_VALUE) then Exit(wlxInvalidHandle);

  hInstance := GetModuleHandleW(nil);

  if not GetClientRect(ParentWin, @ARect) then ARect:= TRect.Create(0, 0, 640, 480);

  Result:= CreateWindowW(RichEditClass, nil, WS_CHILD or WS_HSCROLL or WS_VSCROLL or ES_READONLY or
                         ES_MULTILINE or ES_AUTOHSCROLL or ES_AUTOVSCROLL or WS_TABSTOP,
                         ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, ParentWin, hInstance, 0, nil);

  if (Result <> wlxInvalidHandle) then
  begin
    AStream.dwError     := 0;
    AStream.dwCookie    := DWORD_PTR(AFile);
    AStream.pfnCallback := @EditStreamCallback;

    AResult := (SendMessage(Result, EM_STREAMIN, SF_RTF, LPARAM(@AStream)) <> 0) and (AStream.dwError = 0);

    if AResult then
    begin
      AWndProc:= SetWindowLongPtr(Result, GWLP_WNDPROC, LONG_PTR(@RichEditWndProc));
      SetWindowLongPtr(Result, GWLP_USERDATA, AWndProc);
      ShowWindow(Result, SW_SHOW);
    end
    else begin
      DestroyWindow(Result);
      Result:= wlxInvalidHandle;
    end;
  end;
  CloseHandle(AFile);
end;

function ListSearchTextW(ListWin: HWND; SearchString: PWideChar; SearchParameter: Integer): Integer; stdcall;
var
  AMode: TFindTextExW;
  wParam: Windows.WPARAM;
  ARange: RichEdit.TCharRange;
begin
  AMode:= Default(TFindTextExW);
  AMode.lpstrText:= SearchString;
  SendMessageW(ListWin, EM_EXGETSEL, 0, LPARAM(@ARange));
  if (SearchParameter and lcs_findfirst <> 0) then
  begin
    if SearchParameter and lcs_backwards <> 0 then
      ARange.cpMin:= High(LONG)
    else
      ARange.cpMax:= 0;
  end;
  if SearchParameter and lcs_backwards <> 0 then
  begin
    wParam:= 0;
    AMode.chrg.cpMax:= 0;
    AMode.chrg.cpMin:= ARange.cpMin;
  end
  else begin
    wParam:= FR_DOWN;
    AMode.chrg.cpMax:= -1;
    AMode.chrg.cpMin:= ARange.cpMax;
  end;
  if (SearchParameter and lcs_matchcase <> 0) then wParam:= wParam or FR_MATCHCASE;
  if (SearchParameter and lcs_wholewords <> 0) then wParam:= wParam or FR_WHOLEWORD;
  if SendMessageW(ListWin, EM_FINDTEXTEXW, wParam, LPARAM(@AMode)) >= 0 then
  begin
    Result:= LISTPLUGIN_OK;
    SendMessageW(ListWin, EM_EXSETSEL, 0, LPARAM(@AMode.chrgText));
  end
  else begin
    Result:= LISTPLUGIN_ERROR;
  end;
end;

function ListSendCommand(ListWin: HWND; Command, Parameter: Integer): Integer; stdcall;
var
  ARange: TCharRange;
begin
  case Command of
    lc_selectall:
    begin
      ARange.cpMin:= 0;
      ARange.cpMax:= -1;
      if SendMessageW(ListWin, EM_EXSETSEL, 0, LPARAM(@ARange)) >= 0 then
        Result:= LISTPLUGIN_OK
      else begin
        Result:= LISTPLUGIN_ERROR;
      end;
    end;
    lc_copy:
    begin
      if SendMessageW(ListWin, WM_COPY, 0, 0) <> 0 then
        Result:= LISTPLUGIN_OK
      else begin
        Result:= LISTPLUGIN_ERROR;
      end;
    end;
    else begin
      Result:= LISTPLUGIN_ERROR;
    end;
  end;
end;

procedure ListGetDetectString(DetectString: PAnsiChar; MaxLen: Integer); stdcall;
begin
  lstrcpynA(DetectString, 'EXT="RTF"', MaxLen);
end;

procedure ListSetDefaultParams(dps: PListDefaultParamStruct); stdcall;
begin
  // Rich Edit Version 4.1
  if (LoadLibraryX('Msftedit.dll') <> NilHandle) then
    RichEditClass := 'RichEdit50W'
  // Rich Edit Version 2.0/3.0
  else if (LoadLibraryX('riched20.dll') <> NilHandle) then
    RichEditClass := 'RichEdit20W'
  else begin
    RichEditClass := nil;
  end;
end;

exports
  ListLoadW,
  ListSearchTextW,
  ListSendCommand,
  ListGetDetectString,
  ListSetDefaultParams;

end.

