{
   Double commander
   -------------------------------------------------------------------------
   Windows Media Player plugin

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

library wmp;

{$mode objfpc}{$H+}

uses
  Classes, WMPLib_1_0_TLB, ActiveXContainer, SysUtils, Windows, WlxPlugin;

const
  CLASS_NAME = UnicodeString('IWMPPlayer4');

function ListLoadW(ParentWin: HWND; FileToLoad: PWideChar; ShowFlags: Integer): HWND; stdcall;
var
  ARect: TRect;
  APlayer: IWMPPlayer4;
  AData: TActiveXContainer;
  WindowClassW: TWndClassW;
begin
  try
    APlayer:= CoWindowsMediaPlayer.Create;
  except
    Exit(wlxInvalidHandle);
  end;

  ZeroMemory(@WindowClassW, SizeOf(WndClassW));
  with WindowClassW do
  begin
    Style := CS_DBLCLKS;
    lpfnWndProc := @DefWindowProc;
    cbWndExtra := SizeOf(Pointer);
    hCursor := LoadCursor(0, IDC_ARROW);
    lpszClassName := CLASS_NAME;
    hInstance := GetModuleHandleW(nil);
  end;
  Windows.RegisterClassW(WindowClassW);

  if not GetClientRect(ParentWin, ARect) then
    ARect:= TRect.Create(0, 0, 640, 480);

  Result:= CreateWindowW(CLASS_NAME, 'PreviewHandler', WS_CHILD or WS_VISIBLE, ARect.Left, ARect.Top,
                         ARect.Right, ARect.Bottom, ParentWin, 0, WindowClassW.hInstance, nil);

  if (Result <> wlxInvalidHandle) then
  begin
    AData:= TActiveXContainer.Create(nil);
    AData.Handle:= Result;
    AData.ComServer:= APlayer;
    try
      AData.Active:= True;
      APlayer.URL:= WideString(FileToLoad);
      APlayer.Controls.Play;
    except
      AData.Free;
      DestroyWindow(Result);
      Result:= wlxInvalidHandle;
    end;
  end;
end;

procedure ListCloseWindow(ListWin: HWND); stdcall;
var
  AData: TActiveXContainer;
  AHandle: THandle absolute AData;
begin
  AHandle:= GetWindowLongPtr(ListWin, GWLP_USERDATA);
  DestroyWindow(ListWin);
  if Assigned(AData) then AData.Free;
end;

function ListLoadNextW(ParentWin, PluginWin: HWND; FileToLoad: PWideChar; ShowFlags: Integer): Integer; stdcall;
var
  APlayer: IWMPPlayer4;
  AData: TActiveXContainer;
  AHandle: THandle absolute AData;
begin
  AHandle:= GetWindowLongPtr(PluginWin, GWLP_USERDATA);
  if Assigned(AData) then
  begin
    APlayer:= AData.ComServer as IWMPPlayer4;
    try
      APlayer.Controls.Stop;
      APlayer.URL:= WideString(FileToLoad);
      APlayer.Controls.Play;
    except
      Exit(LISTPLUGIN_ERROR);
    end;
  end;
  Result:= LISTPLUGIN_OK;
end;

procedure ListGetDetectString(DetectString: PAnsiChar; MaxLen: Integer); stdcall;
begin
  StrLCopy(DetectString, '(EXT="WAV")|(EXT="MP3")|(EXT="WMA")|(EXT="MP4")|(EXT="AVI")|(EXT="WMV")', MaxLen);
end;

exports
  ListLoadW,
  ListLoadNextW,
  ListCloseWindow,
  ListGetDetectString;

end.

