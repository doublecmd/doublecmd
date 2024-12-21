{
  Everything search engine interface via IPC

  Copyright (C) 2017-2023 Alexander Koblov (alexx2000@mail.ru)

  Based on Everything command line interface source

  Copyright (C) 2016 David Carpenter

  Permission is hereby granted, free of charge, to any person obtaining
  a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

unit everything;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

const
  EVERYTHING_DSX_WNDCLASS         = 'EVERYTHING_DSX';

const
  COPYDATA_IPCTEST_QUERYCOMPLETEW = 0;
  MSGFLT_RESET                    = 0;
  MSGFLT_ALLOW                    = 1;
  MSGFLT_DISALLOW                 = 2;

  EVERYTHING_IPC_COPYDATAQUERYW   = 2;

  EVERYTHING_IPC_SEARCH_WNDCLASS  = 'EVERYTHING';
  EVERYTHING_IPC_WNDCLASS         = 'EVERYTHING_TASKBAR_NOTIFICATION';

  EVERYTHING_IPC_ALLRESULTS       = $FFFFFFFF; // all results

  EVERYTHING_IPC_FOLDER           = $00000001;  // The item is a folder. (its a file if not set)
  EVERYTHING_IPC_DRIVE            = $00000002;  // The folder is a drive. Path will be an empty string.

  // search flags for querys
  EVERYTHING_IPC_MATCHCASE        = $00000001;  // match case
  EVERYTHING_IPC_MATCHPATH        = $00000004;  // include paths in search
  EVERYTHING_IPC_REGEX            = $00000008;  // enable regex

type
  PChangeFilterStruct = ^TChangeFilterStruct;
  TChangeFilterStruct = record
    cbSize: DWORD;
    ExtStatus: DWORD;
  end;

  {$push}{$packrecords 1}

  TEVERYTHING_IPC_QUERYW = record
    // the window that will receive the new results.
    reply_hwnd: HWND;

    // the value to set the dwData member in the COPYDATASTRUCT struct
    // sent by Everything when the query is complete.
    reply_copydata_message: ULONG_PTR;

    // search flags (see EVERYTHING_MATCHCASE | EVERYTHING_MATCHWHOLEWORD | EVERYTHING_MATCHPATH)
    search_flags: DWORD;

    // only return results after 'offset' results (0 to return the first result)
    // useful for scrollable lists
    offset: DWORD;

    // the number of results to return
    // zero to return no results
    // EVERYTHING_IPC_ALLRESULTS to return ALL results
    max_results: DWORD;

    // null terminated string. arbitrary sized search_string buffer.
    search_string: WCHAR;
  end;

  PEVERYTHING_IPC_ITEMW = ^TEVERYTHING_IPC_ITEMW;
  TEVERYTHING_IPC_ITEMW = record
    // item flags
    flags: DWORD;

    // The offset of the filename from the beginning of the list structure.
    // (wchar_t *)((char *)everything_list + everythinglist->name_offset)
    filename_offset: DWORD;

    // The offset of the filename from the beginning of the list structure.
    // (wchar_t *)((char *)everything_list + everythinglist->path_offset)
    path_offset: DWORD;
  end;

  PEVERYTHING_IPC_LISTW = ^TEVERYTHING_IPC_LISTW;
  TEVERYTHING_IPC_LISTW = record
    // the total number of folders found.
    totfolders: DWORD;

    // the total number of files found.
    totfiles: DWORD;

    // totfolders + totfiles
    totitems: DWORD;

    // the number of folders available.
    numfolders: DWORD;

    // the number of files available.
    numfiles: DWORD;

    // the number of items available.
    numitems: DWORD;

    // index offset of the first result in the item list.
    offset: DWORD;

    // arbitrary sized item list.
    // use numitems to determine the actual number of items available.
    items: TEVERYTHING_IPC_ITEMW;
  end;

  {$pop}

type
  TFoundCallback = procedure(FileName: PWideChar);

procedure Start(FileMask: String; Flags: Integer; pr: TFoundCallback);

implementation

var
  ChangeWindowMessageFilterEx: function(hWnd: HWND; message: UINT; action: DWORD; filter: PChangeFilterStruct): BOOL; stdcall;

function SendQuery(hwnd: HWND; num: DWORD; const search_string: UnicodeString; search_flags: integer): Boolean;
var
  len: Int32;
  size: Int32;
  cds: COPYDATASTRUCT;
  everything_hwnd: HWND;
  query: ^TEVERYTHING_IPC_QUERYW;
begin
  everything_hwnd:= FindWindow(EVERYTHING_IPC_WNDCLASS, nil);
  if (everything_hwnd = 0) then
    MessageBoxW(0, 'Everything not found!', nil, MB_OK or MB_ICONERROR)
  else begin
    len:= Length(search_string);
    size := SizeOf(TEVERYTHING_IPC_QUERYW) + len * SizeOf(WideChar);

    query:= GetMem(size);
    if (query = nil) then
    begin
      MessageBoxW(0, PWideChar(UnicodeString(SysErrorMessage(E_OUTOFMEMORY))), nil, MB_OK or MB_ICONERROR);
      Exit(False);
    end;
    try
      query^.offset := 0;
      query^.max_results := num;
      query^.reply_copydata_message := COPYDATA_IPCTEST_QUERYCOMPLETEW;
      query^.search_flags := search_flags;
      query^.reply_hwnd := hwnd;
      StrPLCopy(@query^.search_string, search_string, len);

      cds.cbData := size;
      cds.dwData := EVERYTHING_IPC_COPYDATAQUERYW;
      cds.lpData := query;

      if (SendMessage(everything_hwnd, WM_COPYDATA, WPARAM(hwnd), LPARAM(@cds)) <> 0) then
      begin
        Exit(True);
      end;
    finally
      FreeMem(query);
    end;
  end;
  Result:= False;
end;

function EVERYTHING_IPC_ITEMPATHW(AList: PEVERYTHING_IPC_LISTW; Item: PEVERYTHING_IPC_ITEMW): PWideChar; inline;
begin
  Result:= PWideChar(PByte(AList) + Item^.path_offset);
end;

function EVERYTHING_IPC_ITEMFILENAMEW(AList: PEVERYTHING_IPC_LISTW; Item: PEVERYTHING_IPC_ITEMW): PWideChar; inline;
begin
  Result:= PWideChar(PByte(AList) + Item^.filename_offset);
end;

procedure ListResults(hWnd: HWND; AList: PEVERYTHING_IPC_LISTW);
var
  I: DWORD = 0;
  Temp: UnicodeString;
  FileName: PWideChar;
  Callback: TFoundCallback;
  Item: PEVERYTHING_IPC_ITEMW;
  dwOldLong: LONG_PTR absolute Callback;
begin
  dwOldLong:= GetWindowLongPtr(hWnd, GWL_USERDATA);

  while I < AList^.numitems do
  begin
    Item:= PEVERYTHING_IPC_ITEMW(@AList^.items) + I;

    if (Item^.flags and EVERYTHING_IPC_DRIVE) <> 0 then
    begin
      FileName:= EVERYTHING_IPC_ITEMFILENAMEW(AList, Item);
    end
    else
    begin
      Temp:= UnicodeString(EVERYTHING_IPC_ITEMPATHW(AList, Item)) + PathDelim + UnicodeString(EVERYTHING_IPC_ITEMFILENAMEW(AList, Item));
      FileName:= PWideChar(Temp);
    end;

    Callback(FileName);
    Inc(I);
  end;
  Callback(nil);
  PostMessage(hWnd, WM_CLOSE, 0, 0);
end;

function WindowProc(hwnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  cds: PCopyDataStruct absolute lParam;
begin
  Result:= 0;
  case msg of
    WM_COPYDATA:
    begin
      if cds^.dwData = COPYDATA_IPCTEST_QUERYCOMPLETEW then
      begin
        ListResults(hwnd, PEVERYTHING_IPC_LISTW(cds^.lpData));
        Exit(1);
      end;
    end;
    WM_CLOSE:
      DestroyWindow(hwnd);
    else
      Result:= DefWindowProc(hwnd, msg, wParam, lParam);
  end;
end;

procedure Start(FileMask: String; Flags: Integer; pr: TFoundCallback);
var
  hWnd: Windows.HWND;
  dwNewLong: LONG_PTR absolute pr;
begin
  hWnd := CreateWindow(EVERYTHING_DSX_WNDCLASS,
                       '', 0, 0, 0, 0, 0, 0, 0, HINSTANCE,nil);

  if Assigned(ChangeWindowMessageFilterEx) then
  begin
    ChangeWindowMessageFilterEx(hWnd, WM_COPYDATA, MSGFLT_ALLOW, nil);
  end;

  SetWindowLongPtr(hWnd, GWL_USERDATA, dwNewLong);

  if not SendQuery(hWnd, EVERYTHING_IPC_ALLRESULTS, UnicodeString(FileMask), Flags) then
  begin
    pr(nil);
    PostMessage(hWnd, WM_CLOSE, 0, 0);
  end;
end;

procedure Initialize;
var
  hUser32: HMODULE;
  wcex: TWndClassEx;
begin
  wcex:= Default(TWndClassEx);
  wcex.cbSize:= SizeOf(TWndClassEx);
  wcex.hInstance:= System.HINSTANCE;
  wcex.lpfnWndProc:= @WindowProc;
  wcex.lpszClassName:= EVERYTHING_DSX_WNDCLASS;

  if (RegisterClassEx(@wcex) = 0) then
  begin
    // WriteLn('failed to register IPCTEST window class');
  end;
  hUser32:= GetModuleHandle(User32);
  Pointer(ChangeWindowMessageFilterEx):= GetProcAddress(hUser32, 'ChangeWindowMessageFilterEx');
end;

initialization
  Initialize;

end.

