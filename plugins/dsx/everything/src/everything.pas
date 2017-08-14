{
  Everything search engine interface via IPC

  Copyright (C) 2017 Alexander Koblov (alexx2000@mail.ru)

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
  COPYDATA_IPCTEST_QUERYCOMPLETEW = 0;
  MSGFLT_RESET		          = 0;
  MSGFLT_ALLOW                    = 1;
  MSGFLT_DISALLOW                 = 2;

  EVERYTHING_IPC_COPYDATAQUERYW   = 2;

  EVERYTHING_IPC_SEARCH_WNDCLASS  = 'EVERYTHING';
  EVERYTHING_IPC_WNDCLASS         = 'EVERYTHING_TASKBAR_NOTIFICATION';

  EVERYTHING_IPC_ALLRESULTS       = $FFFFFFFF; // all results

  EVERYTHING_IPC_FOLDER           = $00000001;	// The item is a folder. (its a file if not set)
  EVERYTHING_IPC_DRIVE            = $00000002;	// The folder is a drive. Path will be an empty string.

  // search flags for querys
  EVERYTHING_IPC_MATCHCASE        = $00000001;	// match case
  EVERYTHING_IPC_REGEX            = $00000008;	// enable regex

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

var
  ChangeWindowMessageFilterEx: function(hWnd: HWND; message: UINT; action: DWORD; filter: PChangeFilterStruct): BOOL; stdcall;

procedure Start(FileMask: String; Flags: Integer; pr: TFoundCallback);

implementation

function SendQuery(hwnd: HWND; num: DWORD; search_string: PWideChar; search_flags: integer): Boolean;
var
  query: ^TEVERYTHING_IPC_QUERYW;
  len: Int32;
  size: Int32;
  everything_hwnd: HWND;
  cds: COPYDATASTRUCT;
begin
  everything_hwnd:= FindWindow(EVERYTHING_IPC_WNDCLASS, nil);
  if (everything_hwnd <> 0) then
  begin
    len := StrLen(search_string);

    size := SizeOf(TEVERYTHING_IPC_QUERYW) - SizeOf(WideChar) + len * SizeOf(WideChar) + SizeOf(WideChar);

    query := GetMem(size);
    if Assigned(query) then
    begin
      query^.offset := 0;
      query^.max_results := num;
      query^.reply_copydata_message := COPYDATA_IPCTEST_QUERYCOMPLETEW;
      query^.search_flags := search_flags;
      query^.reply_hwnd := hwnd;
      StrLCopy(@query^.search_string, search_string, len);

      cds.cbData := size;
      cds.dwData := EVERYTHING_IPC_COPYDATAQUERYW;
      cds.lpData := query;

      if (SendMessage(everything_hwnd, WM_COPYDATA, WPARAM(hwnd), LPARAM(@cds)) <> 0) then
      begin
	      //HeapFree(GetProcessHeap(),0,query);

	      //return 1;
      REsult:= True;
      end
      else
      begin
	      //write(L"Everything IPC service not running.\n");
      end;

      FreeMem(query);
    end;
  end;

  Result:= False;
end;

function EVERYTHING_IPC_ITEMPATHW(list: PEVERYTHING_IPC_LISTW; item: PEVERYTHING_IPC_ITEMW): PWideChar;
begin
  Result:= PWideChar(PByte(list) + item^.path_offset);
end;

function EVERYTHING_IPC_ITEMFILENAMEW(list: PEVERYTHING_IPC_LISTW; item: PEVERYTHING_IPC_ITEMW): PWideChar;
begin
  Result:= PWideChar(PByte(list) + item^.filename_offset);
end;

procedure listresultsW(hwnd2: HWND; list: PEVERYTHING_IPC_LISTW);
var
  I: Integer;
  Item: PEVERYTHING_IPC_ITEMW;
  CallB: TFoundCallback;
  Result: PWideChar;
  Res: UnicodeString;
begin
  CallB:= TFoundCallback(GetWindowLongPtr(hwnd2, GWL_USERDATA));

  for i:=0 to list^.numitems - 1 do
  begin
          Item:= PEVERYTHING_IPC_ITEMW(@list^.items) + i;
	  if (Item^.flags and EVERYTHING_IPC_DRIVE) <> 0 then
	  begin
		  //WriteLn(WideString(EVERYTHING_IPC_ITEMFILENAMEW(list, Item)));
                  Result:= EVERYTHING_IPC_ITEMFILENAMEW(list, Item);
	  end
	  else
	  begin
		  //WriteLn(WideString(EVERYTHING_IPC_ITEMPATHW(list, Item)));

		  //WriteLn(WideString(EVERYTHING_IPC_ITEMFILENAMEW(list, Item)));
                  Res:= UnicodeString(EVERYTHING_IPC_ITEMPATHW(list, Item)) + PathDelim + UnicodeString(EVERYTHING_IPC_ITEMFILENAMEW(list, Item));
                  Result:= PWideChar(Res);//EVERYTHING_IPC_ITEMFILENAMEW(list, Item);
	  end;
          CallB(REsult);
  end;
  CallB(nil);
  PostQuitMessage(0);
end;

function window_proc(hwnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  cds: PCOPYDATASTRUCT;
begin
  case msg of
    WM_COPYDATA:
    begin
      cds := PCOPYDATASTRUCT(lParam);
      if cds^.dwData = COPYDATA_IPCTEST_QUERYCOMPLETEW then
      begin
	listresultsW(hwnd, PEVERYTHING_IPC_LISTW(cds^.lpData));
	Exit(1);
      end;
    end;
  end;
  Result:= DefWindowProc(hwnd, msg, wParam, lParam);
end;

procedure Start(FileMask: String; Flags: Integer; pr: TFoundCallback);
var
  wcex: WNDCLASSEX;
  hwnd2: HWND;
  HH: HMODULE;
  lpMsg: TMsg;
begin
  ZeroMemory(@wcex, SizeOf(wcex));
  wcex.cbSize := sizeof(wcex);
  wcex.hInstance := System.HINSTANCE;;
  wcex.lpfnWndProc := @window_proc;
  wcex.lpszClassName := 'IPCTEST';

  		if (RegisterClassEx(@wcex) = 0) then
  		begin
  			WriteLn('failed to register IPCTEST window class');


                end;

        	hwnd2 := CreateWindow(
        		'IPCTEST',
        		'',
        		0,
        		0,0,0,0,
        		0,0,HINSTANCE,nil);

HH:= LoadLibrary('user32.dll');
Pointer(ChangeWindowMessageFilterEx) := GetProcAddress(HH, 'ChangeWindowMessageFilterEx');
ChangeWindowMessageFilterEx(hwnd2, WM_COPYDATA, MSGFLT_ALLOW, nil);

SetWindowLongPtr(hwnd2, GWL_USERDATA, LONG_PTR(pr));

sendquery(hwnd2,EVERYTHING_IPC_ALLRESULTS,PWideChar(WideString(FileMask)), Flags);
while (True) do
                begin
        	if (PeekMessage(lpmsg, 0,0,0,0)) then
        	begin
        		if not GetMessage(lpmsg,0,0,0) then Exit;

        		// let windows handle it.
        		TranslateMessage(lpmsg);
        		DispatchMessage(lpmsg);
                end
        	else
        	begin
        		WaitMessage();
        	end;
end;

end;

end.

