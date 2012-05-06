
function BeginDeferWindowPos(nNumWindows:longint):THandle;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function CopyImage(hImage:THANDLE; uType:LongWord; cxDesired, cyDesired: LongInt; fuFlags:LongWord):THandle;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function DeferWindowPos(hWinPosInfo, hWnd, hWndInsertAfter:THandle; x, y, cx, cy:longint; uFlags:LongWord):THandle;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function EndDeferWindowPos(hWinPosInfo:THandle):Boolean;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GdiFlush: Boolean;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GetACP:LongWord;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

{$ifndef HAS_GETBKCOLOR}
function GetBkColor(DC:HDC):COLORREF;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;
{$endif}

function GetDCEx(hWnd:HWND; hrgnClip:HRGN; flags:DWORD):HDC;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GetKeyboardLayout(dwLayout:DWORD):THandle;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GetKeyboardState(lpKeyState: System.PByte):BOOLEAN;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GetLocaleInfo(Locale, LCType:LongWord; lpLCData:PChar; cchData:longint):longint;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function GetRandomRgn(DC: HDC; Rgn: HRGN; iNum: Integer): Integer; stdcall;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

{$ifndef HAS_GETTEXTEXTENTEXPOINT}
function GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
begin
  Result := LCLIntf.GetTextExtentExPoint(DC, Str, Count, MaxWidth, MaxCount, PartialWidths, Size);
end;
{$endif}

{$ifndef HAS_GETTEXTALIGN}
function GetTextAlign(hDC:HDC): LongWord;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;
{$endif}

{$ifndef HAS_GETWINDOWDC}
function GetWindowDC(hWnd:HWND):HDC;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;
{$endif}

function ImageList_DragShowNolock(fShow: Boolean): Boolean;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

function MultiByteToWideChar(CodePage, dwFlags:DWORD; lpMultiByteStr:PChar; cchMultiByte:longint; lpWideCharStr:PWideChar;cchWideChar:longint):longint;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

{$ifndef HAS_OFFSETRGN}
function OffsetRgn(hrgn:HRGN; nxOffset, nYOffset:longint):longint;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;
{$endif}

function ScrollDC(DC:HDC; dx:longint; dy:longint; var lprcScroll:TRECT; var lprcClip:TRECT;hrgnUpdate:HRGN; lprcUpdate:PRECT):Boolean;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;

{$ifndef HAS_SETBRUSHORGEX}
function SetBrushOrgEx(DC:HDC; nXOrg, nYOrg:longint; lppt:PPOINT):Boolean;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;
{$endif}

function ToAscii(uVirtKey, uScanCode:LongWord; lpKeyState: System.PByte; lpChar: System.PWord; uFlags:LongWord):longint;
begin
  {$ifdef DEBUG_DELPHICOMPAT} Logger.SendCallStack('Dummy WinAPI Implementation'); {$endif}
end;
