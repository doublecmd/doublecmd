{
  Double Commander
  -------------------------------------------------------------------------
  Windows pseudoterminal device implementation

  Copyright (C) 2021 Alexander Koblov (alexx2000@mail.ru)

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

unit VTEmuPty;

{$mode delphi}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, Windows, VTEmuCtl;

type

  { TPtyDevice }

  TPtyDevice = class(TCustomPtyDevice)
  private
    FPty: PVOID;
    FSize: TCoord;
    FLength: Integer;
    FThread: TThread;
    FPipeIn, FPipeOut: THandle;
    FBuffer: array[0..8191] of AnsiChar;
  protected
    procedure ReadySync;
    procedure ReadThread;
    procedure DestroyPseudoConsole;
    procedure SetConnected(AValue: Boolean); override;
    function CreatePseudoConsole(const ACommand: String): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function WriteStr(const Str: string): Integer; override;
    function SetCurrentDir(const Path: String): Boolean; override;
    function SetScreenSize(aCols, aRows: Integer): Boolean; override;
  end;

implementation

uses
  CTypes, DCOSUtils, DCConvertEncoding;

type
  TConsoleType = (ctNone, ctNative, ctEmulate);

var
  ConsoleType: TConsoleType = ctNone;

procedure ClosePipe(var AHandle: THandle);
begin
  if (AHandle <> INVALID_HANDLE_VALUE) then
  begin
    CloseHandle(AHandle);
    AHandle:= INVALID_HANDLE_VALUE;
  end;
end;

{
 *******************************************************************************
   Windows Pseudo Console (ConPTY), Windows 10 1809 and higher
 *******************************************************************************
}

const
  EXTENDED_STARTUPINFO_PRESENT = $00080000;
  PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE = $00020016;

type
  PHPCON = ^HPCON;
  HPCON = type PVOID;
  SIZE_T = type ULONG_PTR;
  PSIZE_T = type PULONG_PTR;
  LPPROC_THREAD_ATTRIBUTE_LIST = type PVOID;

  STARTUPINFOEXW = record
    StartupInfo: STARTUPINFOW;
    lpAttributeList: LPPROC_THREAD_ATTRIBUTE_LIST;
  end;

var
  CreatePseudoConsole: function(size: COORD; hInput: HANDLE; hOutput: HANDLE;
                                dwFlags: DWORD; phPC: PHPCON): HRESULT; stdcall;
  ClosePseudoConsole: procedure(hPC: HPCON); stdcall;
  ResizePseudoConsole: function(hPC: HPCON; size: COORD): HRESULT; stdcall;
  InitializeProcThreadAttributeList: function(lpAttributeList: LPPROC_THREAD_ATTRIBUTE_LIST;
                                              dwAttributeCount: DWORD; dwFlags: DWORD;
                                              lpSize: PSIZE_T): BOOL; stdcall;
  UpdateProcThreadAttribute: function(lpAttributeList: LPPROC_THREAD_ATTRIBUTE_LIST;
                                      dwFlags: DWORD; Attribute: DWORD_PTR;
                                      lpValue: PVOID; cbSize: SIZE_T;
                                      lpPreviousValue: PVOID; lpReturnSize: PSIZE_T): BOOL; stdcall;
  DeleteProcThreadAttributeList: procedure(lpAttributeList: LPPROC_THREAD_ATTRIBUTE_LIST); stdcall;

function CreatePseudoConsoleNew(const ACommand: String; phPC: PPointer; phPipeIn, phPipeOut: PHandle; ASize: COORD): Boolean;
var
  attrListSize: SIZE_T = 0;
  startupInfo: STARTUPINFOEXW;
  piClient: PROCESS_INFORMATION;
  hPipePTYIn: HANDLE = INVALID_HANDLE_VALUE;
  hPipePTYOut: HANDLE = INVALID_HANDLE_VALUE;
begin
  startupInfo:= Default(STARTUPINFOEXW);
  Result:= CreatePipe(hPipePTYIn, phPipeOut^, nil, 0) and
           CreatePipe(phPipeIn^, hPipePTYOut, nil, 0);
  if Result then
  begin
    Result:= CreatePseudoConsole(ASize, hPipePTYIn, hPipePTYOut, 0, phPC) = S_OK;
    // We can close the handles here because they are duplicated in the ConHost
    if Result then
    begin
      CloseHandle(hPipePTYIn);
      CloseHandle(hPipePTYOut);
      hPipePTYIn:= INVALID_HANDLE_VALUE;
      hPipePTYOut:=INVALID_HANDLE_VALUE;
    end;
  end;
  if Result then
  begin
    startupInfo.StartupInfo.cb:= SizeOf(STARTUPINFOEXW);
    InitializeProcThreadAttributeList(nil, 1, 0, @attrListSize);
    startupInfo.lpAttributeList:= GetMem(attrListSize);
    Result:= Assigned(startupInfo.lpAttributeList);
    if Result then
    begin
      // Initialize thread attribute list and set Pseudo Console attribute
      Result:= InitializeProcThreadAttributeList(startupInfo.lpAttributeList, 1, 0, @attrListSize) and
               UpdateProcThreadAttribute(startupInfo.lpAttributeList,0,
                                         PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
                                         phPC^, SizeOf(HPCON), nil, nil);
    end;
  end;
  if Result then
  begin
    Result:= CreateProcessW(nil, PWideChar(CeUtf8ToUtf16(ACommand)),
                            nil, nil, False, EXTENDED_STARTUPINFO_PRESENT,
                            nil, nil, @startupInfo.StartupInfo, @piClient);
  end;
  if not Result then
  begin
    ClosePipe(phPipeIn^);
    ClosePipe(phPipeOut^);
    ClosePipe(hPipePTYIn);
    ClosePipe(hPipePTYOut);
  end;
  // Cleanup attribute list
  if Assigned(startupInfo.lpAttributeList) then
  begin
    DeleteProcThreadAttributeList(startupInfo.lpAttributeList);
    FreeMem(startupInfo.lpAttributeList);
  end;
end;

function InitializeNew: Boolean;
var
  hModule: HINST;
begin
  Result:= (Win32MajorVersion >= 10);
  if Result then
  begin
    hModule:= GetModuleHandle(Kernel32);
    CreatePseudoConsole:= GetProcAddress(hModule, 'CreatePseudoConsole');
    Result:= Assigned(CreatePseudoConsole);
    if Result then
    begin
      ClosePseudoConsole:= GetProcAddress(hModule, 'ClosePseudoConsole');
      ResizePseudoConsole:= GetProcAddress(hModule, 'ResizePseudoConsole');
      UpdateProcThreadAttribute:= GetProcAddress(hModule, 'UpdateProcThreadAttribute');
      DeleteProcThreadAttributeList:= GetProcAddress(hModule, 'DeleteProcThreadAttributeList');
      InitializeProcThreadAttributeList:= GetProcAddress(hModule, 'InitializeProcThreadAttributeList');
    end;
  end;
end;

{
 *******************************************************************************
   WinPTY
 *******************************************************************************
}

const
  WINPTY_MOUSE_MODE_AUTO                     = 1;
  WINPTY_MOUSE_MODE_FORCE                    = 2;
  // Agent RPC call: process creation
  WINPTY_SPAWN_FLAG_AUTO_SHUTDOWN            = 1;
  WINPTY_SPAWN_FLAG_EXIT_AFTER_SHUTDOWN      = 2;
  // Configuration of a new agent
  WINPTY_FLAG_CONERR                         = $01;
  WINPTY_FLAG_PLAIN_OUTPUT                   = $02;
  WINPTY_FLAG_COLOR_ESCAPES                  = $04;
  WINPTY_FLAG_ALLOW_CURPROC_DESKTOP_CREATION = $08;
  // Error codes
  WINPTY_ERROR_SUCCESS                        = 0;
  WINPTY_ERROR_OUT_OF_MEMORY                  = 1;
  WINPTY_ERROR_SPAWN_CREATE_PROCESS_FAILED    = 2;
  WINPTY_ERROR_LOST_CONNECTION                = 3;
  WINPTY_ERROR_AGENT_EXE_MISSING              = 4;
  WINPTY_ERROR_UNSPECIFIED                    = 5;
  WINPTY_ERROR_AGENT_DIED                     = 6;
  WINPTY_ERROR_AGENT_TIMEOUT                  = 7;
  WINPTY_ERROR_AGENT_CREATION_FAILED          = 8;

type
  winpty_t = record end;
  Pwinpty_t = ^winpty_t;

  winpty_result_t = type DWORD;

  winpty_config_t = record end;
  Pwinpty_config_t = ^winpty_config_t;

  winpty_error_t = record end;
  winpty_error_ptr_t = ^winpty_error_t;
  Pwinpty_error_ptr_t = ^winpty_error_ptr_t;

  winpty_spawn_config_t = record end;
  Pwinpty_spawn_config_t = ^winpty_spawn_config_t;

var
  winpty_config_new: function(agentFlags: UInt64; err: Pwinpty_error_ptr_t): Pwinpty_config_t; cdecl;
  winpty_config_free: procedure(cfg: Pwinpty_config_t); cdecl;

  winpty_config_set_initial_size: procedure(cfg: Pwinpty_config_t; cols, rows: cint); cdecl;
  winpty_config_set_mouse_mode: procedure(cfg: Pwinpty_config_t; mouseMode: cint); cdecl;

  winpty_open: function(const cfg: Pwinpty_config_t; err: Pwinpty_error_ptr_t): Pwinpty_t; cdecl;
  winpty_free: procedure(wp: Pwinpty_t); cdecl;

  winpty_error_code: function(err: winpty_error_ptr_t): winpty_result_t; cdecl;
  winpty_error_msg: function(err: winpty_error_ptr_t): LPCWSTR; cdecl;
  winpty_error_free: procedure(err: winpty_error_ptr_t); cdecl;

  winpty_spawn_config_new: function(spawnFlags: UInt64; appname, cmdline, cwd,
                                    env: LPCWSTR; err: Pwinpty_error_ptr_t): Pwinpty_spawn_config_t; cdecl;
  winpty_spawn_config_free: procedure(cfg: Pwinpty_spawn_config_t); cdecl;

  winpty_spawn: function(wp: Pwinpty_t; const cfg: Pwinpty_spawn_config_t;
                         process_handle, thread_handle: PHandle;
                         create_process_error: PDWORD; err: Pwinpty_error_ptr_t): BOOL; cdecl;

  winpty_set_size: function(wp: Pwinpty_t; cols, rows: cint; err: Pwinpty_error_ptr_t): BOOL; cdecl;

  winpty_conin_name: function(wp: Pwinpty_t): LPCWSTR; cdecl;
  winpty_conout_name: function(wp: Pwinpty_t): LPCWSTR; cdecl;
  winpty_conerr_name: function(wp: Pwinpty_t): LPCWSTR; cdecl;

function CreatePseudoConsoleOld(const ACommand: String; phPC: PPointer; phPipeIn, phPipeOut: PHandle; ASize: COORD): Boolean;
var
  childHandle: HANDLE;
  lastError: DWORD = 0;
  agentCfg: Pwinpty_config_t;
  spawnCfg: Pwinpty_spawn_config_t;
  agentFlags: DWORD = WINPTY_FLAG_ALLOW_CURPROC_DESKTOP_CREATION;
begin
  // SetEnvironmentVariableW('WINPTY_SHOW_CONSOLE', '1');
  agentCfg:= winpty_config_new(agentFlags, nil);
  Result:= Assigned(agentCfg);
  if Result then
  begin
    winpty_config_set_initial_size(agentCfg, ASize.X, ASize.Y);

    phPC^:= winpty_open(agentCfg, nil);

    Result:= Assigned(phPC^);

    winpty_config_free(agentCfg);

    if Result then
    begin
      phPipeIn^:= CreateFileW(winpty_conout_name(phPC^), GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0);
      phPipeOut^:= CreateFileW(winpty_conin_name(phPC^), GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);

      spawnCfg:= winpty_spawn_config_new(WINPTY_SPAWN_FLAG_AUTO_SHUTDOWN,
                                         nil, PWideChar(CeUtf8ToUtf16(ACommand)), nil, nil, nil);
      Result:= Assigned(spawnCfg);
      if Result then
      begin
        Result:= winpty_spawn(phPC^, spawnCfg, @childHandle, nil, @lastError, nil);
        winpty_spawn_config_free(spawnCfg);
      end;

      if not Result then
      begin
        ClosePipe(phPipeIn^);
        ClosePipe(phPipeOut^);
        winpty_free(phPC^);
      end;
    end;
  end;
end;

var
  libwinpty: HINST;

function InitializeOld: Boolean;
begin
  libwinpty:= LoadLibrary('winpty.dll');
  Result:= (libwinpty <> 0);
  if Result then
  begin
    winpty_config_new:= GetProcAddress(libwinpty, 'winpty_config_new');
    winpty_config_free:= GetProcAddress(libwinpty, 'winpty_config_free');

    winpty_config_set_initial_size:= GetProcAddress(libwinpty, 'winpty_config_set_initial_size');
    winpty_config_set_mouse_mode:= GetProcAddress(libwinpty, 'winpty_config_set_mouse_mode');

    winpty_open:= GetProcAddress(libwinpty, 'winpty_open');
    winpty_free:= GetProcAddress(libwinpty, 'winpty_free');

    winpty_error_code:= GetProcAddress(libwinpty, 'winpty_error_code');
    winpty_error_msg:= GetProcAddress(libwinpty, 'winpty_error_msg');
    winpty_error_free:= GetProcAddress(libwinpty, 'winpty_error_free');

    winpty_spawn_config_new:= GetProcAddress(libwinpty, 'winpty_spawn_config_new');
    winpty_spawn_config_free:= GetProcAddress(libwinpty, 'winpty_spawn_config_free');

    winpty_spawn:= GetProcAddress(libwinpty, 'winpty_spawn');
    winpty_set_size:= GetProcAddress(libwinpty, 'winpty_set_size');

    winpty_conin_name:= GetProcAddress(libwinpty, 'winpty_conin_name');
    winpty_conout_name:= GetProcAddress(libwinpty, 'winpty_conout_name');
    winpty_conerr_name:= GetProcAddress(libwinpty, 'winpty_conerr_name');
  end;
end;

{ TPtyDevice }

procedure TPtyDevice.SetConnected(AValue: Boolean);
var
  AShell: String;
begin
  if FConnected = AValue then Exit;
  FConnected:= AValue;

  if FConnected then
  begin
    AShell:= mbGetEnvironmentVariable('ComSpec');
    if Length(AShell) = 0 then AShell:= 'cmd.exe';
    FConnected:= CreatePseudoConsole(AShell);
    if FConnected then
    begin
      FThread:= TThread.ExecuteInThread(ReadThread);
    end;
  end
  else begin
    DestroyPseudoConsole;
  end;
end;

procedure TPtyDevice.ReadySync;
begin
  if Assigned(FOnRxBuf) then
    FOnRxBuf(Self, FBuffer, FLength);
end;

procedure TPtyDevice.ReadThread;
begin
  while FConnected do
  begin
    FLength:= FileRead(FPipeIn, FBuffer, SizeOf(FBuffer));

    if (FLength > 0) then
    begin
      TThread.Synchronize(nil, ReadySync);
    end;
  end;
end;

procedure TPtyDevice.DestroyPseudoConsole;
begin
  case ConsoleType of
    ctNative:  ClosePseudoConsole(FPty);
    ctEmulate: winpty_free(FPty);
  end;
  FPty:= nil;
  ClosePipe(FPipeIn);
  ClosePipe(FPipeOut);
end;

function TPtyDevice.CreatePseudoConsole(const ACommand: String): Boolean;
begin
  case ConsoleType of
    ctNative:  Result:= CreatePseudoConsoleNew(ACommand, @FPty, @FPipeIn, @FPipeOut, FSize);
    ctEmulate: Result:= CreatePseudoConsoleOld(ACommand, @FPty, @FPipeIn, @FPipeOut, FSize);
    ctNone:    Result:= False;
  end;
end;

constructor TPtyDevice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSize.X:= 80;
  FSize.Y:= 25;
  FPipeIn:= INVALID_HANDLE_VALUE;
  FPipeOut:= INVALID_HANDLE_VALUE;
end;

destructor TPtyDevice.Destroy;
begin
  inherited Destroy;
  SetConnected(False);
end;

function TPtyDevice.SetCurrentDir(const Path: String): Boolean;
begin
  Result:= WriteStr('cd /D "' + Path + '"' + #13#10) > 0;
end;

function TPtyDevice.WriteStr(const Str: string): Integer;
begin
  Result:= FileWrite(FPipeOut, Pointer(Str)^, Length(Str));
end;

function TPtyDevice.SetScreenSize(aCols, aRows: Integer): Boolean;
var
  ASize: TCoord;
begin
  if (FPty = nil) then Exit(False);

  if (ConsoleType = ctEmulate) then
  begin
    Result:= winpty_set_size(FPty, aCols, aRows, nil);
  end
  else if (ConsoleType = ctNative) then
  begin
    ASize.Y:= aRows;
    ASize.X:= aCols;
    Result:= Succeeded(ResizePseudoConsole(FPty, ASize));
  end;
  if Result then
  begin
    FSize.Y:= aRows;
    FSize.X:= aCols;
  end;
end;

procedure Initialize;
begin
  if InitializeNew then
    ConsoleType:= ctNative
  else if InitializeOld then
    ConsoleType:= ctEmulate;
end;

initialization
  Initialize;

end.

