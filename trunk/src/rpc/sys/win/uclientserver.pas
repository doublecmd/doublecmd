{
   Double commander
   -------------------------------------------------------------------------
   Remote procedure call implementation (Windows)

   Copyright (C) 2019 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uClientServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uService;

type

  { TPipeTransport }

  TPipeTransport = class(TBaseTransport)
  private
    FPipe: THandle;
    FProcessID: UInt32;
    FAddress: UnicodeString;
  private
    procedure Connect;
  public
    procedure Disconnect; override;
    procedure WriteHandle(AHandle: THandle); override;
    function ReadHandle(var AHandle: THandle) : Int64; override;
    procedure WriteBuffer(const AData; const ALength : Int64); override;
    function ReadBuffer(var AData; const ALength : Int64) : Int64; override;
  public
    constructor Create(APipe: THandle; ProcessID: UInt32);
    constructor Create(Address: String);
    destructor Destroy; override;
  end;

  { TClientHandlerThread }

  TClientHandlerThread = class(TClientThread)
  public
    constructor Create(APipe : THandle; AOwner : TBaseService);
  end;
  
  { TServerListnerThread }

  TServerListnerThread = class(TServerThread)
  public
    procedure Execute; override;
  end;

implementation

uses 
  JwaWinBase, Windows, uNamedPipes;

{ TPipeTransport }

procedure TPipeTransport.Connect;
begin
  if (FPipe = 0) then
  begin
    WriteLn('Connect to ', FAddress,'_');

    FPipe:= CreateFileW(PWideChar('\\.\pipe\' + FAddress),
      GENERIC_WRITE or
      GENERIC_READ,
      FILE_SHARE_READ or
      FILE_SHARE_WRITE,
      nil,
      OPEN_EXISTING,
      FILE_FLAG_OVERLAPPED,
      0);

    if FPipe = INVALID_HANDLE_VALUE then RaiseLastOSError;
  end;
end;

procedure TPipeTransport.Disconnect;
begin
  CloseHandle(FPipe);
  FPipe:= 0;
end;

procedure TPipeTransport.WriteHandle(AHandle: THandle);
var
  hProcess: THandle;
  hDuplicate: THandle;
begin
  hProcess:= OpenProcess(PROCESS_DUP_HANDLE, False, FProcessID);
  if (hProcess <> 0) then
  begin
    if DuplicateHandle(GetCurrentProcess(), AHandle, hProcess, @hDuplicate, 0, False, DUPLICATE_SAME_ACCESS or DUPLICATE_CLOSE_SOURCE) then
      WriteBuffer(hDuplicate, SizeOf(hDuplicate));
    CloseHandle(hProcess);
  end;
end;

function TPipeTransport.ReadHandle(var AHandle: THandle): Int64;
begin
  Result:= ReadBuffer(AHandle, SizeOf(AHandle));
end;

procedure TPipeTransport.WriteBuffer(const AData; const ALength : Int64);
var
  P: PByte;
  Len: Int64;
  LastError: DWORD;
  Overlapped: TOverlapped;
  dwNumberOfBytesTransferred: DWORD;
begin
  Connect;
  Len := ALength;
  P := PByte(@AData);
  Repeat
    LastError := 0;
    dwNumberOfBytesTransferred:= 0;
    ZeroMemory(@Overlapped, SizeOf(TOverlapped));

    if not WriteFile(FPipe, P^, Len, dwNumberOfBytesTransferred, @Overlapped) then
      LastError := GetLastError;

    if (LastError <> 0) and (LastError <> ERROR_IO_PENDING) then
      raise EInOutError.Create(SysErrorMessage(LastError));

    LastError := WaitForSingleObject(FPipe, INFINITE);

    if LastError = WAIT_TIMEOUT then
    begin
      raise EInOutError.Create(SysErrorMessage(LastError));
    end;

    if not GetOverlappedResult(FPipe, Overlapped, dwNumberOfBytesTransferred, False) then
      raise EInOutError.Create(SysErrorMessage(GetLastError));

    if (dwNumberOfBytesTransferred > 0) then
    begin
      Inc(P, dwNumberOfBytesTransferred);
      Dec(Len, dwNumberOfBytesTransferred);
    end;
  until (Len = 0);
end;

function TPipeTransport.ReadBuffer(var AData; const ALength : Int64) : Int64;
Var
  P : PByte;
  Len : Int64;
  LastError: DWORD;
  Overlapped: TOverlapped;
  dwNumberOfBytesTransferred: DWORD;
begin
  Len := ALength;
  P := PByte(@AData);
  repeat
    LastError := 0;
    dwNumberOfBytesTransferred:= 0;
    ZeroMemory(@Overlapped, SizeOf(TOverlapped));

    if not ReadFile(FPipe, P^, Len, dwNumberOfBytesTransferred, @Overlapped) then
      LastError := GetLastError;

    if (LastError <> 0) and (LastError <> ERROR_IO_PENDING) then
      raise EInOutError.Create(SysErrorMessage(LastError));

    LastError := WaitForSingleObject(FPipe, INFINITE);

    if LastError = WAIT_TIMEOUT then
      raise EInOutError.Create(SysErrorMessage(LastError));

    if not GetOverlappedResult(FPipe, Overlapped, dwNumberOfBytesTransferred, False) then
      raise EInOutError.Create(SysErrorMessage(GetLastError));

    if (dwNumberOfBytesTransferred = 0) then
      raise EInOutError.Create(EmptyStr);

    if (dwNumberOfBytesTransferred > 0) then
    begin
      Inc(P, dwNumberOfBytesTransferred);
      Dec(Len, dwNumberOfBytesTransferred);
    end
  until (Len = 0);
  Result := ALength;
end;

constructor TPipeTransport.Create(APipe: THandle; ProcessID: UInt32);
begin
  FPipe:= APipe;
  FProcessID:= ProcessID;
end;

constructor TPipeTransport.Create(Address: String);
begin
  FAddress:= UTF8Decode(Address);
end;

destructor TPipeTransport.Destroy;
begin
  CloseHandle(FPipe);
  inherited Destroy;
end;

{ TClientHandlerThread }

constructor TClientHandlerThread.Create(APipe: THandle; AOwner: TBaseService);
begin
  FOwner := AOwner;
  FreeOnTerminate := True;
  WriteLn('Connected success');
  FTransport:= TPipeTransport.Create(APipe, FOwner.ProcessID);
  inherited Create(False);
end;

{ TServerListnerThread }

procedure TServerListnerThread.Execute;
const
  IN_BUF_SIZE = maxSmallint;
  OUT_BUF_SIZE = maxSmallint;
var
  hPipe: THandle;
  AName: UnicodeString;
  SA: SECURITY_ATTRIBUTES;
  SD: SECURITY_DESCRIPTOR;
begin
  AName:= UTF8Decode(FOwner.Name);

  if not InitializeSecurityDescriptor (@SD, SECURITY_DESCRIPTOR_REVISION) then
    Halt;
  if not SetSecurityDescriptorDacl(@SD, True, nil, False) then
    Halt;

  while not Terminated do
  begin
    SA.nLength := SizeOf(SA);
    SA.lpSecurityDescriptor := @SD;
    SA.bInheritHandle := True;

    // Create pipe server
    hPipe := CreateNamedPipeW(PWideChar('\\.\pipe\' + AName),
      PIPE_ACCESS_DUPLEX or
      FILE_FLAG_OVERLAPPED,
      PIPE_WAIT or
      PIPE_READMODE_BYTE or
      PIPE_TYPE_BYTE,
      PIPE_UNLIMITED_INSTANCES,
      OUT_BUF_SIZE,
      IN_BUF_SIZE,
      0,
      @SA);

    if hPipe = INVALID_HANDLE_VALUE then
      Halt;

    WriteLn('Start server ', AName);

    // Wait client connection
    if not (ConnectNamedPipe(hPipe, nil) or (GetLastError() = ERROR_PIPE_CONNECTED)) then
      CloseHandle(hPipe)
    else begin
      if (FOwner.VerifyChild and not VerifyChild(hPipe)) or
         (FOwner.VerifyParent and not VerifyParent(hPipe)) then
      begin
        DisconnectNamedPipe(hPipe);
        CloseHandle(hPipe);
        Continue;
      end;

      if not Terminated then
        TClientHandlerThread.Create(hPipe, FOwner)
      else begin
        DisconnectNamedPipe(hPipe);
        CloseHandle(hPipe);
      end;
    end;
  end;
end;

end.

