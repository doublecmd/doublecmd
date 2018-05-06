{
   Double commander
   -------------------------------------------------------------------------
   Wfx plugin for working with File Transfer Protocol

   Copyright (C) 2013-2018 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit ScpSend;

{$mode delphi}
{$pointermath on}

interface

uses
  Classes, SysUtils, WfxPlugin, FtpAdv, libssh;

type

  { TScpSend }

  TScpSend = class(TFTPSendEx)
  private
    FChannel: PLIBSSH2_CHANNEL;
  private
    function OpenChannel: Boolean;
    function CloseChannel(Channel: PLIBSSH2_CHANNEL): Boolean;
    function SendCommand(const Command: String): Boolean; overload;
    function SendCommand(const Command: String; out Answer: String): Boolean; overload;
  private
    FAnswer: String;
  protected
    FCurrentDir: String;
    FLastError: Integer;
    FSavedPassword: Boolean;
    FSession: PLIBSSH2_SESSION;
    SourceName, TargetName: PWideChar;
    procedure DoProgress(Percent: Int64);
  protected
    function Connect: Boolean; override;
  public
    constructor Create(const Encoding: String); override;
    function Login: Boolean; override;
    function Logout: Boolean; override;
    function GetCurrentDir: String; override;
    function FileSize(const FileName: String): Int64; override;
    function CreateDir(const Directory: string): Boolean; override;
    function DeleteDir(const Directory: string): Boolean; override;
    function DeleteFile(const FileName: string): Boolean; override;
    function ExecuteCommand(const Command: String): Boolean; override;
    function ChangeWorkingDir(const Directory: string): Boolean; override;
    function RenameFile(const OldName, NewName: string): Boolean; override;
    function ChangeMode(const FileName, Mode: String): Boolean; override;
    function StoreFile(const FileName: string; Restore: Boolean): Boolean; override;
    function RetrieveFile(const FileName: string; FileSize: Int64; Restore: Boolean): Boolean; override;
  public
    function DataRead(const DestStream: TStream): Boolean; override;
  public
    function List(Directory: String; NameList: Boolean): Boolean; override;
    function FsSetTime(const FileName: String; LastAccessTime, LastWriteTime: PFileTime): BOOL; override;
  end;

implementation

uses
  CTypes, LazUTF8, FtpFunc, DCStrUtils, DCClassesUtf8, DCOSUtils;

const
  SMB_BUFFER_SIZE = 131072;

function EscapeNoQuotes(const Str: String): String;
begin
  Result:= Str;
end;

procedure userauth_kbdint(const name: PAnsiChar; name_len: cint;
                          const instruction: PAnsiChar; instruction_len: cint;
                          num_prompts: cint; const prompts: PLIBSSH2_USERAUTH_KBDINT_PROMPT;
                          responses: PLIBSSH2_USERAUTH_KBDINT_RESPONSE; abstract: PPointer); cdecl;
var
  S: String;
  I: Integer;
  Sender: TScpSend;
  Title, Message, Password: UnicodeString;
begin
  Sender:= TScpSend(abstract^);
  for I:= 0 to num_prompts - 1 do
  begin
    if (I = 0) and (Length(Sender.FPassword) > 0) and (not Sender.FSavedPassword) then
    begin
      Sender.FSavedPassword:= True;
      responses^.text:= GetMem(Length(Sender.FPassword) + 1);
      StrCopy(responses^.text, PAnsiChar(Sender.FPassword));
      responses^.length:= Length(Sender.FPassword);
    end
    else begin
      Title:= EmptyWideStr;
      Message:= EmptyWideStr;
      if Assigned(instruction) and (instruction_len > 0) then
      begin
        SetString(S, instruction, instruction_len);
        Message:= Sender.ServerToClient(S) + LineEnding;
      end;
      if Assigned(prompts[I].text) and (prompts[I].length > 0) then
      begin
        SetString(S, prompts[I].text, prompts[I].length);
        Message+= Sender.ServerToClient(S);
      end;
      if Assigned(name) and (name_len > 0) then
      begin
        SetString(S, name, name_len);
        Title:= Sender.ServerToClient(S) + #32;
      end;
      SetLength(Password, MAX_PATH + 1);
      Title+= 'sftp://' + UTF8ToUTF16(Sender.UserName + '@' + Sender.TargetHost);
      if not RequestProc(PluginNumber, RT_Password, PWideChar(Title), PWideChar(Message), PWideChar(Password), MAX_PATH) then
      begin
        responses[I].text:= nil;
        responses[I].length:= 0;
      end
      else begin
        Sender.FPassword:= Sender.ClientToServer(Password);
        responses[I].text:= GetMem(Length(Sender.FPassword) + 1);
        StrCopy(responses[I].text, PAnsiChar(Sender.FPassword));
        responses[I].length:= Length(Sender.FPassword);
      end;
    end;
  end;
end;

{ TScpSend }

function TScpSend.OpenChannel: Boolean;
begin
  repeat
    FChannel := libssh2_channel_open_session(FSession);
    if Assigned(FChannel) then
      libssh2_channel_set_blocking(FChannel, 0)
    else begin
      FLastError:= libssh2_session_last_errno(FSession);
      if (FLastError <> LIBSSH2_ERROR_EAGAIN) then Exit(False);
    end;
  until not ((FChannel = nil) and (FLastError = LIBSSH2_ERROR_EAGAIN));
  Result:= Assigned(FChannel);
end;

function TScpSend.CloseChannel(Channel: PLIBSSH2_CHANNEL): Boolean;
begin
  repeat
    FLastError:= libssh2_channel_free(Channel);
  until (FLastError <> LIBSSH2_ERROR_EAGAIN);
  Result:= (FLastError = 0);
end;

function TScpSend.SendCommand(const Command: String): Boolean;
var
  Ret: Int32;
begin
  repeat
    FLastError := libssh2_channel_exec(FChannel, PAnsiChar(Command));
  until (FLastError <> LIBSSH2_ERROR_EAGAIN);

  while (libssh2_channel_flush(FChannel) = LIBSSH2_ERROR_EAGAIN) do;

  while (libssh2_channel_send_eof(FChannel) = LIBSSH2_ERROR_EAGAIN) do;

  Result:= (FLastError >= 0);
end;

function TScpSend.SendCommand(const Command: String;
  out Answer: String): Boolean;
const
  BUFFER_SIZE = 4096;
var
  Ret: cint;
begin
  Result:= OpenChannel;
  if Result then
  begin
    Result:= SendCommand(Command);
    if Result then
    begin
      SetLength(Answer, BUFFER_SIZE + 1);
      while libssh2_channel_eof(FChannel) = 0 do
      begin
        if libssh2_channel_read_stderr(FChannel, Pointer(Answer), BUFFER_SIZE) > 0 then
          Result:= False;
        Ret:= libssh2_channel_read(FChannel, Pointer(Answer), BUFFER_SIZE);
        if (Ret > 0) then begin
          SetLength(Answer, Ret);
          Answer:= TrimRightLineEnding(Answer, tlbsLF);
        end;
      end;
      Result:=  Result and (libssh2_channel_get_exit_status(FChannel) = 0);
    end;
    CloseChannel(FChannel);
  end;
end;

procedure TScpSend.DoProgress(Percent: Int64);
begin
  if ProgressProc(PluginNumber, SourceName, TargetName, Percent) = 1 then
    raise EUserAbort.Create(EmptyStr);
end;

function TScpSend.Connect: Boolean;
const
  HOSTKEY_SIZE = 20;
var
  S: String;
  I: Integer;
  userauthlist: PAnsiChar;
  FingerPrint: array [0..Pred(HOSTKEY_SIZE)] of AnsiChar;
begin
  FSock.CloseSocket;
  DoStatus(False, 'Connecting to: ' + FTargetHost);
  FSock.Connect(FTargetHost, FTargetPort);
  Result:= (FSock.LastError = 0);
  if Result then
  begin
    FSession := libssh2_session_init(Self);
    if not Assigned(FSession) then Exit(False);
    try
      libssh2_session_set_timeout(FSession, FTimeout);

      //* Since we have not set non-blocking, tell libssh2 we are blocking */
      libssh2_session_set_blocking(FSession, 1);

      if libssh2_session_handshake(FSession, FSock.Socket) <> 0 then
      begin
        DoStatus(False, 'Cannot establishing SSH session');
        Exit(False);
      end;
      DoStatus(False, 'Connection established');
      FingerPrint := libssh2_hostkey_hash(FSession, LIBSSH2_HOSTKEY_HASH_SHA1);
      S:= 'Server fingerprint:';
      for I:= Low(FingerPrint) to High(FingerPrint) do
      begin
        S:= S + #32 + IntToHex(Ord(FingerPrint[i]), 2);
      end;
      DoStatus(False, S);

      //* check what authentication methods are available */
      userauthlist := libssh2_userauth_list(FSession, PAnsiChar(FUserName), Length(FUserName));

      if (strpos(userauthlist, 'password') <> nil) then
      begin
        I:= libssh2_userauth_password(FSession, PAnsiChar(FUserName), PAnsiChar(FPassword));
        if I <> 0 then begin
          DoStatus(False, 'Authentication by password failed');
          Exit(False);
        end;
      end
      else if (strpos(userauthlist, 'keyboard-interactive') <> nil) then
      begin
        FSavedPassword:= False;
        libssh2_session_set_timeout(FSession, 0);
        I:= libssh2_userauth_keyboard_interactive(FSession, PAnsiChar(FUserName), @userauth_kbdint);
        if I <> 0 then begin
          DoStatus(False, 'Authentication by keyboard-interactive failed');
          Exit(False);
        end;
        libssh2_session_set_timeout(FSession, FTimeout);
      end
      else if (strpos(userauthlist, 'publickey') <> nil) then
      begin
        DoStatus(False, 'Authentication by publickey is not supported!');
        Exit(False);
      end;

      DoStatus(False, 'Authentication succeeded');
    finally
      if not Result then begin
        libssh2_session_free(FSession);
        FSock.CloseSocket;
      end;
    end;
  end;
end;

constructor TScpSend.Create(const Encoding: String);
begin
  FCurrentDir:= '/';
  inherited Create(Encoding);
  FTargetPort:= '22';
end;

function TScpSend.Login: Boolean;
begin
  Result:= Connect;
end;

function TScpSend.Logout: Boolean;
begin
  Result:= libssh2_session_disconnect(FSession, 'Logout') = 0;
  libssh2_session_free(FSession);
  FSock.CloseSocket;
end;

function TScpSend.GetCurrentDir: String;
begin
  Result:= FCurrentDir;
  {
  if not OpenChannel then
    Result:= EmptyStr
  else begin
    if not SendCommand('pwd', Result) then
      Result:= EmptyStr;
    CloseChannel;
  end;
  }
end;

function TScpSend.FileSize(const FileName: String): Int64;
begin
  Result:= 0;
end;

function TScpSend.CreateDir(const Directory: string): Boolean;
begin
  Result:= SendCommand('mkdir ' + EscapeNoQuotes(Directory), FAnswer);
end;

function TScpSend.DeleteDir(const Directory: string): Boolean;
begin
  Result:= SendCommand('rmdir ' + EscapeNoQuotes(Directory), FAnswer);
end;

function TScpSend.DeleteFile(const FileName: string): Boolean;
begin
  Result:= SendCommand('rm -f ' + EscapeNoQuotes(FileName), FAnswer);
end;

function TScpSend.ExecuteCommand(const Command: String): Boolean;
begin
  Result:= SendCommand(Command, FAnswer);
end;

function TScpSend.ChangeWorkingDir(const Directory: string): Boolean;
begin
  Result:= SendCommand('cd ' + EscapeNoQuotes(Directory), FAnswer);
  if Result then FCurrentDir:= Directory;
end;

function TScpSend.RenameFile(const OldName, NewName: string): Boolean;
begin
  Result:= SendCommand('mv ' + EscapeNoQuotes(OldName) + ' ' + EscapeNoQuotes(NewName), FAnswer);
end;

function TScpSend.ChangeMode(const FileName, Mode: String): Boolean;
begin
  Result:= SendCommand('chmod ' + Mode + ' ' + EscapeNoQuotes(FileName), FAnswer);
end;

function TScpSend.StoreFile(const FileName: string; Restore: Boolean): Boolean;
var
  Index: PtrInt;
  FBuffer: PByte;
  FileSize: Int64;
  BytesRead: Integer;
  BytesToRead: Integer;
  BytesWritten: PtrInt;
  BytesToWrite: Integer;
  SendStream: TFileStreamEx;
  TotalBytesToWrite: Int64 = 0;
  TargetHandle: PLIBSSH2_CHANNEL = nil;
  Flags: cint = LIBSSH2_FXF_CREAT or LIBSSH2_FXF_WRITE;
begin
  SendStream := TFileStreamEx.Create(FDirectFileName, fmOpenRead or fmShareDenyWrite);

  TargetName:= PWideChar(ServerToClient(FileName));
  SourceName:= PWideChar(UTF8Decode(FDirectFileName));

  FileSize:= SendStream.Size;
  FBuffer:= GetMem(SMB_BUFFER_SIZE);
  libssh2_session_set_blocking(FSession, 0);
  try
    if not Restore then
    begin
      TotalBytesToWrite:= FileSize;
      Flags:= Flags or LIBSSH2_FXF_TRUNC
    end
    else begin
      TotalBytesToWrite:= Self.FileSize(FileName);
      if (FileSize = TotalBytesToWrite) then Exit(True);
      if TotalBytesToWrite < 0 then TotalBytesToWrite:= 0;
      SendStream.Seek(TotalBytesToWrite, soBeginning);
      TotalBytesToWrite := FileSize - TotalBytesToWrite;
      Flags:= Flags or LIBSSH2_FXF_APPEND;
    end;

    // Open remote file
    repeat
      TargetHandle:= libssh2_scp_send64(FSession,
                                         PAnsiChar(FileName),
                                         $1A0, FileSize, 0, 0);
      if (TargetHandle = nil) then
      begin
        FLastError:= libssh2_session_last_errno(FSession);
        if (FLastError <> LIBSSH2_ERROR_EAGAIN) then Exit(False);
        DoProgress((FileSize - TotalBytesToWrite) * 100 div FileSize);
        FSock.CanRead(10);
      end;
    until not ((TargetHandle = nil) and (FLastError = LIBSSH2_ERROR_EAGAIN));

    BytesToRead:= SMB_BUFFER_SIZE;
    while (TotalBytesToWrite > 0) do
    begin
      if (BytesToRead > TotalBytesToWrite) then begin
        BytesToRead:= TotalBytesToWrite;
      end;
      BytesRead:= SendStream.Read(FBuffer^, BytesToRead);
      if (BytesRead = 0) then Exit(False);
      // Start write operation
      Index:= 0;
      BytesToWrite:= BytesRead;
      while (BytesToWrite > 0) do
      begin
        repeat
          BytesWritten:= libssh2_channel_write(TargetHandle, PAnsiChar(FBuffer + Index), BytesToWrite);
          if BytesWritten = LIBSSH2_ERROR_EAGAIN then begin
            DoProgress((FileSize - TotalBytesToWrite) * 100 div FileSize);
            FSock.CanRead(10);
          end;
        until BytesWritten <> LIBSSH2_ERROR_EAGAIN;
        if (BytesWritten < 0) then Exit(False);
        Dec(TotalBytesToWrite, BytesWritten);
        Dec(BytesToWrite, BytesWritten);
        Inc(Index, BytesWritten);
      end;
      DoProgress((FileSize - TotalBytesToWrite) * 100 div FileSize);
    end;
    Result:= True;
  finally
    SendStream.Free;
    FreeMem(FBuffer);
    Result:= CloseChannel(TargetHandle) and Result;
    libssh2_session_set_blocking(FSession, 1);
  end;
end;

function TScpSend.RetrieveFile(const FileName: string; FileSize: Int64;
  Restore: Boolean): Boolean;
var
  FBuffer: PByte;
  BytesRead: PtrInt;
  RetrStream: TFileStreamEx;
  TotalBytesToRead: Int64 = 0;
  SourceHandle: PLIBSSH2_CHANNEL;
begin
  if Restore and mbFileExists(FDirectFileName) then
    RetrStream := TFileStreamEx.Create(FDirectFileName, fmOpenWrite or fmShareExclusive)
  else begin
    RetrStream := TFileStreamEx.Create(FDirectFileName, fmCreate or fmShareDenyWrite)
  end;

  SourceName := PWideChar(ServerToClient(FileName));
  TargetName := PWideChar(UTF8Decode(FDirectFileName));

  if Restore then TotalBytesToRead:= RetrStream.Seek(0, soEnd);

  libssh2_session_set_blocking(FSession, 0);
  try
    repeat
      SourceHandle:= libssh2_scp_recv2(FSession,
                                       PAnsiChar(FileName),
                                       nil);
      if (SourceHandle = nil) then
      begin
        FLastError:= libssh2_session_last_errno(FSession);
        if (FLastError <> LIBSSH2_ERROR_EAGAIN) then Exit(False);
        DoProgress((FileSize - TotalBytesToRead) * 100 div FileSize);
        FSock.CanRead(10);
      end;
    until not ((SourceHandle = nil) and (FLastError = LIBSSH2_ERROR_EAGAIN));

    if Restore then begin
      //libssh2_sftp_seek64(SourceHandle, TotalBytesToRead);
    end;

    FBuffer:= GetMem(SMB_BUFFER_SIZE);
    TotalBytesToRead:= FileSize - TotalBytesToRead;
    try
      while TotalBytesToRead > 0 do
      begin
        repeat
          BytesRead := libssh2_channel_read(SourceHandle, PAnsiChar(FBuffer), SMB_BUFFER_SIZE);
          if BytesRead = LIBSSH2_ERROR_EAGAIN then begin
            DoProgress((FileSize - TotalBytesToRead) * 100 div FileSize);
            FSock.CanRead(10);
          end;
        until BytesRead <> LIBSSH2_ERROR_EAGAIN;

        if (BytesRead < 0) then Exit(False);

        if RetrStream.Write(FBuffer^, BytesRead) <> BytesRead then
          Exit(False);

        Dec(TotalBytesToRead, BytesRead);
        DoProgress((FileSize - TotalBytesToRead) * 100 div FileSize);
      end;
      Result:= True;
    finally
      RetrStream.Free;
      FreeMem(FBuffer);
      Result:= CloseChannel(SourceHandle) and Result;
    end;
  finally
    libssh2_session_set_blocking(FSession, 1);
  end;
end;

function TScpSend.DataRead(const DestStream: TStream): Boolean;
var
  Ret, ERet: cint;
  ABuffer: array[Byte] of AnsiChar;
  AEBuffer: array[Byte] of AnsiChar;
begin
  repeat
    if libssh2_channel_eof(FChannel) <> 0 then Exit(False);
    Ret:= libssh2_channel_read(FChannel, ABuffer, 256);
    ERet:= libssh2_channel_read_stderr(FChannel, AEBuffer, 256);
    if (ERet > 0) then begin
      DoStatus(True, AEBuffer);
    end;
    if Ret > 0 then DestStream.Write(ABuffer, Ret);
  until not ((Ret > 0) or (Ret = LIBSSH2_ERROR_EAGAIN));
  Result:= DestStream.Position > 0;
end;

function TScpSend.List(Directory: String; NameList: Boolean): Boolean;
begin
  FFTPList.Clear;
  FDataStream.Clear;
  Result:= OpenChannel;
  if Result then
  begin
    if Directory <> '' then begin
      Directory := ' ' + EscapeNoQuotes(Directory);
    end;
    Result:= SendCommand('ls -la' + Directory);
    if Result then
    begin
      Result:= DataRead(FDataStream);
      if Result then
      begin
        FDataStream.Position := 0;
        FFTPList.Lines.LoadFromStream(FDataStream);
        FFTPList.ParseLines;
      end;
      FDataStream.Position := 0;
    end;
    CloseChannel(FChannel);
  end;
end;

function TScpSend.FsSetTime(const FileName: String; LastAccessTime,
  LastWriteTime: PFileTime): BOOL;
var
  DateTime: String;
  FileTime: TDateTime;
begin
  if (LastWriteTime = nil) then Exit(False);
  FileTime:= (Int64(LastWriteTime^) / 864000000000.0) - 109205.0;
  DateTime:= FormatDateTime('yyyymmddhhnn.ss', FileTime);
  Result:= SendCommand('touch -t ' + DateTime + ' ' + EscapeNoQuotes(FileName), FAnswer);
end;

end.

