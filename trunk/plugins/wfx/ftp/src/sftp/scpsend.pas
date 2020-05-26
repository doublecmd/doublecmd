{
   Double commander
   -------------------------------------------------------------------------
   Wfx plugin for working with File Transfer Protocol

   Copyright (C) 2013-2019 Alexander Koblov (alexx2000@mail.ru)

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
    FAutoDetect: Boolean;
    FListCommand: String;
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
    FFingerprint: AnsiString;
    FSession: PLIBSSH2_SESSION;
    SourceName, TargetName: PWideChar;
    procedure DoProgress(Percent: Int64);
  protected
    procedure DetectEncoding;
    function AuthKey: Boolean;
    function Connect: Boolean; override;
  public
    constructor Create(const Encoding: String); override;
    function Login: Boolean; override;
    function Logout: Boolean; override;
    function GetCurrentDir: String; override;
    procedure CloneTo(AValue: TFTPSendEx); override;
    function FileSize(const FileName: String): Int64; override;
    function FileExists(const FileName: String): Boolean; override;
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
    function FsSetTime(const FileName: String; LastAccessTime, LastWriteTime: PWfxFileTime): BOOL; override;
  public
    property Fingerprint: AnsiString read FFingerprint write FFingerprint;
  end;

implementation

uses
  CTypes, LazUTF8, FtpFunc, DCStrUtils, DCClassesUtf8, DCOSUtils, DCDateTimeUtils,
  DCBasicTypes, DCConvertEncoding, FileUtil, Base64, LConvEncoding, SynaCode, StrUtils;

const
  SMB_BUFFER_SIZE = 131072;
  LIST_TIME_STYLE = ' --time-style=+%Y.%m.%d-%H:%M:%S';
  LIST_LOCALE_C   = 'export LC_TIME=C' + #10 + 'export LC_MESSAGES=C' + #10;

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
      Title+= 'ssh://' + UTF8ToUTF16(Sender.UserName + '@' + Sender.TargetHost);
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
    if not Assigned(FChannel) then
    begin
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

procedure TScpSend.DetectEncoding;
begin
  if SendCommand('echo $LANG $LC_CTYPE $LC_ALL', FAnswer) then
  begin
    FAuto:= False;
    if Pos('UTF-8', FAnswer) > 0 then
    begin
      Encoding:= EncodingUTF8;
    end;
  end;
end;

function TScpSend.AuthKey: Boolean;
const
  Alphabet = ['a'..'z','A'..'Z','0'..'9','+','/','=', #10, #13];
var
  Index: Integer;
  Memory: PAnsiChar;
  PrivateStream: String;
  Encrypted: Boolean = False;
  Passphrase: AnsiString = '';
  Title, Message, Password: UnicodeString;
begin
  PrivateStream:= ReadFileToString(FPrivateKey);
  // Check private key format
  Index:= Pos(#10, PrivateStream);
  if Index = 0 then Index:= Pos(#13, PrivateStream);
  if Index > 0 then begin
    // Skip first line and empty lines
    Memory:= Pointer(@PrivateStream[Index]) + 1;
    while Memory^ in [#10, #13] do Inc(Memory);
    // Check old private key format
    for Index:= 0 to 31 do
    begin
      if (not (Memory[Index] in Alphabet)) then
      begin
        Encrypted:= True;
        Break;
      end;
    end;
    // Check new OpenSSH private key format
    if not Encrypted then
    begin
      if Pos('-----BEGIN OPENSSH PRIVATE KEY-----', PrivateStream) > 0 then
      begin
        Passphrase:= DecodeStringBase64(Memory);
        Index:= Pos('bcrypt', Passphrase);
        Encrypted:= (Index > 0) and (Index <= 64);
      end;
    end;
  end;
  // Private key encrypted, request pass phrase
  if Encrypted then
  begin
    SetLength(Password, MAX_PATH + 1);
    Message:= 'Private key pass phrase:';
    Title:= 'ssh://' + UTF8ToUTF16(FUserName + '@' + FTargetHost);
    if RequestProc(PluginNumber, RT_Password, PWideChar(Title), PWideChar(Message), PWideChar(Password), MAX_PATH) then
    begin
      Passphrase:= ClientToServer(Password);
    end;
  end;
  Result:= libssh2_userauth_publickey_fromfile(FSession, PAnsiChar(FUserName),
                                               PAnsiChar(CeUtf8ToSys(FPublicKey)),
                                               PAnsiChar(CeUtf8ToSys(FPrivateKey)),
                                               PAnsiChar(Passphrase)) = 0;
end;

function TScpSend.Connect: Boolean;
const
  HASH_SIZE: array[1..3] of Byte = (16, 20, 32);
  HASH_NAME: array[1..3] of String = ('(MD5) ', '(SHA1) ', '(SHA256) ');
var
  S: String;
  F: String = '';
  SS: String = '';
  I, J, Finish: Integer;
  Message: UnicodeString;
  FingerPrint: PAnsiChar;
  userauthlist: PAnsiChar;
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
      LogProc(PluginNumber, MSGTYPE_CONNECT, nil);

      DoStatus(False, 'Connection established');

      if libssh2_version($010900) = nil then
        Finish:= LIBSSH2_HOSTKEY_HASH_SHA1
      else begin
        Finish:= LIBSSH2_HOSTKEY_HASH_SHA256;
      end;

      for J:= LIBSSH2_HOSTKEY_HASH_MD5 to Finish do
      begin
        FingerPrint := libssh2_hostkey_hash(FSession, J);
        if Assigned(FingerPrint) then
        begin
          if (J >= LIBSSH2_HOSTKEY_HASH_SHA256) then
          begin
            SetString(S, FingerPrint, HASH_SIZE[J]);
            S := TrimRightSet(EncodeBase64(S), ['=']);
          end
          else begin
            S:= EmptyStr;
            for I:= 0 to HASH_SIZE[J] - 1 do
            begin
              S+= IntToHex(Ord(FingerPrint[I]), 2) + #32;
            end;
            SetLength(S, Length(S) - 1); // Remove space
          end;
          SS += HASH_NAME[J] + S + LineEnding;
          DoStatus(False, 'Server fingerprint: ' + HASH_NAME[J] + S);
          if (J > LIBSSH2_HOSTKEY_HASH_MD5) and (Length(F) = 0) then F:= S;
        end;
      end;

      // Verify server fingerprint
      if FFingerPrint <> F then
      begin
        if FFingerprint = EmptyStr then
          Message:= 'You are using this connection for the first time.' + LineEnding + 'Please verify that the following host fingerprint matches the fingerprint of your server:'
        else begin
          Message:= 'WARNING!' + LineEnding + 'The fingerprint of the host has changed!' + LineEnding + 'Please make sure that the new fingerprint matches your server:';
        end;
        Message += UnicodeString(LineEnding + LineEnding + SS);
        if not RequestProc(PluginNumber, RT_MsgYesNo, nil, PWideChar(Message), nil, 0) then
        begin
          LogProc(PluginNumber, msgtype_importanterror, 'Wrong server fingerprint!');
          Exit(False);
        end;
        FFingerprint:= F;
      end;

      //* check what authentication methods are available */
      userauthlist := libssh2_userauth_list(FSession, PAnsiChar(FUserName), Length(FUserName));

      if (strpos(userauthlist, 'publickey') <> nil) and (FPublicKey <> '') and (FPrivateKey <> '') then
      begin
        DoStatus(False, 'Auth via public key for user: ' + FUserName);
        if not AuthKey then begin
          LogProc(PluginNumber, msgtype_importanterror, 'Authentication by publickey failed');
          Exit(False);
        end;
      end
      else if (strpos(userauthlist, 'password') <> nil) then
      begin
        I:= libssh2_userauth_password(FSession, PAnsiChar(FUserName), PAnsiChar(FPassword));
        if I <> 0 then begin
          LogProc(PluginNumber, msgtype_importanterror, 'Authentication by password failed');
          Exit(False);
        end;
      end
      else if (strpos(userauthlist, 'keyboard-interactive') <> nil) then
      begin
        FSavedPassword:= False;
        libssh2_session_set_timeout(FSession, 0);
        I:= libssh2_userauth_keyboard_interactive(FSession, PAnsiChar(FUserName), @userauth_kbdint);
        if I <> 0 then begin
          LogProc(PluginNumber, msgtype_importanterror, 'Authentication by keyboard-interactive failed');
          Exit(False);
        end;
        libssh2_session_set_timeout(FSession, FTimeout);
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
  FListCommand:= 'ls -la';
end;

function TScpSend.Login: Boolean;
var
  ACommand: String;
begin
  Result:= Connect;
  if Result then
  begin
    if not FAutoDetect then
    begin
      FAutoDetect:= True;
      // Try to use custom time style
      ACommand:= LIST_LOCALE_C + FListCommand + LIST_TIME_STYLE;
      if SendCommand(ACommand + ' > /dev/null', FAnswer) then
      begin
        FListCommand:= ACommand;
        FFtpList.Masks.Insert(0, 'pppppppppp $!!!S* YYYY MM DD hh mm ss $n*');
      end
      else begin
        // Try to use 'C' locale
        ACommand:= LIST_LOCALE_C + FListCommand;
        if SendCommand(ACommand + ' > /dev/null', FAnswer) then
        begin
          FListCommand:= ACommand
        end;
      end;
    end;
    if FAuto then DetectEncoding;
  end;
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
end;

procedure TScpSend.CloneTo(AValue: TFTPSendEx);
begin
  inherited CloneTo(AValue);
  TScpSend(AValue).FFingerprint:= FFingerprint;
end;

function TScpSend.FileSize(const FileName: String): Int64;
begin
  Result:= -1;
end;

function TScpSend.FileExists(const FileName: String): Boolean;
begin
  Result:= SendCommand('stat ' + EscapeNoQuotes(FileName), FAnswer);
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
var
  Index: Integer;
  Answer: TStringList;
begin
  FDataStream.Clear;
  Result:= OpenChannel;
  if Result then
  begin
    DoStatus(False, Command);
    Result:= SendCommand('cd ' + EscapeNoQuotes(FCurrentDir) + ' && ' + Command);
    if Result then
    begin
      Result:= DataRead(FDataStream);
      if Result then
      begin
        FDataStream.Position:= 0;
        Answer:= TStringList.Create;
        try
          Answer.LoadFromStream(FDataStream);
          for Index:= 0 to Answer.Count - 1 do
            DoStatus(True, Answer.Strings[Index]);
        finally
          Answer.Free;
        end;
      end;
      FDataStream.Clear;
    end;
    CloseChannel(FChannel);
  end;
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
begin
  SendStream := TFileStreamEx.Create(FDirectFileName, fmOpenRead or fmShareDenyWrite);

  TargetName:= PWideChar(ServerToClient(FileName));
  SourceName:= PWideChar(UTF8Decode(FDirectFileName));

  FileSize:= SendStream.Size;
  FBuffer:= GetMem(SMB_BUFFER_SIZE);
  libssh2_session_set_blocking(FSession, 0);
  try
    TotalBytesToWrite:= FileSize;

    // Open remote file
    repeat
      TargetHandle:= libssh2_scp_send64(FSession,
                                         PAnsiChar(FileName),
                                         $1A0, FileSize, 0, 0);
      if (TargetHandle = nil) then
      begin
        FLastError:= libssh2_session_last_errno(FSession);
        if (FLastError <> LIBSSH2_ERROR_EAGAIN) then Exit(False);
        if (FileSize > 0) then DoProgress((FileSize - TotalBytesToWrite) * 100 div FileSize);
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
    // Close remote file
    repeat
      FLastError:= libssh2_channel_send_eof(TargetHandle);
      DoProgress(100);
      FSock.CanRead(10);
    until FLastError <> LIBSSH2_ERROR_EAGAIN;
    Result:= (FLastError = 0);
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
  BytesToRead: Integer;
  RetrStream: TFileStreamEx;
  TotalBytesToRead: Int64 = 0;
  SourceHandle: PLIBSSH2_CHANNEL;
begin
  RetrStream := TFileStreamEx.Create(FDirectFileName, fmCreate or fmShareDenyWrite);

  SourceName := PWideChar(ServerToClient(FileName));
  TargetName := PWideChar(UTF8Decode(FDirectFileName));

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
        if (FileSize > 0) then DoProgress(TotalBytesToRead * 100 div FileSize);
        FSock.CanRead(10);
      end;
    until not ((SourceHandle = nil) and (FLastError = LIBSSH2_ERROR_EAGAIN));

    FBuffer:= GetMem(SMB_BUFFER_SIZE);
    TotalBytesToRead:= FileSize - TotalBytesToRead;
    try
      BytesToRead:= SMB_BUFFER_SIZE;
      while TotalBytesToRead > 0 do
      begin
        if (BytesToRead > TotalBytesToRead) then begin
          BytesToRead := TotalBytesToRead;
        end;
        repeat
          BytesRead := libssh2_channel_read(SourceHandle, PAnsiChar(FBuffer), BytesToRead);
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
      FreeMem(FBuffer);
      Result:= CloseChannel(SourceHandle) and Result;
    end;
  finally
    RetrStream.Free;
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
    if libssh2_channel_eof(FChannel) <> 0 then Break;
    Ret:= libssh2_channel_read(FChannel, ABuffer, 256);
    ERet:= libssh2_channel_read_stderr(FChannel, AEBuffer, 256);
    if (ERet > 0) then begin
      LogProc(PluginNumber, msgtype_importanterror, PWideChar(ServerToClient(AEBuffer)));
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
    Result:= SendCommand(FListCommand + Directory);
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
  LastWriteTime: PWfxFileTime): BOOL;
var
  DateTime: String;
  FileTime: TDateTime;
begin
  if (LastWriteTime = nil) then Exit(False);
  FileTime:= WinFileTimeToDateTime(TWinFileTime(LastWriteTime^));
  DateTime:= FormatDateTime('yyyymmddhhnn.ss', FileTime);
  Result:= SendCommand('touch -ct ' + DateTime + ' ' + EscapeNoQuotes(FileName), FAnswer);
end;

end.

