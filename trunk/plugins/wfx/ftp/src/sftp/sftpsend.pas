unit SFtpSend;

{$mode delphi}

interface

uses
  Classes, SysUtils, WfxPlugin, ftpsend, libssh, FtpAdv;

type

  { TSFtpSend }

  TSFtpSend = class(TFTPSendEx)
  private
    FLastError: Integer;
    SourceName, TargetName: PWideChar;
    procedure DoProgress(Percent: Int64);
    function FileClose(Handle: Pointer): Boolean;
  protected
    FCurrentDir: String;
    FSession: PLIBSSH2_SESSION;
    FSFTPSession: PLIBSSH2_SFTP;
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
    function FsFindFirstW(const Path: String; var FindData: TWin32FindDataW): Pointer; override;
    function FsFindNextW(Handle: Pointer; var FindData: TWin32FindDataW): BOOL; override;
    function FsFindClose(Handle: Pointer): Integer; override;
    function FsSetTime(const FileName: String; LastAccessTime, LastWriteTime: PFileTime): BOOL; override;
  end;

implementation

uses
  DCBasicTypes, DCDateTimeUtils, DCStrUtils, DCOSUtils, FtpFunc, CTypes, DCClassesUtf8;

const
  SMB_BUFFER_SIZE = 131072;

procedure userauth_kbdint(const name: PAnsiChar; name_len: cint;
                          const instruction: PAnsiChar; instruction_len: cint;
                          num_prompts: cint; const prompts: PLIBSSH2_USERAUTH_KBDINT_PROMPT;
                          responses: PLIBSSH2_USERAUTH_KBDINT_RESPONSE; abstract: PPointer); cdecl;
var
  I: Integer;
  Sender: TSFtpSend;
begin
  Sender:= TSFtpSend(abstract^);
  for I:= 0 to num_prompts - 1 do
  begin
    if (I = 0) and (Length(Sender.FPassword) > 0) then
    begin
      responses^.text:= PAnsiChar(Sender.FPassword);
      responses^.length:= Length(Sender.FPassword);
    end;
  end;
end;

{ TSFtpSend }

procedure TSFtpSend.DoProgress(Percent: Int64);
begin
  if ProgressProc(PluginNumber, SourceName, TargetName, Percent) = 1 then
    raise EUserAbort.Create(EmptyStr);
end;

function TSFtpSend.FileClose(Handle: Pointer): Boolean;
begin
  if Assigned(Handle) then
  repeat
    FLastError:= libssh2_sftp_close(Handle);
    DoProgress(100);
  until FLastError <> LIBSSH2_ERROR_EAGAIN;
  Result:= (FLastError = 0);
end;

function TSFtpSend.Connect: Boolean;
const
  HOSTKEY_SIZE = 20;
var
  I: Integer;
  S: String;
  userauthlist: PAnsiChar;
  FingerPrint: array [0..Pred(HOSTKEY_SIZE)] of AnsiChar;
begin
  FSock.CloseSocket;
  DoStatus(False, 'Connecting to: ' + FTargetHost);
  FSock.Connect(FTargetHost, FTargetPort);

  if FSock.LastError=0 then
  begin
    FSession := libssh2_session_init_ex(nil, nil, nil, Self);

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
    i:=0;
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
      if I <> 0 then
      begin
        DoStatus(False, 'Authentication by password failed');
        Exit(False);
      end;
    end
    else if (strpos(userauthlist, 'keyboard-interactive') <> nil) then
    begin
      I:= libssh2_userauth_keyboard_interactive(FSession, PAnsiChar(FUserName), @userauth_kbdint);
      if I <> 0 then
      begin
        DoStatus(False, 'Authentication by keyboard-interactive failed');
        Exit(False);
      end;
    end
    else if (strpos(userauthlist, 'publickey') <> nil) then
    begin
      DoStatus(False, 'Authentication by publickey is not supported!');
      Exit(False);
    end;

    DoStatus(False, 'Authentication succeeded');
    FSFTPSession := libssh2_sftp_init(FSession);

  end;
  Result:= Assigned(FSFTPSession);
end;

constructor TSFtpSend.Create(const Encoding: String);
begin
  FCurrentDir:= '/';
  inherited Create(Encoding);
  FTargetPort:= '22';
  FCanResume := True;
end;

function TSFtpSend.Login: Boolean;
begin
  Result:= Connect;
end;

function TSFtpSend.Logout: Boolean;
begin
  Result:= libssh2_sftp_shutdown(FSFTPSession) = 0;
  libssh2_session_disconnect(FSession, 'Thank you for using sshtest');
  libssh2_session_free(FSession);
  FSock.CloseSocket;
end;

function TSFtpSend.GetCurrentDir: String;
begin
  Result:= FCurrentDir;
end;

function TSFtpSend.FileSize(const FileName: String): Int64;
var
  Attributes: LIBSSH2_SFTP_ATTRIBUTES;
begin
  repeat
    FLastError:= libssh2_sftp_stat(FSFTPSession, PAnsiChar(FileName), @Attributes);
    if (FLastError = 0) then Exit(Attributes.filesize);
    DoProgress(0);
  until FLastError <> LIBSSH2_ERROR_EAGAIN;
  Result:= -1;
end;

function TSFtpSend.CreateDir(const Directory: string): Boolean;
var
  Return: Integer;
  Attributes: LIBSSH2_SFTP_ATTRIBUTES;
begin
  Return:= libssh2_sftp_mkdir(FSFTPSession,
                              PAnsiChar(Directory),
                              LIBSSH2_SFTP_S_IRWXU or
                              LIBSSH2_SFTP_S_IRGRP or LIBSSH2_SFTP_S_IXGRP or
                              LIBSSH2_SFTP_S_IROTH or LIBSSH2_SFTP_S_IXOTH);
  if (Return <> 0) then begin
    Return:= libssh2_sftp_stat(FSFTPSession, PAnsiChar(Directory), @Attributes);
  end;
  Result:= (Return = 0);
end;

function TSFtpSend.DeleteDir(const Directory: string): Boolean;
begin
  Result:= libssh2_sftp_rmdir(FSFTPSession, PAnsiChar(Directory)) = 0;
end;

function TSFtpSend.DeleteFile(const FileName: string): Boolean;
begin
  Result:= libssh2_sftp_unlink(FSFTPSession, PAnsiChar(FileName)) = 0;
end;

function TSFtpSend.ExecuteCommand(const Command: String): Boolean;
begin
  Result:= False;
end;

function TSFtpSend.ChangeWorkingDir(const Directory: string): Boolean;
var
  Attributes: LIBSSH2_SFTP_ATTRIBUTES;
begin
  Result:= libssh2_sftp_stat(FSFTPSession, PAnsiChar(Directory), @Attributes) = 0;
  if Result then FCurrentDir:= Directory;
end;

function TSFtpSend.RenameFile(const OldName, NewName: string): Boolean;
begin
  Result:= libssh2_sftp_rename(FSFTPSession, PAnsiChar(OldName), PAnsiChar(NewName)) = 0;
end;

function TSFtpSend.ChangeMode(const FileName, Mode: String): Boolean;
var
  Attributes: LIBSSH2_SFTP_ATTRIBUTES;
begin
  Attributes.permissions:= OctToDec(Mode);
  Attributes.flags:= LIBSSH2_SFTP_ATTR_PERMISSIONS;
  Result:= libssh2_sftp_setstat(FSFTPSession, PAnsiChar(FileName), @Attributes) = 0;
end;

function TSFtpSend.StoreFile(const FileName: string; Restore: Boolean): Boolean;
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
  TargetHandle: PLIBSSH2_SFTP_HANDLE;
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
      TargetHandle:= libssh2_sftp_open(FSFTPSession,
                                       PAnsiChar(FileName),
                                       Flags, $1A0);
      if (TargetHandle = nil) then
      begin
        FLastError:= libssh2_session_last_errno(FSession);
        if (FLastError <> LIBSSH2_ERROR_EAGAIN) then Exit(False);
        DoProgress((FileSize - TotalBytesToWrite) * 100 div FileSize);
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
          BytesWritten:= libssh2_sftp_write(TargetHandle, FBuffer + Index, BytesToWrite);
          if BytesWritten = LIBSSH2_ERROR_EAGAIN then begin
            DoProgress((FileSize - TotalBytesToWrite) * 100 div FileSize);
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
    Result:= FileClose(TargetHandle) and Result;
    libssh2_session_set_blocking(FSession, 1);
  end;
end;

function TSFtpSend.RetrieveFile(const FileName: string; FileSize: Int64;
  Restore: Boolean): Boolean;
var
  FBuffer: PByte;
  BytesRead: PtrInt;
  RetrStream: TFileStreamEx;
  TotalBytesToRead: Int64 = 0;
  SourceHandle: PLIBSSH2_SFTP_HANDLE;
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
      SourceHandle:= libssh2_sftp_open(FSFTPSession,
                                       PAnsiChar(FileName),
                                       LIBSSH2_FXF_READ, 0);
      if (SourceHandle = nil) then
      begin
        FLastError:= libssh2_session_last_errno(FSession);
        if (FLastError <> LIBSSH2_ERROR_EAGAIN) then Exit(False);
        DoProgress((FileSize - TotalBytesToRead) * 100 div FileSize);
      end;
    until not ((SourceHandle = nil) and (FLastError = LIBSSH2_ERROR_EAGAIN));

    if Restore then begin
      libssh2_sftp_seek64(SourceHandle, TotalBytesToRead);
    end;

    FBuffer:= GetMem(SMB_BUFFER_SIZE);
    TotalBytesToRead:= FileSize - TotalBytesToRead;
    try
      while TotalBytesToRead > 0 do
      begin
        repeat
          BytesRead := libssh2_sftp_read(SourceHandle, PAnsiChar(FBuffer), SMB_BUFFER_SIZE);
          if BytesRead = LIBSSH2_ERROR_EAGAIN then begin
            DoProgress((FileSize - TotalBytesToRead) * 100 div FileSize);
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
      Result:= FileClose(SourceHandle) and Result;
    end;
  finally
    libssh2_session_set_blocking(FSession, 1);
  end;
end;

function TSFtpSend.FsFindFirstW(const Path: String; var FindData: TWin32FindDataW): Pointer;
begin
  Result := libssh2_sftp_opendir(FSFTPSession, PAnsiChar(Path));
  if Assigned(Result) then FsFindNextW(Result, FindData);
end;

function TSFtpSend.FsFindNextW(Handle: Pointer; var FindData: TWin32FindDataW): BOOL;
var
  Return: Integer;
  Attributes: LIBSSH2_SFTP_ATTRIBUTES;
  AFileName: array[0..1023] of AnsiChar;
  AFullData: array[0..1023] of AnsiChar;
begin
  Return:= libssh2_sftp_readdir_ex(Handle, AFileName, SizeOf(AFileName),
                                   AFullData, SizeOf(AFullData), @Attributes);
  Result:= (Return > 0);
  if Result then
  begin
    FillChar(FindData, SizeOf(FindData), 0);
    StrPCopy(FindData.cFileName, ServerToClient(AFileName));
    FindData.dwReserved0:= Attributes.permissions;
    FindData.dwFileAttributes:= FILE_ATTRIBUTE_UNIX_MODE;
    FindData.nFileSizeLow:= Int64Rec(Attributes.filesize).Lo;
    FindData.nFileSizeHigh:= Int64Rec(Attributes.filesize).Hi;
    FindData.ftLastAccessTime:= TWfxFileTime(UnixFileTimeToWinTime(Attributes.atime));
    FindData.ftLastWriteTime:= TWfxFileTime(UnixFileTimeToWinTime(Attributes.mtime));
  end;
end;

function TSFtpSend.FsFindClose(Handle: Pointer): Integer;
begin
  Result:= libssh2_sftp_closedir(Handle);
end;

function TSFtpSend.FsSetTime(const FileName: String; LastAccessTime,
  LastWriteTime: WfxPlugin.PFileTime): BOOL;
var
  Attributes: LIBSSH2_SFTP_ATTRIBUTES;
begin
  if (LastAccessTime = nil) or (LastWriteTime = nil) then
  begin
    if libssh2_sftp_stat(FSFTPSession, PAnsiChar(FileName), @Attributes) <> 0 then
      Exit(False);
  end;
  if Assigned(LastAccessTime) then begin
    Attributes.atime:= WinFileTimeToUnixTime(TWinFileTime(LastAccessTime^));
  end;
  if Assigned(LastWriteTime) then begin
    Attributes.mtime:= WinFileTimeToUnixTime(TWinFileTime(LastWriteTime^));
  end;
  Attributes.flags:= LIBSSH2_SFTP_ATTR_ACMODTIME;
  Result:= libssh2_sftp_setstat(FSFTPSession, PAnsiChar(FileName), @Attributes) = 0;
end;

end.

