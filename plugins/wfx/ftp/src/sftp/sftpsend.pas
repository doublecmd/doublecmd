{
   Double commander
   -------------------------------------------------------------------------
   Wfx plugin for working with File Transfer Protocol

   Copyright (C) 2013-2020 Alexander Koblov (alexx2000@mail.ru)

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

unit SftpSend;

{$mode delphi}
{$pointermath on}

interface

uses
  Classes, SysUtils, WfxPlugin, ftpsend, ScpSend, libssh, FtpAdv;

type

  { TSftpSend }

  TSftpSend = class(TScpSend)
  private
    function FileClose(Handle: Pointer): Boolean;
  protected
    FCopySCP: Boolean;
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
  public
    property CopySCP: Boolean read FCopySCP write FCopySCP;
  end;

implementation

uses
  LazUTF8, DCBasicTypes, DCDateTimeUtils, DCStrUtils, DCOSUtils, FtpFunc, CTypes,
  DCClassesUtf8, DCFileAttributes;

const
  SMB_BUFFER_SIZE = 131072;

type
  PFindRec = ^TFindRec;
  TFindRec = record
    Path: String;
    Handle: PLIBSSH2_SFTP_HANDLE;
  end;

{ TSftpSend }

function TSftpSend.FileClose(Handle: Pointer): Boolean;
begin
  FLastError:= 0;
  if Assigned(Handle) then
  repeat
    FLastError:= libssh2_sftp_close(Handle);
    DoProgress(100);
    FSock.CanRead(10);
  until FLastError <> LIBSSH2_ERROR_EAGAIN;
  Result:= (FLastError = 0);
end;

function TSftpSend.Connect: Boolean;
begin
  Result:= inherited Connect;

  if Result then
  begin
    FSFTPSession := libssh2_sftp_init(FSession);

    Result:= Assigned(FSFTPSession);

    if not Result then begin
      libssh2_session_free(FSession);
      FSock.CloseSocket;
    end;
  end;
end;

constructor TSftpSend.Create(const Encoding: String);
begin
  inherited Create(Encoding);
  FCanResume := True;
end;

function TSftpSend.Login: Boolean;
begin
  Result:= Connect;
  if Result and FAuto then DetectEncoding;
end;

function TSftpSend.Logout: Boolean;
begin
  Result:= libssh2_sftp_shutdown(FSFTPSession) = 0;
  Result:= Result and inherited Logout;
end;

function TSftpSend.GetCurrentDir: String;
begin
  Result:= FCurrentDir;
end;

function TSftpSend.FileSize(const FileName: String): Int64;
var
  Attributes: LIBSSH2_SFTP_ATTRIBUTES;
begin
  repeat
    FLastError:= libssh2_sftp_stat(FSFTPSession, PAnsiChar(FileName), @Attributes);
    if (FLastError = 0) then Exit(Attributes.filesize);
    FSock.CanRead(10);
    DoProgress(0);
  until FLastError <> LIBSSH2_ERROR_EAGAIN;
  Result:= -1;
end;

function TSftpSend.CreateDir(const Directory: string): Boolean;
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

function TSftpSend.DeleteDir(const Directory: string): Boolean;
begin
  Result:= libssh2_sftp_rmdir(FSFTPSession, PAnsiChar(Directory)) = 0;
end;

function TSftpSend.DeleteFile(const FileName: string): Boolean;
begin
  Result:= libssh2_sftp_unlink(FSFTPSession, PAnsiChar(FileName)) = 0;
end;

function TSftpSend.ChangeWorkingDir(const Directory: string): Boolean;
var
  Attributes: LIBSSH2_SFTP_ATTRIBUTES;
begin
  Result:= libssh2_sftp_stat(FSFTPSession, PAnsiChar(Directory), @Attributes) = 0;
  if Result then FCurrentDir:= Directory;
end;

function TSftpSend.RenameFile(const OldName, NewName: string): Boolean;
begin
  Result:= libssh2_sftp_rename(FSFTPSession, PAnsiChar(OldName), PAnsiChar(NewName)) = 0;
end;

function TSftpSend.ChangeMode(const FileName, Mode: String): Boolean;
var
  Attributes: LIBSSH2_SFTP_ATTRIBUTES;
begin
  Attributes.permissions:= OctToDec(Mode);
  Attributes.flags:= LIBSSH2_SFTP_ATTR_PERMISSIONS;
  Result:= libssh2_sftp_setstat(FSFTPSession, PAnsiChar(FileName), @Attributes) = 0;
end;

function TSftpSend.StoreFile(const FileName: string; Restore: Boolean): Boolean;
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
  TargetHandle: PLIBSSH2_SFTP_HANDLE = nil;
  Flags: cint = LIBSSH2_FXF_CREAT or LIBSSH2_FXF_WRITE;
begin
  if FCopySCP then begin
    Result:= inherited StoreFile(FileName, Restore);
    Exit;
  end;

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
          BytesWritten:= libssh2_sftp_write(TargetHandle, FBuffer + Index, BytesToWrite);
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
    Result:= FileClose(TargetHandle) and Result;
    libssh2_session_set_blocking(FSession, 1);
  end;
end;

function TSftpSend.RetrieveFile(const FileName: string; FileSize: Int64;
  Restore: Boolean): Boolean;
var
  FBuffer: PByte;
  BytesRead: PtrInt;
  RetrStream: TFileStreamEx;
  TotalBytesToRead: Int64 = 0;
  SourceHandle: PLIBSSH2_SFTP_HANDLE;
begin
  if FCopySCP then begin
    Result:= inherited RetrieveFile(FileName, FileSize, Restore);
    Exit;
  end;

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
        if (FileSize > 0) then DoProgress((FileSize - TotalBytesToRead) * 100 div FileSize);
        FSock.CanRead(10);
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
      Result:= FileClose(SourceHandle) and Result;
    end;
  finally
    libssh2_session_set_blocking(FSession, 1);
  end;
end;

function TSftpSend.FsFindFirstW(const Path: String; var FindData: TWin32FindDataW): Pointer;
var
  FindRec: PFindRec;
begin
  Result := libssh2_sftp_opendir(FSFTPSession, PAnsiChar(Path));
  if Assigned(Result) then
  begin
    New(FindRec);
    FindRec.Path:= Path;
    FindRec.Handle:= Result;
    FsFindNextW(FindRec, FindData);
    Result:= FindRec;
end;
end;

function TSftpSend.FsFindNextW(Handle: Pointer; var FindData: TWin32FindDataW): BOOL;
var
  Return: Integer;
  FindRec: PFindRec absolute Handle;
  Attributes: LIBSSH2_SFTP_ATTRIBUTES;
  AFileName: array[0..1023] of AnsiChar;
  AFullData: array[0..2047] of AnsiChar;
begin
  Return:= libssh2_sftp_readdir_ex(FindRec.Handle, AFileName, SizeOf(AFileName),
                                   AFullData, SizeOf(AFullData), @Attributes);
  Result:= (Return > 0);
  if Result then
  begin
    FillChar(FindData, SizeOf(FindData), 0);
    FindData.dwReserved0:= Attributes.permissions;
    FindData.dwFileAttributes:= FILE_ATTRIBUTE_UNIX_MODE;
    if (Attributes.permissions and S_IFMT) <> S_IFDIR then
    begin
      FindData.nFileSizeLow:= Int64Rec(Attributes.filesize).Lo;
      FindData.nFileSizeHigh:= Int64Rec(Attributes.filesize).Hi;
    end;
    StrPLCopy(FindData.cFileName, ServerToClient(AFileName), MAX_PATH - 1);
    FindData.ftLastWriteTime:= TWfxFileTime(UnixFileTimeToWinTime(Attributes.mtime));
    FindData.ftLastAccessTime:= TWfxFileTime(UnixFileTimeToWinTime(Attributes.atime));
    if (Attributes.permissions and S_IFMT) = S_IFLNK then
    begin
      if libssh2_sftp_stat(FSFTPSession, PAnsiChar(FindRec.Path + AFileName), @Attributes) = 0 then
      begin
        if (Attributes.permissions and S_IFMT) = S_IFDIR then
        begin
          FindData.nFileSizeLow:= 0;
          FindData.nFileSizeHigh:= 0;
          FindData.dwFileAttributes:= FindData.dwFileAttributes or FILE_ATTRIBUTE_REPARSE_POINT;
        end;
      end;
    end;
  end;
end;

function TSftpSend.FsFindClose(Handle: Pointer): Integer;
var
  FindRec: PFindRec absolute Handle;
begin
  Result:= libssh2_sftp_closedir(FindRec.Handle);
  Dispose(FindRec);
end;

function TSftpSend.FsSetTime(const FileName: String; LastAccessTime,
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

