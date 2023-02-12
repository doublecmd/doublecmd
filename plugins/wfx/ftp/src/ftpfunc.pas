{
   Double commander
   -------------------------------------------------------------------------
   Wfx plugin for working with File Transfer Protocol

   Copyright (C) 2009-2023 Alexander Koblov (alexx2000@mail.ru)

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

unit FtpFunc;

{$mode delphi}{$H+}
{$include calling.inc}

interface

uses
  SysUtils, Classes,
  WfxPlugin, Extension;

const
  cAddConnection = '<Add connection>';
  cQuickConnection = '<Quick connection>';

type

  { TConnection }

  TConnection = class
  public
    ConnectionName, Path, Host: AnsiString;
    Port: AnsiString;
    UserName: AnsiString;
    Password: AnsiString;
    MasterPassword: Boolean;
    CachedPassword: AnsiString;
    Proxy: String;
    PassiveMode: Boolean;
    CopySCP: Boolean;
    OnlySCP: Boolean;
    AutoTLS: Boolean;
    FullSSL: Boolean;
    OpenSSH: Boolean;
    UseAllocate: Boolean;
    Encoding: AnsiString;
    Fingerprint: AnsiString;
    InitCommands: AnsiString;
    ShowHiddenItems: Boolean;
    PasswordChanged: Boolean;
    KeepAliveTransfer: Boolean;
    PublicKey, PrivateKey: String;
  public
    procedure Assign(Connection: TConnection);
  end;

function FsInitW(PluginNr: Integer; pProgressProc: TProgressProcW;
  pLogProc: TLogProcW; pRequestProc: TRequestProcW): Integer; dcpcall;

function FsFindFirstW(Path: PWideChar; var FindData: TWin32FindDataW): THandle; dcpcall;
function FsFindNextW(Hdl: THandle; var FindData: TWin32FindDataW): BOOL; dcpcall;
function FsFindClose(Hdl: THandle): Integer; dcpcall;

function FsExecuteFileW(MainWin: THandle; RemoteName, Verb: PWideChar): Integer; dcpcall;
function FsRenMovFileW(OldName, NewName: PWideChar; Move, OverWrite: BOOL;
  RemoteInfo: pRemoteInfo): Integer; dcpcall;
function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: Integer;
  RemoteInfo: pRemoteInfo): Integer; dcpcall;
function FsPutFileW(LocalName, RemoteName: PWideChar; CopyFlags: Integer)
  : Integer; dcpcall;
function FsDeleteFileW(RemoteName: PWideChar): BOOL; dcpcall;

function FsMkDirW(RemoteDir: PWideChar): BOOL; dcpcall;
function FsRemoveDirW(RemoteName: PWideChar): BOOL; dcpcall;

function FsSetTimeW(RemoteName: PWideChar; CreationTime, LastAccessTime,
                    LastWriteTime: PFileTime): BOOL; dcpcall;

function FsDisconnectW(DisconnectRoot: PWideChar): BOOL; dcpcall;

procedure FsSetCryptCallbackW(pCryptProc: TCryptProcW; CryptoNr, Flags: Integer); dcpcall;
procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer); dcpcall;
procedure FsSetDefaultParams(dps: pFsDefaultParamStruct); dcpcall;
procedure FsStatusInfoW(RemoteDir: PWideChar; InfoStartEnd, InfoOperation: Integer); dcpcall;
function FsGetBackgroundFlags: Integer; dcpcall;
{ Network API }
{
procedure FsNetworkGetSupportedProtocols(Protocols: PAnsiChar; MaxLen: LongInt); dcpcall;
function FsNetworkGetConnection(Index: LongInt; Connection: PAnsiChar; MaxLen: LongInt): LongBool; dcpcall;
function FsNetworkManageConnection(MainWin: HWND; Connection: PAnsiChar; Action: LongInt; MaxLen: LongInt): LongBool; dcpcall;
function FsNetworkOpenConnection(Connection: PAnsiChar; RootDir, RemotePath: PAnsiChar; MaxLen: LongInt): LongBool; dcpcall;
}
{ Extension API }
procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall;

function ReadPassword(ConnectionName: AnsiString; out Password: AnsiString): Boolean;
function DeletePassword(ConnectionName: AnsiString): Boolean;

var
  gStartupInfo: TExtensionStartupInfo;

var
  LogProc: TLogProcW;
  CryptProc: TCryptProcW;
  PluginNumber: Integer;
  CryptoNumber: Integer;
  RequestProc: TRequestProcW;
  ProgressProc: TProgressProcW;

implementation

uses
  IniFiles, StrUtils, FtpAdv, FtpUtils, FtpConfDlg, syncobjs, LazFileUtils,
  LazUTF8, DCClassesUtf8, DCConvertEncoding, SftpSend, ScpSend, FtpProxy;

var
  DefaultIniName: String;
  TcpKeepAlive: Boolean = True;
  ActiveConnectionList, ConnectionList: TStringList;
  IniFile: TIniFile;
  ListLock: TCriticalSection;

threadvar
  ThreadCon: TFtpSendEx;

const
  FS_COPYFLAGS_FORCE = FS_COPYFLAGS_OVERWRITE or FS_COPYFLAGS_RESUME;
  RootList: array [0 .. 1] of AnsiString = (cAddConnection, cQuickConnection);

type
  TListRec = record
    Path: AnsiString;
    Index: Integer;
    FtpSend: TFTPSendEx;
    FtpList: TFTPListEx;
  end;
  PListRec = ^TListRec;

procedure ReadConnectionList;
var
  I, Count: Integer;
  sIndex: AnsiString;
  Connection: TConnection;
begin
  Count := IniFile.ReadInteger('FTP', 'ConnectionCount', 0);
  for I := 1 to Count do
  begin
    sIndex := IntToStr(I);
    Connection := TConnection.Create;
    Connection.ConnectionName := IniFile.ReadString('FTP', 'Connection' + sIndex + 'Name', EmptyStr);
    Connection.Path := IniFile.ReadString('FTP', 'Connection' + sIndex + 'Path', EmptyStr);
    Connection.Host := IniFile.ReadString('FTP', 'Connection' + sIndex + 'Host', EmptyStr);
    Connection.Port := IniFile.ReadString('FTP', 'Connection' + sIndex + 'Port', EmptyStr);
    Connection.UserName := IniFile.ReadString('FTP', 'Connection' + sIndex + 'UserName', EmptyStr);
    Connection.MasterPassword := IniFile.ReadBool('FTP', 'Connection' + sIndex + 'MasterPassword', False);
    if Connection.MasterPassword then
      Connection.Password := EmptyStr
    else
      Connection.Password := DecodeBase64(IniFile.ReadString('FTP', 'Connection' + sIndex + 'Password', EmptyStr));
    Connection.Proxy := IniFile.ReadString('FTP', 'Connection' + sIndex + 'Proxy', EmptyStr);
    Connection.Encoding := IniFile.ReadString('FTP', 'Connection' + sIndex + 'Encoding', EmptyStr);
    Connection.PassiveMode:= IniFile.ReadBool('FTP', 'Connection' + sIndex + 'PassiveMode', True);
    Connection.AutoTLS:= IniFile.ReadBool('FTP', 'Connection' + sIndex + 'AutoTLS', False);
    Connection.FullSSL:= IniFile.ReadBool('FTP', 'Connection' + sIndex + 'FullSSL', False);
    Connection.OpenSSH:= IniFile.ReadBool('FTP', 'Connection' + sIndex + 'OpenSSH', False);
    Connection.OnlySCP:= IniFile.ReadBool('FTP', 'Connection' + sIndex + 'OnlySCP', False);
    Connection.CopySCP:= IniFile.ReadBool('FTP', 'Connection' + sIndex + 'CopySCP', False);
    Connection.UseAllocate:= IniFile.ReadBool('FTP', 'Connection' + sIndex + 'UseAllocate', False);
    Connection.PublicKey := IniFile.ReadString('FTP', 'Connection' + sIndex + 'PublicKey', EmptyStr);
    Connection.PrivateKey := IniFile.ReadString('FTP', 'Connection' + sIndex + 'PrivateKey', EmptyStr);
    Connection.Fingerprint:= IniFile.ReadString('FTP', 'Connection' + sIndex + 'Fingerprint', EmptyStr);
    Connection.InitCommands := IniFile.ReadString('FTP', 'Connection' + sIndex + 'InitCommands', EmptyStr);
    Connection.ShowHiddenItems := IniFile.ReadBool('FTP', 'Connection' + sIndex + 'ShowHiddenItems', True);
    Connection.KeepAliveTransfer := IniFile.ReadBool('FTP', 'Connection' + sIndex + 'KeepAliveTransfer', False);
    // add connection to connection list
    ConnectionList.AddObject(Connection.ConnectionName, Connection);
  end;
  LoadProxyList(IniFile);
end;

procedure WriteConnectionList;
var
  I, Count: Integer;
  sIndex: AnsiString;
  Connection: TConnection;
begin
  IniFile.EraseSection('FTP');
  Count := ConnectionList.Count;
  IniFile.WriteInteger('FTP', 'ConnectionCount', Count);
  for I := 0 to Count - 1 do
  begin
    sIndex := IntToStr(I + 1);
    Connection := TConnection(ConnectionList.Objects[I]);
    IniFile.WriteString('FTP', 'Connection' + sIndex + 'Name', Connection.ConnectionName);
    IniFile.WriteString('FTP', 'Connection' + sIndex + 'Path', Connection.Path);
    IniFile.WriteString('FTP', 'Connection' + sIndex + 'Host', Connection.Host);
    IniFile.WriteString('FTP', 'Connection' + sIndex + 'Port', Connection.Port);
    IniFile.WriteString('FTP', 'Connection' + sIndex + 'UserName', Connection.UserName);
    IniFile.WriteBool('FTP', 'Connection' + sIndex + 'MasterPassword', Connection.MasterPassword);
    if Connection.MasterPassword then
      IniFile.DeleteKey('FTP', 'Connection' + sIndex + 'Password')
    else
      IniFile.WriteString('FTP', 'Connection' + sIndex + 'Password', EncodeBase64(Connection.Password));
    IniFile.WriteString('FTP', 'Connection' + sIndex + 'Proxy', Connection.Proxy);
    IniFile.WriteString('FTP', 'Connection' + sIndex + 'Encoding', Connection.Encoding);
    IniFile.WriteBool('FTP', 'Connection' + sIndex + 'PassiveMode', Connection.PassiveMode);
    IniFile.WriteBool('FTP', 'Connection' + sIndex + 'AutoTLS', Connection.AutoTLS);
    IniFile.WriteBool('FTP', 'Connection' + sIndex + 'FullSSL', Connection.FullSSL);
    IniFile.WriteBool('FTP', 'Connection' + sIndex + 'OpenSSH', Connection.OpenSSH);
    IniFile.WriteBool('FTP', 'Connection' + sIndex + 'OnlySCP', Connection.OnlySCP);
    IniFile.WriteBool('FTP', 'Connection' + sIndex + 'CopySCP', Connection.CopySCP);
    IniFile.WriteBool('FTP', 'Connection' + sIndex + 'UseAllocate', Connection.UseAllocate);
    IniFile.WriteString('FTP', 'Connection' + sIndex + 'PublicKey', Connection.PublicKey);
    IniFile.WriteString('FTP', 'Connection' + sIndex + 'PrivateKey', Connection.PrivateKey);
    IniFile.WriteString('FTP', 'Connection' + sIndex + 'Fingerprint', Connection.Fingerprint);
    IniFile.WriteString('FTP', 'Connection' + sIndex + 'InitCommands', Connection.InitCommands);
    IniFile.WriteBool('FTP', 'Connection' + sIndex + 'ShowHiddenItems', Connection.ShowHiddenItems);
    IniFile.WriteBool('FTP', 'Connection' + sIndex + 'KeepAliveTransfer', Connection.KeepAliveTransfer);
  end;
  SaveProxyList(IniFile);
  IniFile.UpdateFile;
end;

procedure FreeConnectionList;
var
  I, Count: Integer;
  Connection: TConnection;
begin
  Count := ConnectionList.Count;
  for I := Count - 1 downto 0  do
  begin
    Connection := TConnection(ConnectionList.Objects[I]);
    Connection.Free;
    ConnectionList.Delete(I);
  end;
end;

procedure ZeroPassword(var APassword: String);
begin
  if (Length(APassword) > 0) then
  begin
    FillChar(APassword[1], Length(APassword), 0);
    SetLength(APassword, 0);
  end;
end;

function CryptFunc(Mode: LongInt; ConnectionName: String; var Password: String): LongInt;
var
  APassword: UnicodeString;
  AConnection: UnicodeString;
begin
  APassword:= CeUtf8ToUtf16(Password);
  AConnection:= CeUtf8ToUtf16(ConnectionName);
  if (Mode = FS_CRYPT_LOAD_PASSWORD) or (Mode = FS_CRYPT_LOAD_PASSWORD_NO_UI) then
  begin
    SetLength(APassword, MAX_PATH);
    FillChar(APassword[1], MAX_PATH * SizeOf(WideChar), #0);
  end;
  Result:= CryptProc(PluginNumber, CryptoNumber, Mode, PWideChar(AConnection), PWideChar(APassword), MAX_PATH);
  if (Mode = FS_CRYPT_LOAD_PASSWORD) or (Mode = FS_CRYPT_LOAD_PASSWORD_NO_UI) then
  begin
    Password:= UTF16ToUTF8(PWideChar(APassword)); // truncate to #0
  end;
end;

function ShowPasswordDialog(out Password: String): Boolean;
var
  APassword: UnicodeString;
begin
  SetLength(APassword, MAX_PATH); APassword[1] := #0;
  Result := RequestProc(PluginNumber, RT_Password, nil, nil, PWideChar(APassword), MAX_PATH);
  if Result then
    Password := UTF16ToUTF8(PWideChar(APassword)) // truncate to #0
  else
    Password := EmptyStr;
end;

function FtpLogin(const Connection: TConnection; const FtpSend: TFTPSendEx): Boolean;
var
  sTemp: AnsiString;
begin
  Result := False;
  if FtpSend.Login then
  begin
    sTemp:= Connection.InitCommands;
    while sTemp <> EmptyStr do
      FtpSend.ExecuteCommand(Copy2SymbDel(sTemp, ';'));
    if Length(Connection.Path) > 0 then
      FtpSend.ChangeWorkingDir(FtpSend.ClientToServer(CeUtf8ToUtf16(Connection.Path)));
    Result := True;
  end;
end;

function FtpConnect(const ConnectionName: AnsiString; out FtpSend: TFTPSendEx): Boolean;
var
  I: Integer;
  APassword: String;
  Connection: TConnection;
begin
  Result:= False;
  I:= ActiveConnectionList.IndexOf(ConnectionName);
  // If find active connection then use it
  if I >= 0 then
    begin
      FtpSend:= TFTPSendEx(ActiveConnectionList.Objects[I]);
      
      if FtpSend.NetworkError then
        //Server closed the connection, or network error occurred, or whatever else.
        //Attempt to reconnect and execute login sequence
        begin
          LogProc(PluginNumber, msgtype_details, PWideChar('Network error detected, attempting to reconnect...'));
          I:= ConnectionList.IndexOf(ConnectionName);
          if I >= 0 then
          begin
            Connection := TConnection(ConnectionList.Objects[I]);
            if not FtpLogin(Connection, FtpSend) then
              begin
                RequestProc(PluginNumber, RT_MsgOK, nil, 'Connection lost, unable to reconnect!', nil, MAX_PATH);
                Exit;
              end;
          end;
        end;
      
      Result:= True;
    end
  else
    begin
      // find in exists connection list
      I:= ConnectionList.IndexOf(ConnectionName);
      if I >= 0 then
      begin
        Connection := TConnection(ConnectionList.Objects[I]);
        if Connection.OpenSSH then
        begin
          if Connection.OnlySCP then
            FtpSend := TScpSend.Create(Connection.Encoding)
          else begin
            FtpSend := TSftpSend.Create(Connection.Encoding);
            TSftpSend(FtpSend).CopySCP := Connection.CopySCP;
          end;
          FtpSend.PublicKey:= Connection.PublicKey;
          FtpSend.PrivateKey:= Connection.PrivateKey;
          TScpSend(FtpSend).Fingerprint:= Connection.Fingerprint;
        end
        else begin
          FtpSend := TFTPSendEx.Create(Connection.Encoding);
          FtpSend.ShowHidden := Connection.ShowHiddenItems;
          FtpSend.KeepAliveTransfer := Connection.KeepAliveTransfer;
        end;
        FtpSend.TcpKeepAlive := TcpKeepAlive;
        FtpSend.TargetHost := Connection.Host;
        FtpSend.PassiveMode:= Connection.PassiveMode;
        FtpSend.AutoTLS:= Connection.AutoTLS;
        FtpSend.FullSSL:= Connection.FullSSL;
        FtpSend.UseAllocate:= Connection.UseAllocate;
        if Connection.Port <> EmptyStr then
          FtpSend.TargetPort := Connection.Port
        else if Connection.FullSSL then
          FtpSend.TargetPort := cFtpsPort;
        if Connection.UserName <> EmptyStr then
          FtpSend.UserName := Connection.UserName;
        if Connection.MasterPassword then
        begin
          if CryptFunc(FS_CRYPT_LOAD_PASSWORD, Connection.ConnectionName, Connection.Password) <> FS_FILE_OK then
            ZeroPassword(Connection.Password);
        end;
        // if no saved password then ask it
        if Connection.OpenSSH and (Connection.PrivateKey <> '') and (Connection.PublicKey <> '') then
          APassword:= EmptyStr
        else if Length(Connection.Password) > 0 then
          APassword:= Connection.Password
        else if Length(Connection.CachedPassword) > 0 then
          APassword:= Connection.CachedPassword
        else if not ShowPasswordDialog(APassword) then
        begin
          FreeAndNil(FtpSend);
          Exit;
        end;
        FtpSend.Password := FtpSend.ClientToServer(APassword);
        SetProxy(FtpSend, Connection.Proxy);
        // try to connect
        if not FtpLogin(Connection, FtpSend) then
        begin
          RequestProc(PluginNumber, RT_MsgOK, nil, 'Can not connect to the server!', nil, MAX_PATH);
          FreeAndNil(FtpSend);
        end
        else begin
          Connection.CachedPassword:= APassword;
          LogProc(PluginNumber, MSGTYPE_CONNECT, PWideChar('CONNECT ' + PathDelim + CeUtf8ToUtf16(ConnectionName)));
          ActiveConnectionList.AddObject(ConnectionName, FtpSend);
          if Connection.OpenSSH and (ConnectionName <> cQuickConnection) then
          begin
            // Save connection server fingerprint
            if Connection.Fingerprint <> TScpSend(FtpSend).Fingerprint then
            begin
              Connection.Fingerprint:= TScpSend(FtpSend).Fingerprint;
              WriteConnectionList;
            end;
          end;
          Result:= True;
        end;
      end;
    end;
end;

function QuickConnection: Boolean;
var
  Index: Integer;
  FtpSend: TFTPSendEx;
  Connection: TConnection;
begin
  Result:= ActiveConnectionList.IndexOf(cQuickConnection) >= 0;
  if not Result then
  begin
    Connection := TConnection.Create;
    Connection.ConnectionName:= cQuickConnection;
    if ShowFtpConfDlg(Connection) then
    begin
      Connection.ConnectionName:= cQuickConnection;
      Index:= ConnectionList.AddObject(Connection.ConnectionName, Connection);
      Result:= FtpConnect(Connection.ConnectionName, FtpSend);
      ConnectionList.Delete(Index);
    end;
    Connection.Free;
  end;
end;

function AddConnection: Integer;
var
  Connection: TConnection;
begin
  Result := -1;
  Connection := TConnection.Create;
  Connection.PassiveMode := True;

  if ShowFtpConfDlg(Connection) then
  begin
    with Connection do
    begin
      if ConnectionList.IndexOf(ConnectionName) >= 0 then begin
        ConnectionName += '+' + IntToStr(Random(MaxInt));
      end;
      if MasterPassword then
      begin
        if Length(Password) = 0 then
          MasterPassword:= False
        else if CryptFunc(FS_CRYPT_SAVE_PASSWORD, ConnectionName, Password) = FS_FILE_OK then
          Password:= EmptyStr
        else
          MasterPassword:= False;
      end;
      Result:= ConnectionList.AddObject(ConnectionName, Connection);
    end;
  end;

  if Result < 0 then
    FreeAndNil(Connection)
  else
    WriteConnectionList;
end;

function EditConnection(ConnectionName: AnsiString): Boolean;
var
  I: Integer;
  ATemp: TConnection;
  Connection: TConnection;
begin
  Result:= False;
  I := ConnectionList.IndexOf(ConnectionName);
  if I >= 0 then
  begin
    ATemp:= TConnection.Create;
    Connection:= TConnection(ConnectionList.Objects[I]);
    ATemp.Assign(Connection);
    if ShowFtpConfDlg(ATemp) then
    begin
      with ATemp do
      begin
        if ConnectionName <> Connection.ConnectionName then
        begin
          if Connection.MasterPassword then
          begin
            if CryptFunc(FS_CRYPT_MOVE_PASSWORD, Connection.ConnectionName, ConnectionName) <> FS_FILE_OK then
            begin
              gStartupInfo.MessageBox('Cannot save connection!', 'FTP', MB_OK or MB_ICONERROR);
              Exit(False);
            end;
          end;
          ConnectionList[I]:= ConnectionName
        end;
        if PasswordChanged then
        begin
          // Master password enabled
          if MasterPassword then
          begin
            if Length(Password) = 0 then
              MasterPassword:= False
            else if CryptFunc(FS_CRYPT_SAVE_PASSWORD, ConnectionName, Password) = FS_FILE_OK then
              Password:= EmptyStr
            else
              MasterPassword:= False;
          end;
          // Master password disabled
          if (MasterPassword = False) and (Connection.MasterPassword <> MasterPassword) then
          begin
            DeletePassword(ConnectionName);
          end;
        end
      end;
      Connection.Assign(ATemp);
      WriteConnectionList;
      Result:= True;
    end;
    FreeAndNil(ATemp);
  end;
end;

function DeleteConnection(ConnectionName: AnsiString): Boolean;
var
  I: Integer;
  Connection: TConnection;
begin
  Result:= False;
  I:= ConnectionList.IndexOf(ConnectionName);
  if I >= 0 then
  begin
    Connection:= TConnection(ConnectionList.Objects[I]);
    Connection.Free;
    ConnectionList.Delete(I);
    WriteConnectionList;
    Result:= True;
  end;
end;

function ExtractConnectionName(const sPath: AnsiString): AnsiString;
var
  Index: Integer;
begin
  Result:= sPath;
  if sPath[1] = PathDelim then
    Result := Copy(sPath, 2, Length(sPath));
  Index := Pos(PathDelim, Result);
  if Index = 0 then
    Index := MaxInt;
  Result := Copy(Result, 1, Index - 1);
end;

function ExtractRemoteFileName(const FileName: AnsiString): AnsiString;
var
  I: Integer;
begin
  Result := FileName;
  System.Delete(Result, 1, 1);

  I := Pos(PathDelim, Result);
  if I = 0 then
    Result := '/'
  else
  begin
    System.Delete(Result, 1, I - 1);
    Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
  end;
end;

function GetConnectionByPath(const sPath: UnicodeString; out FtpSend: TFTPSendEx;
  out RemotePath: AnsiString): Boolean;
var
  sConnName: AnsiString;
begin
  Result := False;
  if (ExtractFileDir(sPath) = PathDelim) then Exit;
  sConnName := ExtractConnectionName(UTF16ToUTF8(sPath));
  if Assigned(ThreadCon) then
  begin
    Result:= True;
    FtpSend:= ThreadCon;
  end
  else begin
    Result:= FtpConnect(sConnName, FtpSend);
  end;
  if Result then begin
    RemotePath:= FtpSend.ClientToServer(sPath);
    RemotePath:= ExtractRemoteFileName(RemotePath);
  end;
end;

function LocalFindNext(Hdl: THandle; var FindData: TWin32FindDataW): Boolean;
var
  ListRec: PListRec absolute Hdl;
  I, RootCount: Integer;
  Connection: TConnection;
begin
  Result := False;
  I := ListRec^.Index;
  RootCount := High(RootList) + 1;
  FillChar(FindData, SizeOf(FindData), 0);
  if I < RootCount then
  begin
    StrPCopy(FindData.cFileName, CeUtf8ToUtf16(RootList[I]));
    FindData.dwFileAttributes := 0;
    Inc(ListRec^.Index);
    Result := True;
  end
  else if I - RootCount < ConnectionList.Count then
  begin
    Connection := TConnection(ConnectionList.Objects[I - RootCount]);
    StrPCopy(FindData.cFileName, CeUtf8ToUtf16(Connection.ConnectionName));
    FindData.dwFileAttributes := FILE_ATTRIBUTE_NORMAL;
    Inc(ListRec^.Index);
    Result := True;
  end;
  if Result then
  begin
    FindData.nFileSizeLow := $FFFFFFFE;
    FindData.nFileSizeHigh := $FFFFFFFF;
    FindData.ftLastWriteTime.dwLowDateTime := $FFFFFFFE;
    FindData.ftLastWriteTime.dwHighDateTime := $FFFFFFFF;
  end;
end;

function FsInitW(PluginNr: Integer; pProgressProc: TProgressProcW;
  pLogProc: TLogProcW; pRequestProc: TRequestProcW): Integer; dcpcall;
begin
  ProgressProc := pProgressProc;
  LogProc := pLogProc;
  RequestProc := pRequestProc;
  PluginNumber := PluginNr;

  Result := 0;
end;

function FsFindFirstW(Path: PWideChar; var FindData: TWin32FindDataW): THandle; dcpcall;
var
  ListRec: PListRec;
  sPath: AnsiString;
  FtpSend: TFTPSendEx;
  Directory: UnicodeString;
begin
  New(ListRec);
  ListRec.Index := 0;
  ListRec.Path := Path;
  ListRec.FtpSend := nil;
  ListRec.FtpList := nil;
  Result := wfxInvalidHandle;

  if Path = PathDelim then
    begin
      Result := THandle(ListRec);
      LocalFindNext(Result, FindData);
    end
  else
    begin
      ListLock.Acquire;
      try
        Directory:= Path;
        if Directory[Length(Directory)] <> PathDelim then
          Directory:= Directory + PathDelim;
        if GetConnectionByPath(Directory, FtpSend, sPath) then
        begin
          ListRec.FtpSend := FtpSend;
          ListRec.FtpList := FtpSend.FsFindFirstW(sPath, FindData);
          if Assigned(ListRec.FtpList) then Result:= THandle(ListRec);
        end;
      finally
        ListLock.Release;
        if Result = wfxInvalidHandle then
          Dispose(ListRec);
      end;
    end;
end;

function FsFindNextW(Hdl: THandle; var FindData: TWin32FindDataW): BOOL; dcpcall;
var
  ListRec: PListRec absolute Hdl;
begin
  if ListRec.Path = PathDelim then
    Result := LocalFindNext(Hdl, FindData)
  else
    Result := ListRec^.FtpSend.FsFindNextW(ListRec.FtpList, FindData);
end;

function FsFindClose(Hdl: THandle): Integer; dcpcall;
var
  ListRec: PListRec absolute Hdl;
begin
  Result:= 0;
  if Assigned(ListRec) then
  begin
    if Assigned(ListRec^.FtpSend) then begin
      Result:= ListRec^.FtpSend.FsFindClose(ListRec.FtpList);
    end;
    Dispose(ListRec);
  end;
end;

function FsExecuteFileW(MainWin: THandle; RemoteName, Verb: PWideChar): Integer; dcpcall;
var
  FtpSend: TFTPSendEx;
  asFileName: AnsiString;
  wsFileName: UnicodeString;
begin
  Result:= FS_EXEC_YOURSELF;
  if (RemoteName = '') then Exit;

  if Verb = 'open' then
    begin
      if (ExtractFileDir(RemoteName) = PathDelim) then // root path
      begin
        asFileName:= UTF16ToUTF8(RemoteName + 1);
        if RemoteName[1] <> '<' then // connection
          begin
            if not FtpConnect(asFileName, FtpSend) then
              Result := FS_EXEC_OK
            else
              begin
                wsFileName := FtpSend.ServerToClient(FtpSend.GetCurrentDir);
                wsFileName := SetDirSeparators(RemoteName + wsFileName);
                StrPLCopy(RemoteName, wsFileName, MAX_PATH);
                Result := FS_EXEC_SYMLINK;
              end;
          end
        else  // special item
          begin
            if asFileName = cAddConnection then
              begin
                AddConnection;
                Result:= FS_EXEC_OK;
              end
            else if asFileName = cQuickConnection then
              begin
                if QuickConnection then
                  Result := FS_EXEC_SYMLINK
                else
                  Result := FS_EXEC_OK;
              end;
          end;
      end; // root path
    end // Verb = open
  else if Pos('chmod', UnicodeString(Verb)) = 1 then
    begin
      if GetConnectionByPath(RemoteName, FtpSend, asFileName) then
      begin
        if FtpSend.ChangeMode(asFileName, AnsiString(Copy(Verb, 7, MaxInt))) then
          Result:= FS_EXEC_OK
        else
          Result := FS_EXEC_ERROR;
      end;
    end
  else if Pos('quote', UnicodeString(Verb)) = 1 then
    begin
      if GetConnectionByPath(RemoteName, FtpSend, asFileName) then
      begin
        asFileName:= FtpSend.ClientToServer(Verb);
        if FtpSend.ExecuteCommand(Copy(asFileName, 7, MaxInt)) then
          Result := FS_EXEC_OK
        else
          Result := FS_EXEC_ERROR;
      end;
    end
  else if Verb = 'properties' then
    begin
      if (ExtractFileDir(RemoteName) = PathDelim) and not (RemoteName[1] in [#0, '<']) then // connection
      begin
        EditConnection(UTF16ToUTF8(RemoteName + 1));
      end;
      Result:= FS_EXEC_OK;
    end;
end;

function FsRenMovFileW(OldName, NewName: PWideChar; Move, OverWrite: BOOL;
  RemoteInfo: pRemoteInfo): Integer; dcpcall;
var
  O, N: Integer;
  FtpSend: TFTPSendEx;
  sOldName: AnsiString;
  sNewName: AnsiString;
  Connection: TConnection;
begin
  Result := FS_FILE_NOTSUPPORTED;

  if not Move then
  begin
    if (ExtractFileDir(OldName) = PathDelim) and (WideChar(OldName[1]) <> '<') and
       (ExtractFileDir(NewName) = PathDelim) and (WideChar(NewName[1]) <> '<') then
    begin
      O:= ConnectionList.IndexOf(OldName + 1);
      if O < 0 then
        Result:= FS_FILE_NOTFOUND
      else begin
        sNewName:= RepairConnectionName(UTF16ToUTF8(UnicodeString(NewName + 1)));
        N:= ConnectionList.IndexOf(sNewName);
        if (N >= 0) then
        begin
          if not OverWrite then Exit(FS_FILE_EXISTS);
          Connection:= TConnection(ConnectionList.Objects[N]);
        end
        else begin
          Connection:= TConnection.Create;
          ConnectionList.AddObject(sNewName, Connection);
        end;
        Connection.Assign(TConnection(ConnectionList.Objects[O]));
        Connection.ConnectionName:= sNewName;

        WriteConnectionList;
        Result:= FS_FILE_OK;
      end;
    end;
    Exit;
  end;

  if (ExtractFileDir(OldName) = PathDelim) and (WideChar(OldName[1]) <> '<') then
    begin
      O:= ConnectionList.IndexOf(OldName + 1);
      if O < 0 then
        Result:= FS_FILE_NOTFOUND
      else
        begin
          ConnectionList[O]:= RepairConnectionName(UTF16ToUTF8(UnicodeString(NewName + 1)));
          TConnection(ConnectionList.Objects[O]).ConnectionName:= ConnectionList[O];
          WriteConnectionList;
          Result:= FS_FILE_OK;
        end;
    end
  else if GetConnectionByPath(OldName, FtpSend, sOldName) then
    begin
      sNewName := FtpSend.ClientToServer(NewName);
      sNewName := ExtractRemoteFileName(sNewName);
      ProgressProc(PluginNumber, OldName, NewName, 0);
      if FtpSend.RenameFile(sOldName, sNewName) then
      begin
        ProgressProc(PluginNumber, OldName, NewName, 100);
        Result := FS_FILE_OK;
      end;
    end;
end;

function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: Integer;
  RemoteInfo: pRemoteInfo): Integer; dcpcall;
var
  FileSize: Int64;
  FtpSend: TFTPSendEx;
  sFileName: AnsiString;
  FileName: AnsiString;
begin
  Result := FS_FILE_READERROR;
  if GetConnectionByPath(RemoteName, FtpSend, sFileName) then
  try
    FileName:= UTF16ToUTF8(UnicodeString(LocalName));
    if FileExistsUTF8(FileName) and (CopyFlags and FS_COPYFLAGS_FORCE = 0) then
    begin
      if not FtpSend.CanResume then Exit(FS_FILE_EXISTS);
      Exit(FS_FILE_EXISTSRESUMEALLOWED);
    end;
    FtpSend.DataStream.Clear;
    FtpSend.DirectFileName := FileName;
    Int64Rec(FileSize).Lo := RemoteInfo^.SizeLow;
    Int64Rec(FileSize).Hi := RemoteInfo^.SizeHigh;
    ProgressProc(PluginNumber, RemoteName, LocalName, 0);
    if FtpSend.RetrieveFile(sFileName, FileSize, (CopyFlags and FS_COPYFLAGS_RESUME) <> 0) then
    begin
      ProgressProc(PluginNumber, RemoteName, LocalName, 100);
      Result := FS_FILE_OK;
    end;
  except
    on EUserAbort do Result := FS_FILE_USERABORT;
    on EFOpenError do Result := FS_FILE_READERROR;
    else Result := FS_FILE_WRITEERROR;
  end;
end;

function FsPutFileW(LocalName, RemoteName: PWideChar; CopyFlags: Integer): Integer; dcpcall;
var
  FtpSend: TFTPSendEx;
  sFileName: AnsiString;
  FileName: AnsiString;
begin
  Result := FS_FILE_WRITEERROR;
  if GetConnectionByPath(RemoteName, FtpSend, sFileName) then
  try
    FileName:= UTF16ToUTF8(UnicodeString(LocalName));
    if (CopyFlags and FS_COPYFLAGS_FORCE = 0) and (FtpSend.FileExists(sFileName)) then
    begin
      if not FtpSend.CanResume then Exit(FS_FILE_EXISTS);
      Exit(FS_FILE_EXISTSRESUMEALLOWED);
    end;
    FtpSend.DataStream.Clear;
    FtpSend.DirectFileName := FileName;
    ProgressProc(PluginNumber, LocalName, RemoteName, 0);
    if FtpSend.StoreFile(sFileName, (CopyFlags and FS_COPYFLAGS_RESUME) <> 0) then
    begin
      ProgressProc(PluginNumber, LocalName, RemoteName, 100);
      Result := FS_FILE_OK;
    end;
  except
    on EReadError do Result := FS_FILE_READERROR;
    on EUserAbort do Result := FS_FILE_USERABORT;
    else Result := FS_FILE_WRITEERROR;
  end;
end;

function FsDeleteFileW(RemoteName: PWideChar): BOOL; dcpcall;
var
  FtpSend: TFTPSendEx;
  sFileName: AnsiString;
begin
  Result := False;
  // if root path then delete connection
  if (ExtractFileDir(RemoteName) = PathDelim) and (RemoteName[1] <> '<') then
    Result:= DeleteConnection(ExtractConnectionName(UTF16ToUTF8(RemoteName)))
  else if GetConnectionByPath(RemoteName, FtpSend, sFileName) then
    Result := FtpSend.DeleteFile(sFileName);
end;

function FsMkDirW(RemoteDir: PWideChar): BOOL; dcpcall;
var
  sPath: AnsiString;
  FtpSend: TFTPSendEx;
begin
  Result := False;
  if GetConnectionByPath(RemoteDir, FtpSend, sPath) then
    Result := FtpSend.CreateDir(sPath);
end;

function FsRemoveDirW(RemoteName: PWideChar): BOOL; dcpcall;
var
  sPath: AnsiString;
  FtpSend: TFTPSendEx;
begin
  Result := False;
  if GetConnectionByPath(RemoteName, FtpSend, sPath) then
    Result := FtpSend.DeleteDir(sPath);
end;

function FsSetTimeW(RemoteName: PWideChar; CreationTime, LastAccessTime,
  LastWriteTime: PFileTime): BOOL; dcpcall;
var
  sPath: AnsiString;
  FtpSend: TFTPSendEx;
begin
  if (LastAccessTime = nil) and (LastWriteTime = nil) then
    Result := False
  else if GetConnectionByPath(RemoteName, FtpSend, sPath) then
    Result := FtpSend.FsSetTime(sPath, LastAccessTime, LastWriteTime)
  else begin
    Result := False;
  end;
end;

function FsDisconnectW(DisconnectRoot: PWideChar): BOOL; dcpcall;
var
  Index: Integer;
  asTemp: AnsiString;
  FtpSend: TFTPSendEx;
  wsTemp: UnicodeString;
begin
  wsTemp := ExcludeLeadingPathDelimiter(DisconnectRoot);
  asTemp := ExtractConnectionName(UTF16ToUTF8(wsTemp));

  Index := ActiveConnectionList.IndexOf(asTemp);
  Result := (Index >= 0);

  if Result then
  begin
    FtpSend:= TFTPSendEx(ActiveConnectionList.Objects[Index]);

    if not FtpSend.NetworkError then
    begin
      FtpSend.Logout;
    end;
    ActiveConnectionList.Delete(Index);

    Index:= ConnectionList.IndexOf(asTemp);
    if Index >= 0 then begin
      ZeroPassword(TConnection(ConnectionList.Objects[Index]).CachedPassword);
    end;
    LogProc(PluginNumber, MSGTYPE_DISCONNECT, PWideChar('DISCONNECT ' + DisconnectRoot));

    FreeAndNil(FtpSend);
  end;
end;

procedure FsSetCryptCallbackW(pCryptProc: TCryptProcW; CryptoNr, Flags: Integer); dcpcall;
begin
  CryptProc:= pCryptProc;
  CryptoNumber:= CryptoNr;
end;

procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer); dcpcall;
begin
  StrPLCopy(DefRootName, 'FTP', MaxLen);
end;

procedure FsSetDefaultParams(dps: pFsDefaultParamStruct); dcpcall;
begin
  ConnectionList := TStringList.Create;
  ActiveConnectionList := TStringList.Create;
  DefaultIniName:= ExtractFileName(dps.DefaultIniName);
end;

procedure FsStatusInfoW(RemoteDir: PWideChar; InfoStartEnd, InfoOperation: Integer); dcpcall;
var
  FtpSend: TFtpSendEx;
  RemotePath: AnsiString;
begin
  if (InfoOperation in [FS_STATUS_OP_GET_MULTI_THREAD, FS_STATUS_OP_PUT_MULTI_THREAD]) then
  begin
    if InfoStartEnd = FS_STATUS_START then
    begin
      if GetConnectionByPath(RemoteDir, FtpSend, RemotePath) then
      begin
        LogProc(PluginNumber, msgtype_details, 'Create background connection');
        ThreadCon:= FtpSend.Clone;
        if not ThreadCon.Login then
        begin
          FreeAndNil(ThreadCon);
          LogProc(PluginNumber, msgtype_importanterror, 'Cannot create background connection, use foreground');
        end;
      end;
    end
    else if Assigned(ThreadCon) then
    begin
      LogProc(PluginNumber, msgtype_details, 'Destroy background connection');
      ThreadCon.Logout;
      FreeAndNil(ThreadCon);
    end;
  end;
end;

function FsGetBackgroundFlags: Integer; dcpcall;
begin
  Result:= BG_DOWNLOAD or BG_UPLOAD or BG_ASK_USER;
end;

{
procedure FsNetworkGetSupportedProtocols(Protocols: PAnsiChar; MaxLen: LongInt); dcpcall;
begin
  StrPLCopy(Protocols, ftpProtocol, MaxLen);
end;

function FsNetworkGetConnection(Index: LongInt; Connection: PAnsiChar;
  MaxLen: LongInt): LongBool; dcpcall;
begin
  Result:= False;
  if Index >= ConnectionList.Count then Exit;
  StrPLCopy(Connection, TConnection(ConnectionList.Objects[Index]).ConnectionName, MaxLen);
  Result:= True;
end;

function FsNetworkManageConnection(MainWin: HWND; Connection: PAnsiChar; Action: LongInt;
  MaxLen: LongInt): LongBool; dcpcall;
var
  I: Integer;
begin
  Result:= False;
  case Action of
  FS_NM_ACTION_ADD:
    begin
      I:= AddConnection;
      if I >= 0 then
      begin
        StrPLCopy(Connection, ConnectionList[I], MaxLen);
        Result:= True;
      end;
    end;
  FS_NM_ACTION_EDIT:
    begin
      I:= ConnectionList.IndexOf(Connection);
      if I >= 0 then
      begin
        if EditConnection(Connection) then
          begin
            StrPLCopy(Connection, ConnectionList[I], MaxLen);
            Result:= True;
          end;
      end;
    end;
  FS_NM_ACTION_DELETE:
    Result:= DeleteConnection(Connection);
  end;
end;

function FsNetworkOpenConnection(Connection: PAnsiChar; RootDir, RemotePath: PAnsiChar;
  MaxLen: LongInt): LongBool; dcpcall;
var
  I: Integer;
  FtpSend: TFTPSendEx;
  Con: TConnection;
begin
  Result:= False;
  if Connection = #0 then
    begin
      if QuickConnection then
      begin
        I:= ActiveConnectionList.IndexOf(cQuickConnection);
        if I >= 0 then
          begin
            Con:= TConnection(ActiveConnectionList.Objects[I]);
            StrPLCopy(Connection, ftpProtocol + Con.Host, MaxLen);
            StrPLCopy(RootDir, PathDelim + Con.ConnectionName, MaxLen);
            StrPLCopy(RemotePath, Con.Path, MaxLen);
            Result:= True;
          end;
      end;
    end
  else if FtpConnect(Connection, FtpSend) then
    begin
      I:= ConnectionList.IndexOf(Connection);
      if I >= 0 then
        begin
          Con:= TConnection(ConnectionList.Objects[I]);
          StrPLCopy(Connection, ftpProtocol + Con.Host, MaxLen);
          StrPLCopy(RootDir, PathDelim + Con.ConnectionName, MaxLen);
          StrPLCopy(RemotePath, Con.Path, MaxLen);
          Result:= True;
        end;
    end;
end;
}

procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo);
begin
  gStartupInfo:= StartupInfo^;

  DefaultIniName:= gStartupInfo.PluginConfDir + DefaultIniName;
  IniFile := TIniFileEx.Create(DefaultIniName, fmOpenReadWrite);

  // Use TCP keep alive for all connections: Useful for certain
  // firewalls/router if the connection breaks very often.
  TcpKeepAlive := IniFile.ReadBool('General', 'TcpKeepAlive', TcpKeepAlive);

  ReadConnectionList;
end;

function ReadPassword(ConnectionName: AnsiString; out Password: AnsiString): Boolean;
begin
  Password:= EmptyStr;
  case CryptFunc(FS_CRYPT_LOAD_PASSWORD, ConnectionName, Password) of
    FS_FILE_OK,
    FS_FILE_READERROR:
      Result:= True
    else
      Result:= False;
  end;
end;

function DeletePassword(ConnectionName: AnsiString): Boolean;
var
  Password: String = '';
begin
  Result:= CryptFunc(FS_CRYPT_DELETE_PASSWORD, ConnectionName, Password) = FS_FILE_OK;
end;

{ TConnection }

procedure TConnection.Assign(Connection: TConnection);
begin
  Path:= Connection.Path;
  Host:= Connection.Host;
  Port:= Connection.Port;
  Proxy:= Connection.Proxy;
  AutoTLS:= Connection.AutoTLS;
  FullSSL:= Connection.FullSSL;
  OpenSSH:= Connection.OpenSSH;
  CopySCP:= Connection.CopySCP;
  OnlySCP:= Connection.OnlySCP;
  UserName:= Connection.UserName;
  Password:= Connection.Password;
  Encoding:= Connection.Encoding;
  PublicKey:= Connection.PublicKey;
  PrivateKey:= Connection.PrivateKey;
  Fingerprint:= Connection.Fingerprint;
  PassiveMode:= Connection.PassiveMode;
  UseAllocate:= Connection.UseAllocate;
  InitCommands:= Connection.InitCommands;
  MasterPassword:= Connection.MasterPassword;
  ConnectionName:= Connection.ConnectionName;
  PasswordChanged:= Connection.PasswordChanged;
  ShowHiddenItems:= Connection.ShowHiddenItems;
  KeepAliveTransfer:= Connection.KeepAliveTransfer;
end;

initialization
  ListLock := syncobjs.TCriticalSection.Create;

finalization
  FreeAndNil(ListLock);

end.
