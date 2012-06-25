{
   Double commander
   -------------------------------------------------------------------------
   Wfx plugin for working with File Transfer Protocol

   Copyright (C) 2009-2012  Koblov Alexander (Alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit FtpFunc;

{$mode delphi}{$H+}
{$include calling.inc}

interface

uses
  SysUtils, Classes,
  WfxPlugin, FtpSend, Extension;

type

  { TFTPListRecEx }

  TFTPListRecEx = class(TFTPListRec)
  public
    procedure Assign(Value: TFTPListRec); override;
  end;

  { TFTPListEx }

  TFTPListEx = class(TFTPList)
  public
    procedure Assign(Value: TFTPList); override;
  end;

  { TFTPSendEx }

  TFTPSendEx = class(TFTPSend)
  public
    procedure FTPStatus(Sender: TObject; Response: Boolean; const Value: String);
  end;

  TConnection = class
  public
    ConnectionName, Path, Host: AnsiString;
    Port: AnsiString;
    UserName: AnsiString;
    Password: AnsiString;
    MasterPassword: Boolean;
    PassiveMode: Boolean;
    InitCommands: AnsiString;
    PasswordChanged: Boolean;
  end;

function FsInit(PluginNr: Integer; pProgressProc: TProgressProc;
  pLogProc: TLogProc; pRequestProc: TRequestProc): Integer; dcpcall;

function FsFindFirst(Path: PAnsiChar; var FindData: TWin32FindData): THandle; dcpcall;
function FsFindNext(Hdl: THandle; var FindData: TWin32FindData): BOOL; dcpcall;
function FsFindClose(Hdl: THandle): Integer; dcpcall;

function FsExecuteFile(MainWin: THandle; RemoteName, Verb: PAnsiChar): Integer; dcpcall;
function FsRenMovFile(OldName, NewName: PAnsiChar; Move, OverWrite: BOOL;
  RemoteInfo: pRemoteInfo): Integer; dcpcall;
function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: Integer;
  RemoteInfo: pRemoteInfo): Integer; dcpcall;
function FsPutFile(LocalName, RemoteName: PAnsiChar; CopyFlags: Integer)
  : Integer; dcpcall;
function FsDeleteFile(RemoteName: PAnsiChar): BOOL; dcpcall;

function FsMkDir(RemoteDir: PAnsiChar): BOOL; dcpcall;
function FsRemoveDir(RemoteName: PAnsiChar): BOOL; dcpcall;

function FsDisconnect(DisconnectRoot: PAnsiChar): BOOL; dcpcall;

procedure FsSetCryptCallback(pCryptProc: TCryptProc; CryptoNr, Flags: Integer); dcpcall;
procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer); dcpcall;
procedure FsSetDefaultParams(dps: pFsDefaultParamStruct); dcpcall;
{ Network API }
{
procedure FsNetworkGetSupportedProtocols(Protocols: PAnsiChar; MaxLen: LongInt); dcpcall;
function FsNetworkGetConnection(Index: LongInt; Connection: PAnsiChar; MaxLen: LongInt): LongBool; dcpcall;
function FsNetworkManageConnection(MainWin: HWND; Connection: PAnsiChar; Action: LongInt; MaxLen: LongInt): LongBool; dcpcall;
function FsNetworkOpenConnection(Connection: PAnsiChar; RootDir, RemotePath: PAnsiChar; MaxLen: LongInt): LongBool; dcpcall;
}
{ Extension API }
procedure ExtensionInitialize(StartupInfo: PExtensionStartupInfo); dcpcall;

function ReadPassword(ConnectionName: UTF8String): AnsiString;
function DeletePassword(ConnectionName: UTF8String): Boolean;

var
  gStartupInfo: TExtensionStartupInfo;
  gConnection: TConnection;

implementation

uses
  IniFiles, StrUtils, FtpUtils, FtpConfDlg, syncobjs;

var
  ActiveConnectionList, ConnectionList: TStringList;
  IniFile: TIniFile;
  ProgressProc: TProgressProc;
  LogProc: TLogProc;
  RequestProc: TRequestProc;
  PluginNumber: Integer;
  CryptProc: TCryptProc;
  CryptoNumber: Integer;
  HasDialogAPI: Boolean = False;
  ListLock: TCriticalSection;

const
  cAddConnection = '<Add connection>';
  cQuickConnection = '<Quick connection>';
  RootList: array [0 .. 1] of AnsiString = (cAddConnection, cQuickConnection);

type
  TListRec = record
    Path: AnsiString;
    Index: Integer;
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
    Connection.PassiveMode:= IniFile.ReadBool('FTP', 'Connection' + sIndex + 'PassiveMode', True);
    Connection.InitCommands := IniFile.ReadString('FTP', 'Connection' + sIndex + 'InitCommands', EmptyStr);
    // add connection to connection list
    ConnectionList.AddObject(Connection.ConnectionName, Connection);
  end;
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
    IniFile.WriteBool('FTP', 'Connection' + sIndex + 'PassiveMode', Connection.PassiveMode);
    IniFile.WriteString('FTP', 'Connection' + sIndex + 'InitCommands', Connection.InitCommands);
  end;
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

function CryptFunc(Mode: LongInt; ConnectionName: AnsiString; var Password: AnsiString): LongInt;
begin
  if (Mode = FS_CRYPT_LOAD_PASSWORD) or (Mode = FS_CRYPT_LOAD_PASSWORD_NO_UI) then
    SetLength(Password, MAX_PATH);
  Result:= CryptProc(PluginNumber, CryptoNumber, Mode, PAnsiChar(ConnectionName), PAnsiChar(Password), MAX_PATH);
end;

function ShowPasswordDialog(out Password: AnsiString): Boolean;
begin
  SetLength(Password, MAX_PATH);
  Password[1] := #0;
  Result := RequestProc(PluginNumber, RT_Password, nil, nil, PAnsiChar(Password), MAX_PATH);
  if Result then
    Password:= PAnsiChar(Password) // truncate to #0
  else
    Password := '';
end;

function FtpConnect(const ConnectionName: AnsiString; out FtpSend: TFTPSendEx): Boolean;
var
  I: Integer;
  Connection: TConnection;
  sTemp: AnsiString;
begin
  Result:= False;
  I:= ActiveConnectionList.IndexOf(ConnectionName);
  // If find active connection then use it
  if I >= 0 then
    begin
      FtpSend:= TFTPSendEx(ActiveConnectionList.Objects[I]);
      Result:= True;
    end
  else
    begin
      // find in exists connection list
      I:= ConnectionList.IndexOf(ConnectionName);
      if I >= 0 then
      begin
        Connection := TConnection(ConnectionList.Objects[I]);
        FtpSend := TFTPSendEx.Create;
        FtpSend.OnStatus:= FtpSend.FTPStatus;
        FtpSend.TargetHost := Connection.Host;
        FtpSend.PassiveMode:= Connection.PassiveMode;
        if Connection.Port <> EmptyStr then
          FtpSend.TargetPort := Connection.Port;
        if Connection.UserName <> EmptyStr then
          FtpSend.UserName := Connection.UserName;
        if Connection.MasterPassword then
        begin
          if CryptFunc(FS_CRYPT_LOAD_PASSWORD, Connection.ConnectionName, Connection.Password) <> FS_FILE_OK then
            Connection.Password:= EmptyStr;
        end;
        if Connection.Password = EmptyStr then // if no saved password then ask it
        begin
          if not ShowPasswordDialog(Connection.Password) then
          begin
            FreeAndNil(FtpSend);
            Exit;
          end;
        end;
        FtpSend.Password := Connection.Password;
        // try to connect
        if FtpSend.Login then
          begin
            LogProc(PluginNumber, MSGTYPE_CONNECT, PAnsiChar('CONNECT ' + ConnectionName));
            sTemp:= Connection.InitCommands;
            while sTemp <> EmptyStr do
              FtpSend.FTPCommand(Copy2SymbDel(sTemp, ';'));
            ActiveConnectionList.AddObject(ConnectionName, FtpSend);
            Result:= True;
          end
        else
          begin
            RequestProc(PluginNumber, RT_MsgOK, nil, 'Can not connect to the server!', nil, MAX_PATH);
            FreeAndNil(FtpSend);
            Exit;
          end;
      end;
    end;
end;

function AddQuickConnection(const Connection: TConnection): Boolean;
var
  Temp: AnsiString;
begin
  Result:= False;
  SetLength(Temp, MAX_PATH);
  Temp[1]:= #0;
  if RequestProc(PluginNumber, RT_URL, nil, nil, PAnsiChar(Temp), MAX_PATH) then
  begin
    Connection.Host := Temp;
    Temp[1]:= #0;
    if RequestProc(PluginNumber, RT_TargetDir, nil, nil, PAnsiChar(Temp), MAX_PATH) then
    begin
      Connection.Path := Temp;
      Temp[1]:= #0;
      if RequestProc(PluginNumber, RT_UserName, nil, nil, PAnsiChar(Temp), MAX_PATH) then
      begin
        Connection.UserName := Temp;
        Result:= True;
      end;
    end;
  end;
end;

function QuickConnection: Boolean;
var
  I: Integer;
  Connection: TConnection;
  FtpSend: TFTPSendEx;
begin
  Result:= False;
  Connection := TConnection.Create;
  if AddQuickConnection(Connection) then
    begin
      if ShowPasswordDialog(Connection.Password) then
      begin
        Connection.ConnectionName:= Connection.Host;

        I:= ConnectionList.AddObject(Connection.ConnectionName, Connection);
        Result:= FtpConnect(Connection.ConnectionName, FtpSend);
        ConnectionList.Delete(I);
      end;
    end;

  if not Result then
    Connection.Free;
end;

function AddConnection: Integer;
var
  Temp: AnsiString;
  bCancel: Boolean;
begin
  Result:= -1;
  bCancel := True;
  gConnection := TConnection.Create;

  if HasDialogAPI then
    begin
      if ShowFtpConfDlg then
        with gConnection do
        begin
          if MasterPassword then
          begin
            if CryptFunc(FS_CRYPT_SAVE_PASSWORD, ConnectionName, Password) = FS_FILE_OK then
              Password:= EmptyStr;
          end;
          Result:= ConnectionList.AddObject(ConnectionName, gConnection);
          bCancel := False;
        end;
    end
  else
    begin
      SetLength(Temp, MAX_PATH);
      Temp[1]:= #0;
      if RequestProc(PluginNumber, RT_Other, nil, nil, PAnsiChar(Temp), MAX_PATH) then
      begin
        gConnection.ConnectionName := Temp;
        if AddQuickConnection(gConnection) then
        begin
          Result:= ConnectionList.AddObject(gConnection.ConnectionName, gConnection);
          bCancel := False;
        end;
      end;
    end;

  if bCancel then
    FreeAndNil(gConnection)
  else
    WriteConnectionList;
end;

function EditConnection(ConnectionName: AnsiString): Boolean;
var
  I: Integer;
begin
  Result:= False;
  if HasDialogAPI then
    begin
      I := ConnectionList.IndexOf(ConnectionName);
      if I >= 0 then
        begin
          gConnection:= TConnection(ConnectionList.Objects[I]);
          if ShowFtpConfDlg then
            begin
              with gConnection do
              if MasterPassword and PasswordChanged then
                begin
                  if CryptFunc(FS_CRYPT_SAVE_PASSWORD, ConnectionName, Password) = FS_FILE_OK then
                    Password:= EmptyStr;
                end;
              WriteConnectionList;
              Result:= True;
            end;
          gConnection:= nil;
        end;
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

function GetConnectionByPath(const sPath: AnsiString; out FtpSend: TFTPSendEx;
  out RemotePath: AnsiString): Boolean;
var
  sConnName: AnsiString;
begin
  Result := False;
  if (ExtractFileDir(sPath) = PathDelim) then Exit;
  sConnName := ExtractConnectionName(sPath);
  RemotePath := ExtractRemoteFileName(sPath);
  Result:= FtpConnect(sConnName, FtpSend);
end;

function LocalFindNext(Hdl: THandle; var FindData: TWin32FindData): Boolean;
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
    StrPCopy(FindData.cFileName, RootList[I]);
    FindData.dwFileAttributes := 0;
    Inc(ListRec^.Index);
    Result := True;
  end
  else if I - RootCount < ConnectionList.Count then
  begin
    Connection := TConnection(ConnectionList.Objects[I - RootCount]);
    StrPCopy(FindData.cFileName, Connection.ConnectionName);
    FindData.dwFileAttributes := FILE_ATTRIBUTE_NORMAL;
    Inc(ListRec^.Index);
    Result := True;
  end;
end;

function RemoteFindNext(Hdl: THandle; var FindData: TWin32FindData): Boolean;
var
  ListRec: PListRec absolute Hdl;
  I: Integer;
begin
  Result := False;
  if Assigned(ListRec^.FtpList) then
    with ListRec^ do
    begin
      I := Index;
      if I < FtpList.Count then
      begin
        FillChar(FindData, SizeOf(FindData), 0);
        StrPCopy(FindData.cFileName, FtpList.Items[I].FileName);
        FindData.dwFileAttributes := FindData.dwFileAttributes or FILE_ATTRIBUTE_UNIX_MODE;
        if FtpList.Items[I].Directory then
          FindData.dwFileAttributes := FindData.dwFileAttributes or FILE_ATTRIBUTE_DIRECTORY
        else
          begin
            FindData.nFileSizeLow := (FtpList.Items[I].FileSize and MAXDWORD);
            FindData.nFileSizeHigh := (FtpList.Items[I].FileSize shr $20);
          end;
        // set Unix permissions
        FindData.dwReserved0 := ModeStr2Mode(FtpList.Items[I].Permission);
        FindData.ftLastWriteTime := DateTimeToFileTime(FtpList.Items[I].FileTime);
        Inc(ListRec^.Index);
        Result := True;
      end;
    end;
end;

function FsInit(PluginNr: Integer; pProgressProc: TProgressProc;
  pLogProc: TLogProc; pRequestProc: TRequestProc): Integer; dcpcall;
begin
  ProgressProc := pProgressProc;
  LogProc := pLogProc;
  RequestProc := pRequestProc;
  PluginNumber := PluginNr;
  ActiveConnectionList := TStringList.Create;
  ConnectionList := TStringList.Create;

  Result := 0;
end;

function FsFindFirst(Path: PAnsiChar; var FindData: TWin32FindData): THandle; dcpcall;
var
  ListRec: PListRec;
  sPath: AnsiString;
  FtpSend: TFTPSendEx;
begin
  New(ListRec);
  ListRec.Path := Path;
  ListRec.Index := 0;
  ListRec.FtpList:= nil;
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
        if GetConnectionByPath(IncludeTrailingPathDelimiter(Path), FtpSend, sPath) then
        begin
          // Get directory listing
          if FtpSend.List(sPath, False) then
          begin
            if FtpSend.FtpList.Count > 0 then
            begin
              ListRec.FtpList:= TFTPListEx.Create;
              // Save file list
              ListRec.FtpList.Assign(FtpSend.FtpList);
              Result := THandle(ListRec);
              RemoteFindNext(Result, FindData);
            end;
          end;
        end;
      finally
        ListLock.Release;
        if Result = wfxInvalidHandle then
          Dispose(ListRec);
      end;
    end;
end;

function FsFindNext(Hdl: THandle; var FindData: TWin32FindData): BOOL; dcpcall;
var
  ListRec: PListRec absolute Hdl;
begin
  if ListRec.Path = PathDelim then
    Result := LocalFindNext(Hdl, FindData)
  else
    Result := RemoteFindNext(Hdl, FindData);
end;

function FsFindClose(Hdl: THandle): Integer; dcpcall;
var
  ListRec: PListRec absolute Hdl;
begin
  if Assigned(ListRec) then
  begin
    if Assigned(ListRec^.FtpList) then
      FreeAndNil(ListRec^.FtpList);
    Dispose(ListRec);
  end;
  Result:= 0;
end;

function FsExecuteFile(MainWin: THandle; RemoteName, Verb: PAnsiChar): Integer; dcpcall;
var
  FtpSend: TFTPSendEx;
  sFileName: AnsiString;
begin
  Result:= FS_EXEC_YOURSELF;
  if (RemoteName = '') then Exit;

  if Verb = 'open' then
    begin
      if (ExtractFileDir(RemoteName) = PathDelim) then // root path
      begin
        if RemoteName[1] <> '<' then // connection
          begin
            if FtpConnect(RemoteName + 1, FtpSend) then
              Result := FS_EXEC_SYMLINK
            else
              Result := FS_EXEC_OK;
          end
        else  // special item
          begin
            if (RemoteName + 1) = cAddConnection then
              begin
                AddConnection;
                Result:= FS_EXEC_OK;
              end
            else if (RemoteName + 1) = cQuickConnection then
              begin
                if QuickConnection then
                  Result := FS_EXEC_SYMLINK
                else
                  Result := FS_EXEC_OK;
              end;
          end;
      end; // root path
    end // Verb = open
  else if Pos('chmod', Verb) > 0 then
    begin
      if GetConnectionByPath(RemoteName, FtpSend, sFileName) then
      begin
        if (FtpSend.FTPCommand('SITE' + #32 + Verb + #32 + sFileName) div 100) = 2 then
          Result:= FS_EXEC_OK
        else
          Result := FS_EXEC_ERROR;
      end;
    end
  else if Verb = 'properties' then
    if (ExtractFileDir(RemoteName) = PathDelim) and (RemoteName[1] <> '<') then // connection
      begin
        EditConnection(RemoteName + 1);
        Result:= FS_EXEC_OK;
      end;
end;

function FsRenMovFile(OldName, NewName: PAnsiChar; Move, OverWrite: BOOL;
  RemoteInfo: pRemoteInfo): Integer; dcpcall;
var
  I: Integer;
  FtpSend: TFTPSendEx;
  sOldName, sNewName: AnsiString;
begin
  Result := FS_FILE_NOTSUPPORTED;

  if not Move then Exit;

  if (ExtractFileDir(OldName) = PathDelim) and (AnsiChar(OldName[1]) <> '<') then
    begin
      I:= ConnectionList.IndexOf(OldName + 1);
      if I < 0 then
        Result:= FS_FILE_NOTFOUND
      else
        begin
          TConnection(ConnectionList.Objects[I]).ConnectionName:= ExtractFileName(NewName);
          WriteConnectionList;
          Result:= FS_FILE_OK;
        end;
    end
  else if GetConnectionByPath(OldName, FtpSend, sOldName) then
    begin
      sNewName := ExtractRemoteFileName(NewName);
      ProgressProc(PluginNumber, OldName, NewName, 0);
      if FtpSend.RenameFile(sOldName, sNewName) then
        begin
          ProgressProc(PluginNumber, OldName, NewName, 100);
          Result := FS_FILE_OK;
        end;
    end;
end;

function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: Integer;
  RemoteInfo: pRemoteInfo): Integer; dcpcall;
var
  sFileName: AnsiString;
  FtpSend: TFTPSendEx;
begin
  Result := FS_FILE_READERROR;
  if GetConnectionByPath(RemoteName, FtpSend, sFileName) then
  begin
    FtpSend.DataStream.Clear;
    ProgressProc(PluginNumber, RemoteName, LocalName, 0);
    if FtpSend.RetrieveFile(sFileName, (CopyFlags and FS_COPYFLAGS_RESUME) <> 0) then
      try
        FtpSend.DataStream.SaveToFile(LocalName);
        ProgressProc(PluginNumber, RemoteName, LocalName, 100);
        Result := FS_FILE_OK;
      except
        on EFCreateError do
          Result := FS_FILE_WRITEERROR;
        on EWriteError do
          Result := FS_FILE_WRITEERROR;
      end;
  end;
end;

function FsPutFile(LocalName, RemoteName: PAnsiChar; CopyFlags: Integer)
  : Integer; dcpcall;
var
  sFileName: AnsiString;
  FtpSend: TFTPSendEx;
begin
  Result := FS_FILE_WRITEERROR;
  if GetConnectionByPath(RemoteName, FtpSend, sFileName) then
  begin
    FtpSend.DataStream.Clear;
    try
      ProgressProc(PluginNumber, LocalName, RemoteName, 0);
      FtpSend.DataStream.LoadFromFile(LocalName);
    except
      on EFOpenError do
      begin
        Result := FS_FILE_NOTFOUND;
        Exit;
      end;
      on EReadError do
      begin
        Result := FS_FILE_READERROR;
        Exit;
      end;
    end;
    if FtpSend.StoreFile(sFileName, (CopyFlags and FS_COPYFLAGS_RESUME) <> 0) then
      begin
        ProgressProc(PluginNumber, LocalName, RemoteName, 100);
        Result := FS_FILE_OK;
      end;
  end;
end;

function FsDeleteFile(RemoteName: PAnsiChar): BOOL; dcpcall;
var
  sFileName: AnsiString;
  FtpSend: TFTPSendEx;
begin
  Result := False;
  // if root path then delete connection
  if (ExtractFileDir(RemoteName) = PathDelim) and (AnsiChar(RemoteName[1]) <> '<') then
    Result:= DeleteConnection(ExtractConnectionName(RemoteName))
  else if GetConnectionByPath(RemoteName, FtpSend, sFileName) then
    Result := FtpSend.DeleteFile(sFileName);
end;

function FsMkDir(RemoteDir: PAnsiChar): BOOL; dcpcall;
var
  sPath: AnsiString;
  FtpSend: TFTPSendEx;
begin
  Result := False;
  if GetConnectionByPath(RemoteDir, FtpSend, sPath) then
    Result := FtpSend.CreateDir(sPath);
end;

function FsRemoveDir(RemoteName: PAnsiChar): BOOL; dcpcall;
var
  sPath: AnsiString;
  FtpSend: TFTPSendEx;
begin
  Result := False;
  if GetConnectionByPath(RemoteName, FtpSend, sPath) then
    Result := FtpSend.DeleteDir(sPath);
end;

function FsDisconnect(DisconnectRoot: PAnsiChar): BOOL; dcpcall;
var
  FtpSend: TFTPSendEx;
  sTemp: AnsiString;
begin
  Result := False;
  if GetConnectionByPath(DisconnectRoot, FtpSend, sTemp) then
  begin
    Result := FtpSend.Logout;
    LogProc(PluginNumber, MSGTYPE_DISCONNECT, PAnsiChar('DISCONNECT ' + DisconnectRoot));
    ActiveConnectionList.Delete(ActiveConnectionList.IndexOfObject(FtpSend));
    FreeAndNil(FtpSend);
  end;
end;

procedure FsSetCryptCallback(pCryptProc: TCryptProc; CryptoNr, Flags: Integer); dcpcall;
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
  IniFile := TIniFile.Create(dps.DefaultIniName);
  IniFile.WriteDateTime('FTP', 'Test', Now);
  ReadConnectionList;
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

  HasDialogAPI:= True;
end;

function ReadPassword(ConnectionName: UTF8String): AnsiString;
begin
  if CryptFunc(FS_CRYPT_LOAD_PASSWORD, ConnectionName, Result) <> FS_FILE_OK then
    Result:= EmptyStr;
end;

function DeletePassword(ConnectionName: UTF8String): Boolean;
var
  Password: AnsiString;
begin
  Password:= EmptyStr;
  Result:= CryptFunc(FS_CRYPT_DELETE_PASSWORD, ConnectionName, Password) = FS_FILE_OK;
end;

{ TFTPListRecEx }

procedure TFTPListRecEx.Assign(Value: TFTPListRec);
begin
  inherited Assign(Value);
  Permission:= Value.Permission;
end;

{ TFTPListEx }

procedure TFTPListEx.Assign(Value: TFTPList);
var
  flr: TFTPListRecEx;
  n: integer;
begin
  Clear;
  for n := 0 to Value.Count - 1 do
  begin
    flr := TFTPListRecEx.Create;
    flr.Assign(Value[n]);
    Flist.Add(flr);
  end;
  Lines.Assign(Value.Lines);
  Masks.Assign(Value.Masks);
  UnparsedLines.Assign(Value.UnparsedLines);
end;

{ TFTPSendEx }

procedure TFTPSendEx.FTPStatus(Sender: TObject; Response: Boolean;
  const Value: string);
begin
  LogProc(PluginNumber, msgtype_details, PAnsiChar(Value));
end;

initialization
  ListLock := syncobjs.TCriticalSection.Create;

finalization
  FreeAndNil(ListLock);

end.
