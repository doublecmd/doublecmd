{
   Double commander
   -------------------------------------------------------------------------
   WFX plugin for working with File Transfer Protocol

   Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

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

{$IFDEF FPC}
{$MODE delphi}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  ufsplugin, FtpSend, DialogAPI;

type
  TFTPSendEx = class(TFTPSend)
  public
    procedure FTPStatus(Sender: TObject; Response: Boolean;
      const Value: string);
  end;

function FsInit(PluginNr: Integer; pProgressProc: TProgressProc;
  pLogProc: TLogProc; pRequestProc: TRequestProc): Integer; stdcall;

function FsFindFirst(Path: PAnsiChar; var FindData: TWin32FindData): THandle;
  stdcall;
function FsFindNext(Hdl: THandle; var FindData: TWin32FindData): BOOL; stdcall;
function FsFindClose(Hdl: THandle): Integer; stdcall;

function FsExecuteFile(MainWin: THandle; RemoteName, Verb: PAnsiChar): Integer;
  stdcall;
function FsRenMovFile(OldName, NewName: PAnsiChar; Move, OverWrite: BOOL;
  RemoteInfo: pRemoteInfo): Integer; stdcall;
function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: Integer;
  RemoteInfo: pRemoteInfo): Integer; stdcall;
function FsPutFile(LocalName, RemoteName: PAnsiChar; CopyFlags: Integer)
  : Integer; stdcall;
function FsDeleteFile(RemoteName: PAnsiChar): BOOL; stdcall;

function FsMkDir(RemoteDir: PAnsiChar): BOOL; stdcall;
function FsRemoveDir(RemoteName: PAnsiChar): BOOL; stdcall;

function FsDisconnect(DisconnectRoot: PAnsiChar): BOOL; stdcall;

procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer); stdcall;
procedure FsSetDefaultParams(dps: pFsDefaultParamStruct); stdcall;

{Dialog API function}
procedure SetDlgProc(var SetDlgProcInfo: TSetDlgProcInfo);stdcall;

var
  gSetDlgProcInfo: TSetDlgProcInfo;
  gPluginDir: UTF8String;

implementation

uses
  IniFiles, FtpUtils, FtpConfDlg;

var
  ActiveConnectionList, ConnectionList: TStringList;
  IniFile: TIniFile;
  ProgressProc: TProgressProc;
  LogProc: TLogProc;
  RequestProc: TRequestProc;
  PluginNumber: Integer;

const
  cAddConnection = '<Add connection>';
  cQuickConnection = '<Quick connection>';
  RootList: array [0 .. 1] of AnsiString = (cAddConnection, cQuickConnection);

type
  TListRec = record
    Path: AnsiString;
    Index: Integer;
    FtpSend: TFTPSendEx;
  end;
  PListRec = ^TListRec;

  TConnection = class
  public
    ConnectionName, Path, Host: AnsiString;
    Port: AnsiString;
    UserName: AnsiString;
    Password: AnsiString;
  end;

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

function ShowPasswordDialog(out Password: AnsiString): Boolean;
const
  MAX_PATH = 256;
var
  pcTemp: PAnsiChar;
begin
  Result:= False;
  GetMem(pcTemp, MAX_PATH);
  if RequestProc(PluginNumber, RT_Password, nil, nil, pcTemp, MAX_PATH) then
    begin
      Password:= pcTemp;
      Result:= True;
    end;
  FreeMem(pcTemp);
end;


function FtpConnect(const ConnectionName: AnsiString; out FtpSend: TFTPSendEx)
  : Boolean;
var
  I: Integer;
  Connection: TConnection;
begin
  Result:= False;
  if ActiveConnectionList.IndexOf(ConnectionName) < 0 then
  begin
    // find in exists connection list
    I:= ConnectionList.IndexOf(ConnectionName);
    if I >= 0 then
    begin
      Connection := TConnection(ConnectionList.Objects[I]);
      FtpSend := TFTPSendEx.Create;
      FtpSend.OnStatus:= FtpSend.FTPStatus;
      FtpSend.TargetHost := Connection.Host;
      if Connection.Port <> EmptyStr then
        FtpSend.TargetPort := Connection.Port;
      if Connection.UserName <> EmptyStr then
        FtpSend.UserName := Connection.UserName;
      if Connection.Password <> EmptyStr then
        FtpSend.Password := Connection.Password
      else  // if no saved password then ask it
        begin
          if ShowPasswordDialog(Connection.Password) then
            FtpSend.Password := Connection.Password
          else
            begin
              FreeAndNil(FtpSend);
              Exit;
            end;
        end;
      // try to connect
      if FtpSend.Login then
        begin
          LogProc(PluginNumber, MSGTYPE_CONNECT, PAnsiChar('CONNECT ' + ConnectionName));
          ActiveConnectionList.AddObject(ConnectionName, FtpSend);
          Result:= True;
        end
      else
        begin
          FreeAndNil(FtpSend);
          Exit;
        end;
    end;
  end;
end;


function AddQuickConnection(const Connection: TConnection): Boolean;
const
  MAX_PATH = 256;
var
  pcTemp: PAnsiChar;
begin
  Result:= False;
  GetMem(pcTemp, MAX_PATH);
  if RequestProc(PluginNumber, RT_URL, nil, nil, pcTemp, MAX_PATH) then
  begin
    Connection.Host := pcTemp;
    if RequestProc(PluginNumber, RT_TargetDir, nil, nil, pcTemp, MAX_PATH) then
    begin
      Connection.Path := pcTemp;
      if RequestProc(PluginNumber, RT_UserName, nil, nil, pcTemp, MAX_PATH) then
      begin
        Connection.UserName := pcTemp;
        Result:= True;
      end;
    end;
  end;
  FreeMem(pcTemp);
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


procedure AddConnection;
const
  MAX_PATH = 256;
var
  pcTemp: PAnsiChar;
  Connection: TConnection;
  bCancel: Boolean;
begin
  bCancel := True;
  Connection := TConnection.Create;
  GetMem(pcTemp, MAX_PATH);
  if RequestProc(PluginNumber, RT_Other, nil, nil, pcTemp, MAX_PATH) then
  begin
    Connection.ConnectionName := pcTemp;
    if AddQuickConnection(Connection) then
    begin
      ConnectionList.AddObject(Connection.ConnectionName, Connection);
      bCancel := False;
    end;
  end;
  FreeMem(pcTemp);

  if bCancel then
    Connection.Free
  else
    WriteConnectionList;
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
  Index: Integer;
  sConnName: AnsiString;
begin
  Result := False;
  sConnName := ExtractConnectionName(sPath);
  RemotePath := ExtractRemoteFileName(sPath);
  Index:= ActiveConnectionList.IndexOf(sConnName);
  if Index >= 0 then
    begin
      FtpSend := TFTPSendEx(ActiveConnectionList.Objects[Index]);
      Result := True;
    end
  else
    begin
      Result:= FtpConnect(sConnName, FtpSend);
    end;
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
    FindData.dwFileAttributes := FILE_ATTRIBUTE_REPARSE_POINT;
    Inc(ListRec^.Index);
    Result := True;
  end;
end;

function RemoteFindNext(Hdl: THandle; var FindData: TWin32FindData): Boolean;
var
  ListRec: PListRec absolute Hdl;
  I: Integer;
  FtpSend: TFTPSendEx;
  sTemp: AnsiString;
begin
  Result := False;
  if Assigned(ListRec^.FtpSend) then
    with ListRec^ do
    begin
      I := ListRec^.Index;
      if I < FtpSend.FtpList.Count then
      begin
        FillChar(FindData, SizeOf(FindData), 0);
        StrPCopy(FindData.cFileName, FtpSend.FtpList.Items[I].FileName);
        FindData.dwFileAttributes := FindData.dwFileAttributes or faUnixMode;
        if FtpSend.FtpList.Items[I].Directory then
          FindData.dwFileAttributes := FindData.dwFileAttributes or FILE_ATTRIBUTE_DIRECTORY
        else
          FindData.nFileSizeLow := FtpSend.FtpList.Items[I].FileSize;
        // set Unix permissions
        FindData.dwReserved0 := ModeStr2Mode(FtpSend.FtpList.Items[I].Permission);
        FindData.ftLastWriteTime := DateTimeToFileTime(FtpSend.FtpList.Items[I].FileTime);
        Inc(ListRec^.Index);
        Result := True;
      end;
    end;
end;

function FsInit(PluginNr: Integer; pProgressProc: TProgressProc;
  pLogProc: TLogProc; pRequestProc: TRequestProc): Integer; stdcall;
begin
  ProgressProc := pProgressProc;
  LogProc := pLogProc;
  RequestProc := pRequestProc;
  PluginNumber := PluginNr;
  ActiveConnectionList := TStringList.Create;
  ConnectionList := TStringList.Create;
  // ------------------------
  // ------------------------
  Result := 0;
end;

function FsFindFirst(Path: PAnsiChar; var FindData: TWin32FindData): THandle;
  stdcall;
var
  ListRec: PListRec;
  I, iCount: Integer;
  sPath: AnsiString;
  FtpSend: TFTPSendEx;
begin
  New(ListRec);
  ListRec.Path := Path;
  ListRec.Index := 0;
  if Path = PathDelim then
  begin
    Result := THandle(ListRec);
    LocalFindNext(Result, FindData);
  end
  else
  begin
    if not GetConnectionByPath(Path, FtpSend, sPath) then
      begin
        Result := THandle(-1);
        Exit;
      end;
    if Length(sPath) <> 1 then
      sPath := sPath + '/';
    FtpSend.FtpList.Clear;
    if FtpSend.List(sPath, False) then
      begin
        ListRec.FtpSend := FtpSend; // cache object
        Result := THandle(ListRec);
        RemoteFindNext(Result, FindData);
      end;
    if FtpSend.FtpList.Count = 0 then
      begin
        Result := THandle(-1);
        Dispose(ListRec);
        {$IFDEF MSWINDOWS}
        SetLastError(ERROR_NO_MORE_FILES);
        {$ENDIF}
      end;
  end;
end;

function FsFindNext(Hdl: THandle; var FindData: TWin32FindData): BOOL; stdcall;
var
  ListRec: PListRec absolute Hdl;
begin
  if ListRec.Path = PathDelim then
    Result := LocalFindNext(Hdl, FindData)
  else
    Result := RemoteFindNext(Hdl, FindData);
end;

function FsFindClose(Hdl: THandle): Integer; stdcall;
var
  ListRec: PListRec absolute Hdl;
begin
  Dispose(ListRec);
end;

function FsExecuteFile(MainWin: THandle; RemoteName, Verb: PAnsiChar): Integer;
  stdcall;
var
  I: Integer;
  FtpSend: TFTPSendEx;
  sFileName: AnsiString;
begin
  Result:= FS_EXEC_YOURSELF;
  if (RemoteName = '') then Exit;
  ShowFtpConfDlg;
  Exit;
  if Verb = 'open' then
    begin
      if (ExtractFileDir(RemoteName) = PathDelim) then // root path
      begin
        if RemoteName[1] <> '<' then // connection
          begin
            if FtpConnect(RemoteName + 1, FtpSend) then
              Result := FS_EXEC_SYMLINK
            else
              Result := FS_EXEC_ERROR;
          end
        else  // special item
          begin
            if (RemoteName + 1) = cAddConnection then
              AddConnection
            else if (RemoteName + 1) = cQuickConnection then
              begin
                if QuickConnection then
                  Result := FS_EXEC_SYMLINK
                else
                  Result := FS_EXEC_ERROR;
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
    begin
      ShowFtpConfDlg;
    end;
end;

function FsRenMovFile(OldName, NewName: PAnsiChar; Move, OverWrite: BOOL;
  RemoteInfo: pRemoteInfo): Integer; stdcall;
var
  FtpSend: TFTPSendEx;
  sRemotePath, sOldName, sNewName: AnsiString;
begin
  if not Move then
  begin
    Result := FS_FILE_NOTSUPPORTED;
    Exit;
  end;
  if GetConnectionByPath(OldName, FtpSend, sOldName) then
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
  RemoteInfo: pRemoteInfo): Integer; stdcall;
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
  : Integer; stdcall;
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

function FsDeleteFile(RemoteName: PAnsiChar): BOOL; stdcall;
var
  sFileName: AnsiString;
  FtpSend: TFTPSendEx;
  I: Integer;
  Connection: TConnection;
begin
  Result := False;
  // if root path then delete connection
  if (ExtractFileDir(RemoteName) = PathDelim) and (AnsiChar(RemoteName[1]) <> '<') then
    begin
      I:= ConnectionList.IndexOf(ExtractConnectionName(RemoteName));
      if I >= 0 then
        begin
          Connection:= TConnection(ConnectionList.Objects[I]);
          Connection.Free;
          ConnectionList.Delete(I);
          WriteConnectionList;
          Result:= True;
        end;
    end
  else if GetConnectionByPath(RemoteName, FtpSend, sFileName) then
    Result := FtpSend.DeleteFile(sFileName);
end;

function FsMkDir(RemoteDir: PAnsiChar): BOOL; stdcall;
var
  sPath: AnsiString;
  FtpSend: TFTPSendEx;
begin
  Result := False;
  if GetConnectionByPath(RemoteDir, FtpSend, sPath) then
    Result := FtpSend.CreateDir(sPath);
end;

function FsRemoveDir(RemoteName: PAnsiChar): BOOL; stdcall;
var
  sPath: AnsiString;
  FtpSend: TFTPSendEx;
begin
  Result := False;
  if GetConnectionByPath(RemoteName, FtpSend, sPath) then
    Result := FtpSend.DeleteDir(sPath);
end;

function FsDisconnect(DisconnectRoot: PAnsiChar): BOOL; stdcall;
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

procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer); stdcall;
begin
  StrPLCopy(DefRootName, 'FTP', MaxLen);
end;

procedure FsSetDefaultParams(dps: pFsDefaultParamStruct); stdcall;
begin
  IniFile := TIniFile.Create(dps.DefaultIniName);
  IniFile.WriteDateTime('FTP', 'Test', Now);
  ReadConnectionList;
end;

procedure SetDlgProc(var SetDlgProcInfo: TSetDlgProcInfo);
var
  gIni: TIniFile;
begin
  gSetDlgProcInfo:= SetDlgProcInfo;

  gPluginDir := UTF8Encode(gSetDlgProcInfo.PluginDir);

  // Clear so they are not used anymore.
  gSetDlgProcInfo.PluginDir := nil;
  gSetDlgProcInfo.PluginConfDir := nil;
end;


{ TFTPSendEx }

procedure TFTPSendEx.FTPStatus(Sender: TObject; Response: Boolean;
  const Value: string);
begin
  LogProc(PluginNumber, msgtype_details, PAnsiChar(Value));
end;

end.
