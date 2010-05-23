{
   Double Commander
   -------------------------------------------------------------------------
   Virtual File System - class for manage WFX plugins (Version 1.3)
 
   Copyright (C) 2007-2010  Koblov Alexander (Alexx2000@mail.ru)
 
   Callback functions based on:
     Total Commander filesystem plugins debugger
     Author: Pavel Dubrovsky
     
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

unit uWFXmodule;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, WfxPlugin, uWFXprototypes,
  dynlibs, uClassesEx, DialogAPI, uTypes, uXmlConfig;

const
  WFX_SUCCESS      =  0;
  WFX_NOTSUPPORTED = -10;
  WFX_ERROR        = -20;

type

  { TWfxFindData }

  TWfxFindData = record
    FileAttributes : TFileAttrs;
    CreationTime,
    LastAccessTime,
    LastWriteTime : TDateTime;
    FileSize : Int64;
    Reserved0,
    Reserved1 : LongWord;
    FileName : UTF8String;
    AlternateFileName : UTF8String;
  end;

  { TWFXModule }

  TWFXModule = class
  private
    FModuleHandle: TLibHandle;  // Handle to .DLL or .so
    FModuleFileName: UTF8String;
  public
  { Mandatory }
    FsInit : TFsInit;
    FsFindFirst : TFsFindFirst;
    FsFindNext : TFsFindNext;
    FsFindClose : TFsFindClose;
  { Optional }
    FsSetCryptCallback: TFsSetCryptCallback;
    FsGetDefRootName : TFsGetDefRootName;
    FsGetFile : TFsGetFile;
    FsPutFile : TFsPutFile;
    FsDeleteFile : TFsDeleteFile;
    FsRemoveDir : TFsRemoveDir;
    FsExecuteFile : TFsExecuteFile;
    FsMkDir : TFsMkDir;
    FsStatusInfo : TFsStatusInfo;
    FsSetDefaultParams : TFsSetDefaultParams;
    FsContentPluginUnloading : TFsContentPluginUnloading;
    //---------------------
    FsSetAttr:TFsSetAttr;
    FsSetTime:TFsSetTime;
    FsExtractCustomIcon:TFsExtractCustomIcon;
    FsRenMovFile:TFsRenMovFile;
    FsDisconnect:TFsDisconnect;
    FsGetPreviewBitmap:TFsGetPreviewBitmap;
    FsLinksToLocalFiles:TFsLinksToLocalFiles;
    FsGetLocalName:TFsGetLocalName;
    //---------------------
    FsContentGetDetectString:TFsContentGetDetectString;
    FsContentGetSupportedField:TFsContentGetSupportedField;
    FsContentGetValue:TFsContentGetValue;
    FsContentSetDefaultParams:TFsContentSetDefaultParams;
    FsContentStopGetValue:TFsContentStopGetValue;
    FsContentGetDefaultSortOrder:TFsContentGetDefaultSortOrder;
    FsContentGetSupportedFieldFlags:TFsContentGetSupportedFieldFlags;
    FsContentSetValue:TFsContentSetValue;
    FsContentGetDefaultView:TFsContentGetDefaultView;
    //---------------------
    FsNetworkGetSupportedProtocols: TFsNetworkGetSupportedProtocols;
    FsNetworkGetConnection: TFsNetworkGetConnection;
    FsNetworkManageConnection: TFsNetworkManageConnection;
    FsNetworkOpenConnection: TFsNetworkOpenConnection;
    { Unicode }
    FsInitW: TFsInitW;
    FsFindFirstW: TFsFindFirstW;
    FsFindNextW: TFsFindNextW;
    //---------------------
    FsSetCryptCallbackW: TFsSetCryptCallbackW;
    FsMkDirW: TFsMkDirW;
    FsExecuteFileW: TFsExecuteFileW;
    FsRenMovFileW: TFsRenMovFileW;
    FsGetFileW: TFsGetFileW;
    FsPutFileW: TFsPutFileW;
    FsDeleteFileW: TFsDeleteFileW;
    FsRemoveDirW: TFsRemoveDirW;
    FsDisconnectW: TFsDisconnectW;
    FsSetAttrW: TFsSetAttrW;
    FsSetTimeW: TFsSetTimeW;
    FsStatusInfoW: TFsStatusInfoW;
    FsExtractCustomIconW: TFsExtractCustomIconW;
    FsGetPreviewBitmapW: TFsGetPreviewBitmapW;
    FsGetLocalNameW: TFsGetLocalNameW;
    //-----------------------
    FsContentGetValueW: TFsContentGetValueW;
    FsContentStopGetValueW: TFsContentStopGetValueW;
    FsContentSetValueW: TFsContentSetValueW;
    FsContentGetDefaultViewW: TFsContentGetDefaultViewW;
    //---------------------
    FsNetworkGetSupportedProtocolsW: TFsNetworkGetSupportedProtocolsW;
    FsNetworkGetConnectionW: TFsNetworkGetConnectionW;
    FsNetworkManageConnectionW: TFsNetworkManageConnectionW;
    FsNetworkOpenConnectionW: TFsNetworkOpenConnectionW;
    { Dialog API }
    SetDlgProc: TSetDlgProc;
  public
    function WfxFindFirst(Path: UTF8String; var FindData: TWfxFindData): THandle;
    function WfxFindNext(Hdl: THandle; var FindData: TWfxFindData): Boolean;
    procedure WfxStatusInfo(RemoteDir: UTF8String; InfoStartEnd, InfoOperation: Integer);
    function WfxExecuteFile(MainWin: HWND; var RemoteName: UTF8String; Verb: UTF8String): Integer;
    function WfxRenMovFile(OldName, NewName: UTF8String; Move, OverWrite: Boolean; RemoteInfo: PRemoteInfo): Integer;
    function WfxGetFile(RemoteName, LocalName: UTF8String; CopyFlags: Integer; RemoteInfo: PRemoteInfo): Integer;
    function WfxPutFile(LocalName, RemoteName: UTF8String; CopyFlags: Integer): Integer;
    function WfxSetAttr(RemoteName: UTF8String; NewAttr: LongInt): Boolean;
    {en
       Each of CreationTime, LastAccessTime, LastWriteTime may be @nil to leave the value unchanged.
    }
    function WfxSetTime(RemoteName: UTF8String; pCreationTime, pLastAccessTime, pLastWriteTime: PWfxFileTime): Boolean;
    function WfxMkDir(const sBasePath, sDirName: UTF8String): LongInt;
    function WfxRemoveDir(const sDirName: UTF8String): Boolean;
    function WfxDeleteFile(const sFileName: UTF8String): Boolean;
    function WfxGetLocalName(var sFileName: UTF8String): Boolean;
    function WfxNetworkGetSupportedProtocols: UTF8String;
    function WfxNetworkGetConnection(Index: LongInt; var Connection: UTF8String): Boolean;
    function WfxNetworkManageConnection(MainWin: HWND; var Connection: UTF8String; Action: LongInt): Boolean;
    function WfxNetworkOpenConnection(var Connection, RootDir, RemotePath: UTF8String): Boolean;
    procedure WfxNetworkCloseConnection(const Connection: UTF8String);
  public
    constructor Create;
    destructor Destroy; override;
    function LoadModule(const sName: String):Boolean; {Load plugin}
    procedure UnloadModule;
    procedure VFSInit(Data: PtrInt);
    procedure VFSDestroy;

    function VFSConfigure(Parent: THandle):Boolean;
    function VFSRootName: UTF8String;

    function VFSNetworkSupport: Boolean;

    function IsLoaded: Boolean;
  end;

  { TWFXModuleList }

  TWFXModuleList = class(TStringList)
  private
    function GetAEnabled(Index: Integer): Boolean;
    function GetAFileName(Index: Integer): String;
    function GetAName(Index: Integer): String;
    procedure SetAEnabled(Index: Integer; const AValue: Boolean);
    procedure SetAFileName(Index: Integer; const AValue: String);
    procedure SetAName(Index: Integer; const AValue: String);
  public
    procedure Load(Ini: TIniFileEx); overload;
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    procedure Save(Ini: TIniFileEx); overload;
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    function Add(Ext: String; FileName: String): Integer; reintroduce;
    function FindFirstEnabledByName(Name: String): Integer;

    property Name[Index: Integer]: String read GetAName write SetAName;
    property FileName[Index: Integer]: String read GetAFileName write SetAFileName;
    property Enabled[Index: Integer]: Boolean read GetAEnabled write SetAEnabled;
  end;

  function GetErrorMsg(iErrorMsg: LongInt): UTF8String;

implementation

uses
  LCLProc, uLng, FileUtil, uGlobsPaths, uDCUtils, uOSUtils, uWfxPluginUtil,
  fDialogBox;

const
  WfxIniFileName = 'wfx.ini';

function GetErrorMsg(iErrorMsg: LongInt): UTF8String;
begin
  case iErrorMsg of
  WFX_ERROR:
    Result:= 'Unknown error!';
  WFX_NOTSUPPORTED,
  FS_FILE_NOTSUPPORTED:
    Result:= rsMsgErrNotSupported;
  FS_FILE_NOTFOUND:
    Result:= 'File not found!';
  FS_FILE_READERROR:
    Result:= rsMsgErrERead;
  FS_FILE_WRITEERROR:
    Result:= rsMsgErrEWrite;
  FS_FILE_USERABORT:
    Result:= rsMsgErrEAborted;
  end;
end;

{ TWfxFindData }

function LoadWfxFindData(const FindData: TWin32FindData): TWfxFindData; overload;
begin
  with Result do
  begin
    FileAttributes:= FindData.dwFileAttributes;
    CreationTime:= WfxFileTimeToDateTime(FindData.ftCreationTime);
    LastAccessTime:= WfxFileTimeToDateTime(FindData.ftLastAccessTime);
    LastWriteTime:= WfxFileTimeToDateTime(FindData.ftLastWriteTime);
    FileSize:= (Int64(FindData.nFileSizeHigh) * MAXDWORD) + FindData.nFileSizeLow;
    Reserved0:= FindData.dwReserved0;
    Reserved1:= FindData.dwReserved1;
    FileName:= SysToUTF8(FindData.cFileName);
    AlternateFileName:= SysToUTF8(FindData.cAlternateFileName);
  end;
end;

function LoadWfxFindData(const FindData: TWin32FindDataW): TWfxFindData; overload;
begin
  with Result do
  begin
    FileAttributes:= FindData.dwFileAttributes;
    CreationTime:= WfxFileTimeToDateTime(FindData.ftCreationTime);
    LastAccessTime:= WfxFileTimeToDateTime(FindData.ftLastAccessTime);
    LastWriteTime:= WfxFileTimeToDateTime(FindData.ftLastWriteTime);
    FileSize:= (Int64(FindData.nFileSizeHigh) * MAXDWORD) + FindData.nFileSizeLow;
    Reserved0:= FindData.dwReserved0;
    Reserved1:= FindData.dwReserved1;
    FileName:= UTF8Encode(WideString(FindData.cFileName));
    AlternateFileName:= UTF8Encode(WideString(FindData.cAlternateFileName));
  end;
end;

{ TWFXModule }

function TWFXModule.WfxFindFirst(Path: UTF8String; var FindData: TWfxFindData): THandle;
var
  FindDataA: TWin32FindData;
  FindDataW: TWin32FindDataW;
begin
  try
    if Assigned(FsFindFirstW) then
      begin
        Result:= FsFindFirstW(PWideChar(UTF8Decode(Path)), FindDataW);
        if Result <> wfxInvalidHandle then
          FindData:= LoadWfxFindData(FindDataW);
      end
    else if Assigned(FsFindFirst) then
      begin
        Result:= FsFindFirst(PAnsiChar(UTF8ToSys(Path)), FindDataA);
        if Result <> wfxInvalidHandle then
          FindData:= LoadWfxFindData(FindDataA);
      end;
  except
    Result:= wfxInvalidHandle;
  end;
end;

function TWFXModule.WfxFindNext(Hdl: THandle; var FindData: TWfxFindData): Boolean;
var
  FindDataA: TWin32FindData;
  FindDataW: TWin32FindDataW;
begin
  if Assigned(FsFindFirstW) then
    begin
      Result:= FsFindNextW(Hdl, FindDataW);
      if Result then
        FindData:= LoadWfxFindData(FindDataW);
    end
  else if Assigned(FsFindFirst) then
    begin
      Result:= FsFindNext(Hdl, FindDataA);
      if Result then
        FindData:= LoadWfxFindData(FindDataA);
    end;
end;

procedure TWFXModule.WFXStatusInfo(RemoteDir: UTF8String; InfoStartEnd, InfoOperation: Integer);
begin
  if Assigned(FsStatusInfoW) then
    FsStatusInfoW(PWideChar(UTF8Decode(RemoteDir)), InfoStartEnd, InfoOperation)
  else if Assigned(FsStatusInfo) then
    FsStatusInfo(PAnsiChar(UTF8ToSys(RemoteDir)), InfoStartEnd, InfoOperation);
end;

function TWFXModule.WfxExecuteFile(MainWin: HWND; var RemoteName: UTF8String; Verb: UTF8String): Integer;
var
  pacRemoteName: PAnsiChar;
  pwcRemoteName: PWideChar;
begin
  Result:= WFX_NOTSUPPORTED;
  if Assigned(FsExecuteFileW) then
    begin
      pwcRemoteName:= GetMem(MAX_PATH * SizeOf(WideChar));
      StrPCopyW(pwcRemoteName, UTF8Decode(RemoteName));
      Result:= FsExecuteFileW(MainWin, pwcRemoteName, PWideChar(UTF8Decode(Verb)));
      if Result = FS_EXEC_SYMLINK then
          RemoteName:= UTF8Encode(WideString(pwcRemoteName));
      FreeMem(pwcRemoteName);
    end
  else if Assigned(FsExecuteFile) then
    begin
      pacRemoteName:= GetMem(MAX_PATH);
      StrPCopy(pacRemoteName, UTF8ToSys(RemoteName));
      Result:= FsExecuteFile(MainWin, pacRemoteName, PAnsiChar(UTF8ToSys(Verb)));
      if Result = FS_EXEC_SYMLINK then
          RemoteName:= SysToUTF8(StrPas(pacRemoteName));
      FreeMem(pacRemoteName);
    end;
end;

function TWFXModule.WfxRenMovFile(OldName, NewName: UTF8String; Move,
                                  OverWrite: Boolean; RemoteInfo: PRemoteInfo): Integer;
begin
  Result:= FS_FILE_NOTSUPPORTED;
  if Assigned(FsRenMovFileW) then
    Result:= FsRenMovFileW(PWideChar(UTF8Decode(OldName)), PWideChar(UTF8Decode(NewName)), Move, OverWrite, RemoteInfo)
  else if Assigned(FsRenMovFile) then
    Result:= FsRenMovFile(PAnsiChar(UTF8ToSys(OldName)), PAnsiChar(UTF8ToSys(NewName)), Move, OverWrite, RemoteInfo);
end;

function TWFXModule.WfxGetFile(RemoteName, LocalName: UTF8String;
                               CopyFlags: Integer; RemoteInfo: PRemoteInfo): Integer;
begin
  Result:= FS_FILE_NOTSUPPORTED;
  if Assigned(FsGetFileW) then
    Result:= FsGetFileW(PWideChar(UTF8Decode(RemoteName)), PWideChar(UTF8Decode(LocalName)), CopyFlags, RemoteInfo)
  else if Assigned(FsGetFile) then
    Result:= FsGetFile(PAnsiChar(UTF8ToSys(RemoteName)), PAnsiChar(UTF8ToSys(LocalName)), CopyFlags, RemoteInfo);
end;

function TWFXModule.WfxPutFile(LocalName, RemoteName: UTF8String; CopyFlags: Integer): Integer;
begin
  Result:= FS_FILE_NOTSUPPORTED;
  if Assigned(FsPutFileW) then
    Result:= FsPutFileW(PWideChar(UTF8Decode(LocalName)), PWideChar(UTF8Decode(RemoteName)), CopyFlags)
  else if Assigned(FsPutFile) then
    Result:= FsPutFile(PAnsiChar(UTF8ToSys(LocalName)), PAnsiChar(UTF8ToSys(RemoteName)), CopyFlags);
end;

function TWFXModule.WfxSetAttr(RemoteName: UTF8String; NewAttr: LongInt): Boolean;
begin
  Result:= False;
  if Assigned(FsSetAttrW) then
    Result:= FsSetAttrW(PWideChar(UTF8Decode(RemoteName)), NewAttr)
  else if Assigned(FsSetAttr) then
    Result:= FsSetAttr(PAnsiChar(UTF8ToSys(RemoteName)), NewAttr);
end;

function TWFXModule.WfxSetTime(RemoteName: UTF8String; pCreationTime,
                               pLastAccessTime, pLastWriteTime: PWfxFileTime): Boolean;
begin
  Result:= False;
  if Assigned(FsSetTimeW) then
    Result:= FsSetTimeW(PWideChar(UTF8Decode(RemoteName)), pCreationTime, pLastAccessTime, pLastWriteTime)
  else if Assigned(FsSetTime) then
    Result:= FsSetTime(PAnsiChar(UTF8ToSys(RemoteName)), pCreationTime, pLastAccessTime, pLastWriteTime);
end;

function TWFXModule.WfxMkDir(const sBasePath, sDirName: UTF8String): LongInt;
begin
  Result:= WFX_NOTSUPPORTED;
  if Assigned(FsMkDirW) then
    begin
      WfxStatusInfo(sBasePath, FS_STATUS_START, FS_STATUS_OP_MKDIR);
      if FsMkDirW(PWideChar(UTF8Decode(sDirName))) then
        Result:= WFX_SUCCESS
      else
        Result:= WFX_ERROR;
      WfxStatusInfo(sBasePath, FS_STATUS_END, FS_STATUS_OP_MKDIR);
    end
  else if Assigned(FsMkDir) then
    begin
      WfxStatusInfo(sBasePath, FS_STATUS_START, FS_STATUS_OP_MKDIR);
      if FsMkDir(PAnsiChar(UTF8ToSys(sDirName))) then
        Result:= WFX_SUCCESS
      else
        Result:= WFX_ERROR;
      WfxStatusInfo(sBasePath, FS_STATUS_END, FS_STATUS_OP_MKDIR);
    end;
end;

function TWFXModule.WfxRemoveDir(const sDirName: UTF8String): Boolean;
begin
  Result:= False;
  if Assigned(FsRemoveDirW) then
    Result:= FsRemoveDirW(PWideChar(UTF8Decode(sDirName)))
  else if Assigned(FsRemoveDir) then
    Result:= FsRemoveDir(PAnsiChar(UTF8ToSys(sDirName)));
end;

function TWFXModule.WfxDeleteFile(const sFileName: UTF8String): Boolean;
begin
  Result:= False;
  if Assigned(FsDeleteFileW) then
    Result:= FsDeleteFileW(PWideChar(UTF8Decode(sFileName)))
  else if Assigned(FsDeleteFile) then
    Result:= FsDeleteFile(PAnsiChar(UTF8ToSys(sFileName)));
end;

function TWFXModule.WfxGetLocalName(var sFileName: UTF8String): Boolean;
var
  pacRemoteName: PAnsiChar;
  pwcRemoteName: PWideChar;
begin
  Result:= False;
  if Assigned(FsGetLocalNameW) then
    begin
      pwcRemoteName:= GetMem(MAX_PATH * SizeOf(WideChar));
      StrPCopyW(pwcRemoteName, UTF8Decode(sFileName));
      Result:= FsGetLocalNameW(pwcRemoteName, MAX_PATH);
      if Result = True then
        sFileName:= UTF8Encode(WideString(pwcRemoteName));
      FreeMem(pwcRemoteName);
    end
  else if Assigned(FsGetLocalName) then
    begin
      pacRemoteName:= GetMem(MAX_PATH);
      StrPCopy(pacRemoteName, UTF8ToSys(sFileName));
      Result:= FsGetLocalName(pacRemoteName, MAX_PATH);
      if Result = True then
        sFileName:= SysToUTF8(StrPas(pacRemoteName));
      FreeMem(pacRemoteName);
    end;
end;

function TWFXModule.WfxNetworkGetSupportedProtocols: UTF8String;
var
  pacProtocols: PAnsiChar;
  pwcProtocols: PWideChar;
begin
  Result:= EmptyStr;
  if Assigned(FsNetworkGetSupportedProtocolsW) then
    begin
      pwcProtocols:= GetMem(MAX_PATH * SizeOf(WideChar));
      FsNetworkGetSupportedProtocolsW(pwcProtocols, MAX_PATH);
      Result:= UTF8Encode(WideString(pwcProtocols));
      FreeMem(pwcProtocols);
    end
  else if Assigned(FsNetworkGetSupportedProtocols) then
    begin
      pacProtocols:= GetMem(MAX_PATH);
      FsNetworkGetSupportedProtocols(pacProtocols, MAX_PATH);
      Result:= SysToUTF8(StrPas(pacProtocols));
      FreeMem(pacProtocols);
    end;
end;

function TWFXModule.WfxNetworkGetConnection(Index: LongInt; var Connection: UTF8String): Boolean;
var
  pacConnection: PAnsiChar;
  pwcConnection: PWideChar;
begin
  Result:= False;
  if Assigned(FsNetworkGetConnectionW) then
    begin
      pwcConnection:= GetMem(MAX_PATH * SizeOf(WideChar));
      Result:= FsNetworkGetConnectionW(Index, pwcConnection, MAX_PATH);
      if Result = True then
        Connection:= UTF8Encode(WideString(pwcConnection));
      FreeMem(pwcConnection);
    end
  else if Assigned(FsNetworkGetConnection) then
    begin
      pacConnection:= GetMem(MAX_PATH);
      Result:= FsNetworkGetConnection(Index, pacConnection, MAX_PATH);
      if Result = True then
        Connection:= SysToUTF8(StrPas(pacConnection));
      FreeMem(pacConnection);
    end;
end;

function TWFXModule.WfxNetworkManageConnection(MainWin: HWND; var Connection: UTF8String;
  Action: LongInt): Boolean;
var
  pacConnection: PAnsiChar;
  pwcConnection: PWideChar;
begin
  Result:= False;
  if Assigned(FsNetworkManageConnectionW) then
    begin
      pwcConnection:= GetMem(MAX_PATH * SizeOf(WideChar));
      if Action <> FS_NM_ACTION_ADD then
        StrPCopyW(pwcConnection, UTF8Decode(Connection));
      Result:= FsNetworkManageConnectionW(MainWin, pwcConnection, Action, MAX_PATH);
      if (Result = True) and (Action = FS_NM_ACTION_ADD) then
        Connection:= UTF8Encode(WideString(pwcConnection));
      FreeMem(pwcConnection);
    end
  else if Assigned(FsNetworkManageConnection) then
    begin
      pacConnection:= GetMem(MAX_PATH);
      if Action <> FS_NM_ACTION_ADD then
        StrPCopy(pacConnection, UTF8ToSys(Connection));
      Result:= FsNetworkManageConnection(MainWin, pacConnection, Action, MAX_PATH);
      if (Result = True) and (Action = FS_NM_ACTION_ADD) then
        Connection:= SysToUTF8(StrPas(pacConnection));
      FreeMem(pacConnection);
    end;
end;

function TWFXModule.WfxNetworkOpenConnection(var Connection, RootDir, RemotePath: UTF8String): Boolean;
var
  pacConnection: PAnsiChar;
  pwcConnection: PWideChar;
  pacRootDir: PAnsiChar;
  pwcRootDir: PWideChar;
  pacRemotePath: PAnsiChar;
  pwcRemotePath: PWideChar;
begin
  Result:= False;
  if Assigned(FsNetworkOpenConnectionW) then
    begin
      pwcConnection:= GetMem(MAX_PATH * SizeOf(WideChar));
      pwcRootDir:= GetMem(MAX_PATH * SizeOf(WideChar));
      pwcRemotePath:= GetMem(MAX_PATH * SizeOf(WideChar));
      if Connection = EmptyStr then
        pwcConnection:= #0
      else
        StrPCopyW(pwcConnection, UTF8Decode(Connection));
      Result:= FsNetworkOpenConnectionW(pwcConnection, pwcRootDir, pwcRemotePath, MAX_PATH);
      if Result = True then
        begin
          Connection:= UTF8Encode(WideString(pwcConnection));
          RootDir:= UTF8Encode(WideString(pwcRootDir));
          RemotePath:= UTF8Encode(WideString(pwcRemotePath));
        end;
      FreeMem(pwcConnection);
      FreeMem(pwcRootDir);
      FreeMem(pwcRemotePath);
    end
  else if Assigned(FsNetworkOpenConnection) then
    begin
      pacConnection:= GetMem(MAX_PATH);
      pacRootDir:= GetMem(MAX_PATH);
      pacRemotePath:= GetMem(MAX_PATH);
      if Connection = EmptyStr then
        pacConnection:= #0
      else
        StrPCopy(pacConnection, UTF8ToSys(Connection));
      Result:= FsNetworkOpenConnection(pacConnection, pacRootDir, pacRemotePath, MAX_PATH);
      if Result = True then
        begin
          Connection:= SysToUTF8(StrPas(pacConnection));
          RootDir:= SysToUTF8(StrPas(pacRootDir));
          RemotePath:= SysToUTF8(StrPas(pacRemotePath));
        end;
      FreeMem(pacConnection);
      FreeMem(pacRootDir);
      FreeMem(pacRemotePath);
    end;
end;

procedure TWFXModule.WfxNetworkCloseConnection(const Connection: UTF8String);
begin
  if Assigned(FsDisconnectW) then
    FsDisconnectW(PWideChar(UTF8Decode(Connection)))
  else if Assigned(FsDisconnect) then
    FsDisconnect(PAnsiChar(UTF8ToSys(Connection)));
end;

constructor TWFXModule.Create;
begin

end;

destructor TWFXModule.Destroy;
begin
  if IsLoaded then
    begin
      //TODO:Remove this and use VFSDestroy
      //------------------------------------------------------
      if Assigned(FsContentPluginUnloading) then
        FsContentPluginUnloading;
      //------------------------------------------------------
      UnloadModule;
    end;
end;

function TWFXModule.LoadModule(const sName: String): Boolean;
begin
  FModuleHandle := mbLoadLibrary(sName);
  Result := (FModuleHandle <> 0);
  if  FModuleHandle = 0 then Exit(False);

  DebugLn('WFX module loaded ' + sName + ' at ' + hexStr(Pointer(FModuleHandle)));

  FModuleFileName:= sName;
{ Mandatory }
  FsInit := TFsInit(GetProcAddress(FModuleHandle,'FsInit'));
  FsFindFirst := TFsFindFirst(GetProcAddress(FModuleHandle,'FsFindFirst'));
  FsFindNext := TFsFindNext(GetProcAddress(FModuleHandle,'FsFindNext'));
  FsFindClose := TFsFindClose(GetProcAddress(FModuleHandle,'FsFindClose'));
{ Optional }
  FsSetCryptCallback:= TFsSetCryptCallback(GetProcAddress(FModuleHandle,'FsSetCryptCallback'));
  FsGetDefRootName := TFsGetDefRootName(GetProcAddress(FModuleHandle,'FsGetDefRootName'));
  FsExecuteFile := TFsExecuteFile(GetProcAddress(FModuleHandle,'FsExecuteFile'));
  FsGetFile := TFsGetFile(GetProcAddress(FModuleHandle,'FsGetFile'));
  FsPutFile := TFsPutFile(GetProcAddress(FModuleHandle,'FsPutFile'));
  FsDeleteFile := TFsDeleteFile(GetProcAddress(FModuleHandle,'FsDeleteFile'));
  FsMkDir := TFsMkDir(GetProcAddress(FModuleHandle,'FsMkDir'));
  FsRemoveDir := TFsRemoveDir(GetProcAddress(FModuleHandle,'FsRemoveDir'));
  FsStatusInfo := TFsStatusInfo(GetProcAddress(FModuleHandle,'FsStatusInfo'));
  FsSetDefaultParams := TFsSetDefaultParams(GetProcAddress(FModuleHandle,'FsSetDefaultParams'));
  FsContentPluginUnloading := TFsContentPluginUnloading(GetProcAddress(FModuleHandle,'FsContentPluginUnloading'));
  //---------------------
  FsSetAttr := TFsSetAttr (GetProcAddress(FModuleHandle,'FsSetAttr'));
  FsSetTime := TFsSetTime (GetProcAddress(FModuleHandle,'FsSetTime'));
  FsExtractCustomIcon := TFsExtractCustomIcon (GetProcAddress(FModuleHandle,'FsExtractCustomIcon'));
  FsRenMovFile := TFsRenMovFile (GetProcAddress(FModuleHandle,'FsRenMovFile'));
  FsDisconnect := TFsDisconnect (GetProcAddress(FModuleHandle,'FsDisconnect'));
  FsGetPreviewBitmap := TFsGetPreviewBitmap (GetProcAddress(FModuleHandle,'FsGetPreviewBitmap'));
  FsLinksToLocalFiles := TFsLinksToLocalFiles (GetProcAddress(FModuleHandle,'FsLinksToLocalFiles'));
  FsGetLocalName := TFsGetLocalName (GetProcAddress(FModuleHandle,'FsGetLocalName'));
  //---------------------
  FsContentGetDetectString := TFsContentGetDetectString (GetProcAddress(FModuleHandle,'FsContentGetDetectString'));
  FsContentGetSupportedField := TFsContentGetSupportedField (GetProcAddress(FModuleHandle,'FsContentGetSupportedField'));
  FsContentGetValue := TFsContentGetValue (GetProcAddress(FModuleHandle,'FsContentGetValue'));
  FsContentSetDefaultParams := TFsContentSetDefaultParams (GetProcAddress(FModuleHandle,'FsContentSetDefaultParams'));
  FsContentStopGetValue := TFsContentStopGetValue (GetProcAddress(FModuleHandle,'FsContentStopGetValue'));
  FsContentGetDefaultSortOrder := TFsContentGetDefaultSortOrder (GetProcAddress(FModuleHandle,'FsContentGetDefaultSortOrder'));
  FsContentGetSupportedFieldFlags := TFsContentGetSupportedFieldFlags (GetProcAddress(FModuleHandle,'FsContentGetSupportedFieldFlags'));
  FsContentSetValue := TFsContentSetValue (GetProcAddress(FModuleHandle,'FsContentSetValue'));
  FsContentGetDefaultView := TFsContentGetDefaultView (GetProcAddress(FModuleHandle,'FsContentGetDefaultView'));
  //---------------------
  FsNetworkGetSupportedProtocols:= TFsNetworkGetSupportedProtocols(GetProcAddress(FModuleHandle,'FsNetworkGetSupportedProtocols'));
  FsNetworkGetConnection:= TFsNetworkGetConnection(GetProcAddress(FModuleHandle,'FsNetworkGetConnection'));
  FsNetworkManageConnection:= TFsNetworkManageConnection(GetProcAddress(FModuleHandle,'FsNetworkManageConnection'));
  FsNetworkOpenConnection:= TFsNetworkOpenConnection(GetProcAddress(FModuleHandle,'FsNetworkOpenConnection'));
{ Unicode }
  FsInitW := TFsInitW(GetProcAddress(FModuleHandle,'FsInitW'));
  FsFindFirstW := TFsFindFirstW(GetProcAddress(FModuleHandle,'FsFindFirstW'));
  FsFindNextW := TFsFindNextW(GetProcAddress(FModuleHandle,'FsFindNextW'));
  //---------------------
  FsSetCryptCallbackW:= TFsSetCryptCallbackW(GetProcAddress(FModuleHandle,'FsSetCryptCallbackW'));
  FsMkDirW := TFsMkDirW(GetProcAddress(FModuleHandle,'FsMkDirW'));
  FsExecuteFileW := TFsExecuteFileW(GetProcAddress(FModuleHandle,'FsExecuteFileW'));
  FsRenMovFileW := TFsRenMovFileW(GetProcAddress(FModuleHandle,'FsRenMovFileW'));
  FsGetFileW := TFsGetFileW(GetProcAddress(FModuleHandle,'FsGetFileW'));
  FsPutFileW := TFsPutFileW(GetProcAddress(FModuleHandle,'FsPutFileW'));
  FsDeleteFileW := TFsDeleteFileW(GetProcAddress(FModuleHandle,'FsDeleteFileW'));
  FsRemoveDirW := TFsRemoveDirW(GetProcAddress(FModuleHandle,'FsRemoveDirW'));
  FsDisconnectW := TFsDisconnectW(GetProcAddress(FModuleHandle,'FsDisconnectW'));
  FsSetAttrW := TFsSetAttrW (GetProcAddress(FModuleHandle,'FsSetAttrW'));
  FsSetTimeW := TFsSetTimeW (GetProcAddress(FModuleHandle,'FsSetTimeW'));
  FsStatusInfoW := TFsStatusInfoW(GetProcAddress(FModuleHandle,'FsStatusInfoW'));
  FsExtractCustomIconW := TFsExtractCustomIconW(GetProcAddress(FModuleHandle,'FsExtractCustomIconW'));
  FsGetLocalNameW := TFsGetLocalNameW(GetProcAddress(FModuleHandle,'FsGetLocalNameW'));
  //--------------------
  FsNetworkGetSupportedProtocolsW:= TFsNetworkGetSupportedProtocolsW(GetProcAddress(FModuleHandle,'FsNetworkGetSupportedProtocolsW'));
  FsNetworkGetConnectionW:= TFsNetworkGetConnectionW(GetProcAddress(FModuleHandle,'FsNetworkGetConnectionW'));
  FsNetworkManageConnectionW:= TFsNetworkManageConnectionW(GetProcAddress(FModuleHandle,'FsNetworkManageConnectionW'));
  FsNetworkOpenConnectionW:= TFsNetworkOpenConnectionW(GetProcAddress(FModuleHandle,'FsNetworkOpenConnectionW'));
{ Dialog API }
  SetDlgProc:= TSetDlgProc(GetProcAddress(FModuleHandle,'SetDlgProc'));
end;

procedure TWFXModule.UnloadModule;
begin
  if FModuleHandle <> 0 then
    FreeLibrary(FModuleHandle);
  FModuleHandle := 0;
{ Mandatory }
  FsInit := nil;
  FsFindFirst := nil;
  FsFindNext := nil;
  FsFindClose := nil;
{ Optional }
  FsSetCryptCallback := nil;
  FsGetDefRootName := nil;
  FsGetFile := nil;
  FsPutFile := nil;
  FsDeleteFile := nil;
  FsRemoveDir := nil;
  FsExecuteFile := nil;
  FsMkDir := nil;
  FsStatusInfo := nil;
  FsSetDefaultParams:=nil;
  //---------------------
  FsSetAttr := nil;
  FsSetTime := nil;
  FsExtractCustomIcon := nil;
  FsRenMovFile := nil;
  FsDisconnect := nil;
  FsGetPreviewBitmap := nil;
  FsLinksToLocalFiles := nil;
  FsGetLocalName := nil;
  //---------------------
  FsContentGetDetectString := nil;
  FsContentGetSupportedField := nil;
  FsContentGetValue := nil;
  FsContentSetDefaultParams := nil;
  FsContentStopGetValue := nil;
  FsContentGetDefaultSortOrder := nil;
  FsContentGetSupportedFieldFlags := nil;
  FsContentSetValue := nil;
  FsContentGetDefaultView := nil;
  FsContentPluginUnloading := nil;
  //----------------------
  FsNetworkGetSupportedProtocols:= nil;
  FsNetworkGetConnection:= nil;
  FsNetworkManageConnection:= nil;
  FsNetworkOpenConnection:= nil;
{ Unicode }
  FsInitW := nil;
  FsFindFirstW := nil;
  FsFindNextW := nil;
  //---------------------
  FsSetCryptCallbackW:= nil;
  FsMkDirW := nil;
  FsExecuteFileW := nil;
  FsRenMovFileW := nil;
  FsGetFileW := nil;
  FsPutFileW := nil;
  FsDeleteFileW := nil;
  FsRemoveDirW := nil;
  FsDisconnectW := nil;
  FsSetAttrW := nil;
  FsSetTimeW := nil;
  FsStatusInfoW := nil;
  FsExtractCustomIconW := nil;
  FsGetLocalNameW := nil;
  //----------------------
  FsNetworkGetSupportedProtocolsW:= nil;
  FsNetworkGetConnectionW:= nil;
  FsNetworkManageConnectionW:= nil;
  FsNetworkOpenConnectionW:= nil;
{ Dialog API }
  SetDlgProc:= nil;
end;

procedure TWFXModule.VFSInit(Data: PtrInt);
var
  dps: pFsDefaultParamStruct;
  SetDlgProcInfo: TSetDlgProcInfo;
  sPluginDir: WideString;
  sPluginConfDir: WideString;
begin
    if Assigned(FsSetDefaultParams) then
    begin
      GetMem(dps,SizeOf(tFsDefaultParamStruct));
      dps.DefaultIniName:=gpCfgDir + WfxIniFileName;
      dps.PluginInterfaceVersionHi:=1;
      dps.PluginInterfaceVersionLow:=50;
      dps.size:=SizeOf(tFsDefaultParamStruct);
      FsSetDefaultParams(dps);
      FreeMem(dps,SizeOf(tFsDefaultParamStruct));
    end;

  // Dialog API
  if Assigned(SetDlgProc) then
    begin
      sPluginDir := UTF8Decode(ExtractFilePath(FModuleFileName));
      sPluginConfDir := UTF8Decode(gpCfgDir);

      with SetDlgProcInfo do
      begin
        PluginDir:= PWideChar(sPluginDir);
        PluginConfDir:= PWideChar(sPluginConfDir);
        InputBox:= @fDialogBox.InputBox;
        MessageBox:= @fDialogBox.MessageBox;
        DialogBoxLFM:= @fDialogBox.DialogBoxLFM;
        DialogBoxLRS:= @fDialogBox.DialogBoxLRS;
        DialogBoxLFMFile:= @fDialogBox.DialogBoxLFMFile;
        SendDlgMsg:= @fDialogBox.SendDlgMsg;
      end;

      SetDlgProc(SetDlgProcInfo);
    end;
end;

procedure TWFXModule.VFSDestroy;
begin
  //TODO: need to invoke this func
  if Assigned(FsContentPluginUnloading) then
    FsContentPluginUnloading;
end;

function TWFXModule.VFSConfigure(Parent: THandle): Boolean;
var
  RemoteName: UTF8String;
begin
  try
    RemoteName:= PathDelim;
    WFXStatusInfo(PathDelim, FS_STATUS_START, FS_STATUS_OP_EXEC);
    Result:= (WfxExecuteFile(Parent, RemoteName, 'properties') = FS_EXEC_OK);
    WFXStatusInfo(PathDelim, FS_STATUS_END, FS_STATUS_OP_EXEC);
  except
    Result:= False;
  end;	
end;

function TWFXModule.VFSRootName: UTF8String;
var
  pcRootName : PAnsiChar;
begin
  Result:= EmptyStr;
  if Assigned(FsGetDefRootName) then
    try
      pcRootName:= GetMem(MAX_PATH);
      FsGetDefRootName(pcRootName, MAX_PATH);
      Result := StrPas(pcRootName);
    finally
      FreeMem(pcRootName);
    end;
end;

function TWFXModule.VFSNetworkSupport: Boolean;
begin
  Result:= (Assigned(FsNetworkGetConnection) and Assigned(FsNetworkManageConnection) and Assigned(FsNetworkOpenConnection))
        or (Assigned(FsNetworkGetConnectionW) and Assigned(FsNetworkManageConnectionW) and Assigned(FsNetworkOpenConnectionW));
end;

function TWFXModule.IsLoaded: Boolean;
begin
Result := (FModuleHandle <> 0);
end;

{ TWFXModuleList }

function TWFXModuleList.GetAEnabled(Index: Integer): Boolean;
begin
  Result:= Boolean(Objects[Index]);
end;

function TWFXModuleList.GetAFileName(Index: Integer): String;
begin
  Result:= ValueFromIndex[Index];
end;

function TWFXModuleList.GetAName(Index: Integer): String;
begin
  Result:= Names[Index];
end;

procedure TWFXModuleList.SetAEnabled(Index: Integer; const AValue: Boolean);
begin
  Objects[Index]:= TObject(AValue);
end;

procedure TWFXModuleList.SetAFileName(Index: Integer; const AValue: String);
begin
  ValueFromIndex[Index]:= AValue;
end;

procedure TWFXModuleList.SetAName(Index: Integer; const AValue: String);
var
  sValue : String;
begin
  sValue:= ValueFromIndex[Index];
  Self[Index]:= AValue + '=' + sValue;
end;

procedure TWFXModuleList.Load(Ini: TIniFileEx);
var
  I: Integer;
  sCurrPlugin: String;
begin
  Ini.ReadSectionRaw('FileSystemPlugins', Self);
  for I:= 0 to Count - 1 do
    if Pos('#', Name[I]) = 0 then
      begin
        Enabled[I]:= True;
      end
    else
      begin
        sCurrPlugin:= Name[I];
        Name[I]:= Copy(sCurrPlugin, 2, Length(sCurrPlugin) - 1);
        Enabled[I]:= False;
      end;
end;

procedure TWFXModuleList.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I: Integer;
  AName, APath: String;
begin
  Clear;

  ANode := ANode.FindNode('WfxPlugins');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('WfxPlugin') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'Name', AName) and
           AConfig.TryGetValue(ANode, 'Path', APath) then
        begin
          I := Add(AName, APath);
          Enabled[I] := AConfig.GetAttr(ANode, 'Enabled', True);
        end
        else
          DebugLn('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TWFXModuleList.Save(Ini: TIniFileEx);
var
 I: Integer;
begin
  Ini.EraseSection('FileSystemPlugins');
  for I := 0 to Count - 1 do
    begin
      if Enabled[I] then
        begin
          Ini.WriteString('FileSystemPlugins', Name[I], FileName[I])
        end
      else
        begin
          Ini.WriteString('FileSystemPlugins', '#' + Name[I], FileName[I]);
        end;
    end;
end;

procedure TWFXModuleList.Save(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I: Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, 'WfxPlugins', True);
  AConfig.ClearNode(ANode);
  for I := 0 to Count - 1 do
    begin
      SubNode := AConfig.AddNode(ANode, 'WfxPlugin');
      AConfig.SetAttr(SubNode, 'Enabled', Enabled[I]);
      AConfig.AddValue(SubNode, 'Name', Name[I]);
      AConfig.AddValue(SubNode, 'Path', FileName[I]);
    end;
end;

function TWFXModuleList.Add(Ext: String; FileName: String): Integer;
begin
  Result:= AddObject(Ext + '=' + FileName, TObject(True));
end;

function TWFXModuleList.FindFirstEnabledByName(Name: String): Integer;
begin
  Result:=0;
  while Result < Count do
  begin
    if Enabled[Result] and (DoCompareText(Names[Result], Name) = 0) then
       Exit
    else
      Result := Result + 1;
  end;
  if Result=Count then Result:=-1;
end;

end.
