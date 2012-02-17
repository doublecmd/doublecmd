{
   Double Commander
   -------------------------------------------------------------------------
   Virtual File System - class for manage WFX plugins (Version 1.3)
 
   Copyright (C) 2007-2011  Koblov Alexander (Alexx2000@mail.ru)
 
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

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, WfxPlugin, uWFXprototypes,
  dynlibs, uClassesEx, Extension, uTypes, uXmlConfig;

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
    { Extension API }
    ExtensionInitialize: TExtensionInitializeProc;
    ExtensionFinalize:   TExtensionFinalizeProc;
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
  public
    constructor Create;
    destructor Destroy; override;
    function LoadModule(const sName: String):Boolean; {Load plugin}
    procedure UnloadModule;
    procedure VFSInit(Data: PtrInt);

    function VFSConfigure(Parent: THandle):Boolean;
    function VFSRootName: UTF8String;

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
  uLng, FileUtil, uGlobsPaths, uDCUtils, uOSUtils, uWfxPluginUtil,
  fDialogBox, uDebug;

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
    Int64Rec(FileSize).Lo:= FindData.nFileSizeLow;
    Int64Rec(FileSize).Hi:= FindData.nFileSizeHigh;
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
    Int64Rec(FileSize).Lo:= FindData.nFileSizeLow;
    Int64Rec(FileSize).Hi:= FindData.nFileSizeHigh;
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
    on E: Exception do
    begin
      Result:= wfxInvalidHandle;
      DCDebug(ClassName + '.WfxFindFirst(). Error: ' + E.Message);
    end;
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

constructor TWFXModule.Create;
begin
  inherited;
end;

destructor TWFXModule.Destroy;
begin
  if IsLoaded then
  begin
    if Assigned(FsContentPluginUnloading) then
      FsContentPluginUnloading;
    if Assigned(ExtensionFinalize) then
      ExtensionFinalize(nil);
    //------------------------------------------------------
    UnloadModule;
  end;
  inherited;
end;

function TWFXModule.LoadModule(const sName: String): Boolean;
begin
  FModuleHandle := mbLoadLibrary(sName);
  Result := (FModuleHandle <> 0);
  if not Result then
    Exit;

  DCDebug('WFX module loaded ' + sName + ' at ' + hexStr(Pointer(FModuleHandle)));

  FModuleFileName:= sName;
{ Mandatory }
  FsInit := TFsInit(GetProcAddress(FModuleHandle,'FsInit'));
  FsFindFirst := TFsFindFirst(GetProcAddress(FModuleHandle,'FsFindFirst'));
  FsFindNext := TFsFindNext(GetProcAddress(FModuleHandle,'FsFindNext'));
  FsFindClose := TFsFindClose(GetProcAddress(FModuleHandle,'FsFindClose'));

  if (FsInit = nil) or (FsFindFirst = nil) or
     (FsFindNext = nil) or (FsFindClose = nil) then
  begin
    FsInit:= nil;
    FsFindFirst:= nil;
    FsFindNext:= nil;
    FsFindClose:= nil;
    Exit(False);
  end;

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
  { Extension API }
  ExtensionInitialize:= TExtensionInitializeProc(GetProcAddress(FModuleHandle,'ExtensionInitialize'));
  ExtensionFinalize:= TExtensionFinalizeProc(GetProcAddress(FModuleHandle,'ExtensionFinalize'));
end;

procedure TWFXModule.UnloadModule;
begin
{$IF (not DEFINED(LINUX)) or ((FPC_VERSION > 2) or ((FPC_VERSION=2) and (FPC_RELEASE >= 5)))}
  if FModuleHandle <> 0 then
    FreeLibrary(FModuleHandle);
{$ENDIF}
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
  // Extension API
  ExtensionInitialize:= nil;
  ExtensionFinalize:= nil;
end;

procedure TWFXModule.VFSInit(Data: PtrInt);
var
  dps: tFsDefaultParamStruct;
  StartupInfo: TExtensionStartupInfo;
begin
    if Assigned(FsSetDefaultParams) then
    begin
      dps.DefaultIniName := mbFileNameToSysEnc(gpCfgDir + WfxIniFileName);
      dps.PluginInterfaceVersionHi:= 2;
      dps.PluginInterfaceVersionLow:= 0;
      dps.Size:= SizeOf(dps);
      FsSetDefaultParams(@dps);
    end;

  // Extension API
  if Assigned(ExtensionInitialize) then
    begin
      FillByte(StartupInfo, SizeOf(TExtensionStartupInfo), 0);

      with StartupInfo do
      begin
        StructSize:= SizeOf(TExtensionStartupInfo);
        PluginDir:= ExtractFilePath(FModuleFileName);
        PluginConfDir:= gpCfgDir;
        InputBox:= @fDialogBox.InputBox;
        MessageBox:= @fDialogBox.MessageBox;
        DialogBoxLFM:= @fDialogBox.DialogBoxLFM;
        DialogBoxLRS:= @fDialogBox.DialogBoxLRS;
        DialogBoxLFMFile:= @fDialogBox.DialogBoxLFMFile;
        SendDlgMsg:= @fDialogBox.SendDlgMsg;
      end;

      ExtensionInitialize(@StartupInfo);
    end;
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
    on E: Exception do
    begin
      Result:= False;
      DCDebug(ClassName + '.VFSConfigure(). Error: ' + E.Message);
    end;
  end;
end;

function TWFXModule.VFSRootName: UTF8String;
var
  pcRootName : PAnsiChar;
begin
  Result:= EmptyStr;
  if Assigned(FsGetDefRootName) then
  begin
    pcRootName:= GetMem(MAX_PATH);
    Assert(Assigned(pcRootName));
    try
      FsGetDefRootName(pcRootName, MAX_PATH);
      Result := StrPas(pcRootName);
    finally
      FreeMem(pcRootName);
    end;
  end;
end;

function TWFXModule.IsLoaded: Boolean;
begin
Result := (FModuleHandle <> 0);
end;

{ TWFXModuleList }

function TWFXModuleList.GetAEnabled(Index: Integer): Boolean;
begin
  Result:= Boolean(PtrInt(Objects[Index]));
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
  Objects[Index]:= TObject(PtrInt(AValue));
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
  LEnabled: Boolean;
begin
  Ini.ReadSectionRaw('FileSystemPlugins', Self);
  for I:= 0 to Count - 1 do
  begin
    sCurrPlugin := Name[I];
    if (sCurrPlugin = '') then
      Continue;

    LEnabled := (sCurrPlugin[1] <> '#');
    Enabled[I]:= LEnabled;
    if not LEnabled then
      Name[I]:= Copy(sCurrPlugin, 2, MaxInt);
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
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TWFXModuleList.Save(Ini: TIniFileEx);
var
 I: Integer;
 LName: String;
begin
  Ini.EraseSection('FileSystemPlugins');
  for I := 0 to Count - 1 do
  begin
    if Enabled[I] then
      LName := Name[I]
    else
      LName := '#' + Name[I];

    Ini.WriteString('FileSystemPlugins', LName, FileName[I]);
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
  for Result := 0 to Count - 1 do
    if Enabled[Result] and (DoCompareText(Names[Result], Name) = 0) then
      Exit;
  Result := -1;
end;

end.
