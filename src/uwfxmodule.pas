{
   Double Commander
   -------------------------------------------------------------------------
   Virtual File System - class for manage WFX plugins (Version 1.3)
 
   Copyright (C) 2007-2018 Alexander Koblov (alexx2000@mail.ru)

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
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uWFXmodule;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, WfxPlugin, uWFXprototypes, LazUTF8Classes,
  dynlibs, DCClassesUtf8, Extension, DCBasicTypes, DCXmlConfig,
  uWdxPrototypes, uWdxModule, uFileSource;

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
    FileName : String;
    AlternateFileName : String;
    case Boolean of
    True:  ( FindDataA: TWin32FindData;  );
    False: ( FindDataW: TWin32FindDataW; );
  end;

  { TWFXModule }

  TWFXModule = class(TPluginWDX)
  private
    FModuleFileName: String;
    FBackgroundFlags: Integer;
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
    FsGetBackgroundFlags: TFsGetBackgroundFlags;
    //---------------------
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
    FsContentGetDefaultViewW: TFsContentGetDefaultViewW;
    { Extension API }
    ExtensionInitialize: TExtensionInitializeProc;
    ExtensionFinalize:   TExtensionFinalizeProc;
  public
    function WfxFindFirst(Path: String; var FindData: TWfxFindData): THandle;
    function WfxFindNext(Hdl: THandle; var FindData: TWfxFindData): Boolean;
    procedure WfxStatusInfo(RemoteDir: String; InfoStartEnd, InfoOperation: Integer);
    function WfxExecuteFile(MainWin: HWND; var RemoteName: String; Verb: String): Integer;
    function WfxRenMovFile(OldName, NewName: String; Move, OverWrite: Boolean; RemoteInfo: PRemoteInfo): Integer;
    function WfxGetFile(RemoteName, LocalName: String; CopyFlags: Integer; RemoteInfo: PRemoteInfo): Integer;
    function WfxPutFile(LocalName, RemoteName: String; CopyFlags: Integer): Integer;
    function WfxSetAttr(RemoteName: String; NewAttr: LongInt): Boolean;
    {en
       Each of CreationTime, LastAccessTime, LastWriteTime may be @nil to leave the value unchanged.
    }
    function WfxSetTime(RemoteName: String; pCreationTime, pLastAccessTime, pLastWriteTime: PWfxFileTime): Boolean;
    function WfxMkDir(const sBasePath, sDirName: String): LongInt;
    function WfxRemoveDir(const sDirName: String): Boolean;
    function WfxDeleteFile(const sFileName: String): Boolean;
    function WfxGetLocalName(var sFileName: String): Boolean;
    function WfxDisconnect(const DisconnectRoot: String): Boolean;
    function WfxContentGetDefaultView(out DefaultView: TFileSourceFields): Boolean;
  private
    function LoadModule(const sName: String):Boolean; overload; {Load plugin}
    procedure UnloadModule; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure VFSInit;

    function VFSConfigure(Parent: HWND):Boolean;
    function VFSRootName: String;

    function ContentPlugin: Boolean;

    property BackgroundFlags: Integer read FBackgroundFlags write FBackgroundFlags;
  end;

  { TWFXModuleList }

  TWFXModuleList = class(TStringList)
  private
    FModuleList: TStringListUTF8;
  private
    function GetAEnabled(Index: Integer): Boolean;
    function GetAFileName(Index: Integer): String;
    function GetAName(Index: Integer): String;
    procedure SetAEnabled(Index: Integer; const AValue: Boolean);
    procedure SetAFileName(Index: Integer; const AValue: String);
    procedure SetAName(Index: Integer; const AValue: String);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  public
    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode); overload;
    function ComputeSignature(seed: dword): dword;
    function Add(Ext: String; FileName: String): Integer; reintroduce;
    function FindFirstEnabledByName(Name: String): Integer;
    function LoadModule(const FileName: String): TWfxModule;

    property Name[Index: Integer]: String read GetAName write SetAName;
    property FileName[Index: Integer]: String read GetAFileName write SetAFileName;
    property Enabled[Index: Integer]: Boolean read GetAEnabled write SetAEnabled;
  end;

  function GetErrorMsg(iErrorMsg: LongInt): String;

implementation

uses
  //Lazarus, Free-Pascal, etc.
  LazUTF8, FileUtil,

  //DC
  uDCUtils, uLng, uGlobsPaths, uOSUtils, uWfxPluginUtil, fDialogBox, DCOSUtils,
  DCStrUtils, DCConvertEncoding, uComponentsSignature, uOSForms;

const
  WfxIniFileName = 'wfx.ini';

function GetErrorMsg(iErrorMsg: LongInt): String;
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

function ConvertString(const S: String): TStringArray;
var
  Item: String = '';
  Index: Integer = 1;
begin
  Result:= Default(TStringArray);
  while Index < High(S) do
  begin
    if S[Index] = '\' then
    begin
      case S[Index + 1] of
        '\':
        begin
          Item += '\';
          Inc(Index, 2);
          Continue;
        end;
        'n':
        begin
          AddString(Result, Item);
          Item:= EmptyStr;
          Inc(Index, 2);
          Continue;
        end;
      end;
    end;
    Item += S[Index];
    Inc(Index);
  end;
  if Length(Item) > 0 then
  begin
    AddString(Result, Item + S[High(S)]);
  end;
end;

function ConvertFunction(const S: String): String;
var
  AValues: TStringArray;
  Plugin, Field, Arg: String;
begin
  Result:= EmptyStr;
  if Length(S) < 3 then Exit;
  if not StrBegins(S, '[=') then Exit(S);
  if (S[Low(S)] = '[') and (S[High(S)] = ']') then
  begin
    AValues:= (Copy(S, 2, Length(S) - 2)).Split(['.']);
    if (Length(AValues) > 1) then
    begin
      Plugin:= LowerCase(AValues[0]);
      if (Plugin = '=<fs>') then
        Result:= 'Plugin(FS)'
      else if (Plugin = '=tc') then
      begin
        Result:= 'DC()';
        Field:= LowerCase(AValues[1]);
        if (Field = 'writedate') then
          AValues[1]:= 'GETFILETIME'
        else if (Field = 'attributestr') then
          AValues[1]:= 'GETFILEATTR'
        else if (Field = 'writetime') then
        begin
          AValues[1]:= 'GETFILETIME';
          if (Length(AValues) = 2) then
          begin
            AddString(AValues, DefaultFormatSettings.LongTimeFormat);
          end;
        end
        else if (Field = 'size') then
        begin
          AValues[1]:= 'GETFILESIZE';
          if (Length(AValues) = 3) then
          begin
            Arg:= LowerCase(AValues[2]);
            if (Arg = 'bytes') then
              AValues[2]:= 'BYTE'
            else if (Arg = 'kbytes') then
              AValues[2]:= 'KILO'
            else if (Arg = 'mbytes') then
              AValues[2]:= 'MEGA'
            else if (Arg = 'gbytes') then
              AValues[2]:= 'GIGA'
            else
              AValues[2]:= 'FLOAT';
          end;
        end;
      end;
      if (Length(AValues) = 2) then
        Result+= '.' + AValues[1] + '{}'
      else begin
        Result+= '.' + AValues[1] + '{' + AValues[2] + '}';
      end;
      Result:= '[' + Result + ']';
    end;
  end;
end;

{ TWfxFindData }

procedure ConvertFindData(var FindData: TWfxFindData; AnsiData: Boolean);
begin
  with FindData do
  begin
    // Convert file attributes
    FileAttributes:= FindDataW.dwFileAttributes;
    CreationTime:= WfxFileTimeToDateTime(FindDataW.ftCreationTime);
    LastAccessTime:= WfxFileTimeToDateTime(FindDataW.ftLastAccessTime);
    LastWriteTime:= WfxFileTimeToDateTime(FindDataW.ftLastWriteTime);
    Int64Rec(FileSize).Lo:= FindDataW.nFileSizeLow;
    Int64Rec(FileSize).Hi:= FindDataW.nFileSizeHigh;
    Reserved0:= FindDataW.dwReserved0;
    Reserved1:= FindDataW.dwReserved1;
    // Convert file name
    if AnsiData then
      begin
        FileName:= CeSysToUtf8(FindDataA.cFileName);
        AlternateFileName:= CeSysToUtf8(FindDataA.cAlternateFileName);
      end
    else
      begin
        FileName:= UTF16ToUTF8(UnicodeString(FindDataW.cFileName));
        AlternateFileName:= UTF16ToUTF8(UnicodeString(FindDataW.cAlternateFileName));
      end;
  end;
end;

{ TWFXModule }

function TWFXModule.WfxFindFirst(Path: String; var FindData: TWfxFindData): THandle;
begin
  try
    if Assigned(FsFindFirstW) then
      begin
        Result:= FsFindFirstW(PWideChar(CeUtf8ToUtf16(Path)), FindData.FindDataW);
        if Result <> wfxInvalidHandle then ConvertFindData(FindData, False);
      end
    else if Assigned(FsFindFirst) then
      begin
        Result:= FsFindFirst(PAnsiChar(CeUtf8ToSys(Path)), FindData.FindDataA);
        if Result <> wfxInvalidHandle then ConvertFindData(FindData, True);
      end;
  except
    on E: Exception do
    begin
      Result:= wfxInvalidHandle;
    end;
  end;
end;

function TWFXModule.WfxFindNext(Hdl: THandle; var FindData: TWfxFindData): Boolean;
begin
  if Assigned(FsFindFirstW) then
    begin
      Result:= FsFindNextW(Hdl, FindData.FindDataW);
      if Result then ConvertFindData(FindData, False);
    end
  else if Assigned(FsFindFirst) then
    begin
      Result:= FsFindNext(Hdl, FindData.FindDataA);
      if Result then ConvertFindData(FindData, True);
    end
  else Result:= False;
end;

procedure TWFXModule.WfxStatusInfo(RemoteDir: String; InfoStartEnd,
  InfoOperation: Integer);
begin
  if Assigned(FsStatusInfoW) then
    FsStatusInfoW(PWideChar(CeUtf8ToUtf16(RemoteDir)), InfoStartEnd, InfoOperation)
  else if Assigned(FsStatusInfo) then
    FsStatusInfo(PAnsiChar(CeUtf8ToSys(RemoteDir)), InfoStartEnd, InfoOperation);
end;

function TWFXModule.WfxExecuteFile(MainWin: HWND; var RemoteName: String; Verb: String): Integer;
var
  pacRemoteName: PAnsiChar;
  pwcRemoteName: PWideChar;
begin
  Result:= WFX_NOTSUPPORTED;
  MainWin:= GetWindowHandle(MainWin);
  if Assigned(FsExecuteFileW) then
    begin
      pwcRemoteName:= GetMem(MAX_PATH * SizeOf(WideChar));
      StrPCopyW(pwcRemoteName, CeUtf8ToUtf16(RemoteName));
      Result:= FsExecuteFileW(MainWin, pwcRemoteName, PWideChar(CeUtf8ToUtf16(Verb)));
      if Result = FS_EXEC_SYMLINK then
          RemoteName:= UTF16ToUTF8(UnicodeString(pwcRemoteName));
      FreeMem(pwcRemoteName);
    end
  else if Assigned(FsExecuteFile) then
    begin
      pacRemoteName:= GetMem(MAX_PATH);
      StrPCopy(pacRemoteName, CeUtf8ToSys(RemoteName));
      Result:= FsExecuteFile(MainWin, pacRemoteName, PAnsiChar(CeUtf8ToSys(Verb)));
      if Result = FS_EXEC_SYMLINK then
          RemoteName:= CeSysToUtf8(StrPas(pacRemoteName));
      FreeMem(pacRemoteName);
    end;
end;

function TWFXModule.WfxRenMovFile(OldName, NewName: String; Move,
                                  OverWrite: Boolean; RemoteInfo: PRemoteInfo): Integer;
begin
  Result:= FS_FILE_NOTSUPPORTED;
  if Assigned(FsRenMovFileW) then
    Result:= FsRenMovFileW(PWideChar(CeUtf8ToUtf16(OldName)), PWideChar(CeUtf8ToUtf16(NewName)), Move, OverWrite, RemoteInfo)
  else if Assigned(FsRenMovFile) then
    Result:= FsRenMovFile(PAnsiChar(CeUtf8ToSys(OldName)), PAnsiChar(CeUtf8ToSys(NewName)), Move, OverWrite, RemoteInfo);
end;

function TWFXModule.WfxGetFile(RemoteName, LocalName: String;
                               CopyFlags: Integer; RemoteInfo: PRemoteInfo): Integer;
begin
  Result:= FS_FILE_NOTSUPPORTED;
  if Assigned(FsGetFileW) then
    Result:= FsGetFileW(PWideChar(CeUtf8ToUtf16(RemoteName)), PWideChar(CeUtf8ToUtf16(LocalName)), CopyFlags, RemoteInfo)
  else if Assigned(FsGetFile) then
    Result:= FsGetFile(PAnsiChar(CeUtf8ToSys(RemoteName)), PAnsiChar(CeUtf8ToSys(LocalName)), CopyFlags, RemoteInfo);
end;

function TWFXModule.WfxPutFile(LocalName, RemoteName: String; CopyFlags: Integer): Integer;
begin
  Result:= FS_FILE_NOTSUPPORTED;
  if Assigned(FsPutFileW) then
    Result:= FsPutFileW(PWideChar(CeUtf8ToUtf16(LocalName)), PWideChar(CeUtf8ToUtf16(RemoteName)), CopyFlags)
  else if Assigned(FsPutFile) then
    Result:= FsPutFile(PAnsiChar(CeUtf8ToSys(LocalName)), PAnsiChar(CeUtf8ToSys(RemoteName)), CopyFlags);
end;

function TWFXModule.WfxSetAttr(RemoteName: String; NewAttr: LongInt): Boolean;
begin
  Result:= False;
  if Assigned(FsSetAttrW) then
    Result:= FsSetAttrW(PWideChar(CeUtf8ToUtf16(RemoteName)), NewAttr)
  else if Assigned(FsSetAttr) then
    Result:= FsSetAttr(PAnsiChar(CeUtf8ToSys(RemoteName)), NewAttr);
end;

function TWFXModule.WfxSetTime(RemoteName: String; pCreationTime,
                               pLastAccessTime, pLastWriteTime: PWfxFileTime): Boolean;
begin
  Result:= False;
  if Assigned(FsSetTimeW) then
    Result:= FsSetTimeW(PWideChar(CeUtf8ToUtf16(RemoteName)), pCreationTime, pLastAccessTime, pLastWriteTime)
  else if Assigned(FsSetTime) then
    Result:= FsSetTime(PAnsiChar(CeUtf8ToSys(RemoteName)), pCreationTime, pLastAccessTime, pLastWriteTime);
end;

function TWFXModule.WfxMkDir(const sBasePath, sDirName: String): LongInt;
begin
  Result:= WFX_NOTSUPPORTED;
  if Assigned(FsMkDirW) then
    begin
      WfxStatusInfo(sBasePath, FS_STATUS_START, FS_STATUS_OP_MKDIR);
      if FsMkDirW(PWideChar(CeUtf8ToUtf16(sDirName))) then
        Result:= WFX_SUCCESS
      else
        Result:= WFX_ERROR;
      WfxStatusInfo(sBasePath, FS_STATUS_END, FS_STATUS_OP_MKDIR);
    end
  else if Assigned(FsMkDir) then
    begin
      WfxStatusInfo(sBasePath, FS_STATUS_START, FS_STATUS_OP_MKDIR);
      if FsMkDir(PAnsiChar(CeUtf8ToSys(sDirName))) then
        Result:= WFX_SUCCESS
      else
        Result:= WFX_ERROR;
      WfxStatusInfo(sBasePath, FS_STATUS_END, FS_STATUS_OP_MKDIR);
    end;
end;

function TWFXModule.WfxRemoveDir(const sDirName: String): Boolean;
begin
  Result:= False;
  if Assigned(FsRemoveDirW) then
    Result:= FsRemoveDirW(PWideChar(CeUtf8ToUtf16(sDirName)))
  else if Assigned(FsRemoveDir) then
    Result:= FsRemoveDir(PAnsiChar(CeUtf8ToSys(sDirName)));
end;

function TWFXModule.WfxDeleteFile(const sFileName: String): Boolean;
begin
  Result:= False;
  if Assigned(FsDeleteFileW) then
    Result:= FsDeleteFileW(PWideChar(CeUtf8ToUtf16(sFileName)))
  else if Assigned(FsDeleteFile) then
    Result:= FsDeleteFile(PAnsiChar(CeUtf8ToSys(sFileName)));
end;

function TWFXModule.WfxGetLocalName(var sFileName: String): Boolean;
var
  pacRemoteName: PAnsiChar;
  pwcRemoteName: PWideChar;
begin
  Result:= False;
  if Assigned(FsGetLocalNameW) then
    begin
      pwcRemoteName:= GetMem(MAX_PATH * SizeOf(WideChar));
      StrPCopyW(pwcRemoteName, CeUtf8ToUtf16(sFileName));
      Result:= FsGetLocalNameW(pwcRemoteName, MAX_PATH);
      if Result = True then
        sFileName:= UTF16ToUTF8(UnicodeString(pwcRemoteName));
      FreeMem(pwcRemoteName);
    end
  else if Assigned(FsGetLocalName) then
    begin
      pacRemoteName:= GetMem(MAX_PATH);
      StrPCopy(pacRemoteName, CeUtf8ToSys(sFileName));
      Result:= FsGetLocalName(pacRemoteName, MAX_PATH);
      if Result = True then
        sFileName:= CeSysToUtf8(StrPas(pacRemoteName));
      FreeMem(pacRemoteName);
    end;
end;

function TWFXModule.WfxDisconnect(const DisconnectRoot: String): Boolean;
begin
  if Assigned(FsDisconnectW) then
    Result:= FsDisconnectW(PWideChar(CeUtf8ToUtf16(DisconnectRoot)))
  else if Assigned(FsDisconnect) then
    Result:= FsDisconnect(PAnsiChar(CeUtf8ToSys(DisconnectRoot)))
  else
    Result:= False;
end;

function TWFXModule.WfxContentGetDefaultView(out DefaultView: TFileSourceFields): Boolean;
const
  MAX_LEN = 4096;
var
  Index: Integer;
  ViewContents, ViewHeaders,
  ViewWidths, ViewOptions: TStringArray;
  usContents, usHeaders, usWidths, usOptions: String;
  asContents, asHeaders, asWidths, asOptions: array[0..MAX_LEN] of AnsiChar;
  wsContents, wsHeaders, wsWidths, wsOptions: array[0..MAX_LEN] of WideChar;
begin
  Result:= False;
  DefaultView:= Default(TFileSourceFields);

  if Assigned(FsContentGetDefaultViewW) then
  begin
    Result:= FsContentGetDefaultViewW(wsContents, wsHeaders, wsWidths, wsOptions, MAX_LEN);
    if Result then
    begin
      usContents:= CeUtf16ToUtf8(wsContents);
      usHeaders:= CeUtf16ToUtf8(wsHeaders);
      usWidths:= CeUtf16ToUtf8(wsWidths);
      usOptions:= CeUtf16ToUtf8(wsOptions);
    end;
  end
  else if Assigned(FsContentGetDefaultView) then
  begin
    Result:= FsContentGetDefaultView(asContents, asHeaders, asWidths, asOptions, MAX_LEN);
    if Result then
    begin
      usContents:= CeSysToUtf8(asContents);
      usHeaders:= CeSysToUtf8(asHeaders);
      usWidths:= CeSysToUtf8(asWidths);
      usOptions:= CeSysToUtf8(asOptions);
    end;
  end;
  if Result then
  begin
    ViewHeaders:= ConvertString(usHeaders);
    ViewWidths:= SplitString(usWidths, ',');
    ViewOptions:= SplitString(usOptions,'|');
    ViewContents:= ConvertString(usContents);

    SetLength(DefaultView, Length(ViewWidths));

    for Index:= Low(DefaultView) to High(DefaultView) do
    begin
      if (Index = 0) then
      begin
        DefaultView[Index].Header:= rsColName;
        DefaultView[Index].Content:= '[DC().GETFILENAMENOEXT{}]';
      end
      else if (Index = 1) then
      begin
        DefaultView[Index].Header:= rsColExt;
        DefaultView[Index].Content:= '[DC().GETFILEEXT{}]';
      end
      else begin
        DefaultView[Index].Header:= ViewHeaders[Index - 2];
        DefaultView[Index].Content:= ConvertFunction(ViewContents[Index - 2]);
      end;
      DefaultView[Index].Width:= StrToInt(ViewWidths[Index]);
      if (DefaultView[Index].Width < 0) then
      begin
        DefaultView[Index].Align:= taRightJustify;
        DefaultView[Index].Width:= Abs(DefaultView[Index].Width);
      end
      else begin
        DefaultView[Index].Align:= taLeftJustify;
      end;
    end;
  end;
end;

constructor TWFXModule.Create;
begin
  inherited;
  FName:= 'FS';
end;

destructor TWFXModule.Destroy;
begin
  if IsLoaded then
  begin
    if Assigned(ContentPluginUnloading) then
      ContentPluginUnloading;
    if Assigned(ExtensionFinalize) then
      ExtensionFinalize(nil);
  end;
  inherited Destroy;
end;

function TWFXModule.LoadModule(const sName: String): Boolean;
var
  AHandle: TLibHandle;
begin
  EnterCriticalSection(FMutex);
  try
    if FModuleHandle <> NilHandle then Exit(True);
    AHandle := mbLoadLibrary(mbExpandFileName(sName));
    Result := AHandle <> NilHandle;
    if not Result then Exit;

    FModuleFileName:= sName;
  { Mandatory }
    FsInit := TFsInit(GetProcAddress(AHandle,'FsInit'));
    FsFindFirst := TFsFindFirst(GetProcAddress(AHandle,'FsFindFirst'));
    FsFindNext := TFsFindNext(GetProcAddress(AHandle,'FsFindNext'));
    FsFindClose := TFsFindClose(GetProcAddress(AHandle,'FsFindClose'));
  { Unicode }
    FsInitW := TFsInitW(GetProcAddress(AHandle,'FsInitW'));
    FsFindFirstW := TFsFindFirstW(GetProcAddress(AHandle,'FsFindFirstW'));
    FsFindNextW := TFsFindNextW(GetProcAddress(AHandle,'FsFindNextW'));

    Result:= (FsInit <> nil) and (FsFindFirst <> nil) and (FsFindNext <> nil);
    if (Result = False) then
    begin
      FsInit:= nil;
      FsFindFirst:= nil;
      FsFindNext:= nil;
      Result:= (FsInitW <> nil) and (FsFindFirstW <> nil) and (FsFindNextW <> nil);
    end;

    if (Result = False) or (FsFindClose = nil) then
    begin
      FsInitW:= nil;
      FsFindFirstW:= nil;
      FsFindNextW:= nil;
      FsFindClose:= nil;
      FreeLibrary(AHandle);
      Exit(False);
    end;

  { Optional }
    FsSetCryptCallback:= TFsSetCryptCallback(GetProcAddress(AHandle,'FsSetCryptCallback'));
    FsGetDefRootName := TFsGetDefRootName(GetProcAddress(AHandle,'FsGetDefRootName'));
    FsExecuteFile := TFsExecuteFile(GetProcAddress(AHandle,'FsExecuteFile'));
    FsGetFile := TFsGetFile(GetProcAddress(AHandle,'FsGetFile'));
    FsPutFile := TFsPutFile(GetProcAddress(AHandle,'FsPutFile'));
    FsDeleteFile := TFsDeleteFile(GetProcAddress(AHandle,'FsDeleteFile'));
    FsMkDir := TFsMkDir(GetProcAddress(AHandle,'FsMkDir'));
    FsRemoveDir := TFsRemoveDir(GetProcAddress(AHandle,'FsRemoveDir'));
    FsStatusInfo := TFsStatusInfo(GetProcAddress(AHandle,'FsStatusInfo'));
    FsSetDefaultParams := TFsSetDefaultParams(GetProcAddress(AHandle,'FsSetDefaultParams'));
    //---------------------
    FsSetAttr := TFsSetAttr (GetProcAddress(AHandle,'FsSetAttr'));
    FsSetTime := TFsSetTime (GetProcAddress(AHandle,'FsSetTime'));
    FsExtractCustomIcon := TFsExtractCustomIcon (GetProcAddress(AHandle,'FsExtractCustomIcon'));
    FsRenMovFile := TFsRenMovFile (GetProcAddress(AHandle,'FsRenMovFile'));
    FsDisconnect := TFsDisconnect (GetProcAddress(AHandle,'FsDisconnect'));
    FsGetPreviewBitmap := TFsGetPreviewBitmap (GetProcAddress(AHandle,'FsGetPreviewBitmap'));
    FsLinksToLocalFiles := TFsLinksToLocalFiles (GetProcAddress(AHandle,'FsLinksToLocalFiles'));
    FsGetLocalName := TFsGetLocalName (GetProcAddress(AHandle,'FsGetLocalName'));
    //---------------------
    FsGetBackgroundFlags := TFsGetBackgroundFlags (GetProcAddress(AHandle,'FsGetBackgroundFlags'));
    //---------------------
    FsContentGetDefaultView := TFsContentGetDefaultView (GetProcAddress(AHandle,'FsContentGetDefaultView'));
    ContentSetDefaultParams := TContentSetDefaultParams (GetProcAddress(AHandle,'FsContentSetDefaultParams'));
    ContentGetDetectString := TFsContentGetDetectString (GetProcAddress(AHandle,'FsContentGetDetectString'));
    ContentGetSupportedField := TFsContentGetSupportedField (GetProcAddress(AHandle,'FsContentGetSupportedField'));
    ContentGetValue := TFsContentGetValue (GetProcAddress(AHandle,'FsContentGetValue'));
    ContentStopGetValue := TFsContentStopGetValue (GetProcAddress(AHandle,'FsContentStopGetValue'));
    ContentGetDefaultSortOrder := TFsContentGetDefaultSortOrder (GetProcAddress(AHandle,'FsContentGetDefaultSortOrder'));
    ContentGetSupportedFieldFlags := TFsContentGetSupportedFieldFlags (GetProcAddress(AHandle,'FsContentGetSupportedFieldFlags'));
    ContentSetValue := TFsContentSetValue (GetProcAddress(AHandle,'FsContentSetValue'));
    ContentPluginUnloading := TFsContentPluginUnloading(GetProcAddress(AHandle,'FsContentPluginUnloading'));
  { Unicode }
    FsSetCryptCallbackW:= TFsSetCryptCallbackW(GetProcAddress(AHandle,'FsSetCryptCallbackW'));
    FsMkDirW := TFsMkDirW(GetProcAddress(AHandle,'FsMkDirW'));
    FsExecuteFileW := TFsExecuteFileW(GetProcAddress(AHandle,'FsExecuteFileW'));
    FsRenMovFileW := TFsRenMovFileW(GetProcAddress(AHandle,'FsRenMovFileW'));
    FsGetFileW := TFsGetFileW(GetProcAddress(AHandle,'FsGetFileW'));
    FsPutFileW := TFsPutFileW(GetProcAddress(AHandle,'FsPutFileW'));
    FsDeleteFileW := TFsDeleteFileW(GetProcAddress(AHandle,'FsDeleteFileW'));
    FsRemoveDirW := TFsRemoveDirW(GetProcAddress(AHandle,'FsRemoveDirW'));
    FsDisconnectW := TFsDisconnectW(GetProcAddress(AHandle,'FsDisconnectW'));
    FsSetAttrW := TFsSetAttrW (GetProcAddress(AHandle,'FsSetAttrW'));
    FsSetTimeW := TFsSetTimeW (GetProcAddress(AHandle,'FsSetTimeW'));
    FsStatusInfoW := TFsStatusInfoW(GetProcAddress(AHandle,'FsStatusInfoW'));
    FsExtractCustomIconW := TFsExtractCustomIconW(GetProcAddress(AHandle,'FsExtractCustomIconW'));
    FsGetLocalNameW := TFsGetLocalNameW(GetProcAddress(AHandle,'FsGetLocalNameW'));
    //--------------------------
    FsContentGetDefaultViewW := TFsContentGetDefaultViewW(GetProcAddress(AHandle,'FsContentGetDefaultViewW'));
    ContentGetValueW := TFsContentGetValueW(GetProcAddress(AHandle, 'FsContentGetValueW'));
    ContentStopGetValueW := TFsContentStopGetValueW(GetProcAddress(AHandle, 'FsContentStopGetValueW'));
    ContentSetValueW := TFsContentSetValueW(GetProcAddress(AHandle, 'FsContentSetValueW'));
    { Extension API }
    ExtensionInitialize:= TExtensionInitializeProc(GetProcAddress(AHandle,'ExtensionInitialize'));
    ExtensionFinalize:= TExtensionFinalizeProc(GetProcAddress(AHandle,'ExtensionFinalize'));

    VFSInit;

    FModuleHandle := AHandle;
  finally
    LeaveCriticalSection(FMutex);
  end;
end;

procedure TWFXModule.UnloadModule;
var
  AHandle: TLibHandle;
begin
  EnterCriticalSection(FMutex);
  try
    if FModuleHandle <> NilHandle then
    begin
      AHandle:= FModuleHandle;
      FModuleHandle := NilHandle;
      FreeLibrary(AHandle);
    end;

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
    FsGetBackgroundFlags := nil;
    //---------------------
    FsContentGetDefaultView := nil;
    ContentGetDetectString := nil;
    ContentGetSupportedField := nil;
    ContentGetValue := nil;
    ContentSetDefaultParams := nil;
    ContentStopGetValue := nil;
    ContentGetDefaultSortOrder := nil;
    ContentGetSupportedFieldFlags := nil;
    ContentSetValue := nil;
    ContentPluginUnloading := nil;
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
    //---------------------
    FsContentGetDefaultViewW := nil;
    ContentGetValueW := nil;
    ContentStopGetValueW := nil;
    ContentSetValueW := nil;
    // Extension API
    ExtensionInitialize:= nil;
    ExtensionFinalize:= nil;
  finally
    LeaveCriticalSection(FMutex);
  end;
end;

procedure TWFXModule.VFSInit;
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

  if not Assigned(FsGetBackgroundFlags) then
    FBackgroundFlags:= 0
  else
    FBackgroundFlags:= FsGetBackgroundFlags();

  // Extension API
  if Assigned(ExtensionInitialize) then
    begin
      FillByte(StartupInfo, SizeOf(TExtensionStartupInfo), 0);

      with StartupInfo do
      begin
        StructSize:= SizeOf(TExtensionStartupInfo);
        PluginDir:= ExtractFilePath(mbExpandFileName(FModuleFileName));
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

  CallContentSetDefaultParams;
  CallContentGetSupportedField;
  if Length(Self.DetectStr) = 0 then
    Self.DetectStr := CallContentGetDetectString;
end;

function TWFXModule.VFSConfigure(Parent: HWND): Boolean;
var
  RemoteName: String;
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
    end;
  end;
end;

function TWFXModule.VFSRootName: String;
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

function TWFXModule.ContentPlugin: Boolean;
begin
  Result:= Assigned(ContentGetValue) or Assigned(ContentGetValueW);
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

constructor TWFXModuleList.Create;
begin
  FModuleList:= TStringListUTF8.Create;
  FModuleList.Sorted:= True;
end;

destructor TWFXModuleList.Destroy;
var
  I: Integer;
begin
  for I:= 0 to FModuleList.Count - 1 do
  begin
    TWfxModule(FModuleList.Objects[I]).Free;
  end;
  FreeAndNil(FModuleList);
  inherited Destroy;
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
        if AConfig.TryGetValue(ANode, 'Name', AName) and AConfig.TryGetValue(ANode, 'Path', APath) then
        begin
          I := Add(AName, APath);
          Enabled[I] := AConfig.GetAttr(ANode, 'Enabled', True);
        end;
      end;
      ANode := ANode.NextSibling;
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

{ TWFXModuleList.ComputeSignature }
function TWFXModuleList.ComputeSignature(seed: dword): dword;
var
  iIndex: integer;
begin
  result := seed;
  for iIndex := 0 to pred(Count) do
  begin
    result := ComputeSignatureBoolean(result, Enabled[iIndex]);
    result := ComputeSignatureString(result, Name[iIndex]);
    result := ComputeSignatureString(result, FileName[iIndex]);
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

function TWFXModuleList.LoadModule(const FileName: String): TWfxModule;
var
  Index: Integer;
begin
  if FModuleList.Find(FileName, Index) then
    Result := TWfxModule(FModuleList.Objects[Index])
  else begin
    Result := TWfxModule.Create;
    if not Result.LoadModule(FileName) then
      FreeAndNil(Result)
    else begin
      FModuleList.AddObject(FileName, Result);
    end;
  end;
end;

end.
