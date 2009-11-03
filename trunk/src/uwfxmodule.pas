{
   Double Commander
   -------------------------------------------------------------------------
   Virtual File System - class for manage WFX plugins (Version 1.3)
 
   Copyright (C) 2007-2009  Koblov Alexander (Alexx2000@mail.ru)
 
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
 SysUtils, Classes, uVFSModule, uVFSTypes, WfxPlugin, uWFXprototypes,
 dynlibs, uClassesEx, DialogAPI, uOSUtils;

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

  TWFXModule = class (TVFSModule)
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
    { Dialog API }
    SetDlgProc: TSetDlgProc;
  public
    function WfxFindFirst(Path: UTF8String; var FindData: TWfxFindData): THandle;
    function WfxFindNext(Hdl: THandle; var FindData: TWfxFindData): Boolean;
    procedure WfxStatusInfo(RemoteDir: UTF8String; InfoStartEnd, InfoOperation: Integer);
    function WfxExecuteFile(MainWin: HWND; RemoteName, Verb: UTF8String): Integer;
    function WfxSetAttr(RemoteName: UTF8String; NewAttr: LongInt): Boolean;
    function WfxRemoveDir(const sDirName: UTF8String): Boolean;
    function WfxDeleteFile(const sFileName: UTF8String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadModule(const sName: String):Boolean;override; {Load plugin}
    procedure UnloadModule;override;
    function VFSInit(Data: PtrInt):Boolean;override;
    procedure VFSDestroy;override;
    function VFSCaps : TVFSCaps;override;

    function VFSConfigure(Parent: THandle):Boolean;override;

    function VFSMisc: PtrUInt; override;
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
    procedure Save(Ini: TIniFileEx); overload;
    function FindFirstEnabledByName(Name: String): Integer;

    property Name[Index: Integer]: String read GetAName write SetAName;
    property FileName[Index: Integer]: String read GetAFileName write SetAFileName;
    property Enabled[Index: Integer]: Boolean read GetAEnabled write SetAEnabled;
  end;

  function GetErrorMsg(iErrorMsg: LongInt): UTF8String;

implementation

uses
  LCLProc, uLng, FileUtil, uGlobsPaths, fDialogBox;

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
    CreationTime:= FileTimeToDateTime(FindData.ftCreationTime);
    LastAccessTime:= FileTimeToDateTime(FindData.ftLastAccessTime);
    LastWriteTime:= FileTimeToDateTime(FindData.ftLastWriteTime);
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
    CreationTime:= FileTimeToDateTime(FindData.ftCreationTime);
    LastAccessTime:= FileTimeToDateTime(FindData.ftLastAccessTime);
    LastWriteTime:= FileTimeToDateTime(FindData.ftLastWriteTime);
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
  if Assigned(FsFindFirstW) then
    begin
      FillChar(FindDataW, SizeOf(FindDataW), 0);
      Result:= FsFindFirstW(PWideChar(UTF8Decode(Path)), FindDataW);
      if Result <> feInvalidHandle then
        FindData:= LoadWfxFindData(FindDataW);
    end
  else if Assigned(FsFindFirst) then
    begin
      FillChar(FindDataA, SizeOf(FindDataA), 0);
      Result:= FsFindFirst(PAnsiChar(UTF8ToSys(Path)), FindDataA);
      if Result <> feInvalidHandle then
        FindData:= LoadWfxFindData(FindDataA);
    end;
end;

function TWFXModule.WfxFindNext(Hdl: THandle; var FindData: TWfxFindData): Boolean;
var
  FindDataA: TWin32FindData;
  FindDataW: TWin32FindDataW;
begin
  if Assigned(FsFindFirstW) then
    begin
      FillChar(FindDataW, SizeOf(FindDataW), 0);
      Result:= FsFindNextW(Hdl, FindDataW);
      if Result then
        FindData:= LoadWfxFindData(FindDataW);
    end
  else if Assigned(FsFindFirst) then
    begin
      FillChar(FindDataA, SizeOf(FindDataA), 0);
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

function TWFXModule.WfxExecuteFile(MainWin: HWND; RemoteName, Verb: UTF8String): Integer;
begin
  if Assigned(FsExecuteFileW) then
    Result:= FsExecuteFileW(MainWin, PWideChar(UTF8Decode(RemoteName)), PWideChar(UTF8Decode(Verb)))
  else if Assigned(FsExecuteFile) then
    Result:= FsExecuteFile(MainWin, PAnsiChar(UTF8ToSys(RemoteName)), PAnsiChar(UTF8ToSys(Verb)))
  else
    Result:= WFX_NOTSUPPORTED;
end;

function TWFXModule.WfxSetAttr(RemoteName: UTF8String; NewAttr: LongInt): Boolean;
begin
  Result:= False;
  if Assigned(FsSetAttrW) then
    Result:= FsSetAttrW(PWideChar(UTF8Decode(RemoteName)), NewAttr)
  else if Assigned(FsSetAttr) then
    Result:= FsSetAttr(PAnsiChar(UTF8ToSys(RemoteName)), NewAttr);
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

constructor TWFXModule.Create;
begin

end;

destructor TWFXModule.Destroy;
begin
//TODO:Remove this and use VFSDestroy
//------------------------------------------------------
  if Assigned(FsContentPluginUnloading) then
  FsContentPluginUnloading;
//------------------------------------------------------
  UnloadModule;
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
{ Dialog API }
  SetDlgProc:= nil;
end;

function TWFXModule.VFSInit(Data: PtrInt): Boolean;
var
  dps: pFsDefaultParamStruct;
  SetDlgProcInfo: TSetDlgProcInfo;
  sPluginDir: WideString;
  sPluginConfDir: WideString;
begin
    if Assigned(FsSetDefaultParams) then
    begin
      GetMem(dps,SizeOf(tFsDefaultParamStruct));
      dps.DefaultIniName:=gpIniDir + WfxIniFileName;
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
      sPluginConfDir := UTF8Decode(gpIniDir);

      with SetDlgProcInfo do
      begin
        PluginDir:= PWideChar(sPluginDir);
        PluginConfDir:= PWideChar(sPluginConfDir);
        InputBox:= @fDialogBox.InputBox;
        MessageBox:= @fDialogBox.MessageBox;
        DialogBox:= @fDialogBox.DialogBox;
        DialogBoxEx:= @fDialogBox.DialogBoxEx;
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

function TWFXModule.VFSCaps: TVFSCaps;
begin
  Result := [];
  if Assigned(FsGetFile) then
    Include(Result, VFS_CAPS_COPYOUT);
  if Assigned(FsPutFile) then
    Include(Result, VFS_CAPS_COPYIN);
  if Assigned(FsDeleteFile) then
    Include(Result, VFS_CAPS_DELETE);
  if Assigned(FsMkDir) then
    Include(Result, VFS_CAPS_MKDIR);
end;

function TWFXModule.VFSConfigure(Parent: System.THandle): Boolean;
begin
  try
    WFXStatusInfo(PathDelim, FS_STATUS_START, FS_STATUS_OP_EXEC);
    Result:= (WfxExecuteFile(Parent, PathDelim, 'properties') = FS_EXEC_OK);
    WFXStatusInfo(PathDelim, FS_STATUS_END, FS_STATUS_OP_EXEC);
  except
    Result:= False;
  end;	
end;

function TWFXModule.VFSMisc: PtrUInt;
var
  pPlgName : PChar;
begin
  New(pPlgName);
  if Assigned(FsGetDefRootName) then
    begin
      FsGetDefRootName(pPlgName, 256);
      Result := PtrUInt(pPlgName);
    end
  else
    Result:=0;
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
