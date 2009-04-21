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
 sysutils, Classes, uFileList, uVFSModule, uVFSTypes, ufsplugin, uWFXprototypes,
 dynlibs, uTypes, fFileOpDlg, uClassesEx;

const
  OP_COPYOUT = 0;
  OP_COPYIN = 1;

  INVALID_HANDLE_VALUE = THandle(-1);

Type
  TWFXModule = class;

  { CopyIn/CopyOut thread }

   { TWFXCopyThread }

   TWFXCopyThread = class(TThread)
   protected
     procedure Execute; override;
   public
     Operation : Integer;
     WFXModule : TWFXModule;
   end;

  { TWFXModule }

  TWFXModule = class (TVFSModule)
  private
    FModuleHandle:TLibHandle;  // Handle to .DLL or .so
    FFileList : TFileList;
    FFlags : Integer;
    FDstPath,
    FFolder : String; // current directory
    FLastFileSize,
    FFilesSize: Int64;
    FPercent : Double;
    CT : TWFXCopyThread;         // CopyIn/CopyOut thread
    FFileOpDlg: TfrmFileOp; // progress window
    function WFXCopyOut : Boolean; {Copy files from VFS}
    function WFXCopyIn : Boolean;  {Copy files in VFS}
  protected
  {Mandatory}
    FsInit : TFsInit;
    FsFindFirst : TFsFindFirst;
    FsFindNext : TFsFindNext;
    FsFindClose : TFsFindClose;
  {Optional}
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
    procedure FsFillAndCount(var fl:TFileList; out FilesSize : Int64);
    procedure WFXStatusInfo(RemoteDir: String; InfoStartEnd, InfoOperation: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function LoadModule(const sName:String):Boolean;override; {Load plugin}
    procedure UnloadModule;override;
    function VFSInit(Data: PtrInt):Boolean;override;
    procedure VFSDestroy;override;
    function VFSCaps : TVFSCaps;override;

    function VFSConfigure(Parent: THandle):Boolean;override;
    function VFSOpen(const sName:String; bCanYouHandleThisFile : Boolean = False):Boolean;override;
//    function VFSClose:Boolean;override;
    function VFSRefresh : Boolean;override;
    
    function VFSMkDir(const sDirName:String ):Boolean;override;
    function VFSRmDir(const sDirName:String):Boolean;override;
    
    function VFSCopyOut(var flSrcList : TFileList; sDstPath:String; Flags: Integer):Boolean;override;
    function VFSCopyIn(var flSrcList : TFileList; sDstName:String; Flags : Integer):Boolean;override;
    function VFSCopyOutEx(var flSrcList : TFileList; sDstPath:String; Flags: Integer):Boolean;override;
    function VFSCopyInEx(var flSrcList : TFileList; sDstName:String; Flags : Integer):Boolean;override;
    function VFSRename(const sSrcName, sDstName:String):Boolean;override;
    function VFSRun(const sName:String):Boolean;override;
    function VFSDelete(var flNameList:TFileList):Boolean;override;
    
    function VFSList(const sDir:String; var fl:TFileList):Boolean;override;
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

implementation
uses
  LCLProc, LCLType, uGlobs, uLog, uVFSutil, uFileOp, uOSUtils, uLng,
  Dialogs, Forms, Controls, FileUtil;

var
  WFXModuleList: TList = nil;
  
{ TWFXModule }

procedure TWFXModule.FsFillAndCount(var fl: TFileList; out FilesSize: Int64);
var
  I:Integer;
  ptr:PFileRecItem;
  sRealName : String;
  NewFileList: TFileList;

procedure FillAndCountRec(const srcPath, dstPath:String);
var
  FindData : TWIN32FINDDATA;
  Handle:THandle;
  fr:TFileRecItem;

begin
  Handle := FsFindFirst(PChar(UTF8ToSys(srcPath)), FindData);
  if Handle = INVALID_HANDLE_VALUE then
  begin
    Exit;
  end;
  repeat
    if (FindData.cFileName='.') or (FindData.cFileName='..') then Continue;
    fr.sName:=ExtractDirLevel(fl.CurrentDirectory, srcPath+SysToUTF8(FindData.cFileName));
    fr.sPath:=dstPath;
    fr.sNameNoExt:=SysToUTF8(FindData.cFileName); // we use to save dstname

    fr.iMode := FindData.dwFileAttributes;

    fr.bSelected:=False;
    fr.iSize := Int64(FindData.nFileSizeHigh) shl 32 + FindData.nFileSizeLow;;

    NewFileList.AddItem(@fr);

    if FPS_ISDIR(fr.iMode) then
      begin
        FillAndCountRec(srcPath+SysToUTF8(FindData.cFileName)+DirectorySeparator, dstPath+SysToUTF8(FindData.cFileName)+DirectorySeparator);
      end
    else
      inc(FilesSize, fr.iSize);
  until not FsFindNext(Handle, FindData);
  FsFindClose(Handle);
end;



begin
  NewFileList:=TFileList.Create;
  NewFileList.CurrentDirectory := fl.CurrentDirectory;
  for I:=0 to fl.Count-1 do
  begin
    ptr:=fl.GetItem(I);

    if FPS_ISDIR(ptr^.iMode) and (not ptr^.bLinkIsDir) then
    begin
      sRealName := ptr^.sName;
      ptr^.sName := ExtractDirLevel(fl.CurrentDirectory, ptr^.sName);
      NewFileList.AddItem(ptr); // add DIR to List
      FillAndCountRec(sRealName + DirectorySeparator, ptr^.sNameNoExt + DirectorySeparator);  // rekursive browse child dir
    end
    else
    begin
      ptr^.sName := ExtractDirLevel(fl.CurrentDirectory, ptr^.sName);
      NewFileList.AddItem(ptr);
      inc(FilesSize, ptr^.iSize);
    end;
  end;
  fl.Free;
  fl := NewFileList;
end;

procedure TWFXModule.WFXStatusInfo(RemoteDir: String; InfoStartEnd, InfoOperation: Integer);
begin
  if Assigned(FsStatusInfo) then
    FsStatusInfo(PChar(UTF8ToSys(RemoteDir)), InfoStartEnd, InfoOperation);
end;

constructor TWFXModule.Create;
begin
  FFilesSize := 0;
  FPercent := 0;
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
  if  FModuleHandle = 0 then exit;
{Mandatory}
  FsInit := TFsInit(GetProcAddress(FModuleHandle,'FsInit'));
  FsFindFirst := TFsFindFirst(GetProcAddress(FModuleHandle,'FsFindFirst'));
  FsFindNext := TFsFindNext(GetProcAddress(FModuleHandle,'FsFindNext'));
  FsFindClose := TFsFindClose(GetProcAddress(FModuleHandle,'FsFindClose'));
{Optional}
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

  
end;

procedure TWFXModule.UnloadModule;
begin
  if FModuleHandle <> 0 then
    FreeLibrary(FModuleHandle);
  FModuleHandle := 0;
{Mandatory}
  FsInit := nil;
  FsFindFirst := nil;
  FsFindNext := nil;
  FsFindClose := nil;
{Optional}
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

end;

{CallBack functions}
function MainProgressProc (PluginNr:integer;SourceName,TargetName:pchar;PercentDone:integer):integer;stdcall;
begin
  Result := 0;
  DebugLN ('MainProgressProc ('+IntToStr(PluginNr)+','+SourceName+','+TargetName+','+inttostr(PercentDone)+')' ,inttostr(result));

  with TWFXModule(WFXModuleList.Items[PluginNr]) do
  begin
    if FFileOpDlg.ModalResult = mrCancel then // Cancel operation
      Result := 1;
  
    DebugLN('Percent1 = ' + IntToStr(PercentDone));

    FFileOpDlg.iProgress1Pos := PercentDone;

    if (FLastFileSize > 0) and (PercentDone = 100) then
    begin
      FPercent := FPercent + ((FLastFileSize * 100) / FFilesSize);
      DebugLN('Percent2 = ' + IntToStr(Round(FPercent)));

      FFileOpDlg.iProgress2Pos := Round(FPercent);
    end;

    FFileOpDlg.sFileName := SysToUTF8(SourceName + ' -> ' + TargetName);

    if Assigned(CT) then
      CT.Synchronize(FFileOpDlg.UpdateDlg)
    else
      begin
        FFileOpDlg.UpdateDlg;
        Application.ProcessMessages;
      end;
  end; //with
end;

procedure MainLogProc (PluginNr, MsgType : Integer; LogString : PChar);stdcall;
var
  sMsg:String;
Begin
  Case MsgType of
    msgtype_connect: sMsg :='msgtype_connect';
    msgtype_disconnect: sMsg :='msgtype_disconnect';
    msgtype_details: sMsg :='msgtype_details';
    msgtype_transfercomplete: sMsg :='msgtype_transfercomplete';
    msgtype_connectcomplete: sMsg :='msgtype_connectcomplete';
    msgtype_importanterror: sMsg :='msgtype_importanterror';
    msgtype_operationcomplete: sMsg :='msgtype_operationcomplete';
  end;
  // write log info
  if (log_vfs_op in gLogOptions) and (log_info in gLogOptions) then
    logWrite(rsMsgLogInfo + sMsg + ', ' + logString);
    
  //DebugLN('MainLogProc ('+ sMsg + ',' + logString + ')');
End;

function MainRequestProc (PluginNr,RequestType:integer;CustomTitle,CustomText,ReturnedText:pchar;maxlen:integer):longbool;stdcall;
var
  sReq:String;
  ct:string;
  sDir : String;
begin
  if CustomTitle='' then ct:='Double Commander' else ct:=CustomTitle;
  Result:=True;
Case RequestType of
 RT_Other:
  Begin
    sReq:='RT_Other';
    ReturnedText:=pchar(InputBox (CT,CustomText,ReturnedText));
    MaxLen:=Length (ReturnedText);
  End;
 RT_UserName:
  Begin
    sReq:='RT_UserName';
    ReturnedText:=pchar(InputBox (CT,'User name request',ReturnedText));
    MaxLen:=Length (ReturnedText);
  End;
 RT_Password:
  Begin
    sReq:='RT_Password';
    ReturnedText:=pchar(InputBox (CT,'Password request',ReturnedText));
    MaxLen:=Length (ReturnedText);
  End;
 RT_Account:
  Begin
    sReq:='RT_Account';
    ReturnedText:=pchar(InputBox (CT,'Account request',ReturnedText));
    MaxLen:=Length (ReturnedText);
  End;
 RT_UserNameFirewall:
  Begin
    sReq:='RT_UserNameFirewall';
    ReturnedText:=pchar(InputBox (CT,'Firewall username request',ReturnedText));
    MaxLen:=Length (ReturnedText);
  End;
 RT_PasswordFirewall:
  Begin
    sReq:='RT_PasswordFirewall';
    ReturnedText:=pchar(InputBox (CT,'Firewall password request',ReturnedText));
    MaxLen:=Length (ReturnedText);
  End;
 RT_TargetDir:
  Begin
    sReq:='RT_TargetDir';
    SelectDirectory('Directory selection request','', sDir, False);
    ReturnedText := PChar(sDir);
    MaxLen:=Length (ReturnedText);
  End;
 RT_URL:
  Begin
    sReq:='RT_URL';
    ReturnedText:=PChar(InputBox (CT,'URL request',ReturnedText));
    MaxLen:=Length (ReturnedText);
  End;
 RT_MsgOK:
  begin
    sReq:='RT_MsgOK';
    Result:=(MessageBoxFunction(CustomText, CustomTitle, MB_OK) = IDOK);
  end;
 RT_MsgYesNo:
  begin
    sReq:='RT_MsgYesNo';
    Result:=(MessageBoxFunction (CustomText, CustomTitle, MB_YESNO) = IDYES);
  end;
 RT_MsgOKCancel:
  begin
    sReq:='RT_MsgOKCancel';
    Result:=(MessageBoxFunction(CustomText, CustomTitle, MB_OKCANCEL) = IDOK);
  end;
end;
  DebugLn('MainRequestProc ('+IntToStr(PluginNr)+','+sReq+','+CustomTitle+','+CustomText+','+ReturnedText+')',BoolToStr(result,true));

End;
{/CallBack functions}

function TWFXModule.VFSInit(Data: PtrInt): Boolean;
var
  dps: pFsDefaultParamStruct;
begin
    if Assigned(FsSetDefaultParams) then
    begin
      GetMem(dps,SizeOf(tFsDefaultParamStruct));
      dps.DefaultIniName:=gini.FileName;
      dps.PluginInterfaceVersionHi:=1;
      dps.PluginInterfaceVersionLow:=50;
      dps.size:=SizeOf(tFsDefaultParamStruct);
      FsSetDefaultParams(dps);
      FreeMem(dps,SizeOf(tFsDefaultParamStruct));
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
    Result:= (FsExecuteFile(Parent, PathDelim, 'properties') = FS_EXEC_OK);
    WFXStatusInfo(PathDelim, FS_STATUS_END, FS_STATUS_OP_EXEC);
  except
    Result:= False;
  end;	
end;

function TWFXModule.VFSOpen(const sName: String; bCanYouHandleThisFile : Boolean = False): Boolean;
begin
  Debugln('WFXVFSOpen Entered');
  Result := (FsInit(WFXModuleList.Add(Self), @MainProgressProc, @MainLogProc, @MainRequestProc) = 0);
  Debugln('WFXVFSOpen Leaved');
end;

function TWFXModule.VFSRefresh: Boolean;
begin
  Result := True;
end;

function TWFXModule.VFSMkDir(const sDirName: String): Boolean;
begin
  WFXStatusInfo(FFolder, FS_STATUS_START, FS_STATUS_OP_MKDIR);
  if Assigned(FsMkDir) then
    Result:= FsMkDir(PChar(UTF8ToSys(sDirName)))
  else
    Result:= False;
  WFXStatusInfo(FFolder, FS_STATUS_END, FS_STATUS_OP_MKDIR);

  { Log messages }
  if Result then
    // write log success
    if (log_vfs_op in gLogOptions) and (log_success in gLogOptions) then
      logWrite(Format(rsMsgLogSuccess+rsMsgLogMkDir, [sDirName]), lmtSuccess)
  else
    // write log error
    if (log_vfs_op in gLogOptions) and (log_errors in gLogOptions) then
      logWrite(Format(rsMsgLogError+rsMsgLogMkDir, [sDirName]), lmtError);
  {/ Log messages }
end;

function TWFXModule.VFSRmDir(const sDirName: String): Boolean;
begin
  WFXStatusInfo(FFolder, FS_STATUS_START, FS_STATUS_OP_DELETE);
  if Assigned(FsRemoveDir) then
    Result := FsRemoveDir(PChar(UTF8ToSys(sDirName)))
  else
    Result:=false;
  WFXStatusInfo(FFolder, FS_STATUS_END, FS_STATUS_OP_DELETE);
end;

function TWFXModule.WFXCopyOut: Boolean;
var
  Count, I : Integer;
  ri : TRemoteInfo;
  iInt64Rec : TInt64Rec;
  RemoteName,
  LocalName : String;
  iResult : Integer;
begin
  FsFillAndCount(FFileList, FFilesSize);
  Count := FFileList.Count - 1;

  for I := 0 to Count do
    begin
      RemoteName := FFileList.CurrentDirectory + FFileList.GetFileName(I);
      LocalName := ExtractFilePath(FDstPath) +  FFileList.GetFileName(I);

      DebugLN('Remote name == ' + RemoteName);
      DebugLN('Local name == ' + LocalName);

      if FPS_ISDIR(FFileList.GetItem(I)^.iMode) then
        begin
          ForceDirectory(LocalName);
          Continue;
        end;

      with ri, FFileList.GetItem(I)^ do
        begin
          iInt64Rec.Value := iSize;
          SizeLow := iInt64Rec.Low;
          SizeHigh := iInt64Rec.High;
          LastWriteTime := DateTimeToFileTime(fTimeI);
          Attr := iMode;
        end;

      FLastFileSize := FFileList.GetItem(I)^.iSize;

      iResult := FsGetFile(PChar(UTF8ToSys(RemoteName)), PChar(UTF8ToSys(LocalName)), FFlags, @ri);

      if iResult = FS_FILE_USERABORT then
      begin
        FreeAndNil(FFileList);
        Exit; //Copying was aborted by the user (through ProgressProc)
      end;

      Result := (iResult = FS_FILE_OK);

      { Log messages }
      if Result then
        // write log success
        if (log_vfs_op in gLogOptions) and (log_success in gLogOptions) then
          logWrite(CT, Format(rsMsgLogSuccess+rsMsgLogCopy, [RemoteName+' -> '+LocalName]), lmtSuccess)
      else
        // write log error
        if (log_vfs_op in gLogOptions) and (log_errors in gLogOptions) then
          logWrite(CT, Format(rsMsgLogError+rsMsgLogCopy, [RemoteName+' -> '+LocalName]), lmtError);
      {/ Log messages }
    end;
  FreeAndNil(FFileList);
end;

function TWFXModule.WFXCopyIn: Boolean;
var
  Count, I : Integer;
  LocalName,
  RemoteName : String;
  iResult : Integer;
begin
  FillAndCount(FFileList, FFilesSize);
  Count := FFileList.Count - 1;
  for I := 0 to Count do
    begin
      LocalName := FFileList.CurrentDirectory + FFileList.GetFileName(I);
      RemoteName := ExtractFilePath(FDstPath) +  FFileList.GetFileName(I);

      DebugLN('Local name == ' + LocalName);
      DebugLN('Remote name == ' + RemoteName);

      if FPS_ISDIR(FFileList.GetItem(I)^.iMode) then
        begin
          FsMkDir(PChar(UTF8ToSys(RemoteName)));
          Continue;
        end;

      FLastFileSize := FFileList.GetItem(I)^.iSize;

      iResult := FsPutFile(PChar(UTF8ToSys(LocalName)), PChar(UTF8ToSys(RemoteName)), FFlags);

      if iResult = FS_FILE_USERABORT then
      begin
        FreeAndNil(FFileList);
        Exit; //Copying was aborted by the user (through ProgressProc)
      end;

      Result := (iResult = FS_FILE_OK);
      
      { Log messages }
      if Result then
        // write log success
        if (log_vfs_op in gLogOptions) and (log_success in gLogOptions) then
          logWrite(CT, Format(rsMsgLogSuccess+rsMsgLogCopy, [LocalName+' -> '+RemoteName]), lmtSuccess)
      else
        // write log error
        if (log_vfs_op in gLogOptions) and (log_errors in gLogOptions) then
          logWrite(CT, Format(rsMsgLogError+rsMsgLogCopy, [LocalName+' -> '+RemoteName]), lmtError);
      {/ Log messages }
    end;
  FreeAndNil(FFileList);
end;

function TWFXModule.VFSCopyOut(var flSrcList: TFileList; sDstPath: String;
  Flags: Integer): Boolean;
begin
  Result := True;
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max:=100;
    FFileOpDlg.iProgress2Max:=100;
    FFileOpDlg.Caption := rsDlgCp;

    FFileList := flSrcList;
    FDstPath := sDstPath;
    FFlags := Flags;

    CT := nil;
    WFXCopyOut;
    FFileOpDlg.Close;

  except
    Result := False;
  end;

  FFileOpDlg := nil;
end;

function TWFXModule.VFSCopyIn(var flSrcList: TFileList; sDstName: String;
  Flags: Integer): Boolean;
begin
  Result := True;
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max:=100;
    FFileOpDlg.iProgress2Max:=100;
    FFileOpDlg.Caption := rsDlgCp;

    FFileList := flSrcList;
    FDstPath := sDstName;
    FFlags := Flags;

    CT := nil;
    WFXCopyIn;
    FFileOpDlg.Close;

  except
    Result := False
  end;

  FFileOpDlg := nil;
end;

function TWFXModule.VFSCopyOutEx(var flSrcList: TFileList; sDstPath: String;
  Flags: Integer): Boolean;
begin
  //VFSCopyOut(flSrcList, sDstPath, Flags);
  Result := True;
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max:=100;
    FFileOpDlg.iProgress2Max:=100;
    FFileOpDlg.Caption := rsDlgCp;

    FFileList := flSrcList;
    FDstPath := sDstPath;
    FFlags := Flags;

    CT := TWFXCopyThread.Create(True);
    CT.FreeOnTerminate := True;
    CT.Operation := OP_COPYOUT;
    CT.WFXModule := Self;
    FFileOpDlg.Thread := TThread(CT);
    CT.Resume;
  except
    Result := False;
  end;
end;

function TWFXModule.VFSCopyInEx(var flSrcList: TFileList; sDstName: String;
  Flags: Integer): Boolean;
begin
  Result := True;
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max:=100;
    FFileOpDlg.iProgress2Max:=100;
    FFileOpDlg.Caption := rsDlgCp;

    FFileList := flSrcList;
    FDstPath := sDstName;
    FFlags := Flags;

    CT := TWFXCopyThread.Create(True);
    CT.FreeOnTerminate := True;
    CT.Operation := OP_COPYIN;
    CT.WFXModule := Self;
    FFileOpDlg.Thread := TThread(CT);
    CT.Resume;
  except
    Result := False
  end;
end;

function TWFXModule.VFSRename(const sSrcName, sDstName: String): Boolean;
begin

end;

function TWFXModule.VFSRun(const sName: String): Boolean;
begin
  WFXStatusInfo(FFolder, FS_STATUS_START, FS_STATUS_OP_EXEC);
  if Assigned(FsExecuteFile) then
    FsExecuteFile(0, PChar(UTF8ToSys(sName)), 'open');
  WFXStatusInfo(FFolder, FS_STATUS_END, FS_STATUS_OP_EXEC);
end;

function TWFXModule.VFSDelete(var flNameList: TFileList): Boolean;
var
  Count, I : Integer;
  sLogMsg : String;
begin
  WFXStatusInfo(FFolder, FS_STATUS_START, FS_STATUS_OP_DELETE);
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max := 100;
    FFileOpDlg.iProgress2Max := 100;
    FFileOpDlg.Caption := rsDlgDel;

    CT := nil;

    Count := flNameList.Count - 1;
    for I := 0 to Count do
      begin
        DebugLN('Delete name == ' + flNameList.GetFileName(I));
      
        if FPS_ISDIR(flNameList.GetItem(I)^.iMode) then
          begin
            sLogMsg := rsMsgLogRmDir;
            if Assigned(FsRemoveDir) then
              Result := FsRemoveDir(PChar(UTF8ToSys(flNameList.GetFileName(I))))
            else
              Result := false;
          end
        else
          begin
            sLogMsg := rsMsgLogDelete;
            if Assigned(FsDeleteFile) then
              Result := FsDeleteFile(PChar(UTF8ToSys(flNameList.GetFileName(I))))
            else
              Result:=false;
          end;
          
        { Log messages }
        if Result then
          // write log success
          if (log_vfs_op in gLogOptions) and (log_success in gLogOptions) then
            logWrite(Format(rsMsgLogSuccess+sLogMsg, [flNameList.GetFileName(I)]), lmtSuccess)
        else
          // write log error
          if (log_vfs_op in gLogOptions) and (log_errors in gLogOptions) then
            logWrite(Format(rsMsgLogError+sLogMsg, [flNameList.GetFileName(I)]), lmtError);
        {/ Log messages }
      end;
    FFileOpDlg.Close;
  except
    Result := False;
  end;

  FreeAndNil(flNameList);
  FFileOpDlg := nil;

  WFXStatusInfo(FFolder, FS_STATUS_END, FS_STATUS_OP_DELETE);
end;

function TWFXModule.VFSList(const sDir: String; var fl: TFileList): Boolean;
var
  FindData : TWIN32FINDDATA;
  Handle:THandle;
  fr : TFileRecItem;
  CurrFileName : String;  // Current file name
begin
  WFXStatusInfo(sDir, FS_STATUS_START, FS_STATUS_OP_LIST);
  fl.Clear;
  FFolder:= sDir; // save current folder
  AddUpLevel(LowDirLevel(sDir), fl);
  
  fl.CurrentDirectory := sDir;
  Handle := FsFindFirst(PChar(UTF8ToSys(sDir)), FindData);
  repeat
//  Debugln('Repeat in vfsList entered');

  with fr do
    begin
      CurrFileName := SysToUTF8(FindData.cFileName);
      if (CurrFileName = '.') or  (CurrFileName = '..') then Continue;
//      Debugln('ListItem filename= '+CurrFileName);
      sName := CurrFileName;
      //DebugLN('CurrFileName ==' + CurrFileName);
      iMode := FindData.dwFileAttributes;
      sModeStr := AttrToStr(iMode);
      bLinkIsDir := False;
      bSelected := False;
      if FPS_ISDIR(iMode) then
        sExt:=''
      else
        sExt:=ExtractFileExt(sName);
      sNameNoExt:=Copy(sName,1,length(sName)-length(sExt));
      sPath := sDir;

      iSize := (Int64(FindData.nFileSizeHigh) * MAXDWORD)+FindData.nFileSizeLow;
      fTimeI := FileTimeToDateTime(FindData.ftLastWriteTime);
      sTime := FormatDateTime(gDateTimeFormat, fTimeI);
    end;
  fl.AddItem(@fr);
//  if FsFindNext(Handle, FindData) then DebugLn('FsFindNex=true') else DebugLn('FsFindNex=false');
  until (not FsFindNext(Handle, FindData));
  
  FsFindClose(Handle);
  WFXStatusInfo(sDir, FS_STATUS_END, FS_STATUS_OP_LIST);

  Result := True;
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

{ TWFXCopyThread }

procedure TWFXCopyThread.Execute;
begin
// main archive thread code started here
  try
    with WFXModule do
      begin
      case Operation of
        OP_COPYOUT:
          begin
            WFXCopyOut;
          end;
        OP_COPYIN:
          begin
            WFXCopyIn;
          end;
      end; //case
    end; //with
  except
    DebugLN('Error in "WFXCopyThread.Execute"');
  end;

  Synchronize(WFXModule.FFileOpDlg.Close);
  WFXModule.FFileOpDlg := nil;
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

initialization
    WFXModuleList:= TList.Create
finalization
  if Assigned(WFXModuleList) then
    FreeAndNil(WFXModuleList);
end.
