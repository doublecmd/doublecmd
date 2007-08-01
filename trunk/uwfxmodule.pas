{
   Double Commander
   -------------------------------------------------------------------------
   Virtual File System - class for manage WFX plugins (Version 1.3)
 
   Copyright (C) 2007  Koblov Alexander (Alexx2000@mail.ru)
 
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

interface
uses
  Classes, uFileList, uVFSModule, ufsplugin, uWFXprototypes, dynlibs, uTypes, fFileOpDlg;

{$mode delphi}{$H+}
const
  OP_COPYOUT = 0;
  OP_COPYIN = 1;

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
    FDstPath : String;
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
    procedure FsFillAndCount(var fl:TFileList; out FilesSize : Int64);
  public
    constructor Create;
    destructor Destroy; override;
    function LoadModule(const sName:String):Boolean;override; {Load plugin}
    procedure UnloadModule;override;
    function VFSInit:Boolean;override;
    procedure VFSDestroy;override;
    function VFSCaps : Integer;override;

    function VFSConfigure(Parent: THandle):Boolean;override;
    function VFSOpen(const sName:String; bCanYouHandleThisFile : Boolean = False):Boolean;override;
    //function VFSClose:Boolean;override;
    
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
  end;

implementation
uses
  SysUtils, LCLProc, LCLType, uVFSutil, uFileOp, uOSUtils, uFileProcs, uLng, Dialogs, Forms, Controls;

var
  WFXModule : TWFXModule;

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
  Handle := FsFindFirst(PChar(srcPath), FindData);
  if Handle < 0 then
  begin
    FsFindClose(Handle);
    Exit;
  end;
  repeat
    if (FindData.cFileName='.') or (FindData.cFileName='..') then Continue;
    fr.sName:=ExtractDirLevel(fl.CurrentDirectory, srcPath+FindData.cFileName);
    fr.sPath:=dstPath;
    fr.sNameNoExt:=FindData.cFileName; // we use to save dstname

    fr.iMode := FindData.dwFileAttributes;

    fr.bSelected:=False;
    fr.iSize := Int64(FindData.nFileSizeHigh) shl 32 + FindData.nFileSizeLow;;

    NewFileList.AddItem(@fr);

    if FPS_ISDIR(fr.iMode) then
      begin
        FillAndCountRec(srcPath+FindData.cFileName+DirectorySeparator, dstPath+FindData.cFileName+DirectorySeparator);
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

constructor TWFXModule.Create;
begin
  FFilesSize := 0;
  FPercent := 0;
  WFXModule := Self;
end;

destructor TWFXModule.Destroy;
begin
  UnloadModule;
end;

function TWFXModule.LoadModule(const sName: String): Boolean;
begin
  FModuleHandle := LoadLibrary(sName);
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
end;

{CallBack functions}
function MainProgressProc (PluginNr:integer;SourceName,TargetName:pchar;PercentDone:integer):integer;stdcall;
begin
  Result := 0;
  DebugLN ('MainProgressProc ('+IntToStr(PluginNr)+','+SourceName+','+TargetName+','+inttostr(PercentDone)+')' ,inttostr(result));

  with WFXModule do
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

    FFileOpDlg.sFileName := SourceName + ' -> ' + TargetName;

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
  DebugLN('MainLogProc ('+ sMsg + ',' + logString + ')');
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
    sReq:='RT_MsgOKCancel=';
    Result:=(MessageBoxFunction(CustomText, CustomTitle, MB_OKCANCEL) = IDOK);
  end;
end;
  DebugLn('MainRequestProc ('+IntToStr(PluginNr)+','+sReq+','+CustomTitle+','+CustomText+','+ReturnedText+')',BoolToStr(result,true));

End;
{/CallBack functions}

function TWFXModule.VFSInit: Boolean;
begin

end;

procedure TWFXModule.VFSDestroy;
begin

end;

function TWFXModule.VFSCaps: Integer;
var
  pPlgName : PChar;
begin
  New(pPlgName);
  FsGetDefRootName(pPlgName, 256);
  Result := Integer(pPlgName);
end;

function TWFXModule.VFSConfigure(Parent: THandle): Boolean;
begin
  FsStatusInfo('', 0, 0);
end;

function TWFXModule.VFSOpen(const sName: String; bCanYouHandleThisFile : Boolean = False): Boolean;
begin
  FsInit(Random(MaxInt), @MainProgressProc, @MainLogProc, @MainRequestProc);
end;

function TWFXModule.VFSMkDir(const sDirName: String): Boolean;
begin
  Result := FsMkDir(PChar(sDirName));
end;

function TWFXModule.VFSRmDir(const sDirName: String): Boolean;
begin
  Result := FsRemoveDir(PChar(sDirName));
end;

function TWFXModule.WFXCopyOut: Boolean;
var
  Count, I : Integer;
  ri : pRemoteInfo;
  iInt64Rec : TInt64Rec;
  RemoteName,
  LocalName : String;
  iResult : Integer;
begin
  FsFillAndCount(FFileList, FFilesSize);
  Count := FFileList.Count - 1;
  New(ri);
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

      with ri^, FFileList.GetItem(I)^ do
        begin
          iInt64Rec.Value := iSize;
          SizeLow := iInt64Rec.Low;
          SizeHigh := iInt64Rec.High;
          //LastWriteTime := fTimeI;
          Attr := iMode;
        end;

      FLastFileSize := FFileList.GetItem(I)^.iSize;

      iResult := FsGetFile(PChar(RemoteName), PChar(LocalName), FFlags, ri);

      if iResult = FS_FILE_USERABORT then Exit; //Copying was aborted by the user (through ProgressProc)

      Result := (iResult = FS_FILE_OK);
    end;
    Dispose(ri);
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
          FsMkDir(PChar(RemoteName));
          Continue;
        end;

      FLastFileSize := FFileList.GetItem(I)^.iSize;

      iResult := FsPutFile(PChar(LocalName), PChar(RemoteName), FFlags);

      if iResult = FS_FILE_USERABORT then Exit; //Copying was aborted by the user (through ProgressProc)

      Result := (iResult = FS_FILE_OK);
    end;
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
    FFileOpDlg.Caption := lngGetString(clngDlgCp);

    FFileList := flSrcList;
    FDstPath := sDstPath;
    FFlags := Flags;

    CT := nil;
    WFXCopyOut;
    FFileOpDlg.Close;
    FFileOpDlg.Free;

  except
    Result := False;
  end;
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
    FFileOpDlg.Caption := lngGetString(clngDlgCp);

    FFileList := flSrcList;
    FDstPath := sDstName;
    FFlags := Flags;

    CT := nil;
    WFXCopyIn;
    FFileOpDlg.Close;
    FFileOpDlg.Free;

  except
    Result := False
  end;
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
    FFileOpDlg.Caption := lngGetString(clngDlgCp);

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
    FFileOpDlg.Caption := lngGetString(clngDlgCp);

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
  FsExecuteFile(0, PChar(sName), 'open');
end;

function TWFXModule.VFSDelete(var flNameList: TFileList): Boolean;
var
  Count, I : Integer;
begin
  try
    FFileOpDlg:= TfrmFileOp.Create(nil);
    FFileOpDlg.Show;
    FFileOpDlg.iProgress1Max := 100;
    FFileOpDlg.iProgress2Max := 100;
    FFileOpDlg.Caption := lngGetString(clngDlgDel);

    CT := nil;

    Count := flNameList.Count - 1;
    for I := 0 to Count do
      begin
        DebugLN('Delete name == ' + flNameList.GetFileName(I));
      
        if FPS_ISDIR(flNameList.GetItem(I)^.iMode) then
          Result := FsRemoveDir(PChar(flNameList.GetFileName(I)))
       else
          Result := FsDeleteFile(PChar(flNameList.GetFileName(I)));
      end;
    FFileOpDlg.Close;
    FFileOpDlg.Free;
  except
    Result := False;
  end;
end;

function TWFXModule.VFSList(const sDir: String; var fl: TFileList): Boolean;
var
  FindData : TWIN32FINDDATA;
  Handle:THandle;
  fr : PFileRecItem;
  CurrFileName : String;  // Current file name
begin
  fl.Clear;
  AddUpLevel(LowDirLevel(sDir), fl);
  
  fl.CurrentDirectory := sDir;
  Handle := FsFindFirst(PChar(sDir), FindData);
  repeat
  New(fr);
  with fr^ do
    begin
      CurrFileName := FindData.cFileName;
      if (CurrFileName = '.') or  (CurrFileName = '..') then Continue;
      
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

      iSize := (FindData.nFileSizeHigh * MAXDWORD)+FindData.nFileSizeLow;
      fTimeI := FileTimeToDateTime(FindData.ftLastWriteTime);
      sTime := DateToStr(fTimeI);
    end;
  fl.AddItem(fr);
  until not FsFindNext(Handle, FindData);
  FsFindClose(Handle);
  
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
        Synchronize(FFileOpDlg.Close);
      end; //with
  except
    DebugLN('Error in "WFXCopyThread.Execute"');
  end;
end;

end.
