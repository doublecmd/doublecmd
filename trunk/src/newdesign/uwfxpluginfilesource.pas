unit uWfxPluginFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uWFXModule, ufsplugin,
  uFile, uFileSourceProperty, uFileSourceOperationTypes,
  uFileProperty, uFileSource, uFileSourceOperation;

type

  { TWfxPluginFileSource }

  TWfxPluginFileSource = class(TFileSource)
  private
    FModuleFileName,
    FPluginRootName: UTF8String;
    FWFXModule: TWFXModule;
    FPluginNumber: LongInt;
  protected
    class function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetCurrentAddress: String; override;
  public
    procedure FillAndCount(Files: TFiles; out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64);
    procedure WfxStatusInfo(RemoteDir: UTF8String; InfoStartEnd, InfoOperation: Integer);
    function WfxMkDir(const sDirName: UTF8String): LongInt;
    function WfxRemoveDir(const sDirName: UTF8String): Boolean;
    function WfxDeleteFile(const sFileName: UTF8String): Boolean;
    function WfxCopyMove(sSourceFile, sTargetFile: UTF8String; Flags: LongInt;
                         RemoteInfo: PRemoteInfo; Internal, CopyMoveIn: Boolean): LongInt;
    function WfxExecuteFile(const sFileName, sVerb: UTF8String; out sNewPath: UTF8String): LongInt;
  public
    constructor Create(aModuleFileName, aPluginRootName: UTF8String); reintroduce;
    destructor Destroy; override;

    function Clone: TWfxPluginFileSource; override;
    procedure CloneTo(FileSource: TFileSource); override;

    // Retrieve operations permitted on the source.  = capabilities?
    class function GetOperationsTypes: TFileSourceOperationTypes; override;

    // Returns a list of property types supported by this source for each file.
    class function GetFilePropertiesDescriptions: TFilePropertiesDescriptions; override;

    // Retrieve some properties of the file source.
    class function GetProperties: TFileSourceProperties; override;

    // These functions create an operation object specific to the file source.
    // Each parameter will be owned by the operation (will be freed).
    function CreateListOperation: TFileSourceOperation; override;
    function CreateCopyInOperation(var SourceFileSource: TFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(var TargetFileSource: TFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; override;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
    function CreateCreateDirectoryOperation(DirectoryPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(ExecutablePath, Verb: String): TFileSourceOperation; override;

    class function CreateByRootName(aRootName: String): TWfxPluginFileSource;

    property PluginNumber: LongInt read FPluginNumber;
    property WfxModule: TWfxModule read FWfxModule;

  end;

var
  // Used in callback functions
  WfxOperationList: TList = nil;

implementation

uses
  LCLProc, FileUtil,{} Forms, Dialogs, LCLType,{} uGlobs, uDCUtils, uLog, uLng,
  uWfxPluginCopyInOperation, uWfxPluginCopyOutOperation, uWfxPluginExecuteOperation,
  uWfxPluginListOperation, uWfxPluginCreateDirectoryOperation, uWfxPluginDeleteOperation,
  uWfxPluginFile, uWfxPluginUtil;

{ CallBack functions }

function MainProgressProc(PluginNr: Integer; SourceName, TargetName: PChar; PercentDone: Integer): Integer; stdcall;
var
  UpdateProgressClass: TUpdateProgressClass;
begin
  Result:= 0;

  DebugLn('MainProgressProc ('+IntToStr(PluginNr)+','+SourceName+','+TargetName+','+IntToStr(PercentDone)+')=' ,IntTostr(Result));

  UpdateProgressClass:= TUpdateProgressClass(WfxOperationList.Items[PluginNr]);

  if not Assigned(UpdateProgressClass) then Exit;

  Result:= UpdateProgressClass.UpdateProgressFunction(SysToUTF8(SourceName), SysToUTF8(TargetName), PercentDone);
end;

procedure MainLogProc(PluginNr, MsgType: Integer; LogString: PChar); stdcall;
var
  sMsg: String;
  LogMsgType: TLogMsgType = lmtInfo;
  bLogFile: Boolean;
  bLock: Boolean = True;
Begin
  sMsg:= rsMsgLogInfo;
  bLogFile:= ((log_vfs_op in gLogOptions) and (log_info in gLogOptions));
  case MsgType of
    msgtype_connect:
      begin
        sMsg:= sMsg + 'msgtype_connect';
        ShowLogWindow(True, @bLock);
      end;
    msgtype_disconnect: sMsg:= sMsg + 'msgtype_disconnect';
    msgtype_details: sMsg:= sMsg + 'msgtype_details';
    msgtype_transfercomplete: sMsg:= sMsg + 'msgtype_transfercomplete';
    msgtype_connectcomplete: sMsg:= sMsg + 'msgtype_connectcomplete';
    msgtype_importanterror:
      begin
        sMsg:= rsMsgLogError + 'msgtype_importanterror';
        LogMsgType:= lmtError;
        bLogFile:= (log_vfs_op in gLogOptions) and (log_errors in gLogOptions);
      end;
    msgtype_operationcomplete: sMsg:= sMsg + 'msgtype_operationcomplete';
  end;
  // write log info
  logWrite(sMsg + ', ' + logString, LogMsgType, False, bLogFile);

  //DebugLN('MainLogProc ('+ sMsg + ',' + logString + ')');
end;

function MainRequestProc(PluginNr, RequestType: Integer; CustomTitle, CustomText, ReturnedText: PChar; MaxLen: Integer): Bool; stdcall;
var
  sReq,
  sCustomTitle,
  sReturnedText: String;
begin
  Result:= False;
  // Use operation UI for this?
  if CustomTitle = '' then
    sCustomTitle:= 'Double Commander'
  else
    sCustomTitle:= CustomTitle;
  sReturnedText:= StrPas(ReturnedText);

  case RequestType of
    RT_Other:
      begin
        sReq:= 'RT_Other';
        Result:= InputQuery(sCustomTitle, CustomText, sReturnedText);
      end;
    RT_UserName:
      begin
        sReq:= 'RT_UserName';
        Result:= InputQuery(sCustomTitle, 'User name request', sReturnedText);
      end;
    RT_Password:
      begin
        sReq:= 'RT_Password';
        Result:= InputQuery(sCustomTitle, 'Password request', True, sReturnedText);
      end;
    RT_Account:
      begin
        sReq:= 'RT_Account';
        Result:= InputQuery(sCustomTitle, 'Account request', sReturnedText);
      end;
    RT_UserNameFirewall:
      begin
        sReq:= 'RT_UserNameFirewall';
        Result:= InputQuery(sCustomTitle, 'Firewall username request', sReturnedText);
      end;
    RT_PasswordFirewall:
      begin
        sReq:= 'RT_PasswordFirewall';
        Result:= InputQuery(sCustomTitle, 'Firewall password request', True, sReturnedText);
      end;
    RT_TargetDir:
      begin
        sReq:= 'RT_TargetDir';
        Result:= SelectDirectory('Directory selection request', '', sReturnedText, False);
      end;
    RT_URL:
      begin
        sReq:= 'RT_URL';
        Result:= InputQuery(sCustomTitle, 'URL request', sReturnedText);
      end;
    RT_MsgOK:
      begin
        sReq:= 'RT_MsgOK';
        Result:= (MessageBoxFunction(CustomText, CustomTitle, MB_OK) = IDOK);
      end;
    RT_MsgYesNo:
      begin
        sReq:= 'RT_MsgYesNo';
        Result:= (MessageBoxFunction (CustomText, CustomTitle, MB_YESNO) = IDYES);
      end;
    RT_MsgOKCancel:
      begin
        sReq:= 'RT_MsgOKCancel';
        Result:= (MessageBoxFunction(CustomText, CustomTitle, MB_OKCANCEL) = IDOK);
      end;
  end;
  if Result then
    begin
      if ReturnedText <> nil then
        StrPCopy(ReturnedText, Copy(sReturnedText, 1, MaxLen));
    end;

  DebugLn('MainRequestProc ('+IntToStr(PluginNr)+','+sReq+','+CustomTitle+','+CustomText+','+ReturnedText+')', BoolToStr(Result, True));
end;

{ TWfxPluginFileSource }

constructor TWfxPluginFileSource.Create(aModuleFileName, aPluginRootName: UTF8String);
begin
  inherited Create;
  CurrentPath:= PathDelim;
  FModuleFileName:= aModuleFileName;
  FPluginRootName:= aPluginRootName;
  FWfxModule:= TWFXModule.Create;
  if FWfxModule.LoadModule(FModuleFileName) then
    begin
      FPluginNumber:= WfxOperationList.Add(nil);
      FWfxModule.FsInit(FPluginNumber, @MainProgressProc, @MainLogProc, @MainRequestProc);
      FWfxModule.VFSInit(0);
    end;
end;

destructor TWfxPluginFileSource.Destroy;
begin
  inherited Destroy;
end;

function TWfxPluginFileSource.Clone: TWfxPluginFileSource;
begin
  Result := TWfxPluginFileSource.Create(FModuleFileName, FPluginRootName);
  CloneTo(Result);
end;

procedure TWfxPluginFileSource.CloneTo(FileSource: TFileSource);
begin
  if Assigned(FileSource) then
  begin
    inherited CloneTo(FileSource);
  end;
end;

class function TWfxPluginFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList, fsoCopyIn, fsoCopyOut, fsoDelete, fsoCreateDirectory, fsoExecute];
end;

class function TWfxPluginFileSource.GetFilePropertiesDescriptions: TFilePropertiesDescriptions;
begin
  Result := nil;
end;

class function TWfxPluginFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [];
end;

class function TWfxPluginFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := [];
end;

function TWfxPluginFileSource.GetCurrentAddress: String;
begin
  Result:= 'wfx://' + FPluginRootName;
end;

procedure TWfxPluginFileSource.FillAndCount(Files: TFiles; out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64);

  procedure FillAndCountRec(const srcPath: UTF8String);
  var
    FindData: TWin32FindData;
    Handle: THandle;
    aFile: TWfxPluginFile;
  begin
    with FWfxModule do
    begin
      Handle := FsFindFirst(PChar(UTF8ToSys(srcPath)), FindData);
      if Handle = feInvalidHandle then Exit;

      repeat
        if (FindData.cFileName = '.') or (FindData.cFileName = '..') then Continue;
        aFile:= TWfxPluginFile.Create(FindData);
        aFile.Path:= srcPath;
        NewFiles.Add(aFile);

        if aFile.IsDirectory then
          begin
            FillAndCountRec(srcPath + SysToUTF8(FindData.cFileName) + PathDelim);
          end
        else
          begin
            Inc(FilesSize, aFile.Size);
            Inc(FilesCount);
          end;
      until not FsFindNext(Handle, FindData);

      FsFindClose(Handle);
    end;
  end;

var
  I: Integer;
  aFile: TWfxPluginFile;
begin
  NewFiles := TFiles.Create;
  FilesCount:= 0;
  FilesSize:= 0;
  for I := 0 to Files.Count - 1 do
  begin
    aFile := Files[I] as TWfxPluginFile;

    NewFiles.Add(aFile.Clone);

    if aFile.IsDirectory and (not aFile.IsLinkToDirectory) then
      begin
        FillAndCountRec(aFile.Path + aFile.Name + DirectorySeparator);  // recursive browse child dir
      end
    else
      begin
        Inc(FilesCount);
        Inc(FilesSize, aFile.Size); // in first level we know file size -> use it
      end;
  end;
end;

procedure TWfxPluginFileSource.WfxStatusInfo(RemoteDir: UTF8String; InfoStartEnd, InfoOperation: Integer);
begin
  with FWfxModule do
  begin
    if Assigned(FsStatusInfo) then
      FsStatusInfo(PChar(UTF8ToSys(RemoteDir)), InfoStartEnd, InfoOperation);
  end;
end;

function TWfxPluginFileSource.WfxMkDir(const sDirName: UTF8String): LongInt;
begin
  with FWfxModule do
  begin
    Result:= WFX_NOTSUPPORTED;
    if Assigned(FsMkDir) then
      begin
        WfxStatusInfo(CurrentPath, FS_STATUS_START, FS_STATUS_OP_MKDIR);
        if FsMkDir(PChar(UTF8ToSys(sDirName))) then
          Result:= WFX_SUCCESS
        else
          Result:= WFX_ERROR;
        WfxStatusInfo(CurrentPath, FS_STATUS_END, FS_STATUS_OP_MKDIR);
      end;
  end;
end;

function TWfxPluginFileSource.WfxRemoveDir(const sDirName: UTF8String): Boolean;
begin
  with FWfxModule do
  begin
    Result:= False;
    if Assigned(FsRemoveDir) then
      begin
        Result:= FsRemoveDir(PChar(UTF8ToSys(sDirName)));
      end;
  end;
end;

function TWfxPluginFileSource.WfxDeleteFile(const sFileName: UTF8String): Boolean;
begin
  with FWfxModule do
  begin
    Result:= False;
    if Assigned(FsDeleteFile) then
      begin
        Result:= FsDeleteFile(PChar(UTF8ToSys(sFileName)));
      end;
  end;
end;

function TWfxPluginFileSource.WfxCopyMove(sSourceFile, sTargetFile: UTF8String;
                                          Flags: LongInt; RemoteInfo: PRemoteInfo;
                                          Internal, CopyMoveIn: Boolean): LongInt;
var
  bMove,
  bOverWrite: Boolean;
  pcSourceName,
  pcTargetName: PChar;
begin
  with FWfxModule do
  begin
    pcSourceName:= PChar(UTF8ToSys(sSourceFile));
    pcTargetName:= PChar(UTF8ToSys(sTargetFile));
    if Internal then
      begin
        bMove:= ((Flags and FS_COPYFLAGS_MOVE) <> 0);
        bOverWrite:= ((Flags and FS_COPYFLAGS_OVERWRITE) <> 0);
        Result:= FsRenMovFile(pcSourceName, pcTargetName, bMove, bOverWrite, RemoteInfo);
      end
    else
      begin
        if CopyMoveIn then
          Result:= FsPutFile(pcSourceName, pcTargetName, Flags)
        else
          Result:= FsGetFile(pcSourceName, pcTargetName, Flags, RemoteInfo);
      end;
  end;
end;

function TWfxPluginFileSource.WfxExecuteFile(const sFileName, sVerb: UTF8String;
                                             out sNewPath: UTF8String): LongInt;
var
  pcRemoteName: PChar;
begin
  with FWfxModule do
  begin
    Result:= WFX_NOTSUPPORTED;
    if Assigned(FsExecuteFile) then
      begin
        pcRemoteName:= GetMem(MAX_PATH);
        StrPCopy(pcRemoteName, UTF8ToSys(sFileName));
        Result:= FsExecuteFile(0, pcRemoteName, PChar(UTF8ToSys(sVerb)));
        if Result = FS_EXEC_SYMLINK then
          sNewPath:= SysToUTF8(pcRemoteName);
        FreeMem(pcRemoteName);
      end;
  end;
end;

function TWfxPluginFileSource.CreateListOperation: TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TWfxPluginListOperation.Create(TargetFileSource);
end;

function TWfxPluginFileSource.CreateCopyInOperation(
           var SourceFileSource: TFileSource;
           var SourceFiles: TFiles;
           TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TWfxPluginCopyInOperation.Create(SourceFileSource,
                                              TargetFileSource,
                                              SourceFiles, TargetPath);
end;

function TWfxPluginFileSource.CreateCopyOutOperation(
            var TargetFileSource: TFileSource;
            var SourceFiles: TFiles;
            TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: TFileSource;
begin
  SourceFileSource := Self.Clone;
  Result := TWfxPluginCopyOutOperation.Create(SourceFileSource,
                                              TargetFileSource,
                                              SourceFiles, TargetPath);
end;

function TWfxPluginFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TWfxPluginDeleteOperation.Create(TargetFileSource, FilesToDelete);
end;

function TWfxPluginFileSource.CreateCreateDirectoryOperation(DirectoryPath: String): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TWfxPluginCreateDirectoryOperation.Create(TargetFileSource, DirectoryPath);
end;

function TWfxPluginFileSource.CreateExecuteOperation(ExecutablePath, Verb: String): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result:=  TWfxPluginExecuteOperation.Create(TargetFileSource, ExecutablePath, Verb);
end;

class function TWfxPluginFileSource.CreateByRootName(aRootName: String): TWfxPluginFileSource;
var
  sModuleFileName: UTF8String;
begin
  Result:= nil;

  if gWFXPlugins.Count = 0 then Exit;
  // Check if there is a registered plugin for the name of the file system plugin.
  sModuleFileName:= gWFXPlugins.Values[aRootName];
  if sModuleFileName <> EmptyStr then
    begin
      sModuleFileName:= GetCmdDirFromEnvVar(sModuleFileName);
      Result:= TWfxPluginFileSource.Create(sModuleFileName, aRootName);

      DebugLn('Registered plugin ' + sModuleFileName + ' for file system ' + aRootName);
    end;
end;

initialization
  WfxOperationList:= TList.Create;
finalization
  if Assigned(WfxOperationList) then
    FreeAndNil(WfxOperationList);

end.
