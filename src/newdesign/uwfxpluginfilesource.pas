unit uWfxPluginFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uWFXModule, WfxPlugin,
  uFile, uFileSourceProperty, uFileSourceOperationTypes,
  uFileProperty, uFileSource, uFileSourceOperation;

type

  { IWfxPluginFileSource }

  IWfxPluginFileSource = interface(IFileSource)
    ['{F1F728C6-F718-4B17-8DE2-BE0134134ED8}']

    procedure FillAndCount(Files: TFiles; out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64);
    function WfxCopyMove(sSourceFile, sTargetFile: UTF8String; Flags: LongInt;
                         RemoteInfo: PRemoteInfo; Internal, CopyMoveIn: Boolean): LongInt;

    function GetPluginNumber: LongInt;
    function GetWfxModule: TWfxModule;

    property PluginNumber: LongInt read GetPluginNumber;
    property WfxModule: TWfxModule read GetWfxModule;
  end;

  { TWfxPluginFileSource }

  TWfxPluginFileSource = class(TFileSource, IWfxPluginFileSource)
  private
    FModuleFileName,
    FPluginRootName: UTF8String;
    FWFXModule: TWFXModule;
    FPluginNumber: LongInt;

    function GetPluginNumber: LongInt;
    function GetWfxModule: TWfxModule;
  protected
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetCurrentAddress: String; override;
  public
    procedure FillAndCount(Files: TFiles; out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64);
    function WfxCopyMove(sSourceFile, sTargetFile: UTF8String; Flags: LongInt;
                         RemoteInfo: PRemoteInfo; Internal, CopyMoveIn: Boolean): LongInt;
  public
    constructor Create(aWfxModule: TWfxModule; aModuleFileName, aPluginRootName: UTF8String); reintroduce;
    destructor Destroy; override;

    // Retrieve operations permitted on the source.  = capabilities?
    function GetOperationsTypes: TFileSourceOperationTypes; override;

    // Returns a list of property types supported by this source for each file.
    function GetFilePropertiesDescriptions: TFilePropertiesDescriptions; override;

    // Retrieve some properties of the file source.
    function GetProperties: TFileSourceProperties; override;

    // These functions create an operation object specific to the file source.
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCopyInOperation(SourceFileSource: IFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: IFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; override;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(BasePath, ExecutablePath, Verb: String): TFileSourceOperation; override;

    class function CreateByRootName(aRootName: String): IWfxPluginFileSource;

    property PluginNumber: LongInt read FPluginNumber;
    property WfxModule: TWfxModule read FWfxModule;

  end;

var
  // Used in callback functions
  WfxOperationList: TList = nil;

implementation

uses
  LCLProc, FileUtil,{} Forms, Dialogs, LCLType,{} uGlobs, uDCUtils, uLog, uLng, uCryptProc,
  uWfxPluginCopyInOperation, uWfxPluginCopyOutOperation, uWfxPluginExecuteOperation,
  uWfxPluginListOperation, uWfxPluginCreateDirectoryOperation, uWfxPluginDeleteOperation,
  uWfxPluginSetAttributeOperation, uWfxPluginFile, uWfxPluginUtil;

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
        Result:= InputQuery(sCustomTitle, 'User name:', sReturnedText);
      end;
    RT_Password:
      begin
        sReq:= 'RT_Password';
        Result:= InputQuery(sCustomTitle, 'Password:', True, sReturnedText);
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
        Result:= InputQuery(sCustomTitle, 'Target path:', sReturnedText);
      end;
    RT_URL:
      begin
        sReq:= 'RT_URL';
        Result:= InputQuery(sCustomTitle, 'URL:', sReturnedText);
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

function CryptProc(PluginNr, CryptoNumber: Integer; Mode: Integer; ConnectionName, Password: PChar; MaxLen: Integer): Integer; stdcall;
const
  cPrefix = 'wfx';
var
  sPassword: AnsiString;
begin
  try
    case Mode of
    FS_CRYPT_SAVE_PASSWORD:
      begin
        if PasswordStore.WritePassword(cPrefix, 'Test', ConnectionName, Password) then
          Result:= FS_FILE_OK
        else
          Result:= FS_FILE_WRITEERROR;
      end;
    FS_CRYPT_LOAD_PASSWORD,
    FS_CRYPT_LOAD_PASSWORD_NO_UI:
      begin
        Result:= FS_FILE_READERROR;
        if (Mode = FS_CRYPT_LOAD_PASSWORD_NO_UI) and (PasswordStore.HasMasterKey = False) then
          Exit(FS_FILE_NOTFOUND);
        if PasswordStore.ReadPassword(cPrefix, 'Test', ConnectionName, sPassword) then
          begin
            StrPLCopy(Password, sPassword, MaxLen);
            Result:= FS_FILE_OK;
          end;
      end;
    FS_CRYPT_COPY_PASSWORD,
    FS_CRYPT_MOVE_PASSWORD:
      begin
        Result:= FS_FILE_READERROR;
        if PasswordStore.ReadPassword(cPrefix, 'Test', ConnectionName, sPassword) then
          begin
            if not PasswordStore.WritePassword(cPrefix, 'Test', Password, sPassword) then
              Exit(FS_FILE_WRITEERROR);
            if Mode = FS_CRYPT_MOVE_PASSWORD then
              PasswordStore.DeletePassword(cPrefix, 'Test', ConnectionName);
            Result:= FS_FILE_OK;
          end;
      end;
    FS_CRYPT_DELETE_PASSWORD:
      begin
        PasswordStore.DeletePassword(cPrefix, 'Test', ConnectionName);
        Result:= FS_FILE_OK;
      end;
    end;
  except
    Result:= FS_FILE_NOTSUPPORTED;
  end;
end;

{ TWfxPluginFileSource }

constructor TWfxPluginFileSource.Create(aWfxModule: TWfxModule; aModuleFileName, aPluginRootName: UTF8String);
begin
  inherited Create;
  FModuleFileName:= aModuleFileName;
  FPluginRootName:= aPluginRootName;

  if Assigned(aWfxModule) then
    begin
      FWfxModule:= aWfxModule;
      Exit;
    end;

  FWfxModule:= TWfxModule.Create;
  if FWfxModule.LoadModule(FModuleFileName) then
    with FWfxModule do
    begin
      FPluginNumber:= WfxOperationList.Add(nil);
      FsInit(FPluginNumber, @MainProgressProc, @MainLogProc, @MainRequestProc);
      if Assigned(FsSetCryptCallback) then
        FsSetCryptCallback(@CryptProc, 0, 0);
      VFSInit(0);
    end;
end;

destructor TWfxPluginFileSource.Destroy;
begin
  inherited Destroy;
end;

function TWfxPluginFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList, fsoCopyIn, fsoCopyOut, fsoDelete, fsoCreateDirectory, fsoExecute];
end;

function TWfxPluginFileSource.GetFilePropertiesDescriptions: TFilePropertiesDescriptions;
begin
  Result := nil;
end;

function TWfxPluginFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [];
end;

function TWfxPluginFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := [];
end;

function TWfxPluginFileSource.GetCurrentAddress: String;
begin
  Result:= 'wfx://' + FPluginRootName;
end;

function TWfxPluginFileSource.GetPluginNumber: LongInt;
begin
  Result := FPluginNumber;
end;

function TWfxPluginFileSource.GetWfxModule: TWfxModule;
begin
  Result := FWFXModule;
end;

procedure TWfxPluginFileSource.FillAndCount(Files: TFiles; out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64);

  procedure FillAndCountRec(const srcPath: UTF8String);
  var
    FindData: TWfxFindData;
    Handle: THandle;
    aFile: TWfxPluginFile;
  begin
    with FWfxModule do
    begin
      Handle := WfxFindFirst(srcPath, FindData);
      if Handle = feInvalidHandle then Exit;

      repeat
        if (FindData.FileName = '.') or (FindData.FileName = '..') then Continue;
        aFile:= TWfxPluginFile.Create(FindData);
        aFile.Path:= srcPath;
        NewFiles.Add(aFile);

        if aFile.IsDirectory then
          begin
            FillAndCountRec(srcPath + FindData.FileName + PathDelim);
          end
        else
          begin
            Inc(FilesSize, aFile.Size);
            Inc(FilesCount);
          end;
      until not WfxFindNext(Handle, FindData);

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

function TWfxPluginFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TWfxPluginListOperation.Create(TargetFileSource, TargetPath);
end;

function TWfxPluginFileSource.CreateCopyInOperation(
           SourceFileSource: IFileSource;
           var SourceFiles: TFiles;
           TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TWfxPluginCopyInOperation.Create(SourceFileSource,
                                              TargetFileSource,
                                              SourceFiles, TargetPath);
end;

function TWfxPluginFileSource.CreateCopyOutOperation(
            TargetFileSource: IFileSource;
            var SourceFiles: TFiles;
            TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  Result := TWfxPluginCopyOutOperation.Create(SourceFileSource,
                                              TargetFileSource,
                                              SourceFiles, TargetPath);
end;

function TWfxPluginFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TWfxPluginDeleteOperation.Create(TargetFileSource, FilesToDelete);
end;

function TWfxPluginFileSource.CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TWfxPluginCreateDirectoryOperation.Create(TargetFileSource, BasePath, DirectoryPath);
end;

function TWfxPluginFileSource.CreateExecuteOperation(BasePath, ExecutablePath, Verb: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:=  TWfxPluginExecuteOperation.Create(TargetFileSource, BasePath, ExecutablePath, Verb);
end;

class function TWfxPluginFileSource.CreateByRootName(aRootName: String): IWfxPluginFileSource;
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
      Result:= TWfxPluginFileSource.Create(nil, sModuleFileName, aRootName);

      DebugLn('Found registered plugin ' + sModuleFileName + ' for file system ' + aRootName);
    end;
end;

initialization
  WfxOperationList:= TList.Create;
finalization
  if Assigned(WfxOperationList) then
    FreeAndNil(WfxOperationList);

end.
