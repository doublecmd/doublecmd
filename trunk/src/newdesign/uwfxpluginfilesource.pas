unit uWfxPluginFileSource;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  Classes, SysUtils, uWFXModule, WfxPlugin,
  uFile, uFileSourceProperty, uFileSourceOperationTypes,
  uFileProperty, uFileSource, uFileSourceOperation;

type

  TUpdateProgress = function(SourceName, TargetName: UTF8String; PercentDone: Integer): Integer of object;

  { IWfxPluginFileSource }

  IWfxPluginFileSource = interface(IFileSource)
    ['{F1F728C6-F718-4B17-8DE2-BE0134134ED8}']

    procedure FillAndCount(Files: TFiles; CountDirs: Boolean; ExcludeRootDir: Boolean;
                           out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64);
    function WfxCopyMove(sSourceFile, sTargetFile: UTF8String; Flags: LongInt;
                         RemoteInfo: PRemoteInfo; Internal, CopyMoveIn: Boolean): LongInt;

    function GetPluginNumber: LongInt;
    function GetWfxModule: TWfxModule;

    property PluginNumber: LongInt read GetPluginNumber;
    property WfxModule: TWfxModule read GetWfxModule;
  end;

  { TWfxPluginFileSource }

  TWfxPluginFileSource = class;

  { TCallbackDataClass }

  TCallbackDataClass = class
  public
    // Must use class here instead of interface because of circular reference
    // between TWfxPluginFileSource and TCallbackDataClass, which would cause
    // the file source never to be destroyed.
    // TWfxPluginFileSource controls the lifetime of TCallbackDataClass though,
    // so it should be fine.
    FileSource: TWfxPluginFileSource;
    UpdateProgressFunction: TUpdateProgress;
    constructor Create(aFileSource: TWfxPluginFileSource);
  end;

  { TWfxPluginFileSource }

  TWfxPluginFileSource = class(TFileSource, IWfxPluginFileSource)
  private
    FModuleFileName,
    FPluginRootName: UTF8String;
    FWFXModule: TWFXModule;
    FPluginNumber: LongInt;
    FCallbackDataClass: TCallbackDataClass;

    function GetPluginNumber: LongInt;
    function GetWfxModule: TWfxModule;

    function CreateConnection: TFileSourceConnection;
    procedure CreateConnections;

    procedure AddToConnectionQueue(Operation: TFileSourceOperation);
    procedure RemoveFromConnectionQueue(Operation: TFileSourceOperation);
    procedure AddConnection(Connection: TFileSourceConnection);
    procedure RemoveConnection(Connection: TFileSourceConnection);

    {en
       Searches connections list for a connection assigned to operation.
    }
    function FindConnectionByOperation(operation: TFileSourceOperation): TFileSourceConnection; virtual;

    procedure NotifyNextWaitingOperation(allowedOps: TFileSourceOperationTypes);

    procedure ClearCurrentOperation(Operation: TFileSourceOperation);
  protected
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetCurrentAddress: String; override;
    procedure OperationFinished(Operation: TFileSourceOperation); override;
  public
    procedure FillAndCount(Files: TFiles; CountDirs: Boolean; ExcludeRootDir: Boolean;
                           out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64);
    function WfxCopyMove(sSourceFile, sTargetFile: UTF8String; Flags: LongInt;
                         RemoteInfo: PRemoteInfo; Internal, CopyMoveIn: Boolean): LongInt;
  public
    constructor Create(aModuleFileName, aPluginRootName: UTF8String); reintroduce;
    destructor Destroy; override;

    class function CreateFile(const APath: String): TFile; override;
    class function CreateFile(const APath: String; FindData: TWfxFindData): TFile; overload;

    // Retrieve operations permitted on the source.  = capabilities?
    function GetOperationsTypes: TFileSourceOperationTypes; override;

    // Retrieve some properties of the file source.
    function GetProperties: TFileSourceProperties; override;

    // These functions create an operation object specific to the file source.
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation; override;
    function CreateCopyInOperation(SourceFileSource: IFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: IFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; override;
    function CreateMoveOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation; override;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; override;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation; override;

    function GetLocalName(var aFile: TFile): Boolean; override;

    class function CreateByRootName(aRootName: String): IWfxPluginFileSource;

    function GetConnection(Operation: TFileSourceOperation): TFileSourceConnection; override;
    procedure RemoveOperationFromQueue(Operation: TFileSourceOperation); override;

    property PluginNumber: LongInt read FPluginNumber;
    property WfxModule: TWfxModule read FWfxModule;

  end;

  { TWfxPluginFileSourceConnection }

  TWfxPluginFileSourceConnection = class(TFileSourceConnection)
  private
    FWfxModule: TWfxModule;

  public
    constructor Create(aWfxModule: TWfxModule); reintroduce;

    property WfxModule: TWfxModule read FWfxModule;
  end;

var
  // Used in callback functions
  WfxOperationList: TStringList = nil;

implementation

uses
  LCLProc, FileUtil, StrUtils, {} LCLType, uShowMsg, {} uGlobs, uDCUtils, uLog,
  uDebug, uLng, uCryptProc, uFileAttributes, uConnectionManager, contnrs, syncobjs,
  uWfxPluginCopyInOperation, uWfxPluginCopyOutOperation,  uWfxPluginMoveOperation,
  uWfxPluginExecuteOperation, uWfxPluginListOperation, uWfxPluginCreateDirectoryOperation,
  uWfxPluginDeleteOperation, uWfxPluginSetFilePropertyOperation, uWfxPluginCopyOperation;

const
  connCopyIn      = 0;
  connCopyOut     = 1;
  connDelete      = 2;

var
  // Always use appropriate lock to access these lists.
  WfxConnections: TObjectList; // store connections created by Wcx file sources
  WfxOperationsQueue: TObjectList; // store queued operations, use only under FOperationsQueueLock
  WfxConnectionsLock: TCriticalSection; // used to synchronize access to connections
  WfxOperationsQueueLock: TCriticalSection; // used to synchronize access to operations queue

{ CallBack functions }

function MainProgressProc(PluginNr: Integer; SourceName, TargetName: UTF8String; PercentDone: Integer): Integer;
var
  CallbackDataClass: TCallbackDataClass;
begin
  Result:= 0;

  DCDebug('MainProgressProc ('+IntToStr(PluginNr)+','+SourceName+','+TargetName+','+IntToStr(PercentDone)+')=' ,IntTostr(Result));

  CallbackDataClass:= TCallbackDataClass(WfxOperationList.Objects[PluginNr]);

  if not (Assigned(CallbackDataClass) and Assigned(CallbackDataClass.UpdateProgressFunction)) then Exit;

  Result:= CallbackDataClass.UpdateProgressFunction(SourceName, TargetName, PercentDone);
end;

function MainProgressProcA(PluginNr: Integer; SourceName, TargetName: PAnsiChar; PercentDone: Integer): Integer; dcpcall;
var
  sSourceName,
  sTargetName: UTF8String;
begin
  sSourceName:= SysToUTF8(StrPas(SourceName));
  sTargetName:= SysToUTF8(StrPas(TargetName));
  Result:= MainProgressProc(PluginNr, sSourceName, sTargetName, PercentDone);
end;

function MainProgressProcW(PluginNr: Integer; SourceName, TargetName: PWideChar; PercentDone: Integer): Integer; dcpcall;
var
  sSourceName,
  sTargetName: UTF8String;
begin
  sSourceName:= UTF8Encode(WideString(SourceName));
  sTargetName:= UTF8Encode(WideString(TargetName));
  Result:= MainProgressProc(PluginNr, sSourceName, sTargetName, PercentDone);
end;

procedure MainLogProc(PluginNr, MsgType: Integer; LogString: UTF8String);
var
  I: Integer;
  sMsg: UTF8String;
  LogMsgType: TLogMsgType = lmtInfo;
  bLogFile: Boolean;
  sName: UTF8String;
  CallbackDataClass: TCallbackDataClass;
Begin
  sMsg:= rsMsgLogInfo;
  bLogFile:= ((log_vfs_op in gLogOptions) and (log_info in gLogOptions));
  CallbackDataClass:= TCallbackDataClass(WfxOperationList.Objects[PluginNr]);
  case MsgType of
    msgtype_connect:
      begin
        if Assigned(CallbackDataClass) then
          begin
            I:= Pos(#32, LogString);
            sName:= WfxOperationList[PluginNr] + ':' + Copy(LogString, I, MaxInt);
            AddNetworkConnection(sName, CallbackDataClass.FileSource);
          end;
        sMsg:= sMsg + '[' + IntToStr(MsgType) + ']';
        ShowLogWindow(True);
      end;
    msgtype_disconnect:
      begin
        if Assigned(CallbackDataClass) then
          begin
            I:= Pos(#32, LogString);
            sName:= WfxOperationList[PluginNr] + Copy(LogString, I, MaxInt);
            RemoveNetworkConnection(sName);
          end;
      end;
    msgtype_details,
    msgtype_operationcomplete,
    msgtype_transfercomplete,
    msgtype_connectcomplete:
      sMsg:= sMsg + '[' + IntToStr(MsgType) + ']';
    msgtype_importanterror:
      begin
        sMsg:= rsMsgLogError + '[' + IntToStr(MsgType) + ']';
        LogMsgType:= lmtError;
        bLogFile:= (log_vfs_op in gLogOptions) and (log_errors in gLogOptions);
      end;
  end;
  // write log info
  logWrite(sMsg + ', ' + logString, LogMsgType, True, bLogFile);

  //DCDebug('MainLogProc ('+ sMsg + ',' + logString + ')');
end;

procedure MainLogProcA(PluginNr, MsgType: Integer; LogString: PAnsiChar); dcpcall;
begin
  MainLogProc(PluginNr, MsgType, SysToUTF8(StrPas(LogString)));
end;

procedure MainLogProcW(PluginNr, MsgType: Integer; LogString: PWideChar); dcpcall;
begin
  MainLogProc(PluginNr, MsgType, UTF8Encode(WideString(LogString)));
end;

function MainRequestProc(PluginNr, RequestType: Integer; CustomTitle, CustomText: UTF8String; var ReturnedText: UTF8String): Bool;
var
  sReq: UTF8String;
begin
  Result:= False;
  // Use operation UI for this?
  if CustomTitle = '' then
    CustomTitle:= 'Double Commander';

  case RequestType of
    RT_Other:
      begin
        sReq:= 'RT_Other';
        Result:= ShowInputQuery(CustomTitle, CustomText, ReturnedText);
      end;
    RT_UserName:
      begin
        sReq:= 'RT_UserName';
        Result:= ShowInputQuery(CustomTitle, IfThen(CustomText = EmptyStr, rsMsgUserName, CustomText), ReturnedText);
      end;
    RT_Password:
      begin
        sReq:= 'RT_Password';
        Result:= ShowInputQuery(CustomTitle, IfThen(CustomText = EmptyStr, rsMsgPassword, CustomText), True, ReturnedText);
      end;
    RT_Account:
      begin
        sReq:= 'RT_Account';
        Result:= ShowInputQuery(CustomTitle, IfThen(CustomText = EmptyStr, rsMsgAccount, CustomText), ReturnedText);
      end;
    RT_UserNameFirewall:
      begin
        sReq:= 'RT_UserNameFirewall';
        Result:= ShowInputQuery(CustomTitle, IfThen(CustomText = EmptyStr, rsMsgUserNameFirewall, CustomText), ReturnedText);
      end;
    RT_PasswordFirewall:
      begin
        sReq:= 'RT_PasswordFirewall';
        Result:= ShowInputQuery(CustomTitle, IfThen(CustomText = EmptyStr, rsMsgPasswordFirewall, CustomText), True, ReturnedText);
      end;
    RT_TargetDir:
      begin
        sReq:= 'RT_TargetDir';
        Result:= ShowInputQuery(CustomTitle, IfThen(CustomText = EmptyStr, rsMsgTargetDir, CustomText), ReturnedText);
      end;
    RT_URL:
      begin
        sReq:= 'RT_URL';
        Result:= ShowInputQuery(CustomTitle, IfThen(CustomText = EmptyStr, rsMsgURL, CustomText), ReturnedText);
      end;
    RT_MsgOK:
      begin
        sReq:= 'RT_MsgOK';
        Result:= (ShowMessageBox(CustomText, CustomTitle, MB_OK) = IDOK);
      end;
    RT_MsgYesNo:
      begin
        sReq:= 'RT_MsgYesNo';
        Result:= (ShowMessageBox(CustomText, CustomTitle, MB_YESNO) = IDYES);
      end;
    RT_MsgOKCancel:
      begin
        sReq:= 'RT_MsgOKCancel';
        Result:= (ShowMessageBox(CustomText, CustomTitle, MB_OKCANCEL) = IDOK);
      end;
  end;

  DCDebug('MainRequestProc ('+IntToStr(PluginNr)+','+sReq+','+CustomTitle+','+CustomText+','+ReturnedText+')', BoolToStr(Result, True));
end;

function MainRequestProcA(PluginNr, RequestType: Integer; CustomTitle, CustomText, ReturnedText: PAnsiChar; MaxLen: Integer): Bool; dcpcall;
var
  sCustomTitle,
  sCustomText,
  sReturnedText: UTF8String;
begin
  sCustomTitle:= SysToUTF8(StrPas(CustomTitle));
  sCustomText:=  SysToUTF8(StrPas(CustomText));
  sReturnedText:= SysToUTF8(StrPas(ReturnedText));
  Result:= MainRequestProc(PluginNr, RequestType, sCustomTitle, sCustomText, sReturnedText);
  if Result then
    begin
      if ReturnedText <> nil then
        StrPLCopy(ReturnedText, UTF8ToSys(sReturnedText), MaxLen);
    end;
end;

function MainRequestProcW(PluginNr, RequestType: Integer; CustomTitle, CustomText, ReturnedText: PWideChar; MaxLen: Integer): Bool; dcpcall;
var
  sCustomTitle,
  sCustomText,
  sReturnedText: UTF8String;
begin
  sCustomTitle:= UTF8Encode(WideString(CustomTitle));
  sCustomText:=  UTF8Encode(WideString(CustomText));
  sReturnedText:= UTF8Encode(WideString(ReturnedText));
  Result:= MainRequestProc(PluginNr, RequestType, sCustomTitle, sCustomText, sReturnedText);
  if Result then
    begin
      if ReturnedText <> nil then
        StrPLCopyW(ReturnedText, UTF8Decode(sReturnedText), MaxLen);
    end;
end;

function CryptProc(PluginNr, CryptoNumber: Integer; Mode: Integer; ConnectionName: UTF8String; var Password: UTF8String): Integer;
const
  cPrefix = 'wfx';
var
  sGroup,
  sPassword: AnsiString;
begin
  try
    sGroup:= WfxOperationList[CryptoNumber];
    case Mode of
    FS_CRYPT_SAVE_PASSWORD:
      begin
        if PasswordStore.WritePassword(cPrefix, sGroup, ConnectionName, Password) then
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
        if PasswordStore.ReadPassword(cPrefix, sGroup, ConnectionName, Password) then
          Result:= FS_FILE_OK;
      end;
    FS_CRYPT_COPY_PASSWORD,
    FS_CRYPT_MOVE_PASSWORD:
      begin
        Result:= FS_FILE_READERROR;
        if PasswordStore.ReadPassword(cPrefix, sGroup, ConnectionName, sPassword) then
          begin
            if not PasswordStore.WritePassword(cPrefix, sGroup, Password, sPassword) then
              Exit(FS_FILE_WRITEERROR);
            if Mode = FS_CRYPT_MOVE_PASSWORD then
              PasswordStore.DeletePassword(cPrefix, sGroup, ConnectionName);
            Result:= FS_FILE_OK;
          end;
      end;
    FS_CRYPT_DELETE_PASSWORD:
      begin
        PasswordStore.DeletePassword(cPrefix, sGroup, ConnectionName);
        Result:= FS_FILE_OK;
      end;
    end;
  except
    Result:= FS_FILE_NOTSUPPORTED;
  end;
end;

function CryptProcA(PluginNr, CryptoNumber: Integer; Mode: Integer; ConnectionName, Password: PAnsiChar; MaxLen: Integer): Integer; dcpcall;
var
  sConnectionName,
  sPassword: UTF8String;
begin
  sConnectionName:= SysToUTF8(StrPas(ConnectionName));
  sPassword:= SysToUTF8(StrPas(Password));
  Result:= CryptProc(PluginNr, CryptoNumber, Mode, sConnectionName, sPassword);
  if Result = FS_FILE_OK then
    begin
      if Password <> nil then
        StrPLCopy(Password, UTF8ToSys(sPassword), MaxLen);
    end;
end;

function CryptProcW(PluginNr, CryptoNumber: Integer; Mode: Integer; ConnectionName, Password: PWideChar; MaxLen: Integer): Integer; dcpcall;
var
  sConnectionName,
  sPassword: UTF8String;
begin
  sConnectionName:= UTF8Encode(WideString(ConnectionName));
  sPassword:= UTF8Encode(WideString(Password));
  Result:= CryptProc(PluginNr, CryptoNumber, Mode, sConnectionName, sPassword);
  if Result = FS_FILE_OK then
    begin
      if Password <> nil then
        StrPLCopyW(Password, UTF8Decode(sPassword), MaxLen);
    end;
end;

{ TWfxPluginFileSource }

constructor TWfxPluginFileSource.Create(aModuleFileName, aPluginRootName: UTF8String);
begin
  inherited Create;
  FPluginNumber:= -1;
  FCallbackDataClass:= nil;
  FModuleFileName:= aModuleFileName;
  FPluginRootName:= aPluginRootName;
  FWfxModule:= TWfxModule.Create;

  if not FWfxModule.LoadModule(FModuleFileName) then
    raise EFileSourceException.Create('Cannot load WFX module ' + FModuleFileName);

  with FWfxModule do
  begin
    FCallbackDataClass:= TCallbackDataClass.Create(Self);
    FPluginNumber:= WfxOperationList.AddObject(FPluginRootName, FCallbackDataClass);
    FsInit(FPluginNumber, @MainProgressProcA, @MainLogProcA, @MainRequestProcA);
    if Assigned(FsInitW) then
      FsInitW(FPluginNumber, @MainProgressProcW, @MainLogProcW, @MainRequestProcW);
    if Assigned(FsSetCryptCallback) then
      FsSetCryptCallback(@CryptProcA, FPluginNumber, 0);
    if Assigned(FsSetCryptCallbackW) then
      FsSetCryptCallbackW(@CryptProcW, FPluginNumber, 0);
    VFSInit(0);
  end;

  FOperationsClasses[fsoList]            := TWfxPluginListOperation.GetOperationClass;
  FOperationsClasses[fsoCopy]            := TWfxPluginCopyOperation.GetOperationClass;
  FOperationsClasses[fsoCopyIn]          := TWfxPluginCopyInOperation.GetOperationClass;
  FOperationsClasses[fsoCopyOut]         := TWfxPluginCopyOutOperation.GetOperationClass;
  FOperationsClasses[fsoMove]            := TWfxPluginMoveOperation.GetOperationClass;
  FOperationsClasses[fsoDelete]          := TWfxPluginDeleteOperation.GetOperationClass;
  FOperationsClasses[fsoCreateDirectory] := TWfxPluginCreateDirectoryOperation.GetOperationClass;
  FOperationsClasses[fsoSetFileProperty] := TWfxPluginSetFilePropertyOperation.GetOperationClass;
  FOperationsClasses[fsoExecute]         := TWfxPluginExecuteOperation.GetOperationClass;

  CreateConnections;
end;

destructor TWfxPluginFileSource.Destroy;
begin
  if (FPluginNumber >= 0) and (FPluginNumber < WfxOperationList.Count) then
    WfxOperationList.Objects[FPluginNumber]:= nil;
  FreeThenNil(FCallbackDataClass);
  inherited Destroy;
end;

class function TWfxPluginFileSource.CreateFile(const APath: String): TFile;
begin
  Result := TFile.Create(APath);

  with Result do
  begin
    AttributesProperty := TFileAttributesProperty.CreateOSAttributes;
    SizeProperty := TFileSizeProperty.Create;
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create;
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create;
    CreationTimeProperty := TFileCreationDateTimeProperty.Create;
    LinkProperty := TFileLinkProperty.Create;
  end;
end;

class function TWfxPluginFileSource.CreateFile(const APath: String; FindData: TWfxFindData): TFile;
begin
  Result := TFile.Create(APath);

  with Result do
  begin
    // Check that attributes is used
    if (FindData.FileAttributes and FILE_ATTRIBUTE_UNIX_MODE) = 0 then // Windows attributes
      begin
        LinkProperty := TFileLinkProperty.Create;
        LinkProperty.IsLinkToDirectory :=
            ((FindData.FileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) and
            ((FindData.FileAttributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0);
        AttributesProperty := TNtfsFileAttributesProperty.Create(FindData.FileAttributes);
      end
    else  // Unix attributes
      begin
        LinkProperty := TFileLinkProperty.Create;
        LinkProperty.IsLinkToDirectory :=
            (((FindData.FileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) or
             ((FindData.FileAttributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0)) and
            ((FindData.Reserved0 and S_IFMT) = S_IFLNK);
        if ((FindData.FileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0) and
           ((FindData.Reserved0 and S_IFMT) <> S_IFDIR) then
          FindData.Reserved0:= FindData.Reserved0 or S_IFDIR;
        AttributesProperty := TUnixFileAttributesProperty.Create(FindData.Reserved0);
      end;

    SizeProperty := TFileSizeProperty.Create(FindData.FileSize);
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create(FindData.LastWriteTime);
    LastAccessTimeProperty := TFileLastAccessDateTimeProperty.Create(FindData.LastAccessTime);
    CreationTimeProperty := TFileCreationDateTimeProperty.Create(FindData.CreationTime);

    // Set name after assigning Attributes property, because it is used to get extension.
    Name := FindData.FileName;
  end;
end;

function TWfxPluginFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  with WfxModule do
  begin
    Result := [fsoList]; // supports by any plugin
    if Assigned(FsPutFile) or Assigned(FsPutFileW) then
      Result:= Result + [fsoCopyIn];
    if Assigned(FsGetFile) or Assigned(FsGetFileW) then
      Result:= Result + [fsoCopyOut];
    if Assigned(FsRenMovFile) or Assigned(FsRenMovFileW) then
      Result:= Result + [fsoCopy, fsoMove];
    if Assigned(FsDeleteFile) or Assigned(FsDeleteFileW) then
        Result:= Result + [fsoDelete];
    if Assigned(FsMkDir) or Assigned(FsMkDirW) then
        Result:= Result + [fsoCreateDirectory];
    if Assigned(FsExecuteFile) or Assigned(FsExecuteFileW) then
        Result:= Result + [fsoExecute];
    if Assigned(FsSetAttr) or Assigned(FsSetAttrW) or
       Assigned(FsExecuteFile) or Assigned(FsExecuteFileW) or
       Assigned(FsRenMovFile) or Assigned(FsRenMovFileW) then
        Result:= Result + [fsoSetFileProperty];
  end;
end;

function TWfxPluginFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspUsesConnections];
  with FWfxModule do
  if Assigned(FsLinksToLocalFiles) and FsLinksToLocalFiles() then
    Result:= Result + [fspLinksToLocalFiles];
end;

function TWfxPluginFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedFileProperties
          + [fpSize, fpAttributes, fpModificationTime, fpCreationTime,
             fpLastAccessTime, fpLink];
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

procedure TWfxPluginFileSource.FillAndCount(Files: TFiles; CountDirs: Boolean;
  ExcludeRootDir: Boolean; out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64);

  procedure FillAndCountRec(const srcPath: UTF8String);
  var
    FindData: TWfxFindData;
    Handle: THandle;
    aFile: TFile;
  begin
    with FWfxModule do
    begin
      Handle := WfxFindFirst(srcPath, FindData);
      if Handle = wfxInvalidHandle then Exit;

      repeat
        if (FindData.FileName = '.') or (FindData.FileName = '..') then Continue;
        aFile:= TWfxPluginFileSource.CreateFile(srcPath, FindData);
        NewFiles.Add(aFile);

        if aFile.IsDirectory then
          begin
            if CountDirs then
              Inc(FilesCount);
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
  aFile: TFile;
begin
  FilesCount:= 0;
  FilesSize:= 0;

  if ExcludeRootDir then
  begin
    if Files.Count <> 1 then
      raise Exception.Create('Only a single directory can be set with ExcludeRootDir=True');
    NewFiles := TFiles.Create(Files[0].FullPath);
    FillAndCountRec(Files[0].FullPath + DirectorySeparator);
  end
  else
  begin
    NewFiles := TFiles.Create(Files.Path);
    for I := 0 to Files.Count - 1 do
    begin
      aFile := Files[I];

      NewFiles.Add(aFile.Clone);

      if aFile.AttributesProperty.IsDirectory and (not aFile.LinkProperty.IsLinkToDirectory) then
        begin
          if CountDirs then
            Inc(FilesCount);
          FillAndCountRec(aFile.Path + aFile.Name + DirectorySeparator);  // recursive browse child dir
        end
      else
        begin
          Inc(FilesCount);
          Inc(FilesSize, aFile.Size); // in first level we know file size -> use it
        end;
    end;
  end;
end;

function TWfxPluginFileSource.WfxCopyMove(sSourceFile, sTargetFile: UTF8String;
                                          Flags: LongInt; RemoteInfo: PRemoteInfo;
                                          Internal, CopyMoveIn: Boolean): LongInt;
var
  bMove,
  bOverWrite: Boolean;
begin
  with FWfxModule do
  begin
    if Internal then
      begin
        bMove:= ((Flags and FS_COPYFLAGS_MOVE) <> 0);
        bOverWrite:= ((Flags and FS_COPYFLAGS_OVERWRITE) <> 0);
        Result:= WfxRenMovFile(sSourceFile, sTargetFile, bMove, bOverWrite, RemoteInfo);
      end
    else
      begin
        if CopyMoveIn then
          Result:= WfxPutFile(sSourceFile, sTargetFile, Flags)
        else
          Result:= WfxGetFile(sSourceFile, sTargetFile, Flags, RemoteInfo);
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

function TWfxPluginFileSource.CreateCopyOperation(var SourceFiles: TFiles;
                                                  TargetPath: String): TFileSourceOperation;
var
  FileSource: IFileSource;
begin
  FileSource := Self;
  Result := TWfxPluginCopyOperation.Create(FileSource, FileSource, SourceFiles, TargetPath);
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

function TWfxPluginFileSource.CreateMoveOperation(var SourceFiles: TFiles;
                                                      TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TWfxPluginMoveOperation.Create(TargetFileSource, SourceFiles, TargetPath);
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

function TWfxPluginFileSource.CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:=  TWfxPluginExecuteOperation.Create(TargetFileSource, ExecutableFile, BasePath, Verb);
end;

function TWfxPluginFileSource.CreateSetFilePropertyOperation(
             var theTargetFiles: TFiles;
             var theNewProperties: TFileProperties): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TWfxPluginSetFilePropertyOperation.Create(
                TargetFileSource,
                theTargetFiles,
                theNewProperties);
end;

function TWfxPluginFileSource.GetLocalName(var aFile: TFile): Boolean;
var
  sFileName: UTF8String;
begin
  Result:= False;
  sFileName:= aFile.FullPath;
  if FWfxModule.WfxGetLocalName(sFileName) then
    begin
      aFile.FullPath:= sFileName;
      Result:= True;
    end;
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
      Result:= TWfxPluginFileSource.Create(sModuleFileName, aRootName);

      DCDebug('Found registered plugin ' + sModuleFileName + ' for file system ' + aRootName);
    end;
end;

procedure TWfxPluginFileSource.AddToConnectionQueue(Operation: TFileSourceOperation);
begin
  WfxOperationsQueueLock.Acquire;
  try
    if WfxOperationsQueue.IndexOf(Operation) < 0 then
      WfxOperationsQueue.Add(Operation);
  finally
    WfxOperationsQueueLock.Release;
  end;
end;

procedure TWfxPluginFileSource.RemoveFromConnectionQueue(Operation: TFileSourceOperation);
begin
  WfxOperationsQueueLock.Acquire;
  try
    WfxOperationsQueue.Remove(Operation);
  finally
    WfxOperationsQueueLock.Release;
  end;
end;

procedure TWfxPluginFileSource.AddConnection(Connection: TFileSourceConnection);
begin
  WfxConnectionsLock.Acquire;
  try
    if WfxConnections.IndexOf(Connection) < 0 then
      WfxConnections.Add(Connection);
  finally
    WfxConnectionsLock.Release;
  end;
end;

procedure TWfxPluginFileSource.RemoveConnection(Connection: TFileSourceConnection);
begin
  WfxConnectionsLock.Acquire;
  try
    WfxConnections.Remove(Connection);
  finally
    WfxConnectionsLock.Release;
  end;
end;

function TWfxPluginFileSource.GetConnection(Operation: TFileSourceOperation): TFileSourceConnection;
begin
  Result := nil;
  case Operation.ID of
    fsoCopyIn:
      Result := WfxConnections[connCopyIn] as TFileSourceConnection;
    fsoCopyOut:
      Result := WfxConnections[connCopyOut] as TFileSourceConnection;
    fsoDelete:
      Result := WfxConnections[connDelete] as TFileSourceConnection;
    else
      begin
        Result := CreateConnection;
        if Assigned(Result) then
          AddConnection(Result);
      end;
  end;

  if Assigned(Result) then
    Result := TryAcquireConnection(Result, Operation);

  // No available connection - wait.
  if not Assigned(Result) then
    AddToConnectionQueue(Operation)
  else
    // Connection acquired.
    // The operation may have been waiting in the queue
    // for the connection, so remove it from the queue.
    RemoveFromConnectionQueue(Operation);
end;

procedure TWfxPluginFileSource.RemoveOperationFromQueue(Operation: TFileSourceOperation);
begin
  RemoveFromConnectionQueue(Operation);
end;

function TWfxPluginFileSource.CreateConnection: TFileSourceConnection;
begin
  Result := TWfxPluginFileSourceConnection.Create(FWfxModule);
end;

procedure TWfxPluginFileSource.CreateConnections;
begin
  WfxConnectionsLock.Acquire;
  try
    if WfxConnections.Count = 0 then
    begin
      // Reserve some connections (only once).
      WfxConnections.Add(CreateConnection); // connCopyIn
      WfxConnections.Add(CreateConnection); // connCopyOut
      WfxConnections.Add(CreateConnection); // connDelete
    end;
  finally
    WfxConnectionsLock.Release;
  end;
end;

function TWfxPluginFileSource.FindConnectionByOperation(operation: TFileSourceOperation): TFileSourceConnection;
var
  i: Integer;
  connection: TFileSourceConnection;
begin
  Result := nil;
  WfxConnectionsLock.Acquire;
  try
    for i := 0 to WfxConnections.Count - 1 do
    begin
      connection := WfxConnections[i] as TFileSourceConnection;
      if connection.AssignedOperation = operation then
        Exit(connection);
    end;
  finally
    WfxConnectionsLock.Release;
  end;
end;

procedure TWfxPluginFileSource.OperationFinished(Operation: TFileSourceOperation);
var
  allowedIDs: TFileSourceOperationTypes = [];
  connection: TFileSourceConnection;
begin
  ClearCurrentOperation(Operation);

  connection := FindConnectionByOperation(Operation);
  if Assigned(connection) then
  begin
    connection.Release; // unassign operation

    WfxConnectionsLock.Acquire;
    try
      // If there are operations waiting, take the first one and notify
      // that a connection is available.
      // Only check operation types for which there are reserved connections.
      if Operation.ID in [fsoCopyIn, fsoCopyOut, fsoDelete, fsoTestArchive] then
      begin
        Include(allowedIDs, Operation.ID);
        NotifyNextWaitingOperation(allowedIDs);
      end
      else
      begin
        WfxConnections.Remove(connection);
      end;

    finally
      WfxConnectionsLock.Release;
    end;
  end;
end;

procedure TWfxPluginFileSource.NotifyNextWaitingOperation(allowedOps: TFileSourceOperationTypes);
var
  i: Integer;
  operation: TFileSourceOperation;
begin
  WfxOperationsQueueLock.Acquire;
  try
    for i := 0 to WfxOperationsQueue.Count - 1 do
    begin
      operation := WfxOperationsQueue.Items[i] as TFileSourceOperation;
      if (operation.State = fsosWaitingForConnection) and
         (operation.ID in allowedOps) then
      begin
        operation.ConnectionAvailableNotify;
        Exit;
      end;
    end;
  finally
    WfxOperationsQueueLock.Release;
  end;
end;

procedure TWfxPluginFileSource.ClearCurrentOperation(Operation: TFileSourceOperation);
begin
  {
  case Operation.ID of
    fsoCopyIn:
      TWfxPluginCopyInOperation.ClearCurrentOperation;
    fsoCopyOut:
      TWfxPluginCopyOutOperation.ClearCurrentOperation;
    fsoDelete:
      TWfxPluginDeleteOperation.ClearCurrentOperation;
  end;
  }
end;

{ TWfxPluginFileSourceConnection }

constructor TWfxPluginFileSourceConnection.Create(aWfxModule: TWfxModule);
begin
  FWfxModule := aWfxModule;
  inherited Create;
end;

{ TCallbackDataClass }

constructor TCallbackDataClass.Create(aFileSource: TWfxPluginFileSource);
begin
  inherited Create;
  FileSource:= aFileSource;
  UpdateProgressFunction:= nil;
end;

initialization
  WfxOperationList:= TStringList.Create;
  WfxConnections := TObjectList.Create(True); // True = destroy objects when destroying list
  WfxConnectionsLock := TCriticalSection.Create;
  WfxOperationsQueue := TObjectList.Create(False); // False = don't destroy operations (only store references)
  WfxOperationsQueueLock := TCriticalSection.Create;

finalization
  FreeThenNil(WfxOperationList);
  FreeThenNil(WfxConnections);
  FreeThenNil(WfxConnectionsLock);
  FreeThenNil(WfxOperationsQueue);
  FreeThenNil(WfxOperationsQueueLock);

end.
