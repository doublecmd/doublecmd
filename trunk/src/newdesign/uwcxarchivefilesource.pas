unit uWcxArchiveFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, syncobjs, StringHashList, uOSUtils,
  WcxPlugin, uWCXmodule, uFile, uFileSourceProperty, uFileSourceOperationTypes,
  uArchiveFileSource, uFileProperty, uFileSource, uFileSourceOperation;

type

  TWcxArchiveFileSourceConnection = class;

  { IWcxArchiveFileSource }

  IWcxArchiveFileSource = interface(IArchiveFileSource)
    ['{DB32E8A8-486B-4053-9448-4C145C1A33FA}']

    function GetArcFileList: TObjectList;
    function GetPluginCapabilities: PtrInt;
    function GetWcxModule: TWcxModule;

    property ArchiveFileList: TObjectList read GetArcFileList;
    property PluginCapabilities: PtrInt read GetPluginCapabilities;
    property WcxModule: TWCXModule read GetWcxModule;
  end;

  { TWcxArchiveFileSource }

  TWcxArchiveFileSource = class(TArchiveFileSource, IWcxArchiveFileSource)
  private
    FModuleFileName: String;
    FPluginCapabilities: PtrInt;
    FArcFileList : TObjectList;
    FWcxModule: TWCXModule;

    function LoadModule: Boolean;
    procedure UnloadModule;

    function ReadArchive(bCanYouHandleThisFile : Boolean = False): Boolean;

    function GetArcFileList: TObjectList;
    function GetPluginCapabilities: PtrInt;
    function GetWcxModule: TWcxModule;

    function CreateConnection: TFileSourceConnection;

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
    procedure OperationFinished(Operation: TFileSourceOperation); override;

    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

    procedure DoReload(const PathsToReload: TPathsArray); override;

  public
    constructor Create(anArchiveFileName: String;
                       aWcxPluginFileName: String;
                       aWcxPluginCapabilities: PtrInt); reintroduce;
    destructor Destroy; override;

    class function CreateFile(const APath: String; WcxHeader: TWCXHeader): TFile; overload;

    // Retrieve operations permitted on the source.  = capabilities?
    function GetOperationsTypes: TFileSourceOperationTypes; override;

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
    function CreateExecuteOperation(const ExecutableFile: TFile;
                                    BasePath, Verb: String): TFileSourceOperation; override;
    function CreateTestArchiveOperation(var theSourceFiles: TFiles): TFileSourceOperation; override;

    class function CreateByArchiveType(anArchiveFileName, anArchiveType: String): IWcxArchiveFileSource;
    class function CreateByArchiveName(anArchiveFileName: String): IWcxArchiveFileSource;

    function GetConnection(Operation: TFileSourceOperation): TFileSourceConnection; override;
    procedure RemoveOperationFromQueue(Operation: TFileSourceOperation); override;

    property ArchiveFileList: TObjectList read FArcFileList;
    property PluginCapabilities: PtrInt read FPluginCapabilities;
    property WcxModule: TWCXModule read FWcxModule;
  end;

  { TWcxArchiveFileSourceConnection }

  TWcxArchiveFileSourceConnection = class(TFileSourceConnection)
  private
    FWcxModule: TWCXModule;

  public
    constructor Create(aWcxModule: TWCXModule); reintroduce;

    property WcxModule: TWCXModule read FWcxModule;
  end;

  EModuleNotLoadedException = class(EFileSourceException);

implementation

uses
  uGlobs, LCLProc, uDCUtils,
  uDateTimeUtils,
  FileUtil,
  uWcxArchiveListOperation,
  uWcxArchiveCopyInOperation,
  uWcxArchiveCopyOutOperation,
  uWcxArchiveDeleteOperation,
  uWcxArchiveExecuteOperation,
  uWcxArchiveTestArchiveOperation;

const
  connCopyIn      = 0;
  connCopyOut     = 1;
  connDelete      = 2;
  connTestArchive = 3;

var
  // Always use appropriate lock to access these lists.
  WcxConnections: TObjectList; // store connections created by Wcx file sources
  WcxOperationsQueue: TObjectList; // store queued operations, use only under FOperationsQueueLock
  WcxConnectionsLock: TCriticalSection; // used to synchronize access to connections
  WcxOperationsQueueLock: TCriticalSection; // used to synchronize access to operations queue

class function TWcxArchiveFileSource.CreateByArchiveType(anArchiveFileName, anArchiveType: String): IWcxArchiveFileSource;
var
  i: Integer;
  ModuleFileName: String;
begin
  Result := nil;

  // Check if there is a registered plugin for the extension of the archive file name.
  for i := 0 to gWCXPlugins.Count - 1 do
  begin
    if SameText(anArchiveType, gWCXPlugins.Ext[i]) and (gWCXPlugins.Enabled[i]) then
    begin
      ModuleFileName := GetCmdDirFromEnvVar(gWCXPlugins.FileName[I]);

      Result := TWcxArchiveFileSource.Create(anArchiveFileName,
                                             ModuleFileName,
                                             gWCXPlugins.Flags[I]);

      DebugLn('Found registered plugin ' + ModuleFileName + ' for archive ' + anArchiveFileName);
      break;
    end;
  end;
end;

class function TWcxArchiveFileSource.CreateByArchiveName(anArchiveFileName: String): IWcxArchiveFileSource;
var
  sExtension: String;
begin
  Result := nil;

  sExtension := ExtractFileExt(anArchiveFileName);
  if sExtension <> '' then   // delete '.' at the front
    Delete(sExtension, 1, 1);

  Result:= CreateByArchiveType(anArchiveFileName, sExtension);
end;

// ----------------------------------------------------------------------------

constructor TWcxArchiveFileSource.Create(anArchiveFileName: String;
                                         aWcxPluginFileName: String;
                                         aWcxPluginCapabilities: PtrInt);
begin
  inherited Create(anArchiveFileName);

  FModuleFileName := aWcxPluginFileName;
  FPluginCapabilities := aWcxPluginCapabilities;
  FArcFileList := TObjectList.Create(True);
  FWcxModule := TWCXModule.Create;

  if LoadModule = False then
    raise EModuleNotLoadedException.Create('Cannot load WCX module ' + FModuleFileName);

  ReadArchive;

  WcxConnectionsLock.Acquire;
  try
    if WcxConnections.Count = 0 then
    begin
      // Reserve some connections (only once).
      WcxConnections.Add(CreateConnection); // connCopyIn
      WcxConnections.Add(CreateConnection); // connCopyOut
      WcxConnections.Add(CreateConnection); // connDelete
      WcxConnections.Add(CreateConnection); // connTestArchive
    end;
  finally
    WcxConnectionsLock.Release;
  end;
end;

destructor TWcxArchiveFileSource.Destroy;
begin
  UnloadModule;

  inherited;

  if Assigned(FArcFileList) then
    FreeAndNil(FArcFileList);
  if Assigned(FWcxModule) then
    FreeAndNil(FWcxModule);
end;

class function TWcxArchiveFileSource.CreateFile(const APath: String; WcxHeader: TWCXHeader): TFile;
begin
  Result := TFile.Create(APath);

  with Result do
  begin
  {
      FileCRC,
      CompressionMethod,
      Comment,
  }
    SizeProperty := TFileSizeProperty.Create(WcxHeader.UnpSize);
    CompressedSizeProperty := TFileCompressedSizeProperty.Create(WcxHeader.PackSize);
    AttributesProperty := {TNtfsFileAttributesProperty or Unix?}
                          TFileAttributesProperty.CreateOSAttributes(WcxHeader.FileAttr);
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create(0);
    try
      ModificationTime := WcxFileTimeToDateTime(WcxHeader);
    except
      on EConvertError do;
    end;

    // Set name after assigning Attributes property, because it is used to get extension.
    Name := ExtractFileName(WcxHeader.FileName);
  end;
end;

function TWcxArchiveFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList, fsoCopyOut, fsoTestArchive, fsoExecute]; // by default
  with FWcxModule do
  begin
    if (((FPluginCapabilities and PK_CAPS_NEW) <> 0) or ((FPluginCapabilities and PK_CAPS_MODIFY) <> 0)) and
       (Assigned(PackFiles) or Assigned(PackFilesW)) then
      Result:= Result + [fsoCopyIn];
    if ((FPluginCapabilities and PK_CAPS_DELETE) <> 0) and
       (Assigned(DeleteFiles) or Assigned(DeleteFilesW)) then
      Result:= Result + [fsoDelete];
  end;
end;

function TWcxArchiveFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspUsesConnections];
end;

function TWcxArchiveFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedFileProperties;
end;

function TWcxArchiveFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
var
  I: Integer;
  Header: TWCXHeader;
begin
  Result := False;
  if Length(NewDir) > 0 then
  begin
    if NewDir = GetRootDir() then
      Exit(True);

    NewDir := IncludeTrailingPathDelimiter(NewDir);

    // Search file list for a directory with name NewDir.
    for I := 0 to FArcFileList.Count - 1 do
    begin
      Header := TWCXHeader(FArcFileList.Items[I]);
      if FPS_ISDIR(Header.FileAttr) and (Length(Header.FileName) > 0) then
      begin
        if NewDir = IncludeTrailingPathDelimiter(GetRootDir() + Header.FileName) then
          Exit(True);
      end;
    end;
  end;
end;

function TWcxArchiveFileSource.LoadModule: Boolean;
begin
  Result := WcxModule.LoadModule(FModuleFileName);
end;

procedure TWcxArchiveFileSource.UnloadModule;
begin
  WcxModule.UnloadModule;
end;

function TWcxArchiveFileSource.GetArcFileList: TObjectList;
begin
  Result := FArcFileList;
end;

function TWcxArchiveFileSource.GetPluginCapabilities: PtrInt;
begin
  Result := FPluginCapabilities;
end;

function TWcxArchiveFileSource.GetWcxModule: TWcxModule;
begin
  Result := FWcxModule;
end;

function TWcxArchiveFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TWcxArchiveListOperation.Create(TargetFileSource, TargetPath);
end;

function TWcxArchiveFileSource.CreateCopyInOperation(
            SourceFileSource: IFileSource;
            var SourceFiles: TFiles;
            TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TWcxArchiveCopyInOperation.Create(SourceFileSource,
                                              TargetFileSource,
                                              SourceFiles, TargetPath);
end;

function TWcxArchiveFileSource.CreateCopyOutOperation(
            TargetFileSource: IFileSource;
            var SourceFiles: TFiles;
            TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  Result := TWcxArchiveCopyOutOperation.Create(SourceFileSource,
                                               TargetFileSource,
                                               SourceFiles, TargetPath);
end;

function TWcxArchiveFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TWcxArchiveDeleteOperation.Create(TargetFileSource,
                                              FilesToDelete);
end;

function TWcxArchiveFileSource.CreateExecuteOperation(const ExecutableFile: TFile;
                                                      BasePath, Verb: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:=  TWcxArchiveExecuteOperation.Create(TargetFileSource, ExecutableFile, BasePath, Verb);
end;

function TWcxArchiveFileSource.CreateTestArchiveOperation(var theSourceFiles: TFiles): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  Result:=  TWcxArchiveTestArchiveOperation.Create(SourceFileSource, theSourceFiles);
end;

function TWcxArchiveFileSource.ReadArchive(bCanYouHandleThisFile : Boolean = False): Boolean;

  procedure CollectDirs(Path: PAnsiChar; var DirsList: TStringHashList);
  var
    I : Integer;
    Dir : AnsiString;
  begin
    // Scan from the second char from the end, to the second char from the beginning.
    for I := strlen(Path) - 2 downto 1 do
    begin
      if Path[I] = PathDelim then
      begin
        SetString(Dir, Path, I);
        if DirsList.Find(Dir) = -1 then
          // Add directory and continue scanning for parent directories.
          DirsList.Add(Dir)
        else
          // This directory is already in the list and we assume
          // that all parent directories are too.
          Exit;
      end
    end;
  end;

var
  ArcHandle : TArcHandle;
  Header: TWCXHeader;
  AllDirsList, ExistsDirList : TStringHashList;
  I : Integer;
  NameLength: Integer;
  iResult : Integer;
  lOpenResult : Longint;
begin
  if not mbFileAccess(ArchiveFileName, fmOpenRead) then
    begin
      Result := False;
      Exit;
    end;

  if bCanYouHandleThisFile and (Assigned(WcxModule.CanYouHandleThisFile) or Assigned(WcxModule.CanYouHandleThisFileW)) then
    begin
      Result := WcxModule.WcxCanYouHandleThisFile(ArchiveFileName);
      if not Result then Exit;
    end;

  DebugLN('Open Archive');

  (*Open Archive*)
  ArcHandle := WcxModule.OpenArchiveHandle(ArchiveFileName, PK_OM_LIST, lOpenResult);
  if ArcHandle = 0 then
    begin
      {if not bCanYouHandleThisFile then
        ShowErrorMsg(lOpenResult);}
      Result := False;
      Exit;
    end;

  WcxModule.WcxSetChangeVolProc(ArcHandle, nil, nil {ChangeVolProc});
  WcxModule.WcxSetProcessDataProc(ArcHandle, nil, nil {ProcessDataProc});

  DebugLN('Get File List');
  (*Get File List*)
  FArcFileList.Clear;
  ExistsDirList := TStringHashList.Create(True);
  AllDirsList := TStringHashList.Create(True);

  try
    while (WcxModule.ReadWCXHeader(ArcHandle, Header) = E_SUCCESS) do
      begin
        // Some plugins end directories with path delimiter. Delete it if present.
        if FPS_ISDIR(Header.FileAttr) then
        begin
          NameLength := Length(Header.FileName);
          if (Header.FileName[NameLength] = PathDelim) then
            Delete(Header.FileName, NameLength, 1);

        //****************************
        (* Workaround for plugins that don't give a list of folders
           or the list does not include all of the folders. *)

          // Collect directories that the plugin supplies.
          if (ExistsDirList.Find(Header.FileName) < 0) then
            ExistsDirList.Add(Header.FileName);
        end;

        // Collect all directories.
        CollectDirs(PAnsiChar(Header.FileName), AllDirsList);

        //****************************

        FArcFileList.Add(Header);

        // get next file
        iResult := WcxModule.WcxProcessFile(ArcHandle, PK_SKIP, EmptyStr, EmptyStr);

        //Check for errors
        {if iResult <> E_SUCCESS then
          ShowErrorMessage;}
      end; // while

      (* if plugin does not give a list of folders *)
      for I := 0 to AllDirsList.Count - 1 do
      begin
        // Add only those directories that were not supplied by the plugin.
        if ExistsDirList.Find(AllDirsList.List[I]^.Key) < 0 then
        begin
          Header := TWCXHeader.Create;
          try
            Header.FileName := AllDirsList.List[I]^.Key;
            Header.ArcName  := ArchiveFileName;
            Header.FileAttr := faFolder;
{$IFDEF MSWINDOWS}
            WinToDosTime(mbFileAge(ArchiveFileName), Header.FileTime);
{$ELSE}
{$PUSH}{$R-}
            Header.FileTime := LongInt(mbFileAge(ArchiveFileName));
{$POP}
{$ENDIF}
            FArcFileList.Add(Header);
          except
            FreeAndNil(Header);
          end;
        end;
      end;

  finally
    AllDirsList.Free;
    ExistsDirList.Free;
    WcxModule.CloseArchive(ArcHandle);
  end;

  Result := True;
end;

procedure TWcxArchiveFileSource.AddToConnectionQueue(Operation: TFileSourceOperation);
begin
  WcxOperationsQueueLock.Acquire;
  try
    if WcxOperationsQueue.IndexOf(Operation) < 0 then
      WcxOperationsQueue.Add(Operation);
  finally
    WcxOperationsQueueLock.Release;
  end;
end;

procedure TWcxArchiveFileSource.RemoveFromConnectionQueue(Operation: TFileSourceOperation);
begin
  WcxOperationsQueueLock.Acquire;
  try
    WcxOperationsQueue.Remove(Operation);
  finally
    WcxOperationsQueueLock.Release;
  end;
end;

procedure TWcxArchiveFileSource.AddConnection(Connection: TFileSourceConnection);
begin
  WcxConnectionsLock.Acquire;
  try
    if WcxConnections.IndexOf(Connection) < 0 then
      WcxConnections.Add(Connection);
  finally
    WcxConnectionsLock.Release;
  end;
end;

procedure TWcxArchiveFileSource.RemoveConnection(Connection: TFileSourceConnection);
begin
  WcxConnectionsLock.Acquire;
  try
    WcxConnections.Remove(Connection);
  finally
    WcxConnectionsLock.Release;
  end;
end;

function TWcxArchiveFileSource.GetConnection(Operation: TFileSourceOperation): TFileSourceConnection;
begin
  Result := nil;
  case Operation.ID of
    fsoCopyIn:
      Result := WcxConnections[connCopyIn] as TFileSourceConnection;
    fsoCopyOut:
      Result := WcxConnections[connCopyOut] as TFileSourceConnection;
    fsoDelete:
      Result := WcxConnections[connDelete] as TFileSourceConnection;
    fsoTestArchive:
      Result := WcxConnections[connTestArchive] as TFileSourceConnection;
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

procedure TWcxArchiveFileSource.RemoveOperationFromQueue(Operation: TFileSourceOperation);
begin
  RemoveFromConnectionQueue(Operation);
end;

function TWcxArchiveFileSource.CreateConnection: TFileSourceConnection;
begin
  Result := TWcxArchiveFileSourceConnection.Create(FWcxModule);
end;

function TWcxArchiveFileSource.FindConnectionByOperation(operation: TFileSourceOperation): TFileSourceConnection;
var
  i: Integer;
  connection: TFileSourceConnection;
begin
  Result := nil;
  WcxConnectionsLock.Acquire;
  try
    for i := 0 to WcxConnections.Count - 1 do
    begin
      connection := WcxConnections[i] as TFileSourceConnection;
      if connection.AssignedOperation = operation then
        Exit(connection);
    end;
  finally
    WcxConnectionsLock.Release;
  end;
end;

procedure TWcxArchiveFileSource.OperationFinished(Operation: TFileSourceOperation);
var
  allowedIDs: TFileSourceOperationTypes = [];
  connection: TFileSourceConnection;
begin
  ClearCurrentOperation(Operation);

  connection := FindConnectionByOperation(Operation);
  if Assigned(connection) then
  begin
    connection.Release; // unassign operation

    WcxConnectionsLock.Acquire;
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
        WcxConnections.Remove(connection);
      end;

    finally
      WcxConnectionsLock.Release;
    end;
  end;
end;

procedure TWcxArchiveFileSource.NotifyNextWaitingOperation(allowedOps: TFileSourceOperationTypes);
var
  i: Integer;
  operation: TFileSourceOperation;
begin
  WcxOperationsQueueLock.Acquire;
  try
    for i := 0 to WcxOperationsQueue.Count - 1 do
    begin
      operation := WcxOperationsQueue.Items[i] as TFileSourceOperation;
      if (operation.State = fsosWaitingForConnection) and
         (operation.ID in allowedOps) then
      begin
        operation.ConnectionAvailableNotify;
        Exit;
      end;
    end;
  finally
    WcxOperationsQueueLock.Release;
  end;
end;

procedure TWcxArchiveFileSource.ClearCurrentOperation(Operation: TFileSourceOperation);
begin
  case Operation.ID of
    fsoCopyIn:
      TWcxArchiveCopyInOperation.ClearCurrentOperation;
    fsoCopyOut:
      TWcxArchiveCopyOutOperation.ClearCurrentOperation;
    fsoDelete:
      TWcxArchiveDeleteOperation.ClearCurrentOperation;
    fsoTestArchive:
      TWcxArchiveTestArchiveOperation.ClearCurrentOperation;
  end;
end;

procedure TWcxArchiveFileSource.DoReload(const PathsToReload: TPathsArray);
begin
  ReadArchive;
end;

{ TWcxArchiveFileSourceConnection }

constructor TWcxArchiveFileSourceConnection.Create(aWcxModule: TWCXModule);
begin
  FWcxModule := aWcxModule;
  inherited Create;
end;

initialization
  WcxConnections := TObjectList.Create(True); // True = destroy objects when destroying list
  WcxConnectionsLock := TCriticalSection.Create;
  WcxOperationsQueue := TObjectList.Create(False); // False = don't destroy operations (only store references)
  WcxOperationsQueueLock := TCriticalSection.Create;

finalization
  FreeThenNil(WcxConnections);
  FreeThenNil(WcxConnectionsLock);
  FreeThenNil(WcxOperationsQueue);
  FreeThenNil(WcxOperationsQueueLock);

end.
