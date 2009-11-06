unit uWcxArchiveFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Dialogs, StringHashList, uOSUtils,
  WcxPlugin, uWCXmodule, uFile, uFileSourceProperty, uFileSourceOperationTypes,
  uArchiveFileSource, uFileProperty, uFileSource, uFileSourceOperation;

type

  TWcxArchiveFileSourceConnection = class;

  { IWcxArchiveFileSource }

  IWcxArchiveFileSource = interface(IArchiveFileSource)
    ['{DB32E8A8-486B-4053-9448-4C145C1A33FA}']

    function GetArcFileList: TObjectList;
    function GetPluginFlags: PtrInt;
    procedure SetPluginFlags(NewPluginFlags: PtrInt);
    function GetWcxModule: TWcxModule;

    property ArchiveFileList: TObjectList read GetArcFileList;
    property PluginFlags: PtrInt read GetPluginFlags write SetPluginFlags;
    property WcxModule: TWCXModule read GetWcxModule;
  end;

  { TWcxArchiveFileSource }

  TWcxArchiveFileSource = class(TArchiveFileSource, IWcxArchiveFileSource)
  private
    FModuleFileName: String;
    FPluginFlags: PtrInt;
    FArcFileList : TObjectList;
    FWcxModule: TWCXModule;

    function LoadModule: Boolean;
    procedure UnloadModule;

    function ReadArchive(bCanYouHandleThisFile : Boolean = False): Boolean;

    function GetArcFileList: TObjectList;
    function GetPluginFlags: PtrInt;
    procedure SetPluginFlags(NewPluginFlags: PtrInt);
    function GetWcxModule: TWcxModule;

    function CreateConnection: TFileSourceConnection; override;
    procedure FinishedUsingConnection(connection: TFileSourceConnection); override;
    procedure NotifyNextWaitingOperation(allowedOps: TFileSourceOperationTypes);

  protected
    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

  public
    constructor Create(anArchiveFileName: String;
                       aWcxPluginFileName: String;
                       aWcxPluginFlags: PtrInt); reintroduce;
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

    class function CreateByArchiveName(anArchiveFileName: String): IWcxArchiveFileSource;

    function GetConnection(Operation: TFileSourceOperation): TFileSourceConnection; override;

    property ArchiveFileList: TObjectList read FArcFileList;
    property PluginFlags: PtrInt read FPluginFlags write FPluginFlags;
    property WcxModule: TWCXModule read FWcxModule;
  end;

  { TWcxArchiveFileSourceConnection }

  TWcxArchiveFileSourceConnection = class(TFileSourceConnection)
  private
    FWcxModule: TWCXModule;

  public
    constructor Create(aWcxModule: TWCXModule);

    property WcxModule: TWCXModule read FWcxModule;
  end;

  EModuleNotLoadedException = class(EFileSourceException);

implementation

uses Forms, Controls, uGlobs, LCLProc, uDCUtils,
     uGlobsPaths, FileUtil, uWcxArchiveFile,
     uWcxArchiveListOperation,
     uWcxArchiveCopyInOperation,
     uWcxArchiveCopyOutOperation,
     uWcxArchiveDeleteOperation;

const
  connCopyIn  = 0;
  connCopyOut = 1;
  connMove    = 2;
  connDelete  = 3;

class function TWcxArchiveFileSource.CreateByArchiveName(anArchiveFileName: String): IWcxArchiveFileSource;
var
  i: Integer;
  ModuleFileName: String;
  sExtension: String;
begin
  Result := nil;

  // Check if there is a registered plugin for the extension of the archive file name.
  for i := 0 to gWCXPlugins.Count - 1 do
  begin
    sExtension := ExtractFileExt(anArchiveFileName);
    if sExtension <> '' then   // delete '.' at the front
      Delete(sExtension, 1, 1);

    if (sExtension = gWCXPlugins.Ext[i]) and (gWCXPlugins.Enabled[i]) then
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

// ----------------------------------------------------------------------------

constructor TWcxArchiveFileSource.Create(anArchiveFileName: String;
                                         aWcxPluginFileName: String;
                                         aWcxPluginFlags: PtrInt);
begin
  inherited Create(anArchiveFileName);

  FModuleFileName := aWcxPluginFileName;
  FPluginFlags := aWcxPluginFlags;
  FArcFileList := TObjectList.Create(True);
  FWcxModule := TWCXModule.Create;

  if LoadModule = False then
    raise EModuleNotLoadedException.Create('Cannot load WCX module ' + FModuleFileName);

  ReadArchive;

  // Reserve some connections.
  FConnections.Add(CreateConnection); // connCopyIn
  FConnections.Add(CreateConnection); // connCopyOut
  FConnections.Add(CreateConnection); // connMove
  FConnections.Add(CreateConnection); // connDelete
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

function TWcxArchiveFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList, fsoCopyIn, fsoCopyOut, fsoDelete];
end;

function TWcxArchiveFileSource.GetFilePropertiesDescriptions: TFilePropertiesDescriptions;
begin
  Result := nil;
end;

function TWcxArchiveFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspUsesConnections];
end;

function TWcxArchiveFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := TWcxArchiveFile.GetSupportedProperties;
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
  WcxModule.VFSInit(FPluginFlags);
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

function TWcxArchiveFileSource.GetPluginFlags: PtrInt;
begin
  Result := FPluginFlags;
end;

procedure TWcxArchiveFileSource.SetPluginFlags(NewPluginFlags: PtrInt);
begin
  FPluginFlags := NewPluginFlags;
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

  if bCanYouHandleThisFile and Assigned(WcxModule.CanYouHandleThisFile) then
    begin
      Result := WcxModule.CanYouHandleThisFile(PChar(UTF8ToSys(ArchiveFileName)));
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

//  WCXModule := Self;  // set WCXModule variable to current module
{  SetChangeVolProc(ArcHandle, ChangeVolProc);
  SetProcessDataProc(ArcHandle, ProcessDataProc);}

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
        iResult := WcxModule.ProcessFile(ArcHandle, PK_SKIP, nil, nil);

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
            Header.FileTime := mbFileAge(ArchiveFileName);
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

function TWcxArchiveFileSource.GetConnection(Operation: TFileSourceOperation): TFileSourceConnection;
begin
  Result := nil;
  case Operation.ID of
    fsoCopyIn:
      Result := TryAcquireConnection(FConnections[connCopyIn] as TFileSourceConnection, Operation);
    fsoCopyOut:
      Result := TryAcquireConnection(FConnections[connCopyOut] as TFileSourceConnection, Operation);
    fsoMove:
      Result := TryAcquireConnection(FConnections[connMove] as TFileSourceConnection, Operation);
    fsoDelete:
      Result := TryAcquireConnection(FConnections[connDelete] as TFileSourceConnection, Operation);
    else
      Result := GetNewConnection(Operation);
  end;

  // No available connection - wait.
  if not Assigned(Result) then
    AddToConnectionQueue(operation);
end;

function TWcxArchiveFileSource.CreateConnection: TFileSourceConnection;
begin
  Result := TWcxArchiveFileSourceConnection.Create(FWcxModule);
end;

procedure TWcxArchiveFileSource.FinishedUsingConnection(connection: TFileSourceConnection);
begin
  FConnectionsLock.Acquire;
  try
    // If there are operations waiting, take the first one and notify
    // that a connection is available.
    // Only check operations for which there are reserved connections.
    if connection = FConnections[connCopyIn] then
      NotifyNextWaitingOperation([fsoCopyIn])
    else if connection = FConnections[connCopyOut] then
      NotifyNextWaitingOperation([fsoCopyOut])
    else if connection = FConnections[connMove] then
      NotifyNextWaitingOperation([fsoMove])
    else if connection = FConnections[connDelete] then
      NotifyNextWaitingOperation([fsoDelete])
    else
    begin
      RemoveConnection(connection);
    end;

  finally
    FConnectionsLock.Release;
  end;
end;

procedure TWcxArchiveFileSource.NotifyNextWaitingOperation(allowedOps: TFileSourceOperationTypes);
var
  i: Integer;
  operation: TFileSourceOperation;
begin
  FOperationsQueueLock.Acquire;
  try
    for i := 0 to FOperationsQueue.Count - 1 do
    begin
      operation := FOperationsQueue.Items[i] as TFileSourceOperation;
      if (operation.State = fsosWaitingForConnection) and
         (operation.ID in allowedOps) then
      begin
        operation.ConnectionAvailableNotify;
        Exit;
      end;
    end;

  finally
    FOperationsQueueLock.Release;
  end;
end;

{ TWcxArchiveFileSourceConnection }

constructor TWcxArchiveFileSourceConnection.Create(aWcxModule: TWCXModule);
begin
  FWcxModule := aWcxModule;
  inherited Create;
end;

end.

