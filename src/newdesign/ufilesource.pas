unit uFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDCUtils, contnrs, syncobjs,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSourceProperty,
  uFileProperty,
  uFile;

type

  TFileSource = class;
  TFileSourceConnection = class;

  { IFileSource }

  IFileSource = interface(IInterface)
    ['{B7F0C4C8-59F6-4A35-A54C-E8242F4AD809}']

    function IsInterface(InterfaceGuid: TGuid): Boolean;
    function IsClass(ClassType: TClass): Boolean;

    function GetCurrentAddress: String;
    function GetCurrentWorkingDirectory: String;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean;

    function GetSupportedFileProperties: TFilePropertiesTypes;
    function GetOperationsTypes: TFileSourceOperationTypes;
    function GetFilePropertiesDescriptions: TFilePropertiesDescriptions;
    function GetProperties: TFileSourceProperties;
    function GetFiles(TargetPath: String): TFiles;

    function CreateFiles: TFiles;

    function CreateListOperation(TargetPath: String): TFileSourceOperation;
    function CreateCopyInOperation(SourceFileSource: IFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation;
    function CreateCopyOutOperation(TargetFileSource: IFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation;
    function CreateMoveOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
    function CreateWipeOperation(var FilesToWipe: TFiles): TFileSourceOperation;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation;
    function CreateExecuteOperation(BasePath, ExecutablePath, Verb: String): TFileSourceOperation;
    function CreateCalcChecksumOperation(var theFiles: TFiles;
                                         aTargetPath: String;
                                         aTargetMask: String): TFileSourceOperation;
    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation;

    function IsPathAtRoot(Path: String): Boolean;
    function GetParentDir(sPath : String): String;
    function GetRootDir(sPath : String): String; overload;
    function GetRootDir: String; overload;
    function GetPathType(sPath : String): TPathType;
    function GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean;

    function GetConnection(Operation: TFileSourceOperation): TFileSourceConnection;
    procedure RemoveFromConnectionQueue(Operation: TFileSourceOperation);

    property CurrentAddress: String read GetCurrentAddress;
    property Properties: TFileSourceProperties read GetProperties;
    property SupportedFileProperties: TFilePropertiesTypes read GetSupportedFileProperties;
  end;

  { TFileSource }

  TFileSource = class(TInterfacedObject, IFileSource)

  private

  protected
    FCurrentAddress: String;

    // These lists are made protected to be able to use them in descendant classes.
    // Always use appropriate lock to access them.
    FConnections: TObjectList; // store connections created by this file source
    FOperationsQueue: TObjectList; // store queued operations, use only under FOperationsQueueLock
    FConnectionsLock: TCriticalSection; // used to synchronize access to connections
    FOperationsQueueLock: TCriticalSection; // used to synchronize access to operations queue

    {en
       Retrieves the full address of the file source
       (the CurrentPath is relative to this).
       This may be used for specifying address:
       - archive : path to archive
       - network : address of server
       etc.
    }
    function GetCurrentAddress: String; virtual;

    {en
       Retrieves the current directory of the file source.
    }
    function GetCurrentWorkingDirectory: String; virtual;
    {en
       Sets the current directory for the file source.
       @returns(@true if path change was successful,
                @false otherwise)
    }
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; virtual;

    {en
       Returns all the properties supported by the file type of the given file source.
    }
    function GetSupportedFileProperties: TFilePropertiesTypes; virtual abstract;
//    class function ClassGetSupportedFileProperties: TFilePropertiesTypes;

    procedure AddToConnectionQueue(Operation: TFileSourceOperation);
    procedure RemoveFromConnectionQueue(Operation: TFileSourceOperation);
    procedure AddConnection(Connection: TFileSourceConnection);
    procedure RemoveConnection(Connection: TFileSourceConnection);

    {en
       Creates a new connection object.
    }
    function CreateConnection: TFileSourceConnection; virtual;
    {en
       Returns a new connection for the operation.
    }
    function GetNewConnection(Operation: TFileSourceOperation): TFileSourceConnection; virtual;
    {en
       Checks if the connection is available and, if it is, assigns it to the operation.
       @returns(Connection object if the connection is available,
                @nil otherwise.)
    }
    function TryAcquireConnection(connection: TFileSourceConnection;
                                  operation: TFileSourceOperation): TFileSourceConnection; virtual;

    {en
       Searches connections list for a connection assigned to operation.
    }
    function FindConnectionByOperation(operation: TFileSourceOperation): TFileSourceConnection; virtual;
    {en
       This function is called when an operation finishes
       and the connection is free again.
       By default it just destroys the connection.
       Override if other logic needed (connection reuse for example).
    }
    procedure FinishedUsingConnection(connection: TFileSourceConnection); virtual;

    procedure NotifyNextWaitingOperation;

    {en
       Callback called when an operation assigned to a connection finishes.
    }
    procedure OperationFinished(Operation: TFileSourceOperation;
                                Event: TFileSourceOperationEvent);

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function IsInterface(InterfaceGuid: TGuid): Boolean;
    function IsClass(aClassType: TClass): Boolean;

    // Retrieve operations permitted on the source.  = capabilities?
    function GetOperationsTypes: TFileSourceOperationTypes; virtual abstract;
//    class function ClassGetOperationsTypes: TFileSourceOperationTypes;

    // Returns a list of property types supported by this source for each file.
    function GetFilePropertiesDescriptions: TFilePropertiesDescriptions; virtual abstract;
//    class function ClassGetFilePropertiesDescriptions: TFilePropertiesDescriptions;

    // Retrieve some properties of the file source.
    function GetProperties: TFileSourceProperties; virtual abstract;
//    class function ClassGetProperties: TFileSourceProperties;

    // Retrieves a list of files.
    // This is the same as GetOperation(fsoList), executing it
    // and returning the result of Operation.ReleaseFiles.
    // Caller is responsible for freeing the result list.
    function GetFiles(TargetPath: String): TFiles; virtual;

    // Create an empty TFiles object of appropriate type for the file source.
    function CreateFiles: TFiles; virtual;

    // These functions create an operation object specific to the file source.
    function CreateListOperation(TargetPath: String): TFileSourceOperation; virtual;
    function CreateCopyInOperation(SourceFileSource: IFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; virtual;
    function CreateCopyOutOperation(TargetFileSource: IFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; virtual;
    function CreateMoveOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation; virtual;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; virtual;
    function CreateWipeOperation(var FilesToWipe: TFiles): TFileSourceOperation; virtual;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; virtual;
    function CreateExecuteOperation(BasePath, ExecutablePath, Verb: String): TFileSourceOperation; virtual;
    function CreateCalcChecksumOperation(var theFiles: TFiles;
                                         aTargetPath: String;
                                         aTargetMask: String): TFileSourceOperation; virtual;
    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation; virtual;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation; virtual;

    {en
       Returns @true if the given path is the root path of the file source,
       @false otherwise.
    }
    function IsPathAtRoot(Path: String): Boolean; virtual;

    function GetParentDir(sPath : String): String; virtual;
    function GetRootDir(sPath : String): String; virtual;
    function GetRootDir: String; virtual;
    function GetPathType(sPath : String): TPathType; virtual;

{
    class function ClassGetParentDir(sPath : String): String;
    class function ClassGetRootDir(sPath : String): String;
    class function ClassGetPathType(sPath : String): TPathType;
}

    function GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean; virtual;

    function GetConnection(Operation: TFileSourceOperation): TFileSourceConnection; virtual;

    property CurrentAddress: String read GetCurrentAddress;
    property Properties: TFileSourceProperties read GetProperties;
    property SupportedFileProperties: TFilePropertiesTypes read GetSupportedFileProperties;

  end;

  { TFileSourceConnection }

  TFileSourceConnection = class
  private
    FAssignedOperation: TFileSourceOperation;
    FOperationLock: TCriticalSection;

    function GetAssignedOperation: TFileSourceOperation;

  protected
    FCurrentPath: String;    // Always includes trailing path delimiter.

    function GetCurrentPath: String; virtual;
    procedure SetCurrentPath(NewPath: String); virtual;

    property AssignedOperation: TFileSourceOperation read GetAssignedOperation;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function IsAvailable: Boolean;
    function Acquire(Operation: TFileSourceOperation): Boolean;
    procedure Release;

    property CurrentPath: String read GetCurrentPath write SetCurrentPath;
  end;

  { TFileSources }

  TFileSources = class(TInterfaceList)
  private
    function Get(I: Integer): IFileSource;

  public
    procedure Assign(otherFileSources: TFileSources);

    property Items[I: Integer]: IFileSource read Get; default;
  end;

  { TFileSourceManager }

  TFileSourceManager = class
  private
    FFileSources: TFileSources;

    // Only allow adding and removing to/from Manager by TFileSource constructor and destructor.
    procedure Add(aFileSource: IFileSource);
    procedure Remove(aFileSource: IFileSource);

  public
    constructor Create;
    destructor Destroy; override;

    function Find(FileSourceClass: TClass; Address: String): IFileSource;
  end;

  EFileSourceException = class(Exception);

var
  FileSourceManager: TFileSourceManager;

implementation

uses
  LCLProc, uFileSourceListOperation;

{ TFileSource }

constructor TFileSource.Create;
begin
  if ClassType = TFileSource then
    raise Exception.Create('Cannot construct abstract class');
  inherited Create;

  FConnections := TObjectList.Create(True); // True = destroy objects when destroying list
  FConnectionsLock := TCriticalSection.Create;
  FOperationsQueue := TObjectList.Create(False); // False = don't destroy operations (only store references)
  FOperationsQueueLock := TCriticalSection.Create;

  FileSourceManager.Add(Self); // Increases RefCount

  // We don't want to count the reference in Manager, because we want to detect
  // when there are no more references other than this single one in the Manager.
  // So, we remove this reference here.
  // When RefCount reaches 0 Destroy gets called and the last remaining reference
  // (in the Manager) is removed there.
  InterLockedDecrement(frefcount);

  DebugLn('Creating ', ClassName);
end;

destructor TFileSource.Destroy;
begin
  DebugLn('Destroying ', ClassName, ' when refcount=', DbgS(refcount));

  if RefCount <> 0 then
  begin
    // There could have been an exception raised in the constructor
    // in which case RefCount will be 1, so only issue warning if there was no exception.
    // This will check for any exception, but it's enough for a warning.
    if not Assigned(ExceptObject) then
      DebugLn('Error: RefCount <> 0 for ', Self.ClassName);
  end;

  if Assigned(FileSourceManager) then
  begin
    // Restore reference removed in Create and
    // remove the instance remaining in Manager.

    // Increase refcount by 2, because we don't want removing the last instance
    // from Manager to trigger another Destroy.

    // RefCount = 0
    InterLockedIncrement(frefcount);
    InterLockedIncrement(frefcount);
    // RefCount = 2
    FileSourceManager.Remove(Self);
    // RefCount = 1
    InterLockedDecrement(frefcount);
    // RefCount = 0 (back at the final value)
  end
  else
    DebugLn('Error: Cannot remove file source - manager already destroyed!');

  inherited Destroy;

  if Assigned(FConnections) then
    FreeAndNil(FConnections);
  if Assigned(FConnectionsLock) then
    FreeAndNil(FConnectionsLock);
  if Assigned(FOperationsQueue) then
    FreeAndNil(FOperationsQueue);
  if Assigned(FOperationsQueueLock) then
    FreeAndNil(FOperationsQueueLock);
end;

function TFileSource.IsInterface(InterfaceGuid: TGuid): Boolean;
var
  t: TObject;
begin
  Result := (Self.QueryInterface(InterfaceGuid, t) = S_OK);
  if Result then
    _Release;  // QueryInterface increases refcount.
end;

function TFileSource.IsClass(aClassType: TClass): Boolean;
begin
  Result := Self is aClassType;
end;

function TFileSource.GetCurrentAddress: String;
begin
  Result := FCurrentAddress;
end;

function TFileSource.GetCurrentWorkingDirectory: String;
begin
  Result := '';
end;

function TFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  // By default every path setting succeeds.
  Result := True;
end;

function TFileSource.IsPathAtRoot(Path: String): Boolean;
begin
  // Default root is '/'. Override in descendant classes for other.
  Result := (Path = PathDelim);
end;

function TFileSource.GetParentDir(sPath : String): String;
begin
  Result := uDCUtils.GetParentDir(sPath);
end;

function TFileSource.GetRootDir(sPath : String): String;
begin
  Result := PathDelim;
end;

function TFileSource.GetPathType(sPath : String): TPathType;
begin
  Result := ptNone;
  if sPath <> '' then
  begin
    // Default root is '/'. Override in descendant classes for other.
    if (sPath[1] = PathDelim) then
      Result := ptAbsolute
    else if ( Pos( PathDelim, sPath ) > 0 ) then
      Result := ptRelative;
  end;
end;

function TFileSource.GetRootDir: String;
begin
  Result := GetRootDir('');
end;

function TFileSource.GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean;
begin
  Result := False; // not supported by default
end;

// Operations.

function TFileSource.GetFiles(TargetPath: String): TFiles;
var
  Operation: TFileSourceOperation;
  ListOperation: TFileSourceListOperation;
begin
  Result := nil;

  if fsoList in GetOperationsTypes then
  begin
    Operation := CreateListOperation(TargetPath);
    if Assigned(Operation) then
      try
        ListOperation := Operation as TFileSourceListOperation;
        ListOperation.Execute;
        Result := ListOperation.ReleaseFiles;

      finally
        FreeAndNil(Operation);
      end;
  end;
end;

function TFileSource.CreateFiles: TFiles;
begin
  Result := TFiles.Create;
end;

function TFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateCopyInOperation(SourceFileSource: IFileSource;
                                           var SourceFiles: TFiles;
                                           TargetPath: String): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateCopyOutOperation(TargetFileSource: IFileSource;
                                            var SourceFiles: TFiles;
                                            TargetPath: String): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateMoveOperation(var SourceFiles: TFiles;
                                         TargetPath: String): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateWipeOperation(var FilesToWipe: TFiles): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateExecuteOperation(BasePath, ExecutablePath, Verb: String): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateCalcChecksumOperation(var theFiles: TFiles;
                                                 aTargetPath: String;
                                                 aTargetMask: String): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                                    var theNewProperties: TFileProperties): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.GetConnection(Operation: TFileSourceOperation): TFileSourceConnection;
begin
  // By default always return a new connection.
  // Override this logic in descendant classes.
  Result := GetNewConnection(Operation);
end;

function TFileSource.TryAcquireConnection(connection: TFileSourceConnection;
                                          operation: TFileSourceOperation): TFileSourceConnection;
begin
  if connection.Acquire(operation) then
  begin
    // We must know when the operation is finished,
    // that is when the connection is free again.
    operation.AddEventsListener([fsoevStateChanged], @OperationFinished);
    Result := connection;
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TFileSource.FinishedUsingConnection(connection: TFileSourceConnection);
begin
  RemoveConnection(connection);
end;

function TFileSource.GetNewConnection(Operation: TFileSourceOperation): TFileSourceConnection;
begin
  Result := CreateConnection;
  if Assigned(Result) then
  begin
    AddConnection(Result);
    Result := TryAcquireConnection(Result, Operation);
  end;
end;

function TFileSource.CreateConnection: TFileSourceConnection;
begin
  Result := nil;
end;

procedure TFileSource.AddToConnectionQueue(Operation: TFileSourceOperation);
begin
  FOperationsQueueLock.Acquire;
  try
    if FOperationsQueue.IndexOf(Operation) < 0 then
      FOperationsQueue.Add(Operation);
  finally
    FOperationsQueueLock.Release;
  end;
end;

procedure TFileSource.RemoveFromConnectionQueue(Operation: TFileSourceOperation);
begin
  FOperationsQueueLock.Acquire;
  try
    FOperationsQueue.Remove(Operation);
  finally
    FOperationsQueueLock.Release;
  end;
end;

procedure TFileSource.AddConnection(Connection: TFileSourceConnection);
begin
  FConnectionsLock.Acquire;
  try
    if FConnections.IndexOf(Connection) < 0 then
      FConnections.Add(Connection);
  finally
    FConnectionsLock.Release;
  end;
end;

procedure TFileSource.RemoveConnection(Connection: TFileSourceConnection);
begin
  FConnectionsLock.Acquire;
  try
    FConnections.Remove(Connection);
  finally
    FConnectionsLock.Release;
  end;
end;

procedure TFileSource.NotifyNextWaitingOperation;
var
  i: Integer;
  operation: TFileSourceOperation;
begin
  FOperationsQueueLock.Acquire;
  try
    for i := 0 to FOperationsQueue.Count - 1 do
    begin
      operation := FOperationsQueue.Items[i] as TFileSourceOperation;
      if operation.State = fsosWaitingForConnection then
      begin
        operation.ConnectionAvailableNotify;
        Exit;
      end;
    end;
  finally
    FOperationsQueueLock.Release;
  end;
end;

function TFileSource.FindConnectionByOperation(operation: TFileSourceOperation): TFileSourceConnection;
var
  i: Integer;
  connection: TFileSourceConnection;
begin
  Result := nil;
  FConnectionsLock.Acquire;
  try
    for i := 0 to FConnections.Count - 1 do
    begin
      connection := FConnections[i] as TFileSourceConnection;
      if connection.AssignedOperation = operation then
        Exit(connection);
    end;
  finally
    FConnectionsLock.Release;
  end;
end;

procedure TFileSource.OperationFinished(Operation: TFileSourceOperation;
                                        Event: TFileSourceOperationEvent);
var
  connection: TFileSourceConnection;
begin
  if (Operation.State = fsosStopped) then
  begin
    operation.RemoveEventsListener([fsoevStateChanged], @OperationFinished);
    connection := FindConnectionByOperation(Operation);
    if Assigned(connection) then
    begin
      connection.Release; // unassign operation
      FinishedUsingConnection(connection);
    end;
  end;
end;

{ TFileSourceConnection }

constructor TFileSourceConnection.Create;
begin
  FAssignedOperation := nil;
  FOperationLock := TCriticalSection.Create;
  inherited Create;
  debugln('Creating connection ', ClassName);
end;

destructor TFileSourceConnection.Destroy;
begin
  if Assigned(FAssignedOperation) and (FAssignedOperation.State <> fsosStopped) then
    DebugLn('Error: Destroying connection ', ClassName, ' with active operation ', FAssignedOperation.ClassName);

  inherited Destroy;

  DebugLn('Destroying connection ', ClassName);

  if Assigned(FOperationLock) then
    FreeAndNil(FOperationLock);
end;

function TFileSourceConnection.GetAssignedOperation: TFileSourceOperation;
begin
  // For just reading lock is probably not needed here.
  Result := FAssignedOperation;
end;

function TFileSourceConnection.GetCurrentPath: String;
begin
  Result := FCurrentPath;
end;

procedure TFileSourceConnection.SetCurrentPath(NewPath: String);
begin
  if NewPath <> '' then
    NewPath := IncludeTrailingPathDelimiter(NewPath);

  FCurrentPath := NewPath;
end;

function TFileSourceConnection.IsAvailable: Boolean;
begin
  Result := (GetAssignedOperation() = nil);
end;

function TFileSourceConnection.Acquire(Operation: TFileSourceOperation): Boolean;
begin
  FOperationLock.Acquire;
  try
    Result := (FAssignedOperation = nil);
    if Result then
      FAssignedOperation := Operation;
  finally
    FOperationLock.Release;
  end;
end;

procedure TFileSourceConnection.Release;
begin
  FOperationLock.Acquire;
  try
    FAssignedOperation := nil;
  finally
    FOperationLock.Release;
  end;
end;

{ TFileSources }

function TFileSources.Get(I: Integer): IFileSource;
begin
  if (I >= 0) and (I < Count) then
    Result := inherited Items[I] as IFileSource
  else
    Result := nil;
end;

procedure TFileSources.Assign(otherFileSources: TFileSources);
var
  i: Integer;
begin
  Clear;
  for i := 0 to otherFileSources.Count - 1 do
    Add(otherFileSources.Items[i]);
end;

{ TFileSourceManager }

constructor TFileSourceManager.Create;
begin
  FFileSources := TFileSources.Create;
end;

destructor TFileSourceManager.Destroy;
var
  i: Integer;
begin
  if FFileSources.Count > 0 then
  begin
    DebugLn('Warning: Destroying manager with existing file sources!');

    for i := 0 to FFileSources.Count - 1 do
    begin
      // Restore the reference taken in TFileSource.Create before removing
      // all file sources from the list.
      FFileSources[i]._AddRef;
      // Free instance.
      FFileSources.put(i, nil);
    end;
  end;

  if Assigned(FFileSources) then
    FreeAndNil(FFileSources);

  inherited;
end;

procedure TFileSourceManager.Add(aFileSource: IFileSource);
begin
  if FFileSources.IndexOf(aFileSource) < 0 then
  begin
    FFileSources.Add(aFileSource);
  end
  else
    DebugLn('Error: File source already exists in manager!');
end;

procedure TFileSourceManager.Remove(aFileSource: IFileSource);
begin
  FFileSources.Remove(aFileSource);
end;

function TFileSourceManager.Find(FileSourceClass: TClass; Address: String): IFileSource;
var
  i: Integer;
begin
  for i := 0 to FFileSources.Count - 1 do
  begin
    if (FFileSources[i].IsClass(FileSourceClass)) and
       (FFileSources[i].CurrentAddress = Address) then
    begin
      Result := FFileSources[i];
      Exit;
    end;
  end;
  Result := nil;
end;

initialization
  FileSourceManager := TFileSourceManager.Create;

finalization
  FreeAndNil(FileSourceManager);

end.

