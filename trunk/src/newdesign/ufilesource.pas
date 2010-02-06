unit uFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDCUtils, syncobjs, LCLProc,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSourceProperty,
  uFileProperty,
  uFile;

type

  TFileSource = class;
  TFileSourceConnection = class;
  IFileSource = interface;

  TPathsArray = array of string;

  TFileSourceReloadEventNotify = procedure(const aFileSource: IFileSource;
                                           const ReloadedPaths: TPathsArray) of object;

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

    function CreateFiles(const APath: String): TFiles;

    function CreateListOperation(TargetPath: String): TFileSourceOperation;
    function CreateCopyOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation;
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
    function CreateExecuteOperation(const ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation;
    function CreateTestArchiveOperation(var theSourceFiles: TFiles): TFileSourceOperation;
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
    function GetLocalName(var aFile: TFile): Boolean;

    function GetConnection(Operation: TFileSourceOperation): TFileSourceConnection;
    procedure RemoveOperationFromQueue(Operation: TFileSourceOperation);

    procedure Reload(const PathsToReload: TPathsArray);
    procedure Reload(const PathToReload: String);
    procedure AddReloadEventListener(FunctionToCall: TFileSourceReloadEventNotify);
    procedure RemoveReloadEventListener(FunctionToCall: TFileSourceReloadEventNotify);

    property CurrentAddress: String read GetCurrentAddress;
    property Properties: TFileSourceProperties read GetProperties;
    property SupportedFileProperties: TFilePropertiesTypes read GetSupportedFileProperties;
  end;

  { TFileSource }

  TFileSource = class(TInterfacedObject, IFileSource)

  private
    FReloadEventListeners: TMethodList;

    {en
       Callback called when an operation assigned to a connection finishes.
       It just redirects to a virtual function.
    }
    procedure OperationFinishedCallback(Operation: TFileSourceOperation;
                                        State: TFileSourceOperationState);

  protected
    FCurrentAddress: String;

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

    {en
       Checks if the connection is available and, if it is, assigns it to the operation.
       @returns(Connection object if the connection is available,
                @nil otherwise.)
    }
    function TryAcquireConnection(connection: TFileSourceConnection;
                                  operation: TFileSourceOperation): TFileSourceConnection; virtual;

    procedure OperationFinished(Operation: TFileSourceOperation); virtual;

    {en
       Reloads any internal file lists/caches.
       @param(PathsToReload
              Describes paths in file source from which file lists should be reloaded.
              The function may also reload any subpaths, though that is
              dependent on the specific file source implementation.)
    }
    procedure DoReload(const PathsToReload: TPathsArray); virtual;

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
    function CreateFiles(const APath: String): TFiles; virtual;

    // These functions create an operation object specific to the file source.
    function CreateListOperation(TargetPath: String): TFileSourceOperation; virtual;
    function CreateCopyOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation; virtual;
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
    function CreateExecuteOperation(const ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; virtual;
    function CreateTestArchiveOperation(var theSourceFiles: TFiles): TFileSourceOperation; virtual;
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
    function GetRootDir(sPath : String): String; virtual; overload;
    function GetRootDir: String; virtual; overload;
    function GetPathType(sPath : String): TPathType; virtual;

{
    class function ClassGetParentDir(sPath : String): String;
    class function ClassGetRootDir(sPath : String): String;
    class function ClassGetPathType(sPath : String): TPathType;
}

    function GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean; virtual;
    function GetLocalName(var aFile: TFile): Boolean; virtual;

    function GetConnection(Operation: TFileSourceOperation): TFileSourceConnection; virtual;

    {en
       This function is to ensure the operation does not stay in the queue
       when it's being destroyed.
    }
    procedure RemoveOperationFromQueue(Operation: TFileSourceOperation); virtual;

    {en
       Reloads the file list from the file source.
       This is used if a file source has any internal cache or file list.
       Overwrite DoReload in descendant classes.
    }
    procedure Reload(const PathsToReload: TPathsArray); virtual;
    procedure Reload(const PathToReload: String);

    procedure AddReloadEventListener(FunctionToCall: TFileSourceReloadEventNotify);
    procedure RemoveReloadEventListener(FunctionToCall: TFileSourceReloadEventNotify);

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

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function IsAvailable: Boolean;
    function Acquire(Operation: TFileSourceOperation): Boolean;
    procedure Release;

    property CurrentPath: String read GetCurrentPath write SetCurrentPath;
    property AssignedOperation: TFileSourceOperation read GetAssignedOperation;
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
  uFileSourceListOperation;

{ TFileSource }

constructor TFileSource.Create;
begin
  if ClassType = TFileSource then
    raise Exception.Create('Cannot construct abstract class');
  inherited Create;

  FReloadEventListeners := TMethodList.Create;

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

  FreeThenNil(FReloadEventListeners);

  inherited Destroy;
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
  Result := (Path = GetRootDir(Path));
end;

function TFileSource.GetParentDir(sPath : String): String;
begin
  Result := uDCUtils.GetParentDir(sPath);
end;

function TFileSource.GetRootDir(sPath : String): String;
begin
  // Default root is '/'. Override in descendant classes for other.
  Result := PathDelim;
end;

function TFileSource.GetRootDir: String;
begin
  Result := GetRootDir('');
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

function TFileSource.GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean;
begin
  Result := False; // not supported by default
end;

function TFileSource.GetLocalName(var aFile: TFile): Boolean;
begin
  Result:= False;
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

function TFileSource.CreateFiles(const APath: String): TFiles;
begin
  Result := TFiles.Create(APath);
end;

function TFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateCopyOperation(var SourceFiles: TFiles;
                                         TargetPath: String): TFileSourceOperation;
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

function TFileSource.CreateExecuteOperation(const ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateTestArchiveOperation(var theSourceFiles: TFiles): TFileSourceOperation;
begin
  Result:= nil;
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
  // By default connections are not supported.
  Result := nil;
end;

function TFileSource.TryAcquireConnection(connection: TFileSourceConnection;
                                          operation: TFileSourceOperation): TFileSourceConnection;
begin
  if connection.Acquire(operation) then
  begin
    // We must know when the operation is finished,
    // that is when the connection is free again.
    operation.AddStateChangedListener([fsosStopped], @OperationFinishedCallback);
    Result := connection;
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TFileSource.RemoveOperationFromQueue(Operation: TFileSourceOperation);
begin
  // Nothing by default.
end;

procedure TFileSource.OperationFinishedCallback(Operation: TFileSourceOperation;
                                                State: TFileSourceOperationState);
begin
  if State = fsosStopped then
  begin
    Operation.RemoveStateChangedListener([fsosStopped], @OperationFinishedCallback);
    OperationFinished(Operation);
  end;
end;

procedure TFileSource.OperationFinished(Operation: TFileSourceOperation);
begin
  // Nothing by default.
end;

procedure TFileSource.DoReload(const PathsToReload: TPathsArray);
begin
  // Nothing by default.
end;

procedure TFileSource.Reload(const PathsToReload: TPathsArray);
var
  i: Integer;
  FunctionToCall: TFileSourceReloadEventNotify;
begin
  DoReload(PathsToReload);

  if Assigned(FReloadEventListeners) then
    for i := 0 to FReloadEventListeners.Count - 1 do
    begin
      FunctionToCall := TFileSourceReloadEventNotify(FReloadEventListeners.Items[i]);
      FunctionToCall(Self, PathsToReload);
    end;
end;

procedure TFileSource.Reload(const PathToReload: String);
var
  PathsToReload: TPathsArray;
begin
  SetLength(PathsToReload, 1);
  PathsToReload[0] := PathToReload;
  Reload(PathsToReload);
end;

procedure TFileSource.AddReloadEventListener(FunctionToCall: TFileSourceReloadEventNotify);
begin
  FReloadEventListeners.Add(TMethod(FunctionToCall));
end;

procedure TFileSource.RemoveReloadEventListener(FunctionToCall: TFileSourceReloadEventNotify);
begin
  FReloadEventListeners.Remove(TMethod(FunctionToCall));
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

