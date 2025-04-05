unit uFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCStrUtils, syncobjs, LCLProc, URIParser, Menus,
  uFile, uDisplayFile, uFileProperty,
  uFileSourceWatcher,
  uFileSourceOperation, uFileSourceOperationTypes, uFileSourceProperty;

type

  TFileSource = class;
  TFileSourceConnection = class;
  IFileSource = interface;

  TFileSourceConsultResult = ( fscrSuccess, fscrNotImplemented, fscrNotSupported, fscrCancel );

  {$scopedEnums on}
  TFileSourceConsultPhase = ( source, target );

  TFileSourceConsultParams = Record
    handled: Boolean;
    phase: TFileSourceConsultPhase;

    operationType: TFileSourceOperationType;
    files: TFiles;
    targetPath: String;

    sourceFS: IFileSource;
    targetFS: IFileSource;

    currentFS: IFileSource;
    partnerFS: IFileSource;

    consultResult: TFileSourceConsultResult;
    resultOperationType: TFileSourceOperationType;
    resultFS: IFileSource;
    resultTargetPath: String;
    operationTemp: Boolean;
  end;

  TFileSourceProcessor = class
    procedure consultOperation( var params: TFileSourceConsultParams ); virtual; abstract;
    procedure confirmOperation( var params: TFileSourceConsultParams ); virtual; abstract;
  end;

  TFileSourceUIParams = record
    sender: TObject;
    fs: IFileSource;
    displayFile: TDisplayFile;

    multiColumns: Boolean;   // True for ColumnsView, False for BriefView
    col: Integer;
    row: Integer;
    drawingRect: TRect;

    case Byte of
      0: (
           iconRect: TRect;
           focused: Boolean
         );
      1: (
           shift: TShiftState;
           x: Integer;
           y: Integer
         );
  end;

  TFileSourceUIHandler = class
    procedure draw( var params: TFileSourceUIParams ); virtual; abstract;
    procedure click( var  params: TFileSourceUIParams ); virtual; abstract;
  end;

  TFileSourceField = record
    Content: String;
    Header: String;
    Width: Integer;
    Option: String;
    Align: TAlignment;
  end;

  TFileSourceFields = array of TFileSourceField;

  TFileSourceOperationsClasses = array[TFileSourceOperationType] of TFileSourceOperationClass;

  TPathsArray = array of string;

  {$scopedEnums on}
  TFileSourceEventType = ( reload, relocation, queryActive );

  TFileSourceEventParams = record
    eventType: TFileSourceEventType;
    fs: IFileSource;

    // reload input
    paths: TPathsArray;

    // relocation input
    newPath: String;

    // queryActive output
    resultDisplayFile: TDisplayFile;
  end;

  TFileSourceEventListener = procedure(var params: TFileSourceEventParams) of object;

  { IFileSource }

  IFileSource = interface(IInterface)
    ['{B7F0C4C8-59F6-4A35-A54C-E8242F4AD809}']

    function GetWatcher: TFileSourceWatcher;
    function GetProcessor: TFileSourceProcessor;
    function GetUIHandler: TFileSourceUIHandler;

    function Equals(aFileSource: IFileSource): Boolean;
    function IsInterface(InterfaceGuid: TGuid): Boolean;
    function IsClass(ClassType: TClass): Boolean;
    function GetClass: TFileSource;
    function GetURI: TURI;
    function GetClassName: String;
    function GetRefCount: Integer;

    function GetFileSystem: String;
    function GetCurrentAddress: String;
    function GetCurrentWorkingDirectory: String;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean;

    function GetSupportedFileProperties: TFilePropertiesTypes;
    function GetRetrievableFileProperties: TFilePropertiesTypes;
    function GetOperationsTypes: TFileSourceOperationTypes;
    function GetProperties: TFileSourceProperties;
    function GetFiles(TargetPath: String): TFiles;
    function GetParentFileSource: IFileSource;
    procedure SetParentFileSource(NewValue: IFileSource);

    function CreateFileObject(const APath: String): TFile;

    function CanRetrieveProperties(AFile: TFile; PropertiesToSet: TFilePropertiesTypes): Boolean;
    procedure RetrieveProperties(AFile: TFile; PropertiesToSet: TFilePropertiesTypes; const AVariantProperties: array of String);

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
    function CreateSplitOperation(var aSourceFile: TFile;
                                    aTargetPath: String): TFileSourceOperation;
    function CreateCombineOperation(var theSourceFiles: TFiles;
                                    aTargetFile: String): TFileSourceOperation;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation;
    function CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation;
    function CreateTestArchiveOperation(var theSourceFiles: TFiles): TFileSourceOperation;
    function CreateCalcChecksumOperation(var theFiles: TFiles;
                                         aTargetPath: String;
                                         aTargetMask: String): TFileSourceOperation;
    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation;
    function GetOperationClass(OperationType: TFileSourceOperationType): TFileSourceOperationClass;

    function IsSystemFile(aFile: TFile): Boolean;
    function IsHiddenFile(aFile: TFile): Boolean;
    function GetFileName(aFile: TFile): String;
    function GetDisplayFileName(aFile: TFile): String;

    function IsPathAtRoot(Path: String): Boolean;
    function GetParentDir(sPath : String): String;
    function GetRootDir(sPath : String): String; overload;
    function GetRootDir: String; overload;
    function GetPathType(sPath : String): TPathType;
    function GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean;
    function GetRealPath(const path: String): String;
    function GetLocalName(var aFile: TFile): Boolean;

    function CreateDirectory(const Path: String): Boolean;
    function FileSystemEntryExists(const Path: String): Boolean;
    function GetDefaultView(out DefaultView: TFileSourceFields): Boolean;
    function QueryContextMenu(AFiles: TFiles; var AMenu: TPopupMenu): Boolean;

    function GetConnection(Operation: TFileSourceOperation): TFileSourceConnection;
    procedure RemoveOperationFromQueue(Operation: TFileSourceOperation);

    procedure AddChild(AFileSource: IFileSource);
    procedure eventNotify( var params: TFileSourceEventParams );
    procedure Reload(const PathsToReload: TPathsArray);
    procedure Reload(const PathToReload: String);
    procedure AddEventListener(FunctionToCall: TFileSourceEventListener);
    procedure RemoveEventListener(FunctionToCall: TFileSourceEventListener);

    property URI: TURI read GetURI;
    property ClassName: String read GetClassName;
    property FileSystem: String read GetFileSystem;
    property CurrentAddress: String read GetCurrentAddress;
    property ParentFileSource: IFileSource read GetParentFileSource write SetParentFileSource;
    property Properties: TFileSourceProperties read GetProperties;
    property SupportedFileProperties: TFilePropertiesTypes read GetSupportedFileProperties;
    property RetrievableFileProperties: TFilePropertiesTypes read GetRetrievableFileProperties;
  end;

  { TFileSource }

  TFileSource = class(TObject, IFileSource)
    RefCount: Integer;
    function QueryInterface(constref iid: TGuid; out obj): Integer;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: Integer;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};

  private
    FEventListeners: TMethodList;
    {en
       File source on which this file source is dependent on
       (files that it accesses are on the parent file source).
    }
    FParentFileSource: IFileSource;

    {en
       Callback called when an operation assigned to a connection finishes.
       It just redirects to a virtual function.
    }
    procedure OperationFinishedCallback(Operation: TFileSourceOperation;
                                        State: TFileSourceOperationState);

  protected
    FURI: TURI;
    FCurrentAddress: String;
    FOperationsClasses: TFileSourceOperationsClasses;
    {en
       Children file source list
    }
    FChildrenFileSource: TInterfaceList;

    function GetURI: TURI;
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
    function GetSupportedFileProperties: TFilePropertiesTypes; virtual;
    {en
       Returns all the file properties that can be retrieved by the file source.
    }
    function GetRetrievableFileProperties: TFilePropertiesTypes; virtual;

    function GetParentFileSource: IFileSource;
    procedure SetParentFileSource(NewValue: IFileSource);

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

    function CreateFileObject(const APath: String): TFile;

  public
    constructor Create; virtual; overload;
    constructor Create(const URI: TURI); virtual; overload;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;

    function GetWatcher: TFileSourceWatcher; virtual;
    function GetProcessor: TFileSourceProcessor; virtual;
    function GetUIHandler: TFileSourceUIHandler; virtual;

    function Equals(aFileSource: IFileSource): Boolean; overload;
    function IsInterface(InterfaceGuid: TGuid): Boolean;
    function IsClass(aClassType: TClass): Boolean;
    function GetClass: TFileSource;
    function GetClassName: String; // For debugging purposes.
    function GetRefCount: Integer; // For debugging purposes.

    // Retrieve operations permitted on the source.  = capabilities?
    function GetOperationsTypes: TFileSourceOperationTypes; virtual;

    // Retrieve some properties of the file source.
    function GetProperties: TFileSourceProperties; virtual;

    // Retrieves a list of files.
    // This is the same as GetOperation(fsoList), executing it
    // and returning the result of Operation.ReleaseFiles.
    // Caller is responsible for freeing the result list.
    function GetFiles(TargetPath: String): TFiles; virtual;

    // Create an empty TFile object with appropriate properties for the file.
    class function CreateFile(const APath: String): TFile; virtual;

    procedure RetrieveProperties(AFile: TFile; PropertiesToSet: TFilePropertiesTypes; const AVariantProperties: array of String); virtual;
    function CanRetrieveProperties(AFile: TFile; PropertiesToSet: TFilePropertiesTypes): Boolean; virtual;

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
    function CreateSplitOperation(var aSourceFile: TFile;
                                    aTargetPath: String): TFileSourceOperation; virtual;
    function CreateCombineOperation(var theSourceFiles: TFiles;
                                    aTargetFile: String): TFileSourceOperation; virtual;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; virtual;
    function CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; virtual;
    function CreateTestArchiveOperation(var theSourceFiles: TFiles): TFileSourceOperation; virtual;
    function CreateCalcChecksumOperation(var theFiles: TFiles;
                                         aTargetPath: String;
                                         aTargetMask: String): TFileSourceOperation; virtual;
    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation; virtual;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation; virtual;
    function GetOperationClass(OperationType: TFileSourceOperationType): TFileSourceOperationClass;

    class function GetMainIcon(out Path: String): Boolean; virtual;
    {en
       Returns @true if the given path is supported by the file source,
       @false otherwise.
    }
    class function IsSupportedPath(const Path: String): Boolean; virtual;
    {en
       Returns @true if the given path is the root path of the file source,
       @false otherwise.
    }

    function IsSystemFile(aFile: TFile): Boolean; virtual;
    function IsHiddenFile(aFile: TFile): Boolean; virtual;
    function GetDisplayFileName(aFile: TFile): String; virtual;
    function GetFileName(aFile: TFile): String; virtual;

    function IsPathAtRoot(Path: String): Boolean; virtual;

    function GetParentDir(sPath : String): String; virtual;
    function GetRootDir(sPath : String): String; virtual; overload;
    function GetRootDir: String; virtual; overload;
    function GetPathType(sPath : String): TPathType; virtual;
    function GetFileSystem: String; virtual;

    function CreateDirectory(const Path: String): Boolean; virtual;
    function FileSystemEntryExists(const Path: String): Boolean; virtual;
    function GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean; virtual;
    function QueryContextMenu(AFiles: TFiles; var AMenu: TPopupMenu): Boolean; virtual;
    function GetDefaultView(out DefaultView: TFileSourceFields): Boolean; virtual;
    function GetRealPath(const path: String): String; virtual;
    function GetLocalName(var aFile: TFile): Boolean; virtual;

    function GetConnection(Operation: TFileSourceOperation): TFileSourceConnection; virtual;

    {en
       This function is to ensure the operation does not stay in the queue
       when it's being destroyed.
    }
    procedure RemoveOperationFromQueue(Operation: TFileSourceOperation); virtual;

    procedure AddChild(AFileSource: IFileSource);
    {en
       Reloads the file list from the file source.
       This is used if a file source has any internal cache or file list.
       Overwrite DoReload in descendant classes.
    }
    procedure eventNotify( var params: TFileSourceEventParams );
    procedure Reload(const PathsToReload: TPathsArray); virtual; overload;
    procedure Reload(const PathToReload: String); overload;

    procedure AddEventListener(FunctionToCall: TFileSourceEventListener);
    procedure RemoveEventListener(FunctionToCall: TFileSourceEventListener);

    property CurrentAddress: String read GetCurrentAddress;
    property ParentFileSource: IFileSource read GetParentFileSource write SetParentFileSource;
    property Properties: TFileSourceProperties read GetProperties;
    property SupportedFileProperties: TFilePropertiesTypes read GetSupportedFileProperties;
    property RetrievableFileProperties: TFilePropertiesTypes read GetRetrievableFileProperties;

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

  EFileSourceException = class(Exception);
  EFileNotFound = class(EFileSourceException)
  private
    FFilePath: String;
  public
    constructor Create(const AFilePath: string); reintroduce;
    property FilePath: String read FFilePath;
  end;

var
  defaultFileSourceProcessor: TFileSourceProcessor;

implementation

uses
  uDebug, uFileSourceManager, uFileSourceListOperation, uLng;

var
  defaultFileSourceWatcher: TFileSourceWatcher;

{ TFileSource }

constructor TFileSource.Create;
begin
  if ClassType = TFileSource then
    raise Exception.Create('Cannot construct abstract class');
  inherited Create;

  FEventListeners := TMethodList.Create;

  DCDebug('Creating ', ClassName);
end;

constructor TFileSource.Create(const URI: TURI);
var
  AddressURI: TURI;
begin
  Create;
  FURI:= URI;
  FillChar(AddressURI, SizeOf(TURI), 0);
  AddressURI.Protocol:= FURI.Protocol;
  AddressURI.Username:= FURI.Username;
  AddressURI.Host:= FURI.Host;
  AddressURI.Port:= FURI.Port;
  AddressURI.HasAuthority:= FURI.HasAuthority;
  FCurrentAddress:= EncodeURI(AddressURI);
end;

procedure TFileSource.AfterConstruction;
begin
  inherited AfterConstruction;

  if RefCount <> 0 then
    DCDebug('Error: AfterConstruction ', ClassName, ' when refcount=', DbgS(refcount));

  EnterCriticalSection(AllFileSourceObjectsCriticalSection);
  try
    AllFileSourceObjects.Add(Self);
  finally
    LeaveCriticalSection(AllFileSourceObjectsCriticalSection);
  end;
end;

procedure TFileSource.BeforeDestruction;
begin
  if RefCount <> 0 then
    DCDebug('Error: BeforeDestruction ', ClassName, ' when refcount=', DbgS(refcount));

  EnterCriticalSection(AllFileSourceObjectsCriticalSection);
  try
    AllFileSourceObjects.Remove(Self);
  finally
    LeaveCriticalSection(AllFileSourceObjectsCriticalSection);
  end;

  inherited BeforeDestruction;
end;

destructor TFileSource.Destroy;
begin
  DCDebug('Destroying ', ClassName, ' when refcount=', DbgS(refcount));

  if RefCount <> 0 then
    DCDebug('Error: RefCount <> 0 for ', ClassName);

  FreeAndNil(FChildrenFileSource);
  FreeAndNil(FEventListeners);

  inherited Destroy;
end;

function TFileSource.QueryInterface(constref iid: TGuid; out obj): Integer;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(iid, obj) then
    Result:= S_OK
  else
    Result:= E_NOINTERFACE;
end;

function TFileSource._AddRef: Integer;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  EnterCriticalSection(AllFileSourceObjectsCriticalSection);
  try
    Result:= RefCount+1;
    RefCount:= Result;
  finally
    LeaveCriticalSection(AllFileSourceObjectsCriticalSection);
  end;
end;

function TFileSource._Release: Integer;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  EnterCriticalSection(AllFileSourceObjectsCriticalSection);
  try
    Result:= RefCount-1;
    RefCount:= Result;
    // don't destroy if ...
    if// there are still references or ...
      (Result<>0) or
      // during construction or destruction (self is not in AllFileSourceObjects)
      (AllFileSourceObjects.IndexOf(Self)<0) then Exit;
  finally
    LeaveCriticalSection(AllFileSourceObjectsCriticalSection);
  end;
  Destroy;
end;

function TFileSource.GetWatcher: TFileSourceWatcher;
begin
  Result:= defaultFileSourceWatcher;
end;

function TFileSource.GetProcessor: TFileSourceProcessor;
begin
  Result:= defaultFileSourceProcessor;
end;

function TFileSource.GetUIHandler: TFileSourceUIHandler;
begin
  Result:= nil;
end;

function TFileSource.Equals(aFileSource: IFileSource): Boolean;
begin
  // Both interface variables must be brought to the same interface.
  Result := (Self as IFileSource) = (aFileSource as IFileSource);
end;

function TFileSource.IsInterface(InterfaceGuid: TGuid): Boolean;
var
  t: Pointer;
begin
  Result := GetInterfaceWeak(InterfaceGuid, t);
end;

function TFileSource.IsClass(aClassType: TClass): Boolean;
begin
  Result := Self is aClassType;
end;

function TFileSource.GetClass: TFileSource;
begin
  Result := Self
end;

function TFileSource.GetClassName: String;
begin
  Result := ClassName;
end;

function TFileSource.GetRefCount: Integer;
begin
  Result := RefCount;
end;

function TFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [];
end;

function TFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [];
end;

function TFileSource.GetURI: TURI;
begin
  Result := FURI;
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

function TFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := [fpName];
end;

function TFileSource.GetRetrievableFileProperties: TFilePropertiesTypes;
begin
  Result := [];
end;

function TFileSource.GetParentFileSource: IFileSource;
begin
  Result := FParentFileSource;
end;

procedure TFileSource.SetParentFileSource(NewValue: IFileSource);
begin
  FParentFileSource := NewValue;
end;

function TFileSource.IsPathAtRoot(Path: String): Boolean;
begin
  Result := (Path = GetRootDir(Path));
end;

function TFileSource.GetParentDir(sPath : String): String;
begin
  Result := DCStrUtils.GetParentDir(sPath);
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

function TFileSource.GetFileSystem: String;
begin
  Result:= EmptyStr;
end;

function TFileSource.CreateDirectory(const Path: String): Boolean;
begin
  Result := False;
end;

function TFileSource.FileSystemEntryExists(const Path: String): Boolean;
begin
  Result := True;
end;

function TFileSource.GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean;
begin
  Result := False; // not supported by default
end;

function TFileSource.QueryContextMenu(AFiles: TFiles; var AMenu: TPopupMenu): Boolean;
begin
  Result:= False;
end;

function TFileSource.GetDefaultView(out DefaultView: TFileSourceFields): Boolean;
begin
  Result:= False;
end;

function TFileSource.GetRealPath(const path: String): String;
begin
  Result:= path;
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

class function TFileSource.CreateFile(const APath: String): TFile;
begin
  Result := TFile.Create(APath);
end;

function TFileSource.CreateFileObject(const APath: String): TFile;
begin
  Result := CreateFile(APath);
end;

procedure TFileSource.RetrieveProperties(AFile: TFile; PropertiesToSet: TFilePropertiesTypes; const AVariantProperties: array of String);
begin
  // Does not set any properties by default.
end;

function TFileSource.CanRetrieveProperties(AFile: TFile; PropertiesToSet: TFilePropertiesTypes): Boolean;
begin
  Result := ((PropertiesToSet - AFile.AssignedProperties) * RetrievableFileProperties) <> [];
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

function TFileSource.CreateSplitOperation(var aSourceFile: TFile;
                                          aTargetPath: String): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateCombineOperation(var theSourceFiles: TFiles;
                                            aTargetFile: String): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation;
begin
  Result := nil;
end;

function TFileSource.CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation;
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

function TFileSource.GetOperationClass(OperationType: TFileSourceOperationType): TFileSourceOperationClass;
begin
  Result := FOperationsClasses[OperationType];
end;

class function TFileSource.GetMainIcon(out Path: String): Boolean;
begin
  Result := False;
end;

class function TFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= True;
end;

function TFileSource.IsSystemFile(aFile: TFile): Boolean;
begin
{$IF DEFINED(MSWINDOWS)}
  if fpAttributes in aFile.SupportedProperties then
    Result := TFileAttributesProperty(aFile.Properties[fpAttributes]).IsSysFile
  else
    Result := False;
{$ELSEIF DEFINED(DARWIN)}
  if (Length(aFile.Name) > 1) and (aFile.Name[1] = '.') and (aFile.Name <> '..') then exit(true);
  if aFile.Name='Icon'#$0D then exit(true);
  exit(false);
{$ELSE}
  // Files beginning with '.' are treated as system/hidden files on Unix.
  Result := (Length(aFile.Name) > 1) and (aFile.Name[1] = '.') and (aFile.Name <> '..');
{$ENDIF}
end;

function TFileSource.IsHiddenFile(aFile: TFile): Boolean;
begin
  if not (fpAttributes in aFile.SupportedProperties) then
    Result := False
  else begin
    if aFile.Properties[fpAttributes] is TNtfsFileAttributesProperty then
      Result := TNtfsFileAttributesProperty(aFile.Properties[fpAttributes]).IsHidden
    else begin
      // Files beginning with '.' are treated as system/hidden files on Unix.
      Result := (Length(aFile.Name) > 1) and (aFile.Name[1] = '.') and (aFile.Name <> '..');
    end;
  end;
end;

function TFileSource.GetDisplayFileName(aFile: TFile): String;
begin
  Result:= EmptyStr;
end;

function TFileSource.GetFileName(aFile: TFile): String;
begin
  Result:= aFile.Name;
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

procedure TFileSource.AddChild(AFileSource: IFileSource);
begin
  if (FChildrenFileSource = nil) then
  begin
    FChildrenFileSource:= TInterfaceList.Create;
  end
  else if FChildrenFileSource.Count > 32 then
  begin
    FChildrenFileSource.Delete(0);
  end;
  FChildrenFileSource.Add(AFileSource);
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

procedure TFileSource.eventNotify( var params: TFileSourceEventParams );
var
  i: Integer;
  FunctionToCall: TFileSourceEventListener;
begin
  if FEventListeners = nil then
    Exit;

  for i := 0 to FEventListeners.Count - 1 do begin
    FunctionToCall:= TFileSourceEventListener(FEventListeners.Items[i]);
    FunctionToCall( params );
  end;
end;

procedure TFileSource.Reload(const PathsToReload: TPathsArray);
var
  params: TFileSourceEventParams;
begin
  DoReload(PathsToReload);

  params.fs:= Self;
  params.eventType:= TFileSourceEventType.reload;
  params.paths:= PathsToReload;
  eventNotify( params );
end;

procedure TFileSource.Reload(const PathToReload: String);
var
  PathsToReload: TPathsArray;
begin
  SetLength(PathsToReload, 1);
  PathsToReload[0] := PathToReload;
  Reload(PathsToReload);
end;

procedure TFileSource.AddEventListener(FunctionToCall: TFileSourceEventListener);
begin
  FEventListeners.Add(TMethod(FunctionToCall));
end;

procedure TFileSource.RemoveEventListener(FunctionToCall: TFileSourceEventListener);
begin
  FEventListeners.Remove(TMethod(FunctionToCall));
end;

{ TFileSourceConnection }

constructor TFileSourceConnection.Create;
begin
  FOperationLock := TCriticalSection.Create;
  inherited Create;
  DCDebug('Creating connection ', ClassName);
end;

destructor TFileSourceConnection.Destroy;
begin
  if Assigned(FAssignedOperation) and (FAssignedOperation.State <> fsosStopped) then
    DCDebug('Error: Destroying connection ', ClassName, ' with active operation ', FAssignedOperation.ClassName);

  inherited Destroy;

  DCDebug('Destroying connection ', ClassName);

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

constructor EFileNotFound.Create(const AFilePath: string);
begin
  FFilePath := AFilePath;
  inherited Create(Format(rsMsgFileNotFound, [aFilePath]));
end;

initialization
  defaultFileSourceWatcher:= TDefaultFileSourceWatcher.Create;

finalization
  FreeAndNil( defaultFileSourceWatcher );

end.

