unit WinRT.Classes;

{$mode delphi}

interface

uses
  Classes, SysUtils, Types, WinRT.Core;

const
  Windows_System_Launcher = UnicodeString('Windows.System.Launcher');
  Windows_Storage_StorageFile = UnicodeString('Windows.Storage.StorageFile');
  Windows_Storage_StorageFolder = UnicodeString('Windows.Storage.StorageFolder');
  Windows_System_LauncherOptions = UnicodeString('Windows.System.LauncherOptions');

type
  // Windows.Foundation.AsyncStatus
  AsyncStatus = (
    Started = 0,
    Completed = 1,
    Canceled = 2,
    Error = 3
  );
  // Windows.Storage.Search.FolderDepth
  FolderDepth =
  (
    Shallow = 0,
    Deep    = 1
  );
  // Windows.Storage.Search.IndexerOption
  IndexerOption =
  (
    UseIndexerWhenAvailable                       = 0,
    OnlyUseIndexer                                = 1,
    DoNotUseIndexer                               = 2,
    OnlyUseIndexerAndOptimizeForIndexedProperties = 3
  );
  // Windows.Storage.Search.DateStackOption
  DateStackOption =
  (
    None  = 0,
    Year  = 1,
    Month = 2
  );
  // Windows.Storage.Search.CommonFileQuery
  CommonFileQuery =
  (
    DefaultQuery           = 0,
    OrderByName            = 1,
    OrderByTitle           = 2,
    OrderByMusicProperties = 3,
    OrderBySearchRank      = 4,
    OrderByDate            = 5
  );
  // Windows.Storage.FileAccessMode
  FileAccessMode = (
    Read = 0,
    ReadWrite = 1
  );
  // Windows.Storage.NameCollisionOption
  NameCollisionOption = (
    GenerateUniqueName = 0,
    ReplaceExisting = 1,
    FailIfExists = 2
  );
  // Windows.Storage.CreationCollisionOption
  CreationCollisionOption = (
    GenerateUniqueNamez = 0,
    ReplaceExistingz = 1,
    FailIfExistsz = 2,
    OpenIfExists = 3
  );

  // Forward declarations
  IStorageFolder = interface;
  IAsyncOperation_1_Boolean = interface;
  IAsyncOperation_1_IStorageFile = interface;
  IAsyncOperation_1_IStorageFolder = interface;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.StorageFolder>
  IAsyncOperationCompletedHandler_1_IStorageFolder = interface(IUnknown)
    ['{C211026E-9E63-5452-BA54-3A07D6A96874}']
    procedure Invoke(asyncInfo: IAsyncOperation_1_IStorageFolder; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageFolder>
  IAsyncOperation_1_IStorageFolder = interface(IInspectable)
    ['{6BE9E7D7-E83A-5CBC-802C-1768960B52C3}']
    procedure Set_Completed(handler: IAsyncOperationCompletedHandler_1_IStorageFolder); safecall;
    function Get_Completed: IAsyncOperationCompletedHandler_1_IStorageFolder; safecall;
    function GetResults: IStorageFolder; safecall;
    property Completed: IAsyncOperationCompletedHandler_1_IStorageFolder read Get_Completed write Set_Completed;
  end;

  IStorageFolder = interface(IInspectable)
    ['{72D1CB78-B3EF-4F75-A80B-6FD9DAE2944B}']
    function CreateFileAsync(desiredName: HSTRING): IAsyncOperation_1_IStorageFile; safecall; overload;
    function CreateFileAsync(desiredName: HSTRING; options: CreationCollisionOption): IAsyncOperation_1_IStorageFile; safecall; overload;
    function CreateFolderAsync(desiredName: HSTRING): IAsyncOperation_1_IStorageFolder; safecall; overload;
    function CreateFolderAsync(desiredName: HSTRING; options: CreationCollisionOption): IAsyncOperation_1_IStorageFolder; safecall; overload;
    function GetFileAsync(name: HSTRING): IAsyncOperation_1_IStorageFile; safecall;
    function GetFolderAsync(name: HSTRING): IAsyncOperation_1_IStorageFolder; safecall;
    function GetItemAsync(name: HSTRING): IInspectable; safecall;
    function GetFilesAsync: IInspectable; safecall;
    function GetFoldersAsync: IInspectable; safecall;
    function GetItemsAsync: IInspectable; safecall;
  end;

  IStorageFile = interface(IInspectable)
    ['{FA3F6186-4214-428C-A64C-14C9AC7315EA}']
    function Get_FileType: HSTRING; safecall;
    function Get_ContentType: HSTRING; safecall;
    function OpenAsync(accessMode: FileAccessMode): IInspectable; safecall;
    function OpenTransactedWriteAsync: IInspectable; safecall;
    function CopyAsync(destinationFolder: IStorageFolder): IAsyncOperation_1_IStorageFile; safecall; overload;
    function CopyAsync(destinationFolder: IStorageFolder; desiredNewName: HSTRING): IAsyncOperation_1_IStorageFile; safecall; overload;
    function CopyAsync(destinationFolder: IStorageFolder; desiredNewName: HSTRING; option: NameCollisionOption): IAsyncOperation_1_IStorageFile; safecall; overload;
    function CopyAndReplaceAsync(fileToReplace: IStorageFile): IInspectable; safecall;
    function MoveAsync(destinationFolder: IStorageFolder): IInspectable; safecall; overload;
    function MoveAsync(destinationFolder: IStorageFolder; desiredNewName: HSTRING): IInspectable; safecall; overload;
    function MoveAsync(destinationFolder: IStorageFolder; desiredNewName: HSTRING; option: NameCollisionOption): IInspectable; safecall; overload;
    function MoveAndReplaceAsync(fileToReplace: IStorageFile): IInspectable; safecall;
    property ContentType: HSTRING read Get_ContentType;
    property FileType: HSTRING read Get_FileType;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageFile>
  IAsyncOperationCompletedHandler_1_IStorageFile = interface(IUnknown)
    ['{E521C894-2C26-5946-9E61-2B5E188D01ED}']
    procedure Invoke(asyncInfo: IAsyncOperation_1_IStorageFile; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageFile>
  IAsyncOperation_1_IStorageFile = interface(IInspectable)
    ['{5E52F8CE-ACED-5A42-95B4-F674DD84885E}']
    procedure Set_Completed(handler: IAsyncOperationCompletedHandler_1_IStorageFile); safecall;
    function Get_Completed: IAsyncOperationCompletedHandler_1_IStorageFile; safecall;
    function GetResults: IStorageFile; safecall;
    property Completed: IAsyncOperationCompletedHandler_1_IStorageFile read Get_Completed write Set_Completed;
  end;

  // Windows.Storage.StorageFile
  IStorageFileStatics = interface(IInspectable)
    ['{5984C710-DAF2-43C8-8BB4-A4D3EACFD03F}']
    function GetFileFromPathAsync(path: HSTRING): IAsyncOperation_1_IStorageFile; safecall;
    function GetFileFromApplicationUriAsync(uri: IInspectable): IAsyncOperation_1_IStorageFile; safecall;
    function CreateStreamedFileAsync(displayNameWithExtension: HSTRING; dataRequested: IUnknown; thumbnail: IInspectable): IAsyncOperation_1_IStorageFile; safecall;
    function ReplaceWithStreamedFileAsync(fileToReplace: IStorageFile; dataRequested: IUnknown; thumbnail: IInspectable): IAsyncOperation_1_IStorageFile; safecall;
    function CreateStreamedFileFromUriAsync(displayNameWithExtension: HSTRING; uri: IInspectable; thumbnail: IInspectable): IAsyncOperation_1_IStorageFile; safecall;
    function ReplaceWithStreamedFileFromUriAsync(fileToReplace: IStorageFile; uri: IInspectable; thumbnail: IInspectable): IAsyncOperation_1_IStorageFile; safecall;
  end;

  // Windows.Storage.Search.StorageFileQueryResult
  IStorageFileQueryResult = interface(IInspectable)
    ['{52FDA447-2BAA-412C-B29F-D4B1778EFA1E}']
    function GetFilesAsync(startIndex: UINT32; maxNumberOfItems: UINT32; out operation: IInspectable): HRESULT; stdcall;
    function GetFilesAsyncDefaultStartAndCount(out operation: IInspectable): HRESULT; stdcall;
  end;

  // Windows.Storage.Search.QueryOptions
  IQueryOptions = interface(IInspectable)
    ['{1E5E46EE-0F45-4838-A8E9-D0479D446C30}']
    function Get_FileTypeFilter(out value: IUnknown): HRESULT; stdcall;
    function Get_FolderDepth(out value: FolderDepth): HRESULT; stdcall;
    function Put_FolderDepth(value: FolderDepth): HRESULT; stdcall;
    function Get_ApplicationSearchFilter(out value: HSTRING): HRESULT; stdcall;
    function Put_ApplicationSearchFilter(value: HSTRING): HRESULT; stdcall;
    function Get_UserSearchFilter(out value: HSTRING): HRESULT; stdcall;
    function Put_UserSearchFilter(value: HSTRING): HRESULT; stdcall;
    function Get_Language(out value: HSTRING): HRESULT; stdcall;
    function Put_Language(value: HSTRING): HRESULT; stdcall;
    function Get_IndexerOption(out value: IndexerOption): HRESULT; stdcall;
    function Put_IndexerOption(value: IndexerOption): HRESULT; stdcall;
    function Get_SortOrder(out value: IUnknown): HRESULT; stdcall;
    function Get_GroupPropertyName(out value: HSTRING): HRESULT; stdcall;
    function Get_DateStackOption(out value: DateStackOption): HRESULT; stdcall;
    function SaveToString(out value: HSTRING): HRESULT; stdcall;
    function LoadFromString(value: HSTRING): HRESULT; stdcall;
    function SetThumbnailPrefetch(mode: DWORD; requestedSize: UINT32; options: DWORD): HRESULT; stdcall;
    function SetPropertyPrefetch(options: DWORD; propertiesToRetrieve: IUnknown): HRESULT; stdcall;
  end;

  // Windows.Storage.Search.QueryOptions
  IQueryOptionsFactory = interface(IInspectable)
    ['{032E1F8C-A9C1-4E71-8011-0DEE9D4811A3}']
    function CreateCommonFileQuery(query: CommonFileQuery; fileTypeFilter: IInspectable; out queryOptions: IQueryOptions): HRESULT; stdcall;
    function CreateCommonFolderQuery(query: DWORD; out queryOptions: IQueryOptions): HRESULT; stdcall;
  end;

  IStorageFolderQueryOperations = interface(IInspectable)
  ['{CB43CCC9-446B-4A4F-BE97-757771BE5203}']
    function GetIndexedStateAsync(out operation: IUnknown): HRESULT; stdcall;
    function CreateFileQueryOverloadDefault(out value: IStorageFileQueryResult): HRESULT; stdcall;
    function CreateFileQuery(query: CommonFileQuery; out value: IStorageFileQueryResult): HRESULT; stdcall;
    function CreateFileQueryWithOptions(queryOptions: IQueryOptions; out value: IStorageFileQueryResult): HRESULT; stdcall;
    function CreateFolderQueryOverloadDefault(out value: IInspectable): HRESULT; stdcall;
    function CreateFolderQuery(query: DWORD; out value: IInspectable): HRESULT; stdcall;
    function CreateFolderQueryWithOptions(queryOptions: IQueryOptions; out value: IInspectable): HRESULT; stdcall;
    function CreateItemQuery(out value: IInspectable): HRESULT; stdcall;
    function CreateItemQueryWithOptions(queryOptions: IQueryOptions; out value: IInspectable): HRESULT; stdcall;
    function GetFilesAsync(query: CommonFileQuery; startIndex: UINT32; maxItemsToRetrieve: UINT32; out operation: IUnknown): HRESULT; stdcall;
    function GetFilesAsyncOverloadDefaultStartAndCount(query: CommonFileQuery; out operation: IInspectable): HRESULT; stdcall;
    function GetFoldersAsync(query: DWORD; startIndex: UINT32; maxItemsToRetrieve: UINT32; out operation: IInspectable): HRESULT; stdcall;
    function GetFoldersAsyncOverloadDefaultStartAndCount(query: DWORD; out operation: IInspectable): HRESULT; stdcall;
    function GetItemsAsync(startIndex: UINT32; maxItemsToRetrieve: UINT32; out operation: IInspectable): HRESULT; stdcall;
    function AreQueryOptionsSupported(queryOptions: IQueryOptions; out value: LongBool): HRESULT; stdcall;
    function IsCommonFolderQuerySupported(query: DWORD; out value: LongBool): HRESULT; stdcall;
    function IsCommonFileQuerySupported(query: CommonFileQuery; out value: LongBool): HRESULT; stdcall;
  end;

  // Windows.Storage.StorageFolder
  IStorageFolderStatics = interface(IInspectable)
    ['{08F327FF-85D5-48B9-AEE9-28511E339F9F}']
    function GetFolderFromPathAsync(path: HSTRING): IAsyncOperation_1_IStorageFolder; safecall;
  end;

  // Windows.System.LauncherOptions
  ILauncherOptions = interface(IInspectable)
    ['{BAFA21D8-B071-4CD8-853E-341203E557D3}']
    function Get_TreatAsUntrusted(value: PLongBool): HRESULT; stdcall;
    function Set_TreatAsUntrusted(value: LongBool): HRESULT; stdcall;
    function Get_DisplayApplicationPicker(value: PLongBool): HRESULT; stdcall;
    function Set_DisplayApplicationPicker(value: LongBool): HRESULT; stdcall;
    function UI(out value: IInspectable): HRESULT; stdcall;
    function Get_PreferredApplicationPackageFamilyName(out value: HSTRING): HRESULT; stdcall;
    function Set_PreferredApplicationPackageFamilyName(value: HSTRING): HRESULT; stdcall;
    function Get_PreferredApplicationDisplayName(out value: HSTRING): HRESULT; stdcall;
    function Set_PreferredApplicationDisplayName(value: HSTRING): HRESULT; stdcall;
    function Get_FallbackUri(out value: IUnknown): HRESULT; stdcall;
    function Set_FallbackUri(value: IUnknown): HRESULT; stdcall;
    function Get_ContentType(out value: HSTRING): HRESULT; stdcall;
    function Set_ContentType(value: HSTRING): HRESULT; stdcall;
  end;

  // Windows.System.LauncherOptions
  ILauncherOptions2 = interface(IInspectable)
    ['{3BA08EB4-6E40-4DCE-A1A3-2F53950AFB49}']
    function Get_TargetApplicationPackageFamilyName: HSTRING; safecall;
    procedure Set_TargetApplicationPackageFamilyName(value: HSTRING); safecall;
    function Get_NeighboringFilesQuery: IStorageFileQueryResult; safecall;
    procedure Set_NeighboringFilesQuery(value: IStorageFileQueryResult); safecall;
    property NeighboringFilesQuery: IStorageFileQueryResult read Get_NeighboringFilesQuery write Set_NeighboringFilesQuery;
    property TargetApplicationPackageFamilyName: HSTRING read Get_TargetApplicationPackageFamilyName write Set_TargetApplicationPackageFamilyName;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Boolean>
  IAsyncOperationCompletedHandler_1_Boolean = interface(IUnknown)
    ['{C1D3D1A2-AE17-5A5F-B5A2-BDCC8844889A}']
    procedure Invoke(asyncInfo: IAsyncOperation_1_Boolean; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Boolean>
  IAsyncOperation_1_Boolean = interface(IInspectable)
    ['{CDB5EFB3-5788-509D-9BE1-71CCB8A3362A}']
    procedure Set_Completed(handler: IAsyncOperationCompletedHandler_1_Boolean); safecall;
    function Get_Completed: IAsyncOperationCompletedHandler_1_Boolean; safecall;
    function GetResults: Boolean; safecall;
    property Completed: IAsyncOperationCompletedHandler_1_Boolean read Get_Completed write Set_Completed;
  end;

  // Windows.System.Launcher
  ILauncherStatics = interface(IInspectable)
    ['{277151C3-9E3E-42F6-91A4-5DFDEB232451}']
    function LaunchFileAsync(AFile: IStorageFile): IAsyncOperation_1_Boolean; safecall;
    function LaunchFileWithOptionsAsync(AFile: IStorageFile; options: ILauncherOptions): IAsyncOperation_1_Boolean; safecall;
    function LaunchUriAsync(uri: IUnknown): IAsyncOperation_1_Boolean; safecall;
    function LaunchUriWithOptionsAsync(uri: IUnknown; options: ILauncherOptions): IAsyncOperation_1_Boolean; safecall;
  end;

  { TStorageFile }

  TStorageFile = class
  private
    FInstance: IStorageFileStatics; static;
    class function Instance: IStorageFileStatics;
  public
    class function GetFileFromPathAsync(const APath: String): IAsyncOperation_1_IStorageFile;
  end;

  { TStorageFolder }

  TStorageFolder = class
  private
    FInstance: IStorageFolderStatics; static;
    class function Instance: IStorageFolderStatics;
  public
    class function GetFolderFromPathAsync(const APath: String): IAsyncOperation_1_IStorageFolder;
  end;

  { TLauncher }

  TLauncher = class
  private
    FInstance: ILauncherStatics; static;
    class function Instance: ILauncherStatics;
  public
    class function LaunchFileAsync(AFile: IStorageFile): IAsyncOperation_1_Boolean;
    class function LaunchFileWithOptionsAsync(AFile: IStorageFile; options: ILauncherOptions): IAsyncOperation_1_Boolean;
  end;

  { TLauncherThread }

  TLauncherThread = class(TThread)
  private
    FEvent: THandle;
    FFileName: String;
    FStorageFile: IStorageFile;
    FStorageFolder: IStorageFolder;
  protected
    procedure Execute; override;
  public
    constructor Create(const FileName: String);
    destructor Destroy; override;
    class procedure LaunchFileAsync(const FileName: String);
  end;

  { TAsyncOperationCompletedHandler_1_IStorageFile }

  TAsyncOperationCompletedHandler_1_IStorageFile = class(TInterfacedObject, IAsyncOperationCompletedHandler_1_IStorageFile)
  private
    FLauncher: TLauncherThread;
  protected
    procedure Invoke(asyncInfo: IAsyncOperation_1_IStorageFile; asyncStatus: AsyncStatus); safecall;
  public
    constructor Create(ALauncher: TLauncherThread);
  end;

  { TAsyncOperationCompletedHandler_1_IStorageFolder }

  TAsyncOperationCompletedHandler_1_IStorageFolder = class(TInterfacedObject, IAsyncOperationCompletedHandler_1_IStorageFolder)
  private
    FLauncher: TLauncherThread;
  protected
    procedure Invoke(asyncInfo: IAsyncOperation_1_IStorageFolder; asyncStatus: AsyncStatus); safecall;
  public
    constructor Create(ALauncher: TLauncherThread);
  end;

implementation

uses
  ComObj, Windows;

{ TStorageFile }

class function TStorageFile.Instance: IStorageFileStatics;
begin
  if (FInstance = nil) then
  begin
    OleCheck(RoCreateInstance(Windows_Storage_StorageFile, IStorageFileStatics, FInstance));
  end;
  Result:= FInstance;
end;

class function TStorageFile.GetFileFromPathAsync(const APath: String): IAsyncOperation_1_IStorageFile;
begin
  with TWindowsString.Create(APath) do
  try
    Result:= Instance.GetFileFromPathAsync(Handle);
  finally
    Free;
  end;
end;

{ TStorageFolder }

class function TStorageFolder.Instance: IStorageFolderStatics;
begin
  if (FInstance = nil) then
  begin
    OleCheck(RoCreateInstance(Windows_Storage_StorageFolder, IStorageFolderStatics, FInstance));
  end;
  Result:= FInstance;
end;

class function TStorageFolder.GetFolderFromPathAsync(const APath: String): IAsyncOperation_1_IStorageFolder;
begin
  with TWindowsString.Create(APath) do
  try
    Result:= Instance.GetFolderFromPathAsync(Handle);
  finally
    Free;
  end;
end;

{ TLauncher }

class function TLauncher.Instance: ILauncherStatics;
begin
  if (FInstance = nil) then
  begin
    OleCheck(RoCreateInstance(Windows_System_Launcher, ILauncherStatics, FInstance));
  end;
  Result:= FInstance;
end;

class function TLauncher.LaunchFileAsync(AFile: IStorageFile): IAsyncOperation_1_Boolean;
begin
  Result:= Instance.LaunchFileAsync(AFile);
end;

class function TLauncher.LaunchFileWithOptionsAsync(AFile: IStorageFile; options: ILauncherOptions): IAsyncOperation_1_Boolean;
begin
  Result:= Instance.LaunchFileWithOptionsAsync(AFile, options);
end;

{ TLauncherThread }

procedure TLauncherThread.Execute;
var
  AOptions: ILauncherOptions;
  AOptions2: ILauncherOptions2;
  AQuery: IStorageFileQueryResult;
  AFolderQuery: IStorageFolderQueryOperations;
  FileOperation: IAsyncOperation_1_IStorageFile;
  FolderOperation: IAsyncOperation_1_IStorageFolder;
  FileHandler: TAsyncOperationCompletedHandler_1_IStorageFile;
  FolderHandler: TAsyncOperationCompletedHandler_1_IStorageFolder;
begin
  try
    FileHandler:= TAsyncOperationCompletedHandler_1_IStorageFile.Create(Self);
    FileOperation:= TStorageFile.GetFileFromPathAsync(FFileName);
    FileOperation.Completed:= FileHandler;
    WaitForSingleObject(FEvent, INFINITE);
    ResetEvent(FEvent);

    FolderHandler:= TAsyncOperationCompletedHandler_1_IStorageFolder.Create(Self);
    FolderOperation:= TStorageFolder.GetFolderFromPathAsync(ExtractFileDir(FFileName));
    FolderOperation.Completed:= FolderHandler;
    WaitForSingleObject(FEvent, INFINITE);

    OleCheck(RtActivateInstance(Windows_System_LauncherOptions, AOptions));

    OleCheck(AOptions.QueryInterface(ILauncherOptions2, AOptions2));

    OleCheck(FStorageFolder.QueryInterface(IStorageFolderQueryOperations, AFolderQuery));

    OleCheck(AFolderQuery.CreateFileQuery(DefaultQuery, AQuery));

    AOptions2.NeighboringFilesQuery:= AQuery;

    TLauncher.LaunchFileWithOptionsAsync(FStorageFile, AOptions);
  except
    on E: Exception do
    begin
      MessageBoxW(0, PWideChar(UTF8Decode(E.Message)), nil, MB_OK or MB_ICONERROR);
    end;
  end;
end;

constructor TLauncherThread.Create(const FileName: String);
begin
  FFileName:= FileName;
  inherited Create(True);
  FreeOnTerminate:= True;
  FEvent:= CreateEventW(nil, True, False, nil);
end;

destructor TLauncherThread.Destroy;
begin
  CloseHandle(FEvent);
  inherited Destroy;
end;

class procedure TLauncherThread.LaunchFileAsync(const FileName: String);
begin
  with TLauncherThread.Create(FileName) do Start;
end;

{ TAsyncOperationCompletedHandler_1_IStorageFolder }

procedure TAsyncOperationCompletedHandler_1_IStorageFolder.Invoke(
  asyncInfo: IAsyncOperation_1_IStorageFolder; asyncStatus: AsyncStatus);
  safecall;
begin
  FLauncher.FStorageFolder:= asyncInfo.GetResults;
  SetEvent(FLauncher.FEvent);
end;

constructor TAsyncOperationCompletedHandler_1_IStorageFolder.Create(
  ALauncher: TLauncherThread);
begin
  FLauncher:= ALauncher;
end;

{ TAsyncOperationCompletedHandler_1_IStorageFile }

procedure TAsyncOperationCompletedHandler_1_IStorageFile.Invoke(
  asyncInfo: IAsyncOperation_1_IStorageFile; asyncStatus: AsyncStatus);
  safecall;
begin
  FLauncher.FStorageFile:= asyncInfo.GetResults;
  SetEvent(FLauncher.FEvent);
end;

constructor TAsyncOperationCompletedHandler_1_IStorageFile.Create(
  ALauncher: TLauncherThread);
begin
  FLauncher:= ALauncher;
end;

end.
