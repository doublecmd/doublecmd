unit uFileView; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics, ComCtrls, contnrs, fgl,
  uFile, uDisplayFile, uFileSource, uFormCommands, uDragDropEx, DCXmlConfig,
  DCClassesUtf8, uFileSorting, uFileViewHistory, uFileProperty, uFileViewWorker,
  uFunctionThread, uFileSystemWatcher, fQuickSearch, StringHashList, uGlobs;

type

  TFileView = class;

  {en
     Called before path is changed. If it returns @true the paths is changed
     (and if successful, OnAfterChangePath is called). If it returns @false,
     the path is not changed.
     NewFileSource is @nil the last file source is to be removed.
  }
  TOnBeforeChangePath = function (FileView: TFileView;
                                  NewFileSource: IFileSource;
                                  const NewPath : String): Boolean of object;
  TOnAfterChangePath = procedure (FileView: TFileView) of object;
  TOnChangeActiveFile = procedure (FileView: TFileView; const aFile : TFile) of object;
  TOnActivate = procedure (aFileView: TFileView) of object;
  TOnFileListChanged = procedure (aFileView: TFileView) of object;

  TDropParams = class;
  TDragDropType = (ddtInternal, ddtExternal);
  // Lists all operations supported by dragging and dropping items
  // in the panel (external, internal and via menu).
  TDragDropOperation = (ddoCopy, ddoMove, ddoSymLink, ddoHardLink);

  TFileViewWorkers = specialize TFPGObjectList<TFileViewWorker>;

  TFileViewFlag = (fvfDelayLoadingFiles, fvfDontLoadFiles, fvfDontWatch);
  TFileViewFlags = set of TFileViewFlag;

  TFileViewRequest = (fvrqApplyPendingFilesChanges,   // Pending files changes need to be applied to the file list
                      fvrqHashFileList,               // Files names need rehashing due to file list changes
                      fvrqMakeDisplayFileList);       // Filtered file list needs to be created
  TFileViewRequests = set of TFileViewRequest;
  TFileViewNotification = (fvnDisplayFileListChanged,        // Filtered file list was created (filter changed, show/hide hidden files option changed, etc.)
                           fvnFileSourceFileListChanged,     // File list was loaded from FileSource
                           fvnSelectionChanged,              // Files were selected/deselected
                           fvnVisibleFilePropertiesChanged); // Different files or their properties are now visible
  TFileViewNotifications = set of TFileViewNotification;

  {en
     Base class for any view of a file or files.
     There should always be at least one file displayed on the view.
  }

  { TFileView }

  TFileView = class(TWinControl)
  private
    {en
       History of viewed paths and file sources.

       Contains:

       - File sources hierarchy associated with this view.
         Last element is the file source that is currently being viewed,
         parent file source is (index-1) and so on up to zero (first file source).

       - Visited paths history for each file source.
         Last path is the currently viewed path.
    }
    FHistory: TFileViewHistory;
    FSortings: TFileSortings;
    {en
       Which file properties are needed to be displayed for each file.
    }
    FFilePropertiesNeeded: TFilePropertiesTypes;
    FFileViewWorkers: TFileViewWorkers;
    FFlags: TFileViewFlags;
    FHashedFiles: TBucketList;  //<en Contains pointers to file source files for quick checking if a file object is still valid
    FHashedNames: TStringHashList;
    FPendingFilesChanges: TFPList;
    FReloadNeeded: Boolean;     //<en If file list should be reloaded
    FWorkersThread: TFunctionThread;
    FReloadTimer: TTimer;
    FLoadFilesStartTime: TDateTime;
    FLoadFilesFinishTime: TDateTime;
    FLoadFilesNoDelayCount: Integer; //<en How many reloads have been accepted without delay
    FNotifications: TFileViewNotifications;
    FRecentlyUpdatedFiles: TDisplayFiles;    //<en Recently updated files.
    FRecentlyUpdatedFilesTimer: TTimer;
    FRequests: TFileViewRequests;
    FUpdateCount: Integer;           //<en Nr of times BeginUpdate was called without corresponding EndUpdate

    FActive: Boolean;             //<en Is this view active
    FLastActiveFile: String;      //<en Last active file (cursor)
    {en
       File name which should be selected. Sometimes the file might not yet
       exist in the filelist (for example after rename or create), but will be
       in the list on next reload.
    }
    FRequestedActiveFile: String;
    FFileFilter: String;
    FFilterOptions: TQuickSearchOptions;
    FWatchPath: String;
    FLastMark: String;
    FLastLoadedFileSource: IFileSource;
    FLastLoadedPath: String;

    FMethods: TFormCommands;

    FOnBeforeChangePath : TOnBeforeChangePath;
    FOnAfterChangePath : TOnAfterChangePath;
    FOnChangeActiveFile: TOnChangeActiveFile;
    FOnActivate : TOnActivate;
    FOnFileListChanged : TOnFileListChanged;

    procedure AddFile(const FileName, APath: String; NewFilesPosition: TNewFilesPosition; UpdatedFilesPosition: TUpdatedFilesPosition);
    procedure AddEventToPendingFilesChanges(const EventData: TFSWatcherEventData);
    procedure ApplyPendingFilesChanges;
    procedure ClearPendingFilesChanges;
    procedure ClearRecentlyUpdatedFiles;
    procedure DoOnFileListChanged;
    function FileListLoaded: Boolean;
    function GetCurrentAddress: String;
    function GetNotebookPage: TCustomPage;
    function GetCurrentFileSource: IFileSource;
    function GetCurrentFileSourceIndex: Integer;
    function GetCurrentPathIndex: Integer;
    function GetFileSource(Index: Integer): IFileSource;
    function GetFileSourcesCount: Integer;
    function GetFiltered: Boolean;
    function GetPath(FileSourceIndex, PathIndex: Integer): UTF8String;
    function GetPathsCount(FileSourceIndex: Integer): Integer;
    function GetSortingForSorter: TFileSortings;
    function GetWatcherActive: Boolean;
    procedure HandleNotifications;
    procedure HandleRequests;
    {en
       Store pointers to file source files in a fast to read structure.
    }
    procedure HashFileList;
    procedure InsertFile(ADisplayFile: TDisplayFile; AFileList: TDisplayFiles; NewFilesPosition: TNewFilesPosition);
    procedure RemoveFile(ADisplayFile: TDisplayFile);
    procedure RemoveFile(const FileName: String);
    procedure RenameFile(const NewFileName, OldFileName, APath: String; NewFilesPosition: TNewFilesPosition; UpdatedFilesPosition: TUpdatedFilesPosition);
    procedure ResortFile(ADisplayFile: TDisplayFile; AFileList: TDisplayFiles);
    procedure SetFlags(AValue: TFileViewFlags);
    procedure StartRecentlyUpdatedTimerIfNeeded;
    procedure UpdateFile(const FileName, APath: String; NewFilesPosition: TNewFilesPosition; UpdatedFilesPosition: TUpdatedFilesPosition);
    procedure UpdatedFilesTimerEvent(Sender: TObject);
    procedure UpdatePath(UpdateAddressToo: Boolean);
    procedure UpdateTitle;
    procedure VisualizeFileUpdate(AFile: TDisplayFile);
    {en
       Assigns the built lists to the file view and displays new the file list.
    }
    procedure SetFileList(var NewAllDisplayFiles: TDisplayFiles;
                          var NewFilteredDisplayFiles: TDisplayFiles);
    procedure EnableWatcher(Enable: Boolean);

    procedure ActivateEvent(Sender: TObject);
    function CheckIfDelayReload: Boolean;
    procedure DoReload;
    procedure HandleFSWatcherEvent(const EventData: TFSWatcherEventData;
                                   NewFilesPosition: TNewFilesPosition;
                                   UpdatedFilesPosition: TUpdatedFilesPosition);
    procedure ReloadEvent(const aFileSource: IFileSource; const ReloadedPaths: TPathsArray);
    procedure ReloadTimerEvent(Sender: TObject);
    procedure WatcherEvent(const EventData: TFSWatcherEventData);

  protected
    FAllDisplayFiles: TDisplayFiles;    //<en List of all files that can be displayed
    FFiles: TDisplayFiles;              //<en List of displayed files (filtered)
    FSavedSelection: TStringListEx;

    {en
       Initializes parts of the view common to all creation methods.
    }
    procedure CreateDefault(AOwner: TWinControl); virtual;

    procedure AddWorker(const Worker: TFileViewWorker; SetEvents: Boolean = True);
    procedure BeginUpdate;
    procedure CalculateSpace(AFile: TDisplayFile);
    procedure CalculateSpace(var AFileList: TFVWorkerFileList);
    procedure CalculateSpaceOnUpdate(const UpdatedFile: TDisplayFile;
                                     const UserData: Pointer);
    procedure EndUpdate;
    procedure EnsureDisplayProperties; virtual; abstract;
    function GetCurrentPath: String; virtual;
    procedure SetCurrentPath(NewPath: String); virtual;
    function GetActiveDisplayFile: TDisplayFile; virtual; abstract;
    function GetWorkersThread: TFunctionThread;
    procedure InvertFileSelection(AFile: TDisplayFile; bNotify: Boolean = True);
    function IsVisibleToUser: Boolean;
    procedure Notify(NewNotifications: TFileViewNotifications);
    procedure PropertiesRetrieverOnUpdate(const UpdatedFile: TDisplayFile;
                                          const UserData: Pointer);
    procedure Request(NewRequests: TFileViewRequests);
    {en
       This function should set active file by reference of TFile
       or at least by all the properties of the given TFile,
       in case the object is a clone.
       It could be useful in case there are multiple files with the
       same name in the panel and SetActiveFile(String) is not enough.
    }
    procedure SetActiveFile(const aFile: TFile); virtual; overload;
    procedure SetActive(bActive: Boolean); virtual;

    {en
       Executed after file list has been retrieved.
       Runs from GUI thread.
    }
    procedure AfterMakeFileList; virtual;
    {en
       Executed before file list has been retrieved.
       Runs from GUI thread.
    }
    procedure BeforeMakeFileList; virtual;
    function BeginDragExternal(DragFile: TDisplayFile; DragDropSource: uDragDropEx.TDragDropSource;
                               MouseButton: TMouseButton; ScreenStartPoint: TPoint): Boolean;
    procedure ChooseFile(const AFile: TDisplayFile; FolderMode: Boolean = False); virtual;
    function DimColor(AColor: TColor): TColor;
    procedure DoFileUpdated(AFile: TDisplayFile; UpdatedProperties: TFilePropertiesTypes = []); virtual;
    procedure DoHandleKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure DoSelectionChanged; virtual;
    procedure DoUpdateView; virtual;
    {en
       Returns current work type in progress.
    }
    function GetCurrentWorkType: TFileViewWorkType;
    function IsActiveItemValid: Boolean;
    function IsReferenceValid(aFile: TDisplayFile): Boolean;
    {en
       Returns True if there are no files shown in the panel.
    }
    function IsEmpty: Boolean; inline;
    {en
       Returns True if item is not nil and not '..'.
       May be extended to include other conditions.
    }
    function IsItemValid(AFile: TDisplayFile): Boolean;

    procedure SetSorting(const NewSortings: TFileSortings); virtual;
    procedure SortAllDisplayFiles;

    {en
       Retrieves file list from file source into FAllDisplayFiles.
       Either runs directly or starts a new thread.
    }
    procedure MakeFileSourceFileList;

    {en
       Called before changing path. If returns @false the path is not changed.
       NewFileSource is @nil if the last file source is to be removed.
    }
    function BeforeChangePath(NewFileSource: IFileSource; NewPath: String): Boolean; virtual;
    {en
       Called after path is changed.
    }
    procedure AfterChangePath; virtual;
    {en
       Makes a new display file list and redisplays the changed list.
    }
    procedure ReDisplayFileList;
    {en
       Redraw DisplayFile if it is visible.
    }
    procedure RedrawFile(DisplayFile: TDisplayFile); virtual; abstract;
    procedure WorkerStarting(const Worker: TFileViewWorker); virtual;
    procedure WorkerFinished(const Worker: TFileViewWorker); virtual;

    property Active: Boolean read FActive write SetActive;
    property FilePropertiesNeeded: TFilePropertiesTypes read FFilePropertiesNeeded write FFilePropertiesNeeded;
    property LastActiveFile: String read FLastActiveFile write FLastActiveFile;
    property RequestedActiveFile: String read FRequestedActiveFile write FRequestedActiveFile;
    property SortingForSorter: TFileSortings read GetSortingForSorter;
    property WorkersThread: TFunctionThread read GetWorkersThread;

  public
    property  DisplayFiles: TDisplayFiles read FFiles;

  public
    constructor Create(AOwner: TWinControl;
                       AFileSource: IFileSource;
                       APath: String;
                       AFlags: TFileViewFlags = []); virtual reintroduce;
    // Constructor for cloning.
    constructor Create(AOwner: TWinControl;
                       AFileView: TFileView;
                       AFlags: TFileViewFlags = []); virtual reintroduce;
    constructor Create(AOwner: TWinControl;
                       AConfig: TIniFileEx;
                       ASectionName: String;
                       ATabIndex: Integer;
                       AFlags: TFileViewFlags = []); virtual reintroduce;
    constructor Create(AOwner: TWinControl;
                       AConfig: TXmlConfig;
                       ANode: TXmlNode;
                       AFlags: TFileViewFlags = []); virtual reintroduce;

    destructor Destroy; override;

    function Clone(NewParent: TWinControl): TFileView; virtual;
    procedure CloneTo(AFileView: TFileView); virtual;

    procedure AddFileSource(aFileSource: IFileSource; aPath: String); virtual;
    procedure RemoveCurrentFileSource; virtual;
    procedure RemoveAllFileSources; virtual;
    {en
       Assigns the list of file sources and paths into those file sources
       from another file view.
    }
    procedure AssignFileSources(const otherFileView: TFileView); virtual;

    {en
       Returns a copy of currently active file.
       Caller is responsible for freeing it.

       There should always be at least one file in the view at any time, but
       what 'active' means depends on the specific view, so ActiveFile may
       return 'nil' if there is no such file. Usually it is the file pointed
       to by the cursor or some other indicator.
    }
    function CloneActiveFile: TFile;
    {en
       A list of all files in the file view.
       Caller is responsible for freeing the list.
    }
    function CloneFiles: TFiles;
    {en
       A list of files selected by the user
       (this should be a subset of displayed files list returned by Files).
       Caller is responsible for freeing the list.
    }
    function CloneSelectedFiles: TFiles;

    {en
       Retrieves files from file source again and displays the new list of files.
       Returns @true if reloading is done, @false if reloading will not be done
       (for example paths don't match).
    }
    function Reload(const PathsToReload: TPathsArray = nil): Boolean; overload;
    function Reload(const PathToReload: String): Boolean; overload;
    procedure ReloadIfNeeded;
    procedure StopWorkers; virtual;

    // For now we use here the knowledge that there are tabs.
    // Config should be independent of that in the future.
    procedure LoadConfiguration(Section: String; TabIndex: Integer); virtual;
    procedure SaveConfiguration(Section: String; TabIndex: Integer); virtual;
    procedure LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); virtual;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); virtual;

    procedure UpdateView;

    {en
       Moves the selection focus to the file specified by aFilePath.
       @param(aFilePath may be an absolute path to the file or just a file name.)
    }
    procedure SetActiveFile(aFilePath: String); virtual; overload;

    procedure CalculateSpaceOfAllDirectories;
    {en
       Changes the current path to a parent directory.
       @param(AllowChangingFileSource
              If this parameter is @true and current path is the root path
              of the current file source, then the current file source will
              be removed (closed) and a previous file source will be displayed.)
    }
    procedure ChangePathToParent(AllowChangingFileSource: Boolean); virtual;
    {en
       Change the current path to a subdirectory pointed to by aFile.
    }
    procedure ChangePathToChild(const aFile: TFile); virtual;

    procedure ExecuteCommand(CommandName: String; const Params: array of String); virtual;

    {en
       Returns @true if at least one file is somehow selected.
       What "selected" means depends on the concrete file view implementation.
       (Usually it will be a different method of selecting than ActiveFile.)
    }
    function HasSelectedFiles: Boolean; virtual;
    procedure InvertAll;
    procedure LoadSelectionFromClipboard;
    procedure LoadSelectionFromFile(const AFileName: String);
    procedure MarkCurrentExtension(bSelect: Boolean);
    procedure MarkFile(AFile: TDisplayFile; bSelect: Boolean; bNotify: Boolean = True);
    procedure MarkFiles(bSelect: Boolean);
    procedure MarkGroup(const sMask: String; bSelect: Boolean);
    procedure MarkGroup(bSelect: Boolean);
    procedure OpenActiveFile;
    procedure RestoreSelection;
    procedure SaveSelection;
    procedure SaveSelectionToFile(const AFileName: String);

    {en
       Handles drag&drop operations onto the file view.
       Does any graphic work and executes operations with dropped files if allowed.
       Handles freeing DropParams.
    }
    procedure DoDragDropOperation(Operation: TDragDropOperation;
                                  var DropParams: TDropParams); virtual abstract;

    procedure GoToHistoryIndex(aFileSourceIndex, aPathIndex: Integer);
    procedure GoToPrevHistory;
    procedure GoToNextHistory;

    procedure SetFileFilter(NewFilter: String; NewFilterOptions: TQuickSearchOptions);

    property CurrentAddress: String read GetCurrentAddress;
    property CurrentFileSourceIndex: Integer read GetCurrentFileSourceIndex;
    property CurrentPath: String read GetCurrentPath write SetCurrentPath;
    property CurrentPathIndex: Integer read GetCurrentPathIndex;
    property FileFilter: String read FFileFilter;
    property FilterOptions: TQuickSearchOptions read FFilterOptions;
    property Filtered: Boolean read GetFiltered;
    property FileSource: IFileSource read GetCurrentFileSource;
    property FileSources[Index: Integer]: IFileSource read GetFileSource;
    property FileSourcesCount: Integer read GetFileSourcesCount;
    property Flags: TFileViewFlags read FFlags write SetFlags;
    property Path[FileSourceIndex, PathIndex: Integer]: UTF8String read GetPath;
    property PathsCount[FileSourceIndex: Integer]: Integer read GetPathsCount;

    property Sorting: TFileSortings read FSortings write SetSorting;
    property WatcherActive: Boolean read GetWatcherActive;

    property NotebookPage: TCustomPage read GetNotebookPage;
    property OnBeforeChangePath : TOnBeforeChangePath read FOnBeforeChangePath write FOnBeforeChangePath;
    property OnAfterChangePath : TOnAfterChangePath read FOnAfterChangePath write FOnAfterChangePath;
    property OnChangeActiveFile : TOnChangeActiveFile read FOnChangeActiveFile write FOnChangeActiveFile;
    property OnActivate : TOnActivate read FOnActivate write FOnActivate;
    {en
       Called when files on the file source in the currently displayed path
       change (are added, removed or updated). It is not called when simply
       the list of files is filtered.
    }
    property OnFileListChanged : TOnFileListChanged read FOnFileListChanged write FOnFileListChanged;
  end;

  { TDropParams }

  {  Parameters passed to functions handling drag&drop.

     FileList
        List of files dropped (the class handles freeing it).
     DropEffect
        Desired action to take with regard to the files.
     ScreenDropPoint
        Point where the drop occurred.
     DropIntoDirectories
        If true it is/was allowed to drop into specific directories
        (directories may have been tracked while dragging).
        Target path will be modified accordingly if ScreenDropPoint points
        to a directory in the target panel.
     SourcePanel
        If drag drop type is internal, this field points to the source panel.
     TargetPanel
        Panel, where the drop occurred. }
  TDropParams = class
  public
    Files: TFiles;
    DropEffect: TDropEffect;
    ScreenDropPoint: TPoint;
    DropIntoDirectories: Boolean;
    SourcePanel: TFileView;
    TargetPanel: TFileView;
    TargetPath: String;

    constructor Create(var aFiles: TFiles;
                       aDropEffect: TDropEffect;
                       aScreenDropPoint: TPoint;
                       aDropIntoDirectories: Boolean;
                       aSourcePanel: TFileView;
                       aTargetPanel: TFileView;
                       aTargetPath: String);
    destructor Destroy; override;

    // States, whether the drag&drop operation was internal or external.
    // If SourcePanel is not nil, then it's assumed it was internal.
    function GetDragDropType: TDragDropType;
  end;
  PDropParams = ^TDropParams;

implementation

uses
  Clipbrd, Dialogs, LCLProc, LCLType, Forms, StrUtils, dmCommonData,
  fMaskInputDlg, uMasks, DCOSUtils, uOSUtils, DCStrUtils, uDCUtils,
  uDebug, uLng, uShowMsg, uFileSystemFileSource, uFileSourceUtil,
  uFileViewNotebook, uSearchTemplate, uKeyboard, uFileFunctions;

const
  MinimumReloadInterval  = 1000; // 1 second

constructor TFileView.Create(AOwner: TWinControl; AFileSource: IFileSource; APath: String; AFlags: TFileViewFlags = []);
begin
  FFlags := AFlags;
  CreateDefault(AOwner);

  FHistory.Add(AFileSource, aPath);
  FileSource.AddReloadEventListener(@ReloadEvent);

  // Update view before making file source file list,
  // so that file list isn't unnecessarily displayed twice.
  UpdateView;
  MakeFileSourceFileList;
end;

constructor TFileView.Create(AOwner: TWinControl; AFileView: TFileView; AFlags: TFileViewFlags = []);
begin
  FFlags := AFlags;
  CreateDefault(AOwner);
  AFileView.CloneTo(Self);
  if Assigned(FileSource) then
    FileSource.AddReloadEventListener(@ReloadEvent);
  UpdateView;
end;

constructor TFileView.Create(AOwner: TWinControl; AConfig: TIniFileEx; ASectionName: String; ATabIndex: Integer; AFlags: TFileViewFlags = []);
begin
  FFlags := AFlags;
  CreateDefault(AOwner);
  LoadConfiguration(ASectionName, ATabIndex);

  // Update view before making file source file list,
  // so that file list isn't unnecessarily displayed twice.
  UpdateView;

  if FileSourcesCount > 0 then
  begin
    MakeFileSourceFileList;
  end;
end;

constructor TFileView.Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode; AFlags: TFileViewFlags = []);
begin
  FFlags := AFlags;
  CreateDefault(AOwner);

  LoadConfiguration(AConfig, ANode);

  // Update view before making file source file list,
  // so that file list isn't unnecessarily displayed twice.
  UpdateView;

  if FileSourcesCount > 0 then
  begin
    MakeFileSourceFileList;
  end;
end;

procedure TFileView.CreateDefault(AOwner: TWinControl);
begin
  FMethods := TFormCommands.Create(Self);
  FHistory := TFileViewHistory.Create;
  FSavedSelection:= TStringListEx.Create;
  FLastMark := '*';
  FFiles := TDisplayFiles.Create(False);
  FFilterOptions := gQuickSearchOptions;
  FHashedNames := TStringHashList.Create(True);
  FFileViewWorkers := TFileViewWorkers.Create(False);
  FReloadTimer := TTimer.Create(Self);
  FReloadTimer.Enabled := False;
  FReloadTimer.OnTimer := @ReloadTimerEvent;

  BorderStyle := bsNone; // Before Create or the window handle may be recreated
  inherited Create(AOwner);
  Align := alClient;
  Parent := AOwner;

  if Parent is TFileViewPage then
    (Parent as TFileViewPage).OnActivate := @ActivateEvent;
end;

destructor TFileView.Destroy;
var
  i: Integer;
  DbgWorkersThread: TFunctionThread;
begin
  for i := 0 to FileSourcesCount - 1 do
    FHistory.FileSource[i].RemoveReloadEventListener(@ReloadEvent);

  if Assigned(FWorkersThread) then
  begin
    StopWorkers;

    // Wait until all the workers finish.
    FWorkersThread.Finish;
    DCDebug('Waiting for workers thread ', hexStr(FWorkersThread));
    DbgWorkersThread := FWorkersThread;
    TFunctionThread.Finalize(FWorkersThread);
    DCDebug('Finalized workers thread   ', hexStr(DbgWorkersThread));
  end;

  // Now all the workers can be safely freed.
  if Assigned(FFileViewWorkers) then
  begin
    for i := 0 to FFileViewWorkers.Count - 1 do
    begin
      with FFileViewWorkers[i] do
      begin
        if Working then
          DCDebug('Error: Worker still working.')
        else if not CanBeDestroyed then
          DCDebug('Error: Worker cannot be destroyed.');
        Free;
      end;
    end;
    FreeAndNil(FFileViewWorkers);
  end;

  ClearPendingFilesChanges;
  ClearRecentlyUpdatedFiles;
  RemoveAllFileSources;

  FreeAndNil(FFiles);
  FreeAndNil(FAllDisplayFiles);
  FreeAndNil(FHashedFiles);
  FreeAndNil(FHashedNames);

  inherited Destroy;

  FreeAndNil(FHistory);
  FreeThenNil(FSavedSelection);
  FPendingFilesChanges.Free;
  FRecentlyUpdatedFiles.Free;
end;

function TFileView.Clone(NewParent: TWinControl): TFileView;
begin
  raise Exception.Create('Cannot create object of abstract class');
  Result := nil; // For compiler warning.
end;

procedure TFileView.CloneTo(AFileView: TFileView);
var
  I: Integer;
begin
  if Assigned(AFileView) then
  begin
    AFileView.FFlags := FFlags;
    AFileView.FLastLoadedFileSource := FLastLoadedFileSource;
    AFileView.FLastLoadedPath := FLastLoadedPath;
    AFileView.FLastMark := FLastMark;
    // FFileSource should have been passed to FileView constructor already.
    // FMethods are created in FileView constructor.
    AFileView.OnBeforeChangePath := Self.OnBeforeChangePath;
    AFileView.OnAfterChangePath := Self.OnAfterChangePath;
    AFileView.OnActivate := Self.OnActivate;
    AFileView.OnFileListChanged := Self.OnFileListChanged;

    for I := 0 to FSavedSelection.Count - 1 do
      AFileView.FSavedSelection.Add(FSavedSelection.Strings[I]);

    AFileView.FHistory.Assign(Self.FHistory);
    AFileView.FSortings := CloneSortings(Self.FSortings);
    AFileView.FLastActiveFile := Self.FLastActiveFile;
    AFileView.FRequestedActiveFile := Self.FRequestedActiveFile;
    AFileView.FReloadNeeded := Self.FReloadNeeded;

    if Assigned(Self.FAllDisplayFiles) then
    begin
      AFileView.FAllDisplayFiles := Self.FAllDisplayFiles.Clone(True);
      AFileView.Notify([fvnFileSourceFileListChanged]);
      AFileView.Request([fvrqHashFileList]);
    end;

    // FFiles need to be recreated because the filter is not cloned.
    // This is done in AFileView.UpdateView.
  end;
end;

procedure TFileView.AddEventToPendingFilesChanges(const EventData: TFSWatcherEventData);
  function CheckLast(const sFileName: String; const EventType: TFSWatcherEventTypes; bDelete: Boolean): Boolean;
  var
    i: Integer;
    pEvent: PFSWatcherEventData;
  begin
    Result := False;
    for i := FPendingFilesChanges.Count - 1 downto 0 do
    begin
      pEvent := PFSWatcherEventData(FPendingFilesChanges[i]);
      if pEvent^.FileName = sFileName then
      begin
        if pEvent^.EventType in EventType then
        begin
          Result := True;
          if bDelete then
          begin
            Dispose(pEvent);
            FPendingFilesChanges.Delete(i);
          end
          else
            Break;
        end
        else
          Break;
      end;
    end;
  end;
var
  pEvent: PFSWatcherEventData;
begin
  if not Assigned(FPendingFilesChanges) then
    FPendingFilesChanges := TFPList.Create;

  if (Assigned(FAllDisplayFiles) and (FAllDisplayFiles.Count > 0) and
      (FPendingFilesChanges.Count > FAllDisplayFiles.Count div 4)) or
     (FPendingFilesChanges.Count > 100) then
  begin
    // Too many changes. Reload the whole file list again.
    Reload(EventData.Path);
  end
  else
  begin
    // Remove obsolete events if they exist.
    case EventData.EventType of
      fswFileCreated:
        CheckLast(EventData.FileName, [fswFileChanged, fswFileCreated, fswFileDeleted], True);
      fswFileDeleted:
        CheckLast(EventData.FileName, [fswFileChanged, fswFileCreated, fswFileDeleted], True);
      fswFileChanged:
        // If FileChanged or FileCreated already exists then new one is not scheduled.
        // FileCreated will cause update anyway if a file already exists in the filelist.
        if CheckLast(EventData.FileName, [fswFileChanged, fswFileCreated], False) then
          Exit;
      fswFileRenamed:
        CheckLast(EventData.FileName, [fswFileChanged, fswFileCreated, fswFileDeleted], True);
    end;
    New(pEvent);
    FPendingFilesChanges.Add(pEvent);
    pEvent^ := EventData;
  end;
end;

procedure TFileView.ApplyPendingFilesChanges;
var
  i: Integer;
  pEvent: PFSWatcherEventData;
begin
  if Assigned(FPendingFilesChanges) then
  begin
    BeginUpdate;
    try
      // Check if another reload was not issued.
      if FileListLoaded and (GetCurrentWorkType <> fvwtCreate) then
      begin
        for i := 0 to FPendingFilesChanges.Count - 1 do
        begin
          pEvent := PFSWatcherEventData(FPendingFilesChanges[i]);
          // Insert new files at sorted position since the filelist hasn't been
          // shown to the user yet, so no need to use user setting.
          HandleFSWatcherEvent(pEvent^, nfpSortedPosition, ufpSortedPosition);
        end;
      end;
      ClearPendingFilesChanges;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TFileView.ClearPendingFilesChanges;
var
  pEvent: PFSWatcherEventData;
  i: Integer;
begin
  if Assigned(FPendingFilesChanges) then
  begin
    for i := 0 to FPendingFilesChanges.Count - 1 do
    begin
      pEvent := PFSWatcherEventData(FPendingFilesChanges[i]);
      Dispose(pEvent);
    end;
    FreeAndNil(FPendingFilesChanges);
  end;
end;

procedure TFileView.ClearRecentlyUpdatedFiles;
begin
  if Assigned(FRecentlyUpdatedFilesTimer) then
    FRecentlyUpdatedFilesTimer.Enabled := False;

  if Assigned(FRecentlyUpdatedFiles) then
    FRecentlyUpdatedFiles.Clear;
end;

function TFileView.DimColor(AColor: TColor): TColor;
begin
  if (not Active) and (gInactivePanelBrightness < 100) then
    Result := ModColor(AColor, gInactivePanelBrightness)
  else
    Result := AColor;
end;

procedure TFileView.DoFileUpdated(AFile: TDisplayFile; UpdatedProperties: TFilePropertiesTypes);
begin
  RedrawFile(AFile);
end;

procedure TFileView.DoHandleKeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of

    VK_BACK:
      begin
        ChangePathToParent(True);
        Key := 0;
      end;

    VK_MULTIPLY:
    begin
      InvertAll;
      Key := 0;
    end;

    VK_ADD:
      begin
        if Shift = [ssCtrl] then
          MarkFiles(True)
        else if Shift = [] then
          MarkGroup(True)
        else if Shift = [ssShift] then
          MarkCurrentExtension(True);
        Key := 0;
      end;

    VK_SUBTRACT:
      begin
        if Shift = [ssCtrl] then
          MarkFiles(False)
        else if Shift = [] then
          MarkGroup(False)
        else if Shift = [ssShift] then
          MarkCurrentExtension(False);
        Key := 0;
      end;

    VK_RETURN, VK_SELECT:
      begin
        if (Shift * KeyModifiersShortcut = []) then
        begin
          // Only if there are items in the panel.
          if not IsEmpty then
          begin
            ChooseFile(GetActiveDisplayFile);
            Key := 0;
          end;
        end
        // execute active file in terminal (Shift+Enter)
        else if (Shift * KeyModifiersShortcut = [ssShift]) then
        begin
          if IsActiveItemValid then
          begin
            mbSetCurrentDir(CurrentPath);
            ExecCmdFork(CurrentPath + GetActiveDisplayFile.FSFile.Name, True, gRunInTerm);
            Key := 0;
          end;
        end;
      end;
  end;
end;

function TFileView.FileListLoaded: Boolean;
begin
  Result := Assigned(FAllDisplayFiles);
end;

function TFileView.GetNotebookPage: TCustomPage;
begin
  if Parent is TCustomPage then
    Result := Parent as TCustomPage
  else
    Result := nil;
end;

procedure TFileView.AddFile(const FileName, APath: String; NewFilesPosition: TNewFilesPosition; UpdatedFilesPosition: TUpdatedFilesPosition);
var
  ADisplayFile: TDisplayFile;
  AFile: TFile;
  I: Integer;
begin
  I := FHashedNames.Find(FileName);
  if I < 0 then
  begin
    AFile := TFile.Create(APath);
    AFile.Name := FileName;
    try
      FileSource.RetrieveProperties(AFile, FilePropertiesNeeded);
    except
      on EFileSourceException do
        begin
          FreeAndNil(AFile);
          Exit;
        end;
    end;
    ADisplayFile := TDisplayFile.Create(AFile);
    FHashedFiles.Add(ADisplayFile, nil);
    FHashedNames.Add(FileName, ADisplayFile);
    InsertFile(ADisplayFile, FAllDisplayFiles, NewFilesPosition);
    if not TFileListBuilder.MatchesFilter(ADisplayFile.FSFile, FileFilter, FFilterOptions) then
    begin
      InsertFile(ADisplayFile, FFiles, NewFilesPosition);
      VisualizeFileUpdate(ADisplayFile);
      Notify([fvnFileSourceFileListChanged, fvnDisplayFileListChanged]);
    end
    else
      Notify([fvnFileSourceFileListChanged]);
  end
  else
    UpdateFile(FileName, APath, NewFilesPosition, UpdatedFilesPosition);
end;

procedure TFileView.RemoveFile(const FileName: String);
var
  I: Integer;
begin
  I := FHashedNames.Find(FileName);
  if I >= 0 then
    RemoveFile(TDisplayFile(FHashedNames.List[I]^.Data));
end;

procedure TFileView.RemoveFile(ADisplayFile: TDisplayFile);
begin
  FHashedNames.Remove(ADisplayFile.FSFile.Name);
  FHashedFiles.Remove(ADisplayFile);
  FFiles.Remove(ADisplayFile);
  FAllDisplayFiles.Remove(ADisplayFile);
  if Assigned(FRecentlyUpdatedFiles) then
    FRecentlyUpdatedFiles.Remove(ADisplayFile);
  Notify([fvnFileSourceFileListChanged, fvnDisplayFileListChanged]);
end;

procedure TFileView.RenameFile(const NewFileName, OldFileName, APath: String; NewFilesPosition: TNewFilesPosition; UpdatedFilesPosition: TUpdatedFilesPosition);
var
  ADisplayFile: TDisplayFile;
  OldIndex, NewIndex, FilteredFilesIndex: Integer;
  bFilter: Boolean;
begin
  OldIndex := FHashedNames.Find(OldFileName);
  NewIndex := FHashedNames.Find(NewFileName);
  if OldIndex >= 0 then
  begin
    ADisplayFile := TDisplayFile(FHashedNames.List[OldIndex]^.Data);
    if NewIndex < 0 then
    begin
      ADisplayFile.FSFile.Name := NewFileName;
      FHashedNames.Remove(OldFileName);
      FHashedNames.Add(NewFileName, ADisplayFile);
      ADisplayFile.IconID := -1;
      ADisplayFile.IconOverlayID := -1;
      ADisplayFile.DisplayStrings.Clear;
      ResortFile(ADisplayFile, FAllDisplayFiles);
      bFilter := TFileListBuilder.MatchesFilter(ADisplayFile.FSFile, FileFilter, FFilterOptions);
      FilteredFilesIndex := FFiles.Find(ADisplayFile);
      if FilteredFilesIndex >= 0 then
      begin
        if not bFilter then
        begin
          if GetActiveDisplayFile = ADisplayFile then
            RequestedActiveFile := ADisplayFile.FSFile.FullPath;
          ResortFile(ADisplayFile, FFiles);
          VisualizeFileUpdate(ADisplayFile);
        end
        else
        begin
          FFiles.Delete(FilteredFilesIndex);
          if Assigned(FRecentlyUpdatedFiles) then
            FRecentlyUpdatedFiles.Remove(ADisplayFile);
        end;
      end
      else if not bFilter then
      begin
        InsertFile(ADisplayFile, FFiles, NewFilesPosition);
        VisualizeFileUpdate(ADisplayFile);
      end;
      Notify([fvnFileSourceFileListChanged, fvnDisplayFileListChanged]);
    end
    else
    begin
      RemoveFile(ADisplayFile);
      UpdateFile(NewFileName, APath, NewFilesPosition, UpdatedFilesPosition);
    end;
  end
  else
  begin
    if NewIndex < 0 then
      AddFile(NewFileName, APath, NewFilesPosition, UpdatedFilesPosition)
    else
      UpdateFile(NewFileName, APath, NewFilesPosition, UpdatedFilesPosition);
  end;
end;

procedure TFileView.Request(NewRequests: TFileViewRequests);
begin
  FRequests := FRequests + NewRequests;
  if FUpdateCount = 0 then
    HandleRequests;
end;

procedure TFileView.ResortFile(ADisplayFile: TDisplayFile; AFileList: TDisplayFiles);
var
  I: Integer;
begin
  I := AFileList.Find(ADisplayFile);
  if I >= 0 then
    TDisplayFileSorter.ResortSingle(I, AFileList, SortingForSorter);
end;

procedure TFileView.StartRecentlyUpdatedTimerIfNeeded;
begin
  if Assigned(FRecentlyUpdatedFiles) and (FRecentlyUpdatedFiles.Count > 0) then
  begin
    if not Assigned(FRecentlyUpdatedFilesTimer) then
    begin
      FRecentlyUpdatedFilesTimer := TTimer.Create(Self);
      FRecentlyUpdatedFilesTimer.Interval := 50;
      FRecentlyUpdatedFilesTimer.OnTimer  := @UpdatedFilesTimerEvent;
    end;
    FRecentlyUpdatedFilesTimer.Enabled := True;
  end;
end;

procedure TFileView.UpdateFile(const FileName, APath: String; NewFilesPosition: TNewFilesPosition; UpdatedFilesPosition: TUpdatedFilesPosition);
var
  AFile: TFile;
  ADisplayFile: TDisplayFile;
  I: Integer;

  procedure Resort;
  begin
    ResortFile(ADisplayFile, FAllDisplayFiles);
    ResortFile(ADisplayFile, FFiles);
  end;

begin
  I := FHashedNames.Find(FileName);
  if I >= 0 then
  begin
    ADisplayFile := TDisplayFile(FHashedNames.List[I]^.Data);
    AFile := ADisplayFile.FSFile;
    AFile.ClearProperties;
    try
      FileSource.RetrieveProperties(AFile, FilePropertiesNeeded);
    except
      on EFileNotFound do
        begin
          RemoveFile(ADisplayFile);
          Exit;
        end;
      on EFileSourceException do
        begin
          Exit;
        end;
    end;
    ADisplayFile.DisplayStrings.Clear;

    case UpdatedFilesPosition of
      ufpNoChange: ; // Do nothing
      ufpSameAsNewFiles:
        if NewFilesPosition = nfpSortedPosition then
          Resort
        else
        begin
          FAllDisplayFiles.OwnsObjects := False;
          FAllDisplayFiles.Remove(ADisplayFile); // Remove only temporarily
          FAllDisplayFiles.OwnsObjects := True;
          InsertFile(ADisplayFile, FAllDisplayFiles, NewFilesPosition);
          FFiles.Remove(ADisplayFile);
          InsertFile(ADisplayFile, FFiles, NewFilesPosition);
        end;
      ufpSortedPosition:
        Resort;
      else
        raise Exception.Create('Unsupported UpdatedFilesPosition setting.');
    end;
    VisualizeFileUpdate(ADisplayFile);
    Notify([fvnFileSourceFileListChanged, fvnDisplayFileListChanged]);
  end
  else
    AddFile(FileName, APath, NewFilesPosition, UpdatedFilesPosition);
end;

procedure TFileView.UpdatedFilesTimerEvent(Sender: TObject);
var
  AFile: TDisplayFile;
  i: Integer = 0;
begin
  while i < FRecentlyUpdatedFiles.Count do
  begin
    AFile := FRecentlyUpdatedFiles[i];
    if AFile.RecentlyUpdatedPct = 0 then
    begin
      FRecentlyUpdatedFiles.Delete(i);
    end
    else
    begin
      AFile.RecentlyUpdatedPct := AFile.RecentlyUpdatedPct - 10;
      Inc(i);
      RedrawFile(AFile);
    end;
  end;
  if i = 0 then
    FRecentlyUpdatedFilesTimer.Enabled := False;
end;

procedure TFileView.UpdatePath(UpdateAddressToo: Boolean);
begin
  // Maybe better to do via some notification like FileSourceHasChanged.
  UpdateView;
end;

procedure TFileView.UpdateTitle;
begin
  if Parent is TFileViewPage then
    TFileViewPage(Parent).UpdateTitle;
end;

procedure TFileView.VisualizeFileUpdate(AFile: TDisplayFile);
begin
  if gHighlightUpdatedFiles then
  begin
    if not Assigned(FRecentlyUpdatedFiles) then
      FRecentlyUpdatedFiles := TDisplayFiles.Create(False);
    if FRecentlyUpdatedFiles.Find(AFile) < 0 then
    begin
      FRecentlyUpdatedFiles.Add(AFile);
      AFile.RecentlyUpdatedPct := 100;
    end;
  end;
end;

function TFileView.GetCurrentAddress: String;
begin
  if FileSourcesCount > 0 then
    Result := FileSource.CurrentAddress
  else
    Result := '';
end;

procedure TFileView.AddWorker(const Worker: TFileViewWorker; SetEvents: Boolean = True);
begin
  FFileViewWorkers.Add(Worker);

  if SetEvents then
  begin
    Worker.OnStarting := @WorkerStarting;
    Worker.OnFinished := @WorkerFinished;
  end;
end;

procedure TFileView.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TFileView.CalculateSpace(AFile: TDisplayFile);
var
  AFileList: TFVWorkerFileList;
begin
  AFileList := TFVWorkerFileList.Create;
  try
    if IsItemValid(AFile) and AFile.FSFile.IsDirectory then
      AFileList.AddClone(AFile, AFile);

    CalculateSpace(AFileList);
  finally
    FreeAndNil(AFileList);
  end;
end;

procedure TFileView.CalculateSpace(var AFileList: TFVWorkerFileList);
var
  Worker: TFileViewWorker;
begin
  if GetCurrentWorkType = fvwtCreate then
    Exit;

  if AFileList.Count > 0 then
  begin
    Worker := TCalculateSpaceWorker.Create(
      FileSource,
      WorkersThread,
      @CalculateSpaceOnUpdate,
      AFileList);

    AddWorker(Worker);
    WorkersThread.QueueFunction(@Worker.StartParam);
  end
  else
    FreeAndNil(AFileList);
end;

procedure TFileView.CalculateSpaceOfAllDirectories;
var
  i: Integer;
  AFileList: TFVWorkerFileList;
  AFile: TDisplayFile;
begin
  AFileList := TFVWorkerFileList.Create;
  try
    for i := 0 to FFiles.Count - 1 do
    begin
      AFile := FFiles[i];
      if IsItemValid(AFile) and AFile.FSFile.IsDirectory then
        AFileList.AddClone(AFile, AFile);
    end;

    CalculateSpace(AFileList);
  finally
    FreeAndNil(AFileList);
  end;
end;

procedure TFileView.CalculateSpaceOnUpdate(const UpdatedFile: TDisplayFile; const UserData: Pointer);
var
  OrigDisplayFile: TDisplayFile;
begin
  OrigDisplayFile := TDisplayFile(UserData);

  if not IsReferenceValid(OrigDisplayFile) then
    Exit; // File does not exist anymore (reference is invalid).

  OrigDisplayFile.FSFile.Size := UpdatedFile.FSFile.Size;
  DoFileUpdated(OrigDisplayFile, [fpSize]);
end;

procedure TFileView.DoOnFileListChanged;
begin
  if Assigned(OnFileListChanged) then
    OnFileListChanged(Self);
end;

procedure TFileView.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and
     // This condition prevents endless recursion.
     ((FRequests <> []) or (FNotifications <> [])) then
  begin
    BeginUpdate;
    DisableAutoSizing;
    try
      HandleRequests;
      HandleNotifications;
    finally
      EnableAutoSizing;
      EndUpdate;
    end;
  end;
end;

function TFileView.GetCurrentPath: String;
begin
  Result := FHistory.CurrentPath;
end;

procedure TFileView.SetCurrentPath(NewPath: String);
begin
  if (NewPath <> CurrentPath) and BeforeChangePath(FileSource, NewPath) then
  begin
    EnableWatcher(False);
    FHistory.AddPath(NewPath); // Sets CurrentPath.
    AfterChangePath;
    EnableWatcher(True);
    {$IFDEF DEBUG_HISTORY}
    FHistory.DebugShow;
    {$ENDIF}
  end;
end;

procedure TFileView.SetFlags(AValue: TFileViewFlags);
var
  AddedFlags, RemovedFlags: TFileViewFlags;
begin
  if FFlags = AValue then Exit;

  AddedFlags   := AValue - FFlags;
  RemovedFlags := FFlags - AValue;
  FFlags := AValue;

  if fvfDontWatch in AddedFlags then
    EnableWatcher(False);

  if ([fvfDelayLoadingFiles, fvfDontLoadFiles] * RemovedFlags <> []) then
  begin
    if not (FileListLoaded or (GetCurrentWorkType = fvwtCreate)) then
      Reload;
    EnableWatcher(True);
  end;

  if fvfDontWatch in RemovedFlags then
    EnableWatcher(True);
end;

function TFileView.CloneActiveFile: TFile;
var
  aFile: TDisplayFile;
begin
  aFile := GetActiveDisplayFile;

  if Assigned(aFile) then
    Result := aFile.FSFile.Clone
  else
    Result := nil;
end;

function TFileView.CloneFiles: TFiles;
var
  i: Integer;
begin
  Result := TFiles.Create(CurrentPath);

  for i := 0 to FFiles.Count - 1 do
  begin
    Result.Add(FFiles[i].FSFile.Clone);
  end;
end;

function TFileView.CloneSelectedFiles: TFiles;
var
  i: Integer;
  aFile: TDisplayFile;
begin
  Result := TFiles.Create(CurrentPath);

  for i := 0 to FFiles.Count - 1 do
  begin
    if FFiles[i].Selected then
      Result.Add(FFiles[i].FSFile.Clone);
  end;

  // If no files are selected, add currently active file if it is valid.
  if (Result.Count = 0) then
  begin
    aFile := GetActiveDisplayFile;
    if IsItemValid(aFile) then
      Result.Add(aFile.FSFile.Clone);
  end;
end;

function TFileView.GetWorkersThread: TFunctionThread;
begin
  if not Assigned(FWorkersThread) then
    FWorkersThread := TFunctionThread.Create(False);
  Result := FWorkersThread;
end;

procedure TFileView.SaveSelection;
var
  I: Integer;
begin
  FSavedSelection.Clear;
  for I := 0 to FFiles.Count - 1 do
    with FFiles[I] do
    begin
      if Selected then
        FSavedSelection.Add(FSFile.Name);
    end;
end;

procedure TFileView.SaveSelectionToFile(const AFileName: String);
begin
  with dmComData do
  begin
    SaveDialog.DefaultExt := '.txt';
    SaveDialog.Filter     := '*.txt|*.txt';
    SaveDialog.FileName   := AFileName;
    if (AFileName <> EmptyStr) or SaveDialog.Execute then
      try
        SaveSelection;
        FSavedSelection.SaveToFile(SaveDialog.FileName);
      except
        on E: Exception do
          msgError(rsMsgErrSaveFile + '-' + E.Message);
      end;
  end;
end;

procedure TFileView.RestoreSelection;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to FFiles.Count - 1 do
      with FFiles[I] do
        Selected:= (FSavedSelection.IndexOf(FSFile.Name) >= 0);
    Notify([fvnSelectionChanged]);
  finally
    EndUpdate;
  end;
end;

procedure TFileView.InvertFileSelection(AFile: TDisplayFile; bNotify: Boolean = True);
begin
  MarkFile(AFile, not AFile.Selected, bNotify);
end;

procedure TFileView.InvertAll;
var
  i: Integer;
begin
  BeginUpdate;
  try
    for i := 0 to FFiles.Count-1 do
      InvertFileSelection(FFiles[i]);
  finally
    EndUpdate;
  end;
end;

procedure TFileView.MarkFile(AFile: TDisplayFile; bSelect: Boolean; bNotify: Boolean = True);
begin
  // Don't check if valid when just unselecting.
  if not bSelect then
  begin
    if not Assigned(AFile) then
      Exit;
  end
  else if not IsItemValid(AFile) then
    Exit;

  AFile.Selected := bSelect;
  if bNotify then
    Notify([fvnSelectionChanged]);
end;

procedure TFileView.MarkFiles(bSelect: Boolean);
var
  i: Integer;
begin
  BeginUpdate;
  try
    for i := 0 to FFiles.Count - 1 do
      MarkFile(FFiles[i], bSelect);
  finally
    EndUpdate;
  end;
end;

procedure TFileView.MarkGroup(const sMask: String; bSelect: Boolean);
var
  I: Integer;
  SearchTemplate: TSearchTemplate = nil;
  bSelected: Boolean = False;
begin
  BeginUpdate;
  try
    if IsMaskSearchTemplate(sMask) then
      begin
        SearchTemplate:= gSearchTemplateList.TemplateByName[sMask];
        if Assigned(SearchTemplate) then
          for I := 0 to FFiles.Count - 1 do
            begin
              if FFiles[I].FSFile.Name = '..' then Continue;
              if SearchTemplate.CheckFile(FFiles[I].FSFile) then
                begin
                  FFiles[I].Selected := bSelect;
                  bSelected := True;
                end;
            end;
      end
    else
      for I := 0 to FFiles.Count - 1 do
        begin
          if FFiles[I].FSFile.Name = '..' then Continue;
          if MatchesMaskList(FFiles[I].FSFile.Name, sMask) then
            begin
              FFiles[I].Selected := bSelect;
              bSelected := True;
            end;
        end;

    if bSelected then
      Notify([fvnSelectionChanged]);
  finally
    EndUpdate;
  end;
end;

procedure TFileView.MarkGroup(bSelect: Boolean);
var
  s, ADlgTitle: String;
begin
  if not IsEmpty then
  begin
    if bSelect then
      ADlgTitle := rsMarkPlus
    else
      ADlgTitle := rsMarkMinus;
    s := FLastMark;
    if ShowMaskInputDlg(ADlgTitle, rsMaskInput, glsMaskHistory, s) then
    begin
      FLastMark := s;
      MarkGroup(s, bSelect);
    end;
  end;
end;

procedure TFileView.MarkCurrentExtension(bSelect: Boolean);
var
  sGroup: String;
begin
  if IsActiveItemValid then
  begin
    sGroup := GetActiveDisplayFile.FSFile.Extension;
    if sGroup <> '' then
      sGroup := '.' + sGroup;
    MarkGroup('*' + sGroup, bSelect);
  end;
end;

function TFileView.IsVisibleToUser: Boolean;
begin
  if NotebookPage is TFileViewPage then
    Result := TFileViewPage(NotebookPage).IsActive
  else
    Result := True;
end;

procedure TFileView.PropertiesRetrieverOnUpdate(const UpdatedFile: TDisplayFile; const UserData: Pointer);
var
  propType: TFilePropertyType;
  aFile: TFile;
  OrigDisplayFile: TDisplayFile;
begin
  OrigDisplayFile := TDisplayFile(UserData);

  if not IsReferenceValid(OrigDisplayFile) then
    Exit; // File does not exist anymore (reference is invalid).

  aFile := OrigDisplayFile.FSFile;

{$IF (fpc_version>2) or ((fpc_version=2) and (fpc_release>4))}
  // This is a bit faster.
  for propType in UpdatedFile.FSFile.AssignedProperties - aFile.AssignedProperties do
{$ELSE}
  for propType := Low(TFilePropertyType) to High(TFilePropertyType) do
    if (propType in UpdatedFile.FSFile.AssignedProperties) and
       (not (propType in aFile.AssignedProperties)) then
{$ENDIF}
    begin
      aFile.Properties[propType] := UpdatedFile.FSFile.ReleaseProperty(propType);
    end;

  if UpdatedFile.IconID <> -1 then
    OrigDisplayFile.IconID := UpdatedFile.IconID;

  if UpdatedFile.IconOverlayID <> -1 then
    OrigDisplayFile.IconOverlayID := UpdatedFile.IconOverlayID;

  DoFileUpdated(OrigDisplayFile);
end;

procedure TFileView.SetActiveFile(const aFile: TFile);
begin
end;

procedure TFileView.SetActiveFile(aFilePath: String);
begin
end;

procedure TFileView.SetActive(bActive: Boolean);
begin
  FActive := bActive;
end;

procedure TFileView.SetSorting(const NewSortings: TFileSortings);
begin
  FSortings := CloneSortings(NewSortings);
end;

procedure TFileView.SortAllDisplayFiles;
begin
  TDisplayFileSorter.Sort(FAllDisplayFiles, SortingForSorter);
end;

procedure TFileView.MakeFileSourceFileList;
var
  Worker: TFileViewWorker;
  AThread: TFunctionThread = nil;
  ClonedDisplayFiles: TDisplayFiles = nil;
  DisplayFilesHashed: TStringHashList = nil;
  i: Integer;
begin
  if (csDestroying in ComponentState) or (FileSourcesCount = 0) or
     ([fvfDelayLoadingFiles, fvfDontLoadFiles] * Flags <> []) then
    Exit;

  {$IFDEF timeFileView}
  filelistLoaderTime := Now;
  DCDebug('--------- Start ---------');
  {$ENDIF}

  StopWorkers;

  if gListFilesInThread then
    AThread := GetWorkersThread;

  if FileSource.Equals(FLastLoadedFileSource) and
     (FLastLoadedPath = CurrentPath) and
     (FAllDisplayFiles.Count > 0) then
  begin
    // Clone all properties of display files, but don't clone the FS files
    // themselves because new ones will be retrieved from FileSource.
    ClonedDisplayFiles := FAllDisplayFiles.Clone(False);
    DisplayFilesHashed := TStringHashList.Create(True);
    // Map filename to display file.
    for i := 0 to FAllDisplayFiles.Count - 1 do
      DisplayFilesHashed.Add(FAllDisplayFiles[i].FSFile.Name, ClonedDisplayFiles[i]);
  end;

  Worker := TFileListBuilder.Create(
    FileSource,
    FileSourcesCount,
    FileFilter,
    FilterOptions,
    CurrentPath,
    SortingForSorter,
    AThread,
    FilePropertiesNeeded,
    @SetFileList,
    ClonedDisplayFiles,
    DisplayFilesHashed);

  AddWorker(Worker);

  ClearPendingFilesChanges;

  if gListFilesInThread then
  begin
    // Clear files.
    if Assigned(FAllDisplayFiles) then
    begin
      ClearRecentlyUpdatedFiles;
      FFiles.Clear;
      FAllDisplayFiles.Clear; // Clear references to files from the source.
      HashFileList;
    end;

    BeforeMakeFileList;
    AThread.QueueFunction(@Worker.StartParam);
  end
  else
  begin
    BeforeMakeFileList;
    Worker.Start;
  end;
end;

procedure TFileView.AfterMakeFileList;
begin
end;

procedure TFileView.BeforeMakeFileList;
begin
end;

function TFileView.BeginDragExternal(DragFile: TDisplayFile; DragDropSource: uDragDropEx.TDragDropSource; MouseButton: TMouseButton; ScreenStartPoint: TPoint): Boolean;
var
  fileNamesList: TStringList;
  i: Integer;
begin
  Result := False;

  if Assigned(DragDropSource) then
  begin
    fileNamesList := TStringList.Create;
    try
      if IsItemValid(DragFile) = True then
      begin
        for i := 0 to FFiles.Count-1 do
        begin
          if FFiles[i].Selected then
            fileNamesList.Add(FFiles[i].FSFile.FullPath);
        end;

        // If there were no files selected add the dragged file.
        if fileNamesList.Count = 0 then
          fileNamesList.Add(DragFile.FSFile.FullPath);

        // Initiate external drag&drop operation.
        Result := DragDropSource.DoDragDrop(fileNamesList, MouseButton, ScreenStartPoint);

        // Refresh source file panel after drop to (possibly) another application
        // (files could have been moved for example).
        // 'draggedFileItem' is invalid after this.
        Reload;
      end;

    finally
      FreeAndNil(fileNamesList);
    end;
  end;
end;

procedure TFileView.ChooseFile(const AFile: TDisplayFile; FolderMode: Boolean = False);
var
  FSFile: TFile;
begin
  if Assigned(AFile) then
  begin
    FSFile := AFile.FSFile.Clone;
    try
      if FSFile.Name = '..' then
        ChangePathToParent(True)
      else if FSFile.IsLinkToDirectory then
        ChooseSymbolicLink(Self, FSFile)
      else if FSFile.IsDirectory then
        ChangePathToChild(FSFile)
      else if not FolderMode then
        try
          uFileSourceUtil.ChooseFile(Self, FSFile);
        except
          on e: EInvalidCommandLine do
            MessageDlg(rsMsgInvalidCommandLine, rsMsgInvalidCommandLine + ': ' + e.Message, mtError, [mbOK], 0);
          on e: Exception do
            MessageDlg('Error', e.Message, mtError, [mbOK], 0);
        end;
    finally
      FSFile.Free;
    end;
  end;
end;

procedure TFileView.DoSelectionChanged;
begin
  // Empty.
end;

procedure TFileView.DoUpdateView;
begin
  // Empty.
end;

function TFileView.GetCurrentWorkType: TFileViewWorkType;
var
  i: Integer;
begin
  if Assigned(FFileViewWorkers) then
  begin
    for i := 0 to FFileViewWorkers.Count - 1 do
      if FFileViewWorkers[i].Working then
        Exit(FFileViewWorkers[i].WorkType);
  end;
  Result := fvwtNone;
end;

procedure TFileView.HashFileList;
var
  i: Integer;
begin
  // Cannot use FHashedFiles.Clear because it also destroys the buckets.
  FHashedFiles.Free;
  // TBucketList seems to do fairly well without needing a proper hash table.
  FHashedFiles := TBucketList.Create(bl256);
  FHashedNames.Clear;
  for i := 0 to FAllDisplayFiles.Count - 1 do
  begin
    FHashedFiles.Add(FAllDisplayFiles[i], nil);
    FHashedNames.Add(FAllDisplayFiles[i].FSFile.Name, FAllDisplayFiles[i]);
  end;
end;

procedure TFileView.InsertFile(ADisplayFile: TDisplayFile; AFileList: TDisplayFiles; NewFilesPosition: TNewFilesPosition);

  procedure InsertAfterUpDir;
  var
    i, InsertPos: Integer;
  begin
    InsertPos := AFileList.Count;
    for i := 0 to AFileList.Count - 1 do
    begin
      if (AFileList[i].FSFile.Name <> '..') and
         (AFileList[i].FSFile.Name <> '.') then
      begin
        InsertPos := i;
        Break;
      end;
    end;
    AFileList.List.Insert(InsertPos, ADisplayFile);
  end;

  procedure InsertIntoSortedPosition;
  begin
    TDisplayFileSorter.InsertSort(ADisplayFile, AFileList, SortingForSorter);
  end;

var
  EmptySortings: TFileSortings = nil;
begin
  if ADisplayFile.FSFile.IsDirectory or ADisplayFile.FSFile.IsLinkToDirectory then
    InsertIntoSortedPosition
  else
    case NewFilesPosition of
      nfpTop:
        InsertAfterUpDir;
      nfpTopAfterDirectories:
        if gSortFolderMode <> sfmSortLikeFile then
          // Will only sort by directory attribute.
          TDisplayFileSorter.InsertSort(ADisplayFile, AFileList, EmptySortings, True)
        else
          InsertIntoSortedPosition;
      nfpSortedPosition:
        InsertIntoSortedPosition;
      nfpBottom:
        AFileList.Add(ADisplayFile);
      else
        raise Exception.Create('Unsupported NewFilesPosition setting.');
    end;
end;

function TFileView.HasSelectedFiles: Boolean;
var
  i: Integer;
begin
  for i := 0 to FFiles.Count - 1 do
  begin
    if FFiles[i].Selected then
      Exit(True);
  end;
  Result := False;
end;

function TFileView.IsActiveItemValid:Boolean;
begin
  Result := IsItemValid(GetActiveDisplayFile);
end;

function TFileView.IsReferenceValid(aFile: TDisplayFile): Boolean;
begin
  Result := FHashedFiles.Exists(aFile);
end;

function TFileView.IsEmpty: Boolean;
begin
  Result := (FFiles.Count = 0);
end;

function TFileView.IsItemValid(AFile: TDisplayFile): Boolean;
begin
  if Assigned(AFile) and (AFile.FSFile.Name <> '..') then
    Result := True
  else
    Result := False;
end;

function TFileView.Reload(const PathsToReload: TPathsArray = nil): Boolean;
var
  i: Integer;
begin
  if csDestroying in ComponentState then
    Exit(False);

  if Assigned(PathsToReload) then
  begin
    Result := False;
    for i := Low(PathsToReload) to High(PathsToReload) do
      if IsInPath(PathsToReload[i], CurrentPath, True, True) then
      begin
        Result := True;
        Break;
      end;

    if not Result then
      Exit;
  end;

  if FReloadTimer.Enabled then
  begin
    // Reload is already scheduled.
    Result := True;
  end
  else if CheckIfDelayReload then
  begin
    // Delay reloading.
    Result := False;
    FReloadNeeded := True;
  end
  else
  begin
    if GetCurrentWorkType = fvwtCreate then
    begin
      Result := False;

      // Allow interrupting loading a few times.
      if FLoadFilesNoDelayCount < 2 then
      begin
        Inc(FLoadFilesNoDelayCount);
        DoReload;
      end
      else
      begin
        // Let current loading finish and another will be scheduled after delay via timer.
        FReloadNeeded := True;
      end;
    end
    else
    begin
      Result := True;

      if DateTimeToTimeStamp(SysUtils.Now - FLoadFilesFinishTime).Time > MinimumReloadInterval then
      begin
        FLoadFilesNoDelayCount := 0;
        DoReload;
      end
      // Allow a few reloads in quick succession.
      else if FLoadFilesNoDelayCount < 4 then
      begin
        Inc(FLoadFilesNoDelayCount);
        DoReload;
      end
      else
      begin
        FReloadTimer.Interval := MinimumReloadInterval;
        FReloadTimer.Enabled  := True;
      end;
    end;
  end;
end;

function TFileView.Reload(const PathToReload: String): Boolean;
var
  Paths: TPathsArray;
begin
  SetLength(Paths, 1);
  Paths[0] := PathToReload;
  Result := Reload(Paths);
end;

procedure TFileView.ReloadIfNeeded;
begin
  if FReloadNeeded then
    Reload;
end;

procedure TFileView.StopWorkers;
var
  i: Integer = 0;
begin
  // Abort any working workers and destroy those that have finished.
  while i < FFileViewWorkers.Count do
  begin
    if FFileViewWorkers[i].CanBeDestroyed then
    begin
      FFileViewWorkers[i].Free;
      FFileViewWorkers.Delete(i);
    end
    else
    begin
      if FFileViewWorkers[i].Working then
        FFileViewWorkers[i].Abort;
      Inc(i);
    end;
  end;
end;

procedure TFileView.LoadConfiguration(Section: String; TabIndex: Integer);
begin
  // Empty. For backward compatibility with loading from INI.
end;

procedure TFileView.SaveConfiguration(Section: String; TabIndex: Integer);
begin
  // Empty. For backward compatibility with saving to INI.
end;

procedure TFileView.LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
var
  HistoryNode, EntryNode, FSNode, PathsNode: TXmlNode;
  SortingsNode, SortingSubNode, SortFunctionNode: TXmlNode;
  sFSType, sPath: String;
  aFileSource: IFileSource = nil;
  ActiveFSIndex: Integer = -1;
  ActivePathIndex: Integer = -1;
  NewSorting: TFileSortings = nil;
  SortDirection: TSortDirection;
  SortFunctions: TFileFunctions;
  SortFunctionInt: Integer;
begin
  RemoveAllFileSources;

  // Sorting.
  SortingsNode := AConfig.FindNode(ANode, 'Sortings');
  if Assigned(SortingsNode) then
  begin
    SortingSubNode := SortingsNode.FirstChild;
    while Assigned(SortingSubNode) do
    begin
      if SortingSubNode.CompareName('Sorting') = 0 then
      begin
        if AConfig.TryGetValue(SortingSubNode, 'Direction', Integer(SortDirection)) then
        begin
          SortFunctions := nil;
          SortFunctionNode := SortingSubNode.FirstChild;
          while Assigned(SortFunctionNode) do
          begin
            if SortFunctionNode.CompareName('Function') = 0 then
            begin
              if TryStrToInt(AConfig.GetContent(SortFunctionNode), SortFunctionInt) then
                AddSortFunction(SortFunctions, TFileFunction(SortFunctionInt));
            end;
            SortFunctionNode := SortFunctionNode.NextSibling;
          end;
          AddSorting(NewSorting, SortFunctions, SortDirection);
        end;
      end;
      SortingSubNode := SortingSubNode.NextSibling;
    end;
  end;
  FSortings := NewSorting; // SetSorting not needed here, will be called in UpdateView

  // History.
  HistoryNode := AConfig.FindNode(ANode, 'History');
  if Assigned(HistoryNode) then
  begin
    EntryNode := HistoryNode.FirstChild;
    while Assigned(EntryNode) do
    begin
      if EntryNode.CompareName('Entry') = 0 then
      begin
        FSNode := EntryNode.FindNode('FileSource');
        if Assigned(FSNode) then
        begin
          if AConfig.TryGetAttr(FSNode, 'Type', sFSType) then
          begin
            // Create file source based on saved configuration or create empty and
            // allow it to read its configuration from FSNode.
            if sFSType = 'FileSystem' then
              aFileSource := TFileSystemFileSource.GetFileSource;

            if Assigned(aFileSource) then
            begin
              FHistory.AddFileSource(aFileSource);

              // Load paths history.
              PathsNode := AConfig.FindNode(EntryNode, 'Paths');
              if Assigned(PathsNode) then
              begin
                PathsNode := PathsNode.FirstChild;
                while Assigned(PathsNode) do
                begin
                  if PathsNode.CompareName('Path') = 0 then
                  begin
                    sPath := AConfig.GetContent(PathsNode);

                    // Go to upper directory if it doesn't exist (filesystem only for now).
                    if aFileSource.IsInterface(IFileSystemFileSource) then
                    begin
                      sPath := GetDeepestExistingPath(sPath);
                    end;

                    if sPath <> EmptyStr then
                    begin
                      FHistory.AddPath(sPath);

                      if AConfig.GetAttr(PathsNode, 'Active', False) then
                        ActivePathIndex := FHistory.PathsCount[FHistory.Count - 1] - 1;
                    end;
                  end;
                  PathsNode := PathsNode.NextSibling;
                end;
              end;

              // Delete the file source if no paths loaded.
              if FHistory.PathsCount[FHistory.Count - 1] = 0 then
                FHistory.DeleteFromCurrentFileSource
              else
              begin
                // Check if the current history entry is active.
                if AConfig.GetAttr(EntryNode, 'Active', False) then
                  ActiveFSIndex := FHistory.Count - 1;
              end;
            end;
          end;
        end;
      end;
      EntryNode := EntryNode.NextSibling;
    end;
  end;

  // Set current history position.
  if (ActiveFSIndex < 0) or (ActiveFSIndex > FHistory.Count - 1) then
    ActiveFSIndex := FHistory.Count - 1;
  if ActiveFSIndex <> -1 then
  begin
    if (ActivePathIndex < 0) or (ActivePathIndex > FHistory.PathsCount[ActiveFSIndex] - 1) then
      ActivePathIndex := FHistory.PathsCount[ActiveFSIndex] - 1;
  end
  else
    ActivePathIndex := -1;
  FHistory.SetIndexes(ActiveFSIndex, ActivePathIndex);

  if Assigned(FileSource) then
    FileSource.AddReloadEventListener(@ReloadEvent);
  // No automatic reload here.
end;

procedure TFileView.LoadSelectionFromClipboard;
begin
  FSavedSelection.Text:= Clipboard.AsText;
  RestoreSelection;
end;

procedure TFileView.LoadSelectionFromFile(const AFileName: String);
begin
  with dmComData do
  begin
    OpenDialog.DefaultExt := '.txt';
    OpenDialog.Filter     := '*.txt|*.txt';
    OpenDialog.FileName   := AFileName;
    if ((AFileName <> EmptyStr) and mbFileExists(AFileName)) or OpenDialog.Execute then
      try
        FSavedSelection.LoadFromFile(OpenDialog.FileName);
        RestoreSelection;
      except
        on E: Exception do
          msgError(rsMsgErrEOpen + '-' + E.Message);
      end;
  end;
end;

procedure TFileView.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
var
  HistoryNode, EntryNode, FSNode, PathsNode, PathNode: TXmlNode;
  SortingsNode, SortingSubNode: TXmlNode;
  i, j: Integer;
  PathIndex: Integer;
  ASorting: TFileSortings;
begin
  AConfig.ClearNode(ANode);

  // Sorting.
  ASorting := Sorting;
  if Length(ASorting) > 0 then
  begin
    SortingsNode := AConfig.FindNode(ANode, 'Sortings', True);
    for i := Low(ASorting) to High(ASorting) do
    begin
      SortingSubNode := AConfig.AddNode(SortingsNode, 'Sorting');
      AConfig.AddValue(SortingSubNode, 'Direction', Integer(ASorting[i].SortDirection));
      for j := Low(ASorting[i].SortFunctions) to High(ASorting[i].SortFunctions) do
        AConfig.AddValue(SortingSubNode, 'Function', Integer(ASorting[i].SortFunctions[j]));
    end;
  end;

  // History.
  HistoryNode := AConfig.FindNode(ANode, 'History', True);
  AConfig.ClearNode(HistoryNode);

  for i := 0 to FileSourcesCount - 1 do
  begin
    // Currently saves only FileSystem.

    if TFileSystemFileSource.ClassNameIs(FHistory.FileSource[i].ClassName) then
    begin
      EntryNode := AConfig.AddNode(HistoryNode, 'Entry');
      if FHistory.CurrentFileSourceIndex = i then
        AConfig.SetAttr(EntryNode, 'Active', True);

      FSNode := AConfig.AddNode(EntryNode, 'FileSource');
      AConfig.SetAttr(FSNode, 'Type', 'FileSystem');

      // Save paths history.
      PathsNode := AConfig.AddNode(EntryNode, 'Paths');
      if gSaveDirHistory then
      begin
        for j := 0 to FHistory.PathsCount[i] - 1 do
        begin
          PathNode := AConfig.AddNode(PathsNode, 'Path');

          // Mark path as active (don't need to if it is the last one).
          if (FHistory.CurrentFileSourceIndex = i) and
             (FHistory.CurrentPathIndex = j) and
             (j < FHistory.PathsCount[i] - 1) then
          begin
            AConfig.SetAttr(PathNode, 'Active', True);
          end;

          AConfig.SetContent(PathNode, FHistory.Path[i, j]);
        end;
      end
      else
      begin
        if FHistory.CurrentFileSourceIndex = i then
          PathIndex := FHistory.CurrentPathIndex
        else
          PathIndex := FHistory.PathsCount[i] - 1;

        AConfig.AddValue(PathsNode, 'Path', FHistory.Path[i, PathIndex]);
      end;
    end;
  end;
end;

procedure TFileView.UpdateView;
var
  bLoadingFilelist: Boolean;
begin
  bLoadingFilelist := GetCurrentWorkType = fvwtCreate;
  StopWorkers;

  DoUpdateView;

  if bLoadingFilelist then
    MakeFileSourceFileList
  else
  begin
    // Always recreate file list because things like ignore list might have changed.
    if Assigned(FAllDisplayFiles) then
      Request([fvrqMakeDisplayFileList]);
  end;

  EnableWatcher(IsFileSystemWatcher);
  UpdateTitle;
end;

function TFileView.BeforeChangePath(NewFileSource: IFileSource; NewPath: String): Boolean;
begin
  if NewPath <> '' then
  begin
    if Assigned(OnBeforeChangePath) then
      if not OnBeforeChangePath(Self, NewFileSource, NewPath) then
        Exit(False);

    if Assigned(NewFileSource) and not NewFileSource.SetCurrentWorkingDirectory(NewPath) then
    begin
      msgError(Format(rsMsgChDirFailed, [NewPath]));
      Exit(False);
    end;

    Result := True;
  end
  else
    Result := False;
end;

procedure TFileView.AfterChangePath;
begin
  LastActiveFile := '';
  RequestedActiveFile := '';

  FReloadNeeded := False;
  FReloadTimer.Enabled := False;
  FLoadFilesStartTime := 0;
  FLoadFilesFinishTime := 0;
  FLoadFilesNoDelayCount := 0;

  if Assigned(OnAfterChangePath) then
    OnAfterChangePath(Self);

  UpdateTitle;

  MakeFileSourceFileList;
end;

procedure TFileView.ChangePathToParent(AllowChangingFileSource: Boolean);
var
  PreviousSubDirectory,
  sUpLevel: String;
begin
  // Check if this is root level of the current file source.
  if FileSource.IsPathAtRoot(CurrentPath) then
  begin
    // If there is a higher level file source then change to it.
    if (FileSourcesCount > 1) and AllowChangingFileSource then
    begin
      RemoveCurrentFileSource;
    end;
  end
  else
  begin
    PreviousSubDirectory := ExtractFileName(ExcludeTrailingPathDelimiter(CurrentPath));

    sUpLevel:= FileSource.GetParentDir(CurrentPath);
    if sUpLevel <> EmptyStr then
    begin
      CurrentPath := sUpLevel;
      SetActiveFile(PreviousSubDirectory);
    end;
  end;
end;

procedure TFileView.ChangePathToChild(const aFile: TFile);
begin
  if Assigned(aFile) and aFile.IsNameValid and
     (aFile.IsDirectory or aFile.IsLinkToDirectory) then
  begin
    CurrentPath := CurrentPath + IncludeTrailingPathDelimiter(aFile.Name);
  end;
end;

procedure TFileView.ExecuteCommand(CommandName: String; const Params: array of String);
begin
  FMethods.ExecuteCommand(CommandName, Params);
end;

procedure TFileView.AddFileSource(aFileSource: IFileSource; aPath: String);
var
  IsNewFileSource: Boolean;
begin
  IsNewFileSource := not aFileSource.Equals(FileSource);

  if BeforeChangePath(aFileSource, aPath) then
  begin
    if Assigned(FileSource) and IsNewFileSource then
      FileSource.RemoveReloadEventListener(@ReloadEvent);
    EnableWatcher(False);

    FHistory.Add(aFileSource, aPath);

    if Assigned(FileSource) and IsNewFileSource then
    begin
      UpdatePath(True);
      FileSource.AddReloadEventListener(@ReloadEvent);
    end;

    AfterChangePath;
    EnableWatcher(True);

    {$IFDEF DEBUG_HISTORY}
    FHistory.DebugShow;
    {$ENDIF}
  end;
end;

procedure TFileView.RemoveCurrentFileSource;
var
  NewFileSource: IFileSource = nil;
  NewPath: String = '';
  IsNewFileSource: Boolean;
  PrevIndex: Integer;
  FocusedFile: String;
begin
  if FileSourcesCount > 0 then
  begin
    // TODO: Do this by remembering focused file name in a list?
    FocusedFile := ExtractFileName(FileSource.CurrentAddress);

    PrevIndex := FHistory.CurrentFileSourceIndex - 1;
    if PrevIndex >= 0 then
    begin
      NewFileSource := FHistory.FileSource[PrevIndex];
      NewPath := FHistory.Path[PrevIndex, FHistory.PathsCount[PrevIndex] - 1];
    end;
    IsNewFileSource := not NewFileSource.Equals(FileSource);

    if BeforeChangePath(NewFileSource, NewPath) then
    begin
      if IsNewFileSource then
        FileSource.RemoveReloadEventListener(@ReloadEvent);
      EnableWatcher(False);

      FHistory.DeleteFromCurrentFileSource;

      if Assigned(FileSource) and IsNewFileSource then
      begin
        UpdatePath(True);
        FileSource.AddReloadEventListener(@ReloadEvent);
      end;

      AfterChangePath;
      EnableWatcher(True);

      SetActiveFile(FocusedFile);

      {$IFDEF DEBUG_HISTORY}
      FHistory.DebugShow;
      {$ENDIF}
    end;
  end;
end;

procedure TFileView.RemoveAllFileSources;
begin
  if FileSourcesCount > 0 then
  begin
    FileSource.RemoveReloadEventListener(@ReloadEvent);
    EnableWatcher(False);
    FHistory.Clear;

    if not (csDestroying in ComponentState) then AfterChangePath;

    {$IFDEF DEBUG_HISTORY}
    FHistory.DebugShow;
    {$ENDIF}
  end;
end;

procedure TFileView.AssignFileSources(const otherFileView: TFileView);
begin
  FileSource.RemoveReloadEventListener(@ReloadEvent);
  EnableWatcher(False);
  FHistory.Assign(otherFileView.FHistory);
  UpdatePath(True);
  FileSource.AddReloadEventListener(@ReloadEvent);
  AfterChangePath;
  EnableWatcher(True);
end;

function TFileView.GetCurrentFileSource: IFileSource;
begin
  Result := FHistory.CurrentFileSource;
end;

function TFileView.GetCurrentFileSourceIndex: Integer;
begin
  Result := FHistory.CurrentFileSourceIndex;
end;

function TFileView.GetCurrentPathIndex: Integer;
begin
  Result := FHistory.CurrentPathIndex;
end;

function TFileView.GetFileSource(Index: Integer): IFileSource;
begin
  Result := FHistory.FileSource[Index];
end;

function TFileView.GetFileSourcesCount: Integer;
begin
  Result := FHistory.Count;
end;

function TFileView.GetFiltered: Boolean;
begin
  Result := Self.FileFilter <> EmptyStr;
end;

function TFileView.GetPath(FileSourceIndex, PathIndex: Integer): UTF8String;
begin
  with FHistory do
  begin
    if (Count > 0) and (PathIndex >= 0) and (PathIndex < PathsCount[FileSourceIndex]) then
      Result := Path[FileSourceIndex, PathIndex]
    else
      Result := EmptyStr;
  end;
end;

function TFileView.GetPathsCount(FileSourceIndex: Integer): Integer;
begin
  with FHistory do
  begin
    if Count > 0 then
      Result := PathsCount[FileSourceIndex]
    else
      Result := 0;
  end;
end;

function TFileView.GetSortingForSorter: TFileSortings;
begin
  Result := CloneAndAddSortByNameIfNeeded(Sorting);
end;

function TFileView.GetWatcherActive: Boolean;
begin
  Result := FWatchPath <> EmptyStr;
end;

procedure TFileView.HandleNotifications;
begin
  BeginUpdate;
  DisableAutoSizing;
  try
    while FNotifications <> [] do
    begin
      if fvnFileSourceFileListChanged in FNotifications then
      begin
        FNotifications := FNotifications - [fvnFileSourceFileListChanged];
        DoOnFileListChanged;
      end
      else if fvnDisplayFileListChanged in FNotifications then
      begin
        FNotifications := FNotifications - [fvnDisplayFileListChanged];
        AfterMakeFileList;
        StartRecentlyUpdatedTimerIfNeeded;
      end
      else if fvnVisibleFilePropertiesChanged in FNotifications then
      begin
        FNotifications := FNotifications - [fvnVisibleFilePropertiesChanged];
        EnsureDisplayProperties;
      end
      else if fvnSelectionChanged in FNotifications then
      begin
        FNotifications := FNotifications - [fvnSelectionChanged];
        DoSelectionChanged;
      end;
    end;
  finally
    EndUpdate;
    EnableAutoSizing;
  end;
end;

procedure TFileView.HandleRequests;
begin
  BeginUpdate;
  DisableAutoSizing;
  try
    while FRequests <> [] do
    begin
      // Order is important because of dependencies.
      // Remove request before acting on it, since a function called may request it again.
      if fvrqHashFileList in FRequests then
      begin
        FRequests := FRequests - [fvrqHashFileList];
        HashFileList;
      end
      else if fvrqApplyPendingFilesChanges in FRequests then
      begin
        FRequests := FRequests - [fvrqApplyPendingFilesChanges];
        ApplyPendingFilesChanges;
      end
      else if fvrqMakeDisplayFileList in FRequests then
      begin
        FRequests := FRequests - [fvrqMakeDisplayFileList];
        ReDisplayFileList;
      end;
    end;
  finally
    EndUpdate;
    EnableAutoSizing;
  end;
end;

procedure TFileView.Notify(NewNotifications: TFileViewNotifications);
begin
  FNotifications := FNotifications + NewNotifications;
  if FUpdateCount = 0 then
    HandleNotifications;
end;

procedure TFileView.OpenActiveFile;
begin
  ChooseFile(GetActiveDisplayFile);
end;

procedure TFileView.SetFileFilter(NewFilter: String; NewFilterOptions: TQuickSearchOptions);
begin
  // do not reload if filter has not changed
  if (FFileFilter = NewFilter) and (FFilterOptions = NewFilterOptions) then
    Exit;

  FFileFilter := NewFilter;
  FFilterOptions := NewFilterOptions;

  Request([fvrqMakeDisplayFileList]);
end;

procedure TFileView.SetFilelist(var NewAllDisplayFiles: TDisplayFiles;
                                var NewFilteredDisplayFiles: TDisplayFiles);
var
  ARequests: TFileViewRequests;
begin
  ClearRecentlyUpdatedFiles;

  FFiles.Free;
  FFiles := NewFilteredDisplayFiles;
  NewFilteredDisplayFiles := nil;

  FAllDisplayFiles.Free;
  FAllDisplayFiles := NewAllDisplayFiles;
  NewAllDisplayFiles := nil;

  FLastLoadedFileSource := FileSource;
  FLastLoadedPath := CurrentPath;

  BeginUpdate;
  try
    ARequests := [fvrqHashFileList];
    if not FReloadNeeded then
      Include(ARequests, fvrqApplyPendingFilesChanges)
    else
      ClearPendingFilesChanges;
    Request(ARequests);
    Notify([fvnFileSourceFileListChanged, fvnDisplayFileListChanged]);
  finally
    EndUpdate;
  end;

  // We have just reloaded file list, so the requested file should be there.
  // Regardless if it is there or not it should be cleared so that it doesn't
  // get selected on further reloads.
  RequestedActiveFile := '';
end;

procedure TFileView.EnableWatcher(Enable: Boolean);
var
  sDrive, sWatchDirsExclude: String;
  WatchFilter: TFSWatchFilter;
begin
  if Enable then
  begin
    if ([fvfDelayLoadingFiles, fvfDontWatch] * Flags = []) and
       Assigned(FileSource) and
       FileSource.IsClass(TFileSystemFileSource) and
       (FWatchPath <> CurrentPath) then
    begin
      if WatcherActive then
        EnableWatcher(False);

      // If current path is in exclude list then exit.
      if (watch_exclude_dirs in gWatchDirs) and (gWatchDirsExclude <> '') then
      begin
        sWatchDirsExclude := gWatchDirsExclude;
        repeat
          sDrive := Copy2SymbDel(sWatchDirsExclude, ';');
          if IsInPath(UTF8UpperCase(sDrive), UTF8UpperCase(CurrentPath), True, True) then
            Exit;
        until sWatchDirsExclude = '';
      end;

      WatchFilter := [];
      if watch_file_name_change in gWatchDirs then
        Include(WatchFilter, wfFileNameChange);
      if watch_attributes_change in gWatchDirs then
        Include(WatchFilter, wfAttributesChange);

      if WatchFilter <> [] then
      begin
        FWatchPath := CurrentPath;
        if TFileSystemWatcher.AddWatch(FWatchPath, WatchFilter, @WatcherEvent) = False then
          FWatchPath := EmptyStr;
      end;
    end;
  end
  else
  begin
    TFileSystemWatcher.RemoveWatch(FWatchPath, @WatcherEvent);
    FWatchPath := EmptyStr;
  end;
end;

procedure TFileView.ActivateEvent(Sender: TObject);
begin
  SetFlags(Flags - [fvfDelayLoadingFiles]);
  ReloadIfNeeded;
end;

function TFileView.CheckIfDelayReload: Boolean;
begin
  Result := ((watch_only_foreground in gWatchDirs) and (not Application.Active)) or
            (not IsVisibleToUser);
end;

procedure TFileView.DoReload;
begin
  FReloadNeeded := False;
  MakeFileSourceFileList;
end;

procedure TFileView.HandleFSWatcherEvent(const EventData: TFSWatcherEventData; NewFilesPosition: TNewFilesPosition; UpdatedFilesPosition: TUpdatedFilesPosition);
begin
  case EventData.EventType of
    fswFileCreated:
      Self.AddFile(EventData.FileName, EventData.Path, NewFilesPosition, UpdatedFilesPosition);
    fswFileChanged:
      Self.UpdateFile(EventData.FileName, EventData.Path, NewFilesPosition, UpdatedFilesPosition);
    fswFileDeleted:
      Self.RemoveFile(EventData.FileName);
    fswFileRenamed:
      Self.RenameFile(EventData.NewFileName, EventData.FileName, EventData.Path, NewFilesPosition, UpdatedFilesPosition);
    else
      Reload(EventData.Path);
  end;
end;

procedure TFileView.ReloadEvent(const aFileSource: IFileSource; const ReloadedPaths: TPathsArray);
begin
  // Reload file view but only if the file source is currently viewed
  // and FileSystemWatcher is not being used.
  if not WatcherActive and aFileSource.Equals(FileSource) then
    Reload(ReloadedPaths);
end;

procedure TFileView.ReloadTimerEvent(Sender: TObject);
begin
  FReloadTimer.Enabled := False;
  DoReload;
end;

procedure TFileView.WatcherEvent(const EventData: TFSWatcherEventData);
begin
  if not (csDestroying in ComponentState) and
     not FReloadNeeded and
     (IncludeTrailingPathDelimiter(EventData.Path) = CurrentPath) then
  begin
    if GetCurrentWorkType = fvwtCreate then
    begin
      // If some unknown change then we can only reload the whole file list.
      if EventData.EventType <> fswUnknownChange then
        AddEventToPendingFilesChanges(EventData)
      else
        Reload(EventData.Path);
    end
    else
    begin
      if FileListLoaded then
        HandleFSWatcherEvent(EventData, gNewFilesPosition, gUpdatedFilesPosition)
      else
        Reload(EventData.Path);
    end;
  end;
end;

procedure TFileView.GoToHistoryIndex(aFileSourceIndex, aPathIndex: Integer);
var
  IsNewFileSource: Boolean;
begin
  IsNewFileSource := not FHistory.FileSource[aFileSourceIndex].Equals(FHistory.CurrentFileSource);

  if BeforeChangePath(FHistory.FileSource[aFileSourceIndex],
                      FHistory.Path[aFileSourceIndex, aPathIndex]) then
  begin
    if Assigned(FileSource) and IsNewFileSource then
      FileSource.RemoveReloadEventListener(@ReloadEvent);
    EnableWatcher(False);

    FHistory.SetIndexes(aFileSourceIndex, aPathIndex);

    if Assigned(FileSource) and IsNewFileSource then
    begin
      UpdatePath(True);
      FileSource.AddReloadEventListener(@ReloadEvent);
    end;

    AfterChangePath;
    EnableWatcher(True);

    {$IFDEF DEBUG_HISTORY}
    FHistory.DebugShow;
    {$ENDIF}
  end;
end;

procedure TFileView.GoToPrevHistory;
var
  aFileSourceIndex, aPathIndex: Integer;
begin
  if FHistory.CurrentPathIndex > 0 then
  begin
    aFileSourceIndex := FHistory.CurrentFileSourceIndex;
    aPathIndex := FHistory.CurrentPathIndex - 1;
  end
  else if FHistory.CurrentFileSourceIndex > 0 then
  begin
    aFileSourceIndex := FHistory.CurrentFileSourceIndex - 1;
    aPathIndex := FHistory.PathsCount[aFileSourceIndex] - 1;
  end
  else
    Exit;

  GoToHistoryIndex(aFileSourceIndex, aPathIndex);
end;

procedure TFileView.GoToNextHistory;
var
  aFileSourceIndex, aPathIndex: Integer;
begin
  if FHistory.CurrentFileSourceIndex >= 0 then
  begin
    if FHistory.CurrentPathIndex < FHistory.PathsCount[FHistory.CurrentFileSourceIndex] - 1 then
    begin
      aFileSourceIndex := FHistory.CurrentFileSourceIndex;
      aPathIndex := FHistory.CurrentPathIndex + 1;
    end
    else if FHistory.CurrentFileSourceIndex < FHistory.Count - 1 then
    begin
      aFileSourceIndex := FHistory.CurrentFileSourceIndex + 1;
      aPathIndex := 0;
    end
    else
      Exit;

    GoToHistoryIndex(aFileSourceIndex, aPathIndex);
  end;
end;

procedure TFileView.ReDisplayFileList;
begin
  case GetCurrentWorkType of
    fvwtNone: ; // Ok to continue.
    fvwtCreate:
      // File list is being loaded from file source - cannot display yet.
      Exit;
    fvwtUpdate:
      StopWorkers;
    else
      Exit;
  end;

  // Redisplaying file list is done in the main thread because it takes
  // relatively short time, so the user usually won't notice it and it is
  // a bit faster this way.
  TFileListBuilder.MakeDisplayFileList(
    FAllDisplayFiles, FFiles, FileFilter, FFilterOptions);
  Notify([fvnDisplayFileListChanged]);
end;

procedure TFileView.WorkerStarting(const Worker: TFileViewWorker);
begin
  if (Worker.WorkType = fvwtCreate) and not Worker.Aborted then
  begin
    FLoadFilesStartTime := SysUtils.Now;
  end;
end;

procedure TFileView.WorkerFinished(const Worker: TFileViewWorker);
var
  Interval: Integer;
begin
  if (Worker.WorkType = fvwtCreate) and not Worker.Aborted then
  begin
    FLoadFilesFinishTime := SysUtils.Now;

    // Schedule another reload if needed.
    if FReloadNeeded and not CheckIfDelayReload then
    begin
      // Delay by half the time taken by previous loading.
      Interval := DateTimeToTimeStamp(SysUtils.Now - FLoadFilesStartTime).Time div 2;
      if Interval < MinimumReloadInterval then
        Interval := MinimumReloadInterval;
      FReloadTimer.Interval := Interval;
      FReloadTimer.Enabled  := True;
    end;
  end;

  if Worker is TCalculateSpaceWorker then
  begin
    if TCalculateSpaceWorker(Worker).CompletedCalculations > 1 then
    begin
      SortAllDisplayFiles;
      ReDisplayFileList;
    end;
  end;
end;

{ TDropParams }

constructor TDropParams.Create(
                  var aFiles: TFiles;
                  aDropEffect: TDropEffect;
                  aScreenDropPoint: TPoint;
                  aDropIntoDirectories: Boolean;
                  aSourcePanel: TFileView;
                  aTargetPanel: TFileView;
                  aTargetPath: String);
begin
  Files := aFiles;
  aFiles := nil;
  DropEffect := aDropEffect;
  ScreenDropPoint := aScreenDropPoint;
  DropIntoDirectories := aDropIntoDirectories;
  SourcePanel := aSourcePanel;
  TargetPanel := aTargetPanel;
  TargetPath := aTargetPath;
end;

destructor TDropParams.Destroy;
begin
  inherited Destroy;
  FreeAndNil(Files);
end;

function TDropParams.GetDragDropType: TDragDropType;
begin
  if Assigned(SourcePanel) then
    Result := ddtInternal
  else
    Result := ddtExternal;
end;

end.

