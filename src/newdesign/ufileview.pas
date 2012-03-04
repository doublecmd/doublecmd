unit uFileView; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, ComCtrls, contnrs, fgl,
  uFile, uDisplayFile, uFileSource, uFormCommands, uDragDropEx, uXmlConfig,
  uClassesEx, uFileSorting, uFileViewHistory, uFileProperty, uFileViewWorker,
  uFunctionThread, uFileSystemWatcher, fQuickSearch, StringHashList;

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
  TOnReload = procedure (aFileView: TFileView) of object;

  TDropParams = class;
  TDragDropType = (ddtInternal, ddtExternal);
  // Lists all operations supported by dragging and dropping items
  // in the panel (external, internal and via menu).
  TDragDropOperation = (ddoCopy, ddoMove, ddoSymLink, ddoHardLink);

  TFileViewWorkers = specialize TFPGObjectList<TFileViewWorker>;

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
    FHashedFiles: TBucketList;  //<en Contains pointers to file source files for quick checking if a file object is still valid
    FHashedNames: TStringHashList;
    FReloading: Boolean;        //<en If currently reloading file list
    FReloadNeeded: Boolean;     //<en If file list should be reloaded
    FWorkersThread: TFunctionThread;
    FReloadTimer: TTimer;
    FLoadFilesStartTime: TDateTime;
    FLoadFilesFinishTime: TDateTime;
    FLoadFilesNoDelayCount: Integer; //<en How many reloads have been accepted without delay

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
    FOnReload : TOnReload;

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
    function GetWatcherActive: Boolean;
    {en
       Assigns the built lists to the file view and displays new the file list.
    }
    procedure SetFileList(var NewAllDisplayFiles: TDisplayFiles;
                          var NewFilteredDisplayFiles: TDisplayFiles);
    procedure EnableWatcher(Enable: Boolean);

    procedure ActivateEvent(Sender: TObject);
    function CheckIfDelayReload: Boolean;
    procedure DoReload;
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
    procedure DoOnReload;
    function GetCurrentPath: String; virtual;
    procedure SetCurrentPath(NewPath: String); virtual;
    function GetActiveDisplayFile: TDisplayFile; virtual; abstract;
    function GetWorkersThread: TFunctionThread;

    procedure SaveSelection; virtual;
    procedure RestoreSelection; virtual;

    procedure SelectFile(AFile: TDisplayFile); virtual;
    procedure InvertFileSelection(AFile: TDisplayFile);
    procedure InvertAll; virtual;
    procedure MarkAllFiles(bMarked: Boolean);
    procedure MarkGroup(const sMask: String; bSelect: Boolean);
    function MarkMinus: Boolean; virtual;
    function MarkPlus: Boolean; virtual;
    function MarkShiftPlus: Boolean; virtual;
    function MarkShiftMinus: Boolean; virtual;

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
    procedure ChooseFile(const AFile: TDisplayFile; FolderMode: Boolean = False); virtual;
    {en
       Returns current work type in progress.
    }
    function GetCurrentWorkType: TFileViewWorkType;
    {en
       Store pointers to file source files in a fast to read structure.
    }
    procedure HashFileList;
    function IsActiveItemValid: Boolean;
    function IsReferenceValid(aFile: TDisplayFile): Boolean;
    {en
       Returns True if there are no files shown in the panel.
    }
    function IsEmpty: Boolean;
    {en
       Returns True if item is not nil and not '..'.
       May be extended to include other conditions.
    }
    function IsItemValid(AFile: TDisplayFile): Boolean;

    procedure SetSorting(const NewSortings: TFileSortings); virtual;

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
    procedure WorkerStarting(const Worker: TFileViewWorker); virtual;
    procedure WorkerFinished(const Worker: TFileViewWorker); virtual;

    property Active: Boolean read FActive write SetActive;
    property FilePropertiesNeeded: TFilePropertiesTypes read FFilePropertiesNeeded write FFilePropertiesNeeded;
    property LastActiveFile: String read FLastActiveFile write FLastActiveFile;
    property RequestedActiveFile: String read FRequestedActiveFile write FRequestedActiveFile;
    property WorkersThread: TFunctionThread read GetWorkersThread;

  public
    procedure MarkFile(AFile: TDisplayFile; bMarked: Boolean);
    property  DisplayFiles: TDisplayFiles read FFiles;

  public
    constructor Create(AOwner: TWinControl;
                       AFileSource: IFileSource;
                       APath: String); virtual reintroduce;
    constructor Create(AOwner: TWinControl;
                       AFileView: TFileView); virtual reintroduce;
    constructor Create(AOwner: TWinControl;
                       AConfig: TIniFileEx;
                       ASectionName: String;
                       ATabIndex: Integer); virtual reintroduce;
    constructor Create(AOwner: TWinControl;
                       AConfig: TXmlConfig;
                       ANode: TXmlNode); virtual reintroduce;

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

    procedure UpdateView; virtual;

    {en
       Moves the selection focus to the file specified by aFilePath.
       @param(aFilePath may be an absolute path to the file or just a file name.)
    }
    procedure SetActiveFile(aFilePath: String); virtual; overload;

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

    procedure ExecuteCommand(CommandName: String; Parameter: String = ''); virtual;

    {en
       Returns @true if at least one file is somehow selected.
       What "selected" means depends on the concrete file view implementation.
       (Usually it will be a different method of selecting than ActiveFile.)
    }
    function HasSelectedFiles: Boolean; virtual;
    procedure UnselectAllFiles; virtual;

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
    property Path[FileSourceIndex, PathIndex: Integer]: UTF8String read GetPath;
    property PathsCount[FileSourceIndex: Integer]: Integer read GetPathsCount;

    property Sorting: TFileSortings read FSortings write SetSorting;
    property WatcherActive: Boolean read GetWatcherActive;

    property NotebookPage: TCustomPage read GetNotebookPage;
    property OnBeforeChangePath : TOnBeforeChangePath read FOnBeforeChangePath write FOnBeforeChangePath;
    property OnAfterChangePath : TOnAfterChangePath read FOnAfterChangePath write FOnAfterChangePath;
    property OnChangeActiveFile : TOnChangeActiveFile read FOnChangeActiveFile write FOnChangeActiveFile;
    property OnActivate : TOnActivate read FOnActivate write FOnActivate;
    property OnReload : TOnReload read FOnReload write FOnReload;
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
  Dialogs, LCLProc, Forms, StrUtils, uMasks, fMaskInputDlg,
  uDebug, uLng, uShowMsg, uFileSystemFileSource, uFileSourceUtil,
  uDCUtils, uGlobs, uFileViewNotebook, uSearchTemplate, uOSUtils;

const
  MinimumReloadInterval  = 1000; // 1 second

constructor TFileView.Create(AOwner: TWinControl; AFileSource: IFileSource; APath: String);
begin
  CreateDefault(AOwner);

  FHistory.Add(AFileSource, aPath);
  FileSource.AddReloadEventListener(@ReloadEvent);
end;

constructor TFileView.Create(AOwner: TWinControl; AFileView: TFileView);
begin
  CreateDefault(AOwner);
  AFileView.CloneTo(Self);
  if Assigned(FileSource) then
    FileSource.AddReloadEventListener(@ReloadEvent);
end;

constructor TFileView.Create(AOwner: TWinControl; AConfig: TIniFileEx; ASectionName: String; ATabIndex: Integer);
begin
  CreateDefault(AOwner);
end;

constructor TFileView.Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode);
begin
  CreateDefault(AOwner);
end;

procedure TFileView.CreateDefault(AOwner: TWinControl);
begin
  FOnBeforeChangePath := nil;
  FOnAfterChangePath := nil;
  FOnChangeActiveFile := nil;
  FOnActivate := nil;
  FOnReload := nil;
  FSortings := nil;
  FFilePropertiesNeeded := [];
  FMethods := TFormCommands.Create(Self);
  FHistory := TFileViewHistory.Create;
  FSavedSelection:= TStringListEx.Create;
  FActive := False;
  FLastActiveFile := '';
  FRequestedActiveFile := '';
  FLastMark := '*';
  FFileFilter := '';
  FFilterOptions := gQuickSearchOptions;
  FFiles := nil;
  FHashedFiles := nil;
  FHashedNames := TStringHashList.Create(True);
  FWorkersThread := nil;
  FReloading := False;
  FReloadNeeded := False;
  FFileViewWorkers := TFileViewWorkers.Create(False);
  FWatchPath := EmptyStr;
  FReloadTimer := TTimer.Create(Self);
  FReloadTimer.Enabled := False;
  FReloadTimer.OnTimer := @ReloadTimerEvent;
  FLoadFilesStartTime := 0;
  FLoadFilesFinishTime := 0;
  FLoadFilesNoDelayCount := 0;

  inherited Create(AOwner);
  Parent := AOwner;

  if AOwner is TFileViewPage then
    (AOwner as TFileViewPage).OnActivate := @ActivateEvent;
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

  RemoveAllFileSources;

  if Assigned(FFiles) then
    FreeAndNil(FFiles);
  FreeAndNil(FAllDisplayFiles);
  if Assigned(FHashedFiles) then
    FreeAndNil(FHashedFiles);
  FreeAndNil(FHashedNames);

  inherited Destroy;

  FreeAndNil(FHistory);
  FreeThenNil(FSavedSelection);
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
    AFileView.FLastMark := FLastMark;
    // FFileSource should have been passed to FileView constructor already.
    // FMethods are created in FileView constructor.
    AFileView.OnBeforeChangePath := Self.OnBeforeChangePath;
    AFileView.OnAfterChangePath := Self.OnAfterChangePath;
    AFileView.OnActivate := Self.OnActivate;
    AFileView.OnReload := Self.OnReload;

    for I := 0 to FSavedSelection.Count - 1 do
      AFileView.FSavedSelection.Add(FSavedSelection.Strings[I]);

    AFileView.FHistory.Assign(Self.FHistory);
    AFileView.FSortings := CloneSortings(Self.FSortings);
    AFileView.FLastActiveFile := Self.FLastActiveFile;
    AFileView.FRequestedActiveFile := Self.FRequestedActiveFile;
    AFileView.FReloadNeeded := Self.FReloadNeeded;

    AFileView.FAllDisplayFiles := Self.FAllDisplayFiles.Clone(True);

    // FFiles need to be recreated because the filter is not cloned.
    // This is done in AFileView.UpdateView.
  end;
end;

function TFileView.GetNotebookPage: TCustomPage;
begin
  if Parent is TCustomPage then
    Result := Parent as TCustomPage
  else
    Result := nil;
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

procedure TFileView.DoOnReload;
begin
  if FReloading then
  begin
    FReloading := False;
    if Assigned(OnReload) then
      OnReload(Self);
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

procedure TFileView.RestoreSelection;
var
  I: Integer;
begin
  for I := 0 to FFiles.Count - 1 do
    with FFiles[I] do
    Selected:= (FSavedSelection.IndexOf(FSFile.Name) >= 0);
end;

procedure TFileView.SelectFile(AFile: TDisplayFile);
begin
  InvertFileSelection(AFile);
end;

procedure TFileView.InvertFileSelection(AFile: TDisplayFile);
begin
  if Assigned(AFile) then
    MarkFile(AFile, not AFile.Selected);
end;

procedure TFileView.InvertAll;
var
  i:Integer;
begin
  for i := 0 to FFiles.Count-1 do
    InvertFileSelection(FFiles[i]);
end;

procedure TFileView.MarkFile(AFile: TDisplayFile; bMarked: Boolean);
begin
  if IsItemValid(AFile) then
    AFile.Selected := bMarked;
end;

procedure TFileView.MarkAllFiles(bMarked: Boolean);
var
  i: Integer;
begin
  for i := 0 to FFiles.Count - 1 do
    MarkFile(FFiles[i], bMarked);
end;

procedure TFileView.MarkGroup(const sMask: String; bSelect: Boolean);
var
  I: Integer;
  SearchTemplate: TSearchTemplate = nil;
begin
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
          end;
      end;
end;

function TFileView.MarkMinus: Boolean;
var
  s: String;
begin
  Result:= True;
  if IsEmpty then Exit(False);
  s := FLastMark;
  if not ShowMaskInputDlg(rsMarkMinus, rsMaskInput, glsMaskHistory, s) then Exit(False);
  FLastMark := s;
  MarkGroup(s, False);
end;

function TFileView.MarkPlus: Boolean;
var
  s: String;
begin
  Result:= True;
  if IsEmpty then Exit(False);
  s := FLastMark;
  if not ShowMaskInputDlg(rsMarkPlus, rsMaskInput, glsMaskHistory, s) then Exit(False);
  FLastMark := s;
  MarkGroup(s, True);
end;

function TFileView.MarkShiftPlus: Boolean;
var
  sGroup: String;
begin
  Result:= IsActiveItemValid;
  if Result then
  begin
    sGroup := GetActiveDisplayFile.FSFile.Extension;
    if sGroup <> '' then
      sGroup := '.' + sGroup;
    MarkGroup('*' + sGroup, True);
  end;
end;

function TFileView.MarkShiftMinus: Boolean;
var
  sGroup: String;
begin
  Result:= IsActiveItemValid;
  if Result then
  begin
    sGroup := GetActiveDisplayFile.FSFile.Extension;
    if sGroup <> '' then
      sGroup := '.' + sGroup;
    MarkGroup('*' + sGroup, False);
   end;
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

procedure TFileView.MakeFileSourceFileList;
var
  Worker: TFileViewWorker;
  AThread: TFunctionThread = nil;
  ClonedDisplayFiles: TDisplayFiles = nil;
  DisplayFilesHashed: TStringHashList = nil;
  i: Integer;
begin
  if csDestroying in ComponentState then
    Exit;

  {$IFDEF timeFileView}
  filelistLoaderTime := Now;
  DCDebug('--------- Start ---------');
  {$ENDIF}

  StopWorkers;

  if gListFilesInThread then
    AThread := GetWorkersThread;

  if FileSource.Equals(FLastLoadedFileSource) and
     (FLastLoadedPath = CurrentPath) then
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
    Sorting,
    AThread,
    FilePropertiesNeeded,
    @SetFileList,
    ClonedDisplayFiles,
    DisplayFilesHashed);

  AddWorker(Worker);

  if gListFilesInThread then
  begin
    // Clear files.
    if Assigned(FFiles) then
    begin
      FFiles.Clear; // Clear references to files from the source.
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
  if Assigned(FHashedFiles) then
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

procedure TFileView.UnselectAllFiles;
var
  i: Integer;
begin
  for i := 0 to FFiles.Count - 1 do
    FFiles[i].Selected := False;
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
  sFSType, sPath: String;
  aFileSource: IFileSource = nil;
  ActiveFSIndex: Integer = -1;
  ActivePathIndex: Integer = -1;
begin
  RemoveAllFileSources;

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

procedure TFileView.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
var
  HistoryNode, EntryNode, FSNode, PathsNode, PathNode: TXmlNode;
  i, j: Integer;
  PathIndex: Integer;
begin
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
begin
  EnableWatcher(IsFileSystemWatcher);
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
  FReloading := False;
  FReloadTimer.Enabled := False;
  FLoadFilesStartTime := 0;
  FLoadFilesFinishTime := 0;
  FLoadFilesNoDelayCount := 0;

  if Assigned(OnAfterChangePath) then
    OnAfterChangePath(Self);
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
      Reload;
      UpdateView;
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

procedure TFileView.ExecuteCommand(CommandName: String; Parameter: String);
begin
  FMethods.ExecuteCommand(CommandName, Parameter);
end;

procedure TFileView.AddFileSource(aFileSource: IFileSource; aPath: String);
var
  IsNewFileSource: Boolean;
begin
  IsNewFileSource := aFileSource <> FileSource;

  if BeforeChangePath(aFileSource, aPath) then
  begin
    if Assigned(FileSource) and IsNewFileSource then
      FileSource.RemoveReloadEventListener(@ReloadEvent);
    EnableWatcher(False);

    FHistory.Add(aFileSource, aPath);

    if Assigned(FileSource) and IsNewFileSource then
    begin
      Reload;
      UpdateView;
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
begin
  if FileSourcesCount > 0 then
  begin
    PrevIndex := FHistory.CurrentFileSourceIndex - 1;
    if PrevIndex >= 0 then
    begin
      NewFileSource := FHistory.FileSource[PrevIndex];
      NewPath := FHistory.Path[PrevIndex, FHistory.PathsCount[PrevIndex] - 1];
    end;
    IsNewFileSource := NewFileSource <> FileSource;

    if BeforeChangePath(NewFileSource, NewPath) then
    begin
      if IsNewFileSource then
        FileSource.RemoveReloadEventListener(@ReloadEvent);
      EnableWatcher(False);

      FHistory.DeleteFromCurrentFileSource;

      if Assigned(FileSource) and IsNewFileSource then
      begin
        Reload;
        UpdateView;
        FileSource.AddReloadEventListener(@ReloadEvent);
      end;

      AfterChangePath;
      EnableWatcher(True);

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
  Reload;
  UpdateView;
  FileSource.AddReloadEventListener(@ReloadEvent);
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

function TFileView.GetWatcherActive: Boolean;
begin
  Result := FWatchPath <> EmptyStr;
end;

procedure TFileView.SetFileFilter(NewFilter: String; NewFilterOptions: TQuickSearchOptions);
begin
  // do not reload if filter has not changed
  if (FFileFilter = NewFilter) and (FFilterOptions = NewFilterOptions) then
    Exit;

  FFileFilter := NewFilter;
  FFilterOptions := NewFilterOptions;

  ReDisplayFileList;
end;

procedure TFileView.SetFilelist(var NewAllDisplayFiles: TDisplayFiles;
                                var NewFilteredDisplayFiles: TDisplayFiles);
begin
  FFiles.Free;
  FFiles := NewFilteredDisplayFiles;
  NewFilteredDisplayFiles := nil;

  FAllDisplayFiles.Free;
  FAllDisplayFiles := NewAllDisplayFiles;
  NewAllDisplayFiles := nil;

  FLastLoadedFileSource := FileSource;
  FLastLoadedPath := CurrentPath;

  HashFileList;
  AfterMakeFileList;

  // We have just reloaded file list, so the requested file should be there.
  // Regardless if it is there or not it should be cleared so that it doesn't
  // get selected on further reloads.
  RequestedActiveFile := '';

  DoOnReload;
end;

procedure TFileView.EnableWatcher(Enable: Boolean);
var
  sDrive, sWatchDirsExclude: String;
  WatchFilter: TFSWatchFilter;
begin
  if Enable then
  begin
    if Assigned(FileSource) and
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
  ReloadIfNeeded;
end;

function TFileView.CheckIfDelayReload: Boolean;
begin
  Result := ((watch_only_foreground in gWatchDirs) and (not Application.Active)) or
            ((NotebookPage is TFileViewPage) and not TFileViewPage(NotebookPage).IsActive);
end;

procedure TFileView.DoReload;
begin
  FReloading := True;
  FReloadNeeded := False;
  MakeFileSourceFileList;
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
  Reload(EventData.Path);
end;

procedure TFileView.GoToHistoryIndex(aFileSourceIndex, aPathIndex: Integer);
var
  IsNewFileSource: Boolean;
begin
  IsNewFileSource := FHistory.FileSource[aFileSourceIndex] <> FHistory.CurrentFileSource;

  if BeforeChangePath(FHistory.FileSource[aFileSourceIndex],
                      FHistory.Path[aFileSourceIndex, aPathIndex]) then
  begin
    if Assigned(FileSource) and IsNewFileSource then
      FileSource.RemoveReloadEventListener(@ReloadEvent);
    EnableWatcher(False);

    FHistory.SetIndexes(aFileSourceIndex, aPathIndex);

    if Assigned(FileSource) and IsNewFileSource then
    begin
      Reload;
      UpdateView;
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
  if not Assigned(FFiles) then
    FFiles := TDisplayFiles.Create(False);
  TFileListBuilder.MakeDisplayFileList(
    FAllDisplayFiles, FFiles, FileFilter, FFilterOptions);
  AfterMakeFileList;
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
  if Assigned(Files) then
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

