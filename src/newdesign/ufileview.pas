unit uFileView; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, contnrs, fgl,
  uFile, uDisplayFile, uFileSource, uMethodsList, uDragDropEx, uXmlConfig,
  uClassesEx, uFileSorting, uFileViewHistory, uFileProperty, uFileViewWorker,
  uFunctionThread, uFileSystemWatcher;

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
    FReloading: Boolean;        //<en If currently reloading file list
    FWorkersThread: TFunctionThread;

    FActive: Boolean;             //<en Is this view active
    FLastActiveFile: String;      //<en Last active file (cursor)
    {en
       File name which should be selected. Sometimes the file might not yet
       exist in the filelist (for example after rename or create), but will be
       in the list on next reload.
    }
    FRequestedActiveFile: String;
    FFileFilter: String;
    FWatchPath: String;

    FMethods: TMethodsList;

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
    function GetPath(FileSourceIndex, PathIndex: Integer): UTF8String;
    function GetPathsCount(FileSourceIndex: Integer): Integer;
    function GetWatcherActive: Boolean;
    procedure SetFileFilter(const NewFilter: String);
    {en
       Assigns the built lists to the file view and displays new the file list.
    }
    procedure SetFileList(var NewDisplayFiles: TDisplayFiles;
                          var NewFileSourceFiles: TFiles);
    procedure EnableWatcher(Enable: Boolean);

    procedure ReloadEvent(const aFileSource: IFileSource; const ReloadedPaths: TPathsArray);
    procedure WatcherEvent(const WatchPath: String; NotifyData, UserData: Pointer);

  protected
    FFiles: TDisplayFiles;      //<en List of displayed files
    FFileSourceFiles: TFiles;   //<en List of files from file source

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
       Retrieves file list from file source into FFileSourceFiles.
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
    function Reload(const PathsToReload: TPathsArray = nil): Boolean; virtual;
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

    property CurrentAddress: String read GetCurrentAddress;
    property CurrentFileSourceIndex: Integer read GetCurrentFileSourceIndex;
    property CurrentPath: String read GetCurrentPath write SetCurrentPath;
    property CurrentPathIndex: Integer read GetCurrentPathIndex;
    property FileFilter: String read FFileFilter write SetFileFilter;
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
  Dialogs, LCLProc, Forms, strutils,
  uActs, uLng, uShowMsg, uFileSystemFileSource, uFileSourceUtil, uDCUtils, uGlobs;

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
  FMethods := TMethodsList.Create(Self);
  FHistory := TFileViewHistory.Create;
  FActive := False;
  FLastActiveFile := '';
  FRequestedActiveFile := '';
  FFileFilter := '';
  FFiles := nil;
  FHashedFiles := nil;
  FFileSourceFiles := nil;
  FWorkersThread := nil;
  FReloading := False;
  FFileViewWorkers := TFileViewWorkers.Create(False);
  FWatchPath := EmptyStr;

  inherited Create(AOwner);
  Parent := AOwner;
end;

destructor TFileView.Destroy;
var
  i: Integer;
begin
  for i := 0 to FileSourcesCount - 1 do
    FHistory.FileSource[i].RemoveReloadEventListener(@ReloadEvent);

  RemoveAllFileSources;

  if Assigned(FWorkersThread) then
  begin
    StopWorkers;

    // Wait until all the workers finish.
    FWorkersThread.Finish;
    DebugLn('Waiting for workers thread ', hexStr(FWorkersThread));
    TFunctionThread.WaitForWithSynchronize(FWorkersThread);
    FWorkersThread := nil;
  end;

  // Now all the workers can be safely freed.
  if Assigned(FFileViewWorkers) then
  begin
    for i := 0 to FFileViewWorkers.Count - 1 do
    begin
      with FFileViewWorkers[i] do
      begin
        if Working then
          DebugLn('Error: Worker still working.')
        else if not CanBeDestroyed then
          DebugLn('Error: Worker cannot be destroyed.');
        Free;
      end;
    end;
    FreeAndNil(FFileViewWorkers);
  end;

  if Assigned(FFiles) then
    FreeAndNil(FFiles);
  if Assigned(FFileSourceFiles) then
    FreeAndNil(FFileSourceFiles);
  if Assigned(FHashedFiles) then
    FreeAndNil(FHashedFiles);

  inherited;

  FreeAndNil(FMethods);
  FreeAndNil(FHistory);
end;

function TFileView.Clone(NewParent: TWinControl): TFileView;
begin
  raise Exception.Create('Cannot create object of abstract class');
  Result := nil; // For compiler warning.
end;

procedure TFileView.CloneTo(AFileView: TFileView);
begin
  if Assigned(AFileView) then
  begin
    // FFileSource should have been passed to FileView constructor already.
    // FMethods are created in FileView constructor.
    AFileView.OnBeforeChangePath := Self.OnBeforeChangePath;
    AFileView.OnAfterChangePath := Self.OnAfterChangePath;
    AFileView.OnActivate := Self.OnActivate;
    AFileView.OnReload := Self.OnReload;

    AFileView.FHistory.Assign(Self.FHistory);
    AFileView.FSortings := CloneSortings(Self.FSortings);
    AFileView.FLastActiveFile := Self.FLastActiveFile;
    AFileView.FRequestedActiveFile := Self.FRequestedActiveFile;
    AFileView.FFileFilter := Self.FFileFilter;

    if Assigned(Self.FFileSourceFiles) then
      AFileView.FFileSourceFiles := Self.FFileSourceFiles.Clone;
    AFileView.FFiles := Self.FFiles.Clone(Self.FFileSourceFiles, AFileView.FFileSourceFiles);
    AFileView.HashFileList;
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
begin
  if csDestroying in ComponentState then
    Exit;

  {$IFDEF timeFileView}
  startTime := Now;
  DebugLn('---- Start ----');
  {$ENDIF}

  StopWorkers;

  if gListFilesInThread then
    AThread := GetWorkersThread;

  Worker := TFileListBuilder.Create(
    FileSource,
    FileSourcesCount,
    FileFilter,
    CurrentPath,
    Sorting,
    AThread,
    FilePropertiesNeeded,
    @SetFileList);

  AddWorker(Worker);

  if gListFilesInThread then
  begin
    // Clear files.
    if Assigned(FFileSourceFiles) then
    begin
      FFiles.Clear; // Clear references to files from the source.
      FreeAndNil(FFileSourceFiles);
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
        on e: Exception do
          MessageDlg('Error', e.Message, mtError, [mbOK], 0);
      end;
  finally
    FSFile.Free;
  end;
end;

function TFileView.GetCurrentWorkType: TFileViewWorkType;
var
  i: Integer;
begin
  for i := 0 to FFileViewWorkers.Count - 1 do
    if FFileViewWorkers[i].Working then
      Exit(FFileViewWorkers[i].WorkType);
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
  for i := 0 to FFiles.Count - 1 do
    FHashedFiles.Add(FFiles[i], nil);
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
  Result := False;

  if Assigned(PathsToReload) then
  begin
    for i := Low(PathsToReload) to High(PathsToReload) do
      if IsInPath(PathsToReload[i], CurrentPath, True) then
      begin
        Result := True;
        Break;
      end;

    if not Result then
      Exit;
  end;

  FReloading := True;
  MakeFileSourceFileList;
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

    if FHistory.FileSource[i].IsInterface(IFileSystemFileSource) then
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
var
  Method: TMethod;
begin
  Method := FMethods.GetMethod(CommandName);
  if Assigned(Method.Code) then
  begin
    // Command is supported - execute it.
    TCommandFunc(Method)(Parameter);
  end;
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

    AfterChangePath;

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

procedure TFileView.SetFileFilter(const NewFilter: String);
begin
  FFileFilter := NewFilter;
  ReDisplayFileList;
end;

procedure TFileView.SetFilelist(var NewDisplayFiles: TDisplayFiles;
                                var NewFileSourceFiles: TFiles);
begin
  if Assigned(FFiles) then
    FFiles.Free;
  FFiles := NewDisplayFiles;
  NewDisplayFiles := nil;

  if Assigned(FFileSourceFiles) then
    FFileSourceFiles.Free;
  FFileSourceFiles := NewFileSourceFiles;
  NewFileSourceFiles := nil;

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
    if WatcherActive then
      EnableWatcher(False);

    if IsFileSystemWatcher and
       Assigned(FileSource) and
       FileSource.IsClass(TFileSystemFileSource) then
    begin
      // If current path is in exclude list then exit.
      if (watch_exclude_dirs in gWatchDirs) and (gWatchDirsExclude <> '') then
      begin
        sWatchDirsExclude := gWatchDirsExclude;
        repeat
          sDrive := Copy2SymbDel(sWatchDirsExclude, ';');
          if IsInPath(UTF8UpperCase(sDrive), UTF8UpperCase(CurrentPath), True) then
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
        TFileSystemWatcher.AddWatch(FWatchPath, WatchFilter, @WatcherEvent);
      end;
    end;
  end
  else
  begin
    TFileSystemWatcher.RemoveWatch(FWatchPath, @WatcherEvent);
    FWatchPath := EmptyStr;
  end;
end;

procedure TFileView.ReloadEvent(const aFileSource: IFileSource; const ReloadedPaths: TPathsArray);
begin
  // Reload file view, but only if the file source is currently viewed.
  if aFileSource.Equals(FileSource) then
    Reload(ReloadedPaths);
end;

procedure TFileView.WatcherEvent(const WatchPath: String; NotifyData, UserData: Pointer);
var
  Paths: TPathsArray;
begin
  // if not active and refresh only in foreground then exit
  if (watch_only_foreground in gWatchDirs) and (not Application.Active) then
    Exit;

  SetLength(Paths, 1);
  Paths[0] := WatchPath;
  Reload(Paths);
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
  TFileListBuilder.MakeDisplayFileList(
    FileSource, FFileSourceFiles, FFiles, FileFilter);
  HashFileList;
  AfterMakeFileList;
end;

procedure TFileView.WorkerStarting(const Worker: TFileViewWorker);
begin
end;

procedure TFileView.WorkerFinished(const Worker: TFileViewWorker);
begin
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

