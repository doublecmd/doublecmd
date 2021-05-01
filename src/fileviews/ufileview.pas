{
   Double commander
   -------------------------------------------------------------------------
   FileView, base class of all of them

   Copyright (C) 2016 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

unit uFileView;

{$mode objfpc}{$H+}

interface

uses
  uFindFiles, Classes, SysUtils, Controls, ExtCtrls, Graphics, ComCtrls, contnrs, fgl, LMessages,
  uFile, uDisplayFile, uFileSource, uFormCommands, uDragDropEx, DCXmlConfig, DCBasicTypes,
  DCClassesUtf8, uFileSorting, uFileViewHistory, uFileProperty, uFileViewWorker,
  uFunctionThread, uFileSystemWatcher, fQuickSearch, DCStringHashListUtf8, uGlobs;

type

  TFileView = class;

  TFileViewClass = class of TFileView;

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
                           fvnFileSourceFileListLoaded,      // File list was loaded from FileSource
                           fvnFileSourceFileListUpdated,     // File list was updated (files added, removed or updated)
                           fvnSelectionChanged,              // Files were selected/deselected
                           fvnVisibleFilePropertiesChanged); // Different files or their properties are now visible
  TFileViewNotifications = set of TFileViewNotification;
  TFileViewApplyFilterResult = (fvaprRemoved, fvaprInserted,
                                fvaprExisting, fvaprNotExisting);

  { TMarkApplyOnAllDispatcher }
  TMarkApplyOnAllDispatcher = (tmaoa_Mark, tmaoa_UnMark, tmaoa_InvertMark);

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
    FHashedNames: TStringHashListUtf8;
    FPendingFilesChanges: TFPList;
    FPendingFilesTimer: TTimer;
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
    FWatcherEventLastTime: TDateTime;
    FWatcherEventsApplied: Integer;  //<en How many filesystem watcher events have been applied immediately before postponing them

    FActive: Boolean;             //<en Is this view active
    FLastActiveFile: String;      //<en Last active file (cursor)
    {en
       File name which should be selected. Sometimes the file might not yet
       exist in the filelist (for example after rename or create), but will be
       in the list on next reload.
    }
    FRequestedActiveFile: String;
    FFilterOptions: TQuickSearchOptions;
    FWatchPath: String;
    FLastMark: String;
    FLastMarkCaseSensitive: Boolean;
    FLastMarkIgnoreAccents: Boolean;
    FLastLoadedFileSource: IFileSource;
    FLastLoadedPath: String;
    FLoadingFileListLongTime: Boolean;
    FMethods: TFormCommands;
    FForceReload: Boolean;

    FOnBeforeChangePath : TOnBeforeChangePath;
    FOnAfterChangePath : TOnAfterChangePath;
    FOnChangeActiveFile: TOnChangeActiveFile;
    FOnActivate : TOnActivate;
    FOnFileListChanged : TOnFileListChanged;
    FLoadingFileListLongTimer: TTimer;

    procedure AddFile(const FileName, APath: String; NewFilesPosition: TNewFilesPosition; UpdatedFilesPosition: TUpdatedFilesPosition);
    procedure AddEventToPendingFilesChanges(const EventData: TFSWatcherEventData);
    function ApplyFilter(ADisplayFile: TDisplayFile; NewFilesPosition: TNewFilesPosition): TFileViewApplyFilterResult;
    procedure ApplyPendingFilesChanges(NewFilesPosition: TNewFilesPosition;
                                       UpdatedFilesPosition: TUpdatedFilesPosition);
    procedure ClearPendingFilesChanges;
    procedure ClearRecentlyUpdatedFiles;
    procedure DoOnFileListChanged;
    procedure EachViewDeactivate(AFileView: TFileView; {%H-}UserData: Pointer);
    function FileListLoaded: Boolean;
    function GetCurrentAddress: String;
    function GetCurrentLocation: String;
    function GetNotebookPage: TControl;
    function GetCurrentFileSource: IFileSource;
    function GetCurrentFileSourceIndex: Integer;
    function GetCurrentPathIndex: Integer;
    function GetFileSource(Index: Integer): IFileSource;
    function GetFileSourcesCount: Integer;
    function GetFiltered: Boolean;
    function GetPath(FileSourceIndex, PathIndex: Integer): String;
    function GetPathsCount(FileSourceIndex: Integer): Integer;
    function GetSortingProperties: TFilePropertiesTypes;
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
    procedure SetActive(bActive: Boolean); inline; overload;
    procedure SetActive(bActive, bNotify: Boolean); overload;
    procedure SetFlags(AValue: TFileViewFlags);
    procedure SetLoadingFileListLongTime(AValue: Boolean);
    procedure StartRecentlyUpdatedTimerIfNeeded;
    procedure StartUpdatePendingTimer;
    procedure UpdateFile(const FileName, APath: String; NewFilesPosition: TNewFilesPosition; UpdatedFilesPosition: TUpdatedFilesPosition);
    procedure UpdatedFilesTimerEvent(Sender: TObject);
    procedure UpdatePath({%H-}UpdateAddressToo: Boolean);
    procedure UpdatePendingTimerEvent(Sender: TObject);
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
    procedure LoadingFileListTimer(Sender: TObject);
    procedure ReloadEvent(const aFileSource: IFileSource; const ReloadedPaths: TPathsArray);
    procedure ReloadTimerEvent(Sender: TObject);
    procedure WatcherEvent(const EventData: TFSWatcherEventData);

  protected
    FFlatView: Boolean;
    FFileFilter: String;
    FAllDisplayFiles: TDisplayFiles;    //<en List of all files that can be displayed
    FFiles: TDisplayFiles;              //<en List of displayed files (filtered)
    FSavedSelection: TStringListEx;
    FSortingProperties: TFilePropertiesTypes;

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
    procedure CancelLastPathChange;
    procedure ClearFiles;
    {en
       Called when display file list (filtered list) has changed.
    }
    procedure DisplayFileListChanged; virtual;
    procedure EndUpdate;
    procedure EnsureDisplayProperties; virtual; abstract;
    {en
       Called after file list has been retrieved from file source.
       Runs from GUI thread.
    }
    procedure FileSourceFileListLoaded; virtual;
    {en
       Called when files were added, removed or updated in the filesource file list.
    }
    procedure FileSourceFileListUpdated; virtual;
    function GetCurrentPath: String; virtual;
    procedure SetCurrentPath(NewPath: String); virtual;
    function GetActiveDisplayFile: TDisplayFile; virtual; abstract;
    function GetWorkersThread: TFunctionThread;
    procedure InvertFileSelection(AFile: TDisplayFile; bNotify: Boolean = True);
    function IsLoadingFileList: Boolean; inline;
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
    procedure SetActiveFile(const {%H-}aFile: TFile); virtual; overload;

    {en
       Executed before file list has been retrieved.
       Runs from GUI thread.
    }
    procedure BeforeMakeFileList; virtual;
    function BeginDragExternal(DragFile: TDisplayFile; DragDropSource: uDragDropEx.TDragDropSource;
                               MouseButton: TMouseButton; ScreenStartPoint: TPoint): Boolean;
    procedure ChooseFile(const AFile: TDisplayFile; FolderMode: Boolean = False); virtual;
    function DimColor(AColor: TColor): TColor;
    procedure DoActiveChanged; virtual;
    procedure DoFileUpdated(AFile: TDisplayFile; {%H-}UpdatedProperties: TFilePropertiesTypes = []); virtual;
    procedure DoHandleKeyDown(var Key: Word; Shift: TShiftState); virtual;
    {en
       Handles keys when file list is being loaded.
    }
    procedure DoHandleKeyDownWhenLoading(var Key: Word; {%H-}Shift: TShiftState); virtual;
    procedure DoLoadingFileListLongTime; virtual;
    procedure DoSelectionChanged; virtual;
    procedure DoUpdateView; virtual;
    {en
       Returns current work type in progress.
    }
    function GetCurrentWorkType: TFileViewWorkType;
    procedure HandleKeyDownWhenLoading(var Key: Word; Shift: TShiftState);
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
    procedure RedrawFiles; virtual; abstract;
    procedure WorkerStarting(const Worker: TFileViewWorker); virtual;
    procedure WorkerFinished(const Worker: TFileViewWorker); virtual;

    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

    function GetVariantFileProperties: TDynamicStringArray; virtual;

    property Active: Boolean read FActive write SetActive;
    property FilePropertiesNeeded: TFilePropertiesTypes read FFilePropertiesNeeded write FFilePropertiesNeeded;
    property History: TFileViewHistory read FHistory;
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
                       AConfig: TXmlConfig;
                       ANode: TXmlNode;
                       AFlags: TFileViewFlags = []); virtual reintroduce;

    destructor Destroy; override;
    procedure Clear;

    function Clone({%H-}NewParent: TWinControl): TFileView; virtual;
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
    function CloneSelectedDirectories: TFiles;
    {en
       A list of files selected by the user
       (this should be a subset of displayed files list returned by Files).
       If there are no selected files then the active file pointed to by the cursor
       is added to the list as the only file.
       Caller is responsible for freeing the list.
    }
    function CloneSelectedOrActiveFiles: TFiles;
    function CloneSelectedOrActiveDirectories: TFiles;

    function GetActiveFileName: String;

    {en
       Retrieves files from file source again and displays the new list of files.
       Returns @true if reloading is done, @false if reloading will not be done
       (for example paths don't match).
    }
    function Reload(const PathsToReload: TPathsArray = nil): Boolean; overload;
    function Reload(const PathToReload: String): Boolean; overload;
    procedure Reload(AForced: Boolean);
    procedure ReloadIfNeeded;
    procedure StopWorkers; virtual;

    // For now we use here the knowledge that there are tabs.
    // Config should be independent of that in the future.
    procedure LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); virtual;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode; ASaveHistory:boolean); virtual;

    procedure UpdateView;

    {en
       Moves the selection focus to the file specified by aFilePath.
       @param(aFilePath may be an absolute path to the file or just a file name.)
    }
    procedure SetActiveFile({%H-}aFilePath: String); virtual; overload;

    {en
       If given path is a path to the directory, then changes current path
       to the given one;
       if given path is a path to the file, then changes current path
       to the path to a given file, and moves the selection to the file.
       @param(aFilePath may be an absolute path to the directory or to the file)
    }
    procedure ChangePathAndSetActiveFile({%H-}aFilePath: String); virtual; overload;

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
    procedure MarkCurrentName(bSelect: Boolean);
    procedure MarkCurrentNameExt(bSelect: Boolean);
    procedure MarkCurrentExtension(bSelect: Boolean);
    procedure MarkCurrentPath(bSelect: Boolean);
    procedure MarkFile(AFile: TDisplayFile; bSelect: Boolean; bNotify: Boolean = True);
    procedure MarkFiles(bSelect: Boolean);
    procedure MarkFiles(FromIndex, ToIndex: PtrInt; bSelect: Boolean);
    procedure MarkApplyOnAllFiles(const MarkApplyOnAllDispatcher: TMarkApplyOnAllDispatcher; MarkFileChecks: TFindFileChecks);
    procedure MarkGroup(const sMask: String; bSelect: Boolean; pbCaseSensitive:PBoolean = nil; pbIgnoreAccents: PBoolean = nil; pbWindowsInterpretation: PBoolean = nil; pMarkFileChecks: TPFindFileChecks = nil);
    procedure MarkGroup(bSelect: Boolean; pbCaseSensitive:PBoolean = nil; pbIgnoreAccents: PBoolean = nil; pbWindowsInterpretation: PBoolean = nil; psAttribute:PString = nil);
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

    procedure SetDragCursor(Shift: TShiftState); virtual; abstract;
    procedure SetFileFilter(NewFilter: String; NewFilterOptions: TQuickSearchOptions);
    procedure JustForColorPreviewSetActiveState(bActive: Boolean);

    property CurrentAddress: String read GetCurrentAddress;
    property CurrentFileSourceIndex: Integer read GetCurrentFileSourceIndex;
    property CurrentPath: String read GetCurrentPath write SetCurrentPath;
    property CurrentPathIndex: Integer read GetCurrentPathIndex;
    property CurrentLocation: String read GetCurrentLocation;
    property FileFilter: String read FFileFilter;
    property FilterOptions: TQuickSearchOptions read FFilterOptions;
    property Filtered: Boolean read GetFiltered;
    property FileSource: IFileSource read GetCurrentFileSource;
    property FileSources[Index: Integer]: IFileSource read GetFileSource;
    property FileSourcesCount: Integer read GetFileSourcesCount;
    property Flags: TFileViewFlags read FFlags write SetFlags;
    property FlatView: Boolean read FFlatView write FFlatView;
    property Path[FileSourceIndex, PathIndex: Integer]: String read GetPath;
    property PathsCount[FileSourceIndex: Integer]: Integer read GetPathsCount;

    property Sorting: TFileSortings read FSortings write SetSorting;
    property WatcherActive: Boolean read GetWatcherActive;

    property NotebookPage: TControl read GetNotebookPage;
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
    TargetFileSource: IFileSource;
    TargetPath: String;

    constructor Create(var aFiles: TFiles;
                       aDropEffect: TDropEffect;
                       aScreenDropPoint: TPoint;
                       aDropIntoDirectories: Boolean;
                       aSourcePanel: TFileView;
                       aTargetPanel: TFileView;
                       aTargetFileSource: IFileSource;
                       aTargetPath: String);
    destructor Destroy; override;

    // States, whether the drag&drop operation was internal or external.
    // If SourcePanel is not nil, then it's assumed it was internal.
    function GetDragDropType: TDragDropType;
  end;
  PDropParams = ^TDropParams;

implementation

uses
  Clipbrd, Dialogs, LCLProc, LCLType, Forms, dmCommonData,
  uShellExecute, fMaskInputDlg, uMasks, DCOSUtils, uOSUtils, DCStrUtils,
  uDCUtils, uDebug, uLng, uShowMsg, uFileSystemFileSource, uFileSourceUtil,
  uFileViewNotebook, uSearchTemplate, uKeyboard, uFileFunctions,
  fMain, uSearchResultFileSource, uFileSourceProperty, uVfsModule, uFileViewWithPanels;

const
  MinimumReloadInterval  = 1000; // 1 second
  UpdateFilelistInterval =  500;

constructor TFileView.Create(AOwner: TWinControl; AFileSource: IFileSource; APath: String; AFlags: TFileViewFlags = []);
begin
  DisableAutoSizing;
  try
    FFlags := AFlags;
    CreateDefault(AOwner);

    FHistory.AddFileSource(AFileSource);
    ChangePathAndSetActiveFile(aPath);
    FileSource.AddReloadEventListener(@ReloadEvent);

    // Update view before making file source file list,
    // so that file list isn't unnecessarily displayed twice.
    UpdateView;
    MakeFileSourceFileList;
  finally
    EnableAutoSizing;
  end;
end;

constructor TFileView.Create(AOwner: TWinControl; AFileView: TFileView; AFlags: TFileViewFlags = []);
begin
  DisableAutoSizing;
  try
    FFlags := AFlags;
    CreateDefault(AOwner);
    AFileView.CloneTo(Self);
    if Assigned(FileSource) then
      FileSource.AddReloadEventListener(@ReloadEvent);
    UpdateView;
  finally
    EnableAutoSizing;
  end;
end;

constructor TFileView.Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode; AFlags: TFileViewFlags = []);
begin
  DisableAutoSizing;
  try
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
  finally
    EnableAutoSizing;
  end;
end;

procedure TFileView.CreateDefault(AOwner: TWinControl);
begin
  FMethods := TFormCommands.Create(Self);
  FHistory := TFileViewHistory.Create;
  FSavedSelection:= TStringListEx.Create;
  FLastMark := '*';
  FLastMarkCaseSensitive := gbMarkMaskCaseSensitive;
  FLastMarkIgnoreAccents := gbMarkMaskIgnoreAccents;
  FFiles := TDisplayFiles.Create(False);
  FFilterOptions := gQuickSearchOptions;
  FHashedNames := TStringHashListUtf8.Create(True);
  FFileViewWorkers := TFileViewWorkers.Create(False);
  FReloadTimer := TTimer.Create(Self);
  FReloadTimer.Enabled := False;
  FReloadTimer.OnTimer := @ReloadTimerEvent;
  FLoadingFileListLongTimer := TTimer.Create(Self);
  FLoadingFileListLongTimer.Enabled := False;
  FLoadingFileListLongTimer.Interval := 2000;
  FLoadingFileListLongTimer.OnTimer := @LoadingFileListTimer;

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
  Clear;

  if Assigned(FWorkersThread) then
  begin
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

  inherited Destroy;

  FreeAndNil(FHashedFiles);
  FreeAndNil(FHashedNames);
  FreeAndNil(FHistory);
  FreeAndNil(FSavedSelection);
  FreeAndNil(FPendingFilesChanges);
  FreeAndNil(FRecentlyUpdatedFiles);
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
    AFileView.FFlatView := FFlatView;
    AFileView.FLastLoadedFileSource := FLastLoadedFileSource;
    AFileView.FLastLoadedPath := FLastLoadedPath;
    AFileView.FLastMark := FLastMark;
    AFileView.FLastMarkCaseSensitive := FLastMarkCaseSensitive;
    AFileView.FLastMarkIgnoreAccents := FLastMarkIgnoreAccents;
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
    AFileView.FSortingProperties := GetSortingProperties;
    AFileView.FLastActiveFile := Self.FLastActiveFile;
    AFileView.FRequestedActiveFile := Self.FRequestedActiveFile;
    AFileView.FReloadNeeded := Self.FReloadNeeded;

    if Assigned(Self.FAllDisplayFiles) then
    begin
      AFileView.FAllDisplayFiles := Self.FAllDisplayFiles.Clone(True);
      AFileView.Notify([fvnFileSourceFileListLoaded]);
      AFileView.Request([fvrqHashFileList]);
    end;

    AFileView.FFileFilter := Self.FFileFilter;
    AFileView.FFilterOptions := Self.FFilterOptions;

    // FFiles need to be recreated because the filter is not cloned.
    // This is done in AFileView.UpdateView.
    // UPDATE: Added filter cloning, is the aforementioned statement relevant now?
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

procedure TFileView.ApplyPendingFilesChanges(NewFilesPosition: TNewFilesPosition;
                                             UpdatedFilesPosition: TUpdatedFilesPosition);
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
          HandleFSWatcherEvent(pEvent^, NewFilesPosition, UpdatedFilesPosition);

          // HandleFSWatcherEvent might call Reload which clears FPendingFilesChanges, so check for it.
          if not Assigned(FPendingFilesChanges) then
            Break;
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
  if Assigned(FPendingFilesTimer) then
    FPendingFilesTimer.Enabled := False;

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
  else if FLoadingFileListLongTime then
    Result := DarkColor(AColor, 25)
  else
    Result := AColor;
end;

procedure TFileView.DisplayFileListChanged;
begin
  // Empty.
end;

procedure TFileView.DoActiveChanged;
begin
  // Empty.
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
            ProcessExtCommandFork(CurrentPath + GetActiveDisplayFile.FSFile.Name, '', CurrentPath, nil, True, True);
            Key := 0;
          end;
        end;
      end;
  end;
end;

procedure TFileView.DoHandleKeyDownWhenLoading(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_BACK:
      begin
        ChangePathToParent(True);
        Key := 0;
      end;
  end;
end;

procedure TFileView.DoLoadingFileListLongTime;
begin
  RedrawFiles;
end;

function TFileView.FileListLoaded: Boolean;
begin
  Result := Assigned(FAllDisplayFiles);
end;

procedure TFileView.FileSourceFileListLoaded;
begin
  FLoadingFileListLongTimer.Enabled := False;
end;

procedure TFileView.FileSourceFileListUpdated;
begin
  // Empty.
end;

procedure TFileView.Clear;
var
  i: Integer;
begin
  StopWorkers;

  for i := 0 to FHistory.Count - 1 do
    FHistory.FileSource[i].RemoveReloadEventListener(@ReloadEvent);

  ClearRecentlyUpdatedFiles;
  ClearPendingFilesChanges;
  RemoveAllFileSources;

  FreeAndNil(FFiles);
  FreeAndNil(FAllDisplayFiles);
  HashFileList;
end;

procedure TFileView.ClearFiles;
begin
  if Assigned(FAllDisplayFiles) then
  begin
    ClearRecentlyUpdatedFiles;
    ClearPendingFilesChanges;
    FFiles.Clear;
    FAllDisplayFiles.Clear; // Clear references to files from the source.
    HashFileList;
    Notify([fvnDisplayFileListChanged]);
  end;
end;

function TFileView.GetNotebookPage: TControl;
begin
  if Parent is TFileViewPage then
    Result := TFileViewPage(Parent)
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
      FileSource.RetrieveProperties(AFile, FilePropertiesNeeded, GetVariantFileProperties);
    except
      on EFileSourceException do
        begin
          FreeAndNil(AFile);
          Reload(APath);
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
      Notify([fvnFileSourceFileListUpdated, fvnDisplayFileListChanged]);
    end
    else
      Notify([fvnFileSourceFileListUpdated]);
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
  Notify([fvnFileSourceFileListUpdated, fvnDisplayFileListChanged]);
end;

procedure TFileView.RenameFile(const NewFileName, OldFileName, APath: String; NewFilesPosition: TNewFilesPosition; UpdatedFilesPosition: TUpdatedFilesPosition);
var
  ADisplayFile: TDisplayFile;
  OldIndex, NewIndex: Integer;
  ANotifications: TFileViewNotifications;
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
      ADisplayFile.Selected := False;
      ADisplayFile.IconOverlayID := -1;
      ADisplayFile.TextColor := clNone;
      ADisplayFile.DisplayStrings.Clear;
      ResortFile(ADisplayFile, FAllDisplayFiles);

      ANotifications := [fvnFileSourceFileListUpdated];
      case ApplyFilter(ADisplayFile, NewFilesPosition) of
        fvaprInserted, fvaprRemoved:
          Include(ANotifications, fvnDisplayFileListChanged);
        fvaprExisting:
          begin
            if GetActiveDisplayFile = ADisplayFile then
              RequestedActiveFile := ADisplayFile.FSFile.FullPath;
            ResortFile(ADisplayFile, FFiles);
            VisualizeFileUpdate(ADisplayFile);
            Include(ANotifications, fvnDisplayFileListChanged);
          end;
      end;
      Notify(ANotifications);
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

procedure TFileView.StartUpdatePendingTimer;
begin
  if not Assigned(FPendingFilesTimer) then
  begin
    FPendingFilesTimer := TTimer.Create(Self);
    FPendingFilesTimer.Interval := UpdateFilelistInterval;
    FPendingFilesTimer.OnTimer := @UpdatePendingTimerEvent;
  end;

  FPendingFilesTimer.Enabled := True;
end;

procedure TFileView.UpdateFile(const FileName, APath: String; NewFilesPosition: TNewFilesPosition; UpdatedFilesPosition: TUpdatedFilesPosition);
var
  AFile: TFile;
  ADisplayFile: TDisplayFile;
  I: Integer;
  ANotifications: TFileViewNotifications;

  procedure Resort;
  begin
    ResortFile(ADisplayFile, FAllDisplayFiles);
    ResortFile(ADisplayFile, FFiles);
  end;

  procedure Update;
  begin
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
  end;

begin
  I := FHashedNames.Find(FileName);
  if I >= 0 then
  begin
    ADisplayFile := TDisplayFile(FHashedNames.List[I]^.Data);
    AFile := ADisplayFile.FSFile;
    AFile.ClearProperties;
    try
      FileSource.RetrieveProperties(AFile, FilePropertiesNeeded, GetVariantFileProperties);
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
    ADisplayFile.TextColor := clNone;
    ADisplayFile.IconOverlayID := -1;
    ADisplayFile.DisplayStrings.Clear;

    ANotifications := [fvnFileSourceFileListUpdated];
    case ApplyFilter(ADisplayFile, NewFilesPosition) of
      fvaprInserted, fvaprRemoved:
        Include(ANotifications, fvnDisplayFileListChanged);
      fvaprExisting:
        begin
          Update;
          Include(ANotifications, fvnDisplayFileListChanged);
        end;
    end;
    Notify(ANotifications);
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

procedure TFileView.UpdatePendingTimerEvent(Sender: TObject);
begin
  FPendingFilesTimer.Enabled := False;
  ApplyPendingFilesChanges(gNewFilesPosition, gUpdatedFilesPosition);
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

function TFileView.GetCurrentLocation: String;
begin
  if Length(CurrentAddress) = 0 then
    Result := GetCurrentPath
  else begin
    Result := CurrentAddress;
    if (PathDelim = '/') then
      {%H-}Result += GetCurrentPath
    else
      Result += StringReplace(GetCurrentPath, PathDelim, '/', [rfReplaceAll]);
  end;
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
  if IsLoadingFileList then Exit;

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

procedure TFileView.CancelLastPathChange;
var
  FSIndex, PathIndex: Integer;
begin
  // Get previous entry in history.
  FSIndex := FHistory.CurrentFileSourceIndex;
  PathIndex := FHistory.CurrentPathIndex - 1;
  while PathIndex < 0 do
  begin
    Dec(FSIndex);
    if FSIndex < 0 then
      Break;
    PathIndex := FHistory.PathsCount[FSIndex] - 1;
  end;

  // Go to it if it is the same as last loaded file list.
  if (FSIndex >= 0) and
     FHistory.FileSource[FSIndex].Equals(FLastLoadedFileSource) and
     (FHistory.Path[FSIndex, PathIndex] = FLastLoadedPath) then
  begin
    // Don't reload file list because we already have it.
    Flags := Flags + [fvfDontLoadFiles];
    GoToHistoryIndex(FSIndex, PathIndex);
    Flags := Flags - [fvfDontLoadFiles];
  end
  else
    ClearFiles;
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
    try
      HandleRequests;
      HandleNotifications;
    finally
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
    FFlatView:= False;
    EnableWatcher(False);

    //-- before changing path, remember currently active filename
    //   TODO: move this call to some generic place that is called
    //         ALWAYS when currently selected file is changed
    FHistory.SetFilenameForCurrentPath(GetActiveFileName());
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

procedure TFileView.SetLoadingFileListLongTime(AValue: Boolean);
begin
  FLoadingFileListLongTimer.Enabled := False;
  if FLoadingFileListLongTime <> AValue then
  begin
    FLoadingFileListLongTime := AValue;
    DoLoadingFileListLongTime;
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
begin
  Result := TFiles.Create(CurrentPath);

  for i := 0 to FFiles.Count - 1 do
  begin
    if FFiles[i].Selected then
      Result.Add(FFiles[i].FSFile.Clone);
  end;
end;

function TFileView.CloneSelectedDirectories: TFiles;
var
  i: Integer;
begin
  Result := TFiles.Create(CurrentPath);

  for i := 0 to FFiles.Count - 1 do
  begin
    if FFiles[i].Selected then
      if FFiles[i].FSFile.IsDirectory then
        Result.Add(FFiles[i].FSFile.Clone);
  end;
end;

function TFileView.CloneSelectedOrActiveFiles: TFiles;
var
  aFile: TDisplayFile;
begin
  Result := CloneSelectedFiles;

  Result.Flat := FFlatView;
  // If no files are selected, add currently active file if it is valid.
  if (Result.Count = 0) then
  begin
    aFile := GetActiveDisplayFile;
    if IsItemValid(aFile) then
      Result.Add(aFile.FSFile.Clone);
  end;
end;

function TFileView.CloneSelectedOrActiveDirectories: TFiles;
var
  aFile: TDisplayFile;
begin
  Result := CloneSelectedDirectories;

  // If no directory(ies) is(are) selected, add currently active directory if it is valid.
  if (Result.Count = 0) then
  begin
    aFile := GetActiveDisplayFile;
    if IsItemValid(aFile) then
      if aFile.FSFile.IsDirectory then
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
begin
  MarkFiles(0, FFiles.Count - 1, bSelect);
end;

procedure TFileView.MarkFiles(FromIndex, ToIndex: PtrInt; bSelect: Boolean);
var
  Index: PtrInt;
begin
  BeginUpdate;
  try
    for Index := FromIndex to ToIndex do
      MarkFile(FFiles[Index], bSelect);
  finally
    EndUpdate;
  end;
end;

{ TFileView.MarkApplyOnAllFiles }
procedure TFileView.MarkApplyOnAllFiles(const MarkApplyOnAllDispatcher: TMarkApplyOnAllDispatcher; MarkFileChecks: TFindFileChecks);
var
  Index: PtrInt;
  bInitialValue: boolean;
  bSelected: boolean = False;
begin
  BeginUpdate;
  try
    for Index := 0 to pred(FFiles.Count) do
    begin
      if FFiles[Index].FSFile.Name = '..' then Continue;
      if CheckFileAttributes(MarkFileChecks, FFiles[Index].FSFile.Attributes) then
      begin
        bInitialValue := FFiles[Index].Selected;
        case MarkApplyOnAllDispatcher of
          tmaoa_Mark: FFiles[Index].Selected := True;
          tmaoa_UnMark: FFiles[Index].Selected := False;
          tmaoa_InvertMark: FFiles[Index].Selected := not FFiles[Index].Selected;
        end;
        bSelected := bSelected OR (bInitialValue xor FFiles[Index].Selected);
      end;
    end;

    if bSelected then
      Notify([fvnSelectionChanged]);
  finally
    EndUpdate;
  end;
end;

{ TFileView.MarkGroup (Where we have all the parameters) }
procedure TFileView.MarkGroup(const sMask: String; bSelect: Boolean; pbCaseSensitive:PBoolean = nil; pbIgnoreAccents: PBoolean = nil; pbWindowsInterpretation: PBoolean = nil; pMarkFileChecks: TPFindFileChecks = nil);
var
  I: Integer;
  MaskList: TMaskList;
  SearchTemplate: TSearchTemplate = nil;
  bSelected: Boolean = False;
  bCaseSensitive, bIgnoreAccents, bWindowsInterpretation: boolean;
  LocalMarkFileChecks: TFindFileChecks;
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
    begin
      if pbCaseSensitive <> nil then bCaseSensitive := pbCaseSensitive^ else bCaseSensitive := gbMarkMaskCaseSensitive;
      if pbIgnoreAccents <> nil then bIgnoreAccents := pbIgnoreAccents^ else bIgnoreAccents := gbMarkMaskIgnoreAccents;
      if pbWindowsInterpretation <> nil then bWindowsInterpretation := pbWindowsInterpretation^ else bWindowsInterpretation := gMarkMaskFilterWindows;
      if pMarkFileChecks<> nil then LocalMarkFileChecks:=pMarkFileChecks^ else LocalMarkFileChecks.Attributes:=nil;

      MaskList := TMaskList.Create(sMask, ';,', bCaseSensitive, bIgnoreAccents, bWindowsInterpretation);
      for I := 0 to FFiles.Count - 1 do
      begin
        if FFiles[I].FSFile.Name = '..' then Continue;
        if CheckFileAttributes(LocalMarkFileChecks, FFiles[I].FSFile.Attributes) then
        begin
          if MaskList.Matches(FFiles[I].FSFile.Name) then
          begin
            FFiles[I].Selected := bSelect;
            bSelected := True;
          end;
        end;
      end;
      MaskList.Free;
    end;

    if bSelected then
      Notify([fvnSelectionChanged]);
  finally
    EndUpdate;
  end;
end;

{ TFileView.MarkGroup (Where we prompt the user) }
procedure TFileView.MarkGroup(bSelect: Boolean; pbCaseSensitive:PBoolean = nil; pbIgnoreAccents: PBoolean = nil; pbWindowsInterpretation: PBoolean = nil; psAttribute:PString = nil);
var
  s, ADlgTitle, sAttribute: String;
  bCaseSensitive, bIgnoreAccents, bWindowsInterpretation: boolean;
  MarkSearchTemplateRec: TSearchTemplateRec;
  MarkFileChecks: TFindFileChecks;
begin
  if not IsEmpty then
  begin
    if bSelect then
      ADlgTitle := rsMarkPlus
    else
      ADlgTitle := rsMarkMinus;

    s := FLastMark;
    if pbCaseSensitive <> nil then bCaseSensitive := pbCaseSensitive^ else bCaseSensitive := FLastMarkCaseSensitive;
    if pbIgnoreAccents <> nil then bIgnoreAccents := pbIgnoreAccents^ else bIgnoreAccents := FLastMarkIgnoreAccents;
    if pbWindowsInterpretation <> nil then bWindowsInterpretation := pbWindowsInterpretation^ else bWindowsInterpretation := gMarkMaskFilterWindows;
    if psAttribute <> nil then sAttribute := psAttribute^ else
      if not gMarkShowWantedAttribute then sAttribute:=gMarkDefaultWantedAttribute else sAttribute := gMarkLastWantedAttribute;

    if ShowExtendedMaskInputDlg(ADlgTitle, rsMaskInput, glsMaskHistory, s, midsFull, bCaseSensitive, bIgnoreAccents, sAttribute) then
      begin
        FLastMark := s;
        FLastMarkCaseSensitive := bCaseSensitive;
        FLastMarkIgnoreAccents := bIgnoreAccents;
        gbMarkMaskCaseSensitive := bCaseSensitive;
        gbMarkMaskIgnoreAccents := bIgnoreAccents;
        if (psAttribute = nil) AND gMarkShowWantedAttribute then gMarkLastWantedAttribute:=sAttribute;

        MarkSearchTemplateRec.AttributesPattern := sAttribute;
        AttrsPatternOptionsToChecks(MarkSearchTemplateRec, MarkFileChecks);

        MarkGroup(s, bSelect, @bCaseSensitive, @bIgnoreAccents, @bWindowsInterpretation, @MarkFileChecks);
      end;
  end;
end;

procedure TFileView.MarkCurrentExtension(bSelect: Boolean);
var
  sGroup: String;
  bCaseSensitive: boolean = false;
  bIgnoreAccents: boolean = false;
  bWindowsInterpretation: boolean = false;
begin
  if IsActiveItemValid then
  begin
    sGroup := GetActiveDisplayFile.FSFile.Extension;
    if sGroup <> '' then
      sGroup := '.' + sGroup;
    MarkGroup('*' + sGroup, bSelect, @bCaseSensitive, @bIgnoreAccents, @bWindowsInterpretation);
  end;
end;

procedure TFileView.MarkCurrentPath(bSelect: Boolean);
var
  I: Integer;
  sPath: String;
  bSelected: Boolean = False;
begin
  if IsActiveItemValid then
  begin
    sPath := GetActiveDisplayFile.FSFile.Path;

    BeginUpdate;
    try
      for I := 0 to FFiles.Count - 1 do
      begin
        if FFiles[I].FSFile.IsDirectory then Continue;

        if mbCompareFileNames(FFiles[I].FSFile.Path, sPath) then
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

  if UpdatedFile.TextColor <> clNone then
    OrigDisplayFile.TextColor := UpdatedFile.TextColor;

  DoFileUpdated(OrigDisplayFile);
end;

function TFileView.GetActiveFileName: String;
var
  aFile: TDisplayFile;
begin
  aFile := GetActiveDisplayFile;

  if Assigned(aFile) then
    Result := aFile.FSFile.Name
  else
    Result := '';
end;

procedure TFileView.SetActiveFile(const aFile: TFile);
begin
end;

procedure TFileView.SetActiveFile(aFilePath: String);
begin
end;

procedure TFileView.ChangePathAndSetActiveFile(aFilePath: String);
begin
end;

procedure TFileView.SetActive(bActive, bNotify: Boolean);
begin
  if FActive <> bActive then
  begin
    FActive := bActive;
    DoActiveChanged;
  end;

  if bActive and bNotify then
  begin
    // Deactivate all other views.
    frmMain.ForEachView(@EachViewDeactivate, nil);

    if Assigned(OnActivate) then
      OnActivate(Self);
  end;
end;

procedure TFileView.SetActive(bActive: Boolean);
begin
  SetActive(bActive, True);
end;

procedure TFileView.JustForColorPreviewSetActiveState(bActive: Boolean);
begin
  SetActive(bActive, False);
end;

procedure TFileView.SetSorting(const NewSortings: TFileSortings);
var
  SortingProperties: TFilePropertiesTypes;
begin
  FSortings := CloneSortings(NewSortings);
  if not IsLoadingFileList then
  begin
    SortingProperties:= GetSortingProperties;
    // Force reload if new sorting properties needed
    FForceReload:= (SortingProperties <> []) and (SortingProperties <> FSortingProperties);
    FSortingProperties:= SortingProperties;
    if FForceReload then
      Reload()
    else begin
      SortAllDisplayFiles;
      ReDisplayFileList;
    end;
  end;
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
  DisplayFilesHashed: TStringHashListUtf8 = nil;
  i: Integer;
begin
  if (csDestroying in ComponentState) or (FileSourcesCount = 0) or
     ([fvfDelayLoadingFiles, fvfDontLoadFiles] * Flags <> []) then
    Exit;

  {$IFDEF timeFileView}
  filelistTime := GetTickCount64;
  filelistPrevTime := filelistTime;
  filelistLoaderTime := filelistTime;
  DCDebug('--------- Start ---------');
  {$ENDIF}

  StopWorkers;

  if gListFilesInThread and not (fspListOnMainThread in FileSource.GetProperties) then
    AThread := GetWorkersThread;

  if FileSource.Equals(FLastLoadedFileSource) and
     (FLastLoadedPath = CurrentPath) and
     (FAllDisplayFiles.Count > 0) and
     (FForceReload = False) then
  begin
    // Clone all properties of display files, but don't clone the FS files
    // themselves because new ones will be retrieved from FileSource.
    ClonedDisplayFiles := FAllDisplayFiles.Clone(False);
    DisplayFilesHashed := TStringHashListUtf8.Create(True);
    // Map filename to display file.
    for i := 0 to FAllDisplayFiles.Count - 1 do
      DisplayFilesHashed.Add(FAllDisplayFiles[i].FSFile.FullPath, ClonedDisplayFiles[i]);
  end;

  // Drop FForceReload flag
  FForceReload := False;

  Worker := TFileListBuilder.Create(
    FileSource,
    CurrentFileSourceIndex,
    FileFilter,
    FilterOptions,
    CurrentPath,
    SortingForSorter,
    FlatView,
    AThread,
    FSortingProperties,
    GetVariantFileProperties,
    @SetFileList,
    ClonedDisplayFiles,
    DisplayFilesHashed);

  AddWorker(Worker);

  ClearPendingFilesChanges;

  if gListFilesInThread and not (fspListOnMainThread in FileSource.GetProperties) then
  begin
    ClearRecentlyUpdatedFiles;
    BeforeMakeFileList;
    AThread.QueueFunction(@Worker.StartParam);
  end
  else
  begin
    BeforeMakeFileList;
    Worker.Start;
  end;
end;

function TFileView.ApplyFilter(ADisplayFile: TDisplayFile; NewFilesPosition: TNewFilesPosition): TFileViewApplyFilterResult;
var
  bFilterOut: Boolean;
  FilteredFilesIndex: Integer;
begin
  bFilterOut := TFileListBuilder.MatchesFilter(ADisplayFile.FSFile, FileFilter, FFilterOptions);
  FilteredFilesIndex := FFiles.Find(ADisplayFile);
  if FilteredFilesIndex >= 0 then
  begin
    if bFilterOut then
    begin
      FFiles.Delete(FilteredFilesIndex);
      if Assigned(FRecentlyUpdatedFiles) then
        FRecentlyUpdatedFiles.Remove(ADisplayFile);
      Result := fvaprRemoved;
    end
    else
      Result := fvaprExisting;
  end
  else if not bFilterOut then
  begin
    InsertFile(ADisplayFile, FFiles, NewFilesPosition);
    VisualizeFileUpdate(ADisplayFile);
    Result := fvaprInserted;
  end
  else
    Result := fvaprNotExisting;
end;

procedure TFileView.BeforeMakeFileList;
begin
  FLoadingFileListLongTimer.Enabled := True;
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
  if Assigned(AFile) and not IsLoadingFileList then
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
          uFileSourceUtil.ChooseFile(Self, FileSource, FSFile);
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

procedure TFileView.EachViewDeactivate(AFileView: TFileView; UserData: Pointer);
var
  ThisFileViewPage, OtherFileViewPage: TFileViewPage;
begin
  if AFileView <> Self then
  begin
    ThisFileViewPage  := TFileViewPage(GetNotebookPage);
    OtherFileViewPage := TFileViewPage(AFileView.GetNotebookPage);

    // Pages on the same notebook set to active and others to not active.
    if Assigned(ThisFileViewPage) and Assigned(OtherFileViewPage) then
      AFileView.SetActive(ThisFileViewPage.Notebook = OtherFileViewPage.Notebook, False);
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
  FHashedFiles.Free;
  // TBucketList seems to do fairly well without needing a proper hash table.
  FHashedFiles := TBucketList.Create(bl256);
  FHashedNames.Clear;
  if Assigned(FAllDisplayFiles) then
  begin
    for i := 0 to FAllDisplayFiles.Count - 1 do
    begin
      FHashedFiles.Add(FAllDisplayFiles[i], nil);
      FHashedNames.Add(FAllDisplayFiles[i].FSFile.Name, FAllDisplayFiles[i]);
    end;
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

function TFileView.IsLoadingFileList: Boolean;
begin
  Result := GetCurrentWorkType = fvwtCreate;
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

procedure TFileView.Reload(AForced: Boolean);
begin
  FForceReload:= AForced;
  DoReload;
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
  SetLoadingFileListLongTime(False);
end;

procedure TFileView.LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
var
  HistoryNode, EntryNode, FSNode, PathsNode: TXmlNode;
  SortingsNode, SortingSubNode, SortFunctionNode: TXmlNode;
  FileSourceClass: TFileSourceClass;
  sFSType, sPath, sFilename: String;
  aFileSource: IFileSource = nil;
  ActiveFSIndex: Integer = -1;
  ActivePathIndex: Integer = -1;
  NewSorting: TFileSortings = nil;
  SortDirection: TSortDirection;
  SortFunctions: TFileFunctions;
  SortFunctionInt: Integer;
  APage: TFileViewPage;
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
              aFileSource := TFileSystemFileSource.GetFileSource
            else begin
              FileSourceClass := gVfsModuleList.FindFileSource(sFSType);
              if Assigned(FileSourceClass) then aFileSource := FileSourceClass.Create;
            end;

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

                    if sPath <> EmptyStr then
                    begin
                      FHistory.AddPath(sPath);

                      if AConfig.GetAttr(PathsNode, 'Active', False) then
                        ActivePathIndex := FHistory.PathsCount[FHistory.Count - 1] - 1;

                      //-- if selected filename is specified in xml file, load it too
                      if AConfig.TryGetAttr(PathsNode, 'Filename', sFilename) then
                      begin
                        FHistory.SetFilenameForCurrentPath(sFilename);
                      end

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

  aFileSource:= GetCurrentFileSource;

  if Assigned(aFileSource) and TFileSystemFileSource.ClassNameIs(aFileSource.ClassName) then
  begin
    APage := TFileViewPage(NotebookPage);
    // Go to lock path if tab is locked
    if Assigned(APage) and (APage.LockState <> tlsNormal) then
    begin
      if not mbCompareFileNames(FHistory.CurrentPath, APage.LockPath) then
        FHistory.Add(aFileSource, APage.LockPath);
    end;
    // Go to upper directory if current doesn't exist
    sPath := GetDeepestExistingPath(FHistory.CurrentPath);
    if Length(sPath) = 0 then sPath := mbGetCurrentDir;
    if not mbCompareFileNames(sPath, FHistory.CurrentPath) then
      FHistory.Add(aFileSource, sPath);
  end;

  if Assigned(aFileSource) then
  begin
    FSortingProperties := GetSortingProperties;
    FileSource.AddReloadEventListener(@ReloadEvent);
  end;

  //TODO: probably it's not the best place for calling SetActiveFile() :
  //      initially-active file should be set in the same place where
  //      initial path is set
  SetActiveFile(FHistory.CurrentFilename);

  // No automatic reload here.
end;

procedure TFileView.LoadingFileListTimer(Sender: TObject);
begin
  SetLoadingFileListLongTime(True);
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

procedure TFileView.MarkCurrentName(bSelect: Boolean);
var
  sGroup: String;
  bCaseSensitive: boolean = false;
  bIgnoreAccents: boolean = false;
  bWindowsInterpretation: boolean = True;
begin
  if IsActiveItemValid then
  begin
    sGroup := GetActiveDisplayFile.FSFile.NameNoExt;
    if Length(sGroup) > 0 then sGroup += ExtensionSeparator + '*';
    MarkGroup(sGroup, bSelect, @bCaseSensitive, @bIgnoreAccents, @bWindowsInterpretation);
  end;
end;

procedure TFileView.MarkCurrentNameExt(bSelect: Boolean);
var
  sGroup: String;
  bCaseSensitive: boolean = False;
  bIgnoreAccents: boolean = False;
  bWindowsInterpretation: boolean = False;
begin
  if IsActiveItemValid then
  begin
    sGroup := GetActiveDisplayFile.FSFile.Name;
    MarkGroup(sGroup, bSelect, @bCaseSensitive, @bIgnoreAccents, @bWindowsInterpretation);
  end;
end;

procedure TFileView.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode; ASaveHistory:boolean);
var
  HistoryNode, EntryNode, FSNode, PathsNode, PathNode: TXmlNode;
  SortingsNode, SortingSubNode: TXmlNode;
  i, j: Integer;
  PathIndex: Integer;
  ASorting: TFileSortings;
begin
  //-- remember currently active filename
  //   TODO: move this call to some generic place that is called
  //         ALWAYS when currently selected file is changed
  if not (fvfDelayLoadingFiles in Flags) then
    FHistory.SetFilenameForCurrentPath(GetActiveFileName());

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
    if FHistory.FileSource[i].IsClass(TFileSystemFileSource) then
    begin
      EntryNode := AConfig.AddNode(HistoryNode, 'Entry');
      if FHistory.CurrentFileSourceIndex = i then
        AConfig.SetAttr(EntryNode, 'Active', True);

      FSNode := AConfig.AddNode(EntryNode, 'FileSource');
      if TFileSystemFileSource.ClassNameIs(FHistory.FileSource[i].ClassName) then
        AConfig.SetAttr(FSNode, 'Type', 'FileSystem')
      else begin
        AConfig.SetAttr(FSNode, 'Type', FHistory.FileSource[i].ClassName);
      end;

      // Save paths history.
      PathsNode := AConfig.AddNode(EntryNode, 'Paths');
      if ASaveHistory then
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

          //-- set path
          AConfig.SetContent(PathNode, FHistory.Path[i, j]);

          //-- set selected filename
          AConfig.SetAttr(PathNode, 'Filename', FHistory.Filename[i, j]);
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
  AllowChangingFileSource:= AllowChangingFileSource and
                            not (fspNoneParent in FileSource.Properties);
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
    // Workaround for Search Result File Source
    if FileSource is TSearchResultFileSource then
      SetFileSystemPath(Self, aFile.FullPath)
    else
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
    FFlatView := False;

    if Assigned(FileSource) and IsNewFileSource then
      FileSource.RemoveReloadEventListener(@ReloadEvent);

    EnableWatcher(False);

    FHistory.Add(aFileSource, aPath);

    AfterChangePath;

    if Assigned(FileSource) and IsNewFileSource then
    begin
      UpdatePath(True);
      FileSource.AddReloadEventListener(@ReloadEvent);
    end;

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
    FFlatView := False;
    // TODO: Do this by remembering focused file name in a list?
    FocusedFile := ExtractFileName(FileSource.CurrentAddress);

    PrevIndex := FHistory.CurrentFileSourceIndex - 1;
    if PrevIndex < 0 then
      begin
        FileSource.RemoveReloadEventListener(@ReloadEvent);
        EnableWatcher(False);

        FHistory.Clear;
        AfterChangePath;
      end
    else
      begin
        NewFileSource := FHistory.FileSource[PrevIndex];
        NewPath := FHistory.Path[PrevIndex, FHistory.PathsCount[PrevIndex] - 1];

        if BeforeChangePath(NewFileSource, NewPath) then
        begin
          IsNewFileSource := not NewFileSource.Equals(FileSource);

          if IsNewFileSource then
            FileSource.RemoveReloadEventListener(@ReloadEvent);

          EnableWatcher(False);

          FHistory.DeleteFromCurrentFileSource;

          AfterChangePath;

          if Assigned(FileSource) and IsNewFileSource then
          begin
            UpdatePath(True);
            FileSource.AddReloadEventListener(@ReloadEvent);
          end;

          EnableWatcher(True);

          SetActiveFile(FocusedFile);

          {$IFDEF DEBUG_HISTORY}
          FHistory.DebugShow;
          {$ENDIF}
        end;
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

function TFileView.GetPath(FileSourceIndex, PathIndex: Integer): String;
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

function TFileView.GetSortingProperties: TFilePropertiesTypes;
var
  I, J: Integer;
begin
  Result:= [];
  // Retrieve RetrievableFileProperties which used in sorting
  for I:= Low(FSortings) to High(FSortings) do
  begin
    for J:= Low(FSortings[I].SortFunctions) to High(FSortings[I].SortFunctions) do
    begin
      Result:= Result + GetFilePropertyType(FSortings[I].SortFunctions[J]);
    end;
  end;
  Result:= (Result - FileSource.SupportedFileProperties) * FileSource.RetrievableFileProperties;
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
  try
    while FNotifications <> [] do
    begin
      if fvnFileSourceFileListLoaded in FNotifications then
      begin
        FNotifications := FNotifications - [fvnFileSourceFileListLoaded];
        FileSourceFileListLoaded;
        DoOnFileListChanged;
      end
      else if fvnFileSourceFileListUpdated in FNotifications then
      begin
        FNotifications := FNotifications - [fvnFileSourceFileListUpdated];
        FileSourceFileListUpdated;
        DoOnFileListChanged;
      end
      else if fvnDisplayFileListChanged in FNotifications then
      begin
        FNotifications := FNotifications - [fvnDisplayFileListChanged];
        DisplayFileListChanged;
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
  end;
end;

procedure TFileView.HandleRequests;
begin
  BeginUpdate;
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
        ApplyPendingFilesChanges(nfpSortedPosition, ufpSortedPosition);
      end
      else if fvrqMakeDisplayFileList in FRequests then
      begin
        FRequests := FRequests - [fvrqMakeDisplayFileList];
        ReDisplayFileList;
      end;
    end;
  finally
    EndUpdate;
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

procedure TFileView.SetFileList(var NewAllDisplayFiles: TDisplayFiles;
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
    Notify([fvnFileSourceFileListLoaded, fvnDisplayFileListChanged]);
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
        if IsInPathList(gWatchDirsExclude, CurrentPath) then
          Exit;
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

procedure TFileView.HandleKeyDownWhenLoading(var Key: Word; Shift: TShiftState);
begin
  // Only allow some keys and always zero Key (handled).
  DoHandleKeyDownWhenLoading(Key, Shift);
  Key := 0;
end;

procedure TFileView.ReloadEvent(const aFileSource: IFileSource; const ReloadedPaths: TPathsArray);
var
  NoWatcher: Boolean;
begin
  if aFileSource.Equals(FileSource) then
  begin
    // Reload file view but only if the file source is
    // currently viewed and FileSystemWatcher is not being used.
    NoWatcher:= not (WatcherActive and
                     TFileSystemWatcher.CanWatch(ReloadedPaths) and
                     TFileSystemFileSource.ClassNameIs(FileSource.ClassName)
                     );
    if (NoWatcher or FlatView) then Reload(ReloadedPaths);
  end;
end;

procedure TFileView.ReloadTimerEvent(Sender: TObject);
begin
  FReloadTimer.Enabled := False;
  DoReload;
end;

procedure TFileView.WatcherEvent(const EventData: TFSWatcherEventData);
var
  CurrentTime: TDateTime;
  AddToPending: Boolean;
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
      begin
        AddToPending := Assigned(FPendingFilesTimer) and FPendingFilesTimer.Enabled;
        if not AddToPending then
        begin
          CurrentTime := SysUtils.Now;
          if DateTimeToTimeStamp(CurrentTime - FWatcherEventLastTime).Time > UpdateFilelistInterval then
            FWatcherEventsApplied := 0;

          FWatcherEventLastTime := CurrentTime;
          if FWatcherEventsApplied < 5 then
          begin
            Inc(FWatcherEventsApplied);
            HandleFSWatcherEvent(EventData, gNewFilesPosition, gUpdatedFilesPosition);
          end
          else
            AddToPending := True;
        end;

        if AddToPending then
        begin
          AddEventToPendingFilesChanges(EventData);
          StartUpdatePendingTimer;
        end;
      end
      // else filelist not loaded and not even started loading - discard the event
    end;
  end;
end;

procedure TFileView.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

function TFileView.GetVariantFileProperties: TDynamicStringArray;
begin
  SetLength(Result, 0);
end;

procedure TFileView.GoToHistoryIndex(aFileSourceIndex, aPathIndex: Integer);
var
  IsNewFileSource: Boolean;
  FilenameFromHistory: String;
begin
  //-- before changing path, remember currently active filename
  //   TODO: move this call to some generic place that is called
  //         ALWAYS when currently selected file is changed
  FHistory.SetFilenameForCurrentPath(GetActiveFileName());

  IsNewFileSource := not FHistory.FileSource[aFileSourceIndex].Equals(FHistory.CurrentFileSource);

  if BeforeChangePath(FHistory.FileSource[aFileSourceIndex],
                      FHistory.Path[aFileSourceIndex, aPathIndex]) then
  begin
    FFlatView := False;

    FilenameFromHistory := FHistory.Filename[aFileSourceIndex, aPathIndex];

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

    if FilenameFromHistory <> '' then
      SetActiveFile(FilenameFromHistory)

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

    SetLoadingFileListLongTime(False);
  end;

  if (Worker is TCalculateSpaceWorker) and (Worker.Aborted = False) then
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
                  aTargetFileSource: IFileSource;
                  aTargetPath: String);
begin
  Files := aFiles;
  aFiles := nil;
  DropEffect := aDropEffect;
  ScreenDropPoint := aScreenDropPoint;
  DropIntoDirectories := aDropIntoDirectories;
  SourcePanel := aSourcePanel;
  TargetPanel := aTargetPanel;
  TargetFileSource := aTargetFileSource;
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

