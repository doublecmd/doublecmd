unit uFileView; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls,
  uFile, uFileSource, uMethodsList, uDragDropEx, uXmlConfig, uClassesEx,
  uFileSorting, uFileViewHistory;

type

  TFileView = class;

  TOnBeforeChangeDirectory = function (FileView: TFileView; const NewDir : String): Boolean of object;
  TOnAfterChangeDirectory = procedure (FileView: TFileView; const NewDir : String) of object;
  TOnChangeActiveFile = procedure (FileView: TFileView; const aFile : TFile) of object;
  TOnChangeFileSource = procedure (FileView: TFileView) of object;
  TOnActivate = procedure (aFileView: TFileView) of object;
  TOnReload = procedure (aFileView: TFileView) of object;

  TDropParams = class;
  TDragDropType = (ddtInternal, ddtExternal);
  // Lists all operations supported by dragging and dropping items
  // in the panel (external, internal and via menu).
  TDragDropOperation = (ddoCopy, ddoMove, ddoSymLink, ddoHardLink);

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
    FLastActiveFile: String;      //<en Last active file (cursor)
    {en
       File name which should be selected. Sometimes the file might not yet
       exist in the filelist (for example after rename or create), but will be
       in the list on next reload.
    }
    FRequestedActiveFile: String;

    FMethods: TMethodsList;

    FOnBeforeChangeDirectory : TOnBeforeChangeDirectory;
    FOnAfterChangeDirectory : TOnAfterChangeDirectory;
    FOnChangeActiveFile: TOnChangeActiveFile;
    FOnChangeFileSource : TOnChangeFileSource;
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

    procedure ReloadEvent(const aFileSource: IFileSource; const ReloadedPaths: TPathsArray);

  protected
    {en
       Initializes parts of the view common to all creation methods.
    }
    procedure CreateDefault(AOwner: TWinControl); virtual;

    function GetCurrentPath: String; virtual;
    procedure SetCurrentPath(NewPath: String); virtual;
    function GetActiveFile: TFile; virtual;
    {en
       This function should set active file by reference of TFile
       or at least by all the properties of the given TFile,
       in case the object is a clone.
       It could be useful in case there are multiple files with the
       same name in the panel and SetActiveFile(String) is not enough.
    }
    procedure SetActiveFile(const aFile: TFile); virtual; overload;
    function GetDisplayedFiles: TFiles; virtual abstract;
    function GetSelectedFiles: TFiles; virtual abstract;
    procedure SetSorting(NewSortings: TFileSortings); virtual;

    {en
       Called before changing path. If returns @false the path is not changed.
    }
    function BeforeChangePath(NewPath: String): Boolean; virtual;
    {en
       Called after path is changed.
    }
    procedure AfterChangePath(NewPath: String); virtual;

    property LastActiveFile: String read FLastActiveFile write FLastActiveFile;
    property RequestedActiveFile: String read FRequestedActiveFile write FRequestedActiveFile;

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
    procedure RemoveLastFileSource; virtual;
    procedure RemoveAllFileSources; virtual;
    {en
       Assigns the list of file sources and paths into those file sources
       from another file view.
    }
    procedure AssignFileSources(const otherFileView: TFileView); virtual;

    // Retrieves files from file source again and displays the new list of files.
    procedure Reload(const PathsToReload: TPathsArray = nil); virtual abstract;
    procedure StopBackgroundWork; virtual;

    // For now we use here the knowledge that there are tabs.
    // Config should be independent of that in the future.
    procedure LoadConfiguration(Section: String; TabIndex: Integer); virtual abstract;
    procedure SaveConfiguration(Section: String; TabIndex: Integer); virtual abstract;
    procedure LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); virtual abstract;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); virtual abstract;

    procedure UpdateView; virtual abstract;

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
    function HasSelectedFiles: Boolean; virtual abstract;

    {en
       Sorts files in FilesToSort using ASorting.
    }
    class procedure Sort(FilesToSort: TFiles; ASortings: TFileSortings);

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
    property FileSource: IFileSource read GetCurrentFileSource;
    property FileSources[Index: Integer]: IFileSource read GetFileSource;
    property FileSourcesCount: Integer read GetFileSourcesCount;
    property Path[FileSourceIndex, PathIndex: Integer]: UTF8String read GetPath;
    property PathsCount[FileSourceIndex: Integer]: Integer read GetPathsCount;

    {en
       Currently active file.
       There should always be at least one file in the view at any time, but
       what 'active' means depends on the specific view, so ActiveFile may
       return 'nil' if there is no file active. Usually it is the file pointed
       to by a cursor or some other indicator.
    }
    property ActiveFile: TFile read GetActiveFile write SetActiveFile;
    {en
       A list of currently displayed files.
       Caller is responsible for freeing the list.
    }
    property Files: TFiles read GetDisplayedFiles;
    {en
       A list of files selected by the user
       (this should be a subset of displayed files list returned by Files).
       Caller is responsible for freeing the list.
    }
    property SelectedFiles: TFiles read GetSelectedFiles;
    property Sorting: TFileSortings read FSortings write SetSorting;

    property NotebookPage: TCustomPage read GetNotebookPage;
    property OnBeforeChangeDirectory : TOnBeforeChangeDirectory read FOnBeforeChangeDirectory write FOnBeforeChangeDirectory;
    property OnAfterChangeDirectory : TOnAfterChangeDirectory read FOnAfterChangeDirectory write FOnAfterChangeDirectory;
    property OnChangeActiveFile : TOnChangeActiveFile read FOnChangeActiveFile write FOnChangeActiveFile;
    property OnChangeFileSource : TOnChangeFileSource read FOnChangeFileSource write FOnChangeFileSource;
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
  uActs, uLng, uShowMsg;

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
  FOnBeforeChangeDirectory := nil;
  FOnAfterChangeDirectory := nil;
  FOnChangeActiveFile := nil;
  FOnChangeFileSource := nil;
  FOnActivate := nil;
  FOnReload := nil;
  FSortings := nil;
  FMethods := TMethodsList.Create(Self);
  FHistory := TFileViewHistory.Create;
  FLastActiveFile := '';
  FRequestedActiveFile := '';

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
    AFileView.OnBeforeChangeDirectory := Self.OnBeforeChangeDirectory;
    AFileView.OnAfterChangeDirectory := Self.OnAfterChangeDirectory;
    AFileView.OnChangeFileSource := Self.OnChangeFileSource;
    AFileView.OnActivate := Self.OnActivate;
    AFileView.OnReload := Self.OnReload;

    AFileView.FHistory.Assign(Self.FHistory);
    AFileView.FSortings := CloneSortings(Self.FSortings);
    AFileView.FLastActiveFile := Self.FLastActiveFile;
    AFileView.FRequestedActiveFile := Self.FRequestedActiveFile;
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

function TFileView.GetCurrentPath: String;
begin
  Result := FHistory.CurrentPath;
end;

procedure TFileView.SetCurrentPath(NewPath: String);
begin
  if BeforeChangePath(NewPath) then
  begin
    FHistory.AddPath(NewPath); // Sets CurrentPath.
    AfterChangePath(NewPath);
    {$IFDEF DEBUG_HISTORY}
    FHistory.DebugShow;
    {$ENDIF}
  end;
end;

function TFileView.GetActiveFile: TFile;
begin
  Result := nil;
end;

procedure TFileView.SetActiveFile(const aFile: TFile);
begin
end;

procedure TFileView.SetActiveFile(aFilePath: String);
begin
end;

procedure TFileView.SetSorting(NewSortings: TFileSortings);
begin
  FSortings := CloneSortings(NewSortings);
end;

procedure TFileView.StopBackgroundWork;
begin
end;

function TFileView.BeforeChangePath(NewPath: String): Boolean;
begin
  if NewPath <> '' then
  begin
    if Assigned(OnBeforeChangeDirectory) then
      if not OnBeforeChangeDirectory(Self, NewPath) then
        Exit(False);

    if not FileSource.SetCurrentWorkingDirectory(NewPath) then
    begin
      msgError(Format(rsMsgChDirFailed, [NewPath]));
      Exit(False);
    end;

    Result := True;
  end
  else
    Result := False;
end;

procedure TFileView.AfterChangePath(NewPath: String);
begin
  LastActiveFile := '';
  RequestedActiveFile := '';

  if Assigned(OnAfterChangeDirectory) then
    OnAfterChangeDirectory(Self, NewPath);
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
      RemoveLastFileSource;
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
begin
  if Assigned(FileSource) then
    FileSource.RemoveReloadEventListener(@ReloadEvent);
  FHistory.Add(aFileSource, aPath);
  Reload;
  UpdateView;
  FileSource.AddReloadEventListener(@ReloadEvent);
  {$IFDEF DEBUG_HISTORY}
  FHistory.DebugShow;
  {$ENDIF}
end;

procedure TFileView.RemoveLastFileSource;
begin
  if FileSourcesCount > 0 then
  begin
    FileSource.RemoveReloadEventListener(@ReloadEvent);
    FHistory.DeleteFromCurrentFileSource;
    Reload;
    UpdateView;
    FileSource.AddReloadEventListener(@ReloadEvent);
    {$IFDEF DEBUG_HISTORY}
    FHistory.DebugShow;
    {$ENDIF}
  end;
end;

procedure TFileView.RemoveAllFileSources;
begin
  if FileSourcesCount > 0 then
  begin
    FileSource.RemoveReloadEventListener(@ReloadEvent);
    FHistory.Clear;
  end;
end;

procedure TFileView.AssignFileSources(const otherFileView: TFileView);
begin
  FileSource.RemoveReloadEventListener(@ReloadEvent);
  FHistory.Assign(otherFileView.FHistory);
  Reload;
  UpdateView;
  FileSource.AddReloadEventListener(@ReloadEvent);
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

procedure TFileView.ReloadEvent(const aFileSource: IFileSource; const ReloadedPaths: TPathsArray);
begin
  // Reload file view, but only if the file source is currently viewed.
  if aFileSource = FileSource then
    Reload(ReloadedPaths);
end;

class procedure TFileView.Sort(FilesToSort: TFiles; ASortings: TFileSortings);
var
  FileListSorter: TListSorter;
  ASortingsCopy: TFileSortings;
begin
  ASortingsCopy := CloneSortings(ASortings);

  // Add automatic sorting by name and/or extension if there wasn't any.
  AddSortingByNameIfNeeded(ASortingsCopy);

  FileListSorter := TListSorter.Create(FilesToSort, ASortingsCopy);
  try
    FileListSorter.Sort;
  finally
    FreeAndNil(FileListSorter);
  end;
end;

procedure TFileView.GoToHistoryIndex(aFileSourceIndex, aPathIndex: Integer);
var
  OldFileSourceIndex, OldPathIndex: Integer;
begin
  if aFileSourceIndex <> FHistory.CurrentFileSourceIndex then
  begin
    OldFileSourceIndex := FHistory.CurrentFileSourceIndex;
    OldPathIndex := FHistory.CurrentPathIndex;

    FileSource.RemoveReloadEventListener(@ReloadEvent);
    FHistory.SetIndexes(aFileSourceIndex, aPathIndex);
    if BeforeChangePath(FHistory.CurrentPath) then
    begin
      Reload;
      UpdateView;
    end
    else
      FHistory.SetIndexes(OldFileSourceIndex, OldPathIndex);
    FileSource.AddReloadEventListener(@ReloadEvent);
  end
  else if aPathIndex <> FHistory.CurrentPathIndex then
  begin
    if BeforeChangePath(FHistory.Path[aFileSourceIndex, aPathIndex]) then
    begin
      FHistory.SetIndexes(aFileSourceIndex, aPathIndex);
      AfterChangePath(FHistory.Path[aFileSourceIndex, aPathIndex]);
    end;
  end;

  {$IFDEF DEBUG_HISTORY}
  FHistory.DebugShow;
  {$ENDIF}
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

