unit uFileViewWorker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, syncobjs, StringHashList,
  uDisplayFile, uFile, uFileSource, uFileSorting, uFileProperty,
  uFileSourceOperation,
  uFileSourceListOperation,
  fQuickSearch;

type
  TFileViewWorkType = (fvwtNone,
                       fvwtCreate,  // Creates file list
                       fvwtUpdate); // Updates file list

  TFileViewWorker = class;

  TStartingWorkMethod = procedure (const Worker: TFileViewWorker) of object;
  TFinishedWorkMethod = procedure (const Worker: TFileViewWorker) of object;

  { TFileViewWorker }

  TFileViewWorker = class
  strict private
    FAborted: Boolean;
    FCanBeDestroyed: Boolean;
    FWorking: Boolean;
    FOnStarting: TStartingWorkMethod;
    FOnFinished: TFinishedWorkMethod;
    FThread: TThread;
    procedure DoFinished;
    procedure DoStarting;
  protected
    FWorkType: TFileViewWorkType;
    procedure DoneWorking;
    procedure Execute; virtual; abstract;
    function IsWorking: Boolean; virtual;
    property Thread: TThread read FThread;
  public
    constructor Create(AThread: TThread); virtual;
    procedure Abort; virtual;
    procedure Start;
    procedure StartParam(Params: Pointer);
    property Aborted: Boolean read FAborted;
    property CanBeDestroyed: Boolean read FCanBeDestroyed;
    property OnFinished: TFinishedWorkMethod read FOnFinished write FOnFinished;
    property OnStarting: TStartingWorkMethod read FOnStarting write FOnStarting;
    property Working: Boolean read IsWorking;
    property WorkType: TFileViewWorkType read FWorkType;
  end;

  TFVWorkerFileList = class
  private
    FFiles: TFPObjectList;
    FUserData: TFPList;
    function GetCount: Integer;
    function GetFile(Index: Integer): TDisplayFile;
    function GetData(Index: Integer): Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddClone(const AFile: TDisplayFile; UserData: Pointer): Integer;
    property Count: Integer read GetCount;
    property Files[Index: Integer]: TDisplayFile read GetFile;
    property Data[Index: Integer]: Pointer read GetData;
  end;

  TSetFileListMethod = procedure (var NewAllDisplayFiles: TDisplayFiles;
                                  var NewFilteredDisplayFiles: TDisplayFiles) of object;
  TUpdateFileMethod = procedure (const UpdatedFile: TDisplayFile;
                                 const UserData: Pointer) of object;

  { TFileListBuilder }

  TFileListBuilder = class(TFileViewWorker)
  private
    FFilteredDisplayFiles: TDisplayFiles;
    FAllDisplayFiles: TDisplayFiles;
    FExistingDisplayFilesHashed: TStringHashList;
    FSetFileListMethod: TSetFileListMethod;
    FListOperation: TFileSourceListOperation;
    FListOperationLock: TCriticalSection;

    // Data captured from the file view before start.
    FFileSource: IFileSource;
    FFileSourcesCount: Integer;
    FFileFilter: String;
    FFilterOptions: TQuickSearchOptions;
    FCurrentPath: String;
    FSortings: TFileSortings;
    FFilePropertiesNeeded: TFilePropertiesTypes;

    {en
       Calls the update method with the new built lists.
       It is called from GUI thread.
    }
    procedure DoSetFileList;

  protected
    {en
       Retrieves file list from file source, sorts and creates a display file list.
       It may be run from a worker thread so it cannot access GUI directly.
    }
    procedure Execute; override;

  public
    constructor Create(AFileSource: IFileSource;
                       AFileSourcesCount: Integer;
                       const AFileFilter: String;
                       const AFilterOptions: TQuickSearchOptions;
                       const ACurrentPath: String;
                       const ASorting: TFileSortings;
                       AThread: TThread;
                       AFilePropertiesNeeded: TFilePropertiesTypes;
                       ASetFileListMethod: TSetFileListMethod;
                       var ExistingDisplayFiles: TDisplayFiles;
                       var ExistingDisplayFilesHashed: TStringHashList); reintroduce;
    destructor Destroy; override;
    procedure Abort; override;

    {en
       Fills aFiles with files from aFileSourceFiles.
       Filters out any files that shouldn't be shown using aFileFilter.
    }
    class procedure MakeDisplayFileList(allDisplayFiles: TDisplayFiles;
                                        filteredDisplayFiles: TDisplayFiles;
                                        const aFileFilter: String;
                                        const aFilterOptions: TQuickSearchOptions);

    class procedure MakeAllDisplayFileList(aFileSource: IFileSource;
                                           aFileSourceFiles: TFiles;
                                           aDisplayFiles: TDisplayFiles;
                                           const aSortings: TFileSortings);

    class procedure MakeAllDisplayFileList(aFileSource: IFileSource;
                                           aFileSourceFiles: TFiles;
                                           aExistingDisplayFiles: TDisplayFiles;
                                           const aSortings: TFileSortings;
                                           aExistingDisplayFilesHashed: TStringHashList);
  end;

  { TFilePropertiesRetriever }

  TFilePropertiesRetriever = class(TFileViewWorker)
  private
    FWorkingFile: TDisplayFile;
    FWorkingUserData: Pointer;
    FFileList: TFVWorkerFileList;
    FUpdateFileMethod: TUpdateFileMethod;
    FFileSource: IFileSource;
    FFilePropertiesNeeded: TFilePropertiesTypes;

    {en
       Updates file in the file view with new data from FWorkerData.
       It is called from GUI thread.
    }
    procedure DoUpdateFile;

  protected
    procedure Execute; override;

  public
    constructor Create(AFileSource: IFileSource;
                       AThread: TThread;
                       AFilePropertiesNeeded: TFilePropertiesTypes;
                       AUpdateFileMethod: TUpdateFileMethod;
                       var AFileList: TFVWorkerFileList); reintroduce;
    destructor Destroy; override;
  end;

  { TCalculateSpaceWorker }

  TCalculateSpaceWorker = class(TFileViewWorker)
  private
    FWorkingFile: TDisplayFile;
    FWorkingUserData: Pointer;
    FFileList: TFVWorkerFileList;
    FUpdateFileMethod: TUpdateFileMethod;
    FFileSource: IFileSource;
    FOperation: TFileSourceOperation;
    FOperationLock: TCriticalSection;

    {en
       Updates file in the file view with new data.
       It is called from GUI thread.
    }
    procedure DoUpdateFile;

  protected
    procedure Execute; override;

  public
    constructor Create(AFileSource: IFileSource;
                       AThread: TThread;
                       AUpdateFileMethod: TUpdateFileMethod;
                       var AFileList: TFVWorkerFileList); reintroduce;
    destructor Destroy; override;
    procedure Abort; override;
  end;

{$IFDEF timeFileView}
var
  filelistLoaderTime: TDateTime;
{$ENDIF}

implementation

uses
  {$IFDEF timeFileView} uDebug, {$ENDIF}
  LCLProc,
  uFileSourceOperationTypes, uOSUtils, uDCUtils, uExceptions,
  uGlobs, uMasks, uPixMapManager, uFileSourceProperty,
  uFileSourceCalcStatisticsOperation,
  uFileSourceOperationOptions;

{ TFVWorkerFileList }

constructor TFVWorkerFileList.Create;
begin
  FFiles := TFPObjectList.Create(True);
  FUserData := TFPList.Create;
  inherited;
end;

destructor TFVWorkerFileList.Destroy;
begin
  inherited;
  FFiles.Free;
  FUserData.Free;
end;

function TFVWorkerFileList.AddClone(const AFile: TDisplayFile; UserData: Pointer): Integer;
var
  ClonedFile: TDisplayFile;
begin
  ClonedFile := AFile.Clone(True);
  Result := FFiles.Add(ClonedFile);
  FUserData.Add(UserData);
end;

function TFVWorkerFileList.GetCount: Integer;
begin
  Result := FFiles.Count;
end;

function TFVWorkerFileList.GetFile(Index: Integer): TDisplayFile;
begin
  Result := TDisplayFile(FFiles.Items[Index]);
end;

function TFVWorkerFileList.GetData(Index: Integer): Pointer;
begin
  Result := FUserData.Items[Index];
end;

{ TFileViewWorker }

constructor TFileViewWorker.Create(AThread: TThread);
begin
  FAborted := False;
  // After FCanBeDestroyed is set to True the worker may be destroyed.
  FCanBeDestroyed := False;

  // Set Working=True on creation because these workers are usually scheduled
  // to run by a non-main thread, so it might take a while for Execute to be called.
  FWorking := True;
  FWorkType := fvwtNone;

  FOnStarting := nil;
  FOnFinished := nil;
  FThread := AThread;
end;

procedure TFileViewWorker.Abort;
begin
  FAborted := True;
end;

procedure TFileViewWorker.DoFinished;
begin
  FWorking := False;
  try
    FOnFinished(Self);
  except
    on e: Exception do
      HandleException(e);
  end;
end;

procedure TFileViewWorker.DoStarting;
begin
  try
    FOnStarting(Self);
  except
    on e: Exception do
      HandleException(e);
  end;
end;

procedure TFileViewWorker.DoneWorking;
begin
  FWorking := False;
end;

function TFileViewWorker.IsWorking: Boolean;
begin
  Result := FWorking and not FAborted;
end;

procedure TFileViewWorker.Start;
begin
  try
    if not Aborted then
    begin
      if Assigned(FOnStarting) then
        TThread.Synchronize(Thread, @DoStarting);

      if not Aborted then
        Execute; // virtual call

      if Assigned(FOnFinished) then
        TThread.Synchronize(Thread, @DoFinished);
    end;
  finally
    FWorking := False;
    FCanBeDestroyed := True;
  end;
end;

procedure TFileViewWorker.StartParam(Params: Pointer);
begin
  Start;
end;

{ TFileListBuilder }

constructor TFileListBuilder.Create(AFileSource: IFileSource;
                                    AFileSourcesCount: Integer;
                                    const AFileFilter: String;
                                    const AFilterOptions: TQuickSearchOptions;
                                    const ACurrentPath: String;
                                    const ASorting: TFileSortings;
                                    AThread: TThread;
                                    AFilePropertiesNeeded: TFilePropertiesTypes;
                                    ASetFileListMethod: TSetFileListMethod;
                                    var ExistingDisplayFiles: TDisplayFiles;
                                    var ExistingDisplayFilesHashed: TStringHashList);
begin
  inherited Create(AThread);

  FAllDisplayFiles            := ExistingDisplayFiles;
  ExistingDisplayFiles        := nil;
  FExistingDisplayFilesHashed := ExistingDisplayFilesHashed;
  ExistingDisplayFilesHashed  := nil;

  FWorkType              := fvwtCreate;
  FListOperation         := nil;
  FListOperationLock     := TCriticalSection.Create;

  FFileSource           := AFileSource;
  FFileSourcesCount     := AFileSourcesCount;
  FFileFilter           := AFileFilter;
  FFilterOptions        := AFilterOptions;
  FCurrentPath          := ACurrentPath;
  FSortings             := CloneSortings(ASorting);
  FFilePropertiesNeeded := AFilePropertiesNeeded;
  FSetFileListMethod    := ASetFileListMethod;
end;

destructor TFileListBuilder.Destroy;
begin
  inherited Destroy;
  FListOperationLock.Free;
  FExistingDisplayFilesHashed.Free;
  FFilteredDisplayFiles.Free;
  FAllDisplayFiles.Free;
end;

procedure TFileListBuilder.Abort;
begin
  inherited;

  FListOperationLock.Acquire;
  try
    if Assigned(FListOperation) then
      FListOperation.Stop;
  finally
    FListOperationLock.Release;
  end;
end;

procedure TFileListBuilder.Execute;
var
  AFile: TFile;
  i: Integer;
  HaveUpDir: Boolean = False;
  FileSourceFiles: TFiles;
begin
  try
    if Aborted then
      Exit;

    if fsoList in FFileSource.GetOperationsTypes then
    begin
      FListOperationLock.Acquire;
      try
        FListOperation := FFileSource.CreateListOperation(FCurrentPath) as TFileSourceListOperation;
      finally
        FListOperationLock.Release;
      end;

      if Assigned(FListOperation) then
      try
        FListOperation.AssignThread(Thread);
        FListOperation.Execute;
        if FListOperation.Result = fsorFinished then
          FileSourceFiles := FListOperation.ReleaseFiles;
      finally
        FListOperationLock.Acquire;
        try
          FreeAndNil(FListOperation);
        finally
          FListOperationLock.Release;
        end;
      end;
    end;

    {$IFDEF timeFileView}
    DCDebug('Loaded files         : ' + IntToStr(DateTimeToTimeStamp(Now - filelistLoaderTime).Time));
    {$ENDIF}

    if Aborted then
      Exit;

    if Assigned(FileSourceFiles) then
    begin
      // Check if up-dir '..' is present.
      // If it is present it will usually be the first file.
      for i := 0 to FileSourceFiles.Count - 1 do
      begin
        if FileSourceFiles[i].Name = '..' then
        begin
          HaveUpDir := True;
          Break;
        end;
      end;

      if (not HaveUpDir) and
         ((not FFileSource.IsPathAtRoot(FCurrentPath)) or
          // Add '..' to go to higher level file source, if there is more than one.
          (FFileSourcesCount > 1)) then
      begin
        AFile := FFileSource.CreateFileObject(FCurrentPath);
        AFile.Name := '..';
        if fpAttributes in AFile.SupportedProperties then
          AFile.Attributes := faFolder;
        FileSourceFiles.Insert(AFile, 0);
      end;
    end;

    if Aborted then
      Exit;

    // Make display file list from file source file list.
    if Assigned(FAllDisplayFiles) and Assigned(FExistingDisplayFilesHashed) then
    begin
      // Updating existing list.
      MakeAllDisplayFileList(
        FFileSource, FileSourceFiles, FAllDisplayFiles, FSortings, FExistingDisplayFilesHashed);
    end
    else
    begin
      // Creating new list.
      if Assigned(FAllDisplayFiles) then
        FAllDisplayFiles.Clear
      else
        FAllDisplayFiles := TDisplayFiles.Create(True);
      MakeAllDisplayFileList(FFileSource, FileSourceFiles, FAllDisplayFiles, FSortings);
    end;

    // By now the TFile objects have been transfered to FAllDisplayFiles.
    if Assigned(FileSourceFiles) then
      FileSourceFiles.OwnsObjects := False;

    {$IFDEF timeFileView}
    DCDebug('Made sorted disp.lst: ' + IntToStr(DateTimeToTimeStamp(Now - filelistLoaderTime).Time));
    {$ENDIF}

    FFilteredDisplayFiles := TDisplayFiles.Create(False);
    MakeDisplayFileList(FAllDisplayFiles, FFilteredDisplayFiles, FFileFilter, FFilterOptions);

    {$IFDEF timeFileView}
    DCDebug('Made filtered list  : ' + IntToStr(DateTimeToTimeStamp(Now - filelistLoaderTime).Time));
    {$ENDIF}

    if Aborted then
      Exit;

    // Loading file list is complete. Update grid with the new file list.
    TThread.Synchronize(Thread, @DoSetFilelist);

    {$IFDEF timeFileView}
    DCDebug('Grid files updated  : ' + IntToStr(DateTimeToTimeStamp(Now - filelistLoaderTime).Time));
    {$ENDIF}

  finally
    {$IFDEF timeFileView}
    DCDebug('Finished            : ' + IntToStr(DateTimeToTimeStamp(Now - filelistLoaderTime).Time));
    {$ENDIF}

    FreeAndNil(FFilteredDisplayFiles);
    FreeAndNil(FileSourceFiles);
    FreeAndNil(FAllDisplayFiles);
  end;
end;

class procedure TFileListBuilder.MakeDisplayFileList(
  allDisplayFiles: TDisplayFiles;
  filteredDisplayFiles: TDisplayFiles;
  const aFileFilter: String;
  const aFilterOptions: TQuickSearchOptions);
var
  i: Integer;
  invalidFilter: Boolean = False;
  sFileName,
  sFilterNameNoExt,
  sFilterExt,
  localFilter: String;
  filter: Boolean;
begin
  filteredDisplayFiles.Clear;

  if Assigned(allDisplayFiles) then
  begin
    // Prepare filter string based on options.
    if aFileFilter <> EmptyStr then
    begin
      localFilter := aFileFilter;
      if Pos('.', aFileFilter) <> 0 then
        begin
          sFilterNameNoExt := ExtractOnlyFileName(localFilter);
          sFilterExt := ExtractFileExt(localFilter);
          if not (qsmBeginning in aFilterOptions.Match) then
            sFilterNameNoExt := '*' + sFilterNameNoExt;
          if not (qsmEnding in aFilterOptions.Match) then
            sFilterNameNoExt := sFilterNameNoExt + '*';
          localFilter := sFilterNameNoExt + sFilterExt + '*';
        end
      else
        begin
          if not (qsmBeginning in aFilterOptions.Match) then
            localFilter := '*' + localFilter;
          localFilter := localFilter + '*';
        end;
    end;

    for i := 0 to allDisplayFiles.Count - 1 do
    begin
      if gShowSystemFiles = False then
      begin
        if allDisplayFiles[i].FSFile.IsSysFile and (allDisplayFiles[i].FSFile.Name <> '..') then
          Continue;
      end;

      // Ignore list
      if gIgnoreListFileEnabled then
      begin
        if MatchesMaskListEx(allDisplayFiles[i].FSFile, glsIgnoreList) then Continue;
      end;

      // Filter files.
      if (aFileFilter <> EmptyStr) and (invalidFilter = False) then
      begin
        try
          filter := True;

          if (allDisplayFiles[i].FSFile.Name = '..') or
             (allDisplayFiles[i].FSFile.Name = '.') then
            filter := False;

          if (aFilterOptions.Items = qsiFiles) and
             (allDisplayFiles[i].FSFile.IsDirectory or
              allDisplayFiles[i].FSFile.IsLinkToDirectory) then
            filter := False;

          if (aFilterOptions.Items = qsiDirectories) and
             not allDisplayFiles[i].FSFile.IsDirectory and
             not allDisplayFiles[i].FSFile.IsLinkToDirectory then
            filter := False;

          if aFilterOptions.SearchCase = qscSensitive then
            sFileName := allDisplayFiles[i].FSFile.Name
          else
            sFileName := UTF8LowerCase(allDisplayFiles[i].FSFile.Name);

          if MatchesMask(sFileName,
                         localFilter,
                         aFilterOptions.SearchCase = qscSensitive)
          then
            filter := False;

          if filter then
            Continue;

        except
          on EConvertError do
            invalidFilter := True;
        end;
      end;

      filteredDisplayFiles.Add(allDisplayFiles[i]);
    end;
  end;
end;

class procedure TFileListBuilder.MakeAllDisplayFileList(
  aFileSource: IFileSource;
  aFileSourceFiles: TFiles;
  aDisplayFiles: TDisplayFiles;
  const aSortings: TFileSortings);
var
  i: PtrInt;
  AFile: TDisplayFile;
begin
  aDisplayFiles.Clear;

  if Assigned(aFileSourceFiles) then
  begin
    for i := 0 to aFileSourceFiles.Count - 1 do
    begin
      AFile := TDisplayFile.Create(aFileSourceFiles[i]);

      if gShowIcons <> sim_none then
      begin
        AFile.IconID := PixMapManager.GetIconByFile(AFile.FSFile,
                                                    fspDirectAccess in aFileSource.Properties,
                                                    not gLoadIconsSeparately);
      end;

      aDisplayFiles.Add(AFile);
    end;
    TFileSorter.Sort(aDisplayFiles, aSortings);
  end;
end;

class procedure TFileListBuilder.MakeAllDisplayFileList(
  aFileSource: IFileSource;
  aFileSourceFiles: TFiles;
  aExistingDisplayFiles: TDisplayFiles;
  const aSortings: TFileSortings;
  aExistingDisplayFilesHashed: TStringHashList);
var
  i: PtrInt;
  j: Integer;
  AFile: TDisplayFile;
  aNewFiles: TDisplayFiles;
begin
  if Assigned(aFileSourceFiles) then
  begin
    aNewFiles := TDisplayFiles.Create(False);
    try
      for i := 0 to aFileSourceFiles.Count - 1 do
      begin
        j := aExistingDisplayFilesHashed.Find(aFileSourceFiles[i].Name);
        if j >= 0 then
        begin
          // Existing file.
          AFile := TDisplayFile(aExistingDisplayFilesHashed.List[j]^.Data);
          AFile.FSFile := aFileSourceFiles[i];
        end
        else
        begin
          AFile := TDisplayFile.Create(aFileSourceFiles[i]);

          if gShowIcons <> sim_none then
          begin
            AFile.IconID := PixMapManager.GetIconByFile(AFile.FSFile,
                                                        fspDirectAccess in aFileSource.Properties,
                                                        not gLoadIconsSeparately);
          end;

          // New file.
          aNewFiles.Add(AFile);
        end;
      end;

      // Remove files that don't exist anymore.
      for i := aExistingDisplayFiles.Count - 1 downto 0 do
      begin
        if not Assigned(aExistingDisplayFiles[i].FSFile) then
          aExistingDisplayFiles.Delete(i);
      end;

      // Merge new files into existing files list.
      TFileSorter.InsertSort(aNewFiles, aExistingDisplayFiles, aSortings);

    finally
      aNewFiles.Free;
    end;
  end
  else
  begin
    aExistingDisplayFiles.Clear;
  end;
end;

procedure TFileListBuilder.DoSetFileList;
begin
  DoneWorking;
  if not Aborted and Assigned(FSetFileListMethod) then
    FSetFileListMethod(FAllDisplayFiles, FFilteredDisplayFiles);
end;

{ TFilePropertiesRetriever }

constructor TFilePropertiesRetriever.Create(AFileSource: IFileSource;
                                            AThread: TThread;
                                            AFilePropertiesNeeded: TFilePropertiesTypes;
                                            AUpdateFileMethod: TUpdateFileMethod;
                                            var AFileList: TFVWorkerFileList);
begin
  inherited Create(AThread);

  FWorkType             := fvwtUpdate;
  FFileList             := AFileList;
  AFileList             := nil;
  FFileSource           := AFileSource;
  FFilePropertiesNeeded := AFilePropertiesNeeded;
  FUpdateFileMethod     := AUpdateFileMethod;
end;

destructor TFilePropertiesRetriever.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

procedure TFilePropertiesRetriever.Execute;
var
  i: Integer;
begin
  for i := 0 to FFileList.Count - 1 do
  begin
    if Aborted then
      Exit;

    FWorkingFile := FFileList.Files[i];
    FWorkingUserData := FFileList.Data[i];

    try
      if FFileSource.CanRetrieveProperties(FWorkingFile.FSFile, FFilePropertiesNeeded) then
        FFileSource.RetrieveProperties(FWorkingFile.FSFile, FFilePropertiesNeeded);

      if FWorkingFile.IconID = -1 then
        FWorkingFile.IconID := PixMapManager.GetIconByFile(
            FWorkingFile.FSFile,
            fspDirectAccess in FFileSource.Properties,
            True);

      {$IF DEFINED(MSWINDOWS)}
      if gIconOverlays and (FWorkingFile.IconOverlayID < 0) then
        FWorkingFile.IconOverlayID := PixMapManager.GetIconOverlayByFile(
            FWorkingFile.FSFile,
            fspDirectAccess in FFileSource.Properties);
      {$ENDIF}

      if Aborted then
        Exit;

      TThread.Synchronize(Thread, @DoUpdateFile);

    except
      on EFileNotFound do;
    end;
  end;
end;

procedure TFilePropertiesRetriever.DoUpdateFile;
begin
  if not Aborted and Assigned(FUpdateFileMethod) then
    FUpdateFileMethod(FWorkingFile, FWorkingUserData);
end;

{ TCalculateSpaceWorker }

constructor TCalculateSpaceWorker.Create(AFileSource: IFileSource;
                                         AThread: TThread;
                                         AUpdateFileMethod: TUpdateFileMethod;
                                         var AFileList: TFVWorkerFileList);
begin
  inherited Create(AThread);

  FWorkType         := fvwtUpdate;
  FFileList         := AFileList;
  AFileList         := nil;
  FFileSource       := AFileSource;
  FUpdateFileMethod := AUpdateFileMethod;
  FOperation        := nil;
  FOperationLock    := TCriticalSection.Create;
end;

destructor TCalculateSpaceWorker.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
  FOperationLock.Free;
end;

procedure TCalculateSpaceWorker.Abort;
begin
  inherited;

  FOperationLock.Acquire;
  try
    if Assigned(FOperation) then
      FOperation.Stop;
  finally
    FOperationLock.Release;
  end;
end;

procedure TCalculateSpaceWorker.Execute;
var
  CalcStatisticsOperation: TFileSourceCalcStatisticsOperation;
  CalcStatisticsOperationStatistics: TFileSourceCalcStatisticsOperationStatistics;
  TargetFiles: TFiles = nil;
  AFile: TFile;
  i: Integer;
begin
  if fsoCalcStatistics in FFileSource.GetOperationsTypes then
  begin
    for i := 0 to FFileList.Count - 1 do
    begin
      if Aborted then
        Exit;

      FWorkingFile := FFileList.Files[i];
      FWorkingUserData := FFileList.Data[i];

      AFile := FWorkingFile.FSFile;
      if (fpSize in AFile.SupportedProperties) and AFile.IsDirectory then
      begin
        TargetFiles := TFiles.Create(AFile.Path);
        try
          TargetFiles.Add(AFile.Clone);

          FOperationLock.Acquire;
          try
            FOperation := FFileSource.CreateCalcStatisticsOperation(TargetFiles);
          finally
            FOperationLock.Release;
          end;

          CalcStatisticsOperation := FOperation as TFileSourceCalcStatisticsOperation;
          CalcStatisticsOperation.SkipErrors := True;
          CalcStatisticsOperation.SymLinkOption := fsooslDontFollow;

          FOperation.Execute; // blocks until finished

          if FOperation.Result = fsorFinished then
          begin
            CalcStatisticsOperationStatistics := CalcStatisticsOperation.RetrieveStatistics;
            AFile.Size := CalcStatisticsOperationStatistics.Size;

            if Aborted then
              Exit;

            TThread.Synchronize(Thread, @DoUpdateFile);
          end;

        finally
          if Assigned(TargetFiles) then
            FreeAndNil(TargetFiles);
          FOperationLock.Acquire;
          try
            if Assigned(FOperation) then
              FreeAndNil(FOperation);
          finally
            FOperationLock.Release;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCalculateSpaceWorker.DoUpdateFile;
begin
  if not Aborted and Assigned(FUpdateFileMethod) then
    FUpdateFileMethod(FWorkingFile, FWorkingUserData);
end;

end.

