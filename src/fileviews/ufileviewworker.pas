unit uFileViewWorker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, syncobjs, DCStringHashListUtf8,
  uDisplayFile, uFile, uFileSource, uFileSorting, uFileProperty,
  DCBasicTypes,
  uFileSourceOperation,
  uFileSourceListOperation,
  fQuickSearch,uMasks;

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
    {en After FCanBeDestroyed is set to True the worker may be destroyed.}
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
    property UserData: TFPList read FUserData;
  end;

  TSetFileListMethod = procedure (var NewAllDisplayFiles: TDisplayFiles;
                                  var NewFilteredDisplayFiles: TDisplayFiles) of object;
  TUpdateFileMethod = procedure (const UpdatedFile: TDisplayFile;
                                 const UserData: Pointer) of object;
  TAbortFileMethod = procedure (AStart: Integer; AList: TFPList) of object;

  { TFileListBuilder }

  TFileListBuilder = class(TFileViewWorker)
  private
    FFilteredDisplayFiles: TDisplayFiles;
    FAllDisplayFiles: TDisplayFiles;
    FExistingDisplayFilesHashed: TStringHashListUtf8;
    FSetFileListMethod: TSetFileListMethod;
    FListOperation: TFileSourceListOperation;
    FListOperationLock: TCriticalSection;

    // Data captured from the file view before start.
    FFileSource: IFileSource;
    FFileSourceIndex: Integer;
    FFileFilter: String;
    FFilterOptions: TQuickSearchOptions;
    FCurrentPath: String;
    FFlatView: Boolean;
    FSortings: TFileSortings;
    FVariantProperties: TDynamicStringArray;
    FFilePropertiesNeeded: TFilePropertiesTypes;

    {en
       Calls the update method with the new built lists.
       It is called from GUI thread.
    }
    procedure DoSetFileList;

    class function InternalMatchesFilter(aFile: TFile;
                                         const aFileFilter: String;
                                         const aFilterOptions: TQuickSearchOptions): Boolean;overload;


    class function InternalMatchesFilter(aFile: TFile;
      const aMasks: TMaskList; const aFilterOptions: TQuickSearchOptions): Boolean;overload;


  protected
    {en
       Retrieves file list from file source, sorts and creates a display file list.
       It may be run from a worker thread so it cannot access GUI directly.
    }
    procedure Execute; override;

  public
    constructor Create(AFileSource: IFileSource;
                       AFileSourceIndex: Integer;
                       const AFileFilter: String;
                       const AFilterOptions: TQuickSearchOptions;
                       const ACurrentPath: String;
                       const ASorting: TFileSortings;
                       AFlatView: Boolean;
                       AThread: TThread;
                       AFilePropertiesNeeded: TFilePropertiesTypes;
                       AVariantProperties: TDynamicStringArray;
                       ASetFileListMethod: TSetFileListMethod;
                       var ExistingDisplayFiles: TDisplayFiles;
                       var ExistingDisplayFilesHashed: TStringHashListUtf8); reintroduce;
    destructor Destroy; override;
    procedure Abort; override;

    {en
       Prepare filter string based on options.
    }
    class function PrepareFilter(const aFileFilter: String;
                                 const aFilterOptions: TQuickSearchOptions): String;


    {en
       Fills aFiles with files from aFileSourceFiles.
       Filters out any files that shouldn't be shown using aFileFilter.
    }
    class procedure MakeDisplayFileList(allDisplayFiles: TDisplayFiles;
                                        filteredDisplayFiles: TDisplayFiles;
                                        aFileFilter: String;
                                        const aFilterOptions: TQuickSearchOptions);

    class procedure MakeAllDisplayFileList(aFileSource: IFileSource;
                                           aFileSourceFiles: TFiles;
                                           aDisplayFiles: TDisplayFiles;
                                           const aSortings: TFileSortings);

    class procedure MakeAllDisplayFileList(aFileSource: IFileSource;
                                           aFileSourceFiles: TFiles;
                                           aExistingDisplayFiles: TDisplayFiles;
                                           const aSortings: TFileSortings;
                                           aExistingDisplayFilesHashed: TStringHashListUtf8);

    class function MatchesFilter(aFile: TFile;
                                 aFileFilter: String;
                                 const aFilterOptions: TQuickSearchOptions): Boolean;
  end;

  { TFilePropertiesRetriever }

  TFilePropertiesRetriever = class(TFileViewWorker)
  private
    FIndex: Integer;
    FWorkingFile: TDisplayFile;
    FWorkingUserData: Pointer;
    FFileList: TFVWorkerFileList;
    FUpdateFileMethod: TUpdateFileMethod;
    FAbortFileMethod: TAbortFileMethod;
    FFileSource: IFileSource;
    FVariantProperties: TDynamicStringArray;
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
                       AVariantProperties: TDynamicStringArray;
                       AUpdateFileMethod: TUpdateFileMethod;
                       ABreakFileMethod: TAbortFileMethod;
                       var AFileList: TFVWorkerFileList); reintroduce;
    destructor Destroy; override;
    procedure Abort; override;
  end;

  { TCalculateSpaceWorker }

  TCalculateSpaceWorker = class(TFileViewWorker)
  private
    FWorkingIndex: Integer;
    FWorkingFile: TDisplayFile;
    FWorkingUserData: Pointer;
    FFileList: TFVWorkerFileList;
    FCompletedCalculations: Integer;
    FUpdateFileMethod: TUpdateFileMethod;
    FFileSource: IFileSource;
    FOperation: TFileSourceOperation;
    FOperationLock: TCriticalSection;

    {en
       Updates file in the file view with new data.
       It is called from GUI thread.
    }
    procedure DoUpdateFile;

    procedure DoUpdateFolders;

  protected
    procedure Execute; override;

  public
    constructor Create(AFileSource: IFileSource;
                       AThread: TThread;
                       AUpdateFileMethod: TUpdateFileMethod;
                       var AFileList: TFVWorkerFileList); reintroduce;
    destructor Destroy; override;
    procedure Abort; override;
    property CompletedCalculations: Integer read FCompletedCalculations;
  end;

{$IFDEF timeFileView}
var
  filelistTime,
  filelistPrevTime,
  filelistLoaderTime: QWord;
{$ENDIF}

implementation

uses
  {$IFDEF timeFileView} uDebug, {$ENDIF}
  LCLProc, Graphics, DCFileAttributes,
  uFileSourceOperationTypes, uOSUtils, DCStrUtils, uDCUtils, uExceptions,
  uGlobs, uPixMapManager, uFileSourceProperty,
  uFileSourceCalcStatisticsOperation,
  uFileSourceOperationOptions;

{$IFDEF timeFileView}
procedure filelistPrintTime(const AMessage: String); inline;
begin
  filelistTime:= GetTickCount64;
  DCDebug(AMessage + IntToStr(filelistTime - filelistLoaderTime) +
          ', offset ' + IntToStr(filelistTime - filelistPrevTime));
  filelistPrevTime:= filelistTime;
end;
{$ENDIF}

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
  // Set Working=True on creation because these workers are usually scheduled
  // to run by a non-main thread, so it might take a while for Execute to be called.
  FWorking := True;
  FWorkType := fvwtNone;

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
  AFileSourceIndex: Integer; const AFileFilter: String;
  const AFilterOptions: TQuickSearchOptions; const ACurrentPath: String;
  const ASorting: TFileSortings; AFlatView: Boolean; AThread: TThread;
  AFilePropertiesNeeded: TFilePropertiesTypes;
  AVariantProperties: TDynamicStringArray;
  ASetFileListMethod: TSetFileListMethod;
  var ExistingDisplayFiles: TDisplayFiles;
  var ExistingDisplayFilesHashed: TStringHashListUtf8);
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
  FFileSourceIndex      := AFileSourceIndex;
  FFlatView             := AFlatView;
  FFileFilter           := AFileFilter;
  FFilterOptions        := AFilterOptions;
  FCurrentPath          := ACurrentPath;
  FSortings             := CloneSortings(ASorting);
  FVariantProperties    := AVariantProperties;
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
  I: Integer;
  HaveUpDir: Boolean = False;
  FileSourceFiles: TFiles = nil;
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
        FListOperation.FlatView := FFlatView;
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
    filelistPrintTime('Loaded files        : ');
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
          ((FFileSourceIndex > 0) and not (fspNoneParent in FFileSource.Properties))) then
      begin
        AFile := FFileSource.CreateFileObject(FCurrentPath);
        AFile.Name := '..';
        if fpAttributes in AFile.SupportedProperties then
        begin
          if AFile.AttributesProperty is TNtfsFileAttributesProperty then
            AFile.Attributes := FILE_ATTRIBUTE_DIRECTORY
          else if AFile.AttributesProperty is TUnixFileAttributesProperty then
            AFile.Attributes := S_IFDIR
          else
            AFile.Attributes := faFolder;
        end;
        FileSourceFiles.Insert(AFile, 0);
      end;
    end;

    if Aborted then
      Exit;

    // Retrieve RetrievableFileProperties which used in sorting
    if FFilePropertiesNeeded <> [] then
    begin
      for I:= 0 to FileSourceFiles.Count - 1 do
        FFileSource.RetrieveProperties(FileSourceFiles[I], FFilePropertiesNeeded, FVariantProperties);
    end;

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
    filelistPrintTime('Made sorted disp.lst: ');
    {$ENDIF}

    FFilteredDisplayFiles := TDisplayFiles.Create(False);
    MakeDisplayFileList(FAllDisplayFiles, FFilteredDisplayFiles, FFileFilter, FFilterOptions);

    {$IFDEF timeFileView}
    filelistPrintTime('Made filtered list  : ');
    {$ENDIF}

    if Aborted then
      Exit;

    // Loading file list is complete. Update grid with the new file list.
    TThread.Synchronize(Thread, @DoSetFilelist);

    {$IFDEF timeFileView}
    filelistPrintTime('Grid files updated  : ');
    {$ENDIF}

  finally
    {$IFDEF timeFileView}
    filelistPrintTime('Finished            : ');
    {$ENDIF}

    FreeAndNil(FFilteredDisplayFiles);
    FreeAndNil(FileSourceFiles);
    FreeAndNil(FAllDisplayFiles);
  end;
end;

class function TFileListBuilder.InternalMatchesFilter(aFile: TFile;
                                                      const aFileFilter: String;
                                                      const aFilterOptions: TQuickSearchOptions): Boolean;
const
  ACaseSensitive: array[Boolean] of TMaskOptions = ([], [moCaseSensitive]);
begin
  if (gShowSystemFiles = False) and AFile.IsSysFile and (AFile.Name <> '..') then
    Result := True

  // Ignore list
  else if gIgnoreListFileEnabled and MatchesMaskListEx(AFile, glsIgnoreList) then
    Result := True

  // Filter files.
  else if aFileFilter <> EmptyStr then
  begin
    Result := True;

    if (AFile.Name = '..') or (AFile.Name = '.') then
      Result := False
    else
    if (aFilterOptions.Items = qsiFiles) and
       (AFile.IsDirectory or AFile.IsLinkToDirectory) then
      Result := False
    else
    if (aFilterOptions.Items = qsiDirectories) and
       not AFile.IsDirectory and not AFile.IsLinkToDirectory then
      Result := False
    else
    begin
      if MatchesMask(AFile.Name,
                     aFileFilter,
                     ACaseSensitive[aFilterOptions.SearchCase = qscSensitive])
      then
        Result := False;
    end;
  end
  else
    Result := False;
end;

class function TFileListBuilder.InternalMatchesFilter(aFile: TFile;
  const aMasks: TMaskList; const aFilterOptions: TQuickSearchOptions): Boolean;
begin
  if (gShowSystemFiles = False) and AFile.IsSysFile and (AFile.Name <> '..') then
    Result := True

  // Ignore list
  else if gIgnoreListFileEnabled and MatchesMaskListEx(AFile, glsIgnoreList) then
    Result := True

  // Filter files.
  else if aMasks.Count <> 0 then
  begin
    Result := True;

    if (AFile.Name = '..') or (AFile.Name = '.') then
      Result := False
    else
    if (aFilterOptions.Items = qsiFiles) and
       (AFile.IsDirectory or AFile.IsLinkToDirectory) then
      Result := False
    else
    if (aFilterOptions.Items = qsiDirectories) and
       not AFile.IsDirectory and not AFile.IsLinkToDirectory then
      Result := False
    else
    begin
      // Match the file name and Pinyin letter
      if aMasks.Matches(AFile.Name) then
         Result := False;
    end;
  end
  else
    Result := False;
end;

class function TFileListBuilder.PrepareFilter(const aFileFilter: String;
                                              const aFilterOptions: TQuickSearchOptions): String;
var
  Index: Integer;
  sFileExt: String;
  sFilterNameNoExt: String;
begin
  Result := aFileFilter;
  if Result <> EmptyStr then
  begin
    Index:= Pos('.', Result);
    if (Index > 0) and ((Index > 1) or FirstDotAtFileNameStartIsExtension) then
      begin
        sFileExt := ExtractFileExt(Result);
        sFilterNameNoExt := ExtractOnlyFileName(Result);
        if not (qsmBeginning in aFilterOptions.Match) then
          sFilterNameNoExt := '*' + sFilterNameNoExt;
        if not (qsmEnding in aFilterOptions.Match) then
          sFilterNameNoExt := sFilterNameNoExt + '*';
        Result := sFilterNameNoExt + sFileExt + '*';
      end
    else
      begin
        if not (qsmBeginning in aFilterOptions.Match) then
          Result := '*' + Result;
        Result := Result + '*';
      end;
  end;
end;


class procedure TFileListBuilder.MakeDisplayFileList(
  allDisplayFiles: TDisplayFiles;
  filteredDisplayFiles: TDisplayFiles;
  aFileFilter: String;
  const aFilterOptions: TQuickSearchOptions);
var
  S: String;
  I: Integer;
  AFile: TFile;
  AFilter: Boolean;
  Masks: TMaskList;
  AOptions: TMaskOptions = [moPinyin];
begin
  filteredDisplayFiles.Clear;
  if qscSensitive in [aFilterOptions.SearchCase] then
    AOptions += [moCaseSensitive];

  if Assigned(allDisplayFiles) then
  try
    Masks:= TMaskList.Create(aFileFilter, ';,', AOptions);

    for I := 0 to Masks.Count - 1 do
    begin
      S:= Masks.Items[I].Template;
      S:= PrepareFilter(S, aFilterOptions);
      Masks.Items[I].Template:= S;
    end;

    for I := 0 to allDisplayFiles.Count - 1 do
    begin
      AFile := allDisplayFiles[I].FSFile;

      try
        AFilter := InternalMatchesFilter(AFile, Masks, aFilterOptions);
      except
        on EConvertError do
          aFileFilter := EmptyStr;
      end;

      if not AFilter then
        filteredDisplayFiles.Add(allDisplayFiles[I]);
    end;
  finally
    Masks.Free;
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
  HaveIcons: Boolean;
  DirectAccess: Boolean;
begin
  aDisplayFiles.Clear;

  if Assigned(aFileSourceFiles) then
  begin
    HaveIcons := gShowIcons <> sim_none;
    DirectAccess := fspDirectAccess in aFileSource.Properties;
    if HaveIcons and gIconsExclude and DirectAccess then
    begin
      DirectAccess := not IsInPathList(gIconsExcludeDirs, aFileSourceFiles.Path);
    end;
    for i := 0 to aFileSourceFiles.Count - 1 do
    begin
      AFile := TDisplayFile.Create(aFileSourceFiles[i]);

      AFile.TextColor:= gColorExt.GetColorBy(AFile.FSFile);

      if HaveIcons then
      begin
        AFile.IconID := PixMapManager.GetIconByFile(AFile.FSFile,
                                                    DirectAccess,
                                                    not gLoadIconsSeparately,
                                                    gShowIcons,
                                                    not gIconOverlays);
      end;

      aDisplayFiles.Add(AFile);
    end;
    TDisplayFileSorter.Sort(aDisplayFiles, aSortings);
  end;
end;

class procedure TFileListBuilder.MakeAllDisplayFileList(
  aFileSource: IFileSource;
  aFileSourceFiles: TFiles;
  aExistingDisplayFiles: TDisplayFiles;
  const aSortings: TFileSortings;
  aExistingDisplayFilesHashed: TStringHashListUtf8);
var
  i: PtrInt;
  j: Integer;
  AFile: TDisplayFile;
  aNewFiles: TDisplayFiles;
  HaveIcons: Boolean;
  DirectAccess: Boolean;
begin
  if Assigned(aFileSourceFiles) then
  begin
    HaveIcons := gShowIcons <> sim_none;
    DirectAccess := fspDirectAccess in aFileSource.Properties;
    if HaveIcons and gIconsExclude and DirectAccess then
    begin
      DirectAccess := not IsInPathList(gIconsExcludeDirs, aFileSourceFiles.Path);
    end;
    aNewFiles := TDisplayFiles.Create(False);
    try
      for i := 0 to aFileSourceFiles.Count - 1 do
      begin
        j := aExistingDisplayFilesHashed.Find(aFileSourceFiles[i].FullPath);
        if j >= 0 then
        begin
          // Existing file.
          AFile := TDisplayFile(aExistingDisplayFilesHashed.List[j]^.Data);
          AFile.FSFile := aFileSourceFiles[i];
        end
        else
        begin
          AFile := TDisplayFile.Create(aFileSourceFiles[i]);

          AFile.TextColor:= gColorExt.GetColorBy(AFile.FSFile);

          if HaveIcons then
          begin
            AFile.IconID := PixMapManager.GetIconByFile(AFile.FSFile,
                                                        DirectAccess,
                                                        not gLoadIconsSeparately,
                                                        gShowIcons,
                                                        not gIconOverlays);
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
      TDisplayFileSorter.InsertSort(aNewFiles, aExistingDisplayFiles, aSortings);

    finally
      aNewFiles.Free;
    end;
  end
  else
  begin
    aExistingDisplayFiles.Clear;
  end;
end;

class function TFileListBuilder.MatchesFilter(aFile: TFile;
                                              aFileFilter: String;
                                              const aFilterOptions: TQuickSearchOptions): Boolean;
begin
  aFileFilter := PrepareFilter(aFileFilter, aFilterOptions);
  try
    Result := InternalMatchesFilter(AFile, aFileFilter, aFilterOptions);
  except
    on EConvertError do
      Result := False;
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
  AThread: TThread; AFilePropertiesNeeded: TFilePropertiesTypes;
  AVariantProperties: TDynamicStringArray;
  AUpdateFileMethod: TUpdateFileMethod; ABreakFileMethod: TAbortFileMethod;
  var AFileList: TFVWorkerFileList);
begin
  inherited Create(AThread);

  FWorkType             := fvwtUpdate;
  FFileList             := AFileList;
  AFileList             := nil;
  FFileSource           := AFileSource;
  FVariantProperties    := AVariantProperties;
  FFilePropertiesNeeded := AFilePropertiesNeeded;
  FUpdateFileMethod     := AUpdateFileMethod;
  FAbortFileMethod      := ABreakFileMethod;
end;

destructor TFilePropertiesRetriever.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

procedure TFilePropertiesRetriever.Abort;
begin
  inherited Abort;

  if Assigned(FAbortFileMethod) then
  begin
    FAbortFileMethod(FIndex, FFileList.FUserData);
  end;
end;

procedure TFilePropertiesRetriever.Execute;
var
  HaveIcons: Boolean;
  DirectAccess: Boolean;
begin
  HaveIcons := gShowIcons <> sim_none;
  DirectAccess := fspDirectAccess in FFileSource.Properties;
  if HaveIcons and gIconsExclude and DirectAccess then
  begin
    DirectAccess := not IsInPathList(gIconsExcludeDirs, FFileList.Files[0].FSFile.Path);
  end;
  while (FIndex < FFileList.Count) and (Aborted = False) do
  begin

    try
      FWorkingFile := FFileList.Files[FIndex];
      FWorkingUserData := FFileList.Data[FIndex];

      if FFileSource.CanRetrieveProperties(FWorkingFile.FSFile, FFilePropertiesNeeded) then
        FFileSource.RetrieveProperties(FWorkingFile.FSFile, FFilePropertiesNeeded, FVariantProperties);

      if FWorkingFile.TextColor = clNone then
        FWorkingFile.TextColor:= gColorExt.GetColorBy(FWorkingFile.FSFile);

      if HaveIcons then
      begin
        if FWorkingFile.IconID < 0 then
          FWorkingFile.IconID := PixMapManager.GetIconByFile(
              FWorkingFile.FSFile,
              DirectAccess,
              True,
              gShowIcons,
              not gIconOverlays);

        {$IF DEFINED(MSWINDOWS) OR DEFINED(RabbitVCS)}
        if gIconOverlays and (FWorkingFile.IconOverlayID < 0) then
          FWorkingFile.IconOverlayID := PixMapManager.GetIconOverlayByFile(
              FWorkingFile.FSFile,
              DirectAccess);
        {$ENDIF}
      end;

      TThread.Synchronize(Thread, @DoUpdateFile);

    except
      on EListError do;
      on EFileNotFound do;
    end;
    Inc(FIndex);
  end;
end;

procedure TFilePropertiesRetriever.DoUpdateFile;
begin
  if Assigned(FUpdateFileMethod) then
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
begin
  if fsoCalcStatistics in FFileSource.GetOperationsTypes then
  begin
    FWorkingIndex:= 0;
    TThread.Synchronize(Thread, @DoUpdateFolders);
    try
      while FWorkingIndex < FFileList.Count do
      begin
        if Aborted then Break;

        FWorkingFile := FFileList.Files[FWorkingIndex];
        FWorkingUserData := FFileList.Data[FWorkingIndex];

        AFile := FWorkingFile.FSFile;
        if (fpSize in AFile.SupportedProperties) and (AFile.IsDirectory and not AFile.IsLinkToDirectory) then
        begin
          TargetFiles := TFiles.Create(AFile.Path);
          try
            TargetFiles.Add(AFile.Clone);

            AFile.Size:= FOLDER_SIZE_CALC;
            TThread.Synchronize(Thread, @DoUpdateFile);

            FOperationLock.Acquire;
            try
              FOperation := FFileSource.CreateCalcStatisticsOperation(TargetFiles);
            finally
              FOperationLock.Release;
            end;

            CalcStatisticsOperation := FOperation as TFileSourceCalcStatisticsOperation;
            CalcStatisticsOperation.SkipErrors := True;
            CalcStatisticsOperation.SymLinkOption := fsooslDontFollow;

            if fspListOnMainThread in FFileSource.Properties then
              TThread.Synchronize(Thread, @FOperation.Execute)
            else begin
              FOperation.Execute; // blocks until finished
            end;

            if Aborted then Break;

            if FOperation.Result = fsorFinished then
            begin
              CalcStatisticsOperationStatistics := CalcStatisticsOperation.RetrieveStatistics;
              AFile.Size := CalcStatisticsOperationStatistics.Size;
              if AFile.Size = 0 then AFile.Size:= FOLDER_SIZE_ZERO;
              Inc(FCompletedCalculations);

              TThread.Synchronize(Thread, @DoUpdateFile);
            end;

          finally
            FreeAndNil(TargetFiles);
            FOperationLock.Acquire;
            try
              FreeAndNil(FOperation);
            finally
              FOperationLock.Release;
            end;
          end;
        end;
        Inc(FWorkingIndex);
      end;
    finally
      if Aborted then
      begin
        TThread.Synchronize(Thread, @DoUpdateFolders);
      end;
    end;
  end;
end;

procedure TCalculateSpaceWorker.DoUpdateFile;
begin
  if Assigned(FUpdateFileMethod) then
    FUpdateFileMethod(FWorkingFile, FWorkingUserData);
end;

procedure TCalculateSpaceWorker.DoUpdateFolders;
var
  ASize: Int64;
  Index: Integer;
begin
  if Assigned(FUpdateFileMethod) then
  begin
    if Aborted then
      ASize:= FOLDER_SIZE_UNKN
    else begin
      ASize:= FOLDER_SIZE_WAIT;
    end;
    Index:= FWorkingIndex;

    while Index < FFileList.Count do
    begin
      FWorkingFile:= FFileList.Files[Index];
      FWorkingUserData := FFileList.Data[Index];

      if FWorkingFile.FSFile.IsDirectory and not FWorkingFile.FSFile.IsLinkToDirectory then
      begin
        FWorkingFile.FSFile.Size:= ASize;
        FUpdateFileMethod(FWorkingFile, FWorkingUserData);
      end;

      Inc(Index);
    end;
  end;
end;

end.

