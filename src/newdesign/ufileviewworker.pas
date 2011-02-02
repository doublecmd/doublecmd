unit uFileViewWorker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileView, uDisplayFile, uFile, uFileSource, uFileSorting, uFileProperty;

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
    procedure Abort;
    procedure Start;
    procedure StartParam(Params: Pointer);
    property Aborted: Boolean read FAborted;
    property CanBeDestroyed: Boolean read FCanBeDestroyed;
    property OnFinished: TFinishedWorkMethod read FOnFinished write FOnFinished;
    property OnStarting: TStartingWorkMethod read FOnStarting write FOnStarting;
    property Working: Boolean read IsWorking;
    property WorkType: TFileViewWorkType read FWorkType;
  end;

  PFVWorkerData = ^TFVWorkerData;
  TFVWorkerData = record
    UserData: Pointer;    // Custom data to use by the worker creator in callbacks.
    FSFile: TFile;        // Here is updated file.
    IconID: PtrInt;
  end;

  TSetFileListMethod = procedure (var NewDisplayFiles: TDisplayFiles;
                                  var NewFileSourceFiles: TFiles) of object;
  TUpdateFileMethod = procedure (const WorkerData: TFVWorkerData) of object;

  { TFileListBuilder }

  TFileListBuilder = class(TFileViewWorker)
  private
    FTmpFileSourceFiles: TFiles;
    FTmpDisplayFiles: TDisplayFiles;
    FSetFileListMethod: TSetFileListMethod;

    // Data captured from the file view before start.
    FFileView: TFileView;
    FFileSource: IFileSource;
    FFileSourcesCount: Integer;
    FFileFilter: String;
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
    constructor Create(AFileView: TFileView;
                       AThread: TThread;
                       AFilePropertiesNeeded: TFilePropertiesTypes;
                       ASetFileListMethod: TSetFileListMethod); reintroduce;

    {en
       Fills aFiles with files from aFileSourceFiles.
       Filters out any files that shouldn't be shown using aFileFilter.
    }
    class procedure MakeDisplayFileList(aFileSource: IFileSource;
                                        aFileSourceFiles: TFiles;
                                        aFiles: TDisplayFiles;
                                        aFileFilter: String);
  end;

  { TFilePropertiesRetriever }

  TFilePropertiesRetriever = class(TFileViewWorker)
  private
    FWorkerData: TFVWorkerData;
    FFileList: TFPList;
    FUpdateFileMethod: TUpdateFileMethod;
    FFileSource: IFileSource;
    FFilePropertiesNeeded: TFilePropertiesTypes;

    {en
       Updates file in the file view with new data from FWorkerData.
       It is called from GUI thread.
    }
    procedure DoUpdateFile;
    procedure DestroyFileList;

  protected
    procedure Execute; override;

  public
    constructor Create(AFileSource: IFileSource;
                       AThread: TThread;
                       AFilePropertiesNeeded: TFilePropertiesTypes;
                       AUpdateFileMethod: TUpdateFileMethod;
                       var AFileList: TFPList); reintroduce;
    destructor Destroy; override;
  end;

  { TCalculateSpaceWorker }

  TCalculateSpaceWorker = class(TFileViewWorker)
  private
    FWorkerData: TFVWorkerData;
    FFileList: TFPList;
    FUpdateFileMethod: TUpdateFileMethod;
    FFileSource: IFileSource;

    {en
       Updates file in the file view with new data.
       It is called from GUI thread.
    }
    procedure DoUpdateFile;
    procedure DestroyFileList;

  protected
    procedure Execute; override;

  public
    constructor Create(AFileSource: IFileSource;
                       AThread: TThread;
                       AUpdateFileMethod: TUpdateFileMethod;
                       var AFileList: TFPList); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  LCLProc,
  uFileSourceListOperation, uFileSourceOperationTypes, uOSUtils, uDCUtils,
  uGlobs, uMasks, uPixMapManager, uFileSourceProperty,
  uFileSourceOperation,
  uFileSourceCalcStatisticsOperation,
  uFileSourceOperationOptions;

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
  FOnFinished(Self);
end;

procedure TFileViewWorker.DoStarting;
begin
  FOnStarting(Self);
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
    if Assigned(FOnStarting) then
      TThread.Synchronize(Thread, @DoStarting);

    Execute; // virtual call

    if Assigned(FOnFinished) then
      TThread.Synchronize(Thread, @DoFinished);
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

constructor TFileListBuilder.Create(AFileView: TFileView;
                                    AThread: TThread;
                                    AFilePropertiesNeeded: TFilePropertiesTypes;
                                    ASetFileListMethod: TSetFileListMethod);
begin
  inherited Create(AThread);

  FTmpFileSourceFiles := nil;
  FTmpDisplayFiles := nil;
  FWorkType := fvwtCreate;

  // Copy these parameters while it's still safe to access them from the main thread.
  FFileView             := AFileView;
  FFileSource           := AFileView.FileSource;
  FFileSourcesCount     := AFileView.FileSourcesCount;
  FFileFilter           := AFileView.FileFilter;
  FCurrentPath          := AFileView.CurrentPath;
  FSortings             := CloneSortings(AFileView.Sorting);
  FFilePropertiesNeeded := AFilePropertiesNeeded;
  FSetFileListMethod    := ASetFileListMethod;
end;

procedure TFileListBuilder.Execute;
var
  AFile: TFile;
  ListOperation: TFileSourceListOperation;
  i: Integer;
  HaveUpDir: Boolean = False;
begin
  try
    if Aborted then
      Exit;

    if fsoList in FFileSource.GetOperationsTypes then
    begin
      ListOperation := FFileSource.CreateListOperation(FCurrentPath) as TFileSourceListOperation;
      if Assigned(ListOperation) then
        try
          ListOperation.AssignThread(Thread);
          ListOperation.Execute;
          FTmpFileSourceFiles := ListOperation.ReleaseFiles;
        finally
          FreeAndNil(ListOperation);
        end;
    end;

    {$IFDEF timeFileView}
    DebugLn('Loaded files   : ' + IntToStr(DateTimeToTimeStamp(Now - startTime).Time));
    {$ENDIF}

    if Aborted then
      Exit;

    if Assigned(FTmpFileSourceFiles) then
    begin
      TFileView.Sort(FTmpFileSourceFiles, FSortings);

      // Check if up-dir '..' is present.
      // If it is present it will usually be the first file.
      for i := 0 to FTmpFileSourceFiles.Count - 1 do
      begin
        if FTmpFileSourceFiles[i].Name = '..' then
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
        FTmpFileSourceFiles.Insert(AFile, 0);
      end;
    end;

    if Aborted then
      Exit;

    {$IFDEF timeFileView}
    DebugLn('Sorted files   : ' + IntToStr(DateTimeToTimeStamp(Now - startTime).Time));
    {$ENDIF}

    // Make display file list from file source file list.
    FTmpDisplayFiles := TDisplayFiles.Create;
    MakeDisplayFileList(FFileSource, FTmpFileSourceFiles, FTmpDisplayFiles, FFileFilter);

    {$IFDEF timeFileView}
    DebugLn('Made disp. list: ' + IntToStr(DateTimeToTimeStamp(Now - startTime).Time));
    {$ENDIF}

    if Aborted then
      Exit;

    // Loading file list is complete. Update grid with the new file list.
    TThread.Synchronize(Thread, @DoSetFilelist);

    {$IFDEF timeFileView}
    DebugLn('Grid updated   : ' + IntToStr(DateTimeToTimeStamp(Now - startTime).Time));
    {$ENDIF}

  finally
    {$IFDEF timeFileView}
    DebugLn('Finished       : ' + IntToStr(DateTimeToTimeStamp(Now - startTime).Time));
    {$ENDIF}

    FreeThenNil(FTmpDisplayFiles);
    FreeThenNil(FTmpFileSourceFiles);
  end;
end;

class procedure TFileListBuilder.MakeDisplayFileList(
                  aFileSource: IFileSource;
                  aFileSourceFiles: TFiles;
                  aFiles: TDisplayFiles;
                  aFileFilter: String);
var
  AFile: TDisplayFile;
  i: Integer;
  invalidFilter: Boolean = False;
  sFilterNameNoExt,
  sFilterExt,
  localFilter: String;
begin
  aFiles.Clear;

  if Assigned(aFileSourceFiles) then
  begin
    // Prepare filter string based on options.
    if aFileFilter <> EmptyStr then
    begin
      localFilter := aFileFilter;
      if Pos('.', aFileFilter) <> 0 then
        begin
          sFilterNameNoExt := ExtractOnlyFileName(localFilter);
          sFilterExt := ExtractFileExt(localFilter);
          if not gQuickSearchMatchBeginning then
            sFilterNameNoExt := '*' + sFilterNameNoExt;
          if not gQuickSearchMatchEnding then
            sFilterNameNoExt := sFilterNameNoExt + '*';
          localFilter := sFilterNameNoExt + sFilterExt + '*';
        end
      else
        begin
          if not gQuickSearchMatchBeginning then
            localFilter := '*' + localFilter;
          localFilter := localFilter + '*';
        end;
    end;

    for i := 0 to aFileSourceFiles.Count - 1 do
    begin
      if gShowSystemFiles = False then
      begin
        if aFileSourceFiles[i].IsSysFile and (aFileSourceFiles[i].Name <> '..') then
          Continue;
      end;

      // Ignore list
      if gIgnoreListFileEnabled then
      begin
        if MatchesMaskListEx(aFileSourceFiles[i], glsIgnoreList) then Continue;
      end;

      // Filter files.
      if (aFileFilter <> EmptyStr) and (invalidFilter = False) then
      begin
        try
          if (aFileSourceFiles[i].Name <> '..') and
             (aFileSourceFiles[i].Name <> '.') and

             // Don't filter directories.
             not (aFileSourceFiles[i].IsDirectory or
                  aFileSourceFiles[i].IsLinkToDirectory) and

             not MatchesMask(UTF8LowerCase(aFileSourceFiles[i].Name),
                             UTF8LowerCase(localFilter))
          then
            Continue;

        except
          on EConvertError do
            invalidFilter := True;
        end;
      end;

      AFile := TDisplayFile.Create(aFileSourceFiles[i]);

      if gShowIcons <> sim_none then
      begin
        AFile.IconID := PixMapManager.GetIconByFile(AFile.FSFile,
                                                    fspDirectAccess in aFileSource.Properties,
                                                    not gLoadIconsSeparately);
      end;

      aFiles.Add(AFile);
    end;
  end;
end;

procedure TFileListBuilder.DoSetFileList;
begin
  DoneWorking;
  if not Aborted and Assigned(FSetFileListMethod) then
    FSetFileListMethod(FTmpDisplayFiles, FTmpFileSourceFiles);
end;

{ TFilePropertiesRetriever }

constructor TFilePropertiesRetriever.Create(AFileSource: IFileSource;
                                            AThread: TThread;
                                            AFilePropertiesNeeded: TFilePropertiesTypes;
                                            AUpdateFileMethod: TUpdateFileMethod;
                                            var AFileList: TFPList);
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
  DestroyFileList;
  inherited Destroy;
end;

procedure TFilePropertiesRetriever.Execute;
var
  i: Integer;
begin
  try
    for i := 0 to FFileList.Count - 1 do
    begin
      if Aborted then
        Exit;

      FWorkerData := PFVWorkerData(FFileList[i])^;
      if FFileSource.CanRetrieveProperties(FWorkerData.FSFile, FFilePropertiesNeeded) then
        FFileSource.RetrieveProperties(FWorkerData.FSFile, FFilePropertiesNeeded);

      if FWorkerData.IconID = -1 then
        FWorkerData.IconID := PixMapManager.GetIconByFile(
            FWorkerData.FSFile,
            fspDirectAccess in FFileSource.Properties,
            True);

      TThread.Synchronize(Thread, @DoUpdateFile);
    end;

  finally
    DestroyFileList;
  end;
end;

procedure TFilePropertiesRetriever.DoUpdateFile;
begin
  if not Aborted and Assigned(FUpdateFileMethod) then
    FUpdateFileMethod(FWorkerData);
end;

procedure TFilePropertiesRetriever.DestroyFileList;
var
  i: Integer;
begin
  if Assigned(FFileList) then
  begin
    for i := 0 to FFileList.Count - 1 do
    begin
      PFVWorkerData(FFileList[i])^.FSFile.Free;
      Dispose(PFVWorkerData(FFileList[i]));
    end;
    FreeAndNil(FFileList);
  end;
end;

{ TCalculateSpaceWorker }

constructor TCalculateSpaceWorker.Create(AFileSource: IFileSource;
                                         AThread: TThread;
                                         AUpdateFileMethod: TUpdateFileMethod;
                                         var AFileList: TFPList);
begin
  inherited Create(AThread);

  FWorkType         := fvwtUpdate;
  FFileList         := AFileList;
  AFileList         := nil;
  FFileSource       := AFileSource;
  FUpdateFileMethod := AUpdateFileMethod;
end;

destructor TCalculateSpaceWorker.Destroy;
begin
  DestroyFileList;
  inherited Destroy;
end;

procedure TCalculateSpaceWorker.Execute;
var
  Operation: TFileSourceOperation = nil;
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

      FWorkerData := PFVWorkerData(FFileList[i])^;
      AFile := FWorkerData.FSFile;
      if (fpSize in AFile.SupportedProperties) and AFile.IsDirectory then
      begin
        TargetFiles := TFiles.Create(AFile.Path);
        try
          TargetFiles.Add(AFile.Clone);

          Operation := FFileSource.CreateCalcStatisticsOperation(TargetFiles);
          CalcStatisticsOperation := Operation as TFileSourceCalcStatisticsOperation;
          CalcStatisticsOperation.SkipErrors := True;
          CalcStatisticsOperation.SymLinkOption := fsooslDontFollow;

          Operation.Execute; // blocks until finished

          CalcStatisticsOperationStatistics := CalcStatisticsOperation.RetrieveStatistics;

          AFile.Size := CalcStatisticsOperationStatistics.Size;

          TThread.Synchronize(Thread, @DoUpdateFile);

        finally
          if Assigned(TargetFiles) then
            FreeAndNil(TargetFiles);
          if Assigned(Operation) then
            FreeAndNil(Operation);
        end;
      end;
    end;
  end;
end;

procedure TCalculateSpaceWorker.DoUpdateFile;
begin
  if not Aborted and Assigned(FUpdateFileMethod) then
    FUpdateFileMethod(FWorkerData);
end;

procedure TCalculateSpaceWorker.DestroyFileList;
var
  i: Integer;
begin
  if Assigned(FFileList) then
  begin
    for i := 0 to FFileList.Count - 1 do
    begin
      PFVWorkerData(FFileList[i])^.FSFile.Free;
      Dispose(PFVWorkerData(FFileList[i]));
    end;
    FreeAndNil(FFileList);
  end;
end;

end.

