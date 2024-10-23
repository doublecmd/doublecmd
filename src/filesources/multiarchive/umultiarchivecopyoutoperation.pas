unit uMultiArchiveCopyOutOperation;

{$mode objfpc}{$H+}

interface

uses
  LazFileUtils,LazUtf8,Classes, SysUtils, DCStringHashListUtf8, uLog, uGlobs, un_process,
  uFileSourceOperation,
  uFileSourceCopyOperation,
  uFileSourceOperationUI,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI,
  uFileSource,
  uFile,
  uArchiveCopyOperation,
  uMultiArchiveFileSource;

type

  { TMultiArchiveCopyOutOperation }

  TMultiArchiveCopyOutOperation = class(TArchiveCopyOutOperation)

  private
    FMultiArchiveFileSource: IMultiArchiveFileSource;
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    FFullFilesTreeToExtract: TFiles;  // source files including all files/dirs in subdirectories

    // Options
    FPassword: String;
    FExtractWithoutPath: Boolean;

    {en
      Creates neccessary paths before extracting files from archive.

      @param(Files
             List of files/directories to extract (relative to archive root).)
      @param(sDestPath
             Destination path where the files will be extracted.)
      @param(CurrentArchiveDir
             Path inside the archive from where the files will be extracted.)
      @param(CreatedPaths
             This list will be filled with absolute paths to directories
             that were created, together with their attributes.)}
    procedure CreateDirs(const theFiles: TFiles; sDestPath: String;
                                      CurrentArchiveDir: String;
                                      var CreatedPaths: TStringHashListUtf8);

    {en
      Sets attributes for directories.
      @param(Paths
             The list of absolute paths, which attributes are to be set.
             Each list item's data field must be a pointer to TMultiArchiveFile,
             from where the attributes are retrieved.}
    function SetDirsAttributes(const Paths: TStringHashListUtf8): Boolean;

    function DoFileExists(aFile: TFile; const AbsoluteTargetFileName: String): TFileSourceOperationOptionFileExists;

    procedure ShowError(sMessage: String; logOptions: TLogOptions = []);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
    procedure CheckForErrors(const SourceName, TargetName: String; ExitStatus: LongInt);

  protected
    FCurrentFile: TFile;
    FCurrentTargetFilePath: String;
    procedure QuestionActionHandler(Action: TFileSourceOperationUIAction);

  protected
    FExProcess: TExProcess;
    FTempFile: String;
    FErrorLevel: LongInt;
    procedure OnReadLn(str: string);
    procedure OnQueryString(str: string);
    procedure UpdateProgress(SourceName, TargetName: String; IncSize: Int64);
    procedure FileSourceOperationStateChangedNotify(Operation: TFileSourceOperation;
                                                    AState: TFileSourceOperationState);

  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

    class function GetOptionsUIClass: TFileSourceOperationOptionsUIClass; override;

    property Password: String read FPassword write FPassword;
    property ExtractWithoutPath: Boolean read FExtractWithoutPath write FExtractWithoutPath;
  end;

implementation

uses
  LCLProc, FileUtil, uOSUtils, DCOSUtils, DCStrUtils, uMultiArc,
  fMultiArchiveCopyOperationOptions, uMultiArchiveUtil, uFileProcs, uLng, DCDateTimeUtils,
  DCBasicTypes, uShowMsg, uFileSystemUtil;

constructor TMultiArchiveCopyOutOperation.Create(aSourceFileSource: IFileSource;
                                               aTargetFileSource: IFileSource;
                                               var theSourceFiles: TFiles;
                                               aTargetPath: String);
begin
  FMultiArchiveFileSource := aSourceFileSource as IMultiArchiveFileSource;
  FPassword:= FMultiArchiveFileSource.Password;
  FFullFilesTreeToExtract:= nil;
  FFileExistsOption := fsoofeNone;
  FExtractWithoutPath:= False;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
  with FStatistics do
  begin
    DoneBytes := -1;
    DoneFiles := -1;
    RemainingTime := -1;
    CurrentFileDoneBytes := -1;
    UpdateStatistics(FStatistics);
  end;
end;

destructor TMultiArchiveCopyOutOperation.Destroy;
begin
  FreeAndNil(FFullFilesTreeToExtract);
  inherited Destroy;
end;

procedure TMultiArchiveCopyOutOperation.Initialize;
var
  Index: Integer;
  ACount: Integer;
  AFileName: String;
  ArcFileList: TList;
begin
  FExProcess:= TExProcess.Create(EmptyStr);
  FExProcess.OnReadLn:= @OnReadLn;
  FExProcess.OnOperationProgress:= @CheckOperationState;
  FTempFile:= GetTempName(GetTempFolder);

  with FMultiArchiveFileSource.MultiArcItem do
  if Length(FPasswordQuery) <> 0 then
  begin
    FExProcess.QueryString:= UTF8ToConsole(FPasswordQuery);
    FExProcess.OnQueryString:= @OnQueryString;
  end;

  if efSmartExtract in ExtractFlags then
  begin
    ACount:= 0;
    ArcFileList := FMultiArchiveFileSource.ArchiveFileList.Clone;
    try
      for Index := 0 to ArcFileList.Count - 1 do
      begin
        AFileName := PathDelim + TArchiveItem(ArcFileList[Index]).FileName;

        if IsInPath(PathDelim, AFileName, False, False) then
        begin
          Inc(ACount);
          if (ACount > 1) then
          begin
            FTargetPath := FTargetPath + ExtractOnlyFileName(FMultiArchiveFileSource.ArchiveFileName) + PathDelim;
            Break;
          end;
        end;
      end;
    finally
      ArcFileList.Free;
    end;
  end;

  AddStateChangedListener([fsosStarting, fsosPausing, fsosStopping], @FileSourceOperationStateChangedNotify);

  if FExtractMask = '' then FExtractMask := '*';  // extract all selected files/folders

  with FMultiArchiveFileSource do
  FillAndCount(FExtractMask, SourceFiles,
               True,
               FFullFilesTreeToExtract,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)
end;

procedure TMultiArchiveCopyOutOperation.MainExecute;
var
  TargetFileName,
  SourcePath,
  sTempDir: String;
  CreatedPaths: TStringHashListUtf8 = nil;
  I: Integer;
  aFile: TFile;
  MultiArcItem: TMultiArcItem;
  sReadyCommand,
  sCommandLine: String;
  FilesToExtract: TFiles = nil;
begin
  MultiArcItem := FMultiArchiveFileSource.MultiArcItem;
  try
    // Archive current path
    SourcePath:= ExcludeFrontPathDelimiter(SourceFiles.Path);
    // Check ExtractWithoutPath option
    if FExtractWithoutPath then
      sCommandLine:= MultiArcItem.FExtractWithoutPath
    else
      begin
        // Create needed directories.
        CreatedPaths := TStringHashListUtf8.Create(True);
        CreateDirs(FFullFilesTreeToExtract, TargetPath, SourcePath, CreatedPaths);
        sCommandLine:= MultiArcItem.FExtract;
      end;
    if not FExtractWithoutPath then
    begin
      sTempDir:= GetTempName(TargetPath);
      mbCreateDir(sTempDir);
    end;
    // Get maximum acceptable command errorlevel
    FErrorLevel:= ExtractErrorLevel(sCommandLine);
    if Pos('%F', sCommandLine) <> 0 then // extract file by file
    begin
      FStatistics.DoneBytes:= 0;

      for I:= 0 to FFullFilesTreeToExtract.Count - 1 do
      begin
        CheckOperationState;
        aFile:= FFullFilesTreeToExtract[I];
        // Now check if the file is to be extracted.
        if (not aFile.AttributesProperty.IsDirectory) then  // Omit directories (we handle them ourselves).
          begin
            // Check ExtractWithoutPath option
            if FExtractWithoutPath then
              TargetFileName := TargetPath + aFile.Name
            else
              TargetFileName := TargetPath + ExtractDirLevel(SourcePath, aFile.FullPath);

            // Check existence of target file
            if (DoFileExists(aFile, TargetFileName) <> fsoofeOverwrite) then
              Continue;


            UpdateProgress(aFile.FullPath, TargetFileName, 0);

            sReadyCommand:= FormatArchiverCommand(
                                                  MultiArcItem.FArchiver,
                                                  sCommandLine,
                                                  FMultiArchiveFileSource.ArchiveFileName,
                                                  nil,
                                                  aFile.FullPath,
                                                  TargetPath,
                                                  FTempFile,
                                                  FPassword
                                                  );
            OnReadLn(sReadyCommand);

            // Set target directory as archiver current directory
            if FExtractWithoutPath then
              FExProcess.Process.CurrentDirectory:= TargetPath
            else
              FExProcess.Process.CurrentDirectory:= sTempDir;

            FExProcess.SetCmdLine(sReadyCommand);
            FExProcess.Execute;

            if not FExtractWithoutPath then
              mbRenameFile(sTempDir + PathDelim + aFile.FullPath, TargetFileName);

            UpdateProgress(aFile.FullPath, TargetFileName, aFile.Size);
            // Check for errors.
            CheckForErrors(aFile.FullPath, TargetFileName, FExProcess.ExitStatus);
          end;
    end // for
  end
  else  // extract whole file list
    begin
      // Check existence of target files
      FilesToExtract:= TFiles.Create(FFullFilesTreeToExtract.Path);
      for I:= 0 to FFullFilesTreeToExtract.Count - 1 do
      begin
        aFile:= FFullFilesTreeToExtract[I];
        if FExtractWithoutPath then
           TargetFileName := TargetPath + aFile.Name
        else
           TargetFileName := TargetPath + ExtractDirLevel(SourcePath, aFile.FullPath);
        if (DoFileExists(aFile, TargetFileName) = fsoofeOverwrite) then
          FilesToExtract.Add(aFile.Clone);
      end;

      if FilesToExtract.Count = 0 then Exit;

      with FStatistics do
      begin
        if FilesToExtract.Count = 1 then
        begin
          FStatistics.CurrentFileFrom:= FilesToExtract[0].FullPath;
          FStatistics.CurrentFileTo:= TargetFileName;
        end
        else begin
          FStatistics.CurrentFileFrom:= SourceFiles.Path;
          FStatistics.CurrentFileTo:= TargetPath;
        end;
        UpdateStatistics(FStatistics);
      end;

      sReadyCommand:= FormatArchiverCommand(
                                            MultiArcItem.FArchiver,
                                            sCommandLine,
                                            FMultiArchiveFileSource.ArchiveFileName,
                                            FilesToExtract,
                                            EmptyStr,
                                            TargetPath,
                                            FTempFile,
                                            FPassword
                                            );
      OnReadLn(sReadyCommand);

      // Set target directory as archiver current directory
      if (SourceFiles.Path = PathDelim) or FExtractWithoutPath then
        FExProcess.Process.CurrentDirectory:= TargetPath
      else
        FExProcess.Process.CurrentDirectory:= sTempDir;

      FExProcess.SetCmdLine(sReadyCommand);
      FExProcess.Execute;

      // Check for errors.
      CheckForErrors(FMultiArchiveFileSource.ArchiveFileName, EmptyStr, FExProcess.ExitStatus);

      // if extract from not root directory and with path
      if (SourceFiles.Path <> PathDelim) and (FExtractWithoutPath = False) then
      begin
        FStatistics.DoneBytes:= 0;
        // move files to real target directory
        for I:= 0 to FilesToExtract.Count - 1 do
        begin
          aFile:= FilesToExtract[I];
          if not aFile.AttributesProperty.IsDirectory then
            begin
              TargetFileName := TargetPath + ExtractDirLevel(SourcePath, aFile.FullPath);
              UpdateProgress(aFile.FullPath, TargetFileName, 0);
              mbRenameFile(sTempDir + PathDelim + aFile.FullPath, TargetFileName);
              UpdateProgress(aFile.FullPath, TargetFileName, aFile.Size);
            end
        end;
      end;
    end;

    if not FExtractWithoutPath then
    begin
      SetDirsAttributes(CreatedPaths);
      DelTree(sTempDir);
    end;

  finally
    FreeAndNil(CreatedPaths);
    FreeAndNil(FilesToExtract);
  end;
end;

procedure TMultiArchiveCopyOutOperation.Finalize;
begin
  FreeAndNil(FExProcess);
  with FMultiArchiveFileSource.MultiArcItem do
  if not FDebug then
    mbDeleteFile(FTempFile);
end;

procedure TMultiArchiveCopyOutOperation.CreateDirs(
              const theFiles: TFiles;
              sDestPath: String; CurrentArchiveDir: String;
              var CreatedPaths: TStringHashListUtf8);
var
  // List of paths that we know must be created.
  PathsToCreate: TStringHashListUtf8;

  // List of possible directories to create with their attributes.
  // This hash list is created to speed up searches for attributes in archive file list.
  DirsAttributes: TStringHashListUtf8;

  i: Integer;
  CurrentFileName: String;
  aFile: TFile;
  Directories: TStringList;
  PathIndex: Integer;
  ListIndex: Integer;
  TargetDir: String;
begin
  { First, collect all the paths that need to be created and their attributes. }

  PathsToCreate := TStringHashListUtf8.Create(True);
  DirsAttributes := TStringHashListUtf8.Create(True);

  for I := 0 to theFiles.Count - 1 do
  begin
    aFile := theFiles[I];

    if aFile.AttributesProperty.IsDirectory then
      begin
        CurrentFileName := ExtractDirLevel(CurrentArchiveDir, aFile.FullPath);

        // Save this directory and a pointer to its entry.
        DirsAttributes.Add(CurrentFileName, aFile);

        // Paths in PathsToCreate list must end with path delimiter.
        CurrentFileName := IncludeTrailingPathDelimiter(CurrentFileName);

        if PathsToCreate.Find(CurrentFileName) < 0 then
          PathsToCreate.Add(CurrentFileName);
      end
    else
      begin
        CurrentFileName := ExtractDirLevel(CurrentArchiveDir, aFile.Path);

        // If CurrentFileName is empty now then it was a file in current archive
        // directory, therefore we don't have to create any paths for it.
        if Length(CurrentFileName) > 0 then
          if PathsToCreate.Find(CurrentFileName) < 0 then
            PathsToCreate.Add(CurrentFileName);
      end;
  end;

  { Second, create paths and save which paths were created and their attributes. }

  Directories := TStringList.Create;

  try
    sDestPath := IncludeTrailingPathDelimiter(sDestPath);

    // Create path to destination directory (we don't have attributes for that).
    mbForceDirectory(sDestPath);

    CreatedPaths.Clear;

    for PathIndex := 0 to PathsToCreate.Count - 1 do
    begin
      Directories.Clear;

      // Create also all parent directories of the path to create.
      // This adds directories to list in order from the outer to inner ones,
      // for example: dir, dir/dir2, dir/dir2/dir3.
      if GetDirs(PathsToCreate.List[PathIndex]^.Key, Directories) <> -1 then
      try
        for i := 0 to Directories.Count - 1 do
        begin
          TargetDir := sDestPath + Directories.Strings[i];

          if (CreatedPaths.Find(TargetDir) = -1) and
             (not DirPathExists(TargetDir)) then
          begin
             if mbForceDirectory(TargetDir) = False then
             begin
               // Error, cannot create directory.
               Break; // Don't try to create subdirectories.
             end
             else
             begin
               // Retrieve attributes for this directory, if they are stored.
               ListIndex := DirsAttributes.Find(Directories.Strings[i]);
               if ListIndex <> -1 then
                 aFile := TFile(DirsAttributes.List[ListIndex]^.Data)
               else
                 aFile := nil;

               CreatedPaths.Add(TargetDir, aFile);
             end;
          end;
        end;
      except
      end;
    end;

  finally
    FreeAndNil(PathsToCreate);
    FreeAndNil(DirsAttributes);
    FreeAndNil(Directories);
  end;
end;

function TMultiArchiveCopyOutOperation.SetDirsAttributes(const Paths: TStringHashListUtf8): Boolean;
var
  PathIndex: Integer;
  TargetDir: String;
  aFile: TFile;
  Time: TFileTime;
begin
  Result := True;

  for PathIndex := 0 to Paths.Count - 1 do
  begin
    // Get attributes.
    aFile := TFile(Paths.List[PathIndex]^.Data);

    if Assigned(aFile) then
    begin
      TargetDir := Paths.List[PathIndex]^.Key;

      try
{$IF DEFINED(MSWINDOWS)}
        // Restore attributes, e.g., hidden, read-only.
        // On Unix attributes value would have to be translated somehow.
        mbFileSetAttr(TargetDir, aFile.Attributes);
{$ENDIF}
       Time:= DateTimeToFileTime(aFile.ModificationTime);
        // Set creation, modification time
        mbFileSetTime(TargetDir, Time, Time, Time);

      except
        Result := False;
      end;
    end;
  end;
end;

procedure TMultiArchiveCopyOutOperation.QuestionActionHandler(
  Action: TFileSourceOperationUIAction);
var aFile: TFile;
begin
  if Action = fsouaCompare then
  begin
    aFile := FCurrentFile.Clone;
    try
      aFile.FullPath := IncludeFrontPathDelimiter(aFile.FullPath);
      ShowCompareFilesUI(aFile, FCurrentTargetFilePath);
    finally
      aFile.Free;
    end;
  end;
end;

function TMultiArchiveCopyOutOperation.DoFileExists(aFile: TFile;
  const AbsoluteTargetFileName: String): TFileSourceOperationOptionFileExists;
const
  PossibleResponses: array[0..8] of TFileSourceOperationUIResponse
    = (fsourOverwrite, fsourSkip, fsourOverwriteLarger, fsourOverwriteAll,
       fsourSkipAll, fsourOverwriteSmaller, fsourOverwriteOlder, fsouaCompare,
       fsourCancel);
var
  Message: String;

  function OverwriteOlder: TFileSourceOperationOptionFileExists;
  begin
    if aFile.ModificationTime > FileTimeToDateTime(mbFileAge(AbsoluteTargetFileName)) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

  function OverwriteSmaller: TFileSourceOperationOptionFileExists;
  begin
    if aFile.Size > mbFileSize(AbsoluteTargetFileName) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

  function OverwriteLarger: TFileSourceOperationOptionFileExists;
  begin
    if aFile.Size < mbFileSize(AbsoluteTargetFileName) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

begin
  if not mbFileExists(AbsoluteTargetFileName) then
    Result:= fsoofeOverwrite
  else case FFileExistsOption of
    fsoofeNone:
      begin
        Message:= FileExistsMessage(AbsoluteTargetFileName, aFile.FullPath,
                                    aFile.Size, aFile.ModificationTime);
        FCurrentFile := aFile;
        FCurrentTargetFilePath := AbsoluteTargetFileName;
        case AskQuestion(Message, '',
                         PossibleResponses, fsourOverwrite, fsourSkip,
                         @QuestionActionHandler) of
          fsourOverwrite:
            Result := fsoofeOverwrite;
          fsourSkip:
            Result := fsoofeSkip;
          fsourOverwriteAll:
            begin
              FFileExistsOption := fsoofeOverwrite;
              Result := fsoofeOverwrite;
            end;
          fsourSkipAll:
            begin
              FFileExistsOption := fsoofeSkip;
              Result := fsoofeSkip;
            end;
          fsourOverwriteOlder:
            begin
              FFileExistsOption := fsoofeOverwriteOlder;
              Result:= OverwriteOlder;
            end;
          fsourOverwriteSmaller:
            begin
              FFileExistsOption := fsoofeOverwriteSmaller;
              Result:= OverwriteSmaller;
            end;
          fsourOverwriteLarger:
            begin
              FFileExistsOption := fsoofeOverwriteLarger;
              Result:= OverwriteLarger;
            end;
          fsourNone,
          fsourCancel:
            RaiseAbortOperation;
        end;
      end;
    fsoofeOverwriteOlder:
      begin
        Result:= OverwriteOlder;
      end;
    fsoofeOverwriteSmaller:
      begin
        Result:= OverwriteSmaller;
      end;
    fsoofeOverwriteLarger:
      begin
        Result:= OverwriteLarger;
      end;
    else begin
      Result := FFileExistsOption;
    end;
  end;
end;

procedure TMultiArchiveCopyOutOperation.ShowError(sMessage: String; logOptions: TLogOptions);
begin
  if not gSkipFileOpError then
  begin
    if AskQuestion(sMessage, '', [fsourSkip, fsourCancel],
                   fsourSkip, fsourAbort) = fsourAbort then
    begin
      RaiseAbortOperation;
    end;
  end
  else
  begin
    LogMessage(sMessage, logOptions, lmtError);
  end;
end;

procedure TMultiArchiveCopyOutOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
begin
  case logMsgType of
    lmtError:
      if not (log_errors in gLogOptions) then Exit;
    lmtInfo:
      if not (log_info in gLogOptions) then Exit;
    lmtSuccess:
      if not (log_success in gLogOptions) then Exit;
  end;

  if logOptions <= gLogOptions then
  begin
    logWrite(Thread, sMessage, logMsgType);
  end;
end;

procedure TMultiArchiveCopyOutOperation.CheckForErrors(const SourceName,
                                                       TargetName: String;
                                                       ExitStatus: LongInt);
begin
  if ExitStatus > FErrorLevel then
    begin
      ShowError(Format(rsMsgLogError + rsMsgLogExtract,
                       [FMultiArchiveFileSource.ArchiveFileName + PathDelim +
                        SourceName + ' -> ' + TargetName +
                        ' - ' + rsMsgExitStatusCode + ' ' + IntToStr(ExitStatus)]), [log_arc_op]);
    end // Error
  else
    begin
      LogMessage(Format(rsMsgLogSuccess + rsMsgLogExtract,
                        [FMultiArchiveFileSource.ArchiveFileName + PathDelim +
                         SourceName +' -> ' + TargetName]), [log_arc_op], lmtSuccess);
    end; // Success
end;

procedure TMultiArchiveCopyOutOperation.OnReadLn(str: string);
begin
  with FMultiArchiveFileSource.MultiArcItem do
  if FOutput or FDebug then
    logWrite(Thread, str, lmtInfo, True, False);
end;

procedure TMultiArchiveCopyOutOperation.OnQueryString(str: string);
var
  pcPassword: PAnsiChar;
begin
  ShowInputQuery(FMultiArchiveFileSource.MultiArcItem.FDescription, rsMsgPasswordEnter, True, FPassword);
  pcPassword:= PAnsiChar(UTF8ToConsole(FPassword + LineEnding));
  FExProcess.Process.Input.Write(pcPassword^, Length(pcPassword));
end;

procedure TMultiArchiveCopyOutOperation.UpdateProgress(SourceName,
  TargetName: String; IncSize: Int64);
begin
  with FStatistics do
  begin
    FStatistics.CurrentFileFrom:= SourceName;
    FStatistics.CurrentFileTo:= TargetName;

    CurrentFileDoneBytes:= IncSize;
    DoneBytes := DoneBytes + CurrentFileDoneBytes;

    UpdateStatistics(FStatistics);
  end;
end;

procedure TMultiArchiveCopyOutOperation.FileSourceOperationStateChangedNotify(
  Operation: TFileSourceOperation; AState: TFileSourceOperationState);
begin
  case AState of
    fsosStarting:
      FExProcess.Process.Resume;
    fsosPausing:
      FExProcess.Process.Suspend;
    fsosStopping:
      FExProcess.Stop;
  end;
end;

class function TMultiArchiveCopyOutOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result:= TMultiArchiveCopyOperationOptionsUI;
end;

end.

