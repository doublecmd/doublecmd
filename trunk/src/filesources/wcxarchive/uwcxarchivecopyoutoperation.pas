unit uWcxArchiveCopyOutOperation;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  Classes, LazFileUtils,SysUtils, StringHashList, WcxPlugin, uLog, uGlobs,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI,
  uFile,
  uWcxModule,
  uWcxArchiveFileSource;

type

  { TWcxArchiveCopyOutOperation }

  TWcxArchiveCopyOutOperation = class(TFileSourceCopyOutOperation)

  private
    FWcxArchiveFileSource: IWcxArchiveFileSource;
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    FCurrentFileSize: Int64;
    FRenamingFiles: Boolean;
    FRenameNameMask, FRenameExtMask: String;

    // Options.
    FExtractWithoutPath: Boolean;

    {en
      Creates neccessary paths before extracting files from archive.
      Also counts size of all files that will be extracted.

      @param(Files
             List of files/directories to extract (relative to archive root).)
      @param(FileMask
             Only directories containing files matching this mask will be created.)
      @param(sDestPath
             Destination path where the files will be extracted.)
      @param(CurrentArchiveDir
             Path inside the archive from where the files will be extracted.)
      @param(CreatedPaths
             This list will be filled with absolute paths to directories
             that were created, together with their attributes.)}
    procedure CreateDirsAndCountFiles(const theFiles: TFiles; FileMask: String;
                                      sDestPath: String; CurrentArchiveDir: String;
                                      var CreatedPaths: TStringHashList);

    {en
      Sets attributes for directories.
      @param(Paths
             The list of absolute paths, which attributes are to be set.
             Each list item's data field must be a pointer to THeaderData,
             from where the attributes are retrieved.}
    function SetDirsAttributes(const Paths: TStringHashList): Boolean;

    function DoFileExists(Header: TWcxHeader; var AbsoluteTargetFileName: String): TFileSourceOperationOptionFileExists;
	
    procedure ShowError(const sMessage: String; iError: Integer; logOptions: TLogOptions = []);
    procedure LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  protected
    procedure SetProcessDataProc(hArcData: TArcHandle);

  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

    class procedure ClearCurrentOperation;

    function GetDescription(Details: TFileSourceOperationDescriptionDetails): String; override;
    class function GetOptionsUIClass: TFileSourceOperationOptionsUIClass; override;

    property ExtractWithoutPath: Boolean read FExtractWithoutPath write FExtractWithoutPath;
  end;

implementation

uses
  Forms, LazUTF8, uMasks, FileUtil, contnrs, DCOSUtils, DCStrUtils, uDCUtils,
  uFileSourceOperationUI, fWcxArchiveCopyOperationOptions, uFileSystemUtil,
  uFileProcs, uLng, DCDateTimeUtils, DCBasicTypes, uShowMsg, DCConvertEncoding;

// ----------------------------------------------------------------------------
// WCX callbacks

var
  // This global variable is used to store currently running operation
  // for plugins that not supports background operations (see GetBackgroundFlags)
  WcxCopyOutOperationG: TWcxArchiveCopyOutOperation = nil;

threadvar
  // This thread variable is used to store currently running operation
  // for plugins that supports background operations (see GetBackgroundFlags)
  WcxCopyOutOperationT: TWcxArchiveCopyOutOperation;

function ProcessDataProc(WcxCopyOutOperation: TWcxArchiveCopyOutOperation;
                         FileName: String; Size: LongInt): LongInt;
begin
  //DCDebug('Working (' + IntToStr(GetCurrentThreadId) + ') ' + FileName + ' Size = ' + IntToStr(Size));

  Result := 1;

  if Assigned(WcxCopyOutOperation) then
  begin
    if WcxCopyOutOperation.State = fsosStopping then  // Cancel operation
      Exit(0);

    with WcxCopyOutOperation.FStatistics do
    begin
      // Get the number of bytes processed since the previous call
      if Size > 0 then
      begin
        CurrentFileDoneBytes := CurrentFileDoneBytes + Size;
        if CurrentFileDoneBytes > CurrentFileTotalBytes then
          CurrentFileDoneBytes := CurrentFileTotalBytes;
        DoneBytes := DoneBytes + Size;
      end
      // Get progress percent value to directly set progress bar
      else if Size < 0 then
      begin
        CurrentFileFrom:= FileName;
        // Total operation percent
        if (Size >= -100) and (Size <= -1) then
          begin
            DoneBytes := TotalBytes * Int64(-Size) div 100;
          end
        // Current file percent
        else if (Size >= -1100) and (Size <= -1000) then
          begin
            CurrentFileTotalBytes := 100;
            CurrentFileDoneBytes := Int64(-Size) - 1000;
          end;
      end;

      //DCDebug('CurrentDone  = ' + IntToStr(CurrentFileDoneBytes) + ' Done  = ' + IntToStr(DoneBytes));
      //DCDebug('CurrentTotal = ' + IntToStr(CurrentFileTotalBytes) + ' Total = ' + IntToStr(TotalBytes));
      WcxCopyOutOperation.UpdateStatistics(WcxCopyOutOperation.FStatistics);
      if not WcxCopyOutOperation.CheckOperationStateSafe then Exit(0);
    end;
  end;
end;

function ProcessDataProcAG(FileName: PAnsiChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxCopyOutOperationG, CeSysToUtf8(StrPas(FileName)), Size);
end;

function ProcessDataProcWG(FileName: PWideChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxCopyOutOperationG, UTF16ToUTF8(UnicodeString(FileName)), Size);
end;

function ProcessDataProcAT(FileName: PAnsiChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxCopyOutOperationT, CeSysToUtf8(StrPas(FileName)), Size);
end;

function ProcessDataProcWT(FileName: PWideChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxCopyOutOperationT, UTF16ToUTF8(UnicodeString(FileName)), Size);
end;

// ----------------------------------------------------------------------------

constructor TWcxArchiveCopyOutOperation.Create(aSourceFileSource: IFileSource;
                                               aTargetFileSource: IFileSource;
                                               var theSourceFiles: TFiles;
                                               aTargetPath: String);
begin
  FWcxArchiveFileSource := aSourceFileSource as IWcxArchiveFileSource;
  FFileExistsOption := fsoofeNone;
  FExtractWithoutPath := False;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);

  FNeedsConnection:= (FWcxArchiveFileSource.WcxModule.BackgroundFlags and BACKGROUND_UNPACK = 0);
end;

destructor TWcxArchiveCopyOutOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TWcxArchiveCopyOutOperation.Initialize;
begin
  // Is plugin allow multiple Operations?
  if FNeedsConnection then
    WcxCopyOutOperationG := Self
  else
    WcxCopyOutOperationT := Self;

  // Extract without path from flat view
  if not FExtractWithoutPath then begin
    FExtractWithoutPath := SourceFiles.Flat;
  end;

  // Check rename mask
  FRenamingFiles := (RenameMask <> '*.*') and (RenameMask <> '');
  if FRenamingFiles then SplitFileMask(RenameMask, FRenameNameMask, FRenameExtMask);
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
end;

procedure TWcxArchiveCopyOutOperation.MainExecute;
var
  ArcHandle: TArcHandle;
  Header: TWCXHeader;
  TargetFileName: String;
  FileMask: String;
  CreatedPaths: TStringHashList;
  OpenResult: Longint;
  iResult: Integer;
  Files: TFiles = nil;
  WcxModule: TWcxModule;
begin
  WcxModule := FWcxArchiveFileSource.WcxModule;

  ArcHandle := WcxModule.OpenArchiveHandle(FWcxArchiveFileSource.ArchiveFileName,
                                           PK_OM_EXTRACT,
                                           OpenResult);
  if ArcHandle = 0 then
  begin
    AskQuestion(uWcxModule.GetErrorMsg(OpenResult), '', [fsourOk], fsourOk, fsourOk);
    RaiseAbortOperation;
  end;

  FileMask := ExtractFileName(TargetPath);
  if FileMask = '' then FileMask := '*';  // extract all selected files/folders

  // Convert file list so that filenames are relative to archive root.
  Files := SourceFiles.Clone;
  ChangeFileListRoot(PathDelim, Files);

  CreatedPaths := TStringHashList.Create(True);

  try
    // Count total files size and create needed directories.
    CreateDirsAndCountFiles(Files, FileMask,
                            TargetPath, Files.Path,
                            CreatedPaths);

    SetProcessDataProc(ArcHandle);
    FWcxArchiveFileSource.SetChangeVolProc(ArcHandle);

    while (WcxModule.ReadWCXHeader(ArcHandle, Header) = E_SUCCESS) do
    try
      CheckOperationState;

      // Now check if the file is to be extracted.

      if  (not FPS_ISDIR(Header.FileAttr))           // Omit directories (we handle them ourselves).
      and MatchesFileList(Files, Header.FileName)    // Check if it's included in the filelist
      and ((FileMask = '*.*') or (FileMask = '*')    // And name matches file mask
          or MatchesMaskList(ExtractFileName(Header.FileName), FileMask))
      then
      begin
        if FExtractWithoutPath then
          TargetFileName := TargetPath + ExtractFileName(Header.FileName)
        else
          TargetFileName := TargetPath + ExtractDirLevel(Files.Path, Header.FileName);

        if FRenamingFiles then
        begin
          TargetFileName := ExtractFilePath(TargetFileName) +
                            ApplyRenameMask(ExtractFileName(TargetFileName),
                                            FRenameNameMask, FRenameExtMask);

        end;

        with FStatistics do
        begin
          CurrentFileFrom := Header.FileName;
          CurrentFileTo := TargetFileName;
          CurrentFileTotalBytes := Header.UnpSize;
          CurrentFileDoneBytes := 0;

          UpdateStatistics(FStatistics);
          FCurrentFileSize := Header.UnpSize;
        end;

        if (DoFileExists(Header, TargetFileName) = fsoofeOverwrite) then
          iResult := WcxModule.WcxProcessFile(ArcHandle, PK_EXTRACT, EmptyStr, TargetFileName)
        else
          iResult := WcxModule.WcxProcessFile(ArcHandle, PK_SKIP, EmptyStr, EmptyStr);

        if iResult <> E_SUCCESS then
        begin
          // User aborted operation.
          if iResult = E_EABORTED then Break;

          ShowError(Format(rsMsgLogError + rsMsgLogExtract,
                           [FWcxArchiveFileSource.ArchiveFileName + PathDelim +
                            Header.FileName + ' -> ' + TargetFileName +
                            ' : ' + GetErrorMsg(iResult)]), iResult, [log_arc_op]);
        end // Error
        else
        begin
          LogMessage(Format(rsMsgLogSuccess + rsMsgLogExtract,
                            [FWcxArchiveFileSource.ArchiveFileName + PathDelim +
                             Header.FileName +' -> ' + TargetFileName]), [log_arc_op], lmtSuccess);
        end; // Success
      end // Extract
      else // Skip
      begin
        iResult := WcxModule.WcxProcessFile(ArcHandle, PK_SKIP, EmptyStr, EmptyStr);

        //Check for errors
        if iResult <> E_SUCCESS then
        begin
          ShowError(Format(rsMsgLogError + rsMsgLogExtract,
                           [FWcxArchiveFileSource.ArchiveFileName + PathDelim +
                            Header.FileName + ' -> ' + TargetFileName +
                            ' : ' + GetErrorMsg(iResult)]), iResult, [log_arc_op]);
        end;
      end; // Skip

    finally
      FreeAndNil(Header);
    end;

    if (FExtractWithoutPath = False) then SetDirsAttributes(CreatedPaths);

  finally
    // Close archive
    iResult := WcxModule.CloseArchive(ArcHandle);
    // Check for errors
    if iResult <> E_SUCCESS then
    begin
      ShowError(Format(rsMsgLogError + rsMsgLogExtract,
                       [FWcxArchiveFileSource.ArchiveFileName +
                        ' : ' + GetErrorMsg(iResult)]), iResult, [log_arc_op]);
    end;
    // Free memory
    FreeAndNil(Files);
    FreeAndNil(CreatedPaths);
  end;
end;

procedure TWcxArchiveCopyOutOperation.Finalize;
begin
  ClearCurrentOperation;
end;

function TWcxArchiveCopyOutOperation.GetDescription(Details: TFileSourceOperationDescriptionDetails): String;
begin
  case Details of
    fsoddJobAndTarget:
      Result := Format(rsOperExtractingFromTo, [FWcxArchiveFileSource.ArchiveFileName, TargetPath]);
    else
      Result := rsOperExtracting;
  end;
end;

procedure TWcxArchiveCopyOutOperation.CreateDirsAndCountFiles(
              const theFiles: TFiles; FileMask: String;
              sDestPath: String; CurrentArchiveDir: String;
              var CreatedPaths: TStringHashList);
var
  // List of paths that we know must be created.
  PathsToCreate: TStringHashList;

  // List of possible directories to create with their attributes.
  // This hash list is created to speed up searches for attributes in archive file list.
  DirsAttributes: TStringHashList;

  i: Integer;
  CurrentFileName: String;
  Header: TWCXHeader;
  Directories: TStringList = nil;
  PathIndex: Integer;
  ListIndex: Integer;
  TargetDir: String;
  FileList: TObjectList;
begin
  FileList := FWcxArchiveFileSource.ArchiveFileList;

  { First, collect all the paths that need to be created and their attributes. }

  PathsToCreate := TStringHashList.Create(True);
  DirsAttributes := TStringHashList.Create(True);

  for i := 0 to FileList.Count - 1 do
  begin
    Header := TWCXHeader(FileList.Items[i]);

    // Check if the file from the archive fits the selection given via SourceFiles.
    if not MatchesFileList(theFiles, Header.FileName) then
      Continue;

    if FPS_ISDIR(Header.FileAttr) then
    begin
      CurrentFileName := ExtractDirLevel(CurrentArchiveDir, Header.FileName);

      // Save this directory and a pointer to its entry.
      DirsAttributes.Add(CurrentFileName, Header);

      // If extracting all files and directories, add this directory
      // to PathsToCreate so that empty directories are also created.
      if (FileMask = '*.*') or (FileMask = '*') then
      begin
        // Paths in PathsToCreate list must end with path delimiter.
        CurrentFileName := IncludeTrailingPathDelimiter(CurrentFileName);

        if PathsToCreate.Find(CurrentFileName) < 0 then
          PathsToCreate.Add(CurrentFileName);
      end;
    end
    else
    begin
      if ((FileMask = '*.*') or (FileMask = '*') or
          MatchesMaskList(ExtractFileName(Header.FileName), FileMask)) then
      begin
        Inc(FStatistics.TotalBytes, Header.UnpSize);
        Inc(FStatistics.TotalFiles, 1);

        CurrentFileName := ExtractDirLevel(CurrentArchiveDir, ExtractFilePath(Header.FileName));

        // If CurrentFileName is empty now then it was a file in current archive
        // directory, therefore we don't have to create any paths for it.
        if Length(CurrentFileName) > 0 then
          if PathsToCreate.Find(CurrentFileName) < 0 then
            PathsToCreate.Add(CurrentFileName);
      end;
    end;
  end;

  if FExtractWithoutPath then Exit;

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
                 Header := TWcxHeader(DirsAttributes.List[ListIndex]^.Data)
               else
                 Header := nil;

               CreatedPaths.Add(TargetDir, Header);
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

function TWcxArchiveCopyOutOperation.SetDirsAttributes(const Paths: TStringHashList): Boolean;
var
  PathIndex: Integer;
  TargetDir: String;
  Header: TWCXHeader;
  Time: TFileTime;
begin
  Result := True;

  for PathIndex := 0 to Paths.Count - 1 do
  begin
    // Get attributes.
    Header := TWCXHeader(Paths.List[PathIndex]^.Data);

    if Assigned(Header) then
    begin
      TargetDir := Paths.List[PathIndex]^.Key;

      try
        // Restore attributes
        mbFileSetAttr(TargetDir, Header.FileAttr);

{$IF DEFINED(MSWINDOWS)}
        DosToWinTime(TDosFileTime(Header.FileTime), Time);
{$ELSE}
  {$PUSH}{$R-}
        Time := Header.FileTime;
  {$POP}
{$ENDIF}

        // Set creation, modification time
        mbFileSetTime(TargetDir, Time, Time, Time);

      except
        Result := False;
      end;
    end;
  end;
end;

function TWcxArchiveCopyOutOperation.DoFileExists(Header: TWcxHeader;
  var AbsoluteTargetFileName: String): TFileSourceOperationOptionFileExists;
const
  PossibleResponses: array[0..9] of TFileSourceOperationUIResponse
    = (fsourOverwrite, fsourSkip, fsourOverwriteLarger, fsourOverwriteAll,
       fsourSkipAll, fsourOverwriteSmaller, fsourOverwriteOlder, fsourCancel,
       fsourRenameSource, fsourAutoRenameSource);
var
  Answer: Boolean;
  Message: String;

  function OverwriteOlder: TFileSourceOperationOptionFileExists;
  begin
    if WcxFileTimeToDateTime(Header.FileTime) > FileTimeToDateTime(mbFileAge(AbsoluteTargetFileName)) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

  function OverwriteSmaller: TFileSourceOperationOptionFileExists;
  begin
    if Header.UnpSize > mbFileSize(AbsoluteTargetFileName) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

  function OverwriteLarger: TFileSourceOperationOptionFileExists;
  begin
    if Header.UnpSize < mbFileSize(AbsoluteTargetFileName) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

begin
  if not mbFileExists(AbsoluteTargetFileName) then
    Result:= fsoofeOverwrite
  else case FFileExistsOption of
    fsoofeNone:
      repeat
        Answer := True;
        Message:= FileExistsMessage(AbsoluteTargetFileName, Header.FileName,
                                    Header.UnpSize, WcxFileTimeToDateTime(Header.FileTime));
        case AskQuestion(Message, '',
                         PossibleResponses, fsourOverwrite, fsourSkip) of
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
          fsourAutoRenameSource:
            begin
              Result:= fsoofeOverwrite;
              FFileExistsOption:= fsoofeAutoRenameSource;
              AbsoluteTargetFileName:= GetNextCopyName(AbsoluteTargetFileName);
            end;
          fsourRenameSource:
            begin
              Message:= ExtractFileName(AbsoluteTargetFileName);
              Answer:= ShowInputQuery(Thread, Application.Title, rsEditNewFileName, Message);
              if Answer then
              begin
                Result:= fsoofeOverwrite;
                AbsoluteTargetFileName:= ExtractFilePath(AbsoluteTargetFileName) + Message;
              end;
            end;
          fsourNone,
          fsourCancel:
            RaiseAbortOperation;
        end;
      until Answer;
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
    fsoofeAutoRenameSource:
      begin
        Result:= fsoofeOverwrite;
        AbsoluteTargetFileName:= GetNextCopyName(AbsoluteTargetFileName);
      end;
    else begin
      Result := FFileExistsOption;
    end;
  end;
end;

procedure TWcxArchiveCopyOutOperation.ShowError(const sMessage: String;
  iError: Integer; logOptions: TLogOptions);
begin
  LogMessage(sMessage, logOptions, lmtError);

  if (gSkipFileOpError = False) and (iError > E_SUCCESS) then
  begin
    if AskQuestion(sMessage, '', [fsourSkip, fsourAbort],
                   fsourSkip, fsourAbort) = fsourAbort then
    begin
      RaiseAbortOperation;
    end;
  end;
end;

procedure TWcxArchiveCopyOutOperation.LogMessage(const sMessage: String;
  logOptions: TLogOptions; logMsgType: TLogMsgType);
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

procedure TWcxArchiveCopyOutOperation.SetProcessDataProc(hArcData: TArcHandle);
begin
  with FWcxArchiveFileSource.WcxModule do
  begin
    if FNeedsConnection then
      WcxSetProcessDataProc(hArcData, @ProcessDataProcAG, @ProcessDataProcWG)
    else
      WcxSetProcessDataProc(hArcData, @ProcessDataProcAT, @ProcessDataProcWT);
  end;
end;

class procedure TWcxArchiveCopyOutOperation.ClearCurrentOperation;
begin
  WcxCopyOutOperationG := nil;
end;

class function TWcxArchiveCopyOutOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result:= TWcxArchiveCopyOperationOptionsUI;
end;

end.

