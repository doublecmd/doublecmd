unit uMultiArchiveCopyOutOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StringHashList, uLog, uGlobs, un_process,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFile,
  uMultiArchiveFile,
  uMultiArchiveFileSource;

type

  { TMultiArchiveCopyOutOperation }

  TMultiArchiveCopyOutOperation = class(TFileSourceCopyOutOperation)

  private
    FMultiArchiveFileSource: IMultiArchiveFileSource;
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    FFullFilesTreeToExtract: TMultiArchiveFiles;  // source files including all files/dirs in subdirectories
    FCurrentFileSize: Int64;

    {en
      Creates neccessary paths before extracting files from archive.

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
    procedure CreateDirs(const theFiles: TFiles; FileMask: String;
                                      sDestPath: String; CurrentArchiveDir: String;
                                      var CreatedPaths: TStringHashList);

    {en
      Sets attributes for directories.
      @param(Paths
             The list of absolute paths, which attributes are to be set.
             Each list item's data field must be a pointer to THeaderData,
             from where the attributes are retrieved.}
    function SetDirsAttributes(const Paths: TStringHashList): Boolean;

    procedure ShowError(sMessage: String; logOptions: TLogOptions = []);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
    procedure CheckForErrors(const SourceName, TargetName: UTF8String; ExitStatus: LongInt);

  protected
    FExProcess: TExProcess;
    FTempFile: UTF8String;
    FFileMask: UTF8String;
    procedure OnReadLn(str: string);
    procedure UpdateProgress(SourceName, TargetName: UTF8String; IncSize: Int64);
  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

  end;

implementation

uses
  LCLProc, Masks, FileUtil, contnrs, uOSUtils, uDCUtils, uShowMsg, uMultiArc, Process,
  uFileSourceOperationUI, uMultiArchiveUtil, uFileProcs, uLng, uDateTimeUtils, uTypes;

constructor TMultiArchiveCopyOutOperation.Create(aSourceFileSource: IFileSource;
                                               aTargetFileSource: IFileSource;
                                               var theSourceFiles: TFiles;
                                               aTargetPath: String);
begin
  FMultiArchiveFileSource := aSourceFileSource as IMultiArchiveFileSource;
  FFullFilesTreeToExtract:= nil;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TMultiArchiveCopyOutOperation.Destroy;
begin
  FreeThenNil(FFullFilesTreeToExtract);
  inherited Destroy;
end;

procedure TMultiArchiveCopyOutOperation.Initialize;
begin
  FExProcess:= TExProcess.Create(EmptyStr);
  FExProcess.Process.Options:= FExProcess.Process.Options + [poWaitOnExit];
  FExProcess.OnReadLn:= @OnReadLn;
  FTempFile:= GetTempName(GetTempFolder);

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FFileMask := ExtractFileName(TargetPath);
  if FFileMask = '' then FFileMask := '*';  // extract all selected files/folders

  with FMultiArchiveFileSource do
  FillAndCount(FFileMask, SourceFiles as TMultiArchiveFiles,
               True,
               FFullFilesTreeToExtract,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)
end;

procedure TMultiArchiveCopyOutOperation.MainExecute;
var
  TargetFileName,
  sCurrPath: UTF8String;
  CreatedPaths: TStringHashList;
  I: Integer;
  Files: TFiles = nil;
  aFile: TMultiArchiveFile;
  MultiArcItem: TMultiArcItem;
begin
  MultiArcItem := FMultiArchiveFileSource.MultiArcItem;

  // Convert file list so that filenames are relative to archive root.
  Files := SourceFiles.Clone;
  ChangeFileListRoot(PathDelim, Files);

  CreatedPaths := TStringHashList.Create(True);

  try
    // save current path
    sCurrPath:= mbGetCurrentDir;
    // Create needed directories.
    CreateDirs(FFullFilesTreeToExtract, FFileMask,
                            TargetPath, Files.Path,
                            CreatedPaths);

    if Pos('%F', MultiArcItem.FExtract) <> 0 then // extract file by file
      for I:= 0 to FFullFilesTreeToExtract.Count - 1 do
      begin
        CheckOperationState;
        aFile:= FFullFilesTreeToExtract[I] as TMultiArchiveFile;
        // Now check if the file is to be extracted.
        if (not aFile.IsDirectory)           // Omit directories (we handle them ourselves).
           and ((FFileMask = '*.*') or (FFileMask = '*')    // And name matches file mask
           or MatchesMaskList(aFile.Name, FFileMask)) then
          begin
            TargetFileName := TargetPath + ExtractDirLevel(Files.Path, aFile.FullPath);
            // go to target directory
            mbSetCurrentDir(ExtractFileDir(TargetFileName));

            UpdateProgress(aFile.FullPath, TargetFileName, 0);

            FExProcess.SetCmdLine(FormatArchiverCommand(
                                                        MultiArcItem.FArchiver,
                                                        MultiArcItem.FExtract,
                                                        FMultiArchiveFileSource.ArchiveFileName,
                                                        nil,
                                                        aFile.FullPath,
                                                        TargetPath,
                                                        FTempFile));
           FExProcess.Execute;

           UpdateProgress(aFile.FullPath, TargetFileName, aFile.Size);
           // Check for errors.
           CheckForErrors(aFile.FullPath, TargetFileName, FExProcess.ExitStatus);
      end;
    end
  else  // extract whole file list
    begin
      FExProcess.SetCmdLine(FormatArchiverCommand(
                                                  MultiArcItem.FArchiver,
                                                  MultiArcItem.FExtract,
                                                  FMultiArchiveFileSource.ArchiveFileName,
                                                  Files,
                                                  EmptyStr,
                                                  TargetPath,
                                                  FTempFile));
      FExProcess.Execute;

      // Check for errors.
      CheckForErrors(FMultiArchiveFileSource.ArchiveFileName, EmptyStr, FExProcess.ExitStatus);
    end;

    SetDirsAttributes(CreatedPaths);

  finally
    if Assigned(Files) then
      FreeAndNil(Files);
    FreeAndNil(CreatedPaths);
    // restore current path
    mbSetCurrentDir(sCurrPath);
  end;
end;

procedure TMultiArchiveCopyOutOperation.Finalize;
begin
  FreeThenNil(FExProcess);
  mbDeleteFile(FTempFile);
end;

procedure TMultiArchiveCopyOutOperation.CreateDirs(
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
  Header: TArchiveItem;
  Directories: TStringList;
  PathIndex: Integer;
  ListIndex: Integer;
  TargetDir: String;
  FileList: TObjectList;
begin
  FileList := FMultiArchiveFileSource.ArchiveFileList;

  { First, collect all the paths that need to be created and their attributes. }

  PathsToCreate := TStringHashList.Create(True);
  DirsAttributes := TStringHashList.Create(True);

  for i := 0 to FileList.Count - 1 do
  begin
    Header := TArchiveItem(FileList.Items[i]);

    // Check if the file from the archive fits the selection given via SourceFiles.
    if not MatchesFileList(theFiles, Header.FileName) then
      Continue;

    if FPS_ISDIR(Header.Attributes) then
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
        CurrentFileName := ExtractDirLevel(CurrentArchiveDir, ExtractFilePath(Header.FileName));

        // If CurrentFileName is empty now then it was a file in current archive
        // directory, therefore we don't have to create any paths for it.
        if Length(CurrentFileName) > 0 then
          if PathsToCreate.Find(CurrentFileName) < 0 then
            PathsToCreate.Add(CurrentFileName);
      end;
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
                 Header := TArchiveItem(DirsAttributes.List[ListIndex]^.Data)
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

function TMultiArchiveCopyOutOperation.SetDirsAttributes(const Paths: TStringHashList): Boolean;
var
  PathIndex: Integer;
  TargetDir: String;
  Header: TArchiveItem;
  Time: TFileTime;
begin
  Result := True;

  for PathIndex := 0 to Paths.Count - 1 do
  begin
    // Get attributes.
    Header := TArchiveItem(Paths.List[PathIndex]^.Data);

    if Assigned(Header) then
    begin
      TargetDir := Paths.List[PathIndex]^.Key;

      try
{$IF DEFINED(MSWINDOWS)}
        // Restore attributes, e.g., hidden, read-only.
        // On Unix attributes value would have to be translated somehow.
        mbFileSetAttr(TargetDir, Header.Attributes);

       // DosToWinTime(TDosFileTime(Header.FileTime), Time);
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
                                                       TargetName: UTF8String;
                                                       ExitStatus: LongInt);
begin
  if ExitStatus <> 0 then
    begin
      ShowError(Format(rsMsgLogError + rsMsgLogExtract,
                       [FMultiArchiveFileSource.ArchiveFileName + PathDelim +
                        SourceName + ' -> ' + TargetName +
                        ' - Exit status: ' + IntToStr(ExitStatus)]), [log_arc_op]);
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
  if FMultiArchiveFileSource.MultiArcItem.FConsoleOutput then
    logWrite(Thread, str, lmtInfo, True, False);
end;

procedure TMultiArchiveCopyOutOperation.UpdateProgress(SourceName,
  TargetName: UTF8String; IncSize: Int64);
begin

end;

end.

