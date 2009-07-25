unit uFileSystemCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCopyOperation,
  uFileSystemFileSource,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uFileSystemFile,
  uDescr;

type
  {
    Both operations are the same, just source and target reversed.
    Implement them in terms of the same functions,
    or have one use the other.
  }

  TFileSystemCopyInOperation = class(TFileSourceCopyInOperation)

  private
    FSourceFileSource: TFileSystemFileSource;
    FTargetFileSource: TFileSystemFileSource;
    FSourceFiles: TFiles;
    FTargetFiles: TFiles;

  public
    constructor Create(SourceFileSource: TFileSystemFileSource;
                       TargetFileSource: TFileSystemFileSource;
                       SourceFiles: TFiles;
                       TargetFiles: TFiles); reintroduce;

    procedure Execute; override;

  end;

  TFileSystemCopyOutOperation = class(TFileSourceCopyOutOperation)

  private
    FBuffer: Pointer;
    FBufferSize: LongWord;
    FSourceFileSource: TFileSystemFileSource;
    FTargetFileSource: TFileSystemFileSource;
    FSourceFiles: TFiles;
    FFullSourceFilesTree: TFileSystemFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    FTargetPath: String;
    FFileMask: String;
    FNameMask, FExtMask: String;
    FDescription: TDescription;

    // Options.
    FCheckFreeSpace: Boolean;
    FSkipAllBigFiles: Boolean;
    FDropReadOnlyFlag: Boolean;
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FFileExistsOption: TFileSourceOperationOptionFileExists;
    FDirExistsOption: TFileSourceOperationOptionDirectoryExists;

  protected
    function ProcessFile(FileSource: TFileSystemFileSource;
                         aFile: TFileSystemFile; TargetPath: String;
                         AbsoluteTargetFileName: String): Boolean;

    // ProcessFileNoQuestions (when we're sure the targets don't exist)

    function CopyFile(const SourceFileName, TargetFileName: String; bAppend: Boolean): Boolean;
    function ShowError(sMessage: String): TFileSourceOperationUIResponse;

  public
    constructor Create(SourceFileSource: TFileSystemFileSource;
                       TargetFileSource: TFileSystemFileSource;
                       SourceFiles: TFiles;
                       TargetPath: String;
                       FileMask: String); reintroduce;

    destructor Destroy; override;

    procedure Initialize; override;
    function  MainExecute: TFileSourceOperationExecuteStepResult; override;
    procedure Finalize; override;
  end;

implementation

uses
  uOSUtils, uDCUtils, uFileProcs, uFileProperty, uLng,
  uFilesystemUtil, strutils, uClassesEx, FileUtil, LCLProc, uGlobs, uLog;

// -- TFileSystemCopyInOperation ----------------------------------------------

constructor TFileSystemCopyInOperation.Create(SourceFileSource: TFileSystemFileSource;
                                              TargetFileSource: TFileSystemFileSource;
                                              SourceFiles: TFiles;
                                              TargetFiles: TFiles);
begin
  inherited Create;

  FSourceFileSource := SourceFileSource;
  FTargetFileSource := TargetFileSource;
  FSourceFiles := SourceFiles;
  FTargetFiles := TargetFiles;
end;

procedure TFileSystemCopyInOperation.Execute;
begin
end;

// -- TFileSystemCopyOutOperation ---------------------------------------------

constructor TFileSystemCopyOutOperation.Create(SourceFileSource: TFileSystemFileSource;
                                               TargetFileSource: TFileSystemFileSource;
                                               SourceFiles: TFiles;
                                               TargetPath: String;
                                               FileMask: String);
begin
  inherited Create;

  FBuffer := nil;
  FSourceFileSource := SourceFileSource;
  FTargetFileSource := TargetFileSource;
  FSourceFiles := SourceFiles;
  FTargetPath := TargetPath;
  FFileMask := FileMask;

  // Here we can read global settings if there are any.
  FSymLinkOption := fsooslNone;
  FFileExistsOption := fsoofeNone;
  FDirExistsOption := fsoodeNone;
  FCheckFreeSpace := True;
  FSkipAllBigFiles := False;
  FDropReadOnlyFlag := False;

  if gProcessComments then
    FDescription := TDescription.Create(True)
  else
    FDescription := nil;
end;

destructor TFileSystemCopyOutOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;

  if Assigned(FDescription) then
  begin
    FDescription.SaveDescription;
    FreeAndNil(FDescription);
  end;
end;

procedure TFileSystemCopyOutOperation.Initialize;
begin
  SplitFileMask(FFileMask, FNameMask, FExtMask);

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FillAndCount(FSourceFiles as TFileSystemFiles,
               FFullSourceFilesTree,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)

  // Create destination path if it doesn't exist.
  if not mbDirectoryExists(FTargetPath) then
    mbForceDirectory(FTargetPath);

  FBufferSize := gCopyBlockSize;
  GetMem(FBuffer, FBufferSize);

  FDescription.Clear;
end;

function TFileSystemCopyOutOperation.MainExecute: TFileSourceOperationExecuteStepResult;
var
  aFile: TFileSystemFile;
  iTotalDiskSize, iFreeDiskSize: Int64;
  bProceed, ProcessedOk: Boolean;
  UIResponse: TFileSourceOperationUIResponse;
  TargetName: String;
  OldDoneBytes: Int64; // for if there was an error
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := 0 to FFullSourceFilesTree.Count - 1 do
  begin
    aFile := FFullSourceFilesTree[CurrentFileIndex] as TFileSystemFile;

    TargetName := GetAbsoluteTargetFileName(aFile,
                                            FSourceFileSource.CurrentPath,
                                            FTargetPath,
                                            FNameMask, FExtMask);

    with FStatistics do
    begin
      CurrentFileFrom := aFile.Path + aFile.Name;
      CurrentFileTo := TargetName;
      CurrentFileTotalBytes := aFile.Size;
      CurrentFileDoneBytes := 0;
    end;

    UpdateStatistics(FStatistics);

    bProceed := True;

    { Check disk free space }
    if FCheckFreeSpace = True then
    begin
      GetDiskFreeSpace(FTargetPath, iFreeDiskSize, iTotalDiskSize);
      if aFile.Size > iFreeDiskSize then
      begin
        if FSkipAllBigFiles = True then
        begin
          bProceed:= False;
        end
        else
        begin
          case AskQuestion('', rsMsgNoFreeSpaceCont,
                           [fsourYes, fsourAll, fsourNo, fsourSkip, fsourSkipAll],
                           fsourYes, fsourNo) of
            fsourNo:
              RaiseAbortOperation;

            fsourSkip:
              bProceed := False;

            fsourAll:
              FCheckFreeSpace := False;

            fsourSkipAll:
              begin
                bProceed := False;
                FSkipAllBigFiles := True;
              end;
          end;
        end;
      end;
    end;

    // If there will be an error in ProcessFile the DoneBytes value
    // will be inconsistent, so remember it here.
    OldDoneBytes := FStatistics.DoneBytes;

    if bProceed then
    begin
      ProcessedOk := ProcessFile(FSourceFileSource, aFile, FTargetPath, TargetName);
    end;

    if (not bProceed) or (not ProcessedOk) then
    begin
      // Correct statistics as we don't know what state they are in after an error.
      with FStatistics do
      begin
        DoneFiles := DoneFiles + 1;
        DoneBytes := OldDoneBytes + aFile.Size;

        EstimateSpeedAndTime(FStatistics);
        UpdateStatistics(FStatistics);

        // Update overall progress.
        if TotalBytes <> 0 then
          UpdateProgress((DoneBytes * 100) div TotalBytes);
      end;
    end;

    CheckOperationState;
  end;

  Result := fsoesrFinished;
end;

procedure TFileSystemCopyOutOperation.Finalize;
begin
end;

function TFileSystemCopyOutOperation.ProcessFile(
             FileSource: TFileSystemFileSource;
             aFile: TFileSystemFile; TargetPath: String;
             AbsoluteTargetFileName: String): Boolean;
var
  sDstName: String;
  bIsFolder,
  bIsSymLink: Boolean;
  iAttr: TFileAttrs;
  sMsg: String;
  SourceFileName: String;
  UIResponse: TFileSourceOperationUIResponse;
  bAppend: Boolean = False;
  logMessage: String;
begin
  // Check if copying to the same file.
  if CompareFilenames(aFile.Path + aFile.Name, AbsoluteTargetFileName) = 0 then
    Exit(False);

  if aFile.IsLink then
    begin
      // use sDstName as link target
      sDstName:= ReadSymLink(aFile.Path + aFile.Name);     // use sLinkTo ?
      if sDstName <> '' then
        begin
          sDstName:= GetAbsoluteFileName(aFile.Path, sDstName);
//          DebugLn('ReadSymLink := ' + sDstName);

          iAttr := mbFileGetAttr(AbsoluteTargetFileName);
          if iAttr <> faInvalidAttributes then // file exists
            begin
              bIsFolder:= FPS_ISDIR(iAttr);
              bIsSymLink:= FPS_ISLNK(iAttr);

              case FFileExistsOption of
                fsoofeSkip:  Exit(False);
                fsoofeNone:
                  begin
                    sMsg := IfThen(bIsFolder and not bIsSymLink, rsMsgFolderExistsRwrt, rsMsgFileExistsRwrt);
                    sMsg := Format(sMsg, [AbsoluteTargetFileName]);

                    case AskQuestion(sMsg, '',
                                     [fsourRewrite, fsourSkip, fsourRewriteAll, fsourSkipAll],
                                     fsourRewrite, fsourSkip) of
                      fsourSkip: Exit(False);
                      fsourRewrite: ; //continue
                      fsourRewriteAll:
                        begin
                          FFileExistsOption := fsoofeOverwrite;
                          //continue
                        end;
                      fsourSkipAll:
                        begin
                          FFileExistsOption := fsoofeSkip;
                          Exit(False);
                        end;
                    end; //case
                  end;
                // else continue
              end;

              if bIsFolder and bIsSymLink then // symlink to folder
                mbRemoveDir(AbsoluteTargetFileName)
              else if bIsFolder then // folder
                DelTree(AbsoluteTargetFileName)
              else // file
                mbDeleteFile(AbsoluteTargetFileName);
            end; // mbFileExists

          if not CreateSymlink(sDstName, AbsoluteTargetFileName) then
            DebugLn('Symlink error');
        end
      else
        DebugLn('Error reading link');
      Result:= True;
    end
  else if aFile.IsDirectory then
    begin
      if not mbDirectoryExists(AbsoluteTargetFileName) then
        mbForceDirectory(AbsoluteTargetFileName);
      // if preserve attrs/times - set them here
      Result:= True;
    end
  else
    begin // files and other stuff
      Result:= False;

      iAttr := mbFileGetAttr(AbsoluteTargetFileName);
      if iAttr <> faInvalidAttributes then // file exists
        begin
          if FPS_ISLNK(iAttr) then
            begin
              case FFileExistsOption of
                fsoofeSkip: Exit(False);
                fsoofeNone:
                  begin
                    sMsg := Format(rsMsgFileExistsRwrt, [AbsoluteTargetFileName]);
                    case AskQuestion(sMsg, '',
                                     [fsourRewrite, fsourSkip, fsourRewriteAll, fsourSkipAll],
                                     fsourRewrite, fsourSkip) of
                      fsourSkip: Exit(False);
                      fsourRewrite: ; //continue
                      fsourRewriteAll:
                        begin
                          FFileExistsOption := fsoofeOverwrite;
                          //continue
                        end;
                      fsourSkipAll:
                        begin
                          FFileExistsOption := fsoofeSkip;
                          Exit(False);
                        end;
                    end; //case
                  end;
              end;

              mbDeleteFile(AbsoluteTargetFileName);
            end // FPS_ISLNK
          else if FPS_ISDIR(iAttr) then
            begin
              // what if directory exists? ask if copy into it?
            end
          else // file
            begin
              case FFileExistsOption of
                fsoofeSkip: Exit(False);
                fsoofeAppend:
                  bAppend := True;
                fsoofeNone:
                  begin
                    sMsg := Format(rsMsgFileExistsRwrt, [AbsoluteTargetFileName]);
                    case AskQuestion(sMsg, '',
                                     [fsourRewrite, fsourSkip, fsourRewriteAll, fsourSkipAll, fsourAppend],
                                     fsourRewrite, fsourSkip) of
                      fsourSkip: Exit(False);
                      fsourRewrite: ; //continue
                      fsourRewriteAll:
                        begin
                          FFileExistsOption := fsoofeOverwrite;
                          //continue
                        end;
                      fsourSkipAll:
                        begin
                          FFileExistsOption := fsoofeSkip;
                          Exit(False);
                        end;
                      fsourAppend:
                        begin
                          //FFileExistsOption := fsoofeAppend; - append all
                          bAppend := True;
                        end;
                    end; //case
                  end;
              end; //case

//            if not bAppend then mbDeleteFile(AbsoluteTargetFileName);
            end;
        end; // file exists

      Result:= Self.CopyFile(aFile.Path + aFile.Name, AbsoluteTargetFileName, bAppend);

      // process comments if need
      if Result and gProcessComments then
        FDescription.CopyDescription(aFile.Path + aFile.Name, AbsoluteTargetFileName);

      if Result = True then
        begin
          // write log success
          if (log_cp_mv_ln in gLogOptions) and (log_success in gLogOptions) then
          begin
            logMessage := Format(rsMsgLogSuccess+rsMsgLogCopy, [aFile.Path + aFile.Name+' -> '+AbsoluteTargetFileName]);
            if Assigned(Thread) then
              logWrite(Thread, logMessage, lmtSuccess)
            else
              logWrite(logMessage, lmtSuccess);
          end;
        end
      else
        begin
          // write log error
          if (log_cp_mv_ln in gLogOptions) and (log_errors in gLogOptions) then
          begin
            logMessage := Format(rsMsgLogError+rsMsgLogCopy, [aFile.Path + aFile.Name+' -> '+AbsoluteTargetFileName]);
            if Assigned(Thread) then
              logWrite(Thread, logMessage, lmtError)
            else
              logWrite(logMessage, lmtError);
          end;
        end;
    end; // files and other stuff
end;

function TFileSystemCopyOutOperation.CopyFile(const SourceFileName, TargetFileName: String; bAppend: Boolean): Boolean;
var
  SourceFile, TargetFile: TFileStreamEx;
  iTotalDiskSize, iFreeDiskSize: Int64;
  bRetry: Boolean;
  BytesRead, BytesToRead: Int64;
  TotalBytesToRead: Int64 = 0;
  logMessage: String;
begin
  Result:= False;
  BytesToRead := FBufferSize;
  SourceFile := nil;
  TargetFile := nil; // for safety exception handling
  try
    try
      SourceFile := TFileStreamEx.Create(SourceFileName, fmOpenRead or fmShareDenyNone);
      if bAppend then
        begin
          TargetFile:= TFileStreamEx.Create(TargetFileName, fmOpenReadWrite);
          TargetFile.Seek(0,soFromEnd); // seek to end
        end
      else
        begin
          TargetFile:= TFileStreamEx.Create(TargetFileName, fmCreate);
        end;

      TotalBytesToRead := SourceFile.Size;

      while TotalBytesToRead > 0 do
      begin
        // Without the following line the reading is very slow
        // if it tries to read past end of file.
        if TotalBytesToRead < BytesToRead then
          BytesToRead := TotalBytesToRead;

        BytesRead := SourceFile.Read(FBuffer^, BytesToRead);

        if (BytesRead = 0) then
          Raise EReadError.Create('read error');

        TotalBytesToRead := TotalBytesToRead - BytesRead;

        repeat
          try
            bRetry:= False;
            TargetFile.WriteBuffer(FBuffer^, BytesRead);
          except
            on EWriteError do
              begin
                { Check disk free space }
                GetDiskFreeSpace(FTargetPath, iFreeDiskSize, iTotalDiskSize);
                if BytesRead > iFreeDiskSize then
                  begin
                    case AskQuestion(rsMsgNoFreeSpaceRetry, '',
                                     [fsourYes, fsourNo, fsourSkip],
                                     fsourYes, fsourNo) of
                      fsourYes:
                        bRetry:= True;
                      fsourNo:
                        RaiseAbortOperation;
                      fsourSkip:
                        Exit;
                    end; // case
                  end;
              end; // on do
          end; // except
        until not bRetry;

        with FStatistics do
        begin
          CurrentFileDoneBytes := CurrentFileDoneBytes + BytesRead;
          DoneBytes := DoneBytes + BytesRead;

          EstimateSpeedAndTime(FStatistics);
          UpdateStatistics(FStatistics);

          if TotalBytes <> 0 then
            UpdateProgress((DoneBytes * 100) div TotalBytes);
        end;

        CheckOperationState; // check pause and stop
      end;//while

    finally
      if assigned(SourceFile) then
        FreeAndNil(SourceFile);
      if assigned(TargetFile) then
      begin
        FreeAndNil(TargetFile);
        if TotalBytesToRead > 0 then
          // There was some error, because not all of the file has been copied.
          // Delete the not completed target file.
          mbDeleteFile(TargetFileName);
      end;
    end;
  // copy file attributes
  Result:= FileCopyAttr(SourceFileName, TargetFileName, FDropReadOnlyFlag);
  //if Preserve_attr
  except
    on EFCreateError do
      begin
        ShowError(rsMsgLogError + rsMsgErrECreate + ' - ' + TargetFileName);
      end;
    on EFOpenError do
      begin
        ShowError(rsMsgLogError + rsMsgErrEOpen + ' - ' + SourceFileName);
      end;
    on EWriteError do
      begin
        ShowError(rsMsgLogError + rsMsgErrEWrite + ' - ' + TargetFileName);
      end;
  end;
end;

function TFileSystemCopyOutOperation.ShowError(sMessage: String): TFileSourceOperationUIResponse;
begin
  if gSkipFileOpError then
  begin
    if Assigned(Thread) then
      logWrite(Thread, sMessage, lmtError, True)
    else
      logWrite(sMessage, lmtError, True);

    Result := fsourSkip;
  end
  else
  begin
    Result := AskQuestion(sMessage, '', [fsourSkip, fsourCancel], fsourSkip, fsourCancel);
    if Result = fsourCancel then
      RaiseAbortOperation;
  end;
end;

end.

