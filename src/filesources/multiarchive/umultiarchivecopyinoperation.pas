unit uMultiArchiveCopyInOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLog, uGlobs, un_process,
  uFileSourceOperation,
  uFileSourceCopyOperation,
  uFileSource,
  uFile,
  uArchiveCopyInOperation,
  uMultiArchiveFileSource;

type

  { TMultiArchiveCopyInOperation }

  TMultiArchiveCopyInOperation = class(TArchiveCopyInOperation)

  private
    FMultiArchiveFileSource: IMultiArchiveFileSource;
    FRemoveFilesTree: TFiles;
    FPassword: String;
    FVolumeSize: String;
    FCustomParams: String;
    FCallResult: Boolean;

    procedure ShowError(sMessage: String; logOptions: TLogOptions = []);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
    function CheckForErrors(const FileName: String; ExitStatus: LongInt): Boolean;
    procedure DeleteFile(const BasePath: String; aFile: TFile);
    procedure DeleteFiles(const BasePath: String; aFiles: TFiles);

  protected
    FExProcess: TExProcess;
    FTempFile: String;
    FErrorLevel: LongInt;
    FCommandLine: String;
    function Tar: Boolean;
    procedure OnReadLn(str: string);
    procedure OperationProgressHandler;
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

    property PackingFlags: Integer read FPackingFlags write FPackingFlags;
    property Password: String read FPassword write FPassword;
    property VolumeSize: String read FVolumeSize write FVolumeSize;
    property CustomParams: String read FCustomParams write FCustomParams;
    property TarBefore: Boolean read FTarBefore write FTarBefore;
  end;

implementation

uses
  LazUTF8, DCStrUtils, uDCUtils, uMultiArc, uLng, WcxPlugin, uFileSourceOperationUI,
  uFileSystemFileSource, uFileSystemUtil, uMultiArchiveUtil, DCOSUtils, uOSUtils,
  uTarWriter, uShowMsg;

constructor TMultiArchiveCopyInOperation.Create(aSourceFileSource: IFileSource;
                                              aTargetFileSource: IFileSource;
                                              var theSourceFiles: TFiles;
                                              aTargetPath: String);
begin
  FMultiArchiveFileSource := aTargetFileSource as IMultiArchiveFileSource;
  FPassword:= FMultiArchiveFileSource.Password;
  FFullFilesTree := nil;
  FRemoveFilesTree := nil;
  FPackingFlags := 0;
  FVolumeSize := EmptyStr;
  FTarBefore:= False;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
  with FStatistics do
  begin
    DoneFiles := -1;
    CurrentFileDoneBytes := -1;
    UpdateStatistics(FStatistics);
  end;
end;

destructor TMultiArchiveCopyInOperation.Destroy;
begin
  inherited Destroy;

  FreeAndNil(FFullFilesTree);
  FreeAndNil(FRemoveFilesTree);
end;

procedure TMultiArchiveCopyInOperation.Initialize;
begin
  with FMultiArchiveFileSource do
  begin
    if (ExtractFileExt(ArchiveFileName) = GetSfxExt) and (Length(MultiArcItem.FAddSelfExtract) <> 0) then
      FCommandLine:= MultiArcItem.FAddSelfExtract
    else
      FCommandLine:= MultiArcItem.FAdd;
  end;

  if (TargetPath <> PathDelim) and (Pos('%R', FCommandLine) = 0) then
  begin
    AskQuestion('', rsMsgErrNotSupported, [fsourOk], fsourOk, fsourOk);
    RaiseAbortOperation;
  end;

  FExProcess:= TExProcess.Create(EmptyStr);
  FExProcess.OnReadLn:= @OnReadLn;
  FExProcess.OnOperationProgress:= @OperationProgressHandler;
  FTempFile:= GetTempName(GetTempFolder);

  with FMultiArchiveFileSource.MultiArcItem do
  if Length(FPasswordQuery) <> 0 then
  begin
    FExProcess.QueryString:= UTF8ToConsole(FPasswordQuery);
    FExProcess.OnQueryString:= @OnQueryString;
  end;

  AddStateChangedListener([fsosStarting, fsosPausing, fsosStopping], @FileSourceOperationStateChangedNotify);

  with FStatistics do
  begin
    if SourceFiles.Count = 1 then
      CurrentFileFrom:= SourceFiles[0].FullPath
    else begin
      CurrentFileFrom:= SourceFiles.Path + AllFilesMask;
    end;
    CurrentFileTo:= FMultiArchiveFileSource.ArchiveFileName;
  end;

  FillAndCount(SourceFiles, False, False,
               FFullFilesTree,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)
end;

procedure TMultiArchiveCopyInOperation.MainExecute;
var
  I: Integer;
  sRootPath,
  sDestPath: String;
  MultiArcItem: TMultiArcItem;
  aFile: TFile;
  sReadyCommand: String;
begin
  // Put to TAR archive if needed
  if FTarBefore then Tar;

  MultiArcItem := FMultiArchiveFileSource.MultiArcItem;

  sDestPath := ExcludeFrontPathDelimiter(TargetPath);
  sDestPath := ExcludeTrailingPathDelimiter(sDestPath);
  sRootPath:= FFullFilesTree.Path;
  ChangeFileListRoot(EmptyStr, FFullFilesTree);
  // Get maximum acceptable command errorlevel
  FErrorLevel:= ExtractErrorLevel(FCommandLine);
  if Pos('%F', FCommandLine) <> 0 then // pack file by file
    for I:= FFullFilesTree.Count - 1 downto 0 do
    begin
      aFile:= FFullFilesTree[I];
      UpdateProgress(sRootPath + aFile.FullPath, sDestPath, 0);

      sReadyCommand:= FormatArchiverCommand(
                                            MultiArcItem.FArchiver,
                                            FCommandLine,
                                            FMultiArchiveFileSource.ArchiveFileName,
                                            nil,
                                            aFile.FullPath,
                                            sDestPath,
                                            FTempFile,
                                            Password,
                                            VolumeSize,
                                            CustomParams
                                            );
      OnReadLn(sReadyCommand);

      // Set archiver current path to file list root
      FExProcess.Process.CurrentDirectory:= sRootPath;
      FExProcess.SetCmdLine(sReadyCommand);
      FExProcess.Execute;

      UpdateProgress(sRootPath + aFile.FullPath, sDestPath, aFile.Size);
      // Check for errors.
      if CheckForErrors(sRootPath + aFile.FullPath, FExProcess.ExitStatus) then
        begin
          if (PackingFlags and PK_PACK_MOVE_FILES) <> 0 then
            DeleteFile(sRootPath, aFile);
        end;
    end
  else  // pack whole file list
    begin
      sReadyCommand:= FormatArchiverCommand(
                                            MultiArcItem.FArchiver,
                                            FCommandLine,
                                            FMultiArchiveFileSource.ArchiveFileName,
                                            FFullFilesTree,
                                            EmptyStr,
                                            sDestPath,
                                            FTempFile,
                                            Password,
                                            VolumeSize,
                                            CustomParams
                                            );
      OnReadLn(sReadyCommand);

      // Set archiver current path to file list root
      FExProcess.Process.CurrentDirectory:= sRootPath;
      FExProcess.SetCmdLine(sReadyCommand);
      FExProcess.Execute;

      // Check for errors.
      if CheckForErrors(FMultiArchiveFileSource.ArchiveFileName, FExProcess.ExitStatus) then
        begin
          if (PackingFlags and PK_PACK_MOVE_FILES) <> 0 then
            DeleteFiles(sRootPath, FFullFilesTree);
        end;
    end;

  // Delete temporary TAR archive if needed
  if FTarBefore then
  begin
    mbDeleteFile(FTarFileName);
    if FCallResult and (PackingFlags and PK_PACK_MOVE_FILES <> 0) then
      DeleteFiles(EmptyStr, FRemoveFilesTree);
  end;
end;

procedure TMultiArchiveCopyInOperation.Finalize;
begin
  FreeAndNil(FExProcess);
  with FMultiArchiveFileSource.MultiArcItem do
  if not FDebug then
    mbDeleteFile(FTempFile);
end;

procedure TMultiArchiveCopyInOperation.ShowError(sMessage: String; logOptions: TLogOptions);
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

procedure TMultiArchiveCopyInOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

function TMultiArchiveCopyInOperation.CheckForErrors(const FileName: String; ExitStatus: LongInt): Boolean;
begin
  if ExitStatus > FErrorLevel then
    begin
      Result:= False;
      ShowError(Format(rsMsgLogError + rsMsgLogPack,
                       [FileName +
                        ' - Exit status: ' + IntToStr(ExitStatus)]), [log_arc_op]);
    end
  else
    begin
      Result:= True;
      LogMessage(Format(rsMsgLogSuccess + rsMsgLogPack,
                        [FileName]), [log_arc_op], lmtSuccess);
    end;
  FCallResult:= Result;
end;

procedure TMultiArchiveCopyInOperation.DeleteFile(const BasePath: String; aFile: TFile);
begin
  if aFile.IsDirectory then
    mbRemoveDir(BasePath + aFile.FullPath)
  else
    mbDeleteFile(BasePath + aFile.FullPath);
end;

procedure TMultiArchiveCopyInOperation.DeleteFiles(const BasePath: String; aFiles: TFiles);
var
  I: Integer;
  aFile: TFile;
begin
  for I:= aFiles.Count - 1 downto 0 do
  begin
    aFile:= aFiles[I];
    if aFile.IsDirectory then
      mbRemoveDir(BasePath + aFile.FullPath)
    else
      mbDeleteFile(BasePath + aFile.FullPath);
  end;
end;

function TMultiArchiveCopyInOperation.Tar: Boolean;
var
  TarWriter: TTarWriter = nil;
begin
  Result:= False;
  FTarFileName:= RemoveFileExt(FMultiArchiveFileSource.ArchiveFileName);
  TarWriter:= TTarWriter.Create(FTarFileName,
                                @AskQuestion,
                                @RaiseAbortOperation,
                                @CheckOperationState,
                                @UpdateStatistics
                               );
  try
    if TarWriter.ProcessTree(FFullFilesTree, FStatistics) then
    begin
      // Fill file list with tar archive file
      FRemoveFilesTree:= FFullFilesTree;
      FFullFilesTree:= TFiles.Create(ExtractFilePath(FTarFileName));
      FFullFilesTree.Add(TFileSystemFileSource.CreateFileFromFile(FTarFileName));
      Result:= True;
    end;
  finally
    FreeAndNil(TarWriter);
  end;
end;

procedure TMultiArchiveCopyInOperation.OnReadLn(str: string);
begin
  with FMultiArchiveFileSource.MultiArcItem do
  if FOutput or FDebug then
    logWrite(Thread, str, lmtInfo, True, False);
end;

procedure TMultiArchiveCopyInOperation.OperationProgressHandler;
var
  ArchiveSize: Int64;
begin
  Self.CheckOperationState;
  with FStatistics do
  begin
    ArchiveSize := mbFileSize(FMultiArchiveFileSource.ArchiveFileName);
    if ArchiveSize > DoneBytes then
      DoneBytes := ArchiveSize;

    UpdateStatistics(FStatistics);
  end;
end;

procedure TMultiArchiveCopyInOperation.OnQueryString(str: string);
var
  pcPassword: PAnsiChar;
begin
  ShowInputQuery(FMultiArchiveFileSource.MultiArcItem.FDescription, rsMsgPasswordEnter, True, FPassword);
  pcPassword:= PAnsiChar(UTF8ToConsole(FPassword + LineEnding));
  FExProcess.Process.Input.Write(pcPassword^, Length(pcPassword));
end;

procedure TMultiArchiveCopyInOperation.UpdateProgress(SourceName,
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

procedure TMultiArchiveCopyInOperation.FileSourceOperationStateChangedNotify(
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

end.

