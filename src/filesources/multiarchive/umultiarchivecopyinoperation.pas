unit uMultiArchiveCopyInOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLog, uGlobs, un_process,
  uFileSourceOperation,
  uFileSourceCopyOperation,
  uFileSource,
  uFile,
  uArchiveCopyOperation,
  uMultiArchiveFileSource,
  uTarWriter;

type

  { TMultiArchiveCopyInOperation }

  TMultiArchiveCopyInOperation = class(TArchiveCopyInOperation)

  private
    FMultiArchiveFileSource: IMultiArchiveFileSource;
    FTarWriter: TTarWriter;
    FPassword: String;
    FVolumeSize: String;
    FCustomParams: String;

    procedure ShowError(sMessage: String; logOptions: TLogOptions = []);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
    function CheckForErrors(const FileName: String; ExitStatus: LongInt): Boolean;
    procedure DeleteFile(const BasePath: String; aFile: TFile);
    procedure DeleteFiles(const BasePath: String; aFiles: TFiles);

    function doMultiPackFiles(const files: TFiles): Integer;
    function doTarFiles(const files: TFiles): Integer;
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
  LazUTF8, FileUtil, DCStrUtils, uDCUtils, uMultiArc, uLng, WcxPlugin, uFileSourceOperationUI,
  uFileSystemFileSource, uFileSystemUtil, uMultiArchiveUtil, DCOSUtils, uOSUtils,
  uShowMsg, uAdministrator,
  uArchiveFileSourceUtil;

constructor TMultiArchiveCopyInOperation.Create(aSourceFileSource: IFileSource;
                                              aTargetFileSource: IFileSource;
                                              var theSourceFiles: TFiles;
                                              aTargetPath: String);
begin
  FMultiArchiveFileSource := aTargetFileSource as IMultiArchiveFileSource;
  FPassword:= FMultiArchiveFileSource.Password;
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

  ElevateAction:= dupError;
end;

function TMultiArchiveCopyInOperation.doMultiPackFiles(
  const files: TFiles): Integer;
var
  currentFullFiles: TFiles = nil;
  currentFiles: TFiles;
  currentPath: String;
  aFile: TFile;
  I: Integer;
  oneByOne: Boolean;
  sRootPath: String;
  sDestPath: String;
  sReadyCommand: String;
  uselessTotalFiles: Int64;
  uselessTotalBytes: Int64;
begin
  Result:= -1;

  oneByOne:= Pos('%F', FCommandLine) <> 0;  // pack file by file
  if oneByOne then
    FExProcess.OnOperationProgress:= nil;

  try
    if Assigned(FFullFilesTree) then begin
      currentFullFiles:= FFullFilesTree;
    end else begin
      uFileSystemUtil.FillAndCount(
        files, False, False,
        currentFullFiles,
        uselessTotalFiles,
        uselessTotalBytes);     // gets full list of files (recursive)
    end;

    sDestPath:= ExcludeFrontPathDelimiter(TargetPath);
    sDestPath:= ExcludeTrailingPathDelimiter(sDestPath);
    sRootPath:= currentFullFiles.Path;

    ChangeFileListRoot(EmptyStr, currentFullFiles);

    for I:= currentFullFiles.Count - 1 downto 0 do begin
      if oneByOne then begin
        aFile:= currentFullFiles[I];
        UpdateProgress(sRootPath + aFile.FullPath, FMultiArchiveFileSource.ArchiveFileName, 0);
        currentFiles:= nil;
        currentPath:= aFile.FullPath;
      end else begin
        currentFiles:= currentFullFiles;
        currentPath:= EmptyStr;
      end;

      sReadyCommand:= FormatArchiverCommand(
                                            FMultiArchiveFileSource.MultiArcItem.FArchiver,
                                            FCommandLine,
                                            FMultiArchiveFileSource.ArchiveFileName,
                                            currentFiles,
                                            currentPath,
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

      Result:= FExProcess.ExitStatus;
      if Result <> 0 then
        Exit;

      if NOT oneByOne then
        break;

      UpdateProgress(sRootPath + aFile.FullPath, FMultiArchiveFileSource.ArchiveFileName, aFile.Size);
    end
  finally
    if currentFullFiles <> FFullFilesTree then
      currentFullFiles.Free;
  end;
end;

procedure TMultiArchiveCopyInOperation.MainExecute;

  procedure tarAndPack;
  begin
    // Put to TAR archive if needed
    if FTarBefore then begin
      if NOT self.Tar() then
        Exit;
      UpdateProgress( SourceFiles[0].FullPath, FMultiArchiveFileSource.ArchiveFileName, 0);
    end;

    ProcessFilesWithMultiRootPath( self.SourceFiles, @self.doMultiPackFiles );
  end;

var
  removeFiles: TFiles = nil;
begin
  // 1. calc statistics
  uFileSystemUtil.FillAndCount(
    SourceFiles, False, False,
    FFullFilesTree,
    FStatistics.TotalFiles,
    FStatistics.TotalBytes);     // gets full list of files (recursive)

  // 2. if MultiRootPath, free FFullFilesTree, only retain statistics
  if SourceFiles.Path = EmptyStr then begin
    // in this case, FFullFilesTree is not useful, we will need to expand path by path
    FreeAndNil( FFullFilesTree );
    // sorting allows files from the same path to be grouped together,
    // enabling the processing of more files at once.
    SourceFiles.sort;
  end;

  // Get maximum acceptable command errorlevel
  FErrorLevel:= ExtractErrorLevel(FCommandLine);

  if (PackingFlags and PK_PACK_MOVE_FILES) <> 0 then
    removeFiles:= SourceFiles.Clone;

  try
    tarAndPack;
  finally
    try
      // Delete temporary TAR archive if needed
      if FTarBefore then
        mbDeleteFile(FTarFileName);

      if CheckForErrors(FMultiArchiveFileSource.ArchiveFileName, FExProcess.ExitStatus) then begin
        // if success, delete files need to be removed
        if Assigned(removeFiles) then
          DeleteFiles(EmptyStr, removeFiles);
      end else begin
        // if fail, delete Archive File
        mbDeleteFile(FMultiArchiveFileSource.ArchiveFileName);
      end;
    finally
      FreeAndNil(FFullFilesTree);
      removeFiles.Free;
    end;
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
  if ExitStatus > FErrorLevel then begin
    Result:= False;
    ShowError(Format(rsMsgLogError + rsMsgLogPack,
                     [FileName +
                      ' - ' + rsMsgExitStatusCode + ' ' + IntToStr(ExitStatus)]), [log_arc_op]);
  end else if ExitStatus < 0 then begin
    Result:= False;
  end else begin
    Result:= True;
    LogMessage(Format(rsMsgLogSuccess + rsMsgLogPack,
                      [FileName]), [log_arc_op], lmtSuccess);
  end;
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
      DeleteDirectory(BasePath + aFile.FullPath, False)
    else
      mbDeleteFile(BasePath + aFile.FullPath);
  end;
end;

function TMultiArchiveCopyInOperation.doTarFiles(const files: TFiles): Integer;
var
  success: Boolean;
  currentFullFiles: TFiles = nil;
  uselessTotalFiles: Int64;
  uselessTotalBytes: Int64;
begin
  Result:= -1;
  try
    if Assigned(FFullFilesTree) then begin
      success:= FTarWriter.TarFiles(FFullFilesTree, FStatistics);
    end else begin
      uArchiveFileSourceUtil.FillAndCount(files,
                   currentFullFiles,
                   uselessTotalFiles,
                   uselessTotalBytes);    // gets full list of files (recursive)
      success:= FTarWriter.TarFiles(currentFullFiles, FStatistics);
    end;
    if success then
      Result:= 0;
  finally
    currentFullFiles.Free;
  end;
end;

function TMultiArchiveCopyInOperation.Tar: Boolean;

  function tarFiles: Boolean;
  var
    tarBeginResult: Boolean;
    resultCode: Integer;
  begin
    Result:= False;
    tarBeginResult:= FTarWriter.TarBegin( FStatistics );
    if tarBeginResult then begin
      resultCode:= -1;
      try
        resultCode:= ProcessFilesWithMultiRootPath( SourceFiles, @self.doTarFiles );
      finally
        Result:= FTarWriter.TarEnd( FStatistics, resultCode=0 );
      end;
    end;
  end;

begin
  Result:= False;
  FTarFileName:= RemoveFileExt(FMultiArchiveFileSource.ArchiveFileName);
  FTarWriter:= TTarWriter.Create(FTarFileName,
                                @AskQuestion,
                                @RaiseAbortOperation,
                                @CheckOperationState,
                                @UpdateStatistics
                               );

  try
    if tarFiles() then
    begin
      // Fill file list with tar archive file
      SourceFiles.Clear;
      SourceFiles.Path:= ExtractFilePath(FTarFileName);
      SourceFiles.Add(TFileSystemFileSource.CreateFileFromFile(FTarFileName));
      // SourceFiles changed, FFullFilesTree becomes meaningless
      FreeAndNil(FFullFilesTree);

      Result:= True;
    end;
  finally
    FreeAndNil(FTarWriter);
  end;
end;

procedure TMultiArchiveCopyInOperation.OnReadLn(str: string);
begin
  with FMultiArchiveFileSource.MultiArcItem do
  if FOutput or FDebug then
    logWrite(Thread, str, lmtInfo, True, False);
end;

{
  detecting changes to ArchiveFileName is not always effective,
  it depends on the specific Archiver.

  for example, 7z may saves the data to a temporary file first during compression,
  and then replaces it with ArchiveFileName after successful compression.

  additionally, in oneByOne mode, UpdateProgress() is called for each file to
  update doneBytes, therefore OperationProgressHandler() should not be enabled.
}
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

