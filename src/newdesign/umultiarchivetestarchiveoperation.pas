unit uMultiArchiveTestArchiveOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceTestArchiveOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationUI,
  uFile,
  uMultiArchiveFile,
  uMultiArchiveFileSource,
  uGlobs, uLog, un_process;

type

  { TMultiArchiveTestArchiveOperation }

  TMultiArchiveTestArchiveOperation = class(TFileSourceTestArchiveOperation)

  private
    FMultiArchiveFileSource: IMultiArchiveFileSource;
    FStatistics: TFileSourceTestArchiveOperationStatistics; // local copy of statistics
    FFullFilesTreeToTest: TMultiArchiveFiles;  // source files including all files/dirs in subdirectories

    procedure ShowError(sMessage: String; logOptions: TLogOptions);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
    procedure CheckForErrors(const FileName: UTF8String; ExitStatus: LongInt);

  protected
    FExProcess: TExProcess;
    FTempFile: UTF8String;
    FErrorLevel: LongInt;
    procedure OnReadLn(str: string);
    procedure UpdateProgress(SourceName: UTF8String; IncSize: Int64);


  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToDelete: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

  end;

implementation

uses
  uOSUtils, uDCUtils, uLng, uShowMsg, uMultiArc, uMultiArchiveUtil,
  Masks, FileUtil, LCLProc, Process;

constructor TMultiArchiveTestArchiveOperation.Create(aTargetFileSource: IFileSource;
                                              var theFilesToDelete: TFiles);
begin
  FMultiArchiveFileSource := aTargetFileSource as IMultiArchiveFileSource;
  FFullFilesTreeToTest:= nil;

  inherited Create(aTargetFileSource, theFilesToDelete);
end;

destructor TMultiArchiveTestArchiveOperation.Destroy;
begin
  FreeThenNil(FFullFilesTreeToTest);
  inherited Destroy;
end;

procedure TMultiArchiveTestArchiveOperation.Initialize;
begin
  FExProcess:= TExProcess.Create(EmptyStr);
  FExProcess.OnReadLn:= @OnReadLn;
  FTempFile:= GetTempName(GetTempFolder);

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
  FStatistics.ArchiveFile:= FMultiArchiveFileSource.ArchiveFileName;

  with FMultiArchiveFileSource do
  FillAndCount('*.*', SourceFiles as TMultiArchiveFiles,
               True,
               FFullFilesTreeToTest,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)
end;

procedure TMultiArchiveTestArchiveOperation.MainExecute;
var
  I: Integer;
  MultiArcItem: TMultiArcItem;
  aFile: TMultiArchiveFile;
  sReadyCommand,
  sCommandLine: UTF8String;
begin
  MultiArcItem := FMultiArchiveFileSource.MultiArcItem;
  sCommandLine:= MultiArcItem.FTest;
  // Get maximum acceptable command errorlevel
  FErrorLevel:= ExtractErrorLevel(sCommandLine);
  if Pos('%F', sCommandLine) <> 0 then // test file by file
    for I:=0 to FFullFilesTreeToTest.Count - 1 do
    begin
      aFile:= FFullFilesTreeToTest[I] as TMultiArchiveFile;
      UpdateProgress(aFile.FullPath, 0);

      sReadyCommand:= FormatArchiverCommand(
                                            MultiArcItem.FArchiver,
                                            sCommandLine,
                                            FMultiArchiveFileSource.ArchiveFileName,
                                            nil,
                                            aFile.FullPath
                                            );
      OnReadLn(sReadyCommand);

      FExProcess.SetCmdLine(sReadyCommand);
      FExProcess.Execute;

      UpdateProgress(aFile.FullPath, aFile.Size);
      // Check for errors.
      CheckForErrors(aFile.FullPath , FExProcess.ExitStatus);
    end
  else  // test whole file list
    begin
      sReadyCommand:= FormatArchiverCommand(
                                            MultiArcItem.FArchiver,
                                            sCommandLine,
                                            FMultiArchiveFileSource.ArchiveFileName,
                                            FFullFilesTreeToTest,
                                            EmptyStr,
                                            EmptyStr,
                                            FTempFile
                                            );
      OnReadLn(sReadyCommand);

      FExProcess.SetCmdLine(sReadyCommand);
      FExProcess.Execute;

      // Check for errors.
      CheckForErrors(FMultiArchiveFileSource.ArchiveFileName, FExProcess.ExitStatus);
    end;
end;

procedure TMultiArchiveTestArchiveOperation.Finalize;
begin
  FreeThenNil(FExProcess);
  with FMultiArchiveFileSource.MultiArcItem do
  if not FDebug then
    mbDeleteFile(FTempFile);
end;

procedure TMultiArchiveTestArchiveOperation.ShowError(sMessage: String; logOptions: TLogOptions);
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

procedure TMultiArchiveTestArchiveOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

procedure TMultiArchiveTestArchiveOperation.OnReadLn(str: string);
begin
  with FMultiArchiveFileSource.MultiArcItem do
  if FOutput or FDebug then
    logWrite(Thread, str, lmtInfo, True, False);
end;

procedure TMultiArchiveTestArchiveOperation.CheckForErrors(const FileName: UTF8String; ExitStatus: LongInt);
begin
  if ExitStatus > FErrorLevel then
    begin
      ShowError(Format(rsMsgLogError + rsMsgLogDelete,
                 [FileName +
                  ' - Exit status: ' + IntToStr(ExitStatus)]), [log_arc_op]);
    end
  else
    begin
      LogMessage(Format(rsMsgLogSuccess + rsMsgLogDelete,
                  [FileName]), [log_arc_op], lmtSuccess);
    end;
end;

procedure TMultiArchiveTestArchiveOperation.UpdateProgress(SourceName: UTF8String; IncSize: Int64);
begin
  with FStatistics do
  begin
    FStatistics.CurrentFile:= SourceName;

    DoneBytes := DoneBytes + IncSize;

    UpdateStatistics(FStatistics);
  end;
end;

end.
