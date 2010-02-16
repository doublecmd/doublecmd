unit uMultiArchiveDeleteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceDeleteOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationUI,
  uFile,
  uMultiArchiveFileSource,
  uGlobs, uLog, un_process;

type

  { TMultiArchiveDeleteOperation }

  TMultiArchiveDeleteOperation = class(TFileSourceDeleteOperation)

  private
    FMultiArchiveFileSource: IMultiArchiveFileSource;
    FStatistics: TFileSourceDeleteOperationStatistics; // local copy of statistics

    procedure CountFiles(const theFiles: TFiles; FileMask: String);
    procedure CheckForErrors(const FileName: UTF8String; ExitStatus: LongInt);

  protected
    FExProcess: TExProcess;
    FTempFile: UTF8String;
    procedure UpdateProgress(SourceName: UTF8String; IncSize: Int64);
    procedure ShowError(sMessage: String; logOptions: TLogOptions);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

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
  uOSUtils, uDCUtils, uLng, uShowMsg, uMultiArc, uMultiArchiveUtil, uMultiArchiveFile,
  Masks, FileUtil, LCLProc, Process;

constructor TMultiArchiveDeleteOperation.Create(aTargetFileSource: IFileSource;
                                              var theFilesToDelete: TFiles);
begin
  FMultiArchiveFileSource := aTargetFileSource as IMultiArchiveFileSource;

  inherited Create(aTargetFileSource, theFilesToDelete);
end;

destructor TMultiArchiveDeleteOperation.Destroy;
begin

  inherited Destroy;
end;

procedure TMultiArchiveDeleteOperation.Initialize;
begin
  FExProcess:= TExProcess.Create(EmptyStr);
  FExProcess.Process.Options:= FExProcess.Process.Options + [poWaitOnExit];
  FTempFile:= GetTempName(GetTempFolder);

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  CountFiles(FilesToDelete, '*.*');
end;

procedure TMultiArchiveDeleteOperation.MainExecute;
var
  I: Integer;
  MultiArcItem: TMultiArcItem;
  aFile: TMultiArchiveFile;
begin
  MultiArcItem := FMultiArchiveFileSource.MultiArcItem;

  if Pos('%F', MultiArcItem.FDelete) <> 0 then // delete file by file
    for I:=0 to FilesToDelete.Count - 1 do
    begin
      aFile:= FilesToDelete[I] as TMultiArchiveFile;
      UpdateProgress(aFile.FullPath, 0);

      FExProcess.SetCmdLine(FormatArchiverCommand(
                                                  MultiArcItem.FArchiver,
                                                  MultiArcItem.FDelete,
                                                  FMultiArchiveFileSource.ArchiveFileName,
                                                  nil,
                                                  aFile.FullPath
                                                  ));
      FExProcess.Execute;

      UpdateProgress(aFile.FullPath, aFile.Size);
      // Check for errors.
      CheckForErrors(aFile.FullPath , FExProcess.ExitStatus);
    end
  else  // delete whole file list
    begin
      FExProcess.SetCmdLine(FormatArchiverCommand(
                                                  MultiArcItem.FArchiver,
                                                  MultiArcItem.FDelete,
                                                  FMultiArchiveFileSource.ArchiveFileName,
                                                  FilesToDelete,
                                                  EmptyStr,
                                                  EmptyStr,
                                                  FTempFile
                                                  ));
      FExProcess.Execute;

      // Check for errors.
      CheckForErrors(FMultiArchiveFileSource.ArchiveFileName, FExProcess.ExitStatus);
    end;
end;

procedure TMultiArchiveDeleteOperation.Finalize;
begin
  FreeThenNil(FExProcess);
  mbDeleteFile(FTempFile);
end;

procedure TMultiArchiveDeleteOperation.ShowError(sMessage: String; logOptions: TLogOptions);
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

procedure TMultiArchiveDeleteOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

procedure TMultiArchiveDeleteOperation.CountFiles(const theFiles: TFiles; FileMask: String);
var
  I: Integer;
  ArchiveItem: TArchiveItem;
  ArcFileList: TList;
begin
  ArcFileList := FMultiArchiveFileSource.ArchiveFileList;
  for i := 0 to ArcFileList.Count - 1 do
  begin
    ArchiveItem := TArchiveItem(ArcFileList.Items[I]);

    // Check if the file from the archive fits the selection given via theFiles.
    if  (not FPS_ISDIR(ArchiveItem.Attributes))           // Omit directories
    and MatchesFileList(theFiles, ArchiveItem.FileName) // Check if it's included in the filelist
    and ((FileMask = '*.*') or (FileMask = '*')    // And name matches file mask
        or MatchesMaskList(ExtractFileName(ArchiveItem.FileName), FileMask))
    then
    begin
      Inc(FStatistics.TotalBytes, ArchiveItem.UnpSize);
      Inc(FStatistics.TotalFiles, 1);
    end;
  end;

  UpdateStatistics(FStatistics);
end;

procedure TMultiArchiveDeleteOperation.CheckForErrors(const FileName: UTF8String; ExitStatus: LongInt);
begin
  if ExitStatus <> 0 then
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

procedure TMultiArchiveDeleteOperation.UpdateProgress(SourceName: UTF8String; IncSize: Int64);
begin
  with FStatistics do
  begin
    FStatistics.CurrentFile:= SourceName;

    DoneBytes := DoneBytes + IncSize;

    UpdateStatistics(FStatistics);
  end;
end;

end.
