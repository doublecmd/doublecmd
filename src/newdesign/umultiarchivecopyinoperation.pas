unit uMultiArchiveCopyInOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StringHashList, uLog, uGlobs, un_process,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFile,
  uFileSystemFile,
  uMultiArchiveFileSource;

type

  { TMultiArchiveCopyInOperation }

  TMultiArchiveCopyInOperation = class(TFileSourceCopyInOperation)

  private
    FMultiArchiveFileSource: IMultiArchiveFileSource;
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    FFullFilesTree: TFileSystemFiles;

    procedure ShowError(sMessage: String; logOptions: TLogOptions = []);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  protected
    FExProcess: TExProcess;
    FTempFile: UTF8String;
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
  LCLProc, FileUtil, uDCUtils, uMultiArc, uLng, uShowMsg, Process,
  uFileSourceOperationUI, uFileSystemUtil, uMultiArchiveUtil, uOSUtils;

constructor TMultiArchiveCopyInOperation.Create(aSourceFileSource: IFileSource;
                                              aTargetFileSource: IFileSource;
                                              var theSourceFiles: TFiles;
                                              aTargetPath: String);
begin
  FMultiArchiveFileSource := aTargetFileSource as IMultiArchiveFileSource;
  FFullFilesTree := nil;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TMultiArchiveCopyInOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FFullFilesTree) then
    FreeAndNil(FFullFilesTree);
end;

procedure TMultiArchiveCopyInOperation.Initialize;
begin
  FExProcess:= TExProcess.Create(EmptyStr);
  FExProcess.Process.Options:= FExProcess.Process.Options + [poWaitOnExit];
  FTempFile:= GetTempName(GetTempFolder);

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FillAndCount(SourceFiles as TFileSystemFiles, False,
               FFullFilesTree,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)
end;

procedure TMultiArchiveCopyInOperation.MainExecute;
var
  I: Integer;
  sDestPath: String;
  MultiArcItem: TMultiArcItem;
  iResult: Longint;
begin
  MultiArcItem := FMultiArchiveFileSource.MultiArcItem;

  sDestPath := ExcludeFrontPathDelimiter(TargetPath);
  sDestPath := ExcludeTrailingPathDelimiter(sDestPath);
  sDestPath := sDestPath;

  if Pos('%F', MultiArcItem.FAdd) <> 0 then // pack file by file
    for I:=0 to FFullFilesTree.Count - 1 do
    begin
      UpdateProgress(FFullFilesTree[I].FullPath, sDestPath, 0);

      FExProcess.SetCmdLine(FormatArchiverCommand(
                                                  MultiArcItem.FArchiver,
                                                  MultiArcItem.FAdd,
                                                  FMultiArchiveFileSource.ArchiveFileName,
                                                  nil,
                                                  FFullFilesTree[I].FullPath,
                                                  sDestPath,
                                                  FTempFile));
      FExProcess.Execute;

      UpdateProgress(FFullFilesTree[I].FullPath, sDestPath, FFullFilesTree[I].Size);
      // Check for errors.
      iResult:= FExProcess.ExitStatus;
      if iResult <> 0 then
        begin
          ShowError(Format(rsMsgLogError + rsMsgLogPack,
                           [FMultiArchiveFileSource.ArchiveFileName +
                            ' - Exit status: ' + IntToStr(iResult)]), [log_arc_op]);
        end
      else
        begin
          LogMessage(Format(rsMsgLogSuccess + rsMsgLogPack,
                            [FMultiArchiveFileSource.ArchiveFileName]), [log_arc_op], lmtSuccess);
        end;
    end
  else  // pack whole file list
    begin
      FExProcess.SetCmdLine(FormatArchiverCommand(
                                                  MultiArcItem.FArchiver,
                                                  MultiArcItem.FAdd,
                                                  FMultiArchiveFileSource.ArchiveFileName,
                                                  FFullFilesTree,
                                                  EmptyStr,
                                                  sDestPath,
                                                  FTempFile));
      FExProcess.Execute;

      // Check for errors.
      iResult:= FExProcess.ExitStatus;
      if iResult <> 0 then
        begin
          ShowError(Format(rsMsgLogError + rsMsgLogPack,
                           [FMultiArchiveFileSource.ArchiveFileName +
                            ' - Exit status: ' + IntToStr(iResult)]), [log_arc_op]);
        end
      else
        begin
          LogMessage(Format(rsMsgLogSuccess + rsMsgLogPack,
                            [FMultiArchiveFileSource.ArchiveFileName]), [log_arc_op], lmtSuccess);
        end;
    end;
end;

procedure TMultiArchiveCopyInOperation.Finalize;
begin
  FreeThenNil(FExProcess);
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

procedure TMultiArchiveCopyInOperation.UpdateProgress(SourceName,
  TargetName: UTF8String; IncSize: Int64);
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

end.

