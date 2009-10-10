unit uWcxArchiveCopyInOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StringHashList, uLog, uGlobs,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFile,
  uFileSystemFile,
  uWcxArchiveFileSource;

type
  TWcxArchiveCopyInOperation = class(TFileSourceCopyInOperation)

  private
    FWcxArchiveFileSource: TWcxArchiveFileSource;
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    FFullFilesTree: TFileSystemFiles;

    {en
      Convert TFiles into a string separated with #0 (format used by WCX).
    }
    function GetFileList(const theFiles: TFiles): String;
    procedure ShowError(sMessage: String; logOptions: TLogOptions = []);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  protected

  public
    constructor Create(var aSourceFileSource: TFileSource;
                       var aTargetFileSource: TFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  LCLProc, FileUtil, uDCUtils, uWCXhead, uWCXmodule, uLng,
  uFileSourceOperationUI, uFileSystemUtil;

// ----------------------------------------------------------------------------
// WCX callbacks

var
  // WCX interface cannot discern different operations (for reporting progress),
  // so this global variable is used to store currently running operation.
  // (There may be other running concurrently, but only one may report progress.)
  WcxCopyInOperation: TWcxArchiveCopyInOperation; // used in ProcessDataProc

function ChangeVolProc(ArcName : Pchar; Mode:Longint):Longint; stdcall;
begin
{ // Use operation UI for this.

  case Mode of
    PK_VOL_ASK:
      ArcName := PChar(UTF8ToSys(Dialogs.InputBox('Double Commander', rsMsgSelLocNextVol, SysToUTF8(ArcName))));
    PK_VOL_NOTIFY:
      ShowMessage(rsMsgNextVolUnpack);
  end;
}
  Result := 0;
end;

function ProcessDataProc(FileName: PChar; Size: Integer): Integer; stdcall;
begin
  //DebugLn('Working ' + FileName + ' Size = ' + IntToStr(Size));

  Result := 1;

  if Assigned(WcxCopyInOperation) then
  begin
    if WcxCopyInOperation.State = fsosStopping then  // Cancel operation
      Exit(0);

    with WcxCopyInOperation.FStatistics do
    begin
      if Size >= 0 then
      begin
        CurrentFileDoneBytes := CurrentFileDoneBytes + Size;
        DoneBytes := DoneBytes + Size;
      end
      else // For plugins which unpack in CloseArchive
      begin
        if (Size >= -100) and (Size <= -1) then // first percent bar
          begin
            CurrentFileDoneBytes := CurrentFileTotalBytes * (-Size) div 100;
            CurrentFileTotalBytes := 100;

{            if Size = -100 then // File finished
              DoneBytes := DoneBytes + WcxCopyOutOperation.FCurrentFileSize;}
            //DebugLn('Working ' + FileName + ' Percent1 = ' + IntToStr(FFileOpDlg.iProgress1Pos));
          end
        else if (Size >= -1100) and (Size <= -1000) then // second percent bar
          begin
            DoneBytes := TotalBytes * Int64(-Size - 1000) div 100;
            //DebugLn('Working ' + FileName + ' Percent2 = ' + IntToStr(FFileOpDlg.iProgress2Pos));
          end
        else
          begin
//            DoneBytes := DoneBytes + WcxCopyOutOperation.FCurrentFileSize;
          end;
      end;

      WcxCopyInOperation.UpdateStatistics(WcxCopyInOperation.FStatistics);
    end;
  end;
end;

// ----------------------------------------------------------------------------

constructor TWcxArchiveCopyInOperation.Create(var aSourceFileSource: TFileSource;
                                              var aTargetFileSource: TFileSource;
                                              var theSourceFiles: TFiles;
                                              aTargetPath: String);
begin
  FWcxArchiveFileSource := aTargetFileSource as TWcxArchiveFileSource;
  FFullFilesTree := nil;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TWcxArchiveCopyInOperation.Destroy;
begin
  WcxCopyInOperation := nil; // clear global variable pointing to self

  inherited Destroy;

  if Assigned(FFullFilesTree) then
    FreeAndNil(FFullFilesTree);
end;

procedure TWcxArchiveCopyInOperation.Initialize;
begin
  if Assigned(WcxCopyInOperation) then
    raise Exception.Create('Another WCX copy operation is already running');

  WcxCopyInOperation := Self;

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FillAndCount(SourceFiles as TFileSystemFiles,
               FFullFilesTree,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)

  // Make filenames relative to current directory.
  FFullFilesTree.Path := SourceFiles.Path;
end;

procedure TWcxArchiveCopyInOperation.MainExecute;
var
  pDestPath: PChar;
  sDestPath: String;
  WcxModule: TWcxModule;
  iResult: Longint;
begin
  WcxModule := FWcxArchiveFileSource.WcxModule;

  sDestPath := ExcludeFrontPathDelimiter(TargetPath);
  sDestPath := ExcludeTrailingPathDelimiter(sDestPath);
  sDestPath := UTF8ToSys(sDestPath);

  if sDestPath = '' then
    pDestPath := nil
  else
    pDestPath := PAnsiChar(sDestPath); // Make pointer to local variable

  WcxModule.SetChangeVolProc(wcxInvalidHandle, @ChangeVolProc);
  WcxModule.SetProcessDataProc(wcxInvalidHandle, @ProcessDataProc);

  iResult := WcxModule.PackFiles(
               PAnsiChar(UTF8ToSys(FWcxArchiveFileSource.ArchiveFileName)),
               pDestPath, // no trailing path delimiter here
               PAnsiChar(UTF8ToSys(IncludeTrailingPathDelimiter(FFullFilesTree.Path))), // end with path delimiter here
               PAnsiChar(UTF8ToSys(GetFileList(FFullFilesTree))),  // Convert TFiles into PAnsiChar
               FWcxArchiveFileSource.PluginFlags);

  // Check for errors.
  if iResult <> E_SUCCESS then
  begin
    ShowError(Format(rsMsgLogError + rsMsgLogPack,
                     [FWcxArchiveFileSource.ArchiveFileName +
                      ' - ' + GetErrorMsg(iResult)]), [log_arc_op]);
  end
  else
  begin
    LogMessage(Format(rsMsgLogSuccess + rsMsgLogPack,
                      [FWcxArchiveFileSource.ArchiveFileName]), [log_arc_op], lmtSuccess);
  end;
end;

procedure TWcxArchiveCopyInOperation.Finalize;
begin
  WcxCopyInOperation := nil;
end;

function TWcxArchiveCopyInOperation.GetFileList(const theFiles: TFiles): String;
var
  I        : Integer;
  FileName : String;
begin
  Result := '';

  for I := 0 to theFiles.Count - 1 do
    begin
      // Filenames must be relative to the current directory.
      FileName := ExtractDirLevel(theFiles.Path, theFiles[I].FullPath);

      // Special treatment of directories.
      if theFiles[i].IsDirectory then
        // TC ends paths to directories to be packed with '\'.
        FileName := IncludeTrailingPathDelimiter(FileName);

      Result := Result + FileName + #0;
    end;

  Result := Result + #0;
end;

procedure TWcxArchiveCopyInOperation.ShowError(sMessage: String; logOptions: TLogOptions);
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

procedure TWcxArchiveCopyInOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

end.

