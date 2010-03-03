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
    FWcxArchiveFileSource: IWcxArchiveFileSource;
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    FFullFilesTree: TFileSystemFiles;
    FPackingFlags: Integer; // Packing flags passed to plugin

    {en
      Convert TFiles into a string separated with #0 (format used by WCX).
    }
    function GetFileList(const theFiles: TFiles): String;
    procedure ShowError(sMessage: String; logOptions: TLogOptions = []);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  protected

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

    property PackingFlags: Integer read FPackingFlags write FPackingFlags;
  end;

implementation

uses
  LCLProc, FileUtil, uDCUtils, WcxPlugin, uWCXmodule, uLng, uShowMsg,
  uFileSourceOperationUI, uFileSystemUtil;

// ----------------------------------------------------------------------------
// WCX callbacks

var
  // WCX interface cannot discern different operations (for reporting progress),
  // so this global variable is used to store currently running operation.
  // (There may be other running concurrently, but only one may report progress.)
  WcxCopyInOperation: TWcxArchiveCopyInOperation = nil;

function ChangeVolProc(var ArcName : UTF8String; Mode: LongInt): LongInt;
begin
  Result:= 1;
  case Mode of
  PK_VOL_ASK:
    begin
      // Use operation UI for this?
      if not ShowInputQuery('Double Commander', rsMsgSelLocNextVol, ArcName) then
        Result := 0; // Abort operation
    end;
  PK_VOL_NOTIFY:
    if log_arc_op in gLogOptions then
      LogWrite(rsMsgNextVolUnpack + #32 + ArcName);
  end;
end;

function ChangeVolProcA(ArcName : PAnsiChar; Mode: LongInt): LongInt; stdcall;
var
  sArcName: UTF8String;
begin
  sArcName:= SysToUTF8(StrPas(ArcName));
  Result:= ChangeVolProc(sArcName, Mode);
  if Result <> 0 then
    StrPLCopy(ArcName, UTF8ToSys(sArcName), MAX_PATH);
end;

function ChangeVolProcW(ArcName : PWideChar; Mode: LongInt): LongInt; stdcall;
var
  sArcName: UTF8String;
begin
  sArcName:= UTF8Encode(WideString(ArcName));
  Result:= ChangeVolProc(sArcName, Mode);
  if Result <> 0 then
    StrPLCopyW(ArcName, UTF8Decode(sArcName), MAX_PATH);
end;

function ProcessDataProc(FileName: UTF8String; Size: LongInt): LongInt;
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

function ProcessDataProcA(FileName: PAnsiChar; Size: LongInt): LongInt; stdcall;
begin
  Result:= ProcessDataProc(SysToUTF8(StrPas(FileName)), Size);
end;

function ProcessDataProcW(FileName: PWideChar; Size: LongInt): LongInt; stdcall;
begin
  Result:= ProcessDataProc(UTF8Encode(WideString(FileName)), Size);
end;

// ----------------------------------------------------------------------------

constructor TWcxArchiveCopyInOperation.Create(aSourceFileSource: IFileSource;
                                              aTargetFileSource: IFileSource;
                                              var theSourceFiles: TFiles;
                                              aTargetPath: String);
begin
  FWcxArchiveFileSource := aTargetFileSource as IWcxArchiveFileSource;
  FFullFilesTree := nil;
  FPackingFlags := 0;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TWcxArchiveCopyInOperation.Destroy;
begin
  ClearCurrentOperation;

  inherited Destroy;

  if Assigned(FFullFilesTree) then
    FreeAndNil(FFullFilesTree);
end;

procedure TWcxArchiveCopyInOperation.Initialize;
begin
  if Assigned(WcxCopyInOperation) and (WcxCopyInOperation <> Self) then
    raise Exception.Create('Another WCX copy operation is already running');

  WcxCopyInOperation := Self;

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FillAndCount(SourceFiles as TFileSystemFiles, False,
               FFullFilesTree,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)
end;

procedure TWcxArchiveCopyInOperation.MainExecute;
var
  sDestPath: String;
  WcxModule: TWcxModule;
  iResult: Longint;
begin
  WcxModule := FWcxArchiveFileSource.WcxModule;

  sDestPath := ExcludeFrontPathDelimiter(TargetPath);
  sDestPath := ExcludeTrailingPathDelimiter(sDestPath);
  sDestPath := sDestPath;

  WcxModule.WcxSetChangeVolProc(wcxInvalidHandle, @ChangeVolProcA, @ChangeVolProcW);
  WcxModule.WcxSetProcessDataProc(wcxInvalidHandle, @ProcessDataProcA, @ProcessDataProcW);

  iResult := WcxModule.WcxPackFiles(
               FWcxArchiveFileSource.ArchiveFileName,
               sDestPath, // no trailing path delimiter here
               IncludeTrailingPathDelimiter(FFullFilesTree.Path), // end with path delimiter here
               GetFileList(FFullFilesTree),  // Convert TFiles into UTF8String
               PackingFlags);

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
  ClearCurrentOperation;
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

class procedure TWcxArchiveCopyInOperation.ClearCurrentOperation;
begin
  WcxCopyInOperation := nil;
end;

end.

