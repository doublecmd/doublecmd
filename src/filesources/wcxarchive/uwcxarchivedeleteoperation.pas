unit uWcxArchiveDeleteOperation;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  Classes, SysUtils,
  uFileSourceDeleteOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationUI,
  uFile,
  uWcxArchiveFileSource,
  uGlobs, uLog;

type

  { TWcxArchiveDeleteOperation }

  TWcxArchiveDeleteOperation = class(TFileSourceDeleteOperation)

  private
    FWcxArchiveFileSource: IWcxArchiveFileSource;
    FStatistics: TFileSourceDeleteOperationStatistics; // local copy of statistics

    procedure CountFiles(const theFiles: TFiles; FileMask: String);

    {en
      Convert TFiles into a string separated with #0 (format used by WCX).
    }
    function GetFileList(const theFiles: TFiles): String;

  protected
    procedure ShowError(const sMessage: String; iError: Integer; logOptions: TLogOptions);
    procedure LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToDelete: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

    class procedure ClearCurrentOperation;
  end;

implementation

uses
  DCOSUtils, DCStrUtils, uDCUtils, uLng, uShowMsg, uWCXmodule, WcxPlugin, uMasks,
  FileUtil, LazUTF8, DCConvertEncoding;

// ----------------------------------------------------------------------------
// WCX callbacks

var
  // WCX interface cannot discern different operations (for reporting progress),
  // so this global variable is used to store currently running operation.
  // (There may be other running concurrently, but only one may report progress.)
  WcxDeleteOperation: TWcxArchiveDeleteOperation = nil;

function ProcessDataProc(FileName: String; Size: LongInt): LongInt;
begin
  //DCDebug('Working ' + FileName + ' Size = ' + IntToStr(Size));

  Result := 1;

  if Assigned(WcxDeleteOperation) then
  begin
    if WcxDeleteOperation.State = fsosStopping then  // Cancel operation
      Exit(0);

    with WcxDeleteOperation.FStatistics do
    begin
      CurrentFile := FileName;

      // Get the number of bytes processed since the previous call
      if Size > 0 then
      begin
        TotalFiles := 100;
        DoneBytes := DoneBytes + Size;
        DoneFiles := DoneBytes * 100 div TotalBytes;
      end
      // Get progress percent value to directly set progress bar
      else if Size < 0 then
      begin
        // Total operation percent
        if (Size >= -100) and (Size <= -1) then
        begin
          TotalFiles := 100;
          DoneFiles := -Size;
        end;
      end;

      WcxDeleteOperation.UpdateStatistics(WcxDeleteOperation.FStatistics);
      if not WcxDeleteOperation.CheckOperationStateSafe then Exit(0);
    end;
  end;
end;

function ProcessDataProcA(FileName: PAnsiChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(CeSysToUtf8(StrPas(FileName)), Size);
end;

function ProcessDataProcW(FileName: PWideChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(UTF16ToUTF8(UnicodeString(FileName)), Size);
end;

// ----------------------------------------------------------------------------

constructor TWcxArchiveDeleteOperation.Create(aTargetFileSource: IFileSource;
                                              var theFilesToDelete: TFiles);
begin
  FWcxArchiveFileSource := aTargetFileSource as IWcxArchiveFileSource;

  inherited Create(aTargetFileSource, theFilesToDelete);
end;

destructor TWcxArchiveDeleteOperation.Destroy;
begin
  ClearCurrentOperation;
  inherited Destroy;
end;

procedure TWcxArchiveDeleteOperation.Initialize;
begin
  if Assigned(WcxDeleteOperation) and (WcxDeleteOperation <> Self) then
    raise Exception.Create('Another WCX delete operation is already running');

  WcxDeleteOperation := Self;

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  CountFiles(FilesToDelete, '*.*');
end;

procedure TWcxArchiveDeleteOperation.MainExecute;
var
  iResult: Integer;
  WcxModule: TWcxModule;
begin
  WcxModule := FWcxArchiveFileSource.WcxModule;

  FWcxArchiveFileSource.SetChangeVolProc(wcxInvalidHandle);
  WcxModule.WcxSetProcessDataProc(wcxInvalidHandle, @ProcessDataProcA, @ProcessDataProcW);

  iResult := WcxModule.WcxDeleteFiles(FWcxArchiveFileSource.ArchiveFileName,
                                      GetFileList(FilesToDelete));

  // Check for errors.
  if iResult <> E_SUCCESS then
  begin
    // User aborted operation.
    if iResult = E_EABORTED then Exit;

    ShowError(Format(rsMsgLogError + rsMsgLogDelete,
                     [FWcxArchiveFileSource.ArchiveFileName +
                      ' - ' + GetErrorMsg(iResult)]), iResult, [log_arc_op]);
  end
  else
  begin
    LogMessage(Format(rsMsgLogSuccess + rsMsgLogDelete,
                      [FWcxArchiveFileSource.ArchiveFileName]), [log_arc_op], lmtSuccess);
  end;
end;

procedure TWcxArchiveDeleteOperation.Finalize;
begin
  ClearCurrentOperation;
end;

procedure TWcxArchiveDeleteOperation.ShowError(const sMessage: String;
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

procedure TWcxArchiveDeleteOperation.LogMessage(const sMessage: String;
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

procedure TWcxArchiveDeleteOperation.CountFiles(const theFiles: TFiles; FileMask: String);
var
  i: Integer;
  Header: TWCXHeader;
  ArcFileList: TList;
begin
  ArcFileList := FWcxArchiveFileSource.ArchiveFileList;
  for i := 0 to ArcFileList.Count - 1 do
  begin
    Header := TWCXHeader(ArcFileList.Items[I]);

    // Check if the file from the archive fits the selection given via theFiles.
    if  (not FPS_ISDIR(Header.FileAttr))           // Omit directories
    and MatchesFileList(theFiles, Header.FileName) // Check if it's included in the filelist
    and ((FileMask = '*.*') or (FileMask = '*')    // And name matches file mask
        or MatchesMaskList(ExtractFileName(Header.FileName), FileMask))
    then
    begin
      Inc(FStatistics.TotalBytes, Header.UnpSize);
      Inc(FStatistics.TotalFiles, 1);
    end;
  end;

  UpdateStatistics(FStatistics);
end;

function TWcxArchiveDeleteOperation.GetFileList(const theFiles: TFiles): String;
var
  I        : Integer;
  FileName : String;
begin
  Result := '';

  for I := 0 to theFiles.Count - 1 do
    begin
      // Filenames must be relative to archive root and shouldn't start with path delimiter.
      FileName := ExcludeFrontPathDelimiter(theFiles[I].FullPath);
                  //ExtractDirLevel(FWcxArchiveFileSource.GetRootString, theFiles[I].FullPath)

      // Special treatment of directories.
      if theFiles[i].IsDirectory then
        // TC ends paths to directories to be deleted with '\*.*'
        // (which means delete this directory and all files in it).
        FileName := IncludeTrailingPathDelimiter(FileName) + '*.*';

      Result := Result + FileName + #0;
    end;

  Result := Result + #0;
end;

class procedure TWcxArchiveDeleteOperation.ClearCurrentOperation;
begin
  WcxDeleteOperation := nil;
end;

end.
