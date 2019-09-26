unit uWcxArchiveTestArchiveOperation;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  Classes, SysUtils, WcxPlugin, uLog, uGlobs,
  uFileSourceTestArchiveOperation,
  uFileSource,
  uFileSourceOperation,
  uFile,
  uWcxArchiveFileSource;

type

  { TWcxArchiveTestArchiveOperation }

  TWcxArchiveTestArchiveOperation = class(TFileSourceTestArchiveOperation)

  private
    FWcxArchiveFileSource: IWcxArchiveFileSource;
    FStatistics: TFileSourceTestArchiveOperationStatistics; // local copy of statistics
    FCurrentFileSize: Int64;

    procedure ShowError(const sMessage: String; iError: Integer; logOptions: TLogOptions = []);
    procedure LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  protected
    procedure SetProcessDataProc(hArcData: TArcHandle);

  public
    constructor Create(aSourceFileSource: IFileSource;
                       var theSourceFiles: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

    class procedure ClearCurrentOperation;
  end;

implementation

uses
  FileUtil, LazUTF8, DCOSUtils, DCStrUtils, uDCUtils, uShowMsg,
  uFileSourceOperationUI, uWCXmodule, uLng, DCConvertEncoding;

// ----------------------------------------------------------------------------
// WCX callbacks

var
  // This global variable is used to store currently running operation
  // for plugins that not supports background operations (see GetBackgroundFlags)
  WcxTestArchiveOperationG: TWcxArchiveTestArchiveOperation = nil;

threadvar
  // This thread variable is used to store currently running operation
  // for plugins that supports background operations (see GetBackgroundFlags)
  WcxTestArchiveOperationT: TWcxArchiveTestArchiveOperation;

function ProcessDataProc(WcxTestArchiveOperation: TWcxArchiveTestArchiveOperation;
                         FileName: String; Size: LongInt; UpdateName: Pointer): LongInt;
begin
  //DCDebug('Working (' + IntToStr(GetCurrentThreadId) + ') ' + FileName + ' Size = ' + IntToStr(Size));

  Result := 1;

  if Assigned(WcxTestArchiveOperation) then
  begin
    if WcxTestArchiveOperation.State = fsosStopping then  // Cancel operation
      Exit(0);

    with WcxTestArchiveOperation.FStatistics do
    begin
      // Update file name
      if Assigned(UpdateName) then begin
        CurrentFile:= FileName;
      end;
      // Get the number of bytes processed since the previous call
      if Size > 0 then
      begin
        CurrentFileDoneBytes := CurrentFileDoneBytes + Size;
        if CurrentFileDoneBytes > CurrentFileTotalBytes then
          CurrentFileDoneBytes := CurrentFileTotalBytes;
        DoneBytes := DoneBytes + Size;
      end
      // Get progress percent value to directly set progress bar
      else if Size < 0 then
      begin
        // Total operation percent
        if (Size >= -100) and (Size <= -1) then
        begin
          DoneBytes := TotalBytes * Int64(-Size) div 100;
        end
        // Current file percent
        else if (Size >= -1100) and (Size <= -1000) then
        begin
          CurrentFileTotalBytes := 100;
          CurrentFileDoneBytes := Int64(-Size) - 1000;
        end;
      end;

      WcxTestArchiveOperation.UpdateStatistics(WcxTestArchiveOperation.FStatistics);
      if not WcxTestArchiveOperation.CheckOperationStateSafe then Exit(0);
    end;
  end;
end;

function ProcessDataProcAG(FileName: PAnsiChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxTestArchiveOperationG, CeSysToUtf8(StrPas(FileName)), Size, FileName);
end;

function ProcessDataProcWG(FileName: PWideChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxTestArchiveOperationG, UTF16ToUTF8(UnicodeString(FileName)), Size, FileName);
end;

function ProcessDataProcAT(FileName: PAnsiChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxTestArchiveOperationT, CeSysToUtf8(StrPas(FileName)), Size, FileName);
end;

function ProcessDataProcWT(FileName: PWideChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxTestArchiveOperationT, UTF16ToUTF8(UnicodeString(FileName)), Size, FileName);
end;

// ----------------------------------------------------------------------------

constructor TWcxArchiveTestArchiveOperation.Create(aSourceFileSource: IFileSource;
                                                   var theSourceFiles: TFiles);
begin
  FWcxArchiveFileSource := aSourceFileSource as IWcxArchiveFileSource;

  inherited Create(aSourceFileSource, theSourceFiles);

  FNeedsConnection:= (FWcxArchiveFileSource.WcxModule.BackgroundFlags and BACKGROUND_UNPACK = 0);
end;

destructor TWcxArchiveTestArchiveOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TWcxArchiveTestArchiveOperation.Initialize;
begin
  // Is plugin allow multiple Operations?
  if FNeedsConnection then
    WcxTestArchiveOperationG := Self
  else
    WcxTestArchiveOperationT := Self;

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
  FStatistics.ArchiveFile:= FWcxArchiveFileSource.ArchiveFileName;
end;

procedure TWcxArchiveTestArchiveOperation.MainExecute;
var
  ArcHandle: TArcHandle;
  Header: TWCXHeader;
  OpenResult: Longint;
  iResult: Integer;
  Files: TFiles = nil;
  WcxModule: TWcxModule;
begin
  WcxModule := FWcxArchiveFileSource.WcxModule;

  ArcHandle := WcxModule.OpenArchiveHandle(FWcxArchiveFileSource.ArchiveFileName,
                                           PK_OM_EXTRACT,
                                           OpenResult);
  if ArcHandle = 0 then
  begin
    AskQuestion(uWcxModule.GetErrorMsg(OpenResult), '', [fsourOk], fsourOk, fsourOk);
    RaiseAbortOperation;
  end;

  // Convert file list so that filenames are relative to archive root.
  Files := SourceFiles.Clone;
  ChangeFileListRoot(PathDelim, Files);

  try
    SetProcessDataProc(ArcHandle);
    WcxModule.WcxSetChangeVolProc(ArcHandle);

    while (WcxModule.ReadWCXHeader(ArcHandle, Header) = E_SUCCESS) do
    try
      CheckOperationState;

      // Now check if the file is to be extracted.

      if  (not FPS_ISDIR(Header.FileAttr))           // Omit directories (we handle them ourselves).
      and MatchesFileList(Files, Header.FileName)    // Check if it's included in the filelist
      then
      begin
        with FStatistics do
        begin
          CurrentFile := Header.FileName;
          CurrentFileTotalBytes := Header.UnpSize;
          CurrentFileDoneBytes := 0;

          UpdateStatistics(FStatistics);
          FCurrentFileSize := Header.UnpSize;
        end;

        iResult := WcxModule.WcxProcessFile(ArcHandle, PK_TEST, EmptyStr, EmptyStr);

        if iResult <> E_SUCCESS then
        begin
          // User aborted operation.
          if iResult = E_EABORTED then
            Break;

          ShowError(Format(rsMsgLogError + rsMsgLogTest,
                           [FWcxArchiveFileSource.ArchiveFileName + PathDelim +
                            Header.FileName +
                            ' : ' + GetErrorMsg(iResult)]), iResult, [log_arc_op]);
        end // Error
        else
        begin
          LogMessage(Format(rsMsgLogSuccess + rsMsgLogTest,
                            [FWcxArchiveFileSource.ArchiveFileName + PathDelim +
                             Header.FileName]), [log_arc_op], lmtSuccess);
        end; // Success
      end // Extract
      else // Skip
      begin
        iResult := WcxModule.WcxProcessFile(ArcHandle, PK_SKIP, EmptyStr, EmptyStr);

        //Check for errors
        if iResult <> E_SUCCESS then
        begin
          ShowError(Format(rsMsgLogError + rsMsgLogTest,
                           [FWcxArchiveFileSource.ArchiveFileName + PathDelim +
                            Header.FileName +
                            ' : ' + GetErrorMsg(iResult)]), iResult, [log_arc_op]);
        end;
      end; // Skip

    finally
      FreeAndNil(Header);
    end;

  finally
    WcxModule.CloseArchive(ArcHandle);
    FreeAndNil(Files);
  end;
end;

procedure TWcxArchiveTestArchiveOperation.Finalize;
begin
  ClearCurrentOperation;
end;

procedure TWcxArchiveTestArchiveOperation.ShowError(const sMessage: String;
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

procedure TWcxArchiveTestArchiveOperation.LogMessage(const sMessage: String;
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

procedure TWcxArchiveTestArchiveOperation.SetProcessDataProc(hArcData: TArcHandle);
begin
  with FWcxArchiveFileSource.WcxModule do
  begin
    if FNeedsConnection then
      WcxSetProcessDataProc(hArcData, @ProcessDataProcAG, @ProcessDataProcWG)
    else
      WcxSetProcessDataProc(hArcData, @ProcessDataProcAT, @ProcessDataProcWT);
  end;
end;

class procedure TWcxArchiveTestArchiveOperation.ClearCurrentOperation;
begin
  WcxTestArchiveOperationG := nil;
end;

end.

