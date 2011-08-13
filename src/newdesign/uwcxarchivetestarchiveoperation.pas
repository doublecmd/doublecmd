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

    procedure ShowError(sMessage: String; logOptions: TLogOptions = []);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  protected
    procedure SetChangeVolProc(hArcData: TArcHandle);
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
  FileUtil, uOSUtils, uDCUtils, uShowMsg, uFileSourceOperationUI,
  uWCXmodule, uLng;

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

function ChangeVolProcA(ArcName : PAnsiChar; Mode: LongInt): LongInt; dcpcall;
var
  sArcName: UTF8String;
begin
  sArcName:= SysToUTF8(StrPas(ArcName));
  Result:= ChangeVolProc(sArcName, Mode);
  if Result <> 0 then
    StrPLCopy(ArcName, UTF8ToSys(sArcName), MAX_PATH);
end;

function ChangeVolProcW(ArcName : PWideChar; Mode: LongInt): LongInt; dcpcall;
var
  sArcName: UTF8String;
begin
  sArcName:= UTF8Encode(WideString(ArcName));
  Result:= ChangeVolProc(sArcName, Mode);
  if Result <> 0 then
    StrPLCopyW(ArcName, UTF8Decode(sArcName), MAX_PATH);
end;

function ProcessDataProc(WcxTestArchiveOperation: TWcxArchiveTestArchiveOperation;
                         FileName: UTF8String; Size: LongInt): LongInt;
begin
  //DCDebug('Working (' + IntToStr(GetCurrentThreadId) + ') ' + FileName + ' Size = ' + IntToStr(Size));

  Result := 1;

  if Assigned(WcxTestArchiveOperation) then
  begin
    if WcxTestArchiveOperation.State = fsosStopping then  // Cancel operation
      Exit(0);

    with WcxTestArchiveOperation.FStatistics do
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

            if Size = -100 then // File finished
              DoneBytes := DoneBytes + WcxTestArchiveOperation.FCurrentFileSize;
            //DCDebug('Working ' + FileName + ' Percent1 = ' + IntToStr(FFileOpDlg.iProgress1Pos));
          end
        else if (Size >= -1100) and (Size <= -1000) then // second percent bar
          begin
            DoneBytes := TotalBytes * Int64(-Size - 1000) div 100;
            //DCDebug('Working ' + FileName + ' Percent2 = ' + IntToStr(FFileOpDlg.iProgress2Pos));
          end
        else
          begin
            DoneBytes := DoneBytes + WcxTestArchiveOperation.FCurrentFileSize;
          end;
      end;

      WcxTestArchiveOperation.UpdateStatistics(WcxTestArchiveOperation.FStatistics);
      WcxTestArchiveOperation.CheckOperationState;
    end;
  end;
end;

function ProcessDataProcAG(FileName: PAnsiChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxTestArchiveOperationG, SysToUTF8(StrPas(FileName)), Size);
end;

function ProcessDataProcWG(FileName: PWideChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxTestArchiveOperationG, UTF8Encode(WideString(FileName)), Size);
end;

function ProcessDataProcAT(FileName: PAnsiChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxTestArchiveOperationT, SysToUTF8(StrPas(FileName)), Size);
end;

function ProcessDataProcWT(FileName: PWideChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxTestArchiveOperationT, UTF8Encode(WideString(FileName)), Size);
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
  ClearCurrentOperation;
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
    SetChangeVolProc(ArcHandle);
    SetProcessDataProc(ArcHandle);

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
          ShowError(Format(rsMsgLogError + rsMsgLogTest,
                           [FWcxArchiveFileSource.ArchiveFileName + PathDelim +
                            Header.FileName +
                            ' - ' + GetErrorMsg(iResult)]), [log_arc_op]);

          // User aborted operation.
          if iResult = E_EABORTED then
            Break;
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
                            ' - ' + GetErrorMsg(iResult)]), [log_arc_op]);
        end;
      end; // Skip

    finally
      FreeAndNil(Header);
    end;

    WcxModule.CloseArchive(ArcHandle);

  finally
    if Assigned(Files) then
      FreeAndNil(Files);
  end;
end;

procedure TWcxArchiveTestArchiveOperation.Finalize;
begin
  ClearCurrentOperation;
end;

procedure TWcxArchiveTestArchiveOperation.ShowError(sMessage: String; logOptions: TLogOptions);
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

procedure TWcxArchiveTestArchiveOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

procedure TWcxArchiveTestArchiveOperation.SetChangeVolProc(hArcData: TArcHandle);
begin
  with FWcxArchiveFileSource.WcxModule do
  WcxSetChangeVolProc(hArcData, @ChangeVolProcA, @ChangeVolProcW);
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

