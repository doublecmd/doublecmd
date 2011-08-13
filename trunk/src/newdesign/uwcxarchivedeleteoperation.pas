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
    procedure ShowError(sMessage: String; logOptions: TLogOptions);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

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
  uOSUtils, uDCUtils, uLng, uShowMsg, uWCXmodule, WcxPlugin, uMasks,
  FileUtil, LCLProc;

// ----------------------------------------------------------------------------
// WCX callbacks

var
  // WCX interface cannot discern different operations (for reporting progress),
  // so this global variable is used to store currently running operation.
  // (There may be other running concurrently, but only one may report progress.)
  WcxDeleteOperation: TWcxArchiveDeleteOperation = nil;

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

function ProcessDataProc(FileName: UTF8String; Size: LongInt): LongInt;
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

      if Size >= 0 then
      begin
        DoneBytes := DoneBytes + Size;
        DoneFiles := DoneFiles + 1;
      end
      else // For plugins which unpack in CloseArchive
      begin
        if (Size >= -100) and (Size <= -1) then // first percent bar
          begin
            if Size = -100 then // File finished
            begin
              //DoneBytes := DoneBytes + {FileSize(FileName)};
              DoneFiles := DoneFiles + 1;
            end;
          end
        else if (Size >= -1100) and (Size <= -1000) then // second percent bar
          begin
            DoneBytes := TotalBytes * Int64(-Size - 1000) div 100;
            DoneFiles := DoneFiles + 1;
          end
        else
          begin
            DoneFiles := DoneFiles + 1;
          end;
      end;

      WcxDeleteOperation.UpdateStatistics(WcxDeleteOperation.FStatistics);
      WcxDeleteOperation.CheckOperationState;
    end;
  end;
end;

function ProcessDataProcA(FileName: PAnsiChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(SysToUTF8(StrPas(FileName)), Size);
end;

function ProcessDataProcW(FileName: PWideChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(UTF8Encode(WideString(FileName)), Size);
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

  WcxModule.WcxSetChangeVolProc(wcxInvalidHandle, @ChangeVolProcA, @ChangeVolProcW);
  WcxModule.WcxSetProcessDataProc(wcxInvalidHandle, @ProcessDataProcA, @ProcessDataProcW);

  iResult := WcxModule.WcxDeleteFiles(FWcxArchiveFileSource.ArchiveFileName,
                                      GetFileList(FilesToDelete));

  // Check for errors.
  if iResult <> E_SUCCESS then
  begin
    ShowError(Format(rsMsgLogError + rsMsgLogDelete,
                     [FWcxArchiveFileSource.ArchiveFileName +
                      ' - ' + GetErrorMsg(iResult)]), [log_arc_op]);
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

procedure TWcxArchiveDeleteOperation.ShowError(sMessage: String; logOptions: TLogOptions);
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

procedure TWcxArchiveDeleteOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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
