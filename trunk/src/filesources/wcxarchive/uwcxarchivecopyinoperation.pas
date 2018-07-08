unit uWcxArchiveCopyInOperation;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  Classes, SysUtils, StringHashList, WcxPlugin, uLog, uGlobs,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFile,
  uWcxModule,
  uWcxArchiveFileSource,
  uFileSourceOperationUI,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI;

type

  { TWcxArchiveCopyInOperation }

  TWcxArchiveCopyInOperation = class(TFileSourceCopyInOperation)

  private
    FWcxArchiveFileSource: IWcxArchiveFileSource;
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    FFullFilesTree: TFiles;
    FPackingFlags: Integer; // Packing flags passed to plugin
    FTarBefore: Boolean;      // Create TAR archive first
    FTarFileName: String; // Temporary TAR archive name
    FFileList: TStringHashList;

    {en
      Convert TFiles into a string separated with #0 (format used by WCX).
    }
    function GetFileList(const theFiles: TFiles): String;
    procedure SetTarBefore(const AValue: Boolean);
    procedure ShowError(const sMessage: String; iError: Integer; logOptions: TLogOptions = []);
    procedure LogMessage(const sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
    procedure DeleteFiles(const aFiles: TFiles);

  protected
    function Tar: Boolean;
    procedure SetProcessDataProc(hArcData: TArcHandle);

  protected
    FCurrentFile: TFile;
    FCurrentTargetFilePath: String;
    procedure QuestionActionHandler(Action: TFileSourceOperationUIAction);
    function FileExistsMessage(aSourceFile: TFile; aTargetHeader: TWcxHeader): String;
    function FileExists(aSourceFile: TFile; aTargetHeader: TWcxHeader): TFileSourceOperationOptionFileExists;

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
    class function GetOptionsUIClass: TFileSourceOperationOptionsUIClass; override;
    function GetDescription(Details: TFileSourceOperationDescriptionDetails): String; override;

    property PackingFlags: Integer read FPackingFlags write FPackingFlags;
    property TarBefore: Boolean read FTarBefore write SetTarBefore;
  end;

implementation

uses
  LazUTF8, FileUtil, StrUtils, DCStrUtils, uLng, uShowMsg, fWcxArchiveCopyOperationOptions,
  uFileSystemFileSource, DCOSUtils, uTarWriter,
  DCConvertEncoding, DCDateTimeUtils, uArchiveFileSourceUtil;

// ----------------------------------------------------------------------------
// WCX callbacks

var
  // This global variable is used to store currently running operation
  // for plugins that not supports background operations (see GetBackgroundFlags)
  WcxCopyInOperationG: TWcxArchiveCopyInOperation = nil;

threadvar
  // This thread variable is used to store currently running operation
  // for plugins that supports background operations (see GetBackgroundFlags)
  WcxCopyInOperationT: TWcxArchiveCopyInOperation;

function ProcessDataProc(WcxCopyInOperation: TWcxArchiveCopyInOperation;
                         FileName: String; Size: LongInt): LongInt;
begin
  //DCDebug('Working (' + IntToStr(GetCurrentThreadId) + ') ' + FileName + ' Size = ' + IntToStr(Size));

  Result := 1;

  if Assigned(WcxCopyInOperation) then
  begin
    if WcxCopyInOperation.State = fsosStopping then  // Cancel operation
      Exit(0);

    with WcxCopyInOperation.FStatistics do
    begin
      CurrentFileFrom:= FileName;

      // Get the number of bytes processed since the previous call
      if Size > 0 then
      begin
        DoneBytes := DoneBytes + Size;
        if TotalFiles = 1 then begin
          CurrentFileDoneBytes := DoneBytes;
          CurrentFileTotalBytes := TotalBytes;
        end;
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

      WcxCopyInOperation.UpdateStatistics(WcxCopyInOperation.FStatistics);
      if not WcxCopyInOperation.AppProcessMessages(True) then Exit(0);
    end;
  end;
end;

function ProcessDataProcAG(FileName: PAnsiChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxCopyInOperationG, CeSysToUtf8(StrPas(FileName)), Size);
end;

function ProcessDataProcWG(FileName: PWideChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxCopyInOperationG, UTF16ToUTF8(UnicodeString(FileName)), Size);
end;

function ProcessDataProcAT(FileName: PAnsiChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxCopyInOperationT, CeSysToUtf8(StrPas(FileName)), Size);
end;

function ProcessDataProcWT(FileName: PWideChar; Size: LongInt): LongInt; dcpcall;
begin
  Result:= ProcessDataProc(WcxCopyInOperationT, UTF16ToUTF8(UnicodeString(FileName)), Size);
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
  FTarBefore:= False;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);

  FNeedsConnection:= (FWcxArchiveFileSource.WcxModule.BackgroundFlags and BACKGROUND_PACK = 0);

  FFileList:= TStringHashList.Create(True);
end;

destructor TWcxArchiveCopyInOperation.Destroy;
begin
  inherited Destroy;

  FreeAndNil(FFileList);
  FreeAndNil(FFullFilesTree);
end;

procedure TWcxArchiveCopyInOperation.Initialize;
var
  Item: TObject;
  Index: Integer;
begin
  // Is plugin allow multiple Operations?
  if FNeedsConnection then
    WcxCopyInOperationG := Self
  else
    WcxCopyInOperationT := Self;

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
  FStatistics.CurrentFileDoneBytes := -1;

  // Gets full list of files (recursive)
  FillAndCount(SourceFiles,
               FFullFilesTree,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);

  // Need to check file existence
  if FFileExistsOption <> fsoofeOverwrite then
  begin
    // Populate archive file list
    for Index:= 0 to FWcxArchiveFileSource.ArchiveFileList.Count - 1 do
    begin
      Item:= FWcxArchiveFileSource.ArchiveFileList[Index];
      FFileList.Add(UTF8LowerCase(TWcxHeader(Item).FileName), Item);
    end;
  end;
end;

procedure TWcxArchiveCopyInOperation.MainExecute;
var
  iResult: Integer;
  sFileList: String;
  sDestPath: String;
  WcxModule: TWcxModule;
begin
  // Put to TAR archive if needed
  if FTarBefore and Tar then Exit;

  WcxModule := FWcxArchiveFileSource.WcxModule;

  sDestPath := ExcludeFrontPathDelimiter(TargetPath);
  sDestPath := ExcludeTrailingPathDelimiter(sDestPath);
  sDestPath := sDestPath;

  with FStatistics do
  begin
    if FTarBefore then CurrentFileDoneBytes := -1;
    CurrentFileTo:= FWcxArchiveFileSource.ArchiveFileName;
    UpdateStatistics(FStatistics);
  end;

  SetProcessDataProc(wcxInvalidHandle);
  FWcxArchiveFileSource.SetChangeVolProc(wcxInvalidHandle);

  // Convert TFiles into String;
  sFileList:= GetFileList(FFullFilesTree);
  // Nothing to pack (user skip all files)
  if sFileList = #0 then Exit;

  iResult := WcxModule.WcxPackFiles(
               FWcxArchiveFileSource.ArchiveFileName,
               sDestPath, // no trailing path delimiter here
               IncludeTrailingPathDelimiter(FFullFilesTree.Path), // end with path delimiter here
               sFileList,
               PackingFlags);

  // Check for errors.
  if iResult <> E_SUCCESS then
  begin
    // User aborted operation.
    if iResult = E_EABORTED then RaiseAbortOperation;

    ShowError(Format(rsMsgLogError + rsMsgLogPack,
                     [FWcxArchiveFileSource.ArchiveFileName +
                      ' : ' + GetErrorMsg(iResult)]), iResult, [log_arc_op]);
  end
  else
  begin
    LogMessage(Format(rsMsgLogSuccess + rsMsgLogPack,
                      [FWcxArchiveFileSource.ArchiveFileName]), [log_arc_op], lmtSuccess);

    FStatistics.DoneFiles:= FStatistics.TotalFiles;
    UpdateStatistics(FStatistics);
  end;

  // Delete temporary TAR archive if needed
  if FTarBefore then mbDeleteFile(FTarFileName);
end;

procedure TWcxArchiveCopyInOperation.Finalize;
begin
  ClearCurrentOperation;
end;

function TWcxArchiveCopyInOperation.GetDescription(Details: TFileSourceOperationDescriptionDetails): String;
begin
  case Details of
    fsoddJobAndTarget:
    begin
      if SourceFiles.Count = 1 then
        Result := Format(rsOperPackingSomethingTo, [SourceFiles[0].Name, FWcxArchiveFileSource.ArchiveFileName])
      else
        Result := Format(rsOperPackingFromTo, [SourceFiles.Path, FWcxArchiveFileSource.ArchiveFileName]);
    end;
    else
      Result := rsOperPacking;
  end;
end;

function TWcxArchiveCopyInOperation.GetFileList(const theFiles: TFiles): String;
var
  I: Integer;
  SubPath: String;
  FileName: String;
  Header: TWCXHeader;
  ArchiveExists: Boolean;
begin
  Result := '';

  ArchiveExists := FFileList.Count > 0;
  SubPath := UTF8LowerCase(ExcludeFrontPathDelimiter(TargetPath));

  for I := 0 to theFiles.Count - 1 do
    begin
      // Filenames must be relative to the current directory.
      FileName := ExtractDirLevel(theFiles.Path, theFiles[I].FullPath);

      // Special treatment of directories.
      if theFiles[i].IsDirectory then
      begin
        // TC ends paths to directories to be packed with '\'.
        FileName := IncludeTrailingPathDelimiter(FileName);
      end
      // Need to check file existence
      else if ArchiveExists then
      begin
        Header := TWcxHeader(FFileList[SubPath + UTF8LowerCase(FileName)]);
        if Assigned(Header) then
        begin
          if FileExists(theFiles[I], Header) = fsoofeSkip then
            Continue;
        end;
      end;

      Result := Result + FileName + #0;
    end;

  Result := Result + #0;
end;

procedure TWcxArchiveCopyInOperation.SetTarBefore(const AValue: Boolean);
begin
  with FWcxArchiveFileSource, FWcxArchiveFileSource.WcxModule do
  begin
    FTarBefore:= AValue;
    if FTarBefore and Assigned(PackToMem) and (PluginCapabilities and PK_CAPS_MEMPACK <> 0) then
      FNeedsConnection:= (BackgroundFlags and BACKGROUND_MEMPACK = 0)
    else
      FNeedsConnection:= (BackgroundFlags and BACKGROUND_PACK = 0);
  end;
end;

procedure TWcxArchiveCopyInOperation.ShowError(const sMessage: String;
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

procedure TWcxArchiveCopyInOperation.LogMessage(const sMessage: String;
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

procedure TWcxArchiveCopyInOperation.DeleteFiles(const aFiles: TFiles);
var
  I: Integer;
  aFile: TFile;
begin
  for I:= aFiles.Count - 1 downto 0 do
  begin
    aFile:= aFiles[I];
    if aFile.IsDirectory then
      mbRemoveDir(aFile.FullPath)
    else
      mbDeleteFile(aFile.FullPath);
  end;
end;

procedure TWcxArchiveCopyInOperation.SetProcessDataProc(hArcData: TArcHandle);
begin
  with FWcxArchiveFileSource.WcxModule do
  begin
    if FNeedsConnection then
      WcxSetProcessDataProc(hArcData, @ProcessDataProcAG, @ProcessDataProcWG)
    else
      WcxSetProcessDataProc(hArcData, @ProcessDataProcAT, @ProcessDataProcWT);
  end;
end;

procedure TWcxArchiveCopyInOperation.QuestionActionHandler(
  Action: TFileSourceOperationUIAction);
begin
  if Action = fsouaCompare then
    ShowCompareFilesUI(FCurrentFile, IncludeFrontPathDelimiter(FCurrentTargetFilePath));
end;

function TWcxArchiveCopyInOperation.FileExistsMessage(aSourceFile: TFile; aTargetHeader: TWcxHeader): String;
begin
  Result:= rsMsgFileExistsOverwrite + LineEnding + aTargetHeader.FileName + LineEnding;

  Result:= Result + Format(rsMsgFileExistsFileInfo, [Numb2USA(IntToStr(aTargetHeader.UnpSize)),
                           DateTimeToStr(WcxFileTimeToDateTime(aTargetHeader.FileTime))]) + LineEnding;

  Result:= Result + LineEnding + rsMsgFileExistsWithFile + LineEnding + aSourceFile.FullPath + LineEnding +
           Format(rsMsgFileExistsFileInfo, [Numb2USA(IntToStr(aSourceFile.Size)), DateTimeToStr(aSourceFile.ModificationTime)]);
end;

function TWcxArchiveCopyInOperation.FileExists(aSourceFile: TFile;
  aTargetHeader: TWcxHeader): TFileSourceOperationOptionFileExists;
const
  PossibleResponses: array[0..8] of TFileSourceOperationUIResponse
    = (fsourOverwrite, fsourSkip, fsourOverwriteLarger,
       fsourOverwriteAll, fsourSkipAll, fsourOverwriteSmaller,
       fsourOverwriteOlder, fsouaCompare, fsourCancel);

  function OverwriteOlder: TFileSourceOperationOptionFileExists;
  begin
    if aSourceFile.ModificationTime > WcxFileTimeToDateTime(aTargetHeader.FileTime)  then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

  function OverwriteSmaller: TFileSourceOperationOptionFileExists;
  begin
    if aSourceFile.Size > aTargetHeader.UnpSize then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

  function OverwriteLarger: TFileSourceOperationOptionFileExists;
  begin
    if aSourceFile.Size < aTargetHeader.UnpSize then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

begin
  case FFileExistsOption of
    fsoofeNone:
      begin
        FCurrentFile := aSourceFile;
        FCurrentTargetFilePath := aTargetHeader.FileName;
        case AskQuestion(FileExistsMessage(aSourceFile, aTargetHeader), '',
                         PossibleResponses, fsourOverwrite, fsourSkip,
                         @QuestionActionHandler) of
          fsourOverwrite:
            Result := fsoofeOverwrite;
          fsourSkip:
            Result := fsoofeSkip;
          fsourOverwriteAll:
            begin
              FFileExistsOption := fsoofeOverwrite;
              Result := fsoofeOverwrite;
            end;
          fsourSkipAll:
            begin
              FFileExistsOption := fsoofeSkip;
              Result := fsoofeSkip;
            end;
          fsourOverwriteOlder:
            begin
              FFileExistsOption := fsoofeOverwriteOlder;
              Result:= OverwriteOlder;
            end;
          fsourOverwriteSmaller:
            begin
              FFileExistsOption := fsoofeOverwriteSmaller;
              Result:= OverwriteSmaller;
            end;
          fsourOverwriteLarger:
            begin
              FFileExistsOption := fsoofeOverwriteLarger;
              Result:= OverwriteLarger;
            end;
          fsourNone,
          fsourCancel:
            RaiseAbortOperation;
        end;
      end;
    fsoofeOverwriteOlder:
      begin
        Result:= OverwriteOlder;
      end;
    fsoofeOverwriteSmaller:
      begin
        Result:= OverwriteSmaller;
      end;
    fsoofeOverwriteLarger:
      begin
        Result:= OverwriteLarger;
      end;
    else
      Result := FFileExistsOption;
  end;
end;

class procedure TWcxArchiveCopyInOperation.ClearCurrentOperation;
begin
  WcxCopyInOperationG := nil;
end;

class function TWcxArchiveCopyInOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result:= TWcxArchiveCopyOperationOptionsUI;
end;

function TWcxArchiveCopyInOperation.Tar: Boolean;
var
  TarWriter: TTarWriter = nil;
begin
  with FWcxArchiveFileSource, FWcxArchiveFileSource.WcxModule do
  begin
    if Assigned(PackToMem) and (PluginCapabilities and PK_CAPS_MEMPACK <> 0) then
      begin
        FTarFileName:= ArchiveFileName;
        TarWriter:= TTarWriter.Create(FTarFileName,
                                      @AskQuestion,
                                      @RaiseAbortOperation,
                                      @CheckOperationState,
                                      @UpdateStatistics,
                                      WcxModule
                                     );
        Result:= True;
      end
    else
      begin
        FTarFileName:= RemoveFileExt(ArchiveFileName);
        TarWriter:= TTarWriter.Create(FTarFileName,
                                      @AskQuestion,
                                      @RaiseAbortOperation,
                                      @CheckOperationState,
                                      @UpdateStatistics
                                     );
        Result:= False;
      end;
  end;

  try
    if TarWriter.ProcessTree(FFullFilesTree, FStatistics) then
    begin
      if Result and (PackingFlags and PK_PACK_MOVE_FILES <> 0) then
        DeleteFiles(FFullFilesTree)
      else
        begin
          // Fill file list with tar archive file
          FFullFilesTree.Clear;
          FFullFilesTree.Path:= ExtractFilePath(FTarFileName);
          FFullFilesTree.Add(TFileSystemFileSource.CreateFileFromFile(FTarFileName));
        end;
    end;
  finally
    FreeAndNil(TarWriter);
  end;
end;

end.

