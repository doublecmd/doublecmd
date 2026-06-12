unit uWcxArchiveCopyInOperation;

{$mode objfpc}{$H+}
{$include calling.inc}

interface

uses
  Classes, SysUtils, DCStringHashListUtf8, WcxPlugin, uLog, uGlobs,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFile,
  uWcxModule,
  uWcxArchiveFileSource,
  uArchiveCopyOperation,
  uFileSourceOperationUI,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI;

type

  { TWcxArchiveCopyInOperation }

  TWcxArchiveCopyInOperation = class(TArchiveCopyInOperation)

  private
    FWcxArchiveFileSource: IWcxArchiveFileSource;
    FFileList: TStringHashListUtf8;

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

    property PackingFlags: Integer read FPackingFlags write FPackingFlags;
    property TarBefore: Boolean read FTarBefore write SetTarBefore;
  end;

implementation

uses
  LazUTF8, FileUtil, DCStrUtils, uDCUtils, uLng,
  fWcxArchiveCopyOperationOptions, uFileSystemFileSource, DCOSUtils,
  uTarWriter, uClassesEx, DCConvertEncoding, DCDateTimeUtils,
  uArchiveFileSourceUtil;

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
          // Show only percent
          CurrentFileTotalBytes := -100;
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
  FPackingFlags := PK_PACK_SAVE_PATHS;
  FTarBefore:= False;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);

  FNeedsConnection:= (FWcxArchiveFileSource.WcxModule.BackgroundFlags and BACKGROUND_PACK = 0);

  FFileList:= TStringHashListUtf8.Create(True);

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
  with FStatistics do
  begin
    DoneFiles := -1;
    CurrentFileDoneBytes := -1;
    UpdateStatistics(FStatistics);
  end;
end;

destructor TWcxArchiveCopyInOperation.Destroy;
var
  Index: Integer;
begin
  inherited Destroy;

  for Index:= 0 to FFileList.Count - 1 do
  begin
    TObject(FFileList.List[Index]^.Data).Free;
  end;

  FreeAndNil(FFileList);
end;

procedure TWcxArchiveCopyInOperation.Initialize;
var
  Index: Integer;
  Item: TObjectEx;
  AFileList: TList;
begin
  // Is plugin allow multiple Operations?
  if FNeedsConnection then
    WcxCopyInOperationG := Self
  else
    WcxCopyInOperationT := Self;

  // Need to check file existence
  if FFileExistsOption <> fsoofeOverwrite then
  begin
    AFileList:= FWcxArchiveFileSource.ArchiveFileList.LockList;
    try
      // Populate archive file list
      for Index:= 0 to AFileList.Count - 1 do
      begin
        Item:= TObjectEx(AFileList[Index]).Clone;
        FFileList.Add(UTF8LowerCase(TWcxHeader(Item).FileName), Item);
      end;
    finally
      FWcxArchiveFileSource.ArchiveFileList.UnlockList;
    end;
  end;
end;

procedure TWcxArchiveCopyInOperation.MainExecute;
var
  iResult: LongInt;
  sDestPath: String;
  WcxModule: TWcxModule;

  function doPackFiles( files: TFiles ): LongInt;
  var
    currentFullFiles: TFiles = nil;
    sFileList: String;
  begin
    Result:= E_UNKNOWN;
    try
      FillAndCount(files,
                   currentFullFiles,
                   FStatistics.TotalFiles,
                   FStatistics.TotalBytes);

      // Convert TFiles into String;
      sFileList:= GetFileList(currentFullFiles);
      // Nothing to pack (user skip all files)
      if sFileList = #0 then Exit;

      Result:= WcxModule.WcxPackFiles(
        FWcxArchiveFileSource.ArchiveFileName,
        sDestPath, // no trailing path delimiter here
        IncludeTrailingPathDelimiter(files.Path), // end with path delimiter here
        sFileList,
        PackingFlags);
    finally
      currentFullFiles.Free;
    end;
  end;

  {
    due to the limitations of WcxPackFiles(), when copying multiple paths from
    a virtual FileSource (Search Result / Stash / iCloud) to a Wcx/Zip,
    in some cases, each file must be processed individually. for examples:
    1. SourceFiles in Search Result:
       /home/user/folder1/a
       /home/user/folder2/b
    2. DestPath in Wcx/Zip:
       /Result
    3. the expected path structure after copying is:
       /Result/a (from /home/user/folder1/a)
       /Result/b (from /home/user/folder2/b)
    in this situation, the goal cannot be achieved by calling WcxPackFiles() once.
  }
  function packFileOneByOne: LongInt;
  var
    currentFiles: TFiles;
    f: TFile;
    i: Integer;
  begin
    Result:= E_UNKNOWN;
    currentFiles:= TFiles.Create( EmptyStr );
    currentFiles.OwnsObjects:= False;
    try
      for i:= 0 to SourceFiles.Count-1 do begin
        f:= SourceFiles[i];
        currentFiles.Path:= f.Path;
        currentFiles.Add( f );
        Result:= doPackFiles( currentFiles );
        if Result <> E_SUCCESS then
          break;
        currentFiles.Clear;
      end;
    finally
      currentFiles.Free;
    end;
  end;

  function packAllFiles: LongInt;
  begin
    if SourceFiles.Path <> EmptyStr then begin
      Result:= doPackFiles( self.SourceFiles );
    end else begin
      Result:= packFileOneByOne;
    end;
  end;

begin
  // Put to TAR archive if needed
  if FTarBefore and Tar then Exit;

  WcxModule := FWcxArchiveFileSource.WcxModule;

  sDestPath := ExcludeFrontPathDelimiter(TargetPath);
  sDestPath := ExcludeTrailingPathDelimiter(sDestPath);

  with FStatistics do
  begin
    if FTarBefore then CurrentFileDoneBytes := -1;
    CurrentFileTo:= FWcxArchiveFileSource.ArchiveFileName;
    UpdateStatistics(FStatistics);
  end;

  SetProcessDataProc(wcxInvalidHandle);
  WcxModule.WcxSetChangeVolProc(wcxInvalidHandle);

  iResult:= packAllFiles;

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
      if FileName = EmptyStr then
        continue;

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

  Result:= Result + Format(rsMsgFileExistsFileInfo, [IntToStrTS(aTargetHeader.UnpSize),
                           DateTimeToStr(aTargetHeader.DateTime)]) + LineEnding;

  Result:= Result + LineEnding + rsMsgFileExistsWithFile + LineEnding + aSourceFile.FullPath + LineEnding +
           Format(rsMsgFileExistsFileInfo, [IntToStrTS(aSourceFile.Size), DateTimeToStr(aSourceFile.ModificationTime)]);
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
    if aSourceFile.ModificationTime > aTargetHeader.DateTime then
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
  Result:= TWcxArchiveCopyInOperationOptionsUI;
end;

function TWcxArchiveCopyInOperation.Tar: Boolean;
var
  TarWriter: TTarWriter = nil;
  tarFullFiles: TFiles;
begin
  FillAndCount(SourceFiles,
               tarFullFiles,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);

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
    if TarWriter.ProcessTree(tarFullFiles, FStatistics) then
    begin
      if Result and (PackingFlags and PK_PACK_MOVE_FILES <> 0) then
        DeleteFiles(SourceFiles)
      else
        begin
          // Fill file list with tar archive file
          SourceFiles.Clear;
          SourceFiles.Path:= ExtractFilePath(FTarFileName);
          SourceFiles.Add(TFileSystemFileSource.CreateFileFromFile(FTarFileName));
        end;
    end;
  finally
    FreeAndNil(TarWriter);
    FreeAndNil(tarFullFiles);
  end;
end;

end.

