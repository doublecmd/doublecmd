unit uFileSystemCalcChecksumOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  uFileSourceCalcChecksumOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uFileSystemFile,
  uGlobs, uLog, uHash, uClassesEx;

type

  TFileSystemCalcChecksumOperation = class(TFileSourceCalcChecksumOperation)

  private
    FFullFilesTree: TFileSystemFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceCalcChecksumOperationStatistics; // local copy of statistics
    FCheckSumFile: TStringListEx;
    FBuffer: Pointer;
    FBufferSize: LongWord;
    FChecksumsList: TObjectList;

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FSkipErrors: Boolean;

    //procedure ShowVerifyCheckSumResult;
    function CheckSumCalc(aFile: TFile): String;
    function GetHashAlgByFileName(const sFileName: UTF8String): THashAlgorithm;
    procedure InitializeVerifyMode;
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

    function CalcChecksumProcessFile(aFile: TFileSystemFile): Boolean;
    function VerifyChecksumProcessFile(aFile: TFileSystemFile; ExpectedChecksum: String): Boolean;

  public
    constructor Create(var aTargetFileSource: TFileSource;
                       var theFiles: TFiles;
                       aTargetPath: String;
                       aTargetMask: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  uDCUtils, uOSUtils, uLng,
  uFileSystemUtil, LCLProc,
  FileUtil, StrUtils, fCheckSumVerify;

type
  TChecksumEntry = class
  public
    Checksum: String;
    Algorithm: THashAlgorithm;
  end;

constructor TFileSystemCalcChecksumOperation.Create(
                var aTargetFileSource: TFileSource;
                var theFiles: TFiles;
                aTargetPath: String;
                aTargetMask: String);
begin
  FBuffer := nil;
  FSymLinkOption := fsooslNone;
  FSkipErrors := False;
  FFullFilesTree := nil;
  FCheckSumFile := TStringListEx.Create;
  FChecksumsList := TObjectList.Create(True);

  inherited Create(aTargetFileSource, theFiles,
                   aTargetPath, aTargetMask);
end;

destructor TFileSystemCalcChecksumOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;

  if Assigned(FFullFilesTree) then
    FreeAndNil(FFullFilesTree);
  if Assigned(FCheckSumFile) then
    FreeAndNil(FCheckSumFile);
  if Assigned(FChecksumsList) then
    FreeAndNil(FChecksumsList);
end;

procedure TFileSystemCalcChecksumOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  case Mode of
    checksum_calc:
      FillAndCount(Files as TFileSystemFiles,
                   FFullFilesTree,
                   FStatistics.TotalFiles,
                   FStatistics.TotalBytes);     // gets full list of files (recursive)

    checksum_verify:
      InitializeVerifyMode;
  end;

  FBufferSize := gCopyBlockSize;
  GetMem(FBuffer, FBufferSize);

  FResult.Clear;
end;

procedure TFileSystemCalcChecksumOperation.MainExecute;
var
  aFile: TFileSystemFile;
  CurrentFileIndex: Integer;
  OldDoneBytes: Int64; // for if there was an error
  Entry: TChecksumEntry;
begin
  for CurrentFileIndex := 0 to FFullFilesTree.Count - 1 do
  begin
    aFile := FFullFilesTree[CurrentFileIndex] as TFileSystemFile;

    with FStatistics do
    begin
      CurrentFile := aFile.Path + aFile.Name;
      CurrentFileTotalBytes := aFile.Size;
      CurrentFileDoneBytes := 0;
    end;

    UpdateStatistics(FStatistics);

    // If there will be an error in ProcessFile the DoneBytes value
    // will be inconsistent, so remember it here.
    OldDoneBytes := FStatistics.DoneBytes;

    case Mode of
      checksum_calc:
        if not aFile.IsDirectory then
          CalcChecksumProcessFile(aFile);

      checksum_verify:
        begin
          Entry := FChecksumsList.Items[CurrentFileIndex] as TChecksumEntry;
          Algorithm := Entry.Algorithm;
          VerifyChecksumProcessFile(aFile, Entry.Checksum);
        end;
    end;

    with FStatistics do
    begin
      DoneFiles := DoneFiles + 1;
      DoneBytes := OldDoneBytes + aFile.Size;

      UpdateStatistics(FStatistics);
    end;

    CheckOperationState;
  end;

  case Mode of
    checksum_calc:
      // make result
      if OneFile then
        FCheckSumFile.SaveToFile(TargetMask);

    checksum_verify:
      begin
      {
         Synchronize(@FFileOpDlg.Hide);
         Synchronize(@ShowVerifyCheckSumResult);
      }
      end;
  end;
end;

procedure TFileSystemCalcChecksumOperation.Finalize;
begin
end;

procedure TFileSystemCalcChecksumOperation.InitializeVerifyMode;
var
  CurrentFileIndex, I: Integer;
  aFile, aFileToVerify: TFileSystemFile;
  anAlgorithm: THashAlgorithm;
  FileName: String;
  Entry: TChecksumEntry;
begin
  FFullFilesTree := TFileSystemFiles.Create;
  FChecksumsList.Clear;
  for CurrentFileIndex := 0 to Files.Count - 1 do
  begin
    aFile := Files[CurrentFileIndex] as TFileSystemFile;
    FCheckSumFile.Clear;
    FCheckSumFile.NameValueSeparator:= #32;
    FCheckSumFile.LoadFromFile(aFile.FullPath);

    anAlgorithm := GetHashAlgByFileName(aFile.FullPath);

    for I := 0 to FCheckSumFile.Count - 1 do
    begin
      FileName := aFile.Path + Copy(FCheckSumFile.ValueFromIndex[I], 2, MaxInt);
      try
        aFileToVerify := TFileSystemFile.Create(FileName);
        try
          with FStatistics do
          begin
            TotalFiles := TotalFiles + 1;
            TotalBytes := TotalBytes + aFileToVerify.Size;
          end;

          FFullFilesTree.Add(aFileToVerify);
          Entry := TChecksumEntry.Create;
          FChecksumsList.Add(Entry);
          Entry.Checksum := FCheckSumFile.Names[I];
          Entry.Algorithm := anAlgorithm;

        except
          FreeAndNil(aFileToVerify);
          raise;
        end;

      except
        on EFileSystemFileNotExists do
          begin
            // error - file does not exist
          end;
      end;

      CheckOperationState;
    end;
  end;
end;

function TFileSystemCalcChecksumOperation.CalcChecksumProcessFile(aFile: TFileSystemFile): Boolean;
var
  FileName: String;
  sCheckSum: String;
begin
  Result := False;
  FileName := aFile.Path + aFile.Name;

  if not OneFile then
    FCheckSumFile.Clear;

  sCheckSum := CheckSumCalc(aFile);
  FCheckSumFile.Add(sCheckSum + ' *' +
                    ExtractDirLevel(FileSource.CurrentPath, aFile.Path) + aFile.Name);

  if not OneFile then
    FCheckSumFile.SaveToFile(FileName + '.' + HashFileExt[Algorithm]);
end;

function TFileSystemCalcChecksumOperation.VerifyChecksumProcessFile(
           aFile: TFileSystemFile; ExpectedChecksum: String): Boolean;
var
  sCheckSum: String;
  bResult: Boolean;
begin
  Result := False;

  sCheckSum:= CheckSumCalc(aFile);
  bResult:= (StrComp(PChar(sCheckSum), PChar(ExpectedChecksum)) = 0);
  FResult.Add(ExtractDirLevel(FileSource.CurrentPath, aFile.Path) +
              aFile.Name + ': ' +
              IfThen(bResult, 'True', 'False'));
end;

function TFileSystemCalcChecksumOperation.CheckSumCalc(aFile: TFile): String;
var
  hFile: THandle;
  Context: THashContext;
  Digest: THashDigest;
  BytesRead, BytesToRead: Int64;
  bRetryRead: Boolean;
  TotalBytesToRead: Int64 = 0;
begin
  Result:= EmptyStr;
  hFile := feInvalidHandle;
  BytesToRead := FBufferSize;

  HashInit(Context, Algorithm);
  try
    hFile:= mbFileOpen(aFile.FullPath, fmOpenRead or fmShareDenyNone);

    if hFile <> feInvalidHandle then
      begin
        TotalBytesToRead := mbFileSize(aFile.FullPath);

        while TotalBytesToRead > 0 do
        begin
          // Without the following line the reading is very slow
          // if it tries to read past end of file.
          if TotalBytesToRead < BytesToRead then
            BytesToRead := TotalBytesToRead;

          repeat
            try
              bRetryRead := False;
              BytesRead := FileRead(hFile, FBuffer^, BytesToRead);

              if (BytesRead = 0) then
                Raise EReadError.Create(mbSysErrorMessage(GetLastOSError));

              TotalBytesToRead := TotalBytesToRead - BytesRead;

              HashUpdate(Context, FBuffer^, BytesRead);

            except
              on E: EReadError do
                begin
                  if gSkipFileOpError then
                  begin
                    LogMessage(rsMsgErrERead + ' ' + aFile.FullPath + ': ' + E.Message,
                               [], lmtError);
		    Exit;
                  end
                  else
                  case AskQuestion(rsMsgErrERead + ' ' + aFile.FullPath + ': ',
                                   E.Message,
                                   [fsourRetry, fsourSkip, fsourAbort],
                                   fsourRetry, fsourSkip) of
                    fsourRetry:
                      bRetryRead := True;
                    fsourAbort:
                      RaiseAbortOperation;
                    fsourSkip:
                      Exit;
                  end; // case
                end;
            end;
          until not bRetryRead;

          with FStatistics do
          begin
            CurrentFileDoneBytes := CurrentFileDoneBytes + BytesRead;
            DoneBytes := DoneBytes + BytesRead;

            UpdateStatistics(FStatistics);
          end;

          CheckOperationState; // check pause and stop
        end;//while
      end;

  finally
    HashFinal(Context, Digest);
    Result:= HashPrint(Digest);
    if hFile <> feInvalidHandle then
    begin
      FileClose(hFile);
      hFile := feInvalidHandle;
    end;
  end;
end;

function TFileSystemCalcChecksumOperation.GetHashAlgByFileName(const sFileName: UTF8String): THashAlgorithm;
var
  sExt: UTF8String;
begin
  sExt:= ExtractFileExt(sFileName);
  if mbCompareText(sExt, '.md5') = 0 then
    Result:= HASH_MD5
  else if mbCompareText(sExt, '.sha') = 0 then
    Result:= HASH_SHA1;
end;

procedure TFileSystemCalcChecksumOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

