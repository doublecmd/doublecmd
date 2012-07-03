unit uFileSystemCalcChecksumOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  uFileSourceCalcChecksumOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uGlobs, uLog, uHash, DCClassesUtf8;

type

  TFileSystemCalcChecksumOperation = class(TFileSourceCalcChecksumOperation)

  private
    FFullFilesTree: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceCalcChecksumOperationStatistics; // local copy of statistics
    FCheckSumFile: TStringListEx;
    FBuffer: Pointer;
    FBufferSize: LongWord;
    FChecksumsList: TObjectList;

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FSkipErrors: Boolean;

    function CheckSumCalc(aFile: TFile): String;
    procedure InitializeVerifyMode;
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

    function CalcChecksumProcessFile(aFile: TFile): Boolean;
    function VerifyChecksumProcessFile(aFile: TFile; ExpectedChecksum: String): Boolean;

  public
    constructor Create(aTargetFileSource: IFileSource;
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
  LCLProc, StrUtils, FileUtil,
  uLng, uFileSystemUtil, uFileSystemFileSource, DCOSUtils, DCStrUtils;

type
  TChecksumEntry = class
  public
    Checksum: String;
    Algorithm: THashAlgorithm;
  end;

constructor TFileSystemCalcChecksumOperation.Create(
                aTargetFileSource: IFileSource;
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
  FResult.Clear;
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  case Mode of
    checksum_calc:
      FillAndCount(Files, False, False,
                   FFullFilesTree,
                   FStatistics.TotalFiles,
                   FStatistics.TotalBytes);     // gets full list of files (recursive)

    checksum_verify:
      InitializeVerifyMode;
  end;

  FBufferSize := gCopyBlockSize;
  GetMem(FBuffer, FBufferSize);
end;

procedure TFileSystemCalcChecksumOperation.MainExecute;
var
  aFile: TFile;
  CurrentFileIndex: Integer;
  OldDoneBytes: Int64; // for if there was an error
  Entry: TChecksumEntry;
begin
  for CurrentFileIndex := 0 to FFullFilesTree.Count - 1 do
  begin
    aFile := FFullFilesTree[CurrentFileIndex];

    with FStatistics do
    begin
      CurrentFile := aFile.Path + aFile.Name;
      CurrentFileTotalBytes := aFile.Size;
      CurrentFileDoneBytes := 0;
    end;

    UpdateStatistics(FStatistics);

    if not aFile.IsDirectory then
    begin
      // If there will be an error in ProcessFile the DoneBytes value
      // will be inconsistent, so remember it here.
      OldDoneBytes := FStatistics.DoneBytes;

      case Mode of
        checksum_calc:
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
    end;

    CheckOperationState;
  end;

  case Mode of
    checksum_calc:
      // make result
      if OneFile then
        try
          FCheckSumFile.SaveToFile(TargetMask);
        except
          on E: EFCreateError do
            AskQuestion(rsMsgErrECreate + ' ' + TargetMask + ':',
                                 E.Message, [fsourOk], fsourOk, fsourOk);
          on E: EWriteError do
            AskQuestion(rsMsgErrEWrite + ' ' + TargetMask + ':',
                               E.Message, [fsourOk], fsourOk, fsourOk);
        end;

    checksum_verify:
      begin
      end;
  end;
end;

procedure TFileSystemCalcChecksumOperation.Finalize;
begin
end;

procedure TFileSystemCalcChecksumOperation.InitializeVerifyMode;
var
  CurrentFileIndex, I: Integer;
  aFile, aFileToVerify: TFile;
  anAlgorithm: THashAlgorithm;
  FileName: String;
  Entry: TChecksumEntry;
begin
  FFullFilesTree := TFiles.Create(Files.Path);
  FChecksumsList.Clear;
  for CurrentFileIndex := 0 to Files.Count - 1 do
  begin
    aFile := Files[CurrentFileIndex];
    FCheckSumFile.Clear;
    FCheckSumFile.NameValueSeparator:= #32;
    FCheckSumFile.LoadFromFile(aFile.FullPath);

    anAlgorithm := FileExtToHashAlg(aFile.Extension);

    for I := 0 to FCheckSumFile.Count - 1 do
    begin
      FileName := aFile.Path + Copy(FCheckSumFile.ValueFromIndex[I], 2, MaxInt);
      try
        aFileToVerify := TFileSystemFileSource.CreateFileFromFile(FileName);
        if not (aFileToVerify.IsDirectory or aFileToVerify.IsLinkToDirectory) then
        begin
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
        end
        else
          FreeAndNil(aFileToVerify);

      except
        on EFileNotFound do
          begin
            FResult.Add(Format(rsViewNotFound,
                               [Copy(FCheckSumFile.ValueFromIndex[I], 2, MaxInt) + ': '])
                       );
          end
        else
          begin
            FreeAndNil(aFileToVerify);
            raise;
          end;
      end;

      CheckOperationState;
    end;
  end;
end;

function TFileSystemCalcChecksumOperation.CalcChecksumProcessFile(aFile: TFile): Boolean;
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
                    ExtractDirLevel(FFullFilesTree.Path,
                                    aFile.Path) + aFile.Name);

  if not OneFile then
    try
      FCheckSumFile.SaveToFile(FileName + '.' + HashFileExt[Algorithm]);
    except
      on E: EFCreateError do
        AskQuestion(rsMsgErrECreate + ' ' + FileName + '.' + HashFileExt[Algorithm] + ':',
                                 E.Message, [fsourOk], fsourOk, fsourOk);
      on E: EWriteError do
        AskQuestion(rsMsgErrEWrite + ' ' + FileName + '.' + HashFileExt[Algorithm] + ':',
                               E.Message, [fsourOk], fsourOk, fsourOk);
    end;
end;

function TFileSystemCalcChecksumOperation.VerifyChecksumProcessFile(
           aFile: TFile; ExpectedChecksum: String): Boolean;
var
  sCheckSum: String;
  bResult: Boolean;
begin
  Result := False;

  sCheckSum:= CheckSumCalc(aFile);
  bResult:= (CompareText(sCheckSum, ExpectedChecksum) = 0);
  FResult.Add(ExtractDirLevel(FFullFilesTree.Path, aFile.Path) +
              aFile.Name + ': ' +
              IfThen(bResult, 'True', 'False'));
end;

function TFileSystemCalcChecksumOperation.CheckSumCalc(aFile: TFile): String;
var
  hFile: THandle;
  Context: THashContext;
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
    HashFinal(Context, Result);
    if hFile <> feInvalidHandle then
    begin
      FileClose(hFile);
      hFile := feInvalidHandle;
    end;
  end;
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

