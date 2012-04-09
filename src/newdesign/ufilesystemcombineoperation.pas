unit uFileSystemCombineOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCombineOperation,
  uFileSource,
  uFileSourceOperationUI,
  uFile,
  uGlobs, uLog, DCClassesUtf8;

type

  { TFileSystemCombineOperation }

  TFileSystemCombineOperation = class(TFileSourceCombineOperation)
  private
    FFullFilesTreeToCombine: TFiles;  // source files including all files
    FStatistics: TFileSourceCombineOperationStatistics; // local copy of statistics
    FTargetPath: UTF8String;
    FBuffer: Pointer;
    FBufferSize: LongWord;
    FCheckFreeSpace: Boolean;

  protected
    function Combine(aSourceFile: TFile; aTargetFileStream: TFileStreamEx): Boolean;
    procedure ShowError(sMessage: String);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  public
    constructor Create(aFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetFile: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  uOSUtils, DCOSUtils, uLng, uFileSystemUtil, LCLProc;

constructor TFileSystemCombineOperation.Create(aFileSource: IFileSource;
                                               var theSourceFiles: TFiles;
                                               aTargetFile: String);
begin
  FFullFilesTreeToCombine := nil;
  FCheckFreeSpace := True;
  FTargetPath := ExtractFilePath(aTargetFile);
  FBufferSize := gCopyBlockSize;
  GetMem(FBuffer, FBufferSize);

  inherited Create(aFileSource, theSourceFiles, aTargetFile);
end;

destructor TFileSystemCombineOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;

  if Assigned(FFullFilesTreeToCombine) then
    FreeAndNil(FFullFilesTreeToCombine);
end;

procedure TFileSystemCombineOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
  FStatistics.CurrentFileTo:= TargetFile;

  FillAndCount(SourceFiles, False, False,
               FFullFilesTreeToCombine,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes); // count files
end;

procedure TFileSystemCombineOperation.MainExecute;
var
  aFile: TFile;
  CurrentFileIndex: Integer;
  iTotalDiskSize, iFreeDiskSize: Int64;
  TargetFileStream: TFileStreamEx = nil;
begin
  try
    { Check disk free space }
    if FCheckFreeSpace = True then
    begin
      GetDiskFreeSpace(FTargetPath, iFreeDiskSize, iTotalDiskSize);
      if FStatistics.TotalBytes > iFreeDiskSize then
      begin
        AskQuestion('', rsMsgNoFreeSpaceCont, [fsourAbort], fsourAbort, fsourAbort);
        RaiseAbortOperation;
      end;
    end;

    // Create destination file
    TargetFileStream := TFileStreamEx.Create(TargetFile, fmCreate);
    try
      for CurrentFileIndex := 0 to FFullFilesTreeToCombine.Count - 1 do
      begin
        aFile := FFullFilesTreeToCombine[CurrentFileIndex];

        with FStatistics do
        begin
          CurrentFileFrom := aFile.FullPath;
          CurrentFileTotalBytes := aFile.Size;
          CurrentFileDoneBytes := 0;
        end;
        UpdateStatistics(FStatistics);

        // Combine with current file
        if not Combine(aFile, TargetFileStream) then Break;

        with FStatistics do
        begin
          DoneFiles := DoneFiles + 1;
          UpdateStatistics(FStatistics);
        end;

        CheckOperationState;
      end;
    finally
      if Assigned(TargetFileStream) then
        begin
          FreeAndNil(TargetFileStream);
          if (FStatistics.DoneBytes <> FStatistics.TotalBytes) then
            // There was some error, because not all files has been combined.
            // Delete the not completed target file.
            mbDeleteFile(TargetFile);
        end;
    end;
  except
    on EFCreateError do
      begin
        ShowError(rsMsgLogError + rsMsgErrECreate + ': ' + TargetFile);
      end;
  end;
end;

procedure TFileSystemCombineOperation.Finalize;
begin
end;

function TFileSystemCombineOperation.Combine(aSourceFile: TFile;
                                             aTargetFileStream: TFileStreamEx): Boolean;
var
  SourceFileStream: TFileStreamEx;
  iTotalDiskSize, iFreeDiskSize: Int64;
  bRetryRead, bRetryWrite: Boolean;
  BytesRead, BytesToRead, BytesWrittenTry, BytesWritten: Int64;
  TotalBytesToRead: Int64 = 0;
begin
  Result := False;

  BytesToRead := FBufferSize;
  SourceFileStream := nil; // for safety exception handling
  try
    try
      SourceFileStream := TFileStreamEx.Create(aSourceFile.FullPath, fmOpenRead or fmShareDenyNone);

      TotalBytesToRead := SourceFileStream.Size;

      while TotalBytesToRead > 0 do
      begin
        // Without the following line the reading is very slow
        // if it tries to read past end of file.
        if TotalBytesToRead < BytesToRead then
          BytesToRead := TotalBytesToRead;

        repeat
          try
            bRetryRead := False;
            BytesRead := SourceFileStream.Read(FBuffer^, BytesToRead);

            if (BytesRead = 0) then
              Raise EReadError.Create(mbSysErrorMessage(GetLastOSError));

            TotalBytesToRead := TotalBytesToRead - BytesRead;
            BytesWritten := 0;

            repeat
              try
                bRetryWrite := False;
                BytesWrittenTry := aTargetFileStream.Write((FBuffer + BytesWritten)^, BytesRead);
                BytesWritten := BytesWritten + BytesWrittenTry;
                if BytesWrittenTry = 0 then
                begin
                  Raise EWriteError.Create(mbSysErrorMessage(GetLastOSError));
                end
                else if BytesWritten < BytesRead then
                begin
                  bRetryWrite := True;   // repeat and try to write the rest
                end;
              except
                on E: EWriteError do
                  begin
                    { Check disk free space }
                    GetDiskFreeSpace(FTargetPath, iFreeDiskSize, iTotalDiskSize);
                    if BytesRead > iFreeDiskSize then
                      begin
                        case AskQuestion(rsMsgNoFreeSpaceRetry, '',
                                         [fsourYes, fsourNo],
                                         fsourYes, fsourNo) of
                          fsourYes:
                            bRetryWrite := True;
                          fsourNo:
                            RaiseAbortOperation;
                        end; // case
                      end
                    else
                      begin
                        case AskQuestion(rsMsgErrEWrite + ' ' + TargetFile + ':',
                                         E.Message,
                                         [fsourRetry, fsourSkip, fsourAbort],
                                         fsourRetry, fsourSkip) of
                          fsourRetry:
                            bRetryWrite := True;
                          fsourAbort:
                            RaiseAbortOperation;
                          fsourSkip:
                            Exit;
                        end; // case
                      end;

                  end; // on do
              end; // except
            until not bRetryWrite;
          except
            on E: EReadError do
              begin
                case AskQuestion(rsMsgErrERead + ' ' + aSourceFile.FullPath + ':',
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

    finally
      if Assigned(SourceFileStream) then
        FreeAndNil(SourceFileStream);
    end;

    Result:= True;

  except
    on EFOpenError do
      begin
        ShowError(rsMsgLogError + rsMsgErrEOpen + ': ' + aSourceFile.FullPath);
      end;
    on EWriteError do
      begin
        ShowError(rsMsgLogError + rsMsgErrEWrite + ': ' + TargetFile);
      end;
  end;
end;

procedure TFileSystemCombineOperation.ShowError(sMessage: String);
begin
  if gSkipFileOpError then
    logWrite(Thread, sMessage, lmtError, True)
  else
    begin
      AskQuestion(sMessage, '', [fsourAbort], fsourAbort, fsourAbort);
      RaiseAbortOperation;
    end;
end;

procedure TFileSystemCombineOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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
