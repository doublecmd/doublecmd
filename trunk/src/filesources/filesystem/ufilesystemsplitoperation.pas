unit uFileSystemSplitOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSplitOperation,
  uFileSource,
  uFileSourceOperationUI,
  uFile,
  uGlobs, uLog, DCClassesUtf8;

type

  { TFileSystemSplitOperation }

  TFileSystemSplitOperation = class(TFileSourceSplitOperation)
  private
    FStatistics: TFileSourceSplitOperationStatistics; // local copy of statistics
    FTargetpath: String;
    FBuffer: Pointer;
    FBufferSize: LongWord;
    FCheckFreeSpace: Boolean;
  protected
    function Split(aSourceFileStream: TFileStreamEx; TargetFile: String): Boolean;
    procedure ShowError(sMessage: String);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  public
    constructor Create(aFileSource: IFileSource;
                       var aSourceFile: TFile;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  //Lazarus, Free-Pascal, etc.
  LCLProc, LazUTF8, crc,

  //DC
  DCConvertEncoding, uOSUtils, DCOSUtils, uLng, uFileProcs;

constructor TFileSystemSplitOperation.Create(aFileSource: IFileSource;
                                               var aSourceFile: TFile;
                                               aTargetPath: String);
begin
  FCheckFreeSpace := True;
  FTargetpath := IncludeTrailingPathDelimiter(aTargetPath);
  FBufferSize := gCopyBlockSize;
  GetMem(FBuffer, FBufferSize);

  inherited Create(aFileSource, aSourceFile, aTargetPath);
end;

destructor TFileSystemSplitOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;
end;

//TC, when creating the CRC32 verification file after a split will include the target filename with both ANSI and UTF8 string filename
//In the ANSI filename, he puts "_" to replace any UTF8 needed character, not present in regular ANSI set.
//Let's do the same!
//
function ConvertStringToTCStringUTF8CharReplacedByUnderscore(const sString: string): string;
begin
  Result:= StringReplace(CeUtf8ToSys(sString), '?', '_', [rfReplaceAll]);
end;

procedure TFileSystemSplitOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  with FStatistics do
  begin
    CurrentFileFrom:= SourceFile.FullPath;
    TotalFiles:= VolumeNumber;
    TotalBytes:= SourceFile.Size;
  end;
end;

procedure TFileSystemSplitOperation.MainExecute;
var
  iExt, CurrentFileIndex: Integer;
  iTotalDiskSize, iFreeDiskSize: Int64;
  SourceFileStream: TFileStreamEx = nil;
  TargetFilename: String;
  hSummaryFile: THandle;
  SummaryFilename:String;
  respAutomaticSwapDisk: TFileSourceOperationUIResponse;
begin
  try
    if not AutomaticSplitMode then
    begin
      { Check disk free space }
      if FCheckFreeSpace = True then
      begin
        GetDiskFreeSpace(TargetPath, iFreeDiskSize, iTotalDiskSize);
        if FStatistics.TotalBytes > iFreeDiskSize then
        begin
          AskQuestion('', rsMsgNoFreeSpaceCont, [fsourAbort], fsourAbort, fsourAbort);
          RaiseAbortOperation;
        end;
      end;
    end;

    // Open source file
    SourceFileStream := TFileStreamEx.Create(SourceFile.FullPath, fmOpenRead or fmShareDenyNone);
    try
      // Calculate extension length
      iExt:= 3; // Minimum length 3 symbols
      if not AutomaticSplitMode then
      begin
        CurrentFileIndex:= (FStatistics.TotalFiles div 1000);
        while CurrentFileIndex >= 1 do
        begin
          CurrentFileIndex:= CurrentFileIndex div 10;
          Inc(iExt);
        end;
      end;

      //For-loop has been replaced by a while if for any reason the number of files has been miscomputed, it won't create hundreds of file of 0 byte long!
      CurrentFileIndex:=1;
      while ((CurrentFileIndex<=FStatistics.TotalFiles) OR AutomaticSplitMode) AND (FStatistics.TotalBytes>FStatistics.DoneBytes) do
      begin
        //Determine what will be the next filename to the output file
        if RequireACRC32VerificationFile then
          TargetFilename:= FTargetpath + SourceFile.NameNoExt + ExtensionSeparator + Format('%.*d',[iExt, CurrentFileIndex]) //like TC
        else
          TargetFilename:= FTargetpath + SourceFile.Name + ExtensionSeparator + Format('%.*d',[iExt, CurrentFileIndex]); //like DC originally

        if AutomaticSplitMode then
        begin
          repeat
            GetDiskFreeSpace(TargetPath, iFreeDiskSize, iTotalDiskSize);
            VolumeSize:=iFreeDiskSize-(64*1024); //Let's keep a possible 64KB free of space on target even after copy
            if VolumeSize<(64*1024) then
            begin
              respAutomaticSwapDisk:=AskQuestion('',Format(rsMsgInsertNextDisk,[TargetFilename,(FStatistics.TotalBytes-FStatistics.DoneBytes)]),[fsourOk,fsourAbort],fsourOk,fsourAbort);
              if respAutomaticSwapDisk = fsourAbort then RaiseAbortOperation;
              if respAutomaticSwapDisk = fsourOk then VolumeSize:=1*1024*1024; //~~~Debug
            end;
          until (VolumeSize >= (64*1024));
          FStatistics.TotalFiles:=FStatistics.TotalFiles+1;
        end;


        with FStatistics do
        begin
          // Last file can be smaller then volume size
          if (TotalBytes - DoneBytes) < VolumeSize then
            VolumeSize:= TotalBytes - DoneBytes;
          CurrentFileTo := TargetFilename;
          CurrentFileTotalBytes := VolumeSize;
          CurrentFileDoneBytes := 0;
        end;
        UpdateStatistics(FStatistics);

        // Split with current file
        if not Split(SourceFileStream, TargetFilename) then Break;

        with FStatistics do
        begin
          DoneFiles := DoneFiles + 1;
          UpdateStatistics(FStatistics);
        end;

        CheckOperationState;
        inc(CurrentFileIndex);
      end;
    finally
      if Assigned(SourceFileStream) then
      begin
        FreeAndNil(SourceFileStream);
        if (FStatistics.DoneBytes <> FStatistics.TotalBytes) then
        begin
          for CurrentFileIndex := 1 to FStatistics.TotalFiles do
            // There was some error, because not all files has been created.
            // Delete the not completed target files.
            mbDeleteFile(FTargetpath + SourceFile.NameNoExt + ExtensionSeparator + Format('%.*d',[iExt, CurrentFileIndex]));
        end
        else
        begin
          //If requested, let's create the CRC32 verification file
          if RequireACRC32VerificationFile then
          begin
            //We just mimic TC who set in uppercase the "CRC" extension if the filename (without extension!) is made all with capital letters.
            if SourceFile.NameNoExt = UTF8UpperCase(SourceFile.NameNoExt) then
              SummaryFilename:= FTargetpath + SourceFile.NameNoExt + ExtensionSeparator + 'CRC'
            else
              SummaryFilename:= FTargetpath + SourceFile.NameNoExt + ExtensionSeparator + 'crc';

            hSummaryFile := mbFileCreate(SummaryFilename);
            try
              FileWriteLn(hSummaryFile,'filename='+ConvertStringToTCStringUTF8CharReplacedByUnderscore(SourceFile.Name));
              FileWriteLn(hSummaryFile,'filenameutf8='+SourceFile.Name);
              FileWriteLn(hSummaryFile,'size='+IntToStr(SourceFile.Size));
              FileWriteLn(hSummaryFile,'crc32='+hexStr(CurrentCRC32,8));
            finally
              FileClose(hSummaryFile);
            end;
          end;
        end;
      end;
    end;
  except
    on EFOpenError do
      begin
        ShowError(rsMsgLogError + rsMsgErrEOpen + ': ' + SourceFile.FullPath);
      end;
  end;
end;

procedure TFileSystemSplitOperation.Finalize;
begin
end;

function TFileSystemSplitOperation.Split(aSourceFileStream: TFileStreamEx; TargetFile: String): Boolean;
var
  TargetFileStream: TFileStreamEx = nil; // for safety exception handling
  iTotalDiskSize, iFreeDiskSize: Int64;
  bRetryRead, bRetryWrite: Boolean;
  BytesRead, BytesToRead, BytesWrittenTry, BytesWritten: Int64;
  TotalBytesToRead: Int64 = 0;
begin
  Result := False;

  BytesToRead := FBufferSize;
  try
    try
      TargetFileStream := TFileStreamEx.Create(TargetFile, fmCreate);

      TotalBytesToRead := VolumeSize;

      while TotalBytesToRead > 0 do
      begin
        // Without the following line the reading is very slow
        // if it tries to read past end of file.
        if TotalBytesToRead < BytesToRead then
          BytesToRead := TotalBytesToRead;

        repeat
          try
            bRetryRead := False;
            BytesRead := aSourceFileStream.Read(FBuffer^, BytesToRead);

            if (BytesRead = 0) then
              Raise EReadError.Create(mbSysErrorMessage(GetLastOSError));

            if RequireACRC32VerificationFile then CurrentCRC32:=crc32(CurrentCRC32,FBuffer,BytesRead);

            TotalBytesToRead := TotalBytesToRead - BytesRead;
            BytesWritten := 0;

            repeat
              try
                bRetryWrite := False;
                BytesWrittenTry := TargetFileStream.Write((FBuffer + BytesWritten)^, BytesRead);
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
                    GetDiskFreeSpace(TargetPath, iFreeDiskSize, iTotalDiskSize);
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
                case AskQuestion(rsMsgErrERead + ' ' + SourceFile.FullPath + ':',
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
      end; //while

    finally
      if Assigned(TargetFileStream) then
        FreeAndNil(TargetFileStream);
    end;

    Result:= True;

  except
    on EFCreateError do
      begin
        ShowError(rsMsgLogError + rsMsgErrECreate + ': ' + TargetFile);
      end;
    on EWriteError do
      begin
        ShowError(rsMsgLogError + rsMsgErrEWrite + ': ' + TargetFile);
      end;
  end;
end;

procedure TFileSystemSplitOperation.ShowError(sMessage: String);
begin
  if gSkipFileOpError then
    logWrite(Thread, sMessage, lmtError, True)
  else
    begin
      AskQuestion(sMessage, '', [fsourAbort], fsourAbort, fsourAbort);
      RaiseAbortOperation;
    end;
end;

procedure TFileSystemSplitOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

