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
    FTargetPath: String;
    FBuffer: Pointer;
    FBufferSize: LongWord;
    FCheckFreeSpace: Boolean;
    FExtensionLengthRequired : longint;

  protected
    function Combine(aSourceFile: TFile; aTargetFileStream: TFileStreamEx): Boolean;
    procedure ShowError(sMessage: String);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
    function TryToGetInfoFromTheCRC32VerificationFile: Boolean;
    procedure BegForPresenceOfThisFile(aFilename: String);

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
  //Lazarus, Free-Pascal, etc.
  LCLProc, LazUTF8, crc,

  //DC
  uOSUtils, DCOSUtils, uLng, uFileSystemUtil, uFileSystemFileSource,
  uFileProcs, DCConvertEncoding;

{ TFileSystemCombineOperation.Create }
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

{ TFileSystemCombineOperation.Destroy }
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

{ TFileSystemCombineOperation.Initialize }
procedure TFileSystemCombineOperation.Initialize;
var
  MaybeFileIndex: integer;
  MaybeAdditionalSourceFilename: String;
  MaybeFile: TFile;
begin
  // If we're under "RequireDynamicMode", we have just ONE file in "SourceFiles" list,
  // so let's see immediately if we would have the other ready...
  if RequireDynamicMode then
  begin
    // If we're under "RequireDynamicMode", we'll make sure the ".001" file is
    // the first one in the list and available. We need to do that since in main
    // panel we did not force user to select the ".001" file to make this
    // user friendly
    //
    // Also, since we're here, let''s try to see if we have other files
    // in the series ready in the current same folder.
    // It is pertinent to do that so the bar graph will be set as close
    // as possible right from the start as oppose as TC which don't have a global graph.
    FExtensionLengthRequired:=length(SourceFiles[0].Extension);
    MaybeFileIndex:=1;
    repeat
      MaybeAdditionalSourceFilename:=SourceFiles[0].Path + SourceFiles[0].NameNoExt + ExtensionSeparator + Format('%.*d',[FExtensionLengthRequired, MaybeFileIndex]);
      if (mbFileExists(MaybeAdditionalSourceFilename)) OR (MaybeFileIndex=1) then
      begin
        //Let's make sure the first file is available and if not, beg for it!
        if (mbFileExists(MaybeAdditionalSourceFilename)=FALSE) AND (MaybeFileIndex=1) then BegForPresenceOfThisFile(MaybeAdditionalSourceFilename);

        MaybeFile := TFileSystemFileSource.CreateFileFromFile(MaybeAdditionalSourceFilename);
        SourceFiles.Add(MaybeFile);
      end;
      inc(MaybeFileIndex);
    until (not mbFileExists(MaybeAdditionalSourceFilename)) AND (MaybeFileIndex<>1);
    SourceFiles.Delete(0); //We may now delete the first one, which could have been any of the series
  end; //if RequireDynamicMode then...

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
  FStatistics.CurrentFileTo:= TargetFile;

  FillAndCount(SourceFiles, False, False,
               FFullFilesTreeToCombine,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes); // count files

  //If we're under "RequireDynamicMode", check if we have a summary file like TC
  //We do that *after* the standard statistic in case we need to correct them from info in the summary file
  if RequireDynamicMode AND (not WeGotTheCRC32VerificationFile) then TryToGetInfoFromTheCRC32VerificationFile;
end;

{ TFileSystemCombineOperation.MainExecute }
procedure TFileSystemCombineOperation.MainExecute;
var
  aFile, DynamicNextFile: TFile;
  CurrentFileIndex: Integer;
  iTotalDiskSize, iFreeDiskSize: Int64;
  TargetFileStream: TFileStreamEx = nil;
  DynamicNextFilename : String;
  UserAnswer: TFileSourceOperationUIResponse;
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
      CurrentFileIndex:=0;
      while (CurrentFileIndex<FFullFilesTreeToCombine.Count) OR
            (RequireDynamicMode AND (FStatistics.DoneBytes < FStatistics.TotalBytes)) do
      begin
        // In "RequireDynamicMode", we might be here with the next file not available.
        // Let's make sure it's not the case and if so, let's add it to current list
        if (CurrentFileIndex>=FFullFilesTreeToCombine.Count) then
        begin
          DynamicNextFilename:=SourceFiles[0].Path + SourceFiles[0].NameNoExt + ExtensionSeparator + Format('%.*d',[FExtensionLengthRequired, (CurrentFileIndex+1)]);
          BegForPresenceOfThisFile(DynamicNextFilename);
          DynamicNextFile := TFileSystemFileSource.CreateFileFromFile(DynamicNextFilename);
          SourceFiles.Add(DynamicNextFile);
          FFullFilesTreeToCombine.Add(DynamicNextFile.Clone);
        end;

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
        inc(CurrentFileIndex);
      end;
    finally
      if Assigned(TargetFileStream) then
        begin
          FreeAndNil(TargetFileStream);
          if (FStatistics.DoneBytes <> FStatistics.TotalBytes) then
          begin
            // There was some error, because not all files has been combined.
            // Delete the not completed target file.
            mbDeleteFile(TargetFile);

            // In "RequireDynamicMode", to give little feedback to user, let's him know he won't have his file
            if RequireDynamicMode then
            begin
              ShowError(rsMsgLogError + Format(rsMsgIncorrectFilelength,[TargetFile]));
            end;
          end
          else
          begin
            // If all the data have been copied, in the case of the "RequireDynamicMode", we may validate the CRC32,
            // if it was available, so we could valide the integrity of resulting file
            if RequireDynamicMode AND (ExpectedCRC32<>$00000000) then
            begin
              if CurrentCRC32<>ExpectedCRC32 then
              begin
                UserAnswer:=AskQuestion('',rsMsgLogError + Format(rsMsgBadCRC32,[TargetFile]),[fsourNo,fsourYes], fsourNo, fsourNo);
                if UserAnswer=fsourNo then mbDeleteFile(TargetFile);
                RaiseAbortOperation;
              end;
            end;
          end;
        end;
    end;
  except
    on EFCreateError do
      begin
        ShowError(rsMsgLogError + rsMsgErrECreate + ': ' + TargetFile);
      end;
  end;
end;

{ TFileSystemCombineOperation.Finalize }
procedure TFileSystemCombineOperation.Finalize;
begin
end;

{ TFileSystemCombineOperation.Combine }
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

            if BytesRead>0 then CurrentCRC32:=crc32(CurrentCRC32,FBuffer,BytesRead);

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

{ TFileSystemCombineOperation.ShowError }
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

{ TFileSystemCombineOperation.LogMessage }
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

{ TFileSystemCombineOperation.TryToGetInfroFromTheCRC32VerificationFile }
function TFileSystemCombineOperation.TryToGetInfoFromTheCRC32VerificationFile: Boolean;
var
  PosOfEqualSign: integer;
  MaybeSummaryFilename: String;
  SummaryLines: TStringList;
  LineToParse: string;
  UserAnswer: TFileSourceOperationUIResponse;
  i: integer;
begin
  Result:= False;

  //We just mimic TC who set in uppercase the "CRC" extension if the filename (without extension!) is made all with capital letters.
  if SourceFiles[0].NameNoExt = UTF8UpperCase(SourceFiles[0].NameNoExt) then
    MaybeSummaryFilename:= SourceFiles[0].Path + SourceFiles[0].NameNoExt + ExtensionSeparator + 'CRC'
  else begin
    MaybeSummaryFilename:= SourceFiles[0].Path + SourceFiles[0].NameNoExt + ExtensionSeparator + 'crc';
  end;

  //If CRC32 verification file is not found, try to ask user to make it available for us or maybe continue without it if it is what user want
  UserAnswer:=fsourOk;
  while (not mbFileExists(MaybeSummaryFilename)) AND (UserAnswer=fsourOk) do
  begin
    UserAnswer:=AskQuestion(Format(msgTryToLocateCRCFile,[MaybeSummaryFilename]), '' , [fsourOk,fsourCancel], fsourOk, fsourCancel);
  end;

  if mbFileExists(MaybeSummaryFilename) then
  begin
    SummaryLines := TStringListEx.Create;
    try
      SummaryLines.LoadFromFile(MaybeSummaryFilename);
      for i := 0 to SummaryLines.Count - 1 do
      begin
        LineToParse := SummaryLines[i];
        PosOfEqualSign := UTF8Pos('=', LineToParse);
        if PosOfEqualSign > 0 then //Investiguate *only* if the equal sign is present
        begin
          // Let's see if we could extract final filename.
          // We first look for a UTF8 filename style. If so, take it, if not, search for the ANSI flavor
          if UTF8Pos('filenameutf8=', UTF8LowerCase(LineToParse)) > 0 then
          begin
            TargetFile:= ExtractFilePath(TargetFile) + UTF8Copy(LineToParse, (PosOfEqualSign + 1), MaxInt);
          end
          else
          begin
            if Pos('filename=', LowerCase(LineToParse)) > 0 then
              TargetFile:= ExtractFilePath(TargetFile) + CeSysToUtf8(Copy(LineToParse,(PosOfEqualSign + 1) ,MaxInt));
          end;

          //Let's see if we could extract final filesize...
          if UTF8Pos('size=',UTF8LowerCase(LineToParse))>0 then
            FStatistics.TotalBytes:=StrToInt64(UTF8Copy(LineToParse,(PosOfEqualSign+1),(UTF8length(LineToParse)-PosOfEqualSign)));

          //Let's see if we could extract final CRC32...
          if UTF8Pos('crc32=',UTF8LowerCase(LineToParse))>0 then
            ExpectedCRC32:=StrToQWord('x'+UTF8Copy(LineToParse,(PosOfEqualSign+1),(UTF8length(LineToParse)-PosOfEqualSign)));
        end;
      end;

    finally
      SummaryLines.Free;
    end;

    WeGotTheCRC32VerificationFile:=TRUE;
    result:=TRUE;
  end;
end;

{ TFileSystemCombineOperation.BegForPresenceOfThisFile }
procedure TFileSystemCombineOperation.BegForPresenceOfThisFile(aFilename: String);
begin
  while not mbFileExists(aFilename) do
  begin
    case AskQuestion(Format(rsMsgFileNotFound+#$0A+rsMsgProvideThisFile,[aFilename]), '',
                     [fsourRetry, fsourAbort],
                     fsourRetry, fsourAbort) of
      fsourAbort:
        RaiseAbortOperation;
    end;
  end;
end;

end.

