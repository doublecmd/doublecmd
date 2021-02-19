unit uFileSystemUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDescr, uLog, uGlobs, DCOSUtils,
  uFile,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFileSourceCopyOperation,
  uFileSourceTreeBuilder;

  function ApplyRenameMask(aFile: TFile; NameMask: String; ExtMask: String): String; overload;
  procedure FillAndCount(Files: TFiles;
                         CountDirs: Boolean;
                         ExcludeRootDir: Boolean;
                         out NewFiles: TFiles;
                         out FilesCount: Int64;
                         out FilesSize: Int64);
  function FileExistsMessage(const TargetName, SourceName: String;
                             SourceSize: Int64; SourceTime: TDateTime): String;

type

  TUpdateStatisticsFunction = procedure(var NewStatistics: TFileSourceCopyOperationStatistics) of object;

  TFileSystemOperationTargetExistsResult =
    (fsoterNotExists, fsoterDeleted, fsoterAddToTarget, fsoterResume,
     fsoterSkip, fsoterRenamed);

  TFileSystemOperationHelperMode =
    (fsohmCopy, fsohmMove);

  TFileSystemOperationHelperCopyMode =
    (fsohcmDefault, fsohcmAppend, fsohcmResume);

  TFileSystemOperationHelperMoveOrCopy
    = function(SourceFile: TFile; TargetFileName: String;
               Mode: TFileSystemOperationHelperCopyMode): Boolean of object;

  { TFileSystemTreeBuilder }

  TFileSystemTreeBuilder = class(TFileSourceTreeBuilder)
  protected
    procedure AddLinkTarget(aFile: TFile; CurrentNode: TFileTreeNode); override;
    procedure AddFilesInDirectory(srcPath: String; CurrentNode: TFileTreeNode); override;
  end;

  { TFileSystemOperationHelper }

  TFileSystemOperationHelper = class
  private
    FOperationThread: TThread;
    FMode: TFileSystemOperationHelperMode;
    FBuffer: Pointer;
    FBufferSize: LongWord;
    FRootTargetPath: String;
    FRenameMask: String;
    FRenameNameMask, FRenameExtMask: String;
    FSetPropertyError: TFileSourceOperationOptionSetPropertyError;
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    FDescription: TDescription;
    FLogCaption: String;
    FRenamingFiles: Boolean;
    FRenamingRootDir: Boolean;
    FRootDir: TFile;
    FVerify,
    FReserveSpace,
    FCheckFreeSpace: Boolean;
    FSkipAllBigFiles: Boolean;
{$IF DEFINED(UNIX)}
    FSkipAllSpecialFiles: Boolean;
{$ENDIF}
    FSkipRenameError: Boolean;
    FSkipOpenForReadingError: Boolean;
    FSkipOpenForWritingError: Boolean;
    FSkipReadError: Boolean;
    FSkipWriteError: Boolean;
    FSkipCopyError: Boolean;
    FAutoRenameItSelf: Boolean;
    FCorrectSymLinks: Boolean;
    FCopyAttributesOptions: TCopyAttributesOptions;
    FMaxPathOption: TFileSourceOperationUIResponse;
    FDeleteFileOption: TFileSourceOperationUIResponse;
    FFileExistsOption: TFileSourceOperationOptionFileExists;
    FDirExistsOption: TFileSourceOperationOptionDirectoryExists;

    FCurrentFile: TFile;
    FCurrentTargetFilePath: String;

    AskQuestion: TAskQuestionFunction;
    AbortOperation: TAbortOperationFunction;
    CheckOperationState: TCheckOperationStateFunction;
    UpdateStatistics: TUpdateStatisticsFunction;
    AppProcessMessages: TAppProcessMessagesFunction;
    ShowCompareFilesUI: TShowCompareFilesUIFunction;
    MoveOrCopy: TFileSystemOperationHelperMoveOrCopy;

    procedure ShowError(sMessage: String);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

    function CheckFileHash(const FileName, Hash: String; Size: Int64): Boolean;
    function CompareFiles(const FileName1, FileName2: String; Size: Int64): Boolean;
    function CopyFile(SourceFile: TFile; TargetFileName: String; Mode: TFileSystemOperationHelperCopyMode): Boolean;
    function MoveFile(SourceFile: TFile; TargetFileName: String; Mode: TFileSystemOperationHelperCopyMode): Boolean;
    procedure CopyProperties(SourceFile: TFile; TargetFileName: String);

    function ProcessNode(aFileTreeNode: TFileTreeNode; CurrentTargetPath: String): Boolean;
    function ProcessDirectory(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
    function ProcessLink(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
    function ProcessFile(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;

    function TargetExists(aNode: TFileTreeNode; var AbsoluteTargetFileName: String)
                 : TFileSystemOperationTargetExistsResult;
    function DirExists(aFile: TFile;
                       AbsoluteTargetFileName: String;
                       AllowCopyInto: Boolean;
                       AllowDelete: Boolean): TFileSourceOperationOptionDirectoryExists;
    procedure QuestionActionHandler(Action: TFileSourceOperationUIAction);
    function FileExists(aFile: TFile;
                        var AbsoluteTargetFileName: String;
                        AllowAppend: Boolean): TFileSourceOperationOptionFileExists;

    procedure CountStatistics(aNode: TFileTreeNode);

  public
    constructor Create(AskQuestionFunction: TAskQuestionFunction;
                       AbortOperationFunction: TAbortOperationFunction;
                       AppProcessMessagesFunction: TAppProcessMessagesFunction;
                       CheckOperationStateFunction: TCheckOperationStateFunction;
                       UpdateStatisticsFunction: TUpdateStatisticsFunction;
                       ShowCompareFilesUIFunction: TShowCompareFilesUIFunction;

                       OperationThread: TThread;
                       Mode: TFileSystemOperationHelperMode;
                       TargetPath: String;
                       StartingStatistics: TFileSourceCopyOperationStatistics);
    destructor Destroy; override;

    procedure Initialize;

    procedure ProcessTree(aFileTree: TFileTree);

    property Verify: Boolean read FVerify write FVerify;
    property FileExistsOption: TFileSourceOperationOptionFileExists read FFileExistsOption write FFileExistsOption;
    property DirExistsOption: TFileSourceOperationOptionDirectoryExists read FDirExistsOption write FDirExistsOption;
    property CheckFreeSpace: Boolean read FCheckFreeSpace write FCheckFreeSpace;
    property ReserveSpace: Boolean read FReserveSpace write FReserveSpace;
    property SetPropertyError: TFileSourceOperationOptionSetPropertyError read FSetPropertyError write FSetPropertyError;
    property SkipAllBigFiles: Boolean read FSkipAllBigFiles write FSkipAllBigFiles;
    property AutoRenameItSelf: Boolean read FAutoRenameItSelf write FAutoRenameItSelf;
    property CopyAttributesOptions: TCopyAttributesOptions read FCopyAttributesOptions write FCopyAttributesOptions;
    property CorrectSymLinks: Boolean read FCorrectSymLinks write FCorrectSymLinks;
    property RenameMask: String read FRenameMask write FRenameMask;
  end;

implementation

uses
  uDebug, uOSUtils, DCStrUtils, FileUtil, uFindEx, DCClassesUtf8, uFileProcs, uLng,
  DCBasicTypes, uFileSource, uFileSystemFileSource, uFileProperty, uAdministrator,
  StrUtils, DCDateTimeUtils, uShowMsg, Forms, LazUTF8, uHash, uFileCopyEx, SysConst
{$IFDEF UNIX}
  , BaseUnix
{$ENDIF}
  ;

const
  HASH_TYPE = HASH_BEST;

function ApplyRenameMask(aFile: TFile; NameMask: String; ExtMask: String): String; overload;
begin
  // Only change name for files.
  if aFile.IsDirectory or aFile.IsLink then
    Result := aFile.Name
  else
    Result := ApplyRenameMask(aFile.Name, NameMask, ExtMask);
end;

procedure FillAndCount(Files: TFiles; CountDirs: Boolean; ExcludeRootDir: Boolean;
  out NewFiles: TFiles; out FilesCount: Int64; out FilesSize: Int64);

  procedure FillAndCountRec(const srcPath: String);
  var
    sr: TSearchRecEx;
    aFile: TFile;
  begin
    if FindFirstUAC(srcPath + '*', 0, sr) = 0 then
    begin
      repeat
        if (sr.Name='.') or (sr.Name='..') then Continue;
        aFile := TFileSystemFileSource.CreateFile(srcPath, @sr);

        NewFiles.Add(aFile);
        if aFile.IsLink then
        begin
        end
        else if aFile.IsDirectory then
        begin
          if CountDirs then
            Inc(FilesCount);
          FillAndCountRec(srcPath + sr.Name + DirectorySeparator); // go down to directory
        end
        else
        begin
          FilesSize:= FilesSize + aFile.Size;
          Inc(FilesCount);
        end;
      until FindNextUAC(sr) <> 0;
    end;

    FindCloseUAC(sr);
  end;

var
  i: Integer;
  aFile: TFile;
  aFindData: TFileAttributeData;
begin
  FilesCount:= 0;
  FilesSize:= 0;

  if ExcludeRootDir then
  begin
    if Files.Count <> 1 then
      raise Exception.Create('Only a single directory can be set with ExcludeRootDir=True');
    NewFiles := TFiles.Create(Files[0].FullPath);
    FillAndCountRec(Files[0].FullPath + DirectorySeparator);
  end
  else
  begin
    NewFiles := TFiles.Create(Files.Path);
    for i := 0 to Files.Count - 1 do
    begin
      aFile := Files[i];

      // Update file attributes
      if FileGetAttrUAC(aFile.FullPath, aFindData) then
      begin
        aFile.Size:= aFindData.Size;
        aFile.Attributes:= aFindData.Attr;
        aFile.ModificationTime:= FileTimeToDateTime(aFindData.LastWriteTime);
      end;

      NewFiles.Add(aFile.Clone);

      if aFile.IsLink then
      begin
      end
      else if aFile.IsDirectory then
      begin
        if CountDirs then
          Inc(FilesCount);
        FillAndCountRec(aFile.FullPath + DirectorySeparator);  // recursive browse child dir
      end
      else
      begin
        Inc(FilesCount);
        FilesSize:= FilesSize + aFile.Size; // in first level we know file size -> use it
      end;
    end;
  end;
end;

function FileExistsMessage(const TargetName, SourceName: String;
                           SourceSize: Int64; SourceTime: TDateTime): String;
var
  TargetInfo: TFileAttributeData;
begin
  Result:= rsMsgFileExistsOverwrite + LineEnding + WrapTextSimple(TargetName, 100) + LineEnding;
  if FileGetAttrUAC(TargetName, TargetInfo) then
  begin
    Result:= Result + Format(rsMsgFileExistsFileInfo, [Numb2USA(IntToStr(TargetInfo.Size)),
                             DateTimeToStr(FileTimeToDateTime(TargetInfo.LastWriteTime))]) + LineEnding;
  end;
  Result:= Result + LineEnding + rsMsgFileExistsWithFile + LineEnding + WrapTextSimple(SourceName, 100) + LineEnding +
           Format(rsMsgFileExistsFileInfo, [Numb2USA(IntToStr(SourceSize)), DateTimeToStr(SourceTime)]);
end;

function FileCopyProgress(TotalBytes, DoneBytes: Int64; UserData: Pointer): LongBool;
var
  Helper: TFileSystemOperationHelper absolute UserData;
begin
  if (DoneBytes > 0) then
    Helper.FStatistics.DoneBytes+= (DoneBytes - Helper.FStatistics.CurrentFileDoneBytes);

  //Helper.FStatistics.CurrentFileTotalBytes:= TotalBytes;
  Helper.FStatistics.CurrentFileDoneBytes:= DoneBytes;
  Helper.UpdateStatistics(Helper.FStatistics);
  try
    Helper.CheckOperationState;
  except
    on E: EFileSourceOperationAborting do
      Exit(False);
  end;
  Result:= True;
end;

// ----------------------------------------------------------------------------

procedure TFileSystemTreeBuilder.AddLinkTarget(aFile: TFile; CurrentNode: TFileTreeNode);
var
  LinkedFilePath: String;
  LinkedFile: TFile = nil;
  AddedNode: TFileTreeNode;
  AddedIndex: Integer;
begin
  LinkedFilePath := mbReadAllLinks(aFile.FullPath);
  if (LinkedFilePath <> '') and (LinkedFilePath <> PathDelim) then
  begin
    try
      LinkedFile := TFileSystemFileSource.CreateFileFromFile(LinkedFilePath);

      // Add link to current node.
      AddedIndex := CurrentNode.AddSubNode(aFile);
      AddedNode := CurrentNode.SubNodes[AddedIndex];
      AddedNode.Data := TFileTreeNodeData.Create;
      (CurrentNode.Data as TFileTreeNodeData).SubnodesHaveLinks := True;

      // Then add linked file/directory as a subnode of the link.
      AddItem(LinkedFile, AddedNode);

    except
      on EFileNotFound do
        begin
          // Link target doesn't exist - add symlink instead of target (or ask user).
          AddLink(aFile, CurrentNode);
        end;
    end;
  end
  else
  begin
    // error - cannot follow symlink - adding symlink instead of target (or ask user)
    AddLink(aFile, CurrentNode);
  end;
end;

procedure TFileSystemTreeBuilder.AddFilesInDirectory(
              srcPath: String;
              CurrentNode: TFileTreeNode);
var
  sr: TSearchRecEx;
  aFile: TFile;
begin
  if FindFirstUAC(srcPath + '*', 0, sr) = 0 then
  begin
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then Continue;

      aFile := TFileSystemFileSource.CreateFile(srcPath, @sr);
      AddItem(aFile, CurrentNode);
    until FindNextUAC(sr) <> 0;
  end;

  FindCloseUAC(sr);
end;

// ----------------------------------------------------------------------------

constructor TFileSystemOperationHelper.Create(
  AskQuestionFunction: TAskQuestionFunction;
  AbortOperationFunction: TAbortOperationFunction;
  AppProcessMessagesFunction: TAppProcessMessagesFunction;
  CheckOperationStateFunction: TCheckOperationStateFunction;
  UpdateStatisticsFunction: TUpdateStatisticsFunction;
  ShowCompareFilesUIFunction: TShowCompareFilesUIFunction;
  OperationThread: TThread; Mode: TFileSystemOperationHelperMode;
  TargetPath: String; StartingStatistics: TFileSourceCopyOperationStatistics);
begin
  AskQuestion := AskQuestionFunction;
  AbortOperation := AbortOperationFunction;
  AppProcessMessages := AppProcessMessagesFunction;
  CheckOperationState := CheckOperationStateFunction;
  UpdateStatistics := UpdateStatisticsFunction;
  ShowCompareFilesUI := ShowCompareFilesUIFunction;

  FOperationThread := OperationThread;
  FMode := Mode;

  FBufferSize := gCopyBlockSize;
  GetMem(FBuffer, FBufferSize);

  FCheckFreeSpace := True;
  FSkipAllBigFiles := False;
  FSkipReadError := False;
  FSkipWriteError := False;
  FCopyAttributesOptions := CopyAttributesOptionCopyAll;
  FFileExistsOption := fsoofeNone;
  FDirExistsOption := fsoodeNone;
  FSetPropertyError := fsoospeNone;
  FRootTargetPath := TargetPath;
  FRenameMask := '';
  FStatistics := StartingStatistics;
  FRenamingFiles := False;
  FRenamingRootDir := False;
  FRootDir := nil;

  if gProcessComments then
    FDescription := TDescription.Create(True)
  else
    FDescription := nil;

  case FMode of
    fsohmCopy:
      begin
        MoveOrCopy := @CopyFile;
        FLogCaption := rsMsgLogCopy;
      end;
    fsohmMove:
      begin
        MoveOrCopy := @MoveFile;
        FLogCaption := rsMsgLogMove;
      end;
    else
      raise Exception.Create('Invalid operation mode');
  end;

  inherited Create;
end;

destructor TFileSystemOperationHelper.Destroy;
begin
  inherited Destroy;

  if Assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;

  if Assigned(FDescription) then
  begin
    FDescription.SaveDescription;
    FreeAndNil(FDescription);
  end;
end;

procedure TFileSystemOperationHelper.Initialize;
begin
  SplitFileMask(FRenameMask, FRenameNameMask, FRenameExtMask);

  // Create destination path if it doesn't exist.
  if not DirectoryExistsUAC(FRootTargetPath) then
    if not ForceDirectoriesUAC(FRootTargetPath) then
      Exit; // do error
end;

procedure TFileSystemOperationHelper.ProcessTree(aFileTree: TFileTree);
var
  aFile: TFile;
begin
  FRenamingFiles := (FRenameMask <> '*.*') and (FRenameMask <> '');

  // If there is a single root dir and rename mask doesn't have wildcards
  // treat is as a rename of the root dir.
  if (aFileTree.SubNodesCount = 1) and FRenamingFiles then
  begin
    aFile := aFileTree.SubNodes[0].TheFile;
    if (aFile.IsDirectory or aFile.IsLinkToDirectory) and
       not ContainsWildcards(FRenameMask) then
    begin
      FRenamingFiles := False;
      FRenamingRootDir := True;
      FRootDir := aFile;
    end;
  end;

  ProcessNode(aFileTree, FRootTargetPath);
end;

// ----------------------------------------------------------------------------

function TFileSystemOperationHelper.CopyFile(
           SourceFile: TFile;
           TargetFileName: String;
           Mode: TFileSystemOperationHelperCopyMode): Boolean;
var
  SourceFileStream, TargetFileStream: TFileStreamUAC;
  iTotalDiskSize, iFreeDiskSize: Int64;
  bRetryRead, bRetryWrite: Boolean;
  BytesRead, BytesToRead, BytesWrittenTry, BytesWritten: Int64;
  TotalBytesToRead: Int64 = 0;
  NewPos: Int64;
  Hash: String;
  Options: UInt32;
  Context: THashContext;
  DeleteFile: Boolean = False;

  procedure OpenSourceFile;
  var
    bRetry: Boolean = True;
  begin
    while bRetry do
    begin
      bRetry := False;
      SourceFileStream.Free; // In case stream was created but 'while' loop run again
      try
        SourceFileStream := TFileStreamUAC.Create(SourceFile.FullPath, fmOpenRead or fmShareDenyNone);
      except
        on EFOpenError do
          begin
            if not FSkipOpenForReadingError then
            begin
              case AskQuestion(rsMsgErrEOpen + ': ' + SourceFile.FullPath, '',
                               [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                               fsourRetry, fsourSkip) of
                fsourRetry:
                  bRetry := True;
                fsourAbort:
                  AbortOperation;
                fsourSkip: ; // Do nothing
                fsourSkipAll:
                  FSkipOpenForReadingError := True;
              end;
            end;
          end;
      end;
    end;
    if not Assigned(SourceFileStream) and (log_errors in gLogOptions) then
      logWrite(FOperationThread, rsMsgLogError + rsMsgErrEOpen + ': ' + SourceFile.FullPath, lmtError, True);
  end;

  procedure OpenTargetFile;
    function GetMsgByMode: String;
    begin
      if Mode in [fsohcmAppend, fsohcmResume] then
        Result := rsMsgErrEOpen
      else
        Result := rsMsgErrECreate;
    end;
    function HandleError: Boolean;
    begin
      Result := False;
      if not FSkipOpenForWritingError then
      begin
        case AskQuestion(GetMsgByMode + ': ' + TargetFileName, '',
                         [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                         fsourRetry, fsourSkip) of
          fsourRetry:
            Result := True;
          fsourAbort:
            AbortOperation;
          fsourSkip: ; // Do nothing
          fsourSkipAll:
            FSkipOpenForWritingError := True;
        end;
      end;
    end;
  var
    Flags: LongWord = 0;
    bRetry: Boolean = True;
  begin
    while bRetry do
    begin
      bRetry := False;
      if FVerify then Flags := fmOpenSync;
      try
        TargetFileStream.Free; // In case stream was created but 'while' loop run again
        case Mode of
        fsohcmAppend:
          begin
            TargetFileStream := TFileStreamUAC.Create(TargetFileName, fmOpenReadWrite or Flags);
            TargetFileStream.Seek(0, soFromEnd); // seek to end
            TotalBytesToRead := SourceFileStream.Size;
          end;
        fsohcmResume:
          begin
            TargetFileStream := TFileStreamUAC.Create(TargetFileName, fmOpenReadWrite or Flags);
            NewPos := TargetFileStream.Seek(0, soFromEnd);
            SourceFileStream.Seek(NewPos, soFromBeginning);
            TotalBytesToRead := SourceFileStream.Size - NewPos;
          end
        else
          begin
            TargetFileStream := TFileStreamUAC.Create(TargetFileName, fmCreate or Flags);
            TotalBytesToRead := SourceFileStream.Size;
            if FReserveSpace then
            begin
              TargetFileStream.Size:= SourceFileStream.Size;
              TargetFileStream.Seek(0, fsFromBeginning);
            end;
          end;
        end;
      except
        on EFOpenError do
          bRetry := HandleError;
        on EFCreateError do
          bRetry := HandleError;
      end;
    end;
    if not Assigned(TargetFileStream) and (log_errors in gLogOptions) then
      logWrite(FOperationThread, rsMsgLogError + GetMsgByMode + ': ' + TargetFileName, lmtError, True);
  end;
begin
  Result := False;

  { Check disk free space }
  if FCheckFreeSpace = True then
  begin
    GetDiskFreeSpace(ExtractFilePath(TargetFileName), iFreeDiskSize, iTotalDiskSize);
    if SourceFile.Size > iFreeDiskSize then
    begin
      if FSkipAllBigFiles = True then
      begin
        Exit;
      end
      else
      begin
        case AskQuestion('', rsMsgNoFreeSpaceCont,
                         [fsourYes, fsourAll, fsourNo, fsourSkip, fsourSkipAll],
                         fsourYes, fsourNo) of
          fsourNo:
            AbortOperation;

          fsourSkip:
            Exit;

          fsourAll:
            FCheckFreeSpace := False;

          fsourSkipAll:
            begin
              FSkipAllBigFiles := True;
              Exit;
            end;
        end;
      end;
    end;
  end;

  if Assigned(FileCopyEx) and (Mode = fsohcmDefault) then
  begin
    if FVerify then
      Options:= FILE_COPY_NO_BUFFERING
    else begin
      Options:= 0;
    end;
    repeat
      bRetryWrite:= False;
      Result:= FileCopyUAC(SourceFile.FullPath, TargetFileName, Options, @FileCopyProgress, Self);
      if not Result then
      begin
        if FSkipCopyError then Exit;
        case AskQuestion('',
                         Format(rsMsgErrCannotCopyFile, [WrapTextSimple(SourceFile.FullPath, 64), WrapTextSimple(TargetFileName, 64)]) +
                         LineEnding + LineEnding + mbSysErrorMessage,
                         [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                         fsourRetry, fsourSkip) of
          fsourRetry:
            bRetryWrite := True;
          fsourAbort:
            AbortOperation;
          fsourSkip:
            Exit;
          fsourSkipAll:
            begin
              FSkipCopyError := True;
              Exit;
            end;
        end; // case
      end;
    until not bRetryWrite;
    if Result and FVerify then
    begin
      Result:= CompareFiles(SourceFile.FullPath, TargetFileName, SourceFile.Size);
    end;
    Exit;
  end;

  SourceFileStream := nil;
  TargetFileStream := nil; // for safety exception handling
  BytesToRead := FBufferSize;
  if FVerify then HashInit(Context, HASH_TYPE);
  try
    try
      OpenSourceFile;
      if not Assigned(SourceFileStream) then
        Exit;

      OpenTargetFile;
      if not Assigned(TargetFileStream) then
        Exit;

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

            if FVerify then HashUpdate(Context, FBuffer^, BytesRead);

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
                    GetDiskFreeSpace(ExtractFilePath(TargetFileName), iFreeDiskSize, iTotalDiskSize);
                    if BytesRead > iFreeDiskSize then
                      begin
                        case AskQuestion(rsMsgNoFreeSpaceRetry, '',
                                         [fsourYes, fsourNo, fsourSkip],
                                         fsourYes, fsourNo) of
                          fsourYes:
                            bRetryWrite := True;
                          fsourNo:
                            AbortOperation;
                          fsourSkip:
                            Exit;
                        end; // case
                      end
                    else
                      begin
                        DeleteFile := FSkipWriteError and not (Mode in [fsohcmAppend, fsohcmResume]);
                        if FSkipWriteError then Exit;
                        case AskQuestion(rsMsgErrEWrite + ' ' + TargetFileName + ':',
                                         E.Message,
                                         [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                                         fsourRetry, fsourSkip) of
                          fsourRetry:
                            bRetryWrite := True;
                          fsourAbort:
                            AbortOperation;
                          fsourSkip:
                            Exit;
                          fsourSkipAll:
                            begin
                              DeleteFile := not (Mode in [fsohcmAppend, fsohcmResume]);
                              FSkipWriteError := True;
                              Exit;
                            end;
                        end; // case
                      end;

                  end; // on do
              end; // except
            until not bRetryWrite;
          except
            on E: EReadError do
              begin
                DeleteFile := FSkipReadError and not (Mode in [fsohcmAppend, fsohcmResume]);
                if FSkipReadError then Exit;
                case AskQuestion(rsMsgErrERead + ' ' + SourceFile.FullPath + ':',
                                 E.Message,
                                 [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                                 fsourRetry, fsourSkip) of
                  fsourRetry:
                    bRetryRead := True;
                  fsourAbort:
                    AbortOperation;
                  fsourSkip:
                    Exit;
                  fsourSkipAll:
                    begin
                      DeleteFile := not (Mode in [fsohcmAppend, fsohcmResume]);
                      FSkipReadError := True;
                      Exit;
                    end;
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

        AppProcessMessages;
        CheckOperationState; // check pause and stop
      end;//while

      if FVerify then
      begin
        HashFinal(Context, Hash);
        TargetFileStream.Flush;
      end;

      Result:= True;

    except
      on EFileSourceOperationAborting do
      begin
        // Always delete file when user aborted operation.
        DeleteFile := True;
        raise;
      end;
    end;

  finally
    FreeAndNil(SourceFileStream);
    if FVerify then Context.Free;
    if Assigned(TargetFileStream) then
    begin
      FreeAndNil(TargetFileStream);
      if TotalBytesToRead > 0 then
      begin
        // There was some error, because not all of the file has been copied.
        // Ask if delete the not completed target file.
        if DeleteFile or
           (AskQuestion('', rsMsgDeletePartiallyCopied,
                        [fsourYes, fsourNo], fsourYes, fsourNo) = fsourYes) then
        begin
          DeleteFileUAC(TargetFileName);
        end;
      end;
      if Result and FVerify then begin
        Result:= CheckFileHash(TargetFileName, Hash, SourceFile.Size);
      end;
    end;
  end;

  if Result then CopyProperties(SourceFile, TargetFileName);
end;

procedure TFileSystemOperationHelper.CopyProperties(SourceFile: TFile;
  TargetFileName: String);
var
  Msg: String = '';
  ACopyTime: Boolean;
  CopyAttrResult: TCopyAttributesOptions = [];
  ACopyAttributesOptions: TCopyAttributesOptions;
begin
  if FCopyAttributesOptions <> [] then
  begin
    ACopyAttributesOptions := FCopyAttributesOptions;
    ACopyTime := (FMode = fsohmMove) and (caoCopyTime in ACopyAttributesOptions);
    if ACopyTime then ACopyAttributesOptions -= [caoCopyTime];
    if ACopyAttributesOptions <> [] then begin
      CopyAttrResult := FileCopyAttrUAC(SourceFile.FullPath, TargetFileName, ACopyAttributesOptions);
    end;
    if ACopyTime then
    try
      // Copy time from properties because move operation change time of original folder
      if not FileSetTimeUAC(TargetFileName, DateTimeToFileTime(SourceFile.ModificationTime),
                    {$IF DEFINED(MSWINDOWS)}DateTimeToFileTime(SourceFile.CreationTime){$ELSE}0{$ENDIF},
                                            DateTimeToFileTime(SourceFile.LastAccessTime)) then
        CopyAttrResult += [caoCopyTime];
    except
      on E: EDateOutOfRange do CopyAttrResult += [caoCopyTime];
    end;
    if CopyAttrResult <> [] then
    begin
      case FSetPropertyError of
        fsoospeIgnoreErrors: ; // Do nothing
        fsoospeDontSet:
          FCopyAttributesOptions := FCopyAttributesOptions - CopyAttrResult;
        fsoospeNone:
          begin
            if caoCopyAttributes in CopyAttrResult then
              AddStrWithSep(Msg, Format(rsMsgErrSetAttribute, [SourceFile.FullPath]), LineEnding);
            if caoCopyTime in CopyAttrResult then
              AddStrWithSep(Msg, Format(rsMsgErrSetDateTime, [SourceFile.FullPath]), LineEnding);
            if caoCopyOwnership in CopyAttrResult then
              AddStrWithSep(Msg, Format(rsMsgErrSetOwnership, [SourceFile.FullPath]), LineEnding);
            if caoCopyPermissions in CopyAttrResult then
              AddStrWithSep(Msg, Format(rsMsgErrSetPermissions, [SourceFile.FullPath]), LineEnding);

            case AskQuestion(Msg, '',
                             [fsourSkip, fsourSkipAll, fsourIgnoreAll, fsourAbort],
                             fsourSkip, fsourIgnoreAll) of
              //fsourSkip: do nothing
              fsourSkipAll:
                // Don't set properties that failed to be set anymore.
                FCopyAttributesOptions := FCopyAttributesOptions - CopyAttrResult;
              fsourIgnoreAll:
                FSetPropertyError := fsoospeIgnoreErrors;
              fsourAbort:
                AbortOperation;
            end;
          end
        else
          Assert(False, 'Invalid TFileSourceOperationOptionSetPropertyError value.');
      end;
    end;
  end;
end;

function TFileSystemOperationHelper.MoveFile(SourceFile: TFile; TargetFileName: String;
  Mode: TFileSystemOperationHelperCopyMode): Boolean;
var
  Message: String;
  RetryRename: Boolean;
  RetryDelete: Boolean;
begin
  if not (Mode in [fsohcmAppend, fsohcmResume]) then
  begin
    repeat
      RetryRename := True;
      if RenameFileUAC(SourceFile.FullPath, TargetFileName) then
        Exit(True);
      if (GetLastOSError <> ERROR_NOT_SAME_DEVICE) then
      begin
        if FSkipRenameError then Exit(False);
        Message := Format(rsMsgErrCannotMoveFile, [WrapTextSimple(SourceFile.FullPath, 100)]) +
                   LineEnding + LineEnding + mbSysErrorMessage;
        case AskQuestion('', Message, [fsourSkip, fsourRetry, fsourAbort, fsourSkipAll], fsourSkip, fsourAbort) of
          fsourSkip: Exit(False);
          fsourAbort: AbortOperation;
          fsourRetry: RetryRename := False;
          fsourSkipAll: FSkipRenameError := True;
        end;
      end;
    until RetryRename;
  end;

  if FVerify then FStatistics.TotalBytes += SourceFile.Size;
  if CopyFile(SourceFile, TargetFileName, Mode) then
  begin
    repeat
      RetryDelete := True;
      if FileIsReadOnly(SourceFile.Attributes) then
        FileSetReadOnlyUAC(SourceFile.FullPath, False);
      Result := DeleteFileUAC(SourceFile.FullPath);
      if (not Result) and (FDeleteFileOption = fsourInvalid) then
      begin
        Message := Format(rsMsgNotDelete, [WrapTextSimple(SourceFile.FullPath, 100)]) + LineEnding + LineEnding + mbSysErrorMessage;
        case AskQuestion('', Message, [fsourSkip, fsourRetry, fsourAbort, fsourSkipAll], fsourSkip, fsourAbort) of
          fsourAbort: AbortOperation;
          fsourRetry: RetryDelete := False;
          fsourSkipAll: FDeleteFileOption := fsourSkipAll;
        end;
      end;
    until RetryDelete;
  end
  else
    Result := False;
end;

function TFileSystemOperationHelper.ProcessNode(aFileTreeNode: TFileTreeNode;
                                                CurrentTargetPath: String): Boolean;
var
  aFile: TFile;
  TargetName: String;
  ProcessedOk: Boolean;
  CurrentFileIndex: Integer;
  CurrentSubNode: TFileTreeNode;
  AskResult: TFileSourceOperationUIResponse;
begin
  Result := True;

  for CurrentFileIndex := 0 to aFileTreeNode.SubNodesCount - 1 do
  begin
    CurrentSubNode := aFileTreeNode.SubNodes[CurrentFileIndex];
    aFile := CurrentSubNode.TheFile;

    if FRenamingRootDir and (aFile = FRootDir) then
      TargetName := CurrentTargetPath + FRenameMask
    else if FRenamingFiles then
      TargetName := CurrentTargetPath + ApplyRenameMask(aFile, FRenameNameMask, FRenameExtMask)
    else
      TargetName := CurrentTargetPath + aFile.Name;

    with FStatistics do
    begin
      CurrentFileFrom := aFile.FullPath;
      CurrentFileTo := TargetName;
      CurrentFileTotalBytes := aFile.Size;
      CurrentFileDoneBytes := 0;
    end;

    UpdateStatistics(FStatistics);

    // Check if moving to the same file.
    if mbFileSame(TargetName, aFile.FullPath) then
    begin
      if (FMode = fsohmCopy) and FAutoRenameItSelf then
        TargetName := GetNextCopyName(TargetName, aFile.IsDirectory or aFile.IsLinkToDirectory)
      else
        case AskQuestion(Format(rsMsgCanNotCopyMoveItSelf, [TargetName]), '',
                         [fsourAbort, fsourSkip], fsourAbort, fsourSkip) of
          fsourAbort:
            AbortOperation();
        else
            begin
              Result := False;
              CountStatistics(CurrentSubNode);
              AppProcessMessages;
              CheckOperationState;
              Continue;
            end;
        end;
    end;

    // Check MAX_PATH
    if UTF8Length(TargetName) > MAX_PATH - 1 then
    begin
      if FMaxPathOption <> fsourInvalid then
        AskResult := FMaxPathOption
      else begin
        AskResult := AskQuestion(Format(rsMsgFilePathOverMaxPath,
                         [UTF8Length(TargetName), MAX_PATH - 1, LineEnding + WrapTextSimple(TargetName, 100) + LineEnding]), '',
                         [fsourIgnore, fsourSkip, fsourAbort, fsourIgnoreAll, fsourSkipAll], fsourIgnore, fsourSkip);
      end;
      case AskResult of
        fsourAbort: AbortOperation();
        fsourSkip,
        fsourSkipAll:
          begin
            Result := False;
            FMaxPathOption := fsourSkip;
            CountStatistics(CurrentSubNode);
            AppProcessMessages;
            CheckOperationState;
            Continue;
          end;
        fsourIgnore: ;
        fsourIgnoreAll: FMaxPathOption := fsourIgnore;
      end;
    end;

    if aFile.IsLink then
      ProcessedOk := ProcessLink(CurrentSubNode, TargetName)
    else if aFile.IsDirectory then
      ProcessedOk := ProcessDirectory(CurrentSubNode, TargetName)
    else
      ProcessedOk := ProcessFile(CurrentSubNode, TargetName);

    if not ProcessedOk then
      Result := False
    // Process comments if need
    else if gProcessComments then
    begin
      case FMode of
        fsohmCopy:
          FDescription.CopyDescription(CurrentSubNode.TheFile.FullPath, TargetName);
        fsohmMove:
          FDescription.MoveDescription(CurrentSubNode.TheFile.FullPath, TargetName);
      end;
    end;

    AppProcessMessages;
    CheckOperationState;
  end;
end;

function TFileSystemOperationHelper.ProcessDirectory(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
var
  bRemoveDirectory: Boolean;
  NodeData: TFileTreeNodeData;
begin
  NodeData := aNode.Data as TFileTreeNodeData;

  // If some files will not be moved then source directory cannot be deleted.
  bRemoveDirectory := (FMode = fsohmMove) and (NodeData.SubnodesHaveExclusions = False);

  case TargetExists(aNode, AbsoluteTargetFileName) of
    fsoterSkip:
      begin
        Result := False;
        CountStatistics(aNode);
      end;

    fsoterDeleted, fsoterNotExists:
      begin
        // Try moving whole directory tree. It can be done only if we don't have
        // to process each subnode: if there are no links, or they're not being
        // processed, if the files are not being renamed or excluded.
        if (FMode = fsohmMove) and
           (not FRenamingFiles) and
           ((FCorrectSymlinks = False) or (NodeData.SubnodesHaveLinks = False)) and
           (NodeData.SubnodesHaveExclusions = False) and
           RenameFileUAC(aNode.TheFile.FullPath, AbsoluteTargetFileName) then
        begin
          // Success.
          CountStatistics(aNode);
          Result := True;
          bRemoveDirectory := False;
        end
        else
        begin
          // Create target directory.
          if CreateDirectoryUAC(AbsoluteTargetFileName) then
          begin
            // Copy/Move all files inside.
            Result := ProcessNode(aNode, IncludeTrailingPathDelimiter(AbsoluteTargetFileName));
            // Copy attributes after copy/move directory contents, because this operation can change date/time
            CopyProperties(aNode.TheFile, AbsoluteTargetFileName);
          end
          else
          begin
            // Error - all files inside not copied/moved.
            ShowError(rsMsgLogError + Format(rsMsgErrForceDir, [AbsoluteTargetFileName]));
            Result := False;
            CountStatistics(aNode);
          end;
        end;
      end;

    fsoterAddToTarget:
      begin
        // Don't create existing directory, but copy files into it.
        Result := ProcessNode(aNode, IncludeTrailingPathDelimiter(AbsoluteTargetFileName));
      end;

    else
      raise Exception.Create('Invalid TargetExists result');
  end;

  if bRemoveDirectory and Result then
  begin
    if FileIsReadOnly(aNode.TheFile.Attributes) then
      FileSetReadOnlyUAC(aNode.TheFile.FullPath, False);
    RemoveDirectoryUAC(aNode.TheFile.FullPath);
  end;
end;

function TFileSystemOperationHelper.ProcessLink(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
var
  LinkTarget, CorrectedLink: String;
  aFile: TFile;
  aSubNode: TFileTreeNode;
begin
  Result := True;

  // If link was followed then it's target is stored in a subnode.
  if aNode.SubNodesCount > 0 then
  begin
    aSubNode := aNode.SubNodes[0];
    //DCDebug('Link ' + aFile.FullPath + ' followed to '
    //        + (aSubNode.TheFile as TFileSystemFile).FullPath
    //        + ' will be copied as: ' + AbsoluteTargetFileName);
    if aSubNode.TheFile.AttributesProperty.IsDirectory then
      Result := ProcessDirectory(aSubNode, AbsoluteTargetFileName)
    else
      Result := ProcessFile(aSubNode, AbsoluteTargetFileName);

    Exit; // exit without counting statistics, because they are not counted for followed links
  end;

  aFile := aNode.TheFile;

  case TargetExists(aNode, AbsoluteTargetFileName) of
    fsoterSkip:
      Result := False;

    fsoterDeleted, fsoterNotExists:
      begin
        if (FMode <> fsohmMove) or
           (not RenameFileUAC(aFile.FullPath, AbsoluteTargetFileName)) then
        begin
          LinkTarget := ReadSymLink(aFile.FullPath);     // use sLinkTo ?
          if LinkTarget <> '' then
          begin
            if FCorrectSymlinks then
            begin
              CorrectedLink := GetAbsoluteFileName(aFile.Path, LinkTarget);

              // If the link was relative - make also the corrected link relative.
              if GetPathType(LinkTarget) = ptRelative then
                LinkTarget := ExtractRelativepath(AbsoluteTargetFileName, CorrectedLink)
              else
                LinkTarget := CorrectedLink;
            end;

            if CreateSymbolicLinkUAC(LinkTarget, AbsoluteTargetFileName) then
            begin
              CopyProperties(aFile, AbsoluteTargetFileName);
            end
            else
            begin
              ShowError(rsMsgLogError + Format(rsMsgLogSymLink, [AbsoluteTargetFileName]));
              Result := False;
            end;
          end
          else
          begin
            DCDebug('Error reading link');
            Result := False;
          end;
        end;
      end;

    fsoterAddToTarget:
      raise Exception.Create('Cannot add to link');

    else
      raise Exception.Create('Invalid TargetExists result');
  end;

  Inc(FStatistics.DoneFiles);
  UpdateStatistics(FStatistics);
end;

function TFileSystemOperationHelper.ProcessFile(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
var
  OldDoneBytes: Int64; // for if there was an error
begin
  // If there will be an error the DoneBytes value
  // will be inconsistent, so remember it here.
  OldDoneBytes := FStatistics.DoneBytes;

  // Skip descript.ion, it will be processed below
  if gProcessComments and (FStatistics.TotalFiles > 1) and mbCompareFileNames(aNode.TheFile.Name, DESCRIPT_ION) then
    Result:= True
  else begin
    Result:= False;

{$IF DEFINED(UNIX)}
    if not fpS_ISREG(aNode.TheFile.Attributes) then
    begin
      if FSkipAllSpecialFiles then Exit(False);

      case AskQuestion('', Format(rsMsgCannotCopySpecialFile, [LineEnding + aNode.TheFile.FullPath]),
                       [fsourSkip, fsourSkipAll, fsourAbort],
                       fsourSkip, fsourAbort) of
        fsourSkip:
          Exit(False);
        fsourSkipAll:
          begin
            FSkipAllSpecialFiles:= True;
            Exit(False);
          end
        else
          AbortOperation;
        end;
    end;
{$ENDIF}

    if (aNode.TheFile.Size > GetDiskMaxFileSize(ExtractFileDir(AbsoluteTargetFileName))) then
      case AskQuestion('', Format(rsMsgFileSizeTooBig, [aNode.TheFile.Name]),
                       [fsourSkip, fsourAbort],
                       fsourSkip, fsourAbort) of
        fsourSkip:
          Result := False;
        else
          AbortOperation;
      end
    else
      case TargetExists(aNode, AbsoluteTargetFileName) of
        fsoterSkip:
          Result := False;

        fsoterDeleted, fsoterNotExists:
          Result := MoveOrCopy(aNode.TheFile, AbsoluteTargetFileName, fsohcmDefault);

        fsoterAddToTarget:
          Result := MoveOrCopy(aNode.TheFile, AbsoluteTargetFileName, fsohcmAppend);

        fsoterResume:
          Result := MoveOrCopy(aNode.TheFile, AbsoluteTargetFileName, fsohcmResume);

        else
          raise Exception.Create('Invalid TargetExists result');
      end;
  end;

  if Result = True then
    begin
      LogMessage(Format(rsMsgLogSuccess+FLogCaption, [aNode.TheFile.FullPath + ' -> ' + AbsoluteTargetFileName]),
                 [log_cp_mv_ln], lmtSuccess);
    end
  else
    begin
      LogMessage(Format(rsMsgLogError+FLogCaption, [aNode.TheFile.FullPath + ' -> ' + AbsoluteTargetFileName]),
                 [log_cp_mv_ln], lmtError);
    end;

  with FStatistics do
  begin
    DoneFiles := DoneFiles + 1;
    DoneBytes := OldDoneBytes + aNode.TheFile.Size;
    UpdateStatistics(FStatistics);
  end;
end;

// ----------------------------------------------------------------------------

function TFileSystemOperationHelper.TargetExists(aNode: TFileTreeNode;
  var AbsoluteTargetFileName: String): TFileSystemOperationTargetExistsResult;
var
  Attrs, LinkTargetAttrs: TFileAttrs;
  SourceFile: TFile;

  function DoDirectoryExists(AllowCopyInto: Boolean; AllowDeleteDirectory: Boolean): TFileSystemOperationTargetExistsResult;
  begin
    case DirExists(SourceFile, AbsoluteTargetFileName, AllowCopyInto, AllowDeleteDirectory) of
      fsoodeSkip:
        Exit(fsoterSkip);
      fsoodeDelete:
        begin
          DeleteFileUAC(AbsoluteTargetFileName);
          Exit(fsoterDeleted);
        end;
      fsoodeCopyInto:
        begin
          Exit(fsoterAddToTarget);
        end;
      else
        raise Exception.Create('Invalid dir exists option');
    end;
  end;

  function DoFileExists(AllowAppend: Boolean): TFileSystemOperationTargetExistsResult;
  begin
    case FileExists(SourceFile, AbsoluteTargetFileName, AllowAppend) of
      fsoofeSkip:
        Exit(fsoterSkip);
      fsoofeOverwrite:
        begin
          if FileIsReadOnly(Attrs) then
            FileSetReadOnlyUAC(AbsoluteTargetFileName, False);
          if FPS_ISLNK(Attrs) or (FMode = fsohmMove) then
          begin
            DeleteFileUAC(AbsoluteTargetFileName);
            Exit(fsoterDeleted);
          end;
          Exit(fsoterNotExists);
        end;
      fsoofeAppend:
        begin
          Exit(fsoterAddToTarget);
        end;
      fsoofeResume:
        begin
          Exit(fsoterResume);
        end;
      fsoofeAutoRenameTarget,
      fsoofeAutoRenameSource:
        begin
          Exit(fsoterRenamed);
        end
      else
        raise Exception.Create('Invalid file exists option');
    end;
  end;

  function IsLinkFollowed: Boolean;
  begin
    // If link was followed then it's target is stored in a subnode.
    Result := SourceFile.AttributesProperty.IsLink and (aNode.SubNodesCount > 0);
  end;

  function AllowAppendFile: Boolean;
  begin
    Result := (not SourceFile.AttributesProperty.IsDirectory) and (not FReserveSpace) and
              ((not SourceFile.AttributesProperty.IsLink) or
               (IsLinkFollowed and (not aNode.SubNodes[0].TheFile.AttributesProperty.IsDirectory)));
  end;

  function AllowCopyInto: Boolean;
  begin
    Result := SourceFile.AttributesProperty.IsDirectory or
              (IsLinkFollowed and aNode.SubNodes[0].TheFile.IsDirectory);
  end;

begin
  repeat
    Attrs := FileGetAttrUAC(AbsoluteTargetFileName);
    if Attrs <> faInvalidAttributes then
    begin
      SourceFile := aNode.TheFile;

      // Target exists - ask user what to do.
      if FPS_ISLNK(Attrs) then
      begin
        // Check if target of the link exists.
        LinkTargetAttrs := FileGetAttrUAC(AbsoluteTargetFileName, True);
        if (LinkTargetAttrs <> faInvalidAttributes) then
        begin
          if FPS_ISDIR(LinkTargetAttrs) then
            Result := DoDirectoryExists(AllowCopyInto, False)
          else
            Result := DoFileExists(AllowAppendFile);
        end
        else
          // Target of link doesn't exist. Treat link as file and don't allow append.
          Result := DoFileExists(False);
      end
      else if FPS_ISDIR(Attrs) then
      begin
        Result := DoDirectoryExists(AllowCopyInto, False)
      end
      else
        // Existing target is a file.
        Result := DoFileExists(AllowAppendFile);
    end
    else
      Result := fsoterNotExists;
  until Result <> fsoterRenamed;
end;

function TFileSystemOperationHelper.DirExists(
             aFile: TFile;
             AbsoluteTargetFileName: String;
             AllowCopyInto: Boolean;
             AllowDelete: Boolean): TFileSourceOperationOptionDirectoryExists;
var
  Message: String;
  PossibleResponses: array of TFileSourceOperationUIResponse = nil;
  DefaultOkResponse: TFileSourceOperationUIResponse;

  procedure AddResponse(Response: TFileSourceOperationUIResponse);
  begin
    SetLength(PossibleResponses, Length(PossibleResponses) + 1);
    PossibleResponses[Length(PossibleResponses) - 1] := Response;
  end;

begin
  if (FDirExistsOption = fsoodeNone) or
     ((FDirExistsOption = fsoodeDelete) and (AllowDelete = False)) or
     ((FDirExistsOption = fsoodeCopyInto) and (AllowCopyInto = False)) then
    begin
      if AllowDelete then
        AddResponse(fsourOverwrite);
      if AllowCopyInto then
      begin
        AddResponse(fsourCopyInto);
        AddResponse(fsourCopyIntoAll);
      end;
      AddResponse(fsourSkip);
      if AllowDelete then
        AddResponse(fsourOverwriteAll);
      if AllowCopyInto or AllowDelete then
        AddResponse(fsourSkipAll);
      AddResponse(fsourCancel);

      if AllowCopyInto then
        DefaultOkResponse := fsourCopyInto
      else if AllowDelete then
        DefaultOkResponse := fsourOverwrite
      else
        DefaultOkResponse := fsourSkip;

      if AllowCopyInto or AllowDelete then
        Message:= Format(rsMsgFolderExistsRwrt, [AbsoluteTargetFileName])
      else begin
        Message:= Format(rsMsgCannotOverwriteDirectory, [AbsoluteTargetFileName, aFile.FullPath]);
      end;

      case AskQuestion(Message, '',
                       PossibleResponses, DefaultOkResponse, fsourSkip) of
        fsourOverwrite:
          Result := fsoodeDelete;
        fsourCopyInto:
          Result := fsoodeCopyInto;
        fsourCopyIntoAll:
          begin
            FDirExistsOption := fsoodeCopyInto;
            Result := fsoodeCopyInto;
          end;
        fsourSkip:
          Result := fsoodeSkip;
        fsourOverwriteAll:
          begin
            FDirExistsOption := fsoodeDelete;
            Result := fsoodeDelete;
          end;
        fsourSkipAll:
          begin
            FDirExistsOption := fsoodeSkip;
            Result := fsoodeSkip;
          end;
        fsourNone,
        fsourCancel:
          AbortOperation;
      end;
    end

    else
      Result := FDirExistsOption;
end;

procedure TFileSystemOperationHelper.QuestionActionHandler(
  Action: TFileSourceOperationUIAction);
begin
  if Action = fsouaCompare then
    ShowCompareFilesUI(FCurrentFile, FCurrentTargetFilePath);
end;

function TFileSystemOperationHelper.FileExists(aFile: TFile;
  var AbsoluteTargetFileName: String; AllowAppend: Boolean
  ): TFileSourceOperationOptionFileExists;
const
  Responses: array[0..13] of TFileSourceOperationUIResponse
    = (fsourOverwrite, fsourSkip, fsourRenameSource, fsourOverwriteAll,
       fsourSkipAll, fsourResume, fsourOverwriteOlder, fsourCancel,
       fsouaCompare, fsourAppend, fsourOverwriteSmaller, fsourOverwriteLarger,
       fsourAutoRenameSource, fsourAutoRenameTarget);
  ResponsesNoAppend: array[0..11] of TFileSourceOperationUIResponse
    = (fsourOverwrite, fsourSkip, fsourRenameSource,  fsourOverwriteAll,
       fsourSkipAll, fsourOverwriteSmaller, fsourOverwriteOlder, fsourCancel,
       fsouaCompare, fsourOverwriteLarger, fsourAutoRenameSource, fsourAutoRenameTarget);
var
  Answer: Boolean;
  Message: String;
  PossibleResponses: array of TFileSourceOperationUIResponse;

  function RenameTarget: TFileSourceOperationOptionFileExists;
  var
    bRetry: Boolean;
  begin
    repeat
      Message:= GetNextCopyName(AbsoluteTargetFileName, aFile.IsDirectory or aFile.IsLinkToDirectory);

      if RenameFileUAC(AbsoluteTargetFileName, Message) then
        Exit(fsoofeAutoRenameTarget);

      bRetry:= False;
      Message:= Format(rsMsgErrRename, [AbsoluteTargetFileName, Message]);
      case AskQuestion(Message, '',
                       [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort], fsourRetry, fsourSkip) of
        fsourRetry:
          bRetry:= True;
        fsourSkip:
          Result := fsoofeSkip;
        fsourSkipAll:
          begin
            FFileExistsOption := fsoofeSkip;
            Result := fsoofeSkip;
          end;
        fsourNone,
        fsourAbort:
          AbortOperation;
      end;
    until not bRetry;
  end;

  function OverwriteOlder: TFileSourceOperationOptionFileExists;
  begin
    if aFile.ModificationTime > FileTimeToDateTime(mbFileAge(AbsoluteTargetFileName)) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

  function OverwriteSmaller: TFileSourceOperationOptionFileExists;
  begin
    if aFile.Size > mbFileSize(AbsoluteTargetFileName) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

  function OverwriteLarger: TFileSourceOperationOptionFileExists;
  begin
    if aFile.Size < mbFileSize(AbsoluteTargetFileName) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

begin
  case FFileExistsOption of
    fsoofeNone:
      repeat
        Answer := True;
        case AllowAppend of
          True :  PossibleResponses := Responses;
          False:  PossibleResponses := ResponsesNoAppend;
        end;
        Message:= FileExistsMessage(AbsoluteTargetFileName, aFile.FullPath,
                                    aFile.Size, aFile.ModificationTime);
        FCurrentFile := aFile;
        FCurrentTargetFilePath := AbsoluteTargetFileName;
        case AskQuestion(Message, '',
                         PossibleResponses, fsourOverwrite, fsourSkip,
                         @QuestionActionHandler) of
          fsourOverwrite:
            Result := fsoofeOverwrite;
          fsourSkip:
            Result := fsoofeSkip;
          fsourAppend:
            begin
              //FFileExistsOption := fsoofeAppend; - for AppendAll
              Result := fsoofeAppend;
            end;
          fsourResume:
            begin
              Result := fsoofeResume;
            end;
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
          fsourAutoRenameSource:
            begin
              Result:= fsoofeAutoRenameSource;
              FFileExistsOption:= fsoofeAutoRenameSource;
              AbsoluteTargetFileName:= GetNextCopyName(AbsoluteTargetFileName, aFile.IsDirectory or aFile.IsLinkToDirectory);
            end;
          fsourAutoRenameTarget:
            begin
              FFileExistsOption := fsoofeAutoRenameTarget;
              Result:= RenameTarget;
            end;
          fsourRenameSource:
            begin
              Message:= ExtractFileName(AbsoluteTargetFileName);
              Answer:= ShowInputQuery(FOperationThread, Application.Title, rsEditNewFileName, Message);
              if Answer then
              begin
                Result:= fsoofeAutoRenameSource;
                AbsoluteTargetFileName:= ExtractFilePath(AbsoluteTargetFileName) + Message;
              end;
            end;
          fsourNone,
          fsourCancel:
            AbortOperation;
        end;
        until Answer;
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
    fsoofeAutoRenameTarget:
      begin
        Result:= RenameTarget;
      end;
    fsoofeAutoRenameSource:
      begin
        Result:= fsoofeAutoRenameSource;
        AbsoluteTargetFileName:= GetNextCopyName(AbsoluteTargetFileName, aFile.IsDirectory or aFile.IsLinkToDirectory);
      end;

    else
      Result := FFileExistsOption;
  end;
end;

procedure TFileSystemOperationHelper.ShowError(sMessage: String);
begin
  if gSkipFileOpError then
  begin
    if log_errors in gLogOptions then
      logWrite(FOperationThread, sMessage, lmtError, True);
  end
  else
  begin
    if AskQuestion(sMessage, '', [fsourSkip, fsourAbort],
                   fsourSkip, fsourAbort) <> fsourSkip then
    begin
      AbortOperation;
    end;
  end;
end;

procedure TFileSystemOperationHelper.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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
    logWrite(FOperationThread, sMessage, logMsgType);
  end;
end;

function TFileSystemOperationHelper.CheckFileHash(const FileName, Hash: String;
  Size: Int64): Boolean;
const
  BLOCK_SIZE = $20000;
var
  Handle: THandle;
  FileHash: String;
  bRetryRead: Boolean;
  Context: THashContext;
  Buffer, Aligned: Pointer;
  TotalBytesToRead: Int64 = 0;
  BytesRead, BytesToRead: Int64;
begin
  Result := False;
  FStatistics.CurrentFileDoneBytes:= 0;
  // Flag fmOpenDirect requires: file access sizes must be for a number of bytes
  // that is an integer multiple of the volume block size, file access buffer
  // addresses for read and write operations should be physical block size aligned
  BytesToRead:= BLOCK_SIZE;
  Buffer:= GetMem(BytesToRead * 2 - 1);
  {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
  Aligned:= Pointer(PtrUInt(Buffer + BytesToRead - 1) and not (BytesToRead - 1));
  {$POP}
  HashInit(Context, HASH_TYPE);
  try
    Handle:= FileOpenUAC(FileName, fmOpenRead or fmShareDenyWrite or fmOpenSync or fmOpenDirect);

    if Handle = feInvalidHandle then
    begin
      case AskQuestion(rsMsgVerify, rsMsgErrEOpen + ' ' + FileName,
                       [fsourSkip, fsourAbort],
                       fsourAbort, fsourSkip) of
        fsourAbort:
          AbortOperation();
        fsourSkip:
          Exit(False);
      end; // case
    end
    else begin
      TotalBytesToRead := Size;

      while TotalBytesToRead > 0 do
      begin
        repeat
          try
            bRetryRead := False;
            BytesRead := FileRead(Handle, Aligned^, BytesToRead);

            if (BytesRead <= 0) then
              Raise EReadError.Create(mbSysErrorMessage(GetLastOSError));

            TotalBytesToRead := TotalBytesToRead - BytesRead;

            HashUpdate(Context, Aligned^, BytesRead);

          except
            on E: EReadError do
              begin
                case AskQuestion(rsMsgVerify + ' ' + rsMsgErrERead + ' ' + FileName + LineEnding,
                                 E.Message,
                                 [fsourRetry, fsourSkip, fsourAbort],
                                 fsourRetry, fsourSkip) of
                  fsourRetry:
                    bRetryRead := True;
                  fsourAbort:
                    AbortOperation();
                  fsourSkip:
                    Exit(False);
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
      end; // while

      Result := True;
    end;
  finally
    FreeMem(Buffer);
    HashFinal(Context, FileHash);
    if Handle <> feInvalidHandle then
    begin
      FileClose(Handle);
    end;
    if Result then
    begin
      Result:= SameText(Hash, FileHash);
      if not Result then
      begin
        case AskQuestion(rsMsgVerify, rsMsgVerifyWrong + LineEnding + FileName,
                         [fsourSkip, fsourAbort],
                         fsourAbort, fsourSkip) of
          fsourAbort:
            AbortOperation();
        end; // case
      end;
    end;
  end;
end;

function TFileSystemOperationHelper.CompareFiles(const FileName1, FileName2: String;
  Size: Int64): Boolean;
const
  BLOCK_SIZE = $20000;
  BUF_LEN = 1024 * 1024 * 8;
var
  Count: Int64;
  Buffer1, Buffer2: PByte;
  Aligned1, Aligned2: PByte;
  File1, File2: TFileStreamUAC;
begin
  Buffer1:= GetMem(BUF_LEN * 2);
  Buffer2:= GetMem(BUF_LEN * 2);
  try
    if (Buffer1 = nil) or (Buffer2 = nil) then
      raise EOutOfMemory.Create(SOutOfMemory);

    Aligned1:= Align(Buffer1, BLOCK_SIZE);
    Aligned2:= Align(Buffer2, BLOCK_SIZE);
    try
      File1 := TFileStreamUAC.Create(FileName1, fmOpenRead or fmShareDenyWrite or fmOpenSync or fmOpenDirect);
      try
        File2 := TFileStreamUAC.Create(FileName2, fmOpenRead or fmShareDenyWrite or fmOpenSync or fmOpenDirect);
        try
          FStatistics.CurrentFileDoneBytes:= 0;

          repeat
            if Size - FStatistics.CurrentFileDoneBytes <= BUF_LEN then
              Count := Size - FStatistics.CurrentFileDoneBytes
            else begin
              Count := BUF_LEN;
            end;

            File1.ReadBuffer(Aligned1^, Count);
            File2.ReadBuffer(Aligned2^, Count);

            if (Count <> BUF_LEN) then
              Result := CompareMem(Aligned1, Aligned2, Count)
            else begin
              Result := CompareDWord(Aligned1^, Aligned2^, Count div SizeOf(Dword)) = 0;
            end;

            with FStatistics do
            begin
              DoneBytes += Count;
              CurrentFileDoneBytes += Count;
              UpdateStatistics(FStatistics);
            end;

            CheckOperationState; // check pause and stop

          until not Result or (FStatistics.CurrentFileDoneBytes >= Size);
        finally
          File2.Free;
        end;
      finally
        File1.Free;
      end;
    except
      on E: Exception do
      begin
        if E is EFileSourceOperationAborting then
          raise;

        case AskQuestion(rsMsgVerify, E.Message,
                         [fsourSkip, fsourAbort],
                         fsourAbort, fsourSkip) of
          fsourAbort:
            AbortOperation();
          fsourSkip:
            Exit(False);
        end; // case
      end;
    end;
  finally
    if Assigned(Buffer1) then FreeMem(Buffer1);
    if Assigned(Buffer2) then FreeMem(Buffer2);
  end;
end;

procedure TFileSystemOperationHelper.CountStatistics(aNode: TFileTreeNode);
  procedure CountNodeStatistics(aNode: TFileTreeNode);
  var
    aFileAttrs: TFileAttributesProperty;
    i: Integer;
  begin
    aFileAttrs := aNode.TheFile.AttributesProperty;

    with FStatistics do
    begin
      if aFileAttrs.IsDirectory then
      begin
        // No statistics for directory.
        // Go through subdirectories.
        for i := 0 to aNode.SubNodesCount - 1 do
          CountNodeStatistics(aNode.SubNodes[i]);
      end
      else if aFileAttrs.IsLink then
      begin
        // Count only not-followed links.
        if aNode.SubNodesCount = 0 then
          DoneFiles := DoneFiles + 1
        else
          // Count target of link.
          CountNodeStatistics(aNode.SubNodes[0]);
      end
      else
      begin
        // Count files.
        DoneFiles := DoneFiles + 1;
        DoneBytes := DoneBytes + aNode.TheFile.Size;
      end;
    end;
  end;

begin
  CountNodeStatistics(aNode);
  UpdateStatistics(FStatistics);
end;

end.

