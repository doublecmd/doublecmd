unit uWfxPluginUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, DCOSUtils, uLog, uGlobs,
  WfxPlugin, uWfxModule,
  uFile,
  uFileSource,
  uFileSourceOperation,
  uFileSourceTreeBuilder,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFileSourceCopyOperation,
  uWfxPluginFileSource;

type

  TWfxPluginOperationHelperMode =
    (wpohmCopy, wpohmCopyIn, wpohmCopyOut, wpohmMove);

  TUpdateStatisticsFunction = procedure(var NewStatistics: TFileSourceCopyOperationStatistics) of object;

  { TWfxTreeBuilder }

  TWfxTreeBuilder = class(TFileSourceTreeBuilder)
  private
    FWfxModule: TWfxModule;
  protected
    procedure AddLinkTarget(aFile: TFile; CurrentNode: TFileTreeNode); override;
    procedure AddFilesInDirectory(srcPath: String; CurrentNode: TFileTreeNode); override;
  public
    property WfxModule: TWfxModule read FWfxModule write FWfxModule;
  end;

  { TWfxPluginOperationHelper }

  TWfxPluginOperationHelper = class
  private
    FRootDir: TFile;
    FWfxPluginFileSource: IWfxPluginFileSource;
    FOperationThread: TThread;
    FMode: TWfxPluginOperationHelperMode;
    FRootTargetPath: String;
    FRenameMask: String;
    FRenameNameMask, FRenameExtMask: String;
    FLogCaption: String;
    FRenamingFiles,
    FRenamingRootDir,
    FInternal: Boolean;
    FStatistics: PFileSourceCopyOperationStatistics;
    FCopyAttributesOptions: TCopyAttributesOptions;
    FFileExistsOption: TFileSourceOperationOptionFileExists;

    FCurrentFile: TFile;
    FCurrentTargetFile: TFile;
    FCurrentTargetFilePath: String;

    AskQuestion: TAskQuestionFunction;
    AbortOperation: TAbortOperationFunction;
    CheckOperationState: TCheckOperationStateFunction;
    UpdateStatistics: TUpdateStatisticsFunction;
    ShowCompareFilesUI: TShowCompareFilesUIFunction;
    ShowCompareFilesUIByFileObject: TShowCompareFilesUIByFileObjectFunction;

    procedure ShowError(sMessage: String);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

    function ProcessNode(aFileTreeNode: TFileTreeNode; CurrentTargetPath: String): Integer;

    function ProcessDirectory(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Integer;
    function ProcessLink(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Integer;
    function ProcessFile(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Integer;

    procedure QuestionActionHandler(Action: TFileSourceOperationUIAction);
    function FileExists(aFile: TFile;
                        AbsoluteTargetFileName: String;
                        AllowResume: Boolean): TFileSourceOperationOptionFileExists;

    procedure CopyProperties(SourceFile: TFile; const TargetFileName: String);

    procedure CountStatistics(aNode: TFileTreeNode);

  public
    constructor Create(FileSource: IFileSource;
                       AskQuestionFunction: TAskQuestionFunction;
                       AbortOperationFunction: TAbortOperationFunction;
                       CheckOperationStateFunction: TCheckOperationStateFunction;
                       UpdateStatisticsFunction: TUpdateStatisticsFunction;
                       ShowCompareFilesUIFunction: TShowCompareFilesUIFunction;
                       ShowCompareFilesUIByFileObjectFunction: TShowCompareFilesUIByFileObjectFunction;
                       OperationThread: TThread;
                       Mode: TWfxPluginOperationHelperMode;
                       TargetPath: String
                       );
    destructor Destroy; override;

    procedure Initialize;

    procedure ProcessTree(aFileTree: TFileTree; var Statistics: TFileSourceCopyOperationStatistics);

    property FileExistsOption: TFileSourceOperationOptionFileExists read FFileExistsOption write FFileExistsOption;
    property CopyAttributesOptions: TCopyAttributesOptions read FCopyAttributesOptions write FCopyAttributesOptions;
    property RenameMask: String read FRenameMask write FRenameMask;
  end;

  function WfxRenameFile(aFileSource: IWfxPluginFileSource; const aFile: TFile; const NewFileName: String): Boolean;

  function WfxFileTimeToDateTime(FileTime : TWfxFileTime) : TDateTime; inline;
  function DateTimeToWfxFileTime(DateTime : TDateTime) : TWfxFileTime; inline;

implementation

uses
  uFileProcs, StrUtils, DCStrUtils, uLng, uFileSystemUtil, uFileProperty,
  DCDateTimeUtils, DCBasicTypes, DCFileAttributes;

function WfxRenameFile(aFileSource: IWfxPluginFileSource; const aFile: TFile; const NewFileName: String): Boolean;
var
  RemoteInfo: TRemoteInfo;
  iTemp: TInt64Rec;
begin
  with aFileSource do
  begin
    with RemoteInfo do
    begin
      iTemp.Value := (aFile.Properties[fpSize] as TFileSizeProperty).Value;
      SizeLow := iTemp.Low;
      SizeHigh := iTemp.High;
      LastWriteTime := DateTimeToWfxFileTime((aFile.Properties[fpModificationTime] as TFileModificationDateTimeProperty).Value);
      Attr := LongInt((aFile.Properties[fpAttributes] as TFileAttributesProperty).Value);
    end;
    Result := (WfxCopyMove(aFile.Path + aFile.Name, aFile.Path + NewFileName, FS_COPYFLAGS_MOVE, @RemoteInfo, True, True) = FS_FILE_OK);
  end;
end;

function WfxFileTimeToDateTime(FileTime: TWfxFileTime): TDateTime;
begin
  if (FileTime.dwLowDateTime = $FFFFFFFE) and (FileTime.dwHighDateTime = $FFFFFFFF) then
    Result:= Default(TDateTime)
  else
    Result:= WinFileTimeToDateTime(TWinFileTime(FileTime));
end;

function DateTimeToWfxFileTime(DateTime: TDateTime): TWfxFileTime;
begin
  Result:= TWfxFileTime(DateTimeToWinFileTime(DateTime));
end;

{ TWfxTreeBuilder }

procedure TWfxTreeBuilder.AddLinkTarget(aFile: TFile; CurrentNode: TFileTreeNode);
begin
  // Add as normal file/directory
  if aFile.AttributesProperty is TNtfsFileAttributesProperty then
    aFile.Attributes:= aFile.Attributes and (not FILE_ATTRIBUTE_REPARSE_POINT)
  else
    aFile.Attributes:= aFile.Attributes and (not S_IFLNK);

  if not aFile.IsLinkToDirectory then
    AddFile(aFile, CurrentNode)
  else begin
    if aFile.AttributesProperty is TNtfsFileAttributesProperty then
      aFile.Attributes:= aFile.Attributes or FILE_ATTRIBUTE_DIRECTORY
    else begin
      aFile.Attributes:= aFile.Attributes or S_IFDIR;
    end;
    AddDirectory(aFile, CurrentNode);
  end;
end;

procedure TWfxTreeBuilder.AddFilesInDirectory(srcPath: String;
  CurrentNode: TFileTreeNode);
var
  FindData: TWfxFindData;
  Handle: THandle;
  aFile: TFile;
begin
  with FWfxModule do
  begin
    Handle := WfxFindFirst(srcPath, FindData);
    if Handle = wfxInvalidHandle then Exit;

    repeat
      if (FindData.FileName = '.') or (FindData.FileName = '..') then Continue;
      aFile:= TWfxPluginFileSource.CreateFile(srcPath, FindData);
      AddItem(aFile, CurrentNode);
    until not WfxFindNext(Handle, FindData);

    FsFindClose(Handle);
  end;
end;

{ TWfxPluginOperationHelper }

procedure TWfxPluginOperationHelper.ShowError(sMessage: String);
begin
  if gSkipFileOpError then
  begin
    if log_errors in gLogOptions then
      logWrite(FOperationThread, sMessage, lmtError, True);
  end
  else
  begin
    if AskQuestion(sMessage, '', [fsourSkip, fsourAbort],
                   fsourSkip, fsourAbort) = fsourAbort then
    begin
      AbortOperation;
    end;
  end;
end;

procedure TWfxPluginOperationHelper.LogMessage(sMessage: String;
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
    logWrite(FOperationThread, sMessage, logMsgType);
  end;
end;

function TWfxPluginOperationHelper.ProcessNode(aFileTreeNode: TFileTreeNode;
  CurrentTargetPath: String): Integer;
var
  aFile: TFile;
  TargetName: String;
  ProcessedOk: Integer;
  CurrentFileIndex: Integer;
  CurrentSubNode: TFileTreeNode;
begin
  Result := FS_FILE_OK;

  for CurrentFileIndex := 0 to aFileTreeNode.SubNodesCount - 1 do
  begin
    CurrentSubNode := aFileTreeNode.SubNodes[CurrentFileIndex];
    aFile := CurrentSubNode.TheFile;

    if FRenamingRootDir and (aFile = FRootDir) then
      TargetName := FRenameMask
    else if FRenamingFiles then
      TargetName := ApplyRenameMask(aFile, FRenameNameMask, FRenameExtMask)
    else
      TargetName := aFile.Name;

    if FMode <> wpohmCopyOut then
      TargetName := CurrentTargetPath + TargetName
    else begin
      TargetName := CurrentTargetPath + ReplaceInvalidChars(TargetName);
    end;

    with FStatistics^ do
    begin
      CurrentFileFrom := aFile.FullPath;
      CurrentFileTo := TargetName;
      CurrentFileTotalBytes := aFile.Size;
      CurrentFileDoneBytes := 0;
    end;

    UpdateStatistics(FStatistics^);

    if aFile.IsLink then
      ProcessedOk := ProcessLink(CurrentSubNode, TargetName)
    else if aFile.IsDirectory then
      ProcessedOk := ProcessDirectory(CurrentSubNode, TargetName)
    else
      ProcessedOk := ProcessFile(CurrentSubNode, TargetName);

    if ProcessedOk <> FS_FILE_OK then
      Result := ProcessedOk;

    if ProcessedOk = FS_FILE_USERABORT then AbortOperation();

    if ProcessedOk = FS_FILE_OK then CopyProperties(aFile, TargetName);

    if ProcessedOk = FS_FILE_OK then
      begin
        LogMessage(Format(rsMsgLogSuccess+FLogCaption, [aFile.FullPath + ' -> ' + TargetName]),
                   [log_vfs_op], lmtSuccess);
      end
    else
      begin
        ShowError(Format(rsMsgLogError + FLogCaption,
                         [aFile.FullPath + ' -> ' + TargetName +
                          ' - ' + GetErrorMsg(ProcessedOk)]));
        LogMessage(Format(rsMsgLogError+FLogCaption, [aFile.FullPath + ' -> ' + TargetName]),
                   [log_vfs_op], lmtError);
      end;

    CheckOperationState;
  end;
end;

function TWfxPluginOperationHelper.ProcessDirectory(aNode: TFileTreeNode;
  AbsoluteTargetFileName: String): Integer;
begin
  // Create target directory
  if (FMode <> wpohmCopyOut) then
    Result:= FWfxPluginFileSource.WfxModule.WfxMkDir('', AbsoluteTargetFileName)
  else begin
    if mbForceDirectory(AbsoluteTargetFileName) then
      Result:= FS_FILE_OK
    else
      Result:= WFX_ERROR;
  end;
  if Result = FS_FILE_OK then
  begin
    // Copy/Move all files inside.
    Result := ProcessNode(aNode, IncludeTrailingPathDelimiter(AbsoluteTargetFileName));
  end
  else
  begin
    // Error - all files inside not copied/moved.
    ShowError(rsMsgLogError + Format(rsMsgErrForceDir, [AbsoluteTargetFileName]));
    CountStatistics(aNode);
  end;
  if (Result = FS_FILE_OK) and (FMode = wpohmMove) then
    FWfxPluginFileSource.WfxModule.WfxRemoveDir(aNode.TheFile.FullPath);
end;

function TWfxPluginOperationHelper.ProcessLink(aNode: TFileTreeNode;
  AbsoluteTargetFileName: String): Integer;
var
  aSubNode: TFileTreeNode;
begin
  if (FMode = wpohmMove) then
    Result := ProcessFile(aNode, AbsoluteTargetFileName)
  else if aNode.SubNodesCount > 0 then
  begin
    aSubNode := aNode.SubNodes[0];
    if aSubNode.TheFile.AttributesProperty.IsDirectory then
      Result := ProcessDirectory(aSubNode, AbsoluteTargetFileName)
    else
      Result := ProcessFile(aSubNode, AbsoluteTargetFileName);
  end;
end;

function TWfxPluginOperationHelper.ProcessFile(aNode: TFileTreeNode;
  AbsoluteTargetFileName: String): Integer;
var
  iFlags: Integer = 0;
  RemoteInfo: TRemoteInfo;
  iTemp: TInt64Rec;
  bCopyMoveIn: Boolean;
  aFile: TFile;
  OldDoneBytes: Int64; // for if there was an error
begin
  // If there will be an error the DoneBytes value
  // will be inconsistent, so remember it here.
  OldDoneBytes := FStatistics^.DoneBytes;

  aFile:= aNode.TheFile;

  with FWfxPluginFileSource do
  begin
      with RemoteInfo do
      begin
        iTemp.Value := aFile.Size;
        SizeLow := iTemp.Low;
        SizeHigh := iTemp.High;
        LastWriteTime := DateTimeToWfxFileTime(aFile.ModificationTime);
        Attr := LongInt(aFile.Attributes);
      end;
      if (FMode = wpohmMove) then
        iFlags:= iFlags + FS_COPYFLAGS_MOVE;
      if FFileExistsOption = fsoofeOverwrite then
        iFlags:= iFlags + FS_COPYFLAGS_OVERWRITE;
      bCopyMoveIn:= (FMode = wpohmCopyIn);

      Result := WfxCopyMove(aFile.Path + aFile.Name, AbsoluteTargetFileName, iFlags, @RemoteInfo, FInternal, bCopyMoveIn);

      case Result of
      FS_FILE_EXISTS, // The file already exists, and resume isn't supported
      FS_FILE_EXISTSRESUMEALLOWED: // The file already exists, and resume is supported
        begin
          case FileExists(aFile, AbsoluteTargetFileName, Result = FS_FILE_EXISTSRESUMEALLOWED) of
            fsoofeSkip:
              Exit(FS_FILE_OK);
            fsoofeOverwrite:
                iFlags:= iFlags + FS_COPYFLAGS_OVERWRITE;
            fsoofeResume:
                iFlags:= iFlags + FS_COPYFLAGS_RESUME;
          else
            raise Exception.Create('Invalid file exists option');
          end;
          Result := WfxCopyMove(aFile.Path + aFile.Name, AbsoluteTargetFileName, iFlags, @RemoteInfo, FInternal, bCopyMoveIn);
        end;
      end;
   end;

  with FStatistics^ do
  begin
    if Result = FS_FILE_OK then DoneFiles := DoneFiles + 1;
    DoneBytes := OldDoneBytes + aFile.Size;
    UpdateStatistics(FStatistics^);
  end;
end;

procedure TWfxPluginOperationHelper.QuestionActionHandler(
  Action: TFileSourceOperationUIAction);
begin
  if Action = fsouaCompare then
  begin
    if Assigned(FCurrentTargetFile) then
      ShowCompareFilesUIByFileObject(FCurrentFile, FCurrentTargetFile)
    else
      ShowCompareFilesUI(FCurrentFile, FCurrentTargetFilePath);
  end;
end;

function FileExistsMessage(TargetFile: TFile; SourceFile: TFile): String;
begin
  Result:= rsMsgFileExistsOverwrite + LineEnding + TargetFile.FullPath + LineEnding +
           Format(rsMsgFileExistsFileInfo, [Numb2USA(IntToStr(TargetFile.Size)), DateTimeToStr(TargetFile.ModificationTime)]) + LineEnding;
  Result:= Result + LineEnding + rsMsgFileExistsWithFile + LineEnding + SourceFile.FullPath + LineEnding +
           Format(rsMsgFileExistsFileInfo, [Numb2USA(IntToStr(SourceFile.Size)), DateTimeToStr(SourceFile.ModificationTime)]);
end;

function TWfxPluginOperationHelper.FileExists(aFile: TFile;
  AbsoluteTargetFileName: String; AllowResume: Boolean
  ): TFileSourceOperationOptionFileExists;
const
  Responses: array[0..6] of TFileSourceOperationUIResponse
    = (fsourOverwrite, fsourSkip, fsourResume, fsourOverwriteAll, fsourSkipAll,
       fsouaCompare, fsourCancel);
  ResponsesNoResume: array[0..5] of TFileSourceOperationUIResponse
    = (fsourOverwrite, fsourSkip, fsourOverwriteAll, fsourSkipAll, fsouaCompare,
       fsourCancel);
var
  Message: String;
  PossibleResponses: array of TFileSourceOperationUIResponse;
begin
  case FFileExistsOption of
    fsoofeNone:
      try
        FCurrentTargetFile := nil;
        case AllowResume of
          True :  PossibleResponses := Responses;
          False:  PossibleResponses := ResponsesNoResume;
        end;
        if FMode = wpohmCopyOut then
          Message := uFileSystemUtil.FileExistsMessage(AbsoluteTargetFileName, aFile.FullPath, aFile.Size, aFile.ModificationTime)
        else if FWfxPluginFileSource.FillSingleFile(AbsoluteTargetFileName, FCurrentTargetFile) then
          Message := FileExistsMessage(FCurrentTargetFile, aFile)
        else
          Message := Format(rsMsgFileExistsRwrt, [AbsoluteTargetFileName]);
        FCurrentFile := aFile;
        FCurrentTargetFilePath := AbsoluteTargetFileName;
        case AskQuestion(Message, '',
                         PossibleResponses, fsourOverwrite, fsourSkip,
                         @QuestionActionHandler) of
          fsourOverwrite:
            Result := fsoofeOverwrite;
          fsourSkip:
            Result := fsoofeSkip;
          fsourResume:
            begin
              // FFileExistsOption := fsoofeResume; - for ResumeAll
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
          fsourNone,
          fsourCancel:
            AbortOperation;
        end;
      finally
        FreeAndNil(FCurrentTargetFile);
      end;

    else
      Result := FFileExistsOption;
  end;
end;

procedure TWfxPluginOperationHelper.CopyProperties(SourceFile: TFile;
  const TargetFileName: String);
var
  WfxFileTime: TWfxFileTime;
begin
  if caoCopyTime in FCopyAttributesOptions then
  begin
    if (FMode = wpohmCopyOut) then
      mbFileSetTime(TargetFileName, DateTimeToFileTime(SourceFile.ModificationTime))
    else begin
      WfxFileTime := DateTimeToWfxFileTime(SourceFile.ModificationTime);
      FWfxPluginFileSource.WfxModule.WfxSetTime(TargetFileName, nil, nil, @WfxFileTime);
    end;
  end;
end;

procedure TWfxPluginOperationHelper.CountStatistics(aNode: TFileTreeNode);

  procedure CountNodeStatistics(aNode: TFileTreeNode);
  var
    aFileAttrs: TFileAttributesProperty;
    i: Integer;
  begin
    aFileAttrs := aNode.TheFile.AttributesProperty;

    with FStatistics^ do
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
  UpdateStatistics(FStatistics^);
end;

constructor TWfxPluginOperationHelper.Create(FileSource: IFileSource;
                                             AskQuestionFunction: TAskQuestionFunction;
                                             AbortOperationFunction: TAbortOperationFunction;
                                             CheckOperationStateFunction: TCheckOperationStateFunction;
                                             UpdateStatisticsFunction: TUpdateStatisticsFunction;
                                             ShowCompareFilesUIFunction: TShowCompareFilesUIFunction;
                                             ShowCompareFilesUIByFileObjectFunction: TShowCompareFilesUIByFileObjectFunction;
                                             OperationThread: TThread;
                                             Mode: TWfxPluginOperationHelperMode;
                                             TargetPath: String
                                             );
begin
  FWfxPluginFileSource:= FileSource as IWfxPluginFileSource;
  AskQuestion := AskQuestionFunction;
  AbortOperation := AbortOperationFunction;
  CheckOperationState := CheckOperationStateFunction;
  UpdateStatistics := UpdateStatisticsFunction;
  ShowCompareFilesUI := ShowCompareFilesUIFunction;
  ShowCompareFilesUIByFileObject := ShowCompareFilesUIByFileObjectFunction;
  FOperationThread:= OperationThread;
  FMode := Mode;
  FInternal:= (FMode in [wpohmCopy, wpohmMove]);

  FFileExistsOption := fsoofeNone;
  FRootTargetPath := TargetPath;
  FRenameMask := '';
  FRenamingFiles := False;
  FRenamingRootDir := False;

  inherited Create;
end;

destructor TWfxPluginOperationHelper.Destroy;
begin
  inherited Destroy;
end;

procedure TWfxPluginOperationHelper.Initialize;
begin
  case FMode of
  wpohmCopy,
  wpohmCopyIn,
  wpohmCopyOut:
    FLogCaption := rsMsgLogCopy;
  wpohmMove:
    FLogCaption := rsMsgLogMove;
  end;

  SplitFileMask(FRenameMask, FRenameNameMask, FRenameExtMask);
end;

procedure TWfxPluginOperationHelper.ProcessTree(aFileTree: TFileTree;
  var Statistics: TFileSourceCopyOperationStatistics);
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

  FStatistics:= @Statistics;

  ProcessNode(aFileTree, FRootTargetPath);
end;

end.

