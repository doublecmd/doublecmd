unit uGioFileSourceUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCStrUtils, uFile, uFileSource, uFileSourceOperation,
  uFileSourceCopyOperation, uFileSystemUtil, uFileSourceOperationOptions,
  uFileSourceTreeBuilder, uGioFileSource, uGLib2, uGio2, uLog, uGlobs,
  uFileSourceOperationUI;

const
  CONST_DEFAULT_QUERY_INFO_ATTRIBUTES = FILE_ATTRIBUTE_STANDARD_TYPE + ',' + FILE_ATTRIBUTE_STANDARD_NAME + ',' +
                                        FILE_ATTRIBUTE_STANDARD_DISPLAY_NAME + ',' + FILE_ATTRIBUTE_STANDARD_SIZE + ',' +
                                        FILE_ATTRIBUTE_STANDARD_SYMLINK_TARGET + ',' + FILE_ATTRIBUTE_TIME_MODIFIED + ',' +
                                        FILE_ATTRIBUTE_TIME_ACCESS + ',' + FILE_ATTRIBUTE_TIME_CREATED + ',' +
                                        FILE_ATTRIBUTE_UNIX_MODE + ',' + FILE_ATTRIBUTE_UNIX_UID + ',' +
                                        FILE_ATTRIBUTE_UNIX_GID + ',' + FILE_ATTRIBUTE_STANDARD_TARGET_URI;

type
  TUpdateStatisticsFunction = procedure(var NewStatistics: TFileSourceCopyOperationStatistics) of object;
  TCopyMoveFileFunction = function(source: PGFile; destination: PGFile; flags: TGFileCopyFlags; cancellable: PGCancellable; progress_callback: TGFileProgressCallback; progress_callback_data: gpointer; error: PPGError): gboolean; cdecl;

  { TGioTreeBuilder }

  TGioTreeBuilder = class(TFileSourceTreeBuilder)
  protected
    procedure AddLinkTarget(aFile: TFile; CurrentNode: TFileTreeNode); override;
    procedure AddFilesInDirectory(srcPath: String; CurrentNode: TFileTreeNode); override;
  end;

  { TGioOperationHelper }

  TGioOperationHelper = class
  private
    FGioFileSource: IGioFileSource;
    FOperation: TFileSourceOperation;
    FCopyMoveFile: TCopyMoveFileFunction;
    FRootTargetPath: String;
    FRenameMask: String;
    FRenameNameMask, FRenameExtMask: String;
    FLogCaption: String;
    FRenamingFiles,
    FRenamingRootDir: Boolean;
    FCancel: PGCancellable;
    FRootDir: TFile;
    FOldDoneBytes: Int64;
    FSkipAnyError: Boolean;
    FStatistics: TFileSourceCopyOperationStatistics;
    FFileExistsOption: TFileSourceOperationOptionFileExists;
    FDirExistsOption: TFileSourceOperationOptionDirectoryExists;

    FCurrentFile: TFile;
    FCurrentTargetFilePath: String;

    AskQuestion: TAskQuestionFunction;
    AbortOperation: TAbortOperationFunction;
    CheckOperationState: TCheckOperationStateFunction;
    UpdateStatistics: TUpdateStatisticsFunction;
    ShowCompareFilesUI: TShowCompareFilesUIFunction;

    procedure ShowError(const Message: String; AError: PGError);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
    function ProcessNode(aFileTreeNode: TFileTreeNode; CurrentTargetPath: String): Boolean;
    function ProcessDirectory(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
    function ProcessLink(aNode: TFileTreeNode; AbsoluteTargetFileName: String): Boolean;
    function ProcessFile(aNode: TFileTreeNode; AbsoluteTargetFileName: String; Flags:  TGFileCopyFlags): Boolean;

    function TargetExists(aNode: TFileTreeNode; var aTargetFile: PGFile; var AbsoluteTargetFileName: String)
                 : TFileSystemOperationTargetExistsResult;
    function DirExists(aFile: TFile;
                       AbsoluteTargetFileName: String;
                       AllowCopyInto: Boolean): TFileSourceOperationOptionDirectoryExists;
    procedure QuestionActionHandler(Action: TFileSourceOperationUIAction);
    function FileExists(aFile: TFile;
                        aTargetInfo: PGFileInfo;
                        var AbsoluteTargetFileName: String): TFileSourceOperationOptionFileExists;

    procedure CountStatistics(aNode: TFileTreeNode);

  public
    constructor Create(FileSource: IFileSource;
                       Operation: TFileSourceOperation;
                       Statistics: TFileSourceCopyOperationStatistics;
                       AskQuestionFunction: TAskQuestionFunction;
                       AbortOperationFunction: TAbortOperationFunction;
                       CheckOperationStateFunction: TCheckOperationStateFunction;
                       UpdateStatisticsFunction: TUpdateStatisticsFunction;
                       ShowCompareFilesUIFunction: TShowCompareFilesUIFunction;
                       CopyMoveFileFunction: TCopyMoveFileFunction;
                       TargetPath: String
                       );
    destructor Destroy; override;

    procedure Initialize;

    procedure ProcessTree(aFileTree: TFileTree);

    property FileExistsOption: TFileSourceOperationOptionFileExists read FFileExistsOption write FFileExistsOption;
    property DirExistsOption: TFileSourceOperationOptionDirectoryExists read FDirExistsOption write FDirExistsOption;
    property RenameMask: String read FRenameMask write FRenameMask;
  end;

procedure ShowError(AError: PGError);
procedure FreeAndNil(var AError: PGError); overload;

procedure FillAndCount(Files: TFiles; CountDirs: Boolean; out NewFiles: TFiles;
                       out FilesCount: Int64; out FilesSize: Int64);

implementation

uses
  Forms, StrUtils, DCDateTimeUtils, uFileProperty,
  uShowMsg, uLng, uGObject2, DCFileAttributes;

procedure ShowError(AError: PGError);
begin
  msgError(nil, AError^.message);
  g_error_free(AError);
end;

procedure FillAndCount(Files: TFiles; CountDirs: Boolean; out NewFiles: TFiles;
  out FilesCount: Int64; out FilesSize: Int64);
var
  I: Integer;
  aFile: TFile;

  procedure FillAndCountRec(const srcPath: String);
  var
    AFolder: PGFile;
    AInfo: PGFileInfo;
    AFileName: Pgchar;
    AError: PGError = nil;
    AFileEnum: PGFileEnumerator;
  begin
    AFolder:= g_file_new_for_commandline_arg (Pgchar(srcPath));
    try
      AFileEnum:= g_file_enumerate_children (AFolder, CONST_DEFAULT_QUERY_INFO_ATTRIBUTES,
                                             G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS, nil, @AError);

      if Assigned(AFileEnum) then
      begin
        AInfo:= g_file_enumerator_next_file (AFileEnum, nil, @AError);
        while Assigned(AInfo) do
        begin
          AFileName:= g_file_info_get_name(AInfo);

          if (aFileName <> '.') and (aFileName <> '..') then
          begin
            aFile:= TGioFileSource.CreateFile(srcPath, AFolder, AInfo);
            NewFiles.Add(aFile);

            if aFile.IsLink then
              begin
              end
            else if aFile.IsDirectory then
              begin
                if CountDirs then Inc(FilesCount);
                FillAndCountRec(srcPath + aFileName + PathDelim);
              end
            else
              begin
                Inc(FilesSize, aFile.Size);
                Inc(FilesCount);
              end;
           end;

          AInfo:= g_file_enumerator_next_file (AFileEnum, nil, @AError);
        end;
        g_object_unref(AFileEnum);
      end;
      if Assigned(AError) then ShowError(AError);
    finally
      g_object_unref(PGObject(AFolder));
    end;
  end;

begin
  FilesCount:= 0;
  FilesSize:= 0;

  NewFiles := TFiles.Create(Files.Path);
  for I := 0 to Files.Count - 1 do
  begin
    aFile := Files[I];

    NewFiles.Add(aFile.Clone);

    if aFile.IsDirectory and (not aFile.IsLinkToDirectory) then
      begin
        if CountDirs then
          Inc(FilesCount);
        FillAndCountRec(aFile.FullPath + DirectorySeparator);  // recursive browse child dir
      end
    else
      begin
        Inc(FilesCount);
        Inc(FilesSize, aFile.Size); // in first level we know file size -> use it
      end;
  end;
end;

function FileExistsMessage(SourceFile: TFile; TargetInfo: PGFileInfo; const TargetName: String): String;
begin
  Result:= rsMsgFileExistsOverwrite + LineEnding + TargetName + LineEnding +
           Format(rsMsgFileExistsFileInfo, [Numb2USA(IntToStr(g_file_info_get_size(TargetInfo))),
                  DateTimeToStr(UnixFileTimeToDateTime(g_file_info_get_attribute_uint64(TargetInfo, FILE_ATTRIBUTE_TIME_MODIFIED)))]) + LineEnding;
  Result:= Result + LineEnding + rsMsgFileExistsWithFile + LineEnding + SourceFile.FullPath + LineEnding +
           Format(rsMsgFileExistsFileInfo, [Numb2USA(IntToStr(SourceFile.Size)), DateTimeToStr(SourceFile.ModificationTime)]);
end;

procedure FreeAndNil(var AError: PGError);
begin
  g_error_free(AError);
  AError:= nil;
end;

procedure ProgressCallback(current_num_bytes: gint64; total_num_bytes: gint64; user_data: gpointer); cdecl;
var
  Helper: TGioOperationHelper absolute user_data;
begin
  with Helper do
  begin
    if FOperation.State = fsosStopping then  // Cancel operation
    begin
      g_cancellable_cancel(FCancel);
      Exit;
    end;

    with FStatistics do
    begin
      CurrentFileDoneBytes:= current_num_bytes;
      CurrentFileTotalBytes:= total_num_bytes;
      DoneBytes:= FOldDoneBytes + current_num_bytes;
    end;
    UpdateStatistics(FStatistics);

    CheckOperationState;
  end;
end;

{ TGioTreeBuilder }

procedure TGioTreeBuilder.AddLinkTarget(aFile: TFile; CurrentNode: TFileTreeNode);
begin
  // Add as normal file/directory
  aFile.Attributes:= aFile.Attributes and (not S_IFLNK);

  if aFile.IsLinkToDirectory then
  begin
    aFile.Attributes:= aFile.Attributes or S_IFDIR;
    AddDirectory(aFile, CurrentNode);
  end
  else begin
    AddFile(aFile, CurrentNode);
  end;
end;

procedure TGioTreeBuilder.AddFilesInDirectory(srcPath: String;
  CurrentNode: TFileTreeNode);
var
  AFile: TFile;
  AFolder: PGFile;
  AInfo: PGFileInfo;
  AError: PGError = nil;
  AFileEnum: PGFileEnumerator;
begin
  AFolder:= g_file_new_for_commandline_arg(Pgchar(srcPath));
  try
    AFileEnum := g_file_enumerate_children (AFolder, CONST_DEFAULT_QUERY_INFO_ATTRIBUTES,
                                            G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS, nil, @AError);
    // List files
    try
      AInfo:= g_file_enumerator_next_file(AFileEnum, nil, @AError);
      while Assigned(AInfo) do
      begin
        CheckOperationState;
        AFile:= TGioFileSource.CreateFile(srcPath, AFolder, AInfo);
        g_object_unref(AInfo);
        AddItem(aFile, CurrentNode);
        AInfo:= g_file_enumerator_next_file(AFileEnum, nil, @AError);
      end;
      if Assigned(AError) then ShowError(AError);
    finally
      g_object_unref(AFileEnum);
    end;

  finally
    g_object_unref(PGObject(AFolder));
  end;
end;

{ TGioOperationHelper }

procedure TGioOperationHelper.ShowError(const Message: String; AError: PGError);
begin
  try
    if not gSkipFileOpError then
    begin
      if AskQuestion(Message + LineEnding + AError^.message,
                     '', [fsourSkip, fsourAbort],
                     fsourSkip, fsourAbort) = fsourAbort then
      begin
        AbortOperation;
      end;
    end;
    if log_errors in gLogOptions then
      logWrite(FOperation.Thread, Message, lmtError, gSkipFileOpError);
  finally
    g_error_free(AError);
  end;
end;

procedure TGioOperationHelper.LogMessage(sMessage: String;
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
    logWrite(FOperation.Thread, sMessage, logMsgType);
  end;
end;

function TGioOperationHelper.ProcessNode(aFileTreeNode: TFileTreeNode;
  CurrentTargetPath: String): Boolean;
var
  aFile: TFile;
  ProcessedOk: Boolean;
  TargetName: String;
  CurrentFileIndex: Integer;
  CurrentSubNode: TFileTreeNode;
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

    if aFile.IsDirectory then
      ProcessedOk := ProcessDirectory(CurrentSubNode, TargetName)
    else if aFile.IsLink then
      ProcessedOk := ProcessLink(CurrentSubNode, TargetName)
    else
      ProcessedOk := ProcessFile(CurrentSubNode, TargetName, G_FILE_COPY_NONE);

    if not ProcessedOk then
      Result := False;

    CheckOperationState;
  end;
end;

function TGioOperationHelper.ProcessDirectory(aNode: TFileTreeNode;
  AbsoluteTargetFileName: String): Boolean;
var
  AError: PGError = nil;
  bRemoveDirectory: Boolean;
  NodeData: TFileTreeNodeData;
  SourceFile, TargetFile: PGFile;
begin
  NodeData := aNode.Data as TFileTreeNodeData;

  SourceFile:= g_file_new_for_commandline_arg(Pgchar(aNode.TheFile.FullPath));
  TargetFile:= g_file_new_for_commandline_arg(Pgchar(AbsoluteTargetFileName));
  try
  // If some files will not be moved then source directory cannot be deleted.
  bRemoveDirectory := (FCopyMoveFile = g_file_move) and (NodeData.SubnodesHaveExclusions = False);

  case TargetExists(aNode, TargetFile, AbsoluteTargetFileName) of
    fsoterSkip:
      begin
        Result := False;
        CountStatistics(aNode);
      end;

    fsoterNotExists:
      begin
        // Try moving whole directory tree. It can be done only if we don't have
        // to process each subnode: if the files are not being renamed or excluded.
        if (FCopyMoveFile = g_file_move) and
           (not FRenamingFiles) and
           (NodeData.SubnodesHaveExclusions = False) and
           g_file_move(SourceFile, TargetFile, G_FILE_COPY_NOFOLLOW_SYMLINKS or G_FILE_COPY_NO_FALLBACK_FOR_MOVE, nil, nil, nil, nil)
           then
        begin
          // Success.
          CountStatistics(aNode);
          Result := True;
          bRemoveDirectory := False;
        end
        else
        begin
          // Create target directory.
          if g_file_make_directory_with_parents(TargetFile, nil, @AError) then
          begin
            // Copy/Move all files inside.
            Result := ProcessNode(aNode, IncludeTrailingPathDelimiter(AbsoluteTargetFileName));
          end
          else
          begin
            // Error - all files inside not copied/moved.
            ShowError(rsMsgLogError + Format(rsMsgErrForceDir, [AbsoluteTargetFileName]), AError);
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
    g_file_delete(SourceFile, nil, nil);
  end;

  finally
    g_object_unref(PGObject(SourceFile));
    g_object_unref(PGObject(TargetFile));
  end;
end;

function TGioOperationHelper.ProcessLink(aNode: TFileTreeNode;
  AbsoluteTargetFileName: String): Boolean;
begin
  Result:= ProcessFile(aNode, AbsoluteTargetFileName, G_FILE_COPY_NOFOLLOW_SYMLINKS);
end;

function TGioOperationHelper.ProcessFile(aNode: TFileTreeNode;
  AbsoluteTargetFileName: String; Flags: TGFileCopyFlags): Boolean;
var
  AError: PGError = nil;
  SourceFile, TargetFile: PGFile;
begin
  FOldDoneBytes:= FStatistics.DoneBytes;

  FCancel:= g_cancellable_new();
  SourceFile:= g_file_new_for_commandline_arg(Pgchar(aNode.TheFile.FullPath));
  TargetFile:= g_file_new_for_commandline_arg(Pgchar(AbsoluteTargetFileName));

  try
    repeat
      Result:= FCopyMoveFile(SourceFile, TargetFile, Flags, FCancel, @ProgressCallback, Self, @AError);
      if Assigned(AError) then
      try
        if AError^.code = G_IO_ERROR_CANCELLED then
          AbortOperation
        else if AError^.code = G_IO_ERROR_EXISTS then
        begin
          case TargetExists(aNode, TargetFile, AbsoluteTargetFileName) of
            fsoterDeleted:
            begin
              FreeAndNil(AError);
              Flags += G_FILE_COPY_OVERWRITE;
            end;
            fsoterSkip:
            begin
              Result:= True;
              Break;
            end;
            fsoterNotExists:
              FreeAndNil(AError);
          end;
        end
        else
        begin
          if FSkipAnyError then Break;
          case AskQuestion(AError^.message, '', [fsourRetry, fsourSkip, fsourSkipAll, fsourCancel], fsourRetry, fsourCancel) of
            fsourSkip: Break;
            fsourSkipAll:
              begin
                FSkipAnyError:= True;
                Break;
              end;
            fsourRetry: FreeAndNil(AError);
            fsourCancel: AbortOperation;
          end;
        end;
      except
        on EFileSourceOperationAborting do
        begin
          FreeAndNil(AError);
          raise;
        end;
      end;
    until Result;

    if Result then
    begin
      LogMessage(Format(rsMsgLogSuccess + FLogCaption,
                        [aNode.TheFile.FullPath + ' -> ' + AbsoluteTargetFileName]),
                 [log_vfs_op], lmtSuccess);
    end
    else begin
      LogMessage(Format(rsMsgLogError + FLogCaption,
                       [aNode.TheFile.FullPath + ' -> ' + AbsoluteTargetFileName]) + LineEnding + AError^.message,
                 [log_vfs_op], lmtError);
      FreeAndNil(AError);
    end;

  finally
    g_object_unref(FCancel);
    g_object_unref(PGObject(SourceFile));
    g_object_unref(PGObject(TargetFile));
  end;

  with FStatistics do
  begin
    DoneFiles := DoneFiles + 1;
    DoneBytes := FOldDoneBytes + aNode.TheFile.Size;
    UpdateStatistics(FStatistics);
  end;
end;

function TGioOperationHelper.TargetExists(aNode: TFileTreeNode;
  var aTargetFile: PGFile; var AbsoluteTargetFileName: String
  ): TFileSystemOperationTargetExistsResult;
var
  AInfo, ASymlinkInfo: PGFileInfo;
  AFileType: TGFileType;
  SourceFile: TFile;

  function DoDirectoryExists(AllowCopyInto: Boolean): TFileSystemOperationTargetExistsResult;
  begin
    case DirExists(SourceFile, AbsoluteTargetFileName, AllowCopyInto) of
      fsoodeSkip:
        Exit(fsoterSkip);
      fsoodeCopyInto:
        begin
          Exit(fsoterAddToTarget);
        end;
      else
        raise Exception.Create('Invalid dir exists option');
    end;
  end;

  function DoFileExists(): TFileSystemOperationTargetExistsResult;
  begin
    case FileExists(SourceFile, AInfo, AbsoluteTargetFileName) of
      fsoofeSkip: Exit(fsoterSkip);
      fsoofeAutoRenameSource:
        begin
          g_object_unref(PGObject(aTargetFile));
          aTargetFile:= g_file_new_for_commandline_arg(Pgchar(AbsoluteTargetFileName));
          Exit(fsoterRenamed);
        end;
      fsoofeOverwrite: Exit(fsoterDeleted);
      else
        raise Exception.Create('Invalid file exists option');
    end;
  end;

begin
  repeat
    AInfo:= g_file_query_info(aTargetFile, FILE_ATTRIBUTE_STANDARD_TYPE + ',' +  FILE_ATTRIBUTE_STANDARD_SIZE +','+ FILE_ATTRIBUTE_TIME_MODIFIED, G_FILE_QUERY_INFO_NOFOLLOW_SYMLINKS, nil, nil);

    if Assigned(AInfo) then
    begin
      SourceFile:= aNode.TheFile;
      AFileType:= g_file_info_get_file_type(AInfo);

      // Target exists - ask user what to do.
      if AFileType = G_FILE_TYPE_DIRECTORY then
      begin
        Result := DoDirectoryExists(SourceFile.IsDirectory)
      end
      else if AFileType = G_FILE_TYPE_SYMBOLIC_LINK then
      begin
        // Check if target of the link exists.
        ASymlinkInfo:= g_file_query_info(aTargetFile, FILE_ATTRIBUTE_STANDARD_TYPE, G_FILE_QUERY_INFO_NONE, nil, nil);
        if Assigned(ASymlinkInfo) then
        begin
          AFileType:= g_file_info_get_file_type(ASymlinkInfo);
          if AFileType = G_FILE_TYPE_DIRECTORY then
            Result := DoDirectoryExists(SourceFile.IsDirectory)
          else begin
            Result := DoFileExists();
          end;
          g_object_unref(ASymlinkInfo);
        end
        else
          // Target of link doesn't exist. Treat link as file.
          Result := DoFileExists();
      end
      else begin
        // Existing target is a file.
        Result := DoFileExists();
      end;
      g_object_unref(AInfo);
    end
    else
      Result := fsoterNotExists;
  until Result <> fsoterRenamed;
end;

function TGioOperationHelper.DirExists(aFile: TFile;
  AbsoluteTargetFileName: String; AllowCopyInto: Boolean
  ): TFileSourceOperationOptionDirectoryExists;
var
  PossibleResponses: array of TFileSourceOperationUIResponse = nil;
  DefaultOkResponse: TFileSourceOperationUIResponse;

  procedure AddResponse(Response: TFileSourceOperationUIResponse);
  begin
    SetLength(PossibleResponses, Length(PossibleResponses) + 1);
    PossibleResponses[Length(PossibleResponses) - 1] := Response;
  end;

begin
  case FDirExistsOption of
    fsoodeNone:
      begin
        if AllowCopyInto then
        begin
          AddResponse(fsourCopyInto);
          AddResponse(fsourCopyIntoAll);
        end;
        AddResponse(fsourSkip);
        AddResponse(fsourSkipAll);
        AddResponse(fsourCancel);

        if AllowCopyInto then
          DefaultOkResponse := fsourCopyInto
        else
          DefaultOkResponse := fsourSkip;

        case AskQuestion(Format(rsMsgFolderExistsRwrt, [AbsoluteTargetFileName]), '',
                         PossibleResponses, DefaultOkResponse, fsourSkip) of
          fsourCopyInto:
            Result := fsoodeCopyInto;
          fsourCopyIntoAll:
            begin
              FDirExistsOption := fsoodeCopyInto;
              Result := fsoodeCopyInto;
            end;
          fsourSkip:
            Result := fsoodeSkip;
          fsourSkipAll:
            begin
              FDirExistsOption := fsoodeSkip;
              Result := fsoodeSkip;
            end;
          fsourNone,
          fsourCancel:
            AbortOperation;
        end;
      end;

    else
      Result := FDirExistsOption;
  end;
end;

procedure TGioOperationHelper.QuestionActionHandler(
  Action: TFileSourceOperationUIAction);
begin
  if Action = fsouaCompare then
    ShowCompareFilesUI(FCurrentFile, FCurrentTargetFilePath);
end;

function TGioOperationHelper.FileExists(aFile: TFile; aTargetInfo: PGFileInfo;
  var AbsoluteTargetFileName: String): TFileSourceOperationOptionFileExists;
const
  Responses: array[0..9] of TFileSourceOperationUIResponse
    = (fsourOverwrite, fsourSkip, fsourRenameSource, fsourOverwriteAll,
       fsourSkipAll, fsourOverwriteOlder,fsourOverwriteSmaller,
       fsourCancel, fsouaCompare, fsourOverwriteLarger);
var
  Answer: Boolean;
  Message: String;

  function OverwriteOlder: TFileSourceOperationOptionFileExists;
  begin
    if aFile.ModificationTime > UnixFileTimeToDateTime(g_file_info_get_attribute_uint64(aTargetInfo, FILE_ATTRIBUTE_TIME_MODIFIED)) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

  function OverwriteSmaller: TFileSourceOperationOptionFileExists;
  begin
    if aFile.Size > g_file_info_get_size(aTargetInfo) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

  function OverwriteLarger: TFileSourceOperationOptionFileExists;
  begin
    if aFile.Size < g_file_info_get_size(aTargetInfo) then
      Result := fsoofeOverwrite
    else
      Result := fsoofeSkip;
  end;

begin
  case FFileExistsOption of
    fsoofeNone:
      repeat
        Answer := True;
        Message:= FileExistsMessage(aFile, aTargetInfo, AbsoluteTargetFileName);
        FCurrentFile := aFile;
        FCurrentTargetFilePath := AbsoluteTargetFileName;
        case AskQuestion(Message, '',
                         Responses, fsourOverwrite, fsourSkip,
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
          fsourRenameSource:
            begin
              Message:= ExtractFileName(AbsoluteTargetFileName);
              Answer:= ShowInputQuery(FOperation.Thread, Application.Title, rsEditNewFileName, Message);
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

    else
      Result := FFileExistsOption;
  end;
end;

procedure TGioOperationHelper.CountStatistics(aNode: TFileTreeNode);

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

constructor TGioOperationHelper.Create(FileSource: IFileSource;
  Operation: TFileSourceOperation;
  Statistics: TFileSourceCopyOperationStatistics;
  AskQuestionFunction: TAskQuestionFunction;
  AbortOperationFunction: TAbortOperationFunction;
  CheckOperationStateFunction: TCheckOperationStateFunction;
  UpdateStatisticsFunction: TUpdateStatisticsFunction;
  ShowCompareFilesUIFunction: TShowCompareFilesUIFunction;
  CopyMoveFileFunction: TCopyMoveFileFunction; TargetPath: String);
begin
  FGioFileSource:= FileSource as IGioFileSource;
  FOperation:= Operation;
  FStatistics:= Statistics;
  AskQuestion := AskQuestionFunction;
  AbortOperation := AbortOperationFunction;
  CheckOperationState := CheckOperationStateFunction;
  UpdateStatistics := UpdateStatisticsFunction;
  ShowCompareFilesUI := ShowCompareFilesUIFunction;
  FCopyMoveFile := CopyMoveFileFunction;

  FFileExistsOption := fsoofeNone;
  FRootTargetPath := TargetPath;
  FRenameMask := '';
  FRenamingFiles := False;
  FRenamingRootDir := False;

  inherited Create;
end;

destructor TGioOperationHelper.Destroy;
begin
  inherited Destroy;
end;

procedure TGioOperationHelper.Initialize;
begin
  if FCopyMoveFile = g_file_copy then
    FLogCaption := rsMsgLogCopy
  else begin
    FLogCaption := rsMsgLogMove;
  end;

  SplitFileMask(FRenameMask, FRenameNameMask, FRenameExtMask);
end;

procedure TGioOperationHelper.ProcessTree(aFileTree: TFileTree);
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

end.

