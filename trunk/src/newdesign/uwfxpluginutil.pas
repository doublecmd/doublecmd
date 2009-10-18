unit uWfxPluginUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, uDescr, uLog, uGlobs,
  uFile,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFileSourceCopyOperation,
  uWfxPluginFileSource,
  uFileSystemFile;

type

  TUpdateProgress = function(SourceName, TargetName: UTF8String; PercentDone: Integer): Integer of object;

  TUpdateProgressClass = class
  public
    UpdateProgressFunction: TUpdateProgress;
  end;

  TWfxPluginOperationHelperMode =
    (wpohmCopyMoveIn, wpohmCopyMoveOut);

  TUpdateStatisticsFunction = procedure(var NewStatistics: TFileSourceCopyOperationStatistics) of object;

  { TWfxPluginOperationHelper }

  TWfxPluginOperationHelper = class
  private
    FWfxPluginFileSource: TWfxPluginFileSource;
    FOperationThread: TThread;
    FMode: TWfxPluginOperationHelperMode;
    FRootTargetPath: String;
    FRenameMask: String;
    FRenameNameMask, FRenameExtMask: String;
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    FLogCaption: String;
    FRenamingFiles,
    FInternal: Boolean;
    FFileExistsOption: TFileSourceOperationOptionFileExists;

    AskQuestion: TAskQuestionFunction;
    AbortOperation: TAbortOperationFunction;
    CheckOperationState: TCheckOperationStateFunction;
    UpdateStatistics: TUpdateStatisticsFunction;

    procedure ShowError(sMessage: String);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

    function ProcessDirectory(aFile: TFileSystemFile; AbsoluteTargetFileName: String): LongInt;
    function ProcessFile(aFile: TFileSystemFile; AbsoluteTargetFileName: String): LongInt;

    function FileExists(aFile: TFileSystemFile;
                        AbsoluteTargetFileName: String;
                        AllowAppend: Boolean): TFileSourceOperationOptionFileExists;

  public
    constructor Create(FileSource: TFileSource;
                       AskQuestionFunction: TAskQuestionFunction;
                       AbortOperationFunction: TAbortOperationFunction;
                       CheckOperationStateFunction: TCheckOperationStateFunction;
                       UpdateStatisticsFunction: TUpdateStatisticsFunction;
                       OperationThread: TThread;
                       Mode: TWfxPluginOperationHelperMode;
                       TargetPath: String;
                       StartingStatistics: TFileSourceCopyOperationStatistics);
    destructor Destroy; override;

    procedure Initialize(Internal: Boolean);

    procedure ProcessFiles(aFiles: TFiles);

    property FileExistsOption: TFileSourceOperationOptionFileExists read FFileExistsOption write FFileExistsOption;
    property RenameMask: String read FRenameMask write FRenameMask;
  end;

implementation

uses
  uFileProcs, uDCUtils, uLng, ufsplugin, uWfxModule, uFileSystemUtil, uOSUtils;

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
    if AskQuestion(sMessage, '', [fsourSkip, fsourCancel],
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

function TWfxPluginOperationHelper.ProcessDirectory(aFile: TFileSystemFile;
  AbsoluteTargetFileName: String): LongInt;
begin
  Result:= WFX_ERROR;
  case FMode of
  wpohmCopyMoveIn:
    begin
      Result:= FWfxPluginFileSource.WfxMkDir(AbsoluteTargetFileName);
    end;
  wpohmCopyMoveOut:
    begin
      if mbForceDirectory(AbsoluteTargetFileName) then
        Result:= WFX_SUCCESS;
    end;
  end;
end;

function TWfxPluginOperationHelper.ProcessFile(aFile: TFileSystemFile;
  AbsoluteTargetFileName: String): LongInt;
var
  iFlags: Integer;
  RemoteInfo: TRemoteInfo;
  iTemp: TInt64Rec;
  OldDoneBytes: Int64; // for if there was an error
begin
  // If there will be an error the DoneBytes value
  // will be inconsistent, so remember it here.
  OldDoneBytes := FStatistics.DoneBytes;

  with FWfxPluginFileSource do
  begin
  { FCurrentFileSize:= aFile.Size;
  }
      iFlags:= 0;
      with RemoteInfo do
      begin
        iTemp.Value := aFile.Size;
        SizeLow := iTemp.Low;
        SizeHigh := iTemp.High;
        LastWriteTime := DateTimeToFileTime(aFile.ModificationTime);
        Attr := LongInt(aFile.Attributes);
      end;
      Result := WfxCopyMove(aFile.Path + aFile.Name, AbsoluteTargetFileName, iFlags, @RemoteInfo, FInternal, FMode = wpohmCopyMoveIn);

      case Result of
      FS_FILE_EXISTS, // The file already exists, and resume isn't supported
      FS_FILE_EXISTSRESUMEALLOWED: // The file already exists, and resume is supported
        begin
          case FileExists(aFile, AbsoluteTargetFileName, Result = FS_FILE_EXISTSRESUMEALLOWED) of
            fsoofeSkip:
              Exit(FS_FILE_OK);
            fsoofeOverwrite:
                iFlags:= iFlags + FS_COPYFLAGS_OVERWRITE;
            fsoofeAppend:
                iFlags:= iFlags + FS_COPYFLAGS_RESUME;
          else
            raise Exception.Create('Invalid file exists option');
          end;
          Result := WfxCopyMove(aFile.Path + aFile.Name, AbsoluteTargetFileName, iFlags, @RemoteInfo, FInternal, FMode = wpohmCopyMoveIn);
        end;
      end;
   end;

  with FStatistics do
  begin
    DoneFiles := DoneFiles + 1;
    DoneBytes := OldDoneBytes + aFile.Size;
    UpdateStatistics(FStatistics);
  end;
end;

function TWfxPluginOperationHelper.FileExists(aFile: TFileSystemFile;
  AbsoluteTargetFileName: String; AllowAppend: Boolean
  ): TFileSourceOperationOptionFileExists;
const
  Responses: array[0..4] of TFileSourceOperationUIResponse
    = (fsourRewrite, fsourSkip, fsourAppend, fsourRewriteAll, fsourSkipAll);
  ResponsesNoAppend: array[0..3] of TFileSourceOperationUIResponse
    = (fsourRewrite, fsourSkip, fsourRewriteAll, fsourSkipAll);
var
  PossibleResponses: array of TFileSourceOperationUIResponse;
begin
  case FFileExistsOption of
    fsoofeNone:
      begin
        case AllowAppend of
          True :  PossibleResponses := Responses;
          False:  PossibleResponses := ResponsesNoAppend;
        end;

        case AskQuestion(Format(rsMsgFileExistsRwrt, [AbsoluteTargetFileName]), '',
                         PossibleResponses, fsourRewrite, fsourSkip) of
          fsourRewrite:
            Result := fsoofeOverwrite;
          fsourSkip:
            Result := fsoofeSkip;
          fsourAppend:
            begin
              //FFileExistsOption := fsoofeAppend; - for AppendAll
              Result := fsoofeAppend;
            end;
          fsourRewriteAll:
            begin
              FFileExistsOption := fsoofeOverwrite;
              Result := fsoofeOverwrite;
            end;
          fsourSkipAll:
            begin
              FFileExistsOption := fsoofeSkip;
              Result := fsoofeSkip;
            end;
        end;
      end;

    else
      Result := FFileExistsOption;
  end;
end;


constructor TWfxPluginOperationHelper.Create(FileSource: TFileSource;
                                             AskQuestionFunction: TAskQuestionFunction;
                                             AbortOperationFunction: TAbortOperationFunction;
                                             CheckOperationStateFunction: TCheckOperationStateFunction;
                                             UpdateStatisticsFunction: TUpdateStatisticsFunction;
                                             OperationThread: TThread;
                                             Mode: TWfxPluginOperationHelperMode;
                                             TargetPath: String;
                                             StartingStatistics: TFileSourceCopyOperationStatistics);
begin
  FWfxPluginFileSource:= FileSource as TWfxPluginFileSource;
  AskQuestion := AskQuestionFunction;
  AbortOperation := AbortOperationFunction;
  CheckOperationState := CheckOperationStateFunction;
  UpdateStatistics := UpdateStatisticsFunction;
  FOperationThread:= OperationThread;
  FMode := Mode;

  FFileExistsOption := fsoofeNone;
  FRootTargetPath := TargetPath;
  FRenameMask := '';
  FStatistics := StartingStatistics;
  FRenamingFiles := False;

  inherited Create;
end;

destructor TWfxPluginOperationHelper.Destroy;
begin
  inherited Destroy;
end;

procedure TWfxPluginOperationHelper.Initialize(Internal: Boolean);
begin
  FInternal:= Internal;

  case FMode of
  wpohmCopyMoveIn:
    FLogCaption := rsMsgLogCopy;
  wpohmCopyMoveOut:
    FLogCaption := rsMsgLogMove;
  end;

  SplitFileMask(FRenameMask, FRenameNameMask, FRenameExtMask);
  FRenamingFiles := (FRenameMask <> '*.*') and (FRenameMask <> '');
end;

procedure TWfxPluginOperationHelper.ProcessFiles(aFiles: TFiles);
var
  I: Integer;
  iResult: LongInt;
  sSourceFile,
  sTargetFile : UTF8String;
  aFile: TFileSystemFile;
begin
  for I:= 0 to aFiles.Count - 1 do
    with FWfxPluginFileSource do
    begin
      aFile:= aFiles.Items[I] as TFileSystemFile;
      sSourceFile := aFile.Path + aFile.Name;
      // Filenames must be relative to the current directory.
      sTargetFile := FRootTargetPath + ExtractDirLevel(aFiles.Path, aFile.Path);
      sTargetFile := sTargetFile + ApplyRenameMask(aFile, FRenameNameMask, FRenameExtMask);

      DebugLn('Source name == ' + sSourceFile);
      DebugLn('Target name == ' + sTargetFile);

      with FStatistics do
      begin
        CurrentFileFrom := aFile.Path + aFile.Name;
        CurrentFileTo := sTargetFile;
        CurrentFileTotalBytes := aFile.Size;
        CurrentFileDoneBytes := 0;
      end;

      UpdateStatistics(FStatistics);

      if (not aFile.IsDirectory) or FInternal then
        iResult := ProcessFile(aFile, sTargetFile)
      else
        iResult := ProcessDirectory(aFile, sTargetFile);

      if iResult = FS_FILE_OK then
        begin
          LogMessage(Format(rsMsgLogSuccess+FLogCaption, [aFile.FullPath + ' -> ' + sTargetFile]),
                     [log_vfs_op], lmtSuccess);
        end
      else
        begin
          ShowError(Format(rsMsgLogError + FLogCaption,
                           [aFile.FullPath + ' -> ' + sTargetFile +
                            ' - ' + GetErrorMsg(iResult)]));
          LogMessage(Format(rsMsgLogError+FLogCaption, [aFile.FullPath + ' -> ' + sTargetFile]),
                     [log_vfs_op], lmtError);
        end;

      CheckOperationState;
    end;
end;

end.

