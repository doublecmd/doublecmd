unit uWfxPluginUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, uLog, uGlobs,
  WfxPlugin,
  uFile,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFileSourceCopyOperation,
  uWfxPluginFileSource;

type

  TWfxPluginOperationHelperMode =
    (wpohmCopy, wpohmCopyIn, wpohmCopyOut, wpohmMove);

  TUpdateStatisticsFunction = procedure(var NewStatistics: TFileSourceCopyOperationStatistics) of object;

  { TWfxPluginOperationHelper }

  TWfxPluginOperationHelper = class
  private
    FWfxPluginFileSource: IWfxPluginFileSource;
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

    function ProcessDirectory(aFile: TFile; AbsoluteTargetFileName: String): LongInt;
    function ProcessFile(aFile: TFile; AbsoluteTargetFileName: String): LongInt;

    function FileExists(aFile: TFile;
                        AbsoluteTargetFileName: String;
                        AllowAppend: Boolean): TFileSourceOperationOptionFileExists;

  public
    constructor Create(FileSource: IFileSource;
                       AskQuestionFunction: TAskQuestionFunction;
                       AbortOperationFunction: TAbortOperationFunction;
                       CheckOperationStateFunction: TCheckOperationStateFunction;
                       UpdateStatisticsFunction: TUpdateStatisticsFunction;
                       OperationThread: TThread;
                       Mode: TWfxPluginOperationHelperMode;
                       TargetPath: String;
                       StartingStatistics: TFileSourceCopyOperationStatistics);
    destructor Destroy; override;

    procedure Initialize;

    procedure ProcessFiles(aFiles: TFiles);

    property FileExistsOption: TFileSourceOperationOptionFileExists read FFileExistsOption write FFileExistsOption;
    property RenameMask: String read FRenameMask write FRenameMask;
  end;

  function WfxRenameFile(aFileSource: IWfxPluginFileSource; const aFile: TFile; const NewFileName: UTF8String): Boolean;

  function WfxFileTimeToDateTime(FileTime : TWfxFileTime) : TDateTime; inline;
  function DateTimeToWfxFileTime(DateTime : TDateTime) : TWfxFileTime; inline;

implementation

uses
  uFileProcs, uDCUtils, uLng, uWfxModule, uFileSystemUtil, uFileProperty,
  uDateTimeUtils, uTypes;

function WfxRenameFile(aFileSource: IWfxPluginFileSource; const aFile: TFile; const NewFileName: UTF8String): Boolean;
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
  Result:= WinFileTimeToDateTime(TWinFileTime(FileTime));
end;

function DateTimeToWfxFileTime(DateTime: TDateTime): TWfxFileTime;
begin
  Result:= TWfxFileTime(DateTimeToWinFileTime(DateTime));
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

function TWfxPluginOperationHelper.ProcessDirectory(aFile: TFile;
  AbsoluteTargetFileName: String): LongInt;
begin
  Result:= WFX_ERROR;
  case FMode of
  wpohmCopy,
  wpohmCopyIn,
  wpohmMove:
    begin
      Result:= FWfxPluginFileSource.WfxModule.WfxMkDir('', AbsoluteTargetFileName);
    end;
  wpohmCopyOut:
    begin
      if mbForceDirectory(AbsoluteTargetFileName) then
        Result:= WFX_SUCCESS;
    end;
  end;
end;

function TWfxPluginOperationHelper.ProcessFile(aFile: TFile;
  AbsoluteTargetFileName: String): LongInt;
var
  iFlags: Integer;
  RemoteInfo: TRemoteInfo;
  iTemp: TInt64Rec;
  bCopyMoveIn: Boolean;
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
        iTemp.Value := (aFile.Properties[fpSize] as TFileSizeProperty).Value;
        SizeLow := iTemp.Low;
        SizeHigh := iTemp.High;
        LastWriteTime := DateTimeToWfxFileTime((aFile.Properties[fpModificationTime] as TFileModificationDateTimeProperty).Value);
        Attr := LongInt((aFile.Properties[fpAttributes] as TFileAttributesProperty).Value);
      end;
      if (FMode = wpohmMove) then
        iFlags:= iFlags + FS_COPYFLAGS_MOVE;
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
            fsoofeAppend:
                iFlags:= iFlags + FS_COPYFLAGS_RESUME;
          else
            raise Exception.Create('Invalid file exists option');
          end;
          Result := WfxCopyMove(aFile.Path + aFile.Name, AbsoluteTargetFileName, iFlags, @RemoteInfo, FInternal, bCopyMoveIn);
        end;
      end;
   end;

  with FStatistics do
  begin
    DoneFiles := DoneFiles + 1;
    DoneBytes := OldDoneBytes + (aFile.Properties[fpSize] as TFileSizeProperty).Value;
    UpdateStatistics(FStatistics);
  end;
end;

function TWfxPluginOperationHelper.FileExists(aFile: TFile;
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


constructor TWfxPluginOperationHelper.Create(FileSource: IFileSource;
                                             AskQuestionFunction: TAskQuestionFunction;
                                             AbortOperationFunction: TAbortOperationFunction;
                                             CheckOperationStateFunction: TCheckOperationStateFunction;
                                             UpdateStatisticsFunction: TUpdateStatisticsFunction;
                                             OperationThread: TThread;
                                             Mode: TWfxPluginOperationHelperMode;
                                             TargetPath: String;
                                             StartingStatistics: TFileSourceCopyOperationStatistics);
begin
  FWfxPluginFileSource:= FileSource as IWfxPluginFileSource;
  AskQuestion := AskQuestionFunction;
  AbortOperation := AbortOperationFunction;
  CheckOperationState := CheckOperationStateFunction;
  UpdateStatistics := UpdateStatisticsFunction;
  FOperationThread:= OperationThread;
  FMode := Mode;
  FInternal:= (FMode in [wpohmCopy, wpohmMove]);

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
  FRenamingFiles := (FRenameMask <> '*.*') and (FRenameMask <> '');
end;

procedure TWfxPluginOperationHelper.ProcessFiles(aFiles: TFiles);
var
  I: Integer;
  iResult: LongInt;
  sTargetFile : UTF8String;
  aFile: TFile;
begin
  for I:= 0 to aFiles.Count - 1 do
    with FWfxPluginFileSource do
    begin
      aFile:= aFiles.Items[I];

      // Filenames must be relative to the current directory.
      sTargetFile := FRootTargetPath + ExtractDirLevel(aFiles.Path, aFile.Path);
      sTargetFile := sTargetFile + ApplyRenameMask(aFile, FRenameNameMask, FRenameExtMask);

      //DebugLn('Source name == ' + aFile.FullPath);
      //DebugLn('Target name == ' + sTargetFile);

      with FStatistics do
      begin
        CurrentFileFrom := aFile.Path + aFile.Name;
        CurrentFileTo := sTargetFile;
        CurrentFileTotalBytes := (aFile.Properties[fpSize] as TFileSizeProperty).Value;
        CurrentFileDoneBytes := 0;
      end;

      UpdateStatistics(FStatistics);

      if not aFile.IsDirectory then
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

