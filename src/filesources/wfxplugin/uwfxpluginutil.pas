unit uWfxPluginUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, DCOSUtils, uLog, uGlobs,
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
    FLogCaption: String;
    FRenamingFiles,
    FRenamingRootDir,
    FInternal: Boolean;
    FCopyAttributesOptions: TCopyAttributesOptions;
    FFileExistsOption: TFileSourceOperationOptionFileExists;

    AskQuestion: TAskQuestionFunction;
    AbortOperation: TAbortOperationFunction;
    CheckOperationState: TCheckOperationStateFunction;
    UpdateStatistics: TUpdateStatisticsFunction;

    procedure ShowError(sMessage: String);
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

    function ProcessDirectory(aFile: TFile; AbsoluteTargetFileName: String): LongInt;
    function ProcessFile(aFile: TFile; AbsoluteTargetFileName: String; var Statistics: TFileSourceCopyOperationStatistics): LongInt;

    function FileExists(aFile: TFile;
                        AbsoluteTargetFileName: String;
                        AllowResume: Boolean): TFileSourceOperationOptionFileExists;

    procedure CopyProperties(SourceFile: TFile; const TargetFileName: String);

  public
    constructor Create(FileSource: IFileSource;
                       AskQuestionFunction: TAskQuestionFunction;
                       AbortOperationFunction: TAbortOperationFunction;
                       CheckOperationStateFunction: TCheckOperationStateFunction;
                       UpdateStatisticsFunction: TUpdateStatisticsFunction;
                       OperationThread: TThread;
                       Mode: TWfxPluginOperationHelperMode;
                       TargetPath: String
                       );
    destructor Destroy; override;

    procedure Initialize;

    procedure ProcessFiles(aFiles: TFiles; var Statistics: TFileSourceCopyOperationStatistics);

    property FileExistsOption: TFileSourceOperationOptionFileExists read FFileExistsOption write FFileExistsOption;
    property CopyAttributesOptions: TCopyAttributesOptions read FCopyAttributesOptions write FCopyAttributesOptions;
    property RenameMask: String read FRenameMask write FRenameMask;
  end;

  function WfxRenameFile(aFileSource: IWfxPluginFileSource; const aFile: TFile; const NewFileName: String): Boolean;

  function WfxFileTimeToDateTime(FileTime : TWfxFileTime) : TDateTime; inline;
  function DateTimeToWfxFileTime(DateTime : TDateTime) : TWfxFileTime; inline;

implementation

uses
  uFileProcs, StrUtils, DCStrUtils, uLng, uWfxModule, uFileSystemUtil, uFileProperty,
  DCDateTimeUtils, DCBasicTypes;

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
  AbsoluteTargetFileName: String; var Statistics: TFileSourceCopyOperationStatistics): LongInt;
var
  iFlags: Integer = 0;
  RemoteInfo: TRemoteInfo;
  iTemp: TInt64Rec;
  bCopyMoveIn: Boolean;
  OldDoneBytes: Int64; // for if there was an error
begin
  // If there will be an error the DoneBytes value
  // will be inconsistent, so remember it here.
  OldDoneBytes := Statistics.DoneBytes;

  with FWfxPluginFileSource do
  begin
  { FCurrentFileSize:= aFile.Size;
  }
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

  with Statistics do
  begin
    if Result = FS_FILE_OK then DoneFiles := DoneFiles + 1;
    DoneBytes := OldDoneBytes + aFile.Size;
    UpdateStatistics(Statistics);
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
  Responses: array[0..5] of TFileSourceOperationUIResponse
    = (fsourOverwrite, fsourSkip, fsourResume, fsourOverwriteAll, fsourSkipAll,
       fsourCancel);
  ResponsesNoResume: array[0..4] of TFileSourceOperationUIResponse
    = (fsourOverwrite, fsourSkip, fsourOverwriteAll, fsourSkipAll, fsourCancel);
var
  Message: String;
  PossibleResponses: array of TFileSourceOperationUIResponse;
  TargetFile: TFile;
begin
  case FFileExistsOption of
    fsoofeNone:
      begin
        case AllowResume of
          True :  PossibleResponses := Responses;
          False:  PossibleResponses := ResponsesNoResume;
        end;
        if FMode = wpohmCopyOut then
          Message := uFileSystemUtil.FileExistsMessage(AbsoluteTargetFileName, aFile.FullPath, aFile.Size, aFile.ModificationTime)
        else if FWfxPluginFileSource.FillSingleFile(AbsoluteTargetFileName, TargetFile) then
        begin
          Message := FileExistsMessage(TargetFile, aFile);
          TargetFile.Free;
        end
        else
          Message := Format(rsMsgFileExistsRwrt, [AbsoluteTargetFileName]);
        case AskQuestion(Message, '',
                         PossibleResponses, fsourOverwrite, fsourSkip) of
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

constructor TWfxPluginOperationHelper.Create(FileSource: IFileSource;
                                             AskQuestionFunction: TAskQuestionFunction;
                                             AbortOperationFunction: TAbortOperationFunction;
                                             CheckOperationStateFunction: TCheckOperationStateFunction;
                                             UpdateStatisticsFunction: TUpdateStatisticsFunction;
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

procedure TWfxPluginOperationHelper.ProcessFiles(aFiles: TFiles; var Statistics: TFileSourceCopyOperationStatistics);
var
  I: Integer;
  iResult: LongInt;
  sTargetFile : String;
  aFile: TFile;
begin
  FRenamingFiles := (FRenameMask <> '*.*') and (FRenameMask <> '');

  // If there is a single root dir and rename mask doesn't have wildcards
  // treat is as a rename of the root dir.
  if (aFiles.Count = 1) and FRenamingFiles then
  begin
    aFile := aFiles[0];
    if (aFile.IsDirectory or aFile.IsLinkToDirectory) and
       not ContainsWildcards(FRenameMask) then
    begin
      FRenamingFiles := False;
      FRenamingRootDir := True;
    end;
  end;

  for I:= 0 to aFiles.Count - 1 do
    with FWfxPluginFileSource do
    begin
      aFile:= aFiles.Items[I];

      // Filenames must be relative to the current directory.
      sTargetFile := FRootTargetPath + ExtractDirLevel(aFiles.Path, aFile.Path);

      if FRenamingRootDir then
        sTargetFile := sTargetFile + FRenameMask
      else
        sTargetFile := sTargetFile + ApplyRenameMask(aFile, FRenameNameMask, FRenameExtMask);

      //DCDebug('Source name == ' + aFile.FullPath);
      //DCDebug('Target name == ' + sTargetFile);

      with Statistics do
      begin
        CurrentFileFrom := aFile.Path + aFile.Name;
        CurrentFileTo := sTargetFile;
        CurrentFileTotalBytes := (aFile.Properties[fpSize] as TFileSizeProperty).Value;
        CurrentFileDoneBytes := 0;
      end;

      UpdateStatistics(Statistics);

      if not aFile.IsDirectory then
        iResult := ProcessFile(aFile, sTargetFile, Statistics)
      else
        iResult := ProcessDirectory(aFile, sTargetFile);

      if iResult = FS_FILE_USERABORT then AbortOperation();

      if iResult = FS_FILE_OK then CopyProperties(aFile, sTargetFile);

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

