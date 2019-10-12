unit uFileSystemDeleteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceDeleteOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uDescr, uGlobs, uLog;

type

  { TFileSystemDeleteOperation }

  TFileSystemDeleteOperation = class(TFileSourceDeleteOperation)

  private
    FFullFilesTreeToDelete: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceDeleteOperationStatistics; // local copy of statistics
    FDescription: TDescription;

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FSkipErrors: Boolean;
    FRecycle: Boolean;
    FDeleteReadOnly,
    FDeleteDirectly: TFileSourceOperationOptionGeneral;

    procedure DeleteSubDirectory(const aFile: TFile);

  protected
    procedure ProcessFile(aFile: TFile);
    procedure ProcessList(aFiles: TFiles);
    function ShowError(sMessage: String): TFileSourceOperationUIResponse;
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToDelete: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

    // For delete to trash
    property Recycle : boolean read FRecycle write FRecycle default false;
    property DeleteReadOnly: TFileSourceOperationOptionGeneral read FDeleteReadOnly write FDeleteReadOnly;
    property SymLinkOption: TFileSourceOperationOptionSymLink read FSymLinkOption write FSymLinkOption;
    property SkipErrors: Boolean read FSkipErrors write FSkipErrors;
  end;

implementation

uses
  DCOSUtils, DCStrUtils, uLng, uFileSystemUtil, uTrash, uAdministrator
{$IF DEFINED(MSWINDOWS)}
  , Windows,  uFileUnlock, fFileUnlock, uSuperUser
{$ENDIF}
  ;

constructor TFileSystemDeleteOperation.Create(aTargetFileSource: IFileSource;
                                              var theFilesToDelete: TFiles);
begin
  FSymLinkOption := fsooslNone;
  FSkipErrors := gSkipFileOpError;
  FRecycle := False;
  FDeleteReadOnly := fsoogNone;
  FDeleteDirectly:= fsoogNone;

  if gProcessComments then
    FDescription := TDescription.Create(True);

  inherited Create(aTargetFileSource, theFilesToDelete);
end;

destructor TFileSystemDeleteOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FDescription) then
  begin
    FDescription.SaveDescription;
    FreeAndNil(FDescription);
  end;

  if not FRecycle then
  begin
    FreeAndNil(FFullFilesTreeToDelete);
  end;
end;

procedure TFileSystemDeleteOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  if FRecycle then
    begin
      FFullFilesTreeToDelete:= FilesToDelete;
      FStatistics.TotalFiles:= FFullFilesTreeToDelete.Count;
    end
  else
    begin
      FillAndCount(FilesToDelete, True, False,
                   FFullFilesTreeToDelete,
                   FStatistics.TotalFiles,
                   FStatistics.TotalBytes);     // gets full list of files (recursive)
    end;

  if gProcessComments then
    FDescription.Clear;
end;

procedure TFileSystemDeleteOperation.MainExecute;
begin
  ProcessList(FFullFilesTreeToDelete);
end;

procedure TFileSystemDeleteOperation.Finalize;
begin
end;

procedure TFileSystemDeleteOperation.DeleteSubDirectory(const aFile: TFile);
var
  RootFiles: TFiles = nil;
  SubFiles: TFiles = nil;
  FilesCount, BytesCount: Int64;
begin
  RootFiles := TFiles.Create(aFile.Path);
  try
    RootFiles.Add(aFile.Clone);
    // Only count statistics for subfiles because statistics for the root dir
    // have already been counted.
    FillAndCount(RootFiles, True, True, SubFiles, FilesCount, BytesCount);

    FStatistics.TotalFiles := FStatistics.TotalFiles + FilesCount;
    FStatistics.TotalBytes := FStatistics.TotalBytes + BytesCount;

    // Only now insert root directory.
    SubFiles.Insert(aFile.Clone, 0);

    // This function will only be called if deleting to trash failed
    // so we can assume Recycle is True. Turn off temporarily as we delete this subdirectory.
    FRecycle := False;

    ProcessList(SubFiles);

  finally
    RootFiles.Free;
    SubFiles.Free;
    FRecycle := True;
  end;
end;

procedure TFileSystemDeleteOperation.ProcessFile(aFile: TFile);
const
  ResponsesError: array[0..3] of TFileSourceOperationUIResponse
    = (fsourRetry, fsourSkip, fsourSkipAll, fsourAbort);
var
  FileName: String;
  bRetry: Boolean;
  LastError: Integer;
  RemoveDirectly: TFileSourceOperationOptionGeneral = fsoogNone;
  sMessage, sQuestion: String;
  logOptions: TLogOptions;
  DeleteResult: Boolean;
  PossibleResponses: array of TFileSourceOperationUIResponse;
{$IF DEFINED(MSWINDOWS)}
  ProcessInfo: TProcessInfoArray;
{$ENDIF}
begin
  FileName := aFile.FullPath;

  if FileIsReadOnly(aFile.Attributes) then
  begin
    case FDeleteReadOnly of
      fsoogNone:
        case AskQuestion(Format(rsMsgFileReadOnly, [WrapTextSimple(FileName)]), '',
                         [fsourYes, fsourSkip, fsourAbort, fsourAll, fsourSkipAll],
                         fsourYes, fsourAbort) of
          fsourAll:
            FDeleteReadOnly := fsoogYes;
          fsourSkip:
            Exit;
          fsourSkipAll:
            begin
              FDeleteReadOnly := fsoogNo;
              Exit;
            end;
          fsourAbort:
            RaiseAbortOperation;
        end;

       fsoogNo:
         Exit;
    end;
  end;

{$IF DEFINED(MSWINDOWS)}
  ElevateAction:= dupError;
{$ENDIF}

  repeat
    bRetry := False;

    if (FRecycle = False) then
    begin
      if FileIsReadOnly(aFile.Attributes) then
        FileSetReadOnlyUAC(FileName, False);

      if aFile.IsDirectory then // directory
      begin
        DeleteResult := RemoveDirectoryUAC(FileName);
      end
      else
      begin // files and other stuff
        DeleteResult := DeleteFileUAC(FileName);
      end;
    end
    else
    begin
      // Delete to trash (one function for file and folder)
      DeleteResult:= FileTrashUtf8(FileName);
      if not DeleteResult then begin
        DeleteResult:= not mbFileSystemEntryExists(FileName);
      end;
      if not DeleteResult then
        begin
          case FDeleteDirectly of
            fsoogNone:
              begin
                case AskQuestion(Format(rsMsgDelToTrashForce, [WrapTextSimple(FileName)]), '',
                                 [fsourYes, fsourAll, fsourSkip, fsourSkipAll, fsourAbort],
                                 fsourYes, fsourAbort) of
                  fsourYes:
                    RemoveDirectly:= fsoogYes;
                  fsourAll:
                    begin
                      FDeleteDirectly := fsoogYes;
                      RemoveDirectly:= fsoogYes;
                    end;
                  fsourSkip:
                    RemoveDirectly:= fsoogNo;
                  fsourSkipAll:
                    begin
                      FDeleteDirectly := fsoogNo;
                      RemoveDirectly:= fsoogNo;
                    end;
                  fsourAbort:
                    RaiseAbortOperation;
                end;
              end;
            fsoogYes:
              RemoveDirectly:= fsoogYes;
            fsoogNo:
              RemoveDirectly:= fsoogNo;
          end;
          if RemoveDirectly = fsoogYes then
            begin
              if aFile.IsLinkToDirectory then
                begin
                  DeleteResult := RemoveDirectoryUAC(FileName);
                end
              else if aFile.IsDirectory then // directory
                begin
                  DeleteSubDirectory(aFile);
                  // This directory has already been processed.
                  Exit;
                end
              else  // files and other stuff
                begin
                  DeleteResult := DeleteFileUAC(FileName);
                end;
            end;
        end;
    end;

    if DeleteResult then
    begin // success
      // process comments if need
      if gProcessComments then
        FDescription.DeleteDescription(FileName);

      if aFile.IsDirectory then
      begin
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogRmDir, [FileName]), [log_dir_op, log_delete], lmtSuccess);
      end
      else
      begin
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogDelete, [FileName]), [log_delete], lmtSuccess);
      end;
    end
    else // error
    begin
      if aFile.IsDirectory then
      begin
        logOptions := [log_dir_op, log_delete];
        sMessage := Format(rsMsgLogError + rsMsgLogRmDir, [FileName]);
        sQuestion := Format(rsMsgCannotDeleteDirectory, [FileName]);
      end
      else
      begin
        logOptions := [log_delete];
        sMessage := Format(rsMsgLogError + rsMsgLogDelete, [FileName]);
        sQuestion := Format(rsMsgNotDelete, [FileName]);
      end;

      if FSkipErrors or (RemoveDirectly = fsoogNo) then
        LogMessage(sMessage, logOptions, lmtError)
      else
      begin
        if (FRecycle = False) or (RemoveDirectly = fsoogYes) then
        begin
          LastError:= GetLastOSError;
{$IF DEFINED(MSWINDOWS)}
          if GetFileInUseProcessFast(FileName, ProcessInfo) then
          begin
            sQuestion+= LineEnding + LineEnding + rsMsgOpenInAnotherProgram + LineEnding;
            sQuestion+= LineEnding + Format(rsMsgProcessId, [ProcessInfo[0].ProcessId]) + LineEnding;
            if (Length(ProcessInfo[0].ApplicationName) > 0) then begin
              sQuestion+= Format(rsMsgApplicationName, [ProcessInfo[0].ApplicationName]) + LineEnding;
            end;
            if (Length(ProcessInfo[0].ExecutablePath) > 0) then begin
              sQuestion+= Format(rsMsgExecutablePath, [ProcessInfo[0].ExecutablePath]) + LineEnding;
            end;
          end
          else
{$ENDIF}
          sQuestion+= LineEnding + mbSysErrorMessage(LastError);
        end;

{$IF DEFINED(MSWINDOWS)}
        if ElevationRequired(LastError) then
        begin
          SetLength(PossibleResponses, Length(ResponsesError) + 1);
          Move(ResponsesError[0], PossibleResponses[0], SizeOf(ResponsesError));
          PossibleResponses[High(PossibleResponses)]:= fsourRetryAdmin;
        end
        else
{$ENDIF}
        begin
          SetLength(PossibleResponses, Length(ResponsesError));
          Move(ResponsesError[0], PossibleResponses[0], SizeOf(ResponsesError));
        end;
{$IF DEFINED(MSWINDOWS)}
        if (Length(ProcessInfo) > 0) or (LastError = ERROR_ACCESS_DENIED) or (LastError = ERROR_SHARING_VIOLATION) then
        begin
          SetLength(PossibleResponses, Length(PossibleResponses) + 1);
          PossibleResponses[High(PossibleResponses)]:= fsourUnlock;
        end;
{$ENDIF}
        case AskQuestion(sQuestion, '',
                         PossibleResponses,
                         fsourRetry, fsourAbort) of
          fsourRetry:
            bRetry := True;
          fsourSkipAll:
            FSkipErrors := True;
          fsourAbort:
            RaiseAbortOperation;
{$IF DEFINED(MSWINDOWS)}
          fsourRetryAdmin:
            begin
              bRetry:= True;
              ElevateAction:= dupAccept;
            end;
          fsourUnlock:
            begin
              bRetry:= True;
              GetFileInUseProcessSlow(FileName, LastError, ProcessInfo);
              ShowUnlockForm(ProcessInfo);
            end;
{$ENDIF}
        end;
      end;
    end;
  until bRetry = False;
end;

procedure TFileSystemDeleteOperation.ProcessList(aFiles: TFiles);
var
  aFile: TFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := aFiles.Count - 1 downto 0 do
  begin
    aFile := aFiles[CurrentFileIndex];

    FStatistics.CurrentFile := aFile.FullPath;
    UpdateStatistics(FStatistics);

    ProcessFile(aFile);

    with FStatistics do
    begin
      DoneFiles := DoneFiles + 1;
      DoneBytes := DoneBytes + aFile.Size;
    end;
    UpdateStatistics(FStatistics);

    CheckOperationState;
  end;
end;

function TFileSystemDeleteOperation.ShowError(sMessage: String): TFileSourceOperationUIResponse;
begin
  if FSkipErrors then
  begin
    logWrite(Thread, sMessage, lmtError, True);
    Result := fsourSkip;
  end
  else
  begin
    Result := AskQuestion(sMessage, '', [fsourSkip, fsourCancel], fsourSkip, fsourCancel);
    if Result = fsourCancel then
      RaiseAbortOperation;
  end;
end;

procedure TFileSystemDeleteOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

