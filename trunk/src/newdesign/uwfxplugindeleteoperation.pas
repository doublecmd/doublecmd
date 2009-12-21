unit uWfxPluginDeleteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceDeleteOperation,
  uWfxPluginFileSource,
  uFileSource,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uWfxPluginFile,
  uGlobs, uLog;

type

  TWfxPluginDeleteOperation = class(TFileSourceDeleteOperation)

  private
    FWfxPluginFileSource: IWfxPluginFileSource;
    FFullFilesTreeToDelete: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceDeleteOperationStatistics; // local copy of statistics

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FSkipErrors: Boolean;
    FDeleteReadOnly: TFileSourceOperationOptionGeneral;

  protected
    function ProcessFile(aFile: TWfxPluginFile): Boolean;
    function ShowError(sMessage: String): TFileSourceOperationUIResponse;
    procedure LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToDelete: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  uOSUtils, uLng, WfxPlugin;

constructor TWfxPluginDeleteOperation.Create(aTargetFileSource: IFileSource;
                                             var theFilesToDelete: TFiles);
begin
  FSymLinkOption := fsooslNone;
  FSkipErrors := False;
  FDeleteReadOnly := fsoogNone;
  FFullFilesTreeToDelete := nil;
  FWfxPluginFileSource:= aTargetFileSource as IWfxPluginFileSource;

  inherited Create(aTargetFileSource, theFilesToDelete);
end;

destructor TWfxPluginDeleteOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TWfxPluginDeleteOperation.Initialize;
begin
  with FWfxPluginFileSource do
  WfxModule.WfxStatusInfo(FilesToDelete.Path, FS_STATUS_START, FS_STATUS_OP_DELETE);

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FWfxPluginFileSource.FillAndCount(FilesToDelete, True,
                                    FFullFilesTreeToDelete,
                                    FStatistics.TotalFiles,
                                    FStatistics.TotalBytes);     // gets full list of files (recursive)
end;

procedure TWfxPluginDeleteOperation.MainExecute;
var
  aFile: TWfxPluginFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := FFullFilesTreeToDelete.Count - 1 downto 0 do
  begin
    aFile := FFullFilesTreeToDelete[CurrentFileIndex] as TWfxPluginFile;

    FStatistics.CurrentFile := aFile.Path + aFile.Name;
    UpdateStatistics(FStatistics);

    ProcessFile(aFile);

    with FStatistics do
    begin
      DoneFiles := DoneFiles + 1;
      DoneBytes := DoneBytes + aFile.Size;

      UpdateStatistics(FStatistics);
    end;

    CheckOperationState;
  end;
end;

procedure TWfxPluginDeleteOperation.Finalize;
begin
  with FWfxPluginFileSource do
  WfxModule.WfxStatusInfo(FilesToDelete.Path, FS_STATUS_END, FS_STATUS_OP_DELETE);
end;

function TWfxPluginDeleteOperation.ProcessFile(aFile: TWfxPluginFile): Boolean;
var
  FileName: String;
  bRetry: Boolean;
  sMessage, sQuestion: String;
  logOptions: TLogOptions;
begin
  Result := False;
  FileName := aFile.Path + aFile.Name;

  if FileIsReadOnly(aFile.Attributes) then
  begin
    case FDeleteReadOnly of
      fsoogNone:
        case AskQuestion(Format(rsMsgFileReadOnly, [FileName]), '',
                         [fsourYes, fsourAll, fsourSkip, fsourSkipAll],
                         fsourYes, fsourSkip) of
          fsourAll:
            FDeleteReadOnly := fsoogYes;
          fsourSkip:
            Exit;
          fsourSkipAll:
            begin
              FDeleteReadOnly := fsoogNo;
              Exit;
            end;
        end;

       fsoogNo:
         Exit;
    end;
  end;

  repeat
    bRetry := False;

    //if FileIsReadOnly(aFile.Attributes) then
    //  mbFileSetReadOnly(FileName, False);

    with FWfxPluginFileSource.WfxModule do
    if aFile.IsDirectory then // directory
      begin
        Result := WfxRemoveDir(FileName);
      end
    else
      begin // files and other stuff
        Result := WfxDeleteFile(FileName);
      end;

    if Result then
    begin // success
      if aFile.IsDirectory then
      begin
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogRmDir, [FileName]), [log_vfs_op], lmtSuccess);
      end
      else
      begin
        LogMessage(Format(rsMsgLogSuccess + rsMsgLogDelete, [FileName]), [log_vfs_op], lmtSuccess);
      end;
    end
    else // error
    begin
      if aFile.IsDirectory then
      begin
        logOptions := [log_vfs_op];
        sMessage := Format(rsMsgLogError + rsMsgLogRmDir, [FileName]);
        sQuestion := Format(rsMsgNotDelete, [FileName]);
      end
      else
      begin
        logOptions := [log_vfs_op];
        sMessage := Format(rsMsgLogError + rsMsgLogDelete, [FileName]);
        sQuestion := Format(rsMsgNotDelete, [FileName]);
      end;

      if gSkipFileOpError or (FSkipErrors = True) then
        LogMessage(sMessage, logOptions, lmtError)
      else
      begin
        case AskQuestion(sQuestion, '',
                         [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                         fsourRetry, fsourSkip) of
          fsourRetry:
            bRetry := True;
          fsourSkipAll:
            FSkipErrors := True;
          fsourAbort:
            RaiseAbortOperation;
        end;
      end;
    end;
  until bRetry = False;
end;

function TWfxPluginDeleteOperation.ShowError(sMessage: String): TFileSourceOperationUIResponse;
begin
  if gSkipFileOpError then
  begin
    if Assigned(Thread) then
      logWrite(Thread, sMessage, lmtError, True)
    else
      logWrite(sMessage, lmtError, True);

    Result := fsourSkip;
  end
  else
  begin
    Result := AskQuestion(sMessage, '', [fsourSkip, fsourCancel], fsourSkip, fsourCancel);
    if Result = fsourCancel then
      RaiseAbortOperation;
  end;
end;

procedure TWfxPluginDeleteOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

