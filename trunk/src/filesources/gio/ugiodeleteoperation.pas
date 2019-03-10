unit uGioDeleteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceDeleteOperation,
  uGioFileSource,
  uFileSource,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uGlobs, uLog;

type

  TGioDeleteOperation = class(TFileSourceDeleteOperation)

  private
    FWfxPluginFileSource: IGioFileSource;
    FFullFilesTreeToDelete: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceDeleteOperationStatistics; // local copy of statistics

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FSkipErrors: Boolean;
    FDeleteReadOnly: TFileSourceOperationOptionGeneral;

  protected
    function ProcessFile(aFile: TFile): Boolean;
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
  DCOSUtils, uLng, uGlib2, uGio2, uGObject2, uGio, uGioFileSourceUtil;

constructor TGioDeleteOperation.Create(aTargetFileSource: IFileSource;
                                             var theFilesToDelete: TFiles);
begin
  FSymLinkOption := fsooslNone;
  FSkipErrors := False;
  FDeleteReadOnly := fsoogNone;
  FFullFilesTreeToDelete := nil;
  FWfxPluginFileSource:= aTargetFileSource as IGioFileSource;

  inherited Create(aTargetFileSource, theFilesToDelete);
end;

destructor TGioDeleteOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TGioDeleteOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FillAndCount(FilesToDelete, True,
               FFullFilesTreeToDelete,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)
end;

procedure TGioDeleteOperation.MainExecute;
var
  aFile: TFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := FFullFilesTreeToDelete.Count - 1 downto 0 do
  begin
    aFile := FFullFilesTreeToDelete[CurrentFileIndex];

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

procedure TGioDeleteOperation.Finalize;
begin

end;

function TGioDeleteOperation.ProcessFile(aFile: TFile): Boolean;
var
  AGFile: PGFile;
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

    AGFile:= GioNewFile(FileName);
    Result:= g_file_delete(AGFile, nil, nil);
    g_object_unref(PGObject(AGFile));

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

function TGioDeleteOperation.ShowError(sMessage: String): TFileSourceOperationUIResponse;
begin
  if gSkipFileOpError then
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

procedure TGioDeleteOperation.LogMessage(sMessage: String; logOptions: TLogOptions; logMsgType: TLogMsgType);
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

