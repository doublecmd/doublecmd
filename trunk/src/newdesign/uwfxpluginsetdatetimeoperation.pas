unit uWfxPluginSetDateTimeOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSetDateTimeOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uWfxPluginFileSource,
  uWfxPluginFile,
  uGlobs, uLog;

type

  TWfxPluginSetDateTimeOperation = class(TFileSourceSetDateTimeOperation)

  private
    FWfxPluginFileSource: IWfxPluginFileSource;
    FFullFilesTreeToSetDateTime: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceSetDateTimeOperationStatistics; // local copy of statistics

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FSkipErrors: Boolean;

  protected
    function ProcessFile(aFile: TWfxPluginFile): Boolean;

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToSetDateTime: TFiles; aLastWriteTime: TDateTime); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

  end;

implementation

uses
  uFileProperty, uLng, uDCUtils, WfxPlugin, uOSUtils;

constructor TWfxPluginSetDateTimeOperation.Create(aTargetFileSource: IFileSource;
                                              var theFilesToSetDateTime: TFiles; aLastWriteTime: TDateTime);
begin
  FSymLinkOption := fsooslNone;
  FSkipErrors := False;
  FFullFilesTreeToSetDateTime := nil;
  FWfxPluginFileSource:= aTargetFileSource as IWfxPluginFileSource;

  inherited Create(aTargetFileSource, theFilesToSetDateTime, aLastWriteTime);
end;

destructor TWfxPluginSetDateTimeOperation.Destroy;
begin
  inherited Destroy;

  if Recursive then
  begin
    if Assigned(FFullFilesTreeToSetDateTime) then
      FreeAndNil(FFullFilesTreeToSetDateTime);
  end;
end;

procedure TWfxPluginSetDateTimeOperation.Initialize;
begin
  with FWfxPluginFileSource do
  WfxModule.WfxStatusInfo(FilesToSetDateTime.Path, FS_STATUS_START, FS_STATUS_OP_ATTRIB);
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  if not Recursive then
    begin
      FFullFilesTreeToSetDateTime:= FilesToSetDateTime;
      FStatistics.TotalFiles:= FFullFilesTreeToSetDateTime.Count;
    end
  else
    begin
      FWfxPluginFileSource.FillAndCount(FilesToSetDateTime,
                                        FFullFilesTreeToSetDateTime,
                                        FStatistics.TotalFiles,
                                        FStatistics.TotalBytes);     // gets full list of files (recursive)
    end;
end;

procedure TWfxPluginSetDateTimeOperation.MainExecute;
var
  aFile: TWfxPluginFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := FFullFilesTreeToSetDateTime.Count - 1 downto 0 do
  begin
    aFile := FFullFilesTreeToSetDateTime[CurrentFileIndex] as TWfxPluginFile;

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

procedure TWfxPluginSetDateTimeOperation.Finalize;
begin
  with FWfxPluginFileSource do
  WfxModule.WfxStatusInfo(FilesToSetDateTime.Path, FS_STATUS_END, FS_STATUS_OP_ATTRIB);
end;

function TWfxPluginSetDateTimeOperation.ProcessFile(aFile: TWfxPluginFile): Boolean;
var
  FileName: String;
  bRetry: Boolean;
  sMessage, sQuestion: String;
  ftLastWriteTime,
  ftCreationTime,
  ftLastAccessTime: TFileTime;
begin
  Result := False;
  FileName := aFile.Path + aFile.Name;

  ftLastWriteTime:= DateTimeToFileTime(LastWriteTime);
  ftCreationTime:= DateTimeToFileTime(CreationTime);
  ftLastAccessTime:= DateTimeToFileTime(LastAccessTime);

  repeat
    bRetry := False;

    with FWfxPluginFileSource.WfxModule do
    Result:= WfxSetTime(FileName, ftCreationTime, ftLastAccessTime, ftLastWriteTime);

    if not Result then
      begin
        sMessage := Format(rsMsgLogError + rsMsgErrSetDateTime, [FileName]);
        sQuestion := Format(rsMsgErrSetDateTime, [FileName]);

        if gSkipFileOpError or (FSkipErrors = True) then
          logWrite(Thread, sMessage, lmtError)
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

end.

