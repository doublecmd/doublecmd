unit uFileSystemSetDateTimeOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSetDateTimeOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uFileSystemFile,
  uGlobs, uLog, uOSUtils;

type

  TFileSystemSetDateTimeOperation = class(TFileSourceSetDateTimeOperation)

  private
    FFullFilesTreeToSetDateTime: TFileSystemFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceSetDateTimeOperationStatistics; // local copy of statistics

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FSkipErrors: Boolean;

  protected
    function ProcessFile(aFile: TFileSystemFile): Boolean;

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
  uLng, uFileSystemUtil;

constructor TFileSystemSetDateTimeOperation.Create(aTargetFileSource: IFileSource;
                                              var theFilesToSetDateTime: TFiles; aLastWriteTime: TDateTime);
begin
  FSymLinkOption := fsooslNone;
  FSkipErrors := False;
  FFullFilesTreeToSetDateTime := nil;

  inherited Create(aTargetFileSource, theFilesToSetDateTime, aLastWriteTime);
end;

destructor TFileSystemSetDateTimeOperation.Destroy;
begin
  inherited Destroy;

  if Recursive then
  begin
    if Assigned(FFullFilesTreeToSetDateTime) then
      FreeAndNil(FFullFilesTreeToSetDateTime);
  end;
end;

procedure TFileSystemSetDateTimeOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  if not Recursive then
    begin
      FFullFilesTreeToSetDateTime:= FilesToSetDateTime as TFileSystemFiles;
      FStatistics.TotalFiles:= FFullFilesTreeToSetDateTime.Count;
    end
  else
    begin
      FillAndCount(FilesToSetDateTime as TFileSystemFiles,
                   FFullFilesTreeToSetDateTime,
                   FStatistics.TotalFiles,
                   FStatistics.TotalBytes);     // gets full list of files (recursive)
    end;
end;

procedure TFileSystemSetDateTimeOperation.MainExecute;
var
  aFile: TFileSystemFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := FFullFilesTreeToSetDateTime.Count - 1 downto 0 do
  begin
    aFile := FFullFilesTreeToSetDateTime[CurrentFileIndex] as TFileSystemFile;

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

procedure TFileSystemSetDateTimeOperation.Finalize;
begin
end;

function TFileSystemSetDateTimeOperation.ProcessFile(aFile: TFileSystemFile): Boolean;
var
  FileName: String;
  bRetry: Boolean;
  sMessage, sQuestion: String;
  fdLastWriteTime,
  fdCreationTime,
  fdLastAccessTime: LongInt;
begin
  Result := False;
  FileName := aFile.Path + aFile.Name;

  fdLastWriteTime:= DateTimeToFileDate(LastWriteTime);
  fdCreationTime:= DateTimeToFileDate(CreationTime);
  fdLastAccessTime:= DateTimeToFileDate(LastAccessTime);

  repeat
    bRetry := False;
    Result:= mbFileSetTime(FileName, fdLastWriteTime, fdCreationTime, fdLastAccessTime) <> 0;

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

