unit uFileSystemSetAttributeOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSetAttributeOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uFileSystemFile,
  uGlobs, uLog, uOSUtils;

type

  TFileSystemSetAttributeOperation = class(TFileSourceSetAttributeOperation)

  private
    FFullFilesTreeToSetAttribute: TFileSystemFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceSetAttributeOperationStatistics; // local copy of statistics

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FSkipErrors: Boolean;

  protected
    function ProcessFile(aFile: TFileSystemFile): Boolean;

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToSetAttribute: TFiles; aNewAttributes: TFileAttrs); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

  end;

implementation

uses
  uLng, uFileSystemUtil;

constructor TFileSystemSetAttributeOperation.Create(aTargetFileSource: IFileSource;
                                              var theFilesToSetAttribute: TFiles; aNewAttributes: TFileAttrs);
begin
  FSymLinkOption := fsooslNone;
  FSkipErrors := False;
  FFullFilesTreeToSetAttribute := nil;

  inherited Create(aTargetFileSource, theFilesToSetAttribute, aNewAttributes);
end;

destructor TFileSystemSetAttributeOperation.Destroy;
begin
  inherited Destroy;

  if Recursive then
  begin
    if Assigned(FFullFilesTreeToSetAttribute) then
      FreeAndNil(FFullFilesTreeToSetAttribute);
  end;
end;

procedure TFileSystemSetAttributeOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  if not Recursive then
    begin
      FFullFilesTreeToSetAttribute:= FilesToSetAttribute as TFileSystemFiles;
      FStatistics.TotalFiles:= FFullFilesTreeToSetAttribute.Count;
    end
  else
    begin
      FillAndCount(FilesToSetAttribute as TFileSystemFiles,
                   FFullFilesTreeToSetAttribute,
                   FStatistics.TotalFiles,
                   FStatistics.TotalBytes);     // gets full list of files (recursive)
    end;
end;

procedure TFileSystemSetAttributeOperation.MainExecute;
var
  aFile: TFileSystemFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := FFullFilesTreeToSetAttribute.Count - 1 downto 0 do
  begin
    aFile := FFullFilesTreeToSetAttribute[CurrentFileIndex] as TFileSystemFile;

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

procedure TFileSystemSetAttributeOperation.Finalize;
begin
end;

function TFileSystemSetAttributeOperation.ProcessFile(aFile: TFileSystemFile): Boolean;
var
  FileName: String;
  bRetry: Boolean;
  sMessage, sQuestion: String;
begin
  Result := False;
  FileName := aFile.Path + aFile.Name;

  repeat
    bRetry := False;

    Result:= mbFileSetAttr(FileName, NewAttributes) = 0;

    if not Result then
      begin
        sMessage := Format(rsMsgLogError + 'Can not set attributes for "%s"', [FileName]);
        sQuestion := Format('Can not set attributes for "%s"', [FileName]);

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

