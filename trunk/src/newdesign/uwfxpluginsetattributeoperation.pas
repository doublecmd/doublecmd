unit uWfxPluginSetAttributeOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSetAttributeOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uFile,
  uWfxPluginFileSource,
  uWfxPluginFile,
  uGlobs, uLog, uOSUtils;

type

  TWfxPluginSetAttributeOperation = class(TFileSourceSetAttributeOperation)

  private
    FWfxPluginFileSource: IWfxPluginFileSource;
    FFullFilesTreeToSetAttribute: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceSetAttributeOperationStatistics; // local copy of statistics

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FSkipErrors: Boolean;

  protected
    function ProcessFile(aFile: TWfxPluginFile): Boolean;

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
  uFileProperty, uLng, uDCUtils, WfxPlugin;

constructor TWfxPluginSetAttributeOperation.Create(aTargetFileSource: IFileSource;
                                              var theFilesToSetAttribute: TFiles; aNewAttributes: TFileAttrs);
begin
  FSymLinkOption := fsooslNone;
  FSkipErrors := False;
  FFullFilesTreeToSetAttribute := nil;
  FWfxPluginFileSource:= aTargetFileSource as IWfxPluginFileSource;

  inherited Create(aTargetFileSource, theFilesToSetAttribute, aNewAttributes);
end;

destructor TWfxPluginSetAttributeOperation.Destroy;
begin
  inherited Destroy;

  if Recursive then
  begin
    if Assigned(FFullFilesTreeToSetAttribute) then
      FreeAndNil(FFullFilesTreeToSetAttribute);
  end;
end;

procedure TWfxPluginSetAttributeOperation.Initialize;
begin
  with FWfxPluginFileSource do
  WfxModule.WfxStatusInfo(FilesToSetAttribute.Path, FS_STATUS_START, FS_STATUS_OP_ATTRIB);
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  if not Recursive then
    begin
      FFullFilesTreeToSetAttribute:= FilesToSetAttribute;
      FStatistics.TotalFiles:= FFullFilesTreeToSetAttribute.Count;
    end
  else
    begin
      FWfxPluginFileSource.FillAndCount(FilesToSetAttribute,
                                        FFullFilesTreeToSetAttribute,
                                        FStatistics.TotalFiles,
                                        FStatistics.TotalBytes);     // gets full list of files (recursive)
    end;
end;

procedure TWfxPluginSetAttributeOperation.MainExecute;
var
  aFile: TWfxPluginFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := FFullFilesTreeToSetAttribute.Count - 1 downto 0 do
  begin
    aFile := FFullFilesTreeToSetAttribute[CurrentFileIndex] as TWfxPluginFile;

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

procedure TWfxPluginSetAttributeOperation.Finalize;
begin
  with FWfxPluginFileSource do
  WfxModule.WfxStatusInfo(FilesToSetAttribute.Path, FS_STATUS_END, FS_STATUS_OP_ATTRIB);
end;

function TWfxPluginSetAttributeOperation.ProcessFile(aFile: TWfxPluginFile): Boolean;
var
  FileName: String;
  bRetry: Boolean;
  sMessage, sQuestion: String;
begin
  Result := False;
  FileName := aFile.Path + aFile.Name;

  repeat
    bRetry := False;

    with FWfxPluginFileSource.WfxModule do
    if aFile.Properties[fpAttributes] is TNtfsFileAttributesProperty then
      Result:= WfxSetAttr(FileName, NewAttributes)
    else if aFile.Properties[fpAttributes] is TUnixFileAttributesProperty then
      Result:= WfxExecuteFile(0, FileName, 'chmod' + #32 + DecToOct(NewAttributes)) = FS_EXEC_OK;

    if not Result then
      begin
        sMessage := Format(rsMsgLogError + rsMsgErrSetAttribute, [FileName]);
        sQuestion := Format(rsMsgErrSetAttribute, [FileName]);

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

