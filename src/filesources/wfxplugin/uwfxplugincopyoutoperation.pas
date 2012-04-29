unit uWfxPluginCopyOutOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI,
  uFile,
  uWfxPluginFileSource,
  uWfxPluginUtil;

type

  { TWfxPluginCopyOutOperation }

  TWfxPluginCopyOutOperation = class(TFileSourceCopyOutOperation)

  private
    FWfxPluginFileSource: IWfxPluginFileSource;
    FOperationHelper: TWfxPluginOperationHelper;
    FCallbackDataClass: TCallbackDataClass;
    FFullFilesTreeToCopy: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    // Options
    FInfoOperation: LongInt;
    FFileExistsOption: TFileSourceOperationOptionFileExists;
  protected
    function UpdateProgress(SourceName, TargetName: UTF8String; PercentDone: Integer): Integer;

  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

    class function GetOptionsUIClass: TFileSourceOperationOptionsUIClass; override;

    property FileExistsOption: TFileSourceOperationOptionFileExists read FFileExistsOption write FFileExistsOption;
    property NeedsConnection: Boolean read FNeedsConnection write FNeedsConnection;

  end;

implementation

uses
  fWfxPluginCopyMoveOperationOptions, WfxPlugin;

// -- TWfxPluginCopyOutOperation ---------------------------------------------

function TWfxPluginCopyOutOperation.UpdateProgress(SourceName, TargetName: UTF8String;
                                                   PercentDone: Integer): Integer;
var
  iTemp: Int64;
begin
  Result := 0;

  //DCDebug('SourceName=', SourceName, #32, 'TargetName=', TargetName, #32, 'PercentDone=', IntToStr(PercentDone));

  if State = fsosStopping then  // Cancel operation
    Exit(1);

  with FStatistics do
  begin
    FStatistics.CurrentFileFrom:= SourceName;
    FStatistics.CurrentFileTo:= TargetName;

    iTemp:= CurrentFileTotalBytes * PercentDone div 100;
    DoneBytes := DoneBytes + (iTemp - CurrentFileDoneBytes);
    CurrentFileDoneBytes:= iTemp;

    UpdateStatistics(FStatistics);
  end;

  CheckOperationState;
end;

constructor TWfxPluginCopyOutOperation.Create(aSourceFileSource: IFileSource;
                                              aTargetFileSource: IFileSource;
                                              var theSourceFiles: TFiles;
                                              aTargetPath: String);
begin
  FWfxPluginFileSource:= aSourceFileSource as IWfxPluginFileSource;
  with FWfxPluginFileSource do
  FCallbackDataClass:= TCallbackDataClass(WfxOperationList.Objects[PluginNumber]);

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);

  if (FNeedsConnection = False) then
    FInfoOperation:= FS_STATUS_OP_GET_MULTI_THREAD
  else if (SourceFiles.Count > 1) then
    FInfoOperation:= FS_STATUS_OP_GET_MULTI
  else
    FInfoOperation:= FS_STATUS_OP_GET_SINGLE;
end;

destructor TWfxPluginCopyOutOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TWfxPluginCopyOutOperation.Initialize;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(SourceFiles.Path, FS_STATUS_START, FInfoOperation);
    FCallbackDataClass.UpdateProgressFunction:= @UpdateProgress;
    UpdateProgressFunction:= @UpdateProgress;
    // Get initialized statistics; then we change only what is needed.
    FStatistics := RetrieveStatistics;

    FillAndCount(SourceFiles, False, False,
                 FFullFilesTreeToCopy,
                 FStatistics.TotalFiles,
                 FStatistics.TotalBytes);     // gets full list of files (recursive)
  end;

  if Assigned(FOperationHelper) then
    FreeAndNil(FOperationHelper);

  FOperationHelper := TWfxPluginOperationHelper.Create(
                        FWfxPluginFileSource,
                        @AskQuestion,
                        @RaiseAbortOperation,
                        @CheckOperationState,
                        @UpdateStatistics,
                        Thread,
                        wpohmCopyOut,
                        TargetPath);

  FOperationHelper.RenameMask := RenameMask;
  FOperationHelper.FileExistsOption := FileExistsOption;

  FOperationHelper.Initialize;
end;

procedure TWfxPluginCopyOutOperation.MainExecute;
begin
  FOperationHelper.ProcessFiles(FFullFilesTreeToCopy, FStatistics);
end;

procedure TWfxPluginCopyOutOperation.Finalize;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(SourceFiles.Path, FS_STATUS_END, FInfoOperation);
    FCallbackDataClass.UpdateProgressFunction:= nil;
    UpdateProgressFunction:= nil;
  end;
end;

class function TWfxPluginCopyOutOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result := TWfxPluginCopyOutOperationOptionsUI;
end;

end.

