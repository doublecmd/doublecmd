unit uWfxPluginMoveOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceMoveOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI,
  uFile,
  uWfxPluginFileSource,
  uWfxPluginUtil;

type

  { TWfxPluginMoveOperation }

  TWfxPluginMoveOperation = class(TFileSourceMoveOperation)

  private
    FWfxPluginFileSource: IWfxPluginFileSource;
    FOperationHelper: TWfxPluginOperationHelper;
    FCallbackDataClass: TCallbackDataClass;
    FFullFilesTreeToCopy: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceMoveOperationStatistics; // local copy of statistics
    // Options
    FInfoOperation: LongInt;
  protected
    function UpdateProgress(SourceName, TargetName: PAnsiChar; PercentDone: Integer): Integer;

  public
    constructor Create(aFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

    class function GetOptionsUIClass: TFileSourceOperationOptionsUIClass; override;

  end;

implementation

uses
  fWfxPluginCopyMoveOperationOptions, WfxPlugin;

// -- TWfxPluginMoveOperation ---------------------------------------------

function TWfxPluginMoveOperation.UpdateProgress(SourceName, TargetName: PAnsiChar;
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
    if Assigned(SourceName) then begin
      FStatistics.CurrentFileFrom:= SourceName;
    end;
    if Assigned(TargetName) then begin
      FStatistics.CurrentFileTo:= TargetName;
    end;

    iTemp:= CurrentFileTotalBytes * PercentDone div 100;
    DoneBytes := DoneBytes + (iTemp - CurrentFileDoneBytes);
    CurrentFileDoneBytes:= iTemp;

    UpdateStatistics(FStatistics);
  end;

  if not AppProcessMessages(True) then
    Exit(1);
end;

constructor TWfxPluginMoveOperation.Create(aFileSource: IFileSource;
                                           var theSourceFiles: TFiles;
                                           aTargetPath: String);
begin
  FWfxPluginFileSource:= aFileSource as IWfxPluginFileSource;
  with FWfxPluginFileSource do
  FCallbackDataClass:= TCallbackDataClass(WfxOperationList.Objects[PluginNumber]);

  if theSourceFiles.Count > 1 then
    FInfoOperation:= FS_STATUS_OP_RENMOV_MULTI
  else
    FInfoOperation:= FS_STATUS_OP_RENMOV_SINGLE;

  inherited Create(aFileSource, theSourceFiles, aTargetPath);
end;

destructor TWfxPluginMoveOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TWfxPluginMoveOperation.Initialize;
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
                        @ShowCompareFilesUI,
                        @ShowCompareFilesUIByFileObject,
                        Thread,
                        wpohmMove,
                        TargetPath);

  FOperationHelper.RenameMask := RenameMask;
  FOperationHelper.FileExistsOption := FileExistsOption;

  FOperationHelper.Initialize;
end;

procedure TWfxPluginMoveOperation.MainExecute;
begin
  FOperationHelper.ProcessFiles(FFullFilesTreeToCopy, FStatistics);
end;

procedure TWfxPluginMoveOperation.Finalize;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(SourceFiles.Path, FS_STATUS_END, FInfoOperation);
    FCallbackDataClass.UpdateProgressFunction:= nil;
    UpdateProgressFunction:= nil;
  end;
  FileExistsOption := FOperationHelper.FileExistsOption;
  FOperationHelper.Free;
end;

class function TWfxPluginMoveOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result := TWfxPluginMoveOperationOptionsUI;
end;

end.

