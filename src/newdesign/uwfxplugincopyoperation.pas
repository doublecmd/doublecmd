unit uWfxPluginCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFile,
  uWfxPluginFileSource,
  uWfxPluginUtil;

type

  { TWfxPluginCopyOperation }

  TWfxPluginCopyOperation = class(TFileSourceCopyOperation)

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

    property FileExistsOption: TFileSourceOperationOptionFileExists read FFileExistsOption write FFileExistsOption;

  end;

implementation

uses
  WfxPlugin;

// -- TWfxPluginCopyOperation ---------------------------------------------

function TWfxPluginCopyOperation.UpdateProgress(SourceName, TargetName: UTF8String;
                                                   PercentDone: Integer): Integer;
begin
  Result := 0;

  //DebugLn('SourceName=', SourceName, #32, 'TargetName=', TargetName, #32, 'PercentDone=', IntToStr(PercentDone));

  if State = fsosStopping then  // Cancel operation
    Exit(1);

  with FStatistics do
  begin
    FStatistics.CurrentFileFrom:= SourceName;
    FStatistics.CurrentFileTo:= TargetName;

    CurrentFileDoneBytes:= CurrentFileTotalBytes * PercentDone div 100;
    DoneBytes := DoneBytes + CurrentFileDoneBytes;

    UpdateStatistics(FStatistics);
  end;
end;

constructor TWfxPluginCopyOperation.Create(aSourceFileSource: IFileSource;
                                              aTargetFileSource: IFileSource;
                                              var theSourceFiles: TFiles;
                                              aTargetPath: String);
begin
  FWfxPluginFileSource:= aSourceFileSource as IWfxPluginFileSource;
  with FWfxPluginFileSource do
  FCallbackDataClass:= TCallbackDataClass(WfxOperationList.Objects[PluginNumber]);

  if theSourceFiles.Count > 1 then
    FInfoOperation:= FS_STATUS_OP_RENMOV_MULTI
  else
    FInfoOperation:= FS_STATUS_OP_RENMOV_SINGLE;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TWfxPluginCopyOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TWfxPluginCopyOperation.Initialize;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(SourceFiles.Path, FS_STATUS_START, FInfoOperation);
    FCallbackDataClass.UpdateProgressFunction:= @UpdateProgress;
    // Get initialized statistics; then we change only what is needed.
    FStatistics := RetrieveStatistics;

    FillAndCount(SourceFiles, False,
                 FFullFilesTreeToCopy,
                 FStatistics.TotalFiles,
                 FStatistics.TotalBytes);     // gets full list of files (recursive)
  end;

  // Make filenames relative to current directory.
  FFullFilesTreeToCopy.Path := SourceFiles.Path;

  if Assigned(FOperationHelper) then
    FreeAndNil(FOperationHelper);

  FOperationHelper := TWfxPluginOperationHelper.Create(
                        FWfxPluginFileSource,
                        @AskQuestion,
                        @RaiseAbortOperation,
                        @CheckOperationState,
                        @UpdateStatistics,
                        Thread,
                        wpohmCopy,
                        TargetPath,
                        FStatistics);

  FOperationHelper.RenameMask := RenameMask;
  FOperationHelper.FileExistsOption := FileExistsOption;

  FOperationHelper.Initialize;
end;

procedure TWfxPluginCopyOperation.MainExecute;
begin
  FOperationHelper.ProcessFiles(FFullFilesTreeToCopy);
end;

procedure TWfxPluginCopyOperation.Finalize;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(SourceFiles.Path, FS_STATUS_END, FInfoOperation);
    FCallbackDataClass.UpdateProgressFunction:= nil;
  end;
end;

end.

