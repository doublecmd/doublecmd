unit uWfxPluginCopyOutOperation;

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

  { TWfxPluginCopyOutOperation }

  TWfxPluginCopyOutOperation = class(TFileSourceCopyOutOperation)

  private
    FWfxPluginFileSource: IWfxPluginFileSource;
    FOperationHelper: TWfxPluginOperationHelper;
    FUpdateProgressClass: TUpdateProgressClass;
    FFullFilesTreeToCopy: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    // Options
    FInternal: Boolean;
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

// -- TWfxPluginCopyOutOperation ---------------------------------------------

function TWfxPluginCopyOutOperation.UpdateProgress(SourceName, TargetName: UTF8String;
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

constructor TWfxPluginCopyOutOperation.Create(aSourceFileSource: IFileSource;
                                              aTargetFileSource: IFileSource;
                                              var theSourceFiles: TFiles;
                                              aTargetPath: String);
begin
  FWfxPluginFileSource:= aSourceFileSource as IWfxPluginFileSource;
  FUpdateProgressClass:= TUpdateProgressClass.Create;
  FInternal:= aTargetFileSource.IsInterface(IWfxPluginFileSource);
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TWfxPluginCopyOutOperation.Destroy;
begin
  if Assigned(FUpdateProgressClass) then
    FreeAndNil(FUpdateProgressClass);
  inherited Destroy;
end;

procedure TWfxPluginCopyOutOperation.Initialize;
begin
  FUpdateProgressClass.UpdateProgressFunction:= @UpdateProgress;
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo({CurrentPath}SourceFiles.Path, FS_STATUS_START, FS_STATUS_OP_PUT_MULTI);
    WfxOperationList.Objects[PluginNumber]:= FUpdateProgressClass;
    // Get initialized statistics; then we change only what is needed.
    FStatistics := RetrieveStatistics;

    FillAndCount(SourceFiles,
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
                        wpohmCopyMoveOut,
                        TargetPath,
                        FStatistics);

  FOperationHelper.RenameMask := RenameMask;
  FOperationHelper.FileExistsOption := FileExistsOption;

  FOperationHelper.Initialize(FInternal);
end;

procedure TWfxPluginCopyOutOperation.MainExecute;
begin
  FOperationHelper.ProcessFiles(FFullFilesTreeToCopy);
end;

procedure TWfxPluginCopyOutOperation.Finalize;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo({CurrentPath}SourceFiles.Path, FS_STATUS_END, FS_STATUS_OP_PUT_MULTI);
    WfxOperationList.Objects[PluginNumber]:= nil;
  end;
end;

end.

