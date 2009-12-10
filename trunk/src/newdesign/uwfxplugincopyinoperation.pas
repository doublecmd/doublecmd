unit uWfxPluginCopyInOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFile,
  uFileSystemFile,
  uWfxPluginFileSource,
  uWfxPluginUtil;

type

  { TWfxPluginCopyInOperation }

  TWfxPluginCopyInOperation = class(TFileSourceCopyInOperation)

  private
    FWfxPluginFileSource: IWfxPluginFileSource;
    FOperationHelper: TWfxPluginOperationHelper;
    FCallbackDataClass: TCallbackDataClass;
    FFullFilesTreeToCopy: TFileSystemFiles;  // source files including all files/dirs in subdirectories
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
  WfxPlugin, uFileSystemUtil;

// -- TWfxPluginCopyInOperation ---------------------------------------------

function TWfxPluginCopyInOperation.UpdateProgress(SourceName,TargetName: UTF8String;
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

constructor TWfxPluginCopyInOperation.Create(aSourceFileSource: IFileSource;
                                             aTargetFileSource: IFileSource;
                                             var theSourceFiles: TFiles;
                                             aTargetPath: String);
begin
  FWfxPluginFileSource:= aTargetFileSource as IWfxPluginFileSource;
  FCallbackDataClass:= TCallbackDataClass.Create;
  FInternal:= aSourceFileSource.IsInterface(IWfxPluginFileSource);
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TWfxPluginCopyInOperation.Destroy;
begin
  if Assigned(FCallbackDataClass) then
    FreeAndNil(FCallbackDataClass);
  inherited Destroy;
end;

procedure TWfxPluginCopyInOperation.Initialize;
begin
  FCallbackDataClass.FileSource:= FWfxPluginFileSource;
  FCallbackDataClass.UpdateProgressFunction:= @UpdateProgress;
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(SourceFiles.Path, FS_STATUS_START, FS_STATUS_OP_PUT_MULTI);
    WfxOperationList.Objects[PluginNumber]:= FCallbackDataClass;
  end;
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  FillAndCount(SourceFiles as TFileSystemFiles, False,
               FFullFilesTreeToCopy,
               FStatistics.TotalFiles,
               FStatistics.TotalBytes);     // gets full list of files (recursive)

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
                        wpohmCopyMoveIn,
                        TargetPath,
                        FStatistics);

  FOperationHelper.RenameMask := RenameMask;
  FOperationHelper.FileExistsOption := FileExistsOption;

  FOperationHelper.Initialize(FInternal);
end;

procedure TWfxPluginCopyInOperation.MainExecute;
begin
  FOperationHelper.ProcessFiles(FFullFilesTreeToCopy);
end;

procedure TWfxPluginCopyInOperation.Finalize;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(SourceFiles.Path, FS_STATUS_END, FS_STATUS_OP_PUT_MULTI);
    WfxOperationList.Objects[PluginNumber]:= nil;
  end;
end;

end.

