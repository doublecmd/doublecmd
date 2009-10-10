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
  uFileSystemFile,
  uWfxPluginFile,
  uWfxPluginFileSource,
  uWfxPluginUtil;

type

  TWfxPluginCopyOutOperation = class(TFileSourceCopyOutOperation)

  private
    FWfxPluginFileSource: TWfxPluginFileSource;
    FOperationHelper: TWfxPluginOperationHelper;
    FFullFilesTreeToCopy: TFileSystemFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    FCurrentFileSize: Int64;
    // Options
    FInternal: Boolean;
    FFileExistsOption: TFileSourceOperationOptionFileExists;
  protected

    // ProcessFileNoQuestions (when we're sure the targets don't exist)

  public
    constructor Create(var aSourceFileSource: TFileSource;
                       var aTargetFileSource: TFileSource;
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
  uOSUtils, FileUtil, LCLProc, uGlobs, ufsplugin, uFileSystemUtil;

// -- TWfxPluginCopyOutOperation ---------------------------------------------

constructor TWfxPluginCopyOutOperation.Create(var aSourceFileSource: TFileSource;
                                               var aTargetFileSource: TFileSource;
                                               var theSourceFiles: TFiles;
                                               aTargetPath: String);
begin
  FWfxPluginFileSource:= aSourceFileSource as TWfxPluginFileSource;
  FInternal:= aTargetFileSource is TWfxPluginFileSource;
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TWfxPluginCopyOutOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TWfxPluginCopyOutOperation.Initialize;
begin
  with FWfxPluginFileSource do
  begin
    WfxStatusInfo(CurrentPath, FS_STATUS_START, FS_STATUS_OP_PUT_MULTI);
    WfxOperationList[PluginNumber]:= Self;
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
    WfxStatusInfo(CurrentPath, FS_STATUS_END, FS_STATUS_OP_PUT_MULTI);
    WfxOperationList[PluginNumber]:= nil;
  end;
end;

end.

