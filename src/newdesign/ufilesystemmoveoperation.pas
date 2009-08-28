unit uFileSystemMoveOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceMoveOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFile,
  uFileSystemFile,
  uFileSystemUtil;

type
  TFileSystemMoveOperation = class(TFileSourceMoveOperation)

  private
    FOperationHelper: TFileSystemOperationHelper;
    FSourceFilesTree: TFileTree;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceMoveOperationStatistics; // local copy of statistics

    // Options.
    FCheckFreeSpace: Boolean;
    FSkipAllBigFiles: Boolean;
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FFileExistsOption: TFileSourceOperationOptionFileExists;
    FDirExistsOption: TFileSourceOperationOptionDirectoryExists;
    FCorrectSymlinks: Boolean;

  protected

  public
    constructor Create(var aFileSource: TFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  uOSUtils, FileUtil, LCLProc, uGlobs;

constructor TFileSystemMoveOperation.Create(var aFileSource: TFileSource;
                                            var theSourceFiles: TFiles;
                                            aTargetPath: String);
begin
  FSourceFilesTree := nil;
  FOperationHelper := nil;

  // Here we can read global settings if there are any.
  FSymLinkOption := fsooslNone;
  FFileExistsOption := fsoofeNone;
  FDirExistsOption := fsoodeNone;
  FCheckFreeSpace := True;
  FSkipAllBigFiles := False;
  FCorrectSymlinks := False;

  inherited Create(aFileSource, theSourceFiles, aTargetPath);
end;

destructor TFileSystemMoveOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FSourceFilesTree) then
    FreeAndNil(FSourceFilesTree);

  if Assigned(FOperationHelper) then
    FreeAndNil(FOperationHelper);
end;

procedure TFileSystemMoveOperation.Initialize;
var
  TreeBuilder: TFileSystemTreeBuilder;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  TreeBuilder := TFileSystemTreeBuilder.Create(
                        @AskQuestion,
                        @CheckOperationState);
  try
    // In move operation don't follow symlinks.
    TreeBuilder.SymLinkOption := fsooslDontFollow;
    TreeBuilder.BuildFromFiles(SourceFiles as TFileSystemFiles);
    FSourceFilesTree := TreeBuilder.ReleaseTree;
    FStatistics.TotalFiles := TreeBuilder.FilesCount;
    FStatistics.TotalBytes := TreeBuilder.FilesSize;
  finally
    FreeAndNil(TreeBuilder);
  end;

  if Assigned(FOperationHelper) then
    FreeAndNil(FOperationHelper);

  FOperationHelper := TFileSystemOperationHelper.Create(
                        @AskQuestion,
                        @RaiseAbortOperation,
                        @CheckOperationState,
                        @UpdateStatistics,
                        Thread,
                        fsohmMove,
                        TargetPath,
                        FStatistics);

  FOperationHelper.RenameMask := RenameMask;
  FOperationHelper.Initialize;
end;

procedure TFileSystemMoveOperation.MainExecute;
begin
  FOperationHelper.ProcessTree(FSourceFilesTree);
end;

procedure TFileSystemMoveOperation.Finalize;
begin
  if Assigned(FOperationHelper) then
    FreeAndNil(FOperationHelper);
end;

end.

