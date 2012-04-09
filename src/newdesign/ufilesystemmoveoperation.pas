unit uFileSystemMoveOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceMoveOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI,
  uFile,
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
    FFileExistsOption: TFileSourceOperationOptionFileExists;
    FDirExistsOption: TFileSourceOperationOptionDirectoryExists;
    FCorrectSymlinks: Boolean;

  protected

  public
    constructor Create(aFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

    class function GetOptionsUIClass: TFileSourceOperationOptionsUIClass; override;

    property CheckFreeSpace: Boolean read FCheckFreeSpace write FCheckFreeSpace;
    property SkipAllBigFiles: Boolean read FSkipAllBigFiles write FSkipAllBigFiles;
    property CorrectSymLinks: Boolean read FCorrectSymLinks write FCorrectSymLinks;
    property FileExistsOption: TFileSourceOperationOptionFileExists read FFileExistsOption write FFileExistsOption;
    property DirExistsOption: TFileSourceOperationOptionDirectoryExists read FDirExistsOption write FDirExistsOption;
  end;

implementation

uses
  fFileSystemCopyMoveOperationOptions, uGlobs;

constructor TFileSystemMoveOperation.Create(aFileSource: IFileSource;
                                            var theSourceFiles: TFiles;
                                            aTargetPath: String);
begin
  // Here we can read global settings if there are any.
  FFileExistsOption := gOperationOptionFileExists;
  FDirExistsOption := gOperationOptionDirectoryExists;
  FCheckFreeSpace := gOperationOptionCheckFreeSpace;
  FSkipAllBigFiles := False;
  FCorrectSymlinks := gOperationOptionCorrectLinks;

  inherited Create(aFileSource, theSourceFiles, aTargetPath);
end;

destructor TFileSystemMoveOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSourceFilesTree);
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
    TreeBuilder.BuildFromFiles(SourceFiles);
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
//  FOperation.OnlyFilesMask := OnlyFilesMask;
  FOperationHelper.CheckFreeSpace := CheckFreeSpace;
  FOperationHelper.SkipAllBigFiles := SkipAllBigFiles;
  FOperationHelper.CorrectSymLinks := CorrectSymLinks;
  FOperationHelper.FileExistsOption := FileExistsOption;
  FOperationHelper.DirExistsOption := DirExistsOption;
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

class function TFileSystemMoveOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result := TFileSystemMoveOperationOptionsUI;
end;

end.

