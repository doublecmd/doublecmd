unit uFileSystemCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFile,
  uFileSystemFile,
  uFileSystemUtil;

type
  {
    Both operations are the same, just source and target reversed.
    Implement them in terms of the same functions,
    or have one use the other.
  }

  TFileSystemCopyInOperation = class(TFileSourceCopyInOperation)

  private

  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    procedure MainExecute; override;

  end;

  TFileSystemCopyOutOperation = class(TFileSourceCopyOutOperation)

  private
    FOperationHelper: TFileSystemOperationHelper;
    FSourceFilesTree: TFileTree;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics

    // Options.
    FCheckFreeSpace: Boolean;
    FSkipAllBigFiles: Boolean;
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FCorrectSymLinks: Boolean;
    FFileExistsOption: TFileSourceOperationOptionFileExists;
    FDirExistsOption: TFileSourceOperationOptionDirectoryExists;

  protected

    // ProcessFileNoQuestions (when we're sure the targets don't exist)

  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

    property CheckFreeSpace: Boolean read FCheckFreeSpace write FCheckFreeSpace;
    property SkipAllBigFiles: Boolean read FSkipAllBigFiles write FSkipAllBigFiles;
    property SymLinkOption: TFileSourceOperationOptionSymLink read FSymLinkOption write FSymLinkOption;
    property CorrectSymLinks: Boolean read FCorrectSymLinks write FCorrectSymLinks;
    property FileExistsOption: TFileSourceOperationOptionFileExists read FFileExistsOption write FFileExistsOption;
    property DirExistsOption: TFileSourceOperationOptionDirectoryExists read FDirExistsOption write FDirExistsOption;
  end;

implementation

// -- TFileSystemCopyInOperation ----------------------------------------------

constructor TFileSystemCopyInOperation.Create(aSourceFileSource: IFileSource;
                                              aTargetFileSource: IFileSource;
                                              var theSourceFiles: TFiles;
                                              aTargetPath: String);
begin
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

procedure TFileSystemCopyInOperation.MainExecute;
begin
end;

// -- TFileSystemCopyOutOperation ---------------------------------------------

constructor TFileSystemCopyOutOperation.Create(aSourceFileSource: IFileSource;
                                               aTargetFileSource: IFileSource;
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

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TFileSystemCopyOutOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FSourceFilesTree) then
    FreeAndNil(FSourceFilesTree);

  if Assigned(FOperationHelper) then
    FreeAndNil(FOperationHelper);
end;

procedure TFileSystemCopyOutOperation.Initialize;
var
  TreeBuilder: TFileSystemTreeBuilder;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  TreeBuilder := TFileSystemTreeBuilder.Create(
                        @AskQuestion,
                        @CheckOperationState);
  try
    TreeBuilder.SymLinkOption := Self.SymLinkOption;

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
                        fsohmCopy,
                        TargetPath,
                        FStatistics);

  FOperationHelper.RenameMask := RenameMask;
//  FOperation.OnlyFilesMask := OnlyFilesMask;
  FOperationHelper.DropReadOnlyAttribute := DropReadOnlyAttribute;
  FOperationHelper.CheckFreeSpace := CheckFreeSpace;
  FOperationHelper.SkipAllBigFiles := SkipAllBigFiles;
  FOperationHelper.CorrectSymLinks := CorrectSymLinks;
  FOperationHelper.FileExistsOption := FileExistsOption;
  FOperationHelper.DirExistsOption := DirExistsOption;

  FOperationHelper.Initialize;
end;

procedure TFileSystemCopyOutOperation.MainExecute;
begin
  FOperationHelper.ProcessTree(FSourceFilesTree);
end;

procedure TFileSystemCopyOutOperation.Finalize;
begin
  if Assigned(FOperationHelper) then
    FreeAndNil(FOperationHelper);
end;

end.

