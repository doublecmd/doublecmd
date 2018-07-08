unit uGioMoveOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceMoveOperation,
  uFileSource,
  uFile,
  uGioFileSourceUtil;

type

  { TGioMoveOperation }

  TGioMoveOperation = class(TFileSourceMoveOperation)

  private
    FOperationHelper: TGioOperationHelper;
    FSourceFilesTree: TFileTree;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceMoveOperationStatistics; // local copy of statistics

  public
    constructor Create(aFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); virtual reintroduce;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

  end;

implementation

uses
  uFileSourceOperationOptions, uGio2;

constructor TGioMoveOperation.Create(aFileSource: IFileSource;
  var theSourceFiles: TFiles; aTargetPath: String);
begin
  inherited Create(aFileSource, theSourceFiles, aTargetPath);
end;

destructor TGioMoveOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSourceFilesTree);
end;

procedure TGioMoveOperation.Initialize;
var
  TreeBuilder: TGioTreeBuilder;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  TreeBuilder := TGioTreeBuilder.Create(@AskQuestion, @CheckOperationState);
  try
    TreeBuilder.SymLinkOption  := fsooslDontFollow;

    TreeBuilder.BuildFromFiles(SourceFiles);
    FSourceFilesTree := TreeBuilder.ReleaseTree;
    FStatistics.TotalFiles := TreeBuilder.FilesCount;
    FStatistics.TotalBytes := TreeBuilder.FilesSize;
  finally
    FreeAndNil(TreeBuilder);
  end;

  FOperationHelper := TGioOperationHelper.Create(
                        FileSource as IFileSource,
                        Self,
                        FStatistics,
                        @AskQuestion,
                        @RaiseAbortOperation,
                        @CheckOperationState,
                        @UpdateStatistics,
                        @ShowCompareFilesUI,
                        g_file_move,
                        TargetPath);

  FOperationHelper.RenameMask := RenameMask;
  FOperationHelper.FileExistsOption := FileExistsOption;
  FOperationHelper.DirExistsOption := DirExistsOption;

  FOperationHelper.Initialize;
end;

procedure TGioMoveOperation.MainExecute;
begin
  FOperationHelper.ProcessTree(FSourceFilesTree);
end;

procedure TGioMoveOperation.Finalize;
begin
  FileExistsOption := FOperationHelper.FileExistsOption;
  FOperationHelper.Free;
end;

end.

