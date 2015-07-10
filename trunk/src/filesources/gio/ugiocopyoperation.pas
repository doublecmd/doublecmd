unit uGioCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperationTypes,
  uFile,
  uGioFileSourceUtil;

type

  { TGioCopyOperation }

  TGioCopyOperation = class(TFileSourceCopyOperation)

  private
    FOperationHelper: TGioOperationHelper;
    FSourceFilesTree: TFileTree;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics

  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

  end;

  {
    Both operations are the same, just source and target reversed.
    Implement them in terms of the same functions,
    or have one use the other.
  }

  { TGioCopyInOperation }

  TGioCopyInOperation = class(TGioCopyOperation)

  protected
    function GetID: TFileSourceOperationType; override;

  end;

  { TGioCopyOutOperation }

  TGioCopyOutOperation = class(TGioCopyOperation)

  protected
    function GetID: TFileSourceOperationType; override;

  end;

implementation

uses
  uGio2, uGlobs;

constructor TGioCopyOperation.Create(aSourceFileSource: IFileSource;
                                            aTargetFileSource: IFileSource;
                                            var theSourceFiles: TFiles;
                                            aTargetPath: String);
begin
  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TGioCopyOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TGioCopyOperation.Initialize;
var
  TreeBuilder: TGioTreeBuilder;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  TreeBuilder := TGioTreeBuilder.Create(@AskQuestion, @CheckOperationState);
  try
    // TreeBuilder.SymLinkOption  := Self.SymLinkOption;
    // TreeBuilder.SearchTemplate := Self.SearchTemplate;
    // TreeBuilder.ExcludeEmptyTemplateDirectories := Self.ExcludeEmptyTemplateDirectories;

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
                        g_file_copy,
                        TargetPath);

  FOperationHelper.RenameMask := RenameMask;
  FOperationHelper.FileExistsOption := FileExistsOption;

  FOperationHelper.Initialize;
end;

procedure TGioCopyOperation.MainExecute;
begin
  FOperationHelper.ProcessTree(FSourceFilesTree);
end;

procedure TGioCopyOperation.Finalize;
begin
  FOperationHelper.Free;
end;

{ TGioCopyInOperation }

function TGioCopyInOperation.GetID: TFileSourceOperationType;
begin
  Result:= fsoCopyIn;
end;

{ TGioCopyOutOperation }

function TGioCopyOutOperation.GetID: TFileSourceOperationType;
begin
  Result:= fsoCopyOut;
end;

end.

