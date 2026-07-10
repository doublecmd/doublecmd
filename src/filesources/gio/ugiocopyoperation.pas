unit uGioCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperationTypes,
  uFileSourceOperationOptionsUI,
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

    class function GetOptionsUIClass: TFileSourceOperationOptionsUIClass; override;
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
  public
    procedure Initialize; override;

  end;

  { TGioCopyOutOperation }

  TGioCopyOutOperation = class(TGioCopyOperation)

  protected
    function GetID: TFileSourceOperationType; override;

  end;

implementation

uses
  fGioCopyMoveOperationOptions, uGio2, uTempFileSystemFileSource, uFileProperty,
  uGLib2, uGObject2;

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
  FreeAndNil(FSourceFilesTree);
end;

procedure TGioCopyOperation.Initialize;
var
  TreeBuilder: TGioTreeBuilder;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  TreeBuilder := TGioTreeBuilder.Create(@AskQuestion, @CheckOperationState);
  try
    TreeBuilder.SymLinkOption  := Self.SymLinkOption;

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
                        g_file_copy,
                        TargetPath);

  FOperationHelper.RenameMask := RenameMask;
  FOperationHelper.FileExistsOption := FileExistsOption;
  FOperationHelper.DirExistsOption := DirExistsOption;

  FOperationHelper.Initialize;
end;

procedure TGioCopyOperation.MainExecute;
begin
  FOperationHelper.ProcessTree(FSourceFilesTree);
end;

procedure TGioCopyOperation.Finalize;
begin
  FileExistsOption := FOperationHelper.FileExistsOption;
  FOperationHelper.Free;
end;

class function TGioCopyOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result := TGioCopyOperationOptionsUI;
end;

{ TGioCopyInOperation }

function TGioCopyInOperation.GetID: TFileSourceOperationType;
begin
  Result:= fsoCopyIn;
end;

procedure TGioCopyInOperation.Initialize;
var
  aFile: TFile;
  Index: Integer;
  aLinkProperty: TFileLinkProperty;
begin
  // Special case: replace invalid TGioLinkProperty.Item
  if SourceFileSource.IsClass(TTempFileSystemFileSource) then
  begin
    for Index := 0 to SourceFiles.Count - 1 do
    begin
      aFile:= SourceFiles[Index];
      aLinkProperty:= aFile.LinkProperty;
      if Assigned(aLinkProperty) and (aLinkProperty is TGioLinkProperty) then
      begin
        g_object_unref(PGObject(TGioLinkProperty(aLinkProperty).Item));
        TGioLinkProperty(aLinkProperty).Item:= g_file_new_for_path(Pgchar(aFile.FullPath));
      end;
    end;
  end;
  inherited Initialize;
end;

{ TGioCopyOutOperation }

function TGioCopyOutOperation.GetID: TFileSourceOperationType;
begin
  Result:= fsoCopyOut;
end;

end.

