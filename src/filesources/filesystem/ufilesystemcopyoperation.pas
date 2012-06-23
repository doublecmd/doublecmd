unit uFileSystemCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperationTypes,
  uFileSourceOperationOptions,
  uFileSourceOperationOptionsUI,
  uFile,
  uFileSystemUtil,
  DCOSUtils,
  uSearchTemplate;

type

  { TFileSystemCopyOperation }

  TFileSystemCopyOperation = class(TFileSourceCopyOperation)

  private
    FOperationHelper: TFileSystemOperationHelper;
    FExcludeEmptyTemplateDirectories: Boolean;
    FSearchTemplate: TSearchTemplate;
    FSetPropertyError: TFileSourceOperationOptionSetPropertyError;
    FSourceFilesTree: TFileTree;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics

    // Options.
    FReserveSpace,
    FCheckFreeSpace: Boolean;
    FCopyAttributesOptions: TCopyAttributesOptions;
    FSkipAllBigFiles: Boolean;
    FAutoRenameItSelf: Boolean;
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FCorrectSymLinks: Boolean;
    FFileExistsOption: TFileSourceOperationOptionFileExists;
    FDirExistsOption: TFileSourceOperationOptionDirectoryExists;
    procedure SetSearchTemplate(AValue: TSearchTemplate);

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

    property CheckFreeSpace: Boolean read FCheckFreeSpace write FCheckFreeSpace;
    property ReserveSpace: Boolean read FReserveSpace write FReserveSpace;
    property CopyAttributesOptions: TCopyAttributesOptions read FCopyAttributesOptions write FCopyAttributesOptions;
    property SkipAllBigFiles: Boolean read FSkipAllBigFiles write FSkipAllBigFiles;
    property AutoRenameItSelf: Boolean read FAutoRenameItSelf write FAutoRenameItSelf;
    property SymLinkOption: TFileSourceOperationOptionSymLink read FSymLinkOption write FSymLinkOption;
    property CorrectSymLinks: Boolean read FCorrectSymLinks write FCorrectSymLinks;
    property FileExistsOption: TFileSourceOperationOptionFileExists read FFileExistsOption write FFileExistsOption;
    property DirExistsOption: TFileSourceOperationOptionDirectoryExists read FDirExistsOption write FDirExistsOption;
    property SetPropertyError: TFileSourceOperationOptionSetPropertyError read FSetPropertyError write FSetPropertyError;
    property ExcludeEmptyTemplateDirectories: Boolean read FExcludeEmptyTemplateDirectories write FExcludeEmptyTemplateDirectories;
    {en
       Operation takes ownership of assigned template and will free it.
    }
    property SearchTemplate: TSearchTemplate read FSearchTemplate write SetSearchTemplate;
  end;

  {
    Both operations are the same, just source and target reversed.
    Implement them in terms of the same functions,
    or have one use the other.
  }

  { TFileSystemCopyInOperation }

  TFileSystemCopyInOperation = class(TFileSystemCopyOperation)

  protected
    function GetID: TFileSourceOperationType; override;

  end;

  { TFileSystemCopyOutOperation }

  TFileSystemCopyOutOperation = class(TFileSystemCopyOperation)

  protected
    function GetID: TFileSourceOperationType; override;

  end;

implementation

uses
  fFileSystemCopyMoveOperationOptions, uGlobs;

// -- TFileSystemCopyOperation ---------------------------------------------

constructor TFileSystemCopyOperation.Create(aSourceFileSource: IFileSource;
                                            aTargetFileSource: IFileSource;
                                            var theSourceFiles: TFiles;
                                            aTargetPath: String);
begin
  // Here we can read global settings if there are any.
  FCopyAttributesOptions := [];
  if gOperationOptionCopyAttributes then
    FCopyAttributesOptions := FCopyAttributesOptions + [caoCopyAttributes];
  if gOperationOptionCopyTime then
    FCopyAttributesOptions := FCopyAttributesOptions + [caoCopyTime];
  if gOperationOptionCopyOwnership then
    FCopyAttributesOptions := FCopyAttributesOptions + [caoCopyOwnership];
  if gDropReadOnlyFlag then
    FCopyAttributesOptions := FCopyAttributesOptions + [caoRemoveReadOnlyAttr];
  FSymLinkOption := gOperationOptionSymLinks;
  FFileExistsOption := gOperationOptionFileExists;
  FDirExistsOption := gOperationOptionDirectoryExists;
  FSetPropertyError := gOperationOptionSetPropertyError;
  FReserveSpace := gOperationOptionReserveSpace;
  FCheckFreeSpace := gOperationOptionCheckFreeSpace;
  FSkipAllBigFiles := False;
  FAutoRenameItSelf := False;
  FCorrectSymLinks := gOperationOptionCorrectLinks;
  FExcludeEmptyTemplateDirectories := True;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);
end;

destructor TFileSystemCopyOperation.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSourceFilesTree);
  FreeAndNil(FOperationHelper);
  FreeAndNil(FSearchTemplate);
end;

procedure TFileSystemCopyOperation.Initialize;
var
  TreeBuilder: TFileSystemTreeBuilder;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  TreeBuilder := TFileSystemTreeBuilder.Create(
                        @AskQuestion,
                        @CheckOperationState);
  try
    TreeBuilder.SymLinkOption  := Self.SymLinkOption;
    TreeBuilder.SearchTemplate := Self.SearchTemplate;
    TreeBuilder.ExcludeEmptyTemplateDirectories := Self.ExcludeEmptyTemplateDirectories;

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
                        fsohmCopy,
                        TargetPath,
                        FStatistics);

  FOperationHelper.RenameMask := RenameMask;
  FOperationHelper.ReserveSpace :=  FReserveSpace;
  FOperationHelper.CheckFreeSpace := CheckFreeSpace;
  FOperationHelper.CopyAttributesOptions := CopyAttributesOptions;
  FOperationHelper.SkipAllBigFiles := SkipAllBigFiles;
  FOperationHelper.AutoRenameItSelf := AutoRenameItSelf;
  FOperationHelper.CorrectSymLinks := CorrectSymLinks;
  FOperationHelper.FileExistsOption := FileExistsOption;
  FOperationHelper.DirExistsOption := DirExistsOption;
  FOperationHelper.SetPropertyError := SetPropertyError;

  FOperationHelper.Initialize;
end;

procedure TFileSystemCopyOperation.MainExecute;
begin
  FOperationHelper.ProcessTree(FSourceFilesTree);
end;

procedure TFileSystemCopyOperation.SetSearchTemplate(AValue: TSearchTemplate);
begin
  FSearchTemplate.Free;
  FSearchTemplate := AValue;
end;

procedure TFileSystemCopyOperation.Finalize;
begin
  FreeAndNil(FOperationHelper);
end;

class function TFileSystemCopyOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result := TFileSystemCopyOperationOptionsUI;
end;

{ TFileSystemCopyInOperation }

function TFileSystemCopyInOperation.GetID: TFileSourceOperationType;
begin
  Result:= fsoCopyIn;
end;

{ TFileSystemCopyOutOperation }

function TFileSystemCopyOutOperation.GetID: TFileSourceOperationType;
begin
  Result:= fsoCopyOut;
end;

end.

