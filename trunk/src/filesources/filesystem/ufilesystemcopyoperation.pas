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
    FVerify,
    FReserveSpace,
    FCheckFreeSpace: Boolean;
    FSkipAllBigFiles: Boolean;
    FAutoRenameItSelf: Boolean;
    FCorrectSymLinks: Boolean;
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

    property Verify: Boolean read FVerify write FVerify;
    property CheckFreeSpace: Boolean read FCheckFreeSpace write FCheckFreeSpace;
    property ReserveSpace: Boolean read FReserveSpace write FReserveSpace;
    property SkipAllBigFiles: Boolean read FSkipAllBigFiles write FSkipAllBigFiles;
    property AutoRenameItSelf: Boolean read FAutoRenameItSelf write FAutoRenameItSelf;
    property CorrectSymLinks: Boolean read FCorrectSymLinks write FCorrectSymLinks;
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
  // Here we can read global settings if there are any
  FSymLinkOption := gOperationOptionSymLinks;
  FSetPropertyError := gOperationOptionSetPropertyError;
  FReserveSpace := gOperationOptionReserveSpace;
  FCheckFreeSpace := gOperationOptionCheckFreeSpace;
  FSkipAllBigFiles := False;
  FAutoRenameItSelf := False;
  FCorrectSymLinks := gOperationOptionCorrectLinks;
  FExcludeEmptyTemplateDirectories := True;

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);

  // Here we can read global settings if there are any
  FFileExistsOption := gOperationOptionFileExists;
  FDirExistsOption := gOperationOptionDirectoryExists;

  if gOperationOptionCopyAttributes then
    FCopyAttributesOptions := FCopyAttributesOptions + [caoCopyAttributes];
  if gOperationOptionCopyXattributes then
    FCopyAttributesOptions := FCopyAttributesOptions + [caoCopyXattributes];
  if gOperationOptionCopyTime then
    FCopyAttributesOptions := FCopyAttributesOptions + [caoCopyTime];
  if gOperationOptionCopyOwnership then
    FCopyAttributesOptions := FCopyAttributesOptions + [caoCopyOwnership];
  if gOperationOptionCopyPermissions then
    FCopyAttributesOptions := FCopyAttributesOptions + [caoCopyPermissions];
  if gDropReadOnlyFlag then
    FCopyAttributesOptions := FCopyAttributesOptions + [caoRemoveReadOnlyAttr];
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
    if FVerify then FStatistics.TotalBytes := FStatistics.TotalBytes * 2;
  finally
    FreeAndNil(TreeBuilder);
  end;

  if Assigned(FOperationHelper) then
    FreeAndNil(FOperationHelper);

  FOperationHelper := TFileSystemOperationHelper.Create(
                        @AskQuestion,
                        @RaiseAbortOperation,
                        @AppProcessMessages,
                        @CheckOperationState,
                        @UpdateStatistics,
                        @ShowCompareFilesUI,
                        Thread,
                        fsohmCopy,
                        TargetPath,
                        FStatistics);

  FOperationHelper.Verify := FVerify;
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
  FileExistsOption := FOperationHelper.FileExistsOption;
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

