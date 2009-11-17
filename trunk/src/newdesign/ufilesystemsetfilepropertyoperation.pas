unit uFileSystemSetFilePropertyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSetFilePropertyOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFile,
  uFileProperty,
  uFileSystemFile,
  uGlobs, uOSUtils;

type

  TFileSystemSetFilePropertyOperation = class(TFileSourceSetFilePropertyOperation)

  private
    FFullFilesTree: TFileSystemFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceSetFilePropertyOperationStatistics; // local copy of statistics

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;

  protected
    function SetNewProperty(aFile: TFile; aTemplateProperty: TFileProperty): Boolean; override;

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theTargetFiles: TFiles;
                       theNewProperties: TFileProperties); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

  end;

implementation

uses
  uFileSystemUtil;

constructor TFileSystemSetFilePropertyOperation.Create(aTargetFileSource: IFileSource;
                                                       var theTargetFiles: TFiles;
                                                       theNewProperties: TFileProperties);
begin
  FSymLinkOption := fsooslNone;
  FFullFilesTree := nil;

  inherited Create(aTargetFileSource, theTargetFiles, theNewProperties);

  // Assign after calling inherited constructor.
  FSupportedProperties := [fpName,
                           fpAttributes,
                           fpModificationTime,
                           fpCreationTime,
                           fpLastAccessTime];
end;

destructor TFileSystemSetFilePropertyOperation.Destroy;
begin
  inherited Destroy;

  if Recursive then
  begin
    if Assigned(FFullFilesTree) then
      FreeAndNil(FFullFilesTree);
  end;
end;

procedure TFileSystemSetFilePropertyOperation.Initialize;
var
  TotalBytes: Int64;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  if not Recursive then
    begin
      FFullFilesTree := TargetFiles as TFileSystemFiles;
      FStatistics.TotalFiles := FFullFilesTree.Count;
    end
  else
    begin
      FillAndCount(TargetFiles as TFileSystemFiles,
                   FFullFilesTree,
                   FStatistics.TotalFiles,
                   TotalBytes);     // gets full list of files (recursive)
    end;
end;

procedure TFileSystemSetFilePropertyOperation.MainExecute;
var
  aFile: TFileSystemFile;
  aTemplateFile: TFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := 0 to FFullFilesTree.Count - 1 do
  begin
    aFile := FFullFilesTree[CurrentFileIndex] as TFileSystemFile;

    FStatistics.CurrentFile := aFile.FullPath;
    UpdateStatistics(FStatistics);

    if Assigned(TemplateFiles) and (CurrentFileIndex < TemplateFiles.Count) then
      aTemplateFile := TemplateFiles[CurrentFileIndex]
    else
      aTemplateFile := nil;

    SetProperties(aFile, aTemplateFile);

    with FStatistics do
    begin
      DoneFiles := DoneFiles + 1;
      UpdateStatistics(FStatistics);
    end;

    CheckOperationState;
  end;
end;

procedure TFileSystemSetFilePropertyOperation.Finalize;
begin
end;

function TFileSystemSetFilePropertyOperation.SetNewProperty(aFile: TFile;
                                                            aTemplateProperty: TFileProperty): Boolean;
begin
  Result := True;

  case aTemplateProperty.GetID of
    fpName:
      if (aTemplateProperty as TFileNameProperty).Value <> aFile.Name then
      begin
        Result := mbRenameFile(
          aFile.FullPath,
          (aTemplateProperty as TFileNameProperty).Value);
      end;

    fpAttributes:
      if (aTemplateProperty as TFileAttributesProperty).Value <>
         (aFile.Properties[fpAttributes] as TFileAttributesProperty).Value then
      begin
        Result := mbFileSetAttr(
          aFile.FullPath,
          (aTemplateProperty as TFileAttributesProperty).Value) = 0;
      end;

    fpModificationTime:
      if (aTemplateProperty as TFileModificationDateTimeProperty).Value <>
         (aFile.Properties[fpModificationTime] as TFileModificationDateTimeProperty).Value then
      begin
        Result := mbFileSetTime(
          aFile.FullPath,
          DateTimeToFileDate((aTemplateProperty as TFileModificationDateTimeProperty).Value),
          0,
          0) <> 0;
      end;

    fpCreationTime:
      if (aTemplateProperty as TFileCreationDateTimeProperty).Value <>
         (aFile.Properties[fpCreationTime] as TFileCreationDateTimeProperty).Value then
      begin
        Result := mbFileSetTime(
          aFile.FullPath,
          0,
          DateTimeToFileDate((aTemplateProperty as TFileCreationDateTimeProperty).Value),
          0) <> 0;
      end;

    fpLastAccessTime:
      if (aTemplateProperty as TFileLastAccessDateTimeProperty).Value <>
         (aFile.Properties[fpLastAccessTime] as TFileLastAccessDateTimeProperty).Value then
      begin
        Result := mbFileSetTime(
          aFile.FullPath,
          0,
          0,
          DateTimeToFileDate((aTemplateProperty as TFileLastAccessDateTimeProperty).Value)) <> 0;
      end;

    else
      raise Exception.Create('Trying to set unsupported property');
  end;
end;

end.

