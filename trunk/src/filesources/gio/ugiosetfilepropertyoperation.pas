unit uGioSetFilePropertyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSetFilePropertyOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFile, uGio2, uGLib2,
  uFileProperty,
  uWfxPluginFileSource;

type

  { TGioSetFilePropertyOperation }

  TGioSetFilePropertyOperation = class(TFileSourceSetFilePropertyOperation)

  private
    FFullFilesTree: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceSetFilePropertyOperationStatistics; // local copy of statistics

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
  private
    function SetFileTime(AFile: PGFile; ATime: Pgchar; AValue: TDateTime): Boolean;

  protected
    function SetNewProperty(aFile: TFile; aTemplateProperty: TFileProperty): TSetFilePropertyResult; override;

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theTargetFiles: TFiles;
                       var theNewProperties: TFileProperties); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;

  end;

implementation

uses
  DCBasicTypes, DCDateTimeUtils, uGioFileSourceUtil, uGObject2, uGio;

constructor TGioSetFilePropertyOperation.Create(aTargetFileSource: IFileSource;
                                                      var theTargetFiles: TFiles;
                                                      var theNewProperties: TFileProperties);
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

destructor TGioSetFilePropertyOperation.Destroy;
begin
  inherited Destroy;

  if Recursive then
  begin
    if Assigned(FFullFilesTree) then
      FreeAndNil(FFullFilesTree);
  end;
end;

procedure TGioSetFilePropertyOperation.Initialize;
var
  TotalBytes: Int64;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  if not Recursive then
    begin
      FFullFilesTree := TargetFiles;
      FStatistics.TotalFiles:= FFullFilesTree.Count;
    end
  else
    begin
      FillAndCount(TargetFiles, True,
                   FFullFilesTree,
                   FStatistics.TotalFiles,
                   TotalBytes);     // gets full list of files (recursive)
    end;
end;

procedure TGioSetFilePropertyOperation.MainExecute;
var
  aFile: TFile;
  aTemplateFile: TFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := 0 to FFullFilesTree.Count - 1 do
  begin
    aFile := FFullFilesTree[CurrentFileIndex];

    FStatistics.CurrentFile := aFile.FullPath;
    UpdateStatistics(FStatistics);

    if Assigned(TemplateFiles) and (CurrentFileIndex < TemplateFiles.Count) then
      aTemplateFile := TemplateFiles[CurrentFileIndex]
    else
      aTemplateFile := nil;

    SetProperties(CurrentFileIndex, aFile, aTemplateFile);

    with FStatistics do
    begin
      DoneFiles := DoneFiles + 1;
      UpdateStatistics(FStatistics);
    end;

    CheckOperationState;
  end;
end;

function TGioSetFilePropertyOperation.SetFileTime(AFile: PGFile; ATime: Pgchar;
  AValue: TDateTime): Boolean;
begin
  Result:= g_file_set_attribute_uint64 (AFile, ATime, DateTimeToUnixFileTime(AValue), G_FILE_QUERY_INFO_NONE, nil, nil);
end;

function TGioSetFilePropertyOperation.SetNewProperty(aFile: TFile;
                                                           aTemplateProperty: TFileProperty): TSetFilePropertyResult;
var
  AGFile: PGFile;
  AGNewFile: PGFile;
  NewAttributes: TFileAttrs;
begin
  Result := sfprSuccess;

  AGFile:= GioNewFile(aFile.FullPath);
  case aTemplateProperty.GetID of
    fpName:
      if (aTemplateProperty as TFileNameProperty).Value <> aFile.Name then
      begin
        AGNewFile:= g_file_set_display_name(AGFile, Pgchar((aTemplateProperty as TFileNameProperty).Value), nil, nil);
        if (AGNewFile = nil) then
          Result := sfprError
        else begin
          g_object_unref(PGObject(AGNewFile));
        end;
      end
      else
        Result := sfprSkipped;

    fpAttributes:
      if (aTemplateProperty as TFileAttributesProperty).Value <>
         (aFile.Properties[fpAttributes] as TFileAttributesProperty).Value then
      begin
        NewAttributes := (aTemplateProperty as TFileAttributesProperty).Value;

        if aTemplateProperty is TUnixFileAttributesProperty then
        begin
          if not g_file_set_attribute_uint32 (AGFile, FILE_ATTRIBUTE_UNIX_MODE, NewAttributes, G_FILE_QUERY_INFO_NONE, nil, nil) then
            Result := sfprError;
        end
        else
          raise Exception.Create('Unsupported file attributes type');
      end
      else
        Result := sfprSkipped;

    fpModificationTime:
      if (aTemplateProperty as TFileModificationDateTimeProperty).Value <>
         (aFile.Properties[fpModificationTime] as TFileModificationDateTimeProperty).Value then
      begin
        if not SetFileTime(AGFile, FILE_ATTRIBUTE_TIME_MODIFIED, (aTemplateProperty as TFileModificationDateTimeProperty).Value) then
          Result := sfprError;
      end
      else
        Result := sfprSkipped;

    fpCreationTime:
      if (aTemplateProperty as TFileCreationDateTimeProperty).Value <>
         (aFile.Properties[fpCreationTime] as TFileCreationDateTimeProperty).Value then
      begin
        if not SetFileTime(AGFile, FILE_ATTRIBUTE_TIME_CREATED, (aTemplateProperty as TFileCreationDateTimeProperty).Value) then
          Result := sfprError;
      end
      else
        Result := sfprSkipped;

    fpLastAccessTime:
      if (aTemplateProperty as TFileLastAccessDateTimeProperty).Value <>
         (aFile.Properties[fpLastAccessTime] as TFileLastAccessDateTimeProperty).Value then
      begin
        if not SetFileTime(AGFile, FILE_ATTRIBUTE_TIME_ACCESS, (aTemplateProperty as TFileLastAccessDateTimeProperty).Value) then
          Result := sfprError;
      end
      else
        Result := sfprSkipped;

    else
      raise Exception.Create('Trying to set unsupported property');
  end;
  g_object_unref(PGObject(AGFile));
end;

end.

