unit uWfxPluginSetFilePropertyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSetFilePropertyOperation,
  uFileSource,
  uFileSourceOperationOptions,
  uFile,
  uFileProperty,
  uWfxPluginFileSource;

type

  TWfxPluginSetFilePropertyOperation = class(TFileSourceSetFilePropertyOperation)

  private
    FWfxPluginFileSource: IWfxPluginFileSource;
    FFullFilesTree: TFiles;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceSetFilePropertyOperationStatistics; // local copy of statistics

    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;

  protected
    function SetNewProperty(aFile: TFile; aTemplateProperty: TFileProperty): TSetFilePropertyResult; override;

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theTargetFiles: TFiles;
                       var theNewProperties: TFileProperties); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

  end;

implementation

uses
  uTypes, uDCUtils, WfxPlugin, uWfxPluginFile, uWfxPluginUtil, uDateTimeUtils;

constructor TWfxPluginSetFilePropertyOperation.Create(aTargetFileSource: IFileSource;
                                                      var theTargetFiles: TFiles;
                                                      var theNewProperties: TFileProperties);
begin
  FSymLinkOption := fsooslNone;
  FFullFilesTree := nil;
  FWfxPluginFileSource:= aTargetFileSource as IWfxPluginFileSource;

  inherited Create(aTargetFileSource, theTargetFiles, theNewProperties);

  // Assign after calling inherited constructor.
  FSupportedProperties := [fpName,
                           fpAttributes,
                           fpModificationTime,
                           fpCreationTime,
                           fpLastAccessTime];
end;

destructor TWfxPluginSetFilePropertyOperation.Destroy;
begin
  inherited Destroy;

  if Recursive then
  begin
    if Assigned(FFullFilesTree) then
      FreeAndNil(FFullFilesTree);
  end;
end;

procedure TWfxPluginSetFilePropertyOperation.Initialize;
var
  TotalBytes: Int64;
begin
  with FWfxPluginFileSource do
    WfxModule.WfxStatusInfo(TargetFiles.Path, FS_STATUS_START, FS_STATUS_OP_ATTRIB);

  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  if not Recursive then
    begin
      FFullFilesTree := TargetFiles;
      FStatistics.TotalFiles:= FFullFilesTree.Count;
    end
  else
    begin
      FWfxPluginFileSource.FillAndCount(TargetFiles, True,
                                        FFullFilesTree,
                                        FStatistics.TotalFiles,
                                        TotalBytes);     // gets full list of files (recursive)
    end;
end;

procedure TWfxPluginSetFilePropertyOperation.MainExecute;
var
  aFile: TWfxPluginFile;
  aTemplateFile: TFile;
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := 0 to FFullFilesTree.Count - 1 do
  begin
    aFile := FFullFilesTree[CurrentFileIndex] as TWfxPluginFile;

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

procedure TWfxPluginSetFilePropertyOperation.Finalize;
begin
  with FWfxPluginFileSource do
    WfxModule.WfxStatusInfo(TargetFiles.Path, FS_STATUS_END, FS_STATUS_OP_ATTRIB);
end;

function TWfxPluginSetFilePropertyOperation.SetNewProperty(aFile: TFile;
                                                           aTemplateProperty: TFileProperty): TSetFilePropertyResult;
var
  FileName: UTF8String;
  NewAttributes: TFileAttrs;
  ftTime: TFileTime;
begin
  Result := sfprSuccess;

  case aTemplateProperty.GetID of
    fpName:
      if (aTemplateProperty as TFileNameProperty).Value <> aFile.Name then
      begin
        if not WfxRenameFile(FWfxPluginFileSource, aFile, (aTemplateProperty as TFileNameProperty).Value) then
          Result := sfprError;
      end
      else
        Result := sfprSkipped;

    fpAttributes:
      if (aTemplateProperty as TFileAttributesProperty).Value <>
         (aFile.Properties[fpAttributes] as TFileAttributesProperty).Value then
      begin
        NewAttributes := (aTemplateProperty as TFileAttributesProperty).Value;
        FileName := aFile.FullPath;

        with FWfxPluginFileSource.WfxModule do
          if aTemplateProperty is TNtfsFileAttributesProperty then
          begin
            if not WfxSetAttr(FileName, NewAttributes) then
              Result := sfprError;
          end
          else if aTemplateProperty is TUnixFileAttributesProperty then
          begin
            if WfxExecuteFile(0, FileName, 'chmod' + #32 + DecToOct(NewAttributes)) <> FS_EXEC_OK then
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
        ftTime := DateTimeToWfxFileTime((aTemplateProperty as TFileModificationDateTimeProperty).Value);
        with FWfxPluginFileSource.WfxModule do
          if not WfxSetTime(aFile.FullPath, nil, nil, @ftTime) then
            Result := sfprError;
      end
      else
        Result := sfprSkipped;

    fpCreationTime:
      if (aTemplateProperty as TFileCreationDateTimeProperty).Value <>
         (aFile.Properties[fpCreationTime] as TFileCreationDateTimeProperty).Value then
      begin
        ftTime := DateTimeToWfxFileTime((aTemplateProperty as TFileCreationDateTimeProperty).Value);
        with FWfxPluginFileSource.WfxModule do
          if not WfxSetTime(aFile.FullPath, @ftTime, nil, nil) then
            Result := sfprError;
      end
      else
        Result := sfprSkipped;

    fpLastAccessTime:
      if (aTemplateProperty as TFileLastAccessDateTimeProperty).Value <>
         (aFile.Properties[fpLastAccessTime] as TFileLastAccessDateTimeProperty).Value then
      begin
        ftTime := DateTimeToWfxFileTime((aTemplateProperty as TFileLastAccessDateTimeProperty).Value);
        with FWfxPluginFileSource.WfxModule do
          if not WfxSetTime(aFile.FullPath, nil, @ftTime, nil) then
            Result := sfprError;
      end
      else
        Result := sfprSkipped;

    else
      raise Exception.Create('Trying to set unsupported property');
  end;
end;

end.

