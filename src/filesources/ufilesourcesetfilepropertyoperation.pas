unit uFileSourceSetFilePropertyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  DCBasicTypes,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource,
  uFile,
  uFileProperty;

type

  TSetFilePropertyResult = (sfprSuccess, sfprError, sfprSkipped);

  TSetFilePropertyResultFunction = procedure(Index: Integer; aFile: TFile;
      aTemplate: TFileProperty; Result: TSetFilePropertyResult) of object;

  PFileSourceSetFilePropertyOperationStatistics = ^TFileSourceSetFilePropertyOperationStatistics;
  TFileSourceSetFilePropertyOperationStatistics = record
    CurrentFile: String;
    TotalFiles: Int64;
    DoneFiles: Int64;
    FilesPerSecond: Int64;
    RemainingTime: TDateTime;
  end;

  {en
     Operation that can set any of the file properties supported by
     a file source. It doesn't have to support all the file properties
     supported by the file source, it can be a subset.

     There are two methods of setting properties available:

     - NewProperties
       Set via constructor, this is a list of properties that should be set for
       each file. If a property in this list is not assigned it is not set.
       If a property in this list is not supported by the file source or by
       this operation it is also not set.

     - TemplateFiles
       Set by calling SetTemplateFiles.
       Template files describe 1 to 1 correspondence between files and their
       new properties. Each i-th file in the TargetFiles list will be assigned
       properties based on propertes of i-th template file.
       Template files need not be of the same type as target files,
       it is enough for them to have properties supported by the target files.
       If template file is not used for i-th file, then the i-th member
       of the list should be set to @nil, but should be present to maintain
       the correct correspondence between target and template files.
       In other words number of target files must be the same as number of
       template files.

     The two above methods can be used together.
     Template files, if present, always take precedence over NewProperties.
     If a template file is not present (= @nil), then theNewProperties are used as a template.
     Template files usually will not be used when Recursive is @true,
     although this behaviour is dependent on the concrete descendant operations.
     If template files list is @nil, to indicate that the template files
     are not used, then only the NewProperties are used.)
  }

  { TFileSourceSetFilePropertyOperation }

  TFileSourceSetFilePropertyOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceSetFilePropertyOperationStatistics;
    FStatisticsAtStartTime: TFileSourceSetFilePropertyOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.
    FFileSource: IFileSource;
    FTargetFiles: TFiles;
    FTemplateFiles: TFiles;
    FNewProperties: TFileProperties;
    FReload: Boolean;
    FRecursive: Boolean;
    FSkipErrors: Boolean;

  protected
    FSupportedProperties: TFilePropertiesTypes;
    FSetFilePropertyResultFunction: TSetFilePropertyResultFunction;

    function GetID: TFileSourceOperationType; override;
    procedure DoReloadFileSources; override;

    procedure UpdateStatistics(var NewStatistics: TFileSourceSetFilePropertyOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;

    procedure SetProperties(Index: Integer; aFile: TFile; aTemplateFile: TFile);
    function SetNewProperty(aFile: TFile; aTemplateProperty: TFileProperty): TSetFilePropertyResult; virtual abstract;

    function GetErrorString(aFile: TFile; aProperty: TFileProperty): String;

    property FileSource: IFileSource read FFileSource;

  public
    IncludeAttributes: TFileAttrs;
    ExcludeAttributes: TFileAttrs;

  public
    {en
       @param(aTargetFileSource
              File source on which the operation will be executed.)
       @param(theTargetFiles
              List of files which properties should be changed.)
       @param(theNewProperties
              Describes the set of properties that should be set for each file
              of theTargetFiles. All elements of this parameter will be freed automatically.)
    }
    constructor Create(aTargetFileSource: IFileSource;
                       var theTargetFiles: TFiles;
                       var theNewProperties: TFileProperties); virtual reintroduce;

    destructor Destroy; override;

    procedure SetTemplateFiles(var theTemplateFiles: TFiles);

    function GetDescription(Details: TFileSourceOperationDescriptionDetails): String; override;
    function RetrieveStatistics: TFileSourceSetFilePropertyOperationStatistics;

    property Reload: Boolean write FReload;
    property TargetFiles: TFiles read FTargetFiles;
    property NewProperties: TFileProperties read FNewProperties write FNewProperties;
    property TemplateFiles: TFiles read FTemplateFiles; // set by SetTemplateFiles because can't use "var" in properties
    property Recursive: Boolean read FRecursive write FRecursive;
    property SupportedProperties: TFilePropertiesTypes read FSupportedProperties;
    property SkipErrors: Boolean read FSkipErrors write FSkipErrors;
    property OnSetFilePropertyResult: TSetFilePropertyResultFunction write FSetFilePropertyResultFunction;
  end;

implementation

uses
  uDCUtils, uGlobs, uLog, uLng, uFileSourceOperationUI;

constructor TFileSourceSetFilePropertyOperation.Create(aTargetFileSource: IFileSource;
                                                       var theTargetFiles: TFiles;
                                                       var theNewProperties: TFileProperties);
begin
  with FStatistics do
  begin
    CurrentFile := '';
    TotalFiles := 0;
    DoneFiles := 0;
    FilesPerSecond := 0;
    RemainingTime := 0;
  end;

  FStatisticsLock := TCriticalSection.Create;

  inherited Create(aTargetFileSource);

  FFileSource := aTargetFileSource;
  aTargetFileSource := nil;
  FTargetFiles := theTargetFiles;
  theTargetFiles := nil;
  FNewProperties := theNewProperties;
  FillByte(theNewProperties, SizeOf(theNewProperties), 0);
  FTemplateFiles := nil;
  FReload := True;
  FRecursive := False;
  FSkipErrors := gSkipFileOpError;

  FSupportedProperties := [];
end;

destructor TFileSourceSetFilePropertyOperation.Destroy;
var
  prop: TFilePropertyType;
begin
  inherited Destroy;

  if Assigned(FStatisticsLock) then
    FreeAndNil(FStatisticsLock);
  if Assigned(FTargetFiles) then
    FreeAndNil(FTargetFiles);
  if Assigned(FTemplateFiles) then
    FreeAndNil(FTemplateFiles);

  for prop := Low(FNewProperties) to High(FNewProperties) do
    if Assigned(FNewProperties[prop]) then
      FreeAndNil(FNewProperties[prop]);
end;

function TFileSourceSetFilePropertyOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoSetFileProperty;
end;

procedure TFileSourceSetFilePropertyOperation.DoReloadFileSources;
begin
  if FReload then FFileSource.Reload(FTargetFiles.Path);
end;

function TFileSourceSetFilePropertyOperation.GetDescription(Details: TFileSourceOperationDescriptionDetails): String;
begin
  case Details of
    fsoddJobAndTarget:
    begin
      if TargetFiles.Count = 1 then
        Result := Format(rsOperSettingPropertyOf, [TargetFiles[0].FullPath])
      else
        Result := Format(rsOperSettingPropertyIn, [TargetFiles.Path]);
    end;
    else
      Result := rsOperSettingProperty;
  end;
end;

procedure TFileSourceSetFilePropertyOperation.UpdateStatistics(var NewStatistics: TFileSourceSetFilePropertyOperationStatistics);
begin
  FStatisticsLock.Acquire;
  try
    // Check if the value by which we calculate progress and remaining time has changed.
    if FStatistics.DoneFiles <> NewStatistics.DoneFiles then
    begin
      with NewStatistics do
      begin
        RemainingTime :=
            EstimateRemainingTime(FStatisticsAtStartTime.DoneFiles,
                                  DoneFiles,
                                  TotalFiles,
                                  StartTime,
                                  SysUtils.Now,
                                  FilesPerSecond);

        // Update overall progress.
        if TotalFiles <> 0 then
          UpdateProgress(DoneFiles/TotalFiles);
      end;
    end;

    FStatistics := NewStatistics;

    AppProcessMessages();
  finally
    FStatisticsLock.Release;
  end;
end;

procedure TFileSourceSetFilePropertyOperation.UpdateStatisticsAtStartTime;
begin
  FStatisticsLock.Acquire;
  try
    Self.FStatisticsAtStartTime := Self.FStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

function TFileSourceSetFilePropertyOperation.RetrieveStatistics: TFileSourceSetFilePropertyOperationStatistics;
begin
  // Statistics have to be synchronized because there are multiple values
  // and they all have to be consistent at every moment.
  FStatisticsLock.Acquire;
  try
    Result := Self.FStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

procedure TFileSourceSetFilePropertyOperation.SetTemplateFiles(var theTemplateFiles: TFiles);
begin
  if Assigned(FTemplateFiles) then
    FreeAndNil(FTemplateFiles);

  FTemplateFiles := theTemplateFiles;
  theTemplateFiles := nil;
end;

procedure TFileSourceSetFilePropertyOperation.SetProperties(Index: Integer;
  aFile: TFile; aTemplateFile: TFile);
var
  FileAttrs: TFileAttrs;
  AProp: TFilePropertyType;
  templateProperty: TFileProperty;
  bRetry: Boolean;
  sMessage, sQuestion: String;
  SetResult: TSetFilePropertyResult;
  ErrorString: String;
begin
  // Iterate over all properties supported by this operation.
  for AProp := Low(SupportedProperties) to High(SupportedProperties) do
  begin
    repeat
      bRetry := False;
      SetResult := sfprSuccess;

      // Double-check that the property really is supported by the file.
      if ((AProp in (aFile.SupportedProperties * fpAll)) or
          (AProp in (FFileSource.GetRetrievableFileProperties * fpAll))) then
      begin
        // Get template property from template file (if exists) or NewProperties.
        if Assigned(aTemplateFile) then
          templateProperty := aTemplateFile.Properties[AProp]
        else
          templateProperty := NewProperties[AProp];

        // Check if there is a new property to be set.
        if Assigned(templateProperty) then
        begin
          // Special case for attributes property
          if templateProperty is TFileAttributesProperty then
          begin
            if (IncludeAttributes <> 0) or (ExcludeAttributes <> 0) then
            begin
              FileAttrs:= aFile.Attributes;
              FileAttrs:= FileAttrs or IncludeAttributes;
              FileAttrs:= FileAttrs and not ExcludeAttributes;
              TFileAttributesProperty(templateProperty).Value:= FileAttrs;
            end;
          end;

          SetResult := SetNewProperty(aFile, templateProperty);

          if Assigned(FSetFilePropertyResultFunction) then begin
            FSetFilePropertyResultFunction(Index, aFile, templateProperty, SetResult);
          end;
        end;
      end;

      if SetResult = sfprError then
        begin
          ErrorString := GetErrorString(aFile, templateProperty);

          sMessage := rsMsgLogError + ErrorString;
          sQuestion := ErrorString;

          if FSkipErrors then
            logWrite(Thread, sMessage, lmtError)
          else
            begin
              case AskQuestion(sQuestion, '',
                               [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                               fsourRetry, fsourAbort) of
              fsourRetry:
                bRetry := True;
              fsourSkipAll:
                FSkipErrors := True;
              fsourAbort:
                RaiseAbortOperation;
              end;
            end;
        end;
    until bRetry = False;
  end;
end;

function TFileSourceSetFilePropertyOperation.GetErrorString(aFile: TFile; aProperty: TFileProperty): String;
begin
  case aProperty.GetID of
    fpName:
      Result := Format(rsMsgErrRename, [aFile.FullPath, (aProperty as TFileNameProperty).Value]);

    fpAttributes:
      Result := Format(rsMsgErrSetAttribute, [aFile.FullPath]);

    fpModificationTime, fpCreationTime, fpLastAccessTime:
      Result := Format(rsMsgErrSetDateTime, [aFile.FullPath]);

    fpOwner:
      Result := Format(rsMsgErrSetOwnership, [aFile.FullPath]);

    else
      Result := rsMsgLogError;
  end;
end;

end.

