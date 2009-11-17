unit uFileSourceSetFilePropertyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource,
  uFile,
  uFileProperty,
  uLng;

const
  SetFilePropertyErrMsg: array[TFilePropertyType] of String =
    (rsMsgErrRename,
     rsMsgLogError,
     rsMsgLogError,
     rsMsgErrSetAttribute,
     rsMsgErrSetDateTime,
     rsMsgErrSetDateTime,
     rsMsgErrSetDateTime,
     rsMsgErrSetDateTime);

type

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
  TFileSourceSetFilePropertyOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceSetFilePropertyOperationStatistics;
    FStatisticsAtStartTime: TFileSourceSetFilePropertyOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.
    FFileSource: IFileSource;
    FTargetFiles: TFiles;
    FTemplateFiles: TFiles;
    FNewProperties: TFileProperties;
    FRecursive: Boolean;
    FSkipErrors: Boolean;

  protected
    FSupportedProperties: TFilePropertiesTypes;

    function GetID: TFileSourceOperationType; override;

    procedure UpdateStatistics(var NewStatistics: TFileSourceSetFilePropertyOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;

    procedure SetProperties(aFile: TFile; aTemplateFile: TFile);
    function SetNewProperty(aFile: TFile; aTemplateProperty: TFileProperty): Boolean; virtual abstract;

    property FileSource: IFileSource read FFileSource;
    property TargetFiles: TFiles read FTargetFiles;

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
                       theNewProperties: TFileProperties); virtual reintroduce;

    destructor Destroy; override;

    procedure SetTemplateFiles(var theTemplateFiles: TFiles);

    function RetrieveStatistics: TFileSourceSetFilePropertyOperationStatistics;

    property NewProperties: TFileProperties read FNewProperties;
    property TemplateFiles: TFiles read FTemplateFiles; // set by SetTemplateFiles because can't use "var" in properties
    property Recursive: Boolean read FRecursive write FRecursive;
    property SupportedProperties: TFilePropertiesTypes read FSupportedProperties;
  end;

implementation

uses
  uDCUtils, uGlobs, uLog, uFileSourceOperationUI;

constructor TFileSourceSetFilePropertyOperation.Create(aTargetFileSource: IFileSource;
                                                       var theTargetFiles: TFiles;
                                                       theNewProperties: TFileProperties);
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

  inherited Create(aTargetFileSource, aTargetFileSource);

  FFileSource := aTargetFileSource;
  aTargetFileSource := nil;
  FTargetFiles := theTargetFiles;
  theTargetFiles := nil;
  FNewProperties := theNewProperties;
  FTemplateFiles := nil;
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

  for prop := Low(TFilePropertiesTypes) to High(TFilePropertiesTypes) do
    if Assigned(FNewProperties[prop]) then
      FreeAndNil(FNewProperties[prop]);
end;

function TFileSourceSetFilePropertyOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoSetFileProperty;
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
          UpdateProgress((DoneFiles * 100) div TotalFiles);
      end;
    end;

    FStatistics := NewStatistics;

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

procedure TFileSourceSetFilePropertyOperation.SetProperties(aFile: TFile;
                                                            aTemplateFile: TFile);
var
  prop: TFilePropertyType;
  templateProperty: TFileProperty;
  {$IFNDEF AssignFileNameProperty}
  aNameProperty: TFileNameProperty = nil;
  {$ENDIF}
  bRetry: Boolean;
  sMessage, sQuestion: String;
  Success: Boolean;
begin
  // Iterate over all properties supported by this operation.
  for prop := Low(SupportedProperties) to High(SupportedProperties) do
  begin
    repeat
      bRetry := False;
      Success := True;

      {$IFNDEF AssignFileNameProperty}
      if prop = fpName then
      begin
        if Assigned(aTemplateFile) then
        begin
          aNameProperty := TFileNameProperty.Create(aTemplateFile.Name);
          Success := SetNewProperty(aFile, aNameProperty);
          FreeAndNil(aNameProperty);
        end
        else if Assigned(NewProperties[fpName]) then
        begin
          Success := SetNewProperty(aFile, NewProperties[fpName]);
        end;
      end
      else
      {$ENDIF}
      // Double-check that the property really is supported by the file.
      if prop in aFile.SupportedProperties then
      begin
        // Get template property from template file (if exists) or NewProperties.
        if Assigned(aTemplateFile) then
          templateProperty := aTemplateFile.Properties[prop]
        else
          templateProperty := NewProperties[prop];

        // Check if there is a new property to be set.
        if Assigned(templateProperty) then
          Success := SetNewProperty(aFile, templateProperty);
      end;

      if not Success then
        begin
          sMessage := Format(rsMsgLogError + SetFilePropertyErrMsg[prop], [aFile.FullPath]);
          sQuestion := Format(SetFilePropertyErrMsg[prop], [aFile.FullPath]);

          if FSkipErrors then
            logWrite(Thread, sMessage, lmtError)
          else
            begin
              case AskQuestion(sQuestion, '',
                               [fsourRetry, fsourSkip, fsourSkipAll, fsourAbort],
                               fsourRetry, fsourSkip) of
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

end.

