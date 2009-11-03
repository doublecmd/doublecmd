unit uFileSourceSetAttributeOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource,
  uFile,
  uOSUtils;

type

  TFileSourceSetAttributeOperationStatistics = record
    CurrentFile: String;
    TotalFiles: Int64;
    DoneFiles: Int64;
    TotalBytes: Int64;
    DoneBytes: Int64;
    FilesPerSecond: Int64;
    RemainingTime: TDateTime;
  end;

  {en
     Operation that set attributes for files from an arbitrary file source.
     File source should match the class type.
  }
  TFileSourceSetAttributeOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceSetAttributeOperationStatistics;
    FStatisticsAtStartTime: TFileSourceSetAttributeOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.
    FFileSource: IFileSource;
    FFilesToSetAttribute: TFiles;
    FNewAttributes: TFileAttrs;
    FRecursive: Boolean;

  protected
    function GetID: TFileSourceOperationType; override;

    procedure UpdateStatistics(var NewStatistics: TFileSourceSetAttributeOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;

    property FileSource: IFileSource read FFileSource;
    property FilesToSetAttribute: TFiles read FFilesToSetAttribute;

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToSetAttribute: TFiles; aNewAttributes: TFileAttrs); virtual reintroduce;
    destructor Destroy; override;

    function RetrieveStatistics: TFileSourceSetAttributeOperationStatistics;
    property NewAttributes: TFileAttrs read FNewAttributes write FNewAttributes;
    property Recursive: Boolean read FRecursive write FRecursive;
  end;

implementation

uses
  uDCUtils;

constructor TFileSourceSetAttributeOperation.Create(aTargetFileSource: IFileSource;
                                              var theFilesToSetAttribute: TFiles; aNewAttributes: TFileAttrs);
begin
  with FStatistics do
  begin
    CurrentFile := '';

    TotalFiles := 0;
    DoneFiles := 0;
    TotalBytes := 0;
    DoneBytes := 0;
    FilesPerSecond := 0;
    RemainingTime := 0;
  end;

  FStatisticsLock := TCriticalSection.Create;

  inherited Create(aTargetFileSource, aTargetFileSource);

  FFileSource := aTargetFileSource;
  aTargetFileSource := nil;
  FFilesToSetAttribute := theFilesToSetAttribute;
  theFilesToSetAttribute := nil;
  FNewAttributes:= aNewAttributes;
  FRecursive:= False;
end;

destructor TFileSourceSetAttributeOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FStatisticsLock) then
    FreeAndNil(FStatisticsLock);
  if Assigned(FFilesToSetAttribute) then
    FreeAndNil(FFilesToSetAttribute);
end;

function TFileSourceSetAttributeOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoSetAttribute;
end;

procedure TFileSourceSetAttributeOperation.UpdateStatistics(var NewStatistics: TFileSourceSetAttributeOperationStatistics);
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

procedure TFileSourceSetAttributeOperation.UpdateStatisticsAtStartTime;
begin
  FStatisticsLock.Acquire;
  try
    Self.FStatisticsAtStartTime := Self.FStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

function TFileSourceSetAttributeOperation.RetrieveStatistics: TFileSourceSetAttributeOperationStatistics;
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

end.

