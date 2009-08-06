unit uFileSourceCalcStatisticsOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource,
  uFileProperty,
  uFile;

type

  TFileSourceCalcStatisticsOperationStatistics = record
    SupportedProperties: TFilePropertiesTypes;
    CurrentFile: String;
    Files: Int64;          // only files, i.e., not directories
    Directories: Int64;
    Links: Int64;
    Size: Int64;           // total size of all the files
    CompressedSize: Int64; // if fpCompressedSize supported
    OldestFile: TDateTime; // if fpModificationTime (or fpDateTime) supported
    NewestFile: TDateTime;
    // Maybe some other:
    // SystemFiles
    // ReadOnlyFiles
    // ExecutableFiles
  end;

  {en
     Operation that calculates several statistics for a directory tree.
  }
  TFileSourceCalcStatisticsOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceCalcStatisticsOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.
    FFileSource: TFileSource;
    FFiles: TFiles;

  protected
    function GetID: TFileSourceOperationType; override;

    procedure UpdateStatistics(var NewStatistics: TFileSourceCalcStatisticsOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;

    property FileSource: TFileSource read FFileSource;
    property Files: TFiles read FFiles;

  public
    constructor Create(var aTargetFileSource: TFileSource;
                       var theFiles: TFiles); virtual reintroduce;

    destructor Destroy; override;

    function RetrieveStatistics: TFileSourceCalcStatisticsOperationStatistics;
  end;

implementation

uses
  uDCUtils;

constructor TFileSourceCalcStatisticsOperation.Create(
                var aTargetFileSource: TFileSource;
                var theFiles: TFiles);
begin
  with FStatistics do
  begin
    SupportedProperties := aTargetFileSource.SupportedFileProperties;
    CurrentFile := '';

    Files := 0;
    Directories := 0;
    Links := 0;
    Size := 0;
    CompressedSize := 0; // if fpCompressedSize supported
    OldestFile := 0;
    NewestFile := 0;
  end;

  FStatisticsLock := TCriticalSection.Create;

  inherited Create(aTargetFileSource, nil);

  FFileSource := aTargetFileSource;
  aTargetFileSource := nil;
  FFiles := theFiles;
  theFiles := nil;
end;

destructor TFileSourceCalcStatisticsOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FStatisticsLock) then
    FreeAndNil(FStatisticsLock);
  if Assigned(FFiles) then
    FreeAndNil(FFiles);
  if Assigned(FFileSource) then
    FreeAndNil(FFileSource);
end;

function TFileSourceCalcStatisticsOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoCalcStatistics;
end;

procedure TFileSourceCalcStatisticsOperation.UpdateStatistics(
            var NewStatistics: TFileSourceCalcStatisticsOperationStatistics);
begin
  FStatisticsLock.Acquire;
  try
    // Cannot determine progress for this operation.

    FStatistics := NewStatistics;

  finally
    FStatisticsLock.Release;
  end;
end;

procedure TFileSourceCalcStatisticsOperation.UpdateStatisticsAtStartTime;
begin
  // Empty, because we don't have any progress or remaining time.
end;

function TFileSourceCalcStatisticsOperation.RetrieveStatistics: TFileSourceCalcStatisticsOperationStatistics;
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

