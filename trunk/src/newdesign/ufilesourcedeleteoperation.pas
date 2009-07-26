unit uFileSourceDeleteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  uFileSourceOperation,
  uFileSourceOperationTypes;

type

  TFileSourceDeleteOperationStatistics = record
    CurrentFile: String;
    TotalFiles: Int64;
    DoneFiles: Int64;
    TotalBytes: Int64;
    DoneBytes: Int64;
    FilesPerSecond: Int64;
    RemainingTime: TDateTime;
  end;

  {en
     Operation that deletes files from an arbitrary file source.
     File source should match the class type.
  }
  TFileSourceDeleteOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceDeleteOperationStatistics;
    FStatisticsAtStartTime: TFileSourceDeleteOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.

  protected
    function GetID: TFileSourceOperationType; override;

    procedure UpdateStatistics(NewStatistics: TFileSourceDeleteOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;
    procedure EstimateSpeedAndTime(var theStatistics: TFileSourceDeleteOperationStatistics);

  public
    constructor Create; override;
    destructor Destroy; override;

    function RetrieveStatistics: TFileSourceDeleteOperationStatistics;
  end;

implementation

uses
  uDCUtils;

constructor TFileSourceDeleteOperation.Create;
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

  inherited Create;
end;

destructor TFileSourceDeleteOperation.Destroy;
begin
  inherited Destroy;

  FreeAndNil(FStatisticsLock);
end;

function TFileSourceDeleteOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoDelete;
end;

procedure TFileSourceDeleteOperation.UpdateStatistics(NewStatistics: TFileSourceDeleteOperationStatistics);
begin
  FStatisticsLock.Acquire;
  try
    Self.FStatistics := NewStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

procedure TFileSourceDeleteOperation.UpdateStatisticsAtStartTime;
begin
  FStatisticsLock.Acquire;
  try
    Self.FStatisticsAtStartTime := Self.FStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

function TFileSourceDeleteOperation.RetrieveStatistics: TFileSourceDeleteOperationStatistics;
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

procedure TFileSourceDeleteOperation.EstimateSpeedAndTime(
              var theStatistics: TFileSourceDeleteOperationStatistics);
begin
  FStatisticsLock.Acquire;
  try
    theStatistics.RemainingTime :=
        EstimateRemainingTime(FStatisticsAtStartTime.DoneFiles,
                              theStatistics.DoneFiles,
                              theStatistics.TotalFiles,
                              StartTime,
                              SysUtils.Now,
                              theStatistics.FilesPerSecond);
  finally
    FStatisticsLock.Release;
  end;
end;

end.

