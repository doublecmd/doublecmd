unit uFileSourceTestArchiveOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource,
  uFile;

type

  // Statistics for TestArchive operation.
  TFileSourceTestArchiveOperationStatistics = record
    ArchiveFile: String;
    CurrentFile: String;
    CurrentFileTotalBytes: Int64;
    CurrentFileDoneBytes: Int64;
    TotalFiles: Int64;
    DoneFiles: Int64;
    TotalBytes: Int64;
    DoneBytes: Int64;
    BytesPerSecond: Int64;
    RemainingTime: TDateTime;
  end;

  {en
     Operation that test files in archive.
  }

  { TFileSourceTestArchiveOperation }

  TFileSourceTestArchiveOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceTestArchiveOperationStatistics;
    FStatisticsAtStartTime: TFileSourceTestArchiveOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.
    FSourceFileSource: IFileSource;
    FSourceFiles: TFiles;

  protected
    function GetID: TFileSourceOperationType; override;
    procedure UpdateStatistics(var NewStatistics: TFileSourceTestArchiveOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;

    property SourceFiles: TFiles read FSourceFiles;

  public
    {en
       @param(aSourceFileSource
              File source from which the files will be copied.)
       @param(theSourceFiles
              Files which are to be copied.
              Class takes ownership of the pointer.)
    }
    constructor Create(aSourceFileSource: IFileSource;
                       var theSourceFiles: TFiles); virtual reintroduce;

    destructor Destroy; override;

    function GetDescription(Details: TFileSourceOperationDescriptionDetails): String; override;
    function RetrieveStatistics: TFileSourceTestArchiveOperationStatistics;

  end;

implementation

uses
  uDCUtils, uLng;

// -- TFileSourceTestArchiveOperation ------------------------------------------------

constructor TFileSourceTestArchiveOperation.Create(aSourceFileSource: IFileSource;
                                                   var theSourceFiles: TFiles);
begin
  with FStatistics do
  begin
    ArchiveFile := '';
    CurrentFile := '';

    TotalFiles := 0;
    DoneFiles := 0;
    TotalBytes := 0;
    DoneBytes := 0;
    CurrentFileTotalBytes := 0;
    CurrentFileDoneBytes := 0;
    BytesPerSecond := 0;
    RemainingTime := 0;
  end;

  FStatisticsLock := TCriticalSection.Create;

  inherited Create(aSourceFileSource);

  FSourceFileSource := aSourceFileSource;
  FSourceFiles := theSourceFiles;
  theSourceFiles := nil;
end;

destructor TFileSourceTestArchiveOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FStatisticsLock) then
    FreeAndNil(FStatisticsLock);
  if Assigned(FSourceFiles) then
    FreeAndNil(FSourceFiles);
end;

function TFileSourceTestArchiveOperation.GetDescription(Details: TFileSourceOperationDescriptionDetails): String;
begin
  Result := rsOperTesting;
end;

function TFileSourceTestArchiveOperation.GetID: TFileSourceOperationType;
begin
  Result:= fsoTestArchive;
end;

procedure TFileSourceTestArchiveOperation.UpdateStatistics(var NewStatistics: TFileSourceTestArchiveOperationStatistics);
begin
  FStatisticsLock.Acquire;
  try
    // Check if the value by which we calculate progress and remaining time has changed.
    if FStatistics.DoneBytes <> NewStatistics.DoneBytes then
    begin
      with NewStatistics do
      begin
        RemainingTime :=
          EstimateRemainingTime(FStatisticsAtStartTime.DoneBytes,
                                DoneBytes,
                                TotalBytes,
                                StartTime,
                                SysUtils.Now,
                                BytesPerSecond);

        // Update overall progress.
        if TotalBytes <> 0 then
          UpdateProgress(DoneBytes/TotalBytes);
      end;
    end;

    FStatistics := NewStatistics;

  finally
    FStatisticsLock.Release;
  end;
end;

procedure TFileSourceTestArchiveOperation.UpdateStatisticsAtStartTime;
begin
  FStatisticsLock.Acquire;
  try
    Self.FStatisticsAtStartTime := Self.FStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

function TFileSourceTestArchiveOperation.RetrieveStatistics: TFileSourceTestArchiveOperationStatistics;
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

