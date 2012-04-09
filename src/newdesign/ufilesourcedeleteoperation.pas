unit uFileSourceDeleteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource,
  uFile;

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

  { TFileSourceDeleteOperation }

  TFileSourceDeleteOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceDeleteOperationStatistics;
    FStatisticsAtStartTime: TFileSourceDeleteOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.
    FFileSource: IFileSource;
    FFilesToDelete: TFiles;

  protected
    function GetID: TFileSourceOperationType; override;
    procedure DoReloadFileSources; override;

    procedure UpdateStatistics(var NewStatistics: TFileSourceDeleteOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;

    property FileSource: IFileSource read FFileSource;
    property FilesToDelete: TFiles read FFilesToDelete;

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToDelete: TFiles); virtual reintroduce;
    destructor Destroy; override;

    function GetDescription(Details: TFileSourceOperationDescriptionDetails): String; override;
    function RetrieveStatistics: TFileSourceDeleteOperationStatistics;
  end;

implementation

uses
  uDCUtils, uLng;

constructor TFileSourceDeleteOperation.Create(aTargetFileSource: IFileSource;
                                              var theFilesToDelete: TFiles);
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

  inherited Create(aTargetFileSource);

  FFileSource := aTargetFileSource;
  aTargetFileSource := nil;
  FFilesToDelete := theFilesToDelete;
  theFilesToDelete := nil;
end;

destructor TFileSourceDeleteOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FStatisticsLock) then
    FreeAndNil(FStatisticsLock);
  if Assigned(FFilesToDelete) then
    FreeAndNil(FFilesToDelete);
end;

function TFileSourceDeleteOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoDelete;
end;

procedure TFileSourceDeleteOperation.DoReloadFileSources;
begin
  FFileSource.Reload(FFilesToDelete.Path);
end;

function TFileSourceDeleteOperation.GetDescription(Details: TFileSourceOperationDescriptionDetails): String;
begin
  case Details of
    fsoddJobAndTarget:
    begin
      if FilesToDelete.Count = 1 then
        Result := Format(rsOperDeletingSomething, [FilesToDelete[0].FullPath])
      else
        Result := Format(rsOperDeletingIn, [FilesToDelete.Path]);
    end;
    else
      Result := rsOperDeleting;
  end;
end;

procedure TFileSourceDeleteOperation.UpdateStatistics(var NewStatistics: TFileSourceDeleteOperationStatistics);
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

end.

