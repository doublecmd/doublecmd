unit uFileSourceSplitOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource,
  uFile,
  uFileSourceCopyOperation;

type

   TFileSourceSplitOperationStatistics = TFileSourceCopyOperationStatistics;

  {en
     Operation that split file within the same file source.
  }

  { TFileSourceSplitOperation }

  TFileSourceSplitOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceSplitOperationStatistics;
    FStatisticsAtStartTime: TFileSourceSplitOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.
    FFileSource: IFileSource;
    FSourceFile: TFile;
    FTargetPath: String;
    FVolumeSize: Int64;
    FVolumeNumber: LongInt;

  protected
    function GetID: TFileSourceOperationType; override;
    procedure DoReloadFileSources; override;

    procedure UpdateStatistics(var NewStatistics: TFileSourceSplitOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;

    property FileSource: IFileSource read FFileSource;
    property SourceFile: TFile read FSourceFile;
    property TargetPath: String read FTargetPath;

  public
    {en
       @param(aFileSource
              File source within which the operation should take place.
              Class takes ownership of the pointer.)
       @param(aSourceFile
              The file which are to be splitted.
              Class takes ownership of the pointer.)
       @param(aTargetPath
              Target path for splitted files.)
    }
    constructor Create(aFileSource: IFileSource;
                       var aSourceFile: TFile;
                       aTargetPath: String); virtual reintroduce;

    destructor Destroy; override;

    function GetDescription(Details: TFileSourceOperationDescriptionDetails): String; override;
    function RetrieveStatistics: TFileSourceSplitOperationStatistics;

    property VolumeSize: Int64 read FVolumeSize write FVolumeSize;
    property VolumeNumber: LongInt read FVolumeNumber write FVolumeNumber;
  end;

implementation

uses
  uDCUtils, uLng;

// -- TFileSourceSplitOperation ------------------------------------------------

constructor TFileSourceSplitOperation.Create(aFileSource: IFileSource;
                                            var aSourceFile: TFile;
                                            aTargetPath: String);
begin
  with FStatistics do
  begin
    CurrentFileFrom := '';
    CurrentFileTo := '';

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

  inherited Create(aFileSource);

  FFileSource := aFileSource;
  FSourceFile := aSourceFile;
  aSourceFile := nil;
  FTargetPath := IncludeTrailingPathDelimiter(aTargetPath);
end;

destructor TFileSourceSplitOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FStatisticsLock) then
    FreeAndNil(FStatisticsLock);
  if Assigned(FSourceFile) then
    FreeAndNil(FSourceFile);
end;

procedure TFileSourceSplitOperation.UpdateStatistics(var NewStatistics: TFileSourceSplitOperationStatistics);
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

procedure TFileSourceSplitOperation.UpdateStatisticsAtStartTime;
begin
  FStatisticsLock.Acquire;
  try
    Self.FStatisticsAtStartTime := Self.FStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

function TFileSourceSplitOperation.RetrieveStatistics: TFileSourceSplitOperationStatistics;
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

function TFileSourceSplitOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoSplit;
end;

procedure TFileSourceSplitOperation.DoReloadFileSources;
var
  Paths: TPathsArray;
begin
  SetLength(Paths, 1);
  Paths[0] := FTargetPath;  // Split target path
  FFileSource.Reload(Paths);
end;

function TFileSourceSplitOperation.GetDescription(Details: TFileSourceOperationDescriptionDetails): String;
begin
  case Details of
    fsoddJobAndTarget:
      Result := Format(rsOperSplittingFromTo, [SourceFile.Path, TargetPath]);
    else
      Result := rsOperSplitting;
  end;
end;

end.

