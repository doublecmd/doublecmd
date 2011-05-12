unit uFileSourceCombineOperation;

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

   TFileSourceCombineOperationStatistics = TFileSourceCopyOperationStatistics;

  {en
     Operation that combine files within the same file source.
  }
  TFileSourceCombineOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceCombineOperationStatistics;
    FStatisticsAtStartTime: TFileSourceCombineOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.
    FFileSource: IFileSource;
    FSourceFiles: TFiles;
    FTargetFile: String;

  protected
    function GetID: TFileSourceOperationType; override;
    procedure DoReloadFileSources; override;

    procedure UpdateStatistics(var NewStatistics: TFileSourceCombineOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;

    property FileSource: IFileSource read FFileSource;
    property SourceFiles: TFiles read FSourceFiles;
    property TargetFile: String read FTargetFile;

  public
    {en
       @param(aFileSource
              File source within which the operation should take place.
              Class takes ownership of the pointer.)
       @param(theSourceFiles
              Files which are to be combined.
              Class takes ownership of the pointer.)
       @param(aTargetFile
              Target name of combined file.)
    }
    constructor Create(aFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetFile: String); virtual reintroduce;

    destructor Destroy; override;

    function RetrieveStatistics: TFileSourceCombineOperationStatistics;

  end;

implementation

uses
  uDCUtils;

// -- TFileSourceCombineOperation ------------------------------------------------

constructor TFileSourceCombineOperation.Create(aFileSource: IFileSource;
                                            var theSourceFiles: TFiles;
                                            aTargetFile: String);
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
  FSourceFiles := theSourceFiles;
  theSourceFiles := nil;
  FTargetFile := aTargetFile;
end;

destructor TFileSourceCombineOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FStatisticsLock) then
    FreeAndNil(FStatisticsLock);
  if Assigned(FSourceFiles) then
    FreeAndNil(FSourceFiles);
end;

procedure TFileSourceCombineOperation.UpdateStatistics(var NewStatistics: TFileSourceCombineOperationStatistics);
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

procedure TFileSourceCombineOperation.UpdateStatisticsAtStartTime;
begin
  FStatisticsLock.Acquire;
  try
    Self.FStatisticsAtStartTime := Self.FStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

function TFileSourceCombineOperation.RetrieveStatistics: TFileSourceCombineOperationStatistics;
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

function TFileSourceCombineOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoCombine;
end;

procedure TFileSourceCombineOperation.DoReloadFileSources;
var
  Paths: TPathsArray;
begin
  SetLength(Paths, 1);
  Paths[0] := ExtractFilePath(FTargetFile);  // Combine target path
  FFileSource.Reload(Paths);
end;

end.

