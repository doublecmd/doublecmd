unit uFileSourceMoveOperation;

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

   TFileSourceMoveOperationStatistics = TFileSourceCopyOperationStatistics;

  {en
     Operation that moves or renames files within the same file source
     (for example: in the same archive, in the same ftp server).
  }
  TFileSourceMoveOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceMoveOperationStatistics;
    FStatisticsAtStartTime: TFileSourceMoveOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.
    FFileSource: IFileSource;
    FSourceFiles: TFiles;
    FTargetPath: String;
    FRenameMask: String;

  protected
    function GetID: TFileSourceOperationType; override;
    procedure DoReloadFileSources; override;

    procedure UpdateStatistics(var NewStatistics: TFileSourceMoveOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;

    property FileSource: IFileSource read FFileSource;
    property SourceFiles: TFiles read FSourceFiles;
    property TargetPath: String read FTargetPath;

  public
    {en
       @param(aFileSource
              File source within which the operation should take place.
              Class takes ownership of the pointer.)
       @param(theSourceFiles
              Files which are to be moved.
              Class takes ownership of the pointer.)
       @param(aTargetPath
              Path in the file source where the files should be moved.)
    }
    constructor Create(aFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); virtual reintroduce;

    destructor Destroy; override;

    function RetrieveStatistics: TFileSourceMoveOperationStatistics;

    property RenameMask: String read FRenameMask write FRenameMask;
  end;

implementation

uses
  uDCUtils;

// -- TFileSourceMoveOperation ------------------------------------------------

constructor TFileSourceMoveOperation.Create(aFileSource: IFileSource;
                                            var theSourceFiles: TFiles;
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
  FSourceFiles := theSourceFiles;
  theSourceFiles := nil;
  FTargetPath := IncludeTrailingPathDelimiter(aTargetPath);

  FRenameMask := '';
end;

destructor TFileSourceMoveOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FStatisticsLock) then
    FreeAndNil(FStatisticsLock);
  if Assigned(FSourceFiles) then
    FreeAndNil(FSourceFiles);
end;

procedure TFileSourceMoveOperation.UpdateStatistics(var NewStatistics: TFileSourceMoveOperationStatistics);
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

procedure TFileSourceMoveOperation.UpdateStatisticsAtStartTime;
begin
  FStatisticsLock.Acquire;
  try
    Self.FStatisticsAtStartTime := Self.FStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

function TFileSourceMoveOperation.RetrieveStatistics: TFileSourceMoveOperationStatistics;
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

function TFileSourceMoveOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoMove;
end;

procedure TFileSourceMoveOperation.DoReloadFileSources;
var
  Paths: TPathsArray;
begin
  SetLength(Paths, 2);
  Paths[0] := FSourceFiles.Path;  // Move source path
  Paths[1] := FTargetPath;        // Move target path
  FFileSource.Reload(Paths);
end;

end.

