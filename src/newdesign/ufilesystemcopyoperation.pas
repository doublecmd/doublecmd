unit uFileSystemCopyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCopyOperation,
  uFileSystemFileSource,
  uFileSource,
  uFile;

type
  {
    Both operations are the same, just source and target reversed.
    Implement them in terms of the same functions,
    or have one use the other.
  }

  TFileSystemCopyInOperation = class(TFileSourceCopyInOperation)

  private
    FSourceFileSource: TFileSystemFileSource;
    FTargetFileSource: TFileSystemFileSource;
    FSourceFiles: TFiles;
    FTargetFiles: TFiles;

  public
    constructor Create(SourceFileSource: TFileSystemFileSource;
                       TargetFileSource: TFileSystemFileSource;
                       SourceFiles: TFiles;
                       TargetFiles: TFiles); reintroduce;

    procedure Execute; override;

  end;

  TFileSystemCopyOutOperation = class(TFileSourceCopyOutOperation)

  private
    FSourceFileSource: TFileSystemFileSource;
    FTargetFileSource: TFileSystemFileSource;
    FSourceFiles: TFiles;
    FTargetFiles: TFiles;
    FStatistics: TFileSourceCopyOperationStatistics;
    FCounter: Integer;

  public
    constructor Create(SourceFileSource: TFileSystemFileSource;
                       TargetFileSource: TFileSystemFileSource;
                       SourceFiles: TFiles;
                       TargetFiles: TFiles); reintroduce;

    procedure fExecute; override;

    procedure Initialize; override;
    function  ExecuteStep: Boolean; override;
    procedure Finalize; override;

  end;

implementation

uses
  uFileSourceOperation;

// -- TFileSystemCopyInOperation ----------------------------------------------

constructor TFileSystemCopyInOperation.Create(SourceFileSource: TFileSystemFileSource;
                                              TargetFileSource: TFileSystemFileSource;
                                              SourceFiles: TFiles;
                                              TargetFiles: TFiles);
begin
  inherited Create;

  FSourceFileSource := SourceFileSource;
  FTargetFileSource := TargetFileSource;
  FSourceFiles := SourceFiles;
  FTargetFiles := TargetFiles;
end;

procedure TFileSystemCopyInOperation.Execute;
begin
end;

// -- TFileSystemCopyOutOperation ---------------------------------------------

constructor TFileSystemCopyOutOperation.Create(SourceFileSource: TFileSystemFileSource;
                                              TargetFileSource: TFileSystemFileSource;
                                              SourceFiles: TFiles;
                                              TargetFiles: TFiles);
begin
  inherited Create;

  FSourceFileSource := SourceFileSource;
  FTargetFileSource := TargetFileSource;
  FSourceFiles := SourceFiles;
  FTargetFiles := TargetFiles;
end;

procedure TFileSystemCopyOutOperation.fExecute;
var
  i: Integer;
  Statistics: TFileSourceCopyOperationStatistics;
begin
  if GetDesiredState <> fsosRunning then
    DoPause;

  UpdateState(fsosStarting);

  // Get initialized statistics; then we change only what is needed.
  Statistics := RetrieveStatistics;

  with Statistics do
  begin
    TotalBytes := 300 * 50;
    TotalFiles := 300;
  end;

  UpdateState(fsosRunning);

  // Main loop follows.

  // Some dummy long operation for now.
  for i := 1 to 300 do
  begin
    case GetDesiredState of
      fsosPaused:
        begin
          DoPause;
        end;

      fsosStopped:
        begin
          //cleanup
          Exit;
        end;
    end;

    with Statistics do
    begin
      CurrentFileFrom := 'sourceFile_' + inttostr(i)  +'.pas';
      CurrentFileTo := 'targetFile_'+ inttostr(i)+ '.pas';
    end;

    UpdateStatistics(Statistics);

    // Main work single step.
    Sleep(50);

    // Update overall progress.
    // (should this be under the same lock as statistics?)
    UpdateProgress((i * 100)  div  300);

    // Update specific statistics.
    with Statistics do
    begin
      DoneFiles := DoneFiles + 1;
      DoneBytes := DoneBytes + 50;
    end;
  end;

  // Final statistics.
  UpdateStatistics(Statistics);
  UpdateProgress(100);

  // cleanup
end;

procedure TFileSystemCopyOutOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  with FStatistics do
  begin
    TotalBytes := 300 * 50;
    TotalFiles := 300;
  end;

  FCounter := 1;
end;

function TFileSystemCopyOutOperation.ExecuteStep: Boolean;
begin
  // Some dummy long operation for now.
  if FCounter <= 300 then
  begin
    with FStatistics do
    begin
      CurrentFileFrom := 'sourceFile_' + inttostr(FCounter)  +'.pas';
      CurrentFileTo := 'targetFile_'+ inttostr(FCounter)+ '.pas';
    end;

    UpdateStatistics(FStatistics);

    // Main work single step.
    Sleep(50);

    // Update overall progress.
    // (should this be under the same lock as statistics?)
    UpdateProgress((FCounter * 100)  div  300);

    // Update specific statistics.
    with FStatistics do
    begin
      DoneFiles := DoneFiles + 1;
      DoneBytes := DoneBytes + 50;
    end;

    FCounter := FCounter + 1;

    Result := True;
  end
  else
    Result := False;
end;

procedure TFileSystemCopyOutOperation.Finalize;
begin
  // Final statistics.
  UpdateStatistics(FStatistics);
end;

end.

