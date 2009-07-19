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

  public
    constructor Create(SourceFileSource: TFileSystemFileSource;
                       TargetFileSource: TFileSystemFileSource;
                       SourceFiles: TFiles;
                       TargetFiles: TFiles); reintroduce;

    procedure Execute; override;

  end;

implementation

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

procedure TFileSystemCopyOutOperation.Execute;
var
  i: Integer;
  Statistics: TFileSourceCopyOperationStatistics;
begin
  // Get initialized statistics; then we change only what is needed.
  Statistics := RetrieveStatistics;

  with Statistics do
  begin
    TotalBytes := 300 * 50;
    TotalFiles := 300;
  end;

  // Some dummy long operation for now.
  for i := 1 to 300 do
  begin
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
end;

end.

