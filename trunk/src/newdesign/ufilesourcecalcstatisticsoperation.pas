unit uFileSourceCalcStatisticsOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSourceOperationOptions,
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
    FFileSource: IFileSource;
    FFiles: TFiles;

  protected
    // Options.
    FSymLinkOption: TFileSourceOperationOptionSymLink;
    FSkipErrors: Boolean;

    function GetID: TFileSourceOperationType; override;

    procedure UpdateStatistics(var NewStatistics: TFileSourceCalcStatisticsOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;

    property FileSource: IFileSource read FFileSource;
    property Files: TFiles read FFiles;

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFiles: TFiles); virtual reintroduce;

    destructor Destroy; override;

    function RetrieveStatistics: TFileSourceCalcStatisticsOperationStatistics;

    property SymLinkOption: TFileSourceOperationOptionSymLink
             read FSymLinkOption write FSymLinkOption;
    property SkipErrors: Boolean read FSkipErrors write FSkipErrors;
  end;

implementation

uses
  uGlobs;

constructor TFileSourceCalcStatisticsOperation.Create(
                aTargetFileSource: IFileSource;
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

  inherited Create(aTargetFileSource);

  FFileSource := aTargetFileSource;
  FFiles := theFiles;
  theFiles := nil;

  FSymLinkOption := fsooslNone;
  FSkipErrors := gSkipFileOpError;
end;

destructor TFileSourceCalcStatisticsOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FStatisticsLock) then
    FreeAndNil(FStatisticsLock);
  if Assigned(FFiles) then
    FreeAndNil(FFiles);
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

