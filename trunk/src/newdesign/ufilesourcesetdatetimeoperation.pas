unit uFileSourceSetDateTimeOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource,
  uFile,
  uOSUtils;

type

  TFileSourceSetDateTimeOperationStatistics = record
    CurrentFile: String;
    TotalFiles: Int64;
    DoneFiles: Int64;
    TotalBytes: Int64;
    DoneBytes: Int64;
    FilesPerSecond: Int64;
    RemainingTime: TDateTime;
  end;

  {en
     Operation that set date/time for files from an arbitrary file source.
     File source should match the class type.
  }
  TFileSourceSetDateTimeOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceSetDateTimeOperationStatistics;
    FStatisticsAtStartTime: TFileSourceSetDateTimeOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.
    FFileSource: IFileSource;
    FFilesToSetDateTime: TFiles;
    FCreationTime,
    FLastAccessTime,
    FLastWriteTime: TDateTime;
    FRecursive: Boolean;

  protected
    function GetID: TFileSourceOperationType; override;

    procedure UpdateStatistics(var NewStatistics: TFileSourceSetDateTimeOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;

    property FileSource: IFileSource read FFileSource;
    property FilesToSetDateTime: TFiles read FFilesToSetDateTime;

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFilesToSetDateTime: TFiles; aLastWriteTime: TDateTime); virtual reintroduce;
    destructor Destroy; override;

    function RetrieveStatistics: TFileSourceSetDateTimeOperationStatistics;

    property CreationTime: TDateTime read FCreationTime write FCreationTime;
    property LastAccessTime: TDateTime read FLastAccessTime write FLastAccessTime;
    property LastWriteTime: TDateTime read FLastWriteTime write FLastWriteTime;
    property Recursive: Boolean read FRecursive write FRecursive;
  end;

implementation

uses
  uDCUtils;

constructor TFileSourceSetDateTimeOperation.Create(aTargetFileSource: IFileSource;
                                              var theFilesToSetDateTime: TFiles; aLastWriteTime: TDateTime);
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

  inherited Create(aTargetFileSource, aTargetFileSource);

  FFileSource := aTargetFileSource;
  aTargetFileSource := nil;
  FFilesToSetDateTime := theFilesToSetDateTime;
  theFilesToSetDateTime := nil;
  FLastWriteTime:= aLastWriteTime;
  FRecursive:= False;
end;

destructor TFileSourceSetDateTimeOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FStatisticsLock) then
    FreeAndNil(FStatisticsLock);
  if Assigned(FFilesToSetDateTime) then
    FreeAndNil(FFilesToSetDateTime);
end;

function TFileSourceSetDateTimeOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoSetDateTime;
end;

procedure TFileSourceSetDateTimeOperation.UpdateStatistics(var NewStatistics: TFileSourceSetDateTimeOperationStatistics);
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
          UpdateProgress((DoneFiles * 100) div TotalFiles);
      end;
    end;

    FStatistics := NewStatistics;

  finally
    FStatisticsLock.Release;
  end;
end;

procedure TFileSourceSetDateTimeOperation.UpdateStatisticsAtStartTime;
begin
  FStatisticsLock.Acquire;
  try
    Self.FStatisticsAtStartTime := Self.FStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

function TFileSourceSetDateTimeOperation.RetrieveStatistics: TFileSourceSetDateTimeOperationStatistics;
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

