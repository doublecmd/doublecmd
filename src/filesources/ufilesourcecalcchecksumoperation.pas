unit uFileSourceCalcChecksumOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  DCBasicTypes,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSource,
  uFile,
  uHash;

type

  TCalcCheckSumOperationMode = (checksum_calc, checksum_verify);

  TVerifyChecksumResult = record
    Success: TDynamicStringArray;
    Broken: TDynamicStringArray;
    Missing: TDynamicStringArray;
    ReadError: TDynamicStringArray;
  end;

  TFileSourceCalcChecksumOperationStatistics = record
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
     Operation that calculates checksum of the files.
  }

  { TFileSourceCalcChecksumOperation }

  TFileSourceCalcChecksumOperation = class(TFileSourceOperation)

  private
    FStatistics: TFileSourceCalcChecksumOperationStatistics;
    FStatisticsAtStartTime: TFileSourceCalcChecksumOperationStatistics;
    FStatisticsLock: TCriticalSection;             //<en For synchronizing statistics.
    FFileSource: IFileSource;
    FFiles: TFiles;
    FMode: TCalcCheckSumOperationMode;
    FTargetPath: String;
    FTargetMask: String;
    FAlgorithm: THashAlgorithm;
    FOneFile, FSeparateFolder: Boolean;
    FOpenFileAfterOperationCompleted: Boolean;

  protected
    FResult: TVerifyChecksumResult;
    FTextLineBreakStyle: TTextLineBreakStyle;

    function GetID: TFileSourceOperationType; override;

    procedure UpdateStatistics(var NewStatistics: TFileSourceCalcChecksumOperationStatistics);
    procedure UpdateStatisticsAtStartTime; override;
    procedure DoReloadFileSources; override;

    property FileSource: IFileSource read FFileSource;
    property Files: TFiles read FFiles;
    property TargetPath: String read FTargetPath;
    property TargetMask: String read FTargetMask;

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFiles: TFiles;
                       aTargetPath: String;
                       aTargetMask: String); virtual reintroduce;

    destructor Destroy; override;

    function GetDescription(Details: TFileSourceOperationDescriptionDetails): String; override;
    function RetrieveStatistics: TFileSourceCalcChecksumOperationStatistics;

    property Mode: TCalcCheckSumOperationMode read FMode write FMode;
    property Algorithm: THashAlgorithm read FAlgorithm write FAlgorithm;
    property OneFile: Boolean read FOneFile write FOneFile;
    property SeparateFolder: Boolean read FSeparateFolder write FSeparateFolder;
    property TextLineBreakStyle: TTextLineBreakStyle read FTextLineBreakStyle write FTextLineBreakStyle;
    property OpenFileAfterOperationCompleted: Boolean  read FOpenFileAfterOperationCompleted write FOpenFileAfterOperationCompleted;
    property Result: TVerifyChecksumResult read FResult;
  end;

implementation

uses
  uDCUtils, uLng, uShowForm;

constructor TFileSourceCalcChecksumOperation.Create(
                aTargetFileSource: IFileSource;
                var theFiles: TFiles;
                aTargetPath: String;
                aTargetMask: String);
begin
  with FStatistics do
  begin
    CurrentFile := '';

    TotalFiles := 0;
    DoneFiles := 0;
    TotalBytes := 0;
    DoneBytes := 0;
    BytesPerSecond := 0;
    RemainingTime := 0;
  end;

  FStatisticsLock := TCriticalSection.Create;

  inherited Create(aTargetFileSource);

  FFileSource := aTargetFileSource;
  FFiles := theFiles;
  theFiles := nil;

  FTargetPath := aTargetPath;
  FTargetMask := aTargetMask;
  FMode := checksum_calc;
  FAlgorithm := HASH_MD5;
  FOneFile := False;
  FOpenFileAfterOperationCompleted := FALSE;
end;

destructor TFileSourceCalcChecksumOperation.Destroy;
begin
  inherited Destroy;

  if Assigned(FStatisticsLock) then
    FreeAndNil(FStatisticsLock);
  if Assigned(FFiles) then
    FreeAndNil(FFiles);
end;

function TFileSourceCalcChecksumOperation.GetDescription(Details: TFileSourceOperationDescriptionDetails): String;
begin
  case Mode of
    checksum_calc:
      case Details of
        fsoddJobAndTarget:
        begin
          if Files.Count = 1 then
            Result := Format(rsOperCalculatingCheckSumOf, [Files[0].FullPath])
          else
            Result := Format(rsOperCalculatingCheckSumIn, [Files.Path]);
        end;
        else
          Result := rsOperCalculatingCheckSum;
      end;
    checksum_verify:
      case Details of
        fsoddJobAndTarget:
        begin
          if Files.Count = 1 then
            Result := Format(rsOperVerifyingCheckSumOf, [Files[0].FullPath])
          else
            Result := Format(rsOperVerifyingCheckSumIn, [Files.Path]);
        end;
        else
          Result := rsOperVerifyingCheckSum;
      end;
    else
      Result := inherited GetDescription(Details);
  end;
end;

function TFileSourceCalcChecksumOperation.GetID: TFileSourceOperationType;
begin
  Result := fsoCalcChecksum;
end;

procedure TFileSourceCalcChecksumOperation.UpdateStatistics(
            var NewStatistics: TFileSourceCalcChecksumOperationStatistics);
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
        if TotalFiles <> 0 then
          UpdateProgress(DoneBytes/TotalBytes);
      end;
    end;

    FStatistics := NewStatistics;

  finally
    FStatisticsLock.Release;
  end;
end;

procedure TFileSourceCalcChecksumOperation.UpdateStatisticsAtStartTime;
begin
  FStatisticsLock.Acquire;
  try
    Self.FStatisticsAtStartTime := Self.FStatistics;
  finally
    FStatisticsLock.Release;
  end;
end;

function TFileSourceCalcChecksumOperation.RetrieveStatistics: TFileSourceCalcChecksumOperationStatistics;
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

procedure TFileSourceCalcChecksumOperation.DoReloadFileSources;
begin
  if OneFile AND OpenFileAfterOperationCompleted then ShowViewerByGlob(TargetMask);
end;


end.

