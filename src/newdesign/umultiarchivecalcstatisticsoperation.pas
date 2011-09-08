unit uMultiArchiveCalcStatisticsOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCalcStatisticsOperation,
  uFileSource,
  uMultiArchiveFileSource,
  uFile;

type

  TMultiArchiveCalcStatisticsOperation = class(TFileSourceCalcStatisticsOperation)

  private
    FMultiArchiveFileSource: IMultiArchiveFileSource;
    FStatistics: TFileSourceCalcStatisticsOperationStatistics; // local copy of statistics

    procedure ProcessFile(aFile: TFile);
    procedure ProcessSubDirs(const srcPath: String);

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFiles: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
  end;

implementation

uses
  uOSUtils, uLng, uMultiArc, uDCUtils;

constructor TMultiArchiveCalcStatisticsOperation.Create(
                aTargetFileSource: IFileSource;
                var theFiles: TFiles);
begin
  inherited Create(aTargetFileSource, theFiles);
  FMultiArchiveFileSource:= aTargetFileSource as IMultiArchiveFileSource;
end;

destructor TMultiArchiveCalcStatisticsOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TMultiArchiveCalcStatisticsOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
end;

procedure TMultiArchiveCalcStatisticsOperation.MainExecute;
var
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := 0 to Files.Count - 1 do
  begin
    ProcessFile(Files[CurrentFileIndex]);
    CheckOperationState;
  end;
end;

procedure TMultiArchiveCalcStatisticsOperation.ProcessFile(aFile: TFile);
begin
  FStatistics.CurrentFile := aFile.Path + aFile.Name;
  UpdateStatistics(FStatistics);

  if aFile.IsDirectory then
    begin
      Inc(FStatistics.Directories);
      ProcessSubDirs(aFile.Path + aFile.Name + DirectorySeparator);
    end
  else if aFile.IsLink then
    begin
      Inc(FStatistics.Links);
    end
  else
    begin
      // Not always this will be regular file (on Unix can be socket, FIFO, block, char, etc.)
      // Maybe check with: FPS_ISREG() on Unix?

      Inc(FStatistics.Files);
      FStatistics.Size := FStatistics.Size + aFile.Size;
      if aFile.ModificationTime < FStatistics.OldestFile then
        FStatistics.OldestFile := aFile.ModificationTime;
      if aFile.ModificationTime > FStatistics.NewestFile then
        FStatistics.NewestFile := aFile.ModificationTime;
    end;

  UpdateStatistics(FStatistics);
end;

procedure TMultiArchiveCalcStatisticsOperation.ProcessSubDirs(const srcPath: String);
var
  I: Integer;
  ArchiveItem: TArchiveItem;
  CurrFileName: UTF8String;
  ModificationTime: TDateTime;
begin
  for I:= 0 to FMultiArchiveFileSource.ArchiveFileList.Count - 1 do
  begin
    ArchiveItem := TArchiveItem(FMultiArchiveFileSource.ArchiveFileList.Items[I]);
    CurrFileName := PathDelim + ArchiveItem.FileName;

    if not IsInPath(srcPath, CurrFileName, True, False) then
       Continue;

    if FMultiArchiveFileSource.FileIsDirectory(ArchiveItem) then
      Inc(FStatistics.Directories)
    else if FMultiArchiveFileSource.FileIsLink(ArchiveItem) then
      Inc(FStatistics.Links)
    else
      begin
        Inc(FStatistics.Files);
        FStatistics.Size := FStatistics.Size + ArchiveItem.UnpSize;
        try
          with ArchiveItem do
          ModificationTime := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
          if ModificationTime < FStatistics.OldestFile then
            FStatistics.OldestFile := ModificationTime;
          if ModificationTime > FStatistics.NewestFile then
            FStatistics.NewestFile := ModificationTime;
        except
          on EConvertError do;
        end;
    end;
  end;
end;

end.

