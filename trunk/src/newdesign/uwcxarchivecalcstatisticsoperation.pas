unit uWcxArchiveCalcStatisticsOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCalcStatisticsOperation,
  uFileSource,
  uWcxArchiveFileSource,
  uFile;

type

  TWcxArchiveCalcStatisticsOperation = class(TFileSourceCalcStatisticsOperation)

  private
    FWcxArchiveFileSource: IWcxArchiveFileSource;
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
  uOSUtils, uWcxModule, uDCUtils;

constructor TWcxArchiveCalcStatisticsOperation.Create(
                aTargetFileSource: IFileSource;
                var theFiles: TFiles);
begin
  inherited Create(aTargetFileSource, theFiles);
  FWcxArchiveFileSource:= aTargetFileSource as IWcxArchiveFileSource;
end;

destructor TWcxArchiveCalcStatisticsOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TWcxArchiveCalcStatisticsOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;
end;

procedure TWcxArchiveCalcStatisticsOperation.MainExecute;
var
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := 0 to Files.Count - 1 do
  begin
    ProcessFile(Files[CurrentFileIndex]);
    CheckOperationState;
  end;
end;

procedure TWcxArchiveCalcStatisticsOperation.ProcessFile(aFile: TFile);
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

procedure TWcxArchiveCalcStatisticsOperation.ProcessSubDirs(const srcPath: String);
var
  I: Integer;
  Header: TWCXHeader;
  CurrFileName: UTF8String;
  ModificationTime: TDateTime;
begin
  for I:= 0 to FWcxArchiveFileSource.ArchiveFileList.Count - 1 do
  begin
    Header := TWCXHeader(FWcxArchiveFileSource.ArchiveFileList.Items[I]);
    CurrFileName := PathDelim + Header.FileName;

    if not IsInPath(srcPath, CurrFileName, True, False) then
       Continue;

    if FPS_ISDIR(Header.FileAttr) then
      Inc(FStatistics.Directories)
    else if FPS_ISLNK(Header.FileAttr) then
      Inc(FStatistics.Links)
    else
      begin
        Inc(FStatistics.Files);
        FStatistics.Size := FStatistics.Size + Header.UnpSize;
        ModificationTime:= WcxFileTimeToDateTime(Header);
        if ModificationTime < FStatistics.OldestFile then
          FStatistics.OldestFile := ModificationTime;
        if ModificationTime > FStatistics.NewestFile then
          FStatistics.NewestFile := ModificationTime;
    end;
  end;
end;

end.
