unit uWfxPluginCalcStatisticsOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCalcStatisticsOperation,
  uFileSource,
  uWfxPluginFileSource,
  uFile;

type

  { TWfxPluginCalcStatisticsOperation }

  TWfxPluginCalcStatisticsOperation = class(TFileSourceCalcStatisticsOperation)

  private
    FWfxPluginFileSource: IWfxPluginFileSource;
    FStatistics: TFileSourceCalcStatisticsOperationStatistics; // local copy of statistics

    procedure ProcessFile(aFile: TFile);
    procedure ProcessSubDirs(const srcPath: String);

  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFiles: TFiles); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  WfxPlugin, uWfxModule;

constructor TWfxPluginCalcStatisticsOperation.Create(
                aTargetFileSource: IFileSource;
                var theFiles: TFiles);
begin
  inherited Create(aTargetFileSource, theFiles);
  FWfxPluginFileSource:= aTargetFileSource as IWfxPluginFileSource;
end;

destructor TWfxPluginCalcStatisticsOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TWfxPluginCalcStatisticsOperation.Initialize;
begin
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(Files.Path, FS_STATUS_START, FS_STATUS_OP_CALCSIZE);
  end;
end;

procedure TWfxPluginCalcStatisticsOperation.MainExecute;
var
  CurrentFileIndex: Integer;
begin
  for CurrentFileIndex := 0 to Files.Count - 1 do
  begin
    ProcessFile(Files[CurrentFileIndex]);
  end;
end;

procedure TWfxPluginCalcStatisticsOperation.Finalize;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(Files.Path, FS_STATUS_END, FS_STATUS_OP_CALCSIZE);
  end;
end;

procedure TWfxPluginCalcStatisticsOperation.ProcessFile(aFile: TFile);
begin
  FStatistics.CurrentFile := aFile.Path + aFile.Name;
  UpdateStatistics(FStatistics);

  AppProcessMessages;

  CheckOperationState;

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
      Inc(FStatistics.Files);
      FStatistics.Size := FStatistics.Size + aFile.Size;
      if aFile.ModificationTime < FStatistics.OldestFile then
        FStatistics.OldestFile := aFile.ModificationTime;
      if aFile.ModificationTime > FStatistics.NewestFile then
        FStatistics.NewestFile := aFile.ModificationTime;
    end;

  UpdateStatistics(FStatistics);
end;

procedure TWfxPluginCalcStatisticsOperation.ProcessSubDirs(const srcPath: String);
var
  AFile: TFile;
  Handle: THandle;
  FindData: TWfxFindData;
begin
  with FWfxPluginFileSource.WfxModule do
  begin
    Handle := WfxFindFirst(srcPath, FindData);
    if Handle = wfxInvalidHandle then Exit;

    repeat
      if (FindData.FileName = '.') or (FindData.FileName = '..') then Continue;

      AFile := TWfxPluginFileSource.CreateFile(srcPath, FindData);
      try
        ProcessFile(aFile);
      finally
        FreeAndNil(aFile);
      end;

    until not WfxFindNext(Handle, FindData);

    FsFindClose(Handle);
  end;
end;

end.

