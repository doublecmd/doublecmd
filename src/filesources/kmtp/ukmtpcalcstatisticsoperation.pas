{
   Double Commander
   -------------------------------------------------------------------------
   Calculate statistics operation for KDE KMTP file source

   Copyright (C) 2026 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uKmtpCalcStatisticsOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCalcStatisticsOperation,
  uFileSource,
  uFile,
  uKmtpFileSource;

type

  { TKmtpCalcStatisticsOperation }

  TKmtpCalcStatisticsOperation = class(TFileSourceCalcStatisticsOperation)
  private
    FKmtpFileSource: IKmtpFileSource;
    FStatistics: TFileSourceCalcStatisticsOperationStatistics;
    procedure ProcessFile(aFile: TFile);
    procedure ProcessSubDirs(const srcPath: String);
  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theFiles: TFiles); override;
    procedure Initialize; override;
    procedure MainExecute; override;
  end;

implementation

uses
  BaseUnix, uKde;

{ TKmtpCalcStatisticsOperation }

constructor TKmtpCalcStatisticsOperation.Create(aTargetFileSource: IFileSource;
  var theFiles: TFiles);
begin
  FKmtpFileSource := aTargetFileSource as IKmtpFileSource;
  inherited Create(aTargetFileSource, theFiles);
end;

procedure TKmtpCalcStatisticsOperation.Initialize;
begin
  FStatistics := RetrieveStatistics;
end;

procedure TKmtpCalcStatisticsOperation.MainExecute;
var
  I: Integer;
begin
  for I := 0 to Files.Count - 1 do
  begin
    ProcessFile(Files[I]);
    CheckOperationState;
  end;
end;

procedure TKmtpCalcStatisticsOperation.ProcessFile(aFile: TFile);
begin
  FStatistics.CurrentFile := aFile.FullPath;
  UpdateStatistics(FStatistics);

  if aFile.IsDirectory then
  begin
    Inc(FStatistics.Directories);
    ProcessSubDirs(aFile.FullPath + DirectorySeparator);
  end
  else
  begin
    Inc(FStatistics.Files);
    FStatistics.Size := FStatistics.Size + aFile.Size;
    if aFile.ModificationTime > 0 then
    begin
      if (FStatistics.OldestFile = 0) or (aFile.ModificationTime < FStatistics.OldestFile) then
        FStatistics.OldestFile := aFile.ModificationTime;
      if aFile.ModificationTime > FStatistics.NewestFile then
        FStatistics.NewestFile := aFile.ModificationTime;
    end;
  end;
  UpdateStatistics(FStatistics);
end;

procedure TKmtpCalcStatisticsOperation.ProcessSubDirs(const srcPath: String);
var
  DevPath, StorPath, RelPath: String;
  Level, I, ExistsCode: Integer;
  Storages: TKmtpStorageArray;
  SubFiles: TKmtpFileArray;
  AFile: TFile;
begin
  if not FKmtpFileSource.ResolveTarget(srcPath, DevPath, StorPath, RelPath, Level) then
    Exit;

  if Level = 1 then
  begin
    Storages := KmtpListStorages(DevPath);
    for I := 0 to High(Storages) do
    begin
      CheckOperationState;
      AFile := TKmtpFileSource.CreateFile(srcPath);
      AFile.Name := Storages[I].Description;
      AFile.Attributes := S_IFDIR or $01ED;
      try
        ProcessFile(AFile);
      finally
        AFile.Free;
      end;
    end;
  end
  else if Level >= 2 then
  begin
    if KmtpGetFilesAndFolders(StorPath, RelPath, SubFiles, ExistsCode) and (ExistsCode = 0) then
    begin
      for I := 0 to High(SubFiles) do
      begin
        CheckOperationState;
        if (SubFiles[I].FileName = '') or (SubFiles[I].FileName = '.') or (SubFiles[I].FileName = '..') then
          Continue;
        AFile := TKmtpFileSource.CreateFile(srcPath, SubFiles[I]);
        try
          ProcessFile(AFile);
        finally
          AFile.Free;
        end;
      end;
    end;
  end;
end;

end.
