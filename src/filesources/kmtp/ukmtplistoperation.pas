{
   Double Commander
   -------------------------------------------------------------------------
   List operation for KDE KMTP file source

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

unit uKmtpListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uFileSource,
  uKmtpFileSource;

type

  { TKmtpListOperation }

  TKmtpListOperation = class(TFileSourceListOperation)
  private
    FKmtpFileSource: IKmtpFileSource;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  BaseUnix, uFile, uKde;

{ TKmtpListOperation }

constructor TKmtpListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FKmtpFileSource := aFileSource as IKmtpFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TKmtpListOperation.MainExecute;
var
  DevPath, StorPath, RelPath: String;
  Level, I, ExistsCode: Integer;
  Devices: TKmtpDeviceArray;
  Storages: TKmtpStorageArray;
  KmtpFiles: TKmtpFileArray;
  AFile: TFile;
begin
  FFiles.Clear;
  if not FKmtpFileSource.ResolveTarget(Path, DevPath, StorPath, RelPath, Level) then
    Exit;

  case Level of
    0:
      begin
        Devices := KmtpListDevices;
        for I := 0 to High(Devices) do
        begin
          CheckOperationState;
          if Devices[I].FriendlyName = '' then
            Continue;
          AFile := TKmtpFileSource.CreateFile(Path);
          AFile.Name := Devices[I].FriendlyName;
          AFile.Size := 0;
          AFile.Attributes := S_IFDIR or $01ED;
          FFiles.Add(AFile);
        end;
      end;
    1:
      begin
        Storages := KmtpListStorages(DevPath);
        for I := 0 to High(Storages) do
        begin
          CheckOperationState;
          AFile := TKmtpFileSource.CreateFile(Path);
          AFile.Name := Storages[I].Description;
          AFile.Size := 0;
          AFile.Attributes := S_IFDIR or $01ED;
          FFiles.Add(AFile);
        end;
      end;
    2:
      begin
        if KmtpGetFilesAndFolders(StorPath, RelPath, KmtpFiles, ExistsCode) and (ExistsCode = 0) then
        begin
          for I := 0 to High(KmtpFiles) do
          begin
            CheckOperationState;
            if (KmtpFiles[I].FileName = '') or (KmtpFiles[I].FileName = '.') or (KmtpFiles[I].FileName = '..') then
              Continue;
            AFile := TKmtpFileSource.CreateFile(Path, KmtpFiles[I]);
            FFiles.Add(AFile);
          end;
        end;
      end;
  end;
end;

end.
