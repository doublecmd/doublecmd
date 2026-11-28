{
   Double Commander
   -------------------------------------------------------------------------
   Set file property operation for KDE KMTP file source

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

unit uKmtpSetFilePropertyOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceSetFilePropertyOperation,
  uFileSource,
  uFile,
  uFileProperty,
  uKmtpFileSource;

type

  { TKmtpSetFilePropertyOperation }

  TKmtpSetFilePropertyOperation = class(TFileSourceSetFilePropertyOperation)
  private
    FKmtpFileSource: IKmtpFileSource;
    FFullFilesTree: TFiles;
    FStatistics: TFileSourceSetFilePropertyOperationStatistics;
  protected
    function SetNewProperty(aFile: TFile; aTemplateProperty: TFileProperty): TSetFilePropertyResult; override;
  public
    constructor Create(aTargetFileSource: IFileSource;
                       var theTargetFiles: TFiles;
                       var theNewProperties: TFileProperties); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure MainExecute; override;
  end;

implementation

uses
  uKde;

{ TKmtpSetFilePropertyOperation }

constructor TKmtpSetFilePropertyOperation.Create(aTargetFileSource: IFileSource;
  var theTargetFiles: TFiles; var theNewProperties: TFileProperties);
begin
  FKmtpFileSource := aTargetFileSource as IKmtpFileSource;
  FFullFilesTree := nil;
  inherited Create(aTargetFileSource, theTargetFiles, theNewProperties);
  FSupportedProperties := [fpName];
end;

destructor TKmtpSetFilePropertyOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TKmtpSetFilePropertyOperation.Initialize;
begin
  FStatistics := RetrieveStatistics;
  FFullFilesTree := TargetFiles;
  FStatistics.TotalFiles := FFullFilesTree.Count;
end;

procedure TKmtpSetFilePropertyOperation.MainExecute;
var
  I: Integer;
  aFile, aTemplateFile: TFile;
begin
  for I := 0 to FFullFilesTree.Count - 1 do
  begin
    aFile := FFullFilesTree[I];
    FStatistics.CurrentFile := aFile.FullPath;
    UpdateStatistics(FStatistics);

    if Assigned(TemplateFiles) and (I < TemplateFiles.Count) then
      aTemplateFile := TemplateFiles[I]
    else
      aTemplateFile := nil;

    SetProperties(I, aFile, aTemplateFile);

    FStatistics.DoneFiles := FStatistics.DoneFiles + 1;
    UpdateStatistics(FStatistics);
    CheckOperationState;
  end;
end;

function TKmtpSetFilePropertyOperation.SetNewProperty(aFile: TFile;
  aTemplateProperty: TFileProperty): TSetFilePropertyResult;
var
  DevPath, StorPath, RelPath, NewName: String;
  Level: Integer;
begin
  if aTemplateProperty.GetID <> fpName then
    Exit(sfprSkipped);

  NewName := (aTemplateProperty as TFileNameProperty).Value;
  if NewName = aFile.Name then
    Exit(sfprSkipped);

  if (not FKmtpFileSource.ResolveTarget(aFile.FullPath, DevPath, StorPath, RelPath, Level)) or
     (Level < 2) or (StorPath = '') or (RelPath = '') then
    Exit(sfprError);

  if KmtpSetFileName(StorPath, RelPath, NewName) then
    Result := sfprSuccess
  else
    Result := sfprError;
end;

end.
