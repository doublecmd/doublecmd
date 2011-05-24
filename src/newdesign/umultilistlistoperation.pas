unit uMultiListListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uFileSource,
  uMultiListFileSource;

type

  TMultiListListOperation = class(TFileSourceListOperation)
  private
    FFileSource: IMultiListFileSource;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  uOSUtils, uDCUtils, uFile, uFileProperty;

constructor TMultiListListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FFileSource := aFileSource as IMultiListFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TMultiListListOperation.MainExecute;
var
  AFile: TFile;
  IsRootPath: Boolean;
  CurrentNode: TFileTreeNode;
  CurrentPath: String;
  Found: Boolean;
  i: Integer;
begin
  FFiles.Clear;

  IsRootPath  := FileSource.IsPathAtRoot(Path);
  CurrentNode := FFileSource.FileList;
  CurrentPath := FileSource.GetRootDir;

  // Search for files in the given path.
  while (Path <> CurrentPath) and IsInPath(CurrentPath, Path, True, False) do
  begin
    Found := False;
    for i := 0 to CurrentNode.SubNodesCount - 1 do
    begin
      if IsInPath(IncludeTrailingPathDelimiter(CurrentPath) +
                  CurrentNode.SubNodes[i].TheFile.Name,
                  Path, True, False) then
      begin
        CurrentNode := CurrentNode.SubNodes[i];
        Found := True;
        Break;
      end;
    end;
    if not Found then
      Break;
  end;

  if not IsRootPath then
  begin
    AFile := FileSource.CreateFileObject(Path);
    AFile.Name := '..';
    if fpAttributes in AFile.SupportedProperties then
      AFile.Attributes := faFolder;
    FFiles.Add(AFile);
  end;

  if Path = CurrentPath then
  begin
    for i := 0 to CurrentNode.SubNodesCount - 1 do
    begin
      AFile := CurrentNode.SubNodes[i].TheFile;
      FFiles.Add(AFile);
    end;
  end;
end;

end.


