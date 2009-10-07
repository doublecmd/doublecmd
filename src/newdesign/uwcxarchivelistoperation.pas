unit uWcxArchiveListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uWcxArchiveFileSource,
  uFileSource;

type

  TWcxArchiveListOperation = class(TFileSourceListOperation)
  private
    FWcxArchiveFileSource: TWcxArchiveFileSource;
  public
    constructor Create(var aFileSource: TFileSource); reintroduce;
    procedure MainExecute; override;
  end;

implementation

uses
  LCLProc, uOSUtils, uDCUtils, uWcxArchiveFile, uFile, uWCXmodule;

constructor TWcxArchiveListOperation.Create(var aFileSource: TFileSource);
begin
  FFiles := TFiles.Create;
  FWcxArchiveFileSource := aFileSource as TWcxArchiveFileSource;
  inherited Create(aFileSource);
end;

procedure TWcxArchiveListOperation.MainExecute;
var
  I : Integer;
  CurrFileName : String;  // Current file name
  ArcFileList: TList;
  aFile: TWcxArchiveFile;
begin
  FFiles.Clear;
  FFiles.Path := IncludeTrailingPathDelimiter(FileSource.CurrentPath);

  if not FileSource.IsAtRootPath then
  begin
    aFile := TWcxArchiveFile.Create;
    aFile.Path := FileSource.CurrentPath;
    aFile.Name := '..';
    aFile.Attributes := faFolder;
    FFiles.Add(AFile);
  end;

  ArcFileList := FWcxArchiveFileSource.ArchiveFileList;
  for I := 0 to ArcFileList.Count - 1 do
    begin
      CurrFileName := PathDelim + TWCXHeader(ArcFileList.Items[I]).FileName;

      if not IsInPath(FileSource.CurrentPath, CurrFileName, False) then
        Continue;

      aFile := TWcxArchiveFile.Create(TWCXHeader(ArcFileList.Items[I]));
      aFile.Path := FileSource.CurrentPath;
      FFiles.Add(AFile);
    end;
end;

end.

