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
    FWcxArchiveFileSource: IWcxArchiveFileSource;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  uOSUtils, uDCUtils, uWCXmodule, uFile;

constructor TWcxArchiveListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FWcxArchiveFileSource := aFileSource as IWcxArchiveFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TWcxArchiveListOperation.MainExecute;
var
  I : Integer;
  CurrFileName : String;  // Current file name
  ArcFileList: TList;
  aFile: TFile;
begin
  FFiles.Clear;

  if not FileSource.IsPathAtRoot(Path) then
  begin
    aFile := TWcxArchiveFileSource.CreateFile(Path);
    aFile.Name := '..';
    aFile.Attributes := faFolder;
    FFiles.Add(AFile);
  end;

  ArcFileList := FWcxArchiveFileSource.ArchiveFileList;
  for I := 0 to ArcFileList.Count - 1 do
    begin
      CurrFileName := PathDelim + TWCXHeader(ArcFileList.Items[I]).FileName;

      if not IsInPath(Path, CurrFileName, False, False) then
        Continue;

      aFile := TWcxArchiveFileSource.CreateFile(Path, TWCXHeader(ArcFileList.Items[I]));
      FFiles.Add(aFile);
    end;
end;

end.

