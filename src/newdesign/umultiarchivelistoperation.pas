unit uMultiArchiveListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uMultiArchiveFileSource,
  uFileSource;

type

  TMultiArchiveListOperation = class(TFileSourceListOperation)
  private
    FMultiArchiveFileSource: IMultiArchiveFileSource;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  LCLProc, uOSUtils, uDCUtils, uMultiArc, uFile;

constructor TMultiArchiveListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FMultiArchiveFileSource := aFileSource as IMultiArchiveFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TMultiArchiveListOperation.MainExecute;
var
  I : Integer;
  CurrFileName : String;  // Current file name
  ArcFileList: TList;
  aFile: TFile;
begin
  FFiles.Clear;

  if not FileSource.IsPathAtRoot(Path) then
  begin
    aFile := TMultiArchiveFileSource.CreateFile(Path);
    aFile.Name := '..';
    aFile.Attributes := faFolder;
    FFiles.Add(AFile);
  end;

  ArcFileList := FMultiArchiveFileSource.ArchiveFileList;
  for I := 0 to ArcFileList.Count - 1 do
    begin
      CurrFileName := PathDelim + TArchiveItem(ArcFileList.Items[I]).FileName;

      if not IsInPath(Path, CurrFileName, False) then
        Continue;

      aFile := TMultiArchiveFileSource.CreateFile(Path, TArchiveItem(ArcFileList.Items[I]));
      FFiles.Add(AFile);
    end;
end;

end.

