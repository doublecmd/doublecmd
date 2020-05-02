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
  DCOSUtils, uOSUtils, DCStrUtils, uWCXmodule, uFile;

constructor TWcxArchiveListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FWcxArchiveFileSource := aFileSource as IWcxArchiveFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TWcxArchiveListOperation.MainExecute;
var
  I : Integer;
  aFile: TFile;
  Header: TWcxHeader;
  ArcFileList: TList;
  CurrFileName : String;  // Current file name
begin
  FFiles.Clear;

  if FWcxArchiveFileSource.Changed then
  begin
    FWcxArchiveFileSource.Reload(Path);
  end;

  if not FileSource.IsPathAtRoot(Path) then
  begin
    aFile := TWcxArchiveFileSource.CreateFile(Path);
    aFile.Name := '..';
    aFile.Attributes := faFolder;
    FFiles.Add(AFile);
  end;

  ArcFileList := FWcxArchiveFileSource.ArchiveFileList.Clone;
  try
    for I := 0 to ArcFileList.Count - 1 do
    begin
      CheckOperationState;

      Header := TWcxHeader(ArcFileList.Items[I]);
      CurrFileName := PathDelim + Header.FileName;

      if not IsInPath(Path, CurrFileName, FFlatView, False) then
        Continue;

      if FFlatView = False then
        aFile := TWcxArchiveFileSource.CreateFile(Path, Header)
      else begin
        if FPS_ISDIR(Header.FileAttr) then Continue;
        aFile := TWcxArchiveFileSource.CreateFile(ExtractFilePath(CurrFileName), Header)
      end;

      FFiles.Add(aFile);
    end;

  finally
    ArcFileList.Free;
  end;
end;

end.

