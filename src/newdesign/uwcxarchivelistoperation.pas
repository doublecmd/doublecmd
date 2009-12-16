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
  LCLProc, uOSUtils, uDCUtils, uWcxArchiveFile, uFile, uWCXmodule;

constructor TWcxArchiveListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TWcxArchiveFiles.Create;
  FWcxArchiveFileSource := aFileSource as IWcxArchiveFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TWcxArchiveListOperation.MainExecute;
var
  I : Integer;
  CurrFileName : String;  // Current file name
  ArcFileList: TList;
  aFile: TWcxArchiveFile;
begin
  FFiles.Clear;
  FFiles.Path := IncludeTrailingPathDelimiter(Path);

  if not FileSource.IsPathAtRoot(Path) then
  begin
    aFile := TWcxArchiveFile.Create;
    aFile.Path := Path;
    aFile.Name := '..';
    aFile.Attributes := faFolder;
    FFiles.Add(AFile);
  end;

  ArcFileList := FWcxArchiveFileSource.ArchiveFileList;
  for I := 0 to ArcFileList.Count - 1 do
    begin
      CurrFileName := PathDelim + TWCXHeader(ArcFileList.Items[I]).FileName;

      if not IsInPath(Path, CurrFileName, False) then
        Continue;

      aFile := TWcxArchiveFile.Create(TWCXHeader(ArcFileList.Items[I]));
      aFile.Path := Path;
      FFiles.Add(AFile);
    end;
end;

end.

