unit uVfsListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uVfsFileSource,
  uFileSource;

type

  TVfsListOperation = class(TFileSourceListOperation)
  private
    FVfsFileSource: TVfsFileSource;
  public
    constructor Create(var aFileSource: TFileSource); reintroduce;
    procedure MainExecute; override;
  end;

implementation

uses
  LCLProc, uOSUtils, uDCUtils, uVfsFile, uFile;

constructor TVfsListOperation.Create(var aFileSource: TFileSource);
begin
  FFiles := TFiles.Create;
  FVfsFileSource := aFileSource as TVfsFileSource;
  inherited Create(aFileSource);
end;

procedure TVfsListOperation.MainExecute;
var
  I : Integer;
  aFile: TVfsFile;
begin
  FFiles.Clear;
  FFiles.Path := IncludeTrailingPathDelimiter(FileSource.CurrentPath);

  with FVfsFileSource do
  for I := 0 to VfsFileList.Count - 1 do
    begin
      aFile := TVfsFile.Create;
      aFile.Name:= VfsFileList.Name[I];
      aFile.Path := FileSource.CurrentPath;
      aFile.ModificationTime:= FileDateToDateTime(mbFileAge(VfsFileList.FileName[I]));
      FFiles.Add(aFile);
    end;
end;

end.

