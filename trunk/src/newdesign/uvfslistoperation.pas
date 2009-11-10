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
    FVfsFileSource: IVfsFileSource;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  LCLProc, uOSUtils, uDCUtils, uVfsFile, uFile;

constructor TVfsListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create;
  FVfsFileSource := aFileSource as IVfsFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TVfsListOperation.MainExecute;
var
  I : Integer;
  aFile: TVfsFile;
begin
  FFiles.Clear;
  FFiles.Path := IncludeTrailingPathDelimiter(Path);

  with FVfsFileSource do
  for I := 0 to VfsFileList.Count - 1 do
    begin
      aFile := TVfsFile.Create;
      aFile.Name:= VfsFileList.Name[I];
      aFile.Path := Path;
      //aFile.ModificationTime:= FileDateToDateTime(mbFileAge(VfsFileList.FileName[I]));
      FFiles.Add(aFile);
    end;
end;

end.

