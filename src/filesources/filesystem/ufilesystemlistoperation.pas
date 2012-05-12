unit uFileSystemListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uFileSource
  ;

type

  TFileSystemListOperation = class(TFileSourceListOperation)
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  uFile, uFindEx, uOSUtils, uFileSystemFileSource;

constructor TFileSystemListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  inherited Create(aFileSource, aPath);
end;

procedure TFileSystemListOperation.MainExecute;
var
  AFile: TFile;
  sr: TSearchRecEx;
  IsRootPath, Found: Boolean;
begin
  FFiles.Clear;

  IsRootPath := FileSource.IsPathAtRoot(Path);

  Found := FindFirstEx(FFiles.Path + '*', faAnyFile, sr) = 0;
  try
    if not Found then
    begin
      { No files have been found. }

      if not IsRootPath then
      begin
        AFile := TFileSystemFileSource.CreateFile(Path);
        AFile.Name := '..';
        AFile.Attributes := faFolder;
        FFiles.Add(AFile);
      end;
    end
    else
    begin
      repeat
        CheckOperationState;

        if sr.Name='.' then Continue;

        // Don't include '..' in the root directory.
        if (sr.Name='..') and IsRootPath then
          Continue;

        AFile := TFileSystemFileSource.CreateFile(Path, @sr);
        FFiles.Add(AFile);
      until FindNextEx(sr)<>0;
    end;
  finally
    FindCloseEx(sr);
  end;
end;

end.

