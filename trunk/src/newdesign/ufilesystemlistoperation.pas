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
  uFileSystemFile, uFindEx, uOSUtils;

constructor TFileSystemListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFileSystemFiles.Create;
  inherited Create(aFileSource, aPath);
end;

procedure TFileSystemListOperation.MainExecute;
var
  AFile: TFileSystemFile;
  sr: TSearchRec;
  IsRootPath: Boolean;
begin
  FFiles.Clear;
  FFiles.Path := Path;

  IsRootPath := FileSource.IsPathAtRoot(Path);

  if FindFirstEx(FFiles.Path + '*', faAnyFile, sr) <> 0 then
  begin
    { No files have been found. }
    FindCloseEx(sr);

    if not IsRootPath then
    begin
      AFile := TFileSystemFile.Create;
      AFile.Path := Path;
      AFile.Name := '..';
      AFile.Attributes := faFolder;
      FFiles.Add(AFile);
    end;

    Exit;
  end;

  repeat
    if sr.Name='.' then Continue;

    // Don't include '..' in the root directory.
    if (sr.Name='..') and IsRootPath then
      Continue;

    AFile := TFileSystemFile.Create(sr);
    AFile.Path := Path;
    FFiles.Add(AFile);

  until FindNextEx(sr)<>0;
  FindCloseEx(sr);
end;

end.

