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
  uFileSystemFile, uFindEx;

constructor TFileSystemListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFileSystemFiles.Create;
  inherited Create(aFileSource, aPath);
end;

procedure TFileSystemListOperation.MainExecute;
var
  AFile: TFileSystemFile;
  sr: TSearchRec;
  sParentDir: UTF8String;
  IsRootPath: Boolean;
begin
  FFiles.Clear;
  FFiles.Path := Path;

  if FindFirstEx(FFiles.Path + '*', faAnyFile, sr) <> 0 then
  begin
    { No files have been found. }
    FindCloseEx(sr);
{
    sParentDir := GetParentDir(FileSource.CurrentPath);
    if sParentDir <> EmptyStr then // if parent dir exists then add up level item
	    AddUpLevel(sParentDir, fl);
}
    Exit;
  end;

  IsRootPath := FileSource.IsPathAtRoot(Path);

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

