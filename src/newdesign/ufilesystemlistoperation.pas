unit uFileSystemListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uFileSystemFileSource
  ;

type

  TFileSystemListOperation = class(TFileSourceListOperation)
  private
    FFileSource: TFileSystemFileSource;
  public
    constructor Create(aFileSource: TFileSystemFileSource); reintroduce;
    procedure Execute; override;
  end;

implementation

uses
  LCLProc, uFileSystemFile, uFindEx, uDCUtils;

constructor TFileSystemListOperation.Create(aFileSource: TFileSystemFileSource);
begin
  FFileSource := aFileSource;
  FFiles := TFileSystemFiles.Create;
  inherited Create(aFileSource);
end;

procedure TFileSystemListOperation.Execute;
var
  AFile: TFileSystemFile;
  sr: TSearchRec;
  sParentDir: UTF8String;
  sDir: UTF8String;
  IsRootPath: Boolean;
begin
  FFiles.Clear;
  FFiles.Path := IncludeTrailingPathDelimiter(FFileSource.CurrentPath);

  if FindFirstEx(FFiles.Path + '*', faAnyFile, sr) <> 0 then
  begin
    { No files have been found. }
    FindCloseEx(sr);
    sParentDir := GetParentDir(FFileSource.CurrentPath);
{
    if sParentDir <> EmptyStr then // if parent dir exists then add up level item
	    AddUpLevel(sParentDir, fl);
}
    Exit;
  end;

  sDir := IncludeTrailingPathDelimiter(FFileSource.CurrentPath);
  FFiles.Path := sDir;

  if (sDir = PathDelim) or
     (sDir = ExtractFileDrive(FFileSource.CurrentPath){+PathDelim})
  then
    IsRootPath := True
  else
    IsRootPath := False;

  repeat
    if sr.Name='.' then Continue;
//    if sr.Name='' then Continue;

    // Don't include '..' in the root directory.
    if (sr.Name='..') and IsRootPath then
      Continue;

    AFile := TFileSystemFile.Create(sr);
    AFile.Path := sDir;
    FFiles.Add(AFile);

  until FindNextEx(sr)<>0;
  FindCloseEx(sr);
end;

end.

