unit uFileSystemListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uFileSource
  ;

type

  { TFileSystemListOperation }

  TFileSystemListOperation = class(TFileSourceListOperation)
  private
    procedure FlatView(const APath: String);
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  DCOSUtils, uFile, uFindEx, uOSUtils, uFileSystemFileSource;

procedure TFileSystemListOperation.FlatView(const APath: String);
var
  AFile: TFile;
  sr: TSearchRecEx;
begin
  try
    if FindFirstEx(APath + '*', 0, sr) = 0 then
    repeat
      CheckOperationState;

      if (sr.Name = '.') or (sr.Name = '..') then Continue;

      if FPS_ISDIR(sr.Attr) then
        FlatView(APath + sr.Name + DirectorySeparator)
      else begin
        AFile := TFileSystemFileSource.CreateFile(APath, @sr);
        FFiles.Add(AFile);
      end;
    until FindNextEx(sr) <> 0;
  finally
    FindCloseEx(sr);
  end;
end;

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

  if FFlatView then
  begin
    FlatView(Path);
    Exit;
  end;

  IsRootPath := FileSource.IsPathAtRoot(Path);

  Found := FindFirstEx(FFiles.Path + '*', 0, sr) = 0;
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

