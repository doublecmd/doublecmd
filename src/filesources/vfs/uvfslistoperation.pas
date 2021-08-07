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
  LCLProc, DCFileAttributes, uFile, uVfsModule, uDCUtils;

constructor TVfsListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FVfsFileSource := aFileSource as IVfsFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TVfsListOperation.MainExecute;
var
  I : Integer;
  aFile: TFile;
  APath: String;
  VfsModule: TVfsModule;
begin
  FFiles.Clear;

  with FVfsFileSource do
  for I := 0 to VfsFileList.Count - 1 do
    begin
      CheckOperationState;
      if VfsFileList.Enabled[I] then
      begin
        aFile := TVfsFileSource.CreateFile(Path);
        aFile.Name:= VfsFileList.Name[I];
        aFile.Attributes:= FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_VIRTUAL;
        aFile.LinkProperty.LinkTo:= mbExpandFileName(VfsFileList.FileName[I]);
        FFiles.Add(aFile);
      end;
    end;
  for I:= 0 to gVfsModuleList.Count - 1 do
    begin
      CheckOperationState;
      VfsModule:= TVfsModule(gVfsModuleList.Objects[I]);
      if VfsModule.Visible then
      begin
        aFile := TVfsFileSource.CreateFile(Path);
        aFile.Name:= gVfsModuleList.Strings[I];
        if VfsModule.FileSourceClass.GetMainIcon(APath) then
        begin
          aFile.LinkProperty.LinkTo:= mbExpandFileName(APath);
          aFile.Attributes:= FILE_ATTRIBUTE_TEMPORARY or FILE_ATTRIBUTE_VIRTUAL;
        end;
        FFiles.Add(aFile);
      end;
    end;
end;

end.

