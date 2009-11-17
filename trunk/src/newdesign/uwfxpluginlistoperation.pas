unit uWfxPluginListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uWfxPluginFileSource,
  uFileSource;

type

  TWfxPluginListOperation = class(TFileSourceListOperation)
  private
    FWfxPluginFileSource: IWfxPluginFileSource;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  LCLProc, FileUtil, uOSUtils, uWfxPluginFile, uFile,
  WfxPlugin, uWfxModule;

constructor TWfxPluginListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create;
  FWfxPluginFileSource := aFileSource as IWfxPluginFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TWfxPluginListOperation.MainExecute;
var
  FindData : TWfxFindData;
  Handle: THandle;
  aFile: TWfxPluginFile;
  sPath : UTF8String;
begin
  with FWfxPluginFileSource.WFXModule do
  begin
    sPath:= Path;
    WFXStatusInfo(sPath, FS_STATUS_START, FS_STATUS_OP_LIST);

    FFiles.Clear;
    FFiles.Path := IncludeTrailingPathDelimiter(sPath);

    if not FileSource.IsPathAtRoot(Path) then
    begin
      aFile := TWfxPluginFile.Create;
      aFile.Path := sPath;
      aFile.Name := '..';
      aFile.Attributes := faFolder;
      FFiles.Add(aFile);
    end;

    Handle := WfxFindFirst(sPath, FindData);
    if Handle = feInvalidHandle then Exit;
    repeat
      if (FindData.FileName = '.') or (FindData.FileName = '..') then Continue;

      aFile := TWfxPluginFile.Create(FindData);
      aFile.Path := sPath;
      FFiles.Add(aFile);
    until (not WfxFindNext(Handle, FindData));

    FsFindClose(Handle);

    WFXStatusInfo(sPath, FS_STATUS_END, FS_STATUS_OP_LIST);
  end; // with
end;

end.

