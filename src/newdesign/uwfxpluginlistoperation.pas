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
    FWfxPluginFileSource: TWfxPluginFileSource;
  public
    constructor Create(var aFileSource: TFileSource); reintroduce;
    procedure MainExecute; override;
  end;

implementation

uses
  LCLProc, FileUtil, uOSUtils, uDCUtils, uWfxPluginFile, uFile, uFileAttributes, WfxPlugin;

constructor TWfxPluginListOperation.Create(var aFileSource: TFileSource);
begin
  FFiles := TFiles.Create;
  FWfxPluginFileSource := aFileSource as TWfxPluginFileSource;
  inherited Create(aFileSource);
end;

procedure TWfxPluginListOperation.MainExecute;
var
  FindData : TWIN32FINDDATA;
  Handle: THandle;
  aFile: TWfxPluginFile;
  sPath : UTF8String;
begin
  with FWfxPluginFileSource.WFXModule do
  begin
    sPath:= FileSource.CurrentPath;
    WFXStatusInfo(sPath, FS_STATUS_START, FS_STATUS_OP_LIST);

    FFiles.Clear;
    FFiles.Path := IncludeTrailingPathDelimiter(sPath);

    if not FileSource.IsAtRootPath then
    begin
      aFile := TWfxPluginFile.Create;
      aFile.Path := sPath;
      aFile.Name := '..';
      aFile.Attributes := FILE_ATTRIBUTE_DIRECTORY; // Windows attributes by default
      FFiles.Add(aFile);
    end;

    Handle := FsFindFirst(PChar(UTF8ToSys(sPath)), FindData);
    if Handle = feInvalidHandle then Exit;
    repeat
      if (FindData.cFileName = '.') or (FindData.cFileName = '..') then Continue;

      aFile := TWfxPluginFile.Create(FindData);
      aFile.Path := sPath;
      FFiles.Add(aFile);
    until (not FsFindNext(Handle, FindData));

    FsFindClose(Handle);

    WFXStatusInfo(sPath, FS_STATUS_END, FS_STATUS_OP_LIST);
  end; // with
end;

end.

