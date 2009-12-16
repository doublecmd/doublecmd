unit uWfxPluginListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uWfxPluginFileSource,
  uWfxPluginUtil,
  uFileSource;

type

  { TWfxPluginListOperation }

  TWfxPluginListOperation = class(TFileSourceListOperation)
  private
    FWfxPluginFileSource: IWfxPluginFileSource;
    FCallbackDataClass: TCallbackDataClass;
    FCurrentPath: UTF8String;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  LCLProc, FileUtil, uOSUtils, uDCUtils, uWfxPluginFile, uFile,
  WfxPlugin, uWfxModule;

constructor TWfxPluginListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TWfxPluginFiles.Create;
  FWfxPluginFileSource := aFileSource as IWfxPluginFileSource;
  FCallbackDataClass:= TCallbackDataClass.Create;
  FCurrentPath:= ExcludeBackPathDelimiter(aPath);
  inherited Create(aFileSource, aPath);
end;

destructor TWfxPluginListOperation.Destroy;
begin
  if Assigned(FCallbackDataClass) then
    FreeAndNil(FCallbackDataClass);
  inherited Destroy;
end;

procedure TWfxPluginListOperation.Initialize;
begin
  FCallbackDataClass.FileSource:= FWfxPluginFileSource;
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(FCurrentPath, FS_STATUS_START, FS_STATUS_OP_LIST);
    WfxOperationList.Objects[PluginNumber]:= FCallbackDataClass;
  end;
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

    Handle := WfxFindFirst(FCurrentPath, FindData);
    if Handle = wfxInvalidHandle then Exit;
    repeat
      if (FindData.FileName = '.') or (FindData.FileName = '..') then Continue;

      aFile := TWfxPluginFile.Create(FindData);
      aFile.Path := sPath;
      FFiles.Add(aFile);
    until (not WfxFindNext(Handle, FindData));

    FsFindClose(Handle);

  end; // with
end;

procedure TWfxPluginListOperation.Finalize;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(FCurrentPath, FS_STATUS_END, FS_STATUS_OP_LIST);
    WfxOperationList.Objects[PluginNumber]:= nil;
  end;
end;

end.

