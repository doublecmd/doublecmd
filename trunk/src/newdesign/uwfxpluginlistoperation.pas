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
  protected
    function UpdateProgress(SourceName, TargetName: UTF8String; PercentDone: Integer): Integer;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  LCLProc, FileUtil, uOSUtils, uDCUtils, uWfxPluginFile,
  WfxPlugin, uWfxModule, uLog, uLng;

function TWfxPluginListOperation.UpdateProgress(SourceName, TargetName: UTF8String;
                                                PercentDone: Integer): Integer;
begin
  logWrite(rsMsgLoadingFileList + IntToStr(PercentDone) + '%', lmtInfo, False, False);
end;

constructor TWfxPluginListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TWfxPluginFiles.Create(aPath);
  FWfxPluginFileSource := aFileSource as IWfxPluginFileSource;
  with FWfxPluginFileSource do
  FCallbackDataClass:= TCallbackDataClass(WfxOperationList.Objects[PluginNumber]);
  FCurrentPath:= ExcludeBackPathDelimiter(aPath);
  inherited Create(aFileSource, aPath);
end;

destructor TWfxPluginListOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TWfxPluginListOperation.Initialize;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(FCurrentPath, FS_STATUS_START, FS_STATUS_OP_LIST);
    FCallbackDataClass.UpdateProgressFunction:= @UpdateProgress;
  end;
end;

procedure TWfxPluginListOperation.MainExecute;
var
  FindData : TWfxFindData;
  Handle: THandle;
  aFile: TWfxPluginFile;
begin
  with FWfxPluginFileSource.WFXModule do
  begin
    FFiles.Clear;

    if not FileSource.IsPathAtRoot(Path) then
    begin
      aFile := TWfxPluginFile.Create(Path);
      aFile.Name := '..';
      aFile.Attributes := faFolder;
      FFiles.Add(aFile);
    end;

    Handle := WfxFindFirst(FCurrentPath, FindData);
    if Handle = wfxInvalidHandle then Exit;
    repeat
      if (FindData.FileName = '.') or (FindData.FileName = '..') then Continue;

      aFile := TWfxPluginFile.Create(Path, FindData);
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
    FCallbackDataClass.UpdateProgressFunction:= nil;
  end;
end;

end.

