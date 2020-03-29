unit uWfxPluginListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uWfxPluginFileSource,
  uFileSource;

type

  { TWfxPluginListOperation }

  TWfxPluginListOperation = class(TFileSourceListOperation)
  private
    FWfxPluginFileSource: IWfxPluginFileSource;
    FCallbackDataClass: TCallbackDataClass;
    FCurrentPath: String;
  protected
    function UpdateProgress(SourceName, TargetName: PAnsiChar; PercentDone: Integer): Integer;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  DCFileAttributes, DCStrUtils, uFile, WfxPlugin, uWfxModule, uLog, uLng;

function TWfxPluginListOperation.UpdateProgress(SourceName, TargetName: PAnsiChar;
                                                PercentDone: Integer): Integer;
begin
  if State = fsosStopping then  // Cancel operation
    Exit(1);

  logWrite(rsMsgLoadingFileList + IntToStr(PercentDone) + '%', lmtInfo, False, False);

  if CheckOperationStateSafe then
    Result := 0
  else begin
    Result := 1;
  end;
end;

constructor TWfxPluginListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
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
    UpdateProgressFunction:= @UpdateProgress;
  end;
end;

procedure TWfxPluginListOperation.MainExecute;
var
  aFile: TFile;
  Handle: THandle;
  FindData : TWfxFindData;
  HaveUpDir: Boolean = False;
begin
  with FWfxPluginFileSource.WFXModule do
  try
    FFiles.Clear;
    Handle := WfxFindFirst(FCurrentPath, FindData);
    if Handle <> wfxInvalidHandle then
    try
      repeat
        CheckOperationState;
        if (FindData.FileName = '.') then Continue;
        if (FindData.FileName = '..') then HaveUpDir:= True;

        aFile := TWfxPluginFileSource.CreateFile(Path, FindData);
        FFiles.Add(aFile);
      until (not WfxFindNext(Handle, FindData));
    finally
      FsFindClose(Handle);
    end;
  finally
    if not HaveUpDir then
    begin
      aFile := TWfxPluginFileSource.CreateFile(Path);
      aFile.Name := '..';
      aFile.Attributes := GENERIC_ATTRIBUTE_FOLDER;
      FFiles.Insert(aFile, 0);
    end;
  end; // with
end;

procedure TWfxPluginListOperation.Finalize;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(FCurrentPath, FS_STATUS_END, FS_STATUS_OP_LIST);
    FCallbackDataClass.UpdateProgressFunction:= nil;
    UpdateProgressFunction:= nil;
  end;
end;

end.

