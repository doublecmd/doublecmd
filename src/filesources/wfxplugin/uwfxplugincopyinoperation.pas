unit uWfxPluginCopyInOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCopyOperation,
  uFileSource,
  uFileSourceOperation,
  uFileSourceOperationOptionsUI,
  uFile,
  uWfxPluginFileSource,
  uWfxPluginUtil;

type

  { TWfxPluginCopyInOperation }

  TWfxPluginCopyInOperation = class(TFileSourceCopyInOperation)

  private
    FWfxPluginFileSource: IWfxPluginFileSource;
    FOperationHelper: TWfxPluginOperationHelper;
    FCallbackDataClass: TCallbackDataClass;
    FSourceFilesTree: TFileTree;  // source files including all files/dirs in subdirectories
    FStatistics: TFileSourceCopyOperationStatistics; // local copy of statistics
    // Options
    FInfoOperation: LongInt;
    procedure SetNeedsConnection(AValue: Boolean);

  protected
    function UpdateProgress(SourceName, TargetName: PAnsiChar; PercentDone: Integer): Integer;

  public
    constructor Create(aSourceFileSource: IFileSource;
                       aTargetFileSource: IFileSource;
                       var theSourceFiles: TFiles;
                       aTargetPath: String); override;

    destructor Destroy; override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;

    class function GetOptionsUIClass: TFileSourceOperationOptionsUIClass; override;

    property NeedsConnection: Boolean read FNeedsConnection write SetNeedsConnection;

  end;

implementation

uses
  uFileSourceOperationOptions, fWfxPluginCopyMoveOperationOptions, WfxPlugin, uFileSystemUtil;

// -- TWfxPluginCopyInOperation ---------------------------------------------

procedure TWfxPluginCopyInOperation.SetNeedsConnection(AValue: Boolean);
begin
  FNeedsConnection:= AValue;
  if (FNeedsConnection = False) then
    FInfoOperation:= FS_STATUS_OP_PUT_MULTI_THREAD
  else if (SourceFiles.Count > 1) then
    FInfoOperation:= FS_STATUS_OP_PUT_MULTI
  else
    FInfoOperation:= FS_STATUS_OP_PUT_SINGLE;
end;

function TWfxPluginCopyInOperation.UpdateProgress(SourceName,TargetName: PAnsiChar;
                                                  PercentDone: Integer): Integer;
var
  iTemp: Int64;
begin
  Result := 0;

  //DCDebug('SourceName=', SourceName, #32, 'TargetName=', TargetName, #32, 'PercentDone=', IntToStr(PercentDone));

  if State = fsosStopping then  // Cancel operation
    Exit(1);

  with FStatistics do
  begin
    if Assigned(SourceName) then begin
      FStatistics.CurrentFileFrom:= SourceName;
    end;
    if Assigned(TargetName) then begin
      FStatistics.CurrentFileTo:= TargetName;
    end;

    iTemp:= CurrentFileTotalBytes * PercentDone div 100;
    DoneBytes := DoneBytes + (iTemp - CurrentFileDoneBytes);
    CurrentFileDoneBytes:= iTemp;

    UpdateStatistics(FStatistics);
  end;

  if not AppProcessMessages(True) then
    Exit(1);
end;

constructor TWfxPluginCopyInOperation.Create(aSourceFileSource: IFileSource;
                                             aTargetFileSource: IFileSource;
                                             var theSourceFiles: TFiles;
                                             aTargetPath: String);
begin
  FWfxPluginFileSource:= aTargetFileSource as IWfxPluginFileSource;
  with FWfxPluginFileSource do
  FCallbackDataClass:= TCallbackDataClass(WfxOperationList.Objects[PluginNumber]);

  inherited Create(aSourceFileSource, aTargetFileSource, theSourceFiles, aTargetPath);

  SetNeedsConnection(FNeedsConnection);
end;

destructor TWfxPluginCopyInOperation.Destroy;
begin
  inherited Destroy;
end;

procedure TWfxPluginCopyInOperation.Initialize;
var
  TreeBuilder: TFileSystemTreeBuilder;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(TargetPath, FS_STATUS_START, FInfoOperation);
    FCallbackDataClass.UpdateProgressFunction:= @UpdateProgress;
    UpdateProgressFunction:= @UpdateProgress;
  end;
  // Get initialized statistics; then we change only what is needed.
  FStatistics := RetrieveStatistics;

  TreeBuilder := TFileSystemTreeBuilder.Create(@AskQuestion, @CheckOperationState);
  try
    TreeBuilder.SymLinkOption:= fsooslFollow;
    TreeBuilder.BuildFromFiles(SourceFiles);
    FSourceFilesTree := TreeBuilder.ReleaseTree;
    FStatistics.TotalFiles := TreeBuilder.FilesCount;
    FStatistics.TotalBytes := TreeBuilder.FilesSize;
  finally
    FreeAndNil(TreeBuilder);
  end;

  if Assigned(FOperationHelper) then
    FreeAndNil(FOperationHelper);

  FOperationHelper := TWfxPluginOperationHelper.Create(
                        FWfxPluginFileSource,
                        @AskQuestion,
                        @RaiseAbortOperation,
                        @CheckOperationState,
                        @UpdateStatistics,
                        @ShowCompareFilesUI,
                        @ShowCompareFilesUIByFileObject,
                        Thread,
                        wpohmCopyIn,
                        TargetPath);

  FOperationHelper.RenameMask := RenameMask;
  FOperationHelper.FileExistsOption := FileExistsOption;
  FOperationHelper.CopyAttributesOptions := CopyAttributesOptions;

  FOperationHelper.Initialize;
end;

procedure TWfxPluginCopyInOperation.MainExecute;
begin
  FOperationHelper.ProcessTree(FSourceFilesTree, FStatistics);
end;

procedure TWfxPluginCopyInOperation.Finalize;
begin
  with FWfxPluginFileSource do
  begin
    WfxModule.WfxStatusInfo(TargetPath, FS_STATUS_END, FInfoOperation);
    FCallbackDataClass.UpdateProgressFunction:= nil;
    UpdateProgressFunction:= nil;
  end;
  FileExistsOption := FOperationHelper.FileExistsOption;
  FOperationHelper.Free;
end;

class function TWfxPluginCopyInOperation.GetOptionsUIClass: TFileSourceOperationOptionsUIClass;
begin
  Result := TWfxPluginCopyInOperationOptionsUI;
end;

end.

