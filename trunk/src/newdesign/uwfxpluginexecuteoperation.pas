unit uWfxPluginExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource,
  uFileSourceExecuteOperation,
  uWfxPluginFileSource;

type

  { TWfxPluginExecuteOperation }

  TWfxPluginExecuteOperation = class(TFileSourceExecuteOperation)
  private
    FWfxPluginFileSource: TWfxPluginFileSource;
  public
    {en
       @param(aTargetFileSource
              File source where the directory should be created.
              Class takes ownership of the pointer.)
       @param(aExecutablePath
              Absolute or relative (to TargetFileSource.CurrentPath) path
              to a executable that should be executed.
    }
    constructor Create(var aTargetFileSource: TFileSource;
                       aExecutablePath, aVerb: UTF8String); override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  ufsplugin;

constructor TWfxPluginExecuteOperation.Create(
                var aTargetFileSource: TFileSource;
                aExecutablePath, aVerb: UTF8String);
begin
  FWfxPluginFileSource := aTargetFileSource as TWfxPluginFileSource;
  inherited Create(aTargetFileSource, aExecutablePath, aVerb);
end;

procedure TWfxPluginExecuteOperation.Initialize;
begin
  with FWfxPluginFileSource do
  WfxStatusInfo(CurrentPath, FS_STATUS_START, FS_STATUS_OP_EXEC);
end;

procedure TWfxPluginExecuteOperation.MainExecute;
var
  iResult: LongInt;
begin
    iResult:= FWfxPluginFileSource.WfxExecuteFile(AbsolutePath, Verb, FExecutablePath);
    case iResult of
    FS_EXEC_OK:
      FExecuteOperationResult:= fseorSuccess;
    FS_EXEC_ERROR:
      FExecuteOperationResult:= fseorError;
    FS_EXEC_YOURSELF:
      FExecuteOperationResult:= fseorYourSelf;
    FS_EXEC_SYMLINK:
      FExecuteOperationResult:= fseorSymLink;
    end;
end;

procedure TWfxPluginExecuteOperation.Finalize;
begin
  with FWfxPluginFileSource do
  WfxStatusInfo(CurrentPath, FS_STATUS_END, FS_STATUS_OP_EXEC);
end;

end.

