unit uWfxPluginExecuteOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uFileSource,
  uFileSourceExecuteOperation,
  uWfxPluginFileSource;

type

  { TWfxPluginExecuteOperation }

  TWfxPluginExecuteOperation = class(TFileSourceExecuteOperation)
  private
    FWfxPluginFileSource: IWfxPluginFileSource;
  public
    {en
       @param(aTargetFileSource
              File source where the file should be executed.)
       @param(aExecutableFile
              File that should be executed.)
       @param(aCurrentPath
              Path of the file source where the execution should take place.)
    }
    constructor Create(aTargetFileSource: IFileSource;
                       var aExecutableFile: TFile;
                       aCurrentPath,
                       aVerb: String); override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  Forms, WfxPlugin;

constructor TWfxPluginExecuteOperation.Create(
                aTargetFileSource: IFileSource;
                var aExecutableFile: TFile;
                aCurrentPath,
                aVerb: String);
begin
  FWfxPluginFileSource := aTargetFileSource as IWfxPluginFileSource;
  inherited Create(aTargetFileSource, aExecutableFile, aCurrentPath, aVerb);
end;

procedure TWfxPluginExecuteOperation.Initialize;
begin
  with FWfxPluginFileSource do
  WfxModule.WfxStatusInfo(CurrentPath, FS_STATUS_START, FS_STATUS_OP_EXEC);
end;

procedure TWfxPluginExecuteOperation.MainExecute;
var
  RemoteName: String;
  iResult: LongInt;
begin
    if Pos('quote ', Verb) = 1 then
      RemoteName:= CurrentPath
    else begin
      RemoteName:= AbsolutePath;
    end;
    iResult:= FWfxPluginFileSource.WfxModule.WfxExecuteFile(Application.MainForm.Tag, RemoteName, Verb);
    case iResult of
    FS_EXEC_OK:
      FExecuteOperationResult:= fseorSuccess;
    FS_EXEC_ERROR:
      FExecuteOperationResult:= fseorError;
    FS_EXEC_YOURSELF:
      FExecuteOperationResult:= fseorYourSelf;
    FS_EXEC_SYMLINK:
      begin
        FResultString:= RemoteName;
        FExecuteOperationResult:= fseorSymLink;
      end;
    end;
end;

procedure TWfxPluginExecuteOperation.Finalize;
begin
  with FWfxPluginFileSource do
  WfxModule.WfxStatusInfo(CurrentPath, FS_STATUS_END, FS_STATUS_OP_EXEC);
end;

end.

