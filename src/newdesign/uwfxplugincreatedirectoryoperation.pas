unit uWfxPluginCreateDirectoryOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCreateDirectoryOperation,
  uFileSource, uWfxPluginFileSource;

type

  TWfxPluginCreateDirectoryOperation = class(TFileSourceCreateDirectoryOperation)
  private
    FWfxPluginFileSource: IWfxPluginFileSource;
  public
    constructor Create(aTargetFileSource: IFileSource;
                       aCurrentPath: String;
                       aDirectoryPath: String); override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  uFileSourceOperationUI, uLog, uLng, uGlobs, uWfxModule;

constructor TWfxPluginCreateDirectoryOperation.Create(
                aTargetFileSource: IFileSource;
                aCurrentPath: String;
                aDirectoryPath: String);
begin
  FWfxPluginFileSource := aTargetFileSource as IWfxPluginFileSource;
  inherited Create(aTargetFileSource, aCurrentPath, aDirectoryPath);
end;

procedure TWfxPluginCreateDirectoryOperation.Initialize;
begin
end;

procedure TWfxPluginCreateDirectoryOperation.MainExecute;
begin
  with FWfxPluginFileSource do
  begin
    case WfxModule.WfxMkDir(BasePath, AbsolutePath) of
    WFX_NOTSUPPORTED:
      AskQuestion(rsMsgErrNotSupported, '', [fsourOk], fsourOk, fsourOk);
    WFX_SUCCESS:
      begin
        // write log success
        if (log_vfs_op in gLogOptions) and (log_success in gLogOptions) then
          logWrite(Thread, Format(rsMsgLogSuccess+rsMsgLogMkDir, [AbsolutePath]), lmtSuccess)
      end;
    else
      begin
        // write log error
        if (log_vfs_op in gLogOptions) and (log_errors in gLogOptions) then
          logWrite(Thread, Format(rsMsgLogError+rsMsgLogMkDir, [AbsolutePath]), lmtError);
      end;
    end; // case
  end; // with
end;

procedure TWfxPluginCreateDirectoryOperation.Finalize;
begin
end;

end.

