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
    FWfxPluginFileSource: TWfxPluginFileSource;
  public
    constructor Create(var aTargetFileSource: TFileSource;
                       aDirectoryPath: String); override;

    procedure Initialize; override;
    procedure MainExecute; override;
    procedure Finalize; override;
  end;

implementation

uses
  uFileSourceOperationUI, uWfxPluginFile, uFileProcs, uLog, uLng, uGlobs, uOSUtils, uWfxModule, ufsplugin;

constructor TWfxPluginCreateDirectoryOperation.Create(
                var aTargetFileSource: TFileSource;
                aDirectoryPath: String);
begin
  FWfxPluginFileSource := aTargetFileSource as TWfxPluginFileSource;
  inherited Create(aTargetFileSource, aDirectoryPath);
end;

procedure TWfxPluginCreateDirectoryOperation.Initialize;
begin
end;

procedure TWfxPluginCreateDirectoryOperation.MainExecute;
begin
  with FWfxPluginFileSource do
  begin
    case WfxMkDir(AbsolutePath) of
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

