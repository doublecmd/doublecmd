unit uShellCreateDirectoryOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCreateDirectoryOperation,
  uShellFileSource,
  uFileSource;

type

  { TShellCreateDirectoryOperation }

  TShellCreateDirectoryOperation = class(TFileSourceCreateDirectoryOperation)
  private
    FShellFileSource: IShellFileSource;
  public
    constructor Create(aTargetFileSource: IFileSource;
                       aCurrentPath: String;
                       aDirectoryPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  uFileSourceOperationUI, uGlobs, uLog, uLng;

{ TShellCreateDirectoryOperation }

constructor TShellCreateDirectoryOperation.Create(aTargetFileSource: IFileSource;
  aCurrentPath: String; aDirectoryPath: String);
begin
  FShellFileSource := aTargetFileSource as IShellFileSource;
  inherited Create(aTargetFileSource, aCurrentPath, aDirectoryPath);
end;

procedure TShellCreateDirectoryOperation.MainExecute;
begin
  if FShellFileSource.CreateDirectory(AbsolutePath) then
  begin
    if (log_dir_op in gLogOptions) and (log_success in gLogOptions) then
      logWrite(Thread, Format(rsMsgLogSuccess + rsMsgLogMkDir, [AbsolutePath]), lmtSuccess);
  end
  else begin
    if (log_dir_op in gLogOptions) and (log_errors in gLogOptions) then
      logWrite(Thread, Format(rsMsgLogError + rsMsgLogMkDir, [AbsolutePath]), lmtError);

    AskQuestion(Format(rsMsgErrForceDir, [AbsolutePath]), '', [fsourOk], fsourOk, fsourOk);
  end;
end;

end.

