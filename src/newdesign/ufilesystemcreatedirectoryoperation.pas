unit uFileSystemCreateDirectoryOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceCreateDirectoryOperation,
  uFileSource, uFileSystemFileSource;

type

  TFileSystemCreateDirectoryOperation = class(TFileSourceCreateDirectoryOperation)
  private
    FFileSystemFileSource: IFileSystemFileSource;
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
  uFileSourceOperationUI, uFileProcs, uLog, uLng, uGlobs, uOSUtils;

constructor TFileSystemCreateDirectoryOperation.Create(
                aTargetFileSource: IFileSource;
                aCurrentPath: String;
                aDirectoryPath: String);
begin
  FFileSystemFileSource := aTargetFileSource as IFileSystemFileSource;
  inherited Create(aTargetFileSource, aCurrentPath, aDirectoryPath);
end;

procedure TFileSystemCreateDirectoryOperation.Initialize;
begin
end;

procedure TFileSystemCreateDirectoryOperation.MainExecute;
begin
  if mbFileGetAttr(AbsolutePath) <> faInvalidAttributes then
  begin
    AskQuestion(Format(rsMsgErrDirExists, [AbsolutePath]), '', [fsourOk], fsourOk, fsourOk);
  end
  else if uFileProcs.mbForceDirectory(AbsolutePath) = False then
  begin
    if (log_dir_op in gLogOptions) and (log_errors in gLogOptions) then
      logWrite(Thread, Format(rsMsgLogError+rsMsgLogMkDir, [AbsolutePath]), lmtError);

    AskQuestion(Format(rsMsgErrForceDir, [AbsolutePath]), '', [fsourOk], fsourOk, fsourOk);
  end
  else
  begin
    if (log_dir_op in gLogOptions) and (log_success in gLogOptions) then
      logWrite(Thread, Format(rsMsgLogSuccess+rsMsgLogMkDir,[AbsolutePath]), lmtSuccess);
  end;
end;

procedure TFileSystemCreateDirectoryOperation.Finalize;
begin
end;

end.

