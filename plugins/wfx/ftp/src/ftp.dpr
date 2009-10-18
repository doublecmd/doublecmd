library ftp;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

uses
  Classes, FtpFunc, FtpUtils;

exports
  FsInit,
  FsFindFirst,
  FsFindNext,
  FsFindClose,
  FsExecuteFile,
  FsRenMovFile,
  FsGetFile,
  FsPutFile,
  FsDeleteFile,
  FsMkDir,
  FsRemoveDir,
  FsDisconnect,
  FsGetDefRootName,
  FsSetDefaultParams,
  SetDlgProc;

{$IFDEF WINDOWS}{$R ftp.rc}{$ENDIF}

begin
end.

