library ftp;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

uses
  Classes, FtpFunc, FtpUtils, FtpConfDlg;

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
  FsSetCryptCallback,
  FsGetDefRootName,
  FsSetDefaultParams,
  SetDlgProc;

{$IFDEF WINDOWS}{$R ftp.rc}{$ENDIF}

begin
end.

