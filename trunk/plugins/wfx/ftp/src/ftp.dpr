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
  {
  FsNetworkGetSupportedProtocols,
  FsNetworkGetConnection,
  FsNetworkManageConnection,
  FsNetworkOpenConnection,
  }
  ExtensionInitialize;

begin
end.

