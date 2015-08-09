library ftp;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

uses
  Classes, FtpFunc, FtpUtils, FtpConfDlg
  , ssl_openssl
{$IF DEFINED(UNIX)}
  , ssl_openssl_ver
{$ENDIF}
{$IF DEFINED(LINUX)}
  , ssl_gnutls_lib
{$ELSEIF DEFINED(MSWINDOWS)}
  , ssl_winssl_lib
{$ENDIF}
  ;

{$IF DEFINED(LINUX)}
{$I ssl_gnutls_lib.inc}
{$ELSEIF DEFINED(MSWINDOWS)}
{$I ssl_winssl_lib.inc}
{$ENDIF}

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

{$R *.res}

begin
end.

