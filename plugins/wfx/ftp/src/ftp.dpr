library ftp;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  DCConvertEncoding, FPCAdds, Classes, FtpFunc, FtpUtils, FtpConfDlg
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
  FsInitW,
  FsFindFirstW,
  FsFindNextW,
  FsFindClose,
  FsExecuteFileW,
  FsRenMovFileW,
  FsGetFileW,
  FsPutFileW,
  FsDeleteFileW,
  FsMkDirW,
  FsRemoveDirW,
  FsSetTimeW,
  FsDisconnectW,
  FsSetCryptCallbackW,
  FsGetDefRootName,
  FsSetDefaultParams,
  FsStatusInfoW,
  FsGetBackgroundFlags,
  {
  FsNetworkGetSupportedProtocols,
  FsNetworkGetConnection,
  FsNetworkManageConnection,
  FsNetworkOpenConnection,
  }
  ExtensionInitialize;

{$R *.res}

begin
  Randomize;
end.
