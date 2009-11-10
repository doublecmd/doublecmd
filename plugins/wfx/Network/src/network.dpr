library network;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

uses
  Classes, NetFunc;

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
  FsSetAttr,
  FsSetTime,
  FsLinksToLocalFiles,
  FsGetLocalName,
  FsGetDefRootName;

begin
end.

