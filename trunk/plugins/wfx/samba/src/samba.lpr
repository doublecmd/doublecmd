library samba;

{$mode objfpc}{$H+}

uses
  Classes, SmbFunc
  { you can add units after this };

exports
  FsInit,
  FsFindFirst,
  FsFindNext,
  FsFindClose,
  FsRenMovFile,
  FsGetFile,
  FsPutFile,
  FsDeleteFile,
  FsMkDir,
  FsRemoveDir,
  FsSetAttr,
  FsSetTime,
  FsGetDefRootName;

begin
end.

