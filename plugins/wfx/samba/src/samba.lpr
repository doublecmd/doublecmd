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
  FsDeleteFile,
  FsMkDir,
  FsRemoveDir,
  FsGetDefRootName;

begin
end.

