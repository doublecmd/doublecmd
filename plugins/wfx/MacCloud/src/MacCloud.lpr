{
   Double Commander
   -------------------------------------------------------------------------
   Cloud Plugin for macOS.

   Copyright (C) 2025 Alexander Koblov (alexx2000@mail.ru)
   Copyright (C) 2025 Rich Chang (rich2014.git@outlook.com)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.

   Notes:
   1. the goal of the plugin is to target macOS completely and pursue the
      ultimate optimization for macOS, rather than being a cross-system plugin.
   2. only in this way can we make the most of macOS facilities:
     (1) OAuth2 with Safari and CFBundleURLSchemes in info.plist
     (2) avoid version hell of libcrypto, openssl, etc.
     (3) minimal external dependencies, json / hash / string encoding / file utils, etc.
     (4) reduce unnecessary size of plugins
}

library MacCloud;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, SysUtils,
  uWFXFunc;

exports
  ExtensionInitialize,
  FsInitW,
  FsFindFirstW,
  FsFindNextW,
  FsFindClose,
  FsGetFileW,
  FsPutFileW,
  FsMkDirW,
  FsDeleteFileW,
  FsRemoveDirW,
  FsRenMovFileW,
  FsExecuteFileW,
  FsGetDefRootName,
  FsGetBackgroundFlags;

end.
