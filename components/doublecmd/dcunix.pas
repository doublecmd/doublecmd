{
   Double commander
   -------------------------------------------------------------------------
   This unit contains Unix specific functions

   Copyright (C) 2015 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit DCUnix;

{$mode objfpc}{$H+}

interface

uses
  InitC, BaseUnix;

{en
   Change owner and group of a file (does not follow symbolic links)
   @param(path Full path to file)
   @param(owner User ID)
   @param(group Group ID)
   @returns(On success, zero is returned. On error, -1 is returned, and errno is set appropriately)
}
function fpLChown(path : pChar; owner : TUid; group : TGid): cInt;

implementation

function lchown(path : pChar; owner : TUid; group : TGid): cInt; cdecl; external clib name 'lchown';

function fpLChown(path: pChar; owner: TUid; group: TGid): cInt;
begin
  Result := lchown(path, owner, group);
  if Result = -1 then fpseterrno(fpgetCerrno);
end;

end.

