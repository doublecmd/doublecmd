{
   File name: uUsersGroups.pas
   Date:      2003/07/03
   Author:    Martin Matusu <xmat@volny.cz>

   Copyright (C) 2003

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   in a file called COPYING along with this program; if not, write to
   the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
   02139, USA.
}

{mate}
unit uUsersGroups;

{$mode objfpc}{$H+}

interface

uses
  Classes, uMyUnix;

const
  groupInfo = '/etc/group';
  userInfo = '/etc/passwd';

function uidToStr(uid: Cardinal): String;
function gidToStr(gid: Cardinal): String;

function strToUID(uname: AnsiString): Cardinal;
function strToGID(gname: AnsiString): Cardinal;

procedure getUsrGroups(uid: Cardinal;  List: TStrings);
procedure getUsers(List: TStrings);
procedure getGroups(List: TStrings);

implementation

uses
  SysUtils;
  
function uidToStr(uid: Cardinal): String;
var
  uinfo: PPasswordRecord;
begin
  uinfo:= getpwuid(uid);
  if (uinfo = nil) then
    Result:= ''
  else
    Result:= String(uinfo^.pw_name);
end;

function gidToStr(gid: Cardinal): String;
var
  ginfo: PGroupRecord;
begin
  ginfo:= getgrgid(gid);
  if (ginfo = nil) then
    Result:= ''
  else
    Result:= String(ginfo^.gr_name);
end;

procedure getUsrGroups(uid: Cardinal;  List: TStrings);
var
  groups: TStrings;
  iC,iD: integer;
  sT: string;
begin
  // parse groups records
  groups:= TStringlist.Create;
  try
    List.Clear;

    groups.LoadFromFile(groupInfo);

    for ic:= 0 to (groups.Count - 1) do
      begin
        st:= groups.Strings[ic]; //get one record to parse
        id:= Pos(UIDtoStr(uid), st); //get position of uname
        if ((id<>0) or (uid=0)) then
          begin
            st:= Copy(st, 1, Pos(':',st) - 1);
            List.Append(st);
          end; // if
      end; // for
   finally
     FreeAndNil(groups);
   end;
end;

procedure getGroups(List: TStrings);
begin
  getUsrGroups(0, List);
end;

procedure GetUsers(List: TStrings);
var
  Users: TStrings;
  iC: integer;
  sT: string;
begin
  users:= TStringList.Create;
  try
    users.LoadFromFile(userInfo);

    List.Clear;
    for ic:= 0 to (users.Count - 1) do
      begin
        st:= users.Strings[ic]; //get one record (line)
        st:= copy(st, 1, Pos(':',st) - 1); //extract username
        List.Append(st); //append to the list
      end;
  finally
    FreeAndNil(users);
  end;  
end;

function strToUID(uname: AnsiString): Cardinal;
//Converts username to UID ('root' results to 0)
var
  uinfo: PPasswordRecord;
begin
  uinfo:= getpwnam(PChar(uname));
  if (uinfo = nil) then
    Result:= High(Cardinal)
  else
    Result:= uinfo^.pw_uid;
end;

function strToGID(gname: AnsiString): Cardinal;
var
  ginfo: PGroupRecord;
begin
  ginfo:= getgrnam(PChar(gname));
  if (ginfo = nil) then
    Result:= High(Cardinal)
  else
    Result:= ginfo^.gr_gid;
end;
{/mate}
end.

