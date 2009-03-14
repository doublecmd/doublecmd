{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains specific UNIX functions.

    Copyright (C) 2008-2009  Koblov Alexander (Alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uMyUnix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, UnixUtil;

const
  libc = 'c';
  _PATH_FSTAB = '/etc/fstab';
  _PATH_MOUNTED = '/etc/mtab';

const
  DE_UNKNOWN = 0;
  DE_KDE = 1;
  DE_GNOME = 2;
  DE_XFCE = 3;

type
  PIOFILE = Pointer;
  PFILE = PIOFILE;
  //en Mount entry record
  mntent = record
    mnt_fsname: PChar;   //en< name of mounted file system
    mnt_dir: PChar;      //en< file system path prefix
    mnt_type: PChar;     //en< mount type
    mnt_opts: PChar;     //en< mount options
    mnt_freq: LongInt;   //en< dump frequency in days
    mnt_passno: LongInt; //en< pass number on parallel fsck
  end;
  TMountEntry = mntent;
  PMountEntry = ^TMountEntry;

type
  __uid_t = DWORD;
  __gid_t = DWORD;
  //en Password file entry record
  passwd = record
    pw_name: PChar;   //en< user name
    pw_passwd: PChar; //en< user password
    pw_uid: __uid_t;  //en< user ID
    pw_gid: __gid_t;  //en< group ID
    pw_gecos: PChar;  //en< real name
    pw_dir: PChar;    //en< home directory
    pw_shell: PChar;  //en< shell program
  end;
  TPasswordRecord = passwd;
  PPasswordRecord = ^TPasswordRecord;
  //en Group file entry record
  group = record
    gr_name: PChar;   //en< group name
    gr_passwd: PChar; //en< group password
    gr_gid: __gid_t;  //en< group ID
    gr_mem: ^PChar;   //en< group members
  end;
  TGroupRecord = group;
  PGroupRecord = ^TGroupRecord;

{en
   Opens the file system description file
   @param(filename File system description file)
   @param(mode Type of access)
   @returns(The function returns a file pointer to file system description file)
}
function setmntent(const filename: PChar; const mode: PChar): PFILE; cdecl; external libc name 'setmntent';
{en
   Reads the next line from  the file system description file
   @param(stream File pointer to file system description file)
   @returns(The function returns a pointer to a structure containing the
            broken out fields from a line in the file)
}
function getmntent(stream: PFILE): PMountEntry; cdecl; external libc name 'getmntent';
{en
   Closes the file system description file
   @param(stream File pointer to file system description file)
   @returns(The function always returns 1)
}
function endmntent(stream: PFILE): LongInt; cdecl; external libc name 'endmntent';
{en
   Get password file entry
   @param(uid User ID)
   @returns(The function returns a pointer to a structure containing the broken-out
            fields of the record in the password database that matches the user ID)
}
function getpwuid(uid: __uid_t): PPasswordRecord; cdecl; external libc name 'getpwuid';
{en
   Get password file entry
   @param(name User name)
   @returns(The function returns a pointer to a structure containing the broken-out
            fields of the record in the password database that matches the user name)
}
function getpwnam(const name: PChar): PPasswordRecord; cdecl; external libc name 'getpwnam';
{en
   Get group file entry
   @param(gid Group ID)
   @returns(The function returns a pointer to a structure containing the broken-out
            fields of the record in the group database that matches the group ID)
}
function getgrgid(gid: __gid_t): PGroupRecord; cdecl; external libc name 'getgrgid';
{en
   Get group file entry
   @param(name Group name)
   @returns(The function returns a pointer to a structure containing the broken-out
            fields of the record in the group database that matches the group name)
}
function getgrnam(name: PChar): PGroupRecord; cdecl; external libc name 'getgrnam';

function fpOpenDir(__name: PChar): pDir; cdecl; external libc name 'opendir';
function fpReadDir(__dirp: pDir): pDirent; cdecl; external libc name 'readdir64';
function fpCloseDir(__dirp: pDir): cInt; cdecl; external libc name 'closedir';

function UnixToWinAge(UnixAge: time_t): LongInt;
function LinuxToWinAttr(pFileName: PChar; const srInfo: BaseUnix.Stat): Longint;
function GetDesktopEnvironment: Cardinal;

implementation

function UnixToWinAge(UnixAge: time_t): LongInt;
var
  Y,M,D,hh,mm,ss : word;
begin
  EpochToLocal(UnixAge,y,m,d,hh,mm,ss);
  Result:= DateTimeToFileDate(EncodeDate(y,m,d) + EncodeTime(hh,mm,ss,0));
end;

function LinuxToWinAttr(pFileName: PChar; const srInfo: BaseUnix.Stat): Longint;
begin
  Result:= faArchive;
  if fpS_ISDIR(srInfo.st_mode) then
    Result:= Result or faDirectory;
  if (pFileName[0]='.') and (not (pFileName[1] in [#0,'.']))  then
    Result:= Result or faHidden;
  if (srInfo.st_Mode and S_IWUSR) = 0 Then
     Result:= Result or faReadOnly;
  if fpS_ISSOCK(srInfo.st_mode) or fpS_ISBLK(srInfo.st_mode) or fpS_ISCHR(srInfo.st_mode) or fpS_ISFIFO(srInfo.st_mode) Then
     Result:= Result or faSysFile;
end;

function GetDesktopEnvironment: Cardinal;
var
  DesktopSession: String;
begin
  Result:= DE_UNKNOWN;
  if GetEnvironmentVariable('KDE_FULL_SESSION') <> '' then
    Exit(DE_KDE);
  if GetEnvironmentVariable('GNOME_DESKTOP_SESSION_ID') <> '' then
    Exit(DE_GNOME);
  DesktopSession:= GetEnvironmentVariable('DESKTOP_SESSION');
  DesktopSession:= LowerCase(DesktopSession);
  if Pos('kde', DesktopSession) <> 0 then
    Exit(DE_KDE);
  if Pos('gnome', DesktopSession) <> 0 then
    Exit(DE_GNOME);
  if Pos('xfce', DesktopSession) <> 0 then
    Exit(DE_XFCE);
end;

end.

