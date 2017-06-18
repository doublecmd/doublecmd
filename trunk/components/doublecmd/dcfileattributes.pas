{
   Double Commander
   -------------------------------------------------------------------------
   Functions handling file attributes.

   Copyright (C) 2012 Przemysław Nagay (cobines@gmail.com)

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

unit DCFileAttributes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes;

const
  // Windows attributes
  FILE_ATTRIBUTE_READONLY            = $0001;
  FILE_ATTRIBUTE_HIDDEN              = $0002;
  FILE_ATTRIBUTE_SYSTEM              = $0004;
  FILE_ATTRIBUTE_DIRECTORY           = $0010;
  FILE_ATTRIBUTE_ARCHIVE             = $0020;
  FILE_ATTRIBUTE_NORMAL              = $0080;
  FILE_ATTRIBUTE_TEMPORARY           = $0100;
  FILE_ATTRIBUTE_SPARSE_FILE         = $0200;
  FILE_ATTRIBUTE_REPARSE_POINT       = $0400;
  FILE_ATTRIBUTE_COMPRESSED          = $0800;
  FILE_ATTRIBUTE_OFFLINE             = $1000;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $2000;
  FILE_ATTRIBUTE_ENCRYPTED           = $4000;
  FILE_ATTRIBUTE_VIRTUAL             = $20000;

  // Unix attributes
  { attributes mask                      }
  S_IFMT   = $F000;
  { first-in/first-out (FIFO/pipe)       }
  S_IFIFO  = $1000;
  { character-special file (tty/console) }
  S_IFCHR  = $2000;
  { directory                            }
  S_IFDIR  = $4000;
  { blocking device (unused)             }
  S_IFBLK  = $6000;
  { regular                              }
  S_IFREG  = $8000;
  { symbolic link (unused)               }
  S_IFLNK  = $A000;
  { Berkeley socket                      }
  S_IFSOCK = $C000;

  { mode_t possible values }
  S_IRUSR =  %0100000000;     { Read permission for owner   }
  S_IWUSR =  %0010000000;     { Write permission for owner  }
  S_IXUSR =  %0001000000;     { Exec  permission for owner  }
  S_IRGRP =  %0000100000;     { Read permission for group   }
  S_IWGRP =  %0000010000;     { Write permission for group  }
  S_IXGRP =  %0000001000;     { Exec permission for group   }
  S_IROTH =  %0000000100;     { Read permission for world   }
  S_IWOTH =  %0000000010;     { Write permission for world  }
  S_IXOTH =  %0000000001;     { Exec permission for world   }
  S_IRWXU =  S_IRUSR or S_IWUSR or S_IXUSR;
  S_IRWXG =  S_IRGRP or S_IWGRP or S_IXGRP;
  S_IRWXO =  S_IROTH or S_IWOTH or S_IXOTH;

  { POSIX setuid(), setgid(), and sticky bit }
  S_ISUID  = $0800;
  S_ISGID  = $0400;
  S_ISVTX  = $0200;

  function WinToUnixFileAttr(Attr: TFileAttrs): TFileAttrs;
  function UnixToWinFileAttr(Attr: TFileAttrs): TFileAttrs;
  function UnixToWinFileAttr(const FileName: String; Attr: TFileAttrs): TFileAttrs;

  function SingleStrToFileAttr(sAttr: String): TFileAttrs;
  function WinSingleStrToFileAttr(sAttr: String): TFileAttrs;
  function UnixSingleStrToFileAttr(sAttr: String): TFileAttrs;

  {en
     Convert file attributes from string to number
     @param(Attributes File attributes as string)
     @returns(File attributes as number)
  }
  function StrToFileAttr(sAttr: String): TFileAttrs;
  {en
     Convert file attributes to string in the format of "attr1+attr2+attr3+".
     @param(Attributes File attributes)
     @returns(File attributes as string)
  }
  function FileAttrToStr(Attr: TFileAttrs): String;
  {en
       Convert Windows file attributes from string to number
       @param(Attributes File attributes as string)
       @returns(File attributes as number)
    }
  function WinStrToFileAttr(sAttr: String): TFileAttrs;
  {en
       Convert Unix file attributes from string to number
       @param(Attributes File attributes as string)
       @returns(File attributes as number)
    }
  function UnixStrToFileAttr(sAttr: String): TFileAttrs;

  function FormatNtfsAttributes(iAttr: TFileAttrs): String;
  function FormatUnixAttributes(iAttr: TFileAttrs): String;

implementation

uses
  DCStrUtils;

type
  TAttrStrToFileAttr = record
    Str: String;
    Attr: TFileAttrs;
  end;

const
  WinAttrStrToFileAttr: array[0..9] of TAttrStrToFileAttr = (
      (Str: 'r'; Attr: FILE_ATTRIBUTE_READONLY),
      (Str: 'h'; Attr: FILE_ATTRIBUTE_HIDDEN),
      (Str: 's'; Attr: FILE_ATTRIBUTE_SYSTEM),
      (Str: 'd'; Attr: FILE_ATTRIBUTE_DIRECTORY),
      (Str: 'a'; Attr: FILE_ATTRIBUTE_ARCHIVE),
      (Str: 't'; Attr: FILE_ATTRIBUTE_TEMPORARY),
      (Str: 'p'; Attr: FILE_ATTRIBUTE_SPARSE_FILE),
      (Str: 'l'; Attr: FILE_ATTRIBUTE_REPARSE_POINT),
      (Str: 'c'; Attr: FILE_ATTRIBUTE_COMPRESSED),
      (Str: 'e'; Attr: FILE_ATTRIBUTE_ENCRYPTED));

  UnixAttrStrToFileAttr: array[0..18] of TAttrStrToFileAttr = (
      // Permissions
      (Str: 'ur'; Attr: S_IRUSR),
      (Str: 'uw'; Attr: S_IWUSR),
      (Str: 'ux'; Attr: S_IXUSR),
      (Str: 'gr'; Attr: S_IRGRP),
      (Str: 'gw'; Attr: S_IWGRP),
      (Str: 'gx'; Attr: S_IXGRP),
      (Str: 'or'; Attr: S_IROTH),
      (Str: 'ow'; Attr: S_IWOTH),
      (Str: 'ox'; Attr: S_IXOTH),
      (Str: 'us'; Attr: S_ISUID),
      (Str: 'gs'; Attr: S_ISGID),
      (Str: 'sb'; Attr: S_ISVTX),
      // File types
      (Str: 'f'; Attr: S_IFIFO),
      (Str: 'c'; Attr: S_IFCHR),
      (Str: 'd'; Attr: S_IFDIR),
      (Str: 'b'; Attr: S_IFBLK),
      (Str: 'r'; Attr: S_IFREG),
      (Str: 'l'; Attr: S_IFLNK),
      (Str: 's'; Attr: S_IFSOCK));

function WinToUnixFileAttr(Attr: TFileAttrs): TFileAttrs;
begin
  Result := S_IRUSR or S_IRGRP or S_IROTH;

  if (Attr and faReadOnly) = 0 then
    Result := Result or S_IWUSR;

  if (Attr and faDirectory) <> 0 then
    Result := Result or S_IFDIR
  else
    Result := Result or S_IFREG;
end;

function UnixToWinFileAttr(Attr: TFileAttrs): TFileAttrs;
begin
  Result := 0;
  case (Attr and S_IFMT) of
    0, S_IFREG:
      Result := faArchive;

    S_IFLNK:
      Result := Result or faSymLink;

    S_IFDIR:
      Result := Result or faDirectory;

    S_IFIFO, S_IFCHR, S_IFBLK, S_IFSOCK:
      Result := Result or faSysFile;
  end;

  if (Attr and S_IWUSR) = 0 then
    Result := Result or faReadOnly;
end;

function UnixToWinFileAttr(const FileName: String; Attr: TFileAttrs): TFileAttrs;
begin
  Result := UnixToWinFileAttr(Attr);
  if (Length(FileName) > 1) and (FileName[1] = '.') and (FileName[2] <> '.')  then
    Result := Result or faHidden;
end;

function SingleStrToFileAttr(sAttr: String): TFileAttrs;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := WinSingleStrToFileAttr(sAttr);
{$ELSEIF DEFINED(UNIX)}
  Result := UnixSingleStrToFileAttr(sAttr);
{$ENDIF}
end;

function WinSingleStrToFileAttr(sAttr: String): TFileAttrs;
var
  i: Integer;
begin
  for i := Low(WinAttrStrToFileAttr) to High(WinAttrStrToFileAttr) do
  begin
    if sAttr = WinAttrStrToFileAttr[i].Str then
      Exit(WinAttrStrToFileAttr[i].Attr);
  end;
  Result := 0;
end;

function UnixSingleStrToFileAttr(sAttr: String): TFileAttrs;
var
  i: Integer;
begin
  if Length(sAttr) > 0 then
  begin
    if sAttr[1] in ['0'..'7'] then
    begin
      // Octal representation.
      Exit(TFileAttrs(OctToDec(sAttr)));
    end
    else
    begin
      for i := Low(UnixAttrStrToFileAttr) to High(UnixAttrStrToFileAttr) do
      begin
        if sAttr = UnixAttrStrToFileAttr[i].Str then
          Exit(UnixAttrStrToFileAttr[i].Attr);
      end;
    end;
  end;
  Result := 0;
end;

function StrToFileAttr(sAttr: String): TFileAttrs; inline;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := WinStrToFileAttr(sAttr);
{$ELSEIF DEFINED(UNIX)}
  Result := UnixStrToFileAttr(sAttr);
{$ENDIF}
end;

function FileAttrToStr(Attr: TFileAttrs): String;
var
  i: Integer;
begin
  Result := '';
{$IF DEFINED(MSWINDOWS)}
  for i := Low(WinAttrStrToFileAttr) to High(WinAttrStrToFileAttr) do
  begin
    if Attr and WinAttrStrToFileAttr[i].Attr <> 0 then
      Result := Result + WinAttrStrToFileAttr[i].Str + '+';
  end;
{$ELSEIF DEFINED(UNIX)}
  for i := Low(UnixAttrStrToFileAttr) to High(UnixAttrStrToFileAttr) do
  begin
    if Attr and UnixAttrStrToFileAttr[i].Attr <> 0 then
      Result := Result + UnixAttrStrToFileAttr[i].Str + '+';
  end;
{$ENDIF}
end;

function WinStrToFileAttr(sAttr: String): TFileAttrs;
var
  I: LongInt;
begin
  Result:= 0;
  sAttr:= LowerCase(sAttr);

  for I:= 1 to Length(sAttr) do
  case sAttr[I] of
    'd': Result := Result or FILE_ATTRIBUTE_DIRECTORY;
    'l': Result := Result or FILE_ATTRIBUTE_REPARSE_POINT;
    'r': Result := Result or FILE_ATTRIBUTE_READONLY;
    'a': Result := Result or FILE_ATTRIBUTE_ARCHIVE;
    'h': Result := Result or FILE_ATTRIBUTE_HIDDEN;
    's': Result := Result or FILE_ATTRIBUTE_SYSTEM;
  end;
end;

function UnixStrToFileAttr(sAttr: String): TFileAttrs;
begin
  Result:= 0;
  if Length(sAttr) < 10 then Exit;

  if sAttr[1] = 'd' then Result:= Result or S_IFDIR;
  if sAttr[1] = 'l' then Result:= Result or S_IFLNK;
  if sAttr[1] = 's' then Result:= Result or S_IFSOCK;
  if sAttr[1] = 'f' then Result:= Result or S_IFIFO;
  if sAttr[1] = 'b' then Result:= Result or S_IFBLK;
  if sAttr[1] = 'c' then Result:= Result or S_IFCHR;


  if sAttr[2] = 'r' then Result:= Result or S_IRUSR;
  if sAttr[3] = 'w' then Result:= Result or S_IWUSR;
  if sAttr[4] = 'x' then Result:= Result or S_IXUSR;
  if sAttr[5] = 'r' then Result:= Result or S_IRGRP;
  if sAttr[6] = 'w' then Result:= Result or S_IWGRP;
  if sAttr[7] = 'x' then Result:= Result or S_IXGRP;
  if sAttr[8] = 'r' then Result:= Result or S_IROTH;
  if sAttr[9] = 'w' then Result:= Result or S_IWOTH;
  if sAttr[10] = 'x' then Result:= Result or S_IXOTH;

  if sAttr[4] = 'S' then Result:= Result or S_ISUID;
  if sAttr[7] = 'S' then Result:= Result or S_ISGID;
  if sAttr[10] = 'T' then Result:= Result or S_ISVTX;

  if sAttr[4] = 's' then Result:= Result or S_IXUSR or S_ISUID;
  if sAttr[7] = 's' then Result:= Result or S_IXGRP or S_ISGID;
  if sAttr[10] = 't' then Result:= Result or S_IXOTH or S_ISVTX;
end;

function FormatNtfsAttributes(iAttr: TFileAttrs): String;
begin
  Result:= '--------';

  if (iAttr and FILE_ATTRIBUTE_DIRECTORY    ) <> 0 then Result[1] := 'd';
  if (iAttr and FILE_ATTRIBUTE_REPARSE_POINT) <> 0 then Result[1] := 'l';
  if (iAttr and FILE_ATTRIBUTE_READONLY     ) <> 0 then Result[2] := 'r';
  if (iAttr and FILE_ATTRIBUTE_ARCHIVE      ) <> 0 then Result[3] := 'a';
  if (iAttr and FILE_ATTRIBUTE_HIDDEN       ) <> 0 then Result[4] := 'h';
  if (iAttr and FILE_ATTRIBUTE_SYSTEM       ) <> 0 then Result[5] := 's';

  // These two are exclusive on NTFS.
  if (iAttr and FILE_ATTRIBUTE_COMPRESSED   ) <> 0 then Result[6] := 'c';
  if (iAttr and FILE_ATTRIBUTE_ENCRYPTED    ) <> 0 then Result[6] := 'e';

  if (iAttr and FILE_ATTRIBUTE_TEMPORARY    ) <> 0 then Result[7] := 't';
  if (iAttr and FILE_ATTRIBUTE_SPARSE_FILE  ) <> 0 then Result[8] := 'p';
end;

function FormatUnixAttributes(iAttr: TFileAttrs): String;
begin
  Result:= '----------';

  if ((iAttr and S_IFMT) = S_IFDIR)  then Result[1]  := 'd';
  if ((iAttr and S_IFMT) = S_IFLNK)  then Result[1]  := 'l';
  if ((iAttr and S_IFMT) = S_IFSOCK) then Result[1]  := 's';
  if ((iAttr and S_IFMT) = S_IFIFO)  then Result[1]  := 'f';
  if ((iAttr and S_IFMT) = S_IFBLK)  then Result[1]  := 'b';
  if ((iAttr and S_IFMT) = S_IFCHR)  then Result[1]  := 'c';

  if ((iAttr and S_IRUSR) = S_IRUSR) then Result[2]  := 'r';
  if ((iAttr and S_IWUSR) = S_IWUSR) then Result[3]  := 'w';
  if ((iAttr and S_IXUSR) = S_IXUSR) then Result[4]  := 'x';
  if ((iAttr and S_IRGRP) = S_IRGRP) then Result[5]  := 'r';
  if ((iAttr and S_IWGRP) = S_IWGRP) then Result[6]  := 'w';
  if ((iAttr and S_IXGRP) = S_IXGRP) then Result[7]  := 'x';
  if ((iAttr and S_IROTH) = S_IROTH) then Result[8]  := 'r';
  if ((iAttr and S_IWOTH) = S_IWOTH) then Result[9]  := 'w';
  if ((iAttr and S_IXOTH) = S_IXOTH) then Result[10] := 'x';

  if ((iAttr and S_ISUID) = S_ISUID) then
  begin
    if Result[4] = 'x' then
      Result[4]  := 's'
    else
      Result[4]  := 'S';
  end;
  if ((iAttr and S_ISGID) = S_ISGID) then
  begin
    if Result[7] = 'x' then
      Result[7]  := 's'
    else
      Result[7]  := 'S';
  end;
  if ((iAttr and S_ISVTX) = S_ISVTX) then
  begin
    if Result[10] = 'x' then
      Result[10]  := 't'
    else
      Result[10]  := 'T';
  end;
end;

end.

