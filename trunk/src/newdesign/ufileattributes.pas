unit uFileAttributes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes;

const // Windows attributes
  FILE_ATTRIBUTE_ARCHIVE     = 32;
  FILE_ATTRIBUTE_NORMAL      = 128;
  FILE_ATTRIBUTE_DIRECTORY   = 16;
  FILE_ATTRIBUTE_HIDDEN      = 2;
  FILE_ATTRIBUTE_READONLY    = 1;
  FILE_ATTRIBUTE_SYSTEM      = 4;
  FILE_ATTRIBUTE_TEMPORARY   = 256;
  FILE_ATTRIBUTE_SPARSE_FILE = $0200;
  FILE_ATTRIBUTE_REPARSE_POINT = $0400;
  FILE_ATTRIBUTE_COMPRESSED  = $0800;
  FILE_ATTRIBUTE_OFFLINE     = $1000;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $2000;
  FILE_ATTRIBUTE_ENCRYPTED   = $4000;
  FILE_ATTRIBUTE_VIRTUAL     = $20000;

const // Unix attributes
  S_IFMT   = $F000;
  { first-in/first-out (FIFO/pipe)            }
  S_IFIFO  = $1000;
  { character-special file (tty/console)      }
  S_IFCHR  = $2000;
  { directory                                 }
  S_IFDIR  = $4000;
  { blocking device (unused)                  }
  S_IFBLK  = $6000;
  { regular                                   }
  S_IFREG  = $8000;
  { symbolic link (unused)                    }
  S_IFLNK  = $A000;
  { Berkeley socket                           }
  S_IFSOCK = $C000;
  S_IRWXU  = $01C0;
  S_IRUSR  = $0100;
  S_IWUSR  = $0080;
  S_IXUSR  = $0040;
  S_IREAD  = S_IRUSR;
  S_IWRITE = S_IWUSR;
  S_IEXEC  = S_IXUSR;
  { POSIX file modes: group permission...  }
  S_IRWXG  = $0038;
  S_IRGRP  = $0020;
  S_IWGRP  = $0010;
  S_IXGRP  = $0008;
  { POSIX file modes: other permission...  }
  S_IRWXO  = $0007;
  S_IROTH  = $0004;
  S_IWOTH  = $0002;
  S_IXOTH  = $0001;
  { POSIX setuid(), setgid(), and sticky...  }
  S_ISUID  = $0800;
  S_ISGID  = $0400;
  S_ISVTX  = $0200;

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
  sAttr:= LowerCase(sAttr);

  if sAttr[1]='d' then Result:= Result or S_IFDIR;
  if sAttr[1]='l' then Result:= Result or S_IFLNK;
  if sAttr[1]='s' then Result:= Result or S_IFSOCK;
  if sAttr[1]='f' then Result:= Result or S_IFIFO;
  if sAttr[1]='b' then Result:= Result or S_IFBLK;
  if sAttr[1]='c' then Result:= Result or S_IFCHR;


  if sAttr[2]='r' then Result:= Result or S_IRUSR;
  if sAttr[3]='w' then Result:= Result or S_IWUSR;
  if sAttr[4]='x' then Result:= Result or S_IXUSR;
  if sAttr[5]='r' then Result:= Result or S_IRGRP;
  if sAttr[6]='w' then Result:= Result or S_IWGRP;
  if sAttr[7]='x' then Result:= Result or S_IXGRP;
  if sAttr[8]='r' then Result:= Result or S_IROTH;
  if sAttr[9]='w' then Result:= Result or S_IWOTH;
  if sAttr[10]='x' then Result:= Result or S_IXOTH;

  if sAttr[4]='s' then Result:= Result or S_ISUID;
  if sAttr[7]='s' then Result:= Result or S_ISGID;
end;

end.
