unit uFileAttributes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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

implementation

end.

