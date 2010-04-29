//***************************************************************
// This file is part of RPMWCX, a archiver plugin for
// Windows Commander.
// Copyright (C) 2000 Mandryka Yurij  e-mail:braingroup@hotmail.ru
//***************************************************************

//***************************************************************
// This code based on Christian Ghisler (support@ghisler.com) sources
//***************************************************************

{$A-,I-}
unit cpio_def;

interface

type
  CPIO_Header = record
    magic,
    dev_major,
    dev_minor,
    inode,
    mode,
    uid,
    gid,
    nlink,
    mtime,
    filesize,
    namesize: Longword;
    filename : UTF8String;
    origname : UTF8String;
    IsOldHeader: Boolean;
  end;{CPIO_Header}

  TOldBinaryHeader=packed record
                    c_magic,
                    c_dev,
                    c_ino,
                    c_mode,
                    c_uid,
                    c_gid,
                    c_nlink,
                    c_rdev:word;
                    c_mtime1,c_mtime2:word;
                    c_namesize:word;
                    c_filesize1,c_filesize2:word;
               (*   char c_name[c_namesize rounded to word];*)
                 end;

  TOldCharHeader=packed record
                   c_magic   : array[0..5] of AnsiChar; {070707}
                   c_dev     : array[0..5] of AnsiChar;
                   c_ino     : array[0..5] of AnsiChar;
                   c_mode    : array[0..5] of AnsiChar;
                   c_uid     : array[0..5] of AnsiChar;
                   c_gid     : array[0..5] of AnsiChar;
                   c_nlink   : array[0..5] of AnsiChar;
                   c_rdev    : array[0..5] of AnsiChar;
                   c_mtime   : array[0..10] of AnsiChar;
                   c_namesize: array[0..5] of AnsiChar;
                   c_filesize: array[0..10] of AnsiChar;
                 end;

  TNewCharHeader=packed record
                   c_magic    : array[0..5] of AnsiChar; {070701} {070702 - CRC format}
                   c_ino      : array[0..7] of AnsiChar;
                   c_mode     : array[0..7] of AnsiChar;
                   c_uid      : array[0..7] of AnsiChar;
                   c_gid      : array[0..7] of AnsiChar;
                   c_nlink    : array[0..7] of AnsiChar;
                   c_mtime    : array[0..7] of AnsiChar;
                   c_filesize : array[0..7] of AnsiChar; //must be 0 for FIFOs and directories
                   c_devmajor : array[0..7] of AnsiChar;
                   c_devminor : array[0..7] of AnsiChar;
                   c_rdevmajor: array[0..7] of AnsiChar; //only valid for chr and blk special files
                   c_rdevminor: array[0..7] of AnsiChar; //only valid for chr and blk special files
                   c_namesize : array[0..7] of AnsiChar; //count includes terminating NUL in pathname
                   c_check    : array[0..7] of AnsiChar; //0 for "new" portable format; for CRC format the sum of all the bytes in the file
                 end;

implementation

end.