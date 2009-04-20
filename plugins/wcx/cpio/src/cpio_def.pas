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

{$ifdef ver90}
type longword=longint;
{$endif}
{$ifdef ver100}
type longword=longint;
{$endif}

type
  CPIO_Header = record
    records  : array [1..14] of LongWord;
    filename : String;
    origname : String;
    oldhdrtype:boolean;
(* records array contains fields :
  {01}c_magic    : LongWord; //"070701" for "new" portable format "070702" for CRC format
  {02}c_ino      : LongWord;
  {03}c_mode     : LongWord;
  {04}c_uid      : LongWord;
  {05}c_gid      : LongWord;
  {06}c_nlink    : LongWord;
  {07}c_mtime    : LongWord;
  {08}c_filesize : LongWord; //must be 0 for FIFOs and directories
  {09}c_maj      : LongWord;
  {10}c_min      : LongWord;
  {11}c_rmaj     : LongWord; //only valid for chr and blk special files
  {12}c_rmin     : LongWord; //only valid for chr and blk special files
  {13}c_namesize : LongWord; //count includes terminating NUL in pathname
  {14}c_chksum   : LongWord; //0 for "new" portable format; for CRC format the sum of all the bytes in the file
*)
  end;{CPIO_Header}

(* Old CPIO header structure:*)

type tOldHdr=packed record
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

implementation

end.
