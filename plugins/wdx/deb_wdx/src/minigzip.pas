{
 minigzip.c -- simulate gzip using the zlib compression library
 Copyright (C) 1995-1998 Jean-loup Gailly.

 minigzip is a minimal implementation of the gzip utility. This is
 only an example of using zlib and isn't meant to replace the
 full-featured gzip. No attempt is made to deal with file systems
 limiting names to 14 or 8+3 characters, etc... Error checking is
 very limited. So use minigzip only for testing; use gzip for the
 real thing. On MSDOS, use only on file names without extension
 or in pipe mode.

  Pascal tranlastion based on code contributed by Francisco Javier Crespo
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

unit minigzip;

{$mode fpc}{$H+}

interface

uses
  gzio;

procedure gz_compress (var infile:file; outfile:gzFile);
procedure gz_uncompress (infile:gzFile; var outfile:file);
procedure file_compress (filename:string; mode:string);
procedure file_uncompress (filename:string);

implementation

const
  BUFLEN       = 16384 ;
  GZ_SUFFIX    = '.gz' ;

{$DEFINE MAXSEF_64K}

var
  buf  : packed array [0..BUFLEN-1] of byte; { Global uses BSS instead of stack }
  prog : string;

{ ERROR =====================================================================

  Display error message and exit

============================================================================}

procedure error (msg:string);
begin
  writeln (prog,': ',msg);
  halt(1);
end;


{ GZ_COMPRESS ===============================================================

  Compress input to output then close both files

============================================================================}

procedure gz_compress (var infile:file; outfile:gzFile);
var
  len   : cardinal;
  ioerr : integer;
  err   : integer;
begin

  while true do begin

    {$I-}
    blockread (infile, buf, BUFLEN, len);
    {$I+}
    ioerr := IOResult;
    if (ioerr <> 0) then begin
      writeln ('read error: ',ioerr);
      halt(1);
    end;

    if (len = 0) then break;

    if (gzwrite (outfile, @buf, len) <> len)
      then error (gzerror (outfile, err));

  end; {WHILE}

  close (infile);
  if (gzclose (outfile) <> 0{Z_OK})
    then error ('gzclose error');
end;


{ GZ_UNCOMPRESS =============================================================

  Uncompress input to output then close both files

============================================================================}

procedure gz_uncompress (infile:gzFile; var outfile:file);
var
  len     : integer;
  written : cardinal;
  ioerr   : integer;
  err     : integer;
begin
  while true do begin

    len := gzread (infile, @buf, BUFLEN);
    if (len < 0)
      then error (gzerror (infile, err));
    if (len = 0)
      then break;

    {$I-}
    blockwrite (outfile, buf, len, written);
    {$I+}
    if (written <> len)
      then error ('write error');

  end; {WHILE}

  {$I-}
  close (outfile);
  {$I+}
  ioerr := IOResult;
  if (ioerr <> 0) then begin
    writeln ('close error: ',ioerr);
    halt(1);
  end;

  if (gzclose (infile) <> 0{Z_OK})
    then error ('gzclose error');
end;


{ FILE_COMPRESS =============================================================

  Compress the given file:
  create a corresponding .gz file and remove the original

============================================================================}

procedure file_compress (filename:string; mode:string);
var
  infile  : file;
  outfile : gzFile;
  ioerr   : integer;
  outname : string;
begin
  Assign (infile, filename);
  {$I-}
  Reset (infile,1);
  {$I+}
  ioerr := IOResult;
  if (ioerr <> 0) then begin
    writeln ('open error: ',ioerr);
    halt(1);
  end;

  outname := filename + GZ_SUFFIX;
  outfile := gzopen (outname, mode);

  if (outfile = NIL) then begin
    writeln (prog,': can''t gzopen ',outname);
    halt(1);
  end;

  gz_compress(infile, outfile);
  erase (infile);
end;


{ FILE_UNCOMPRESS ===========================================================

  Uncompress the given file and remove the original

============================================================================}

procedure file_uncompress (filename:string);
var
  inname  : string;
  outname : string;
  infile  : gzFile;
  outfile : file;
  ioerr   : integer;
  len     : integer;
begin
  len := Length(filename);

  if (copy(filename,len-2,3) = GZ_SUFFIX) then begin
    inname := filename;
    outname := copy(filename,0,len-3);
  end
  else begin
    inname := filename + GZ_SUFFIX;
    outname := filename;
  end;

  infile := gzopen (inname, 'r');
  if (infile = NIL) then begin
    writeln (prog,': can''t gzopen ',inname);
    halt(1);
  end;

  Assign (outfile, outname);
  {$I-}
  Rewrite (outfile,1);
  {$I+}
  ioerr := IOResult;
  if (ioerr <> 0) then begin
    writeln ('open error: ',ioerr);
    halt(1);
  end;

  gz_uncompress (infile, outfile);

{ erase (infile); }
end;

end.
