{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Marco van de Voort(marco@freepascal.org)
    member of the Free Pascal development team

    libiconv header translation + a helper routine
    http://wiki.freepascal.org/iconvenc Dynamic version

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright. (LGPL)

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit dc_iconvenc_dyn;

interface
{$mode objfpc}{$H+}

uses
  ctypes,unixtype,baseunix,
  dl,
  initc;

const
  n = 1;

{$ifdef beos}
  ESysEILSEQ = EILSEQ;
{$endif}

type
   piconv_t = ^iconv_t;
   iconv_t = pointer;

   Ticonv_open = function(__tocode: pchar; __fromcode: pchar): iconv_t; cdecl;
   Ticonv = function(__cd: iconv_t; __inbuf: ppchar; __inbytesleft: psize_t; __outbuf: ppchar; __outbytesleft: psize_t): size_t; cdecl;
   Ticonv_close = function(__cd: iconv_t): cint; cdecl;

var
  iconv_lib: pointer;
  iconv_open: Ticonv_open;
  iconv: Ticonv;
  iconv_close: Ticonv_close;
  IconvLibFound: boolean = False;

function TryLoadLib(LibName: string; var error: string): boolean; // can be used to load non standard libname
function Iconvert(s: string; var res: string; const FromEncoding, ToEncoding: string): cint;
function InitIconv(var error: string): boolean;

implementation

function TryLoadLib(LibName: string; var error: string): boolean;

    function resolvesymbol (var funcptr; symbol: string): Boolean;
    begin
      pointer(funcptr) := pointer(dlsym(iconv_lib, pchar(symbol)));
      result := assigned(pointer(funcptr));
      if not result then
        error := error+#13#10+dlerror();
    end;

var
  res: boolean;
begin
  result := false;
  Error := Error+#13#10'Trying '+LibName;
  iconv_lib := dlopen(pchar(libname), RTLD_NOW);
  if Assigned(iconv_lib) then
  begin
    result := true;
    result := result and resolvesymbol(pointer(iconv),'iconv');
    result := result and resolvesymbol(pointer(iconv_open),'iconv_open');
    result := result and resolvesymbol(pointer(iconv_close),'iconv_close');
    if not result then
      begin
        result:=true;
        result := result and resolvesymbol(pointer(iconv),'libiconv');
        result := result and resolvesymbol(pointer(iconv_open),'libiconv_open');
        result := result and resolvesymbol(pointer(iconv_close),'libiconv_close');
      end;
//    if not res then
//      dlclose(iconv_lib);
  end else
    error:=error+#13#10+dlerror();
end;


function InitIconv(var error: string): boolean;
begin
  result := true;
  error := '';
  if not TryLoadLib('libc.so.6', error) then
    if not TryLoadLib('libiconv.so', error) then
    {$if defined(haiku)}
        if not TryLoadLib('libtextencoding.so', error) then
      {$ifend}
      result := false;
  iconvlibfound := iconvlibfound or result;
end;

{$i dc_iconvert.inc}

end.
