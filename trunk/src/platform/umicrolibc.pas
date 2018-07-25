{
   Double commander
   -------------------------------------------------------------------------
   Some standard C library functions

   Copyright (C) 2017-2018 Alexander Koblov (alexx2000@mail.ru)

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

unit uMicroLibC;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, CTypes
{$IF DEFINED(UNIX)}
  , InitC
{$ENDIF}
  ;

{$IF DEFINED(MSWINDOWS)}
const
  clib = 'msvcrt.dll';

const
  _IOFBF = $0000;
  _IONBF = $0004;
  _IOLBF = $0040;

function fpgetCerrno: cint;
procedure fpsetCerrno(err: cint);
{$ELSE}
const
  _IOFBF = 0; //* Fully buffered.  */
  _IOLBF = 1; //* Line buffered.  */
  _IONBF = 2; //* No buffering.  */

function fpgetCerrno: cint; inline;
{$ENDIF}

function csystem(const command: String): cint;
function cfopen(const path, mode: String): Pointer;
function cpopen(const command, mode: String): Pointer;
function cstrerror(errnum: cint): PAnsiChar; cdecl; external clib name 'strerror';

{$IF DEFINED(MSWINDOWS)}
function cpclose(stream: Pointer): cint; cdecl; external clib name '_pclose';
{$ELSE}
function cpclose(stream: Pointer): cint; cdecl; external clib name 'pclose';
{$ENDIF}

function cftell(stream: Pointer): cuint32; cdecl; external clib name 'ftell';
function cfseek(stream: Pointer; offset: clong; origin: cint): cint; cdecl; external clib name 'fseek';

procedure cclearerr(stream: Pointer); cdecl; external clib name 'clearerr';
function cferror(stream: Pointer): cint; cdecl; external clib name 'ferror';

function csetvbuf(stream: Pointer; buffer: PAnsiChar; mode: cint; size: csize_t): cint; cdecl; external clib name 'setvbuf';

function cgetc(stream: Pointer): cint; cdecl; external clib name 'getc';
function cungetc(c: cint; stream: Pointer): cint; cdecl; external clib name 'ungetc';
function cfgets(str: PAnsiChar; n: cint; stream: Pointer): PAnsiChar; cdecl; external clib name 'fgets';
function cfread(buffer: Pointer; size, count: csize_t; stream: Pointer): csize_t; cdecl; external clib name 'fread';
function cfwrite(buffer: Pointer; size, count: csize_t; stream: Pointer): csize_t; cdecl; external clib name 'fwrite';

function cfflush(stream: Pointer): cint; cdecl; external clib name 'fflush';

function ctmpfile(): Pointer; cdecl; external clib name 'tmpfile';
function cfclose(stream: Pointer): cint; cdecl; external clib name 'fclose';

function cfprintf(stream: Pointer; format: PAnsiChar): cint; cdecl; varargs; external clib name 'fprintf';
function cfscanf(stream: Pointer; format: PAnsiChar; argument: Pointer): cint; cdecl; external clib name 'fscanf';

property cerrno: cint read fpgetCerrno write fpsetcerrno;

implementation

uses
  {$IF DEFINED(MSWINDOWS)}
  LazUTF8, DCWindows;
  {$ELSE}
  DCConvertEncoding;
  {$ENDIF}

{$IF DEFINED(MSWINDOWS)}

function _wsystem(const command: pwidechar): cint; cdecl; external clib;
function _wfopen(const filename, mode: pwidechar): Pointer; cdecl; external clib;
function _wpopen(const command, mode: pwidechar): Pointer; cdecl; external clib;
function geterrnolocation: pcint; cdecl; external clib name '_errno';

function fpgetCerrno:cint;
begin
  fpgetCerrno:= geterrnolocation^;
end;

procedure fpsetCerrno(err:cint);
begin
  geterrnolocation^:= err;
end;

{$ELSE}

function system(const command: PAnsiChar): cint; cdecl; external clib;
function popen(const command, mode: PAnsiChar): Pointer; cdecl; external clib;
function fopen(const filename, mode: PAnsiChar): Pointer; cdecl; external clib;

function fpgetCerrno: cint; inline;
begin
  Result:= InitC.fpgetCerrno;
end;

{$ENDIF}

function cfopen(const path, mode: String): Pointer;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= _wfopen(PWideChar(UTF16LongName(path)), PWideChar(UnicodeString(mode)));
end;
{$ELSE}
begin
  Result:= fopen(PAnsiChar(CeUtf8ToSys(path)), PAnsiChar(mode));
end;
{$ENDIF}

function cpopen(const command, mode: String): Pointer;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= _wpopen(PWideChar(UTF8ToUTF16(command)), PWideChar(UnicodeString(mode)));
end;
{$ELSE}
begin
  cfflush(nil);
  Result:= popen(PAnsiChar(CeUtf8ToSys(command)), PAnsiChar(mode));
end;
{$ENDIF}

function csystem(const command: String): cint;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= _wsystem(PWideChar(UTF8ToUTF16(command)));
end;
{$ELSE}
begin
  Result:= system(PAnsiChar(CeUtf8ToSys(command)));
end;
{$ENDIF}

end.

