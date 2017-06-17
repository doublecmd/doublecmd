{
   Double commander
   -------------------------------------------------------------------------
   Some standard C library functions

   Copyright (C) 2017 Alexander Koblov (alexx2000@mail.ru)

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

function fpgetCerrno: cint;
procedure fpsetCerrno(err: cint);
{$ENDIF}

function csystem(const command: String): cint;
function cfopen(const path, mode: String): Pointer;
function cpopen(const command, mode: String): Pointer;
function cstrerror(errnum: cint): PAnsiChar; cdecl; external clib name 'strerror';

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

function fflush(stream: Pointer): cint; cdecl; external clib;
function system(const command: PAnsiChar): cint; cdecl; external clib;
function popen(const command, mode: PAnsiChar): Pointer; cdecl; external clib;
function fopen(const filename, mode: PAnsiChar): Pointer; cdecl; external clib;

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
  fflush(nil);
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

