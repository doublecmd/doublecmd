{
   Double commander
   -------------------------------------------------------------------------
   Standard Lua libraries with UTF-8 support

   Copyright (C) 2016-2018 Alexander Koblov (alexx2000@mail.ru)

   Based on Lua 5.1 - 5.3 source code

   Copyright (C) 1994-2018 Lua.org, PUC-Rio.

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

unit uLuaStd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua;

procedure ReplaceLibrary(L : Plua_State);

implementation

uses
  CTypes, DCBasicTypes, DCOSUtils, uMicroLibC, uOSUtils;

const
  EOF = -1;
  IO_PREFIX = '_IO_';
  IO_INPUT_ = IO_PREFIX + 'input';
  IO_OUTPUT_ = IO_PREFIX + 'output';

type
  PLStream = ^luaL_Stream;
  luaL_Stream = record
    f: Pointer;  //* stream (NULL for incompletely created streams) */
    closef: lua_CFunction;  //* to close stream (NULL for closed streams) */
  end;

function os_pushresult(L: Plua_State; i: Boolean; const filename: PAnsiChar): cint;
var
  en: cint;
begin
  en := GetLastOSError;  //* calls to Lua API may change this value */
  if (i) then begin
    lua_pushboolean(L, true);
    Result:= 1;
  end
  else begin
    lua_pushnil(L);
    lua_pushfstring(L, '%s: %s', filename, PAnsiChar(mbSysErrorMessage(en)));
    lua_pushinteger(L, en);
    Result:= 3;
  end;
end;

function luaGetEnvironmentVariable(L : Plua_State) : Integer; cdecl;
var
  AValue: String;
begin
  Result:= 1;
  AValue:= mbGetEnvironmentVariable(luaL_checkstring(L, 1));
  if (Length(AValue) = 0) then
    lua_pushnil(L)
  else begin
    lua_pushstring(L, PAnsiChar(AValue));
  end;
end;

function luaExecute(L: Plua_State): Integer; cdecl;
begin
  Result:= 1;
  lua_pushinteger(L, csystem(luaL_optstring(L, 1, nil)));
end;

function luaRemove(L: Plua_State): Integer; cdecl;
var
  ok: Boolean;
  attr: TFileAttrs;
  filename: PAnsiChar;
begin
  filename := luaL_checkstring(L, 1);
  attr:= mbFileGetAttr(filename);
  if (attr = faInvalidAttributes) then
    ok:= True
  else if FPS_ISDIR(attr) then
    ok:= mbRemoveDir(filename)
  else begin
    ok:= mbDeleteFile(filename);
  end;
  Result:= os_pushresult(L, ok, filename);
end;

function luaRenameFile(L: Plua_State): Integer; cdecl;
var
  oldname, newname: PAnsiChar;
begin
  oldname := luaL_checkstring(L, 1);
  newname := luaL_checkstring(L, 2);
  Result:= os_pushresult(L, mbRenameFile(oldname, newname), oldname);
end;

function luaTempName(L: Plua_State): Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, PAnsiChar(GetTempName(EmptyStr)));
end;

function luaL_testudata (L: Plua_State; ud: cint; tname: PAnsiChar): Pointer;
begin
  Result := lua_touserdata(L, ud);
  if (Result <> nil) then
  begin  //* value is a userdata? */
    if (lua_getmetatable(L, ud)) then
    begin  //* does it have a metatable? */
      luaL_getmetatable(L, tname);  //* get correct metatable */
      if (not lua_rawequal(L, -1, -2)) then  //* not the same? */
        Result := nil;  //* value is a userdata with wrong metatable */
      lua_pop(L, 2);  //* remove both metatables */
    end;
  end;
end;

function luaL_fileresult(L: Plua_State; i: Boolean; const filename: PAnsiChar): cint;
var
  en: cint;
begin
  en := cerrno;  //* calls to Lua API may change this value */
  if (i) then
  begin
    lua_pushboolean(L, true);
    Result:= 1;
  end
  else begin
    lua_pushnil(L);
    if Assigned(filename) then
      lua_pushfstring(L, '%s: %s', filename, cstrerror(en))
    else
      lua_pushfstring(L, '%s', cstrerror(en));
    lua_pushinteger(L, en);
    Result:= 3;
  end;
end;

function tolstream(L: Pointer): PLStream; inline;
begin
  Result := PLStream(luaL_checkudata(L, 1, LUA_FILEHANDLE));
end;

function isclosed(p: PLStream): Boolean; inline;
begin
  Result := (p^.closef = nil);
end;

function io_type (L: Plua_State): cint; cdecl;
var
  p: PLStream;
begin
  luaL_checkany(L, 1);
  p := PLStream(luaL_testudata(L, 1, LUA_FILEHANDLE));
  if (p = nil) then
    lua_pushnil(L)  //* not a file */
  else if (isclosed(p)) then
    lua_pushliteral(L, 'closed file')
  else
    lua_pushliteral(L, 'file');
  Result := 1;
end;

function f_tostring (L: Plua_State): cint; cdecl;
var
  p: PLStream;
begin
  p := tolstream(L);
  if (isclosed(p)) then
    lua_pushliteral(L, 'file (closed)')
  else
    lua_pushfstring(L, 'file (%p)', p^.f);
  Result := 1;
end;

function tofile (L: Plua_State): Pointer;
var
  p: PLStream;
begin
  p := tolstream(L);
  if (isclosed(p)) then
    luaL_error(L, 'attempt to use a closed file');
  lua_assert(p^.f <> nil);
  Result := p^.f;
end;

(*
** When creating file handles, always creates a 'closed' file handle
** before opening the actual file; so, if there is a memory error, the
** handle is in a consistent state.
*)
function newprefile (L: Plua_State): PLStream;
begin
  Result := PLStream(lua_newuserdata(L, sizeof(luaL_Stream)));
  // WriteLn('newprefile: ', HexStr(Result));
  Result^.closef := nil;  //* mark file handle as 'closed' */
  luaL_getmetatable(L, LUA_FILEHANDLE);
  lua_setmetatable(L, -2);
end;

(*
** Calls the 'close' function from a file handle.
*)
function aux_close (L: Plua_State): cint; cdecl;
var
  p: PLStream;
  cf: lua_CFunction;
begin
  p := tolstream(L);
  cf := p^.closef;
  p^.closef := nil;  //* mark stream as closed */
  Result := cf(L);   //* close it */
end;

function f_close (L: Plua_State): cint; cdecl;
begin
  tofile(L);  //* make sure argument is an open stream */
  Result := aux_close(L);
end;

function io_close (L: Plua_State): cint; cdecl;
begin
  if (lua_isnone(L, 1)) then  //* no argument? */
    lua_getfield(L, LUA_REGISTRYINDEX, IO_OUTPUT_);  //* use standard output */
  Result := f_close(L);
end;

function f_gc (L: Plua_State): cint; cdecl;
var
  p: PLStream;
begin
  p := tolstream(L);
  // WriteLn('f_gc: ', hexStr(p));
  if (not isclosed(p)) and (p^.f <> nil) then
    aux_close(L);  //* ignore closed and incompletely open files */
  Result := 0;
end;

(*
** function to close regular files
*)
function io_fclose (L: Plua_State): cint; cdecl;
var
  p: PLStream;
begin
  p := tolstream(L);
  Result := cfclose(p^.f);
  // p^.closef := nil;
  Result := luaL_fileresult(L, (Result = 0), nil);
end;

function newfile (L: Plua_State): PLStream;
begin
  Result := newprefile(L);
  Result^.f := nil;
  Result^.closef := @io_fclose;
end;

procedure opencheck (L: Plua_State; const fname, mode: pansichar);
var
  p: PLStream;
begin
  p := newfile(L);
  p^.f := cfopen(fname, mode);
  if (p^.f = nil) then
    luaL_error(L, 'cannot open file "%s" (%s)', fname, cstrerror(cerrno));
end;

function io_open (L: Plua_State): cint; cdecl;
var
  p: PLStream;
  filename, mode: pansichar;
begin
  filename := luaL_checkstring(L, 1);
  mode := luaL_optstring(L, 2, 'r');
  p := newfile(L);
  p^.f := cfopen(filename, mode);
  if (p^.f <> nil) then
    Result := 1
  else begin
    luaL_fileresult(L, false, filename);
  end;
end;

(*
** function to close 'popen' files
*)
function io_pclose (L: Plua_State): cint; cdecl;
var
  p: PLStream;
begin
  p := tolstream(L);
  if Assigned(luaL_execresult) then
    Result := luaL_execresult(L, cpclose(p^.f))
  else begin
    Result := cpclose(p^.f);
    Result := luaL_fileresult(L, (Result = 0), nil);
  end;
end;

function io_popen (L: Plua_State): cint; cdecl;
var
  p: PLStream;
  filename, mode: pansichar;
begin
  filename := luaL_checkstring(L, 1);
  mode := luaL_optstring(L, 2, 'r');
  p := newprefile(L);
  p^.f := cpopen(filename, mode);
  p^.closef := @io_pclose;
  if (p^.f <> nil) then
    Result := 1
  else begin
    luaL_fileresult(L, false, filename);
  end;
end;

function io_tmpfile (L: Plua_State): cint; cdecl;
var
  p: PLStream;
begin
  p := newfile(L);
  p^.f := ctmpfile();
  if (p^.f <> nil) then
    Result := 1
  else begin
    luaL_fileresult(L, false, nil);
  end;
end;

function getiofile (L: Plua_State; findex: PAnsiChar): Pointer;
var
  p: PLStream;
begin
  lua_getfield(L, LUA_REGISTRYINDEX, findex);
  p := PLStream(lua_touserdata(L, -1));
  if (isclosed(p)) then
    luaL_error(L, 'standard %s file is closed', findex + Length(IO_PREFIX));
  Result :=  p^.f;
end;

function g_iofile (L: Plua_State; f, mode: pansichar): cint; cdecl;
var
  filename: PAnsiChar;
begin
  if (not lua_isnoneornil(L, 1)) then
  begin
    filename := lua_tostring(L, 1);
    if Assigned(filename) then
      opencheck(L, filename, mode)
    else begin
      tofile(L);  //* check that it's a valid file handle */
      lua_pushvalue(L, 1);
    end;
    lua_setfield(L, LUA_REGISTRYINDEX, f);
  end;
  //* return current value */
  lua_getfield(L, LUA_REGISTRYINDEX, f);
  Result := 1;
end;

function io_input (L: Plua_State): cint; cdecl;
begin
  Result :=  g_iofile(L, IO_INPUT_, 'r');
end;

function io_output (L: Plua_State): cint; cdecl;
begin
  Result := g_iofile(L, IO_OUTPUT_, 'w');
end;

function io_readline (L: Plua_State): cint; cdecl; forward;

procedure aux_lines (L: Plua_State; toclose: boolean); cdecl;
var
  i, n: cint;
begin
  n := lua_gettop(L) - 1;  //* number of arguments to read */
  //* ensure that arguments will fit here and into 'io_readline' stack */
  luaL_argcheck(L, n <= LUA_MINSTACK - 3, LUA_MINSTACK - 3, 'too many options');
  lua_pushvalue(L, 1);  //* file handle */
  lua_pushinteger(L, n);  //* number of arguments to read */
  lua_pushboolean(L, toclose);  //* close/not close file when finished */
  for i := 1 to n do lua_pushvalue(L, i + 1);  //* copy arguments */
  lua_pushcclosure(L, @io_readline, 3 + n);
end;

function f_lines (L: Plua_State): cint; cdecl;
begin
  tofile(L);  //* check that it's a valid file handle */
  aux_lines(L, false);
  Result := 1;
end;

function io_lines (L: Plua_State): cint; cdecl;
var
  toclose: boolean;
  filename: PAnsiChar;
begin
  if (lua_isnone(L, 1)) then lua_pushnil(L);  //* at least one argument */
  if (lua_isnil(L, 1)) then
  begin  //* no file name? */
    lua_getfield(L, LUA_REGISTRYINDEX, IO_INPUT_);  //* get default input */
    lua_replace(L, 1);  //* put it at index 1 */
    tofile(L);  //* check that it's a valid file handle */
    toclose := false;  //* do not close it after iteration */
  end
  else begin  //* open a new file */
    filename := luaL_checkstring(L, 1);
    opencheck(L, filename, 'r');
    lua_replace(L, 1);  //* put file at index 1 */
    toclose := true;  //* close it after iteration */
  end;
  aux_lines(L, toclose);
  Result := 1;
end;

(*
** {======================================================
** READ
** =======================================================
*)

function read_number (L: Plua_State; f: Pointer): cint; cdecl;
var
  d: lua_Number;
begin
  if (cfscanf(f, LUA_NUMBER_SCAN, @d) = 1) then
  begin
    lua_pushnumber(L, d);
    Result := 1;
  end
  else begin
    lua_pushnil(L);  //* "result" to be removed */
    Result := 0;  //* read fails */
  end;
end;

function test_eof (L: Plua_State; f: Pointer): cint; cdecl;
var
  c: cint;
begin
  c := cgetc(f);
  cungetc(c, f);  //* no-op when c == EOF */
  lua_pushliteral(L, '');
  Result := cint(LongBool(c <> EOF));
end;

function read_line (L: Plua_State; f: Pointer; chop: cint): cint; cdecl;
var
  k: csize_t;
  p: PAnsiChar;
  b: luaL_Buffer;
begin
  luaL_buffinit(L, @b);
  while (True) do
  begin
    p := luaL_prepbuffer(@b);
    if (cfgets(p, LUAL_BUFFERSIZE, f) = nil) then //* eof? */
    begin
      luaL_pushresult(@b);  //* close buffer */
      Exit(cint(lua_objlen(L, -1) > 0));  //* check whether read something */
    end;
    k := strlen(p);
    if (k = 0) or (p[k - 1] <> #10) then
      luaL_addsize(@b, k)
    else begin
      luaL_addsize(@b, k - chop);  //* chop 'eol' if needed */
      luaL_pushresult(@b);  //* close buffer */
      Exit(1);  //* read at least an `eol' */
    end;
  end;
end;

procedure read_all (L: Plua_State; f: Pointer); cdecl;
var
  nr: csize_t;
  p: PAnsiChar;
  b: luaL_Buffer;
begin
  luaL_buffinit(L, @b);
  repeat  //* read file in chunks of LUAL_BUFFERSIZE bytes */
    p := luaL_prepbuffer(@b);
    nr := cfread(p, sizeof(AnsiChar), LUAL_BUFFERSIZE, f);
    luaL_addsize(@b, nr);
  until (nr <> LUAL_BUFFERSIZE);
  luaL_pushresult(@b);  //* close buffer */
end;

function read_chars (L: Plua_State; f: Pointer; n: csize_t): cint; cdecl;
var
  rlen: csize_t;  //* how much to read */
  nr: csize_t;  //* number of chars actually read */
  p: PAnsiChar;
  b: luaL_Buffer;
begin
  luaL_buffinit(L, @b);
  rlen := LUAL_BUFFERSIZE;  //* try to read that much each time */
  repeat
    p := luaL_prepbuffer(@b);
    if (rlen > n) then rlen := n;  //* cannot read more than asked */
    nr := cfread(p, sizeof(AnsiChar), rlen, f);
    luaL_addsize(@b, nr);
    n -= nr;  //* still have to read 'n' chars */
  until not ((n > 0) and (nr = rlen));  //* until end of count or eof */
  luaL_pushresult(@b);  //* close buffer */
  Result := cint((n = 0) or (lua_objlen(L, -1) > 0));
end;

function g_read (L: Plua_State; f: Pointer; first: cint): cint; cdecl;
var
  k: csize_t;
  p: PAnsiChar;
  n, nargs, success: cint;
begin
  nargs := lua_gettop(L) - 1;
  cclearerr(f);
  if (nargs = 0) then
  begin  //* no arguments? */
    success := read_line(L, f, 1);
    n := first + 1;  //* to return 1 result */
  end
  else begin  //* ensure stack space for all results and for auxlib's buffer */
    luaL_checkstack(L, nargs + LUA_MINSTACK, 'too many arguments');
    success := 1;
    n := first;
    while (nargs <> 0) and (success <> 0) do
    begin
      if (lua_type(L, n) = LUA_TNUMBER) then
      begin
        k := csize_t(luaL_checkinteger(L, n));
        if (k = 0) then
          success := test_eof(L, f)
        else
          success := read_chars(L, f, k);
      end
      else begin
        p := luaL_checkstring(L, n);
        if (p^ = '*') then Inc(p);  //* skip optional '*' (for compatibility) */
        case (p^) of
          'n':  //* number */
            success := read_number(L, f);
          'l':  //* line */
            success := read_line(L, f, 1);
          'L':  //* line with end-of-line */
            success := read_line(L, f, 0);
          'a':  //* file */
            begin
              read_all(L, f);  //* read entire file */
              success := 1; //* always success */
            end;
          else
            Exit(luaL_argerror(L, n, 'invalid format'));
        end;
      end;
      Inc(n);
      Dec(nargs);
    end;
  end;
  if (cferror(f) <> 0) then
    Exit(luaL_fileresult(L, false, nil));
  if (success = 0) then
  begin
    lua_pop(L, 1);  //* remove last result */
    lua_pushnil(L);  //* push nil instead */
  end;
  Result := n - first;
end;

function io_read (L: Plua_State): cint; cdecl;
begin
  Result := g_read(L, getiofile(L, IO_INPUT_), 1);
end;

function f_read (L: Plua_State): cint; cdecl;
begin
  Result := g_read(L, tofile(L), 2);
end;

function io_readline (L: Plua_State): cint; cdecl;
var
  i, n: cint;
  p: PLStream;
begin
  p := PLStream(lua_touserdata(L, lua_upvalueindex(1)));
  n := cint(lua_tointeger(L, lua_upvalueindex(2)));
  if (isclosed(p)) then  //* file is already closed? */
    Exit(luaL_error(L, 'file is already closed'));
  lua_settop(L , 1);
  luaL_checkstack(L, n, 'too many arguments');
  for i := 1 to n do  //* push arguments to 'g_read' */
    lua_pushvalue(L, lua_upvalueindex(3 + i));
  n := g_read(L, p^.f, 2);  //* 'n' is number of results */
  lua_assert(n > 0);  //* should return at least a nil */
  if (lua_toboolean(L, -n)) then  //* read at least one value? */
    Exit(n);  //* return them */
  //* first result is nil: EOF or error */
  if (n > 1) then
  begin
    //* is there error information? */
    //* 2nd result is error message */
    Exit(luaL_error(L, '%s', lua_tostring(L, -n + 1)));
  end;
  if (lua_toboolean(L, lua_upvalueindex(3))) then
  begin  //* generator created file? */
    lua_settop(L, 0);
    lua_pushvalue(L, lua_upvalueindex(1));
    aux_close(L);  //* close it */
  end;
  Result := 0;
end;

//* }====================================================== */

function g_write (L: Plua_State; f: Pointer; arg: cint): cint; cdecl;
var
  k: csize_t;
  s: PAnsiChar;
  len, nargs: cint;
  status: Boolean = True;
begin
  nargs := lua_gettop(L) - arg;
  while (nargs > 0) do
  begin
    if (lua_type(L, arg) = LUA_TNUMBER) then
    begin
      //* optimization: could be done exactly as for strings */
      if Assigned(lua_isinteger) and lua_isinteger(L, arg) then
        len := cfprintf(f, LUA_INTEGER_FMT, lua_tointeger(L, arg))
       else
        len := cfprintf(f, LUA_NUMBER_FMT, lua_tonumber(L, arg));
      status := status and (len > 0);
    end
    else begin
      s := luaL_checklstring(L, arg, @k);
      status := status and (cfwrite(s, sizeof(AnsiChar), k, f) = k);
    end;
    Inc(arg);
    Dec(nargs);
  end;
  if (status) then
    Result := 1 //* file handle already on stack top */
  else
    Result := luaL_fileresult(L, status, nil);
end;

function io_write (L: Plua_State): cint; cdecl;
begin
  Result := g_write(L, getiofile(L, IO_OUTPUT_), 1);
end;

function f_write (L: Plua_State): cint; cdecl;
var
  f: Pointer;
begin
  f := tofile(L);
  lua_pushvalue(L, 1);  //* push file at the stack top (to be returned) */
  Result := g_write(L, f, 2);
end;

function f_seek (L: Plua_State): cint; cdecl;
const
  mode: array[0..2] of cint = (0, 1, 2);
  modenames: array[0..3] of PAnsiChar = ('set', 'cur', 'end', nil);
var
  op: cint;
  f: Pointer;
  offset: clong;
begin
  f := tofile(L);
  op := luaL_checkoption(L, 2, 'cur', modenames);
  offset := luaL_optlong(L, 3, 0);
  op := cfseek(f, offset, mode[op]);
  if (op <> 0) then
    Result := luaL_fileresult(L, false, nil)  //* error */
  else begin
    lua_pushinteger(L, lua_Integer(cftell(f)));
    Result := 1;
  end;
end;

function f_setvbuf (L: Plua_State): cint; cdecl;
const
  mode: array[0..2] of cint = (_IONBF,  _IOFBF, _IOLBF);
  modenames: array[0..3] of PAnsiChar = ('no', 'full', 'line', nil);
var
  op: cint;
  f: Pointer;
  sz: lua_Integer;
begin
  f := tofile(L);
  op := luaL_checkoption(L, 2, nil, modenames);
  sz := luaL_optinteger(L, 3, LUAL_BUFFERSIZE);
  Result := csetvbuf(f, nil, mode[op], csize_t(sz));
  Result := luaL_fileresult(L, Result = 0, nil);
end;

function io_flush (L: Plua_State): cint; cdecl;
begin
  Result := luaL_fileresult(L, cfflush(getiofile(L, IO_OUTPUT_)) = 0, nil);
end;

function f_flush (L: Plua_State): cint; cdecl;
begin
  Result := luaL_fileresult(L, cfflush(tofile(L)) = 0, nil);
end;

var
  io_noclose_: lua_CFunction;

(*
** function to (not) close the standard files stdin, stdout, and stderr
*)
function io_noclose (L: Plua_State): cint; cdecl;
var
  p: PLStream;
begin
  p := tolstream(L);
  p^.closef := io_noclose_;  //* keep file opened */
  lua_pushnil(L);
  lua_pushliteral(L, 'cannot close standard file');
  Result := 2;
end;

procedure createstdfile (L: Plua_State; f: Pointer; k, fname: PAnsiChar);
var
  p: PLStream;
begin
  p := newprefile(L);
  p^.f := f;
  p^.closef := @io_noclose;
  if (k <> nil) then
  begin
    lua_pushvalue(L, -1);
    lua_setfield(L, LUA_REGISTRYINDEX, k);  //* add file to registry */
  end;
  lua_setfield(L, -2, fname);  //* add file to module */
end;

procedure luaP_register(L : Plua_State; n : PChar; f : lua_CFunction);
begin
  lua_pushcfunction(L, f);
  lua_setfield(L, -2, n);
end;

procedure ReplaceLibrary(L : Plua_State);
var
  StdIn, StdOut, StdErr: PPointer;
begin
  //* Replace functions for 'os' library */
  lua_getglobal(L, LUA_OSLIBNAME);
    luaP_register(L, 'remove', @luaRemove);
    luaP_register(L, 'execute', @luaExecute);
    luaP_register(L, 'tmpname', @luaTempName);
    luaP_register(L, 'rename', @luaRenameFile);
    luaP_register(L, 'getenv', @luaGetEnvironmentVariable);
  lua_pop(L, 1);

  io_noclose_:= @io_noclose;
  //* Replace functions for 'io' library */
  lua_getglobal(L, LUA_IOLIBNAME);
    luaP_register(L, 'close', @io_close);
    luaP_register(L, 'flush', @io_flush);
    luaP_register(L, 'input', @io_input);
    luaP_register(L, 'lines', @io_lines);
    luaP_register(L, 'open', @io_open);
    luaP_register(L, 'output', @io_output);
    luaP_register(L, 'popen', @io_popen);
    luaP_register(L, 'read', @io_read);
    luaP_register(L, 'tmpfile', @io_tmpfile);
    luaP_register(L, 'type', @io_type);
    luaP_register(L, 'write', @io_write);
  lua_pop(L, 1);

  //* Remove (clear) default metatable */
  lua_pushnil(L);
  lua_setfield(L, LUA_REGISTRYINDEX, LUA_FILEHANDLE);

  //* Create metatable for file handles */
  luaL_newmetatable(L, LUA_FILEHANDLE);
    lua_pushvalue(L, -1);  //* push metatable */
    lua_setfield(L, -2, '__index');  //* metatable.__index = metatable */
    //* add file methods to new metatable */
    luaP_register(L, 'close', @f_close);
    luaP_register(L, 'flush', @f_flush);
    luaP_register(L, 'lines', @f_lines);
    luaP_register(L, 'read', @f_read);
    luaP_register(L, 'seek', @f_seek);
    luaP_register(L, 'setvbuf', @f_setvbuf);
    luaP_register(L, 'write', @f_write);
    luaP_register(L, '__gc', @f_gc);
    luaP_register(L, '__tostring', @f_tostring);
  lua_pop(L, 1);  //* pop new metatable */

  //* get and set default files */
  lua_getglobal(L, LUA_IOLIBNAME);
    lua_getfield(L, -1, 'stdin');
      StdIn := lua_touserdata(L, -1);
    lua_pop(L, 1);
    lua_getfield(L, -1, 'stdout');
      StdOut := lua_touserdata(L, -1);
    lua_pop(L, 1);
    lua_getfield(L, -1, 'stderr');
      StdErr := lua_touserdata(L, -1);
    lua_pop(L, 1);

    createstdfile(L, stdin^, IO_INPUT_, 'stdin');
    createstdfile(L, stdout^, IO_OUTPUT_, 'stdout');
    createstdfile(L, stderr^, nil, 'stderr');
  lua_pop(L, 1);
end;

end.

