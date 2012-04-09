unit lua;

(*
 * A complete Pascal wrapper for Lua 5.1 DLL module.
 *
 * Created by Geo Massar, 2006
 * Distributed as free/open source.
 * 2008 Added dinamicly library loading by Dmitry Kolomiets (B4rr4cuda@rambler.ru)
 *)

interface

//{$I CallType.inc}
{$DEFINE DINAMIC}

{$IFDEF UNIX}
uses
  dl;
{$ENDIF}


{$DEFINE LUA51}


type
  size_t   = type Cardinal;
  Psize_t  = ^size_t;
  PPointer = ^Pointer;

  lua_State = record end;
  Plua_State = ^lua_State;

const
{$IF DEFINED(MSWINDOWS)}
      LuaDLL = 'lua5.1.dll';
{$ELSEIF DEFINED(UNIX)}
  {$IFDEF DARWIN}
    {$IF DEFINED(STATIC)}
      LuaDLL = 'lua5.1.dylib';
    {$ELSEIF DEFINED(DINAMIC)}
      LuaDLL = 'liblua5.1.dylib';
    {$ENDIF}
  {$ELSE}
    {$IF DEFINED(STATIC)}
      LuaDLL = 'lua5.1.so';
    {$ELSEIF DEFINED(DINAMIC)}
      LuaDLL = 'liblua5.1.so.0';
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

(* formats for Lua numbers *)
{$IFNDEF LUA_NUMBER_SCAN}
const
  LUA_NUMBER_SCAN = '%lf';
{$ENDIF}

{$IFNDEF LUA_NUMBER_FMT}
const
  LUA_NUMBER_FMT = '%.14g';
{$ENDIF}


{$IFDEF DINAMIC}
  function LoadLuaLib(filename:string):boolean;
  procedure UnloadLuaLib;
  function IsLuaLibLoaded:boolean;
{$ENDIF}


(*****************************************************************************)
(*                               luaconfig.h                                 *)
(*****************************************************************************)

(*
** $Id: luaconf.h,v 1.81 2006/02/10 17:44:06 roberto Exp $
** Configuration file for Lua
** See Copyright Notice in lua.h
*)

(*
** {==================================================================
@@ LUA_NUMBER is the type of numbers in Lua.
** CHANGE the following definitions only if you want to build Lua
** with a number type different from double. You may also need to
** change lua_number2int & lua_number2integer.
** ===================================================================
*)
type
  LUA_NUMBER_  = type Double;            // ending underscore is needed in Pascal
  LUA_INTEGER_ = type Integer;

(*
@@ LUA_IDSIZE gives the maximum size for the description of the source
@* of a function in debug information.
** CHANGE it if you want a different size.
*)
const
  LUA_IDSIZE = 60;

(*
@@ LUAL_BUFFERSIZE is the buffer size used by the lauxlib buffer system.
*)
const
  LUAL_BUFFERSIZE = 1024;

(*
@@ LUA_PROMPT is the default prompt used by stand-alone Lua.
@@ LUA_PROMPT2 is the default continuation prompt used by stand-alone Lua.
** CHANGE them if you want different prompts. (You can also change the
** prompts dynamically, assigning to globals _PROMPT/_PROMPT2.)
*)
const
  LUA_PROMPT  = '> ';
  LUA_PROMPT2 = '>> ';

(*
@@ lua_readline defines how to show a prompt and then read a line from
@* the standard input.
@@ lua_saveline defines how to "save" a read line in a "history".
@@ lua_freeline defines how to free a line read by lua_readline.
** CHANGE them if you want to improve this functionality (e.g., by using
** GNU readline and history facilities).
*)
function  lua_readline(L : Plua_State; var b : PChar; p : PChar): Boolean;
procedure lua_saveline(L : Plua_State; idx : Integer);
procedure lua_freeline(L : Plua_State; b : PChar);

(*
@@ lua_stdin_is_tty detects whether the standard input is a 'tty' (that
@* is, whether we're running lua interactively).
** CHANGE it if you have a better definition for non-POSIX/non-Windows
** systems.
*/
#include <io.h>
#include <stdio.h>
#define lua_stdin_is_tty()	_isatty(_fileno(stdin))
*)
const
  lua_stdin_is_tty = TRUE;

(*****************************************************************************)
(*                                  lua.h                                    *)
(*****************************************************************************)

(*
** $Id: lua.h,v 1.216 2006/01/10 12:50:13 roberto Exp $
** Lua - An Extensible Extension Language
** Lua.org, PUC-Rio, Brazil (http://www.lua.org)
** See Copyright Notice at the end of this file
*)

const
  LUA_VERSION     = 'Lua 5.1';
  LUA_VERSION_NUM = 501;
  LUA_COPYRIGHT   = 'Copyright (C) 1994-2006 Tecgraf, PUC-Rio';
  LUA_AUTHORS     = 'R. Ierusalimschy, L. H. de Figueiredo & W. Celes';

  (* mark for precompiled code (`<esc>Lua') *)
  LUA_SIGNATURE = #27'Lua';

  (* option for multiple returns in `lua_pcall' and `lua_call' *)
  LUA_MULTRET = -1;

  (*
  ** pseudo-indices
  *)
  LUA_REGISTRYINDEX = -10000;
  LUA_ENVIRONINDEX  = -10001;
  LUA_GLOBALSINDEX  = -10002;

function lua_upvalueindex(idx : Integer) : Integer;   // a marco

const
  (* thread status; 0 is OK *)
  LUA_YIELD_    = 1;     // Note: the ending underscore is needed in Pascal
  LUA_ERRRUN    = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM    = 4;
  LUA_ERRERR    = 5;

type
  lua_CFunction = function(L : Plua_State) : Integer; cdecl;

  (*
  ** functions that read/write blocks when loading/dumping Lua chunks
  *)
  lua_Reader = function (L : Plua_State; ud : Pointer;
                         sz : Psize_t) : PChar; cdecl;
  lua_Writer = function (L : Plua_State; const p : Pointer; sz : size_t;
                         ud : Pointer) : Integer; cdecl;

  (*
  ** prototype for memory-allocation functions
  *)
  lua_Alloc = function (ud, ptr : Pointer;
                        osize, nsize : size_t) : Pointer; cdecl;

const
  (*
  ** basic types
  *)
  LUA_TNONE          = -1;

  LUA_TNIL           = 0;
  LUA_TBOOLEAN       = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER        = 3;
  LUA_TSTRING        = 4;
  LUA_TTABLE         = 5;
  LUA_TFUNCTION      = 6;
  LUA_TUSERDATA	     = 7;
  LUA_TTHREAD        = 8;

  (* minimum Lua stack available to a C function *)
  LUA_MINSTACK = 20;

type
  (* type of numbers in Lua *)
  lua_Number = LUA_NUMBER_;

  (* type for integer functions *)
  lua_Integer = LUA_INTEGER_;

(*
** state manipulation
*)
{$IFDEF STATIC}
function  lua_newstate(f : lua_Alloc; ud : Pointer) : Plua_State;
  cdecl; external LuaDLL;
procedure lua_close(L: Plua_State);
  cdecl; external LuaDLL;
function  lua_newthread(L : Plua_State) : Plua_State;
  cdecl; external LuaDLL;

function  lua_atpanic(L : Plua_State; panicf : lua_CFunction) : lua_CFunction;
  cdecl; external LuaDLL;
{$ENDIF}

{$IFDEF DINAMIC}
type
  Tlua_newstate=  function (f : lua_Alloc; ud : Pointer) : Plua_State; cdecl;
  Tlua_close=     procedure (L: Plua_State);  cdecl;
  Tlua_newthread= function  (L : Plua_State) : Plua_State;  cdecl;
  Tlua_atpanic=   function  (L : Plua_State; panicf : lua_CFunction) : lua_CFunction;  cdecl;
{$ENDIF}

(*
** basic stack manipulation
*)
{$IFDEF STATIC}
function  lua_gettop(L : Plua_State) : Integer;
  cdecl; external LuaDLL;
procedure lua_settop(L : Plua_State; idx : Integer);
  cdecl; external LuaDLL;
procedure lua_pushvalue(L : Plua_State; idx : Integer);
  cdecl; external LuaDLL;
procedure lua_remove(L : Plua_State; idx : Integer);
  cdecl; external LuaDLL;
procedure lua_insert(L : Plua_State; idx : Integer);
  cdecl; external LuaDLL;
procedure lua_replace(L : Plua_State; idx : Integer);
  cdecl; external LuaDLL;
function  lua_checkstack(L : Plua_State; sz : Integer) : LongBool;
  cdecl; external LuaDLL;

procedure lua_xmove(src, dest : Plua_State; n : Integer);
  cdecl; external LuaDLL;
{$ENDIF}

{$IFDEF DINAMIC}
type
Tlua_gettop= function  (L : Plua_State) : Integer;  cdecl;
Tlua_settop=  procedure (L : Plua_State; idx : Integer);  cdecl;
Tlua_pushvalue=  procedure (L : Plua_State; idx : Integer);  cdecl;
Tlua_remove=  procedure (L : Plua_State; idx : Integer);  cdecl;
Tlua_insert=  procedure (L : Plua_State; idx : Integer);  cdecl;
Tlua_replace=  procedure (L : Plua_State; idx : Integer);  cdecl;
Tlua_checkstack=  function  (L : Plua_State; sz : Integer) : LongBool;  cdecl;
Tlua_xmove=  procedure (src, dest : Plua_State; n : Integer);  cdecl;

{$ENDIF}

(*
** access functions (stack -> C)
*)
{$IFDEF STATIC}
function lua_isnumber(L : Plua_State; idx : Integer) : LongBool;
  cdecl; external LuaDLL;
function lua_isstring(L : Plua_State; idx : Integer) : LongBool;
  cdecl; external LuaDLL;
function lua_iscfunction(L : Plua_State; idx : Integer) : LongBool;
  cdecl; external LuaDLL;
function lua_isuserdata(L : Plua_State; idx : Integer) : LongBool;
  cdecl; external LuaDLL;
function lua_type(L : Plua_State; idx : Integer) : Integer;
  cdecl; external LuaDLL;
function lua_typename(L : Plua_State; tp : Integer) : PChar;
  cdecl; external LuaDLL;

function lua_equal(L : Plua_State; idx1, idx2 : Integer) : LongBool;
  cdecl; external LuaDLL;
function lua_rawequal(L : Plua_State; idx1, idx2 : Integer) : LongBool;
  cdecl; external LuaDLL;
function lua_lessthan(L : Plua_State; idx1, idx2 : Integer) : LongBool;
  cdecl; external LuaDLL;

function lua_tonumber(L : Plua_State; idx : Integer) : lua_Number;
  cdecl; external LuaDLL;
function lua_tointeger(L : Plua_State; idx : Integer) : lua_Integer;
  cdecl; external LuaDLL;
function lua_toboolean(L : Plua_State; idx : Integer) : LongBool;
  cdecl; external LuaDLL;
function lua_tolstring(L : Plua_State; idx : Integer;
                       len : Psize_t) : PChar;
  cdecl; external LuaDLL;
function lua_objlen(L : Plua_State; idx : Integer) : size_t;
  cdecl; external LuaDLL;
function lua_tocfunction(L : Plua_State; idx : Integer) : lua_CFunction;
  cdecl; external LuaDLL;
function lua_touserdata(L : Plua_State; idx : Integer) : Pointer;
  cdecl; external LuaDLL;
function lua_tothread(L : Plua_State; idx : Integer) : Plua_State;
  cdecl; external LuaDLL;
function lua_topointer(L : Plua_State; idx : Integer) : Pointer;
  cdecl; external LuaDLL;

{$ENDIF}

{$IFDEF DINAMIC}
Tlua_isnumber=  function (L : Plua_State; idx : Integer) : LongBool;  cdecl;
Tlua_isstring=  function (L : Plua_State; idx : Integer) : LongBool;  cdecl;
Tlua_iscfunction=  function (L : Plua_State; idx : Integer) : LongBool;  cdecl;
Tlua_isuserdata=  function (L : Plua_State; idx : Integer) : LongBool;  cdecl;
Tlua_type=  function (L : Plua_State; idx : Integer) : Integer;  cdecl;
Tlua_typename=  function (L : Plua_State; tp : Integer) : PChar;  cdecl;
Tlua_equal=  function (L : Plua_State; idx1, idx2 : Integer) : LongBool;  cdecl;
Tlua_rawequal=  function (L : Plua_State; idx1, idx2 : Integer) : LongBool;  cdecl;
Tlua_lessthan=  function (L : Plua_State; idx1, idx2 : Integer) : LongBool;  cdecl;
Tlua_tonumber=  function (L : Plua_State; idx : Integer) : lua_Number;  cdecl;
Tlua_tointeger=  function (L : Plua_State; idx : Integer) : lua_Integer;  cdecl;
Tlua_toboolean=  function (L : Plua_State; idx : Integer) : LongBool;  cdecl;
Tlua_tolstring=  function (L : Plua_State; idx : Integer; len : Psize_t) : PChar;  cdecl;
Tlua_objlen=  function (L : Plua_State; idx : Integer) : size_t;  cdecl;
Tlua_tocfunction=  function (L : Plua_State; idx : Integer) : lua_CFunction;  cdecl;
Tlua_touserdata=  function (L : Plua_State; idx : Integer) : Pointer;  cdecl;
Tlua_tothread=  function (L : Plua_State; idx : Integer) : Plua_State;  cdecl;
Tlua_topointer=  function (L : Plua_State; idx : Integer) : Pointer;  cdecl;

{$ENDIF}


(*
** push functions (C -> stack)
*)
{$IFDEF STATIC}
procedure lua_pushnil(L : Plua_State);
  cdecl; external LuaDLL;
procedure lua_pushnumber(L : Plua_State; n : lua_Number);
  cdecl; external LuaDLL;
procedure lua_pushinteger(L : Plua_State; n : lua_Integer);
  cdecl; external LuaDLL;
procedure lua_pushlstring(L : Plua_State; const s : PChar; ls : size_t);
  cdecl; external LuaDLL;
procedure lua_pushstring(L : Plua_State; const s : PChar);
  cdecl; external LuaDLL;
function  lua_pushvfstring(L : Plua_State;
                           const fmt : PChar; argp : Pointer) : PChar;
  cdecl; external LuaDLL;
function  lua_pushfstring(L : Plua_State; const fmt : PChar) : PChar; varargs;
  cdecl; external LuaDLL;
procedure lua_pushcclosure(L : Plua_State; fn : lua_CFunction; n : Integer);
  cdecl; external LuaDLL;
procedure lua_pushboolean(L : Plua_State; b : LongBool);
  cdecl; external LuaDLL;
procedure lua_pushlightuserdata(L : Plua_State; p : Pointer);
  cdecl; external LuaDLL;
function  lua_pushthread(L : Plua_state) : Cardinal;
  cdecl; external LuaDLL;
{$ENDIF}

{$IFDEF DINAMIC}
Tlua_pushnil=  procedure (L : Plua_State);  cdecl;
Tlua_pushnumber=  procedure (L : Plua_State; n : lua_Number);  cdecl;
Tlua_pushinteger=  procedure (L : Plua_State; n : lua_Integer);  cdecl;
Tlua_pushlstring=  procedure (L : Plua_State; const s : PChar; ls : size_t);  cdecl;
Tlua_pushstring=  procedure (L : Plua_State; const s : PChar);  cdecl;
Tlua_pushvfstring=  function  (L : Plua_State; const fmt : PChar; argp : Pointer) : PChar;  cdecl;
Tlua_pushfstring=  function  (L : Plua_State; const fmt : PChar) : PChar; {varargs;}  cdecl;
Tlua_pushcclosure=  procedure (L : Plua_State; fn : lua_CFunction; n : Integer);  cdecl;
Tlua_pushboolean=  procedure (L : Plua_State; b : LongBool);  cdecl;
Tlua_pushlightuserdata=  procedure (L : Plua_State; p : Pointer);  cdecl;
Tlua_pushthread=  function  (L : Plua_state) : Cardinal;  cdecl;
{$ENDIF}


(*
** get functions (Lua -> stack)
*)
{$IFDEF STATIC}
procedure lua_gettable(L : Plua_State ; idx : Integer);
  cdecl; external LuaDLL;
procedure lua_getfield(L : Plua_State; idx : Integer; k : PChar);
  cdecl; external LuaDLL;
procedure lua_rawget(L : Plua_State; idx : Integer);
  cdecl; external LuaDLL;
procedure lua_rawgeti(L : Plua_State; idx, n : Integer);
  cdecl; external LuaDLL;
procedure lua_createtable(L : Plua_State; narr, nrec : Integer);
  cdecl; external LuaDLL;
function  lua_newuserdata(L : Plua_State; sz : size_t) : Pointer;
  cdecl; external LuaDLL;
function  lua_getmetatable(L : Plua_State; objindex : Integer) : LongBool;
  cdecl; external LuaDLL;
procedure lua_getfenv(L : Plua_State; idx : Integer);
  cdecl; external LuaDLL;
{$ENDIF}

{$IFDEF DINAMIC}
Tlua_gettable=  procedure (L : Plua_State ; idx : Integer);  cdecl;
Tlua_getfield=  procedure (L : Plua_State; idx : Integer; k : PChar);  cdecl;
Tlua_rawget=  procedure (L : Plua_State; idx : Integer);  cdecl;
Tlua_rawgeti=  procedure (L : Plua_State; idx, n : Integer);  cdecl;
Tlua_createtable=  procedure (L : Plua_State; narr, nrec : Integer);  cdecl;
Tlua_newuserdata=  function  (L : Plua_State; sz : size_t) : Pointer;  cdecl;
Tlua_getmetatable=  function  (L : Plua_State; objindex : Integer) : LongBool;  cdecl;
Tlua_getfenv=  procedure (L : Plua_State; idx : Integer);  cdecl;

{$ENDIF}


(*
** set functions (stack -> Lua)
*)
{$IFDEF STATIC}
procedure lua_settable(L : Plua_State; idx : Integer);
  cdecl; external LuaDLL;
procedure lua_setfield(L : Plua_State; idx : Integer; const k : PChar);
  cdecl; external LuaDLL;
procedure lua_rawset(L : Plua_State; idx : Integer);
  cdecl; external LuaDLL;
procedure lua_rawseti(L : Plua_State; idx , n: Integer);
  cdecl; external LuaDLL;
function lua_setmetatable(L : Plua_State; objindex : Integer): LongBool;
  cdecl; external LuaDLL;
function lua_setfenv(L : Plua_State; idx : Integer): LongBool;
  cdecl; external LuaDLL;
{$ENDIF}

{$IFDEF DINAMIC}
Tlua_settable=  procedure (L : Plua_State; idx : Integer);  cdecl;
Tlua_setfield=  procedure (L : Plua_State; idx : Integer; const k : PChar);  cdecl;
Tlua_rawset=  procedure (L : Plua_State; idx : Integer);  cdecl;
Tlua_rawseti=  procedure (L : Plua_State; idx , n: Integer);  cdecl;
Tlua_setmetatable=  function (L : Plua_State; objindex : Integer): LongBool;  cdecl;
Tlua_setfenv=  function (L : Plua_State; idx : Integer): LongBool;  cdecl;
{$ENDIF}

(*
** `load' and `call' functions (load and run Lua code)
*)
{$IFDEF STATIC}
procedure lua_call(L : Plua_State; nargs, nresults : Integer);
  cdecl; external LuaDLL;
function  lua_pcall(L : Plua_State;
                    nargs, nresults, errfunc : Integer) : Integer;
  cdecl; external LuaDLL;
function  lua_cpcall(L : Plua_State;
                     func : lua_CFunction; ud : Pointer) : Integer;
  cdecl; external LuaDLL;
function  lua_load(L : Plua_State; reader : lua_Reader;
                   dt : Pointer; const chunkname : PChar) : Integer;
  cdecl; external LuaDLL;

function lua_dump(L : Plua_State; writer : lua_Writer; data: Pointer) : Integer;
  cdecl; external LuaDLL;

{$ENDIF}

{$IFDEF DINAMIC}
Tlua_call=  procedure (L : Plua_State; nargs, nresults : Integer);  cdecl;
Tlua_pcall=  function  (L : Plua_State; nargs, nresults, errfunc : Integer) : Integer;  cdecl;
Tlua_cpcall=  function  (L : Plua_State; func : lua_CFunction; ud : Pointer) : Integer;  cdecl;
Tlua_load=  function  (L : Plua_State; reader : lua_Reader; dt : Pointer; const chunkname : PChar) : Integer;  cdecl;
Tlua_dump=  function (L : Plua_State; writer : lua_Writer; data: Pointer) : Integer; cdecl;
{$ENDIF}


(*
** coroutine functions
*)
{$IFDEF STATIC}
function lua_yield(L : Plua_State; nresults : Integer) : Integer;
  cdecl; external LuaDLL;
function lua_resume(L : Plua_State; narg : Integer) : Integer;
  cdecl; external LuaDLL;
function lua_status(L : Plua_State) : Integer;
  cdecl; external LuaDLL;
{$ENDIF}

{$IFDEF DINAMIC}
Tlua_yield=  function (L : Plua_State; nresults : Integer) : Integer;  cdecl;
Tlua_resume=  function (L : Plua_State; narg : Integer) : Integer;  cdecl;
Tlua_status=  function (L : Plua_State) : Integer;  cdecl;
{$ENDIF}

(*
** garbage-collection functions and options
*)
const
  LUA_GCSTOP       = 0;
  LUA_GCRESTART    = 1;
  LUA_GCCOLLECT    = 2;
  LUA_GCCOUNT      = 3;
  LUA_GCCOUNTB	   = 4;
  LUA_GCSTEP       = 5;
  LUA_GCSETPAUSE   = 6;
  LUA_GCSETSTEPMUL = 7;

{$IFDEF STATIC}
function lua_gc(L : Plua_State; what, data : Integer) : Integer;
  cdecl; external LuaDLL;
{$ENDIF}

{$IFDEF DINAMIC}
type Tlua_gc = function(L : Plua_State; what, data : Integer):Integer; cdecl;
{$ENDIF}

(*
** miscellaneous functions
*)
{$IFDEF STATIC}
function lua_error(L : Plua_State) : Integer;
  cdecl; external LuaDLL;

function lua_next(L : Plua_State; idx : Integer) : Integer;
  cdecl; external LuaDLL;

procedure lua_concat(L : Plua_State; n : Integer);
  cdecl; external LuaDLL;

function  lua_getallocf(L : Plua_State; ud : PPointer) : lua_Alloc;
  cdecl; external LuaDLL;
procedure lua_setallocf(L : Plua_State; f : lua_Alloc; ud : Pointer);
  cdecl; external LuaDLL;
{$ENDIF}

{$IFDEF DINAMIC}
Tlua_error=  function (L : Plua_State) : Integer;  cdecl;
Tlua_next=  function (L : Plua_State; idx : Integer) : Integer;  cdecl;
Tlua_concat=  procedure (L : Plua_State; n : Integer);  cdecl;
Tlua_getallocf= function  (L : Plua_State; ud : PPointer) : lua_Alloc;  cdecl;
Tlua_setallocf=  procedure (L : Plua_State; f : lua_Alloc; ud : Pointer);  cdecl;
{$ENDIF}

(*
** ===============================================================
** some useful macros
** ===============================================================
*)
procedure lua_pop(L : Plua_State; n : Integer);

procedure lua_newtable(L : Plua_State);

procedure lua_register(L : Plua_State; n : PChar; f : lua_CFunction);

procedure lua_pushcfunction(L : Plua_State; f : lua_CFunction);

function  lua_strlen(L : Plua_State; idx : Integer) : Integer;

function lua_isfunction(L : Plua_State; n : Integer) : Boolean;
function lua_istable(L : Plua_State; n : Integer) : Boolean;
function lua_islightuserdata(L : Plua_State; n : Integer) : Boolean;
function lua_isnil(L : Plua_State; n : Integer) : Boolean;
function lua_isboolean(L : Plua_State; n : Integer) : Boolean;
function lua_isthread(L : Plua_State; n : Integer) : Boolean;
function lua_isnone(L : Plua_State; n : Integer) : Boolean;
function lua_isnoneornil(L : Plua_State; n : Integer) : Boolean;

procedure lua_pushliteral(L : Plua_State; s : PChar);

procedure lua_setglobal(L : Plua_State; s : PChar);
procedure lua_getglobal(L : Plua_State; s : PChar);

function lua_tostring(L : Plua_State; idx : Integer) : PChar;


(*
** compatibility macros and functions
*)
function lua_open : Plua_State;

procedure lua_getregistry(L : Plua_State);

function lua_getgccount(L : Plua_State) : Integer;

type
  lua_Chuckreader = type lua_Reader;
  lua_Chuckwriter = type lua_Writer;

(* ====================================================================== *)

(*
** {======================================================================
** Debug API
** =======================================================================
*)

(*
** Event codes
*)
const
  LUA_HOOKCALL    = 0;
  LUA_HOOKRET     = 1;
  LUA_HOOKLINE    = 2;
  LUA_HOOKCOUNT   = 3;
  LUA_HOOKTAILRET = 4;


(*
** Event masks
*)
  LUA_MASKCALL  = 1 shl LUA_HOOKCALL;
  LUA_MASKRET   = 1 shl LUA_HOOKRET;
  LUA_MASKLINE  = 1 shl LUA_HOOKLINE;
  LUA_MASKCOUNT = 1 shl LUA_HOOKCOUNT;

type
  lua_Debug = packed record
    event : Integer;
    name : PChar;          (* (n) *)
    namewhat : PChar;      (* (n) `global', `local', `field', `method' *)
    what : PChar;          (* (S) `Lua', `C', `main', `tail' *)
    source : PChar;        (* (S) *)
    currentline : Integer; (* (l) *)
    nups : Integer;        (* (u) number of upvalues *)
    linedefined : Integer; (* (S) *)
    short_src : array [0..LUA_IDSIZE-1] of Char; (* (S) *)
    (* private part *)
    i_ci : Integer;        (* active function *)
  end;
  Plua_Debug = ^lua_Debug;

  (* Functions to be called by the debuger in specific events *)
  lua_Hook = procedure (L : Plua_State; ar : Plua_Debug); cdecl;

{$IFDEF STATIC}
function lua_getstack(L : Plua_State; level : Integer;
                      ar : Plua_Debug) : Integer;
  cdecl; external LuaDLL;
function lua_getinfo(L : Plua_State; const what : PChar;
                     ar: Plua_Debug): Integer;
  cdecl; external LuaDLL;
function lua_getlocal(L : Plua_State;
                      ar : Plua_Debug; n : Integer) : PChar;
  cdecl; external LuaDLL;
function lua_setlocal(L : Plua_State;
                      ar : Plua_Debug; n : Integer) : PChar;
  cdecl; external LuaDLL;
function lua_getupvalue(L : Plua_State; funcindex, n : Integer) : PChar;
  cdecl; external LuaDLL;
function lua_setupvalue(L : Plua_State; funcindex, n : Integer) : PChar;
  cdecl; external LuaDLL;

function lua_sethook(L : Plua_State; func : lua_Hook;
                     mask, count: Integer): Integer;
  cdecl; external LuaDLL;
{
function lua_gethook(L : Plua_State) : lua_Hook;
  cdecl; external LuaDLL;
}
function lua_gethookmask(L : Plua_State) : Integer;
  cdecl; external LuaDLL;
function lua_gethookcount(L : Plua_State) : Integer;
  cdecl; external LuaDLL;
{$ENDIF}

{$IFDEF DINAMIC}
type
Tlua_getstack=  function (L : Plua_State; level : Integer; ar : Plua_Debug) : Integer;  cdecl;
Tlua_getinfo=  function (L : Plua_State; const what : PChar; ar: Plua_Debug): Integer;  cdecl;
Tlua_getlocal=  function (L : Plua_State; ar : Plua_Debug; n : Integer) : PChar;  cdecl;
Tlua_setlocal=  function (L : Plua_State; ar : Plua_Debug; n : Integer) : PChar;  cdecl;
Tlua_getupvalue=  function (L : Plua_State; funcindex, n : Integer) : PChar;  cdecl;
Tlua_setupvalue=  function (L : Plua_State; funcindex, n : Integer) : PChar;  cdecl;
Tlua_sethook=  function (L : Plua_State; func : lua_Hook; mask, count: Integer): Integer;  cdecl;
{function lua_gethook(L : Plua_State) : lua_Hook;  cdecl;}
Tlua_gethookmask=  function (L : Plua_State) : Integer;  cdecl;
Tlua_gethookcount=  function (L : Plua_State) : Integer;  cdecl;
{$ENDIF}

(*****************************************************************************)
(*                                  lualib.h                                 *)
(*****************************************************************************)

(*
** $Id: lualib.h,v 1.36 2005/12/27 17:12:00 roberto Exp $
** Lua standard libraries
** See Copyright Notice at the end of this file
*)

const
  (* Key to file-handle type *)
  LUA_FILEHANDLE  = 'FILE*';

  LUA_COLIBNAME   = 'coroutine';
  LUA_TABLIBNAME  = 'table';
  LUA_IOLIBNAME   = 'io';
  LUA_OSLIBNAME   = 'os';
  LUA_STRLIBNAME  = 'string';
  LUA_MATHLIBNAME = 'math';
  LUA_DBLIBNAME   = 'debug';
  LUA_LOADLIBNAME = 'package';

{$IFDEF STATIC}
function luaopen_base(L : Plua_State) : Integer;
  cdecl; external LuaDLL;

function luaopen_table(L : Plua_State) : Integer;
  cdecl; external LuaDLL;

function luaopen_io(L : Plua_State) : Integer;
  cdecl; external LuaDLL;

function luaopen_os(L : Plua_State) : Integer;
  cdecl; external LuaDLL;

function luaopen_string(L : Plua_State) : Integer;
  cdecl; external LuaDLL;

function luaopen_math(L : Plua_State) : Integer;
  cdecl; external LuaDLL;

function luaopen_debug(L : Plua_State) : Integer;
  cdecl; external LuaDLL;

function luaopen_package(L : Plua_State) : Integer;
  cdecl; external LuaDLL;

procedure luaL_openlibs(L : Plua_State);
  cdecl; external LuaDLL;
{$ENDIF}

{$IFDEF DINAMIC}
type
Tluaopen_base=  function (L : Plua_State) : Integer;  cdecl;
Tluaopen_table=  function (L : Plua_State) : Integer;  cdecl;
Tluaopen_io=  function (L : Plua_State) : Integer;  cdecl;
Tluaopen_os=  function (L : Plua_State) : Integer;  cdecl;
Tluaopen_string=  function (L : Plua_State) : Integer;  cdecl;
Tluaopen_math=  function (L : Plua_State) : Integer;  cdecl;
Tluaopen_debug=  function (L : Plua_State) : Integer;  cdecl;
Tluaopen_package=  function (L : Plua_State) : Integer;  cdecl;
TluaL_openlibs=  procedure (L : Plua_State);  cdecl;
{$ENDIF}

procedure lua_assert(x : Boolean);    // a macro


(*****************************************************************************)
(*                                  lauxlib.h                                *)
(*****************************************************************************)

(*
** $Id: lauxlib.h,v 1.87 2005/12/29 15:32:11 roberto Exp $
** Auxiliary functions for building Lua libraries
** See Copyright Notice at the end of this file.
*)

// not compatibility with the behavior of setn/getn in Lua 5.0
function  luaL_getn(L : Plua_State; idx : Integer) : Integer;
procedure luaL_setn(L : Plua_State; i, j : Integer);

const
  LUA_ERRFILE = LUA_ERRERR + 1;

type
  luaL_Reg = packed record
    name : PChar;
    func : lua_CFunction;
  end;
  PluaL_Reg = ^luaL_Reg;

{$IFDEF STATIC}
procedure luaL_openlib(L : Plua_State; const libname : PChar;
                       const lr : PluaL_Reg; nup : Integer);
  cdecl; external LuaDLL;
procedure luaL_register(L : Plua_State; const libname : PChar;
                       const lr : PluaL_Reg);
  cdecl; external LuaDLL;
function luaL_getmetafield(L : Plua_State; obj : Integer;
                           const e : PChar) : Integer;
  cdecl; external LuaDLL;
function luaL_callmeta(L : Plua_State; obj : Integer;
                       const e : PChar) : Integer;
  cdecl; external LuaDLL;
function luaL_typerror(L : Plua_State; narg : Integer;
                       const tname : PChar) : Integer;
  cdecl; external LuaDLL;
function luaL_argerror(L : Plua_State; numarg : Integer;
                       const extramsg : PChar) : Integer;
  cdecl; external LuaDLL;
function luaL_checklstring(L : Plua_State; numArg : Integer;
                           ls : Psize_t) : PChar;
  cdecl; external LuaDLL;
function luaL_optlstring(L : Plua_State; numArg : Integer;
                         const def: PChar; ls: Psize_t) : PChar;
  cdecl; external LuaDLL;
function luaL_checknumber(L : Plua_State; numArg : Integer) : lua_Number;
  cdecl; external LuaDLL;
function luaL_optnumber(L : Plua_State; nArg : Integer;
                        def : lua_Number) : lua_Number;
  cdecl; external LuaDLL;

function luaL_checkinteger(L : Plua_State; numArg : Integer) : lua_Integer;
  cdecl; external LuaDLL;
function luaL_optinteger(L : Plua_State; nArg : Integer;
                        def : lua_Integer) : lua_Integer;
  cdecl; external LuaDLL;

procedure luaL_checkstack(L : Plua_State; sz : Integer; const msg : PChar);
  cdecl; external LuaDLL;
procedure luaL_checktype(L : Plua_State; narg, t : Integer);
  cdecl; external LuaDLL;
procedure luaL_checkany(L : Plua_State; narg : Integer);
  cdecl; external LuaDLL;

function luaL_newmetatable(L : Plua_State; const tname : PChar) : Integer;
  cdecl; external LuaDLL;
function luaL_checkudata(L : Plua_State; ud : Integer;
                         const tname : PChar) : Pointer;
  cdecl; external LuaDLL;

procedure luaL_where(L : Plua_State; lvl : Integer);
  cdecl; external LuaDLL;
function  luaL_error(L : Plua_State; const fmt : PChar) : Integer; varargs;
  cdecl; external LuaDLL;

function luaL_checkoption(L : Plua_State; narg : Integer; const def : PChar;
                          const lst : array of PChar) : Integer;
  cdecl; external LuaDLL;

function  luaL_ref(L : Plua_State; t : Integer) : Integer;
  cdecl; external LuaDLL;
procedure luaL_unref(L : Plua_State; t, ref : Integer);
  cdecl; external LuaDLL;

function luaL_loadfile(L : Plua_State; const filename : PChar) : Integer;
  cdecl; external LuaDLL;
function luaL_loadbuffer(L : Plua_State; const buff : PChar;
                         sz : size_t; const name: PChar) : Integer;
  cdecl; external LuaDLL;

function luaL_loadstring(L : Plua_State; const s : Pchar) : Integer;
  cdecl; external LuaDLL;

function luaL_newstate : Plua_State;
  cdecl; external LuaDLL;

function luaL_gsub(L : Plua_State; const s, p, r : PChar) : PChar;
  cdecl; external LuaDLL;

function luaL_findtable(L : Plua_State; idx : Integer;
                        const fname : PChar; szhint : Integer) : PChar;
  cdecl; external LuaDLL;
{$ENDIF}

{$IFDEF DINAMIC}
type
TluaL_openlib=  procedure (L : Plua_State; const libname : PChar; const lr : PluaL_Reg; nup : Integer);  cdecl;
TluaL_register=  procedure (L : Plua_State; const libname : PChar; const lr : PluaL_Reg);  cdecl;
TluaL_getmetafield=  function (L : Plua_State; obj : Integer; const e : PChar) : Integer;  cdecl;
TluaL_callmeta=  function (L : Plua_State; obj : Integer; const e : PChar) : Integer;  cdecl;
TluaL_typerror=  function (L : Plua_State; narg : Integer; const tname : PChar) : Integer;  cdecl;
TluaL_argerror=  function (L : Plua_State; numarg : Integer; const extramsg : PChar) : Integer;  cdecl;
TluaL_checklstring=  function (L : Plua_State; numArg : Integer; ls : Psize_t) : PChar;  cdecl;
TluaL_optlstring=  function (L : Plua_State; numArg : Integer; const def: PChar; ls: Psize_t) : PChar;  cdecl;
TluaL_checknumber=  function (L : Plua_State; numArg : Integer) : lua_Number;  cdecl;
TluaL_optnumber=  function (L : Plua_State; nArg : Integer; def : lua_Number) : lua_Number;  cdecl;
TluaL_checkinteger=  function (L : Plua_State; numArg : Integer) : lua_Integer; cdecl;
TluaL_optinteger=  function (L : Plua_State; nArg : Integer; def : lua_Integer) : lua_Integer;  cdecl;
TluaL_checkstack=  procedure (L : Plua_State; sz : Integer; const msg : PChar); cdecl;
TluaL_checktype=  procedure (L : Plua_State; narg, t : Integer);  cdecl;
TluaL_checkany=  procedure (L : Plua_State; narg : Integer);  cdecl;
TluaL_newmetatable=  function (L : Plua_State; const tname : PChar) : Integer;  cdecl;
TluaL_checkudata=  function (L : Plua_State; ud : Integer; const tname : PChar) : Pointer;  cdecl;
TluaL_where=  procedure (L : Plua_State; lvl : Integer);  cdecl;
TluaL_error=  function  (L : Plua_State; const fmt : PChar) : Integer; varargs;  cdecl;
TluaL_checkoption=  function (L : Plua_State; narg : Integer; const def : PChar;  const lst : array of PChar) : Integer;  cdecl;
TluaL_ref=  function  (L : Plua_State; t : Integer) : Integer;  cdecl;
TluaL_unref=  procedure (L : Plua_State; t, ref : Integer);  cdecl;
TluaL_loadfile=  function (L : Plua_State; const filename : PChar) : Integer;  cdecl;
TluaL_loadbuffer=  function (L : Plua_State; const buff : PChar; sz : size_t; const name: PChar) : Integer;  cdecl;
TluaL_loadstring=  function (L : Plua_State; const s : Pchar) : Integer; cdecl;
TluaL_newstate=  function  : Plua_State;  cdecl;
TluaL_gsub=  function (L : Plua_State; const s, p, r : PChar) : PChar;  cdecl;
TluaL_findtable=  function (L : Plua_State; idx : Integer; const fname : PChar; szhint : Integer) : PChar;  cdecl;
{$ENDIF}

(*
** ===============================================================
** some useful macros
** ===============================================================
*)

function luaL_argcheck(L : Plua_State; cond : Boolean; numarg : Integer;
                       extramsg : PChar): Integer;
function luaL_checkstring(L : Plua_State; n : Integer) : PChar;
function luaL_optstring(L : Plua_State; n : Integer; d : PChar) : PChar;
function luaL_checkint(L : Plua_State; n : Integer) : Integer;
function luaL_optint(L : Plua_State; n, d : Integer): Integer;
function luaL_checklong(L : Plua_State; n : LongInt) : LongInt;
function luaL_optlong(L : Plua_State; n : Integer; d : LongInt) : LongInt;

function luaL_typename(L : Plua_State; idx : Integer) : PChar;

function luaL_dofile(L : Plua_State; fn : PChar) : Integer;

function luaL_dostring(L : Plua_State; s : PChar) : Integer;

procedure luaL_getmetatable(L : Plua_State; n : PChar);

(* not implemented yet
#define luaL_opt(L,f,n,d) (lua_isnoneornil(L,(n)) ? (d) : f(L,(n)))
*)

(*
** {======================================================
** Generic Buffer manipulation
** =======================================================
*)

type
  luaL_Buffer = packed record
    p : PChar;       (* current position in buffer *)
    lvl : Integer;   (* number of strings in the stack (level) *)
    L : Plua_State;
    buffer : array [0..LUAL_BUFFERSIZE-1] of Char;
  end;
  PluaL_Buffer = ^luaL_Buffer;

procedure luaL_addchar(B : PluaL_Buffer; c : Char);

(* compatibility only *)
procedure luaL_putchar(B : PluaL_Buffer; c : Char);

procedure luaL_addsize(B : PluaL_Buffer; n : Integer);

{$IFDEF STATIC}
procedure luaL_buffinit(L : Plua_State; B : PluaL_Buffer);
  cdecl; external LuaDLL;
function  luaL_prepbuffer(B : PluaL_Buffer) : PChar;
  cdecl; external LuaDLL;
procedure luaL_addlstring(B : PluaL_Buffer; const s : PChar; ls : size_t);
  cdecl; external LuaDLL;
procedure luaL_addstring(B : PluaL_Buffer; const s : PChar);
  cdecl; external LuaDLL;
procedure luaL_addvalue(B : PluaL_Buffer);
  cdecl; external LuaDLL;
procedure luaL_pushresult(B : PluaL_Buffer);
  cdecl; external LuaDLL;
{$ENDIF}

{$IFDEF DINAMIC}
type
TluaL_buffinit=  procedure (L : Plua_State; B : PluaL_Buffer);  cdecl;
TluaL_prepbuffer=  function  (B : PluaL_Buffer) : PChar;  cdecl;
TluaL_addlstring=  procedure (B : PluaL_Buffer; const s : PChar; ls : size_t);  cdecl;
TluaL_addstring=  procedure (B : PluaL_Buffer; const s : PChar);  cdecl;
TluaL_addvalue=  procedure (B : PluaL_Buffer);  cdecl;
TluaL_pushresult=  procedure (B : PluaL_Buffer);  cdecl;
{$ENDIF}

(* ====================================================== *)


(* compatibility with ref system *)

(* pre-defined references *)
const
  LUA_NOREF  = -2;
  LUA_REFNIL = -1;

function lua_ref(L : Plua_State; lock : Boolean) : Integer;

procedure lua_unref(L : Plua_State; ref : Integer);

procedure lua_getref(L : Plua_State; ref : Integer);


(******************************************************************************)
(******************************************************************************)
(******************************************************************************)

{$IFDEF DINAMIC}
type TLibHandle = PtrInt;
    var
  lua_newstate:Tlua_newstate;
  lua_close:Tlua_close;
  lua_newthread:Tlua_newthread;
  lua_atpanic:Tlua_atpanic;
  luaL_buffinit:TluaL_buffinit;
  luaL_prepbuffer:TluaL_prepbuffer;
  luaL_addlstring:TluaL_addlstring;
  luaL_addstring:TluaL_addstring;
  luaL_addvalue:TluaL_addvalue;
  luaL_pushresult:TluaL_pushresult;
  luaL_openlib:TluaL_openlib;
  luaL_register:TluaL_register;
  luaL_getmetafield:TluaL_getmetafield;
  luaL_callmeta:TluaL_callmeta;
  luaL_typerror:TluaL_typerror;
  luaL_argerror:TluaL_argerror;
  luaL_checklstring:TluaL_checklstring;
  luaL_optlstring:TluaL_optlstring;
  luaL_checknumber:TluaL_checknumber;
  luaL_optnumber:TluaL_optnumber;
  luaL_checkinteger:TluaL_checkinteger;
  luaL_optinteger:TluaL_optinteger;
  luaL_checkstack:TluaL_checkstack;
  luaL_checktype:TluaL_checktype;
  luaL_checkany:TluaL_checkany;
  luaL_newmetatable:TluaL_newmetatable;
  luaL_checkudata:TluaL_checkudata;
  luaL_where:TluaL_where;
  luaL_error:TluaL_error;
  luaL_checkoption:TluaL_checkoption;
  luaL_ref:TluaL_ref;
  luaL_unref:TluaL_unref;
  luaL_loadfile:TluaL_loadfile;
  luaL_loadbuffer:TluaL_loadbuffer;
  luaL_loadstring:TluaL_loadstring;
  luaL_newstate:TluaL_newstate;
  luaL_gsub:TluaL_gsub;
  luaL_findtable:TluaL_findtable;
  luaopen_base:Tluaopen_base;
  luaopen_table:Tluaopen_table;
  luaopen_io:Tluaopen_io;
  luaopen_os:Tluaopen_os;
  luaopen_string:Tluaopen_string;
  luaopen_math:Tluaopen_math;
  luaopen_debug:Tluaopen_debug;
  luaopen_package:Tluaopen_package;
  luaL_openlibs:TluaL_openlibs;
  lua_getstack:Tlua_getstack;
  lua_getinfo:Tlua_getinfo;
  lua_getlocal:Tlua_getlocal;
  lua_setlocal:Tlua_setlocal;
  lua_getupvalue:Tlua_getupvalue;
  lua_setupvalue:Tlua_setupvalue;
  lua_sethook:Tlua_sethook;
  {function lua_gethook(L : Plua_State) : lua_Hook;  cdecl;}
  lua_gethookmask:Tlua_gethookmask;
  lua_gethookcount:Tlua_gethookcount;
  lua_error:Tlua_error;
  lua_next:Tlua_next;
  lua_concat:Tlua_concat;
  lua_getallocf:Tlua_getallocf;
  lua_setallocf:Tlua_setallocf;
  lua_gc:Tlua_gc;
  lua_yield:Tlua_yield;
  lua_resume:Tlua_resume;
  lua_status:Tlua_status;
  lua_call:Tlua_call;
  lua_pcall:Tlua_pcall;
  lua_cpcall:Tlua_cpcall;
  lua_load:Tlua_load;
  lua_dump:Tlua_dump;
  lua_settable:Tlua_settable;
  lua_setfield:Tlua_setfield;
  lua_rawset:Tlua_rawset;
  lua_rawseti:Tlua_rawseti;
  lua_setmetatable:Tlua_setmetatable;
  lua_setfenv:Tlua_setfenv;
  lua_gettable:Tlua_gettable;
  lua_getfield:Tlua_getfield;
  lua_rawget:Tlua_rawget;
  lua_rawgeti:Tlua_rawgeti;
  lua_createtable:Tlua_createtable;
  lua_newuserdata:Tlua_newuserdata;
  lua_getmetatable:Tlua_getmetatable;
  lua_getfenv:Tlua_getfenv;
  lua_pushnil:Tlua_pushnil;
  lua_pushnumber:Tlua_pushnumber;
  lua_pushinteger:Tlua_pushinteger;
  lua_pushlstring:Tlua_pushlstring;
  lua_pushstring:Tlua_pushstring;
  lua_pushvfstring:Tlua_pushvfstring;
  lua_pushfstring:Tlua_pushfstring;
  lua_pushcclosure:Tlua_pushcclosure;
  lua_pushboolean:Tlua_pushboolean;
  lua_pushlightuserdata:Tlua_pushlightuserdata;
  lua_pushthread:Tlua_pushthread;
  lua_isnumber:Tlua_isnumber;
  lua_isstring:Tlua_isstring;
  lua_iscfunction:Tlua_iscfunction;
  lua_isuserdata:Tlua_isuserdata;
  lua_type:Tlua_type;
  lua_typename:Tlua_typename;
  lua_equal:Tlua_equal;
  lua_rawequal:Tlua_rawequal;
  lua_lessthan:Tlua_lessthan;
  lua_tonumber:Tlua_tonumber;
  lua_tointeger:Tlua_tointeger;
  lua_toboolean:Tlua_toboolean;
  lua_tolstring:Tlua_tolstring;
  lua_objlen:Tlua_objlen;
  lua_tocfunction:Tlua_tocfunction;
  lua_touserdata:Tlua_touserdata;
  lua_tothread:Tlua_tothread;
  lua_topointer:Tlua_topointer;
  lua_gettop:Tlua_gettop;
  lua_settop:Tlua_settop;
  lua_pushvalue:Tlua_pushvalue;
  lua_remove:Tlua_remove;
  lua_insert:Tlua_insert;
  lua_replace:Tlua_replace;
  lua_checkstack:Tlua_checkstack;
  lua_xmove:Tlua_xmove;

  LuaLibD:TLibHandle;
{$ENDIF}

implementation

uses
  SysUtils
  {$IFDEF DINAMIC}
   ,dynlibs
  {$ENDIF}
;


{$IFDEF DINAMIC}
  procedure UnloadLuaLib;
  begin
   if LuaLibD<>0 then
     FreeLibrary(LuaLibD);
  end;

  function IsLuaLibLoaded: boolean;
  begin
    result:= (LuaLibD<>0);
  end;

 function LoadLuaLib(filename:string):boolean;
  begin
   result:=false;
   LuaLibD:=LoadLibrary(FileName);
   result:= (LuaLibD<>0);
   if LuaLibD=0 then exit;

  lua_newstate:=Tlua_newstate(GetProcAddress(LuaLibD,'lua_newstate'));
  lua_close:=Tlua_close(GetProcAddress(LuaLibD,'lua_close'));
  lua_newthread:=Tlua_newthread(GetProcAddress(LuaLibD,'lua_newthread'));
  lua_atpanic:=Tlua_atpanic(GetProcAddress(LuaLibD,'lua_atpanic'));
  luaL_buffinit:=TluaL_buffinit(GetProcAddress(LuaLibD,'luaL_buffinit'));
  luaL_prepbuffer:=TluaL_prepbuffer(GetProcAddress(LuaLibD,'luaL_prepbuffer'));
  luaL_addlstring:=TluaL_addlstring(GetProcAddress(LuaLibD,'luaL_addlstring'));
  luaL_addstring:=TluaL_addstring(GetProcAddress(LuaLibD,'luaL_addstring'));
  luaL_addvalue:=TluaL_addvalue(GetProcAddress(LuaLibD,'luaL_addvalue'));
  luaL_pushresult:=TluaL_pushresult(GetProcAddress(LuaLibD,'luaL_pushresult'));
  luaL_openlib:=TluaL_openlib(GetProcAddress(LuaLibD,'luaL_openlib'));
  luaL_register:=TluaL_register(GetProcAddress(LuaLibD,'luaL_register'));
  luaL_getmetafield:=TluaL_getmetafield(GetProcAddress(LuaLibD,'luaL_getmetafield'));
  luaL_callmeta:=TluaL_callmeta(GetProcAddress(LuaLibD,'luaL_callmeta'));
  luaL_typerror:=TluaL_typerror(GetProcAddress(LuaLibD,'luaL_typerror'));
  luaL_argerror:=TluaL_argerror(GetProcAddress(LuaLibD,'luaL_argerror'));
  luaL_checklstring:=TluaL_checklstring(GetProcAddress(LuaLibD,'luaL_checklstring'));
  luaL_optlstring:=TluaL_optlstring(GetProcAddress(LuaLibD,'luaL_optlstring'));
  luaL_checknumber:=TluaL_checknumber(GetProcAddress(LuaLibD,'luaL_checknumber'));
  luaL_optnumber:=TluaL_optnumber(GetProcAddress(LuaLibD,'luaL_optnumber'));
  luaL_checkinteger:=TluaL_checkinteger(GetProcAddress(LuaLibD,'luaL_checkinteger'));
  luaL_optinteger:=TluaL_optinteger(GetProcAddress(LuaLibD,'luaL_optinteger'));
  luaL_checkstack:=TluaL_checkstack(GetProcAddress(LuaLibD,'luaL_checkstack'));
  luaL_checktype:=TluaL_checktype(GetProcAddress(LuaLibD,'luaL_checktype'));
  luaL_checkany:=TluaL_checkany(GetProcAddress(LuaLibD,'luaL_checkany'));
  luaL_newmetatable:=TluaL_newmetatable(GetProcAddress(LuaLibD,'luaL_newmetatable'));
  luaL_checkudata:=TluaL_checkudata(GetProcAddress(LuaLibD,'luaL_checkudata'));
  luaL_where:=TluaL_where(GetProcAddress(LuaLibD,'luaL_where'));
  luaL_error:=TluaL_error(GetProcAddress(LuaLibD,'luaL_error'));
  luaL_checkoption:=TluaL_checkoption(GetProcAddress(LuaLibD,'luaL_checkoption'));
  luaL_ref:=TluaL_ref(GetProcAddress(LuaLibD,'luaL_ref'));
  luaL_unref:=TluaL_unref(GetProcAddress(LuaLibD,'luaL_unref'));
  luaL_loadfile:=TluaL_loadfile(GetProcAddress(LuaLibD,'luaL_loadfile'));
  luaL_loadbuffer:=TluaL_loadbuffer(GetProcAddress(LuaLibD,'luaL_loadbuffer'));
  luaL_loadstring:=TluaL_loadstring(GetProcAddress(LuaLibD,'luaL_loadstring'));
  luaL_newstate:=TluaL_newstate(GetProcAddress(LuaLibD,'luaL_newstate'));
  luaL_gsub:=TluaL_gsub(GetProcAddress(LuaLibD,'luaL_gsub'));
  luaL_findtable:=TluaL_findtable(GetProcAddress(LuaLibD,'luaL_findtable'));
  luaopen_base:=Tluaopen_base(GetProcAddress(LuaLibD,'luaopen_base'));
  luaopen_table:=Tluaopen_table(GetProcAddress(LuaLibD,'luaopen_table'));
  luaopen_io:=Tluaopen_io(GetProcAddress(LuaLibD,'luaopen_io'));
  luaopen_os:=Tluaopen_os(GetProcAddress(LuaLibD,'luaopen_os'));
  luaopen_string:=Tluaopen_string(GetProcAddress(LuaLibD,'luaopen_string'));
  luaopen_math:=Tluaopen_math(GetProcAddress(LuaLibD,'luaopen_math'));
  luaopen_debug:=Tluaopen_debug(GetProcAddress(LuaLibD,'luaopen_debug'));
  luaopen_package:=Tluaopen_package(GetProcAddress(LuaLibD,'luaopen_package'));
  luaL_openlibs:=TluaL_openlibs(GetProcAddress(LuaLibD,'luaL_openlibs'));
  lua_getstack:=Tlua_getstack(GetProcAddress(LuaLibD,'lua_getstack'));
  lua_getinfo:=Tlua_getinfo(GetProcAddress(LuaLibD,'lua_getinfo'));
  lua_getlocal:=Tlua_getlocal(GetProcAddress(LuaLibD,'lua_getlocal'));
  lua_setlocal:=Tlua_setlocal(GetProcAddress(LuaLibD,'lua_setlocal'));
  lua_getupvalue:=Tlua_getupvalue(GetProcAddress(LuaLibD,'lua_getupvalue'));
  lua_setupvalue:=Tlua_setupvalue(GetProcAddress(LuaLibD,'lua_setupvalue'));
  lua_sethook:=Tlua_sethook(GetProcAddress(LuaLibD,'lua_sethook'));
   //function lua_gethook(L := Plua_State) := lua_Hook;  cdecl;'));
  lua_gethookmask:=Tlua_gethookmask(GetProcAddress(LuaLibD,'lua_gethookmask'));
  lua_gethookcount:=Tlua_gethookcount(GetProcAddress(LuaLibD,'lua_gethookcount'));
  lua_error:=Tlua_error(GetProcAddress(LuaLibD,'lua_error'));
  lua_next:=Tlua_next(GetProcAddress(LuaLibD,'lua_next'));
  lua_concat:=Tlua_concat(GetProcAddress(LuaLibD,'lua_concat'));
  lua_getallocf:=Tlua_getallocf(GetProcAddress(LuaLibD,'lua_getallocf'));
  lua_setallocf:=Tlua_setallocf(GetProcAddress(LuaLibD,'lua_setallocf'));
  lua_gc:=Tlua_gc(GetProcAddress(LuaLibD,'lua_gc'));
  lua_yield:=Tlua_yield(GetProcAddress(LuaLibD,'lua_yield'));
  lua_resume:=Tlua_resume(GetProcAddress(LuaLibD,'lua_resume'));
  lua_status:=Tlua_status(GetProcAddress(LuaLibD,'lua_status'));
  lua_call:=Tlua_call(GetProcAddress(LuaLibD,'lua_call'));
  lua_pcall:=Tlua_pcall(GetProcAddress(LuaLibD,'lua_pcall'));
  lua_cpcall:=Tlua_cpcall(GetProcAddress(LuaLibD,'lua_cpcall'));
  lua_load:=Tlua_load(GetProcAddress(LuaLibD,'lua_load'));
  lua_dump:=Tlua_dump(GetProcAddress(LuaLibD,'lua_dump'));
  lua_settable:=Tlua_settable(GetProcAddress(LuaLibD,'lua_settable'));
  lua_setfield:=Tlua_setfield(GetProcAddress(LuaLibD,'lua_setfield'));
  lua_rawset:=Tlua_rawset(GetProcAddress(LuaLibD,'lua_rawset'));
  lua_rawseti:=Tlua_rawseti(GetProcAddress(LuaLibD,'lua_rawseti'));
  lua_setmetatable:=Tlua_setmetatable(GetProcAddress(LuaLibD,'lua_setmetatable'));
  lua_setfenv:=Tlua_setfenv(GetProcAddress(LuaLibD,'lua_setfenv'));
  lua_gettable:=Tlua_gettable(GetProcAddress(LuaLibD,'lua_gettable'));
  lua_getfield:=Tlua_getfield(GetProcAddress(LuaLibD,'lua_getfield'));
  lua_rawget:=Tlua_rawget(GetProcAddress(LuaLibD,'lua_rawget'));
  lua_rawgeti:=Tlua_rawgeti(GetProcAddress(LuaLibD,'lua_rawgeti'));
  lua_createtable:=Tlua_createtable(GetProcAddress(LuaLibD,'lua_createtable'));
  lua_newuserdata:=Tlua_newuserdata(GetProcAddress(LuaLibD,'lua_newuserdata'));
  lua_getmetatable:=Tlua_getmetatable(GetProcAddress(LuaLibD,'lua_getmetatable'));
  lua_getfenv:=Tlua_getfenv(GetProcAddress(LuaLibD,'lua_getfenv'));
  lua_pushnil:=Tlua_pushnil(GetProcAddress(LuaLibD,'lua_pushnil'));
  lua_pushnumber:=Tlua_pushnumber(GetProcAddress(LuaLibD,'lua_pushnumber'));
  lua_pushinteger:=Tlua_pushinteger(GetProcAddress(LuaLibD,'lua_pushinteger'));
  lua_pushlstring:=Tlua_pushlstring(GetProcAddress(LuaLibD,'lua_pushlstring'));
  lua_pushstring:=Tlua_pushstring(GetProcAddress(LuaLibD,'lua_pushstring'));
  lua_pushvfstring:=Tlua_pushvfstring(GetProcAddress(LuaLibD,'lua_pushvfstring'));
  lua_pushfstring:=Tlua_pushfstring(GetProcAddress(LuaLibD,'lua_pushfstring'));
  lua_pushcclosure:=Tlua_pushcclosure(GetProcAddress(LuaLibD,'lua_pushcclosure'));
  lua_pushboolean:=Tlua_pushboolean(GetProcAddress(LuaLibD,'lua_pushboolean'));
  lua_pushlightuserdata:=Tlua_pushlightuserdata(GetProcAddress(LuaLibD,'lua_pushlightuserdata'));
  lua_pushthread:=Tlua_pushthread(GetProcAddress(LuaLibD,'lua_pushthread'));
  lua_isnumber:=Tlua_isnumber(GetProcAddress(LuaLibD,'lua_isnumber'));
  lua_isstring:=Tlua_isstring(GetProcAddress(LuaLibD,'lua_isstring'));
  lua_iscfunction:=Tlua_iscfunction(GetProcAddress(LuaLibD,'lua_iscfunction'));
  lua_isuserdata:=Tlua_isuserdata(GetProcAddress(LuaLibD,'lua_isuserdata'));
  lua_type:=Tlua_type(GetProcAddress(LuaLibD,'lua_type'));
  lua_typename:=Tlua_typename(GetProcAddress(LuaLibD,'lua_typename'));
  lua_equal:=Tlua_equal(GetProcAddress(LuaLibD,'lua_equal'));
  lua_rawequal:=Tlua_rawequal(GetProcAddress(LuaLibD,'lua_rawequal'));
  lua_lessthan:=Tlua_lessthan(GetProcAddress(LuaLibD,'lua_lessthan'));
  lua_tonumber:=Tlua_tonumber(GetProcAddress(LuaLibD,'lua_tonumber'));
  lua_tointeger:=Tlua_tointeger(GetProcAddress(LuaLibD,'lua_tointeger'));
  lua_toboolean:=Tlua_toboolean(GetProcAddress(LuaLibD,'lua_toboolean'));
  lua_tolstring:=Tlua_tolstring(GetProcAddress(LuaLibD,'lua_tolstring'));
  lua_objlen:=Tlua_objlen(GetProcAddress(LuaLibD,'lua_objlen'));
  lua_tocfunction:=Tlua_tocfunction(GetProcAddress(LuaLibD,'lua_tocfunction'));
  lua_touserdata:=Tlua_touserdata(GetProcAddress(LuaLibD,'lua_touserdata'));
  lua_tothread:=Tlua_tothread(GetProcAddress(LuaLibD,'lua_tothread'));
  lua_topointer:=Tlua_topointer(GetProcAddress(LuaLibD,'lua_topointer'));
  lua_gettop:=Tlua_gettop(GetProcAddress(LuaLibD,'lua_gettop'));
  lua_settop:=Tlua_settop(GetProcAddress(LuaLibD,'lua_settop'));
  lua_pushvalue:=Tlua_pushvalue(GetProcAddress(LuaLibD,'lua_pushvalue'));
  lua_remove:=Tlua_remove(GetProcAddress(LuaLibD,'lua_remove'));
  lua_insert:=Tlua_insert(GetProcAddress(LuaLibD,'lua_insert'));
  lua_replace:=Tlua_replace(GetProcAddress(LuaLibD,'lua_replace'));
  lua_checkstack:=Tlua_checkstack(GetProcAddress(LuaLibD,'lua_checkstack'));
  lua_xmove:=Tlua_xmove(GetProcAddress(LuaLibD,'lua_xmove'));
  
  end;

{$ENDIF}


(*****************************************************************************)
(*                            luaconfig.h                                    *)
(*****************************************************************************)

function  lua_readline(L : Plua_State; var b : PChar; p : PChar): Boolean;
var
  s : AnsiString;
begin
  Write(p);                        // show prompt
  ReadLn(s);                       // get line
  b := PChar(s);                   //   and return it
  lua_readline := (b[0] <> #4);          // test for ctrl-D
end;

procedure lua_saveline(L : Plua_State; idx : Integer);
begin
end;

procedure lua_freeline(L : Plua_State; b : PChar);
begin
end;


(*****************************************************************************)
(*                                  lua.h                                    *)
(*****************************************************************************)

function lua_upvalueindex(idx : Integer) : Integer;
begin
  lua_upvalueindex := LUA_GLOBALSINDEX - idx;
end;

procedure lua_pop(L : Plua_State; n : Integer);
begin
  lua_settop(L, -n - 1);
end;

procedure lua_newtable(L : Plua_State);
begin
  lua_createtable(L, 0, 0);
end;

procedure lua_register(L : Plua_State; n : PChar; f : lua_CFunction);
begin
  lua_pushcfunction(L, f);
  lua_setglobal(L, n);
end;

procedure lua_pushcfunction(L : Plua_State; f : lua_CFunction);
begin
  lua_pushcclosure(L, f, 0);
end;

function  lua_strlen(L : Plua_State; idx : Integer) : Integer;
begin
  lua_strlen := lua_objlen(L, idx);
end;

function lua_isfunction(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isfunction := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable(L : Plua_State; n : Integer) : Boolean;
begin
  lua_istable := lua_type(L, n) = LUA_TTABLE;
end;

function lua_islightuserdata(L : Plua_State; n : Integer) : Boolean;
begin
  lua_islightuserdata := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isnil := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isboolean := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isthread(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isthread := lua_type(L, n) = LUA_TTHREAD;
end;

function lua_isnone(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isnone := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isnoneornil := lua_type(L, n) <= 0;
end;

procedure lua_pushliteral(L : Plua_State; s : PChar);
begin
  lua_pushlstring(L, s, StrLen(s));
end;

procedure lua_setglobal(L : Plua_State; s : PChar);
begin
  lua_setfield(L, LUA_GLOBALSINDEX, s);
end;

procedure lua_getglobal(L: Plua_State; s: PChar);
begin
  lua_getfield(L, LUA_GLOBALSINDEX, s);
end;

function lua_tostring(L : Plua_State; idx : Integer) : PChar;
begin
  lua_tostring := lua_tolstring(L, idx, nil);
end;

function lua_open : Plua_State;
begin
  lua_open := luaL_newstate();
end;

procedure lua_getregistry(L : Plua_State);
begin
  lua_pushvalue(L, LUA_REGISTRYINDEX);
end;

function lua_getgccount(L : Plua_State) : Integer;
begin
  lua_getgccount := lua_gc(L, LUA_GCCOUNT, 0);
end;


(*****************************************************************************)
(*                                  lualib.h                                 *)
(*****************************************************************************)

procedure lua_assert(x : Boolean);
begin
end;


(*****************************************************************************)
(*                                  lauxlib.h    n                           *)
(*****************************************************************************)

function luaL_getn(L : Plua_State; idx : Integer) : Integer;
begin
  luaL_getn := lua_objlen(L, idx);
end;

procedure luaL_setn(L : plua_State; i, j : Integer);
begin
  (* no op *)
end;

function luaL_argcheck(L : Plua_State; cond : Boolean; numarg : Integer;
                       extramsg : PChar): Integer;
begin
  if not cond then
    luaL_argcheck := luaL_argerror(L, numarg, extramsg)
  else
    luaL_argcheck := 0;
end;

function luaL_checkstring(L : Plua_State; n : Integer) : PChar;
begin
  luaL_checkstring := luaL_checklstring(L, n, nil);
end;

function luaL_optstring(L : Plua_State; n : Integer; d : PChar) : PChar;
begin
  luaL_optstring := luaL_optlstring(L, n, d, nil);
end;

function luaL_checkint(L : Plua_State; n : Integer) : Integer;
begin
  luaL_checkint := luaL_checkinteger(L, n);
end;

function luaL_optint(L : Plua_State; n, d : Integer): Integer;
begin
  luaL_optint := luaL_optinteger(L, n, d);
end;

function luaL_checklong(L : Plua_State; n : LongInt) : LongInt;
begin
  luaL_checklong := luaL_checkinteger(L, n);
end;

function luaL_optlong(L : Plua_State; n : Integer; d : LongInt) : LongInt;
begin
  luaL_optlong := luaL_optinteger(L, n, d);
end;

function luaL_typename(L : Plua_State; idx : Integer) : PChar;
begin
  luaL_typename := lua_typename( L, lua_type(L, idx) );
end;

function luaL_dofile(L : Plua_State; fn : PChar) : Integer;
Var
  Res : Integer;
begin
  // WC 2007\03\22 - Updated for Delphi
  Res := luaL_loadfile(L, fn);
  if Res = 0 then
    Res := lua_pcall(L, 0, 0, 0);
  Result := Res;
end;

function luaL_dostring(L : Plua_State; s : PChar) : Integer;
Var
  Res : Integer;
begin
  // WC 2007\03\22 - Updated for Delphi
  Res := luaL_loadstring(L, s);
  if Res = 0 then
    Res := lua_pcall(L, 0, 0, 0);
  Result := Res;
end;

procedure luaL_getmetatable(L : Plua_State; n : PChar);
begin
  lua_getfield(L, LUA_REGISTRYINDEX, n);
end;

procedure luaL_addchar(B : PluaL_Buffer; c : Char);
begin
  if not(B^.p < B^.buffer + LUAL_BUFFERSIZE) then
    luaL_prepbuffer(B);
  B^.p^ := c;
  Inc(B^.p);
end;

procedure luaL_putchar(B : PluaL_Buffer; c : Char);
begin
  luaL_addchar(B, c);
end;

procedure luaL_addsize(B : PluaL_Buffer; n : Integer);
begin
  Inc(B^.p, n);
end;

function lua_ref(L : Plua_State; lock : Boolean) : Integer;
begin
  if lock then
    lua_ref := luaL_ref(L, LUA_REGISTRYINDEX)
  else begin
    lua_pushstring(L, 'unlocked references are obsolete');
    lua_error(L);
    lua_ref := 0;
  end;
end;

procedure lua_unref(L : Plua_State; ref : Integer);
begin
  luaL_unref(L, LUA_REGISTRYINDEX, ref);
end;

procedure lua_getref(L : Plua_State; ref : Integer);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, ref);
end;


(******************************************************************************
* Original copyright for the lua source and headers:
*  1994-2004 Tecgraf, PUC-Rio.
*  www.lua.org.
*
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)

end.

