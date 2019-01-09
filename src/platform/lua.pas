(*
 * A Pascal wrapper for Lua 5.1-5.3 library.
 *
 * Created by Geo Massar, 2006
 * Distributed as free/open source.
 * 2008 Added dinamicly library loading by Dmitry Kolomiets (B4rr4cuda@rambler.ru)
 * 2018 Added Lua 5.2 - 5.3 library support by Alexander Koblov (alexx2000@mail.ru)
 *)

unit lua;

{$mode delphi}

interface

uses
  DynLibs;

type
  size_t = SizeUInt;
  Psize_t  = ^size_t;

  lua_State = record end;
  Plua_State = ^lua_State;

const
{$IF DEFINED(MSWINDOWS)}
  LuaDLL = 'lua5.1.dll';
{$ELSEIF DEFINED(DARWIN)}
  LuaDLL = 'liblua5.1.dylib';
{$ELSEIF DEFINED(UNIX)}
  LuaDLL = 'liblua5.1.so.0';
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

{$IFNDEF LUA_INTEGER_FMT}
const
  LUA_INTEGER_FMT = '%d';
{$ENDIF}

function LoadLuaLib(FileName: String): Boolean;
procedure UnloadLuaLib;
function IsLuaLibLoaded: Boolean;
function LuaCheckVersion: Boolean;

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
  LUA_NUMBER_  = type Double;           // ending underscore is needed in Pascal
  LUA_INTEGER_ = type Int64;

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
  LUA_VERSION_    = 'Lua 5.1';
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
  // Type for continuation-function contexts
  lua_KContext = Pointer;

  // Type for continuation functions
  lua_KFunction = function(L : Plua_State; status: Integer; ctx: lua_KContext): Integer; cdecl;

  // Type for C functions registered with Lua
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
  Plua_Number = ^lua_Number;

  (* type for integer functions *)
  lua_Integer = LUA_INTEGER_;

(*
** state manipulation
*)
var
  lua_newstate:  function (f : lua_Alloc; ud : Pointer) : Plua_State; cdecl;
  lua_close:     procedure (L: Plua_State);  cdecl;
  lua_newthread: function  (L : Plua_State) : Plua_State;  cdecl;
  lua_atpanic:   function  (L : Plua_State; panicf : lua_CFunction) : lua_CFunction;  cdecl;

(*
** basic stack manipulation
*)
var
  lua_gettop:     function  (L : Plua_State) : Integer;  cdecl;
  lua_settop:     procedure (L : Plua_State; idx : Integer);  cdecl;
  lua_pushvalue:  procedure (L : Plua_State; idx : Integer);  cdecl;
  lua_remove:     procedure (L : Plua_State; idx : Integer);  cdecl;
  lua_insert:     procedure (L : Plua_State; idx : Integer);  cdecl;
  lua_replace:    procedure (L : Plua_State; idx : Integer);  cdecl;
  lua_checkstack: function  (L : Plua_State; sz : Integer) : LongBool;  cdecl;
  lua_xmove:      procedure (src, dest : Plua_State; n : Integer);  cdecl;

(*
** access functions (stack -> C)
*)
  lua_isnumber:    function (L : Plua_State; idx : Integer) : LongBool;  cdecl;
  lua_isstring:    function (L : Plua_State; idx : Integer) : LongBool;  cdecl;
  lua_iscfunction: function (L : Plua_State; idx : Integer) : LongBool;  cdecl;
  lua_isinteger:   function (L: Plua_State;  idx: Integer)  : LongBool;  cdecl;
  lua_isuserdata:  function (L : Plua_State; idx : Integer) : LongBool;  cdecl;
  lua_type:        function (L : Plua_State; idx : Integer) : Integer;  cdecl;
  lua_typename:    function (L : Plua_State; tp : Integer) : PChar;  cdecl;
  lua_equal:       function (L : Plua_State; idx1, idx2 : Integer) : LongBool;  cdecl;
  lua_rawequal:    function (L : Plua_State; idx1, idx2 : Integer) : LongBool;  cdecl;
  lua_lessthan:    function (L : Plua_State; idx1, idx2 : Integer) : LongBool;  cdecl;
  lua_toboolean:   function (L : Plua_State; idx : Integer) : LongBool;  cdecl;
  lua_tolstring:   function (L : Plua_State; idx : Integer; len : Psize_t) : PChar;  cdecl;
  lua_tocfunction: function (L : Plua_State; idx : Integer) : lua_CFunction;  cdecl;
  lua_touserdata:  function (L : Plua_State; idx : Integer) : Pointer;  cdecl;
  lua_tothread:    function (L : Plua_State; idx : Integer) : Plua_State;  cdecl;
  lua_topointer:   function (L : Plua_State; idx : Integer) : Pointer;  cdecl;

  function lua_tonumber(L : Plua_State; idx : Integer) : lua_Number;
  function lua_tointeger(L : Plua_State; idx : Integer) : lua_Integer;
  function lua_objlen(L : Plua_State; idx : Integer) : size_t;

(*
** push functions (C -> stack)
*)
var
  lua_pushnil:  procedure (L : Plua_State);  cdecl;
  lua_pushnumber:  procedure (L : Plua_State; n : lua_Number);  cdecl;
  lua_pushinteger:  procedure (L : Plua_State; n : lua_Integer);  cdecl;
  lua_pushlstring:  procedure (L : Plua_State; const s : PChar; ls : size_t);  cdecl;
  lua_pushvfstring:  function  (L : Plua_State; const fmt : PChar; argp : Pointer) : PChar;  cdecl;
  lua_pushfstring:  function  (L : Plua_State; const fmt : PChar) : PChar; varargs;  cdecl;
  lua_pushcclosure:  procedure (L : Plua_State; fn : lua_CFunction; n : Integer);  cdecl;
  lua_pushboolean:  procedure (L : Plua_State; b : LongBool);  cdecl;
  lua_pushlightuserdata:  procedure (L : Plua_State; p : Pointer);  cdecl;
  lua_pushthread:  function  (L : Plua_state) : Cardinal;  cdecl;

(*
** get functions (Lua -> stack)
*)
  lua_gettable:  procedure (L : Plua_State ; idx : Integer);  cdecl;
  lua_getfield:  procedure (L : Plua_State; idx : Integer; k : PChar);  cdecl;
  lua_rawget:  procedure (L : Plua_State; idx : Integer);  cdecl;
  lua_rawgeti:  procedure (L : Plua_State; idx, n : Integer);  cdecl;
  lua_createtable:  procedure (L : Plua_State; narr, nrec : Integer);  cdecl;
  lua_newuserdata:  function  (L : Plua_State; sz : size_t) : Pointer;  cdecl;
  lua_getmetatable:  function  (L : Plua_State; objindex : Integer) : LongBool;  cdecl;
  lua_getfenv:  procedure (L : Plua_State; idx : Integer);  cdecl;

(*
** set functions (stack -> Lua)
*)
  lua_settable:  procedure (L : Plua_State; idx : Integer);  cdecl;
  lua_setfield:  procedure (L : Plua_State; idx : Integer; const k : PChar);  cdecl;
  lua_rawset:  procedure (L : Plua_State; idx : Integer);  cdecl;
  lua_rawseti:  procedure (L : Plua_State; idx , n: Integer);  cdecl;
  lua_setmetatable:  function (L : Plua_State; objindex : Integer): LongBool;  cdecl;
  lua_setfenv:  function (L : Plua_State; idx : Integer): LongBool;  cdecl;

procedure lua_setglobal(L: Plua_State; const name : PAnsiChar);
procedure lua_getglobal(L: Plua_State; const name : PAnsiChar);

(*
** `load' and `call' functions (load and run Lua code)
*)
var
  lua_call:  procedure (L : Plua_State; nargs, nresults : Integer);  cdecl;
  lua_cpcall:  function  (L : Plua_State; func : lua_CFunction; ud : Pointer) : Integer;  cdecl;
  lua_load:  function  (L : Plua_State; reader : lua_Reader; dt : Pointer; const chunkname : PChar) : Integer;  cdecl;
  lua_dump:  function (L : Plua_State; writer : lua_Writer; data: Pointer) : Integer; cdecl;

function lua_pcall(L : Plua_State; nargs, nresults, errfunc : Integer) : Integer;

(*
** coroutine functions
*)
var
  lua_yield:  function (L : Plua_State; nresults : Integer) : Integer;  cdecl;
  lua_resume:  function (L : Plua_State; narg : Integer) : Integer;  cdecl;
  lua_status:  function (L : Plua_State) : Integer;  cdecl;

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

var
  lua_gc: function(L : Plua_State; what, data : Integer):Integer; cdecl;

(*
** miscellaneous functions
*)
var
  lua_error:  function (L : Plua_State) : Integer;  cdecl;
  lua_next:  function (L : Plua_State; idx : Integer) : Integer;  cdecl;
  lua_concat:  procedure (L : Plua_State; n : Integer);  cdecl;
  lua_getallocf: function  (L : Plua_State; ud : PPointer) : lua_Alloc;  cdecl;
  lua_setallocf:  procedure (L : Plua_State; f : lua_Alloc; ud : Pointer);  cdecl;

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
procedure lua_pushstring(L : Plua_State; const S : PChar); overload;
procedure lua_pushstring(L : Plua_State; const S : String); overload;

function lua_tostring(L : Plua_State; idx : Integer) : String;
function lua_tocstring(L : Plua_State; idx : Integer) : PAnsiChar;

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

var
  lua_getstack:  function (L : Plua_State; level : Integer; ar : Plua_Debug) : Integer;  cdecl;
  lua_getinfo:  function (L : Plua_State; const what : PChar; ar: Plua_Debug): Integer;  cdecl;
  lua_getlocal:  function (L : Plua_State; ar : Plua_Debug; n : Integer) : PChar;  cdecl;
  lua_setlocal:  function (L : Plua_State; ar : Plua_Debug; n : Integer) : PChar;  cdecl;
  lua_getupvalue:  function (L : Plua_State; funcindex, n : Integer) : PChar;  cdecl;
  lua_setupvalue:  function (L : Plua_State; funcindex, n : Integer) : PChar;  cdecl;
  lua_sethook:  function (L : Plua_State; func : lua_Hook; mask, count: Integer): Integer;  cdecl;
  {function lua_gethook(L : Plua_State) : lua_Hook;  cdecl;}
  lua_gethookmask:  function (L : Plua_State) : Integer;  cdecl;
  lua_gethookcount:  function (L : Plua_State) : Integer;  cdecl;


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

var
  luaopen_base:  function (L : Plua_State) : Integer;  cdecl;
  luaopen_table:  function (L : Plua_State) : Integer;  cdecl;
  luaopen_io:  function (L : Plua_State) : Integer;  cdecl;
  luaopen_os:  function (L : Plua_State) : Integer;  cdecl;
  luaopen_string:  function (L : Plua_State) : Integer;  cdecl;
  luaopen_math:  function (L : Plua_State) : Integer;  cdecl;
  luaopen_debug:  function (L : Plua_State) : Integer;  cdecl;
  luaopen_package:  function (L : Plua_State) : Integer;  cdecl;
  luaL_openlibs:  procedure (L : Plua_State);  cdecl;

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

var
  luaL_openlib:  procedure (L : Plua_State; const libname : PChar; const lr : PluaL_Reg; nup : Integer);  cdecl;
  luaL_register:  procedure (L : Plua_State; const libname : PChar; const lr : PluaL_Reg);  cdecl;
  luaL_getmetafield:  function (L : Plua_State; obj : Integer; const e : PChar) : Integer;  cdecl;
  luaL_callmeta:  function (L : Plua_State; obj : Integer; const e : PChar) : Integer;  cdecl;
  luaL_typerror:  function (L : Plua_State; narg : Integer; const tname : PChar) : Integer;  cdecl;
  luaL_argerror:  function (L : Plua_State; numarg : Integer; const extramsg : PChar) : Integer;  cdecl;
  luaL_checklstring:  function (L : Plua_State; numArg : Integer; ls : Psize_t) : PChar;  cdecl;
  luaL_optlstring:  function (L : Plua_State; numArg : Integer; const def: PChar; ls: Psize_t) : PChar;  cdecl;
  luaL_checknumber:  function (L : Plua_State; numArg : Integer) : lua_Number;  cdecl;
  luaL_optnumber:  function (L : Plua_State; nArg : Integer; def : lua_Number) : lua_Number;  cdecl;
  luaL_checkinteger:  function (L : Plua_State; numArg : Integer) : lua_Integer; cdecl;
  luaL_optinteger:  function (L : Plua_State; nArg : Integer; def : lua_Integer) : lua_Integer;  cdecl;
  luaL_checkstack:  procedure (L : Plua_State; sz : Integer; const msg : PChar); cdecl;
  luaL_checktype:  procedure (L : Plua_State; narg, t : Integer);  cdecl;
  luaL_checkany:  procedure (L : Plua_State; narg : Integer);  cdecl;
  luaL_newmetatable:  function (L : Plua_State; const tname : PChar) : Integer;  cdecl;
  luaL_checkudata:  function (L : Plua_State; ud : Integer; const tname : PChar) : Pointer;  cdecl;
  luaL_where:  procedure (L : Plua_State; lvl : Integer);  cdecl;
  luaL_error:  function  (L : Plua_State; const fmt : PChar) : Integer; varargs;  cdecl;
  luaL_checkoption:  function (L : Plua_State; narg : Integer; const def : PChar;  const lst : array of PChar) : Integer;  cdecl;
  luaL_ref:  function  (L : Plua_State; t : Integer) : Integer;  cdecl;
  luaL_unref:  procedure (L : Plua_State; t, ref : Integer);  cdecl;
  luaL_loadfilex : function (L: Plua_State; const filename, mode: PAnsiChar): Integer; cdecl;
  luaL_loadbuffer:  function (L : Plua_State; const buff : PChar; sz : size_t; const name: PChar) : Integer;  cdecl;
  luaL_loadstring:  function (L : Plua_State; const s : Pchar) : Integer; cdecl;
  luaL_newstate:  function  : Plua_State;  cdecl;
  luaL_gsub:  function (L : Plua_State; const s, p, r : PChar) : PChar;  cdecl;
  luaL_findtable:  function (L : Plua_State; idx : Integer; const fname : PChar; szhint : Integer) : PChar;  cdecl;
  luaL_execresult: function (L: Plua_State; stat: Integer): Integer; cdecl;

function luaL_loadfile(L: Plua_State; const filename: PAnsiChar): Integer;

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

var
  luaL_buffinit:  procedure (L : Plua_State; B : PluaL_Buffer);  cdecl;
  luaL_prepbuffer:  function  (B : PluaL_Buffer) : PChar;  cdecl;
  luaL_addlstring:  procedure (B : PluaL_Buffer; const s : PChar; ls : size_t);  cdecl;
  luaL_addstring:  procedure (B : PluaL_Buffer; const s : PChar);  cdecl;
  luaL_addvalue:  procedure (B : PluaL_Buffer);  cdecl;
  luaL_pushresult:  procedure (B : PluaL_Buffer);  cdecl;

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

var
  LuaLibD: TLibHandle;
  luaJIT: Boolean;

implementation

uses
  SysUtils
{$IFDEF UNIX}
  , dl
{$ENDIF}
  ;

var
  lua_version: function (L: Plua_State): Plua_Number; cdecl;
  lua_rawlen: function (L : Plua_State; idx : Integer): size_t; cdecl;
  lua_pushstring_:  procedure (L : Plua_State; const s : PChar);  cdecl;
  lua_objlen_: function (L : Plua_State; idx : Integer) : size_t;  cdecl;
  lua_setglobal_: procedure (L: Plua_State; const name: PAnsiChar); cdecl;
  lua_getglobal_: function (L: Plua_State; const name: PAnsiChar): Integer; cdecl;
  lua_tonumber_:    function (L : Plua_State; idx : Integer) : lua_Number;  cdecl;
  lua_tointeger_:   function (L : Plua_State; idx : Integer) : lua_Integer;  cdecl;
  luaL_loadfile_: function (L: Plua_State; const filename: PAnsiChar): Integer; cdecl;
  lua_tonumberx: function(L: Plua_State; idx: Integer; isnum: PLongBool): lua_Number; cdecl;
  luaL_loadfilex_: function (L: Plua_State; const filename, mode: PAnsiChar): Integer; cdecl;
  lua_pcall_: function (L : Plua_State; nargs, nresults, errfunc : Integer) : Integer; cdecl;
  lua_tointegerx: function(L: Plua_State; idx: Integer; isnum: PLongBool): lua_Integer; cdecl;
  lua_pcallk: function(L: Plua_State; nargs, nresults, errfunc: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; cdecl;

procedure UnloadLuaLib;
begin
 if LuaLibD <> NilHandle then
   FreeLibrary(LuaLibD);
end;

function IsLuaLibLoaded: Boolean;
begin
  Result:= (LuaLibD <> NilHandle);
end;

function LuaCheckVersion: Boolean;
begin
  Result:= True;
  if Assigned(lua_version) then begin
    Result:= (Trunc(lua_version(nil)^) = LUA_VERSION_NUM);
  end;
  Result:= Result and (luaJIT = False);
end;

function LoadLuaLib(FileName: String): Boolean;
begin
{$IF DEFINED(UNIX)}
  LuaLibD:= TLibHandle(dlopen(PAnsiChar(FileName), RTLD_NOW or RTLD_GLOBAL));
{$ELSE}
  LuaLibD:= LoadLibrary(FileName);
{$ENDIF}
  Result:= (LuaLibD <> NilHandle);
  if not Result then Exit;

  @lua_newstate := GetProcAddress(LuaLibD, 'lua_newstate');
  @lua_close := GetProcAddress(LuaLibD, 'lua_close');
  @lua_newthread := GetProcAddress(LuaLibD, 'lua_newthread');
  @lua_atpanic := GetProcAddress(LuaLibD, 'lua_atpanic');
  @luaL_buffinit := GetProcAddress(LuaLibD, 'luaL_buffinit');
  @luaL_prepbuffer := GetProcAddress(LuaLibD, 'luaL_prepbuffer');
  @luaL_addlstring := GetProcAddress(LuaLibD, 'luaL_addlstring');
  @luaL_addstring := GetProcAddress(LuaLibD, 'luaL_addstring');
  @luaL_addvalue := GetProcAddress(LuaLibD, 'luaL_addvalue');
  @luaL_pushresult := GetProcAddress(LuaLibD, 'luaL_pushresult');
  @luaL_openlib := GetProcAddress(LuaLibD, 'luaL_openlib');
  @luaL_register := GetProcAddress(LuaLibD, 'luaL_register');
  @luaL_getmetafield := GetProcAddress(LuaLibD, 'luaL_getmetafield');
  @luaL_callmeta := GetProcAddress(LuaLibD, 'luaL_callmeta');
  @luaL_typerror := GetProcAddress(LuaLibD, 'luaL_typerror');
  @luaL_argerror := GetProcAddress(LuaLibD, 'luaL_argerror');
  @luaL_checklstring := GetProcAddress(LuaLibD, 'luaL_checklstring');
  @luaL_optlstring := GetProcAddress(LuaLibD, 'luaL_optlstring');
  @luaL_checknumber := GetProcAddress(LuaLibD, 'luaL_checknumber');
  @luaL_optnumber := GetProcAddress(LuaLibD, 'luaL_optnumber');
  @luaL_checkinteger := GetProcAddress(LuaLibD, 'luaL_checkinteger');
  @luaL_optinteger := GetProcAddress(LuaLibD, 'luaL_optinteger');
  @luaL_checkstack := GetProcAddress(LuaLibD, 'luaL_checkstack');
  @luaL_checktype := GetProcAddress(LuaLibD, 'luaL_checktype');
  @luaL_checkany := GetProcAddress(LuaLibD, 'luaL_checkany');
  @luaL_newmetatable := GetProcAddress(LuaLibD, 'luaL_newmetatable');
  @luaL_checkudata := GetProcAddress(LuaLibD, 'luaL_checkudata');
  @luaL_where := GetProcAddress(LuaLibD, 'luaL_where');
  @luaL_error := GetProcAddress(LuaLibD, 'luaL_error');
  @luaL_checkoption := GetProcAddress(LuaLibD, 'luaL_checkoption');
  @luaL_ref := GetProcAddress(LuaLibD, 'luaL_ref');
  @luaL_unref := GetProcAddress(LuaLibD, 'luaL_unref');
  @luaL_loadfile_ := GetProcAddress(LuaLibD, 'luaL_loadfile');
  @luaL_loadbuffer := GetProcAddress(LuaLibD, 'luaL_loadbuffer');
  @luaL_loadstring := GetProcAddress(LuaLibD, 'luaL_loadstring');
  @luaL_newstate := GetProcAddress(LuaLibD, 'luaL_newstate');
  @luaL_gsub := GetProcAddress(LuaLibD, 'luaL_gsub');
  @luaL_findtable := GetProcAddress(LuaLibD, 'luaL_findtable');
  @luaL_execresult := GetProcAddress(LuaLibD, 'luaL_execresult');
  @luaopen_base := GetProcAddress(LuaLibD, 'luaopen_base');
  @luaopen_table := GetProcAddress(LuaLibD, 'luaopen_table');
  @luaopen_io := GetProcAddress(LuaLibD, 'luaopen_io');
  @luaopen_os := GetProcAddress(LuaLibD, 'luaopen_os');
  @luaopen_string := GetProcAddress(LuaLibD, 'luaopen_string');
  @luaopen_math := GetProcAddress(LuaLibD, 'luaopen_math');
  @luaopen_debug := GetProcAddress(LuaLibD, 'luaopen_debug');
  @luaopen_package := GetProcAddress(LuaLibD, 'luaopen_package');
  @luaL_openlibs := GetProcAddress(LuaLibD, 'luaL_openlibs');
  @lua_getstack := GetProcAddress(LuaLibD, 'lua_getstack');
  @lua_getinfo := GetProcAddress(LuaLibD, 'lua_getinfo');
  @lua_getlocal := GetProcAddress(LuaLibD, 'lua_getlocal');
  @lua_setlocal := GetProcAddress(LuaLibD, 'lua_setlocal');
  @lua_getupvalue := GetProcAddress(LuaLibD, 'lua_getupvalue');
  @lua_setupvalue := GetProcAddress(LuaLibD, 'lua_setupvalue');
  @lua_sethook := GetProcAddress(LuaLibD, 'lua_sethook');
  // function lua_gethook(L : Plua_State) : lua_Hook; cdecl;
  @lua_gethookmask := GetProcAddress(LuaLibD, 'lua_gethookmask');
  @lua_gethookcount := GetProcAddress(LuaLibD, 'lua_gethookcount');
  @lua_error := GetProcAddress(LuaLibD, 'lua_error');
  @lua_next := GetProcAddress(LuaLibD, 'lua_next');
  @lua_concat := GetProcAddress(LuaLibD, 'lua_concat');
  @lua_getallocf := GetProcAddress(LuaLibD, 'lua_getallocf');
  @lua_setallocf := GetProcAddress(LuaLibD, 'lua_setallocf');
  @lua_gc := GetProcAddress(LuaLibD, 'lua_gc');
  @lua_yield := GetProcAddress(LuaLibD, 'lua_yield');
  @lua_resume := GetProcAddress(LuaLibD, 'lua_resume');
  @lua_status := GetProcAddress(LuaLibD, 'lua_status');
  @lua_call := GetProcAddress(LuaLibD, 'lua_call');
  @lua_pcall_ := GetProcAddress(LuaLibD, 'lua_pcall');
  @lua_cpcall := GetProcAddress(LuaLibD, 'lua_cpcall');
  @lua_load := GetProcAddress(LuaLibD, 'lua_load');
  @lua_dump := GetProcAddress(LuaLibD, 'lua_dump');
  @lua_settable := GetProcAddress(LuaLibD, 'lua_settable');
  @lua_setfield := GetProcAddress(LuaLibD, 'lua_setfield');
  @lua_rawset := GetProcAddress(LuaLibD, 'lua_rawset');
  @lua_rawseti := GetProcAddress(LuaLibD, 'lua_rawseti');
  @lua_setmetatable := GetProcAddress(LuaLibD, 'lua_setmetatable');
  @lua_setfenv := GetProcAddress(LuaLibD, 'lua_setfenv');
  @lua_gettable := GetProcAddress(LuaLibD, 'lua_gettable');
  @lua_getfield := GetProcAddress(LuaLibD, 'lua_getfield');
  @lua_rawget := GetProcAddress(LuaLibD, 'lua_rawget');
  @lua_rawgeti := GetProcAddress(LuaLibD, 'lua_rawgeti');
  @lua_createtable := GetProcAddress(LuaLibD, 'lua_createtable');
  @lua_newuserdata := GetProcAddress(LuaLibD, 'lua_newuserdata');
  @lua_getmetatable := GetProcAddress(LuaLibD, 'lua_getmetatable');
  @lua_getfenv := GetProcAddress(LuaLibD, 'lua_getfenv');
  @lua_pushnil := GetProcAddress(LuaLibD, 'lua_pushnil');
  @lua_pushnumber := GetProcAddress(LuaLibD, 'lua_pushnumber');
  @lua_pushinteger := GetProcAddress(LuaLibD, 'lua_pushinteger');
  @lua_pushlstring := GetProcAddress(LuaLibD, 'lua_pushlstring');
  @lua_pushstring_ := GetProcAddress(LuaLibD, 'lua_pushstring');
  @lua_pushvfstring := GetProcAddress(LuaLibD, 'lua_pushvfstring');
  @lua_pushfstring := GetProcAddress(LuaLibD, 'lua_pushfstring');
  @lua_pushcclosure := GetProcAddress(LuaLibD, 'lua_pushcclosure');
  @lua_pushboolean := GetProcAddress(LuaLibD, 'lua_pushboolean');
  @lua_pushlightuserdata := GetProcAddress(LuaLibD, 'lua_pushlightuserdata');
  @lua_pushthread := GetProcAddress(LuaLibD, 'lua_pushthread');
  @lua_isnumber := GetProcAddress(LuaLibD, 'lua_isnumber');
  @lua_isstring := GetProcAddress(LuaLibD, 'lua_isstring');
  @lua_iscfunction := GetProcAddress(LuaLibD, 'lua_iscfunction');
  @lua_isinteger :=GetProcAddress(LuaLibD, 'lua_isinteger');
  @lua_isuserdata := GetProcAddress(LuaLibD, 'lua_isuserdata');
  @lua_type := GetProcAddress(LuaLibD, 'lua_type');
  @lua_typename := GetProcAddress(LuaLibD, 'lua_typename');
  @lua_equal := GetProcAddress(LuaLibD, 'lua_equal');
  @lua_rawequal := GetProcAddress(LuaLibD, 'lua_rawequal');
  @lua_lessthan := GetProcAddress(LuaLibD, 'lua_lessthan');
  @lua_tonumber_ := GetProcAddress(LuaLibD, 'lua_tonumber');
  @lua_tointeger_ := GetProcAddress(LuaLibD, 'lua_tointeger');
  @lua_toboolean := GetProcAddress(LuaLibD, 'lua_toboolean');
  @lua_tolstring := GetProcAddress(LuaLibD, 'lua_tolstring');
  @lua_objlen_ := GetProcAddress(LuaLibD, 'lua_objlen');
  @lua_tocfunction := GetProcAddress(LuaLibD, 'lua_tocfunction');
  @lua_touserdata := GetProcAddress(LuaLibD, 'lua_touserdata');
  @lua_tothread := GetProcAddress(LuaLibD, 'lua_tothread');
  @lua_topointer := GetProcAddress(LuaLibD, 'lua_topointer');
  @lua_gettop := GetProcAddress(LuaLibD, 'lua_gettop');
  @lua_settop := GetProcAddress(LuaLibD, 'lua_settop');
  @lua_pushvalue := GetProcAddress(LuaLibD, 'lua_pushvalue');
  @lua_remove := GetProcAddress(LuaLibD, 'lua_remove');
  @lua_insert := GetProcAddress(LuaLibD, 'lua_insert');
  @lua_replace := GetProcAddress(LuaLibD, 'lua_replace');
  @lua_checkstack := GetProcAddress(LuaLibD, 'lua_checkstack');
  @lua_xmove := GetProcAddress(LuaLibD, 'lua_xmove');

  // Lua 5.2 - 5.3 specific stuff
  @lua_rawlen := GetProcAddress(LuaLibD, 'lua_rawlen');
  @lua_pcallk := GetProcAddress(LuaLibD, 'lua_pcallk');
  @lua_version := GetProcAddress(LuaLibD, 'lua_version');
  @lua_tonumberx := GetProcAddress(LuaLibD, 'lua_tonumberx');
  @lua_setglobal_ := GetProcAddress(LuaLibD, 'lua_setglobal');
  @lua_getglobal_ := GetProcAddress(LuaLibD, 'lua_getglobal');
  @lua_tointegerx := GetProcAddress(LuaLibD, 'lua_tointegerx');
  @luaL_loadfilex_ := GetProcAddress(LuaLibD, 'luaL_loadfilex');

  // luaJIT specific stuff
  luaJIT := GetProcAddress(LuaLibD, 'luaJIT_setmode') <> nil;
end;

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

procedure lua_pushstring(L: Plua_State; const S: PChar); inline;
begin
  lua_pushstring_(L, S);
end;

procedure lua_pushstring(L: Plua_State; const S: String); inline;
begin
  lua_pushlstring(L, PAnsiChar(S), Length(S));
end;

function lua_tonumber(L: Plua_State; idx: Integer): lua_Number;
begin
  if Assigned(lua_tonumberx) then
    Result:= lua_tonumberx(L, idx, nil)
  else
    Result:= lua_tonumber_(L, idx);
end;

function lua_tointeger(L: Plua_State; idx: Integer): lua_Integer;
begin
  if Assigned(lua_tointegerx) then
    Result:= lua_tointegerx(L, idx, nil)
  else
    Result:= lua_tointeger_(L, idx);
end;

function lua_objlen(L: Plua_State; idx: Integer): size_t;
begin
  if Assigned(lua_rawlen) then
    Result:= lua_rawlen(L, idx)
  else
    Result:= lua_objlen_(L, idx);
end;

procedure lua_setglobal(L: Plua_State; const name: PAnsiChar);
begin
  if Assigned(lua_setglobal_) then
    lua_setglobal_(L, name)
  else
    lua_setfield(L, LUA_GLOBALSINDEX, name);
end;

procedure lua_getglobal(L: Plua_State; const name: PAnsiChar);
begin
  if Assigned(lua_getglobal_) then
    lua_getglobal_(L, name)
  else
    lua_getfield(L, LUA_GLOBALSINDEX, name);
end;

function lua_tostring(L : Plua_State; idx : Integer) : String;
var
  N: size_t;
begin
  SetString(Result, lua_tolstring(L, idx, @N), N);
end;

function lua_tocstring(L : Plua_State; idx : Integer) : PAnsiChar;
begin
  lua_tocstring := lua_tolstring(L, idx, nil);
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

function lua_pcall(L: Plua_State; nargs, nresults, errfunc: Integer): Integer;
begin
  if Assigned(lua_pcallk) then
    Result:= lua_pcallk(L, nargs, nresults, errfunc, nil, nil)
  else
    Result:= lua_pcall_(L, nargs, nresults, errfunc);
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

procedure luaL_setn(L: Plua_State; i, j: Integer);
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

function luaL_loadfile(L: Plua_State; const filename: PAnsiChar): Integer;
begin
  if Assigned(luaL_loadfilex_) then
    Result:= luaL_loadfilex_(L, filename, nil)
  else
    Result:= luaL_loadfile_(L, filename);
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

