{
   Double commander
   -------------------------------------------------------------------------
   Push some useful functions to Lua

   Copyright (C) 2016-2018 Alexander Koblov (alexx2000@mail.ru)

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

unit uLuaPas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lua;

procedure RegisterPackages(L : Plua_State);
procedure SetPackagePath(L: Plua_State; const Path: String);
function LuaPCall(L : Plua_State; nargs, nresults : Integer): Boolean;
function ExecuteScript(const FileName: String; Args: array of String): Boolean;

implementation

uses
  Forms, Dialogs, Clipbrd, LazUTF8, LCLVersion, CTypes, DCOSUtils, DCBasicTypes,
  DCConvertEncoding, fMain, uFormCommands, uOSUtils, uGlobs, uLog, uMicroLibC,
  uClipboard, uShowMsg;

function tofilep(L: Plua_State): PPointer; inline;
begin
  Result:= PPointer(luaL_checkudata(L, 1, LUA_FILEHANDLE))
end;

function tofile(L: Plua_State): Pointer;
var
  f: PPointer;
begin
  f := tofilep(L);
  if (f^ = nil) then
    luaL_error(L, 'attempt to use a closed file');
  Result := f^;
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

function pushresult(L: Plua_State; i: Boolean; const filename: PAnsiChar): cint;
var
  en: cint;
begin
  en := cerrno;  //* calls to Lua API may change this value */
  if (i) then begin
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

function newfile(L: Plua_State): PPointer; cdecl;
begin
  Result := PPointer(lua_newuserdata(L, SizeOf(Pointer)));
  Result^ := nil;  //* file handle is currently `closed' */
  luaL_getmetatable(L, LUA_FILEHANDLE);
  lua_setmetatable(L, -2);
end;

function io_open(L: Plua_State): Integer; cdecl;
var
  pf: PPointer;
  filename, mode: PAnsiChar;
begin
  filename := luaL_checkstring(L, 1);
  mode := luaL_optstring(L, 2, 'r');
  pf := newfile(L);
  pf^ := cfopen(filename, mode);
  if (pf^ <> nil) then
    Result := 1
  else begin
    Result := pushresult(L, False, filename);
  end;
end;

function io_lines(L: Plua_State): Integer; cdecl;
const
  IO_INPUT = 1;
var
  pf: PPointer;
  filename: PAnsiChar;
  f_lines: lua_CFunction;
begin
  // Get f_lines C function
  luaL_getmetatable(L, LUA_FILEHANDLE);
    lua_getfield(L, -1, 'lines');
    f_lines := lua_tocfunction(L, -1);
  lua_pop(L, 2);
  //* no arguments? */
  if (lua_isnoneornil(L, 1)) then
  begin
    //* will iterate over default input */
    lua_rawgeti(L, LUA_ENVIRONINDEX, IO_INPUT);
    Result := f_lines(L);
  end
  else begin
    filename := luaL_checkstring(L, 1);
    pf := newfile(L);
    pf^ := cfopen(filename, 'r');
    if (pf^ = nil) then begin
      lua_pushfstring(L, '%s: %s', filename, cstrerror(cerrno));
      luaL_argerror(L, 1, lua_tostring(L, -1));
    end;
    lua_replace(L, 1);
    Result := f_lines(L);
  end;
end;

function io_popen(L: Plua_State): Integer; cdecl;
var
  pf: PPointer;
  filename, mode: PAnsiChar;
begin
  filename := luaL_checkstring(L, 1);
  mode := luaL_optstring(L, 2, 'r');
  pf := newfile(L);
  pf^ := cpopen(filename, mode);
  if (pf^ <> nil) then
    Result := 1
  else begin
    Result := pushresult(L, False, filename);
  end;
end;

procedure luaPushSearchRec(L : Plua_State; var Rec: TSearchRec);
var
  FindHandle: PtrInt absolute Rec.FindHandle;
begin
  lua_pushinteger(L, FindHandle);
  lua_newtable(L);
  lua_pushinteger(L, Rec.Time);
  lua_setfield(L, -2, 'Time');
  lua_pushinteger(L, Rec.Size);
  lua_setfield(L, -2, 'Size');
  lua_pushinteger(L, Rec.Attr);
  lua_setfield(L, -2, 'Attr');
  lua_pushstring(L, PAnsiChar(Rec.Name));
  lua_setfield(L, -2, 'Name');
end;

function luaFindFirst(L : Plua_State) : Integer; cdecl;
var
  Path: String;
  Rec: TSearchRec;
begin
  Result:= 2;
  Path:= String(mbFileNameToNative(lua_tostring(L, 1)));
  if FindFirst(Path, faAnyFile, Rec) = 0 then
    luaPushSearchRec(L, Rec)
  else begin
    lua_pushnil(L);
    lua_pushnil(L);
  end;
end;

function luaFindNext(L : Plua_State) : Integer; cdecl;
var
  Rec: TSearchRec;
  FindHandle: PtrInt absolute Rec.FindHandle;
begin
  Result:= 2;
  FillChar({%H-}Rec, SizeOf(TSearchRec), 0);
  FindHandle:= PtrInt(lua_tointeger(L, 1));
  if FindNext(Rec) = 0 then
    luaPushSearchRec(L, Rec)
  else begin
    lua_pushnil(L);
    lua_pushnil(L);
  end;
end;

function luaFindClose(L : Plua_State) : Integer; cdecl;
var
  Rec: TSearchRec;
  FindHandle: PtrInt absolute Rec.FindHandle;
begin
  Result:= 0;
  FillChar({%H-}Rec, SizeOf(TSearchRec), 0);
  FindHandle:= PtrInt(lua_tointeger(L, 1));
  FindClose(Rec);
end;

function luaSleep(L : Plua_State) : Integer; cdecl;
begin
  Result:= 0;
  Sleep(lua_tointeger(L, 1));
end;

function luaFileGetAttr(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushinteger(L, FileGetAttr(mbFileNameToNative(lua_tostring(L, 1))));
end;

function luaFileExists(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushboolean(L, mbFileExists(lua_tostring(L, 1)));
end;

function luaDirectoryExists(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushboolean(L, mbDirectoryExists(lua_tostring(L, 1)));
end;

function luaClipbrdClear(L : Plua_State) : Integer; cdecl;
begin
  Result:= 0;
  Clipboard.Clear;
end;

function luaClipbrdGetText(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, PAnsiChar(Clipboard.AsText));
end;

function luaClipbrdSetText(L : Plua_State) : Integer; cdecl;
begin
  Result:= 0;
  ClipboardSetText(luaL_checkstring(L, 1));
end;

{$if lcl_fullversion >= 1070000}
function luaClipbrdSetHtml(L : Plua_State) : Integer; cdecl;
begin
  Result:= 0;
  Clipboard.SetAsHtml(luaL_checkstring(L, 1));
end;
{$endif}

function luaMessageBox(L : Plua_State) : Integer; cdecl;
var
  flags: Integer;
  text, caption: PAnsiChar;
begin
  Result:= 1;
  text:= luaL_checkstring(L, 1);
  caption:= luaL_checkstring(L, 2);
  flags:= Integer(lua_tointeger(L, 3));
  flags:= ShowMessageBox(text, caption, flags);
  lua_pushinteger(L, flags);
end;

function luaInputQuery(L : Plua_State) : Integer; cdecl;
var
  AValue: String;
  AMaskInput: Boolean;
  APrompt, ACaption: PAnsiChar;
begin
  Result:= 1;
  ACaption:= luaL_checkstring(L, 1);
  APrompt:= luaL_checkstring(L, 2);
  AMaskInput:= lua_toboolean(L, 3);
  AValue:= luaL_checkstring(L, 4);
  AMaskInput:= ShowInputQuery(ACaption, APrompt, AMaskInput, AValue);
  lua_pushboolean(L, AMaskInput);
  if AMaskInput then
  begin
    Result:= 2;
    lua_pushstring(L, PAnsiChar(AValue));
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

function luaExecuteCommand(L : Plua_State) : Integer; cdecl;
var
  Index,
  Count: Integer;
  Command: String;
  Args: array of String;
  Res: TCommandFuncResult;
begin
  Result:= 1;
  Res:= cfrNotFound;
  Count:= lua_gettop(L);
  if Count > 0 then
  begin
    // Get command
    Command:= lua_tostring(L, 1);
    // Get parameters
    SetLength(Args, Count - 1);
    for Index:= 2 to Count do
     Args[Index - 2]:= lua_tostring(L, Index);
    // Execute internal command
    Res:= frmMain.Commands.Commands.ExecuteCommand(Command, Args);
    Application.ProcessMessages;
  end;
  lua_pushboolean(L, Res = cfrSuccess);
end;

procedure luaP_register(L : Plua_State; n : PChar; f : lua_CFunction);
begin
  lua_pushcfunction(L, f);
  lua_setfield(L, -2, n);
end;

procedure luaC_register(L : Plua_State; n : PChar; c : PChar);
begin
  lua_pushstring(L, c);
  lua_setfield(L, -2, n);
end;

procedure luaP_copyfenv(L: Plua_State; Idx: Integer; N: PAnsiChar);
begin
  lua_getfield(L, -2, N);
  lua_getfenv(L, Idx);
  lua_setfenv(L, -2);
  lua_pop(L, 1);
end;

procedure ReplaceLib(L: Plua_State);
var
  Index: Integer;
begin
  // Get 'popen' environment
  lua_getglobal(L, LUA_IOLIBNAME);
    lua_getfield(L, -1, 'popen');
  lua_replace(L, 1);

  // Replace 'io' library functions
  lua_getglobal(L, LUA_IOLIBNAME);
    luaP_register(L, 'open', @io_open);
    luaP_register(L, 'lines', @io_lines);
    luaP_register(L, 'popen', @io_popen);
    // Get 'close' environment
    lua_getfield(L, -1, 'close');
    Index:= lua_gettop(L);
    // Set correct environment
    luaP_copyfenv(L, 1, 'popen');
    luaP_copyfenv(L, Index, 'open');
    luaP_copyfenv(L, Index, 'lines');
  lua_pop(L, 3);
end;

procedure RegisterPackages(L: Plua_State);
begin
  lua_newtable(L);
    luaP_register(L, 'Sleep', @luaSleep);
    luaP_register(L, 'FindNext', @luaFindNext);
    luaP_register(L, 'FindFirst', @luaFindFirst);
    luaP_register(L, 'FindClose', @luaFindClose);
    luaP_register(L, 'FileExists', @luaFileExists);
    luaP_register(L, 'FileGetAttr', @luaFileGetAttr);
    luaP_register(L, 'DirectoryExists', @luaDirectoryExists);
    luaC_register(L, 'PathDelim', PathDelim);
  lua_setglobal(L, 'SysUtils');

  lua_newtable(L);
    luaP_register(L, 'Clear', @luaClipbrdClear);
    luaP_register(L, 'GetAsText', @luaClipbrdGetText);
    luaP_register(L, 'SetAsText', @luaClipbrdSetText);
{$if lcl_fullversion >= 1070000}
    luaP_register(L, 'SetAsHtml', @luaClipbrdSetHtml);
{$endif}
  lua_setglobal(L, 'Clipbrd');

  lua_newtable(L);
    luaP_register(L, 'MessageBox', @luaMessageBox);
    luaP_register(L, 'InputQuery', @luaInputQuery);
  lua_setglobal(L, 'Dialogs');

  lua_newtable(L);
    luaP_register(L, 'ExecuteCommand', @luaExecuteCommand);
  lua_setglobal(L, 'DC');

  lua_getglobal(L, LUA_OSLIBNAME);
    luaP_register(L, 'remove', @luaRemove);
    luaP_register(L, 'execute', @luaExecute);
    luaP_register(L, 'tmpname', @luaTempName);
    luaP_register(L, 'rename', @luaRenameFile);
    luaP_register(L, 'getenv', @luaGetEnvironmentVariable);
  lua_pop(L, 1);

  if LuaCheckVersion then ReplaceLib(L);
end;

procedure SetPackagePath(L: Plua_State; const Path: String);
var
  APath: String;
begin
  lua_getglobal(L, 'package');
    lua_getfield(L, -1, 'path');
      APath := lua_tostring(L, -1);
      APath := APath + ';' + Path + '?.lua';
    lua_pop(L, 1);
    lua_pushstring(L, PAnsiChar(APath));
    lua_setfield(L, -2, 'path');
  lua_pop(L, 1);
end;

function LuaPCall(L: Plua_State; nargs, nresults: Integer): Boolean;
var
  Status: Integer;
begin
  Status:= lua_pcall(L, nargs, nresults, 0);
  // Check execution result
  if Status <> 0 then begin
    logWrite(lua_tostring(L, -1), lmtError, True, False);
  end;
  Result:= (Status = 0);
end;

function ExecuteScript(const FileName: String; Args: array of String): Boolean;
var
  L: Plua_State;
  Index: Integer;
  Count: Integer;
  Script: String;
  Status: Integer;
begin
  Result:= False;

  // Load Lua library
  if not IsLuaLibLoaded then
  begin
    if not LoadLuaLib(gLuaLib) then Exit;
  end;

  // Get script file name
  Script:= mbFileNameToSysEnc(FileName);

  L := lua_open;
  if Assigned(L) then
  begin
    luaL_openlibs(L);
    RegisterPackages(L);

    // Load script from file
    Status := luaL_loadfile(L, PAnsiChar(Script));
    if (Status = 0) then
    begin
      // Push arguments
      Count:= Length(Args);
      if (Count > 0) then
      begin
        for Index := 0 to Count - 1 do begin
          lua_pushstring(L, PAnsiChar(Args[Index]));
        end;
      end;
      // Execute script
      Status := lua_pcall(L, Count, 0, 0)
    end;

    // Check execution result
    if Status <> 0 then begin
      MessageDlg(lua_tostring(L, -1), mtError, [mbOK], 0);
    end;

    lua_close(L);

    Result:= (Status = 0);
  end;
end;

end.

