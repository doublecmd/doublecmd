{
   Double commander
   -------------------------------------------------------------------------
   Push some useful functions to Lua

   Copyright (C) 2016 Alexander Koblov (alexx2000@mail.ru)

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
function ExecuteScript(const FileName: String; Args: array of String): Boolean;

implementation

uses
  Forms, Dialogs, LazUTF8, DCOSUtils, fMain, uFormCommands, uOSUtils, uGlobs;

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

procedure RegisterPackages(L: Plua_State);
begin
  lua_newtable(L);
    luaP_register(L, 'Sleep', @luaSleep);
    luaP_register(L, 'FileExists', @luaFileExists);
    luaP_register(L, 'FileGetAttr', @luaFileGetAttr);
    luaP_register(L, 'DirectoryExists', @luaDirectoryExists);
  lua_setglobal(L, 'SysUtils');

  lua_newtable(L);
    luaP_register(L, 'ExecuteCommand', @luaExecuteCommand);
  lua_setglobal(L, 'DC');
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

    // Push arguments
    Count:= Length(Args);
    if (Count > 0) then
    begin
      for Index := 0 to Count - 1 do begin
        lua_pushstring(L, PAnsiChar(Args[Index]));
      end;
      lua_createtable(L, Count, 0);
      lua_setglobal(L, 'arg');
    end;

    // Execute script
    Status := luaL_loadfile(L, PAnsiChar(Script));
    lua_insert(L, -(Count + 1));
    if (Status = 0) then
      Status := lua_pcall(L, Count, 0, 0)
    else begin
      lua_pop(L, Count);
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

