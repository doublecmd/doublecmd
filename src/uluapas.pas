{
   Double commander
   -------------------------------------------------------------------------
   Push some useful functions to Lua

   Copyright (C) 2016-2021 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uLuaPas;

{$mode objfpc}{$H+}

interface

uses
  uDCUtils, Classes, SysUtils, Lua;

procedure RegisterPackages(L : Plua_State);
procedure SetPackagePath(L: Plua_State; const Path: String);
function LuaPCall(L : Plua_State; nargs, nresults : Integer): Boolean;
function ExecuteScript(const FileName: String; Args: array of String; var sErrorToReportIfAny:string): Boolean;

implementation

uses
  Forms, Dialogs, Clipbrd, LazUTF8, LCLVersion, uLng, DCOSUtils,
  DCConvertEncoding, fMain, uFormCommands, uOSUtils, uGlobs, uLog,
  uClipboard, uShowMsg, uLuaStd, uFindEx, uConvEncoding, uFileProcs,
  uFilePanelSelect, uMasks;

procedure luaPushSearchRec(L : Plua_State; Rec: PSearchRecEx);
begin
  lua_pushlightuserdata(L, Rec);
  lua_newtable(L);
  lua_pushinteger(L, Rec^.Time);
  lua_setfield(L, -2, 'Time');
  lua_pushinteger(L, Rec^.Size);
  lua_setfield(L, -2, 'Size');
  lua_pushinteger(L, Rec^.Attr);
  lua_setfield(L, -2, 'Attr');
  lua_pushstring(L, PAnsiChar(Rec^.Name));
  lua_setfield(L, -2, 'Name');
end;

function luaFindFirst(L : Plua_State) : Integer; cdecl;
var
  Path: String;
  Rec: PSearchRecEx;
begin
  New(Rec);
  Path:= lua_tostring(L, 1);
  if FindFirstEx(Path, fffPortable, Rec^) = 0 then
  begin
    Result:= 2;
    luaPushSearchRec(L, Rec);
  end
  else begin
    FindCloseEx(Rec^);
    lua_pushnil(L);
    Dispose(Rec);
    Result:= 1;
  end;
end;

function luaFindNext(L : Plua_State) : Integer; cdecl;
var
  Rec: PSearchRecEx;
begin
  Rec:= lua_touserdata(L, 1);
  if (Rec <> nil) and (FindNextEx(Rec^) = 0) then
  begin
    Result:= 2;
    luaPushSearchRec(L, Rec);
  end
  else begin
    lua_pushnil(L);
    Result:= 1;
  end;
end;

function luaFindClose(L : Plua_State) : Integer; cdecl;
var
  Rec: PSearchRecEx;
begin
  Rec:= lua_touserdata(L, 1);
  if Assigned(Rec) then
  begin
    FindCloseEx(Rec^);
    Dispose(Rec);
  end;
  Result:= 0;
end;

function luaSleep(L : Plua_State) : Integer; cdecl;
begin
  Result:= 0;
  Sleep(lua_tointeger(L, 1));
end;

function luaGetTickCount(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushinteger(L, GetTickCount64);
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

function luaCreateDirectory(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushboolean(L, mbForceDirectory(lua_tostring(L, 1)));
end;

function luaCreateHardLink(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushboolean(L, CreateHardLink(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function luaCreateSymbolicLink(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushboolean(L, CreateSymLink(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function luaReadSymbolicLink(L : Plua_State) : Integer; cdecl;
var
  Path: String;
  Recursive: Boolean = False;
begin
  Result:= 1;
  Path:= lua_tostring(L, 1);
  if lua_isboolean(L, 2) then begin
    Recursive:= lua_toboolean(L, 2)
  end;
  if Recursive then
    Path:= mbReadAllLinks(Path)
  else begin
    Path:= ReadSymLink(Path);
  end;
  lua_pushstring(L, Path);
end;

function luaExtractFilePath(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, ExtractFilePath(lua_tostring(L, 1)));
end;

function luaExtractFileDrive(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, ExtractFileDrive(lua_tostring(L, 1)));
end;

function luaExtractFileName(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, ExtractFileName(lua_tostring(L, 1)));
end;

function luaExtractFileExt(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, ExtractFileExt(lua_tostring(L, 1)));
end;

function luaMatchesMask(L : Plua_State) : Integer; cdecl;
var
  FileName: String;
  FileMask: String;
  AOptions: TMaskOptions;
begin
  Result:= 1;
  FileName:= lua_tostring(L, 1);
  FileMask:= lua_tostring(L, 2);
  if lua_isnumber(L, 3) then
    AOptions:= TMaskOptions(Integer(lua_tointeger(L, 3)))
  else begin
    AOptions:= [];
  end;
  lua_pushboolean(L, MatchesMask(FileName, FileMask, AOptions));
end;

function luaMatchesMaskList(L : Plua_State) : Integer; cdecl;
var
  FileName: String;
  FileMask: String;
  AOptions: TMaskOptions;
  ASeparatorCharset: String;
begin
  Result:= 1;
  FileName:= lua_tostring(L, 1);
  FileMask:= lua_tostring(L, 2);
  if lua_isstring(L, 3) then
    ASeparatorCharset:= lua_tostring(L, 3)
  else begin
    ASeparatorCharset:= ';';
  end;
  if lua_isnumber(L, 4) then
    AOptions:= TMaskOptions(Integer(lua_tointeger(L, 4)))
  else begin
    AOptions:= [];
  end;
  lua_pushboolean(L, MatchesMaskList(FileName, FileMask, ASeparatorCharset, AOptions));
end;

function luaExtractFileDir(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, ExtractFileDir(lua_tostring(L, 1)));
end;

function luaPos(L : Plua_State) : Integer; cdecl;
var
  Offset: SizeInt = 1;
  Search, Source: String;
begin
  Result:= 1;
  Search:= lua_tostring(L, 1);
  Source:= lua_tostring(L, 2);
  if lua_isinteger(L, 3) then begin
    Offset:= lua_tointeger(L, 3)
  end;
  lua_pushinteger(L, UTF8Pos(Search, Source, Offset));
end;

function luaCopy(L : Plua_State) : Integer; cdecl;
var
  S: String;
  Start, Count: PtrInt;
begin
  Result:= 1;
  S:= lua_tostring(L, 1);
  Start:= lua_tointeger(L, 2);
  Count:= lua_tointeger(L, 3);
  S:= UTF8Copy(S, Start, Count);
  lua_pushstring(L, PAnsiChar(S));
end;

function luaLength(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushinteger(L, UTF8Length(lua_tostring(L, 1)));
end;

function luaUpperCase(L : Plua_State) : Integer; cdecl;
var
  S: String;
begin
  Result:= 1;
  S:= lua_tostring(L, 1);
  S:= UTF8UpperCase(S);
  lua_pushstring(L, PAnsiChar(S));
end;

function luaLowerCase(L : Plua_State) : Integer; cdecl;
var
  S: String;
begin
  Result:= 1;
  S:= lua_tostring(L, 1);
  S:= UTF8LowerCase(S);
  lua_pushstring(L, PAnsiChar(S));
end;

function luaConvertEncoding(L : Plua_State) : Integer; cdecl;
var
  S, FromEnc, ToEnc: String;
begin
  Result:= 1;
  S:= lua_tostring(L, 1);
  FromEnc:= lua_tostring(L, 2);
  ToEnc:= lua_tostring(L, 3);
  lua_pushstring(L, ConvertEncoding(S, FromEnc, ToEnc));
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

function luaClipbrdSetHtml(L : Plua_State) : Integer; cdecl;
begin
  Result:= 0;
  Clipboard.SetAsHtml(luaL_checkstring(L, 1));
end;

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

function luaInputListBox(L : Plua_State) : Integer; cdecl;
var
  AValue: String = '';
  AIndex, ACount: Integer;
  AStringList: TStringList;
  APrompt, ACaption: PAnsiChar;
begin
  Result:= 1;
  if (lua_gettop(L) < 3) or (not lua_istable(L, 3)) then
  begin
    lua_pushnil(L);
    Exit;
  end;
  ACaption:= lua_tocstring(L, 1);
  APrompt:= lua_tocstring(L, 2);
  ACount:= lua_objlen(L, 3);
  AStringList:= TStringList.Create;
  for AIndex := 1 to ACount do
  begin
    lua_rawgeti(L, 3, AIndex);
    AStringList.Add(luaL_checkstring(L, -1));
    lua_pop(L, 1);
  end;
  if lua_isstring(L, 4) then begin
    AValue:= lua_tostring(L, 4);
  end;
  if ShowInputListBox(ACaption, APrompt, AStringList, AValue, AIndex) then
    lua_pushstring(L, PAnsiChar(AValue))
  else begin
    lua_pushnil(L);
  end;
  AStringList.Free;
end;

function luaLogWrite(L : Plua_State) : Integer; cdecl;
var
  sText: String;
  bForce: Boolean = True;
  bLogFile: Boolean = False;
  LogMsgType: TLogMsgType = lmtInfo;
begin
  Result:= 0;
  sText:= lua_tostring(L, 1);
  if lua_isnumber(L, 2) then
    LogMsgType:= TLogMsgType(lua_tointeger(L, 2));
  if lua_isboolean(L, 3) then
    bForce:= lua_toboolean(L, 3);
  if lua_isboolean(L, 4) then
    bLogFile:= lua_toboolean(L, 4);
  logWrite(sText, LogMsgType, bForce, bLogFile);
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

function luaCurrentPanel(L : Plua_State) : Integer; cdecl;
var
  Count: Integer;
begin
  Result:= 1;
  Count:= lua_gettop(L);
  lua_pushinteger(L, Integer(frmMain.SelectedPanel));
  if (Count > 0) then
    frmMain.SetActiveFrame(TFilePanelSelect(lua_tointeger(L, 1)));
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

procedure RegisterPackages(L: Plua_State);
begin
  lua_newtable(L);
    luaP_register(L, 'Sleep', @luaSleep);
    luaP_register(L, 'FindNext', @luaFindNext);
    luaP_register(L, 'FindFirst', @luaFindFirst);
    luaP_register(L, 'FindClose', @luaFindClose);
    luaP_register(L, 'FileExists', @luaFileExists);
    luaP_register(L, 'FileGetAttr', @luaFileGetAttr);
    luaP_register(L, 'GetTickCount', @luaGetTickCount);
    luaP_register(L, 'DirectoryExists', @luaDirectoryExists);
    luaP_register(L, 'CreateDirectory', @luaCreateDirectory);

    luaP_register(L, 'CreateHardLink', @luaCreateHardLink);
    luaP_register(L, 'CreateSymbolicLink', @luaCreateSymbolicLink);
    luaP_register(L, 'ReadSymbolicLink', @luaReadSymbolicLink);

    luaP_register(L, 'ExtractFileExt', @luaExtractFileExt);
    luaP_register(L, 'ExtractFileDir', @luaExtractFileDir);
    luaP_register(L, 'ExtractFilePath', @luaExtractFilePath);
    luaP_register(L, 'ExtractFileName', @luaExtractFileName);
    luaP_register(L, 'ExtractFileDrive', @luaExtractFileDrive);

    luaP_register(L, 'MatchesMask', @luaMatchesMask);
    luaP_register(L, 'MatchesMaskList', @luaMatchesMaskList);

    luaC_register(L, 'PathDelim', PathDelim);
  lua_setglobal(L, 'SysUtils');

  lua_newtable(L);
    luaP_register(L, 'Pos', @luaPos);
    luaP_register(L, 'Copy', @luaCopy);
    luaP_register(L, 'Length', @luaLength);
    luaP_register(L, 'UpperCase', @luaUpperCase);
    luaP_register(L, 'LowerCase', @luaLowerCase);
    luaP_register(L, 'ConvertEncoding', @luaConvertEncoding);
  lua_setglobal(L, 'LazUtf8');

  lua_newtable(L);
    luaP_register(L, 'Clear', @luaClipbrdClear);
    luaP_register(L, 'GetAsText', @luaClipbrdGetText);
    luaP_register(L, 'SetAsText', @luaClipbrdSetText);
    luaP_register(L, 'SetAsHtml', @luaClipbrdSetHtml);
  lua_setglobal(L, 'Clipbrd');

  lua_newtable(L);
    luaP_register(L, 'MessageBox', @luaMessageBox);
    luaP_register(L, 'InputQuery', @luaInputQuery);
    luaP_register(L, 'InputListBox', @luaInputListBox);
  lua_setglobal(L, 'Dialogs');

  lua_newtable(L);
    luaP_register(L, 'LogWrite', @luaLogWrite);
    luaP_register(L, 'CurrentPanel', @luaCurrentPanel);
    luaP_register(L, 'ExecuteCommand', @luaExecuteCommand);
  lua_setglobal(L, 'DC');

  ReplaceLibrary(L);
end;

procedure SetPackagePath(L: Plua_State; const Path: String);
var
  APath: String;
begin
  lua_getglobal(L, 'package');
    // Set package.path
    lua_getfield(L, -1, 'path');
      APath := lua_tostring(L, -1);
      APath := StringReplace(APath, '.' + PathDelim, Path, []);
    lua_pop(L, 1);
    lua_pushstring(L, PAnsiChar(APath));
    lua_setfield(L, -2, 'path');
    // Set package.cpath
    lua_getfield(L, -1, 'cpath');
      APath := lua_tostring(L, -1);
      APath := StringReplace(APath, '.' + PathDelim, Path, []);
    lua_pop(L, 1);
    lua_pushstring(L, PAnsiChar(APath));
    lua_setfield(L, -2, 'cpath');
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

function ExecuteScript(const FileName: String; Args: array of String; var sErrorToReportIfAny:string): Boolean;
var
  L: Plua_State;
  Index: Integer;
  Count: Integer;
  Script: String;
  Status: Integer;
begin
  Result:= False;
  sErrorToReportIfAny := '';

  // Load Lua library
  if not IsLuaLibLoaded then
  begin
    if not LoadLuaLib(mbExpandFileName(gLuaLib)) then
    begin
      sErrorToReportIfAny := Format(rsMsgScriptCantFindLibrary, [gLuaLib]);
      Exit;
    end;
  end;

  // Get script file name
  Script:= mbFileNameToSysEnc(FileName);

  L := lua_open;
  if Assigned(L) then
  begin
    luaL_openlibs(L);
    RegisterPackages(L);
    SetPackagePath(L, ExtractFilePath(Script));

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
      Script:= lua_tostring(L, -1);
      MessageDlg(CeRawToUtf8(Script), mtError, [mbOK], 0);
    end;

    lua_close(L);

    Result:= (Status = 0);
  end;
end;

end.

