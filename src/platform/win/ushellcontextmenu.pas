{
    Double Commander
    -------------------------------------------------------------------------
    Shell context menu implementation.

    Copyright (C) 2006-2010  Koblov Alexander (Alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uShellContextMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFile, Windows, ShellAPI, ComObj, ShlObj, ActiveX,
  JwaShlGuid, uShlObjAdditional;

type

  { TShellContextMenu }

  TShellContextMenu = class(TInterfacedObject, IContextMenu2)
  private
    FShellNew: Boolean;
    FShellMenuMin  : UINT; // Shell menu first command ID
    FShellMenuMax  : UINT; // Shell menu last command ID
    FShellNewMenuMin  : UINT; // New menu first command ID
    FShellNewMenuMax  : UINT; // New menu last command ID
    FShellMenu1    : IContextMenu;
    FShellMenu2    : IContextMenu2;
    FShellNewMenu2 : IContextMenu2;
    FShellNewMenu  : HMENU;
  public
    constructor Create(Handle : THandle; Files : TFiles; ShellNew: Boolean);
    destructor Destroy; override;
    // *** IContextMenu ***
    function QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult; stdcall;
    // *** IContextMenu2 ***
    function HandleMenuMsg(uMsg: UINT; wParam : WPARAM; lParam : LPARAM): HResult; stdcall;
  end;

implementation

uses
  uMyWindows;

{ TShellContextMenu }

constructor TShellContextMenu.Create(Handle: THandle; Files: TFiles; ShellNew: Boolean);
type
  PPIDLArray = ^PItemIDList;

var
  Folder,
  DesktopFolder: IShellFolder;
  newMenu: IShellExtInit;
  PathPIDL: PItemIDList = nil;
  tmpPIDL: PItemIDList = nil;
  S: WideString;
  List: PPIDLArray = nil;
  I : Integer;
  pchEaten: ULONG;
  dwAttributes: ULONG = 0;
begin
  FShellNew:= ShellNew;
  FShellMenu1 := nil;
  FShellMenu2 := nil;
  FShellNewMenu2 := nil;
  FShellNewMenu := INVALID_HANDLE_VALUE;

  OleCheckUTF8(SHGetDesktopFolder(DesktopFolder));
  try
    List := CoTaskMemAlloc(SizeOf(PItemIDList)*Files.Count);
    ZeroMemory(List, SizeOf(PItemIDList)*Files.Count);

    for I := 0 to Files.Count - 1 do
      begin
        if Files[I].Name = EmptyStr then
          S := EmptyStr
        else
          S := UTF8Decode(Files[I].Path);

        OleCheckUTF8(DeskTopFolder.ParseDisplayName(Handle, nil, PWideChar(S), pchEaten, PathPIDL, dwAttributes));
        try
          OleCheckUTF8(DeskTopFolder.BindToObject(PathPIDL, nil, IID_IShellFolder, Folder));
        finally
          CoTaskMemFree(PathPIDL);
        end;

        if Files[I].Name = EmptyStr then
          S := UTF8Decode(Files[I].Path)
        else
          S := UTF8Decode(Files[I].Name);

        OleCheckUTF8(Folder.ParseDisplayName(Handle, nil, PWideChar(S), pchEaten, tmpPIDL, dwAttributes));
        (List + i)^ := tmpPIDL;
      end;

    Folder.GetUIObjectOf(Handle, Files.Count, PItemIDList(List^), IID_IContextMenu, nil, FShellMenu1);

    FShellMenu1.QueryInterface(IID_IContextMenu2, FShellMenu2); // to handle submenus.

    // Add "New" submenu if needed
    if FShellNew and (Files.Count = 1) and Files[0].IsDirectory then
    begin
      S:= UTF8Decode(Files[0].FullPath);
      OleCheckUTF8(DesktopFolder.ParseDisplayName(Handle, nil, PWideChar(S), pchEaten, PathPIDL, dwAttributes));
      try
        CoCreateInstance(CLSID_NewMenu, nil, CLSCTX_ALL, IID_IShellExtInit, newMenu);
        newMenu.Initialize(PathPIDL, nil, 0);
        newMenu.QueryInterface(IID_IContextMenu2, FShellNewMenu2);
      finally
        newMenu:= nil;
        CoTaskMemFree(PathPIDL);
      end;
    end;

  finally
    if Assigned(List) then
    begin
      for I := 0 to Files.Count - 1 do
        if Assigned((List + i)^) then
          CoTaskMemFree((List + i)^);
      CoTaskMemFree(List);
    end;

    DesktopFolder:= nil;
  end;
end;

destructor TShellContextMenu.Destroy;
begin
  FShellMenu1:= nil;
  FShellMenu2:= nil;
  FShellNewMenu2  := nil;
  if FShellNewMenu <> INVALID_HANDLE_VALUE then
    DestroyMenu(FShellNewMenu);
  inherited Destroy;
end;

function TShellContextMenu.QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst,
  idCmdLast, uFlags: UINT): HResult; stdcall;
var
  iPos: LongInt;
  wsText: WideString;
begin
  FShellMenuMin:= idCmdFirst;
  FShellMenuMax:= (idCmdLast - idCmdFirst) div 2;
  FShellNewMenuMin:= FShellMenuMax + 1;
  FShellNewMenuMax:= idCmdLast;

  Result:= FShellMenu1.QueryContextMenu(Menu, indexMenu, FShellMenuMin, FShellMenuMax, uFlags);
  iPos:= GetMenuItemCount(Menu);
  if Assigned(FShellNewMenu2) and (iPos > 0) then
  begin
    FShellNewMenu:= CreatePopupMenu;
    FShellNewMenu2.QueryContextMenu(FShellNewMenu, indexMenu, FShellNewMenuMin, FShellNewMenuMax, uFlags);
    if GetMenuItemCount(FShellNewMenu) > 0 then
    begin
      wsText:= GetMenuItemText(FShellNewMenu, 0, True);
      InsertMenuItemEx(Menu, FShellNewMenu, PWideChar(wsText), iPos - 2, FShellMenuMax, MFT_STRING);
    end;
  end;
end;

function TShellContextMenu.InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
var
  idCmd: UINT;
begin
  idCmd:= UINT(lpici.lpVerb) + 1;
  if (not FShellNew) or ((idCmd >= FShellMenuMin) and (idCmd <= FShellMenuMax)) then
    Result:= FShellMenu1.InvokeCommand(lpici)
  else if (idCmd >= FShellNewMenuMin) and (idCmd <= FShellNewMenuMax) then
    Result:= FShellNewMenu2.InvokeCommand(lpici)
  else
    Result:= E_NOTIMPL;
end;

function TShellContextMenu.GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
  pszName: LPSTR; cchMax: UINT): HResult; stdcall;
begin
  if (idCmd >= FShellMenuMin) and (idCmd <= FShellMenuMax) then
    Result:= FShellMenu1.GetCommandString(idCmd, uType, pwReserved, pszName, cchMax)
  else if (idCmd >= FShellNewMenuMin) and (idCmd <= FShellNewMenuMax) then
    Result:= FShellNewMenu2.GetCommandString(idCmd, uType, pwReserved, pszName, cchMax)
  else
    Result:= E_NOTIMPL;
end;

function TShellContextMenu.HandleMenuMsg(uMsg: UINT; wParam: WPARAM; lParam: LPARAM
  ): HResult; stdcall;
var
  itemID : UINT;
begin
  Result:= E_NOTIMPL;
  case uMsg of
    WM_INITMENUPOPUP:
      begin
        // wParam is a handle to the drop-down menu or submenu
        if wParam = FShellNewMenu then
          Result:= FShellNewMenu2.HandleMenuMsg(uMsg, wParam, lParam)
        else if Assigned(FShellMenu2) then
          Result:= FShellMenu2.HandleMenuMsg(uMsg, wParam, lParam);
      end;
    WM_DRAWITEM:
      begin
        itemID:= PDRAWITEMSTRUCT(lParam)^.itemID;
        if (itemID >= FShellMenuMin) and (itemID <= FShellMenuMax) then
          Result:= FShellMenu2.HandleMenuMsg(uMsg, wParam, lParam)
        else if (itemID >= FShellNewMenuMin) and (itemID <= FShellNewMenuMax) and Assigned(FShellNewMenu2) then
          Result:= FShellNewMenu2.HandleMenuMsg(uMsg, wParam, lParam);
      end;
    WM_MENUCHAR:
      begin
        // lParam is a handle to the active menu
        if lParam = FShellNewMenu then
          Result:= FShellNewMenu2.HandleMenuMsg(uMsg, wParam, lParam)
        else if Assigned(FShellMenu2) then
          Result:= FShellMenu2.HandleMenuMsg(uMsg, wParam, lParam);
      end;
    WM_MEASUREITEM:
      begin
        itemID:= PMEASUREITEMSTRUCT(lParam)^.itemID;
        if (itemID >= FShellMenuMin) and (itemID <= FShellMenuMax) then
          Result:= FShellMenu2.HandleMenuMsg(uMsg, wParam, lParam)
        else if (itemID >= FShellNewMenuMin) and (itemID <= FShellNewMenuMax) and Assigned(FShellNewMenu2) then
          Result:= FShellNewMenu2.HandleMenuMsg(uMsg, wParam, lParam);
      end;
  end;
end;

end.

