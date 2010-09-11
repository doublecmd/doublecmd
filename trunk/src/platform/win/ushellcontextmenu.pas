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
  Classes, SysUtils, uFile, Windows, ShellAPI, ComObj, ShlObj, ActiveX, JwaShlGuid, uShlObjAdditional;

function GetShellContextMenu(Handle: THandle; Files: TFiles; Background: Boolean): IContextMenu;

implementation

uses
  uMyWindows;

function GetForegroundContextMenu(Handle : THandle; Files : TFiles): IContextMenu;
type
  PPIDLArray = ^PItemIDList;

var
  Folder,
  DesktopFolder: IShellFolder;
  PathPIDL: PItemIDList = nil;
  tmpPIDL: PItemIDList = nil;
  S: WideString;
  List: PPIDLArray = nil;
  I : Integer;
  pchEaten: ULONG;
  dwAttributes: ULONG = 0;
begin
  Result := nil;

  OleCheckUTF8(SHGetDesktopFolder(DeskTopFolder));
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

    Folder.GetUIObjectOf(Handle, Files.Count, PItemIDList(List^), IID_IContextMenu, nil, Result);

  finally
    if Assigned(List) then
    begin
      for I := 0 to Files.Count - 1 do
        if Assigned((List + i)^) then
          CoTaskMemFree((List + i)^);
      CoTaskMemFree(List);
    end;

    Folder:= nil;
    DesktopFolder:= nil;
  end;
end;

function GetBackgroundContextMenu(Handle : THandle; Files : TFiles): IContextMenu;
var
  DesktopFolder, Folder: IShellFolder;
  wsFileName: WideString;
  PathPIDL: PItemIDList = nil;
  pchEaten: ULONG;
  dwAttributes: ULONG;
begin
  Result:= nil;

  if Files.Count > 0 then
  begin
    wsFileName:= UTF8Decode(Files[0].FullPath);
    OleCheckUTF8(SHGetDesktopFolder(DesktopFolder));
    try
      OleCheckUTF8(DesktopFolder.ParseDisplayName(Handle, nil, PWideChar(wsFileName), pchEaten, PathPIDL, dwAttributes));
      try
        OleCheckUTF8(DesktopFolder.BindToObject(PathPIDL, nil, IID_IShellFolder, Folder));
      finally
        CoTaskMemFree(PathPIDL);
      end;
      OleCheckUTF8(Folder.CreateViewObject(Handle, IID_IContextMenu, Result));
    finally
      Folder:= nil;
      DesktopFolder:= nil;
    end;
  end;
end;

function GetShellContextMenu(Handle: THandle; Files: TFiles; Background: Boolean): IContextMenu; inline;
begin
  if Background then
    Result:= GetBackgroundContextMenu(Handle, Files)
  else
    Result:= GetForegroundContextMenu(Handle, Files);
end;

end.

