{
   Double Commander
   -------------------------------------------------------------------------
   Interface to GIO - GLib Input, Output and Streaming Library
   This unit loads all libraries dynamically so it can work without it

   Copyright (C) 2011 Alexander Koblov (alexx2000@mail.ru)

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

unit uGio;

{$mode delphi}
{$packrecords c}

interface

uses
  Classes, SysUtils;

type
  gpointer  = pointer;
  gboolean  = longbool;
  Pgchar    = PChar;
  PGFile    = type Pointer;
  PGAppInfo = type Pointer;
  PGCancellable = type Pointer;
  PGAppLaunchContext = type Pointer;

type
  PGList = ^TGList;
  TGList = record
    data: gpointer;
    next: PGList;
    prev: PGList;
  end;

function GioOpen(const Uri: UTF8String): Boolean;

var
  HasGio: Boolean = False;

implementation

uses
  DynLibs;

const
  gobjectlib = 'libgobject-2.0.so.0';
  giolib     = 'libgio-2.0.so.0';

var
  hgobjectlib: TLibHandle = 0;
  hgiolib: TLibHandle = 0;

var
  g_object_unref: procedure(anObject: gpointer); cdecl;
  g_file_is_native: function(AFile: PGFile): gboolean; cdecl;
  g_file_new_for_commandline_arg: function(arg: Pgchar): PGFile; cdecl;
  g_file_query_default_handler: function(AFile: PGFile; cancellable: PGCancellable): PGAppInfo; cdecl;
  g_app_info_launch: function(AAppInfo: PGAppInfo; files: PGList; launch_context: PGAppLaunchContext): gboolean; cdecl;
  g_app_info_launch_uris: function(AAppInfo: PGAppInfo; uris: PGList; launch_context: PGAppLaunchContext): gboolean; cdecl;

function GioOpen(const Uri: UTF8String): Boolean;
var
  AFile: PGFile;
  AFileList: TGList;
  AppInfo: PGAppInfo;
begin
  Result:= False;
  AFileList.next:= nil;
  AFileList.prev:= nil;
  if not HasGio then Exit;
  AFile:= g_file_new_for_commandline_arg(Pgchar(Uri));
  try
    AppInfo:= g_file_query_default_handler(AFile, nil);
    if (AppInfo = nil) then Exit;
    if g_file_is_native(AFile) then
      begin
        AFileList.data:= AFile;
        Result:= g_app_info_launch (AppInfo, @AFileList, nil);
      end
    else
      begin
        AFileList.data:= Pgchar(Uri);
        Result:= g_app_info_launch_uris (AppInfo, @AFileList, nil);
      end;
    g_object_unref(AppInfo);
  finally
    g_object_unref(AFile);
  end;
end;

initialization
  // Load GObject library
  hgobjectlib:= LoadLibrary(gobjectlib);
  if hgobjectlib <> 0 then
  begin
    @g_object_unref:= GetProcedureAddress(hgobjectlib, 'g_object_unref');
    // Load GIO library
    hgiolib:= LoadLibrary(giolib);
    HasGio:= (hgiolib <> 0);
    if HasGio then
    begin
      @g_file_is_native:= GetProcedureAddress(hgiolib, 'g_file_is_native');
      @g_file_new_for_commandline_arg:= GetProcedureAddress(hgiolib, 'g_file_new_for_commandline_arg');
      @g_file_query_default_handler:= GetProcedureAddress(hgiolib, 'g_file_query_default_handler');
      @g_app_info_launch:= GetProcedureAddress(hgiolib, 'g_app_info_launch');
      @g_app_info_launch_uris:= GetProcedureAddress(hgiolib, 'g_app_info_launch_uris');
    end;
  end;

finalization
  if hgiolib <> 0 then FreeLibrary(hgiolib);
  if hgobjectlib <> 0 then FreeLibrary(hgobjectlib);

end.

