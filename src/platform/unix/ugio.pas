{
   Double Commander
   -------------------------------------------------------------------------
   Interface to GIO - GLib Input, Output and Streaming Library
   This unit loads all libraries dynamically so it can work without it

   Copyright (C) 2011-2019 Alexander Koblov (alexx2000@mail.ru)

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
{$assertions on}

interface

uses
  Classes, SysUtils, DCBasicTypes, uGio2;

function GioOpen(const Uri: String): Boolean;
function GioNewFile(const Address: String): PGFile;
function GioGetIconTheme(const Scheme: String): String;
function GioFileGetIcon(const FileName: String): String;
function GioMimeTypeGetActions(const MimeType: String): TDynamicStringArray;
function GioGetMimeType(const FileName: String; MaxExtent: LongWord): String;

var
  HasGio: Boolean = True;

implementation

uses
  StrUtils, DCStrUtils, DCClassesUtf8, uGlib2, uGObject2;

function GioOpen(const Uri: String): Boolean;
var
  AFile: PGFile;
  AFileList: TGList;
  AppInfo: PGAppInfo;
begin
  Result:= False;
  AFileList.next:= nil;
  AFileList.prev:= nil;
  if not HasGio then Exit;
  AFile:= GioNewFile(Pgchar(Uri));
  try
    AppInfo:= g_file_query_default_handler(AFile, nil, nil);
    if (AppInfo = nil) then Exit;
    if g_file_is_native(AFile) then
      begin
        AFileList.data:= AFile;
        Result:= g_app_info_launch (AppInfo, @AFileList, nil, nil);
      end
    else
      begin
        AFileList.data:= Pgchar(Uri);
        Result:= g_app_info_launch_uris (AppInfo, @AFileList, nil, nil);
      end;
    g_object_unref(PGObject(AppInfo));
  finally
    g_object_unref(PGObject(AFile));
  end;
end;

function GioNewFile(const Address: String): PGFile;
var
  URI: Pgchar;
  Index: Integer;
begin
  Index:= Pos('://', Address);
  if Index = 0 then
    Result:= g_file_new_for_path(Pgchar(Address))
  else begin
    Index:= PosEx('/', Address, Index + 3);
    if (Index = 0) or (Index = Length(Address)) then
      Result:= g_file_new_for_uri(Pgchar(Address))
    else begin
      URI:= g_uri_escape_string(Pgchar(Address) + Index, ':/', True);
      if (URI = nil) then
        Result:= g_file_new_for_uri(Pgchar(Address))
      else begin
        Result:= g_file_new_for_uri(Pgchar(Copy(Address, 1, Index) + URI));
        g_free(URI);
      end;
    end;
  end;
end;

function GioGetIconTheme(const Scheme: String): String;
var
  Theme: Pgchar;
  Settings: PGSettings;
  SettingsSchema: PGSettingsSchema;
  SchemaSource: PGSettingsSchemaSource;
begin
  if not HasGio then Exit(EmptyStr);
  SchemaSource:= g_settings_schema_source_get_default();
  if Assigned(SchemaSource) then
  begin
    SettingsSchema:= g_settings_schema_source_lookup(SchemaSource, Pgchar(Scheme), False);
    if Assigned(SettingsSchema) then
    begin
      Settings:= g_settings_new(Pgchar(Scheme));
      if Assigned(Settings) then
      begin
        Theme:= g_settings_get_string(Settings, 'icon-theme');
        if Assigned(Theme) then
        begin
          Result:= StrPas(Theme);
          g_free(Theme);
        end;
        g_object_unref(Settings);
      end;
      g_object_unref(PGObject(SettingsSchema));
    end;
    g_object_unref(PGObject(SchemaSource));
  end;
end;

function GioFileGetIcon(const FileName: String): String;
var
  GFile: PGFile;
  GIcon: PGIcon;
  AIconList: PPgchar;
  GFileInfo: PGFileInfo;
begin
  Result:= EmptyStr;
  GFile:= GioNewFile(Pgchar(FileName));
  GFileInfo:= g_file_query_info(GFile, FILE_ATTRIBUTE_STANDARD_ICON, 0, nil, nil);
  if Assigned(GFileInfo) then
  begin
    GIcon:= g_file_info_get_icon(GFileInfo);
    if g_type_check_instance_is_a(PGTypeInstance(GIcon), g_themed_icon_get_type()) then
    begin
      AIconList:= g_themed_icon_get_names(PGThemedIcon(GIcon));
      if Assigned(AIconList) then Result:= AIconList[0];
    end;
    g_object_unref(GFileInfo);
  end;
  g_object_unref(PGObject(GFile));
end;

function GioMimeTypeGetActions(const MimeType: String): TDynamicStringArray;
var
  AppList,
  TempList: PGList;
  DesktopFile: PAnsiChar;
begin
  AppList:= g_app_info_get_all_for_type(PAnsiChar(MimeType));
  if Assigned(AppList) then
  begin
    TempList:= AppList;
    repeat
      DesktopFile:= g_app_info_get_id(TempList^.data);
      if Assigned(DesktopFile) then AddString(Result, DesktopFile);
      g_object_unref(TempList^.data);
      TempList:= TempList^.next;
    until TempList = nil;
    g_list_free(AppList);
  end;
end;

function GioGetMimeType(const FileName: String; MaxExtent: LongWord): String;
var
  Size: gsize;
  MimeType: Pgchar;
  Uncertain: gboolean;
  Buffer: array of Byte;
  FileStream: TFileStreamEx;
begin
  // First check by file name (fast)
  MimeType:= g_content_type_guess(Pgchar(FileName), nil, 0, @Uncertain);
  if Assigned(MimeType) then
  begin
    Result:= StrPas(MimeType);
    g_free(MimeType);
  end;
  // Second check by file content (slow)
  if Uncertain then
  begin
    if MaxExtent = 0 then
      Result:= 'text/plain'
    else begin
      SetLength(Buffer, MaxExtent);
      try
        FileStream:= TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
        try
          Size:= FileStream.Read(Buffer[0], MaxExtent);
        finally
          FileStream.Free;
        end;
        MimeType:= g_content_type_guess(nil, @Buffer[0], Size, @Uncertain);
        if Assigned(MimeType) then
        begin
          Result:= StrPas(MimeType);
          g_free(MimeType);
        end;
      except
        // Skip
      end;
    end;
  end;
end;

procedure Initialize;
begin
  try
    Assert(@g_file_is_native <> nil, 'g_file_is_native');
    Assert(@g_file_new_for_commandline_arg <> nil, 'g_file_new_for_commandline_arg');
    Assert(@g_file_query_default_handler <> nil, 'g_file_query_default_handler');
    Assert(@g_file_query_info <> nil, 'g_file_query_info');
    Assert(@g_file_info_get_icon <> nil, 'g_file_info_get_icon');
    Assert(@g_themed_icon_get_type <> nil, 'g_themed_icon_get_type');
    Assert(@g_themed_icon_get_names <> nil, 'g_themed_icon_get_names');
    Assert(@g_app_info_launch <> nil, 'g_app_info_launch');
    Assert(@g_app_info_launch_uris <> nil, 'g_app_info_launch_uris');
    Assert(@g_app_info_get_all_for_type <> nil, 'g_app_info_get_all_for_type');
    Assert(@g_app_info_get_id <> nil, 'g_app_info_get_id');
    Assert(@g_settings_new <> nil, 'g_settings_new');
    Assert(@g_settings_get_string <> nil, 'g_settings_get_string');
    Assert(@g_settings_schema_source_get_default <> nil, 'g_settings_schema_source_get_default');
    Assert(@g_settings_schema_source_lookup <> nil, 'g_settings_schema_source_lookup');
    Assert(@g_content_type_guess <> nil, 'g_content_type_guess');
  except
    on E: Exception do
    begin
      HasGio:= False;
      WriteLn(E.Message);
    end;
  end;
end;

initialization
  Initialize;

end.
