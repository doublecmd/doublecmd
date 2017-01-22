{
   Double Commander
   -------------------------------------------------------------------------
   Shared MIME-info Database - mime type guess

   Copyright (C) 2014-2017 Alexander Koblov (alexx2000@mail.ru)

   Based on PCManFM v0.5.1 (http://pcmanfm.sourceforge.net)
   Copyright (C) 2007 Houng Jen Yee (PCMan) <pcman.tw@gmail.com>

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.
}

unit uMimeType;

{$mode delphi}

interface

uses
  DCBasicTypes;

{en
   Get file mime type.
}
function GetFileMimeType(const FileName: String): String;
{en
   Get file mime type with parents.
}
function GetFileMimeTypes(const FileName: String): TDynamicStringArray;

implementation

uses
  Classes, SysUtils, Unix, BaseUnix, Math, LazFileUtils, DCStrUtils, DCOSUtils,
  uMimeCache, uXdg, uGio, uGlib2, uGio2;

const
  xdg_mime_type_plain_text = 'text/plain';
  xdg_mime_type_directory = 'inode/directory';
  xdg_mime_type_unknown = 'application/octet-stream';
  xdg_mime_type_executable = 'application/x-executable';

var
  caches: TFPList = nil;
  mime_magic_buf: PByte;
  mime_cache_max_extent: LongWord = 0;
  CriticalSection: TRTLCriticalSection;

(* load all mime.cache files on the system,
 * including /usr/share/mime/mime.cache,
 * /usr/local/share/mime/mime.cache,
 * and $HOME/.local/share/mime/mime.cache. *)
procedure mime_cache_load_all();
const
  MIME_CACHE = 'mime/mime.cache';
var
  I: Integer;
  Cache: PMimeCache;
  FileName: String;
  Path: TDynamicStringArray;
begin
  caches := TFPList.Create;
  Path := GetSystemDataDirs;
  AddString(Path, GetUserDataDir);
  for I := Low(Path) to High(Path) do
  begin
    FileName := IncludeTrailingBackslash(Path[I]) + MIME_CACHE;
    if mbFileAccess(FileName, fmOpenRead or fmShareDenyNone) then
    begin
      Cache := mime_cache_new(FileName);
      caches.Add(Cache);
      if Cache^.magic_max_extent > mime_cache_max_extent then
        mime_cache_max_extent := Cache^.magic_max_extent;
    end;
  end;
  mime_magic_buf := GetMem(mime_cache_max_extent);
end;

//* free all mime.cache files on the system */
procedure mime_cache_free_all();
var
  I: Integer;
begin
  for I := 0 to caches.Count - 1 do
  begin
    mime_cache_free(PMimeCache(caches[I]));
  end;
  FreeAndNil(caches);
  FreeMem(mime_magic_buf);
end;

(*
 * Get mime-type of the specified file (quick, but less accurate):
 * Mime-type of the file is determined by cheking the filename only.
 * If statbuf != NULL, it will be used to determine if the file is a directory.
*)
function mime_type_get_by_filename(const filename: PAnsiChar): PAnsiChar;
var
  i: cint;
  cache: PMimeCache;
  glob_len: cint = 0;
  max_glob_len: cint = 0;
  suffix_pos: PByte = nil;
  mime_type, type_suffix: PAnsiChar;
  prev_suffix_pos: PByte = PByte(-1);
begin
  //* literal matching */
  for i := 0 to caches.Count - 1 do
  begin
    cache := PMimeCache(caches[i]);
    mime_type := mime_cache_lookup_literal(cache, filename);
    if Assigned(mime_type) then Exit(PAnsiChar(mime_type));
  end;

  //* suffix matching */
  for i := 0 to caches.Count - 1 do
  begin
    cache := PMimeCache(caches[i]);
    type_suffix := mime_cache_lookup_suffix(cache, filename, @suffix_pos);
    if (type_suffix <> nil) and (suffix_pos < prev_suffix_pos) then
    begin
      mime_type := type_suffix;
      prev_suffix_pos := suffix_pos;
    end;
  end;

  if Assigned(mime_type) then
    Exit(PAnsiChar(mime_type));

  //* glob matching */
  for i := 0 to caches.Count - 1 do
  begin
    cache := PMimeCache(caches[i]);
    type_suffix := mime_cache_lookup_glob(cache, filename, @glob_len);
    //* according to the mime.cache 1.0 spec, we should use the longest glob matched. */
    if (type_suffix <> nil) and (glob_len > max_glob_len) then
    begin
      mime_type := type_suffix;
      max_glob_len := glob_len;
    end;
  end;

  if Assigned(mime_type) then
    Exit(PAnsiChar(mime_type));

  Result := XDG_MIME_TYPE_UNKNOWN;
end;

(*
 * Get mime-type info of the specified file (slow, but more accurate):
 * To determine the mime-type of the file, mime_type_get_by_filename() is
 * tried first.  If the mime-type couldn't be determined, the content of
 * the file will be checked, which is much more time-consuming.
 * If statbuf is not NULL, it will be used to determine if the file is a directory,
 * or if the file is an executable file; otherwise, the function will call stat()
 * to gather this info itself. So if you already have stat info of the file,
 * pass it to the function to prevent checking the file stat again.
 * If you have basename of the file, pass it to the function can improve the
 * efifciency, too. Otherwise, the function will try to get the basename of
 * the specified file again.
*)
function mime_type_get_by_file(const filepath: String; max_extent: cint): String;
var
  data: PByte;
  i, len: cint;
  fd: cint = -1;
  mime_type: PAnsiChar;
  FileName: String;
begin
  FileName := ExtractFileName(FilePath);
  mime_type := mime_type_get_by_filename(PAnsiChar(FileName));
  if (strcomp(mime_type, XDG_MIME_TYPE_UNKNOWN) <> 0) then
    Exit(mime_type);

  if (max_extent > 0) then
  begin
    //* Open the file and map it into memory */
    fd := mbFileOpen(filepath, fmOpenRead or fmShareDenyNone);
    if (fd <> -1) then
    begin
{$IF DEFINED(HAVE_MMAP)}
      data := fpmmap(nil, mime_cache_max_extent, PROT_READ, MAP_SHARED, fd, 0);
{$ELSE}
      (*
       * FIXME: Can g_alloca() be used here? It's very fast, but is it safe?
       * Actually, we can allocate a block of memory with the size of mime_cache_max_extent,
       * then we don't need to  do dynamic allocation/free every time, but multi-threading
       * will be a nightmare, so...
       *)
      //* try to lock the common buffer */
      if (TryEnterCriticalSection(CriticalSection) <> 0) then
        data := mime_magic_buf
      else //* the buffer is in use, allocate new one */
        data := GetMem(max_extent);

      len := fpRead(fd, data^, max_extent);

      if (len = -1) then
      begin
        if (data = mime_magic_buf) then
          LeaveCriticalSection(CriticalSection)
        else
          FreeMem(data);
        data := Pointer(-1);
      end;
{$ENDIF}
      if (data <> Pointer(-1)) then
      begin
        for i := 0 to caches.Count - 1 do
        begin
          mime_type := mime_cache_lookup_magic(PMimeCache(caches[i]), data, len);
          if (mime_type <> nil) then Break;
        end;

        //* Check for executable file */
        if (mime_type = nil) and g_file_test(PAnsiChar(filepath), G_FILE_TEST_IS_EXECUTABLE) then
          mime_type := XDG_MIME_TYPE_EXECUTABLE;

        //* fallback: check for plain text */
        if (mime_type = nil) then
        begin
          if FileIsText(filepath) then
            mime_type := XDG_MIME_TYPE_PLAIN_TEXT;
        end;

{$IF DEFINED(HAVE_MMAP)}
        fpmunmap(data, mime_cache_max_extent);
{$ELSE}
        if (data = mime_magic_buf) then
          LeaveCriticalSection(CriticalSection) //* unlock the common buffer */
        else //* we use our own buffer */
          FreeMem(data);
{$ENDIF}
      end;
      FileClose(fd);
    end;
  end
  else
  begin
    //* empty file can be viewed as text file */
    mime_type := XDG_MIME_TYPE_PLAIN_TEXT;
  end;
  if Assigned(mime_type) then
    Result := StrPas(mime_type)
  else
    Result := XDG_MIME_TYPE_UNKNOWN;
end;

(*
 * Get all parent type of this mime_type
 *)
procedure mime_type_get_parents(const MimeType: String; var Parents: TDynamicStringArray);
var
  I, J: Integer;
  Temp: TDynamicStringArray;
begin
  for I := 0 to caches.Count - 1 do
  begin
    Temp := mime_cache_lookup_parents(PMimeCache(caches[I]), PAnsiChar(MimeType));
    for J := Low(Temp) to High(Temp) do
    begin
      AddString(Parents, Temp[J]);
    end;
  end;
end;

function GetFileMimeType(const FileName: String): String;
var
  Stat: TStat;
  MaxExtent: LongWord;
begin
  if fpStat(FileName, Stat) < 0 then
    Exit(EmptyStr);

  if fpS_ISREG(Stat.st_mode) then
  begin
    MaxExtent:= Min(mime_cache_max_extent, Stat.st_size);
    if HasGio then
      Result:= GioGetMimeType(FileName, MaxExtent)
    else begin
      Result := mime_type_get_by_file(FileName, MaxExtent);
    end;
  end
  else if fpS_ISDIR(Stat.st_mode) then
    Result:= XDG_MIME_TYPE_DIRECTORY
  else if fpS_ISCHR(Stat.st_mode) then
    Result:= 'inode/chardevice'
  else if fpS_ISBLK(Stat.st_mode) then
    Result:= 'inode/blockdevice'
  else if fpS_ISFIFO(Stat.st_mode) then
    Result:= 'inode/fifo'
  else if fpS_ISLNK(Stat.st_mode) then
    Result:= 'inode/symlink'
  else if fpS_ISSOCK(Stat.st_mode) then
    Result:= 'inode/socket';
end;

function GetFileMimeTypes(const FileName: String): TDynamicStringArray;
var
  MimeType: String;
begin
  MimeType:= GetFileMimeType(FileName);
  AddString(Result, MimeType);
  mime_type_get_parents(MimeType, Result);
end;

initialization
  mime_cache_load_all();
  InitCriticalSection(CriticalSection);

finalization
  mime_cache_free_all();
  DoneCriticalSection(CriticalSection);

end.
