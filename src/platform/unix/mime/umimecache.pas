{
   Double Commander
   -------------------------------------------------------------------------
   Shared MIME-info Database mime.cache file parser
   (http://standards.freedesktop.org/shared-mime-info-spec)

   Copyright (C) 2014-2015 Alexander Koblov (alexx2000@mail.ru)

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

unit uMimeCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, Unix, Math, DCBasicTypes, DCOSUtils;

type
  PMimeCache = ^TMimeCache;

  TMimeCache = record
    file_path: String;

    buffer: PAnsiChar;
    size: LongWord;

    n_alias: LongWord;
    s_alias: PAnsiChar;

    n_parents: LongWord;
    parents: PAnsiChar;

    n_literals: LongWord;
    literals: PAnsiChar;

    n_globs: LongWord;
    globs: PAnsiChar;

    n_suffix_roots: LongWord;
    suffix_roots: PAnsiChar;

    n_magics: LongWord;
    magic_max_extent: LongWord;
    magics: PAnsiChar;
  end;

function mime_cache_new(const file_path: String): PMimeCache;
procedure mime_cache_free(cache: PMimeCache);

function mime_cache_load(cache: PMimeCache; const file_path: String): Boolean;
function mime_cache_lookup_literal(cache: PMimeCache; const filename: PChar): PAnsiChar;
function mime_cache_lookup_magic(cache: PMimeCache; const data: PByte; len: Integer): PAnsiChar;
function mime_cache_lookup_parents(cache: PMimeCache; const mime_type: PChar): TDynamicStringArray;
function mime_cache_lookup_glob(cache: PMimeCache; const filename: PChar; glob_len: PInteger): PAnsiChar;
function mime_cache_lookup_suffix(cache: PMimeCache; const filename: PChar; const suffix_pos: PPChar): PAnsiChar;

implementation

uses
  uGlib2, CTypes;

function fnmatch(const pattern: PAnsiChar; const str: PAnsiChar; flags: cint): cint; cdecl; external;

const
  LIB_MAJOR_VERSION = 1;
  (* FIXME: since mime-cache 1.2, weight is splitted into three parts
   * only lower 8 bit contains weight, and higher bits are flags and case-sensitivity.
   * anyway, since we don't support weight at all, it'll be fixed later.
   * We claimed that we support 1.2 to cheat pcmanfm as a temporary quick dirty fix
   * for the broken file manager, but this should be correctly done in the future.
   * Weight and case-sensitivity are not handled now.
  *)
  LIB_MAX_MINOR_VERSION = 2;
  LIB_MIN_MINOR_VERSION = 1;

const
  //* cache header */
  MAJOR_VERSION = 0;
  MINOR_VERSION = 2;
  ALIAS_LIST = 4;
  PARENT_LIST = 8;
  LITERAL_LIST = 12;
  SUFFIX_TREE = 16;
  GLOB_LIST = 20;
  MAGIC_LIST = 24;
  NAMESPACE_LIST = 28;

function VAL16(buffer: PAnsiChar; idx: Integer): Word; inline;
begin
  Result := BEtoN(PWord(buffer + idx)^);
end;

function VAL32(buffer: PAnsiChar; idx: Integer): LongWord; inline;
begin
  Result := BEtoN(PLongWord(buffer + idx)^);
end;

function mime_cache_new(const file_path: String): PMimeCache;
begin
  New(Result);
  FillChar(Result^, SizeOf(TMimeCache), 0);
  if (Length(file_path) > 0) then
    mime_cache_load(Result, file_path);
end;

procedure mime_cache_unload(cache: PMimeCache; Clear: Boolean);
begin
  if Assigned(cache^.buffer) then
  begin
  {$IF DEFINED(HAVE_MMAP)}
    fpmunmap(cache^.buffer, cache^.size);
  {$ELSE}
    FreeMem(cache^.buffer);
  {$ENDIF}
  end;
  if (Clear) then
    FillChar(cache^, sizeof(TMimeCache), 0);
end;

procedure mime_cache_free(cache: PMimeCache);
begin
  mime_cache_unload(cache, False);
  Dispose(cache);
end;

function mime_cache_load(cache: PMimeCache; const file_path: String): Boolean;
var
  offset: LongWord;
  fd: Integer = -1;
  majv, minv: LongWord;
  statbuf: BaseUnix.Stat;
  buffer: PAnsiChar = nil;
begin
  //* Unload old cache first if needed */
  mime_cache_unload(cache, True);

  //* Store the file path */
  cache^.file_path := file_path;

  //* Open the file and map it into memory */
  fd := mbFileOpen(file_path, fmOpenRead);

  if (fd < 0) then
    Exit(False);

  if (fpFStat(fd, statbuf) < 0) then
  begin
    FileClose(fd);
    Exit(False);
  end;

{$IF DEFINED(HAVE_MMAP)}
  buffer := fpmmap(nil, statbuf.st_size, PROT_READ, MAP_SHARED, fd, 0);
{$ELSE}
  buffer := GetMem(statbuf.st_size);
  if Assigned(buffer) then
    fpRead(fd, buffer^, statbuf.st_size)
  else
    buffer := Pointer(-1);
{$ENDIF}
  FileClose(fd);

  if (buffer = Pointer(-1)) then
    Exit(False);

  majv := VAL16(buffer, MAJOR_VERSION);
  minv := VAL16(buffer, MINOR_VERSION);

  //* Check version */
  if (majv > LIB_MAJOR_VERSION) or (minv > LIB_MAX_MINOR_VERSION) or (minv < LIB_MIN_MINOR_VERSION) then
  begin
    {$IF DEFINED(HAVE_MMAP)}
    fpmunmap(buffer, statbuf.st_size);
    {$ELSE}
    FreeMem(buffer);
    {$ENDIF}
    Exit(False);
  end;

  cache^.buffer := buffer;
  cache^.size := statbuf.st_size;

  offset := VAL32(buffer, ALIAS_LIST);
  cache^.s_alias := buffer + offset + 4;
  cache^.n_alias := VAL32(buffer, offset);

  offset := VAL32(buffer, PARENT_LIST);
  cache^.parents := buffer + offset + 4;
  cache^.n_parents := VAL32(buffer, offset);

  offset := VAL32(buffer, LITERAL_LIST);
  cache^.literals := buffer + offset + 4;
  cache^.n_literals := VAL32(buffer, offset);

  offset := VAL32(buffer, GLOB_LIST);
  cache^.globs := buffer + offset + 4;
  cache^.n_globs := VAL32(buffer, offset);

  offset := VAL32(buffer, SUFFIX_TREE);
  cache^.suffix_roots := buffer + VAL32(buffer + offset, 4);
  cache^.n_suffix_roots := VAL32(buffer, offset);

  offset := VAL32(buffer, MAGIC_LIST);
  cache^.n_magics := VAL32(buffer, offset);
  cache^.magic_max_extent := VAL32(buffer + offset, 4);
  cache^.magics := buffer + VAL32(buffer + offset, 8);

  Result := True;
end;

function magic_rule_match(const buf: PAnsiChar; rule: PAnsiChar; const data: PByte; len: Integer): Boolean;
var
  i: Integer;
  value, mask: PByte;
  match: Boolean = False;
  offset, range: LongWord;
  val_off, mask_off: LongWord;
  max_offset, val_len: LongWord;
  n_children, first_child_off: LongWord;
begin
  offset := VAL32(rule, 0);
  range := VAL32(rule, 4);

  max_offset := offset + range;
  val_len := VAL32(rule, 12);

  while (offset < max_offset) and ((offset + val_len) <= len) do
  begin
    val_off := VAL32(rule, 16);
    mask_off := VAL32(rule, 20);
    value := PByte(buf + val_off);
    //* FIXME: word_size and byte order are not supported! */

    if (mask_off > 0) then    //* compare with mask applied */
    begin
      mask := PByte(buf + mask_off);

      for i := 0 to val_len - 1 do
      begin
        if ((data[offset + i] and mask[i]) <> value[i]) then
          break;
      end;
      if (i >= val_len) then
        match := True;
    end
    else    //* direct comparison */
    begin
      if (CompareMem(value, data + offset, val_len)) then
        match := True;
    end;

    if (match) then
    begin
      n_children := VAL32(rule, 24);
      if (n_children > 0) then
      begin
        i := 0;
        first_child_off := VAL32(rule, 28);
        rule := buf + first_child_off;
        while (i < n_children) do
        begin
          if (magic_rule_match(buf, rule, data, len)) then
            Exit(True);
          Inc(i);
          rule += 32;
        end;
      end
      else
        Exit(True);
    end;
    Inc(offset);
  end;
  Result := False;
end;

function magic_match(const buf: PAnsiChar; const magic: PAnsiChar; const data: PByte; len: Integer): Boolean;
var
  i: Integer = 0;
  rule: PAnsiChar;
  n_rules, rules_off: LongWord;
begin
  n_rules := VAL32(magic, 8);
  rules_off := VAL32(magic, 12);
  rule := buf + rules_off;

  while (i < n_rules) do
  begin
    if (magic_rule_match(buf, rule, data, len)) then
      Exit(True);
    Inc(i);
    rule += 32;
  end;
  Result := False;
end;

function mime_cache_lookup_magic(cache: PMimeCache; const data: PByte; len: Integer): PAnsiChar;
var
  i: Integer = 0;
  magic: PAnsiChar;
begin
  magic := cache^.magics;

  if (data = nil) or (0 = len) or (magic = nil) then
    Exit(nil);

  while (i < cache^.n_magics) do
  begin
    if (magic_match(cache^.buffer, magic, data, len)) then
    begin
      Exit(cache^.buffer + VAL32(magic, 4));
    end;
    Inc(i);
    magic += 16;
  end;
  Result := nil;
end;

(* Reverse suffix tree is used since mime.cache 1.1 (shared mime info 0.4)
 * Returns the address of the found "node", not mime-type.
 * FIXME: 1. Should be optimized with binary search
 *        2. Should consider weight of suffix nodes
 *)
function lookup_reverse_suffix_nodes(const buf: PAnsiChar; const nodes: PAnsiChar; n: LongWord; const name: Pgchar; const suffix: Pgchar; const suffix_pos: PPChar): PAnsiChar;
var
  i: Integer;
  ch: LongWord;
  uchar: gunichar;
  node: PAnsiChar;
  ret: PAnsiChar = nil;
  cur_suffix_pos: Pgchar;
  _suffix_pos: Pgchar = nil;
  leaf_node: PAnsiChar = nil;
  n_children, first_child_off: LongWord;
begin
  cur_suffix_pos := suffix + 1;

  if Assigned(suffix) then
    uchar := g_unichar_tolower(g_utf8_get_char(suffix))
  else
    uchar := 0;
  //* g_debug("%s: suffix= '%s'", name, suffix); */

  for i := 0 to n - 1 do
  begin
    node := nodes + i * 12;
    ch := VAL32(node, 0);
    _suffix_pos := suffix;

    if (ch > 0) then
    begin
      if (ch = uchar) then
      begin
        n_children := VAL32(node, 4);
        first_child_off := VAL32(node, 8);
        leaf_node := lookup_reverse_suffix_nodes(buf,
          buf + first_child_off,
          n_children,
          name,
          g_utf8_find_prev_char(name, suffix), @_suffix_pos);
        if Assigned(leaf_node) and (_suffix_pos < cur_suffix_pos) then
        begin
          ret := leaf_node;
          cur_suffix_pos := _suffix_pos;
        end;
      end;
    end
    else //* ch == 0 */
    begin
      //* guint32 weight = VAL32(node, 8); */
      //* suffix is found in the tree! */

      if (suffix < cur_suffix_pos) then
      begin
        ret := node;
        cur_suffix_pos := suffix;
      end;
    end;
  end;
  suffix_pos^ := cur_suffix_pos;
  Result := ret;
end;

function mime_cache_lookup_suffix(cache: PMimeCache; const filename: PChar; const suffix_pos: PPChar): PAnsiChar;
var
  suffix: Pgchar;
  root: PAnsiChar;
  n, fn_len: Integer;
  ret: PAnsiChar = nil;
  leaf_node: PAnsiChar;
  mime_type: PAnsiChar = nil;
  _suffix_pos: PChar = PAnsiChar(-1);
begin
  root := cache^.suffix_roots;
  n := cache^.n_suffix_roots;

  if (filename = nil) or (filename^ = #0) or (0 = n) then
    Exit(nil);

  fn_len := strlen(filename);
  suffix := g_utf8_find_prev_char(filename, filename + fn_len);
  leaf_node := lookup_reverse_suffix_nodes(cache^.buffer, root, n, filename, suffix, @_suffix_pos);

  if (leaf_node <> nil) then
  begin
    mime_type := cache^.buffer + VAL32(leaf_node, 4);
    //* g_debug( "found: %s", mime_type ); */
    suffix_pos^ := _suffix_pos;
    ret := mime_type;
  end;
  Result := ret;
end;

function lookup_str_in_entries(cache: PMimeCache; const entries: PAnsiChar; n: Integer; const str: Pgchar): PAnsiChar;
var
  str2: Pgchar;
  entry: PAnsiChar;
  lower: Integer = 0;
  comp, upper, middle: Integer;
begin
  upper := n;
  middle := upper div 2;

  if (Assigned(entries) and Assigned(str) and (str^ <> #0)) then
  begin
    //* binary search */
    while (upper >= lower) do
    begin
      entry := entries + middle * 8;
      str2 := Pgchar(cache^.buffer + VAL32(entry, 0));
      comp := strcomp(str, str2);
      if (comp < 0) then
        upper := middle - 1
      else if (comp > 0) then
        lower := middle + 1
      else //* comp == 0 */
        Exit(cache^.buffer + VAL32(entry, 4));
      middle := (upper + lower) div 2;
    end;
  end;
  Result := nil;
end;

function mime_cache_lookup_alias(cache: PMimeCache; const mime_type: PChar): PAnsiChar;
begin
  Result := lookup_str_in_entries(cache, cache^.s_alias, cache^.n_alias, mime_type);
end;

function mime_cache_lookup_literal(cache: PMimeCache; const filename: PChar): PAnsiChar;
var
  str2: Pgchar;
  lower: Integer = 0;
  entries, entry: PAnsiChar;
  comp, upper, middle: Integer;
begin
  (* FIXME: weight is used in literal lookup after mime.cache v1.1.
   * However, it's poorly documented. So I've no idea how to implement this. *)
  entries := cache^.literals;
  upper := cache^.n_literals;
  middle := upper div 2;

  if (Assigned(entries) and Assigned(filename) and (filename^ <> #0)) then
  begin
    //* binary search */
    while (upper >= lower) do
    begin
      //* The entry size is different in v 1.1 */
      entry := entries + middle * 12;
      str2 := Pgchar(cache^.buffer + VAL32(entry, 0));
      comp := strcomp(filename, str2);
      if (comp < 0) then
        upper := middle - 1
      else if (comp > 0) then
        lower := middle + 1
      else //* comp == 0 */
        Exit(cache^.buffer + VAL32(entry, 4));
      middle := (upper + lower) div 2;
    end;
  end;
  Result := nil;
end;

function mime_cache_lookup_glob(cache: PMimeCache; const filename: PChar; glob_len: PInteger): PAnsiChar;
var
  glob: PChar;
  entry: PAnsiChar;
  i, _glob_len: Integer;
  max_glob_len: Integer = 0;
begin
  Result := nil;
  entry := cache^.globs;

  for i := 0 to cache^.n_globs - 1 do
  begin
    glob := PChar(cache^.buffer + VAL32(entry, 0));
    _glob_len := strlen(glob);
    if (fnmatch(glob, filename, 0) = 0) and (_glob_len > max_glob_len) then
    begin
      max_glob_len := _glob_len;
      Result := (cache^.buffer + VAL32(entry, 4));
    end;
    entry += 12;
  end;
  glob_len^ := max_glob_len;
end;

function mime_cache_lookup_parents(cache: PMimeCache; const mime_type: PChar): TDynamicStringArray;
var
  parent: PChar;
  n, i: LongWord;
  parents: PAnsiChar;
  parent_off: LongWord;
begin
  parents := lookup_str_in_entries(cache, cache^.parents, cache^.n_parents, mime_type);
  if (parents = nil) then Exit(nil);

  n := VAL32(parents, 0);
  parents += 4;

  SetLength(Result, n);

  for i := 0 to n - 1 do
  begin
    parent_off := VAL32(parents, i * 4);
    parent := PChar(cache^.buffer + parent_off);
    Result[i] := parent;
  end;
end;

end.
