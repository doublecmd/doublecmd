{
  Double Commander
  -------------------------------------------------------------------------
  This unit contains Haiku specific functions

  Copyright (C) 2022 Alexander Koblov (alexx2000@mail.ru)

  Permission is hereby granted, free of charge, to any person obtaining
  a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

unit DCHaiku;

{$mode objfpc}{$H+}
{$packrecords c}

interface

uses
  Classes, SysUtils, BaseUnix, Unix;

// fcntl.h
const
  O_NOTRAVERSE = $2000;

// TypeConstants.h
const
  B_STRING_TYPE = $43535452; // 'CSTR'

// fs_attr.h
type
  attr_info = record
    type_: cuint32;
    size: coff_t;
  end;
  Tattr_info = attr_info;
  Pattr_info = ^attr_info;

function fs_remove_attr(fd: cint; attribute: PAnsiChar): cint; cdecl; external clib;
function fs_stat_attr(fd: cint; attribute: pansichar; attrInfo: Pattr_info): cint; cdecl; external clib;

function fs_open_attr(path: pansichar; attribute: pansichar;
                      type_: cuint32; openMode: cint): cint; cdecl; external clib;
function fs_fopen_attr(fd: cint; attribute: pansichar;
                       type_: cuint32; openMode: cint): cint; cdecl; external clib;
function fs_close_attr(fd: cint): cint; cdecl; external clib;

function fs_open_attr_dir(path: pansichar): pDir; cdecl; external clib;
function fs_lopen_attr_dir(path: pansichar): pDir; cdecl; external clib;
function fs_fopen_attr_dir(fd: cint): pDir; cdecl; external clib;
function fs_close_attr_dir(dir: pDir): cint; cdecl; external clib;
function fs_read_attr_dir(dir: pDir): pDirent; cdecl; external clib;

// OS.h
const
  B_OS_NAME_LENGTH   = 32;

// StorageDefs.h
const
  B_FILE_NAME_LENGTH = 256;

// fs_info.h
const
  B_FS_IS_READONLY   = $00000001;
  B_FS_IS_REMOVABLE  = $00000002;
  B_FS_IS_PERSISTENT = $00000004;
  B_FS_IS_SHARED     = $00000008;

type
  Tfs_info = record
    dev: dev_t;
    root: ino_t;
    flags: cuint32;
    block_size: coff_t;
    io_size: coff_t;
    total_blocks: coff_t;
    free_blocks: coff_t;
    total_nodes: coff_t;
    free_nodes: coff_t;
    device_name: array[0..127] of AnsiChar;
    volume_name: array[0..Pred(B_FILE_NAME_LENGTH)] of AnsiChar;
    fsh_name: array[0..Pred(B_OS_NAME_LENGTH)] of AnsiChar;
  end;
  Pfs_info = ^Tfs_info;

function dev_for_path(path: PAnsiChar): dev_t; cdecl; external clib;
function next_dev(pos: pcint32): dev_t; cdecl; external clib;
function fs_stat_dev(dev: dev_t; info: Pfs_info): cint; cdecl; external clib;

// FindDirectory.h
const
  B_TRASH_DIRECTORY         =    1;
  B_USER_DIRECTORY          = 3000;
  B_USER_SETTINGS_DIRECTORY = 3006;
  B_USER_DATA_DIRECTORY     = 3012;
  B_USER_CACHE_DIRECTORY    = 3013;

function find_directory(which: cuint32; volume: dev_t; createIt: cbool;
                        pathString: PAnsiChar; length: cint32): status_t; cdecl; external clib;


function mbFileCopyXattr(const Source, Target: String): Boolean;
function mbFileWriteXattr(const FileName, AttrName, Value: String): Boolean;
function mbFindDirectory(which: cuint32; volume: dev_t; createIt: cbool; out pathString: String): Boolean;

implementation

uses
  DCUnix, DCConvertEncoding;

function mbFileOpen(const FileName: String; Flags: cInt): THandle;
begin
  repeat
    Result:= fpOpen(FileName, Flags or O_CLOEXEC);
  until (Result <> -1) or (fpgeterrno <> ESysEINTR);
end;

function mbFileGetXattr(hFile: THandle): TStringArray;
var
  DirPtr: pDir;
  PtrDirEnt: pDirent;
  Index: Integer = 0;
begin
  Result:= Default(TStringArray);
  DirPtr:= fs_fopen_attr_dir(hFile);
  if Assigned(DirPtr) then
  try
    SetLength(Result, 512);
    PtrDirEnt:= fs_read_attr_dir(DirPtr);
    while PtrDirEnt <> nil do
    begin
      Result[Index]:= StrPas(PtrDirEnt^.d_name);
      Inc(Index);

      if (Index > High(Result)) then
      begin
        SetLength(Result, Length(Result) * 2);
      end;

      PtrDirEnt:= fs_read_attr_dir(DirPtr);
    end;
  finally
    fs_close_attr_dir(DirPtr);
  end;
  SetLength(Result, Index);
end;

function mbFileWriteXattr(const FileName, AttrName, Value: String): Boolean;
var
  hAttr: cint;
  ALen: Integer;
  hTarget: THandle;
begin
  hTarget:= mbFileOpen(FileName, O_RDWR or O_NOTRAVERSE);
  Result:= (hTarget <> feInvalidHandle);
  if Result then
  begin
    hAttr:= fs_fopen_attr(hTarget, PAnsiChar(AttrName),
                          B_STRING_TYPE, O_CREAT or O_TRUNC or O_WRONLY);
    Result:= (hAttr <> feInvalidHandle);
    if Result then
    begin
      ALen:= Length(Value) + 1;
      Result:= (FileWrite(hAttr, PAnsiChar(Value)^, ALen) = ALen);
      fs_close_attr(hAttr);
    end;
    FileClose(hTarget);
  end;
end;

function mbFileCopyXattr(const Source, Target: String): Boolean;
var
  AData: TBytes;
  Index: Integer;
  Names: TStringArray;
  AttrName: PAnsiChar;
  AttrInfo: Tattr_info;
  hSource, hTarget: THandle;

  function ReadAttr(hFile: cint; attribute: pansichar): Boolean;
  var
    hAttr: THandle;
  begin
    hAttr:= fs_fopen_attr(hFile, attribute, attrInfo.type_, O_RDONLY);
    Result:= (hAttr <> feInvalidHandle);
    if Result then
    begin
      Result:= (FileRead(hAttr, AData[0], attrInfo.size) = attrInfo.size);
      fs_close_attr(hAttr);
    end;
  end;

  function WriteAttr(hFile: cint; attribute: pansichar): Boolean;
  var
    hAttr: THandle;
  begin
    hAttr:= fs_fopen_attr(hFile, attribute, attrInfo.type_, O_CREAT or O_WRONLY);
    Result:= (hAttr <> feInvalidHandle);
    if Result then
    begin
      Result:= (FileWrite(hAttr, AData[0], attrInfo.size) = attrInfo.size);
      fs_close_attr(hAttr);
    end;
  end;

begin
  hSource:= mbFileOpen(Source, O_RDONLY or O_NOTRAVERSE);
  Result:= (hSource <> feInvalidHandle);
  if Result then
  begin
    hTarget:= mbFileOpen(Target, O_RDWR or O_NOTRAVERSE);
    Result:= (hTarget <> feInvalidHandle);
    if Result then
    begin
      // Remove attributes from target
      Names:= mbFileGetXattr(hTarget);
      for Index:= 0 to High(Names) do
      begin
        fs_remove_attr(hTarget, PAnsiChar(Names[Index]));
      end;

      SetLength(AData, $FFFF);
      Names:= mbFileGetXattr(hSource);

      for Index:= 0 to High(Names) do
      begin
        AttrName:= PAnsiChar(Names[Index]);

        if (fs_stat_attr(hSource, AttrName, @AttrInfo) >= 0) then
        begin
          if (AttrInfo.size > Length(AData)) then
          begin
            SetLength(AData, AttrInfo.size);
          end;

          Result:= ReadAttr(hSource, AttrName);
          if Result then
          begin
            Result:= WriteAttr(hTarget, AttrName);
            if not Result then Break;
          end;
        end;
      end;
      FileClose(hTarget);
    end;
    FileClose(hSource);
  end;
end;

function mbFindDirectory(which: cuint32; volume: dev_t; createIt: cbool; out
  pathString: String): Boolean;
var
  APath: array[0..MAX_PATH] of AnsiChar;
begin
  Result:= find_directory(which, volume, createIt, APath, MAX_PATH) >= 0;
  if Result then begin
    pathString:= CeSysToUtf8(APath);
  end;
end;

end.

