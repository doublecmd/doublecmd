{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains UTF-8 versions of Find(First, Next, Close) functions

    Copyright (C) 2006-2018 Alexander Koblov (alexx2000@mail.ru)

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
}

unit uFindEx;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DCBasicTypes
  {$IFDEF UNIX}
  , BaseUnix, uMasks
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF DARWIN}
  , MacOSAll
  {$ENDIF}
  ;

const
  fffPortable = $80000000;

type
{$IFDEF UNIX}
  TUnixFindData = record
    DirPtr: PDir;   //en> directory pointer for reading directory
    sPath: String;  //en> file name path
    Mask: TMask;    //en> object that will check mask
    StatRec: Stat;
  end;
  PUnixFindData = ^TUnixFindData;
{$ENDIF}

  PSearchRecEx = ^TSearchRecEx;
  TSearchRecEx = record
    Time : DCBasicTypes.TFileTime;  // modification time
    Size : Int64;
    Attr : TFileAttrs;
    Name : String;
    Flags : UInt32;
{$ifdef unix}
    FindHandle : Pointer;
{$else unix}
    FindHandle : THandle;
{$endif unix}
{$if defined(Win32) or defined(WinCE) or defined(Win64)}
    FindData : Windows.TWin32FindDataW;
{$endif}
  end;

function FindFirstEx (const Path : String; Flags : UInt32; out SearchRec : TSearchRecEx) : Longint;
function FindNextEx (var SearchRec : TSearchRecEx) : Longint;
procedure FindCloseEx(var SearchRec: TSearchRecEx);

implementation

uses
  LazUTF8, uDebug
  {$IFDEF MSWINDOWS}
  , DCWindows, DCDateTimeUtils, uMyWindows
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix, DCOSUtils, DCFileAttributes, DCConvertEncoding, uMyUnix
  {$ENDIF};

{$IF DEFINED(LINUX)}
  function fpOpenDir(dirname: PAnsiChar): pDir; cdecl; external libc name 'opendir';
  function fpReadDir(var dirp: TDir): pDirent; cdecl; external libc name 'readdir64';
  function fpCloseDir(var dirp: TDir): cInt; cdecl; external libc name 'closedir';
{$ENDIF}

function mbFindMatchingFile(var SearchRec: TSearchRecEx): Integer;
{$IFDEF MSWINDOWS}
begin
  with SearchRec do
  begin
    if (Flags and fffPortable = 0) then
      Time:= TWinFileTime(FindData.ftLastWriteTime)
    else begin
      Time:= WinFileTimeToUnixTime(TWinFileTime(FindData.ftLastWriteTime));
    end;
    FindData.dwFileAttributes:= ExtractFileAttributes(FindData);
    Size:= (Int64(FindData.nFileSizeHigh) shl 32) + FindData.nFileSizeLow;
    Name:= UTF16ToUTF8(UnicodeString(FindData.cFileName));
    Attr:= FindData.dwFileAttributes;
  end;
  Result:= 0;
end;
{$ELSE}
var
  UnixFindData: PUnixFindData absolute SearchRec.FindHandle;
begin
  Result:= -1;
  if UnixFindData = nil then Exit;
  if (UnixFindData^.Mask = nil) or UnixFindData^.Mask.Matches(SearchRec.Name) then
  begin
    if fpLStat(UTF8ToSys(UnixFindData^.sPath + SearchRec.Name), @UnixFindData^.StatRec) >= 0 then
    begin
      with UnixFindData^.StatRec do
      begin
        SearchRec.Size:= Int64(st_size);
        SearchRec.Time:= DCBasicTypes.TFileTime(st_mtime);
        if (SearchRec.Flags and fffPortable = 0) then
          SearchRec.Attr:= DCBasicTypes.TFileAttrs(st_mode)
        else begin
          SearchRec.Attr:= UnixToWinFileAttr(SearchRec.Name, TFileAttrs(st_mode));
        end;
      end;
      Result:= 0;
    end;
  end;
end;
{$ENDIF}

function FindFirstEx(const Path: String; Flags: UInt32; out SearchRec: TSearchRecEx): Longint;
{$IFDEF MSWINDOWS}
var
  wsPath: UnicodeString;
begin
  SearchRec.Flags:= Flags;
  wsPath:= UTF16LongName(Path);
  SearchRec.FindHandle:= FindFirstFileW(PWideChar(wsPath), SearchRec.FindData);
  // if error then exit
  if SearchRec.FindHandle = INVALID_HANDLE_VALUE then Exit(GetLastError);
  Result:= mbFindMatchingFile(SearchRec);
end;
{$ELSE}
var
  UnixFindData: PUnixFindData;
begin
  { Allocate UnixFindData }
  New(UnixFindData);
  FillChar(UnixFindData^, SizeOf(UnixFindData^), 0);
  SearchRec.Flags:= Flags;
  SearchRec.FindHandle:= UnixFindData;

  with UnixFindData^ do
  begin
    sPath:= ExtractFileDir(Path);
    if sPath = '' then begin
      sPath := mbGetCurrentDir;
    end;
    sPath:= IncludeTrailingBackSlash(sPath);

    // Assignment of SearchRec.Name also needed if the path points to a specific
    // file and only a single mbFindMatchingFile() check needs to be done below.
    SearchRec.Name:= ExtractFileName(Path);

    // Check if searching for all files. If yes don't need to use Mask.
    if (SearchRec.Name <> '*') and (SearchRec.Name <> '') then
    // '*.*' searches for files with a dot in name so mask needs to be checked.
    begin
      // If searching for single specific file, just check if it exists and exit.
      if (Pos('?', SearchRec.Name) = 0) and (Pos('*', SearchRec.Name) = 0) then
        begin
          if FileExists(UTF8ToSys(Path)) and (mbFindMatchingFile(SearchRec) = 0) then
            Exit(0)
          else
            Exit(-1);
        end;
      Mask := TMask.Create(SearchRec.Name);
    end;

    DirPtr:= fpOpenDir(PAnsiChar(CeUtf8ToSys(sPath)));
  end;
  Result:= FindNextEx(SearchRec);
end;
{$ENDIF}

function FindNextEx (var SearchRec : TSearchRecEx) : Longint;
{$IFDEF MSWINDOWS}
begin
  if FindNextFileW(SearchRec.FindHandle, SearchRec.FindData) then
    begin
      Result:= mbFindMatchingFile(SearchRec);
    end
  else
    Result:= GetLastError;
end;
{$ELSE}
var
  PtrDirEnt: pDirent;
  UnixFindData: PUnixFindData absolute SearchRec.FindHandle;
begin
  Result:= -1;
  if UnixFindData = nil then Exit;
  if UnixFindData^.DirPtr = nil then Exit;
  PtrDirEnt:= fpReadDir(UnixFindData^.DirPtr^);
  while PtrDirEnt <> nil do
  begin
    SearchRec.Name:= CeSysToUtf8(PtrDirEnt^.d_name);
    Result:= mbFindMatchingFile(SearchRec);
    if Result = 0 then // if found then exit
      Exit
    else // else read next
      PtrDirEnt:= fpReadDir(UnixFindData^.DirPtr^);
  end;
end;
{$ENDIF}

procedure FindCloseEx(var SearchRec: TSearchRecEx);
{$IFDEF MSWINDOWS}
begin
   if SearchRec.FindHandle <> INVALID_HANDLE_VALUE then
    Windows.FindClose(SearchRec.FindHandle);
end;
{$ELSE}
var
  UnixFindData: PUnixFindData absolute SearchRec.FindHandle;
begin
  if UnixFindData = nil then Exit;
  if UnixFindData^.DirPtr <> nil then
    fpCloseDir(UnixFindData^.DirPtr^);
  if Assigned(UnixFindData^.Mask) then
    UnixFindData^.Mask.Free;
  Dispose(UnixFindData);
  SearchRec.FindHandle:= nil;
end;
{$ENDIF}

end.

