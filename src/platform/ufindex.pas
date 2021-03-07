{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains UTF-8 versions of Find(First, Next, Close) functions

    Copyright (C) 2006-2020 Alexander Koblov (alexx2000@mail.ru)

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

{$macro on}
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, DCBasicTypes
  {$IFDEF UNIX}
  , BaseUnix, DCUnix, uMasks
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
  fffElevated = $40000000;

type
{$IFDEF UNIX}
  TUnixFindHandle = record
    DirPtr: PDir;      //en> directory pointer for reading directory
    FindPath: String;  //en> file name path
    Mask: TMask;       //en> object that will check mask
  end;
  PUnixFindHandle = ^TUnixFindHandle;
{$ENDIF}

  PSearchRecEx = ^TSearchRecEx;
  TSearchRecEx = record
    Time : DCBasicTypes.TFileTime;  // modification time
    Size : Int64;
    Attr : TFileAttrs;
    Name : String;
    Flags : UInt32;
{$IF DEFINED(MSWINDOWS)}
    FindHandle : THandle;
    FindData : Windows.TWin32FindDataW;
    property PlatformTime: TFileTime read FindData.ftCreationTime;
    property LastAccessTime: TFileTime read FindData.ftLastAccessTime;
{$ELSE}
    FindHandle : Pointer;
    FindData : BaseUnix.Stat;
    property PlatformTime: TUnixTime read FindData.st_ctime;
    property LastAccessTime: TUnixTime read FindData.st_atime;
{$ENDIF}
  end;

function FindFirstEx(const Path: String; Flags: UInt32; out SearchRec: TSearchRecEx): Integer;
function FindNextEx(var SearchRec: TSearchRecEx): Integer;
procedure FindCloseEx(var SearchRec: TSearchRecEx);

implementation

uses
  LazUTF8, uDebug
  {$IFDEF MSWINDOWS}
  , DCWindows, DCDateTimeUtils, uMyWindows
  {$ENDIF}
  {$IFDEF UNIX}
  , InitC, Unix, DCOSUtils, DCFileAttributes, DCConvertEncoding
  {$ENDIF};

{$IF DEFINED(LINUX)}
  {$define fpgeterrno:= fpgetCerrno}
  function fpOpenDir(dirname: PAnsiChar): pDir; cdecl; external clib name 'opendir';
  function fpReadDir(var dirp: TDir): pDirent; cdecl; external clib name 'readdir64';
  function fpCloseDir(var dirp: TDir): cInt; cdecl; external clib name 'closedir';
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
  UnixFindHandle: PUnixFindHandle absolute SearchRec.FindHandle;
begin
  Result:= -1;
  if UnixFindHandle = nil then Exit;
  if (UnixFindHandle^.Mask = nil) or UnixFindHandle^.Mask.Matches(SearchRec.Name) then
  begin
    if fpLStat(UTF8ToSys(UnixFindHandle^.FindPath + SearchRec.Name), @SearchRec.FindData) >= 0 then
    begin
      with SearchRec.FindData do
      begin
        // On Unix a size for directory entry on filesystem is returned in StatInfo.
        // We don't want to use it.
        if fpS_ISDIR(st_mode) then
          SearchRec.Size:= 0
        else begin
          SearchRec.Size:= Int64(st_size);
        end;
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

function FindFirstEx(const Path: String; Flags: UInt32; out SearchRec: TSearchRecEx): Integer;
{$IFDEF MSWINDOWS}
var
  wsPath: UnicodeString;
  fInfoLevelId: FINDEX_INFO_LEVELS;
begin
  SearchRec.Flags:= Flags;
  wsPath:= UTF16LongName(Path);
  if CheckWin32Version(6, 1) then
  begin
    fInfoLevelId:= FindExInfoBasic;
    Flags:= FIND_FIRST_EX_LARGE_FETCH;
  end
  else begin
    Flags:= 0;
    fInfoLevelId:= FindExInfoStandard;
  end;
  SearchRec.FindHandle:= FindFirstFileExW(PWideChar(wsPath), fInfoLevelId,
                                          @SearchRec.FindData, FindExSearchNameMatch, nil, Flags);

  if SearchRec.FindHandle = INVALID_HANDLE_VALUE then
    Result:= GetLastError
  else begin
    Result:= mbFindMatchingFile(SearchRec);
  end;
end;
{$ELSE}
var
  UnixFindHandle: PUnixFindHandle;
begin
  New(UnixFindHandle);

  SearchRec.Flags:= Flags;
  SearchRec.FindHandle:= UnixFindHandle;
  FillChar(UnixFindHandle^, SizeOf(TUnixFindHandle), 0);

  with UnixFindHandle^ do
  begin
    FindPath:= ExtractFileDir(Path);
    if FindPath = '' then begin
      FindPath := mbGetCurrentDir;
    end;
    FindPath:= IncludeTrailingBackSlash(FindPath);

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
          if mbFileSystemEntryExists(Path) and (mbFindMatchingFile(SearchRec) = 0) then
            Exit(0)
          else
            Exit(-1);
        end;
      Mask := TMask.Create(SearchRec.Name);
    end;

    DirPtr:= fpOpenDir(PAnsiChar(CeUtf8ToSys(FindPath)));
    if (DirPtr = nil) then Exit(fpgeterrno);
  end;
  Result:= FindNextEx(SearchRec);
end;
{$ENDIF}

function FindNextEx(var SearchRec: TSearchRecEx): Integer;
{$IFDEF MSWINDOWS}
begin
  if FindNextFileW(SearchRec.FindHandle, SearchRec.FindData) then
    Result:= mbFindMatchingFile(SearchRec)
  else begin
    Result:= GetLastError;
  end;
end;
{$ELSE}
var
  PtrDirEnt: pDirent;
  UnixFindHandle: PUnixFindHandle absolute SearchRec.FindHandle;
begin
  Result:= -1;
  if UnixFindHandle = nil then Exit;
  if UnixFindHandle^.DirPtr = nil then Exit;
  PtrDirEnt:= fpReadDir(UnixFindHandle^.DirPtr^);
  while PtrDirEnt <> nil do
  begin
    SearchRec.Name:= CeSysToUtf8(PtrDirEnt^.d_name);
    Result:= mbFindMatchingFile(SearchRec);
    if Result = 0 then // if found then exit
      Exit
    else // else read next
      PtrDirEnt:= fpReadDir(UnixFindHandle^.DirPtr^);
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
  UnixFindHandle: PUnixFindHandle absolute SearchRec.FindHandle;
begin
  if UnixFindHandle = nil then Exit;
  if UnixFindHandle^.DirPtr <> nil then
    fpCloseDir(UnixFindHandle^.DirPtr^);
  if Assigned(UnixFindHandle^.Mask) then
    UnixFindHandle^.Mask.Free;
  Dispose(UnixFindHandle);
  SearchRec.FindHandle:= nil;
end;
{$ENDIF}

end.

