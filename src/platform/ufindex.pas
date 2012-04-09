{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains UTF8 versions of Find(First, Next) functions and other stuff
    
    Copyright (C) 2006-2010  Koblov Alexander (Alexx2000@mail.ru)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
  TSearchRecEx = Record
    Time : DCBasicTypes.TFileTime;  // modification time
    Size : Int64;
    Attr : TFileAttrs;
    Name : UTF8String;
    ExcludeAttr : TFileAttrs;
{$ifdef unix}
    FindHandle : Pointer;
{$else unix}
    FindHandle : THandle;
{$endif unix}
{$if defined(Win32) or defined(WinCE) or defined(Win64)}
    FindData : Windows.TWin32FindDataW;
{$endif}
{$ifdef netware_clib}
    FindData : TNetwareFindData;
{$endif}
{$ifdef netware_libc}
    FindData : TNetwareLibcFindData;
{$endif}
{$ifdef MacOS}
    FindData : TMacOSFindData;
{$endif}
  end;

function FindFirstEx (const Path : UTF8String; Attr : TFileAttrs; out SearchRec : TSearchRecEx) : Longint;
function FindNextEx (var SearchRec : TSearchRecEx) : Longint;
procedure FindCloseEx(var SearchRec: TSearchRecEx);
function CheckAttrMask(DefaultAttr : TFileAttrs; sAttr : String; Attr : TFileAttrs) : Boolean;

implementation

uses
  LCLProc, uDebug
  {$IFDEF UNIX}
  , uMyUnix, Unix, FileUtil, DCOSUtils
  {$ENDIF};

const
  faSpecial = faVolumeID or faDirectory;

function mbFindMatchingFile(var SearchRec: TSearchRecEx): Integer;
{$IFDEF MSWINDOWS}
begin
  with SearchRec do
  begin
    while (FindData.dwFileAttributes and ExcludeAttr) <> 0 do
      if not FindNextFileW(FindHandle, FindData) then Exit(GetLastError);

    Time:= TWinFileTime(FindData.ftLastWriteTime);
    Size:= (Int64(FindData.nFileSizeHigh) shl 32) + FindData.nFileSizeLow;
    Attr:= FindData.dwFileAttributes;
    Name:= UTF8Encode(WideString(FindData.cFileName));
  end;
  Result:= 0;
end;
{$ELSE}
var
  UnixFindData: PUnixFindData;
  WinAttr: LongInt;
begin
  Result:= -1;
  UnixFindData:= PUnixFindData(SearchRec.FindHandle);
  if UnixFindData = nil then Exit;
  if not Assigned(UnixFindData^.Mask) or
     UnixFindData^.Mask.Matches(SearchRec.Name) then
    begin
      if fpLStat(UTF8ToSys(UnixFindData^.sPath + SearchRec.Name), @UnixFindData^.StatRec) >= 0 then
      begin
        with UnixFindData^.StatRec do
        begin
          WinAttr:= LinuxToWinAttr(PChar(SearchRec.Name), UnixFindData^.StatRec);
          if (WinAttr and SearchRec.ExcludeAttr) <> 0 then Exit;
{$PUSH}
{$R-}
          SearchRec.Size:= st_size;
          SearchRec.Time:= st_mtime;
          SearchRec.Attr:= st_mode;
{$POP}
        end;
        Result:= 0;
      end;
    end;
end;
{$ENDIF}

function FindFirstEx (const Path : UTF8String; Attr : TFileAttrs; out SearchRec : TSearchRecEx) : Longint;
{$IFDEF MSWINDOWS}
var
  wPath: WideString;
begin
  wPath:= UTF8Decode(Path);
  SearchRec.ExcludeAttr:= not Attr and faSpecial;
  SearchRec.FindHandle:= FindFirstFileW(PWideChar(wPath), SearchRec.FindData);
  // if error then exit
  if SearchRec.FindHandle = INVALID_HANDLE_VALUE then Exit(GetLastError);
  Result:= mbFindMatchingFile(SearchRec);
end;
{$ELSE}
var
  UnixFindData: PUnixFindData;
begin
  //DCDebug('FindFirstEx with Path == ', Path);
  { Allocate UnixFindData }
  New(UnixFindData);
  FillChar(UnixFindData^, SizeOf(UnixFindData^), 0);
  SearchRec.FindHandle:= UnixFindData;
  SearchRec.ExcludeAttr:= not Attr and faSpecial;

  with UnixFindData^ do
  begin
    sPath:= ExtractFileDir(Path);
    if sPath = '' then
      sPath := mbGetCurrentDir;
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

    DirPtr:= fpOpenDir(PChar(UTF8ToSys(sPath)));
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
  UnixFindData: PUnixFindData absolute SearchRec.FindHandle;
  PtrDirEnt: pDirent;
begin
  Result:= -1;
  if UnixFindData = nil then Exit;
  if UnixFindData^.DirPtr = nil then Exit;
  PtrDirEnt:= fpReadDir(UnixFindData^.DirPtr);
  while PtrDirEnt <> nil do
  begin
    SearchRec.Name:= SysToUTF8(PtrDirEnt^.d_name);
    Result:= mbFindMatchingFile(SearchRec);
    if Result = 0 then // if found then exit
      Exit
    else // else read next
      PtrDirEnt:= fpReadDir(UnixFindData^.DirPtr);
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
    fpCloseDir(UnixFindData^.DirPtr);
  if Assigned(UnixFindData^.Mask) then
    UnixFindData^.Mask.Free;
  Dispose(UnixFindData);
  SearchRec.FindHandle:= nil;
end;
{$ENDIF}

function CheckAttrMask(DefaultAttr : TFileAttrs; sAttr : String; Attr : TFileAttrs) : Boolean;
{$IFDEF WINDOWS}
begin
  Result := True;
  if (DefaultAttr <> 0) and (DefaultAttr <> faAnyFile) then
    Result := (Attr and DefaultAttr) = DefaultAttr;
  if Length(sAttr) < 4 then Exit;
  if Result then
    begin
      if sAttr[1] = 'r' then Result := Result and ((Attr and faReadOnly) = faReadOnly)
      else if sAttr[1] = '-' then Result := Result and ((Attr and faReadOnly) <> faReadOnly);
      //WriteLN('After r == ', BoolToStr(Result));
      if sAttr[2] = 'a' then Result := Result and ((Attr and faArchive) = faArchive)
      else if sAttr[2] = '-' then Result := Result and ((Attr and faArchive) <> faArchive);
      //WriteLN('After a == ', BoolToStr(Result));
      if sAttr[3] = 'h' then Result := Result and ((Attr and faHidden) = faHidden)
      else if sAttr[3] = '-' then Result := Result and ((Attr and faHidden) <> faHidden);
      //WriteLN('After h == ', BoolToStr(Result));
      if sAttr[4] = 's' then Result := Result and ((Attr and faSysFile) = faSysFile)
      else if sAttr[4] = '-' then Result := Result and ((Attr and faSysFile) <> faSysFile);
  end;
end;
{$ELSE}
begin
  Result := True;
  if (DefaultAttr <> 0) and (DefaultAttr <> faAnyFile) then
    begin
      if Boolean(DefaultAttr and faDirectory) then
        Result := Result and fpS_ISDIR(Attr);
      DCDebug('Result do == ', BoolToStr(Result));
      if Boolean(DefaultAttr and faSymLink) then
        Result := Result and ((Attr and S_IFLNK) = S_IFLNK);
         DCDebug('Result after == ', BoolToStr(Result));
    end;
  if Length(sAttr) < 9 then Exit;

  if sAttr[1]='r' then Result:=Result and ((Attr AND S_IRUSR) = S_IRUSR)
  else if sAttr[1]='-' then Result:=Result and ((Attr AND S_IRUSR) <> S_IRUSR);
  if sAttr[2]='w' then Result:=Result and ((Attr AND S_IWUSR) = S_IWUSR)
  else if sAttr[2]='-' then Result:=Result and ((Attr AND S_IWUSR) <> S_IWUSR);
  if sAttr[3]='x' then Result:=Result and ((Attr AND S_IXUSR) = S_IXUSR)
  else if sAttr[3]='-' then Result:=Result and ((Attr AND S_IXUSR) <> S_IXUSR);
  if sAttr[4]='r' then Result:=Result and ((Attr AND S_IRGRP) = S_IRGRP)
  else if sAttr[4]='-' then Result:=Result and ((Attr AND S_IRGRP) <> S_IRGRP);
  if sAttr[5]='w' then Result:=Result and ((Attr AND S_IWGRP) = S_IWGRP)
  else if sAttr[5]='-' then Result:=Result and ((Attr AND S_IWGRP) <> S_IWGRP);
  if sAttr[6]='x' then Result:=Result and ((Attr AND S_IXGRP) = S_IXGRP)
  else if sAttr[6]='-' then Result:=Result and ((Attr AND S_IXGRP) <> S_IXGRP);
  if sAttr[7]='r' then Result:=Result and ((Attr AND S_IROTH) = S_IROTH)
  else if sAttr[7]='-' then Result:=Result and ((Attr AND S_IROTH) <> S_IROTH);
  if sAttr[8]='w' then Result:=Result and ((Attr AND S_IWOTH) = S_IWOTH)
  else if sAttr[8]='-' then Result:=Result and ((Attr AND S_IWOTH) <> S_IWOTH);
  if sAttr[9]='x' then Result:=Result and ((Attr AND S_IXOTH) = S_IXOTH)
  else if sAttr[9]='-' then Result:=Result and ((Attr AND S_IXOTH) <> S_IXOTH);

  if sAttr[3]='s' then Result:=Result and ((Attr AND STAT_ISUID) = STAT_ISUID);
  if sAttr[6]='s' then Result:=Result and ((Attr AND STAT_ISGID) = STAT_ISGID);
end;
{$ENDIF}

end.

