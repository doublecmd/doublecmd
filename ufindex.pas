{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains UTF8 versions of Find(First, Next) functions and other stuff
    
    Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)

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
   SysUtils {$IFDEF UNIX}, BaseUnix, UnixUtil, uMyUnix{$ELSE}, Windows{$ENDIF};

{$IFDEF UNIX}
type
  TUnixFindData = record
    DirPtr: PDir;   //en> directory pointer for reading directory
    sPath: String;  //en> file name path
    sMask: String;  //en> file name mask
    iAttr: LongInt; //en> attribute we are searching for
    StatRec: Stat;
  end;
  PUnixFindData = ^TUnixFindData;
{$ENDIF}

function FindFirstEx (const Path : UTF8String; Attr : Longint; out Rslt : TSearchRec) : Longint;
function FindNextEx (var Rslt : TSearchRec) : Longint;
procedure FindCloseEx(var Rslt: TSearchRec);
function CheckAttrMask(DefaultAttr : Cardinal; sAttr : String; Attr : Cardinal) : Boolean;

implementation
uses LCLProc, uFileOp;

function mbFindMatchingFile(var Rslt: TSearchRec): Integer;
{$IFDEF MSWINDOWS}
var
  LocalFileTime: TFileTime;
  wFindData: TWin32FindDataW; 
  pwFindData: PWIN32FINDDATAW absolute Rslt.FindData; // for use PWin32FindDataW instead TWin32FindData   
begin
  with Rslt do
  begin
   wFindData:= pwFindData^;
    while (wFindData.dwFileAttributes and ExcludeAttr) <> 0 do
      if not FindNextFileW(FindHandle, wFindData) then Exit(GetLastError);
    
    pwFindData:= @wFindData;
    FileTimeToLocalFileTime(wFindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi, LongRec(Time).Lo);
    Size:= (Int64(wFindData.nFileSizeHigh) shl 32) + wFindData.nFileSizeLow;
    Attr:= wFindData.dwFileAttributes;
    Name:= UTF8Encode(wFindData.cFileName);
  end;
  Result:= 0;
end;
{$ELSE}
var
  UnixFindData: PUnixFindData;
  WinAttr: LongInt;
begin
  Result:= -1;
  UnixFindData:= PUnixFindData(Rslt.FindHandle);
  if UnixFindData = nil then Exit;
  if FNMatch(UnixFindData^.sMask, Rslt.Name) then
    begin
      if fpLStat(UnixFindData^.sPath + Rslt.Name, @UnixFindData^.StatRec) >= 0 then
        with Rslt, UnixFindData^.StatRec do
        begin
          WinAttr:= LinuxToWinAttr(PChar(Rslt.Name), UnixFindData^.StatRec);
          if (WinAttr and UnixFindData^.iAttr) = 0 then Exit;
          Size:= st_size;
          Time:= UnixToWinAge(st_mtime);
          Attr:= st_mode;
        end;
      Result:= 0;
    end;
end;
{$ENDIF}

function FindFirstEx (const Path : UTF8String; Attr : Longint; out Rslt : TSearchRec) : Longint;
{$IFDEF MSWINDOWS}
const
  faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;
var
  wPath: WideString;
  wFindData: TWin32FindDataW;
  pwFindData: PWIN32FINDDATAW absolute Rslt.FindData; // for use PWin32FindDataW instead TWin32FindData 
begin
  wPath:= UTF8Decode(Path);
  Rslt.ExcludeAttr:= not Attr and faSpecial;
  Rslt.FindHandle:= FindFirstFileW(PWideChar(wPath), wFindData);
  // if error then exit
  if Rslt.FindHandle = INVALID_HANDLE_VALUE then Exit(GetLastError);	
  
  pwFindData:= @wFindData;	
  
  Result:= mbFindMatchingFile(Rslt);
end;
{$ELSE}
var
  UnixFindData: PUnixFindData;
begin
  DebugLn('FindFirstEx with Path == ', Path);
  { Allocate UnixFindData }
  New(UnixFindData);
  FillChar(UnixFindData^, SizeOf(UnixFindData^), 0);
  Rslt.FindHandle:= UnixFindData;

  with UnixFindData^ do
  begin
    iAttr:= Attr;
    sPath:= ExtractFileDir(Path);
    sMask:= ExtractFileName(Path);
    if sPath = '' then
      GetDir(0, sPath);
    if sMask = '' then
      sMask:= '*';
    sPath:= IncludeTrailingBackSlash(sPath);

    if (Pos('?', sMask) = 0) and (Pos('*', sMask) = 0) and FileExists(Path) then
      begin
        Rslt.Name:= sMask;
        if mbFindMatchingFile(Rslt) = 0 then
          Exit(0);
      end;

    DirPtr:= fpOpenDir(sPath);
  end;
  Result:= FindNextEx(Rslt);
end;
{$ENDIF}

function FindNextEx (var Rslt : TSearchRec) : Longint;
{$IFDEF MSWINDOWS}
var  
  wFindData: TWin32FindDataW;
  pwFindData: PWIN32FINDDATAW absolute Rslt.FindData; // for use PWin32FindDataW instead TWin32FindData    
begin
  wFindData:= pwFindData^;
  if FindNextFileW(Rslt.FindHandle, wFindData) then
    begin
      pwFindData:= @wFindData;
      Result:= mbFindMatchingFile(Rslt);
    end
  else
    Result:= GetLastError;
end;
{$ELSE}
var
  UnixFindData: PUnixFindData absolute Rslt.FindHandle;
  PtrDirEnt: pDirent;
begin
  Result:= -1;
  if UnixFindData = nil then Exit;
  if UnixFindData^.DirPtr = nil then Exit;
  PtrDirEnt:= fpReadDir(UnixFindData^.DirPtr^);
  while PtrDirEnt <> nil do
  begin
    Rslt.Name:= PtrDirEnt^.d_name;
    Result:= mbFindMatchingFile(Rslt);
    if Result = 0 then // if found then exit
      Exit
    else // else read next
      PtrDirEnt:= fpReadDir(UnixFindData^.DirPtr^);
  end;
end;
{$ENDIF}

procedure FindCloseEx(var Rslt: TSearchRec);
{$IFDEF MSWINDOWS}
begin
   if Rslt.FindHandle <> INVALID_HANDLE_VALUE then
    Windows.FindClose(Rslt.FindHandle);
end;
{$ELSE}
var
  UnixFindData: PUnixFindData absolute Rslt.FindHandle;
begin
  if UnixFindData = nil then Exit;
  if UnixFindData^.DirPtr <> nil then
    fpCloseDir(UnixFindData^.DirPtr^);
  Dispose(UnixFindData);
  Rslt.FindHandle:= nil;
end;
{$ENDIF}

function CheckAttrMask(DefaultAttr : Cardinal; sAttr : String; Attr : Cardinal) : Boolean;
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
      DebugLn('Result do == ', BoolToStr(Result));
      if Boolean(DefaultAttr and faSymLink) then
        Result := Result and ((Attr and S_IFLNK) = S_IFLNK);
         DebugLn('Result after == ', BoolToStr(Result));
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

  if sAttr[3]='s' then Result:=Result and ((Attr AND S_ISUID) = S_ISUID);
  if sAttr[6]='s' then Result:=Result and ((Attr AND S_ISGID) = S_ISGID);
end;
{$ENDIF}

end.

