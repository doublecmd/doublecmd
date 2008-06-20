{
   File name: FindEx.pas
   Date:      2004/05/xx
   Author:    Radek Cervinka  <radek.cervinka@centrum.cz>

   very fast file utils for 64 bit access
   
   fpStat64, fplStat64, Find*64
   

   Copyright (C) 2004

   Licence: GNU LGPL or later
   
   Warning Libc version is not much tested

   contributors:

   Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)
}


unit uFindEx;

{$mode objfpc}{$H+}

interface

{$DEFINE FAKE_FIND}
{$DEFINE USE_STAT64}
{$IFDEF CPU64}
{$DEFINE USE_STAT64LIBC}    // libc version
{$ENDIF}

uses
   SysUtils {$IFNDEF WIN32},BaseUnix, Unix{$IFDEF USE_STAT64LIBC}, Libc {$ELSE}, SysCall{$ENDIF}{$ELSE}, Windows{$ENDIF};

Type
  TFindStatus = (fsOK, fsStatFailed, fsBadAttr);

{$IFDEF USE_STAT64}
  {$IFDEF USE_STAT64LIBC}
  Stat64 = Libc._stat64;
  {$ELSE}
   // for kernel syscall check structure
   {$I stat64.inc}
   
  {$ENDIF}
{$ENDIF}


function FindFirstEx (const Path : UTF8String; Attr : Longint; out Rslt : TSearchRec) : Longint;
function FindNextEx (var Rslt : TSearchRec) : Longint;
function CheckAttrMask(DefaultAttr : Cardinal; sAttr : String; Attr : Cardinal) : Boolean;
{$IFDEF UNIX} //*nix systems

{$IFNDEF FAKE_FIND}
function FindStat (Var Rslt : TSearchRec) :TFindStatus;
{$ENDIF}

{$IFDEF USE_STAT64}
function Fpstat64(path:String; var buf:stat64):cint;
function Fplstat64(path:String; var buf:stat64):cint;
{$IFNDEF FAKE_FIND}
function FindStat64 (Var Rslt : TSearchRec) :TFindStatus;
{$ENDIF}
{$ENDIF}

{$ENDIF} //*nix systems

implementation
uses LCLProc;

{$IFDEF UNIX} //*nix systems

{$IFNDEF FAKE_FIND}
Function GlobToTSearchRec (Var Info : TSearchRec) : Boolean;

Var
  p     : Pglob;
  GlobSearchRec : PGlobSearchRecEx;

begin
  GlobSearchRec:=PGlobSearchRecEx(Info.FindHandle);
  P:=GlobSearchRec^.GlobHandle;
  Result:=P<>Nil;
  If Result then
  begin
    GlobSearchRec^.GlobHandle:=P^.Next;
    With Info do
    begin
      If P^.Name<>Nil then
        Name:=strpas(p^.name)
      else
        Name:='';
      GlobSearchRec^.LastName:=Name;
    end;
    P^.Next:=Nil;
    Unix.GlobFree(P);
  end;
end;


Function DoFind(Var Rslt : TSearchRec) : Longint;

Var
  GlobSearchRec : PGlobSearchRecEx;

begin
  Result:=-1;
  GlobSearchRec:=PGlobSearchRecEx(Rslt.FindHandle);
  If (GlobSearchRec^.GlobHandle<>Nil) then
    While (GlobSearchRec^.GlobHandle<>Nil) and not (Result=0) do
      If GlobToTSearchRec(Rslt) Then Result:=0;
end;
{$ENDIF}

{$IFNDEF FAKE_FIND}
Function LinuxToWinAttr (FN : Pchar; Const Info : BaseUnix.Stat) : Longint;

begin
  Result:=faArchive;
  If fpS_ISDIR(Info.st_mode) then
    Result:=Result or faDirectory;
  If (FN[0]='.') and (not (FN[1] in [#0,'.']))  then
    Result:=Result or faHidden;
  If (Info.st_Mode and S_IWUSR)=0 Then
     Result:=Result or faReadOnly;
  If fpS_ISSOCK(Info.st_mode) or fpS_ISBLK(Info.st_mode) or fpS_ISCHR(Info.st_mode) or fpS_ISFIFO(Info.st_mode) Then
     Result:=Result or faSysFile;
end;
{$ENDIF}

{$IFNDEF FAKE_FIND}
function FindStat (Var Rslt : TSearchRec) :TFindStatus;
Var
  SInfo : BaseUnix.Stat;
  GlobSearchRec : PGlobSearchRecEx;

begin
  Result:=fsOK;
  GlobSearchRec:=PGlobSearchrecEx(Rslt.FindHandle);

  if Fpstat(GlobSearchRec^.Path+GlobSearchRec^.LastName,SInfo)<0 then
    Result:=fsStatFailed;
  If Result = fsOK then
  begin
    Rslt.Attr:=LinuxToWinAttr(PChar(GlobSearchRec^.LastName),SInfo);
    // hmm, attr support is not good
    if (Rslt.ExcludeAttr and Rslt.Attr)<>0 then
      Result:=fsBadAttr;
    If Result = fsOK Then
       With Rslt do
       begin
         Attr:=Rslt.Attr;
         Time:=Sinfo.st_mtime;
         Size:=Sinfo.st_Size;
       end;
  end;
end;
{$ENDIF}

{$IFDEF USE_STAT64}

Function LinuxToWinAttr64 (FN : Pchar; Const Info : Stat64) : Longint;

begin
  Result:=faArchive;
  If fpS_ISDIR(Info.st_mode) then
    Result:=Result or faDirectory;
  If (FN[0]='.') and (not (FN[1] in [#0,'.']))  then
    Result:=Result or faHidden;
  If (Info.st_Mode and S_IWUSR)=0 Then
     Result:=Result or faReadOnly;
  If fpS_ISSOCK(Info.st_mode) or fpS_ISBLK(Info.st_mode) or fpS_ISCHR(Info.st_mode) or fpS_ISFIFO(Info.st_mode) Then
     Result:=Result or faSysFile;
end;

{$IFDEF USE_STAT64LIBC}
function Fpstat64(path:String; var buf:stat64):cint;
begin
  Result:=Libc.stat64(Pchar(path),buf);
end;

function Fplstat64(path: String; var buf: stat64): cint;
begin
  Result:=Libc.lstat64(Pchar(path),buf);
end;

{$ELSE}
function Fpstat64(path:String; var buf:stat64):cint;
begin
  Result:=do_syscall(syscall_nr_stat64,TSysParam(PChar(path)),TSysParam(@buf));
end;

function Fplstat64(path: String; var buf: stat64): cint;
begin
  Result:=do_syscall(syscall_nr_lstat64,TSysParam(PChar(path)),TSysParam(@buf));
end;

{$ENDIF}

{$IFNDEF FAKE_FIND}
function FindStat64 (Var Rslt : TSearchRec) :TFindStatus;
Var
  SInfo : Stat64;
  GlobSearchRec : PGlobSearchRecEx;

begin
  Result:=fsOK;
  GlobSearchRec:=PGlobSearchrecEx(Rslt.FindHandle);
  if Fpstat64(GlobSearchRec^.Path+GlobSearchRec^.LastName,SInfo)<0 then
    Result:=fsStatFailed;
  If Result = fsOK then
  begin
    Rslt.Attr:=LinuxToWinAttr64(PChar(GlobSearchRec^.LastName),SInfo);
    // hmm, attr support is not good
    if (Rslt.ExcludeAttr and Rslt.Attr)<>0 then
      Result:=fsBadAttr;
    If Result = fsOK Then
       With Rslt do
       begin
         Attr:=Rslt.Attr;
         Time:=Sinfo.st_mtime;
         Size:=Sinfo.st_Size;
       end;
  end;
end;

{$ENDIF}
{$ENDIF}

{$ENDIF} //*nix systems

{$IFDEF MSWINDOWS}
function mbFindMatchingFile(var Rslt: TSearchRec): Integer;
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
begin  
  if (Attr and faSymLink) = faSymLink then
    Attr:= Attr or not faSymLink;
  // call standart FindFirst function
  Result:= FindFirst(Path, Attr, Rslt);
  // and replace Attr by Mode  
  if Result = 0 then
    Rslt.Attr:= Rslt.Mode;
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
begin
  // call standart FindNext function
  Result:= FindNext(Rslt);
  // and replace Attr by Mode
  if Result = 0 then
    Rslt.Attr:= Rslt.Mode;  
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

