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

Alexander Koblov (Alexx2000@mail.ru)
   
}


unit uFindEx;

{$mode objfpc}{$H+}

interface

{$DEFINE FAKE_FIND}
{$DEFINE USE_STAT64}
{ $DEFINE USE_STAT64LIBC}    // libc version

uses
   SysUtils {$IFNDEF WIN32},BaseUnix, Unix{$IFDEF USE_STAT64LIBC}, Libc {$ELSE}, SysCall{$ENDIF}{$ENDIF};

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


function FindFirstEx (Const Path : String; Attr : Longint; out Rslt : TSearchRec) : Longint;
function FindNextEx (Var Rslt : TSearchRec) : Longint;
{$IFNDEF WIN32} //*nix systems

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

{$IFNDEF WIN32} //*nix systems

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

function FindFirstEx (Const Path : String; Attr : Longint; out Rslt : TSearchRec) : Longint;
begin
  Result := FindFirst(Path, Attr, Rslt);
  {$IFDEF UNIX}
  if Result = 0 then
    Rslt.Attr := Rslt.Mode;
  {$ENDIF}
end;

function FindNextEx (Var Rslt : TSearchRec) : Longint;
begin
  Result := FindNext(Rslt);
  {$IFDEF UNIX}
  if Result = 0 then
    Rslt.Attr := Rslt.Mode;
  {$ENDIF}
end;

end.

