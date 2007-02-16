{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

contributors:

Peter Cernoch 2002, pcernoch@volny.cz

Martin Matusu, xmat@volny.cz

Alexander Koblov (Alexx2000@mail.ru)

}

unit uFileOp;
{$mode objfpc}{$H+}
interface
uses
  uFileList, uTypes;

Function LoadFilesbyDir(const sDir:String; fl:TFileList):Boolean;
Function AttrToStr(iAttr:Cardinal):String;
//Function IsDirByName(const sName:String):Boolean;

const
  __S_IFMT        = $F000;
  __S_IFDIR       = $4000;
  __S_IFCHR       = $2000;
  __S_IFBLK       = $6000;
  __S_IFREG       = $8000;
  __S_IFIFO       = $1000;
  __S_IFLNK       = $A000;
  __S_IFSOCK      = $C000;

  __S_ISUID       = $800;
  __S_ISGID       = $400;
  __S_ISVTX       = $200;
  __S_IREAD       = $100;
  __S_IWRITE      = $80;
  __S_IEXEC       = $40;

    S_ISUID = __S_ISUID;
    S_ISGID = __S_ISGID;
    S_ISVTX = __S_ISVTX;

implementation
uses
  SysUtils, uFileProcs, FindEx, uOSUtils {$IFNDEF WIN32}, uUsersGroups, Unix, BaseUnix{$ENDIF};

{$IFNDEF WIN32}   // *nix
Function IsDirByName(const sName:String):Boolean;
var
  stat:stat64;
begin
  fpStat64(PChar(sName),stat);
  Result:=FPS_ISDIR(stat.st_mode);
end;
{$ENDIF}

Function LoadFilesbyDir(const sDir:String; fl:TFileList):Boolean;
var
  fr:TFileRecItem;
  sr:TSearchRec;
  sb: stat64; //buffer for stat64
  
begin
//  writeln('Enter LoadFilesbyDir');
  Result:=True;
  fl.Clear;
  if FindFirst('*',faAnyFile,sr)<>0 then
  begin
    with fr do     // append "blank dir"
    begin
      fr.sName:='..';
      fr.sNameNoExt:='..';
      fr.sExt:='';
      fr.iDirSize:=0;
      fr.iMode:=0;
      fr.bExecutable:=False;
      fr.bIsLink:=False;
      fr.sLinkTo:='';
      fr.bLinkIsDir:=False;
      fr.bSelected:=False;
      fr.sModeStr:='';
      fr.iSize:=0;
      fl.AddItem(@fr);
    end;
    FindClose(sr);
    Exit;
  end;
  repeat
    if sr.Name='.' then Continue;
    if (sDir=DirectorySeparator) and (sr.Name='..') then Continue;
//    if sr.Name='' then COntinue;

    {$IFNDEF WIN32}   // *nix
    Fplstat64(sr.Name,sb);
    fr.iSize:=sb.st_size;

    fr.iOwner:=sb.st_uid; //UID
    fr.iGroup:=sb.st_gid; //GID
    fr.sOwner:=UIDToStr(fr.iOwner);
    fr.sGroup:=GIDToStr(fr.iGroup);
{/mate}
    fr.iMode:=sb.st_mode;
    fr.fTimeI:= FileStampToDateTime(sb.st_mtime); // EncodeDate (1970, 1, 1) + (sr.Time / 86400.0);
    {$ELSE}  // Windows
     fr.iSize:= sr.Size;
     fr.iMode:= sr.Attr;
     fr.fTimeI:= FileDateToDateTime(sr.Time);
    {$ENDIF}
    
    if FPS_ISDIR(fr.iMode) or (sr.Name[1]='.') then //!!!!!
      fr.sExt:=''
    else
      fr.sExt:=ExtractFileExt(sr.Name);
    fr.sNameNoExt:=Copy(sr.Name,1,length(sr.Name)-length(fr.sExt));
    fr.sName:=sr.Name;

    fr.sTime:=DateTimeToStr(Trunc(fr.fTimeI));
    fr.bIsLink:=FPS_ISLNK(fr.iMode);
    fr.sLinkTo:='';
    fr.iDirSize:=0;
    {$IFDEF UNIX}   // *nix
    if fr.bIsLink then
    begin
      fr.sLinkTo:=fpReadLink(PChar(sr.Name));
    end;
    if fr.bIsLink then
      fr.bLinkIsDir:=IsDirByName(fr.sLinkTo)
    else
      fr.bLinkIsDir:=False;
    fr.bExecutable:=(not FPS_ISDIR(fr.iMode)) and (fr.iMode AND (S_IXUSR OR S_IXGRP OR S_IXOTH)>0);
    {$ELSE}  // Windows for ShellExecute
    fr.bExecutable:= not FPS_ISDIR(fr.iMode);
    fr.bLinkIsDir:=False;
    {$ENDIF}
    fr.bSelected:=False;
    fr.sModeStr:=AttrToStr(fr.iMode);
    fr.sPath := sDir;
    fl.AddItem(@fr);
  until FindNext(sr)<>0;
  FindClose(sr);
  Result:=True;
end;

Function AttrToStr(iAttr:Cardinal):String;
begin
  Result     := '----------';
  
{$IFDEF WIN32}

if Boolean(iAttr and $10) then Result[1] := 'd';
if Boolean(iAttr and $02) then Result[2] := 'h';
if Boolean(iAttr and $04) then Result[3] := 's';
if Boolean(iAttr and $08) then Result[4] := 'v';
if Boolean(iAttr and $01) then Result[5] := 'r';
if Boolean(iAttr and $20) then Result[6] := 'a';

{$ELSE}
  if FPS_ISDIR(iAttr) then Result[1]:='d';
  if FPS_ISLNK(iAttr) then Result[1]:='l';
  if FPS_ISSOCK(iAttr) then Result[1]:='s';
  if FPS_ISFIFO(iAttr) then Result[1]:='f';
  if FPS_ISBLK(iAttr) then Result[1]:='b';
  if FPS_ISCHR(iAttr) then Result[1]:='c';

  if ((iAttr AND S_IRUSR) = S_IRUSR) then Result[2]  := 'r';
  if ((iAttr AND S_IWUSR) = S_IWUSR) then Result[3]  := 'w';
  if ((iAttr AND S_IXUSR) = S_IXUSR) then Result[4]  := 'x';
  if ((iAttr AND S_IRGRP) = S_IRGRP) then Result[5]  := 'r';
  if ((iAttr AND S_IWGRP) = S_IWGRP) then Result[6]  := 'w';
  if ((iAttr AND S_IXGRP) = S_IXGRP) then Result[7]  := 'x';
  if ((iAttr AND S_IROTH) = S_IROTH) then Result[8]  := 'r';
  if ((iAttr AND S_IWOTH) = S_IWOTH) then Result[9]  := 'w';
  if ((iAttr AND S_IXOTH) = S_IXOTH) then Result[10] := 'x';

  if ((iAttr AND S_ISUID) = S_ISUID) then Result[4]  := 's';
  if ((iAttr AND S_ISGID) = S_ISGID) then Result[7]  := 's';
{$ENDIF}
end;

end.
