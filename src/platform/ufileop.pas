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
  SysUtils, uFileList, uTypes,lclproc;
  
function LoadFileInfoBySearchRec(Path: String; SearchRec: TSearchRecEx): TFileRecItem;
function LoadFilebyName(const sFileName: String): TFileRecItem;
function LoadFilesbyDir(const sDir: String; fl: TFileList): Boolean;
function AttrToStr(iAttr: Cardinal): String;

//function IsDirByName(const sName:String):Boolean;

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
  uFindEx, uGlobs, uDCUtils, uVFSUtil, uOSUtils {$IFDEF UNIX}, uUsersGroups, Unix, BaseUnix{$ENDIF}
  {$IFDEF WINDOWS}, Windows {$ENDIF};

{$IFDEF UNIX}   // *nix
function IsDirByName(const sName:String):Boolean;
var
  StatInfo: BaseUnix.Stat;
begin
  fpStat(PChar(sName),StatInfo);
  Result:= FPS_ISDIR(StatInfo.st_mode);
end;
{$ENDIF}

function LoadFileInfoBySearchRec(Path: String; SearchRec: TSearchRecEx): TFileRecItem;
{$IFDEF MSWINDOWS}
begin
  with Result do
  begin
    iSize:= SearchRec.Size;
    iMode:= SearchRec.Attr;
    bSysFile:= Boolean(SearchRec.Attr and faSysFile) or Boolean(SearchRec.Attr and faHidden);
    fTimeI:= FileDateToDateTime(SearchRec.Time);

    if FPS_ISDIR(iMode) or (SearchRec.Name[1]='.') then //!!!!!
      sExt:= ''
    else
      sExt:= ExtractFileExt(SearchRec.Name);
    sNameNoExt:= Copy(SearchRec.Name,1,Length(SearchRec.Name)-Length(sExt));
    sName:= SearchRec.Name;

    sTime:= FormatDateTime(gDateTimeFormat, fTimeI);
    bIsLink:= FPS_ISLNK(iMode);
    sLinkTo:= '';
    iDirSize:= 0;

    if bIsLink then
    begin
      sLinkTo:= ReadSymLink(SearchRec.Name);
    end;

    bExecutable:= not FPS_ISDIR(iMode); // for ShellExecute
    if bIsLink then
      bLinkIsDir:= True //Because symbolic link works on Windows 2k/XP for directories only
    else
      bLinkIsDir:= False;

    bSelected:= False;
    sModeStr:= AttrToStr(iMode);
    sPath:= Path;
  end; // with
end;
{$ENDIF}
{$IFDEF UNIX}
var
  sb: BaseUnix.Stat; //buffer for stat info
begin
  with Result do
  begin
    sb:= PUnixFindData(SearchRec.FindHandle)^.StatRec;
    iSize:=sb.st_size;

    iOwner:=sb.st_uid; //UID
    iGroup:=sb.st_gid; //GID
    sOwner:=UIDToStr(iOwner);
    sGroup:=GIDToStr(iGroup);
{/mate}
    iMode:=sb.st_mode;
    bSysFile := (SearchRec.Name[1] = '.') and (SearchRec.Name <> '..');
    fTimeI:= FileDateToDateTime(sb.st_mtime); // EncodeDate (1970, 1, 1) + (SearchRec.Time / 86400.0);


    if FPS_ISDIR(iMode) or (SearchRec.Name[1]='.') then //!!!!!
      sExt:= ''
    else
      sExt:= ExtractFileExt(SearchRec.Name);
    sNameNoExt:= Copy(SearchRec.Name,1,Length(SearchRec.Name)-Length(sExt));
    sName:= SearchRec.Name;

    sTime:= FormatDateTime(gDateTimeFormat, fTimeI);
    bIsLink:= FPS_ISLNK(iMode);
    sLinkTo:= '';
    iDirSize:= 0;

    if bIsLink then
    begin
      sLinkTo:= ReadSymLink(SearchRec.Name);
    end;

    if bIsLink then
      bLinkIsDir:= IsDirByName(sLinkTo)
    else
      bLinkIsDir:= False;
    bExecutable:= (not FPS_ISDIR(iMode)) and (iMode AND (S_IXUSR OR S_IXGRP OR S_IXOTH)>0);

    bSelected:= False;
    sModeStr:= AttrToStr(iMode);
    sPath:= Path;
  end; // with
end;
{$ENDIF}

function LoadFilebyName(const sFileName: String): TFileRecItem;
var
  fr:TFileRecItem;
  sr:TSearchRecEx;
begin
//  writeln('Enter LoadFilesbyDir');
  DebugLn('LoadFileByName sFileName = '+sFileName);
  if FindFirstEx(sFileName,faAnyFile,sr)<>0 then
  begin    DebugLn('FindFirst <> 0');
    with fr do     // append "blank dir"
    begin
      fr.sName:='';
      fr.sNameNoExt:='';
      fr.sExt:='';
      fr.iDirSize:=0;
      fr.iMode:=0;
      fr.bExecutable:=False;
      fr.bSysFile := False;
      fr.bIsLink:=False;
      fr.sLinkTo:='';
      fr.bLinkIsDir:=False;
      fr.bSelected:=False;
      fr.sModeStr:='';
      fr.iSize:=0;
      Result:=fr
    end;
    FindCloseEx(sr);
    Exit;
  end;
//  repeat

    // get TFileRecItem structure by SearchRec
    fr:= LoadFileInfoBySearchRec(ExtractFilePath(sr.Name), sr);

    Result:=fr;
//  until FindNext(sr)<>0;
  FindCloseEx(sr);
end;


function LoadFilesbyDir(const sDir: String; fl: TFileList): Boolean;
var
  fr: TFileRecItem;
  sr: TSearchRecEx;
  sParentDir: UTF8String;
begin
//  DebugLn('Enter LoadFilesbyDir');
  Result:= False;
  fl.Clear;
  fl.CurrentDirectory := IncludeTrailingPathDelimiter(sDir);
  if FindFirstEx('*',faAnyFile,sr) <> 0 then 
  begin
    { No files have been found. }
    FindCloseEx(sr);
    sParentDir:= GetParentDir(sDir);
	if sParentDir <> EmptyStr then // if parent dir exists then add up level item
	  AddUpLevel(sParentDir, fl);
    Exit;
  end;
  repeat
    if sr.Name='.' then Continue;
    if sr.Name='' then Continue;

    // Don't include '..' in the root directory.
    if ( (sDir=DirectorySeparator) or (sDir=(ExtractFileDrive(sDir)+PathDelim)) or (sDir=ExtractFileDrive(sDir))) and (sr.Name='..') then Continue;

    // get TFileRecItem structure by SearchRec
    fr:= LoadFileInfoBySearchRec(sDir, sr);

    fl.AddItem(@fr);
  until FindNextEx(sr)<>0;
  FindCloseEx(sr);
  Result:= True;
end;

function AttrToStr(iAttr: Cardinal): String;
{$IFDEF MSWINDOWS}
begin
  Result:= '--------';

  if FPS_ISDIR(iAttr) then Result[1]:='d';
  if FPS_ISLNK(iAttr) then Result[1]:='l';

  if Boolean(iAttr and FILE_ATTRIBUTE_READONLY   ) then Result[2] := 'r';
  if Boolean(iAttr and FILE_ATTRIBUTE_ARCHIVE    ) then Result[3] := 'a';
  if Boolean(iAttr and FILE_ATTRIBUTE_HIDDEN     ) then Result[4] := 'h';
  if Boolean(iAttr and FILE_ATTRIBUTE_SYSTEM     ) then Result[5] := 's';

  // These two are exclusive on NTFS.
  if Boolean(iAttr and FILE_ATTRIBUTE_COMPRESSED ) then Result[6] := 'c';
  if Boolean(iAttr and FILE_ATTRIBUTE_ENCRYPTED  ) then Result[6] := 'e';

  if Boolean(iAttr and FILE_ATTRIBUTE_TEMPORARY  ) then Result[7] := 't';
  if Boolean(iAttr and FILE_ATTRIBUTE_SPARSE_FILE) then Result[8] := 'p';

end;
{$ELSE}
begin
  Result:= '----------';

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
end;
{$ENDIF}

end.
