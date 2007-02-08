unit uVFSutil;
{
  Some utils for VFS system (and maybe classic fs) .
  Part of Commander, realised under GNU GPL 2.  (C)opyright 2003

Authors:
  Radek Cervinka, radek.cervinka@centrum.cz


contributors:

Alexander Koblov (Alexx2000@mail.ru)

}


interface

uses
  Classes, uTypes;

function ls2FileInfo(sls:string):TFileRecItem;
// convert line in ls -la (or vfs) format to our structure
function ModeStr2Mode(const sMode:String):Integer;

implementation

uses
  SysUtils, uFileOp {$IFNDEF WIN32}, BaseUnix{$ENDIF};

{ TFileList }
function ModeStr2Mode(const sMode:String):Integer;
begin
// stupid conversion
  Result:=0;
{$IFDEF WIN32}

if sMode[1] = 'd' then Result := Result or $10;
if sMode[2] = 'h' then Result := Result or $02;
if sMode[3] = 's' then Result := Result or $04;
if sMode[4] = 'v' then Result := Result or $08;
if sMode[5] = 'r' then Result := Result or $01;
if sMode[6] = 'a' then Result := Result or $20;

{$ELSE}
//  if sMode[1]='-' then Result:=Result+10;
  if sMode[1]='d' then Result:=Result or __S_IFDIR;
  if sMode[1]='l' then Result:=Result or __S_IFLNK;
  if sMode[1]='s' then Result:=Result or __S_IFSOCK;
  if sMode[1]='f' then Result:=Result or __S_IFIFO;
  if sMode[1]='b' then Result:=Result or __S_IFBLK;
  if sMode[1]='c' then Result:=Result or __S_IFCHR;


  if sMode[2]='r' then Result:=Result or S_IRUSR;
  if sMode[3]='w' then Result:=Result or S_IWUSR;
  if sMode[4]='x' then Result:=Result or S_IXUSR;
  if sMode[5]='r' then Result:=Result or S_IRGRP;
  if sMode[6]='w' then Result:=Result or S_IWGRP;
  if sMode[7]='x' then Result:=Result or S_IXGRP;
  if sMode[8]='r' then Result:=Result or S_IROTH;
  if sMode[9]='w' then Result:=Result or S_IWOTH;
  if sMode[10]='x' then Result:=Result or S_IXOTH;

  if sMode[4]='s' then Result:=Result or S_ISUID;
  if sMode[7]='s' then Result:=Result or S_ISGID;
{$ENDIF}
end;

function ls2FileInfo(sls:string):TFileRecItem;
var
  i:Integer;
  iPoz:Integer;
begin
  if sls='' then
    Raise Exception.Create('ls2FileInfo: sls is empty');
  Result.sModeStr:=Copy(sls,1,10); // Atrrs
  Result.iMode:=ModeStr2Mode(Result.sModeStr);
  iPoz:=11;
  i:=iPoz;
  while sls[i]<=' ' do
    inc(i);

//  Result.iNr:=StrToInt(sls[i]); //???
  iPoz:=i+1;
  i:=iPoz+1;
  while sls[i]>' ' do
    inc(i);

  Result.sOwner:=Trim(copy(sls,iPoz,i-iPoz)); // Owner
  while sls[i]<=' ' do
    inc(i);

  iPoz:=i;
  while sls[i]>' ' do
    inc(i);
  Result.sGroup:=Trim(copy(sls,iPoz,i-iPoz));  //Group

  while sls[i]<=' ' do
    inc(i);

  iPoz:=i;
  while sls[i]>' ' do
    inc(i);
  Result.iSize:=StrToInt(copy(sls,iPoz,i-iPoz));  // FileSize
  while sls[i]<=' ' do
    inc(i);

  iPoz:=i;
  i:=length(sls);
  while (sls[i]>' ') and (i>iPoz) do
    dec(i);

  Result.sTime:=Copy(sls,iPoz,i-iPoz); // date
//  Result.fTimeI:=StrToDateTime(Result.fTime);
//  Inc(iPoz,12);
  iPoz:=i;
  Result.sName:=Trim(Copy(sls,iPoz,length(sls)-iPoz+1)); // FileName
  Result.sExt:='';
  Result.sNameNoExt:=Result.sName;


  Result.bSelected:=False;
  Result.bExecutable:=False;
  Result.bIsLink:=False;
  Result.bLinkIsDir:=False;
  Result.iDirSize:=0;
end;



end.
