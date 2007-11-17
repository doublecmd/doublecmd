{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

some file rutines (obsolete)

contributors:

  Mattias Gaertner (from Lazarus code)
  
  Alexander Koblov (Alexx2000@mail.ru)

}
{$mode objfpc}{$H+}

unit uFileProcs;

interface
uses
  uTypes, ComCtrls;

type
  TFileProc= Function (fr:PFileRecItem; const sDst:String; pb:TProgressBar):Boolean;

function ForceDirectory(DirectoryName: string): boolean;


function FileStampToDateTime(TimeStamp:Longint):TDateTime; // not portable

Function CopyFile(const sSrc, sDst:String; bAppend:Boolean):Boolean;
Function MoveFile(const sSrc, sDst:String; pb:TProgressBar; iSrcRights:Integer):Boolean;
Function DelFile(const sSrc:String):Boolean;
Function RenFile(const sSrc, sDst:String):Boolean;

implementation
uses
  SysUtils, uShowMsg, Classes, uLng, uFindEx {$IFNDEF WIN32}, BaseUnix, UnixUtil{$ENDIF};

const
  cBlockSize=16384; // size of block if copyfile
// if pb is assigned > use, else work without pb :-)


Function CopyFile(const sSrc, sDst:String; bAppend:Boolean):Boolean;
var
  src, dst:TFileStream;
  stat:stat64;
  iDstBeg:Integer; // in the append mode we store original size
  Buffer: PChar;
  {$IFNDEF WIN32}
  utb:putimbuf;
  {$ENDIF}

begin
  Result:=False;

  dst:=nil; // for safety exception handling
  GetMem(Buffer,cBlockSize+1);

  try
    try
      src:=TFileStream.Create(sSrc,fmOpenReadWrite);
      if bAppend then
      begin
        dst:=TFileStream.Create(sDst,fmOpenReadWrite);
        dst.Seek(0,soFromEnd); // seek to end
      end
      else
        dst:=TFileStream.Create(sDst,fmCreate);
      iDstBeg:=dst.Size;
      // we dont't use CopyFrom, because it's alocate and free buffer every time is called
      while (dst.Size+cBlockSize)<= (src.Size+iDstBeg) do
      begin
        Src.ReadBuffer(Buffer^, cBlockSize);
        dst.WriteBuffer(Buffer^, cBlockSize);

      end;
      if (iDstBeg+src.Size)>dst.Size then
      begin
//        dst.CopyFrom(src,src.Size-dst.size);
        src.ReadBuffer(Buffer^, src.Size+iDstBeg-dst.size);
        dst.WriteBuffer(Buffer^, src.Size+iDstBeg-dst.size);
      end;
      {$IFNDEF WIN32} // *nix
      fpstat64(PChar(sSrc),stat);

  // file time
      new(utb);
      utb^.actime:=stat.st_atime;  //last access time // maybe now
      utb^.modtime:=stat.st_mtime; // last modification time
      fputime(PChar(sSrc),utb);
      dispose(utb);
   // end file

   // owner & group
      fpChown(PChar(sSrc),stat.st_uid, stat.st_gid);
   // mod
      fpChmod(PChar(sSrc), stat.st_mode);
      {$ENDIF}
      Result:=True;
    finally
      if assigned(src) then
        FreeAndNil(src);
      if assigned(dst) then
        FreeAndNil(dst);
      if assigned(Buffer) then
        FreeMem(Buffer);
    end;
  except
    on EFCreateError do
      msgError('!!!!EFCreateError');
    on EFOpenError do
      msgError('!!!!EFOpenError');
    on EWriteError do
      msgError('!!!!EFWriteError');
  end;
end;

Function MoveFile(const sSrc, sDst:String; pb:TProgressBar; iSrcRights:Integer):Boolean;
begin
  Result:=False;
  if CopyFile(sSrc, sDst,False) then
    Result:=DelFile(sSrc);
end;

// only wrapper for SysUtils.DeleteFile (raise Exception)
Function DelFile(const sSrc:String):Boolean;
begin
  Result:=SysUtils.DeleteFile(sSrc);
  if not Result then
    msgError(Format(rsMsgNotDelete,[sSrc]));
end;

Function RenFile(const sSrc, sDst:String):Boolean;
begin
  Result:=False;
  if FileExists(sDst) and not MsgYesNo(rsMsgFileExistsRwrt) then
    Exit;
  Result:=SysUtils.RenameFile(sSrc, sDst);
end;


function ForceDirectory(DirectoryName: string): boolean;
var
  i: integer;
  iBeg:Integer;
  sDir: string;
begin
  writeln('ForceDirectory:',DirectoryName);
  i:=1;
  iBeg:=1;
  if DirectoryName[Length(DirectoryName)]<>PathDelim then
    DirectoryName:=DirectoryName+PathDelim;
  writeln('ForceDirectory:',DirectoryName);
  while i<=length(DirectoryName) do
  begin
    if DirectoryName[i]=PathDelim then
    begin
      sDir:=copy(DirectoryName,1,i-1);
      if (sDir='') then
        {$IFDEF WIN32}
        sDir:='C:\'; // root
        {$ELSE}
        sDir:='/'; // root
        {$ENDIF}
        
      writeln('Dir:'+sDir);
      if not DirectoryExists(sDir) then
      begin
        writeln(copy(DirectoryName,1,iBeg-1));
        chdir(copy(DirectoryName,1,iBeg-1));
        Result:=CreateDir(Copy(DirectoryName, iBeg, i-iBeg));
        if not Result then exit;
      end;
      iBeg:=i+1;

    end;
    inc(i);
  end;
  Result:=true;
end;

function FileStampToDateTime(TimeStamp:Longint):TDateTime;
{$IFDEF WIN32}
begin
  Result := EncodeDate (1970, 1, 1) + (TimeStamp / 86400.0);
end;
{$ELSE}
Var
    Y,M,D,hh,mm,ss : word;
begin
    EpochToLocal(TimeStamp,y,m,d,hh,mm,ss);
    Result:=EncodeDate(y,m,d)+EncodeTime(hh,mm,ss,0);
end;
{$ENDIF}
end.
