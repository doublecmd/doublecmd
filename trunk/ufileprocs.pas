{
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   some file rutines (obsolete)

   contributors:

   Mattias Gaertner (from Lazarus code)
  
   Copyright (C) 2007-2008  Koblov Alexander (Alexx2000@mail.ru)
}

unit uFileProcs;

{$mode objfpc}{$H+}

interface
uses
  uTypes, ComCtrls;

type
  TFileProc = function (fr:PFileRecItem; const sDst:String; pb:TProgressBar):Boolean;

function ForceDirectory(DirectoryName: string): boolean;

function CopyFile(const sSrc, sDst:String; bAppend:Boolean=False):Boolean;
function MoveFile(const sSrc, sDst:String; pb:TProgressBar; iSrcRights:Integer):Boolean;
function DelFile(const sSrc:String):Boolean;
function RenFile(const sSrc, sDst:String):Boolean;

implementation
uses
  LCLProc, SysUtils, uGlobs, uShowMsg, Classes, uLng, uDCUtils, uFindEx, uOSUtils;

const
  cBlockSize=16384; // size of block if copyfile
// if pb is assigned > use, else work without pb :-)


function CopyFile(const sSrc, sDst:String; bAppend:Boolean):Boolean;
var
  src, dst:TFileStream;
  stat:stat64;
  iDstBeg:Integer; // in the append mode we store original size
  Buffer: PChar;
begin
  Result:=False;
  if not FileExists(sSrc) then Exit;
  
  dst:=nil; // for safety exception handling
  GetMem(Buffer,cBlockSize+1);

  try
    try
      src:=TFileStream.Create(sSrc,fmOpenRead or fmShareDenyNone);
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
      Result := FileCopyAttr(sSrc, sDst, gDropReadOnlyFlag); // chmod, chgrp
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

function MoveFile(const sSrc, sDst:String; pb:TProgressBar; iSrcRights:Integer):Boolean;
begin
  Result:=False;
  if CopyFile(sSrc, sDst,False) then
    Result:=DelFile(sSrc);
end;

// only wrapper for SysUtils.DeleteFile (raise Exception)
function DelFile(const sSrc:String):Boolean;
begin
  Result:= mbDeleteFile(sSrc);
  if not Result then
    msgError(Format(rsMsgNotDelete,[sSrc]));
end;

function RenFile(const sSrc, sDst:String):Boolean;
begin
  Result:=False;
  if mbFileExists(sDst) and not MsgYesNo(rsMsgFileExistsRwrt) then
    Exit;
  Result:=SysUtils.RenameFile(sSrc, sDst);
end;


function ForceDirectory(DirectoryName: string): boolean;
var
  i: integer;
  iBeg:Integer;
  sDir: string;
begin
  //DebugLn('ForceDirectory:',DirectoryName);
  i:=1;
  iBeg:=1;
  if DirectoryName[Length(DirectoryName)]<>PathDelim then
    DirectoryName:=DirectoryName+PathDelim;
  //DebugLn('ForceDirectory:',DirectoryName);
  
  if Pos('\\', DirectoryName) = 1 then // if network path
    begin
      i := CharPos(PathDelim, DirectoryName, 3); // index of the end of computer name
      i := CharPos(PathDelim, DirectoryName, i + 1); // index of the end of first remote directory
    end;  
  
  while i<=length(DirectoryName) do
  begin
    if DirectoryName[i]=PathDelim then
    begin
      sDir:=copy(DirectoryName,1,i-1);
      if (sDir='') then
        GetDir(0, sDir);

      //DebugLn('Dir: ' + sDir);
      if not mbDirectoryExists(sDir) then
      begin
        //DebugLn(copy(DirectoryName,1,iBeg-1));
        mbSetCurrentDir(copy(DirectoryName,1,iBeg-1));
        Result:=mbCreateDir(Copy(DirectoryName, iBeg, i-iBeg));
        if not Result then exit;
      end;
      iBeg:=i+1;

    end;
    inc(i);
  end;
  Result := True;
end;

end.
