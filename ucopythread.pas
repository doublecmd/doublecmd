{
Seksi Commander
----------------------------
Implementing of Copy files Thread

Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

contributors:

Alexander Koblov (Alexx2000@mail.ru)

}

unit uCopyThread;
{$mode objfpc}{$H+}
interface
uses
  uFileOpThread, uTypes;
type
  TCopyThread=Class(TFileOpThread)

  protected
    Function CpFile (fr:PFileRecItem; const sDst:String; bShowDlg:Boolean):Boolean;
    Function CopyFile(const sSrc, sDst:String; bAppend:Boolean):Boolean;
    procedure MainExecute; override;
    Function GetCaptionLng:String; override;
    function CorrectFileInfo(const sSrc, sDst:String):Boolean;
  end;

implementation
uses
  SysUtils, Classes, uLng, uShowMsg, uFilter, uFileProcs, FindEx, uOSUtils {$IFNDEF WIN32}, BaseUnix, Unix{$ENDIF};

const
  cBlockSize=16384; // size of block if copyfile

procedure TCopyThread.MainExecute;
var
  pr:PFileRecItem;
  xIndex:Integer;
  iCoped:Int64;
begin
  CorrectMask;
  FReplaceAll:=False;
  FSkipAll:=False;
  iCoped:=0;
  for xIndex:=0 to NewFileList.Count-1 do // copy
  begin
    if Terminated then
       Exit;
    pr:=NewFileList.GetItem(xIndex);
//    writeln(pr^.sname,' ',pr^.sNameNoExt);
    EstimateTime(iCoped);
    CpFile(pr,sDstPath, True);
    if not FPS_ISDIR(pr^.iMode) then
      inc(iCoped,pr^.iSize);
    FFileOpDlg.iProgress2Pos:=iCoped;
    Synchronize(@FFileOpDlg.UpdateDlg);
  end;
//  writeln('iCoped:',iCoped,' FFileSize', FFilesSize);
end;

// bShowDlg is only for Rename
Function TCopyThread.CpFile (fr:PFileRecItem; const sDst:String; bShowDlg:Boolean):Boolean;
var
  sDstExt:String;
  sDstName:String;
  sDstNew:String;
begin
//  writeln(fr^.sName);
//  writeln(fr^.sNameNoExt);

  DivFileName(fr^.sNameNoExt,sDstName, sDstExt);
  sDstName:=CorrectDstName(sDstName);
  sDstExt:=CorrectDstExt(sDstExt);
  sDstNew:='';
  if sDstName<>'' then
    sDstNew:=sDstName;
  if sDstExt<>'.' then
    sDstNew:=sDstNew+sDstExt;
//  writeln(sDstNew);
  FFileOpDlg.sFileName:=ExtractFileName(fr^.sName)+' -> '+fr^.sPath+sDstNew;
  Synchronize(@FFileOpDlg.UpdateDlg);
  if FPS_ISLNK(fr^.iMode) then
  begin
    // use sDstName as link target
    {$IFNDEF WIN32}  // Alexx2000
    sDstName:=fpReadlink(fr^.sName);
    {$ENDIF}
    if sDstName<>'' then
    begin
      if not CreateSymlink(fr^.sName, sDstName) then
        writeln('Symlink error:');
    end
    else
      writeln('Error reading link');
   Result:=True;
  end
  else
  if FPS_ISDIR(fr^.iMode) then
   begin
    if not DirectoryExists(sDst+fr^.sPath+fr^.sNameNoExt) then
      uFileProcs.ForceDirectory(sDst+fr^.sPath+fr^.sNameNoExt);
    Result:=True;
   end
  else
  begin // files and other stuff
//    writeln('fr^.sPath:'+fr^.sPath);
    if bShowDlg then
    begin
      Result:=False;
//      writeln('testing:'+sDst+fr^.sPath+sDstNew);
      if FileExists(sDst+fr^.sPath+sDstNew) and not FReplaceAll then
      begin
        if FSkipAll then
        begin
          Result:=True;
          Exit;
        end;
        if not DlgFileExist(Format(lngGetString(clngMsgFileExistsRwrt),[sDst+fr^.sPath+sDstNew, fr^.sName])) then
          Exit;
      end;   
    end;
    Result:=CopyFile(fr^.sName, sDst+fr^.sPath+sDstNew, FAppend);
  end;
end;

Function TCopyThread.CopyFile(const sSrc, sDst:String; bAppend:Boolean):Boolean;
var
  src, dst:TFileStream;
//  bAppend:Boolean;
  iDstBeg:Int64; // in the append mode we store original size
  Buffer:PChar;
begin
  Result:=False;

  writeln('CopyFile:',sSrc,' ',sDst);
  GetMem(Buffer, cBlockSize+1);
  dst:=nil; // for safety exception handling
  try
    try
      src:=TFileStream.Create(sSrc,fmOpenRead);
      writeln(sDst);
      if bAppend then
      begin
        dst:=TFileStream.Create(sDst,fmOpenReadWrite);
        dst.Seek(0,soFromEnd); // seek to end
      end
      else

         dst:=TFileStream.Create(sDst,fmCreate);
      iDstBeg:=dst.Size;
      // we dont't use CopyFrom, because it's alocate and free buffer every time is called
      FFileOpDlg.iProgress1Pos:=0;
      FFileOpDlg.iProgress1Max:=src.Size;
      writeln('SrcSize:',src.Size);
//      writeln(FFileOpDlg.iProgress1Max);
      Synchronize(@FFileOpDlg.UpdateDlg);

      while (dst.Size+cBlockSize)<= (src.Size+iDstBeg) do
      begin
        if Terminated then
          Exit;
        Src.ReadBuffer(Buffer^, cBlockSize);
        dst.WriteBuffer(Buffer^, cBlockSize);
        FFileOpDlg.iProgress1Pos:=dst.Size;
        Synchronize(@FFileOpDlg.UpdateDlg);
      end;
      if (iDstBeg+src.Size)>dst.Size then
      begin
        src.ReadBuffer(Buffer^, src.Size+iDstBeg-dst.size);
        dst.WriteBuffer(Buffer^, src.Size+iDstBeg-dst.size);
      end;
      FFileOpDlg.iProgress1Pos:=dst.Size;
      Synchronize(@FFileOpDlg.UpdateDlg);
      Result:=CorrectFileInfo(sSrc, sDst); // chmod, chgrp, udate a spol
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

function TCopyThread.CorrectFileInfo(const sSrc, sDst:String):Boolean;
{$IFDEF WIN32}  // Alexx2000
begin
Result := True;
end;
{$ELSE}  // *nix
var
  stat:Stat64;
  utb:PUTimBuf;

begin
  fpstat64(PChar(sSrc),stat);
//  writeln(AttrToStr(stat.st_mode));  // file time
  new(utb);
  utb^.actime:=stat.st_atime;  //last access time // maybe now
  utb^.modtime:=stat.st_mtime; // last modification time
  fputime(PChar(sDst),utb);
  dispose(utb);
// end file

// owner & group
  if fpChown(PChar(sDst),stat.st_uid, stat.st_gid)=-1 then
  begin
    // development messages
    writeln(Format('chown (%s) failed',[sSrc]));
  end;
// mod
  if fpChmod(PChar(sDst), stat.st_mode)=-1 then
  begin
    // development messages
    writeln(Format('chmod (%s) failed',[sSrc]));
  end;
  Result:=True;
end;
{$ENDIF}

Function TCopyThread.GetCaptionLng:String;
begin
  Result:=lngGetString(clngDlgCp);
end;


end.
