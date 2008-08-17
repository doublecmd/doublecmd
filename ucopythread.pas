{
   Seksi Commander
   ----------------------------
   Implementing of Copy files Thread

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:

   Copyright (C) 2007-2008  Koblov Alexander (Alexx2000@mail.ru)
}

unit uCopyThread;

{$mode objfpc}{$H+}

interface
uses
  uFileOpThread, uTypes;
type
  TCopyThread = class(TFileOpThread)

  protected
    Function CpFile (fr:PFileRecItem; const sDst:String; bShowDlg:Boolean):Boolean;
    Function CopyFile(const sSrc, sDst:String; bAppend:Boolean):Boolean;
    procedure MainExecute; override;
    Function GetCaptionLng:String; override;
  end;

implementation
uses
  LCLProc, SysUtils, Classes, uLng, uGlobs, uLog, uShowMsg, uFileProcs, uFindEx, uDCUtils, uOSUtils, uClassesEx;

procedure TCopyThread.MainExecute;
var
  pr:PFileRecItem;
  xIndex:Integer;
  iCoped:Int64;
  iTotalDiskSize,
  iFreeDiskSize : Int64;
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

    {Check disk free space}
    GetDiskFreeSpace(sDstPath, iFreeDiskSize, iTotalDiskSize);
    if pr^.iSize > iFreeDiskSize then
      begin
        case MsgBox(Self, rsMsgNoFreeSpaceCont, [msmbYes, msmbNo,msmbSkip], msmbYes, msmbNo) of
          mmrNo:
            Exit;
          mmrSkip:
            Continue;
        end;
      end;

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
  DebugLn('NameNoExt ==' +fr^.sNameNoExt);

  DivFileName(fr^.sNameNoExt,sDstName, sDstExt);
  sDstName:=CorrectDstName(sDstName);
  DebugLn('sDstName ==' + sDstName);
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
      sDstName:=ReadSymLink(fr^.sName);
      if sDstName<>'' then
            begin
              sDstName := GetAbsoluteFileName(ExtractFilePath(fr^.sName), sDstName);
              DebugLn('ReadSymLink := ' + sDstName);
              if not CreateSymlink(sDstName, sDst+fr^.sPath+sDstNew) then
                DebugLn('Symlink error:');
            end
          else
            DebugLn('Error reading link');
          Result:=True;
    end
  else
  if FPS_ISDIR(fr^.iMode) then
   begin
   DebugLn('Force =' + sDst+fr^.sPath+fr^.sNameNoExt);
    if not mbDirectoryExists(sDst+fr^.sPath+fr^.sNameNoExt) then
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
      if mbFileExists(sDst+fr^.sPath+sDstNew) and not FReplaceAll then
      begin
        if FSkipAll then
        begin
          Result:=True;
          Exit;
        end;
        if not DlgFileExist(Format(rsMsgFileExistsRwrt,[sDst+fr^.sPath+sDstNew, fr^.sName])) then
          Exit;
      end;   
    end;
    Result:=CopyFile(fr^.sName, sDst+fr^.sPath+sDstNew, FAppend);

    if Result then
      // write log success
      if (log_cp_mv_ln in gLogOptions) and (log_success in gLogOptions) then
        logWrite(Self, Format(rsMsgLogSuccess+rsMsgLogCopy, [fr^.sName+' -> '+sDst+fr^.sPath+sDstNew]), lmtSuccess)
    else
      // write log error
      if (log_cp_mv_ln in gLogOptions) and (log_errors in gLogOptions) then
        logWrite(Self, Format(rsMsgLogError+rsMsgLogCopy, [fr^.sName+' -> '+sDst+fr^.sPath+sDstNew]), lmtError);
  end; // files and other stuff
end;

Function TCopyThread.CopyFile(const sSrc, sDst:String; bAppend:Boolean):Boolean;
var
  src, dst:TFileStreamEx;
//  bAppend:Boolean;
  iDstBeg:Int64; // in the append mode we store original size
  Buffer:PChar;
  iTotalDiskSize,
  iFreeDiskSize : Int64;
  bRetry : Boolean;
begin
  Result:=False;

  GetMem(Buffer, gCopyBlockSize+1);
  dst:=nil; // for safety exception handling
  try
    try
      src:=TFileStreamEx.Create(sSrc,fmOpenRead or fmShareDenyNone);
      DebugLn(sDst);
      if bAppend then
      begin
        dst:=TFileStreamEx.Create(sDst,fmOpenReadWrite);
        dst.Seek(0,soFromEnd); // seek to end
      end
      else

         dst:=TFileStreamEx.Create(sDst,fmCreate);
      iDstBeg:=dst.Size;
      // we dont't use CopyFrom, because it's alocate and free buffer every time is called
      FFileOpDlg.iProgress1Pos:=0;
      FFileOpDlg.iProgress1Max:=src.Size;
      DebugLn('SrcSize:',IntToStr(src.Size));
//      writeln(FFileOpDlg.iProgress1Max);
      Synchronize(@FFileOpDlg.UpdateDlg);

      while (dst.Size+gCopyBlockSize)<= (src.Size+iDstBeg) do
      begin
        if Terminated then
          Exit;
        Src.ReadBuffer(Buffer^, gCopyBlockSize);

        repeat
          try
            bRetry := False;
            dst.WriteBuffer(Buffer^, gCopyBlockSize);
          except
            on EWriteError do
              begin
                {Check disk free space}
                GetDiskFreeSpace(sDstPath, iFreeDiskSize, iTotalDiskSize);
                if gCopyBlockSize > iFreeDiskSize then
                  case MsgBox(Self, rsMsgNoFreeSpaceRetry, [msmbYes, msmbNo,msmbSkip], msmbYes, msmbNo) of
                    mmrYes:
                      bRetry := True;
                    mmrNo:
                      Terminate;
                    mmrSkip:
                      Exit;
                  end; // case
              end; // on do
          end; // except
        until not bRetry;
        
        FFileOpDlg.iProgress1Pos:=dst.Size;
        Synchronize(@FFileOpDlg.UpdateDlg);
      end;
      if (iDstBeg+src.Size)>dst.Size then
      begin
        src.ReadBuffer(Buffer^, src.Size+iDstBeg-dst.size);

        repeat
          try
            bRetry := False;
            dst.WriteBuffer(Buffer^, src.Size+iDstBeg-dst.size);
          except
            on EWriteError do
              begin
                {Check disk free space}
                GetDiskFreeSpace(sDstPath, iFreeDiskSize, iTotalDiskSize);
                if (src.Size+iDstBeg-dst.size) > iFreeDiskSize then
                  case MsgBox(Self, rsMsgNoFreeSpaceRetry, [msmbYes, msmbNo,msmbSkip], msmbYes, msmbNo) of
                    mmrYes:
                      bRetry := True;
                    mmrNo:
                      Terminate;
                    mmrSkip:
                      Exit;
                  end; // case
              end; // on do
          end; // except
        until not bRetry;
      end;
      FFileOpDlg.iProgress1Pos:=dst.Size;
      Synchronize(@FFileOpDlg.UpdateDlg);      
    finally
      DebugLn('finally');
      if assigned(src) then
        FreeAndNil(src);
      if assigned(dst) then
        FreeAndNil(dst);
      if assigned(Buffer) then
        FreeMem(Buffer);
    end;
  Result := FileCopyAttr(sSrc, sDst, bDropReadOnlyFlag); // chmod, chgrp, udate a spol	
  except
    on EFCreateError do
       msgError(Self, '!!!!EFCreateError');
    on EFOpenError do
      msgError(Self, '!!!!EFOpenError');
    on EWriteError do
      msgError(Self, '!!!!EFWriteError');
  end;
end;

Function TCopyThread.GetCaptionLng:String;
begin
  Result:=rsDlgCp;
end;

end.
