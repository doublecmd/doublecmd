{
   Seksi Commander
   ----------------------------
   Implementing of Copy files Thread

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:

   Copyright (C) 2007-2009  Koblov Alexander (Alexx2000@mail.ru)
}

unit uCopyThread;

{$mode objfpc}{$H+}

interface
uses
  uFileOpThread, uTypes;
type
  TCopyThread = class(TFileOpThread)
  private
  protected
    FCopied: Int64;

    function CpFile(fr: PFileRecItem; const sDst: String; bShowDlg: Boolean): Boolean;
    function CopyFile(const sSrc, sDst: String; bAppend: Boolean): Boolean;
    procedure MainExecute; override;
    function GetCaptionLng:String; override;
  end;

implementation
uses
  LCLProc, SysUtils, Classes, StrUtils, FileUtil, uLng, uGlobs, uLog, uShowMsg, uFileProcs, uFindEx,
  uDCUtils, uOSUtils, uClassesEx, uDescr;

procedure TCopyThread.MainExecute;
var
  pr:PFileRecItem;
  xIndex:Integer;
  iTotalDiskSize,
  iFreeDiskSize: Int64;
  bProceed: Boolean;

  bSkipFreeSpaceCheck : Boolean;
  bSkipAllBigFiles : Boolean;

  mmrResult : TMyMsgResult;
begin
  CorrectMask;
  FReplaceAll:= False;
  FSkipAll:= False;
  FCopied:= 0;

  bSkipFreeSpaceCheck:= False;
  bSkipAllBigFiles:= False;

  for xIndex:=0 to NewFileList.Count-1 do // copy
  begin
    if Terminated then
       Exit;

    bProceed := True;

    pr:= NewFileList.GetItem(xIndex);

    EstimateTime(FCopied);

    { Check disk free space }
    if not bSkipFreeSpaceCheck then
    begin
      GetDiskFreeSpace(sDstPath, iFreeDiskSize, iTotalDiskSize);
      if pr^.iSize > iFreeDiskSize then
      begin
        if bSkipAllBigFiles then
        begin
          bProceed:= False;
        end
          else
        begin
          mmrResult := MsgBox(Self, rsMsgNoFreeSpaceCont, [msmbYes, msmbAll, msmbNo, msmbSkip, msmbSkipAll], msmbYes, msmbNo);
          case mmrResult of
            mmrNo:
              Exit;

            mmrSkip:
              bProceed:= False;

            mmrAll:
              bSkipFreeSpaceCheck:= True;

            mmrSkipAll:
            begin
              bProceed:= False;
              bSkipAllBigFiles:= True;
            end;

          end;
        end;
      end;
    end;

    if bProceed then
      CpFile(pr,sDstPath, True);

    if not FPS_ISDIR(pr^.iMode) then
    begin
      inc(FCopied,pr^.iSize);

      if FFilesSize <> 0 then
        FFileOpDlg.iProgress2Pos:= (FCopied * 100) div FFilesSize;
      Synchronize(@FFileOpDlg.UpdateDlg);
    end;
  end;
end;

// bShowDlg is only for Rename
function TCopyThread.CpFile(fr: PFileRecItem; const sDst: String; bShowDlg: Boolean): Boolean;
var
  sDstExt: String;
  sDstName: String;
  sDstNew: String;
  bIsFolder,
  bIsSymLink: Boolean;
  iAttr: LongInt;
  sMsg: String;
begin
//  DebugLn(fr^.sName);
//  DebugLn('NameNoExt ==' +fr^.sNameNoExt);

  DivFileName(fr^.sNameNoExt,sDstName, sDstExt);
  sDstName:= CorrectDstName(sDstName);
//  DebugLn('sDstName ==' + sDstName);
  sDstExt:= CorrectDstExt(sDstExt);
  sDstNew:= '';
  if sDstName<>'' then
    sDstNew:= sDstName;
  if sDstExt<>'.' then
    sDstNew:= sDstNew+sDstExt;
//  DebugLn(sDstNew);
  if CompareFilenames(fr^.sName, sDst+fr^.sPath+sDstNew) = 0 then Exit(False);
  FFileOpDlg.sFileNameFrom:= fr^.sName;
  FFileOpDlg.sFileNameTo:= sDst + fr^.sPath + sDstNew;
  Synchronize(@FFileOpDlg.UpdateDlg);
  if FPS_ISLNK(fr^.iMode) then
    begin
      // use sDstName as link target
      sDstName:= ReadSymLink(fr^.sName);
      if sDstName <> '' then
        begin
          sDstName:= GetAbsoluteFileName(ExtractFilePath(fr^.sName), sDstName);
//          DebugLn('ReadSymLink := ' + sDstName);

          if mbFileExists(sDst+fr^.sPath+sDstNew) or mbDirectoryExists(sDst+fr^.sPath+sDstNew) then
            begin
              iAttr:= mbFileGetAttr(sDst+fr^.sPath+sDstNew);
              bIsFolder:= FPS_ISDIR(iAttr);
              bIsSymLink:= FPS_ISLNK(iAttr);
              if not FReplaceAll then
                begin
                  if FSkipAll then Exit(True);
                  sMsg:= IfThen(bIsFolder and not bIsSymLink, rsMsgFolderExistsRwrt, rsMsgFileExistsRwrt);
                  if not DlgFileExist(Format(sMsg, [sDst+fr^.sPath+sDstNew, fr^.sName])) then
                    Exit(False);
                end; // replace all
              if bIsFolder and bIsSymLink then // symlink to folder
                mbRemoveDir(sDst+fr^.sPath+sDstNew)
              else if bIsFolder then // folder
                DelTree(sDst+fr^.sPath+sDstNew)
              else // file
                mbDeleteFile(sDst+fr^.sPath+sDstNew);
            end; // mbFileExists

          if not CreateSymlink(sDstName, sDst+fr^.sPath+sDstNew) then
            DebugLn('Symlink error:');
        end
      else
        DebugLn('Error reading link');
      Result:= True;
    end
  else if FPS_ISDIR(fr^.iMode) then
    begin
      DebugLn('Force = ' + sDst+fr^.sPath+fr^.sNameNoExt);
      if not mbDirectoryExists(sDst+fr^.sPath+fr^.sNameNoExt) then
        uFileProcs.ForceDirectory(sDst+fr^.sPath+fr^.sNameNoExt);
      Result:= True;
    end
  else
    begin // files and other stuff
//    DebugLn('fr^.sPath:'+fr^.sPath);
      if bShowDlg then
        begin
          Result:= False;
//      DebugLn('testing:'+sDst+fr^.sPath+sDstNew);
          if mbFileExists(sDst+fr^.sPath+sDstNew) then
            begin
              if not FReplaceAll then
                begin
                  if FSkipAll then Exit(True);
                  if not DlgFileExist(Format(rsMsgFileExistsRwrt,[sDst+fr^.sPath+sDstNew, fr^.sName])) then
                    Exit;
                end; // replace all
              if FPS_ISLNK(mbFileGetAttr(sDst+fr^.sPath+sDstNew)) then
                begin
                  mbDeleteFile(sDst+fr^.sPath+sDstNew);
                  FAppend:= False; // we can not append to symlink
                end;
            end; // mbFileExists
        end; // bShowDlg

      Result:= CopyFile(fr^.sName, sDst+fr^.sPath+sDstNew, FAppend);

      // process comments if need
      if Result and gProcessComments and Assigned(FDescr) then
        FDescr.CopyDescription(fr^.sName, sDst+fr^.sPath+sDstNew);

      if Result then
        begin
          // write log success
          if (log_cp_mv_ln in gLogOptions) and (log_success in gLogOptions) then
            logWrite(Self, Format(rsMsgLogSuccess+rsMsgLogCopy, [fr^.sName+' -> '+sDst+fr^.sPath+sDstNew]), lmtSuccess);
        end
      else
        begin
          // write log error
          if (log_cp_mv_ln in gLogOptions) and (log_errors in gLogOptions) then
            logWrite(Self, Format(rsMsgLogError+rsMsgLogCopy, [fr^.sName+' -> '+sDst+fr^.sPath+sDstNew]), lmtError);
        end;
    end; // files and other stuff
end;

function TCopyThread.CopyFile(const sSrc, sDst: String; bAppend: Boolean): Boolean;
var
  src, dst: TFileStreamEx;
//  bAppend:Boolean;
  iDstSize: Int64; // in the append mode we store new destination size
  Buffer: PChar;
  iTotalDiskSize,
  iFreeDiskSize: Int64;
  bRetry: Boolean;
  iCopyBlockSize: Integer;
begin
  Result:= False;
  iCopyBlockSize:= gCopyBlockSize;
  GetMem(Buffer, iCopyBlockSize+1);
  src:= nil;
  dst:= nil; // for safety exception handling
  try
    try
      src:= TFileStreamEx.Create(sSrc,fmOpenRead or fmShareDenyNone);
      DebugLn(sDst);
      if bAppend then
        begin
          dst:= TFileStreamEx.Create(sDst,fmOpenReadWrite);
          dst.Seek(0,soFromEnd); // seek to end
        end
      else
         dst:= TFileStreamEx.Create(sDst,fmCreate);

      iDstSize:= src.Size + dst.Size;
      // we dont't use CopyFrom, because it's alocate and free buffer every time is called
      FFileOpDlg.iProgress1Pos:= 0;
      FFileOpDlg.iProgress1Max:= 100;
      DebugLn('SrcSize: ',IntToStr(src.Size));
      Synchronize(@FFileOpDlg.UpdateDlg);

      while (dst.Size+iCopyBlockSize) <= iDstSize do
      begin
        if Terminated then
          begin
            if dst.Size < iDstSize then
              begin
                FreeAndNil(dst);
                mbDeleteFile(sDst);
              end;
            Exit;
          end;
        if Paused then Suspend;
        src.ReadBuffer(Buffer^, iCopyBlockSize);
        repeat
          try
            bRetry:= False;
            dst.WriteBuffer(Buffer^, iCopyBlockSize);
          except
            on EWriteError do
              begin
                { Check disk free space }
                GetDiskFreeSpace(sDstPath, iFreeDiskSize, iTotalDiskSize);
                if iCopyBlockSize > iFreeDiskSize then
                  case MsgBox(Self, rsMsgNoFreeSpaceRetry, [msmbYes, msmbNo,msmbSkip], msmbYes, msmbNo) of
                    mmrYes:
                      bRetry:= True;
                    mmrNo:
                      Terminate;
                    mmrSkip:
                      Exit;
                  end; // case
              end; // on do
          end; // except
        until not bRetry;
        
        if (iDstSize <> 0) and (FFilesSize <> 0) then
          begin
            FFileOpDlg.iProgress1Pos:= (dst.Size * 100) div iDstSize;
            FFileOpDlg.iProgress2Pos:= ((FCopied + dst.Size) * 100) div FFilesSize;
            EstimateTime(FCopied + dst.Size);
            Synchronize(@FFileOpDlg.UpdateDlg);
          end;
      end;
      if iDstSize > dst.Size then
      begin
        if Paused then Suspend;
        src.ReadBuffer(Buffer^, iDstSize-dst.size);
        repeat
          try
            bRetry:= False;
            dst.WriteBuffer(Buffer^, iDstSize-dst.size);
          except
            on EWriteError do
              begin
                { Check disk free space }
                GetDiskFreeSpace(sDstPath, iFreeDiskSize, iTotalDiskSize);
                if (iDstSize-dst.size) > iFreeDiskSize then
                  case MsgBox(Self, rsMsgNoFreeSpaceRetry, [msmbYes, msmbNo,msmbSkip], msmbYes, msmbNo) of
                    mmrYes:
                      bRetry:= True;
                    mmrNo:
                      Terminate;
                    mmrSkip:
                      Exit;
                  end; // case
              end; // on do
          end; // except
        until not bRetry;
      end;
      if iDstSize <> 0 then
        FFileOpDlg.iProgress1Pos:= (dst.Size * 100) div iDstSize;
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
  // copy file attributes
  Result:= FileCopyAttr(sSrc, sDst, bDropReadOnlyFlag);
  except
    on EFCreateError do
      begin
        if gSkipFileOpError then
          logWrite(Self, rsMsgErrECreate + ' - ' + sDst, lmtError, True)
        else
          if MsgBox(Self, rsMsgErrECreate + ' - ' + sDst, [msmbSkip, msmbCancel], msmbSkip, msmbCancel) = mmrCancel then
            Terminate;
      end;
    on EFOpenError do
      begin
        if gSkipFileOpError then
          logWrite(Self, rsMsgErrEOpen + ' - ' + sSrc, lmtError, True)
        else
          if MsgBox(Self, rsMsgErrEOpen + ' - ' + sSrc, [msmbSkip, msmbCancel], msmbSkip, msmbCancel) = mmrCancel then
            Terminate;
      end;
    on EWriteError do
      begin
        if gSkipFileOpError then
          logWrite(Self, rsMsgErrEWrite + ' - ' + sDst, lmtError, True)
        else
          if MsgBox(Self, rsMsgErrEWrite + ' - ' + sDst, [msmbSkip, msmbCancel], msmbSkip, msmbCancel) = mmrCancel then
            Terminate;
      end;
  end;
end;

function TCopyThread.GetCaptionLng:String;
begin
  Result:= rsDlgCp;
end;

end.
