{
   Seksi Commander
   ----------------------------
   Implementing of Move files thread

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:

   Copyright (C) 2007-2008  Koblov Alexander (Alexx2000@mail.ru)
}

unit uMoveThread;

{$mode objfpc}{$H+}

interface
uses
   uTypes, uCopyThread;
type
  TMoveThread = class(TCopyThread)
  protected
    procedure MainExecute; override;
    function GetCaptionLng:String; override;
  end;

implementation
uses
  uFileProcs, SysUtils, uShowMsg, uLng, uGlobs, uLog, uDCUtils, uOSUtils;


procedure TMoveThread.MainExecute;
var
  pr:PFileRecItem;
  xIndex:Integer;
  iCoped:Int64;
  sDstExt:String;
  sDstName:String;
  sDstNew:String;
  iTotalDiskSize,
  iFreeDiskSize : Int64;
begin
  CorrectMask;
  FReplaceAll:=False;
  FSkipAll:=False;

// we first create dir structure
  for xIndex:=0 to NewFileList.Count-1 do // copy
  begin
    pr:=NewFileList.GetItem(xIndex);
    if FPS_ISDIR(pr^.iMode) then
    begin
      if not mbDirectoryExists(sDstPath+pr^.sPath+ pr^.sNameNoExt) then
        ForceDirectory(sDstPath+pr^.sPath+pr^.sNameNoExt);
//      writeln('move:mkdir:',sDstPath+pr^.sNameNoExt);
    end;
  end;
  iCoped:=0;
  for xIndex:=NewFileList.Count-1 downto 0 do // copy and delete
  begin
    pr:=NewFileList.GetItem(xIndex);

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

    if FPS_ISDIR(pr^.iMode) then
    begin
      RmDir(pr^.sName);
    end
    else
    begin
      inc(iCoped,pr^.iSize);
      // change dst name by mask
      DivFileName(pr^.sNameNoExt,sDstName, sDstExt);
      sDstName:=CorrectDstName(sDstName);
      sDstExt:=CorrectDstExt(sDstExt);
      sDstNew:='';
      if sDstName<>'' then
        sDstNew:=sDstName;
      if sDstExt<>'.' then
        sDstNew:=sDstNew+sDstExt;
      FFileOpDlg.sFileName:=ExtractFileName(pr^.sName)+' -> '+pr^.sPath+sDstNew;
      Synchronize(@FFileOpDlg.UpdateDlg);
//  test if exists and show dialog
      FAppend:=False;
      if mbFileExists(sDstPath+pr^.sPath+sDstNew) and not FReplaceAll then
      begin
        if FSkipAll then
          Exit;
        if not DlgFileExist(Format(rsMsgFileExistsRwrt,[sDstPath+pr^.sPath+sDstNew, pr^.sName])) then
          Continue;
      end;

      if FAppend or not mbRenameFile(pr^.sName, sDstPath+pr^.sPath+ sDstNew) then
      begin
        // rename failed, maybe not the same filesystem (or we want append)
        // OK, copy standard way and delete src file
        if cpFile(pr, sDstPath, False) then // False >> not show confirmation dialog
          begin
            if mbDeleteFile(pr^.sName) then
              if (log_delete in gLogOptions) and (log_success in gLogOptions) then
                logWrite(Self, Format(rsMsgLogSuccess+rsMsgLogDelete, [pr^.sName]), lmtSuccess)
            else
              if (log_delete in gLogOptions) and (log_errors in gLogOptions) then
                logWrite(Self, Format(rsMsgLogError+rsMsgLogDelete, [pr^.sName]), lmtError);
          end; // cpFile
      end
      else
        begin // rename succes
          // process comments if need
          if gProcessComments and Assigned(FDescr) then
            FDescr.MoveDescription(pr^.sName, sDstPath+pr^.sPath+ sDstNew);
          // write log
          if (log_cp_mv_ln in gLogOptions) and (log_success in gLogOptions) then
            logWrite(Self, Format(rsMsgLogSuccess+rsMsgLogMove, [pr^.sName+' -> '+sDstPath+pr^.sPath+ sDstNew]), lmtSuccess)
        end;
    end;
    FFileOpDlg.iProgress2Pos:=iCoped;
    Synchronize(@FFileOpDlg.UpdateDlg);
  end;
end;

function TMoveThread.GetCaptionLng:String;
begin
  Result:=rsDlgMv;
end;

end.
