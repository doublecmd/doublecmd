{
Seksi Commander
----------------------------
Implementing of Move files thread

Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

contributors:

Alexander Koblov (Alexx2000@mail.ru)

}
unit uMoveThread;
{$mode objfpc}{$H+}
interface
uses
   uTypes, uCopyThread;
type
  TMoveThread=Class(TCopyThread)

  protected
//    Function CpFile (fr:FileRecPtr; const sDst:String):Boolean;
    procedure MainExecute; override;
    Function GetCaptionLng:String; override;
  end;

implementation
uses
  uFileProcs, SysUtils, uShowMsg, uLng, uDCUtils, uOSUtils;


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
      if not DirectoryExists(sDstPath+pr^.sPath+ pr^.sNameNoExt) then
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
        case MsgBoxForThread(Self, rsMsgNoFreeSpaceCont, [msmbYes, msmbNo,msmbSkip], msmbYes, msmbNo) of
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
      if FileExists(sDstPath+pr^.sPath+sDstNew) and not FReplaceAll then
      begin
        if FSkipAll then
          Exit;
        if not DlgFileExist(Format(rsMsgFileExistsRwrt,[sDstPath+pr^.sPath+sDstNew, pr^.sName])) then
          Continue;
      end;

      if FAppend or not RenameFile(pr^.sName, sDstPath+pr^.sPath+ sDstNew) then
      begin
      // rename failed, maybe not the same filesystem (or we want append)
      // OK, copy standard way and delete src file
        if cpFile(pr, sDstPath, False) then // False >> not show confirmation dialog
          sysutils.DeleteFile(pr^.sName);
      end;
    end;
    FFileOpDlg.iProgress2Pos:=iCoped;
    Synchronize(@FFileOpDlg.UpdateDlg);
  end;
end;

Function TMoveThread.GetCaptionLng:String;
begin
  Result:=rsDlgMv;
end;


{
Function TMoveThread.CpFile (fr:FileRecPtr; const sDst:String):Boolean;
begin
//  writeln(fr^.sName,'>',sDst+fr^.sName);
  if S_ISDIR(fr.fMode) then
  begin
    writeln('Error: mkdir:',sDst+fr^.sNameNoExt);
    Result:=True;
   end
  else
  begin // directory and other stuff
    Result:=CopyFile(fr^.sName, sDst+fr^.sNameNoExt,False);
    writeln('file:',fr^.sName,'>',sDst+fr^.sNameNoExt);
  end;
end;}
end.
