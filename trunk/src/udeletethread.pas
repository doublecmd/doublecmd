{
   Seksi Commander
   ----------------------------
   Implementing of Delete thread

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:

   Copyright (C) 2006-2009  Koblov Alexander (Alexx2000@mail.ru)
}

unit uDeleteThread;

{$mode objfpc}{$H+}

interface

uses
  uFileOpThread, uFileList, uTypes, SysUtils, LCLProc;

type

  { TDeleteThread }

  TDeleteThread = class(TFileOpThread)
  private
   // 30.04.2009 - поле, показывающее удаление в корзину
    FRecycle : boolean;
  protected
    constructor Create(aFileList:TFileList); override;
    procedure MainExecute; override;
    function DeleteFile(fr:PFileRecItem):Boolean;
    function GetCaptionLng: String; override;
    function CheckFile(FileRecItem: PFileRecItem): Boolean; override;
 public
    // 30.04.2009 - свойство для удаления в корзину
    property Recycle : boolean read FRecycle write FRecycle default false;
  end;

implementation
uses
  uLng, uGlobs, uLog, uOSUtils;

constructor TDeleteThread.Create(aFileList: TFileList);
begin
  inherited Create(aFileList);
  FSymLinkAll:= True;
end;

procedure TDeleteThread.MainExecute;
var
  pr:PFileRecItem;
  xIndex:Integer;
  iCopied:Int64;
begin
  iCopied:=0;
  FFileOpDlg.iProgress1Max:= 1;
  FFileOpDlg.iProgress1Pos:= 1; // in delete use only 1 progress

  Synchronize(@FFileOpDlg.UpdateDlg);

  for xIndex:=NewFileList.Count-1 downto 0 do // deleting
  begin
    if Paused then Suspend;
    pr:=NewFileList.GetItem(xIndex);
    FFileOpDlg.sFileName:=pr^.sName;
    Synchronize(@FFileOpDlg.UpdateDlg);
    inc(iCopied,pr^.iSize);
    EstimateTime(iCopied);
    DeleteFile(pr);
    if FFilesSize <> 0 then
      FFileOpDlg.iProgress2Pos:= (iCopied * 100) div FFilesSize;
    Synchronize(@FFileOpDlg.UpdateDlg);
  end;
end;


function TDeleteThread.DeleteFile (fr:PFileRecItem):Boolean;
begin
  try
  If (FRecycle = false) {$IFNDEF MSWINDOWS} or True{$ENDIF} then
   begin
    if FPS_ISDIR(fr^.iMode) then // directory
      begin
        Result := mbRemoveDir(fr^.sName);
        // write log
        if Result then
          begin
            if (log_dir_op in gLogOptions) and (log_success in gLogOptions) then
              logWrite(Self, Format(rsMsgLogSuccess+rsMsgLogRmDir, [fr^.sName]), lmtSuccess);
          end
        else
          begin
            if (log_dir_op in gLogOptions) and (log_errors in gLogOptions) then
              logWrite(Self, Format(rsMsgLogError+rsMsgLogRmDir, [fr^.sName]), lmtError);
          end;
      end
    else
      begin // files and other stuff
        Result := mbDeleteFile(fr^.sName);
        // write log
        if Result then
          begin
            if (log_delete in gLogOptions) and (log_success in gLogOptions) then
              logWrite(Self, Format(rsMsgLogSuccess+rsMsgLogDelete, [fr^.sName]), lmtSuccess);
          end
        else
          begin
            if (log_delete in gLogOptions) and (log_errors in gLogOptions) then
              logWrite(Self, Format(rsMsgLogError+rsMsgLogDelete, [fr^.sName]), lmtError);
          end;
      end;
   end
  else
   begin // 30.04.2009 - Вызов удаления в корзину. Файлы и папки удаляются одной функцией.
    Result := mbDeleteToTrash(fr^.sName);
    // write log
    if Result then
     begin
      if (log_delete in gLogOptions) and (log_success in gLogOptions) then
        logWrite(Self, Format(rsMsgLogSuccess+rsMsgLogDelete, [fr^.sName]), lmtSuccess);
     end
    else
     begin
      if (log_delete in gLogOptions) and (log_errors in gLogOptions) then
        logWrite(Self, Format(rsMsgLogError+rsMsgLogDelete, [fr^.sName]), lmtError);
     end;
   end;
    // process comments if need
    if Result and gProcessComments and Assigned(FDescr) then
      FDescr.DeleteDescription(fr^.sName);
  except
    DebugLN('Can not delete ', fr^.sName);
  end;
end;

function TDeleteThread.GetCaptionLng:String;
begin
  Result:= rsDlgDel;
end;

function TDeleteThread.CheckFile(FileRecItem: PFileRecItem): Boolean;
begin
  Result:= inherited CheckFile(FileRecItem);
  if FileIsReadOnly(FileRecItem^.iMode) then
    mbFileSetReadOnly(FileRecItem^.sName, False);
end;

end.
