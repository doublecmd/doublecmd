{
Seksi Commander
----------------------------
Implementing of Delete thread

Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

contributors:

}
unit uDeleteThread;
{$mode objfpc}{$H+}
interface
uses
  uFileOpThread, uFileList, uTypes, SysUtils;
type

  { TDeleteThread }

  TDeleteThread=Class(TFileOpThread)

  protected
    constructor Create(aFileList:TFileList);override;
    procedure MainExecute; override;
    Function DeleteFile(fr:PFileRecItem):Boolean;
    Function GetCaptionLng:String;override;
  end;

implementation
uses
  uLng, uOSUtils;

constructor TDeleteThread.Create(aFileList: TFileList);
begin
  inherited Create(aFileList);
  FSymLinkAll := True;
end;

procedure TDeleteThread.MainExecute;
var
  pr:PFileRecItem;
  xIndex:Integer;
  iCoped:Int64;
begin
  iCoped:=0;
  FFileOpDlg.iProgress1Max:=1;
  FFileOpDlg.iProgress1Pos:=1; // in delete use only 1 progress

  Synchronize(@FFileOpDlg.UpdateDlg);

  for xIndex:=NewFileList.Count-1 downto 0 do // deleting
  begin
    pr:=NewFileList.GetItem(xIndex);
    FFileOpDlg.sFileName:=pr^.sName;
    Synchronize(@FFileOpDlg.UpdateDlg);
    inc(iCoped,pr^.iSize);
    EstimateTime(iCoped);
    DeleteFile(pr);
    FFileOpDlg.iProgress2Pos:=iCoped;
    Synchronize(@FFileOpDlg.UpdateDlg);
  end;
end;


Function TDeleteThread.DeleteFile (fr:PFileRecItem):Boolean;
begin
//  writeln(fr^.sName,'>',sDst+fr^.sName);
  if FPS_ISDIR(fr^.iMode) then
   begin
//     writeln('rmdir:',fr^.sName);
     RmDir(fr^.sName);      // not complete (link...)
     Result:=True;
   end
  else
  begin // files and other stuff
    Result:= sysutils.DeleteFile(fr^.sName);
//    writeln('del file not implemented:',fr^.sName);
  end;
end;

Function TDeleteThread.GetCaptionLng:String;
begin
  Result:=lngGetString(clngDlgDel);
end;

end.
