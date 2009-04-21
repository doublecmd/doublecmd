{
   Double Commander
   -------------------------------------------------------------------------
   Packed file information window

   Copyright (C) 2008-2009  Koblov Alexander (Alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

}

unit fPackInfoDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, uWCXModule, uOSUtils;

type

  { TfrmPackInfoDlg }

  TfrmPackInfoDlg = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnClose: TButton;
    btnUnpackAllAndExec: TButton;
    btnUnpackAndExec: TButton;
    lblAttributes: TLabel;
    lblCompressionRatio: TLabel;
    lblDate: TLabel;
    lblMethod: TLabel;
    lblOriginalSize: TLabel;
    lblPackedFile: TLabel;
    lblPackedSize: TLabel;
    lblPacker: TLabel;
    lblTime: TLabel;
    nbProperties: TNotebook;
    lblPackedAttr: TLabel;
    lblPackedCompression: TLabel;
    lblPackedDate: TLabel;
    edtPackedFile: TEdit;
    lblPackedMethod: TLabel;
    lblPackedOrgSize: TLabel;
    lblPackedPackedSize: TLabel;
    lblPackedPacker: TLabel;
    lblPackedTime: TLabel;
    pgGeneral: TPage;
    procedure btnUnpackAllAndExecClick(Sender: TObject);
    procedure btnUnpackAndExecClick(Sender: TObject);
  private
    fWCXModule: TWCXModule;
  public
    { public declarations }
  end; 

function ShowPackInfoDlg(WCXModule: TWCXModule; HeaderData: TWCXHeader): Boolean;

implementation
uses
  LCLProc, FileUtil,  uTypes, uFileOp, uFileList, uDCUtils, uShellExecute;

function ShowPackInfoDlg(WCXModule: TWCXModule; HeaderData: TWCXHeader): Boolean;
var
  dtDateTime: TDateTime;
  sArcType: String;
begin
  with TfrmPackInfoDlg.Create(Application) do
  begin
    // save current VFS module
    fWCXModule:= WCXModule;

    edtPackedFile.Text:= HeaderData.FileName;
    sArcType:= ExtractFileExt(HeaderData.ArcName);
    Delete(sArcType, 1, 1);
    lblPackedPacker.Caption:= sArcType;

    if not FPS_ISDIR(HeaderData.FileAttr) then
    begin
      lblPackedOrgSize.Caption:=  IntToStr(HeaderData.UnpSize);
      lblPackedPackedSize.Caption:= IntToStr(HeaderData.PackSize);
      if HeaderData.UnpSize > 0 then
        lblPackedCompression.Caption:= IntToStr(100 - (HeaderData.PackSize*100 div HeaderData.UnpSize))+'%';
      lblPackedMethod.Caption:= IntToStr(HeaderData.Method);
    end;

    // DateTime and Attributes
    try
      dtDateTime:= FileDateToDateTime(HeaderData.FileTime);
    except
      dtDateTime:= 0;
    end;
    lblPackedDate.Caption:= DateToStr(dtDateTime);
    lblPackedTime.Caption:= TimeToStr(dtDateTime);
    lblPackedAttr.Caption:= AttrToStr(HeaderData.FileAttr);
    ShowModal;
    Free;
  end;
  Result := True;
end;

{ TfrmPackInfoDlg }

procedure TfrmPackInfoDlg.btnUnpackAndExecClick(Sender: TObject);
var
  ExtractFileList : TFileList;
  sDestPath: String;
  pfri: PFileRecItem;
begin
  Close;
  ExtractFileList:= TFileList.Create;
  ExtractFileList.CurrentDirectory:= PathDelim + ExtractFilePath(edtPackedFile.Text);
  sDestPath:= GetTempFolder;
  //DebugLn('sDestPath == ', sDestPath);
  New(pfri);
  pfri^.sName:= edtPackedFile.Text;
  pfri^.iMode:= 0;
  ExtractFileList.AddItem(pfri);
  Dispose(pfri);
  fWCXModule.VFSCopyOut(ExtractFileList, sDestPath + '*', 0);
  // execute file
  ShellExecuteEx('open', sDestPath+ExtractFileName(edtPackedFile.Text), sDestPath);
end;

procedure TfrmPackInfoDlg.btnUnpackAllAndExecClick(Sender: TObject);
var
  ExtractFileList : TFileList;
  sDestPath: String;
begin
  Close;
  ExtractFileList:= TFileList.Create;
  ExtractFileList.CurrentDirectory:= PathDelim;
  sDestPath:= GetTempFolder;
  //DebugLn('sDestPath == ', sDestPath);
  fWCXModule.VFSList(PathDelim, ExtractFileList);

  fWCXModule.VFSCopyOut(ExtractFileList, sDestPath + '*', 0);
  // execute file
  ShellExecuteEx('open', sDestPath+edtPackedFile.Text, sDestPath);
end;

initialization
  {$I fpackinfodlg.lrs}

end.

