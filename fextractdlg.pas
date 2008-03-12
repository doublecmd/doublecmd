{
   Double Commander
   -------------------------------------------------------------------------
   File unpacking window

   Copyright (C) 2007  Koblov Alexander (Alexx2000@mail.ru)

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

unit fExtractDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, uVFS, uFileList, framePanel,
  EditBtn;

type

{ TfrmExtractDlg }

TfrmExtractDlg = class(TForm)
  edtExtractTo: TDirectoryEdit;
    lblExtractTo : TLabel;
    lblFileMask : TLabel;
    cbFileMask : TComboBox;
    cbExtractPath : TCheckBox;
    cbOverwrite : TCheckBox;
    cbInSeparateFolder : TCheckBox;
    btnOK : TButton;
    btnCancel : TButton;
    btnHelp : TButton;
  private
    { private declarations }
  public
    { public declarations }
  end; 

function  ShowExtractDlg(ActiveFrame:TFrameFilePanel; var fl : TFileList; sDestPath:String): Boolean;

implementation
uses
  uTypes, uDCUtils;
  
var
  CurrentVFS : TVFS;

function ShowExtractDlg(ActiveFrame:TFrameFilePanel; var fl: TFileList; sDestPath: String): Boolean;
var
  I : Integer;
  ExtractFileList : TFileList;
begin
  with TfrmExtractDlg.Create(nil) do
    begin
      edtExtractTo.Text := sDestPath;
      CurrentVFS := ActiveFrame.pnlFile.VFS;

      if ActiveFrame.pnlFile.PanelMode = pmArchive then
        cbInSeparateFolder.Visible := False;

      Result:= (ShowModal = mrOK);
      if Result then
        begin
          sDestPath := IncludeTrailingPathDelimiter(edtExtractTo.Text) + cbFileMask.Text;
          ExtractFileList := TFileList.Create;
          ExtractFileList.CurrentDirectory := PathDelim;

          // if in archive
          if ActiveFrame.pnlFile.PanelMode = pmArchive then
            begin
              if CurrentVFS.FindModule(CurrentVFS.ArcFullName) then
                CurrentVFS.VFSmodule.VFSCopyOutEx(fl, sDestPath, 0);
            end;

          // if in real directory
          if ActiveFrame.pnlFile.PanelMode = pmDirectory then
            for I := 0 to fl.Count - 1 do // extract all selected archives
              if CurrentVFS.FindModule(fl.GetFileName(I)) then
                begin
                  ExtractFileList := TFileList.Create;
                  ExtractFileList.CurrentDirectory := PathDelim;
                  // if each archive in separate folder
                  if cbInSeparateFolder.Checked then
                    begin
                      sDestPath := IncludeTrailingPathDelimiter(edtExtractTo.Text);
                      sDestPath := sDestPath + ExtractOnlyFileName(CurrentVFS.ArcFullName) + PathDelim + cbFileMask.Text;
                    end;
                  // select all files and extract
                  CurrentVFS.VFSmodule.VFSList(PathDelim, ExtractFileList);
                  CurrentVFS.VFSmodule.VFSCopyOut(ExtractFileList, sDestPath, 0);
                end;
        end; // if Result
      Free;
    end;
end;

initialization
  {$I fextractdlg.lrs}

end.

