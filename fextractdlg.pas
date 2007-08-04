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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, uVFS, uFileList;

type

{ TExtractDlg }

TExtractDlg = class(TForm)
    lblExtractTo : TLabel;
    edtExtractTo : TEdit;
    lblFileMask : TLabel;
    cbFileMask : TComboBox;
    cbExtractPath : TCheckBox;
    cbOverwrite : TCheckBox;
    cbInSeparateFolder : TCheckBox;
    btnOK : TButton;
    btnTree : TButton;
    btnCancel : TButton;
    btnHelp : TButton;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ExtractDlg: TExtractDlg;

procedure  ShowExtractDlg(VFS : TVFS; var fl : TFileList; sDestPath:String);

implementation

var
  CurrentVFS : TVFS;

procedure ShowExtractDlg(VFS : TVFS; var fl: TFileList; sDestPath: String);
var
  I : Integer;
  ExtractFileList : TFileList;
begin
  with TExtractDlg.Create(nil) do
    begin
      edtExtractTo.Text := sDestPath;
      CurrentVFS := VFS;
      if (ShowModal = mrOK) then
        begin
          sDestPath := IncludeTrailingPathDelimiter(edtExtractTo.Text) + cbFileMask.Text;
          ExtractFileList := TFileList.Create;
          ExtractFileList.CurrentDirectory := PathDelim;
          for I := 0 to fl.Count - 1 do
          if VFS.FindModule(fl.GetFileName(I)) then
            begin
              VFS.VFSmodule.VFSList(PathDelim, ExtractFileList); // select all files
              VFS.VFSmodule.VFSCopyOutEx(ExtractFileList, sDestPath, 0);
            end;
        end;
      Free;
    end;
end;

initialization
  {$I fextractdlg.lrs}

end.

