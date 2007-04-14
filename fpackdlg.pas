{
   Double Commander
   -------------------------------------------------------------------------
   File packing window

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

unit fPackDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, uFileList, uVFS;

type

  { TPackDlg }

  TPackDlg = class(TForm)
    btnHelp: TButton;
    btnCancel: TButton;
    btnConfig: TButton;
    cbCreateSeparateArchives: TCheckBox;
    cbCreateSFX: TCheckBox;
    cbEncrypt: TCheckBox;
    btnTree: TButton;
    cbMoveToArchive: TCheckBox;
    cbMultivolume: TCheckBox;
    btnOk: TButton;
    gbPacker: TGroupBox;
    cbPackerList: TComboBox;
    lblPrompt: TLabel;
    cbRecurse: TCheckBox;
    cbStoredir: TCheckBox;
    edtPackCmd: TEdit;
    rb_6: TRadioButton;
    rb_2: TRadioButton;
    rb_8: TRadioButton;
    rb_3: TRadioButton;
    rbOtherPlugins: TRadioButton;
    rb_4: TRadioButton;
    rb_7: TRadioButton;
    rb_9: TRadioButton;
    rb_5: TRadioButton;
    rb_1: TRadioButton;

  private
    { private declarations }
  public
    { public declarations }
  end; 
procedure ShowPackFilesForm(var VFS : TVFS; var fl : TFileList; sDestPath:String);

implementation


procedure ShowPackFilesForm(var VFS : TVFS; var fl: TFileList; sDestPath:String);
begin
  with TPackDlg.Create(nil) do
    begin
      (* if one file selected *)
      if fl.Count = 1 then
        begin
          edtPackCmd.Text := sDestPath + ExtractFileName(fl.GetFileName(0));
          edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, '.7z');
        end
      else
      (* if some files selected *)
        begin
          edtPackCmd.Text := sDestPath + ExtractFileName(ExcludeTrailingPathDelimiter(fl.CurrentDirectory)) + '.7z';
        end;
      if (ShowModal = mrOK) then
          if VFS.FindModule(edtPackCmd.Text) then
            begin
              VFS.VFSmodule.VFSCopyIn(fl, '', 2);
            end;
      Free;
    end;
end;

initialization
  {$I fpackdlg.lrs}

end.

