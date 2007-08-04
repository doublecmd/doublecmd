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
    rbOtherPlugins: TRadioButton;
    procedure btnConfigClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure arbChange(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end; 
procedure ShowPackFilesForm(VFS : TVFS; var fl : TFileList; sDestPath:String);

var
  arbRadioButtonArray : array [0..8] of TRadioButton;

implementation
uses
  uWCXhead;

var
  CurrentVFS : TVFS;

procedure ShowPackFilesForm(VFS : TVFS; var fl: TFileList; sDestPath:String);
var
  Flags : LongInt;
begin
  with TPackDlg.Create(nil) do
    begin
      (* if one file selected *)
      if fl.Count = 1 then
        begin
          edtPackCmd.Text := sDestPath + ExtractFileName(fl.GetFileName(0));
          edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, '.none');
        end
      else
      (* if some files selected *)
        begin
          edtPackCmd.Text := sDestPath + ExtractFileName(ExcludeTrailingPathDelimiter(fl.CurrentDirectory)) + '.none';
        end;
      CurrentVFS := VFS;
      if (ShowModal = mrOK) then
          if VFS.FindModule(edtPackCmd.Text) then
            begin
              Flags := 0;
              if cbMoveToArchive.Checked then Flags := Flags or PK_PACK_MOVE_FILES;
              if cbStoredir.Checked then Flags := Flags or PK_PACK_SAVE_PATHS;
              if cbEncrypt.Checked then Flags := Flags or PK_PACK_ENCRYPT;
              VFS.VFSmodule.VFSCopyIn(fl, '', Flags);
            end;
      Free;
    end;
end;

{ TPackDlg }

procedure TPackDlg.FormShow(Sender: TObject);
var
 I, J : Integer;
 sCurrentPlugin : String;
 iCurPlugCaps : Integer;
 Count : Integer;
begin
  J := 0;
  Count := 0;
  with CurrentVFS do
    begin
      for I:=0 to Plugins.Count -1 do
        begin
          sCurrentPlugin := Plugins.ValueFromIndex[i];
          iCurPlugCaps := StrToInt(Copy(sCurrentPlugin, 1, Pos(',',sCurrentPlugin) - 1));
          if (iCurPlugCaps and PK_CAPS_NEW) = PK_CAPS_NEW then
            begin
              (* First 9 plugins we display as  RadioButtons *)
              if J < 9 then
                begin
                  arbRadioButtonArray[J] := TRadioButton.Create(gbPacker);
                  arbRadioButtonArray[J].Parent := gbPacker;
                  arbRadioButtonArray[J].Left := 5 + 45 * (J div 3);
                  arbRadioButtonArray[J].Top := Count * (arbRadioButtonArray[J].Height + 4);
                  arbRadioButtonArray[J].Visible := True;
                  arbRadioButtonArray[J].Caption := Plugins.Names[I];
                  arbRadioButtonArray[J].OnChange := @arbChange;
                  J := J + 1;
                  Count := Count + 1;
                  if Count > 2 then
                    Count := 0;
                end
              else
                (* Other plugins we add in ComboBox *)
                begin
                  cbPackerList.Items.Add(Plugins.Names[I]);
                end;
            end;
        end; //for
        if arbRadioButtonArray[0] <> nil then
          arbRadioButtonArray[0].Checked := True;
        if cbPackerList.Items.Count > 0 then
          begin
            rbOtherPlugins.Enabled := True;
            cbPackerList.ItemIndex := 0;
          end;
    end;
end;

procedure TPackDlg.btnConfigClick(Sender: TObject);
begin
   if CurrentVFS.FindModule(edtPackCmd.Text) then
     CurrentVFS.VFSmodule.VFSConfigure(Handle);
end;

procedure TPackDlg.arbChange(Sender: TObject);
begin
  cbPackerList.Enabled := rbOtherPlugins.Checked;
  if rbOtherPlugins.Checked then
    edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, '.' + cbPackerList.Text)
  else
    edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, '.' + TRadioButton(Sender).Caption);
end;


initialization
  {$I fpackdlg.lrs}

end.

