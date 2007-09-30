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
  Buttons, uFileList, uVFS, EditBtn, ExtCtrls;

type

  { TPackDlg }

  TPackDlg = class(TForm)
    btnConfig: TButton;
    btnHelp: TButton;
    btnCancel: TButton;
    cbCreateSeparateArchives: TCheckBox;
    cbCreateSFX: TCheckBox;
    cbEncrypt: TCheckBox;
    cbMoveToArchive: TCheckBox;
    cbMultivolume: TCheckBox;
    btnOk: TButton;
    cbPackerList: TComboBox;
    cbOtherPlugins: TCheckBox;
    edtPackCmd: TDirectoryEdit;
    lblPrompt: TLabel;
    cbRecurse: TCheckBox;
    cbStoredir: TCheckBox;
    rgPacker: TRadioGroup;
    procedure btnConfigClick(Sender: TObject);
    procedure cbOtherPluginsChange(Sender: TObject);
    procedure edtPackCmdAcceptDirectory(Sender: TObject; var Value: String);
    procedure FormShow(Sender: TObject);
    procedure arbChange(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end; 
procedure ShowPackFilesForm(VFS : TVFS; var fl : TFileList; sDestPath:String);

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
      for I:=0 to WCXPlugins.Count - 1 do
        begin
        if Pos('#', WCXPlugins.Names[I]) <> 0 then Continue;
          sCurrentPlugin := WCXPlugins.ValueFromIndex[i];
          iCurPlugCaps := StrToInt(Copy(sCurrentPlugin, 1, Pos(',',sCurrentPlugin) - 1));
          if (iCurPlugCaps and PK_CAPS_NEW) = PK_CAPS_NEW then
            begin
              (* First 9 plugins we display as  RadioButtons *)
              if J < 9 then
                begin
                  rgPacker.Items.Add(WCXPlugins.Names[I]);
                  J := J + 1;
                end
              else
                (* Other plugins we add in ComboBox *)
                begin
                  cbPackerList.Items.Add(WCXPlugins.Names[I]);
                end;
            end;
        end; //for
        if rgPacker.Items.Count > 0 then
          rgPacker.ItemIndex := 0;
        if cbPackerList.Items.Count > 0 then
          begin
            cbOtherPlugins.Visible := True;
            cbPackerList.Visible := True;
            cbOtherPlugins.Enabled := True;
            cbOtherPlugins.Enabled := True;
            cbPackerList.ItemIndex := 0;
          end
        else
          btnConfig.AnchorToCompanion(akTop, 6, rgPacker);
    end;
end;

procedure TPackDlg.btnConfigClick(Sender: TObject);
begin
   if CurrentVFS.FindModule(edtPackCmd.Text) then
     CurrentVFS.VFSmodule.VFSConfigure(Handle);
end;

procedure TPackDlg.cbOtherPluginsChange(Sender: TObject);
begin
  if cbOtherPlugins.Checked then
    begin
      edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, '.' + cbPackerList.Text);
      rgPacker.ItemIndex := -1;
    end
  else
    begin
      if rgPacker.ItemIndex = -1 then
        rgPacker.ItemIndex := 0;
    end;
  cbPackerList.Enabled := cbOtherPlugins.Checked;
end;

procedure TPackDlg.edtPackCmdAcceptDirectory(Sender: TObject; var Value: String
  );
begin
  Value := IncludeTrailingPathDelimiter(Value) + ExtractFileName(edtPackCmd.Text);
end;

procedure TPackDlg.arbChange(Sender: TObject);
begin
  if rgPacker.ItemIndex >= 0 then
    begin
      edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, '.' + rgPacker.Items[rgPacker.ItemIndex]);
      cbOtherPlugins.Checked := False;
    end;
end;


initialization
  {$I fpackdlg.lrs}

end.

