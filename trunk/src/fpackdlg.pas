{
   Double Commander
   -------------------------------------------------------------------------
   File packing window

   Copyright (C) 2007-2008  Koblov Alexander (Alexx2000@mail.ru)

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

  { TfrmPackDlg }

  TfrmPackDlg = class(TForm)
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
    pnlOptions: TPanel;
    procedure btnConfigClick(Sender: TObject);
    procedure cbOtherPluginsChange(Sender: TObject);
    procedure edtPackCmdAcceptDirectory(Sender: TObject; var Value: String);
    procedure FormShow(Sender: TObject);
    procedure arbChange(Sender: TObject);

  private
    CurrentVFS : TVFS;
  public
    { public declarations }
  end;

// Frees 'fl'.
function ShowPackDlg(VFS : TVFS; var fl : TFileList; sDestPath:String; bNewArchive : Boolean = True): Boolean;

implementation
uses
  uWCXhead, uGlobs, uDCUtils;

function ShowPackDlg(VFS : TVFS; var fl: TFileList; sDestPath:String; bNewArchive : Boolean = True): Boolean;
var
  Flags : LongInt;
begin
  with TfrmPackDlg.Create(nil) do
    begin
      if bNewArchive then  // create new archive
        (* if one file selected *)
        if fl.Count = 1 then
          begin
            edtPackCmd.Text := sDestPath + ExtractFileName(fl.GetFileName(0));
            edtPackCmd.Text := ChangeFileExt(edtPackCmd.Text, '.none');
          end
        else
        (* if some files selected *)
          begin
            edtPackCmd.Text := sDestPath + MakeFileName(fl.CurrentDirectory, 'archive') + '.none';
          end
      else  // pack in exsists archive
        edtPackCmd.Text := VFS.ArcFullName;
        
      CurrentVFS := VFS;
      Result:= (ShowModal = mrOK);
      if Result then
        begin
          if VFS.FindModule(edtPackCmd.Text, False) then
            begin
              Flags := 0;
              if cbMoveToArchive.Checked then Flags := Flags or PK_PACK_MOVE_FILES;
              if cbStoredir.Checked then Flags := Flags or PK_PACK_SAVE_PATHS;
              if cbEncrypt.Checked then Flags := Flags or PK_PACK_ENCRYPT;
              if bNewArchive then
                begin
                  VFS.LoadAndOpen(edtPackCmd.Text, False);
                  VFS.VFSmodule.VFSCopyIn(fl, '', Flags);
                  VFS.VFSmodule.VFSClose;
                end
              else
                begin
                  VFS.LoadAndOpen(edtPackCmd.Text, True);
                  VFS.VFSmodule.VFSCopyIn(fl, sDestPath, Flags)
                end;
            end
            else
              FreeAndNil(fl);
        end
        else
          FreeAndNil(fl);

      Free;
    end;
end;

{ TfrmPackDlg }

procedure TfrmPackDlg.FormShow(Sender: TObject);
var
 iIndex,
 I, J : Integer;
 bExsistArchive : Boolean;
 sExt,
 sCurrentPlugin : String;
 iCurPlugCaps : Integer;
begin
  J := 0;
  sExt := ExtractFileExt(edtPackCmd.Text);
  Delete(sExt, 1, 1);  // delete a dot
  bExsistArchive := (sExt <> 'none');
  with CurrentVFS do
    begin
      for I:=0 to gWCXPlugins.Count - 1 do
        if gWCXPlugins.Enabled[I] then
        begin
          sCurrentPlugin := gWCXPlugins.ValueFromIndex[i];
          iCurPlugCaps := StrToInt(Copy(sCurrentPlugin, 1, Pos(',',sCurrentPlugin) - 1));
          if (iCurPlugCaps and PK_CAPS_NEW) = PK_CAPS_NEW then
            begin
              (* First 9 plugins we display as  RadioButtons *)
              if J < 9 then
                begin
                  iIndex := rgPacker.Items.Add(gWCXPlugins.Names[I]);
                  if bExsistArchive then
                    if (sExt = gWCXPlugins.Names[I]) then
                      rgPacker.ItemIndex := iIndex
                    else
                      rgPacker.Controls[iIndex + 1].Enabled := False;
                  J := J + 1;
                end
              else
                (* Other plugins we add in ComboBox *)
                begin
                  iIndex := cbPackerList.Items.Add(gWCXPlugins.Names[I]);
                  if bExsistArchive and (sExt = gWCXPlugins.Names[I]) then
                    cbPackerList.ItemIndex := iIndex;
                end;
            end;
        end; //for
        
        if (rgPacker.Items.Count > 0) and (rgPacker.ItemIndex < 0) then
          rgPacker.ItemIndex := 0;
        if cbPackerList.Items.Count > 0 then
          begin
            cbOtherPlugins.Visible := True;
            cbPackerList.Visible := True;

            if bExsistArchive then
              cbPackerList.Enabled:= False
            else
              cbOtherPlugins.Enabled := True;
              
            if cbPackerList.ItemIndex < 0 then
              cbPackerList.ItemIndex := 0;
          end
        else
          btnConfig.AnchorToCompanion(akTop, 6, rgPacker);
    end;
end;

procedure TfrmPackDlg.btnConfigClick(Sender: TObject);
begin
   with CurrentVFS do
   if FindModule(edtPackCmd.Text, False) and LoadAndOpen(edtPackCmd.Text, False) then
     VFSmodule.VFSConfigure(Handle);
end;

procedure TfrmPackDlg.cbOtherPluginsChange(Sender: TObject);
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

procedure TfrmPackDlg.edtPackCmdAcceptDirectory(Sender: TObject; var Value: String
  );
begin
  Value := IncludeTrailingPathDelimiter(Value) + ExtractFileName(edtPackCmd.Text);
end;

procedure TfrmPackDlg.arbChange(Sender: TObject);
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

