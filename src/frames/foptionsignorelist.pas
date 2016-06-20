{
   Double Commander
   -------------------------------------------------------------------------
   Ignore list options page

   Copyright (C) 2006-2016  Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit fOptionsIgnoreList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, EditBtn, Buttons, Menus, fOptionsFrame;

type

  { TfrmOptionsIgnoreList }

  TfrmOptionsIgnoreList = class(TOptionsEditor)
    btnAddSel: TButton;
    btnAddSelWithPath: TButton;
    btnRelativeSaveIn: TSpeedButton;
    chkIgnoreEnable: TCheckBox;
    fneSaveIn: TFileNameEdit;
    lblSaveIn: TLabel;
    memIgnoreList: TMemo;
    pmPathHelper: TPopupMenu;
    procedure btnAddSelClick(Sender: TObject);
    procedure btnAddSelWithPathClick(Sender: TObject);
    procedure btnRelativeSaveInClick(Sender: TObject);
    procedure chkIgnoreEnableChange(Sender: TObject);
  private
    procedure FillIgnoreList(bWithFullPath: Boolean);
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  end; 

implementation

{$R *.lfm}

uses
  Controls, uGlobs, uFile, uLng, fMain, uSpecialDir;

{ TfrmOptionsIgnoreList }

procedure TfrmOptionsIgnoreList.btnAddSelClick(Sender: TObject);
begin
  FillIgnoreList(False);
end;

procedure TfrmOptionsIgnoreList.btnAddSelWithPathClick(Sender: TObject);
begin
  FillIgnoreList(True);
end;

procedure TfrmOptionsIgnoreList.btnRelativeSaveInClick(Sender: TObject);
begin
  fneSaveIn.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(fneSaveIn,pfFILE);
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;


procedure TfrmOptionsIgnoreList.chkIgnoreEnableChange(Sender: TObject);
begin
  memIgnoreList.Enabled:= chkIgnoreEnable.Checked;
  lblSaveIn.Enabled:= chkIgnoreEnable.Checked;
  fneSaveIn.Enabled:= chkIgnoreEnable.Checked;
  btnAddSelWithPath.Enabled:= chkIgnoreEnable.Checked;
  btnAddSel.Enabled:= chkIgnoreEnable.Checked;
  btnRelativeSaveIn.Enabled:= chkIgnoreEnable.Checked;
end;

procedure TfrmOptionsIgnoreList.FillIgnoreList(bWithFullPath: Boolean);
var
  I: Integer;
  SelectedFiles: TFiles;
begin
  SelectedFiles := frmMain.ActiveFrame.CloneSelectedOrActiveFiles;
  try
    for I:= 0 to SelectedFiles.Count - 1 do
      if bWithFullPath then
        memIgnoreList.Lines.Add(SelectedFiles[I].FullPath)
      else
        memIgnoreList.Lines.Add(SelectedFiles[I].Name);
  finally
    FreeAndNil(SelectedFiles);
  end;
end;

class function TfrmOptionsIgnoreList.GetIconIndex: Integer;
begin
  Result := 17;
end;

class function TfrmOptionsIgnoreList.GetTitle: String;
begin
  Result := rsOptionsEditorIgnoreList;
end;

procedure TfrmOptionsIgnoreList.Load;
begin
  chkIgnoreEnable.Checked:= gIgnoreListFileEnabled;
  fneSaveIn.FileName:= gIgnoreListFile;
  memIgnoreList.Lines.Assign(glsIgnoreList);
  chkIgnoreEnableChange(chkIgnoreEnable);
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper,mp_PATHHELPER,nil);
end;

function TfrmOptionsIgnoreList.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gIgnoreListFileEnabled:= chkIgnoreEnable.Checked;
  gIgnoreListFile:= fneSaveIn.FileName;
  glsIgnoreList.Assign(memIgnoreList.Lines);
end;

end.

