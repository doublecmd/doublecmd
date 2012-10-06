{
   Double Commander
   -------------------------------------------------------------------------
   Ignore list options page

   Copyright (C) 2006-2011  Koblov Alexander (Alexx2000@mail.ru)

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

unit fOptionsIgnoreList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, EditBtn, fOptionsFrame;

type

  { TfrmOptionsIgnoreList }

  TfrmOptionsIgnoreList = class(TOptionsEditor)
    btnAddSel: TButton;
    btnAddSelWithPath: TButton;
    chkIgnoreEnable: TCheckBox;
    fneSaveIn: TFileNameEdit;
    lblSaveIn: TLabel;
    memIgnoreList: TMemo;
    procedure btnAddSelClick(Sender: TObject);
    procedure btnAddSelWithPathClick(Sender: TObject);
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
  uGlobs, uFile, uLng, fMain;

{ TfrmOptionsIgnoreList }

procedure TfrmOptionsIgnoreList.btnAddSelClick(Sender: TObject);
begin
  FillIgnoreList(False);
end;

procedure TfrmOptionsIgnoreList.btnAddSelWithPathClick(Sender: TObject);
begin
  FillIgnoreList(True);
end;

procedure TfrmOptionsIgnoreList.chkIgnoreEnableChange(Sender: TObject);
begin
  memIgnoreList.Enabled:= chkIgnoreEnable.Checked;
  fneSaveIn.Enabled:= chkIgnoreEnable.Checked;
  btnAddSelWithPath.Enabled:= chkIgnoreEnable.Checked;
  btnAddSel.Enabled:= chkIgnoreEnable.Checked;
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
end;

function TfrmOptionsIgnoreList.Save: TOptionsEditorSaveFlags;
begin
  Result := [];

  gIgnoreListFileEnabled:= chkIgnoreEnable.Checked;
  gIgnoreListFile:= fneSaveIn.FileName;
  glsIgnoreList.Assign(memIgnoreList.Lines);
end;

end.

