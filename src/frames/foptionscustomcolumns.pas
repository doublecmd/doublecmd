{
   Double Commander
   -------------------------------------------------------------------------
   Custom columns options page

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

unit fOptionsCustomColumns;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls,
  fOptionsFrame;

type

  { TfrmOptionsCustomColumns }

  TfrmOptionsCustomColumns = class(TOptionsEditor)
    btnCopyColumnsSet: TButton;
    btnDelColumnsSet: TButton;
    btnEditColumnsSet: TButton;
    btnNewColumnsSet: TButton;
    cbbFileSystem: TComboBox;
    lblConfigColumns: TLabel;
    lstColumnsSets: TListBox;
    pnlButtons: TPanel;
    procedure btnCopyColumnsSetClick(Sender: TObject);
    procedure btnDelColumnsSetClick(Sender: TObject);
    procedure btnEditColumnsSetClick(Sender: TObject);
    procedure btnNewColumnsSetClick(Sender: TObject);
  private
    procedure FillColumnsList;
  protected
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  uGlobs, uLng, fColumnsSetConf;

{ TfrmOptionsCustomColumns }

procedure TfrmOptionsCustomColumns.btnCopyColumnsSetClick(Sender: TObject);
var
  s: string;
begin
  if lstColumnsSets.ItemIndex <> -1 then
  begin
    s := lstColumnsSets.Items[lstColumnsSets.ItemIndex];
    ColSet.CopyColumnSet(s, s + '_Copy');
    FillColumnsList;
  end;
end;

procedure TfrmOptionsCustomColumns.btnDelColumnsSetClick(Sender: TObject);
begin
  if lstColumnsSets.ItemIndex=-1 then exit;
  if lstColumnsSets.Count=1 then exit;
  ColSet.DeleteColumnSet(lstColumnsSets.Items[lstColumnsSets.ItemIndex]);
  FillColumnsList;
end;

procedure TfrmOptionsCustomColumns.btnEditColumnsSetClick(Sender: TObject);
var
  frmColumnsSetConf: TfColumnsSetConf;
begin
  if lstColumnsSets.ItemIndex=-1 then exit;

  frmColumnsSetConf := TfColumnsSetConf.Create(nil);
  try
    {EDIT Set}
    frmColumnsSetConf.edtNameofColumnsSet.Text:=lstColumnsSets.Items[lstColumnsSets.ItemIndex];
    frmColumnsSetConf.lbNrOfColumnsSet.Caption:=IntToStr(lstColumnsSets.ItemIndex+1);
    frmColumnsSetConf.Tag:=lstColumnsSets.ItemIndex;
    frmColumnsSetConf.SetColumnsClass(ColSet.GetColumnSet(lstColumnsSets.Items[lstColumnsSets.ItemIndex]));
    {EDIT Set}
    frmColumnsSetConf.ShowModal;
    FillColumnsList;
  finally
    FreeAndNil(frmColumnsSetConf);
  end;
end;

procedure TfrmOptionsCustomColumns.btnNewColumnsSetClick(Sender: TObject);
var
  frmColumnsSetConf: TfColumnsSetConf;
begin
  frmColumnsSetConf := TfColumnsSetConf.Create(nil);
  try
    // Create new Set
    frmColumnsSetConf.edtNameofColumnsSet.Text:='New Columns'+inttostr(ColSet.count);
    frmColumnsSetConf.lbNrOfColumnsSet.Caption:=IntToStr(lstColumnsSets.Count+1);
    frmColumnsSetConf.Tag:=-1;
    frmColumnsSetConf.SetColumnsClass(nil);
    frmColumnsSetConf.ShowModal;
    FillColumnsList;
  finally
    FreeAndNil(frmColumnsSetConf);
  end;
end;

procedure TfrmOptionsCustomColumns.FillColumnsList;
begin
 lstColumnsSets.Clear;
 If ColSet.Items.Count>0 then
   begin
     lstColumnsSets.Items.AddStrings(ColSet.Items);
   end;
end;

class function TfrmOptionsCustomColumns.GetIconIndex: Integer;
begin
  Result := 30;
end;

class function TfrmOptionsCustomColumns.GetTitle: String;
begin
  Result := rsOptionsEditorCustomColumns;
end;

procedure TfrmOptionsCustomColumns.Load;
begin
  FillColumnsList;
end;

function TfrmOptionsCustomColumns.Save: TOptionsEditorSaveFlags;
begin
  Result := [];
end;

end.

