{
   Double Commander
   -------------------------------------------------------------------------
   Columns files view options page

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

unit fOptionsColumnsView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fOptionsFrame, StdCtrls;

type

  { TfrmOptionsColumnsView }

  TfrmOptionsColumnsView = class(TOptionsEditor)
    cbCutTextToColWidth: TCheckBox;
    cbGridHorzLine: TCheckBox;
    cbGridVertLine: TCheckBox;
    chkAutoFillColumns: TCheckBox;
    cmbAutoSizeColumn: TComboBox;
    gbShowGrid: TGroupBox;
    grpMisc: TGroupBox;
    grpAutosizeColumns: TGroupBox;
    lblAutoSizeColumn: TLabel;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  DCStrUtils, uGlobs, uLng;

{ TfrmOptionsColumnsView }

procedure TfrmOptionsColumnsView.Init;
begin
  ParseLineToList(rsOptAutoSizeColumn, cmbAutoSizeColumn.Items);
end;

procedure TfrmOptionsColumnsView.Load;
begin
  cbGridVertLine.Checked      := gGridVertLine;
  cbGridHorzLine.Checked      := gGridHorzLine;
  chkAutoFillColumns.Checked  := gAutoFillColumns;
  cmbAutoSizeColumn.ItemIndex := gAutoSizeColumn;
  cbCutTextToColWidth.Checked := gCutTextToColWidth;
end;

function TfrmOptionsColumnsView.Save: TOptionsEditorSaveFlags;
begin
  gGridVertLine      := cbGridVertLine.Checked;
  gGridHorzLine      := cbGridHorzLine.Checked;
  gAutoFillColumns   := chkAutoFillColumns.Checked;
  gAutoSizeColumn    := cmbAutoSizeColumn.ItemIndex;
  gCutTextToColWidth := cbCutTextToColWidth.Checked;

  Result := [];
end;

class function TfrmOptionsColumnsView.GetIconIndex: Integer;
begin
  Result := 13;
end;

class function TfrmOptionsColumnsView.GetTitle: String;
begin
  Result := rsOptionsEditorColumnsView;
end;

end.
