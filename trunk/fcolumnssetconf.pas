{
   Double Commander
   -------------------------------------------------------------------------
   Implementing of columns' configure dialog

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)

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


unit fColumnsSetConf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, Grids,  ComCtrls, Menus, LCLType;

type

  { TfColumnsSetConf }

  TfColumnsSetConf = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    edtNameofColumnsSet: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbNrOfColumnsSet: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PopupMenu1: TPopupMenu;
    stgColumns: TStringGrid;
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MenuItem1Click(Sender: TObject);
    procedure stgColumnsHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure stgColumnsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure stgColumnsSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure stgColumnsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    {Editors}
    procedure UpDownXClick(Sender: TObject; Button: TUDBtnType);
    procedure BitBtnDeleteFieldClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ComboBoxXSelect(Sender: TObject);
    procedure EditNameExit(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmColumnsSetConf: TfColumnsSetConf;
  updWidth:TUpDown;
  cbbAlign:TComboBox;
  edtName:TEdit;
  btnAdd:TButton;
  btnDel:TBitBtn;
implementation

uses uColumns;

{ TfColumnsSetConf }


procedure TfColumnsSetConf.stgColumnsSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);

var i:integer;

begin
  if assigned(updWidth) then FreeAndNil(updWidth);
  if assigned(cbbAlign) then FreeAndNil(cbbAlign);
  if assigned(edtName) then FreeAndNil(edtName);
  if assigned(btnAdd) then FreeAndNil(btnAdd);
  if assigned(btnDel) then FreeAndNil(btnDel);

 try
  case aCol of
    0: begin
         btnDel:=TBitBtn.Create(frmColumnsSetConf);
         with btnDel do
           begin
             Parent:=stgColumns;
             Glyph.Assign(btnCancel.Glyph);
             Caption:='';
             Height:=stgColumns.RowHeights[aRow];
             Width:=stgColumns.ColWidths[aCol]-2;
             Visible:=false;
             Tag:=aRow;
             OnClick:=@BitBtnDeleteFieldClick;
             Left:=(Sender as TStringGrid).CellRect(aCol,aRow).Right-Width;
             Top:=(Sender as TStringGrid).CellRect(aCol,aRow).Top;
           end;
         Editor:=btnDel;
       end;
    1:begin
       edtName:=TEdit.Create(frmColumnsSetConf);
       with edtName do
         begin
           Parent:=(Sender as TStringGrid);
           Width:=(Sender as TStringGrid).ColWidths[aCol];;
           Left:=(Sender as TStringGrid).CellRect(aCol,aRow).Right-Width;
           Top:=(Sender as TStringGrid).CellRect(aCol,aRow).Top;
           Height:=(Sender as TStringGrid).RowHeights[aRow];
           Tag:=aRow;
           OnExit:=@EditNameExit;
           Text:=(Sender as TStringGrid).Cells[aCol,aRow];
           Visible:=false;
         end;
         Editor:=edtName;
      end;

    2: begin
         updWidth:=Tupdown.Create(frmColumnsSetConf);
         with  updWidth do
           begin
             Parent:=(Sender as TStringGrid);
             Left:=(Sender as TStringGrid).CellRect(aCol,aRow).Right-Width;
             Top:=(Sender as TStringGrid).CellRect(aCol,aRow).Top;
             Height:=(Sender as TStringGrid).RowHeights[aRow];
             Tag:=aRow;
             if ((Sender as TStringGrid).Cells[aCol,aRow])<>'' then
              Position:=StrToInt((Sender as TStringGrid).Cells[aCol,aRow]);
             OnClick:=@updownXClick;
             Visible:=false;
         end;
         Editor:=updWidth;
       end;
    3: begin
         cbbAlign:=TComboBox.Create(frmColumnsSetConf);
         with cbbAlign do
           begin
             Parent:=(Sender as TStringGrid);
             Width:=(Sender as TStringGrid).ColWidths[aCol];
             Left:=(Sender as TStringGrid).CellRect(aCol,aRow).Right-Width;
             Top:=(Sender as TStringGrid).CellRect(aCol,aRow).Top;
             Height:=(Sender as TStringGrid).RowHeights[aRow];
             Tag:=aRow;
             Style:=csDropDownList;
             AddItem('<-',nil);
             AddItem('->',nil);
             OnSelect:=@ComboBoxXSelect;
             ItemIndex:=Items.IndexOf((Sender as TStringGrid).Cells[aCol,aRow]);
             Visible:=false;
           end;
           Editor:=cbbAlign;
       end;
    4: begin
         btnAdd:=TButton.Create(frmColumnsSetConf);
         with btnAdd do
           begin
             Parent:=(Sender as TStringGrid);
             Width:=20;
             Left:=(Sender as TStringGrid).CellRect(aCol,aRow).Right-Width;
             Top:=(Sender as TStringGrid).CellRect(aCol,aRow).Top;
             Height:=(Sender as TStringGrid).RowHeights[aRow];
             Tag:=aRow;
             Caption:='+';
             OnClick:=@ButtonAddClick;
             Visible:=true;
           end;
       end;
  end;

 finally
 end;
end;


procedure TfColumnsSetConf.stgColumnsHeaderSized(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
begin
  //TODO: move editors
end;


procedure TfColumnsSetConf.stgColumnsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if (Key=vk_Down) and (stgColumns.Row=stgColumns.RowCount-1) then
    begin
      stgColumns.RowCount:=stgColumns.RowCount+1;
    end;
end;

procedure TfColumnsSetConf.EditNameExit(Sender: TObject);
begin
stgColumns.Cells[1,(Sender as TEdit).Tag]:=(Sender as TEdit).Text;
end;

procedure TfColumnsSetConf.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin

end;


procedure TfColumnsSetConf.MenuItem1Click(Sender: TObject);
begin
  stgColumns.RowCount:=stgColumns.RowCount+1;
end;

procedure TfColumnsSetConf.ComboBoxXSelect(Sender: TObject);
begin
stgColumns.Cells[3,(Sender as TComboBox).Tag]:=(Sender as TComboBox).Text;
end;

procedure TfColumnsSetConf.BitBtnDeleteFieldClick(Sender: TObject);
begin
ShowMessage(IntTostr((Sender as TBitBtn).Tag));
end;

procedure TfColumnsSetConf.btnCancelClick(Sender: TObject);
begin

end;

procedure TfColumnsSetConf.ButtonAddClick(Sender: TObject);
begin
  //TODO: show column fields menu
end;

procedure TfColumnsSetConf.stgColumnsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin

end;

procedure TfColumnsSetConf.updownXClick(Sender: TObject; Button: TUDBtnType);
begin
stgColumns.Cells[2,(Sender as Tupdown).Tag]:=IntToStr((Sender as Tupdown).Position);
end;


initialization
  {$I fcolumnssetconf.lrs}

end.

