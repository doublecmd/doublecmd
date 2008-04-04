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
  ExtCtrls, Buttons, Grids,  ComCtrls, Menus, LCLType, uColumns,uGlobs, Spin;

type

  { TfColumnsSetConf }

  TfColumnsSetConf = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    edtNameofColumnsSet: TEdit;
    lblConfigViewNr: TLabel;
    lblName: TLabel;
    lbNrOfColumnsSet: TLabel;
    miAddColumn: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pmStringGrid: TPopupMenu;
    pmFields: TPopupMenu;
    stgColumns: TStringGrid;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miAddColumnClick(Sender: TObject);
    procedure MenuFieldsClick(Sender: TObject);
    procedure stgColumnsHeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure stgColumnsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure stgColumnsSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);

    {Editors}
    procedure SpinEditExit(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure BitBtnDeleteFieldClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ComboBoxXSelect(Sender: TObject);
    procedure UpDownXClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDownXChanging(Sender: TObject; var AllowChange: Boolean);
  private
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    { private declarations }
  public
    { public declarations }
    ColumnClass:TPanelColumnsClass;
    procedure AddNewField;
  end; 

  procedure EditorSaveResult(Sender: TObject);

var
  frmColumnsSetConf: TfColumnsSetConf;
  updWidth:TSpinEdit;
  cbbAlign:TComboBox;
  edtField:TEdit;
  btnAdd:TButton;
  btnDel:TBitBtn;
  updMove:TUpDown;
  Showed:boolean;
implementation

uses uLng;

procedure EditorSaveResult(Sender: TObject);
begin
 with frmColumnsSetConf do
   begin
    if Sender is TSpinEdit then
     stgColumns.Cells[2,(Sender as TSpinEdit).Tag]:=inttostr(updWidth.Value);
    if Sender is TComboBox then
     stgColumns.Cells[3,(Sender as TComboBox).Tag]:=(Sender as TComboBox).Text;
    if Sender is TEdit then
     stgColumns.Cells[4,(Sender as TEdit).Tag]:=(Sender as TEdit).Text;

   end;
end;


{ TfColumnsSetConf }


procedure TfColumnsSetConf.stgColumnsSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);

var i:integer;

begin
  if assigned(updWidth) then FreeAndNil(updWidth);
  if assigned(cbbAlign) then FreeAndNil(cbbAlign);
  if assigned(btnAdd) then FreeAndNil(btnAdd);
  if assigned(btnDel) then FreeAndNil(btnDel);
  if assigned(edtField) then FreeAndNil(edtField);
  if assigned(updMove) then FreeAndNil(updMove);

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

    2: begin
         updWidth:=TSpinEdit.Create(frmColumnsSetConf);
         with  updWidth do
           begin
             Parent:=(Sender as TStringGrid);
             Left:=(Sender as TStringGrid).CellRect(aCol,aRow).Left;
             Top:=(Sender as TStringGrid).CellRect(aCol,aRow).Top;
             Height:=(Sender as TStringGrid).RowHeights[aRow];
             Width:=(Sender as TStringGrid).ColWidths[aCol];
             Tag:=aRow;
             if ((Sender as TStringGrid).Cells[aCol,aRow])<>'' then
             MaxValue:=1000;
             Value:=StrToInt((Sender as TStringGrid).Cells[aCol,aRow]);
             OnKeyDown:=@EditorKeyDown;
             OnExit:=@SpinEditExit;
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
             Left:=(Sender as TStringGrid).CellRect(aCol,aRow).Left;
             Top:=(Sender as TStringGrid).CellRect(aCol,aRow).Top;
             Height:=(Sender as TStringGrid).RowHeights[aRow];
             Tag:=aRow;
             Style:=csDropDownList;
             AddItem('<-',nil);
             AddItem('->',nil);
           //TODO: center alignment
           //AddItem('=',nil);
             OnSelect:=@ComboBoxXSelect;
             OnKeyDown:=@EditorKeyDown;
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
         edtField:=TEdit.Create(frmColumnsSetConf);
         with edtField do
           begin
             Parent:=(Sender as TStringGrid);
             Width:=(Sender as TStringGrid).ColWidths[aCol];
             Left:=(Sender as TStringGrid).CellRect(aCol,aRow).Left;
             Top:=(Sender as TStringGrid).CellRect(aCol,aRow).Top;
             Height:=(Sender as TStringGrid).RowHeights[aRow];
             Tag:=aRow;
             OnExit:=@EditExit;
             OnKeyDown:=@EditorKeyDown;
             Visible:=false;
             Text:=(Sender as TStringGrid).Cells[aCol,aRow];
           end;
         Editor:=edtField;
       end;
    5: begin
     updMove:=TUpDown.Create(frmColumnsSetConf);
     with updMove do
       begin
         Parent:=stgColumns;
         Height:=stgColumns.RowHeights[aRow];
         Width:=stgColumns.ColWidths[aCol]-2;
         Visible:=false;
         Tag:=aRow;
         Min:=-((Sender as TStringGrid).RowCount-1);
         Max:=-1;
         Position:=-aRow;
         OnChanging:=@UpDownXChanging;
         OnClick:=@UpDownXClick;
         Left:=(Sender as TStringGrid).CellRect(aCol,aRow).Right-Width;
         Top:=(Sender as TStringGrid).CellRect(aCol,aRow).Top;
       end;
     Editor:=updMove;
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
      AddNewField;
    end;
end;

procedure TfColumnsSetConf.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key=VK_RETURN then
      begin
         EditorSaveResult(Sender);
         stgColumns.EditorExit(Sender);
         Key:=0;
      end;
end;

procedure TfColumnsSetConf.AddNewField;
begin
  stgColumns.RowCount:=stgColumns.RowCount+1;
  stgColumns.Cells[1,stgColumns.RowCount-1]:='Field N'+IntToStr(stgColumns.RowCount-1);
  stgColumns.Cells[2,stgColumns.RowCount-1]:='25';
  stgColumns.Cells[3,stgColumns.RowCount-1]:='<-';
  stgColumns.Cells[4,stgColumns.RowCount-1]:='';
end;

procedure TfColumnsSetConf.FormCreate(Sender: TObject);
begin
  ColumnClass:=TPanelColumnsClass.Create;
end;

procedure TfColumnsSetConf.FormDestroy(Sender: TObject);
begin
  showed:=false;
  if assigned(updWidth) then FreeAndNil(updWidth);
  if assigned(cbbAlign) then FreeAndNil(cbbAlign);
  if assigned(btnAdd) then FreeAndNil(btnAdd);
  if assigned(btnDel) then FreeAndNil(btnDel);
  if assigned(edtField) then FreeAndNil(edtField);
  if assigned(updMove) then FreeAndNil(updMove);
   ColumnClass.Free;
end;

procedure TfColumnsSetConf.FormResize(Sender: TObject);
var z,i:integer;
begin
if not showed then exit;
   //Size of content field
    z:=stgColumns.Width;
    for i:=0 to 3 do
     z:=z-stgColumns.ColWidths[i];
    z:=z-stgColumns.ColWidths[5];
    stgColumns.ColWidths[4]:=z;
end;

procedure TfColumnsSetConf.FormShow(Sender: TObject);
var i:integer;
begin
    if ColumnClass.ColumnsCount>0 then
      begin
        stgColumns.RowCount:=ColumnClass.ColumnsCount+1;
        for i:=0 to ColumnClass.ColumnsCount-1 do
          begin
              stgColumns.Cells[1,i+1]:=ColumnClass.GetColumnTitle(i);
              stgColumns.Cells[2,i+1]:=inttostr(ColumnClass.GetColumnWidth(i));
              stgColumns.Cells[3,i+1]:=ColumnClass.GetColumnAlignString(i);
              stgColumns.Cells[4,i+1]:=ColumnClass.GetColumnFuncString(i);
          end;
      end
    else
        begin
            stgColumns.RowCount:=1;
            frmColumnsSetConf.AddNewField;
        end;
    Showed:=true;
    // Localize StringGrid header
    stgColumns.Cells[0,0]:= rsConfColDelete;
    stgColumns.Cells[1,0]:= rsConfColCaption;
    stgColumns.Cells[2,0]:= rsConfColWidth;
    stgColumns.Cells[3,0]:= rsConfColAlign;
    stgColumns.Cells[4,0]:= rsConfColFieldCont;

end;


procedure TfColumnsSetConf.miAddColumnClick(Sender: TObject);
begin
  AddNewField;
end;

procedure TfColumnsSetConf.SpinEditExit(Sender: TObject);
begin
  EditorSaveResult(Sender);
end;

procedure TfColumnsSetConf.EditExit(Sender: TObject);
begin
  EditorSaveResult(Sender);
end;

procedure TfColumnsSetConf.ComboBoxXSelect(Sender: TObject);
begin
  EditorSaveResult(Sender);
end;

procedure TfColumnsSetConf.UpDownXClick(Sender: TObject; Button: TUDBtnType);
begin
 stgColumns.ExchangeColRow(False,updMove.Tag,abs(updMove.Position));
 with updMove do
   begin
     Left:=stgColumns.CellRect(5,abs(updMove.Position)).Right-Width;
     Top:=stgColumns.CellRect(5,abs(updMove.Position)).Top;
   end;


end;

procedure TfColumnsSetConf.UpDownXChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  updMove.tag:=abs(updMove.Position);
end;

procedure TfColumnsSetConf.BitBtnDeleteFieldClick(Sender: TObject);
begin
stgColumns.DeleteColRow(false,(Sender as TBitBtn).Tag);
end;

procedure TfColumnsSetConf.btnOkClick(Sender: TObject);
var i:integer;
   Tit,
   FuncString: string;
   Wid: integer;
   Ali: TAlignment;
begin
  // Save fields
  ColumnClass.Clear;
  for i:=1 to stgColumns.RowCount-1 do
    begin
      with stgColumns do
        begin
          Tit:=Cells[1,i];
          Wid:=StrToInt(Cells[2,i]);
          Ali:=StrToAlign(Cells[3,i]);
          FuncString:=Cells[4,i];
        end;
      ColumnClass.Add(Tit,FuncString,Wid,Ali);
    end;


    case Self.Tag of
    -1: ColSet.Items.Add(edtNameofColumnsSet.Text);
    else
      begin
        ColSet.DeleteColumnSet(gIni,Self.Tag);
        ColSet.Items.Insert(Self.Tag,edtNameofColumnsSet.Text);
      end;
    end;
ColumnClass.Save(gIni,(edtNameofColumnsSet.Text));
end;

procedure TfColumnsSetConf.MenuFieldsClick(Sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    0:  begin
          stgColumns.Cells[4,btnAdd.Tag]:=stgColumns.Cells[4,btnAdd.Tag]+'[DC().'+(Sender as TMenuItem).Caption+'{}] ';
        end;
    1: begin
          stgColumns.Cells[4,btnAdd.Tag]:=stgColumns.Cells[4,btnAdd.Tag]+'[Plugin('+(Sender as TMenuItem).Parent.Caption+').'+(Sender as TMenuItem).Caption+'{}] ';
       end;
    2: begin
         //for WDX scripts
       end;
  end;

end;


procedure TfColumnsSetConf.ButtonAddClick(Sender: TObject);
var Mi:TMenuItem; i,j:integer; point:TPoint;
begin
// show column fields menu

  pmFields.Items.Clear;

  //DC commands

       MI:=TMenuItem.Create(pmFields);
       MI.Caption:='DC';
       pmFields.Items.Add(MI);
       for i:= 0 to IntList.Count-1 do
         begin
           MI:=TMenuItem.Create(pmFields);
           MI.Tag:=0;
           MI.Caption:=IntList[i];
           MI.OnClick:=@MenuFieldsClick;
           pmFields.Items.Items[0].Add(MI);
         end;
  //Plugins
       MI:=TMenuItem.Create(pmFields);
       MI.Caption:='Plugins';
       pmFields.Items.Add(MI);
       for i:=0 to WdxPlugins.Count-1 do
         begin
           MI:=TMenuItem.Create(pmFields);
           MI.Caption:=WdxPlugins.GetWdxModule(i).Name;
           pmFields.Items.Items[1].Add(MI);
           //Load fields list
           if WdxPlugins.GetWdxModule(i).IsLoaded=false then
             if not (WdxPlugins.GetWdxModule(i).LoadModule) then break;
           for j:=0 to  WdxPlugins.GetWdxModule(i).FieldList.Count-1 do
             begin
               with WdxPlugins.GetWdxModule(i) do
                 begin
                   MI:=TMenuItem.Create(pmFields);
                   MI.Tag:=1;
                   MI.Caption:=FieldList[j];
                   MI.OnClick:=@MenuFieldsClick;
                   pmFields.Items.Items[1].Items[i].Add(MI);
                 end;
             end;
         end;
         
         
   point.x:=(Sender as TButton).Left-25;
   point.y:=(Sender as TButton).top+(Sender as TButton).Height+40;;
   point:=ClientToScreen(Point);
   pmFields.PopUp(point.X,point.Y);
  
end;


initialization
  {$I fcolumnssetconf.lrs}

end.

