{
   Double Commander
   -------------------------------------------------------------------------
   File types colors options page

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

unit fOptionsFileTypesColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ColorBox, Dialogs, Buttons,
  fOptionsFrame;

type

  { TfrmOptionsFileTypesColors }

  TfrmOptionsFileTypesColors = class(TOptionsEditor)
    optColorDialog: TColorDialog;
    btnAddCategory: TBitBtn;
    btnApplyCategory: TBitBtn;
    btnDeleteCategory: TBitBtn;
    btnCategoryColor: TButton;
    btnSearchTemplate: TBitBtn;
    cbCategoryColor: TColorBox;
    edtCategoryAttr: TEdit;
    edtCategoryMask: TEdit;
    edtCategoryName: TEdit;
    gbFileTypesColors: TGroupBox;
    lbCategories: TListBox;
    lblCategoryAttr: TLabel;
    lblCategoryColor: TLabel;
    lblCategoryMask: TLabel;
    lblCategoryName: TLabel;

    procedure lbCategoriesClick(Sender: TObject);
    procedure btnSearchTemplateClick(Sender: TObject);
    procedure btnAddCategoryClick(Sender: TObject);
    procedure btnApplyCategoryClick(Sender: TObject);
    procedure btnDeleteCategoryClick(Sender: TObject);
    procedure btnCategoryColorClick(Sender: TObject);
    procedure lbCategoriesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbCategoriesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbCategoriesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
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
  Graphics, uLng, uGlobs, uColorExt, fMaskInputDlg, uSearchTemplate, uDCUtils;

{ TfrmOptionsFileTypesColors }

procedure TfrmOptionsFileTypesColors.lbCategoriesClick(Sender: TObject);
var
  MaskItem : TMaskItem;
  bEnabled: Boolean;
begin
  if (lbCategories.ItemIndex <> -1) then
    begin
      MaskItem := TMaskItem(lbCategories.Items.Objects[lbCategories.ItemIndex]);

      edtCategoryName.Text := MaskItem.sName;
      edtCategoryMask.Text := MaskItem.sExt;
      SetColorInColorBox(cbCategoryColor, MaskItem.cColor);
      bEnabled:= (MaskItem.sExt = '') or (MaskItem.sExt[1] <> '>');
      edtCategoryMask.Enabled:= bEnabled;
      edtCategoryAttr.Enabled:= bEnabled;
      edtCategoryAttr.Text := MaskItem.sModeStr;
    end
  else
    begin
      edtCategoryName.Text := '';
      edtCategoryMask.Text := '';
      edtCategoryAttr.Text := '';
      cbCategoryColor.ItemIndex := -1;
    end;
end;

procedure TfrmOptionsFileTypesColors.btnSearchTemplateClick(Sender: TObject);
var
  sMask: UTF8String = '';
  bTemplate: Boolean;
begin
  if ShowMaskInputDlg(rsMarkPlus, rsMaskInput, glsMaskHistory, sMask) then
    begin
      bTemplate:= IsMaskSearchTemplate(sMask);
      edtCategoryMask.Text:= sMask;
      if bTemplate then edtCategoryAttr.Text:= EmptyStr;
      edtCategoryMask.Enabled:= not bTemplate;
      edtCategoryAttr.Enabled:= not bTemplate;
    end;
end;

procedure TfrmOptionsFileTypesColors.btnAddCategoryClick(Sender: TObject);
var
  iIndex : Integer;
  MaskItem: TMaskItem;
begin
  if lbCategories.Count = 0 then
    begin
      edtCategoryName.Enabled := True;
      edtCategoryMask.Enabled := True;
      edtCategoryAttr.Enabled := True;
      cbCategoryColor.Enabled := True;
      btnCategoryColor.Enabled := True;
      btnDeleteCategory.Enabled := True;
      btnApplyCategory.Enabled := True;
    end;

  MaskItem := TMaskItem.Create;
  try
   edtCategoryName.Text := 'New';
   edtCategoryMask.Text := '*';
   edtCategoryAttr.Text := '';
   cbCategoryColor.ItemIndex := -1;

   MaskItem.sName:= edtCategoryName.Text;
   MaskItem.sExt:= edtCategoryMask.Text;
   MaskItem.sModeStr:= edtCategoryAttr.Text;
   MaskItem.cColor:= clBlack;
   iIndex := lbCategories.Items.AddObject(MaskItem.sName, MaskItem);
  except
    FreeAndNil(MaskItem);
    raise;
  end;
  lbCategories.ItemIndex:= iIndex;
  edtCategoryName.SetFocus;
end;

procedure TfrmOptionsFileTypesColors.btnApplyCategoryClick(Sender: TObject);
var
  MaskItem : TMaskItem;
  I, iCount : Integer;
begin
  if (lbCategories.ItemIndex <> -1) then
  begin
    lbCategories.Items[lbCategories.ItemIndex] := edtCategoryName.Text;
    if edtCategoryMask.Text = '' then
      edtCategoryMask.Text := '*'; // because we load colors from ini by mask
    MaskItem := TMaskItem(lbCategories.Items.Objects[lbCategories.ItemIndex]);

    MaskItem.sName := edtCategoryName.Text;
    MaskItem.cColor := cbCategoryColor.Selected;
    MaskItem.sExt := edtCategoryMask.Text;
    MaskItem.sModeStr := edtCategoryAttr.Text;
  end;
end;

procedure TfrmOptionsFileTypesColors.btnDeleteCategoryClick(Sender: TObject);
begin
  if (lbCategories.ItemIndex <> -1) then
  begin
    lbCategories.Items.Objects[lbCategories.ItemIndex].Free;
    lbCategories.Items.Delete(lbCategories.ItemIndex);
    if lbCategories.Count > 0 then
      lbCategories.ItemIndex := 0;
    lbCategoriesClick(lbCategories);
  end;
end;

procedure TfrmOptionsFileTypesColors.btnCategoryColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbCategoryColor.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbCategoryColor, optColorDialog.Color);
end;

procedure TfrmOptionsFileTypesColors.lbCategoriesDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  SrcIndex, DestIndex: Integer;
begin
  SrcIndex := lbCategories.ItemIndex;
  if SrcIndex = -1 then
    Exit;
  DestIndex := lbCategories.GetIndexAtY(Y);
  if (DestIndex < 0) or (DestIndex >= lbCategories.Count) then
    DestIndex := lbCategories.Count - 1;

  lbCategories.Items.Move(SrcIndex, DestIndex);
  lbCategories.ItemIndex := DestIndex;
end;

procedure TfrmOptionsFileTypesColors.lbCategoriesDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lbCategories) and (lbCategories.ItemIndex <> -1);
end;

procedure TfrmOptionsFileTypesColors.lbCategoriesDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox) do
   begin
     Canvas.FillRect(ARect);
     if (not Selected[Index]) and Assigned(Items.Objects[Index]) then
       Canvas.Font.Color:= TMaskItem(Items.Objects[Index]).cColor
     else
       Canvas.Font.Color:= gCursorText;

     Canvas.TextOut(ARect.Left+2,ARect.Top,Items[Index]);
   end;
end;

procedure TfrmOptionsFileTypesColors.Init;
begin
  lbCategories.Canvas.Font := lbCategories.Font;
  lbCategories.ItemHeight := lbCategories.Canvas.TextHeight('Wg');
end;

class function TfrmOptionsFileTypesColors.GetIconIndex: Integer;
begin
  Result := 21;
end;

class function TfrmOptionsFileTypesColors.GetTitle: String;
begin
  Result := rsOptionsEditorFileTypes;
end;

procedure TfrmOptionsFileTypesColors.Load;
var
  I : Integer;
  MaskItem: TMaskItem;
begin
  lbCategories.Clear;

  { File lbtypes category color }
  for I := 0 to gColorExt.Count - 1 do
    begin
      MaskItem := TMaskItem.Create;
      try
       MaskItem.Assign(gColorExt[I]);
       lbCategories.Items.AddObject(MaskItem.sName, MaskItem);
      except
        FreeAndNil(MaskItem);
        raise;
      end;
    end; // for

    if lbCategories.Count > 0 then
      lbCategories.ItemIndex := 0
    else
      begin
        edtCategoryName.Enabled := False;
        edtCategoryMask.Enabled := False;
        edtCategoryAttr.Enabled := False;
        cbCategoryColor.Enabled := False;
        btnCategoryColor.Enabled := False;
        btnDeleteCategory.Enabled := False;
        btnApplyCategory.Enabled := False;
      end;
  lbCategoriesClick(lbCategories);
end;

function TfrmOptionsFileTypesColors.Save: TOptionsEditorSaveFlags;
var
  i: Integer;
  MaskItem: TMaskItem;
begin
  Result := [];
  gColorExt.Clear;

  for I := 0 to lbCategories.Count - 1 do  //write new categories
    if Assigned(lbCategories.Items.Objects[I]) then
    begin
      MaskItem := TMaskItem.Create;
      try
       MaskItem.Assign(TMaskItem(lbCategories.Items.Objects[I]));
       gColorExt.Add(MaskItem);
      except
        FreeAndNil(MaskItem);
        raise;
      end;
    end;
end;

end.

