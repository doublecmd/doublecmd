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
    procedure lbCategoriesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
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
  if (lbCategories.Count > 0) and (Assigned(lbCategories.Items.Objects[lbCategories.ItemIndex])) then
    begin
      edtCategoryName.Text := lbCategories.Items[lbCategories.ItemIndex];
      MaskItem := TMaskItem(lbCategories.Items.Objects[lbCategories.ItemIndex]);

      edtCategoryMask.Text := MaskItem.sExt;
      SetColorInColorBox(cbCategoryColor, MaskItem.cColor);
      bEnabled:= Pos('>', MaskItem.sExt) <> 1;
      edtCategoryMask.Enabled:= bEnabled;
      edtCategoryAttr.Enabled:= bEnabled;
      edtCategoryAttr.Text := MaskItem.sModeStr;
    end
  else
    begin
      if lbCategories.Count = 0 then
        edtCategoryName.Text := ''
      else
        edtCategoryName.Text := lbCategories.Items[lbCategories.ItemIndex];
      edtCategoryMask.Text := '*';
      edtCategoryAttr.Text := '';
      cbCategoryColor.ItemIndex := -1;
      cbCategoryColor.Selected := clWindow;
    end;
end;

procedure TfrmOptionsFileTypesColors.btnSearchTemplateClick(Sender: TObject);
var
  sMask: UTF8String;
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
  iIndex := lbCategories.Items.AddObject('', nil);
  lbCategories.ItemIndex := iIndex;
  edtCategoryName.Text := '';
  edtCategoryMask.Text := '*';
  edtCategoryAttr.Text := '';
  cbCategoryColor.ItemIndex := -1;
end;

procedure TfrmOptionsFileTypesColors.btnApplyCategoryClick(Sender: TObject);
var
  MaskItem : TMaskItem;
  I, iCount : Integer;
begin
  if btnDeleteCategory.Tag = 0 then // if we add or change category
    begin
      lbCategories.Items[lbCategories.ItemIndex] := edtCategoryName.Text;
      MaskItem := TMaskItem.Create;
      MaskItem.sName := edtCategoryName.Text;
      MaskItem.cColor := cbCategoryColor.Selected;
      if edtCategoryMask.Text = '' then
        edtCategoryMask.Text := '*'; // because we load colors from ini by mask
      MaskItem.sExt := edtCategoryMask.Text;
      MaskItem.sModeStr := edtCategoryAttr.Text;
      lbCategories.Items.Objects[lbCategories.ItemIndex] := MaskItem;
      if lbCategories.ItemIndex >= gColorExt.MaskItemList.Count then
        gColorExt.MaskItemList.Add(MaskItem)
      else
        begin
          TMaskItem(gColorExt.MaskItemList.Items[lbCategories.ItemIndex]).Free;
          gColorExt.MaskItemList.Items[lbCategories.ItemIndex] := MaskItem;
        end;

    end
  else  // if we delete category
    begin
      iCount := gColorExt.MaskItemList.Count - 1;
      for I := iCount downto 0 do  // delete old categories
        begin
          gColorExt.MaskItemList.Delete(I);
        end;
      iCount := lbCategories.Count;
      for I := 0 to iCount - 1 do  //write new categories
        begin
          gColorExt.MaskItemList.Add(lbCategories.Items.Objects[I]);
        end;
    end; // delete category

    btnDeleteCategory.Tag := 0;
end;

procedure TfrmOptionsFileTypesColors.btnDeleteCategoryClick(Sender: TObject);
begin
  lbCategories.Items.Delete(lbCategories.ItemIndex);
  btnDeleteCategory.Tag := 1; // show that we delete category
  if lbCategories.Count > 0 then
    lbCategories.ItemIndex := 0;
  lbCategoriesClick(lbCategories);
end;

procedure TfrmOptionsFileTypesColors.btnCategoryColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbCategoryColor.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbCategoryColor, optColorDialog.Color);
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

     Canvas.TextOut(ARect.Left+2,ARect.Top+1,Items[Index]);
   end;
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
  sCategoryName : String;
  I : Integer;
begin
  lbCategories.Clear;

  { File lbtypes category color }
  for I := 0 to gColorExt.MaskItemList.Count - 1 do
    begin
      sCategoryName := TMaskItem(gColorExt.MaskItemList.Items[I]).sName;
      lbCategories.Items.AddObject(sCategoryName,TMaskItem(gColorExt.MaskItemList.Items[I]));
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
begin
  Result := [];
end;

end.

