{
   Double Commander
   -------------------------------------------------------------------------
   File types colors options page

   Copyright (C) 2006-2025 Alexander Koblov (alexx2000@mail.ru)

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

unit fOptionsFileTypesColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, KASComboBox, Dialogs, Buttons,
  LMessages, fOptionsFrame;

type

  { TfrmOptionsFileTypesColors }

  TfrmOptionsFileTypesColors = class(TOptionsEditor)
    btnAddCategory: TBitBtn;
    btnDeleteCategory: TBitBtn;
    btnSearchTemplate: TBitBtn;
    cbCategoryColor: TKASColorBoxButton;
    edtCategoryAttr: TEdit;
    edtCategoryMask: TEdit;
    edtCategoryName: TEdit;
    gbFileTypesColors: TGroupBox;
    lbCategories: TListBox;
    lblCategoryAttr: TLabel;
    lblCategoryColor: TLabel;
    lblCategoryMask: TLabel;
    lblCategoryName: TLabel;

    procedure cbCategoryColorChange(Sender: TObject);
    procedure edtCategoryAttrChange(Sender: TObject);
    procedure edtCategoryMaskChange(Sender: TObject);
    procedure edtCategoryNameChange(Sender: TObject);
    procedure btnSearchTemplateClick(Sender: TObject);
    procedure btnAddCategoryClick(Sender: TObject);
    procedure btnDeleteCategoryClick(Sender: TObject);
    procedure lbCategoriesDragDrop(Sender, {%H-}Source: TObject; {%H-}X, Y: Integer);
    procedure lbCategoriesDragOver(Sender, Source: TObject; {%H-}X, {%H-}Y: Integer;
      {%H-}State: TDragState; var Accept: Boolean);
    procedure lbCategoriesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure lbCategoriesSelectionChange(Sender: TObject; User: boolean);

    procedure Clear;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
    procedure CMThemeChanged(var Message: TLMessage); message CM_THEMECHANGED;
  public
    destructor Destroy; override;
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    function IsSignatureComputedFromAllWindowComponents: Boolean; override;
    function ExtraOptionsSignature(CurrentSignature: dword): dword; override;
  end;

implementation

{$R *.lfm}

uses
  Graphics, CRC, uLng, uGlobs, uColorExt, fMaskInputDlg, uSearchTemplate;

{ TfrmOptionsFileTypesColors }

procedure TfrmOptionsFileTypesColors.edtCategoryNameChange(Sender: TObject);
begin
  if lbCategories.ItemIndex < 0 then Exit;
  lbCategories.Items[lbCategories.ItemIndex]:= edtCategoryName.Text;
  TMaskItem(lbCategories.Items.Objects[lbCategories.ItemIndex]).sName:= edtCategoryName.Text;
end;

procedure TfrmOptionsFileTypesColors.edtCategoryMaskChange(Sender: TObject);
begin
  if lbCategories.ItemIndex < 0 then Exit;
  TMaskItem(lbCategories.Items.Objects[lbCategories.ItemIndex]).sExt:= edtCategoryMask.Text;
end;

procedure TfrmOptionsFileTypesColors.edtCategoryAttrChange(Sender: TObject);
begin
  if lbCategories.ItemIndex < 0 then Exit;
  TMaskItem(lbCategories.Items.Objects[lbCategories.ItemIndex]).sModeStr:= edtCategoryAttr.Text;
end;

procedure TfrmOptionsFileTypesColors.cbCategoryColorChange(Sender: TObject);
begin
  if lbCategories.ItemIndex < 0 then Exit;
  TMaskItem(lbCategories.Items.Objects[lbCategories.ItemIndex]).cColor:= cbCategoryColor.Selected;
end;

procedure TfrmOptionsFileTypesColors.btnSearchTemplateClick(Sender: TObject);
var
  sMask: String;
  bTemplate: Boolean;
begin
  sMask:= edtCategoryMask.Text;
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
  MaskItem := TMaskItem.Create;
  try
    MaskItem.sName:= rsOptionsEditorFileNewFileTypes;
    MaskItem.sExt:= AllFilesMask;
    MaskItem.sModeStr:= EmptyStr;
    MaskItem.cColor:= gColors.FilePanel^.ForeColor;

    iIndex:= lbCategories.Items.AddObject(MaskItem.sName, MaskItem);

  except
    FreeAndNil(MaskItem);
    raise;
  end;
  lbCategories.ItemIndex:= iIndex;
  edtCategoryName.SetFocus;
end;

procedure TfrmOptionsFileTypesColors.btnDeleteCategoryClick(Sender: TObject);
begin
  if (lbCategories.ItemIndex <> -1) then
  begin
    lbCategories.Items.Objects[lbCategories.ItemIndex].Free;
    lbCategories.Items.Delete(lbCategories.ItemIndex);

    if lbCategories.Count > 0 then
      lbCategories.ItemIndex:= 0
    else begin
      lbCategoriesSelectionChange(lbCategories, True);
    end;
  end;
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
  with (Control as TListBox), gColors.FilePanel^ do
   begin
     if (not Selected[Index]) and Assigned(Items.Objects[Index]) then
       begin
         Canvas.Brush.Color:= BackColor;
         Canvas.Font.Color:= TMaskItem(Items.Objects[Index]).cColor;
       end
     else
       begin
         Canvas.Brush.Color:= CursorColor;
         Canvas.Font.Color:= CursorText;
       end;

     Canvas.FillRect(ARect);
     Canvas.TextOut(ARect.Left+2,ARect.Top,Items[Index]);
   end;
end;

procedure TfrmOptionsFileTypesColors.lbCategoriesSelectionChange(
  Sender: TObject; User: boolean);
var
  Index: Integer;
  bEnabled: Boolean;
  MaskItem: TMaskItem;
begin
  Index:= lbCategories.ItemIndex;

  if (Index <> -1) then
  begin
    MaskItem := TMaskItem(lbCategories.Items.Objects[Index]);

    edtCategoryName.Text := MaskItem.sName;
    edtCategoryMask.Text := MaskItem.sExt;
    cbCategoryColor.Selected := MaskItem.cColor;
    bEnabled:= (MaskItem.sExt = '') or (MaskItem.sExt[1] <> '>');
    edtCategoryAttr.Text := MaskItem.sModeStr;
  end
  else begin
    bEnabled := False;
    edtCategoryName.Text := '';
    edtCategoryMask.Text := '';
    edtCategoryAttr.Text := '';
    cbCategoryColor.Selected := gColors.FilePanel^.ForeColor;
  end;

  edtCategoryMask.Enabled:= bEnabled;
  edtCategoryAttr.Enabled:= bEnabled;
  edtCategoryName.Enabled:= (Index <> -1);
  cbCategoryColor.Enabled:= (Index <> -1);
  btnSearchTemplate.Enabled:= (Index <> -1);
  btnDeleteCategory.Enabled:= (Index <> -1);
end;

procedure TfrmOptionsFileTypesColors.Clear;
var
  I: Integer;
begin
  for I := lbCategories.Count - 1 downto 0 do
    lbCategories.Items.Objects[I].Free;

  lbCategories.Clear;
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

function TfrmOptionsFileTypesColors.IsSignatureComputedFromAllWindowComponents: Boolean;
begin
  Result := False;
end;

function TfrmOptionsFileTypesColors.ExtraOptionsSignature(
  CurrentSignature: dword): dword;
var
  I: Integer;
  AColor: TColor;
  MaskItem: TMaskItem;
begin
  Result:= CurrentSignature;

  for I:= 0 to lbCategories.Count - 1 do
  begin
    if Assigned(lbCategories.Items.Objects[I]) then
    begin
      MaskItem:= TMaskItem(lbCategories.Items.Objects[I]);

      AColor:= MaskItem.cColor;
      Result:= CRC32(Result, @AColor, SizeOf(AColor));
      Result:= CRC32(Result, Pointer(MaskItem.sExt), Length(MaskItem.sExt));
      Result:= CRC32(Result, Pointer(MaskItem.sName), Length(MaskItem.sName));
      Result:= CRC32(Result, Pointer(MaskItem.sModeStr), Length(MaskItem.sModeStr));
    end;
  end;
end;

procedure TfrmOptionsFileTypesColors.Load;
var
  I : Integer;
  MaskItem: TMaskItem;
begin
  Clear;
  lbCategories.Color:= gColors.FilePanel^.BackColor;

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
  begin
    lbCategories.ItemIndex := 0
  end;
  lbCategoriesSelectionChange(lbCategories, True);
end;

function TfrmOptionsFileTypesColors.Save: TOptionsEditorSaveFlags;
var
  I: Integer;
  MaskItem: TMaskItem;
begin
  Result := [];
  gColorExt.Clear;

  for I := 0 to lbCategories.Count - 1 do
  begin
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
end;

procedure TfrmOptionsFileTypesColors.CMThemeChanged(var Message: TLMessage);
begin
  lbCategories.Color:= gColors.FilePanel^.BackColor;
  lbCategories.Repaint;
end;

destructor TfrmOptionsFileTypesColors.Destroy;
begin
  Clear;
  inherited Destroy;
end;

end.

