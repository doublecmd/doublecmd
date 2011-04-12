{
   Double Commander
   -------------------------------------------------------------------------
   Colors options page

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

unit fOptionsColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, ColorBox,
  Graphics, ExtCtrls, Dialogs, Buttons,
  fOptionsFrame;

type

  { TfrmOptionsColors }

  TfrmOptionsColors = class(TOptionsEditor)
    optColorDialog: TColorDialog;
    pcColors: TPageControl;
    tsFileTypes: TTabSheet;
    tsFilePanels: TTabSheet;
    btnBackColor: TButton;
    btnBackColor2: TButton;
    btnCursorColor: TButton;
    btnCursorText: TButton;
    btnForeColor: TButton;
    btnMarkColor: TButton;
    cBackGrndLabel: TLabel;
    cbBackColor: TColorBox;
    cbBackColor2: TColorBox;
    cbbUseFrameCursor: TCheckBox;
    cbbUseInvertedSelection: TCheckBox;
    cbCursorColor: TColorBox;
    cbCursorText: TColorBox;
    cbMarkColor: TColorBox;
    cbTextColor: TColorBox;
    cTextLabel: TLabel;
    gbExample: TGroupBox;
    lblBackground2: TLabel;
    lblCursorColor: TLabel;
    lblCursorText: TLabel;
    lblInactivePanelBrightness: TLabel;
    lblMarkColor: TLabel;
    pbExample: TPaintBox;
    tbInactivePanelBrightness: TTrackBar;
    bbtnAddCategory: TBitBtn;
    bbtnApplyCategory: TBitBtn;
    bbtnDeleteCategory: TBitBtn;
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
    procedure cbColorBoxChange(Sender: TObject);
    procedure btnForeColorClick(Sender: TObject);
    procedure btnBackColorClick(Sender: TObject);
    procedure cbbUseFrameCursorChange(Sender: TObject);
    procedure cbbUseInvertedSelectionChange(Sender: TObject);
    procedure pbExamplePaint(Sender: TObject);
    procedure btnBackColor2Click(Sender: TObject);
    procedure btnCursorColorClick(Sender: TObject);
    procedure btnCursorTextClick(Sender: TObject);
    procedure btnMarkColorClick(Sender: TObject);
    procedure lbCategoriesClick(Sender: TObject);
    procedure btnSearchTemplateClick(Sender: TObject);
    procedure bbtnAddCategoryClick(Sender: TObject);
    procedure bbtnApplyCategoryClick(Sender: TObject);
    procedure bbtnDeleteCategoryClick(Sender: TObject);
    procedure btnCategoryColorClick(Sender: TObject);
    procedure lbCategoriesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    procedure SetColorInColorBox(const lcbColorBox:TColorBox;const lColor:TColor);
  protected
    procedure Init; override;
    procedure Done; override;
  public
    procedure Load; override;
    procedure Save; override;
  end; 

implementation

{$R *.lfm}

uses
  uLng, uGlobs, uColorExt, fMaskInputDlg, uSearchTemplate;

{ TfrmOptionsColors }

procedure TfrmOptionsColors.cbColorBoxChange(Sender: TObject);
begin
   pbExample.Repaint;
end;

procedure TfrmOptionsColors.btnForeColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbTextColor.Color;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbTextColor,optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptionsColors.btnBackColorClick(Sender: TObject);
begin
   optColorDialog.Color:= cbBackColor.Color;
   if optColorDialog.Execute then
   begin
    SetColorInColorBox(cbBackColor,optColorDialog.Color);
    pbExample.Repaint;
   end;
end;

procedure TfrmOptionsColors.cbbUseFrameCursorChange(Sender: TObject);
begin
  pbExample.Repaint;
end;

procedure TfrmOptionsColors.cbbUseInvertedSelectionChange(Sender: TObject);
begin
  pbExample.Repaint;
end;

procedure TfrmOptionsColors.pbExamplePaint(Sender: TObject);
var
  h, I : integer;
  sText : String;
  iTextTop, iTextLeft : Integer;
  Rect : TRect;
begin
  h := pbExample.Height div 6;
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := pbExample.Width;
  Rect.Bottom := h;

  for I := 1 to 6 do
  with pbExample.Canvas do
  begin
    case I of
    1:
      begin
       if  cbbUseInvertedSelection.Checked then
         begin
            Brush.Color := cbMarkColor.Selected;
            Font.Color := cbTextColor.Selected;
            sText := rsOptExampleMark;
         end
       else
         begin
            Brush.Color := cbBackColor.Selected;
            Font.Color := cbMarkColor.Selected;
            sText := rsOptExampleMark;
         end;
      end;
    2:
      begin
       if  cbbUseInvertedSelection.Checked then
         begin
            Brush.Color := cbMarkColor.Selected;
            Font.Color := cbTextColor.Selected;
            sText := rsOptExampleMark;
         end
       else
         begin
            Brush.Color := cbBackColor2.Selected;
            Font.Color := cbMarkColor.Selected;
            sText := rsOptExampleMark;
         end;
      end;
    3:
      begin
        Brush.Color := cbBackColor.Selected;
        Font.Color := cbTextColor.Selected;
        sText := rsOptExampleText;
      end;
    4:
      begin
        Brush.Color := cbBackColor2.Selected;
        Font.Color := cbTextColor.Selected;
        sText := rsOptExampleText;
      end;
    5:
      begin
        if cbbUseFrameCursor.Checked then
         begin
          Pen.Color := cbCursorColor.Selected;
          Brush.Color := cbBackColor.Selected;
         end
          else  Brush.Color := cbCursorColor.Selected;

        Font.Color := cbCursorText.Selected;
        sText := rsOptExampleCursor;
      end;
    6:
      begin
       if  cbbUseInvertedSelection.Checked then
         begin
           if cbbUseFrameCursor.Checked then
              begin
                Pen.Color := cbCursorColor.Selected;
                Brush.Color := cbBackColor.Selected;
                Font.Color := cbMarkColor.Selected;
              end
           else
             begin
               Brush.Color := cbCursorColor.Selected;
               Font.Color :=InvertColor(cbCursorText.Selected);
             end;
            sText := rsOptExampleMarkCursor;
         end
       else
         begin
           if cbbUseFrameCursor.Checked then
              begin
                Pen.Color := cbCursorColor.Selected;
                Brush.Color := cbBackColor.Selected;
                Font.Color := cbMarkColor.Selected;
              end
           else
             begin
               Brush.Color := cbCursorColor.Selected;
               Font.Color := cbMarkColor.Selected;
             end;
            sText := rsOptExampleMarkCursor;
         end;
      end;
    end; // case

    with gFonts[dcfMain] do
    begin
      Font.Style := Style;
      Font.Size  := Size;
      Font.Name  := Name;
    end;

    iTextTop := Rect.Top + (h div 2) - (TextHeight(sText) div 2);
    iTextLeft := Rect.Left + (pbExample.Width div 2) - (TextWidth(sText) div 2);
    FillRect(Rect);
    if (cbbUseFrameCursor.Checked) and ((i=5) or (i=6)) then
      begin
         Pen.Color:=cbCursorColor.Selected;
         Line(Rect.Left, Rect.Top, Rect.Right, Rect.Top);
         Line(Rect.Left, Rect.Bottom - 1, Rect.Right, Rect.Bottom - 1);
      end;
    TextOut(iTextLeft, iTextTop, sText);
    Rect.Top := Rect.Bottom;
    Rect.Bottom := h * (I + 1);
  end; // for
end;

procedure TfrmOptionsColors.btnBackColor2Click(Sender: TObject);
begin
  optColorDialog.Color:= cbBackColor2.Color;
  if optColorDialog.Execute then
   begin
    SetColorInColorBox(cbBackColor2, optColorDialog.Color);
    pbExample.Repaint;
   end;
end;

procedure TfrmOptionsColors.btnCursorColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbCursorColor.Color;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbCursorColor, optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptionsColors.btnCursorTextClick(Sender: TObject);
begin
  optColorDialog.Color:= cbCursorText.Color;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbCursorText, optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptionsColors.btnMarkColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbMarkColor.Color;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbMarkColor, optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptionsColors.lbCategoriesClick(Sender: TObject);
var
  MaskItem : TMaskItem;
  bEnabled: Boolean;
begin
  if (lbCategories.Count > 0) and (Assigned(lbCategories.Items.Objects[lbCategories.ItemIndex])) then
    begin
      edtCategoryName.Text := lbCategories.Items[lbCategories.ItemIndex];
      MaskItem := TMaskItem(lbCategories.Items.Objects[lbCategories.ItemIndex]);

      edtCategoryMask.Text := MaskItem.sExt;
      SetColorInColorBox(cbCategoryColor,MaskItem.cColor);
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
      cbCategoryColor.Color := clWindow;
      cbCategoryColor.Selected := cbCategoryColor.Color;
    end;
end;

procedure TfrmOptionsColors.btnSearchTemplateClick(Sender: TObject);
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

procedure TfrmOptionsColors.bbtnAddCategoryClick(Sender: TObject);
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
      bbtnDeleteCategory.Enabled := True;
      bbtnApplyCategory.Enabled := True;
    end;
  iIndex := lbCategories.Items.AddObject('', nil);
  lbCategories.ItemIndex := iIndex;
  edtCategoryName.Text := '';
  edtCategoryMask.Text := '*';
  edtCategoryAttr.Text := '';
  cbCategoryColor.ItemIndex := -1;
  cbCategoryColor.Color := clWindow;
end;

procedure TfrmOptionsColors.bbtnApplyCategoryClick(Sender: TObject);
var
  MaskItem : TMaskItem;
  I, iCount : Integer;
begin
  if bbtnDeleteCategory.Tag = 0 then // if we add or change category
    begin
      lbCategories.Items[lbCategories.ItemIndex] := edtCategoryName.Text;
      MaskItem := TMaskItem.Create;
      MaskItem.sName := edtCategoryName.Text;
      MaskItem.cColor := cbCategoryColor.Color;
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

    bbtnDeleteCategory.Tag := 0;
end;

procedure TfrmOptionsColors.bbtnDeleteCategoryClick(Sender: TObject);
begin
  lbCategories.Items.Delete(lbCategories.ItemIndex);
  bbtnDeleteCategory.Tag := 1; // show that we delete category
  if lbCategories.Count > 0 then
    lbCategories.ItemIndex := 0;
  lbCategoriesClick(lbCategories);
end;

procedure TfrmOptionsColors.btnCategoryColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbCategoryColor.Color;
  if optColorDialog.Execute then
    SetColorInColorBox(cbCategoryColor, optColorDialog.Color);
end;

procedure TfrmOptionsColors.lbCategoriesDrawItem(Control: TWinControl;
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

procedure TfrmOptionsColors.SetColorInColorBox(const lcbColorBox: TColorBox;
  const lColor: TColor);
//< select in lcbColorBox lColor if lColor in lcbColorBox else
// add to lcbColorBox lColor and select him
var
  i: LongInt;
begin
  if (lcbColorBox = nil) then Exit; // if lcbColorBox not exist

  with lcbColorBox do
  begin
    // search lColor in colorbox colorlist
    for i:= 0 to Items.Count - 1 do
      if Colors[i] = lColor then // find color
      begin
        // select color
        Selected:= lColor;
        Exit;
      end;//  if for

    //add items to colorbox list
    Items.Objects[Items.Add('$'+HexStr(lColor,8))]:= TObject(PtrInt(lColor));
    Selected:= lColor;
  end; // with
end;

procedure TfrmOptionsColors.Init;
var
  sCategoryName : String;
  I : Integer;
begin
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
        bbtnDeleteCategory.Enabled := False;
        bbtnApplyCategory.Enabled := False;
      end;
  lbCategoriesClick(lbCategories);
end;

procedure TfrmOptionsColors.Done;
begin

end;

procedure TfrmOptionsColors.Load;
begin
  SetColorInColorBox(cbTextColor,gForeColor);
  SetColorInColorBox(cbBackColor,gBackColor);
  SetColorInColorBox(cbBackColor2,gBackColor2);
  SetColorInColorBox(cbMarkColor,gMarkColor);
  SetColorInColorBox(cbCursorColor,gCursorColor);
  SetColorInColorBox(cbCursorText,gCursorText);
  cbbUseInvertedSelection.Checked:=gUseInvertedSelection;
  cbbUseFrameCursor.Checked:=gUseFrameCursor;
  tbInactivePanelBrightness.Position:=gInactivePanelBrightness;
end;

procedure TfrmOptionsColors.Save;
begin
  gForeColor := cbTextColor.Color;
  gBackColor := cbBackColor.Color; // background color
  gBackColor2 := cbBackColor2.Color;
  gMarkColor := cbMarkColor.Color;
  gCursorColor := cbCursorColor.Color;
  gCursorText := cbCursorText.Color;
  gUseInvertedSelection:=cbbUseInvertedSelection.Checked;
  gInactivePanelBrightness:=tbInactivePanelBrightness.Position;
  gUseFrameCursor:=cbbUseFrameCursor.Checked;
end;

initialization
  RegisterOptionsEditor(4, TfrmOptionsColors);

end.

