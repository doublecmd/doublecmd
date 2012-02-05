{
   Double Commander
   -------------------------------------------------------------------------
   File panels colors options page

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

unit fOptionsFilePanelsColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, StdCtrls, ColorBox, ExtCtrls, Dialogs,
  DividerBevel, fOptionsFrame;

type

  { TfrmOptionsFilePanelsColors }

  TfrmOptionsFilePanelsColors = class(TOptionsEditor)
    btnBackColor: TButton;
    btnBackColor2: TButton;
    btnCursorColor: TButton;
    btnCursorText: TButton;
    btnForeColor: TButton;
    btnMarkColor: TButton;
    btnIndColor: TButton;
    btnIndBackColor: TButton;
    cbBackColor: TColorBox;
    cbBackColor2: TColorBox;
    cbbUseFrameCursor: TCheckBox;
    cbbUseInvertedSelection: TCheckBox;
    cbCursorColor: TColorBox;
    cbCursorText: TColorBox;
    cbMarkColor: TColorBox;
    cbTextColor: TColorBox;
    cbIndColor: TColorBox;
    cbIndBackColor: TColorBox;
    cbbUseGradientInd: TCheckBox;
    dbFreeSpaceIndicator: TDividerBevel;
    dbOptions: TDividerBevel;
    gbExample: TGroupBox;
    lblIndBackColor: TLabel;
    lblIndColor: TLabel;
    lblBackgroundColor: TLabel;
    lblBackgroundColor2: TLabel;
    lblCursorColor: TLabel;
    lblCursorText: TLabel;
    lblInactivePanelBrightness: TLabel;
    lblMarkColor: TLabel;
    lblTextColor: TLabel;
    optColorDialog: TColorDialog;
    pbExample: TPaintBox;
    tbInactivePanelBrightness: TTrackBar;
    procedure btnBackColor2Click(Sender: TObject);
    procedure btnBackColorClick(Sender: TObject);
    procedure btnCursorColorClick(Sender: TObject);
    procedure btnCursorTextClick(Sender: TObject);
    procedure btnForeColorClick(Sender: TObject);
    procedure btnIndBackColorClick(Sender: TObject);
    procedure btnIndColorClick(Sender: TObject);
    procedure btnMarkColorClick(Sender: TObject);
    procedure cbbUseGradientIndChange(Sender: TObject);
    procedure cbColorBoxChange(Sender: TObject);
    procedure pbExamplePaint(Sender: TObject);
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
  Graphics, Types, uLng, uGlobs, uDCUtils;

{ TfrmOptionsFilePanelsColors }

class function TfrmOptionsFilePanelsColors.GetIconIndex: Integer;
begin
  Result := 20;
end;

class function TfrmOptionsFilePanelsColors.GetTitle: String;
begin
  Result := rsOptionsEditorFilePanels;
end;

procedure TfrmOptionsFilePanelsColors.Load;
begin
  SetColorInColorBox(cbTextColor, gForeColor);
  SetColorInColorBox(cbBackColor, gBackColor);
  SetColorInColorBox(cbBackColor2, gBackColor2);
  SetColorInColorBox(cbMarkColor, gMarkColor);
  SetColorInColorBox(cbCursorColor, gCursorColor);
  SetColorInColorBox(cbCursorText, gCursorText);
  SetColorInColorBox(cbIndColor, gIndForeColor);
  SetColorInColorBox(cbIndBackColor, gIndBackColor);
  cbbUseInvertedSelection.Checked:= gUseInvertedSelection;
  cbbUseFrameCursor.Checked:= gUseFrameCursor;
  cbbUseGradientInd.Checked := gIndUseGradient;
  tbInactivePanelBrightness.Position:= gInactivePanelBrightness;
end;

function TfrmOptionsFilePanelsColors.Save: TOptionsEditorSaveFlags;
begin
  gForeColor := cbTextColor.Selected;
  gBackColor := cbBackColor.Selected; // background color
  gBackColor2 := cbBackColor2.Selected;
  gMarkColor := cbMarkColor.Selected;
  gCursorColor := cbCursorColor.Selected;
  gCursorText := cbCursorText.Selected;
  gIndForeColor :=  cbIndColor.Selected;
  gIndBackColor := cbIndBackColor.Selected;
  gUseInvertedSelection := cbbUseInvertedSelection.Checked;
  gInactivePanelBrightness := tbInactivePanelBrightness.Position;
  gUseFrameCursor := cbbUseFrameCursor.Checked;
  gIndUseGradient := cbbUseGradientInd.Checked;
  Result := [];
end;

procedure TfrmOptionsFilePanelsColors.btnBackColor2Click(Sender: TObject);
begin
  optColorDialog.Color:= cbBackColor2.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbBackColor2, optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptionsFilePanelsColors.btnBackColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbBackColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbBackColor, optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptionsFilePanelsColors.btnCursorColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbCursorColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbCursorColor, optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptionsFilePanelsColors.btnCursorTextClick(Sender: TObject);
begin
  optColorDialog.Color:= cbCursorText.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbCursorText, optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptionsFilePanelsColors.btnForeColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbTextColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbTextColor, optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptionsFilePanelsColors.btnIndBackColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbIndBackColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbIndBackColor, optColorDialog.Color);
    //pbExample.Repaint;
  end;
end;

procedure TfrmOptionsFilePanelsColors.btnIndColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbIndColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbIndColor, optColorDialog.Color);
    //pbExample.Repaint;
  end;
end;

procedure TfrmOptionsFilePanelsColors.btnMarkColorClick(Sender: TObject);
begin
  optColorDialog.Color:= cbMarkColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbMarkColor, optColorDialog.Color);
    pbExample.Repaint;
  end;
end;

procedure TfrmOptionsFilePanelsColors.cbbUseGradientIndChange(Sender: TObject);
begin
  lblIndColor.Enabled     := not(cbbUseGradientInd.Checked);
  lblIndBackColor.Enabled := not(cbbUseGradientInd.Checked);
  cbIndColor.Enabled      := not(cbbUseGradientInd.Checked);
  cbIndBackColor.Enabled  := not(cbbUseGradientInd.Checked);
  btnIndColor.Enabled     := not(cbbUseGradientInd.Checked);
  btnIndBackColor.Enabled := not(cbbUseGradientInd.Checked);
end;

procedure TfrmOptionsFilePanelsColors.cbColorBoxChange(Sender: TObject);
begin
  pbExample.Repaint;
end;

procedure TfrmOptionsFilePanelsColors.pbExamplePaint(Sender: TObject);
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

procedure TfrmOptionsFilePanelsColors.Init;
  procedure CalcMaxSize(var MaxSize: TSize; Text: String);
  var
    TextSize: TSize;
  begin
    TextSize := Canvas.TextExtent(Text);
    if TextSize.cx > MaxSize.cx then
      MaxSize.cx := TextSize.cx;
    if TextSize.cy > MaxSize.cy then
      MaxSize.cy := TextSize.cy;
  end;
var
  TextSize: TSize = (cx: 0; cy: 0);
begin
  with gFonts[dcfMain] do
  begin
    Canvas.Font.Style := Style;
    Canvas.Font.Size  := Size;
    Canvas.Font.Name  := Name;
  end;

  CalcMaxSize(TextSize, rsOptExampleMark);
  CalcMaxSize(TextSize, rsOptExampleText);
  CalcMaxSize(TextSize, rsOptExampleCursor);
  CalcMaxSize(TextSize, rsOptExampleMarkCursor);

  pbExample.Constraints.MinWidth  := TextSize.cx + 20;
  pbExample.Constraints.MinHeight := (TextSize.cy + 4) * 6;
end;

end.

