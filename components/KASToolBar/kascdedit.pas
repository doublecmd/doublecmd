{
    Double Commander
    -------------------------------------------------------------------------
    Custom edit control with the look and feel like TLabel

    Copyright (C) 2017-2018 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit KASCDEdit;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics, Dialogs, Types,
  Menus, CustomDrawnControls, CustomDrawnDrawers, CustomDrawn_Common;

type

  { TKASCDEdit }

  TKASCDEdit = class(TCDEdit)
  private
    FEditMenu: TPopupMenu; static;
  private
    procedure CreatePopupMenu;
    procedure ShowMenu(Data: PtrInt);
    procedure MenuCopy(Sender: TObject);
    procedure MenuSelectAll(Sender: TObject);
  protected
    procedure RealSetText(const Value: TCaption); override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
                                     WithThemeSpace: Boolean); override;
    procedure CalculateSize(MaxWidth: Integer; var NeededWidth, NeededHeight: Integer);
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  public
    procedure SelectAll;
    procedure CopyToClipboard;
  published
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TKASCDDrawer }

  TKASCDDrawer = class(TCDDrawerCommon)
  public
    function GetMeasures(AMeasureID: Integer): Integer; override;
    procedure DrawEditBackground(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    procedure DrawEdit(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
  end;

procedure Register;

implementation

uses
  Math, Forms, Clipbrd, LCLType, LCLIntf, LazUTF8;

resourcestring
  rsMnuCopyToClipboard = 'Copy';
  rsMnuSelectAll = 'Select &All';

procedure Register;
begin
  RegisterComponents('KASComponents', [TKASCDEdit]);
end;

{ TKASCDDrawer }

function TKASCDDrawer.GetMeasures(AMeasureID: Integer): Integer;
begin
  case AMeasureID of
    TCDEDIT_LEFT_TEXT_SPACING: Result := 0;
    TCDEDIT_RIGHT_TEXT_SPACING: Result := 0;
    else Result:= inherited GetMeasures(AMeasureID);
  end;
end;

procedure TKASCDDrawer.DrawEditBackground(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
begin
  // The background
  ADest.Pen.Style := psSolid;
  ADest.Pen.Color := AStateEx.RGBColor;
  ADest.Brush.Style := bsSolid;
  ADest.Brush.Color := AStateEx.RGBColor;
  ADest.Rectangle(0, 0, ASize.cx, ASize.cy);
end;

procedure TKASCDDrawer.DrawEdit(ADest: TCanvas; ASize: TSize;
  AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  lVisibleText, lControlText: TCaption;
  lSelLeftPos, lSelLeftPixelPos, lSelLength, lSelRightPos: Integer;
  lTextWidth, lLineHeight, lLineTop: Integer;
  lControlTextLen: PtrInt;
  lTextLeftSpacing, lTextTopSpacing, lTextBottomSpacing: Integer;
  lTextColor: TColor;
  i, lVisibleLinesCount: Integer;
begin
  // Background
  DrawEditBackground(ADest, Point(0, 0), ASize, AState, AStateEx);

  // General text configurations which apply to all lines
  // Configure the text color
  if csfEnabled in AState then
    lTextColor := AStateEx.Font.Color
  else
    lTextColor := clGrayText;

  ADest.Brush.Style := bsClear;
  ADest.Font.Assign(AStateEx.Font);
  ADest.Font.Color := lTextColor;
  lTextLeftSpacing := GetMeasures(TCDEDIT_LEFT_TEXT_SPACING);
  //lTextRightSpacing := GetMeasures(TCDEDIT_RIGHT_TEXT_SPACING);
  lTextTopSpacing := GetMeasures(TCDEDIT_TOP_TEXT_SPACING);
  lTextBottomSpacing := GetMeasures(TCDEDIT_BOTTOM_TEXT_SPACING);

  lLineHeight := ADest.TextHeight(cddTestStr)+2;
  lLineHeight := Min(ASize.cy-lTextBottomSpacing, lLineHeight);

  // Fill this to be used in other parts
  AStateEx.LineHeight := lLineHeight;
  AStateEx.FullyVisibleLinesCount := ASize.cy - lTextTopSpacing - lTextBottomSpacing;
  AStateEx.FullyVisibleLinesCount := AStateEx.FullyVisibleLinesCount div lLineHeight;
  AStateEx.FullyVisibleLinesCount := Min(AStateEx.FullyVisibleLinesCount, AStateEx.Lines.Count);

  // Calculate how many lines to draw
  if AStateEx.Multiline then
    lVisibleLinesCount := AStateEx.FullyVisibleLinesCount + 1
  else
    lVisibleLinesCount := 1;
  lVisibleLinesCount := Min(lVisibleLinesCount, AStateEx.Lines.Count);

  // Now draw each line
  for i := 0 to lVisibleLinesCount - 1 do
  begin
    lControlText := AStateEx.Lines.Strings[AStateEx.VisibleTextStart.Y+i];
    lControlText :=  VisibleText(lControlText, AStateEx.PasswordChar);
    lControlTextLen := UTF8Length(lControlText);
    lLineTop := lTextTopSpacing + i * lLineHeight;

    // The text
    ADest.Pen.Style := psClear;
    ADest.Brush.Style := bsClear;
    // ToDo: Implement multi-line selection
    if (AStateEx.SelLength = 0) or (AStateEx.SelStart.Y <> AStateEx.VisibleTextStart.Y+i) then
    begin
      lVisibleText := UTF8Copy(lControlText, AStateEx.VisibleTextStart.X, lControlTextLen);
      ADest.TextOut(lTextLeftSpacing, lLineTop, lVisibleText);
    end
    // Text and Selection
    else
    begin
      lSelLeftPos := AStateEx.SelStart.X;
      if AStateEx.SelLength < 0 then lSelLeftPos := lSelLeftPos + AStateEx.SelLength;

      lSelRightPos := AStateEx.SelStart.X;
      if AStateEx.SelLength > 0 then lSelRightPos := lSelRightPos + AStateEx.SelLength;

      lSelLength := AStateEx.SelLength;
      if lSelLength < 0 then lSelLength := lSelLength * -1;

      // Text left of the selection
      lVisibleText := UTF8Copy(lControlText, AStateEx.VisibleTextStart.X, lSelLeftPos-AStateEx.VisibleTextStart.X+1);
      ADest.TextOut(lTextLeftSpacing, lLineTop, lVisibleText);
      lSelLeftPixelPos := ADest.TextWidth(lVisibleText)+lTextLeftSpacing;

      // The selection background
      lVisibleText := UTF8Copy(lControlText, lSelLeftPos+1, lSelLength);
      lTextWidth := ADest.TextWidth(lVisibleText);
      ADest.Brush.Color := clHighlight;
      ADest.Brush.Style := bsSolid;
      ADest.Rectangle(Bounds(lSelLeftPixelPos, lLineTop, lTextWidth, lLineHeight));
      ADest.Brush.Style := bsClear;

      // The selection text
      ADest.Font.Color := clHighlightText;
      ADest.TextOut(lSelLeftPixelPos, lLineTop, lVisibleText);
      lSelLeftPixelPos := lSelLeftPixelPos + lTextWidth;

      // Text right of the selection
      ADest.Brush.Color := AStateEx.RGBColor;
      ADest.Font.Color := lTextColor;
      lVisibleText := UTF8Copy(lControlText, lSelLeftPos+lSelLength+1, lControlTextLen);
      ADest.TextOut(lSelLeftPixelPos, lLineTop, lVisibleText);
    end;
  end;

  // And the caret
  DrawCaret(ADest, Point(0, 0), ASize, AState, AStateEx);
end;

{ TKASCDEdit }

procedure TKASCDEdit.CreatePopupMenu;
var
  MenuItem: TMenuItem;
begin
  if not Assigned(FEditMenu) then
  begin
    FEditMenu:= TPopupMenu.Create(Application);
    MenuItem:= TMenuItem.Create(FEditMenu);
    MenuItem.Caption:= rsMnuCopyToClipboard;
    MenuItem.OnClick:= MenuCopy;
    FEditMenu.Items.Add(MenuItem);
    MenuItem:= TMenuItem.Create(FEditMenu);
    MenuItem.Caption:= '-';
    FEditMenu.Items.Add(MenuItem);
    MenuItem:= TMenuItem.Create(FEditMenu);
    MenuItem.Caption:= rsMnuSelectAll;
    MenuItem.OnClick:= MenuSelectAll;
    FEditMenu.Items.Add(MenuItem);
  end;
end;

procedure TKASCDEdit.ShowMenu(Data: PtrInt);
begin
  FEditMenu.Tag:= Data;
  FEditMenu.PopUp;
end;

procedure TKASCDEdit.MenuCopy(Sender: TObject);
begin
  TKASCDEdit(TMenuItem(Sender).Owner.Tag).CopyToClipboard;
end;

procedure TKASCDEdit.MenuSelectAll(Sender: TObject);
begin
  TKASCDEdit(TMenuItem(Sender).Owner.Tag).SelectAll;
end;

procedure TKASCDEdit.RealSetText(const Value: TCaption);
begin
  Lines.Text := Value;
  inherited RealSetText(Value);
end;

procedure TKASCDEdit.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: Integer; WithThemeSpace: Boolean);
var
  AWidth: Integer;
begin
  if (Parent = nil) or (not Parent.HandleAllocated) then Exit;
  AWidth := Constraints.MinMaxWidth(10000);
  CalculateSize(AWidth, PreferredWidth, PreferredHeight);
end;

procedure TKASCDEdit.CalculateSize(MaxWidth: Integer; var NeededWidth,
  NeededHeight: Integer);
var
  DC: HDC;
  R: TRect;
  Flags: Cardinal;
  OldFont: HGDIOBJ;
  LabelText: String;
  lTextLeftSpacing, lTextTopSpacing,
  lTextBottomSpacing, lTextRightSpacing: Integer;
begin
  LabelText := Text;

  if LabelText = '' then
  begin
    NeededWidth:= 1;
    NeededHeight:= 1;
    Exit;
  end;

  lTextLeftSpacing := FDrawer.GetMeasures(TCDEDIT_LEFT_TEXT_SPACING);
  lTextTopSpacing := FDrawer.GetMeasures(TCDEDIT_TOP_TEXT_SPACING);
  lTextRightSpacing := FDrawer.GetMeasures(TCDEDIT_RIGHT_TEXT_SPACING);
  lTextBottomSpacing := FDrawer.GetMeasures(TCDEDIT_BOTTOM_TEXT_SPACING);

  DC := GetDC(Parent.Handle);
  try
    R := Rect(0, 0, MaxWidth, 10000);
    OldFont := SelectObject(DC, HGDIOBJ(Font.Reference.Handle));
    Flags := DT_CALCRECT or DT_EXPANDTABS;
    if not MultiLine then Flags := Flags or DT_SINGLELINE;
    DrawText(DC, PAnsiChar(LabelText), Length(LabelText), R, Flags);
    SelectObject(DC, OldFont);
    NeededWidth := R.Right - R.Left + lTextLeftSpacing + lTextRightSpacing;
    NeededHeight := R.Bottom - R.Top + lTextTopSpacing + lTextBottomSpacing;
  finally
    ReleaseDC(Parent.Handle, DC);
  end;
end;

procedure TKASCDEdit.KeyDown(var Key: word; Shift: TShiftState);
begin
  if (ssModifier in Shift) then
  begin
    case Key of
      VK_A:
        begin
          SelectAll;
          Key:= 0;
        end;
      VK_C:
        begin
          CopyToClipboard;
          Key:= 0;
        end;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

constructor TKASCDEdit.Create(AOwner: TComponent);
begin
  CreatePopupMenu;
  inherited Create(AOwner);
  Color:= clForm;
  ReadOnly:= True;
  Cursor:= crIBeam;
  DrawStyle:= dsExtra1;
end;

procedure TKASCDEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) or (GetSelLength = 0) then
    inherited MouseDown(Button, Shift, X, Y)
  else if Assigned(OnMouseDown) then begin
    OnMouseDown(Self, Button, Shift, X, Y);
  end;
end;

procedure TKASCDEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbRight then begin
    Application.QueueAsyncCall(ShowMenu, PtrInt(Self));
  end;
end;

procedure TKASCDEdit.SelectAll;
begin
  FEditState.SelStart.X:= 0;
  FEditState.SelLength:= UTF8Length(Text);
  Invalidate;
end;

procedure TKASCDEdit.CopyToClipboard;
begin
  if (FEditState.SelLength >= 0) then
    Clipboard.AsText:= UTF8Copy(Text, FEditState.SelStart.X + 1, FEditState.SelLength)
  else begin
    Clipboard.AsText:= UTF8Copy(Text, FEditState.SelStart.X + FEditState.SelLength + 1, -FEditState.SelLength);
  end;
end;

initialization
  RegisterDrawer(TKASCDDrawer.Create, dsExtra1);

end.
