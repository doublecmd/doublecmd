unit KASCDEdit;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Types,
  CustomDrawnControls, CustomDrawnDrawers, CustomDrawn_Common;

type

  { TKASCDEdit }

  TKASCDEdit = class(TCDEdit)
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
                                     WithThemeSpace: Boolean); override;
    procedure CalculateSize(MaxWidth: Integer; var NeededWidth, NeededHeight: Integer);
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TKASCDDrawer }

  TKASCDDrawer = class(TCDDrawerCommon)
  public
    procedure DrawEdit(ADest: TCanvas; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
  end;

procedure Register;

implementation

uses
  Math, Clipbrd, LCLType, LCLIntf, LazUTF8;

procedure Register;
begin
  RegisterComponents('KASComponents', [TKASCDEdit]);
end;

{ TKASCDDrawer }

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
      ADest.Brush.Color := clForm;
      ADest.Font.Color := lTextColor;
      lVisibleText := UTF8Copy(lControlText, lSelLeftPos+lSelLength+1, lControlTextLen);
      ADest.TextOut(lSelLeftPixelPos, lLineTop, lVisibleText);
    end;
  end;

  // And the caret
  DrawCaret(ADest, Point(0, 0), ASize, AState, AStateEx);
end;

{ TKASCDEdit }

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
          FEditState.SelStart.X:= 0;
          FEditState.SelLength:= UTF8Length(Text);
          Invalidate;
          Key:= 0;
        end;
      VK_C:
        begin
          Clipboard.AsText:= Copy(Text, FEditState.SelStart.X + 1, FEditState.SelLength);
          Key:= 0;
        end;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

constructor TKASCDEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Cursor:= crIBeam;
  DrawStyle:= dsExtra1;
end;

initialization
  RegisterDrawer(TKASCDDrawer.Create, dsExtra1);

end.
