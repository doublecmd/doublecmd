unit fOptionsEditorColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, StdCtrls, ExtCtrls,
  ColorBox, ComCtrls, Dialogs, Menus, Buttons, fOptionsFrame, DividerBevel, types,
  Graphics, SynEditHighlighter;

type

  { TfrmOptionsEditorColors }

  TfrmOptionsEditorColors = class(TOptionsEditor)
    BackGroundColorBox: TColorBox;
    BackGroundLabel: TLabel;
    BackGroundUseDefaultCheckBox: TCheckBox;
    bvlAttributeSection: TDividerBevel;
    cmbLanguage: TComboBox;
    ColorPreview: TSynEdit;
    ColumnPosBevel: TPanel;
    ForegroundColorBox: TColorBox;
    ForeGroundLabel: TLabel;
    ForeGroundUseDefaultCheckBox: TCheckBox;
    FrameColorBox: TColorBox;
    FrameColorUseDefaultCheckBox: TCheckBox;
    FrameEdgesBox: TComboBox;
    FrameStyleBox: TComboBox;
    ColorElementTree: TListBox;
    pnlBold: TPanel;
    pnlElementAttributes: TPanel;
    pnlItalic: TPanel;
    pnlStrikeOut: TPanel;
    pnlTop: TPanel;
    PnlTop2: TPanel;
    pnlUnderline: TPanel;
    Splitter1: TSplitter;
    stFileExtensions: TStaticText;
    TextBoldCheckBox: TCheckBox;
    TextBoldRadioInvert: TRadioButton;
    TextBoldRadioOff: TRadioButton;
    TextBoldRadioOn: TRadioButton;
    TextBoldRadioPanel: TPanel;
    TextItalicCheckBox: TCheckBox;
    TextStrikeOutCheckBox: TCheckBox;
    TextItalicRadioInvert: TRadioButton;
    TextStrikeOutRadioInvert: TRadioButton;
    TextItalicRadioOff: TRadioButton;
    TextStrikeOutRadioOff: TRadioButton;
    TextItalicRadioOn: TRadioButton;
    TextStrikeOutRadioOn: TRadioButton;
    TextItalicRadioPanel: TPanel;
    TextStrikeOutRadioPanel: TPanel;
    TextUnderlineCheckBox: TCheckBox;
    TextUnderlineRadioInvert: TRadioButton;
    TextUnderlineRadioOff: TRadioButton;
    TextUnderlineRadioOn: TRadioButton;
    TextUnderlineRadioPanel: TPanel;
    procedure FrameStyleBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure cmbLanguageChange(Sender: TObject);
    procedure ForegroundColorBoxChange(Sender: TObject);
    procedure FrameEdgesBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ColorElementTreeDrawItem(Control: TWinControl; Index: Integer;
      NodeRect: TRect; State: TOwnerDrawState);
    procedure ColorElementTreeSelectionChange(Sender: TObject; User: boolean);
    procedure pnlElementAttributesResize(Sender: TObject);
    procedure TextStyleRadioOnChange(Sender: TObject);
  private
    FCurHighlightElement: TSynHighlighterAttributes;
    UpdatingColor: Boolean;
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
  LCLType, LCLIntf, SynEditTypes, SynEditStrConst, GraphUtil,
  dmHigh;

const
  COLOR_NODE_PREFIX = ' abc  ';

function DefaultToNone(AColor: TColor): TColor;
begin
  if AColor = clDefault then
    Result := clNone
  else
    Result := AColor;
end;

function NoneToDefault(AColor: TColor): TColor;
begin
  if AColor = clNone then
    Result := clDefault
  else
    Result := AColor;
end;

{ TfrmOptionsEditorColors }

procedure TfrmOptionsEditorColors.FrameEdgesBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState); //+++
var
  r: TRect;
  PCol: Integer;
begin
  if Index  < 0 then exit;;

  r.top := ARect.top + 3;
  r.bottom := ARect.bottom - 3;
  r.left := ARect.left + 5;
  r.right := ARect.Right - 5;

  with TCustomComboBox(Control).Canvas do
  begin
    FillRect(ARect);
    Pen.Width := 1;
    PCol := pen.Color;
    Pen.Color := clGray;
    Pen.Style := psDot;
    Pen.EndCap := pecFlat;
    Rectangle(r);
    Pen.Width := 2;
    pen.Color := PCol;
    Pen.Style := psSolid;
    case Index of
      ord(sfeAround): Rectangle(r);
      ord(sfeBottom): begin
          MoveTo(r.Left, r.Bottom);
          LineTo(r.Right-1, r.Bottom);
        end;
      ord(sfeLeft): begin
          MoveTo(r.Left, r.Top);
          LineTo(r.Left, r.Bottom-1);
        end;
    end;
  end;
end;

procedure TfrmOptionsEditorColors.FrameStyleBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState); //++
var
  p: TPoint;
begin
  if Index  < 0 then exit;;

  with TCustomComboBox(Control).Canvas do
  begin
    FillRect(ARect);
    Pen.Width := 2;
    pen.EndCap := pecFlat;
    case Index of
      0: Pen.Style := psSolid;
      1: Pen.Style := psDash;
      2: Pen.Style := psDot;
      3: Pen.Style := psSolid;
    end;
    if Index = 3 then begin
      MoveToEx(Handle, ARect.Left + 5, (ARect.Top + ARect.Bottom) div 2 - 2, @p);
      WaveTo(Handle, ARect.Right - 5, (ARect.Top + ARect.Bottom) div 2 - 2, 4);
    end else begin
      MoveTo(ARect.Left + 5, (ARect.Top + ARect.Bottom) div 2);
      LineTo(ARect.Right - 5, (ARect.Top + ARect.Bottom) div 2);
    end;
  end;
end;

procedure TfrmOptionsEditorColors.cmbLanguageChange(Sender: TObject);
var
  I: LongInt;
  HighLighter: TSynCustomHighlighter;
begin
  if (cmbLanguage.ItemIndex < 0) then Exit;
  HighLighter:= TSynCustomHighlighter(cmbLanguage.Items.Objects[cmbLanguage.ItemIndex]);
  stFileExtensions.Caption:= Copy(HighLighter.DefaultFilter, 1, Pos('|', HighLighter.DefaultFilter) - 1);
  ColorPreview.Lines.Text:= HighLighter.SampleSource;
  ColorPreview.Highlighter:= HighLighter;
  ColorElementTree.Items.Clear;
  for i:=0 to HighLighter.AttrCount-1 do
    ColorElementTree.Items.AddObject(HighLighter.Attribute[i].StoredName, HighLighter.Attribute[i]);
  if (ColorElementTree.Count > 0) then ColorElementTree.ItemIndex:= 0;
end;

procedure TfrmOptionsEditorColors.ForegroundColorBoxChange(Sender: TObject); //+++
var
  AttrToEdit: TSynHighlighterAttributes;
begin
  if (FCurHighlightElement = nil) or UpdatingColor then
    exit;
  UpdatingColor := True;

  AttrToEdit := FCurHighlightElement;

  if Sender = ForegroundColorBox then
  begin
    AttrToEdit.Foreground := DefaultToNone(ForeGroundColorBox.Selected);
    ForeGroundUseDefaultCheckBox.Checked := ForeGroundColorBox.Selected <> clDefault;
  end;
  if Sender = BackGroundColorBox then
  begin
    AttrToEdit.Background := DefaultToNone(BackGroundColorBox.Selected);
    BackGroundUseDefaultCheckBox.Checked := BackGroundColorBox.Selected <> clDefault;
  end;
  if Sender = FrameColorBox then
  begin
    AttrToEdit.FrameColor := DefaultToNone(FrameColorBox.Selected);
    FrameColorUseDefaultCheckBox.Checked := FrameColorBox.Selected <> clDefault;
    FrameEdgesBox.Enabled := FrameColorBox.Selected <> clDefault;
    FrameStyleBox.Enabled := FrameColorBox.Selected <> clDefault;
  end;
  if Sender = FrameEdgesBox then
  begin
    AttrToEdit.FrameEdges := TSynFrameEdges(FrameEdgesBox.ItemIndex);
  end;
  if Sender = FrameStyleBox then
  begin
    AttrToEdit.FrameStyle := TSynLineStyle(FrameStyleBox.ItemIndex);
  end;

  UpdatingColor := False;
end;

procedure TfrmOptionsEditorColors.ColorElementTreeDrawItem(Control: TWinControl;
  Index: Integer; NodeRect: TRect; State: TOwnerDrawState); //+++
var
  FullAbcWidth, AbcWidth: Integer;
  Attri: TSynHighlighterAttributes;
  TextY: Integer;
  AttriIdx: LongInt;
  c: TColor;
  s: String;
begin
  Attri := TSynHighlighterAttributes(ColorElementTree.Items.Objects[Index]);

  if (Attri = nil) then Exit;

  // Draw node background and name
  if odSelected in State then begin
    ColorElementTree.Canvas.Brush.Color := clHighlight;
    ColorElementTree.Canvas.Font.Color := clHighlightText;
  end else begin
    ColorElementTree.Canvas.Brush.Color := ColorElementTree.Color;
    ColorElementTree.Canvas.Font.Color := Font.Color;
  end;
  FullAbcWidth := ColorElementTree.Canvas.TextExtent(COLOR_NODE_PREFIX).cx;
  TextY := (NodeRect.Top + NodeRect.Bottom - ColorElementTree.Canvas.TextHeight(ColorElementTree.Items[Index])) div 2;
  ColorElementTree.Canvas.FillRect(NodeRect);
  ColorElementTree.Canvas.TextOut(NodeRect.Left+FullAbcWidth, TextY, Attri.Name);

  // Draw preview box - Background
  c := clNone;
  if (hafBackColor in  Attri.Features) then
    c := Attri.Background;

  // Fallback Background-color for text
  if (c = clNone) or (c = clDefault) then
    c := ColorPreview.Color;
  ColorElementTree.Canvas.Brush.Color := c;
  ColorElementTree.Canvas.FillRect(NodeRect.Left+2, NodeRect.Top+2, NodeRect.Left+FullAbcWidth-2, NodeRect.Bottom-2);

  // Draw preview Frame
  ColorElementTree.Canvas.Pen.Color := Attri.FrameColor;
  if (hafFrameColor in Attri.Features) and
     (Attri.FrameColor <> clDefault) and (Attri.FrameColor <> clNone)
  then
    ColorElementTree.Canvas.Rectangle(NodeRect.Left+2, NodeRect.Top+2,
                                      NodeRect.Left+FullAbcWidth-2, NodeRect.Bottom-2);

  // Draw preview ForeGround
  if (hafForeColor in Attri.Features) //and
       //(ahaSupportedFeatures[TAdditionalHilightAttribute(AttriIdx)].BG) )       // if no BG, then FG was used
  then begin
    c := Attri.Foreground;
    if (c = clNone) or (c = clDefault) then
      c := ColorPreview.Font.Color;

    begin
      s := 'abc';
      ColorElementTree.Canvas.Font.Color := c;
      ColorElementTree.Canvas.Font.Style := Attri.Style;
      ColorElementTree.Canvas.Font.Height := -(NodeRect.Bottom - NodeRect.Top - 7);
      TextY := (NodeRect.Top + NodeRect.Bottom - canvas.TextHeight(s)) div 2;
      AbcWidth := ColorElementTree.Canvas.TextExtent(s).cx;
      SetBkMode(ColorElementTree.Canvas.Handle, TRANSPARENT);
      ColorElementTree.Canvas.TextOut(NodeRect.Left+(FullAbcWidth - AbcWidth) div 2, TextY, s);
      SetBkMode(ColorElementTree.Canvas.Handle, OPAQUE);

      ColorElementTree.Canvas.Font.Height := Font.Height;
      ColorElementTree.Canvas.Font.Style := [];
    end;
  end;
end;

procedure TfrmOptionsEditorColors.ColorElementTreeSelectionChange(
  Sender: TObject; User: boolean); //+++
var
  AttrToShow: TSynHighlighterAttributes;
  CanGlobal: Boolean;
begin
  if (ColorElementTree.ItemIndex < 0) or UpdatingColor then
    Exit;
  FCurHighlightElement:= TSynHighlighterAttributes(ColorElementTree.Items.Objects[ColorElementTree.ItemIndex]);
  UpdatingColor := True;
  DisableAlign;
  try

  AttrToShow := FCurHighlightElement;
  CanGlobal := (cmbLanguage.ItemIndex <> 0);

  ForegroundColorBox.Style := ForegroundColorBox.Style + [cbIncludeDefault];
  BackGroundColorBox.Style := BackGroundColorBox.Style + [cbIncludeDefault];

  // Forground
  ForeGroundLabel.Visible              := (hafForeColor in AttrToShow.Features) and
                                          (CanGlobal = False);
  ForeGroundUseDefaultCheckBox.Visible := (hafForeColor in AttrToShow.Features) and
                                          (CanGlobal = True);
  ForegroundColorBox.Visible           := (hafForeColor in AttrToShow.Features);

  ForegroundColorBox.Selected := NoneToDefault(AttrToShow.Foreground);
  if ForegroundColorBox.Selected = clDefault then
    ForegroundColorBox.Tag := ForegroundColorBox.DefaultColorColor
  else
    ForegroundColorBox.Tag := ForegroundColorBox.Selected;
  ForeGroundUseDefaultCheckBox.Checked := ForegroundColorBox.Selected <> clDefault;

  // BackGround
  BackGroundLabel.Visible              := (hafBackColor in AttrToShow.Features) and
                                          (CanGlobal = False);
  BackGroundUseDefaultCheckBox.Visible := (hafBackColor in AttrToShow.Features) and
                                          (CanGlobal = True);
  BackGroundColorBox.Visible           := (hafBackColor in AttrToShow.Features);

  BackGroundColorBox.Selected := NoneToDefault(AttrToShow.Background);
  if BackGroundColorBox.Selected = clDefault then
    BackGroundColorBox.Tag := BackGroundColorBox.DefaultColorColor
  else
    BackGroundColorBox.Tag := BackGroundColorBox.Selected;
  BackGroundUseDefaultCheckBox.Checked := BackGroundColorBox.Selected <> clDefault;

  // Frame
  FrameColorUseDefaultCheckBox.Visible := hafFrameColor in AttrToShow.Features;
  FrameColorBox.Visible                := hafFrameColor in AttrToShow.Features;
  FrameEdgesBox.Visible                := hafFrameEdges in AttrToShow.Features;
  FrameStyleBox.Visible                := hafFrameStyle in AttrToShow.Features;

  FrameColorBox.Selected := NoneToDefault(AttrToShow.FrameColor);
  if FrameColorBox.Selected = clDefault then
    FrameColorBox.Tag := FrameColorBox.DefaultColorColor
  else
    FrameColorBox.Tag := FrameColorBox.Selected;
  FrameColorUseDefaultCheckBox.Checked := FrameColorBox.Selected <> clDefault;
  FrameEdgesBox.ItemIndex := integer(AttrToShow.FrameEdges);
  FrameStyleBox.ItemIndex := integer(AttrToShow.FrameStyle);
  FrameEdgesBox.Enabled := FrameColorUseDefaultCheckBox.Checked;
  FrameStyleBox.Enabled := FrameColorUseDefaultCheckBox.Checked;

  // Styles
  TextBoldCheckBox.Visible      := hafStyle in AttrToShow.Features;
  TextItalicCheckBox.Visible    := hafStyle in AttrToShow.Features;
  TextUnderlineCheckBox.Visible := hafStyle in AttrToShow.Features;
  TextStrikeOutCheckBox.Visible := hafStyle in AttrToShow.Features;

  TextBoldRadioPanel.Visible      := hafStyleMask in AttrToShow.Features;
  TextItalicRadioPanel.Visible    := hafStyleMask in AttrToShow.Features;
  TextUnderlineRadioPanel.Visible := hafStyleMask in AttrToShow.Features;
  TextStrikeOutRadioPanel.Visible := hafStyleMask in AttrToShow.Features;

  if hafStyleMask in AttrToShow.Features then begin
    TextBoldCheckBox.Checked   := (fsBold in AttrToShow.Style) or
                                  (fsBold in AttrToShow.StyleMask);
    TextBoldRadioPanel.Enabled := TextBoldCheckBox.Checked;

    if not(fsBold in AttrToShow.StyleMask) then
      TextBoldRadioInvert.Checked := True
    else
    if fsBold in AttrToShow.Style then
      TextBoldRadioOn.Checked := True
    else
      TextBoldRadioOff.Checked := True;

    TextItalicCheckBox.Checked   := (fsItalic in AttrToShow.Style) or
                                    (fsItalic in AttrToShow.StyleMask);
    TextItalicRadioPanel.Enabled := TextItalicCheckBox.Checked;

    if not(fsItalic in AttrToShow.StyleMask) then
      TextItalicRadioInvert.Checked := True
    else
    if fsItalic in AttrToShow.Style then
      TextItalicRadioOn.Checked := True
    else
      TextItalicRadioOff.Checked := True;

    TextUnderlineCheckBox.Checked := (fsUnderline in AttrToShow.Style) or
                                (fsUnderline in AttrToShow.StyleMask);
    TextUnderlineRadioPanel.Enabled := TextUnderlineCheckBox.Checked;

    if not(fsUnderline in AttrToShow.StyleMask) then
      TextUnderlineRadioInvert.Checked := True
    else
    if fsUnderline in AttrToShow.Style then
      TextUnderlineRadioOn.Checked := True
    else
      TextUnderlineRadioOff.Checked := True;

    TextStrikeOutCheckBox.Checked := (fsStrikeOut in AttrToShow.Style) or
                                (fsStrikeOut in AttrToShow.StyleMask);
    TextStrikeOutRadioPanel.Enabled := TextStrikeOutCheckBox.Checked;

    if not(fsStrikeOut in AttrToShow.StyleMask) then
      TextStrikeOutRadioInvert.Checked := True
    else
    if fsStrikeOut in AttrToShow.Style then
      TextStrikeOutRadioOn.Checked := True
    else
      TextStrikeOutRadioOff.Checked := True;
  end
  else
  begin
    TextBoldCheckBox.Checked      := fsBold in AttrToShow.Style;
    TextItalicCheckBox.Checked    := fsItalic in AttrToShow.Style;
    TextUnderlineCheckBox.Checked := fsUnderline in AttrToShow.Style;
    TextStrikeOutCheckBox.Checked := fsStrikeOut in AttrToShow.Style;
  end;

  UpdatingColor := False;
  finally
    EnableAlign;
  end;
  pnlElementAttributesResize(nil);
end;

procedure TfrmOptionsEditorColors.pnlElementAttributesResize(Sender: TObject); //+++
var
  MinAnchor: TControl;
  MinWidth: Integer;

  procedure CheckControl(Other: TControl);
  var w,h: Integer;
  begin
    if not Other.Visible then exit;
    Other.GetPreferredSize(w,h);
    if w <= MinWidth then exit;
    MinAnchor := Other;
    MinWidth := w;
  end;
begin
  MinWidth := -1;
  MinAnchor := ForeGroundLabel;
  CheckControl(ForeGroundLabel);
  CheckControl(BackGroundLabel);
  CheckControl(ForeGroundUseDefaultCheckBox);
  CheckControl(BackGroundUseDefaultCheckBox);
  CheckControl(FrameColorUseDefaultCheckBox);

  ColumnPosBevel.AnchorSide[akLeft].Control := MinAnchor;
end;

procedure TfrmOptionsEditorColors.TextStyleRadioOnChange(Sender: TObject); //+++
var
  AttrToEdit: TSynHighlighterAttributes;

  procedure CalcNewStyle(CheckBox: TCheckBox; RadioOn, RadioOff,
                         RadioInvert: TRadioButton; fs : TFontStyle;
                         Panel: TPanel);
  begin
    if CheckBox.Checked then
    begin
      Panel.Enabled := True;
      if RadioInvert.Checked then
      begin
        AttrToEdit.Style     := AttrToEdit.Style + [fs];
        AttrToEdit.StyleMask := AttrToEdit.StyleMask - [fs];
      end
      else
      if RadioOn.Checked then
      begin
        AttrToEdit.Style     := AttrToEdit.Style + [fs];
        AttrToEdit.StyleMask := AttrToEdit.StyleMask + [fs];
      end
      else
      if RadioOff.Checked then
      begin
        AttrToEdit.Style     := AttrToEdit.Style - [fs];
        AttrToEdit.StyleMask := AttrToEdit.StyleMask + [fs];
      end
    end
    else
    begin
      Panel.Enabled := False;
      AttrToEdit.Style     := AttrToEdit.Style - [fs];
      AttrToEdit.StyleMask := AttrToEdit.StyleMask - [fs];
    end;
  end;
begin
  if UpdatingColor or not (hafStyleMask in FCurHighlightElement.Features) then
    Exit;

  AttrToEdit := FCurHighlightElement;

  if (Sender = TextBoldCheckBox) or
     (Sender = TextBoldRadioOn) or
     (Sender = TextBoldRadioOff) or
     (Sender = TextBoldRadioInvert) then
    CalcNewStyle(TextBoldCheckBox, TextBoldRadioOn, TextBoldRadioOff,
                    TextBoldRadioInvert, fsBold, TextBoldRadioPanel);

  if (Sender = TextItalicCheckBox) or
     (Sender = TextItalicRadioOn) or
     (Sender = TextItalicRadioOff) or
     (Sender = TextItalicRadioInvert) then
    CalcNewStyle(TextItalicCheckBox, TextItalicRadioOn, TextItalicRadioOff,
                    TextItalicRadioInvert, fsItalic, TextItalicRadioPanel);

  if (Sender = TextUnderlineCheckBox) or
     (Sender = TextUnderlineRadioOn) or
     (Sender = TextUnderlineRadioOff) or
     (Sender = TextUnderlineRadioInvert) then
    CalcNewStyle(TextUnderlineCheckBox, TextUnderlineRadioOn, TextUnderlineRadioOff,
                    TextUnderlineRadioInvert, fsUnderline, TextUnderlineRadioPanel);

  if (Sender = TextStrikeOutCheckBox) or
     (Sender = TextStrikeOutRadioOn) or
     (Sender = TextStrikeOutRadioOff) or
     (Sender = TextStrikeOutRadioInvert) then
    CalcNewStyle(TextStrikeOutCheckBox, TextStrikeOutRadioOn, TextStrikeOutRadioOff,
                    TextStrikeOutRadioInvert, fsStrikeOut, TextStrikeOutRadioPanel);
end;

procedure TfrmOptionsEditorColors.Init;
var
  I: LongInt;
  Highlighter: TSynCustomHighlighter;
begin
  inherited Init;
  cmbLanguage.Items.AddObject(dmHighl.FSynGlobalHighlighter.LanguageName, dmHighl.FSynGlobalHighlighter);
  for I:= 0 to dmHighl.slHighLighters.Count - 1 do
  begin
    Highlighter:= TSynCustomHighlighter(dmHighl.slHighLighters.Objects[I]);
    cmbLanguage.Items.AddObject(Highlighter.LanguageName, Highlighter);
  end;
  cmbLanguage.ItemIndex:= 0;
  cmbLanguageChange(nil);
end;

procedure TfrmOptionsEditorColors.Load;
begin
  //inherited Load;
end;

function TfrmOptionsEditorColors.Save: TOptionsEditorSaveFlags;
begin
  //Result:=inherited Save;
 // dmHighl.SaveToFile('d:\test.xml');
end;

class function TfrmOptionsEditorColors.GetIconIndex: Integer;
begin
  Result:= 21;
end;

class function TfrmOptionsEditorColors.GetTitle: String;
begin
  Result:= 'Highlighters'
end;

end.

