{
    Double Commander
    -------------------------------------------------------------------------
    Internal editor highlighters configuration frame

    Copyright (C) 2012-2017 Alexander Koblov (alexx2000@mail.ru)

    Based on Lazarus IDE editor configuration frame (Editor/Display/Colors)

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

unit fOptionsEditorColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, StdCtrls, ExtCtrls,
  ColorBox, ComCtrls, Dialogs, Menus, Buttons, fOptionsFrame, DividerBevel, types,
  Graphics, SynEditHighlighter, SynUniClasses, SynUniRules, dmHigh;

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
    edtFileExtensions: TEdit;
    ForegroundColorBox: TColorBox;
    ForeGroundLabel: TLabel;
    ForeGroundUseDefaultCheckBox: TCheckBox;
    FrameColorBox: TColorBox;
    FrameColorUseDefaultCheckBox: TCheckBox;
    FrameEdgesBox: TComboBox;
    FrameStyleBox: TComboBox;
    ColorElementTree: TTreeView;
    pnlBold: TPanel;
    pnlElementAttributes: TPanel;
    pnlItalic: TPanel;
    pnlStrikeOut: TPanel;
    pnlTop: TPanel;
    PnlTop2: TPanel;
    pnlUnderline: TPanel;
    btnSaveMask: TSpeedButton;
    btnResetMask: TSpeedButton;
    Splitter1: TSplitter;
    pnlFileExtensions: TPanel;
    tbtnGlobal: TToolButton;
    tbtnLocal: TToolButton;
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
    ToolBar1: TToolBar;
    ToolButton3: TToolButton;
    procedure btnResetMaskClick(Sender: TObject);
    procedure btnSaveMaskClick(Sender: TObject);
    procedure FrameStyleBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure cmbLanguageChange(Sender: TObject);
    procedure ForegroundColorBoxChange(Sender: TObject);
    procedure FrameEdgesBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure ColorElementTreeAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; {%H-}Stage: TCustomDrawStage; var {%H-}PaintImages, {%H-}DefaultDraw: Boolean);
    procedure ColorElementTreeChange(Sender: TObject; {%H-}Node: TTreeNode);
    procedure GeneralCheckBoxOnChange(Sender: TObject);
    procedure pnlElementAttributesResize(Sender: TObject);
    procedure tbtnGlobalClick(Sender: TObject);
    procedure TextStyleRadioOnChange(Sender: TObject);
    procedure SynPlainTextHighlighterChange(Sender: TObject);
  private
    FHighl: TdmHighl;
    FDefHighlightElement,
    FCurHighlightElement: TSynHighlighterAttributes;
    FCurrentHighlighter: TSynCustomHighlighter;
    FIsEditingDefaults: Boolean;
    UpdatingColor: Boolean;
    procedure UpdateCurrentScheme;
    function TreeAddSet(Node: TTreeNode; SymbSet: TSynSet): TTreeNode;
    function TreeAddRange(Node: TTreeNode; Range: TSynRange): TTreeNode;
    function TreeAddKeyList(Node: TTreeNode; KeyList: TSynKeyList): TTreeNode;
    function SynAttributeSortCompare(Node1, Node2: TTreeNode): Integer;
  protected
    procedure Init; override;
    procedure Done; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
    function IsSignatureComputedFromAllWindowComponents: Boolean; override;
  end;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, SynEditTypes, SynUniHighlighter, GraphUtil, uLng, uGlobs;

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

function TfrmOptionsEditorColors.SynAttributeSortCompare(Node1, Node2: TTreeNode): Integer;
begin
  if CompareStr(Node1.Text, rsSynDefaultText) = 0 then
    Result:= -1
  else if CompareStr(Node2.Text, rsSynDefaultText) = 0 then
    Result:=  1
  else
    Result:= CompareStr(Node1.Text, Node2.Text);
end;

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

procedure TfrmOptionsEditorColors.btnSaveMaskClick(Sender: TObject);
begin
  FCurrentHighlighter.DefaultFilter:= FCurrentHighlighter.LanguageName + ' (' + edtFileExtensions.Text + ')|' + edtFileExtensions.Text;
end;

procedure TfrmOptionsEditorColors.btnResetMaskClick(Sender: TObject);
begin
  with TSynCustomHighlighterClass(FCurrentHighlighter.ClassType).Create(nil) do
  begin
    FCurrentHighlighter.DefaultFilter:= DefaultFilter;
    edtFileExtensions.Text:= Copy(FCurrentHighlighter.DefaultFilter, Pos('|', FCurrentHighlighter.DefaultFilter) + 1, MaxInt);
    Free;
  end;
end;

procedure TfrmOptionsEditorColors.cmbLanguageChange(Sender: TObject);
var
  I: LongInt;
  ANode: TTreeNode;
  SynUniSyn: Boolean;
begin
  if (cmbLanguage.ItemIndex < 0) then Exit;
  FCurrentHighlighter:= TSynCustomHighlighter(cmbLanguage.Items.Objects[cmbLanguage.ItemIndex]);
  pnlFileExtensions.Enabled:= not (FCurrentHighlighter is TSynPlainTextHighlighter);
  edtFileExtensions.Text:= Copy(FCurrentHighlighter.DefaultFilter, Pos('|', FCurrentHighlighter.DefaultFilter) + 1, MaxInt);
  ColorPreview.Lines.Text:= FHighl.GetSampleSource(FCurrentHighlighter);
  if ColorPreview.Lines.Text = EmptyStr then
  try
    ColorPreview.Lines.Text:= FCurrentHighlighter.SampleSource;
  except
    ColorPreview.Lines.Text:= EmptyStr;
  end;
  FHighl.SetHighlighter(ColorPreview, FCurrentHighlighter);
  SynUniSyn:= (FCurrentHighlighter is TSynUniSyn);
  ColorElementTree.ShowButtons:= SynUniSyn;
  ColorElementTree.ShowRoot:= SynUniSyn;
  btnResetMask.Enabled:= not SynUniSyn;
  ColorElementTree.Items.Clear;
  if SynUniSyn then
  begin
    ANode:= TreeAddRange(nil, TSynUniSyn(FCurrentHighlighter).MainRules);
    ANode.Expand(False);
  end
  else if (FCurrentHighlighter.AttrCount > 0) then
  begin
    for I:= 0 to FCurrentHighlighter.AttrCount - 1 do
    begin
      ANode:= ColorElementTree.Items.Add(nil, FCurrentHighlighter.Attribute[I].Name);
      ANode.Data:= FCurrentHighlighter.Attribute[I];
    end;
    ColorElementTree.CustomSort(@SynAttributeSortCompare);
  end;
  if ColorElementTree.Items.GetFirstNode <> nil then
  begin
    ColorElementTree.Items.GetFirstNode.Selected := True;
    ColorElementTreeChange(ColorElementTree, ColorElementTree.Items.GetFirstNode);
  end;
end;

procedure TfrmOptionsEditorColors.ForegroundColorBoxChange(Sender: TObject); //+++
var
  AttrToEdit: TSynHighlighterAttributes;
begin
  if (FCurHighlightElement = nil) or UpdatingColor then
    Exit;
  UpdatingColor := True;

  if FIsEditingDefaults then
    AttrToEdit := FHighl.SynPlainTextHighlighter.Attribute[FHighl.SynPlainTextHighlighter.AttrCount-1]
  else
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
  UpdateCurrentScheme;
end;

procedure TfrmOptionsEditorColors.ColorElementTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
  DefaultDraw: Boolean); //+++
var
  NodeRect: TRect;
  FullAbcWidth, AbcWidth: Integer;
  Attri: TSynHighlighterAttributes;
  TextY: Integer;
  AText: String;
  c: TColor;
  s: String;
begin
  if not (TObject(Node.Data) is TSynHighlighterAttributes) then
  begin
    AText:= TSynRule(Node.Data).Name;
    Attri := TSynRule(Node.Data).Attribs;
  end
  else begin
    if (ColorElementTree.Items.GetFirstNode = Node) and FIsEditingDefaults then
      Attri := FDefHighlightElement
    else begin
      Attri := TSynHighlighterAttributes(Node.Data);
    end;
    AText:= Attri.Name;
  end;

  if (Attri = nil) then Exit;

  // Draw node background and name
  if cdsSelected in State then
  begin
    ColorElementTree.Canvas.Brush.Color := ColorElementTree.SelectionColor;
    ColorElementTree.Canvas.Font.Color := InvertColor(ColorElementTree.SelectionColor);
  end else begin
    ColorElementTree.Canvas.Brush.Color := ColorElementTree.Color;
    ColorElementTree.Canvas.Font.Color := Font.Color;
  end;
  NodeRect := Node.DisplayRect(True);
  FullAbcWidth := ColorElementTree.Canvas.TextExtent(COLOR_NODE_PREFIX).cx;
  TextY := (NodeRect.Top + NodeRect.Bottom - ColorElementTree.Canvas.TextHeight(Node.Text)) div 2;
  NodeRect.Right+= FullAbcWidth;
  ColorElementTree.Canvas.FillRect(NodeRect);
  ColorElementTree.Canvas.TextOut(NodeRect.Left+FullAbcWidth, TextY, AText);

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

procedure TfrmOptionsEditorColors.SynPlainTextHighlighterChange(Sender: TObject);
var
  SynPlainTextHighlighter: TSynHighlighterAttributes absolute Sender;
begin
  ColorPreview.Color:= SynPlainTextHighlighter.Background;
  ColorPreview.Font.Color:= SynPlainTextHighlighter.Foreground;
end;

procedure TfrmOptionsEditorColors.ColorElementTreeChange(Sender: TObject; Node: TTreeNode); //+++
var
  ParentFore, ParentBack: Boolean;
  AttrToShow: TSynHighlighterAttributes;
  IsDefault, CanGlobal: Boolean;
  ARule: TSynRule;
begin
  if UpdatingColor or (ColorElementTree.Selected = nil) or (ColorElementTree.Selected.Data = nil) then
    Exit;

  if (TObject(ColorElementTree.Selected.Data) is TSynHighlighterAttributes) then
  begin
    FCurHighlightElement:= TSynHighlighterAttributes(ColorElementTree.Selected.Data);
    IsDefault := SameText(rsSynDefaultText, FCurHighlightElement.Name);
    CanGlobal := (cmbLanguage.ItemIndex > 0) and IsDefault;
    ParentFore:= False;
    ParentBack:= False;
  end
  else begin
    ARule:= TSynRule(ColorElementTree.Selected.Data);
    ParentFore:= ARule.Attribs.ParentForeground;
    ParentBack:= ARule.Attribs.ParentBackground;
    FCurHighlightElement:= ARule.Attribs;
    IsDefault := (Node.Level = 0);
    CanGlobal := False;
  end;

  UpdatingColor := True;
  DisableAlign;
  try

  FDefHighlightElement:= FHighl.SynPlainTextHighlighter.Attribute[FHighl.SynPlainTextHighlighter.AttrCount - 1];

  FIsEditingDefaults:= CanGlobal and (FCurrentHighlighter.Tag = 1);

  tbtnGlobal.Enabled := CanGlobal;
  tbtnLocal.Enabled := CanGlobal;
  tbtnGlobal.AllowAllUp := not CanGlobal;
  tbtnLocal.AllowAllUp := not CanGlobal;
  tbtnGlobal.Down := (FCurrentHighlighter.Tag = 1) and CanGlobal;
  tbtnLocal.Down  := (FCurrentHighlighter.Tag = 0) and CanGlobal;

  if FIsEditingDefaults then
    AttrToShow := FDefHighlightElement
  else
    AttrToShow := FCurHighlightElement;

  ForegroundColorBox.Style := ForegroundColorBox.Style + [cbIncludeDefault];
  BackGroundColorBox.Style := BackGroundColorBox.Style + [cbIncludeDefault];

  // Foreground
  ForeGroundLabel.Visible              := (hafForeColor in AttrToShow.Features) and
                                          (IsDefault = True);
  ForeGroundUseDefaultCheckBox.Visible := (hafForeColor in AttrToShow.Features) and
                                          (IsDefault = False);
  ForegroundColorBox.Visible           := (hafForeColor in AttrToShow.Features);

  ForegroundColorBox.Selected := NoneToDefault(AttrToShow.Foreground);
  if ForegroundColorBox.Selected = clDefault then
    ForegroundColorBox.Tag := ForegroundColorBox.DefaultColorColor
  else
    ForegroundColorBox.Tag := ForegroundColorBox.Selected;
  ForeGroundUseDefaultCheckBox.Checked := (ForegroundColorBox.Selected <> clDefault) and
                                          (ParentFore = False);

  // BackGround
  BackGroundLabel.Visible              := (hafBackColor in AttrToShow.Features) and
                                          (IsDefault = True);
  BackGroundUseDefaultCheckBox.Visible := (hafBackColor in AttrToShow.Features) and
                                          (IsDefault = False);
  BackGroundColorBox.Visible           := (hafBackColor in AttrToShow.Features);

  BackGroundColorBox.Selected := NoneToDefault(AttrToShow.Background);
  if BackGroundColorBox.Selected = clDefault then
    BackGroundColorBox.Tag := BackGroundColorBox.DefaultColorColor
  else
    BackGroundColorBox.Tag := BackGroundColorBox.Selected;
  BackGroundUseDefaultCheckBox.Checked := (BackGroundColorBox.Selected <> clDefault) and
                                          (ParentBack = False);

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

  if SameText(AttrToShow.Name, rsSynDefaultText) then
  begin
    AttrToShow.OnChange:= @SynPlainTextHighlighterChange;
  end;

  UpdatingColor := False;
  finally
    EnableAlign;
  end;
  pnlElementAttributesResize(nil);
end;

procedure TfrmOptionsEditorColors.GeneralCheckBoxOnChange(Sender: TObject);
var
  TheColorBox: TColorBox;
  AttrToEdit: TSynHighlighterAttributes;

  procedure SetCheckBoxStyle(CheckBox: TCheckBox; style: TFontStyle);
  begin
    if hafStyleMask in AttrToEdit.Features then
      TextStyleRadioOnChange(Sender)
    else
    if CheckBox.Checked xor (style in AttrToEdit.Style) then
    begin
      if CheckBox.Checked then
        AttrToEdit.Style := AttrToEdit.Style + [style]
      else
        AttrToEdit.Style := AttrToEdit.Style - [style];
      UpdateCurrentScheme;
    end;
  end;

begin
  if FCurHighlightElement = nil then Exit;

  if FIsEditingDefaults then
    AttrToEdit := FDefHighlightElement
  else
    AttrToEdit := FCurHighlightElement;

  if UpdatingColor = False then begin
    UpdatingColor := True;

    TheColorBox := nil;
    if Sender = ForeGroundUseDefaultCheckBox then TheColorBox := ForegroundColorBox;
    if Sender = BackGroundUseDefaultCheckBox then TheColorBox := BackGroundColorBox;
    if Sender = FrameColorUseDefaultCheckBox then TheColorBox := FrameColorBox;
    if Assigned(TheColorBox) then
    begin
      if TCheckBox(Sender).Checked then
      begin
        TheColorBox.Selected := TheColorBox.Tag;
        if (AttrToEdit is TSynAttributes) then
        begin
          if (Sender = ForeGroundUseDefaultCheckBox) then
          begin
            TSynAttributes(AttrToEdit).ParentForeground:= False;
          end
          else if (Sender = BackGroundUseDefaultCheckBox) then
          begin
            TSynAttributes(AttrToEdit).ParentBackground:= False;
          end;
        end;
      end
      else begin
        TheColorBox.Tag := TheColorBox.Selected;
        if not (AttrToEdit is TSynAttributes) then
          TheColorBox.Selected := clDefault
        else if Assigned(ColorElementTree.Selected) and Assigned(ColorElementTree.Selected.Parent) then
        begin
          if (Sender = ForeGroundUseDefaultCheckBox) then
          begin
            TSynAttributes(AttrToEdit).ParentForeground:= True;
            TheColorBox.Selected := TSynRange(ColorElementTree.Selected.Parent.Data).Attribs.Foreground
          end
          else if (Sender = BackGroundUseDefaultCheckBox) then
          begin
            TSynAttributes(AttrToEdit).ParentBackground:= True;
            TheColorBox.Selected := TSynRange(ColorElementTree.Selected.Parent.Data).Attribs.Background;
          end;
        end;
      end;

      if (Sender = ForeGroundUseDefaultCheckBox) and
         (DefaultToNone(ForegroundColorBox.Selected) <> AttrToEdit.Foreground)
      then begin
        AttrToEdit.Foreground := DefaultToNone(ForegroundColorBox.Selected);
        UpdateCurrentScheme;
      end;
      if (Sender = BackGroundUseDefaultCheckBox) and
         (DefaultToNone(BackGroundColorBox.Selected) <> AttrToEdit.Background)
      then begin
        AttrToEdit.Background := DefaultToNone(BackGroundColorBox.Selected);
        UpdateCurrentScheme;
      end;
      if (Sender = FrameColorUseDefaultCheckBox) and
         (DefaultToNone(FrameColorBox.Selected) <> AttrToEdit.FrameColor)
      then begin
        AttrToEdit.FrameColor := DefaultToNone(FrameColorBox.Selected);
        FrameEdgesBox.Enabled := TCheckBox(Sender).Checked;
        FrameStyleBox.Enabled := TCheckBox(Sender).Checked;
        UpdateCurrentScheme;
      end;
    end;

    UpdatingColor := False;
  end;

  if Sender = TextBoldCheckBox then
    SetCheckBoxStyle(TextBoldCheckBox, fsBold);

  if Sender = TextItalicCheckBox then
    SetCheckBoxStyle(TextItalicCheckBox, fsItalic);

  if Sender = TextUnderlineCheckBox then
    SetCheckBoxStyle(TextUnderlineCheckBox, fsUnderline);

  if Sender = TextStrikeOutCheckBox then
    SetCheckBoxStyle(TextStrikeOutCheckBox, fsStrikeOut);
end;

procedure TfrmOptionsEditorColors.pnlElementAttributesResize(Sender: TObject); //+++
var
  MinAnchor: TControl;
  MinWidth: Integer;

  procedure CheckControl(Other: TControl);
  var
    w: Integer = 0;
    h: Integer = 0;
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

procedure TfrmOptionsEditorColors.tbtnGlobalClick(Sender: TObject);
begin
  if (FCurHighlightElement = nil) or UpdatingColor then
    Exit;

  FCurrentHighlighter.Tag := PtrInt(tbtnGlobal.Down);
  ColorElementTreeChange(ColorElementTree, nil);
  UpdateCurrentScheme;
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

  if FIsEditingDefaults then
    AttrToEdit := FDefHighlightElement
  else
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

procedure TfrmOptionsEditorColors.UpdateCurrentScheme;
begin
  ColorPreview.Invalidate;
  ColorElementTree.Invalidate;
end;

function TfrmOptionsEditorColors.TreeAddSet(Node: TTreeNode; SymbSet: TSynSet
  ): TTreeNode;
begin
  Result:= ColorElementTree.Items.AddChild(Node, SymbSet.Name);
  Result.Data:= SymbSet;
end;

function TfrmOptionsEditorColors.TreeAddRange(Node: TTreeNode; Range: TSynRange
  ): TTreeNode;
var
  Index: Integer;
begin
  if (Node = nil) then
    Result:= ColorElementTree.Items.Add(nil, Range.Name)
  else begin
    Result:= ColorElementTree.Items.AddChild(Node, Range.Name);
  end;
  Result.Data:= Range;

  for Index := 0 to Range.SetCount - 1 do
    TreeAddSet(Result, Range.Sets[Index]);

  for Index := 0 to Range.RangeCount - 1 do
    TreeAddRange(Result, Range.Ranges[Index]);

  for Index := 0 to Range.KeyListCount - 1 do
    TreeAddKeyList(Result, Range.KeyLists[Index]);
end;

function TfrmOptionsEditorColors.TreeAddKeyList(Node: TTreeNode;
  KeyList: TSynKeyList): TTreeNode;
begin
  Result:= ColorElementTree.Items.AddChild(Node, KeyList.Name);
  Result.Data:= KeyList;
end;

procedure TfrmOptionsEditorColors.Init;
begin
  inherited Init;
  FHighl:= TdmHighl.Create(nil, True);
  FontOptionsToFont(gFonts[dcfEditor], ColorPreview.Font);
end;

procedure TfrmOptionsEditorColors.Done;
begin
  FHighl.Free;
  inherited Done;
end;

procedure TfrmOptionsEditorColors.Load;
begin
  FHighl.Assign(dmHighl);
  cmbLanguage.Items.Assign(FHighl.SynHighlighterList);
  cmbLanguage.ItemIndex:= 0;
  cmbLanguageChange(nil);
end;

function TfrmOptionsEditorColors.Save: TOptionsEditorSaveFlags;
begin
  Result:= [];
  dmHighl.Assign(FHighl);
end;

class function TfrmOptionsEditorColors.GetIconIndex: Integer;
begin
  Result:= 21;
end;

class function TfrmOptionsEditorColors.GetTitle: String;
begin
  Result:= rsOptionsEditorHighlighters;
end;

{ TfrmOptionsEditorColors.IsSignatureComputedFromAllWindowComponents }
function TfrmOptionsEditorColors.IsSignatureComputedFromAllWindowComponents: Boolean;
begin
  Result := False;
end;

end.

