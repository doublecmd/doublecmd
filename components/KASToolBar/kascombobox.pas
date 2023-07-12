{
   Double Commander Components
   -------------------------------------------------------------------------
   Extended ComboBox classes

   Copyright (C) 2012 Przemyslaw Nagay (cobines@gmail.com)
   Copyright (C) 2015-2023 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit KASComboBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorBox, Buttons, LMessages, Types;

const
  DEF_COLOR_STYLE = [cbStandardColors, cbExtendedColors,
                     cbSystemColors, cbPrettyNames];

type

  { TComboBoxWithDelItems }

  {en
     Combo box that allows removing items with Shift+Delete.
  }
  TComboBoxWithDelItems = class(TComboBox)
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  { TComboBoxAutoWidth }

  TComboBoxAutoWidth = class(TComboBox)
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
                                     WithThemeSpace: Boolean); override;
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
                const AXProportion, AYProportion: Double); override;
  end;

  { TKASColorBox }

  TKASColorBox = class(TColorBox)
  protected
    procedure SetCustomColor(AColor: TColor);
    function PickCustomColor: Boolean; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
                                     WithThemeSpace: Boolean); override;
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
                const AXProportion, AYProportion: Double); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DefaultColorColor default clNone;
    property Style default DEF_COLOR_STYLE;
  end;

  { TKASColorBoxButton }

  TKASColorBoxButton = class(TCustomControl)
  private
    function GetSelected: TColor;
    function GetStyle: TColorBoxStyle;
    function GetOnChange: TNotifyEvent;
    procedure SetSelected(AValue: TColor);
    procedure SetStyle(AValue: TColorBoxStyle);
    procedure SetOnChange(AValue: TNotifyEvent);
  protected
    FButton: TSpeedButton;
    FColorBox: TKASColorBox;
    procedure DoAutoSize; override;
    procedure ButtonClick(Sender: TObject);
    class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
                WithThemeSpace: Boolean); override;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
    function Focused: Boolean; override;
    property Selected: TColor read GetSelected write SetSelected default clBlack;
  published
    property Align;
    property TabOrder;
    property AutoSize default True;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property Style: TColorBoxStyle read GetStyle write SetStyle default DEF_COLOR_STYLE;
  end;

procedure Register;

implementation

uses
  LCLType, LCLIntf;

procedure Register;
begin
  RegisterComponents('KASComponents',[TComboBoxWithDelItems, TComboBoxAutoWidth,
                                      TKASColorBox, TKASColorBoxButton]);
end;

procedure CalculateSize(ComboBox: TCustomComboBox;
                        var PreferredWidth: Integer; PreferredHeight: Integer);
var
  DC: HDC;
  R: TRect;
  I, M: Integer;
  Flags: Cardinal;
  OldFont: HGDIOBJ;
  MaxWidth: Integer;
  LabelText: String;
  Idx: Integer = -1;
begin
  with ComboBox do
  begin
    MaxWidth:= Constraints.MinMaxWidth(10000);

    if Items.Count = 0 then
      LabelText:= Text
    else begin
      M := Canvas.TextWidth(Text);
      for I := 0 to Items.Count - 1 do
      begin
        Flags := Canvas.TextWidth(Items[I]);
        if Flags > M then
        begin
          M := Flags;
          Idx := I;
        end;
      end;
      if Idx < 0 then
        LabelText := Text
      else begin
        LabelText := Items[Idx];
      end;
    end;

    if LabelText = '' then begin
      PreferredWidth := 1;
      Exit;
    end;

    DC := GetDC(Parent.Handle);
    try
      LabelText:= LabelText + 'W';
      R := Rect(0, 0, MaxWidth, 10000);
      OldFont := SelectObject(DC, HGDIOBJ(Font.Reference.Handle));
      Flags := DT_CALCRECT or DT_EXPANDTABS;

      DrawText(DC, PChar(LabelText), Length(LabelText), R, Flags);
      SelectObject(DC, OldFont);
      PreferredWidth := (R.Right - R.Left) + PreferredHeight;
    finally
      ReleaseDC(Parent.Handle, DC);
    end;
  end;
end;

{ TComboBoxWithDelItems }

procedure TComboBoxWithDelItems.KeyDown(var Key: Word; Shift: TShiftState);
var
  Index: Integer;
begin
  if DroppedDown and (Key = VK_DELETE) and (Shift = [ssShift]) then
  begin
    Index := ItemIndex;
    if (Index >= 0) and (Index < Items.Count) then
    begin
      Items.Delete(Index);
      ItemIndex := Index;
      Key := 0;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

{ TComboBoxAutoWidth }

procedure TComboBoxAutoWidth.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  if csDesigning in ComponentState then Exit;
  if (Parent = nil) or (not Parent.HandleAllocated) then Exit;

  CalculateSize(Self, PreferredWidth, PreferredHeight);
end;

procedure TComboBoxAutoWidth.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  // Don't auto adjust horizontal layout
  inherited DoAutoAdjustLayout(AMode, 1.0, AYProportion);
end;

{ TKASColorBox }

procedure TKASColorBox.SetCustomColor(AColor: TColor);
var
  Index: Integer;
begin
  for Index:= Ord(cbCustomColor in Style) to Items.Count - 1 do
  begin
    if Colors[Index] = AColor then
    begin
      Selected:= AColor;
      Exit;
    end;
  end;
  if cbCustomColor in Style then
  begin
    Items.Objects[0]:= TObject(PtrInt(AColor));
  end;
  Items.AddObject('$' + HexStr(AColor, 8), TObject(PtrInt(AColor)));
  Selected:= AColor;
end;

function TKASColorBox.PickCustomColor: Boolean;
begin
  Result:= inherited PickCustomColor;
  SetCustomColor(Colors[0]);
end;

procedure TKASColorBox.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  if csDesigning in ComponentState then Exit;
  if (Parent = nil) or (not Parent.HandleAllocated) then Exit;

  CalculateSize(Self, PreferredWidth, PreferredHeight);
  PreferredWidth+= ColorRectWidth + ColorRectOffset;
end;

procedure TKASColorBox.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    ColorRectWidth:= Round(ColorRectWidth * AXProportion);
  end;
  // Don't auto adjust horizontal layout
  inherited DoAutoAdjustLayout(AMode, 1.0, AYProportion);
end;

constructor TKASColorBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style:= DEF_COLOR_STYLE;
  DefaultColorColor:= clNone;
end;

{ TKASColorBoxButton }

function TKASColorBoxButton.GetSelected: TColor;
begin
  Result:= FColorBox.Selected;
end;

function TKASColorBoxButton.GetStyle: TColorBoxStyle;
begin
  Result:= FColorBox.Style;
end;

function TKASColorBoxButton.GetOnChange: TNotifyEvent;
begin
  Result:= FColorBox.OnChange;
end;

procedure TKASColorBoxButton.SetSelected(AValue: TColor);
begin
  FColorBox.SetCustomColor(AValue);
end;

procedure TKASColorBoxButton.SetStyle(AValue: TColorBoxStyle);
begin
  FColorBox.Style:= AValue;
end;

procedure TKASColorBoxButton.SetOnChange(AValue: TNotifyEvent);
begin
  FColorBox.OnChange:= AValue;
end;

procedure TKASColorBoxButton.DoAutoSize;
begin
  inherited DoAutoSize;
  FButton.Constraints.MinWidth:= FButton.Height;
end;

procedure TKASColorBoxButton.ButtonClick(Sender: TObject);
Var
  FreeDialog: Boolean;
begin
  if csDesigning in ComponentState then Exit;
  with FColorBox do
  begin
    FreeDialog:= (ColorDialog = nil);
    if FreeDialog then
    begin
      ColorDialog:= TColorDialog.Create(GetTopParent);
    end;
    try
      with ColorDialog do
      begin
        Color:= FColorBox.Selected;
        if Execute Then
         begin
           FColorBox.SetCustomColor(Color);
           Invalidate;
         end;
      end;
    finally
      if FreeDialog Then
      begin
        ColorDialog.Free;
        ColorDialog:= nil;
      end;
    end;
  end;
end;

class function TKASColorBoxButton.GetControlClassDefaultSize: TSize;
begin
  Result:= TKASColorBox.GetControlClassDefaultSize;
  Result.cx += Result.cy;
end;

procedure TKASColorBoxButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if csDesigning in ComponentState then
  begin
    with GetControlClassDefaultSize do
    begin
      PreferredWidth:= cx;
      PreferredHeight:= cy;
    end;
  end
  else begin
    FColorBox.CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
    PreferredWidth += FButton.Width;
  end;
end;

procedure TKASColorBoxButton.CMParentColorChanged(var Message: TLMessage);
begin
  if inherited ParentColor then
  begin
    inherited SetColor(Parent.Color);
    inherited ParentColor:= True;
  end;
end;

constructor TKASColorBoxButton.Create(AOwner: TComponent);
begin
  FButton:= TSpeedButton.Create(Self);
  FColorBox:= TKASColorBox.Create(Self);

  inherited Create(AOwner);

  ControlStyle:= ControlStyle + [csNoFocus];
  FColorBox.ParentColor:= False;
  BorderStyle:= bsNone;
  TabStop:= True;
  inherited TabStop:= False;

  with FButton do
  begin
    Align:= alRight;
    Caption:= '..';
    OnClick:= @ButtonClick;
    Parent:= Self;
  end;
  with FColorBox do
  begin
    Align:= alClient;
    ParentColor:= False;
    ParentFont:= True;
    Parent:= Self;
  end;

  AutoSize:= True;
  Color:= clWindow;
  inherited ParentColor:= True;
end;

procedure TKASColorBoxButton.SetFocus;
begin
  FColorBox.SetFocus;
end;

function TKASColorBoxButton.Focused: Boolean;
begin
  Result:= FColorBox.Focused;
end;

end.
