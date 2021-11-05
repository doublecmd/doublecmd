{
    Double Commander
    -------------------------------------------------------------------------
    Control like TButton which does not steal focus on click

    Copyright (C) 2021 Alexander Koblov (alexx2000@mail.ru)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit KASButton;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Themes;

type

  { TKASButton }

  TKASButton = class(TPanel)
  private
    FState: TButtonState;
    FShowCaption: Boolean;
    FButtonGlyph: TButtonGlyph;
    function GetGlyph: TBitmap;
    function IsGlyphStored: Boolean;
    procedure SetGlyph(AValue: TBitmap);
    procedure SetShowCaption(AValue: Boolean);
    function GetDrawDetails: TThemedElementDetails;
  protected
    procedure Paint; override;
    procedure DoExit; override;
    procedure DoEnter; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  protected
    procedure GlyphChanged(Sender: TObject);
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsGlyphStored;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
  end;

procedure Register;

implementation

uses
  LCLType, LCLProc, LCLIntf, ActnList;

procedure Register;
begin
  RegisterComponents('KASComponents',[TKASButton]);
end;

{ TKASButton }

procedure TKASButton.DoEnter;
begin
  inherited DoEnter;
  FState:= bsExclusive;
  Invalidate;
end;

procedure TKASButton.DoExit;
begin
  inherited DoExit;
  FState:= bsUp;
  Invalidate;
end;

function TKASButton.GetDrawDetails: TThemedElementDetails;
var
  Detail: TThemedButton;
begin
  if not IsEnabled then
    Detail := tbPushButtonDisabled
  else if FState = bsDown then
    Detail := tbPushButtonPressed
  else if FState = bsHot then
    Detail := tbPushButtonHot
  else if FState = bsExclusive then
    Detail := tbPushButtonDefaulted
  else begin
    Detail := tbPushButtonNormal;
  end;
  Result := ThemeServices.GetElementDetails(Detail)
end;

procedure TKASButton.SetShowCaption(AValue: Boolean);
begin
  if FShowCaption = AValue then Exit;
  FShowCaption:= AValue;
  Invalidate;
end;

function TKASButton.GetGlyph: TBitmap;
begin
  Result:= FButtonGlyph.Glyph;
end;

function TKASButton.IsGlyphStored: Boolean;
var
  Act: TCustomAction;
begin
  if Action <> nil then
  begin
    Result:= True;
    Act:= TCustomAction(Action);
    if (Act.ActionList <> nil) and (Act.ActionList.Images <> nil) and
      (Act.ImageIndex >= 0) and (Act.ImageIndex < Act.ActionList.Images.Count) then
        Result := False;
  end
  else Result:= (FButtonGlyph.Glyph <> nil) and (not FButtonGlyph.Glyph.Empty) and
                (FButtonGlyph.Glyph.Width > 0) and (FButtonGlyph.Glyph.Height > 0);
end;

procedure TKASButton.SetGlyph(AValue: TBitmap);
begin
  FButtonGlyph.Glyph := AValue;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TKASButton.Paint;
var
  APoint: TPoint;
  SysFont: TFont;
  PaintRect: TRect;
  TextFlags: Integer;
  Details: TThemedElementDetails;
begin
  PaintRect:= ClientRect;
  Details:= GetDrawDetails;
  ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
  PaintRect := ThemeServices.ContentRect(Canvas.Handle, Details, PaintRect);

  if FShowCaption and (Caption <> EmptyStr) then
  begin
    TextFlags := DT_CENTER or DT_VCENTER;
    if UseRightToLeftReading then begin
      TextFlags := TextFlags or DT_RTLREADING;
    end;

    SysFont := Screen.SystemFont;

    if (SysFont.Color = Font.Color) and
       ((SysFont.Name = Font.Name) or IsFontNameDefault(Font.Name)) and
       (SysFont.Pitch = Font.Pitch) and (SysFont.Style = Font.Style) then
    begin
      ThemeServices.DrawText(Canvas, Details, Caption, PaintRect, TextFlags, 0);
    end
    else begin
      Canvas.Brush.Style := bsClear;
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), PaintRect, TextFlags);
    end;
  end
  else if not FButtonGlyph.Glyph.Empty then
  begin
    APoint.X:= (PaintRect.Width - FButtonGlyph.Width) div 2;
    APoint.Y:= (PaintRect.Height - FButtonGlyph.Height) div 2;
    FButtonGlyph.Draw(Canvas, PaintRect, APoint, FState, True, 0);
  end;
end;

procedure TKASButton.MouseEnter;
begin
  inherited MouseEnter;
  FState:= bsHot;
  Invalidate;
end;

procedure TKASButton.MouseLeave;
begin
  inherited MouseLeave;
  FState:= bsUp;
  Invalidate;
end;

procedure TKASButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if (Key in [VK_SPACE, VK_RETURN]) and (Shift = []) then
  begin
    FState:= bsUp;
    Invalidate;
    Click;
  end;
end;

procedure TKASButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key in [VK_SPACE, VK_RETURN]) and (Shift = []) then
  begin
    FState:= bsDown;
    Invalidate;
  end;
end;

procedure TKASButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FState:= bsUp;
  Invalidate;
end;

procedure TKASButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FState:= bsDown;
  Invalidate;
end;

procedure TKASButton.GlyphChanged(Sender: TObject);
begin
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TKASButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);

  if Sender is TCustomAction then
  begin
    with TCustomAction(Sender) do
    begin
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        ActionList.Images.GetBitmap(ImageIndex, Glyph);
    end;
  end;
end;

procedure TKASButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: Integer; WithThemeSpace: Boolean);
var
  PaintRect: TRect;
  ClientRect: TRect;
  Details: TThemedElementDetails;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  if (not FButtonGlyph.Glyph.Empty) then
  begin
    Details:= GetDrawDetails;
    PaintRect:= TRect.Create(0, 0, 32, 32);
    ClientRect:= ThemeServices.ContentRect(Canvas.Handle, Details, PaintRect);

    PreferredWidth:= Abs(PaintRect.Width - ClientRect.Width) + FButtonGlyph.Width;
    PreferredHeight:= Abs(PaintRect.Height - ClientRect.Height) + FButtonGlyph.Height;
  end;
end;

constructor TKASButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FButtonGlyph := TButtonGlyph.Create;
  FButtonGlyph.NumGlyphs := 1;
  FButtonGlyph.OnChange := GlyphChanged;
  FButtonGlyph.IsDesigning := csDesigning in ComponentState;

  FShowCaption:= True;
  TabStop:= True;
end;

destructor TKASButton.Destroy;
begin
  FreeAndNil(FButtonGlyph);
  inherited Destroy;
end;

end.
