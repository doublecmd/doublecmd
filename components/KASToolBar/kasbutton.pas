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
  public
    constructor Create(TheOwner: TComponent); override;
  end;

procedure Register;

implementation

uses
  LCLType, LCLProc, LCLIntf;

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

procedure TKASButton.Paint;
var
  SysFont: TFont;
  PaintRect: TRect;
  TextFlags: Integer;
  Details: TThemedElementDetails;
begin
  PaintRect:= ClientRect;
  Details:= GetDrawDetails;
  ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
  PaintRect := ThemeServices.ContentRect(Canvas.Handle, Details, PaintRect);

  if Caption <> EmptyStr then
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

constructor TKASButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  TabStop:= True;
end;

end.
