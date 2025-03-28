unit KASComCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics, Dialogs;

type

  { TToolButtonClr }

  TToolButtonClr = class(TToolButton)
  private
    FButtonColor: TColor;
    FColorDialog: TColorDialog;
    procedure SetButtonColor(AValue: TColor);
  protected
    procedure Paint; override;
    procedure ShowColorDialog;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Click; override;
    property ButtonColor: TColor read FButtonColor write SetButtonColor;
  end;

  { TToolBarAdv }

  TToolBarAdv = class(TToolBar)
  private
    FToolBarFlags: TToolBarFlags;
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
                                     PreferredHeight: Integer;
                                     {%H-}WithThemeSpace: Boolean); override;
    procedure AlignControls({%H-}AControl: TControl;
                            var RemainingClientRect: TRect); override;
    function WrapButtons(UseSize: Integer; out NewWidth,
                         NewHeight: Integer; Simulate: Boolean): Boolean;
  end;

procedure Register;

implementation

uses
  Math;

{ TToolButtonClr }

procedure TToolButtonClr.SetButtonColor(AValue: TColor);
begin
  if FButtonColor <> AValue then
  begin
    FButtonColor:= AValue;
    Invalidate;
  end;
end;

procedure TToolButtonClr.Paint;
var
  ARect, IconRect: TRect;
begin
  inherited Paint;

  if (FToolBar <> nil) and (ClientWidth > 0) and (ClientHeight > 0) then
  begin
    ARect:= ClientRect;

    IconRect.Left:= (ARect.Width - FToolBar.ImagesWidth) div 2;
    IconRect.Top:= (ARect.Height - FToolBar.ImagesWidth) div 2;
    IconRect.Right:= IconRect.Left + FToolBar.ImagesWidth;
    IconRect.Bottom:= IconRect.Top + FToolBar.ImagesWidth;

    if Enabled then
    begin
      Canvas.Brush.Style:= bsSolid;
      Canvas.Brush.Color:= FButtonColor
    end
    else begin
      Canvas.Brush.Color:= clGrayText;
      Canvas.Brush.Style:= bsDiagCross;
    end;

    Canvas.Pen.Color:= clBtnText;
    Canvas.Rectangle(IconRect);
  end;
end;

procedure TToolButtonClr.ShowColorDialog;
begin
  if not Enabled then Exit;

  if (FColorDialog = nil) then
  begin
    FColorDialog := TColorDialog.Create(Self);
  end;
  FColorDialog.Color := ButtonColor;

  if FColorDialog.Execute then
  begin
    ButtonColor := FColorDialog.Color;
  end;
end;

constructor TToolButtonClr.Create(TheOwner: TComponent);
begin
  FButtonColor:= clRed;
  inherited Create(TheOwner);
end;

procedure TToolButtonClr.Click;
begin
  inherited Click;
  ShowColorDialog;
end;

{ TToolBarAdv }

procedure TToolBarAdv.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: Integer; WithThemeSpace: Boolean);
begin
  if IsVertical then
    WrapButtons(Height, PreferredWidth, PreferredHeight, True)
  else
    WrapButtons(Width, PreferredWidth, PreferredHeight, True);
end;

procedure TToolBarAdv.AlignControls(AControl: TControl;
  var RemainingClientRect: TRect);
var
  NewWidth, NewHeight: integer;
begin
  if tbfPlacingControls in FToolBarFlags then exit;
  Include(FToolBarFlags, tbfPlacingControls);
  DisableAlign;
  try
    AdjustClientRect(RemainingClientRect);
    if IsVertical then
      WrapButtons(Height, NewWidth, NewHeight, False)
    else
      WrapButtons(Width, NewWidth, NewHeight, False);
  finally
    Exclude(FToolBarFlags, tbfPlacingControls);
    EnableAlign;
  end;
end;

function TToolBarAdv.WrapButtons(UseSize: Integer; out NewWidth,
  NewHeight: Integer; Simulate: Boolean): Boolean;
var
  ARect: TRect;
  X, Y: Integer;
  Vertical: Boolean;
  LeftToRight: Boolean;
  CurControl: TControl;
  StartX, StartY: Integer;
  FRowWidth, FRowHeight: Integer;

  procedure CalculatePosition;
  var
    NewBounds: TRect;
    StartedAtRowStart: Boolean;
  begin
    if IsVertical then
    begin
      NewBounds := Bounds(X, Y, FRowWidth, CurControl.Height);
      repeat
        if (not Wrapable) or
           (NewBounds.Top = StartY) or
           (NewBounds.Bottom <= ARect.Bottom) then
        begin
          // control fits into the column
          X := NewBounds.Left;
          Y := NewBounds.Top;
          Break;
        end;

        // try next column
        NewBounds.Top := StartY;
        NewBounds.Bottom := NewBounds.Top + CurControl.Height;
        Inc(NewBounds.Left, FRowWidth);
        Inc(NewBounds.Right, FRowWidth);
      until False;
    end
    else begin
      StartedAtRowStart := (X = StartX);

      if LeftToRight then
        NewBounds := Bounds(X, Y, CurControl.Width, FRowHeight)
      else begin
        NewBounds := Bounds(X - CurControl.Width, Y, CurControl.Width, FRowHeight);
      end;

      repeat
        if (not Wrapable) or
           (StartedAtRowStart) or
           (LeftToRight and ((NewBounds.Left = StartX) or (NewBounds.Right <= ARect.Right))) or
           ((not LeftToRight) and ((NewBounds.Right = StartX) or (NewBounds.Left >= ARect.Left))) then
        begin
          // control fits into the row
          X := NewBounds.Left;
          Y := NewBounds.Top;
          Break;
        end;
        StartedAtRowStart := True;

        // try next row
        if LeftToRight then
        begin
          NewBounds.Left := StartX;
          NewBounds.Right := NewBounds.Left + CurControl.Width;
        end else begin
          NewBounds.Right := StartX;
          NewBounds.Left := NewBounds.Right - CurControl.Width;
        end;
        Inc(NewBounds.Top, FRowHeight);
        Inc(NewBounds.Bottom, FRowHeight);
      until False;
    end;
  end;

var
  I: Integer;
  W, H: Integer;
  CurClientRect: TRect;
  AdjustClientFrame: TRect;
begin
  NewWidth := 0;
  NewHeight := 0;
  Result := True;
  Vertical := IsVertical;
  FRowWidth:= ButtonWidth;
  FRowHeight:= ButtonHeight;
  if Vertical then
  begin
    LeftToRight := True;
  end
  else begin
    LeftToRight := not UseRightToLeftAlignment;
  end;
  DisableAlign;
  BeginUpdate;
  try
    CurClientRect := ClientRect;
    if Vertical then
      Inc(CurClientRect.Bottom, UseSize - Height)
    else begin
      Inc(CurClientRect.Right, UseSize - Width);
    end;
    ARect := CurClientRect;
    AdjustClientRect(ARect);
    AdjustClientFrame.Left := ARect.Left - CurClientRect.Left;
    AdjustClientFrame.Top := ARect.Top - CurClientRect.Top;
    AdjustClientFrame.Right := CurClientRect.Right - ARect.Right;
    AdjustClientFrame.Bottom := CurClientRect.Bottom - ARect.Bottom;
    //DebugLn(['TToolBar.WrapButtons ',DbgSName(Self),' ARect=',dbgs(ARect)]);
    // important: top, left button must start in the AdjustClientRect top, left
    // otherwise Toolbar.AutoSize=true will create an endless loop
    if Vertical or LeftToRight then
      StartX := ARect.Left
    else begin
      StartX := ARect.Right;
    end;
    StartY := ARect.Top;
    X := StartX;
    Y := StartY;
    for I := 0 to ButtonList.Count - 1 do
    begin
      CurControl := TControl(ButtonList[I]);

      if not CurControl.IsControlVisible then
        Continue;

      CalculatePosition;
      W := CurControl.Width;
      H := CurControl.Height;

      if (not Simulate) and ((CurControl.Left <> X) or (CurControl.Top <> Y)) then
      begin
        CurControl.SetBounds(X, Y, W, H); // Note: do not use SetBoundsKeepBase
      end;

      // adjust NewWidth, NewHeight
      if LeftToRight then
        NewWidth := Max(NewWidth, X + W + AdjustClientFrame.Right)
      else begin
        NewWidth := Max(NewWidth, ARect.Right - X + ARect.Left + AdjustClientFrame.Right);
      end;
      NewHeight := Max(NewHeight, Y + H + AdjustClientFrame.Bottom);

      // step to next position
      if IsVertical then
        Inc(Y, H)
      else if LeftToRight then
        Inc(X, W);
    end;
  finally
    EndUpdate;
    EnableAlign;
  end;
end;

procedure Register;
begin
  RegisterComponents('KASComponents', [TToolBarAdv]);
  RegisterNoIcon([TToolButtonClr]);
end;

end.

