unit uSmoothScrollingGrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Grids;

type

  { TSmoothScrollingGrid }

  TSmoothScrollingGrid = class( TDrawGrid )
  private
    _wheelDeltaAccumulator: Array [Boolean] of Integer;
  private
    function calcCurrentDelta( isVert: Boolean; WheelDelta: Integer ): Integer;
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function DoMouseWheelHorz(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  end;

implementation

const
  WHEEL_DELTA_UNIT = 120;

{ TSmoothScrollingGrid }

function TSmoothScrollingGrid.calcCurrentDelta(isVert: Boolean; WheelDelta: Integer): Integer;
begin
  if ((WheelDelta>0) and (_wheelDeltaAccumulator[isVert]<0)) or
     ((WheelDelta<0) and (_wheelDeltaAccumulator[isVert]>0)) then
    _wheelDeltaAccumulator[isVert]:= 0;
  inc( _wheelDeltaAccumulator[isVert], WheelDelta );

  Result:= 0;
  if _wheelDeltaAccumulator[isVert] >= WHEEL_DELTA_UNIT then
    Result:= WHEEL_DELTA_UNIT
  else if _wheelDeltaAccumulator[isVert] <= -WHEEL_DELTA_UNIT then
    Result:= -WHEEL_DELTA_UNIT;

  if Result <> 0 then
    dec( _wheelDeltaAccumulator[isVert], Result );
end;

function TSmoothScrollingGrid.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  currentDelta: Integer;
begin
  Result:= True;
  currentDelta:= calcCurrentDelta( True, WheelDelta );
  if currentDelta <> 0 then
    Result:= inherited DoMouseWheel(Shift, currentDelta, MousePos);
end;

function TSmoothScrollingGrid.DoMouseWheelHorz(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  currentDelta: Integer;
begin
  Result:= True;
  currentDelta:= calcCurrentDelta( False, WheelDelta );
  if currentDelta <> 0 then
    Result:= inherited DoMouseWheelHorz(Shift, currentDelta, MousePos);
end;

end.

