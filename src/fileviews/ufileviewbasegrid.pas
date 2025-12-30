unit uFileViewBaseGrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uSmoothScrollingGrid;

type

  { TFileViewBaseGrid }

  TFileViewBaseGrid = class( TSmoothScrollingGrid )
  public
    function MouseOnGrid(X, Y: LongInt): Boolean;
    procedure MouseToCellWithoutOutbound(X, Y: Integer; out ACol, ARow: Longint);
    function CellToIndex(ACol, ARow: Integer): Integer; virtual; abstract;
  end;

implementation

{ TFileViewBaseGrid }

function TFileViewBaseGrid.MouseOnGrid(X, Y: LongInt): Boolean;
var
  bTemp: Boolean;
  iRow, iCol: LongInt;
begin
  bTemp:= AllowOutboundEvents;
  AllowOutboundEvents:= False;
  MouseToCell(X, Y, iCol, iRow);
  AllowOutboundEvents:= bTemp;
  Result:= not (CellToIndex(iCol, iRow) < 0);
end;

procedure TFileViewBaseGrid.MouseToCellWithoutOutbound(X, Y: Integer; out ACol,
  ARow: Longint);
var
  bTemp: Boolean;
begin
  bTemp:= AllowOutboundEvents;
  AllowOutboundEvents:= False;
  MouseToCell(X, Y, ACol, ARow);
  AllowOutboundEvents:= bTemp;
end;

end.

