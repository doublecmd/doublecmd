
function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints; cPoints: UINT): Integer;
var
  i: Integer;
  XOffset, YOffset: SmallInt;
  FromPoint, ToPoint: TPoint;
begin
  FromPoint := Point(0, 0);
  ToPoint := Point(0, 0);
  if hWndFrom <> 0 then
    ClientToScreen(hWndFrom, FromPoint);
  if hWndTo <> 0 then
    ClientToScreen(hWndTo, ToPoint);
  XOffset := (FromPoint.X - ToPoint.X);
  YOffset := (FromPoint.Y - ToPoint.Y);
  for i := 0 to cPoints - 1 do
  begin
    PPoint(@lpPoints)[i].x := XOffset + PPoint(@lpPoints)[i].x;
    PPoint(@lpPoints)[i].y := YOffset + PPoint(@lpPoints)[i].y;
  end;
  Result := MakeLong(XOffset, YOffset);
end;

{$ifndef HAS_BITBLT}
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc,
  YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := LCLIntf.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop);
end;
{$endif}
