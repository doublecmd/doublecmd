

function DirectMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP): Boolean;
begin
  //todo: see if is possible todo it faster
  Result := GTKWidgetSet.StretchCopyArea(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width, Height,
   Mask, XSrc, YSrc,  SRCCOPY);
end;
