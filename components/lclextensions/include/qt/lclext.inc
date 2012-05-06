function DirectMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP): Boolean;
begin
  //todo: see if is possible todo it faster
  Result := StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width, Height,
   Mask, 0, 0,  SRCCOPY);
end;
