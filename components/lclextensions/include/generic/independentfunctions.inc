{$ifndef HAS_BITBLT}
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc,
  YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := LCLIntf.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop);
end;
{$endif}
