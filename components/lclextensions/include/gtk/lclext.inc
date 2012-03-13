

function CreateBitmapMask(BitmapDC: HDC; Width, Height: Integer; TransparentColor: TColor): HBITMAP;
begin
  //todo
  Result := 0;
end;

function DirectMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP): Boolean;
begin
  //todo: see if is possible todo it faster
  Result := GTKWidgetSet.StretchCopyArea(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width, Height,
   Mask, XSrc, YSrc,  SRCCOPY);
end;

function OptimalPixelFormat: TPixelFormat;
begin
  Result := pfDevice;
end;

function OSSupportsUTF16: Boolean;
begin
  Result := False;
end;
