//todo: properly implement
procedure AlphaBlend(Source, Destination: HDC; const R: TRect; const Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);
begin
  case Mode of
    bmConstantAlpha,
    bmPerPixelAlpha,
    bmMasterAlpha,
    bmConstantAlphaAndColor:
      begin
	    BitBlt(Destination, Target.X, Target.Y, R.Right - R.Left, R.Bottom - R.Top, Source, R.Left, R.Right, SRCCOPY); 
      end;
  end;
end;

function CalculateScanline(Bits: Pointer; Width, Height, Row: Integer): Pointer;
begin
  Result := nil;
end;

function GetBitmapBitsFromBitmap(Bitmap: HBITMAP): Pointer;
begin
  Result := nil;
end;