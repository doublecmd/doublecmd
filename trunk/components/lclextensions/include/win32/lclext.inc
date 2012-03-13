

function CreateBitmapMask(BitmapDC: HDC; Width, Height: Integer; TransparentColor: TColor): HBITMAP;
var
  OldColor: COLORREF;
  OldObj: HBITMAP;
  MaskDC: HDC;
begin
  Result := Windows.CreateBitmap(Width,Height,1,1,nil);
  MaskDC := Windows.CreateCompatibleDC(BitmapDC);
  
  OldObj := Windows.SelectObject(MaskDC,Result);
  OldColor := Windows.SetBkColor(BitmapDC, Windows.COLORREF(ColorToRGB(TransparentColor)));

  Windows.BitBlt(MaskDC,0,0,Width,Height,BitmapDC,0,0,SRCCOPY);

  Windows.SetBkColor(BitmapDC,OldColor);
  Windows.SelectObject(MaskDC,OldObj);
  Windows.DeleteDC(MaskDC);
end;

function DirectMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP): Boolean;
var
  MaskDC: HDC;
  MaskObj: HGDIOBJ;
  PrevTextColor, PrevBkColor: COLORREF;
begin
  //this is a stripped version of LCL.StretchMaskBlt
  if Mask <> 0 then
  begin
    MaskDC := Windows.CreateCompatibleDC(DestDC);
    MaskObj := Windows.SelectObject(MaskDC, Mask);

    PrevTextColor := Windows.SetTextColor(DestDC, $00000000);
    PrevBkColor := Windows.SetBkColor(DestDC, $00FFFFFF);
    
    Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SRCINVERT);
    Windows.BitBlt(DestDC, X, Y, Width, Height, MaskDC, XSrc, YSrc, SRCAND);
    Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC,  XSrc, YSrc, SRCINVERT);
    
    Windows.SetTextColor(DestDC, PrevTextColor);
    Windows.SetBkColor(DestDC, PrevBkColor);
    Windows.SelectObject(MaskDC, MaskObj);
    Windows.DeleteDC(MaskDC);
  end
  else
    Result := Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, SRCCOPY);
end;

function OptimalPixelFormat: TPixelFormat;
begin
  if ScreenInfo.ColorDepth = 32 then
    Result := pf32bit
  else
    Result := pfDevice;
end;

function OSSupportsUTF16: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
end;
