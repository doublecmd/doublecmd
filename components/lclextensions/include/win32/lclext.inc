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
