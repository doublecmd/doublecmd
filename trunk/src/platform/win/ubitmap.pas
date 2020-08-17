{
   Double Commander
   -------------------------------------------------------------------------
   Windows specific bitmap functions

   Copyright (C) 2020 Alexander Koblov (alexx2000@mail.ru)

   Based on Win32Proc.pas from the Lazarus Component Library (LCL)

   See the file COPYING.modifiedLGPL.txt, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit uBitmap;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, Classes, Graphics, Windows;

function BitmapCreateFromHICON(Handle: HICON): Graphics.TBitmap;
function BitmapCreateFromHBITMAP(Handle: HBITMAP): Graphics.TBitmap;

implementation

uses
  FPImage, GraphType, Forms, IntfGraphics
{$IF DEFINED(LCLQT5)}
  , SysUtils, LCLProc
{$ENDIF}
  ;

{$IF DEFINED(LCLQT5)}

procedure FillRawImageDescriptionColors(var ADesc: TRawImageDescription);
begin
  case ADesc.BitsPerPixel of
    1,4,8:
      begin
        // palette mode, no offsets
        ADesc.Format := ricfGray;
        ADesc.RedPrec := ADesc.BitsPerPixel;
        ADesc.GreenPrec := 0;
        ADesc.BluePrec := 0;
        ADesc.RedShift := 0;
        ADesc.GreenShift := 0;
        ADesc.BlueShift := 0;
      end;
    16:
      begin
        // 5-5-5 mode
        ADesc.RedPrec := 5;
        ADesc.GreenPrec := 5;
        ADesc.BluePrec := 5;
        ADesc.RedShift := 10;
        ADesc.GreenShift := 5;
        ADesc.BlueShift := 0;
        ADesc.Depth := 15;
      end;
    24:
      begin
        // 8-8-8 mode
        ADesc.RedPrec := 8;
        ADesc.GreenPrec := 8;
        ADesc.BluePrec := 8;
        ADesc.RedShift := 16;
        ADesc.GreenShift := 8;
        ADesc.BlueShift := 0;
      end;
  else    //  32:
    // 8-8-8-8 mode, high byte can be native alpha or custom 1bit maskalpha
    ADesc.AlphaPrec := 8;
    ADesc.RedPrec := 8;
    ADesc.GreenPrec := 8;
    ADesc.BluePrec := 8;
    ADesc.AlphaShift := 24;
    ADesc.RedShift := 16;
    ADesc.GreenShift := 8;
    ADesc.BlueShift := 0;
    ADesc.Depth := 32;
  end;
end;

procedure FillRawImageDescription(const ABitmapInfo: Windows.TBitmap; out ADesc: TRawImageDescription);
begin
  ADesc.Init;
  ADesc.Format := ricfRGBA;
  ADesc.Depth := ABitmapInfo.bmBitsPixel;             // used bits per pixel
  ADesc.Width := ABitmapInfo.bmWidth;
  ADesc.Height := ABitmapInfo.bmHeight;
  ADesc.BitOrder := riboReversedBits;
  ADesc.ByteOrder := riboLSBFirst;
  ADesc.LineOrder := riloTopToBottom;
  ADesc.BitsPerPixel := ABitmapInfo.bmBitsPixel;      // bits per pixel. can be greater than Depth.
  ADesc.LineEnd := rileDWordBoundary;

  if ABitmapInfo.bmBitsPixel <= 8
  then begin
    // each pixel is an index in the palette
    // TODO, ColorCount
    ADesc.PaletteColorCount := 0;
  end
  else ADesc.PaletteColorCount := 0;

  FillRawImageDescriptionColors(ADesc);

  ADesc.MaskBitsPerPixel := 1;
  ADesc.MaskShift := 0;
  ADesc.MaskLineEnd := rileWordBoundary; // CreateBitmap requires word boundary
  ADesc.MaskBitOrder := riboReversedBits;
end;

function GetBitmapOrder(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP): TRawImageLineOrder;

  procedure DbgLog(const AFunc: String);
  begin
    DebugLn('GetBitmapOrder - GetDIBits ', AFunc, ' failed: ', SysErrorMessage(Windows.GetLastError));
  end;

var
  SrcPixel: PCardinal absolute AWinBmp.bmBits;
  OrgPixel, TstPixel: Cardinal;
  Scanline: Pointer;
  DC: HDC;
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[Byte] of Cardinal; // reserve extra color for colormasks
  end;

  FullScanLine: Boolean; // win9x requires a full scanline to be retrieved
                         // others won't fail when one pixel is requested
begin
  if AWinBmp.bmBits = nil
  then begin
    // no DIBsection so always bottom-up
    Exit(riloBottomToTop);
  end;

  // try to figure out the orientation of the given bitmap.
  // Unfortunately MS doesn't provide a direct function for this.
  // So modify the first pixel to see if it changes. This pixel is always part
  // of the first scanline of the given bitmap.
  // When we request the data through GetDIBits as bottom-up, windows adjusts
  // the data when it is a top-down. So if the pixel doesn't change the bitmap
  // was internally a top-down image.

  FullScanLine := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;
  if FullScanLine
  then ScanLine := GetMem(AWinBmp.bmWidthBytes)
  else ScanLine := nil;

  FillChar(Info.Header, sizeof(Windows.TBitmapInfoHeader), 0);
  Info.Header.biSize := sizeof(Windows.TBitmapInfoHeader);
  DC := Windows.GetDC(0);
  if Windows.GetDIBits(DC, ABitmap, 0, 1, nil, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
  then begin
    DbgLog('Getinfo');
    // failed ???
    Windows.ReleaseDC(0, DC);
    Exit(riloBottomToTop);
  end;

  // Get only 1 pixel (or full scanline for win9x)
  OrgPixel := 0;
  if FullScanLine
  then begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, ScanLine, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then DbgLog('OrgPixel')
    else OrgPixel := PCardinal(ScanLine)^;
  end
  else begin
    Info.Header.biWidth := 1;
    if Windows.GetDIBits(DC, ABitmap, 0, 1, @OrgPixel, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then DbgLog('OrgPixel');
  end;

  // modify pixel
  SrcPixel^ := not SrcPixel^;

  // get test
  TstPixel := 0;
  if FullScanLine
  then begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, ScanLine, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then DbgLog('TstPixel')
    else TstPixel := PCardinal(ScanLine)^;
  end
  else begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, @TstPixel, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then DbgLog('TstPixel');
  end;

  if OrgPixel = TstPixel
  then Result := riloTopToBottom
  else Result := riloBottomToTop;

  // restore pixel & cleanup
  SrcPixel^ := not SrcPixel^;
  Windows.ReleaseDC(0, DC);
  if FullScanLine
  then FreeMem(Scanline);
end;

function GetBitmapBytes(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; const ARect: TRect; ALineEnd: TRawImageLineEnd; ALineOrder: TRawImageLineOrder; out AData: Pointer; out ADataSize: PtrUInt): Boolean;
var
  DC: HDC;
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[Byte] of TRGBQuad; // reserve extra colors for palette (256 max)
  end;
  H: Cardinal;
  R: TRect;
  SrcData: PByte;
  SrcSize: PtrUInt;
  SrcLineBytes: Cardinal;
  SrcLineOrder: TRawImageLineOrder;
  StartScan: Integer;
begin
  SrcLineOrder := GetBitmapOrder(AWinBmp, ABitmap);
  SrcLineBytes := (AWinBmp.bmWidthBytes + 3) and not 3;

  if AWinBmp.bmBits <> nil
  then begin
    // this is bitmapsection data :) we can just copy the bits

    // We cannot trust windows with bmWidthBytes. Use SrcLineBytes which takes
    // DWORD alignment into consideration
    with AWinBmp do
      Result := CopyImageData(bmWidth, bmHeight, SrcLineBytes, bmBitsPixel, bmBits, ARect, SrcLineOrder, ALineOrder, ALineEnd, AData, ADataSize);
    Exit;
  end;

  // retrieve the data though GetDIBits

  // initialize bitmapinfo structure
  Info.Header.biSize := sizeof(Info.Header);
  Info.Header.biPlanes := 1;
  Info.Header.biBitCount := AWinBmp.bmBitsPixel;
  Info.Header.biCompression := BI_RGB;
  Info.Header.biSizeImage := 0;

  Info.Header.biWidth := AWinBmp.bmWidth;
  H := ARect.Bottom - ARect.Top;
  // request a top-down DIB
  if AWinBmp.bmHeight > 0
  then begin
    Info.Header.biHeight := -AWinBmp.bmHeight;
    StartScan := AWinBmp.bmHeight - ARect.Bottom;
  end
  else begin
    Info.Header.biHeight := AWinBmp.bmHeight;
    StartScan := ARect.Top;
  end;
  // adjust height
  if StartScan < 0
  then begin
    Inc(H, StartScan);
    StartScan := 0;
  end;

  // alloc buffer
  SrcSize := SrcLineBytes * H;
  GetMem(SrcData, SrcSize);

  DC := Windows.GetDC(0);
  Result := Windows.GetDIBits(DC, ABitmap, StartScan, H, SrcData, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) <> 0;
  Windows.ReleaseDC(0, DC);

  // since we only got the needed scanlines, adjust top and bottom
  R.Left := ARect.Left;
  R.Top := 0;
  R.Right := ARect.Right;
  R.Bottom := H;

  with Info.Header do
    Result := Result and CopyImageData(biWidth, H, SrcLineBytes, biBitCount, SrcData, R, riloTopToBottom, ALineOrder, ALineEnd, AData, ADataSize);

  FreeMem(SrcData);
end;

function RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
var
  WinDIB: Windows.TDIBSection;
  WinBmp: Windows.TBitmap absolute WinDIB.dsBm;
  ASize: Integer;
  R: TRect;
begin
  ARawImage.Init;
  FillChar(WinDIB, SizeOf(WinDIB), 0);
  ASize := Windows.GetObject(ABitmap, SizeOf(WinDIB), @WinDIB);
  if ASize = 0
  then Exit(False);

  //DbgDumpBitmap(ABitmap, 'FromBitmap - Image');
  //DbgDumpBitmap(AMask, 'FromMask - Mask');

  FillRawImageDescription(WinBmp, ARawImage.Description);
  // if it is not DIB then alpha in bitmaps is not supported => use 0 alpha prec
  if ASize < SizeOf(WinDIB) then
    ARawImage.Description.AlphaPrec := 0;

  if ARect = nil
  then begin
    R := Classes.Rect(0, 0, WinBmp.bmWidth, WinBmp.bmHeight);
  end
  else begin
    R := ARect^;
    if R.Top > WinBmp.bmHeight then
      R.Top := WinBmp.bmHeight;
    if R.Bottom > WinBmp.bmHeight then
      R.Bottom := WinBmp.bmHeight;
    if R.Left > WinBmp.bmWidth then
      R.Left := WinBmp.bmWidth;
    if R.Right > WinBmp.bmWidth then
      R.Right := WinBmp.bmWidth;
  end;

  ARawImage.Description.Width := R.Right - R.Left;
  ARawImage.Description.Height := R.Bottom - R.Top;

  // copy bitmap
  Result := GetBitmapBytes(WinBmp, ABitmap, R, ARawImage.Description.LineEnd, ARawImage.Description.LineOrder, ARawImage.Data, ARawImage.DataSize);

  // check mask
  if AMask <> 0 then
  begin
    if Windows.GetObject(AMask, SizeOf(WinBmp), @WinBmp) = 0
    then Exit(False);

    Result := GetBitmapBytes(WinBmp, AMask, R, ARawImage.Description.MaskLineEnd, ARawImage.Description.LineOrder, ARawImage.Mask, ARawImage.MaskSize);
  end
  else begin
    ARawImage.Description.MaskBitsPerPixel := 0;
  end;
end;

{$ENDIF}

function BitmapCreateFromHICON(Handle: HICON): Graphics.TBitmap;
var
  Index: Integer;
  IconInfo: TIconInfo;
  ARawImage: TRawImage;
  AImage: TLazIntfImage;
begin
  Result:= Graphics.TBitmap.Create;

  if Windows.GetIconInfo(Handle, IconInfo) = False then
    Exit;

  if RawImage_FromBitmap(ARawImage, IconInfo.hbmColor, IconInfo.hbmMask) then
  begin
    // Check if the bitmap has alpha channel
    if (ARawImage.Description.BitsPerPixel = 32) and (ScreenInfo.ColorDepth = 32) then
    begin
      for Index:= 0 to (ARawImage.DataSize div 4) - 1 do
      begin
        if (PLongWord(ARawImage.Data)[Index] shr ARawImage.Description.AlphaShift) and $FF <> 0 then
        begin
          ARawImage.Description.AlphaPrec:= 8;
          Break;
        end;
      end;
      // Invalid alpha channel, use mask instead
      if ARawImage.Description.AlphaPrec = 0 then
      begin
        ARawImage.Description.AlphaPrec:= 8;
        AImage:= TLazIntfImage.Create(ARawImage, False);
        AImage.AlphaFromMask(False);
        AImage.Free;
      end;
    end;
    Result.LoadFromRawImage(ARawImage, True);
  end;
  Windows.DeleteObject(IconInfo.hbmMask);
  Windows.DeleteObject(IconInfo.hbmColor);
end;

function BitmapCreateFromHBITMAP(Handle: HBITMAP): Graphics.TBitmap;
{$IF DEFINED(LCLWIN32)}
begin
  Result:= Graphics.TBitmap.Create;
  Result.Handle:= Handle;
end;
{$ELSE}
var
  ARawImage: TRawImage;
begin
  Result:= Graphics.TBitmap.Create;
  if RawImage_FromBitmap(ARawImage, Handle, 0) then
  begin
    Result.LoadFromRawImage(ARawImage, True);
  end;
end;
{$ENDIF}

end.

