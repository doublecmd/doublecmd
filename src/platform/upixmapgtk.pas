unit uPixMapGtk;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IntfGraphics,
  gtkdef, gdk2pixbuf, gdk2, glib2;

procedure DrawPixbufAtCanvas(Canvas: TCanvas; Pixbuf : PGdkPixbuf; SrcX, SrcY, DstX, DstY, Width, Height: Integer);
function PixBufToBitmap(Pixbuf: PGdkPixbuf): TBitmap;

implementation

uses
  GraphType;

procedure DrawPixbufAtCanvas(Canvas: TCanvas; Pixbuf : PGdkPixbuf; SrcX, SrcY, DstX, DstY, Width, Height: Integer);
var
  gdkDrawable : PGdkDrawable;
  gdkGC : PGdkGC;
  gtkDC : TGtkDeviceContext;
  iPixbufWidth, iPixbufHeight: Integer;
  StretchedPixbuf: PGdkPixbuf;
begin
  gtkDC := TGtkDeviceContext(Canvas.Handle);
  gdkDrawable := gtkDC.Drawable;
  gdkGC := gdk_gc_new(gdkDrawable);

  iPixbufWidth := gdk_pixbuf_get_width(Pixbuf);
  iPixbufHeight := gdk_pixbuf_get_height(Pixbuf);

  if (Width <> iPixbufWidth) or (Height <> iPixbufHeight) then
  begin
    StretchedPixbuf := gdk_pixbuf_scale_simple(Pixbuf, Width, Height, GDK_INTERP_BILINEAR);
    gdk_draw_pixbuf(gdkDrawable, gdkGC, StretchedPixbuf, SrcX, SrcY, DstX, DstY, -1, -1, GDK_RGB_DITHER_NONE, 0, 0);
    gdk_pixbuf_unref(StretchedPixbuf);
  end
  else
    gdk_draw_pixbuf(gdkDrawable, gdkGC, Pixbuf, SrcX, SrcY, DstX, DstY, -1, -1, GDK_RGB_DITHER_NONE, 0, 0);
  g_object_unref(gdkGC);
end;

function PixBufToBitmap(Pixbuf: PGdkPixbuf): TBitmap;
var
  width, height, rowstride, n_channels, i, j: Integer;
  pixels: Pguchar;
  pSrc: PByte;
  pDst: PLongWord;
  BmpData: TLazIntfImage;
  hasAlphaChannel: Boolean;
  QueryFlags: TRawImageQueryFlags = [riqfRGB];
  Description: TRawImageDescription;
begin
  Result := nil;

  n_channels:= gdk_pixbuf_get_n_channels(Pixbuf);

  if ((n_channels <> 3) and (n_channels <> 4)) or  // RGB or RGBA
     (gdk_pixbuf_get_colorspace(pixbuf) <> GDK_COLORSPACE_RGB) or
     (gdk_pixbuf_get_bits_per_sample(pixbuf) <> 8) then Exit;

  width:= gdk_pixbuf_get_width(Pixbuf);
  height:= gdk_pixbuf_get_height(Pixbuf);
  rowstride:= gdk_pixbuf_get_rowstride(Pixbuf);
  pixels:= gdk_pixbuf_get_pixels(Pixbuf);
  hasAlphaChannel:= gdk_pixbuf_get_has_alpha(Pixbuf);

  if hasAlphaChannel then
    Include(QueryFlags, riqfAlpha);

  BmpData := TLazIntfImage.Create(width, height, QueryFlags);
  try
    BmpData.CreateData;
    Description := BmpData.DataDescription;

    pDst := PLongWord(BmpData.PixelData);
    for j:= 0 to Height - 1 do
    begin
      pSrc := PByte(pixels) + j * rowstride;
      for i:= 0 to Width - 1 do
      begin
        pDst^ := pSrc[0] shl Description.RedShift +
                 pSrc[1] shl Description.GreenShift +
                 pSrc[2] shl Description.BlueShift;

        if hasAlphaChannel then
          pDst^ := pDst^ + pSrc[3] shl Description.AlphaShift;

        Inc(pSrc, n_channels);
        Inc(pDst);
      end;
    end;

    Result := TBitmap.Create;
    Result.LoadFromIntfImage(BmpData);
    if not hasAlphaChannel then
      Result.Transparent := True;

  finally
    BmpData.Free;
  end;
end;

// or use this
{
begin
  iPixbufWidth := gdk_pixbuf_get_width(pbPicture);
  iPixbufHeight := gdk_pixbuf_get_height(pbPicture);

  Result := TBitMap.Create;
  Result.SetSize(iPixbufWidth, iPixbufHeight);
  Result.Canvas.Brush.Color := clBackColor;
  Result.Canvas.FillRect(0, 0, iPixbufWidth, iPixbufHeight);

  DrawPixbufAtCanvas(Result.Canvas, pbPicture, 0, 0, 0, 0, iPixbufWidth, iPixbufHeight);
end;
}

end.

