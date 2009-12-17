unit uPixMapGtk;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IntfGraphics,
  gtkdef, gtk2, gdk2pixbuf, gdk2, glib2;

procedure DrawPixbufAtCanvas(Canvas: TCanvas; Pixbuf : PGdkPixbuf; SrcX, SrcY, DstX, DstY, Width, Height: Integer);
function PixBufToBitmap(Pixbuf: PGdkPixbuf): TBitmap;

implementation

procedure DrawPixbufAtCanvas(Canvas: TCanvas; Pixbuf : PGdkPixbuf; SrcX, SrcY, DstX, DstY, Width, Height: Integer);
var
  gdkDrawable : PGdkDrawable;
  gdkGC : PGdkGC;
  gtkDC : TGtkDeviceContext;
begin
  gtkDC := TGtkDeviceContext(Canvas.Handle);
  gdkDrawable := gtkDC.Drawable;
  gdkGC := gdk_gc_new(gdkDrawable);
  gdk_draw_pixbuf(gdkDrawable, gdkGC, Pixbuf, SrcX, SrcY, DstX, DstY, Width, Height, GDK_RGB_DITHER_NONE, 0, 0);
  g_object_unref(gdkGC)
end;

function PixBufToBitmap(Pixbuf: PGdkPixbuf): TBitmap;
var
  width, height, rowstride, n_channels, i, j: Integer;
  pixels, p: Pguchar;
  BmpData: TLazIntfImage;
begin
  n_channels:= gdk_pixbuf_get_n_channels(Pixbuf);
  width:= gdk_pixbuf_get_width(Pixbuf);
  height:= gdk_pixbuf_get_height(Pixbuf);
  rowstride:= gdk_pixbuf_get_rowstride(Pixbuf);
  pixels:= gdk_pixbuf_get_pixels(Pixbuf);
  Result:= TBitmap.Create;
  Result.Height:= height;
  Result.Width:= width;
  Result.PixelFormat:= pf32bit;

  BmpData:= Result.CreateIntfImage;
  try
    for i:= 0 to Width - 1 do
      for j:= 0 to Height - 1 do
      begin
        p:= pixels + j * rowstride + i * n_channels;
        PByteArray(BmpData.PixelData)^[(((J*Width)+i)*4)]:= Ord((p+2)^);  // +2
        PByteArray(BmpData.PixelData)^[(((J*Width)+i)*4)+1]:= Ord((p+1)^); // +1
        PByteArray(BmpData.PixelData)^[(((J*Width)+i)*4)+2]:= Ord(p^); // +0
        PByteArray(BmpData.PixelData)^[(((J*Width)+i)*4)+3]:= 0;
      end;
    Result.Transparent:= True;
    Result.LoadFromIntfImage(BmpData);
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

