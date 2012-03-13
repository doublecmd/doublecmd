uses
  gtkdef, gtkint, CairoXlib, gdk, Cairo, glib;

//procedure gdk_drawable_get_size(drawable: PGdkDrawable; width, height: Pgint); cdecl; external gdkdll;

procedure AlphaBlend(Source, Destination: HDC; const R: TRect; const Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);

  function CreateSurface(GtkDC: TGtkDeviceContext): Pcairo_surface_t;
  var
    Width, Height: gint;
    Visual: PGdkVisual;
  begin
    Result := nil;
    if (GtkDC <> nil) and (GtkDC.Drawable <> nil) then
    begin
      gdk_window_get_size(GtkDC.Drawable, @Width, @Height);
      Visual := gdk_visual_get_system;
      Result := cairo_xlib_surface_create(
        GDK_WINDOW_XDISPLAY(PGdkWindowPrivate(GtkDC.Drawable)),
        GDK_WINDOW_XWINDOW(PGdkWindowPrivate(GtkDC.Drawable)),
        GDK_VISUAL_XVISUAL(PGdkVisualPrivate(Visual)),
        Width, Height);
    end;
  end;

var
  SrcDC: TGtkDeviceContext absolute Source;
  DestDC: TGtkDeviceContext absolute Destination;
  SrcSurface, DestSurface: Pcairo_surface_t;
  SrcContext, DestContext: Pcairo_t;
begin
  case Mode of
    bmConstantAlpha:;
    bmPerPixelAlpha:;
    bmMasterAlpha:;
    bmConstantAlphaAndColor:
      begin
        DestSurface := CreateSurface(DestDC);
        if DestSurface <> nil then
        begin
          DestContext := cairo_create(DestSurface);
          cairo_set_source_rgba(DestContext,
            (Bias and $000000FF) / 255,
            ((Bias shr 8) and $000000FF) / 255,
            ((Bias shr 16) and $000000FF) / 255,
            ConstantAlpha / 255
            );
          cairo_rectangle(DestContext, R.Left + Target.x, R.Top + Target.y,
            R.Right - R.Left, R.Bottom - R.Top);
          cairo_fill(DestContext);

          cairo_destroy(DestContext);
          cairo_surface_destroy(DestSurface);
        end;
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
