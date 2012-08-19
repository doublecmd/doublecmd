uses
  gtk2def, gdk2, GTK2Proc, Cairo;


function gdk_cairo_create(drawable: PGdkDrawable): Pcairo_t cdecl external gdklib;

procedure AlphaBlend(Source, Destination: HDC; const R: TRect; const Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);

  function GetContext(GtkDC: TGtk2DeviceContext): Pcairo_t;
  begin
    Result := nil;
    if (GtkDC <> nil) and (GtkDC.Drawable <> nil) then
        Result := gdk_cairo_create(GtkDC.Drawable);
  end;

var
  SrcDC: TGtk2DeviceContext absolute Source;
  DestDC: TGtk2DeviceContext absolute Destination;
  SrcContext, DestContext: Pcairo_t;
begin
  case Mode of
    bmConstantAlpha:;
    bmPerPixelAlpha:;
    bmMasterAlpha:;
    bmConstantAlphaAndColor:
      begin
        DestContext := GetContext(DestDC);
        if DestContext <> nil then
        begin
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
