unit Interfaces;

{$mode objfpc}{$H+}

interface

uses
  InitC, InterfaceBase, LCLType, QtInt, Types, uWayland;

type

  { TQtWidgetSetEx }

  TQtWidgetSetEx = Class(TQtWidgetSet)
  private
    FWayland: TWaylandClient;
  public
    constructor Create; override;
    function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
      SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP;
      XMask, YMask: Integer; Rop: DWORD): Boolean; override;
    property Wayland: TWaylandClient read FWayland;
  end;

var
  QtWidgetSetEx: TQtWidgetSetEx;

implementation

uses
  CTypes, SysUtils, Forms, Graphics, GraphUtil, qtobjects, qt6;

function setenv(const name, value: pchar; overwrite: cint): cint; cdecl; external clib;

{ TQtWidgetSetEx }

constructor TQtWidgetSetEx.Create;
begin
  FWayland:= TWaylandClient.Create;
  setenv('QT_SCALE_FACTOR_ROUNDING_POLICY', 'PassThrough', 1);
  inherited Create;
  QtWidgetSetEx:= Self
end;

function TQtWidgetSetEx.StretchMaskBlt(DestDC: HDC; X, Y, Width,
  Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
var
  SrcQDC: TQtDeviceContext absolute SrcDC;
  DstQDC: TQtDeviceContext absolute DestDC;
  SrcRect, DstRect, MaskRect: TRect;
  Image, TmpImage, QMask, TmpMask: QImageH;
  TmpPixmap: QPixmapH;
  SrcMatrix: QTransformH;
  dx, dy: integer;
  OldRop: Integer;
  OldBkColor: TColorRef;
  RestoreClip: Boolean;
  AClipRect: TRect;
  AWindow: QWindowH;
  AScreen: QScreenH;
  AWidgetID: PtrUInt;

  function GetWidgetScaleFactor: TQtPointF;
  var
    ASize: TSize;
    AName: String;
    Index: Integer;
    WName: WideString;
    AScreen: QScreenH;
    AWindow: QWidgetH;
    WScreen: TWaylandScreen;
  begin
    begin
      AWindow:= QWidget_window(DstQDC.Parent);
      AScreen:= QWindow_screen(QWidget_windowHandle(AWindow));
      if (AScreen = nil) then AScreen:= QGuiApplication_primaryScreen();
      QScreen_size(AScreen, @ASize);
      QScreen_name(AScreen, @WName);
      AName:= UTF8Encode(WName);

      // Find corresponding Wayland screen by name
      for Index:= 0 to FWayland.Screens.Count - 1 do
      begin
        WScreen:= TWaylandScreen(FWayland.Screens[Index]);

        if SameText(AName, WScreen.Name) then
        begin
          // Calculate fractional scaling factor
          Result.x:= (WScreen.Width / ASize.Width);
          Result.y:= (WScreen.Height / ASize.Height);
          Result.x:= Round(Result.x * 100) / 100;
          Result.y:= Round(Result.y * 100) / 100;
          Exit;
        end;
      end;
    end;
    Result.x:= 1.0;
    Result.y:= 1.0;
  end;

  function NeedScaling(const ADstRect, ASrcRect: TRect): boolean;
  var
    R: TRect;
    Factor: TQtPointF;
    TgtW, TgtH, ClpW, ClpH: integer;
  begin
    Factor:= GetWidgetScaleFactor;

    if not DstQDC.getClipping then
      Exit((Round(Width * Factor.x) <> SrcWidth) or (Round(Height * Factor.y) <> SrcHeight));

    R := DstQDC.getClipRegion.getBoundingRect;

    TgtW := Round(ADstRect.Right * Factor.x) - Round(ADstRect.Left * Factor.x);
    TgtH := Round(ADstRect.Bottom * Factor.y) - Round(ADstRect.Top * Factor.y);
    ClpW := Round(R.Right * Factor.x) - Round(R.Left * Factor.x);
    ClpH := Round(R.Bottom * Factor.y) - Round(R.Top * Factor.y);

    Result := PtInRect(R, Point(R.Left + 1, R.Top + 1)) and
      (((ClpW <= TgtW) and (ClpH <= TgtH)) or ((TgtW < SrcWidth) and (TgtH < SrcHeight)));
  end;

  procedure RenderPixmap(APixmap: QPixmapH; AMaskImg: QImageH; const AHighQuality: boolean);
  var
    ATempPixmap: QPixmapH;
    ABitmap: QBitmapH;
    AMaskPix, AScaledPix: QPixmapH;
    ARenderHints: QPainterRenderHints;
    ARenderHint: Boolean;
  begin
    ATempPixmap := QPixmap_create();

    ARenderHints := QPainter_renderHints(DstQDC.Widget);
    ARenderHint := (AHighQuality and not EqualRect(DstRect, SrcRect)) and
      ((ARenderHints and QPainterAntialiasing <> 0) or (ARenderHints and QPainterSmoothPixmapTransform <> 0) or
       (ARenderHints and QPainterVerticalSubpixelPositioning <> 0));

    if ARenderHint and (ARenderHints and QPainterSmoothPixmapTransform <> 0) then
      ARenderHint := False; // do not touch render hints

    if NeedScaling(DstRect, SrcRect) then
      QPixmap_scaled(APixmap, ATempPixmap, Width, Height)
    else
      QPixmap_copy(APixmap, ATempPixmap, 0, 0, QPixmap_width(APixmap), QPixmap_height(APixmap));

    if AMaskImg <> nil then
    begin
      // apply mask to pixmap
      AScaledPix := QPixmap_create();
      AMaskPix := QPixmap_create();
      QPixmap_fromImage(AMaskPix, AMaskImg);
      try

        if IsRectEmpty(MaskRect) or (MaskRect.Size <> DstRect.Size) then
        begin
          if (QPixmap_width(ATempPixmap) < (DstRect.Right - DstRect.Left)) or
            (QPixmap_Height(ATempPixmap) < (DstRect.Bottom - DstRect.Top)) then
            QPixmap_scaled(AMaskPix, AScaledPix, QPixmap_width(ATempPixmap), QPixmap_height(ATempPixmap))
          else
            QPixmap_scaled(AMaskPix, AScaledPix, Width, Height);
        end else
          QPixmap_copy(AMaskPix, AScaledPix, MaskRect.Left, MaskRect.Top,
            MaskRect.Right - MaskRect.Left, MaskRect.Bottom - MaskRect.Top);

        ABitmap := QBitmap_Create(AScaledPix);

        QPixmap_setMask(ATempPixmap, ABitmap);
        QBitmap_Destroy(ABitmap);
        QPixmap_destroy(AScaledPix);
      finally
        QPixmap_destroy(AMaskPix);
      end;
    end;

    if ARenderHint then
      QPainter_setRenderHint(DstQDC.Widget, QPainterSmoothPixmapTransform, True);

    QPainter_drawPixmap(DstQDC.Widget, X, Y, Width, Height, ATempPixmap);

    if ARenderHint then
      QPainter_setRenderHint(DstQDC.Widget, QPainterSmoothPixmapTransform, not ARenderHint);

    QPixmap_destroy(ATempPixmap);
  end;

begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI StretchMaskBlt]',
     ' DestDC:', dbghex(DestDC),
     ' SrcDC:', dbghex(SrcDC),
     ' Image:', dbghex(PtrUInt(Image)),
     ' X:', dbgs(X), ' Y:', dbgs(Y),
     ' W:', dbgs(Width), ' H:', dbgs(Height),
     ' XSrc:', dbgs(XSrc), ' YSrc:', dbgs(YSrc),
     ' WSrc:', dbgs(SrcWidth), ' HSrc:', dbgs(SrcHeight),' Rop: ',dbgs(Rop));
  {$endif}

  Result := False;

  SrcMatrix := QPainter_transform(SrcQDC.Widget);

  QTransform_map(SrcMatrix, XSrc, YSrc, @XSrc, @YSrc);
  // our map can have some transformations
  if XSrc < 0 then // we cannot draw from negative coord, so we will draw from zero with shift
  begin
    dx := -XSrc;
    XSrc := 0;
  end
  else
    dx := 0;

  if YSrc < 0 then
  begin
    dy := -YSrc;
    YSrc := 0;
  end
  else
    dy := 0;

  if dx <> 0 then // apply shifts
  begin
    inc(X, dx);        // shift destination
    dec(Width, dx);    // substract width
    dec(SrcWidth, dx); // and do not forget about SrcWidth or we will get unneeded stretching
  end;

  if dy <> 0 then
  begin
    inc(Y, dy);
    dec(Height, dy);
    dec(SrcHeight, dy);
  end;

  DstRect := Bounds(X, Y, Width, Height);
  SrcRect := Bounds(XSrc, YSrc, SrcWidth, SrcHeight);
  MaskRect := Bounds(XMask, YMask, SrcWidth, SrcHeight);

  if Mask <> 0 then
    QMask := TQtImage(Mask).Handle
  else
    QMask := nil;

  if ((Rop = SRCCOPY) and (SrcQDC.vImage <> nil) and (SrcQDC.vImage.Handle <> nil))
    and not ((DstRect.Right < DstRect.Left) or (DstRect.Bottom < DstRect.Top)) then
  begin
    //issue #32137
    TmpImage := QImage_Create();
    QImage_copy(SrcQDC.vImage.Handle, TmpImage, XSrc, YSrc, SrcWidth, SrcHeight);

    // #11713, #25590
    if (QImage_format(TmpImage) = QImageFormat_RGB32) then
    begin
      Image := QImage_create();
      QImage_convertToFormat(TmpImage, Image, QImageFormat_ARGB32);
      QImage_destroy(TmpImage);
      TmpImage := QImage_create(Image);
      QImage_destroy(Image);
    end;

    TmpPixmap := QPixmap_create();
    QPixmap_fromImage(TmpPixmap, TmpImage);

    RenderPixmap(TmpPixmap, QMask,
      QImage_format(SrcQDC.vImage.Handle) = QImageFormat_ARGB32);
    QImage_destroy(TmpImage);
    QPixmap_destroy(TmpPixmap);
    exit(True);
  end else
  if SrcQDC.vImage = nil then
  begin
    if SrcQDC.Parent <> nil then
    begin
      AWindow := QWidget_windowHandle(SrcQDC.Parent);
      if AWindow = nil then
        exit(False);

      AWidgetID := QWindow_winId(AWindow);
      AScreen := QWindow_screen(AWindow);

      if (Rop = SRCCOPY) and not ((DstRect.Right < DstRect.Left) or (DstRect.Bottom < DstRect.Top)) then
      begin
        TmpPixmap := QPixmap_create();
        QScreen_grabWindow(AScreen, TmpPixmap, AWidgetID, XSrc,  YSrc, SrcWidth, SrcHeight);
        RenderPixmap(TmpPixmap, QMask, QPixmap_hasAlphaChannel(TmpPixmap));
        QPixmap_destroy(TmpPixmap);
        exit(True);
      end else
      begin
        with SrcQDC.getDeviceSize do
          TmpPixmap := QPixmap_create(x, y);
        QScreen_grabWindow(AScreen, TmpPixmap, AWidgetID, 0, 0);
        Image := QImage_create();
        QPixmap_toImage(TmpPixmap, Image);
        QPixmap_destroy(TmpPixmap);
      end;
    end
    else
      Exit;
  end
  else
    Image := SrcQDC.vImage.Handle;

  if (DstRect.Right < DstRect.Left) or (DstRect.Bottom < DstRect.Top) then
  begin
    // Right < Left mean horizontal flip, Bottom < Top - vertical
    TmpImage := QImage_create();
    QImage_mirrored(Image, TmpImage, DstRect.Right < DstRect.Left, DstRect.Bottom < DstRect.Top);
    if QMask <> nil then
    begin
      TmpMask := QImage_create();
      QImage_mirrored(QMask, TmpMask, DstRect.Right < DstRect.Left, DstRect.Bottom < DstRect.Top);
    end
    else
      TmpMask := QMask;
    DstRect := NormalizeRect(DstRect);
    MaskRect := NormalizeRect(MaskRect);

    OldRop := DstQDC.Rop2;
    if Rop <> SRCCOPY then
      DstQDC.Rop2 := Integer(Rop);
    try
      DstQDC.drawImage(@DstRect, TmpImage, @SrcRect, TmpMask, @MaskRect);
    finally
      if Rop <> SRCCOPY then
        DstQDC.Rop2 := OldRop;
    end;

    QImage_destroy(TmpImage);
    if TmpMask <> nil then
      QImage_destroy(TmpMask);
  end else
  begin
    if (Rop = PATCOPY) then
    begin
      OldRop := DstQDC.Rop2;
      DstQDC.Rop2 := SRCCOPY;
      try
        DstQDC.setBrush(DstQDC.BackgroundBrush);
        with DstRect do
          DstQDC.fillRect(Left, Top, Right - Left, Bottom - Top);
      finally
        DstQDC.Rop2 := OldRop;
      end;
    end else
    if (Rop = BLACKNESS) or (Rop = WHITENESS) then
    begin
      OldRop := DstQDC.Rop2;
      DstQDC.Rop2 := SRCCOPY;
      try
        if (Rop = BLACKNESS) then
          OldBkColor := DstQDC.SetBkColor(clBlack)
        else
          OldBkColor := DstQDC.SetBkColor(clWhite);
        with DstRect do
          DstQDC.fillRect(Left, Top, Right - Left, Bottom - Top);
      finally
        DstQDC.SetBkColor(OldBkColor);
        DstQDC.Rop2 := OldRop;
      end;
    end else
    begin
      OldRop := DstQDC.Rop2;
      if Rop <> SRCCOPY then
        DstQDC.Rop2 := Integer(Rop);
      try
        RestoreClip := False;
        if ((SrcRect.Left < DstRect.Left) or (SrcRect.Top < DstRect.Top)) and
          DstQDC.getClipping and DstQDC.getClipRegion.containsRect(SrcRect) then
        begin
          AClipRect := DstQDC.getClipRegion.getBoundingRect;
          Types.OffsetRect(AClipRect, -AClipRect.Left, -AClipRect.Top);

          if (DstRect.Right - DstRect.Left <= AClipRect.Right) and
            (DstRect.Bottom - DstRect.Top <= AClipRect.Bottom) and
            DstQDC.getClipRegion.containsPoint(DstRect.Left, DstRect.Top) then
          begin
            if (QPaintEngine_type(DstQDC.PaintEngine) = QPaintEngineRaster) then
            begin
              // issue #26744 - only affects raster engine.
              RestoreClip := ((DstRect.Left > 0) or (DstRect.Top > 0)) and
                (DstRect.Right - DstRect.Left - AClipRect.Right - AClipRect.Left = 0) and
                (DstRect.Bottom - DstRect.Top - AClipRect.Bottom - AClipRect.Top = 0);
            end else
              // issue #26342, unset clipping only when transform is dirty with non raster engine.
              RestoreClip := QPaintEngine_testDirty(DstQDC.PaintEngine, QPaintEngineDirtyTransform);
            if RestoreClip then
              DstQDC.setClipping(False);
          end;
        end;
        DstQDC.drawImage(@DstRect, Image, @SrcRect, QMask, @MaskRect);
        if RestoreClip then
          DstQDC.setClipping(True);
      finally
        if Rop <> SRCCOPY then
          DstQDC.Rop2 := OldRop;
      end;
    end;
  end;

  if SrcQDC.vImage = nil then
    QImage_destroy(Image);

  Result := True;
end;

function IsWayland: Boolean;
var
  ASession: String;
  APlatform: String;
begin
  ASession:= LowerCase(GetEnvironmentVariable('XDG_SESSION_TYPE'));
  APlatform:= LowerCase(GetEnvironmentVariable('QT_QPA_PLATFORM'));
  Result:= (ASession = 'wayland') and ((APlatform = '') or (APlatform = 'wayland'));
end;

initialization
  if IsWayland then
    CreateWidgetset(TQtWidgetSetEx)
  else begin
    CreateWidgetset(TQtWidgetSet);
  end;

finalization
  FreeWidgetset;

end.
