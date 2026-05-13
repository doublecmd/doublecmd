unit uQtWSButtons;

{$mode objfpc}{$H+}

interface

uses
  QtWSButtons, Buttons, Graphics, GraphType;

type

  { TCustomBitBtnEx }

  TCustomBitBtnEx = class(TCustomBitBtn);

  { TQtWSBitBtnEx }

  TQtWSBitBtnEx = class(TQtWSBitBtn)
  published
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
  end;

implementation

uses
  Types, ImgList, Qt6, QtObjects, QtWidgets, WSProc, WSLCLClasses, LResources;

{ TQtWSBitBtnEx }

class procedure TQtWSBitBtnEx.SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph);
const
  IconModeToButtonState: array[QIconMode] of TButtonState =
  (
{ QIconNormal   } bsUp,
{ QIconDisabled } bsDisabled,
{ QIconActive   } bsHot,
{ QIconSelected } bsDown
  );

var
  AIcon: QIconH;
  APixmap: QPixmapH;
  AGlyph: TBitmap;
  AIndex: Integer;
  AEffect: TGraphicsDrawEffect;
  Mode: QIconMode;
  ASize: TSize;
  AImageRes: TScaledImageListResolution;
  AScaleFactor: Double;
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetGlyph') then
    Exit;

  TQtBitBtn(ABitBtn.Handle).GlyphLayout := Ord(ABitBtn.Layout);
  AIcon := QIcon_create();
  if ABitBtn.CanShowGlyph(True) then
  begin
    AGlyph := TBitmap.Create;
    APixmap := QPixmap_create();
    AScaleFactor:= ABitBtn.GetCanvasScaleFactor;

    for Mode := QIconNormal to QIconSelected do
    begin
      AValue.GetImageIndexAndEffect(IconModeToButtonState[Mode],
        ABitBtn.Font.PixelsPerInch, 1, AImageRes, AIndex, AEffect);
      AImageRes.GetBitmap(AIndex, AGlyph, AEffect);
      QPixmap_fromImage(APixmap, TQtImage(AGlyph.Handle).Handle);
      QIcon_addPixmap(AIcon, APixmap, Mode, QIconOn);

      if AScaleFactor <> 1 then
      begin
        AValue.GetImageIndexAndEffect(IconModeToButtonState[Mode],
          ABitBtn.Font.PixelsPerInch, AScaleFactor, AImageRes, AIndex, AEffect);
        AImageRes.GetBitmap(AIndex, AGlyph, AEffect);
        QImage_setDevicePixelRatio(TQtImage(AGlyph.Handle).Handle, AScaleFactor);
        QPixmap_fromImage(APixmap, TQtImage(AGlyph.Handle).Handle);
        QIcon_addPixmap(AIcon, APixmap, Mode, QIconOn);
      end;
    end;
    QPixmap_destroy(APixmap);
    AGlyph.Free;

    ASize.cx := AImageRes.Width;
    ASize.cy := AImageRes.Height;
    TQtBitBtn(ABitBtn.Handle).setIconSize(@ASize);
  end;

  TQtBitBtn(ABitBtn.Handle).setIcon(AIcon);
  QIcon_destroy(AIcon);
end;

procedure Initialize;
begin
  TCustomBitBtnEx.WSRegisterClass;
  RegisterWSComponent(TCustomBitBtn, TQtWSBitBtnEx);
end;

initialization
  Initialize;

end.
