unit Img32.Draw;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.8                                                             *
* Date      :  10 January 2025                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
*                                                                              *
* Purpose   :  Polygon renderer for TImage32                                   *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Types, Math, Img32, Img32.Vector;

type
  TFillRule = Img32.Vector.TFillRule;

  // TGradientColor: used internally by both
  // TLinearGradientRenderer and TRadialGradientRenderer
  TGradientColor = record
    offset: double;
    color: TColor32;
  end;
  TArrayOfGradientColor = array of TGradientColor;

  TGradientFillStyle = (gfsClamp, gfsMirror, gfsRepeat);

  // TBoundsProc: Function template for TCustomRenderer.
  TBoundsProc = function(dist, colorsCnt: integer): integer;
  TBoundsProcD = function(dist: double; colorsCnt: integer): integer;

  TImage32ChangeProc = procedure of object;

  // TCustomRenderer: can accommodate pixels of any size
  TCustomRenderer = class {$IFDEF ABSTRACT_CLASSES} abstract {$ENDIF}
  private
    fImgWidth    : integer;
    fImgHeight   : integer;
    fImgBase     : Pointer;
    fCurrY       : integer;
    fCurrLinePtr : Pointer;
    fPixelSize   : integer;
    fChangeProc  : TImage32ChangeProc;
    fOpacity     : Byte;
  protected
    procedure NotifyChange;
    function Initialize(imgBase: Pointer;
      imgWidth, imgHeight, pixelSize: integer): Boolean; overload; virtual;
    function Initialize(targetImage: TImage32): Boolean; overload; virtual;
    function GetDstPixel(x,y: integer): Pointer;
    // RenderProc: x & y refer to pixel coords in the destination image and
    // where x1 is the start (and left) and x2 is the end of the render
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); virtual; abstract;
    // RenderProcSkip: is called for every skipped line block if
    // SupportsRenderProcSkip=True and the Rasterize() function skips scanlines.
    procedure RenderProcSkip(const skippedRect: TRect); virtual;
    // SetClipRect is called by the Rasterize() function with the
    // rasterization clipRect. The default implementation does nothing.
    procedure SetClipRect(const clipRect: TRect); virtual;
    // If SupportsRenderProcSkip returns True the Rasterize() function
    // will call RenderProcSkip() for every scanline where it didn't have
    // anything to rasterize.
    function SupportsRenderProcSkip: Boolean; virtual;
  public
    constructor Create; virtual;
    property ImgWidth: integer read fImgWidth;
    property ImgHeight: integer read fImgHeight;
    property ImgBase: Pointer read fImgBase;
    property PixelSize: integer read fPixelSize;
    property Opacity: Byte read fOpacity write fOpacity;
  end;

  TCustomColorRenderer = class(TCustomRenderer)
  private
    fColor: TColor32;
  protected
    property Color: TColor32 read fColor write fColor;
  public
    procedure SetColor(value: TColor32); virtual;
  end;

  TColorRenderer = class(TCustomColorRenderer)
  private
    fAlpha: Byte;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
    function Initialize(targetImage: TImage32): Boolean; override;
  public
    constructor Create(color: TColor32 = clNone32); reintroduce;
    procedure SetColor(value: TColor32); override;
  end;

  TAliasedColorRenderer = class(TCustomColorRenderer)
  protected
    function Initialize(targetImage: TImage32): Boolean; override;
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  public
    constructor Create(color: TColor32 = clNone32); reintroduce;
  end;

  // TMaskRenderer masks all pixels inside the clipRect area
  // where the alpha[]-array is zero.
  TMaskRenderer = class(TCustomRenderer)
  private
    fClipRect: TRect;
  protected
    procedure SetClipRect(const clipRect: TRect); override;
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
    procedure RenderProcSkip(const skippedRect: TRect); override;
    function SupportsRenderProcSkip: Boolean; override;
  end;

  // TCustomRendererCache is used to not create Renderer
  // objects for every DrawPolygon/DrawLine function call. The color
  // of the TCustomColorRenderer will be changed by the DrawPolygon/
  // DrawLine method.
  TCustomRendererCache = class(TObject)
  private
    fColorRenderer: TColorRenderer;
    fAliasedColorRenderer: TAliasedColorRenderer;
    fMaskRenderer: TMaskRenderer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetColorRenderer(color: TColor32): TColorRenderer;

    property ColorRenderer: TColorRenderer read fColorRenderer;
    property AliasedColorRenderer: TAliasedColorRenderer read fAliasedColorRenderer;
    property MaskRenderer: TMaskRenderer read fMaskRenderer;
  end;

  TEraseRenderer = class(TCustomRenderer)
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  end;

  TInverseRenderer = class(TCustomRenderer)
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  end;

  TImageRenderer = class(TCustomRenderer)
  private
    fImage        : TImage32;
    fOffset       : TPoint;
    fBrushPixel   :  PARGB;
    fLastYY       : integer;
    fMirrorY      : Boolean;
    fBoundsProc   : TBoundsProc;
    function GetFirstBrushPixel(x, y: integer): PColor32;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
    function Initialize(targetImage: TImage32): Boolean; override;
  public
    constructor Create(tileFillStyle: TTileFillStyle = tfsRepeat;
      brushImage: TImage32 = nil); reintroduce;
    destructor Destroy; override;
    procedure SetTileFillStyle(value: TTileFillStyle);
    property Image: TImage32 read fImage;
    property Offset: TPoint read fOffset write fOffset;
  end;

  // TCustomGradientRenderer is also an abstract class
  TCustomGradientRenderer = class(TCustomRenderer)
  private
    fBoundsProc      : TBoundsProc;
    fGradientColors  : TArrayOfGradientColor;
  protected
    fColors          : TArrayOfColor32;
    fColorsCnt       : integer;
    procedure SetGradientFillStyle(value: TGradientFillStyle); virtual;
  public
    constructor Create; override;
    procedure SetParameters(startColor, endColor: TColor32;
      gradFillStyle: TGradientFillStyle = gfsClamp); virtual;
    procedure InsertColorStop(offsetFrac: double; color: TColor32);
    procedure Clear;
  end;

  TLinearGradientRenderer = class(TCustomGradientRenderer)
  private
    fStartPt         : TPointD;
    fEndPt           : TPointD;
    fPerpendicOffsets: TArrayOfInteger;
    fIsVert          : Boolean;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
    function Initialize(targetImage: TImage32): Boolean; override;
  public
    procedure SetParameters(const startPt, endPt: TPointD;
      startColor, endColor: TColor32;
      gradFillStyle: TGradientFillStyle = gfsClamp); reintroduce;
  end;

  TRadialGradientRenderer = class(TCustomGradientRenderer)
  private
    fCenterPt       : TPointD;
    fScaleX         : double;
    fScaleY         : double;
    fColors         : TArrayOfColor32;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
    function Initialize(targetImage: TImage32): Boolean; override;
  public
    procedure SetParameters(const focalRect: TRect;
      innerColor, outerColor: TColor32;
      gradientFillStyle: TGradientFillStyle = gfsClamp); reintroduce;
  end;

  TSvgRadialGradientRenderer = class(TCustomGradientRenderer)
  private
    fA, fB          : double;
    fAA, fBB        : double;
    fCenterPt       : TPointD;
    fFocusPt        : TPointD;
    fBoundsProcD    : TBoundsProcD;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
    function Initialize(targetImage: TImage32): Boolean; override;
  public
    procedure SetParameters(const ellipseRect: TRect;
      const focus: TPoint; innerColor, outerColor: TColor32;
      gradientFillStyle: TGradientFillStyle = gfsClamp); reintroduce;
  end;

  // Barycentric rendering colorizes inside triangles
  TBarycentricRenderer = class(TCustomRenderer)
  private
    a: TPointD;
    c1, c2, c3: TARGB;
    v0, v1: TPointD;
    d00, d01, d11, invDenom: double;
    function GetColor(const pt: TPointD): TColor32;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  public
    procedure SetParameters(const a, b, c: TPointD; c1, c2, c3: TColor32);
  end;

  // /////////////////////////////////////////////////////////////////////////
  // DRAWING FUNCTIONS
  // /////////////////////////////////////////////////////////////////////////

  procedure DrawPoint(img: TImage32; const pt: TPointD;
    radius: double; color: TColor32); overload;
  procedure DrawPoint(img: TImage32; const pt: TPointD;
    radius: double; renderer: TCustomRenderer); overload;
  procedure DrawPoint(img: TImage32; const points: TPathD;
    radius: double; color: TColor32); overload;
  procedure DrawPoint(img: TImage32; const paths: TPathsD;
    radius: double; color: TColor32); overload;

  procedure DrawInvertedPoint(img: TImage32; const pt: TPointD; radius: double);

  procedure DrawLine(img: TImage32;
    const pt1, pt2: TPointD; lineWidth: double; color: TColor32); overload;
  procedure DrawLine(img: TImage32;
    const line: TPathD; lineWidth: double; color: TColor32;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto;
    miterLimit: double = 2); overload;
  procedure DrawLine(img: TImage32;
    const line: TPathD; lineWidth: double; color: TColor32;
    rendererCache: TCustomRendererCache;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto;
    miterLimit: double = 2); overload;
  procedure DrawLine(img: TImage32;
    const line: TPathD; lineWidth: double; renderer: TCustomRenderer;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto;
    miterLimit: double = 2); overload;
  procedure DrawLine(img: TImage32; const lines: TPathsD;
    lineWidth: double; color: TColor32;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto;
    miterLimit: double = 2); overload;
  procedure DrawLine(img: TImage32; const lines: TPathsD;
    lineWidth: double; color: TColor32; rendererCache: TCustomRendererCache;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto;
    miterLimit: double = 2); overload;
  procedure DrawLine(img: TImage32; const lines: TPathsD;
    lineWidth: double; renderer: TCustomRenderer;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto;
    miterLimit: double = 2); overload;

   procedure DrawInvertedLine(img: TImage32;
     const line: TPathD; lineWidth: double;
     endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;
   procedure DrawInvertedLine(img: TImage32;
     const lines: TPathsD; lineWidth: double;
     endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;

  procedure DrawDashedLine(img: TImage32; const line: TPathD;
    dashPattern: TArrayOfDouble; patternOffset: PDouble;
    lineWidth: double; color: TColor32;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto;
    rendererCache: TCustomRendererCache = nil); overload;
  procedure DrawDashedLine(img: TImage32; const lines: TPathsD;
    dashPattern: TArrayOfDouble; patternOffset: PDouble;
    lineWidth: double; color: TColor32; endStyle: TEndStyle;
    joinStyle: TJoinStyle = jsAuto;
    rendererCache: TCustomRendererCache = nil); overload;
  procedure DrawDashedLine(img: TImage32; const line: TPathD;
    dashPattern: TArrayOfDouble; patternOffset: PDouble;
    lineWidth: double; renderer: TCustomRenderer; endStyle: TEndStyle;
    joinStyle: TJoinStyle = jsAuto); overload;
  procedure DrawDashedLine(img: TImage32; const lines: TPathsD;
    dashPattern: TArrayOfDouble; patternOffset: PDouble;
    lineWidth: double; renderer: TCustomRenderer;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;

  procedure DrawInvertedDashedLine(img: TImage32;
    const line: TPathD; dashPattern: TArrayOfDouble;
    patternOffset: PDouble; lineWidth: double; endStyle: TEndStyle;
    joinStyle: TJoinStyle = jsAuto); overload;
  procedure DrawInvertedDashedLine(img: TImage32;
    const lines: TPathsD; dashPattern: TArrayOfDouble;
    patternOffset: PDouble; lineWidth: double;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;

  procedure DrawPolygon(img: TImage32; const polygon: TPathD;
    fillRule: TFillRule; color: TColor32); overload;
  procedure DrawPolygon(img: TImage32; const polygon: TPathD;
    fillRule: TFillRule; renderer: TCustomRenderer); overload;
  procedure DrawPolygon(img: TImage32; const polygons: TPathsD;
    fillRule: TFillRule; color: TColor32); overload;
  procedure DrawPolygon(img: TImage32; const polygons: TPathsD;
    fillRule: TFillRule; color: TColor32;
    rendererCache: TCustomRendererCache); overload;
  procedure DrawPolygon(img: TImage32; const polygons: TPathsD;
    fillRule: TFillRule; renderer: TCustomRenderer); overload;

  procedure DrawInvertedPolygon(img: TImage32; const polygon: TPathD;
    fillRule: TFillRule); overload;
  procedure DrawInvertedPolygon(img: TImage32; const polygons: TPathsD;
    fillRule: TFillRule); overload;

  // 'Clear Type' text rendering is quite useful for low resolution
  // displays (96 ppi). However it's of little to no benefit on higher
  // resolution displays and becomes unnecessary overhead. See also:
  // https://en.wikipedia.org/wiki/Subpixel_rendering
  // https://www.grc.com/ctwhat.htm
  // https://www.grc.com/cttech.htm
  procedure DrawPolygon_ClearType(img: TImage32; const polygons: TPathsD;
    fillRule: TFillRule; color: TColor32; backColor: TColor32 = clWhite32);

  // /////////////////////////////////////////////////////////////////////////
  // MISCELLANEOUS FUNCTIONS
  // /////////////////////////////////////////////////////////////////////////

  procedure ErasePolygon(img: TImage32; const polygon: TPathD;
    fillRule: TFillRule); overload;
  procedure ErasePolygon(img: TImage32; const polygons: TPathsD;
    fillRule: TFillRule); overload;

  // Both DrawBoolMask and DrawAlphaMask require
  // 'mask' length to equal 'img' width * height
  procedure DrawBoolMask(img: TImage32;
    const mask: TArrayOfByte; color: TColor32 = clBlack32);
  procedure DrawAlphaMask(img: TImage32;
    const mask: TArrayOfByte; color: TColor32 = clBlack32);

  procedure Rasterize(const paths: TPathsD;
    const clipRec: TRect; fillRule: TFillRule; renderer: TCustomRenderer); overload;
  procedure Rasterize(img: TImage32; const paths: TPathsD;
    const clipRec: TRect; fillRule: TFillRule; renderer: TCustomRenderer); overload;

implementation

{$IFDEF CPUX86}
const
  // Use faster Trunc for x86 code in this unit.
  Trunc: function(Value: Double): Integer = __Trunc;
{$ENDIF CPUX86}

type

  // A horizontal scanline contains any number of line fragments. A fragment
  // can be a number of pixels wide but it can't be more than one pixel high.
  PFragment = ^TFragment;
  TFragment = record
    botX, topX, dy, dydx: double; // ie x at bottom and top of scanline
  end;

  TScanLine = record
    Y: integer;
    minX, maxX: integer;
    fragCnt: integer;
    fragOffset: integer;
  end;
  PScanline = ^TScanline;
  TArrayOfScanline = array of TScanline;

// ------------------------------------------------------------------------------
// ApplyClearType (see DrawPolygon_ClearType below)
// ------------------------------------------------------------------------------

type
  PArgbs = ^TArgbs;
  TArgbs = array [0.. (Maxint div SizeOf(TARGB)) -1] of TARGB;

procedure ApplyClearType(img: TImage32; textColor: TColor32 = clBlack32;
  bkColor: TColor32 = clWhite32);
const
  centerWeighting = 5; //0 <= centerWeighting <= 25
var
  h, w: integer;
  src, dst: PARGB;
  srcArr: PArgbs;
  fgColor: TARGB absolute textColor;
  bgColor: TARGB absolute bkColor;
  diff_R, diff_G, diff_B: integer;
  bg8_R, bg8_G, bg8_B: integer;
  rowBuffer: TArrayOfARGB;
  primeTbl, nearTbl, FarTbl: PByteArray;
begin
  // Precondition: the background to text drawn onto 'img' must be transparent

  // 85 + (2 * 57) + (2 * 28) == 255
  primeTbl := PByteArray(@MulTable[85 + centerWeighting *2]);
  nearTbl  := PByteArray(@MulTable[57]);
  farTbl   := PByteArray(@MulTable[28 - centerWeighting]);
  SetLength(rowBuffer, img.Width +4);

  for h := 0 to img.Height -1 do
  begin
    // each row of the image is copied into a temporary buffer ...
    // noting that while 'dst' (img.Pixels) is initially the source
    // it will later be destination (during image compression).
    dst := PARGB(@img.Pixels[h * img.Width]);
    src := PARGB(@rowBuffer[2]);
    Move(dst^, src^, img.Width * SizeOf(TColor32));
    srcArr := PArgbs(rowBuffer);

    // using this buffer compress the image ...
    w := 2;
    while w < img.Width do
    begin
      dst.R := primeTbl[srcArr[w].A] +
        nearTbl[srcArr[w-1].A] + farTbl[srcArr[w-2].A] +
        nearTbl[srcArr[w+1].A] + farTbl[srcArr[w+2].A];
      inc(w);
      dst.G := primeTbl[srcArr[w].A] +
        nearTbl[srcArr[w-1].A] + farTbl[srcArr[w-2].A] +
        nearTbl[srcArr[w+1].A] + farTbl[srcArr[w+2].A];
      inc(w);
      dst.B := primeTbl[srcArr[w].A] +
        nearTbl[srcArr[w-1].A] + farTbl[srcArr[w-2].A] +
        nearTbl[srcArr[w+1].A] + farTbl[srcArr[w+2].A];
      inc(w);
      dst.A := 255;
      inc(dst);
    end;
  end;

  // Following compression the right 2/3 of the image is redundant
   img.Crop(Types.Rect(0,0, img.Width div 3, img.Height));

  // currently text is white and the background is black
  // so blend in the text and background colors ...
  diff_R := fgColor.R - bgColor.R;
  diff_G := fgColor.G - bgColor.G;
  diff_B := fgColor.B - bgColor.B;
  bg8_R := bgColor.R shl 8;
  bg8_G := bgColor.G shl 8;
  bg8_B := bgColor.B shl 8;
  dst := PARGB(img.PixelBase);
  for h := 0 to img.Width * img.Height -1 do
  begin
    if dst.R = 0 then
      dst.Color := bkColor
    else
    begin
      // blend front (text) and background colors ...
      dst.R := (bg8_R + diff_R * dst.R) shr 8;
      dst.G := (bg8_G + diff_G * dst.G) shr 8;
      dst.B := (bg8_B + diff_B * dst.B) shr 8;
    end;
    inc(dst);
  end;
end;

// ------------------------------------------------------------------------------
// Other miscellaneous functions
// ------------------------------------------------------------------------------

function ClampByte(val: double): byte; {$IFDEF INLINE} inline; {$ENDIF}
begin
  if val < 0 then result := 0
  else if val > 255 then result := 255
  else result := Round(val);
end;
// ------------------------------------------------------------------------------

function GetPixel(current: PARGB; delta: integer): PARGB;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := current;
  inc(Result, delta);
end;
// ------------------------------------------------------------------------------

// Here "const" is used for opimization reasons, to skip the
// dyn-array reference counting. "const" for dyn-arrays doesn't
// prevent one from changing the array's content.
procedure ReverseColors(const colors: TArrayOfGradientColor);
var
  highI: integer;
  dst, src: ^TGradientColor;
  // Not using a TGradientColor record for the temporary value
  // allows the 64-bit compiler to use an XMM register for it.
  tmpOffset: double;
  tmpColor: TColor32;
begin
  highI := High(colors);

  dst := @colors[0];
  src := @colors[highI];
  while PByte(dst) < PByte(src) do
  begin
    tmpColor := dst.color;
    tmpOffset := dst.offset;

    dst.color := src.color;
    dst.offset := 1 - src.offset;

    src.color := tmpColor;
    src.offset := 1 - tmpOffset;

    inc(dst);
    dec(src);
  end;
end;
// ------------------------------------------------------------------------------

procedure SwapColors(var color1, color2: TColor32);
var
  c: TColor32;
begin
  c := color1;
  color1 := color2;
  color2 := c;
end;
// ------------------------------------------------------------------------------

procedure SwapPoints(var point1, point2: TPoint); overload;
var
  pt: TPoint;
begin
  pt := point1;
  point1 := point2;
  point2 := pt;
end;
// ------------------------------------------------------------------------------

procedure SwapPoints(var point1, point2: TPointD); overload;
var
  pt: TPointD;
begin
  pt := point1;
  point1 := point2;
  point2 := pt;
end;
// ------------------------------------------------------------------------------

function ClampQ(q, endQ: integer): integer;
begin
  if q < 0 then result := 0
  else if q >= endQ then result := endQ -1
  else result := q;
end;
// ------------------------------------------------------------------------------

function ClampD(d: double; colorCnt: integer): integer;
begin
  dec(colorCnt);
  if d < 0 then result := 0
  else if d >= 1 then result := colorCnt
  else result := Round(d * colorCnt);
end;
// ------------------------------------------------------------------------------

function MirrorQ(q, endQ: integer): integer;
begin
  result := q mod endQ;
  if (result < 0) then result := -result;
  if Odd(q div endQ) then
    result := (endQ -1) - result;
end;
// ------------------------------------------------------------------------------

function MirrorD(d: double; colorCnt: integer): integer;
begin
  dec(colorCnt);
  if Odd(Trunc(d)) then
    result := Trunc((1 - frac(d)) * colorCnt) else
    result := Trunc(frac(d)  * colorCnt);
end;
// ------------------------------------------------------------------------------

function RepeatQ(q, endQ: integer): integer;
begin
  if (q < 0) or (q >= endQ) then
  begin
    endQ := Abs(endQ);
    result := q mod endQ;
    if result < 0 then inc(result, endQ);
  end
  else result := q;
end;
// ------------------------------------------------------------------------------

function SoftRptQ(q, endQ: integer): integer;
begin
  if (q < 0) then
    result := endQ + (q mod endQ) else
    result := (q mod endQ);
  if result = 0 then result := endQ div 2;
end;
// ------------------------------------------------------------------------------

function RepeatD(d: double; colorCnt: integer): integer;
begin
  dec(colorCnt);
  if (d < 0) then
    result := Trunc((1 + frac(d)) * colorCnt) else
    result := Trunc(frac(d)  * colorCnt);
end;
// ------------------------------------------------------------------------------

function BlendColorUsingMask(bgColor, fgColor: TColor32; mask: Byte): TColor32;
var
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
  res: TARGB absolute Result;
  R, invR: PByteArray;
begin
  if fg.A = 0 then
  begin
    Result := bgColor;
    res.A := MulTable[res.A, not mask];
  end
  else if bg.A = 0 then
  begin
    Result := fgColor;
    res.A := MulTable[res.A, mask];
  end
  else if (mask = 0) then
    Result := bgColor
  else if (mask = 255) then
    Result := fgColor
  else
  begin
    R    := PByteArray(@MulTable[mask]);
    InvR := PByteArray(@MulTable[not mask]);
    res.A := R[fg.A] + InvR[bg.A];
    res.R := R[fg.R] + InvR[bg.R];
    res.G := R[fg.G] + InvR[bg.G];
    res.B := R[fg.B] + InvR[bg.B];
  end;
end;
// ------------------------------------------------------------------------------

// MakeColorGradient: using the supplied array of TGradientColor,
// create an array of TColor32 of the specified length
procedure MakeColorGradient(const gradColors: TArrayOfGradientColor;
  len: integer; var result: TArrayOfColor32);
var
  i,j, lenC: integer;
  dist, offset1, offset2, step, pos, reciprocalDistTimes255: double;
  color1, color2: TColor32;
begin
  lenC := length(gradColors);
  if (len = 0) or (lenC < 2) then Exit;
  if Length(result) <> len then // we can reuse the array
    SetLength(result, len);

  color2 := gradColors[0].color;
  result[0] := color2;
  if len = 1 then Exit;

  reciprocalDistTimes255 := 0;
  step := 1/(len-1);
  pos := step;
  offset2 := 0;
  i := 1; j := 1;
  repeat
    offset1 := offset2;
    offset2 := gradColors[i].offset;
    dist := offset2 - offset1;
    color1 := color2;
    color2 := gradColors[i].color;
    if dist > 0 then
      reciprocalDistTimes255 := 255/dist; // 1/dist*255
    while (pos <= dist) and (j < len) do
    begin
      result[j] := BlendColorUsingMask(color1, color2, Round(pos * reciprocalDistTimes255));
      inc(j);
      pos := pos + step;
    end;
    pos := pos - dist;
    inc(i);
  until i = lenC;
  if j < len then result[j] := result[j-1];
end;

// ------------------------------------------------------------------------------
// Rasterize() support functions
// ------------------------------------------------------------------------------

procedure AllocateScanlines(const polygons: TPathsD;
  const scanlines: TArrayOfScanline; var fragments: PFragment; clipBottom, clipRight: integer);
var
  i,j, highI, highJ: integer;
  y1, y2: integer;
  fragOff: Cardinal;
  psl: PScanline;
begin
  // first count how often each edge intersects with each horizontal scanline
  for i := 0 to high(polygons) do
  begin
    highJ := high(polygons[i]);
    if highJ < 2 then continue;
    y1 := Trunc(polygons[i][highJ].Y);
    for j := 0 to highJ do
    begin
      y2 := Trunc(polygons[i][j].Y);
      if y1 < y2 then
      begin
        // descending (but ignore edges outside the clipping range)
        if (y2 >= 0) and (y1 <= clipBottom) then
        begin
          if (y1 > 0) then
            dec(scanlines[y1 -1].fragCnt);
          if y2 >= clipBottom then
            inc(scanlines[clipBottom].fragCnt) else
            inc(scanlines[y2].fragCnt);
        end;
      end else
      begin
        // ascending (but ignore edges outside the clipping range)
        if (y1 >= 0) and (y2 <= clipBottom) then
        begin
          if (y2 > 0) then
            dec(scanlines[y2 -1].fragCnt);
          if y1 >= clipBottom then
            inc(scanlines[clipBottom].fragCnt) else
            inc(scanlines[y1].fragCnt);
        end;
      end;
      y1 := y2;
    end;
  end;

  // convert 'count' accumulators into real counts and allocate storage
  j := 0;
  fragOff := 0;
  highI := high(scanlines);
  psl := @scanlines[highI];

  // 'fragments' is a pointer and not a dynamic array because
  // dynamic arrays are zero initialized (hence slower than GetMem).
  for i := highI downto 0 do
  begin
    inc(j, psl.fragCnt); // nb: psl.fragCnt may be < 0 here!
    if j > 0 then
    begin
      psl.fragOffset := fragOff;
      inc(fragOff, j);
    end else
      psl.fragOffset := -1;
    psl.fragCnt := 0; // reset for later
    psl.minX := clipRight;
    psl.maxX := 0;
    psl.Y := i;
    dec(psl);
  end;
  // allocate fragments as a single block of memory
  GetMem(fragments, fragOff * sizeOf(TFragment));
end;
// ------------------------------------------------------------------------------

procedure SplitEdgeIntoFragments(const pt1, pt2: TPointD;
  const scanlines: TArrayOfScanline; fragments: PFragment; const clipRec: TRect);
var
  x,y, dx,dy, absDx, dydx, dxdy: double;
  i, scanlineY, maxY, maxX: integer;
  psl: PScanLine;
  pFrag: PFragment;
  bot, top: TPointD;
begin
  dy := pt1.Y - pt2.Y;

  if dy > 0 then
  begin
    // ASCENDING EDGE (+VE WINDING DIR)
    if dy < 0.0001 then Exit;            //ignore near horizontals
    bot := pt1; top := pt2;
  end else
  begin
    // DESCENDING EDGE (-VE WINDING DIR)
    if dy > -0.0001 then Exit;           //ignore near horizontals
    bot := pt2; top := pt1;
  end;
  // exclude edges that are completely outside the top or bottom clip region
  RectWidthHeight(clipRec, maxX, maxY);
  if (top.Y >= maxY) or (bot.Y <= 0) then Exit;

  dx := pt2.X - pt1.X;
  absDx := abs(dx);

  if absDx < 0.000001 then
  begin
    // VERTICAL EDGE
    top.X := bot.X; //this circumvents v. rare rounding issues.

    // exclude vertical edges that are outside the right clip region
    // but still update maxX for each scanline the edge passes
    if bot.X > maxX then
    begin
      for i := Min(maxY, Trunc(bot.Y)) downto Max(0, Trunc(top.Y)) do
        scanlines[i].maxX := maxX;
      Exit;
    end;

    dxdy := 0;
    if dy > 0 then dydx := 1 else dydx := -1;
  end else
  begin
    dxdy := dx/dy;
    dydx := dy/absDx;
  end;

  // TRIM EDGES THAT CROSS CLIPPING BOUNDARIES (EXCEPT THE LEFT BOUNDARY)
  if bot.X >= maxX then
  begin
    if top.X >= maxX then
    begin
      for i := Min(maxY, Trunc(bot.Y)) downto Max(0, Trunc(top.Y)) do
        scanlines[i].maxX := maxX;
      Exit;
    end;
    // here the edge must be oriented bottom-right to top-left
    y := bot.Y - (bot.X - maxX) * Abs(dydx);
    for i := Min(maxY, Trunc(bot.Y)) downto Max(0, Trunc(y)) do
      scanlines[i].maxX := maxX;
    bot.Y := y;
    if bot.Y <= 0 then Exit;
    bot.X := maxX;
  end
  else if top.X > maxX then
  begin
    // here the edge must be oriented bottom-left to top-right
    y := top.Y + (top.X - maxX) * Abs(dydx);
    for i := Min(maxY, Trunc(y)) downto Max(0, Trunc(top.Y)) do
      scanlines[i].maxX := maxX;
    top.Y := y;
    if top.Y >= maxY then Exit;
    top.X := maxX;
  end;
  if bot.Y > maxY then
  begin
    bot.X := bot.X + dxdy * (bot.Y - maxY);
    if (bot.X > maxX) then Exit;        //nb: no clipping on the left
    bot.Y := maxY;
  end;
  if top.Y < 0 then
  begin
    top.X := top.X + (dxdy * top.Y);
    if (top.X > maxX) then Exit;        //nb: no clipping on the left
    top.Y := 0;
  end;

  // SPLIT THE EDGE INTO MULTIPLE SCANLINE FRAGMENTS
  scanlineY := Trunc(bot.Y);
  if bot.Y = scanlineY then dec(scanlineY);

  // at the lower-most extent of the edge 'split' the first fragment
  if scanlineY < 0 then Exit;

  psl := @scanlines[scanlineY];
  if psl.fragOffset < 0 then Exit; //a very rare event

  pFrag := fragments;
  inc(pFrag, psl.fragOffset + psl.fragCnt);
  inc(psl.fragCnt);

  pFrag.botX := bot.X;
  if scanlineY <= top.Y then
  begin
    // the whole edge is within 1 scanline
    pFrag.topX := top.X;
    pFrag.dy := bot.Y - top.Y;
    pFrag.dydx := dydx;
    Exit;
  end;

  x := bot.X + (bot.Y - scanlineY) * dxdy;
  pFrag.topX := x;
  pFrag.dy := bot.Y - scanlineY;
  pFrag.dydx := dydx;
  // 'split' subsequent fragments until the top fragment
  dec(psl);
  while psl.Y > top.Y do
  begin
    pFrag := fragments;
    inc(pFrag, psl.fragOffset + psl.fragCnt);
    inc(psl.fragCnt);
    pFrag.botX := x;
    x := x + dxdy;
    pFrag.topX := x;
    pFrag.dy := 1;
    pFrag.dydx := dydx;
    dec(psl);
  end;
  // and finally the top fragment
  pFrag := fragments;
  inc(pFrag, psl.fragOffset + psl.fragCnt);
  inc(psl.fragCnt);
  pFrag.botX := x;
  pFrag.topX := top.X;
  pFrag.dy := psl.Y + 1 - top.Y;
  pFrag.dydx := dydx;
end;
// ------------------------------------------------------------------------------

procedure InitializeScanlines(const polygons: TPathsD;
  const scanlines: TArrayOfScanline; fragments: PFragment; const clipRec: TRect);
var
  i,j, highJ: integer;
  pt1, pt2: PPointD;
begin
 for i := 0 to high(polygons) do
  begin
    highJ := high(polygons[i]);
    if highJ < 2 then continue;
    pt1 := @polygons[i][highJ];
    pt2 := @polygons[i][0];
    for j := 0 to highJ do
    begin
      SplitEdgeIntoFragments(pt1^, pt2^, scanlines, fragments, clipRec);
      pt1 := pt2;
      inc(pt2);
    end;
  end;
end;
// ------------------------------------------------------------------------------

procedure ProcessScanlineFragments(var scanline: TScanLine;
  fragments: PFragment; const buffer: TArrayOfDouble);
var
  i,j, leftXi,rightXi: integer;
  fracX, yy, q{, windDir}: double;
  left, right, dy, dydx: double;
  frag: PFragment;
  pd: PDouble;
begin
  frag := fragments;
  inc(frag, scanline.fragOffset);
  for i := 1 to scanline.fragCnt do
  begin
    left := frag.botX;
    right := frag.topX;
    dy := frag.dy;
    dydx := frag.dydx;
    inc(frag);

    // converting botX & topX to left & right simplifies code
    if {botX > topX} left > right then
    begin
      q := left;
      left := right;
      right := q;
    end;

    leftXi := Max(0, Trunc(left));
    rightXi := Max(0, Trunc(right));
    if (leftXi = rightXi) then
    begin
      // the fragment is only one pixel wide
      //if dydx < 0 then windDir := -1.0 else windDir := 1.0;
      if dydx < 0 then dy := -dy;

      if leftXi < scanline.minX then
        scanline.minX := leftXi;
      if rightXi > scanline.maxX then
        scanline.maxX := rightXi;
      pd := @buffer[leftXi];
      if (left <= 0) then
      begin
        pd^ := pd^ + dy {* windDir};
      end else
      begin
        q := (left + right) * 0.5 - leftXi;
        pd^ := pd^ + (1-q) * dy {* windDir};
        inc(pd);
        pd^ := pd^ + q * dy {* windDir};
      end;
    end else
    begin
      if leftXi < scanline.minX then
        scanline.minX := leftXi;
      if rightXi > scanline.maxX then
        scanline.maxX := rightXi;
      pd := @buffer[leftXi];
      // left pixel
      fracX := leftXi + 1 - left;
      yy := dydx * fracX;
      q := fracX * yy * 0.5;
      pd^ := pd^ + q;
      q :=  yy - q;
      inc(pd);
      // middle pixels
      for j := leftXi +1 to rightXi -1 do
      begin
        pd^ := pd^ + q + dydx * 0.5;
        q := dydx * 0.5;
        inc(pd);
      end;
      // right pixel
      fracX := right - rightXi;
      yy :=  fracX * dydx;
      pd^ := pd^ + q + (1 - fracX * 0.5) * yy;
      inc(pd);
      // overflow
      pd^ := pd^ + fracX * 0.5 * yy;
    end;
  end;
end;
// ------------------------------------------------------------------------------

{$RANGECHECKS OFF} // negative array index is used
{ CPU register optimized implementations. Every data type must be exactly the one used. }
procedure FillByteBufferEvenOdd(byteBuffer: PByte;
  windingAccum: PDouble; count: nativeint);
var
  accum: double;
  lastValue: integer;
  start: nativeint;
  buf: PByteArray;
begin
  accum := 0; //winding count accumulator
  lastValue := 0;
  // Copy byteBuffer to a local variable, so Delphi's 32bit compiler
  // can put buf into a CPU register.
  buf := PByteArray(byteBuffer);

  // Use the negative offset trick to only increment "count"
  // until it reaches zero. And by offsetting the arrays, "count"
  // also becomes the index for those.
  inc(PByte(buf), count);
  inc(windingAccum, count);
  count := -count;
  while count < 0 do
  begin
    // lastValue can be used if accum doesn't change
    if PInt64Array(windingAccum)[count] = 0 then
    begin
      start := count;
      repeat
        inc(count);
      until (count = 0) or (PInt64Array(windingAccum)[count] <> 0);
      FillChar(buf[start], count - start, Byte(lastValue));
      if count = 0 then break;
    end;

    accum := accum + PDoubleArray(windingAccum)[count];

    // EvenOdd
    lastValue := Trunc(Abs(accum) * 1275) mod 2550; // mul 5
    if lastValue > 1275 then
      lastValue := (2550 - lastValue) shr 2 else    // div 4
      lastValue := lastValue shr 2;                 // div 4
    if lastValue > 255 then lastValue := 255;

    buf[count] := Byte(lastValue);
    PDoubleArray(windingAccum)[count] := 0;
    inc(count); // walk towards zero
  end;
end;

procedure FillByteBufferNonZero(byteBuffer: PByte;
  windingAccum: PDouble; count: nativeint);
var
  accum: double;
  lastValue: integer;
  start: nativeint;
  buf: PByteArray;
begin
  accum := 0; //winding count accumulator
  lastValue := 0;
  // Copy byteBuffer to a local variable, so Delphi's 32bit compiler
  // can put buf into a CPU register.
  buf := PByteArray(byteBuffer);

  // Use the negative offset trick to only increment "count"
  // until it reaches zero. And by offsetting the arrays, "count"
  // also becomes the index for those.
  inc(PByte(buf), count);
  inc(windingAccum, count);
  count := -count;
  while count < 0 do
  begin
    // lastValue can be used if accum doesn't change
    if PInt64Array(windingAccum)[count] = 0 then
    begin
      start := count;
      repeat
        inc(count);
      until (count = 0) or (PInt64Array(windingAccum)[count] <> 0);
      FillChar(buf[start], count - start, Byte(lastValue));
      if count = 0 then break;
    end;

    accum := accum + PDoubleArray(windingAccum)[count];

    // NonZero
    lastValue := Trunc(Abs(accum) * 318);
    if lastValue > 255 then lastValue := 255;

    buf[count] := Byte(lastValue);
    PDoubleArray(windingAccum)[count] := 0;
    inc(count); // walk towards zero
  end;
end;

procedure FillByteBufferPositive(byteBuffer: PByte;
  windingAccum: PDouble; count: nativeint);
var
  accum: double;
  lastValue: integer;
  start: nativeint;
  buf: PByteArray;
begin
  accum := 0; //winding count accumulator
  lastValue := 0;
  // Copy byteBuffer to a local variable, so Delphi's 32bit compiler
  // can put buf into a CPU register.
  buf := PByteArray(byteBuffer);

  // Use the negative offset trick to only increment "count"
  // until it reaches zero. And by offsetting the arrays, "count"
  // also becomes the index for those.
  inc(PByte(buf), count);
  inc(windingAccum, count);
  count := -count;
  while count < 0 do
  begin
    // lastValue can be used if accum doesn't change
    if PInt64Array(windingAccum)[count] = 0 then
    begin
      start := count;
      repeat
        inc(count);
      until (count = 0) or (PInt64Array(windingAccum)[count] <> 0);
      FillChar(buf[start], count - start, Byte(lastValue));
      if count = 0 then break;
    end;

    accum := accum + PDoubleArray(windingAccum)[count];

    // Positive
    lastValue := 0;
    if accum > 0.002 then
    begin
      lastValue := Trunc(accum * 318);
      if lastValue > 255 then lastValue := 255;
    end;

    buf[count] := Byte(lastValue);
    PDoubleArray(windingAccum)[count] := 0;
    inc(count); // walk towards zero
  end;
end;

procedure FillByteBufferNegative(byteBuffer: PByte;
  windingAccum: PDouble; count: nativeint);
var
  accum: double;
  lastValue: integer;
  start: nativeint;
  buf: PByteArray;
begin
  accum := 0; //winding count accumulator
  lastValue := 0;
  // Copy byteBuffer to a local variable, so Delphi's 32bit compiler
  // can put buf into a CPU register.
  buf := PByteArray(byteBuffer);

  // Use the negative offset trick to only increment "count"
  // until it reaches zero. And by offsetting the arrays, "count"
  // also becomes the index for those.
  inc(PByte(buf), count);
  inc(windingAccum, count);
  count := -count;
  while count < 0 do
  begin
    // lastValue can be used if accum doesn't change
    if PInt64Array(windingAccum)[count] = 0 then
    begin
      start := count;
      repeat
        inc(count);
      until (count = 0) or (PInt64Array(windingAccum)[count] <> 0);
      FillChar(buf[start], count - start, Byte(lastValue));
      if count = 0 then break;
    end;

    accum := accum + PDoubleArray(windingAccum)[count];

    // Negative
    lastValue := 0;
    if accum < -0.002 then
    begin
      lastValue := Trunc(accum * -318);
      if lastValue > 255 then lastValue := 255;
    end;

    buf[count] := Byte(lastValue);
    PDoubleArray(windingAccum)[count] := 0;
    inc(count); // walk towards zero
  end;
end;
{$IFDEF RANGECHECKS_ENABLED}
  {$RANGECHECKS ON}
{$ENDIF}

procedure Rasterize(const paths: TPathsD; const clipRec: TRect;
  fillRule: TFillRule; renderer: TCustomRenderer);
var
  i, xli,xri, maxW, maxH: integer;
  clipRec2: TRect;
  paths2: TPathsD;
  windingAccum: TArrayOfDouble;
  byteBuffer: PByteArray;
  scanlines: TArrayOfScanline;
  fragments: PFragment;
  scanline: PScanline;
  skippedScanlines: integer;
  skipRenderer: boolean;

  // FPC generates wrong code if "count" isn't NativeInt
  FillByteBuffer: procedure(byteBuffer: PByte; windingAccum: PDouble; count: nativeint);
begin
  // See also https://nothings.org/gamedev/rasterize/
  if not assigned(renderer) then Exit;
  renderer.SetClipRect(clipRec);
  skipRenderer := renderer.SupportsRenderProcSkip;

  Types.IntersectRect(clipRec2, clipRec, GetBounds(paths));
  if IsEmptyRect(clipRec2) then
  begin
    if skipRenderer then renderer.RenderProcSkip(clipRec);
    Exit;
  end;

  if (clipRec2.Left = 0) and (clipRec2.Top = 0) then
    paths2 := paths
  else
    paths2 := TranslatePath(paths, -clipRec2.Left, -clipRec2.Top);

  // Delphi's Round() function is *much* faster than Trunc(),
  // and even a little faster than Trunc() above (except
  // when the FastMM4 memory manager is enabled.)
  fragments := nil;
  byteBuffer := nil;
  try
    RectWidthHeight(clipRec2, maxW, maxH);
    if maxW <= 0 then Exit;
    GetMem(byteBuffer, maxW); // no need for dyn. array zero initialize
    SetLength(scanlines, maxH +1);
    SetLength(windingAccum, maxW +2);
    AllocateScanlines(paths2, scanlines, fragments, maxH, maxW-1);
    InitializeScanlines(paths2, scanlines, fragments, clipRec2);

    case fillRule of
      frEvenOdd:
        FillByteBuffer := FillByteBufferEvenOdd;
      frNonZero:
        FillByteBuffer := FillByteBufferNonZero;
{$IFDEF REVERSE_ORIENTATION}
      frPositive:
{$ELSE}
      frNegative:
{$ENDIF}
        FillByteBuffer := FillByteBufferPositive;
{$IFDEF REVERSE_ORIENTATION}
      frNegative:
{$ELSE}
      frPositive:
{$ENDIF}
        FillByteBuffer := FillByteBufferNegative;
      else
        if skipRenderer then renderer.RenderProcSkip(clipRec);
        Exit;
    end;

    // Notify the renderer about the parts at the top
    // that we didn't touch.
    if skipRenderer and (clipRec2.Top > clipRec.Top) then
    begin
      renderer.RenderProcSkip(Rect(clipRec.Left, clipRec.Top,
                                   clipRec.Right, clipRec2.Top - 1));
    end;

    skippedScanlines := 0;
    scanline := @scanlines[0];
    for i := 0 to high(scanlines) do
    begin
      if scanline.fragCnt = 0 then
      begin
        inc(scanline);
        if skipRenderer then inc(skippedScanlines);
        Continue;
      end;

      // If we have skipped some scanlines, we must notify the renderer.
      if skipRenderer and (skippedScanlines > 0) then
      begin
        renderer.RenderProcSkip(Rect(clipRec.Left, clipRec2.Top + i - skippedScanlines,
                                     clipRec.Right, clipRec2.Top + i - 1));
        skippedScanlines := 0;
      end;

      // process each scanline to fill the winding count accumulation buffer
      ProcessScanlineFragments(scanline^, fragments, windingAccum);
      // it's faster to process only the modified sub-array of windingAccum
      xli := scanline.minX;
      xri := Min(maxW -1, scanline.maxX +1);

      // a 25% weighting has been added to the alpha channel to minimize any
      // background bleed-through where polygons join with a common edge.

      // FillByteBuffer overwrites every byte in byteBuffer[xli..xri] and also resets
      // windingAccum[xli..xri] to 0.
      FillByteBuffer(@byteBuffer[xli], @windingAccum[xli], xri - xli +1);

      renderer.RenderProc(clipRec2.Left + xli, clipRec2.Left + xri,
        clipRec2.Top + i, @byteBuffer[xli]);

      inc(scanline);
    end;

    // Notify the renderer about the last skipped scanlines
    if skipRenderer then
    begin
      clipRec2.Bottom := clipRec2.top + High(scanlines) - skippedScanlines;
      if clipRec2.Bottom < clipRec.Bottom then
      begin
        renderer.RenderProcSkip(Rect(clipRec.Left, clipRec2.Bottom + 1,
                                     clipRec.Right, clipRec.Bottom));
      end;
    end;
  finally
    // cleanup and deallocate memory
    FreeMem(fragments);
    FreeMem(byteBuffer);
  end;
end;
// ------------------------------------------------------------------------------

procedure Rasterize(img: TImage32; const paths: TPathsD;
  const clipRec: TRect; fillRule: TFillRule; renderer: TCustomRenderer);
begin
  if renderer.Initialize(img) then
  begin
    Rasterize(paths, clipRec, fillRule, renderer);
    renderer.NotifyChange;
  end;
end;

// ------------------------------------------------------------------------------
// TAbstractRenderer
// ------------------------------------------------------------------------------

constructor TCustomRenderer.Create;
begin
  inherited;
  fOpacity := 255;
end;
// ------------------------------------------------------------------------------

function TCustomRenderer.Initialize(imgBase: Pointer;
  imgWidth, imgHeight, pixelSize: integer): Boolean;
begin
  fImgBase := imgBase;
  fImgWidth := ImgWidth;
  fImgHeight := ImgHeight;
  fPixelSize := pixelSize;

  fCurrLinePtr := fImgBase;
  fCurrY       := 0;
  result       := true;
end;
// ------------------------------------------------------------------------------

procedure TCustomRenderer.NotifyChange;
begin
  if assigned(fChangeProc) then fChangeProc;
end;
// ------------------------------------------------------------------------------

type THackedImage32 = class(TImage32); //exposes protected Changed method.

function TCustomRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  fChangeProc := THackedImage32(targetImage).Changed;
  with targetImage do
    result := Initialize(PixelBase, Width, Height, SizeOf(TColor32));
end;
// ------------------------------------------------------------------------------

function TCustomRenderer.GetDstPixel(x, y: integer): Pointer;
begin
  if (y <> fCurrY) then
  begin
    fCurrY := y;
    fCurrLinePtr := fImgBase;
    inc(PByte(fCurrLinePtr), fCurrY * fImgWidth * fPixelSize);
  end;
  Result := fCurrLinePtr;
  inc(PByte(Result), x * fPixelSize);
end;
// ------------------------------------------------------------------------------

procedure TCustomRenderer.SetClipRect(const clipRect: TRect);
begin
  // default: do nothing
end;
// ------------------------------------------------------------------------------

procedure TCustomRenderer.RenderProcSkip(const skippedRect: TRect);
begin
  // default: do nothing
end;
// ------------------------------------------------------------------------------

function TCustomRenderer.SupportsRenderProcSkip: Boolean;
begin
  Result := False;
end;

// ------------------------------------------------------------------------------
// TCustomColorRenderer
// ------------------------------------------------------------------------------

procedure TCustomColorRenderer.SetColor(value: TColor32);
begin
  fColor := value;
end;

// ------------------------------------------------------------------------------
// TColorRenderer
// ------------------------------------------------------------------------------

constructor TColorRenderer.Create(color: TColor32 = clNone32);
begin
  inherited Create;
  if color <> clNone32 then SetColor(color);
end;
// ------------------------------------------------------------------------------

function TColorRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  // there's no point rendering if the color is fully transparent
  result := (fAlpha > 0) and inherited Initialize(targetImage);
end;
// ------------------------------------------------------------------------------

procedure TColorRenderer.SetColor(value: TColor32);
begin
  fColor := value and $FFFFFF;
  fAlpha := GetAlpha(value);
end;
// ------------------------------------------------------------------------------

{$RANGECHECKS OFF} // negative array index usage (Delphi 7-2007 have no pointer math)
type
  // Used to reduce the number of parameters to help the compiler's
  // optimizer.
  TRenderProcData = record
    dst: PColor32Array;
    alpha: PByteArray;
  end;

function RenderProcBlendToAlpha255(count: nativeint; dstColor: TColor32;
  var data: TRenderProcData): nativeint;
// CPU register optimized
var
  a: byte;
  dst: PColor32Array;
  alpha: PByteArray;
begin
  Result := count;
  dst := data.dst;
  alpha := data.alpha;

  a := alpha[Result];
  dst[Result] := dstColor;
  inc(Result);

  while (Result < 0) and (alpha[Result] = a) do
  begin
    dst[Result] := dstColor;
    inc(Result);
  end;
end;

procedure RenderProcBlendToAlpha(dst: PColor32Array; alpha: PByteArray;
  count: nativeint; color: TColor32; alphaTable: PByteArray);
var
  a: byte;
  lastDst, dstColor: TColor32;
  data: TRenderProcData;
begin
  // Use negative offset trick.
  alpha := @alpha[count];
  dst := @dst[count];
  count := -count;

  // store pointers for RenderProcBlendToAlpha255
  data.dst := dst;
  data.alpha := alpha;

  while count < 0 do
  begin
    a := alpha[count];
    if a > 1 then
    begin
      a := alphaTable[a];
      dstColor := (a shl 24) or color;

      // Special handling for alpha channel 255 (copy dstColor into dst)
      if a = 255 then
        count := RenderProcBlendToAlpha255(count, dstColor, data)
      else
      begin
        lastDst := dst[count];
        dstColor := BlendToAlpha(lastDst, dstColor);

        a := alpha[count];
        dst[count] := dstColor;
        inc(count);

        // if we have the same dst-pixel and the same alpha channel, we can
        // just copy the already calculated BlendToAlpha color.
        while (count < 0) and (a = alpha[count]) and (dst[count] = lastDst) do
        begin
          dst[count] := dstColor;
          inc(count);
        end;
      end;
    end
    else
      inc(count);
  end;
end;
{$IFDEF RANGECHECKS_ENABLED}
  {$RANGECHECKS ON}
{$ENDIF}

procedure TColorRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
begin
  // Help the compiler to get better CPU register allocation.
  // Without the hidden Self parameter the compiler optimizes
  // better.
  RenderProcBlendToAlpha(PColor32Array(GetDstPixel(x1, y)),
                         PByteArray(alpha), x2 - x1 + 1, fColor,
                         PByteArray(@MulTable[fAlpha]));
end;

// ------------------------------------------------------------------------------
// TAliasedColorRenderer
// ------------------------------------------------------------------------------

constructor TAliasedColorRenderer.Create(color: TColor32 = clNone32);
begin
  inherited Create;
  fColor := color;
end;
// ------------------------------------------------------------------------------

function TAliasedColorRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  // there's no point rendering if the color is fully transparent
  result := (GetAlpha(fColor) > 0) and
    inherited Initialize(targetImage);
end;
// ------------------------------------------------------------------------------

procedure TAliasedColorRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  dst: PColor32;
  c: TColor32;
begin
  dst := GetDstPixel(x1,y);
  c := fColor; // copy fColor to local variable
  for i := x1 to x2 do
  begin
    if Ord(alpha^) > 127 then dst^ := c; //ie no blending
    inc(dst); inc(alpha);
  end;
end;

// ------------------------------------------------------------------------------
// TMaskRenderer
// ------------------------------------------------------------------------------

procedure TMaskRenderer.SetClipRect(const clipRect: TRect);
begin
  fClipRect := clipRect;
  // clipping to the image size
  if fClipRect.Left < 0 then fClipRect.Left := 0;
  if fClipRect.Top < 0 then fClipRect.Top := 0;
  if fClipRect.Right > fImgWidth then fClipRect.Right := fImgWidth;
  if fClipRect.Bottom > fImgHeight then fClipRect.Bottom := fImgHeight;
end;
// ------------------------------------------------------------------------------

procedure TMaskRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  p: PColor32;
  i: integer;
begin
  // CopyBlend excludes ClipRect.Right/Bottom, so we also
  // need to exclude it.
  if (y < fClipRect.Top) or (y >= fClipRect.Bottom) then Exit;
  if x2 >= fClipRect.Right then x2 := fClipRect.Right - 1;

  if x1 < fClipRect.Left then
  begin
    inc(alpha, fClipRect.Left - x1);
    x1 := fClipRect.Left;
  end;

  p := GetDstPixel(fClipRect.Left, y);

  // Clear the area before x1 (inside OutsideBounds)
  FillChar(p^, (x1 - fClipRect.Left) * SizeOf(TColor32), 0);
  inc(p, x1 - fClipRect.Left);

  // Fill the area between x1 and x2
  for i := x1 to x2 do
  begin
    if p^ <> 0 then
    begin
      if Ord(alpha^) = 0 then
        p^ := 0
      else if Ord(alpha^) <> 255 then
        p^ := BlendMask(p^, Ord(alpha^) shl 24);
    end;
    inc(p);
    inc(alpha);
  end;

  // Clear the area after x2 (inside OutsideBounds)
  FillChar(p^, (fClipRect.Right - (x2 + 1)) * SizeOf(TColor32), 0);
end;
// ------------------------------------------------------------------------------

procedure TMaskRenderer.RenderProcSkip(const skippedRect: TRect);
var
  i, h, w: integer;
  p: PColor32;
  r: TRect;
begin
  r := skippedRect;
  if r.Left < fClipRect.Left then r.Left := fClipRect.Left;
  if r.Top < fClipRect.Top then r.Top := fClipRect.Top;
  // CopyBlend excludes ClipRect.Right/Bottom, so we also
  // need to exclude it.
  if r.Right >= fClipRect.Right then r.Right := fClipRect.Right - 1;
  if r.Bottom >= fClipRect.Bottom then r.Bottom := fClipRect.Bottom - 1;

  if r.Right < r.Left then Exit;
  if r.Bottom < r.Top then Exit;

  w := r.Right - r.Left + 1;
  h := r.Bottom - r.Top + 1;
  p := GetDstPixel(r.Left, r.Top);
  if w = fImgWidth then
    FillChar(p^, w * h * SizeOf(TColor32), 0)
  else
  begin
    for i := 1 to h do
    begin
      FillChar(p^, w * SizeOf(TColor32), 0);
      inc(p, fImgWidth);
    end;
  end;
end;

// ------------------------------------------------------------------------------
function TMaskRenderer.SupportsRenderProcSkip: Boolean;
begin
  Result := True;
end;

// ------------------------------------------------------------------------------
// TCustomRendererCache
// ------------------------------------------------------------------------------

constructor TCustomRendererCache.Create;
begin
  inherited Create;
  fColorRenderer := TColorRenderer.Create;
  fAliasedColorRenderer := TAliasedColorRenderer.Create;
  fMaskRenderer := TMaskRenderer.Create;
end;
// ------------------------------------------------------------------------------

destructor TCustomRendererCache.Destroy;
begin
  fColorRenderer.Free;
  fAliasedColorRenderer.Free;
  fMaskRenderer.Free;
end;
// ------------------------------------------------------------------------------

function TCustomRendererCache.GetColorRenderer(color: TColor32): TColorRenderer;
begin
  Result := fColorRenderer;
  Result.SetColor(color);
end;

// ------------------------------------------------------------------------------
// TBrushImageRenderer
// ------------------------------------------------------------------------------

constructor TImageRenderer.Create(tileFillStyle: TTileFillStyle;
  brushImage: TImage32);
begin
  inherited Create;
  fImage := TImage32.Create(brushImage);
  SetTileFillStyle(tileFillStyle);
end;
// ------------------------------------------------------------------------------

destructor TImageRenderer.Destroy;
begin
  fImage.Free;
  inherited;
end;
// ------------------------------------------------------------------------------

procedure TImageRenderer.SetTileFillStyle(value: TTileFillStyle);
begin
  case value of
    tfsRepeat: fBoundsProc := RepeatQ;
    tfsMirrorHorz: fBoundsProc := MirrorQ;
    tfsMirrorVert: fBoundsProc := RepeatQ;
    tfsRotate180 : fBoundsProc := MirrorQ;
  end;
  fMirrorY := value in [tfsMirrorVert, tfsRotate180];
end;
// ------------------------------------------------------------------------------

function TImageRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  result := inherited Initialize(targetImage) and (not fImage.IsEmpty);
  if not result then Exit;
  fLastYY := 0;
  fBrushPixel := PARGB(fImage.PixelBase);
end;
// ------------------------------------------------------------------------------

procedure TImageRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  pDst: PColor32;
  pImg: PColor32;
  opacityTable: PByteArray;
begin
  pDst := GetDstPixel(x1,y);
  dec(x1, fOffset.X);
  dec(x2, fOffset.X);
  dec(y, fOffset.Y);
  pImg := GetFirstBrushPixel(x1, y);
  if Opacity < 255 then
  begin
    opacityTable := PByteArray(@MulTable[Opacity]);
    for i := x1 to x2 do
    begin
      pDst^ := BlendToAlpha3(pDst^, pImg^, opacityTable[Ord(alpha^)]);
      inc(pDst); inc(alpha);
      pImg := PColor32(GetPixel(fBrushPixel, fBoundsProc(i, fImage.Width)));
    end;
  end else
    for i := x1 to x2 do
    begin
      pDst^ := BlendToAlpha3(pDst^, pImg^, Ord(alpha^));
      inc(pDst); inc(alpha);
      pImg := PColor32(GetPixel(fBrushPixel, fBoundsProc(i, fImage.Width)));
    end;
end;
// ------------------------------------------------------------------------------

function TImageRenderer.GetFirstBrushPixel(x, y: integer): PColor32;
begin
  if fMirrorY then
    y := MirrorQ(y, fImage.Height) else
    y := RepeatQ(y, fImage.Height);
  if y <> fLastYY then
  begin
    fBrushPixel := PARGB(fImage.PixelRow[y]);
    fLastYY := y;
  end;
  x := fBoundsProc(x, fImage.Width);
  result := PColor32(GetPixel(fBrushPixel, x));
end;

// ------------------------------------------------------------------------------
// TGradientRenderer
// ------------------------------------------------------------------------------

constructor TCustomGradientRenderer.Create;
begin
  inherited Create;
  fBoundsProc := ClampQ; //default proc
end;
// ------------------------------------------------------------------------------

procedure TCustomGradientRenderer.Clear;
begin
  fGradientColors := nil;
  fColors := nil;
end;
// ------------------------------------------------------------------------------

procedure TCustomGradientRenderer.SetGradientFillStyle(value: TGradientFillStyle);
begin
  case value of
    gfsClamp: fBoundsProc := ClampQ;
    gfsMirror: fBoundsProc := MirrorQ;
    else fBoundsProc := RepeatQ;
  end;
end;
// ------------------------------------------------------------------------------

procedure TCustomGradientRenderer.SetParameters(startColor, endColor: TColor32;
  gradFillStyle: TGradientFillStyle = gfsClamp);
begin
  SetGradientFillStyle(gradFillStyle);
  // reset gradient colors if perviously set
  SetLength(fGradientColors, 2);
  fGradientColors[0].offset := 0;
  fGradientColors[0].color := startColor;
  fGradientColors[1].offset := 1;
  fGradientColors[1].color := endColor;
end;
// ------------------------------------------------------------------------------

procedure TCustomGradientRenderer.InsertColorStop(offsetFrac: double; color: TColor32);
var
  i, len: integer;
  gradColor: TGradientColor;
begin
  len := Length(fGradientColors);
  // colorstops can only be inserted after calling SetParameters
  if len = 0 then Exit;

  if offsetFrac < 0 then offsetFrac := 0
  else if offsetFrac > 1 then offsetFrac := 1;

  if offsetFrac = 0 then
  begin
    fGradientColors[0].color := color;
    Exit;
  end
  else if offsetFrac = 1 then
  begin
    fGradientColors[len -1].color := color;
    Exit;
  end;
  gradColor.offset := offsetFrac;
  gradColor.color  := color;

  i := 1;
  while (i < len-1) and
    (fGradientColors[i].offset <= offsetFrac) do inc(i);
  SetLength(fGradientColors, len +1);
  Move(fGradientColors[i],
    fGradientColors[i+1], (len -i) * SizeOf(TGradientColor));
  fGradientColors[i] := gradColor;
end;

// ------------------------------------------------------------------------------
// TLinearGradientRenderer
// ------------------------------------------------------------------------------

procedure TLinearGradientRenderer.SetParameters(const startPt, endPt: TPointD;
  startColor, endColor: TColor32; gradFillStyle: TGradientFillStyle);
begin
  inherited SetParameters(startColor, endColor, gradFillStyle);
  fStartPt := startPt;
  fEndPt := endPt;
end;
// ------------------------------------------------------------------------------

function TLinearGradientRenderer.Initialize(targetImage: TImage32): Boolean;
var
  i: integer;
  dx,dy, dxdy,dydx: double;
begin
  result := inherited Initialize(targetImage) and assigned(fGradientColors);
  if not result then Exit;

  if abs(fEndPt.Y - fStartPt.Y) > abs(fEndPt.X - fStartPt.X) then
  begin
    // gradient > 45 degrees
    if (fEndPt.Y < fStartPt.Y) then
    begin
      ReverseColors(fGradientColors);
      SwapPoints(fStartPt, fEndPt);
    end;
    fIsVert := true;
    dx := (fEndPt.X - fStartPt.X);
    dy := (fEndPt.Y - fStartPt.Y);
    dxdy := dx/dy;

    fColorsCnt := Ceil(dy + dxdy * (fEndPt.X - fStartPt.X));
    MakeColorGradient(fGradientColors, fColorsCnt, fColors);
    // get a list of perpendicular offsets for each
    NewIntegerArray(fPerpendicOffsets, ImgWidth, True);
    // from an imaginary line that's through fStartPt and perpendicular to
    // the gradient line, get a list of Y offsets for each X in image width
    for i := 0 to ImgWidth -1 do
      fPerpendicOffsets[i] := Round(dxdy * (fStartPt.X - i) + fStartPt.Y);
  end
  else //gradient <= 45 degrees
  begin
    if (fEndPt.X = fStartPt.X) then
    begin
      Result := false;
      Exit;
    end;
    if (fEndPt.X < fStartPt.X) then
    begin
      ReverseColors(fGradientColors);
      SwapPoints(fStartPt, fEndPt);
    end;
    fIsVert := false;
    dx := (fEndPt.X - fStartPt.X);
    dy := (fEndPt.Y - fStartPt.Y);
    dydx := dy/dx; //perpendicular slope

    fColorsCnt := Ceil(dx + dydx * (fEndPt.Y - fStartPt.Y));
    MakeColorGradient(fGradientColors, fColorsCnt, fColors);
    NewIntegerArray(fPerpendicOffsets, ImgHeight, True);
    // from an imaginary line that's through fStartPt and perpendicular to
    // the gradient line, get a list of X offsets for each Y in image height
    for i := 0 to ImgHeight -1 do
      fPerpendicOffsets[i] := Round(dydx * (fStartPt.Y - i) + fStartPt.X);
  end;
end;
// ------------------------------------------------------------------------------

procedure TLinearGradientRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i, colorsCnt: integer;
  pDst: PColor32;
  color: TColor32;
  boundsProc: TBoundsProc;
  offset: Integer;
  colors: PColor32Array;
  perpendicOffsets: PIntegerArray;
  opacityTable: PByteArray;
begin
  pDst := GetDstPixel(x1,y);
  // optimize self fields access
  colorsCnt := fColorsCnt;
  colors := @fColors[0];
  boundsProc := fBoundsProc;
  if fIsVert then
  begin
    perpendicOffsets := @fPerpendicOffsets[0]; // optimize self field access
    if Opacity < 255 then
    begin
      opacityTable := PByteArray(@MulTable[Opacity]);
      for i := x1 to x2 do
      begin
        // when fIsVert = true, fPerpendicOffsets is an array of Y for each X
        color := colors[boundsProc(y - perpendicOffsets[i], colorsCnt)];
        pDst^ := BlendToAlpha3(pDst^, color, opacityTable[Ord(alpha^)]);
        inc(pDst); inc(alpha);
      end;
    end else
    begin
      for i := x1 to x2 do
      begin
        // when fIsVert = true, fPerpendicOffsets is an array of Y for each X
        color := colors[boundsProc(y - perpendicOffsets[i], colorsCnt)];
        pDst^ := BlendToAlpha3(pDst^, color, Ord(alpha^));
        inc(pDst); inc(alpha);
      end;
    end;
  end
  else
  begin
    // when fIsVert = false, fPerpendicOffsets is an array of X for each Y
    offset := fPerpendicOffsets[y];
    if Opacity < 255 then
    begin
      opacityTable := PByteArray(@MulTable[Opacity]);
      for i := x1 to x2 do
      begin
        color := colors[boundsProc(i - offset, colorsCnt)];
        pDst^ := BlendToAlpha3(pDst^, color, opacityTable[Ord(alpha^)]);
        inc(pDst); inc(alpha);
      end;
    end else
    begin
      for i := x1 to x2 do
      begin
        color := colors[boundsProc(i - offset, colorsCnt)];
        pDst^ := BlendToAlpha3(pDst^, color, Ord(alpha^));
        inc(pDst); inc(alpha);
      end;
    end;
  end;
end;

// ------------------------------------------------------------------------------
// TRadialGradientRenderer
// ------------------------------------------------------------------------------

function TRadialGradientRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  result := inherited Initialize(targetImage) and (fColorsCnt > 1);
  if result then
    MakeColorGradient(fGradientColors, fColorsCnt, fColors);
end;
// ------------------------------------------------------------------------------

procedure TRadialGradientRenderer.SetParameters(const focalRect: TRect;
  innerColor, outerColor: TColor32;
  gradientFillStyle: TGradientFillStyle);
var
  w,h: integer;
  radX,radY: double;
begin
  inherited SetParameters(innerColor, outerColor, gradientFillStyle);
  fColorsCnt := 0;
  if IsEmptyRect(focalRect) then Exit;

  fCenterPt.X  := (focalRect.Left + focalRect.Right) * 0.5;
  fCenterPt.Y  := (focalRect.Top + focalRect.Bottom) * 0.5;
  RectWidthHeight(focalRect, w, h);
  radX    :=  w * 0.5;
  radY    :=  h * 0.5;
  if radX >= radY then
  begin
    fScaleX     := 1;
    fScaleY     := radX/radY;
    fColorsCnt := Ceil(radX) +1;
  end else
  begin
    fScaleX     := radY/radX;
    fScaleY     := 1;
    fColorsCnt := Ceil(radY) +1;
  end;
end;
// ------------------------------------------------------------------------------

procedure TRadialGradientRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  dist: double;
  color: TColor32;
  pDst: PColor32;
  opacityTable: PByteArray;
begin
  pDst := GetDstPixel(x1,y);
  if Opacity < 255 then
  begin
    opacityTable := PByteArray(@MulTable[Opacity]);
    for i := x1 to x2 do
    begin
      dist := Hypot((y - fCenterPt.Y) *fScaleY, (i - fCenterPt.X) *fScaleX);
      color := fColors[fBoundsProc(Trunc(dist), fColorsCnt)];
      pDst^ := BlendToAlpha3(pDst^, color, opacityTable[Ord(alpha^)]);
      inc(pDst); inc(alpha);
    end;
  end else
  begin
    for i := x1 to x2 do
    begin
      dist := Hypot((y - fCenterPt.Y) *fScaleY, (i - fCenterPt.X) *fScaleX);
      color := fColors[fBoundsProc(Trunc(dist), fColorsCnt)];
      pDst^ := BlendToAlpha3(pDst^, color, Ord(alpha^));
      inc(pDst); inc(alpha);
    end;
  end;
end;

// ------------------------------------------------------------------------------
// TSvgRadialGradientRenderer
// ------------------------------------------------------------------------------

function TSvgRadialGradientRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  result := inherited Initialize(targetImage) and (fColorsCnt > 1);
  if result then
    MakeColorGradient(fGradientColors, fColorsCnt, fColors);
end;
// ------------------------------------------------------------------------------

procedure TSvgRadialGradientRenderer.SetParameters(const ellipseRect: TRect;
  const focus: TPoint; innerColor, outerColor: TColor32;
  gradientFillStyle: TGradientFillStyle = gfsClamp);
var
  w, h  : integer;
begin
  inherited SetParameters(innerColor, outerColor);
  case gradientFillStyle of
    gfsMirror: fBoundsProcD := MirrorD;
    gfsRepeat: fBoundsProcD := RepeatD;
    else fBoundsProcD := ClampD;
  end;

  fColorsCnt := 0;
  if IsEmptyRect(ellipseRect) then Exit;

  fCenterPt  := RectD(ellipseRect).MidPoint;
  RectWidthHeight(ellipseRect, w, h);
  fA    := w * 0.5;
  fB    := h * 0.5;

  fFocusPt.X := focus.X - fCenterPt.X;
  fFocusPt.Y := focus.Y - fCenterPt.Y;
  fColorsCnt := Ceil(Hypot(fA*2, fB*2)) +1;
  fAA := fA * fA;
  fBB := fB * fB;
end;
// ------------------------------------------------------------------------------

procedure TSvgRadialGradientRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  q,qq, m,c, qa,qb,qc,qs: double;
  dist, dist2: double;
  color: TColor32;
  pDst: PColor32;
  pt, ellipsePt: TPointD;
  opacityTable: PByteArray;
begin
  opacityTable := PByteArray(@MulTable[Opacity]);
  // get the left-most pixel to render
  pDst := GetDstPixel(x1,y);
  pt.X := x1 - fCenterPt.X; pt.Y := y - fCenterPt.Y;
  for i := x1 to x2 do
  begin
    // equation of ellipse = (x*x)/aa + (y*y)/bb = 1
    // equation of line = y = mx + c;
    if (pt.X = fFocusPt.X) then //vertical line
    begin
      // let x = pt.X, then y*y = b*b(1 - Sqr(pt.X)/aa)
      qq := (1 - Sqr(pt.X)/fAA);
      if (qq > 1) then qq := 1
      else if (qq < 0) then qq := 0;
      q := Sqrt(fBB*qq);
      ellipsePt.X := pt.X;
      if pt.Y >= fFocusPt.Y then
        ellipsePt.Y := q else
        ellipsePt.Y := -q;
      dist := abs(pt.Y - fFocusPt.Y);
      dist2 := abs(ellipsePt.Y - fFocusPt.Y);
      if dist2 = 0 then
        q := 1 else
        q := dist/ dist2;
    end else
    begin
      // using simultaneous equations and substitution
      // given y = mx + c
      m := (pt.Y - fFocusPt.Y)/(pt.X - fFocusPt.X);
      c := pt.Y - m * pt.X;
      // given (x*x)/aa + (y*y)/bb = 1
      // (x*x)/aa*bb + (y*y) = bb
      // bb/aa *(x*x) + Sqr(m*x +c) = bb
      // bb/aa *(x*x) + (m*m)*(x*x) + 2*m*x*c +c*c = b*b
      // (bb/aa +(m*m)) *(x*x) + 2*m*c*(x) + (c*c) - bb = 0
      // solving quadratic equation
      qa := (fBB/fAA +(m*m));
      qb := 2*m*c;
      qc := (c*c) - fBB;
      qs := (qb*qb) - 4*qa*qc;
      if qs >= 0 then
      begin
        qs := Sqrt(qs);
        if pt.X <= fFocusPt.X then
          ellipsePt.X := (-qb -qs)/(2 * qa) else
          ellipsePt.X := (-qb +qs)/(2 * qa);
        ellipsePt.Y := m * ellipsePt.X + c;

        // Use sqr'ed distances (Sqrt(a^2+b^2)/Sqrt(x^2+y^2) => Sqrt((a^2+b^2)/(x^2+y^2))
        dist := Sqr(pt.X - fFocusPt.X) + Sqr(pt.Y - fFocusPt.Y);
        dist2 := Sqr(ellipsePt.X - fFocusPt.X) + Sqr(ellipsePt.Y - fFocusPt.Y);
        if dist2 = 0 then
          q := 1 else
          q := Sqrt(dist/dist2);
      end else
        q := 1; //shouldn't happen :)
    end;
    color := fColors[fBoundsProcD(Abs(q), fColorsCnt)];
    pDst^ := BlendToAlpha3(pDst^, color, opacityTable[Ord(alpha^)]);
    inc(pDst); pt.X := pt.X + 1; inc(alpha);
  end;
end;

// ------------------------------------------------------------------------------
// TEraseRenderer
// ------------------------------------------------------------------------------

procedure TEraseRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  dst: PARGB;
begin
  dst := PARGB(GetDstPixel(x1,y));
  for i := x1 to x2 do
  begin
    {$IFDEF PBYTE}
    dst.A := MulTable[dst.A, not alpha^];
    {$ELSE}
    dst.A := MulTable[dst.A, not Ord(alpha^)];
    {$ENDIF}
    inc(dst); inc(alpha);
  end;
end;

// ------------------------------------------------------------------------------
// TInverseRenderer
// ------------------------------------------------------------------------------

procedure TInverseRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  dst: PARGB;
  c: TARGB;
begin
  dst := PARGB(GetDstPixel(x1,y));
  for i := x1 to x2 do
  begin
    c.Color := not dst.Color;
    c.A := MulTable[dst.A, Ord(alpha^)];
    dst.Color := BlendToAlpha(dst.Color, c.Color);
    inc(dst); inc(alpha);
  end;
end;

// ------------------------------------------------------------------------------

procedure TBarycentricRenderer.SetParameters(const a, b, c: TPointD;
  c1, c2, c3: TColor32);
begin

  self.a := a;
  self.c1.Color := c1;
  self.c2.Color := c2;
  self.c3.Color := c3;

  v0.X := b.X - a.X;
  v0.Y := b.Y - a.Y;
  v1.X := c.X - a.X;
  v1.Y := c.Y - a.Y;
  d00 := (v0.X * v0.X + v0.Y * v0.Y);
  d01 := (v0.X * v1.X + v0.Y * v1.Y);
  d11 := (v1.X * v1.X + v1.Y * v1.Y);
  invDenom := 1/(d00 * d11 - d01 * d01);
end;
// ------------------------------------------------------------------------------

function TBarycentricRenderer.GetColor(const pt: TPointD): TColor32;
var
  v2: TPointD;
  d20, d21, v, w, u: Double;
  res: TARGB absolute Result;
begin
  Result := 0;
  v2.X := pt.X - a.X;
  v2.Y := pt.Y - a.Y;
  d20 := (v2.X * v0.X + v2.Y * v0.Y);
  d21 := (v2.X * v1.X + v2.Y * v1.Y);

  v := (d11 * d20 - d01 * d21) * invDenom;
  w := (d00 * d21 - d01 * d20) * invDenom;
  u := 1.0 - v - w;

  Res.A := ClampByte(c1.A * u + c2.A * v + c3.A * w);
  Res.R := ClampByte(c1.R * u + c2.R * v + c3.R * w);
  Res.G := ClampByte(c1.G * u + c2.G * v + c3.G * w);
  Res.B := ClampByte(c1.B * u + c2.B * v + c3.B * w);
end;
// ------------------------------------------------------------------------------

procedure TBarycentricRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  x: integer;
  p: PARGB;
  c: TARGB;
  opacityTable: PByteArray;
begin
  p := PARGB(fImgBase);
  inc(p, y * ImgWidth + x1);
  if Opacity < 255 then
  begin
    opacityTable := PByteArray(@MulTable[Opacity]);
    for x := x1 to x2 do
    begin
      c.Color := GetColor(PointD(x, y));
      c.A := opacityTable[MulTable[c.A, Ord(alpha^)]];
      p.Color := BlendToAlpha(p.Color, c.Color);
      inc(p); inc(alpha);
    end
  end
  else
    for x := x1 to x2 do
    begin
      c.Color := GetColor(PointD(x, y));
      c.A := MulTable[c.A, Ord(alpha^)];
      p.Color := BlendToAlpha(p.Color, c.Color);
      inc(p); inc(alpha);
    end


end;

// ------------------------------------------------------------------------------
// Draw functions
// ------------------------------------------------------------------------------

procedure DrawPoint(img: TImage32;
  const pt: TPointD; radius: double; color: TColor32);
var
  path: TPathD;
begin
  if radius <= 1 then
    path := Rectangle(pt.X-radius, pt.Y-radius, pt.X+radius, pt.Y+radius) else
    path := Ellipse(RectD(pt.X-radius, pt.Y-radius, pt.X+radius, pt.Y+radius));
  DrawPolygon(img, path, frEvenOdd, color);
end;
// ------------------------------------------------------------------------------

procedure DrawPoint(img: TImage32; const pt: TPointD;
  radius: double; renderer: TCustomRenderer);
var
  path: TPathD;
begin
  path := Ellipse(RectD(pt.X -radius, pt.Y -radius, pt.X +radius, pt.Y +radius));
  DrawPolygon(img, path, frEvenOdd, renderer);
end;
// ------------------------------------------------------------------------------

procedure DrawInvertedPoint(img: TImage32; const pt: TPointD; radius: double);
var
  cr: TCustomRenderer;
begin
  cr := TInverseRenderer.Create;
  try
    DrawPoint(img, pt, radius, cr);
  finally
    cr.Free;
  end;
end;
// ------------------------------------------------------------------------------

procedure DrawPoint(img: TImage32; const points: TPathD;
  radius: double; color: TColor32);
var
  i: integer;
begin
  for i := 0 to high(points) do
    DrawPoint(img, points[i], radius, color);
end;
// ------------------------------------------------------------------------------

procedure DrawPoint(img: TImage32; const paths: TPathsD;
  radius: double; color: TColor32);
var
  i: integer;
begin
  for i := 0 to high(paths) do
    DrawPoint(img, paths[i], radius, color);
end;
// ------------------------------------------------------------------------------

procedure DrawLine(img: TImage32;
  const pt1, pt2: TPointD; lineWidth: double; color: TColor32);
var
  lines: TPathsD;
begin
  setLength(lines, 1);
  NewPointDArray(lines[0], 2, True);
  lines[0][0] := pt1;
  lines[0][1] := pt2;
  DrawLine(img, lines, lineWidth, color, esRound);
end;
// ------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const line: TPathD; lineWidth: double;
  color: TColor32; endStyle: TEndStyle; joinStyle: TJoinStyle;
  miterLimit: double);
var
  lines: TPathsD;
begin
  setLength(lines, 1);
  lines[0] := line;
  DrawLine(img, lines, lineWidth, color, endStyle, joinStyle, miterLimit);
end;
// ------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const line: TPathD; lineWidth: double;
  color: TColor32; rendererCache: TCustomRendererCache;
  endStyle: TEndStyle; joinStyle: TJoinStyle; miterLimit: double);
var
  lines: TPathsD;
begin
  setLength(lines, 1);
  lines[0] := line;
  DrawLine(img, lines, lineWidth, color, rendererCache, endStyle, joinStyle,
    miterLimit);
end;
// ------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const line: TPathD; lineWidth: double;
  renderer: TCustomRenderer; endStyle: TEndStyle; joinStyle: TJoinStyle;
  miterLimit: double);
var
  lines: TPathsD;
begin
  setLength(lines, 1);
  lines[0] := line;
  DrawLine(img, lines, lineWidth, renderer, endStyle, joinStyle, miterLimit);
end;
// ------------------------------------------------------------------------------

procedure DrawInvertedLine(img: TImage32; const line: TPathD;
lineWidth: double; endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto);
var
  lines: TPathsD;
begin
  setLength(lines, 1);
  lines[0] := line;
  DrawInvertedLine(img, lines, lineWidth, endStyle, joinStyle);
end;
// ------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const lines: TPathsD;
  lineWidth: double; color: TColor32;
  endStyle: TEndStyle; joinStyle: TJoinStyle; miterLimit: double);
var
  cr: TCustomColorRenderer;
begin
  if not assigned(lines) then exit;

  if img.AntiAliased then
    cr := TColorRenderer.Create(color) else
    cr := TAliasedColorRenderer.Create(color);
  try
    DrawLine(img, lines, lineWidth, cr, endStyle, joinStyle, miterLimit);
  finally
    cr.free;
  end;
end;
// ------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const lines: TPathsD;
  lineWidth: double; color: TColor32; rendererCache: TCustomRendererCache;
  endStyle: TEndStyle; joinStyle: TJoinStyle; miterLimit: double);
var
  cr: TCustomColorRenderer;
begin
  if not assigned(lines) then exit;
  if rendererCache = nil then
    DrawLine(img, lines, lineWidth, color, endStyle, joinStyle, miterLimit)
  else
  begin
    if img.AntiAliased then
      cr := rendererCache.ColorRenderer else
      cr := rendererCache.AliasedColorRenderer;
    DrawLine(img, lines, lineWidth, cr, endStyle, joinStyle, miterLimit);
  end;
end;
// ------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const lines: TPathsD;
  lineWidth: double; renderer: TCustomRenderer;
  endStyle: TEndStyle; joinStyle: TJoinStyle;
  miterLimit: double);
var
  lines2: TPathsD;
begin
  if (not assigned(lines)) or (not assigned(renderer)) then exit;
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  lines2 := RoughOutline(lines, lineWidth, joinStyle, endStyle, miterLimit);
  Rasterize(img, lines2, img.bounds, frNonZero, renderer);
end;
// ------------------------------------------------------------------------------

procedure DrawInvertedLine(img: TImage32;
  const lines: TPathsD; lineWidth: double;
  endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto);
var
  lines2: TPathsD;
  ir: TInverseRenderer;
begin
  if not assigned(lines) then exit;
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  lines2 := RoughOutline(lines, lineWidth, joinStyle, endStyle, 2);
  ir := TInverseRenderer.Create;
  try
    Rasterize(img, lines2, img.bounds, frNonZero, ir);
  finally
    ir.free;
  end;
end;
// ------------------------------------------------------------------------------

procedure DrawDashedLine(img: TImage32; const line: TPathD;
  dashPattern: TArrayOfDouble; patternOffset: PDouble; lineWidth: double;
  color: TColor32; endStyle: TEndStyle; joinStyle: TJoinStyle;
  rendererCache: TCustomRendererCache);
var
  lines: TPathsD;
  cr: TColorRenderer;
  i: integer;
begin
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  if not assigned(line) then exit;

  for i := 0 to High(dashPattern) do
    if dashPattern[i] <= 0 then dashPattern[i] := 1;

  lines := GetDashedPath(line, endStyle = esPolygon, dashPattern, patternOffset);
  if Length(lines) = 0 then Exit;

  case joinStyle of
    jsAuto:
      if endStyle = esRound then
        joinStyle := jsRound else
        joinStyle := jsSquare;
    jsSquare, jsMiter:
      endStyle := esSquare;
    jsRound:
      endStyle := esRound;
    jsButt:
      endStyle := esButt;
  end;
  lines := RoughOutline(lines, lineWidth, joinStyle, endStyle);

  if rendererCache = nil then
    cr := TColorRenderer.Create(color) else
    cr := rendererCache.GetColorRenderer(color);
  try
    Rasterize(img, lines, img.bounds, frNonZero, cr);
  finally
    if rendererCache = nil then
      cr.free;
  end;
end;
// ------------------------------------------------------------------------------

procedure DrawDashedLine(img: TImage32; const lines: TPathsD;
  dashPattern: TArrayOfDouble; patternOffset: PDouble; lineWidth: double;
  color: TColor32; endStyle: TEndStyle; joinStyle: TJoinStyle;
  rendererCache: TCustomRendererCache);
var
  i: integer;
begin
  if not assigned(lines) then exit;
  for i := 0 to high(lines) do
    DrawDashedLine(img, lines[i],
      dashPattern, patternOffset, lineWidth, color, endStyle, joinStyle,
      rendererCache);
end;
// ------------------------------------------------------------------------------

procedure DrawDashedLine(img: TImage32; const line: TPathD;
  dashPattern: TArrayOfDouble; patternOffset: PDouble; lineWidth: double;
  renderer: TCustomRenderer; endStyle: TEndStyle; joinStyle: TJoinStyle);
var
  i: integer;
  lines: TPathsD;
begin
  if (not assigned(line)) or (not assigned(renderer)) then exit;
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;

  for i := 0 to High(dashPattern) do
    if dashPattern[i] <= 0 then dashPattern[i] := 1;

  lines := GetDashedPath(line, endStyle = esPolygon, dashPattern, patternOffset);
  if Length(lines) = 0 then Exit;
  lines := RoughOutline(lines, lineWidth, joinStyle, endStyle);
  Rasterize(img, lines, img.bounds, frNonZero, renderer);
end;
// ------------------------------------------------------------------------------

procedure DrawDashedLine(img: TImage32; const lines: TPathsD;
  dashPattern: TArrayOfDouble; patternOffset: PDouble; lineWidth: double;
  renderer: TCustomRenderer; endStyle: TEndStyle; joinStyle: TJoinStyle);
var
  i: integer;
begin
  if not assigned(lines) then exit;
  for i := 0 to high(lines) do
    DrawDashedLine(img, lines[i],
      dashPattern, patternOffset, lineWidth, renderer, endStyle, joinStyle);
end;
// ------------------------------------------------------------------------------

procedure DrawInvertedDashedLine(img: TImage32;
  const line: TPathD; dashPattern: TArrayOfDouble;
  patternOffset: PDouble; lineWidth: double; endStyle: TEndStyle;
  joinStyle: TJoinStyle = jsAuto);
var
  i: integer;
  lines: TPathsD;
  renderer: TInverseRenderer;
begin
  if not assigned(line) then exit;
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;

  for i := 0 to High(dashPattern) do
    if dashPattern[i] <= 0 then dashPattern[i] := 1;

  lines := GetDashedPath(line, endStyle = esPolygon, dashPattern, patternOffset);
  if Length(lines) = 0 then Exit;
  lines := RoughOutline(lines, lineWidth, joinStyle, endStyle);
  renderer := TInverseRenderer.Create;
  try
    Rasterize(img, lines, img.bounds, frNonZero, renderer);
  finally
    renderer.Free;
  end;
end;
// ------------------------------------------------------------------------------

procedure DrawInvertedDashedLine(img: TImage32;
  const lines: TPathsD; dashPattern: TArrayOfDouble;
  patternOffset: PDouble; lineWidth: double;
  endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto);
var
  i: integer;
begin
  if not assigned(lines) then exit;
  for i := 0 to high(lines) do
    DrawInvertedDashedLine(img, lines[i],
      dashPattern, patternOffset, lineWidth, endStyle, joinStyle);
end;
// ------------------------------------------------------------------------------

procedure DrawPolygon(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; color: TColor32);
var
  polygons: TPathsD;
begin
  if not assigned(polygon) then exit;
  setLength(polygons, 1);
  polygons[0] := polygon;
  DrawPolygon(img, polygons, fillRule, color);
end;
// ------------------------------------------------------------------------------

procedure DrawPolygon(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; renderer: TCustomRenderer);
var
  polygons: TPathsD;
begin
  if (not assigned(polygon)) or (not assigned(renderer)) then exit;
  setLength(polygons, 1);
  polygons[0] := polygon;
  Rasterize(img, polygons, img.Bounds, fillRule, renderer);
end;
// ------------------------------------------------------------------------------

procedure DrawPolygon(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; color: TColor32);
var
  cr: TCustomRenderer;
begin
  if not assigned(polygons) then exit;
  if img.AntiAliased then
    cr := TColorRenderer.Create(color) else
    cr := TAliasedColorRenderer.Create(color);
  try
    Rasterize(img, polygons, img.bounds, fillRule, cr);
  finally
    cr.free;
  end;
end;
// ------------------------------------------------------------------------------

procedure DrawPolygon(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; color: TColor32;
  rendererCache: TCustomRendererCache);
var
  cr: TCustomColorRenderer;
begin
  if not assigned(polygons) then exit;
  if rendererCache = nil then
    DrawPolygon(img, polygons, fillRule, color)
  else
  begin
    if img.AntiAliased then
      cr := rendererCache.ColorRenderer else
      cr := rendererCache.AliasedColorRenderer;
    cr.SetColor(color);
    Rasterize(img, polygons, img.bounds, fillRule, cr);
  end;
end;
// ------------------------------------------------------------------------------

procedure DrawPolygon(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; renderer: TCustomRenderer);
begin
  if (not assigned(polygons)) or (not assigned(renderer)) then exit;
  Rasterize(img, polygons, img.bounds, fillRule, renderer);
end;
// ------------------------------------------------------------------------------

procedure DrawInvertedPolygon(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule);
var
  polygons: TPathsD;
begin
  if not assigned(polygon) then exit;
  setLength(polygons, 1);
  polygons[0] := polygon;
  DrawInvertedPolygon(img, polygons, fillRule);
end;
// ------------------------------------------------------------------------------

procedure DrawInvertedPolygon(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule);
var
  cr: TCustomRenderer;
begin
  if not assigned(polygons) then exit;
  cr := TInverseRenderer.Create;
  try
    Rasterize(img, polygons, img.bounds, fillRule, cr);
  finally
    cr.free;
  end;
end;
// ------------------------------------------------------------------------------

procedure DrawPolygon_ClearType(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; color: TColor32; backColor: TColor32);
var
  w, h: integer;
  tmpImg: TImage32;
  rec: TRect;
  tmpPolygons: TPathsD;
  cr: TColorRenderer;
begin
  if not assigned(polygons) then exit;

  rec := GetBounds(polygons);
  RectWidthHeight(rec, w, h);
  tmpImg := TImage32.Create(w *3, h);
  try
    tmpPolygons := TranslatePath(polygons, -rec.Left, -rec.Top);
    tmpPolygons := ScalePath(tmpPolygons, 3, 1);
    cr := TColorRenderer.Create(clBlack32);
    try
      Rasterize(tmpImg, tmpPolygons, tmpImg.bounds, fillRule, cr);
    finally
      cr.Free;
    end;
    ApplyClearType(tmpImg, color, backColor);
    img.CopyBlend(tmpImg, tmpImg.Bounds, rec, BlendToAlphaLine);
  finally
    tmpImg.Free;
  end;
end;
// ------------------------------------------------------------------------------

procedure ErasePolygon(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule);
var
  polygons: TPathsD;
begin
  if not assigned(polygon) then exit;
  setLength(polygons, 1);
  polygons[0] := polygon;
  ErasePolygon(img, polygons, fillRule);
end;
// ------------------------------------------------------------------------------

procedure ErasePolygon(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule);
var
  er: TEraseRenderer;
begin
  er := TEraseRenderer.Create;
  try
    Rasterize(img, polygons, img.bounds, fillRule, er);
  finally
    er.Free;
  end;
end;
// ------------------------------------------------------------------------------

procedure DrawBoolMask(img: TImage32; const mask: TArrayOfByte; color: TColor32);
var
  i, len: integer;
  pc: PColor32;
  pb: PByte;
begin
  len := Length(mask);
  if (len = 0) or (len <> img.Width * img.Height) then Exit;
  pc := img.PixelBase;
  pb := @mask[0];
  for i := 0 to len -1 do
  begin
    {$IFDEF PBYTE}
    if pb^ > 0 then
    {$ELSE}
    if pb^ > #0 then
    {$ENDIF}
      pc^ := color else
      pc^ := clNone32;
    inc(pc); inc(pb);
  end;
end;
// ------------------------------------------------------------------------------

procedure DrawAlphaMask(img: TImage32; const mask: TArrayOfByte; color: TColor32);
var
  i, len: integer;
  pc: PColor32;
  pb: PByte;
begin
  len := Length(mask);
  if (len = 0) or (len <> img.Width * img.Height) then Exit;
  color := color and $FFFFFF; //strip alpha value
  pc := img.PixelBase;
  pb := @mask[0];
  for i := 0 to len -1 do
  begin
    {$IFDEF PBYTE}
    if pb^ > 0 then
      pc^ := color or pb^ shl 24 else
      pc^ := clNone32;
    {$ELSE}
    if pb^ > #0 then
      pc^ := color or Ord(pb^) shl 24 else
      pc^ := clNone32;
    {$ENDIF}
    inc(pc); inc(pb);
  end;
end;
// ------------------------------------------------------------------------------

end.
