unit Img32.Extra;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.8                                                             *
* Date      :  10 January 2025                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  Miscellaneous routines that don't belong in other modules.      *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface
{$I Img32.inc}

uses
  SysUtils, Classes, Math, Types,
  Img32, Img32.Draw, Img32.Vector;

type
  TButtonShape = (bsRound, bsSquare, bsDiamond);
  TButtonAttribute = (baShadow, ba3D, baEraseBeneath);
  TButtonAttributes = set of TButtonAttribute;

procedure DrawEdge(img: TImage32; const rec: TRect;
  topLeftColor, bottomRightColor: TColor32; penWidth: double = 1.0); overload;
procedure DrawEdge(img: TImage32; const rec: TRectD;
  topLeftColor, bottomRightColor: TColor32; penWidth: double = 1.0); overload;
procedure DrawEdge(img: TImage32; const path: TPathD;
  topLeftColor, bottomRightColor: TColor32;
  penWidth: double = 1.0; closePath: Boolean = true); overload;

//DrawShadowRect: is **much** faster than DrawShadow
procedure DrawShadowRect(img: TImage32; const rec: TRect; depth: double;
  angle: double = angle45; color: TColor32 = $80000000);
procedure DrawShadow(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; depth: double; angleRads: double = angle45;
  color: TColor32 = $80000000; cutoutInsideShadow: Boolean = false); overload;
procedure DrawShadow(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; depth: double; angleRads: double = angle45;
  color: TColor32 = $80000000; cutoutInsideShadow: Boolean = false); overload;

procedure DrawGlow(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; color: TColor32; blurRadius: integer); overload;
procedure DrawGlow(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; color: TColor32; blurRadius: integer); overload;

//FloodFill: If no CompareFunc is provided, FloodFill will fill whereever
//adjoining pixels exactly match the starting pixel - Point(x,y).
procedure FloodFill(img: TImage32; x, y: Integer; newColor: TColor32;
  tolerance: Byte = 0; compareFunc: TCompareFunctionEx = nil);

procedure FastGaussianBlur(img: TImage32;
  const rec: TRect; stdDev: integer; repeats: integer = 2); overload;
procedure FastGaussianBlur(img: TImage32;
  const rec: TRect; stdDevX, stdDevY: integer; repeats: integer); overload;

procedure GaussianBlur(img: TImage32; rec: TRect; radius: Integer);

//Emboss: A smaller radius is sharper. Increasing depth increases contrast.
//Luminance changes grayscale balance (unless preserveColor = true)
procedure Emboss(img: TImage32; radius: Integer = 1; depth: Integer = 10;
  luminance: Integer = 75; preserveColor: Boolean = false);

//Sharpen: Radius range is 1 - 10; amount range is 1 - 50.<br>
//see https://en.wikipedia.org/wiki/Unsharp_masking
procedure Sharpen(img: TImage32; radius: Integer = 2; amount: Integer = 10);

//HatchBackground: Assumes the current image is semi-transparent.
procedure HatchBackground(img: TImage32; color1: TColor32 = clWhite32;
  color2: TColor32= $FFE8E8E8; hatchSize: Integer = 10); overload;
procedure HatchBackground(img: TImage32; const rec: TRect;
  color1: TColor32 = clWhite32; color2: TColor32= $FFE8E8E8;
  hatchSize: Integer = 10); overload;

procedure GridBackground(img: TImage32; majorInterval, minorInterval: integer;
  fillColor: TColor32 = clWhite32;
  majColor: TColor32 = $30000000; minColor: TColor32 = $20000000);

procedure ReplaceExactColor(img: TImage32; oldColor, newColor: TColor32);

//RemoveColor: Removes the specified color from the image, even from
//pixels that are a blend of colors including the specified color.<br>
//see https://stackoverflow.com/questions/9280902/
procedure RemoveColor(img: TImage32; color: TColor32);

//FilterOnColor: Removes everything not nearly matching 'color'
//This uses an algorithm that's very similar to the one in RemoveColor.
procedure FilterOnColor(img: TImage32; color: TColor32);

procedure FilterOnExactColor(img: TImage32; color: TColor32);

procedure FilterOnAlpha(img: TImage32; alpha: byte; tolerance: byte);

//RedEyeRemove: Removes 'red eye' from flash photo images.
procedure RedEyeRemove(img: TImage32; const rect: TRect);

procedure PencilEffect(img: TImage32; intensity: integer = 0);

procedure TraceContours(img: TImage32; intensity: integer);

procedure EraseInsidePath(img: TImage32;
  const path: TPathD; fillRule: TFillRule);
procedure EraseInsidePaths(img: TImage32;
  const paths: TPathsD; fillRule: TFillRule);

procedure EraseOutsidePath(img: TImage32; const path: TPathD;
  fillRule: TFillRule; const outsideBounds: TRect);
procedure EraseOutsidePaths(img: TImage32; const paths: TPathsD;
  fillRule: TFillRule; const outsideBounds: TRect;
  rendererCache: TCustomRendererCache = nil); overload;

procedure Draw3D(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; height, blurRadius: double;
  colorLt: TColor32 = $DDFFFFFF; colorDk: TColor32 = $80000000;
  angleRads: double = angle225); overload;
procedure Draw3D(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; height, blurRadius: double;
  colorLt: TColor32 = $DDFFFFFF; colorDk: TColor32 = $80000000;
  angleRads: double = angle225); overload;

function RainbowColor(fraction: double; luminance: byte = 128): TColor32;
function GradientColor(color1, color2: TColor32; frac: single): TColor32;
function MakeDarker(color: TColor32; percent: cardinal): TColor32;
function MakeLighter(color: TColor32; percent: cardinal): TColor32;

function DrawButton(img: TImage32; const pt: TPointD;
  size: double; color: TColor32 = clNone32;
  buttonShape: TButtonShape = bsRound;
  buttonAttributes: TButtonAttributes = [baShadow, ba3D, baEraseBeneath]): TPathD;

// RamerDouglasPeucker: simplifies paths, recursively removing vertices where
// they deviate no more than 'epsilon' from their adjacent vertices.
function RamerDouglasPeucker(const path: TPathD;
  epsilon: double): TPathD; overload;
function RamerDouglasPeucker(const paths: TPathsD;
  epsilon: double): TPathsD; overload;

{$IFDEF USE_OLD_SIMPLIFYPATHS}
// SimplifyPath: Better than RDP when simplifying closed paths
function SimplifyPath(const path: TPathD;
  shapeTolerance: double = 0.1; isOpenPath: Boolean = false): TPathD;
function SimplifyPaths(const paths: TPathsD;
  shapeTolerance: double = 0.1; isOpenPath: Boolean = false): TPathsD;
{$ELSE}
// SimplifyPath: Better than RDP when simplifying closed paths
function SimplifyPath(const path: TPathD;
  shapeTolerance: double = 0.1; isClosedPath: Boolean = true): TPathD;
function SimplifyPaths(const paths: TPathsD;
  shapeTolerance: double = 0.1; isClosedPath: Boolean = true): TPathsD;
{$ENDIF}

// SimplifyPathEx: this is particularly useful following Vectorize()
// because it also removes very short zig-zag segments
function SimplifyPathEx(const path: TPathD; shapeTolerance: double): TPathD;
function SimplifyPathsEx(const paths: TPathsD; shapeTolerance: double): TPathsD;

// SmoothToCubicBezier and SmoothToCubicBezier2 have been deprecated in
// favour of SmoothPath that's much simpler
function SmoothToCubicBezier(const path: TPathD;
  pathIsClosed: Boolean; maxOffset: integer = 0): TPathD; overload; deprecated;
function SmoothToCubicBezier(const paths: TPathsD;
  pathIsClosed: Boolean; maxOffset: integer = 0): TPathsD; overload; deprecated;
function SmoothToCubicBezier2(const path: TPathD;
  pathIsClosed: Boolean; maxOffset: integer = 0): TPathD; overload; deprecated;
function SmoothToCubicBezier2(const paths: TPathsD;
  pathIsClosed: Boolean; maxOffset: integer = 0): TPathsD; overload; deprecated;

// SmoothPath - smooths a path using bicubic interpolation
//   tension (range -1 to 1): from least to most curve constraint
function SmoothPath(const path: TPathD; isClosedPath: Boolean;
  tension: double = 0; shapeTolerance: double = 0.1): TPathD;
function SmoothPaths(const paths: TPathsD; isClosedPath: Boolean;
  tension: double = 0; shapeTolerance: double = 0.1): TPathsD;

function GetFloodFillMask(imgIn, imgMaskOut: TImage32; x, y: Integer;
  tolerance: Byte; compareFunc: TCompareFunctionEx): Boolean;

procedure SymmetricCropTransparent(img: TImage32);

//3 additional blend functions (see TImage32.CopyBlend)
function BlendAverage(bgColor, fgColor: TColor32): TColor32;
function BlendLinearBurn(bgColor, fgColor: TColor32): TColor32;
function BlendColorDodge(bgColor, fgColor: TColor32): TColor32;

implementation

uses
  {$IFDEF USING_FMX}
  Img32.FMX,
  {$ENDIF}
  Img32.Transform;

const
  FloodFillDefaultRGBTolerance: byte = 64;
  MaxBlur = 100;

type
  PColor32Array = ^TColor32Array;
  TColor32Array = array [0.. maxint div SizeOf(TColor32) -1] of TColor32;
  PWeightedColorArray = ^TWeightedColorArray;
  TWeightedColorArray = array [0.. $FFFFFF] of TWeightedColor;

  // SimplifyPathsEx structures
  PVertex = ^TVertex;
  TVertex = record
    pt    : TPointD;
    uvec  : TPointD;
    dist  : double;
    perpD : double;
    next  : PVertex;
    prev:  PVertex;
  end;
  TArrayOfVertices = array of TVertex;


//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function Clamp(val, endVal: integer): integer;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  if val < 0 then Result := 0
  else if val >= endVal then Result := endVal -1
  else Result := val;
end;
//------------------------------------------------------------------------------

function ModEx(val, endVal: integer): integer;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := val mod endVal;
  if Result < 0 then Result := endVal + Result;
end;
//------------------------------------------------------------------------------

function GetSymmetricCropTransparentRect(img: TImage32): TRect;
var
  w,h, x,y, x1,y1: Integer;
  p1,p2: PARGB;
  opaquePxlFound: Boolean;
begin
  Result := img.Bounds;
  w := img.Width;
  y1 := 0;
  opaquePxlFound := false;
  for y := 0 to (img.Height div 2) -1 do
  begin
    p1 := PARGB(img.PixelRow[y]);
    p2 := PARGB(img.PixelRow[img.Height - y -1]);
    for x := 0 to w -1 do
    begin
      if (p1.A > 0) or (p2.A > 0) then
      begin
        y1 := y;
        opaquePxlFound := true;
        break;
      end;
      inc(p1); inc(p2);
    end;
    if opaquePxlFound then break;
  end;
  // probably safeset not to resize empty images
  if not opaquePxlFound then Exit;
  if y1 > 0 then
  begin
    inc(Result.Top, y1);
    dec(Result.Bottom, y1);
  end;
  x1 := 0;
  h := RectHeight(Result);
  opaquePxlFound := false;
  for x := 0 to (w div 2) -1 do
  begin
    p1 := PARGB(@img.Pixels[Result.Top * w + x]);
    p2 := PARGB(@img.Pixels[Result.Top * w + (w -1) - x]);
    for y := 0 to h -1 do
    begin
      if (p1.A > 0) or (p2.A > 0) then
      begin
        x1 := x;
        opaquePxlFound := true;
        break;
      end;
      inc(p1, w); inc(p2, w);
    end;
    if opaquePxlFound then break;
  end;
  if not opaquePxlFound then Exit;
  inc(Result.Left, x1);
  dec(Result.Right, x1);
end;
//------------------------------------------------------------------------------

//SymmetricCropTransparent: after cropping, the image's midpoint
//will be the same pixel as before cropping. (Important for rotating.)
procedure SymmetricCropTransparent(img: TImage32);
var
  rec: TRect;
begin
  rec := GetSymmetricCropTransparentRect(img);
  if (rec.Top > 0) or (rec.Left > 0) then img.Crop(rec);
end;
//------------------------------------------------------------------------------

procedure DrawEdge(img: TImage32; const rec: TRect;
  topLeftColor, bottomRightColor: TColor32; penWidth: double = 1.0);
begin
  DrawEdge(img, RectD(rec), topLeftColor, bottomRightColor, penWidth);
end;
//------------------------------------------------------------------------------

procedure DrawEdge(img: TImage32; const rec: TRectD;
  topLeftColor, bottomRightColor: TColor32; penWidth: double = 1.0);
var
  p: TPathD;
  c: TColor32;
begin
  if penWidth = 0 then Exit
  else if penWidth < 0 then
  begin
    c := topLeftColor;
    topLeftColor := bottomRightColor;
    bottomRightColor := c;
    penWidth := -penWidth;
  end;
  if topLeftColor <> bottomRightColor then
  begin
    with rec do
    begin
      p := Img32.Vector.MakePath([left, bottom, left, top, right, top]);
      DrawLine(img, p, penWidth, topLeftColor, esButt);
      p := Img32.Vector.MakePath([right, top, right, bottom, left, bottom]);
      DrawLine(img, p, penWidth, bottomRightColor, esButt);
    end;
  end else
    DrawLine(img, Rectangle(rec), penWidth, topLeftColor, esPolygon);
end;
//------------------------------------------------------------------------------

procedure DrawEdge(img: TImage32; const path: TPathD;
  topLeftColor, bottomRightColor: TColor32;
  penWidth: double = 1.0; closePath: Boolean = true);
var
  i, highI, deg: integer;
  frac: double;
  c: TColor32;
  p: TPathD;
const
  RadToDeg = 180/PI;
begin
  if penWidth = 0 then Exit
  else if penWidth < 0 then
  begin
    c := topLeftColor;
    topLeftColor := bottomRightColor;
    bottomRightColor := c;
    penWidth := -penWidth;
  end;
  highI := high(path);
  if highI < 2 then Exit;
  p := path;
  if closePath and not PointsNearEqual(p[0], p[highI], 0.01) then
  begin
    AppendPoint(p, p[0]);
    inc(highI);
  end;
  for i := 1 to highI do
  begin
    deg := Round(GetAngle(p[i-1], p[i]) * RadToDeg);
    case deg of
      -180..-136: frac := (-deg-135)/45;
      -135..0   : frac := 0;
      1..44     : frac := deg/45;
      else        frac := 1;
    end;
    c := GradientColor(topLeftColor, bottomRightColor, frac);
    DrawLine(img, p[i-1], p[i], penWidth, c);
  end;
end;
//------------------------------------------------------------------------------

procedure FillColorHorz(img: TImage32; x, endX, y: integer; color: TColor32);
var
  i,dx: integer;
  p: PColor32;
begin
  if (x < 0) or (x >= img.Width) then Exit;
  if (y < 0) or (y >= img.Height) then Exit;
  p := img.PixelRow[y]; inc(p, x);
  if endX >= img.Width then endX := img.Width -1
  else if endX < 0 then endX := 0;
  if endX < x then dx := -1 else dx := 1;
  for i := 0 to Abs(x-endX) do
  begin
    p^ := color;
    inc(p, dx);
  end;
end;
//------------------------------------------------------------------------------

procedure FillColorVert(img: TImage32; x, y, endY: integer; color: TColor32);
var
  i, dy: integer;
  p: PColor32;
begin
  if (x < 0) or (x >= img.Width) then Exit;
  if (y < 0) or (y >= img.Height) then Exit;
  p := img.PixelRow[y]; inc(p, x);
  if endY >= img.Height then
    endY := img.Height -1 else if endY < 0 then endY := 0;
  dy := img.Width;
  if endY < y then dy := -dy;
  for i := 0 to Abs(y - endY) do
  begin
    p^ := color;
    inc(p, dy);
  end;
end;
//------------------------------------------------------------------------------

procedure DrawShadowRect(img: TImage32; const rec: TRect; depth: double;
  angle: double = angle45; color: TColor32 = $80000000);
var
  i,j, sX,sY: integer;
  l,t,r,b: integer;
  tmpImg: TImage32;
  tmpRec: TRect;
  xx,yy: double;
  ss: TPointD;
  c: TColor32;
begin
  GetSinCos(angle, yy, xx);
  ss.X := depth * xx;
  ss.Y := depth * yy;
  sX := Abs(Round(ss.X));
  sY := Abs(Round(ss.Y));
  if rec.Left + ss.X < 0 then ss.X := -rec.Left
  else if rec.Right + ss.X > img.Width then ss.X := img.Width - rec.Right -1;
  if rec.Top + ss.Y < 0 then ss.Y := -rec.Top
  else if rec.Bottom + ss.Y > img.Height then ss.Y := img.Height -rec.Bottom -1;
  tmpImg  := TImage32.Create(sX*3 +1, sY*3 +1);
  try
    i := sX div 2; j := sY div 2;
    DrawPolygon(tmpImg, Rectangle(i,j,i+sX*2,j+sY*2), frNonZero, color);
    FastGaussianBlur(tmpImg, tmpImg.Bounds, Round(sX/4),Round(sY/4), 1);
    // t-l corner
    if (ss.X < 0) or (ss.Y < 0) then
    begin
      tmpRec := Rect(0, 0, sX, sY);
      l := rec.Left; t := rec.Top;
      if ss.X < 0 then dec(l, sX);
      if ss.Y < 0 then dec(t, sY);
      img.Copy(tmpImg, tmpRec, Rect(l,t,l+sX,t+sY));
    end;
    // t-r corner
    if (ss.X > 0) or (ss.Y < 0) then
    begin
      tmpRec := Rect(sX*2+1, 0, sX*3+1, sY);
      l := rec.Right; t := rec.Top;
      if ss.X < 0 then dec(l, sX);
      if ss.Y < 0 then dec(t, sY);
      img.Copy(tmpImg, tmpRec, Rect(l,t,l+sX,t+sY));
    end;
    // b-l corner
    if (ss.X < 0) or (ss.Y > 0) then
    begin
      tmpRec := Rect(0, sY*2+1, sX, sY*3+1);
      l := rec.Left; t := rec.Bottom;
      if ss.X < 0 then dec(l, sX);
      if ss.Y < 0 then dec(t, sY);
      img.Copy(tmpImg, tmpRec, Rect(l,t,l+sX,t+sY));
    end;
    // b-r corner
    if (ss.X > 0) or (ss.Y > 0) then
    begin
      tmpRec := Rect(sX*2+1, sY*2+1, sX*3+1, sY*3+1);
      l := rec.Right; t := rec.Bottom;
      if ss.X < 0 then dec(l, sX);
      if ss.Y < 0 then dec(t, sY);
      img.Copy(tmpImg, tmpRec, Rect(l,t,l+sX,t+sY));
    end;
    // l-edge
    if (ss.X < 0) then
    begin
      l := rec.Left; t := rec.Top+sY; b := rec.Bottom-1;
      if ss.Y < 0 then begin dec(t, sY); dec(b,sY); end;
      for i := 1 to sX do
      begin
        c := tmpImg.Pixel[sX-i, sY+1];
        FillColorVert(img, l-i, t, b, c);
      end;
    end;
    // t-edge
    if (ss.Y < 0) then
    begin
      l := rec.Left+sX; r := rec.Right-1; t := rec.Top;
      if ss.X < 0 then begin dec(l, sX); dec(r,sX); end;
      for i := 1 to sY do
      begin
        c := tmpImg.Pixel[sX+1, sY-i];
        FillColorHorz(img, l, r, t-i, c);
      end;
    end;
    // r-edge
    if (ss.X > 0) then
    begin
      r := rec.Right-1; t := rec.Top+sY; b := rec.Bottom-1;
      if ss.Y < 0 then begin dec(t, sY); dec(b,sY); end;
      for i := 1 to sX do
      begin
        c := tmpImg.Pixel[sX*2+i, sY+1];
        FillColorVert(img, r+i, t, b, c);
      end;
    end;
    // b-edge
    if (ss.Y > 0) then
    begin
      l := rec.Left+sX; r := rec.Right-1; b := rec.Bottom-1;
      if ss.X < 0 then begin dec(l, sX); dec(r,sX); end;
      for i := 1 to sY do
      begin
        c := tmpImg.Pixel[sX+1, sY*2+i];
        FillColorHorz(img, l, r, b+i, c);
      end;
    end;
  finally
    tmpImg.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawShadow(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; depth: double; angleRads: double;
  color: TColor32; cutoutInsideShadow: Boolean);
var
  polygons: TPathsD;
begin
  setlength(polygons, 1);
  polygons[0] := polygon;
  DrawShadow(img, polygons, fillRule, depth,
    angleRads, color, cutoutInsideShadow);
end;
//------------------------------------------------------------------------------

procedure DrawShadow(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; depth: double; angleRads: double;
  color: TColor32; cutoutInsideShadow: Boolean);
var
  x, y: double;
  blurSize, w,h: integer;
  rec: TRect;
  polys, shadowPolys: TPathsD;
  shadowImg: TImage32;
begin
  rec := GetBounds(polygons);
  if IsEmptyRect(rec) or (depth < 1) then Exit;
{$IFDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
  angleRads := -angleRads;
{$ENDIF}
  NormalizeAngle(angleRads);
  GetSinCos(angleRads, y, x);
  depth := depth * 0.5;
  x := depth * x;
  y := depth * y;
  blurSize := Max(1,Round(depth / 2));
  Img32.Vector.InflateRect(rec, Ceil(depth*2), Ceil(depth*2));
  polys := TranslatePath(polygons, -rec.Left, -rec.Top);
  shadowPolys := TranslatePath(polys, x, y);
  RectWidthHeight(rec, w, h);
  shadowImg := TImage32.Create(w, h);
  try
    DrawPolygon(shadowImg, shadowPolys, fillRule, color);
    FastGaussianBlur(shadowImg, shadowImg.Bounds, blurSize, 1);
    if cutoutInsideShadow then EraseInsidePaths(shadowImg, polys, fillRule);
    img.CopyBlend(shadowImg, shadowImg.Bounds, rec, BlendToAlphaLine);
  finally
    shadowImg.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawGlow(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; color: TColor32; blurRadius: integer);
var
  polygons: TPathsD;
begin
  setlength(polygons, 1);
  polygons[0] := polygon;
  DrawGlow(img, polygons, fillRule, color, blurRadius);
end;
//------------------------------------------------------------------------------

procedure DrawGlow(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; color: TColor32; blurRadius: integer);
var
  w,h: integer;
  rec: TRect;
  glowPolys: TPathsD;
  glowImg: TImage32;
begin
  rec := GetBounds(polygons);
  glowPolys := TranslatePath(polygons,
    blurRadius -rec.Left +1, blurRadius -rec.Top +1);
  Img32.Vector.InflateRect(rec, blurRadius +1, blurRadius +1);
  RectWidthHeight(rec, w, h);
  glowImg := TImage32.Create(w, h);
  try
    DrawPolygon(glowImg, glowPolys, fillRule, color);
    FastGaussianBlur(glowImg, glowImg.Bounds, blurRadius, 2);
    glowImg.ScaleAlpha(4);
    img.CopyBlend(glowImg, glowImg.Bounds, rec, BlendToAlphaLine);
  finally
    glowImg.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure Sharpen(img: TImage32; radius: Integer; amount: Integer);
var
  i: Integer;
  amt: double;
  weightAmount: array [-255 .. 255] of Integer;
  bmpBlur: TImage32;
  pColor, pBlur: PARGB;
begin
  if radius = 0 then Exit;
  amt := ClampRange(amount/10, 0.1, 5);
  radius := ClampRange(radius, 1, 10);
  for i := -255 to 255 do
    weightAmount[i] := Round(amt * i);
  bmpBlur := TImage32.Create(img); // clone self
  try
    pColor := PARGB(img.pixelBase);
    FastGaussianBlur(bmpBlur, bmpBlur.Bounds, radius, 2);
    pBlur := PARGB(bmpBlur.pixelBase);
    for i := 1 to img.Width * img.Height do
    begin
      if (pColor.A > 0) then
      begin
        pColor.R := ClampByte(pColor.R  + weightAmount[pColor.R - pBlur.R]);
        pColor.G := ClampByte(pColor.G  + weightAmount[pColor.G - pBlur.G]);
        pColor.B := ClampByte(pColor.B  + weightAmount[pColor.B - pBlur.B]);
      end;
      Inc(pColor); Inc(pBlur);
    end;
  finally
    bmpBlur.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure InternalHatchBackground(img: TImage32; const rec: TRect;
  color1, color2: TColor32; hatchSize: Integer = 10);
var
  i, j, imgWidth: Integer;
  pc: PColor32;
  colors: array[boolean] of TColor32;
  hatch: Boolean;
  x: integer;
begin
  colors[false] := color1;
  colors[true] := color2;
  imgWidth := img.Width;

  for i := rec.Top to rec.Bottom -1 do
  begin
    pc := @img.Pixels[i * imgWidth + rec.Left];
    hatch := Odd(i div hatchSize);

    x := (rec.Left + 1) mod hatchSize;
    if x = 0 then hatch := not hatch;
    for j := rec.Left to rec.Right -1 do
    begin
      if pc^ = 0 then
        pc^ := colors[hatch]
      else if GetAlpha(pc^) < 255 then
        pc^ := BlendToOpaque(colors[hatch], pc^);
      inc(pc);
      inc(x);
      if x >= hatchSize then
      begin
        x := 0;
        hatch := not hatch;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure HatchBackground(img: TImage32; const rec: TRect;
  color1: TColor32 = clWhite32; color2: TColor32= $FFE8E8E8;
  hatchSize: Integer = 10); overload;
begin
  if (rec.Right <= rec.Left) or (rec.Bottom - rec.Top <= 0) then Exit;
  img.BeginUpdate;
  try
    InternalHatchBackground(img, rec, color1, color2, hatchSize);
  finally
    img.EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure HatchBackground(img: TImage32;
  color1: TColor32; color2: TColor32; hatchSize: Integer);
begin
  HatchBackground(img, img.Bounds, color1, color2, hatchSize);
end;
//------------------------------------------------------------------------------

procedure GridBackground(img: TImage32; majorInterval, minorInterval: integer;
  fillColor: TColor32; majColor: TColor32; minColor: TColor32);
var
  i, x,y, w,h: integer;
  path: TPathD;
  cr: TCustomColorRenderer;
begin
  img.Clear(fillColor);
  w := img.Width; h := img.Height;
  NewPointDArray(path, 2, True);

  if img.AntiAliased then
    cr := TColorRenderer.Create(minColor) else
    cr := TAliasedColorRenderer.Create(minColor);
  try
    if minorInterval > 0 then
    begin
      //cr.SetColor(minColor);

      x := minorInterval;
      path[0] := PointD(x, 0); path[1] := PointD(x, h);;
      for i := 1 to (w div minorInterval) do
      begin
        Img32.Draw.DrawLine(img, path, 1, cr, esSquare);
        path[0].X := path[0].X + minorInterval;
        path[1].X := path[1].X + minorInterval;
      end;
      y := minorInterval;
      path[0] := PointD(0, y); path[1] := PointD(w, y);
      for i := 1 to (h div minorInterval) do
      begin
        Img32.Draw.DrawLine(img, path, 1, cr, esSquare);
        path[0].Y := path[0].Y + minorInterval;
        path[1].Y := path[1].Y + minorInterval;
      end;
    end;
    if majorInterval > minorInterval then
    begin
      cr.SetColor(majColor);

      x := majorInterval;
      path[0] := PointD(x, 0); path[1] := PointD(x, h);;
      for i := 1 to (w div majorInterval) do
      begin
        Img32.Draw.DrawLine(img, path, 1, cr, esSquare);
        path[0].X := path[0].X + majorInterval;
        path[1].X := path[1].X + majorInterval;
      end;
      y := majorInterval;
      path[0] := PointD(0, y); path[1] := PointD(w, y);
      for i := 1 to (h div majorInterval) do
      begin
        Img32.Draw.DrawLine(img, path, 1, cr, esSquare);
        path[0].Y := path[0].Y + majorInterval;
        path[1].Y := path[1].Y + majorInterval;
      end;
    end;
  finally
    cr.Free;
  end;
end;
//------------------------------------------------------------------------------

function ColorDifference(color1, color2: TColor32): cardinal;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  c1: TARGB absolute color1;
  c2: TARGB absolute color2;
begin
  result := Abs(c1.R - c2.R) + Abs(c1.G - c2.G) + Abs(c1.B - c2.B);
  result := (result * 341) shr 10; // divide by 3
end;
//------------------------------------------------------------------------------

procedure ReplaceExactColor(img: TImage32; oldColor, newColor: TColor32);
var
  color: PColor32;
  i: Integer;
begin
  color := img.PixelBase;
  for i := 0 to img.Width * img.Height -1 do
  begin
    if color^ = oldColor then color^ := newColor;
    inc(color);
  end;
end;
//------------------------------------------------------------------------------

procedure RemoveColor(img: TImage32; color: TColor32);
var
  fg: TARGB absolute color;
  bg: PARGB;
  i: Integer;
  Q: byte;
begin
  if fg.A = 0 then Exit;
  bg := PARGB(img.PixelBase);
  for i := 0 to img.Width * img.Height -1 do
  begin
    if bg.A > 0 then
    begin
      // red
      if (bg.R > fg.R) then Q := bg.R - fg.R
      else if (bg.R < fg.R) then Q := DivTable[fg.R - bg.R, fg.R]
      else Q := 0;
      // green
      if (bg.G > fg.G) then Q := Max(Q, bg.G - fg.G)
      else if (bg.G < fg.G) then Q := Max(Q, DivTable[fg.G - bg.G, fg.G]);
      // blue
      if (bg.B > fg.B) then Q := Max(Q, bg.B - fg.B)
      else if (bg.B < fg.B) then Q := Max(Q, DivTable[fg.B - bg.B, fg.B]);

      // weight Q toward either fully opaque or fully translucent
      Q := Sigmoid[Q];

      if (Q = 0) then
        bg.Color := clNone32
      else if (Q < 255) then
      begin
        bg.A := MulTable[bg.A, Q];
        bg.R := DivTable[bg.R - MulTable[not Q, fg.R], Q];
        bg.G := DivTable[bg.G - MulTable[not Q, fg.G], Q];
        bg.B := DivTable[bg.B - MulTable[not Q, fg.B], Q];
      end;
    end;
    inc(bg);
  end;
end;
//------------------------------------------------------------------------------

procedure FilterOnColor(img: TImage32; color: TColor32);
var
  fg: TARGB absolute color;
  bg: PARGB;
  i: Integer;
  Q: byte;
begin
  if fg.A = 0 then Exit;
  bg := PARGB(img.PixelBase);
  for i := 0 to img.Width * img.Height -1 do
  begin
    if bg.A > 0 then
    begin
      // red
      if (bg.R > fg.R) then
        Q := bg.R - fg.R
      else if (bg.R < fg.R) then
        Q := DivTable[fg.R - bg.R, fg.R]
      else
        Q := 0;

      // green
      if (bg.G > fg.G) then
        Q := Max(Q, bg.G - fg.G)
      else if (bg.G < fg.G) then
        Q := Max(Q, DivTable[fg.G - bg.G, fg.G]);

      // blue
      if (bg.B > fg.B) then
        Q := Max(Q, bg.B - fg.B)
      else if (bg.B < fg.B) then
        Q := Max(Q, DivTable[fg.B - bg.B, fg.B]);

      // weight Q toward either fully opaque or fully translucent
      Q := Sigmoid[Q];

      Q := MulTable[bg.A, not Q];
      bg.Color := color;
      bg.A := Q; // note: fg.A is ignored
    end;
    inc(bg);
  end;
end;
//------------------------------------------------------------------------------

procedure FilterOnExactColor(img: TImage32; color: TColor32);
var
  pc: PColor32;
  i: Integer;
  mask: TColor32;
begin
  // alpha channel is ignored
  mask := $FFFFFF;
  color := color and mask;

  pc := img.PixelBase;
  for i := 0 to img.Width * img.Height -1 do
  begin
    if (pc^ and mask) <> color then pc^ := clNone32;
    inc(pc);
  end;
end;
//------------------------------------------------------------------------------

procedure FilterOnAlpha(img: TImage32; alpha: byte; tolerance: byte);
var
  bg: PARGB;
  i: Integer;
begin
  bg := PARGB(img.PixelBase);
  for i := 0 to img.Width * img.Height -1 do
  begin
    if abs(bg.A - alpha) > tolerance then bg.A := 0;
    inc(bg);
  end;
end;
//------------------------------------------------------------------------------

procedure RedEyeRemove(img: TImage32; const rect: TRect);
var
  k: integer;
  cutout, mask: TImage32;
  path: TPathD;
  cutoutRec, rect3: TRect;
  radGrad: TRadialGradientRenderer;
begin
  k := RectWidth(rect) * RectHeight(rect);
  if k < 120 then k := 2
  else if k < 230 then k := 3
  else k := 4;
  cutoutRec := rect;
  Img32.Vector.InflateRect(cutoutRec, k, k);
  cutout  := TImage32.Create(img, cutoutRec);
  mask    := TImage32.Create(cutout.Width, cutout.Height);
  radGrad := TRadialGradientRenderer.Create;
  try
    // fill behind the cutout with black also
    // blurring the fill to soften its edges
    rect3 := cutout.Bounds;
    Img32.Vector.InflateRect(rect3, -k, -k);
    path := Ellipse(rect3);
    DrawPolygon(mask, path, frNonZero, clBlack32);
    // given the very small area and small radius of the blur, the
    // speed improvement of BoxBlur over GaussianBlur is inconsequential.
    GaussianBlur(mask, mask.Bounds, k);
    img.CopyBlend(mask, mask.Bounds, cutoutRec, BlendToOpaque);
    // gradient fill to clNone32 a mask to soften cutout's edges
    path := Ellipse(cutoutRec);
    radGrad.SetParameters(rect3, clBlack32, clNone32);
    DrawPolygon(mask, path, frNonZero, radGrad);
    cutout.CopyBlend(mask, mask.Bounds, cutout.Bounds, BlendMaskLine);
    // now remove red from the cutout
    RemoveColor(cutout, clRed32);
    // finally replace the cutout ...
    img.CopyBlend(cutout, cutout.Bounds, cutoutRec, BlendToOpaque);
  finally
    mask.Free;
    cutout.Free;
    radGrad.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure EraseInsidePath(img: TImage32; const path: TPathD; fillRule: TFillRule);
begin
  if assigned(path) then
    ErasePolygon(img, path, fillRule);
end;
//------------------------------------------------------------------------------

procedure EraseInsidePaths(img: TImage32; const paths: TPathsD; fillRule: TFillRule);
begin
  if assigned(paths) then
    ErasePolygon(img, paths, fillRule);
end;
//------------------------------------------------------------------------------

procedure EraseOutsideRect(img: TImage32; const r, outsideBounds: TRect);
begin
  // Fill the parts, that are in outsideBounds but not in r with zeros

  // whole top block
  if r.Top > outsideBounds.Top then
    img.FillRect(Rect(outsideBounds.Left, outsideBounds.Top, outsideBounds.Right, r.Top - 1), 0);
  // whole bottom block
  if r.Bottom < outsideBounds.Bottom then
    img.FillRect(Rect(outsideBounds.Left, r.Bottom + 1, outsideBounds.Right, outsideBounds.Bottom), 0);

  // remaining left block
  if r.Left > outsideBounds.Left then
    img.FillRect(Rect(outsideBounds.Left, r.Top, r.Left - 1, r.Bottom), 0);
  // remaining right block
  if r.Right < outsideBounds.Right then
    img.FillRect(Rect(r.Right + 1, r.Top, outsideBounds.Right, r.Bottom), 0);
end;
//------------------------------------------------------------------------------

procedure EraseOutsidePath(img: TImage32; const path: TPathD;
  fillRule: TFillRule; const outsideBounds: TRect);
var
  w, h: integer;
  renderer: TMaskRenderer;
  r: TRect;
  polygons: TPathsD;
begin
  if not assigned(path) then Exit;
  RectWidthHeight(outsideBounds, w, h);
  if (w <= 0) or (h <= 0)  then Exit;

  // We can skip the costly polygon rasterization if the path is
  // a rectangle
  if (fillRule in [frEvenOdd, frNonZero]) and IsSimpleRectanglePath(path, r) then
  begin
    EraseOutsideRect(img, r, outsideBounds);
    Exit;
  end;

  renderer := TMaskRenderer.Create;
  try
    SetLength(polygons, 1);
    polygons[0] := path;
    Rasterize(img, polygons, outsideBounds, fillRule, renderer);
  finally
    renderer.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure EraseOutsidePaths(img: TImage32; const paths: TPathsD;
  fillRule: TFillRule; const outsideBounds: TRect;
  rendererCache: TCustomRendererCache);
var
  w, h: integer;
  renderer: TMaskRenderer;
  r: TRect;
begin
  if not assigned(paths) then Exit;
  RectWidthHeight(outsideBounds, w, h);
  if (w <= 0) or (h <= 0)  then Exit;

  // We can skip the costly polygon rasterization if the path is
  // a rectangle.
  if (fillRule in [frEvenOdd, frNonZero]) and IsSimpleRectanglePath(paths, r) then
  begin
    EraseOutsideRect(img, r, outsideBounds);
    Exit;
  end;

  if rendererCache = nil then
    renderer := TMaskRenderer.Create
  else
    renderer := rendererCache.MaskRenderer;
  try
    Rasterize(img, paths, outsideBounds, fillRule, renderer);
  finally
    if rendererCache = nil then
      renderer.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure Draw3D(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; height, blurRadius: double;
  colorLt: TColor32; colorDk: TColor32; angleRads: double);
var
  polygons: TPathsD;
begin
  setLength(polygons, 1);
  polygons[0] := polygon;
  Draw3D(img, polygons, fillRule, height, blurRadius, colorLt, colorDk, angleRads);
end;
//------------------------------------------------------------------------------

procedure Draw3D(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; height, blurRadius: double;
  colorLt: TColor32; colorDk: TColor32; angleRads: double);
var
  tmp: TImage32;
  rec: TRect;
  paths, paths2: TPathsD;
  w,h: integer;
  x,y: double;
begin
  rec := GetBounds(polygons);
  if IsEmptyRect(rec) then Exit;
{$IFDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
  angleRads := -angleRads;
{$ENDIF}
  GetSinCos(angleRads, y, x);
  paths := TranslatePath(polygons, -rec.Left, -rec.Top);
  RectWidthHeight(rec, w, h);
  tmp := TImage32.Create(w, h);
  try
    if GetAlpha(colorLt) > 0 then
    begin
      tmp.Clear(colorLt);
      paths2 := TranslatePath(paths, -height*x, -height*y);
      EraseInsidePaths(tmp, paths2, fillRule);
      FastGaussianBlur(tmp, tmp.Bounds, Round(blurRadius), 0);
      EraseOutsidePaths(tmp, paths, fillRule, tmp.Bounds);
      img.CopyBlend(tmp, tmp.Bounds, rec, BlendToAlphaLine);
    end;
    if GetAlpha(colorDk) > 0 then
    begin
      tmp.Clear(colorDk);
      paths2 := TranslatePath(paths, height*x, height*y);
      EraseInsidePaths(tmp, paths2, fillRule);
      FastGaussianBlur(tmp, tmp.Bounds, Round(blurRadius), 0);
      EraseOutsidePaths(tmp, paths, fillRule, tmp.Bounds);
      img.CopyBlend(tmp, tmp.Bounds, rec, BlendToAlphaLine);
    end;
  finally
    tmp.Free;
  end;
end;
//------------------------------------------------------------------------------

function RainbowColor(fraction: double; luminance: byte = 128): TColor32;
var
  hsl: THsl;
begin
  if (fraction < 0) or (fraction > 1) then
    fraction := frac(fraction);

  hsl.hue := Round(fraction * 255);
  hsl.sat := 255;
  hsl.lum := luminance;
  hsl.alpha := 255;
  Result := HslToRgb(hsl);
end;
//------------------------------------------------------------------------------

function GradientColor(color1, color2: TColor32; frac: single): TColor32;
var
  hsl1, hsl2: THsl;
begin
  if (frac <= 0) then result := color1
  else if (frac >= 1) then result := color2
  else
  begin
    hsl1 := RgbToHsl(color1); hsl2 := RgbToHsl(color2);
    hsl1.hue := ClampByte(hsl1.hue*(1-frac) + hsl2.hue*frac);
    hsl1.sat := ClampByte(hsl1.sat*(1-frac) + hsl2.sat*frac);
    hsl1.lum := ClampByte(hsl1.lum*(1-frac) + hsl2.lum*frac);
    hsl1.alpha := ClampByte(hsl1.alpha*(1-frac) + hsl2.alpha*frac);
    Result := HslToRgb(hsl1);
  end;
end;
//------------------------------------------------------------------------------

function MakeDarker(color: TColor32; percent: cardinal): TColor32;
var
  hsl: THsl;
begin
  hsl := RgbToHsl(color);
  hsl.lum := ClampByte(hsl.lum - (percent/100 * hsl.lum));
  Result := HslToRgb(hsl);
end;
//------------------------------------------------------------------------------

function MakeLighter(color: TColor32; percent: cardinal): TColor32;
var
  hsl: THsl;
begin
  hsl := RgbToHsl(color);
  hsl.lum := ClampByte(hsl.lum + percent/100 * (255 - hsl.lum));
  Result := HslToRgb(hsl);
end;
//------------------------------------------------------------------------------

function DrawButton(img: TImage32; const pt: TPointD;
  size: double; color: TColor32; buttonShape: TButtonShape;
  buttonAttributes: TButtonAttributes): TPathD;
var
  i: integer;
  radius: double;
  rec: TRectD;
  lightSize, lightAngle: double;
begin
  if (size < 5) then Exit;
  radius := size * 0.5;
  lightSize := radius * 0.25;
  rec := RectD(pt.X -radius, pt.Y -radius, pt.X +radius, pt.Y +radius);
  if baEraseBeneath in buttonAttributes then
    img.Clear(Rect(rec));
  case buttonShape of
    bsDiamond:
      begin
        NewPointDArray(Result, 4, True);
        for i := 0 to 3 do Result[i] := pt;
        Result[0].X := Result[0].X -radius;
        Result[1].Y := Result[1].Y -radius;
        Result[2].X := Result[2].X +radius;
        Result[3].Y := Result[3].Y +radius;
      end;
    bsSquare:
      begin
        Img32.Vector.InflateRect(rec, -1,-1);
        Result := Rectangle(rec);
      end;
    else
      Result := Ellipse(rec);
  end;
  lightAngle := angle225;
  img.BeginUpdate;
  try
    // nb: only need to cutout the inside shadow if
    // the pending color fill is semi-transparent
    if baShadow in buttonAttributes then
      DrawShadow(img, Result, frNonZero, lightSize *2,
        (lightAngle + angle180), $AA000000, GetAlpha(color) < $FE);
    if GetAlpha(color) > 2 then
      DrawPolygon(img, Result, frNonZero, color);
    if ba3D in buttonAttributes then
      Draw3D(img, Result, frNonZero, lightSize*2,
        Ceil(lightSize), $CCFFFFFF, $AA000000, lightAngle);
    DrawLine(img, Result, dpiAware1, clBlack32, esPolygon);
  finally
    img.EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

function AlphaAverage(color1, color2: TColor32): cardinal;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  c1: TARGB absolute color1;
  c2: TARGB absolute color2;
begin
  result := (c1.A + c2.A) shr 1;
end;
//------------------------------------------------------------------------------

function BlendAverage(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
begin
  res.A := (fg.A + bg.A) shr 1;
  res.R := (fg.R + bg.R) shr 1;
  res.G := (fg.G + bg.G) shr 1;
  res.B := (fg.B + bg.B) shr 1;
end;
//------------------------------------------------------------------------------

function BlendLinearBurn(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
begin
  res.A := 255;
  res.R := Max(0, bg.R + fg.R - 255);
  res.G := Max(0, bg.G + fg.G - 255);
  res.B := Max(0, bg.B + fg.B - 255);
end;
//------------------------------------------------------------------------------

function BlendColorDodge(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
begin
  res.A := 255;
  res.R := DivTable[bg.R, not fg.R];
  res.G := DivTable[bg.G, not fg.G];
  res.B := DivTable[bg.B, not fg.B];
end;
//------------------------------------------------------------------------------

procedure PencilEffect(img: TImage32; intensity: integer);
var
  img2: TImage32;
begin
  if img.IsEmpty then Exit;
  intensity := max(1, min(10, intensity));
  img.Grayscale;
  img2 := TImage32.Create(img);
  try
    img2.InvertColors;
    FastGaussianBlur(img2, img2.Bounds, intensity, 2);
    img.CopyBlend(img2, img2.Bounds, img.Bounds, BlendColorDodge);
  finally
    img2.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TraceContours(img: TImage32; intensity: integer);
var
  i,j, w,h: integer;
  tmp, tmp2: TArrayOfColor32;
  s, s2: PColor32;
  d: PARGB;
begin
  w := img.Width; h := img.Height;
  if w * h = 0 then Exit;
  NewColor32Array(tmp, w * h);
  NewColor32Array(tmp2, w * h);
  s := img.PixelRow[0]; d := @tmp[0];
  for j := 0 to h-1 do
  begin
    s2 := IncPColor32(s, 1);
    for i := 0 to w-2 do
    begin
      d.A := ColorDifference(s^, s2^);
      inc(s); inc(s2); inc(d);
    end;
    inc(s); inc(d);
  end;
  for j := 0 to w-1 do
  begin
    s := @tmp[j]; d := @tmp2[j];
    s2 := IncPColor32(s, w);
    for i := 0 to h-2 do
    begin
      d.A := AlphaAverage(s^, s2^);
      inc(s, w); inc(s2, w); inc(d, w);
    end;
  end;

  img.BlockNotify;
  img.AssignPixelArray(tmp2, w, h);
  img.UnblockNotify;

  if intensity < 1 then Exit;
  if intensity > 10 then
    intensity := 10; // range = 1-10
  img.ScaleAlpha(intensity);
end;
//------------------------------------------------------------------------------
// FLOODFILL - AND SUPPORT FUNCTIONS
//------------------------------------------------------------------------------
type
  PFloodFillRec = ^TFloodFillRec;
  TFloodFillRec = record
    xLeft     : Integer;
    xRight    : Integer;
    y         : Integer;
    dirY      : Integer;
    next      : PFloodFillRec;
  end;
  TFloodFillStack = class
    first     : PFloodFillRec;
    maxY      : integer;
    constructor Create(maxY: integer);
    destructor Destroy; override;
    procedure Push(xLeft, xRight,y, direction: Integer);
    procedure Pop(out xLeft, xRight,y, direction: Integer);
    function IsEmpty: Boolean;
  end;
  TFloodFillMask = class
  private
    img          : TImage32;
    mask         : TImage32;
    colorsRow    : PColor32Array;
    maskRow      : PColor32Array;
    initialColor : TColor32;
    compareFunc  : TCompareFunctionEx;
    tolerance    : Integer;
  public
    function Execute(imgIn, imgMaskOut: TImage32; x,y: integer;
      aTolerance: Byte = 0; compFunc: TCompareFunctionEx = nil): Boolean;
    procedure SetCurrentY(y: Integer);
    function IsMatch(x: Integer): Boolean;
  end;
//------------------------------------------------------------------------------
// TFloodFillStack methods
//------------------------------------------------------------------------------
constructor TFloodFillStack.Create(maxY: integer);
begin
  self.maxY := maxY;
end;
//------------------------------------------------------------------------------
destructor TFloodFillStack.Destroy;
var
  ffr: PFloodFillRec;
begin
  while assigned(first) do
  begin
    ffr := first;
    first := first.next;
    dispose(ffr);
  end;
end;
//------------------------------------------------------------------------------

procedure TFloodFillStack.Push(xLeft, xRight, y, direction: Integer);
var
  ffr: PFloodFillRec;
begin
  if ((y <= 0) and (direction = -1)) or
    ((y >= maxY) and (direction = 1)) then Exit;
  new(ffr);
  ffr.xLeft  := xLeft;
  ffr.xRight := xRight;
  ffr.y      := y;
  ffr.dirY   := direction;
  ffr.next   := first;
  first      := ffr;
end;
//------------------------------------------------------------------------------

procedure TFloodFillStack.Pop(out xLeft, xRight, y, direction: Integer);
var
  ffr: PFloodFillRec;
begin
  xLeft     := first.xLeft;
  xRight    := first.xRight;
  direction := first.dirY;
  y         := first.y + direction;
  ffr := first;
  first := first.next;
  dispose(ffr);
end;
//------------------------------------------------------------------------------

function TFloodFillStack.IsEmpty: Boolean;
begin
  result := not assigned(first);
end;
//------------------------------------------------------------------------------
// TFloodFillMask methods
//------------------------------------------------------------------------------

function TFloodFillMask.Execute(imgIn, imgMaskOut: TImage32; x,y: integer;
  aTolerance: Byte; compFunc: TCompareFunctionEx): Boolean;
var
  ffs          : TFloodFillStack;
  w,h          : integer;
  xl, xr, xr2  : Integer;
  maxX         : Integer;
  dirY         : Integer;
begin
  Result := Assigned(imgIn) and Assigned(imgMaskOut) and
    InRange(x,0,imgIn.Width -1) and InRange(y,0,imgIn.Height -1);
  if not Result then Exit;
  w := imgIn.Width; h := imgIn.Height;
  // make sure the mask is the size of the image
  imgMaskOut.SetSize(w,h);
  img   := imgIn;
  mask  := imgMaskOut;
  compareFunc := compFunc;
  tolerance := aTolerance;
  maxX := w -1;
  ffs := TFloodFillStack.create(h -1);
  try
    initialColor := imgIn.Pixel[x, y];
    xl := x; xr := x;
    SetCurrentY(y);
    IsMatch(x);
    while (xl > 0) and IsMatch(xl -1) do dec(xl);
    while (xr < maxX) and IsMatch(xr +1) do inc(xr);
    ffs.Push(xl, xr, y, -1); // down
    ffs.Push(xl, xr, y, 1);  // up
    while not ffs.IsEmpty do
    begin
      ffs.Pop(xl, xr, y, dirY);
      SetCurrentY(y);
      xr2 := xl;
      // check left ...
      if IsMatch(xl) then
      begin
        while (xl > 0) and IsMatch(xl-1) do dec(xl);
        if xl <= xr2 -2 then
          ffs.Push(xl, xr2-2, y, -dirY);
        while (xr2 < maxX) and IsMatch(xr2+1) do inc(xr2);
        ffs.Push(xl, xr2, y, dirY);
        if xr2 >= xr +2 then
          ffs.Push(xr+2, xr2, y, -dirY);
        xl := xr2 +2;
      end;
      // check right ...
      while (xl <= xr) and not IsMatch(xl) do inc(xl);
      while (xl <= xr) do
      begin
        xr2 := xl;
        while (xr2 < maxX) and IsMatch(xr2+1) do inc(xr2);
        ffs.Push(xl, xr2, y, dirY);
        if xr2 >= xr +2 then
        begin
          ffs.Push(xr+2, xr2, y, -dirY);
          break;
        end;
        inc(xl, 2);
        while (xl <= xr) and not IsMatch(xl) do inc(xl);
      end;
    end;
  finally
    ffs.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TFloodFillMask.SetCurrentY(y: Integer);
begin
  colorsRow := PColor32Array(img.PixelRow[y]);
  maskRow   := PColor32Array(mask.PixelRow[y]);
end;
//------------------------------------------------------------------------------

function TFloodFillMask.IsMatch(x: Integer): Boolean;
var
  b: Byte;
begin
  if (maskRow[x] > 0) then
    result := false
  else
  begin
    b := compareFunc(initialColor, colorsRow[x]);
    result := b < tolerance;
    if Result then
      maskRow[x] := tolerance - b else
      maskRow[x] := 1;
  end;
end;
//------------------------------------------------------------------------------

function GetFloodFillMask(imgIn, imgMaskOut: TImage32; x, y: Integer;
  tolerance: Byte; compareFunc: TCompareFunctionEx): Boolean;
var
  ffm: TFloodFillMask;
begin
  if not Assigned(compareFunc) then compareFunc := CompareRGBEx;
  ffm := TFloodFillMask.Create;
  try
    Result := ffm.Execute(imgIn, imgMaskOut, x, y, tolerance, compareFunc);
  finally
    ffm.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure FloodFill(img: TImage32; x, y: Integer; newColor: TColor32;
  tolerance: Byte; compareFunc: TCompareFunctionEx);
var
  i: Integer;
  pc, pm: PColor32;
  mask: TImage32;
begin
  if not assigned(compareFunc) then
  begin
    compareFunc := CompareRGBEx;
    if tolerance = 0 then
      tolerance := FloodFillDefaultRGBTolerance;
  end;
  mask := TImage32.Create;
  try
    if not GetFloodFillMask(img, mask, x, y, tolerance, compareFunc) then
      Exit;
    pc := img.PixelBase;
    pm := mask.PixelBase;
    for i := 0 to img.Width * img.Height -1 do
    begin
      if (pm^ > 1) then pc^ := newColor;
      inc(pm); inc(pc);
    end;
  finally
    mask.free;
  end;
end;
//------------------------------------------------------------------------------
// EMBOSS - AND SUPPORT FUNCTIONS
//------------------------------------------------------------------------------

function IncPWeightColor(pwc: PWeightedColor; cnt: Integer): PWeightedColor;
begin
  result := PWeightedColor(PByte(pwc) + cnt * SizeOf(TWeightedColor));
end;
//------------------------------------------------------------------------------

function Intensity(color: TColor32): byte;
var
  c: TARGB absolute color;
begin
  Result := (c.R * 61 + c.G * 174 + c.B * 21) shr 8;
end;
//------------------------------------------------------------------------------

function Gray(color: TColor32): TColor32;
var
  c: TARGB absolute color;
  res: TARGB absolute Result;
begin
  res.A := c.A;
  res.R := Intensity(color);
  res.G := res.R;
  res.B := res.R;
end;
//------------------------------------------------------------------------------

procedure Emboss(img: TImage32; radius: Integer;
  depth: Integer; luminance: Integer; preserveColor: Boolean);
var
  yy,xx, x,y, w,h: Integer;
  b: byte;
  kernel: array [0 .. MaxBlur, 0 .. MaxBlur] of Integer;
  wca: TArrayOfWeightedColor;
  pc0, pcf, pcb: PColor32; // pointers to pixels (forward & backward in kernel)
  pw0, pw: PWeightedColor; // pointers to weight
  customGray: TColor32;
  pc: PColor32;
const
  maxDepth = 50;
begin
  // grayscale luminance as percent where 0% is black and 100% is white
  //(luminance is ignored when preserveColor = true)
  luminance := ClampRange(luminance, 0, 100);
  b := luminance *255 div 100;
  customGray := $FF000000 + b shl 16 + b shl 8 + b;
  ClampRange(radius, 1, 5);
  inc(depth);
  ClampRange(depth, 2, maxDepth);
  kernel[0][0] := 1;
  for y := 1 to radius do
    for x := 1 to radius do
      kernel[y][x] := depth;
  w := img.Width; h := img.Height;
  // nb: dynamic arrays are zero-initialized (unless they're a function result)
  SetLength(wca, w * h);
  pc0 := IncPColor32(img.PixelBase, radius * w);
  pw0 := @wca[radius * w];
  for y := radius to h -1 - radius do
  begin
    for x := radius to w -1 - radius do
    begin
      pw := IncPWeightColor(pw0, x);
      pcb := IncPColor32(pc0, x - 1);
      if preserveColor then
      begin
        pcf := IncPColor32(pc0, x);
        pw^.Add(pcf^, kernel[0,0]);
        inc(pcf);
      end else
      begin
        pw^.Add(customGray, kernel[0,0]);
        pcf := IncPColor32(pc0, x + 1);
      end;
      // parse the kernel ...
      for yy := 1 to radius do
      begin
        for xx := 1 to radius do
        begin
          pw^.Subtract(Gray(pcf^), kernel[yy,xx]);
          pw^.Add(Gray(pcb^), kernel[yy,xx]);
          dec(pcb); inc(pcf);
        end;
        dec(pcb, img.Width - radius);
        inc(pcf, img.Width - radius);
      end;
    end;
    inc(pc0, img.Width);
    inc(pw0, img.Width);
  end;
  pc := @img.Pixels[0]; pw := @wca[0];
  for x := 0 to img.width * img.Height - 1 do
  begin
    pc^ := pw.Color or $FF000000;
    inc(pc); inc(pw);
  end;
end;

//------------------------------------------------------------------------------
// RamerDouglasPeucker - and support functions
//------------------------------------------------------------------------------

procedure RDP(const path: TPathD; startIdx, endIdx: integer;
  epsilonSqrd: double; var flags: TArrayOfInteger);
var
  i, idx: integer;
  d, maxD: double;
begin
  idx := 0;
  maxD := 0;
  for i := startIdx +1 to endIdx -1 do
  begin
    // PerpendicularDistSqrd - avoids expensive Sqrt()
    d := PerpendicularDistSqrd(path[i], path[startIdx], path[endIdx]);
    if d <= maxD then Continue;
    maxD := d;
    idx := i;
  end;
  if maxD < epsilonSqrd then Exit;
  flags[idx] := 1;
  if idx > startIdx + 1 then RDP(path, startIdx, idx, epsilonSqrd, flags);
  if endIdx > idx + 1 then RDP(path, idx, endIdx, epsilonSqrd, flags);
end;
//------------------------------------------------------------------------------

function RamerDouglasPeucker(const path: TPathD;
  epsilon: double): TPathD;
var
  i,j, len: integer;
  buffer: TArrayOfInteger;
begin
  len := length(path);
  if len < 5 then
  begin
    result := Copy(path, 0, len);
    Exit;
  end;
  SetLength(buffer, len); // buffer is zero initialized
  buffer[0] := 1;
  buffer[len -1] := 1;
  RDP(path, 0, len -1, Sqr(epsilon), buffer);
  j := 0;
  SetLength(Result, len);
  for i := 0 to len -1 do
    if buffer[i] = 1 then
    begin
      Result[j] := path[i];
      inc(j);
    end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

function RamerDouglasPeucker(const paths: TPathsD;
  epsilon: double): TPathsD;
var
  i,j, len: integer;
begin
  j := 0;
  len := length(paths);
  setLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[j] := RamerDouglasPeucker(paths[i], epsilon);
    if Result[j] <> nil then inc(j);
  end;
  setLength(Result, j);
end;
//------------------------------------------------------------------------------

function GetNext(current, high: integer; var flags: array of Boolean): integer;
begin
  Result := current +1;
  while (Result <= high) and flags[Result] do inc(Result);
  if (Result <= high) then Exit;
  Result := 0;
  while (flags[Result]) do inc(Result);
end;
//---------------------------------------------------------------------------

function GetPrior(current, high: integer; var flags: array of Boolean): integer;
begin
  Result := current;
  if (Result = 0) then Result := high
  else dec(Result);
  while (Result > 0) and flags[Result] do dec(Result);
  if not flags[Result] then Exit;
  Result := high;
  while flags[Result] do dec(Result);
end;
//---------------------------------------------------------------------------

type
  PSimplifyRec = ^TSimplifyRec;
  TSimplifyRec = record
    pt      : TPointD;
    pdSqrd  : double;
    prev    : PSimplifyRec;
    next    : PSimplifyRec;
    isEndPt   : Boolean;
  end;

function SimplifyPath(const path: TPathD;
  shapeTolerance: double; isClosedPath: Boolean): TPathD;
var
  i, iPrev, iNext, len, minLen: integer;
  tolSqrd: double;
  srArray: array of TSimplifyRec;
  current, last: PSimplifyRec;
begin
  Result := nil;
  len := Length(path);
  if not isClosedPath then minLen := 2 else minLen := 3;
  if len < minLen then Exit;

  SetLength(srArray, len);
  for i := 0 to len -1 do
    with srArray[i] do
    begin
      iPrev := ModEx(i-1, len);
      iNext := ModEx(i+1, len);
      pt      := path[i];
      prev    := @srArray[iPrev];
      next    := @srArray[iNext];
      pdSqrd  := PerpendicularDistSqrd(path[i], path[iPrev], path[iNext]);
      isEndPt   := not isClosedPath and ((i = 0) or (i = len -1));
    end;

  current := @srArray[0];
  last := current.prev;

  tolSqrd := Sqr(shapeTolerance);
  while current <> last do
  begin
    if not current.isEndPt and
      ((current.pdSqrd < tolSqrd) and (current.next.pdSqrd > current.pdSqrd)) then
    begin
      current.prev.next := current.next;
      current.next.prev := current.prev;
      last := current.prev;
      dec(len);
      if last.next = last.prev then break;
      last.pdSqrd := PerpendicularDistSqrd(last.pt, last.prev.pt, last.next.pt);
      current := last.next;
      current.pdSqrd := PerpendicularDistSqrd(current.pt, current.prev.pt, current.next.pt);
    end
    else
      current := current.next;
  end;

  if len < minLen then Exit;
  if not isClosedPath then current := @srArray[0];
  NewPointDArray(Result, len, True);
  for i := 0 to len -1 do
  begin
    Result[i] := current.pt;
    current := current.next;
  end;
end;
//------------------------------------------------------------------------------

function SimplifyPaths(const paths: TPathsD;
  shapeTolerance: double; isClosedPath: Boolean): TPathsD;
var
  i,j, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  j := 0;
  for i := 0 to len -1 do
  begin
    result[j] := SimplifyPath(paths[i], shapeTolerance, isClosedPath);
    if Length(result[j]) > 0 then inc(j);
  end;
  SetLength(Result, j);
end;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

type
  PSimplifyExRec = ^TSimplifyExRec;
  TSimplifyExRec = record
    pt        : TPointD;
    pdSqrd    : double;
    segLenSq  : double;
    prev      : PSimplifyExRec;
    next      : PSimplifyExRec;
  end;

function DeleteCurrent(var current: PSimplifyExRec): Boolean;
var
  next: PSimplifyExRec;
begin
  current.prev.next := current.next;
  current.next.prev := current.prev;
  current := current.prev;
  next := current.next;
  Result := next <> current.prev;
  if not Result then Exit;
  next.pdSqrd := PerpendicularDistSqrd(next.pt, next.prev.pt, next.next.pt);
  current.segLenSq := DistanceSqrd(current.pt, current.next.pt);
  current.pdSqrd := PerpendicularDistSqrd(current.pt, current.prev.pt, current.next.pt);
end;
//---------------------------------------------------------------------------

function SimplifyPathEx(const path: TPathD; shapeTolerance: double): TPathD;
var
  i, prevI, nextI, len: integer;
  shapeTolSqr: double;
  srArray: array of TSimplifyExRec;
  current, start: PSimplifyExRec;
begin
  Result := nil;
  len := Length(path);
  if len < 3 then Exit;

  shapeTolSqr := Sqr(shapeTolerance);
  SetLength(srArray, len);

  for i := 0 to len -1 do
  begin
    prevI := i -1;
    nextI := i +1;
    if i = 0 then prevI := len -1
    else if i = len -1 then nextI := 0;

    with srArray[i] do
    begin
      pt      := path[i];
      segLenSq:= DistanceSqrd(path[i], path[nextI]);
      pdSqrd  := PerpendicularDistSqrd(path[i], path[prevI], path[nextI]);
      prev    := @srArray[prevI];
      next    := @srArray[nextI];
    end;
  end;

  current := @srArray[0];
  start := current.prev;

  while current <> start do
  begin
    // Irrespective of segment length, remove vertices that deviate very little
    // from imaginary lines that pass through their adjacent vertices.
    // However, if the following vertex has an even sorter distance from its
    // respective imaginary line, its important to remove that vertex first.
    if ((current.pdSqrd < shapeTolSqr) and
      (current.pdSqrd < current.next.pdSqrd)) then
    begin
      dec(len);
      if not DeleteCurrent(current) then Break;
      start := current.prev;
    end
    // also remove insignificant path zig-zags
    else if (current.prev.segLenSq < shapeTolSqr) and
      (current.segLenSq < shapeTolSqr) and
      ((CrossProduct(current.prev.pt, current.pt, current.next.pt) > 0) <>
      (CrossProduct(current.pt, current.next.pt, current.next.next.pt) > 0)) then
    begin
      dec(len);
      if not DeleteCurrent(current) then Break;
      start := current.prev;
    end else
      current := current.next;
  end;

  if len < 3 then Exit;
  NewPointDArray(Result, len, True);
  for i := 0 to len -1 do
  begin
    Result[i] := current.pt;
    current := current.next;
  end;
end;
//------------------------------------------------------------------------------

function SimplifyPathsEx(const paths: TPathsD; shapeTolerance: double): TPathsD;
var
  i,j, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  j := 0;
  for i := 0 to len -1 do
  begin
    Result[j] := SimplifyPathEx(paths[i], shapeTolerance);
    if Length(Result[j]) > 0 then inc(j);
  end;
  SetLength(Result, len);
end;
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


function DotProdVecs(const vec1, vec2: TPointD): double;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := (vec1.X * vec2.X + vec1.Y * vec2.Y);
end;
//---------------------------------------------------------------------------

function SmoothToCubicBezier(const path: TPathD;
  pathIsClosed: Boolean; maxOffset: integer): TPathD;
var
  i, j, len, prev: integer;
  vec: TPointD;
  pl: TArrayOfDouble;
  unitVecs: TPathD;
  d, angle, d1,d2: double;
begin
  // SmoothToCubicBezier - returns cubic bezier control points
  Result := nil;
  len := Length(path);
  if len < 3 then Exit;

  NewPointDArray(Result, len *3 +1, True);
  prev := len-1;
  SetLength(pl, len);
  SetLength(unitVecs, len);
  pl[0] := Distance(path[prev], path[0]);
  unitVecs[0] := GetUnitVector(path[prev], path[0]);
  for i := 0 to len -1 do
  begin
    if i = prev then
    begin
      j := 0;
    end else
    begin
      j := i +1;
      pl[j] := Distance(path[i], path[j]);
      unitVecs[j] := GetUnitVector(path[i], path[j]);
    end;
    vec := GetAvgUnitVector(unitVecs[i], unitVecs[j]);

    angle := arccos(Max(-1,Min(1,(DotProdVecs(unitVecs[i], unitVecs[j])))));
    d := abs(Pi-angle)/TwoPi;
    d1 := pl[i] * d;
    d2 := pl[j] * d;

    if maxOffset > 0 then
    begin
      d1 := Min(maxOffset, d1);
      d2 := Min(maxOffset, d2);
    end;

    if i = 0 then
      Result[len*3-1] := TranslatePoint(path[0], -vec.X * d1, -vec.Y * d1)
    else
      Result[i*3-1] := TranslatePoint(path[i], -vec.X * d1, -vec.Y * d1);
    Result[i*3] := path[i];
    Result[i*3+1] := TranslatePoint(path[i], vec.X * d2, vec.Y * d2);
  end;
  Result[len*3] := path[0];

  if pathIsClosed then Exit;
  Result[1] := Result[0];
  dec(len);
  Result[len*3-1] := Result[len*3];
  SetLength(Result, Len*3 +1);
end;
//------------------------------------------------------------------------------

function SmoothToCubicBezier(const paths: TPathsD;
  pathIsClosed: Boolean; maxOffset: integer = 0): TPathsD;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := SmoothToCubicBezier(paths[i], pathIsClosed, maxOffset);
end;
//------------------------------------------------------------------------------

function SmoothToCubicBezier2(const path: TPathD;
  pathIsClosed: Boolean; maxOffset: integer): TPathD;
var
  i, j, len, prev: integer;
  vec: TPointD;
  pl: TArrayOfDouble;
  unitVecs: TPathD;
  d1,d2: double;
begin
  // SmoothToCubicBezier2 - returns cubic bezier control points
  Result := nil;
  len := Length(path);
  if len < 3 then Exit;

  NewPointDArray(Result, len *3 +1);
  prev := len-1;
  SetLength(pl, len);
  SetLength(unitVecs, len);
  pl[0] := Distance(path[prev], path[0]);
  unitVecs[0] := GetUnitVector(path[prev], path[0]);
  for i := 0 to len -1 do
  begin
    if i = prev then
    begin
      j := 0;
    end else
    begin
      j := i +1;
      pl[j] := Distance(path[i], path[j]);
      unitVecs[j] := GetUnitVector(path[i], path[j]);
    end;
    vec := GetAvgUnitVector(unitVecs[i], unitVecs[j]);

    d1 := pl[i]/2;
    d2 := pl[j]/2;

    if maxOffset > 0 then
    begin
      d1 := Min(maxOffset, d1);
      d2 := Min(maxOffset, d2);
    end;

    if i = 0 then
      Result[len*3-1] := TranslatePoint(path[0], -vec.X * d1, -vec.Y * d1)
    else
      Result[i*3-1] := TranslatePoint(path[i], -vec.X * d1, -vec.Y * d1);
    Result[i*3] := path[i];
    Result[i*3+1] := TranslatePoint(path[i], vec.X * d2, vec.Y * d2);
  end;
  Result[len*3] := path[0];

  if pathIsClosed then Exit;
  Result[1] := Result[0];
  dec(len);
  Result[len*3-1] := Result[len*3];
  SetLength(Result, Len*3 +1);
end;
//------------------------------------------------------------------------------

function SmoothToCubicBezier2(const paths: TPathsD;
  pathIsClosed: Boolean; maxOffset: integer = 0): TPathsD;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := SmoothToCubicBezier2(paths[i], pathIsClosed, maxOffset);
end;
//------------------------------------------------------------------------------

function CubicInterpolate(v1, v2, v3, v4: double;
  t: double; tension: double = 0): double;
var
   m0, m1, tt, ttt, tensionEx: double;
   a, b: double;
begin
	tt := t * t;
	ttt := tt * t;
  tensionEx := (1-tension) * 0.5;
  m0 := (v3 - v1)*tensionEx;
  m1 := (v4 - v2)*tensionEx;
  a :=  2*v2 - 2*v3 + m0 + m1;
  b :=  3*v3 -3*v2 -2*m0 - m1;
  Result := a*ttt + b*tt + m0*t + v2;
end;
//------------------------------------------------------------------------------

procedure Append(var path: TPathD; const pt: TPointD);
  {$IFDEF INLINE} inline; {$ENDIF}
var
  len: integer;
begin
  len := Length(path);
  SetLengthUninit(path, len +1);
  path[len] := pt;
end;
//------------------------------------------------------------------------------

function SmoothPath(const path: TPathD; isClosedPath: Boolean;
  tension: double; shapeTolerance: double): TPathD;
var
  i, j, highI, len, cnt: integer;
  pt: TPointD;
  dists: TArrayOfDouble;
const
  maxInterval = 1.5;
begin
  Result := nil;
  len := Length(path);
  if len < 3 then Exit;
  SetLength(dists, len);
  highI := len -1;
  dists[highI] := Distance(path[highI], path[0]);
  for i := 0 to highI-1 do
    dists[i] := Distance(path[i], path[i+1]);

  if tension > 1 then tension := 1
  else if tension < -1 then tension := -1;
  if tension > 0.9 then
  begin
    Result := path;
    Exit;
  end;

  if isClosedPath then
    for i := 0 to highI do
    begin
      cnt := Ceil(dists[i]/maxInterval);
      Append(Result, path[i]);
      for j := 1 to cnt -1 do
      begin
        pt.X := CubicInterpolate(
          path[ModEx(i-1, len)].X,
          path[i].X,
          path[ModEx(i+1, len)].X,
          path[ModEx(i+2, len)].X, j/cnt, tension);
        pt.Y := CubicInterpolate(
          path[ModEx(i-1, len)].Y,
          path[i].Y,
          path[ModEx(i+1, len)].Y,
          path[ModEx(i+2, len)].Y, j/cnt, tension);
        Append(Result, pt);
      end;
    end
  else
  begin
    for i := 0 to highI -1 do
    begin
      cnt := Ceil(dists[i]/maxInterval);
      Append(Result, path[i]);
      for j := 1 to cnt -1 do
      begin
        pt.X := CubicInterpolate(
          path[Clamp(i-1, len)].X,
          path[Clamp(i, len)].X,
          path[Clamp(i+1, len)].X,
          path[Clamp(i+2, len)].X, j/cnt, tension);
        pt.Y := CubicInterpolate(
          path[Clamp(i-1, len)].Y,
          path[Clamp(i, len)].Y,
          path[Clamp(i+1, len)].Y,
          path[Clamp(i+2, len)].Y, j/cnt, tension);
        Append(Result, pt);
      end;
    end;
    Append(Result, path[highi]);
  end;
  Result := SimplifyPath(Result, shapeTolerance, false);
end;
//------------------------------------------------------------------------------

function SmoothPaths(const paths: TPathsD; isClosedPath: Boolean;
  tension: double = 0; shapeTolerance: double = 0.1): TPathsD;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := SmoothPath(paths[i], isClosedPath, tension, shapeTolerance);
end;

//------------------------------------------------------------------------------
// GaussianBlur
//------------------------------------------------------------------------------

procedure GaussianBlur(img: TImage32; rec: TRect; radius: Integer);
var
  i, w,h, highX, x,y,yy,z,startz: Integer;
  expConst: double;
  gaussTable: array [-MaxBlur .. MaxBlur] of integer;
  wc: TWeightedColor;
  wca: TArrayOfWeightedColor;
  wcaColor: TArrayOfColor32;
  row: PColor32Array;
  wcRow: PWeightedColorArray;
  imgWidth: Integer;
  dst, pc: PColor32;
const
  tableConst = 1024;
  sigma = 3;
begin
  Types.IntersectRect(rec, rec, img.Bounds);
  if IsEmptyRect(rec) or (radius < 1) then Exit
  else if radius > MaxBlur then radius := MaxBlur;

  expConst := - 1 / (Sqr(radius) * 2 * Sqr(sigma));
  gaussTable[0] := Round(tableConst * Exp(expConst));
  for i := 1 to radius do
  begin
    gaussTable[i] := Round(tableConst * Exp(expConst * Sqr(i)));
    gaussTable[-i] := gaussTable[i];
  end;

  RectWidthHeight(rec, w, h);
  setLength(wca, w * h);
  NewColor32Array(wcaColor, w * h, True);
  imgWidth := img.Width;
  highX := imgWidth -1;
  for y := 0 to h -1 do
  begin
    row := PColor32Array(@img.Pixels[(y + rec.Top) * imgWidth + rec.Left]);
    wcRow := PWeightedColorArray(@wca[y * w]);
    for x := 0 to w -1 do
      for z := max(0, x - radius) to min(highX, x + radius) do
        wcRow[x].Add(row[z], gaussTable[x-z]);
  end;

  // calculate colors
  for x := 0 to w * h - 1 do
    wcaColor[x] := wca[x].Color;

  dst := @img.Pixels[rec.Left + rec.Top * imgWidth];
  imgWidth := imgWidth * SizeOf(TColor32); // convert to byte size
  for x := 0 to w -1 do
  begin
    pc := dst;
    inc(pc, x);
    for y := 0 to h -1 do
    begin
      wc.Reset;
      startz := max(0, y - radius);
      yy := startz * w;
      for z := startz to min(h -1, y + radius) do
      begin
        wc.Add(wcaColor[x + yy], gaussTable[y-z]);
        inc(yy, w);
      end;
      pc^ := wc.Color;
      inc(PByte(pc), imgWidth); // increment by byte size
    end;
  end;
end;

//------------------------------------------------------------------------------
// FastGaussian blur - and support functions
//------------------------------------------------------------------------------

//http://blog.ivank.net/fastest-gaussian-blur.html
//https://www.peterkovesi.com/papers/FastGaussianSmoothing.pdf

function BoxesForGauss(stdDev, boxCnt: integer): TArrayOfInteger;
var
  i, wl, wu, m: integer;
  wIdeal, mIdeal: double;
begin
  NewIntegerArray(Result, boxCnt, True);
  wIdeal := Sqrt((12*stdDev*stdDev/boxCnt)+1); // Ideal averaging filter width
  wl := Floor(wIdeal); if not Odd(wl) then dec(wl);
  mIdeal :=
    (-3*stdDev*stdDev +0.25*boxCnt*wl*wl +boxCnt*wl +0.75*boxCnt)/(wl+1);
  m := Floor(mIdeal) div 2;   // nb: variation on Ivan Kutskir's code.
  wl := (wl -1) div 2;        //    It's better to do this here
  wu := wl+1;                 //    than later in both BoxBlurH & BoxBlurV
  for i := 0 to boxCnt -1 do
    if i < m then
      Result[i] := wl else
      Result[i] := wu;
end;
//------------------------------------------------------------------------------

procedure FastGaussianBlur(img: TImage32;
  const rec: TRect; stdDev: integer; repeats: integer);
begin
  FastGaussianBlur(img, rec, stdDev, stdDev, repeats);
end;
//------------------------------------------------------------------------------

procedure BoxBlurHLine(src, dst: PColor32; srcRiOffset: nativeint;
  count, w: integer; dstLast: PColor32; var v: TWeightedColor);
var
  lastColor: TColor32;
  val: PWeightedColor;
  s, d: PColor32;
begin
  lastColor := v.Color;
  if count > w then
    count := w;
  w := w - count;

  // The Delphi compiler sometimes is really stupid with
  // the CPU register allocation. With this, even if no actual
  // code is produced, the compiler happens to make better
  // decisions.
  val := @v;
  s := src;
  d := dst;

  if count > 0 then
  begin
    while count > 0 do
    begin
      if val.AddSubtract(PColor32Array(s)[srcRiOffset], s^) then
        lastColor := val.Color;
      inc(s);
      d^ := lastColor;
      inc(d);
      dec(count);
    end;

    count := w;
    while count > 0 do
    begin
      d^ := lastColor;
      inc(d);
      dec(count);
    end;
  end;

  while PByte(d) <= PByte(dstLast) do
  begin
    if val.AddNoneSubtract(s^) then
      lastColor := val.Color;
    inc(s);
    d^ := lastColor;
    inc(d);
  end;
end;
//------------------------------------------------------------------------------

procedure BoxBlurH(const src, dst: TArrayOfColor32; w,h, stdDev: integer);
var
  i,j, ti, li, ri, re, ovr: integer;
  fv, val: TWeightedColor;
  lastColor: TColor32;
  stdDevW: integer;
begin
  ovr := Max(0, stdDev - w);
  for i := 0 to h -1 do
  begin
    ti := i * w;
    li := ti;
    ri := ti +stdDev;
    re := ti +w -1; // idx of last pixel in row
    fv.Reset(src[ti]);
    val.Reset(src[ti], stdDev +1);
    for j := 0 to stdDev -1 - ovr do
      val.Add(src[ti + j]);
    if ovr > 0 then val.Add(clNone32, ovr);
    for j := 0 to stdDev do
    begin
      if ri <= re then
        val.Add(src[ri]) else
        val.Add(src[re]); // color of last pixel in row
      inc(ri);
      val.Subtract(fv);
      if ti <= re then
        dst[ti] := val.Color;
      inc(ti);
    end;

    // Skip "val.Color" calculation if both for-loops are skipped anyway
    stdDevW := w - stdDev*2 - 1;
    if (ti <= re) or (stdDevW > 0) then
    begin
      if w > 4 then // prevent the call-overhead if it would be slower than the inline version
        BoxBlurHLine(@src[li], @dst[ti], ri - li, re - ri + 1, stdDevW, @dst[re], val)
      else
      begin
        lastColor := val.Color;
        for j := stdDevW downto 1 do
        begin
          if ri <= re then
          begin
            if val.AddSubtract(src[ri], src[li]) then
              lastColor := val.Color;
            inc(ri);
            inc(li);
          end;
          dst[ti] := lastColor;
          inc(ti);
        end;
        while ti <= re do
        begin
          if val.AddNoneSubtract(src[li]) then
            lastColor := val.Color;
          inc(li);
          dst[ti] := lastColor;
          inc(ti);
        end;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure BoxBlurVLine(src, dst: PColor32; srcRiOffset: nativeint;
  widthBytes, count, h: integer; dstLast: PColor32; var v: TWeightedColor);
var
  lastColor: TColor32;
  val: PWeightedColor;
  s, d: PColor32;
begin
  lastColor := v.Color;
  if count > h then
    count := h;
  h := h - count;

  // The Delphi compiler sometimes is really stupid with
  // the CPU register allocation. With this, even if no actual
  // code is produced, the compiler happens to make better
  // decisions.
  val := @v;
  s := src;
  d := dst;

  if count > 0 then
  begin
    while count > 0 do
    begin
      if val.AddSubtract(PColor32Array(s)[srcRiOffset], s^) then
        lastColor := val.Color;
      inc(PByte(s), widthBytes);
      d^ := lastColor;
      inc(PByte(d), widthBytes);
      dec(count);
    end;

    count := h;
    while count > 0 do
    begin
      d^ := lastColor;
      inc(PByte(d), widthBytes);
      dec(count);
    end;
  end;

  while PByte(d) <= PByte(dstLast) do
  begin
    if val.AddNoneSubtract(s^) then
      lastColor := val.Color;
    inc(PByte(s), widthBytes);
    d^ := lastColor;
    inc(PByte(d), widthBytes);
  end;
end;
//------------------------------------------------------------------------------

procedure BoxBlurV(const src, dst: TArrayOfColor32; w, h, stdDev: integer);
var
  i,j, ti, li, ri, re, ovr: integer;
  fv, val: TWeightedColor;
  lastColor: TColor32;
  stdDevH: integer;
begin
  ovr := Max(0, stdDev - h);
  for i := 0 to w -1 do
  begin
    ti := i;
    li := ti;
    ri := ti + stdDev * w;
    re := ti +w *(h-1); // idx of last pixel in column
    fv.Reset(src[ti]);
    val.Reset(src[ti], stdDev +1);
    for j := 0 to stdDev -1 -ovr do
      val.Add(src[ti + j *w]);
    if ovr > 0 then val.Add(clNone32, ovr);
    for j := 0 to stdDev do
    begin
      if ri <= re then
        val.Add(src[ri]) else
        val.Add(src[re]); // color of last pixel in column
      inc(ri, w);
      val.Subtract(fv);
      if ti <= re then
        dst[ti] := val.Color;
      inc(ti, w);
    end;

    // Skip "val.Color" calculation if both for-loops are skipped anyway
    stdDevH := h - stdDev*2 - 1;
    if (ti <= re) or (stdDevH > 0) then
    begin
      if stdDevH > 4 then // prevent the call-overhead if it would be slower than the inline version
        BoxBlurVLine(@src[li], @dst[ti], ri - li, w * SizeOf(TColor32), re - ri + 1, stdDevH, @dst[re], val)
      else
      begin
        lastColor := val.Color;
        for j := stdDevH downto 1 do
        begin
          if ri <= re then
          begin
            if val.AddSubtract(src[ri], src[li]) then
              lastColor := val.Color;
            inc(ri, w);
            inc(li, w);
          end;

          dst[ti] := lastColor;
          inc(ti, w);
        end;
        while ti <= re do
        begin
          if val.AddNoneSubtract(src[li]) then
            lastColor := val.Color;
          inc(li, w);
          dst[ti] := lastColor;
          inc(ti, w);
        end;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure FastGaussianBlur(img: TImage32;
  const rec: TRect; stdDevX, stdDevY: integer; repeats: integer);
var
  i,j,len, w,h: integer;
  rec2: TRect;
  boxesH: TArrayOfInteger;
  boxesV: TArrayOfInteger;
  src, dst: TArrayOfColor32;
  blurFullImage: Boolean;
  pSrc, pDst: PColor32;
begin
  if not Assigned(img) then Exit;
  Types.IntersectRect(rec2, rec, img.Bounds);
  if IsEmptyRect(rec2) then Exit;
  blurFullImage := RectsEqual(rec2, img.Bounds);
  RectWidthHeight(rec2, w, h);
  if (Min(w, h) < 2) or ((stdDevX < 1) and (stdDevY < 1)) then Exit;
  len := w * h;
  NewColor32Array(src, len, True); // content is overwritten in BoxBlurH
  if blurFullImage then
  begin
    // Use the img.Pixels directly instead of copying the entire image into 'dst'.
    // The first thing the code does is BoxBlurH({source:=}dst, {dest:=}src, ...).
    dst := img.Pixels;
  end
  else
  begin
    // copy a rectangular region into 'dst'
    NewColor32Array(dst, len, True);
    pSrc := img.PixelRow[rec2.Top];
    inc(pSrc, rec2.Left);
    pDst := @dst[0];
    for i := 0 to h -1 do
    begin
      Move(pSrc^, pDst^, w * SizeOf(TColor32));
      inc(pSrc, img.Width);
      inc(pDst, w);
    end;
  end;

  // do the blur
  inc(repeats); // now represents total iterations
  boxesH := BoxesForGauss(stdDevX, repeats);
  if stdDevY = stdDevX then
    boxesV := boxesH else
    boxesV := BoxesForGauss(stdDevY, repeats);

  img.BeginUpdate;
  try
    for j := 0 to repeats -1 do
    begin
      BoxBlurH(dst, src, w, h, boxesH[j]);
      BoxBlurV(src, dst, w, h, boxesV[j]);
    end;

    if not blurFullImage then
    begin
      // copy dst array back to image rect
      pDst := img.PixelRow[rec2.Top];
      inc(pDst, rec2.Left);
      pSrc := @dst[0];
      for i := 0 to h -1 do
      begin
        Move(pSrc^, pDst^, w * SizeOf(TColor32));
        inc(pSrc, w);
        inc(pDst, img.Width);
      end;
    end;
  finally
    img.EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

end.
