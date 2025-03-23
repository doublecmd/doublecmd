unit Img32.Resamplers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.8                                                             *
* Date      :  10 January 2025                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  For image transformations (scaling, rotating etc.)              *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Img32;

// Premultiplies the alpha channel into the color channels from pSrc and stores
// it into pDst. pSrc and pDst can be the same pointer.
procedure PremultiplyAlpha(pSrc, pDst: PARGB; count: nativeint); overload;

// BoxDownSampling: As the name implies, is only intended for image
// down-sampling (ie shrinking images) where it performs a little better
// than other resamplers which tend toward pixelation. Nevertheless, this
// routine is inferior to other resamplers when performing other
// types of transformations (ie when enlarging, rotating, and skewing images),
// so BoxDownSampling should not be used as a general purpose resampler.
procedure BoxDownSampling(Image: TImage32; scale: double); overload;
procedure BoxDownSampling(Image: TImage32; scaleX, scaleY: double); overload;
procedure BoxDownSampling(Image: TImage32; newWidth, newHeight: Integer); overload;
procedure BoxDownSampling(Image, TargetImage: TImage32; scale: double); overload;
procedure BoxDownSampling(Image, TargetImage: TImage32; scaleX, scaleY: double); overload;
procedure BoxDownSampling(Image, TargetImage: TImage32; newWidth, newHeight: Integer); overload;

procedure NearestNeighborResize(Image: TImage32; newWidth, newHeight: Integer); overload;
procedure NearestNeighborResize(Image, TargetImage: TImage32; newWidth, newHeight: Integer); overload;
procedure ResamplerResize(Image: TImage32; newWidth, newHeight: Integer); overload;
procedure ResamplerResize(Image, TargetImage: TImage32; newWidth, newHeight: Integer); overload;

// The following general purpose resamplers are registered below:
// function NearestResampler(img: TImage32; x, y: double): TColor32;
// function BilinearResample(img: TImage32; x, y: double): TColor32;
// function BicubicResample (img: TImage32; x, y: double): TColor32;
// function WeightedBilinear(img: TImage32; x, y: double): TColor32;

implementation

uses
  Img32.Transform;

var
  sinWeighted: array [0..255] of Cardinal;

//------------------------------------------------------------------------------
// NearestNeighbor resampler
//------------------------------------------------------------------------------

function NearestResampler(img: TImage32; x, y: double): TColor32;
var
  xi, yi: integer;
begin
  xi := Round(x); yi := Round(y);
  if (xi < 0) or (yi < 0) or (xi >= img.Width) or (yi >= img.Height) then
    Result := clNone32 else
    Result := img.Pixels[xi + yi * img.Width];
end;

//------------------------------------------------------------------------------
// BiLinear resampler
//------------------------------------------------------------------------------

function BilinearResample(img: TImage32; x, y: double): TColor32;
var
  iw, ih: integer;
  xx, yy, xR, yB: integer;
  weight: integer;
  pixels: TArrayOfColor32;
  weightedColor: TWeightedColor;
  xf, yf: double;
begin
  iw := img.Width;
  ih := img.Height;
  pixels := img.Pixels;

  if (x < 0) then
  begin
    if (x < -0.5) then
    begin
      xf := -x;
    end else
    begin
      x := 0;
      xf := 0;
    end;
    xx := 0;
    xR := 0;
  end else
  begin
    xf := 1-frac(x);
    if x >= iw -1 then
    begin
      xx := iw -1;
      xR := xx;
    end else
    begin
      xx := Trunc(x);
      xR := xx +1;
    end;
  end;

  if (y < 0) then
  begin
    if (y < -0.5) then
    begin
      yf := -y;
    end else
    begin
      y := 0;
      yf := 0;
    end;
    yy := 0;
    yB := 0;
  end else
  begin
    yf := 1-frac(y);
    if y >= ih -1 then
    begin
      yy := ih -1;
      yB := yy;
    end else
    begin
      yy := Trunc(y);
      yB := yy +1;
    end;
  end;

  weightedColor.Reset;

  weight := Round(xf * yf * 255);      //top-left
  if weight > 0 then
  begin
    if (x < 0) or (y < 0) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xx + yy * iw], weight);
  end;

  weight := Round((1-xf) * yf * 255);         //top-right
  if weight > 0 then
  begin
    if (x > iw - 0.5) or (y < 0) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xR + yy * iw], weight);
  end;

  weight := Round(xf * (1-yf) * 255);          //bottom-left
  if weight > 0 then
  begin
    if (x < 0) or (y > ih - 0.5) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xx + yB * iw], weight);
  end;

  weight := Round((1-xf) * (1-yf) * 255);              //bottom-right
  if weight > 0 then
  begin
    if (x > iw - 0.5) or (y > ih - 0.5) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xR + yB * iw], weight);
  end;
  Result := weightedColor.Color;
end;
//------------------------------------------------------------------------------

// WeightedBilinearResample: A modified bilinear resampler that's
// less blurry but also a little more pixelated.
function WeightedBilinearResample(img: TImage32; x, y: double): TColor32;
var
  iw, ih: integer;
  xx, yy, xR, yB: integer;
  weight: integer;
  pixels: TArrayOfColor32;
  weightedColor: TWeightedColor;
  xf, yf: double;
begin
  iw := img.Width;
  ih := img.Height;
  pixels := img.Pixels;

  if (x < 0) then
  begin
    if (x < -0.5) then
    begin
      xf := -x;
    end else
    begin
      x := 0;
      xf := 0;
    end;
    xx := 0;
    xR := 0;
  end else
  begin
    xf := 1-frac(x);
    if x >= iw -1 then
    begin
      xx := iw -1;
      xR := xx;
    end else
    begin
      xx := Trunc(x);
      xR := xx +1;
    end;
  end;

  if (y < 0) then
  begin
    if (y < -0.5) then
    begin
      yf := -y;
    end else
    begin
      y := 0;
      yf := 0;
    end;
    yy := 0;
    yB := 0;
  end else
  begin
    yf := 1-frac(y);
    if y >= ih -1 then
    begin
      yy := ih -1;
      yB := yy;
    end else
    begin
      yy := Trunc(y);
      yB := yy +1;
    end;
  end;

  weightedColor.Reset;

  weight := sinWeighted[Round(xf * yf * 255)];      //top-left
  if weight > 0 then
  begin
    if (x < 0) or (y < 0) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xx + yy * iw], weight);
  end;

  weight := sinWeighted[Round((1-xf) * yf * 255)];        //top-right
  if weight > 0 then
  begin
    if (x > iw - 0.5) or (y < 0) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xR + yy * iw], weight);
  end;

  weight := sinWeighted[Round(xf * (1-yf) * 255)];          //bottom-left
  if weight > 0 then
  begin
    if (x < 0) or (y > ih - 0.5) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xx + yB * iw], weight);
  end;

  weight := sinWeighted[Round((1-xf) * (1-yf) * 255)];              //bottom-right
  if weight > 0 then
  begin
    if (x > iw - 0.5) or (y > ih - 0.5) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xR + yB * iw], weight);
  end;
  Result := weightedColor.Color;
end;

//------------------------------------------------------------------------------
// BiCubic resampler
//------------------------------------------------------------------------------

type
  TBiCubicEdgeAdjust = (eaCenterFill,
    eaPreStart, eaStart, eaPostStart, eaEnd, eaPostEnd);

var
  byteFrac: array [0..255] of double;
  byteFracSq: array [0..255] of double;
  byteFracCubed: array [0..255] of double;

//------------------------------------------------------------------------------

function CubicInterpolate(aclr: PColor32;
  t: Byte; bce: TBiCubicEdgeAdjust): TColor32;
var
  a,b,c,d: PARGB;
  q: TARGB;
	aa, bb, m0, m1: double;
  t1, t2, t3: double;
  res: TARGB absolute Result;
const
  clTrans: TColor32 = clNone32;
begin
  case bce of
    eaPreStart:
      begin
        a := @clTrans;
        b := @clTrans;
        c := PARGB(aclr);
        d := c;
      end;
    eaStart:
      begin
        Result := aclr^;
        Exit;
      end;
    eaPostStart:
      begin
        a := PARGB(aclr);
        b := a;
        Inc(aclr);
        c := PARGB(aclr);
        d := c;
      end;
    eaEnd:
      begin
        Inc(aclr);
        Result := aclr^;
        Exit;
      end;
    eaPostEnd:
      begin
        Inc(aclr);
        a := PARGB(aclr);
        b := a;
        c := @clTrans;
        d := @clTrans;
      end;
    else
      begin
        a := PARGB(aclr);
        Inc(aclr);
        b := PARGB(aclr);
        Inc(aclr);
        c := PARGB(aclr);
        Inc(aclr);
        d := PARGB(aclr);
      end;
  end;

  if (b.A = 0) and (c.A = 0) then
  begin
    result := clNone32;
    Exit;
  end
  else if (b = c) then
  begin
    result := b.Color;
    Exit;
  end
  else if b.A = 0 then
  begin
    // ignore differences between b & c's color channels
    q := c^;
    q.A := 0;
    b := @q;
  end;
  if c.A = 0 then
  begin
    // ignore differences between b & c's color channels
    q := b^;
    q.A := 0;
    c := @q;
  end;

  t1 := byteFrac[t];
  t2 := byteFracSq[t];
  t3 := byteFracCubed[t];

  // find piecewise bicubic interpolation between pixel_b and pixel_c
  // at point 't' (as byte div 255) ...
  // given parametric equation aa(t^3) + bb(t^2) + cc(t)+ dd = 0
  // where t(0) = pixel_b and t(1) = pixel_c
  // let m1 = slope at pixel_b (using slope of pixel_c - pixel_a)
  // let m2 = slope at pixel_c (using slope of pixel_d - pixel_b)
  // then t(0) = aa(0^3) + bb(0^2) + cc(0) + dd = dd
  // then t(1) = aa(1^3) + bb(1^2) + cc(1) + dd = aa + bb + cc + dd
  // differentiating parametic equation at t'(0) and t'(1) ...
  // t'(0) = m0 = 3*aa(0^2) + 2*bb(0) + cc = cc
  // t'(1) = m1 = 3*aa(1^2) + 2*bb(1) + cc = 3*aa + 2*bb + cc
  // t(0)  = dd                 ::EQ1
  // t(1)  = aa+bb+cc+dd        ::EQ2
  // t'(0) = cc                 ::EQ3
  // t'(1) = 3*aa + 2*bb + cc   ::EQ4
  // solving simultaneous equations
  // aa = 2*t(0) -2*t(1) +t'(0)   +t'(1)
  // bb = 3*t(1) -3*t(0) -2*t'(0) -t'(1)
  // cc = m0
  // dd = t(0)

  m0 {aka t'(0)} := (c.A - a.A) /2;
  m1 {aka t'(1)} := (d.A - b.A) /2;
  aa := 2*b.A - 2*c.A + m0 + m1;
  bb := 3*c.A -3*b.A -2*m0 - m1;
  Res.A := ClampByte(aa*t3 + bb*t2 + m0*t1 + b.A);

  m0 := (c.R - a.R) /2;
  m1 := (d.R - b.R) /2;
  aa := 2*b.R - 2*c.R + m0 + m1;
  bb := 3*c.R -3*b.R -2*m0 - m1;
  Res.R := ClampByte(aa*t3 + bb*t2 + m0*t1 + b.R);

  m0 := (c.G - a.G) /2;
  m1 := (d.G - b.G) /2;
  aa := 2*b.G - 2*c.G + m0 + m1;
  bb := 3*c.G -3*b.G -2*m0 - m1;
  Res.G := ClampByte(aa*t3 + bb*t2 + m0*t1 + b.G);

  m0 := (c.B - a.B) /2;
  m1 := (d.B - b.B) /2;
  aa := 2*b.B - 2*c.B + m0 + m1;
  bb := 3*c.B -3*b.B -2*m0 - m1;
  Res.B := ClampByte(aa*t3 + bb*t2 + m0*t1 + b.B);
end;
//------------------------------------------------------------------------------

function BicubicResample(img: TImage32; x, y: double): TColor32;
var
  i, pi, iw, ih, last: Integer;
  c: array[0..3] of TColor32;
  xFrac, yFrac: byte;
  bceX, bceY: TBiCubicEdgeAdjust;
begin
  iw := img.Width;
  ih := img.Height;
  last := iw * ih -1;

  if x < 1 then
  begin
    if x < -0.5 then
    begin
      xFrac := Round((1+x) *255);
      bceX := eaPreStart;
    end
    else if (x < 0) or
      ((iw = 1) and (x < 0.5)) then
    begin
      x := 0;
      xFrac := 0;
      bceX := eaStart;
    end
    else if (iw = 1) and (x > 0.5) then
    begin
      // the following is a workaround to avoid the increment in eaPostEnd
      bceX := eaPreStart;         // ie anti-aliase but without increment
      xFrac := Round((1-x) *127); // reversed because 'end' not 'start'
    end else
    begin
      xFrac := Round(frac(x) *255);
      bceX := eaPostStart;
    end;
  end else
  begin
    xFrac := Round(frac(x) *255);
    if x > iw - 1 then
    begin
      if x > iw - 0.5 then bceX := eaPostEnd
      else bceX := eaEnd
    end
    else
      bceX := eaCenterFill;
  end;

  if y < 1 then
  begin
    if y < -0.5 then
    begin
      yFrac := Round((1+y) *255);
      bceY := eaPreStart;
    end
    else if (y < 0) or
      ((ih = 1) and (y < 0.5)) then
    begin
      y := 0;
      yFrac := 0;
      bceY := eaStart;
    end
    else if (ih = 1) and (y > 0.5) then
    begin
      // the following is a workaround to avoid the increment in eaPostEnd
      bceY := eaPreStart;         // ie anti-aliase but without increment
      yFrac := Round((1-y) *127); // reversed because 'end' not 'start'
    end else
    begin
      yFrac := Round(frac(y) *255);
      bceY := eaPostStart;
    end;
  end else
  begin
    yFrac := Round(frac(y) *255);
    if y > ih - 1 then
    begin
      if y > ih - 0.5 then bceY := eaPostEnd
      else bceY := eaEnd
    end
    else
      bceY := eaCenterFill;
  end;

  x := Max(0, Min(iw -1, x -1));
  y := Max(0, Min(ih -1, y -1));
  pi := Trunc(y) * iw + Trunc(x);

  for i := 0 to 3 do
  begin
    c[i] := CubicInterpolate(@img.Pixels[pi], xFrac, bceX);
    inc(pi, iw);
    if pi > last then break;
  end;
  Result := CubicInterpolate(@c[0], yFrac, bceY);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{$RANGECHECKS OFF} // negative index usage for Delphi 7-2007
procedure PremultiplyAlpha(pSrc, pDst: PARGB; count: nativeint);
var
  a: byte;
  tab: PByteArray;
  c: TColor32;
  s, d: PColor32Array;
begin
  if count = 0 then exit;

  // Use negative index trick
  inc(pSrc, count);
  inc(pDst, count);
  count := -count;

  // This function is optmized with the assumption that if a pixel has a certain
  // alpha channel, then the propability that the following pixels have the same
  // alpha channel, is very high.

  c := PColor32Array(pSrc)[count];
  a := c shr 24;
  while True do
  begin
    case a of
      0: // Special handling for 0 => color becomes black
        begin
          // Win32: Load stack variable into CPU register
          s := PColor32Array(pSrc);
          d := PColor32Array(pDst);
          while True do
          begin
            d[count] := 0;
            inc(count);
            if count = 0 then exit;
            c := s[count];
            a := c shr 24;
            if a <> 0 then break;
          end;
        end;

      255: // Special handling for 255 => no color change
        begin
          // Win32: Load stack variable into CPU register
          s := PColor32Array(pSrc);
          d := PColor32Array(pDst);
          if s = d then // if source=dest, we can skip writing to d
          begin
            while True do
            begin
              //d[count] := c; // skip the write
              inc(count);
              if count = 0 then exit;
              c := s[count];
              a := c shr 24;
              if a <> 255 then break;
            end;
          end
          else
          begin
            while True do
            begin
              d[count] := c;
              inc(count);
              if count = 0 then exit;
              c := s[count];
              a := c shr 24;
              if a <> 255 then break;
            end;
          end;
        end;

    else
      // Premultiply the alpha channel

      // Win32: Load stack variable into CPU register
      s := PColor32Array(pSrc);
      // Win32: This line "breaks" Delphi's register allocator
      //d := PColor32Array(pDst);
      while True do
      begin
        tab := @MulTable[a];
        c := (c and $FF000000) or
             (tab[Byte(c shr 16)] shl 16) or
             (tab[Byte(c shr  8)] shl  8) or
             (tab[Byte(c       )]       );
        //d[count] := c;
        PColor32Array(pDst)[count] := c;
        inc(count);
        if count = 0 then exit;
        c := s[count];
        a := c shr 24;
        if (a = 0) or (a = 255) then break;
      end;
    end;
  end;
end;
{$IFDEF RANGECHECKS_ENABLED}
{$RANGECHECKS ON}
{$ENDIF RANGECHECKS_ENABLED}

//------------------------------------------------------------------------------
// BoxDownSampling and related functions
//------------------------------------------------------------------------------

function GetWeightedColor(const srcBits: TArrayOfColor32;
  x256, y256, xx256, yy256, maxX: Integer): TColor32;
var
  i, j, xi, yi, xxi, yyi, weight: Integer;
  xf, yf, xxf, yyf: cardinal;
  color: TWeightedColor;
begin
  //This function performs 'box sampling' and differs from GetWeightedPixel
  //(bilinear resampling) in one important aspect - it accommodates weighting
  //any number of pixels (rather than just adjacent pixels) and this produces
  //better image quality when significantly downsizing.

  //Note: there's no range checking here, so the precondition is that the
  //supplied boundary values are within the bounds of the srcBits array.

  color.Reset;

  xi := x256 shr 8; xf := x256 and $FF;
  yi := y256 shr 8; yf := y256 and $FF;
  xxi := xx256 shr 8; xxf := xx256 and $FF;
  yyi := yy256 shr 8; yyf := yy256 and $FF;

  //1. average the corners ...
  weight := (($100 - xf) * ($100 - yf)) shr 8;
  color.Add(srcBits[xi + yi * maxX], weight);
  weight := (xxf * ($100 - yf)) shr 8;
  if (weight <> 0) then color.Add(srcBits[xxi + yi * maxX], weight);
  weight := (($100 - xf) * yyf) shr 8;
  if (weight <> 0) then color.Add(srcBits[xi + yyi * maxX], weight);
  weight := (xxf * yyf) shr 8;
  if (weight <> 0) then color.Add(srcBits[xxi + yyi * maxX], weight);

  //2. average the edges
  if (yi +1 < yyi) then
  begin
    xf := $100 - xf;
    for i := yi + 1 to yyi - 1 do
      color.Add(srcBits[xi + i * maxX], xf);
    if (xxf <> 0) then
      for i := yi + 1 to yyi - 1 do
        color.Add(srcBits[xxi + i * maxX], xxf);
  end;
  if (xi + 1 < xxi) then
  begin
    yf := $100 - yf;
    for i := xi + 1 to xxi - 1 do
      color.Add(srcBits[i + yi * maxX], yf);
    if (yyf <> 0) then
      for i := xi + 1 to xxi - 1 do
        color.Add(srcBits[i + yyi * maxX], yyf);
  end;

  //3. average the non-fractional pixel 'internals' ...
  for i := xi + 1 to xxi - 1 do
    for j := yi + 1 to yyi - 1 do
      color.Add(srcBits[i + j * maxX], $100);

  //4. finally get the weighted color ...
  if color.AddCount = 0 then
    Result := srcBits[xi + yi * maxX] else
    Result := color.Color;
end;
//------------------------------------------------------------------------------

procedure BoxDownSampling(Image: TImage32; scaleX, scaleY: double);
begin
  BoxDownSampling(Image, Image, scaleX, scaleY);
end;
//------------------------------------------------------------------------------

procedure BoxDownSampling(Image: TImage32; scale: double);
begin
  BoxDownSampling(Image, Image, scale);
end;
//------------------------------------------------------------------------------

procedure BoxDownSampling(Image: TImage32; newWidth, newHeight: Integer);
begin
  BoxDownSampling(Image, Image, newWidth, newHeight);
end;
//------------------------------------------------------------------------------

procedure BoxDownSampling(Image, TargetImage: TImage32; scaleX, scaleY: double);
begin
  BoxDownSampling(Image, TargetImage,
    Max(1, Integer(Round(Image.Width * scaleX))),
    Max(1, Integer(Round(Image.Height * scaleY))));
end;
//------------------------------------------------------------------------------

procedure BoxDownSampling(Image, TargetImage: TImage32; scale: double);
begin
  BoxDownSampling(Image, TargetImage,
    Max(1, Integer(Round(Image.Width * scale))),
    Max(1, Integer(Round(Image.Height * scale))));
end;
//------------------------------------------------------------------------------

procedure BoxDownSampling(Image, TargetImage: TImage32; newWidth, newHeight: Integer);
var
  x,y, x256,y256,xx256,yy256: Integer;
  sx,sy: double;
  tmp: TArrayOfColor32;
  pc: PColor32;
  scaledX: TArrayOfInteger;
begin
  sx := Image.Width/newWidth * 256;
  sy := Image.Height/newHeight * 256;
  NewColor32Array(tmp, newWidth * newHeight, True);

  NewIntegerArray(scaledX, newWidth, True);
  for x := 0 to newWidth -1 do
    scaledX[x] := Round((x+1) * sx);

  y256 := 0;
  pc := @tmp[0];
  for y := 0 to newHeight - 1 do
  begin
    x256 := 0;
    yy256 := Round((y+1) * sy);
    for x := 0 to newWidth - 1 do
    begin
      xx256 := scaledX[x];
      pc^ := GetWeightedColor(Image.Pixels,
        x256, y256, xx256, yy256, Image.Width);
      x256 := xx256;
      inc(pc);
    end;
    y256 := yy256;
  end;

  TargetImage.AssignPixelArray(tmp, newWidth, newHeight);
end;
//------------------------------------------------------------------------------

procedure NearestNeighborResize(Image: TImage32; newWidth, newHeight: Integer);
begin
  NearestNeighborResize(Image, Image, newWidth, newHeight);
end;
//------------------------------------------------------------------------------

procedure NearestNeighborResize(Image, TargetImage: TImage32; newWidth, newHeight: Integer);
var
  x, y, offset: Integer;
  scaledXi, scaledYiOffset: TArrayOfInteger;
  tmp: TArrayOfColor32;
  pc: PColor32;
  pixels: TArrayOfColor32;
begin
  //this NearestNeighbor code is slightly more efficient than
  //the more general purpose one in Img32.Resamplers

  if (newWidth = Image.Width) and (newHeight = Image.Height) then
  begin
    if TargetImage <> Image then TargetImage.Assign(Image);
    Exit;
  end;
  NewColor32Array(tmp, newWidth * newHeight, True);

  //get scaled X & Y values once only (storing them in lookup arrays) ...
  NewIntegerArray(scaledXi, newWidth, True);
  for x := 0 to newWidth -1 do
    scaledXi[x] := (x * Image.Width) div newWidth;
  NewIntegerArray(scaledYiOffset, newHeight, True);
  SetLength(scaledYiOffset, newHeight);
  for y := 0 to newHeight -1 do
    //scaledYiOffset[y] := Round(y * Image.Height / newHeight) * Image.Width;
    scaledYiOffset[y] := ((y * Image.Height) div newHeight) * Image.Width;

  pc := @tmp[0];
  pixels := Image.Pixels;
  for y := 0 to newHeight - 1 do
  begin
    offset := scaledYiOffset[y];
    for x := 0 to newWidth - 1 do
    begin
      pc^ := pixels[scaledXi[x] + offset];
      inc(pc);
    end;
  end;

  TargetImage.AssignPixelArray(tmp, newWidth, newHeight);
end;
//------------------------------------------------------------------------------

procedure ResamplerResize(Image: TImage32; newWidth, newHeight: Integer);
begin
  ResamplerResize(Image, Image, newWidth, newHeight);
end;
//------------------------------------------------------------------------------

procedure ResamplerResize(Image, TargetImage: TImage32; newWidth, newHeight: Integer);
var
  mat: TMatrixD;
begin
  mat := IdentityMatrix;
  MatrixScale(mat, newWidth/Image.Width, newHeight/Image.Height);
  AffineTransformImage(Image, TargetImage, mat);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure InitByteExponents;
var
  i: integer;
const
  inv255     : double = 1/255;
  inv255sqrd : double = 1/(255*255);
  inv255cubed: double = 1/(255*255*255);
  piDiv256   : double = Pi / 256;
begin
  for i := 0 to 255 do
  begin
    byteFrac[i]  := i     *inv255;
    byteFracSq[i]  := i*i   *inv255sqrd;
    byteFracCubed[i] := i*i*i *inv255cubed;

    sinWeighted[i] := Round((Sin(i * piDiv256 - Pi/2) +1) /2 * 255);
  end;
end;
//------------------------------------------------------------------------------

initialization
  InitByteExponents;

  rNearestResampler  := RegisterResampler(NearestResampler, 'NearestNeighbor');
  rBilinearResampler := RegisterResampler(BilinearResample, 'Bilinear');
  rBicubicResampler  := RegisterResampler(BicubicResample, 'HermiteBicubic');
  rWeightedBilinear  := RegisterResampler(WeightedBilinearResample, 'WeightedBilinear');
  DefaultResampler   := rBilinearResampler;

end.
