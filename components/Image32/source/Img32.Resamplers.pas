unit Img32.Resamplers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.3                                                             *
* Date      :  27 September 2022                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  For image transformations (scaling, rotating etc.)              *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Img32;

//BoxDownSampling: As the name implies, this routine is only intended for
//image down-sampling (ie when shrinking images) where it generally performs
//better than other resamplers which tend to lose too much detail. However,
//because this routine is inferior to other resamplers when performing other
//transformations (ie when enlarging, rotating, and skewing images), it's not
//intended as a general purpose resampler.
procedure BoxDownSampling(Image: TImage32; newWidth, newHeight: Integer);

(* The following functions are registered in the initialization section below
function NearestResampler(img: TImage32; x256, y256: Integer): TColor32;
function BilinearResample(img: TImage32; x256, y256: Integer): TColor32;
function BicubicResample(img: TImage32; x256, y256: Integer): TColor32;
*)

implementation

uses
  Img32.Transform;

//------------------------------------------------------------------------------
// NearestNeighbor resampler
//------------------------------------------------------------------------------

function NearestResampler(img: TImage32; x256, y256: Integer): TColor32;
begin
  if (x256 < -$7f) then
  begin
    Result := clNone32;
    Exit;
  end;

  if (y256 < -$7f) then
  begin
    Result := clNone32;
    Exit;
  end;

  if (x256 and $FF > $7F) then inc(x256, $100);
  x256 := x256 shr 8;
  if y256 and $FF > $7F then inc(y256, $100);
  y256 := y256 shr 8;

  if (x256 < 0) or (x256 >= img.Width) or
    (y256 < 0) or (y256 >= img.Height) then
      Result := clNone32 else
      Result := img.Pixels[y256 * img.Width + x256];
end;

//------------------------------------------------------------------------------
// BiLinear resampler
//------------------------------------------------------------------------------

function BilinearResample(img: TImage32; x256, y256: Integer): TColor32;
var
  xi,yi, weight: Integer;
  iw, ih: integer;
  pixels: TArrayOfColor32;
  color: TWeightedColor;
  xf, yf: cardinal;
begin
  iw := img.Width;
  ih := img.Height;
  pixels := img.Pixels;

  if (x256 <= -$100) or (x256 >= iw *$100) or
     (y256 <= -$100) or (y256 >= ih *$100) then
  begin
    result := clNone32;
    Exit;
  end;

  if x256 < 0 then xi := -1
  else xi := x256 shr 8;

  if y256 < 0 then yi := -1
  else yi := y256 shr 8;

  xf := x256 and $FF;
  yf := y256 and $FF;

  color.Reset;

  weight := (($100 - xf) * ($100 - yf)) shr 8;        //top-left
  if (xi < 0) or (yi < 0) then
    color.AddWeight(weight) else
    color.Add(pixels[xi + yi * iw], weight);

  weight := (xf * ($100 - yf)) shr 8;                 //top-right
  if ((xi+1) >= iw) or (yi < 0) then
    color.AddWeight(weight) else
    color.Add(pixels[(xi+1) + yi * iw], weight);

  weight := (($100 - xf) * yf) shr 8;                 //bottom-left
  if (xi < 0) or ((yi+1) >= ih) then
    color.AddWeight(weight) else
    color.Add(pixels[(xi) + (yi+1) * iw], weight);

  weight := (xf * yf) shr 8;                          //bottom-right
  if (xi + 1 >= iw) or (yi + 1 >= ih) then
    color.AddWeight(weight) else
    color.Add(pixels[(xi+1) + (yi+1) * iw], weight);

  Result := color.Color;
end;

//------------------------------------------------------------------------------
// BiCubic resampler
//------------------------------------------------------------------------------

type
  TBiCubicEdgeAdjust = (eaNone, eaOne, eaTwo, eaThree, eaFour);

var
  byteFrac: array [0..255] of double;
  byteFracSq: array [0..255] of double;
  byteFracCubed: array [0..255] of double;

//------------------------------------------------------------------------------

function CubicHermite(aclr: PColor32; t: Byte; bce: TBiCubicEdgeAdjust): TColor32;
var
  a,b,c,d: PARGB;
  q: TARGB;
	aa, bb, cc: integer;
  t1, t2, t3: double;
  res: TARGB absolute Result;
const
  clTrans: TColor32 = clNone32;
begin
  case bce of
    eaOne:
      begin
        a := @clTrans;
        b := @clTrans;
        c := PARGB(aclr);
        Inc(aclr);
        d := PARGB(aclr);
      end;
    eaTwo:
      begin
        a := PARGB(aclr);
        b := a;
        Inc(aclr);
        c := PARGB(aclr);
        Inc(aclr);
        d := PARGB(aclr);
      end;
    eaThree:
      begin
        a := PARGB(aclr);
        Inc(aclr);
        b := PARGB(aclr);
        Inc(aclr);
        c := PARGB(aclr);
        d := c;
      end;
    eaFour:
      begin
        a := PARGB(aclr);
        Inc(aclr);
        b := PARGB(aclr);
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
  else if b.A = 0 then
  begin
    q := c^;
    q.A := 0;
    b := @q;
  end;
  if c.A = 0 then
  begin
    q := b^;
    q.A := 0;
    c := @q;
  end;

  t1 := byteFrac[t];
  t2 := byteFracSq[t];
  t3 := byteFracCubed[t];

	aa := Integer(-a.A + 3*b.A - 3*c.A + d.A) div 2;
	bb := Integer(2*a.A - 5*b.A + 4*c.A - d.A) div 2;
	cc := Integer(-a.A + c.A) div 2;
  Res.A := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.A);

	aa := Integer(-a.R + 3*b.R - 3*c.R + d.R) div 2;
	bb := Integer(2*a.R - 5*b.R + 4*c.R - d.R) div 2;
	cc := Integer(-a.R + c.R) div 2;
  Res.R := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.R);

	aa := Integer(-a.G + 3*b.G - 3*c.G + d.G) div 2;
	bb := Integer(2*a.G - 5*b.G + 4*c.G - d.G) div 2;
	cc := Integer(-a.G + c.G) div 2;
  Res.G := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.G);

	aa := Integer(-a.B + 3*b.B - 3*c.B + d.B) div 2;
	bb := Integer(2*a.B - 5*b.B + 4*c.B - d.B) div 2;
	cc := Integer(-a.B + c.B) div 2;
  Res.B := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.B);
end;
//------------------------------------------------------------------------------

function BicubicResample(img: TImage32; x256, y256: Integer): TColor32;
var
  i, dx,dy, pi, iw, w,h: Integer;
  c: array[0..3] of TColor32;
  x, y: Integer;
  bceX, bceY: TBiCubicEdgeAdjust;
begin
  Result := clNone32;

  iw := img.Width;
  w := iw -1;
  h := img.Height -1;

  x := Abs(x256) shr 8;
  y := Abs(y256) shr 8;

  if (x256 < -$FF) or (x > w) or  (y256 < -$FF) or (y > h) then Exit;

  if (x256 < 0) then bceX := eaOne
  else if (x = 0) then bceX := eaTwo
  else if (x256 > w shl 8) then bceX := eaFour
  else if (x256 > (w -1) shl 8) then bceX := eaThree
  else bceX := eaNone;

  if (bceX = eaOne) or (bceX = eaTwo) then dx := 1
  else dx := 0;

  if (y256 < 0) then bceY := eaOne
  else if y = 0 then bceY := eaTwo
  else if y = h -1 then bceY := eaThree
  else if y = h then bceY := eaFour
  else bceY := eaNone;

  if (bceY = eaOne) or (bceY = eaTwo) then dy := 1
  else dy := 0;

  pi := (y -1 +dy) * iw + (x -1 + dx);

  if bceY = eaFour then dx := 2
  else if bceY = eaThree then dx := 1
  else dx := 0;

  for i := dy to 3 -dx do
  begin
    c[i] := CubicHermite(@img.Pixels[pi], x256 and $FF, bceX);
    inc(pi, iw);
  end;
  Result := CubicHermite(@c[dy], y256 and $FF, bceY);
end;

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

procedure BoxDownSampling(Image: TImage32; newWidth, newHeight: Integer);
var
  x,y, x256,y256,xx256,yy256: Integer;
  sx,sy: double;
  tmp: TArrayOfColor32;
  pc: PColor32;
  scaledX: array of Integer;
begin
  sx := Image.Width/newWidth * 256;
  sy := Image.Height/newHeight * 256;
  SetLength(tmp, newWidth * newHeight);

  SetLength(scaledX, newWidth +1); //+1 for fractional overrun
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

  Image.BeginUpdate;
  Image.SetSize(newWidth, newHeight);
  Move(tmp[0], Image.Pixels[0], newWidth * newHeight * SizeOf(TColor32));
  Image.EndUpdate;
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
begin
  for i := 0 to 255 do
  begin
    byteFrac[i]  := i     *inv255;
    byteFracSq[i]  := i*i   *inv255sqrd;
    byteFracCubed[i] := i*i*i *inv255cubed;
  end;
end;
//------------------------------------------------------------------------------

initialization
  InitByteExponents;

  rNearestResampler  := RegisterResampler(NearestResampler, 'NearestNeighbor');
  rBilinearResampler := RegisterResampler(BilinearResample, 'Bilinear');
  rBicubicResampler  := RegisterResampler(BicubicResample, 'HermiteBicubic');
  DefaultResampler   := rBilinearResampler;

end.
