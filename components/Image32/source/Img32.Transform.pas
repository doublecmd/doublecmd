unit Img32.Transform;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.3                                                             *
* Date      :  27 September 2022                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
*                                                                              *
* Purpose   :  Affine and projective transformation routines for TImage32      *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Types,
  Img32, Img32.Vector;

type
  TMatrixD = array [0..2, 0..2] of double;

  //Matrix functions
  function IsIdentityMatrix(const matrix: TMatrixD): Boolean;
  function IsValidMatrix(const matrix: TMatrixD): Boolean;
  function Matrix(const m00, m01, m02, m10, m11, m12, m20, m21, m22: double): TMatrixD;
  function MatrixDeterminant(const matrix: TMatrixD): double;
  function MatrixAdjugate(const matrix: TMatrixD): TMatrixD;
  function MatrixMultiply(const modifier, matrix: TMatrixD): TMatrixD;

  procedure MatrixApply(const matrix: TMatrixD;
    var x, y: double); overload; {$IFDEF INLINE} inline; {$ENDIF}
  procedure MatrixApply(const matrix: TMatrixD;
    var pt: TPointD); overload; {$IFDEF INLINE} inline; {$ENDIF}
  procedure MatrixApply(const matrix: TMatrixD; var rec: TRect); overload;
  procedure MatrixApply(const matrix: TMatrixD; var rec: TRectD); overload;
  procedure MatrixApply(const matrix: TMatrixD; var path: TPathD); overload;
  procedure MatrixApply(const matrix: TMatrixD; var paths: TPathsD); overload;
  function  MatrixInvert(var matrix: TMatrixD): Boolean;

  //MatrixSkew: dx represents the delta offset of an X coordinate as a
  //fraction of its Y coordinate, and likewise for dy. For example, if dx = 0.1
  //and dy = 0, and the matrix is applied to the coordinate [20,15], then the
  //transformed coordinate will become [20 + (15 * 0.1),10], ie [21.5,10].
  procedure MatrixSkew(var matrix: TMatrixD; angleX, angleY: double);
  procedure MatrixScale(var matrix: TMatrixD; scale: double); overload;
  procedure MatrixScale(var matrix: TMatrixD; scaleX, scaleY: double); overload;
  procedure MatrixRotate(var matrix: TMatrixD;
    const center: TPointD; angRad: double);
  procedure MatrixTranslate(var matrix: TMatrixD; dx, dy: double);

  //AffineTransformImage: automagically resizes and translates the image
  function AffineTransformImage(img: TImage32; matrix: TMatrixD): TPoint;

  //ProjectiveTransform:
  //  srcPts, dstPts => each path must contain 4 points
  //  margins => the margins around dstPts (in the dest. projective).
  //  Margins are only meaningful when srcPts are inside the image.
  function ProjectiveTransform(img: TImage32;
    const srcPts, dstPts: TPathD; const margins: TRect): Boolean;

  function SplineVertTransform(img: TImage32; const topSpline: TPathD;
    splineType: TSplineType; backColor: TColor32; out offset: TPoint): Boolean;
  function SplineHorzTransform(img: TImage32; const leftSpline: TPathD;
    splineType: TSplineType; backColor: TColor32; out offset: TPoint): Boolean;

  function ExtractAngleFromMatrix(const mat: TMatrixD): double;
  function ExtractScaleFromMatrix(const mat: TMatrixD): TSizeD;
  function ExtractAvgScaleFromMatrix(const mat: TMatrixD): double;
  procedure ExtractAllFromMatrix(const mat: TMatrixD;
    out angle: double; out scale, skew, trans: TPointD);

type
  PWeightedColor = ^TWeightedColor;
  TWeightedColor = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
  private
    fAddCount : Integer;
    fAlphaTot : Int64;
    fColorTotR: Int64;
    fColorTotG: Int64;
    fColorTotB: Int64;
    function GetColor: TColor32;
  public
    procedure Reset; {$IFDEF INLINE} inline; {$ENDIF}
    procedure Add(c: TColor32; w: Integer = 1); overload;
    procedure Add(const other: TWeightedColor); overload;
      {$IFDEF INLINE} inline; {$ENDIF}
    procedure Subtract(c: TColor32; w: Integer =1); overload;
    procedure Subtract(const other: TWeightedColor); overload;
      {$IFDEF INLINE} inline; {$ENDIF}
    procedure AddWeight(w: Integer); {$IFDEF INLINE} inline; {$ENDIF}
    property AddCount: Integer read fAddCount;
    property Color: TColor32 read GetColor;
    property Weight: integer read fAddCount;
  end;
  TArrayOfWeightedColor = array of TWeightedColor;

const
  IdentityMatrix: TMatrixD = ((1, 0, 0),(0, 1, 0),(0, 0, 1));

implementation

resourcestring
  rsInvalidScale   = 'Invalid matrix scaling factor (0)';

//------------------------------------------------------------------------------
// Matrix functions
//------------------------------------------------------------------------------

function IsIdentityMatrix(const matrix: TMatrixD): Boolean;
var
  i,j: integer;
const
  matVal: array [boolean] of double = (0.0, 1.0);
begin
  result := false;
  for i := 0 to 2 do
    for j := 0 to 2 do
      if matrix[i][j] <> matVal[j=i] then Exit;
  Result := true;
end;
//------------------------------------------------------------------------------

function IsValidMatrix(const matrix: TMatrixD): Boolean;
begin
  result := matrix[2][2] = 1.0;
end;
//------------------------------------------------------------------------------

function Matrix(const m00, m01, m02, m10, m11, m12, m20, m21, m22: double): TMatrixD;
begin
  Result[0,0] := m00; Result[0,1] := m01; Result[0,2] := m02;
  Result[1,0] := m10; Result[1,1] := m11; Result[1,2] := m12;
  Result[2,0] := m20; Result[2,1] := m21; Result[2,2] := m22;
end;
//------------------------------------------------------------------------------

function Det4(a1, a2, b1, b2: double): double; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := a1 * b2 - a2 * b1;
end;
//------------------------------------------------------------------------------

function Det9(a1, a2, a3, b1, b2, b3, c1, c2, c3: double): double;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := a1 * Det4(b2, b3, c2, c3) -
            b1 * Det4(a2, a3, c2, c3) +
            c1 * Det4(a2, a3, b2, b3);
end;
//------------------------------------------------------------------------------

function MatrixDeterminant(const matrix: TMatrixD): double;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := Det9(matrix[0,0], matrix[1,0], matrix[2,0],
                 matrix[0,1], matrix[1,1], matrix[2,1],
                 matrix[0,2], matrix[1,2], matrix[2,2]);
end;
//------------------------------------------------------------------------------

function MatrixAdjugate(const matrix: TMatrixD): TMatrixD;
begin
  //https://en.wikipedia.org/wiki/Adjugate_matrix
  Result[0,0] :=  Det4(matrix[1,1], matrix[1,2], matrix[2,1], matrix[2,2]);
  Result[0,1] := -Det4(matrix[0,1], matrix[0,2], matrix[2,1], matrix[2,2]);
  Result[0,2] :=  Det4(matrix[0,1], matrix[0,2], matrix[1,1], matrix[1,2]);

  Result[1,0] := -Det4(matrix[1,0], matrix[1,2], matrix[2,0], matrix[2,2]);
  Result[1,1] :=  Det4(matrix[0,0], matrix[0,2], matrix[2,0], matrix[2,2]);
  Result[1,2] := -Det4(matrix[0,0], matrix[0,2], matrix[1,0], matrix[1,2]);

  Result[2,0] :=  Det4(matrix[1,0], matrix[1,1], matrix[2,0], matrix[2,1]);
  Result[2,1] := -Det4(matrix[0,0], matrix[0,1], matrix[2,0], matrix[2,1]);
  Result[2,2] :=  Det4(matrix[0,0], matrix[0,1], matrix[1,0], matrix[1,1]);
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var x, y: double);
var
  tmpX: double;
begin
  tmpX := x;
  x := tmpX * matrix[0, 0] + y * matrix[1, 0] + matrix[2, 0];
  y := tmpX * matrix[0, 1] + y * matrix[1, 1] + matrix[2, 1];
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var pt: TPointD);
var
  tmpX: double;
begin
  tmpX := pt.x;
  pt.X := tmpX * matrix[0, 0] + pt.Y * matrix[1, 0] + matrix[2, 0];
  pt.Y := tmpX * matrix[0, 1] + pt.Y * matrix[1, 1] + matrix[2, 1];
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var rec: TRect);
var
  l,t,b,r,tmpX: double;
begin
  tmpX := rec.Left;
  l := tmpX * matrix[0, 0] + rec.Top * matrix[1, 0] + matrix[2, 0];
  t := tmpX * matrix[0, 1] + rec.Top * matrix[1, 1] + matrix[2, 1];
  tmpX := rec.Right;
  r := tmpX * matrix[0, 0] + rec.Bottom * matrix[1, 0] + matrix[2, 0];
  b := tmpX * matrix[0, 1] + rec.Bottom * matrix[1, 1] + matrix[2, 1];
  rec := Rect(RectD(l,t,r,b));
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var rec: TRectD);
var
  path: TPathD;
begin
  path := Rectangle(rec);
  MatrixApply(matrix, path);
  rec := GetBoundsD(path);
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var path: TPathD);
var
  i, len: integer;
  tmpX: double;
  pp: PPointD;
begin
  len := Length(path);
  if (len = 0) or IsIdentityMatrix(matrix) then Exit;
  pp := @path[0];
  for i := 0 to len -1 do
  begin
    tmpX := pp.X;
    pp.X := tmpX * matrix[0, 0] + pp.Y * matrix[1, 0] + matrix[2, 0];
    pp.Y := tmpX * matrix[0, 1] + pp.Y * matrix[1, 1] + matrix[2, 1];
    inc(pp);
  end;
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var paths: TPathsD);
var
  i,j,len: integer;
  tmpX: double;
  pp: PPointD;
begin
  if not Assigned(paths) or IsIdentityMatrix(matrix) then
    Exit;

  for i := 0 to High(paths) do
  begin
    len := Length(paths[i]);
    if len = 0 then Continue;
    pp := @paths[i][0];
    for j := 0 to High(paths[i]) do
    begin
      tmpX := pp.X;
      pp.X := tmpX * matrix[0, 0] + pp.Y * matrix[1, 0] + matrix[2, 0];
      pp.Y := tmpX * matrix[0, 1] + pp.Y * matrix[1, 1] + matrix[2, 1];
      inc(pp);
    end;
  end;
end;
//------------------------------------------------------------------------------

function MatrixMultiply(const modifier, matrix: TMatrixD): TMatrixD;
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] :=
        (modifier[0, j] * matrix[i, 0]) +
        (modifier[1, j] * matrix[i, 1]) +
        (modifier[2, j] * matrix[i, 2]);
end;
//------------------------------------------------------------------------------

procedure MatrixScale(var matrix: TMatrixD; scaleX, scaleY: double);
var
  m: TMatrixD;
begin
  m := IdentityMatrix;
  if (scaleX = 0) or (scaleY = 0) then
    raise Exception(rsInvalidScale);

  if ValueAlmostOne(scaleX) and ValueAlmostOne(scaleY) then Exit;
  m[0, 0] := scaleX;
  m[1, 1] := scaleY;
  matrix := MatrixMultiply(m, matrix);
end;
//------------------------------------------------------------------------------

procedure MatrixScale(var matrix: TMatrixD; scale: double);
begin
  if (scale = 0) or (scale = 1) then Exit;
  MatrixScale(matrix, scale, scale);
end;
//------------------------------------------------------------------------------

procedure MatrixRotate(var matrix: TMatrixD;
  const center: TPointD; angRad: double);
var
  m: TMatrixD;
  sinA, cosA: double;
  origOffset: Boolean;
begin
  NormalizeAngle(angRad);
  if angRad = 0 then Exit;
  if ClockwiseRotationIsAnglePositive then
    angRad := -angRad; //negated angle because of inverted Y-axis.
  m := IdentityMatrix;
  origOffset := (center.X <> 0) or (center.Y <> 0);
  if origOffset then MatrixTranslate(matrix, -center.X, -center.Y);
  GetSinCos(angRad, sinA, cosA);
  m := IdentityMatrix;
  m[0, 0] := cosA;   m[1, 0] := sinA;
  m[0, 1] := -sinA;  m[1, 1] := cosA;
  matrix := MatrixMultiply(m, matrix);
  if origOffset then MatrixTranslate(matrix, center.X, center.Y);
end;
//------------------------------------------------------------------------------

procedure MatrixTranslate(var matrix: TMatrixD; dx, dy: double);
var
  m: TMatrixD;
begin
  if ValueAlmostZero(dx) and ValueAlmostZero(dy) then Exit;
  m := IdentityMatrix;
  m[2, 0] := dx;
  m[2, 1] := dy;
  matrix := MatrixMultiply(m, matrix);
end;
//------------------------------------------------------------------------------

procedure ScaleInternal(var matrix: TMatrixD; s: double);
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      matrix[i,j] := matrix[i,j] * s;
end;
//------------------------------------------------------------------------------

function MatrixInvert(var matrix: TMatrixD): Boolean;
var
  d: double;
const
  tolerance = 1.0E-5;
begin
  d := MatrixDeterminant(matrix);
  Result := abs(d) > tolerance;
  if Result then
  begin
    matrix := MatrixAdjugate(matrix);
    ScaleInternal(matrix, 1/d);
  end;
end;
//------------------------------------------------------------------------------

procedure MatrixSkew(var matrix: TMatrixD; angleX, angleY: double);
var
  m: TMatrixD;
begin
  if ValueAlmostZero(angleX) and ValueAlmostZero(angleY) then Exit;
  m := IdentityMatrix;
  m[1, 0] := tan(angleX);
  m[0, 1] := tan(angleY);
  matrix := MatrixMultiply(m, matrix);
end;

//------------------------------------------------------------------------------
// Affine Transformation
//------------------------------------------------------------------------------

function GetTransformBounds(img: TImage32; const matrix: TMatrixD): TRect;
var
  pts: TPathD;
begin
  pts := Rectangle(img.Bounds);
  MatrixApply(matrix, pts);
  Result := GetBounds(pts);
end;
//------------------------------------------------------------------------------

function AffineTransformImage(img: TImage32; matrix: TMatrixD): TPoint;
var
  i,j, srcWidth, srcHeight: integer;
  newWidth, newHeight: integer;
  x,y: double;
  pc: PColor32;
  tmp: TArrayOfColor32;
  dstRec: TRect;
  resampler: TResamplerFunction;
begin
  Result := NullPoint;
  srcWidth := img.Width;
  srcHeight := img.Height;

  if img.Resampler = 0 then
    resampler := nil else
    resampler := GetResampler(img.Resampler);

  if not Assigned(resampler) or
    (srcWidth * srcHeight = 0) or IsIdentityMatrix(matrix) then
      Exit;

  //auto-resize the image so it'll fit transformed image
  dstRec := GetTransformBounds(img, matrix);
  RectWidthHeight(dstRec, newWidth, newHeight);
  //auto-translate the image too
  Result := dstRec.TopLeft;

  //starting with the result pixel coords, reverse lookup
  //the fractional coordinates in the untransformed image
  if not MatrixInvert(matrix) then Exit;

  SetLength(tmp, newWidth * newHeight);
  pc := @tmp[0];

  for i := dstRec.Top to + dstRec.Bottom -1 do
    for j := dstRec.Left to dstRec.Right -1 do
    begin
      //convert dest X,Y to src X,Y ...
      x := j; y := i;
      MatrixApply(matrix, x, y);
      //get weighted pixel (slow)
      pc^ := resampler(img, Round(x * 256), Round(y * 256));
      inc(pc);
    end;
  img.BeginUpdate;
  try
    img.SetSize(newWidth, newHeight);
    Move(tmp[0], img.Pixels[0], newWidth * newHeight * sizeOf(TColor32));
  finally
    img.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
// Projective Transformation
//------------------------------------------------------------------------------

procedure MatrixMulCoord(const matrix: TMatrixD; var x,y,z: double);
{$IFDEF INLINE} inline; {$ENDIF}
var
  xx, yy: double;
begin
  xx := x; yy := y;
  x := matrix[0,0] *xx + matrix[0,1] *yy + matrix[0,2] *z;
  y := matrix[1,0] *xx + matrix[1,1] *yy + matrix[1,2] *z;
  z := matrix[2,0] *xx + matrix[2,1] *yy + matrix[2,2] *z;
end;
//------------------------------------------------------------------------------

function BasisToPoints(x1, y1, x2, y2, x3, y3, x4, y4: double): TMatrixD;
var
  m, m2: TMatrixD;
  z4: double;
begin
  m := Matrix(x1, x2, x3, y1, y2, y3, 1,  1,  1);
  m2 := MatrixAdjugate(m);
  z4 := 1;
  MatrixMulCoord(m2, x4, y4, z4);
  m2 := Matrix(x4, 0, 0, 0, y4, 0, 0, 0, z4);
  Result := MatrixMultiply(m2, m);
end;
//------------------------------------------------------------------------------

procedure GetSrcCoords256(const matrix: TMatrixD; var x, y: integer);
{$IFDEF INLINE} inline; {$ENDIF}
var
  xx,yy,zz: double;
const
  Q: integer = MaxInt div 256;
begin
  //returns coords multiplied by 256 in anticipation of the following
  //GetWeightedPixel function call which in turn expects the lower 8bits
  //of the integer coord value to represent a fraction.
  xx := x; yy := y; zz := 1;
  MatrixMulCoord(matrix, xx, yy, zz);

  if zz = 0 then
  begin
    if xx >= 0 then x := Q else x := -MaxInt;
    if yy >= 0 then y := Q else y := -MaxInt;
  end else
  begin
    xx := xx/zz;
    if xx > Q then x := MaxInt
    else if xx < -Q then x := -MaxInt
    else x := Round(xx *256);

    yy := yy/zz;
    if yy > Q then y := MaxInt
    else if yy < -Q then y := -MaxInt
    else y := Round(yy *256);
  end;
end;
//------------------------------------------------------------------------------

function GetProjectionMatrix(const srcPts, dstPts: TPathD): TMatrixD;
var
  srcMat, dstMat: TMatrixD;
begin
  if (length(srcPts) <> 4) or (length(dstPts) <> 4) then
  begin
    Result := IdentityMatrix;
    Exit;
  end;
  srcMat := BasisToPoints(srcPts[0].X, srcPts[0].Y,
    srcPts[1].X, srcPts[1].Y, srcPts[2].X, srcPts[2].Y, srcPts[3].X, srcPts[3].Y);
  dstMat := BasisToPoints(dstPts[0].X, dstPts[0].Y,
    dstPts[1].X, dstPts[1].Y, dstPts[2].X, dstPts[2].Y, dstPts[3].X, dstPts[3].Y);
  Result := MatrixMultiply(MatrixAdjugate(dstMat), srcMat);
end;
//------------------------------------------------------------------------------

function ProjectiveTransform(img: TImage32;
  const srcPts, dstPts: TPathD; const margins: TRect): Boolean;
var
  w,h,i,j: integer;
  x,y: integer;
  rec: TRect;
  dstPts2: TPathD;
  mat: TMatrixD;
  tmp: TArrayOfColor32;
  pc: PColor32;
  resampler: TResamplerFunction;
begin
  //https://math.stackexchange.com/a/339033/384709

  if img.Resampler = 0 then
    resampler := nil else
    resampler := GetResampler(img.Resampler);

  Result := Assigned(resampler) and not img.IsEmpty and
    (Length(dstPts) = 4) and IsPathConvex(dstPts);
  if not Result then Exit;

  rec := GetBounds(dstPts);
  dec(rec.Left, margins.Left);
  dec(rec.Top, margins.Top);
  inc(rec.Right, margins.Right);
  inc(rec.Bottom, margins.Bottom);
  dstPts2 := OffsetPath(dstPts, -rec.Left, -rec.Top);

  mat := GetProjectionMatrix(srcPts, dstPts2);
  RectWidthHeight(rec, w, h);
  SetLength(tmp, w * h);
  pc := @tmp[0];
  for i :=  0 to h -1 do
    for j := 0 to w -1 do
    begin
      x := j; y := i;
      GetSrcCoords256(mat, x, y);
      pc^ := resampler(img, x, y);
      inc(pc);
    end;
  img.SetSize(w, h);
  Move(tmp[0], img.PixelBase^, w * h * sizeOf(TColor32));
end;

//------------------------------------------------------------------------------
// Spline transformations
//------------------------------------------------------------------------------

function ReColor(color, newColor: TColor32): TColor32;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := (color and $FF000000) or newColor;
end;
//------------------------------------------------------------------------------

function InterpolateSegX(const pt1, pt2: TPointD): TPathD;
var
  i, x1, x2: integer;
  xo,dydx: double;
begin
  Result := nil;
  if pt2.X > pt1.X then
  begin
    x1 := Ceil(pt1.X);
    x2 := Ceil(pt2.X);
    if x1 = x2 then Exit;
    dydx := (pt2.Y - pt1.Y)/(pt2.X - pt1.X);
    xo := x1 -pt1.X;
    SetLength(Result, x2-x1);
    for i:= 0 to x2 - x1 -1 do
    begin
      Result[i].X := x1 +i;
      Result[i].Y := pt1.Y + dydx * (xo +i);
    end;
  end else
  begin
    x1 := Floor(pt1.X);
    x2 := Floor(pt2.X);
    if x1 = x2 then Exit;
    dydx := (pt2.Y - pt1.Y)/(pt2.X - pt1.X);
    xo := x1 -pt1.X;
    SetLength(Result, x1-x2);
    for i:= 0 to x1 - x2 -1 do
    begin
      Result[i].X := x1 -i;
      Result[i].Y := pt1.Y + dydx * (xo -i);
    end;
  end;
end;
//------------------------------------------------------------------------------

function InterpolateSegY(const pt1, pt2: TPointD): TPathD;
var
  i, y1,y2: integer;
  yo,dxdy: double;
begin
  Result := nil;
  if pt2.Y > pt1.Y then
  begin
    y1 := Ceil(pt1.Y);
    y2 := Ceil(pt2.Y);
    if y1 = y2 then Exit;
    dxdy := (pt2.X - pt1.X)/(pt2.Y - pt1.Y);
    yo := y1 -pt1.Y;
    SetLength(Result, y2-y1);
    for i:= 0 to y2 - y1 -1 do
    begin
      Result[i].Y := y1 +i;
      Result[i].X := pt1.X + dxdy * (yo +i);
    end;
  end else
  begin
    y1 := Floor(pt1.Y);
    y2 := Floor(pt2.Y);
    if y1 = y2 then Exit;
    dxdy := (pt2.X - pt1.X)/(pt2.Y - pt1.Y);
    yo := y1 -pt1.Y;
    SetLength(Result, y1-y2);
    for i:= 0 to y1 - y2 -1 do
    begin
      Result[i].Y := y1 -i;
      Result[i].X := pt1.X + dxdy * (yo -i);
    end;
  end;
end;
//------------------------------------------------------------------------------

function InterpolatePathForX(const path: TPathD): TPathD;
var
  i,len: integer;
  tmp: TPathD;
begin
  Result := nil;
  len := length(path);
  if len < 2 then Exit;
  for i := 1 to len -1 do
  begin
    tmp := InterpolateSegX(path[i-1], path[i]);
    AppendPath(Result, tmp);
  end;
end;
//------------------------------------------------------------------------------

function InterpolatePathForY(const path: TPathD): TPathD;
var
  i, len: integer;
  tmp: TPathD;
begin
  Result := nil;
  len := length(path);
  if len < 2 then Exit;
  for i := 1 to len -1 do
  begin
    tmp := InterpolateSegY(path[i-1], path[i]);
    AppendPath(Result, tmp);
  end;
end;
//------------------------------------------------------------------------------

function SplineVertTransform(img: TImage32; const topSpline: TPathD;
  splineType: TSplineType; backColor: TColor32; out offset: TPoint): Boolean;
var
  i,j, w,h, len: integer;
  y, q: double;
  distances: TArrayOfDouble;
  pc: PColor32;
  rec: TRect;
  tmp: TArrayOfColor32;
  topPath: TPathD;
  prevX: double;
  resampler: TResamplerFunction;
  backColoring, allowBackColoring: Boolean;
begin
  offset := NullPoint;
  if img.Resampler = 0 then
    resampler := nil else
    resampler := GetResampler(img.Resampler);

  //convert the top spline control points into a flattened path
  if splineType = stQuadratic then
    topPath := FlattenQSpline(topSpline) else
    topPath := FlattenCSpline(topSpline);

  rec := GetBounds(topPath);
  //return false if the spline is invalid or there's no vertical transformation
  Result := Assigned(resampler) and not IsEmptyRect(rec);
  if not Result then Exit;

  offset := rec.TopLeft;
  topPath := InterpolatePathForX(topPath);
  len := Length(topPath);
  inc(rec.Bottom, img.Height);
  RectWidthHeight(rec, w, h);
  SetLength(tmp, (w+1) * h);

  prevX := topPath[0].X;
  allowBackColoring := GetAlpha(backColor) > 2;
  backColor := backColor and $00FFFFFF;

  distances := GetCumulativeDistances(topPath);
  q := img.Width * 256 / distances[High(distances)];;
  for i := 0 to len -1 do
  begin
    pc := @tmp[Round(topPath[i].X)-rec.Left];
    backColoring := allowBackColoring and (prevX >= topPath[i].X);
    prevX := topPath[i].X;
    y := topPath[i].Y;
    for j := rec.top to rec.bottom -1 do
    begin
      if (j > y-1.0) and (j < y + img.Height) then
        if backColoring then
          pc^ := BlendToAlpha(pc^,
            ReColor(resampler(img, Round(Distances[i]*q) ,Round((j - y)*256)), backColor))
        else
          pc^ := BlendToAlpha(pc^,
            resampler(img, Round(Distances[i]*q) ,Round((j - y)*256)));
      inc(pc, w);
    end;
  end;

  img.BeginUpdate;
  img.SetSize(w,h);
  Move(tmp[0], img.Pixels[0], img.Width * img.Height * SizeOf(TColor32));
  img.EndUpdate;
end;
//------------------------------------------------------------------------------

function SplineHorzTransform(img: TImage32; const leftSpline: TPathD;
  splineType: TSplineType; backColor: TColor32; out offset: TPoint): Boolean;
var
  i,j, len, w,h: integer;
  x, q, prevY: double;
  leftPath: TPathD;
  distances: TArrayOfDouble;
  rec: TRect;
  pc: PColor32;
  tmp: TArrayOfColor32;
  backColoring, allowBackColoring: Boolean;
  resampler: TResamplerFunction;
begin
  offset := NullPoint;

  if img.Resampler = 0 then
    resampler := nil else
    resampler := GetResampler(img.Resampler);

  //convert the left spline control points into a flattened path
  if splineType = stQuadratic then
    leftPath := FlattenQSpline(leftSpline) else
    leftPath := FlattenCSpline(leftSpline);
  rec := GetBounds(leftPath);

  //return false if the spline is invalid or there's no horizontal transformation
  Result := Assigned(resampler) and not IsEmptyRect(rec);
  if not Result then Exit;

  offset := rec.TopLeft;
  leftPath := InterpolatePathForY(leftPath);
  len := Length(leftPath);
  inc(rec.Right, img.Width);
  RectWidthHeight(rec, w, h);
  SetLength(tmp, w * (h+1));

  prevY := leftPath[0].Y;
  allowBackColoring := GetAlpha(backColor) > 2;
  backColor :=   backColor and $00FFFFFF;

  distances := GetCumulativeDistances(leftPath);
  q := img.Height * 256 / distances[High(distances)];;
  for i := 0 to len -1 do
  begin
    pc := @tmp[Round(leftPath[i].Y - rec.Top) * w];
    backColoring := allowBackColoring and (prevY >= leftPath[i].Y);
    prevY := leftPath[i].Y;
    x := leftPath[i].X;
    for j := rec.left to rec.right -1 do
    begin
      if (j > x-1.0) and (j < x + img.Width) then
        if backColoring then
          pc^ := BlendToAlpha(pc^,
            ReColor(resampler(img, Round((j - x) *256), Round(Distances[i]*q)), backColor))
        else
          pc^ := BlendToAlpha(pc^,
            resampler(img, Round((j - x) *256), Round(Distances[i]*q)));
      inc(pc);
    end;
  end;

  img.BeginUpdate;
  img.SetSize(w,h);
  Move(tmp[0], img.Pixels[0], img.Width * img.Height * SizeOf(TColor32));
  img.EndUpdate;
end;

//------------------------------------------------------------------------------
// TWeightedColor
//------------------------------------------------------------------------------

procedure TWeightedColor.Reset;
begin
  fAddCount := 0;
  fAlphaTot := 0;
  fColorTotR := 0;
  fColorTotG := 0;
  fColorTotB := 0;
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.AddWeight(w: Integer);
begin
  inc(fAddCount, w);
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.Add(c: TColor32; w: Integer);
var
  a: Integer;
  argb: TARGB absolute c;
begin
  inc(fAddCount, w);
  a := w * argb.A;
  if a = 0 then Exit;
  inc(fAlphaTot, a);
  inc(fColorTotB, (a * argb.B));
  inc(fColorTotG, (a * argb.G));
  inc(fColorTotR, (a * argb.R));
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.Add(const other: TWeightedColor);
begin
  inc(fAddCount, other.fAddCount);
  inc(fAlphaTot, other.fAlphaTot);
  inc(fColorTotR, other.fColorTotR);
  inc(fColorTotG, other.fColorTotG);
  inc(fColorTotB, other.fColorTotB);
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.Subtract(c: TColor32; w: Integer);
var
  a: Integer;
  argb: TARGB absolute c;
begin
  dec(fAddCount, w);
  a := w * argb.A;
  if a = 0 then Exit;
  dec(fAlphaTot, a);
  dec(fColorTotB, (a * argb.B));
  dec(fColorTotG, (a * argb.G));
  dec(fColorTotR, (a * argb.R));
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.Subtract(const other: TWeightedColor);
begin
  dec(fAddCount, other.fAddCount);
  dec(fAlphaTot, other.fAlphaTot);
  dec(fColorTotR, other.fColorTotR);
  dec(fColorTotG, other.fColorTotG);
  dec(fColorTotB, other.fColorTotB);
end;
//------------------------------------------------------------------------------

function TWeightedColor.GetColor: TColor32;
var
  invAlpha: double;
  res: TARGB absolute Result;
begin
  if (fAlphaTot <= 0) or (fAddCount <= 0) then
  begin
    result := clNone32;
    Exit;
  end;
  res.A := Min(255, (fAlphaTot  + (fAddCount shr 1)) div fAddCount);
  //nb: alpha weighting is applied to colors when added,
  //so we now need to div by fAlphaTot here ...
  invAlpha := 1/fAlphaTot;
  res.R := ClampByte(fColorTotR * invAlpha);
  res.G := ClampByte(fColorTotG * invAlpha);
  res.B := ClampByte(fColorTotB * invAlpha);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure ExtractAllFromMatrix(const mat: TMatrixD; out angle: double;
  out scale, skew, trans: TPointD);
var
  a,b,c,d,e,f: double;
  delta, r,s: double;
begin
  a := mat[0][0]; b := mat[1][0];
  c := mat[0][1]; d := mat[1][1];
  e := mat[2][0]; f := mat[2][1];

  delta := a * d - b * c;
  trans := PointD(e,f);
  angle := 0;
  scale := PointD(1,1);
  skew := NullPointD;

  if (a <> 0) or (b <> 0) then
  begin
    r := Sqrt(a * a + b * b);
	  angle :=  ArcCos(a / r);
	  if b < 0 then angle := -angle;
    scale.X	:= r;
	  scale.Y	:= delta / r;
    skew.X	:= ArcTan((a * c + b * d) / (r * r));
  end
  else if (c <> 0) or (d <> 0) then
  begin
    s := Sqrt(c * c + d * d);
    if d > 0 then
      angle := Angle90 - ArcCos(-c / s) else
	  angle := Angle90 + ArcCos(c / s);
    scale.X := delta / s;
    scale.Y := s;
    skew.Y  := ArcTan((a * c + b * d) / (s * s));
  end;
  angle := -angle;
  NormalizeAngle(angle);
end;
//------------------------------------------------------------------------------

function ExtractAngleFromMatrix(const mat: TMatrixD): double;
var
  a,b,c,d: double;
  r,s: double;
begin
  a := mat[0][0]; b := mat[1][0];
  c := mat[0][1]; d := mat[1][1];

  if (a <> 0) or (b <> 0) then
  begin
    r := Sqrt(a * a + b * b);
	  Result :=  ArcCos(a / r);
	  if b < 0 then Result := -Result;
  end
  else if (c <> 0) or (d <> 0) then
  begin
    s := Sqrt(c * c + d * d);
    if d > 0 then
      Result := Angle90 - ArcCos(-c / s) else
	  Result := Angle90 + ArcCos(c / s);
  end else
  begin
    Result := InvalidD; //error
    Exit;
  end;
  Result := -Result;
  NormalizeAngle(Result);
end;
//------------------------------------------------------------------------------

function ExtractScaleFromMatrix(const mat: TMatrixD): TSizeD;
var
  a,b,c,d: double;
  delta, q: double;
begin
  a := mat[0][0]; b := mat[1][0];
  c := mat[0][1]; d := mat[1][1];

  delta := a * d - b * c;
  if (a <> 0) or (b <> 0) then
  begin
    q := Sqrt(a * a + b * b);
    Result.cx	:= q;
	  Result.cy	:= delta / q;
  end
  else if (c <> 0) or (d <> 0) then
  begin
    q := Sqrt(c * c + d * d);
    Result.cx := delta / q;
    Result.cy := q;
  end else
    Result := SizeD(0.0, 0.0);
end;
//------------------------------------------------------------------------------

function ExtractAvgScaleFromMatrix(const mat: TMatrixD): double;
var
  scale: TSizeD;
begin
  scale := ExtractScaleFromMatrix(mat);
  Result := Average(Abs(scale.cx), Abs(scale.cy));
end;
//------------------------------------------------------------------------------

end.
