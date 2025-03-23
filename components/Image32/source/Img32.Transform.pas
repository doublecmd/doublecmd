unit Img32.Transform;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.7                                                             *
* Date      :  6 January 2025                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  Affine and projective transformation routines for TImage32      *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Types, Img32, Img32.Vector;

type
  TMatrixD = array [0..2, 0..2] of double;

  //Matrix functions
  function IsIdentityMatrix(const matrix: TMatrixD): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
  function IsValidMatrix(const matrix: TMatrixD): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
  function Matrix(const m00, m01, m02, m10, m11, m12, m20, m21, m22: double): TMatrixD;
  function MatrixDeterminant(const matrix: TMatrixD): double;
  function MatrixAdjugate(const matrix: TMatrixD): TMatrixD;
  function  MatrixInvert(var matrix: TMatrixD): Boolean;

  // Note: Matrix multiplication IS NOT commutative hence ...
  procedure MatrixMultiply(var matrix1: TMatrixD; const matrix2: TMatrixD);
  procedure MatrixMultiply2(const matrix1: TMatrixD; var matrix2: TMatrixD);

  procedure MatrixApply(const matrix: TMatrixD;
    var x, y: double); overload; {$IFDEF INLINE} inline; {$ENDIF}
  procedure MatrixApply(const matrix: TMatrixD;
    var pt: TPointD); overload; {$IFDEF INLINE} inline; {$ENDIF}
  procedure MatrixApply(const matrix: TMatrixD; var rec: TRect); overload;
  procedure MatrixApply(const matrix: TMatrixD; var rec: TRectD); overload;
  procedure MatrixApply(const matrix: TMatrixD; var path: TPathD); overload;
  procedure MatrixApply(const matrix: TMatrixD; var paths: TPathsD); overload;
  procedure MatrixApply(const matrix: TMatrixD;
    img: TImage32; scaleAdjust: Boolean = false); overload; {$IFDEF INLINE} inline; {$ENDIF}
  procedure MatrixApply(const matrix: TMatrixD;
    img, targetImg: TImage32; scaleAdjust: Boolean = false); overload; {$IFDEF INLINE} inline; {$ENDIF}

  procedure MatrixSkew(var matrix: TMatrixD; angleX, angleY: double);

  procedure MatrixScale(var matrix: TMatrixD; scale: double); overload;
  procedure MatrixScale(var matrix: TMatrixD; scaleX, scaleY: double); overload;

  procedure MatrixRotate(var matrix: TMatrixD; angRad: double); overload;
  procedure MatrixRotate(var matrix: TMatrixD; const center: TPointD; angRad: double); overload;

  procedure MatrixTranslate(var matrix: TMatrixD; dx, dy: double);

  // The following MatrixExtract routines assume here is no skew
  procedure MatrixExtractScale(const mat: TMatrixD; out scale: double); overload;
  procedure MatrixExtractScale(const mat: TMatrixD; out X, Y: double); overload;
  procedure MatrixExtractTranslation(const mat: TMatrixD; out dx, dy: double);
  procedure MatrixExtractRotation(const mat: TMatrixD; out angle: double);
  // MatrixExtractAll - except skew :)
  function MatrixExtractAll(const mat: TMatrixD; out angle: double;
    out scale, trans: TPointD): Boolean;

  // AffineTransformImage: will automagically translate the image
  // Note: when the "scaleAdjust" parameter is enabled, it prevents antialiasing
  // from extending way outside of images when they are being enlarged
  // significantly (> 2 times) and rotated concurrently
  function AffineTransformImage(img: TImage32; matrix: TMatrixD;
    scaleAdjust: Boolean = false): TPoint; overload; {$IFDEF INLINE} inline; {$ENDIF}
  function AffineTransformImage(img, targetImg: TImage32; matrix: TMatrixD;
    scaleAdjust: Boolean = false): TPoint; overload;

  // ProjectiveTransform:
  //  srcPts, dstPts => each path must contain 4 points
  //  margins => the margins around dstPts (in the dest. projective).
  //  Margins are only meaningful when srcPts are inside the image.
  function ProjectiveTransform(img: TImage32;
    const srcPts, dstPts: TPathD; const margins: TRect): Boolean; overload; {$IFDEF INLINE} inline; {$ENDIF}
  function ProjectiveTransform(img, targetImg: TImage32;
    const srcPts, dstPts: TPathD; const margins: TRect): Boolean; overload;

  function SplineVertTransform(img: TImage32; const topSpline: TPathD;
    splineType: TSplineType; backColor: TColor32; out offset: TPoint): Boolean; overload; {$IFDEF INLINE} inline; {$ENDIF}
  function SplineVertTransform(img, targetImg: TImage32; const topSpline: TPathD;
    splineType: TSplineType; backColor: TColor32; out offset: TPoint): Boolean; overload;
  function SplineHorzTransform(img: TImage32; const leftSpline: TPathD;
    splineType: TSplineType; backColor: TColor32; out offset: TPoint): Boolean; overload; {$IFDEF INLINE} inline; {$ENDIF}
  function SplineHorzTransform(img, targetImg: TImage32; const leftSpline: TPathD;
    splineType: TSplineType; backColor: TColor32; out offset: TPoint): Boolean; overload;

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
    procedure Reset; overload; {$IFDEF INLINE} inline; {$ENDIF}
    procedure Reset(c: TColor32; w: Integer = 1); overload; {$IFDEF INLINE} inline; {$ENDIF}
    procedure Add(c: TColor32; w: Integer); overload; {$IFDEF INLINE_COMPATIBLE} inline; {$ENDIF}
    procedure Add(c: TColor32); overload; {$IFDEF INLINE} inline; {$ENDIF}
    procedure Add(const other: TWeightedColor); overload;
      {$IFDEF INLINE} inline; {$ENDIF}
    procedure Subtract(c: TColor32; w: Integer); overload;
    procedure Subtract(c: TColor32); overload; {$IFDEF INLINE} inline; {$ENDIF}
    procedure Subtract(const other: TWeightedColor); overload;
      {$IFDEF INLINE} inline; {$ENDIF}
    function AddSubtract(addC, subC: TColor32): Boolean; {$IFDEF INLINE_COMPATIBLE} inline; {$ENDIF}
    function AddNoneSubtract(c: TColor32): Boolean; {$IFDEF INLINE_COMPATIBLE} inline; {$ENDIF}
    procedure AddWeight(w: Integer); {$IFDEF INLINE} inline; {$ENDIF}
    property AddCount: Integer read fAddCount;
    property Color: TColor32 read GetColor;
    property Weight: integer read fAddCount;
  end;
  TArrayOfWeightedColor = array of TWeightedColor;

const
  IdentityMatrix: TMatrixD = ((1, 0, 0),(0, 1, 0),(0, 0, 1));

implementation

uses Img32.Resamplers;

resourcestring
  rsInvalidScale   = 'Invalid matrix scaling factor (0)';

const
  DivOneByXTableSize = 1024;

{$IFDEF CPUX86}
  // Use faster Trunc for x86 code in this unit.
  Trunc: function(Value: Double): Integer = __Trunc;
{$ENDIF CPUX86}

var
  // DivOneByXTable[x] = 1/x
  DivOneByXTable: array[0 .. DivOneByXTableSize -1] of Double;

//------------------------------------------------------------------------------
// Matrix functions
//------------------------------------------------------------------------------

function IsIdentityMatrix(const matrix: TMatrixD): Boolean;
begin
  result := (matrix[0,0] = 1) and (matrix[0,1] = 0) and (matrix[0,2] = 0) and
    (matrix[1,0] = 0) and (matrix[1,1] = 1) and (matrix[1,2] = 0) and
    (matrix[2,0] = 0) and (matrix[2,1] = 0) and (matrix[2,2] = 1);
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
  path: TPathD;
begin
  if not IsValidMatrix(matrix) then Exit;
  path := Rectangle(rec);
  MatrixApply(matrix, path);
  rec := GetBounds(path);
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var rec: TRectD);
var
  path: TPathD;
begin
  if not IsValidMatrix(matrix) then Exit;
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
  if (len = 0) or IsIdentityMatrix(matrix) or
    not IsValidMatrix(matrix) then Exit;
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
  if not Assigned(paths) or IsIdentityMatrix(matrix) or
    not IsValidMatrix(matrix) then Exit;

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

procedure MatrixApply(const matrix: TMatrixD;
  img: TImage32; scaleAdjust: Boolean);
begin
  AffineTransformImage(img, matrix, scaleAdjust);
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD;
  img, targetImg: TImage32; scaleAdjust: Boolean);
begin
  AffineTransformImage(img, targetImg, matrix, scaleAdjust);
end;
//------------------------------------------------------------------------------

procedure MatrixMultiply(var matrix1: TMatrixD; const matrix2: TMatrixD);
var
  i, j: Integer;
  m: TMatrixD;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      m[i, j] :=
        (matrix1[i, 0] * matrix2[0, j]) +
        (matrix1[i, 1] * matrix2[1, j]) +
        (matrix1[i, 2] * matrix2[2, j]);
  matrix1 := m;
end;
//------------------------------------------------------------------------------

procedure MatrixMultiply2(const matrix1: TMatrixD; var matrix2: TMatrixD);
var
  i, j: Integer;
  m: TMatrixD;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      m[i, j] :=
        (matrix1[i, 0] * matrix2[0, j]) +
        (matrix1[i, 1] * matrix2[1, j]) +
        (matrix1[i, 2] * matrix2[2, j]);
  matrix2 := m;
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
  MatrixMultiply(matrix, m);
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
begin
  if not PointsEqual(center, NullPointD) then
  begin
    NormalizeAngle(angRad);
    if angRad = 0 then Exit;
{$IFNDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
    angRad := -angRad; //negated angle because of inverted Y-axis.
{$ENDIF}
    m := IdentityMatrix;
    MatrixTranslate(matrix, -center.X, -center.Y);
    GetSinCos(angRad, sinA, cosA);
    m := IdentityMatrix;
    m[0, 0] := cosA;   m[1, 0] := sinA;
    m[0, 1] := -sinA;  m[1, 1] := cosA;
    MatrixMultiply(matrix, m);
    MatrixTranslate(matrix, center.X, center.Y);
  end
  else
    MatrixRotate(matrix, angRad)
end;
//------------------------------------------------------------------------------

procedure MatrixRotate(var matrix: TMatrixD; angRad: double);
var
  m: TMatrixD;
  sinA, cosA: double;
begin
  NormalizeAngle(angRad);
  if angRad = 0 then Exit;
{$IFNDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
    angRad := -angRad; //negated angle because of inverted Y-axis.
{$ENDIF}
  m := IdentityMatrix;
  GetSinCos(angRad, sinA, cosA);
  m := IdentityMatrix;
  m[0, 0] := cosA;   m[1, 0] := sinA;
  m[0, 1] := -sinA;  m[1, 1] := cosA;
  MatrixMultiply(matrix, m);
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
  MatrixMultiply(matrix, m);
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
  MatrixMultiply(matrix, m);
end;
//------------------------------------------------------------------------------

procedure MatrixExtractScale(const mat: TMatrixD; out X, Y: double);
begin
  // https://stackoverflow.com/a/32125700/359538
  X := Sqrt(Sqr(mat[0,0]) + Sqr(mat[0,1]));
  //Y := Sqrt(Sqr(mat[1,0]) + Sqr(mat[1,1]));
  Y := Abs((mat[0,0] * mat[1,1] - mat[1,0] * mat[0,1]) / X);
end;
//------------------------------------------------------------------------------

procedure MatrixExtractScale(const mat: TMatrixD; out scale: double);
var
  x,y: double;
begin
  MatrixExtractScale(mat, x, y);
  scale := Average(x,y);
end;
//------------------------------------------------------------------------------

procedure MatrixExtractTranslation(const mat: TMatrixD; out dx, dy: double);
begin
  dx := mat[2,0];
  dy := mat[2,1];
end;
//------------------------------------------------------------------------------

procedure MatrixExtractRotation(const mat: TMatrixD; out angle: double);
begin
  angle := ArcTan2(mat[0,1], mat[0,0]);
end;
//------------------------------------------------------------------------------

function MatrixExtractAll(const mat: TMatrixD;
  out angle: double; out scale, trans: TPointD): Boolean;
var
  m00, m01, m10, m11: double;
begin
  m00 := mat[0][0]; m10 := mat[1][0];
  m01 := mat[0][1]; m11 := mat[1][1];
  trans.X := mat[2][0];
  trans.Y := mat[2][1];

  angle := 0;
  scale := PointD(1,1);

  Result := (m00 <> 0) or (m01 <> 0);
  if not Result then Exit;

  angle := ArcTan2(m01, m00);
  // https://stackoverflow.com/a/32125700/359538
  scale.X := Sqrt(Sqr(mat[0,0]) + Sqr(mat[0,1]));
  scale.Y := (m00 * m11 - m10 * m01) / scale.X;
end;
//------------------------------------------------------------------------------

{$IFDEF USE_DOWNSAMPLER_AUTOMATICALLY}
function CanUseBoxDownsampler(const mat: TMatrixD; sx, sy: double): Boolean;
begin
  // If the matrix looks like this after removing the scale,
  // the box downsampler can be used. (only translation and scaling)
  //  cos(0)  -sin(0)  tx          1   0   tx
  //  sin(0)   cos(0)  ty    =>    0   1   ty
  //  0        0       1           0   0   1

{
  Result := (mat[0,0]/sx = 1) and (mat[0,1]/sx = 0) and
            (mat[1,0]/sy = 0) and (mat[1,1]/sy = 1) and
            (mat[2,0]    = 0) and (mat[2,1]    = 0) and
            (mat[2,2]    = 1);
}

  // We can skip the divisions, because m/s is only zero if m is zero
  // and m/s=1 is the same as m=s
  Result := (SameValue(mat[0,0], sx)) and (mat[0,1] = 0) and
            (mat[1,0] = 0)            and (SameValue(mat[1,1], sy)) and
            (mat[2,0] = 0)            and (mat[2,1] = 0) and
            (mat[2,2] = 1);
end;
{$ENDIF USE_DOWNSAMPLER_AUTOMATICALLY}

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

function AffineTransformImage(img: TImage32; matrix: TMatrixD;
  scaleAdjust: Boolean): TPoint;
begin
  Result := AffineTransformImage(img, img, matrix, scaleAdjust);
end;
//------------------------------------------------------------------------------

function AffineTransformImage(img, targetImg: TImage32; matrix: TMatrixD;
  scaleAdjust: Boolean): TPoint;
var
  i, j: integer;
  newWidth, newHeight: integer;
  sx, sy, x,y: double;
  xLimLo, yLimLo, xLimHi, yLimHi: double;
  pc: PColor32;
  tmp: TArrayOfColor32;
  dstRec: TRect;
  resampler: TResamplerFunction;
{$IFDEF USE_DOWNSAMPLER_AUTOMATICALLY}
  useBoxDownsampler: Boolean;
{$ENDIF}
begin
  Result := NullPoint;
  if IsIdentityMatrix(matrix) or
    img.IsEmpty or (targetImg.Resampler = 0) then
  begin
    if targetImg <> img then targetImg.Assign(img);
    Exit;
  end;

  resampler := GetResampler(targetImg.Resampler);
  if not Assigned(resampler) then
  begin
    if targetImg <> img then targetImg.Assign(img);
    Exit;
  end;

  //auto-resize the image so it'll fit transformed image

  dstRec := img.Bounds;
  MatrixApply(matrix, dstRec);
  RectWidthHeight(dstRec, newWidth, newHeight);

  MatrixExtractScale(matrix, sx, sy);
{$IFDEF USE_DOWNSAMPLER_AUTOMATICALLY}
  if (sx < 1.0) and (sy < 1.0) then
  begin
    //only use box downsampling when downsizing
    useBoxDownsampler := CanUseBoxDownsampler(matrix, sx, sy);
  end else
    useBoxDownsampler := false;

  if useBoxDownsampler then
  begin
    BoxDownSampling(img, targetImg, sx, sy);
    Exit;
  end;
{$ENDIF}

  if scaleAdjust then
  begin
    sx := Max(1, sx * 0.5);
    sy := Max(1, sy * 0.5);
  end;

  //auto-translate the image too
  Result := dstRec.TopLeft;

  //starting with the result pixel coords, reverse lookup
  //the fractional coordinates in the untransformed image
  if not MatrixInvert(matrix) then
  begin
    if targetImg <> img then targetImg.Assign(img);
    Exit;
  end;

  NewColor32Array(tmp, newWidth * newHeight, True);
  pc := @tmp[0];
  xLimLo := -0.5/sx;
  xLimHi := img.Width + 0.5/sx;
  yLimLo := -0.5/sy;
  yLimHi := img.Height + 0.5/sy;

  for i := dstRec.Top to dstRec.Bottom -1 do
  begin
    for j := dstRec.Left to dstRec.Right -1 do
    begin
      x := j; y := i;
      MatrixApply(matrix, x, y);

      if (x <= xLimLo) or (x >= xLimHi) or (y <= yLimLo) or (y >= yLimHi) then
        pc^ := clNone32
      else
        // nb: -0.5 below is needed to properly center the transformed image
        // (and this is most obviously needed when there is large scaling)
        pc^ := resampler(img, x - 0.5, y - 0.5);

      inc(pc);
    end;
  end;

  targetImg.AssignPixelArray(tmp, newWidth, newHeight);
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
  m2: TMatrixD;
  z4: double;
begin
  Result := Matrix(x1, x2, x3, y1, y2, y3, 1,  1,  1);
  m2 := MatrixAdjugate(Result);
  z4 := 1;
  MatrixMulCoord(m2, x4, y4, z4);
  m2 := Matrix(x4, 0, 0, 0, y4, 0, 0, 0, z4);
  MatrixMultiply(Result, m2);
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

procedure GetSrcCoords(const matrix: TMatrixD; var x, y: double);
{$IFDEF INLINE} inline; {$ENDIF}
var
  zz: double;
const
  Q: integer = MaxInt div 256;
begin
  //returns coords multiplied by 256 in anticipation of the following
  //GetWeightedPixel function call which in turn expects the lower 8bits
  //of the integer coord value to represent a fraction.
  zz := 1;
  MatrixMulCoord(matrix, x, y, zz);

  if zz = 0 then
  begin
    if x >= 0 then x := Q else x := -MaxDouble;
    if y >= 0 then y := Q else y := -MaxDouble;
  end else
  begin
    x := x/zz;
    if x > Q then x := MaxDouble
    else if x < -Q then x := -MaxDouble;

    y := y/zz;
    if y > Q then y := MaxDouble
    else if y < -Q then y := -MaxDouble
  end;
end;
//------------------------------------------------------------------------------

function GetProjectionMatrix(const srcPts, dstPts: TPathD): TMatrixD;
var
  dstMat: TMatrixD;
begin
  if (length(srcPts) <> 4) or (length(dstPts) <> 4) then
  begin
    Result := IdentityMatrix;
    Exit;
  end;
  Result := BasisToPoints(srcPts[0].X, srcPts[0].Y,
    srcPts[1].X, srcPts[1].Y, srcPts[2].X, srcPts[2].Y, srcPts[3].X, srcPts[3].Y);
  dstMat := BasisToPoints(dstPts[0].X, dstPts[0].Y,
    dstPts[1].X, dstPts[1].Y, dstPts[2].X, dstPts[2].Y, dstPts[3].X, dstPts[3].Y);
  MatrixMultiply(Result, MatrixAdjugate(dstMat));
end;
//------------------------------------------------------------------------------

function ProjectiveTransform(img: TImage32;
  const srcPts, dstPts: TPathD; const margins: TRect): Boolean;
begin
  Result := ProjectiveTransform(img, img, srcPts, dstPts, margins);
end;
//------------------------------------------------------------------------------

function ProjectiveTransform(img, targetImg: TImage32;
  const srcPts, dstPts: TPathD; const margins: TRect): Boolean;
var
  w,h,i,j: integer;
  x,y: double;
  xLimLo, yLimLo, xLimHi, yLimHi: double;
  rec: TRect;
  dstPts2: TPathD;
  mat: TMatrixD;
  tmp: TArrayOfColor32;
  pc: PColor32;
  resampler: TResamplerFunction;
begin
  //https://math.stackexchange.com/a/339033/384709

  if targetImg.Resampler = 0 then
    resampler := nil else
    resampler := GetResampler(targetImg.Resampler);

  Result := Assigned(resampler) and not img.IsEmpty and
    (Length(dstPts) = 4) and IsPathConvex(dstPts);
  if not Result then
  begin
    if targetImg <> img then targetImg.Assign(img);
    Exit;
  end;

  rec := GetBounds(dstPts);
  dec(rec.Left, margins.Left);
  dec(rec.Top, margins.Top);
  inc(rec.Right, margins.Right);
  inc(rec.Bottom, margins.Bottom);
  dstPts2 := TranslatePath(dstPts, -rec.Left, -rec.Top);

  xLimLo := -0.5;
  xLimHi := img.Width + 0.5;
  yLimLo := -0.5;
  yLimHi := img.Height + 0.5;

  mat := GetProjectionMatrix(srcPts, dstPts2);
  RectWidthHeight(rec, w, h);
  NewColor32Array(tmp, w * h, True);
  pc := @tmp[0];
  for i :=  0 to h -1 do
    for j := 0 to w -1 do
    begin
      x := j; y := i;
      GetSrcCoords(mat, x, y);

      if (x <= xLimLo) or (x >= xLimHi) or (y <= yLimLo) or (y >= yLimHi) then
        pc^ := clNone32
      else
        pc^ := resampler(img, x -0.5, y -0.5);
      inc(pc);
    end;

  targetImg.BlockNotify;
  targetImg.AssignPixelArray(tmp, w, h);
  targetImg.UnblockNotify;
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
    NewPointDArray(Result, x2-x1, True);
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
    NewPointDArray(Result, x1-x2, True);
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
    NewPointDArray(Result, y2-y1, True);
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
    NewPointDArray(Result, y1-y2, True);
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
    ConcatPaths(Result, tmp);
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
    ConcatPaths(Result, tmp);
  end;
end;
//------------------------------------------------------------------------------

function SplineVertTransform(img: TImage32; const topSpline: TPathD;
  splineType: TSplineType; backColor: TColor32; out offset: TPoint): Boolean;
begin
  Result := SplineVertTransform(img, img, topSpline, splineType, backColor, offset);
end;
//------------------------------------------------------------------------------

function SplineVertTransform(img, targetImg: TImage32; const topSpline: TPathD;
  splineType: TSplineType; backColor: TColor32; out offset: TPoint): Boolean;
var
  i,j, w,h, len: integer;
  x,y, yy, q: double;
  yLimLo, yLimHi: double;
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
  if targetImg.Resampler = 0 then
    resampler := nil else
    resampler := GetResampler(targetImg.Resampler);

  //convert the top spline control points into a flattened path
  if splineType = stQuadratic then
    topPath := FlattenQSpline(topSpline) else
    topPath := FlattenCSpline(topSpline);

  rec := GetBounds(topPath);
  //return false if the spline is invalid or there's no vertical transformation
  Result := Assigned(resampler) and not IsEmptyRect(rec);
  if not Result then
  begin
    if targetImg <> img then targetImg.Assign(img);
    Exit;
  end;

  offset := rec.TopLeft;
  topPath := InterpolatePathForX(topPath);
  len := Length(topPath);
  inc(rec.Bottom, img.Height);
  RectWidthHeight(rec, w, h);
  NewColor32Array(tmp, (w+1) * h, False);

  prevX := topPath[0].X;
  allowBackColoring := GetAlpha(backColor) > 2;
  backColor := backColor and $00FFFFFF;

  distances := GetCumulativeDistances(topPath);
  q := img.Width / distances[High(distances)];

  yLimLo := -0.5;
  yLimHi := img.Height + 0.5;

  for i := 0 to len -1 do
  begin
    pc := @tmp[Round(topPath[i].X)-rec.Left];
    backColoring := allowBackColoring and (prevX >= topPath[i].X);
    prevX := topPath[i].X;
    yy := topPath[i].Y;
    for j := rec.top to rec.bottom -1 do
    begin
      x := Distances[i]*q;
      y := j - yy;
      if (y < yLimLo) or (y > yLimHi) then
        // do nothing !
      else if backColoring then
        pc^ := BlendToAlpha(pc^, ReColor(resampler(img, x -0.5, y -0.5), backColor))
      else
        pc^ := BlendToAlpha(pc^, resampler(img, x -0.5, y -0.5));
      inc(pc, w);
    end;
  end;

  // tmp was creates with "(w+1)*h". We take advantage of the
  // memory manager's inplace shrink.
  SetLength(tmp, w * h);
  targetImg.AssignPixelArray(tmp, w, h);
end;
//------------------------------------------------------------------------------

function SplineHorzTransform(img: TImage32; const leftSpline: TPathD;
  splineType: TSplineType; backColor: TColor32; out offset: TPoint): Boolean;
begin
  Result := SplineHorzTransform(img, img, leftSpline, splineType, backColor, offset);
end;
//------------------------------------------------------------------------------

function SplineHorzTransform(img, targetImg: TImage32; const leftSpline: TPathD;
  splineType: TSplineType; backColor: TColor32; out offset: TPoint): Boolean;
var
  i,j, len, w,h: integer;
  x,y, xx, q, prevY: double;
  xLimLo, xLimHi: double;
  leftPath: TPathD;
  distances: TArrayOfDouble;
  rec: TRect;
  pc: PColor32;
  tmp: TArrayOfColor32;
  backColoring, allowBackColoring: Boolean;
  resampler: TResamplerFunction;
begin
  offset := NullPoint;

  if targetImg.Resampler = 0 then
    resampler := nil else
    resampler := GetResampler(targetImg.Resampler);

  //convert the left spline control points into a flattened path
  if splineType = stQuadratic then
    leftPath := FlattenQSpline(leftSpline) else
    leftPath := FlattenCSpline(leftSpline);
  rec := GetBounds(leftPath);

  //return false if the spline is invalid or there's no horizontal transformation
  Result := Assigned(resampler) and not IsEmptyRect(rec);
  if not Result then
  begin
    if targetImg <> img then targetImg.Assign(img);
    Exit;
  end;

  offset := rec.TopLeft;
  leftPath := InterpolatePathForY(leftPath);
  len := Length(leftPath);
  inc(rec.Right, img.Width);
  RectWidthHeight(rec, w, h);
  NewColor32Array(tmp, w * (h+1), False);

  prevY := leftPath[0].Y;
  allowBackColoring := GetAlpha(backColor) > 2;
  backColor :=   backColor and $00FFFFFF;

  distances := GetCumulativeDistances(leftPath);
  q := img.Height / distances[High(distances)];;
  xLimLo := -0.5;
  xLimHi := img.Width + 0.5;

  for i := 0 to len -1 do
  begin
    pc := @tmp[Round(leftPath[i].Y - rec.Top) * w];
    backColoring := allowBackColoring and (prevY >= leftPath[i].Y);
    prevY := leftPath[i].Y;
    xx := leftPath[i].X;
    y := Distances[i]*q;
    for j := rec.left to rec.right -1 do
    begin
      x := j - xx;

      if (x < xLimLo) or (x > xLimHi) then
        // do nothing !
      else if backColoring then
        pc^ := BlendToAlpha(pc^, ReColor(resampler(img, x -0.5, y -0.5), backColor))
      else
        pc^ := BlendToAlpha(pc^, resampler(img, x -0.5, y -0.5));

      inc(pc);
    end;
  end;

  // tmp was creates with "w*(h+1)". We take advantage of the
  // memory manager's inplace shrink.
  SetLength(tmp, w * h);
  targetImg.AssignPixelArray(tmp, w, h);
end;

//------------------------------------------------------------------------------
// Miscellaneous WeightedColor function
//------------------------------------------------------------------------------

function LimitByte(val: Cardinal): byte; {$IFDEF INLINE} inline; {$ENDIF}
begin
  if val > 255 then result := 255
  else result := val;
end;

//------------------------------------------------------------------------------
// TWeightedColor
//------------------------------------------------------------------------------

procedure TWeightedColor.Reset;
{$IFDEF CPUX64}
var
  Zero: Int64;
{$ENDIF CPUX64}
begin
  {$IFDEF CPUX64}
  Zero := 0;
  fAddCount := Zero;
  fAlphaTot := Zero;
  fColorTotR := Zero;
  fColorTotG := Zero;
  fColorTotB := Zero;
  {$ELSE}
  fAddCount := 0;
  fAlphaTot := 0;
  fColorTotR := 0;
  fColorTotG := 0;
  fColorTotB := 0;
  {$ENDIF CPUX64}
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.Reset(c: TColor32; w: Integer);
var
  a: Cardinal;
begin
  fAddCount := w;
  a := w * Byte(c shr 24);
  if a = 0 then
  begin
    fAlphaTot := 0;
    fColorTotB := 0;
    fColorTotG := 0;
    fColorTotR := 0;
  end else
  begin
    fAlphaTot := a;
    fColorTotB := (a * Byte(c));
    fColorTotG := (a * Byte(c shr 8));
    fColorTotR := (a * Byte(c shr 16));
  end;
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.AddWeight(w: Integer);
begin
  inc(fAddCount, w);
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.Add(c: TColor32; w: Integer);
var
  a: Int64;
begin
  inc(fAddCount, w);
  a := Byte(c shr 24);
  if a <> 0 then
  begin
    a := a * Cardinal(w);
    inc(fAlphaTot, a);
    inc(fColorTotB, (a * Byte(c)));
    inc(fColorTotG, (a * Byte(c shr 8)));
    inc(fColorTotR, (a * Byte(c shr 16)));
  end;
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.Add(c: TColor32);
// Optimized for w=1
var
  a: Int64;
begin
  inc(fAddCount);
  a := Byte(c shr 24);
  if a = 0 then Exit;
  inc(fAlphaTot, a);
  inc(fColorTotB, (a * Byte(c)));
  inc(fColorTotG, (a * Byte(c shr 8)));
  inc(fColorTotR, (a * Byte(c shr 16)));
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
  a: Int64;
begin
  dec(fAddCount, w);
  a := w * Byte(c shr 24);
  if a = 0 then Exit;
  dec(fAlphaTot, a);
  dec(fColorTotB, (a * Byte(c)));
  dec(fColorTotG, (a * Byte(c shr 8)));
  dec(fColorTotR, (a * Byte(c shr 16)));
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.Subtract(c: TColor32);
// Optimized for w=1
var
  a: Int64;
begin
  dec(fAddCount);
  a := Byte(c shr 24);
  if a = 0 then Exit;
  dec(fAlphaTot, a);
  dec(fColorTotB, (a * Byte(c)));
  dec(fColorTotG, (a * Byte(c shr 8)));
  dec(fColorTotR, (a * Byte(c shr 16)));
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

function TWeightedColor.AddSubtract(addC, subC: TColor32): Boolean;
var
  a: Int64;
begin
  // add+subtract => fAddCount stays the same

  // skip identical colors
  Result := False;
  if addC = subC then Exit;

  a := Byte(addC shr 24);
  if a > 0 then
  begin
    inc(fAlphaTot, a);
    inc(fColorTotB, (a * Byte(addC)));
    inc(fColorTotG, (a * Byte(addC shr 8)));
    inc(fColorTotR, (a * Byte(addC shr 16)));
    Result := True;
  end;

  a := Byte(subC shr 24);
  if a > 0 then
  begin
    dec(fAlphaTot, a);
    dec(fColorTotB, (a * Byte(subC)));
    dec(fColorTotG, (a * Byte(subC shr 8)));
    dec(fColorTotR, (a * Byte(subC shr 16)));
    Result := True;
  end;
end;
//------------------------------------------------------------------------------

function TWeightedColor.AddNoneSubtract(c: TColor32): Boolean;
var
  a: Int64;
begin
  // add+subtract => fAddCount stays the same

  a := Byte(c shr 24);
  if a > 0 then
  begin
    dec(fAlphaTot, a);
    dec(fColorTotB, (a * Byte(c)));
    dec(fColorTotG, (a * Byte(c shr 8)));
    dec(fColorTotR, (a * Byte(c shr 16)));
    Result := True;
  end
  else
    Result := False;
end;
//------------------------------------------------------------------------------

function TWeightedColor.GetColor: TColor32;
var
  oneDivAlphaTot: double;
  alpha: Integer;
begin
  result := clNone32;
  if (fAlphaTot <= 0) or (fAddCount <= 0) then
    Exit;
  {$IFDEF CPUX86}
  if fAlphaTot and $FFFFFFFF80000000 = 0 then // small, so can avoid _lldiv call
    alpha := (Cardinal(fAlphaTot) + (Cardinal(fAddCount) shr 1)) div
      Cardinal(fAddCount)
  else
  {$ENDIF CPUX86}
    alpha := (fAlphaTot + (Cardinal(fAddCount) shr 1)) div Cardinal(fAddCount);

  result := TColor32(Min(255, alpha)) shl 24;
  // alpha weighting has been applied to color channels, so div by fAlphaTot

  if fAlphaTot < DivOneByXTableSize then // use precalculated 1/X values
    oneDivAlphaTot := DivOneByXTable[fAlphaTot] else
    oneDivAlphaTot := 1/(fAlphaTot);

  // 1. Skip zero calculations.
  // 2. LimitByte(Integer): Values can't be less than 0, so don't use ClampByte.
  // 3. x86: Round expects the value in the st(0)/xmm1 FPU register.
  //         Thus we need to do the calculation and Round call in one expression.
  //         Otherwise the compiler will use a temporary double variable on
  //         the stack that will cause unnecessary store and load operations.
  if fColorTotB > 0 then
    result := result or LimitByte(System.Round(fColorTotB * oneDivAlphaTot));
  if fColorTotG > 0 then
    result := result or LimitByte(System.Round(fColorTotG * oneDivAlphaTot)) shl 8;
  if fColorTotR > 0 then
    result := result or LimitByte(System.Round(fColorTotR * oneDivAlphaTot)) shl 16;
end;

//------------------------------------------------------------------------------
// Initialization
//------------------------------------------------------------------------------

procedure MakeDivOneByXTable;
var
  i: Integer;
begin
  DivOneByXTable[0] := 0; // NaN
  for i := 1 to High(DivOneByXTable) do
    DivOneByXTable[i] := 1/i;
end;

initialization
  MakeDivOneByXTable;

end.
