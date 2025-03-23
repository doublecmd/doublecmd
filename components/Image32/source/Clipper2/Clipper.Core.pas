unit Clipper.Core;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  22 November 2024                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  Core Clipper Library module                                     *
*              Contains structures and functions used throughout the library   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

{$I Clipper.inc}

interface

uses
  SysUtils, Classes, Math;

type
{$IFDEF USINGZ}
    ZType = Int64; // or alternatively, ZType = double
{$ENDIF}

  PPoint64  = ^TPoint64;
  TPoint64  = record
    X, Y: Int64;
{$IFDEF USINGZ}
    Z: ZType;
{$ENDIF}
  end;

  PPointD   = ^TPointD;
  TPointD   = record
    X, Y: double;
{$IFDEF USINGZ}
    Z: ZType;
{$ENDIF}
  end;

  // Path: a simple data structure representing a series of vertices, whether
  // open (poly-line) or closed (polygon). Paths may be simple or complex (self
  // intersecting). For simple polygons, consisting of a single non-intersecting
  // path, path orientation is unimportant. However, for complex polygons and
  // for overlapping polygons, various 'filling rules' define which regions will
  // be inside (filled) and which will be outside (unfilled).

  TPath64  = array of TPoint64;
  TPaths64 = array of TPath64;
  TArrayOfPaths = array of TPaths64;

  TPathD = array of TPointD;
  TPathsD = array of TPathD;
  TArrayOfPathsD = array of TPathsD;

  // The most commonly used filling rules for polygons are EvenOdd and NonZero.
  // https://en.wikipedia.org/wiki/Even-odd_rule
  // https://en.wikipedia.org/wiki/Nonzero-rule
  TFillRule = (frEvenOdd, frNonZero, frPositive, frNegative);

  TArrayOfBoolean = array of Boolean;
  TArrayOfInteger = array of Integer;
  TArrayOfDouble = array of double;

  TRect64 = {$IFDEF RECORD_METHODS}record{$ELSE}object{$ENDIF}
  private
    function GetWidth: Int64; {$IFDEF INLINING} inline; {$ENDIF}
    function GetHeight: Int64; {$IFDEF INLINING} inline; {$ENDIF}
    function GetIsEmpty: Boolean; {$IFDEF INLINING} inline; {$ENDIF}
    function GetIsValid: Boolean; {$IFDEF INLINING} inline; {$ENDIF}
    function GetMidPoint: TPoint64; {$IFDEF INLINING} inline; {$ENDIF}
  public
    Left   : Int64;
    Top    : Int64;
    Right  : Int64;
    Bottom : Int64;
    function Contains(const pt: TPoint64; inclusive: Boolean = false): Boolean; overload;
    function Contains(const rec: TRect64): Boolean; overload;
    function Intersect(const rec: TRect64): TRect64;
    function Intersects(const rec: TRect64): Boolean;
    function AsPath: TPath64;
    property Width: Int64 read GetWidth;
    property Height: Int64 read GetHeight;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsValid: Boolean read GetIsValid;
    property MidPoint: TPoint64 read GetMidPoint;
  end;

  TRectD = {$ifdef RECORD_METHODS}record{$else}object{$endif}
  private
    function GetWidth: double; {$IFDEF INLINING} inline; {$ENDIF}
    function GetHeight: double; {$IFDEF INLINING} inline; {$ENDIF}
    function GetIsEmpty: Boolean; {$IFDEF INLINING} inline; {$ENDIF}
    function GetIsValid: Boolean; {$IFDEF INLINING} inline; {$ENDIF}
    function GetMidPoint: TPointD; {$IFDEF INLINING} inline; {$ENDIF}
  public
    Left   : double;
    Top    : double;
    Right  : double;
    Bottom : double;
    function Contains(const pt: TPointD): Boolean; overload;
    function Contains(const rec: TRectD): Boolean; overload;
    function Intersects(const rec: TRectD): Boolean;
    function AsPath: TPathD;
    property Width: double read GetWidth;
    property Height: double read GetHeight;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsValid: Boolean read GetIsValid;
    property MidPoint: TPointD read GetMidPoint;
  end;

{$IFDEF FPC}
  TPointerList = array of Pointer;
  TListSortCompareFunc = function (Item1, Item2: Pointer): Integer;
{$ELSE}
{$IF COMPILERVERSION < 23} //PRIOR DELPHI XE2
  TPointerList = array of Pointer;
  TListSortCompareFunc = function (Item1, Item2: Pointer): Integer;
{$IFEND}
{$ENDIF}

  TListEx = class
  private
    fCount    : integer;
    fCapacity : integer;
    fList     : TPointerList;
    fSorted   : Boolean;
  protected
    function UnsafeGet(idx: integer): Pointer; // no range checking
    procedure UnsafeSet(idx: integer; val: Pointer);
    procedure UnsafeDelete(index: integer); virtual;
  public
    constructor Create(capacity: integer = 0); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Add(item: Pointer): integer;
    procedure DeleteLast;
    procedure Swap(idx1, idx2: integer);
    procedure Sort(Compare: TListSortCompareFunc);
    procedure Resize(count: integer);
    property Count: integer read fCount;
    property Sorted: Boolean read fSorted;
    property Item[idx: integer]: Pointer read UnsafeGet; default;
  end;

  TClipType = (ctNoClip, ctIntersection, ctUnion, ctDifference, ctXor);

  TPointInPolygonResult = (pipOn, pipInside, pipOutside);

  EClipper2LibException = class(Exception);

function Area(const path: TPath64): Double; overload;
function Area(const paths: TPaths64): Double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function Area(const path: TPathD): Double; overload;
function Area(const paths: TPathsD): Double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function IsPositive(const path: TPath64): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function IsPositive(const path: TPathD): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function IsCollinear(const pt1, sharedPt, pt2: TPoint64): Boolean;

function CrossProduct(const pt1, pt2, pt3: TPoint64): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function CrossProduct(const pt1, pt2, pt3: TPointD): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function CrossProduct(const vec1, vec2: TPointD): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function CrossProduct(vec1x, vec1y, vec2x, vec2y: double): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function DotProduct(const pt1, pt2, pt3: TPoint64): double;
  {$IFDEF INLINING} inline; {$ENDIF}

function DistanceSqr(const pt1, pt2: TPoint64): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function DistanceSqr(const pt1, pt2: TPointD): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function PerpendicDistFromLineSqrd(const pt, linePt1, linePt2: TPoint64): double; overload;
function PerpendicDistFromLineSqrd(const pt, linePt1, linePt2: TPointD): double; overload;

function SegmentsIntersect(const s1a, s1b, s2a, s2b: TPoint64;
  inclusive: Boolean = false): boolean; {$IFDEF INLINING} inline; {$ENDIF}

function PointsEqual(const pt1, pt2: TPoint64): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function PointsNearEqual(const pt1, pt2: TPointD): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function PointsNearEqual(const pt1, pt2: TPointD; distanceSqrd: double): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

{$IFDEF USINGZ}
function Point64(const X, Y: Int64; Z: ZType = 0): TPoint64; overload;
{$IFDEF INLINING} inline; {$ENDIF}
function Point64(const X, Y: Double; Z: ZType = 0): TPoint64; overload;
{$IFDEF INLINING} inline; {$ENDIF}
function PointD(const X, Y: Double; Z: ZType = 0): TPointD; overload;
{$IFDEF INLINING} inline; {$ENDIF}
{$ELSE}
function Point64(const X, Y: Int64): TPoint64; overload; {$IFDEF INLINING} inline; {$ENDIF}
function Point64(const X, Y: Double): TPoint64; overload; {$IFDEF INLINING} inline; {$ENDIF}
function PointD(const X, Y: Double): TPointD; overload; {$IFDEF INLINING} inline; {$ENDIF}
{$ENDIF}

function Negate(const pt: TPoint64): TPoint64; overload; {$IFDEF INLINING} inline; {$ENDIF}
function Negate(const pt: TPointD): TPointD; overload; {$IFDEF INLINING} inline; {$ENDIF}
function NegatePath(const path: TPathD): TPathD; overload; {$IFDEF INLINING} inline; {$ENDIF}

function Point64(const pt: TPointD): TPoint64; overload; {$IFDEF INLINING} inline; {$ENDIF}
function PointD(const pt: TPoint64): TPointD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function Rect64(const left, top, right, bottom: Int64): TRect64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function Rect64(const recD: TRectD): TRect64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function RectD(const left, top, right, bottom: double): TRectD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function RectD(const rec64: TRect64): TRectD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function GetBounds(const paths: TArrayOfPaths): TRect64; overload;
function GetBounds(const paths: TPaths64): TRect64; overload;
function GetBounds(const paths: TPathsD): TRectD; overload;
function GetBounds(const path: TPath64): TRect64; overload;
function GetBounds(const path: TPathD): TRectD; overload;

function TranslatePoint(const pt: TPoint64; dx, dy: Int64): TPoint64; overload;
function TranslatePoint(const pt: TPointD; dx, dy: double): TPointD; overload;

procedure RotatePt(var pt: TPointD; const center: TPointD; sinA, cosA: double);
procedure RotatePath(var path: TPathD; const center: TPointD; sinA, cosA: double);

procedure InflateRect(var rec: TRect64; dx, dy: Int64); overload;
  {$IFDEF INLINING} inline; {$ENDIF}
procedure InflateRect(var rec: TRectD; dx, dy: double); overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function  UnionRect(const rec, rec2: TRect64): TRect64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function  UnionRect(const rec, rec2: TRectD): TRectD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function  RotateRect(const rec: TRect64; angleRad: double): TRect64; overload;
function  RotateRect(const rec: TRectD; angleRad: double): TRectD; overload;
procedure OffsetRect(var rec: TRect64; dx, dy: Int64); overload;
  {$IFDEF INLINING} inline; {$ENDIF}
procedure OffsetRect(var rec: TRectD; dx, dy: double); overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function ScaleRect(const rec: TRect64; scale: double): TRect64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function ScaleRect(const rec: TRectD; scale: double): TRectD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function ScalePoint(const pt: TPoint64; scale: double): TPointD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function ScalePoint(const pt: TPointD; scale: double): TPointD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function ScalePath(const path: TPath64; sx, sy: double): TPath64; overload;
function ScalePath(const path: TPathD; sx, sy: double): TPath64; overload;
function ScalePath(const path: TPath64; scale: double): TPath64; overload;
function ScalePath(const path: TPathD; scale: double): TPath64; overload;

function ScalePathD(const path: TPath64; sx, sy: double): TPathD; overload;
function ScalePathD(const path: TPathD; sx, sy: double): TPathD; overload;
function ScalePathD(const path: TPath64; scale: double): TPathD; overload;
function ScalePathD(const path: TPathD; scale: double): TPathD; overload;

function ScalePaths(const paths: TPaths64; sx, sy: double): TPaths64; overload;
function ScalePaths(const paths: TPathsD; sx, sy: double): TPaths64; overload;
function ScalePaths(const paths: TPaths64; scale: double): TPaths64; overload;
function ScalePaths(const paths: TPathsD; scale: double): TPaths64; overload;

function ScalePathsD(const paths: TPaths64; sx, sy: double): TPathsD; overload;
function ScalePathsD(const paths: TPathsD; sx, sy: double): TPathsD; overload;
function ScalePathsD(const paths: TPaths64; scale: double): TPathsD; overload;
function ScalePathsD(const paths: TPathsD; scale: double): TPathsD; overload;

function Path64(const pathD: TPathD): TPath64;
function PathD(const path: TPath64): TPathD;
function Paths64(const path: TPath64): TPaths64; overload;
function Paths64(const pathsD: TPathsD): TPaths64; overload;
function PathsD(const paths: TPaths64): TPathsD; overload;
function PathsD(const path: TPathD): TPathsD; overload;

function StripDuplicates(const path: TPath64; isClosedPath: Boolean = false): TPath64;
function StripNearDuplicates(const path: TPathD;
  minLenSqrd: double; isClosedPath: Boolean): TPathD;

function ValueBetween(val, end1, end2: Int64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
function ValueEqualOrBetween(val, end1, end2: Int64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}

function ReversePath(const path: TPath64): TPath64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function ReversePath(const path: TPathD): TPathD; overload;
function ReversePaths(const paths: TPaths64): TPaths64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function ReversePaths(const paths: TPathsD): TPathsD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function ShiftPath(const path: TPath64; shift: integer): TPath64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
function ShiftPath(const path: TPathD; shift: integer): TPathD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

procedure AppendPoint(var path: TPath64; const pt: TPoint64); overload;
  {$IFDEF INLINING} inline; {$ENDIF}
procedure AppendPoint(var path: TPathD; const pt: TPointD); overload;
  {$IFDEF INLINING} inline; {$ENDIF}

function AppendPoints(const path, extra: TPath64): TPath64;
  {$IFDEF INLINING} inline; {$ENDIF}

procedure AppendPath(var paths: TPaths64; const extra: TPath64); overload;
procedure AppendPath(var paths: TPathsD; const extra: TPathD); overload;
procedure AppendPaths(var paths: TPaths64; const extra: TPaths64); overload;
procedure AppendPaths(var paths: TPathsD; const extra: TPathsD); overload;

function ArrayOfPathsToPaths(const ap: TArrayOfPaths): TPaths64;

function GetSegmentIntersectPt(const ln1a, ln1b, ln2a, ln2b: TPoint64;
  out ip: TPoint64): Boolean;

function PointInPolygon(const pt: TPoint64; const polygon: TPath64): TPointInPolygonResult;

function GetClosestPointOnSegment(const pt, seg1, seg2: TPoint64): TPoint64;
  {$IFDEF INLINING} inline; {$ENDIF}

function RamerDouglasPeucker(const path: TPath64; epsilon: double): TPath64; overload;
function RamerDouglasPeucker(const paths: TPaths64; epsilon: double): TPaths64; overload;
function RamerDouglasPeucker(const path: TPathD; epsilon: double): TPathD; overload;
function RamerDouglasPeucker(const paths: TPathsD; epsilon: double): TPathsD; overload;

procedure GetSinCos(angle: double; out sinA, cosA: double);
function Ellipse(const rec: TRect64; steps: integer = 0): TPath64; overload;
function Ellipse(const rec: TRectD; steps: integer = 0): TPathD; overload;

procedure QuickSort(SortList: TPointerList;
  L, R: Integer; const SCompare: TListSortCompareFunc);

procedure CheckPrecisionRange(var precision: integer);

function Iif(eval: Boolean; trueVal, falseVal: Boolean): Boolean; overload;
function Iif(eval: Boolean; trueVal, falseVal: integer): integer; overload;
function Iif(eval: Boolean; trueVal, falseVal: Int64): Int64; overload;
function Iif(eval: Boolean; trueVal, falseVal: double): double; overload;

const
  MaxInt64    = 9223372036854775807;
  MinInt64    = -MaxInt64;
  MaxCoord    = MaxInt64 div 4;
  MinCoord    = - MaxCoord;
  invalid64   = MaxInt64;
  invalidD    = infinity;

  NullPointD  : TPointD = (X: 0; Y: 0);
  NullRect64  : TRect64 = (left: 0; top: 0; right: 0; Bottom: 0);
  InvalidPt64 : TPoint64 = (X: invalid64; Y: invalid64);
  InvalidPtD :  TPointD = (X: invalidD; Y: invalidD);

  NullRectD   : TRectD = (left: 0; top: 0; right: 0; Bottom: 0);
  InvalidRect64 : TRect64 =
    (left: invalid64; top: invalid64; right: invalid64; bottom: invalid64);
  InvalidRectD : TRectD =
    (left: invalidD; top: invalidD; right: invalidD; bottom: invalidD);

  Tolerance   : Double = 1.0E-12;

  //https://github.com/AngusJohnson/Clipper2/discussions/564
  MaxDecimalPrecision = 8;

implementation

resourcestring
  rsClipper_PrecisonErr = 'The decimal rounding value is invalid';

//------------------------------------------------------------------------------
// TRect64 methods ...
//------------------------------------------------------------------------------

function TRect64.GetWidth: Int64;
begin
  result := right - left;
end;
//------------------------------------------------------------------------------

function TRect64.GetHeight: Int64;
begin
  result := bottom - top;
end;
//------------------------------------------------------------------------------

function TRect64.GetIsEmpty: Boolean;
begin
  result := (bottom <= top) or (right <= left);
end;
//------------------------------------------------------------------------------

function TRect64.GetIsValid: Boolean;
begin
  result := left <> invalid64;
end;
//------------------------------------------------------------------------------

function TRect64.GetMidPoint: TPoint64;
begin
  result := Point64((Left + Right) div 2, (Top + Bottom) div 2);
end;
//------------------------------------------------------------------------------

function TRect64.Contains(const pt: TPoint64; inclusive: Boolean = false): Boolean;
begin
  if inclusive then
    result := (pt.X >= Left) and (pt.X <= Right) and
      (pt.Y >= Top) and (pt.Y <= Bottom)
  else
    result := (pt.X > Left) and (pt.X < Right) and
      (pt.Y > Top) and (pt.Y < Bottom);
end;
//------------------------------------------------------------------------------

function TRect64.Contains(const rec: TRect64): Boolean;
begin
  result := (rec.Left >= Left) and (rec.Right <= Right) and
    (rec.Top >= Top) and (rec.Bottom <= Bottom);
end;
//------------------------------------------------------------------------------

function TRect64.Intersects(const rec: TRect64): Boolean;
begin
  Result := (Max(Left, rec.Left) <= Min(Right, rec.Right)) and
    (Max(Top, rec.Top) <= Min(Bottom, rec.Bottom));
end;
//------------------------------------------------------------------------------

function TRect64.Intersect(const rec: TRect64): TRect64;
begin
  Result.Left := Max(Left, rec.Left);
  Result.Top := Max(Top, rec.Top);
  Result.Right := Min(Right, rec.Right);
  Result.Bottom := Min(Bottom, rec.Bottom);
  if IsEmpty then Result := NullRect64;
end;
//------------------------------------------------------------------------------

function TRect64.AsPath: TPath64;
begin
  SetLength(Result, 4);
  Result[0] := Point64(Left, Top);
  Result[1] := Point64(Right, Top);
  Result[2] := Point64(Right, Bottom);
  Result[3] := Point64(Left, Bottom);
end;

//------------------------------------------------------------------------------
// TRectD methods ...
//------------------------------------------------------------------------------

function TRectD.GetWidth: double;
begin
  result := right - left;
end;
//------------------------------------------------------------------------------

function TRectD.GetHeight: double;
begin
  result := bottom - top;
end;
//------------------------------------------------------------------------------

function TRectD.GetIsEmpty: Boolean;
begin
  result := (bottom <= top) or (right <= left);
end;
//------------------------------------------------------------------------------

function TRectD.GetIsValid: Boolean;
begin
  result := left <> invalidD;
end;
//------------------------------------------------------------------------------

function TRectD.GetMidPoint: TPointD;
begin
  result := PointD((Left + Right) *0.5, (Top + Bottom) *0.5);
end;
//------------------------------------------------------------------------------

function TRectD.Contains(const pt: TPointD): Boolean;
begin
  result := (pt.X > Left) and (pt.X < Right) and
    (pt.Y > Top) and (pt.Y < Bottom);
end;
//------------------------------------------------------------------------------

function TRectD.Contains(const rec: TRectD): Boolean;
begin
  result := (rec.Left >= Left) and (rec.Right <= Right) and
    (rec.Top >= Top) and (rec.Bottom <= Bottom);
end;
//------------------------------------------------------------------------------

function TRectD.Intersects(const rec: TRectD): Boolean;
begin
  Result := (Max(Left, rec.Left) <= Min(Right, rec.Right)) and
    (Max(Top, rec.Top) <= Min(Bottom, rec.Bottom));
end;
//------------------------------------------------------------------------------

function TRectD.AsPath: TPathD;
begin
  SetLength(Result, 4);
  Result[0] := PointD(Left, Top);
  Result[1] := PointD(Right, Top);
  Result[2] := PointD(Right, Bottom);
  Result[3] := PointD(Left, Bottom);
end;

//------------------------------------------------------------------------------
// TListEx class
//------------------------------------------------------------------------------

constructor TListEx.Create(capacity: integer);
begin
  if capacity > 0 then
  begin
    fCapacity := 16;
    while capacity > fCapacity do fCapacity := fCapacity * 2;
    SetLength(fList, fCapacity);
  end;
end;
//------------------------------------------------------------------------------

destructor TListEx.Destroy;
begin
  Clear;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TListEx.Clear;
begin
  fList := nil;
  fCount := 0;
  fCapacity := 0;
  fSorted := false;
end;
//------------------------------------------------------------------------------

function TListEx.Add(item: Pointer): integer;
begin
  if fCount = fCapacity then
  begin
    if fCapacity = 0 then
      fCapacity := 16 else
      fCapacity := fCapacity *2;
    SetLength(fList, fCapacity);
  end;
  fList[fCount] := item;
  Result := fCount;
  inc(fCount);
  fSorted := false;
end;
//------------------------------------------------------------------------------

procedure TListEx.DeleteLast;
begin
  dec(fCount);
end;
//------------------------------------------------------------------------------

procedure QuickSort(SortList: TPointerList; L, R: Integer;
  const SCompare: TListSortCompareFunc);
var
  I, J: Integer;
  P, T: Pointer;
begin
  if L >= R then Exit;

  repeat
    if (R - L) = 1 then
    begin
      if SCompare(SortList[L], SortList[R]) > 0 then
      begin
        T := SortList[L];
        SortList[L] := SortList[R];
        SortList[R] := T;
      end;
      break;
    end;

    I := L;
    J := R;
    P := SortList[(L + R) shr 1];
    repeat
      while SCompare(SortList[I], P) < 0 do Inc(I);
      while SCompare(SortList[J], P) > 0 do Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          T := SortList[I];
          SortList[I] := SortList[J];
          SortList[J] := T;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;

    if (J - L) > (R - I) then
    begin
      if I < R then QuickSort(SortList, I, R, SCompare);
      R := J;
    end
    else
    begin
      if L < J then QuickSort(SortList, L, J, SCompare);
      L := I;
    end;
  until L >= R;
end;
//------------------------------------------------------------------------------

procedure TListEx.Sort(Compare: TListSortCompareFunc);
begin
  if fCount < 2 then Exit;
  QuickSort(FList, 0, fCount - 1, Compare);
  fSorted := true;
end;
//------------------------------------------------------------------------------

procedure TListEx.Resize(count: integer);
begin
  if (fCapacity = 0) then fCapacity := 16;
  while count > fCapacity do fCapacity := fCapacity * 2;
  SetLength(fList, fCapacity);
  fCount := count;
end;
//------------------------------------------------------------------------------

function TListEx.UnsafeGet(idx: integer): Pointer;
begin
  Result := fList[idx];
end;
//------------------------------------------------------------------------------

procedure TListEx.UnsafeSet(idx: integer; val: Pointer);
begin
  fList[idx] := val;
end;
//------------------------------------------------------------------------------

procedure TListEx.UnsafeDelete(index: integer);
begin
  dec(fCount);
  if index < fCount then
    Move(fList[index +1], fList[index], (fCount - index) * SizeOf(Pointer));
end;
//------------------------------------------------------------------------------

procedure TListEx.Swap(idx1, idx2: integer);
var
  p: Pointer;
begin
  p := fList[idx1];
  fList[idx1] := fList[idx2];
  fList[idx2] := p;
  fSorted := false;
end;

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

function Iif(eval: Boolean; trueVal, falseVal: Boolean): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if eval then Result := trueVal else Result := falseVal;
end;
//------------------------------------------------------------------------------

function Iif(eval: Boolean; trueVal, falseVal: integer): integer;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if eval then Result := trueVal else Result := falseVal;
end;
//------------------------------------------------------------------------------

function Iif(eval: Boolean; trueVal, falseVal: Int64): Int64;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if eval then Result := trueVal else Result := falseVal;
end;
//------------------------------------------------------------------------------

function Iif(eval: Boolean; trueVal, falseVal: double): double;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if eval then Result := trueVal else Result := falseVal;
end;
//------------------------------------------------------------------------------

procedure CheckPrecisionRange(var precision: integer);
begin
  if (precision < -MaxDecimalPrecision) or (precision > MaxDecimalPrecision) then
      Raise EClipper2LibException(rsClipper_PrecisonErr);
end;
//------------------------------------------------------------------------------

procedure RaiseError(const msg: string); {$IFDEF INLINING} inline; {$ENDIF}
begin
  raise EClipper2LibException.Create(msg);
end;
//------------------------------------------------------------------------------

function PointsEqual(const pt1, pt2: TPoint64): Boolean;
begin
  Result := (pt1.X = pt2.X) and (pt1.Y = pt2.Y);
end;
//------------------------------------------------------------------------------

function PointsNearEqual(const pt1, pt2: TPointD): Boolean;
begin
  Result := (Abs(pt1.X - pt2.X) < Tolerance) and
    (Abs(pt1.Y - pt2.Y) < Tolerance);
end;
//------------------------------------------------------------------------------

function PointsNearEqual(const pt1, pt2: TPointD; distanceSqrd: double): Boolean;
begin
  Result := Sqr(pt1.X - pt2.X) + Sqr(pt1.Y - pt2.Y) < distanceSqrd;
end;
//------------------------------------------------------------------------------

function StripDuplicates(const path: TPath64; isClosedPath: Boolean): TPath64;
var
  i,j, len: integer;
begin
  len := length(path);
  SetLength(Result, len);
  if len = 0 then Exit;
  Result[0] := path[0];
  j := 0;
  for i := 1 to len -1 do
    if not PointsEqual(Result[j], path[i]) then
    begin
      inc(j);
      Result[j] := path[i];
    end;
  if isClosedPath and PointsEqual(Result[0], path[j]) then dec(j);
  SetLength(Result, j +1);
end;
//------------------------------------------------------------------------------

function StripNearDuplicates(const path: TPathD;
  minLenSqrd: double; isClosedPath: Boolean): TPathD;
var
  i,j, len: integer;
begin
  len := length(path);
  SetLength(Result, len);
  if len = 0 then Exit;

  Result[0] := path[0];
  j := 0;
  for i := 1 to len -1 do
    if not PointsNearEqual(Result[j], path[i], minLenSqrd) then
    begin
      inc(j);
      Result[j] := path[i];
    end;

  if isClosedPath and
    PointsNearEqual(Result[j], Result[0], minLenSqrd) then dec(j);
  SetLength(Result, j +1);
end;
//------------------------------------------------------------------------------

function ValueBetween(val, end1, end2: Int64): Boolean;
begin
  // nb: accommodates axis aligned between where end1 == end2
  Result := ((val <> end1) = (val <> end2)) and
    ((val > end1) = (val < end2));
end;
//------------------------------------------------------------------------------

function ValueEqualOrBetween(val, end1, end2: Int64): Boolean;
begin
  Result := (val = end1) or (val = end2) or
    ((val > end1) = (val < end2));
end;
//------------------------------------------------------------------------------

function ScaleRect(const rec: TRect64; scale: double): TRect64;
begin
  Result.Left := Round(rec.Left * scale);
  Result.Top := Round(rec.Top * scale);
  Result.Right := Round(rec.Right * scale);
  Result.Bottom := Round(rec.Bottom * scale);
end;
//------------------------------------------------------------------------------

function ScaleRect(const rec: TRectD; scale: double): TRectD;
begin
  Result.Left := rec.Left * scale;
  Result.Top := rec.Top * scale;
  Result.Right := rec.Right * scale;
  Result.Bottom := rec.Bottom * scale;
end;
//------------------------------------------------------------------------------

function ScalePoint(const pt: TPoint64; scale: double): TPointD;
begin
  Result.X := pt.X * scale;
  Result.Y := pt.Y * scale;
{$IFDEF USINGZ}
  Result.Z := pt.Z;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function ScalePoint(const pt: TPointD; scale: double): TPointD;
begin
  Result.X := pt.X * scale;
  Result.Y := pt.Y * scale;
{$IFDEF USINGZ}
  Result.Z := pt.Z;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function ScalePath(const path: TPath64; sx, sy: double): TPath64;
var
  i,len: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  len := length(path);
  setlength(result, len);
  for i := 0 to len -1 do
  begin
    result[i].X := Round(path[i].X * sx);
    result[i].Y := Round(path[i].Y * sy);
{$IFDEF USINGZ}
    result[i].Z := path[i].Z;
{$ENDIF}
  end;
end;
//------------------------------------------------------------------------------

function ScalePath(const path: TPathD; sx, sy: double): TPath64;
var
  i,j, len: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  len := length(path);
  setlength(result, len);
  if len = 0 then Exit;
  j := 1;
  result[0].X := Round(path[0].X * sx);
  result[0].Y := Round(path[0].Y * sy);
{$IFDEF USINGZ}
  result[0].Z := path[0].Z;
{$ENDIF}
  for i := 1 to len -1 do
  begin
    result[j].X := Round(path[i].X * sx);
    result[j].Y := Round(path[i].Y * sy);
{$IFDEF USINGZ}
    result[j].Z := path[i].Z;
{$ENDIF}
    if (result[j].X <> result[j-1].X) or
      (result[j].Y <> result[j-1].Y) then inc(j);
  end;
  setlength(result, j);
end;
//------------------------------------------------------------------------------

function ScalePath(const path: TPath64; scale: double): TPath64;
var
  i,j, len: integer;
begin
  len := length(path);
  setlength(result, len);
  if len = 0 then Exit;
  j := 1;
  result[0].X := Round(path[0].X * scale);
  result[0].Y := Round(path[0].Y * scale);
{$IFDEF USINGZ}
  result[0].Z := path[0].Z;
{$ENDIF}
  for i := 1 to len -1 do
  begin
    result[j].X := Round(path[i].X * scale);
    result[j].Y := Round(path[i].Y * scale);
{$IFDEF USINGZ}
    result[j].Z := path[i].Z;
{$ENDIF}
    if (result[j].X <> result[j-1].X) or
      (result[j].Y <> result[j-1].Y) then inc(j);
  end;
  setlength(result, j);
end;
//------------------------------------------------------------------------------

function ScalePath(const path: TPathD; scale: double): TPath64;
var
  i,len: integer;
begin
  len := length(path);
  setlength(result, len);
  for i := 0 to len -1 do
  begin
    result[i].X := Round(path[i].X * scale);
    result[i].Y := Round(path[i].Y * scale);
{$IFDEF USINGZ}
    result[i].Z := path[i].Z;
{$ENDIF}
  end;
end;
//------------------------------------------------------------------------------

function ScalePaths(const paths: TPaths64; sx, sy: double): TPaths64;
var
  i,len: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  len := length(paths);
  setlength(result, len);
  for i := 0 to len -1 do
    result[i] := ScalePath(paths[i], sx, sy);
end;
//------------------------------------------------------------------------------

function ScalePaths(const paths: TPathsD; sx, sy: double): TPaths64;
var
  i,len: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  len := length(paths);
  setlength(result, len);
  for i := 0 to len -1 do
    result[i] := ScalePath(paths[i], sx, sy);
end;
//------------------------------------------------------------------------------

function ScalePathD(const path: TPath64; sx, sy: double): TPathD;
var
  i: integer;
begin
  setlength(result, length(path));
  for i := 0 to high(path) do
  begin
    result[i].X := path[i].X * sx;
    result[i].Y := path[i].Y * sy;
{$IFDEF USINGZ}
    result[i].Z := path[i].Z;
{$ENDIF}
  end;
end;
//------------------------------------------------------------------------------

function ScalePathD(const path: TPathD; sx, sy: double): TPathD;
var
  i: integer;
begin
  setlength(result, length(path));
  for i := 0 to high(path) do
  begin
    result[i].X := path[i].X * sx;
    result[i].Y := path[i].Y * sy;
{$IFDEF USINGZ}
    result[i].Z := path[i].Z;
{$ENDIF}
  end;
end;
//------------------------------------------------------------------------------

function ScalePathD(const path: TPath64; scale: double): TPathD;
var
  i: integer;
begin
  setlength(result, length(path));
  for i := 0 to high(path) do
  begin
    result[i].X := path[i].X * scale;
    result[i].Y := path[i].Y * scale;
{$IFDEF USINGZ}
    result[i].Z := path[i].Z;
{$ENDIF}
  end;
end;
//------------------------------------------------------------------------------

function ScalePathD(const path: TPathD; scale: double): TPathD;
var
  i: integer;
begin
  setlength(result, length(path));
  for i := 0 to high(path) do
  begin
    result[i].X := path[i].X * scale;
    result[i].Y := path[i].Y * scale;
{$IFDEF USINGZ}
    result[i].Z := path[i].Z;
{$ENDIF}
  end;
end;
//------------------------------------------------------------------------------

function ScalePathsD(const paths: TPaths64; sx, sy: double): TPathsD;
var
  i,j: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := (paths[i][j].X * sx);
      result[i][j].Y := (paths[i][j].Y * sy);
{$IFDEF USINGZ}
      result[i][j].Z := paths[i][j].Z;
{$ENDIF}
    end;
  end;
end;
//------------------------------------------------------------------------------

function ScalePathsD(const paths: TPathsD; sx, sy: double): TPathsD;
var
  i,j: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := paths[i][j].X * sx;
      result[i][j].Y := paths[i][j].Y * sy;
{$IFDEF USINGZ}
      result[i][j].Z := paths[i][j].Z;
{$ENDIF}
    end;
  end;
end;
//------------------------------------------------------------------------------

function ScalePaths(const paths: TPaths64; scale: double): TPaths64;
var
  i,j: integer;
begin
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := Round(paths[i][j].X * scale);
      result[i][j].Y := Round(paths[i][j].Y * scale);
{$IFDEF USINGZ}
      result[i][j].Z := paths[i][j].Z;
{$ENDIF}
    end;
  end;
end;
//------------------------------------------------------------------------------

function ScalePaths(const paths: TPathsD; scale: double): TPaths64;
var
  i,j: integer;
begin
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := Round(paths[i][j].X * scale);
      result[i][j].Y := Round(paths[i][j].Y * scale);
{$IFDEF USINGZ}
      result[i][j].Z := paths[i][j].Z;
{$ENDIF}
    end;
  end;
end;
//------------------------------------------------------------------------------

function ScalePathsD(const paths: TPaths64; scale: double): TPathsD; overload;
var
  i,j: integer;
begin
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := paths[i][j].X * scale;
      result[i][j].Y := paths[i][j].Y * scale;
{$IFDEF USINGZ}
      result[i][j].Z := paths[i][j].Z;
{$ENDIF}
    end;
  end;
end;
//------------------------------------------------------------------------------

function ScalePathsD(const paths: TPathsD; scale: double): TPathsD; overload;
var
  i,j: integer;
begin
  setlength(result, length(paths));
  for i := 0 to high(paths) do
  begin
    setlength(result[i], length(paths[i]));
    for j := 0 to high(paths[i]) do
    begin
      result[i][j].X := paths[i][j].X * scale;
      result[i][j].Y := paths[i][j].Y * scale;
{$IFDEF USINGZ}
      result[i][j].Z := paths[i][j].Z;
{$ENDIF}
    end;
  end;
end;
//------------------------------------------------------------------------------

function Path64(const pathD: TPathD): TPath64;
var
  i, len: integer;
begin
  len := Length(pathD);
  setLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := Round(pathD[i].X);
    Result[i].Y := Round(pathD[i].Y);
{$IFDEF USINGZ}
    Result[i].Z := pathD[i].Z;
{$ENDIF}
  end;
end;
//------------------------------------------------------------------------------

function PathD(const path: TPath64): TPathD;
var
  i, len: integer;
begin
  len := Length(path);
  setLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := path[i].X;
    Result[i].Y := path[i].Y;
{$IFDEF USINGZ}
    Result[i].Z := path[i].Z;
{$ENDIF}
  end;
end;
//------------------------------------------------------------------------------

function Paths64(const path: TPath64): TPaths64;
begin
  setLength(Result, 1);
  Result[0] := path;
end;
//------------------------------------------------------------------------------

function Paths64(const pathsD: TPathsD): TPaths64;
var
  i, len: integer;
begin
  len := Length(pathsD);
  setLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := Path64(pathsD[i]);
end;
//------------------------------------------------------------------------------

function PathsD(const paths: TPaths64): TPathsD;
var
  i, len: integer;
begin
  len := Length(paths);
  setLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := PathD(paths[i]);
end;
//------------------------------------------------------------------------------

function PathsD(const path: TPathD): TPathsD;
begin
  setLength(Result, 1);
  Result[0] := path;
end;
//------------------------------------------------------------------------------


function ReversePath(const path: TPath64): TPath64;
var
  i, highI: Integer;
begin
  highI := high(path);
  SetLength(Result, highI +1);
  for i := 0 to highI do
    Result[i] := path[highI - i];
end;
//------------------------------------------------------------------------------

function ReversePath(const path: TPathD): TPathD;
var
  i, highI: Integer;
begin
  highI := high(path);
  SetLength(Result, highI +1);
  for i := 0 to highI do
    Result[i] := path[highI - i];
end;
//------------------------------------------------------------------------------

function ReversePaths(const paths: TPaths64): TPaths64;
var
  i, j, highJ: Integer;
begin
  i := length(paths);
  SetLength(Result, i);
  for i := 0 to i -1 do
  begin
    highJ := high(paths[i]);
    SetLength(Result[i], highJ+1);
    for j := 0 to highJ do
      Result[i][j] := paths[i][highJ - j];
  end;
end;
//------------------------------------------------------------------------------

function ReversePaths(const paths: TPathsD): TPathsD;
var
  i, j, highJ: Integer;
begin
  i := length(paths);
  SetLength(Result, i);
  for i := 0 to i -1 do
  begin
    highJ := high(paths[i]);
    SetLength(Result[i], highJ+1);
    for j := 0 to highJ do
      Result[i][j] := paths[i][highJ - j];
  end;
end;
//------------------------------------------------------------------------------

function ShiftPath(const path: TPath64; shift: integer): TPath64;
var
  diff, len: Integer;
begin
  Result := nil;
  len := Length(path);
  if len = 0 then Exit;
  Result := Copy(path, 0, len);
  shift := shift mod len;
  if shift = 0 then Exit;
  if shift < 0 then shift := len + shift;
  diff := len - shift;
  Move(path[shift], Result[0], diff *SizeOf(TPoint64));
  Move(path[0], Result[diff], shift *SizeOf(TPoint64));
end;
//------------------------------------------------------------------------------

function ShiftPath(const path: TPathD; shift: integer): TPathD;
var
  diff, len: Integer;
begin
  Result := nil;
  len := Length(path);
  if len = 0 then Exit;
  Result := Copy(path, 0, len);
  shift := shift mod len;
  if shift = 0 then Exit;
  if shift < 0 then shift := len + shift;
  diff := len - shift;
  Move(path[shift], Result[0], diff *SizeOf(TPointD));
  Move(path[0], Result[diff], shift *SizeOf(TPointD));
end;
//------------------------------------------------------------------------------


procedure AppendPoint(var path: TPath64; const pt: TPoint64);
var
  len: Integer;
begin
  len := length(path);
  SetLength(path, len +1);
  path[len] := pt;
end;
//------------------------------------------------------------------------------

function AppendPoints(const path, extra: TPath64): TPath64;
var
  len1, len2: Integer;
begin
  len1 := length(path);
  len2 := length(extra);
  SetLength(Result, len1 + len2);
  if len1 > 0 then
    Move(path[0], Result[0], len1 * sizeOf(TPoint64));
  if len2 > 0 then
    Move(extra[0], Result[len1], len2 * sizeOf(TPoint64));
end;
//------------------------------------------------------------------------------

procedure AppendPoint(var path: TPathD; const pt: TPointD);
var
  len: Integer;
begin
  len := length(path);
  SetLength(path, len +1);
  path[len] := pt;
end;
//------------------------------------------------------------------------------

procedure AppendPath(var paths: TPaths64; const extra: TPath64);
var
  len: Integer;
begin
  if not Assigned(extra) then Exit;
  len := length(paths);
  SetLength(paths, len +1);
  paths[len] := extra;
end;
//------------------------------------------------------------------------------

procedure AppendPath(var paths: TPathsD; const extra: TPathD);
var
  len: Integer;
begin
  if not Assigned(extra) then Exit;
  len := length(paths);
  SetLength(paths, len +1);
  paths[len] := extra;
end;
//------------------------------------------------------------------------------

procedure AppendPaths(var paths: TPaths64; const extra: TPaths64);
var
  i, len1, len2: Integer;
begin
  len1 := length(paths);
  len2 := length(extra);
  SetLength(paths, len1 + len2);
  for i := 0 to len2 -1 do
    paths[len1 + i] := extra[i];
end;
//------------------------------------------------------------------------------

procedure AppendPaths(var paths: TPathsD; const extra: TPathsD);
var
  i, len1, len2: Integer;
begin
  len1 := length(paths);
  len2 := length(extra);
  SetLength(paths, len1 + len2);
  for i := 0 to len2 -1 do
    paths[len1 + i] := extra[i];
end;
//------------------------------------------------------------------------------

function ArrayOfPathsToPaths(const ap: TArrayOfPaths): TPaths64;
var
  i,j,k, len, cnt: integer;
begin
  cnt := 0;
  len := length(ap);
  for i := 0 to len -1 do
    inc(cnt, length(ap[i]));
  k := 0;
  setlength(result, cnt);
  for i := 0 to len -1 do
    for j := 0 to length(ap[i]) -1 do
    begin
      result[k] := ap[i][j];
      inc(k);
    end;
end;
//------------------------------------------------------------------------------

{$IFDEF USINGZ}
function Point64(const X, Y: Int64; Z: ZType): TPoint64;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;
//------------------------------------------------------------------------------

function Point64(const X, Y: Double; Z: ZType): TPoint64;
begin
  Result.X := Round(X);
  Result.Y := Round(Y);
  Result.Z := Z;
end;
//------------------------------------------------------------------------------

function PointD(const X, Y: Double; Z: ZType): TPointD;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;
//------------------------------------------------------------------------------

function Point64(const pt: TPointD): TPoint64;
begin
  Result.X := Round(pt.X);
  Result.Y := Round(pt.Y);
  Result.Z := pt.Z;
end;
//------------------------------------------------------------------------------

function PointD(const pt: TPoint64): TPointD;
begin
  Result.X := pt.X;
  Result.Y := pt.Y;
  Result.Z := pt.Z;
end;
//------------------------------------------------------------------------------

{$ELSE}

function Point64(const X, Y: Int64): TPoint64;
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

function Point64(const X, Y: Double): TPoint64;
begin
  Result.X := Round(X);
  Result.Y := Round(Y);
end;
//------------------------------------------------------------------------------

function PointD(const X, Y: Double): TPointD;
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

function Point64(const pt: TPointD): TPoint64;
begin
  Result.X := Round(pt.X);
  Result.Y := Round(pt.Y);
end;
//------------------------------------------------------------------------------

function PointD(const pt: TPoint64): TPointD;
begin
  Result.X := pt.X;
  Result.Y := pt.Y;
end;
//------------------------------------------------------------------------------
{$ENDIF}

function Negate(const pt: TPoint64): TPoint64;
begin
  Result.X := -pt.X;
  Result.Y := -pt.Y;
end;
//------------------------------------------------------------------------------

function Negate(const pt: TPointD): TPointD;
begin
  Result.X := -pt.X;
  Result.Y := -pt.Y;
end;
//------------------------------------------------------------------------------

function NegatePath(const path: TPathD): TPathD;
var
  i: Integer;
begin
  Result := path;
  for i := 0 to High(Result) do
    with Result[i] do
    begin
      X := -X;
      Y := -Y;
    end;
end;
//------------------------------------------------------------------------------

function Rect64(const left, top, right, bottom: Int64): TRect64;
begin
  Result.Left   := left;
  Result.Top    := top;
  Result.Right  := right;
  Result.Bottom := bottom;
end;
//------------------------------------------------------------------------------

function Rect64(const recD: TRectD): TRect64;
begin
  Result.Left   := Floor(recD.left);
  Result.Top    := Floor(recD.top);
  Result.Right  := Ceil(recD.right);
  Result.Bottom := Ceil(recD.bottom);
end;
//------------------------------------------------------------------------------

function RectD(const left, top, right, bottom: double): TRectD;
begin
  Result.Left   := left;
  Result.Top    := top;
  Result.Right  := right;
  Result.Bottom := bottom;
end;
//------------------------------------------------------------------------------

function RectD(const rec64: TRect64): TRectD; overload;
begin
  Result.Left   := rec64.left;
  Result.Top    := rec64.top;
  Result.Right  := rec64.right;
  Result.Bottom := rec64.bottom;
end;
//------------------------------------------------------------------------------

function GetBounds(const paths: TArrayOfPaths): TRect64; overload;
var
  i,j,k: Integer;
  p: PPoint64;
begin
  Result := Rect64(MaxInt64, MaxInt64, -MaxInt64, -MaxInt64);
  for i := 0 to High(paths) do
    for j := 0 to High(paths[i]) do
      if Assigned(paths[i][j]) then
      begin
        p := @paths[i][j][0];
        for k := 0 to High(paths[i][j]) do
        begin
          if p.X < Result.Left then Result.Left := p.X;
          if p.X > Result.Right then Result.Right := p.X;
          if p.Y < Result.Top then Result.Top := p.Y;
          if p.Y > Result.Bottom then Result.Bottom := p.Y;
          inc(p);
        end;
      end;
  if Result.Left > Result.Right then Result := NullRect64;
end;
//------------------------------------------------------------------------------

function GetBounds(const paths: TPaths64): TRect64;
var
  i,j: Integer;
  p: PPoint64;
begin
  Result := Rect64(MaxInt64, MaxInt64, -MaxInt64, -MaxInt64);
  for i := 0 to High(paths) do
    if Assigned(paths[i]) then
    begin
      p := @paths[i][0];
      for j := 0 to High(paths[i]) do
      begin
        if p.X < Result.Left then Result.Left := p.X;
        if p.X > Result.Right then Result.Right := p.X;
        if p.Y < Result.Top then Result.Top := p.Y;
        if p.Y > Result.Bottom then Result.Bottom := p.Y;
        inc(p);
      end;
    end;
  if Result.Left = MaxInt64 then Result := NullRect64;
end;
//------------------------------------------------------------------------------

function GetBounds(const paths: TPathsD): TRectD;
var
  i,j: Integer;
  p: PPointD;
begin
  Result := RectD(MaxDouble, MaxDouble, -MaxDouble, -MaxDouble);
  for i := 0 to High(paths) do
    if Assigned(paths[i]) then
    begin
      p := @paths[i][0];
      for j := 0 to High(paths[i]) do
      begin
        if p.X < Result.Left then Result.Left := p.X;
        if p.X > Result.Right then Result.Right := p.X;
        if p.Y < Result.Top then Result.Top := p.Y;
        if p.Y > Result.Bottom then Result.Bottom := p.Y;
        inc(p);
      end;
    end;
  if Result.Left = MaxDouble then Result := NullRectD;
end;
//------------------------------------------------------------------------------

function GetBounds(const path: TPath64): TRect64;
var
  i, len: Integer;
  p: PPoint64;
begin
  len := Length(path);
  if len = 0 then
  begin
    Result := NullRect64;
    Exit;
  end;

  Result := Rect64(MaxInt64, MaxInt64, -MaxInt64, -MaxInt64);
  p := @path[0];
  for i := 0 to High(path) do
  begin
    if p.X < Result.Left then Result.Left := p.X;
    if p.X > Result.Right then Result.Right := p.X;
    if p.Y < Result.Top then Result.Top := p.Y;
    if p.Y > Result.Bottom then Result.Bottom := p.Y;
    inc(p);
  end;
end;
//------------------------------------------------------------------------------

function GetBounds(const path: TPathD): TRectD;
var
  i, len: Integer;
  p: PPointD;
begin
  len := Length(path);
  if len = 0 then
  begin
    Result := NullRectD;
    Exit;
  end;

  Result := RectD(infinity, infinity, -infinity, -infinity);
  p := @path[0];
  for i := 0 to High(path) do
  begin
    if p.X < Result.Left then Result.Left := p.X;
    if p.X > Result.Right then Result.Right := p.X;
    if p.Y < Result.Top then Result.Top := p.Y;
    if p.Y > Result.Bottom then Result.Bottom := p.Y;
    inc(p);
  end;
end;
//------------------------------------------------------------------------------

function TranslatePoint(const pt: TPoint64; dx, dy: Int64): TPoint64;
begin
  Result.X := pt.X + dx;
  Result.Y := pt.Y + dy;
end;
//------------------------------------------------------------------------------

function TranslatePoint(const pt: TPointD; dx, dy: double): TPointD;
begin
  Result.X := pt.X + dx;
  Result.Y := pt.Y + dy;
end;
//------------------------------------------------------------------------------

procedure InflateRect(var rec: TRect64; dx, dy: Int64);
begin
  dec(rec.Left, dx);
  inc(rec.Right, dx);
  dec(rec.Top, dy);
  inc(rec.Bottom, dy);
end;
//------------------------------------------------------------------------------

procedure InflateRect(var rec: TRectD; dx, dy: double);
begin
  rec.Left := rec.Left - dx;
  rec.Right := rec.Right + dx;
  rec.Top := rec.Top - dy;
  rec.Bottom := rec.Bottom + dy;
end;
//------------------------------------------------------------------------------

procedure RotatePt(var pt: TPointD; const center: TPointD; sinA, cosA: double);
var
  tmpX, tmpY: double;
begin
  tmpX := pt.X-center.X;
  tmpY := pt.Y-center.Y;
  pt.X := tmpX * cosA - tmpY * sinA + center.X;
  pt.Y := tmpX * sinA + tmpY * cosA + center.Y;
end;
//------------------------------------------------------------------------------

procedure RotatePath(var path: TPathD; const center: TPointD; sinA, cosA: double);
var
  i: integer;
begin
  for i := 0 to High(path) do
    RotatePt(path[i], center, sinA, cosA);
end;
//------------------------------------------------------------------------------

function RotateRect(const rec: TRectD; angleRad: double): TRectD;
var
  i: integer;
  sinA, cosA: double;
  cp: TPointD;
  pts: TPathD;
begin
  setLength(pts, 4);
  sinA := Sin(-angleRad);
  cosA := cos(-angleRad);
  cp.X := (rec.Right + rec.Left) / 2;
  cp.Y := (rec.Bottom + rec.Top) / 2;
  pts[0] := PointD(rec.Left, rec.Top);
  pts[1] := PointD(rec.Right, rec.Top);
  pts[2] := PointD(rec.Left, rec.Bottom);
  pts[3] := PointD(rec.Right, rec.Bottom);
  for i := 0 to 3 do RotatePt(pts[i], cp, sinA, cosA);
  result.Left := pts[0].X;
  result.Right := result.Left;
  result.Top := pts[0].Y;
  result.Bottom := result.Top;
  for i := 1 to 3 do
  begin
    if pts[i].X < result.Left then result.Left := pts[i].X;
    if pts[i].Y < result.Top then result.Top := pts[i].Y;
    if pts[i].X > result.Right then result.Right := pts[i].X;
    if pts[i].Y > result.Bottom then result.Bottom := pts[i].Y;
  end;
end;
//------------------------------------------------------------------------------

function RotateRect(const rec: TRect64; angleRad: double): TRect64;
var
  recD: TRectD;
begin
  recD := RectD(rec.Left, rec.Top, rec.Right, rec.Bottom);
  recD := RotateRect(recD, angleRad);
  result.Left := Floor(recD.Left);
  result.Top := Floor(recD.Top);
  result.Right := Ceil(recD.Right);
  result.Bottom := Ceil(recD.Bottom);
end;
//------------------------------------------------------------------------------

procedure OffsetRect(var rec: TRect64; dx, dy: Int64);
begin
  inc(rec.Left, dx); inc(rec.Top, dy);
  inc(rec.Right, dx); inc(rec.Bottom, dy);
end;
//------------------------------------------------------------------------------

procedure OffsetRect(var rec: TRectD; dx, dy: double);
begin
  rec.Left   := rec.Left   + dx;
  rec.Right  := rec.Right  + dx;
  rec.Top    := rec.Top    + dy;
  rec.Bottom := rec.Bottom + dy;
end;
//------------------------------------------------------------------------------

function UnionRect(const rec, rec2: TRect64): TRect64;
begin
  // nb: don't use rec.IsEmpty as this will
  // reject open axis-aligned flat paths
  if (rec.Width <= 0) and (rec.Height <= 0) then result := rec2
  else if (rec2.Width <= 0) and (rec2.Height <= 0) then result := rec
  else
  begin
    result.Left := min(rec.Left, rec2.Left);
    result.Right := max(rec.Right, rec2.Right);
    result.Top := min(rec.Top, rec2.Top);
    result.Bottom := max(rec.Bottom, rec2.Bottom);
  end;
end;
//------------------------------------------------------------------------------

function UnionRect(const rec, rec2: TRectD): TRectD;
begin
  // nb: don't use rec.IsEmpty as this will
  // reject open axis-aligned flat paths
  if (rec.Width <= 0) and (rec.Height <= 0) then result := rec2
  else if (rec2.Width <= 0) and (rec2.Height <= 0) then result := rec
  else
  begin
    result.Left := min(rec.Left, rec2.Left);
    result.Right := max(rec.Right, rec2.Right);
    result.Top := min(rec.Top, rec2.Top);
    result.Bottom := max(rec.Bottom, rec2.Bottom);
  end;
end;
//------------------------------------------------------------------------------

function Area(const path: TPath64): Double;
var
  i, highI: Integer;
  d: double;
  p1,p2: PPoint64;
begin
  // shoelace formula
  Result := 0.0;
  highI := High(path);
  if highI < 2 then Exit;
  p1 := @path[highI];
  p2 := @path[0];
  for i := 0 to highI do
  begin
    d := (p1.Y + p2.Y); // needed for Delphi7
    Result := Result + d * (p1.X - p2.X);
    p1 := p2; inc(p2);
  end;
  Result := Result * 0.5;
end;
//------------------------------------------------------------------------------

function Area(const paths: TPaths64): Double;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(paths) do
    Result := Result + Area(paths[i]);
end;
//------------------------------------------------------------------------------

function Area(const path: TPathD): Double;
var
  i, highI: Integer;
  p1,p2: PPointD;
begin
  // https://en.wikipedia.org/wiki/Shoelace_formula
  Result := 0.0;
  highI := High(path);
  if highI < 2 then Exit;
  p1 := @path[highI];
  p2 := @path[0];
  for i := 0 to highI do
  begin
    Result := Result + (p1.Y + p2.Y) * (p1.X - p2.X);
    p1 := p2; inc(p2);
  end;
  Result := Result * 0.5;
end;
//------------------------------------------------------------------------------

function Area(const paths: TPathsD): Double;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(paths) do
    Result := Result + Area(paths[i]);
end;
//------------------------------------------------------------------------------

function IsPositive(const path: TPath64): Boolean;
begin
  Result := (Area(path) >= 0);
end;
//------------------------------------------------------------------------------

function IsPositive(const path: TPathD): Boolean;
begin
  Result := (Area(path) >= 0);
end;
//------------------------------------------------------------------------------

function TriSign(val: Int64): integer; // returns 0, 1 or -1
{$IFDEF INLINING} inline; {$ENDIF}
begin
  if (val < 0) then Result := -1
  else if (val > 1) then Result := 1
  else Result := 0;
end;
//------------------------------------------------------------------------------

type
  TMultiplyUInt64Result = record
    lo64: UInt64;
    hi64 : UInt64;
  end;

function MultiplyUInt64(a, b: UInt64): TMultiplyUInt64Result; // #834, #835
{$IFDEF INLINING} inline; {$ENDIF}
var
  x1, x2, x3: UInt64;
begin
  x1 := (a and $FFFFFFFF) * (b and $FFFFFFFF);
  x2 := (a shr 32) * (b and $FFFFFFFF) + (x1 shr 32);
  x3 := (a and $FFFFFFFF) * (b shr 32) + (x2 and $FFFFFFFF);
  Result.lo64 := ((x3 and $FFFFFFFF) shl 32) or (x1 and $FFFFFFFF);
  Result.hi64 := hi(a shr 32) * (b shr 32) + (x2 shr 32) + (x3 shr 32);
end;
//------------------------------------------------------------------------------

function ProductsAreEqual(a, b, c, d: Int64): Boolean;
var
  absA,absB,absC,absD: UInt64;
  absAB, absCD       : TMultiplyUInt64Result;
  signAB, signCD     : integer;
begin
  // nb: unsigned values will be needed for CalcOverflowCarry()
  absA := UInt64(Abs(a));
  absB := UInt64(Abs(b));
  absC := UInt64(Abs(c));
  absD := UInt64(Abs(d));

  absAB := MultiplyUInt64(absA, absB);
  absCD := MultiplyUInt64(absC, absD);

  // nb: it's important to differentiate 0 values here from other values
  signAB := TriSign(a) * TriSign(b);
  signCD := TriSign(c) * TriSign(d);

  Result := (absAB.lo64 = absCD.lo64) and
    (absAB.hi64 = absCD.hi64) and (signAB = signCD);
end;
//------------------------------------------------------------------------------

function IsCollinear(const pt1, sharedPt, pt2: TPoint64): Boolean;
var
  a,b,c,d: Int64;
begin
  a := sharedPt.X - pt1.X;
  b := pt2.Y - sharedPt.Y;
  c := sharedPt.Y - pt1.Y;
  d := pt2.X - sharedPt.X;
  // When checking for collinearity with very large coordinate values
  // then ProductsAreEqual is more accurate than using CrossProduct.
  Result := ProductsAreEqual(a, b, c, d);
end;
//------------------------------------------------------------------------------

function CrossProduct(const pt1, pt2, pt3: TPoint64): double;
begin
  result := CrossProduct(
    pt2.X - pt1.X, pt2.Y - pt1.Y,
    pt3.X - pt2.X, pt3.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function CrossProduct(const pt1, pt2, pt3: TPointD): double;
begin
  result := CrossProduct(
    pt2.X - pt1.X, pt2.Y - pt1.Y,
    pt3.X - pt2.X, pt3.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function CrossProduct(const vec1, vec2: TPointD): double;
begin
  result := (vec1.X * vec2.Y - vec1.Y * vec2.X);
end;
//------------------------------------------------------------------------------

function CrossProduct(vec1x, vec1y, vec2x, vec2y: double): double;
begin
  result := (vec1x * vec2y - vec1y * vec2x);
end;
//------------------------------------------------------------------------------

function DotProduct(const pt1, pt2, pt3: TPoint64): double;
var
  x1,x2,y1,y2: double; // avoids potential int overflow
begin
  x1 := pt2.X - pt1.X;
  y1 := pt2.Y - pt1.Y;
  x2 := pt3.X - pt2.X;
  y2 := pt3.Y - pt2.Y;
  result := (x1 * x2 + y1 * y2);
end;
//------------------------------------------------------------------------------

function SqrInt64(val: Int64): double; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := val; // force conversion
  Result := Result * Result;
end;
//------------------------------------------------------------------------------

function DistanceSqr(const pt1, pt2: TPoint64): double;
begin
  Result := SqrInt64(pt1.X - pt2.X) + SqrInt64(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function DistanceSqr(const pt1, pt2: TPointD): double;
begin
  Result := Sqr(pt1.X - pt2.X) + Sqr(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function PerpendicDistFromLineSqrd(const pt, linePt1, linePt2: TPoint64): double;
var
  a,b,c: double;
begin
  // perpendicular distance of point (x0,y0) = (a*x0 + b*y0 + C)/Sqrt(a*a + b*b)
  // where ax + by +c = 0 is the equation of the line
  // see https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
	a := (linePt1.Y - linePt2.Y);
	b := (linePt2.X - linePt1.X);
	c := a * linePt1.X + b * linePt1.Y;
	c := a * pt.x + b * pt.y - c;
   if (a = 0) and (b = 0) then
    Result := 0 else
	  Result := (c * c) / (a * a + b * b);
end;
//---------------------------------------------------------------------------

function PerpendicDistFromLineSqrd(const pt, linePt1, linePt2: TPointD): double;
var
  a,b,c: double;
begin
	a := (linePt1.Y - linePt2.Y);
	b := (linePt2.X - linePt1.X);
	c := a * linePt1.X + b * linePt1.Y;
	c := a * pt.x + b * pt.y - c;
  if (a = 0) and (b = 0) then
    Result := 0 else
	  Result := (c * c) / (a * a + b * b);
end;
//---------------------------------------------------------------------------

function CleanPath(const path: TPath64): TPath64;
var
  i,j, len: integer;
  prev: TPoint64;
begin
  Result := nil;
  len := Length(path);
  while (len > 2) and
   (IsCollinear(path[len-2], path[len-1], path[0])) do dec(len);
  SetLength(Result, len);
  if (len < 2) then Exit;
  prev := path[len -1];
  j := 0;
  for i := 0 to len -2 do
  begin
    if IsCollinear(prev, path[i], path[i+1]) then Continue;
    Result[j] := path[i];
    inc(j);
    prev := path[i];
  end;
  Result[j] := path[len -1];
  SetLength(Result, j+1);
end;
//------------------------------------------------------------------------------

function GetSign(const val: double): integer; {$IFDEF INLINING} inline; {$ENDIF}
begin
  if val = 0 then Result := 0
  else if val < 0 then Result := -1
  else Result := 1;
end;
//------------------------------------------------------------------------------

function SegmentsIntersect(const s1a, s1b, s2a, s2b: TPoint64;
  inclusive: Boolean): boolean;
var
  res1, res2, res3, res4: double;
begin
  if inclusive then
  begin
    //result can include segments that only touch
    Result := false;
    res1 := CrossProduct(s1a, s2a, s2b);
    res2 := CrossProduct(s1b, s2a, s2b);
    if (res1 * res2 > 0) then Exit;
    res3 := CrossProduct(s2a, s1a, s1b);
    res4 := CrossProduct(s2b, s1a, s1b);
    if (res3 * res4 > 0) then Exit;
    Result := (res1 <> 0) or (res2 <> 0) or
      (res3 <> 0) or (res4 <> 0); // ensures not collinear
  end else
  begin
    result := (GetSign(CrossProduct(s1a, s2a, s2b)) *
      GetSign(CrossProduct(s1b, s2a, s2b)) < 0) and
      (GetSign(CrossProduct(s2a, s1a, s1b)) *
      GetSign(CrossProduct(s2b, s1a, s1b)) < 0);
  end;
end;
//------------------------------------------------------------------------------

function GetSegmentIntersectPt(const ln1a, ln1b, ln2a, ln2b: TPoint64;
  out ip: TPoint64): Boolean;
var
  dx1,dy1, dx2,dy2, t, cp: double;
begin
  // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
  dy1 := (ln1b.y - ln1a.y);
  dx1 := (ln1b.x - ln1a.x);
  dy2 := (ln2b.y - ln2a.y);
  dx2 := (ln2b.x - ln2a.x);
  cp  := dy1 * dx2 - dy2 * dx1;
  Result := (cp <> 0.0);
  if not Result then Exit;
  t := ((ln1a.x-ln2a.x) * dy2 - (ln1a.y-ln2a.y) * dx2) / cp;
  if t <= 0.0 then ip := ln1a
  else if t >= 1.0 then ip := ln1b;
  ip.X :=  Trunc(ln1a.X + t * dx1);
  ip.Y :=  Trunc(ln1a.Y + t * dy1);
{$IFDEF USINGZ}
  ip.Z := 0;
{$ENDIF}
end;
//------------------------------------------------------------------------------

{$R-}
function PointInPolygon(const pt: TPoint64;
  const polygon: TPath64): TPointInPolygonResult;
var
  len, val: Integer;
  isAbove, startingAbove: Boolean;
  d: Double; // avoids integer overflow
  curr, prev, cbegin, cend, first: PPoint64;
begin
  result := pipOutside;
  len := Length(polygon);
  if len < 3 then Exit;

  cbegin := @polygon[0];
  cend := @polygon[len]; // stop is just past the last point (nb {$R-})

  first := cbegin;
  while (first <> cend) and (first.Y = pt.Y) do inc(first);
  if (first = cend) then Exit; // not a proper polygon

  isAbove := first.Y < pt.Y;
  startingAbove := isAbove;
  Result := pipOn;
  curr := first;
  inc(curr);
  val := 0;
  while true do
  begin
    if (curr = cend) then
    begin
      if (cend = first) or (first = cbegin) then break;
      cend := first;
      curr := cbegin;
    end;

    if isAbove then
    begin
      while (curr <> cend) and (curr.Y < pt.Y) do inc(curr);
      if (curr = cend) then Continue;
    end else
    begin
      while (curr <> cend) and (curr.Y > pt.Y) do inc(curr);
      if (curr = cend) then Continue;
    end;

    if curr = cbegin then
      prev := @polygon[len] else // NOT cend!
      prev := curr;
    dec(prev);

    if (curr.Y = pt.Y) then
    begin
      if (curr.X = pt.X) or ((curr.Y = prev.Y) and
        ((pt.X < prev.X) <> (pt.X < curr.X))) then Exit;
      inc(curr);
      if (curr = first) then Break;
      Continue;
    end;

    if (pt.X < curr.X) and (pt.X < prev.X) then
      // we're only interested in edges crossing on the left
    else if((pt.X > prev.X) and (pt.X > curr.X)) then
      val := 1 - val // toggle val
    else
    begin
      d := CrossProduct(prev^, curr^, pt);
      if d = 0 then Exit; // ie point on path
      if (d < 0) = isAbove then val := 1 - val;
    end;

    isAbove := not isAbove;
    inc(curr);
  end;

  if (isAbove <> startingAbove) then
  begin
    cend := @polygon[len];
    if (curr = cend) then curr := cbegin;
    if curr = cbegin then
      prev := cend else
      prev := curr;
    dec(prev);
    d := CrossProduct(prev^, curr^, pt);
    if d = 0 then Exit; // ie point on path
    if (d < 0) = isAbove then val := 1 - val;
  end;

  if val = 0 then
     result := pipOutside else
     result := pipInside;
end;
//------------------------------------------------------------------------------
{$R+}

procedure GetSinCos(angle: double; out sinA, cosA: double);
  {$IFDEF INLINE} inline; {$ENDIF}
{$IFNDEF FPC}
var s, c: extended;
{$ENDIF}
begin
{$IFDEF FPC}
  Math.SinCos(angle, sinA, cosA);
{$ELSE}
  Math.SinCos(angle, s, c);
  sinA := s; cosA := c;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function Ellipse(const rec: TRect64; steps: integer): TPath64;
begin
  Result := Path64(Ellipse(RectD(rec), steps));
end;
//------------------------------------------------------------------------------

function Ellipse(const rec: TRectD; steps: integer): TPathD;
var
  i: Integer;
  sinA, cosA: double;
  centre, radius, delta: TPointD;
begin
  result := nil;
  if rec.IsEmpty then Exit;
  with rec do
  begin
    centre := rec.MidPoint;
    radius := PointD(Width * 0.5, Height  * 0.5);
  end;
  if (steps < 3) then
    steps := Ceil(PI * sqrt(rec.width + rec.height));
  GetSinCos(2 * Pi / Steps, sinA, cosA);
  delta.x := cosA; delta.y := sinA;
  SetLength(Result, Steps);
  Result[0] := PointD(centre.X + radius.X, centre.Y);
  for i := 1 to steps -1 do
  begin
    Result[i] := PointD(centre.X + radius.X * delta.x,
      centre.Y + radius.y * delta.y);
    delta :=  PointD(delta.X * cosA - delta.Y * sinA,
      delta.Y * cosA + delta.X * sinA);
  end; // rotates clockwise
end;
//------------------------------------------------------------------------------

function GetClosestPointOnSegment(const pt, seg1, seg2: TPoint64): TPoint64;
var
  dx, dy, q: double;
begin
    if (seg1.X = seg2.X) and (seg1.Y = seg2.Y) then
    begin
      Result := seg1;
      Exit;
    end;
    dx := (seg2.X - seg1.X);
    dy := (seg2.Y - seg1.Y);
    q := ((pt.X - seg1.X) * dx + (pt.Y - seg1.Y) * dy) / (Sqr(dx) + Sqr(dy));
    if (q < 0) then q := 0
    else if (q > 1) then q := 1;
    Result := Point64(
      seg1.X + Round(q * dx),
      seg1.Y + Round(q * dy));
end;
//------------------------------------------------------------------------------

procedure RDP(const path: TPath64; startIdx, endIdx: integer;
  epsilonSqrd: double; var boolArray: TArrayOfBoolean); overload;
var
  i, idx: integer;
  d, maxD: double;
begin
  idx := 0;
  maxD := 0;
	while (endIdx > startIdx) and
    PointsEqual(path[startIdx], path[endIdx]) do
    begin
      boolArray[endIdx] := false;
      dec(endIdx);
    end;
  for i := startIdx +1 to endIdx -1 do
  begin
    // PerpendicDistFromLineSqrd - avoids expensive Sqrt()
    d := PerpendicDistFromLineSqrd(path[i], path[startIdx], path[endIdx]);
    if d <= maxD then Continue;
    maxD := d;
    idx := i;
  end;
  if maxD < epsilonSqrd then Exit;
  boolArray[idx] := true;
  if idx > startIdx + 1 then RDP(path, startIdx, idx, epsilonSqrd, boolArray);
  if endIdx > idx + 1 then RDP(path, idx, endIdx, epsilonSqrd, boolArray);
end;
//------------------------------------------------------------------------------

procedure RDP(const path: TPathD; startIdx, endIdx: integer;
  epsilonSqrd: double; var boolArray: TArrayOfBoolean); overload;
var
  i, idx: integer;
  d, maxD: double;
begin
  idx := 0;
  maxD := 0;
	while (endIdx > startIdx) and
    PointsNearEqual(path[startIdx], path[endIdx]) do
    begin
      boolArray[endIdx] := false;
      dec(endIdx);
    end;
  for i := startIdx +1 to endIdx -1 do
  begin
    // PerpendicDistFromLineSqrd - avoids expensive Sqrt()
    d := PerpendicDistFromLineSqrd(path[i], path[startIdx], path[endIdx]);
    if d <= maxD then Continue;
    maxD := d;
    idx := i;
  end;
  if maxD < epsilonSqrd then Exit;
  boolArray[idx] := true;
  if idx > startIdx + 1 then RDP(path, startIdx, idx, epsilonSqrd, boolArray);
  if endIdx > idx + 1 then RDP(path, idx, endIdx, epsilonSqrd, boolArray);
end;
//------------------------------------------------------------------------------

function RamerDouglasPeucker(const path: TPath64; epsilon: double): TPath64;
var
  i,j, len: integer;
  boolArray: TArrayOfBoolean;
begin
  len := length(path);
  if len < 5 then
  begin
    result := Copy(path, 0, len);
    Exit;
  end;
  SetLength(boolArray, len); // already zero initialized
  boolArray[0] := true;
  boolArray[len -1] := true;
  RDP(path, 0, len -1, Sqr(epsilon), boolArray);
  j := 0;
  SetLength(Result, len);
  for i := 0 to len -1 do
    if boolArray[i] then
    begin
      Result[j] := path[i];
      inc(j);
    end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

function RamerDouglasPeucker(const paths: TPaths64; epsilon: double): TPaths64;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := RamerDouglasPeucker(paths[i], epsilon);
end;
//------------------------------------------------------------------------------

function RamerDouglasPeucker(const path: TPathD; epsilon: double): TPathD; overload;
var
  i,j, len: integer;
  boolArray: TArrayOfBoolean;
begin
  len := length(path);
  if len < 5 then
  begin
    result := Copy(path, 0, len);
    Exit;
  end;
  SetLength(boolArray, len); // already zero initialized
  boolArray[0] := true;
  boolArray[len -1] := true;
  RDP(path, 0, len -1, Sqr(epsilon), boolArray);
  j := 0;
  SetLength(Result, len);
  for i := 0 to len -1 do
    if boolArray[i] then
    begin
      Result[j] := path[i];
      inc(j);
    end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

function RamerDouglasPeucker(const paths: TPathsD; epsilon: double): TPathsD; overload;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := RamerDouglasPeucker(paths[i], epsilon);
end;
//------------------------------------------------------------------------------

end.

