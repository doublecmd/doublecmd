unit Img32.Vector;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.7                                                             *
* Date      :  6 January 2025                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
*                                                                              *
* Purpose   :  Vector drawing for TImage32                                     *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Types, Img32;

type
  TArrowStyle = (asNone, asSimple, asFancy, asDiamond, asCircle, asTail);
  // TJoinStyle:
  //   jsSquare - Convex joins will be truncated using a 'squaring' edge.
  //   The mid-points of these squaring edges will also be exactly the offset
  //  (ie delta) distance away from their origins (ie the starting vertices).
  //  jsButt - joins are similar to 'squared' joins except that squaring
  //  won't occur at a fixed distance. While bevelled joins may not be as
  //  pretty as squared joins, bevelling will be much faster than squaring.
  //  And perhaps this is why bevelling (rather than squaring) is preferred
  //  in numerous graphics display formats (including SVG & PDF documents).
  TJoinStyle  = (jsAuto, jsSquare, jsButt, jsMiter, jsRound);
  TEndStyle   = (esPolygon = 0, esClosed = 0, esButt, esSquare, esRound);
  TPathEnd    = (peStart, peEnd, peBothEnds);
  TSplineType = (stQuadratic, stCubic);
  TFillRule = (frEvenOdd, frNonZero, frPositive, frNegative);
  TImg32FillRule = TFillRule; //useful whenever there's ambiguity with Clipper

  TSizeD = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    cx  : double;
    cy  : double;
    function average: double;
    property Width: Double read cx write cx;
    property Height: Double read cy write cy;
  end;

  TRectWH = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
  public
    Left, Top, Width, Height: double;
    function IsEmpty: Boolean;
    function IsValid: Boolean;
    function Right: double;
    function Bottom: double;
    function Contains(const Pt: TPoint): Boolean; overload;
    function Contains(const Pt: TPointD): Boolean; overload;
    function MidPoint: TPointD;
    function RectD: TRectD;
    function Rect: TRect;
  end;

  function RectWH(left, top, width, height: integer): TRectWH; overload;
  function RectWH(left, top, width, height: double ): TRectWH; overload;
  function RectWH(const rec: TRectD): TRectWH; overload;

  //InflateRect: missing in Delphi 7
  procedure InflateRect(var rec: TRect; dx, dy: integer); overload;
  procedure InflateRect(var rec: TRectD; dx, dy: double); overload;

  function NormalizeRect(var rect: TRect): Boolean;

  function PrePendPoint(const pt: TPointD; const p: TPathD): TPathD; overload;
  procedure PrePendPoint(const pt: TPointD; const p: TPathD; var Result: TPathD); overload;
  function PrePendPoints(const pt1, pt2: TPointD; const p: TPathD): TPathD;

  function Rectangle(const rec: TRect): TPathD; overload;
  function Rectangle(const rec: TRectD): TPathD; overload;
  function Rectangle(l, t, r, b: double): TPathD; overload;

  function RoundRect(const rec: TRect; radius: integer): TPathD; overload;
  function RoundRect(const rec: TRectD; radius: double): TPathD; overload;
  function RoundRect(const rec: TRect; radius: TPoint): TPathD; overload;
  function RoundRect(const rec: TRectD; radius: TPointD): TPathD; overload;

  function Ellipse(const rec: TRect; steps: integer = 0): TPathD; overload;
  function Ellipse(const rec: TRectD; steps: integer = 0): TPathD; overload;
  function Ellipse(const rec: TRectD; pendingScale: double): TPathD; overload;

  function RotatedEllipse(const rec: TRectD; angle: double; steps: integer = 0): TPathD; overload;
  function RotatedEllipse(const rec: TRectD; angle: double; pendingScale: double): TPathD; overload;

  function AngleToEllipticalAngle(const ellRec: TRectD; angle: double): double;

  function EllipticalAngleToAngle(const ellRec: TRectD; angle: double): double;

  function Circle(const pt: TPoint; radius: double): TPathD; overload;
  function Circle(const pt: TPointD; radius: double): TPathD; overload;
  function Circle(const pt: TPointD; radius: double; pendingScale: double): TPathD; overload;

  function CalcCircleFrom3Points(const p1,p2,p3: TPointD;
    out centre: TPointD; out radius: double): Boolean;

  function Star(const rec: TRectD; points: integer; indentFrac: double = 0.4): TPathD; overload;
  function Star(const focalPt: TPointD;
    innerRadius, outerRadius: double; points: integer): TPathD; overload;

  function Arc(const rec: TRectD;
    startAngle, endAngle: double; scale: double = 0): TPathD;

  function Pie(const rec: TRectD;
    StartAngle, EndAngle: double; scale: double = 0): TPathD;

  function FlattenQBezier(const pt1, pt2, pt3: TPointD;
    tolerance: double = 0.0): TPathD; overload;
  function FlattenQBezier(const pts: TPathD;
    tolerance: double = 0.0): TPathD; overload;
  function FlattenQBezier(const firstPt: TPointD; const pts: TPathD;
    tolerance: double = 0.0): TPathD; overload;

  function GetPointInQuadBezier(const a,b,c: TPointD; t: double): TPointD;

  function FlattenCBezier(const pt1, pt2, pt3, pt4: TPointD;
    tolerance: double = 0.0): TPathD; overload;
  function FlattenCBezier(const path: TPathD;
    tolerance: double = 0.0): TPathD; overload;
  function FlattenCBezier(const paths: TPathsD;
    tolerance: double = 0.0): TPathsD; overload;
  function FlattenCBezier(const firstPt: TPointD; const pts: TPathD;
    tolerance: double = 0.0): TPathD; overload;

  function GetPointInCubicBezier(const a,b,c,d: TPointD; t: double): TPointD;

  //FlattenCSpline: Approximates the 'S' command inside the 'd' property of an
  //SVG path. (See https://www.w3.org/TR/SVG/paths.html#DProperty)
  function FlattenCSpline(const pts: TPathD;
    tolerance: double = 0.0): TPathD; overload;
  function FlattenCSpline(const priorCtrlPt, startPt: TPointD;
    const pts: TPathD; tolerance: double = 0.0): TPathD; overload;

  //FlattenQSpline: Approximates the 'T' command inside the 'd' property of an
  //SVG path. (See https://www.w3.org/TR/SVG/paths.html#DProperty)
  function FlattenQSpline(const pts: TPathD;
    tolerance: double = 0.0): TPathD; overload;
  function FlattenQSpline(const priorCtrlPt, startPt: TPointD;
    const pts: TPathD; tolerance: double = 0.0): TPathD; overload;

  //ArrowHead: The ctrlPt's only function is to control the angle of the arrow.
  function ArrowHead(const arrowTip, ctrlPt: TPointD; size: double;
    arrowStyle: TArrowStyle): TPathD;

  function GetDefaultArrowHeadSize(lineWidth: double): double;

  procedure AdjustPoint(var pt: TPointD;
    const referencePt: TPointD; delta: double);

  function ShortenPath(const path: TPathD;
    pathEnd: TPathEnd; amount: double): TPathD;

  //GetDashPath: Returns a polyline (not polygons)
  function GetDashedPath(const path: TPathD;
    closed: Boolean; const pattern: TArrayOfDouble;
    patternOffset: PDouble): TPathsD;

  function GetDashedOutLine(const path: TPathD;
    closed: Boolean; const pattern: TArrayOfDouble;
    patternOffset: PDouble; lineWidth: double;
    joinStyle: TJoinStyle; endStyle: TEndStyle): TPathsD;

  function TranslatePoint(const pt: TPoint; dx, dy: integer): TPoint; overload;
  function TranslatePoint(const pt: TPointD; dx, dy: double): TPointD; overload;

  function TranslatePath(const path: TPathD;
    dx, dy: double): TPathD; overload;
  function TranslatePath(const paths: TPathsD;
    dx, dy: double): TPathsD; overload;
  function TranslatePath(const ppp: TArrayOfPathsD;
    dx, dy: double): TArrayOfPathsD; overload;

  function Paths(const path: TPathD): TPathsD;
  {$IFDEF INLINING} inline; {$ENDIF}

  //CopyPath: note that only dynamic string arrays are copy-on-write
  function CopyPath(const path: TPathD): TPathD;
  {$IFDEF INLINING} inline; {$ENDIF}
  function CopyPaths(const paths: TPathsD): TPathsD;

  function ScalePoint(const pt: TPointD; scale: double): TPointD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
  function ScalePoint(const pt: TPointD; sx, sy: double): TPointD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
  function ScalePath(const path: TPathD;
    sx, sy: double): TPathD; overload;

  function ScalePath(const path: TPathD;
    scale: double): TPathD; overload;
  function ScalePath(const paths: TPathsD;
    sx, sy: double): TPathsD; overload;
  function ScalePath(const paths: TPathsD;
    scale: double): TPathsD; overload;

  function ScaleRect(const rec: TRect; scale: double): TRect; overload;
  function ScaleRect(const rec: TRectD; scale: double): TRectD; overload;
  function ScaleRect(const rec: TRect; sx, sy: double): TRect; overload;
  function ScaleRect(const rec: TRectD; sx, sy: double): TRectD; overload;

  function ScalePathToFit(const path: TPathD; const rec: TRect): TPathD;
  function ScalePathsToFit(const paths: TPathsD; const rec: TRect): TPathsD;

  function ReversePath(const path: TPathD): TPathD; overload;
  function ReversePath(const paths: TPathsD): TPathsD; overload;

  function OpenPathToFlatPolygon(const path: TPathD): TPathD;

  procedure AppendPoint(var path: TPathD; const extra: TPointD);

  // AppendPath - adds TPathD & TPathsD objects to the end of
  // TPathsD (or TArrayOfPathsD) objects
  procedure AppendPath(var paths: TPathsD; const extra: TPathD); overload;
  procedure AppendPath(var paths: TPathsD; const extra: TPathsD); overload;
  procedure AppendPath(var ppp: TArrayOfPathsD; const extra: TPathsD); overload;

  // ConcatPaths - concats multiple paths into a single path.
  // It also avoids point duplicates where path joins
  procedure ConcatPaths(var dstPath: TPathD; const path: TPathD); overload;
  procedure ConcatPaths(var dstPath: TPathD; const paths: TPathsD); overload;

  function GetAngle(const origin, pt: TPoint): double; overload;
  function GetAngle(const origin, pt: TPointD): double; overload;
  function GetAngle(const a, b, c: TPoint): double; overload;
  function GetAngle(const a, b, c: TPointD): double; overload;

  procedure GetSinCos(angle: double; out sinA, cosA: double);

  function GetPointAtAngleAndDist(const origin: TPointD;
    angle, distance: double): TPointD;

  function IntersectPoint(const ln1a, ln1b, ln2a, ln2b: TPointD): TPointD; overload;
  function IntersectPoint(const ln1a, ln1b, ln2a, ln2b: TPointD; out ip: TPointD): Boolean; overload;

  function SegmentIntersectPt(const ln1a, ln1b, ln2a, ln2b: TPointD): TPointD;
  function SegmentsIntersect(const ln1a, ln1b, ln2a, ln2b: TPointD;
    out ip: TPointD): Boolean;

  procedure RotatePoint(var pt: TPointD;
    const focalPoint: TPointD; sinA, cosA: double); overload;
  procedure RotatePoint(var pt: TPointD;
    const focalPoint: TPointD; angleRad: double); overload;

  function RotatePath(const path: TPathD;
    const focalPoint: TPointD; angleRads: double): TPathD; overload;
  function RotatePath(const paths: TPathsD;
    const focalPoint: TPointD; angleRads: double): TPathsD; overload;

  //function MakePath(const pts: array of integer): TPathD; overload;
  function MakePath(const pts: array of double): TPathD; overload;
  function MakePath(const pt: TPointD): TPathD; overload;

  function GetBounds(const path: TPathD): TRect; overload;
  function GetBounds(const paths: TPathsD): TRect; overload;

  function GetBoundsD(const path: TPathD): TRectD; overload;
  function GetBoundsD(const paths: TPathsD): TRectD; overload;
  function GetBoundsD(const paths: TArrayOfPathsD): TRectD; overload;

  function GetRotatedRectBounds(const rec: TRect; angle: double): TRect; overload;
  function GetRotatedRectBounds(const rec: TRectD; angle: double): TRectD; overload;

  function Rect(const recD: TRectD): TRect; overload;
  function Rect(const left,top,right,bottom: integer): TRect; overload;

  function PtInRect(const rec: TRectD; const pt: TPointD): Boolean; overload;

  function Size(cx, cy: integer): TSize;
  function SizeD(cx, cy: double): TSizeD;

  function IsClockwise(const path: TPathD): Boolean;

  // IsSimpleRectanglePath returns true if the specified path has only one polygon
  // with 4 points that describe a rectangle.
  function IsSimpleRectanglePath(const paths: TPathsD; var R: TRect): Boolean; overload;
  function IsSimpleRectanglePath(const path: TPathD; var R: TRect): Boolean; overload;

  function Area(const path: TPathD): Double; overload;

  function RectsEqual(const rec1, rec2: TRect): Boolean;

  procedure TranslateRect(var rec: TRect; dx, dy: integer); overload;
  procedure TranslateRect(var rec: TRectD; dx, dy: double); overload;

  function MakeSquare(rec: TRect): TRect;

  function IsValid(value: integer): Boolean; overload;
  function IsValid(value: double): Boolean; overload;
  function IsValid(const pt: TPoint): Boolean; overload;
  function IsValid(const pt: TPointD): Boolean; overload;
  function IsValid(const rec: TRect): Boolean; overload;

  function Point(X,Y: Integer): TPoint; overload;
  function Point(const pt: TPointD): TPoint; overload;

  function PointsEqual(const pt1, pt2: TPointD): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

  function PointsNearEqual(const pt1, pt2: TPoint;
    dist: integer): Boolean; overload;
  function PointsNearEqual(const pt1, pt2: TPointD;
    distSqrd: double): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

  function StripNearDuplicates(const path: TPathD;
    minDist: double; isClosedPath: Boolean): TPathD; overload;
  function StripNearDuplicates(const paths: TPathsD;
    minLength: double; isClosedPaths: Boolean): TPathsD; overload;

  function MidPoint(const rec: TRect): TPoint; overload;
  function MidPoint(const rec: TRectD): TPointD; overload;
  function MidPoint(const pt1, pt2: TPoint): TPoint; overload;
  function MidPoint(const pt1, pt2: TPointD): TPointD; overload;

  function Average(val1, val2: integer): integer; overload;
  function Average(val1, val2: double): double; overload;

  function ReflectPoint(const pt, pivot: TPointD): TPointD;
  {$IFDEF INLINING} inline; {$ENDIF}

  function RectsOverlap(const rec1, rec2: TRect): Boolean;

  function IsSameRect(const rec1, rec2: TRect): Boolean;

  function RectsIntersect(const rec1, rec2: TRect): Boolean; overload;
  function RectsIntersect(const rec1, rec2: TRectD): Boolean; overload;
  function IntersectRect(const rec1, rec2: TRectD): TRectD; overload;

  // UnionRect: this behaves differently to types.UnionRect
  // in that if either parameter is empty the other parameter is returned
  function UnionRect(const rec1, rec2: TRect): TRect; overload;
  function UnionRect(const rec1, rec2: TRectD): TRectD; overload;

  //these 2 functions are only needed to support older versions of Delphi
  function MakeArrayOfInteger(const ints: array of integer): TArrayOfInteger;
  function MakeArrayOfDouble(const doubles: array of double): TArrayOfDouble;

  function CrossProduct(const vector1, vector2: TPointD): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
  function CrossProduct(const pt1, pt2, pt3: TPointD): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
  function CrossProduct(const pt1, pt2, pt3, pt4: TPointD): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

  function DotProduct(const vector1, vector2: TPointD): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
  function DotProduct(const pt1, pt2, pt3: TPointD): double; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

  function TurnsLeft(const pt1, pt2, pt3: TPointD): boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
  function TurnsRight(const pt1, pt2, pt3: TPointD): boolean;
  {$IFDEF INLINING} inline; {$ENDIF}

  function IsPathConvex(const path: TPathD): Boolean;

  function NormalizeVector(const vec: TPointD): TPointD;
  {$IFDEF INLINING} inline; {$ENDIF}

  //GetUnitVector: Used internally
  function GetUnitVector(const pt1, pt2: TPointD): TPointD;

  //GetUnitNormal: Used internally
  function GetUnitNormal(const pt1, pt2: TPointD): TPointD; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
  function GetUnitNormal(const pt1, pt2: TPointD; out norm: TPointD): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}

  function GetAvgUnitVector(const vec1, vec2: TPointD): TPointD;
  {$IFDEF INLINING} inline; {$ENDIF}

  //GetVectors: Used internally
  function GetVectors(const path: TPathD): TPathD;
  //GetNormals: Used internally

  function GetNormals(const path: TPathD): TPathD;

  //DistanceSqrd: Used internally
  function DistanceSqrd(const pt1, pt2: TPoint): double; overload;
  {$IFDEF INLINE} inline; {$ENDIF}
  //DistanceSqrd: Used internally
  function DistanceSqrd(const pt1, pt2: TPointD): double; overload;
  {$IFDEF INLINE} inline; {$ENDIF}

  function Distance(const pt1, pt2: TPoint): double; overload;
  {$IFDEF INLINE} inline; {$ENDIF}
  function Distance(const pt1, pt2: TPointD): double; overload;
  {$IFDEF INLINE} inline; {$ENDIF}
  function Distance(const path: TPathD; stopAt: integer = 0): double; overload;

  function GetDistances(const path: TPathD): TArrayOfDouble;

  function GetCumulativeDistances(const path: TPathD): TArrayOfDouble;

  function PerpendicularDistSqrd(const pt, line1, line2: TPointD): double;

  function PointInPolygon(const pt: TPointD;
    const polygon: TPathD; fillRule: TFillRule): Boolean;

  function PointInPolygons(const pt: TPointD;
    const polygons: TPathsD; fillRule: TFillRule): Boolean;

  function PerpendicularDist(const pt, line1, line2: TPointD): double;

  function ClosestPointOnLine(const pt, linePt1, linePt2: TPointD): TPointD;

  function ClosestPointOnSegment(const pt, segPt1, segPt2: TPointD): TPointD;

  function IsPointInEllipse(const ellipseRec: TRect; const pt: TPoint): Boolean;

  //GetLineEllipseIntersects: Gets the intersection of a line and
  //an ellipse. The function succeeds when the line either touches
  //tangentially or passes through the ellipse. If the line touches
  //tangentially, the coordintates returned in pt1 and pt2 will match.
  function GetLineEllipseIntersects(const ellipseRec: TRect;
    var linePt1, linePt2: TPointD): Boolean;

  function GetPtOnEllipseFromAngle(const ellipseRect: TRectD; angle: double): TPointD;

  function GetPtOnRotatedEllipseFromAngle(const ellipseRect: TRectD;
    ellipseRotAngle, angle: double): TPointD;

  function GetEllipticalAngleFromPoint(const ellipseRect: TRectD;
    const pt: TPointD): double;

  function GetRotatedEllipticalAngleFromPoint(const ellipseRect: TRectD;
    ellipseRotAngle: double; pt: TPointD): double;

  function GetClosestPtOnRotatedEllipse(const ellipseRect: TRectD;
    ellipseRotation: double; const pt: TPointD): TPointD;

  // RoughOutline: outlines are **rough** because they will contain numerous
  // self-intersections and negative area regions. (This untidiness will be
  // hidden as long as the NonZero fill rule is applied when rendering, and
  // this function will be **much** faster than Img32.Clipper.InflatePaths.)
  // The 'scale' parameter doesn't actually scale the returned outline, it's
  // only a warning of future scaling and used to guide the returned precision.
  // RoughOutline is intended mostly for internal use.
  function RoughOutline(const line: TPathD; lineWidth: double;
    joinStyle: TJoinStyle; endStyle: TEndStyle;
    miterLim: double = 0; scale: double = 1.0): TPathsD; overload;
  function RoughOutline(const lines: TPathsD; lineWidth: double;
    joinStyle: TJoinStyle; endStyle: TEndStyle;
    miterLim: double = 0; scale: double = 1.0): TPathsD; overload;

  // Grow: For the same reasons stated in RoughOutline's comments above,
  // this function is also intended mostly for internal use
  function Grow(const path, normals: TPathD; delta: double;
    joinStyle: TJoinStyle; miterLim: double = 0; scale: double = 1.0; isOpen: Boolean = false): TPathD;

  function ValueAlmostZero(val: double; epsilon: double = 0.001): Boolean;
  function ValueAlmostOne(val: double; epsilon: double = 0.001): Boolean;
const
  Invalid       = -MaxInt;
  InvalidD      = -Infinity;
  NullPoint     : TPoint  = (X: 0; Y: 0);
  NullPointD    : TPointD = (X: 0; Y: 0);
  InvalidPoint  : TPoint  = (X: -MaxInt; Y: -MaxInt);
  InvalidPointD : TPointD = (X: -Infinity; Y: -Infinity);
  NullRect      : TRect = (left: 0; top: 0; right: 0; Bottom: 0);
  NullRectD     : TRectD = (left: 0; top: 0; right: 0; Bottom: 0);
  InvalidRect   : TRect = (left: MaxInt; top: MaxInt; right: 0; Bottom: 0);
  BezierTolerance: double  = 0.25;
  DoubleTolerance: double  = 1.0e-12;
var
  //AutoWidthThreshold: When JoinStyle = jsAuto, this is the threshold at
  //which line joins will be rounded instead of squared. With wider strokes,
  //rounded joins generally look better, but as rounding is more complex it
  //also requries more processing and hence is slower to execute.
  AutoWidthThreshold: double = 5.0;
  //When lines are too narrow, they become too faint to sensibly draw
  MinStrokeWidth: double = 0.5;
  //Miter limit avoids excessive spikes when line offsetting
  DefaultMiterLimit: double = 4.0;

resourcestring
  rsInvalidMatrix = 'Invalid matrix.'; //nb: always start with IdentityMatrix

implementation

uses
  Img32.Transform;

resourcestring
  rsInvalidQBezier = 'Invalid number of control points for a QBezier';
  rsInvalidCBezier = 'Invalid number of control points for a CBezier';

const
  BuffSize = 64;

{$IFDEF CPUX86}
  // Use faster Trunc for x86 code in this unit.
  Trunc: function(Value: Double): Integer = __Trunc;
{$ENDIF CPUX86}

//------------------------------------------------------------------------------
// TSizeD
//------------------------------------------------------------------------------

function TSizeD.average: double;
begin
  Result := (cx + cy) * 0.5;
end;

//------------------------------------------------------------------------------
// TRectWH record/object.
//------------------------------------------------------------------------------

function TRectWH.IsEmpty: Boolean;
begin
  Result := (Width <= 0) or (Height <= 0);
end;
//------------------------------------------------------------------------------

function TRectWH.IsValid: Boolean;
begin
  Result := (Left <> InvalidD) and (Top <> InvalidD)
    and (Width >= 0) and (Height >= 0);
end;
//------------------------------------------------------------------------------

function TRectWH.Right: double;
begin
  Result := Left + Width;
end;
//------------------------------------------------------------------------------

function TRectWH.Bottom: double;
begin
  Result := Top + Height;
end;
//------------------------------------------------------------------------------

function TRectWH.Contains(const Pt: TPoint): Boolean;
begin
  Result := (pt.X >= Left) and (pt.X <= Left + Width) and
    (pt.Y >= Top) and (pt.Y <= Top + Height)
end;
//------------------------------------------------------------------------------

function TRectWH.Contains(const Pt: TPointD): Boolean;
begin
  Result := (pt.X >= Left) and (pt.X <= Left + Width) and
    (pt.Y >= Top) and (pt.Y <= Top + Height)
end;
//------------------------------------------------------------------------------

function TRectWH.MidPoint: TPointD;
begin
  Result := PointD(left + Width * 0.5, top + Height * 0.5);
end;
//------------------------------------------------------------------------------

function TRectWH.RectD: TRectD;
begin
  Result := Img32.RectD(left, top, left + Width, top + Height);
end;
//------------------------------------------------------------------------------

function TRectWH.Rect: TRect;
begin
  Result := Img32.Vector.Rect(RectD);
end;
//------------------------------------------------------------------------------

function RectWH(left, top, width, height: integer): TRectWH;
begin
  Result.Left := left;
  Result.Top := top;
  Result.Width := width;
  Result.Height := height;
end;
//------------------------------------------------------------------------------

function RectWH(left, top, width, height: double): TRectWH;
begin
  Result.Left := left;
  Result.Top := top;
  Result.Width := width;
  Result.Height := height;
end;
//------------------------------------------------------------------------------

function RectWH(const rec: TRectD): TRectWH;
begin
  Result.Left := rec.left;
  Result.Top := rec.top;
  Result.Width := rec.width;
  Result.Height := rec.height;
end;
//------------------------------------------------------------------------------

function RectsEqual(const rec1, rec2: TRect): Boolean;
begin
  result := (rec1.Left = rec2.Left) and (rec1.Top = rec2.Top) and
    (rec1.Right = rec2.Right) and (rec1.Bottom = rec2.Bottom);
end;
//------------------------------------------------------------------------------

function Rect(const left, top, right, bottom: integer): TRect;
begin
  Result.Left := left;
  Result.Top := top;
  Result.Right := right;
  Result.Bottom := bottom;
end;
//------------------------------------------------------------------------------

function IsValid(value: integer): Boolean;
begin
  Result := value <> -MaxInt;
end;
//------------------------------------------------------------------------------

function IsValid(value: double): Boolean;
begin
  Result := value <> InvalidD;
end;
//------------------------------------------------------------------------------

function IsValid(const pt: TPoint): Boolean;
begin
  result := (pt.X <> Invalid) and (pt.Y <> Invalid);
end;
//------------------------------------------------------------------------------

function IsValid(const pt: TPointD): Boolean;
begin
  result := (pt.X <> -Infinity) and (pt.Y <> -Infinity);
end;
//------------------------------------------------------------------------------

function IsValid(const rec: TRect): Boolean;
begin
  result := (rec.Left <> MaxInt) and (rec.Top <> MaxInt);
end;
//------------------------------------------------------------------------------

function Point(X,Y: Integer): TPoint;
begin
  result.X := X;
  result.Y := Y;
end;
//------------------------------------------------------------------------------

function Point(const pt: TPointD): TPoint;
begin
  result.X := Round(pt.x);
  result.Y := Round(pt.y);
end;
//------------------------------------------------------------------------------

function PointsEqual(const pt1, pt2: TPointD): Boolean;
begin
  result := (pt1.X = pt2.X) and (pt1.Y = pt2.Y);
end;
//------------------------------------------------------------------------------

function PointsNearEqual(const pt1, pt2: TPoint; dist: integer): Boolean;
begin
  Result := (Abs(pt1.X - pt2.X) <= dist) and (Abs(pt1.Y - pt2.Y) < dist);
end;
//------------------------------------------------------------------------------

function PointsNearEqual(const pt1, pt2: TPointD; distSqrd: double): Boolean;
begin
  Result := Sqr(pt1.X - pt2.X) + Sqr(pt1.Y - pt2.Y) < distSqrd;
end;
//------------------------------------------------------------------------------

function StripNearDuplicates(const path: TPathD;
  minDist: double; isClosedPath: Boolean): TPathD;
var
  i,j, len: integer;
begin
  len := length(path);
  NewPointDArray(Result, len, True);
  if len = 0 then Exit;
  Result[0] := path[0];
  j := 0;
  minDist := minDist * minDist;
  for i := 1 to len -1 do
    if not PointsNearEqual(Result[j], path[i], minDist) then
    begin
      inc(j);
      Result[j] := path[i];
    end;
  if isClosedPath and
    PointsNearEqual(Result[j], Result[0], minDist) then dec(j);
  SetLength(Result, j +1);
end;
//------------------------------------------------------------------------------

function StripNearDuplicates(const paths: TPathsD;
  minLength: double; isClosedPaths: Boolean): TPathsD;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := StripNearDuplicates(paths[i], minLength, isClosedPaths);
end;
//------------------------------------------------------------------------------

function ValueAlmostZero(val: double; epsilon: double = 0.001): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := Abs(val) < epsilon;
end;
//------------------------------------------------------------------------------

function ValueAlmostOne(val: double; epsilon: double = 0.001): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := Abs(val-1) < epsilon;
end;
//------------------------------------------------------------------------------

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

function GetRotatedRectBounds(const rec: TRect; angle: double): TRect;
var
  p: TPathD;
  mp: TPointD;
begin
  p := Rectangle(rec);
  mp := PointD((rec.Left + rec.Right)/2, (rec.Top + rec.Bottom)/2);
  if angle <> 0 then
    p := RotatePath(p, mp, angle);
  Result := GetBounds(p);
end;
//------------------------------------------------------------------------------

function GetRotatedRectBounds(const rec: TRectD; angle: double): TRectD;
var
  p: TPathD;
  mp: TPointD;
begin
  p := Rectangle(rec);
  mp := PointD((rec.Left + rec.Right)/2, (rec.Top + rec.Bottom)/2);
  if angle <> 0 then
    p := RotatePath(p, mp, angle);
  Result := GetBoundsD(p);
end;
//------------------------------------------------------------------------------

function Rect(const recD: TRectD): TRect;
begin
  // see https://github.com/AngusJohnson/Image32/issues/15
  Result.Left := Floor(recD.Left + DoubleTolerance);
  Result.Top := Floor(recD.Top + DoubleTolerance);
  Result.Right := Ceil(recD.Right - DoubleTolerance);
  Result.Bottom := Ceil(recD.Bottom - DoubleTolerance);
end;
//------------------------------------------------------------------------------

function PtInRect(const rec: TRectD; const pt: TPointD): Boolean;
begin
  Result := (pt.X >= rec.Left) and (pt.X < rec.Right) and
    (pt.Y >= rec.Top) and (pt.Y < rec.Bottom);
end;
//------------------------------------------------------------------------------

function Size(cx, cy: integer): TSize;
begin
  Result.cx := cx;
  Result.cy := cy;
end;
//------------------------------------------------------------------------------

function SizeD(cx, cy: double): TSizeD;
begin
  Result.cx := cx;
  Result.cy := cy;
end;
//------------------------------------------------------------------------------

function IsClockwise(const path: TPathD): Boolean;
begin
  Result := Area(path) > 0;
end;
//------------------------------------------------------------------------------

function IsSimpleRectanglePath(const path: TPathD; var R: TRect): Boolean;
type
  TLastMatch = (lmX, lmY);
var
  i: Integer;
  lastMatch: TLastMatch;
begin
  Result := False;
  // If we have a single path with 4 points, it could be a rectangle
  if Length(path) = 4 then
  begin
    // For a rectangle the X and Y coordinates of the points alternate
    // in being equal
    if path[0].X = path[3].X then
      lastMatch := lmX
    else if path[0].Y = path[3].Y then
      lastMatch := lmY
    else
      Exit;

    R.Left := Trunc(path[0].X);
    R.Top := Trunc(path[0].Y);
    R.Right := Ceil(path[0].X);
    R.Bottom := Ceil(path[0].Y);
    for i := 1 to 3 do
    begin
      case lastMatch of
        lmY: // now the X-coordinates must be equal
          begin
            if path[i].X <> path[i - 1].X then Exit;
            lastMatch := lmX;
            R.Top := Min(R.Top, Trunc(path[i].Y));
            R.Bottom := Max(R.Bottom, Ceil(path[i].Y));
          end;
        lmX: // now the Y-coordinates must be equal
          begin
            if path[i].Y <> path[i - 1].Y then Exit;
            lastMatch := lmY;
            R.Left := Min(R.Left, Trunc(path[i].X));
            R.Right := Max(R.Right, Ceil(path[i].X));
          end;
      end;
    end;
    Result := True;
  end;
end;

//------------------------------------------------------------------------------
function IsSimpleRectanglePath(const paths: TPathsD; var R: TRect): Boolean;
begin
  if (Length(paths) = 1) and (Length(paths[0]) = 4) then
    Result := IsSimpleRectanglePath(paths[0], r)
  else
    Result := False;
end;
//------------------------------------------------------------------------------

function Area(const path: TPathD): Double;
var
  i, j, highI: Integer;
  d: Double;
begin
  Result := 0.0;
  highI := High(path);
  if (highI < 2) then Exit;
  j := highI;
  for i := 0 to highI do
  begin
    d := (path[j].X + path[i].X);
    Result := Result + d * (path[j].Y - path[i].Y);
    j := i;
  end;
  Result := -Result * 0.5;
end;
//------------------------------------------------------------------------------

procedure TranslateRect(var rec: TRect; dx, dy: integer);
begin
  rec.Left := rec.Left + dx;
  rec.Top := rec.Top + dy;
  rec.Right := rec.Right + dx;
  rec.Bottom := rec.Bottom + dy;
end;
//------------------------------------------------------------------------------

procedure TranslateRect(var rec: TRectD; dx, dy: double);
begin
  rec.Left := rec.Left + dx;
  rec.Top := rec.Top + dy;
  rec.Right := rec.Right + dx;
  rec.Bottom := rec.Bottom + dy;
end;
//------------------------------------------------------------------------------

function MakeSquare(rec: TRect): TRect;
var
  i: integer;
begin
  Result := rec;
  i := ((rec.Right - rec.Left) + (rec.Bottom - rec.Top)) div 2;
  Result.Right := Result.Left + i;
  Result.Bottom := Result.Top + i;
end;
//------------------------------------------------------------------------------

function MidPoint(const rec: TRect): TPoint;
begin
  Result.X := (rec.Left + rec.Right) div 2;
  Result.Y := (rec.Top + rec.Bottom) div 2;
end;
//------------------------------------------------------------------------------

function MidPoint(const rec: TRectD): TPointD;
begin
  Result.X := (rec.Left + rec.Right) * 0.5;
  Result.Y := (rec.Top + rec.Bottom) * 0.5;
end;
//------------------------------------------------------------------------------

function MidPoint(const pt1, pt2: TPoint): TPoint;
begin
  Result.X := (pt1.X + pt2.X) div 2;
  Result.Y := (pt1.Y + pt2.Y) div 2;
end;
//------------------------------------------------------------------------------

function MidPoint(const pt1, pt2: TPointD): TPointD;
begin
  Result.X := (pt1.X + pt2.X) * 0.5;
  Result.Y := (pt1.Y + pt2.Y) * 0.5;
end;
//------------------------------------------------------------------------------

function Average(val1, val2: integer): integer;
begin
  Result := (val1 + val2) div 2;
end;
//------------------------------------------------------------------------------

function Average(val1, val2: double): double;
begin
  Result := (val1 + val2) * 0.5;
end;
//------------------------------------------------------------------------------

function RectsOverlap(const rec1, rec2: TRect): Boolean;
begin
  Result := (rec1.Left < rec2.Right) and (rec1.Right > rec2.Left) and
     (rec1.Top < rec2.Bottom) and (rec1.Bottom > rec2.Top);
end;
//------------------------------------------------------------------------------

function IsSameRect(const rec1, rec2: TRect): Boolean;
begin
  Result := (rec1.Left = rec2.Left) and (rec1.Top = rec2.Top) and
    (rec1.Right = rec2.Right) and (rec1.Bottom = rec2.Bottom);
end;
//------------------------------------------------------------------------------

function RectsIntersect(const rec1, rec2: TRect): Boolean;
var
  dummy: TRect;
begin
  Result := Types.IntersectRect(dummy, rec1, rec2);
end;
//------------------------------------------------------------------------------

function RectsIntersect(const rec1, rec2: TRectD): Boolean;
begin
  Result := not IntersectRect(rec1, rec2).IsEmpty;
end;
//------------------------------------------------------------------------------

function IntersectRect(const rec1, rec2: TRectD): TRectD;
begin
  result.Left := Max(rec1.Left, rec2.Left);
  result.Top := Max(rec1.Top, rec2.Top);
  result.Right := Min(rec1.Right, rec2.Right);
  result.Bottom := Min(rec1.Bottom, rec2.Bottom);
end;
//------------------------------------------------------------------------------

function UnionRect(const rec1, rec2: TRect): TRect;
begin
  if IsEmptyRect(rec1) then
    Result := rec2
  else if IsEmptyRect(rec2) then
    Result := rec1
  else
  begin
    result.Left := Min(rec1.Left, rec2.Left);
    result.Top := Min(rec1.Top, rec2.Top);
    result.Right := Max(rec1.Right, rec2.Right);
    result.Bottom := Max(rec1.Bottom, rec2.Bottom);
  end;
end;
//------------------------------------------------------------------------------

function UnionRect(const rec1, rec2: TRectD): TRectD;
begin
  if IsEmptyRect(rec1) then
    Result := rec2
  else if IsEmptyRect(rec2) then
    Result := rec1
  else
  begin
    result.Left := Min(rec1.Left, rec2.Left);
    result.Top := Min(rec1.Top, rec2.Top);
    result.Right := Max(rec1.Right, rec2.Right);
    result.Bottom := Max(rec1.Bottom, rec2.Bottom);
  end;
end;
//------------------------------------------------------------------------------

function MakeArrayOfInteger(const ints: array of integer): TArrayOfInteger;
var
  i, len: integer;
begin
  len := Length(ints);
  NewIntegerArray(Result, len, True);
  for i := 0 to len -1 do Result[i] := ints[i];
end;
//------------------------------------------------------------------------------

function MakeArrayOfDouble(const doubles: array of double): TArrayOfDouble;
var
  i, len: integer;
begin
  len := Length(doubles);
  SetLength(Result, len);
  for i := 0 to len -1 do Result[i] := doubles[i];
end;
//------------------------------------------------------------------------------

function CrossProduct(const vector1, vector2: TPointD): double;
begin
  result := vector1.X * vector2.Y - vector2.X * vector1.Y;
end;
//------------------------------------------------------------------------------

function CrossProduct(const pt1, pt2, pt3: TPointD): double;
var
  x1,x2,y1,y2: double;
begin
  x1 := pt2.X - pt1.X;
  y1 := pt2.Y - pt1.Y;
  x2 := pt3.X - pt2.X;
  y2 := pt3.Y - pt2.Y;
  result := (x1 * y2 - y1 * x2);
end;
//---------------------------------------------------------------------------

function CrossProduct(const pt1, pt2, pt3, pt4: TPointD): double;
var
  x1,x2,y1,y2: double;
begin
  x1 := pt2.X - pt1.X;
  y1 := pt2.Y - pt1.Y;
  x2 := pt4.X - pt3.X;
  y2 := pt4.Y - pt3.Y;
  result := (x1 * y2 - y1 * x2);
end;
//---------------------------------------------------------------------------

function DotProduct(const vector1, vector2: TPointD): double;
begin
  result := vector1.X * vector2.X + vector1.Y * vector2.Y;
end;
//------------------------------------------------------------------------------

function DotProduct(const pt1, pt2, pt3: TPointD): double;
var
  x1,x2,y1,y2: double;
begin
  x1 := pt2.X - pt1.X;
  y1 := pt2.Y - pt1.Y;
  x2 := pt2.X - pt3.X;
  y2 := pt2.Y - pt3.Y;
  result := (x1 * x2 + y1 * y2);
end;
//------------------------------------------------------------------------------

function TurnsLeft(const pt1, pt2, pt3: TPointD): boolean;
begin
  result := CrossProduct(pt1, pt2, pt3) < 0;
end;
//------------------------------------------------------------------------------

function TurnsRight(const pt1, pt2, pt3: TPointD): boolean;
begin
  result := CrossProduct(pt1, pt2, pt3) > 0;
end;
//------------------------------------------------------------------------------

function IsPathConvex(const path: TPathD): Boolean;
var
  i, pathLen: integer;
  dir: boolean;
begin
  result := false;
  pathLen := length(path);
  if pathLen < 3 then Exit;
  //get the winding direction of the first angle
  dir := TurnsRight(path[0], path[1], path[2]);
  //check that each other angle has the same winding direction
  for i := 1 to pathLen -1 do
    if TurnsRight(path[i], path[(i+1) mod pathLen],
      path[(i+2) mod pathLen]) <> dir then Exit;
  result := true;
end;
//------------------------------------------------------------------------------

function GetUnitVector(const pt1, pt2: TPointD): TPointD;
var
  dx, dy, inverseHypot: Double;
begin
  if (pt1.x = pt2.x) and (pt1.y = pt2.y) then
  begin
    Result.X := 0;
    Result.Y := 0;
    Exit;
  end;
  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);
  inverseHypot := 1 / Hypot(dx, dy);
  dx := dx * inverseHypot;
  dy := dy * inverseHypot;
  Result.X := dx;
  Result.Y := dy;
end;
//------------------------------------------------------------------------------

function GetUnitNormal(const pt1, pt2: TPointD): TPointD;
begin
  if not GetUnitNormal(pt1, pt2, Result) then
    Result := NullPointD;
end;
//------------------------------------------------------------------------------

function GetUnitNormal(const pt1, pt2: TPointD; out norm: TPointD): Boolean;
var
  dx, dy, inverseHypot: Double;
begin
  result := not PointsNearEqual(pt1, pt2, 0.001);
  if not result then Exit;
  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);
  inverseHypot := 1 / Hypot(dx, dy);
  dx := dx * inverseHypot;
  dy := dy * inverseHypot;
  norm.X := dy;
  norm.Y := -dx
end;
//------------------------------------------------------------------------------

function NormalizeVector(const vec: TPointD): TPointD;
var
  h, inverseHypot: Double;
begin
  h := Hypot(vec.X, vec.Y);
  if ValueAlmostZero(h, 0.001) then
  begin
    Result := NullPointD;
    Exit;
  end;
  inverseHypot := 1 / h;
  Result.X := vec.X * inverseHypot;
  Result.Y := vec.Y * inverseHypot;
end;
//------------------------------------------------------------------------------

function GetAvgUnitVector(const vec1, vec2: TPointD): TPointD;
begin
  Result := NormalizeVector(PointD(vec1.X + vec2.X, vec1.Y + vec2.Y));
end;
//------------------------------------------------------------------------------

function Paths(const path: TPathD): TPathsD;
begin
  SetLength(Result, 1);
  result[0] := Copy(path, 0, length(path));
end;
//------------------------------------------------------------------------------

function CopyPath(const path: TPathD): TPathD;
begin
  Result := Copy(path, 0, Length(path));
end;
//------------------------------------------------------------------------------

function CopyPaths(const paths: TPathsD): TPathsD;
var
  i, len1: integer;
begin
  len1 := length(paths);
  setLength(result, len1);
  for i := 0 to len1 -1 do
    result[i] := Copy(paths[i], 0, length(paths[i]));
end;
//------------------------------------------------------------------------------

function TranslatePoint(const pt: TPoint; dx, dy: integer): TPoint;
begin
  result.x := pt.x + dx;
  result.y := pt.y + dy;
end;
//------------------------------------------------------------------------------

function TranslatePoint(const pt: TPointD; dx, dy: double): TPointD;
begin
  result.x := pt.x + dx;
  result.y := pt.y + dy;
end;
//------------------------------------------------------------------------------

function TranslatePath(const path: TPathD; dx, dy: double): TPathD;
var
  i, len: integer;
begin
  len := length(path);
  NewPointDArray(result, len, True);
  for i := 0 to len -1 do
  begin
    result[i].x := path[i].x + dx;
    result[i].y := path[i].y + dy;
  end;
end;
//------------------------------------------------------------------------------

function TranslatePath(const paths: TPathsD;
  dx, dy: double): TPathsD;
var
  i,len: integer;
begin
  len := length(paths);
  setLength(result, len);
  for i := 0 to len -1 do
    result[i] := TranslatePath(paths[i], dx, dy);
end;
//------------------------------------------------------------------------------

function TranslatePath(const ppp: TArrayOfPathsD; dx, dy: double): TArrayOfPathsD;
var
  i,len: integer;
begin
  len := length(ppp);
  setLength(result, len);
  for i := 0 to len -1 do
    result[i] := TranslatePath(ppp[i], dx, dy);
end;
//------------------------------------------------------------------------------

function ScalePoint(const pt: TPointD; scale: double): TPointD;
begin
  Result.X := pt.X * scale;
  Result.Y := pt.Y * scale;
end;
//------------------------------------------------------------------------------

function ScalePoint(const pt: TPointD; sx, sy: double): TPointD;
begin
  Result.X := pt.X * sx;
  Result.Y := pt.Y * sy;
end;
//------------------------------------------------------------------------------

function ScalePath(const path: TPathD; sx, sy: double): TPathD;
var
  i, len: integer;
begin
  if (sx = 0) or (sy = 0) then
    Result := nil
  else if ((sx = 1) and (sy = 1)) then
  begin
    Result := Copy(path, 0, Length(path));
  end else
  begin
    len := length(path);
    NewPointDArray(result, len, True);
    for i := 0 to len -1 do
    begin
      result[i].x := path[i].x * sx;
      result[i].y := path[i].y * sy;
    end;
  end;
end;
//------------------------------------------------------------------------------

function ScalePath(const path: TPathD;
  scale: double): TPathD;
begin
  result := ScalePath(path, scale, scale);
end;
//------------------------------------------------------------------------------

function ScalePath(const paths: TPathsD;
  sx, sy: double): TPathsD;
var
  i,len: integer;
begin
  len := length(paths);
  setLength(result, len);
  for i := 0 to len -1 do
    result[i] := ScalePath(paths[i], sx, sy);
end;
//------------------------------------------------------------------------------

function ScalePath(const paths: TPathsD;
  scale: double): TPathsD;
begin
  result := ScalePath(paths, scale, scale);
end;
//------------------------------------------------------------------------------

function ScaleRect(const rec: TRect; scale: double): TRect;
begin
  result := rec;
  Result.Left := Round(Result.Left * scale);
  Result.Top := Round(Result.Top * scale);
  Result.Right := Round(Result.Right * scale);
  Result.Bottom := Round(Result.Bottom * scale);
end;
//------------------------------------------------------------------------------

function ScaleRect(const rec: TRect; sx, sy: double): TRect;
begin
  result := rec;
  Result.Left := Round(Result.Left * sx);
  Result.Top := Round(Result.Top * sy);
  Result.Right := Round(Result.Right * sx);
  Result.Bottom := Round(Result.Bottom * sy);
end;
//------------------------------------------------------------------------------

function ScaleRect(const rec: TRectD; scale: double): TRectD;
begin
  result := rec;
  Result.Left := Result.Left * scale;
  Result.Top := Result.Top * scale;
  Result.Right := Result.Right * scale;
  Result.Bottom := Result.Bottom * scale;
end;
//------------------------------------------------------------------------------

function ScaleRect(const rec: TRectD; sx, sy: double): TRectD;
begin
  result := rec;
  Result.Left := Result.Left * sx;
  Result.Top := Result.Top * sy;
  Result.Right := Result.Right * sx;
  Result.Bottom := Result.Bottom * sy;
end;
//------------------------------------------------------------------------------

function ScalePathToFit(const path: TPathD; const rec: TRect): TPathD;
var
  pathWidth, pathHeight, outHeight, outWidth: integer;
  pathBounds: TRect;
  scale: double;
begin
  pathBounds := GetBounds(path);
  RectWidthHeight(pathBounds, pathWidth, pathHeight);
  RectWidthHeight(rec, outWidth, outHeight);
  Result := TranslatePath(path,
    rec.Left - pathBounds.Left, rec.Top - pathBounds.Top);
  if outWidth / pathWidth < outHeight / pathHeight then
    scale := outWidth / pathWidth else
    scale := outHeight / pathHeight;
  Result := ScalePath(Result, scale, scale);
end;
//------------------------------------------------------------------------------

function ScalePathsToFit(const paths: TPathsD; const rec: TRect): TPathsD;
var
  pathWidth, pathHeight, outHeight, outWidth: integer;
  pathBounds: TRect;
  scale: double;
begin
  pathBounds := GetBounds(paths);
  RectWidthHeight(pathBounds, pathWidth, pathHeight);
  RectWidthHeight(rec, outWidth, outHeight);
  Result := TranslatePath(paths,
    rec.Left - pathBounds.Left, rec.Top - pathBounds.Top);
  if outWidth / pathWidth < outHeight / pathHeight then
    scale := outWidth / pathWidth else
    scale := outHeight / pathHeight;
  Result := ScalePath(Result, scale, scale);
end;
//------------------------------------------------------------------------------

function ReversePath(const path: TPathD): TPathD;
var
  i, highI: integer;
begin
  highI := High(path);
  NewPointDArray(result, highI +1, True);
  for i := 0 to highI do
    result[i] := path[highI -i];
end;
//------------------------------------------------------------------------------

function ReversePath(const paths: TPathsD): TPathsD;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(result, len);
  for i := 0 to len -1 do
    result[i] := ReversePath(paths[i]);
end;
//------------------------------------------------------------------------------

function OpenPathToFlatPolygon(const path: TPathD): TPathD;
var
  i, len, len2: integer;
begin
  len := Length(path);
  len2 := Max(0, len - 2);
  NewPointDArray(Result, len + len2, True);
  if len = 0 then Exit;
  Move(path[0], Result[0], len * SizeOf(TPointD));
  if len2 = 0 then Exit;
  for i := 0 to len - 3 do
    result[len + i] := path[len - 2 -i];
end;
//------------------------------------------------------------------------------

function GetVectors(const path: TPathD): TPathD;
var
  i,j, len: cardinal;
  pt: TPointD;
begin
  len := length(path);
  NewPointDArray(result, len, True);
  if len = 0 then Exit;
  pt := path[0];
  //skip duplicates
  i := len -1;
  while (i > 0) and
    (path[i].X = pt.X) and (path[i].Y = pt.Y) do dec(i);
  if (i = 0) then
  begin
    //all points are equal!
    for i := 0 to len -1 do result[i] := PointD(0,0);
    Exit;
  end;
  result[i] := GetUnitVector(path[i], pt);
  //fix up any duplicates at the end of the path
  for j := i +1 to len -1 do
    result[j] := result[j-1];
  //with at least one valid vector, we can now
  //safely get the remaining vectors
  pt := path[i];
  for i := i -1 downto 0 do
  begin
    if (path[i].X <> pt.X) or (path[i].Y <> pt.Y) then
    begin
      result[i] := GetUnitVector(path[i], pt);
      pt := path[i];
    end else
      result[i] := result[i+1]
  end;
end;
//------------------------------------------------------------------------------

function GetNormals(const path: TPathD): TPathD;
var
  i, highI: integer;
  last: TPointD;
begin
  highI := High(path);
  NewPointDArray(result, highI+1, True);
  if highI < 0 then Exit;

  last := NullPointD;
  for i := 0 to highI -1 do
  begin
    if GetUnitNormal(path[i], path[i+1], result[i]) then
      last := result[i] else
      result[i] := last;
  end;
  if GetUnitNormal(path[highI], path[0], result[highI]) then
    last := result[highI];

  for i := 0 to highI do
  begin
    if (result[i].X <> 0) or (result[i].Y <> 0) then Break;
    result[i] := last;
  end;

end;
//------------------------------------------------------------------------------

function DistanceSqrd(const pt1, pt2: TPoint): double;
begin
  result := Sqr(pt1.X - pt2.X) + Sqr(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function DistanceSqrd(const pt1, pt2: TPointD): double;
begin
  result := Sqr(pt1.X - pt2.X) + Sqr(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function Distance(const pt1, pt2: TPoint): double;
begin
  Result := Sqrt(DistanceSqrd(pt1, pt2));
end;
//------------------------------------------------------------------------------

function Distance(const pt1, pt2: TPointD): double;
begin
  Result := Sqrt(DistanceSqrd(pt1, pt2));
end;
//------------------------------------------------------------------------------

function Distance(const path: TPathD; stopAt: integer): double;
var
  i, highI: integer;
begin
  Result := 0;
  highI := High(path);
  if (stopAt > 0) and (stopAt < HighI) then highI := stopAt;
  for i := 1 to highI do
    Result := Result + Distance(path[i-1],path[i]);
end;
//------------------------------------------------------------------------------

function GetDistances(const path: TPathD): TArrayOfDouble;
var
  i, len: integer;
begin
  len := Length(path);
  SetLength(Result, len);
  if len = 0 then Exit;
  Result[0] := 0;
  for i := 1 to len -1 do
    Result[i] := Distance(path[i-1], path[i]);
end;
//------------------------------------------------------------------------------

function GetCumulativeDistances(const path: TPathD): TArrayOfDouble;
var
  i, len: integer;
begin
  len := Length(path);
  SetLength(Result, len);
  if len = 0 then Exit;
  Result[0] := 0;
  for i := 1 to len -1 do
    Result[i] := Result[i-1] + Distance(path[i-1], path[i]);
end;
//------------------------------------------------------------------------------

function PerpendicularDistSqrd(const pt, line1, line2: TPointD): double;
var
  a,b,c,d: double;
begin
  if PointsEqual(line1, line2) then
  begin
    Result := DistanceSqrd(pt, line1);
  end else
  begin
    a := pt.X - line1.X;
    b := pt.Y - line1.Y;
    c := line2.X - line1.X;
    d := line2.Y - line1.Y;
    if (c = 0) and (d = 0) then
      result := 0 else
      result := Sqr(a * d - c * b) / (c * c + d * d);
  end;
end;
//------------------------------------------------------------------------------

function PointInPolyWindingCount(const pt: TPointD;
  const path: TPathD; out PointOnEdgeDir: integer): integer;
var
  i, len: integer;
  prevPt: TPointD;
  isAbove: Boolean;
  crossProd: double;
begin
  //nb: PointOnEdgeDir == 0 unless 'pt' is on 'path'
  Result := 0;
  PointOnEdgeDir := 0;
  i := 0;
  len := Length(path);
  if len = 0 then Exit;
  prevPt := path[len-1];
  while (i < len) and (path[i].Y = prevPt.Y) do inc(i);
  if i = len then Exit;
  isAbove := (prevPt.Y < pt.Y);
  while (i < len) do
  begin
    if isAbove then
    begin
      while (i < len) and (path[i].Y < pt.Y) do inc(i);
      if i = len then break
      else if i > 0 then prevPt := path[i -1];
      crossProd := CrossProduct(prevPt, path[i], pt);
      if crossProd = 0 then
      begin
        PointOnEdgeDir := -1;
        //nb: could safely exit here with frNonZero or frEvenOdd fill rules
      end
      else if crossProd < 0 then dec(Result);
    end else
    begin
      while (i < len) and (path[i].Y > pt.Y) do inc(i);
      if i = len then break
      else if i > 0 then prevPt := path[i -1];
      crossProd := CrossProduct(prevPt, path[i], pt);
      if crossProd = 0 then
      begin
        PointOnEdgeDir := 1;
        //nb: could safely exit here with frNonZero or frEvenOdd fill rules
      end
      else if crossProd > 0 then inc(Result);
    end;
    inc(i);
    isAbove := not isAbove;
  end;
end;
//------------------------------------------------------------------------------

function PointInPolygon(const pt: TPointD;
  const polygon: TPathD; fillRule: TFillRule): Boolean;
var
  wc: integer;
  PointOnEdgeDir: integer;
begin
  wc := PointInPolyWindingCount(pt, polygon, PointOnEdgeDir);
  case fillRule of
    frEvenOdd: result := (PointOnEdgeDir <> 0)  or Odd(wc);
    frNonZero: result := (PointOnEdgeDir <> 0)  or (wc <> 0);
    frPositive: result := (PointOnEdgeDir + wc > 0);
    else {frNegative} result := (PointOnEdgeDir + wc < 0);
  end;
end;
//------------------------------------------------------------------------------

function PointInPolysWindingCount(const pt: TPointD;
  const paths: TPathsD; out PointOnEdgeDir: integer): integer;
var
  i,j, len: integer;
  p: TPathD;
  prevPt: TPointD;
  isAbove: Boolean;
  crossProd: double;
begin
  //nb: PointOnEdgeDir == 0 unless 'pt' is on 'path'
  Result := 0;
  PointOnEdgeDir := 0;
  for i := 0 to High(paths) do
  begin
    j := 0;
    p := paths[i];
    len := Length(p);
    if len < 3 then Continue;
    prevPt := p[len-1];
    while (j < len) and (p[j].Y = prevPt.Y) do inc(j);
    if j = len then continue;
    isAbove := (prevPt.Y < pt.Y);
    while (j < len) do
    begin
      if isAbove then
      begin
        while (j < len) and (p[j].Y < pt.Y) do inc(j);
        if j = len then break
        else if j > 0 then prevPt := p[j -1];
        crossProd := CrossProduct(prevPt, p[j], pt);
        if crossProd = 0 then PointOnEdgeDir := -1
        else if crossProd < 0 then dec(Result);
      end else
      begin
        while (j < len) and (p[j].Y > pt.Y) do inc(j);
        if j = len then break
        else if j > 0 then prevPt := p[j -1];
        crossProd := CrossProduct(prevPt, p[j], pt);
        if crossProd = 0 then PointOnEdgeDir := 1
        else if crossProd > 0 then inc(Result);
      end;
      inc(j);
      isAbove := not isAbove;
    end;
  end;
end;
//------------------------------------------------------------------------------

function PointInPolygons(const pt: TPointD;
  const polygons: TPathsD; fillRule: TFillRule): Boolean;
var
  wc: integer;
  PointOnEdgeDir: integer;
begin
  wc := PointInPolysWindingCount(pt, polygons, PointOnEdgeDir);
  case fillRule of
    frEvenOdd: result := (PointOnEdgeDir <> 0) or Odd(wc);
    frNonZero: result := (PointOnEdgeDir <> 0) or (wc <> 0);
    frPositive: result := (PointOnEdgeDir + wc > 0);
    else {frNegative} result := (PointOnEdgeDir + wc < 0);
  end;
end;
//------------------------------------------------------------------------------

function PerpendicularDist(const pt, line1, line2: TPointD): double;
var
  a,b,c,d: double;
begin
  //given: cross product of 2 vectors = area of parallelogram
  //and given: area of parallelogram = length base * height
  //height (ie perpendic. dist.) = cross product of 2 vectors / length base
  a := pt.X - line1.X;
  b := pt.Y - line1.Y;
  c := line2.X - line1.X;
  d := line2.Y - line1.Y;
  result := abs(a * d - c * b) / Sqrt(c * c + d * d);
end;
//------------------------------------------------------------------------------

function ClosestPoint(const pt, linePt1, linePt2: TPointD;
  constrainToSegment: Boolean): TPointD;
var
  q: double;
begin
  if (linePt1.X = linePt2.X) and (linePt1.Y = linePt2.Y) then
  begin
    Result := linePt1;
  end else
  begin
    q := ((pt.X-linePt1.X)*(linePt2.X-linePt1.X) +
      (pt.Y-linePt1.Y)*(linePt2.Y-linePt1.Y)) /
      (sqr(linePt2.X-linePt1.X) + sqr(linePt2.Y-linePt1.Y));
    if constrainToSegment then
    begin
      if q < 0 then q := 0 else if q > 1 then q := 1;
    end;
    Result.X := (1-q)*linePt1.X + q*linePt2.X;
    Result.Y := (1-q)*linePt1.Y + q*linePt2.Y;
  end;
end;
//------------------------------------------------------------------------------

function ClosestPointOnLine(const pt, linePt1, linePt2: TPointD): TPointD;
begin
  result := ClosestPoint(pt, linePt1, linePt2, false);
end;
//------------------------------------------------------------------------------

function ClosestPointOnSegment(const pt, segPt1, segPt2: TPointD): TPointD;
begin
  result := ClosestPoint(pt, segPt1, segPt2, true);
end;
//------------------------------------------------------------------------------

function GetPtOnEllipseFromAngle(const ellipseRect: TRectD;
  angle: double): TPointD;
var
  sn, co: double;
begin
  NormalizeAngle(angle);
  GetSinCos(angle, sn, co);
  Result.X := ellipseRect.MidPoint.X + ellipseRect.Width/2 * co;
  Result.Y := ellipseRect.MidPoint.Y + ellipseRect.Height/2 * sn;
end;
//------------------------------------------------------------------------------

function GetEllipticalAngleFromPoint(const ellipseRect: TRectD;
  const pt: TPointD): double;
begin
  with ellipseRect do
    Result := ArcTan2(Width/Height * (pt.Y - MidPoint.Y), (pt.X - MidPoint.X));
end;
//------------------------------------------------------------------------------

function GetRotatedEllipticalAngleFromPoint(const ellipseRect: TRectD;
  ellipseRotAngle: double; pt: TPointD): double;
begin
  Result := 0;
  if ellipseRect.IsEmpty then Exit;
  RotatePoint(pt, ellipseRect.MidPoint, -ellipseRotAngle);
  Result := GetEllipticalAngleFromPoint(ellipseRect, pt);
end;
//------------------------------------------------------------------------------

function GetPtOnRotatedEllipseFromAngle(const ellipseRect: TRectD;
  ellipseRotAngle, angle: double): TPointD;
begin
  Result := GetPtOnEllipseFromAngle(ellipseRect, angle);
  if ellipseRotAngle <> 0 then
    img32.Vector.RotatePoint(Result, ellipseRect.MidPoint, ellipseRotAngle);
end;
//------------------------------------------------------------------------------

function GetClosestPtOnRotatedEllipse(const ellipseRect: TRectD;
  ellipseRotation: double; const pt: TPointD): TPointD;
var
  pt2: TPointD;
  angle: double;
begin
  pt2 := pt;
  Img32.Vector.RotatePoint(pt2, ellipseRect.MidPoint, -ellipseRotation);
  angle := GetEllipticalAngleFromPoint(ellipseRect, pt2);
  Result := GetPtOnEllipseFromAngle(ellipseRect, angle);
  Img32.Vector.RotatePoint(Result, ellipseRect.MidPoint, ellipseRotation);
end;
//------------------------------------------------------------------------------

function IsPointInEllipse(const ellipseRec: TRect; const pt: TPoint): Boolean;
var
  rec: TRectD;
  w,h: integer;
  x,y, y2, a,b, dx,dy: double;
begin
  RectWidthHeight(ellipseRec, w, h);
  a := w * 0.5;
  b := h * 0.5;
  dx := ellipseRec.Left + a;
  dy := ellipseRec.Top + b;
  rec := RectD(ellipseRec);
  TranslateRect(rec, -dx, -dy);
  x := pt.X -dx; y := pt.Y -dy;
  //first make sure pt is inside rect
  Result := (abs(x) <= a) and (abs(y) <= b);
  if not result then Exit;
  //given (x*x)/(a*a) + (y*y)/(b*b) = 1
  //then y*y = b*b(1 - (x*x)/(a*a))
  //nb: contents of Sqrt below will always be positive
  //since the substituted x must be within ellipseRec bounds
  y2 := Sqrt((b*b*(1 - (x*x)/(a*a))));
  Result := (y >= -y2) and (y <= y2);
end;
//------------------------------------------------------------------------------

function GetLineEllipseIntersects(const ellipseRec: TRect;
  var linePt1, linePt2: TPointD): Boolean;
var
  dx, dy, m,a,b,c,q: double;
  qa,qb,qc,qs: double;
  rec: TRectD;
  pt1, pt2: TPointD;
begin
  rec := RectD(ellipseRec);
  a := rec.Width *0.5;
  b := rec.Height *0.5;
  //offset ellipseRect so it's centered over the coordinate origin
  dx := ellipseRec.Left + a; dy := ellipseRec.Top + b;
  TranslateRect(rec, -dx, -dy);
  pt1 := TranslatePoint(linePt1, -dx, -dy);
  pt2 := TranslatePoint(linePt2, -dx, -dy);
  //equation of ellipse = (x*x)/(a*a) + (y*y)/(b*b) = 1
  //equation of line = y = mx + c;
  if (pt1.X = pt2.X) then //vertical line (ie infinite slope)
  begin
    //given x = K, then y*y = b*b(1 - (x*x)/(a*a))
    q := (b*b)*(1 - Sqr(pt1.X)/(a*a));
    result := q >= 0;
    if not result then Exit;
    q := Sqrt(q);
    pt1.Y := q;
    pt2.Y := -q;
  end else
  begin
    //using simultaneous equations and substitution
    //given y = mx + c
    m := (pt1.Y - pt2.Y)/(pt1.X - pt2.X);
    c := pt1.Y - m * pt1.X;
    //given (x*x)/(a*a) + (y*y)/(b*b) = 1
    //(x*x)/(a*a)*(b*b) + (y*y) = (b*b)
    //(b*b)/(a*a) *(x*x) + Sqr(m*x +c) = (b*b)
    //(b*b)/(a*a) *(x*x) + (m*m)*(x*x) + 2*m*x*c +c*c = b*b
    //((b*b)/(a*a) +(m*m)) *(x*x) + 2*m*c*(x) + (c*c) - (b*b) = 0
    //solving quadratic equation
    qa := ((b*b)/(a*a) +(m*m));
    qb := 2*m*c;
    qc := (c*c) - (b*b);
    qs := (qb*qb) - 4*qa*qc;
    Result := qs >= 0;
    if not result then Exit;
    qs := Sqrt(qs);
    pt1.X := (-qb +qs)/(2 * qa);
    pt1.Y := m * pt1.X + c;
    pt2.X := (-qb -qs)/(2 * qa);
    pt2.Y := m * pt2.X + c;
  end;
  //finally reverse initial offset
  linePt1 := TranslatePoint(pt1, dx, dy);
  linePt2 := TranslatePoint(pt2, dx, dy);
end;
//------------------------------------------------------------------------------

function Sign(const value: Double): integer; {$IFDEF INLINE} inline; {$ENDIF}
begin
  if value < 0 then Result := -1
  else if value > 0 then Result := 1
  else Result := 0;
end;
//------------------------------------------------------------------------------

function ApplyNormal(const pt, norm: TPointD; delta: double): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := PointD(pt.X + norm.X * delta, pt.Y + norm.Y * delta);
end;
//------------------------------------------------------------------------------

procedure AppendPoint(var path: TPathD; const extra: TPointD);
var
  len: integer;
begin
  len := length(path);
  SetLengthUninit(path, len +1);
  path[len] := extra;
end;
//------------------------------------------------------------------------------

procedure AppendPath(var paths: TPathsD; const extra: TPathD);
var
  len1, len2: integer;
begin
  len2 := length(extra);
  if len2 = 0 then Exit;
  len1 := length(paths);
  setLength(paths, len1 + 1);
  paths[len1] := Copy(extra, 0, len2);
end;
//------------------------------------------------------------------------------

procedure AppendPath(var paths: TPathsD; const extra: TPathsD);
var
  i, len1, len2: integer;
begin
  len2 := length(extra);
  if len2 = 0 then Exit;
  len1 := length(paths);
  setLength(paths, len1 + len2);
  for i := 0 to len2 -1 do
    paths[len1+i] := Copy(extra[i], 0, length(extra[i]));
end;
//------------------------------------------------------------------------------

procedure AppendPath(var ppp: TArrayOfPathsD; const extra: TPathsD);
var
  len: integer;
begin
  len := length(ppp);
  setLength(ppp, len + 1);
  if Assigned(extra) then
    AppendPath(ppp[len], extra) else
    ppp[len] := nil;
end;
//------------------------------------------------------------------------------

procedure ConcatPaths(var dstPath: TPathD; const path: TPathD); overload;
var
  len, pathLen: integer;
begin
  // calculate the length of the final array
  len := Length(dstPath);
  pathLen := Length(path);
  if pathLen = 0 then Exit;
  // Avoid point duplicates where paths join
  if (len > 0) and PointsEqual(dstPath[len -1], path[0]) then dec(len);
  // fill the array
  SetLengthUninit(dstPath, len + pathLen);
  Move(path[0], dstPath[len], pathLen * SizeOf(TPointD));
end;
//------------------------------------------------------------------------------

procedure ConcatPaths(var dstPath: TPathD; const paths: TPathsD);
var
  i, len, pathLen, offset: integer;
begin
  // calculate the length of the final array
  len := 0;
  for i := 0 to high(paths) do
  begin
    pathLen := Length(paths[i]);
    if pathLen > 0 then
    begin
      // Skip the start-point if it matches the previous path's end-point
      if (i > 0) and PointsEqual(paths[i][0], paths[i -1][high(paths[i -1])]) then
        dec(pathLen);
      inc(len, pathLen);
    end;
  end;
  SetLengthUninit(dstPath, len);

  // fill the array
  len := 0;
  for i := 0 to high(paths) do
  begin
    pathLen := Length(paths[i]);
    if pathLen > 0 then
    begin
      offset := 0;
      // Skip the start-point if it matches the previous path's end-point
      if (i > 0) and PointsEqual(paths[i][0], paths[i -1][high(paths[i -1])]) then
      begin
        dec(pathLen);
        offset := 1;
      end;
      // Skip if we have a path with only one point and that point also matches
      // the previous path's end-point.
      if pathLen > 0 then
      begin
        Move(paths[i][offset], dstPath[len], pathLen * SizeOf(TPointD));
        inc(len, pathLen);
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure RotatePoint(var pt: TPointD;
  const focalPoint: TPointD; sinA, cosA: double);
var
  tmpX, tmpY: double;
begin
  tmpX := pt.X-focalPoint.X;
  tmpY := pt.Y-focalPoint.Y;
  pt.X := tmpX * cosA - tmpY * sinA + focalPoint.X;
  pt.Y := tmpX * sinA + tmpY * cosA + focalPoint.Y;
end;
//------------------------------------------------------------------------------

procedure RotatePoint(var pt: TPointD;
  const focalPoint: TPointD; angleRad: double);
var
  sinA, cosA: double;
begin
  if angleRad = 0 then Exit;
{$IFDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
  angleRad := -angleRad;
{$ENDIF}
  GetSinCos(angleRad, sinA, cosA);
  RotatePoint(pt, focalPoint, sinA, cosA);
end;
//------------------------------------------------------------------------------

function RotatePathInternal(const path: TPathD;
  const focalPoint: TPointD; sinA, cosA: double): TPathD;
var
  i: integer;
  x,y: double;
begin
  NewPointDArray(Result, length(path), True);
  for i := 0 to high(path) do
  begin
    x := path[i].X - focalPoint.X;
    y := path[i].Y - focalPoint.Y;
    Result[i].X := x * cosA - y * sinA + focalPoint.X;
    Result[i].Y := x * sinA + y * cosA + focalPoint.Y;
  end;
end;
//------------------------------------------------------------------------------

function RotatePath(const path: TPathD;
  const focalPoint: TPointD; angleRads: double): TPathD;
var
  sinA, cosA: double;
begin
  if angleRads = 0 then
  begin
    Result := path;
    Exit;
  end;
{$IFDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
  angleRads := -angleRads;
{$ENDIF}
  GetSinCos(angleRads, sinA, cosA);
  Result := RotatePathInternal(path, focalPoint, sinA, cosA);
end;
//------------------------------------------------------------------------------

function RotatePath(const paths: TPathsD;
  const focalPoint: TPointD; angleRads: double): TPathsD;
var
  i: integer;
  sinA, cosA: double;
  fp: TPointD;
begin
  Result := paths;
  if not IsValid(angleRads) then Exit;
  NormalizeAngle(angleRads);
  if angleRads = 0 then Exit;
{$IFDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
  angleRads := -angleRads;
{$ENDIF}
  GetSinCos(angleRads, sinA, cosA);
  SetLength(Result, length(paths));
  if IsValid(focalPoint) then
    fp := focalPoint else
    fp := GetBoundsD(paths).MidPoint;
  for i := 0 to high(paths) do
    Result[i] := RotatePathInternal(paths[i], fp, sinA, cosA);
end;
//------------------------------------------------------------------------------

function GetAngle(const origin, pt: TPoint): double;
var
  x,y: double;
begin
  x := pt.X - origin.X;
  y := pt.Y - origin.Y;
  if x = 0 then
  begin
    if y > 0 then result := angle90
    else result := -angle90;
  end
  else if y = 0 then
  begin
    if x > 0 then result := 0
    else result := angle180;
  end else
    result := arctan2(y, x); //range between -Pi and Pi
{$IFDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
  Result := -Result;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function GetAngle(const origin, pt: TPointD): double;
var
  x,y: double;
begin
  x := pt.X - origin.X;
  y := pt.Y - origin.Y;
  if x = 0 then
  begin
    if y > 0 then result := angle90
    else result := -angle90;
  end
  else if y = 0 then
  begin
    if x > 0 then result := 0
    else result := angle180;
  end else
    result := arctan2(y, x); //range between -Pi and Pi
{$IFDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
  Result := -Result;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function GetAngle(const a, b, c: TPoint): double;
var
  ab, bc: TPointD;
  dp, cp: double;
begin
  //https://stackoverflow.com/a/3487062/359538
  ab := PointD(b.x - a.x, b.y - a.y);
  bc := PointD(b.x - c.x, b.y - c.y);
  dp := (ab.x * bc.x + ab.y * bc.y);
  cp := (ab.x * bc.y - ab.y * bc.x);
  Result := arctan2(cp, dp); //range between -Pi and Pi
{$IFDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
  Result := -Result;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function GetAngle(const a, b, c: TPointD): double;
var
  ab, bc: TPointD;
  dp, cp: double;
begin
  //https://stackoverflow.com/a/3487062/359538
  ab := PointD(b.x - a.x, b.y - a.y);
  bc := PointD(b.x - c.x, b.y - c.y);
  dp := (ab.x * bc.x + ab.y * bc.y);
  cp := (ab.x * bc.y - ab.y * bc.x);
  Result := arctan2(cp, dp); //range between -Pi and Pi
{$IFDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
  Result := -Result;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function GetPointAtAngleAndDist(const origin: TPointD;
  angle, distance: double): TPointD;
begin
  Result := origin;
  Result.X := Result.X + distance;
  RotatePoint(Result, origin, angle);
end;
//------------------------------------------------------------------------------

function IntersectPoint(const ln1a, ln1b, ln2a, ln2b: TPointD): TPointD;
var
  m1,b1,m2,b2: double;
begin
  result := InvalidPointD;
  //see http://paulbourke.net/geometry/pointlineplane/
  if (ln1B.X = ln1A.X) then
  begin
    if (ln2B.X = ln2A.X) then exit; //parallel lines
    m2 := (ln2B.Y - ln2A.Y)/(ln2B.X - ln2A.X);
    b2 := ln2A.Y - m2 * ln2A.X;
    Result.X := ln1A.X;
    Result.Y := m2*ln1A.X + b2;
  end
  else if (ln2B.X = ln2A.X) then
  begin
    m1 := (ln1B.Y - ln1A.Y)/(ln1B.X - ln1A.X);
    b1 := ln1A.Y - m1 * ln1A.X;
    Result.X := ln2A.X;
    Result.Y := m1*ln2A.X + b1;
  end else
  begin
    m1 := (ln1B.Y - ln1A.Y)/(ln1B.X - ln1A.X);
    b1 := ln1A.Y - m1 * ln1A.X;
    m2 := (ln2B.Y - ln2A.Y)/(ln2B.X - ln2A.X);
    b2 := ln2A.Y - m2 * ln2A.X;
    if m1 = m2 then exit; //parallel lines
    Result.X := (b2 - b1)/(m1 - m2);
    Result.Y := m1 * Result.X + b1;
  end;
end;
//------------------------------------------------------------------------------

function IntersectPoint(const ln1a, ln1b, ln2a, ln2b: TPointD;
  out ip: TPointD): Boolean;
begin
  ip := IntersectPoint(ln1a, ln1b, ln2a, ln2b);
  Result := IsValid(ip);
end;
//------------------------------------------------------------------------------

function SegmentIntersectPt(const ln1a, ln1b, ln2a, ln2b: TPointD): TPointD;
var
  pqd,r,s : TPointD; //scalar vectors;
  rs, t   : double;
begin
  //https://stackoverflow.com/a/565282/359538
  Result := InvalidPointD;
  r := PointD(ln1b.X - ln1a.X, ln1b.Y - ln1a.Y);
  s := PointD(ln2b.X - ln2a.X, ln2b.Y - ln2a.Y);
  rs := CrossProduct(r,s);
  if Abs(rs) < 1 then Exit;
  pqd.X := ln2a.X - ln1a.X;
  pqd.y := ln2a.Y - ln1a.Y;
  t := CrossProduct(pqd, s) / rs;
  if (t < -0.025) or (t > 1.025) then Exit;
  Result.X := ln1a.X + t * r.X;
  Result.Y := ln1a.Y + t * r.Y;
//  pqd.X := -pqd.X; pqd.Y := -pqd.Y;
//  u := CrossProduct(pqd, r) / rs;
//  if (u < -0.05) or (u > 1.05) then Exit;
end;
//------------------------------------------------------------------------------

function SegmentsIntersect(const ln1a, ln1b, ln2a, ln2b: TPointD;
  out ip: TPointD): Boolean;
begin
  ip := SegmentIntersectPt(ln1a, ln1b, ln2a, ln2b);
  Result := IsValid(ip);
end;
//------------------------------------------------------------------------------

function CalcRoundingSteps(radius: double): double;
begin
  //the results of this function have been derived empirically
  //and may need further adjustment
  if radius < 0.55 then result := 4
  else result := Pi * Sqrt(radius *2);
end;
//------------------------------------------------------------------------------

function Grow(const path, normals: TPathD; delta: double;
  joinStyle: TJoinStyle; miterLim: double; scale: double; isOpen: Boolean): TPathD;
var
  resCnt, resCap    : integer;
  norms             : TPathD;
  stepsPerRadian    : double;
  stepSin, stepCos  : double;
  asin, acos        : double;

  procedure AddPoint(const pt: TPointD);
  begin
    if resCnt >= resCap then
    begin
      inc(resCap, 64);
      SetLengthUninit(result, resCap);
    end;
    result[resCnt] := pt;
    inc(resCnt);
  end;

  procedure DoMiter(j, k: Integer; cosA: Double);
  var
    q: Double;
  begin
    q := delta / (cosA +1);
    AddPoint(PointD(
      path[j].X + (norms[k].X + norms[j].X) *q,
      path[j].Y + (norms[k].Y + norms[j].Y) *q));
  end;

  procedure DoBevel(j, k: Integer);
  var
    absDelta: double;
  begin
    if k = j then
    begin
      absDelta := Abs(delta);
      AddPoint(PointD(
        path[j].x - absDelta * norms[j].x,
        path[j].y - absDelta * norms[j].y));
      AddPoint(PointD(
        path[j].x + absDelta * norms[j].x,
        path[j].y + absDelta * norms[j].y));
    end else
    begin
      AddPoint(PointD(
        path[j].x + delta * norms[k].x,
        path[j].y + delta * norms[k].y));
      AddPoint(PointD(
        path[j].x + delta * norms[j].x,
        path[j].y + delta * norms[j].y));
    end;
  end;

  procedure DoSquare(j, k: Integer);
  var
    vec, ptQ, ptR, ptS, ptT, ptU, ip: TPointD;
    absDelta: double;
  begin
    if k = j then
    begin
      vec.X := norms[j].Y;     //squaring a line end
      vec.Y := -norms[j].X;
    end else
    begin
      // using the reciprocal of unit normals (as unit vectors)
      // get the average unit vector ...
      vec := GetAvgUnitVector(
        PointD(-norms[k].Y, norms[k].X),
        PointD(norms[j].Y, -norms[j].X));
    end;

    absDelta := Abs(delta);
    ptQ := PointD(path[j].X + absDelta * vec.X, path[j].Y + absDelta * vec.Y);

    ptR := PointD(ptQ.X + delta * vec.Y, ptQ.Y + delta * -vec.X);
    ptS := ReflectPoint(ptR, ptQ);

    // get 2 vertices along one edge offset
    ptT := PointD(
      path[k].X + norms[k].X * delta,
      path[k].Y + norms[k].Y * delta);

    if (j = k) then
    begin
      ptU.X := ptT.X + vec.X * delta;
      ptU.Y := ptT.Y + vec.Y * delta;
      ip := IntersectPoint(ptR, ptS, ptT, ptU);
      AddPoint(ReflectPoint(ip, ptQ));
      AddPoint(ip);
    end else
    begin
      ptU := PointD(
        path[j].X + norms[k].X * delta,
        path[j].Y + norms[k].Y * delta);
      ip := IntersectPoint(ptR, ptS, ptT, ptU);
      AddPoint(ip);
      AddPoint(ReflectPoint(ip, ptQ));
    end;
  end;

  procedure DoRound(j, k: Integer);
  var
    i, steps: Integer;
    pt: TPointD;
    dx, dy, oldDx: double;
    angle: double;
  begin
    // nb: angles may be negative but this will always be a convex join
    pt := path[j];
    if j = k then
    begin
      dx := -norms[k].X * delta;
      dy := -norms[k].Y * delta;
    end else
    begin
      dx := norms[k].X * delta;
      dy := norms[k].Y * delta;
    end;
    AddPoint(PointD(pt.X + dx, pt.Y + dy));

    angle := ArcTan2(asin, acos);
    steps := Ceil(stepsPerRadian * abs(angle));

    for i := 2 to steps do
    begin
      oldDx := dx;
      dx := oldDx * stepCos - stepSin * dy;
      dy := oldDx * stepSin + stepCos * dy;
      AddPoint(PointD(pt.X + dx, pt.Y + dy));
    end;
    AddPoint(PointD(
      pt.X + norms[j].X * delta,
      pt.Y + norms[j].Y * delta));
  end;

var
  j, k      : cardinal;
  len       : cardinal;
  steps     : double;
  highI     : cardinal;
  iLo,iHi   : cardinal;
  absDelta  : double;
begin
  Result := nil;
  if not Assigned(path) then exit;
  len := Length(path);
  if not isOpen then
    while (len > 2) and
      PointsNearEqual(path[len -1], path[0], 0.001) do
        dec(len);
  if len < 2 then Exit;

  if scale = 0 then scale := 1.0;

  absDelta := Abs(delta);
  if absDelta * scale < 1 then
    joinStyle := jsButt
  else if joinStyle = jsAuto then
  begin
    if delta < AutoWidthThreshold / 2 then
      joinStyle := jsSquare else
      joinStyle := jsRound;
  end;

  if absDelta < MinStrokeWidth/2 then
  begin
    if delta < 0 then
      delta := -MinStrokeWidth/2 else
      delta := MinStrokeWidth/2;
  end;


  if assigned(normals) then
    norms := normals else
    norms := GetNormals(path);

  highI := len -1;

  stepsPerRadian := 0;
  if joinStyle = jsRound then
  begin
    steps := CalcRoundingSteps(delta * scale);
    stepSin := sin(TwoPi/steps);
    stepCos := cos(TwoPi/steps);
		if (delta < 0) then stepSin := -stepSin;
    stepsPerRadian := steps / TwoPi;
  end;

  if miterLim <= 0 then miterLim := DefaultMiterLimit
  else if miterLim < 2 then miterLim := 2;
  miterLim := 2 /(sqr(miterLim));

  resCnt := 0;
  resCap := 0;

  if isOpen then
  begin
    iLo := 1; iHi := highI -1;
    k := 0;
    AddPoint(PointD(
     path[0].X + norms[0].X * delta,
     path[0].Y + norms[0].Y * delta));
  end else
  begin
    iLo := 0; iHi := highI;
    k := highI;
  end;

  for j := iLo to iHi do
  begin

    if PointsNearEqual(path[j], path[k], 0.01) then
    begin
       k := j; // todo - check if needed
       Continue;
    end;

    asin := CrossProduct(norms[k], norms[j]);
    if (asin > 1.0) then asin := 1.0
    else if (asin < -1.0) then asin := -1.0;
    acos := DotProduct(norms[k], norms[j]);

    if (acos > -0.999) and (asin * delta < 0) then
    begin
      // is concave
      AddPoint(PointD(
        path[j].X + norms[k].X * delta, path[j].Y + norms[k].Y * delta));
      AddPoint(path[j]);
      AddPoint(PointD(
        path[j].X + norms[j].X * delta, path[j].Y + norms[j].Y * delta));
    end
    else if (acos > 0.999) and (joinStyle <> jsRound) then
    begin
      // almost straight - less than 2.5 degree, so miter
      DoMiter(j, k, acos);
    end
    else if (joinStyle = jsMiter) then
    begin
      if (1 + acos > miterLim) then
        DoMiter(j, k, acos) else
        DoSquare(j, k);
    end
    else if (joinStyle = jsRound) then
      DoRound(j, k)
    else if (joinStyle = jsSquare) then
      DoSquare(j, k)
    else
      DoBevel(j, k);
    k := j;
  end;

  if isOpen then
    AddPoint(PointD(
     path[highI].X + norms[highI].X * delta,  //todo - check this !!!
     path[highI].Y + norms[highI].Y * delta));

  SetLength(Result, resCnt);
end;
//------------------------------------------------------------------------------

function GrowOpenLine(const line: TPathD; delta: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLim: double = 0; scale: double = 1.0): TPathD;
var
  len               : integer;
  resCnt, resCap    : integer;
  asin, acos        : double;
  stepSin, stepCos  : double;
  stepsPerRadian    : double;
  path, norms       : TPathD;

  procedure AddPoint(const pt: TPointD);
  begin
    if resCnt >= resCap then
    begin
      inc(resCap, 64);
      SetLengthUninit(result, resCap);
    end;
    result[resCnt] := pt;
    inc(resCnt);
  end;

  procedure DoMiter(j, k: Integer; cosA: Double);
  var
    q: Double;
  begin
    q := delta / (cosA +1);
    AddPoint(PointD(
      path[j].X + (norms[k].X + norms[j].X) *q,
      path[j].Y + (norms[k].Y + norms[j].Y) *q));
  end;

  procedure DoBevel(j, k: Integer);
  var
    absDelta: double;
  begin
    if k = j then
    begin
      absDelta := Abs(delta);
      AddPoint(PointD(
        path[j].x - absDelta * norms[j].x,
        path[j].y - absDelta * norms[j].y));
      AddPoint(PointD(
        path[j].x + absDelta * norms[j].x,
        path[j].y + absDelta * norms[j].y));
    end else
    begin
      AddPoint(PointD(
        path[j].x + delta * norms[k].x,
        path[j].y + delta * norms[k].y));
      AddPoint(PointD(
        path[j].x + delta * norms[j].x,
        path[j].y + delta * norms[j].y));
    end;
  end;

  procedure DoSquare(j, k: Integer);
  var
    vec, ptQ, ptR, ptS, ptT, ptU, ip: TPointD;
    absDelta: double;
  begin
    if k = j then
    begin
      vec.X := norms[j].Y;     //squaring a line end
      vec.Y := -norms[j].X;
    end else
    begin
      // using the reciprocal of unit normals (as unit vectors)
      // get the average unit vector ...
      vec := GetAvgUnitVector(
        PointD(-norms[k].Y, norms[k].X),
        PointD(norms[j].Y, -norms[j].X));
    end;

    absDelta := Abs(delta);
    ptQ := PointD(path[j].X + absDelta * vec.X, path[j].Y + absDelta * vec.Y);

    ptR := PointD(ptQ.X + delta * vec.Y, ptQ.Y + delta * -vec.X);
    ptS := ReflectPoint(ptR, ptQ);

    // get 2 vertices along one edge offset
    ptT := PointD(
      path[k].X + norms[k].X * delta,
      path[k].Y + norms[k].Y * delta);

    if (j = k) then
    begin
      ptU.X := ptT.X + vec.X * delta;
      ptU.Y := ptT.Y + vec.Y * delta;
      ip := IntersectPoint(ptR, ptS, ptT, ptU);
      AddPoint(ReflectPoint(ip, ptQ));
      AddPoint(ip);
    end else
    begin
      ptU := PointD(
        path[j].X + norms[k].X * delta,
        path[j].Y + norms[k].Y * delta);
      ip := IntersectPoint(ptR, ptS, ptT, ptU);
      AddPoint(ip);
      AddPoint(ReflectPoint(ip, ptQ));
    end;
  end;

  procedure DoRound(j, k: Integer);
  var
    i, steps: Integer;
    pt: TPointD;
    dx, dy, oldDx: double;
    angle: double;
  begin
    // nb: angles may be negative but this will always be a convex join
    pt := path[j];
    if j = k then
    begin
      dx := -norms[k].X * delta;
      dy := -norms[k].Y * delta;
      angle := PI;
    end else
    begin
      dx := norms[k].X * delta;
      dy := norms[k].Y * delta;
      angle := ArcTan2(asin, acos);
    end;
    AddPoint(PointD(pt.X + dx, pt.Y + dy));

    steps := Ceil(stepsPerRadian * abs(angle));
    for i := 2 to steps do
    begin
      oldDx := dx;
      dx := oldDx * stepCos - stepSin * dy;
      dy := oldDx * stepSin + stepCos * dy;
      AddPoint(PointD(pt.X + dx, pt.Y + dy));
    end;
    AddPoint(PointD(
      pt.X + norms[j].X * delta,
      pt.Y + norms[j].Y * delta));
  end;

  procedure DoPoint(j: Cardinal; var k: Cardinal);
  begin
    asin := CrossProduct(norms[k], norms[j]);
    if (asin > 1.0) then asin := 1.0
    else if (asin < -1.0) then asin := -1.0;
    acos := DotProduct(norms[k], norms[j]);

    if (acos > -0.999) and (asin * delta < 0) then
    begin
      // is concave
      AddPoint(PointD(
        path[j].X + norms[k].X * delta, path[j].Y + norms[k].Y * delta));
      AddPoint(path[j]);
      AddPoint(PointD(
        path[j].X + norms[j].X * delta, path[j].Y + norms[j].Y * delta));
    end
    else if (acos > 0.999) and (joinStyle <> jsRound) then
      // almost straight - less than 2.5 degree, so miter
      DoMiter(j, k, acos)
    else if (joinStyle = jsMiter) then
    begin
      if (1 + acos > miterLim) then
        DoMiter(j, k, acos) else
        DoSquare(j, k);
    end
    else if (joinStyle = jsRound) then
      DoRound(j, k)
    else if (joinStyle = jsSquare) then
      DoSquare(j, k)
    else
      DoBevel(j, k);
    k := j;
  end;

var
  highJ : cardinal;
  j, k  : cardinal;
  steps : double;
begin
  Result := nil;
  path := StripNearDuplicates(line, 0.1, false);
  len := length(path);
  if (len = 0) or (delta <= 0) then Exit;
  // don't specify a minimum delta as this path may be scaled later
//  if delta < MinStrokeWidth then
//    delta := MinStrokeWidth;
  delta := delta * 0.5;

  if len = 1 then
  begin
    with path[0] do
      result := Ellipse(RectD(x-delta, y-delta, x+delta, y+delta));
    Exit;
  end;

  //Assert(endStyle <> esClosed);

  //with very narrow lines, don't get fancy with joins and line ends
  if (delta <= 1) then
  begin
    if (joinStyle = jsRound) and (delta * scale <= 1) then
      joinStyle := jsButt;
    if (endStyle = esRound) and (delta * scale <= 1) then
      endStyle := esSquare;
  end
  else if joinStyle = jsAuto then
  begin
    if (endStyle = esRound) and (delta * scale >= AutoWidthThreshold) then
      joinStyle := jsRound else
      joinStyle := jsSquare;
  end;

  stepsPerRadian := 0;
  if (joinStyle = jsRound) or (endStyle = esRound) then
  begin
    steps := CalcRoundingSteps(delta * scale);
    stepSin := sin(TwoPi/steps);
    stepCos := cos(TwoPi/steps);
		if (delta < 0) then stepSin := -stepSin;
    stepsPerRadian := steps / TwoPi;
  end;

  if miterLim <= 0 then miterLim := DefaultMiterLimit
  else if miterLim < 2 then miterLim := 2;
  miterLim := 2 /(sqr(miterLim));

  norms := GetNormals(path);
  resCnt := 0; resCap := 0;

  case endStyle of
    esButt: DoBevel(0,0);
    esRound: DoRound(0,0);
    else DoSquare(0, 0);
  end;

  // offset the left side going **forward**
  k := 0;
  highJ := len -1;
  for j := 1 to highJ -1 do DoPoint(j,k);

  // reverse the normals ...
  for j := highJ downto 1 do
  begin
    norms[j].X := -norms[j-1].X;
    norms[j].Y := -norms[j-1].Y;
  end;
  norms[0] := norms[len -1];

  case endStyle of
    esButt: DoBevel(highJ,highJ);
    esRound: DoRound(highJ,highJ);
    else DoSquare(highJ,highJ);
  end;

  // offset the left side going **backward**
  k := highJ;
  for j := highJ -1 downto 1 do
    DoPoint(j, k);

  SetLength(Result, resCnt);
end;
//------------------------------------------------------------------------------

function GrowClosedLine(const line: TPathD; width: double;
  joinStyle: TJoinStyle; miterLim: double = 0; scale: double = 1.0): TPathsD;
var
  norms: TPathD;
  rec: TRectD;
  skipHole: Boolean;
begin
  rec := GetBoundsD(line);
  skipHole := (rec.Width <= width) or (rec.Height <= width);
  if skipHole then
  begin
    SetLength(Result, 1);
    norms := GetNormals(line);
    Result[0] := Grow(line, norms, width/2, joinStyle, miterLim, scale, false);
  end else
  begin
    SetLength(Result, 2);
    norms := GetNormals(line);
    Result[0] := Grow(line, norms, width/2, joinStyle, miterLim, scale, false);
    Result[1] := ReversePath(
      Grow(line, norms, -width/2, joinStyle, miterLim, scale, false));
  end;
end;
//------------------------------------------------------------------------------

function RoughOutline(const line: TPathD; lineWidth: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLim: double = 0; scale: double = 1.0): TPathsD;
var
  lines: TPathsD;
begin
  SetLength(lines,1);
  lines[0] := line;
  Result := RoughOutline(lines, lineWidth, joinStyle, endStyle, miterLim, scale);
end;
//------------------------------------------------------------------------------

function RoughOutline(const lines: TPathsD; lineWidth: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLim: double = 0; scale: double = 1.0): TPathsD;
var
  i: integer;
  lwDiv2: double;
  p: TPathD;
begin
  result := nil;
  if not assigned(lines) then exit;
  if joinStyle = jsAuto then
  begin
    if endStyle in [esPolygon, esRound] then
      joinStyle := jsRound else
      joinStyle := jsSquare;
  end;
  if scale = 0 then scale := 1;

  if endStyle = esPolygon then
  begin
    for i := 0 to high(lines) do
    begin
      if Length(lines[i]) = 1 then
      begin
        lwDiv2 := lineWidth/2;
        with lines[i][0] do
          AppendPath(Result,
            Ellipse(RectD(x-lwDiv2, y-lwDiv2, x+lwDiv2, y+lwDiv2)));
      end else
      begin
        p := StripNearDuplicates(lines[i], 0.1, true);
        if Length(p) = 2 then AppendPoint(p, p[0]);
        AppendPath(Result,
          GrowClosedLine(p, lineWidth, joinStyle, miterLim, scale));
      end;
    end;
  end
  else
  begin
    SetLength(Result, Length(lines));
    for i := 0 to high(lines) do
      Result[i] := GrowOpenLine(lines[i], lineWidth,
        joinStyle, endStyle, miterLim, scale);
  end;
end;
//------------------------------------------------------------------------------

function Rectangle(const rec: TRect): TPathD;
begin
  NewPointDArray(Result, 4, True);
  with rec do
  begin
    result[0] := PointD(left, top);
    result[1] := PointD(right, top);
    result[2] := PointD(right, bottom);
    result[3] := PointD(left, bottom);
  end;
end;
//------------------------------------------------------------------------------

function Rectangle(const rec: TRectD): TPathD;
begin
  NewPointDArray(Result, 4, True);
  with rec do
  begin
    result[0] := PointD(left, top);
    result[1] := PointD(right, top);
    result[2] := PointD(right, bottom);
    result[3] := PointD(left, bottom);
  end;
end;
//------------------------------------------------------------------------------

function Rectangle(l, t, r, b: double): TPathD;
begin
  NewPointDArray(Result, 4, True);
  result[0] := PointD(l, t);
  result[1] := PointD(r, t);
  result[2] := PointD(r, b);
  result[3] := PointD(l, b);
end;
//------------------------------------------------------------------------------

procedure InflateRect(var rec: TRect; dx, dy: integer);
begin
  rec.Left := rec.Left - dx;
  rec.Top := rec.Top - dy;
  rec.Right := rec.Right + dx;
  rec.Bottom := rec.Bottom + dy;
end;
//------------------------------------------------------------------------------

procedure InflateRect(var rec: TRectD; dx, dy: double);
begin
  rec.Left := rec.Left - dx;
  rec.Top := rec.Top - dy;
  rec.Right := rec.Right + dx;
  rec.Bottom := rec.Bottom + dy;
end;
//------------------------------------------------------------------------------

function NormalizeRect(var rect: TRect): Boolean;
var
  i: integer;
begin
  Result := False;
  with rect do
  begin
    if Left > Right then
    begin
      i := Left;
      Left := Right;
      Right := i;
      Result := True;
    end;
    if Top > Bottom then
    begin
      i := Top;
      Top := Bottom;
      Bottom := i;
      Result := True;
    end;
  end;
end;
//------------------------------------------------------------------------------

function RoundRect(const rec: TRect; radius: integer): TPathD;
begin
  Result := RoundRect(RectD(rec), PointD(radius, radius));
end;
//------------------------------------------------------------------------------

function RoundRect(const rec: TRect; radius: TPoint): TPathD;
begin
  Result := RoundRect(RectD(rec), PointD(radius));
end;
//------------------------------------------------------------------------------

function RoundRect(const rec: TRectD; radius: double): TPathD;
begin
  Result := RoundRect(rec, PointD(radius, radius));
end;
//------------------------------------------------------------------------------

function RoundRect(const rec: TRectD; radius: TPointD): TPathD;
var
  i,j     : integer;
  corners : TPathD;
  bezPts  : TPathD;
  magic   : TPointD;
const
  magicC: double = 0.55228475; // =4/3 * (sqrt(2)-1)
begin
  Result := nil;
  if rec.IsEmpty then Exit;
  radius.X := Min(radius.X, rec.Width/2);
  radius.Y := Min(radius.Y, rec.Height/2);
  if (radius.X < 1) and (radius.Y < 1) then
  begin
    Result := Rectangle(rec);
    Exit;
  end;
  magic.X := radius.X * magicC;
  magic.Y := radius.Y * magicC;
  NewPointDArray(Corners, 4, True);
  with rec do
  begin
    corners[0] := PointD(Right, Top);
    corners[1] := BottomRight;
    corners[2] := PointD(Left, Bottom);
    corners[3] := TopLeft;
  end;
  NewPointDArray(Result, 1, True);
  Result[0].X := corners[3].X + radius.X;
  Result[0].Y := corners[3].Y;
  NewPointDArray(bezPts, 4, True);
  for i := 0 to High(corners) do
  begin
    for j := 0 to 3 do bezPts[j] := corners[i];
    case i of
      3:
        begin
          bezPts[0].Y := bezPts[0].Y + radius.Y;
          bezPts[1].Y := bezPts[0].Y - magic.Y;
          bezPts[3].X := bezPts[3].X + radius.X;
          bezPts[2].X := bezPts[3].X - magic.X;
        end;
      0:
        begin
          bezPts[0].X := bezPts[0].X - radius.X;
          bezPts[1].X := bezPts[0].X + magic.X;
          bezPts[3].Y := bezPts[3].Y + radius.Y;
          bezPts[2].Y := bezPts[3].Y - magic.Y;
        end;
      1:
        begin
          bezPts[0].Y := bezPts[0].Y - radius.Y;
          bezPts[1].Y := bezPts[0].Y + magic.Y;
          bezPts[3].X := bezPts[3].X - radius.X;
          bezPts[2].X := bezPts[3].X + magic.X;
        end;
      2:
        begin
          bezPts[0].X := bezPts[0].X + radius.X;
          bezPts[1].X := bezPts[0].X - magic.X;
          bezPts[3].Y := bezPts[3].Y - radius.Y;
          bezPts[2].Y := bezPts[3].Y + magic.Y;
        end;
    end;
    ConcatPaths(Result, FlattenCBezier(bezPts));
  end;
end;
//------------------------------------------------------------------------------

function Circle(const pt: TPoint; radius: double): TPathD;
var
  rec: TRectD;
begin
  rec.Left := pt.X - radius;
  rec.Right := pt.X + radius;
  rec.Top := pt.Y - radius;
  rec.Bottom := pt.Y + radius;
  Result := Ellipse(rec);
end;
//------------------------------------------------------------------------------

function Circle(const pt: TPointD; radius: double): TPathD;
var
  rec: TRectD;
begin
  rec.Left := pt.X - radius;
  rec.Right := pt.X + radius;
  rec.Top := pt.Y - radius;
  rec.Bottom := pt.Y + radius;
  Result := Ellipse(rec);
end;
//------------------------------------------------------------------------------

function Circle(const pt: TPointD; radius: double; pendingScale: double): TPathD;
var
  rec: TRectD;
begin
  rec.Left := pt.X - radius;
  rec.Right := pt.X + radius;
  rec.Top := pt.Y - radius;
  rec.Bottom := pt.Y + radius;
  Result := Ellipse(rec, pendingScale);
end;
//------------------------------------------------------------------------------

function CalcCircleFrom3Points(const p1,p2,p3: TPointD;
  out centre: TPointD; out radius: double): Boolean;
var
  mat11, mat12, mat13, mat14: TMatrixD;
  m11,m12,m13,m14: double;
begin
  mat11 := Matrix(p1.X, p1.Y, 1, p2.X, p2.Y, 1, p3.X, p3.Y, 1);
  m11 := MatrixDeterminant(mat11);
  Result := m11 <> 0;
  if not Result then Exit;
  mat12 := Matrix(Sqr(p1.X)+Sqr(p1.Y), p1.Y, 1,
    Sqr(p2.X)+Sqr(p2.Y), p2.Y, 1, Sqr(p3.X)+Sqr(p3.Y), p3.Y, 1);
  mat12 := Matrix(2, 1, 1, 20, 4, 1, 34, 3, 1);
  m12 := MatrixDeterminant(mat12);
  mat13 := Matrix(Sqr(p1.X)+Sqr(p1.Y), p1.X, 1,
    Sqr(p2.X)+Sqr(p2.Y), p2.X, 1, Sqr(p3.X)+Sqr(p3.Y), p3.X, 1);
  m13 := MatrixDeterminant(mat13);
  mat14 := Matrix(Sqr(p1.X)+Sqr(p1.Y), p1.X, p1.Y,
    Sqr(p2.X)+Sqr(p2.Y), p2.X, p2.Y, Sqr(p3.X)+Sqr(p3.Y), p3.X, p3.Y);
  m14 := MatrixDeterminant(mat14);
  centre.X := 0.5 * m12/m11;
  centre.Y := -0.5 * m13/m11;
  radius := Sqrt(Sqr(centre.X) + Sqr(centre.Y) + m14/m11);
end;
//------------------------------------------------------------------------------

function Ellipse(const rec: TRectD; pendingScale: double): TPathD;
var
  steps: integer;
begin
  if pendingScale <= 0 then pendingScale := 1;
  steps := Round(CalcRoundingSteps((rec.width + rec.Height) * pendingScale));
  Result := Ellipse(rec, steps);
end;
//------------------------------------------------------------------------------


function Ellipse(const rec: TRect; steps: integer): TPathD;
begin
  Result := Ellipse(RectD(rec), steps);
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
  if steps < 4 then
    steps := Round(CalcRoundingSteps(rec.width + rec.height));
  GetSinCos(2 * Pi / Steps, sinA, cosA);
  delta.x := cosA; delta.y := sinA;
  NewPointDArray(Result, Steps, True);
  Result[0] := PointD(centre.X + radius.X, centre.Y);
  for i := 1 to steps -1 do
  begin
    Result[i] := PointD(centre.X + radius.X * delta.x,
      centre.Y + radius.y * delta.y);
    delta :=  PointD(delta.X * cosA - delta.Y * sinA,
      delta.Y * cosA + delta.X * sinA);
  end; //rotates clockwise
end;
//------------------------------------------------------------------------------

function RotatedEllipse(const rec: TRectD; angle: double; steps: integer = 0): TPathD;
begin
  Result := Ellipse(rec, steps);
  if angle = 0 then Exit;
  Result := RotatePath(Result, rec.MidPoint, angle);
end;
//------------------------------------------------------------------------------

function RotatedEllipse(const rec: TRectD; angle: double; pendingScale: double): TPathD;
begin
  Result := Ellipse(rec, pendingScale);
  if angle = 0 then Exit;
  Result := RotatePath(Result, rec.MidPoint, angle);
end;
//------------------------------------------------------------------------------

function AngleToEllipticalAngle(const ellRec: TRectD; angle: double): double;
begin
  Result := arctan2(ellRec.Height/ellRec.Width * sin(angle), cos(angle));
end;
//------------------------------------------------------------------------------

function EllipticalAngleToAngle(const ellRec: TRectD; angle: double): double;
begin
  Result := ArcTan2(sin(angle) *ellRec.Width, cos(angle) * ellRec.Height);
end;
//------------------------------------------------------------------------------

function Star(const rec: TRectD; points: integer; indentFrac: double): TPathD;
var
  i: integer;
  innerOff: double;
  p, p2: TPathD;
  rec2: TRectD;
begin
  Result := nil;
  if points < 5 then points := 5
  else if points > 15 then points := 15;
  if indentFrac < 0.2 then indentFrac := 0.2
  else if indentFrac > 0.8 then indentFrac := 0.8;
  innerOff := Min(rec.Width, rec.Height) * indentFrac * 0.5;
  if not Odd(points) then inc(points);
  p := Ellipse(rec, points);
  if not Assigned(p) then Exit;
  rec2 := rec;
  Img32.Vector.InflateRect(rec2, -innerOff, -innerOff);
  if rec2.IsEmpty then
    p2 := Ellipse(rec, points*2) else
    p2 := Ellipse(rec2, points*2);
  NewPointDArray(Result, points*2, True);
  for i := 0 to points -1 do
  begin
    Result[i*2] := p[i];
    Result[i*2+1] := p2[i*2+1];
  end;
end;
//------------------------------------------------------------------------------

function Star(const focalPt: TPointD;
  innerRadius, outerRadius: double; points: integer): TPathD;
var
  i: Integer;
  sinA, cosA: double;
  delta: TPointD;
begin
  result := nil;
  if (innerRadius <= 0) or (outerRadius <= 0) then Exit;
  if points <= 5 then points := 10
  else points := points * 2;
  GetSinCos(2 * Pi / points, sinA, cosA);
  delta.x := cosA; delta.y := sinA;
  NewPointDArray(Result, points, True);
  Result[0] := PointD(focalPt.X + innerRadius, focalPt.Y);
  for i := 1 to points -1 do
  begin
    if Odd(i) then
      Result[i] := PointD(focalPt.X + outerRadius * delta.x,
        focalPt.Y + outerRadius * delta.y)
    else
      Result[i] := PointD(focalPt.X + innerRadius * delta.x,
        focalPt.Y + innerRadius * delta.y);
    delta :=  PointD(delta.X * cosA - delta.Y * sinA,
      delta.Y * cosA + delta.X * sinA);
  end;
end;
//------------------------------------------------------------------------------

function Arc(const rec: TRectD;
  startAngle, endAngle: double; scale: double): TPathD;
var
  i, steps: Integer;
  angle: double;
  sinA, cosA: double;
  centre, radius: TPointD;
  deltaX, deltaX2, deltaY: double;
const
  qtrDeg = PI/1440;
begin
  Result := nil;
  if (endAngle = startAngle) or IsEmptyRect(rec) then Exit;
  if scale <= 0 then scale := 4.0;

{$IFDEF CLOCKWISE_ROTATION_WITH_NEGATIVE_ANGLES}
  startAngle := -startAngle;
  endAngle := -endAngle;
{$ENDIF}

  NormalizeAngle(startAngle, qtrDeg);
  NormalizeAngle(endAngle, qtrDeg);
  with rec do
  begin
    centre := MidPoint;
    radius := PointD(Width * 0.5, Height  * 0.5);
  end;
  if endAngle < startAngle then
    angle := endAngle - startAngle + angle360 else
    angle := endAngle - startAngle;
  //steps = (No. steps for a whole ellipse) * angle/(2*Pi)
  steps := Round(CalcRoundingSteps((rec.width + rec.height)/2 * scale));
  steps := steps div 2; /////////////////////////////////
  if steps < 2 then steps := 2;
  NewPointDArray(Result, Steps +1, True);
  //angle of the first step ...
  GetSinCos(startAngle, deltaY, deltaX);
  Result[0].X := centre.X + radius.X * deltaX;
  Result[0].Y := centre.Y + radius.y * deltaY;
  //angle of each subsequent step ...
  GetSinCos(angle / Steps, sinA, cosA);
  for i := 1 to steps do
  begin
    deltaX2 := deltaX * cosA - deltaY * sinA;
    deltaY := deltaY * cosA + deltaX * sinA;
    deltaX := deltaX2;
    Result[i].X := centre.X + radius.X * deltaX;
    Result[i].Y := centre.Y + radius.y * deltaY;
  end; //progresses clockwise from start to end
end;
//------------------------------------------------------------------------------

function Pie(const rec: TRectD;
  StartAngle, EndAngle: double; scale: double): TPathD;
var
  len: integer;
begin
  result := Arc(rec, StartAngle, EndAngle, scale);
  len := length(result);
  SetLengthUninit(result, len +1);
  result[len] := PointD((rec.Left + rec.Right)/2, (rec.Top + rec.Bottom)/2);
end;
//------------------------------------------------------------------------------

function ArrowHead(const arrowTip, ctrlPt: TPointD; size: double;
  arrowStyle: TArrowStyle): TPathD;
var
  unitVec, basePt: TPointD;
  sDiv40, sDiv50, sDiv60, sDiv120: double;
begin
  result := nil;
  sDiv40 := size * 0.40;
  sDiv50 := size * 0.50;
  sDiv60 := size * 0.60;
  sDiv120 := sDiv60 * 2;
  unitVec := GetUnitVector(ctrlPt, arrowTip);
  case arrowStyle of
    asNone:
      Exit;
    asSimple:
      begin
        NewPointDArray(result, 3, True);
        basePt := TranslatePoint(arrowTip, -unitVec.X * size, -unitVec.Y * size);
        result[0] := arrowTip;
        result[1] := TranslatePoint(basePt, -unitVec.Y * sDiv50, unitVec.X * sDiv50);
        result[2] := TranslatePoint(basePt, unitVec.Y * sDiv50, -unitVec.X * sDiv50);
      end;
    asFancy:
      begin
        NewPointDArray(result, 4, True);
        basePt := TranslatePoint(arrowTip,
          -unitVec.X * sDiv120, -unitVec.Y * sDiv120);
        result[0] := TranslatePoint(basePt, -unitVec.Y *sDiv50, unitVec.X *sDiv50);
        result[1] := TranslatePoint(arrowTip, -unitVec.X *size, -unitVec.Y *size);
        result[2] := TranslatePoint(basePt, unitVec.Y *sDiv50, -unitVec.X *sDiv50);
        result[3] := arrowTip;
      end;
    asDiamond:
      begin
        NewPointDArray(result, 4, True);
        basePt := TranslatePoint(arrowTip, -unitVec.X * sDiv60, -unitVec.Y * sDiv60);
        result[0] := arrowTip;
        result[1] := TranslatePoint(basePt, -unitVec.Y * sDiv50, unitVec.X * sDiv50);
        result[2] := TranslatePoint(arrowTip, -unitVec.X * sDiv120, -unitVec.Y * sDiv120);
        result[3] := TranslatePoint(basePt, unitVec.Y * sDiv50, -unitVec.X * sDiv50);
      end;
    asCircle:
      begin
        basePt := TranslatePoint(arrowTip, -unitVec.X * sDiv50, -unitVec.Y * sDiv50);
        with Point(basePt) do
          result := Ellipse(RectD(x - sDiv50, y - sDiv50, x + sDiv50, y + sDiv50));
      end;
    asTail:
      begin
        NewPointDArray(result, 6, True);
        basePt := TranslatePoint(arrowTip, -unitVec.X * sDiv60, -unitVec.Y * sDiv60);
        result[0] := TranslatePoint(arrowTip, -unitVec.X * sDiv50, -unitVec.Y * sDiv50);
        result[1] := TranslatePoint(arrowTip, -unitVec.Y * sDiv40, unitVec.X * sDiv40);
        result[2] := TranslatePoint(basePt, -unitVec.Y * sDiv40, unitVec.X * sDiv40);
        result[3] := TranslatePoint(arrowTip, -unitVec.X * sDiv120, -unitVec.Y * sDiv120);
        result[4] := TranslatePoint(basePt, unitVec.Y * sDiv40, -unitVec.X * sDiv40);
        result[5] := TranslatePoint(arrowTip, unitVec.Y * sDiv40, -unitVec.X * sDiv40);
      end;
  end;
end;
//------------------------------------------------------------------------------

function GetDefaultArrowHeadSize(lineWidth: double): double;
begin
  Result := lineWidth *3 + 7;
end;
//------------------------------------------------------------------------------

procedure AdjustPoint(var pt: TPointD; const referencePt: TPointD; delta: double);
var
  vec: TPointD;
begin
  //Positive delta moves pt away from referencePt, and
  //negative delta moves pt toward referencePt.
  vec := GetUnitVector(referencePt, pt);
  pt.X := pt.X + (vec.X * delta);
  pt.Y := pt.Y + (vec.Y * delta);
end;
//------------------------------------------------------------------------------

function ShortenPath(const path: TPathD;
  pathEnd: TPathEnd; amount: double): TPathD;
var
  len, amount2: double;
  vec: TPointD;
  i, highPath: integer;
begin
  result := path;
  highPath := high(path);
  if highPath < 1 then Exit;
  amount2 := amount;
  if pathEnd <> peEnd then
  begin
    //shorten start
    i := 0;
    while (i < highPath) do
    begin
      len := Distance(result[i], result[i+1]);
      if (len >= amount) then Break;
      amount := amount - len;
      inc(i);
    end;
    if i > 0 then
    begin
      Move(path[i], Result[0], (highPath - i +1) * SizeOf(TPointD));
      dec(highPath, i);
      SetLength(Result, highPath +1);
    end;
    if amount > 0 then
    begin
      vec := GetUnitVector(result[0], result[1]);
      result[0].X := result[0].X + vec.X * amount;
      result[0].Y := result[0].Y + vec.Y * amount;
    end;
  end;
  if pathEnd <> peStart then
  begin
    //shorten end
    while (highPath > 1) do
    begin
      len := Distance(result[highPath], result[highPath -1]);
      if (len >= amount2) then Break;
      amount2 := amount2 - len;
      dec(highPath);
    end;
    SetLength(Result, highPath +1);
    if amount2 > 0 then
    begin
      vec := GetUnitVector(result[highPath], result[highPath -1]);
      result[highPath].X := result[highPath].X + vec.X * amount2;
      result[highPath].Y := result[highPath].Y + vec.Y * amount2;
    end;
  end;
end;
//------------------------------------------------------------------------------

function GetDashedPath(const path: TPathD;
  closed: Boolean; const pattern: TArrayOfDouble;
  patternOffset: PDouble): TPathsD;
var
  i, highI, paIdx: integer;
  vecs, path2, dash: TPathD;
  patCnt: integer;
  patLen: double;
  dashCapacity, dashCnt, ptsCapacity, ptsCnt: integer;
  segLen, residualPat, patOff: double;
  filling: Boolean;
  pt, pt2: TPointD;

  procedure NewDash;
  begin
    if ptsCnt = 1 then ptsCnt := 0;
    if ptsCnt = 0 then Exit;
    if dashCnt = dashCapacity then
    begin
      inc(dashCapacity, BuffSize);
      setLength(result, dashCapacity);
    end;
    result[dashCnt] := Copy(dash, 0, ptsCnt);
    inc(dashCnt);
    ptsCapacity := BuffSize;
    setLength(dash, ptsCapacity);
    ptsCnt := 0;
  end;

  procedure ExtendDash(const pt: TPointD);
  begin
    if ptsCnt = ptsCapacity then
    begin
      inc(ptsCapacity, BuffSize);
      setLength(dash, ptsCapacity);
    end;
    dash[ptsCnt] := pt;
    inc(ptsCnt);
  end;

begin
  Result := nil;
  paIdx := 0;
  patCnt := length(pattern);
  path2 := path;
  highI := high(path2);
  if (highI < 1) or (patCnt = 0) then Exit;
  if closed and
    ((path2[highI].X <> path2[0].X) or (path2[highI].Y <> path2[0].Y)) then
  begin
    inc(highI);
    setLength(path2, highI +2);
    path2[highI] := path2[0];
  end;
  vecs := GetVectors(path2);
  if (vecs[0].X = 0) and (vecs[0].Y = 0) then Exit; //not a line
  if not assigned(patternOffset) then
    patOff := 0 else
    patOff := patternOffset^;
  patLen := 0;
  for i := 0 to patCnt -1 do
    patLen := patLen + pattern[i];
  if patOff < 0 then
  begin
    patOff := patLen + patOff;
    while patOff < 0 do
      patOff := patOff + patLen;
  end
  else while patOff > patLen do
    patOff := patOff - patLen;
  //nb: each dash is made up of 2 or more pts
  dashCnt := 0;
  dashCapacity := 0;
  ptsCnt := 0;
  ptsCapacity := 0;
  filling := true;
  while patOff >= pattern[paIdx] do
  begin
    filling := not filling;
    patOff := patOff - pattern[paIdx];
    paIdx := (paIdx + 1) mod patCnt;
  end;
  residualPat := pattern[paIdx] - patOff;
  pt := path2[0];
  ExtendDash(pt);
  i := 0;
  while (i < highI) do
  begin
    segLen := Distance(pt, path2[i+1]);
    if residualPat > segLen then
    begin
      if filling then ExtendDash(path2[i+1]);
      residualPat := residualPat - segLen;
      pt := path2[i+1];
      inc(i);
    end else
    begin
      pt2.X := pt.X + vecs[i].X * residualPat;
      pt2.Y := pt.Y + vecs[i].Y * residualPat;
      if filling then ExtendDash(pt2);
      filling := not filling;
      NewDash;
      paIdx := (paIdx + 1) mod patCnt;
      residualPat := pattern[paIdx];
      pt := pt2;
      ExtendDash(pt);
    end;
  end;
  NewDash;
  SetLength(Result, dashCnt);
  if not assigned(patternOffset) then Exit;
  patOff := 0;
  for i := 0 to paIdx -1 do
    patOff := patOff + pattern[i];
  patternOffset^ := patOff + (pattern[paIdx] - residualPat);
end;
//------------------------------------------------------------------------------

function GetDashedOutLine(const path: TPathD;
  closed: Boolean; const pattern: TArrayOfDouble;
  patternOffset: PDouble; lineWidth: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle): TPathsD;
var
  i: integer;
  tmp: TPathsD;
begin
  Result := nil;
  for i := 0 to High(pattern) do
    if pattern[i] <= 0 then pattern[i] := 1;
  tmp := GetDashedPath(path, closed, pattern, patternOffset);
  for i := 0 to high(tmp) do
//    AppendPath(Result, GrowOpenLine(tmp[i],
//      lineWidth, joinStyle, endStyle, 2));
    AppendPath(Result, GrowClosedLine(tmp[i], lineWidth, joinStyle, 2));
end;
//------------------------------------------------------------------------------

function GetBoundsD(const paths: TArrayOfPathsD): TRectD;
var
  i, len: integer;
  rec: TRectD;
begin
  len := Length(paths);
  i := 0;
  while (i < len) do
  begin
    rec := GetBoundsD(paths[i]);
    if not IsEmptyRect(rec) then Break;
    inc(i);
  end;

  if i = len then
  begin
    Result := NullRectD;
    Exit;
  end;
  Result := rec;

  for i := i + 1 to len -1 do
  begin
    rec := GetBoundsD(paths[i]);
    if IsEmptyRect(rec) then Continue;
    Result := UnionRect(Result, rec);
  end;
end;
//------------------------------------------------------------------------------

function GetBoundsD(const paths: TPathsD): TRectD;
var
  i,j: integer;
  p: PPointD;
  {$IFDEF CPUX64}
  l,t,r,b,x,y: double;
  {$ENDIF CPUX64}
begin
  if paths = nil then
  begin
    Result := NullRectD;
    Exit;
  end;

  {$IFDEF CPUX64}
  l := MaxDouble; t := l;
  r := -MaxDouble; b := r;
  {$ELSE}
  Result.Left := MaxDouble;
  Result.Top := MaxDouble;
  Result.Right := -MaxDouble;
  Result.Bottom := -MaxDouble;
  {$ENDIF CPUX64}
  for i := 0 to high(paths) do
  begin
    p := PPointD(paths[i]);
    if not assigned(p) then Continue;
    for j := 0 to high(paths[i]) do
    begin
      {$IFDEF CPUX64}
      // load p.X and p.Y into xmm registers
      x := p.X;
      y := p.Y;
      if x < l then l := x;
      if x > r then r := x;
      if y < t then t := y;
      if y > b then b := y;
      {$ELSE}
      // If we must use the FPU and memory then we should write directly
      // to the target memory.
      if p.x < Result.Left   then Result.Left := p.x;
      if p.x > Result.Right  then Result.Right := p.x;
      if p.y < Result.Top    then Result.Top := p.y;
      if p.y > Result.Bottom then Result.Bottom := p.y;
      {$ENDIF CPUX64}
      inc(p);
    end;
  end;
  {$IFDEF CPUX64}
  if r < l then
    Result := NullRectD
  else
  begin
    // Inline the RectD() call by hand
    Result.Left := l;
    Result.Top := t;
    Result.Right := r;
    Result.Bottom := b;
  end;
  {$ELSE}
  if Result.Right < Result.Left then
    Result := NullRectD;
  {$ENDIF CPUX64}
end;
//------------------------------------------------------------------------------

function GetBoundsD(const path: TPathD): TRectD;
var
  i,highI: integer;
  p: PPointD;
  {$IFDEF CPUX64}
  l,t,r,b,x,y: double;
  {$ENDIF CPUX64}
begin
  highI := High(path);
  if highI < 0 then
  begin
    Result := NullRectD;
    Exit;
  end;

  {$IFDEF CPUX64}
  l := path[0].X; r := l;
  t := path[0].Y; b := t;
  p := PPointD(path);
  for i := 1 to highI do
  begin
    inc(p);
    // load p.X and p.Y into xmm registers
    x := p.X;
    y := p.Y;
    if x < l then l := x;
    if x > r then r := x;
    if y < t then t := y;
    if y > b then b := y;
  end;
  // Inline the RectD() call by hand
  Result.Left := l;
  Result.Top := t;
  Result.Right := r;
  Result.Bottom := b;
  {$ELSE}
  // If we must use the FPU and memory then we should write directly
  // to the target memory.
    {$IFDEF RECORD_METHODS}
  Result.TopLeft := path[0]; // uses "rep movsd"
  Result.BottomRight := Result.TopLeft;
    {$ELSE}
  Result.Left := path[0].X; // uses "fld" and "fstp"
  Result.Top := path[0].Y;
  Result.Right := Result.Left;
  Result.Bottom := Result.Right;
    {$ENDIF RECORD_METHODS}
  p := PPointD(path);
  for i := 1 to highI do
  begin
    inc(p);
    if p.x < Result.Left   then Result.Left := p.x;
    if p.x > Result.Right  then Result.Right := p.x;
    if p.y < Result.Top    then Result.Top := p.y;
    if p.y > Result.Bottom then Result.Bottom := p.y;
  end;
  {$ENDIF CPUX64}
end;
//------------------------------------------------------------------------------

function GetBounds(const path: TPathD): TRect;
var
  recD: TRectD;
begin
  recD := GetBoundsD(path);
  Result := Rect(recD);
end;
//------------------------------------------------------------------------------

function GetBounds(const paths: TPathsD): TRect;
var
  recD: TRectD;
begin
  recD := GetBoundsD(paths);
  Result := Rect(recD);
end;
//------------------------------------------------------------------------------

procedure PrePendPoint(const pt: TPointD; const p: TPathD; var Result: TPathD);
var
  len: integer;
begin
  len := Length(p);
  SetLengthUninit(Result, len +1);
  Result[0] := pt;
  if len > 0 then Move(p[0], Result[1], len * SizeOf(TPointD));
end;
//------------------------------------------------------------------------------

function PrePendPoint(const pt: TPointD; const p: TPathD): TPathD;
begin
  PrePendPoint(pt, p, Result);
end;
//------------------------------------------------------------------------------

function PrePendPoints(const pt1, pt2: TPointD; const p: TPathD): TPathD;
var
  len: integer;
begin
  len := Length(p);
  NewPointDArray(Result, len +2, True);
  Result[0] := pt1;
  Result[1] := pt2;
  if len > 0 then Move(p[0], Result[2], len * SizeOf(TPointD));
end;
//------------------------------------------------------------------------------

function GetPointInQuadBezier(const a,b,c: TPointD; t: double): TPointD;
var
  omt: double;
begin
  if t > 1 then t := 1
  else if t < 0 then t := 0;
  omt := 1 - t;
  Result.X := a.X*omt*omt + b.X*2*omt*t + c.X*t*t;
  Result.Y := a.Y*omt*omt + b.Y*2*omt*t + c.Y*t*t;
end;
//------------------------------------------------------------------------------

function FlattenQBezier(const firstPt: TPointD; const pts: TPathD;
  tolerance: double = 0.0): TPathD; overload;
begin
  if tolerance <= 0.0 then tolerance := BezierTolerance;
  Result := FlattenQBezier(PrePendPoint(firstPt, pts), tolerance);
end;
//------------------------------------------------------------------------------

function FlattenQBezier(const pts: TPathD; tolerance: double = 0.0): TPathD;
var
  i, highI: integer;
  p: TPathD;
begin
  Result := nil;
  highI := high(pts);
  if highI < 0 then Exit;
  if (highI < 2) or Odd(highI) then
    raise Exception.CreateRes(@rsInvalidQBezier);
  if tolerance <= 0.0 then tolerance := BezierTolerance;
  NewPointDArray(Result, 1, True);
  Result[0] := pts[0];
  for i := 0 to (highI div 2) -1 do
  begin
    if PointsEqual(pts[i*2], pts[i*2+1]) and
      PointsEqual(pts[i*2+1], pts[i*2+2]) then
    begin
      AppendPoint(Result, pts[i*2]);
      AppendPoint(Result, pts[i*2 +2]);
    end else
    begin
      p := FlattenQBezier(pts[i*2], pts[i*2+1], pts[i*2+2], tolerance);
      ConcatPaths(Result, Copy(p, 1, Length(p) -1));
    end;
  end;
end;
//------------------------------------------------------------------------------

function FlattenQBezier(const pt1, pt2, pt3: TPointD;
  tolerance: double = 0.0): TPathD;
var
  resultCnt, resultLen: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if resultCnt = resultLen then
    begin
      inc(resultLen, BuffSize);
      SetLengthUninit(result, resultLen);
    end;
    result[resultCnt] := pt;
    inc(resultCnt);
  end;

  procedure DoCurve(const p1, p2, p3: TPointD);
  var
    p12, p23, p123: TPointD;
  begin
    if (abs(p1.x + p3.x - 2 * p2.x) +
      abs(p1.y + p3.y - 2 * p2.y) < tolerance) then
    begin
      AddPoint(p3);
    end else
    begin
      P12.X := (P1.X + P2.X) * 0.5;
      P12.Y := (P1.Y + P2.Y) * 0.5;
      P23.X := (P2.X + P3.X) * 0.5;
      P23.Y := (P2.Y + P3.Y) * 0.5;
      P123.X := (P12.X + P23.X) * 0.5;
      P123.Y := (P12.Y + P23.Y) * 0.5;
      DoCurve(p1, p12, p123);
      DoCurve(p123, p23, p3);
    end;
  end;

begin
  resultLen := 0; resultCnt := 0;
  if tolerance <= 0.0 then tolerance := BezierTolerance;
  AddPoint(pt1);
  if ((pt1.X = pt2.X) and (pt1.Y = pt2.Y)) or
    ((pt2.X = pt3.X) and (pt2.Y = pt3.Y)) then
  begin
    AddPoint(pt3)
  end else
    DoCurve(pt1, pt2, pt3);
  SetLength(result, resultCnt);
end;
//------------------------------------------------------------------------------

function GetPointInCubicBezier(const a,b,c,d: TPointD; t: double): TPointD;
var
  omt: double;
begin
  if t > 1 then t := 1
  else if t < 0 then t := 0;
  omt := 1 - t;
  Result.X := a.X*omt*omt*omt +b.X*3*omt*omt*t +c.X*3*omt*t*t +d.X*t*t*t;
  Result.Y := a.Y*omt*omt*omt +b.Y*3*omt*omt*t +c.Y*3*omt*t*t +d.Y*t*t*t;
end;
//------------------------------------------------------------------------------

function FlattenCBezier(const firstPt: TPointD; const pts: TPathD;
  tolerance: double = 0.0): TPathD; overload;
begin
  Result := FlattenCBezier(PrePendPoint(firstPt, pts), tolerance);
end;
//------------------------------------------------------------------------------

function FlattenCBezier(const path: TPathD; tolerance: double = 0.0): TPathD;
var
  i, len: integer;
  p: TPathD;
begin
  Result := nil;
  len := Length(path) -1;
  if len < 0 then Exit;
  if (len < 3) or (len mod 3 <> 0) then
    raise Exception.Create(rsInvalidCBezier);
  if tolerance <= 0.0 then tolerance := BezierTolerance;
  NewPointDArray(Result, 1, True);
  Result[0] := path[0];
  for i := 0 to (len div 3) -1 do
  begin
    if PointsEqual(path[i*3], path[i*3+1]) and
      PointsEqual(path[i*3+2], path[i*3+3]) then
    begin
      AppendPoint(Result, path[i*3]);
      AppendPoint(Result, path[i*3 +3]);
    end else
    begin
      p := FlattenCBezier(path[i*3], path[i*3+1],
        path[i*3+2], path[i*3+3], tolerance);
      ConcatPaths(Result, Copy(p, 1, Length(p) -1));
    end;
  end;
end;
//------------------------------------------------------------------------------

function FlattenCBezier(const paths: TPathsD; tolerance: double): TPathsD;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := FlattenCBezier(paths[i], tolerance);
end;
//------------------------------------------------------------------------------

function FlattenCBezier(const pt1, pt2, pt3, pt4: TPointD;
  tolerance: double = 0.0): TPathD;
var
  resultCnt, resultLen: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if resultCnt = resultLen then
    begin
      inc(resultLen, BuffSize);
      SetLengthUninit(result, resultLen);
    end;
    result[resultCnt] := pt;
    inc(resultCnt);
  end;

  procedure DoCurve(const p1, p2, p3, p4: TPointD);
  var
    p12, p23, p34, p123, p234, p1234: TPointD;
  begin
    if  ((abs(p1.x +p3.x - 2*p2.x)  < tolerance) and
        (abs(p2.x +p4.x - 2*p3.x) < tolerance)) and
        ((abs(p1.y +p3.y - 2*p2.y)  < tolerance) and
        (abs(p2.y +p4.y - 2*p3.y) < tolerance)) then
    begin
      AddPoint(p4);
    end else
    begin
      p12.X := (p1.X + p2.X) / 2;
      p12.Y := (p1.Y + p2.Y) / 2;
      p23.X := (p2.X + p3.X) / 2;
      p23.Y := (p2.Y + p3.Y) / 2;
      p34.X := (p3.X + p4.X) / 2;
      p34.Y := (p3.Y + p4.Y) / 2;
      p123.X := (p12.X + p23.X) / 2;
      p123.Y := (p12.Y + p23.Y) / 2;
      p234.X := (p23.X + p34.X) / 2;
      p234.Y := (p23.Y + p34.Y) / 2;
      p1234.X := (p123.X + p234.X) / 2;
      p1234.Y := (p123.Y + p234.Y) / 2;
      DoCurve(p1, p12, p123, p1234);
      DoCurve(p1234, p234, p34, p4);
    end;
  end;

begin
  result := nil;
  resultLen := 0; resultCnt := 0;
  if tolerance <= 0.0 then tolerance := BezierTolerance;
  AddPoint(pt1);
  if ValueAlmostZero(pt1.X - pt2.X) and ValueAlmostZero(pt1.Y - pt2.Y) and
    ValueAlmostZero(pt3.X - pt4.X) and ValueAlmostZero(pt3.Y - pt4.Y) then
  begin
    AddPoint(pt4)
  end else
    DoCurve(pt1, pt2, pt3, pt4);
  SetLength(result,resultCnt);
end;
//------------------------------------------------------------------------------

function ReflectPoint(const pt, pivot: TPointD): TPointD;
begin
  Result.X := pivot.X + (pivot.X - pt.X);
  Result.Y := pivot.Y + (pivot.Y - pt.Y);
end;
//------------------------------------------------------------------------------

function FlattenCSpline(const priorCtrlPt, startPt: TPointD;
  const pts: TPathD; tolerance: double = 0.0): TPathD;
var
  p: TPathD;
  len: integer;
begin
  len := Length(pts);
  NewPointDArray(p, len + 2, True);
  p[0] := startPt;
  p[1] := ReflectPoint(priorCtrlPt, startPt);
  if len > 0 then
    Move(pts[0], p[2], len * SizeOf(TPointD));
  Result := FlattenCSpline(p, tolerance);
end;
//------------------------------------------------------------------------------

function FlattenCSpline(const pts: TPathD; tolerance: double = 0.0): TPathD;
var
  resultCnt, resultLen: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if resultCnt = resultLen then
    begin
      inc(resultLen, BuffSize);
      SetLengthUninit(result, resultLen);
    end;
    result[resultCnt] := pt;
    inc(resultCnt);
  end;

  procedure DoCurve(const p1, p2, p3, p4: TPointD);
  var
    p12, p23, p34, p123, p234, p1234: TPointD;
  begin
    if (abs(p1.x + p3.x - 2*p2.x) + abs(p2.x + p4.x - 2*p3.x) +
      abs(p1.y + p3.y - 2*p2.y) + abs(p2.y + p4.y - 2*p3.y)) < tolerance then
    begin
      AddPoint(p4);
    end else
    begin
      p12.X := (p1.X + p2.X) / 2;
      p12.Y := (p1.Y + p2.Y) / 2;
      p23.X := (p2.X + p3.X) / 2;
      p23.Y := (p2.Y + p3.Y) / 2;
      p34.X := (p3.X + p4.X) / 2;
      p34.Y := (p3.Y + p4.Y) / 2;
      p123.X := (p12.X + p23.X) / 2;
      p123.Y := (p12.Y + p23.Y) / 2;
      p234.X := (p23.X + p34.X) / 2;
      p234.Y := (p23.Y + p34.Y) / 2;
      p1234.X := (p123.X + p234.X) / 2;
      p1234.Y := (p123.Y + p234.Y) / 2;
      DoCurve(p1, p12, p123, p1234);
      DoCurve(p1234, p234, p34, p4);
    end;
  end;

var
  i, len: integer;
  p: PPointD;
  pt1,pt2,pt3,pt4: TPointD;
begin
  result := nil;
  len := Length(pts); resultLen := 0; resultCnt := 0;
  if (len < 4) then Exit;
  if tolerance <= 0.0 then tolerance := BezierTolerance;
  //ignore incomplete trailing control points
  if Odd(len) then dec(len);
  p := @pts[0];
  AddPoint(p^);
  pt1 := p^; inc(p);
  pt2 := p^; inc(p);
  for i := 0 to (len shr 1) - 2 do
  begin
    pt3 := p^; inc(p);
    pt4 := p^; inc(p);
    DoCurve(pt1, pt2, pt3, pt4);
    pt1 := pt4;
    pt2 := ReflectPoint(pt3, pt1);
  end;
  SetLength(result,resultCnt);
end;
//------------------------------------------------------------------------------

function FlattenQSpline(const priorCtrlPt, startPt: TPointD;
  const pts: TPathD; tolerance: double = 0.0): TPathD;
var
  p: TPathD;
  len: integer;
begin
  len := Length(pts);
  NewPointDArray(p, len + 2, True);
  p[0] := startPt;
  p[1] := ReflectPoint(priorCtrlPt, startPt);
  if len > 0 then
    Move(pts[0], p[2], len * SizeOf(TPointD));
  Result := FlattenQSpline(p, tolerance);
end;
//------------------------------------------------------------------------------

function FlattenQSpline(const pts: TPathD; tolerance: double = 0.0): TPathD;
var
  resultCnt, resultLen: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if resultCnt = resultLen then
    begin
      inc(resultLen, BuffSize);
      SetLengthUninit(result, resultLen);
    end;
    result[resultCnt] := pt;
    inc(resultCnt);
  end;

  procedure DoCurve(const p1, p2, p3: TPointD);
  var
    p12, p23, p123: TPointD;
  begin
    if (abs(p1.x + p3.x - 2 * p2.x) +
      abs(p1.y + p3.y - 2 * p2.y) < tolerance) then
    begin
      AddPoint(p3);
    end else
    begin
      P12.X := (P1.X + P2.X) * 0.5;
      P12.Y := (P1.Y + P2.Y) * 0.5;
      P23.X := (P2.X + P3.X) * 0.5;
      P23.Y := (P2.Y + P3.Y) * 0.5;
      P123.X := (P12.X + P23.X) * 0.5;
      P123.Y := (P12.Y + P23.Y) * 0.5;
      DoCurve(p1, p12, p123);
      DoCurve(p123, p23, p3);
    end;
  end;

var
  i, len: integer;
  p: PPointD;
  pt1, pt2, pt3: TPointD;
begin
  result := nil;
  len := Length(pts);
  if (len < 3) then Exit;
  resultLen := 0;
  resultCnt := 0;
  if tolerance <= 0.0 then tolerance := BezierTolerance;
  p := @pts[0];
  AddPoint(p^);
  pt1 := p^; inc(p);
  pt2 := p^; inc(p);
  for i := 0 to len - 3 do
  begin
    pt3 := p^; inc(p);
    DoCurve(pt1, pt2, pt3);
    pt1 := pt3;
    pt2 := ReflectPoint(pt2, pt1);
  end;
  SetLength(result,resultCnt);
end;
//------------------------------------------------------------------------------

function MakePath(const pts: array of double): TPathD;
var
  i, len: Integer;
  x,y: double;
begin
  Result := nil;
  len := length(pts) div 2;
  if len = 0 then Exit;
  NewPointDArray(Result, len, True);
  Result[0].X := pts[0];
  Result[0].Y := pts[1];
  for i := 1 to len -1 do
  begin
    x := pts[i*2];
    y := pts[i*2 +1];
    Result[i].X := x;
    Result[i].Y := y;
  end;
end;
//------------------------------------------------------------------------------

function MakePath(const pt: TPointD): TPathD;
begin
  SetLengthUninit(Result, 1);
  Result[0] := pt;
end;
//------------------------------------------------------------------------------

end.
