unit Img32.Clipper2;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.7                                                             *
* Date      :  6 January 2025                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  Wrapper module for the Clipper library                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Img32, Img32.Draw, Img32.Vector, Clipper.Offset;

//nb: InflatePath assumes that there's consistent winding where
//outer paths wind in one direction and inner paths in the other
type
  TClipperEndType = Clipper.Offset.TEndType;

function ClipperEndType(endStyle: TEndStyle): TClipperEndType;

function InflatePath(const path: TPathD; delta: Double;
  joinStyle: TJoinStyle = jsAuto; endType: TClipperEndType = etPolygon;
  miterLimit: double = 2.0; arcTolerance: double = 0.0;
  minEdgeLength: double = 0.25): TPathsD;

function InflatePaths(const paths: TPathsD; delta: Double;
  joinStyle: TJoinStyle = jsAuto; endType: TClipperEndType = etPolygon;
  miterLimit: double = 2.0; arcTolerance: double = 0.0;
  minEdgeLength: double = 0): TPathsD;

//UnionPolygon: removes self-intersections
function UnionPolygon(const polygon: TPathD;
  fillRule: TFillRule): TPathsD;

function UnionPolygons(const polygons: TPathsD;
  fillRule: TFillRule): TPathsD; overload;
function UnionPolygons(const polygon1, polygon2: TPathD;
  fillRule: TFillRule): TPathsD; overload;
function UnionPolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD; overload;

function IntersectPolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD;

function DifferencePolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD;

const
  etPolygon   = Clipper.Offset.etPolygon;
  etJoined    = Clipper.Offset.etJoined;
  etButt      = Clipper.Offset.etButt;
  etSquare    = Clipper.Offset.etSquare;
  etRound     = Clipper.Offset.etRound;

implementation

uses Clipper, Clipper.Core, Clipper.Engine;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function ClipperEndType(endStyle: TEndStyle): TClipperEndType;
begin
  case endStyle of
    esPolygon: Result := etJoined;
    esButt: Result := etButt;
    esSquare: Result := etSquare;
    else Result := etRound;
  end;
end;
//------------------------------------------------------------------------------

function InflatePath(const path: Img32.TPathD;
  delta: Double; joinStyle: TJoinStyle; endType: TClipperEndType;
  miterLimit: double; arcTolerance: double; minEdgeLength: double): Img32.TPathsD;
var
  paths: Img32.TPathsD;
begin
  setLength(paths, 1);
  paths[0] := path;
  Result := InflatePaths(paths, delta, joinStyle, endType,
    miterLimit, arcTolerance, minEdgeLength);
end;
//------------------------------------------------------------------------------

function InflatePaths(const paths: Img32.TPathsD;
  delta: Double; joinStyle: TJoinStyle; endType: TClipperEndType;
  miterLimit: double; arcTolerance: double; minEdgeLength: double): Img32.TPathsD;
var
  jt: Clipper.Offset.TJoinType;
begin
  case joinStyle of
    jsSquare  : jt := jtSquare;
    jsButt    : jt := jtBevel;
    jsMiter   : jt := jtMiter;
    jsRound   : jt := jtRound;
    else if endType = etRound then jt := jtRound
    else jt := jtSquare;
  end;
  Result := Img32.TPathsD(Clipper.InflatePaths(
    Clipper.Core.TPathsD(paths), delta, jt, endType));
end;
//------------------------------------------------------------------------------

function UnionPolygon(const polygon: Img32.TPathD;
  fillRule: Img32.Vector.TFillRule): Img32.TPathsD;
begin
  with TClipperD.Create do
  try
    AddSubject(Clipper.Core.TPathD(polygon));
    Execute(ctUnion,
      Clipper.Core.TFillRule(fillRule), Clipper.Core.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function UnionPolygons(const polygons: Img32.TPathsD;
  fillRule: Img32.Vector.TFillRule): Img32.TPathsD;
begin
  with TClipperD.Create do
  try
    AddSubject(Clipper.Core.TPathsD(polygons));
    Execute(ctUnion,
      Clipper.Core.TFillRule(fillRule), Clipper.Core.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function UnionPolygons(const polygon1, polygon2: Img32.TPathD;
  fillRule: Img32.Vector.TFillRule): Img32.TPathsD;
begin
  with TClipperD.Create do
  try
    AddSubject(Clipper.Core.TPathD(polygon1));
    AddClip(Clipper.Core.TPathD(polygon2));
    Execute(ctUnion,
      Clipper.Core.TFillRule(fillRule), Clipper.Core.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function UnionPolygons(const polygons1, polygons2: Img32.TPathsD;
  fillRule: Img32.Vector.TFillRule): Img32.TPathsD;
begin
  with TClipperD.Create do
  try
    AddSubject(Clipper.Core.TPathsD(polygons1));
    AddClip(Clipper.Core.TPathsD(polygons2));
    Execute(ctUnion,
      Clipper.Core.TFillRule(fillRule), Clipper.Core.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function IntersectPolygons(const polygons1, polygons2: Img32.TPathsD;
  fillRule: Img32.Vector.TFillRule): Img32.TPathsD;
begin
  with TClipperD.Create do
  try
    AddSubject(Clipper.Core.TPathsD(polygons1));
    AddClip(Clipper.Core.TPathsD(polygons2));
    Execute(ctIntersection,
      Clipper.Core.TFillRule(fillRule), Clipper.Core.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function DifferencePolygons(const polygons1, polygons2: Img32.TPathsD;
  fillRule: Img32.Vector.TFillRule): Img32.TPathsD;
begin
  with TClipperD.Create do
  try
    AddSubject(Clipper.Core.TPathsD(polygons1));
    AddClip(Clipper.Core.TPathsD(polygons2));
    Execute(ctDifference,
      Clipper.Core.TFillRule(fillRule), Clipper.Core.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

end.
