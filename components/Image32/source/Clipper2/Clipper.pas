unit Clipper;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  7 May 2024                                                      *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  This module provides a simple interface to the Clipper Library  *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Clipper.inc}

uses
  Math, SysUtils, Classes,
  Clipper.Core, Clipper.Engine, Clipper.Offset, Clipper.RectClip;

// A number of structures defined in other units are redeclared here
// so those units won't also need to be declared in your own units clauses.
type
  TClipper    = Clipper.Engine.TClipper64;
  TClipper64  = Clipper.Engine.TClipper64;
  TPoint64    = Clipper.Core.TPoint64;
  TRect64     = Clipper.Core.TRect64;
  TPath64     = Clipper.Core.TPath64;
  TPaths64    = Clipper.Core.TPaths64;
  TPointD     = Clipper.Core.TPointD;
  TRectD      = Clipper.Core.TRectD;
  TPathD      = Clipper.Core.TPathD;
  TPathsD     = Clipper.Core.TPathsD;
  TFillRule   = Clipper.Core.TFillRule;
  TPolyTree64 = Clipper.Engine.TPolyTree64;
  TPolyTreeD  = Clipper.Engine.TPolyTreeD;
  TJoinType   = Clipper.Offset.TJoinType;
  TEndType    = Clipper.Offset.TEndType;

  TArrayOfInt64 = array of Int64;
const
  frEvenOdd   = Clipper.Core.frEvenOdd;
  frNonZero   = Clipper.Core.frNonZero;
  frPositive  = Clipper.Core.frPositive;
  frNegative  = Clipper.Core.frNegative;
  jtBevel     = Clipper.Offset.jtBevel;
  jtSquare    = Clipper.Offset.jtSquare;
  jtRound     = Clipper.Offset.jtRound;
  jtMiter     = Clipper.Offset.jtMiter;
  etPolygon   = Clipper.Offset.etPolygon;
  etJoined    = Clipper.Offset.etJoined;
  etButt      = Clipper.Offset.etButt;
  etSquare    = Clipper.Offset.etSquare;
  etRound     = Clipper.Offset.etRound;

  ctNone          = Clipper.Core.ctNoClip;
  ctIntersection  = Clipper.Core.ctIntersection;
  ctUnion         = Clipper.Core.ctUnion;
  ctDifference    = Clipper.Core.ctDifference;
  ctXor           = Clipper.Core.ctXor;

function BooleanOp(clipType: TClipType;
  const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64; overload;
function BooleanOp(clipType: TClipType; const subjects, clips:
  TPathsD; fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
procedure BooleanOp(clipType: TClipType; const subjects, clips: TPaths64;
  fillRule: TFillRule; polytree: TPolyTree64); overload;

function Intersect(const subjects, clips: TPaths64;
  fillRule: TFillRule): TPaths64; overload;
function Union(const subjects, clips: TPaths64;
  fillRule: TFillRule): TPaths64; overload;
function Union(const subjects: TPaths64;
  fillRule: TFillRule): TPaths64; overload;
function Difference(const subjects, clips: TPaths64;
  fillRule: TFillRule): TPaths64; overload;
function XOR_(const subjects, clips: TPaths64;
  fillRule: TFillRule): TPaths64; overload;

function Intersect(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function Union(const subjects: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function Union(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function Difference(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;
function XOR_(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD; overload;

function InflatePaths(const paths: TPaths64; delta: Double;
  jt: TJoinType = jtRound; et: TEndType = etPolygon;
  MiterLimit: double = 2.0; ArcTolerance: double = 0.0): TPaths64; overload;
function InflatePaths(const paths: TPathsD; delta: Double;
  jt: TJoinType = jtRound; et: TEndType = etPolygon;
  miterLimit: double = 2.0; precision: integer = 2;
  ArcTolerance: double = 0.0): TPathsD; overload;

// RectClip: for closed paths only (otherwise use RectClipLines)
function RectClip(const rect: TRect64; const path: TPath64): TPath64; overload;
function RectClip(const rect: TRect64; const paths: TPaths64): TPaths64; overload;
function RectClip(const rect: TRectD; const path: TPathD; precision: integer = 2): TPathD; overload;
function RectClip(const rect: TRectD; const paths: TPathsD; precision: integer = 2): TPathsD; overload;

function RectClipLines(const rect: TRect64;
  const path: TPath64): TPaths64; overload;
function RectClipLines(const rect: TRect64;
  const paths: TPaths64): TPaths64; overload;
function RectClipLines(const rect: TRectD; const path: TPathD;
  precision: integer = 2): TPathsD; overload;
function RectClipLines(const rect: TRectD; const paths: TPathsD;
  precision: integer = 2): TPathsD; overload;

function TranslatePath(const path: TPath64; dx, dy: Int64): TPath64; overload;
function TranslatePath(const path: TPathD; dx, dy: double): TPathD; overload;
function TranslatePaths(const paths: TPaths64; dx, dy: Int64): TPaths64; overload;
function TranslatePaths(const paths: TPathsD; dx, dy: double): TPathsD; overload;

function MinkowskiSum(const pattern, path: TPath64;
  pathIsClosed: Boolean): TPaths64; overload;
function MinkowskiSum(const pattern, path: TPathD;
  pathIsClosed: Boolean): TPathsD; overload;

function PolyTreeToPaths64(PolyTree: TPolyTree64): TPaths64;
function PolyTreeToPathsD(PolyTree: TPolyTreeD): TPathsD;

function PathToString(const p: TPath64;
  indentSpaces: integer = 0; pointsPerRow: integer = 0): string; overload;
function PathToString(const p: TPathD; decimals: integer;
  indentSpaces: integer = 0; pointsPerRow: integer = 0): string; overload;
function PathsToString(const p: TPaths64;
  indentSpaces: integer = 0; pointsPerRow: integer = 0): string; overload;
function PathsToString(const p: TPathsD; decimals: integer;
  indentSpaces: integer = 0; pointsPerRow: integer = 0): string; overload;

//ShowPolyTreeStructure: only useful when debugging
procedure ShowPolyTreeStructure(polytree: TPolyTree64; strings: TStrings); overload;
procedure ShowPolyTreeStructure(polytree: TPolyTreeD; strings: TStrings); overload;

function MakePath(const ints: array of Int64): TPath64; overload;
function MakePathD(const dbls: array of double): TPathD; overload;

function TrimCollinear(const p: TPath64;
  isOpenPath: Boolean = false): TPath64; overload;
function TrimCollinear(const path: TPathD;
  precision: integer; isOpenPath: Boolean = false): TPathD; overload;

function PointInPolygon(const pt: TPoint64; const polygon: TPath64):
  TPointInPolygonResult;

function SimplifyPath(const path: TPath64;
  shapeTolerance: double; isClosedPath: Boolean = true): TPath64; overload;
function SimplifyPaths(const paths: TPaths64;
  shapeTolerance: double; isClosedPath: Boolean = true): TPaths64; overload;
function SimplifyPath(const path: TPathD; shapeTolerance: double;
  isClosedPath: Boolean = true; decimalPrecision: integer = 2): TPathD; overload;
function SimplifyPaths(const paths: TPathsD; shapeTolerance: double;
  isClosedPath: Boolean = true; decimalPrecision: integer = 2): TPathsD; overload;

implementation

uses
  Clipper.Minkowski;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{$IFDEF USINGZ}
function MakePath(const ints: array of Int64): TPath64;
var
  i, len: integer;
begin
  len := length(ints) div 3;
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := ints[i*3];
    Result[i].Y := ints[i*3 +1];
    Result[i].z := ints[i*3 +2];
  end;
end;
//------------------------------------------------------------------------------

function MakePathD(const dbls: array of double): TPathD; overload;
var
  i, len: integer;
begin
  len := length(dbls) div 3;
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := dbls[i*3];
    Result[i].Y := dbls[i*3 +1];
    Result[i].Z := Round(dbls[i*3 +2]);
  end;
end;
//------------------------------------------------------------------------------
{$ELSE}

function MakePath(const ints: array of Int64): TPath64;
var
  i, len: integer;
begin
  len := length(ints) div 2;
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := ints[i*2];
    Result[i].Y := ints[i*2 +1];
  end;
end;
//------------------------------------------------------------------------------

function MakePathD(const dbls: array of double): TPathD; overload;
var
  i, len: integer;
begin
  len := length(dbls) div 2;
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i].X := dbls[i*2];
    Result[i].Y := dbls[i*2 +1];
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF}

procedure AddPolyNodeToPaths(Poly: TPolyPath64; var Paths: TPaths64);
var
  i: Integer;
begin
  if (Length(Poly.Polygon) > 0) then
  begin
    i := Length(Paths);
    SetLength(Paths, i +1);
    Paths[i] := Poly.Polygon;
  end;
  for i := 0 to Poly.Count - 1 do
    AddPolyNodeToPaths(Poly[i], Paths);
end;
//------------------------------------------------------------------------------

function PolyTreeToPaths64(PolyTree: TPolyTree64): TPaths64;
begin
  Result := nil;
  AddPolyNodeToPaths(PolyTree, Result);
end;
//------------------------------------------------------------------------------

procedure AddPolyNodeToPathsD(Poly: TPolyPathD; var Paths: TPathsD);
var
  i: Integer;
begin
  if (Length(Poly.Polygon) > 0) then
  begin
    i := Length(Paths);
    SetLength(Paths, i +1);
    Paths[i] := Poly.Polygon;
  end;
  for i := 0 to Poly.Count - 1 do
    AddPolyNodeToPathsD(Poly[i], Paths);
end;
//------------------------------------------------------------------------------

function PolyTreeToPathsD(PolyTree: TPolyTreeD): TPathsD;
begin
  Result := nil;
  AddPolyNodeToPathsD(PolyTree, Result);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function BooleanOp(clipType: TClipType;
  const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  with TClipper64.Create do
  try
    AddSubject(subjects);
    AddClip(clips);
    Execute(clipType, fillRule, Result);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function BooleanOp(clipType: TClipType; const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  with TClipperD.Create(decimalPrec) do
  try
    AddSubject(subjects);
    AddClip(clips);
    Execute(clipType, fillRule, Result);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

procedure BooleanOp(clipType: TClipType; const subjects, clips: TPaths64;
  fillRule: TFillRule; polytree: TPolyTree64);
var
  dummy: TPaths64;
begin
  with TClipper64.Create do
  try
    AddSubject(subjects);
    AddClip(clips);
    Execute(clipType, fillRule, polytree, dummy);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function Intersect(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctIntersection, subjects, clips, fillRule);
end;
//------------------------------------------------------------------------------

function Union(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctUnion, subjects, clips, fillRule);
end;
//------------------------------------------------------------------------------

function Union(const subjects: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctUnion, subjects, nil, fillRule);
end;
//------------------------------------------------------------------------------

function Difference(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctDifference, subjects, clips, fillRule);
end;
//------------------------------------------------------------------------------

function XOR_(const subjects, clips: TPaths64; fillRule: TFillRule): TPaths64;
begin
  Result := BooleanOp(ctXor, subjects, clips, fillRule);
end;
//------------------------------------------------------------------------------

function Intersect(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctIntersection, subjects, clips, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------

function Union(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctUnion, subjects, clips, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------

function Union(const subjects: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctUnion, subjects, nil, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------

function Difference(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctDifference, subjects, clips, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------

function XOR_(const subjects, clips: TPathsD;
  fillRule: TFillRule; decimalPrec: integer = 2): TPathsD;
begin
  Result := BooleanOp(ctXor, subjects, clips, fillRule, decimalPrec);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function InflatePaths(const paths: TPaths64; delta: Double;
  jt: TJoinType; et: TEndType; MiterLimit: double;
  ArcTolerance: double): TPaths64;
var
  co: TClipperOffset;
begin
  co := TClipperOffset.Create(MiterLimit, ArcTolerance);
  try
    co.AddPaths(paths, jt, et);
    co.Execute(delta, Result);
  finally
    co.free;
  end;
end;
//------------------------------------------------------------------------------

function InflatePaths(const paths: TPathsD; delta: Double;
  jt: TJoinType; et: TEndType; miterLimit: double;
  precision: integer; ArcTolerance: double): TPathsD;
var
  pp: TPaths64;
  scale, invScale: double;
begin
  CheckPrecisionRange(precision);
  scale := Power(10, precision);
  invScale := 1/scale;
  pp := ScalePaths(paths, scale, scale);

  with TClipperOffset.Create(miterLimit, ArcTolerance) do
  try
    AddPaths(pp, jt, et);
    Execute(delta * scale, pp); // reuse pp to receive the solution.
  finally
    free;
  end;
  Result := ScalePathsD(pp, invScale, invScale);
end;
//------------------------------------------------------------------------------

function RectClip(const rect: TRect64;
  const path: TPath64): TPath64;
var
  paths: TPaths64;
begin
  SetLength(paths, 1);
  paths[0] := path;
  paths := RectClip(rect, paths);
  if Assigned(paths) then
    Result := paths[0] else
    Result := nil;
end;
//------------------------------------------------------------------------------

function RectClip(const rect: TRect64; const paths: TPaths64): TPaths64;
begin
  Result := nil;
  if rect.IsEmpty then Exit;
  with TRectClip64.Create(rect) do
  try
    Result := Execute(paths);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function RectClip(const rect: TRectD; const path: TPathD; precision: integer): TPathD;
var
  scale: double;
  tmpPath: TPath64;
  rec: TRect64;
begin
  Result := nil;
  if not rect.Intersects(GetBounds(path)) then Exit;
  CheckPrecisionRange(precision);
  scale := Math.Power(10, precision);
  rec := Rect64(ScaleRect(rect, scale));
  tmpPath := ScalePath(path, scale);
  tmpPath := RectClip(rec, tmpPath);
  Result := ScalePathD(tmpPath, 1/scale);
end;
//------------------------------------------------------------------------------

function RectClip(const rect: TRectD; const paths: TPathsD; precision: integer): TPathsD;
var
  scale: double;
  tmpPaths: TPaths64;
  rec: TRect64;
begin
  CheckPrecisionRange(precision);
  scale := Math.Power(10, precision);
  rec := Rect64(ScaleRect(rect, scale));

  tmpPaths := ScalePaths(paths, scale);
  with TRectClip64.Create(rec) do
  try
    tmpPaths := Execute(tmpPaths);
  finally
    Free;
  end;
  Result := ScalePathsD(tmpPaths, 1/scale);
end;
//------------------------------------------------------------------------------

function RectClipLines(const rect: TRect64; const path: TPath64): TPaths64;
var
  tmp: TPaths64;
begin
  Result := nil;
  SetLength(tmp, 1);
  tmp[0] := path;
  with TRectClipLines64.Create(rect) do
  try
    Result := Execute(tmp);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function RectClipLines(const rect: TRect64; const paths: TPaths64): TPaths64;
begin
  Result := nil;
  if rect.IsEmpty then Exit;
  with TRectClipLines64.Create(rect) do
  try
    Result := Execute(paths);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function RectClipLines(const rect: TRectD;
  const path: TPathD; precision: integer): TPathsD;
var
  scale: double;
  tmpPath: TPath64;
  tmpPaths: TPaths64;
  rec: TRect64;
begin
  Result := nil;
  if not rect.Intersects(GetBounds(path)) then Exit;
  CheckPrecisionRange(precision);
  scale := Math.Power(10, precision);
  rec := Rect64(ScaleRect(rect, scale));
  tmpPath := ScalePath(path, scale);
  tmpPaths := RectClipLines(rec, tmpPath);
  Result := ScalePathsD(tmpPaths, 1/scale);
end;
//------------------------------------------------------------------------------

function RectClipLines(const rect: TRectD; const paths: TPathsD;
  precision: integer = 2): TPathsD;
var
  scale: double;
  tmpPaths: TPaths64;
  rec: TRect64;
begin
  Result := nil;
  if rect.IsEmpty then Exit;
  CheckPrecisionRange(precision);
  scale := Math.Power(10, precision);
  rec := Rect64(ScaleRect(rect, scale));
  tmpPaths := ScalePaths(paths, scale);
  with TRectClipLines64.Create(rec) do
  try
    tmpPaths := Execute(tmpPaths);
  finally
    Free;
  end;
  Result := ScalePathsD(tmpPaths, 1/scale);
end;
//------------------------------------------------------------------------------

function TranslatePath(const path: TPath64; dx, dy: Int64): TPath64;
var
  i, len: integer;
begin
  len := length(path);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i].x := path[i].x + dx;
    result[i].y := path[i].y + dy;
  end;
end;
//------------------------------------------------------------------------------

function TranslatePath(const path: TPathD; dx, dy: double): TPathD;
var
  i, len: integer;
begin
  len := length(path);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i].x := path[i].x + dx;
    result[i].y := path[i].y + dy;
  end;
end;
//------------------------------------------------------------------------------

function TranslatePaths(const paths: TPaths64; dx, dy: Int64): TPaths64;
var
  i, len: integer;
begin
  len := length(paths);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i] := TranslatePath(paths[i], dx, dy);
  end;
end;
//------------------------------------------------------------------------------

function TranslatePaths(const paths: TPathsD; dx, dy: double): TPathsD;
var
  i, len: integer;
begin
  len := length(paths);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i] := TranslatePath(paths[i], dx, dy);
  end;
end;
//------------------------------------------------------------------------------

function MinkowskiSum(const pattern, path: TPath64;
  pathIsClosed: Boolean): TPaths64;
begin
 Result := Clipper.Minkowski.MinkowskiSum(pattern, path, pathIsClosed);
end;
//------------------------------------------------------------------------------

function MinkowskiSum(const pattern, path: TPathD;
  pathIsClosed: Boolean): TPathsD;
begin
 Result := Clipper.Minkowski.MinkowskiSum(pattern, path, pathIsClosed);
end;
//------------------------------------------------------------------------------

function PathToString(const p: TPath64;
  indentSpaces: integer; pointsPerRow: integer): string;
var
  i, highI: Integer;
  spaces: string;
begin
  spaces := StringOfChar(' ', indentSpaces);
  Result := spaces;
  highI := high(p);
  if highI < 0 then Exit;
  for i := 0 to highI -1 do
  begin
    Result := Result + format('%d,%d, ',[p[i].X,p[i].Y]);
    if (pointsPerRow > 0) and ((i + 1) mod pointsPerRow = 0) then
      Result := Result + #10 + spaces;
  end;
  Result := Result + format('%d,%d',[p[highI].X,p[highI].Y]);
end;
//------------------------------------------------------------------------------

function PathToString(const p: TPathD; decimals: integer;
  indentSpaces: integer; pointsPerRow: integer): string;
var
  i, highI: Integer;
  spaces: string;
begin
  spaces := StringOfChar(' ', indentSpaces);
  Result := '';
  highI := high(p);
  if highI < 0 then Exit;
  for i := 0 to highI -1 do
    Result := Result + format('%1.*n,%1.*n, ',
      [decimals, p[i].X, decimals, p[i].Y]);
  Result := Result + format('%1.*n,%1.*n',[
    decimals, p[highI].X, decimals, p[highI].Y]);
end;
//------------------------------------------------------------------------------

function PathsToString(const p: TPaths64;
  indentSpaces: integer = 0; pointsPerRow: integer = 0): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to High(p) do
    Result := Result + PathToString(p[i], indentSpaces, pointsPerRow) + #10#10;
end;
//------------------------------------------------------------------------------

function PathsToString(const p: TPathsD; decimals: integer;
  indentSpaces: integer = 0; pointsPerRow: integer = 0): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to High(p) do
    Result := Result + PathToString(p[i], indentSpaces, pointsPerRow) + #10#10;
end;
//------------------------------------------------------------------------------

procedure ShowPolyPathStructure64(pp: TPolyPath64; level: integer;
  strings: TStrings);
var
  i: integer;
  spaces, plural: string;
begin
  spaces := StringOfChar(' ', level * 2);
  if pp.Count = 1 then plural := '' else plural := 's';
  if pp.IsHole then
    strings.Add(Format('%sA hole containing %d polygon%s', [spaces, pp.Count, plural]))
  else
    strings.Add(Format('%sA polygon containing %d hole%s', [spaces, pp.Count, plural]));
  for i := 0 to pp.Count -1 do
    if pp.child[i].Count> 0 then
      ShowPolyPathStructure64(pp.child[i], level + 1, strings);
end;
//------------------------------------------------------------------------------

procedure ShowPolyTreeStructure(polytree: TPolyTree64; strings: TStrings);
var
  i: integer;
begin
  if polytree.Count = 1 then
    strings.Add('Polytree with just 1 polygon.') else
    strings.Add(Format('Polytree with just %d polygons.', [polytree.Count]));
  for i := 0 to polytree.Count -1 do
    if polytree[i].Count > 0 then
      ShowPolyPathStructure64(polytree[i], 1, strings);
end;
//------------------------------------------------------------------------------

procedure ShowPolyPathStructureD(pp: TPolyPathD; level: integer; strings: TStrings);
var
  i: integer;
  spaces, plural: string;
begin
  spaces := StringOfChar(' ', level * 2);
  if pp.Count = 1 then plural := '' else plural := 's';
  if pp.IsHole then
    strings.Add(Format('%sA hole containing %d polygon%s', [spaces, pp.Count, plural]))
  else
    strings.Add(Format('%sA polygon containing %d hole%s', [spaces, pp.Count, plural]));
  for i := 0 to pp.Count -1 do
    if pp.child[i].Count> 0 then
      ShowPolyPathStructureD(pp.child[i], level + 1, strings);
end;
//------------------------------------------------------------------------------

procedure ShowPolyTreeStructure(polytree: TPolyTreeD; strings: TStrings);
var
  i: integer;
begin
  if polytree.Count = 1 then
    strings.Add('Polytree with just 1 polygon.') else
    strings.Add(Format('Polytree with just %d polygons.', [polytree.Count]));
  for i := 0 to polytree.Count -1 do
    if polytree[i].Count > 0 then
      ShowPolyPathStructureD(polytree[i], 1, strings);
end;
//------------------------------------------------------------------------------

function TrimCollinear(const p: TPath64; isOpenPath: Boolean = false): TPath64;
var
  i,j, len: integer;
begin
  len := Length(p);

  i := 0;
  if not isOpenPath then
  begin
    while (i < len -1) and
      IsCollinear(p[len -1], p[i], p[i+1]) do inc(i);
    while (i < len -1) and
      IsCollinear(p[len -2], p[len -1], p[i]) do dec(len);
  end;
  if (len - i < 3) then
  begin
    if not isOpenPath or (len < 2) or PointsEqual(p[0], p[1]) then
      Result := nil else
      Result := p;
    Exit;
  end;

  SetLength(Result, len -i);

  Result[0] := p[i];
  j := 0;
  for i := i+1 to len -2 do
    if not IsCollinear(result[j], p[i], p[i+1]) then
    begin
      inc(j);
      result[j] := p[i];
    end;

  if isOpenPath then
  begin
    inc(j);
    result[j] := p[len-1];
  end
  else if not IsCollinear(result[j], p[len-1], result[0]) then
  begin
    inc(j);
    result[j] := p[len-1];
  end else
  begin
    while (j > 1) and
      IsCollinear(result[j-1], result[j], result[0]) do dec(j);
    if j < 2 then j := -1;
  end;
  SetLength(Result, j +1);
end;
//------------------------------------------------------------------------------

function TrimCollinear(const path: TPathD;
  precision: integer; isOpenPath: Boolean = false): TPathD;
var
  p: TPath64;
  scale: double;
begin
  scale := power(10, precision);
  p := ScalePath(path, scale);
  p := TrimCollinear(p, isOpenPath);
  Result := ScalePathD(p, 1/scale);
end;
//------------------------------------------------------------------------------

function PointInPolygon(const pt: TPoint64;
  const polygon: TPath64): TPointInPolygonResult;
begin
  Result := Clipper.Core.PointInPolygon(pt, polygon);
end;
//------------------------------------------------------------------------------

function DistanceSqrd(const pt1, pt2: TPoint64): double;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  x1,y1,x2,y2: double;
begin
  // nb: older versions of Delphi don't allow explicit typcasting
  x1 := pt1.X; y1 := pt1.Y;
  x2 := pt2.X; y2 := pt2.Y;
  result := Sqr(x1 - x2) + Sqr(y1 - y2);
end;
//------------------------------------------------------------------------------

function PerpendicDistSqrd(const pt, line1, line2: TPoint64): double;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  a,b,c,d: double;
begin
  a := pt.X - line1.X;
  b := pt.Y - line1.Y;
  c := line2.X - line1.X;
  d := line2.Y - line1.Y;
  result := Iif((c = 0) and (d = 0),
    0, Sqr(a * d - c * b) / (c * c + d * d));
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

type
  PSimplifyRec = ^TSimplifyRec;
  TSimplifyRec = record
    pt      : TPoint64;
    pdSqrd  : double;
    prev    : PSimplifyRec;
    next    : PSimplifyRec;
    //isEnd   : Boolean;
  end;

function SimplifyPath(const path: TPath64;
  shapeTolerance: double; isClosedPath: Boolean): TPath64;
var
  i, highI, minHigh: integer;
  tolSqrd: double;
  srArray: array of TSimplifyRec;
  first, last: PSimplifyRec;
begin
  Result := nil;
  highI := High(path);

  minHigh := Iif(isClosedPath, 2, 1);
  if highI < minHigh then Exit;

  SetLength(srArray, highI +1);
  with srArray[0] do
  begin
    pt      := path[0];
    prev    := @srArray[highI];
    next    := @srArray[1];
    pdSqrd  := Iif(isClosedPath,
      PerpendicDistSqrd(path[0], path[highI], path[1]), invalidD);
  end;

  with srArray[highI] do
  begin
    pt      := path[highI];
    prev    := @srArray[highI-1];
    next    := @srArray[0];
    pdSqrd  := Iif(isClosedPath,
      PerpendicDistSqrd(path[highI], path[highI-1], path[0]), invalidD);
  end;

  for i := 1 to highI -1 do
    with srArray[i] do
    begin
      pt      := path[i];
      prev    := @srArray[i-1];
      next    := @srArray[i+1];
      pdSqrd  := PerpendicDistSqrd(path[i], path[i-1], path[i+1]);
    end;

  first := @srArray[0];
  last := first.prev;

  tolSqrd := Sqr(shapeTolerance);
  while first <> last do
  begin
    if (first.pdSqrd > tolSqrd) or
      (first.next.pdSqrd < first.pdSqrd) then
    begin
      first := first.next;
      Continue;
    end;
    dec(highI);
    first.prev.next := first.next;
    first.next.prev := first.prev;
    last := first.prev;
    first := last.next;
    if first.next = first.prev then break;
    last.pdSqrd := PerpendicDistSqrd(last.pt, last.prev.pt, first.pt);
    first.pdSqrd := PerpendicDistSqrd(first.pt, last.pt, first.next.pt);
  end;
  if highI < minHigh then Exit;
  if not isClosedPath then first := @srArray[0];
  SetLength(Result, highI +1);
  for i := 0 to HighI do
  begin
    Result[i] := first.pt;
    first := first.next;
  end;
end;
//------------------------------------------------------------------------------

function SimplifyPaths(const paths: TPaths64;
  shapeTolerance: double; isClosedPath: Boolean): TPaths64;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    result[i] := SimplifyPath(paths[i], shapeTolerance, isClosedPath);
end;
//------------------------------------------------------------------------------

function SimplifyPath(const path: TPathD; shapeTolerance: double;
  isClosedPath: Boolean; decimalPrecision: integer): TPathD;
var
  p: TPath64;
  scale: double;
begin
  scale := power(10, decimalPrecision);
  p := ScalePath(path, scale);
  p := SimplifyPath(p, shapeTolerance, isClosedPath);
  Result := ScalePathD(p, 1/scale);
end;
//------------------------------------------------------------------------------

function SimplifyPaths(const paths: TPathsD; shapeTolerance: double;
  isClosedPath: Boolean; decimalPrecision: integer): TPathsD;
var
  pp: TPaths64;
  scale: double;
begin
  scale := power(10, decimalPrecision);
  pp := ScalePaths(paths, scale);
  pp := SimplifyPaths(pp, shapeTolerance, isClosedPath);
  Result := ScalePathsD(pp, 1/scale);
end;
//------------------------------------------------------------------------------


end.


