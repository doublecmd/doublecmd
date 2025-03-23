unit Clipper.RectClip;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  5 July 2024                                                     *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2024                                         *
* Purpose   :  FAST rectangular clipping                                       *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Clipper.inc}

uses
  Classes, Math, SysUtils, Clipper.Core;

type
  TLocation = (locLeft, locTop, locRight, locBottom, locInside);

  POutPt2 = ^TOutPt2;
  POutPtArray = ^TOutPtArray;
  TOutPtArray  = array of POutPt2;
  TOutPtArrayArray = array of TOutPtArray;

  TOutPt2 = record
    ownerIdx: Cardinal;
    edge: POutPtArray;
    pt: TPoint64;
    next: POutPt2;
    prev: POutPt2;
  end;

  TRectClip64 = class
    procedure ExecuteInternal(const path: TPath64);
    function GetPath(resultIdx: integer): TPath64;
  protected
    fResults        : TList;
    fRect           : TRect64;
    fPathBounds     : TRect64;
    fRectPath       : TPath64;
    fRectMidPt      : TPoint64;
    fEdges          : TOutPtArrayArray;
    fStartLocs      : TList;
    procedure DisposeResults;
    procedure CheckEdges;
    procedure TidyEdgePair(idx: integer; var cw, ccw: TOutPtArray);
    function Add(const pt: TPoint64; startNewPath: Boolean = false): POutPt2;
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure AddCorner(prev, curr: TLocation); overload;
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure AddCorner(var loc: TLocation; isClockwise: Boolean); overload;
      {$IFDEF INLINING} inline; {$ENDIF}
    procedure GetNextLocation(const path: TPath64;
      var loc: TLocation; var i: integer; highI: integer);
  public
    constructor Create(const rect: TRect64);
    destructor Destroy; override;
    function Execute(const paths: TPaths64): TPaths64;
  end;

  TRectClipLines64 = class(TRectClip64)
  private
    procedure ExecuteInternal(const path: TPath64);
    function GetPath(resultIdx: integer): TPath64;
  public
    function Execute(const paths: TPaths64): TPaths64;
  end;

implementation

type
  PPath64 = ^TPath64;

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function GetLocation(const rec: TRect64; const pt: TPoint64;
  out loc: TLocation): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := false; // only returns false when pt on rect
  if (pt.X = rec.Left) and
    (pt.Y >= rec.Top) and (pt.Y <= rec.Bottom) then
  begin
    loc := locLeft;
    Exit; //false
  end
  else if (pt.X = rec.Right) and
    (pt.Y >= rec.Top) and (pt.Y <= rec.Bottom) then
  begin
    loc := locRight;
    Exit; //false
  end
  else if (pt.Y = rec.Top) and
    (pt.X >= rec.Left) and (pt.X <= rec.Right) then
  begin
    loc := locTop;
    Exit; //false
  end
  else if (pt.Y = rec.Bottom) and
    (pt.X >= rec.Left) and (pt.X <= rec.Right) then
  begin
    loc := locBottom;
    Exit; //false
  end
  else if (pt.X < rec.Left) then loc := locLeft
  else if (pt.X > rec.Right) then loc := locRight
  else if (pt.Y < rec.Top) then loc := locTop
  else if (pt.Y > rec.Bottom) then loc := locBottom
  else loc := locInside;
  Result := true;
end;
//------------------------------------------------------------------------------

function IsHorizontal(pt1: TPoint64; pt2: TPoint64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := pt1.Y = pt2.Y;
end;
//------------------------------------------------------------------------------

function GetSegmentIntersectPt2(p1: TPoint64;
p2: TPoint64; p3: TPoint64; p4: TPoint64; out ip: TPoint64): Boolean;
var
  res1, res2, res3, res4: double;
begin
  res1 := CrossProduct(p1, p3, p4);
  res2 := CrossProduct(p2, p3, p4);
  if (res1 = 0) then
  begin
    ip := p1;
    if (res2 = 0) then
      result := false // segments are collinear
    else if PointsEqual(p1, p3) or PointsEqual(p1, p4) then
      result := true
    else if (IsHorizontal(p3, p4)) then
      result := ((p1.X > p3.X) = (p1.X < p4.X))
    else
      result := (p1.Y > p3.Y) = (p1.Y < p4.Y);
    Exit;
  end;

  if (res2 = 0) then
  begin
    ip := p2;
    if PointsEqual(p2, p3) or PointsEqual(p2, p4) then
      Result := true
    else if (IsHorizontal(p3, p4)) then
      Result := ((p2.X > p3.X) = (p2.X < p4.X))
    else Result := ((p2.Y > p3.Y) = (p2.Y < p4.Y));
    Exit;
  end;

  if ((res1 > 0) = (res2 > 0)) then
  begin
    //ip := Point64(0, 0);
    Result := false;
    Exit;
  end;

  res3 := CrossProduct(p3, p1, p2);
  res4 := CrossProduct(p4, p1, p2);
  if (res3 = 0) then
  begin
    ip := p3;
    if PointsEqual(p3, p1) or PointsEqual(p3, p2) then
      Result := true
    else if (IsHorizontal(p1, p2)) then
      Result := (p3.X > p1.X) = (p3.X < p2.X)
    else
      Result := (p3.Y > p1.Y) = (p3.Y < p2.Y);
  end
  else if (res4 = 0) then
  begin
    ip := p4;
    if PointsEqual(p4, p1) or PointsEqual(p4, p2) then
      Result := true
    else if (IsHorizontal(p1, p2)) then
      Result := (p4.X > p1.X) = (p4.X < p2.X)
    else
      Result := (p4.Y > p1.Y) = (p4.Y < p2.Y);
  end
  else if ((res3 > 0) = (res4 > 0)) then
  begin
    //ip := Point64(0, 0);
    Result := false;
  end
  else
    // segments must intersect to get here
    Result := GetSegmentIntersectPt(p1, p2, p3, p4, ip);
end;
//------------------------------------------------------------------------------

function GetIntersection(const rectPath: TPath64;
  const p, p2: TPoint64; var loc: TLocation; out ip: TPoint64): Boolean;
begin
  // gets the intersection closest to 'p'
  // when Result = false, loc will remain unchanged
  Result := True;
  case loc of
    locLeft:
      if GetSegmentIntersectPt2(p, p2, rectPath[0], rectPath[3], ip) then
        //Result := True
      else if (p.Y < rectPath[0].Y) and
        GetSegmentIntersectPt2(p, p2, rectPath[0], rectPath[1], ip) then
          loc := locTop
      else if GetSegmentIntersectPt2(p, p2, rectPath[2], rectPath[3], ip) then
        loc := locBottom
      else
        Result := False;

    locRight:
      if GetSegmentIntersectPt2(p, p2, rectPath[1], rectPath[2], ip) then
        //Result := True
      else if (p.Y < rectPath[0].Y) and
        GetSegmentIntersectPt2(p, p2, rectPath[0], rectPath[1], ip) then
          loc := locTop
      else if GetSegmentIntersectPt2(p, p2, rectPath[2], rectPath[3], ip) then
        loc := locBottom
      else
        Result := False;

    locTop:
      if GetSegmentIntersectPt2(p, p2, rectPath[0], rectPath[1], ip) then
        //Result := True
      else if (p.X < rectPath[0].X) and
        GetSegmentIntersectPt2(p, p2, rectPath[0], rectPath[3], ip) then
          loc := locLeft
      else if (p.X > rectPath[1].X) and
        GetSegmentIntersectPt2(p, p2, rectPath[1], rectPath[2], ip) then
          loc := locRight
      else
        Result := False;

    locBottom:
      if GetSegmentIntersectPt2(p, p2, rectPath[2], rectPath[3], ip) then
        //Result := True
      else if (p.X < rectPath[3].X) and
        GetSegmentIntersectPt2(p, p2, rectPath[0], rectPath[3], ip) then
          loc := locLeft
      else if (p.X > rectPath[2].X) and
        GetSegmentIntersectPt2(p, p2, rectPath[1], rectPath[2], ip) then
          loc := locRight
      else
        Result := False;

    else // loc = rInside
    begin
      if GetSegmentIntersectPt2(p, p2, rectPath[0], rectPath[3], ip) then
        loc := locLeft
      else if GetSegmentIntersectPt2(p, p2, rectPath[0], rectPath[1], ip) then
        loc := locTop
      else if GetSegmentIntersectPt2(p, p2, rectPath[1], rectPath[2], ip) then
        loc := locRight
      else if GetSegmentIntersectPt2(p, p2, rectPath[2], rectPath[3], ip) then
        loc := locBottom
      else
        Result := False;
    end;
  end;
end;
//------------------------------------------------------------------------------

function AreOpposites(prev, curr: TLocation): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := Abs(Ord(prev) - Ord(curr)) = 2;
end;
//------------------------------------------------------------------------------

function HeadingClockwise(prev, curr: TLocation): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (Ord(prev) + 1) mod 4 = Ord(curr);
end;
//------------------------------------------------------------------------------

function GetAdjacentLocation(loc: TLocation; isClockwise: Boolean): TLocation;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  delta: integer;
begin
  delta := Iif(isClockwise, 1 , 3);
  Result := TLocation((Ord(loc) + delta) mod 4);
end;
//------------------------------------------------------------------------------

function IsClockwise(prev, curr: TLocation;
  const prevPt, currPt, rectMidPt: TPoint64): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := Iif(AreOpposites(prev, curr),
    CrossProduct(prevPt, rectMidPt, currPt) < 0,
    HeadingClockwise(prev, curr));
end;
//------------------------------------------------------------------------------

function CountOp(op: POutPt2): integer;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  op2: POutPt2;
begin
  if not Assigned(op) then
  begin
    Result := 0;
    Exit;
  end;
  Result := 1;
  op2 := op;
  while op2.next <> op do
  begin
    inc(Result);
    op2 := op2.next;
  end;
end;
//------------------------------------------------------------------------------

procedure SetNewOwner(op: POutPt2; newIdx: integer);
  {$IFDEF INLINING} inline; {$ENDIF}
var
  op2: POutPt2;
begin
  op.ownerIdx := newIdx;
  op2 := op.next;
  while op2 <> op do
  begin
    op2.ownerIdx := newIdx;
    op2 := op2.next;
  end;
end;
//------------------------------------------------------------------------------

procedure AddToEdge(var edge: TOutPtArray; op: POutPt2);
  {$IFDEF INLINING} inline; {$ENDIF}
var
  len: integer;
begin
  if Assigned(op.edge) then Exit;
  op.edge := @edge;
  len := Length(edge);
  SetLength(edge, len+1);
  edge[len] := op;
end;
//------------------------------------------------------------------------------

function HasHorzOverlap(const left1, right1, left2, right2: TPoint64): boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (left1.X < right2.X) and (right1.X > left2.X);
end;
//------------------------------------------------------------------------------

function HasVertOverlap(const top1, bottom1, top2, bottom2: TPoint64): boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (top1.Y < bottom2.Y) and (bottom1.Y > top2.Y);
end;
//------------------------------------------------------------------------------

procedure UncoupleEdge(op: POutPt2); {$IFDEF INLINING} inline; {$ENDIF}
var
  i: integer;
begin
  if not Assigned(op.edge) then Exit;
  for i := 0 to High(POutPtArray(op.edge)^) do
    if POutPtArray(op.edge)^[i] = op then
    begin
      POutPtArray(op.edge)^[i] := nil;
      Break;
    end;
  op.edge := nil;
end;
//------------------------------------------------------------------------------

function DisposeOp(op: POutPt2): POutPt2;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if op.next = op then
    Result := nil else
    Result := op.next;
  op.prev.next := op.next;
  op.next.prev := op.prev;
  Dispose(op);
end;
//------------------------------------------------------------------------------

function DisposeOpBack(op: POutPt2): POutPt2;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if op.prev = op then
    Result := nil else
    Result := op.prev;
  op.prev.next := op.next;
  op.next.prev := op.prev;
  Dispose(op);
end;
//------------------------------------------------------------------------------

function GetEdgesForPt(const pt: TPoint64; const rec: TRect64): cardinal;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if pt.X = rec.Left then
    Result := 1
  else if pt.X = rec.Right then
    Result := 4
  else
    Result := 0;

  if pt.Y = rec.Top then
    inc(Result, 2)
  else if pt.Y = rec.Bottom then
    inc(Result, 8);
end;
//------------------------------------------------------------------------------

function IsHeadingClockwise(const pt1, pt2: TPoint64; edgeIdx: integer): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  case edgeIdx of
    0: Result := pt2.Y < pt1.Y;
    1: Result := pt2.X > pt1.X;
    2: Result := pt2.Y > pt1.Y;
    else Result := pt2.X < pt1.X;
  end;
end;

//------------------------------------------------------------------------------
// TRectClip64 class
//------------------------------------------------------------------------------

constructor TRectClip64.Create(const rect: TRect64);
begin
  fResults := TList.Create;

  fRect := rect;
  fRectPath := fRect.AsPath;
  fRectMidPt := rect.MidPoint;
  fStartLocs := TList.Create;
  SetLength(fEdges, 8);
end;
//------------------------------------------------------------------------------

destructor TRectClip64.Destroy;
begin
  fStartLocs.Free;
  fResults.Free;
end;
//------------------------------------------------------------------------------

procedure DisposeOps(op: POutPt2);
var
  tmp: POutPt2;
begin
  if not Assigned(op) then Exit;
  op.prev.next := nil;
  while assigned(op) do
  begin
    tmp := op;
    op := op.next;
    Dispose(tmp);
  end;
end;
//------------------------------------------------------------------------------

procedure TRectClip64.DisposeResults;
var
  i: integer;
begin
  for i := 0 to fResults.Count -1 do
    DisposeOps(fResults[i]);
  fResults.Clear;
end;
//------------------------------------------------------------------------------

function TRectClip64.Add(const pt: TPoint64; startNewPath: Boolean): POutPt2;
var
  currIdx: integer;
  prevOp: POutPt2;
begin
  // this method is only called by InternalExecute.
  // Later splitting and rejoining won't create additional op's,
  // though they will change the (non-storage) fResults count.
  currIdx := fResults.Count -1;
  if (currIdx < 0) or startNewPath then
  begin
    new(Result);
    Result.pt := pt;
    Result.edge := nil;
    Result.ownerIdx := fResults.Add(Result);
    Result.next := Result;
    Result.prev := Result;
  end else
  begin
    prevOp := fResults[currIdx];
    if PointsEqual(prevOp.pt, pt) then
    begin
      Result := prevOp;
      Exit;
    end;
    new(Result);
    Result.pt := pt;
    Result.edge := nil;
    Result.ownerIdx := currIdx;
    Result.next := prevOp.next;
    prevOp.next.prev := Result;
    prevOp.next := Result;
    Result.prev := prevOp;
    fResults[currIdx] := Result;
  end;
end;
//------------------------------------------------------------------------------

procedure TRectClip64.AddCorner(prev, curr: TLocation);
var
  cnrIdx: integer;
begin
  if prev = curr then Exit;
  cnrIdx := Iif(HeadingClockwise(prev, curr), Ord(prev), Ord(curr));
  Add(fRectPath[cnrIdx]);
end;
//------------------------------------------------------------------------------

procedure TRectClip64.AddCorner(var loc: TLocation; isClockwise: Boolean);
begin
  if (isClockwise) then
  begin
    Add(fRectPath[Ord(loc)]);
    loc := GetAdjacentLocation(loc, true);
  end else
  begin
    loc := GetAdjacentLocation(loc, false);
    Add(fRectPath[Ord(loc)]);
  end;
end;
//------------------------------------------------------------------------------

procedure TRectClip64.GetNextLocation(const path: TPath64;
  var loc: TLocation; var i: integer; highI: integer);
begin
  case loc of
    locLeft:
      begin
        while (i <= highI) and (path[i].X <= fRect.Left) do inc(i);
        if (i > highI) then Exit;
        if path[i].X >= fRect.Right then loc := locRight
        else if path[i].Y <= fRect.Top then loc := locTop
        else if path[i].Y >= fRect.Bottom then loc := locBottom
        else loc := locInside;
      end;
    locTop:
      begin
        while (i <= highI) and (path[i].Y <= fRect.Top) do inc(i);
        if (i > highI) then Exit;
        if path[i].Y >= fRect.Bottom then loc := locBottom
        else if path[i].X <= fRect.Left then loc := locLeft
        else if path[i].X >= fRect.Right then loc := locRight
        else loc := locInside;
      end;
    locRight:
      begin
        while (i <= highI) and (path[i].X >= fRect.Right) do inc(i);
        if (i > highI) then Exit;
        if path[i].X <= fRect.Left then loc := locLeft
        else if path[i].Y <= fRect.Top then loc := locTop
        else if path[i].Y >= fRect.Bottom then loc := locBottom
        else loc := locInside;
      end;
    locBottom:
      begin
        while (i <= highI) and (path[i].Y >= fRect.Bottom) do inc(i);
        if (i > highI) then Exit;
        if path[i].Y <= fRect.Top then loc := locTop
        else if path[i].X <= fRect.Left then loc := locLeft
        else if path[i].X >= fRect.Right then loc := locRight
        else loc := locInside;
      end;
    locInside:
      begin
        while (i <= highI) do
        begin
          if path[i].X < fRect.Left then loc := locLeft
          else if path[i].X > fRect.Right then loc := locRight
          else if path[i].Y > fRect.Bottom then loc := locBottom
          else if path[i].Y < fRect.Top then loc := locTop
          else begin Add(path[i]); inc(i); continue; end;
          break; //inner loop
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

function Path1ContainsPath2(const path1, path2: TPath64): Boolean;
var
  i, ioCount: integer;
  pip: TPointInPolygonResult;
begin
  ioCount := 0;
  for i := 0 to High(path2) do
  begin
    pip := PointInPolygon(path2[i], path1);
    case pip of
      pipOn: Continue;
      pipInside: dec(ioCount);
      pipOutside: inc(ioCount);
    end;
    if abs(ioCount) > 1 then break;
  end;
  Result := ioCount <= 0;
end;
//------------------------------------------------------------------------------

function TRectClip64.Execute(const paths: TPaths64): TPaths64;
var
  i,j, len: integer;
  path: TPath64;
begin
  result := nil;
  len:= Length(paths);
  for i := 0 to len -1 do
  begin
    path := paths[i];
    if (Length(path) < 3) then Continue;

    fPathBounds := GetBounds(path);
    if not fRect.Intersects(fPathBounds) then
      Continue // the path must be completely outside fRect
    else if fRect.Contains(fPathBounds) then
    begin
      // the path must be completely inside fRect
      AppendPath(Result, path);
      Continue;
    end;

    ExecuteInternal(path);
    CheckEdges;
    for j := 0 to 3 do
      TidyEdgePair(j, fEdges[j*2], fEdges[j*2 +1]);

    for j := 0 to fResults.Count -1 do
      AppendPath(Result, GetPath(j));

    //clean up after every loop
    DisposeResults;
    fEdges := nil;
    SetLength(fEdges, 8);
  end;
end;
//------------------------------------------------------------------------------

function StartLocsAreClockwise(const startLocs: TList): Boolean;
var
  i,j, res: integer;
begin
  res := 0;
  for i := 1 to startLocs.Count -1 do
  begin
    j := Ord(TLocation(startLocs[i])) - Ord(TLocation(startLocs[i - 1]));
    case j of
      -1: dec(res);
      1: inc(res);
      -3: inc(res);
      3: dec(res);
    end;
  end;
  result := res > 0;
end;
//------------------------------------------------------------------------------

procedure TRectClip64.ExecuteInternal(const path: TPath64);
var
  i,j, highI    : integer;
  prevPt,ip,ip2 : TPoint64;
  loc, prevLoc  : TLocation;
  loc2          : TLocation;
  startingLoc   : TLocation;
  firstCrossLoc : TLocation;
  crossingLoc   : TLocation;
  prevCrossLoc  : TLocation;
  isCw          : Boolean;
  startLocsCW   : Boolean;
begin
  if (Length(path) < 3) then Exit;
  fStartLocs.Clear;
  crossingLoc     := locInside;
  firstCrossLoc   := locInside;
  prevLoc         := locInside;

  highI := Length(path) -1;
  if not GetLocation(fRect, path[highI], loc) then
  begin
    i := highI - 1;
    while (i >= 0) and
      not GetLocation(fRect, path[i], prevLoc) do
        dec(i);

    if (i < 0) then
    begin
      // all of path must be inside fRect
      for i := 0 to highI do Add(path[i]);
      Exit;
    end;

    if (prevLoc = locInside) then
      loc := locInside;
  end;
  startingLoc := loc;

  ///////////////////////////////////////////////////
  i := 0;
  while i <= highI do
  begin
    prevLoc := loc;
    prevCrossLoc := crossingLoc;
    GetNextLocation(path, loc, i, highI);
    if i > highI then Break;

    if i = 0 then
      prevPt := path[highI] else
      prevPt := path[i-1];
    crossingLoc := loc;
    if not GetIntersection(fRectPath, path[i], prevPt, crossingLoc, ip) then
    begin
      // ie remains outside (and crossingLoc still == loc)
      if (prevCrossLoc = locInside) then //ie rect still uncrossed
      begin
        isCw := IsClockwise(prevLoc, loc, prevPt, path[i], fRectMidPt);
        repeat
          fStartLocs.Add(Pointer(prevLoc));
          prevLoc := GetAdjacentLocation(prevLoc, isCw);
        until prevLoc = loc;
        crossingLoc := prevCrossLoc; // because still not crossed
      end
      else if (prevLoc <> locInside) and (prevLoc <> loc) then
      begin
        isCw := IsClockwise(prevLoc, loc, prevPt, path[i], fRectMidPt);
        repeat
          AddCorner(prevLoc, isCw);
        until prevLoc = loc;
      end;
      inc(i);
      Continue;
    end;

    ////////////////////////////////////////////////////
    // we must be crossing the rect boundary to get here
    ////////////////////////////////////////////////////

    if (loc = locInside) then // path must be entering rect
    begin
      if (firstCrossLoc = locInside) then
      begin
        firstCrossLoc := crossingLoc;
        fStartLocs.Add(Pointer(prevLoc));
      end
      else if (prevLoc <> crossingLoc) then
      begin
        isCw := IsClockwise(prevLoc, crossingLoc, prevPt, path[i], fRectMidPt);
        repeat
          AddCorner(prevLoc, isCw);
        until prevLoc = crossingLoc;
      end;
    end
    else if (prevLoc <> locInside) then
    begin
      // passing right through rect. 'ip' here will be the second
      // intersect pt but we'll also need the first intersect pt (ip2)
      loc := prevLoc;
      GetIntersection(fRectPath, prevPt, path[i], loc, ip2);
      if (prevCrossLoc <> locInside) and (prevCrossLoc <> loc) then //#579
        AddCorner(prevCrossLoc, loc);

      if (firstCrossLoc = locInside) then
      begin
        firstCrossLoc := loc;
        fStartLocs.Add(Pointer(prevLoc));
      end;

      ////////////////////////////////
      Add(ip2);
      ////////////////////////////////

      loc := crossingLoc;
      if PointsEqual(ip, ip2) then
      begin
        // it's very likely that path[i] is on rect
        GetLocation(fRect, path[i], loc);
        AddCorner(crossingLoc, loc);
        crossingLoc := loc;
        Continue;
      end;
    end else // path must be exiting rect
    begin
      loc := crossingLoc;
      if (firstCrossLoc = locInside) then
        firstCrossLoc := crossingLoc;
    end;

    ////////////////////////////////
    Add(ip);
    ////////////////////////////////

  end; //while i <= highI
  ///////////////////////////////////////////////////

  if (firstCrossLoc = locInside) then
  begin
    // path never intersects
    if startingLoc <> locInside then
    begin
      // path is outside rect
      // but being outside, it still may not contain rect
      if fPathBounds.Contains(fRect) and
        Path1ContainsPath2(path, fRectPath) then
      begin
        // yep, the path does fully contain rect
        // so add rect to the solution
        startLocsCW := StartLocsAreClockwise(fStartLocs);
        for i := 0 to 3 do
        begin
          if startLocsCW then j := i else j := 3 - i;
          Add(fRectPath[j]);
          AddToEdge(fEdges[j*2], fResults[0]);
        end;
      end;
    end;
  end
  else if (loc <> locInside) and
    ((loc <> firstCrossLoc) or
    (fStartLocs.Count > 2)) then
  begin
    if (fStartLocs.Count > 0) then
    begin
      prevLoc := loc;
      for i := 0 to fStartLocs.Count -1 do
      begin
        loc2 := TLocation(fStartLocs[i]);
        if (prevLoc = loc2) then Continue;
        AddCorner(prevLoc, HeadingClockwise(prevLoc, loc2));
        prevLoc := loc2;
      end;
      loc := prevLoc;
    end;
    if (loc <> firstCrossLoc) then
      AddCorner(loc, HeadingClockwise(loc, firstCrossLoc));
  end;
end;
//------------------------------------------------------------------------------

procedure TRectClip64.CheckEdges;
var
  i,j: integer;
  edgeSet1, edgeSet2, combinedSet: Cardinal;
  op, op2: POutPt2;
begin
  for i := 0 to fResults.Count -1 do
  begin
    op := fResults[i];
    if not assigned(op) then Continue;

    op2 := op;
    repeat
      if IsCollinear(op2.prev.pt, op2.pt, op2.next.pt) then
      begin
        if op2 = op then
        begin
          op2 := DisposeOpBack(op2);
          if not assigned(op2) then break;
          op := op2.prev;
        end else
        begin
          op2 := DisposeOpBack(op2);
          if not assigned(op2) then break;
        end;
      end else
        op2 := op2.next;
    until (op2 = op);

    if not assigned(op2) then
    begin
      fResults[i] := nil;
      Continue;
    end;
    fResults[i] := op; // safety first

    edgeSet1 := GetEdgesForPt(op.prev.pt, fRect);
    op2 := op;
    repeat
      edgeSet2 := GetEdgesForPt(op2.pt, fRect);
      if (edgeSet2 <> 0) and not Assigned(op2.edge) then
      begin
        combinedSet := edgeSet1 and edgeSet2;
        for j := 0 to 3 do
          if combinedSet and (1 shl j) <> 0 then
          begin
            if IsHeadingClockwise(op2.prev.pt, op2.pt, j) then
              AddToEdge(fEdges[j*2], op2)
            else
              AddToEdge(fEdges[j*2+1], op2);
          end;
      end;
      edgeSet1 := edgeSet2;
      op2 := op2.next;
    until op2 = op;
  end;
end;
//------------------------------------------------------------------------------

procedure TRectClip64.TidyEdgePair(idx: integer; var cw, ccw: TOutPtArray);
var
  isHorz, cwIsTowardLarger: Boolean;
  i, j, highJ, newIdx: integer;
  op, op2, p1, p2, p1a, p2a: POutPt2;
  isRejoining, opIsLarger, op2IsLarger: Boolean;
begin
  // cw and ccw must be passed as var params
  // otherwise they'll only be local copies.
  // Alternatively cw and ccw could be POutPtArray locals,
  // but these require lots of dereferencing.
  if not Assigned(ccw) then Exit;
  isHorz := idx in [1,3];
  cwIsTowardLarger := idx in [1,2];
  i := 0; j := 0;
  while (i <= High(cw)) do
  begin

    p1 := cw[i];
    if not Assigned(p1) or (p1.next = p1.prev) then
    begin
      cw[i] := nil;
      inc(i);
      j := 0;
      Continue;
    end;

    highJ := high(ccw);
    while (j <= highJ) and
      (not Assigned(ccw[j]) or (ccw[j].next = ccw[j].prev)) do
        inc(j);

    if (j > highJ) then
    begin
      inc(i);
      j := 0;
      Continue;
    end;

    if cwIsTowardLarger then
    begin
      // p1 >>>> p1a;
      // p2 <<<< p2a;
      p1  := cw[i].prev;
      p1a := cw[i];
      p2  := ccw[j];
      p2a := ccw[j].prev;
    end else
    begin
      // p1 <<<< p1a;
      // p2 >>>> p2a;
      p1  := cw[i];
      p1a := cw[i].prev;
      p2  := ccw[j].prev;
      p2a := ccw[j];
    end;

    if (isHorz and not HasHorzOverlap(p1.pt, p1a.pt, p2.pt, p2a.pt)) or
      (not isHorz and not HasVertOverlap(p1.pt, p1a.pt, p2.pt, p2a.pt)) then
    begin
      inc(j);
      Continue;
    end;

    // to get here we're either splitting or rejoining
    isRejoining := cw[i].ownerIdx <> ccw[j].ownerIdx;

    if isRejoining then
    begin
      fResults[p2.ownerIdx] := nil;
      SetNewOwner(p2, p1.ownerIdx);
    end;

    // do the split or re-join
    if cwIsTowardLarger then
    begin
      // p1 >> | >> p1a;
      // p2 << | << p2a;
      p1.next := p2;
      p2.prev := p1;
      p1a.prev := p2a;
      p2a.next := p1a;
    end else
    begin
      // p1 << | << p1a;
      // p2 >> | >> p2a;
      p1.prev := p2;
      p2.next := p1;
      p1a.next := p2a;
      p2a.prev := p1a;
    end;

    if not isRejoining then
    begin
      NewIdx := fResults.Add(p1a);
      SetNewOwner(p1a, newIdx);
    end;

    if cwIsTowardLarger then
    begin
      op := p2;
      op2 := p1a;
    end else
    begin
      op := p1;
      op2 := p2a;
    end;

    fResults[op.ownerIdx] := op;
    fResults[op2.ownerIdx] := op2;

    // and now lots of work to get ready for the next loop

    if isHorz then // X
    begin
      opIsLarger := op.pt.X > op.prev.pt.X;
      op2IsLarger := op2.pt.X > op2.prev.pt.X;
    end else       // Y
    begin
      opIsLarger := op.pt.Y > op.prev.pt.Y;
      op2IsLarger := op2.pt.Y > op2.prev.pt.Y;
    end;

    if (op.next = op.prev) or
      PointsEqual(op.pt, op.prev.pt) then
    begin
      if op2IsLarger = cwIsTowardLarger then
      begin
        cw[i] := op2;
        ccw[j] := nil;
        inc(j);
      end else
      begin
        ccw[j] := op2;
        cw[i] := nil;
        inc(i);
      end;
    end
    else if (op2.next = op2.prev) or
      PointsEqual(op2.pt, op2.prev.pt) then
    begin
      if opIsLarger = cwIsTowardLarger then
      begin
        cw[i] := op;
        ccw[j] := nil;
        inc(j);
      end else
      begin
        ccw[j] := op;
        cw[i] := nil;
        inc(i);
      end;
    end
    else if opIsLarger = op2IsLarger then
    begin
      if opIsLarger = cwIsTowardLarger then
      begin
        cw[i] := op;
        UncoupleEdge(op2);
        AddToEdge(cw, op2);
        ccw[j] := nil;
        inc(j);
      end else
      begin
        cw[i] := nil;
        ccw[j] := op2;
        UncoupleEdge(op);
        AddToEdge(ccw, op);
        inc(i);
        j := 0;
      end;
    end else
    begin
      if opIsLarger = cwIsTowardLarger then
        cw[i] := op else
        ccw[j] := op;
      if op2IsLarger = cwIsTowardLarger then
        cw[i] := op2 else
        ccw[j] := op2;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TRectClip64.GetPath(resultIdx: integer): TPath64;
var
  i, len: integer;
  op, op2: POutPt2;
begin
  result := nil;
  op := fResults[resultIdx];
  if not Assigned(op) or (op.next = op.prev) then Exit;

  op2 := op.next;
  while Assigned(op2) and (op2 <> op) do
  begin
    if IsCollinear(op2.prev.pt, op2.pt, op2.next.pt) then
    begin
      op := op2.prev;
      op2 := DisposeOp(op2);
    end else
      op2 := op2.next;
  end;
  fResults[resultIdx] := op2; // needed for op cleanup
  if not Assigned(op2) then Exit;

  len := CountOp(op);
  SetLength(result, len);
  for i := 0 to len -1 do
  begin
    Result[i] := op.pt;
    op := op.next;
  end;
end;

//------------------------------------------------------------------------------
// TRectClipLines64
//------------------------------------------------------------------------------

function TRectClipLines64.Execute(const paths: TPaths64): TPaths64;
var
  i,j, len: integer;
  pathrec: TRect64;
begin
  result := nil;

  len:= Length(paths);
  for i := 0 to len -1 do
  begin
    pathrec := GetBounds(paths[i]);

    if not fRect.Intersects(pathRec) then
      Continue; // the path must be completely outside fRect
    // Apart from that, we can't be sure whether the path
    // is completely outside or completed inside or intersects
    // fRect, simply by comparing path bounds with fRect.

    ExecuteInternal(paths[i]);

    for j := 0 to fResults.Count -1 do
      AppendPath(Result, GetPath(j));
    DisposeResults;
    fEdges := nil;
    SetLength(fEdges, 8);
  end;
end;
//------------------------------------------------------------------------------

procedure TRectClipLines64.ExecuteInternal(const path: TPath64);
var
  i, highI      : integer;
  prevPt,ip,ip2 : TPoint64;
  loc, prev     : TLocation;
  crossingLoc   : TLocation;
begin
  if (Length(path) < 2) or fRect.IsEmpty then Exit;

  i := 1;
  highI := Length(path) -1;

  if not GetLocation(fRect, path[0], loc) then
  begin
    while (i <= highI) and
      not GetLocation(fRect, path[i], prev) do
        inc(i);
    if (i > highI) then
    begin
      for i := 0 to High(path) do Add(path[i]);
      Exit;
    end;
    if (prev = locInside) then
      loc := locInside;
    i := 1;
  end;
  if loc = locInside then Add(path[0]);

  ///////////////////////////////////////////////////
  while i <= highI do
  begin
    prev := loc;
    GetNextLocation(path, loc, i, highI);
    if i > highI then Break;
    prevPt := path[i-1];
    crossingLoc := loc;
    if not GetIntersection(fRectPath, path[i], prevPt, crossingLoc, ip) then
    begin
      // must be remaining outside
      inc(i);
      Continue;
    end;

    ////////////////////////////////////////////////////
    // we must be crossing the rect boundary to get here
    ////////////////////////////////////////////////////

    if (loc = locInside) then // path must be entering rect
    begin
      Add(ip, true);
    end
    else if (prev <> locInside) then
    begin
      // passing right through rect. 'ip' here will be the second
      // intersect pt but we'll also need the first intersect pt (ip2)
      crossingLoc := prev;
      GetIntersection(fRectPath, prevPt, path[i], crossingLoc, ip2);
      Add(ip2, true);
      Add(ip);
    end else // path must be exiting rect
      Add(ip);
  end; //while i <= highI
  ///////////////////////////////////////////////////
end;
//------------------------------------------------------------------------------

function TRectClipLines64.GetPath(resultIdx: integer): TPath64;
var
  i, len: integer;
  op: POutPt2;
begin
  result := nil;
  op := fResults[resultIdx];
  if not Assigned(op) or (op = op.prev) then Exit;
  len := CountOp(op);
  op := op.next; // ie start at first not last
  SetLength(result, len);
  for i := 0 to len -1 do
  begin
    Result[i] := op.pt;
    op := op.next;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.

