unit Clipper.Minkowski;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Date      :  21 December 2023                                                *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Minkowski Addition and Difference                               *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

{$I Clipper.inc}

interface

uses
  Classes, Math, Clipper.Core;

function MinkowskiSum(const Pattern, Path: TPath64;
  PathIsClosed: Boolean): TPaths64; overload;
function MinkowskiSum(const Pattern, Path: TPathD;
  PathIsClosed: Boolean; decimalPlaces: integer = 2): TPathsD; overload;
function MinkowskiDiff(const path1, path2: TPath64): TPaths64; overload;
function MinkowskiDiff(const Pattern, Path: TPathD;
  PathIsClosed: Boolean; decimalPlaces: integer): TPathsD; overload;

implementation

uses
  Clipper;

function AddPoints(val1, val2: TPoint64): TPoint64;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result.X := val1.X + val2.X;
  Result.Y := val1.Y + val2.Y;
end;
//------------------------------------------------------------------------------

function SubtractPoints(val1, val2: TPoint64): TPoint64;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result.X := val1.X - val2.X;
  Result.Y := val1.Y - val2.Y;
end;
//------------------------------------------------------------------------------

function Minkowski(const Base, Path: TPath64;
  IsSum: Boolean; IsClosed: Boolean): TPaths64;
var
  i,j,k,g,h, delta, baseLen, pathLen: integer;
  tmp: TPaths64;
  quad: TPath64;
begin
  delta := Iif(IsClosed, 0 , 1);
  baseLen := Length(Base);
  pathLen := Length(Path);
  setLength(tmp, pathLen);

  for i := 0 to pathLen -1 do
  begin
    setLength(tmp[i], baseLen);
    if IsSum then
      for j := 0 to baseLen -1 do
        tmp[i][j] := AddPoints(Path[i], Base[j])
    else
      for j := 0 to baseLen -1 do
        tmp[i][j] := SubtractPoints(Path[i], Base[j]);
  end;

  SetLength(quad, 4);
  SetLength(Result, (pathLen - delta) * baseLen);
  g := Iif(IsClosed, pathLen - 1, 0);

  for i := delta to pathLen - 1 do
  begin
    h := baseLen - 1;
    k := (i - delta) * baseLen;
    for j := 0 to baseLen - 1 do
    begin
      quad[0] := tmp[g][h];
      quad[1] := tmp[i][h];
      quad[2] := tmp[i][(j)];
      quad[3] := tmp[g][(j)];
      if not IsPositive(quad) then
        Result[k + j] := ReversePath(quad) else
        Result[k + j] := copy(quad, 0, 4);
      h := j;
    end;
    g := i;
  end;
end;
//------------------------------------------------------------------------------

function MinkowskiSum(const Pattern, Path: TPath64; PathIsClosed: Boolean): TPaths64;
begin
   Result := Union( Minkowski(Pattern, Path, true, PathIsClosed), frNonZero);
end;
//------------------------------------------------------------------------------

function MinkowskiSum(const Pattern, Path: TPathD;
  PathIsClosed: Boolean; decimalPlaces: integer): TPathsD;
var
  tmp: TPaths64;
  scale: double;
begin
  scale := Power(10, decimalPlaces);
  tmp := Union( Minkowski(ScalePath(Pattern, scale),
    ScalePath(Path, scale), true, PathIsClosed), frNonZero);
  Result := ScalePathsD(tmp, 1/scale);
end;
//------------------------------------------------------------------------------

function MinkowskiDiff(const path1, path2: TPath64): TPaths64;
begin
  Result := Union( Minkowski(path1, path2, false, true), frNonZero);
end;
//------------------------------------------------------------------------------

function MinkowskiDiff(const Pattern, Path: TPathD;
  PathIsClosed: Boolean; decimalPlaces: integer): TPathsD;
var
  tmp: TPaths64;
  scale: double;
begin
  scale := Power(10, decimalPlaces);
  tmp := Union( Minkowski(ScalePath(Pattern, scale),
    ScalePath(Path, scale), false, PathIsClosed), frNonZero);
  Result := ScalePathsD(tmp, 1/scale);
end;
//------------------------------------------------------------------------------

end.

