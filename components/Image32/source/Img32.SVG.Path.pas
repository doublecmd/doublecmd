unit Img32.SVG.Path;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.7                                                             *
* Date      :  6 January 2025                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
*                                                                              *
* Purpose   :  Essential structures and functions to read SVG Path elements    *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Types, Math,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Img32, Img32.SVG.Core, Img32.Vector, Img32.Text;

{$IFDEF ZEROBASEDSTR}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

type

  TSvgPathSegType =
    (stUnknown, stMove, stLine, stHorz, stVert, stArc,
    stQBezier, stCBezier, stQSpline, stCSpline, stClose);

  TArcInfo = record
    rec         : TRectD;
    startPos    : TPointD;
    endPos      : TPointD;
    rectAngle   : double;
    sweepClockW : Boolean;
  end;
  TArcInfos = array of TArcInfo;

  TSvgPath = class;
  TSvgSubPath = class;

  TSvgPathSeg = class
  private
    fParent   : TSvgSubPath;
    fOwner    : TSvgPath;
    fIdx      : integer;
    fFirstPt  : TPointD;
    fFlatPath : TPathD;
    fSegType  : TSvgPathSegType;
    fCtrlPts  : TPathD;
    fExtend   : integer;
  protected
    procedure Changed; {$IFDEF INLINE} inline; {$ENDIF}
    procedure RequireFlattened; virtual;
    function GetFlattened: TPathD; overload;
    procedure GetFlattened2(var Result: TPathD); overload;
    procedure GetFlattenedInternal; virtual; abstract;
    procedure Scale(value: double); virtual;
    function DescaleAndOffset(const pt: TPointD): TPointD; overload;
    function DescaleAndOffset(const p: TPathD): TPathD; overload;
    procedure SetCtrlPts(const pts: TPathD); virtual;
  public
    constructor Create(parent: TSvgSubPath;
      idx: integer; const firstPt : TPointD); virtual;
    function GetCtrlBounds: TRectD; virtual;
    function GetOnPathCtrlPts: TPathD; virtual;
    procedure Offset(dx, dy: double); virtual;
    function GetStringDef(relative: Boolean; decimalPrec: integer): string; virtual;
    function ExtendSeg(const pts: TPathD): Boolean; virtual;

    property Parent   : TSvgSubPath read fParent;
    property Owner    : TSvgPath read fOwner;
    property CtrlPts  : TPathD read fCtrlPts write SetCtrlPts;
    property FirstPt  : TPointD read fFirstPt;
    property FlatPath : TPathD read GetFlattened;
    property Index    : integer read fIdx;
    property SegType  : TSvgPathSegType read fSegType;
  end;

  TSvgStraightSeg = class(TSvgPathSeg)
  protected
    procedure GetFlattenedInternal; override;
  end;

  TSvgCurvedSeg = class(TSvgPathSeg)
  protected
    pendingScale: double;
    procedure RequireFlattened; override;
    function GetPreviousCtrlPt: TPointD;
  public
    function GetLastCtrlPt: TPointD; virtual;
    constructor Create(parent: TSvgSubPath; idx: integer;
      const firstPt : TPointD); override;
  end;

  TSvgASegment = class(TSvgCurvedSeg)
  private
    fRectTop  : Boolean;
    fRectLeft : Boolean;
    fArcInfo   : TArcInfo;
    procedure SetArcInfo(ai: TArcInfo);
    procedure GetRectBtnPoints(out pt1, pt2, pt3: TPointD);
    procedure SetCtrlPtsFromArcInfo;
  protected
    procedure SetCtrlPts(const ctrlPts: TPathD); override;
    procedure GetFlattenedInternal; override;
    procedure Scale(value: double); override;
  public
  public
    constructor Create(parent: TSvgSubPath; idx: integer;
      const firstPt : TPointD); override;
    procedure Offset(dx, dy: double); override;
    procedure ReverseArc;
    function  GetStartAngle: double;
    function  GetEndAngle: double;
    function  GetStringDef(relative: Boolean; decimalPrec: integer): string; override;
    property  ArcInfo: TArcInfo read fArcInfo write SetArcInfo;
    property  IsLeftCtrl: Boolean read fRectLeft;
    property  IsTopCtrl: Boolean read fRectTop;
  end;

  TSvgCSegment = class(TSvgCurvedSeg)
  protected
    procedure GetFlattenedInternal; override;
  public
    constructor Create(parent: TSvgSubPath; idx: integer;
      const firstPt : TPointD); override;
    function GetOnPathCtrlPts: TPathD; override;
    function  GetStringDef(relative: Boolean; decimalPrec: integer): string; override;
  end;

  TSvgHSegment = class(TSvgStraightSeg)
  public
    constructor Create(parent: TSvgSubPath; idx: integer;
      const firstPt : TPointD); override;
    function  GetStringDef(relative: Boolean; decimalPrec: integer): string; override;
  end;

  TSvgLSegment = class(TSvgStraightSeg)
  public
    constructor Create(parent: TSvgSubPath; idx: integer;
      const firstPt : TPointD); override;
    function  GetStringDef(relative: Boolean; decimalPrec: integer): string; override;
  end;

  TSvgQSegment = class(TSvgCurvedSeg)
  protected
    procedure GetFlattenedInternal; override;
  public
    constructor Create(parent: TSvgSubPath; idx: integer;
      const firstPt : TPointD); override;
    function GetOnPathCtrlPts: TPathD; override;
    function  GetStringDef(relative: Boolean; decimalPrec: integer): string; override;
  end;

  TSvgSSegment = class(TSvgCurvedSeg)
  protected
    procedure GetFlattenedInternal; override;
  public
    constructor Create(parent: TSvgSubPath; idx: integer;
      const firstPt : TPointD); override;
    function GetOnPathCtrlPts: TPathD; override;
    function  GetStringDef(relative: Boolean; decimalPrec: integer): string; override;
  end;

  TSvgTSegment = class(TSvgCurvedSeg)
  protected
    procedure GetFlattenedInternal; override;
  public
    constructor Create(parent: TSvgSubPath; idx: integer;
      const firstPt : TPointD); override;
    function GetLastCtrlPt: TPointD; override;
    function  GetStringDef(relative: Boolean; decimalPrec: integer): string; override;
  end;

  TSvgVSegment = class(TSvgStraightSeg)
  public
    constructor Create(parent: TSvgSubPath; idx: integer;
      const firstPt : TPointD); override;
    function  GetStringDef(relative: Boolean; decimalPrec: integer): string; override;
  end;

  TSvgZSegment = class(TSvgStraightSeg)
  public
    constructor Create(parent: TSvgSubPath; idx: integer;
      const firstPt : TPointD); override;
    function  GetStringDef(relative: Boolean; decimalPrec: integer): string; override;
  end;

  TSvgSegmentClass = class of TSvgPathSeg;

  TSvgSubPath = class
  private
    fParent       : TSvgPath;
    fSegs         : array of TSvgPathSeg;
    fPendingScale : double;
    fPathOffset   : TPointD;
    fSegsCount    : integer;
    function GetCount: integer;
    function GetSeg(index: integer): TSvgPathSeg;
    function AddSeg(segType: TSvgPathSegType;
      const startPt: TPointD; const pts: TPathD): TSvgPathSeg;
  protected
    procedure GrowSegs;
    procedure SegsLoaded;
    procedure InitSegs(Capacity: Integer);
  public
    isClosed  : Boolean;
    constructor Create(parent: TSvgPath);
    destructor Destroy; override;
    procedure Clear;
    procedure Offset(dx, dy: double);
    function GetFirstPt: TPointD;
    function GetLastPt: TPointD;
    function GetBounds: TRectD;

    function AddASeg(const startPt, endPt: TPointD; const rect: TRectD;
      angle: double; isClockwise: Boolean): TSvgASegment;
    function AddCSeg(const startPt: TPointD; const pts: TPathD): TSvgCSegment;
    function AddHSeg(const startPt: TPointD; const pts: TPathD): TSvgHSegment;
    function AddLSeg(const startPt: TPointD; const pts: TPathD): TSvgLSegment;
    function AddQSeg(const startPt: TPointD; const pts: TPathD): TSvgQSegment;
    function AddSSeg(const startPt: TPointD; const pts: TPathD): TSvgSSegment;
    function AddTSeg(const startPt: TPointD; const pts: TPathD): TSvgTSegment;
    function AddVSeg(const startPt: TPointD; const pts: TPathD): TSvgVSegment;
    function AddZSeg(const endPt, firstPt: TPointD): TSvgZSegment;

    function GetLastSeg: TSvgPathSeg;
    function DeleteLastSeg: Boolean;
    //pendingScale: allows 'flattening' to occur with curve precision
    //that will accommodate future (anticipated) scaling.
    //Eg: a native image is 32x32 px but will be displayed at 512x512,
    //so pendingScale should be 16 to ensure a smooth curve
    function GetFlattenedPath(pendingScale: double = 1.0): TPathD;
    //GetSimplePath - only used for markers
    function GetSimplePath: TPathD;
    function GetMoveStrDef(relative: Boolean; decimalPrec: integer): string;
    function GetStringDef(relative: Boolean; decimalPrec: integer): string;
    property Count      : integer read GetCount;
    property Parent     : TSvgPath read fParent;
    property PathOffset : TPointD read fPathOffset;
    property Seg[index: integer]: TSvgPathSeg read GetSeg; default;
  end;

  TSvgPath = class
  private
    fPathScale : double;
    fPathOffs  : TPointD;
    fSubPaths: array of TSvgSubPath;
    function GetPath(index: integer): TSvgSubPath;
    function GetBounds: TRectD;
    function GetControlBounds: TRectD;
    function GetCount: integer;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure Parse(const value: UTF8String);
    procedure ScaleAndOffset(scale: double; dx, dy: integer);
    function  GetStringDef(relative: Boolean; decimalPrec: integer): string;

    function AddPath(SegsCapacity: Integer = 0): TSvgSubPath;
    procedure DeleteSubPath(subPath: TSvgSubPath);
    property Bounds: TRectD read GetBounds;
    property CtrlBounds: TRectD read GetControlBounds;
    property Count: integer read GetCount;
    property Path[index: integer]: TSvgSubPath read GetPath; default;
    property Scale: double read fPathScale;
    property Offset : TPointD read fPathOffs;
  end;

  UTF8Strings = array of UTF8String;

  function GetSvgArcInfoRect(const p1, p2: TPointD;
    radii: TPointD; phi_rads: double; fA, fS: boolean): TRectD;

implementation

resourcestring
  rsSvgPathRangeError = 'TSvgPath.GetPath range error';
  rsSvgSubPathRangeError = 'TSvgSubPath.GetSeg range error';

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

function CheckPathLen(const p: TPathD; modLength: integer): TPathD;
var
  i, len: integer;
begin
  Result := nil;
  len := Length(p);
  if (len < modLength) then Exit;
  Result := p;
  i := len mod modLength;
  SetLength(Result, len -i);
end;
//------------------------------------------------------------------------------

function TrimTrailingZeros(const floatValStr: string): string;
var
  i: integer;
begin
  Result := floatValStr;
  if Pos('.', floatValStr) = 0 then Exit;
  i := Length(Result);
  while Result[i] = '0' do dec(i);
  if Result[i] = '.' then dec(i);
  SetLength(Result, i);
end;
//------------------------------------------------------------------------------

function AsIntStr(val: double): string;
begin
  Result := Format('%1.0n ', [val]);
end;
//------------------------------------------------------------------------------

function AsFloatStr(val: double; precision: integer): string;
begin
  Result := TrimTrailingZeros(Format('%1.*f', [precision, val]));
end;
//------------------------------------------------------------------------------

function AsCoordStr(pt: TPointD;
  const relPt: TPointD; relative: Boolean; precision: integer): string;
var
  s1, s2: string;
begin
  if relative then
  begin
    pt.X := pt.X - relPt.X;
    pt.Y := pt.Y - relPt.Y;
  end;
  s1 := TrimTrailingZeros(Format('%1.*f', [precision, pt.x]));
  s2 := TrimTrailingZeros(Format('%1.*f', [precision, pt.y]));
  Result := s1 + ',' + s2 + ' ';
end;
//------------------------------------------------------------------------------

function GetSingleDigit(var c, endC: PUTF8Char;
  out digit: integer): Boolean;
var
  cc: PUTF8Char;
  ch: UTF8Char;
begin
  cc := SkipBlanksAndComma(c, endC);
  Result := cc < endC;
  if not Result then
  begin
    c := cc;
    Exit;
  end;
  ch := cc^;
  Result := (ch >= '0') and (ch <= '9');
  if not Result then Exit;
  digit := Ord(ch) - Ord('0');
  c := cc + 1;
end;
//------------------------------------------------------------------------------

const
  SegTypeMap: array['A'..'Z'] of TSvgPathSegType = (
    stArc,       // A
      stUnknown, // B
    stCBezier,   // C
      stUnknown, // D
      stUnknown, // E
      stUnknown, // F
      stUnknown, // G
    stHorz,      // H
      stUnknown, // I
      stUnknown, // J
      stUnknown, // K
    stLine,      // L
    stMove,      // M
      stUnknown, // N
      stUnknown, // O
      stUnknown, // P
    stQBezier,   // Q
      stUnknown, // R
    stCSpline,   // S
    stQSpline,   // T
      stUnknown, // U
    stVert,      // V
      stUnknown, // W
      stUnknown, // X
      stUnknown, // Y
    stClose      // Z
  );

function GetSegType(var c, endC: PUTF8Char; out isRelative: Boolean): TSvgPathSegType;
var
  ch: UTF8Char;
begin
  Result := stUnknown;
  if not SkipBlanks(c, endC) then Exit;
  ch := c^;
  case ch of
    'a'..'z': Result := SegTypeMap[UTF8Char(Byte(ch) and not $20)];
    'A'..'Z': Result := SegTypeMap[ch];
  end;
  if Result = stUnknown then Exit;
  isRelative := ch >= 'a';
  inc(c);
end;
//------------------------------------------------------------------------------

function Parse2Num(var c, endC: PUTF8Char;
  out pt: TPointD; const relPt: TPointD): Boolean;
begin
  Result := ParseNextNum(c, endC, true, pt.X) and
    ParseNextNum(c, endC, true, pt.Y);
  if not Result or (relPt.X = InvalidD) then Exit;
  pt.X := pt.X + relPt.X;
  pt.Y := pt.Y + relPt.Y;
end;
//------------------------------------------------------------------------------

function Parse1Num(var c: PUTF8Char; endC: PUTF8Char;
  out val: double; relVal: double): Boolean;
begin
  Result := ParseNextNum(c, endC, true, val);
  if Result and (relVal <> InvalidD) then
    val := val + relVal;
end;

//------------------------------------------------------------------------------
// TSvgPathSeg
//------------------------------------------------------------------------------

constructor TSvgPathSeg.Create(parent: TSvgSubPath;
  idx: integer; const firstPt : TPointD);
begin
  Self.fParent  := parent;
  Self.fOwner   := parent.fParent;
  Self.fIdx     := idx;
  Self.fFirstPt := firstPt;
end;
//------------------------------------------------------------------------------

procedure TSvgPathSeg.Scale(value: double);
begin
  if (value <> 0) and (value <> 1) then
  begin
    fCtrlPts := ScalePath(fCtrlPts, value);
    fFirstPt := ScalePoint(fFirstPt, value);
    Changed;
  end;
end;
//------------------------------------------------------------------------------

function TSvgPathSeg.DescaleAndOffset(const pt: TPointD): TPointD;
begin
  Result := TranslatePoint(pt, -parent.PathOffset.X, -parent.PathOffset.Y);
  Result := ScalePoint(Result, 1/Owner.Scale);
end;
//------------------------------------------------------------------------------

function TSvgPathSeg.DescaleAndOffset(const p: TPathD): TPathD;
begin
  Result := TranslatePath(p, -parent.PathOffset.X, -parent.PathOffset.Y);
  Result := ScalePath(Result, 1/Owner.Scale);
end;
//------------------------------------------------------------------------------

procedure TSvgPathSeg.Offset(dx, dy: double);
begin
  fFirstPt := TranslatePoint(fFirstPt, dx, dy);
  fCtrlPts := TranslatePath(fCtrlPts, dx, dy);
  Changed;
end;
//------------------------------------------------------------------------------

procedure TSvgPathSeg.SetCtrlPts(const pts: TPathD);
begin
  fCtrlPts := pts;
  Changed;
end;
//------------------------------------------------------------------------------

function TSvgPathSeg.ExtendSeg(const pts: TPathD): Boolean;
var
  len: integer;
begin
  len := Length(pts);
  Result := (len <> 0) and (fExtend <> 0) and (len mod fExtend = 0);
  if Result then ConcatPaths(fCtrlPts, pts);
end;
//------------------------------------------------------------------------------

function TSvgPathSeg.GetCtrlBounds: TRectD;
begin
  Result := GetBoundsD(PrePendPoint(fFirstPt, CtrlPts));
end;
//------------------------------------------------------------------------------

procedure TSvgPathSeg.Changed;
begin
  if fFlatPath <> nil then
    fFlatPath := nil; // DynArrayClear
end;
//------------------------------------------------------------------------------
procedure TSvgPathSeg.RequireFlattened;
begin
  if fFlatPath = nil then
    GetFlattenedInternal;
end;

//------------------------------------------------------------------------------
function TSvgPathSeg.GetFlattened: TPathD;
begin
  RequireFlattened;
  Result := fFlatPath;
end;
//------------------------------------------------------------------------------

procedure TSvgPathSeg.GetFlattened2(var Result: TPathD);
begin // uses less DynArrayAsg and DynArrayClear calls
  RequireFlattened;
  Result := fFlatPath;
end;
//------------------------------------------------------------------------------

function TSvgPathSeg.GetOnPathCtrlPts: TPathD;
begin
  Result := fCtrlPts;
end;
//------------------------------------------------------------------------------

function TSvgPathSeg.GetStringDef(relative: Boolean; decimalPrec: integer): string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TSvgStraightSeg
//------------------------------------------------------------------------------

procedure TSvgStraightSeg.GetFlattenedInternal;
begin
  PrePendPoint(fFirstPt, fCtrlPts, fFlatPath);
end;

//------------------------------------------------------------------------------
// TSvgCurvedSeg
//------------------------------------------------------------------------------

constructor TSvgCurvedSeg.Create(parent: TSvgSubPath; idx: integer;
  const firstPt : TPointD);
begin
  inherited;
  pendingScale := 1.0;
end;
//------------------------------------------------------------------------------

procedure TSvgCurvedSeg.RequireFlattened;
begin
  //if the image has been rendered previously at a lower resolution, then
  //redo the flattening otherwise curves my look very rough.
  if (pendingScale < Parent.fPendingScale) then
  begin
    pendingScale := Parent.fPendingScale;
    Changed;
  end;
  inherited RequireFlattened;
end;
//------------------------------------------------------------------------------

function TSvgCurvedSeg.GetLastCtrlPt: TPointD;
begin
  Result := CtrlPts[High(CtrlPts) -1];
end;
//------------------------------------------------------------------------------

function TSvgCurvedSeg.GetPreviousCtrlPt: TPointD;
var
  UseParentLastCtrlPt: Boolean;
begin
  UseParentLastCtrlPt := False;
  if fIdx > 0 then
  begin
    case fSegType of
      stQSpline:
        case fParent[fIdx -1].fSegType of
          stQBezier, stQSpline: UseParentLastCtrlPt := True;
        end;
      stCSpline:
        case fParent[fIdx -1].fSegType of
          stCBezier, stCSpline: UseParentLastCtrlPt := True;
        end;
    end;
  end;

  if UseParentLastCtrlPt then
    Result := TSvgCurvedSeg(fParent[fIdx -1]).GetLastCtrlPt
  else
    Result := fFirstPt;
end;

//------------------------------------------------------------------------------
// TSvgASegment
//------------------------------------------------------------------------------

constructor TSvgASegment.Create(parent: TSvgSubPath; idx: integer;
  const firstPt : TPointD);
begin
  inherited;
  fSegType  := stArc;
  fExtend   := 0;
end;
//------------------------------------------------------------------------------

procedure TSvgASegment.SetArcInfo(ai: TArcInfo);
var
  dx, dy: double;
begin
  //make sure that all the ai fields are valid,
  //otherwise adjust them and align with ai.startpos
  with fArcInfo do
  begin
    rec       := ai.rec;
    rectAngle := ai.rectAngle;
    startPos  := GetClosestPtOnRotatedEllipse(rec, rectAngle, ai.startPos);
    endPos    := GetClosestPtOnRotatedEllipse(rec, rectAngle, ai.endPos);
    sweepClockW := ai.sweepClockW;
    if not PointsNearEqual(ai.startPos, startPos, 0.01) then
    begin
      dx := ai.startPos.X - startPos.X;
      dy := ai.startPos.Y - startPos.Y;
      TranslateRect(rec, dx, dy);
      startPos := ai.startPos;
      endPos := TranslatePoint(endPos, dx, dy);
    end;
  end;
  SetCtrlPtsFromArcInfo;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TSvgASegment.GetRectBtnPoints(out pt1, pt2, pt3: TPointD);
var
  d         : double;
  pt, sp    : TPointD;
begin
  with fArcInfo do
  begin
    //keep rec oriented to the XY axis and rotate startpos
    sp := startPos;
    pt2 := rec.MidPoint;

    if rectAngle <> 0 then
      RotatePoint(sp, pt2, -rectAngle);

    pt := PointD(rec.Left, pt2.Y);
    pt3 := PointD(rec.Right, pt2.Y);

    d := DistanceSqrd(pt, sp) - DistanceSqrd(pt3, sp);
    if not ValueAlmostZero(d, 0.01) then
      fRectLeft := d > 0;
    if fRectLeft then
      pt1 := PointD(rec.Left, pt2.Y) else
      pt1 := PointD(rec.Right, pt2.Y);

    pt := PointD(pt2.X, rec.Top);
    pt3 := PointD(pt2.X, rec.Bottom);
    d := DistanceSqrd(pt, sp) - DistanceSqrd(pt3, sp);
    if not ValueAlmostZero(d, 0.01) then fRectTop := d > 0;
    if fRectTop then
      pt3 := PointD(pt2.X, rec.Top) else
      pt3 := PointD(pt2.X, rec.Bottom);

    RotatePoint(pt1, pt2, rectAngle);
    RotatePoint(pt3, pt2, rectAngle);
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgASegment.SetCtrlPtsFromArcInfo;
begin
  NewPointDArray(fCtrlPts, 5, True);
  with fArcInfo do
  begin
    fCtrlPts[0] := startPos;
    GetRectBtnPoints(fCtrlPts[1], fCtrlPts[2], fCtrlPts[3]);
    fCtrlPts[4] := endPos;
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TSvgASegment.GetFlattenedInternal;
var
  a1,a2: double;
  p: TPathD;
begin
  fFlatPath := nil;
  with fArcInfo do
  begin
    a1 := GetStartAngle;
    a2 := GetEndAngle;
    if not sweepClockW then
    begin
      p := Arc(rec, a2, a1, pendingScale);
      p := ReversePath(p);
    end else
      p := Arc(rec, a1, a2, pendingScale);
    if rectAngle <> 0 then
      p := RotatePath(p, rec.MidPoint, rectAngle);
    ConcatPaths(fFlatPath, p);
  end;
end;
//------------------------------------------------------------------------------

function TSvgASegment.GetStartAngle: double;
begin
  with fArcInfo do
    Result := GetRotatedEllipticalAngleFromPoint(rec, rectAngle, startPos);
end;
//------------------------------------------------------------------------------

function TSvgASegment.GetEndAngle: double;
begin
  with fArcInfo do
    Result := GetRotatedEllipticalAngleFromPoint(rec, rectAngle, endPos);
end;
//------------------------------------------------------------------------------

procedure TSvgASegment.ReverseArc;
begin
  fArcInfo.sweepClockW := not fArcInfo.sweepClockW;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TSvgASegment.Offset(dx, dy: double);
begin
  inherited; // calls Changed
  with fArcInfo do
  begin
    TranslateRect(rec, dx, dy);
    startPos := TranslatePoint(startPos, dx, dy);
    endPos := TranslatePoint(endPos, dx, dy);
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgASegment.Scale(value: Double);
begin
  if (value = 0) or (value = 1) then Exit;
  inherited; // calls Changed
  with fArcInfo do
  begin
    rec := ScaleRect(rec, value);
    startPos :=  ScalePoint(startPos, value);
    endPos := ScalePoint(endPos, value);
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgASegment.SetCtrlPts(const ctrlPts: TPathD);
begin
  //SetCtrlPtsFromArcInfo; // calls Changed
end;
//------------------------------------------------------------------------------

function TSvgASegment.GetStringDef(relative: Boolean; decimalPrec: integer): string;
var
  a, a1,a2: double;
  sp, ep: TPointD;
begin
  with fArcInfo do
  begin
    if relative then Result := 'a ' else Result := 'A ';

    Result := Result +
      AsFloatStr(rec.Width *0.5 /Owner.Scale, decimalPrec) + ',';
    Result := Result +
      AsFloatStr(rec.Height *0.5 /Owner.Scale, decimalPrec) + ' ';
    //angle as degrees
    Result := Result + AsIntStr(RadToDeg(rectAngle));

    a1 := GetStartAngle;
    a2 := GetEndAngle;

    //large arce and direction flags
    a := a2 - a1;
    if a < 0 then a := a + angle360;

    if sweepClockW then
    begin
      if a  >= angle180 then
        Result := Result + '1 1 ' else
        Result := Result + '0 1 ';
    end else
    begin
      if a >= angle180 then
        Result := Result + '0 0 ' else
        Result := Result + '1 0 ';
    end;
    //descaled and de-offset end position
    ep := DescaleAndOffset(endPos);
    sp := DescaleAndOffset(startPos);
    Result := Result + AsCoordStr(ep, sp, relative, decimalPrec);
  end;
end;

//------------------------------------------------------------------------------
// TSvgCSegment
//------------------------------------------------------------------------------

constructor TSvgCSegment.Create(parent: TSvgSubPath; idx: integer;
  const firstPt : TPointD);
begin
  inherited;
  fSegType  := stCBezier;
  fExtend   := 3;
end;
//------------------------------------------------------------------------------

function TSvgCSegment.GetOnPathCtrlPts: TPathD;
var
  i, len: integer;
begin
  len := Length(fCtrlPts) div 3;
  NewPointDArray(Result, len, True);
  for i := 0 to High(Result) do
    Result[i] := fCtrlPts[i*3 +2];
end;
//------------------------------------------------------------------------------

procedure TSvgCSegment.GetFlattenedInternal;
var
  bt  : double;
  p: TPathD;
begin
  bt := BezierTolerance / pendingScale;
  p := CheckPathLen(fCtrlPts, 3);
  if p = nil then
    fFlatPath := nil else
    fFlatPath := FlattenCBezier(fFirstPt, p, bt);
end;
//------------------------------------------------------------------------------

function TSvgCSegment.GetStringDef(relative: Boolean; decimalPrec: integer): string;
var
  i: integer;
  pt, relPt: TPointD;
begin
  if relative then Result := 'c ' else Result := 'C ';
  relPt := DescaleAndOffset(fFirstPt);
  for i := 0 to High(fCtrlPts) do
  begin
    pt:= DescaleAndOffset(fCtrlPts[i]);
    Result := Result + AsCoordStr(pt, relPt, relative, decimalPrec);
    if relative and (i mod 3 = 2) then relPt := pt;
  end;
end;

//------------------------------------------------------------------------------
// TSvgHSegment
//------------------------------------------------------------------------------

constructor TSvgHSegment.Create(parent: TSvgSubPath; idx: integer;
  const firstPt : TPointD);
begin
  inherited;
  fSegType  := stHorz;
  fExtend   := 1;
end;
//------------------------------------------------------------------------------

function TSvgHSegment.GetStringDef(relative: Boolean; decimalPrec: integer): string;
var
  i: integer;
  pt, relPt: TPointD;
begin
  if relative then
  begin
    Result := 'h ';
    relPt := DescaleAndOffset(fFirstPt);
    for i := 0 to High(fCtrlPts) do
    begin
      pt := DescaleAndOffset(fCtrlPts[i]);
      Result := Result + AsFloatStr(pt.X - relPt.X, decimalPrec) + ' ';
      relPt := pt;
    end;
  end else
  begin
    Result := 'H ';
    for i := 0 to High(fCtrlPts) do
    begin
      pt := DescaleAndOffset(fCtrlPts[i]);
      Result := Result + AsFloatStr(pt.X, decimalPrec) + ' ';
    end;
  end;
end;

//------------------------------------------------------------------------------
// TSvgLSegment
//------------------------------------------------------------------------------

constructor TSvgLSegment.Create(parent: TSvgSubPath; idx: integer;
  const firstPt : TPointD);
begin
  inherited;
  fSegType  := stLine;
  fExtend   := 1;
end;
//------------------------------------------------------------------------------

function TSvgLSegment.GetStringDef(relative: Boolean; decimalPrec: integer): string;
var
  i: integer;
  pt, relPt: TPointD;
begin
  if relative then Result := 'l ' else Result := 'L ';
  relPt := DescaleAndOffset(fFirstPt);
  for i := 0 to High(fCtrlPts) do
  begin
    pt:= DescaleAndOffset(fCtrlPts[i]);
    Result := Result + AsCoordStr(pt, relPt, relative, decimalPrec);
    relPt := pt;
  end;
end;

//------------------------------------------------------------------------------
// TSvgQSegment
//------------------------------------------------------------------------------

constructor TSvgQSegment.Create(parent: TSvgSubPath; idx: integer;
  const firstPt : TPointD);
begin
  inherited;
  fSegType  := stQBezier;
  fExtend   := 2;
end;
//------------------------------------------------------------------------------

function TSvgQSegment.GetOnPathCtrlPts: TPathD;
var
  i, len: integer;
begin
  len := Length(fCtrlPts) div 2;
  NewPointDArray(Result, len, True);
  for i := 0 to High(Result) do
    Result[i] := fCtrlPts[i*2+1];
end;
//------------------------------------------------------------------------------

procedure TSvgQSegment.GetFlattenedInternal;
var
  bt  : double;
  p: TPathD;
begin
  bt := BezierTolerance / pendingScale;
  p := CheckPathLen(fCtrlPts, 2);
  if p = nil then
    fFlatPath := nil else
    fFlatPath := FlattenQBezier(fFirstPt, fCtrlPts, bt);
end;
//------------------------------------------------------------------------------

function TSvgQSegment.GetStringDef(relative: Boolean; decimalPrec: integer): string;
var
  i: integer;
  pt, relPt: TPointD;
begin
  if relative then Result := 'q ' else Result := 'Q ';
  relPt := DescaleAndOffset(fFirstPt);
  for i := 0 to High(fCtrlPts) do
  begin
    pt := DescaleAndOffset(fCtrlPts[i]);
    Result := Result + AsCoordStr(pt, relPt, relative, decimalPrec);
    if (i mod 2) = 1 then relPt := pt;
  end;
end;

//------------------------------------------------------------------------------
// TSvgSSegment
//------------------------------------------------------------------------------

constructor TSvgSSegment.Create(parent: TSvgSubPath; idx: integer;
  const firstPt : TPointD);
begin
  inherited;
  fSegType  := stCSpline;
  fExtend   := 2;
end;
//------------------------------------------------------------------------------

procedure TSvgSSegment.GetFlattenedInternal;
var
  bt  : double;
  p: TPathD;
begin
  bt := BezierTolerance / pendingScale;
  p := CheckPathLen(fCtrlPts, 2);
  if p = nil then
    fFlatPath := nil else
    fFlatPath := FlattenCSpline(GetPreviousCtrlPt, fFirstPt, fCtrlPts, bt);
end;
//------------------------------------------------------------------------------

function TSvgSSegment.GetOnPathCtrlPts: TPathD;
var
  i, len: integer;
begin
  len := Length(fCtrlPts) div 2;
  NewPointDArray(Result, len, True);
  for i := 0 to High(Result) do
    Result[i] := fCtrlPts[i*2+1];
end;
//------------------------------------------------------------------------------

function TSvgSSegment.GetStringDef(relative: Boolean; decimalPrec: integer): string;
var
  i: integer;
  pt, relPt: TPointD;
begin
  if relative then Result := 's ' else Result := 'S ';
  relPt := DescaleAndOffset(fFirstPt);
  for i := 0 to High(fCtrlPts) do
  begin
    pt := DescaleAndOffset(fCtrlPts[i]);
    Result := Result + AsCoordStr(pt, relPt, relative, decimalPrec);
    if relative and (i mod 2 = 1) then relPt := pt;
  end;
end;

//------------------------------------------------------------------------------
// TSvgTSegment
//------------------------------------------------------------------------------

constructor TSvgTSegment.Create(parent: TSvgSubPath; idx: integer;
  const firstPt : TPointD);
begin
  inherited;
  fSegType  := stQSpline;
  fExtend   := 1;
end;
//------------------------------------------------------------------------------

procedure TSvgTSegment.GetFlattenedInternal;
var
  bt: double;
begin
  bt := BezierTolerance / pendingScale;
  if fCtrlPts = nil then
    fFlatPath := nil else
    fFlatPath := FlattenQSpline(GetPreviousCtrlPt, fFirstPt, fCtrlPts, bt);
end;
//------------------------------------------------------------------------------

function TSvgTSegment.GetLastCtrlPt: TPointD;
var
  i: integer;
begin
  Result := ReflectPoint(GetPreviousCtrlPt, fFirstPt);
  for i := 0 to High(CtrlPts) -1 do
    Result := ReflectPoint(Result, CtrlPts[i]);
end;
//------------------------------------------------------------------------------


function  TSvgTSegment.GetStringDef(relative: Boolean; decimalPrec: integer): string;
var
  i: integer;
  pt, relPt: TPointD;
begin
  if relative then Result := 't ' else Result := 'T ';
  relPt := DescaleAndOffset(fFirstPt);
  for i := 0 to High(fCtrlPts) do
  begin
    pt := DescaleAndOffset(fCtrlPts[i]);
    Result := Result + AsCoordStr(pt, relPt, relative, decimalPrec);
    if relative then relPt := pt;
  end;
end;

//------------------------------------------------------------------------------
// TSvgVSegment
//------------------------------------------------------------------------------

constructor TSvgVSegment.Create(parent: TSvgSubPath; idx: integer;
  const firstPt : TPointD);
begin
  inherited;
  fSegType  := stVert;
  fExtend   := 1;
end;
//------------------------------------------------------------------------------

function TSvgVSegment.GetStringDef(relative: Boolean; decimalPrec: integer): string;
var
  i: integer;
  pt, relPt: TPointD;
begin
  if relative then
  begin
    Result := 'v ';
    relPt := DescaleAndOffset(fFirstPt);
    for i := 0 to High(fCtrlPts) do
    begin
      pt := DescaleAndOffset(fCtrlPts[i]);
      Result := Result + AsFloatStr(pt.Y - relPt.Y, decimalPrec) + ' ';
      relPt := pt;
    end;
  end else
  begin
    Result := 'V ';
    for i := 0 to High(fCtrlPts) do
    begin
      pt := DescaleAndOffset(fCtrlPts[i]);
      Result := Result + AsFloatStr(pt.Y, decimalPrec) + ' ';
    end;
  end;
end;

//------------------------------------------------------------------------------
// TSvgZSegment
//------------------------------------------------------------------------------

constructor TSvgZSegment.Create(parent: TSvgSubPath;
  idx: integer; const firstPt : TPointD);
begin
  inherited;
  fSegType  := stClose;
  fExtend   := 0;
end;
//------------------------------------------------------------------------------

function TSvgZSegment.GetStringDef(relative: Boolean;
  decimalPrec: integer): string;
begin
  Result := 'Z ';
end;

//------------------------------------------------------------------------------
// TSvgSubPath
//------------------------------------------------------------------------------

function TSvgSubPath.GetFlattenedPath(pendingScale: double): TPathD;
var
  i: integer;
  flattenedPaths: TPathsD;
begin
  if pendingScale <= 0 then pendingScale := 1.0;
  if (pendingScale > fPendingScale) then
    fPendingScale := pendingScale;

  Result := nil;
  SetLength(flattenedPaths, fSegsCount);
  for i := 0 to fSegsCount - 1 do
    fSegs[i].GetFlattened2(flattenedPaths[i]);
  ConcatPaths(Result, flattenedPaths);
end;
//------------------------------------------------------------------------------

function TSvgSubPath.AddSeg(segType: TSvgPathSegType;
  const startPt: TPointD; const pts: TPathD): TSvgPathSeg;
var
  i: integer;
begin
  i := fSegsCount;
  if i = Length(fSegs) then
    GrowSegs;
  inc(fSegsCount);

  case segType of
    stCBezier    : Result := TSvgCSegment.Create(self, i, startPt);
    stHorz    : Result := TSvgHSegment.Create(self, i, startPt);
    stLine    : Result := TSvgLSegment.Create(self, i, startPt);
    stQBezier    : Result := TSvgQSegment.Create(self, i, startPt);
    stCSpline : Result := TSvgSSegment.Create(self, i, startPt);
    stQSpline : Result := TSvgTSegment.Create(self, i, startPt);
    stVert    : Result := TSvgVSegment.Create(self, i, startPt);
    else raise Exception.Create('TSvgSubPath.AddSeg error');
  end;
  fSegs[i] := Result;
  Result.fCtrlPts := pts;
  Result.fFlatPath := nil;
  if Result is TSvgCurvedSeg then
    TSvgCurvedSeg(Result).pendingScale := fPendingScale;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.AddASeg(const startPt, endPt: TPointD; const rect: TRectD;
  angle: double; isClockwise: Boolean): TSvgASegment;
var
  i: integer;
begin
  i := fSegsCount;
  if i = Length(fSegs) then
    GrowSegs;
  inc(fSegsCount);

  Result := TSvgASegment.Create(self, i, startPt);
  fSegs[i] := Result;
  Result.pendingScale := self.fPendingScale;
  with Result.fArcInfo do
  begin
    rec := rect;
    startPos := startPt;
    endPos   := endPt;
    rectAngle := angle;
    sweepClockW := isClockwise;
  end;
  Result.SetCtrlPtsFromArcInfo; // calls Changed
end;
//------------------------------------------------------------------------------

function TSvgSubPath.AddHSeg(const startPt: TPointD; const pts: TPathD): TSvgHSegment;
begin
  Result := AddSeg(stHorz, startPt, pts) as TSvgHSegment;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.AddCSeg(const startPt: TPointD; const pts: TPathD): TSvgCSegment;
begin
  Result := AddSeg(stCBezier, startPt, pts) as TSvgCSegment;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.AddLSeg(const startPt: TPointD; const pts: TPathD): TSvgLSegment;
begin
  Result := AddSeg(stLine, startPt, pts) as TSvgLSegment;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.AddQSeg(const startPt: TPointD; const pts: TPathD): TSvgQSegment;
begin
  Result := AddSeg(stQBezier, startPt, pts) as TSvgQSegment;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.AddSSeg(const startPt: TPointD; const pts: TPathD): TSvgSSegment;
begin
  Result := AddSeg(stCSpline, startPt, pts) as TSvgSSegment;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.AddTSeg(const startPt: TPointD; const pts: TPathD): TSvgTSegment;
begin
  Result := AddSeg(stQSpline, startPt, pts) as TSvgTSegment;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.AddVSeg(const startPt: TPointD; const pts: TPathD): TSvgVSegment;
begin
  Result := AddSeg(stVert, startPt, pts) as TSvgVSegment;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.AddZSeg(const endPt, firstPt: TPointD): TSvgZSegment;
var
  i: integer;
begin
  i := fSegsCount;
  if i = Length(fSegs) then
    GrowSegs;
  inc(fSegsCount);

  Result := TSvgZSegment.Create(self, i, endPt);
  fSegs[i] := Result;
  NewPointDArray(Result.fCtrlPts, 1, True);
  Result.fCtrlPts[0] := firstPt;
  isClosed := true;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetLastSeg: TSvgPathSeg;
var
  cnt: integer;
begin
  cnt := Count;
  if cnt = 0 then
    Result := nil else
    Result := seg[cnt -1];
end;
//------------------------------------------------------------------------------

function TSvgSubPath.DeleteLastSeg: Boolean;
var
  cnt: integer;
begin
  cnt := Count;
  Result := cnt > 0;
  if not Result then Exit;
  seg[cnt -1].Free;
  SetLength(fSegs, cnt -1);
  fSegsCount := cnt - 1;
  if isClosed then isClosed := false;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetSimplePath: TPathD;
var
  i: integer;
  paths: TPathsD;
begin
  if fSegsCount <= 1 then
  begin
    Result := Img32.Vector.MakePath(GetFirstPt);
    for i := 0 to fSegsCount - 1 do
      ConcatPaths(Result, fSegs[i].GetOnPathCtrlPts);
  end
  else
  begin
    SetLength(paths, 1 + fSegsCount);
    paths[0] := Img32.Vector.MakePath(GetFirstPt);
    for i := 0 to fSegsCount - 1 do
      paths[1 + i] := fSegs[i].GetOnPathCtrlPts;
    ConcatPaths(Result, paths);
  end;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetMoveStrDef(relative: Boolean; decimalPrec: integer): string;
var
  pt: TPointD;
begin
  Result := '';
  if fSegsCount = 0 then Exit;

  if decimalPrec < -3 then decimalPrec := -3
  else if decimalPrec > 4 then decimalPrec := 4;

  with fParent do
  begin
    pt.X := (fSegs[0].fFirstPt.X - self.PathOffset.X - Offset.X)/fPathScale;
    pt.Y := (fSegs[0].fFirstPt.Y - self.PathOffset.Y - Offset.Y)/fPathScale;
  end;
  Result := 'M ' + AsCoordStr(pt, NullPointD, false, decimalPrec);
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetStringDef(relative: Boolean; decimalPrec: integer): string;
var
  i: integer;
begin
  if decimalPrec < -3 then decimalPrec := -3
  else if decimalPrec > 4 then decimalPrec := 4;
  if Count = 0 then Exit;

  Result := GetMoveStrDef(relative, decimalPrec);
  for i := 0 to Count -1 do
    Result := Result + fSegs[i].GetStringDef(relative, decimalPrec);
end;
//------------------------------------------------------------------------------

constructor TSvgSubPath.Create(parent: TSvgPath);
begin
  fParent := parent;
end;
//------------------------------------------------------------------------------

destructor TSvgSubPath.Destroy;
begin
  Clear;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgSubPath.Clear;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    fSegs[i].Free;
  fSegs := nil;
  fSegsCount := 0;
  fPathOffset := NullPointD;
end;
//------------------------------------------------------------------------------

procedure TSvgSubPath.GrowSegs;
begin
  SetLength(fSegs, (fSegsCount * 2) + 1);
end;
//------------------------------------------------------------------------------

procedure TSvgSubPath.SegsLoaded;
begin
  // Trim the array to the actual used size
  if Length(fSegs) <> fSegsCount then
    SetLength(fSegs, fSegsCount);
end;
//------------------------------------------------------------------------------

procedure TSvgSubPath.InitSegs(Capacity: Integer);
begin
  if Capacity > fSegsCount then
    SetLength(fSegs, Capacity);
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetCount: integer;
begin
  Result := fSegsCount;
end;
//------------------------------------------------------------------------------

procedure TSvgSubPath.Offset(dx, dy: double);
var
  i: integer;
begin
  for i := 0 to fSegsCount - 1 do fSegs[i].Offset(dx, dy);
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetSeg(index: integer): TSvgPathSeg;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsSvgSubPathRangeError);
  Result := fSegs[index];
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetFirstPt: TPointD;
begin
  if Count = 0 then Result := NullPointD
  else Result := fSegs[0].FirstPt;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetLastPt: TPointD;
begin
  if Count = 0 then
    Result := NullPointD
  else with fSegs[Count -1] do
    Result := CtrlPts[High(CtrlPts)];
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetBounds: TRectD;
var
  i: integer;
  p: TPathD;
begin
  p := nil;
  for i := 0 to Count -1 do
    ConcatPaths(p, fSegs[i].fFlatPath);
  Result := Img32.Vector.GetBoundsD(p);
end;

//------------------------------------------------------------------------------
// TSvgPath
//------------------------------------------------------------------------------

destructor TSvgPath.Destroy;
begin
  Clear;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgPath.ScaleAndOffset(scale: double; dx, dy: integer);
var
  i,j: integer;
begin
  if fPathScale = 0 then fPathScale := 1;
  if scale = 0 then scale := 1;
  fPathScale := fPathScale * scale;

  fPathOffs := PointD(dx, dy);
  for i := 0 to Count -1 do
    with fSubPaths[i] do
    begin
      if scale <> 1 then
      for j := 0 to fSegsCount - 1 do
        fSegs[j].Scale(scale);
      Offset(dx,dy);
    end;
end;
//------------------------------------------------------------------------------

function TSvgPath.GetStringDef(relative: Boolean; decimalPrec: integer): string;
var
  i : integer;
begin
  result := '';
  if fPathScale = 0 then  fPathScale := 1;
  for i := 0 to High(fSubPaths) do
    Result := Result + fSubPaths[i].GetStringDef(relative, decimalPrec);
end;
//------------------------------------------------------------------------------

procedure TSvgPath.Parse(const value: UTF8String);
var
  c, endC     : PUTF8Char;
  firstPt     : TPointD;
  lastPt      : TPointD;
  currPt      : TPointD;
  pt2, pt3    : TPointD;
  angle       : double;
  sweepCW     : integer;
  largeArc    : integer;
  arcRec      : TRectD;
  isRelative  : Boolean;
  currSegType : TSvgPathSegType;
  currSubPath : TSvgSubPath;
  pts         : TPathD;
  ptCap       : integer;
  ptCnt       : integer;

  procedure AddPt(const pt: TPointD);
  begin
    if ptCnt = ptCap then
    begin
      inc(ptCap, 8);
      SetLengthUninit(pts, ptCap);
    end;
    pts[ptCnt] := pt;
    inc(ptCnt);
  end;

  procedure AllocEstimatedPtsCount(c, endC: PUTF8Char);
  begin
    // Count the numbers before the next segment type char
    ptCap := 0;
    while c < endC do
    begin
      // skip whitespaces
      while (c < endC) and (c^ <= space) do
        inc(c);

      if c >= endC then
        break;

      case c^ of
        '0'..'9', '-', '.', 'E', 'e':
          begin
            while (c < endC) and (c^ > space) do
              inc(c);
            Inc(ptCap);
          end;
      else
        Break;
      end;
    end;
    ptCap := ptCap div 2; // two numbers are one point
    SetLength(pts, ptCap);
  end;

  function EstimateSegs(c, endC: PUTF8Char): Integer;
  var
    ch: UTF8Char;
  begin
    Result := 0;
    while True do
    begin
      if c >= endC then
        Break;
      ch := c^;
      inc(c);

      case ch of
        'A'..'Z', 'a'..'z':
          begin
            case ch of
              'M', 'm': // move / close
                Break;
              'Z', 'z':
                begin
                  Inc(Result);
                  Break;
                end;
              'E', 'e': ; // Exponent of a number
            else
              Inc(Result);
            end;
          end;
      end;
    end;
  end;

var
  ExpectedSegCount: Integer;
begin
  Clear;
  currSubPath := nil;
  ExpectedSegCount := 1;

  c := PUTF8Char(value);
  endC := c + Length(value);
  isRelative := false;
  currPt := NullPointD;

  while true do
  begin
    currSegType := GetSegType(c, endC, isRelative);
    if currSegType = stUnknown then Break;

    if currSegType = stMove then
    begin
      if currSubPath <> nil then
        currSubPath.SegsLoaded; // Trim the segs array to the actual count
      currSubPath := nil;

      ExpectedSegCount := EstimateSegs(c, endc);

      if isRelative then
        lastPt := currPt else
        lastPt := InvalidPointD;

      if not Parse2Num(c, endC, currPt, lastPt) then break;
      lastPt :=  currPt;
      //values immediately following a Move are implicitly Line statements
      if IsNumPending(c, endC, true) then
        currSegType := stLine else
        Continue;
      Inc(ExpectedSegCount);
    end
    else if (currSegType = stClose) then
    begin
      if currPt.X = InvalidD then Continue;

      if Assigned(currSubPath) and (currSubPath.Count > 0) then
      begin
        lastPt := currPt;
        currPt := currSubPath.GetFirstPt;
        currSubPath.AddZSeg(lastPt, currPt);
      end else
      begin
        if not Assigned(currSubPath) then
          currSubPath := AddPath(1);
        currSubPath.AddZSeg(currPt, currPt);
      end;
      currSubPath.SegsLoaded; // Trim the segs array to the actual count
      currSubPath := nil;
      ExpectedSegCount := 1;
      Continue;
    end;

    if not Assigned(currSubPath) then
      currSubPath := AddPath(ExpectedSegCount);

    pts := nil;
    ptCnt := 0; ptCap := 0;
    firstPt := currPt;
    if isRelative then
      lastPt := firstPt else
      lastPt := InvalidPointD;

    case currSegType of
      stArc:
        begin
          //nb: unlike other segment types,
          //consecutive arc segs are separated.
          while IsNumPending(c, endC, true) and
            Parse2Num(c, endC, pt2, InvalidPointD) and
            ParseNextNum(c, endC, true, angle) and
            GetSingleDigit(c, endC, largeArc) and
            GetSingleDigit(c, endC, sweepCW) and
            Parse2Num(c, endC, currPt, lastPt) do
          begin
            angle := DegToRad(angle);
            arcRec := GetSvgArcInfoRect(firstPt, currPt, pt2,
              angle, largeArc <> 0, sweepCW <> 0);
            if arcRec.IsEmpty then break;

            currSubPath.AddASeg(firstPt, currPt,
              arcRec, angle, sweepCW <> 0);

            if isRelative then lastPt := currPt;
            firstPt := currPt;
          end;
        end;
      stCBezier:
        begin
          AllocEstimatedPtsCount(c, endC);
          while IsNumPending(c, endC, true) and
            Parse2Num(c, endC, pt2, lastPt) and
            Parse2Num(c, endC, pt3, lastPt) and
            Parse2Num(c, endC, currPt, lastPt) do
          begin
            AddPt(pt2);
            AddPt(pt3);
            AddPt(currPt);
            if isRelative then lastPt := currPt;
          end;
          if Length(pts) <> ptCnt then
            SetLength(pts, ptCnt);
          currSubPath.AddSeg(stCBezier, firstPt, pts);
        end;
      stHorz:
        begin
          AllocEstimatedPtsCount(c, endC);
          while IsNumPending(c, endC, true) and
            Parse1Num(c, endC, currPt.X, lastPt.X) do
          begin
            AddPt(currPt);
            if isRelative then lastPt.X := currPt.X;
          end;
          if Length(pts) <> ptCnt then
            SetLength(pts, ptCnt);
          currSubPath.AddHSeg(firstPt, pts);
        end;
      stQBezier, stCSpline:
        begin
          AllocEstimatedPtsCount(c, endC);
          while IsNumPending(c, endC, true) and
            Parse2Num(c, endC, pt2, lastPt) and
            Parse2Num(c, endC, currPt, lastPt) do
          begin
            AddPt(pt2);
            AddPt(currPt);
            if isRelative then lastPt := currPt;
          end;
          if Length(pts) <> ptCnt then
            SetLength(pts, ptCnt);
          currSubPath.AddSeg(currSegType, firstPt, pts);
        end;
      stLine, stQSpline:
        begin
          AllocEstimatedPtsCount(c, endC);
          while IsNumPending(c, endC, true) and
            Parse2Num(c, endC, currPt, lastPt) do
          begin
            AddPt(currPt);
            if isRelative then lastPt := currPt;
          end;
          if Length(pts) <> ptCnt then
            SetLength(pts, ptCnt);
          currSubPath.AddSeg(currSegType, firstPt, pts);
        end;
      stVert:
        begin
          AllocEstimatedPtsCount(c, endC);
          while IsNumPending(c, endC, true) and
            Parse1Num(c, endC, currPt.Y, lastPt.Y) do
          begin
            AddPt(currPt);
            if isRelative then lastPt.Y := currPt.Y;
          end;
          if Length(pts) <> ptCnt then
            SetLength(pts, ptCnt);
          currSubPath.AddVSeg(firstPt, pts);
        end;
    end;
  end;
  if currSubPath <> nil then
    currSubPath.SegsLoaded; // Trim the segs array to the actual count
end;
//------------------------------------------------------------------------------

function TSvgPath.GetCount: integer;
begin
  Result := Length(fSubPaths);
end;
//------------------------------------------------------------------------------

function TSvgPath.GetPath(index: integer): TSvgSubPath;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsSvgPathRangeError);
  Result := fSubPaths[index];
end;
//------------------------------------------------------------------------------

procedure TSvgPath.Clear;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    fSubPaths[i].Free;
  fSubPaths := nil;
  fPathScale := 1;
end;
//------------------------------------------------------------------------------

function TSvgPath.GetBounds: TRectD;
var
  i: integer;
  p: TPathD;
begin
  p := nil;
  for i := 0 to Count -1 do
      ConcatPaths(p, fSubPaths[i].GetFlattenedPath);
  Result := Img32.Vector.GetBoundsD(p);
end;
//------------------------------------------------------------------------------

function TSvgPath.GetControlBounds: TRectD;
var
  i,j: integer;
  p: TPathD;
begin
  p := nil;
  for i := 0 to Count -1 do
    with fSubPaths[i] do
    begin
      AppendPoint(p, GetFirstPt);
      for j := 0 to fSegsCount - 1 do
        ConcatPaths(p, fSegs[j].fCtrlPts);
    end;
  Result := GetBoundsD(p);

  //watch out for straight horizontal or vertical lines
  if IsEmptyRect(Result) then
  begin
    if Result.Width = 0 then
    begin
      Result.Left := Result.Left - 0.5;
      Result.Right := Result.Left + 1.0;
    end
    else if Result.Height = 0 then
    begin
      Result.Top := Result.Top - 0.5;
      Result.Bottom := Result.Top + 1.0;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TSvgPath.AddPath(SegsCapacity: Integer): TSvgSubPath;
var
  i: integer;
begin
  i := Count;
  Result := TSvgSubPath.Create(self);
  Result.InitSegs(SegsCapacity);
  SetLength(fSubPaths, i + 1);
  fSubPaths[i] := Result;
end;
//------------------------------------------------------------------------------

procedure TSvgPath.DeleteSubPath(subPath: TSvgSubPath);
var
  i, len: integer;
begin
  len := Length(fSubPaths);
  for i := 0 to len -1 do
    if subPath = fSubPaths[i] then
    begin
      fSubPaths[i].Free;
      if i < len -1 then
        Move(fSubPaths[i+1], fSubPaths[i],
          (len - i -1) * SizeOf(Pointer));
      SetLength(fSubPaths, len -1);
      break;
    end;
end;

//------------------------------------------------------------------------------
// GetSvgArcInfoRect
//------------------------------------------------------------------------------

//https://stackoverflow.com/a/12329083
function GetSvgArcInfoRect(const p1, p2: TPointD; radii: TPointD;
  phi_rads: double; fA, fS: boolean): TRectD;
var
  x1_, y1_, rxry, rxy1_, ryx1_, s_phi, c_phi: double;
  hd_x, hd_y, hs_x, hs_y, sum_of_sq, lambda, coe: double;
  cx, cy, cx_, cy_: double;
begin
    Result := NullRectD;
    if (radii.X < 0) then radii.X := -radii.X;
    if (radii.Y < 0) then radii.Y := -radii.Y;
    if (radii.X = 0) or (radii.Y = 0) then Exit;

    GetSinCos(phi_rads, s_phi, c_phi);;
    hd_x := (p1.X - p2.X) / 2.0; // half diff of x
    hd_y := (p1.Y - p2.Y) / 2.0; // half diff of y
    hs_x := (p1.X + p2.X) / 2.0; // half sum of x
    hs_y := (p1.Y + p2.Y) / 2.0; // half sum of y

    // F6.5.1
    x1_ := c_phi * hd_x + s_phi * hd_y;
    y1_ := c_phi * hd_y - s_phi * hd_x;

    // F.6.6 Correction of out-of-range radii
    // Step 3: Ensure radii are large enough
    lambda := (x1_ * x1_) / (radii.X * radii.X) +
      (y1_ * y1_) / (radii.Y * radii.Y);
    if (lambda > 1) then
    begin
      radii.X := radii.X * Sqrt(lambda);
      radii.Y := radii.Y * Sqrt(lambda);
    end;

    rxry := radii.X * radii.Y;
    rxy1_ := radii.X * y1_;
    ryx1_ := radii.Y * x1_;
    sum_of_sq := rxy1_ * rxy1_ + ryx1_ * ryx1_; // sum of square
    if (sum_of_sq = 0) then Exit;

    coe := Sqrt(Abs((rxry * rxry - sum_of_sq) / sum_of_sq));
    if (fA = fS) then coe := -coe;

    // F6.5.2
    cx_ := coe * rxy1_ / radii.Y;
    cy_ := -coe * ryx1_ / radii.X;

    // F6.5.3
    cx := c_phi * cx_ - s_phi * cy_ + hs_x;
    cy := s_phi * cx_ + c_phi * cy_ + hs_y;

    Result.Left := cx - radii.X;
    Result.Right := cx + radii.X;
    Result.Top := cy - radii.Y;
    Result.Bottom := cy + radii.Y;
end;
//------------------------------------------------------------------------------

end.
