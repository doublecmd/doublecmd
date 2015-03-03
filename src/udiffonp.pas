unit uDiffONP;

(*******************************************************************************
* Component         TDiff                                                      *
* Version:          4.1                                                        *
* Date:             7 November 2009                                            *
* Compilers:        Delphi 7 - Delphi 2009                                     *
* Author:           Angus Johnson - angusj-AT-myrealbox-DOT-com                *
* Copyright:        © 2001-2009 Angus Johnson                                  *
*                                                                              *
* Licence to use, terms and conditions:                                        *
*                   The code in the TDiff component is released as freeware    *
*                   provided you agree to the following terms & conditions:    *
*                   1. the copyright notice, terms and conditions are          *
*                   left unchanged                                             *
*                   2. modifications to the code by other authors must be      *
*                   clearly documented and accompanied by the modifier's name. *
*                   3. the TDiff component may be freely compiled into binary  *
*                   format and no acknowledgement is required. However, a      *
*                   discrete acknowledgement would be appreciated (eg. in a    *
*                   program's 'About Box').                                    *
*                                                                              *
* Description:      Component to list differences between two integer arrays   *
*                   using a "longest common subsequence" algorithm.            *
*                   Typically, this component is used to diff 2 text files     *
*                   once their individuals lines have been hashed.             *
*                                                                              *
* Acknowledgements: The key algorithm in this component is based on:           *
*                   "An O(NP) Sequence Comparison Algorithm"                   *
*                   by Sun Wu, Udi Manber & Gene Myers                         *
*                   and uses a "divide-and-conquer" technique to avoid         *
*                   using exponential amounts of memory as described in        *
*                   "An O(ND) Difference Algorithm and its Variations"         *
*                   By E Myers - Algorithmica Vol. 1 No. 2, 1986, pp. 251-266  *
*******************************************************************************)

(*******************************************************************************
* History:                                                                     *
* 13 December 2001 - Original release (used Myer's O(ND) Difference Algorithm) *
* 22 April 2008    - Complete rewrite to greatly improve the code and          *
*                    provide a much simpler view of differences through a new  *
*                    'Compares' property.                                      *
* 21 May 2008      - Another complete code rewrite to use Sun Wu et al.'s      *
*                    O(NP) Sequence Comparison Algorithm which more than       *
*                    halves times of typical comparisons.                      *
* 24 May 2008      - Reimplemented "divide-and-conquer" technique (which was   *
*                    omitted in 21 May release) so memory use is again minimal.*
* 25 May 2008      - Removed recursion to avoid the possibility of running out *
*                    of stack memory during massive comparisons.               *
* 2 June 2008      - Bugfix: incorrect number of appended AddChangeInt() calls *
*                    in Execute() for integer arrays. (It was OK with Chars)   *
*                    Added check to prevent repeat calls to Execute() while    *
*                    already executing.                                        *
*                    Added extra parse of differences to find occasional       *
*                    missed matches. (See readme.txt for further discussion)   *
* 7 November 2009  - Updated so now compiles in newer versions of Delphi.      *
*******************************************************************************)

{$mode delphi}

interface

uses
  SysUtils, Classes, Math, Forms, Dialogs;

const
  MAX_DIAGONAL = $FFFFFF; //~16 million

type

{$IFDEF UNICODE}
  P8Bits = PByte;
{$ELSE}
  P8Bits = PAnsiChar;
{$ENDIF}

  PDiags = ^TDiags;
  TDiags = array [-MAX_DIAGONAL .. MAX_DIAGONAL] of integer;

  PIntArray = ^TIntArray;
  TIntArray = array[0 .. MAXINT div sizeof(integer) -1] of Integer;
  PChrArray = ^TChrArray;
  TChrArray = array[0 .. MAXINT div sizeof(char) -1] of Char;

  TChangeKind = (ckNone, ckAdd, ckDelete, ckModify);

  PCompareRec = ^TCompareRec;
  TCompareRec = record
    Kind      : TChangeKind;
    oldIndex1,
    oldIndex2 : integer;
    case boolean of
      false   : (chr1, chr2 : Char);
      true    : (int1, int2 : integer);
  end;

  PDiffVars = ^TDiffVars;
  TDiffVars = record
    offset1 : integer;
    offset2 : integer;
    len1    : integer;
    len2    : integer;
  end;

  TDiffStats = record
    matches  : integer;
    adds     : integer;
    deletes  : integer;
    modifies : integer;
  end;

  TDiff = class(TComponent)
  private
    fCompareList: TList;
    fDiffList: TList;      //this TList circumvents the need for recursion
    fCancelled: boolean;
    fExecuting: boolean;
    fCompareInts: boolean; //ie are we comparing integer arrays or char arrays
    DiagBufferF: pointer;
    DiagBufferB: pointer;
    DiagF, DiagB: PDiags;
    Ints1, Ints2: PIntArray;
    Chrs1, Chrs2: PChrArray;
    fDiffStats: TDiffStats;
    fLastCompareRec: TCompareRec;
    procedure PushDiff(offset1, offset2, len1, len2: integer);
    function  PopDiff: boolean;
    procedure InitDiagArrays(len1, len2: integer);
    procedure DiffInt(offset1, offset2, len1, len2: integer);
    procedure DiffChr(offset1, offset2, len1, len2: integer);
    function SnakeChrF(k,offset1,offset2,len1,len2: integer): boolean;
    function SnakeChrB(k,offset1,offset2,len1,len2: integer): boolean;
    function SnakeIntF(k,offset1,offset2,len1,len2: integer): boolean;
    function SnakeIntB(k,offset1,offset2,len1,len2: integer): boolean;
    procedure AddChangeChr(offset1, range: integer; ChangeKind: TChangeKind);
    procedure AddChangeInt(offset1, range: integer; ChangeKind: TChangeKind);
    function GetCompareCount: integer;
    function GetCompare(index: integer): TCompareRec;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    //compare either and array of characters or an array of integers ...
    function Execute(pints1, pints2: PInteger; len1, len2: integer): boolean; overload;
    function Execute(pchrs1, pchrs2: PChar; len1, len2: integer): boolean; overload;
    //Cancel allows interrupting excessively prolonged comparisons
    procedure Cancel;
    procedure Clear;
    property Cancelled: boolean read fCancelled;
    property Count: integer read GetCompareCount;
    property Compares[index: integer]: TCompareRec read GetCompare; default;
    property DiffStats: TDiffStats read fDiffStats;
  end;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TDiff]);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

constructor TDiff.Create(aOwner: TComponent);
begin
  inherited;
  fCompareList := TList.create;
  fDiffList := TList.Create;
end;
//------------------------------------------------------------------------------

destructor TDiff.Destroy;
begin
  Clear;
  fCompareList.free;
  fDiffList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TDiff.Execute(pints1, pints2: PInteger; len1, len2: integer): boolean;
var
  i, Len1Minus1: integer;
begin
  result := not fExecuting;
  if not result then exit;
  fCancelled := false;
  fExecuting := true;
  try
    Clear;

    Len1Minus1 := len1 -1;
    fCompareList.Capacity := len1 + len2;
    fCompareInts := true;

    GetMem(DiagBufferF, sizeof(integer)*(len1+len2+3));
    GetMem(DiagBufferB, sizeof(integer)*(len1+len2+3));
    Ints1 := pointer(pints1);
    Ints2 := pointer(pints2);
    try
      PushDiff(0, 0, len1, len2);
      while PopDiff do;
    finally
      freeMem(DiagBufferF);
      freeMem(DiagBufferB);
    end;

    if fCancelled then
    begin
      result := false;
      Clear;
      exit;
    end;

    //correct the occasional missed match ...
    for i := 1 to count -1 do
      with PCompareRec(fCompareList[i])^ do
        if (Kind = ckModify) and (int1 = int2) then
        begin
          Kind := ckNone;
          Dec(fDiffStats.modifies);
          Inc(fDiffStats.matches);
        end;
        
    //finally, append any trailing matches onto compareList ...
    with fLastCompareRec do
      AddChangeInt(oldIndex1,len1Minus1-oldIndex1, ckNone);
  finally
    fExecuting := false;
  end;
end;
//------------------------------------------------------------------------------

function TDiff.Execute(pchrs1, pchrs2: PChar; len1, len2: integer): boolean;
var
  i, Len1Minus1: integer;
begin
  result := not fExecuting;
  if not result then exit;
  fCancelled := false;
  fExecuting := true;
  try
    Clear;

    Len1Minus1 := len1 -1;
    fCompareList.Capacity := len1 + len2;
    fDiffList.Capacity := 1024;
    fCompareInts := false;

    GetMem(DiagBufferF, sizeof(integer)*(len1+len2+3));
    GetMem(DiagBufferB, sizeof(integer)*(len1+len2+3));
    Chrs1 := pointer(pchrs1);
    Chrs2 := pointer(pchrs2);
    try
      PushDiff(0, 0, len1, len2);
      while PopDiff do;
    finally
      freeMem(DiagBufferF);
      freeMem(DiagBufferB);
    end;

    if fCancelled then
    begin
      result := false;
      Clear;
      exit;
    end;

    //correct the occasional missed match ...
    for i := 1 to count -1 do
      with PCompareRec(fCompareList[i])^ do
        if (Kind = ckModify) and (chr1 = chr2) then
        begin
          Kind := ckNone;
          Dec(fDiffStats.modifies);
          Inc(fDiffStats.matches);
        end;

    //finally, append any trailing matches onto compareList ...
    with fLastCompareRec do
      AddChangeChr(oldIndex1,len1Minus1-oldIndex1, ckNone);
  finally
    fExecuting := false;
  end;
end;
//------------------------------------------------------------------------------

procedure TDiff.PushDiff(offset1, offset2, len1, len2: integer);
var
  DiffVars: PDiffVars;
begin
  new(DiffVars);
  DiffVars.offset1 := offset1;
  DiffVars.offset2 := offset2;
  DiffVars.len1 := len1;
  DiffVars.len2 := len2;
  fDiffList.Add(DiffVars);
end;
//------------------------------------------------------------------------------

function  TDiff.PopDiff: boolean;
var
  DiffVars: PDiffVars;
  idx: integer;
begin
  idx := fDiffList.Count -1;
  result := idx >= 0;
  if not result then exit;
  DiffVars := PDiffVars(fDiffList[idx]);
  with DiffVars^ do
    if fCompareInts then
      DiffInt(offset1, offset2, len1, len2) else
      DiffChr(offset1, offset2, len1, len2);
  Dispose(DiffVars);
  fDiffList.Delete(idx);
end;
//------------------------------------------------------------------------------

procedure TDiff.InitDiagArrays(len1, len2: integer);
var
  i: integer;
begin
  //assumes that top and bottom matches have been excluded
  P8Bits(DiagF) := P8Bits(DiagBufferF) - sizeof(integer)*(MAX_DIAGONAL-(len1+1));
  for i := - (len1+1) to (len2+1) do DiagF[i] := -MAXINT;
  DiagF[1] := -1;

  P8Bits(DiagB) := P8Bits(DiagBufferB) - sizeof(integer)*(MAX_DIAGONAL-(len1+1));
  for i := - (len1+1) to (len2+1) do DiagB[i] := MAXINT;
  DiagB[len2-len1+1] := len2;
end;
//------------------------------------------------------------------------------

procedure TDiff.DiffInt(offset1, offset2, len1, len2: integer);
var
  p, k, delta: integer;
begin
  //trim matching bottoms ...
  while (len1 > 0) and (len2 > 0) and (Ints1[offset1] = Ints2[offset2]) do
  begin
    inc(offset1); inc(offset2); dec(len1); dec(len2);
  end;
  //trim matching tops ...
  while (len1 > 0) and (len2 > 0) and
    (Ints1[offset1+len1-1] = Ints2[offset2+len2-1]) do
  begin
    dec(len1); dec(len2);
  end;

  //stop diff'ing if minimal conditions reached ...
  if (len1 = 0) then
  begin
    AddChangeInt(offset1 ,len2, ckAdd);
    exit;
  end
  else if (len2 = 0) then
  begin
    AddChangeInt(offset1 ,len1, ckDelete);
    exit;
  end
  else if (len1 = 1) and (len2 = 1) then
  begin
    AddChangeInt(offset1, 1, ckDelete);
    AddChangeInt(offset1, 1, ckAdd);
    exit;
  end;

  p := -1;
  delta := len2 - len1;
  InitDiagArrays(len1, len2);
  if delta < 0 then
  begin
    repeat
      inc(p);
      if (p mod 1024) = 1023 then
      begin
        Application.ProcessMessages;
        if fCancelled then exit;
      end;
      //nb: the Snake order is important here
      for k := p downto delta +1 do
        if SnakeIntF(k,offset1,offset2,len1,len2) then exit;
      for k := -p + delta to delta-1 do
        if SnakeIntF(k,offset1,offset2,len1,len2) then exit;
      for k := delta -p to -1 do
        if SnakeIntB(k,offset1,offset2,len1,len2) then exit;
      for k := p downto 1 do
        if SnakeIntB(k,offset1,offset2,len1,len2) then exit;
      if SnakeIntF(delta,offset1,offset2,len1,len2) then exit;
      if SnakeIntB(0,offset1,offset2,len1,len2) then exit;
    until(false);
  end else
  begin
    repeat
      inc(p);
      if (p mod 1024) = 1023 then
      begin
        Application.ProcessMessages;
        if fCancelled then exit;
      end;
      //nb: the Snake order is important here
      for k := -p to delta -1 do
        if SnakeIntF(k,offset1,offset2,len1,len2) then exit;
      for k := p + delta downto delta +1 do
        if SnakeIntF(k,offset1,offset2,len1,len2) then exit;
      for k := delta + p downto 1 do
        if SnakeIntB(k,offset1,offset2,len1,len2) then exit;
      for k := -p to -1 do
        if SnakeIntB(k,offset1,offset2,len1,len2) then exit;
      if SnakeIntF(delta,offset1,offset2,len1,len2) then exit;
      if SnakeIntB(0,offset1,offset2,len1,len2) then exit;
    until(false);
  end;
end;
//------------------------------------------------------------------------------

procedure TDiff.DiffChr(offset1, offset2, len1, len2: integer);
var
  p, k, delta: integer;
begin
  //trim matching bottoms ...
  while (len1 > 0) and (len2 > 0) and (Chrs1[offset1] = Chrs2[offset2]) do
  begin
    inc(offset1); inc(offset2); dec(len1); dec(len2);
  end;
  //trim matching tops ...
  while (len1 > 0) and (len2 > 0) and
    (Chrs1[offset1+len1-1] = Chrs2[offset2+len2-1]) do
  begin
    dec(len1); dec(len2);
  end;

  //stop diff'ing if minimal conditions reached ...
  if (len1 = 0) then
  begin
    AddChangeChr(offset1 ,len2, ckAdd);
    exit;
  end
  else if (len2 = 0) then
  begin
    AddChangeChr(offset1, len1, ckDelete);
    exit;
  end
  else if (len1 = 1) and (len2 = 1) then
  begin
    AddChangeChr(offset1, 1, ckDelete);
    AddChangeChr(offset1, 1, ckAdd);
    exit;
  end;

  p := -1;
  delta := len2 - len1;
  InitDiagArrays(len1, len2);
  if delta < 0 then
  begin
    repeat
      inc(p);
      if (p mod 1024 = 1023) then
      begin
        Application.ProcessMessages;
        if fCancelled then exit;
      end;
      //nb: the Snake order is important here
      for k := p downto delta +1 do
        if SnakeChrF(k,offset1,offset2,len1,len2) then exit;
      for k := -p + delta to delta-1 do
        if SnakeChrF(k,offset1,offset2,len1,len2) then exit;
      for k := delta -p to -1 do
        if SnakeChrB(k,offset1,offset2,len1,len2) then exit;
      for k := p downto 1 do
        if SnakeChrB(k,offset1,offset2,len1,len2) then exit;
      if SnakeChrF(delta,offset1,offset2,len1,len2) then exit;
      if SnakeChrB(0,offset1,offset2,len1,len2) then exit;
    until(false);
  end else
  begin
    repeat
      inc(p);
      if (p mod 1024 = 1023) then
      begin
        Application.ProcessMessages;
        if fCancelled then exit;
      end;
      //nb: the Snake order is important here
      for k := -p to delta -1 do
        if SnakeChrF(k,offset1,offset2,len1,len2) then exit;
      for k := p + delta downto delta +1 do
        if SnakeChrF(k,offset1,offset2,len1,len2) then exit;
      for k := delta + p downto 1 do
        if SnakeChrB(k,offset1,offset2,len1,len2) then exit;
      for k := -p to -1 do
        if SnakeChrB(k,offset1,offset2,len1,len2) then exit;
      if SnakeChrF(delta,offset1,offset2,len1,len2) then exit;
      if SnakeChrB(0,offset1,offset2,len1,len2) then exit;
    until(false);
  end;
end;
//------------------------------------------------------------------------------

function TDiff.SnakeChrF(k,offset1,offset2,len1,len2: integer): boolean;
var
  x,y: integer;
begin
  if DiagF[k+1] > DiagF[k-1] then
    y := DiagF[k+1] else
    y := DiagF[k-1]+1;
  x := y - k;
  while (x < len1-1) and (y < len2-1) and
    (Chrs1[offset1+x+1] = Chrs2[offset2+y+1]) do
  begin
    inc(x); inc(y);
  end;
  DiagF[k] := y;
  result := (DiagF[k] >= DiagB[k]);
  if not result then exit;

  inc(x); inc(y);
  PushDiff(offset1+x, offset2+y, len1-x, len2-y);
  PushDiff(offset1, offset2, x, y);
end;
//------------------------------------------------------------------------------

function TDiff.SnakeChrB(k,offset1,offset2,len1,len2: integer): boolean;
var
  x,y: integer;
begin
  if DiagB[k-1] < DiagB[k+1] then
    y := DiagB[k-1] else
    y := DiagB[k+1]-1;
  x := y - k;
  while (x >= 0) and (y >= 0) and (Chrs1[offset1+x] = Chrs2[offset2+y]) do
  begin
    dec(x); dec(y);
  end;
  DiagB[k] := y;
  result := DiagB[k] <= DiagF[k];
  if not result then exit;

  inc(x); inc(y);
  PushDiff(offset1+x, offset2+y, len1-x, len2-y);
  PushDiff(offset1, offset2, x, y);
end;
//------------------------------------------------------------------------------

function TDiff.SnakeIntF(k,offset1,offset2,len1,len2: integer): boolean;
var
  x,y: integer;
begin
  if DiagF[k+1] > DiagF[k-1] then
    y := DiagF[k+1] else
    y := DiagF[k-1]+1;
  x := y - k;
  while (x < len1-1) and (y < len2-1) and
    (Ints1[offset1+x+1] = Ints2[offset2+y+1]) do
  begin
    inc(x); inc(y);
  end;
  DiagF[k] := y;
  result := (DiagF[k] >= DiagB[k]);
  if not result then exit;

  inc(x); inc(y);
  PushDiff(offset1+x, offset2+y, len1-x, len2-y);
  PushDiff(offset1, offset2, x, y);
end;
//------------------------------------------------------------------------------

function TDiff.SnakeIntB(k,offset1,offset2,len1,len2: integer): boolean;
var
  x,y: integer;
begin
  if DiagB[k-1] < DiagB[k+1] then
    y := DiagB[k-1] else
    y := DiagB[k+1]-1;
  x := y - k;
  while (x >= 0) and (y >= 0) and (Ints1[offset1+x] = Ints2[offset2+y]) do
  begin
    dec(x); dec(y);
  end;
  DiagB[k] := y;
  result := DiagB[k] <= DiagF[k];
  if not result then exit;

  inc(x); inc(y);
  PushDiff(offset1+x, offset2+y, len1-x, len2-y);
  PushDiff(offset1, offset2, x, y);
end;
//------------------------------------------------------------------------------

procedure TDiff.AddChangeChr(offset1, range: integer; ChangeKind: TChangeKind);
var
  i,j: integer;
  compareRec: PCompareRec;
begin
  //first, add any unchanged items into this list ...
  while (fLastCompareRec.oldIndex1 < offset1 -1) do
  begin
    with fLastCompareRec do
    begin
      Kind := ckNone;
      inc(oldIndex1);
      inc(oldIndex2);
      chr1 := Chrs1[oldIndex1];
      chr2 := Chrs2[oldIndex2];
    end;
    New(compareRec);
    compareRec^ := fLastCompareRec;
    fCompareList.Add(compareRec);
    inc(fDiffStats.matches);
  end;

  case ChangeKind of
    ckNone:
      for i := 1 to range do
      begin
        with fLastCompareRec do
        begin
          Kind := ckNone;
          inc(oldIndex1);
          inc(oldIndex2);
          chr1 := Chrs1[oldIndex1];
          chr2 := Chrs2[oldIndex2];
        end;
        New(compareRec);
        compareRec^ := fLastCompareRec;
        fCompareList.Add(compareRec);
        inc(fDiffStats.matches);
      end;
    ckAdd :
      begin
        for i := 1 to range do
        begin
          with fLastCompareRec do
          begin

            //check if a range of adds are following a range of deletes
            //and convert them to modifies ...
            if Kind = ckDelete then
            begin
              j := fCompareList.Count -1;
              while (j > 0) and
                (PCompareRec(fCompareList[j-1]).Kind = ckDelete) do
                  dec(j);
              PCompareRec(fCompareList[j]).Kind := ckModify;
              dec(fDiffStats.deletes);
              inc(fDiffStats.modifies);
              inc(fLastCompareRec.oldIndex2);
              PCompareRec(fCompareList[j]).oldIndex2 := fLastCompareRec.oldIndex2;
              PCompareRec(fCompareList[j]).chr2 := Chrs2[oldIndex2];
              if j = fCompareList.Count-1 then fLastCompareRec.Kind := ckModify;
              continue;
            end;

            Kind := ckAdd;
            chr1 := #0;
            inc(oldIndex2);
            chr2 := Chrs2[oldIndex2]; //ie what we added
          end;
          New(compareRec);
          compareRec^ := fLastCompareRec;
          fCompareList.Add(compareRec);
          inc(fDiffStats.adds);
        end;
      end;
    ckDelete :
      begin
        for i := 1 to range do
        begin
          with fLastCompareRec do
          begin

            //check if a range of deletes are following a range of adds
            //and convert them to modifies ...
            if Kind = ckAdd then
            begin
              j := fCompareList.Count -1;
              while (j > 0) and (PCompareRec(fCompareList[j-1]).Kind = ckAdd) do
                dec(j);
              PCompareRec(fCompareList[j]).Kind := ckModify;
              dec(fDiffStats.adds);
              inc(fDiffStats.modifies);
              inc(fLastCompareRec.oldIndex1);
              PCompareRec(fCompareList[j]).oldIndex1 := fLastCompareRec.oldIndex1;
              PCompareRec(fCompareList[j]).chr1 := Chrs1[oldIndex1];
              if j = fCompareList.Count-1 then fLastCompareRec.Kind := ckModify;
              continue;
            end;

            Kind := ckDelete;
            chr2 := #0;
            inc(oldIndex1);
            chr1 := Chrs1[oldIndex1]; //ie what we deleted
          end;
          New(compareRec);
          compareRec^ := fLastCompareRec;
          fCompareList.Add(compareRec);
          inc(fDiffStats.deletes);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TDiff.AddChangeInt(offset1, range: integer; ChangeKind: TChangeKind);
var
  i,j: integer;
  compareRec: PCompareRec;
begin
  //first, add any unchanged items into this list ...
  while (fLastCompareRec.oldIndex1 < offset1 -1) do
  begin
    with fLastCompareRec do
    begin
      Kind := ckNone;
      inc(oldIndex1);
      inc(oldIndex2);
      int1 := Ints1[oldIndex1];
      int2 := Ints2[oldIndex2];
    end;
    New(compareRec);
    compareRec^ := fLastCompareRec;
    fCompareList.Add(compareRec);
    inc(fDiffStats.matches);
  end;

  case ChangeKind of
    ckNone:
      for i := 1 to range do
      begin
        with fLastCompareRec do
        begin
          Kind := ckNone;
          inc(oldIndex1);
          inc(oldIndex2);
          int1 := Ints1[oldIndex1];
          int2 := Ints2[oldIndex2];
        end;
        New(compareRec);
        compareRec^ := fLastCompareRec;
        fCompareList.Add(compareRec);
        inc(fDiffStats.matches);
      end;
    ckAdd :
      begin
        for i := 1 to range do
        begin
          with fLastCompareRec do
          begin

            //check if a range of adds are following a range of deletes
            //and convert them to modifies ...
            if Kind = ckDelete then
            begin
              j := fCompareList.Count -1;
              while (j > 0) and (PCompareRec(fCompareList[j-1]).Kind = ckDelete) do
                dec(j);
              PCompareRec(fCompareList[j]).Kind := ckModify;
              dec(fDiffStats.deletes);
              inc(fDiffStats.modifies);
              inc(fLastCompareRec.oldIndex2);
              PCompareRec(fCompareList[j]).oldIndex2 := fLastCompareRec.oldIndex2;
              PCompareRec(fCompareList[j]).int2 := Ints2[oldIndex2];
              if j = fCompareList.Count-1 then fLastCompareRec.Kind := ckModify;
              continue;
            end;

            Kind := ckAdd;
            int1 := $0;
            inc(oldIndex2);
            int2 := Ints2[oldIndex2]; //ie what we added
          end;
          New(compareRec);
          compareRec^ := fLastCompareRec;
          fCompareList.Add(compareRec);
          inc(fDiffStats.adds);
        end;
      end;
    ckDelete :
      begin
        for i := 1 to range do
        begin
          with fLastCompareRec do
          begin

            //check if a range of deletes are following a range of adds
            //and convert them to modifies ...
            if Kind = ckAdd then
            begin
              j := fCompareList.Count -1;
              while (j > 0) and (PCompareRec(fCompareList[j-1]).Kind = ckAdd) do
                dec(j);
              PCompareRec(fCompareList[j]).Kind := ckModify;
              dec(fDiffStats.adds);
              inc(fDiffStats.modifies);
              inc(fLastCompareRec.oldIndex1);
              PCompareRec(fCompareList[j]).oldIndex1 := fLastCompareRec.oldIndex1;
              PCompareRec(fCompareList[j]).int1 := Ints1[oldIndex1];
              if j = fCompareList.Count-1 then fLastCompareRec.Kind := ckModify;
              continue;
            end;

            Kind := ckDelete;
            int2 := $0;
            inc(oldIndex1);
            int1 := Ints1[oldIndex1]; //ie what we deleted
          end;
          New(compareRec);
          compareRec^ := fLastCompareRec;
          fCompareList.Add(compareRec);
          inc(fDiffStats.deletes);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TDiff.Clear;
var
  i: integer;
begin
  for i := 0 to fCompareList.Count-1 do
    dispose(PCompareRec(fCompareList[i]));
  fCompareList.clear;
  fLastCompareRec.Kind := ckNone;
  fLastCompareRec.oldIndex1 := -1;
  fLastCompareRec.oldIndex2 := -1;
  fDiffStats.matches := 0;
  fDiffStats.adds := 0;
  fDiffStats.deletes :=0;
  fDiffStats.modifies :=0;
  Ints1 := nil; Ints2 := nil;
  Chrs1 := nil; Chrs2 := nil;
end;
//------------------------------------------------------------------------------

function TDiff.GetCompareCount: integer;
begin
  result := fCompareList.count;
end;
//------------------------------------------------------------------------------

function TDiff.GetCompare(index: integer): TCompareRec;
begin
  result := PCompareRec(fCompareList[index])^;
end;
//------------------------------------------------------------------------------

procedure TDiff.Cancel;
begin
  fCancelled := true;
end;
//------------------------------------------------------------------------------

end.
