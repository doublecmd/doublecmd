unit uDiff;

(*******************************************************************************
* Component         TDiff                                                      *
* Version:          3.1                                                        *
* Date:             7 November 2009                                            *
* Compilers:        Delphi 7 - Delphi2009                                      *
* Author:           Angus Johnson - angusj-AT-myrealbox-DOT-com                *
* Copyright:        © 2001-200( Angus Johnson                                  *
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
*                   "An O(ND) Difference Algorithm and its Variations"         *
*                   By E Myers - Algorithmica Vol. 1 No. 2, 1986, pp. 251-266  *
*                   http://www.cs.arizona.edu/people/gene/                     *
*                   http://www.cs.arizona.edu/people/gene/PAPERS/diff.ps       *
*                                                                              *
*******************************************************************************)


(*******************************************************************************
* History:                                                                     *
* 13 December 2001 - Original Release                                          *
* 22 April 2008    - Complete rewrite to greatly improve the code and          *
*                    provide a much simpler view of differences through a new  *
*                    'Compares' property.                                      *
* 7 November 2009  - Updated so now compiles in newer versions of Delphi.      *
*******************************************************************************)

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, Math, Forms;

const
  //Maximum realistic deviation from centre diagonal vector ...
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

  TDiffStats = record
    matches  : integer;
    adds     : integer;
    deletes  : integer;
    modifies : integer;
  end;

  TDiff = class(TComponent)
  private
    fCompareList: TList;
    fCancelled: boolean;
    fExecuting: boolean;
    fDiagBuffer, bDiagBuffer: pointer;
    Chrs1, Chrs2: PChrArray;
    Ints1, Ints2: PIntArray;
    LastCompareRec: TCompareRec;
    fDiag, bDiag: PDiags;
    fDiffStats: TDiffStats;
    procedure InitDiagArrays(MaxOscill, len1, len2: integer);
    //nb: To optimize speed, separate functions are called for either
    //integer or character compares ...
    procedure RecursiveDiffChr(offset1, offset2, len1, len2: integer);
    procedure AddChangeChrs(offset1, range: integer; ChangeKind: TChangeKind);
    procedure RecursiveDiffInt(offset1, offset2, len1, len2: integer);
    procedure AddChangeInts(offset1, range: integer; ChangeKind: TChangeKind);

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

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TDiff]);
end;

constructor TDiff.Create(aOwner: TComponent);
begin
  inherited;
  fCompareList := TList.create;
end;
//------------------------------------------------------------------------------

destructor TDiff.Destroy;
begin
  Clear;
  fCompareList.free;
  inherited;
end;
//------------------------------------------------------------------------------

function TDiff.Execute(pchrs1, pchrs2: PChar; len1, len2: integer): boolean;
var
  maxOscill, x1,x2, savedLen: integer;
  compareRec: PCompareRec;
begin
  result := not fExecuting;
  if not result then exit;
  fExecuting := true;
  fCancelled := false;
  try
    Clear;

    //save first string length for later (ie for any trailing matches) ...
    savedLen := len1-1;

    //setup the character arrays ...
    Chrs1 := pointer(pchrs1);
    Chrs2 := pointer(pchrs2);

    //ignore top matches ...
    x1:= 0; x2 := 0;
    while (len1 > 0) and (len2 > 0) and (Chrs1[len1-1] = Chrs2[len2-1]) do
    begin
      dec(len1); dec(len2);
    end;

    //if something doesn't match ...
    if (len1 <> 0) or (len2 <> 0) then
    begin
      //ignore bottom of matches too ...
      while (len1 > 0) and (len2 > 0) and (Chrs1[x1] = Chrs2[x2]) do
      begin
        dec(len1); dec(len2);
        inc(x1); inc(x2);
      end;

      maxOscill := min(max(len1,len2), MAX_DIAGONAL);
      fCompareList.Capacity := len1 + len2;

      //nb: the Diag arrays are extended by 1 at each end to avoid testing
      //for array limits. Hence '+3' because will also includes Diag[0] ...
      GetMem(fDiagBuffer, sizeof(integer)*(maxOscill*2+3));
      GetMem(bDiagBuffer, sizeof(integer)*(maxOscill*2+3));
      try
        RecursiveDiffChr(x1, x2, len1, len2);
      finally
        freeMem(fDiagBuffer);
        freeMem(bDiagBuffer);
      end;
    end;

    if fCancelled then
    begin
      result := false;
      Clear;
      exit;
    end;

    //finally, append any trailing matches onto compareList ...
    while (LastCompareRec.oldIndex1 < savedLen) do
    begin
      with LastCompareRec do
      begin
        Kind := ckNone;
        inc(oldIndex1);
        inc(oldIndex2);
        chr1 := Chrs1[oldIndex1];
        chr2 := Chrs2[oldIndex2];
      end;
      New(compareRec);
      compareRec^ := LastCompareRec;
      fCompareList.Add(compareRec);
      inc(fDiffStats.matches);
    end;
  finally
    fExecuting := false;
  end;

end;
//------------------------------------------------------------------------------

function TDiff.Execute(pints1, pints2: PInteger; len1, len2: integer): boolean;
var
  maxOscill, x1,x2, savedLen: integer;
  compareRec: PCompareRec;
begin
  result := not fExecuting;
  if not result then exit;
  fExecuting := true;
  fCancelled := false;
  try
    Clear;

    //setup the character arrays ...
    Ints1 := pointer(pints1);
    Ints2 := pointer(pints2);

    //save first string length for later (ie for any trailing matches) ...
    savedLen := len1-1;

    //ignore top matches ...
    x1:= 0; x2 := 0;
    while (len1 > 0) and (len2 > 0) and (Ints1[len1-1] = Ints2[len2-1]) do
    begin
      dec(len1); dec(len2);
    end;

    //if something doesn't match ...
    if (len1 <> 0) or (len2 <> 0) then
    begin

      //ignore bottom of matches too ...
      while (len1 > 0) and (len2 > 0) and (Ints1[x1] = Ints2[x2]) do
      begin
        dec(len1); dec(len2);
        inc(x1); inc(x2);
      end;

      maxOscill := min(max(len1,len2), MAX_DIAGONAL);
      fCompareList.Capacity := len1 + len2;

      //nb: the Diag arrays are extended by 1 at each end to avoid testing
      //for array limits. Hence '+3' because will also includes Diag[0] ...
      GetMem(fDiagBuffer, sizeof(integer)*(maxOscill*2+3));
      GetMem(bDiagBuffer, sizeof(integer)*(maxOscill*2+3));
      try
        RecursiveDiffInt(x1, x2, len1, len2);
      finally
        freeMem(fDiagBuffer);
        freeMem(bDiagBuffer);
      end;
    end;

    if fCancelled then
    begin
      result := false;
      Clear;
      exit;
    end;

    //finally, append any trailing matches onto compareList ...
    while (LastCompareRec.oldIndex1 < savedLen) do
    begin
      with LastCompareRec do
      begin
        Kind := ckNone;
        inc(oldIndex1);
        inc(oldIndex2);
        int1 := Ints1[oldIndex1];
        int2 := Ints2[oldIndex2];
      end;
      New(compareRec);
      compareRec^ := LastCompareRec;
      fCompareList.Add(compareRec);
      inc(fDiffStats.matches);
    end;
  finally
    fExecuting := false;
  end;

end;
//------------------------------------------------------------------------------

procedure TDiff.InitDiagArrays(MaxOscill, len1, len2: integer);
var
  diag: integer;
begin
  inc(maxOscill); //for the extra diag at each end of the arrays ...
  P8Bits(fDiag) := P8Bits(fDiagBuffer) - sizeof(integer)*(MAX_DIAGONAL-maxOscill);
  P8Bits(bDiag) := P8Bits(bDiagBuffer) - sizeof(integer)*(MAX_DIAGONAL-maxOscill);
  //initialize Diag arrays (assumes 0 based arrays) ...
  for diag := - maxOscill to maxOscill do fDiag[diag] := -MAXINT;
  fDiag[0] := -1;
  for diag := - maxOscill to maxOscill do bDiag[diag] := MAXINT;
  bDiag[len1 - len2] := len1-1;
end;
//------------------------------------------------------------------------------

procedure TDiff.RecursiveDiffChr(offset1, offset2, len1, len2: integer);
var
  diag, lenDelta, Oscill, maxOscill, x1, x2: integer;
begin
  //nb: the possible depth of recursion here is most unlikely to cause
  //    problems with stack overflows.
  application.processmessages;
  if fCancelled then exit;

  if (len1 = 0) then
  begin
    AddChangeChrs(offset1, len2, ckAdd);
    exit;
  end
  else if (len2 = 0) then
  begin
    AddChangeChrs(offset1, len1, ckDelete);
    exit;
  end
  else if (len1 = 1) and (len2 = 1) then
  begin
    AddChangeChrs(offset1, 1, ckDelete);
    AddChangeChrs(offset1, 1, ckAdd);
    exit;
  end;

  maxOscill := min(max(len1,len2), MAX_DIAGONAL);
  InitDiagArrays(MaxOscill, len1, len2);
  lenDelta := len1 -len2;

  Oscill := 1; //ie assumes prior filter of top and bottom matches
  while Oscill <= maxOscill do
  begin

    if (Oscill mod 200) = 0 then
    begin
      application.processmessages;
      if fCancelled then exit;
    end;

    //do forward oscillation (keeping diag within assigned grid)...
    diag := Oscill;
    while diag > len1 do dec(diag,2);
    while diag >= max(- Oscill, -len2) do
    begin
      if fDiag[diag-1] < fDiag[diag+1] then
        x1 := fDiag[diag+1] else
        x1 := fDiag[diag-1]+1;
      x2 := x1 - diag;
      while (x1 < len1-1) and (x2 < len2-1) and
        (Chrs1[offset1+x1+1] = Chrs2[offset2+x2+1]) do
      begin
        inc(x1); inc(x2);
      end;
      fDiag[diag] := x1;

      //nb: (fDiag[diag] is always < bDiag[diag]) here when NOT odd(lenDelta) ...
      if odd(lenDelta) and (fDiag[diag] >= bDiag[diag]) then
      begin
        inc(x1);inc(x2);
        //save x1 & x2 for second recursive_diff() call by reusing no longer
        //needed variables (ie minimize variable allocation in recursive fn) ...
        diag := x1; Oscill := x2;
        while (x1 > 0) and (x2 > 0) and (Chrs1[offset1+x1-1] = Chrs2[offset2+x2-1]) do
        begin
          dec(x1); dec(x2);
        end;
        RecursiveDiffChr(offset1, offset2, x1, x2);
        x1 := diag; x2 := Oscill;
        RecursiveDiffChr(offset1+x1, offset2+x2, len1-x1, len2-x2);
        exit; //ALL DONE
      end;
      dec(diag,2);
    end;

    //do backward oscillation (keeping diag within assigned grid)...
    diag := lenDelta + Oscill;
    while diag > len1 do dec(diag,2);
    while diag >= max(lenDelta - Oscill, -len2)  do
    begin
      if bDiag[diag-1] < bDiag[diag+1] then
        x1 := bDiag[diag-1] else
        x1 := bDiag[diag+1]-1;
      x2 := x1 - diag;
      while (x1 > -1) and (x2 > -1) and (Chrs1[offset1+x1] = Chrs2[offset2+x2]) do
      begin
        dec(x1); dec(x2);
      end;
      bDiag[diag] := x1;

      if bDiag[diag] <= fDiag[diag] then
      begin
        //flag return value then ...
        inc(x1);inc(x2);
        RecursiveDiffChr(offset1, offset2, x1, x2);
        while (x1 < len1) and (x2 < len2) and
          (Chrs1[offset1+x1] = Chrs2[offset2+x2]) do
        begin
          inc(x1); inc(x2);
        end;
        RecursiveDiffChr(offset1+x1, offset2+x2, len1-x1, len2-x2);
        exit; //ALL DONE
      end;
      dec(diag,2);
    end;

    inc(Oscill);
  end; //while Oscill <= maxOscill

  raise Exception.create('oops - error in RecursiveDiffChr()');
end;
//------------------------------------------------------------------------------

procedure TDiff.RecursiveDiffInt(offset1, offset2, len1, len2: integer);
var
  diag, lenDelta, Oscill, maxOscill, x1, x2: integer;
begin
  //nb: the possible depth of recursion here is most unlikely to cause
  //    problems with stack overflows.
  application.processmessages;
  if fCancelled then exit;

  if (len1 = 0) then
  begin
    assert(len2 > 0,'oops!');
    AddChangeInts(offset1, len2, ckAdd);
    exit;
  end
  else if (len2 = 0) then
  begin
    AddChangeInts(offset1, len1, ckDelete);
    exit;
  end
  else if (len1 = 1) and (len2 = 1) then
  begin
    assert(Ints1[offset1] <> Ints2[offset2],'oops!');
    AddChangeInts(offset1, 1, ckDelete);
    AddChangeInts(offset1, 1, ckAdd);
    exit;
  end;

  maxOscill := min(max(len1,len2), MAX_DIAGONAL);
  InitDiagArrays(MaxOscill, len1, len2);
  lenDelta := len1 -len2;

  Oscill := 1; //ie assumes prior filter of top and bottom matches
  while Oscill <= maxOscill do
  begin

    if (Oscill mod 200) = 0 then
    begin
      application.processmessages;
      if fCancelled then exit;
    end;

    //do forward oscillation (keeping diag within assigned grid)...
    diag := Oscill;
    while diag > len1 do dec(diag,2);
    while diag >= max(- Oscill, -len2) do
    begin
      if fDiag[diag-1] < fDiag[diag+1] then
        x1 := fDiag[diag+1] else
        x1 := fDiag[diag-1]+1;
      x2 := x1 - diag;
      while (x1 < len1-1) and (x2 < len2-1) and
        (Ints1[offset1+x1+1] = Ints2[offset2+x2+1]) do
      begin
        inc(x1); inc(x2);
      end;
      fDiag[diag] := x1;

      //nb: (fDiag[diag] is always < bDiag[diag]) here when NOT odd(lenDelta) ...
      if odd(lenDelta) and (fDiag[diag] >= bDiag[diag]) then
      begin
        inc(x1);inc(x2);
        //save x1 & x2 for second recursive_diff() call by reusing no longer
        //needed variables (ie minimize variable allocation in recursive fn) ...
        diag := x1; Oscill := x2;
        while (x1 > 0) and (x2 > 0) and (Ints1[offset1+x1-1] = Ints2[offset2+x2-1]) do
        begin
          dec(x1); dec(x2);
        end;
        RecursiveDiffInt(offset1, offset2, x1, x2);
        x1 := diag; x2 := Oscill;
        RecursiveDiffInt(offset1+x1, offset2+x2, len1-x1, len2-x2);
        exit; //ALL DONE
      end;
      dec(diag,2);
    end;

    //do backward oscillation (keeping diag within assigned grid)...
    diag := lenDelta + Oscill;
    while diag > len1 do dec(diag,2);
    while diag >= max(lenDelta - Oscill, -len2)  do
    begin
      if bDiag[diag-1] < bDiag[diag+1] then
        x1 := bDiag[diag-1] else
        x1 := bDiag[diag+1]-1;
      x2 := x1 - diag;
      while (x1 > -1) and (x2 > -1) and (Ints1[offset1+x1] = Ints2[offset2+x2]) do
      begin
        dec(x1); dec(x2);
      end;
      bDiag[diag] := x1;

      if bDiag[diag] <= fDiag[diag] then
      begin
        //flag return value then ...
        inc(x1);inc(x2);
        RecursiveDiffInt(offset1, offset2, x1, x2);
        while (x1 < len1) and (x2 < len2) and
          (Ints1[offset1+x1] = Ints2[offset2+x2]) do
        begin
          inc(x1); inc(x2);
        end;
        RecursiveDiffInt(offset1+x1, offset2+x2, len1-x1, len2-x2);
        exit; //ALL DONE
      end;
      dec(diag,2);
    end;

    inc(Oscill);
  end; //while Oscill <= maxOscill

  raise Exception.create('oops - error in RecursiveDiffInt()');
end;
//------------------------------------------------------------------------------

procedure TDiff.Clear;
var
  i: integer;
begin
  for i := 0 to fCompareList.Count-1 do
    dispose(PCompareRec(fCompareList[i]));
  fCompareList.clear;
  LastCompareRec.Kind := ckNone;
  LastCompareRec.oldIndex1 := -1;
  LastCompareRec.oldIndex2 := -1;
  fDiffStats.matches := 0;
  fDiffStats.adds := 0;
  fDiffStats.deletes :=0;
  fDiffStats.modifies :=0;
  Chrs1 := nil; Chrs2 := nil; Ints1 := nil; Ints2 := nil;
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

procedure TDiff.AddChangeChrs(offset1, range: integer; ChangeKind: TChangeKind);
var
  i,j: integer;
  compareRec: PCompareRec;
begin
  //first, add any unchanged items into this list ...
  while (LastCompareRec.oldIndex1 < offset1 -1) do
  begin
    with LastCompareRec do
    begin
      Kind := ckNone;
      inc(oldIndex1);
      inc(oldIndex2);
      chr1 := Chrs1[oldIndex1];
      chr2 := Chrs2[oldIndex2];
    end;
    New(compareRec);
    compareRec^ := LastCompareRec;
    fCompareList.Add(compareRec);
    inc(fDiffStats.matches);
  end;

  case ChangeKind of
    ckAdd :
      begin
        for i := 1 to range do
        begin
          with LastCompareRec do
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
              inc(LastCompareRec.oldIndex2);
              PCompareRec(fCompareList[j]).oldIndex2 := LastCompareRec.oldIndex2;
              PCompareRec(fCompareList[j]).chr2 := Chrs2[oldIndex2];
              if j = fCompareList.Count-1 then LastCompareRec.Kind := ckModify;
              continue;
            end;

            Kind := ckAdd;
            chr1 := #0;
            inc(oldIndex2);
            chr2 := Chrs2[oldIndex2]; //ie what we added
          end;
          New(compareRec);
          compareRec^ := LastCompareRec;
          fCompareList.Add(compareRec);
          inc(fDiffStats.adds);
        end;
      end;
    ckDelete :
      begin
        for i := 1 to range do
        begin
          with LastCompareRec do
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
              inc(LastCompareRec.oldIndex1);
              PCompareRec(fCompareList[j]).oldIndex1 := LastCompareRec.oldIndex1;
              PCompareRec(fCompareList[j]).chr1 := Chrs1[oldIndex1];
              if j = fCompareList.Count-1 then LastCompareRec.Kind := ckModify;
              continue;
            end;

            Kind := ckDelete;
            chr2 := #0;
            inc(oldIndex1);
            chr1 := Chrs1[oldIndex1]; //ie what we deleted
          end;
          New(compareRec);
          compareRec^ := LastCompareRec;
          fCompareList.Add(compareRec);
          inc(fDiffStats.deletes);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TDiff.AddChangeInts(offset1, range: integer; ChangeKind: TChangeKind);
var
  i,j: integer;
  compareRec: PCompareRec;
begin
  //first, add any unchanged items into this list ...
  while (LastCompareRec.oldIndex1 < offset1 -1) do
  begin
    with LastCompareRec do
    begin
      Kind := ckNone;
      inc(oldIndex1);
      inc(oldIndex2);
      int1 := Ints1[oldIndex1];
      int2 := Ints2[oldIndex2];
    end;
    New(compareRec);
    compareRec^ := LastCompareRec;
    fCompareList.Add(compareRec);
    inc(fDiffStats.matches);
  end;

  case ChangeKind of
    ckAdd :
      begin
        for i := 1 to range do
        begin
          with LastCompareRec do
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
              inc(LastCompareRec.oldIndex2);
              PCompareRec(fCompareList[j]).oldIndex2 := LastCompareRec.oldIndex2;
              PCompareRec(fCompareList[j]).int2 := Ints2[oldIndex2];
              if j = fCompareList.Count-1 then LastCompareRec.Kind := ckModify;
              continue;
            end;

            Kind := ckAdd;
            int1 := $0;
            inc(oldIndex2);
            int2 := Ints2[oldIndex2]; //ie what we added
          end;
          New(compareRec);
          compareRec^ := LastCompareRec;
          fCompareList.Add(compareRec);
          inc(fDiffStats.adds);
        end;
      end;
    ckDelete :
      begin
        for i := 1 to range do
        begin
          with LastCompareRec do
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
              inc(LastCompareRec.oldIndex1);
              PCompareRec(fCompareList[j]).oldIndex1 := LastCompareRec.oldIndex1;
              PCompareRec(fCompareList[j]).int1 := Ints1[oldIndex1];
              if j = fCompareList.Count-1 then LastCompareRec.Kind := ckModify;
              continue;
            end;

            Kind := ckDelete;
            int2 := $0;
            inc(oldIndex1);
            int1 := Ints1[oldIndex1]; //ie what we deleted
          end;
          New(compareRec);
          compareRec^ := LastCompareRec;
          fCompareList.Add(compareRec);
          inc(fDiffStats.deletes);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TDiff.Cancel;
begin
  fCancelled := true;
end;
//------------------------------------------------------------------------------

end.
