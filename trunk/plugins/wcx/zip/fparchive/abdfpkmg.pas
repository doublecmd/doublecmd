(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbDfPkMg.pas                                *}
{*********************************************************}
{* Deflate package-merge algorithm                       *}
{*********************************************************}

unit AbDfPkMg;

{$I AbDefine.inc}

interface

uses
  AbDfBase;

procedure GenerateCodeLengths(aMaxCodeLen  : integer;
                        const aWeights     : array of integer;
                          var aCodeLengths : array of integer;
                              aStartInx    : integer;
                              aLog         : TAbLogger);

implementation

type
  PPkgNode = ^TPkgNode;
  TPkgNode = packed record
    pnWeight : integer;
    pnCount  : integer;
    pnLeft   : PPkgNode;
    pnRight  : PPkgNode;
  end;

  PPkgNodeList = ^TPkgNodeList;
  TPkgNodeList = array [0..pred(286 * 2)] of PPkgNode;
  {Note: the "286" is the number of literal/length symbols, the
         maximum number of weights we'll be calculating the optimal
         code lengths for}


{===helper routines==================================================}
function IsCalcFeasible(aCount      : integer;
                        aMaxCodeLen : integer) : boolean;

begin
  {works out if length-limited codes can be calculated for a given
   number of symbols and the maximum code length}

  {return whether 2^aMaxCodeLen > aCount}
  Result := (1 shl aMaxCodeLen) > aCount;
end;
{--------}
procedure QSS(aList  : PPkgNodeList;
              aFirst : integer;
              aLast  : integer);
var
  L, R  : integer;
  Pivot : integer;
  Temp  : pointer;
begin
  {while there are at least two items to sort}
  while (aFirst < aLast) do begin
    {the pivot is the middle item}
    Pivot := aList^[(aFirst+aLast) div 2]^.pnWeight;
    {set indexes and partition}
    L := pred(aFirst);
    R := succ(aLast);
    while true do begin
      repeat dec(R); until (aList^[R]^.pnWeight <= Pivot);
      repeat inc(L); until (aList^[L]^.pnWeight >= Pivot);
      if (L >= R) then Break;
      Temp := aList^[L];
      aList^[L] := aList^[R];
      aList^[R] := Temp;
    end;
    {quicksort the first subfile}
    if (aFirst < R) then
      QSS(aList, aFirst, R);
    {quicksort the second subfile - recursion removal}
    aFirst := succ(R);
  end;
end;
{--------}
procedure SortList(aList : PPkgNodeList; aCount : integer);
begin
  QSS(aList, 0, pred(aCount));
end;
{--------}
procedure Accumulate(aNode : PPkgNode);
begin
  while (aNode^.pnLeft <> nil) do begin
    Accumulate(aNode^.pnLeft);
    aNode := aNode^.pnRight;
  end;
  inc(aNode^.pnCount);
end;
{====================================================================}


{===Interfaced routine===============================================}
procedure GenerateCodeLengths(aMaxCodeLen  : integer;
                        const aWeights     : array of integer;
                          var aCodeLengths : array of integer;
                              aStartInx    : integer;
                              aLog         : TAbLogger);
var
  i   : integer;
  Bit : integer;
  WeightCount    : integer;
  OrigList       : PPkgNodeList;
  OrigListCount  : integer;
  MergeList      : PPkgNodeList;
  MergeListCount : integer;
  PkgList        : PPkgNodeList;
  PkgListCount   : integer;
  OrigInx        : integer;
  PkgInx         : integer;
  Node           : PPkgNode;
  NodeMgr        : TAbNodeManager;
begin
  {calculate the number of weights}
  WeightCount := succ(high(aWeights));

  {check for dumb programming errors}
  Assert((0 < aMaxCodeLen) and (aMaxCodeLen <= 15),
         'GenerateCodeLengths: the maximum code length should be in the range 1..15');
  Assert((1 <= WeightCount) and (WeightCount <= 286),
         'GenerateCodeLengths: the weight array must have 1..286 elements');
  Assert(IsCalcFeasible(WeightCount, aMaxCodeLen),
         'GenerateCodeLengths: the package-merge algorithm should always be feasible');

  {clear the code lengths array}
  FillChar(aCodeLengths[aStartInx], WeightCount * sizeof(integer), 0);

  {prepare for the try..finally}
  OrigList := nil;
  MergeList := nil;
  PkgList := nil;
  NodeMgr := nil;
  try

    {create the node manager}
    NodeMgr := TAbNodeManager.Create(sizeof(TPkgNode));

    {create the original list of nodes}
    GetMem(OrigList, WeightCount * sizeof(PPkgNode));
    OrigListCount := 0;
    for i := 0 to pred(WeightCount) do
      if (aWeights[i] <> 0) then begin
        Node := NodeMgr.AllocNode;
        Node^.pnLeft := nil;           { this will indicate a leaf}
        Node^.pnRight := pointer(i);   { the index of the weight}
        Node^.pnWeight := aWeights[i]; { the weight itself}
        Node^.pnCount := 1;            { how many times used}
        OrigList^[OrigListCount] := Node;
        inc(OrigListCount);
      end;

    {we need at least 2 items, so make anything less a special case}
    if (OrigListCount <= 1) then begin

      {if there are no items at all in the original list, we need to
       pretend that there is one, since we shall eventually need to
       calculate a Count-1 value that cannot be negative}
      if (OrigListCount = 0) then begin
        aCodeLengths[aStartInx] := 1;
        Exit;
      end;

      {otherwise there is only one item: set its code length directly}
      for i := 0 to pred(WeightCount) do
        if (aWeights[i] <> 0) then begin
          aCodeLengths[aStartInx + i] := 1;
          Exit;
        end;
    end;

    {there are at least 2 items in the list; so sort the list}
    SortList(OrigList, OrigListCount);

    {create the merge and package lists}
    GetMem(MergeList, OrigListCount * 2 * sizeof(PPkgNode));
    GetMem(PkgList, OrigListCount * 2 * sizeof(PPkgNode));

    {initialize the merge list to have the same items as the
     original list}
    Move(OrigList^, MergeList^, OrigListCount * sizeof(PPkgNode));
    MergeListCount := OrigListCount;

    {do aMaxCodeLen - 2 times...}
    for Bit := 1 to pred(aMaxCodeLen) do begin

      {generate the package list from the merge list by grouping pairs
       from the merge list and adding them to the package list}
      PkgListCount := 0;
      for i := 0 to pred(MergeListCount div 2) do begin
        Node := NodeMgr.AllocNode;
        Node^.pnLeft := MergeList^[i * 2];
        Node^.pnRight := MergeList^[i * 2 + 1];
        Node^.pnWeight := Node^.pnLeft^.pnWeight +
                          Node^.pnRight^.pnWeight;
        {$IFOPT C+}
        Node^.pnCount := 0;
        {$ENDIF}
        PkgList^[PkgListCount] := Node;
        inc(PkgListCount);
      end;

      {merge the original list and the package list}
      MergeListCount := 0;
      OrigInx := 0;
      PkgInx := 0;
      {note the optimization here: the package list will *always* be
       last to empty in the merge process since it will have at least
       one item whose accumulated weight is greater than all of the
       items in the original list}
      while (OrigInx < OrigListCount) and (PkgInx < PkgListCount) do begin
        if (OrigList^[OrigInx]^.pnWeight <= PkgList^[PkgInx]^.pnWeight) then begin
          MergeList^[MergeListCount] := OrigList^[OrigInx];
          inc(OrigInx);
        end
        else begin
          MergeList^[MergeListCount] := PkgList^[PkgInx];
          inc(PkgInx);
        end;
        inc(MergeListCount);
      end;
      if (OrigInx < OrigListCount) then begin
        Move(OrigList^[OrigInx], MergeList^[MergeListCount],
             (OrigListCount - OrigInx) * sizeof(PPkgNode));
        inc(MergeListCount, (OrigListCount - OrigInx));
      end
      else begin
        Move(PkgList^[PkgInx], MergeList^[MergeListCount],
             (PkgListCount - PkgInx) * sizeof(PPkgNode));
        inc(MergeListCount, (PkgListCount - PkgInx));
      end;
    end;

    {calculate the code lengths}
    for i := 0 to (OrigListCount * 2) - 3 do begin
      Node := MergeList^[i];
      if (Node^.pnLeft <> nil) then
        Accumulate(Node);
    end;
    for i := 0 to pred(OrigListCount) do
      aCodeLengths[aStartInx + integer(OrigList^[i].pnRight)] :=
          OrigList^[i].pnCount;
  finally
    FreeMem(OrigList);
    FreeMem(MergeList);
    FreeMem(PkgList);
    NodeMgr.Free;
  end;
end;
{====================================================================}

end.
