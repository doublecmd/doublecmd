{ Utilities using light weight threads.

  This file is part of the Free Pascal run time library.

  Copyright (C) 2008 Mattias Gaertner mattias@freepascal.org

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  Abstract:
    Utility functions using mtprocs.
    For example a parallel sort.

}
unit MTPUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MTProcs;

type
  TSortPartEvent = procedure(aList: PPointer; aCount: PtrInt);

  { TParallelSortPointerList }

  TParallelSortPointerList = class
  protected
    fBlockSize: PtrInt;
    fBlockCntPowOf2Offset: PtrInt;
    FMergeBuffer: PPointer;
    procedure MTPSort(Index: PtrInt; {%H-}Data: Pointer; Item: TMultiThreadProcItem);
  public
    List: PPointer;
    Count: PtrInt;
    Compare: TListSortCompare;
    BlockCnt: PtrInt;
    OnSortPart: TSortPartEvent;
    constructor Create(aList: PPointer; aCount: PtrInt; const aCompare: TListSortCompare;
                       MaxThreadCount: integer = 0);
    procedure Sort;
  end;

{ Sort a list in parallel using merge sort.
  You must provide a compare function.
  You can provide your own sort function for the blocks which are sorted in a
  single thread, for example a normal quicksort. }
procedure ParallelSortFPList(List: TFPList; const Compare: TListSortCompare;
  MaxThreadCount: integer = 0; const OnSortPart: TSortPartEvent = nil);

implementation

procedure ParallelSortFPList(List: TFPList; const Compare: TListSortCompare;
  MaxThreadCount: integer; const OnSortPart: TSortPartEvent);
var
  Sorter: TParallelSortPointerList;
begin
  if List.Count<=1 then exit;
  Sorter:=TParallelSortPointerList.Create(@List.List[0],List.Count,Compare,
                                          MaxThreadCount);
  try
    Sorter.OnSortPart:=OnSortPart;
    Sorter.Sort;
  finally
    Sorter.Free;
  end;
end;

{ TParallelSortPointerList }

procedure TParallelSortPointerList.MTPSort(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);

  procedure MergeSort(L, M, R: PtrInt; Recursive: boolean);
  var
    Src1: PtrInt;
    Src2: PtrInt;
    Dest1: PtrInt;
  begin
    if R-L<=1 then begin
      // sort lists of 1 and 2 items directly
      if L<R then begin
        if Compare(List[L],List[R])>0 then begin
          FMergeBuffer[L]:=List[L];
          List[L]:=List[R];
          List[R]:=FMergeBuffer[L];
        end;
      end;
      exit;
    end;
    // sort recursively
    if Recursive then begin
      MergeSort(L,(L+M) div 2,M-1,true);
      MergeSort(M,(M+R+1) div 2,R,true);
    end;
    // merge both blocks
    Src1:=L;
    Src2:=M;
    Dest1:=L;
    repeat
      if (Src1<M)
      and ((Src2>R) or (Compare(List[Src1],List[Src2])<=0)) then begin
        FMergeBuffer[Dest1]:=List[Src1];
        inc(Dest1);
        inc(Src1);
      end else if (Src2<=R) then begin
        FMergeBuffer[Dest1]:=List[Src2];
        inc(Dest1);
        inc(Src2);
      end else
        break;
    until false;
    // write the mergebuffer back
    Src1:=L;
    Dest1:=l;
    while Src1<=R do begin
      List[Dest1]:=FMergeBuffer[Src1];
      inc(Src1);
      inc(Dest1);
    end;
  end;

var
  L, M, R: PtrInt;
  i: integer;
  NormIndex: Integer;
  Range: integer;
  MergeIndex: Integer;
begin
  L:=fBlockSize*Index;
  R:=L+fBlockSize-1;
  if R>=Count then
    R:=Count-1; // last block
  //WriteLn('TParallelSortPointerList.LWTSort Index=',Index,' sort block: ',L,' ',(L+R+1) div 2,' ',R);
  if Assigned(OnSortPart) then
    OnSortPart(@List[L],R-L+1)
  else
    MergeSort(L,(L+R+1) div 2,R,true);

  // merge
  //  0 1 2 3 4 5 6 7
  //  \/  \/  \/  \/
  //    \/      \/
  //        \/
  // For example: BlockCnt = 5  => Index in 0..4
  //   fBlockCntPowOf2Offset = 3  (=8-5)
  //   NormIndex = Index + 3  => NormIndex in 3..7
  NormIndex:=Index+fBlockCntPowOf2Offset;
  i:=0;
  repeat
    Range:=1 shl i;
    if NormIndex and Range=0 then break;
    // merge left and right block(s)
    MergeIndex:=NormIndex-Range-fBlockCntPowOf2Offset;
    if (MergeIndex+Range-1>=0) then begin
      // wait until left blocks have finished
      //WriteLn('TParallelSortPointerList.LWTSort Index=',Index,' wait for block ',MergeIndex);
      if (MergeIndex>=0) and (not Item.WaitForIndex(MergeIndex)) then exit;
      // compute left and right block bounds
      M:=L;
      L:=(MergeIndex-Range+1)*fBlockSize;
      if L<0 then L:=0;
      //WriteLn('TParallelSortPointerList.LWTSort Index=',Index,' merge blocks ',L,' ',M,' ',R);
      MergeSort(L,M,R,false);
    end;
    inc(i);
  until false;
  //WriteLn('TParallelSortPointerList.LWTSort END Index='+IntToStr(Index));
end;

constructor TParallelSortPointerList.Create(aList: PPointer; aCount: PtrInt;
  const aCompare: TListSortCompare; MaxThreadCount: integer);
begin
  List:=aList;
  Count:=aCount;
  Compare:=aCompare;
  BlockCnt:=Count div 100; // at least 100 items per thread
  if BlockCnt>ProcThreadPool.MaxThreadCount then
    BlockCnt:=ProcThreadPool.MaxThreadCount;
  if (MaxThreadCount>0) and (BlockCnt>MaxThreadCount) then
    BlockCnt:=MaxThreadCount;
  if BlockCnt<1 then BlockCnt:=1;
end;

procedure TParallelSortPointerList.Sort;
begin
  if (Count<=1) then exit;
  fBlockSize:=(Count+BlockCnt-1) div BlockCnt;
  fBlockCntPowOf2Offset:=1;
  while fBlockCntPowOf2Offset<BlockCnt do
    fBlockCntPowOf2Offset:=fBlockCntPowOf2Offset*2;
  fBlockCntPowOf2Offset:=fBlockCntPowOf2Offset-BlockCnt;
  //WriteLn('TParallelSortPointerList.Sort BlockCnt=',BlockCnt,' fBlockSize=',fBlockSize,' fBlockCntPowOf2Offset=',fBlockCntPowOf2Offset);
  GetMem(FMergeBuffer,SizeOf(Pointer)*Count);
  try
    ProcThreadPool.DoParallel(@MTPSort,0,BlockCnt-1);
  finally
    FreeMem(FMergeBuffer);
    FMergeBuffer:=nil;
  end;
end;

end.

