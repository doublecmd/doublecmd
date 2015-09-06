program RecursiveMTP1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  MTProcs;

type
  TArrayOfInteger = array of integer;
var
  Items: TArrayOfInteger;

type
  TFindMaximumParallelData = record
    Items: TArrayOfInteger;
    Left, Middle, Right: integer;
    LeftMaxIndex, RightMaxIndex: integer;
  end;
  PFindMaximumParallelData = ^TFindMaximumParallelData;

procedure FindMaximumParallel(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
var
  Params: PFindMaximumParallelData absolute Data;
  LeftParams, RightParams: TFindMaximumParallelData;
begin
  if Params^.Left+1000>Params^.Right then begin
    // compute the maximum of the few remaining items
    Params^.LeftMaxIndex:=Params^.Items[Params^.Left];
    for i:=Params^.Left+1 to Params^.Right do
      if Params^.Items[i]>Params^.LeftMaxIndex then
  end else begin

  end;
end;

function FindMaximumIndex(Items: TArrayOfInteger): integer;
begin

end;

begin
  SetLength(Items,10000000);
  for i:=0 to length(Items)-1 do Items[i]:=Random(1000);
  ProcThreadPool.DoParallel(@DoSomethingParallel,1,5,nil); // address, startindex, endindex, optional data
end.

