program SimpleMTP1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  MTProcs;

// a simple parallel procedure
procedure DoSomethingParallel(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
var
  i: Integer;
begin
  writeln(Index);
  for i:=1 to Index*1000000 do ; // do some work
end;

begin
  ProcThreadPool.DoParallel(@DoSomethingParallel,1,5,nil); // address, startindex, endindex, optional data
end.

