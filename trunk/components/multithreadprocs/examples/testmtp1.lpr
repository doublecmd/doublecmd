program TestMTP1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Math, SysUtils, Classes, MTProcs, MTPUtils, MultiThreadProcsLaz;

type

  { TTestItem }

  TTestItem = class
  private
    FIndex: int64;
  public
    property Index: int64 read FIndex;
    constructor Create(NewIndex: int64);
  end;

  { TTests }

  TTests = class
  public
    procedure Work(Seconds: integer);

    // RTLeventSetEvent, RTLeventWaitFor
    procedure TestRTLevent_Set_WaitFor;

    // single thread test
    procedure TestSingleThread;
    procedure MTPLoop_TestSingleThread(Index: PtrInt; Data: Pointer;
                                       Item: TMultiThreadProcItem);

    // two threads test: run once
    procedure TestTwoThreads1;
    procedure MTPLoop_TestTwoThreads1(Index: PtrInt; Data: Pointer;
                                      Item: TMultiThreadProcItem);

    // 0 runs two seconds,
    // 1 runs a second then waits for 0 then runs a second
    // 2 runs a second then waits for 1
    // 3 waits for 0
    // 4 waits for 1
    // 5 waits for 2
    procedure TestMTPWaitForIndex;
    procedure MTPLoop_TestMTPWaitForIndex(Index: PtrInt; Data: Pointer;
                                          Item: TMultiThreadProcItem);

    // two threads test: various run times
    procedure TestMTPTwoThreads2;
    procedure MTPLoop_TestTwoThreads2(Index: PtrInt; Data: Pointer;
                                      Item: TMultiThreadProcItem);

    // test exception in starter thread
    procedure TestMTPExceptionInStarterThread;
    procedure MTPLoop_TestExceptionInStarterThread(Index: PtrInt; Data: Pointer;
                                                   Item: TMultiThreadProcItem);

    // test exception in helper thread
    procedure TestMTPExceptionInHelperThread;
    procedure MTPLoop_TestExceptionInHelperThread(Index: PtrInt; Data: Pointer;
                                                  Item: TMultiThreadProcItem);

    // test parallel sort
    procedure TestMTPSort;
    procedure MTPLoop_TestDoubleMTPSort(Index: PtrInt; Data: Pointer;
                                        Item: TMultiThreadProcItem);
  end;

{ TTestItem }

constructor TTestItem.Create(NewIndex: int64);
begin
  FIndex:=NewIndex;
end;

{ TTests }

procedure TTests.Work(Seconds: integer);
var
  Start: TDateTime;
begin
  Start:=Now;
  while (Now-Start)*86400<Seconds do if GetCurrentDir='' then ;
end;

procedure TTests.TestRTLevent_Set_WaitFor;
var
  e: PRTLEvent;
begin
  e:=RTLEventCreate;
  RTLeventSetEvent(e);
  RTLeventWaitFor(e);
  RTLeventdestroy(e);
end;

procedure TTests.TestSingleThread;
begin
  ProcThreadPool.DoParallel(@MTPLoop_TestSingleThread,1,3,nil,1);
end;

procedure TTests.MTPLoop_TestSingleThread(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
begin
  writeln('TTests.MTPLoop_TestSingleThread Index=',Index);
end;

procedure TTests.TestTwoThreads1;
begin
  WriteLn('TTests.TestTwoThreads1 START');
  ProcThreadPool.DoParallel(@MTPLoop_TestTwoThreads1,1,2,nil,2);
  WriteLn('TTests.TestTwoThreads1 END');
end;

procedure TTests.MTPLoop_TestTwoThreads1(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
var
  i: Integer;
begin
  for i:=1 to 3 do begin
    WriteLn('TTests.MTPLoop_TestTwoThreads1 Index=',Index,' ',i);
    Work(1);
  end;
end;

procedure TTests.TestMTPWaitForIndex;
var
  IndexStates: PInteger;
begin
  ProcThreadPool.MaxThreadCount:=8;
  IndexStates:=nil;
  GetMem(IndexStates,SizeOf(Integer)*10);
  FillByte(IndexStates^,SizeOf(Integer)*10,0);
  WriteLn('TTests.TestMTPWaitForIndex START');
  ProcThreadPool.DoParallel(@MTPLoop_TestMTPWaitForIndex,0,5,IndexStates);
  FreeMem(IndexStates);
  WriteLn('TTests.TestMTPWaitForIndex END');
end;

procedure TTests.MTPLoop_TestMTPWaitForIndex(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
    // 0 runs two seconds,
    // 1 runs a second then waits for 0 then runs a second
    // 2 runs a second then waits for 1
    // 3 waits for 0
    // 4 waits for 1
    // 5 waits for 2

  procedure WaitFor(OtherIndex: PtrInt);
  begin
    WriteLn('TTests.MTPLoop_TestMTPWaitForIndex Index='+IntToStr(Index)+' waiting for '+IntToStr(OtherIndex)+' ...');
    Item.WaitForIndex(OtherIndex);
    WriteLn('TTests.MTPLoop_TestMTPWaitForIndex Index='+IntToStr(Index)+' waited for '+IntToStr(OtherIndex)+'. working ...');
    if PInteger(Data)[OtherIndex]<>2 then begin
      WriteLn('TTests.MTPLoop_TestMTPWaitForIndex Index='+IntToStr(Index)+' ERROR: waited for '+IntToStr(OtherIndex)+' failed: OtherState='+IntToStr(PInteger(Data)[OtherIndex]));
    end;
  end;

begin
  WriteLn('TTests.MTPLoop_TestMTPWaitForIndex Index='+IntToStr(Index)+' START');
  if PInteger(Data)[Index]<>0 then begin
    WriteLn('TTests.MTPLoop_TestMTPWaitForIndex Index='+IntToStr(Index)+' ERROR: IndexState='+IntToStr(PInteger(Data)[Index]));
  end;
  PInteger(Data)[Index]:=1;
  case Index of
  0: Work(2);
  1:begin
      Work(1);
      WaitFor(0);
      Work(1);
    end;
  2:begin
      Work(1);
      WaitFor(1);
    end;
  3:begin
      WaitFor(0);
    end;
  4:begin
      WaitFor(1);
    end;
  5:begin
      WaitFor(2);
    end;
  end;
  WriteLn('TTests.MTPLoop_TestMTPWaitForIndex Index='+IntToStr(Index)+' END');
  PInteger(Data)[Index]:=2;
end;

procedure TTests.TestMTPTwoThreads2;
begin
  WriteLn('TTests.TestMTPTwoThreads1 START');
  ProcThreadPool.DoParallel(@MTPLoop_TestTwoThreads2,1,6,nil,2);
  WriteLn('TTests.TestMTPTwoThreads1 END');
end;

procedure TTests.MTPLoop_TestTwoThreads2(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
var
  i: Integer;
begin
  for i:=1 to (Index mod 3)+1 do begin
    WriteLn('TTests.MTPLoop_TestTwoThreads1 Index=',Index,' i=',i,' ID=',PtrUint(GetThreadID));
    Work(1);
  end;
end;

type
  TMyException = class(Exception);

procedure TTests.TestMTPExceptionInStarterThread;
var
  IndexStates: PInteger;
begin
  WriteLn('TTests.TestMTPExceptionInStarterThread START');
  ProcThreadPool.MaxThreadCount:=8;
  IndexStates:=nil;
  GetMem(IndexStates,SizeOf(Integer)*10);
  FillByte(IndexStates^,SizeOf(Integer)*10,0);
  try
    ProcThreadPool.DoParallel(@MTPLoop_TestExceptionInStarterThread,1,3,IndexStates,2);
  except
    on E: Exception do begin
      WriteLn('TTests.TestMTPExceptionInHelperThread E.ClassName=',E.ClassName,' E.Message=',E.Message);
    end;
  end;
  FreeMem(IndexStates);
  WriteLn('TTests.TestMTPExceptionInStarterThread END');
end;

procedure TTests.MTPLoop_TestExceptionInStarterThread(Index: PtrInt;
  Data: Pointer; Item: TMultiThreadProcItem);
begin
  WriteLn('TTests.MTPLoop_TestExceptionInStarterThread START Index='+IntToStr(Index));
  if PInteger(Data)[Index]<>0 then
    WriteLn('TTests.MTPLoop_TestExceptionInStarterThread Index='+IntToStr(Index)+' ERROR: IndexState='+IntToStr(PInteger(Data)[Index]));
  PInteger(Data)[Index]:=1;
  case Index of
  1:
    begin
      // Main Thread
      Work(1);
      WriteLn('TTests.MTPLoop_TestExceptionInStarterThread raising exception in Index='+IntToStr(Index)+' ...');
      raise Exception.Create('Exception in starter thread');
    end;
  else
    Work(Index);
  end;
  PInteger(Data)[Index]:=2;
  WriteLn('TTests.MTPLoop_TestExceptionInStarterThread END Index='+IntToStr(Index));
end;

procedure TTests.TestMTPExceptionInHelperThread;
var
  IndexStates: PInteger;
begin
  WriteLn('TTests.TestMTPExceptionInHelperThread START');
  ProcThreadPool.MaxThreadCount:=8;
  IndexStates:=nil;
  GetMem(IndexStates,SizeOf(Integer)*10);
  FillByte(IndexStates^,SizeOf(Integer)*10,0);
  try
    ProcThreadPool.DoParallel(@MTPLoop_TestExceptionInHelperThread,1,3,IndexStates,2);
  except
    on E: Exception do begin
      WriteLn('TTests.TestMTPExceptionInHelperThread E.ClassName=',E.ClassName,' E.Message=',E.Message);
    end;
  end;
  FreeMem(IndexStates);
  WriteLn('TTests.TestMTPExceptionInHelperThread END');
end;

procedure TTests.MTPLoop_TestExceptionInHelperThread(Index: PtrInt;
  Data: Pointer; Item: TMultiThreadProcItem);
begin
  WriteLn('TTests.MTPLoop_TestExceptionInHelperThread START Index='+IntToStr(Index));
  if PInteger(Data)[Index]<>0 then
    WriteLn('TTests.MTPLoop_TestExceptionInHelperThread Index='+IntToStr(Index)+' ERROR: IndexState='+IntToStr(PInteger(Data)[Index]));
  PInteger(Data)[Index]:=1;
  case Index of
  2:
    begin
      // Helper Thread 2
      Work(1);
      WriteLn('TTests.MTPLoop_TestExceptionInHelperThread raising exception in Index='+IntToStr(Index)+' ...');
      raise TMyException.Create('Exception in helper thread');
    end;
  else
    Work(Index+1);
  end;
  PInteger(Data)[Index]:=2;
  WriteLn('TTests.MTPLoop_TestExceptionInHelperThread END Index='+IntToStr(Index));
end;

function CompareTestItems(Data1, Data2: Pointer): integer;
begin
  if TTestItem(Data1).Index>TTestItem(Data2).Index then
    Result:=1
  else if TTestItem(Data1).Index<TTestItem(Data2).Index then
    Result:=-1
  else
    Result:=0;
end;

procedure TTests.TestMTPSort;
var
  OuterLoop: Integer;
  InnerLoop: Integer;
begin
  OuterLoop:=1;
  InnerLoop:=0;
  if Paramcount=1 then begin
    InnerLoop:=StrToInt(ParamStr(1));
  end else if Paramcount=2 then begin
    OuterLoop:=StrToInt(ParamStr(1));
    InnerLoop:=StrToInt(ParamStr(2));
  end;
  writeln('TTests.TestMTPSort Running ',OuterLoop,'x',InnerLoop);
  ProcThreadPool.DoParallel(@MTPLoop_TestDoubleMTPSort,1,OuterLoop,@InnerLoop);
end;

procedure TTests.MTPLoop_TestDoubleMTPSort(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem);
var
  i: Integer;
  List: TFPList;
  t: double;
begin
  // create an unsorted list of values
  List:=TFPList.Create;
  for i:=1 to 10000000 do List.Add(TTestItem.Create(Random(99999999999)));
  //QuickSort(List,0,List.Count-1,@AnsiCompareText);

  t:=Now;
  ParallelSortFPList(List,@CompareTestItems,PInteger(Data)^);
  t:=Now-t;
  writeln('TTests.TestMTPSort ',t*86400);
  // check
  sleep(1);
  for i:=0 to List.Count-2 do
    if CompareTestItems(List[i],List[i+1])>0 then raise Exception.Create('not sorted');

  for i:=0 to List.Count-1 do
    TObject(List[i]).Free;
  List.Free;
end;

var
  Tests: TTests;
begin
  writeln('threads=',ProcThreadPool.MaxThreadCount);
  ProcThreadPool.MaxThreadCount:=8;
  Tests:=TTests.Create;
  //Tests.Test1;
  //Tests.Test2;
  //Tests.TestTwoThreads2;
  //Tests.TestRTLevent_Set_WaitFor;
  //Tests.TestMTPWaitForIndex;
  //Tests.TestMTPExceptionInStarterThread;
  Tests.TestMTPExceptionInHelperThread;
  //Tests.TestMTPSort;
  Tests.Free;
end.

