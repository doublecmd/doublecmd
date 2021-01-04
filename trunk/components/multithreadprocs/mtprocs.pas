{
 **********************************************************************
  This file is part of the Free Pascal run time library.

  See the file COPYING.FPC, included in this distribution,
  for details about the license.
 **********************************************************************

  Unit for light weight threads.

  Copyright (C) 2008 Mattias Gaertner mattias@freepascal.org

  Abstract:
    Light weight threads.
    This unit provides methods to easily run a procedure/method with several
    threads at once.
}
unit MTProcs;

{$mode objfpc}{$H+}

{$inline on}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, MTPCPU;

type
  TProcThreadGroup = class;
  TProcThreadPool = class;
  TProcThread = class;

  { TMultiThreadProcItem }

  TMTPThreadState = (
    mtptsNone,
    mtptsActive,
    mtptsWaitingForIndex,
    mtptsWaitingFailed,
    mtptsInactive,
    mtptsTerminated
    );

  TMultiThreadProcItem = class
  private
    FGroup: TProcThreadGroup;
    FIndex: PtrInt;
    FThread: TProcThread;
    FWaitingForIndexEnd: PtrInt;
    FWaitingForIndexStart: PtrInt;
    fWaitForPool: PRTLEvent;
    FState: TMTPThreadState;
  public
    destructor Destroy; override;
    function WaitForIndexRange(StartIndex, EndIndex: PtrInt): boolean;
    function WaitForIndex(Index: PtrInt): boolean; inline;
    procedure CalcBlock(Index, BlockSize, LoopLength: PtrInt;
                        out BlockStart, BlockEnd: PtrInt); inline;
    property Index: PtrInt read FIndex;
    property Group: TProcThreadGroup read FGroup;
    property WaitingForIndexStart: PtrInt read FWaitingForIndexStart;
    property WaitingForIndexEnd: PtrInt read FWaitingForIndexEnd;
    property Thread: TProcThread read FThread;
  end;

  { TProcThread }

  TMTPThreadList = (
    mtptlPool,
    mtptlGroup
    );

  TProcThread = class(TThread)
  private
    FItem: TMultiThreadProcItem;
    FNext, FPrev: array[TMTPThreadList] of TProcThread;
    procedure AddToList(var First: TProcThread; ListType: TMTPThreadList); inline;
    procedure RemoveFromList(var First: TProcThread; ListType: TMTPThreadList); inline;
    procedure Terminating(aPool: TProcThreadPool; E: Exception);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    property Item: TMultiThreadProcItem read FItem;
  end;

  TMTMethod = procedure(Index: PtrInt; Data: Pointer;
                        Item: TMultiThreadProcItem) of object;
  TMTProcedure = procedure(Index: PtrInt; Data: Pointer;
                           Item: TMultiThreadProcItem);
  TMTNestedProcedure = procedure(Index: PtrInt; Data: Pointer;
                           Item: TMultiThreadProcItem) is nested;

  { TProcThreadGroup
    Each task creates a new group of threads.
    A group can either need more threads or it has finished and waits for its
    threads to end.
    The thread that created the group is not in the list FFirstThread. }

  TMTPGroupState = (
    mtpgsNone,
    mtpgsNeedThreads, // the groups waiting for more threads to help
    mtpgsFinishing,   // the groups waiting for its threads to finish
    mtpgsException    // there was an exception => close asap
    );

  TProcThreadGroup = class
  private
    FEndIndex: PtrInt;
    FException: Exception;
    FFirstRunningIndex: PtrInt;
    FFirstThread: TProcThread;
    FLastRunningIndex: PtrInt;
    FMaxThreads: PtrInt;
    FNext, FPrev: TProcThreadGroup;
    FPool: TProcThreadPool;
    FStarterItem: TMultiThreadProcItem;
    FStartIndex: PtrInt;
    FState: TMTPGroupState;
    FTaskData: Pointer;
    FTaskFrame: Pointer;
    FTaskMethod: TMTMethod;
    FTaskNested: TMTNestedProcedure;
    FTaskProcedure: TMTProcedure;
    FThreadCount: PtrInt;
    procedure AddToList(var First: TProcThreadGroup; ListType: TMTPGroupState); inline;
    procedure RemoveFromList(var First: TProcThreadGroup); inline;
    function NeedMoreThreads: boolean; inline;
    procedure IncreaseLastRunningIndex(Item: TMultiThreadProcItem);
    procedure AddThread(AThread: TProcThread);
    procedure RemoveThread(AThread: TProcThread); inline;
    procedure Run(Index: PtrInt; Data: Pointer; Item: TMultiThreadProcItem); inline;
    procedure IndexComplete(Index: PtrInt);
    procedure WakeThreadsWaitingForIndex;
    function HasFinishedIndex(aStartIndex, aEndIndex: PtrInt): boolean;
    procedure EnterExceptionState(E: Exception);
  public
    constructor Create;
    destructor Destroy; override;
    property Pool: TProcThreadPool read FPool;
    property StartIndex: PtrInt read FStartIndex;
    property EndIndex: PtrInt read FEndIndex;
    property FirstRunningIndex: PtrInt read FFirstRunningIndex; // first started
    property LastRunningIndex: PtrInt read FLastRunningIndex; // last started
    property TaskData: Pointer read FTaskData;
    property TaskMethod: TMTMethod read FTaskMethod;
    property TaskNested: TMTNestedProcedure read FTaskNested;
    property TaskProcedure: TMTProcedure read FTaskProcedure;
    property TaskFrame: Pointer read FTaskFrame;
    property MaxThreads: PtrInt read FMaxThreads;
    property StarterItem: TMultiThreadProcItem read FStarterItem;
  end;

  { TLightWeightThreadPool
    Group 0 are the inactive threads }

  { TProcThreadPool }

  TProcThreadPool = class
  private
    FMaxThreadCount: PtrInt;
    FThreadCount: PtrInt;
    FFirstInactiveThread: TProcThread;
    FFirstActiveThread: TProcThread;
    FFirstTerminatedThread: TProcThread;
    FFirstGroupNeedThreads: TProcThreadGroup;
    FFirstGroupFinishing: TProcThreadGroup;
    FCritSection: TRTLCriticalSection;
    FDestroying: boolean;
    procedure SetMaxThreadCount(const AValue: PtrInt);
    procedure CleanTerminatedThreads;
    procedure DoParallelIntern(const AMethod: TMTMethod;
      const AProc: TMTProcedure; const ANested: TMTNestedProcedure;
      const AFrame: Pointer; StartIndex, EndIndex: PtrInt;
      Data: Pointer = nil; MaxThreads: PtrInt = 0);
  public
    // for debugging only: the critical section is public:
    procedure EnterPoolCriticalSection; inline;
    procedure LeavePoolCriticalSection; inline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoParallel(const AMethod: TMTMethod;
      StartIndex, EndIndex: PtrInt;
      Data: Pointer = nil; MaxThreads: PtrInt = 0); inline;
    procedure DoParallel(const AProc: TMTProcedure;
      StartIndex, EndIndex: PtrInt;
      Data: Pointer = nil; MaxThreads: PtrInt = 0); inline;
    procedure DoParallelNested(const ANested: TMTNestedProcedure;
      StartIndex, EndIndex: PtrInt;
      Data: Pointer = nil; MaxThreads: PtrInt = 0); inline;

    // experimental
    procedure DoParallelLocalProc(const LocalProc: Pointer;
      StartIndex, EndIndex: PtrInt;
      Data: Pointer = nil; MaxThreads: PtrInt = 0); // do not make this inline!

    // utility functions for loops:
    procedure CalcBlockSize(LoopLength: PtrInt;
      out BlockCount, BlockSize: PtrInt; MinBlockSize: PtrInt = 0); inline;
  public
    property MaxThreadCount: PtrInt read FMaxThreadCount write SetMaxThreadCount;
    property ThreadCount: PtrInt read FThreadCount;
  end;

var
  ProcThreadPool: TProcThreadPool = nil;

threadvar
  CurrentThread: TThread; // TProcThread sets this, you can set this for your own TThreads descendants

implementation

{ TMultiThreadProcItem }

destructor TMultiThreadProcItem.Destroy;
begin
  if fWaitForPool<>nil then begin
    RTLeventdestroy(fWaitForPool);
    fWaitForPool:=nil;
  end;
  inherited Destroy;
end;

function TMultiThreadProcItem.WaitForIndexRange(
  StartIndex, EndIndex: PtrInt): boolean;
var
  aPool: TProcThreadPool;
begin
  //WriteLn('TLightWeightThreadItem.WaitForIndexRange START Index='+IntToStr(Index)+' StartIndex='+IntToStr(StartIndex)+' EndIndex='+IntToStr(EndIndex));
  if (EndIndex>=Index) then exit(false);
  if EndIndex<StartIndex then exit(true);
  if Group=nil then exit(true); // a single threaded group has no group object
  // multi threaded group
  aPool:=Group.Pool;
  if aPool.FDestroying then exit(false); // no more wait allowed
  aPool.EnterPoolCriticalSection;
  try
    if Group.FState=mtpgsException then begin
      //WriteLn('TLightWeightThreadItem.WaitForIndexRange Index='+IntToStr(Index)+', Group closing because of error');
      exit(false);
    end;
    if Group.HasFinishedIndex(StartIndex,EndIndex) then begin
      //WriteLn('TLightWeightThreadItem.WaitForIndexRange Index='+IntToStr(Index)+', range already finished');
      exit(true);
    end;
    FState:=mtptsWaitingForIndex;
    FWaitingForIndexStart:=StartIndex;
    FWaitingForIndexEnd:=EndIndex;
    if fWaitForPool=nil then
      fWaitForPool:=RTLEventCreate;
    RTLeventResetEvent(fWaitForPool);
  finally
    aPool.LeavePoolCriticalSection;
  end;
  //WriteLn('TLightWeightThreadItem.WaitForIndexRange '+IntToStr(Index)+' waiting ... ');
  RTLeventWaitFor(fWaitForPool);
  Result:=FState=mtptsActive;
  FState:=mtptsActive;
  //WriteLn('TLightWeightThreadItem.WaitForIndexRange END '+IntToStr(Index));
end;

function TMultiThreadProcItem.WaitForIndex(Index: PtrInt): boolean; inline;
begin
  Result:=WaitForIndexRange(Index,Index);
end;

procedure TMultiThreadProcItem.CalcBlock(Index, BlockSize, LoopLength: PtrInt;
  out BlockStart, BlockEnd: PtrInt);
begin
  BlockStart:=BlockSize*Index;
  BlockEnd:=BlockStart+BlockSize;
  if LoopLength<BlockEnd then BlockEnd:=LoopLength;
  dec(BlockEnd);
end;

{ TProcThread }

procedure TProcThread.AddToList(var First: TProcThread;
  ListType: TMTPThreadList);
begin
  FNext[ListType]:=First;
  if FNext[ListType]<>nil then
    FNext[ListType].FPrev[ListType]:=Self;
  First:=Self;
end;

procedure TProcThread.RemoveFromList(var First: TProcThread;
  ListType: TMTPThreadList);
begin
  if First=Self then
    First:=FNext[ListType];
  if FNext[ListType]<>nil then
    FNext[ListType].FPrev[ListType]:=FPrev[ListType];
  if FPrev[ListType]<>nil then
    FPrev[ListType].FNext[ListType]:=FNext[ListType];
  FNext[ListType]:=nil;
  FPrev[ListType]:=nil;
end;

procedure TProcThread.Terminating(aPool: TProcThreadPool;
  E: Exception);
begin
  aPool.EnterPoolCriticalSection;
  try
    // remove from group
    if Item.FGroup<>nil then begin
      // an exception occured
      Item.FGroup.EnterExceptionState(E);
      Item.FGroup.RemoveThread(Self);
      Item.FGroup:=nil;
    end;
    // move to pool's terminated threads
    case Item.FState of
    mtptsActive:   RemoveFromList(aPool.FFirstActiveThread,mtptlPool);
    mtptsInactive: RemoveFromList(aPool.FFirstInactiveThread,mtptlPool);
    end;
    AddToList(aPool.FFirstTerminatedThread,mtptlPool);
    Item.FState:=mtptsTerminated;
  finally
    aPool.LeavePoolCriticalSection;
  end;
end;

constructor TProcThread.Create;
begin
  inherited Create(true);
  fItem:=TMultiThreadProcItem.Create;
  fItem.fWaitForPool:=RTLEventCreate;
  fItem.FThread:=Self;
end;

destructor TProcThread.Destroy;
begin
  FreeAndNil(FItem);
  inherited Destroy;
end;

procedure TProcThread.Execute;
var
  aPool: TProcThreadPool;
  Group: TProcThreadGroup;
  ok: Boolean;
  E: Exception;
begin
  MTProcs.CurrentThread:=Self;
  aPool:=Item.Group.Pool;
  ok:=false;
  try
    repeat
      // work
      Group:=Item.Group;
      Group.Run(Item.Index,Group.TaskData,Item);

      aPool.EnterPoolCriticalSection;
      try
        Group.IndexComplete(Item.Index);

        // find next work
        if Group.LastRunningIndex<Group.EndIndex then begin
          // next index of group
          Group.IncreaseLastRunningIndex(Item);
        end else begin
          // remove from group
          RemoveFromList(Group.FFirstThread,mtptlGroup);
          dec(Group.FThreadCount);
          Item.FGroup:=nil;
          Group:=nil;
          if aPool.FFirstGroupNeedThreads<>nil then begin
            // add to new group
            aPool.FFirstGroupNeedThreads.AddThread(Self);
            Group:=Item.Group;
          end else begin
            // mark inactive
            RemoveFromList(aPool.FFirstActiveThread,mtptlPool);
            AddToList(aPool.FFirstInactiveThread,mtptlPool);
            Item.FState:=mtptsInactive;
            RTLeventResetEvent(Item.fWaitForPool);
          end;
        end;
      finally
        aPool.LeavePoolCriticalSection;
      end;
      // wait for new work
      if Item.FState=mtptsInactive then
        RTLeventWaitFor(Item.fWaitForPool);
    until Item.Group=nil;
    ok:=true;
  except
    // stop the exception and store it
    E:=Exception(AcquireExceptionObject);
    Terminating(aPool,E);
  end;
  if ok then
    Terminating(aPool,nil);
end;

{ TProcThreadGroup }

procedure TProcThreadGroup.AddToList(var First: TProcThreadGroup;
  ListType: TMTPGroupState);
begin
  FNext:=First;
  if FNext<>nil then
    FNext.FPrev:=Self;
  First:=Self;
  FState:=ListType;
end;

procedure TProcThreadGroup.RemoveFromList(
  var First: TProcThreadGroup);
begin
  if First=Self then
    First:=FNext;
  if FNext<>nil then
    FNext.FPrev:=FPrev;
  if FPrev<>nil then
    FPrev.FNext:=FNext;
  FNext:=nil;
  FPrev:=nil;
  FState:=mtpgsNone;
end;

function TProcThreadGroup.NeedMoreThreads: boolean;
begin
  Result:=(FLastRunningIndex<FEndIndex) and (FThreadCount<FMaxThreads)
      and (FState<>mtpgsException);
end;

procedure TProcThreadGroup.IncreaseLastRunningIndex(Item: TMultiThreadProcItem);
begin
  inc(FLastRunningIndex);
  Item.FIndex:=FLastRunningIndex;
  if NeedMoreThreads then exit;
  if FState=mtpgsNeedThreads then begin
    RemoveFromList(Pool.FFirstGroupNeedThreads);
    AddToList(Pool.FFirstGroupFinishing,mtpgsFinishing);
  end;
end;

procedure TProcThreadGroup.AddThread(AThread: TProcThread);
begin
  AThread.Item.FGroup:=Self;
  AThread.AddToList(FFirstThread,mtptlGroup);
  inc(FThreadCount);
  IncreaseLastRunningIndex(AThread.Item);
end;

procedure TProcThreadGroup.RemoveThread(AThread: TProcThread);
begin
  AThread.RemoveFromList(FFirstThread,mtptlGroup);
  dec(FThreadCount);
end;

procedure TProcThreadGroup.Run(Index: PtrInt; Data: Pointer;
  Item: TMultiThreadProcItem); inline;
begin
  if Assigned(FTaskFrame) then
    CallLocalProc(FTaskProcedure,FTaskFrame,Index,Data,Item)
  else if Assigned(FTaskProcedure) then
    FTaskProcedure(Index,Data,Item)
  else if Assigned(FTaskNested) then
    FTaskNested(Index,Data,Item)
  else
    FTaskMethod(Index,Data,Item);
end;

procedure TProcThreadGroup.IndexComplete(Index: PtrInt);
var
  AThread: TProcThread;
  NewFirstRunningThread: PtrInt;
begin
  // update FirstRunningIndex
  NewFirstRunningThread:=FStarterItem.Index;
  AThread:=FFirstThread;
  while AThread<>nil do begin
    if (NewFirstRunningThread>aThread.Item.Index)
    and (aThread.Item.Index<>Index) then
      NewFirstRunningThread:=aThread.Item.Index;
    aThread:=aThread.FNext[mtptlGroup];
  end;
  FFirstRunningIndex:=NewFirstRunningThread;
  // wake up threads (Note: do this even if FFirstRunningIndex has not changed)
  WakeThreadsWaitingForIndex;
end;

procedure TProcThreadGroup.WakeThreadsWaitingForIndex;
var
  aThread: TProcThread;
begin
  if FState<>mtpgsException then begin
    // wake up waiting threads
    aThread:=FFirstThread;
    while aThread<>nil do begin
      if (aThread.Item.FState=mtptsWaitingForIndex)
      and HasFinishedIndex(aThread.Item.WaitingForIndexStart,
                           aThread.Item.WaitingForIndexEnd)
      then begin
        // wake up the thread
        aThread.Item.FState:=mtptsActive;
        RTLeventSetEvent(aThread.Item.fWaitForPool);
      end;
      aThread:=aThread.FNext[mtptlGroup];
    end;
    if (FStarterItem.FState=mtptsWaitingForIndex)
    and HasFinishedIndex(FStarterItem.WaitingForIndexStart,FStarterItem.WaitingForIndexEnd)
    then begin
      // wake up the starter thread of this group
      FStarterItem.FState:=mtptsActive;
      RTLeventSetEvent(FStarterItem.fWaitForPool);
    end;
  end else begin
    // end group: wake up waiting threads
    aThread:=FFirstThread;
    while aThread<>nil do begin
      if (aThread.Item.FState=mtptsWaitingForIndex)
      then begin
        // end group: wake up the thread
        aThread.Item.FState:=mtptsWaitingFailed;
        RTLeventSetEvent(aThread.Item.fWaitForPool);
      end;
      aThread:=aThread.FNext[mtptlGroup];
    end;
    if (FStarterItem.FState=mtptsWaitingForIndex)
    then begin
      // end group: wake up the starter thread of this group
      FStarterItem.FState:=mtptsWaitingFailed;
      RTLeventSetEvent(FStarterItem.fWaitForPool);
    end;
  end;
end;

function TProcThreadGroup.HasFinishedIndex(
  aStartIndex, aEndIndex: PtrInt): boolean;
var
  AThread: TProcThread;
begin
  // test the finished range
  if FFirstRunningIndex>aEndIndex then exit(true);
  // test the unfinished range
  if FLastRunningIndex<aEndIndex then exit(false);
  // test the active range
  AThread:=FFirstThread;
  while AThread<>nil do begin
    if (AThread.Item.Index>=aStartIndex)
    and (AThread.Item.Index<=aEndIndex) then
      exit(false);
    AThread:=AThread.FNext[mtptlGroup];
  end;
  if (FStarterItem.Index>=aStartIndex)
  and (FStarterItem.Index<=aEndIndex) then
    exit(false);
  Result:=true;
end;

procedure TProcThreadGroup.EnterExceptionState(E: Exception);
begin
  if FState=mtpgsException then exit;
  case FState of
  mtpgsFinishing: RemoveFromList(Pool.FFirstGroupFinishing);
  mtpgsNeedThreads: RemoveFromList(Pool.FFirstGroupNeedThreads);
  end;
  FState:=mtpgsException;
  FException:=E;
  WakeThreadsWaitingForIndex;
end;

constructor TProcThreadGroup.Create;
begin
  FStarterItem:=TMultiThreadProcItem.Create;
  FStarterItem.FGroup:=Self;
end;

destructor TProcThreadGroup.Destroy;
begin
  FreeAndNil(FStarterItem);
  inherited Destroy;
end;

{ TProcThreadPool }

procedure TProcThreadPool.SetMaxThreadCount(const AValue: PtrInt);
begin
  if FMaxThreadCount=AValue then exit;
  if AValue<1 then raise Exception.Create('TLightWeightThreadPool.SetMaxThreadCount');
  FMaxThreadCount:=AValue;
end;

procedure TProcThreadPool.CleanTerminatedThreads;
var
  AThread: TProcThread;
begin
  while FFirstTerminatedThread<>nil do begin
    AThread:=FFirstTerminatedThread;
    AThread.RemoveFromList(FFirstTerminatedThread,mtptlPool);
    AThread.Free;
  end;
end;

constructor TProcThreadPool.Create;
begin
  FMaxThreadCount:=GetSystemThreadCount;
  if FMaxThreadCount<1 then
    FMaxThreadCount:=1;
  InitCriticalSection(FCritSection);
end;

destructor TProcThreadPool.Destroy;

  procedure WakeWaitingStarterItems(Group: TProcThreadGroup);
  begin
    while Group<>nil do begin
      if Group.StarterItem.FState=mtptsWaitingForIndex then begin
        Group.StarterItem.FState:=mtptsWaitingFailed;
        RTLeventSetEvent(Group.StarterItem.fWaitForPool);
      end;
      Group:=Group.FNext;
    end;
  end;

var
  AThread: TProcThread;
begin
  FDestroying:=true;
  // wake up all waiting threads
  EnterPoolCriticalSection;
  try
    AThread:=FFirstActiveThread;
    while AThread<>nil do begin
      if aThread.Item.FState=mtptsWaitingForIndex then begin
        aThread.Item.FState:=mtptsWaitingFailed;
        RTLeventSetEvent(AThread.Item.fWaitForPool);
      end;
      AThread:=AThread.FNext[mtptlPool];
    end;
    WakeWaitingStarterItems(FFirstGroupNeedThreads);
    WakeWaitingStarterItems(FFirstGroupFinishing);
  finally
    LeavePoolCriticalSection;
  end;

  // wait for all active threads to become inactive
  while FFirstActiveThread<>nil do
    Sleep(10);

  // wake up all inactive threads (without new work they will terminate)
  EnterPoolCriticalSection;
  try
    AThread:=FFirstInactiveThread;
    while AThread<>nil do begin
      RTLeventSetEvent(AThread.Item.fWaitForPool);
      AThread:=AThread.FNext[mtptlPool];
    end;
  finally
    LeavePoolCriticalSection;
  end;

  // wait for all threads to terminate
  while FFirstInactiveThread<>nil do
    Sleep(10);

  // free threads
  CleanTerminatedThreads;

  DoneCriticalsection(FCritSection);
  inherited Destroy;
end;

procedure TProcThreadPool.EnterPoolCriticalSection;
begin
  EnterCriticalsection(FCritSection);
end;

procedure TProcThreadPool.LeavePoolCriticalSection;
begin
  LeaveCriticalsection(FCritSection);
end;

procedure TProcThreadPool.DoParallel(const AMethod: TMTMethod;
  StartIndex, EndIndex: PtrInt; Data: Pointer; MaxThreads: PtrInt);
begin
  if not Assigned(AMethod) then exit;
  DoParallelIntern(AMethod,nil,nil,nil,StartIndex,EndIndex,Data,MaxThreads);
end;

procedure TProcThreadPool.DoParallel(const AProc: TMTProcedure;
  StartIndex, EndIndex: PtrInt; Data: Pointer; MaxThreads: PtrInt);
begin
  if not Assigned(AProc) then exit;
  DoParallelIntern(nil,AProc,nil,nil,StartIndex,EndIndex,Data,MaxThreads);
end;

procedure TProcThreadPool.DoParallelNested(const ANested: TMTNestedProcedure;
  StartIndex, EndIndex: PtrInt; Data: Pointer; MaxThreads: PtrInt);
begin
  if not Assigned(ANested) then exit;
  DoParallelIntern(nil,nil,ANested,nil,StartIndex,EndIndex,Data,MaxThreads);
end;

procedure TProcThreadPool.DoParallelLocalProc(const LocalProc: Pointer;
  StartIndex, EndIndex: PtrInt; Data: Pointer; MaxThreads: PtrInt);
var
  Frame: Pointer;
begin
  if not Assigned(LocalProc) then exit;
  Frame:=get_caller_frame(get_frame);
  DoParallelIntern(nil,TMTProcedure(LocalProc),nil,Frame,StartIndex,EndIndex,
                   Data,MaxThreads);
end;

procedure TProcThreadPool.CalcBlockSize(LoopLength: PtrInt; out BlockCount,
  BlockSize: PtrInt; MinBlockSize: PtrInt);
begin
  if LoopLength<=0 then begin
    BlockCount:=0;
    BlockSize:=1;
    exit;
  end;
  // split work into equally sized blocks
  BlockCount:=ProcThreadPool.MaxThreadCount;
  BlockSize:=(LoopLength div BlockCount);
  if (BlockSize<MinBlockSize) then BlockSize:=MinBlockSize;
  if BlockSize<1 then BlockSize:=1;
  BlockCount:=((LoopLength-1) div BlockSize)+1;
end;

procedure TProcThreadPool.DoParallelIntern(const AMethod: TMTMethod;
  const AProc: TMTProcedure; const ANested: TMTNestedProcedure;
  const AFrame: Pointer; StartIndex, EndIndex: PtrInt; Data: Pointer;
  MaxThreads: PtrInt);
var
  Group: TProcThreadGroup;
  Index: PtrInt;
  AThread: TProcThread;
  NewThread: Boolean;
  Item: TMultiThreadProcItem;
  HelperThreadException: Exception;
begin
  if (StartIndex>EndIndex) then exit; // nothing to do
  if FDestroying then raise Exception.Create('Pool destroyed');

  if (MaxThreads>MaxThreadCount) or (MaxThreads<=0) then
    MaxThreads:=MaxThreadCount;
  if (StartIndex=EndIndex) or (MaxThreads<=1) then begin
    // single threaded
    Item:=TMultiThreadProcItem.Create;
    try
      for Index:=StartIndex to EndIndex do begin
        Item.FIndex:=Index;
        if Assigned(AFrame) then
          CallLocalProc(AProc,AFrame,Index,Data,Item)
        else if Assigned(AProc) then
          AProc(Index,Data,Item)
        else if Assigned(AMethod) then
          AMethod(Index,Data,Item)
        else
          ANested(Index,Data,Item);
      end;
    finally
      Item.Free;
    end;
    exit;
  end;

  // create a new group
  Group:=TProcThreadGroup.Create;
  Group.FPool:=Self;
  Group.FTaskData:=Data;
  Group.FTaskMethod:=AMethod;
  Group.FTaskProcedure:=AProc;
  Group.FTaskNested:=ANested;
  Group.FTaskFrame:=AFrame;
  Group.FStartIndex:=StartIndex;
  Group.FEndIndex:=EndIndex;
  Group.FFirstRunningIndex:=StartIndex;
  Group.FLastRunningIndex:=StartIndex;
  Group.FMaxThreads:=MaxThreads;
  Group.FThreadCount:=1;
  Group.FStarterItem.FState:=mtptsActive;
  Group.FStarterItem.FIndex:=StartIndex;
  HelperThreadException:=nil;
  try
    // start threads
    EnterPoolCriticalSection;
    try
      Group.AddToList(FFirstGroupNeedThreads,mtpgsNeedThreads);
      while Group.NeedMoreThreads do begin
        AThread:=FFirstInactiveThread;
        NewThread:=false;
        if AThread<>nil then begin
          AThread.RemoveFromList(FFirstInactiveThread,mtptlPool);
        end else if FThreadCount<FMaxThreadCount then begin
          AThread:=TProcThread.Create;
          if Assigned(AThread.FatalException) then
            raise AThread.FatalException;
          NewThread:=true;
          inc(FThreadCount);
        end else begin
          break;
        end;
        // add to Group
        Group.AddThread(AThread);
        // start thread
        AThread.AddToList(FFirstActiveThread,mtptlPool);
        AThread.Item.FState:=mtptsActive;
        if NewThread then
          AThread.Start
        else
          RTLeventSetEvent(AThread.Item.fWaitForPool);
      end;
    finally
      LeavePoolCriticalSection;
    end;

    // run until no more Index left
    Index:=StartIndex;
    repeat
      Group.FStarterItem.FIndex:=Index;
      Group.Run(Index,Data,Group.FStarterItem);

      EnterPoolCriticalSection;
      try
        Group.IndexComplete(Index);
        if (Group.FLastRunningIndex<Group.EndIndex) and (Group.FState<>mtpgsException)
        then begin
          inc(Group.FLastRunningIndex);
          Index:=Group.FLastRunningIndex;
        end else begin
          Index:=StartIndex;
        end;
      finally
        LeavePoolCriticalSection;
      end;
    until Index=StartIndex;
  finally
    // wait for Group to finish
    if Group.FFirstThread<>nil then begin
      EnterPoolCriticalSection;
      try
        Group.FStarterItem.FState:=mtptsInactive;
        Group.FStarterItem.fIndex:=EndIndex;// needed for Group.HasFinishedIndex
        // wake threads waiting for starter thread to finish
        if Group.FStarterItem.FState<>mtptsInactive then
          Group.EnterExceptionState(nil)
        else
          Group.WakeThreadsWaitingForIndex;
      finally
        LeavePoolCriticalSection;
      end;
      // waiting with exponential spin lock
      Index:=0;
      while Group.FFirstThread<>nil do begin
        sleep(Index);
        Index:=Index*2+1;
        if Index>30 then Index:=30;
      end;
    end;
    // remove group from pool
    EnterPoolCriticalSection;
    try
      case Group.FState of
      mtpgsNeedThreads: Group.RemoveFromList(FFirstGroupNeedThreads);
      mtpgsFinishing: Group.RemoveFromList(FFirstGroupFinishing);
      end;
    finally
      LeavePoolCriticalSection;
    end;
    HelperThreadException:=Group.FException;
    Group.Free;
    // free terminated threads (terminated, because of exceptions)
    CleanTerminatedThreads;
  end;
  // if the exception occured in a helper thread raise it now
  if HelperThreadException<>nil then
    raise HelperThreadException;
end;

initialization
  ProcThreadPool:=TProcThreadPool.Create;
  CurrentThread:=nil;

finalization
  ProcThreadPool.Free;
  ProcThreadPool:=nil;

end.

