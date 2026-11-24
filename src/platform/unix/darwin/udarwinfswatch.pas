{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains specific DARWIN FSEvent functions.

   Copyright (C) 2023-2026 Alexander Koblov (alexx2000@mail.ru)
   Copyright (C) 2023-2026 Rich Chang (rich2014.git@outlook.com)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.

   Notes:
   1. multiple directories can be monitored at the same time.
      DC generally monitors more than 2 directories (possibly much more than 2),
      just one TDarwinFSWatcher needed.
   2. subdirectories monitor supported.
   3. file attributes monitoring is supported, and monitoring of adding files,
      renaming files, deleting files is also supported.
      for comparison, file attributes monitoring is missing with kqueue/kevent.
   4. renamed Event fully supported from MacOS 10.13
   5. CFRunLoop is used in TDarwinFSWatcher. because Currently, on Aarch64,
      Free Pascal is incompatible with DispatchQueue, which leads to crashes.
      it seems to be related to Blocks and ARC.
   6. synchronization mechanism
   (1) with FSEvent, file monitoring events are generated and consumed in single
       threaded, no synchronization is required.
   (2) watch paths changes may involve multiple threads, which inherently
       necessitates synchronization
   (3) synchronization is also required between the above two operations.
       therefore, the synchronization within DarwinFSWatch is heterogeneous,
       as it does not originate from a single event source.
   (4) the most suitable synchronization mechanism for this purpose is, in fact,
       the equivalent of `pthread_mutex_t` combined with `pthread_cond_wait`.
       however, Free Pascal lacks an equivalent, so DarwinFSWatch originally
       employed a combination of `TCriticalSection` and `TEventObject` to
       simulate the requirement.
       while this generally achieves synchronization, it inherently carries risks
       in extreme scenarios.
   (5) given that DarwinFSWatch is used exclusively on macOS, there is no need
       for cross-platform support. therefore, in addition to RTL and POSIX,
       one might also consider Cocoa. within Cocoa, `NSCondition` is arguably
       the best choice; it is not only equivalent to a combination of
       `pthread_mutex_t` and `pthread_cond_wait`, but also encapsulates these
       primitives, thereby simplifying operations.
       as for `NSConditionLock`, which appears simpler on the surface, it is not
       actually suitable for a proper application; its design even makes it
       difficult — if not impossible — to gracefully notify relevant threads to exit.
   (6) initially, both file events (RunLoop) and path changes were handled
       within the Watcher thread, but this approach has its flaws.
       because a RunLoop is blocking, it is necessary to switch between
       `RunLoopRun` and `Lock.wait` depending on the circumstances;
       however, this switching process carries potential risks in extreme scenarios.
       now, an additional thread has been introduced specifically for handling
       path changes (see `pathsMonitorExecute`), while the Watcher thread is
       dedicated to `RunLoopRun` (see `runLoopExecute`).
}

unit uDarwinFSWatch;

{$mode delphi}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, Contnrs,
  MacOSAll, CocoaAll, BaseUnix;

type TDarwinFSWatchEventCategory = (
  ecAttribChanged, ecCoreAttribChanged, ecXattrChanged,
  ecStructChanged, ecCreated, ecRemoved, ecRenamed,
  ecRootChanged, ecChildChanged,
  ecFile, ecDir,
  ecDropabled );

type TDarwinFSWatchEventCategories = set of TDarwinFSWatchEventCategory;

type TDarwinFSWatchState = (
  stateInitializing, stateRunning, stateTerminated,
  statePathChanged );

type TInternalEvent = class
private
  path: String;
  renamedPath: String;  // only for Ranamed Event
  flags: FSEventStreamEventFlags;
  iNode: UInt64;
private
  function deepCopy(): TInternalEvent;
end;

type TDarwinFSWatchEventSession = class
private
  _list: TObjectList;
public
  constructor create; overload;
  constructor create( const amount:size_t; eventPaths:CFArrayRef; flags:FSEventStreamEventFlagsPtr );
  destructor destroy; override;
private
  function deepCopy(): TDarwinFSWatchEventSession;

  function count(): Integer;
  function getItem( index:Integer ): TInternalEvent;

  procedure adjustSymlinkIfNecessary( index:Integer; watchPath:String; watchRealPath:String );
  procedure adjustRenamedEventIfNecessary( index:Integer );
  function isRenamed( event:TInternalEvent ): Boolean;

  property items[index: Integer]: TInternalEvent read getItem; default;
end;

type TDarwinFSWatchEvent = class
private
  _categories: TDarwinFSWatchEventCategories;
  _watchPath: String;
  _fullPath: String;
  _renamedPath: String;
  _rawEventFlags: UInt32;
  _rawINode: UInt64;
private
  procedure createCategories;
  {%H-}constructor create( const aWatchPath:String; const internalEvent:TInternalEvent );
public
  function isDropabled(): Boolean;
  function categoriesToStr(): String;
  function rawEventFlagsToStr(): String;
public
  property categories: TDarwinFSWatchEventCategories read _categories;
  property watchPath: String read _watchPath;
  property fullPath: String read _fullPath;
  property renamedPath: String read _renamedPath;
  property rawEventFlags: UInt32 read _rawEventFlags;
  property rawINode: UInt64 read _rawINode;
end;

type TDarwinFSWatchCallBack = Procedure( event:TDarwinFSWatchEvent ) of object;

type 

{ TDarwinFSWatcher }

 TDarwinFSWatcher = class
private
  _watchPaths: NSMutableArray;            // *** needs to be protected ***
  _watchRealPaths: NSMutableArray;        // only one thread access
  _streamPaths: NSArray;                  // only one thread access

  _callback: TDarwinFSWatchCallBack;      // immutable
  _latency: Integer;                      // immutable
  _stream: FSEventStreamRef;              // *** needs to be protected ***
  _streamContext: FSEventStreamContext;   // immutable
  _lastEventId: FSEventStreamEventId;     // only one thread access

  _runLoop: CFRunLoopRef;                 // immutable
  _watcherThread: TThread;                // immutable
  _pathsMonitorThread: TThread;           // immutable

  _condition: NSCondition;                // immutable
  _state: TDarwinFSWatchState;            // *** needs to be protected ***
public
  watchSubtree: Boolean;
private
  procedure runLoopExecute;
  procedure pathsMonitorExecute;

  procedure handleEvents( const originalSession:TDarwinFSWatchEventSession );
  procedure doCallback( const watchPath:String; const internalEvent:TInternalEvent );

  procedure updateStream;
  procedure closeStream(const stream: FSEventStreamRef);

  procedure waitPaths;
  procedure waitStream;
  procedure wakeupAllWaiting;
  procedure doWakeupAllWaiting;
public
  constructor create( const callback:TDarwinFSWatchCallBack; const latency:Integer=300 );
  destructor destroy; override;

  procedure start;
  procedure terminate;

  procedure addPath( path:String );
  procedure removePath( path:String );
  procedure clearPath;
end;

implementation

const
  kFSEventStreamCreateFlagUseExtendedData   = $00000040;
  kFSEventStreamEventFlagItemIsHardlink     = $00100000;
  kFSEventStreamEventFlagItemIsLastHardlink = $00200000;
  kFSEventStreamEventFlagItemCloned         = $00400000;

  CREATE_FLAGS= kFSEventStreamCreateFlagFileEvents
             or kFSEventStreamCreateFlagWatchRoot
             or kFSEventStreamCreateFlagNoDefer
             or kFSEventStreamCreateFlagUseCFTypes
             or kFSEventStreamCreateFlagUseExtendedData;

  NSAppKitVersionNumber10_13 = 1561;

var
  kFSEventStreamEventExtendedDataPathKey: CFStringRef;
  kFSEventStreamEventExtendedFileIDKey: CFStringRef;
  isFlagUseExtendedDataSupported: Boolean;

function StringToNSString(const S: String): NSString;
begin
  Result:= NSString(NSString.stringWithUTF8String(PAnsiChar(S)));
end;

constructor TDarwinFSWatchEvent.create( const aWatchPath:String; const internalEvent:TInternalEvent );
begin
  Inherited Create;
  _watchPath:= aWatchPath;
  _fullPath:= internalEvent.path;
  _renamedPath:= internalEvent.renamedPath;
  _rawEventFlags:= internalEvent.flags;
  _rawINode:= internalEvent.iNode;
  createCategories;
end;

procedure TDarwinFSWatchEvent.createCategories;
begin
  _categories:= [];

  if (_rawEventFlags and (
        kFSEventStreamEventFlagItemModified or
        kFSEventStreamEventFlagItemChangeOwner or
        kFSEventStreamEventFlagItemInodeMetaMod
     )) <> 0 then
    _categories += [ecAttribChanged, ecCoreAttribChanged];

  if (_rawEventFlags and (
        kFSEventStreamEventFlagItemFinderInfoMod or
        kFSEventStreamEventFlagItemXattrMod
     )) <> 0 then
    _categories += [ecAttribChanged, ecXattrChanged];

  if (_rawEventFlags and kFSEventStreamEventFlagItemCreated)<>0 then
    _categories += [ecStructChanged, ecCreated];
  if (_rawEventFlags and kFSEventStreamEventFlagItemRemoved)<>0 then
    _categories += [ecStructChanged, ecRemoved];
  if (_rawEventFlags and kFSEventStreamEventFlagItemRenamed)<>0 then
    _categories += [ecStructChanged, ecRenamed];

  if (_rawEventFlags and kFSEventStreamEventFlagRootChanged)<>0 then begin
    _categories += [ecRootChanged];
  end else begin
    if (_fullPath<>watchPath) and (_fullPath.CountChar(PathDelim)<>_watchPath.CountChar(PathDelim)+1) then begin
      if (_renamedPath.IsEmpty) or ((_renamedPath<>watchPath) and (_renamedPath.CountChar(PathDelim)<>_watchPath.CountChar(PathDelim)+1)) then
        _categories += [ecChildChanged];
    end;
  end;

  if (_rawEventFlags and kFSEventStreamEventFlagItemIsFile)<>0 then
    _categories += [ecFile];

  if (_rawEventFlags and kFSEventStreamEventFlagItemIsDir)<>0 then
    _categories += [ecDir];

  if _categories * [ecAttribChanged,ecStructChanged,ecRootChanged] = [] then
    _categories:= [ecDropabled];
end;

function TDarwinFSWatchEvent.isDropabled(): Boolean;
begin
  Result:= _categories = [ecDropabled];
end;

function TDarwinFSWatchEvent.categoriesToStr(): String;
begin
  Result:= EmptyStr;

  if ecAttribChanged in _categories then
    Result += '|Attrib';
  if ecCoreAttribChanged in _categories then
    Result += '|CoreAttrib';
  if ecXattrChanged in _categories then
    Result += '|Xattr';
  if ecStructChanged in _categories then
    Result += '|Struct';
  if ecCreated in _categories then
    Result += '|Created';
  if ecRemoved in _categories then
    Result += '|Removed';
  if ecRenamed in _categories then
    Result += '|Renamed';
  if ecRootChanged in _categories then
    Result += '|Root';
  if ecChildChanged in _categories then
    Result += '|Child';
  if ecDropabled in _categories then
    Result += '|Dropabled';

  Result:= Result.TrimLeft( '|' );
end;

function TDarwinFSWatchEvent.rawEventFlagsToStr(): String;
begin
  Result:= EmptyStr;

  if (_rawEventFlags and kFSEventStreamEventFlagItemModified)<>0 then
    Result += '|Modified';
  if (_rawEventFlags and kFSEventStreamEventFlagItemChangeOwner)<>0 then
    Result += '|ChangeOwner';
  if (_rawEventFlags and kFSEventStreamEventFlagItemInodeMetaMod)<>0 then
    Result += '|InodeMetaMod';
  if (_rawEventFlags and kFSEventStreamEventFlagItemFinderInfoMod)<>0 then
    Result += '|FinderInfoMod';
  if (_rawEventFlags and kFSEventStreamEventFlagItemXattrMod)<>0 then
    Result += '|XattrMod';

  if (_rawEventFlags and kFSEventStreamEventFlagItemCreated)<>0 then
    Result += '|Created';
  if (_rawEventFlags and kFSEventStreamEventFlagItemRemoved)<>0 then
    Result += '|Removed';
  if (_rawEventFlags and kFSEventStreamEventFlagItemRenamed)<>0 then
    Result += '|Renamed';

  if (_rawEventFlags and kFSEventStreamEventFlagRootChanged)<>0 then
    Result += '|RootChanged';

  if (_rawEventFlags and kFSEventStreamEventFlagItemIsFile)<>0 then
    Result += '|IsFile';
  if (_rawEventFlags and kFSEventStreamEventFlagItemIsDir)<>0 then
    Result += '|IsDir';
  if (_rawEventFlags and kFSEventStreamEventFlagItemIsSymlink)<>0 then
    Result += '|IsSymlink';
  if (_rawEventFlags and kFSEventStreamEventFlagItemIsHardlink)<>0 then
    Result += '|IsHardLink';
  if (_rawEventFlags and kFSEventStreamEventFlagItemIsLastHardlink)<>0 then
    Result += '|IsLastHardLink';

  if (_rawEventFlags and kFSEventStreamEventFlagMustScanSubDirs)<>0 then
    Result += '|ScanSubDirs';
  if (_rawEventFlags and kFSEventStreamEventFlagUserDropped)<>0 then
    Result += '|UserDropped';
  if (_rawEventFlags and kFSEventStreamEventFlagKernelDropped)<>0 then
    Result += '|KernelDropped';
  if (_rawEventFlags and kFSEventStreamEventFlagEventIdsWrapped)<>0 then
    Result += '|IdsWrapped';
  if (_rawEventFlags and kFSEventStreamEventFlagHistoryDone)<>0 then
    Result += '|HistoryDone';
  if (_rawEventFlags and kFSEventStreamEventFlagMount)<>0 then
    Result += '|Mount';
  if (_rawEventFlags and kFSEventStreamEventFlagUnmount)<>0 then
    Result += '|Unmount';
  if (_rawEventFlags and kFSEventStreamEventFlagOwnEvent)<>0 then
    Result += '|OwnEvent';
  if (_rawEventFlags and kFSEventStreamEventFlagItemCloned)<>0 then
    Result += '|Cloned';

  if (_rawEventFlags and $FF800000)<>0 then
    Result:= '|*UnkownFlags:' + IntToHex(_rawEventFlags) + '*' + Result;

  if Result.IsEmpty then
    Result:= '*NoneFlag*'
  else
    Result:= Result.TrimLeft( '|' );
end;

// Note: try to avoid string copy
function isMatchWatchPath(
  const internalEvent:TInternalEvent;
  const watchPath:String;
  const watchSubtree:Boolean ): Boolean;
var
  fullPath: String;
  fullPathDeep: Integer;
  watchPathDeep: Integer;
begin
  fullPath:= internalEvent.path;;

  // detect if fullPath=watchPath
  if (internalEvent.flags and (kFSEventStreamEventFlagItemIsDir or kFSEventStreamEventFlagRootChanged)) <> 0 then
  begin
    Result:= watchPath.Equals( fullPath );
    if Result then exit;  // fullPath=watchPath, matched
  end;

  // detect if fullPath startsWith watchPath
  Result:= fullPath.StartsWith(watchPath);
  if watchSubtree then exit;

  // not watchSubtree
  // not startsWith watchPath, not match
  if not Result then exit;

  // not watchSubtree, and startsWith watchPath
  // detect if fullPath and watchPath in the same level
  fullPathDeep:= fullPath.CountChar(PathDelim);
  if watchPath = PathDelim then
    watchPathDeep:= 1
  else
    watchPathDeep:= watchPath.CountChar(PathDelim)+1;
  Result:= fullPathDeep=watchPathDeep;
end;

function TInternalEvent.deepCopy(): TInternalEvent;
begin
  Result:= TInternalEvent.Create;
  Result.path:= self.path;
  Result.renamedPath:= self.renamedPath;
  Result.flags:= self.flags;
  Result.iNode:= self.iNode;
end;

function TDarwinFSWatchEventSession.deepCopy(): TDarwinFSWatchEventSession;
var
  list: TObjectList;
  i: Integer;
begin
  list:= TObjectList.Create;
  for i:=0 to count-1 do
  begin
    list.Add( Items[i].deepCopy() );
  end;

  Result:= TDarwinFSWatchEventSession.create;
  Result._list:= list;
end;

constructor TDarwinFSWatchEventSession.create();
begin
  Inherited;
end;

constructor TDarwinFSWatchEventSession.create( const amount:size_t; eventPaths:CFArrayRef; flags:FSEventStreamEventFlagsPtr );
var
  i: size_t;
  event: TInternalEvent;
  infoDict: CFDictionaryRef;
  nsPath: NSString;
  nsNode: CFNumberRef;
begin
  _list:= TObjectList.Create;
  for i:=0 to amount-1 do
  begin
    event:= TInternalEvent.Create;

    if isFlagUseExtendedDataSupported then
    begin
      infoDict:= CFArrayGetValueAtIndex( eventPaths, i );
      nsPath:= CFDictionaryGetValue( infoDict, kFSEventStreamEventExtendedDataPathKey );
      nsNode:= CFDictionaryGetValue( infoDict, kFSEventStreamEventExtendedFileIDKey );
      if Assigned(nsNode) then
        CFNumberGetValue( nsNode, kCFNumberLongLongType, @(event.iNode) );
    end else begin
      nsPath:= CFArrayGetValueAtIndex( eventPaths, i );
    end;

    event.path:= nsPath.UTF8String;
    event.flags:= flags^;
    _list.Add( event );

    inc(flags);
  end;
end;

destructor TDarwinFSWatchEventSession.destroy;
begin
  FreeAndNil( _list );
end;

function TDarwinFSWatchEventSession.count: Integer;
begin
  Result:= _list.Count;
end;

function TDarwinFSWatchEventSession.getItem( index:Integer ): TInternalEvent;
begin
  Result:= TInternalEvent( _list[index] );
end;

function TDarwinFSWatchEventSession.isRenamed( event:TInternalEvent ): Boolean;
begin
  Result:= event.flags
       and (kFSEventStreamEventFlagItemRenamed or kFSEventStreamEventFlagItemCreated or kFSEventStreamEventFlagItemRemoved)
         = kFSEventStreamEventFlagItemRenamed;
end;

procedure TDarwinFSWatchEventSession.adjustRenamedEventIfNecessary( index:Integer );
var
  currentEvent: TInternalEvent;
  nextEvent: TInternalEvent;
  i: Integer;
begin
  currentEvent:= Items[index];
  if not isRenamed(currentEvent)
    then exit;

  // find all related Renamed Event, and try to build a complete Renamed Event with NewPath
  if (currentEvent.iNode<>0) and isFlagUseExtendedDataSupported then
  begin
    i:= index + 1;
    while i < count do
    begin
      nextEvent:= Items[i];
      if isRenamed(nextEvent) and (nextEvent.iNode=currentEvent.iNode) then begin
        if currentEvent.path<>nextEvent.path then currentEvent.renamedPath:= nextEvent.path;
        _list.Delete( i );
      end else begin
        inc( i );
      end;
    end;
  end;

  // got the complete Renamed Event, then exit
  if not currentEvent.renamedPath.IsEmpty then
    exit;

  // got the incomplete Renamed Event, change to Created or Removed Event
  currentEvent.flags:= currentEvent.flags and (not kFSEventStreamEventFlagItemRenamed);
  if FpAccess(currentEvent.path, F_OK) = 0 then begin
    currentEvent.flags:= currentEvent.flags or kFSEventStreamEventFlagItemCreated;
  end else begin
    currentEvent.flags:= currentEvent.flags or kFSEventStreamEventFlagItemRemoved;
  end;
end;

procedure TDarwinFSWatchEventSession.adjustSymlinkIfNecessary( index:Integer; watchPath:String; watchRealPath:String );
var
  currentEvent: TInternalEvent;
begin
  if watchPath=watchRealPath then exit;
  currentEvent:= Items[index];
  currentEvent.path:= watchPath + currentEvent.path.Substring(watchRealPath.Length);

  if currentEvent.renamedPath.IsEmpty then exit;
  if not currentEvent.renamedPath.StartsWith(watchRealPath) then exit;
  currentEvent.renamedPath:= watchPath + currentEvent.renamedPath.Substring(watchRealPath.Length);
end;

constructor TDarwinFSWatcher.create( const callback:TDarwinFSWatchCallBack; const latency:Integer );
begin
  Inherited Create;
  _watchPaths:= NSMutableArray.alloc.initWithCapacity( 16 );
  _watchRealPaths:= NSMutableArray.alloc.initWithCapacity( 16 );

  _callback:= callback;
  _latency:= latency;
  _streamContext.info:= self;
  _lastEventId:= FSEventStreamEventId(kFSEventStreamEventIdSinceNow);

  _state:= stateInitializing;
  _condition:= NSCondition.new;
end;

destructor TDarwinFSWatcher.destroy;
begin
  _condition.release;
  _condition:= nil;
  _watchPaths.release;
  _watchPaths:= nil;
  _watchRealPaths.release;
  _watchRealPaths:= nil;
  _streamPaths.release;
  _streamPaths:= nil;
  _pathsMonitorThread.Free;    // _watcherThread managed by caller
  Inherited;
end;

procedure cdeclFSEventsCallback(
  {%H-}streamRef: ConstFSEventStreamRef;
  clientCallBackInfo: UnivPtr;
  numEvents: size_t;
  eventPaths: UnivPtr;
  eventFlags: FSEventStreamEventFlagsPtr;
  {%H-}eventIds: FSEventStreamEventIdPtr ); cdecl;
var
  pool: NSAutoReleasePool;
  watcher: TDarwinFSWatcher absolute clientCallBackInfo;
  session: TDarwinFSWatchEventSession;
begin
  pool:= NSAutoreleasePool.alloc.init;
  session:= TDarwinFSWatchEventSession.create( numEvents, eventPaths, eventFlags );
  watcher.handleEvents( session );
  pool.release;
  // seesion released in handleEvents()
end;

procedure TDarwinFSWatcher.runLoopExecute;
var
  pool: NSAutoReleasePool;
begin
  repeat
    pool:= NSAutoreleasePool.alloc.init;
    CFRunLoopRun;
    self.waitStream;
    pool.release;
  until _state = stateTerminated;
end;

procedure TDarwinFSWatcher.pathsMonitorExecute;
var
  pool: NSAutoReleasePool;
begin
  TThread.CurrentThread.FreeOnTerminate:= False;

  repeat
    pool:= NSAutoreleasePool.alloc.init;
    self.updateStream;
    self.wakeupAllWaiting;
    self.waitPaths;
    pool.release;
  until _state = stateTerminated;
end;

procedure TDarwinFSWatcher.handleEvents( const originalSession:TDarwinFSWatchEventSession );
var
  tempWatchPaths: NSArray;
  tempWatchRealPaths: NSArray;
  watchPath: String;
  watchRealPath: String;
  event: TInternalEvent;
  pathIndex: Integer;
  i: Integer;
  session: TDarwinFSWatchEventSession;
  currentWatchSubtree: Boolean;
begin
  // for multithread
  currentWatchSubtree:= watchSubtree;

  try
    _condition.lock;
    try
      tempWatchPaths:= _watchPaths.copy;
      tempWatchRealPaths:= _watchRealPaths.copy;
    finally
      _condition.unlock;
    end;

    if tempWatchPaths.count=0 then
    begin
      originalSession.Free;
      exit;
    end;

    for pathIndex:=0 to tempWatchPaths.count-1 do
    begin
      watchPath:= NSString(tempWatchPaths.objectAtIndex(pathIndex)).UTF8String;
      watchRealPath:= NSString(tempWatchRealPaths.objectAtIndex(pathIndex)).UTF8String;
      if pathIndex=tempWatchPaths.count-1 then
        session:= originalSession
      else
        session:= originalSession.deepCopy();
      i:= 0;
      while i < session.count do
      begin
        event:= session[i];
        if isMatchWatchPath(event, watchRealPath, currentWatchSubtree) then
        begin
          session.adjustRenamedEventIfNecessary( i );
          session.adjustSymlinkIfNecessary( i, watchPath, watchRealPath );
          doCallback( watchPath, event );
        end;
        inc( i );
      end;
      session.Free;
    end;
  finally
    tempWatchPaths.Release;
    tempWatchRealPaths.Release;
  end;
end;

procedure TDarwinFSWatcher.doCallback( const watchPath:String; const internalEvent:TInternalEvent );
var
  event: TDarwinFSWatchEvent;
begin
  event:= TDarwinFSWatchEvent.create( watchPath, internalEvent );
  _callback( event );
  event.Free;
end;

procedure TDarwinFSWatcher.updateStream;
var
  watchPaths: NSArray;
  stream: FSEventStreamRef;
  pathsChanged: Boolean;

  procedure copyProtectedToLocalWithLock;
  begin
    _condition.lock;
    try
      watchPaths:= _watchPaths.copy;
      stream:= _stream;
      _stream:= nil;
    finally
      _condition.unlock;
    end;
  end;

  procedure setResultWithLock;
  begin
    _condition.lock;
    try
      pathsChanged:= NOT _watchPaths.isEqualToArray(_streamPaths);
      if pathsChanged then
        Exit;
      _stream:= stream;
    finally
      _condition.unlock;
    end;
  end;

  procedure releaseLocal;
  begin
    watchPaths.release;
  end;

  procedure doUpdateWithoutLock;
  begin
    if watchPaths.isEqualToArray(_streamPaths) then
      Exit;

    if Assigned(stream) then
      _lastEventId:= FSEventsGetCurrentEventId();
    closeStream( stream );

    _streamPaths.release;
    _streamPaths:= NSArray.alloc.initWithArray( watchPaths );

    if watchPaths.count = 0 then
      _lastEventId:= FSEventStreamEventId(kFSEventStreamEventIdSinceNow)
    else begin
      stream:= FSEventStreamCreate( nil,
                 @cdeclFSEventsCallback,
                 @_streamContext,
                 CFArrayRef(watchPaths),
                 _lastEventId,
                 _latency/1000,
                 CREATE_FLAGS );
      FSEventStreamScheduleWithRunLoop( stream, _runLoop, kCFRunLoopDefaultMode );
      FSEventStreamStart( stream );
    end;
  end;

begin
  // Closing an FSEvent stream may block on SMB for several seconds.
  // Take a snapshot so the stream can be updated without holding _lockObject.
  repeat
    try
      copyProtectedToLocalWithLock;
      doUpdateWithoutLock;
      setResultWithLock;
    finally
      releaseLocal;
    end;
  until (_state=stateTerminated) or NOT pathsChanged;
end;

procedure TDarwinFSWatcher.closeStream(const stream: FSEventStreamRef);
begin
  if NOT Assigned(stream) then
    Exit;
  FSEventStreamFlushSync( stream );
  FSEventStreamStop( stream );
  FSEventStreamInvalidate( stream );
  FSEventStreamRelease( stream );
end;

procedure TDarwinFSWatcher.start;
begin
  _state:= stateRunning;
  _runLoop:= CFRunLoopGetCurrent;
  _watcherThread:= TThread.CurrentThread;
  _pathsMonitorThread:= TThread.ExecuteInThread( self.pathsMonitorExecute );

  self.runLoopExecute;
end;

procedure TDarwinFSWatcher.terminate;
var
  stream: FSEventStreamRef;
begin
  _condition.lock;
  try
    _state:= stateTerminated;
    stream:= _stream;
    _stream:= nil;
    doWakeupAllWaiting;
    CFRunLoopStop( _runLoop );
    _runLoop:= nil;
  finally
    _condition.unlock;
  end;
  if Assigned(_watcherThread) then
    _watcherThread.WaitFor;
  if Assigned(_pathsMonitorThread) then
    _pathsMonitorThread.WaitFor;
  closeStream( stream );
end;

function realpath(__name:Pchar; __resolved:Pchar):Pchar;cdecl;external clib name 'realpath';

function toRealPath( path:String ): String;
var
  buf: array[0..PATH_MAX] of char;
  resolvedPath: pchar;
begin
  resolvedPath:= realpath( pchar(path), buf );
  if resolvedPath<>nil then
    Result:= resolvedPath
  else
    Result:= path;
end;

procedure TDarwinFSWatcher.addPath( path:String );
var
  nsPath: NSString;
  nsRealPath: NSString;
begin
  _condition.lock;
  try
    if path<>PathDelim then
      path:= ExcludeTrailingPathDelimiter(path);
    nsPath:= StringToNSString( path );
    if _watchPaths.containsObject(nsPath) then exit;
    _watchPaths.addObject( nsPath );

    nsRealPath:= StringToNSString( toRealPath(path) );
    _watchRealPaths.addObject( nsRealPath );

    doWakeupAllWaiting;
  finally
    _condition.unlock;
  end;
end;

procedure TDarwinFSWatcher.removePath( path:String );
var
  index: NSInteger;
  nsPath: NSString;
begin
  _condition.lock;
  try
    if path<>PathDelim then
      path:= ExcludeTrailingPathDelimiter(path);
    nsPath:= StringToNSString( path );

    index:= _watchPaths.indexOfObject( nsPath );
    if index = NSNotFound then exit;

    _watchPaths.removeObjectAtIndex( index );
    _watchRealPaths.removeObjectAtIndex( index );

    doWakeupAllWaiting;
  finally
    _condition.unlock;
  end;
end;

procedure TDarwinFSWatcher.clearPath;
begin
  _condition.lock;
  try
    if _watchPaths.count = 0 then exit;
    _watchPaths.removeAllObjects;
    _watchRealPaths.removeAllObjects;
    doWakeupAllWaiting;
  finally
    _condition.unlock;
  end;
end;

procedure TDarwinFSWatcher.waitPaths;
begin
  _condition.lock;

  while NOT (_state in [statePathChanged, stateTerminated]) do
    _condition.wait;

  if _state <> stateTerminated then
    _state:= stateRunning;

  _condition.unlock;
end;

procedure TDarwinFSWatcher.waitStream;
begin
  _condition.lock;

  while (_stream=nil) and (_state<>stateTerminated) do
    _condition.wait;

  _condition.unlock;
end;

procedure TDarwinFSWatcher.wakeupAllWaiting;
begin
  _condition.lock;
  self.doWakeupAllWaiting;
  _condition.unlock;
end;

procedure TDarwinFSWatcher.doWakeupAllWaiting;
begin
  if _state <> stateTerminated then
    _state:= statePathChanged;
  _condition.broadcast;
end;

initialization
  kFSEventStreamEventExtendedDataPathKey:= CFSTR('path');
  kFSEventStreamEventExtendedFileIDKey:= CFSTR('fileID');
  isFlagUseExtendedDataSupported:= (NSAppKitVersionNumber>=NSAppKitVersionNumber10_13);

end.

