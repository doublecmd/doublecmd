{
   Double Commander
   -------------------------------------------------------------------------
   This unit contains specific DARWIN FSEvent functions.

   Copyright (C) 2023 Alexander Koblov (alexx2000@mail.ru)
   Copyright (C) 2023 Rich Chang (rich2014.git@outlook.com)

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
   2. subdirectories monitor supported. currently in DC, only one level needed.
   3. file attributes monitoring is supported, and monitoring of adding files,
      renaming files, deleting files is also supported.
      for comparison, file attributes monitoring is missing with kqueue/kevent.
   4. CFRunLoop is used in TDarwinFSWatcher. because in DC a separate thread
      has been opened (in uFileSystemWatcher), it is more appropriate to use
      CFRunLoop than DispatchQueue.
}

unit uDarwinFSWatch;

{$mode delphi}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, SyncObjs,
  MacOSAll, CocoaAll;

type TDarwinFSWatchEventCategory = (
  ecAttribChanged, ecCoreAttribChanged, ecXattrChanged,
  ecStructChanged, ecCreated, ecRemoved, ecRenamed,
  ecRootChanged, ecChildChanged,
  ecDropabled );

type TDarwinFSWatchEventCategories = set of TDarwinFSWatchEventCategory;

type TDarwinFSWatchEvent = class
private
  _categories: TDarwinFSWatchEventCategories;
  _watchPath: String;
  _fullPath: String;
  _rawEventFlags: UInt32;
private
  procedure createCategories;
public
  constructor create( const aWatchPath:String; const aFullPath:String; const aFlags:UInt32 );
  function isDropabled(): Boolean;
  function categoriesToStr(): String;
  function rawEventFlagsToStr(): String;
public
  property categories: TDarwinFSWatchEventCategories read _categories;
  property watchPath: String read _watchPath;
  property fullPath: String read _fullPath;
  property rawEventFlags: UInt32 read _rawEventFlags;
end;

type TDarwinFSWatchCallBack = Procedure( event:TDarwinFSWatchEvent ) of object;

type TDarwinFSWatcher = class
private
  _watchPaths: NSMutableArray;
  _streamPaths: NSArray;
  _watchSubtree: Boolean;

  _callback: TDarwinFSWatchCallBack;
  _latency: Integer;
  _stream: FSEventStreamRef;
  _streamContext: FSEventStreamContext;
  _lastEventId: FSEventStreamEventId;

  _running: Boolean;
  _runLoop: CFRunLoopRef;
  _thread: TThread;

  _lockObject: TCriticalSection;
  _pathsSyncObject: TEventObject;
private
  procedure handleEvents( const amount:size_t; paths:PPChar; flags:FSEventStreamEventFlagsPtr; ids:FSEventStreamEventIdPtr );
  procedure doCallback( const path:String; const flags:FSEventStreamEventFlags);

  procedure updateStream;
  procedure closeStream;
  procedure waitPath;
  procedure notifyPath;
  procedure interrupt;
public
  constructor create( const callback:TDarwinFSWatchCallBack; const watchSubtree:Boolean=false; const latency:Integer=300 );
  destructor destroy; override;

  procedure start;
  procedure terminate;

  procedure addPath( path:String );
  procedure removePath( path:String );
  procedure clearPath;
end;


implementation

function StringToNSString(const S: String): NSString;
begin
  Result:= NSString(NSString.stringWithUTF8String(PAnsiChar(S)));
end;

constructor TDarwinFSWatchEvent.create( const aWatchPath:String; const aFullPath:String; const aFlags:UInt32 );
begin
  Inherited Create;
  _watchPath:= aWatchPath;
  _fullPath:= aFullPath;
  _rawEventFlags:= aFlags;
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
    _categories:= _categories + [ecAttribChanged, ecCoreAttribChanged];

  if (_rawEventFlags and (
        kFSEventStreamEventFlagItemFinderInfoMod or
        kFSEventStreamEventFlagItemXattrMod
     )) <> 0 then
    _categories:= _categories + [ecAttribChanged, ecXattrChanged];

  if (_rawEventFlags and kFSEventStreamEventFlagItemCreated)<>0 then
    _categories:= _categories + [ecStructChanged, ecCreated];
  if (_rawEventFlags and kFSEventStreamEventFlagItemRemoved)<>0 then
    _categories:= _categories + [ecStructChanged, ecRemoved];
  if (_rawEventFlags and kFSEventStreamEventFlagItemRenamed)<>0 then
    _categories:= _categories + [ecStructChanged, ecRenamed];

  if (_rawEventFlags and kFSEventStreamEventFlagRootChanged)<>0 then begin
    _categories:= _categories + [ecRootChanged];
  end else begin
    if (_fullPath<>watchPath) and (_fullPath.CountChar(PathDelim)<>_watchPath.CountChar(PathDelim)+1) then
      _categories:= _categories + [ecChildChanged];
  end;

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
    Result:= Result + '|Attrib';
  if ecCoreAttribChanged in _categories then
    Result:= Result + '|CoreAttrib';
  if ecXattrChanged in _categories then
    Result:= Result + '|Xattr';
  if ecStructChanged in _categories then
    Result:= Result + '|Struct';
  if ecCreated in _categories then
    Result:= Result + '|Created';
  if ecRemoved in _categories then
    Result:= Result + '|Removed';
  if ecRenamed in _categories then
    Result:= Result + '|Renamed';
  if ecRootChanged in _categories then
    Result:= Result + '|Root';
  if ecChildChanged in _categories then
    Result:= Result + '|Child';
  if ecDropabled in _categories then
    Result:= Result + '|Dropabled';

  Result:= Result.TrimLeft( '|' );
end;

function TDarwinFSWatchEvent.rawEventFlagsToStr(): String;
begin
  Result:= EmptyStr;

  if (_rawEventFlags and kFSEventStreamEventFlagItemModified)<>0 then
    Result:= Result + '|Modified';
  if (_rawEventFlags and kFSEventStreamEventFlagItemChangeOwner)<>0 then
    Result:= Result + '|ChangeOwner';
  if (_rawEventFlags and kFSEventStreamEventFlagItemInodeMetaMod)<>0 then
    Result:= Result + '|InodeMetaMod';
  if (_rawEventFlags and kFSEventStreamEventFlagItemFinderInfoMod)<>0 then
    Result:= Result + '|FinderInfoMod';
  if (_rawEventFlags and kFSEventStreamEventFlagItemXattrMod)<>0 then
    Result:= Result + '|XattrMod';

  if (_rawEventFlags and kFSEventStreamEventFlagItemCreated)<>0 then
    Result:= Result + '|Created';
  if (_rawEventFlags and kFSEventStreamEventFlagItemRemoved)<>0 then
    Result:= Result + '|Removed';
  if (_rawEventFlags and kFSEventStreamEventFlagItemRenamed)<>0 then
    Result:= Result + '|Renamed';

  if (_rawEventFlags and kFSEventStreamEventFlagRootChanged)<>0 then
    Result:= Result + '|RootChanged';

  if (_rawEventFlags and kFSEventStreamEventFlagItemIsFile)<>0 then
    Result:= Result + '|IsFile';
  if (_rawEventFlags and kFSEventStreamEventFlagItemIsDir)<>0 then
    Result:= Result + '|IsDir';
  if (_rawEventFlags and kFSEventStreamEventFlagItemIsSymlink)<>0 then
    Result:= Result + '|IsSymlink';
  if (_rawEventFlags and $00100000)<>0 then
    Result:= Result + '|IsHardLink';
  if (_rawEventFlags and $00200000)<>0 then
    Result:= Result + '|IsLastHardLink';

  if (_rawEventFlags and kFSEventStreamEventFlagMustScanSubDirs)<>0 then
    Result:= Result + '|ScanSubDirs';
  if (_rawEventFlags and kFSEventStreamEventFlagUserDropped)<>0 then
    Result:= Result + '|UserDropped';
  if (_rawEventFlags and kFSEventStreamEventFlagKernelDropped)<>0 then
    Result:= Result + '|KernelDropped';
  if (_rawEventFlags and kFSEventStreamEventFlagEventIdsWrapped)<>0 then
    Result:= Result + '|IdsWrapped';
  if (_rawEventFlags and kFSEventStreamEventFlagHistoryDone)<>0 then
    Result:= Result + '|HistoryDone';
  if (_rawEventFlags and kFSEventStreamEventFlagMount)<>0 then
    Result:= Result + '|Mount';
  if (_rawEventFlags and kFSEventStreamEventFlagUnmount)<>0 then
    Result:= Result + '|Unmount';
  if (_rawEventFlags and kFSEventStreamEventFlagOwnEvent)<>0 then
    Result:= Result + '|OwnEvent';
  if (_rawEventFlags and $00400000)<>0 then
    Result:= Result + '|Cloned';

  if (_rawEventFlags and $FF800000)<>0 then
    Result:= '|*UnkownFlags:' + IntToHex(_rawEventFlags) + '*' + Result;

  if Result.IsEmpty then
    Result:= '*NoneFlag*'
  else
    Result:= Result.TrimLeft( '|' );
end;

constructor TDarwinFSWatcher.create( const callback:TDarwinFSWatchCallBack; const watchSubtree:Boolean; const latency:Integer );
begin
  Inherited Create;
  _watchPaths:= NSMutableArray.alloc.initWithCapacity( 16 );
  _watchSubtree:= watchSubtree;

  _callback:= callback;
  _latency:= latency;
  _streamContext.info:= self;
  _lastEventId:= FSEventStreamEventId(kFSEventStreamEventIdSinceNow);

  _running:= false;
  _lockObject:= TCriticalSection.Create;
  _pathsSyncObject:= TSimpleEvent.Create;
end;

destructor TDarwinFSWatcher.destroy;
begin
  _pathsSyncObject.SetEvent;
  FreeAndNil( _lockObject );
  FreeAndNil( _pathsSyncObject );
  _watchPaths.release;
  _watchPaths:= nil;
  _streamPaths.release;
  _streamPaths:= nil;
  Inherited;
end;

procedure cdeclFSEventsCallback( {%H-}streamRef: ConstFSEventStreamRef;
  clientCallBackInfo: UnivPtr;
  numEvents: size_t;
  eventPaths: UnivPtr;
  eventFlags: FSEventStreamEventFlagsPtr;
  eventIds: FSEventStreamEventIdPtr ); cdecl;
var
  watcher: TDarwinFSWatcher absolute clientCallBackInfo;
begin
  watcher.handleEvents( NumEvents, PPChar(EventPaths), EventFlags, EventIds );
end;

procedure TDarwinFSWatcher.handleEvents( const amount:size_t; paths:PPChar; flags:FSEventStreamEventFlagsPtr; ids:FSEventStreamEventIdPtr );
var
  i: size_t;
begin
  for i:=amount downto 1 do
  begin
    doCallback( paths^, flags^ );
    inc(paths);
    inc(flags);
  end;
end;

// Note: try to avoid string copy
function isMatchWatchPath(
  const fullPath:String; const watchPath:String;
  const flags:FSEventStreamEventFlags; const watchSubtree:Boolean ): Boolean;
var
  fullPathDeep: Integer;
  watchPathDeep: Integer;
begin
  // detect if fullPath=watchPath
  if (flags and (kFSEventStreamEventFlagItemIsDir or kFSEventStreamEventFlagRootChanged)) <> 0 then
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
  watchPathDeep:= watchPath.CountChar(PathDelim)+1;
  Result:= fullPathDeep=watchPathDeep;
end;

procedure TDarwinFSWatcher.doCallback( const path:String; const flags:FSEventStreamEventFlags );
var
  watchPath: NSString;
  event: TDarwinFSWatchEvent;
begin
  for watchPath in _streamPaths do
  begin
    if isMatchWatchPath(path, watchPath.UTF8String, flags, _watchSubtree) then
    begin
      event:= TDarwinFSWatchEvent.create( watchPath.UTF8String, path, flags );
      _callback( event );
      event.Free;
    end;
  end;
end;

procedure TDarwinFSWatcher.updateStream;
var
  flags: FSEventStreamCreateFlags;
begin
  if _watchPaths.isEqualToArray(_streamPaths) then exit;

  closeStream;

  _streamPaths.release;
  _streamPaths:= NSArray.alloc.initWithArray( _watchPaths );

  if _watchPaths.count = 0 then
  begin
    _lastEventId:= FSEventStreamEventId(kFSEventStreamEventIdSinceNow);
    exit;
  end;

  flags:= kFSEventStreamCreateFlagFileEvents or kFSEventStreamCreateFlagWatchRoot;
  _stream:= FSEventStreamCreate( nil,
              @cdeclFSEventsCallback,
              @_streamContext,
              CFArrayRef(_watchPaths),
              _lastEventId,
              _latency/1000,
              flags );
  FSEventStreamScheduleWithRunLoop( _stream, _runLoop, kCFRunLoopDefaultMode );
  FSEventStreamStart( _stream );
end;

procedure TDarwinFSWatcher.closeStream;
begin
  if Assigned(_stream) then
  begin
    _lastEventId:= FSEventsGetCurrentEventId();
    FSEventStreamFlushSync( _stream );
    FSEventStreamStop( _stream );
    FSEventStreamInvalidate( _stream );
    FSEventStreamRelease( _stream );
    _stream:= nil;
  end;
end;

procedure TDarwinFSWatcher.start;
begin
  _running:= true;
  _runLoop:= CFRunLoopGetCurrent();
  _thread:= TThread.CurrentThread;

  repeat

    _lockObject.Acquire;
    try
      updateStream;
    finally
      _lockObject.Release;
    end;

    if Assigned(_stream) then
      CFRunLoopRun
    else
      waitPath;

  until not _running;
end;

procedure TDarwinFSWatcher.terminate;
begin
  _lockObject.Acquire;
  try
    _running:= false;
    interrupt;
  finally
    _lockObject.Release;
  end;
  if Assigned(_thread) then _thread.WaitFor;
  closeStream;
end;

procedure TDarwinFSWatcher.interrupt;
begin
  if Assigned(_stream) then
    CFRunLoopStop( _runLoop )
  else
    notifyPath;
end;

procedure TDarwinFSWatcher.addPath( path:String );
var
  nsPath: NSString;
begin
  _lockObject.Acquire;
  try
    if path<>PathDelim then
      path:= ExcludeTrailingPathDelimiter(path);
    nsPath:= StringToNSString( path );
    if _watchPaths.containsObject(nsPath) then exit;
    _watchPaths.addObject( nsPath );
    interrupt;
  finally
    _lockObject.Release;
  end;
end;

procedure TDarwinFSWatcher.removePath( path:String );
var
  nsPath: NSString;
begin
  _lockObject.Acquire;
  try
    if path<>PathDelim then
      path:= ExcludeTrailingPathDelimiter(path);
    nsPath:= StringToNSString( path );
    if not _watchPaths.containsObject(nsPath) then exit;
    _watchPaths.removeObject( nsPath );
    interrupt;
  finally
    _lockObject.Release;
  end;
end;

procedure TDarwinFSWatcher.clearPath;
begin
  _lockObject.Acquire;
  try
    if _watchPaths.count = 0 then exit;
    _watchPaths.removeAllObjects;
    interrupt;
  finally
    _lockObject.Release;
  end;
end;

procedure TDarwinFSWatcher.waitPath;
begin
  _pathsSyncObject.WaitFor( INFINITE );
  _pathsSyncObject.ResetEvent;
end;

procedure TDarwinFSWatcher.notifyPath;
begin
  _pathsSyncObject.SetEvent;
end;

end.

