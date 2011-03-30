{
    Double Commander
    -------------------------------------------------------------------------
    This is a thread-component sends an event when a change in the file system occurs.

    Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)
    Copyright (C) 2011  Przemyslaw Nagay (cobines@gmail.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uFileSystemWatcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFSWatchFilter = set of (wfFileNameChange, wfAttributesChange);

  TFSWatcherEventType = (fswFileCreated,
                         fswFileChanged,
                         fswFileDeleted,
                         fswFileRenamed,
                         fswUnknownChange);

  TFSWatcherEventData = record
    Path: String;
    EventType: TFSWatcherEventType;
    FileName: String;    // Valid for fswFileCreated, fswFileChanged, fswFileDeleted, fswFileRenamed
    OldFileName: String; // Valid for fswFileRenamed
    UserData: Pointer;
  end;

  TFSWatcherEvent = procedure(const EventData: TFSWatcherEventData) of object;

  { TFileSystemWatcher }

  TFileSystemWatcher = class
  private
    class procedure CreateFileSystemWatcher;
    class procedure DestroyFileSystemWatcher;
  public
    {en
       Returns @true if watch has been successfully added or already exists.
    }
    class function AddWatch(aWatchPath: UTF8String;
                            aWatchFilter: TFSWatchFilter;
                            aWatcherEvent: TFSWatcherEvent;
                            UserData: Pointer = nil): Boolean;
    class procedure RemoveWatch(aWatchPath: UTF8String;
                                aWatcherEvent: TFSWatcherEvent);
    class procedure RemoveWatch(aWatcherEvent: TFSWatcherEvent);
  end;

implementation

uses
  LCLProc, uDebug, uExceptions, syncobjs, fgl
  {$IF DEFINED(MSWINDOWS)}
  ,Windows
  {$ELSEIF DEFINED(LINUX)}
  ,inotify, BaseUnix
  {$ELSEIF DEFINED(BSD)}
  ,BSD, Unix, BaseUnix, UnixType
  {$ENDIF};

type
  TOSWatchObserver = class
    UserData: Pointer;
    WatcherEvent: TFSWatcherEvent;
    WatchFilter: TFSWatchFilter;
  end;
  TOSWatchObservers = specialize TFPGObjectList<TOSWatchObserver>;

  TOSWatch = class
  private
    FHandle: THandle;
    FObservers: TOSWatchObservers;
    FWatchFilter: TFSWatchFilter;
    FWatchPath: UTF8String;
    {$IF DEFINED(UNIX)}
    FNotifyHandle: THandle;
    {$ENDIF}
    procedure CreateHandle;
    procedure DestroyHandle;
  public
    constructor Create(const aWatchPath: UTF8String
                       {$IFDEF UNIX}; aNotifyHandle: THandle{$ENDIF}); reintroduce;
    destructor Destroy; override;
    procedure UpdateFilter;
    property Handle: THandle read FHandle;
    property Observers: TOSWatchObservers read FObservers;
    property WatchPath: UTF8String read FWatchPath;
  end;
  TOSWatchs = specialize TFPGObjectList<TOSWatch>;

  { TFileSystemWatcherImpl }

  TFileSystemWatcherImpl = class(TThread)
  private
    FWatcherLock: syncobjs.TCriticalSection;
    FOSWatchers: TOSWatchs;
    {$IF DEFINED(MSWINDOWS)}
    FWaitEvent: THandle;
    {$ELSEIF DEFINED(UNIX)}
    FNotifyHandle: THandle;
    {$ENDIF}
    {$IF DEFINED(LINUX)}
    FEventPipe: TFilDes;
    {$ENDIF}
    FCurrentEventData: TFSWatcherEventData;
    FFinished: Boolean;
    FExceptionMessage: String;
    FExceptionBackTrace: String;

    procedure DoWatcherEvent;
    function GetWatchersCount: Integer;
    procedure RemoveOSWatch(aHandle: THandle);
    procedure ShowException;
    procedure TriggerEvent;
  protected
    procedure Execute; override;
    procedure ExecuteWatcher;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Terminate;
    function AddWatch(aWatchPath: UTF8String;
                      aWatchFilter: TFSWatchFilter;
                      aWatcherEvent: TFSWatcherEvent;
                      UserData: Pointer = nil): Boolean;
    procedure RemoveWatch(aWatchPath: UTF8String;
                          aWatcherEvent: TFSWatcherEvent);
    procedure RemoveWatch(aWatcherEvent: TFSWatcherEvent);
    property WatchersCount: Integer read GetWatchersCount;
  end;

var
  FileSystemWatcher: TFileSystemWatcherImpl = nil;

{ TFileSystemWatcher }

class procedure TFileSystemWatcher.CreateFileSystemWatcher;
begin
  if Assigned(FileSystemWatcher) and FileSystemWatcher.FFinished then
    // Thread finished prematurely maybe because of an error.
    // Destroy and recreate below.
    DestroyFileSystemWatcher;

  if not Assigned(FileSystemWatcher) then
    FileSystemWatcher := TFileSystemWatcherImpl.Create;
end;

class procedure TFileSystemWatcher.DestroyFileSystemWatcher;
begin
  if Assigned(FileSystemWatcher) then
  begin
    DCDebug('Waiting for FileSystemWatcher thread');
    FileSystemWatcher.Terminate;
  {$IF (fpc_version<2) or ((fpc_version=2) and (fpc_release<5))}
    If (MainThreadID=GetCurrentThreadID) then
      while not FileSystemWatcher.FFinished do
        CheckSynchronize(100);
  {$ENDIF}
    FileSystemWatcher.WaitFor;
    FreeAndNil(FileSystemWatcher);
  end;
end;

class function TFileSystemWatcher.AddWatch(aWatchPath: UTF8String;
                                           aWatchFilter: TFSWatchFilter;
                                           aWatcherEvent: TFSWatcherEvent;
                                           UserData: Pointer = nil): Boolean;
begin
  CreateFileSystemWatcher;
  if Assigned(FileSystemWatcher) then
    Result := FileSystemWatcher.AddWatch(aWatchPath, aWatchFilter, aWatcherEvent, UserData)
  else
    Result := False;
end;

class procedure TFileSystemWatcher.RemoveWatch(aWatchPath: UTF8String;
                                               aWatcherEvent: TFSWatcherEvent);
begin
  if Assigned(FileSystemWatcher) then
  begin
    FileSystemWatcher.RemoveWatch(aWatchPath, aWatcherEvent);
    if FileSystemWatcher.WatchersCount = 0 then
      DestroyFileSystemWatcher;
  end;
end;

class procedure TFileSystemWatcher.RemoveWatch(aWatcherEvent: TFSWatcherEvent);
begin
  if Assigned(FileSystemWatcher) then
  begin
    FileSystemWatcher.RemoveWatch(aWatcherEvent);
    if FileSystemWatcher.WatchersCount = 0 then
      DestroyFileSystemWatcher;
  end;
end;

// ----------------------------------------------------------------------------

{$IF DEFINED(LINUX)}
type
  TINotifyRenameCookies = specialize TFPGMap<uint32_t, string>;
{$ENDIF}

procedure ShowError(const sErrMsg: String);
begin
  DCDebug('FSWatcher: ' + sErrMsg + ': ' + SysErrorMessage(GetLastOSError));
end;

{ TFileSystemWatcherImpl }

procedure TFileSystemWatcherImpl.Execute;
begin
  DCDebug('FileSystemWatcher thread starting');
  try
    try
      ExecuteWatcher;
    except
      on e: Exception do
      begin
        FExceptionMessage := e.Message;
        FExceptionBackTrace := ExceptionToString;

        if FExceptionBackTrace <> EmptyStr then
          DCDebug(FExceptionBackTrace);

        Synchronize(@ShowException);
      end;
    end;
  finally
    FFinished := True;
    DCDebug('FileSystemWatcher thread finished');
  end;
end;

procedure TFileSystemWatcherImpl.ExecuteWatcher;
{$IF DEFINED(MSWINDOWS)}
var
  dwWaitResult: DWORD;
  WaitHandles: TWOHandleArray;
  WaitObjectsCount: Integer = 0;
  i: Integer;
  SignaledHandle: THandle;
begin
  while not Terminated do
  begin
    WaitHandles[0] := FWaitEvent;

    FWatcherLock.Acquire;
    try
      WaitObjectsCount := min(FOSWatchers.Count + 1, MAXIMUM_WAIT_OBJECTS);

      for i := 0 to WaitObjectsCount - 2 do
        WaitHandles[i + 1] := FOSWatchers[i].Handle;

    finally
      FWatcherLock.Release;
    end; { try - finally }

    dwWaitResult := WaitForMultipleObjects(WaitObjectsCount, @WaitHandles[0], FALSE, INFINITE);

    case dwWaitResult of
      WAIT_OBJECT_0: ;  // FWaitEvent, User triggered

      WAIT_OBJECT_0 + 1 .. WAIT_OBJECT_0 + MAXIMUM_WAIT_OBJECTS - 1:
        begin
          if Terminated then
            Break;

          SignaledHandle := WaitHandles[dwWaitResult - WAIT_OBJECT_0];

          // While waiting, the watcher may have been removed and the signaled
          // handle may now be invalid. Need to confirm it is still there.

          FCurrentEventData.Path := EmptyStr;
          FWatcherLock.Acquire;
          try
            for i := 0 to FOSWatchers.Count - 1 do
            begin
              if FOSWatchers[i].Handle = SignaledHandle then
              begin
                FCurrentEventData.Path := FOSWatchers[i].WatchPath;
                FCurrentEventData.EventType := fswUnknownChange;
                FCurrentEventData.FileName := EmptyStr;
                FCurrentEventData.OldFileName := EmptyStr;

                if not FindNextChangeNotification(SignaledHandle) then
                begin
                  ShowError('FindNextChangeNotification failed');
                  RemoveOSWatch(SignaledHandle);
                end; { if }

                Break;
              end; { if }
            end; { for }

          finally
            FWatcherLock.Release;
          end; { try - finally }

          if FCurrentEventData.Path <> EmptyStr then
            Synchronize(@DoWatcherEvent);
        end;

      WAIT_FAILED:
        begin
          ShowError('WaitForMultipleObjects returned WAIT_FAILED');
          Break;
        end;

      else
        begin
          ShowError('WaitForMultipleObjects returned ' + IntToStr(dwWaitResult));
        Break;
        end;
    end; { case }

  end; { while }
end;
{$ELSEIF DEFINED(LINUX)}
const
  // Buffer size passed to read() must be at least the size of the first event
  // to be read from the file descriptor, otherwise Invalid Parameter is returned.
  // Event record size is variable, we use maximum possible for a single event.
  // Usually it is big enough so that multiple events can be read with single read().
  // The 'name' field is always padded up to multiple of 16 bytes with NULLs.
  buffer_size = sizeof(inotify_event) + MAX_PATH;
var
  bytes_to_parse, p, i: Integer;
  buf: PChar = nil;
  ev: pinotify_event;
  fds: TFDSet;
  nfds: cint;
  Cookies: TINotifyRenameCookies = nil;
  CookieIndex: Integer;
begin
  if (FNotifyHandle = feInvalidHandle) or
     (FEventPipe[0] = -1) or
     (FEventPipe[1] = -1) then
    Exit;

  try
    Cookies := TINotifyRenameCookies.Create;
    buf := GetMem(buffer_size);

    // get maximum file descriptor
    nfds := FEventPipe[0] + 1;
    if FNotifyHandle >= nfds then
      nfds := FNotifyHandle + 1;

    while not Terminated do
    begin
      // clear and set file descriptors
      fpFD_ZERO(fds);
      fpFD_SET(FEventPipe[0], fds);
      fpFD_SET(FNotifyHandle, fds);

      // wait for events
      if fpSelect(nfds, @fds, nil, nil, nil) = -1 then
      begin
        ShowError('select() failed');
        Exit;
      end; { if }

      if fpFD_ISSET(FEventPipe[0], fds) = 1 then
      begin
        // clear pipe
        while FpRead(FEventPipe[0], buf, 1) <> -1 do;
      end; { if }

      if fpFD_ISSET(FNotifyHandle, fds) = 0 then // inotify handle didn't change, so user triggered
        continue;

      // Read events.
      bytes_to_parse := fpread(FNotifyHandle, buf, buffer_size);
      if bytes_to_parse = -1 then
      begin
        ShowError('read(): failed');
        continue;
      end; { if }

      // parse events and print them
      p := 0;
      while p < bytes_to_parse do
      begin
        ev := pinotify_event(buf + p);

        for i := 0 to FOSWatchers.Count - 1 do
        begin
          if ev^.wd = FOSWatchers[i].Handle then
          begin
            with FCurrentEventData do
            begin
              Path := FOSWatchers[i].WatchPath;
              FileName := StrPas(PChar(@ev^.name));
              OldFileName := EmptyStr;

              if (ev^.mask and (IN_IGNORED or
                                IN_Q_OVERFLOW)) <> 0 then
                begin
                  // Ignore this event.
                  Break;
                end
              else if (ev^.mask and (IN_ACCESS or
                                     IN_MODIFY or
                                     IN_ATTRIB or
                                     IN_CLOSE or
                                     IN_OPEN)) <> 0 then
                begin
                  EventType := fswFileChanged;
                end
              else if (ev^.mask and IN_CREATE) <> 0 then
                begin
                  EventType := fswFileCreated;
                end
              else if (ev^.mask and (IN_DELETE or
                                     IN_DELETE_SELF)) <> 0 then
                begin
                  EventType := fswFileDeleted;
                end
              else if (ev^.mask and IN_MOVE_SELF) <> 0 then
                begin
                  EventType := fswFileRenamed;
                  OldFileName := FileName;
                end
              else if (ev^.mask and IN_MOVED_FROM) <> 0 then
                begin
                  Cookies.Add(ev^.cookie, FileName);
                  // Don't send event until IN_MOVED_TO is received.
                  Break;
                end
              else if (ev^.mask and IN_MOVED_TO) <> 0 then
                begin
                  EventType := fswFileRenamed;
                  CookieIndex := Cookies.IndexOf(ev^.cookie);
                  if CookieIndex >= 0 then
                  begin
                    OldFileName := Cookies.Data[CookieIndex];
                    Cookies.Delete(CookieIndex);
                  end;
                  // else
                  // Cookie has not been found but send event anyway
                  // just without OldFileName.
                end
              else
                EventType := fswUnknownChange;
            end;

            // call event handler
            Synchronize(@DoWatcherEvent);

            break;
          end; { if }
        end; { for }

        p := p + sizeof(inotify_event) + ev^.len;
      end; { while }

    end; { while }

  finally
    FreeAndNil(Cookies);
    if Assigned(buf) then
      FreeMem(buf);
  end; { try - finally }
end;
{$ELSEIF DEFINED(BSD)}
var
  ke: TKEvent;
begin
  if FNotifyHandle = feInvalidHandle then
    exit;

  while not Terminated do
  begin
    FillByte(ke, SizeOf(ke), 0);
    if kevent(FNotifyHandle, nil, 0, @ke, 1, nil) = -1 then
      break;

    case ke.Filter of
      EVFILT_TIMER: // user triggered
        continue;

      EVFILT_VNODE:
      begin
        with FCurrentEventData do
        begin
          Path := TOSWatch(ke.uData).WatchPath;
          EventType := fswUnknownChange;
          FileName := EmptyStr;
          OldFileName := EmptyStr;
        end;

        Synchronize(@DoWatcherEvent);
      end;
    end; { case }
  end; { while }
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TFileSystemWatcherImpl.DoWatcherEvent;
var
  i, j: Integer;
begin
  if not Terminated then
  begin
    FWatcherLock.Acquire;
    try
      for i := 0 to FOSWatchers.Count - 1 do
      begin
        if FOSWatchers[i].WatchPath = FCurrentEventData.Path then
        begin
          for j := 0 to FOSWatchers[i].Observers.Count - 1 do
          begin
            // TODO: Check filter.

            // Can be called under the lock because this function is run from
            // the main thread and the watcher thread is suspended anyway because
            // it's waiting until Synchronize call (thus this function) finishes.
            with FOSWatchers[i].Observers[j] do
            begin
              if Assigned(WatcherEvent) then
              begin
                FCurrentEventData.UserData := UserData;
                WatcherEvent(FCurrentEventData);
              end;
            end;
          end;

          Break;
        end; { if }
      end; { for }
    finally
      FWatcherLock.Release;
    end; { try - finally }
  end; { if }
end;

function TFileSystemWatcherImpl.GetWatchersCount: Integer;
begin
  FWatcherLock.Acquire;
  try
    Result := FOSWatchers.Count;
  finally
    FWatcherLock.Release;
  end; { try - finally }
end;

constructor TFileSystemWatcherImpl.Create;
begin
{$IF (fpc_version<2) or ((fpc_version=2) and (fpc_release<5))}
  // Workaround for race condition, see FPC Mantis #16884.
  inherited Create(True);
{$ELSE}
  inherited Create(False);
{$ENDIF}

  FOSWatchers := TOSWatchs.Create(True);
  FWatcherLock := syncobjs.TCriticalSection.Create;

  FreeOnTerminate := False;
  FFinished := False;

  {$IF DEFINED(MSWINDOWS)}
  FWaitEvent := CreateEvent(nil, FALSE, FALSE, nil);
  {$ELSEIF DEFINED(LINUX)}
  // create inotify instance
  FNotifyHandle := inotify_init();
  if FNotifyHandle < 0 then
    ShowError('inotify_init() failed');

  // create pipe for user triggered fake event
  FEventPipe[0] := -1;
  FEventPipe[1] := -1;
  if FpPipe(FEventPipe) = 0 then
  begin
    // set both ends of pipe non blocking
    FpFcntl(FEventPipe[0], F_SetFl, FpFcntl(FEventPipe[0], F_GetFl) or O_NONBLOCK);
    FpFcntl(FEventPipe[1], F_SetFl, FpFcntl(FEventPipe[1], F_GetFl) or O_NONBLOCK);
  end
  else
    ShowError('pipe() failed');
  {$ELSEIF DEFINED(BSD)}
  FNotifyHandle := kqueue();
  if FNotifyHandle = feInvalidHandle then
    ShowError('kqueue() failed');
  {$ELSEIF DEFINED(UNIX)}
  FNotifyHandle := feInvalidHandle;
  {$ENDIF}

{$IF (fpc_version<2) or ((fpc_version=2) and (fpc_release<5))}
  Resume;
{$ENDIF}
end;

destructor TFileSystemWatcherImpl.Destroy;
begin
  {$IF DEFINED(MSWINDOWS)}
  if FWaitEvent <> 0 then
  begin
    CloseHandle(FWaitEvent);
    FWaitEvent := 0;
  end;
  {$ELSEIF DEFINED(LINUX)}
  // close both ends of pipe
  if FEventPipe[0] <> -1 then
  begin
    FpClose(FEventPipe[0]);
    FEventPipe[0] := -1;
  end;
  if FEventPipe[1] <> -1 then
  begin
    FpClose(FEventPipe[1]);
    FEventPipe[1] := -1;
  end;
  if FNotifyHandle <> feInvalidHandle then
  begin
    FpClose(FNotifyHandle);
    FNotifyHandle := feInvalidHandle;
  end;
  {$ELSEIF DEFINED(BSD)}
  if FNotifyHandle <> feInvalidHandle then
  begin
    FpClose(FNotifyHandle);
    FNotifyHandle := feInvalidHandle;
  end;
  {$ENDIF}

  if Assigned(FOSWatchers) then
    FreeAndNil(FOSWatchers);

  if Assigned(FWatcherLock) then
    FreeAndNil(FWatcherLock);

  inherited Destroy;
end;

procedure TFileSystemWatcherImpl.Terminate;
begin
  inherited Terminate;

  TriggerEvent;
end;

function TFileSystemWatcherImpl.AddWatch(aWatchPath: UTF8String;
                                         aWatchFilter: TFSWatchFilter;
                                         aWatcherEvent: TFSWatcherEvent;
                                         UserData: Pointer): Boolean;
var
  OSWatcher: TOSWatch = nil;
  OSWatcherCreated: Boolean = False;
  Observer: TOSWatchObserver;
  i, j: Integer;
begin
  if (aWatchPath = '') or (aWatcherEvent = nil) then
    Exit(False);

{$IFDEF UNIX}
  if aWatchPath <> PathDelim then
{$ENDIF}
    aWatchPath := ExcludeTrailingPathDelimiter(aWatchPath);

  // Check if the path is not already watched.
  FWatcherLock.Acquire;
  try
    for i := 0 to FOSWatchers.Count - 1 do
      if FOSWatchers[i].WatchPath = aWatchPath then
      begin
        OSWatcher := FOSWatchers[i];

        // Check if the observer is not already registered.
        for j := 0 to OSWatcher.Observers.Count - 1 do
        begin
          if CompareMethods(TMethod(OSWatcher.Observers[j].WatcherEvent), TMethod(aWatcherEvent)) then
            Exit(True);
        end;

        Break;
      end;
  finally
    FWatcherLock.Release;
  end;

  if not Assigned(OSWatcher) then
  begin
    OSWatcher := TOSWatch.Create(aWatchPath {$IFDEF UNIX}, FNotifyHandle {$ENDIF});
    OSWatcherCreated := True;
  end;

  Observer := TOSWatchObserver.Create;
  Observer.WatchFilter := aWatchFilter;
  Observer.WatcherEvent := aWatcherEvent;
  Observer.UserData := UserData;

  FWatcherLock.Acquire;
  try
    if OSWatcherCreated then
      FOSWatchers.Add(OSWatcher);

    OSWatcher.Observers.Add(Observer);
    OSWatcher.UpdateFilter; // This recreates handle.

    Result := OSWatcher.Handle <> feInvalidHandle;

    // Remove watcher if could not create notification handle.
    if not Result then
      FOSWatchers.Remove(OSWatcher);

  finally
    FWatcherLock.Release;
  end;

{$IF DEFINED(MSWINDOWS)}
  // trigger fake event to update event handle list
  TriggerEvent;
{$ENDIF}
end;

procedure TFileSystemWatcherImpl.RemoveWatch(aWatchPath: UTF8String;
                                             aWatcherEvent: TFSWatcherEvent);
var
  i, j: Integer;
begin
{$IFDEF UNIX}
  if aWatchPath <> PathDelim then
{$ENDIF}
    aWatchPath := ExcludeTrailingPathDelimiter(aWatchPath);

  FWatcherLock.Acquire;
  try
    for i := 0 to FOSWatchers.Count - 1 do
    begin
      if FOSWatchers[i].WatchPath = aWatchPath then
      begin
        for j := 0 to FOSWatchers[i].Observers.Count - 1 do
        begin
          if CompareMethods(TMethod(FOSWatchers[i].Observers[j].WatcherEvent), TMethod(aWatcherEvent)) then
          begin
            FOSWatchers[i].Observers.Delete(j);
            Break;
          end;
        end;

        if FOSWatchers[i].Observers.Count = 0 then
          FOSWatchers.Delete(i)
        else
          FOSWatchers[i].UpdateFilter;

        Break;
      end;
    end;
  finally
    FWatcherLock.Release;
  end;

{$IF DEFINED(MSWINDOWS)}
  // trigger fake event to update event handle list
  TriggerEvent;
{$ENDIF}
end;

procedure TFileSystemWatcherImpl.RemoveWatch(aWatcherEvent: TFSWatcherEvent);
var
  i, j: Integer;
begin
  FWatcherLock.Acquire;
  try
    for i := 0 to FOSWatchers.Count - 1 do
    begin
      for j := 0 to FOSWatchers[i].Observers.Count - 1 do
      begin
        if CompareMethods(TMethod(FOSWatchers[i].Observers[j].WatcherEvent), TMethod(aWatcherEvent)) then
          FOSWatchers[i].Observers.Delete(j);
      end;

      if FOSWatchers[i].Observers.Count = 0 then
        FOSWatchers.Delete(i)
      else
        FOSWatchers[i].UpdateFilter;
    end;
  finally
    FWatcherLock.Release;
  end;

{$IF DEFINED(MSWINDOWS)}
  // trigger fake event to update event handle list
  TriggerEvent;
{$ENDIF}
end;

procedure TFileSystemWatcherImpl.RemoveOSWatch(aHandle: THandle);
var
  i: Integer;
begin
  FWatcherLock.Acquire;
  try
    for i := 0 to FOSWatchers.Count - 1 do
    begin
      if FOSWatchers[i].Handle = aHandle then
      begin
        FOSWatchers.Delete(i);
        Break;
      end;
    end;
  finally
    FWatcherLock.Release;
  end;

{$IF DEFINED(MSWINDOWS)}
  // trigger fake event to update event handle list
  TriggerEvent;
{$ENDIF}
end;

procedure TFileSystemWatcherImpl.ShowException;
begin
  WriteExceptionToErrorFile(FExceptionBackTrace);
  ShowExceptionDialog(FExceptionMessage);
end;

procedure TFileSystemWatcherImpl.TriggerEvent;
{$IF DEFINED(MSWINDOWS)}
begin
  SetEvent(FWaitEvent);
end;
{$ELSEIF DEFINED(LINUX)}
var
  buf: Char;
begin
  // check if thread has been started
  if Self.FNotifyHandle <> feInvalidHandle then
  begin
    buf := #0;
    FpWrite(FEventPipe[1], buf, 1);
  end; { if }
end;
{$ELSEIF DEFINED(BSD)}
var
  ke: TKEvent;
begin
  // check if thread has been started
  if Self.FNotifyHandle <> feInvalidHandle then
  begin
    FillByte(ke, SizeOf(ke), 0);
    EV_SET(@ke, 0, EVFILT_TIMER, EV_ADD or EV_ONESHOT, 0, 0, nil);
    if kevent(FNotifyHandle, @ke, 1, nil, 0, nil) = -1 then
    begin
      ShowError('ERROR: kevent()');
    end; { if }
  end; { if }
end;
{$ELSE}
begin
end;
{$ENDIF}

// ----------------------------------------------------------------------------

{ TOSWatch }

constructor TOSWatch.Create(const aWatchPath: UTF8String
                            {$IFDEF UNIX}; aNotifyHandle: THandle{$ENDIF});
begin
  FObservers := TOSWatchObservers.Create(True);
  FWatchFilter := [];
  FWatchPath := aWatchPath;
  {$IFDEF UNIX}
  FNotifyHandle := aNotifyHandle;
  {$ENDIF}
  FHandle := feInvalidHandle;
end;

destructor TOSWatch.Destroy;
begin
  FObservers.Free;
  DestroyHandle;
end;

procedure TOSWatch.UpdateFilter;
var
  i: Integer;
  NewFilter: TFSWatchFilter = [];
begin
  for i := 0 to Observers.Count - 1 do
    NewFilter := NewFilter + Observers[i].WatchFilter;

  if FWatchFilter <> NewFilter then
  begin
    FWatchFilter := NewFilter;

    // Change watcher filter or recreate watcher.
    DestroyHandle;
    CreateHandle;
  end;
end;

procedure TOSWatch.CreateHandle;
{$IF DEFINED(MSWINDOWS)}
var
  hNotifyFilter: LongWord = 0;
begin
  if wfFileNameChange in FWatchFilter then
    hNotifyFilter:= hNotifyFilter or FILE_NOTIFY_CHANGE_FILE_NAME
                                  or FILE_NOTIFY_CHANGE_DIR_NAME;
  if wfAttributesChange in FWatchFilter then
    hNotifyFilter:= hNotifyFilter or FILE_NOTIFY_CHANGE_ATTRIBUTES;

  FHandle := FindFirstChangeNotificationW(PWideChar(UTF8Decode(FWatchPath)), False, hNotifyFilter);
  if FHandle = INVALID_HANDLE_VALUE then
  begin
    FHandle := feInvalidHandle;
    ShowError('FindFirstChangeNotificationW failed for ' + FWatchPath);
  end;
end;
{$ELSEIF DEFINED(LINUX)}
var
  hNotifyFilter: uint32_t = 0;
begin
  if wfFileNameChange in FWatchFilter then
    hNotifyFilter := hNotifyFilter or IN_CREATE or IN_DELETE or IN_DELETE_SELF or IN_MOVE or IN_MOVE_SELF;
  if wfAttributesChange in FWatchFilter then
    hNotifyFilter := hNotifyFilter or IN_ATTRIB;

  FHandle := inotify_add_watch(FNotifyHandle, PChar(FWatchPath), hNotifyFilter);
  if FHandle < 0 then
  begin
    FHandle := feInvalidHandle;
    ShowError('inotify_add_watch() failed for ' + FWatchPath);
  end;
end;
{$ELSEIF DEFINED(BSD)}
var
  ke: TKEvent;
  hNotifyFilter: cuint = 0;
begin
  if wfFileNameChange in FWatchFilter then
    hNotifyFilter := hNotifyFilter or NOTE_DELETE or NOTE_WRITE or NOTE_EXTEND or NOTE_RENAME;
  if wfAttributesChange in FWatchFilter then
    hNotifyFilter := hNotifyFilter or NOTE_ATTRIB or NOTE_REVOKE;

  FHandle := fpOpen(PChar(FWatchPath), O_RDONLY);
  if FHandle < 0 then
  begin
    FHandle := feInvalidHandle;
    ShowError('failed to open file ' + FWatchPath);
  end
  else
  begin
    FillByte(ke, SizeOf(ke), 0);
    EV_SET(@ke, FHandle, EVFILT_VNODE, EV_ADD or EV_CLEAR,  hNotifyFilter, 0, Self);
    if kevent(FNotifyHandle, @ke, 1, nil, 0, nil) = -1 then
    begin
      DestroyHandle;
      ShowError('kevent failed');
    end; { if }
  end;
end;
{$ELSE}
begin
  FHandle := feInvalidHandle;
end;
{$ENDIF}

procedure TOSWatch.DestroyHandle;
begin
  if FHandle <> feInvalidHandle then
  begin
    {$IF DEFINED(LINUX)}
    inotify_rm_watch(FNotifyHandle, FHandle);
    {$ENDIF}
    {$IF DEFINED(BSD)}
    FpClose(FHandle);
    {$ENDIF}
    {$IF DEFINED(MSWINDOWS)}
    if FindCloseChangeNotification(FHandle) = False then
      ShowError('FindCloseChangeNotification error');
    {$ENDIF}
    FHandle := feInvalidHandle;
  end;
end;

finalization
  TFileSystemWatcher.DestroyFileSystemWatcher;

end.
