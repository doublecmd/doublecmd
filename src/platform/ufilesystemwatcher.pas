{
    Double Commander
    -------------------------------------------------------------------------
    This is a thread-component sends an event when a change in the file system occurs.

    Copyright (C) 2009-2021 Alexander Koblov (alexx2000@mail.ru)
    Copyright (C) 2011      Przemyslaw Nagay (cobines@gmail.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uFileSystemWatcher;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLVersion;

//{$DEFINE DEBUG_WATCHER}

type
  TFSWatchFilter = set of (wfFileNameChange, wfAttributesChange);

  TFSWatcherEventType = (fswFileCreated,
                         fswFileChanged,
                         fswFileDeleted,
                         fswFileRenamed,
                         fswSelfDeleted,
                         fswUnknownChange);
  TFSWatcherEventTypes = set of TFSWatcherEventType;

  TFSWatcherEventData = record
    Path: String;
    EventType: TFSWatcherEventType;
    FileName: String;    // Valid for fswFileCreated, fswFileChanged, fswFileDeleted, fswFileRenamed
    NewFileName: String; // Valid for fswFileRenamed
    UserData: Pointer;
  end;
  PFSWatcherEventData = ^TFSWatcherEventData;

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
    class function AddWatch(aWatchPath: String;
                            aWatchFilter: TFSWatchFilter;
                            aWatcherEvent: TFSWatcherEvent;
                            UserData: Pointer = nil): Boolean;
    class procedure RemoveWatch(aWatchPath: String;
                                aWatcherEvent: TFSWatcherEvent);
    class procedure RemoveWatch(aWatcherEvent: TFSWatcherEvent);
    class function CanWatch(const WatchPaths: array of String): Boolean;
  end;

implementation

uses
  LCLProc, LazUTF8, LazMethodList, uDebug, uExceptions, syncobjs, fgl
  {$IF DEFINED(MSWINDOWS)}
  , Windows, JwaWinNT, JwaWinBase, DCWindows, DCStrUtils, uGlobs, DCOSUtils,
    DCConvertEncoding
  {$ELSEIF DEFINED(LINUX)}
  , inotify, BaseUnix, FileUtil, DCConvertEncoding, DCUnix
  {$ELSEIF DEFINED(BSD)}
  , BSD, Unix, BaseUnix, UnixType, FileUtil, DCOSUtils
  {$ENDIF};

{$if lcl_fullversion < 2030000}
  {$macro on}
  {$define SameMethod:= CompareMethods}
{$endif}

{$IF DEFINED(MSWINDOWS)}
const
  // For each outstanding ReadDirectoryW a buffer of this size will be allocated
  // by kernel, so this value should be rather small.
  READDIRECTORYCHANGESW_BUFFERSIZE       = 4096;
  READDIRECTORYCHANGESW_DRIVE_BUFFERSIZE = 32768;

var
  VAR_READDIRECTORYCHANGESW_BUFFERSIZE: DWORD = READDIRECTORYCHANGESW_BUFFERSIZE;
  CREATEFILEW_SHAREMODE: DWORD = FILE_SHARE_READ or FILE_SHARE_WRITE;

type
  TOverlappedEx = packed record
    Overlapped: TOverlapped;
    OSWatch: Pointer;
  end;
  POverlappedEx = ^TOverlappedEx;

function GetTargetPath(const Path: String): String;
begin
  Result := mbReadAllLinks(Path);
  if Result = EmptyStr then
    Result := Path;
end;

function GetDriveOfPath(const Path: String): String;
begin
  Result := ExtractFileDrive(GetTargetPath(Path)) + PathDelim;
end;
{$ENDIF}

type
  TOSWatchObserver = class
    UserData: Pointer;
    WatcherEvent: TFSWatcherEvent;
    WatchFilter: TFSWatchFilter;
    {$IF DEFINED(MSWINDOWS)}
    RegisteredWatchPath: String; // Path that was registered to watch (for watching whole drive mode).
    TargetWatchPath: String;     // What path is actually to be watched (for watching whole drive mode).
    {$ENDIF}
  end;
  TOSWatchObservers = specialize TFPGObjectList<TOSWatchObserver>;

  TOSWatch = class
  private
    FHandle: THandle;
    FObservers: TOSWatchObservers;
    FWatchFilter: TFSWatchFilter;
    FWatchPath: String;
    {$IF DEFINED(MSWINDOWS)}
    FOverlapped: TOverlappedEx;
    FBuffer: PByte;
    FNotifyFilter: DWORD;
    FReferenceCount: LongInt;
    FOldFileName: String; // for FILE_ACTION_RENAMED_OLD_NAME action
    {$ENDIF}
    {$IF DEFINED(UNIX)}
    FNotifyHandle: THandle;
    {$ENDIF}
    procedure CreateHandle;
    procedure DestroyHandle;
    {$IF DEFINED(MSWINDOWS)}
    procedure QueueCancelRead;
    procedure QueueRead;
    procedure SetFilter(aWatchFilter: TFSWatchFilter);
    {$ENDIF}
  public
    constructor Create(const aWatchPath: String
                       {$IFDEF UNIX}; aNotifyHandle: THandle{$ENDIF}); reintroduce;
    destructor Destroy; override;
    procedure UpdateFilter;
    {$IF DEFINED(MSWINDOWS)}
    procedure Reference{$IFDEF DEBUG_WATCHER}(s: String){$ENDIF};
    procedure Dereference{$IFDEF DEBUG_WATCHER}(s: String){$ENDIF};
    {$ENDIF}
    property Handle: THandle read FHandle;
    property Observers: TOSWatchObservers read FObservers;
    property WatchPath: String read FWatchPath;
  end;
  TOSWatchs = specialize TFPGObjectList<TOSWatch>;

  { TFileSystemWatcherImpl }

  TFileSystemWatcherImpl = class(TThread)
  private
    FWatcherLock: syncobjs.TCriticalSection;
    FOSWatchers: TOSWatchs;
    {$IF DEFINED(UNIX)}
    FNotifyHandle: THandle;
    {$ENDIF}
    {$IF DEFINED(LINUX)}
    FEventPipe: TFilDes;
    {$ENDIF}
    FCurrentEventData: TFSWatcherEventData;
    FFinished: Boolean;

    procedure DoWatcherEvent;
    function GetWatchersCount: Integer;
    function GetWatchPath(var aWatchPath: String): Boolean;
    {$IF DEFINED(MSWINDOWS)}
    function IsPathObserved(Watch: TOSWatch; FileName: String): Boolean;
    {$ENDIF}
    {en
       Call only under FWatcherLock.
    }
    procedure RemoveObserverLocked(OSWatcherIndex: Integer; aWatcherEvent: TFSWatcherEvent);
    {en
       Call only under FWatcherLock.
    }
    procedure RemoveOSWatchLocked(Index: Integer);
    procedure RemoveOSWatch(Watch: TOSWatch);
    procedure TriggerTerminateEvent;
  protected
    procedure Execute; override;
    procedure ExecuteWatcher;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Terminate;
    function AddWatch(aWatchPath: String;
                      aWatchFilter: TFSWatchFilter;
                      aWatcherEvent: TFSWatcherEvent;
                      UserData: Pointer = nil): Boolean;
    procedure RemoveWatch(aWatchPath: String;
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

class function TFileSystemWatcher.AddWatch(aWatchPath: String;
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

class procedure TFileSystemWatcher.RemoveWatch(aWatchPath: String;
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

class function TFileSystemWatcher.CanWatch(const WatchPaths: array of String): Boolean;
{$IF DEFINED(MSWINDOWS)}
var
  Index: Integer;
  DrivePath: UnicodeString;
begin
  for Index:= Low(WatchPaths) to High(WatchPaths) do
  begin
    if (Pos('\\', WatchPaths[Index]) = 1) then Exit(False);
    DrivePath:= UnicodeString(Copy(WatchPaths[Index], 1, 3));
    if GetDriveTypeW(PWideChar(DrivePath)) = DRIVE_REMOTE then Exit(False);
  end;
  Result:= True;
end;
{$ELSE}
begin
  Result:= True;
end;
{$ENDIF}

// ----------------------------------------------------------------------------

procedure ShowError(const sErrMsg: String);
begin
  DCDebug('FSWatcher: ' + sErrMsg + ': (' + IntToStr(GetLastOSError) + ') ' +
          SysErrorMessage(GetLastOSError));
end;

{$IF DEFINED(MSWINDOWS)}
procedure NotifyRoutine(dwErrorCode: DWORD; dwNumberOfBytes: DWORD; Overlapped: LPOVERLAPPED); stdcall; forward;

function StartReadDirectoryChanges(Watch: TOSWatch): Boolean;
begin
  {$IFDEF DEBUG_WATCHER}
  DCDebug('FSWatcher: ReadChanges for ', Watch.FWatchPath);
  {$ENDIF}
  if Watch.Handle <> feInvalidHandle then
  begin
    Result := ReadDirectoryChangesW(
                Watch.Handle,
                Watch.FBuffer,
                VAR_READDIRECTORYCHANGESW_BUFFERSIZE,
                gWatcherMode = fswmWholeDrive,
                Watch.FNotifyFilter,
                nil,
                LPOVERLAPPED(@Watch.FOverlapped),
                @NotifyRoutine)
            or
              // ERROR_IO_PENDING is a confirmation that the I/O operation has started.
              (GetLastError = ERROR_IO_PENDING);

    if Result then
      Watch.Reference{$IFDEF DEBUG_WATCHER}('StartReadDirectoryChanges'){$ENDIF}
    else
    begin
      // ERROR_INVALID_HANDLE will be when handle was destroyed
      // just before the call to ReadDirectoryChangesW.
      if GetLastError <> ERROR_INVALID_HANDLE then
        ShowError('ReadDirectoryChangesW error');
    end;
  end
  else
    Result := False;
end;

procedure ProcessFileNotifyInfo(Watch: TOSWatch; dwBytesReceived: DWORD);
var
  wFilename: Widestring;
  fnInfo: PFILE_NOTIFY_INFORMATION;
begin
  with FileSystemWatcher do
  begin
    FCurrentEventData.Path := Watch.WatchPath;

    if dwBytesReceived = 0 then
    begin
    {$IFDEF DEBUG_WATCHER}
      DCDebug('FSWatcher: Process watch ', hexStr(Watch), ': Buffer overflowed. Some events happened though.');
    {$ENDIF}
      // Buffer was not large enough to store all events. In this case it is only
      // known that something has changed but all specific events have been lost.
      FCurrentEventData.EventType := fswUnknownChange;
      FCurrentEventData.FileName := EmptyStr;
      FCurrentEventData.NewFileName := EmptyStr;
      Synchronize(@DoWatcherEvent);
      Exit;
    end;

    fnInfo := @Watch.FBuffer[0];

    // FCurrentEventData can be accessed safely because only one ProcessFileNotifyInfo
    // is called at a time due to completion routines being in a queue.
    while True do
    begin
      SetString(wFilename, PWideChar(@fnInfo^.FileName), fnInfo^.FileNameLength div SizeOf(WideChar));
      FCurrentEventData.NewFileName := EmptyStr;

      case fnInfo^.Action of
        FILE_ACTION_ADDED:
          begin
            FCurrentEventData.FileName := UTF16ToUTF8(wFilename);
            FCurrentEventData.EventType := fswFileCreated;
            {$IFDEF DEBUG_WATCHER}
            DCDebug('FSWatcher: Process watch ', hexStr(Watch), ': Created file ',
              IncludeTrailingPathDelimiter(Watch.WatchPath) + FCurrentEventData.FileName);
            {$ENDIF}
          end;
        FILE_ACTION_REMOVED:
          begin
            FCurrentEventData.FileName := UTF16ToUTF8(wFilename);
            FCurrentEventData.EventType := fswFileDeleted;
            {$IFDEF DEBUG_WATCHER}
            DCDebug('FSWatcher: Process watch ', hexStr(Watch), ': Deleted file ',
              IncludeTrailingPathDelimiter(Watch.WatchPath) + FCurrentEventData.FileName);
            {$ENDIF}
          end;
        FILE_ACTION_MODIFIED:
          begin
            FCurrentEventData.FileName := UTF16ToUTF8(wFilename);
            FCurrentEventData.EventType := fswFileChanged;
            {$IFDEF DEBUG_WATCHER}
            DCDebug('FSWatcher: Process watch ', hexStr(Watch), ': Modified file ',
              IncludeTrailingPathDelimiter(Watch.WatchPath) + FCurrentEventData.FileName);
            {$ENDIF}
          end;
        FILE_ACTION_RENAMED_OLD_NAME:
          begin
            Watch.FOldFileName := UTF16ToUTF8(wFilename);
            {$IFDEF DEBUG_WATCHER}
            DCDebug('FSWatcher: Process watch ', hexStr(Watch), ': Rename from ',
              IncludeTrailingPathDelimiter(Watch.WatchPath) + FCurrentEventData.FileName);
            {$ENDIF}
          end;
        FILE_ACTION_RENAMED_NEW_NAME:
          begin
            FCurrentEventData.FileName := Watch.FOldFileName;
            FCurrentEventData.NewFileName := UTF16ToUTF8(wFilename);
            FCurrentEventData.EventType := fswFileRenamed;
            {$IFDEF DEBUG_WATCHER}
            DCDebug('FSWatcher: Process watch ', hexStr(Watch), ': Rename to ',
              IncludeTrailingPathDelimiter(Watch.WatchPath) + FCurrentEventData.FileName);
            {$ENDIF}
          end;
        else
          begin
            FCurrentEventData.EventType := fswUnknownChange;
            FCurrentEventData.FileName := EmptyStr;
            {$IFDEF DEBUG_WATCHER}
            DCDebug(['FSWatcher: Process watch ', hexStr(Watch), ': Action ', fnInfo^.Action, ' for ',
              IncludeTrailingPathDelimiter(Watch.WatchPath) + FCurrentEventData.FileName]);
            {$ENDIF}
          end;
      end;

      if (fnInfo^.Action <> FILE_ACTION_RENAMED_OLD_NAME) and
         ((gWatcherMode <> fswmWholeDrive) or IsPathObserved(Watch, FCurrentEventData.FileName)) then
        Synchronize(@DoWatcherEvent);

      if fnInfo^.NextEntryOffset = 0 then
        Break
      else
        fnInfo := PFILE_NOTIFY_INFORMATION(PByte(fnInfo) + fnInfo^.NextEntryOffset);
    end;
  end;
end;

procedure NotifyRoutine(dwErrorCode: DWORD; dwNumberOfBytes: DWORD; Overlapped: LPOVERLAPPED); stdcall;
var
  Watch: TOSWatch;
  bReadStarted: Boolean = False;
begin
  Watch := TOSWatch(POverlappedEx(Overlapped)^.OSWatch);

  {$IFDEF DEBUG_WATCHER}
  DCDebug(['FSWatcher: NotifyRoutine for watch ', hexStr(Watch), ' bytes=', dwNumberOfBytes, ' code=', dwErrorCode, ' handle=', Integer(Watch.Handle)]);
  {$ENDIF}
  case dwErrorCode of
    ERROR_SUCCESS:
      begin
        if Watch.FHandle <> feInvalidHandle then
        begin
          ProcessFileNotifyInfo(Watch, dwNumberOfBytes);
          bReadStarted := StartReadDirectoryChanges(Watch);
        end
        else
        begin
          {$IFDEF DEBUG_WATCHER}
          DCDebug('FSWatcher: NotifyRoutine Handle destroyed, not starting Read');
          {$ENDIF};
        end;
      end;
    ERROR_OPERATION_ABORTED:
      begin
        // I/O operation has been cancelled to change parameters.
        {$IFDEF DEBUG_WATCHER}
        DCDebug('FSWatcher: NotifyRoutine aborted, will restart');
        {$ENDIF}
        bReadStarted := StartReadDirectoryChanges(Watch);
      end;
    ERROR_ACCESS_DENIED:
      begin
        // Most probably handle has been closed or become invalid.
        {$IFDEF DEBUG_WATCHER}
        DCDebug(['FSWatcher: NotifyRoutine ERROR_ACCESS_DENIED watch=', hexStr(Watch)]);
        {$ENDIF}
      end;
    else
      begin
        DCDebug(['FSWatcher: NotifyRoutine error=', dwErrorCode]);
      end;
  end;

  if not bReadStarted then
  begin
    if Watch.Handle <> feInvalidHandle then
      // This will destroy the handle.
      FileSystemWatcher.RemoveOSWatch(Watch);
    // If Handle = feInvalidHandle that means Watch has already been
    // removed from FileSystemWatcher by main thread.
  end;

  Watch.Dereference{$IFDEF DEBUG_WATCHER}('NotifyRoutine'){$ENDIF};

  {$IFDEF DEBUG_WATCHER}
  DCDebug(['FSWatcher: NotifyRoutine for watch ', hexStr(Watch), ' done']);
  {$ENDIF}
end;

procedure ReadChangesProc(dwParam: ULONG_PTR); stdcall;
var
  Watch: TOSWatch absolute dwParam;
begin
  {$IFDEF DEBUG_WATCHER}
  DCDebug('FSWatcher: ReadChangesProc for watch ', hexStr(Watch));
  {$ENDIF}
  if not StartReadDirectoryChanges(Watch) then
  begin
    if Watch.Handle <> feInvalidHandle then
      FileSystemWatcher.RemoveOSWatch(Watch);
  end;
  Watch.Dereference{$IFDEF DEBUG_WATCHER}('ReadChangesProc'){$ENDIF};
  {$IFDEF DEBUG_WATCHER}
  DCDebug('FSWatcher: ReadChangesProc done for watch ', hexStr(Watch));
  {$ENDIF}
end;

procedure CancelReadChangesProc(dwParam: ULONG_PTR); stdcall;
var
  Watch: TOSWatch absolute dwParam;
begin
  {$IFDEF DEBUG_WATCHER}
  DCDebug(['FSWatcher: CancelReadChangesProc for watch ', hexStr(Watch), ' handle ', Integer(Watch.Handle)]);
  {$ENDIF}
  // CancelIo will cause the completion routine to be called with ERROR_OPERATION_ABORTED.
  // Must be called from the same thread which started the I/O operation.
  if CancelIo(Watch.Handle) = False then
  begin
    if GetLastOSError <> ERROR_INVALID_HANDLE then
      ShowError('CancelReadChangesProc: CancelIo error');
  end;
  Watch.Dereference{$IFDEF DEBUG_WATCHER}('CancelReadChangesProc'){$ENDIF};
  {$IFDEF DEBUG_WATCHER}
  DCDebug('FSWatcher: CancelReadChangesProc done for watch ', hexStr(Watch));
  {$ENDIF}
end;

procedure TerminateProc(dwParam: ULONG_PTR); stdcall;
begin
  // This procedure does nothing. Simply queueing and executing it will cause
  // SleepEx to exit if there were no other APCs in the queue.
end;
{$ENDIF}

{ TFileSystemWatcherImpl }

procedure TFileSystemWatcherImpl.Execute;
begin
  DCDebug('FileSystemWatcher thread starting');
  try
    try
      ExecuteWatcher;
    except
      on e: Exception do
        HandleException(e, Self);
    end;
  finally
    FFinished := True;
    DCDebug('FileSystemWatcher thread finished');
  end;
end;

procedure TFileSystemWatcherImpl.ExecuteWatcher;
{$IF DEFINED(MSWINDOWS)}
begin
  while not Terminated do
  begin
  {$IFDEF DEBUG_WATCHER}
    DCDebug(['FSWatcher: SleepEx (', FOSWatchers.Count, ' watches)']);
  {$ENDIF}
    // Contrary to documentation:
    // SleepEx does not return until all APCs (including I/O completion routines)
    // in queue are called. Then it returns with WAIT_IO_COMPLETION.
    // Therefore there is no need to artificially flush queue.
    SleepEx(INFINITE, True);
  end;
  {$IFDEF DEBUG_WATCHER}
  DCDebug('FSWatcher: SleepEx loop done');
  {$ENDIF}
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
  fds: array[0..1] of tpollfd;
  ret: cint;
begin
  if (FNotifyHandle = feInvalidHandle) or
     (FEventPipe[0] = -1) or
     (FEventPipe[1] = -1) then
    Exit;

  try
    buf := GetMem(buffer_size);

    // set file descriptors
    fds[0].fd:= FEventPipe[0];
    fds[0].events:= POLLIN;

    fds[1].fd:= FNotifyHandle;
    fds[1].events:= POLLIN;

    while not Terminated do
    begin
      // wait for events
      repeat
        ret:= fpPoll(@fds[0], Length(fds), -1);
      until (ret <> -1) or (fpGetErrNo <> ESysEINTR);

      if ret = -1 then
      begin
        ShowError('fpPoll() failed');
        Exit;
      end; { if }

      if (fds[0].revents and POLLIN <> 0) then
      begin
        // clear pipe
        while FileRead(FEventPipe[0], buf^, 1) <> -1 do;
      end; { if }

      if (fds[1].revents and POLLIN = 0) then // inotify handle didn't change, so user triggered
        Continue;

      // Read events.
      bytes_to_parse := FileRead(FNotifyHandle, buf^, buffer_size);
      if bytes_to_parse = -1 then
      begin
        ShowError('read(): failed');
        Continue;
      end; { if }

      // parse events and print them
      p := 0;
      while p < bytes_to_parse do
      begin
        ev := pinotify_event(buf + p);

        {$IFDEF DEBUG_WATCHER}
        DCDebug('FSWatcher: Read event, mask %s, name %s', [HexStr(ev^.mask, 8), StrPas(PChar(@ev^.name))]);
        {$ENDIF};

        for i := 0 to FOSWatchers.Count - 1 do
        begin
          if ev^.wd = FOSWatchers[i].Handle then
          begin
            with FCurrentEventData do
            begin
              Path := FOSWatchers[i].WatchPath;
              FileName := StrPas(PChar(@ev^.name));
              NewFileName := EmptyStr;

              // IN_MOVED_FROM is converted to FileDelete.
              // IN_MOVED_TO is converted to FileCreate.
              // There is no guarantee we will receive as sequence of
              // IN_MOVED_FROM, IN_MOVED_TO as the events are only sent
              // if the source and destination directories respectively
              // are being watched.

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
                                     IN_OPEN or
                                     IN_CLOSE_WRITE or
                                     IN_CLOSE_NOWRITE)) <> 0 then
                begin
                  EventType := fswFileChanged;
                end
              else if (ev^.mask and (IN_CREATE or
                                     IN_MOVED_TO)) <> 0 then
                begin
                  EventType := fswFileCreated;
                end
              else if (ev^.mask and (IN_DELETE or
                                     IN_MOVED_FROM)) <> 0 then
                begin
                  EventType := fswFileDeleted;
                end
              else if (ev^.mask and (IN_DELETE_SELF or
                                     IN_MOVE_SELF)) <> 0 then
                begin
                  // Watched file/directory was deleted or moved.
                  EventType := fswSelfDeleted;
                end
              else begin
                EventType := fswUnknownChange;
              end;
              {$IFDEF DEBUG_WATCHER}
              DCDebug('FSWatcher: Send event, Path %s, FileName %s, EventType %d', [Path, FileName, EventType]);
              {$ENDIF};
            end;

            // call event handler
            Synchronize(@DoWatcherEvent);

            Break;
          end; { if }
        end; { for }

        p := p + sizeof(inotify_event) + ev^.len;
      end; { while }

    end; { while }

  finally
    if Assigned(buf) then
      FreeMem(buf);
  end; { try - finally }
end;
{$ELSEIF DEFINED(BSD)}
var
  ret: cint;
  ke: TKEvent;
begin
  if FNotifyHandle = feInvalidHandle then
    exit;

  while not Terminated do
  begin
    FillByte(ke, SizeOf(ke), 0);

    // Wait for events
    repeat
      ret:= kevent(FNotifyHandle, nil, 0, @ke, 1, nil);
    until (ret <> -1) or (fpGetErrNo <> ESysEINTR);

    if ret = -1 then
    begin
      ShowError('kevent() failed');
      Break;
    end; { if }

    case ke.Filter of
      EVFILT_TIMER: // user triggered
        Continue;

      EVFILT_VNODE:
      begin
        with FCurrentEventData do
        begin
          Path := TOSWatch(ke.uData).WatchPath;
          EventType := fswUnknownChange;
          FileName := EmptyStr;
          NewFileName := EmptyStr;
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
    try
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
                if Assigned(WatcherEvent)
                {$IFDEF MSWINDOWS}
                and ((gWatcherMode <> fswmWholeDrive) or
                      IsInPath(TargetWatchPath,
                               UTF8UpperCase(FOSWatchers[i].WatchPath + FCurrentEventData.FileName),
                               False, False))
                {$ENDIF}
                then
                begin
                  FCurrentEventData.UserData := UserData;
                  {$IFDEF MSWINDOWS}
                  if gWatcherMode = fswmWholeDrive then
                    FCurrentEventData.Path := RegisteredWatchPath;
                  {$ENDIF}
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
    except
      on e: Exception do
        HandleException(e, Self);
    end;
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

function TFileSystemWatcherImpl.GetWatchPath(var aWatchPath: String): Boolean;
begin
  Result := True;
{$IFDEF UNIX}
  if aWatchPath <> PathDelim then
{$ENDIF}
    aWatchPath := ExcludeTrailingPathDelimiter(aWatchPath);

{$IFDEF MSWINDOWS}
  // Special check for network path
  if (Pos(PathDelim, aWatchPath) = 1) and (NumCountChars(PathDelim, aWatchPath) < 3) then
    Exit(False);
  // Special check for drive root
  if (Length(aWatchPath) = 2) and (aWatchPath[2] = ':') then
    aWatchPath := aWatchPath + PathDelim;
{$ENDIF}
end;

{$IF DEFINED(MSWINDOWS)}
function TFileSystemWatcherImpl.IsPathObserved(Watch: TOSWatch; FileName: String): Boolean;
var
  j: Integer;
  Path: String;
begin
  Path := UTF8UpperCase(Watch.WatchPath + FileName);

  FWatcherLock.Acquire;
  try
    for j := 0 to Watch.Observers.Count - 1 do
    begin
      if IsInPath(Watch.Observers[j].TargetWatchPath, Path, False, False) then
        Exit(True);
    end;
  finally
    FWatcherLock.Release;
  end; { try - finally }

  Result := False;
end;
{$ENDIF}

constructor TFileSystemWatcherImpl.Create;
begin
{$IF (fpc_version<2) or ((fpc_version=2) and (fpc_release<5))}
  // Workaround for race condition, see FPC Mantis #16884.
  inherited Create(True);
{$ELSE}
  inherited Create(False);
{$ENDIF}

  FOSWatchers := TOSWatchs.Create({$IFDEF MSWINDOWS}False{$ELSE}True{$ENDIF});
  FWatcherLock := syncobjs.TCriticalSection.Create;

  FreeOnTerminate := False;
  FFinished := False;

  {$IF DEFINED(MSWINDOWS)}
  case gWatcherMode of
    fswmPreventDelete:
      VAR_READDIRECTORYCHANGESW_BUFFERSIZE := READDIRECTORYCHANGESW_BUFFERSIZE;
    fswmAllowDelete:
      begin
        VAR_READDIRECTORYCHANGESW_BUFFERSIZE := READDIRECTORYCHANGESW_BUFFERSIZE;
        CREATEFILEW_SHAREMODE := CREATEFILEW_SHAREMODE or FILE_SHARE_DELETE;
      end;
    fswmWholeDrive:
      begin
        VAR_READDIRECTORYCHANGESW_BUFFERSIZE := READDIRECTORYCHANGESW_DRIVE_BUFFERSIZE;
        CREATEFILEW_SHAREMODE := CREATEFILEW_SHAREMODE or FILE_SHARE_DELETE;
      end;
  end;
  {$ELSEIF DEFINED(LINUX)}
  // create inotify instance
  FNotifyHandle := fpinotify_init();
  if FNotifyHandle < 0 then
    ShowError('inotify_init() failed');

  // create pipe for user triggered fake event
  FEventPipe[0] := -1;
  FEventPipe[1] := -1;
  if FpPipe(FEventPipe) = 0 then
  begin
    // set both ends of pipe non blocking
    FileCloseOnExec(FEventPipe[0]); FileCloseOnExec(FEventPipe[1]);
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
  {$IF DEFINED(LINUX)}
  // close both ends of pipe
  if FEventPipe[0] <> -1 then
  begin
    FileClose(FEventPipe[0]);
    FEventPipe[0] := -1;
  end;
  if FEventPipe[1] <> -1 then
  begin
    FileClose(FEventPipe[1]);
    FEventPipe[1] := -1;
  end;
  if FNotifyHandle <> feInvalidHandle then
  begin
    FileClose(FNotifyHandle);
    FNotifyHandle := feInvalidHandle;
  end;
  {$ELSEIF DEFINED(BSD)}
  if FNotifyHandle <> feInvalidHandle then
  begin
    FileClose(FNotifyHandle);
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
{$IF DEFINED(MSWINDOWS)}
  // Remove leftover watchers before queueing TerminateProc.
  // Their handles will be destroyed which will cause completion routines
  // to be called before Terminate is set and SleepEx loop breaks.
  while FOSWatchers.Count > 0 do
    RemoveOSWatch(FOSWatchers[0]);
  // Then queue TerminateProc in TriggerTerminateEvent.
{$ENDIF}
  inherited Terminate;
  TriggerTerminateEvent;
end;

function TFileSystemWatcherImpl.AddWatch(aWatchPath: String;
                                         aWatchFilter: TFSWatchFilter;
                                         aWatcherEvent: TFSWatcherEvent;
                                         UserData: Pointer): Boolean;
var
  OSWatcher: TOSWatch = nil;
  OSWatcherCreated: Boolean = False;
  Observer: TOSWatchObserver;
  i, j: Integer;
  WatcherIndex: Integer = -1;
  {$IFDEF MSWINDOWS}
  RegisteredPath: String;
  {$ENDIF}
begin
  if (aWatchPath = '') or (aWatcherEvent = nil) then
    Exit(False);

  if not GetWatchPath(aWatchPath) then Exit(False);

  {$IFDEF MSWINDOWS}
  if gWatcherMode = fswmWholeDrive then
  begin
    RegisteredPath := aWatchPath;
    aWatchPath := GetDriveOfPath(aWatchPath);
  end;
  {$ENDIF}

  // Check if the path is not already watched.
  FWatcherLock.Acquire;
  try
    for i := 0 to FOSWatchers.Count - 1 do
      if FOSWatchers[i].WatchPath = aWatchPath then
      begin
        OSWatcher := FOSWatchers[i];
        WatcherIndex := i;

        // Check if the observer is not already registered.
        for j := 0 to OSWatcher.Observers.Count - 1 do
        begin
          if SameMethod(TMethod(OSWatcher.Observers[j].WatcherEvent), TMethod(aWatcherEvent)) then
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
    {$IF DEFINED(MSWINDOWS)}
    OSWatcher.Reference{$IFDEF DEBUG_WATCHER}('AddWatch'){$ENDIF}; // For usage by FileSystemWatcher (main thread)
    {$ENDIF}
    OSWatcherCreated := True;
  end;

  Observer := TOSWatchObserver.Create;
  Observer.WatchFilter := aWatchFilter;
  Observer.WatcherEvent := aWatcherEvent;
  Observer.UserData := UserData;
  {$IFDEF MSWINDOWS}
  if gWatcherMode = fswmWholeDrive then
  begin
    Observer.RegisteredWatchPath := RegisteredPath;
    Observer.TargetWatchPath := UTF8UpperCase(GetTargetPath(RegisteredPath));
  end;
  {$ENDIF}

  FWatcherLock.Acquire;
  try
    if OSWatcherCreated then
      WatcherIndex := FOSWatchers.Add(OSWatcher);

    OSWatcher.Observers.Add(Observer);
    OSWatcher.UpdateFilter; // This creates or recreates handle.

    Result := OSWatcher.Handle <> feInvalidHandle;

    // Remove watcher if could not create notification handle.
    if not Result then
      RemoveOSWatchLocked(WatcherIndex);

  finally
    FWatcherLock.Release;
  end;
end;

procedure TFileSystemWatcherImpl.RemoveWatch(aWatchPath: String;
                                             aWatcherEvent: TFSWatcherEvent);
var
  i: Integer;
begin
  if not GetWatchPath(aWatchPath) then Exit;

  {$IFDEF MSWINDOWS}
  if gWatcherMode = fswmWholeDrive then
    aWatchPath := GetDriveOfPath(aWatchPath);
  {$ENDIF}

  FWatcherLock.Acquire;
  try
    for i := 0 to FOSWatchers.Count - 1 do
    begin
      if FOSWatchers[i].WatchPath = aWatchPath then
      begin
        RemoveObserverLocked(i, aWatcherEvent);
        Break;
      end;
    end;
  finally
    FWatcherLock.Release;
  end;
end;

procedure TFileSystemWatcherImpl.RemoveWatch(aWatcherEvent: TFSWatcherEvent);
var
  i: Integer;
begin
  FWatcherLock.Acquire;
  try
    for i := 0 to FOSWatchers.Count - 1 do
    begin
      RemoveObserverLocked(i, aWatcherEvent);
    end;
  finally
    FWatcherLock.Release;
  end;
end;

procedure TFileSystemWatcherImpl.RemoveObserverLocked(OSWatcherIndex: Integer; aWatcherEvent: TFSWatcherEvent);
var
  j: Integer;
begin
  for j := 0 to FOSWatchers[OSWatcherIndex].Observers.Count - 1 do
  begin
    if SameMethod(TMethod(FOSWatchers[OSWatcherIndex].Observers[j].WatcherEvent), TMethod(aWatcherEvent)) then
    begin
      FOSWatchers[OSWatcherIndex].Observers.Delete(j);

      if FOSWatchers[OSWatcherIndex].Observers.Count = 0 then
        RemoveOSWatchLocked(OSWatcherIndex)
      else
        FOSWatchers[OSWatcherIndex].UpdateFilter;

      Break;
    end;
  end;
end;

procedure TFileSystemWatcherImpl.RemoveOSWatchLocked(Index: Integer);
begin
  {$IF DEFINED(MSWINDOWS)}
  with FOSWatchers[Index] do
  begin
    DestroyHandle;
    Dereference{$IFDEF DEBUG_WATCHER}('RemoveOSWatchLocked'){$ENDIF}; // Not using anymore by FileSystemWatcher from main thread
  end;
  {$ENDIF}
  FOSWatchers.Delete(Index);
end;

procedure TFileSystemWatcherImpl.RemoveOSWatch(Watch: TOSWatch);
var
  i: Integer;
begin
  FWatcherLock.Acquire;
  try
    for i := 0 to FOSWatchers.Count - 1 do
    begin
      if FOSWatchers[i] = Watch then
      begin
        RemoveOSWatchLocked(i);
        Break;
      end;
    end;
  finally
    FWatcherLock.Release;
  end;
end;

procedure TFileSystemWatcherImpl.TriggerTerminateEvent;
{$IF DEFINED(MSWINDOWS)}
begin
  QueueUserAPC(@TerminateProc, Self.Handle, ULONG_PTR(Self));
end;
{$ELSEIF DEFINED(LINUX)}
var
  buf: Char;
begin
  // check if thread has been started
  if Self.FNotifyHandle <> feInvalidHandle then
  begin
    buf := #0;
    FileWrite(FEventPipe[1], buf, 1);
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

constructor TOSWatch.Create(const aWatchPath: String
                            {$IFDEF UNIX}; aNotifyHandle: THandle{$ENDIF});
begin
  FObservers := TOSWatchObservers.Create(True);
  FWatchFilter := [];
  FWatchPath := aWatchPath;
  {$IFDEF UNIX}
  FNotifyHandle := aNotifyHandle;
  {$ENDIF}
  {$IF DEFINED(MSWINDOWS)}
  FReferenceCount := 0;
  FBuffer := GetMem(VAR_READDIRECTORYCHANGESW_BUFFERSIZE);
  {$ENDIF}
  FHandle := feInvalidHandle;
end;

destructor TOSWatch.Destroy;
begin
  DestroyHandle;
  inherited;
  {$IFDEF DEBUG_WATCHER}
  DCDebug(['FSWatcher: Destroying watch ', hexStr(Self)]);
  {$ENDIF}
  FObservers.Free;
  {$IF DEFINED(MSWINDOWS)}
  Freemem(FBuffer);
  {$ENDIF}
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
    {$IF DEFINED(MSWINDOWS)}
    SetFilter(FWatchFilter);
    if FHandle = feInvalidHandle then
      CreateHandle
    else
      QueueCancelRead; // Will cancel and restart Read
    {$ELSE}
    DestroyHandle;
    CreateHandle;
    {$ENDIF}
  end;
end;

{$IF DEFINED(MSWINDOWS)}
procedure TOSWatch.Reference{$IFDEF DEBUG_WATCHER}(s: String){$ENDIF};
{$IFDEF DEBUG_WATCHER}
var
  CurrentRefCount: LongInt;
{$ENDIF}
begin
  {$IFDEF DEBUG_WATCHER}
  CurrentRefCount :=
  {$ENDIF}
  System.InterlockedIncrement(FReferenceCount);
  {$IFDEF DEBUG_WATCHER}
  DCDebug(['FSWatcher: Watch ', hexStr(Self), ' ++ref=', CurrentRefCount, ' ', s]);
  {$ENDIF}
end;

procedure TOSWatch.Dereference{$IFDEF DEBUG_WATCHER}(s: String){$ENDIF};
{$IFDEF DEBUG_WATCHER}
var
  CurrentRefCount: LongInt;
{$ENDIF}
begin
  {$IFDEF DEBUG_WATCHER}
  CurrentRefCount := System.InterlockedDecrement(FReferenceCount);
  DCDebug(['FSWatcher: Watch ', hexStr(Self), ' --ref=', CurrentRefCount, ' ', s]);
  if CurrentRefCount = 0 then
  {$ELSE}
  if System.InterlockedDecrement(FReferenceCount) = 0 then
  {$ENDIF}
    Free;
end;
{$ENDIF}

procedure TOSWatch.CreateHandle;
{$IF DEFINED(MSWINDOWS)}
begin
  FHandle := CreateFileW(PWideChar(UTF16LongName(FWatchPath)),
               FILE_LIST_DIRECTORY,
               CREATEFILEW_SHAREMODE,
               nil,
               OPEN_EXISTING,
               FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,
               0);

  if FHandle = INVALID_HANDLE_VALUE then
  begin
    FHandle := CreateFileW(PWideChar(CeUtf8ToUtf16(FWatchPath)),
                 FILE_LIST_DIRECTORY,
                 CREATEFILEW_SHAREMODE,
                 nil,
                 OPEN_EXISTING,
                 FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,
                 0);
  end;

  if FHandle = INVALID_HANDLE_VALUE then
  begin
    FHandle := feInvalidHandle;
    ShowError('CreateFileW failed for ' + FWatchPath);
  end
  else
  begin
    FillChar(FOverlapped, SizeOf(FOverlapped), 0);
    // Pass pointer to watcher to the notify routine
    FOverlapped.OSWatch := Self;
    QueueRead;
  end;
end;
{$ELSEIF DEFINED(LINUX)}
var
  hNotifyFilter: cuint32 = IN_DELETE_SELF or IN_MOVE_SELF;
begin
  if wfFileNameChange in FWatchFilter then
    hNotifyFilter := hNotifyFilter or IN_CREATE or IN_DELETE or IN_MOVE;
  if wfAttributesChange in FWatchFilter then
    hNotifyFilter := hNotifyFilter or IN_ATTRIB or IN_MODIFY;

  FHandle := fpinotify_add_watch(FNotifyHandle, FWatchPath, hNotifyFilter);
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

  FHandle := mbFileOpen(FWatchPath, fmOpenRead);
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
{$IF DEFINED(MSWINDOWS)}
var
  tmpHandle: THandle;
{$ENDIF}
begin
  if FHandle <> feInvalidHandle then
  begin
    {$IF DEFINED(LINUX)}
    fpinotify_rm_watch(FNotifyHandle, FHandle);
    {$ENDIF}
    {$IF DEFINED(BSD)}
    FileClose(FHandle);
    {$ENDIF}
    {$IF DEFINED(MSWINDOWS)}
    // If there are outstanding I/O operations on the handle calling CloseHandle
    // will fail those operations and cause completion routines to be called
    // but with ErrorCode = 0. Clearing FHandle before the call allows to know
    // that handle has been destroyed and to not schedule new Reads.
    {$IFDEF DEBUG_WATCHER}
    DCDebug(['FSWatcher: Watch ', hexStr(Self),' DestroyHandle ', Integer(FHandle), ' done']);
    {$ENDIF}
    tmpHandle := FHandle;
    FHandle := feInvalidHandle;
    CloseHandle(tmpHandle);
    {$ELSE}
    FHandle := feInvalidHandle;
    {$ENDIF}
  end;
end;

{$IF DEFINED(MSWINDOWS)}
procedure TOSWatch.QueueCancelRead;
begin
  {$IFDEF DEBUG_WATCHER}
  DCDebug('FSWatcher: QueueCancelRead: Queueing Cancel APC');
  {$ENDIF}
  Reference{$IFDEF DEBUG_WATCHER}('QueueCancelRead'){$ENDIF}; // For use by CancelReadChangesProc.
  QueueUserAPC(@CancelReadChangesProc, FileSystemWatcher.Handle, ULONG_PTR(Self));
  {$IFDEF DEBUG_WATCHER}
  DCDebug('FSWatcher: QueueCancelRead: Queueing Cancel APC done');
  {$ENDIF}
end;

procedure TOSWatch.QueueRead;
begin
  {$IFDEF DEBUG_WATCHER}
  DCDebug('FSWatcher: QueueRead: Queueing Read APC');
  {$ENDIF}
  Reference{$IFDEF DEBUG_WATCHER}('QueueRead'){$ENDIF}; // For use by ReadChangesProc.
  QueueUserAPC(@ReadChangesProc, FileSystemWatcher.Handle, ULONG_PTR(Self));
  {$IFDEF DEBUG_WATCHER}
  DCDebug('FSWatcher: QueueRead: Queueing Read APC done');
  {$ENDIF}
end;

procedure TOSWatch.SetFilter(aWatchFilter: TFSWatchFilter);
var
  // Use temp variable so that assigning FNotifyFilter is coherent.
  dwFilter: DWORD = 0;
begin
  if wfFileNameChange in aWatchFilter then
    dwFilter := dwFilter or FILE_NOTIFY_CHANGE_FILE_NAME
                         or FILE_NOTIFY_CHANGE_DIR_NAME;
  if wfAttributesChange in aWatchFilter then
    dwFilter := dwFilter or FILE_NOTIFY_CHANGE_ATTRIBUTES or
                            FILE_NOTIFY_CHANGE_SIZE or
                            FILE_NOTIFY_CHANGE_LAST_WRITE;
  FNotifyFilter := dwFilter;
end;

{$ENDIF}

finalization
  TFileSystemWatcher.DestroyFileSystemWatcher;

end.

