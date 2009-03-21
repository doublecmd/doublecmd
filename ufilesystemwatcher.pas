{
    Double Commander
    -------------------------------------------------------------------------
    This is a thread-component sends an event when a change in the file system occurs.

    Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

    based on:
      TDirWatcher by Bernhard Angerer (angerer@mail.ift.tuwien.ac.at)

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
  TWatchFilter = set of (wfFileNameChange, wfAttributesChange);

  TOnWatcherNotifyEvent = procedure(NotifyEvent: TWatchFilter) of object;

  { TWatcherThread }

  TWatcherThread = class(TThread)
  private
    FOnWatcherNotifyEvent: TOnWatcherNotifyEvent;
    FFileHandle,
    FNotifyHandle: THandle;
    FWatchPath: UTF8String;
    FWatchFilter,
    FNotifyEvent: TWatchFilter;
  protected
    procedure Execute; override;
    procedure WatcherNotifyEvent;
  public
    constructor Create(sPath: UTF8String; aWatchFilter: TWatchFilter);
    destructor Destroy; override;
    property OnWatcherNotifyEvent: TOnWatcherNotifyEvent write FOnWatcherNotifyEvent;
  end;

  { TFileSystemWatcher }

  TFileSystemWatcher = class
  private
    FActive: Boolean;
    FWatchPath: UTF8String;
    FWatchFilter: TWatchFilter;
    FWatcherThread: TWatcherThread;
    FOnWatcherNotifyEvent: TOnWatcherNotifyEvent;
    procedure SetActive(const AValue: Boolean);
    procedure SetWatchFilter(const AValue: TWatchFilter);
    procedure SetWatchPath(const AValue: UTF8String);
  public
    constructor Create(sPath: UTF8String; aWatchFilter: TWatchFilter);
    destructor Destroy; override;
    property Active: Boolean read FActive write SetActive;
    property WatchPath: UTF8String read FWatchPath write SetWatchPath;
    property WatchFilter: TWatchFilter read FWatchFilter write SetWatchFilter;
    property OnWatcherNotifyEvent: TOnWatcherNotifyEvent write FOnWatcherNotifyEvent;
  end;

implementation
uses
  {$IF DEFINED(MSWINDOWS)}
  Windows
  {$ELSEIF DEFINED(LINUX)}
  inotify, Unix, BaseUnix, UnixUtil, UnixType
  {$ENDIF};

{ TWatcherThread }

procedure TWatcherThread.Execute;
{$IF DEFINED(MSWINDOWS)}
var
  hNotifyFilter: LongWord;
  wsPath: WideString;
begin
  hNotifyFilter:= 0;
  if wfFileNameChange in FWatchFilter then
    hNotifyFilter:= hNotifyFilter or FILE_NOTIFY_CHANGE_FILE_NAME;
  if wfAttributesChange in FWatchFilter then
    hNotifyFilter:= hNotifyFilter or FILE_NOTIFY_CHANGE_ATTRIBUTES;

  wsPath:= UTF8Decode(FWatchPath);
  FNotifyHandle:= FindFirstChangeNotificationW(PWideChar(wsPath), False, hNotifyFilter);

  if FNotifyHandle <> INVALID_HANDLE_VALUE then
    repeat
      if WaitForSingleObject(FNotifyHandle, INFINITE) = WAIT_OBJECT_0 then
      begin
        Synchronize(@WatcherNotifyEvent);
      end;
      if not FindNextChangeNotification(FNotifyHandle) then
        begin
          RaiseLastOSError;
          Break;
        end;
    until Terminated;
end;
{$ELSEIF DEFINED(LINUX)}
var
  bytes_to_parse, p: Integer;
  buf: PChar;
  ev: pinotify_event;
  hNotifyFilter: uint32_t;
begin
  hNotifyFilter:= 0;
  if wfFileNameChange in FWatchFilter then
    hNotifyFilter:= hNotifyFilter or IN_CREATE or IN_DELETE or IN_DELETE_SELF or IN_MOVE or IN_MOVE_SELF;
  if wfAttributesChange in FWatchFilter then
    hNotifyFilter:= hNotifyFilter or IN_ATTRIB;

 WriteLn('Start watching');
 // create inotify instance
 FFileHandle:= inotify_init();
 if (FFileHandle < 0) then
 begin
  WriteLn('inotify_init(): ');
  Exit;
 end;
// WriteLn('After inotify_init()');

 // add watch
 FNotifyHandle:= inotify_add_watch(FFileHandle, PChar(FWatchPath), hNotifyFilter);
 if (FNotifyHandle < 0) then
 begin
  WriteLn('inotify_add_watch(): ');
  Exit;
 end;
// WriteLn('After inotify_add_watch()');

 // process events
 while True do
 begin
  // wait events
  repeat

   if (fpioctl(FFileHandle, $541B, @bytes_to_parse) = -1) then
   begin
    WriteLn('ioctl(): ');
    Exit;
   end;
  until (bytes_to_parse >= SizeOf(inotify_event));
//  WriteLn('After fpioctl()');

  // get memory for events and read them
  buf := GetMem(bytes_to_parse);
  if (fpread(FFileHandle, buf, bytes_to_parse) = -1) then
  begin
   WriteLn('read(): ');
   Exit;
  end;
//  WriteLn('After fpread()');

  // parse events and print them
  p:= 0;
  while (p < bytes_to_parse) do
  begin
   ev:= pinotify_event((buf + p));
   WriteLn('wd = ',ev^.wd,', mask = ',ev^.mask,', cookie = ',ev^.cookie, 'name = ', PChar(@ev^.name));
   // call event handler
   Synchronize(@WatcherNotifyEvent);
   p:= p + ev^.len + 16;
  end;
//  WriteLn('After print()');

  // free memory
  FreeMem(buf);
 end;
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TWatcherThread.WatcherNotifyEvent;
begin
  if Assigned(FOnWatcherNotifyEvent) then
    FOnWatcherNotifyEvent(FNotifyEvent);
end;

destructor TWatcherThread.Destroy;
{$IF DEFINED(MSWINDOWS)}
begin
  if FNotifyHandle <> NULL then
    FindCloseChangeNotification(FNotifyHandle);
  inherited Destroy;
end;
{$ELSEIF DEFINED(LINUX)}
begin
  // remove watch
  inotify_rm_watch(FFileHandle, FNotifyHandle);
  // close inotify instance
  fpClose(FFileHandle);
  inherited Destroy;
end;
{$ELSE}
begin
  inherited Destroy;
end;
{$ENDIF}

constructor TWatcherThread.Create(sPath: UTF8String; aWatchFilter: TWatchFilter);
begin
  inherited Create(True); // create suspended

  FWatchPath:= sPath;
  FWatchFilter:= aWatchFilter;
  FreeOnTerminate:= True;
end;

{ TFileSystemWatcher }

procedure TFileSystemWatcher.SetWatchPath(const AValue: UTF8String);
begin
  if Assigned(FWatcherThread) then
    FWatcherThread.Terminate;
  FWatchPath:= AValue;
  FWatcherThread:= TWatcherThread.Create(FWatchPath, FWatchFilter);
  FWatcherThread.OnWatcherNotifyEvent:= FOnWatcherNotifyEvent;
  FWatcherThread.Resume;
end;

procedure TFileSystemWatcher.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then Exit;
  if AValue then
    begin
      FWatcherThread:= TWatcherThread.Create(FWatchPath, FWatchFilter);
      FWatcherThread.OnWatcherNotifyEvent:= FOnWatcherNotifyEvent;
      FWatcherThread.Resume;
      FActive:= AValue;
    end
  else
    begin
      if Assigned(FWatcherThread) then
        FWatcherThread.Terminate;
      FActive:= AValue;
    end;
end;

procedure TFileSystemWatcher.SetWatchFilter(const AValue: TWatchFilter);
begin
  if FWatchFilter = AValue then Exit;
  FWatchFilter:= AValue;
  SetWatchPath(FWatchPath);
end;

constructor TFileSystemWatcher.Create(sPath: UTF8String; aWatchFilter: TWatchFilter);
begin
  FActive:= False;
  FWatchPath:= sPath;
  FWatchFilter:= aWatchFilter;
end;

destructor TFileSystemWatcher.Destroy;
begin
  if Assigned(FWatcherThread) then
    FWatcherThread.Terminate;
  inherited Destroy;
end;

end.

