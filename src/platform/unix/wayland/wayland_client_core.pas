{*
 * Copyright © 2008 Kristian Høgsberg
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice (including the
 * next paragraph) shall be included in all copies or substantial
 * portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *}

unit wayland_client_core;

{$mode objfpc}{$H+}
{$packrecords c}
{$linklib wayland-client}


interface

uses
  Classes, SysUtils, unixtype, wayland_util, ctypes;

type
  Pwl_display = pointer; //^Twl_display;
  Pwl_event_queue = pointer ;//^Twl_event_queue;
  Pwl_proxy = pointer; //^Twl_proxy;
  Pwl_proxy_wrapper = pointer;//^Twl_proxy_wrapper;

  { TWLProxyObject }

  TWLProxyObject = class;

  PWLUserData = ^TWLUserData;
  TWLUserData = record
    ListenerUserData: Pointer; //wl_xxx_set_userdata and add listener share this.
    PascalObject: TWLProxyObject; { private }
    UserData: Pointer;
  end;

  TWLProxyObjectClass = class of TWLProxyObject;
  TWLProxyObject = class
  private
    FOwnsProxy: Boolean;
    function GetUserData: Pointer;
    procedure SetUserData(AValue: Pointer);
  protected
    FProxy: Pwl_proxy;
    FUserDataRec: TWLUserData;
  public
    class function WLToObj(AProxy: Pwl_proxy): TWLProxyObject;
  public
    constructor Create(AProxy: Pwl_proxy; AOwnsProxy: Boolean = True); virtual;
    destructor  Destroy; override;
    function GetVersion: LongInt;
    property Proxy: Pwl_proxy read FProxy;
    property UserData: Pointer read GetUserData write SetUserData;
    property OwnsProxy: Boolean read FOwnsProxy write FOwnsProxy;
  end;

  { TWLDisplayBase }

  TWLDisplayBase = class(TWLProxyObject)
    class function  Connect(AName: String; AClass: TWLProxyObjectClass=nil{TWLDisplay}): TWLDisplayBase;
    class function  ConnectToFd(AFd: LongInt; AClass: TWLProxyObjectClass=nil{TWLDisplay}): TWLDisplayBase;
    procedure Disconnect;
    function  GetFd:LongInt;
    function  Dispatch: LongInt;
    function  DispatchQueue(AQueue: Pwl_event_queue): LongInt;
    function  DispatchQueuePending(AQueue: Pwl_event_queue): LongInt;
    function  DispatchPending: LongInt;
    function  GetError: LongInt;
    function  GetProtocolError(AInterface: PPwl_interface; AId: PLongWord): LongWord;
    function  Flush: LongInt;
    function  RoundtripQueue(AQueue: Pwl_event_queue): LongInt;
    function  Roundtrip: LongInt;
    function  CreateQueue: Pwl_event_queue;
    function  PrepareReadQueue(AQueue: Pwl_event_queue): LongInt;
    function  PrepareRead: LongInt;
    procedure CancelRead;
    function  ReadEvents:LongInt;
    destructor Destroy; override;
  end;

  { Twl_event_queue }




procedure wl_event_queue_destroy(queue: Pwl_event_queue); cdecl; external;

procedure wl_proxy_marshal(p: Pwl_proxy; opcode: cint32); cdecl; external; varargs;
procedure wl_proxy_marshal_array(p: Pwl_proxy; opcode: cint32; args: Pwl_argument); cdecl; external;
function  wl_proxy_create(factory: Pwl_proxy; &interface: Pwl_interface): Pwl_proxy; cdecl; external;
function  wl_proxy_create_wrapper(proxy: pointer): Pwl_proxy_wrapper; cdecl; external;
procedure wl_proxy_wrapper_destroy(proxy_wrapper: Pointer); cdecl; external;
function  wl_proxy_marshal_constructor(p: Pwl_proxy; opcode: cint32; &interface: Pwl_interface): Pwl_proxy; cdecl; external; varargs;
function  wl_proxy_marshal_constructor_versioned(p: Pwl_proxy; opcode: cint32; &interface: Pwl_interface; version: cuint32): Pwl_proxy; cdecl; external; varargs;
function  wl_proxy_marshal_array_constructor(p: Pwl_proxy; opcode: cint32; args: Pwl_argument; &interface: Pwl_interface): Pwl_proxy; cdecl; external;
function  wl_proxy_marshal_array_constructor_versioned(p: Pwl_proxy; opcode: cint32; args: Pwl_argument; &interface: Pwl_interface; version: cuint32): Pwl_proxy; cdecl; external;
procedure wl_proxy_destroy(proxy: Pwl_proxy); cdecl; external;
function  wl_proxy_add_listener(proxy: Pwl_proxy; impl: pointer; data: pointer): cint; cdecl; external;
function  wl_proxy_get_listener(proxy: Pwl_proxy): Pointer; cdecl; external;
function  wl_proxy_add_dispatcher(proxy: Pwl_proxy; dispatcher_func: wl_dispatcher_func_t; dispatcher_data: pointer; data: pointer): cint; cdecl; external;
procedure wl_proxy_set_user_data(proxy: Pwl_proxy; user_data: pointer); cdecl; external;
function  wl_proxy_get_user_data(proxy: Pwl_proxy): Pointer; cdecl; external;
function  wl_proxy_get_version(proxy: Pwl_proxy): cuint32; cdecl; external;
function  wl_proxy_get_id(proxy: Pwl_proxy): cint; cdecl; external;
function  wl_proxy_get_class(proxy: Pwl_proxy): PChar; cdecl; external;
procedure wl_proxy_set_queue(proxy: Pwl_proxy; queue: Pwl_event_queue); cdecl; external;

function  wl_display_connect(name: PChar): Pwl_display; cdecl; external;
function  wl_display_connect_to_fd(fd: cint): Pwl_display; cdecl; external;
procedure wl_display_disconnect(display :Pwl_display); cdecl; external;
function  wl_display_get_fd(display :Pwl_display): cint; cdecl; external;
function  wl_display_dispatch(display :Pwl_display): cint; cdecl; external;
function  wl_display_dispatch_queue(display :Pwl_display; queue: Pwl_event_queue): cint; cdecl; external;
function  wl_display_dispatch_queue_pending(display :Pwl_display; queue: Pwl_event_queue): cint; cdecl; external;
function  wl_display_dispatch_pending(display :Pwl_display): cint; cdecl; external;
function  wl_display_get_error(display :Pwl_display): cint; cdecl; external;
function  wl_display_get_protocol_error(display :Pwl_display; &interface: PPwl_interface; id: Pcuint32): cuint32; cdecl; external;
function  wl_display_flush(display :Pwl_display): cint; cdecl; external;
function  wl_display_roundtrip_queue(display :Pwl_display; queue: Pwl_event_queue): cint; cdecl; external;
function  wl_display_roundtrip(display :Pwl_display): cint; cdecl; external;
function  wl_display_create_queue(display :Pwl_display): Pwl_event_queue; cdecl; external;
function  wl_display_prepare_read_queue(display :Pwl_display; queue: Pwl_event_queue): cint; cdecl; external;
function  wl_display_prepare_read(display :Pwl_display): cint; cdecl; external;
procedure wl_display_cancel_read(display :Pwl_display); cdecl; external;
function  wl_display_read_events(display :Pwl_display): cint; cdecl; external;

procedure wl_log_set_handler_client(handler: wl_log_func_t); cdecl; external;

implementation
uses
  wayland_protocol;

{ TWLDisplayBase }

class function TWLDisplayBase.Connect(AName: String; AClass: TWLProxyObjectClass
  ): TWLDisplayBase;
var
  lDisplay: wayland_client_core.Pwl_display;
  lName: PChar;
begin
  Result := nil;

  if AClass = nil then
    AClass:=TWlDisplay;

  if AName = '' then
    lName:=nil
  else
    lName := PChar(AName);

  lDisplay := wl_display_connect(lName);
  if lDisplay <> nil then
  begin
    Result := TWLDisplayBase(AClass.Create(lDisplay));
  end;
end;

class function TWLDisplayBase.ConnectToFd(AFd: LongInt; AClass: TWLProxyObjectClass): TWLDisplayBase;
var
  lDisplay: wayland_client_core.Pwl_display;
begin
  if AClass = nil then
    AClass:=TWlDisplay;
  Result := nil;
  lDisplay := wl_display_connect_to_fd(AFd);
  if lDisplay <> nil then
  begin
    Result := TWLDisplayBase(AClass.Create(lDisplay));
  end;
end;

procedure TWLDisplayBase.Disconnect;
begin
  if Assigned(FProxy) then
    wl_display_disconnect(FProxy);
  FProxy:=nil;
end;

function TWLDisplayBase.GetFd: LongInt;
begin
  Result := wl_display_get_fd(FProxy);
end;

function TWLDisplayBase.Dispatch: LongInt;
begin
  Result := wl_display_dispatch(FProxy);
end;

function TWLDisplayBase.DispatchQueue(AQueue: Pwl_event_queue): LongInt;
begin
  Result := wl_display_dispatch_queue(FProxy, AQueue);
end;

function TWLDisplayBase.DispatchQueuePending(AQueue: Pwl_event_queue): LongInt;
begin
  Result := wl_display_dispatch_queue_pending(FProxy, AQueue);
end;

function TWLDisplayBase.DispatchPending: LongInt;
begin
  Result := wl_display_dispatch_pending(FProxy);
end;

function TWLDisplayBase.GetError: LongInt;
begin
  Result := wl_display_get_error(FProxy);
end;

function TWLDisplayBase.GetProtocolError(AInterface: PPwl_interface;
  AId: PLongWord): LongWord;
begin
  Result := wl_display_get_protocol_error(FProxy, AInterface, AId);

end;

function TWLDisplayBase.Flush: LongInt;
begin
  Result := wl_display_flush(FProxy);

end;

function TWLDisplayBase.RoundtripQueue(AQueue: Pwl_event_queue): LongInt;
begin
  Result := wl_display_roundtrip_queue(FProxy, AQueue);
end;

function TWLDisplayBase.Roundtrip: LongInt;
begin
  Result := wl_display_roundtrip(FProxy);
end;

function TWLDisplayBase.CreateQueue: Pwl_event_queue;
begin
  Result := wl_display_create_queue(FProxy);
end;

function TWLDisplayBase.PrepareReadQueue(AQueue: Pwl_event_queue): LongInt;
begin
  Result := wl_display_prepare_read_queue(FProxy, AQueue);
end;

function TWLDisplayBase.PrepareRead: LongInt;
begin
  Result := wl_display_prepare_read(FProxy);
end;

procedure TWLDisplayBase.CancelRead;
begin
  wl_display_cancel_read(FProxy);

end;

function TWLDisplayBase.ReadEvents: LongInt;
begin
  Result := wl_display_read_events(FProxy);
end;

destructor TWLDisplayBase.Destroy;
begin
  Disconnect;
  //inherited Destroy;
end;



{ TWLProxyObject }

function TWLProxyObject.GetUserData: Pointer;
begin
  Result := FUserDataRec.UserData;
end;

procedure TWLProxyObject.SetUserData(AValue: Pointer);
begin
  if FUserDataRec.UserData=AValue then Exit;
  FUserDataRec.UserData:=AValue;
end;

class function TWLProxyObject.WLToObj(AProxy: Pwl_proxy): TWLProxyObject;
var
  lData: PWLUserData;
begin
  lData := wl_proxy_get_user_data(AProxy);
  Result := lData^.PascalObject;
end;

constructor TWLProxyObject.Create(AProxy: Pwl_proxy; AOwnsProxy: Boolean);
begin
  FUserDataRec.PascalObject := Self;
  FProxy:=AProxy;
  wl_proxy_set_user_data(FProxy, @FUserDataRec);
end;

destructor TWLProxyObject.Destroy;
begin
  inherited Destroy;
  if Assigned(FProxy) and FOwnsProxy then
    wl_proxy_destroy(FProxy);
end;

function TWLProxyObject.GetVersion: LongInt;
begin
  Result := wl_proxy_get_version(FProxy);
end;

end.

