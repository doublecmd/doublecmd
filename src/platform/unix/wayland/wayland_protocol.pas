unit wayland_protocol;

{$mode objfpc} {$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, wayland_util, wayland_client_core;


type
  Pwl_display = Pointer;
  Pwl_registry = Pointer;
  Pwl_callback = Pointer;
  Pwl_output = Pointer;
const
  WL_DISPLAY_ERROR_INVALID_OBJECT = 0; // server couldn't find object
  WL_DISPLAY_ERROR_INVALID_METHOD = 1; // method doesn't exist on the specified interface or malformed request
  WL_DISPLAY_ERROR_NO_MEMORY = 2; // server is out of memory
  WL_DISPLAY_ERROR_IMPLEMENTATION = 3; // implementation error in compositor

type
  Pwl_display_listener = ^Twl_display_listener;
  Twl_display_listener = record
    error : procedure(data: Pointer; AWlDisplay: Pwl_display; AObjectId: Pointer; ACode: DWord; AMessage: Pchar); cdecl;
    delete_id : procedure(data: Pointer; AWlDisplay: Pwl_display; AId: DWord); cdecl;
  end;

  Pwl_registry_listener = ^Twl_registry_listener;
  Twl_registry_listener = record
    global : procedure(data: Pointer; AWlRegistry: Pwl_registry; AName: DWord; AInterface: Pchar; AVersion: DWord); cdecl;
    global_remove : procedure(data: Pointer; AWlRegistry: Pwl_registry; AName: DWord); cdecl;
  end;

  Pwl_callback_listener = ^Twl_callback_listener;
  Twl_callback_listener = record
    done : procedure(data: Pointer; AWlCallback: Pwl_callback; ACallbackData: DWord); cdecl;
  end;

const
  WL_OUTPUT_SUBPIXEL_UNKNOWN = 0; // unknown geometry
  WL_OUTPUT_SUBPIXEL_NONE = 1; // no geometry
  WL_OUTPUT_SUBPIXEL_HORIZONTAL_RGB = 2; // horizontal RGB
  WL_OUTPUT_SUBPIXEL_HORIZONTAL_BGR = 3; // horizontal BGR
  WL_OUTPUT_SUBPIXEL_VERTICAL_RGB = 4; // vertical RGB
  WL_OUTPUT_SUBPIXEL_VERTICAL_BGR = 5; // vertical BGR
  WL_OUTPUT_TRANSFORM_NORMAL = 0; // no transform
  WL_OUTPUT_TRANSFORM_90 = 1; // 90 degrees counter-clockwise
  WL_OUTPUT_TRANSFORM_180 = 2; // 180 degrees counter-clockwise
  WL_OUTPUT_TRANSFORM_270 = 3; // 270 degrees counter-clockwise
  WL_OUTPUT_TRANSFORM_FLIPPED = 4; // 180 degree flip around a vertical axis
  WL_OUTPUT_TRANSFORM_FLIPPED_90 = 5; // flip and rotate 90 degrees counter-clockwise
  WL_OUTPUT_TRANSFORM_FLIPPED_180 = 6; // flip and rotate 180 degrees counter-clockwise
  WL_OUTPUT_TRANSFORM_FLIPPED_270 = 7; // flip and rotate 270 degrees counter-clockwise
  WL_OUTPUT_MODE_CURRENT = $1; // indicates this is the current mode
  WL_OUTPUT_MODE_PREFERRED = $2; // indicates this is the preferred mode

type
  Pwl_output_listener = ^Twl_output_listener;
  Twl_output_listener = record
    geometry : procedure(data: Pointer; AWlOutput: Pwl_output; AX: LongInt; AY: LongInt; APhysicalWidth: LongInt; APhysicalHeight: LongInt; ASubpixel: LongInt; AMake: Pchar; AModel: Pchar; ATransform: LongInt); cdecl;
    mode : procedure(data: Pointer; AWlOutput: Pwl_output; AFlags: DWord; AWidth: LongInt; AHeight: LongInt; ARefresh: LongInt); cdecl;
    done : procedure(data: Pointer; AWlOutput: Pwl_output); cdecl;
    scale : procedure(data: Pointer; AWlOutput: Pwl_output; AFactor: LongInt); cdecl;
    name : procedure(data: Pointer; AWlOutput: Pwl_output; AName: Pchar); cdecl;
    description : procedure(data: Pointer; AWlOutput: Pwl_output; ADescription: Pchar); cdecl;
  end;



  TWlDisplay = class;
  TWlRegistry = class;
  TWlCallback = class;
  TWlOutput = class;


  IWlDisplayListener = interface
  ['IWlDisplayListener']
    procedure wl_display_error(AWlDisplay: TWlDisplay; AObjectId: Pointer; ACode: DWord; AMessage: String);
    procedure wl_display_delete_id(AWlDisplay: TWlDisplay; AId: DWord);
  end;

  IWlRegistryListener = interface
  ['IWlRegistryListener']
    procedure wl_registry_global(AWlRegistry: TWlRegistry; AName: DWord; AInterface: String; AVersion: DWord);
    procedure wl_registry_global_remove(AWlRegistry: TWlRegistry; AName: DWord);
  end;

  IWlCallbackListener = interface
  ['IWlCallbackListener']
    procedure wl_callback_done(AWlCallback: TWlCallback; ACallbackData: DWord);
  end;

  IWlOutputListener = interface
  ['IWlOutputListener']
    procedure wl_output_geometry(AWlOutput: TWlOutput; AX: LongInt; AY: LongInt; APhysicalWidth: LongInt; APhysicalHeight: LongInt; ASubpixel: LongInt; AMake: String; AModel: String; ATransform: LongInt);
    procedure wl_output_mode(AWlOutput: TWlOutput; AFlags: DWord; AWidth: LongInt; AHeight: LongInt; ARefresh: LongInt);
    procedure wl_output_done(AWlOutput: TWlOutput); {since: 2}
    procedure wl_output_scale(AWlOutput: TWlOutput; AFactor: LongInt); {since: 2}
    procedure wl_output_name(AWlOutput: TWlOutput; AName: String); {since: 4}
    procedure wl_output_description(AWlOutput: TWlOutput; ADescription: String); {since: 4}
  end;




  TWlDisplay = class(TWlDisplayBase)
  private
    const _SYNC = 0;
    const _GET_REGISTRY = 1;
  public
    function Sync(AProxyClass: TWLProxyObjectClass = nil {TWlCallback}): TWlCallback;
    function GetRegistry(AProxyClass: TWLProxyObjectClass = nil {TWlRegistry}): TWlRegistry;
    function AddListener(AIntf: IWlDisplayListener): LongInt;
  end;

  TWlRegistry = class(TWLProxyObject)
  private
    const _BIND = 0;
  public
    function Bind(AName: DWord; AInterface: Pwl_interface; AVersion: LongInt): Pwl_proxy;
    function AddListener(AIntf: IWlRegistryListener): LongInt;
  end;

  TWlCallback = class(TWLProxyObject)
    function AddListener(AIntf: IWlCallbackListener): LongInt;
  end;

  TWlOutput = class(TWLProxyObject)
  private
    const _RELEASE = 0; { since version: 3}
  public
    destructor Destroy; override; {since version: 3}
    function AddListener(AIntf: IWlOutputListener): LongInt;
  end;

var
  wl_display_interface: Twl_interface;
  wl_registry_interface: Twl_interface;
  wl_callback_interface: Twl_interface;
  wl_output_interface: Twl_interface;


implementation

var
  vIntf_wl_display_Listener: Twl_display_listener;
  vIntf_wl_registry_Listener: Twl_registry_listener;
  vIntf_wl_callback_Listener: Twl_callback_listener;
  vIntf_wl_output_Listener: Twl_output_listener;



function TWlDisplay.Sync(AProxyClass: TWLProxyObjectClass = nil {TWlCallback}): TWlCallback;
var
  callback: Pwl_proxy;
begin
  callback := wl_proxy_marshal_constructor(FProxy,
      _SYNC, @wl_callback_interface, nil);
  if AProxyClass = nil then
    AProxyClass := TWlCallback;
  if not AProxyClass.InheritsFrom(TWlCallback) then
    Raise Exception.CreateFmt('%s does not inherit from %s', [AProxyClass.ClassName, TWlCallback]);
  Result := TWlCallback(AProxyClass.Create(callback));
end;

function TWlDisplay.GetRegistry(AProxyClass: TWLProxyObjectClass = nil {TWlRegistry}): TWlRegistry;
var
  registry: Pwl_proxy;
begin
  registry := wl_proxy_marshal_constructor(FProxy,
      _GET_REGISTRY, @wl_registry_interface, nil);
  if AProxyClass = nil then
    AProxyClass := TWlRegistry;
  if not AProxyClass.InheritsFrom(TWlRegistry) then
    Raise Exception.CreateFmt('%s does not inherit from %s', [AProxyClass.ClassName, TWlRegistry]);
  Result := TWlRegistry(AProxyClass.Create(registry));
end;

function TWlDisplay.AddListener(AIntf: IWlDisplayListener): LongInt;
begin
  FUserDataRec.ListenerUserData := Pointer(AIntf);
  Result := wl_proxy_add_listener(FProxy, @vIntf_wl_display_Listener, @FUserDataRec);
end;
function TWlRegistry.Bind(AName: DWord; AInterface: Pwl_interface; AVersion: LongInt): Pwl_proxy;
begin
  Result := wl_proxy_marshal_constructor_versioned(FProxy,
      _BIND, AInterface, AVersion, AName, AInterface^.name, AVersion, nil);
end;

function TWlRegistry.AddListener(AIntf: IWlRegistryListener): LongInt;
begin
  FUserDataRec.ListenerUserData := Pointer(AIntf);
  Result := wl_proxy_add_listener(FProxy, @vIntf_wl_registry_Listener, @FUserDataRec);
end;
function TWlCallback.AddListener(AIntf: IWlCallbackListener): LongInt;
begin
  FUserDataRec.ListenerUserData := Pointer(AIntf);
  Result := wl_proxy_add_listener(FProxy, @vIntf_wl_callback_Listener, @FUserDataRec);
end;
destructor TWlOutput.Destroy;
begin
  wl_proxy_marshal(FProxy, _RELEASE);
  inherited Destroy;
end;

function TWlOutput.AddListener(AIntf: IWlOutputListener): LongInt;
begin
  FUserDataRec.ListenerUserData := Pointer(AIntf);
  Result := wl_proxy_add_listener(FProxy, @vIntf_wl_output_Listener, @FUserDataRec);
end;

procedure wl_display_error_Intf(AData: PWLUserData; Awl_display: Pwl_display; AObjectId: Pointer; ACode: DWord; AMessage: Pchar); cdecl;
var
  AIntf: IWlDisplayListener;
begin
  if AData = nil then Exit;
  AIntf := IWlDisplayListener(AData^.ListenerUserData);
  AIntf.wl_display_error(TWlDisplay(AData^.PascalObject),  Pointer(TWLProxyObject.WLToObj(AObjectId)), ACode, AMessage);
end;

procedure wl_display_delete_id_Intf(AData: PWLUserData; Awl_display: Pwl_display; AId: DWord); cdecl;
var
  AIntf: IWlDisplayListener;
begin
  if AData = nil then Exit;
  AIntf := IWlDisplayListener(AData^.ListenerUserData);
  AIntf.wl_display_delete_id(TWlDisplay(AData^.PascalObject), AId);
end;

procedure wl_registry_global_Intf(AData: PWLUserData; Awl_registry: Pwl_registry; AName: DWord; AInterface: Pchar; AVersion: DWord); cdecl;
var
  AIntf: IWlRegistryListener;
begin
  if AData = nil then Exit;
  AIntf := IWlRegistryListener(AData^.ListenerUserData);
  AIntf.wl_registry_global(TWlRegistry(AData^.PascalObject), AName, AInterface, AVersion);
end;

procedure wl_registry_global_remove_Intf(AData: PWLUserData; Awl_registry: Pwl_registry; AName: DWord); cdecl;
var
  AIntf: IWlRegistryListener;
begin
  if AData = nil then Exit;
  AIntf := IWlRegistryListener(AData^.ListenerUserData);
  AIntf.wl_registry_global_remove(TWlRegistry(AData^.PascalObject), AName);
end;

procedure wl_callback_done_Intf(AData: PWLUserData; Awl_callback: Pwl_callback; ACallbackData: DWord); cdecl;
var
  AIntf: IWlCallbackListener;
begin
  if AData = nil then Exit;
  AIntf := IWlCallbackListener(AData^.ListenerUserData);
  AIntf.wl_callback_done(TWlCallback(AData^.PascalObject), ACallbackData);
end;

procedure wl_output_geometry_Intf(AData: PWLUserData; Awl_output: Pwl_output; AX: LongInt; AY: LongInt; APhysicalWidth: LongInt; APhysicalHeight: LongInt; ASubpixel: LongInt; AMake: Pchar; AModel: Pchar; ATransform: LongInt); cdecl;
var
  AIntf: IWlOutputListener;
begin
  if AData = nil then Exit;
  AIntf := IWlOutputListener(AData^.ListenerUserData);
  AIntf.wl_output_geometry(TWlOutput(AData^.PascalObject), AX, AY, APhysicalWidth, APhysicalHeight, ASubpixel, AMake, AModel, ATransform);
end;

procedure wl_output_mode_Intf(AData: PWLUserData; Awl_output: Pwl_output; AFlags: DWord; AWidth: LongInt; AHeight: LongInt; ARefresh: LongInt); cdecl;
var
  AIntf: IWlOutputListener;
begin
  if AData = nil then Exit;
  AIntf := IWlOutputListener(AData^.ListenerUserData);
  AIntf.wl_output_mode(TWlOutput(AData^.PascalObject), AFlags, AWidth, AHeight, ARefresh);
end;

procedure wl_output_done_Intf(AData: PWLUserData; Awl_output: Pwl_output); cdecl;
var
  AIntf: IWlOutputListener;
begin
  if AData = nil then Exit;
  AIntf := IWlOutputListener(AData^.ListenerUserData);
  AIntf.wl_output_done(TWlOutput(AData^.PascalObject));
end;

procedure wl_output_scale_Intf(AData: PWLUserData; Awl_output: Pwl_output; AFactor: LongInt); cdecl;
var
  AIntf: IWlOutputListener;
begin
  if AData = nil then Exit;
  AIntf := IWlOutputListener(AData^.ListenerUserData);
  AIntf.wl_output_scale(TWlOutput(AData^.PascalObject), AFactor);
end;

procedure wl_output_name_Intf(AData: PWLUserData; Awl_output: Pwl_output; AName: Pchar); cdecl;
var
  AIntf: IWlOutputListener;
begin
  if AData = nil then Exit;
  AIntf := IWlOutputListener(AData^.ListenerUserData);
  AIntf.wl_output_name(TWlOutput(AData^.PascalObject), AName);
end;

procedure wl_output_description_Intf(AData: PWLUserData; Awl_output: Pwl_output; ADescription: Pchar); cdecl;
var
  AIntf: IWlOutputListener;
begin
  if AData = nil then Exit;
  AIntf := IWlOutputListener(AData^.ListenerUserData);
  AIntf.wl_output_description(TWlOutput(AData^.PascalObject), ADescription);
end;



const
  pInterfaces: array[0..93] of Pwl_interface = (
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (@wl_callback_interface),
    (@wl_registry_interface),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (@wl_output_interface),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (@wl_output_interface),
    (nil),
    (nil),
    (nil),
    (@wl_callback_interface),
    (nil),
    (nil),
    (@wl_output_interface),
    (@wl_output_interface),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil),
    (nil)
  );

  wl_display_requests: array[0..1] of Twl_message = (
    (name: 'sync'; signature: 'n'; types: @pInterfaces[8]),
    (name: 'get_registry'; signature: 'n'; types: @pInterfaces[9])
  );
  wl_display_events: array[0..1] of Twl_message = (
    (name: 'error'; signature: 'ous'; types: @pInterfaces[0]),
    (name: 'delete_id'; signature: 'u'; types: @pInterfaces[0])
  );
  wl_registry_requests: array[0..0] of Twl_message = (
    (name: 'bind'; signature: 'usun'; types: @pInterfaces[0])
  );
  wl_registry_events: array[0..1] of Twl_message = (
    (name: 'global'; signature: 'usu'; types: @pInterfaces[0]),
    (name: 'global_remove'; signature: 'u'; types: @pInterfaces[0])
  );
  wl_callback_events: array[0..0] of Twl_message = (
    (name: 'done'; signature: 'u'; types: @pInterfaces[0])
  );
  wl_output_requests: array[0..0] of Twl_message = (
    (name: 'release'; signature: '3'; types: @pInterfaces[0])
  );
  wl_output_events: array[0..5] of Twl_message = (
    (name: 'geometry'; signature: 'iiiiissi'; types: @pInterfaces[0]),
    (name: 'mode'; signature: 'uiii'; types: @pInterfaces[0]),
    (name: 'done'; signature: '2'; types: @pInterfaces[0]),
    (name: 'scale'; signature: '2i'; types: @pInterfaces[0]),
    (name: 'name'; signature: '4s'; types: @pInterfaces[0]),
    (name: 'description'; signature: '4s'; types: @pInterfaces[0])
  );

initialization
  Pointer(vIntf_wl_display_Listener.error) := @wl_display_error_Intf;
  Pointer(vIntf_wl_display_Listener.delete_id) := @wl_display_delete_id_Intf;
  Pointer(vIntf_wl_registry_Listener.global) := @wl_registry_global_Intf;
  Pointer(vIntf_wl_registry_Listener.global_remove) := @wl_registry_global_remove_Intf;
  Pointer(vIntf_wl_callback_Listener.done) := @wl_callback_done_Intf;
  Pointer(vIntf_wl_output_Listener.geometry) := @wl_output_geometry_Intf;
  Pointer(vIntf_wl_output_Listener.mode) := @wl_output_mode_Intf;
  Pointer(vIntf_wl_output_Listener.done) := @wl_output_done_Intf;
  Pointer(vIntf_wl_output_Listener.scale) := @wl_output_scale_Intf;
  Pointer(vIntf_wl_output_Listener.name) := @wl_output_name_Intf;
  Pointer(vIntf_wl_output_Listener.description) := @wl_output_description_Intf;


  wl_display_interface.name := 'wl_display';
  wl_display_interface.version := 1;
  wl_display_interface.method_count := 2;
  wl_display_interface.methods := @wl_display_requests;
  wl_display_interface.event_count := 2;
  wl_display_interface.events := @wl_display_events;

  wl_registry_interface.name := 'wl_registry';
  wl_registry_interface.version := 1;
  wl_registry_interface.method_count := 1;
  wl_registry_interface.methods := @wl_registry_requests;
  wl_registry_interface.event_count := 2;
  wl_registry_interface.events := @wl_registry_events;

  wl_callback_interface.name := 'wl_callback';
  wl_callback_interface.version := 1;
  wl_callback_interface.method_count := 0;
  wl_callback_interface.methods := nil;
  wl_callback_interface.event_count := 1;
  wl_callback_interface.events := @wl_callback_events;

  wl_output_interface.name := 'wl_output';
  wl_output_interface.version := 4;
  wl_output_interface.method_count := 1;
  wl_output_interface.methods := @wl_output_requests;
  wl_output_interface.event_count := 6;
  wl_output_interface.events := @wl_output_events;

end.
