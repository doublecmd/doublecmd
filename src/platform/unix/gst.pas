unit gst;

{$mode delphi}
{$packenum 4}

interface

uses
  SysUtils, CTypes, uGlib2;

const
  GST_CLOCK_TIME_NONE = UInt64(-1);

const
  GST_MESSAGE_EOS       = (1 << 0);
  GST_MESSAGE_ERROR     = (1 << 1);

type
  GstState = (
    GST_STATE_VOID_PENDING        = 0,
    GST_STATE_NULL                = 1,
    GST_STATE_READY               = 2,
    GST_STATE_PAUSED              = 3,
    GST_STATE_PLAYING             = 4
  );

  GstStateChangeReturn = (
    GST_STATE_CHANGE_FAILURE      = 0,
    GST_STATE_CHANGE_SUCCESS      = 1,
    GST_STATE_CHANGE_ASYNC        = 2,
    GST_STATE_CHANGE_NO_PREROLL   = 3
  );

type
  GstClockTime = guint64;
  PGstBus = type Pointer;
  PGstElement = type Pointer;
  PGstMessage = type Pointer;
  PGstMiniObject = type Pointer;
  GstMessageType = type Integer;

var
  gst_object_unref: procedure(object_: gpointer); cdecl;
  gst_element_get_bus: function(element: PGstElement): PGstBus; cdecl;
  gst_mini_object_unref: procedure(mini_object: PGstMiniObject); cdecl;
  gst_init_check: function(argc: pcint; argv: PPChar; error: PPGError): gboolean; cdecl;
  gst_element_set_state: function(element: PGstElement; state: GstState): GstStateChangeReturn; cdecl;
  gst_element_factory_make: function(const factoryname: Pgchar; const name: Pgchar): PGstElement; cdecl;
  gst_bus_timed_pop_filtered: function(bus: PGstBus; timeout: GstClockTime; types: GstMessageType): PGstMessage; cdecl;

function GST_Initialize: Boolean;
function GST_Play(const FileName: String): Boolean;

implementation

uses
  URIParser, LazLogger, DCOSUtils, uGObject2;

function WaitMsg(Parameter: Pointer): PtrInt;
var
  bus: PGstBus;
  msg: PGstMessage;
  playbin: PGstElement absolute Parameter;
begin
  Result:= 0;
  bus:= gst_element_get_bus(playbin);

  msg:= gst_bus_timed_pop_filtered(bus, GST_CLOCK_TIME_NONE, GST_MESSAGE_EOS or GST_MESSAGE_ERROR);
  if Assigned(msg) then
  begin
    gst_mini_object_unref(msg);
  end;

  gst_object_unref(bus);
  gst_element_set_state(playbin, GST_STATE_NULL);
  gst_object_unref(playbin);
end;

function GST_Play(const FileName: String): Boolean;
var
  playbin: PGstElement;
  res: GstStateChangeReturn;
begin
  playbin:= gst_element_factory_make ('playbin', 'playbin');

  Result:= Assigned(playbin);

  if Result then
  begin
    g_object_set(playbin, 'uri', [Pgchar(FilenameToURI(FileName)), nil]);

    BeginThread(@WaitMsg, playbin);

    res:= gst_element_set_state(playbin, GST_STATE_PLAYING);

    Result:= (res <> GST_STATE_CHANGE_FAILURE);
  end;
end;

const
  gstlib = 'libgstreamer-1.0.so.0';

var
  libgst: TLibHandle;

function GST_Initialize: Boolean;
var
  AMsg: String;
  AError: PGError = nil;
begin
  libgst:= SafeLoadLibrary(gstlib);
  Result:= (libgst <> NilHandle);
  if Result then
  try
    gst_init_check:= SafeGetProcAddress(libgst, 'gst_init_check');
    gst_object_unref:= SafeGetProcAddress(libgst, 'gst_object_unref');
    gst_element_get_bus:= SafeGetProcAddress(libgst, 'gst_element_get_bus');
    gst_mini_object_unref:= SafeGetProcAddress(libgst, 'gst_mini_object_unref');
    gst_element_set_state:= SafeGetProcAddress(libgst, 'gst_element_set_state');
    gst_element_factory_make:= SafeGetProcAddress(libgst, 'gst_element_factory_make');
    gst_bus_timed_pop_filtered:= SafeGetProcAddress(libgst, 'gst_bus_timed_pop_filtered');

    Result:= gst_init_check(nil, nil, @AError);

    if not Result then
    begin
      AMsg:= AError^.message;
      g_error_free(AError);
      raise Exception.Create(AMsg);
    end;
  except
    on E: Exception do
    begin
      Result:= False;
      DebugLn(E.Message);
      FreeLibrary(libgst);
    end;
  end;
end;

end.
