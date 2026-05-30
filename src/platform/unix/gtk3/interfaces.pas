unit Interfaces;

{$mode objfpc}{$H+}

interface

uses
  InterfaceBase, LCLType, Gtk3Int;

type

  { TGtk3WidgetSetEx }

  TGtk3WidgetSetEx = class(TGtk3WidgetSet)
  public
    function GetDpiForMonitor(hMonitor: HMONITOR; dpiType: TMonitorDpiType; out dpiX: UINT; out dpiY: UINT): HRESULT; override;
  end;

implementation

uses
  Forms, LazGLib2, LazGdk3;

{ TGtk3WidgetSetEx }

function TGtk3WidgetSetEx.GetDpiForMonitor(hMonitor: HMONITOR;
  dpiType: TMonitorDpiType; out dpiX: UINT; out dpiY: UINT): HRESULT;
var
  ADisplay: PGdkDisplay;
  AMonitor: PGdkMonitor;
  AMonitorRect: TGdkRectangle;
  AWmm, AHmm, AMonitorIdx, ANMon: gint;
  AScreenDpi: gdouble;
  AFallback: UINT;
begin
  Result := S_OK;
  AScreenDpi := gdk_screen_get_resolution(gdk_screen_get_default);

  if AScreenDpi > 0 then
    AFallback := Round(AScreenDpi)
  else
    AFallback := 96;

  dpiX := AFallback;
  dpiY := AFallback;

  if hMonitor = 0 then
    Exit;

  AMonitorIdx := Integer(hMonitor) - 1;
  ADisplay := gdk_display_get_default;

  if ADisplay = nil then
    Exit;

  ANMon := gdk_display_get_n_monitors(ADisplay);
  if (AMonitorIdx < 0) or (AMonitorIdx >= ANMon) then
  begin
    Result := S_FALSE;
    Exit;
  end;

  AMonitor := gdk_display_get_monitor(ADisplay, AMonitorIdx);
  if AMonitor = nil then
  begin
    Result := S_FALSE;
    Exit;
  end;

  case dpiType of
    MDT_RAW_DPI:
    begin
      gdk_monitor_get_geometry(AMonitor, @AMonitorRect);
      AWmm := gdk_monitor_get_width_mm(AMonitor);
      AHmm := gdk_monitor_get_height_mm(AMonitor);
      if AWmm > 0 then
        dpiX := Round(AMonitorRect.width * 25.4 / AWmm)
      else
        dpiX := 96;
      if AHmm > 0 then
        dpiY := Round(AMonitorRect.height * 25.4 / AHmm)
      else
        dpiY := 96;
    end;
  end;
end;

initialization
  CreateWidgetset(TGtk3WidgetSetEx);

finalization
  FreeWidgetSet;

end.

