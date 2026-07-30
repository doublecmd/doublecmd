unit uGtk3WSControls;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Gtk3WSControls;

type

  { TGtk3WSWinControlEx }

  TGtk3WSWinControlEx = class(TGtk3WSWinControl)
  published
    class function GetCanvasScaleFactor(const AControl: TControl): Double; override;
  end;

implementation

uses
  WSLCLClasses, WSControls, Gtk3Widgets, Gtk3Procs, LazGdk3;

procedure Initialize;
begin
  WSControls.RegisterControl;
  WSControls.RegisterWinControl;
  RegisterWSComponent(TWinControl, TGtk3WSWinControlEx);
end;

{ TGtk3WSWinControlEx }

class function TGtk3WSWinControlEx.GetCanvasScaleFactor(const AControl: TControl): Double;
var
  W: TGtk3Widget;
begin
  Result := 1;

  if TWinControl(AControl).HandleAllocated then
  begin
    W := TGtk3Widget(TWinControl(AControl).Handle);
    if Gtk3IsGdkWindow(W.GetWindow) then
      Result := gdk_window_get_scale_factor(W.GetWindow);
  end;
end;

initialization
  Initialize;

end.

