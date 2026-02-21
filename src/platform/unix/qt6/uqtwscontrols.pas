unit uQtWSControls;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, QtWSControls;

type

  { TQtWSControlEx }

  TQtWSControlEx = class(TQtWSControl)
  published
    class function GetCanvasScaleFactor(const AControl: TControl): Double; override;
  end;

  { TQtWSWinControlEx }

  TQtWSWinControlEx = class(TQtWSWinControl)
  published
    class function GetCanvasScaleFactor(const AControl: TControl): Double; override;
  end;

implementation

uses
  Types, Forms, QtWSFactory, WSLCLClasses, WSControls, Interfaces, QtInt, Qt6,
  uWayland;

procedure Initialize;
begin
  if IsWayland then
  begin
    WSControls.RegisterControl;
    RegisterWSComponent(TControl, TQtWSControlEx);

    WSControls.RegisterWinControl;
    RegisterWSComponent(TWinControl, TQtWSWinControlEx);
  end;
end;

function GetControlScaleFactor(const AControl: TControl): Double;
var
  ASize: TSize;
  AName: String;
  Index: Integer;
  WName: WideString;
  AScreen: QScreenH;
  AForm: TCustomForm;
  AMonitor: TMonitor;
  WScreen: TWaylandScreen;
begin
  AForm:= GetParentForm(AControl);
  if Assigned(AForm) then
  begin
    AMonitor:= AForm.Monitor;
    if (AMonitor = nil) then
      AScreen:= nil
    else begin
      AScreen:= QGuiApplication_screenFromNumber(AMonitor.Handle - 1);
    end;
    if (AScreen = nil) then AScreen:= QGuiApplication_primaryScreen();
    QScreen_size(AScreen, @ASize);
    QScreen_name(AScreen, @WName);
    AName:= UTF8Encode(WName);

    // Find corresponding Wayland screen by name
    for Index:= 0 to QtWidgetSetEx.Wayland.Screens.Count - 1 do
    begin
      WScreen:= TWaylandScreen(QtWidgetSetEx.Wayland.Screens[Index]);

      if SameText(AName, WScreen.Name) then
      begin
        // Calculate fractional scaling factor
        Result:= ((WScreen.Width / ASize.Width) + (WScreen.Height / ASize.Height)) * 0.5;
        Result:= Round(Result * 100) / 100;
        Exit;
      end;
    end;
  end;
  Result:= 1.0;
end;

{ TQtWSControlEx }

class function TQtWSControlEx.GetCanvasScaleFactor(const AControl: TControl): Double;
begin
  Result:= GetControlScaleFactor(AControl);
end;

{ TQtWSWinControlEx }

class function TQtWSWinControlEx.GetCanvasScaleFactor(const AControl: TControl): Double;
begin
  Result:= GetControlScaleFactor(AControl);
end;

initialization
  Initialize;

end.

