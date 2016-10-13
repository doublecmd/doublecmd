{
  Workaround:
  http://doublecmd.sourceforge.net/mantisbt/view.php?id=1473
}

unit uGtk2FixCursorPos;

{$mode objfpc}{$H+}

interface

implementation

uses
  Classes, SysUtils, Gtk2WSStdCtrls, Gtk2, Gtk2Def, Gtk2WSSpin, Gtk2Proc,
  WSLCLClasses, StdCtrls, Glib2, Gtk2Globals, Spin;

type

  { TGtk2WSCustomEditEx }

  TGtk2WSCustomEditEx = class(TGtk2WSCustomEdit)
  published
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); override;
  end;

  { TGtk2WSCustomFloatSpinEditEx }

  TGtk2WSCustomFloatSpinEditEx = class(TGtk2WSCustomFloatSpinEdit)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); override;
  end;

procedure gtkcuttoclip_ex(widget: PGtkWidget; {%H-}data: gPointer); cdecl;
var
  Info: PWidgetInfo;
begin
  if (Widget <> nil) and (GTK_IS_ENTRY(Widget)) then
  begin
    Info := GetWidgetInfo(Widget, False);
    Include(Info^.Flags, wwiInvalidEvent);
  end;
end;

procedure gtkchanged_editbox_backspace_ex(widget: PGtkWidget; {%H-}data: gPointer); cdecl;
var
  Info: PWidgetInfo;
begin
  if (Widget <> nil) and (GTK_IS_ENTRY(Widget)) then
  begin
    Info := GetWidgetInfo(Widget, False);
    Include(Info^.Flags, wwiInvalidEvent);
  end;
end;

{ TGtk2WSCustomEditEx }

class procedure TGtk2WSCustomEditEx.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
var
  gObject: PGTKObject;
  ALCLObject: TObject;
begin
  inherited SetCallbacks(AGtkWidget, AWidgetInfo);

  ALCLObject:= AWidgetInfo^.LCLObject;

  // gObject
  if AGtkWidget = nil then gObject := ObjectToGTKObject(ALCLObject)
  else gObject := PGtkObject(AGtkWidget);
  if gObject = nil then Exit;

  if GTK_IS_ENTRY(gObject) then
  begin
    ConnectSignal(gObject, 'cut-clipboard', @gtkcuttoclip_ex, ALCLObject);
    ConnectSignal(gObject, 'backspace', @gtkchanged_editbox_backspace_ex, ALCLObject);
    ConnectSignal(gObject, 'delete-from-cursor', @gtkchanged_editbox_delete, ALCLObject);
  end;
end;

{ TGtk2WSCustomFloatSpinEditEx }

class procedure TGtk2WSCustomFloatSpinEditEx.SetCallbacks(
  const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSCustomEditEx.SetCallbacks(AGtkWidget, AWidgetInfo);
end;

procedure Initialize;
begin
 // Replace TCustomEdit widgetset class
  with TCustomEdit.Create(nil) do Free;
  RegisterWSComponent(TCustomEdit, TGtk2WSCustomEditEx);
  // Replace TCustomFloatSpinEdit widgetset class
  with TCustomFloatSpinEdit.Create(nil) do Free;
  RegisterWSComponent(TCustomFloatSpinEdit, TGtk2WSCustomFloatSpinEditEx);
end;

initialization
  Initialize;

end.

