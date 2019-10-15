{
  Workaround:
  http://doublecmd.sourceforge.net/mantisbt/view.php?id=1473
}

unit uGtk2FixCursorPos;

{$mode objfpc}{$H+}

interface

uses
  LCLVersion;

implementation

uses
  Classes, SysUtils, Gtk2WSStdCtrls, Gtk2, Gtk2Def, Gtk2WSSpin, Gtk2Proc,
  WSLCLClasses, StdCtrls, Glib2, Gtk2Globals, Spin, LMessages, LazUTF8, Gdk2,
  Controls, ExtCtrls
{$if lcl_fullversion < 1090000}
  , Gtk2WSExtCtrls, Graphics
{$endif}
  ;

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

{$if lcl_fullversion < 1090000}

  { TGtk2WSCustomPanelEx }

  TGtk2WSCustomPanelEx = class(TGtk2WSCustomPanel)
  published
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;
  end;

{$endif}

procedure gtkcuttoclip_ex(widget: PGtkWidget; {%H-}data: gPointer); cdecl;
var
  Info: PWidgetInfo;
begin
  if (Widget <> nil) and (GTK_IS_ENTRY(Widget)) then
  begin
    Info := GetWidgetInfo(Widget);
    Include(Info^.Flags, wwiInvalidEvent);
  end;
end;

procedure gtkchanged_editbox_backspace_ex(widget: PGtkWidget; {%H-}data: gPointer); cdecl;
var
  Info: PWidgetInfo;
begin
  if (Widget <> nil) and (GTK_IS_ENTRY(Widget)) then
  begin
    Info := GetWidgetInfo(Widget);
    Include(Info^.Flags, wwiInvalidEvent);
  end;
end;

procedure gtkchanged_editbox_ex(widget: PGtkWidget; {%H-}data: gPointer); cdecl; forward;

function GtkEntryDelayCursorPos(AGtkWidget: Pointer): GBoolean; cdecl;
var
  Info: PWidgetInfo;
begin
  Result := AGtkWidget <> nil;
  if AGtkWidget <> nil then
  begin
    g_idle_remove_by_data(AGtkWidget);
    Info := GetWidgetInfo(AGtkWidget);
    if Info <> nil then
      gtkchanged_editbox_ex(PGtkWidget(AGtkWidget),
        Info^.LCLObject);
  end;
end;

procedure gtkchanged_editbox_ex(widget: PGtkWidget; {%H-}data: gPointer); cdecl;
var
  Mess : TLMessage;
  GStart, GEnd: gint;
  Info: PWidgetInfo;
  EntryText: PgChar;
  NeedCursorCheck: Boolean;
begin
  if LockOnChange(PgtkObject(Widget),0)>0 then exit;
  {$IFDEF EventTrace}
  EventTrace('changed_editbox', data);
  {$ENDIF}
  NeedCursorCheck := False;
  if GTK_IS_ENTRY(Widget) and (not (TObject(data) is TCustomFloatSpinEdit)) then
  begin
    // lcl-do-not-change-selection comes from gtkKeyPress.
    // Only floatspinedit sets that data, so default is nil. issue #18679
    if g_object_get_data(PGObject(Widget),'lcl-do-not-change-selection') = nil then
    begin
      {cheat GtkEditable to update cursor pos in gtkEntry. issue #7243}
      gtk_editable_get_selection_bounds(PGtkEditable(Widget), @GStart, @GEnd);
      EntryText := gtk_entry_get_text(PGtkEntry(Widget));
      if (GStart = GEnd) and
        (UTF8Length(EntryText) >= PGtkEntry(Widget)^.text_length) then
      begin
        Info := GetWidgetInfo(Widget);
        {do not update position if backspace or delete pressed}
        if wwiInvalidEvent in Info^.Flags then
        begin
          Exclude(Info^.Flags, wwiInvalidEvent);

          {take care of pasted data since it does not return proper cursor pos.}
          // issue #7243
          if g_object_get_data(PGObject(Widget),'lcl-delay-cm_textchaged') <> nil then
          begin
            g_object_set_data(PGObject(Widget),'lcl-delay-cm_textchaged',nil);
            g_object_set_data(PGObject(Widget),'lcl-gtkentry-pasted-data',Widget);
            g_idle_add(@GtkEntryDelayCursorPos, Widget);
            exit;
          end;
        end else
        begin
          // if we change selstart in OnChange event new cursor pos need to
          // be postponed in TGtk2WSCustomEdit.SetSelStart
          NeedCursorCheck := True;
          if g_object_get_data(PGObject(Widget),'lcl-gtkentry-pasted-data') <> nil then
          begin
            g_object_set_data(PGObject(Widget),'lcl-gtkentry-pasted-data',nil);
            gtk_editable_set_position(PGtkEditable(Widget), GStart);
          end else
          begin
            //NeedCursorCheck := True;
            g_object_set_data(PGObject(Widget),'lcl-gtkentry-pasted-data',Widget);
            g_idle_add(@GtkEntryDelayCursorPos, Widget);
            exit;
          end;
        end;
      end;
    end else
      g_object_set_data(PGObject(Widget),'lcl-do-not-change-selection', nil);
  end;

  if NeedCursorCheck then
    LockOnChange(PgtkObject(Widget), +1);
  FillByte(Mess{%H-},SizeOf(Mess),0);
  Mess.Msg := CM_TEXTCHANGED;
  DeliverMessage(Data, Mess);
  if NeedCursorCheck then
    LockOnChange(PgtkObject(Widget), -1);
end;

function gtkMotionNotifyEx(Widget:PGTKWidget; Event: PGDKEventMotion;
  Data: gPointer): GBoolean; cdecl;
var
  ACtl: TWinControl;
begin
  // Call inherited function
  Result:= gtkMotionNotify(Widget, Event, Data);

  ACtl:= TWinControl(Data);
  if (ACtl is TCustomEdit) then
  begin
    if (Event^.x < 0) or (Event^.y < 0) or
       (Event^.x > ACtl.Width) or (Event^.y > ACtl.Height) then
      Result:= CallBackDefaultReturn;
  end;
end;

{$if lcl_fullversion < 1090000}

{ TGtk2WSCustomPanelEx }

class function TGtk2WSCustomPanelEx.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
const
  DefColors: array[TDefaultColorType] of TColor = (
 { dctBrush } clBackground,
 { dctFont } clBtnText
  );
begin
  Result := DefColors[ADefaultColorType];
end;

{$endif}

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
    if ALCLObject is TCustomFloatSpinEdit then
      ConnectSignal(gObject, 'value-changed', @gtkchanged_editbox_ex, ALCLObject)
    else begin
      ConnectSignal(gObject, 'changed', @gtkchanged_editbox_ex, ALCLObject);
    end;
    ConnectSignal(gObject, 'cut-clipboard', @gtkcuttoclip_ex, ALCLObject);
    g_signal_handlers_disconnect_by_func(gObject, @gtkchanged_editbox, ALCLObject);
    ConnectSignal(gObject, 'backspace', @gtkchanged_editbox_backspace_ex, ALCLObject);
    ConnectSignal(gObject, 'delete-from-cursor', @gtkchanged_editbox_delete, ALCLObject);

    g_signal_handlers_disconnect_by_func(gObject, @GTKMotionNotify, ALCLObject);
    ConnectSignal(gObject, 'motion-notify-event', @GTKMotionNotifyEx, ALCLObject,
                  GDK_POINTER_MOTION_HINT_MASK or GDK_POINTER_MOTION_MASK);
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
{$if lcl_fullversion < 1090000}
  // Replace TCustomPanel widgetset class
  with TCustomPanel.Create(nil) do Free;
  RegisterWSComponent(TCustomPanel, TGtk2WSCustomPanelEx);
{$endif}
end;

initialization
  Initialize;

end.

