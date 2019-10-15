unit Interfaces;

{$mode objfpc}{$H+}

interface

uses
  InterfaceBase, LCLType, Gtk2Int;

type

  { TGtk2WidgetSetEx }

  TGtk2WidgetSetEx = class(TGtk2WidgetSet)
  public
    function SetCursor(ACursor: HICON): HCURSOR; override;
  end;

implementation

uses
  Forms, Controls, Gtk2Extra, Gtk2Def, Gtk2Proc, Glib2, Gdk2, Gtk2, Gdk2x, X, XLib;

procedure XSetWindowCursor(AWindow: PGdkWindow; ACursor: PGdkCursor);
var
  XCursor: TCursor = None;
begin
  gdk_window_set_cursor(AWindow, ACursor);
  if Assigned(ACursor) then begin
    XCursor:= gdk_x11_cursor_get_xcursor(ACursor)
  end;
  XDefineCursor(gdk_x11_get_default_xdisplay,
                gdk_x11_drawable_get_xid(AWindow),
                XCursor);
end;

{------------------------------------------------------------------------------
  procedure: SetWindowCursor
  Params:  AWindow : PGDkWindow, ACursor: PGdkCursor, ASetDefault: Boolean
  Returns: Nothing

  Sets the cursor for a window.
  Tries to avoid messing with the cursors of implicitly created
  child windows (e.g. headers in TListView) with the following logic:
  - If Cursor <> nil, saves the old cursor (if not already done or ASetDefault = true)
    before setting the new one.
  - If Cursor = nil, restores the old cursor (if not already done).

  Unfortunately gdk_window_get_cursor is only available from
  version 2.18, so it needs to be retrieved dynamically.
  If gdk_window_get_cursor is not available, the cursor is set
  according to LCL widget data.
  ------------------------------------------------------------------------------}
procedure SetWindowCursor(AWindow: PGdkWindow; Cursor: PGdkCursor; ASetDefault: Boolean);
var
  OldCursor: PGdkCursor;
  Data: gpointer;
  Info: PWidgetInfo;
begin
  Info := nil;
  gdk_window_get_user_data(AWindow, @Data);
  if (Data <> nil) and GTK_IS_WIDGET(Data) then
  begin
    Info := GetWidgetInfo(PGtkWidget(Data));
  end;
  if not Assigned(gdk_window_get_cursor) and (Info = nil)
  then Exit;
  if ASetDefault then //and ((Cursor <> nil) or ( <> nil)) then
  begin
    // Override any old default cursor
    g_object_steal_data(PGObject(AWindow), 'havesavedcursor'); // OK?
    g_object_steal_data(PGObject(AWindow), 'savedcursor');
    XSetWindowCursor(AWindow, Cursor);
    Exit;
  end;
  if Cursor <> nil then
  begin
    if Assigned(gdk_window_get_cursor)
    then OldCursor := gdk_window_get_cursor(AWindow)
    else OldCursor := {%H-}PGdkCursor(Info^.ControlCursor);
    // As OldCursor can be nil, use a separate key to indicate whether it
    // is stored.
    if ASetDefault or (g_object_get_data(PGObject(AWindow), 'havesavedcursor') = nil) then
    begin
      g_object_set_data(PGObject(AWindow), 'havesavedcursor', gpointer(1));
      g_object_set_data(PGObject(AWindow), 'savedcursor', gpointer(OldCursor));
    end;
    // gdk_pointer_grab(AWindow, False, 0, AWindow, Cursor, 1);
    try
      XSetWindowCursor(AWindow, Cursor);
    finally
      // gdk_pointer_ungrab(0);
    end;
  end else
  begin
    if g_object_steal_data(PGObject(AWindow), 'havesavedcursor') <> nil then
    begin
      Cursor := g_object_steal_data(PGObject(AWindow), 'savedcursor');
      XSetWindowCursor(AWindow, Cursor);
    end;
  end;
end;

{------------------------------------------------------------------------------
  procedure: SetWindowCursor
  Params:  AWindow : PGDkWindow, ACursor: HCursor, ARecursive: Boolean
  Returns: Nothing

  Sets the cursor for a window (or recursively for window with children)
 ------------------------------------------------------------------------------}
procedure SetWindowCursor(AWindow: PGdkWindow; ACursor: HCursor;
  ARecursive: Boolean; ASetDefault: Boolean);
var
  Cursor: PGdkCursor;

  procedure SetCursorRecursive(AWindow: PGdkWindow);
  var
    ChildWindows, ListEntry: PGList;
  begin
    SetWindowCursor(AWindow, Cursor, ASetDefault);

    ChildWindows := gdk_window_get_children(AWindow);

    ListEntry := ChildWindows;
    while ListEntry <> nil do
    begin
      SetCursorRecursive(PGdkWindow(ListEntry^.Data));
      ListEntry := ListEntry^.Next;
    end;
    g_list_free(ChildWindows);
  end;
begin
  Cursor := {%H-}PGdkCursor(ACursor);
  if ARecursive
  then SetCursorRecursive(AWindow)
  else SetWindowCursor(AWindow, Cursor, ASetDefault);
end;

{------------------------------------------------------------------------------
  procedure: SetGlobalCursor
  Params:  ACursor: HCursor
  Returns: Nothing

  Sets the cursor for all toplevel windows. Also sets the cursor for all child
  windows recursively provided gdk_get_window_cursor is available.
 ------------------------------------------------------------------------------}
procedure SetGlobalCursor(Cursor: HCURSOR);
var
  TopList, List: PGList;
begin
  TopList := gdk_window_get_toplevels;
  List := TopList;
  while List <> nil do
  begin
    if (List^.Data <> nil) then
      SetWindowCursor(PGDKWindow(List^.Data), Cursor,
        Assigned(gdk_window_get_cursor), False);
    list := g_list_next(list);
  end;

  if TopList <> nil then
    g_list_free(TopList);
end;

{ TGtk2WidgetSetEx }

function TGtk2WidgetSetEx.SetCursor(ACursor: HICON): HCURSOR;
begin
  gdk_window_get_cursor:= nil;
  // set global gtk cursor
  Result := FGlobalCursor;
  if ACursor = FGlobalCursor then Exit;
  if ACursor = Screen.Cursors[crDefault]
  then SetGlobalCursor(0)
  else SetGlobalCursor(ACursor);
  FGlobalCursor := ACursor;
end;

initialization
  CreateWidgetset(TGtk2WidgetSetEx);

finalization
  FreeWidgetSet;

end.

