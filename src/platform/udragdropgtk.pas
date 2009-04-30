{
  Drag&Drop operations for GTK.
}

unit uDragDropGtk;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, uDragDropEx
{$IF DEFINED(LCLGTK)}
  ,GLib, Gtk, Gdk
{$ELSEIF DEFINED(LCLGTK2)}
  ,GLib2, Gtk2, Gdk2
{$ENDIF}
  ;

type
  TDragDropSourceGTK = class(TDragDropSource)
    constructor Create(TargetControl: TWinControl); override;
    destructor  Destroy; override;

    function RegisterEvents(DragBeginEvent  : uDragDropEx.TDragBeginEvent;
                            RequestDataEvent: uDragDropEx.TRequestDataEvent;
                            DragEndEvent    : uDragDropEx.TDragEndEvent): Boolean; override;

    procedure UnregisterEvents; override;

    function DoDragDrop(const FileNamesList: TStringList;
                        MouseButton: TMouseButton;
                        ScreenStartPoint: TPoint): Boolean; override;

  private
    procedure ConnectSignal(name: pgChar; func: Pointer);
    procedure DisconnectSignal(func: Pointer);
  end;

  TDragDropTargetGTK = class(TDragDropTarget)
  public
    constructor Create(TargetControl: TWinControl); override;
    destructor  Destroy; override;

    function  RegisterEvents(DragEnterEvent: uDragDropEx.TDragEnterEvent;
                             DragOverEvent : uDragDropEx.TDragOverEvent;
                             DropEvent     : uDragDropEx.TDropEvent;
                             DragLeaveEvent: uDragDropEx.TDragLeaveEvent): Boolean; override;

    procedure UnregisterEvents; override;

  private
    procedure ConnectSignal(name: pgChar; func: Pointer);
    procedure DisconnectSignal(func: Pointer);
  end;

  { Source events }
  function OnDragBegin(widget: PGtkWidget; context: PGdkDragContext; param: gPointer): GBoolean; cdecl;

  function OnDragDataGet(widget: PGtkWidget; context: PGdkDragContext;
                         selection: PGtkSelectionData;
                         info, time: guint; param: gPointer): GBoolean; cdecl;

  function OnDragDataDelete(widget: PGtkWidget; context: PGdkDragContext;
                            param: gPointer): GBoolean; cdecl;

  function OnDragEnd(widget: PGtkWidget; context: PGdkDragContext; param: gPointer): GBoolean; cdecl;

  { Target events }
  function OnDragMotion(widget: PGtkWidget; context: PGdkDragContext;
                        x, y: gint; time: guint; param: gPointer): GBoolean; cdecl;

  function OnDrop(widget: PGtkWidget; context: PGdkDragContext;
                  x, y: gint; time: guint; param: gPointer): GBoolean; cdecl;

  function OnDataReceived(widget: PGtkWidget; context: PGdkDragContext;
                          x, y: gint; selection: PGtkSelectionData;
                          info, time: guint; param: gPointer): GBoolean; cdecl;

  function OnDragLeave(widget: PGtkWidget; context: PGdkDragContext;
                       time: guint; param: gPointer): GBoolean; cdecl;



  function GtkActionToDropEffect(Action: TGdkDragAction):TDropEffect;
  function DropEffectToGtkAction(DropEffect: TDropEffect):TGdkDragAction;


implementation

uses
  uClipboard;     // URI handling


type
  // Order of these should be the same as in Targets array.
  TTargetId = (tidTextUriList, tidTextPlain);

var
  Targets: array [0..1] of TGtkTargetEntry
      // 'info' field is a unique target id
      // Uri-list should be first so it can be catched before other targets, if available.
    = ((target: uriListMime  ; flags: 0; info:LongWord(tidTextUriList)),
       (target: textPlainMime; flags: 0; info:LongWord(tidTextPlain)));

  // True, if the user is already dragging inside the target control.
  // Used to simulate drag-enter event in drag-motion handler.
  DragEntered: Boolean = False;

{ ---------- TDragDropSourceGTK ---------- }

constructor TDragDropSourceGTK.Create(TargetControl: TWinControl);
begin
  inherited Create(TargetControl);
end;

destructor TDragDropSourceGTK.Destroy;
begin
  inherited;
end;

procedure TDragDropSourceGTK.ConnectSignal(name: pgChar; func: Pointer);
begin
  gtk_signal_connect(PGtkObject(GetControl.Handle),
                     name,
                     TGtkSignalFunc(func),
                     gPointer(Self)); // Pointer to class instance
end;

procedure TDragDropSourceGTK.DisconnectSignal(func: Pointer);
begin
  gtk_signal_disconnect_by_func(PGtkObject(GetControl.Handle),
                                TGtkSignalFunc(func),
                                gPointer(Self));
end;

function TDragDropSourceGTK.RegisterEvents(DragBeginEvent  : uDragDropEx.TDragBeginEvent;
                                           RequestDataEvent: uDragDropEx.TRequestDataEvent;
                                           DragEndEvent    : uDragDropEx.TDragEndEvent): Boolean;
begin
  inherited;

  GetControl.HandleNeeded;
  if GetControl.HandleAllocated = True then
  begin
    // We don't set up as a drag source here, as we handle it manually.

    ConnectSignal('drag_begin',       @OnDragBegin);
    ConnectSignal('drag_data_get',    @OnDragDataGet);
    ConnectSignal('drag_data_delete', @OnDragDataDelete);
    ConnectSignal('drag_end',         @OnDragEnd);
    //'drag-failed'(widget, context, result:guint);

    Result := True;
  end;
end;

procedure TDragDropSourceGTK.UnregisterEvents;
begin
  DisconnectSignal(@OnDragBegin);
  DisconnectSignal(@OnDragDataGet);
  DisconnectSignal(@OnDragDataDelete);
  DisconnectSignal(@OnDragEnd);

  inherited;
end;

function TDragDropSourceGTK.DoDragDrop(const FileNamesList: TStringList;
                                       MouseButton: TMouseButton;
                                       ScreenStartPoint: TPoint): Boolean;
var
  PList: PGtkTargetList;
  context: PGdkDragContext;
  ButtonNr: Integer;
begin
  Result := False;

  FFileNamesList.Assign(FileNamesList);

  case MouseButton of
    mbLeft  :  ButtonNr := 1;
    mbMiddle:  ButtonNr := 2;
    mbRight :  ButtonNr := 3;
    else       Exit;
  end;

  PList := gtk_target_list_new(@Targets[0], Length(Targets)); // Will be freed by GTK

  if Assigned(PList) then
  begin
    context := gtk_drag_begin(
                   PGtkWidget(GetControl.Handle),
                   PList,
                   // Allowed effects
                   GDK_ACTION_COPY or GDK_ACTION_MOVE or GDK_ACTION_LINK or GDK_ACTION_ASK,
                   ButtonNr,
                   nil // no event - we're starting manually
    );

    if Assigned(context) then
      Result:=True;
  end;
end;

{ ---------- TDragDropTargetGTK ---------- }

constructor TDragDropTargetGTK.Create(TargetControl: TWinControl);
begin
  inherited Create(TargetControl);
end;

destructor TDragDropTargetGTK.Destroy;
begin
  inherited;
end;

procedure TDragDropTargetGTK.ConnectSignal(name: pgChar; func: Pointer);
begin
  gtk_signal_connect(PGtkObject(GetControl.Handle),
                     name,
                     TGtkSignalFunc(func),
                     gPointer(Self)); // Pointer to class instance
end;

procedure TDragDropTargetGTK.DisconnectSignal(func: Pointer);
begin
  gtk_signal_disconnect_by_func(PGtkObject(GetControl.Handle),
                                TGtkSignalFunc(func),
                                gPointer(Self));
end;

function  TDragDropTargetGTK.RegisterEvents(
                         DragEnterEvent: uDragDropEx.TDragEnterEvent;
                         DragOverEvent : uDragDropEx.TDragOverEvent;
                         DropEvent     : uDragDropEx.TDropEvent;
                         DragLeaveEvent: uDragDropEx.TDragLeaveEvent): Boolean;
begin
  inherited;

  GetControl.HandleNeeded;
  if GetControl.HandleAllocated = True then
  begin
    // Set up as drag target.
    gtk_drag_dest_set(
             PGtkWidget(GetControl.Handle),
             // default handling of some signals
             GTK_DEST_DEFAULT_ALL,
             // What targets the drag source promises to supply.
             @Targets[0], Length(Targets),
             // Effects that target supports
             GDK_ACTION_COPY or GDK_ACTION_MOVE or GDK_ACTION_LINK or GDK_ACTION_ASK
    );

    ConnectSignal('drag_motion',        @OnDragMotion);
    ConnectSignal('drag_drop',          @OnDrop);
    ConnectSignal('drag_data_received', @OnDataReceived);
    ConnectSignal('drag_leave',         @OnDragLeave);

    Result := True;
  end;
end;

procedure TDragDropTargetGTK.UnregisterEvents;
begin
  DisconnectSignal(@OnDragMotion);
  DisconnectSignal(@OnDrop);
  DisconnectSignal(@OnDataReceived);
  DisconnectSignal(@OnDragLeave);

  if GetControl.HandleAllocated = True then
    gtk_drag_dest_unset(PGtkWidget(GetControl.Handle));

  inherited;
end;

{ ---------- Source events ---------- }

function OnDragBegin(widget: PGtkWidget; context: PGdkDragContext; param: gPointer): GBoolean; cdecl;
var
  DragDropSource: TDragDropSourceGTK;
begin
  DragDropSource := TDragDropSourceGTK(param);

  if Assigned(DragDropSource.GetDragBeginEvent) then
    Result := DragDropSource.GetDragBeginEvent()()
  else
    Result := True;
end;

function OnDragDataGet(widget: PGtkWidget; context: PGdkDragContext;
                       selection: PGtkSelectionData;
                       info, time: guint; param: gPointer): GBoolean; cdecl;
var
  DragDropSource: TDragDropSourceGTK;
  dataString: string;
  i: Integer;
begin
  DragDropSource := TDragDropSourceGTK(param);

  if (info < Low(Targets)) or (info > High(Targets)) then
  begin
    // Should not happen, as we didn't promise other targets in gtk_drag_begin.
    Result := False;
    Exit;
  end;

  if Assigned(DragDropSource.GetRequestDataEvent) then

  begin
    // Event has a handler assigned, so ask the control for data string.

    dataString := DragDropSource.GetRequestDataEvent()(
                       DragDropSource.GetFileNamesList,
                       Targets[info].target,
                       // context^.action - the action chosen by the destination
                       GtkActionToDropEffect(context^.action));

  end

  else

  case TTargetId(info) of

    tidTextUriList:
      dataString := FormatUriList(DragDropSource.GetFileNamesList);

    tidTextPlain:
      dataString := FormatTextPlain(DragDropSource.GetFileNamesList);

  end;

  // gtk_selection_data_set makes a copy of passed data and zero-terminates it.
  gtk_selection_data_set(selection,
                         gdk_atom_intern(Targets[info].target, gtk_true),
                         Sizeof(dataString[1]) * 8, // nr of bits per unit (char)
                         pguchar(@dataString[1]),
                         Length(dataString));

  Result := True;
end;

function OnDragDataDelete(widget: PGtkWidget; context: PGdkDragContext;
                          param: gPointer): GBoolean; cdecl;
var
  DragDropSource: TDragDropSourceGTK;
begin
  DragDropSource := TDragDropSourceGTK(param);

  Result := True;
end;

function OnDragEnd(widget: PGtkWidget; context: PGdkDragContext; param: gPointer): GBoolean; cdecl;
var
  DragDropSource: TDragDropSourceGTK;
begin
  DragDropSource := TDragDropSourceGTK(param);

  if Assigned(DragDropSource.GetDragEndEvent) then
    Result := DragDropSource.GetDragEndEvent()()
  else
    Result := True;
end;

{ ---------- Target events ---------- }

function OnDragMotion(widget: PGtkWidget; context: PGdkDragContext;
                      x, y: gint; time: guint; param: gPointer): GBoolean; cdecl;
var
  DropEffect: TDropEffect;
  Action: TGdkDragAction;
  CursorPosition: TPoint;
  DragDropTarget: TDragDropTargetGTK;
begin
  DragDropTarget := TDragDropTargetGTK(param);

  Result := True; // default to accepting drag movement

//  context^.suggested_action - the action suggested by the source
//  context^.actions          - a bitmask of actions proposed by the source
//                              when suggested_action is GDK_ACTION_ASK.

  DropEffect := GtkActionToDropEffect(context^.suggested_action);

  CursorPosition := DragDropTarget.GetControl.ClientToScreen(Point(X, Y));

  if DragEntered = False then
  begin
    // This is the first time a cursor is moving inside the window
    // (possibly after a previous drag-leave event).
    DragEntered := True;

    if Assigned(DragDropTarget.GetDragEnterEvent) then
      Result := DragDropTarget.GetDragEnterEvent()(DropEffect, CursorPosition);
  end
  else
  begin
    if Assigned(DragDropTarget.GetDragOverEvent) then
      Result := DragDropTarget.GetDragOverEvent()(DropEffect, CursorPosition);
  end;

  if Result = True then
    Action := DropEffectToGtkAction(DropEffect)
  else
    Action := 0; // don't accept dragging

  // Reply with appropriate 'action'.
  gdk_drag_status(context, Action, time);
end;

function OnDataReceived(widget: PGtkWidget; context: PGdkDragContext; x, y: gint;
                        selection: PGtkSelectionData; info, time: guint; param: gPointer): GBoolean; cdecl;
var
  DragDropTarget: TDragDropTargetGTK;
  DropEffect: TDropEffect;
  FileNamesList: TStringList = nil;
  CursorPosition: TPoint;
  uriList: string;
begin
  DragDropTarget := TDragDropTargetGTK(param);

  DropEffect := GtkActionToDropEffect(context^.suggested_action);

  CursorPosition := DragDropTarget.GetControl.ClientToScreen(Point(X, Y));

  Result := False;

  if Assigned(DragDropTarget.GetDropEvent) and
     Assigned(selection) and Assigned(selection^.data) and
     (selection^.length > 0) // if selection length < 0 data is invalid
  then
  begin
    SetString(uriList, PChar(selection^.data), selection^.length);

    // 'info' denotes which target was matched by gtk_drag_get_data
    case TTargetId(info) of

      tidTextUriList:
        uriList := URIDecode(Trim(uriList));

      tidTextPlain:
        // try decoding, as text/plain may also be percent-encoded
        uriList := URIDecode(Trim(uriList));

      else
        Exit; // not what we hoped for

    end;

    try
      FileNamesList := ExtractFilenames(uriList);

      if Assigned(FileNamesList) and (FileNamesList.Count > 0) then
        Result := DragDropTarget.GetDropEvent()(FileNamesList, DropEffect, CursorPosition);

    finally
      if Assigned(FileNamesList) then
        FreeAndNil(FileNamesList);
    end;

  end;

  // gtk_drag_finish is called automatically, because
  // GTK_DEST_DEFAULT_DROP flag was passed to gtk_drag_dest_set.
end;

function OnDrop(widget: PGtkWidget; context: PGdkDragContext;
                x, y: gint; time: guint; param: gPointer): GBoolean; cdecl;
var
  DragDropTarget: TDragDropTargetGTK;
begin
  DragDropTarget := TDragDropTargetGTK(param);

  Result := True;
end;

function OnDragLeave(widget: PGtkWidget; context: PGdkDragContext;
                     time: guint; param: gPointer): GBoolean; cdecl;
var
  DragDropTarget: TDragDropTargetGTK;
begin
  DragDropTarget := TDragDropTargetGTK(param);

  DragEntered := False;

  if Assigned(DragDropTarget.GetDragLeaveEvent) then
    Result := DragDropTarget.GetDragLeaveEvent()()
  else
    Result:= True;
end;

{ ---------------------------------------------------------------------------- }

function GtkActionToDropEffect(Action: TGdkDragAction):TDropEffect;
begin
  case Action of
    GDK_ACTION_COPY: Result := DropCopyEffect;
    GDK_ACTION_MOVE: Result := DropMoveEffect;
    GDK_ACTION_LINK: Result := DropLinkEffect;
    GDK_ACTION_ASK : Result := DropAskEffect;
    else             Result := DropNoEffect;
  end;
end;

function DropEffectToGtkAction(DropEffect: TDropEffect):TGdkDragAction;
begin
  case DropEffect of
    DropCopyEffect: Result := GDK_ACTION_COPY;
    DropMoveEffect: Result := GDK_ACTION_MOVE;
    DropLinkEffect: Result := GDK_ACTION_LINK;
    DropAskEffect : Result := GDK_ACTION_ASK;
    else            Result := 0;
  end;
end;

end.

