{
  Drag&Drop operations for QT.
}

unit uDragDropQt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, uDragDropEx,
  qtwidgets
  {$IF DEFINED(LCLQT)}
  , qt4
  {$ELSEIF DEFINED(LCLQT5)}
  , qt5
  {$ELSEIF DEFINED(LCLQT6)}
  , qt6
  {$ENDIF}
  ;

type
  TDragDropSourceQT = class(TDragDropSource)

    function RegisterEvents(DragBeginEvent  : uDragDropEx.TDragBeginEvent;
                            RequestDataEvent: uDragDropEx.TRequestDataEvent;
                            DragEndEvent    : uDragDropEx.TDragEndEvent): Boolean; override;

    function DoDragDrop(const FileNamesList: TStringList;
                        MouseButton: TMouseButton;
                        ScreenStartPoint: TPoint): Boolean; override;

  private
     function GetWidget: QWidgetH;
  end;

  TDragDropTargetQT = class(TDragDropTarget)
  public
    constructor Create(TargetControl: TWinControl); override;

    function  RegisterEvents(DragEnterEvent: uDragDropEx.TDragEnterEvent;
                             DragOverEvent : uDragDropEx.TDragOverEvent;
                             DropEvent     : uDragDropEx.TDropEvent;
                             DragLeaveEvent: uDragDropEx.TDragLeaveEvent): Boolean; override;

    procedure UnregisterEvents; override;

  private
    FEventHook : QObject_hookH;

    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; // called by QT

    function GetWidget: QWidgetH;

    function HasSupportedFormat(DropEvent: QDropEventH): Boolean;

    function OnDragEnter(DragEnterEvent: QDragEnterEventH): Boolean;
    function OnDragOver(DragMoveEvent: QDragMoveEventH): Boolean;
    function OnDrop(DropEvent: QDropEventH): Boolean;
    function OnDragLeave(DragLeaveEvent: QDragLeaveEventH): Boolean;
  end;

  function QtActionToDropEffect(Action: QtDropAction): TDropEffect;
  function DropEffectToQtAction(DropEffect: TDropEffect): QtDropAction;
  function QtDropEventPointToLCLPoint(const PDropEventPoint: PQtPoint): TPoint;


implementation

uses
  uClipboard, LCLIntf
{$IF DEFINED(XDG)}
  , uFileTransferPortal
{$ENDIF}
  ;


const
  uriListMimeW   : WideString = uriListMime;
  textPlainMimeW : WideString = textPlainMime;
{$IF DEFINED(XDG)}
  portalMimeW    : WideString = portalFileTransferMime;
{$ENDIF}


{$IF DEFINED(LCLQT5) or DEFINED(LCLQT6)}
function QDropEvent_pos(handle: QDropEventH): PQtPoint; overload;
const
  retval: TQtPoint = (x: 0; y: 0);
begin
  Result:= @retval;
  QDropEvent_pos(handle, Result);
end;
{$ENDIF}

function GetWidgetFromLCLControl(AWinControl: TWinControl): QWidgetH; inline;
begin
  // Custom controls (TQtCustomControl) are created by LCL as
  // QAbstractScrollArea with a viewport (and two scrollbars).
  // We want the viewport to be the source/target of drag&drop, so we use
  // GetContainerWidget which returns the viewport widget for custom controls
  // and regular widget handle for others.

  Result := TQtWidget(AWinControl.Handle).GetContainerWidget;
end;

{ ---------- TDragDropSourceQT ---------- }

function TDragDropSourceQT.RegisterEvents(DragBeginEvent  : uDragDropEx.TDragBeginEvent;
                                          RequestDataEvent: uDragDropEx.TRequestDataEvent;
                                          DragEndEvent    : uDragDropEx.TDragEndEvent): Boolean;
begin
  inherited;

  // RequestDataEvent is not handled in QT.

  Result := True;
end;

function TDragDropSourceQT.DoDragDrop(const FileNamesList: TStringList;
                                      MouseButton: TMouseButton;
                                      ScreenStartPoint: TPoint): Boolean;

  procedure SetMimeDataInFormat(MimeData: QMimeDataH;
                                MimeType: WideString;
                                DataString: AnsiString);
  var
    ByteArray: QByteArrayH;
  begin
    // Sized, because the portal key is transferred with a trailing zero byte
    // and the pointer-only constructor would cut the array short of it.
    ByteArray := QByteArray_create(PAnsiChar(DataString), Length(DataString));
    try
      QMimeData_setData(MimeData, @MimeType, ByteArray);
    finally
      QByteArray_destroy(ByteArray);
    end;
  end;

var
  DragObject: QDragH = nil;
  MimeData: QMimeDataH = nil;
  DropAction: QtDropAction;
{$IF DEFINED(XDG)}
  TransferKey: String;
{$ENDIF}
begin
  Result := True;
  FLastStatus := DragDropError;

  // Simulate drag-begin event.
  if Assigned(GetDragBeginEvent) then
  begin
    Result := GetDragBeginEvent()();
    if Result = False then
    begin
      FLastStatus := DragDropAborted;
      Exit;
    end;
  end;

  DragObject := QDrag_create(GetWidget);      // deleted automatically by QT
  try
    MimeData := QMimeData_create;
    QDrag_setMimeData(DragObject, MimeData); // MimeData owned by DragObject after this

{$IF DEFINED(XDG)}
    // Offered before the plain formats, the way the toolkits that implement
    // this do. A sandboxed target understands text/uri-list as well, and would
    // otherwise take a host path that it has no permission to open: it shows
    // the file name it parsed out of the URI and reads nothing at all.
    TransferKey := FileTransferRegister(FileNamesList);
    if (Length(TransferKey) > 0) then
      SetMimeDataInFormat(MimeData, portalMimeW, TransferKey + #0);
{$ENDIF}

    SetMimeDataInFormat(MimeData, uriListMimeW, FormatUriList(FileNamesList));
    SetMimeDataInFormat(MimeData, textPlainMimeW, FormatTextPlain(FileNamesList));

  except
    QDrag_destroy(DragObject);
    raise;
  end;

  // Start drag&drop operation (default to Copy action).
  // This blocks in a nested event loop until the drop has been performed or
  // the operation has been cancelled. It must be called while the input event
  // that started the drag is still being handled, otherwise the serial that
  // Qt passes to wl_data_device.start_drag is stale and the compositor
  // silently refuses to start the drag.
  DropAction := QDrag_exec(DragObject, QtCopyAction or QtLinkAction or QtMoveAction, qtCopyAction);

  if (DropAction = QtIgnoreAction) then
    FLastStatus := DragDropAborted
  else
    FLastStatus := DragDropSuccessful;

  // Simulate drag-end event.
  if Assigned(GetDragEndEvent) then
    Result := GetDragEndEvent()() and Result;
end;

function TDragDropSourceQT.GetWidget: QWidgetH;
begin
  Result := GetWidgetFromLCLControl(GetControl);
end;

{ ---------- TDragDropTargetQT ---------- }

constructor TDragDropTargetQT.Create(TargetControl: TWinControl);
begin
  inherited;

  FEventHook := nil;
end;


function TDragDropTargetQT.RegisterEvents(DragEnterEvent: uDragDropEx.TDragEnterEvent;
                                          DragOverEvent : uDragDropEx.TDragOverEvent;
                                          DropEvent     : uDragDropEx.TDropEvent;
                                          DragLeaveEvent: uDragDropEx.TDragLeaveEvent): Boolean;
begin
  inherited;

  QWidget_setAcceptDrops(GetWidget, True);

  if Assigned(FEventHook) then
    QObject_hook_destroy(FEventHook);

  // Tap into target widget's events.
  FEventHook := QObject_hook_create(GetWidget);
  QObject_hook_hook_events(FEventHook, @EventFilter);

  Result := True;
end;

procedure TDragDropTargetQT.UnregisterEvents;
begin
  QWidget_setAcceptDrops(GetWidget, False);

  if Assigned(FEventHook) then
  begin
    QObject_hook_destroy(FEventHook);
    FEventHook := nil;
  end;

  inherited;
end;

function TDragDropTargetQT.GetWidget: QWidgetH;
begin
  Result := GetWidgetFromLCLControl(GetControl);
end;

function TDragDropTargetQT.HasSupportedFormat(DropEvent: QDropEventH): Boolean;
var
  MimeData: QMimeDataH;
begin
  MimeData := QDropEvent_mimeData(DropEvent);

  if Assigned(MimeData) then
  begin
    if QMimeData_hasFormat(mimedata, @urilistmimew) or
       QMimeData_hasFormat(mimedata, @textPlainMimeW)
{$IF DEFINED(XDG)}
       or QMimeData_hasFormat(mimedata, @portalMimeW)
{$ENDIF}
    then
      Exit(True);
  end;

  Result := False;
end;

function TDragDropTargetQT.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False; // False means the event is not filtered out.

  case QEvent_type(Event) of

    QEventDragEnter:
      begin
        QEvent_accept(Event);
        OnDragEnter(QDragEnterEventH(Event));
      end;

    QEventDragMove:
      begin
        QEvent_accept(Event);
        OnDragOver(QDragMoveEventH(Event));
      end;

    QEventDrop:
      begin
        QEvent_accept(Event);
        OnDrop(QDropEventH(Event));
      end;

    QEventDragLeave:
      begin
        QEvent_accept(Event);
        OnDragLeave(QDragLeaveEventH(Event));
      end;

    // QEventDragResponse - used internally by QT
  end;
end;

function TDragDropTargetQT.OnDragEnter(DragEnterEvent: QDragEnterEventH): Boolean;
var
  CursorPosition: TPoint;
  DropEffect: TDropEffect;
  DropEvent: QDropEventH;
  QtAction: QtDropAction;
begin
  // QDragEnterEvent inherits from QDragMoveEvent, which inherits from QDropEvent.
  DropEvent := QDropEventH(DragEnterEvent);

  if not HasSupportedFormat(DropEvent) then
  begin
    QDropEvent_setDropAction(DropEvent, QtIgnoreAction);
    Result := False;
  end
  else if Assigned(GetDragEnterEvent) then
  begin
    DropEffect := QtActionToDropEffect(QDropEvent_proposedAction(DropEvent));
    CursorPosition := QtDropEventPointToLCLPoint(QDropEvent_pos(DropEvent));
    CursorPosition := GetControl.ClientToScreen(CursorPosition);

    Result := GetDragEnterEvent()(DropEffect, CursorPosition);

    if Result then
      QtAction := DropEffectToQtAction(DropEffect)
    else
      QtAction := QtIgnoreAction;

    QDropEvent_setDropAction(DropEvent, QtAction);
  end
  else
  begin
    QDropEvent_acceptProposedAction(DropEvent);
    Result := True;
  end;
end;

function TDragDropTargetQT.OnDragOver(DragMoveEvent: QDragMoveEventH): Boolean;
var
  CursorPosition: TPoint;
  DropEffect: TDropEffect;
  DropEvent: QDropEventH;
  QtAction: QtDropAction;
begin
  // QDragMoveEvent inherits from QDropEvent.
  DropEvent := QDropEventH(DragMoveEvent);

  if not HasSupportedFormat(DropEvent) then
  begin
    QDropEvent_setDropAction(DropEvent, QtIgnoreAction);
    Result := False;
  end
  else if Assigned(GetDragOverEvent) then
  begin
    DropEffect := QtActionToDropEffect(QDropEvent_proposedAction(DropEvent));
    CursorPosition := QtDropEventPointToLCLPoint(QDropEvent_pos(DropEvent));
    CursorPosition := GetControl.ClientToScreen(CursorPosition);

    Result := GetDragOverEvent()(DropEffect, CursorPosition);

    if Result then
      QtAction := DropEffectToQtAction(DropEffect)
    else
      QtAction := QtIgnoreAction;

    QDropEvent_setDropAction(DropEvent, QtAction);
  end
  else
  begin
    QDropEvent_acceptProposedAction(DropEvent);
    Result := True;
  end;
end;

function TDragDropTargetQT.OnDrop(DropEvent: QDropEventH): Boolean;

  function GetMimeDataInFormat(MimeData: QMimeDataH; MimeType: WideString): AnsiString;
  var
    ByteArray: QByteArrayH;
    Size: Integer;
    Data: PAnsiChar;
  begin
    if QMimeData_hasFormat(MimeData, @MimeType) then
    begin
      ByteArray := QByteArray_create();
      try
        QMimeData_data(MimeData, ByteArray, @MimeType);
        Size := QByteArray_size(ByteArray);
        Data := QByteArray_data(ByteArray);

        if (Size > 0) and Assigned(Data) then
          SetString(Result, Data, Size);

      finally
        QByteArray_destroy(ByteArray);
      end;
    end
    else
      Result := '';
  end;

var
  DropAction: QtDropAction;
  DropEffect: TDropEffect;
  CursorPosition: TPoint;
  uriList: String;
  FileNamesList: TStringList = nil;
  MimeData: QMimeDataH;
  Handled: Boolean = False;
{$IF DEFINED(XDG)}
  NulPos: Integer;
  TransferKey: String;
{$ENDIF}
begin
  Result := False;

  // QDropEvent_possibleActions() returns all actions allowed by the source.
  // QDropEvent_proposedAction() is the action proposed by the source.
  DropAction := QDropEvent_dropAction(DropEvent); // action to be performed by the target
  DropEffect := QtActionToDropEffect(DropAction);

  CursorPosition := QtDropEventPointToLCLPoint(QDropEvent_pos(dropEvent));
  CursorPosition := GetControl.ClientToScreen(CursorPosition);

  QDropEvent_setDropAction(DropEvent, QtIgnoreAction); // default to ignoring the drop

  MimeData := QDropEvent_mimeData(DropEvent);
  if Assigned(GetDropEvent) and Assigned(MimeData) then
  begin
{$IF DEFINED(XDG)}
    // Files offered by a sandboxed application. The payload is a key that the
    // document portal turns into names that we are allowed to open; the plain
    // formats it offers next to it point into a sandbox we cannot see into.
    if QMimeData_hasFormat(MimeData, @portalMimeW) then
    begin
      TransferKey := GetMimeDataInFormat(MimeData, portalMimeW);
      // The key is transferred with a terminating zero byte.
      NulPos := Pos(#0, TransferKey);
      if (NulPos > 0) then SetLength(TransferKey, NulPos - 1);

      FileNamesList := TStringList.Create;
      try
        if FileTransferRetrieve(Trim(TransferKey), FileNamesList) then
        begin
          Result := GetDropEvent()(FileNamesList, DropEffect, CursorPosition);
          Handled := True;
        end;
      finally
        FreeAndNil(FileNamesList);
      end;
    end;
{$ENDIF}

    // Not offered, or the portal could not resolve it: fall back to the paths.
    if not Handled then
    begin
      if QMimeData_hasFormat(MimeData, @uriListMimeW) then

        uriList := Trim(GetMimeDataInFormat(MimeData, uriListMimeW))

      else if QMimeData_hasFormat(MimeData, @textPlainMimeW) then

        // try decoding, as text/plain may also be percent-encoded
        uriList := Trim(GetMimeDataInFormat(MimeData, textPlainMimeW))

      else
        Exit;  // reject the drop

      try
        FileNamesList := ExtractFilenames(uriList, True);

        if Assigned(FileNamesList) and (FileNamesList.Count > 0) then
          Result := GetDropEvent()(FileNamesList, DropEffect, CursorPosition);

      finally
        if Assigned(FileNamesList) then
          FreeAndNil(FileNamesList);
      end;
    end;

    QDropEvent_setDropAction(DropEvent, DropAction); // accept the drop
  end;
end;

function TDragDropTargetQT.OnDragLeave(DragLeaveEvent: QDragLeaveEventH): Boolean;
begin
  if Assigned(GetDragLeaveEvent) then
    Result := GetDragLeaveEvent()()
  else
    Result := True;
end;

{ ---------------------------------------------------------------------------- }

function QtActionToDropEffect(Action: QtDropAction): TDropEffect;
begin
  case Action of
    QtCopyAction:       Result := DropCopyEffect;
    QtMoveAction:       Result := DropMoveEffect;
    QtTargetMoveAction: Result := DropMoveEffect;
    QtLinkAction:       Result := DropLinkEffect;
    else                Result := DropNoEffect;
  end;
end;

function DropEffectToQtAction(DropEffect: TDropEffect): QtDropAction;
begin
  case DropEffect of
    DropCopyEffect: Result := QtCopyAction;
    DropMoveEffect: Result := QtMoveAction;
    DropLinkEffect: Result := QtLinkAction;
    else            Result := QtIgnoreAction;
  end;
end;

function QtDropEventPointToLCLPoint(const PDropEventPoint: PQtPoint): TPoint;
begin
  if Assigned(PDropEventPoint) then
  begin
    if (PDropEventPoint^.x <> 0) or (PDropEventPoint^.y <> 0) then
    begin
      Result.X := PDropEventPoint^.x;
      Result.Y := PDropEventPoint^.y;
      Exit;
    end;
  end;

  GetCursorPos(Result);
end;

end.

