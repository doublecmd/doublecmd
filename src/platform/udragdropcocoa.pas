{
    Double Commander
    -------------------------------------------------------------------------
    Drag&Drop operations for Cocoa.

    Copyright (C) 2015 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uDragDropCocoa;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Controls, uDragDropEx
  {$IFDEF LCLCOCOA}, CocoaAll{$ENDIF};

type
  TDragDropSourceCocoa = class(TDragDropSource)
  private
    { Delegate of the drag session that is currently running, if any. }
    FActiveSource: Pointer;
    { Called by the drag session delegate once AppKit reports the session
      as finished. }
    procedure DragSessionEnded(Succeeded: Boolean);
  public
    destructor Destroy; override;

    function RegisterEvents(DragBeginEvent  : uDragDropEx.TDragBeginEvent;
                            RequestDataEvent: uDragDropEx.TRequestDataEvent;
                            DragEndEvent    : uDragDropEx.TDragEndEvent): Boolean; override;

    function DoDragDrop(const FileNamesList: TStringList;
                        MouseButton: TMouseButton;
                        ScreenStartPoint: TPoint): Boolean; override;
  end;

{$IFDEF LCLCOCOA}
  { Native dragging destination for a file panel.

    macOS was the only widgetset without a TDragDropTarget: CreateDragDropTarget
    fell through to the abstract dummy, so OnExDragEnter/Over/Drop/Leave never
    fired and incoming drops had to be served by the form level OnDropFiles
    workaround in fMain. This class closes that gap, which is also what lets the
    drag session be started while the cursor is still inside the panel. }
  TDragDropTargetCocoa = class(TDragDropTarget)
  private
    FView: NSObject;          // the NSView registered as the destination
    FEntered: Boolean;
    function ProposedEffect(sender: NSDraggingInfoProtocol): TDropEffect;
    function ScreenPoint(sender: NSDraggingInfoProtocol): TPoint;
    function FileNames(sender: NSDraggingInfoProtocol): TStringList;
  public
    destructor Destroy; override;

    function RegisterEvents(DragEnterEvent: uDragDropEx.TDragEnterEvent;
                            DragOverEvent : uDragDropEx.TDragOverEvent;
                            DropEvent     : uDragDropEx.TDropEvent;
                            DragLeaveEvent: uDragDropEx.TDragLeaveEvent): Boolean; override;
    procedure UnregisterEvents; override;

    { Called from the NSView category below. }
    function HandleEntered(sender: NSDraggingInfoProtocol): NSDragOperation;
    function HandleUpdated(sender: NSDraggingInfoProtocol): NSDragOperation;
    procedure HandleExited(sender: NSDraggingInfoProtocol);
    function HandlePerformDrop(sender: NSDraggingInfoProtocol): Boolean;
  end;
{$ENDIF}

implementation

uses
  {$IFNDEF LCLCOCOA}CocoaAll,{$ENDIF} uDarwinUtil, uDebug, uKeyboard, uGlobs
  {$IFDEF LCLCOCOA}, CocoaPrivate, CocoaUtils{$ENDIF};

const
  // Size of the drag image drawn for a dragged file.
  DragImageSize = 32;
  // Only the leading items get a drag image. NSWorkspace.iconForFile is not
  // called for the rest.
  MaxDragImageCount = 3;

function NSStrToStr(S: NSString): String;
begin
  if S = nil then Exit('<nil>');
  Result := String(S.UTF8String);
end;

type
  { TCocoaDragSource }

  { Bridges AppKit's NSDraggingSource callbacks back to the Pascal source.
    One instance is created per drag operation and releases itself once the
    drag session actually ends (draggingSession:endedAtPoint:operation:). }
  TCocoaDragSource = objcclass(NSObject, NSDraggingSourceProtocol)
  public
    Owner: TDragDropSourceCocoa;

    function draggingSession_sourceOperationMaskForDraggingContext(
      session: NSDraggingSession; context: NSDraggingContext): NSDragOperation;
      message 'draggingSession:sourceOperationMaskForDraggingContext:';

    procedure draggingSession_endedAtPoint_operation(
      session: NSDraggingSession; screenPoint: NSPoint; operation: NSDragOperation);
      message 'draggingSession:endedAtPoint:operation:';
  end;

{ ---------- Helpers ---------- }

{ -beginDraggingSessionWithItems:event:source: requires a mouse event.
  The application's current event is not necessarily one: an external drag can
  also be started from a key press (see TFileViewWithMainCtrl.MainControlKeyDown,
  where holding Command turns an internal drag into an external one). }
function MouseEventForDrag(View: NSView): NSEvent;
var
  Timestamp: NSTimeInterval = 0;
begin
  Result:= NSApplication.sharedApplication.currentEvent;

  if Assigned(Result) then
  begin
    case Result.type_ of
      NSLeftMouseDown,  NSLeftMouseDragged,
      NSRightMouseDown, NSRightMouseDragged,
      NSOtherMouseDown, NSOtherMouseDragged: Exit;
    end;
    Timestamp:= Result.timestamp;
  end;

  // Not a mouse event, synthesize one at the current mouse position instead.
  Result:= NSEvent.mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressure(
             NSLeftMouseDragged,
             View.window.mouseLocationOutsideOfEventStream,
             0,
             Timestamp,
             View.window.windowNumber,
             nil, 0, 1, 1.0);
end;

{ ---------- TCocoaDragSource ---------- }

function TCocoaDragSource.draggingSession_sourceOperationMaskForDraggingContext(
  session: NSDraggingSession; context: NSDraggingContext): NSDragOperation;
begin
  Result := NSDragOperationCopy or NSDragOperationMove or NSDragOperationLink;
end;

procedure TCocoaDragSource.draggingSession_endedAtPoint_operation(
  session: NSDraggingSession; screenPoint: NSPoint; operation: NSDragOperation);
begin
  // Report drag completion. This is where it happens now that the drag session
  // is asynchronous (unlike the old, blocking
  // dragImage:at:offset:event:pasteboard:source:slideBack: call).
  if Assigned(Owner) then
    Owner.DragSessionEnded(operation <> NSDragOperationNone);

  // Balance the .alloc.init done in TDragDropSourceCocoa.DoDragDrop -- this
  // instance's whole lifetime is exactly one drag operation.
  Self.release;
end;

{$IFDEF LCLCOCOA}
{ ---------- TDragDropTargetCocoa ---------- }

{ AppKit asks the view's class, not the object, whether it accepts a drag, so
  the destination methods have to live on a class. The panel's NSView is created
  by the widgetset, which offers no per-control drop target hook, so they are
  added to NSView through a category and dispatched to whichever target
  registered that particular view. A view that never called
  registerForDraggedTypes is never asked at all. }

type
  TTargetEntry = record
    View: NSObject;
    Target: TDragDropTargetCocoa;
  end;

var
  RegisteredTargets: array of TTargetEntry;

function TargetForView(AView: NSObject): TDragDropTargetCocoa;
var
  I: Integer;
begin
  for I := 0 to High(RegisteredTargets) do
    if RegisteredTargets[I].View = AView then
      Exit(RegisteredTargets[I].Target);
  Result := nil;
end;

procedure AddTarget(AView: NSObject; ATarget: TDragDropTargetCocoa);
var
  N: Integer;
begin
  N := Length(RegisteredTargets);
  SetLength(RegisteredTargets, N + 1);
  RegisteredTargets[N].View := AView;
  RegisteredTargets[N].Target := ATarget;
end;

procedure RemoveTarget(ATarget: TDragDropTargetCocoa);
var
  I, J: Integer;
begin
  for I := High(RegisteredTargets) downto 0 do
    if RegisteredTargets[I].Target = ATarget then
    begin
      for J := I to High(RegisteredTargets) - 1 do
        RegisteredTargets[J] := RegisteredTargets[J + 1];
      SetLength(RegisteredTargets, Length(RegisteredTargets) - 1);
    end;
end;

type
  DCDragDestination = objccategory(NSView)
    function draggingEntered(sender: NSDraggingInfoProtocol): NSDragOperation;
      reintroduce; message 'draggingEntered:';
    function draggingUpdated(sender: NSDraggingInfoProtocol): NSDragOperation;
      reintroduce; message 'draggingUpdated:';
    procedure draggingExited(sender: NSDraggingInfoProtocol);
      reintroduce; message 'draggingExited:';
    function performDragOperation(sender: NSDraggingInfoProtocol): ObjCBOOL;
      reintroduce; message 'performDragOperation:';
  end;

function DCDragDestination.draggingEntered(sender: NSDraggingInfoProtocol): NSDragOperation;
var
  Target: TDragDropTargetCocoa;
begin
  Target := TargetForView(Self);
  if Assigned(Target) then
    Result := Target.HandleEntered(sender)
  else
    Result := NSDragOperationNone;
end;

function DCDragDestination.draggingUpdated(sender: NSDraggingInfoProtocol): NSDragOperation;
var
  Target: TDragDropTargetCocoa;
begin
  Target := TargetForView(Self);
  if Assigned(Target) then
    Result := Target.HandleUpdated(sender)
  else
    Result := NSDragOperationNone;
end;

procedure DCDragDestination.draggingExited(sender: NSDraggingInfoProtocol);
var
  Target: TDragDropTargetCocoa;
begin
  Target := TargetForView(Self);
  if Assigned(Target) then Target.HandleExited(sender);
end;

function DCDragDestination.performDragOperation(sender: NSDraggingInfoProtocol): ObjCBOOL;
var
  Target: TDragDropTargetCocoa;
begin
  Target := TargetForView(Self);
  Result := Assigned(Target) and Target.HandlePerformDrop(sender);
end;

{ ---------- }

destructor TDragDropTargetCocoa.Destroy;
begin
  UnregisterEvents;
  inherited Destroy;
end;

function TDragDropTargetCocoa.RegisterEvents(
  DragEnterEvent: uDragDropEx.TDragEnterEvent;
  DragOverEvent : uDragDropEx.TDragOverEvent;
  DropEvent     : uDragDropEx.TDropEvent;
  DragLeaveEvent: uDragDropEx.TDragLeaveEvent): Boolean;
var
  AView: NSView;
  Types: NSMutableArray;
begin
  inherited;
  Result := False;

  GetControl.HandleNeeded;
  if not GetControl.HandleAllocated then Exit;

  // The handle is the scroll host wrapping the control; the view that is hit
  // tested, and therefore the one a drag is offered to, is its content view.
  AView := NSView(GetControl.Handle);
  if Assigned(AView.lclContentView) then AView := AView.lclContentView;

  Types := NSMutableArray.arrayWithCapacity(2);
  Types.addObject(NSSTR('public.file-url'));
  Types.addObject(NSFilenamesPboardType);
  AView.registerForDraggedTypes(Types);

  FView := AView;
  AddTarget(AView, Self);

  Result := True;
end;

procedure TDragDropTargetCocoa.UnregisterEvents;
begin
  if Assigned(FView) then
  begin
    NSView(FView).unregisterDraggedTypes;
    RemoveTarget(Self);
    FView := nil;
  end;
  inherited UnregisterEvents;
end;

function TDragDropTargetCocoa.ScreenPoint(sender: NSDraggingInfoProtocol): TPoint;
var
  P: NSPoint;
begin
  // draggingLocation is in window coordinates, while the handlers expect LCL
  // screen coordinates, whose origin is the top left of the global screen.
  P := NSView(FView).window.convertRectToScreen(
         NSMakeRect(sender.draggingLocation.x, sender.draggingLocation.y, 0, 0)).origin;
  Result := TCocoaScreenUtil.toLCL(P);
end;

function TDragDropTargetCocoa.ProposedEffect(sender: NSDraggingInfoProtocol): TDropEffect;
var
  Mask: NSDragOperation;
begin
  // The same expression the rest of the program uses to turn the held
  // modifiers into a drop effect, so that what happens to a dropped file does
  // not depend on which widgetset delivered it.
  Result := GetDropEffectByKeyAndMouse(GetKeyShiftStateEx, mbLeft, gDefaultDropEffect);

  // Only fall back when the source cannot perform what was asked for.
  Mask := sender.draggingSourceOperationMask;
  if Mask = NSDragOperationNone then Exit;
  case Result of
    DropCopyEffect: if (Mask and NSDragOperationCopy) = 0 then Result := DropMoveEffect;
    DropMoveEffect: if (Mask and NSDragOperationMove) = 0 then Result := DropCopyEffect;
    DropLinkEffect: if (Mask and NSDragOperationLink) = 0 then Result := DropCopyEffect;
  end;
end;

function EffectToOperation(AEffect: TDropEffect): NSDragOperation;
begin
  case AEffect of
    DropCopyEffect: Result := NSDragOperationCopy;
    DropMoveEffect: Result := NSDragOperationMove;
    DropLinkEffect: Result := NSDragOperationLink;
    DropAskEffect:  Result := NSDragOperationGeneric;
  else
    Result := NSDragOperationNone;
  end;
end;

function TDragDropTargetCocoa.FileNames(sender: NSDraggingInfoProtocol): TStringList;
var
  I: Integer;
  Pb: NSPasteboard;
  Objects: NSArray;
  Classes: NSArray;
  AClass: pobjc_class;
  Plist: NSArray;
begin
  Result := TStringList.Create;
  Pb := sender.draggingPasteboard;

  // Modern file URLs first, the legacy property list as a fallback.
  AClass := NSURL.classClass;
  Classes := NSArray.arrayWithObjects_count(@AClass, 1);
  Objects := Pb.readObjectsForClasses_options(Classes, nil);
  if Assigned(Objects) and (Objects.count > 0) then
  begin
    for I := 0 to Objects.count - 1 do
      Result.Add(NSStrToStr(NSURL(Objects.objectAtIndex(I)).path));
    Exit;
  end;

  Plist := NSArray(Pb.propertyListForType(NSFilenamesPboardType));
  if Assigned(Plist) then
    for I := 0 to Plist.count - 1 do
      Result.Add(NSStrToStr(NSString(Plist.objectAtIndex(I))));
end;

function TDragDropTargetCocoa.HandleEntered(sender: NSDraggingInfoProtocol): NSDragOperation;
var
  Effect: TDropEffect;
  Accepted: Boolean;
begin
  FEntered := True;
  Effect := ProposedEffect(sender);
  Accepted := True;
  if Assigned(GetDragEnterEvent) then
    Accepted := GetDragEnterEvent()(Effect, ScreenPoint(sender));
  if Accepted then
    Result := EffectToOperation(Effect)
  else
    Result := NSDragOperationNone;
end;

function TDragDropTargetCocoa.HandleUpdated(sender: NSDraggingInfoProtocol): NSDragOperation;
var
  Effect: TDropEffect;
  Accepted: Boolean;
begin
  if not FEntered then Exit(HandleEntered(sender));
  Effect := ProposedEffect(sender);
  Accepted := True;
  if Assigned(GetDragOverEvent) then
    Accepted := GetDragOverEvent()(Effect, ScreenPoint(sender));
  if Accepted then
    Result := EffectToOperation(Effect)
  else
    Result := NSDragOperationNone;
end;

procedure TDragDropTargetCocoa.HandleExited(sender: NSDraggingInfoProtocol);
begin
  FEntered := False;
  if Assigned(GetDragLeaveEvent) then GetDragLeaveEvent()();
end;

function TDragDropTargetCocoa.HandlePerformDrop(sender: NSDraggingInfoProtocol): Boolean;
var
  Names: TStringList;
  Effect: TDropEffect;
begin
  FEntered := False;
  Result := False;
  Effect := ProposedEffect(sender);
  Names := FileNames(sender);
  try
    if (Names.Count > 0) and Assigned(GetDropEvent) then
      Result := GetDropEvent()(Names, Effect, ScreenPoint(sender));
  finally
    Names.Free;
  end;
end;
{$ENDIF}

{ ---------- TDragDropSourceCocoa ---------- }

destructor TDragDropSourceCocoa.Destroy;
begin
  // The drag session outlives this object when the file view is destroyed
  // while a drag is still running. Detach the delegate, it must not call back
  // into a freed object; it still releases itself when the session ends.
  if Assigned(FActiveSource) then
    TCocoaDragSource(FActiveSource).Owner:= nil;

  inherited Destroy;
end;

procedure TDragDropSourceCocoa.DragSessionEnded(Succeeded: Boolean);
begin
  FActiveSource:= nil;
  uDragDropEx.ExternalDragSourceControl := nil;

  if Succeeded then
    FLastStatus:= DragDropSuccessful
  else
    FLastStatus:= DragDropAborted;

  // Simulate drag-end event.
  if Assigned(GetDragEndEvent) then GetDragEndEvent()();
end;

function TDragDropSourceCocoa.RegisterEvents(DragBeginEvent  : uDragDropEx.TDragBeginEvent;
                                             RequestDataEvent: uDragDropEx.TRequestDataEvent;
                                             DragEndEvent    : uDragDropEx.TDragEndEvent): Boolean;
begin
  inherited;

  // RequestDataEvent is not handled in Cocoa.

  Result := True;
end;

function TDragDropSourceCocoa.DoDragDrop(const FileNamesList: TStringList;
                                         MouseButton: TMouseButton;
                                         ScreenStartPoint: TPoint): Boolean;
var
  I: Integer;
  View: NSView;
  StartEvent: NSEvent;
  DragItem: NSDraggingItem;
  DragItems: NSMutableArray;
  ItemURL: NSUrl;
  ItemIcon: NSImage;
  ItemFrame: NSRect;
  Source: TCocoaDragSource;
begin
  Result := False;

  // Simulate drag-begin event.
  if Assigned(GetDragBeginEvent) then
  begin
    Result := GetDragBeginEvent()();
    if Result = False then Exit;
  end;

  View:= NSView(GetControl.Handle);
  if View = nil then Exit;

  {$IFDEF LCLCOCOA}
  { TWinControl.Handle is the scroll host wrapping the control, not the view
    that is hit tested and receives the mouse. Starting the session on the
    wrapper makes Finder fail to read the dragged file (invalidPathErr, shown
    as error -8060) on any operation other than the move it picks by default. }
  if Assigned(View.lclContentView) then
    View := View.lclContentView;
  {$ENDIF}

  // One fixed frame, shared by all items.
  ItemFrame:= NSMakeRect(0, 0, DragImageSize, DragImageSize);

  // Build one NSDraggingItem per file, each backed by its own file URL
  // pasteboard writer. This -- instead of a single item carrying all paths
  // via the legacy NSFilenamesPboardType property list -- is what makes
  // modern apps that enumerate per-item pasteboard entries (e.g. WhatsApp)
  // see every dragged file, not just one.
  DragItems:= NSMutableArray.arrayWithCapacity(FileNamesList.Count);
  for I:= 0 to FileNamesList.Count - 1 do
  begin
    ItemURL:= NSUrl.fileURLWithPath(StringToNSString(FileNamesList[I]));
    // NSURL conforms to NSPasteboardWriting at the Objective-C runtime level
    // (via an AppKit category), but this binding's NSURL class declaration
    // doesn't list that protocol, so the cast has to be made explicit.
    DragItem:= NSDraggingItem.alloc.initWithPasteboardWriter(NSPasteboardWritingProtocol(ItemURL));

    if I < MaxDragImageCount then
    begin
      ItemIcon:= NSWorkspace.sharedWorkspace.iconForFile(StringToNSString(FileNamesList[I]));
      DragItem.setDraggingFrame_contents(ItemFrame, ItemIcon);
    end
    else
      // Not setDraggingFrame: -- it does not give the same result here.
      DragItem.setDraggingFrame_contents(ItemFrame, nil);

    DragItems.addObject(DragItem);
    DragItem.release;
  end;

  StartEvent:= MouseEventForDrag(View);
  if StartEvent = nil then Exit;

  // A previous session, if any, must not report back into this object anymore.
  if Assigned(FActiveSource) then
    TCocoaDragSource(FActiveSource).Owner:= nil;

  Source:= TCocoaDragSource.alloc.init;
  Source.Owner:= Self;
  FActiveSource:= Source;

  Result:= View.beginDraggingSessionWithItems_event_source(DragItems, StartEvent, Source) <> nil;

  // The Cocoa session serves drops inside this application as well, so the
  // drop target has to be able to recognise a drag that started here.
  if Result then
    uDragDropEx.ExternalDragSourceControl := GetControl;

  // Note: the drag session started above is asynchronous -- it returns
  // immediately, before the user has dropped or cancelled anything.
  // GetDragEndEvent() is no longer called here; it now fires later, from
  // TCocoaDragSource.draggingSession_endedAtPoint_operation, once AppKit
  // reports the session as actually finished.
end;

end.
