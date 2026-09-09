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
  Classes, SysUtils, Controls, uDragDropEx;

type
  TDragDropSourceCocoa = class(TDragDropSource)
  private
    { Called by the drag session delegate once AppKit reports the session
      as finished. }
    procedure DragSessionEnded(Succeeded: Boolean);
  public
    function RegisterEvents(DragBeginEvent  : uDragDropEx.TDragBeginEvent;
                            RequestDataEvent: uDragDropEx.TRequestDataEvent;
                            DragEndEvent    : uDragDropEx.TDragEndEvent): Boolean; override;

    function DoDragDrop(const FileNamesList: TStringList;
                        MouseButton: TMouseButton;
                        ScreenStartPoint: TPoint): Boolean; override;
  end;

implementation

uses
  CocoaAll, uDarwinUtil;

const
  // Size of the drag image drawn for a dragged file.
  DragImageSize = 32;
  // AppKit only ever draws a small stack of images plus the item count badge,
  // so only the leading items need an image. Asking NSWorkspace for an icon of
  // every single dragged file would just burn LaunchServices lookups when a
  // large selection is dragged.
  MaxDragImageCount = 3;

type
  { TCocoaDragSource }

  { Bridges AppKit's NSDraggingSource callbacks back to a Pascal closure.
    One instance is created per drag operation and released once the
    drag session actually ends (draggingSession:endedAt:operation:). }
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

{ ---------- TDragDropSourceCocoa ---------- }

procedure TDragDropSourceCocoa.DragSessionEnded(Succeeded: Boolean);
begin
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

  // AppKit places the drag images relative to the event that starts the
  // session, so a fixed frame is enough here and all items may share it.
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
      DragItem.setDraggingFrame(ItemFrame);

    DragItems.addObject(DragItem);
    DragItem.release;
  end;

  StartEvent:= MouseEventForDrag(View);
  if StartEvent = nil then Exit;

  Source:= TCocoaDragSource.alloc.init;
  Source.Owner:= Self;

  Result:= View.beginDraggingSessionWithItems_event_source(DragItems, StartEvent, Source) <> nil;

  // Note: the drag session started above is asynchronous -- it returns
  // immediately, before the user has dropped or cancelled anything.
  // GetDragEndEvent() is no longer called here; it now fires later, from
  // TCocoaDragSource.draggingSession_endedAtPoint_operation, once AppKit
  // reports the session as actually finished.
end;

end.
