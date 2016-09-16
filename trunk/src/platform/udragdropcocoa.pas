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

    function RegisterEvents(DragBeginEvent  : uDragDropEx.TDragBeginEvent;
                            RequestDataEvent: uDragDropEx.TRequestDataEvent;
                            DragEndEvent    : uDragDropEx.TDragEndEvent): Boolean; override;

    function DoDragDrop(const FileNamesList: TStringList;
                        MouseButton: TMouseButton;
                        ScreenStartPoint: TPoint): Boolean; override;
  end;

implementation

uses
  CocoaAll, uMyDarwin;

{ ---------- TDragDropSourceCocoa ---------- }

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
  Window: NSWindow;
  DragIcon: NSImage;
  DragPoint: NSPoint;
  FileList: NSMutableArray;
  PasteBoard: NSPasteboard;
begin
  Result := False;

  // Simulate drag-begin event.
  if Assigned(GetDragBeginEvent) then
  begin
    Result := GetDragBeginEvent()();
    if Result = False then Exit;
  end;

  FileList:= NSMutableArray.arrayWithCapacity(FileNamesList.Count);
  for I:= 0 to FileNamesList.Count - 1 do
  begin
    FileList.addObject(StringToNSString(FileNamesList[I]));
  end;

  DragPoint.x:= ScreenStartPoint.X;
  DragPoint.y:= ScreenStartPoint.Y;
  Window:= NSApplication.sharedApplication.keyWindow;
  PasteBoard:= NSPasteboard.pasteboardWithName(NSDragPboard);
  PasteBoard.declareTypes_owner(NSArray.arrayWithObject(NSFileNamesPboardType), nil);
  PasteBoard.setPropertyList_forType(FileList, NSFileNamesPboardType);
  DragIcon:= NSWorkspace.sharedWorkspace.iconForFile(StringToNSString(FileNamesList[0]));
  Window.dragImage_at_offset_event_pasteboard_source_slideBack(DragIcon, DragPoint, NSZeroSize, nil, PasteBoard, Window, True);

  // Simulate drag-end event.
  if Assigned(GetDragEndEvent) then
  begin
    if Result = True then
      Result := GetDragEndEvent()()
    else
      GetDragEndEvent()()
  end;
end;

end.

