{
    Double Commander
    -------------------------------------------------------------------------
    Interface unit for Drag&Drop to external applications.

    Copyright (C) 2009  Koblov Alexander (Alexx2000@mail.ru)

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

{en
   Be aware that raw HWND handles are used to register controls for drag&drop
   in the system. Some LCL's functions may destroy a control's handle and create
   a new one during the lifetime of that control, making drag&drop invalid.
   Override TWinControl.InitializeWnd and TWinControl.FinalizeWnd to handle
   registration/unregistration in each control.
}
unit uDragDropEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type
  TDropEffect = (DropNoEffect, DropCopyEffect, DropMoveEffect, DropLinkEffect, DropAskEffect);
  TDragDropStatus = (DragDropAborted, DragDropSuccessful, DragDropError);

{ Source events }

  { Dragging has started }
  TDragBeginEvent  = function:Boolean of object;

  { Drag destination has requested data }
  TRequestDataEvent = function(
      // This is the same as given to DoDragDrop.
      const FileNamesList: TStringList;
      // MIME-type format in which target requested data, e.g. text/plain.
      MimeType: string;
      // Effect chosen by target (may not be final).
      DropEffect: TDropEffect):string of object;

  { Dragging has ended }
  TDragEndEvent    = function:Boolean of object;

{ Target events }

  { Mouse entered into the control when dragging something }
  TDragEnterEvent = function(
      // Proposed drop effect by the source (can be changed by the target to inform the source).
      var DropEffect: TDropEffect;
      // Screen coordinates of mouse cursor.
      ScreenPoint: TPoint):Boolean of object;

  { Mouse moved inside the control when dragging something }
  TDragOverEvent  = function(
      // Proposed drop effect by the source (can be changed by the target to inform the source).
      var DropEffect: TDropEffect;
      // Screen coordinates of mouse cursor.
      ScreenPoint: TPoint):Boolean of object;

  { Mouse button has been lifted causing a drop event }
  TDropEvent      = function(
      // List of filenames given by the source.
      const FileNamesList: TStringList;
      // Drop effect chosen by the source.
      DropEffect: TDropEffect;
      // Screen coordinates of mouse cursor.
      ScreenPoint: TPoint):Boolean of object;

  { Mouse has left the control when dragging something }
  TDragLeaveEvent = function:Boolean of object;

  { Base class for external source }
  TDragDropSource = class(TObject)
  public
    constructor Create(SourceControl: TWinControl); virtual;
    destructor  Destroy; override;

    function  RegisterEvents(DragBeginEvent  : uDragDropEx.TDragBeginEvent;
                             RequestDataEvent: uDragDropEx.TRequestDataEvent;
                             DragEndEvent    : uDragDropEx.TDragEndEvent): Boolean; virtual;

    procedure UnregisterEvents; virtual;

    function DoDragDrop(const FileNamesList: TStringList;
                        MouseButton: TMouseButton; // button that initiated dragging
                        ScreenStartPoint: TPoint   // mouse position in screen coords
                       ): Boolean; virtual;

    function GetLastStatus: TDragDropStatus;
    function GetFileNamesList: TStringList;

    function GetDragBeginEvent  : TDragBeginEvent;
    function GetRequestDataEvent: TRequestDataEvent;
    function GetDragEndEvent    : TDragEndEvent;

  private
    FDragDropControl: TWinControl;

    FDragBeginEvent   : TDragBeginEvent;
    FRequestDataEvent : TRequestDataEvent;
    FDragEndEvent     : TDragEndEvent;

  protected
    FLastStatus: TDragDropStatus;
    FFileNamesList: TStringList;

    function GetControl: TWinControl;
  end;

  { Base class for external target }
  TDragDropTarget = class(TObject)
  public
    constructor Create(TargetControl: TWinControl); virtual;
    destructor  Destroy; override;

    function  RegisterEvents(DragEnterEvent: uDragDropEx.TDragEnterEvent;
                             DragOverEvent : uDragDropEx.TDragOverEvent;
                             DropEvent     : uDragDropEx.TDropEvent;
                             DragLeaveEvent: uDragDropEx.TDragLeaveEvent): Boolean; virtual;

    procedure UnregisterEvents; virtual;

    function GetDragEnterEvent: TDragEnterEvent;
    function GetDragOverEvent : TDragOverEvent;
    function GetDropEvent     : TDropEvent;
    function GetDragLeaveEvent: TDragLeaveEvent;

  private
    FDragDropControl: TWinControl;

    FDragEnterEvent: TDragEnterEvent;
    FDragOverEvent : TDragOverEvent;
    FDropEvent     : TDropEvent;
    FDragLeaveEvent: TDragLeaveEvent;

  protected
    function GetControl: TWinControl;
  end;

  { These functions return system-appropriate DragDrop... object. }
  function CreateDragDropSource(Control: TWinControl): TDragDropSource;
  function CreateDragDropTarget(Control: TWinControl): TDragDropTarget;

  { Returns True if external dragging is supported based
    on operating system and LCLWidgetType (compile-time) }
  function IsExternalDraggingSupported: Boolean;

  { Analyzes keyboard modifier keys (Shift, Ctrl, etc.) and mouse button nr
    and returns the appropriate drop effect. }
  function GetDropEffectByKeyAndMouse(ShiftState: TShiftState;
                                      MouseButton: TMouseButton): TDropEffect;

var
  { If set to True, then dragging is being transformed: internal to external or vice-versa. }
  TransformDragging : Boolean = False;

  { If set to True, then transforming from external back to internal dragging is enabled. }
  AllowTransformToInternal : Boolean = True;

implementation

{$IF DEFINED(MSWINDOWS)}
uses 
  uOleDragDrop;
{$ELSEIF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
uses
  uDragDropGtk;
{$ELSEIF DEFINED(LCLQT)}
uses
  uDragDropQt;
{$ENDIF}


{ ---------- TDragDropSource ---------- }

constructor TDragDropSource.Create(SourceControl: TWinControl);
begin
  FDragDropControl := SourceControl;

  FDragBeginEvent   := nil;
  FRequestDataEvent := nil;
  FDragEndEvent     := nil;

  FFileNamesList := TStringList.Create;
  FLastStatus := DragDropSuccessful;
end;

destructor TDragDropSource.Destroy;
begin
  UnregisterEvents;

  FDragDropControl := nil;

  if Assigned(FFileNamesList) then
    FreeAndNil(FFileNamesList);
end;

function TDragDropSource.GetControl:TWinControl;
begin
  Result := FDragDropControl;
end;

function TDragDropSource.GetFileNamesList: TStringList;
begin
  Result := FFileNamesList;
end;

function TDragDropSource.GetLastStatus: TDragDropStatus;
begin
  Result := FLastStatus;
end;

function TDragDropSource.GetDragBeginEvent: TDragBeginEvent;
begin
  Result := FDragBeginEvent;
end;

function TDragDropSource.GetRequestDataEvent: TRequestDataEvent;
begin
  Result := FRequestDataEvent;
end;

function TDragDropSource.GetDragEndEvent: TDragEndEvent;
begin
  Result := FDragEndEvent;
end;

function TDragDropSource.RegisterEvents(DragBeginEvent  : uDragDropEx.TDragBeginEvent;
                                        RequestDataEvent: uDragDropEx.TRequestDataEvent;
                                        DragEndEvent    : uDragDropEx.TDragEndEvent): Boolean;
begin
  FDragBeginEvent   := DragBeginEvent;
  FRequestDataEvent := RequestDataEvent;
  FDragEndEvent     := DragEndEvent;

  Result := False;
end;

procedure TDragDropSource.UnregisterEvents;
begin
  FDragBeginEvent   := nil;
  FRequestDataEvent := nil;
  FDragEndEvent     := nil;
end;

function TDragDropSource.DoDragDrop(const FileNamesList: TStringList;
                                    MouseButton: TMouseButton;
                                    ScreenStartPoint: TPoint): Boolean;
begin
  FLastStatus := DragDropError;
  Result := False;
end;


{ ---------- TDragDropTarget ---------- }

constructor TDragDropTarget.Create(TargetControl: TWinControl);
begin
  FDragDropControl := TargetControl;

  FDragEnterEvent := nil;
  FDragOverEvent  := nil;
  FDropEvent      := nil;
  FDragLeaveEvent := nil;
end;

destructor TDragDropTarget.Destroy;
begin
  UnregisterEvents;

  FDragDropControl := nil;
end;

function TDragDropTarget.GetControl:TWinControl;
begin
  Result := FDragDropControl;
end;

function TDragDropTarget.GetDragEnterEvent: TDragEnterEvent;
begin
  Result := FDragEnterEvent;
end;

function TDragDropTarget.GetDragOverEvent: TDragOverEvent;
begin
  Result := FDragOverEvent;
end;

function TDragDropTarget.GetDropEvent: TDropEvent;
begin
  Result := FDropEvent;
end;

function TDragDropTarget.GetDragLeaveEvent: TDragLeaveEvent;
begin
  Result := FDragLeaveEvent;
end;

function TDragDropTarget.RegisterEvents(DragEnterEvent: uDragDropEx.TDragEnterEvent;
                                        DragOverEvent : uDragDropEx.TDragOverEvent;
                                        DropEvent     : uDragDropEx.TDropEvent;
                                        DragLeaveEvent: uDragDropEx.TDragLeaveEvent): Boolean;
begin
  FDragEnterEvent := DragEnterEvent;
  FDragOverEvent  := DragOverEvent;
  FDropEvent      := DropEvent;
  FDragLeaveEvent := DragLeaveEvent;

  Result := False;
end;

procedure TDragDropTarget.UnregisterEvents;
begin
  FDragEnterEvent := nil;
  FDragOverEvent  := nil;
  FDropEvent      := nil;
  FDragLeaveEvent := nil;
end;

{ --------------------------------------------------------------------------- }

function IsExternalDraggingSupported: Boolean;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := True;
{$ELSEIF DEFINED(LCLGTK) OR DEFINED(LCLGTK2)}
  Result := True;
{$ELSEIF DEFINED(LCLQT)}
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function CreateDragDropSource(Control: TWinControl): TDragDropSource;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := TDragDropSourceWindows.Create(Control);
{$ELSEIF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  Result := TDragDropSourceGTK.Create(Control);
{$ELSEIF DEFINED(LCLQT)}
  Result := TDragDropSourceQT.Create(Control);
{$ELSE}
  Result := TDragDropSource.Create(Control);   // Dummy
{$ENDIF}
end;

function CreateDragDropTarget(Control: TWinControl): TDragDropTarget;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := TDragDropTargetWindows.Create(Control);
{$ELSEIF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  Result := TDragDropTargetGTK.Create(Control);
{$ELSEIF DEFINED(LCLQT)}
  Result := TDragDropTargetQT.Create(Control);
{$ELSE}
  Result := TDragDropTarget.Create(Control);   // Dummy
{$ENDIF}
end;

function GetDropEffectByKeyAndMouse(ShiftState: TShiftState;
                                    MouseButton: TMouseButton): TDropEffect;
begin
  case MouseButton of
    mbLeft:
      begin
        if ShiftState = [] then
          Result := DropCopyEffect   // default to Copy when no keys pressed
        else if ShiftState = [ssShift] then
          Result := DropMoveEffect
        else if ShiftState = [ssCtrl] then
          Result := DropCopyEffect
        else if ShiftState = [ssCtrl, ssShift] then
          Result := DropLinkEffect
        else
          Result := DropNoEffect;    // some other key combination pressed
      end;

    mbMiddle:
      Result := DropAskEffect;

    mbRight:
      Result := DropAskEffect;
  end;
end;

end.

