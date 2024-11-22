{
    Double Commander
    -------------------------------------------------------------------------
    Panel displaying file operations.

    Copyright (C) 2012  Przemysław Nagay (cobines@gmail.com)

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

unit uOperationsPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, LCLVersion,
  fFileOpDlg,
  uFileSourceOperation, uOperationsManager, uFileSourceOperationUI;

type

  { TOperationsPanel }

  TOperationsPanel = class(TScrollBox)
  private
    FMaximumItemWidth: Integer;
    FUserInterface: TFileSourceOperationUI;
    FOperations, FQueues: TFPList;
    FParentWidth: Integer;
    procedure ClearItems;
    procedure DeleteItem(List: TFPList; Index: Integer);
    procedure GetStateColor(State: TFileSourceOperationState; out ColorFrom, ColorTo: TColor);
    procedure OperationsManagerEvent(Item: TOperationsManagerItem; Event: TOperationManagerEvent);
    procedure ProgressWindowEvent(OperationHandle: TOperationHandle;
                                  Event: TOperationProgressWindowEvent);
    procedure UpdateItems;
    procedure UpdateVisibility;
{$if lcl_fullversion >= 1070000}
  protected
    procedure SetParent(NewParent: TWinControl); override;
{$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure ParentResized(Sender: TObject);
    procedure UpdateView;
  end;

implementation

uses
  LCLIntf, LCLType, Math,
  fViewOperations,
  uDCUtils,
  uFileSourceOperationMisc,
  uFileSourceOperationMessageBoxesUI;

const
  MinimumHeight = 25;
  MaximumItemWidth = 150;
  LeftRightTextMargin = 4;
  TopBottomTextMargin = 2;
  HorizontalSpaceBetween = 1;
  PanelBorderWidth = 1;

type
  TOperationPanelItem = record
    Width: Integer;
    OperationHandle: TOperationHandle;
    QueueId: TOperationsManagerQueueIdentifier;
  end;
  POperationPanelItem = ^TOperationPanelItem;

{ TOperationsPanel }

procedure TOperationsPanel.ParentResized(Sender: TObject);
begin
  FParentWidth := (Sender as TControl).Width;
  UpdateItems;
end;

procedure TOperationsPanel.ClearItems;
var
  p: Pointer;
begin
  for p in FOperations do
    Dispose(POperationPanelItem(p));
  for p in FQueues do
    Dispose(POperationPanelItem(p));
  FOperations.Clear;
  FQueues.Clear;
end;

procedure TOperationsPanel.DeleteItem(List: TFPList; Index: Integer);
begin
  Dispose(POperationPanelItem(List[Index]));
  List.Delete(Index);
end;

procedure TOperationsPanel.GetStateColor(State: TFileSourceOperationState; out ColorFrom, ColorTo: TColor);
begin
  case State of
    // Green if running
    fsosRunning:
      begin
        ColorFrom:= RGB(203, 233, 171);
        ColorTo:=  RGB(146, 208, 80);
      end;
    // Orange if in waiting
    fsosWaitingForFeedback, fsosWaitingForConnection:
      begin
        ColorFrom:= RGB(255, 202, 100);
        ColorTo:=  RGB(255, 153, 4);
      end;
    // Red if paused, stopped
    fsosPaused, fsosStopped:
      begin
        ColorFrom:= RGB(255, 153, 149);
        ColorTo:=  RGB(255, 110, 103);
      end;
    else
      begin
        ColorFrom:= RGB(0, 0, 0);
        ColorTo:=  RGB(255, 255, 255);
      end;
  end;
end;

procedure TOperationsPanel.OperationsManagerEvent(Item: TOperationsManagerItem; Event: TOperationManagerEvent);
begin
  UpdateItems;
  UpdateView;
  if Event = omevOperationAdded then
    Item.Operation.AddUserInterface(FUserInterface)
  else if Event = omevOperationRemoved then begin
    Item.Operation.RemoveUserInterface(FUserInterface);
  end;
end;

procedure TOperationsPanel.ProgressWindowEvent(OperationHandle: TOperationHandle; Event: TOperationProgressWindowEvent);
begin
  UpdateVisibility;
end;

procedure TOperationsPanel.UpdateItems;
var
  OpManItem: TOperationsManagerItem;
  QueueIndex, OperIndex: Integer;
  OutString: String;
  ItemRect: TRect;
  Queue: TOperationsManagerQueue;
  OperationItem: POperationPanelItem;
  OverallHeight: Integer = MinimumHeight;
  OverallWidth: Integer = 0;
  Visibility: Boolean = False;

  procedure SetSize;
  begin
    ItemRect := Rect(0, 0, 0, 0);
    DrawText(Canvas.Handle, PChar(OutString), Length(OutString), ItemRect,
      DT_NOPREFIX or DT_CALCRECT);

    OperationItem^.Width  :=
      Min(ItemRect.Right  + (LeftRightTextMargin + PanelBorderWidth) * 2, FMaximumItemWidth);
    OverallHeight :=
      Max(ItemRect.Bottom + (TopBottomTextMargin + PanelBorderWidth) * 2, OverallHeight);
    OverallWidth := OverallWidth + OperationItem^.Width + HorizontalSpaceBetween;
  end;
begin
  ClearItems;
  for QueueIndex := 0 to OperationsManager.QueuesCount - 1 do
  begin
    Queue := OperationsManager.QueueByIndex[QueueIndex];
    if Queue.Count > 0 then
    begin
      if Queue.Identifier = FreeOperationsQueueId then
      begin
        for OperIndex := 0 to Queue.Count - 1 do
        begin
          OpManItem := Queue.Items[OperIndex];
          if Assigned(OpManItem) and OpManItem.Visible then
          begin
            New(OperationItem);
            FOperations.Add(OperationItem);
            OperationItem^.QueueId := Queue.Identifier;
            OperationItem^.OperationHandle := OpManItem.Handle;

            OutString := IntToStr(OpManItem.Handle) + ': ' +
              OpManItem.Operation.GetDescription(fsoddJob) + ' - ' + GetProgressString(100);
            SetSize;

            if not TfrmFileOp.IsOpenedFor(OpManItem.Handle) and
               not (OpManItem.Operation.State in [fsosStopping, fsosStopped]) then
              Visibility := True;
          end;
        end;
      end
      else
      begin
        New(OperationItem);
        FQueues.Add(OperationItem);
        OperationItem^.QueueId := Queue.Identifier;
        OperationItem^.OperationHandle := InvalidOperationHandle;

        OutString := Queue.GetDescription(True) + LineEnding +
                     Queue.Items[0].Operation.GetDescription(fsoddJob) + ' - ' +
                     GetProgressString(100);
        SetSize;

        if not TfrmFileOp.IsOpenedFor(Queue.Identifier) then
          Visibility := True;
      end;
    end;
  end;

  ClientHeight := OverallHeight + 2;
  ClientWidth := Max(OverallWidth - HorizontalSpaceBetween, FParentWidth);
  Visible := Visibility;
end;

procedure TOperationsPanel.UpdateVisibility;
var
  OpManItem: TOperationsManagerItem;
  QueueIndex, OperIndex: Integer;
  Queue: TOperationsManagerQueue;
  Visibility: Boolean = False;
begin
  for QueueIndex := 0 to OperationsManager.QueuesCount - 1 do
  begin
    Queue := OperationsManager.QueueByIndex[QueueIndex];
    if Queue.Count > 0 then
    begin
      if Queue.Identifier = FreeOperationsQueueId then
      begin
        for OperIndex := 0 to Queue.Count - 1 do
        begin
          OpManItem := Queue.Items[OperIndex];
          if Assigned(OpManItem) and OpManItem.Visible then
          begin
            if not TfrmFileOp.IsOpenedFor(OpManItem.Handle) and
               not (OpManItem.Operation.State in [fsosStopping, fsosStopped]) then
              Visibility := True;
          end;
        end;
      end
      else
      begin
        if not TfrmFileOp.IsOpenedFor(Queue.Identifier) then
          Visibility := True;
      end;
    end;
  end;
  Visible := Visibility;
end;

{$if lcl_fullversion >= 1070000}
procedure TOperationsPanel.SetParent(NewParent: TWinControl);
var
  AForm: TCustomForm;
begin
  inherited SetParent(NewParent);
  AForm := GetParentForm(NewParent);
  if Assigned(AForm) then begin
    FMaximumItemWidth := ScaleX(MaximumItemWidth, AForm.DesignTimePPI);
  end;
end;
{$endif}

constructor TOperationsPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOperations := TFPList.Create;
  FQueues := TFPList.Create;
  FMaximumItemWidth := MaximumItemWidth;
  FUserInterface := TFileSourceOperationMessageBoxesUI.Create;

  OperationsManager.AddEventsListener(
    [omevOperationAdded, omevOperationRemoved, omevOperationMoved],
    @OperationsManagerEvent);
  TfrmFileOp.AddEventsListener([opwevOpened, opwevClosed],
    @ProgressWindowEvent);
end;

destructor TOperationsPanel.Destroy;
begin
  OperationsManager.RemoveEventsListener(
    [omevOperationAdded, omevOperationRemoved, omevOperationMoved],
    @OperationsManagerEvent);
  TfrmFileOp.RemoveEventsListener([opwevOpened, opwevClosed],
    @ProgressWindowEvent);

  inherited Destroy;
  FUserInterface.Free;
  FOperations.Free;
  FQueues.Free;
end;

procedure TOperationsPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ClickPos: TPoint;
  OpManItem: TOperationsManagerItem;

  procedure HandleItem(Item: POperationPanelItem);
  var
    Queue: TOperationsManagerQueue;
  begin
    Queue := OperationsManager.QueueByIdentifier[Item^.QueueId];
    if Assigned(Queue) and (Queue.Count > 0) then
    begin
      if Item^.OperationHandle = InvalidOperationHandle then
      begin
        case Button of
          mbLeft:
            TfrmFileOp.ShowFor(Item^.QueueId, [opwoIfExistsBringToFront]);
          mbMiddle:
            Queue.TogglePause;
          mbRight:
            ShowOperationsViewer(Item^.QueueId);
        end;
      end
      else
      begin
        OpManItem := Queue.ItemByHandle[Item^.OperationHandle];
        if Assigned(OpManItem) then
        begin
          case Button of
            mbLeft:
              TfrmFileOp.ShowFor(OpManItem.Handle, [opwoIfExistsBringToFront]);
            mbMiddle:
              OpManItem.Operation.TogglePause;
            mbRight:
              ShowOperationsViewer(OpManItem.Handle);
          end;
        end;
      end;
    end;
  end;
var
  ItemRect: TRect;
  Item: POperationPanelItem;
begin
  inherited MouseDown(Button, Shift, X, Y);

  ClickPos := Point(X, Y);

  ItemRect := ClientRect;
  InflateRect(ItemRect, -PanelBorderWidth, -PanelBorderWidth);
  ItemRect.Right := ItemRect.Left - HorizontalSpaceBetween;

  for Item in FQueues do
  begin
    ItemRect.Left  := ItemRect.Right + HorizontalSpaceBetween;
    ItemRect.Right := ItemRect.Left + Item^.Width;
    if PtInRect(ItemRect, ClickPos) then
    begin
      HandleItem(Item);
      Exit;
    end;
  end;

  for Item in FOperations do
  begin
    ItemRect.Left  := ItemRect.Right + HorizontalSpaceBetween;
    ItemRect.Right := ItemRect.Left + Item^.Width;
    if PtInRect(ItemRect, ClickPos) then
    begin
      HandleItem(Item);
      Exit;
    end;
  end;
end;

procedure TOperationsPanel.Paint;
var
  OpManItem: TOperationsManagerItem;
  ARect, ItemRect: TRect;
  ColorFrom, ColorTo: TColor;
  Queue: TOperationsManagerQueue;
  Item: POperationPanelItem;
  i: Integer;
  AProgress: Double;

  procedure DrawString(s: String);
  begin
    // Draw output string
    Canvas.Brush.Style := bsClear;
    ARect := ItemRect;
    InflateRect(ARect, -4, -2);
    DrawText(Canvas.Handle, PChar(s), Length(s), ARect, DT_LEFT or DT_VCENTER or DT_NOPREFIX);
  end;
  procedure DrawProgress(State: TFileSourceOperationState; Progress: Double);
  begin
    // Draw progress bar
    GetStateColor(State, ColorFrom, ColorTo);
    ARect := ItemRect;
    InflateRect(ARect, -1, -1);
    ARect.Right := ARect.Left + Round((ARect.Right - ARect.Left) * Progress);
    Canvas.GradientFill(ARect, ColorFrom, ColorTo, gdVertical);
    // Special indication if operation is paused/stopped
    if State in [fsosPaused, fsosStopped, fsosWaitingForFeedback] then
    begin
      Canvas.Brush.Color:= ColorFrom;
      Canvas.Brush.Style:= bsDiagCross;
      ARect.Left:= ARect.Right + 1;
      ARect.Right:= ItemRect.Right - 1;
      Canvas.FillRect(ARect);
    end;
  end;
begin
  inherited Paint;

  ItemRect := ClientRect;

  Canvas.Pen.Color:= cl3DDkShadow;
  Canvas.Rectangle(ItemRect);

  InflateRect(ItemRect, -PanelBorderWidth, -PanelBorderWidth);
  Canvas.GradientFill(ItemRect, LightColor(clBtnHiLight, 20), clBtnFace, gdVertical);

  ItemRect.Right := ItemRect.Left - HorizontalSpaceBetween;

  i := 0;
  while i < FQueues.Count do
  begin
    Item := FQueues[i];
    Queue := OperationsManager.QueueByIdentifier[Item^.QueueId];
    if Assigned(Queue) and (Queue.Count > 0) then
    begin
      OpManItem := Queue.Items[0];
      if Assigned(OpManItem) then
      begin
        ItemRect.Left  := ItemRect.Right + HorizontalSpaceBetween;
        ItemRect.Right := ItemRect.Left + Item^.Width;

        // Draw border
        Canvas.Pen.Color := LightColor(cl3DDkShadow, 25);
        Canvas.Pen.Style := psSolid;
        Canvas.Rectangle(ItemRect);

        AProgress := OpManItem.Operation.Progress;
        DrawProgress(OpManItem.Operation.State, AProgress);
        DrawString(Queue.GetDescription(True) + LineEnding +
                   OpManItem.Operation.GetDescription(fsoddJob) + ' - ' +
                   GetProgressString(AProgress));
        Inc(i);
      end
      else
        DeleteItem(FQueues, i);
    end
    else
      DeleteItem(FQueues, i);
  end;

  i := 0;
  while i < FOperations.Count do
  begin
    Item := FOperations[i];
    Queue := OperationsManager.QueueByIdentifier[Item^.QueueId];
    if Assigned(Queue) and (Queue.Count > 0) then
    begin
      OpManItem := Queue.ItemByHandle[Item^.OperationHandle];
      if Assigned(OpManItem) then
      begin
        ItemRect.Left  := ItemRect.Right + HorizontalSpaceBetween;
        ItemRect.Right := ItemRect.Left + Item^.Width;

        if TfrmFileOp.IsOpenedFor(OpManItem.Handle) then
          Canvas.Pen.Color := clMenuHighlight
        else
          Canvas.Pen.Color := LightColor(cl3DDkShadow, 40);

        // Draw border
        Canvas.Pen.Style := psSolid;
        Canvas.Rectangle(ItemRect);

        AProgress := OpManItem.Operation.Progress;
        DrawProgress(OpManItem.Operation.State, AProgress);
        DrawString(IntToStr(OpManItem.Handle) + ': ' +
                   OpManItem.Operation.GetDescription(fsoddJob) + ' - ' +
                   GetProgressString(AProgress));
        Inc(i);
      end
      else
        DeleteItem(FOperations, i);
    end
    else
      DeleteItem(FOperations, i);
  end;
end;

procedure TOperationsPanel.UpdateView;
begin
  Invalidate;
end;

end.

