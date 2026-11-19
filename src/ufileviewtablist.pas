{
   Double Commander
   -------------------------------------------------------------------------
   Vertical list of folder tabs shown beside TFileViewNotebook
   (used when tabs position is "Left" or "Right").

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uFileViewTabList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LCLType, LCLIntf, LMessages, Controls, ExtCtrls,
  Graphics, Forms, uFileViewNotebook;

type

  { TFileViewTabList }

  TFileViewTabList = class(TCustomControl)
  private
    FNotebook: TFileViewNotebook;
    FSplitter: TSplitter;
    FScrollTimer: TTimer;
    FScrollDelta: Integer;
    FScrollBarVisible: Boolean;
    FScrollBarPending: Boolean;
    FUpdatingScrollBar: Boolean;
    FItemHeight: Integer;
    FTopIndex: Integer;
    FHotIndex: Integer;
    FHotClose: Boolean;
    FDownIndex: Integer;
    FCloseDown: Boolean;
    FStartDrag: Boolean;
    FDraggedIndex: Integer;
    FDropIndex: Integer;
    FInsertIndex: Integer;
    FDragSource: TObject;
    FActivePanel: Boolean;
    FLastPageIndex: Integer;
    FLastPageCount: Integer;
    FOnWidthChanged: TNotifyEvent;

    function CalcItemHeight: Integer;
    function GetItemHeight: Integer;
    function GetItemsWidth: Integer;
    function GetVisibleCount: Integer;
    function GetItemRect(Index: Integer): TRect;
    function GetCloseRect(Index: Integer): TRect;
    function GetTabCaption(Index: Integer): String;
    function HasCloseButton: Boolean;
    function IsOnCloseButton(Index, X, Y: Integer): Boolean;
    procedure SetActivePanel(AValue: Boolean);
    procedure SetTopIndex(AValue: Integer);
    procedure SetHot(AIndex: Integer; ACloseHot: Boolean);
    procedure SetDropIndex(AValue: Integer);
    procedure SetInsertIndex(AValue: Integer);
    {en
       Returns the position where a tab dragged from ASource will be placed
       (a line is drawn above the item with this index), or -1 if it can not be dropped.
    }
    function GetInsertIndex(ASource: TFileViewTabList; X, Y: Integer): Integer;
    procedure UpdateScrollBar;
    function IsWindowAtBounds: Boolean;
    procedure UpdateVisibility;
    procedure UpdateDragScroll(Y: Integer);
    procedure StopDragScroll;
    procedure MakeActiveVisible;
    procedure CheckActiveVisible;
    procedure NotebookTabsChanged(Sender: TObject);
    procedure ScrollTimerTimer(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure WMVScroll(var Message: TLMVScroll); message LM_VSCROLL;

  protected
    procedure CreateWnd; override;
    procedure DoSendBoundsToInterface; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure FontChanged(Sender: TObject); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoOnShowHint(HintInfo: PHintInfo); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;

  public
    constructor Create(AOwner: TComponent; ANotebook: TFileViewNotebook); reintroduce;
    destructor Destroy; override;

    {en
       Applies tabs position, width and visibility from the global options.
    }
    procedure UpdateLayout;
    {en
       Returns index of the tab at client position or -1.
    }
    function IndexOfTabAt(X, Y: Integer): Integer;

    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
                       var Accept: Boolean); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;

    property Notebook: TFileViewNotebook read FNotebook;
    property DraggedIndex: Integer read FDraggedIndex;
    {en
       @true if the panel of this list is the active (focused) panel.
    }
    property ActivePanel: Boolean read FActivePanel write SetActivePanel;
    property OnWidthChanged: TNotifyEvent read FOnWidthChanged write FOnWidthChanged;

  published
    property OnMouseDown;
    property OnMouseUp;
    property OnDragOver;
    property OnDragDrop;
  end;

implementation

uses
  Math, uGlobs, uFilePanelSelect;

const
  ACCENT_WIDTH = 3;  // at 96 DPI
  TEXT_MARGIN  = 8;  // at 96 DPI

function BlendColors(AColor1, AColor2: TColor; APercent: Integer): TColor;
var
  C1, C2: TColorRef;
begin
  C1:= ColorToRGB(AColor1);
  C2:= ColorToRGB(AColor2);
  Result:= RGBToColor(
    (Red(C1)   * (100 - APercent) + Red(C2)   * APercent) div 100,
    (Green(C1) * (100 - APercent) + Green(C2) * APercent) div 100,
    (Blue(C1)  * (100 - APercent) + Blue(C2)  * APercent) div 100);
end;

{ TFileViewTabList }

constructor TFileViewTabList.Create(AOwner: TComponent; ANotebook: TFileViewNotebook);
begin
  inherited Create(AOwner);
  ControlStyle:= ControlStyle + [csNoFocus, csOpaque] - [csSetCaption];

  FTopIndex:= 0;
  FHotIndex:= -1;
  FDownIndex:= -1;
  FDraggedIndex:= -1;
  FDropIndex:= -1;
  FInsertIndex:= -1;
  FLastPageIndex:= -1;
  FLastPageCount:= -1;

  Visible:= False;
  TabStop:= False;
  ShowHint:= True;
  Color:= clBtnFace;
  DoubleBuffered:= True;

  FNotebook:= ANotebook;
  FNotebook.FreeNotification(Self);
  FNotebook.OnTabsChanged:= @NotebookTabsChanged;

  // Parent must be set before Scale96ToForm, it needs the parent form
  Parent:= FNotebook.Parent;

  Width:= Scale96ToForm(gDirTabVerticalWidth);
  Constraints.MinWidth:= Scale96ToForm(40);

  // Scrolls the list while a tab or files are dragged near its top or bottom edge
  FScrollTimer:= TTimer.Create(Self);
  FScrollTimer.Enabled:= False;
  FScrollTimer.Interval:= 120;
  FScrollTimer.OnTimer:= @ScrollTimerTimer;

  FSplitter:= TSplitter.Create(Self);
  FSplitter.Visible:= False;
  FSplitter.ResizeStyle:= rsUpdate;
  FSplitter.MinSize:= Scale96ToForm(40);
  FSplitter.Parent:= Parent;
  FSplitter.OnMoved:= @SplitterMoved;
end;

destructor TFileViewTabList.Destroy;
begin
  if Assigned(FNotebook) then
  begin
    FNotebook.OnTabsChanged:= nil;
    FNotebook.RemoveFreeNotification(Self);
    FNotebook:= nil;
  end;
  inherited Destroy;
end;

procedure TFileViewTabList.CreateWnd;
begin
  inherited CreateWnd;
  // A new window has no scroll bar
  FScrollBarVisible:= False;
  UpdateScrollBar;
end;

procedure TFileViewTabList.DoSendBoundsToInterface;
begin
  inherited DoSendBoundsToInterface;
  // The window is at its new place now, see UpdateScrollBar
  if FScrollBarPending then
  begin
    FScrollBarPending:= False;
    UpdateScrollBar;
  end;
end;

procedure TFileViewTabList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FNotebook) then
    FNotebook:= nil;
end;

procedure TFileViewTabList.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  FItemHeight:= 0;
  if Visible then
  begin
    UpdateScrollBar;
    Invalidate;
  end;
end;

function TFileViewTabList.CalcItemHeight: Integer;
begin
  Canvas.Font:= Font;
  Result:= Canvas.TextHeight('Wy') + Scale96ToForm(10);
end;

function TFileViewTabList.GetItemHeight: Integer;
begin
  // The value is refreshed on every Paint, because font and DPI
  // can be changed by the form scaling after the control is created
  if FItemHeight = 0 then
  begin
    if not HandleAllocated then
      Exit(Scale96ToForm(26));
    FItemHeight:= CalcItemHeight;
  end;
  Result:= FItemHeight;
end;

function TFileViewTabList.GetItemsWidth: Integer;
begin
  // The scroll bar is in the non-client area, like in the file list
  Result:= ClientWidth;
end;

function TFileViewTabList.GetVisibleCount: Integer;
begin
  Result:= ClientHeight div GetItemHeight;
end;

function TFileViewTabList.GetItemRect(Index: Integer): TRect;
var
  AHeight: Integer;
begin
  AHeight:= GetItemHeight;
  Result:= Rect(0, (Index - FTopIndex) * AHeight, GetItemsWidth,
                (Index - FTopIndex + 1) * AHeight);
end;

function TFileViewTabList.GetCloseRect(Index: Integer): TRect;
var
  ASize: Integer;
  ARect: TRect;
begin
  ARect:= GetItemRect(Index);
  ASize:= Scale96ToForm(16);
  Result.Right:= ARect.Right - Scale96ToForm(6);
  if Align = alRight then Dec(Result.Right, Scale96ToForm(ACCENT_WIDTH));
  Result.Left:= Result.Right - ASize;
  Result.Top:= ARect.Top + (ARect.Height - ASize) div 2;
  Result.Bottom:= Result.Top + ASize;
end;

function TFileViewTabList.GetTabCaption(Index: Integer): String;
begin
  Result:= FNotebook.Page[Index].Caption;
{$IF NOT DEFINED(LCLGTK2)}
  // TFileViewPage.UpdateTitle escapes '&' for the native tab header
  Result:= StringReplace(Result, '&&', '&', [rfReplaceAll]);
{$ENDIF}
end;

function TFileViewTabList.HasCloseButton: Boolean;
begin
  Result:= (tb_show_close_button in gDirTabOptions) and Assigned(FNotebook) and
           (FNotebook.PageCount > 1);
end;

function TFileViewTabList.IsOnCloseButton(Index, X, Y: Integer): Boolean;
begin
  Result:= (Index >= 0) and HasCloseButton and
           PtInRect(GetCloseRect(Index), Point(X, Y));
end;

function TFileViewTabList.IndexOfTabAt(X, Y: Integer): Integer;
begin
  Result:= -1;
  if not Assigned(FNotebook) then Exit;
  if (X < 0) or (X >= GetItemsWidth) or (Y < 0) then Exit;
  Result:= FTopIndex + Y div GetItemHeight;
  if Result >= FNotebook.PageCount then Result:= -1;
end;

procedure TFileViewTabList.SetActivePanel(AValue: Boolean);
begin
  if FActivePanel = AValue then Exit;
  FActivePanel:= AValue;
  Invalidate;
end;

procedure TFileViewTabList.SetTopIndex(AValue: Integer);
var
  AMax: Integer;
begin
  if Assigned(FNotebook) then
    AMax:= FNotebook.PageCount - GetVisibleCount
  else
    AMax:= 0;
  if AValue > AMax then AValue:= AMax;
  if AValue < 0 then AValue:= 0;
  if FTopIndex = AValue then Exit;
  FTopIndex:= AValue;
  if FScrollBarVisible and HandleAllocated and not FScrollBarPending then
    SetScrollPos(Handle, SB_VERT, FTopIndex, True);
  Invalidate;
end;

procedure TFileViewTabList.SetHot(AIndex: Integer; ACloseHot: Boolean);
begin
  if (FHotIndex = AIndex) and (FHotClose = ACloseHot) then Exit;
  FHotIndex:= AIndex;
  FHotClose:= ACloseHot;
  Invalidate;
end;

procedure TFileViewTabList.SetDropIndex(AValue: Integer);
begin
  if FDropIndex = AValue then Exit;
  FDropIndex:= AValue;
  Invalidate;
end;

procedure TFileViewTabList.SetInsertIndex(AValue: Integer);
begin
  if FInsertIndex = AValue then Exit;
  FInsertIndex:= AValue;
  Invalidate;
end;

function TFileViewTabList.GetInsertIndex(ASource: TFileViewTabList; X, Y: Integer): Integer;
var
  AIndex: Integer;
begin
  Result:= -1;
  if not (Assigned(FNotebook) and Assigned(ASource) and Assigned(ASource.Notebook)) then
    Exit;

  AIndex:= IndexOfTabAt(X, Y);
  if ASource = Self then
  begin
    // Same rules as TFileViewNotebook.MovePageFrom: the dragged tab takes
    // the place of the target tab, the empty area below the tabs means the end
    if AIndex = -1 then AIndex:= FNotebook.PageCount - 1;
    if (FDraggedIndex < 0) or (AIndex = FDraggedIndex) then Exit;
    if AIndex > FDraggedIndex then
      Result:= AIndex + 1
    else
      Result:= AIndex;
  end
  else begin
    // A copy is inserted before the target tab or at the end
    if AIndex = -1 then
      Result:= FNotebook.PageCount
    else
      Result:= AIndex;
  end;
end;

function TFileViewTabList.IsWindowAtBounds: Boolean;
var
  X, Y, W, H: Integer;
begin
  X:= 0; Y:= 0; W:= 0; H:= 0;
  Result:= GetWindowRelativePosition(Handle, X, Y) and GetWindowSize(Handle, W, H) and
           (X = Left) and (Y = Top) and (W = Width) and (H = Height);
end;

procedure TFileViewTabList.UpdateScrollBar;
var
  ACount, AVisibleCount: Integer;
  AShow: Boolean;
  AInfo: TScrollInfo;
begin
  // Showing or hiding the scroll bar resizes the control, which calls this again
  if not Assigned(FNotebook) or FUpdatingScrollBar then Exit;
  FUpdatingScrollBar:= True;
  try
    ACount:= FNotebook.PageCount;
    AVisibleCount:= GetVisibleCount;
    AShow:= (AVisibleCount > 0) and (ACount > AVisibleCount);

    SetTopIndex(FTopIndex); // Clamp to new range

    if HandleAllocated then
    begin
{$IF DEFINED(LCLWIN32)}
      // Changing the scroll bar makes Windows send WM_SIZE, and LCL takes the position
      // for it from Left/Top. While the form delays moving the window to new Left/Top
      // (e.g. when the tabs position is changed), LCL would then think that the window
      // was already moved and never move it. So wait until it is moved.
      if not IsWindowAtBounds then
      begin
        FScrollBarPending:= True;
        Exit;
      end;
{$ENDIF}
      FScrollBarPending:= False;
      if AShow then
      begin
        AInfo:= Default(TScrollInfo);
        AInfo.cbSize:= SizeOf(AInfo);
        AInfo.fMask:= SIF_RANGE or SIF_PAGE or SIF_POS;
        AInfo.nMin:= 0;
        AInfo.nMax:= ACount; // LCL treats the maximum as exclusive
        AInfo.nPage:= AVisibleCount;
        AInfo.nPos:= FTopIndex;
        SetScrollInfo(Handle, SB_VERT, AInfo, True);
      end;
      if AShow <> FScrollBarVisible then
      begin
        FScrollBarVisible:= AShow;
        ShowScrollBar(Handle, SB_VERT, AShow);
      end;
    end;
  finally
    FUpdatingScrollBar:= False;
  end;
end;

procedure TFileViewTabList.WMVScroll(var Message: TLMVScroll);
begin
  case Message.ScrollCode of
    SB_TOP:        SetTopIndex(0);
    SB_BOTTOM:     SetTopIndex(MaxInt);
    SB_LINEUP:     SetTopIndex(FTopIndex - 1);
    SB_LINEDOWN:   SetTopIndex(FTopIndex + 1);
    SB_PAGEUP:     SetTopIndex(FTopIndex - Max(1, GetVisibleCount));
    SB_PAGEDOWN:   SetTopIndex(FTopIndex + Max(1, GetVisibleCount));
    SB_THUMBTRACK,
    SB_THUMBPOSITION: SetTopIndex(Message.Pos);
  end;
  Message.Result:= 0;
end;

procedure TFileViewTabList.UpdateDragScroll(Y: Integer);
var
  AZone: Integer;
begin
  AZone:= GetItemHeight div 3;
  if (Y < AZone) and (FTopIndex > 0) then
    FScrollDelta:= -1
  else if (Y >= ClientHeight - AZone) and FScrollBarVisible then
    FScrollDelta:= 1
  else
    FScrollDelta:= 0;
  FScrollTimer.Enabled:= (FScrollDelta <> 0);
end;

procedure TFileViewTabList.StopDragScroll;
begin
  FScrollTimer.Enabled:= False;
  FScrollDelta:= 0;
  FDragSource:= nil;
end;

procedure TFileViewTabList.ScrollTimerTimer(Sender: TObject);
var
  AOldTop: Integer;
  APoint: TPoint;
begin
  if not DragManager.IsDragging then
  begin
    StopDragScroll;
    SetInsertIndex(-1);
    SetDropIndex(-1);
    Exit;
  end;

  AOldTop:= FTopIndex;
  SetTopIndex(FTopIndex + FScrollDelta);
  if FTopIndex = AOldTop then
  begin
    FScrollTimer.Enabled:= False;
    Exit;
  end;

  // The mouse stays still, so update the drop position under it
  if FDragSource is TFileViewTabList then
  begin
    APoint:= ScreenToClient(Mouse.CursorPos);
    SetInsertIndex(GetInsertIndex(TFileViewTabList(FDragSource), APoint.X, APoint.Y));
  end
  else begin
    // Files can be dropped only to some tabs, it is checked on the next mouse move
    SetDropIndex(-1);
  end;
end;

procedure TFileViewTabList.UpdateVisibility;
var
  AVisible: Boolean;
begin
  AVisible:= Assigned(FNotebook) and FNotebook.IsVerticalTabs and FNotebook.TabsVisible;
  if AVisible = Visible then Exit;

  if AVisible then
  begin
    Visible:= True;
    // The splitter must be placed between this list and the notebook
    if Align = alRight then
      FSplitter.Left:= Left - FSplitter.Width - 1
    else
      FSplitter.Left:= Left + Width + 1;
    FSplitter.Visible:= True;
  end
  else begin
    FSplitter.Visible:= False;
    Visible:= False;
  end;
end;

procedure TFileViewTabList.MakeActiveVisible;
var
  AIndex, AVisibleCount: Integer;
begin
  if not Assigned(FNotebook) then Exit;
  AIndex:= FNotebook.PageIndex;
  FLastPageIndex:= AIndex;
  FLastPageCount:= FNotebook.PageCount;
  if AIndex < 0 then Exit;
  AVisibleCount:= GetVisibleCount;
  if AIndex < FTopIndex then
    SetTopIndex(AIndex)
  else if (AVisibleCount > 0) and (AIndex >= FTopIndex + AVisibleCount) then
    SetTopIndex(AIndex - AVisibleCount + 1);
end;

procedure TFileViewTabList.CheckActiveVisible;
begin
  // Scroll to the active tab only when it or the tabs count changes,
  // not on title updates or resizing, to keep the position the user scrolled to
  if Assigned(FNotebook) and ((FNotebook.PageIndex <> FLastPageIndex) or
                              (FNotebook.PageCount <> FLastPageCount)) then
    MakeActiveVisible;
end;

procedure TFileViewTabList.UpdateLayout;
var
  AAlign: TAlign;
begin
  if not Assigned(FNotebook) or not Assigned(Parent) then Exit;

  case gDirTabPosition of
    tbpos_right:
      AAlign:= alRight;
    // Next to the splitter between the panels
    tbpos_middle:
      if FNotebook.Side = fpLeft then AAlign:= alRight else AAlign:= alLeft;
    // On the outer side of each panel
    tbpos_sides:
      if FNotebook.Side = fpLeft then AAlign:= alLeft else AAlign:= alRight;
    else
      AAlign:= alLeft;
  end;

  Parent.DisableAlign;
  try
    // Hide first, so the controls order is restored by UpdateVisibility
    FSplitter.Visible:= False;
    Visible:= False;

    Align:= AAlign;
    FSplitter.Align:= AAlign;
    Width:= Scale96ToForm(gDirTabVerticalWidth);
    if AAlign = alRight then
      Left:= Parent.ClientWidth - Width
    else
      Left:= 0;

    UpdateVisibility;
  finally
    Parent.EnableAlign;
  end;

  if Visible then
  begin
    UpdateScrollBar;
    CheckActiveVisible;
    Invalidate;
  end;
end;

procedure TFileViewTabList.NotebookTabsChanged(Sender: TObject);
begin
  UpdateVisibility;
  if Visible then
  begin
    if (FHotIndex >= 0) and Assigned(FNotebook) and (FHotIndex >= FNotebook.PageCount) then
      FHotIndex:= -1;
    UpdateScrollBar;
    CheckActiveVisible;
    Invalidate;
  end;
end;

procedure TFileViewTabList.SplitterMoved(Sender: TObject);
begin
  gDirTabVerticalWidth:= ScaleFormTo96(Width);
  if Assigned(FOnWidthChanged) then
    FOnWidthChanged(Self);
end;

procedure TFileViewTabList.Resize;
begin
  inherited Resize;
  if Visible then
  begin
    UpdateScrollBar;
    CheckActiveVisible;
  end;
end;

procedure TFileViewTabList.Paint;
var
  I: Integer;
  ARect, ATextRect, ACloseRect: TRect;
  ABackColor, AFontColor, AAccentColor: TColor;
  AActiveTab, AShowClose: Boolean;
  AStyle: TTextStyle;
  AAccent, AMargin, D, Y: Integer;
begin
  D:= CalcItemHeight;
  if D <> FItemHeight then
  begin
    FItemHeight:= D;
    UpdateScrollBar;
  end;

  Canvas.Brush.Style:= bsSolid;
  Canvas.Brush.Color:= clBtnFace;
  Canvas.FillRect(ClientRect);
  if not Assigned(FNotebook) then Exit;

  Canvas.Font:= Font;
  AStyle:= Canvas.TextStyle;
  AStyle.SingleLine:= True;
  AStyle.Wordbreak:= False;
  AStyle.EndEllipsis:= True;
  AStyle.ShowPrefix:= False;
  AStyle.Clipping:= True;
  AStyle.Opaque:= False;
  AStyle.Layout:= tlCenter;
  AStyle.Alignment:= taLeftJustify;

  AAccent:= Scale96ToForm(ACCENT_WIDTH);
  AMargin:= Scale96ToForm(TEXT_MARGIN);

  for I:= FTopIndex to FNotebook.PageCount - 1 do
  begin
    ARect:= GetItemRect(I);
    if ARect.Top >= ClientHeight then Break;

    AActiveTab:= (I = FNotebook.PageIndex);

    // Background
    if AActiveTab then
      ABackColor:= clWindow
    else if (I = FHotIndex) or (I = FDropIndex) then
      ABackColor:= BlendColors(clBtnFace, clWindow, 50)
    else
      ABackColor:= clBtnFace;
    Canvas.Brush.Color:= ABackColor;
    Canvas.FillRect(ARect);

    // Accent bar on the outer side of the active tab
    if AActiveTab then
    begin
      if FActivePanel then
        AAccentColor:= clHighlight
      else
        AAccentColor:= clBtnShadow;
      Canvas.Brush.Color:= AAccentColor;
      if Align = alRight then
        Canvas.FillRect(Rect(ARect.Right - AAccent, ARect.Top, ARect.Right, ARect.Bottom))
      else
        Canvas.FillRect(Rect(ARect.Left, ARect.Top, ARect.Left + AAccent, ARect.Bottom));
    end;

    // Drop target frame
    if (I = FDropIndex) then
    begin
      Canvas.Pen.Color:= clHighlight;
      Canvas.Pen.Width:= 1;
      Canvas.Brush.Style:= bsClear;
      Canvas.Rectangle(ARect);
      Canvas.Brush.Style:= bsSolid;
    end;

    if AActiveTab then
      AFontColor:= clWindowText
    else
      AFontColor:= clBtnText;

    // Close button
    AShowClose:= HasCloseButton and (AActiveTab or (I = FHotIndex));
    ATextRect:= ARect;
    ATextRect.Left:= ARect.Left + AAccent + AMargin;
    ATextRect.Right:= ARect.Right - AMargin;
    if Align = alRight then Dec(ATextRect.Right, AAccent);

    if AShowClose then
    begin
      ACloseRect:= GetCloseRect(I);
      ATextRect.Right:= ACloseRect.Left - Scale96ToForm(2);
      if FHotClose and (I = FHotIndex) then
      begin
        Canvas.Brush.Color:= BlendColors(ABackColor, AFontColor, 20);
        Canvas.FillRect(ACloseRect);
      end;
      D:= ACloseRect.Width div 4;
      Canvas.Pen.Color:= AFontColor;
      Canvas.Pen.Width:= Scale96ToForm(1);
      Canvas.Line(ACloseRect.Left + D, ACloseRect.Top + D, ACloseRect.Right - D, ACloseRect.Bottom - D);
      Canvas.Line(ACloseRect.Left + D, ACloseRect.Bottom - D - 1, ACloseRect.Right - D, ACloseRect.Top + D - 1);
    end;

    // Caption
    Canvas.Font.Color:= AFontColor;
    Canvas.Brush.Style:= bsClear;
    Canvas.TextRect(ATextRect, ATextRect.Left, ATextRect.Top, GetTabCaption(I), AStyle);
    Canvas.Brush.Style:= bsSolid;
  end;

  // Where the dragged tab will be placed
  if FInsertIndex >= 0 then
  begin
    D:= Scale96ToForm(2);
    Y:= (FInsertIndex - FTopIndex) * GetItemHeight - D div 2;
    if (Y >= -D) and (Y <= ClientHeight) then
    begin
      Y:= EnsureRange(Y, 0, ClientHeight - D);
      Canvas.Brush.Color:= clHighlight;
      Canvas.FillRect(Rect(0, Y, GetItemsWidth, Y + D));
    end;
  end;
end;

procedure TFileViewTabList.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AIndex: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not Assigned(FNotebook) then Exit;

  if (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    AIndex:= IndexOfTabAt(X, Y);
    FDownIndex:= AIndex;
    FCloseDown:= IsOnCloseButton(AIndex, X, Y);
    FStartDrag:= (AIndex <> -1) and not FCloseDown;
    if FStartDrag and (AIndex <> FNotebook.PageIndex) then
      FNotebook.PageIndex:= AIndex;
  end;
end;

procedure TFileViewTabList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AIndex: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if not Assigned(FNotebook) then Exit;

  if FStartDrag then
  begin
    FStartDrag:= False;
    FDraggedIndex:= FDownIndex;
    BeginDrag(False);
  end;

  AIndex:= IndexOfTabAt(X, Y);
  SetHot(AIndex, IsOnCloseButton(AIndex, X, Y));
end;

procedure TFileViewTabList.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AIndex: Integer;
begin
  FStartDrag:= False;
  if (Button = mbLeft) and FCloseDown and Assigned(FNotebook) then
  begin
    FCloseDown:= False;
    AIndex:= IndexOfTabAt(X, Y);
    if (AIndex = FDownIndex) and IsOnCloseButton(AIndex, X, Y) and
       Assigned(FNotebook.OnCloseTabClicked) then
    begin
      FNotebook.OnCloseTabClicked(FNotebook.Page[AIndex]);
    end;
  end;
  FCloseDown:= False;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TFileViewTabList.MouseLeave;
begin
  inherited MouseLeave;
  SetHot(-1, False);
end;

procedure TFileViewTabList.DblClick;
var
  APoint: TPoint;
  AIndex: Integer;
begin
  inherited DblClick;
  if not Assigned(FNotebook) then Exit;

  FStartDrag:= False;
  APoint:= ScreenToClient(Mouse.CursorPos);
  AIndex:= IndexOfTabAt(APoint.X, APoint.Y);
  if IsOnCloseButton(AIndex, APoint.X, APoint.Y) then Exit;
  FNotebook.DoTabDblClick(AIndex);
end;

function TFileViewTabList.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:= inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    if WheelDelta > 0 then
      SetTopIndex(FTopIndex - Mouse.WheelScrollLines)
    else if WheelDelta < 0 then
      SetTopIndex(FTopIndex + Mouse.WheelScrollLines);
    Result:= True;
  end;
end;

procedure TFileViewTabList.DoOnShowHint(HintInfo: PHintInfo);
var
  AIndex: Integer;
begin
  AIndex:= IndexOfTabAt(HintInfo^.CursorPos.X, HintInfo^.CursorPos.Y);
  if AIndex >= 0 then
  begin
    HintInfo^.HintStr:= FNotebook.GetTabHint(AIndex);
    // Show the hint again when the mouse moves to another tab
    HintInfo^.CursorRect:= GetItemRect(AIndex);
  end
  else
    HintInfo^.HintStr:= EmptyStr;
  inherited DoOnShowHint(HintInfo);
end;

procedure TFileViewTabList.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  AInsert: Integer;
begin
  if State = dsDragLeave then
    StopDragScroll
  else begin
    FDragSource:= Source;
    UpdateDragScroll(Y);
  end;

  if (Source is TFileViewTabList) then
  begin
    // Reordering tabs: show a line where the tab will be placed.
    // Accept must be set for dsDragLeave too, because LCL sends it
    // on the mouse button release and drops only if it is accepted.
    AInsert:= GetInsertIndex(TFileViewTabList(Source), X, Y);
    Accept:= (AInsert >= 0);
    if State = dsDragLeave then
      SetInsertIndex(-1)
    else
      SetInsertIndex(AInsert);
    SetDropIndex(-1);
  end
  else begin
    // Files: highlight the tab they will be copied to
    inherited DragOver(Source, X, Y, State, Accept);
    if Accept and (State <> dsDragLeave) then
      SetDropIndex(IndexOfTabAt(X, Y))
    else
      SetDropIndex(-1);
    SetInsertIndex(-1);
  end;
end;

procedure TFileViewTabList.DragDrop(Source: TObject; X, Y: Integer);
var
  AIndex: Integer;
  ASource: TFileViewTabList;
begin
  StopDragScroll;
  SetDropIndex(-1);
  SetInsertIndex(-1);
  if (Source is TFileViewTabList) then
  begin
    ASource:= TFileViewTabList(Source);
    if not (Assigned(FNotebook) and Assigned(ASource.Notebook)) then Exit;

    AIndex:= IndexOfTabAt(X, Y);
    if (Source = Self) and (AIndex = -1) then
      AIndex:= FNotebook.PageCount - 1;

    FNotebook.MovePageFrom(ASource.Notebook, ASource.DraggedIndex, AIndex);
  end
  else begin
    inherited DragDrop(Source, X, Y);
  end;
end;

procedure TFileViewTabList.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited DoEndDrag(Target, X, Y);
  FDraggedIndex:= -1;
  StopDragScroll;
  SetDropIndex(-1);
  SetInsertIndex(-1);
end;

end.
