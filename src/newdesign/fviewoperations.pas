unit fViewOperations; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, LCLType,
  ComCtrls, Buttons, LCLIntf, Menus, uFileSourceOperation, uOperationsManager,
  Themes;

type

  TViewBaseItem = class;
  TViewBaseItemClick = procedure(Item: TViewBaseItem; Button: TMouseButton; Shift: TShiftState; const Pt: TPoint) of object;

  { TViewBaseItem }

  TViewBaseItem = class
  private
    FOnClick: TViewBaseItemClick;
    FTreeNode: TTreeNode;
    procedure DrawThemedBackground(Canvas: TCanvas; Element: TThemedTreeview; ARect: TRect);
    procedure DrawThemedText(Canvas: TCanvas; Element: TThemedTreeview; NodeRect: TRect; Center: Boolean; AText: String);
    function GetHeight: Integer;
  public
    constructor Create(ANode: TTreeNode); virtual;
    procedure Click(const Pt: TPoint; Button: TMouseButton; Shift: TShiftState); virtual;
    procedure Draw(Canvas: TCanvas; NodeRect: TRect); virtual; abstract;
    function GetBackgroundColor: TColor; virtual; abstract;
    function GetHeight(Canvas: TCanvas): Integer; virtual; abstract;
    property Height: Integer read GetHeight;
    property OnClick: TViewBaseItemClick read FOnClick write FOnClick;
  end;

  { TViewQueueItem }

  TViewQueueItem = class(TViewBaseItem)
  private
    FQueueIdentifier: TOperationsManagerQueueIdentifier;
    FText: String;
  public
    constructor Create(ANode: TTreeNode; AQueueId: TOperationsManagerQueueIdentifier); reintroduce;
    procedure Draw(Canvas: TCanvas; NodeRect: TRect); override;
    function GetBackgroundColor: TColor; override;
    function GetHeight(Canvas: TCanvas): Integer; override;
  end;

  TViewOperationItem = class;
  TOpItemOnStatusIconClick = procedure(Item: TViewOperationItem) of object;

  { TViewOperationItem }

  TViewOperationItem = class(TViewBaseItem)
  private
    FOperationHandle: TOperationHandle;
    FOnStatusIconClick: TOpItemOnStatusIconClick;
    FTextHeight: Integer;
  public
    constructor Create(ANode: TTreeNode; AOperationHandle: TOperationHandle); reintroduce;
    procedure Click(const Pt: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure Draw(Canvas: TCanvas; NodeRect: TRect); override;
    function GetHeight(Canvas: TCanvas): Integer; override;
    function GetBackgroundColor: TColor; override;
    property OnStatusIconClick: TOpItemOnStatusIconClick read FOnStatusIconClick write FOnStatusIconClick;
  end;

  { TfrmViewOperations }

  TfrmViewOperations = class(TForm)
    btnCancelCurOp: TBitBtn;
    btnStartPause: TBitBtn;
    cbCurrentQueue: TComboBox;
    lblActiveOperations: TLabel;
    lblOperationsCountNumber: TLabel;
    lblActiveOperationsNumber: TLabel;
    mnuCancel: TMenuItem;
    mnuPutFirstInQueue: TMenuItem;
    mnuPutLastInQueue: TMenuItem;
    mnuShowDetached: TMenuItem;
    mnuQueue2: TMenuItem;
    mnuQueue3: TMenuItem;
    mnuQueue5: TMenuItem;
    mnuQueue4: TMenuItem;
    mnuQueue1: TMenuItem;
    mnuQueue0: TMenuItem;
    mnuQueue: TMenuItem;
    lblOperationsCount: TLabel;
    Cntr_running: TPanel;
    pnlCurrentOperation: TPanel;
    pnlHeader: TPanel;
    pmOperationPopup: TPopupMenu;
    tbPauseAll: TToggleBox;
    tvOperations: TTreeView;
    UpdateTimer: TTimer;

    procedure btnCancelCurOpClick(Sender: TObject);
    procedure cbCurrentQueueChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuPutFirstInQueueClick(Sender: TObject);
    procedure mnuPutLastInQueueClick(Sender: TObject);
    procedure mnuShowDetachedClick(Sender: TObject);
    procedure OnOperationItemClick(Item: TViewBaseItem; Button: TMouseButton; Shift: TShiftState; const Pt: TPoint);
    procedure OnItemStatusIconClick(Item: TViewOperationItem);
    procedure OnUpdateTimer(Sender: TObject);
    procedure btnStartPauseClick(Sender: TObject);
    procedure mnuQueueNumberClick(Sender: TObject);
    procedure tbPauseAllChange(Sender: TObject);
    procedure tvOperationsCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvOperationsDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvOperationsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvOperationsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tvOperationsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tvOperationsSelectionChanged(Sender: TObject);
  private
    FMenuOperation: TOperationHandle;
    function GetFocusedItem: TViewBaseItem;
    procedure SetFocusItem(AOperationHandle: TOperationHandle);
    procedure SetNewQueue(Item: TViewOperationItem; NewQueue: TOperationsManagerQueueIdentifier);
    procedure UpdateView(Operation: TFileSourceOperation; Event: TOperationManagerEvent);
    procedure UpdateCounters;
    procedure UpdateControls;
    procedure UpdateItems;
  end;

procedure ShowOperationsViewer(AOperationHandle: TOperationHandle = InvalidOperationHandle);

implementation

{$R *.lfm}

uses
  GraphMath, GraphType, Math,
  uFileSourceOperationTypes,
  uLng, fFileOpDlg, uGlobs, LCLProc;

const
  StatusIconPlay: array[0..2] of TPoint =
    ((x: 12; y: 1), (x: 20; y: 9), (x: 12; y: 17));
  StatusIconPause1: array[0..3] of TPoint =
    ((x: 10; y: 2), (x: 10; y: 16), (x: 14; y: 16), (x: 14; y: 2));
  StatusIconPause2: array[0..3] of TPoint =
    ((x: 17; y: 2), (x: 17; y: 16), (x: 21; y: 16), (x: 21; y: 2));
  StatusIconHourglass1: array[0..2] of TPoint =
    ((x: 10; y: 4), (x: 20; y: 4), (x: 15; y: 10));
  StatusIconHourglass2: array[0..2] of TPoint =
    ((x: 15; y: 10), (x: 20; y: 15), (x: 10; y: 15));
  StatusIconFrame: TRect = (Left: 7; Top: 0; Right: 24; Bottom: 19);
  ProgressHeight = 8;

var
  frmViewOperations: TfrmViewOperations = nil;

procedure ShowOperationsViewer(AOperationHandle: TOperationHandle);
begin
  if not Assigned(frmViewOperations) then
    frmViewOperations := TfrmViewOperations.Create(Application);
  frmViewOperations.ShowOnTop;
  if AOperationHandle <> InvalidOperationHandle then
    frmViewOperations.SetFocusItem(AOperationHandle);
end;

function MoveRect(aRect: TRect; DeltaX, DeltaY: Integer): TRect;
begin
  Result.Left := aRect.Left + DeltaX;
  Result.Right := aRect.Right + DeltaX;
  Result.Top := aRect.Top + DeltaY;
  Result.Bottom := aRect.Bottom + DeltaY;
end;

procedure DrawMovePolygon(Canvas: TCanvas; const Points: array of TPoint; DeltaX, DeltaY: Integer);
var
  CopyPoints: PPoint;
  i: Integer;
begin
  CopyPoints := GetMem(SizeOf(TPoint) * Length(Points));
  for i := 0 to Length(Points) - 1 do
  begin
    CopyPoints[i].x := Points[i].x + DeltaX;
    CopyPoints[i].y := Points[i].y + DeltaY;
  end;
  Canvas.Polygon(CopyPoints, Length(Points), True);
  FreeMem(CopyPoints);
end;

{ TViewBaseItem }

procedure TViewBaseItem.DrawThemedBackground(Canvas: TCanvas; Element: TThemedTreeview; ARect: TRect);
var
  Details: TThemedElementDetails;
begin
  Details := ThemeServices.GetElementDetails(Element);
  if ThemeServices.HasTransparentParts(Details) then
  begin
    Canvas.Brush.Color := GetBackgroundColor;
    Canvas.FillRect(ARect);
  end;
  ThemeServices.DrawElement(Canvas.Handle, Details, ARect, nil);
end;

procedure TViewBaseItem.DrawThemedText(Canvas: TCanvas; Element: TThemedTreeview; NodeRect: TRect; Center: Boolean; AText: String);
var
  Details: TThemedElementDetails;
  Flags: Cardinal = DT_SINGLELINE or DT_NOPREFIX;
begin
  Details := ThemeServices.GetElementDetails(Element);
  if Center then
    Flags := Flags + DT_VCENTER;
  ThemeServices.DrawText(Canvas, Details, AText, NodeRect, Flags, 0);
end;

function TViewBaseItem.GetHeight: Integer;
begin
  Result := FTreeNode.Height;
end;

constructor TViewBaseItem.Create(ANode: TTreeNode);
begin
  FTreeNode := ANode;
end;

procedure TViewBaseItem.Click(const Pt: TPoint; Button: TMouseButton; Shift: TShiftState);
begin
  if Assigned(FOnClick) then
    FOnClick(Self, Button, Shift, Pt);
end;

{ TViewOperationItem }

constructor TViewOperationItem.Create(ANode: TTreeNode; AOperationHandle: TOperationHandle);
begin
  FOperationHandle := AOperationHandle;
  inherited Create(ANode);
end;

procedure TViewOperationItem.Click(const Pt: TPoint; Button: TMouseButton; Shift: TShiftState);
begin
  if not (ssDouble in Shift) and (Button = mbLeft) and PtInRect(StatusIconFrame, Pt) then
    OnStatusIconClick(Self)
  else
    inherited;
end;

procedure TViewOperationItem.Draw(Canvas: TCanvas; NodeRect: TRect);
var
  OutString: string;
  OpManItem: TOperationsManagerItem;
  aRect: TRect;
  Element: TThemedTreeview;
  aWidth: Integer;
  StatusIconTop: Integer;
begin
  if FTreeNode.Selected then
    Element := ttItemSelected
  else
    Element := ttItemNormal;

  DrawThemedBackground(Canvas, Element, NodeRect);

  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) then
  begin
    case OpManItem.Operation.ID of
      fsoCopy, fsoCopyIn, fsoCopyOut:
        OutString := rsDlgCp;
      fsoMove:
        OutString := rsDlgMv;
      fsoDelete:
        OutString := rsDlgDel;
      fsoWipe:
        OutString := rsDlgWipe;
      fsoCalcChecksum:
        OutString := rsDlgCheckSumCalc;
      else
        OutString := rsDlgUnknownOperation;
    end;
    OutString := IntToStr(OpManItem.Handle) + ': ' + OutString + ' - ' +
      FloatToStrF(OpManItem.Operation.Progress * 100, ffFixed, 1, 1) + ' %' +
      ' (' + FileSourceOperationStateText[OpManItem.Operation.State] + ')';

    aWidth := NodeRect.Right - NodeRect.Left;
    aRect := NodeRect;
    aRect.Left := aRect.Left + 30;
    DrawThemedText(Canvas, Element, aRect, False, OutString);

    Canvas.Brush.Color := GetBackgroundColor;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clWindowText;

    // Progress rectangle.
    aRect.Left   := NodeRect.Left + 30;
    aRect.Top    := NodeRect.Top + FTextHeight;
    aRect.Right  := aRect.Left + (aWidth - aRect.Left) - 10;
    aRect.Bottom := aRect.Top + ProgressHeight;
    Canvas.Frame(aRect);

    StatusIconTop := ((NodeRect.Bottom - NodeRect.Top) - (StatusIconFrame.Bottom - StatusIconFrame.Top)) div 2;

    // Paint status icon.
    Canvas.Rectangle(MoveRect(StatusIconFrame, NodeRect.Left, NodeRect.Top + StatusIconTop));

    case OpManItem.Operation.State of
      fsosRunning:  // Paint "Play" triangle
      begin
        Canvas.Brush.Color := RGBToColor(0, 200, 0);
        DrawMovePolygon(Canvas, StatusIconPlay, NodeRect.Left, NodeRect.Top + StatusIconTop);
      end;
      fsosPaused:   // Paint "Pause" double line
      begin
        Canvas.Brush.Color := RGBToColor(0, 0, 200);
        DrawMovePolygon(Canvas, StatusIconPause1, NodeRect.Left, NodeRect.Top + StatusIconTop);
        DrawMovePolygon(Canvas, StatusIconPause2, NodeRect.Left, NodeRect.Top + StatusIconTop);
      end;
      else           // Paint "Hourglass"
      begin
        Canvas.Brush.Color := RGBToColor(255, 255, 255);
        DrawMovePolygon(Canvas, StatusIconHourglass1, NodeRect.Left, NodeRect.Top + StatusIconTop);
        DrawMovePolygon(Canvas, StatusIconHourglass2, NodeRect.Left, NodeRect.Top + StatusIconTop);
      end;
    end;

    // Paint progress.
    aRect.Right := aRect.Left + Round(aRect.Left + (aWidth - aRect.Left) * OpManItem.Operation.Progress);
    InflateRect(aRect, -1, -1);
    Canvas.FillRect(aRect);
  end;
end;

function TViewOperationItem.GetBackgroundColor: TColor;
begin
  Result := FTreeNode.TreeView.BackgroundColor;
end;

function TViewOperationItem.GetHeight(Canvas: TCanvas): Integer;
begin
  FTextHeight := Canvas.TextExtent('Wg').cy;
  Result := Max(FTextHeight + ProgressHeight + 2, (StatusIconFrame.Bottom - StatusIconFrame.Top) + 4);
end;

{ TViewQueueItem }

constructor TViewQueueItem.Create(ANode: TTreeNode; AQueueId: TOperationsManagerQueueIdentifier);
begin
  FQueueIdentifier := AQueueId;
  inherited Create(ANode);
  FText := rsDlgQueue + ' [#' + IntToStr(AQueueId) + ']';
end;

procedure TViewQueueItem.Draw(Canvas: TCanvas; NodeRect: TRect);
begin
  DrawThemedBackground(Canvas, ttItemSelectedNotFocus, NodeRect);
  NodeRect.Left := NodeRect.Left + 5 + FTreeNode.DisplayTextLeft;
  DrawThemedText(Canvas, ttItemSelectedNotFocus, NodeRect, True, FText);
end;

function TViewQueueItem.GetBackgroundColor: TColor;
begin
  Result := FTreeNode.TreeView.BackgroundColor;
end;

function TViewQueueItem.GetHeight(Canvas: TCanvas): Integer;
begin
  Result := Canvas.TextExtent(FText).cy + 6;
end;

{ TfrmViewOperations }

procedure TfrmViewOperations.FormCreate(Sender: TObject);
begin
  InitPropStorage(Self);

  FMenuOperation := InvalidOperationHandle;
  tvOperations.DoubleBuffered := True;

  UpdateCounters;
  UpdateControls;
  UpdateItems;

  OperationsManager.AddEventsListener([omevOperationAdded, omevOperationRemoved],
    @UpdateView);
end;

procedure TfrmViewOperations.btnCancelCurOpClick(Sender: TObject);
var
  OpManItem: TOperationsManagerItem;
  Item: TViewBaseItem;
begin
  Item := GetFocusedItem;
  if Assigned(Item) and (Item is TViewOperationItem) then
  begin
    OpManItem := OperationsManager.GetItemByHandle(TViewOperationItem(Item).FOperationHandle);
    if Assigned(OpManItem) then
      OpManItem.Operation.Stop;
  end;
end;

procedure TfrmViewOperations.cbCurrentQueueChange(Sender: TObject);
var
  Item: TViewBaseItem;
  NewQueue: TOperationsManagerQueueIdentifier;
begin
  Item := GetFocusedItem;
  if Assigned(Item) and (Item is TViewOperationItem) then
  begin
    if cbCurrentQueue.ItemIndex = 0 then
      NewQueue := FreeOperationsQueueId
    else
      NewQueue := cbCurrentQueue.ItemIndex;
    SetNewQueue(TViewOperationItem(Item), NewQueue);
  end;
end;

procedure TfrmViewOperations.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmViewOperations := nil;
end;

procedure TfrmViewOperations.FormDestroy(Sender: TObject);
begin
  OperationsManager.RemoveEventsListener([omevOperationAdded, omevOperationRemoved],
    @UpdateView);
end;

procedure TfrmViewOperations.mnuPutFirstInQueueClick(Sender: TObject);
var
  Item: TViewBaseItem;
  OpManItem: TOperationsManagerItem;
begin
  Item := GetFocusedItem;
  if Assigned(Item) and (Item is TViewOperationItem) then
  begin
    OpManItem := OperationsManager.GetItemByHandle(TViewOperationItem(Item).FOperationHandle);
    if Assigned(OpManItem) then
    begin
      if OpManItem.Queue.Identifier <> FreeOperationsQueueId then
      begin
        // TODO: Move within queue
      end;
    end;
  end;
end;

procedure TfrmViewOperations.mnuPutLastInQueueClick(Sender: TObject);
begin

end;

procedure TfrmViewOperations.mnuShowDetachedClick(Sender: TObject);
var
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(FMenuOperation);
  if Assigned(OpManItem) then
    TfrmFileOp.ShowFor(OpManItem.Handle);
end;

procedure TfrmViewOperations.OnOperationItemClick(Item: TViewBaseItem; Button: TMouseButton; Shift: TShiftState; const Pt: TPoint);
var
  OpManItem: TOperationsManagerItem;
  PopupPoint: TPoint;
begin
  case Button of
    mbLeft:
      if ssDouble in Shift then
      begin
        OpManItem := OperationsManager.GetItemByHandle((Item as TViewOperationItem).FOperationHandle);
        if Assigned(OpManItem) then
        begin
          if (OpManItem.Operation.State in [fsosStarting, fsosRunning, fsosWaitingForConnection]) then
            OpManItem.Operation.Pause
          else
            OpManItem.Operation.Start;
          OpManItem.PauseRunning := False;
        end;
      end;
    mbRight:
      begin
        FMenuOperation := (Item as TViewOperationItem).FOperationHandle;
        PopupPoint := tvOperations.ClientToScreen(Pt);
        pmOperationPopup.PopUp(PopupPoint.x, PopupPoint.y);
      end;
  end;
end;

procedure TfrmViewOperations.OnItemStatusIconClick(Item: TViewOperationItem);
var
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(Item.FOperationHandle);
  if Assigned(OpManItem) then
  begin
    if (OpManItem.Operation.State in [fsosStarting, fsosRunning, fsosWaitingForConnection]) then
      OpManItem.Operation.Pause
    else
      OpManItem.Operation.Start;
    OpManItem.PauseRunning := False;
  end;
end;

procedure TfrmViewOperations.OnUpdateTimer(Sender: TObject);
begin
  tvOperations.Invalidate;
end;

procedure TfrmViewOperations.mnuQueueNumberClick(Sender: TObject);
var
  NewQueueNumber: integer;
  Item: TViewBaseItem;
begin
  if TryStrToInt(Copy((Sender as TMenuItem).Name, 9, 1), NewQueueNumber) then
  begin
    Item := GetFocusedItem;
    if Assigned(Item) and (Item is TViewOperationItem) then
      SetNewQueue(TViewOperationItem(Item), NewQueueNumber);
  end;
end;

procedure TfrmViewOperations.tbPauseAllChange(Sender: TObject);
begin
  if tbPauseAll.State = cbChecked then
    OperationsManager.PauseRunning
  else
    OperationsManager.StartRunning;
end;

procedure TfrmViewOperations.tvOperationsCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Item: TViewBaseItem;
  NodeRect: TRect;

  procedure DrawExpandSign(MidX, MidY: integer; CollapseSign: boolean);
  const
    ExpandSignSize = 9;
    ExpandSignColor = clWindowText;
  var
    HalfSize, ALeft, ATop, ARight, ABottom: integer;
    Points: array [0..2] of TPoint;
    R: TRect;
  begin
    with Sender.Canvas do
    begin
      Pen.Color := ExpandSignColor;
      Pen.Style := psSolid;

      HalfSize := ExpandSignSize shr 1;
      if ((ExpandSignSize and 1) = 0) then
        dec(HalfSize);
      ALeft := MidX - HalfSize;
      ATop := MidY - HalfSize;
      ARight := ALeft + ExpandSignSize;
      ABottom := ATop + ExpandSignSize;

      // draw an arrow. down for collapse and right for expand
      R := Rect(ALeft, ATop, ARight, ABottom);
      if CollapseSign then
      begin
        // draw an arrow down
        Points[0] := Point(R.Left, MidY);
        Points[1] := Point(R.Right - 1, MidY);
        Points[2] := Point(MidX, R.Bottom - 1);
      end else
      begin
        // draw an arrow right
        Points[0] := Point(MidX - 1, ATop);
        Points[1] := Point(R.Right - 2, MidY);
        Points[2] := Point(MidX - 1, R.Bottom - 1);
      end;
      Polygon(Points, False);
    end;
  end;
var
  VertMid: Integer;
begin
  if not Assigned(Node.Data) then
    Exit;

  Item := TViewBaseItem(Node.Data);
  NodeRect := Node.DisplayRect(False);
  VertMid := (NodeRect.Top + NodeRect.Bottom) div 2;

  Item.Draw(Sender.Canvas, NodeRect);

  if tvOperations.ShowButtons and Node.HasChildren and ((tvoShowRoot in tvOperations.Options) or (Node.Parent <> nil)) then
    DrawExpandSign(Node.DisplayExpandSignLeft + tvOperations.Indent shr 1, VertMid, Node.Expanded);

  // draw separator
  if (tvoShowSeparators in tvOperations.Options) then
  begin
    Sender.Canvas.Pen.Color:=tvOperations.SeparatorColor;
    Sender.Canvas.MoveTo(NodeRect.Left,NodeRect.Bottom-1);
    Sender.Canvas.LineTo(NodeRect.Right,NodeRect.Bottom-1);
  end;

  DefaultDraw := False;
end;

procedure TfrmViewOperations.tvOperationsDeletion(Sender: TObject; Node: TTreeNode);
var
  Item: TViewBaseItem;
begin
  Item := TViewBaseItem(Node.Data);
  Node.Data := nil;
  Item.Free;
end;

procedure TfrmViewOperations.tvOperationsDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SourceNode, TargetNode: TTreeNode;
  NodeRect: TRect;
  TargetItem: TViewBaseItem;
  SourceItem: TViewOperationItem;
  SourceOpManItem, TargetOpManItem: TOperationsManagerItem;
  TargetQueue: TOperationsManagerQueue;
  HitTopPart: Boolean;
begin
  SourceNode := tvOperations.Selected;
  TargetNode := tvOperations.GetNodeAt(X, Y);
  if not Assigned(TargetNode) then
    TargetNode := tvOperations.Items.GetLastNode;
  if (Source = tvOperations) and Assigned(SourceNode) and Assigned(TargetNode) then
  begin
    SourceItem := TViewBaseItem(SourceNode.Data) as TViewOperationItem;
    SourceOpManItem := OperationsManager.GetItemByHandle(SourceItem.FOperationHandle);
    if Assigned(SourceOpManItem) then
    begin
      NodeRect := TargetNode.DisplayRect(False);
      TargetItem := TViewBaseItem(TargetNode.Data);

      HitTopPart := Y - NodeRect.Bottom < (NodeRect.Bottom - NodeRect.Top) div 2;

      if HitTopPart and
         (SourceNode = tvOperations.Items.GetFirstNode) and
         (TargetItem is TViewQueueItem) and
         (TViewQueueItem(TargetItem).FQueueIdentifier <> FreeOperationsQueueId) then
      begin
        TargetQueue := OperationsManager.QueueByIdentifier[FreeOperationsQueueId];
        SourceOpManItem.SetQueue(TargetQueue);
      end
      else if TargetItem is TViewQueueItem then
      begin
        TargetQueue := OperationsManager.QueueByIdentifier[TViewQueueItem(TargetItem).FQueueIdentifier];
        SourceOpManItem.SetQueue(TargetQueue);
      end
      else if TargetItem is TViewOperationItem then
      begin
        TargetOpManItem := OperationsManager.GetItemByHandle(TViewOperationItem(TargetItem).FOperationHandle);
        if Assigned(TargetOpManItem) then
        begin
          TargetQueue := TargetOpManItem.Queue;
          {if HitTopPart then
            AttachMode := naInsert
          else
            AttachMode := naInsertBehind;}
          SourceOpManItem.SetQueue(TargetQueue);
        end;
      end;
    end;
  end;
end;

procedure TfrmViewOperations.tvOperationsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TfrmViewOperations.tvOperationsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  NodeRect: TRect;
begin
  Node := tvOperations.GetNodeAt(X, Y);
  if Assigned(Node) then
  begin
    NodeRect := Node.DisplayRect(False);
    TViewBaseItem(Node.Data).Click(Point(X - NodeRect.Left, Y - NodeRect.Top), Button, Shift);
  end;
end;

procedure TfrmViewOperations.tvOperationsSelectionChanged(Sender: TObject);
begin
  UpdateControls;
end;

function TfrmViewOperations.GetFocusedItem: TViewBaseItem;
var
  Node: TTreeNode;
begin
  Node := tvOperations.Selected;
  if Assigned(Node) then
    Result := TViewBaseItem(Node.Data)
  else
    Result := nil;
end;

procedure TfrmViewOperations.SetFocusItem(AOperationHandle: TOperationHandle);
var
  Node: TTreeNode;
begin
  for Node in tvOperations.Items do
  begin
    if (TViewBaseItem(Node.Data) is TViewOperationItem) and
       (TViewOperationItem(Node.Data).FOperationHandle = AOperationHandle) then
    begin
      Node.Selected := True;
      Exit;
    end;
  end;
end;

procedure TfrmViewOperations.SetNewQueue(Item: TViewOperationItem; NewQueue: TOperationsManagerQueueIdentifier);
var
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(Item.FOperationHandle);
  if Assigned(OpManItem) then
    OpManItem.SetQueue(OperationsManager.GetOrCreateQueue(NewQueue));
end;

procedure TfrmViewOperations.btnStartPauseClick(Sender: TObject);
var
  OpManItem: TOperationsManagerItem;
  Item: TViewBaseItem;
begin
  Item := GetFocusedItem;
  if Assigned(Item) and (Item is TViewOperationItem) then
  begin
    OpManItem := OperationsManager.GetItemByHandle(TViewOperationItem(Item).FOperationHandle);
    if Assigned(OpManItem) then
    begin
      if (OpManItem.Operation.State in [fsosStarting, fsosRunning, fsosWaitingForConnection]) then
        OpManItem.Operation.Pause
      else
        OpManItem.Operation.Start;
      OpManItem.PauseRunning := False;
    end;
  end;
end;

procedure TfrmViewOperations.UpdateCounters;
var
  ActiveOperationsCount: Integer = 0;
  TotalOperationsCount: Integer = 0;
  OpManItem: TOperationsManagerItem;
  OperIndex, QueueIndex: Integer;
  Queue: TOperationsManagerQueue;
begin
  for QueueIndex := 0 to OperationsManager.QueuesCount - 1 do
  begin
    Queue := OperationsManager.QueueByIndex[QueueIndex];
    for OperIndex := 0 to Queue.Count - 1 do
    begin
      OpManItem := Queue.Items[OperIndex];
      if OpManItem.Operation.State in [fsosStarting, fsosRunning, fsosPausing, fsosStopping] then
        Inc(ActiveOperationsCount);
      Inc(TotalOperationsCount);
    end;
  end;
  lblActiveOperationsNumber.Caption := IntToStr(ActiveOperationsCount);
  lblOperationsCountNumber.Caption := IntToStr(TotalOperationsCount);
end;

procedure TfrmViewOperations.UpdateControls;
var
  i: integer;
  OpManItem: TOperationsManagerItem;
  Item: TViewBaseItem;
begin
  Item := GetFocusedItem;
  if Assigned(Item) and (Item is TViewOperationItem) then
  begin
    OpManItem := OperationsManager.GetItemByHandle(TViewOperationItem(Item).FOperationHandle);
    if Assigned(OpManItem) then
      begin
        for i := 0 to mnuQueue.Count - 1 do
          if (i = OpManItem.Queue.Identifier) then
            mnuQueue.Items[i].Checked:=True
          else
            mnuQueue.Items[i].Checked:=False;

        if (OpManItem.Operation.State in [fsosStarting, fsosRunning, fsosWaitingForConnection]) then
          btnStartPause.Caption := rsDlgOpPause
        else
          btnStartPause.Caption := rsDlgOpStart;

        If (not cbCurrentQueue.DroppedDown) and (cbCurrentQueue.ItemIndex <> OpManItem.Queue.Identifier) Then
          cbCurrentQueue.ItemIndex:=OpManItem.Queue.Identifier;

        cbCurrentQueue.Enabled := True;
        btnCancelCurOp.Enabled := True;
        mnuShowDetached.Enabled := True;
      end
    else
      begin
        btnStartPause.Enabled    := False;
        btnCancelCurOp.Enabled   := False;
        cbCurrentQueue.ItemIndex := -1;
        cbCurrentQueue.Enabled   := False;
        mnuShowDetached.Enabled   := False;
      end;
  end;
end;

procedure TfrmViewOperations.UpdateItems;
  procedure AddOperations(Queue: TOperationsManagerQueue; QueueNode: TTreeNode);
  var
    OperIndex: Integer;
    OpManItem: TOperationsManagerItem;
    OperNode: TTreeNode;
    Item: TViewBaseItem;
  begin
    for OperIndex := 0 to Queue.Count - 1 do
    begin
      OpManItem := Queue.Items[OperIndex];
      OperNode := tvOperations.Items.AddChild(QueueNode, '');
      Item := TViewOperationItem.Create(OperNode, OpManItem.Handle);
      OperNode.Data := Item;
      OperNode.Height := Item.GetHeight(tvOperations.Canvas);
      TViewOperationItem(Item).OnStatusIconClick := @OnItemStatusIconClick;
      Item.OnClick := @OnOperationItemClick;
    end;
  end;
var
  QueueIndex: Integer;
  Queue: TOperationsManagerQueue;
  QueueNode: TTreeNode;
  Item: TViewBaseItem;
begin
  tvOperations.Items.Clear;

  // First add all free operations.
  Queue := OperationsManager.QueueByIdentifier[FreeOperationsQueueId];
  if Assigned(Queue) then
    AddOperations(Queue, nil);

  for QueueIndex := 0 to OperationsManager.QueuesCount - 1 do
  begin
    Queue := OperationsManager.QueueByIndex[QueueIndex];
    if Queue.Identifier <> FreeOperationsQueueId then
    begin
      QueueNode := tvOperations.Items.AddChild(nil, '');
      Item := TViewQueueItem.Create(QueueNode, Queue.Identifier);
      QueueNode.Data := Item;
      QueueNode.Height := Item.GetHeight(tvOperations.Canvas);
      AddOperations(Queue, QueueNode);
    end;
  end;
end;

procedure TfrmViewOperations.UpdateView(Operation: TFileSourceOperation;
  Event: TOperationManagerEvent);
begin
  UpdateCounters;
  UpdateItems;
  tvOperations.Invalidate;
end;

end.

