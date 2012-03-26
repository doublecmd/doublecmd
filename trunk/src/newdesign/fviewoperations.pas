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
  TViewBaseItemContextMenu = procedure(Item: TViewBaseItem; const Point: TPoint) of object;
  TViewBaseItemSelected = procedure(Item: TViewBaseItem) of object;

  TViewOperationsStatusIcon = (vosiPlay, vosiPause, vosiHourglass);

  { TViewBaseItem }

  TViewBaseItem = class
  private
    FOnClick: TViewBaseItemClick;
    FOnContextMenu: TViewBaseItemContextMenu;
    FOnSelected: TViewBaseItemSelected;
    FTreeNode: TTreeNode;
    procedure DrawProgress(Canvas: TCanvas; NodeRect: TRect; Progress: Double);
    procedure DrawStatusIcon(Canvas: TCanvas; NodeRect: TRect; Icon: TViewOperationsStatusIcon);
    procedure DrawThemedBackground(Canvas: TCanvas; Element: TThemedTreeview; ARect: TRect);
    procedure DrawThemedText(Canvas: TCanvas; Element: TThemedTreeview; NodeRect: TRect; Center: Boolean; AText: String);
    function GetHeight: Integer;
    function GetStatusIconRect(NodeRect: TRect): TRect;
    function GetTextIndent: Integer;
  public
    constructor Create(ANode: TTreeNode); virtual;
    procedure Click(const Pt: TPoint; Button: TMouseButton; Shift: TShiftState); virtual;
    procedure Draw(Canvas: TCanvas; NodeRect: TRect); virtual; abstract;
    function GetBackgroundColor: TColor; virtual; abstract;
    function GetHeight(Canvas: TCanvas): Integer; virtual; abstract;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure Selected; virtual;
    procedure StartPause; virtual; abstract;
    procedure Stop; virtual; abstract;
    property Height: Integer read GetHeight;
    property OnClick: TViewBaseItemClick read FOnClick write FOnClick;
    property OnContextMenu: TViewBaseItemContextMenu read FOnContextMenu write FOnContextMenu;
    property OnSelected: TViewBaseItemSelected read FOnSelected write FOnSelected;
  end;

  { TViewQueueItem }

  TViewQueueItem = class(TViewBaseItem)
  private
    FQueueIdentifier: TOperationsManagerQueueIdentifier;
    FTextHeight: Integer;
  public
    constructor Create(ANode: TTreeNode; AQueueId: TOperationsManagerQueueIdentifier); reintroduce;
    procedure Click(const Pt: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure Draw(Canvas: TCanvas; NodeRect: TRect); override;
    function GetBackgroundColor: TColor; override;
    function GetHeight(Canvas: TCanvas): Integer; override;
    procedure StartPause; override;
    procedure Stop; override;
  end;

  TViewOperationItem = class;

  { TViewOperationItem }

  TViewOperationItem = class(TViewBaseItem)
  private
    FOperationHandle: TOperationHandle;
    FTextHeight: Integer;
  public
    constructor Create(ANode: TTreeNode; AOperationHandle: TOperationHandle); reintroduce;
    procedure Click(const Pt: TPoint; Button: TMouseButton; Shift: TShiftState); override;
    procedure Draw(Canvas: TCanvas; NodeRect: TRect); override;
    function GetHeight(Canvas: TCanvas): Integer; override;
    function GetBackgroundColor: TColor; override;
    procedure StartPause; override;
    procedure Stop; override;
  end;

  { TfrmViewOperations }

  TfrmViewOperations = class(TForm)
    btnStop: TBitBtn;
    btnStartPause: TBitBtn;
    mnuNewQueue: TMenuItem;
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
    pnlHeader: TPanel;
    pmOperationPopup: TPopupMenu;
    tbPauseAll: TToggleBox;
    tvOperations: TTreeView;
    UpdateTimer: TTimer;

    procedure btnStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuNewQueueClick(Sender: TObject);
    procedure mnuPutFirstInQueueClick(Sender: TObject);
    procedure mnuPutLastInQueueClick(Sender: TObject);
    procedure mnuShowDetachedClick(Sender: TObject);
    procedure OnOperationItemContextMenu(Item: TViewBaseItem; const Point: TPoint);
    procedure OnOperationItemSelected(Item: TViewBaseItem);
    procedure OnQueueItemSelected(Item: TViewBaseItem);
    procedure OnUpdateTimer(Sender: TObject);
    procedure btnStartPauseClick(Sender: TObject);
    procedure mnuQueueNumberClick(Sender: TObject);
    procedure tbPauseAllChange(Sender: TObject);
    procedure tvOperationsCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvOperationsDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvOperationsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvOperationsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tvOperationsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tvOperationsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tvOperationsSelectionChanged(Sender: TObject);
  private
    FDraggedOperation: TOperationHandle;
    FMenuOperation: TOperationHandle;
    function GetFocusedItem: TViewBaseItem;
    procedure MoveWithinQueue(MoveToTop: Boolean);
    procedure SetFocusItem(AOperationHandle: TOperationHandle);
    procedure SetFocusItem(AQueueIdentifier: TOperationsManagerQueueIdentifier);
    procedure SetNewQueue(Item: TViewOperationItem; NewQueue: TOperationsManagerQueueIdentifier);
    procedure SetStartPauseCaption(SetPause: Boolean);
    procedure UpdateView(Item: TOperationsManagerItem; Event: TOperationManagerEvent);
    procedure UpdateItems;
  end;

procedure ShowOperationsViewer;
procedure ShowOperationsViewer(AOperationHandle: TOperationHandle);
procedure ShowOperationsViewer(AQueueIdentifier: TOperationsManagerQueueIdentifier);

implementation

{$R *.lfm}

uses
  GraphMath, GraphType, Math,
  uLng, fFileOpDlg, uGlobs, LCLProc;

const
  ExpandSignSize = 9;
  StatusIconPlay: array[0..2] of TPoint =
    ((x: 5; y: 1), (x: 13; y: 9), (x: 5; y: 17));
  StatusIconPause1: array[0..3] of TPoint =
    ((x: 3; y: 2), (x: 3; y: 16), (x: 7; y: 16), (x: 7; y: 2));
  StatusIconPause2: array[0..3] of TPoint =
    ((x: 10; y: 2), (x: 10; y: 16), (x: 14; y: 16), (x: 14; y: 2));
  StatusIconHourglass1: array[0..2] of TPoint =
    ((x: 3; y: 4), (x: 13; y: 4), (x: 8; y: 10));
  StatusIconHourglass2: array[0..2] of TPoint =
    ((x: 8; y: 10), (x: 13; y: 15), (x: 3; y: 15));
  StatusIconFrame: TRect = (Left: 0; Top: 0; Right: 18; Bottom: 19);
  StatusIconRightMargin = 5;
  ProgressHeight = 16;
  ProgressWidth = 150;
  ProgressRightMargin = 5;

var
  frmViewOperations: TfrmViewOperations = nil;
  ProgressRight: Integer;

procedure ShowOperationsViewer;
begin
  if not Assigned(frmViewOperations) then
    frmViewOperations := TfrmViewOperations.Create(Application);
  frmViewOperations.ShowOnTop;
end;

procedure ShowOperationsViewer(AOperationHandle: TOperationHandle);
begin
  ShowOperationsViewer;
  if AOperationHandle <> InvalidOperationHandle then
    frmViewOperations.SetFocusItem(AOperationHandle);
end;

procedure ShowOperationsViewer(AQueueIdentifier: TOperationsManagerQueueIdentifier);
begin
  ShowOperationsViewer;
  frmViewOperations.SetFocusItem(AQueueIdentifier);
end;

procedure ApplyProgress(var ARect: TRect; Progress: Double);
begin
  ARect.Right := ARect.Left + Round((ARect.Right - ARect.Left) * Progress);
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

procedure TViewBaseItem.DrawProgress(Canvas: TCanvas; NodeRect: TRect; Progress: Double);
var
  Details: TThemedElementDetails;
begin
  if Progress > 0 then
  begin
    NodeRect.Right  := NodeRect.Right - ProgressRight;
    NodeRect.Left   := NodeRect.Right - ProgressWidth;
    NodeRect.Top    := NodeRect.Top + (NodeRect.Bottom - NodeRect.Top - ProgressHeight) div 2;
    NodeRect.Bottom := NodeRect.Top + ProgressHeight;

    if ThemeServices.ThemesEnabled then
    begin
      Details := ThemeServices.GetElementDetails(tpBar);
      ThemeServices.DrawElement(Canvas.Handle, Details, NodeRect, nil);
      Details := ThemeServices.GetElementDetails(tpChunk);
      InflateRect(NodeRect, -2, -2);
      ApplyProgress(NodeRect, Progress);
      ThemeServices.DrawElement(Canvas.Handle, Details, NodeRect, nil);
    end
    else
    begin
      Canvas.Pen.Color := clWindowText;
      Canvas.Brush.Color := clForm;
      Canvas.RoundRect(NodeRect, 3, 3);
      Canvas.Brush.Color := clHighlight;
      ApplyProgress(NodeRect, Progress);
      Canvas.RoundRect(NodeRect, 3, 3);
    end;
  end;
end;

procedure TViewBaseItem.DrawStatusIcon(Canvas: TCanvas; NodeRect: TRect; Icon: TViewOperationsStatusIcon);
var
  IconRect: TRect;
begin
  Canvas.Brush.Color := GetBackgroundColor;
  Canvas.Pen.Color := clWindowText;

  IconRect := MoveRect(GetStatusIconRect(NodeRect), NodeRect.Left, NodeRect.Top);
  Canvas.Rectangle(IconRect);

  case Icon of
    vosiPlay:     // Paint "Play" triangle
      begin
        Canvas.Brush.Color := RGBToColor(0, 200, 0);
        DrawMovePolygon(Canvas, StatusIconPlay, IconRect.Left, IconRect.Top);
      end;
    vosiPause:    // Paint "Pause" double line
      begin
        Canvas.Brush.Color := RGBToColor(0, 0, 200);
        DrawMovePolygon(Canvas, StatusIconPause1, IconRect.Left, IconRect.Top);
        DrawMovePolygon(Canvas, StatusIconPause2, IconRect.Left, IconRect.Top);
      end;
    else          // Paint "Hourglass"
      begin
        Canvas.Brush.Color := RGBToColor(255, 255, 255);
        DrawMovePolygon(Canvas, StatusIconHourglass1, IconRect.Left, IconRect.Top);
        DrawMovePolygon(Canvas, StatusIconHourglass2, IconRect.Left, IconRect.Top);
      end;
  end;
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

function TViewBaseItem.GetStatusIconRect(NodeRect: TRect): TRect;
begin
  Result := MoveRect(StatusIconFrame,
                     (NodeRect.Right - NodeRect.Left) - (StatusIconFrame.Right - StatusIconFrame.Left) - StatusIconRightMargin,
                     ((NodeRect.Bottom - NodeRect.Top) - (StatusIconFrame.Bottom - StatusIconFrame.Top)) div 2);
end;

function TViewBaseItem.GetTextIndent: Integer;
begin
  Result := FTreeNode.DisplayExpandSignLeft;
  if FTreeNode.Level = 0 then
    Inc(Result, TTreeView(FTreeNode.TreeView).Indent)
  else
    Dec(Result, TTreeView(FTreeNode.TreeView).Indent * (FTreeNode.Level - 1));
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

procedure TViewBaseItem.KeyDown(var Key: Word; Shift: TShiftState);
var
  Rect: TRect;
  Point: TPoint;
begin
  case Key of
    VK_APPS:
      if Assigned(FOnContextMenu) then
      begin
        Rect := FTreeNode.DisplayRect(False);
        Point.x := Rect.Left + (Rect.Right - Rect.Left) div 2;
        Point.y := Rect.Top + (Rect.Bottom - Rect.Top) div 2;
        OnContextMenu(Self, Point);
        Key := 0;
      end;
    VK_SPACE:
      begin
        StartPause;
        Key := 0;
      end;
    VK_DELETE, VK_BACK:
      begin
        Stop;
        Key := 0;
      end;
  end;
end;

procedure TViewBaseItem.Selected;
begin
  if Assigned(FOnSelected) then
    FOnSelected(Self);
end;

{ TViewOperationItem }

constructor TViewOperationItem.Create(ANode: TTreeNode; AOperationHandle: TOperationHandle);
begin
  FOperationHandle := AOperationHandle;
  inherited Create(ANode);
end;

procedure TViewOperationItem.Click(const Pt: TPoint; Button: TMouseButton; Shift: TShiftState);
var
  Handled: Boolean = False;
  OpManItem: TOperationsManagerItem;
  NodeRect: TRect;
begin
  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) then
  begin
    case Button of
      mbLeft:
        if OpManItem.Queue.IsFree then
        begin
          NodeRect := FTreeNode.DisplayRect(False);
          if ((ssDouble in Shift) or PtInRect(GetStatusIconRect(NodeRect), Pt)) then
          begin
            StartPause;
            Handled := True;
          end;
        end;
      mbRight:
        if Assigned(FOnContextMenu) then
        begin
          OnContextMenu(Self, Pt);
          Handled := True;
        end;
    end;
  end;
  if not Handled then
    inherited;
end;

procedure TViewOperationItem.Draw(Canvas: TCanvas; NodeRect: TRect);
var
  OutString: string;
  OpManItem: TOperationsManagerItem;
  aRect: TRect;
  Element: TThemedTreeview;
  Icon: TViewOperationsStatusIcon;
begin
  if FTreeNode.Selected then
    Element := ttItemSelected
  else
    Element := ttItemNormal;

  DrawThemedBackground(Canvas, Element, NodeRect);

  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) then
  begin
    OutString :=
      IntToStr(OpManItem.Handle) + ': ' +
      OpManItem.Operation.GetDescription([fsoddJob]) + ' - ' +
      FloatToStrF(OpManItem.Operation.Progress * 100, ffFixed, 1, 1) + ' %' +
      ' (' + FileSourceOperationStateText[OpManItem.Operation.State] + ')';

    aRect := NodeRect;
    if OpManItem.Queue.IsFree then
      aRect.Left := aRect.Left + 3
    else
      aRect.Left := aRect.Left + GetTextIndent;
    DrawThemedText(Canvas, Element, aRect, True, OutString);

    if OpManItem.Queue.IsFree then
    begin
      case OpManItem.Operation.State of
        fsosRunning:
          Icon := vosiPlay;
        fsosPaused:
          Icon := vosiPause;
        else
          Icon := vosiHourglass;
      end;
      DrawStatusIcon(Canvas, NodeRect, Icon);
    end;

    DrawProgress(Canvas, aRect, OpManItem.Operation.Progress);
  end;
end;

function TViewOperationItem.GetBackgroundColor: TColor;
begin
  Result := FTreeNode.TreeView.BackgroundColor;
end;

procedure TViewOperationItem.StartPause;
var
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) and OpManItem.Queue.IsFree then
  begin
    if OpManItem.Operation.State in [fsosStarting, fsosRunning, fsosWaitingForConnection] then
      OpManItem.Operation.Pause
    else
      OpManItem.Operation.Start;
  end;
end;

procedure TViewOperationItem.Stop;
var
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) then
    OpManItem.Operation.Stop;
end;

function TViewOperationItem.GetHeight(Canvas: TCanvas): Integer;
var
  OpManItem: TOperationsManagerItem;
begin
  FTextHeight := Canvas.TextExtent('Wg').cy;
  Result := Max(FTextHeight, ProgressHeight);
  OpManItem := OperationsManager.GetItemByHandle(FOperationHandle);
  if Assigned(OpManItem) then
  begin
    if OpManItem.Queue.IsFree then
      Result := Max(Result, StatusIconFrame.Bottom - StatusIconFrame.Top);
  end;
  Inc(Result, 4);
end;

{ TViewQueueItem }

procedure TViewQueueItem.Click(const Pt: TPoint; Button: TMouseButton; Shift: TShiftState);
var
  Handled: Boolean = False;
  NodeRect: TRect;
begin
  case Button of
    mbLeft:
      begin
        NodeRect := FTreeNode.DisplayRect(False);
        if (ssDouble in Shift) or PtInRect(GetStatusIconRect(NodeRect), Pt) then
        begin
          StartPause;
          Handled := True;
        end;
      end;
  end;
  if not Handled then
    inherited Click(Pt, Button, Shift);
end;

constructor TViewQueueItem.Create(ANode: TTreeNode; AQueueId: TOperationsManagerQueueIdentifier);
begin
  FQueueIdentifier := AQueueId;
  inherited Create(ANode);
end;

procedure TViewQueueItem.Draw(Canvas: TCanvas; NodeRect: TRect);
var
  Element: TThemedTreeview;
  OutString: string;
  Queue: TOperationsManagerQueue;
  aRect: TRect;
  AProgress: Double;
  Icon: TViewOperationsStatusIcon;
begin
  if FTreeNode.Selected then
    Element := ttItemSelected
  else
    Element := ttItemSelectedNotFocus;
  DrawThemedBackground(Canvas, Element, NodeRect);

  Queue := OperationsManager.QueueByIdentifier[FQueueIdentifier];
  if Assigned(Queue) then
  begin
    OutString := rsDlgQueue + ' ' + IntToStr(FQueueIdentifier);
    AProgress := Queue.Progress;
    if AProgress > 0 then
      OutString := OutString + ' - ' + FloatToStrF(AProgress * 100, ffFixed, 1, 1) + ' %';
    if Queue.Paused then
      OutString := OutString + ' (' + FileSourceOperationStateText[fsosPaused] + ')';

    aRect := NodeRect;
    aRect.Left := aRect.Left + GetTextIndent;
    DrawThemedText(Canvas, Element, aRect, True, OutString);

    if Queue.Paused then
      Icon := vosiPause
    else
      Icon := vosiPlay;
    DrawStatusIcon(Canvas, NodeRect, Icon);

    DrawProgress(Canvas, aRect, AProgress);
  end;
end;

function TViewQueueItem.GetBackgroundColor: TColor;
begin
  Result := FTreeNode.TreeView.BackgroundColor;
end;

function TViewQueueItem.GetHeight(Canvas: TCanvas): Integer;
begin
  FTextHeight := Canvas.TextExtent('Wg').cy;
  Result := 4 + Max(FTextHeight,
                    Max((StatusIconFrame.Bottom - StatusIconFrame.Top),
                        ProgressHeight));
end;

procedure TViewQueueItem.StartPause;
var
  Queue: TOperationsManagerQueue;
begin
  Queue := OperationsManager.QueueByIdentifier[FQueueIdentifier];
  if Assigned(Queue) then
  begin
    if Queue.Paused then
      Queue.UnPause
    else
      Queue.Pause;
  end;
end;

procedure TViewQueueItem.Stop;
var
  Queue: TOperationsManagerQueue;
begin
  Queue := OperationsManager.QueueByIdentifier[FQueueIdentifier];
  if Assigned(Queue) then
  begin
    Queue.Stop;
  end;
end;

{ TfrmViewOperations }

procedure TfrmViewOperations.FormCreate(Sender: TObject);
begin
  InitPropStorage(Self);

  FMenuOperation := InvalidOperationHandle;
  tvOperations.DoubleBuffered := True;
  DoubleBuffered := True;

  UpdateItems;

  OperationsManager.AddEventsListener(
    [omevOperationAdded, omevOperationRemoved, omevOperationMoved],
    @UpdateView);
end;

procedure TfrmViewOperations.btnStopClick(Sender: TObject);
var
  Item: TViewBaseItem;
begin
  Item := GetFocusedItem;
  if Assigned(Item) then
    Item.Stop;
end;

procedure TfrmViewOperations.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmViewOperations := nil;
end;

procedure TfrmViewOperations.FormDestroy(Sender: TObject);
begin
  OperationsManager.RemoveEventsListener(
    [omevOperationAdded, omevOperationRemoved, omevOperationMoved],
    @UpdateView);
end;

procedure TfrmViewOperations.mnuNewQueueClick(Sender: TObject);
var
  Item: TViewBaseItem;
  OpManItem: TOperationsManagerItem;
begin
  Item := GetFocusedItem;
  if Assigned(Item) and (Item is TViewOperationItem) then
  begin
    OpManItem := OperationsManager.GetItemByHandle(TViewOperationItem(Item).FOperationHandle);
    if Assigned(OpManItem) then
      OpManItem.MoveToNewQueue;
  end;
end;

procedure TfrmViewOperations.mnuPutFirstInQueueClick(Sender: TObject);
begin
  MoveWithinQueue(True);
end;

procedure TfrmViewOperations.mnuPutLastInQueueClick(Sender: TObject);
begin
  MoveWithinQueue(False);
end;

procedure TfrmViewOperations.mnuShowDetachedClick(Sender: TObject);
var
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(FMenuOperation);
  if Assigned(OpManItem) and OpManItem.Queue.IsFree then
    TfrmFileOp.ShowFor(OpManItem.Handle);
end;

procedure TfrmViewOperations.OnOperationItemContextMenu(Item: TViewBaseItem; const Point: TPoint);
var
  i: Integer;
  PopupPoint: TPoint;
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle((Item as TViewOperationItem).FOperationHandle);
  if Assigned(OpManItem) then
  begin
    for i := 0 to mnuQueue.Count - 1 do
      if i = OpManItem.Queue.Identifier then
        mnuQueue.Items[i].Checked := True
      else
        mnuQueue.Items[i].Checked := False;

    mnuShowDetached.Enabled := OpManItem.Queue.IsFree;
    FMenuOperation := (Item as TViewOperationItem).FOperationHandle;
    PopupPoint := tvOperations.ClientToScreen(Point);
    pmOperationPopup.PopUp(PopupPoint.x, PopupPoint.y);
  end;
end;

procedure TfrmViewOperations.OnOperationItemSelected(Item: TViewBaseItem);
var
  OpManItem: TOperationsManagerItem;
begin
  OpManItem := OperationsManager.GetItemByHandle(TViewOperationItem(Item).FOperationHandle);
  if Assigned(OpManItem) then
    begin
      SetStartPauseCaption(OpManItem.Operation.State in [fsosStarting, fsosRunning, fsosWaitingForConnection]);
      btnStartPause.Enabled := OpManItem.Queue.IsFree;
      btnStop.Enabled := True;
    end
  else
    begin
      btnStartPause.Enabled := False;
      btnStop.Enabled := False;
    end;
end;

procedure TfrmViewOperations.OnQueueItemSelected(Item: TViewBaseItem);
var
  Queue: TOperationsManagerQueue;
begin
  Queue := OperationsManager.QueueByIdentifier[TViewQueueItem(Item).FQueueIdentifier];
  if Assigned(Queue) then
    begin
      SetStartPauseCaption(not Queue.Paused);
      btnStartPause.Enabled := True;
    end
  else
    begin
      btnStartPause.Enabled := False;
    end;
  btnStop.Enabled := btnStartPause.Enabled;
end;

procedure TfrmViewOperations.OnUpdateTimer(Sender: TObject);
begin
  tvOperationsSelectionChanged(tvOperations);
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
    OperationsManager.PauseAll
  else
    OperationsManager.UnPauseAll;
end;

procedure TfrmViewOperations.tvOperationsCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Item: TViewBaseItem;
  NodeRect: TRect;

  procedure DrawExpandSign(MidX, MidY: integer; CollapseSign: boolean);
  const
    ExpandSignColor = clWindowText;
  var
    HalfSize, ALeft, ATop, ARight, ABottom: integer;
    Points: array [0..2] of TPoint;
    R: TRect;
  begin
    with Sender.Canvas do
    begin
      Brush.Color := clWindow;
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

  Item.Draw(Sender.Canvas, NodeRect);

  if tvOperations.ShowButtons and Node.HasChildren and ((tvoShowRoot in tvOperations.Options) or (Node.Parent <> nil)) then
  begin
    VertMid := (NodeRect.Top + NodeRect.Bottom) div 2;
    DrawExpandSign(Node.DisplayExpandSignLeft + tvOperations.Indent shr 1, VertMid, Node.Expanded);
  end;

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
  TargetNode: TTreeNode;
  NodeRect: TRect;
  TargetItem: TViewBaseItem;
  QueueItem: TViewQueueItem;
  OperItem: TViewOperationItem;
  SourceOpManItem, TargetOpManItem: TOperationsManagerItem;
  TargetQueue: TOperationsManagerQueue;
  TargetQueueId: TOperationsManagerQueueIdentifier;
  HitTopPart: Boolean;
begin
  if Source = tvOperations then
  begin
    SourceOpManItem := OperationsManager.GetItemByHandle(FDraggedOperation);
    if Assigned(SourceOpManItem) then
    begin
      TargetNode := tvOperations.GetNodeAt(X, Y);
      if not Assigned(TargetNode) then
      begin
        SourceOpManItem.MoveToNewQueue;
      end
      else
      begin
        NodeRect := TargetNode.DisplayRect(False);
        TargetItem := TViewBaseItem(TargetNode.Data);
        HitTopPart := Y - NodeRect.Top < (NodeRect.Bottom - NodeRect.Top) div 2;

        if TargetItem is TViewQueueItem then
        begin
          QueueItem := TViewQueueItem(TargetItem);
          if HitTopPart and
             (TargetNode = tvOperations.Items.GetFirstNode) and
             (QueueItem.FQueueIdentifier <> FreeOperationsQueueId) then
          begin
            // There are no free operations and item was dropped at the top of the list
            // on some queue. Create a free operations queue and move to it.
            TargetQueueId := FreeOperationsQueueId;
            TargetQueue := OperationsManager.GetOrCreateQueue(TargetQueueId);
          end
          else
          begin
            TargetQueueId := QueueItem.FQueueIdentifier;
            TargetQueue := OperationsManager.QueueByIdentifier[TargetQueueId];
          end;
          SourceOpManItem.SetQueue(TargetQueue);
        end
        else if (TargetItem is TViewOperationItem) and
                (FDraggedOperation <> TViewOperationItem(TargetItem).FOperationHandle) then
        begin
          OperItem := TViewOperationItem(TargetItem);
          TargetOpManItem := OperationsManager.GetItemByHandle(OperItem.FOperationHandle);
          if Assigned(TargetOpManItem) then
            SourceOpManItem.Move(TargetOpManItem.Handle, HitTopPart);
        end;
      end;
    end;
  end;
end;

procedure TfrmViewOperations.tvOperationsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TfrmViewOperations.tvOperationsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Item: TViewBaseItem;
begin
  Item := GetFocusedItem;
  if Assigned(Item) then
    Item.KeyDown(Key, Shift);
end;

procedure TfrmViewOperations.tvOperationsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  NodeRect: TRect;
begin
  FDraggedOperation := InvalidOperationHandle;
  Node := tvOperations.GetNodeAt(X, Y);
  if Assigned(Node) then
  begin
    NodeRect := Node.DisplayRect(False);
    TViewBaseItem(Node.Data).Click(Point(X - NodeRect.Left, Y - NodeRect.Top), Button, Shift);
    if TViewBaseItem(Node.Data) is TViewOperationItem then
      FDraggedOperation := TViewOperationItem(Node.Data).FOperationHandle;
  end;
end;

procedure TfrmViewOperations.tvOperationsSelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := tvOperations.Selected;
  if Assigned(Node) then
    TViewBaseItem(Node.Data).Selected;
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

procedure TfrmViewOperations.MoveWithinQueue(MoveToTop: Boolean);
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
        if MoveToTop then
          OpManItem.MoveToTop
        else
          OpManItem.MoveToBottom;
      end;
    end;
  end;
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

procedure TfrmViewOperations.SetFocusItem(AQueueIdentifier: TOperationsManagerQueueIdentifier);
var
  Node: TTreeNode;
begin
  for Node in tvOperations.Items do
  begin
    if (TViewBaseItem(Node.Data) is TViewQueueItem) and
       (TViewQueueItem(Node.Data).FQueueIdentifier = AQueueIdentifier) then
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

procedure TfrmViewOperations.SetStartPauseCaption(SetPause: Boolean);
begin
  if SetPause then
    btnStartPause.Caption := rsDlgOpPause
  else
    btnStartPause.Caption := rsDlgOpStart;
end;

procedure TfrmViewOperations.btnStartPauseClick(Sender: TObject);
var
  Item: TViewBaseItem;
begin
  Item := GetFocusedItem;
  if Assigned(Item) then
    Item.StartPause;
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
      Item.OnContextMenu := @OnOperationItemContextMenu;
      Item.OnSelected := @OnOperationItemSelected;
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
      Item.OnSelected := @OnQueueItemSelected;
      AddOperations(Queue, QueueNode);
    end;
  end;
end;

procedure TfrmViewOperations.UpdateView(Item: TOperationsManagerItem; Event: TOperationManagerEvent);
begin
  UpdateItems;
  tvOperations.Invalidate;
end;

initialization
  ProgressRight := ProgressRightMargin + StatusIconRightMargin + (StatusIconFrame.Right - StatusIconFrame.Left);

end.

