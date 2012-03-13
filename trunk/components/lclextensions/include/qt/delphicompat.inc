
{
  Qt Interface
  
  Initial implementation by Zeljan Rikalo

  SetTimer/KillTimer implementation by Luiz Americo
}

function CF_UNICODETEXT: TClipboardFormat;
begin
  //todo
  Result := TClipboardFormat(0);
end;

{$define HAS_GETBKCOLOR}
{$define HAS_GETCURRENTOBJECT}
{$define HAS_INVERTRECT}
{$define HAS_GETTEXTEXTENTEXPOINT}
{$define HAS_GETDOUBLECLICKTIME}
{$define HAS_GETTEXTALIGN}
{$define HAS_GETWINDOWDC}
{$define HAS_OFFSETRGN}
{$define HAS_REDRAWWINDOW}
{$define HAS_SCROLLWINDOW}
{$define HAS_SETBRUSHORGEX}


{$i ../generic/stubs.inc}
{$i ../generic/independentfunctions.inc}
{$i ../generic/unicodefunctions.inc}

function GetBkColor(DC:HDC):COLORREF;
var
  Color: PQColor;
begin
  if QtWidgetSet.IsValidDC(DC) then
  begin
    Color := TQtDeviceContext(DC).BackgroundBrush.getColor;
    TQColorToColorRef(Color^, Result);
  end else
    Result := CLR_INVALID;
end;

function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc,
  YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width,
    Height, ROP);
end;

function GetCurrentObject(hdc: HDC; uObjectType: UINT): HGDIOBJ;
begin
  Result := 0;
  if QtWidgetSet.IsValidDC(hdc) then
  with TQtDeviceContext(hdc) do
  begin      {TODO: FIXME}
    case uObjectType of
      OBJ_BITMAP: Result := HGDIOBJ(vImage);
      OBJ_BRUSH: Result := HGDIOBJ(vBrush);
      OBJ_FONT: Result := HGDIOBJ(vFont);
      OBJ_PEN: Result := HGDIOBJ(vPen);
    end;
  end;
end;

function GetDoubleClickTime: UINT;
begin
  Result := QApplication_doubleClickInterval;
end;

function GetTextExtentExPoint(DC: HDC; Str: PChar;
  Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger;
  var Size: TSize): BOOL;
var
  lbearing, rbearing, ascent, descent: LongInt;
  IsDBCSFont: Boolean;
  NewCount, Accumulator, i: Integer;
  AStr: WideString;
begin
  //based in lcl code
  Result := QtWidgetSet.IsValidDC(DC);
  if Result then
  with TQtDeviceContext(DC) do
  begin
    AStr := WideString(Str);
    Ascent := Font.Metrics.ascent;
    Descent := Font.Metrics.descent;
    Size.cX := Font.Metrics.width(@AStr, Count);
    Size.cY := Ascent + Descent;
    if PartialWidths <> nil then
    begin
      Accumulator := 0;
      for i:= 0 to Count - 1 do
      begin
        Inc(Accumulator, QFontMetrics_width(Font.Metrics.Widget, PWideChar((Str+i))));
        PartialWidths[i] := Accumulator;
      end;
    end;
  end;
end;

function GetTextAlign(hDC:HDC): LongWord;
var
  QtDC: TQtDeviceContext;
  QtFontMetrics: QFontMetricsH;
  QtFont: QFontH;
begin
  Result := 0;
  if not QtWidgetSet.IsValidDC(hdC) then
    Exit;
  QtDC := TQtDeviceContext(hDC);
  QtFont := QtDC.vFont.Widget;
  QtFontMetrics := QFontMetrics_create(QtFont);
  try
  {TODO: FIXME we should save somehow text flags into QtDC
   cause we don't have any function which returns current flags !}
  finally
    QFontMetrics_destroy(QtFontMetrics);
  end;
end;

function GetWindowDC(hWnd:THandle): HDC;
begin
  Result := LCLIntf.GetDC(hWnd);
end;

function InvertRect(DC: HDC; const lprc: TRect): Boolean;
var
  DCOrigin: TQtPoint;
begin
  //todo: see the windows result when rect is invalid
  Result := QtWidgetSet.IsValidDC(DC) and (lprc.Bottom > lprc.Top)
    and (lprc.Right > lprc.Left);
  if Result then
  begin
    with lprc do
      Result := BitBlt(DC, Left, Top, Right - Left, Bottom-Top,
        DC, Left, Top, LongWord(QPainterCompositionMode_DestinationOver));
    {TODO: FIXME !}
  end;
end;

function OffsetRgn(hrgn:HRGN; nxOffset, nYOffset:longint):longint;
var
  Region: TQtRegion;
begin
  Region := TQtRegion(hrgn);
  QRegion_translate(Region.Widget, nxOffset, nYOffset);
  Result := Region.GetRegionType;
end;

function RedrawWindow(hWnd:THandle; lprcUpdate:PRECT; hrgnUpdate:HRGN; flags:LongWord):BOOLEAN;
var
  QtWidget: TQtWidget absolute hWnd;
  Region: TQtRegion absolute hrgnUpdate;
begin
  {TODO: Check msdn docs for flags ... this implementation could raise AV !}
  if (lprcUpdate = nil) and (hrgnUpdate = 0) then
    QWidget_update(QtWidget.Widget)
  else
  begin
    if lprcUpdate <> nil then
      InvalidateRect(hWnd, lprcUpdate, False)
    else
      QWidget_update(QtWidget.Widget, Region.Widget);
  end;
end;

function ScrollWindow(hWnd:THandle; XAmount, YAmount:longint;lpRect:PRECT; lpClipRect:PRECT): Boolean;
begin
  if hWnd = 0 then
    Exit;
  QWidget_scroll(TQtWidget(hWnd).Widget, XAmount, YAmount, lpRect);
end;

function SetBrushOrgEx(DC:HDC; nXOrg, nYOrg:longint; lppt:PPOINT):Boolean;
var
  QtDC: TQtDeviceContext;
begin
  Result := False;
  if not QtWidgetSet.IsValidDC(DC) then
    Exit;
  QtDC := TQtDeviceContext(DC);
  if lppt <> nil then
    QtDC.getBrushOrigin(lppt);
  QtDC.setBrushOrigin(nXorg, nYOrg);
  Result := True;
end;


type

  { TQtTimerEx }

  TQtTimerEx = class(TQtObject)
  private
    FTimerHook: QTimer_hookH;
    FCallbackFunc: TTimerNotify;
    FUserId: LongWord;
    FControl: TControl;
    FAppObject: QObjectH;
  public
    constructor Create(TimerFunc: TTimerNotify;
      UserId: LongWord; Control: TControl); virtual;
    destructor Destroy; override;
    procedure AttachEvents; override;
    procedure DetachEvents; override;
    procedure signalTimeout; cdecl;
  public
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
    procedure Reset(TimerFunc: TTimerNotify; UserId: LongWord; Control: TControl);
    procedure Start(Interval: Integer);
    procedure Stop;
  end;


  { TTimerList }

  TTimerList = class
  private
    FMap: TMap;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(hWnd: THandle; ID: LongWord; NotifyFunc: TTimerNotify;
      WinControl: TControl): TQtTimerEx;
    function GetTimerObject(hWnd: THandle; ID: LongWord): TQtTimerEx;
  end;

  TQtWidgetSetHack = Class(TWidgetSet)
  private
    App: QApplicationH;
  end;

{ TQtTimerEx }

constructor TQtTimerEx.Create(TimerFunc: TTimerNotify;
  UserId: LongWord; Control: TControl);
var
  AName: WideString;
begin
  inherited Create;
  FDeleteLater := True;
  FAppObject := TQtWidgetSetHack(QtWidgetSet).App;
  FCallbackFunc := TimerFunc;
  FUserId := UserId;
  FControl := Control;
  
  //very big ultra extreme hack to get the app from QtWidgetset
  TheObject := QTimer_create(FAppObject);
  AName := 'tqttimerex';
  QObject_setObjectName(TheObject, @AName);

  AttachEvents;
end;

destructor TQtTimerEx.Destroy;
begin
  FCallbackFunc := nil;
  FControl := nil;
  inherited Destroy;
end;

procedure TQtTimerEx.AttachEvents;
begin
  FTimerHook := QTimer_hook_create(QTimerH(TheObject));
  QTimer_hook_hook_timeout(FTimerHook, @signalTimeout);
  inherited AttachEvents;
end;

procedure TQtTimerEx.DetachEvents;
begin
  QTimer_stop(QTimerH(TheObject));
  if FTimerHook <> nil then
    QTimer_hook_destroy(FTimerHook);
  inherited DetachEvents;
end;

procedure TQtTimerEx.signalTimeout; cdecl;
var
  FCheckHandle: Boolean;
begin
  if Assigned(FCallbackFunc) then
    FCallbackFunc(FUserID)
  else
  if Assigned(FControl) then
  begin
    if not (csLoading in FControl.ComponentState) then
    if not (csDestroying in FControl.ComponentState) or not
    (csDestroyingHandle in FControl.ControlState) then
    begin
      FCheckHandle := True;
      LCLSendTimerMsg(FControl, FUserId, 0);
    end;
  end;
end;

function TQtTimerEx.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False;
  QEvent_accept(Event);
end;

procedure TQtTimerEx.Reset(TimerFunc: TTimerNotify;
  UserId: LongWord; Control: TControl);
begin
  FControl := Control;
  FCallbackFunc := TimerFunc;
  FUserId := UserId;
end;

procedure TQtTimerEx.Start(Interval: Integer);
begin
  QTimer_start(QTimerH(TheObject), Interval);
end;

procedure TQtTimerEx.Stop;
begin
  QTimer_stop(QTimerH(TheObject));
end;

var
  FTimerList: TTimerList;

function MakeQWord(d1, d2: dword): QWord; inline;
begin
  Result := (QWord(d2) shl 32) or d1;
end;

  
function KillTimer(hWnd: THandle; nIDEvent: LongWord):Boolean;
var
  TimerObject: TQtTimerEx;
begin
  TimerObject := FTimerList.GetTimerObject(hWnd, nIDEvent);
  if TimerObject <> nil then
  begin
    // DebugLn('KillTimer HWnd: %d ID: %d TimerObject: %d',[hWnd, nIDEvent, PtrInt(TimerObject)]);
    TimerObject.Stop;
  end;
end;

function SetTimer(hWnd: THandle; nIDEvent: LongWord; uElapse: LongWord; lpTimerFunc: TTimerNotify): LongWord;
var
  Control: TControl;
  TimerObject: TQtTimerEx;
begin
  if hWnd <> 0 then
    Control := FindControl(hWnd)
  else
    Control := nil;
  TimerObject := FTimerList.Add(hWnd, nIDEvent, lpTimerFunc, Control);
  try
    TimerObject.Start(uElapse);
    if hWnd = 0 then
      Result := PtrInt(TimerObject)
    else
      Result := nIdEvent;
  except
    Result := 0;
  end;
  //DebugLn('SetTimer HWnd: %d ID: %d TimerObject: %d',[hWnd, nIDEvent, PtrInt(TimerObject)]);
end;

function TTimerList.Add(hWnd: THandle; ID: LongWord; NotifyFunc: TTimerNotify; WinControl: TControl): TQtTimerEx;
var
  AID: QWord;
begin
  AID := MakeQWord(hWnd, ID);
  with FMap do
  begin
    if HasId(AID) then
    begin
      // DebugLn('Reset timer for HWnd: %d ID: %d AID: %d', [hWnd, ID, AID]);
      GetData(AID, Result);
      Result.Reset(NotifyFunc, ID, WinControl);
    end
    else
    begin
      // DebugLn('Create timer for HWnd: %d ID: %d AID: %d', [hWnd, ID, AID]);
      Result := TQtTimerEx.Create(NotifyFunc, ID, WinControl);
      Add(AID, Result);
    end;
  end;
end;

constructor TTimerList.Create;
begin
  //todo: handle 64bit
  FMap := TMap.Create(itu8, SizeOf(TQtTimerEx));
end;

destructor TTimerList.Destroy;
var
  Iterator: TMapIterator;
  TimerObject: TQtTimerEx;
begin
  Iterator := TMapIterator.Create(FMap);
  with Iterator do
  begin
    while not EOM do
    begin
      GetData(TimerObject);
      TimerObject.Free;
      Next;
    end;
    Destroy;
  end;
  FMap.Destroy;
end;

function TTimerList.GetTimerObject(hWnd: THandle; ID: LongWord): TQtTimerEx;
var
  DataPtr: ^TQtTimerEx;
  AID: QWord;
begin
  Result := nil;
  AID := MakeQWord(hWnd, ID);
  // DebugLn('GetTimerObject for HWnd: %d ID: %d AID: %d', [hWnd, ID, AID]);
  DataPtr := FMap.GetDataPtr(AID);
  if DataPtr <> nil then
    Result := DataPtr^;
end;

