{
   Double Commander
   -------------------------------------------------------------------------
   Virtual terminal emulator control

   Alexander Koblov, 2021-2022

   Based on ComPort Library
     https://sourceforge.net/projects/comport
   Author:
     Dejan Crnila, 1998 - 2002
   Maintainers:
     Lars B. Dybdahl, 2003
     Brian Gochnauer, 2010
   License:
     Public Domain
}

unit VTEmuCtl;

{$mode delphi}
{$pointermath on}

interface

uses
  LCLType, Classes, Controls, StdCtrls, ExtCtrls, Forms, Messages, Graphics,
  VTEmuEsc, LCLIntf, Types, LazUtf8, LMessages;

type

  TOnRxBuf = procedure(Sender: TObject; const Buffer; Count: Integer) of object;

  { TCustomPtyDevice }

  TCustomPtyDevice = class(TComponent)
  protected
    FOnRxBuf: TOnRxBuf;
    FConnected: Boolean;
  protected
    procedure SetConnected(AValue: Boolean); virtual; abstract;
  public
    function WriteStr(const Str: string): Integer; virtual; abstract;
    function SetCurrentDir(const Path: String): Boolean; virtual; abstract;
    function SetScreenSize(aCols, aRows: Integer): Boolean; virtual; abstract;
    property OnRxBuf: TOnRxBuf read FOnRxBuf write FOnRxBuf;
    property Connected: Boolean read FConnected write SetConnected default False;
  end;

  TCustomComTerminal = class;  // forward declaration

  // terminal character
  PComTermChar = ^TComTermChar;
  TComTermChar = record
    Ch: TUTF8Char;
    FrontColor: TColor;
    BackColor: TColor;
    Underline: Boolean;
    Bold: Boolean;
  end;

  // buffer which holds terminal screen data
  TComTermBuffer = class
  private
    FBuffer: PByte;
    FTabs: Pointer;
    FTopLeft: TPoint;
    FCaretPos: TPoint;
    FScrollRange: TRect;
    FOwner: TCustomComTerminal;
  strict private
    FRows: Integer;
    FColumns: Integer;
  public
    constructor Create(AOwner: TCustomComTerminal);
    destructor Destroy; override;
    procedure Init(ARows, AColumns: Integer);
    procedure SetChar(Column, Row: Integer; TermChar: TComTermChar);
    function GetChar(Column, Row: Integer): TComTermChar;
    procedure SetTab(Column: Integer; Put: Boolean);
    function GetTab(Column: Integer): Boolean;
    function NextTab(Column: Integer): Integer;
    procedure ClearAllTabs;
    procedure ScrollDown;
    procedure ScrollUp;
    procedure EraseScreen(Column, Row: Integer);
    procedure EraseLineLeft(Column, Row: Integer);
    procedure EraseLineRight(Column, Row: Integer);
    procedure EraseChar(Column, Row, Count: Integer);
    procedure DeleteLine(Row, Count: Integer);
    procedure InsertLine(Row, Count: Integer);
    function GetLineLength(Line: Integer): Integer;
    function GetLastLine: Integer;
    property Rows: Integer read FRows;
    property Columns: Integer read FColumns;
  end;

  // terminal types
  TTermEmulation = (teVT100orANSI, teVT52, teNone);
  TTermCaret = (tcBlock, tcUnderline, tcNone);
  TAdvanceCaret = (acChar, acReturn, acLineFeed, acReverseLineFeed,
    acTab, acBackspace, acPage);
  TArrowKeys = (akTerminal, akWindows);
  TTermAttributes = record
    FrontColor: TColor;
    BackColor: TColor;
    Invert: Boolean;
    Bold: Boolean;
    Underline: Boolean;
  end;
  TTermMode = record
    Keys: TArrowKeys;
    MouseMode: Boolean;
    MouseTrack: Boolean;
  end;

  TEscapeEvent = procedure(Sender: TObject; var EscapeCodes: TEscapeCodes) of object;
  TUnhandledEvent = procedure(Sender: TObject; Code: TEscapeCode; Data: string) of object;
  TUnhandledModeEvent = procedure(Sender: TObject; const Data: string; OnOff: Boolean) of object;
  TStrRecvEvent = procedure(Sender: TObject; var Str: string) of object;
  TChScreenEvent = procedure(Sender: TObject; Ch: TUTF8Char) of object;

  // communication terminal control

  { TCustomComTerminal }

  TCustomComTerminal = class(TCustomControl)
  private
    FPtyDevice: TCustomPtyDevice;
    FScrollBars: TScrollStyle;
    FArrowKeys: TArrowKeys;
    FWantTab: Boolean;
    FColumns: Integer;
    FRows: Integer;
    FVisibleRows: Integer;
    FLocalEcho: Boolean;
    FSendLF: Boolean;
    FAppendLF: Boolean;
    FForce7Bit: Boolean;
    FWrapLines: Boolean;
    FSmoothScroll: Boolean;
    FAutoFollow : Boolean;
    FFontHeight: Integer;
    FFontWidth: Integer;
    FEmulation: TTermEmulation;
    FCaret: TTermCaret;
    FCaretPos: TPoint;
    FSaveCaret: TPoint;
    FCaretCreated: Boolean;
    FTopLeft: TPoint;
    FCaretHeight: Integer;
    FSaveAttr: TTermAttributes;
    FBuffer: TComTermBuffer;
    FMainBuffer: TComTermBuffer;
    FAlternateBuffer: TComTermBuffer;
    FParams: TStrings;
    FEscapeCodes: TEscapeCodes;
    FTermAttr: TTermAttributes;
    FTermMode: TTermMode;
    FOnChar: TChScreenEvent;
    FOnGetEscapeCodes: TEscapeEvent;
    FOnUnhandledCode: TUnhandledEvent;
    FOnUnhandledMode: TUnhandledModeEvent;
    FOnStrRecieved: TStrRecvEvent;
    procedure AdvanceCaret(Kind: TAdvanceCaret);
    function CalculateMetrics: Boolean;
    procedure CreateEscapeCodes;
    procedure CreateTerminalCaret;
    procedure DrawChar(AColumn, ARow: Integer; Ch: TComTermChar);
    function GetCharAttr: TComTermChar;
    function GetConnected: Boolean;
    procedure HideCaret;
    procedure InitCaret;
    procedure InvalidatePortion(ARect: TRect);
    procedure ModifyScrollBar(ScrollBar, ScrollCode, Pos: Integer);
    procedure SetColumns(const Value: Integer);
    procedure SetPtyDevice(const Value: TCustomPtyDevice);
    procedure SetConnected(const Value: Boolean);
    procedure SetEmulation(const Value: TTermEmulation);
    procedure SetRows(const Value: Integer);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetCaret(const Value: TTermCaret);
    procedure SetAttributes(AParams: TStrings);
    procedure SetMode(AParams: TStrings; OnOff: Boolean);
    procedure ShowCaret;
    procedure StringReceived(Str: string);
    procedure PaintTerminal(Rect: TRect);
    procedure PaintDesign;
    procedure PutChar(Ch: TUTF8Char);
    function PutEscapeCode(ACode: TEscapeCode; AParams: TStrings): Boolean;
    procedure RestoreAttr;
    procedure RestoreCaretPos;
    procedure RxBuf(Sender: TObject; const Buffer; Count: Integer);
    procedure SaveAttr;
    procedure SaveCaretPos;
    procedure SendChar(Ch: TUTF8Char);
    procedure SendCode(Code: TEscapeCode; AParams: TStrings);
    procedure SendCodeNoEcho(Code: TEscapeCode; AParams: TStrings);
    procedure MouseEvent(Code: TEscapeCode; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PerformTest(ACh: Char);
    procedure UpdateScrollPos;
    procedure UpdateScrollRange;
    procedure WrapLine;
  protected
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMLButtonDown(var Message: TLMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure DoChar(Ch: TUTF8Char); dynamic;
    procedure DoGetEscapeCodes(var EscapeCodes: TEscapeCodes); dynamic;
    procedure DoStrRecieved(var Str: string); dynamic;
    procedure DoUnhandledCode(Code: TEscapeCode; Data: string); dynamic;
    procedure DoUnhandledMode(const Data: string; OnOff: Boolean); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearScreen;
    procedure MoveCaret(AColumn, ARow: Integer);
    procedure Write(const Buffer:string; Size: Integer);
    procedure WriteStr(const Str: string);
    procedure WriteEscCode(ACode: TEscapeCode; AParams: TStrings);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure SelectFont;
    property AppendLF: Boolean read FAppendLF write FAppendLF default False;
    property AutoFollow : Boolean read FAutoFollow write FAutoFollow default True;
    property ArrowKeys: TArrowKeys read FArrowKeys write FArrowKeys default akTerminal;
    property Caret: TTermCaret read FCaret write SetCaret default tcBlock;
    property Connected: Boolean read GetConnected write SetConnected stored False;
    property PtyDevice: TCustomPtyDevice read FPtyDevice write SetPtyDevice;
    property Columns: Integer read FColumns write SetColumns default 80;
    property Emulation: TTermEmulation read FEmulation write SetEmulation;
    property EscapeCodes: TEscapeCodes read FEscapeCodes;
    property Force7Bit: Boolean read FForce7Bit write FForce7Bit default False;
    property LocalEcho: Boolean read FLocalEcho write FLocalEcho default False;
    property SendLF: Boolean read FSendLF write FSendLF default False;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
    property SmoothScroll: Boolean read FSmoothScroll write FSmoothScroll default False;
    property Rows: Integer read FRows write SetRows default 24;
    property WantTab: Boolean read FWantTab write FWantTab default False;
    property WrapLines: Boolean read FWrapLines write FWrapLines default False;
    property OnChar: TChScreenEvent read FOnChar write FOnChar;
    property OnGetEscapeCodes: TEscapeEvent
      read FOnGetEscapeCodes write FOnGetEscapeCodes;
    property OnStrRecieved: TStrRecvEvent
      read FOnStrRecieved write FOnStrRecieved;
    property OnUnhandledMode: TUnhandledModeEvent
      read FOnUnhandledMode write FOnUnhandledMode;
    property OnUnhandledCode: TUnhandledEvent
      read FOnUnhandledCode write FOnUnhandledCode;
  end;

  // publish properties
  TVirtualTerminal = class(TCustomComTerminal)
  published
    property Align;
    property AppendLF;
    property ArrowKeys;
    property BorderStyle;
    property Color;
    property Columns;
    property PtyDevice;
    property Connected;
    property DragCursor;
    property DragMode;
    property Emulation;
    property Enabled;
    property Font;
    property Force7Bit;
    property Hint;
    property LocalEcho;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Rows;
    property ScrollBars;
    property SendLF;
    property ShowHint;
    property SmoothScroll;
    property TabOrder;
    property TabStop default True;
    property Caret;
    property Visible;
    property WantTab;
    property WrapLines;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragKind;
    property OnChar;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetEscapeCodes;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnStrRecieved;
    property OnUnhandledCode;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnEndDock;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnUnDock;
    property OnContextPopup;
  end;

implementation

uses
  SysUtils, Dialogs, Math, VTColorTable;

const
  TMPF_FIXED_PITCH = $01;

(*****************************************
 * TComTermBuffer class                  *
 *****************************************)

// create class
constructor TComTermBuffer.Create(AOwner: TCustomComTerminal);
begin
  inherited Create;
  FOwner := AOwner;
  FTopLeft := Classes.Point(1, 1);
  FCaretPos := Classes.Point(1, 1);
end;

// destroy class
destructor TComTermBuffer.Destroy;
begin
  if FBuffer <> nil then
  begin
    FreeMem(FBuffer);
    FreeMem(FTabs);
  end;
  inherited Destroy;
end;

// put char in buffer
procedure TComTermBuffer.SetChar(Column, Row: Integer;
  TermChar: TComTermChar);
var
  Address: Integer;
begin
  Address := (Row - 1) * FColumns + Column - 1;
  Move(
    TermChar,
    (FBuffer + (SizeOf(TComTermChar) * Address))^,
    SizeOf(TComTermChar));
end;

// get char from buffer
function TComTermBuffer.GetChar(Column, Row: Integer): TComTermChar;
var
  Address: Integer;
begin
  Address := (Row - 1) * FColumns + Column - 1;
  Move(
    (FBuffer + (SizeOf(TComTermChar) * Address))^,
    Result,
    SizeOf(TComTermChar));
end;

// scroll down up line
procedure TComTermBuffer.ScrollDown;
var
  BytesToMove: Integer;
  SourceAddr: Pointer;
  ScrollRect: TRect;
begin
  BytesToMove := (FRows - 1) * FColumns * SizeOf(TComTermChar);
  SourceAddr := (FBuffer + FColumns * SizeOf(TComTermChar));
  // scroll in buffer
  Move(SourceAddr^, FBuffer^, BytesToMove);
  SourceAddr := (FBuffer +
    (FRows - 1) * FColumns * SizeOf(TComTermChar));
  FillChar(SourceAddr^, FColumns * SizeOf(TComTermChar), 0);
  // calculate scrolling rectangle
  with ScrollRect do
  begin
    Left := 0;
    Right := Min(FOwner.ClientWidth, FColumns * FOwner.FFontWidth);
    Top := 0;
    Bottom := Min(FOwner.ClientHeight, FRows * FOwner.FFontHeight);
  end;
  // scroll on screen
  if FOwner.DoubleBuffered then
    FOwner.Invalidate
  else
    ScrollWindowEx(FOwner.Handle, 0, -FOwner.FFontHeight,
      @ScrollRect, nil, 0, nil, SW_INVALIDATE or SW_ERASE);
end;

// scroll up one line
procedure TComTermBuffer.ScrollUp;
var
  BytesToMove: Integer;
  DestAddr: Pointer;
  ScrollRect: TRect;
begin
  BytesToMove := (FRows - 1) * FColumns * SizeOf(TComTermChar);
  DestAddr := (FBuffer + FColumns * SizeOf(TComTermChar));
  // scroll in buffer
  Move(FBuffer^, DestAddr^, BytesToMove);
  FillChar(FBuffer^, FColumns * SizeOf(TComTermChar), 0);
  // calculate scrolling rectangle
  with ScrollRect do
  begin
    Left := 0;
    Right := Min(FOwner.ClientWidth, FColumns * FOwner.FFontWidth);
    Top := 0;
    Bottom := Min(FOwner.ClientHeight, FRows * FOwner.FFontHeight);
  end;
  // scroll on screen
  if FOwner.DoubleBuffered then
    FOwner.Invalidate
  else
    ScrollWindowEx(FOwner.Handle, 0, FOwner.FFontHeight,
      @ScrollRect, nil, 0, nil, SW_INVALIDATE or SW_ERASE);
end;

procedure TComTermBuffer.EraseLineLeft(Column, Row: Integer);
var
  Index: Integer;
  Count: Integer;
  B: PComTermChar;
begin
  // in memory
  Count:= (Column + 1);
  B:= PComTermChar(FBuffer) + ((Row - 1) * FColumns - 1);
  for Index:= 0 to Count - 1 do
  begin
    B[Index].Ch:= #32;
    B[Index].BackColor:= FOwner.FTermAttr.BackColor;
    B[Index].FrontColor:= FOwner.FTermAttr.FrontColor;
  end;

  // on screen
  if FOwner.DoubleBuffered then
    FOwner.Invalidate
  else
    FOwner.InvalidatePortion(Classes.Rect(Column, Row, FColumns, Row));
end;

// erase line
procedure TComTermBuffer.EraseLineRight(Column, Row: Integer);
var
  Index: Integer;
  Count: Integer;
  B: PComTermChar;
begin
  // in memory
  Count:= (FColumns - Column + 1);
  B:= PComTermChar(FBuffer) + ((Row - 1) * FColumns + Column - 1);
  for Index:= 0 to Count - 1 do
  begin
    B[Index].Ch:= #32;
    B[Index].BackColor:= FOwner.FTermAttr.BackColor;
    B[Index].FrontColor:= FOwner.FTermAttr.FrontColor;
  end;

  // on screen
  if FOwner.DoubleBuffered then
    FOwner.Invalidate
  else
    FOwner.InvalidatePortion(Classes.Rect(Column, Row, FColumns, Row));
end;

procedure TComTermBuffer.EraseChar(Column, Row, Count: Integer);
var
  Index: Integer;
  B: PComTermChar;
begin
  // in memory
  B:= PComTermChar(FBuffer) + ((Row - 1) * FColumns + Column - 1);
  for Index:= 0 to Count - 1 do
  begin
    B[Index].Ch:= #32;
    B[Index].BackColor:= FOwner.FTermAttr.BackColor;
    B[Index].FrontColor:= FOwner.FTermAttr.FrontColor;
  end;

  // on screen
  if FOwner.DoubleBuffered then
    FOwner.Invalidate
  else
    FOwner.InvalidatePortion(Classes.Rect(Column, Row, FColumns, Row));
end;

procedure TComTermBuffer.DeleteLine(Row, Count: Integer);
var
  Index: Integer;
  B: PComTermChar;
  DstAddr: Pointer;
  SrcAddr: Pointer;
  BytesToMove: Integer;
  Top, Bottom, Height: Integer;
begin
  if FScrollRange.Top > 0 then
    Top:= FScrollRange.Top
  else begin
    Top:= 1;
  end;
  if FScrollRange.Bottom > 0 then
    Bottom:= FScrollRange.Bottom
  else begin
    Bottom:= FRows;
  end;
  Height:= Bottom - Top + 1;
  if Count > Height then Count:= Height;

  DstAddr := (FBuffer + (Row - 1) * FColumns * SizeOf(TComTermChar));
  SrcAddr := (FBuffer + (Row + Count - 1) * FColumns * SizeOf(TComTermChar));
  BytesToMove := (Bottom - Top - Count + 1) * FColumns * SizeOf(TComTermChar);

  // scroll in buffer
  Move(SrcAddr^, DstAddr^, BytesToMove);

  B:= PComTermChar(FBuffer) + ((Bottom - Count) * FColumns);
  for Index:= 0 to Count * FColumns - 1 do
  begin
    B[Index].Ch:= #32;
    B[Index].BackColor:= FOwner.FTermAttr.BackColor;
    B[Index].FrontColor:= FOwner.FTermAttr.FrontColor;
  end;

  // on screen
  if FOwner.DoubleBuffered then
    FOwner.Invalidate
  else
    FOwner.InvalidatePortion(Classes.Rect(1, Top, FColumns, Bottom));
end;

procedure TComTermBuffer.InsertLine(Row, Count: Integer);
var
  Index: Integer;
  B: PComTermChar;
  DstAddr: Pointer;
  SrcAddr: Pointer;
  BytesToMove: Integer;
  Top, Bottom, Height: Integer;
begin
  if FScrollRange.Top > 0 then
    Top:= FScrollRange.Top
  else begin
    Top:= 1;
  end;
  if FScrollRange.Bottom > 0 then
    Bottom:= FScrollRange.Bottom
  else begin
    Bottom:= FRows;
  end;
  Height:= Bottom - Top + 1;
  if Count > Height then Count:= Height;

  SrcAddr := (FBuffer + (Row - 1) * FColumns * SizeOf(TComTermChar));
  DstAddr := (FBuffer + (Row + Count - 1) * FColumns * SizeOf(TComTermChar));
  BytesToMove := (Bottom - Top - Count + 1) * FColumns * SizeOf(TComTermChar);

  // scroll in buffer
  Move(SrcAddr^, DstAddr^, BytesToMove);

  B:= PComTermChar(FBuffer) + ((Row - 1) * FColumns);
  for Index:= 0 to Count * FColumns - 1 do
  begin
    B[Index].Ch:= #32;
    B[Index].BackColor:= FOwner.FTermAttr.BackColor;
    B[Index].FrontColor:= FOwner.FTermAttr.FrontColor;
  end;

  // on screen
  if FOwner.DoubleBuffered then
    FOwner.Invalidate
  else
    FOwner.InvalidatePortion(Classes.Rect(1, Top, FColumns, Bottom));
end;

// erase screen
procedure TComTermBuffer.EraseScreen(Column, Row: Integer);
var
  Index: Integer;
  Count: Integer;
  B: PComTermChar;
begin
  // in memory
  B:= PComTermChar(FBuffer) + ((Row - 1) * FColumns + Column - 1);
  Count:= (FColumns - Column + 1 + (FRows - Row) * FColumns);
  for Index:= 0 to Count - 1 do
  begin
    B[Index].Ch:= #32;
    B[Index].BackColor:= FOwner.FTermAttr.BackColor;
    B[Index].FrontColor:= FOwner.FTermAttr.FrontColor;
  end;

  // on screen
  if FOwner.DoubleBuffered then
    FOwner.Invalidate
  else
    FOwner.InvalidatePortion(Classes.Rect(Column, Row, FColumns, FRows))
end;

// init buffer
procedure TComTermBuffer.Init(ARows, AColumns: Integer);
var
  I: Integer;
begin
  if ARows > 0 then
    FRows:= ARows;
  if AColumns > 0 then
    FColumns:= AColumns;
  if FBuffer <> nil then
  begin
    FreeMem(FBuffer);
    FreeMem(FTabs);
  end;
  GetMem(FBuffer, FColumns * FRows * SizeOf(TComTermChar));
  FillChar(FBuffer^, FColumns * FRows * SizeOf(TComTermChar), 0);
  GetMem(FTabs, FColumns * SizeOf(Boolean));
  FillChar(FTabs^, FColumns * SizeOf(Boolean), 0);
  I := 1;
  while (I <= FColumns) do
  begin
    SetTab(I, True);
    Inc(I, 8);
  end;
end;

// get tab at Column
function TComTermBuffer.GetTab(Column: Integer): Boolean;
begin
  Result := Boolean((FTabs + (Column - 1) * SizeOf(Boolean))^);
end;

// set tab at column
procedure TComTermBuffer.SetTab(Column: Integer; Put: Boolean);
begin
  Boolean((FTabs + (Column - 1) * SizeOf(Boolean))^) := Put;
end;

// find nexts tab position
function TComTermBuffer.NextTab(Column: Integer): Integer;
var
  I: Integer;
begin
  I := Column;
  while (I <= FColumns) do
    if GetTab(I) then
      Break
    else
      Inc(I);
  if I > FColumns then
    Result := 0
  else
    Result := I;
end;

// clear all tabs
procedure TComTermBuffer.ClearAllTabs;
begin
  FillChar(FTabs^, FColumns * SizeOf(Boolean), 0);
end;

function TComTermBuffer.GetLineLength(Line: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to FColumns do
    if GetChar(I, Line).Ch <> #0 then
      Result := I;
end;

function TComTermBuffer.GetLastLine: Integer;
var
  J: Integer;
begin
  Result := 0;
  for J := 1 to FRows do
    if GetLineLength(J) > 0 then
      Result := J;
end;

// get last character in buffer
(*****************************************
 * TComCustomTerminal control            *
 *****************************************)

// create control
constructor TCustomComTerminal.Create(AOwner: TComponent);
begin
  FScrollBars := ssVertical;

  inherited Create(AOwner);

  Parent:= TWinControl(AOwner);

  BorderStyle := bsSingle;
  Color := clBlack;
  DoubleBuffered := True;
  TabStop := True;
  Font.Name := 'Consolas';
  Font.Color:= clWhite;
  FEmulation := teVT100orANSI;
  FColumns := 80;
  FRows := 100;
  FVisibleRows:= 25;
  FAutoFollow := True;
  FCaretPos := Classes.Point(1, 1);
  FTopLeft := Classes.Point(1, 1);
  FMainBuffer := TComTermBuffer.Create(Self);
  FAlternateBuffer := TComTermBuffer.Create(Self);
  FTermAttr.FrontColor := Font.Color;
  FTermAttr.BackColor := Color;

  FBuffer:= FMainBuffer;
  FParams:= TStringList.Create;

  CreateEscapeCodes;
  if not (csDesigning in ComponentState) then
  begin
    FMainBuffer.Init(FRows, FColumns);
    FAlternateBuffer.Init(FVisibleRows, FColumns);
  end;

  SetBounds(Left, Top, 400, 250);
end;

// destroy control
destructor TCustomComTerminal.Destroy;
begin
  PtyDevice := nil;
  FMainBuffer.Free;
  FAlternateBuffer.Free;
  FEscapeCodes.Free;
  FParams.Free;
  inherited Destroy;
end;

// clear terminal screen
procedure TCustomComTerminal.ClearScreen;
begin
  FBuffer.Init(0, 0);
  MoveCaret(1, 1);
  Invalidate;
end;

// move caret
procedure TCustomComTerminal.MoveCaret(AColumn, ARow: Integer);
begin
  if AColumn > FBuffer.Columns then
  begin
    if FWrapLines then
      FCaretPos.X := FBuffer.Columns + 1
    else
      FCaretPos.X := FBuffer.Columns
  end
  else
    if AColumn < 1 then
      FCaretPos.X := 1
    else
      FCaretPos.X := AColumn;

  if ARow > FBuffer.Rows then
    FCaretPos.Y := FBuffer.Rows
  else
    if ARow < 1 then
      FCaretPos.Y := 1
    else
      FCaretPos.Y := ARow;

  if FCaretCreated then
    SetCaretPos((FCaretPos.X - FTopLeft.X) * FFontWidth,
      (FCaretPos.Y - FTopLeft.Y) * FFontHeight + FFontHeight - FCaretHeight);
end;

// write data to screen
procedure TCustomComTerminal.Write(const Buffer:string; Size: Integer);
var
  I: Integer;
  L: Integer;
  Ch: TUTF8Char;
  Res: TEscapeResult;
begin
  HideCaret;
  try
    // show it on screen
    I:= 1;
    while I <= Size do
    begin
      L:= UTF8CodepointSizeFast(@Buffer[I]);
      Ch:= Copy(Buffer, I, L);

      if (FEscapeCodes <> nil) then
      begin
        Res := FEscapeCodes.ProcessChar(Ch);
        if Res = erChar then
          PutChar(FEscapeCodes.Character);
        if Res = erCode then
        begin
          if not PutEscapeCode(FEscapeCodes.Code, FEscapeCodes.Params) then
             DoUnhandledCode(FEscapeCodes.Code, FEscapeCodes.Data);
          FEscapeCodes.Params.Clear;
        end;
      end
      else begin
        PutChar(Ch);
      end;
      I+= L;
    end;
  finally
    ShowCaret;
  end;
end;

// write string on screen, but not to port
procedure TCustomComTerminal.WriteStr(const Str: string);
begin
  Write(Str, Length(Str));
end;

// write escape code on screen
procedure TCustomComTerminal.WriteEscCode(ACode: TEscapeCode;
  AParams: TStrings);
begin
  if FEscapeCodes <> nil then
    PutEscapeCode(ACode, AParams);
end;

// load screen buffer from file
procedure TCustomComTerminal.LoadFromStream(Stream: TStream);
var
  I: Integer;
  Ch: Char;
begin
  HideCaret;
  for I := 0 to Stream.Size - 1 do
  begin
    Stream.Read(Ch, 1);
    PutChar(Ch);
  end;
  ShowCaret;
end;

// save screen buffer to file
procedure TCustomComTerminal.SaveToStream(Stream: TStream);
var
  I, J, LastChar, LastLine: Integer;
  Ch: TUTF8Char;
  EndLine: string;
begin
  EndLine := #13#10;
  LastLine := FBuffer.GetLastLine;
  for J := 1 to LastLine do
  begin
    LastChar := FBuffer.GetLineLength(J);
    if LastChar > 0 then
    begin
      for I := 1 to LastChar do
      begin
        Ch := FBuffer.GetChar(I, J).Ch;
        // replace null characters with blanks
        if Ch = #0 then
          Ch := #32;
        Stream.Write(Ch, 1);
      end;
    end;
    // new line
    if J <> LastLine then
      Stream.Write(EndLine[1], Length(EndLine));
  end;
end;

// select terminal font
procedure TCustomComTerminal.SelectFont;
begin
  with TFontDialog.Create(Application) do
  begin
    Options := Options + [fdFixedPitchOnly];
    Font := Self.Font;
    if Execute then
      Self.Font := Font;
    Free;
  end;
end;

// process font change
procedure TCustomComTerminal.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FTermAttr.FrontColor := Font.Color;
  if not CalculateMetrics then
    ;//Font.Name := ComTerminalFont.Name;
  if fsUnderline in Font.Style then
    Font.Style := Font.Style - [fsUnderline];
  AdjustSize;
  UpdateScrollRange;
end;

procedure TCustomComTerminal.CMColorChanged(var Message: TMessage);
begin
  inherited;
  FTermAttr.BackColor := Color;
end;

procedure TCustomComTerminal.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  // request arrow keys and WM_CHAR message to be handled by the control
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
  // tab key
  if FWantTab then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

// lost focus
procedure TCustomComTerminal.WMKillFocus(var Message: TWMSetFocus);
begin
  // destroy caret because it could be requested by some other control
  DestroyCaret(Handle);
  FCaretCreated := False;
  inherited;
end;

// gained focus
procedure TCustomComTerminal.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  // control activated, create caret
  InitCaret;
end;

// left button pressed
procedure TCustomComTerminal.WMLButtonDown(var Message: TLMLButtonDown);
begin
  // set focus when left button down
  if CanFocus and TabStop then
    SetFocus;
  inherited;
end;

// size changed
procedure TCustomComTerminal.WMSize(var Msg: TWMSize);
var
  ARows, AColumns: Integer;
begin
  inherited WMSize(Msg);

  if (ClientWidth = 0) or (ClientHeight = 0) then
    Exit;

  ARows:= Max(2, ClientHeight div FFontHeight);
  AColumns:= Max(2, ClientWidth div FFontWidth);

  if (ARows <> FVisibleRows) or (AColumns <> FColumns) then
  begin
    FColumns := AColumns;
    FVisibleRows := ARows;
    FRows := Max(FRows, FVisibleRows);
    AdjustSize;
    if not ((csLoading in ComponentState) or (csDesigning in ComponentState)) then
    begin
      FMainBuffer.Init(FRows, FColumns);
      FAlternateBuffer.Init(FVisibleRows, FColumns);
      if Assigned(FPtyDevice) then
        FPtyDevice.SetScreenSize(FColumns, FVisibleRows);
      Invalidate;
    end;
    UpdateScrollRange;
  end;
end;

// vertical scroll
procedure TCustomComTerminal.WMHScroll(var Message: TWMHScroll);
begin
  ModifyScrollBar(SB_HORZ, Message.ScrollCode, Message.Pos);
end;

// horizontal scroll
procedure TCustomComTerminal.WMVScroll(var Message: TWMVScroll);
begin
  ModifyScrollBar(SB_VERT, Message.ScrollCode, Message.Pos);
end;

// set size to fit whole terminal screen
function TCustomComTerminal.CanAutoSize(var NewWidth,
  NewHeight: Integer): Boolean;
var
  Border: Integer;
begin
  Result := True;
  if Align in [alNone, alLeft, alRight] then
  begin
    NewWidth := FFontWidth * FColumns;
    if BorderStyle = bsSingle then
    begin
      Border := SM_CXBORDER;
      NewWidth := NewWidth + 2 * GetSystemMetrics(BORDER);
    end;
  end;
  if Align in [alNone, alTop, alBottom] then
  begin
    NewHeight := FFontHeight * FRows;
    if BorderStyle = bsSingle then
    begin
      Border := SM_CYBORDER;
      NewHeight := NewHeight + 2 * GetSystemMetrics(Border);
    end;
  end;
end;

// set control parameters
procedure TCustomComTerminal.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[BorderStyle];
    if NewStyleControls and (BorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    if FScrollBars in [ssVertical, ssBoth] then
      Style := Style or WS_VSCROLL;
    if FScrollBars in [ssHorizontal, ssBoth] then
      Style := Style or WS_HSCROLL;
  end;
  ControlStyle := ControlStyle + [csOpaque];
end;

// key down
procedure TCustomComTerminal.KeyDown(var Key: Word; Shift: TShiftState);
var
  Code: TEscapeCode;
begin
  inherited KeyDown(Key, Shift);

  if (Key = VK_TAB) then
  begin
    SendChar(Chr(Key));
    Key:= 0;
    Exit;
  end;

  if (Key = VK_BACK) then
  begin
    SendChar(#$7f);
    Key:= 0;
    Exit;
  end;

  if Key in [VK_F1..VK_F12] then
  begin
    Code := ecFuncKey;
    FParams.Text:= IntToStr(Key - VK_F1);
    SendCode(Code, FParams);
    Exit;
  end;

  case Key of
    VK_INSERT: Code := ecInsertKey;
    VK_DELETE: Code := ecDeleteKey;
    VK_PRIOR: Code := ecPageUpKey;
    VK_NEXT: Code := ecPageDownKey;
  else
    Code := ecUnknown;
  end;

  if (Code <> ecUnknown) then
  begin
    SendCode(Code, nil);
    Exit;
  end;

  case Key of
    VK_UP: Code := ecCursorUp;
    VK_DOWN: Code := ecCursorDown;
    VK_LEFT: Code := ecCursorLeft;
    VK_RIGHT: Code := ecCursorRight;
    VK_HOME: Code := ecCursorHome;
    VK_END: Code := ecCursorEnd;
  else
    Code := ecUnknown;
  end;
  if FTermMode.Keys = akTerminal then
  begin
    if Code <> ecUnknown then
      if FArrowKeys = akTerminal then
        SendCode(Code, nil)
      else
        PutEscapeCode(Code, nil);
  end
  else
    case Code of
      ecCursorUp: SendCode(ecAppCursorUp, nil);
      ecCursorDown: SendCode(ecAppCursorDown, nil);
      ecCursorLeft: SendCode(ecAppCursorLeft, nil);
      ecCursorRight: SendCode(ecAppCursorRight, nil);
      ecCursorHome: SendCode(ecAppCursorHome, nil);
      ecCursorEnd: SendCode(ecAppCursorEnd, nil);
    end;
{$IFDEF LCLGTK2}
  if Key in [VK_UP, VK_DOWN] then
  begin
    Key:= 0;
  end;
{$ENDIF}
end;

// key pressed
procedure TCustomComTerminal.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  // SendChar(Key);
end;

procedure TCustomComTerminal.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  inherited UTF8KeyPress(UTF8Key);
  SendChar(UTF8Key);
end;

procedure TCustomComTerminal.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  MouseEvent(ecMouseDown, Button, Shift, X, Y);
end;

procedure TCustomComTerminal.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  MouseEvent(ecMouseUp, Button, Shift, X, Y);
end;

procedure TCustomComTerminal.CreateWnd;
begin
  inherited CreateWnd;
  if FScrollBars in [ssVertical, ssBoth] then
    ShowScrollBar(Handle, SB_VERT, True);
  if FScrollBars in [ssHorizontal, ssBoth] then
    ShowScrollBar(Handle, SB_HORZ, True);
end;

procedure TCustomComTerminal.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FPtyDevice) and (Operation = opRemove) then
    PtyDevice := nil;
end;

// paint characters
procedure TCustomComTerminal.PaintTerminal(Rect: TRect);
var
  I, J, X, Y: Integer;
  Ch: TComTermChar;
begin
  HideCaret;
  if (Rect.Bottom + FTopLeft.Y - 1) > FBuffer.Rows then
    Dec(Rect.Bottom);
  if (Rect.Right + FTopLeft.X - 1) > FBuffer.Columns then
    Dec(Rect.Right);
  for J := Rect.Top to Rect.Bottom do
  begin
    Y := J + FTopLeft.Y - 1;
    for I := Rect.Left to Rect.Right do
    begin
      X := I + FTopLeft.X - 1;
      Ch := FBuffer.GetChar(X, Y);
      if Ch.Ch <> Chr(0) then
        DrawChar(I, J, Ch);
    end;
  end;
  ShowCaret;
end;

procedure TCustomComTerminal.PaintDesign;
begin
  Canvas.TextOut(0, 0, 'Virtual Terminal Emulator');
end;

procedure TCustomComTerminal.Paint;
var
  ARect: TRect;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  if csDesigning in ComponentState then
    PaintDesign
  else
  begin
    MoveCaret(FCaretPos.X, FCaretPos.Y);
    // don't paint whole screen, but only the invalidated portion
    ARect.Left := Canvas.ClipRect.Left div FFontWidth + 1;
    ARect.Right := Min(Canvas.ClipRect.Right div FFontWidth + 1, FBuffer.Columns);
    ARect.Top := Canvas.ClipRect.Top div FFontHeight + 1;
    ARect.Bottom := Min(Canvas.ClipRect.Bottom div FFontHeight + 1, FBuffer.Rows);
    PaintTerminal(ARect);
  end;
end;

// creates caret
procedure TCustomComTerminal.CreateTerminalCaret;
begin
  FCaretHeight := 0;
  if FCaret = tcBlock then
    FCaretHeight := FFontHeight
  else
    if FCaret = tcUnderline then
      FCaretHeight := FFontHeight div 8;
  if FCaretHeight > 0 then
  begin
    CreateCaret(Handle, 0, FFontWidth, FCaretHeight);
    FCaretCreated := True;
  end;
end;

// string received from com port
procedure TCustomComTerminal.StringReceived(Str: string);
begin
  DoStrRecieved(Str);
  WriteStr(Str);
end;

// draw one character on screen, but do not put it in buffer
procedure TCustomComTerminal.DrawChar(AColumn, ARow: Integer;
  Ch: TComTermChar);
var
  OldBackColor, OldFrontColor: Integer;
begin
  OldBackColor := Canvas.Brush.Color;
  OldFrontColor := Canvas.Font.Color;
  Canvas.Brush.Color := Ch.BackColor;
  Canvas.Font.Color := Ch.FrontColor;
  if Ch.Bold then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold]
  else begin
    Canvas.Font.Style := Canvas.Font.Style - [fsBold];
  end;
  if Ch.Underline then
    Canvas.Font.Style := Canvas.Font.Style + [fsUnderline]
  else begin
    Canvas.Font.Style := Canvas.Font.Style - [fsUnderline];
  end;
  Canvas.TextOut((AColumn - 1) * FFontWidth, (ARow - 1) * FFontHeight, Ch.Ch);
  Canvas.Brush.Color := OldBackColor;
  Canvas.Font.Color := OldFrontColor;
end;

procedure TCustomComTerminal.WrapLine;
begin
  if FCaretPos.X = FBuffer.Columns + 1 then
  begin
    if FCaretPos.Y = FBuffer.Rows then
    begin
      FBuffer.ScrollDown;
      MoveCaret(1, FCaretPos.Y);
    end
    else begin
      MoveCaret(1, FCaretPos.Y + 1)
    end;
  end;
end;

// move caret after new char is put on screen
procedure TCustomComTerminal.AdvanceCaret(Kind: TAdvanceCaret);
var
  I: Integer;
begin
  case Kind of
    acChar:
      begin
        if (FCaretPos.X < FColumns) or FWrapLines then
          MoveCaret(FCaretPos.X + 1, FCaretPos.Y);
      end;
    acReturn: MoveCaret(1, FCaretPos.Y);
    acLineFeed:
      begin
        if FCaretPos.Y = FBuffer.Rows then
          FBuffer.ScrollDown
        else
          MoveCaret(FCaretPos.X, FCaretPos.Y + 1);
      end;
    acReverseLineFeed:
      begin
        if FCaretPos.Y = 1 then
          FBuffer.ScrollUp
        else
          MoveCaret(FCaretPos.X, FCaretPos.Y - 1);
      end;
    acBackSpace: MoveCaret(FCaretPos.X - 1, FCaretPos.Y);
    acTab:
      begin
        I := FBuffer.NextTab(FCaretPos.X + 1);
        if I > 0 then
          MoveCaret(I, FCaretPos.Y);
      end;
    acPage:
      ClearScreen;
  end;
  if FAutoFollow then
  begin
    if (FCaretPos.Y - FTopLeft.Y) > FVisibleRows then
    begin
      I:= FCaretPos.Y - FVisibleRows + 1;
      ModifyScrollBar(SB_Vert, SB_THUMBPOSITION, I);
    end;
  end;
end;

// set character attributes
procedure TCustomComTerminal.SetAttributes(AParams: TStrings);
var
  I, Value: Integer;

  procedure AllOff;
  begin
    FTermAttr.FrontColor := Font.Color;
    FTermAttr.BackColor := Color;
    FTermAttr.Invert := False;
    FTermAttr.Bold := False;
    FTermAttr.Underline := False;
  end;

  function GetExtendedColor(var Index: Integer): TColor;
  var
    RGB: Integer;
    R, G, B: Byte;
    AParam: Integer;
  begin
    AParam:= FEscapeCodes.GetParam(Index + 1, AParams);
    // Color from RGB value
    if AParam = 2 then
    begin
      R:= FEscapeCodes.GetParam(Index + 2, AParams);
      G:= FEscapeCodes.GetParam(Index + 3, AParams);
      B:= FEscapeCodes.GetParam(Index + 4, AParams);
      Result:= RGBToColor(R, G, B);
      Inc(Index, 4);
    end
    // Color from 256 color palette
    else if (AParam = 5) then
    begin
      RGB:= FEscapeCodes.GetParam(Index + 2, AParams);
      if (RGB >= 0) and (RGB < 256) then
      begin
        Result:= Color256Table[RGB];
      end;
      Inc(Index, 2);
    end;
  end;

begin
  I:= 1;
  if AParams.Count = 0 then
    AllOff;
  while I <= AParams.Count do
  begin
    Value := FEscapeCodes.GetParam(I, AParams);
    case Value of
      0:  AllOff;
      1:  FTermAttr.Bold := True;
      4:  FTermAttr.Underline := True;
      7:  FTermAttr.Invert := True;
      21: FTermAttr.Bold := False;
      24: FTermAttr.Underline := False;
      27: FTermAttr.Invert := False;

      // Extended foreground color
      38: FTermAttr.FrontColor := GetExtendedColor(I);
      // Default foreground color
      39: FTermAttr.FrontColor := Font.Color;
      // Extended background color
      48: FTermAttr.BackColor := GetExtendedColor(I);
      // Default background color
      49: FTermAttr.BackColor := Color;

      // NEW foreground colors
      else if (Value in [30..37]) then
        FTermAttr.FrontColor := Color256Table[Value - 30]
        // NEW background colors
      else if (Value in [40..47]) then
        FTermAttr.BackColor := Color256Table[Value - 40]

      // BRIGHT foreground colors
      else if (Value in [90..97]) then
        FTermAttr.FrontColor := Color256Table[Value - 90 + 8]
      // BRIGHT background colors
      else if (Value in [100..107]) then
        FTermAttr.BackColor := Color256Table[Value - 100 + 8]

      else begin
        DoUnhandledCode(ecAttributes, IntToStr(Value));
      end;
    end;
    Inc(I);
  end;
end;

procedure TCustomComTerminal.SetMode(AParams: TStrings; OnOff: Boolean);
var
  Str: string;
begin
  if AParams.Count = 0 then
    Exit;
  Str := AParams[0];
  if Str = '?1' then
  begin
    if OnOff then
      FTermMode.Keys := akWindows
    else
      FTermMode.Keys := akTerminal;
  end
  else if Str = '7' then
    FWrapLines := OnOff
  else if Str = '?3' then
  begin
    if OnOff then
      Columns := 132
    else
      Columns := 80;
  end
  else if Str = '?1002' then
    FTermMode.MouseTrack:= OnOff
  else if Str = '?1006' then
    FTermMode.MouseMode:= OnOff
  else if Str = '?1049' then
  begin
    FBuffer.FTopLeft:= FTopLeft;
    FBuffer.FCaretPos:= FCaretPos;
    if OnOff then
      FBuffer := FAlternateBuffer
    else begin
      FBuffer := FMainBuffer;
    end;
    FTopLeft:= FBuffer.FTopLeft;
    FCaretPos:= FBuffer.FCaretPos;
    UpdateScrollRange;
  end
  else begin
    DoUnhandledMode(Str, OnOff);
  end;
end;

// invalidate portion of screen
procedure TCustomComTerminal.InvalidatePortion(ARect: TRect);
var
  Rect: TRect;
begin
  Rect.Left := Max((ARect.Left - FTopLeft.X) * FFontWidth, 0);
  Rect.Right := Max((ARect.Right - FTopLeft.X + 1) * FFontWidth, 0);
  Rect.Top := Max((ARect.Top - FTopLeft.Y) * FFontHeight, 0);
  Rect.Bottom := Max((ARect.Bottom - FTopLeft.Y + 1) * FFontHeight, 0);
  InvalidateRect(Handle, @Rect, True);
end;

// modify scroll bar
procedure TCustomComTerminal.ModifyScrollBar(ScrollBar, ScrollCode,
  Pos: Integer);
var
  CellSize, OldPos, APos, Dx, Dy: Integer;
begin
  if (ScrollCode = SB_ENDSCROLL) or
    ((ScrollCode = SB_THUMBTRACK) and not FSmoothScroll)
  then
    Exit;
  if ScrollBar = SB_HORZ then
    CellSize := FFontWidth
  else
    CellSize := FFontHeight;
  APos := GetScrollPos(Handle, ScrollBar);
  OldPos := APos;
  case ScrollCode of
    SB_LINEUP: Dec(APos);
    SB_LINEDOWN: Inc(APos);
    SB_PAGEUP: Dec(APos, ClientHeight div CellSize);
    SB_PAGEDOWN: Inc(APos, ClientHeight div CellSize);
    SB_THUMBPOSITION,
    SB_THUMBTRACK: APos := Pos;
  end;
  SetScrollPos(Handle, ScrollBar, APos, True);
  APos := GetScrollPos(Handle, ScrollBar);
  if ScrollBar = SB_HORZ then
  begin
    FTopLeft.X := APos + 1;
    Dx := (OldPos - APos) * FFontWidth;
    Dy := 0;
  end else
  begin
    FTopLeft.Y := APos + 1;
    Dx := 0;
    Dy := (OldPos - APos) * FFontHeight;
  end;
  if DoubleBuffered then
    Invalidate
  else
    ScrollWindowEx(Handle, Dx, Dy, nil, nil, 0, nil, SW_ERASE or SW_INVALIDATE);
end;

// calculate scroll position
procedure TCustomComTerminal.UpdateScrollPos;
begin
  if FScrollBars in [ssBoth, ssHorizontal] then
  begin
    FTopLeft.X := GetScrollPos(Handle, SB_HORZ) + 1;
    SetScrollPos(Handle, SB_HORZ, FTopLeft.X - 1, True);
  end;
  if FScrollBars in [ssBoth, ssVertical] then
  begin
    FTopLeft.Y := GetScrollPos(Handle, SB_VERT) + 1;
    SetScrollPos(Handle, SB_VERT, FTopLeft.Y - 1, True);
  end;
end;

// calculate scroll range
procedure TCustomComTerminal.UpdateScrollRange;
var
  OldScrollBars: TScrollStyle;
  AHeight, AWidth: Integer;

  // is scroll bar visible?
  function ScrollBarVisible(Code: Word): Boolean;
  var
    Min, Max: Integer;
  begin
    Result := False;
    if (ScrollBars = ssBoth) or
      ((Code = SB_HORZ) and (ScrollBars = ssHorizontal)) or
      ((Code = SB_VERT) and (ScrollBars = ssVertical)) then
    begin
      GetScrollRange(Handle, Code, Min, Max);
      Result := Min <> Max;
    end;
  end;

  procedure SetRange(Code, Max: Integer);
  var
    Info: TScrollInfo;
  begin
    Info:= Default(TScrollInfo);
    Info.fMask := SIF_RANGE;
    Info.nMax := Max - 1;
    SetScrollInfo(Handle, Code, Info, False);
  end;

  // set horizontal range
  procedure SetHorzRange;
  var
    Max: Integer;
    AColumns: Integer;
  begin
    if OldScrollBars in [ssBoth, ssHorizontal] then
    begin
      AColumns := AWidth div FFontWidth;
      if AColumns >= FBuffer.Columns then
        SetRange(SB_HORZ, 1) // screen is wide enough, hide scroll bar
      else
      begin
        Max := FBuffer.Columns - (AColumns - 1);
        SetRange(SB_HORZ, Max);
      end;
    end;
  end;

  // set vertical range
  procedure SetVertRange;
  var
    Max, ARows: Integer;
  begin
    if OldScrollBars in [ssBoth, ssVertical] then
    begin
      ARows := AHeight div FFontHeight;
      if ARows >= FBuffer.Rows then
        SetRange(SB_VERT, 1)  // screen is high enough, hide scroll bar
      else
      begin
        Max := FBuffer.Rows - (ARows - 1);
        SetRange(SB_VERT, Max);
      end;
    end;
  end;

begin
  if (FScrollBars = ssNone) or (FBuffer = nil) then
    Exit;
  AHeight := ClientHeight;
  AWidth := ClientWidth;
  if ScrollBarVisible(SB_HORZ) then
    Inc(AHeight, GetSystemMetrics(SM_CYHSCROLL));
  if ScrollBarVisible(SB_VERT) then
    Inc(AWidth, GetSystemMetrics(SM_CXVSCROLL));
  // Temporarily mark us as not having scroll bars to avoid recursion
  OldScrollBars := FScrollBars;
  FScrollBars := ssNone;
  try
    SetHorzRange;
    AHeight := ClientHeight;
    SetVertRange;
    if AWidth <> ClientWidth then
    begin
      AWidth := ClientWidth;
      SetHorzRange;
    end;
  finally
    FScrollBars := OldScrollBars;
  end;
  // range changed, update scroll bar position
  UpdateScrollPos;
end;

// hide caret
procedure TCustomComTerminal.HideCaret;
begin
  if FCaretCreated then
    LCLIntf.HideCaret(Handle);
end;

// show caret
procedure TCustomComTerminal.ShowCaret;
begin
  if FCaretCreated then
    LCLIntf.ShowCaret(Handle);
end;

// send character to com port
procedure TCustomComTerminal.SendChar(Ch: TUTF8Char);
begin
  if (FPtyDevice <> nil) and (FPtyDevice.Connected) then
  begin
    FPtyDevice.WriteStr(Ch);
    if FLocalEcho then
    begin
      // local echo is on, show character on screen
      HideCaret;
      PutChar(Ch);
      ShowCaret;
    end;
    // send line feeds after carriage return
    if (Ch = Chr(13)) and FSendLF then
      SendChar(Chr(10));
  end;
end;

// send escape code
procedure TCustomComTerminal.SendCode(Code: TEscapeCode; AParams: TStrings);
begin
  if (FPtyDevice <> nil) and (FPtyDevice.Connected) and (FEscapeCodes <> nil) then
  begin
    FPtyDevice.WriteStr(FEscapeCodes.EscCodeToStr(Code, AParams));
    if FLocalEcho then
    begin
      // local echo is on, show character on screen
      HideCaret;
      PutEscapeCode(Code, AParams);
      ShowCaret;
    end;
  end;
end;

// send escape code to port
procedure TCustomComTerminal.SendCodeNoEcho(Code: TEscapeCode; AParams: TStrings);
begin
  if (FPtyDevice <> nil) and (FPtyDevice.Connected) and (FEscapeCodes <> nil) then
    FPtyDevice.WriteStr(FEscapeCodes.EscCodeToStr(Code, AParams));
end;

procedure TCustomComTerminal.MouseEvent(Code: TEscapeCode;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AButton: Integer;
begin
  if (FTermMode.MouseMode and FTermMode.MouseTrack) then
  begin
    case Button of
      mbLeft:   AButton:= 0;
      mbRight:  AButton:= 2;
      mbMiddle: AButton:= 1;
      else      AButton:= Ord(Button);
    end;
    FParams.Text:= IntToStr(AButton);
    FParams.Add(IntToStr(X div FFontWidth + 1));
    FParams.Add(IntToStr(Y div FFontHeight + 1));
    SendCodeNoEcho(Code, FParams);
  end;
end;

// process escape code on screen
function TCustomComTerminal.PutEscapeCode(ACode: TEscapeCode; AParams: TStrings): Boolean;
begin
  Result := True;
  with FEscapeCodes do
    case ACode of
      ecCursorUp,
      ecAppCursorUp: MoveCaret(FCaretPos.X, FCaretPos.Y - GetParam(1, AParams));
      ecCursorDown: MoveCaret(FCaretPos.X, FCaretPos.Y + GetParam(1, AParams));
      ecCursorRight: MoveCaret(FCaretPos.X + GetParam(1, AParams), FCaretPos.Y);
      ecCursorLeft: MoveCaret(FCaretPos.X - GetParam(1, AParams), FCaretPos.Y);
      ecCursorEnd: MoveCaret(FColumns, FCaretPos.Y);
      ecCursorHome,
      ecCursorMove: MoveCaret(GetParam(2, AParams), GetParam(1, AParams));
      ecCursorMoveX: MoveCaret(GetParam(1, AParams), FCaretPos.Y);
      ecCursorMoveY: MoveCaret(FCaretPos.X, GetParam(1, AParams));
      ecReverseLineFeed: AdvanceCaret(acReverseLineFeed);
      ecEraseLineLeft: FBuffer.EraseLineLeft(FCaretPos.X, FCaretPos.Y);
      ecEraseLineRight: FBuffer.EraseLineRight(FCaretPos.X, FCaretPos.Y);
      ecEraseScreenFrom: FBuffer.EraseScreen(FCaretPos.X, FCaretPos.Y);
      ecEraseScreen: begin FBuffer.EraseScreen(1, 1); MoveCaret(1, 1) end;
      ecEraseLine:
      begin
        FBuffer.EraseLineRight(1, FCaretPos.Y);
        MoveCaret(1, FCaretPos.Y)
      end;
      ecEraseChar:
        begin
          FBuffer.EraseChar(FCaretPos.X, FCaretPos.Y, GetParam(1, AParams));
        end;
      ecIdentify:
      begin
        AParams.Clear;
        AParams.Add('2');
        SendCodeNoEcho(ecIdentResponse, AParams);
      end;
      ecSetTab: FBuffer.SetTab(FCaretPos.X, True);
      ecClearTab: FBuffer.SetTab(FCaretPos.X, False);
      ecClearAllTabs: FBuffer.ClearAllTabs;
      ecAttributes: SetAttributes(AParams);
      ecSetMode: SetMode(AParams, True);
      ecResetMode: SetMode(AParams, False);
      ecReset:
      begin
        AParams.Clear;
        AParams.Add('0');
        SetAttributes(AParams);
      end;
      ecSaveCaret: SaveCaretPos;
      ecRestoreCaret: RestoreCaretPos;
      ecSaveCaretAndAttr: begin SaveCaretPos; SaveAttr; end;
      ecRestoreCaretAndAttr: begin RestoreCaretPos; RestoreAttr; end;
      ecQueryCursorPos:
      begin
        AParams.Clear;
        AParams.Add(IntToStr(FCaretPos.Y));
        AParams.Add(IntToStr(FCaretPos.X));
        SendCodeNoEcho(ecReportCursorPos, AParams);
      end;
      ecQueryDevice: SendCodeNoEcho(ecReportDeviceOK, nil);
      ecTest: PerformTest('E');
      ecScrollRegion:
      begin
        FBuffer.FScrollRange.Top:= GetParam(1, AParams);
        FBuffer.FScrollRange.Bottom:= GetParam(2, AParams);
      end;
      ecInsertLine: FBuffer.InsertLine(FCaretPos.Y, GetParam(1, AParams));
      ecDeleteLine: FBuffer.DeleteLine(FCaretPos.Y, GetParam(1, AParams));
      ecSoftReset:
      begin
        FBuffer.FScrollRange:= Default(TRect);
      end
    else
      Result := False;
    end;
end;

// calculate font height and width
function TCustomComTerminal.CalculateMetrics: Boolean;
var
  Metrics: TTextMetric;
begin
  GetTextMetrics(Canvas.Handle, Metrics);
  FFontHeight := Metrics.tmHeight;
  FFontWidth := Metrics.tmAveCharWidth;
  // allow only fixed pitch fonts
  Result := (Metrics.tmPitchAndFamily and TMPF_FIXED_PITCH) = 0;
end;

// visual character is appears on screen
procedure TCustomComTerminal.DoChar(Ch: TUTF8Char);
begin
  if Assigned(FOnChar) then
    FOnChar(Self, Ch);
end;

// get custom escape codes processor
procedure TCustomComTerminal.DoGetEscapeCodes(
  var EscapeCodes: TEscapeCodes);
begin
  if Assigned(FOnGetEscapeCodes) then
    FOnGetEscapeCodes(Self, EscapeCodes);
end;

// string recieved
procedure TCustomComTerminal.DoStrRecieved(var Str: string);
begin
  if Assigned(FOnStrRecieved) then
    FOnStrRecieved(Self, Str);
end;

// let application handle unhandled escape code
procedure TCustomComTerminal.DoUnhandledCode(Code: TEscapeCode;
  Data: string);
begin
  if Assigned(FOnUnhandledCode) then
    FOnUnhandledCode(Self, Code, Data);
end;

procedure TCustomComTerminal.DoUnhandledMode(const Data: string; OnOff: Boolean);
begin
  if Assigned(FOnUnhandledMode) then
    FOnUnhandledMode(Self, Data, OnOff);
end;

// create escape codes processor
procedure TCustomComTerminal.CreateEscapeCodes;
begin
  if csDesigning in ComponentState then
    Exit;
  case FEmulation of
    teVT52: FEscapeCodes := TEscapeCodesVT52.Create;
    teVT100orANSI: FEscapeCodes := TEscapeCodesVT100.Create;
  else
    begin
      FEscapeCodes := nil;
      DoGetEscapeCodes(FEscapeCodes);
    end;
  end;
end;

// perform screen test
procedure TCustomComTerminal.PerformTest(ACh: Char);
var
  I, J: Integer;
  TermCh: TComTermChar;
begin
  with TermCh do
  begin
    Ch := ACh;
    FrontColor := Font.Color;
    BackColor := Color;
    Underline := False;
  end;
  for I := 1 to FBuffer.Columns do
    for J := 1 to FBuffer.Rows do
      FBuffer.SetChar(I, J, TermCh);
  Invalidate;
end;

// get current character properties
function TCustomComTerminal.GetCharAttr: TComTermChar;
begin
  if FTermAttr.Invert then
    // Result.FrontColor := Color
    Result.FrontColor := FTermAttr.BackColor
  else
    // Result.BackColor := Font.Color;
    Result.FrontColor := FTermAttr.FrontColor;
  if FTermAttr.Invert then
    // Result.BackColor := Font.Color
    Result.BackColor := FTermAttr.FrontColor
  else
    // Result.FrontColor := Color
    Result.BackColor := FTermAttr.BackColor;
  // NEW end changes
  Result.Bold := FTermAttr.Bold;
  Result.Underline := FTermAttr.Underline;
  Result.Ch := #0;
end;

// put one character on screen
procedure TCustomComTerminal.PutChar(Ch: TUTF8Char);
var
  TermCh: TComTermChar;
begin
  case Ch[1] of
    #8: AdvanceCaret(acBackspace);
    #9: AdvanceCaret(acTab);
    #10: AdvanceCaret(acLineFeed);
    #12: AdvanceCaret(acPage);
    #13: AdvanceCaret(acReturn);
    #32..#255:
      begin
        TermCh := GetCharAttr;
        TermCh.Ch := Ch;
        if FWrapLines then WrapLine;
        FBuffer.SetChar(FCaretPos.X, FCaretPos.Y, TermCh);
        DrawChar(FCaretPos.X - FTopLeft.X + 1,
          FCaretPos.Y - FTopLeft.Y + 1, TermCh);
        AdvanceCaret(acChar);
      end;
  end;
  DoChar(Ch);
end;

// init caret
procedure TCustomComTerminal.InitCaret;
begin
  CreateTerminalCaret;
  MoveCaret(FCaretPos.X, FCaretPos.Y);
  ShowCaret;
end;

// restore caret position
procedure TCustomComTerminal.RestoreCaretPos;
begin
  MoveCaret(FSaveCaret.X, FSaveCaret.Y);
end;

// save caret position
procedure TCustomComTerminal.SaveCaretPos;
begin
  FSaveCaret := FCaretPos;
end;

// restore attributes
procedure TCustomComTerminal.RestoreAttr;
begin
  FTermAttr := FSaveAttr;
end;

// save attributes
procedure TCustomComTerminal.SaveAttr;
begin
  FSaveAttr := FTermAttr;
end;

procedure TCustomComTerminal.RxBuf(Sender: TObject; const Buffer;  Count: Integer);
var
  Str: String;
  sa : Ansistring;

  // append line feeds to carriage return
  procedure AppendLineFeeds;
  var
    I: Integer;
  begin
    I := 1;
    while I <= Length(Str) do
    begin
      if Str[I] = Chr(13) then
        Str := Copy(Str, 1, I) + Chr(10) + Copy(Str, I + 1, Length(Str) - I);
      Inc(I);
    end;
  end;

  // convert to 7 bit data
  procedure Force7BitData;
  var
    I: Integer;
  begin
    SetLength(Str, Count);
    for I := 1 to Length(Str) do
      Str[I] := Char(Byte(Sa[I]) and $0FFFFFFF);
  end;

  procedure Force8BitData;
  var
    I: Integer;
  begin
    SetLength(Str, Count);
    for I := 1 to Length(Str) do
      Str[I] := Char(Byte(Sa[I]));
  end;

begin
  SetLength(sa,count);
  Move(Buffer, Sa[1], Count);
//  Move(Buffer, Str[1], Count);
//  Str := AnsiString(sa);
  if FForce7Bit then
    Force7BitData
  else Force8BitData;
  if FAppendLF then
    AppendLineFeeds;
  StringReceived(Str);
end;

function TCustomComTerminal.GetConnected: Boolean;
begin
  Result := False;
  if FPtyDevice <> nil then
    Result := FPtyDevice.Connected;
end;

procedure TCustomComTerminal.SetConnected(const Value: Boolean);
begin
  if FPtyDevice <> nil then
    FPtyDevice.Connected := Value;
end;

procedure TCustomComTerminal.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd(Self);
  end;
end;

procedure TCustomComTerminal.SetColumns(const Value: Integer);
begin
  if Value <> FColumns then
  begin
    FColumns := Min(Max(2, Value), 256);
    AdjustSize;
    if not ((csLoading in ComponentState) or (csDesigning in ComponentState)) then
    begin
      FMainBuffer.Init(0, FColumns);
      FAlternateBuffer.Init(0, FColumns);
      if Assigned(FPtyDevice) then
        FPtyDevice.SetScreenSize(FColumns, FVisibleRows);
      Invalidate;
    end;
    UpdateScrollRange;
  end;
end;

procedure TCustomComTerminal.SetRows(const Value: Integer);
var
  ARows: Integer;
begin
  ARows := Max(Value, FVisibleRows);
  if ARows <> FRows then
  begin
    FRows := ARows;
    if not ((csLoading in ComponentState) or (csDesigning in ComponentState)) then
    begin
      FMainBuffer.Init(FRows, 0);
    end;
    UpdateScrollRange;
  end;
end;

procedure TCustomComTerminal.SetEmulation(const Value: TTermEmulation);
begin
  if FEmulation <> Value then
  begin
    FEmulation := Value;
    if not (csLoading in ComponentState) then
    begin
      FEscapeCodes.Free;
      CreateEscapeCodes;
    end;
  end;
end;

procedure TCustomComTerminal.SetCaret(const Value: TTermCaret);
begin
  if Value <> FCaret then
  begin
    FCaret := Value;
    if Focused then
    begin
      DestroyCaret(Handle);
      InitCaret;
    end;
  end;
end;

procedure TCustomComTerminal.SetPtyDevice(const Value: TCustomPtyDevice);
begin
  if Value <> FPtyDevice then
  begin
    if FPtyDevice <> nil then
    begin
      FPtyDevice.OnRxBuf:= nil;
    end;
    FPtyDevice := Value;
    if FPtyDevice <> nil then
    begin
      FPtyDevice.OnRxBuf:= RxBuf;
      FPtyDevice.FreeNotification(Self);
      FPtyDevice.SetScreenSize(FColumns, FVisibleRows);
    end;
  end;
end;

end.
