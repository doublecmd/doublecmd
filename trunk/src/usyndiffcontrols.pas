unit uSynDiffControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEdit, SynEditMarkupSpecialLine,
  SynEditMiscClasses, SynGutterBase, SynEditFoldedView,
  SynTextDrawer, uDiff;

const
  clPaleGreen: TColor = $AAFFAA;
  clPaleRed  : TColor = $AAAAFF;
  clPaleBlue : TColor = $FFAAAA;

type
  TPaintStyle = (psForeground, psBackground);

type

  { TSynDiffGutterLineNumber }

    TSynDiffGutterLineNumber = class(TSynGutterPartBase)
    private
      FFoldView: TSynEditFoldedView;
      FTextDrawer: TheTextDrawer;

      FDigitCount: integer;
      FAutoSizeDigitCount: integer;
      FLeadingZeros: boolean;

      procedure SetDigitCount(AValue : integer);
      procedure SetLeadingZeros(const AValue : boolean);
      function FormatLineNumber(Line: integer; IsFakeLine: boolean): string;
    protected
      procedure DoChange(Sender: TObject); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;

      procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: Integer); override;
      procedure AutoSizeDigitCount(LinesCount: integer);
      function RealGutterWidth(CharWidth: integer): integer;  override;
    published
      property MarkupInfo;
      property DigitCount: integer read FDigitCount write SetDigitCount;
      property LeadingZeros: boolean read FLeadingZeros write SetLeadingZeros;
    end;

  { TSynDiffGutterChanges }

    TSynDiffGutterChanges = class(TSynGutterPartBase)
    private
      FFoldView: TSynEditFoldedView;
      FAddedColor: TColor;
      FModifiedColor: TColor;
      FDeletedColor: TColor;
    protected
      procedure DoChange(Sender: TObject); override;
    public
      constructor Create(AOwner: TComponent); override;

      procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: Integer); override;
      function RealGutterWidth(CharWidth: integer): Integer;  override;
    published
      property AddedColor: TColor read FAddedColor write FAddedColor;
      property ModifiedColor: TColor read FModifiedColor write FModifiedColor;
      property DeletedColor: TColor read FDeletedColor write FDeletedColor;
    end;

  { TSynDiffEdit }

  TSynDiffEdit = class(TCustomSynEdit)
  private
    FPaintStyle: TPaintStyle;
    FDiff: TDiff;
    FSpecialLineMarkupEvent: TSpecialLineMarkupEvent;
    FDiffCount: Integer;
  private
    function GetDiffCount: Integer;
    function GetDiffKind(Index: Integer): TChangeKind;
    procedure SetPaintStyle(const AValue: TPaintStyle);
  protected
    procedure SpecialLineMarkupEvent(Sender: TObject; Line: Integer;
                                     var Special: boolean; AMarkup: TSynSelectedColor);
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeginCompare(ADiff: TDiff);
    procedure EndCompare(ADiffCount: Integer);
    function DiffBegin(ALine: Integer): Integer;
    function DiffEnd(ALine: Integer): Integer;
    property PaintStyle: TPaintStyle read FPaintStyle write SetPaintStyle;
    property Diff: TDiff read FDiff write FDiff;
    property DiffKind[Index: Integer]: TChangeKind read GetDiffKind;
    property DiffCount: Integer read GetDiffCount;
  published
    property OnStatusChange;
  end;

implementation

uses
  LCLIntf, LCLType, SynEditMiscProcs;

{ TSynDiffEdit }

function TSynDiffEdit.GetDiffCount: Integer;
begin
  if Assigned(FDiff) then
    Result:= FDiff.Count
  else
    Result:= FDiffCount;
end;

function TSynDiffEdit.GetDiffKind(Index: Integer): TChangeKind;
begin
  if Assigned(FDiff) then
    Result:= FDiff.Compares[Index].Kind
  else
    begin
      if PtrInt(Lines.Objects[Index]) = 0 then
        Result:= ckNone
      else
        Result:= ckModify;
    end;
end;

procedure TSynDiffEdit.SetPaintStyle(const AValue: TPaintStyle);
begin
  if FPaintStyle <> AValue then
  begin
    FPaintStyle := AValue;
    Invalidate;
  end;
end;

procedure TSynDiffEdit.SpecialLineMarkupEvent(Sender: TObject; Line: Integer;
  var Special: boolean; AMarkup: TSynSelectedColor);
var
  Kind: TChangeKind;
  LineColor: TColor;
begin
  if Line >= DiffCount then Exit;

  Kind:= DiffKind[Line - 1];
  Special:= (Kind <> ckNone);

  if Special then
  with AMarkup do
  begin
    case Kind of
      ckModify: LineColor := clPaleBlue;
      ckDelete: LineColor := clPaleRed;
      ckAdd:    LineColor := clPaleGreen;
    end;
    if FPaintStyle = psForeground then
      begin
        Foreground := LineColor;
        Background := clWindow;
      end
    else
      begin
        Foreground:= clWindowText;
        Background := LineColor;
      end;
  end;
end;

procedure TSynDiffEdit.BeginCompare(ADiff: TDiff);
begin
  FDiff:= ADiff;
  BeginUpdate;
end;

procedure TSynDiffEdit.EndCompare(ADiffCount: Integer);
begin
  FDiffCount:= ADiffCount;
  EndUpdate;
end;

function TSynDiffEdit.DiffBegin(ALine: Integer): Integer;
var
  Kind: TChangeKind;
begin
  Result:= ALine;
  if ALine = 0 then Exit;
  // Skip lines with current difference type
  Kind := DiffKind[ALine];
  while (ALine > 0) and (DiffKind[ALine] = Kind) do Dec(ALine);
  Result:= ALine + 1;
end;

function TSynDiffEdit.DiffEnd(ALine: Integer): Integer;
var
  Kind: TChangeKind;
begin
  Result:= ALine;
  if ALine = Lines.Count - 1 then Exit;
  // Skip lines with current difference type
  Kind := DiffKind[ALine];
  while (ALine < Lines.Count - 1) and (DiffKind[ALine] = Kind) do Inc(ALine);
  Result:= ALine - 1;
end;

constructor TSynDiffEdit.Create(AOwner: TComponent);
begin
  FDiff:= nil;
  inherited Create(AOwner);
  if not(csLoading in AOwner.ComponentState) then
    begin
      Gutter.Parts.Clear;
      with TSynDiffGutterLineNumber.Create(Gutter.Parts) do
      Name:= 'SynDiffGutterLineNumber';
      with TSynDiffGutterChanges.Create(Gutter.Parts) do
      Name:= 'SynDiffGutterChanges';
    end;
  FPaintStyle:= psBackground;
  OnSpecialLineMarkup:= @SpecialLineMarkupEvent;
end;

{ TSynDiffGutterChanges }

procedure TSynDiffGutterChanges.DoChange(Sender: TObject);
begin
  if AutoSize then
    FWidth := 4;
  inherited DoChange(Sender);
end;

constructor TSynDiffGutterChanges.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFoldView := Gutter.FoldView;

  FAddedColor := clPaleGreen;
  FModifiedColor := clPaleBlue;
  FDeletedColor := clPaleRed;
  MarkupInfo.Background := clNone;

  FWidth := 4;
end;

procedure TSynDiffGutterChanges.Paint(Canvas: TCanvas; AClip: TRect; FirstLine,
  LastLine: Integer);
var
  I, iLine: Integer;
  LineHeight: Integer;
  rcLine: TRect;
  SynDiffEdit: TSynDiffEdit;
begin
  if not Visible then Exit;

  SynDiffEdit:= TSynDiffEdit(SynEdit);
  LineHeight := SynDiffEdit.LineHeight;

  if MarkupInfo.Background <> clNone then
  begin
    Canvas.Brush.Color := MarkupInfo.Background;
    Canvas.FillRect(AClip);
  end;

  Canvas.Pen.Width := Width;
  Canvas.Pen.EndCap:= pecFlat;

  rcLine := AClip;
  rcLine.Left := rcLine.Left + Width div 2;
  rcLine.Bottom := FirstLine * LineHeight;
  for I := FirstLine to LastLine do
  begin
    iLine := FFoldView.TextIndex[I];
    // next line rect
    rcLine.Top := rcLine.Bottom;
    Inc(rcLine.Bottom, LineHeight);
    if iLine >= SynDiffEdit.DiffCount then Continue;
    case SynDiffEdit.DiffKind[iLine] of
      ckNone:
          Continue;
      ckAdd:
          Canvas.Pen.Color := FAddedColor;
      ckDelete:
          Canvas.Pen.Color := FDeletedColor;
      ckModify:
          Canvas.Pen.Color := FModifiedColor;
    end;
    Canvas.Line(rcLine.Left, rcLine.Top + 1, rcLine.Left, rcLine.Bottom - 1);
  end;
end;

function TSynDiffGutterChanges.RealGutterWidth(CharWidth: integer): Integer;
begin
  if not Visible then
  begin
    Result := 0;
    Exit;
  end;

  if AutoSize then
    RealWidth := 4;
  Result := Width;
end;

{ TSynDiffGutterLineNumber }

procedure TSynDiffGutterLineNumber.SetDigitCount(AValue: integer);
begin
  AValue := MinMax(AValue, 2, 12);
  if FDigitCount <> AValue then
  begin
    FDigitCount := AValue;
    FAutoSizeDigitCount := FDigitCount;
    DoChange(Self);
  end;
end;

procedure TSynDiffGutterLineNumber.SetLeadingZeros(const AValue: boolean);
begin
  if FLeadingZeros <> AValue then
  begin
    FLeadingZeros := AValue;
    DoChange(Self);
  end;
end;

function TSynDiffGutterLineNumber.FormatLineNumber(Line: Integer; IsFakeLine: Boolean): String;
var
  I: Integer;
begin
  Result := EmptyStr;
  // if a dot must be showed
  if IsFakeLine then
    begin
      if Line = 0 then
        Result := StringOfChar(' ', FAutoSizeDigitCount-1) + '+'
      else
        Result := StringOfChar(' ', FAutoSizeDigitCount-1) + '-';
    end
  // else format the line number
  else
  begin
    Str(Line : FAutoSizeDigitCount, Result);
    if FLeadingZeros then
      for I := 1 to FAutoSizeDigitCount - 1 do
      begin
        if (Result[I] <> ' ') then Break;
        Result[I] := '0';
      end;
  end;
end;

procedure TSynDiffGutterLineNumber.DoChange(Sender: TObject);
begin
  if AutoSize then
    FWidth := RealGutterWidth(FTextDrawer.CharWidth);
  inherited DoChange(Sender);
end;

constructor TSynDiffGutterLineNumber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFoldView := Gutter.FoldView;
  FTextDrawer := Gutter.TextDrawer;

  FDigitCount := 4;
  FAutoSizeDigitCount := FDigitCount;
  FLeadingZeros := False;
  FWidth := 25;
end;

destructor TSynDiffGutterLineNumber.Destroy;
begin
  inherited Destroy;
end;

procedure TSynDiffGutterLineNumber.Assign(Source: TPersistent);
var
  Src: TSynDiffGutterLineNumber;
begin
  if Assigned(Source) and (Source is TSynDiffGutterLineNumber) then
  begin
    Src := TSynDiffGutterLineNumber(Source);
    FLeadingZeros := Src.FLeadingZeros;
    FDigitCount := Src.FDigitCount;
    FAutoSizeDigitCount := Src.FAutoSizeDigitCount;
  end;
  inherited Assign(Source);
end;

procedure TSynDiffGutterLineNumber.Paint(Canvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: Integer);
var
  I, iLine: Integer;
  rcLine: TRect;
  S: String;
  DC: HDC;
  FakeLine: Boolean;
  LineHeight: Integer;
  SynDiffEdit: TSynDiffEdit;
begin
  if not Visible then Exit;

  SynDiffEdit:= TSynDiffEdit(SynEdit);
  LineHeight := SynDiffEdit.LineHeight;
  // Changed to use fTextDrawer.BeginDrawing and fTextDrawer.EndDrawing only
  // when absolutely necessary.  Note: Never change brush / pen / font of the
  // canvas inside of this block (only through methods of fTextDrawer)!
  if MarkupInfo.Background <> clNone then
    Canvas.Brush.Color := MarkupInfo.Background
  else
    Canvas.Brush.Color := Gutter.Color;
  DC := Canvas.Handle;
  LCLIntf.SetBkColor(DC, Canvas.Brush.Color);
  FTextDrawer.BeginDrawing(DC);
  try
    if MarkupInfo.Background <> clNone then
      FTextDrawer.SetBackColor(MarkupInfo.Background)
    else
      FTextDrawer.SetBackColor(Gutter.Color);
    if MarkupInfo.Foreground <> clNone then
      fTextDrawer.SetForeColor(MarkupInfo.Foreground)
    else
      fTextDrawer.SetForeColor(SynDiffEdit.Font.Color);
    fTextDrawer.SetFrameColor(MarkupInfo.FrameColor);
    fTextDrawer.Style := MarkupInfo.Style;
    // prepare the rect initially
    rcLine := AClip;
    rcLine.Bottom := FirstLine * LineHeight;
    for I := FirstLine to LastLine do
    begin
      iLine := FFoldView.DisplayNumber[I];
      // next line rect
      rcLine.Top := rcLine.Bottom;
      if Assigned(SynDiffEdit.FDiff) and (SynDiffEdit.DiffCount <> 0) then
        begin
          iLine:= PtrInt(SynDiffEdit.Lines.Objects[iLine - 1]);
        end;
      FakeLine := (iLine <= 0);
      // Get the formatted line number or dot
      S := FormatLineNumber(iLine, FakeLine);
      Inc(rcLine.Bottom, LineHeight);
      // erase the background and draw the line number string in one go
      fTextDrawer.ExtTextOut(rcLine.Left, rcLine.Top, ETO_OPAQUE or ETO_CLIPPED, rcLine,
        PChar(Pointer(S)),Length(S));
    end;

    // now erase the remaining area if any
    if AClip.Bottom > rcLine.Bottom then
    begin
      rcLine.Top := rcLine.Bottom;
      rcLine.Bottom := AClip.Bottom;
      with rcLine do
        fTextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, rcLine, nil, 0);
    end;
  finally
    fTextDrawer.EndDrawing;
  end;
end;

procedure TSynDiffGutterLineNumber.AutoSizeDigitCount(LinesCount: integer);
var
  nDigits: integer;
begin
  if Visible and AutoSize then
  begin
    nDigits := Max(Length(IntToStr(LinesCount)), FDigitCount);
    if FAutoSizeDigitCount <> nDigits then
    begin
      FAutoSizeDigitCount := nDigits;
      DoChange(Self);
    end;
  end
  else
  if FAutoSizeDigitCount <> FDigitCount then begin
    FAutoSizeDigitCount := FDigitCount;
    DoChange(Self);
  end;
end;

function TSynDiffGutterLineNumber.RealGutterWidth(CharWidth: integer): integer;
begin
  if not Visible then
  begin
    Result := 0;
    Exit;
  end;

  if AutoSize then
    RealWidth := FAutoSizeDigitCount * CharWidth + 1;
  Result := Width;
end;

end.

