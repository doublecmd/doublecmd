unit uSynDiffControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEdit,
  SynEditMiscClasses, SynGutterBase, SynTextDrawer, SynEditTextBuffer,
  LazSynEditText, uDiffOND;

const
  { Default differ colors }
  clPaleGreen: TColor = $AAFFAA;
  clPaleRed  : TColor = $AAAAFF;
  clPaleBlue : TColor = $FFAAAA;

type
  TPaintStyle = (psForeground, psBackground);

type

  { TDiffColors }

  TDiffColors = class(TPersistent)
  private
    fColors: array [TChangeKind] of TColor;
    fOnChange: TNotifyEvent;
    function GetColor(const AIndex: TChangeKind): TColor;
    procedure SetColor(const AIndex: TChangeKind; const AValue: TColor);
  public
    constructor Create;
    procedure Assign(aSource: TPersistent); override;
    property Colors[const aIndex: TChangeKind]: TColor read GetColor write SetColor; default;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  published
    property Added: TColor index ckAdd read GetColor write SetColor;
    property Modified: TColor index ckModify read GetColor write SetColor;
    property Deleted: TColor index ckDelete read GetColor write SetColor;
  end;

  { TSynDiffGutterLineNumber }

  TSynDiffGutterLineNumber = class(TSynGutterPartBase)
  private
    FTextDrawer: TheTextDrawer;

    FDigitCount: integer;
    FAutoSizeDigitCount: integer;
    FLeadingZeros: boolean;

    procedure SetDigitCount(AValue : integer);
    procedure SetLeadingZeros(const AValue : boolean);
    function FormatLineNumber(Line: PtrInt; Kind: TChangeKind): string;
  protected
    procedure Init; override;
    function  PreferedWidth: Integer; override;
    procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
    procedure BufferChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: Integer); override;
  published
    property MarkupInfo;
    property DigitCount: integer read FDigitCount write SetDigitCount;
    property LeadingZeros: boolean read FLeadingZeros write SetLeadingZeros;
  end;

  { TSynDiffGutterChanges }

  TSynDiffGutterChanges = class(TSynGutterPartBase)
  private
    FColors: TDiffColors;
  protected
    function  PreferedWidth: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: Integer); override;
  published
    property Colors: TDiffColors read FColors write FColors;
  end;

  { TSynDiffEdit }

  TSynDiffEdit = class(TCustomSynEdit)
  private
    FPaintStyle: TPaintStyle;
    FEncoding: String;
    FColors: TDiffColors;
    FOriginalFile,
    FModifiedFile: TSynDiffEdit;
  private
    procedure SetModifiedFile(const AValue: TSynDiffEdit);
    procedure SetOriginalFile(const AValue: TSynDiffEdit);
    procedure SetPaintStyle(const AValue: TPaintStyle);
  protected
    procedure SpecialLineMarkupEvent(Sender: TObject; Line: Integer;
                                     var Special: boolean; AMarkup: TSynSelectedColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure StartCompare;
    procedure FinishCompare;
    function DiffBegin(ALine: Integer): Integer;
    function DiffEnd(ALine: Integer): Integer;
    property PaintStyle: TPaintStyle read FPaintStyle write SetPaintStyle;
    property Encoding: String read FEncoding write FEncoding;
    property Colors: TDiffColors read FColors write FColors;
    property OriginalFile: TSynDiffEdit read FOriginalFile write SetOriginalFile;
    property ModifiedFile: TSynDiffEdit read FModifiedFile write SetModifiedFile;
  published
    property OnStatusChange;
  end;

  { TStringsHelper }

  TStringsHelper = class helper for TStrings
  private
    function GetKind(AIndex: Integer): TChangeKind;
    function GetNumber(AIndex: Integer): PtrInt;
    procedure SetKind(AIndex: Integer; AValue: TChangeKind);
    procedure SetNumber(AIndex: Integer; AValue: PtrInt);
  public
    procedure RemoveFake;
    procedure Append(const S: String; AKind: TChangeKind);
    procedure InsertFake(AIndex: Integer; AKind: TChangeKind);
    procedure SetKindAndNumber(AIndex: Integer; AKind: TChangeKind; ANumber: PtrInt);
  public
    property Kind[AIndex: Integer]: TChangeKind read GetKind write SetKind;
    property Number[AIndex: Integer]: PtrInt read GetNumber write SetNumber;
  end;

implementation

uses
  LCLIntf, LCLType, SynEditMiscProcs;

const
  KindShift = 8;   // Line kind shift
  KindMask  = $FF; // Line kind mask
  FakeLine  = PtrInt(High(PtrUInt) shr KindShift);

{ TDiffColors }

function TDiffColors.GetColor(const AIndex: TChangeKind): TColor;
begin
  Result:= fColors[AIndex];
end;

procedure TDiffColors.SetColor(const AIndex: TChangeKind; const AValue: TColor);
begin
  if fColors[AIndex] <> AValue then
  begin
    fColors[AIndex] := AValue;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

constructor TDiffColors.Create;
begin
  fColors[ckAdd] := clPaleGreen;
  fColors[ckModify] := clPaleBlue;
  fColors[ckDelete] := clPaleRed;
end;

procedure TDiffColors.Assign(aSource: TPersistent);
begin
  if (aSource is TDiffColors) then
  with (aSource as TDiffColors) do
  begin
    fColors[ckAdd]:= Added;
    fColors[ckModify]:= Modified;
    fColors[ckDelete]:= Deleted;
  end;
end;

{ TSynDiffEdit }

procedure TSynDiffEdit.SetModifiedFile(const AValue: TSynDiffEdit);
begin
  if FModifiedFile <> AValue then
  begin
    if (AValue <> nil) and (FOriginalFile <> nil) then
      raise Exception.Create('Having both ModifiedFile and OriginalFile is not supported');
    FModifiedFile := AValue;
  end;
end;

procedure TSynDiffEdit.SetOriginalFile(const AValue: TSynDiffEdit);
begin
  if FOriginalFile <> AValue then
  begin
    if (AValue <> nil) and (FModifiedFile <> nil) then
      raise Exception.Create('Having both OriginalFile and ModifiedFile is not supported');
    FOriginalFile := AValue;
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
  if Line > Lines.Count then Exit;

  Kind:= Lines.Kind[Line - 1];

  if (Kind <> ckNone) then
  with AMarkup do
  begin
    case Kind of
      ckDelete: LineColor := FColors.Deleted;
      ckAdd:    LineColor := FColors.Added;
      ckModify:
        if Assigned(Highlighter) then
          Exit
        else
          LineColor := FColors.Modified;
    end;
    Special:= True;
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

procedure TSynDiffEdit.StartCompare;
begin
  BeginUpdate;
  // Remove fake lines
  Lines.RemoveFake;
end;

procedure TSynDiffEdit.FinishCompare;
begin
  EndUpdate;
  Invalidate;
end;

function TSynDiffEdit.DiffBegin(ALine: Integer): Integer;
var
  Kind: TChangeKind;
begin
  Result:= ALine;
  if ALine = 0 then Exit;
  // Skip lines with current difference type
  Kind := Lines.Kind[ALine];
  while (ALine > 0) and (Lines.Kind[ALine] = Kind) do Dec(ALine);
  Result:= ALine + 1;
end;

function TSynDiffEdit.DiffEnd(ALine: Integer): Integer;
var
  Kind: TChangeKind;
begin
  Result:= ALine;
  if ALine = Lines.Count - 1 then Exit;
  // Skip lines with current difference type
  Kind := Lines.Kind[ALine];
  while (ALine < Lines.Count - 1) and (Lines.Kind[ALine] = Kind) do Inc(ALine);
  Result:= ALine - 1;
end;

constructor TSynDiffEdit.Create(AOwner: TComponent);
begin
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
  FColors:= TDiffColors.Create;
  OnSpecialLineMarkup:= @SpecialLineMarkupEvent;
end;

destructor TSynDiffEdit.Destroy;
begin
  if Assigned(FColors) then
    FreeAndNil(FColors);
  inherited Destroy;
end;

{ TSynDiffGutterChanges }

function TSynDiffGutterChanges.PreferedWidth: Integer;
begin
  Result := 4;
end;

constructor TSynDiffGutterChanges.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FColors:= TDiffColors.Create;
  MarkupInfo.Background := clNone;
end;

destructor TSynDiffGutterChanges.Destroy;
begin
  if Assigned(FColors) then
    FreeAndNil(FColors);
  inherited Destroy;
end;

procedure TSynDiffGutterChanges.Paint(Canvas: TCanvas; AClip: TRect; FirstLine,
  LastLine: Integer);
var
  rcLine: TRect;
  LineHeight: Integer;
  I, LineNumber: Integer;
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
    LineNumber := FoldView.TextIndex[I];
    // next line rect
    rcLine.Top := rcLine.Bottom;
    Inc(rcLine.Bottom, LineHeight);
    if (LineNumber >= 0) and (LineNumber < SynDiffEdit.Lines.Count) then
    begin
      case SynDiffEdit.Lines.Kind[LineNumber] of
        ckNone:
            Continue;
        ckAdd:
            Canvas.Pen.Color := FColors.Added;
        ckDelete:
            Canvas.Pen.Color := FColors.Deleted;
        ckModify:
            Canvas.Pen.Color := FColors.Modified;
      end;
      Canvas.Line(rcLine.Left, rcLine.Top + 1, rcLine.Left, rcLine.Bottom - 1);
    end;
  end;
end;

{ TSynDiffGutterLineNumber }

procedure TSynDiffGutterLineNumber.SetDigitCount(AValue: integer);
begin
  AValue := MinMax(AValue, 2, 12);
  if FDigitCount <> AValue then
  begin
    FDigitCount := AValue;
    if AutoSize then begin
      FAutoSizeDigitCount := Max(FDigitCount, FAutoSizeDigitCount);
      DoAutoSize;
    end else
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

function TSynDiffGutterLineNumber.FormatLineNumber(Line: PtrInt;
  Kind: TChangeKind): string;
var
  I: Integer;
begin
  Result := EmptyStr;
  // if a symbol must be showed
  if (Line = 0) or (Line = FakeLine) then
    begin
      case Kind of
      ckAdd:
        Result := StringOfChar(' ', FAutoSizeDigitCount-1) + '+';
      ckDelete:
        Result := StringOfChar(' ', FAutoSizeDigitCount-1) + '-';
      else
        Result := StringOfChar(' ', FAutoSizeDigitCount-1) + '.';
      end;
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

function TSynDiffGutterLineNumber.PreferedWidth: Integer;
begin
  Result := FAutoSizeDigitCount * FTextDrawer.CharWidth + 1;
end;

procedure TSynDiffGutterLineNumber.LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
var
  nDigits: Integer;
begin
  if not (Visible and AutoSize) then Exit;

  nDigits := Max(Length(IntToStr(TextBuffer.Count)), FDigitCount);
  if FAutoSizeDigitCount <> nDigits then begin
    FAutoSizeDigitCount := nDigits;
    DoAutoSize;
  end;
end;

procedure TSynDiffGutterLineNumber.BufferChanged(Sender: TObject);
begin
  TSynEditStringList(Sender).RemoveHanlders(self);
  TSynEditStringList(TextBuffer).AddChangeHandler(senrLineCount, @LineCountChanged);
  TSynEditStringList(TextBuffer).AddNotifyHandler(senrTextBufferChanged, @BufferChanged);
  LineCountChanged(nil, 0, 0);
end;

procedure TSynDiffGutterLineNumber.FontChanged(Sender: TObject);
begin
  DoAutoSize;
end;

procedure TSynDiffGutterLineNumber.Init;
begin
  inherited Init;
  FTextDrawer := Gutter.TextDrawer;
  TSynEditStringList(TextBuffer).AddChangeHandler(senrLineCount, @LineCountChanged);
  TSynEditStringList(TextBuffer).AddNotifyHandler(senrTextBufferChanged, @BufferChanged);
  FTextDrawer.RegisterOnFontChangeHandler(@FontChanged);
  LineCountchanged(nil, 0, 0);
end;

constructor TSynDiffGutterLineNumber.Create(AOwner: TComponent);
begin
  FDigitCount := 4;
  FAutoSizeDigitCount := FDigitCount;
  FLeadingZeros := False;
  inherited Create(AOwner);
end;

destructor TSynDiffGutterLineNumber.Destroy;
begin
  TSynEditStringList(TextBuffer).RemoveHanlders(Self);
  FTextDrawer.UnRegisterOnFontChangeHandler(@FontChanged);
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
  DC: HDC;
  S: String;
  rcLine: TRect;
  LineNumber: PtrInt;
  LineKind: TChangeKind;
  I, LineHeight: Integer;
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
  {$PUSH}{$R-}
  LCLIntf.SetBkColor(DC, Canvas.Brush.Color);
  {$POP}
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
      LineNumber := FoldView.DisplayNumber[I];
      LineKind := SynDiffEdit.Lines.Kind[LineNumber - 1];
      LineNumber:= SynDiffEdit.Lines.Number[LineNumber - 1];
      // next line rect
      rcLine.Top := rcLine.Bottom;
      // Get the formatted line number or dot
      S := FormatLineNumber(LineNumber, LineKind);
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

{ TStringsHelper }

function TStringsHelper.GetKind(AIndex: Integer): TChangeKind;
var
  AKind: PtrInt;
begin
  AKind:= PtrInt(Objects[AIndex]);
  Result:= TChangeKind(AKind and KindMask);
end;

function TStringsHelper.GetNumber(AIndex: Integer): PtrInt;
begin
  Result:= PtrInt(Objects[AIndex]) shr KindShift;
end;

procedure TStringsHelper.SetKind(AIndex: Integer; AValue: TChangeKind);
var
  ANumber: PtrInt;
begin
  ANumber:= GetNumber(AIndex);
  Objects[AIndex]:= TObject(PtrInt(AValue) or (ANumber shl KindShift));
end;

procedure TStringsHelper.SetNumber(AIndex: Integer; AValue: PtrInt);
var
  AKind: TChangeKind;
begin
  AKind:= GetKind(AIndex);
  Objects[AIndex]:= TObject(PtrInt(AKind) or (AValue shl KindShift));
end;

procedure TStringsHelper.RemoveFake;
var
  I: Integer;
begin
  for I:= Count - 1 downto 0 do
  begin
    if ((PtrInt(Objects[I]) shr KindShift) = FakeLine) and (Self[I] = EmptyStr) then
      Delete(I);
  end;
end;

procedure TStringsHelper.Append(const S: String; AKind: TChangeKind);
begin
  InsertObject(Count, S, TObject(PtrInt(AKind) or (Count shl KindShift)));
end;

procedure TStringsHelper.InsertFake(AIndex: Integer; AKind: TChangeKind);
begin
  InsertObject(AIndex, EmptyStr, TObject(PtrInt(AKind) or PtrInt(FakeLine shl KindShift)));
end;

procedure TStringsHelper.SetKindAndNumber(AIndex: Integer; AKind: TChangeKind;
  ANumber: PtrInt);
begin
  Objects[AIndex]:= TObject(PtrInt(AKind) or (ANumber shl KindShift));
end;

end.

