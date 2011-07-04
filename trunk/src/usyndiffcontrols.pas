unit uSynDiffControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEdit, SynEditMarkupSpecialLine,
  SynEditMiscClasses, SynGutterBase, SynTextDrawer, uDiff;

const
  { Fake line kinds }
  lkFakeAdd = -1;
  lkFakeDelete = -2;
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
      function FormatLineNumber(Line: integer; IsFakeLine: boolean): string;
    protected
      procedure Init; override;
      function  PreferedWidth: Integer; override;
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
    FDiff: TDiff;
    FSpecialLineMarkupEvent: TSpecialLineMarkupEvent;
    FDiffCount: Integer;
    FEncoding: String;
    FColors: TDiffColors;
    FOriginalFile,
    FModifiedFile: TSynDiffEdit;
  private
    function GetDiffCount: Integer;
    function GetDiffKind(Index: Integer): TChangeKind;
    function GetLineNumber(Index: Integer): PtrInt;
    procedure SetLineNumber(Index: Integer; const AValue: PtrInt);
    procedure SetModifiedFile(const AValue: TSynDiffEdit);
    procedure SetOriginalFile(const AValue: TSynDiffEdit);
    procedure SetPaintStyle(const AValue: TPaintStyle);
  protected
    procedure SpecialLineMarkupEvent(Sender: TObject; Line: Integer;
                                     var Special: boolean; AMarkup: TSynSelectedColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InsertFakeLine(AIndex: Integer; ADiffKind: PtrInt);
    procedure RemoveFakeLines(Strings: TStrings);
    procedure BeginCompare(ADiff: TDiff);
    procedure EndCompare(ADiffCount: Integer);
    function DiffBegin(ALine: Integer): Integer;
    function DiffEnd(ALine: Integer): Integer;
    property PaintStyle: TPaintStyle read FPaintStyle write SetPaintStyle;
    property Diff: TDiff read FDiff write FDiff;
    property DiffKind[Index: Integer]: TChangeKind read GetDiffKind;
    property DiffCount: Integer read GetDiffCount;
    property LineNumber[Index: Integer]: PtrInt read GetLineNumber write SetLineNumber;
    property Encoding: String read FEncoding write FEncoding;
    property Colors: TDiffColors read FColors write FColors;
    property OriginalFile: TSynDiffEdit read FOriginalFile write SetOriginalFile;
    property ModifiedFile: TSynDiffEdit read FModifiedFile write SetModifiedFile;
  published
    property OnStatusChange;
  end;

implementation

uses
  LCLIntf, LCLType, SynEditMiscProcs;

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

function TSynDiffEdit.GetLineNumber(Index: Integer): PtrInt;
begin
  Result:= PtrInt(Lines.Objects[Index]);
end;

procedure TSynDiffEdit.SetLineNumber(Index: Integer; const AValue: PtrInt);
begin
  Lines.Objects[Index]:= TObject(AValue);
end;

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
  if Line > DiffCount then Exit;

  Kind:= DiffKind[Line - 1];

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

procedure TSynDiffEdit.BeginCompare(ADiff: TDiff);
begin
  FDiff:= ADiff;
  BeginUpdate;
  // Remove fake lines
  RemoveFakeLines(Lines);
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
  FColors:= TDiffColors.Create;
  OnSpecialLineMarkup:= @SpecialLineMarkupEvent;
end;

destructor TSynDiffEdit.Destroy;
begin
  if Assigned(FColors) then
    FreeAndNil(FColors);
  inherited Destroy;
end;

procedure TSynDiffEdit.InsertFakeLine(AIndex: Integer; ADiffKind: PtrInt);
begin
  Lines.InsertObject(AIndex, EmptyStr, TObject(ADiffKind));
end;

procedure TSynDiffEdit.RemoveFakeLines(Strings: TStrings);
var
  I: Integer;
begin
  for I:= Strings.Count - 1 downto 0 do
  begin
    if (PtrInt(Strings.Objects[I]) < 0) and (Strings[I] = EmptyStr) then
      Strings.Delete(I);
  end;
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
    iLine := FoldView.TextIndex[I];
    // next line rect
    rcLine.Top := rcLine.Bottom;
    Inc(rcLine.Bottom, LineHeight);
    if iLine >= SynDiffEdit.DiffCount then Continue;
    case SynDiffEdit.DiffKind[iLine] of
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

function TSynDiffGutterLineNumber.FormatLineNumber(Line: Integer; IsFakeLine: Boolean): String;
var
  I: Integer;
begin
  Result := EmptyStr;
  // if a symbol must be showed
  if IsFakeLine then
    begin
      case Line of
      lkFakeAdd:
        Result := StringOfChar(' ', FAutoSizeDigitCount-1) + '+';
      lkFakeDelete:
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

procedure TSynDiffGutterLineNumber.Init;
begin
  inherited Init;
  FTextDrawer := Gutter.TextDrawer;
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
      iLine := FoldView.DisplayNumber[I];
      // next line rect
      rcLine.Top := rcLine.Bottom;
      if Assigned(SynDiffEdit.FDiff) and (SynDiffEdit.DiffCount <> 0) then
        begin
          iLine:= SynDiffEdit.LineNumber[iLine - 1];
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

end.

