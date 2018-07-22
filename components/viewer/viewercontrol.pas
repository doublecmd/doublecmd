{
   Component ViewerControl (Free Pascal)
   show file in text (wraped or not) or bin or hex mode

   This is part of Seksi Commander

   To searching use uFindMmap,
   to movement call Upxxxx, Downxxxx, or set Position

   Realised under GNU GPL 2
   author Radek Cervinka (radek.cervinka@centrum.cz)

   changes:
   5.8
     - customizable view area of transformed text(transformed Values per Line)
     - customizable separators
     - added view as decimal
     - added class TCharToCustomValueTransformProc to handle parameters of customizable modes(at this time - Hex and Dec)
     - Hex and Dec mode handled by the same mechanism(potentially can be added any additionsl custom mode)
       so added properties FHex and FDec
     - added property FCustom which is nil if current mode - not custom, or is equal to current customizable mode
     - Added CopyToClipboardF which copy transformed text to clipboard

   5.7. (RC)
     - selecting text with mouse
     - CopyToclipBoard, SelectAll
   ?.6. - LoadFromStdIn and loading first 64Kb of files with size=0 :) (/proc fs ..)
   17.6. (RC)
     - mapfile (in error set FMappedFile=nil)
     - writetext TABs fixed (tab is replaced by 9 spaces)
     - set correct position for modes hex, bin (SetPosition)
   21.7
     - wrap text on 80 character lines works better now (by Radek Polak)
     - problems with function UpLine for specific lines:
       (lines of 80(=cTextWidth) character ended with ENTER (=#10)
   6.2. (RC)
     - ported to fpc for linux (CustomControl and gtk)
   7.2. (RC)
     - use temp to new implementation of LoadFromStdIn (and mmap temp file)
     - faster drawing of text (I hope)

   contributors:

   Copyright (C) 2006-2018 Alexander Koblov (alexx2000@mail.ru)


   TODO:
   a) File mapping blocks writing into file by other processes.
      Either:
      + Open small text files by reading them all into memory (done).
      - Add optional custom loading/caching portions of file in memory
        and only reading from file when neccessary.

   b) Searching in Unicode encodings and case-insensitive searching.

   c) Selecting text does not work well with composed Unicode characters
      (characters that are composed of multiple Unicode characters).

   d) Drawing/selecting text does not work correctly with RTL (right to left) text.

   e) FTextHeight is unreliable with complex unicode characters. It should be
      calculated based on currently displayed text (get max from each line's height).

}

unit ViewerControl;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, StdCtrls, LCLVersion, fgl;

const
  MaxMemSize = $400000; // 4 Mb

type
  TViewerControlMode = (vcmBin, vcmHex, vcmText, vcmWrap, vcmBook, vcmDec);
  TDataAccess = (dtMmap, dtNothing);
  TCharSide = (csBefore, csLeft, csRight, csAfter);

  TPtrIntList = specialize TFPGList<PtrInt>;

  TGuessEncodingEvent = function(const s: string): string;

  TCustomCharsPresentation = class;
  TCharToCustomValueTransformProc = function(AChar:AnsiChar;AMaxDigitsCount:integer):AnsiString of object;

  { TCustomCharsPresentation }

  {
    Presentation one char is called Value
    Function for convert char to Value is ChrToValueProc
  }

  TCustomCharsPresentation = class
  public
    ValuesPerLine  :integer;  // = 16 for Hex by default
    MaxValueDigits :integer;  // the max width of present  char (255) - 3 symbols

    MaxAddrDigits  :integer;  // = 8;
    StartOfs       :integer;  // = OffsetWidth + 2;  // ': '
    EndOfs         :integer;  // = StartOfs + (ValuesPerLine * (ValueMaxDigits+SpaceCount));
    StartAscii     :integer;  // = StartOfs + (ValuesPerLine * (ValueMaxDigits+SpaceCount)) + 2;  // '  '

    SpaceCount     :integer;  // = 1 - one spacebar between Values
    SeparatorSpace :AnsiString;  // spacebar * SpaceCount

    SeparatorChar  :AnsiChar; //  '|'
    CountSeperate  :integer;  // insert SeparatorChar after every CountSeperate values

    ChrToValueProc   :TCharToCustomValueTransformProc; // procedure which return presentation of one char

    constructor Create(APresentValuesPerLine,ACharMaxPresentWidth,AOffsetWidth,ACountSeparate:integer;AChrToValueProc:TCharToCustomValueTransformProc);
    destructor  Destroy();override;
  end;




type
  // If additional encodings are added they should be also supported by:
  // - GetNextCharAsAscii
  // - GetPrevCharAsAscii
  // - GetNextCharAsUtf8
  // - ConvertToUTF8
  // - UpdateSelection
  TViewerEncoding = (veAutoDetect,
                     veUtf8,
                     veUtf8bom,
                     veAnsi,
                     veOem,
                     veCp1250,
                     veCp1251,
                     veCp1252,
                     veCp1253,
                     veCp1254,
                     veCp1255,
                     veCp1256,
                     veCp1257,
                     veCp1258,
                     veCp437,
                     veCp850,
                     veCp852,
                     veCp866,
                     veCp874,
                     veCp932,
                     veCp936,
                     veCp949,
                     veCp950,
                     veIso88591,
                     veIso88592,
                     veKoi8,
                     veUcs2le,
                     veUcs2be,
                     veUtf16le,
                     veUtf16be,
                     veUtf32le,  // = ucs4le
                     veUtf32be); // = ucs4be

  TViewerEncodings = set of TViewerEncoding;

const
  ViewerEncodingsNames: array [TViewerEncoding] of string =
                   ('Auto-detect',
                    'UTF-8',
                    'UTF-8BOM',
                    'ANSI',
                    'OEM',
                    'CP1250',
                    'CP1251',
                    'CP1252',
                    'CP1253',
                    'CP1254',
                    'CP1255',
                    'CP1256',
                    'CP1257',
                    'CP1258',
                    'CP437',
                    'CP850',
                    'CP852',
                    'CP866',
                    'CP874',
                    'CP932',
                    'CP936',
                    'CP949',
                    'CP950',
                    'ISO-8859-1',
                    'ISO-8859-2',
                    'KOI-8',
                    'UCS-2LE',
                    'UCS-2BE',
                    'UTF-16LE',
                    'UTF-16BE',
                    'UTF-32LE',
                    'UTF-32BE');

const
  ViewerEncodingMultiByte: TViewerEncodings = [
    veUtf8, veUtf8bom, veUcs2le, veUcs2be,
    veUtf16le, veUtf16be, veUtf32le, veUtf32be];

  ViewerEncodingDoubleByte: TViewerEncodings = [
    veUcs2le, veUcs2be, veUtf16le, veUtf16be   ];

type


  { TViewerControl }

  TViewerControl = class(TCustomControl)
  protected
    FEncoding:           TViewerEncoding;
    FViewerControlMode:  TViewerControlMode;
    FFileName:           String;
    FFileHandle:         THandle;
    FFileSize:           Int64;
    FMappingHandle:      THandle;
    FMappedFile:         Pointer;
    FPosition:           PtrInt;
    FHPosition:          Integer;  // Tab for text during horizontal scroll
    FHLowEnd:            Integer;  // End for HPosition (string with max char)
    FVisibleOffset:      PtrInt;   // Offset in symbols for current line (see IsVisible and MakeVisible)
    FLowLimit:           PtrInt;   // Lowest possible value for Position
    FHighLimit:          PtrInt;   // Position cannot reach this value
    FBOMLength:          Integer;
    FLineList:           TPtrIntList;
    FBlockBeg:           PtrInt;
    FBlockEnd:           PtrInt;
    FMouseBlockBeg:      PtrInt;
    FMouseBlockSide:     TCharSide;
    FSelecting:          Boolean;
    FTextWidth: 	 Integer; // max char count or width in window
    FTextHeight:         Integer; // measured values of font, rec calc at font changed
    FScrollBarVert:      TScrollBar;
    FScrollBarHorz:      TScrollBar;
    FOnPositionChanged:  TNotifyEvent;
    FUpdateScrollBarPos: Boolean; // used to block updating of scrollbar
    FScrollBarPosition:  Integer;  // for updating vertical scrollbar based on Position
    FHScrollBarPosition: Integer;  // for updating horizontal scrollbar based on HPosition
    FColCount:           Integer;
    FOnGuessEncoding:    TGuessEncodingEvent;

    FHex:TCustomCharsPresentation;
    FDec:TCustomCharsPresentation;
    FCustom:TCustomCharsPresentation;

    function GetPercent: Integer;
    procedure SetPercent(const AValue: Integer);
    procedure SetBlockBegin(const AValue: PtrInt);
    procedure SetBlockEnd(const AValue: PtrInt);
    procedure SetPosition(Value: PtrInt); virtual;
    procedure SetHPosition(Value: Integer);
    procedure SetPosition(Value: PtrInt; Force: Boolean); overload;
    procedure SetHPosition(Value: Integer; Force: Boolean); overload;
    procedure SetEncoding(AEncoding: TViewerEncoding);
    function GetEncodingName: string;
    procedure SetEncodingName(AEncodingName: string);
    procedure SetViewerMode(Value: TViewerControlMode);
    procedure SetColCount(const AValue: Integer);

    {en
       Returns how many lines (given current FTextHeight) will fit into the window.
    }
    function GetClientHeightInLines: Integer; inline;

    {en
       Calculates how many lines can be displayed from given position.
       param(FromPosition
              Position from which to check. It should point to a start of a line.)
       @param(LastLineReached
              If it is set to @true when the function returns, then the last
              line of text was reached when scanning.
              This means that there are no more lines to be displayed other than
              the ones scanned from FromPosition. In other words:
                SetPosition(GetStartOfNextLine(FromPosition)) will be one line
                too many and will be scrolled back.)
    }
    function GetLinesTillEnd(FromPosition: PtrInt; out LastLineReached: Boolean): Integer;

    function GetBomLength: Integer;

    procedure UpdateLimits;

    {en
       @param(iStartPos
              Should point to start of a line.
              It is increased by the amount of parsed data (with line endings).)
       @param(aLimit
              Position which cannot be reached while reading from file.)
       @param(DataLength
              It is length in bytes of parsed data without any line endings.
              iStartPos is moved beyond the line endings though.)
    }
    function CalcTextLineLength(var iStartPos: PtrInt; const aLimit: Int64; out DataLength: PtrInt): Integer;

    function GetStartOfLine(aPosition: PtrInt): PtrInt;
    function GetEndOfLine(aPosition: PtrInt): PtrInt;

    function GetStartOfPrevLine(aPosition: PtrInt): PtrInt;
    function GetStartOfNextLine(aPosition: PtrInt): PtrInt;

    {en
       Changes the value of aPosition to X lines back or forward.
       @param(aPosition
              File position to change.)
       @param(iLines
              Nr of lines to scroll.
              If positive the position is increased by iLines lines,
              if negative the position is decreased by -iLines lines.)
    }
    function ScrollPosition(var aPosition: PtrInt; iLines: Integer): Boolean;

    {en
       Calculates (x,y) cursor position to a position within file.
       @param(x
              Client X coordinate of mouse cursor.)
       @param(y
              Client Y coordinate of mouse cursor.)
       @param(CharSide
              To which side of a character at returned position the (x,y) points to.
              Only valid if returned position is not -1.)
       @returns(Position in file to which (x,y) points to, based on what is
                currently displayed.
                Returns -1 if (x,y) doesn't point to any position (outside of
                the text for example).)
    }
    function XYPos2Adr(x, y: Integer; out CharSide: TCharSide): PtrInt;

    procedure OutText(x, y: Integer; StartPos: PtrInt; DataLength: Integer);
    procedure OutBin(x, y: Integer; sText: string; StartPos: PtrInt; DataLength: Integer);

    procedure OutCustom(x, y: Integer; sText: string;StartPos: PtrInt; DataLength: Integer);  // render one line
    function  TransformCustom(var APosition: PtrInt; ALimit: PtrInt;AWithAdditionalData:boolean=True): AnsiString;
    function  TransformCustomBlock(var APosition: PtrInt; DataLength: integer ; ASeparatorsOn, AAlignData:boolean; out AChars:AnsiString): AnsiString;

    function HexToValueProc(AChar:AnsiChar;AMaxDigitsCount:integer):AnsiString;
    function DecToValueProc(AChar:AnsiChar;AMaxDigitsCount:integer):AnsiString;

    procedure WriteBin;
    procedure WriteText;
    procedure WriteCustom; virtual;
    function  TransformText(const sText: String; const Xoffset: Integer): String;
    function  TransformBin(var aPosition: PtrInt; aLimit: PtrInt): AnsiString;
    function  TransformHex(var aPosition: PtrInt; aLimit: PtrInt): AnsiString;virtual;

    procedure AddLineOffset(const iOffset: PtrInt); inline;

    function MapFile(const sFileName: String): Boolean;
    procedure UnMapFile;
    procedure SetFileName(const sFileName: String);

    procedure UpdateScrollbars;

    procedure ViewerResize(Sender: TObject);

    {en
       Returns next unicode character from the file, depending on Encoding.
       It is a faster version, which does as little conversion as possible,
       but only Ascii values are guaranteed to be valid (0-127).
       Other unicode values may/may not be valid, so shouldn't be tested.
       This function is used for reading pure ascii characters such as
       line endings, tabs, white spaces, etc.
    }
    function GetNextCharAsAscii(const iPosition: PtrInt; out CharLenInBytes: Integer): Cardinal;
    function GetPrevCharAsAscii(const iPosition: PtrInt; out CharLenInBytes: Integer): Cardinal;

    {en
       Retrieve next character from the file depending on encoding and
       automatically convert it to UTF-8.
       If CharLenInBytes is greater than 0 but the result is an empty string
       then it's possible there was no appropriate UTF-8 character for the
       next character of the current encoding.
    }
    function GetNextCharAsUtf8(const iPosition: PtrInt; out CharLenInBytes: Integer): String;

    procedure ReReadFile;

    {en
       Searches for an ASCII character.
       @param(aPosition
              Position from where the search starts.)
       @param(aMaxBytes
              How many bytes are available for reading.)
       @param(AsciiChars
              The function searches for any character that this string contains.)
       @param(bFindNotIn
              If @true searches for first character not included in AsciiChars.
              If @false searches for first character included in AsciiChars.)
    }
    function FindAsciiSetForward(aPosition, aMaxBytes: PtrInt;
                                 const AsciiChars: String;
                                 bFindNotIn: Boolean): PtrInt;

    {en
       Same as FindForward but it searches backwards from pAdr.
       aMaxBytes must be number of available bytes for reading backwards from pAdr.
    }
    function FindAsciiSetBackward(aPosition, aMaxBytes: PtrInt;
                                  const AsciiChars: String;
                                  bFindNotIn: Boolean): PtrInt;

    {en
       Checks if current selection is still valid given current viewer mode and encoding.
       For example checks if selection is not in the middle of a unicode character.
    }
    procedure UpdateSelection;

    procedure ScrollBarVertScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ScrollBarHorzScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);

    function GetText(const StartPos, Len: PtrInt; const Xoffset: Integer): string;

  protected
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
{$if lcl_fullversion >= 1070000}
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double); override;
{$endif}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    {en
       Scrolls the displayed text in the window.

       @param(iLines
              Nr of lines to scroll.
              If positive the text is scrolled downwards,
              if negative the text is scrolled upwards.)
       @returns(@true if the text was scrolled.)
    }
    function Scroll(iLines: Integer): Boolean;
    function HScroll(iSymbols: Integer): Boolean;

    procedure PageUp;
    procedure PageDown;
    procedure GoHome;
    procedure GoEnd;

    procedure HPageUp;
    procedure HPageDown;
    procedure HGoHome;
    procedure HGoEnd;

    function GetDataAdr: Pointer;

    procedure SelectAll;
    procedure SelectText(AStart, AEnd: PtrInt);
    procedure CopyToClipboard;
    procedure CopyToClipboardF;
    function Selection: String;

    function IsVisible(const aPosition: PtrInt): Boolean; overload;
    procedure MakeVisible(const aPosition: PtrInt);

    function ConvertToUTF8(const sText: AnsiString): String;
    function ConvertFromUTF8(const sText: String): AnsiString;
    function FindUtf8Text(iStartPos: PtrInt; const sSearchText: String;
                          bCaseSensitive: Boolean; bSearchBackwards: Boolean): PtrInt;

    procedure ResetEncoding;
    function IsFileOpen: Boolean; inline;
    function DetectEncoding: TViewerEncoding;
    procedure GetSupportedEncodings(List: TStrings);

    property Percent: Integer Read GetPercent Write SetPercent;
    property Position: PtrInt Read FPosition Write SetPosition;
    property FileSize: Int64 Read FFileSize;
    property SelectionStart: PtrInt Read FBlockBeg Write SetBlockBegin;
    property SelectionEnd: PtrInt Read FBlockEnd Write SetBlockEnd;
    property EncodingName: string Read GetEncodingName Write SetEncodingName;
    property ColCount: Integer Read FColCount Write SetColCount;
    property OnGuessEncoding: TGuessEncodingEvent Read FOnGuessEncoding Write FOnGuessEncoding;

  published
    property Mode: TViewerControlMode Read FViewerControlMode Write SetViewerMode default vcmWrap;
    property FileName: String Read FFileName Write SetFileName;
    property Encoding: TViewerEncoding Read FEncoding Write SetEncoding default veAutoDetect;
    property OnPositionChanged: TNotifyEvent Read FOnPositionChanged Write FOnPositionChanged;

    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property OnMouseWheelUp;
    property OnMouseWheelDown;

    property Align;
    property Color;
    property Cursor default crIBeam;
    property Font;
    property ParentColor default False;
    property TabStop default True;
  end;

procedure Register;

implementation

uses
  LCLType, Graphics, Forms, LCLProc, Clipbrd, LConvEncoding,
  DCUnicodeUtils, LCLIntf, LazUTF8, DCOSUtils , DCConvertEncoding
  {$IF DEFINED(UNIX)}
  , BaseUnix, Unix, DCUnix
  {$ELSEIF DEFINED(WINDOWS)}
  , Windows, DCWindows
  {$ENDIF};


const
  //cTextWidth      = 80;  // wrap on 80 chars
  cBinWidth       = 80;
  cMaxTextWidth   = 1024; // maximum of chars on one line unwrapped text
  cTabSpaces      = 8;   // tab stop - allow to set in settings

  // These strings must be Ascii only.
  sNonCharacter: string = ' !"#$%&''()*+,-./:;<=>?@[\]^`{|}~'#13#10#9;
  sWhiteSpace  : string = ' '#13#10#9#8;

{ TCustomCharsPresentation }

constructor TCustomCharsPresentation.Create(APresentValuesPerLine,
  ACharMaxPresentWidth, AOffsetWidth, ACountSeparate: integer;AChrToValueProc:TCharToCustomValueTransformProc);
begin
  SpaceCount:=1;                             // count of spacebars between values, =1
  ValuesPerLine := APresentValuesPerLine;    // default for hex: 16 values
  MaxAddrDigits   := AOffsetWidth;             // = 8 , count of symbols for display caret offset
  StartOfs      := AOffsetWidth + 2;         // ': '
  MaxValueDigits    := ACharMaxPresentWidth; // hex char (FF) - 2 symbols,  dec char (255) - 3 symbols
  EndOfs        := StartOfs + (ValuesPerLine * (MaxValueDigits+SpaceCount));      // +1 - take in spacebar
  StartAscii    := StartOfs + (ValuesPerLine * (MaxValueDigits+SpaceCount)) + 2;  // '  '

  SeparatorChar:='|';
  CountSeperate:=ACountSeparate;

  SeparatorSpace:=' ';

  ChrToValueProc:=AChrToValueProc;           // method for convert char to Value
end;

destructor TCustomCharsPresentation.Destroy;
begin
  inherited;
end;

// ----------------------------------------------------------------------------

constructor TViewerControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Cursor := crIBeam;
  ParentColor := False;
  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csTripleClicks, csOpaque];
  TabStop := True; // so that it can get keyboard focus

  FEncoding   := veAutoDetect;
  FViewerControlMode := vcmText;
  FCustom     := nil;
  FFileName   := '';
  FMappedFile := nil;
  FFileHandle := 0;
  FMappingHandle := 0;
  FPosition  := 0;
  FHPosition := 0;
  FHLowEnd   := 0;
  FLowLimit  := 0;
  FHighLimit := 0;
  FBOMLength := 0;
  FTextHeight:= 14; // dummy value
  FColCount  := 1;

  FLineList := TPtrIntList.Create;

  FScrollBarVert          := TScrollBar.Create(Self);
  FScrollBarVert.Parent   := Self;
  FScrollBarVert.Kind     := sbVertical;
  FScrollBarVert.Align    := alRight;
  FScrollBarVert.OnScroll := @ScrollBarVertScroll;
  FScrollBarVert.TabStop  := False;
  FScrollBarVert.PageSize := 0;

  FScrollBarHorz          := TScrollBar.Create(Self);
  FScrollBarHorz.Parent   := Self;
  FScrollBarHorz.Kind     := sbHorizontal;
  FScrollBarHorz.Align    := alBottom;
  FScrollBarHorz.OnScroll := @ScrollBarHorzScroll;
  FScrollBarHorz.TabStop  := False;
  FScrollBarHorz.PageSize := 0;

  FUpdateScrollBarPos := True;
  FScrollBarPosition  := 0;
  FHScrollBarPosition := 0;

  FOnPositionChanged := nil;
  FOnGuessEncoding   := nil;

  OnResize := @ViewerResize;

  FHex:=TCustomCharsPresentation.Create(16,2,8,8,@HexToValueProc);
  FDec:=TCustomCharsPresentation.Create(15,3,8,5,@DecToValueProc);  // for set bigger ValuePerLine need to improve method GetEndOfLine

end;

destructor TViewerControl.Destroy;
begin
  FHex.Free;
  FDec.Free;

  FHex:=nil;
  FDec:=nil;
  FCustom:=nil;

  UnMapFile;

  if Assigned(FLineList) then
    FreeAndNil(FLineList);

  inherited Destroy;
end;

procedure TViewerControl.Paint;
begin
  if not IsFileOpen then
  begin
    Canvas.Pen.Color := Canvas.Font.Color;
    Canvas.Line(0, 0, ClientWidth - 1, ClientHeight - 1);
    Canvas.Line(0, ClientHeight - 1, ClientWidth - 1, 0);
    Exit;
  end;

  Canvas.Font := Self.Font;
  Canvas.Brush.Color := Self.Color;
  {$IF DEFINED(LCLQT) and (LCL_FULLVERSION < 093100)}
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  {$ENDIF}
  Canvas.Brush.Style := bsClear;
  FTextHeight := Canvas.TextHeight('Wg') + 2;

  if FViewerControlMode = vcmBook then
    FTextWidth := ((ClientWidth - (Canvas.TextWidth('W') * FColCount)) div FColCount)
  else
    FTextWidth := ClientWidth  div Canvas.TextWidth('W') - 2;

  FLineList.Clear;

  case FViewerControlMode of
    vcmBin : WriteBin;
    vcmText: WriteText;
    vcmWrap: WriteText;
    vcmBook: WriteText;
    vcmDec,vcmHex : WriteCustom;

  end;
end;

procedure TViewerControl.SetViewerMode(Value: TViewerControlMode);
begin
  if not (csDesigning in ComponentState) then
  begin
    FLineList.Clear; // do not use cache from previous mode

    FViewerControlMode := Value;
    case FViewerControlMode of
      vcmHex: FCustom := FHex;
      vcmDec: FCustom := FDec;
    else
      FCustom := nil;
    end;

    if not IsFileOpen then
      Exit;

    // Take limits into account for selection.
    FBlockBeg := FBlockBeg + (GetDataAdr - FMappedFile);
    FBlockEnd := FBlockEnd + (GetDataAdr - FMappedFile);

    FHPosition := 0;
    FBOMLength := GetBomLength;
    UpdateLimits;

    // Take limits into account for selection.
    FBlockBeg := FBlockBeg - (GetDataAdr - FMappedFile);
    FBlockEnd := FBlockEnd - (GetDataAdr - FMappedFile);

    UpdateSelection;

    // Force recalculating position.
    SetPosition(FPosition, True);
    SetHPosition(FHPosition, True);

    UpdateScrollbars;
    Invalidate;
  end
  else
    FViewerControlMode := Value;
end;

procedure TViewerControl.SetColCount(const AValue: Integer);
begin
  if AValue > 0 then FColCount := AValue
    else FColCount := 1;
end;

function TViewerControl.ScrollPosition(var aPosition: PtrInt; iLines: Integer): Boolean;
var
  i:      Integer;
  NewPos: PtrInt;
begin
  Result := False;
  NewPos := aPosition;
  if iLines < 0 then
    for i := 1 to -iLines do
      NewPos := GetStartOfPrevLine(NewPos)
  else
    for i := 1 to iLines do
      NewPos := GetStartOfNextLine(NewPos);

  Result    := aPosition <> NewPos;
  aPosition := NewPos;
end;

function TViewerControl.Scroll(iLines: Integer): Boolean;
var
  aPosition: PtrInt;
begin
  if not IsFileOpen then
    Exit;
  aPosition := FPosition;
  Result := ScrollPosition(aPosition, iLines);
  if aPosition <> FPosition then
    SetPosition(aPosition);
end;

function TViewerControl.HScroll(iSymbols: Integer): Boolean;
var
  newPos: Integer;
begin
  if not IsFileOpen then
    Exit;
  newPos := FHPosition + iSymbols;
  if newPos < 0 then
    newPos := 0
  else if (newPos > FHLowEnd - FTextWidth) and (FHLowEnd - FTextWidth > 0) then
    newPos := FHLowEnd - FTextWidth;
  if newPos <> FHPosition then
    SetHPosition(newPos);
end;

function TViewerControl.GetText(const StartPos, Len: PtrInt; const Xoffset: Integer): string;
begin
  SetString(Result, GetDataAdr + StartPos, Len);
  Result := TransformText(ConvertToUTF8(Result), Xoffset);
end;

function TViewerControl.CalcTextLineLength(var iStartPos: PtrInt; const aLimit: Int64; out DataLength: PtrInt): Integer;
var
  MaxLineLength: Boolean;
  CharLenInBytes: Integer;
  OldPos, LastSpacePos: PtrInt;
  LastSpaceResult: Integer;
begin
  Result := 0;
  DataLength := 0;
  LastSpacePos := -1;
  MaxLineLength := True;
  OldPos := iStartPos;

  while MaxLineLength and (iStartPos < aLimit) do
  begin
    case GetNextCharAsAscii(iStartPos, CharLenInBytes) of
      9:             // tab
        Inc(Result, cTabSpaces - Result mod cTabSpaces);
      10:            // stroka
        begin
          DataLength := iStartPos - OldPos;
          iStartPos := iStartPos + CharLenInBytes;
          Exit;
        end;
      13:             // karetka
        begin
          DataLength := iStartPos - OldPos;
          iStartPos := iStartPos + CharLenInBytes;
          // Move after possible #10.
          if (iStartPos < aLimit) and (GetNextCharAsAscii(iStartPos, CharLenInBytes) = 10) then
            Inc(iStartPos, CharLenInBytes);
          Exit;
        end;
      32, 33, 40, 41, 44, 45, 46, 47, 92, 58, 59, 63, 91, 93:              //probel
        begin
          Inc(Result, 1);
          LastSpacePos := iStartPos + CharLenInBytes;
          LastSpaceResult := Result;
        end;
      else
        Inc(Result, 1);
    end;

    if CharLenInBytes = 0 then  // End of data or invalid character.
      break;

    iStartPos := iStartPos + CharLenInBytes;
    DataLength := iStartPos - OldPos;
    case FViewerControlMode of
    vcmText:   MaxLineLength := Result < cMaxTextWidth;
    vcmWrap:   MaxLineLength := Result < FTextWidth;
    vcmBook:   MaxLineLength := Canvas.TextWidth(GetText(OldPos, DataLength, 0)) < FTextWidth;
    else
      Exit;
    end;
  end;

  if (not MaxLineLength) and (LastSpacePos <> -1) then
  begin
    iStartPos := LastSpacePos;
    Result := LastSpaceResult;
    DataLength := iStartPos - OldPos;
  end;
end;

function TViewerControl.TransformText(const sText: String; const Xoffset: Integer): String;
var
  c: AnsiChar;
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(sText) do
  begin
    c := sText[i];
    // Parse only ASCII chars.
    case c of
      #9:
        Result := Result + StringOfChar(' ',
                    cTabSpaces - (UTF8Length(Result) + Xoffset) mod cTabSpaces);
      else
        begin
          if c < ' ' then
            Result := Result + ' '
          else
            Result := Result + c;
        end;
    end;
  end;
end;


function TViewerControl.TransformBin(var aPosition: PtrInt; aLimit: PtrInt): AnsiString;
var
  c: AnsiChar;
  i: Integer;
begin
  Result := '';
  for i := 0 to cBinWidth - 1 do
  begin
    if aPosition >= aLimit then
      Break;
    c := PAnsiChar(GetDataAdr)[aPosition];
    if c < ' ' then
      Result := Result + '.'
    else if c > #127 then
      Result := Result + '.'
    else
      Result := Result + c;

    Inc(aPosition);
  end;
end;

function TViewerControl.TransformHex(var aPosition: PtrInt; aLimit: PtrInt): AnsiString;
begin
  Result:=TransformCustom(aPosition,aLimit);
end;


function TViewerControl.TransformCustom(var APosition: PtrInt; ALimit: PtrInt;
  AWithAdditionalData: boolean): AnsiString;
var
  sAscii: string = '';
  sRez  : string = '';
  tPos  : integer;
begin

  tPos:=APosition;
  sRez:=TransformCustomBlock(APosition,FCustom.ValuesPerLine,True,True,sAscii);

//  Result := LineFormat(sRez, sStr, aStartOffset) else

  if AWithAdditionalData then
  begin
    sRez := Format('%s: %s', [IntToHex(tPos, FCustom.MaxAddrDigits), sRez]);

    if Length(sRez) < FCustom.ValuesPerLine * (FCustom.SpaceCount+FCustom.MaxValueDigits) then
      sRez := sRez + StringOfChar(' ', FCustom.ValuesPerLine * (FCustom.SpaceCount+FCustom.MaxValueDigits) - Length(sRez));
    sRez := sRez + '  ';
    sRez := sRez + sAscii;
  end;

  Result:=sRez;
end;

function TViewerControl.TransformCustomBlock(var APosition: PtrInt; DataLength: integer ; ASeparatorsOn, AAlignData:boolean; out AChars:AnsiString): AnsiString;
var
  c: AnsiChar;
  i: Integer;
  iSep,Len :integer;
  sStr: string = '';
  sRez: string = '';
  sEmpty:string;
  aStartOffset: PtrInt;
begin
  if (APosition+DataLength)>FHighLimit then Len:=FHighLimit-APosition else
     Len:=DataLength;

  iSep:=1;                      // counter for set separator
  aStartOffset := APosition;
  for i := 0 to Len - 1 do
  begin
    c := PAnsiChar(GetDataAdr)[aPosition];
    if c < ' ' then
      AChars := AChars + '.'
    else if c > #127 then
      AChars := AChars + '.'
    else
      AChars := AChars + c;

    sRez := sRez + FCustom.ChrToValueProc(c, FCustom.MaxValueDigits);
    if ( iSep  =  FCustom.CountSeperate) and ASeparatorsOn and
       ( i     < (FCustom.ValuesPerLine - 1))then
    begin
      sRez := sRez + FCustom.SeparatorChar;
      iSep:=0;
    end else
    begin
      sRez := sRez + FCustom.SeparatorSpace;
    end;

    Inc(aPosition);
    inc(iSep);
  end;

  if AAlignData then
  begin
    setlength(sEmpty,FCustom.MaxValueDigits);
    FillChar(sEmpty[1],FCustom.MaxValueDigits,chr(VK_SPACE));

    while (i<FCustom.ValuesPerLine-1) do
    begin
      sRez := sRez + sEmpty + FCustom.SeparatorSpace;
      inc(i);
    end;

    setlength(sEmpty,0);
  end;

  Result:=sRez;
end;


function TViewerControl.DecToValueProc(AChar:AnsiChar;AMaxDigitsCount:integer):AnsiString;
begin
  Result:= IntToStr(Ord(AChar));
  while Length(Result) < AMaxDigitsCount do
    Result:= '0' + Result;
end;

function TViewerControl.HexToValueProc(AChar:AnsiChar;AMaxDigitsCount:integer):AnsiString;
begin
  Result:=IntToHex(Ord(AChar), AMaxDigitsCount);
  while length(Result)<AMaxDigitsCount do
        Result:=' '+Result;
end;


function TViewerControl.GetStartOfLine(aPosition: PtrInt): PtrInt;

  function GetStartOfLineText: PtrInt;
  var
    tmpPos, LineStartPos: PtrInt;
    DataLength: PtrInt;
    prevChar: Cardinal;
    CharLenInBytes: Integer;
  begin
    prevChar := GetPrevCharAsAscii(aPosition, CharLenInBytes);
    if CharLenInBytes = 0 then
      Exit(aPosition);

    // Check if this already is not a start of line (if previous char is #10).
    if prevChar = 10 then
      Exit(aPosition);

    tmpPos := aPosition - CharLenInBytes;

    if tmpPos <= FLowLimit then
      Exit(FLowLimit);

    // Check if we're not in the middle of line ending
    // (previous char is #13, current char is #10).
    if (prevChar = 13) and
       (GetNextCharAsAscii(aPosition, CharLenInBytes) = 10) then
    begin
      prevChar := GetPrevCharAsAscii(tmpPos, CharLenInBytes);
      if CharLenInBytes = 0 then
        Exit(aPosition);
      Dec(tmpPos, CharLenInBytes);
    end;

    if tmpPos <= FLowLimit then
      Exit(FLowLimit);

    // Search for real start of line.
    while (not (prevChar in [10, 13])) and (tmpPos > FLowLimit) do
    begin
      prevChar := GetPrevCharAsAscii(tmpPos, CharLenInBytes);
      if CharLenInBytes = 0 then
        Break;
      Dec(tmpPos, CharLenInBytes);
    end;

    // Previous end of line not found and there are no more data to check.
    if (not (prevChar in [10, 13])) and (tmpPos <= FLowLimit) then
      Exit(FLowLimit);

    // Move forward to first non-line ending character.
    Inc(tmpPos, CharLenInBytes);

    // Search for start of real line or wrapped line.
    while True do
    begin
      LineStartPos := tmpPos;
      CalcTextLineLength(tmpPos, FHighLimit, DataLength);

      if tmpPos = aPosition then
      begin
        if aPosition < FHighLimit then
          Exit(aPosition)       // aPosition is already at start of a line
        else
          Exit(LineStartPos);   // aPosition points to end of file so return start of this line
      end
      else if tmpPos > aPosition then
        Exit(LineStartPos);     // Found start of line
    end;
  end;

  function GetStartOfLineFixed(aFixedWidth: Integer): PtrInt;
  begin
    Result := aPosition - (aPosition mod aFixedWidth);
  end;

var
  i: Integer;
begin
  if aPosition <= FLowLimit then
    Exit(FLowLimit)
  else if aPosition >= FHighLimit then
    aPosition := FHighLimit;   // search from the end of the file

  // Speedup for currently displayed positions.
  if (FLineList.Count > 0) and
     (aPosition >= FLineList.Items[0]) and
     (aPosition <= FLineList.Items[FLineList.Count - 1]) then
  begin
    for i := FLineList.Count - 1 downto 0 do
      if FLineList.Items[i] <= aPosition then
        Exit(FLineList.Items[i]);
  end;

  case FViewerControlMode of
    vcmBin:
      Result := GetStartOfLineFixed(cBinWidth);
    vcmHex, vcmDec:
      Result := GetStartOfLineFixed(FCustom.ValuesPerLine);

    vcmText, vcmWrap, vcmBook:
      Result := GetStartOfLineText;
    else
      Result := aPosition;
  end;
end;

function TViewerControl.GetEndOfLine(aPosition: PtrInt): PtrInt;

  function GetEndOfLineText: PtrInt;
  var
    tmpPos: PtrInt;
    DataLength: PtrInt;
  begin
    Result := GetStartOfLine(aPosition);
    tmpPos := Result;
    CalcTextLineLength(tmpPos, FHighLimit, DataLength);
    Result := Result + DataLength;
    if Result < aPosition then
      Result := aPosition;
  end;

  function GetEndOfLineFixed(aFixedWidth: Integer): PtrInt;
  begin
    Result := aPosition - (aPosition mod aFixedWidth) + aFixedWidth;
  end;

begin
  case FViewerControlMode of
    vcmBin:
      Result := GetEndOfLineFixed(cBinWidth);
    vcmHex,vcmDec:
      Result := GetEndOfLineFixed(FCustom.ValuesPerLine);
    vcmText, vcmWrap, vcmBook:
      Result := GetEndOfLineText;
    else
      Result := aPosition;
  end;
end;

function TViewerControl.GetStartOfPrevLine(aPosition: PtrInt): PtrInt;

  function GetPrevLineText: PtrInt;
  var
    tmpPos, LineStartPos: PtrInt;
    DataLength: PtrInt;
    prevChar: Cardinal;
    CharLenInBytes: Integer;
  begin
    prevChar := GetPrevCharAsAscii(aPosition, CharLenInBytes);
    if CharLenInBytes = 0 then
      Exit(aPosition);

    tmpPos := aPosition - CharLenInBytes; // start search from previous character

    if tmpPos <= FLowLimit then
      Exit(FLowLimit);

    // Check if we're not in the middle of line ending
    // (previous char is #13, current char is #10).
    if (prevChar = 13) and
       (GetNextCharAsAscii(aPosition, CharLenInBytes) = 10) then
    begin
      prevChar := GetPrevCharAsAscii(tmpPos, CharLenInBytes);
      if CharLenInBytes = 0 then
        Exit(aPosition);
      Dec(tmpPos, CharLenInBytes);
    end
    else
    begin
      // Bypass possible end of previous line.
      if prevChar = 10 then
      begin
        prevChar := GetPrevCharAsAscii(tmpPos, CharLenInBytes);
        if CharLenInBytes = 0 then
          Exit(aPosition);
        Dec(tmpPos, CharLenInBytes);
      end;
      if prevChar = 13 then
      begin
        prevChar := GetPrevCharAsAscii(tmpPos, CharLenInBytes);
        if CharLenInBytes = 0 then
          Exit(aPosition);
        Dec(tmpPos, CharLenInBytes);
      end;
    end;

    if tmpPos <= FLowLimit then
      Exit(FLowLimit);

    // Search for real start of line.
    while (not (prevChar in [10, 13])) and (tmpPos > FLowLimit) do
    begin
      prevChar := GetPrevCharAsAscii(tmpPos, CharLenInBytes);
      if CharLenInBytes = 0 then
        Break;
      Dec(tmpPos, CharLenInBytes);
    end;

    // Move forward to first non-line ending character.
    Inc(tmpPos, CharLenInBytes);

    // Search for start of real line or wrapped line.
    while True do
    begin
      LineStartPos := tmpPos;
      CalcTextLineLength(tmpPos, aPosition, DataLength);

      if tmpPos >= aPosition then
        Exit(LineStartPos);  // Found start of line
    end;
  end;

  function GetPrevLineFixed(aFixedWidth: Integer): PtrInt;
  begin
    Result := aPosition - (aPosition mod aFixedWidth);
    if Result >= aFixedWidth then
      Result := Result - aFixedWidth;
  end;

var
  i: Integer;
begin
  if aPosition <= FLowLimit then
    Exit(FLowLimit)
  else if aPosition >= FHighLimit then
    aPosition := FHighLimit;   // search from the end of the file

  // Speedup for currently displayed positions.
  if (FLineList.Count > 0) and
     (aPosition >= FLineList.Items[0]) and
     (aPosition <= FLineList.Items[FLineList.Count - 1]) then
  begin
    for i := FLineList.Count - 1 downto 0 do
      if FLineList.Items[i] < aPosition then
        Exit(FLineList.Items[i]);
  end;

  case FViewerControlMode of
    vcmBin:
      Result := GetPrevLineFixed(cBinWidth);
    vcmHex,vcmDec:
      Result := GetPrevLineFixed(FCustom.ValuesPerLine);
    vcmText, vcmWrap, vcmBook:
      Result := GetPrevLineText;
    else
      Result := aPosition;
  end;
end;

function TViewerControl.GetStartOfNextLine(aPosition: PtrInt): PtrInt;

  function GetNextLineText: PtrInt;
  var
    tmpPos: PtrInt;
    DataLength: PtrInt;
    prevChar: Cardinal;
    CharLenInBytes: Integer;
  begin
    tmpPos := aPosition;

    // This might not be a real start of line (it may be start of wrapped line).
    // Search for start of line.
    while (tmpPos > FLowLimit) do
    begin
      prevChar := GetPrevCharAsAscii(tmpPos, CharLenInBytes);
      if CharLenInBytes = 0 then
        Break;
      if (prevChar in [10, 13]) then
        Break
      else
        Dec(tmpPos, CharLenInBytes);
    end;

    // Now we know we are at the start of a line, search the start of next line.
    while True do
    begin
      CalcTextLineLength(tmpPos, FHighLimit, DataLength);

      if tmpPos >= aPosition then
        Exit(tmpPos);  // Found start of line
    end;
  end;

  function GetNextLineFixed(aFixedWidth: Integer): PtrInt;
  begin
    Result := aPosition - (aPosition mod aFixedWidth);
    if Result + aFixedWidth < FHighLimit then
      Result := Result + aFixedWidth;
  end;

var
  i: Integer;
begin
  if aPosition < FLowLimit then
    aPosition := FLowLimit         // search from the start of the file
  else if aPosition >= FHighLimit then
    aPosition := FHighLimit;   // search from the end of the file

  // Speedup for currently displayed positions.
  if (FLineList.Count > 0) and
     (aPosition >= FLineList.Items[0]) and
     (aPosition <= FLineList.Items[FLineList.Count - 1]) then
  begin
    for i := 0 to FLineList.Count - 1 do
      if FLineList.Items[i] > aPosition then
        Exit(FLineList.Items[i]);
  end;

  case FViewerControlMode of
    vcmBin:
      Result := GetNextLineFixed(cBinWidth);
    vcmHex,vcmDec:
      Result := GetNextLineFixed(FCustom.ValuesPerLine);
    vcmText, vcmWrap, vcmBook:
      Result := GetNextLineText;
    else
      Result := aPosition;
  end;
end;

procedure TViewerControl.PageUp;
var
  H: Integer;
begin
  H := GetClientHeightInLines * FColCount - 1;
  if H <= 0 then
    H := 1;
  Scroll(-H);
end;

procedure TViewerControl.HPageUp;
var
  H: Integer;
begin
  H := FHPosition - FTextWidth;
  if H <= 0 then
    H := FHPosition else H:= FTextWidth;
  HScroll(-H);
end;

procedure TViewerControl.PageDown;
var
  H: Integer;
begin
  H := GetClientHeightInLines * FColCount - 1;
  if H <= 0 then
    H := 1;
  Scroll(H);
end;

procedure TViewerControl.HPageDown;
var
  H: Integer;
begin
  H := FHLowEnd - FHPosition;
  if H > FTextWidth then H := FTextWidth ;
  HScroll(H);
end;

procedure TViewerControl.GoHome;
begin
  Position := FLowLimit;
end;

procedure TViewerControl.GoEnd;
begin
  Position := FHighLimit;
end;

procedure TViewerControl.HGoHome;
begin
  HScroll (-FHPosition);
end;

procedure TViewerControl.HGoEnd;
begin
  HScroll (FHLowEnd-FHPosition);
end;

procedure TViewerControl.SetFileName(const sFileName: String);
begin
  if not (csDesigning in ComponentState) then
  begin
    UnMapFile;

    if sFileName <> '' then
    begin
      if MapFile(sFileName) then
      begin
        FFileName  := sFileName;

        // Detect encoding if needed.
        if FEncoding = veAutoDetect then
          FEncoding := DetectEncoding;

        ReReadFile;
      end;
    end;
  end
  else
    FFileName  := sFileName;
end;

function TViewerControl.MapFile(const sFileName: String): Boolean;

  function ReadFile: Boolean; inline;
  begin
    FMappedFile := GetMem(FFileSize);
    Result := (FileRead(FFileHandle, FMappedFile^, FFileSize) = FFileSize);
    FileClose(FFileHandle);
    FFileHandle := 0;
  end;

{$IFDEF MSWINDOWS}
var
  wFileName: UnicodeString;
begin
  Result := False;
  if Assigned(FMappedFile) then
    UnMapFile; // if needed

  wFileName   := UTF16LongName(sFileName);
  FFileHandle := CreateFileW(PWChar(wFileName), GENERIC_READ,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  if FFileHandle = INVALID_HANDLE_VALUE then
  begin
    FFileHandle := 0;
    Exit;
  end;

  FFileSize := GetFileSize(FFileHandle, nil);

  if (FFileSize < MaxMemSize) then
  begin
    Result := ReadFile;
    Exit;
  end;

  FMappingHandle := CreateFileMapping(FFileHandle, nil, PAGE_READONLY, 0, 0, nil);

  if FMappingHandle <> 0 then
    FMappedFile := MapViewOfFile(FMappingHandle, FILE_MAP_READ, 0, 0, 0)
  else
  begin
    FMappedFile := nil;
    FileClose(FFileHandle);
    FFileHandle := 0;
    Exit;
  end;

  Result := True;
end;
{$ELSE}
var
  StatBuf: Stat;
{$IFDEF LINUX}
  Sbfs: TStatFS;
{$ENDIF}
begin
  Result := False;
  if Assigned(FMappedFile) then
    UnMapFile; // if needed

  FFileHandle := mbFileOpen(sFileName, fmOpenRead);
  if FFileHandle = feInvalidHandle then
  begin
    FFileHandle := 0;
    Exit;
  end;

  if fpFStat(FFileHandle, StatBuf) <> 0 then
  begin
    FileClose(FFileHandle);
    FFileHandle := 0;
    Exit;
  end;

  FFileSize := StatBuf.st_size;

{$IFDEF LINUX}
  if (fpFStatFS(FFileHandle, @Sbfs) = 0) then
  begin
    // Special case for PROC_FS and SYS_FS
    if (sbfs.fstype = PROC_SUPER_MAGIC) or (sbfs.fstype = SYSFS_MAGIC) then
    begin
      FMappedFile := GetMem(MaxMemSize - 1);
      FFileSize := FileRead(FFileHandle, FMappedFile^, MaxMemSize - 1);
      Result := (FFileSize > 0);
      FileClose(FFileHandle);
      FFileHandle := 0;
      Exit;
    end;
  end;
{$ENDIF}

  if (FFileSize < MaxMemSize) then
  begin
    Result := ReadFile;
    Exit;
  end;

  FMappedFile := fpmmap(nil, FFileSize, PROT_READ, MAP_PRIVATE{SHARED}, FFileHandle, 0);
  if FMappedFile = MAP_FAILED then
  begin
    FMappedFile:= nil;
    FileClose(FFileHandle);
    FFileHandle := 0;
    Exit;
  end;

  Result:= True;
end;
{$ENDIF}

procedure TViewerControl.UnMapFile;
begin
  if (FFileSize < MaxMemSize) then
  begin
    if Assigned(FMappedFile) then
    begin
      FreeMem(FMappedFile);
      FMappedFile := nil;
    end;
  end;

{$IFDEF MSWINDOWS}

  if Assigned(FMappedFile) then
  begin
    UnmapViewOfFile(FMappedFile);
    FMappedFile := nil;
  end;

  if FMappingHandle <> 0 then
  begin
    CloseHandle(FMappingHandle);
    FMappingHandle := 0;
  end;

{$ELSE}

  if Assigned(FMappedFile) then
  begin
    if fpmunmap(FMappedFile, FFileSize) = -1 then
      DebugLn('Error unmapping file: ', SysErrorMessage(fpgeterrno));
    FMappedFile := nil;
  end;

{$ENDIF}

  if FFileHandle <> 0 then
  begin
    FileClose(FFileHandle);
    FFileHandle := 0;
  end;

  FFileName  := '';
  FFileSize  := 0;
  Position   := 0;
  FLowLimit  := 0;
  FHighLimit := 0;
  FBOMLength := 0;
  FBlockBeg  := 0;
  FBlockEnd  := 0;
end;

procedure TViewerControl.WriteText;
var
  yIndex, xIndex, w, scrollTab, i: Integer;
  LineStart, iPos: PtrInt;
  DataLength: PtrInt;
begin
  iPos := FPosition;
  if Mode = vcmBook then
     w := Width div FColCount
  else
     w := 0;
  if (Mode = vcmText) and (FHPosition>0) then
    scrollTab := -FHPosition * Canvas.TextWidth('W')
  else
    scrollTab :=0;
  for xIndex := 0 to FColCount-1 do
    begin
      for yIndex := 0 to GetClientHeightInLines - 1 do
      begin
        if iPos >= FHighLimit then
          Break;
        AddLineOffset(iPos);
        LineStart := iPos;
        i := CalcTextLineLength(iPos, FHighLimit, DataLength);
        if i > FHLowEnd then FHLowEnd:=i;
        if DataLength > 0 then
          OutText(scrollTab+xIndex*w, yIndex * FTextHeight, LineStart, DataLength)
      end;
    end;
end;

procedure TViewerControl.WriteCustom;
// this method render visible page of text
var
  yIndex: Integer;
  iPos, LineStart: PtrInt;
  s: string;
begin
  iPos := FPosition;
  for yIndex := 0 to GetClientHeightInLines - 1 do
  begin
    if iPos >= FHighLimit then
      Break;
    LineStart := iPos;
    AddLineOffset(iPos);
    s := TransformCustom(iPos, FHighLimit);  // get line text for render

    if s <> '' then
      OutCustom(0, yIndex * FTextHeight, s, LineStart, iPos - LineStart);  // render line to canvas
  end;
end;

procedure TViewerControl.WriteBin;
var
  yIndex: Integer;
  iPos, LineStart: PtrInt;
  s: string;
begin
  iPos := FPosition;
  for yIndex := 0 to GetClientHeightInLines - 1 do
  begin
    if iPos >= FHighLimit then
      Break;
    LineStart := iPos;
    AddLineOffset(iPos);
    s := TransformBin(iPos, FHighLimit);
    if s <> '' then
      OutBin(0, yIndex * FTextHeight, s, LineStart, iPos - LineStart);
  end;
end;

function TViewerControl.GetDataAdr: Pointer;
begin
  case FViewerControlMode of
    vcmText, vcmWrap, vcmBook:
      Result := FMappedFile + FBOMLength;
    else
      Result := FMappedFile;
  end;
end;

procedure TViewerControl.SetPosition(Value: PtrInt);
begin
  SetPosition(Value, False);
end;

procedure TViewerControl.SetHPosition(Value: Integer);
begin
  SetHPosition(Value, False);
end;

procedure TViewerControl.SetHPosition(Value: Integer; Force: Boolean);
begin
  if not IsFileOpen then
    Exit;

  FHPosition := Value;
  // Set new scroll position.
  if (FHPosition > 0) and (FHLowEnd - FTextWidth > 0) then
    FHScrollBarPosition := FHPosition * 100 div (FHLowEnd - FTextWidth)
  else
    FHScrollBarPosition := 0;
  // Update scrollbar position.
  if FUpdateScrollBarPos then
  begin
    if FScrollBarHorz.Position <> FHScrollBarPosition then
    begin
      // Workaround for bug: http://bugs.freepascal.org/view.php?id=23815
      {$IF DEFINED(LCLQT) and (LCL_FULLVERSION < 1010000)}
      FScrollBarHorz.OnScroll := nil;
      FScrollBarHorz.Position := FHScrollBarPosition;
      Application.ProcessMessages; // Skip message
      FScrollBarHorz.OnScroll := @ScrollBarHorzScroll;
      {$ELSE}
      FScrollBarHorz.Position := FHScrollBarPosition;
      {$ENDIF}
    end;
  end;
  // else the scrollbar position will be updated in ScrollBarVertScroll
  Invalidate;
end;

procedure TViewerControl.SetPosition(Value: PtrInt; Force: Boolean);
var
  LinesTooMany:    Integer;
  LastLineReached: Boolean;
begin
  if not IsFileOpen then
    Exit;

  // Speedup if total nr of lines is less then nr of lines that can be displayed.
  if (FPosition = FLowLimit) and                // only if already at the top
     (FLineList.Count > 0) and (FLineList.Count < GetClientHeightInLines)
  then
    Value := FLowLimit
  else
    // Boundary checks are done in GetStartOfLine.
    Value := GetStartOfLine(Value);

  if (Value <> FPosition) or Force then
  begin
    // Don't allow empty lines at the bottom of the control.
    LinesTooMany := GetClientHeightInLines - GetLinesTillEnd(Value, LastLineReached);
    if LinesTooMany > 0 then
      ScrollPosition(Value, -LinesTooMany); // scroll back upwards

    FPosition := Value;
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self);
    Invalidate;

    // Set new scroll position.
    if LastLineReached and (Value > 0) then
      FScrollBarPosition := 100
    else
      FScrollBarPosition := Percent;
  end;

  // Update scrollbar position.
  if FUpdateScrollBarPos then
  begin
    if FScrollBarVert.Position <> FScrollBarPosition then
    begin
      // Workaround for bug: http://bugs.freepascal.org/view.php?id=23815
      {$IF DEFINED(LCLQT) and (LCL_FULLVERSION < 1010000)}
      FScrollBarVert.OnScroll := nil;
      FScrollBarVert.Position := FScrollBarPosition;
      Application.ProcessMessages; // Skip message
      FScrollBarVert.OnScroll := @ScrollBarVertScroll;
      {$ELSE}
      FScrollBarVert.Position := FScrollBarPosition;
      {$ENDIF}
    end;
  end;
  // else the scrollbar position will be updated in ScrollBarVertScroll
end;

procedure TViewerControl.SetEncoding(AEncoding: TViewerEncoding);
begin
  if not (csDesigning in ComponentState) then
  begin
    if AEncoding = veAutoDetect then
      FEncoding := DetectEncoding
    else
      FEncoding := AEncoding;

    ReReadFile;
  end
  else
    FEncoding := AEncoding;
end;

function TViewerControl.GetEncodingName: string;
begin
  Result := ViewerEncodingsNames[FEncoding];
end;

procedure TViewerControl.SetEncodingName(AEncodingName: string);
var
  i: TViewerEncoding;
begin
  for i := Low(TViewerEncoding) to High(TViewerEncoding) do
    if NormalizeEncoding(ViewerEncodingsNames[i]) = NormalizeEncoding(AEncodingName) then
    begin
      SetEncoding(i);
      break;
    end;
end;

function TViewerControl.GetClientHeightInLines: Integer;
begin
  if FViewerControlMode <> vcmText then
    Result:= 0
  else // Take horizontal scrollbar into account
    Result:= GetSystemMetrics(SM_CYHSCROLL);

  if FTextHeight > 0 then
    Result := (ClientRect.Bottom - ClientRect.Top - Result) div FTextHeight
             // or Self.Height div FTextHeight?
  else
    Result := 0;
end;

function TViewerControl.GetLinesTillEnd(FromPosition: PtrInt;
  out LastLineReached: Boolean): Integer;
var
  yIndex: Integer;
  iPos:   PtrInt;
  DataLength: PtrInt;
begin
  Result := 0;
  iPos   := FromPosition;
  for yIndex := 0 to GetClientHeightInLines - 1 do
  begin
    if iPos >= FHighLimit then
      Break;
    Inc(Result, 1);
    case Mode of
      vcmBin:
        iPos := iPos + cBinWidth;
      vcmHex,vcmDec:
        iPos := iPos + FCustom.ValuesPerLine;
      vcmText, vcmWrap, vcmBook:
        CalcTextLineLength(iPos, FHighLimit, DataLength);
    end;
  end;
  LastLineReached := (iPos >= FHighLimit);
end;

function TViewerControl.GetPercent: Integer;
begin
  if FHighLimit - FLowLimit > 0 then
    Result := (Int64(FPosition - FLowLimit) * 100) div Int64(FHighLimit - FLowLimit)
  else
    Result := 0;
end;

procedure TViewerControl.SetPercent(const AValue: Integer);
begin
  if FHighLimit - FLowLimit > 0 then
    Position := Int64(AValue) * (Int64(FHighLimit  - FLowLimit) div 100) + FLowLimit
  else
    Position := 0;
end;

procedure TViewerControl.SetBlockBegin(const AValue: PtrInt);
begin
  if (AValue >= FLowLimit) and (AValue < FHighLimit) then
  begin
    if FBlockEnd < AValue then
      FBlockEnd := AValue;
    FBlockBeg   := AValue;
    Invalidate;
  end;
end;

procedure TViewerControl.SetBlockEnd(const AValue: PtrInt);
begin
  if (AValue >= FLowLimit) and (AValue < FHighLimit) then
  begin
    if FBlockBeg > AValue then
      FBlockBeg := AValue;
    FBlockEnd   := AValue;
    Invalidate;
  end;
end;

procedure TViewerControl.OutText(x, y: Integer;
  StartPos: PtrInt; DataLength: Integer);
var
  pBegLine, pEndLine: PtrInt;
  iBegDrawIndex, iEndDrawIndex: PtrInt;
  xOffset: Integer;
  sText: string;
begin
  pBegLine := StartPos;
  pEndLine := pBegLine + DataLength;

  if ((FBlockEnd - FBlockBeg) = 0) or ((FBlockBeg < pBegLine) and (FBlockEnd < pBegLine)) or // before
     ((FBlockBeg > pEndLine) and (FBlockEnd > pEndLine)) then //after
  begin
    // out of selection, draw normal
    Canvas.Font.Color := Font.Color;
    Canvas.TextOut(x, y, GetText(StartPos, DataLength, 0));
    Exit;
  end;

  // Get selection start/end.
  if (FBlockBeg <= pBegLine) then
    iBegDrawIndex := pBegLine
  else
    iBegDrawIndex := FBlockBeg;
  if (FBlockEnd < pEndLine) then
    iEndDrawIndex := FBlockEnd
  else
    iEndDrawIndex := pEndLine;

  xOffset := 0;

  // Text before selection.
  if iBegDrawIndex - pBegLine > 0 then
  begin
    sText := GetText(StartPos, iBegDrawIndex - pBegLine, xOffset);
    Canvas.Font.Color := Font.Color;
    Canvas.TextOut(x, y, sText);
    x := x + Canvas.TextWidth(sText);
    xOffset := xOffset + UTF8Length(sText);
  end;

  // Selected text.
  sText := GetText(StartPos + iBegDrawIndex - pBegLine,
                   iEndDrawIndex - iBegDrawIndex, xOffset);

  Canvas.Brush.Color := clHighlight;
  Canvas.Font.Color  := clHighlightText;

  // Cannot simply draw text with brush with TextOut
  // because it differs between widgetsets.
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Bounds(x, y, Canvas.TextWidth(sText), FTextHeight));
  Canvas.Brush.Style := bsClear;

  // Or use TextRect instead of TextOut with Opaque = True.
  //ts := Canvas.TextStyle;
  //ts.Opaque := True;
  //ts.Clipping := True;
  //Canvas.TextRect(Bounds(X, Y, Canvas.TextWidth(sText), FTextHeight), X, Y, sText, ts);

  Canvas.TextOut(x, y, sText);
  x := x + Canvas.TextWidth(sText);
  xOffset := xOffset + UTF8Length(sText);

  // restore previous canvas settings
  Canvas.Brush.Color := Color;
  Canvas.Font.Color  := Font.Color;

  // Text after selection.
  if pEndLine - iEndDrawIndex > 0 then
  begin
    sText := GetText(StartPos + iEndDrawIndex - pBegLine,
                     pEndLine - iEndDrawIndex, xOffset);
    Canvas.TextOut(x, y, sText);
  end;
end;


procedure TViewerControl.OutCustom(x, y: Integer; sText: string;
  StartPos: PtrInt; DataLength: Integer);
var
  pBegLine, pEndLine: PtrInt;
  iBegDrawIndex, iEndDrawIndex: PtrInt;
  sNextText: String = '';
  sTmpText: String;

begin
  pBegLine := StartPos;
  pEndLine := pBegLine + DataLength;

  if ((FBlockEnd - FBlockBeg) = 0) or ((FBlockBeg < pBegLine) and (FBlockEnd <= pBegLine)) or // before
     ((FBlockBeg > pEndLine) and (FBlockEnd > pEndLine)) then //after
  begin
    // out of selection, draw normal
    Canvas.Font.Color := Font.Color;
    Canvas.TextOut(x, y, sText);
    Exit;
  end;

  // Get selection start/end.
  if (FBlockBeg <= pBegLine) then
    iBegDrawIndex := pBegLine
  else
    iBegDrawIndex := FBlockBeg;
  if (FBlockEnd < pEndLine) then
    iEndDrawIndex := FBlockEnd
  else
    iEndDrawIndex := pEndLine;

  // Text before selection (offset and hex part).
  sTmpText := Copy(sText, 1, FCustom.StartOfs + (iBegDrawIndex - pBegLine) * (FCustom.MaxValueDigits+FCustom.SpaceCount));
  Canvas.Font.Color := Font.Color;
  Canvas.TextOut(x, y, sTmpText);
  x := x + Canvas.TextWidth(sTmpText);

  // Selected text (hex part).
  sTmpText := Copy(sText, 1 + FCustom.StartOfs + (iBegDrawIndex - pBegLine) * (FCustom.MaxValueDigits+FCustom.SpaceCount),
                                             (iEndDrawIndex - iBegDrawIndex)* (FCustom.MaxValueDigits+FCustom.SpaceCount));

  // Move last character from selection to not selected text.
  sNextText := Copy(sTmpText, Length(sTmpText), 1);
  Delete(sTmpText, Length(sTmpText), 1);

  Canvas.Brush.Color := clHighlight;
  Canvas.Font.Color  := clHighlightText;

  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Bounds(x, y, Canvas.TextWidth(sTmpText), FTextHeight));
  Canvas.Brush.Style := bsClear;
  Canvas.TextOut(x, y, sTmpText);
  x := x + Canvas.TextWidth(sTmpText);

  // restore previous canvas settings
  Canvas.Brush.Color := Color;
  Canvas.Font.Color  := Font.Color;

  // Text after selection (hex part).
  if pEndLine - iEndDrawIndex > 0 then
  begin
    sTmpText := sNextText +
      Copy(sText, 1 + FCustom.StartOfs + (iEndDrawIndex - pBegLine) * (FCustom.MaxValueDigits+FCustom.SpaceCount),
                                     (pEndLine - iEndDrawIndex) * (FCustom.MaxValueDigits+FCustom.SpaceCount));
    sNextText := '';
    Canvas.TextOut(x, y, sTmpText);
    x := x + Canvas.TextWidth(sTmpText);
  end;

  // Space after hex if data doesn't span full line.
  if DataLength < FHex.ValuesPerLine then
    sNextText := sNextText + Copy(sText, 1 + FCustom.StartOfs + DataLength * (FCustom.MaxValueDigits+FCustom.SpaceCount),
                                              (FCustom.ValuesPerLine - DataLength) * (FCustom.MaxValueDigits+FCustom.SpaceCount));

  // Space between hex and ascii.
  sTmpText := sNextText + '  ';
  Canvas.TextOut(x, y, sTmpText);
  x := x + Canvas.TextWidth(sTmpText);

  // Text before selection (ascii part).
  if iBegDrawIndex - pBegLine > 0 then
  begin
    sTmpText := Copy(sText, 1 + FCustom.StartAscii, iBegDrawIndex - pBegLine);
    Canvas.TextOut(x, y, sTmpText);
    x := x + Canvas.TextWidth(sTmpText);
  end;

  // Selected text (ascii part).
  sTmpText := Copy(sText, 1 + FCustom.StartAscii + iBegDrawIndex - pBegLine,
                                               iEndDrawIndex - iBegDrawIndex);
  Canvas.Brush.Color := clHighlight;
  Canvas.Font.Color  := clHighlightText;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Bounds(x, y, Canvas.TextWidth(sTmpText), FTextHeight));
  Canvas.Brush.Style := bsClear;
  Canvas.TextOut(x, y, sTmpText);
  x := x + Canvas.TextWidth(sTmpText);

  // restore background color
  Canvas.Brush.Color := Color;
  Canvas.Font.Color  := Font.Color;

  // Text after selection.
  if pEndLine - iEndDrawIndex > 0 then
  begin
    sTmpText := Copy(sText, 1 + FCustom.StartAscii + iEndDrawIndex - pBegLine,
                                                 pEndLine - iEndDrawIndex);
    Canvas.TextOut(x, y, sTmpText);
  end;
end;

procedure TViewerControl.OutBin(x, y: Integer; sText: string; StartPos: PtrInt;
  DataLength: Integer);
var
  pBegLine, pEndLine: PtrInt;
  iBegDrawIndex, iEndDrawIndex: PtrInt;
  sTmpText: String;
begin
  pBegLine := StartPos;
  pEndLine := pBegLine + DataLength;

  if ((FBlockEnd - FBlockBeg) = 0) or ((FBlockBeg < pBegLine) and (FBlockEnd < pBegLine)) or // before
     ((FBlockBeg > pEndLine) and (FBlockEnd > pEndLine)) then //after
  begin
    // out of selection, draw normal
    Canvas.Font.Color := Font.Color;
    Canvas.TextOut(x, y, sText);
    Exit;
  end;

  // Get selection start/end.
  if (FBlockBeg <= pBegLine) then
    iBegDrawIndex := pBegLine
  else
    iBegDrawIndex := FBlockBeg;
  if (FBlockEnd < pEndLine) then
    iEndDrawIndex := FBlockEnd
  else
    iEndDrawIndex := pEndLine;

  // Text before selection.
  if iBegDrawIndex - pBegLine > 0 then
  begin
    sTmpText := Copy(sText, 1, iBegDrawIndex - pBegLine);
    Canvas.Font.Color := Font.Color;
    Canvas.TextOut(x, y, sTmpText);
    x := x + Canvas.TextWidth(sTmpText);
  end;

  // Selected text.
  sTmpText := Copy(sText, 1 + iBegDrawIndex - pBegLine, iEndDrawIndex - iBegDrawIndex);

  Canvas.Brush.Color := clHighlight;
  Canvas.Font.Color  := clHighlightText;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Bounds(x, y, Canvas.TextWidth(sTmpText), FTextHeight));
  Canvas.Brush.Style := bsClear;
  Canvas.TextOut(x, y, sTmpText);
  x := x + Canvas.TextWidth(sTmpText);

  // restore previous canvas settings
  Canvas.Brush.Color := Color;
  Canvas.Font.Color  := Font.Color;

  // Text after selection.
  if pEndLine - iEndDrawIndex > 0 then
  begin
    sTmpText := Copy(sText, 1 + iEndDrawIndex - pBegLine, pEndLine - iEndDrawIndex);
    Canvas.TextOut(x, y, sTmpText);
  end;
end;

procedure TViewerControl.AddLineOffset(const iOffset: PtrInt);
begin
  FLineList.Add(iOffset);
end;

procedure TViewerControl.KeyDown(var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_DOWN:
        begin
          Key := 0;
          Scroll(1);
        end;
      VK_UP:
        begin
          Key := 0;
          Scroll(-1);
        end;
      VK_RIGHT:
        begin
          Key := 0;
          HScroll(1);
        end;
      VK_LEFT:
        begin
          Key := 0;
          HScroll(-1);
        end;
      VK_HOME:
        begin
          Key := 0;
          GoHome;
        end;
      VK_END:
        begin
          Key := 0;
          GoEnd;
        end;
      VK_PRIOR:
        begin
          Key := 0;
          PageUp;
        end;
      VK_NEXT:
        begin
          Key := 0;
          PageDown;
        end;
      else
        inherited KeyDown(Key, Shift);
    end;
  end
  else if Shift = [ssCtrl] then
  begin
    case Key of
      VK_HOME:
        begin
          Key := 0;
          GoHome;
        end;
      VK_END:
        begin
          Key := 0;
          GoEnd;
        end;
      else
        inherited KeyDown(Key, Shift);
    end;
  end
  else
    inherited KeyDown(Key, Shift);
end;

function TViewerControl.FindAsciiSetForward(aPosition, aMaxBytes: PtrInt;
                                            const AsciiChars: String;
                                            bFindNotIn: Boolean): PtrInt;
var
  i: Integer;
  found: Boolean;
  u: Cardinal;
  CharLenInBytes: Integer;
begin
  Result := -1;
  while aMaxBytes > 0 do
  begin
    u := GetNextCharAsAscii(aPosition, CharLenInBytes);
    if CharLenInBytes = 0 then
      Exit;

    if not bFindNotIn then
    begin
      for i := 1 to Length(AsciiChars) do
        if u = ord(AsciiChars[i]) then
          Exit(aPosition);
    end
    else
    begin
      found := False;
      for i := 1 to Length(AsciiChars) do
        if u = ord(AsciiChars[i]) then
        begin
          found := True;
          break;
        end;

      if not found then
        Exit(aPosition);
    end;

    Inc(aPosition, CharLenInBytes);
    Dec(aMaxBytes, CharLenInBytes);
  end;
end;

function TViewerControl.FindAsciiSetBackward(aPosition, aMaxBytes: PtrInt;
                                             const AsciiChars: String;
                                             bFindNotIn: Boolean): PtrInt;
var
  i: Integer;
  found: Boolean;
  u: Cardinal;
  CharLenInBytes: Integer;
begin
  Result := -1;
  while aMaxBytes > 0 do
  begin
    u := GetPrevCharAsAscii(aPosition, CharLenInBytes);
    if CharLenInBytes = 0 then
      Exit;

    if not bFindNotIn then
    begin
      for i := 1 to Length(AsciiChars) do
        if u = ord(AsciiChars[i]) then
          Exit(aPosition);
    end
    else
    begin
      found := False;
      for i := 1 to Length(AsciiChars) do
        if u = ord(AsciiChars[i]) then
        begin
          found := True;
          break;
        end;

      if not found then
        Exit(aPosition);
    end;

    Dec(aPosition, CharLenInBytes);
    Dec(aMaxBytes, CharLenInBytes);
  end;
end;

procedure TViewerControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  LineBegin, LineEnd: PtrInt;
  ClickPos: PtrInt;
  CharSide: TCharSide;
begin
  inherited;

  SetFocus;

  if not IsFileOpen then
    Exit;

  case Button of
    mbLeft:
      begin
        if Shift * [ssDouble, ssTriple] = [] then
        begin
          // Single click.
          ClickPos := XYPos2Adr(x, y, CharSide);
          if ClickPos <> -1 then
          begin
            FBlockBeg       := ClickPos;
            FBlockEnd       := ClickPos;
            FMouseBlockBeg  := ClickPos;
            FMouseBlockSide := CharSide;
            FSelecting      := True;
            Invalidate;
          end
          else
            FSelecting := False;
        end
        else // if double click or triple click
        begin
          FSelecting := False;
          LineBegin  := GetStartOfLine(FMouseBlockBeg);
          LineEnd    := GetEndOfLine(FMouseBlockBeg);

          if ssDouble in Shift then
          begin
            // Select word with double-click.
            FBlockBeg := FindAsciiSetBackward(FMouseBlockBeg,
                           FMouseBlockBeg - LineBegin, sNonCharacter, False);

            FBlockEnd := FindAsciiSetForward(FMouseBlockBeg,
                           LineEnd - FMouseBlockBeg, sNonCharacter, False);
          end
          else if ssTriple in Shift then
          begin
            // Select line with triple-click.
            FBlockBeg := FindAsciiSetForward(LineBegin,
                           LineEnd - LineBegin, sWhiteSpace, True);

            FBlockEnd := FindAsciiSetBackward(LineEnd,
                           LineEnd - LineBegin, sWhiteSpace, True);
          end;

          if FBlockBeg = -1 then
            FBlockBeg := LineBegin;
          if FBlockEnd = -1 then
            FBlockEnd := LineEnd;

          if FBlockBeg > FBlockEnd then
            FBlockEnd := FBlockBeg;

          CopyToClipboard;
          Invalidate;
        end;
      end; // mbLeft
  end; // case
end;

procedure TViewerControl.MouseMove(Shift: TShiftState; X, Y: Integer);

  procedure MoveOneChar(var aPosition: PtrInt);
  var
    CharLenInBytes: Integer;
  begin
    GetNextCharAsAscii(aPosition, CharLenInBytes);
    aPosition := aPosition + CharLenInBytes;
  end;

  procedure MoveOneCharByMouseSide(var aPosition: PtrInt);
  begin
    if FMouseBlockSide in [csRight, csAfter] then
      MoveOneChar(aPosition);
  end;

var
  ClickPos: PtrInt;
  CharSide: TCharSide;
begin
  inherited;

  if FSelecting then
  begin
    if y < FTextHeight then
      Scroll(-3)
    else if y > ClientHeight - FTextHeight then
      Scroll(3);

    ClickPos := XYPos2Adr(x, y, CharSide);
    if ClickPos <> -1 then
    begin
      if ClickPos < FMouseBlockBeg then
      begin
        // Got a new beginning.
        FBlockBeg := ClickPos;
        FBlockEnd := FMouseBlockBeg;

        // Move end beyond last character.
        MoveOneCharByMouseSide(FBlockEnd);

        // When selecting from right to left, the current selected side must be
        // either csLeft or csBefore, otherwise current position is not included.
        if not (CharSide in [csLeft, csBefore]) then
        begin
          // Current position should not be included in selection.
          // Move beginning after first character.
          MoveOneChar(FBlockBeg);
        end;
      end
      else if ClickPos > FMouseBlockBeg then
      begin
        // Got a new end.
        FBlockBeg := FMouseBlockBeg;
        FBlockEnd := ClickPos;

        // Move beginning after first character.
        MoveOneCharByMouseSide(FBlockBeg);

        // When selecting from left to right, the current selected side must be
        // either csRight or csAfter, otherwise current position is not included.
        if CharSide in [csRight, csAfter] then
        begin
          // Current position should be included in selection.
          // Move end beyond last character.
          MoveOneChar(FBlockEnd);
        end;
      end
      else if FMouseBlockSide <> CharSide then
      begin
        // Same position but changed side of the character.
        FBlockBeg := FMouseBlockBeg;
        FBlockEnd := FMouseBlockBeg;

        if ((FMouseBlockSide in [csBefore, csLeft]) and
            (CharSide in [csRight, csAfter])) or
           ((FMouseBlockSide in [csRight, csAfter]) and
            (CharSide in [csBefore, csLeft])) then
        begin
          // Move end beyond last character.
          MoveOneChar(FBlockEnd);
        end;
      end
      else
      begin
        FBlockBeg := FMouseBlockBeg;
        FBlockEnd := FMouseBlockBeg;
      end;

      Invalidate;
    end;
  end;
end;

procedure TViewerControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  if FSelecting and (Button = mbLeft) and (Shift * [ssDouble, ssTriple] = []) then
  begin
    CopyToClipboard;
    FSelecting := False;
  end;
end;

function TViewerControl.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited;
  if not Result then
    Result := Scroll(Mouse.WheelScrollLines);
end;

function TViewerControl.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited;
  if not Result then
    Result := Scroll(-Mouse.WheelScrollLines);
end;

{$if lcl_fullversion >= 1070000}
procedure TViewerControl.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double);
begin
  FScrollBarVert.Width  := LCLIntf.GetSystemMetrics(SM_CYVSCROLL);
  FScrollBarHorz.Height := LCLIntf.GetSystemMetrics(SM_CYHSCROLL);
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
end;
{$endif}

function TViewerControl.XYPos2Adr(x, y: Integer; out CharSide: TCharSide): PtrInt;
var
  yIndex:  Integer;
  StartLine, EndLine: PtrInt;

  function XYPos2AdrBin: PtrInt;
  var
    i:  Integer;
    px: Integer = 0;
    charWidth: Integer;
    sText: String;
    tmpPosition: PtrInt;
  begin
    tmpPosition := StartLine;
    sText := TransformBin(tmpPosition, EndLine);
    for i := 1 to Length(sText) do
    begin
      charWidth := Canvas.TextWidth(string(sText[i]));
      if px + charWidth > x then
      begin
        if px + charWidth div 2 > x then
          CharSide := csLeft
        else
          CharSide := csRight;

        Exit(StartLine + i - 1);  // -1 because we count from 1
      end;
      px  := px + charWidth;
    end;
    CharSide := csBefore;
    Result := EndLine;
  end;


  function XYPos2AdrCustom: PtrInt;
  //    | offset part  | custom part  | native part |
  //    |     0000AAAA: | FF AA CC AE | djfjks       |

  var
    i:  Integer;
    px: Integer = 0;
    charWidth: Integer;
    sText, sPartialText: String;
    tmpPosition: PtrInt;
  begin
    tmpPosition  := StartLine;
    sText := TransformCustom(tmpPosition, EndLine);
    if sText='' then exit;

    // Clicked on offset part
    sPartialText := Copy(sText, 1, FCustom.StartOfs);
    charWidth := Canvas.TextWidth(sPartialText);
    px := px + charWidth;
    if px > x then
    begin
      CharSide := csBefore;
      Exit(StartLine);
    end;

    // Clicked on custom part
    for i := 0 to FCustom.ValuesPerLine - 1 do
    begin
      sPartialText := Copy(sText, 1 + FCustom.StartOfs + i * (FCustom.MaxValueDigits+FCustom.SpaceCount), FCustom.MaxValueDigits);
      charWidth := Canvas.TextWidth(sPartialText);
      if px + charWidth > x then
      begin
        // Check if we're not after end of data.
        if StartLine + i >= EndLine then
        begin
          CharSide := csBefore;
          Exit(EndLine);
        end;

        if px + charWidth div 2 > x then
          CharSide := csLeft
        else
          CharSide := csRight;

        Exit(StartLine + i);
      end;

      // Space after hex number.
      charWidth := charWidth +
                   Canvas.TextWidth(string(sText[1 + FCustom.StartOfs + i * (FCustom.MaxValueDigits+1) + FCustom.MaxValueDigits]));

      if px + charWidth > x then
      begin
        CharSide := csAfter;
        Exit(StartLine + i);
      end;

      px := px + charWidth;
    end;

    // Clicked between hex and ascii.
    sPartialText := Copy(sText, 1 + FCustom.StartOfs, FCustom.StartAscii - FCustom.EndOfs);
    charWidth := Canvas.TextWidth(sPartialText);
    if px + charWidth > x then
    begin
      Exit(-1); // No position.
    end;
    px := px + charWidth;

    // Clicked on ascii part.
    for i := 0 to FCustom.ValuesPerLine - 1 do
    begin
      charWidth := Canvas.TextWidth(string(sText[1 + FCustom.StartAscii + i]));
      if px + charWidth > x then
      begin
        // Check if we're not after end of data.
        if StartLine + i >= EndLine then
        begin
          CharSide := csBefore;
          Exit(EndLine);
        end;

        if px + charWidth div 2 > x then
          CharSide := csLeft
        else
          CharSide := csRight;

        Exit(StartLine + i);
      end;
      px := px + charWidth;
    end;

    CharSide := csBefore;
    Result := EndLine;
  end;

  function XYPos2AdrText: PtrInt;
  var
    i: Integer;
    px: Integer = 0;
    charWidth: Integer;
    len: Integer = 0;
    CharLenInBytes: Integer;
    s: String;
  begin
    i := StartLine;

    while i < EndLine do
    begin
      s := GetNextCharAsUtf8(i, CharLenInBytes);
      if CharLenInBytes = 0 then
        Break;

      // Check if the conversion to UTF-8 was successful.
      if Length(s) > 0 then
      begin
        if s = #9 then
        begin
          s := StringOfChar(' ', cTabSpaces - len mod cTabSpaces);
          len := len + (cTabSpaces - len mod cTabSpaces);
        end
        else
          Inc(len); // Assume there is one character after conversion
                    // (otherwise use Inc(len, UTF8Length(s))).

      if len <= FHPosition then
        begin
          i := i + CharLenInBytes;
          Continue;
        end;

        charWidth := Canvas.TextWidth(s);
        if px + charWidth > x then
        begin
          if px + charWidth div 2 > x then
            CharSide := csLeft
          else
            CharSide := csRight;

          Exit(i);
        end;

        px := px + charWidth;
      end;

      i := i + CharLenInBytes;
    end;

    CharSide := csBefore;
    Result := EndLine;
  end;

begin
  if FLineList.Count = 0 then
    Exit(-1);

  yIndex := y div FTextHeight;
  if yIndex >= FLineList.Count then
    yIndex := FLineList.Count - 1;
  if yIndex < 0 then
    yIndex := 0;

  // Get position of first character of the line.
  StartLine := FLineList.Items[yIndex];
  // Get position of last character of the line.
  EndLine   := GetEndOfLine(StartLine);

  if x = 0 then
  begin
    CharSide := csBefore;
    Exit(StartLine);
  end;

  case Mode of
    vcmBin:
      Result := XYPos2AdrBin;
    vcmHex,vcmDec:
      Result := XYPos2AdrCustom;  // XYPos2AdrHex;
    vcmText, vcmWrap, vcmBook:
      Result := XYPos2AdrText;
    else
      raise Exception.Create('Invalid viewer mode');
  end;

end;

procedure TViewerControl.SelectAll;
begin
  SelectText(FLowLimit, FHighLimit);
end;

procedure TViewerControl.SelectText(AStart, AEnd: PtrInt);
begin
  if AStart < FLowLimit then
    AStart := FLowLimit;
  if AEnd > FHighLimit then
    AEnd := FHighLimit;

  if AStart <= AEnd then
  begin
    FBlockBeg := AStart;
    FBlockEnd := AEnd;
    Invalidate;
  end;
end;

procedure TViewerControl.CopyToClipboard;
var
  sText, utf8Text: string;
begin
  if (FBlockEnd - FBlockBeg) <= 0 then
    Exit;
  if (FBlockEnd - FBlockBeg) > 1024 * 1024 then // Max 1 MB to clipboard
    Exit;
  SetString(sText, GetDataAdr + FBlockBeg, FBlockEnd - FBlockBeg);
  utf8Text := ConvertToUTF8(sText);
  {$IFDEF LCLGTK2}
  // Workaround for Lazarus bug #0021453. LCL adds trailing zero to clipboard in Clipboard.AsText.
  Clipboard.Clear;
  Clipboard.AddFormat(PredefinedClipboardFormat(pcfText), utf8Text[1], Length(utf8Text));
  {$ELSE}
  Clipboard.AsText := utf8Text;
  {$ENDIF}
end;

procedure TViewerControl.CopyToClipboardF;
var
  s,sText, utf8Text: string;
  len: Integer;
begin
   len:=FBlockEnd-FBlockBeg;
   if len=0 then exit;
   sText:=TransformCustomBlock(FBlockBeg,len,False,False,s);

   utf8Text := ConvertToUTF8(sText);
   {$IFDEF LCLGTK2}
   // Workaround for Lazarus bug #0021453. LCL adds trailing zero to clipboard in Clipboard.AsText.
   Clipboard.Clear;
   Clipboard.AddFormat(PredefinedClipboardFormat(pcfText), utf8Text[1], Length(utf8Text));
   {$ELSE}
   Clipboard.AsText := utf8Text;
   {$ENDIF}
end;

function TViewerControl.Selection: String;
const
  MAX_LEN = 512;
var
  sText: String;
  AIndex: PtrInt;
  ALength: PtrInt;
  CharLenInBytes: Integer;
begin
  if (FBlockEnd - FBlockBeg) <= 0 then
    Exit(EmptyStr);
  ALength:= FBlockEnd - FBlockBeg;
  if ALength <= MAX_LEN then
  begin
    SetString(sText, GetDataAdr + FBlockBeg, ALength);
    Result := ConvertToUTF8(sText);
  end
  else begin
    Result:= EmptyStr;
    AIndex:= FBlockBeg;
    ALength:= AIndex + MAX_LEN;
    while AIndex < ALength do
    begin
      sText := GetNextCharAsUtf8(AIndex, CharLenInBytes);
      if CharLenInBytes = 0 then
        Break;
      Result:= Result + sText;
      AIndex:= AIndex + CharLenInBytes;
    end;
  end;
end;

function TViewerControl.GetNextCharAsAscii(const iPosition: PtrInt; out CharLenInBytes: Integer): Cardinal;
var
  u1, u2: Word;
  InvalidCharLen: Integer;
begin
  Result := 0;
  case FEncoding of
    veUtf8, veUtf8bom:
      begin
        if iPosition < FHighLimit then
        begin
          CharLenInBytes := SafeUTF8NextCharLen(GetDataAdr + iPosition,
                                                FHighLimit - iPosition,
                                                InvalidCharLen);

          // It's enough to only return Ascii.
          if CharLenInBytes = 1 then
            Result := PByte(GetDataAdr)[iPosition];

          // Full conversion:
          // Result := UTF8CharacterToUnicode(PAnsiChar(GetDataAdr + iPosition), CharLenInBytes);
        end
        else
          CharLenInBytes := 0;
      end;

    veAnsi, veOem,
    veCp1250..veCp950,
    veIso88591,
    veIso88592,
    veKoi8:
      if iPosition < FHighLimit then
      begin
        Result := PByte(GetDataAdr)[iPosition];
        CharLenInBytes := 1;
      end
      else
        CharLenInBytes := 0;

    veUcs2be:
      if iPosition + SizeOf(Word) - 1 < FHighLimit then
      begin
        Result := BEtoN(PWord(GetDataAdr + iPosition)[0]);
        CharLenInBytes := SizeOf(Word);
      end
      else
        CharLenInBytes := 0;

    veUcs2le:
      if iPosition + SizeOf(Word) - 1 < FHighLimit then
      begin
        Result := LEtoN(PWord(GetDataAdr + iPosition)[0]);
        CharLenInBytes := SizeOf(Word);
      end
      else
        CharLenInBytes := 0;

    veUtf16be:
      if iPosition + SizeOf(Word) - 1 < FHighLimit then
      begin
        u1 := BEtoN(PWord(GetDataAdr + iPosition)[0]);
        CharLenInBytes := UTF16CharacterLength(@u1);
        if CharLenInBytes = 1 then
        begin
          Result := u1;
        end
        else if iPosition + SizeOf(Word) * CharLenInBytes - 1 < FHighLimit then
        begin
          u2 := BEtoN(PWord(GetDataAdr + iPosition)[1]);
          Result := utf16PairToUnicode(u1, u2);
        end;

        CharLenInBytes := CharLenInBytes * SizeOf(Word);
      end
      else
        CharLenInBytes := 0;

    veUtf16le:
      if iPosition + SizeOf(Word) - 1 < FHighLimit then
      begin
        u1 := LEtoN(PWord(GetDataAdr + iPosition)[0]);
        CharLenInBytes := UTF16CharacterLength(@u1);
        if CharLenInBytes = 1 then
        begin
          Result := u1;
        end
        else if iPosition + SizeOf(Word) * CharLenInBytes - 1 < FHighLimit then
        begin
          u2 := LEtoN(PWord(GetDataAdr + iPosition)[1]);
          Result := utf16PairToUnicode(u1, u2);
        end
        else
          CharLenInBytes := 0;

        CharLenInBytes := CharLenInBytes * SizeOf(Word);
      end
      else
        CharLenInBytes := 0;

    veUtf32be:
      if iPosition + SizeOf(LongWord) - 1 < FHighLimit then
      begin
        Result := BEtoN(PLongWord(GetDataAdr + iPosition)[0]);
        CharLenInBytes := SizeOf(LongWord);
      end
      else
        CharLenInBytes := 0;

    veUtf32le:
      if iPosition + SizeOf(LongWord) - 1 < FHighLimit then
      begin
        Result := LEtoN(PLongWord(GetDataAdr + iPosition)[0]);
        CharLenInBytes := SizeOf(LongWord);
      end
      else
        CharLenInBytes := 0;

    else
      raise Exception.Create('Unsupported viewer encoding');
  end;
end;

function TViewerControl.GetPrevCharAsAscii(const iPosition: PtrInt; out CharLenInBytes: Integer): Cardinal;
var
  u1, u2: Word;
  InvalidCharLen: Integer;
begin
  Result := 0;
  case FEncoding of
    veUtf8, veUtf8bom:
      begin
        if iPosition > FLowLimit then
        begin
          CharLenInBytes := SafeUTF8PrevCharLen(GetDataAdr + iPosition,
                                                iPosition - FLowLimit,
                                                InvalidCharLen);

          // It's enough to only return Ascii.
          if CharLenInBytes = 1 then
            Result := PByte(GetDataAdr)[iPosition - 1];

          // Full conversion:
          // Result := UTF8CharacterToUnicode(PAnsiChar(GetDataAdr + iPosition - CharLenInBytes), CharLenInBytes);
        end
        else
          CharLenInBytes := 0;
      end;

    veAnsi, veOem,
    veCp1250..veCp950,
    veIso88591,
    veIso88592,
    veKoi8:
      if iPosition > FLowLimit then
      begin
        Result := PByte(GetDataAdr + iPosition)[-1];
        CharLenInBytes := 1;
      end
      else
        CharLenInBytes := 0;

    veUcs2be:
      if iPosition >= FLowLimit + SizeOf(Word) then
      begin
        Result := BEtoN(PWord(GetDataAdr + iPosition)[-1]);
        CharLenInBytes := SizeOf(Word);
      end
      else
        CharLenInBytes := 0;

    veUcs2le:
      if iPosition >= FLowLimit + SizeOf(Word) then
      begin
        Result := LEtoN(PWord(GetDataAdr + iPosition)[-1]);
        CharLenInBytes := SizeOf(Word);
      end
      else
        CharLenInBytes := 0;

    veUtf16be:
      if iPosition >= FLowLimit + SizeOf(Word) then
      begin
        u1 := BEtoN(PWord(GetDataAdr + iPosition)[-1]);
        CharLenInBytes := UTF16CharacterLength(@u1);
        if CharLenInBytes = 1 then
        begin
          Result := u1;
        end
        else if iPosition >= FLowLimit + SizeOf(Word) * CharLenInBytes then
        begin
          u2 := BEtoN(PWord(GetDataAdr + iPosition)[-2]);
          // u2 is the first, u1 is the second value of the pair
          Result := utf16PairToUnicode(u2, u1);
        end;

        CharLenInBytes := CharLenInBytes * SizeOf(Word);
      end
      else
        CharLenInBytes := 0;

    veUtf16le:
      if iPosition >= FLowLimit + SizeOf(Word) then
      begin
        u1 := LEtoN(PWord(GetDataAdr + iPosition)[-1]);
        CharLenInBytes := UTF16CharacterLength(@u1);
        if CharLenInBytes = 1 then
        begin
          Result := u1;
        end
        else if iPosition >= FLowLimit + SizeOf(Word) * CharLenInBytes then
        begin
          u2 := LEtoN(PWord(GetDataAdr + iPosition)[-2]);
          // u2 is the first, u1 is the second value of the pair
          Result := utf16PairToUnicode(u2, u1);
        end;

        CharLenInBytes := CharLenInBytes * SizeOf(Word);
      end
      else
        CharLenInBytes := 0;

    veUtf32be:
      if iPosition >= FLowLimit + SizeOf(LongWord) then
      begin
        Result := BEtoN(PLongWord(GetDataAdr + iPosition)[-1]);
        CharLenInBytes := SizeOf(LongWord);
      end
      else
        CharLenInBytes := 0;

    veUtf32le:
      if iPosition >= FLowLimit + SizeOf(LongWord) then
      begin
        Result := LEtoN(PLongWord(GetDataAdr + iPosition)[-1]);
        CharLenInBytes := SizeOf(LongWord);
      end
      else
        CharLenInBytes := 0;

    else
      raise Exception.Create('Unsupported viewer encoding');
  end;
end;

function TViewerControl.GetNextCharAsUtf8(const iPosition: PtrInt; out CharLenInBytes: Integer): String;
var
  u1: Word;
  s: string;
  InvalidCharLen: Integer;
begin
  Result := '';
  case FEncoding of
    veUtf8, veUtf8bom:
      CharLenInBytes := SafeUTF8NextCharLen(GetDataAdr + iPosition,
                                            FHighLimit - iPosition,
                                            InvalidCharLen);
    veAnsi, veOem,
    veCp1250..veCp950,
    veIso88591,
    veIso88592,
    veKoi8:
      CharLenInBytes := 1;
    veUcs2be, veUcs2le:
      CharLenInBytes := 2;
    veUtf16be:
      if iPosition + SizeOf(Word) - 1 < FHighLimit then
      begin
        u1 := BEtoN(PWord(GetDataAdr + iPosition)[0]);
        CharLenInBytes := UTF16CharacterLength(@u1) * SizeOf(Word);
      end
      else
        CharLenInBytes := 0;
    veUtf16le:
      if iPosition + SizeOf(Word) - 1 < FHighLimit then
      begin
        u1 := LEtoN(PWord(GetDataAdr + iPosition)[0]);
        CharLenInBytes := UTF16CharacterLength(@u1) * SizeOf(Word);
      end
      else
        CharLenInBytes := 0;
    veUtf32be, veUtf32le:
      CharLenInBytes := 4;
    else
      raise Exception.Create('Unsupported viewer encoding');
  end;

  if (CharLenInBytes > 0) and (iPosition + CharLenInBytes - 1 < FHighLimit) then
  begin
    SetString(s, GetDataAdr + iPosition, CharLenInBytes);
    Result := ConvertToUTF8(s);
  end
  else
    Result := '';
end;

function TViewerControl.ConvertToUTF8(const sText: AnsiString): String;
begin
  if FEncoding = veAutoDetect then
    FEncoding := DetectEncoding;  // Force detect encoding.

  case FEncoding of
    veAutoDetect: ;
    veAnsi:
      Result := CeAnsiToUtf8(sText);
    veOem:
      Result := CeOemToUtf8(sText);
    veUtf8, veUtf8bom:
      Result := Utf8ReplaceBroken(sText);
    veUtf16be:
      Result := Utf16BEToUtf8(sText);
    veUtf16le:
      Result := Utf16LEToUtf8(sText);
    veUtf32be:
      Result := Utf32BEToUtf8(sText);
    veUtf32le:
      Result := Utf32LEToUtf8(sText);
    else
      Result := LConvEncoding.ConvertEncoding(sText,
                    ViewerEncodingsNames[FEncoding], EncodingUTF8);
  end;
end;

function TViewerControl.ConvertFromUTF8(const sText: String): AnsiString;
begin
  if FEncoding = veAutoDetect then
    FEncoding := DetectEncoding;  // Force detect encoding.

  case FEncoding of
    veAutoDetect: ;
    veAnsi:
      Result := CeUtf8ToAnsi(sText);
    veOem:
      Result := CeUtf8ToOem(sText);
    veUtf8, veUtf8bom:
      Result := sText;
    veUtf16be:
      Result := Utf8ToUtf16BE(sText);
    veUtf16le:
      Result := Utf8ToUtf16LE(sText);
    veUtf32be:
      Result := '';//Utf8ToUtf32BE(sText);
    veUtf32le:
      Result := '';//Utf8ToUtf32LE(sText);
    else
      Result := LConvEncoding.ConvertEncoding(sText,
                    EncodingUTF8, ViewerEncodingsNames[FEncoding]);
  end;
end;

function TViewerControl.IsVisible(const aPosition: PtrInt): Boolean;
var
  StartPos: PtrInt;
  CharLenInBytes: Integer;
begin
  if IsFileOpen and (FLineList.Count > 0) then
    begin
      FVisibleOffset:= 0;
      StartPos:= GetStartOfLine(aPosition);
      // Calculate horizontal offset in symbols
      while (StartPos < aPosition) do
      begin
        GetNextCharAsAscii(StartPos, CharLenInBytes);
        Inc(StartPos, CharLenInBytes);
        Inc(FVisibleOffset);
      end;

      Result := (aPosition >= FLineList.Items[0]) and
                (aPosition <= FLineList.Items[FLineList.Count - 1]) and
                (FVisibleOffset >= FHPosition) and
                (FVisibleOffset <= FHPosition + FTextWidth);
    end
  else
    Result := False;
end;

procedure TViewerControl.MakeVisible(const aPosition: PtrInt);
var
  Offset: Integer;
  LastLine: Boolean;
begin
  if not IsVisible(aPosition) then
  begin
    SetPosition(aPosition);
    Offset:= GetLinesTillEnd(aPosition, LastLine);
    if (Offset > 4) and (LastLine = False) then Scroll(-4);

    Update;

    if (FVisibleOffset < FHPosition) or
       (FVisibleOffset > FHPosition + FTextWidth) then
    begin
      SetHPosition(FVisibleOffset);
      HScroll(-1);
    end;
  end;
end;

procedure TViewerControl.ScrollBarVertScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  FUpdateScrollBarPos := False;
  case ScrollCode of
    scLineUp:     Scroll(-1);
    scLineDown:   Scroll(1);
    scPageUp:     PageUp;
    scPageDown:   PageDown;
    scTop:        GoHome;
    scBottom:     GoEnd;
    scTrack,
    scPosition:
      begin
        // This check helps avoiding loops if changing ScrollPos below
        // triggers another scPosition message.
        if (ScrollCode = scTrack) or (ScrollPos <> FScrollBarPosition) then
        begin
          if ScrollPos = 0 then
            GoHome
          else if ScrollPos = 100 then
            GoEnd
          else
            Percent := ScrollPos;
        end;
      end;
    scEndScroll:
      begin
      end;
  end;

  ScrollPos := FScrollBarPosition;
  FUpdateScrollBarPos := True;
end;

procedure TViewerControl.ScrollBarHorzScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  FUpdateScrollBarPos := False;
  case ScrollCode of
    scLineUp:     HScroll(-1);
    scLineDown:   HScroll(1);
    scPageUp:     HPageUp;
    scPageDown:   HPageDown;
    scTop:        HGoHome;
    scBottom:     HGoEnd;
    scTrack,
    scPosition:
      begin
        // This check helps avoiding loops if changing ScrollPos below
        // triggers another scPosition message.
        if (ScrollCode = scTrack) or (ScrollPos <> FHScrollBarPosition) then
        begin
          if ScrollPos = 0 then
            HGoHome
          else if ScrollPos = 100 then
            HGoEnd
            else
         HScroll((FHLowEnd - FTextWidth) * ScrollPos div 100 - FHPosition);
        end;
      end;
    scEndScroll:
      begin
      end;
  end;
  ScrollPos := FHScrollBarPosition;
  FUpdateScrollBarPos := True;
end;

procedure TViewerControl.UpdateScrollbars;
begin
  FScrollBarVert.LargeChange := GetClientHeightInLines - 1;
  case Mode of
    vcmBin, vcmHex:
    begin
      //FScrollBarVert.PageSize :=
      //  ((FHighLimit div cHexWidth - GetClientHeightInLines) div 100);
    end
    else
      FScrollBarVert.PageSize := 1;
  end;
  FScrollBarHorz.Visible:= (FViewerControlMode = vcmText);
end;

procedure TViewerControl.ViewerResize(Sender: TObject);
begin
  UpdateScrollbars;
  // Force recalculating position.
  SetPosition(FPosition);
  SetHPosition(FHPosition);
end;

procedure TViewerControl.ReReadFile;
begin
  FBlockBeg := 0;
  FBlockEnd := 0;

  FBOMLength := GetBomLength;
  UpdateLimits;

  UpdateScrollbars;
  Invalidate;
end;

function TViewerControl.IsFileOpen: Boolean;
begin
  Result := Assigned(FMappedFile);
end;

function TViewerControl.DetectEncoding: TViewerEncoding;
var
  DetectStringLength: Integer = 2048; // take first 2kB of the file to detect encoding
  DetectString: String;
  DetectedEncodingName: String;
  Enc: TViewerEncoding;
begin
  if IsFileOpen then
  begin
    // Default to Ansi in case encoding cannot be detected or is unsupported.
    Result := veAnsi;

    if FFileSize < DetectStringLength then
      DetectStringLength := FFileSize;

    SetString(DetectString, PAnsiChar(FMappedFile), DetectStringLength);

    if Assigned(FOnGuessEncoding) then
      DetectedEncodingName := FOnGuessEncoding(DetectString)
    else
      DetectedEncodingName := LConvEncoding.GuessEncoding(DetectString);

    if DetectedEncodingName <> '' then
    begin
      DetectedEncodingName := NormalizeEncoding(DetectedEncodingName);

      // Map UCS-2 to UTF-16.
      if DetectedEncodingName = 'ucs2le' then
        DetectedEncodingName := 'utf16le'
      else if DetectedEncodingName = 'ucs2be' then
        DetectedEncodingName := 'utf16be';

      for Enc := Low(TViewerEncoding) to High(TViewerEncoding) do
      begin
        if NormalizeEncoding(ViewerEncodingsNames[Enc]) = DetectedEncodingName then
        begin
          Result := Enc;
          break;
        end;
      end;
    end;
  end
  else
    Result := veAutoDetect;
end;

procedure TViewerControl.GetSupportedEncodings(List: TStrings);
var
  Enc: TViewerEncoding;
begin
  for Enc := Low(TViewerEncoding) to High(TViewerEncoding) do
    List.Add(ViewerEncodingsNames[Enc]);
end;

function TViewerControl.GetBomLength: Integer;
begin
  Result := 0;

  case FEncoding of
    veUtf8, veUtf8bom:
      if (FFileSize >= 3) and
         (PByte(FMappedFile)[0] = $EF) and
         (PByte(FMappedFile)[1] = $BB) and
         (PByte(FMappedFile)[2] = $BF) then
      begin
        Result := 3;
      end;

    veUcs2be, veUtf16be:
      if (FFileSize >= 2) and
         (PByte(FMappedFile)[0] = $FE) and
         (PByte(FMappedFile)[1] = $FF) then
      begin
        Result := 2;
      end;

    veUcs2le, veUtf16le:
      if (FFileSize >= 2) and
         (PByte(FMappedFile)[0] = $FF) and
         (PByte(FMappedFile)[1] = $FE) then
      begin
        Result := 2;
      end;

    veUtf32be:
      if (FFileSize >= 4) and
         (PByte(FMappedFile)[0] = $00) and
         (PByte(FMappedFile)[1] = $00) and
         (PByte(FMappedFile)[2] = $FE) and
         (PByte(FMappedFile)[3] = $FF) then
      begin
        Result := 4;
      end;

    veUtf32le:
      if (FFileSize >= 4) and
         (PByte(FMappedFile)[0] = $00) and
         (PByte(FMappedFile)[1] = $00) and
         (PByte(FMappedFile)[2] = $FF) and
         (PByte(FMappedFile)[3] = $FE) then
      begin
        Result := 4;
      end;
  end;
end;

procedure TViewerControl.UpdateLimits;
begin
  if FEncoding = veAutoDetect then
    FEncoding := DetectEncoding;

  FBOMLength := GetBomLength;

  case FViewerControlMode of
    vcmText, vcmWrap, vcmBook:
      begin
        FLowLimit  := 0;
        FHighLimit := FFileSize - FBOMLength;
      end;
    else
      begin
        FLowLimit  := 0;
        FHighLimit := FFileSize;
      end;
  end;
end;

procedure TViewerControl.UpdateSelection;

  procedure Check(var aPosition: PtrInt; Backwards: Boolean);
  var
    CharStart: Pointer;
  begin
    case FEncoding of
      veUtf8, veUtf8bom:
        begin
          if not Backwards then
          begin
            CharStart := SafeUTF8NextCharStart(GetDataAdr + aPosition,
                                               FHighLimit - aPosition);
            if Assigned(CharStart) then
              aPosition := CharStart - GetDataAdr
            else
              aPosition := 0;
          end
          else
          begin
            CharStart := SafeUTF8PrevCharEnd(GetDataAdr + aPosition,
                                             aPosition - FLowLimit);
            if Assigned(CharStart) then
              aPosition := CharStart - GetDataAdr
            else
              aPosition := 0;
          end;
        end;

      veAnsi, veOem,
      veCp1250..veCp950,
      veIso88591,
      veIso88592,
      veKoi8:
        ; // any position allowed

      veUcs2be, veUcs2le:
        aPosition := ((aPosition - FLowLimit) and not 1) + FLowLimit;
      veUtf16be, veUtf16le:
        // todo: check if not in the middle of utf-16 character
        aPosition := ((aPosition - FLowLimit) and not 1) + FLowLimit;
      veUtf32be, veUtf32le:
        aPosition := ((aPosition - FLowLimit) and not 3) + FLowLimit;

      else
        raise Exception.Create('Unsupported viewer encoding');
    end;
  end;

begin
  if (FBlockBeg < FLowLimit) or (FBlockBeg >= FHighLimit) or
     (FBlockEnd < FLowLimit) or (FBlockEnd >= FHighLimit) then
  begin
    FBlockBeg := FLowLimit;
    FBlockEnd := FLowLimit;
  end
  else
  begin
    case FViewerControlMode of
      vcmText, vcmWrap, vcmBook:
        begin
          Check(FBlockBeg, False);
          Check(FBlockEnd, True);

          if (FBlockBeg < FLowLimit) or (FBlockBeg >= FHighLimit) or
             (FBlockEnd < FLowLimit) or (FBlockEnd >= FHighLimit) or
             (FBlockEnd < FBlockBeg) then
          begin
            FBlockBeg := FLowLimit;
            FBlockEnd := FLowLimit;
          end;
        end;
      // In non-text modes any selection is valid.
    end;
  end;
end;

function TViewerControl.FindUtf8Text(iStartPos: PtrInt; const sSearchText: String;
                                     bCaseSensitive: Boolean; bSearchBackwards: Boolean): PtrInt;
var
  SearchTextLength: Integer;
  sSearchChars: array of String;
  pCurrentAddr, pEndAddr: PtrInt;
  i, charLen: Integer;

  function sPos2(pAdr: PtrInt):Boolean;
  var
    curChr:String;
    i, charLen: Integer;
  begin
    Result := False;
    for i := 0 to SearchTextLength-1 do
    begin
      curChr:=GetNextCharAsUtf8(pAdr,charLen);
      case bCaseSensitive of
       False: if UTF8UpperCase(curChr) <> UTF8UpperCase(sSearchChars[i]) then Exit;
       True : if curChr <> sSearchChars[i] then Exit;
      end;
      if charLen>0 then
        pAdr:=pAdr+charLen
      else
        Inc(pAdr);
    end;
    Result:=True;
  end;

begin
  Result := PtrInt(-1);
  SearchTextLength := UTF8Length(sSearchText);
  if (SearchTextLength <= 0) then
    Exit;

  setLength(sSearchChars,SearchTextLength);
  for i:=1 to SearchTextLength do
    sSearchChars[i-1]:=UTF8Copy(sSearchText,i,1);


  pCurrentAddr := iStartPos;
  pEndAddr := FHighLimit - Length(ConvertFromUTF8(sSearchText));

  if bSearchBackwards and (pCurrentAddr > pEndAddr) then
    // Move to the first possible position for searching backwards.
    pCurrentAddr := pEndAddr;

  if (pEndAddr < 0) or (pCurrentAddr < 0) or (pCurrentAddr > pEndAddr) then
    Exit;

  while True do
  begin
    if (pCurrentAddr > pEndAddr) or (pCurrentAddr < 0) then
      Exit;

    if sPos2(pCurrentAddr) then
    begin
      Result := pCurrentAddr;
      Exit;
    end;

    case bSearchBackwards of
      False:
      begin
        GetNextCharAsUtf8(pCurrentAddr,charLen);
        if charLen>0 then
          pCurrentAddr:=pCurrentAddr+charLen
        else
          Inc(pCurrentAddr);
       end;
      True : Dec(pCurrentAddr);
    end;
  end;
end;

procedure TViewerControl.ResetEncoding;
begin
  FEncoding:= veAutoDetect;
end;

procedure Register;
begin
  RegisterComponents('SeksiCmd', [TViewerControl]);
end;

end.

