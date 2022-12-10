unit Img32.Text;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.3                                                             *
* Date      :  6 October 2022                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2022                                         *
*                                                                              *
* Purpose   :  TrueType fonts for TImage32 (without Windows dependencies)      *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  {$IFDEF MSWINDOWS} Windows, ShlObj, ActiveX, {$ENDIF}
  Types, SysUtils, Classes, Math,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Img32, Img32.Draw;

type
  TFixed = type single;
  Int16 = type SmallInt;
  TFontFormat = (ffInvalid, ffTrueType, ffCompact);
  TTtfFontFamily = (ttfUnknown, ttfSerif, ttfSansSerif, ttfMonospace);

  {$IFNDEF Unicode}
  UnicodeString = WideString;
  {$ENDIF}

  TMacStyle = (msBold, msItalic, msUnderline, msOutline,
    msShadow, msCondensed, msExtended);
  TMacStyles = set of TMacStyle;

  TTextAlign = (taLeft, taRight, taCenter, taJustify);
  TTextVAlign = (tvaTop, tvaMiddle, tvaBottom);

  //nb: Avoid "packed" records as these cause problems with Android

  TFontHeaderTable = record
    sfntVersion   : Cardinal;  // $10000 or 'OTTO'
    numTables     : WORD;
    searchRange   : WORD;
    entrySelector : WORD;
    rangeShift    : WORD;
  end;

  TFontTable = record
    tag           : Cardinal;
    checkSum      : Cardinal;
    offset        : Cardinal;
    length        : Cardinal;
  end;

  TFontTable_Cmap = record
    version       : WORD;
    numTables     : WORD;
  end;

  TCmapTblRec = record
    platformID    : WORD; //Unicode = 0; Windows = 3 (obsolete);
    encodingID    : WORD;
    offset        : Cardinal;
  end;

  TCmapFormat0 = record
    format        : WORD; //0
    length        : WORD;
    language      : WORD;
  end;

  TCmapFormat4 = record
    format        : WORD; //4
    length        : WORD;
    language      : WORD;
    segCountX2    : WORD;
    searchRange   : WORD;
    entrySelector : WORD;
    rangeShift    : WORD;
    //endCodes    : array of WORD; //last = $FFFF
    //reserved    : WORD; //0
    //startCodes  : array of WORD;
  end;

  TCmapFormat6 = record
    format        : WORD; //6
    length        : WORD;
    language      : WORD;
    firstCode     : WORD;
    entryCount    : WORD;
  end;

  TFontTable_Kern = record
    version       : WORD;
    numTables     : WORD;
  end;

  TKernSubTbl = record
    version       : WORD;
    length        : WORD;
    coverage      : WORD;
  end;

  TFormat0KernHdr = record
    nPairs        : WORD;
    searchRange   : WORD;
    entrySelector : WORD;
    rangeShift    : WORD;
  end;

  TFormat0KernRec = record
    left          : WORD;
    right         : WORD;
    value         : int16;
  end;
  TArrayOfKernRecs = array of TFormat0KernRec;

  TFontTable_Name = record
    format        : WORD;
    count         : WORD;
    stringOffset  : WORD;
    //nameRecords[]
  end;

  TNameRec = record
    platformID        : WORD;
    encodingID        : WORD;
    languageID        : WORD;
    nameID            : WORD;
    length            : WORD;
    offset            : WORD;
  end;

  TFontTable_Head = record
    majorVersion   : Word;
    minorVersion   : Word;
    fontRevision   : TFixed;
    checkSumAdjust : Cardinal;
    magicNumber    : Cardinal;  // $5F0F3CF5
    flags          : Word;
    unitsPerEm     : Word;
    dateCreated    : UInt64;
    dateModified   : UInt64;
    xMin           : Int16;
    yMin           : Int16;
    xMax           : Int16;
    yMax           : Int16;
    macStyle       : Word;      //see TMacStyles
    lowestRecPPEM  : Word;
    fontDirHint    : Int16;     //ltr, rtl
    indexToLocFmt  : Int16;
    glyphDataFmt   : Int16;
  end;

  TFontTable_Maxp = record
    version        : TFixed;
    numGlyphs      : WORD;
    maxPoints      : WORD;
    maxContours    : WORD;
  end;

  TFontTable_Glyf = record
    numContours    : Int16;
    xMin           : Int16;
    yMin           : Int16;
    xMax           : Int16;
    yMax           : Int16;
  end;

  TFontTable_Hhea = record
    version        : TFixed;
    ascent         : Int16;
    descent        : Int16;
    lineGap        : Int16;
    advWidthMax    : WORD;
    minLSB         : Int16;
    minRSB         : Int16;
    xMaxExtent     : Int16;
    caretSlopeRise : Int16;
    caretSlopeRun  : Int16;
    caretOffset    : Int16;
    reserved       : UInt64;
    metricDataFmt  : Int16;
    numLongHorMets : WORD;
  end;

  TFontTable_Hmtx = record
    advanceWidth    : WORD;
    leftSideBearing : Int16;
  end;

  TFontTable_Post = record
    majorVersion   : Word;
    minorVersion   : Word;
    italicAngle    : TFixed;
    underlinePos   : Int16;
    underlineWidth : Int16;
    isFixedPitch   : Cardinal;
    //minMemType42   : Cardinal;
    //maxMemType42   : Cardinal;
    //minMemType1   : Cardinal;
    //maxMemType1   : Cardinal;
  end;

  TFontInfo = record                  //a custom summary record
    fontFormat     : TFontFormat;
    fontFamily     : TTtfFontFamily;
    faceName       : UnicodeString;
    style          : UnicodeString;
    copyright      : UnicodeString;
    manufacturer   : UnicodeString;
    dateCreated    : TDatetime;
    dateModified   : TDatetime;
    macStyles      : TMacStyles;
    glyphCount     : integer;
    unitsPerEm     : integer;
    xMin           : integer;
    yMin           : integer;
    xMax           : integer;
    yMax           : integer;
    ascent         : integer;
    descent        : integer;
    lineGap        : integer;
    advWidthMax    : integer;
    minLSB         : integer;
    minRSB         : integer;
    xMaxExtent     : integer;
  end;

  TKern = record
    rightGlyphIdx  : integer;
    kernValue      : integer;
  end;
  TArrayOfTKern = array of TKern;

  TGlyphMetrics = record              //a custom metrics record
    glyphIdx   : integer;
    unitsPerEm : integer;
    glyf       : TFontTable_Glyf;
    hmtx       : TFontTable_Hmtx;
    kernList   : TArrayOfTKern;
  end;

  TFontTableArray = array of TFontTable;
  TArrayOfWord = array of WORD;
  TArrayOfCardinal = array of Cardinal;
  TArrayOfCmapTblRec = array of TCmapTblRec;

  TPointEx = record
    pt: TPointD;
    flag: byte;
  end;
  TPathEx = array of TPointEx;
  TPathsEx = array of TPathEx;

  TTableName = (tblName, tblHead, tblHhea,
    tblCmap, tblMaxp, tblLoca, tblGlyf, tblHmtx, tblKern, tblPost);

{$IFDEF ZEROBASEDSTR}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

  TFontReader = class;

  TFontManager = class
  private
    fMaxFonts: integer;
{$IFDEF XPLAT_GENERICS}
    fFontList: TList<TFontReader>;
{$ELSE}
    fFontList: TList;
{$ENDIF}
    procedure SetMaxFonts(value: integer);
    function ValidateAdd(fr: TFontReader): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetFont(const fontName: string): TFontReader;
{$IFDEF MSWINDOWS}
    function Load(const fontName: string): TFontReader;
{$ENDIF}
    function LoadFromStream(stream: TStream): TFontReader;
    function LoadFromResource(const resName: string; resType: PChar): TFontReader;
    function LoadFromFile(const filename: string): TFontReader;
    function GetBestMatchFont(const fontInfo: TFontInfo): TFontReader;
    function Delete(fontReader: TFontReader): Boolean;
    property MaxFonts: integer read fMaxFonts write SetMaxFonts;
  end;

  TFontReader = class(TInterfacedObj, INotifySender)
  private
    fFontManager       : TFontManager;
    fDestroying        : Boolean;
    fUpdateCount       : integer;
    fRecipientList     : TRecipients;
    fStream            : TMemoryStream;
    fFontWeight        : integer;
    fFontInfo          : TFontInfo;
    fTables            : TFontTableArray;
    fTblIdxes          : array[TTableName] of integer;
    fTbl_name          : TFontTable_Name;
    fTbl_head          : TFontTable_Head;
    fTbl_hhea          : TFontTable_Hhea;
    fTbl_cmap          : TFontTable_Cmap;
    fTbl_maxp          : TFontTable_Maxp;
    fTbl_glyf          : TFontTable_Glyf;
    fTbl_hmtx          : TFontTable_Hmtx;
    fTbl_post          : TFontTable_Post;
    fTbl_loca2         : TArrayOfWord;
    fTbl_loca4         : TArrayOfCardinal;
    fCmapTblRecs       : TArrayOfCmapTblRec;
    fFormat0CodeMap    : array[0..255] of byte;
    fFormat4EndCodes   : TArrayOfWord;
    fFormat4StartCodes : TArrayOfWord;
    fFormat4IdDelta    : TArrayOfWord;
    fFormat4RangeOff   : TArrayOfWord;
    fFormat4Offset     : integer;
    fKernTable         : TArrayOfKernRecs;

    function GetTables: Boolean;
    function GetTable_name: Boolean;
    function GetTable_cmap: Boolean;
    function GetTable_maxp: Boolean;
    function GetTable_head: Boolean;
    function GetTable_loca: Boolean;
    function GetTable_hhea: Boolean;
    procedure GetTable_kern;
    procedure GetTable_post;

    function GetGlyphPaths(glyphIdx: integer): TPathsEx;
    function GetGlyphIdxFromCmapIdx(idx: Word): integer;
    function GetSimpleGlyph: TPathsEx;
    function GetCompositeGlyph: TPathsEx;
    function ConvertSplinesToBeziers(const pathsEx: TPathsEx): TPathsEx;
    procedure GetPathCoords(var paths: TPathsEx);
    function GetGlyphHorzMetrics(glyphIdx: integer): Boolean;
    function GetFontInfo: TFontInfo;
    function GetGlyphKernList(glyphIdx: integer): TArrayOfTKern;
    function GetGlyphMetricsInternal(glyphIdx: integer): TGlyphMetrics;
    function GetWeight: integer;
    function GetFontFamily: TTtfFontFamily;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NotifyRecipients(notifyFlag: TImg32Notification);
  protected
    property PostTable: TFontTable_Post read fTbl_post;
  public
    constructor Create; overload;
    constructor CreateFromResource(const resName: string; resType: PChar);
{$IFDEF MSWINDOWS}
    constructor Create(const fontname: string); overload;
{$ENDIF}
    destructor Destroy; override;
    procedure Clear;
    procedure AddRecipient(recipient: INotifyRecipient);
    procedure DeleteRecipient(recipient: INotifyRecipient);
    function IsValidFontFormat: Boolean;
    function LoadFromStream(stream: TStream): Boolean;
    function LoadFromResource(const resName: string; resType: PChar): Boolean;
    function LoadFromFile(const filename: string): Boolean;
{$IFDEF MSWINDOWS}
    function Load(const fontname: string): Boolean;
    function LoadUsingFontHdl(hdl: HFont): Boolean;
{$ENDIF}
    function GetGlyphInfo(unicode: Word; out paths: TPathsD;
      out nextX: integer; out glyphMetrics: TGlyphMetrics): Boolean;
    property FontFamily: TTtfFontFamily read GetFontFamily;
    property FontInfo: TFontInfo read GetFontInfo;
    property Weight: integer read GetWeight; //range 100-900
  end;

  PGlyphInfo = ^TGlyphInfo;
  TGlyphInfo = record
    unicode  : Word;
    contours : TPathsD;
    metrics  : TGlyphMetrics;
  end;

  TTextPageMetrics = record
    lineCount       : integer;
    maxLineWidth    : double;
    wordListOffsets : TArrayOfInteger;
    justifyDeltas   : TArrayOfDouble;
    lineWidths      : TArrayOfDouble;
  end;

  TWordInfoList = class;

  TWordInfo = class
    index         : integer;
    aWord         : UnicodeString;
    width         : double;
    length        : integer;
    paths         : TArrayOfPathsD;
    constructor Create(owner: TWordInfoList; idx: integer);
  end;

  TFontCache = class;

  //TWordInfoList: A font formatted word list where text is broken into
  //individual words and stored with their glyph info. This class is very
  //useful with custom text editors.
  TWordInfoList = class
  private
{$IFDEF XPLAT_GENERICS}
    fList         : TList<TWordInfo>;
{$ELSE}
    fList         : TList;
{$ENDIF}
    fSingleLine   : Boolean;
    //fListUpdates: accommodates many calls to UpdateWordList
    //by occasionally refreshing glyph outlines.
    //fListUpdates: integer;
    fUpdateCount: integer;
    fOnChanged  : TNotifyEvent;
    function  GetWord(index: integer): TWordInfo;
    function GetText: UnicodeString;
  protected
    procedure Changed; Virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ApplyNewFont(font: TFontCache);
    procedure Clear;
    function  Count: integer;
    procedure Edit(font: TFontCache; index: Integer; const newWord: string);
    procedure Delete(Index: Integer);
    procedure DeleteRange(startIdx, endIdx: Integer);
    procedure AddNewline;
    procedure AddSpace(font: TFontCache); overload;
    procedure AddSpace(spaceWidth: double); overload;
    procedure AddWord(font: TFontCache;
      const word: UnicodeString; underlineIdx: integer = 0);
    procedure InsertNewline(index: integer);
    procedure InsertSpace(font: TFontCache; index: integer); overload;
    procedure InsertSpace(spaceWidth: double; index: integer); overload;
    procedure InsertWord(font: TFontCache; index: integer;
      const word: UnicodeString; underlineIdx: integer = 0);
    procedure SetText(const text: UnicodeString;
      font: TFontCache; underlineIdx: integer = 0);
    property ForceSingleLine: Boolean read fSingleLine write fSingleLine;
    property WordInfo[index: integer]: TWordInfo read GetWord; default;
    property Text: UnicodeString read GetText;
    property OnChanged: TNotifyEvent read fOnChanged write fOnChanged;
  end;

  //TFontCache: speeds up text rendering by parsing font files only once
  //for each accessed character. It can also scale glyphs to a specified
  //font height and invert them too (which is necessary on Windows PCs).
  TFontCache = class(TInterfacedObj, INotifySender, INotifyRecipient)
  private
{$IFDEF XPLAT_GENERICS}
    fGlyphInfoList     : TList<PGlyphInfo>;
{$ELSE}
    fGlyphInfoList     : TList;
{$ENDIF}
    fFontReader        : TFontReader;
    fRecipientList     : TRecipients;
    fSorted            : Boolean;
    fScale             : double;
    fUseKerning        : Boolean;
    fFontHeight        : double;
    fFlipVert          : Boolean;
    fUnderlined        : Boolean;
    fStrikeOut         : Boolean;
    procedure NotifyRecipients(notifyFlag: TImg32Notification);
    function FoundInList(charOrdinal: WORD): Boolean;
    function AddGlyph(unicode: Word): PGlyphInfo;
    procedure VerticalFlip(var paths: TPathsD);
    procedure SetFlipVert(value: Boolean);
    procedure SetFontHeight(newHeight: double);
    procedure SetFontReader(newFontReader: TFontReader);
    procedure UpdateScale;
    procedure Sort;
    procedure GetMissingGlyphs(const ordinals: TArrayOfWord);
    function IsValidFont: Boolean;
    function GetAscent: double;
    function GetDescent: double;
    function GetLineHeight: double;
    function GetYyHeight: double;

    function GetTextOutlineInternal(x, y: double;
      const text: UnicodeString; out glyphs: TArrayOfPathsD;
      out nextX: double; underlineIdx: integer = 0): Boolean;
  public
    constructor Create(fontReader: TFontReader = nil; fontHeight: double = 10); overload;
    destructor Destroy; override;
    procedure Clear;
    //TFontCache is both an INotifySender and an INotifyRecipient.
    //It receives notifications from a TFontReader object and it sends
    //notificiations to any number of TFontCache object users
    procedure ReceiveNotification(Sender: TObject; notify: TImg32Notification);
    procedure AddRecipient(recipient: INotifyRecipient);
    procedure DeleteRecipient(recipient: INotifyRecipient);
    function GetCharInfo(charOrdinal: WORD): PGlyphInfo;

    function GetTextOutline(x, y: double;
      const text: UnicodeString): TPathsD; overload;
    function GetTextOutline(const rec: TRect; const text: UnicodeString;
      textAlign: TTextAlign; textAlignV: TTextVAlign;
      underlineIdx: integer = 0): TPathsD; overload;
    function GetTextOutline(const rec: TRect; wordList: TWordInfoList;
      tpm: TTextPageMetrics; textAlign: TTextAlign;
      startLine, endLine: integer): TPathsD; overload;
    function GetTextOutline(x, y: double; const text: UnicodeString;
      out nextX: double; underlineIdx: integer = 0): TPathsD; overload;
    function GetVerticalTextOutline(x, y: double;
      const text: UnicodeString; interCharSpace: double =0): TPathsD;


    function GetAngledTextGlyphs(x, y: double; const text: UnicodeString;
      angleRadians: double; const rotatePt: TPointD;
      out nextPt: TPointD): TPathsD;
    function GetCharOffsets(const text: UnicodeString;
      interCharSpace: double = 0): TArrayOfDouble;
    function GetTextWidth(const text: UnicodeString): double;
    function GetSpaceWidth: double;

    property Ascent: double read GetAscent;
    property Descent: double read GetDescent;

    property FontHeight: double read fFontHeight write SetFontHeight;
    property FontReader: TFontReader read
      fFontReader write SetFontReader;
    property InvertY: boolean read fFlipVert write SetFlipVert;
    property Kerning: boolean read fUseKerning write fUseKerning;
    property LineHeight: double read GetLineHeight;
    property YyHeight: double read GetYyHeight;
    property Scale : double read fScale;
    property Underlined: Boolean read fUnderlined write fUnderlined;
    property StrikeOut: Boolean read fStrikeOut write fStrikeOut;
  end;

  //Given a wordList and a specified maximum line width (in pixels),
  //get both the line count and the offsets to the words in wordlist
  //that will start each line.
  function GetPageMetrics(lineWidth: double;
    wordList: TWordInfoList): TTextPageMetrics;

  function DrawText(image: TImage32; x, y: double;
    const text: UnicodeString; font: TFontCache;
    textColor: TColor32 = clBlack32;
    useClearType: Boolean = false;
    clearTypeBgColor: TColor32 = clWhite32): double; overload;

  function DrawText(image: TImage32; x, y: double;
    const text: UnicodeString; font: TFontCache;
    renderer: TCustomRenderer): double; overload;

  procedure DrawText(image: TImage32; const rec: TRect;
    const text: UnicodeString;
    textAlign: TTextAlign; textAlignV: TTextVAlign;
    font: TFontCache; textColor: TColor32 = clBlack32;
    useClearType: Boolean = false;
    clearTypeBgColor: TColor32 = clWhite32); overload;

  function DrawAngledText(image: TImage32;
  x, y: double; angleRadians: double;
  const text: UnicodeString; font: TFontCache;
  textColor: TColor32 = clBlack32): TPointD;

  function DrawVerticalText(image: TImage32;
    x, y, interCharSpace: double;
    const text: UnicodeString; font: TFontCache;
    textColor: TColor32 = clBlack32): double;

  function GetTextOutlineOnPath(const text: UnicodeString;
    const path: TPathD; font: TFontCache; textAlign: TTextAlign;
    perpendicOffset: integer = 0; charSpacing: double = 0): TPathsD; overload;

  function GetTextOutlineOnPath(const text: UnicodeString;
    const path: TPathD; font: TFontCache; textAlign: TTextAlign;
    perpendicOffset: integer; charSpacing: double;
    out charsThatFit: integer): TPathsD; overload;

  {$IFDEF MSWINDOWS}
  procedure CheckFontHeight(var logFont: TLogFont);
  function PointHeightToPixelHeight(pt: double): double;
  function GetFontFolder: string;
  function GetInstalledTtfFilenames: TArrayOfString;
  {$ENDIF}

  function FontManager: TFontManager;

implementation

uses
  Img32.Vector;

resourcestring
  rsTooManyFonts        = 'TFontManager error: Too many fonts are open.';
  rsWordListRangeError  = 'TFFWordList: range error.';
  rsFontCacheError      = 'TFontCache error: message from incorrect TFontReader';
  rsWordListFontError  = 'TFFWordList: invalid font error.';

var
  aFontManager: TFontManager;

const
  lineFrac = 0.05;

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

//GetMeaningfulDateTime: returns UTC date & time
procedure GetMeaningfulDateTime(const secsSince1904: Uint64;
  out yy,mo,dd, hh,mi,ss: cardinal);
const
  dim: array[boolean, 0..12] of cardinal =
    ((0,31,59,90,120,151,181,212,243,273,304,334,365), //non-leap year
    (0,31,60,91,121,152,182,213,244,274,305,335,366)); //leap year
var
  isLeapYr: Boolean;
const
  maxValidYear  = 2100;
  secsPerHour   = 3600;
  secsPerDay    = 86400;
  secsPerNormYr = 31536000;
  secsPerLeapYr = secsPerNormYr + secsPerDay;
  secsPer4Years = secsPerNormYr * 3 + secsPerLeapYr; //126230400;
begin
  //nb: Centuries are not leap years unless they are multiples of 400.
  //    2000 WAS a leap year, but 2100 won't be.
  //    Validate function at http://www.mathcats.com/explore/elapsedtime.html

  ss := (secsSince1904 div secsPer4Years);       //count '4years' since 1904

  //manage invalid dates
  if (secsSince1904 = 0) or
    (ss > (maxValidYear-1904) div 4) then
  begin
    yy := 1904; mo := 1; dd := 1;
    hh := 0; mi := 0; ss := 0;
    Exit;
  end;
  yy := 1904 + (ss * 4);

  ss := secsSince1904 mod secsPer4Years;         //secs since START last leap yr
  isLeapYr := ss < secsPerLeapYr;
  if not isLeapYr then
  begin
    dec(ss, secsPerLeapYr);
    yy := yy + (ss div secsPerNormYr) + 1;
    ss := ss mod secsPerNormYr;                  //remaining secs in final year
  end;
  dd := 1 + ss div secsPerDay;                   //day number in final year
  mo := 1;
  while dim[isLeapYr, mo] < dd do inc(mo);
  ss := ss - (dim[isLeapYr, mo-1] * secsPerDay); //remaining secs in month
  dd := 1 + (ss div secsPerDay);
  ss := ss mod secsPerDay;
  hh := ss div secsPerHour;
  ss := ss mod secsPerHour;
  mi := ss div 60;
  ss := ss mod 60;
end;
//------------------------------------------------------------------------------

function MergePathsArray(const pa: TArrayOfPathsD): TPathsD;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to High(pa) do
    AppendPath(Result, pa[i]);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function WordSwap(val: WORD): WORD;
{$IFDEF ASM_X86}
asm
  rol ax,8;
end;
{$ELSE}
var
  v: array[0..1] of byte absolute val;
  r: array[0..1] of byte absolute result;
begin
  r[0] := v[1];
  r[1] := v[0];
end;
{$ENDIF}
//------------------------------------------------------------------------------

function Int16Swap(val: Int16): Int16;
{$IFDEF ASM_X86}
asm
  rol ax,8;
end;
{$ELSE}
var
  v: array[0..1] of byte absolute val;
  r: array[0..1] of byte absolute result;
begin
  r[0] := v[1];
  r[1] := v[0];
end;
{$ENDIF}
//------------------------------------------------------------------------------

function Int32Swap(val: integer): integer;
{$IFDEF ASM_X86}
asm
  bswap eax
end;
{$ELSE}
var
  i: integer;
  v: array[0..3] of byte absolute val;
  r: array[0..3] of byte absolute Result; //warning: do not inline
begin
  for i := 0 to 3 do r[3-i] := v[i];
end;
{$ENDIF}
//------------------------------------------------------------------------------

function UInt64Swap(val: UInt64): UInt64;
{$IFDEF ASM_X86}
asm
  MOV     EDX, val.Int64Rec.Lo
  BSWAP   EDX
  MOV     EAX, val.Int64Rec.Hi
  BSWAP   EAX
end;
{$ELSE}
var
  i: integer;
  v: array[0..7] of byte absolute val;
  r: array[0..7] of byte absolute Result;
begin
  for i := 0 to 7 do r[7-i] := v[i];
end;
{$ENDIF}
//------------------------------------------------------------------------------

procedure GetByte(stream: TStream; out value: byte);
begin
  stream.Read(value, 1);
end;
//------------------------------------------------------------------------------

procedure GetShortInt(stream: TStream; out value: ShortInt);
begin
  stream.Read(value, 1);
end;
//------------------------------------------------------------------------------

function GetWord(stream: TStream; out value: WORD): Boolean;
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  stream.Read(value, SizeOf(value));
  value := WordSwap(value);
end;
//------------------------------------------------------------------------------

function GetInt16(stream: TStream; out value: Int16): Boolean;
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  stream.Read(value, SizeOf(value));
  value := Int16Swap(value);
end;
//------------------------------------------------------------------------------

function GetCardinal(stream: TStream; out value: Cardinal): Boolean;
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  stream.Read(value, SizeOf(value));
  value := Cardinal(Int32Swap(Integer(value)));
end;
//------------------------------------------------------------------------------

function GetInt(stream: TStream; out value: integer): Boolean;
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  stream.Read(value, SizeOf(value));
  value := Int32Swap(value);
end;
//------------------------------------------------------------------------------

function GetUInt64(stream: TStream; out value: UInt64): Boolean;
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  stream.Read(value, SizeOf(value));
  value := UInt64Swap(value);
end;
//------------------------------------------------------------------------------

function Get2Dot14(stream: TStream; out value: single): Boolean;
var
  val: Int16;
begin
  result := GetInt16(stream, val);
  if result then value := val * 6.103515625e-5; // 16384;
end;
//------------------------------------------------------------------------------

function GetFixed(stream: TStream; out value: TFixed): Boolean;
var
  val: integer;
begin
  result := GetInt(stream, val);
  value := val * 1.52587890625e-5; // 1/35536
end;
//------------------------------------------------------------------------------

function GetWideString(stream: TStream; length: integer): UnicodeString;
var
  i: integer;
  w: Word;
begin
  length := length div 2;
  setLength(Result, length);
  for i := 1 to length do
  begin
    GetWord(stream, w); //nb: reverses byte order
    if w = 0 then
    begin
      SetLength(Result, i -1);
      break;
    end;
    result[i] := WideChar(w);
   end;
end;
//------------------------------------------------------------------------------

function GetAnsiString(stream: TStream; len: integer): string;
var
  i: integer;
  ansi: UTF8String;
begin
  setLength(ansi, len+1);
  ansi[len+1] := #0;
  stream.Read(ansi[1], len);
  result := string(ansi);
  for i := 1 to length(Result) do
    if Result[i] = #0 then
    begin
      SetLength(Result, i -1);
      break;
    end;
end;

//------------------------------------------------------------------------------
// TTrueTypeReader
//------------------------------------------------------------------------------

constructor TFontReader.Create;
begin
  fStream := TMemoryStream.Create;
end;
//------------------------------------------------------------------------------

constructor TFontReader.CreateFromResource(const resName: string; resType: PChar);
begin
  Create;
  LoadFromResource(resName, resType);
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
constructor TFontReader.Create(const fontname: string);
begin
  Create;
  Load(fontname);
end;
//------------------------------------------------------------------------------
{$ENDIF}

destructor TFontReader.Destroy;
begin
  Clear;
  NotifyRecipients(inDestroy);
  fStream.Free;
  if Assigned(fFontManager) then
  begin
    fDestroying := true;
    fFontManager.Delete(self);
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TFontReader.Clear;
begin
  fTables               := nil;
  fCmapTblRecs          := nil;
  fFormat4Offset        := 0;
  fFormat4EndCodes      := nil;
  fKernTable            := nil;
  FillChar(fTbl_post, SizeOf(fTbl_post), 0);
  fTbl_glyf.numContours := 0;
  fFontInfo.fontFormat  := ffInvalid;
  fFontInfo.fontFamily  := ttfUnknown;
  fFontWeight           := 0;
  fStream.Clear;
  NotifyRecipients(inStateChange);
end;
//------------------------------------------------------------------------------

procedure TFontReader.BeginUpdate;
begin
  inc(fUpdateCount);
end;
//------------------------------------------------------------------------------

procedure TFontReader.EndUpdate;
begin
  dec(fUpdateCount);
  if fUpdateCount = 0 then NotifyRecipients(inStateChange);
end;
//------------------------------------------------------------------------------

procedure TFontReader.NotifyRecipients(notifyFlag: TImg32Notification);
var
  i: integer;
begin
  if fUpdateCount > 0 then Exit;
  for i := High(fRecipientList) downto 0 do
    try
      //when destroying in a finalization section
      //it's possible for recipients to have been destroyed
      //without their destructors being called.
      fRecipientList[i].ReceiveNotification(self, notifyFlag);
    except
    end;
end;
//------------------------------------------------------------------------------

procedure TFontReader.AddRecipient(recipient: INotifyRecipient);
var
  len: integer;
begin
  len := Length(fRecipientList);
  SetLength(fRecipientList, len+1);
  fRecipientList[len] := Recipient;
end;
//------------------------------------------------------------------------------

procedure TFontReader.DeleteRecipient(recipient: INotifyRecipient);
var
  i, highI: integer;
begin
  highI := High(fRecipientList);
  i := highI;
  while (i >= 0) and (fRecipientList[i] <> Recipient) do dec(i);
  if i < 0 then Exit;
  if i < highI then
    Move(fRecipientList[i+1], fRecipientList[i],
      (highI - i) * SizeOf(INotifyRecipient));
  SetLength(fRecipientList, highI);
end;
//------------------------------------------------------------------------------

function TFontReader.IsValidFontFormat: Boolean;
begin
  result := fFontInfo.fontFormat = ffTrueType;
end;
//------------------------------------------------------------------------------

function TFontReader.LoadFromStream(stream: TStream): Boolean;
begin
  BeginUpdate;
  try
    Clear;
    fStream.CopyFrom(stream, 0);
    fStream.Position := 0;
    result := GetTables;
    if not result then Clear;
  finally
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.LoadFromResource(const resName: string; resType: PChar): Boolean;
var
  rs: TResourceStream;
begin
  BeginUpdate;
  rs := CreateResourceStream(resName, resType);
  try
    Result := assigned(rs) and LoadFromStream(rs);
  finally
    rs.free;
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.LoadFromFile(const filename: string): Boolean;
var
  fs: TFileStream;
begin
  try
    fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
    try
      Result := LoadFromStream(fs);
    finally
      fs.free;
    end;
  except
    Result := False;
  end;
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function GetFontMemStreamFromFontHdl(hdl: HFont;
  memStream: TMemoryStream): Boolean;
var
  memDc: HDC;
  cnt: DWORD;
begin
  result := false;
  if not Assigned(memStream) or (hdl = 0) then Exit;

  memDc := CreateCompatibleDC(0);
  try
    SelectObject(memDc, hdl);
    //get the required size of the font data (file)
    cnt := Windows.GetFontData(memDc, 0, 0, nil, 0);
    result := cnt <> $FFFFFFFF;
    if not Result then Exit;
    //copy the font data into the memory stream
    memStream.SetSize(cnt);
    Windows.GetFontData(memDc, 0, 0, memStream.Memory, cnt);
  finally
    DeleteDC(memDc);
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.LoadUsingFontHdl(hdl: HFont): Boolean;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    Result := GetFontMemStreamFromFontHdl(hdl, ms) and
      LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.Load(const fontname: string): Boolean;
var
  logfont: TLogFont;
  hdl: HFont;
begin
  Result := false;
  FillChar(logfont, SizeOf(logfont), 0);
  StrPCopy(@logfont.lfFaceName[0], fontname);
  hdl := CreateFontIndirect(logfont);
  if hdl = 0 then Exit;
  try
    Result := LoadUsingFontHdl(hdl);
  finally
    DeleteObject(hdl);
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF}

function GetHeaderTable(stream: TStream;
  out headerTable: TFontHeaderTable): Boolean;
begin
  result := stream.Position < stream.Size - SizeOf(TFontHeaderTable);
  if not result then Exit;
  GetCardinal(stream, headerTable.sfntVersion);
  GetWord(stream, headerTable.numTables);
  GetWord(stream, headerTable.searchRange);
  GetWord(stream, headerTable.entrySelector);
  GetWord(stream, headerTable.rangeShift);
end;
//------------------------------------------------------------------------------

function TFontReader.GetTables: Boolean;
var
  i, tblCount: integer;
  tbl: TTableName;
  headerTable: TFontHeaderTable;
begin
  result := false;
  if not GetHeaderTable(fStream, headerTable) then Exit;
  tblCount := headerTable.numTables;
  result := fStream.Position < fStream.Size - SizeOf(TFontTable) * tblCount;
  if not result then Exit;

  for tbl := low(TTableName) to High(TTableName) do fTblIdxes[tbl] := -1;
  SetLength(fTables, tblCount);

  for i := 0 to tblCount -1 do
  begin
    GetCardinal(fStream, fTables[i].tag);
    GetCardinal(fStream, fTables[i].checkSum);
    GetCardinal(fStream, fTables[i].offset);
    GetCardinal(fStream, fTables[i].length);
    case
      fTables[i].tag of
        $6E616D65: fTblIdxes[tblName] := i;
        $68656164: fTblIdxes[tblHead] := i;
        $676C7966: fTblIdxes[tblGlyf] := i;
        $6C6F6361: fTblIdxes[tblLoca] := i;
        $6D617870: fTblIdxes[tblMaxp] := i;
        $636D6170: fTblIdxes[tblCmap] := i;
        $68686561: fTblIdxes[tblHhea] := i;
        $686D7478: fTblIdxes[tblHmtx] := i;
        $6B65726E: fTblIdxes[tblKern] := i;
        $706F7374: fTblIdxes[tblPost] := i;
    end;
  end;

  if fTblIdxes[tblName] < 0 then fFontInfo.fontFormat := ffInvalid
  else if fTblIdxes[tblGlyf] < 0 then fFontInfo.fontFormat := ffCompact
  else fFontInfo.fontFormat := ffTrueType;

  result := (fFontInfo.fontFormat = ffTrueType) and
    (fTblIdxes[tblName] >= 0) and GetTable_name and
    (fTblIdxes[tblHead] >= 0) and GetTable_head and
    (fTblIdxes[tblHhea] >= 0) and GetTable_hhea and
    (fTblIdxes[tblMaxp] >= 0) and GetTable_maxp and
    (fTblIdxes[tblLoca] >= 0) and GetTable_loca and //loca must follow maxp
    (fTblIdxes[tblCmap] >= 0) and GetTable_cmap;

  if not Result then Exit;
  if (fTblIdxes[tblKern] >= 0) then GetTable_kern;
  if (fTblIdxes[tblPost] >= 0) then GetTable_post;

  fFontInfo.fontFamily := GetFontFamily;
end;
//------------------------------------------------------------------------------

function TFontReader.GetTable_cmap: Boolean;
var
  i, segCount: integer;
  reserved: WORD;
  cmapRec: TCmapTblRec;
  format4Rec: TCmapFormat4;
  cmapTbl: TFontTable;
begin
  Result := false;
  cmapTbl := fTables[fTblIdxes[tblCmap]];
  if (fStream.Size < cmapTbl.offset + cmapTbl.length) then Exit;

  fStream.Position := cmapTbl.offset;
  GetWord(fStream, fTbl_cmap.version);
  GetWord(fStream, fTbl_cmap.numTables);

  //only use the unicode table (0: always first)
  SetLength(fCmapTblRecs, fTbl_cmap.numTables);
  for i := 0 to fTbl_cmap.numTables -1 do
  begin
    GetWord(fStream, fCmapTblRecs[i].platformID);
    GetWord(fStream, fCmapTblRecs[i].encodingID);
    GetCardinal(fStream, fCmapTblRecs[i].offset);
  end;

  i := 0;
  while (i < fTbl_cmap.numTables) and
    (fCmapTblRecs[i].platformID <> 0) and
    (fCmapTblRecs[i].platformID <> 3) do inc(i);
  if i = fTbl_cmap.numTables then Exit;
  cmapRec := fCmapTblRecs[i];

  fStream.Position := cmapTbl.offset + cmapRec.offset;
  GetWord(fStream, format4Rec.format);
  GetWord(fStream, format4Rec.length);
  GetWord(fStream, format4Rec.language);

  if format4Rec.format = 0 then
  begin
    for i := 0 to 255 do
      GetByte(fStream, fFormat0CodeMap[i]);
    fFontInfo.glyphCount := 255;
  end
  else if format4Rec.format = 4 then
  begin
    fFontInfo.glyphCount := 0;
    GetWord(fStream, format4Rec.segCountX2);
    segCount := format4Rec.segCountX2 shr 1;
    GetWord(fStream, format4Rec.searchRange);
    GetWord(fStream, format4Rec.entrySelector);
    GetWord(fStream, format4Rec.rangeShift);
    SetLength(fFormat4EndCodes, segCount);
    for i := 0 to segCount -1 do
      GetWord(fStream, fFormat4EndCodes[i]);
    if fFormat4EndCodes[segCount-1] <> $FFFF then Exit; //error
    GetWord(fStream, reserved);
    if reserved <> 0 then Exit; //error
    SetLength(fFormat4StartCodes, segCount);
    for i := 0 to segCount -1 do
      GetWord(fStream, fFormat4StartCodes[i]);
    if fFormat4StartCodes[segCount-1] <> $FFFF then Exit; //error
    SetLength(fFormat4IdDelta, segCount);
    for i := 0 to segCount -1 do
      GetWord(fStream, fFormat4IdDelta[i]);
    SetLength(fFormat4RangeOff, segCount);
    fFormat4Offset := fStream.Position;
    for i := 0 to segCount -1 do
      GetWord(fStream, fFormat4RangeOff[i]);
  end else
    Exit; //unsupported format

  Result := true;
end;
//------------------------------------------------------------------------------

function TFontReader.GetGlyphIdxFromCmapIdx(idx: Word): integer;
var
  i: integer;
  w: WORD;
begin

  if fFormat4Offset = 0 then
  begin
    if idx > 255 then Result := 0
    else Result := fFormat0CodeMap[idx];
    Exit;
  end;

  //Format4 mapping

  result := 0; //default to the 'missing' glyph
  for i := 0 to High(fFormat4EndCodes) do
    if idx <= fFormat4EndCodes[i] then
    begin
      if idx < fFormat4StartCodes[i] then Exit;
      if fFormat4RangeOff[i] > 0 then
      begin
        fStream.Position := fFormat4Offset + fFormat4RangeOff[i] +
          2 * (i + idx - fFormat4StartCodes[i]);
        GetWord(fStream, w);
        if w < fTbl_maxp.numGlyphs then Result := w;
      end else
        result := (fFormat4IdDelta[i] + idx) and $FFFF;
      Exit;
    end;
end;
//------------------------------------------------------------------------------

function TFontReader.GetTable_maxp: Boolean;
var
  maxpTbl: TFontTable;
begin
  maxpTbl := fTables[fTblIdxes[tblMaxp]];
  Result := (fStream.Size >= maxpTbl.offset + maxpTbl.length) and
    (maxpTbl.length >= SizeOf(TFixed) + SizeOf(WORD));
  if not Result then Exit;
  fStream.Position := maxpTbl.offset;
  GetFixed(fStream, fTbl_maxp.version);
  GetWord(fStream, fTbl_maxp.numGlyphs);
  if fTbl_maxp.version >= 1 then
  begin
    GetWord(fStream, fTbl_maxp.maxPoints);
    GetWord(fStream, fTbl_maxp.maxContours);
    fFontInfo.glyphCount := fTbl_maxp.numGlyphs;
  end else
    Result := false;
end;
//------------------------------------------------------------------------------

function TFontReader.GetTable_loca: Boolean;
var
  i: integer;
  locaTbl: TFontTable;
begin
  locaTbl := fTables[fTblIdxes[tblLoca]];
  Result := fStream.Size >= locaTbl.offset + locaTbl.length;
  if not Result then Exit;
  fStream.Position := locaTbl.offset;

  if fTbl_head.indexToLocFmt = 0 then
  begin
    SetLength(fTbl_loca2, fTbl_maxp.numGlyphs +1);
    for i := 0 to fTbl_maxp.numGlyphs do
      GetWord(fStream, fTbl_loca2[i]);
  end else
  begin
    SetLength(fTbl_loca4, fTbl_maxp.numGlyphs +1);
    for i := 0 to fTbl_maxp.numGlyphs do
      GetCardinal(fStream, fTbl_loca4[i]);
  end;
end;
//------------------------------------------------------------------------------


function IsUnicode(platformID: Word): Boolean;
begin
  Result := (platformID = 0) or (platformID = 3);
end;
//------------------------------------------------------------------------------

function GetNameRecString(stream: TStream;
  const nameRec: TNameRec; offset: cardinal): UnicodeString;
var
  sPos, len: integer;
begin
  sPos := stream.Position;
  stream.Position := offset + nameRec.offset;
  if IsUnicode(nameRec.platformID) then
    Result := GetWideString(stream, nameRec.length) else
    Result := UnicodeString(GetAnsiString(stream, nameRec.length));
  len := Length(Result);
  if (len > 0) and (Result[len] = #0) then SetLength(Result, len -1);
  stream.Position := sPos;
end;
//------------------------------------------------------------------------------

function TFontReader.GetTable_name: Boolean;
var
  i: integer;
  offset: cardinal;
  nameRec: TNameRec;
  nameTbl: TFontTable;
begin
  fFontInfo.faceName := '';
  fFontInfo.style   := '';
  nameTbl := fTables[fTblIdxes[tblName]];
  Result := (fStream.Size >= nameTbl.offset + nameTbl.length) and
    (nameTbl.length >= SizeOf(TFontTable_Name));
  if not Result then Exit;
  fStream.Position := nameTbl.offset;
  GetWord(fStream, fTbl_name.format);
  GetWord(fStream, fTbl_name.count);
  GetWord(fStream, fTbl_name.stringOffset);
  offset := nameTbl.offset + fTbl_name.stringOffset;
  for i := 1 to fTbl_name.count do
  begin
    GetWord(fStream, nameRec.platformID);
    GetWord(fStream, nameRec.encodingID);
    GetWord(fStream, nameRec.languageID);
    GetWord(fStream, nameRec.nameID);
    GetWord(fStream, nameRec.length);
    GetWord(fStream, nameRec.offset);
    case nameRec.nameID of
      0: fFontInfo.copyright    := GetNameRecString(fStream, nameRec, offset);
      1: fFontInfo.faceName     := GetNameRecString(fStream, nameRec, offset);
      2: fFontInfo.style        := GetNameRecString(fStream, nameRec, offset);
      3..7: continue;
      8: fFontInfo.manufacturer := GetNameRecString(fStream, nameRec, offset);
      else break;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.GetTable_head: Boolean;
var
  headTbl: TFontTable;
  yy,mo,dd,hh,mi,ss: cardinal;
begin
  headTbl := fTables[fTblIdxes[tblHead]];
  Result := (fStream.Size >= headTbl.offset +
    headTbl.length) and (headTbl.length >= 54);
  if not Result then Exit;
  fStream.Position := headTbl.offset;
  GetWord(fStream, fTbl_head.majorVersion);
  GetWord(fStream, fTbl_head.minorVersion);
  GetFixed(fStream, fTbl_head.fontRevision);

  GetCardinal(fStream, fTbl_head.checkSumAdjust);
  GetCardinal(fStream, fTbl_head.magicNumber);
  GetWord(fStream, fTbl_head.flags);
  GetWord(fStream, fTbl_head.unitsPerEm);

  GetUInt64(fStream, fTbl_head.dateCreated);
  GetMeaningfulDateTime(fTbl_head.dateCreated, yy,mo,dd,hh,mi,ss);
  fFontInfo.dateCreated := EncodeDate(yy,mo,dd) + EncodeTime(hh,mi,ss, 0);
  GetUInt64(fStream, fTbl_head.dateModified);
  GetMeaningfulDateTime(fTbl_head.dateModified, yy,mo,dd,hh,mi,ss);
  fFontInfo.dateModified := EncodeDate(yy,mo,dd) + EncodeTime(hh,mi,ss, 0);

  GetInt16(fStream, fTbl_head.xMin);
  GetInt16(fStream, fTbl_head.yMin);
  GetInt16(fStream, fTbl_head.xMax);
  GetInt16(fStream, fTbl_head.yMax);
  GetWord(fStream, fTbl_head.macStyle);
  fFontInfo.macStyles := TMacStyles(Byte(fTbl_head.macStyle));
  GetWord(fStream, fTbl_head.lowestRecPPEM);
  GetInt16(fStream, fTbl_head.fontDirHint);
  GetInt16(fStream, fTbl_head.indexToLocFmt);
  GetInt16(fStream, fTbl_head.glyphDataFmt);
  result := fTbl_head.magicNumber = $5F0F3CF5
end;
//------------------------------------------------------------------------------

function TFontReader.GetTable_hhea: Boolean;
var
  hheaTbl: TFontTable;
begin
  hheaTbl := fTables[fTblIdxes[tblHhea]];
  Result := (fStream.Size >= hheaTbl.offset + hheaTbl.length) and
    (hheaTbl.length >= 36);
  if not Result then Exit;
  fStream.Position := hheaTbl.offset;

  GetFixed(fStream,  fTbl_hhea.version);
  GetInt16(fStream,  fTbl_hhea.ascent);
  GetInt16(fStream,  fTbl_hhea.descent);
  GetInt16(fStream,  fTbl_hhea.lineGap);
  GetWord(fStream,   fTbl_hhea.advWidthMax);
  GetInt16(fStream,  fTbl_hhea.minLSB);
  GetInt16(fStream,  fTbl_hhea.minRSB);
  GetInt16(fStream,  fTbl_hhea.xMaxExtent);
  GetInt16(fStream,  fTbl_hhea.caretSlopeRise);
  GetInt16(fStream,  fTbl_hhea.caretSlopeRun);
  GetInt16(fStream,  fTbl_hhea.caretOffset);
  GetUInt64(fStream, fTbl_hhea.reserved);
  GetInt16(fStream,  fTbl_hhea.metricDataFmt);
  GetWord(fStream,   fTbl_hhea.numLongHorMets);
end;
//------------------------------------------------------------------------------

function TFontReader.GetGlyphHorzMetrics(glyphIdx: integer): Boolean;
var
  tbl            : TFontTable;
begin
  tbl := fTables[fTblIdxes[tblHmtx]];
  Result := (fStream.Size >= tbl.offset + tbl.length);
  if not Result then Exit;
  if glyphIdx < fTbl_hhea.numLongHorMets then
  begin
    fStream.Position := Integer(tbl.offset) + glyphIdx * 4;
    GetWord(fStream, fTbl_hmtx.advanceWidth);
    GetInt16(fStream, fTbl_hmtx.leftSideBearing);
  end else
  begin
    fStream.Position := Integer(tbl.offset) +
      Integer(fTbl_hhea.numLongHorMets -1) * 4;
    GetWord(fStream, fTbl_hmtx.advanceWidth);
    fStream.Position := Integer(tbl.offset +
      fTbl_hhea.numLongHorMets * 4) +
      2 * (glyphIdx - Integer(fTbl_hhea.numLongHorMets));
    GetInt16(fStream, fTbl_hmtx.leftSideBearing);
  end;
end;
//------------------------------------------------------------------------------

procedure TFontReader.GetTable_kern;
var
  i              : integer;
  tbl            : TFontTable;
  tbl_kern       : TFontTable_Kern;
  kernSub        : TKernSubTbl;
  format0KernHdr : TFormat0KernHdr;
begin
  if fTblIdxes[tblKern] < 0 then Exit;
  tbl := fTables[fTblIdxes[tblKern]];
  if (fStream.Size < tbl.offset + tbl.length) then Exit;
  fStream.Position := Integer(tbl.offset);

  GetWord(fStream, tbl_kern.version);
  GetWord(fStream, tbl_kern.numTables);
  if tbl_kern.numTables = 0 then Exit;
  //assume there's only one kern table

  GetWord(fStream, kernSub.version);
  GetWord(fStream, kernSub.length);
  GetWord(fStream, kernSub.coverage);
  //we're currently only interested in Format0 horizontal kerning
  if kernSub.coverage <> 1 then Exit;

  GetWord(fStream, format0KernHdr.nPairs);
  GetWord(fStream, format0KernHdr.searchRange);
  GetWord(fStream, format0KernHdr.entrySelector);
  GetWord(fStream, format0KernHdr.rangeShift);

  SetLength(fKernTable, format0KernHdr.nPairs);
  for i := 0 to format0KernHdr.nPairs -1 do
  begin
    GetWord(fStream, fKernTable[i].left);
    GetWord(fStream, fKernTable[i].right);
    GetInt16(fStream, fKernTable[i].value);
  end;
end;
//------------------------------------------------------------------------------

procedure TFontReader.GetTable_post;
var
  tbl: TFontTable;
begin
  if fTblIdxes[tblPost] < 0 then Exit;
  tbl := fTables[fTblIdxes[tblPost]];
  if (fStream.Size < tbl.offset + tbl.length) then Exit;
  fStream.Position := Integer(tbl.offset);

  GetWord(fStream,      fTbl_post.majorVersion);
  GetWord(fStream,      fTbl_post.minorVersion);
  GetFixed(fStream,     fTbl_post.italicAngle);
  GetInt16(fStream,     fTbl_post.underlinePos);
  GetInt16(fStream,     fTbl_post.underlineWidth);
  GetCardinal(fStream,  fTbl_post.isFixedPitch);
end;
//------------------------------------------------------------------------------

function FindKernInTable(glyphIdx: integer;
  const kernTable: TArrayOfKernRecs): integer;
var
  i,l,r: integer;
begin
  l := 0;
  r := High(kernTable);
  while l <= r do
  begin
    Result := (l + r) shr 1;
    i := kernTable[Result].left - glyphIdx;
    if i < 0 then
    begin
      l := Result +1
    end else
    begin
      if i = 0 then
      begin
        //found a match! Now find the very first one ...
        while (Result > 0) and
          (kernTable[Result-1].left = glyphIdx) do dec(Result);
        Exit;
      end;
      r := Result -1;
    end;
  end;
  Result := -1;
end;
//------------------------------------------------------------------------------

function TFontReader.GetGlyphKernList(glyphIdx: integer): TArrayOfTKern;
var
  i,j,len: integer;
begin
  result := nil;
  i := FindKernInTable(glyphIdx, fKernTable);
  if i < 0 then Exit;
  len := Length(fKernTable);
  j := i +1;
  while (j < len) and (fKernTable[j].left = glyphIdx) do inc(j);
  SetLength(Result, j - i);
  for j := 0 to High(Result) do
    with fKernTable[i+j] do
  begin
    Result[j].rightGlyphIdx := right;
    Result[j].kernValue := value;
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.GetGlyphPaths(glyphIdx: integer): TPathsEx;
var
  offset: cardinal;
  glyfTbl: TFontTable;
begin
  result := nil;
  if fTbl_head.indexToLocFmt = 0 then
  begin
    offset := fTbl_loca2[glyphIdx] *2;
    if offset = fTbl_loca2[glyphIdx+1] *2 then Exit; //no contours
  end else
  begin
    offset := fTbl_loca4[glyphIdx];
    if offset = fTbl_loca4[glyphIdx+1] then Exit; //no contours
  end;
  glyfTbl := fTables[fTblIdxes[tblGlyf]];
  if offset >= glyfTbl.length then Exit;
  inc(offset, glyfTbl.offset);

  fStream.Position := offset;
  GetInt16(fStream, fTbl_glyf.numContours);
  GetInt16(fStream, fTbl_glyf.xMin);
  GetInt16(fStream, fTbl_glyf.yMin);
  GetInt16(fStream, fTbl_glyf.xMax);
  GetInt16(fStream, fTbl_glyf.yMax);

  if fTbl_glyf.numContours < 0 then
    result := GetCompositeGlyph else
    result := GetSimpleGlyph;
end;
//------------------------------------------------------------------------------

const
  //glyf flags - simple
  ON_CURVE                  = $1;
  X_SHORT_VECTOR            = $2;
  Y_SHORT_VECTOR            = $4;
  REPEAT_FLAG               = $8;
  X_DELTA                   = $10;
  Y_DELTA                   = $20;
//------------------------------------------------------------------------------

function TFontReader.GetSimpleGlyph: TPathsEx;
var
  i,j: integer;
  instructLen: WORD;
  flag, repeats: byte;
  contourEnds: TArrayOfWord;
begin
  SetLength(contourEnds, fTbl_glyf.numContours);
  for i := 0 to High(contourEnds) do
    GetWord(fStream, contourEnds[i]);

  //hints are currently ignored
  GetWord(fStream, instructLen);
  fStream.Position := fStream.Position + instructLen;

  setLength(result, fTbl_glyf.numContours);
  setLength(result[0], contourEnds[0] +1);
  for i := 1 to High(result) do
    setLength(result[i], contourEnds[i] - contourEnds[i-1]);

  repeats := 0;
  for i := 0 to High(result) do
  begin
    for j := 0 to High(result[i]) do
    begin
      if repeats = 0 then
      begin
        GetByte(fStream, flag);
        if flag and REPEAT_FLAG = REPEAT_FLAG then
          GetByte(fStream, repeats);
      end else
        dec(repeats);
      result[i][j].flag := flag;
    end;
  end;
  GetPathCoords(result);
end;
//------------------------------------------------------------------------------

procedure TFontReader.GetPathCoords(var paths: TPathsEx);
var
  i,j: integer;
  xi,yi: Int16;
  flag, xb,yb: byte;
  pt: TPoint;
begin
  if fTbl_glyf.numContours = 0 then Exit;

  //get X coords
  pt := Point(0,0);
  xi := 0;
  for i := 0 to high(paths) do
  begin
    for j := 0 to high(paths[i]) do
    begin
      flag := paths[i][j].flag;
      if flag and X_SHORT_VECTOR = X_SHORT_VECTOR then
      begin
        GetByte(fStream, xb);
        if (flag and X_DELTA) = 0 then
          dec(pt.X, xb) else
          inc(pt.X, xb);
      end else
      begin
        if flag and X_DELTA = 0 then
        begin
          GetInt16(fStream, xi);
          pt.X := pt.X + xi;
        end;
      end;
      paths[i][j].pt.X := pt.X;
    end;
  end;

  //get Y coords
  yi := 0;
  for i := 0 to high(paths) do
  begin
    for j := 0 to high(paths[i]) do
    begin
      flag := paths[i][j].flag;
      if flag and Y_SHORT_VECTOR = Y_SHORT_VECTOR then
      begin
        GetByte(fStream, yb);
        if (flag and Y_DELTA) = 0 then
          dec(pt.Y, yb) else
          inc(pt.Y, yb);
      end else
      begin
        if flag and Y_DELTA = 0 then
        begin
          GetInt16(fStream, yi);
          pt.Y := pt.Y + yi;
        end;
      end;
      paths[i][j].pt.Y := pt.Y;
    end;
  end;
end;
//------------------------------------------------------------------------------

function OnCurve(flag: byte): Boolean;
begin
  result := flag and ON_CURVE <> 0;
end;
//------------------------------------------------------------------------------

function MidPoint(const pt1, pt2: TPointEx): TPointEx;
begin
  Result.pt.X := (pt1.pt.X + pt2.pt.X) / 2;
  Result.pt.Y := (pt1.pt.Y + pt2.pt.Y) / 2;
  Result.flag := ON_CURVE;
end;
//------------------------------------------------------------------------------

function TFontReader.ConvertSplinesToBeziers(const pathsEx: TPathsEx): TPathsEx;
var
  i,j,k: integer;
  pt: TPointEx;
  prevOnCurve: Boolean;
begin
  SetLength(Result, Length(pathsEx));
  for i := 0 to High(pathsEx) do
  begin
    SetLength(Result[i], Length(pathsEx[i]) *2);
    Result[i][0] := pathsEx[i][0]; k := 1;
    prevOnCurve := true;
    for j := 1 to High(pathsEx[i]) do
    begin
      if OnCurve(pathsEx[i][j].flag) then
      begin
        prevOnCurve := true;
      end
      else if not prevOnCurve then
      begin
        pt := MidPoint(pathsEx[i][j-1], pathsEx[i][j]);
        Result[i][k] := pt; inc(k);
      end else
        prevOnCurve := false;
      Result[i][k] := pathsEx[i][j]; inc(k);
    end;
    SetLength(Result[i], k);
  end;
end;
//------------------------------------------------------------------------------

procedure AppendPathsEx(var paths: TPathsEx; const extra: TPathsEx);
var
  i, len1, len2: integer;
begin
  len2 := length(extra);
  len1 := length(paths);
  setLength(paths, len1 + len2);
  for i := 0 to len2 -1 do
    paths[len1+i] := Copy(extra[i], 0, length(extra[i]));
end;
//------------------------------------------------------------------------------

procedure AffineTransform(const a,b,c,d,e,f: double; var pathsEx: TPathsEx);
const
  q = 9.2863575e-4; // 33/35536
var
  i,j: integer;
  m0,n0,m,n,xx: double;
begin
  m0 := max(abs(a), abs(b));
  n0 := max(abs(c), abs(d));

  if (m0 = 0) or (n0 = 0) then
  begin
    if (e = 0) and (f = 0) then Exit;

    for i := 0 to High(pathsEx) do
      for j := 0 to High(pathsEx[i]) do
        with pathsEx[i][j].pt do
        begin
          X := X + e;
          y := Y + f;
        end;

  end else
  begin
    //see https://developer.apple.com/fonts ...
    //... /TrueType-Reference-Manual/RM06/Chap6glyf.html

    if (abs(a)-abs(c))< q then m := 2 * m0 else m := m0;
    if (abs(b)-abs(d))< q then n := 2 * n0 else n := n0;

    for i := 0 to High(pathsEx) do
      for j := 0 to High(pathsEx[i]) do
        with pathsEx[i][j].pt do
        begin
          xx := m * ((a/m)*X + (c/m)*Y + e);
          y := m * ((b/n)*X + (d/n)*Y + f);
          X := xx;
        end;
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.GetCompositeGlyph: TPathsEx;
var
  streamPos: integer;
  flag, glyphIndex: WORD;
  arg1b, arg2b: ShortInt;
  arg1i, arg2i: Int16;
  tmp: single;
  a,b,c,d,e,f: double;
  componentPaths: TPathsEx;
  tbl_glyf_old: TFontTable_Glyf;
const
  ARG_1_AND_2_ARE_WORDS     = $1;
  ARGS_ARE_XY_VALUES        = $2;
  ROUND_XY_TO_GRID          = $4;
  WE_HAVE_A_SCALE           = $8;
  MORE_COMPONENTS           = $20;
  WE_HAVE_AN_X_AND_Y_SCALE  = $40;
  WE_HAVE_A_TWO_BY_TWO      = $80;
  WE_HAVE_INSTRUCTIONS      = $100;
  USE_MY_METRICS            = $200;
begin
  result := nil;
  flag := MORE_COMPONENTS;

  while (flag and MORE_COMPONENTS <> 0) do
  begin
    glyphIndex := 0;
    a := 0; b := 0; c := 0; d := 0; e := 0; f := 0;

    GetWord(fStream, flag);
    GetWord(fStream, glyphIndex);

    if (flag and ARG_1_AND_2_ARE_WORDS <> 0) then
    begin
      GetInt16(fStream, arg1i);
      GetInt16(fStream, arg2i);
      if (flag and ARGS_ARE_XY_VALUES <> 0) then
      begin
        e := arg1i;
        f := arg2i;
      end;
    end else
    begin
      GetShortInt(fStream, arg1b);
      GetShortInt(fStream, arg2b);
      if (flag and ARGS_ARE_XY_VALUES <> 0) then
      begin
        e := arg1b;
        f := arg2b;
      end;
    end;

    if (flag and WE_HAVE_A_SCALE <> 0) then
    begin
      Get2Dot14(fStream, tmp);
      a := tmp; d := tmp;
    end
    else if (flag and WE_HAVE_AN_X_AND_Y_SCALE <> 0) then
    begin
      Get2Dot14(fStream, tmp); a := tmp;
      Get2Dot14(fStream, tmp); d := tmp;
    end
    else if (flag and WE_HAVE_A_TWO_BY_TWO <> 0) then
    begin
      Get2Dot14(fStream, tmp); a := tmp;
      Get2Dot14(fStream, tmp); b := tmp;
      Get2Dot14(fStream, tmp); c := tmp;
      Get2Dot14(fStream, tmp); d := tmp;
    end;

    tbl_glyf_old := fTbl_glyf;

    streamPos := fStream.Position;
    componentPaths := GetGlyphPaths(glyphIndex);
    fStream.Position := streamPos;

    if (flag and ARGS_ARE_XY_VALUES <> 0) then
    begin
      if (flag and USE_MY_METRICS <> 0) then
      begin
        if Result <> nil then
          AffineTransform(a,b,c,d,e,f, result);
      end else
        AffineTransform(a,b,c,d,e,f, componentPaths);
    end;

    if tbl_glyf_old.numContours > 0 then
    begin
      inc(fTbl_glyf.numContours, tbl_glyf_old.numContours);
      fTbl_glyf.xMin := Min(fTbl_glyf.xMin, tbl_glyf_old.xMin);
      fTbl_glyf.xMax := Max(fTbl_glyf.xMax, tbl_glyf_old.xMax);
      fTbl_glyf.yMin := Min(fTbl_glyf.yMin, tbl_glyf_old.yMin);
      fTbl_glyf.yMax := Max(fTbl_glyf.yMax, tbl_glyf_old.yMax);
    end;

    AppendPathsEx(result, componentPaths);
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.GetGlyphInfo(unicode: Word; out paths: TPathsD;
  out nextX: integer; out glyphMetrics: TGlyphMetrics): Boolean;
var
  i,j, glyphIdx: integer;
  pt2: TPointEx;
  bez: TPathD;
  pathsEx: TPathsEx;
begin
  paths  := nil;
  Result := IsValidFontFormat;
  if not Result then Exit;

  glyphIdx := GetGlyphIdxFromCmapIdx(unicode);
  if not GetGlyphHorzMetrics(glyphIdx) then Exit;

  pathsEx := GetGlyphPaths(glyphIdx); //gets raw splines
  pathsEx := ConvertSplinesToBeziers(pathsEx);
  glyphMetrics := GetGlyphMetricsInternal(glyphIdx); //nb: must follow GetGlyphPaths
  nextX   := fTbl_hmtx.advanceWidth;
  if pathsEx = nil then Exit; //eg space character

  //now flatten ...
  setLength(paths, length(pathsEx));
  for i := 0 to High(pathsEx) do
  begin
    SetLength(paths[i],1);
    paths[i][0] := pathsEx[i][0].pt;
    for j := 1 to High(pathsEx[i]) do
    begin
      if OnCurve(pathsEx[i][j].flag) then
      begin
        AppendPoint(paths[i], pathsEx[i][j].pt);
      end else
      begin
        if j = High(pathsEx[i]) then
          pt2 := pathsEx[i][0] else
          pt2 := pathsEx[i][j+1];
        bez := FlattenQBezier(pathsEx[i][j-1].pt,
                  pathsEx[i][j].pt, pt2.pt);
        AppendPath(paths[i], bez);
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.GetFontInfo: TFontInfo;
begin
  if not IsValidFontFormat then
  begin
    result.faceName := '';
    result.style := '';
    result.unitsPerEm := 0;
  end else
  begin
    result := fFontInfo;
    //and updated the record with everything except the strings
    result.unitsPerEm  := fTbl_head.unitsPerEm;
    result.xMin        := fTbl_head.xMin;
    result.xMax        := fTbl_head.xMax;
    result.yMin        := fTbl_head.yMin;
    result.yMax        := fTbl_head.yMax;

    //note: the following three fields "represent the design
    //intentions of the font's creator rather than any computed value"
    //https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6hhea.html
    result.ascent      := fTbl_hhea.ascent;
    result.descent     := abs(fTbl_hhea.descent);
    result.lineGap     := fTbl_hhea.lineGap;

    result.advWidthMax := fTbl_hhea.advWidthMax;
    result.minLSB      := fTbl_hhea.minLSB;
    result.minRSB      := fTbl_hhea.minRSB;
    result.xMaxExtent  := fTbl_hhea.xMaxExtent;
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.GetGlyphMetricsInternal(glyphIdx: integer): TGlyphMetrics;
begin
  if IsValidFontFormat then
  begin
    result.glyphIdx := glyphIdx;
    result.unitsPerEm  := fTbl_head.unitsPerEm;
    result.glyf := fTbl_glyf;
    result.hmtx := ftbl_hmtx;
    Result.kernList := GetGlyphKernList(glyphIdx);
  end else
    FillChar(result, sizeOf(Result), 0)
end;
//------------------------------------------------------------------------------

function TFontReader.GetWeight: integer;
var
  glyph: TPathsD;
  i, dummy: integer;
  accum: Cardinal;
  gm: TGlyphMetrics;
  rec: TRectD;
  img: TImage32;
  p: PARGB;
const
  imgSize = 16;
  k = 5; //empirical constant
begin
  //get an empirical weight based on the character 'G'
  result := 0;
  if not IsValidFontFormat then Exit;
  if fFontWeight > 0 then
  begin
    Result := fFontWeight;
    Exit;
  end;
  GetGlyphInfo(Ord('G'),glyph, dummy, gm);
  rec := GetBoundsD(glyph);
  glyph := Img32.Vector.OffsetPath(glyph, -rec.Left, -rec.Top);
  glyph := Img32.Vector.ScalePath(glyph,
    imgSize/rec.Width, imgSize/rec.Height);
  img := TImage32.Create(imgSize,imgSize);
  try
    DrawPolygon(img, glyph, frEvenOdd, clBlack32);
    accum := 0;
    p := PARGB(img.PixelBase);
    for i := 0 to imgSize * imgSize do
    begin
      inc(accum, p.A);
      inc(p);
    end;
  finally
    img.Free;
  end;
  fFontWeight := Max(100, Min(900,
    Round(k * accum / (imgSize * imgSize * 100)) * 100));
  Result := fFontWeight;
end;
//------------------------------------------------------------------------------

function TFontReader.GetFontFamily: TTtfFontFamily;
var
  dummy: integer;
  glyphsT, glyphsI, glyphsM: TPathsD;
  gmT, gmI, gmM: TGlyphMetrics;
begin
  result := ttfUnknown;
  if (fTbl_post.majorVersion > 0) and
    (fTbl_post.isFixedPitch <> 0) then
  begin
    result := ttfMonospace;
    Exit;
  end else
  begin
    if not GetGlyphInfo(Ord('T'), glyphsT, dummy, gmT) or
      not Assigned(glyphsT) or
      not GetGlyphInfo(Ord('i'), glyphsI, dummy, gmI) or
      not GetGlyphInfo(Ord('m'), glyphsM, dummy, gmM) then
        Exit;
    if gmi.hmtx.advanceWidth = gmm.hmtx.advanceWidth then
      Result := ttfMonospace
    else if Length(glyphsT[0]) <= 12 then //probably <= 8 is fine too.
      Result := ttfSansSerif else
      Result := ttfSerif;
  end;
end;

//------------------------------------------------------------------------------
// TFontCache
//------------------------------------------------------------------------------

constructor TFontCache.Create(fontReader: TFontReader; fontHeight: double);
begin
{$IFDEF XPLAT_GENERICS}
  fGlyphInfoList := TList<PGlyphInfo>.Create;
{$ELSE}
  fGlyphInfoList := TList.Create;
{$ENDIF}
  fSorted := false;
  fUseKerning := true;
  fFlipVert := true;

  fFontHeight := fontHeight;
  SetFontReader(fontReader);
end;
//------------------------------------------------------------------------------

destructor TFontCache.Destroy;
begin
  SetFontReader(nil);
  Clear;
  NotifyRecipients(inDestroy);
  fGlyphInfoList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TFontCache.ReceiveNotification(Sender: TObject; notify: TImg32Notification);
begin
  if Sender <> fFontReader then
    raise Exception.Create(rsFontCacheError);
  if notify = inStateChange then
  begin
    Clear;
    UpdateScale;
  end else
    SetFontReader(nil);
end;
//------------------------------------------------------------------------------

procedure TFontCache.NotifyRecipients(notifyFlag: TImg32Notification);
var
  i: integer;
begin
  for i := High(fRecipientList) downto 0 do
    try
      //when destroying in in a finalization section
      //it's possible for recipients to have been destroyed
      //without their destructors being called.
      fRecipientList[i].ReceiveNotification(self, notifyFlag);
    except
    end;
end;
//------------------------------------------------------------------------------

procedure TFontCache.AddRecipient(recipient: INotifyRecipient);
var
  len: integer;
begin
  len := Length(fRecipientList);
  SetLength(fRecipientList, len+1);
  fRecipientList[len] := Recipient;
end;
//------------------------------------------------------------------------------

procedure TFontCache.DeleteRecipient(recipient: INotifyRecipient);
var
  i, highI: integer;
begin
  highI := High(fRecipientList);
  i := highI;
  while (i >= 0) and (fRecipientList[i] <> Recipient) do dec(i);
  if i < 0 then Exit;
  if i < highI then
    Move(fRecipientList[i+i], fRecipientList[i],
      (highI - i) * SizeOf(INotifyRecipient));
  SetLength(fRecipientList, highI);
end;
//------------------------------------------------------------------------------

procedure TFontCache.Clear;
var
  i: integer;
begin
  for i := 0 to fGlyphInfoList.Count -1 do
    Dispose(PGlyphInfo(fGlyphInfoList[i]));
  fGlyphInfoList.Clear;
  fSorted := false;
end;
//------------------------------------------------------------------------------

{$IFDEF XPLAT_GENERICS}
function FindInSortedList(charOrdinal: WORD; glyphList: TList<PGlyphInfo>): integer;
{$ELSE}
function FindInSortedList(charOrdinal: WORD; glyphList: TList): integer;
{$ENDIF}
var
  i,l,r: integer;
begin
  //binary search the sorted list ...
  l := 0;
  r := glyphList.Count -1;
  while l <= r do
  begin
    Result := (l + r) shr 1;
    i := PGlyphInfo(glyphList[Result]).unicode - charOrdinal;
    if i < 0 then
    begin
      l := Result +1
    end else
    begin
      if i = 0 then Exit;
      r := Result -1;
    end;
  end;
  Result := -1;
end;
//------------------------------------------------------------------------------

function TFontCache.FoundInList(charOrdinal: WORD): Boolean;
begin
  if not fSorted then Sort;
  result := FindInSortedList(charOrdinal, fGlyphInfoList) >= 0;
end;
//------------------------------------------------------------------------------

procedure TFontCache.GetMissingGlyphs(const ordinals: TArrayOfWord);
var
  i, len: integer;
begin
  if not IsValidFont then Exit;
  len := Length(ordinals);
  for i := 0 to len -1 do
  begin
    if ordinals[i] < 32 then continue
    else if not FoundInList(ordinals[i]) then AddGlyph(ordinals[i]);
  end;
end;
//------------------------------------------------------------------------------

function TFontCache.IsValidFont: Boolean;
begin
  Result := assigned(fFontReader) and fFontReader.IsValidFontFormat;
end;
//------------------------------------------------------------------------------

function TFontCache.GetAscent: double;
begin
  if not IsValidFont then
    Result := 0
  else with fFontReader.FontInfo do
    Result := Max(ascent, yMax) * fScale;
end;
//------------------------------------------------------------------------------

function TFontCache.GetDescent: double;
begin
  if not IsValidFont then
    Result := 0
  else with fFontReader.FontInfo do
    Result := Max(descent, -yMin) * fScale;
end;
//------------------------------------------------------------------------------

function TFontCache.GetLineHeight: double;
begin
  if not IsValidFont then Result := 0
  else Result := Ascent + Descent;
end;
//------------------------------------------------------------------------------

function TFontCache.GetYyHeight: double;
var
  minY, maxY: double;
begin
  //nb: non-inverted Y coordinates.
  maxY := GetCharInfo(ord('Y')).metrics.glyf.yMax;
  minY := GetCharInfo(ord('y')).metrics.glyf.yMin;
  Result := (maxY - minY) * fScale;
end;
//------------------------------------------------------------------------------

procedure TFontCache.VerticalFlip(var paths: TPathsD);
var
  i,j: integer;
begin
  for i := 0 to High(paths) do
    for j := 0 to High(paths[i]) do
      with paths[i][j] do Y := -Y;
end;
//------------------------------------------------------------------------------

function FindInKernList(glyphIdx: integer; const kernList: TArrayOfTKern): integer;
var
  i,l,r: integer;
begin
  l := 0;
  r := High(kernList);
  while l <= r do
  begin
    Result := (l + r) shr 1;
    i := kernList[Result].rightGlyphIdx - glyphIdx;
    if i < 0 then
    begin
      l := Result +1
    end else
    begin
      if i = 0 then Exit; //found!
      r := Result -1;
    end;
  end;
  Result := -1;
end;
//------------------------------------------------------------------------------

function TFontCache.GetCharInfo(charOrdinal: WORD): PGlyphInfo;
var
  listIdx: integer;
begin
  Result := nil;
  if not fSorted then Sort;
  listIdx := FindInSortedList(charOrdinal, fGlyphInfoList);
  if listIdx < 0 then
  begin
    if not IsValidFont then Exit;
    Result := AddGlyph(Ord(charOrdinal));
  end else
    Result := PGlyphInfo(fGlyphInfoList[listIdx]);
end;
//------------------------------------------------------------------------------

function TFontCache.GetCharOffsets(const text: UnicodeString;
  interCharSpace: double): TArrayOfDouble;
var
  i,j, len: integer;
  ordinals: TArrayOfWord;
  glyphInfo: PGlyphInfo;
  thisX: double;
  prevGlyphKernList: TArrayOfTKern;
begin
  len := length(text);
  SetLength(ordinals, len);
  for i := 0 to len -1 do
    ordinals[i] := Ord(text[i+1]);
  SetLength(Result, len +1);
  Result[0] := 0;
  if len = 0 then Exit;
  GetMissingGlyphs(ordinals);

  thisX := 0;
  prevGlyphKernList := nil;
  for i := 0 to High(ordinals) do
  begin
    glyphInfo := GetCharInfo(ordinals[i]);
    if not assigned(glyphInfo) then Break;
    if fUseKerning and assigned(prevGlyphKernList) then
    begin
      j := FindInKernList(glyphInfo.metrics.glyphIdx, prevGlyphKernList);
      if (j >= 0) then
        thisX := thisX + prevGlyphKernList[j].kernValue*fScale;
    end;
    Result[i] := thisX;
    thisX := thisX + glyphInfo.metrics.hmtx.advanceWidth*fScale +interCharSpace;
    prevGlyphKernList := glyphInfo.metrics.kernList;
  end;
  Result[len] := thisX - interCharSpace;
end;
//------------------------------------------------------------------------------

function TFontCache.GetTextWidth(const text: UnicodeString): double;
var
  offsets: TArrayOfDouble;
begin
  Result := 0;
  if not IsValidFont then Exit;
  offsets := GetCharOffsets(text);
  Result := offsets[high(offsets)];
end;
//------------------------------------------------------------------------------

function TFontCache.GetSpaceWidth: double;
begin
  Result := GetCharInfo(32).metrics.hmtx.advanceWidth * fScale;
end;
//------------------------------------------------------------------------------

function TFontCache.GetTextOutline(x, y: double;
  const text: UnicodeString): TPathsD;
var
  dummy: double;
begin
  Result := GetTextOutline(x, y, text, dummy);
end;
//------------------------------------------------------------------------------

function TFontCache.GetTextOutline(const rec: TRect;
  wordList: TWordInfoList; tpm: TTextPageMetrics;
  textAlign: TTextAlign; startLine, endLine: integer): TPathsD;
var
  i,j, a,b: integer;
  x,y,lh, spcDx, lineWidth: double;
  pp: TPathsD;
  app: TArrayOfPathsD;
begin
  Result := nil;
  if not Assigned(wordList) or (wordList.Count = 0) then Exit;

  lh := GetLineHeight;
  y := rec.Top;

  if startLine < 0 then startLine := 0;
  if (endLine < 0) or (endLine >= tpm.lineCount) then
    endLine := tpm.lineCount -1;

  for i := startLine to endLine do
  begin
    a := tpm.wordListOffsets[i];
    b := tpm.wordListOffsets[i+1] -1;
    if textAlign = taJustify then
      spcDx := tpm.justifyDeltas[i] else
      spcDx := 0;
    lineWidth := tpm.lineWidths[i];

    //ingore trailing spaces
    while (b >= a) do
      with wordList.GetWord(b) do
        if aWord <= #32 then
          dec(b) else
          break;

    case textAlign of
      taRight   : x := rec.Left + (RectWidth(rec) - lineWidth);
      taCenter  : x := rec.Left + (RectWidth(rec) - lineWidth)/2;
      else        x := rec.Left;
    end;

    for j := a to b do
      with wordList.GetWord(j) do
        if aWord > #32 then
        begin
          app := OffsetPath(paths, x, y + Ascent);
          pp := MergePathsArray(app);
          AppendPath(Result, pp);
          x := x + width;
        end
        else
          x := x + width + spcDx;
    y := y + lh;
  end;
end;
//------------------------------------------------------------------------------

function TFontCache.GetTextOutline(const rec: TRect;
  const text: UnicodeString; textAlign: TTextAlign; textAlignV: TTextVAlign;
  underlineIdx: integer): TPathsD;
var
  y,dy    : double;
  wl      : TWordInfoList;
  tpm     : TTextPageMetrics;
begin
  Result := nil;

  wl := TWordInfoList.Create;
  try
    wl.SetText(text, Self, underlineIdx);
    tpm := GetPageMetrics(RectWidth(rec), wl);
    Result := GetTextOutline(rec, wl, tpm, textAlign, 0, -1);

    case textAlignV of
      tvaMiddle:
        begin
          y := GetLineHeight * tpm.lineCount;
          dy := (RectHeight(rec) -y) /2 -1;
        end;
      tvaBottom:
        begin
          y := GetLineHeight * tpm.lineCount;
          dy := (RectHeight(rec) -y);
        end;
      else
        Exit;
    end;
    Result := OffsetPath(Result, 0, dy);
  finally
    wl.Free;
  end;
end;
//------------------------------------------------------------------------------

function TFontCache.GetTextOutline(x, y: double; const text: UnicodeString;
  out nextX: double; underlineIdx: integer): TPathsD;
var
  i: integer;
  w, y2: double;
  p: TPathD;
  arrayOfGlyphs: TArrayOfPathsD;
begin
  Result := nil;
  if not GetTextOutlineInternal(x, y, text,
    arrayOfGlyphs, nextX, underlineIdx) then Exit;

  if fUnderlined then
  begin
    w := LineHeight * lineFrac;
    y2 := y + 1.5 *(1+w);
    p := Rectangle(x, y2, nextX, y2 + w);
    AppendPath(Result, p);
  end;

  for i := 0 to high(arrayOfGlyphs) do
    AppendPath(Result, arrayOfGlyphs[i]);

  if fStrikeOut then
  begin
    w := LineHeight * lineFrac;
    y := y - LineHeight/4;
    p := Rectangle(x, y , nextX, y + w);
    AppendPath(Result, p);
  end;
end;
//------------------------------------------------------------------------------

function TFontCache.GetVerticalTextOutline(x, y: double;
  const text: UnicodeString; interCharSpace: double): TPathsD;
var
  i, xxMax: integer;
  glyphInfo: PGlyphInfo;
  dx, dy: double;
begin
  Result := nil;
  if not IsValidFont then Exit;

  xxMax := 0;
  for i := 1 to Length(text) do
  begin
    glyphInfo := GetCharInfo(ord(text[i]));
    if not assigned(glyphInfo) then Exit;
    with glyphInfo.metrics.glyf do
      if xMax > xxMax then
         xxMax := xMax;
  end;

  for i := 1 to Length(text) do
  begin
    glyphInfo := GetCharInfo(ord(text[i]));
    with glyphInfo.metrics.glyf do
    begin
      dx :=  (xxMax - xMax) * 0.5 * scale;
      y := y + yMax  * scale; //yMax = char ascent
      dy := - yMin * scale;   //yMin = char descent
    end;
    AppendPath(Result, Img32.Vector.OffsetPath( glyphInfo.contours, x + dx, y));
    if text[i] = #32 then
      y := y + dy - interCharSpace else
      y := y + dy + interCharSpace;
  end;
end;
//------------------------------------------------------------------------------

function TFontCache.GetTextOutlineInternal(x, y: double;
  const text: UnicodeString; out glyphs: TArrayOfPathsD;
  out nextX: double; underlineIdx: integer): Boolean;
var
  i,j, len  : integer;
  dx,y2,w   : double;
  unicodes  : TArrayOfWord;
  glyphInfo : PGlyphInfo;
  p         : TPathD;
  currGlyph : TPathsD;
  prevGlyphKernList: TArrayOfTKern;
begin
  len := Length(text);
  unicodes := nil;
  setLength(unicodes, len);
  for i := 0 to len -1 do
    unicodes[i] := Ord(text[i +1]);
  Result := true;
  GetMissingGlyphs(unicodes);
  nextX := x;
  prevGlyphKernList := nil;
  dec(underlineIdx);//convert from 1 base to 0 base index
  for i := 0 to len -1 do
  begin
    glyphInfo := GetCharInfo(unicodes[i]);
    if not assigned(glyphInfo) then Break;
    if fUseKerning and assigned(prevGlyphKernList) then
    begin
      j := FindInKernList(glyphInfo.metrics.glyphIdx, prevGlyphKernList);
      if (j >= 0) then
        nextX := nextX + prevGlyphKernList[j].kernValue * fScale;
    end;

    currGlyph := OffsetPath(glyphInfo.contours, nextX, y);
    dx := glyphInfo.metrics.hmtx.advanceWidth * fScale;

    if i = underlineIdx then
    begin
      w := LineHeight * lineFrac;
      y2 := y + 1.5 * (1 + w);
      p := Rectangle(nextX, y2, nextX +dx, y2 + w);
      AppendPath(currGlyph, p);
    end;

    AppendPath(glyphs, currGlyph);
    nextX := nextX + dx;
    prevGlyphKernList := glyphInfo.metrics.kernList;
  end;
end;
//------------------------------------------------------------------------------

function TFontCache.GetAngledTextGlyphs(x, y: double;
  const text: UnicodeString; angleRadians: double;
  const rotatePt: TPointD; out nextPt: TPointD): TPathsD;
begin
  nextPt.Y := y;
  Result := GetTextOutline(x,y, text, nextPt.X);
  if not Assigned(Result) then Exit;
  Result := RotatePath(Result, rotatePt, angleRadians);
  RotatePoint(nextPt, PointD(x,y), angleRadians);
end;
//------------------------------------------------------------------------------

procedure TFontCache.SetFontReader(newFontReader: TFontReader);
begin
  if newFontReader = fFontReader then Exit;
  if Assigned(fFontReader) then
  begin
    fFontReader.DeleteRecipient(self as INotifyRecipient);
    Clear;
  end;
  fFontReader := newFontReader;
  if Assigned(fFontReader) then
    fFontReader.AddRecipient(self as INotifyRecipient);
  UpdateScale;
end;
//------------------------------------------------------------------------------

procedure TFontCache.UpdateScale;
begin
  if IsValidFont and (fFontHeight > 0) then
  begin
    fScale := fFontHeight / fFontReader.FontInfo.unitsPerEm;
    NotifyRecipients(inStateChange);
  end else
  begin
    fScale := 1;
    NotifyRecipients(inDestroy);
  end;
end;
//------------------------------------------------------------------------------

procedure TFontCache.SetFontHeight(newHeight: double);
begin
  if fFontHeight = newHeight then Exit;
  fFontHeight := abs(newHeight);
  Clear;
  UpdateScale;
end;
//------------------------------------------------------------------------------

procedure FlipVert(var paths: TPathsD);
var
  i,j: integer;
begin
  for i := 0 to High(paths) do
    for j := 0 to High(paths[i]) do
      paths[i][j].Y := -paths[i][j].Y;
end;
//------------------------------------------------------------------------------

procedure TFontCache.SetFlipVert(value: Boolean);
var
  i: integer;
  glyphInfo: PGlyphInfo;
begin
  if fFlipVert = value then Exit;
  for i := 0 to fGlyphInfoList.Count -1 do
  begin
     glyphInfo := PGlyphInfo(fGlyphInfoList[i]);
     FlipVert(glyphInfo.contours);
  end;
  fFlipVert := value;
end;
//------------------------------------------------------------------------------

function GlyphSorter(glyph1, glyph2: pointer): integer;
begin
  Result := PGlyphInfo(glyph1).unicode - PGlyphInfo(glyph2).unicode;
end;
//------------------------------------------------------------------------------

procedure TFontCache.Sort;
begin
{$IFDEF XPLAT_GENERICS}
  fGlyphInfoList.Sort(TComparer<PGlyphInfo>.Construct(
    function (const glyph1, glyph2: PGlyphInfo): integer
    begin
      Result := glyph1.unicode - glyph2.unicode;
    end));
{$ELSE}
  fGlyphInfoList.Sort(GlyphSorter);
{$ENDIF}
  fSorted := true;
end;
//------------------------------------------------------------------------------

function TFontCache.AddGlyph(unicode: Word): PGlyphInfo;
var
  dummy: integer;
const
  minLength = 0.25;
begin

  New(Result);
  Result.unicode := unicode;
  fFontReader.GetGlyphInfo(unicode,
    Result.contours, dummy, Result.metrics);
  fGlyphInfoList.Add(Result);

  if fFontHeight > 0 then
  begin
    Result.contours := ScalePath(Result.contours, fScale);
    //text rendering is about twice as fast when excess detail is removed
    Result.contours :=
      StripNearDuplicates(Result.contours, minLength, true);
  end;

  if fFlipVert then VerticalFlip(Result.contours);
  fSorted := false;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function AppendSlash(const foldername: string): string;
begin
  Result := foldername;
  if (Result = '') or (Result[Length(Result)] = '\') then Exit;
  Result := Result + '\';
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

procedure CheckFontHeight(var logFont: TLogFont);
const
  _96Div72 = 96/72;
begin
  if logFont.lfHeight > 0 then
    logFont.lfHeight := -Round(DpiAware(logFont.lfHeight * _96Div72));
end;
//------------------------------------------------------------------------------

function PointHeightToPixelHeight(pt: double): double;
const
  _96Div72 = 96/72;
begin
  Result := Abs(pt) * _96Div72;
end;
//------------------------------------------------------------------------------

function GetFontFolder: string;
var
  pidl: PItemIDList;
  path: array[0..MAX_PATH] of char;
begin
  SHGetSpecialFolderLocation(0, CSIDL_FONTS, pidl);
  SHGetPathFromIDList(pidl, path);
  CoTaskMemFree(pidl);
  result := path;
end;
//------------------------------------------------------------------------------

function GetInstalledTtfFilenames: TArrayOfString;
var
  cnt, buffLen: integer;
  fontFolder: string;
  sr: TSearchRec;
  res: integer;
begin
  cnt := 0; buffLen := 1024;
  SetLength(Result, buffLen);
  fontFolder := AppendSlash(GetFontFolder);
  res := FindFirst(fontFolder + '*.ttf', faAnyFile, sr);
  while res = 0 do
  begin
    if cnt = buffLen then
    begin
      inc(buffLen, 128);
      SetLength(Result, buffLen);
    end;
    Result[cnt] := fontFolder + sr.Name;
    inc(cnt);
    res := FindNext(sr);
  end;
  FindClose(sr);
  SetLength(Result, cnt);
end;
{$ENDIF}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function DrawText(image: TImage32; x, y: double; const text: UnicodeString;
  font: TFontCache; textColor: TColor32 = clBlack32;
    useClearType: Boolean = false;
    clearTypeBgColor: TColor32 = clWhite32): double;
var
  glyphs: TPathsD;
begin
  Result := 0;
  if (text = '') or not assigned(font) or not font.IsValidFont then Exit;
  glyphs := font.GetTextOutline(x,y, text, Result);
  if useClearType then
    DrawPolygon_ClearType(image, glyphs,
      frNonZero, textColor, clearTypeBgColor)
  else
    DrawPolygon(image, glyphs, frNonZero, textColor);
end;
//------------------------------------------------------------------------------

function DrawText(image: TImage32; x, y: double; const text: UnicodeString;
  font: TFontCache; renderer: TCustomRenderer): double;
var
  glyphs: TPathsD;
begin
  Result := 0;
  if (text = '') or not assigned(font) or
    not font.IsValidFont then Exit;
  glyphs := font.GetTextOutline(x,y, text, Result);
  DrawPolygon(image, glyphs, frNonZero, renderer);
end;
//------------------------------------------------------------------------------

procedure DrawText(image: TImage32; const rec: TRect; const text: UnicodeString;
  textAlign: TTextAlign; textAlignV: TTextVAlign; font: TFontCache;
  textColor: TColor32 = clBlack32; useClearType: Boolean = false;
  clearTypeBgColor: TColor32 = clWhite32);
var
  glyphs: TPathsD;
begin
  if (text = '') or not assigned(font) or
    not font.IsValidFont then Exit;
  glyphs := font.GetTextOutline(rec, text, textAlign, textAlignV);
  if useClearType then
    DrawPolygon_ClearType(image, glyphs, frNonZero, textColor, clearTypeBgColor)
  else
    DrawPolygon(image, glyphs, frNonZero, textColor);
end;
//------------------------------------------------------------------------------

function DrawAngledText(image: TImage32;
  x, y: double; angleRadians: double;
  const text: UnicodeString; font: TFontCache;
  textColor: TColor32 = clBlack32): TPointD;
var
  glyphs: TPathsD;
  rotatePt: TPointD;
begin
  rotatePt := PointD(x,y);
  if not assigned(font) or not font.IsValidFont then Exit;
  glyphs := font.GetAngledTextGlyphs(x, y,
    text, angleRadians, rotatePt, Result);
  DrawPolygon(image, glyphs, frNonZero, textColor);
end;
//------------------------------------------------------------------------------

function DrawVerticalText(image: TImage32; x, y, interCharSpace: double;
  const text: UnicodeString; font: TFontCache;
  textColor: TColor32 = clBlack32): double;
var
  i, xxMax: integer;
  glyphs: TPathsD;
  glyphInfo: PGlyphInfo;
  dx, dy, scale: double;
begin
  Result := y;
  if not assigned(font) or not font.IsValidFont then Exit;

  xxMax := 0;
  for i := 1 to Length(text) do
  begin
    glyphInfo := font.GetCharInfo(ord(text[i]));
    if not assigned(glyphInfo) then Exit;
    with glyphInfo.metrics.glyf do
      if xMax > xxMax then
         xxMax := xMax;
  end;

  scale := font.Scale;
  for i := 1 to Length(text) do
  begin
    glyphInfo := font.GetCharInfo(ord(text[i]));
    with glyphInfo.metrics.glyf do
    begin
      dx :=  (xxMax - xMax) * 0.5 * scale;
      y := y + yMax  * scale; //yMax = char ascent
      dy := - yMin * scale;   //yMin = char descent
    end;
    glyphs := Img32.Vector.OffsetPath( glyphInfo.contours, x + dx, y);
    DrawPolygon(image, glyphs, frNonZero, textColor);
    if text[i] = #32 then
      y := y + dy - interCharSpace else
      y := y + dy + interCharSpace;
  end;
  Result := y;
end;
//------------------------------------------------------------------------------

type
  TPathInfo = record
    pt     : TPointD;
    vector : TPointD;
    angle  : Double;
    dist   : double;
  end;
  TPathInfos = array of TPathInfo;

function GetTextOutlineOnPath(const text: UnicodeString;
  const path: TPathD; font: TFontCache; textAlign: TTextAlign;
  perpendicOffset: integer = 0; charSpacing: double = 0): TPathsD;
var
  dummy: integer;
begin
  Result := GetTextOutlineOnPath(text, path, font, textAlign,
    perpendicOffset, charSpacing, dummy);
end;
//------------------------------------------------------------------------------

function GetTextOutlineOnPath(const text: UnicodeString;
  const path: TPathD; font: TFontCache; textAlign: TTextAlign;
  perpendicOffset: integer; charSpacing: double;
  out charsThatFit: integer): TPathsD; overload;
var
  pathLen: integer;
  pathInfos: TPathInfos;

  function GetPathInfo(var startIdx: integer; offset: double): TPathInfo;
  begin
    while startIdx <= pathLen do
    begin
      if pathInfos[startIdx].dist > offset then break;
      inc(startIdx);
    end;
    Result := pathInfos[startIdx -1];
    if Result.angle >= 0 then Exit; //ie already initialized
    Result.angle  := GetAngle(path[startIdx-1], path[startIdx]);
    Result.vector := GetUnitVector(path[startIdx-1], path[startIdx]);
    Result.pt     := path[startIdx -1];
  end;

var
  i, pathInfoIdx: integer;
  textWidth, left, center, center2, scale, dist, dx: double;
  glyph: PGlyphInfo;
  offsets: TArrayOfDouble;
  pathInfo: TPathInfo;
  pt, rotatePt: TPointD;
  tmpPaths: TPathsD;
begin
  Result := nil;
  pathLen := Length(path);
  charsThatFit := Length(text);

  offsets := font.GetCharOffsets(text, charSpacing);
  textWidth := offsets[charsThatFit];

  setLength(pathInfos, pathLen +1);
  if (pathLen < 2) or (charsThatFit = 0) then Exit;

  dist := 0;
  pathInfos[0].angle := -1;
  pathInfos[0].dist := 0;
  for i:= 1 to pathLen -1 do
  begin
    pathInfos[i].angle := -1; //flag uninitialized.
    dist := dist + Distance(path[i-1], path[i]);
    pathInfos[i].dist := dist;
  end;

  //truncate text that doesn't fit ...
  if offsets[charsThatFit] -
    ((offsets[charsThatFit] - offsets[charsThatFit-1])*0.5) > dist then
  begin
    repeat
      dec(charsThatFit);
    until offsets[charsThatFit] <= dist;
    //break text word boundaries
    while (charsThatFit > 1) and (text[charsThatFit] <> #32) do
      dec(charsThatFit);
    if charsThatFit = 0 then charsThatFit := 1;
  end;

  case textAlign of
    taCenter: Left := (dist - textWidth) * 0.5;
    taRight : Left := dist - textWidth;
    else      Left := 0;
  end;

  scale := font.Scale;
  Result := nil;
  pathInfoIdx := 1;
  for i := 1 to charsThatFit do
  begin
    glyph :=  font.GetCharInfo(Ord(text[i]));
    with glyph.metrics do
      center := (glyf.xMax - glyf.xMin) * scale * 0.5;
    center2 := left + center;
    left := left + glyph.metrics.hmtx.advanceWidth * scale + charSpacing;
    pathInfo := GetPathInfo(pathInfoIdx, center2);
    rotatePt := PointD(center, -perpendicOffset);
    tmpPaths := RotatePath(glyph.contours, rotatePt, pathInfo.angle);
    dx := center2 - pathInfo.dist;
    pt.X := pathInfo.pt.X + pathInfo.vector.X * dx - rotatePt.X;
    pt.Y := pathInfo.pt.Y + pathInfo.vector.Y * dx - rotatePt.Y;

    tmpPaths := OffsetPath(tmpPaths, pt.X, pt.Y);
    AppendPath(Result, tmpPaths);
  end;
end;

//------------------------------------------------------------------------------
// TWordInfo
//------------------------------------------------------------------------------

constructor TWordInfo.Create(owner: TWordInfoList; idx: integer);
begin
  index := idx;
end;

//------------------------------------------------------------------------------
// TWordInfoList
//------------------------------------------------------------------------------

procedure TWordInfoList.SetText(const text: UnicodeString;
  font: TFontCache; underlineIdx: integer);
var
  len: integer;
  spaceW: double;
  p, p2, pEnd: PWideChar;
  s: UnicodeString;
begin
  if not Assigned(font) then Exit;

  BeginUpdate;
  try
    Clear;
    spaceW := font.GetSpaceWidth;
    p := PWideChar(text);
    pEnd := p;
    Inc(pEnd, Length(text));
    while p < pEnd do
    begin
      if (p^ <= #32) then
      begin
        if (p^ = #32) then AddSpace(spaceW)
        else if (p^ = #10) then AddNewline;

        inc(p);
        dec(underlineIdx);
      end else
      begin
        p2 := p;
        inc(p);
        while (p < pEnd) and (p^ > #32) do inc(p);
        len := p - p2;
        SetLength(s, len);
        Move(p2^, s[1], len * SizeOf(Char));
        AddWord(font, s, underlineIdx);
        dec(underlineIdx, len);
      end;
    end;
  finally
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.ApplyNewFont(font: TFontCache);
var
  i: integer;
  spaceW, dummy: double;
  wi: TWordInfo;
begin
  if not Assigned(font) then Exit;
  spaceW := font.GetSpaceWidth;
  BeginUpdate;
  try
    for i := 0 to Count -1 do
    begin
      wi := GetWord(i);
      if wi.aWord <= #32 then
      begin
        if wi.aWord = #32 then wi.width := spaceW
        else wi.width := 0;
      end else
      begin
        font.GetTextOutlineInternal(0,0, wi.aWord, wi.paths, dummy);
        wi.width := font.GetTextWidth(wi.aWord);
      end;
    end;
  finally
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

constructor TWordInfoList.Create;
begin
  inherited;
{$IFDEF XPLAT_GENERICS}
  fList := TList<TWordInfo>.Create;
{$ELSE}
  fList := TList.Create;
{$ENDIF}
end;
//------------------------------------------------------------------------------

destructor TWordInfoList.Destroy;
begin
  fOnChanged := nil;
  Clear;
  fList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TWordInfoList.GetWord(index: integer): TWordInfo;
begin
  if (index < 0) or (index >= fList.Count) then
    raise Exception.Create(rsWordListRangeError);
  Result :=  TWordInfo(fList.Items[index]);
end;
//------------------------------------------------------------------------------

function TWordInfoList.GetText: UnicodeString;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Count -1 do
    Result := Result + TWordInfo(fList.Items[i]).aWord;
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.AddNewline;
begin
  InsertNewline(MaxInt);
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.AddSpace(font: TFontCache);
begin
  InsertSpace(font, MaxInt);
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.AddSpace(spaceWidth: double);
begin
  InsertSpace(spaceWidth, MaxInt);
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.AddWord(font: TFontCache;
  const word: UnicodeString; underlineIdx: integer);
begin
  InsertWord(font, MaxInt, word, underlineIdx);
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.InsertNewline(index: integer);
var
  i, cnt: integer;
  newWord : TWordInfo;
begin
  cnt := fList.Count;
  if (index > cnt) then index := cnt
  else if (index < 0) then index := 0;

  newWord := TWordInfo.Create(self, index);
  newWord.aWord  := #10;
  newWord.width := 0;
  newWord.length := 1;
  newWord.paths := nil;
  fList.Insert(index, newWord);

  //reindex
  if index < cnt then
    for i := index +1 to cnt do
      TWordInfo(fList[i]).index := i;
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.InsertSpace(font: TFontCache; index: integer);
var
  width: double;
begin
  if not Assigned(font) or not font.IsValidFont then
    raise Exception.Create(rsWordListFontError);
  width := font.GetCharInfo(32).metrics.hmtx.advanceWidth * font.fScale;
  InsertSpace(width, index);
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.InsertSpace(spaceWidth: double; index: integer);
var
  i, cnt: integer;
  newWord : TWordInfo;
begin
  cnt := fList.Count;
  if (index > cnt) then index := cnt
  else if (index < 0) then index := 0;

  newWord := TWordInfo.Create(self, index);
  newWord.aWord  := #32;
  newWord.width := spaceWidth;
  newWord.length := 1;
  newWord.paths := nil;
  fList.Insert(index, newWord);

  //reindex
  if index < cnt then
    for i := index +1 to cnt do
      TWordInfo(fList[i]).index := i;
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.InsertWord(font: TFontCache;
  index: integer; const word: UnicodeString; underlineIdx: integer);
var
  i, cnt: integer;
  width: double;
  newWord : TWordInfo;
  ap: TArrayOfPathsD;
begin
  if not Assigned(font) or not font.IsValidFont then
    raise Exception.Create(rsWordListFontError);

  font.GetTextOutlineInternal(0,0, word, ap, width, underlineIdx);
  cnt := fList.Count;
  if (index > cnt) then index := cnt
  else if (index < 0) then index := 0;

  newWord := TWordInfo.Create(self, index);
  newWord.aWord  := word;
  newWord.width := width;
  newWord.length := Length(word);
  newWord.paths := ap;
  fList.Insert(index, newWord);

  //reindex
  if index < cnt then
    for i := index +1 to cnt do
      TWordInfo(fList[i]).index := i;
end;
//------------------------------------------------------------------------------

function TWordInfoList.Count: integer;
begin
  Result := fList.Count;
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.Clear;
var
  i: integer;
begin
  for i := 0 to fList.Count -1 do
      TWordInfo(fList.Items[i]).Free;
  fList.Clear;
  if Assigned(fOnChanged) then fOnChanged(Self);
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.BeginUpdate;
begin
  inc(fUpdateCount);
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.EndUpdate;
begin
  dec(fUpdateCount);
  if (fUpdateCount = 0) then Changed;
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.Changed;
begin
  if Assigned(fOnChanged) then fOnChanged(Self);
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.Delete(Index: Integer);
begin
  if (index < 0) or (index >= fList.Count) then
    raise Exception.Create(rsWordListRangeError);
  TWordInfo(fList.Items[index]).Free;
  fList.Delete(index);
  if Assigned(fOnChanged) then fOnChanged(Self);
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.DeleteRange(startIdx, endIdx: Integer);
var
  i, cnt, cnt2: Integer;
begin
  if (startIdx < 0) or (endIdx >= fList.Count) then
    raise Exception.Create(rsWordListRangeError);
  for i := startIdx to endIdx do
    TWordInfo(fList.Items[i]).Free;

  //fList.DeleteRange(startIdx, endIdx - startIdx +1);
  cnt := endIdx - startIdx +1;
  cnt2 := fList.Count - cnt;
  for i := startIdx to cnt2 -1 do
    fList[i] := fList[i +cnt];
  fList.Count := cnt2;

  if Assigned(fOnChanged) then fOnChanged(Self);
end;
//------------------------------------------------------------------------------

procedure TWordInfoList.Edit(font: TFontCache;
  index: Integer; const newWord: string);
var
  len: integer;
  dummy: double;
begin
  if (index < 0) or (index >= fList.Count) then
    raise Exception.Create(rsWordListRangeError);
  len := system.Length(newWord);
  if len = 0 then
    Delete(index)
  else if Assigned(font) then
    with TWordInfo(fList.Items[index]) do
    begin
      aWord := newWord;
      length := 1;
      while (length < len) and (aWord[length+1] > #32) do
        inc(length);
      if length < len then SetLength(aWord, length);
      width := font.GetTextWidth(aWord);
      font.GetTextOutlineInternal(0,0,aWord, paths, dummy);
      if Assigned(fOnChanged) then fOnChanged(Self);
    end;
end;
//------------------------------------------------------------------------------

function GetPageMetrics(lineWidth: double; wordList: TWordInfoList): TTextPageMetrics;
var
  arrayCnt, arrayCap: integer;

  procedure CalcLineWidthsAndJustify(idx: integer);
  var
    i,j,k, spcCnt: integer;
    x: double;
    forceLeftAlign: Boolean;
  begin
    j := Result.wordListOffsets[idx] -1;
    if j < 0 then Exit;

    forceLeftAlign := wordList.GetWord(j).aWord = #10;
    i := Result.wordListOffsets[idx -1];

    while (j > i) and (wordList.GetWord(j).aWord = #32) do
      dec(j);

    spcCnt := 0;
    x := 0;
    for k := i to j do
      with wordList.GetWord(k) do
      begin
        if aWord = #32 then inc(spcCnt);
        x := x + width;
      end;
    Result.lineWidths[idx-1] := x;
    if not forceLeftAlign and (spcCnt > 0) then
      Result.justifyDeltas[idx-1] := (Result.maxLineWidth - x)/spcCnt;
  end;

  procedure AddLine(i: integer);
  begin
    if arrayCnt = arrayCap then
    begin
      inc(arrayCap, 16);
      SetLength(Result.wordListOffsets, arrayCap);
      SetLength(Result.justifyDeltas, arrayCap);
      SetLength(Result.lineWidths, arrayCap);
    end;
    inc(Result.lineCount);
    Result.wordListOffsets[arrayCnt] := i;
    Result.justifyDeltas[arrayCnt] := 0.0;
    if (arrayCnt > 0) then
      CalcLineWidthsAndJustify(arrayCnt);
    inc(arrayCnt);
  end;

var
  i,j, cnt: integer;
  x: double;
  wi: TWordInfo;
begin
  Result.lineCount := 0;
  Result.maxLineWidth := lineWidth;
  Result.wordListOffsets := nil;
  arrayCnt := 0; arrayCap := 0;
  if not Assigned(wordList) or (wordList.Count = 0) then Exit;

  i := 0; j := 0;
  cnt := wordList.Count;
  x := 0;

  while (i < cnt) do
  begin
    wi := wordList.GetWord(i);
    if (i = j) and (wi.aWord = #32) then
    begin
      inc(i); inc(j); Continue;
    end;

    if (wi.aWord = #10) then
    begin
      AddLine(j);
      inc(i); j := i; x := 0;
    end
    else if (x + wi.width > lineWidth) then
    begin
      if j = i then Break; //word is too long for line. Todo: ??hiphenate
      AddLine(j);
      j := i; x := 0;
    end else
    begin
      x := x + wi.width;
      inc(i);
    end;
  end;

  if (j < cnt)then AddLine(j); //add end short line
  AddLine(cnt);
  dec(Result.lineCount);
  SetLength(Result.wordListOffsets, arrayCnt);
  SetLength(Result.justifyDeltas, arrayCnt);
  SetLength(Result.lineWidths, arrayCnt);
  //make sure the 'real' last line isn't justified.
  Result.justifyDeltas[arrayCnt-2] := 0;
  //nb: the 'lineWidths' for the very last line may be longer
  //than maxLineWidth when a word's width exceeds 'maxLineWidth
end;

//------------------------------------------------------------------------------
// TFontManager
//------------------------------------------------------------------------------

constructor TFontManager.Create;
begin
  fMaxFonts := 20;
{$IFDEF XPLAT_GENERICS}
    fFontList := TList<TFontReader>.Create;
{$ELSE}
    fFontList:= TList.Create;
{$ENDIF}
end;
//------------------------------------------------------------------------------

destructor TFontManager.Destroy;
begin
  Clear;
  fFontList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TFontManager.Clear;
var
  i: integer;
begin
  for i := 0 to fFontList.Count -1 do
    with TFontReader(fFontList[i]) do
    begin
      fFontManager := nil;
      Free;
    end;
  fFontList.Clear;
end;
//------------------------------------------------------------------------------

function TFontManager.GetFont(const fontName: string): TFontReader;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to fFontList.Count -1 do
    if SameText(TFontReader(fFontList[i]).fFontInfo.faceName, fontName) then
    begin
      Result := fFontList[i];
      Exit;
    end;
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function TFontManager.Load(const fontName: string): TFontReader;
begin
  if fFontList.Count >= fMaxFonts then
    raise Exception.Create(rsTooManyFonts);

  Result := GetFont(fontname);
  if Assigned(Result) then Exit;

  Result := TFontReader.Create;
  try
    if not Result.Load(fontName) or
      not ValidateAdd(Result) then
        FreeAndNil(Result);
  except
    FreeAndNil(Result);
  end;
  if Assigned(Result) then
    Result.fFontManager := self;
end;
//------------------------------------------------------------------------------
{$ENDIF}

function TFontManager.LoadFromStream(stream: TStream): TFontReader;
begin
  if fFontList.Count >= fMaxFonts then
    raise Exception.Create(rsTooManyFonts);

  Result := TFontReader.Create;
  try
    if not Result.LoadFromStream(stream) or
      not ValidateAdd(Result) then
        FreeAndNil(Result);
  except
    FreeAndNil(Result);
  end;
  if Assigned(Result) then
    Result.fFontManager := self;
end;
//------------------------------------------------------------------------------

function TFontManager.LoadFromResource(const resName: string; resType: PChar): TFontReader;
begin
  if fFontList.Count >= fMaxFonts then
    raise Exception.Create(rsTooManyFonts);

  Result := TFontReader.Create;
  try
    if not Result.LoadFromResource(resName, resType) or
      not ValidateAdd(Result) then
        FreeAndNil(Result);
  except
    FreeAndNil(Result);
  end;
  if Assigned(Result) then
    Result.fFontManager := self;
end;
//------------------------------------------------------------------------------

function TFontManager.LoadFromFile(const filename: string): TFontReader;
begin
  if fFontList.Count >= fMaxFonts then
    raise Exception.Create(rsTooManyFonts);

  Result := TFontReader.Create;
  try
    if not Result.LoadFromFile(filename) or
      not ValidateAdd(Result) then
        FreeAndNil(Result);
  except
    FreeAndNil(Result);
  end;
  if Assigned(Result) then
    Result.fFontManager := self;
end;
//------------------------------------------------------------------------------

function TFontManager.ValidateAdd(fr: TFontReader): Boolean;
var
  fr2: TFontReader;
begin
  Result := Assigned(fr);
  if not Result then Exit;
  //avoid adding duplicates
  fr2 := GetBestMatchFont(fr.fFontInfo);
  if not Assigned(fr2) or
    ((fr.fFontInfo.macStyles <> fr2.fFontInfo.macStyles) or
    not SameText(fr.fFontInfo.faceName, fr2.fFontInfo.faceName)) then
    fFontList.Add(fr)
  else
      Result := false;
end;
//------------------------------------------------------------------------------

function TFontManager.Delete(fontReader: TFontReader): Boolean;
var
  i: integer;
begin
  for i := 0 to fFontList.Count -1 do
    if TFontReader(fFontList[i]) = fontReader then
    begin
      //make sure the FontReader object isn't destroying itself externally
      if not fontReader.fDestroying then fontReader.Free;
      fFontList.Delete(i);
      Result := true;
      Exit;
    end;
  Result := false;
end;
//------------------------------------------------------------------------------

function TFontManager.GetBestMatchFont(const fontInfo: TFontInfo): TFontReader;

  function StylesToInt(macstyles: TMacStyles): integer;
  begin
    if msBold in macStyles then
      Result := 1 else Result := 0;
    if msItalic in macStyles then inc(Result, 2);
  end;

  function FamilyToInt(fontFamily: TTtfFontFamily): integer;
  begin
    Result := Ord(fontFamily);
  end;

  function NameDiff(const name1, name2: string): integer;
  begin
    if SameText(name1, name2) then Result := 0 else Result := 1;
  end;

  function CompareFontInfos(const fi1, fi2: TFontInfo): integer;
  var
    styleDiff: integer;
  begin
    styleDiff := Abs(StylesToInt(fi1.macStyles) - StylesToInt(fi2.macStyles));
    if styleDiff = 2 then Dec(styleDiff);
    Result := styleDiff shl 8 +
      Abs(FamilyToInt(fi1.fontFamily) - FamilyToInt(fi2.fontFamily)) shl 4 +
      NameDiff(fi1.faceName, fi2.faceName);
  end;

var
  i, bestIdx: integer;
  bestDiff, currDiff: integer;
begin
  Result := nil;
  if fFontList.Count = 0 then Exit;

  bestIdx := 0;
  bestDiff := CompareFontInfos(fontInfo,
    TFontReader(fFontList[0]).fFontInfo);

  for i := 1 to fFontList.Count -1 do
  begin
    currDiff := CompareFontInfos(fontInfo,
      TFontReader(fFontList[i]).fFontInfo);
    if (currDiff < bestDiff) then
    begin
      bestIdx := i;
      bestDiff := currDiff;
      if bestDiff = 0 then Break;
    end;
  end;
  Result := TFontReader(fFontList[bestIdx]);
end;
//------------------------------------------------------------------------------

procedure TFontManager.SetMaxFonts(value: integer);
begin
  if value < 0 then value := 0;
  if value <= 0 then Clear
  else while value > fFontList.Count do
    Delete(TFontReader(fFontList[0]));
  fMaxFonts := value;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function FontManager: TFontManager;
begin
  result := aFontManager;
end;
//------------------------------------------------------------------------------

initialization
  aFontManager := TFontManager.Create;

finalization
  aFontManager.Free;

end.
