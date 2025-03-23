unit Img32.Text;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.8                                                             *
* Date      :  22 January 2025                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
* Purpose   :  TrueType fonts for TImage32 (without Windows dependencies)      *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  {$IFDEF MSWINDOWS} Windows, ShlObj, ActiveX, {$ENDIF}
  Types, SysUtils, Classes, Math,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Img32, Img32.Draw, Img32.Vector;

type

  TFixed = type single;
  Int16 = type SmallInt;
  TFontFormat = (ffInvalid, ffTrueType, ffCompact);
  TFontFamily = (tfUnknown, tfSerif, tfSansSerif, tfMonospace);

  TFontReader = class;

{$IFDEF MSWINDOWS}
  PArrayOfEnumLogFontEx = ^TArrayOfEnumLogFontEx;
  TArrayOfEnumLogFontEx = array of TEnumLogFontEx;

  // TFontReaderFamily - a custom (Image32) record
  TFontReaderFamily = record
    regularFR     : TFontReader;
    boldFR        : TFontReader;
    italicFR      : TFontReader;
    boldItalicFR  : TFontReader;
  end;
{$ENDIF}

  {$IFNDEF Unicode}
  UnicodeString = WideString;
  {$ENDIF}

  TMacStyle = (msBold, msItalic, msUnderline, msOutline,
    msShadow, msCondensed, msExtended);
  TMacStyles = set of TMacStyle;

  TTextAlign = (taLeft, taRight, taCenter, taJustify);
  TTextVAlign = (tvaTop, tvaMiddle, tvaBottom);

  // nb: Avoid "packed" records as these cause problems with Android

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
    platformID    : WORD;           // Unicode = 0; Windows = 3 (obsolete);
    encodingID    : WORD;
    offset        : Cardinal;
  end;

  TCmapFormat0 = record
    format        : WORD;           // 0
    length        : WORD;
    language      : WORD;
  end;

  TCmapFormat4 = record
    format        : WORD;           // 4
    length        : WORD;
    language      : WORD;
    segCountX2    : WORD;
    searchRange   : WORD;
    entrySelector : WORD;
    rangeShift    : WORD;
    //endCodes    : array of WORD;  // last = $FFFF
    //reserved    : WORD;           // 0
    //startCodes  : array of WORD;
  end;

  TFormat4Rec = record
    startCode    : WORD;
    endCode      : WORD;
    idDelta      : WORD;
    rangeOffset  : WORD;
  end;

  TCmapFormat6 = record
    format        : WORD;           // 6
    length        : WORD;
    language      : WORD;
    firstCode     : WORD;
    entryCount    : WORD;
  end;

  TCmapFormat12 = record
    format        : WORD;           // 12
    reserved      : WORD;           // 0
    length        : DWORD;
    language      : DWORD;
    nGroups       : DWORD;
    //array[nGroups] of TFormat12Group;
  end;

  TFormat12Rec = record
    startCode     : WORD;
    endCode       : WORD;
    idDelta       : WORD;
    rangeOffset   : WORD;
  end;

  TFormat12Group = record
    startCharCode : DWORD;
    endCharCode   : DWORD;
    startGlyphCode: DWORD;
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
    majorVersion   : WORD;
    minorVersion   : WORD;
    fontRevision   : TFixed;
    checkSumAdjust : Cardinal;
    magicNumber    : Cardinal;      // $5F0F3CF5
    flags          : WORD;
    unitsPerEm     : WORD;
    dateCreated    : UInt64;
    dateModified   : UInt64;
    xMin           : Int16;
    yMin           : Int16;
    xMax           : Int16;
    yMax           : Int16;
    macStyle       : WORD;          // see TMacStyles
    lowestRecPPEM  : WORD;
    fontDirHint    : Int16;         // left to right, right to left
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
    majorVersion   : WORD;
    minorVersion   : WORD;
    italicAngle    : TFixed;
    underlinePos   : Int16;
    underlineWidth : Int16;
    isFixedPitch   : Cardinal;
    //minMemType42   : Cardinal;
    //maxMemType42   : Cardinal;
    //minMemType1   : Cardinal;
    //maxMemType1   : Cardinal;
  end;

  ArrayOfUtf8String = array of Utf8String;

  // TFontInfo: a custom summary record
  TFontInfo = record
    fontFormat     : TFontFormat;
    family         : TFontFamily;
    familyNames    : ArrayOfUtf8String;
    faceName       : Utf8String;
    fullFaceName   : Utf8String;
    style          : Utf8String;
    copyright      : Utf8String;
    manufacturer   : Utf8String;
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

  ///////////////////////////////////////////
  // the following point structures are only
  // used internally by the TFontReader class
  TPointEx = record
    pt: TPointD;
    flag: byte;
  end;
  TPathEx = array of TPointEx;
  TPathsEx = array of TPathEx;
  ///////////////////////////////////////////

  PGlyphInfo = ^TGlyphInfo;
  // TGlyphInfo: another custom record
  TGlyphInfo = record
    codepoint  : integer;
    glyphIdx   : WORD;
    unitsPerEm : integer;
    glyf       : TFontTable_Glyf;
    hmtx       : TFontTable_Hmtx;
    kernList   : TArrayOfTKern;
    paths      : TPathsD;
  end;

  TFontTableArray = array of TFontTable;
  TArrayOfWord = array of WORD;
  TArrayOfCardinal = array of Cardinal;
  TArrayOfCmapTblRec = array of TCmapTblRec;

  TTableName = (tblName, tblHead, tblHhea,
    tblCmap, tblMaxp, tblLoca, tblGlyf,
    tblHmtx, tblKern, tblPost);

{$IFDEF ZEROBASEDSTR}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

  TLoadFontResult = (lfrSuccess, lfrDuplicate, lfrInvalid);

  TFontManager = class
  private
    fMaxFonts: integer;
{$IFDEF XPLAT_GENERICS}
    fFontList: TList<TFontReader>;
{$ELSE}
    fFontList: TList;
{$ENDIF}
    procedure SetMaxFonts(value: integer);
    procedure SortFontListOnLastUse;
    procedure DeleteOldestFont;
    function ValidateFontLoad(var fr: TFontReader): TLoadFontResult;
    function FindDuplicate(fr: TFontReader): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
{$IFDEF MSWINDOWS}
    // LoadFontReaderFamily: call will fail if the fonts have already been
    // loaded, or if the font family hasn't been installed in the PC.
    function LoadFontReaderFamily(const fontFamily: string): TLoadFontResult; overload;
    function LoadFontReaderFamily(const fontFamily: string;
      out fontReaderFamily: TFontReaderFamily): TLoadFontResult; overload;
    function LoadFontReader(const fontName: string): TFontReader;
{$ENDIF}
    function LoadFromStream(stream: TStream): TFontReader;
    function LoadFromResource(const resName: string; resType: PChar): TFontReader;
    function LoadFromFile(const filename: string): TFontReader;
    function GetBestMatchFont(const fontInfo: TFontInfo): TFontReader; overload;
    function GetBestMatchFont(const styles: TMacStyles): TFontReader; overload;
    // FindReaderContainingGlyph: returns a TFontReader object containing the
    // specified glyph, otherwise nil. If a fontfamily is spedified, then the
    // search is limited to within that font family. If a TFontReader is found
    // then the out 'glyphIdx' parameter contains the index to the glyph
    // matching the supplied codepoint.
    function FindReaderContainingGlyph(codepoint: Cardinal;
      fntFamily: TFontFamily; out glyphIdx: WORD): TFontReader;
    function Delete(fontReader: TFontReader): Boolean;
    property MaxFonts: integer read fMaxFonts write SetMaxFonts;
  end;

  TFontReader = class(TInterfacedObj, INotifySender)
  private
    fFontManager       : TFontManager;
    fDestroying        : Boolean;
    fUpdateCount       : integer;
    fRecipientList     : TRecipients;
    fLastUsedTime      : TDateTime;
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
    fTbl_post          : TFontTable_Post;
    fTbl_loca2         : TArrayOfWord;
    fTbl_loca4         : TArrayOfCardinal;
    fKernTable         : TArrayOfKernRecs;

    fFormat0CodeMap    : array of byte;
    fFormat4CodeMap    : array of TFormat4Rec;
    fFormat12CodeMap   : array of TFormat12Group;
    fFormat4Offset     : integer;

    function GetTables: Boolean;
    function GetTable_name: Boolean;
    function GetTable_cmap: Boolean;
    function GetTable_maxp: Boolean;
    function GetTable_head: Boolean;
    function GetTable_loca: Boolean;
    function IsValidFontTable(const tbl : TFontTable): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
    function GetTable_hhea: Boolean;
    procedure GetTable_kern;
    procedure GetTable_post;

    procedure GetFontFamily;
    function GetGlyphPaths(glyphIdx: WORD;
      var tbl_hmtx: TFontTable_Hmtx; out tbl_glyf: TFontTable_Glyf): TPathsEx;
    function GetGlyphIdxUsingCmap(codePoint: Cardinal): WORD;
    function GetSimpleGlyph(tbl_glyf: TFontTable_Glyf): TPathsEx;
    function GetCompositeGlyph(var tbl_glyf: TFontTable_Glyf;
      var tbl_hmtx: TFontTable_Hmtx): TPathsEx;
    function ConvertSplinesToBeziers(const pathsEx: TPathsEx): TPathsEx;
    procedure GetPathCoords(var paths: TPathsEx);
    function GetGlyphHorzMetrics(glyphIdx: WORD): TFontTable_Hmtx;
    function GetFontInfo: TFontInfo;
    function GetGlyphKernList(glyphIdx: WORD): TArrayOfTKern;
    function GetGlyphInfoInternal(glyphIdx: WORD): TGlyphInfo;
    function GetWeight: integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NotifyRecipients(notifyFlag: TImg32Notification);
  protected
    property LastUsedTime: TDatetime read fLastUsedTime write fLastUsedTime;
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
    function HasGlyph(codepoint: Cardinal): Boolean;
    function LoadFromStream(stream: TStream): Boolean;
    function LoadFromResource(const resName: string; resType: PChar): Boolean;
    function LoadFromFile(const filename: string): Boolean;
{$IFDEF MSWINDOWS}
    function Load(const FontName: string): Boolean; overload;
    function Load(const logFont: TLogFont): Boolean; overload;
    function LoadUsingFontHdl(hdl: HFont): Boolean;
{$ENDIF}
    function GetGlyphInfo(codepoint: Cardinal;
      out nextX: integer; out glyphInfo: TGlyphInfo): Boolean;
    property FontFamily: TFontFamily read fFontInfo.family;
    property FontInfo: TFontInfo read GetFontInfo;
    property Weight: integer read GetWeight; // range 100-900
  end;

  TPageTextMetrics = record
    bounds          : TRect;
    lineCount       : integer;
    lineHeight      : double;
    topLinePxOffset : integer;
    nextChuckIdx    : integer;
    startOfLineIdx  : TArrayOfInteger;
    justifyDeltas   : TArrayOfDouble;
    lineWidths      : TArrayOfDouble;
  end;

  TFontCache = class;
  TChunkedText = class;

  TTextChunk = class
  public
    owner         : TChunkedText;
    index         : integer;
    text          : UnicodeString;
    left          : double;
    top           : double;
    width         : double;
    height        : double;
    backColor     : TColor32;
    fontColor     : TColor32;
    ascent        : double;
    userData      : Pointer;
    glyphOffsets  : TArrayOfDouble;
    arrayOfPaths  : TArrayOfPathsD;
    constructor Create(owner: TChunkedText; const chunk: UnicodeString;
      index: integer; fontCache: TFontCache; fontColor: TColor32;
      backColor: TColor32 = clNone32);
  end;

  TDrawChunkEvent = procedure(chunk: TTextChunk; const chunkRec: TRectD) of object;

  // TChunkedText: A font formatted list of text 'chunks' (usually space
  // seperated words) that will greatly speed up displaying word-wrapped text.
  TChunkedText = class
  private
    fSpaceWidth   : double;
    fLastFont    : TFontCache;
{$IFDEF XPLAT_GENERICS}
    fList         : TList<TTextChunk>;
{$ELSE}
    fList         : TList;
{$ENDIF}
    fDrawChunkEvent: TDrawChunkEvent;
    function  GetChunk(index: integer): TTextChunk;
    function GetText: UnicodeString;
    function  GetCount: integer;
  protected
    function GetGlyphsOrDrawInternal(image: TImage32; const rec: TRect;
      textAlign: TTextAlign; textAlignV: TTextVAlign; startChunk: integer;
      lineHeight: double; out paths: TPathsD): TPageTextMetrics;
  public
    constructor Create; overload;
    constructor Create(const text: string; font: TFontCache;
      fontColor: TColor32 = clBlack32; backColor: TColor32 = clNone32); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteChunk(Index: Integer);
    procedure DeleteChunkRange(startIdx, endIdx: Integer);
    procedure AddNewline(font: TFontCache);
    procedure AddSpace(font: TFontCache); overload;
    function  GetPageMetrics(const rec: TRect; lineHeight: double;
      startingChunkIdx: integer): TPageTextMetrics;
    function  GetChunkAndGlyphOffsetAtPt(const ptm: TPageTextMetrics;
      const pt: TPoint; out glyphIdx, chunkChrOff: integer): Boolean;
    function InsertTextChunk(font: TFontCache; index: integer;
      const chunk: UnicodeString; fontColor: TColor32 = clBlack32;
      backColor: TColor32 = clNone32): TTextChunk;
    function AddTextChunk(font: TFontCache; const chunk: UnicodeString;
      fontColor: TColor32 = clBlack32;
      backColor: TColor32 = clNone32): TTextChunk;
    procedure SetText(const text: UnicodeString; font: TFontCache;
      fontColor: TColor32 = clBlack32; backColor: TColor32 = clNone32);
    // DrawText: see Examples/FMX2, Examples/Text & Examples/Experimental apps.
    function DrawText(image: TImage32; const rec: TRect;
      textAlign: TTextAlign; textAlignV: TTextVAlign;
      startChunk: integer; lineHeight: double = 0.0): TPageTextMetrics;
    function GetTextGlyphs(const rec: TRect;
      textAlign: TTextAlign; textAlignV: TTextVAlign; startChunk: integer;
      lineHeight: double = 0.0): TPathsD;
    procedure ApplyNewFont(font: TFontCache);
    property Count: integer read GetCount;
    property Chunk[index: integer]: TTextChunk read GetChunk; default;
    property Text: UnicodeString read GetText;
    property OnDrawChunk: TDrawChunkEvent
      read fDrawChunkEvent write fDrawChunkEvent;
  end;

  // TFontCache: speeds up text rendering by parsing font files only once
  // for each accessed character. It can also scale glyphs to a specified
  // font height and invert glyphs too (which is necessary on Windows PCs).
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
    function FoundInList(charOrdinal: Cardinal): Boolean;
    function AddGlyph(codepoint: Cardinal): PGlyphInfo;
    procedure VerticalFlip(var paths: TPathsD);
    procedure SetFlipVert(value: Boolean);
    procedure SetFontHeight(newHeight: double);
    procedure SetFontReader(newFontReader: TFontReader);
    procedure UpdateScale;
    procedure Sort;
    procedure GetMissingGlyphs(const ordinals: TArrayOfCardinal);
    function IsValidFont: Boolean;
    function GetAscent: double;
    function GetDescent: double;
    function GetGap: double;
    function GetLineHeight: double;
    function GetYyHeight: double;
    function GetTextOutlineInternal(x, y: double; const text: UnicodeString;
      underlineIdx: integer; out glyphs: TArrayOfPathsD;
      out offsets: TArrayOfDouble; out nextX: double): Boolean; overload;
    procedure UpdateFontReaderLastUsedTime;
  public
    constructor Create(fontReader: TFontReader = nil; fontHeight: double = 10); overload;
    destructor Destroy; override;
    procedure Clear;
    // TFontCache is both an INotifySender and an INotifyRecipient.
    // It receives notifications from a TFontReader object and it sends
    // notificiations to any number of TFontCache object users
    procedure ReceiveNotification(Sender: TObject; notify: TImg32Notification);
    procedure AddRecipient(recipient: INotifyRecipient);
    procedure DeleteRecipient(recipient: INotifyRecipient);
    function GetGlyphInfo(codepoint: Cardinal): PGlyphInfo;

    function GetTextOutline(x, y: double; const text: UnicodeString): TPathsD; overload;
    function GetTextOutline(const rec: TRectD; const text: UnicodeString;
      ta: TTextAlign; tav: TTextVAlign; underlineIdx: integer = 0): TPathsD; overload;
    function GetTextOutline(x, y: double; const text: UnicodeString;
      out nextX: double; underlineIdx: integer = 0): TPathsD; overload;

    // GetUnderlineOutline - another way to underline text. 'y' indicates the
    // text baseline, and 'dy' is the offset from that baseline.
    // if dy = InvalidD then the default offset is used (& based on linewidth).
    function GetUnderlineOutline(leftX, rightX, y: double; dy: double = invalidD;
      wavy: Boolean = false; strokeWidth: double = 0): TPathD;

    function GetVerticalTextOutline(x, y: double;
      const text: UnicodeString; lineHeight: double = 0.0): TPathsD;

    function GetAngledTextGlyphs(x, y: double; const text: UnicodeString;
      angleRadians: double; const rotatePt: TPointD;
      out nextPt: TPointD): TPathsD;
    // GetGlyphOffsets - there isn't always a one-to-one relationship between
    // text characters and glyphs since text can on occasions contain
    // "surrogate paired" characters (eg emoji characters).
    function GetGlyphOffsets(const text: UnicodeString;
      interCharSpace: double = 0): TArrayOfDouble;
    // As per the comment above, there isn't always a one-to-one relationship
    // between text characters and their codepoints (2 byte chars vs 4 bytes)
    function GetTextCodePoints(const text: UnicodeString): TArrayOfCardinal;
    function GetTextWidth(const text: UnicodeString): double;
    function CountCharsThatFit(const text: UnicodeString; maxWidth: double): integer;
    function GetSpaceWidth: double;

    property Ascent     : double read GetAscent;
    property Descent    : double read GetDescent;
    property LineGap    : double read GetGap;
    property FontHeight : double read fFontHeight write SetFontHeight;
    property FontReader : TFontReader read fFontReader write SetFontReader;
    property InvertY    : boolean read fFlipVert write SetFlipVert;
    property Kerning    : boolean read fUseKerning write fUseKerning;
    property LineHeight : double read GetLineHeight;
    property YyHeight   : double read GetYyHeight;
    property Scale      : double read fScale;
    property Underlined : Boolean read fUnderlined write fUnderlined;
    property StrikeOut  : Boolean read fStrikeOut write fStrikeOut;
  end;

  function DrawText(image: TImage32; x, y: double;
    const text: UnicodeString; font: TFontCache;
    textColor: TColor32 = clBlack32): double; overload;

  procedure DrawText(image: TImage32; const rec: TRectD;
    const text: UnicodeString; font: TFontCache;
    textColor: TColor32 = clBlack32; align: TTextAlign = taCenter;
    valign: TTextVAlign = tvaMiddle); overload;

  function DrawText(image: TImage32; x, y: double;
    const text: UnicodeString; font: TFontCache;
    renderer: TCustomRenderer): double; overload;

  function DrawAngledText(image: TImage32;
  x, y: double; angleRadians: double;
  const text: UnicodeString; font: TFontCache;
  textColor: TColor32 = clBlack32): TPointD;

  procedure DrawVerticalText(image: TImage32;
    x, y: double; const text: UnicodeString; font: TFontCache;
    lineHeight: double = 0.0; textColor: TColor32 = clBlack32);

  function GetTextOutlineOnPath(const text: UnicodeString;
    const path: TPathD; font: TFontCache; textAlign: TTextAlign;
    x, y: double; charSpacing: double;
    out charsThatFit: integer; out outX: double): TPathsD; overload;

  function GetTextOutlineOnPath(const text: UnicodeString;
    const path: TPathD; font: TFontCache; textAlign: TTextAlign;
    perpendicOffset: integer = 0; charSpacing: double = 0): TPathsD; overload;

  function GetTextOutlineOnPath(const text: UnicodeString;
    const path: TPathD; font: TFontCache; textAlign: TTextAlign;
    perpendicOffset: integer; charSpacing: double;
    out charsThatFit: integer): TPathsD; overload;

  function GetTextOutlineOnPath(const text: UnicodeString;
    const path: TPathD; font: TFontCache; x, y: integer;
    charSpacing: double; out outX: double): TPathsD; overload;

  {$IFDEF MSWINDOWS}
  procedure FontHeightToFontSize(var logFontHeight: integer);
  procedure FontSizeToFontHeight(var logFontHeight: integer);
  function GetFontPixelHeight(logFontHeight: integer): double;
  function GetFontFolder: string;
  function GetInstalledTtfFilenames: TArrayOfString;

  // GetLogFonts: using DEFAULT_CHARSET will get logfonts
  // for ALL charsets that match the specified faceName.
  function GetLogFonts(const faceName: string;
    charSet: byte = DEFAULT_CHARSET): TArrayOfEnumLogFontEx;
  // GetLogFontFromEnumThatMatchesStyles:
  // will return false when no style match is found
  function GetLogFontFromEnumThatMatchesStyles(LogFonts: TArrayOfEnumLogFontEx;
    styles: TMacStyles; out logFont: TLogFont): Boolean;
  {$ENDIF}

  function FontManager: TFontManager;

implementation

uses
  Img32.Transform;

resourcestring
  rsChunkedTextRangeError  =
    'TChunkedText: range error.';
  rsFontCacheError     =
    'TFontCache error: notification received from the wrong TFontReader';
  rsChunkedTextFontError  =
    'TChunkedText: invalid font error.';

var
  aFontManager: TFontManager;

const
  lineFrac = 0.05;
  SPACE = ' ';

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

// GetMeaningfulDateTime: returns UTC date & time
procedure GetMeaningfulDateTime(const secsSince1904: Uint64;
  out yy,mo,dd, hh,mi,ss: cardinal);
const
  dayInYrAtMthStart: array[boolean, 0..12] of cardinal =
    ((0,31,59,90,120,151,181,212,243,273,304,334,365),  // non-leap year
    (0,31,60,91,121,152,182,213,244,274,305,335,366));  // leap year
var
  isLeapYr: Boolean;
const
  maxValidYear  = 2100;
  secsPerHour   = 3600;
  secsPerDay    = 86400;
  secsPerNormYr = 31536000;
  secsPerLeapYr = secsPerNormYr + secsPerDay;
  secsPer4Years = secsPerNormYr * 3 + secsPerLeapYr;    // 126230400;
begin
  // Leap years are divisble by 4, except for centuries which are not
  // leap years unless they are divisble by 400. (Hence 2000 was a leap year,
  // but 1900 was not. But 1904 was a leap year because it's divisble by 4.)
  // Validate at http://www.mathcats.com/explore/elapsedtime.html

  ss := (secsSince1904 div secsPer4Years);    // count '4years' since 1904

  // manage invalid dates
  if (secsSince1904 = 0) or
    (ss > (maxValidYear-1904) div 4) then
  begin
    yy := 1904; mo := 1; dd := 1;
    hh := 0; mi := 0; ss := 0;
    Exit;
  end;
  yy := 1904 + (ss * 4);

  ss := secsSince1904 mod secsPer4Years;      // secs since last leap yr
  isLeapYr := ss < secsPerLeapYr;
  if not isLeapYr then
  begin
    dec(ss, secsPerLeapYr);
    yy := yy + (ss div secsPerNormYr) + 1;
    ss := ss mod secsPerNormYr;               // remaining secs in final year
  end;
  dd := 1 + ss div secsPerDay;                // day number in final year
  mo := 1;                                    // 1, because mo is base 1
  while dayInYrAtMthStart[isLeapYr, mo] < dd do inc(mo);
  // remaining secs in month
  ss := ss - (dayInYrAtMthStart[isLeapYr, mo -1] * secsPerDay);
  dd := 1 + (ss div secsPerDay);              // because dd is base 1 too
  ss := ss mod secsPerDay;
  hh := ss div secsPerHour;
  ss := ss mod secsPerHour;
  mi := ss div 60;
  ss := ss mod 60;
end;
//------------------------------------------------------------------------------

function MergeArrayOfPaths(const pa: TArrayOfPathsD): TPathsD;
var
  i, j: integer;
  resultCount: integer;
begin
  Result := nil;

  // Preallocate the Result-Array
  resultCount := 0;
  for i := 0 to High(pa) do
    inc(resultCount, Length(pa[i]));
  SetLength(Result, resultCount);

  resultCount := 0;
  for i := 0 to High(pa) do
  begin
    for j := 0 to High(pa[i]) do
    begin
      Result[resultCount] := pa[i][j];
      inc(resultCount);
    end;
  end;
end;
//------------------------------------------------------------------------------

// MergeArrayOfPathsEx - merges AND translates/offsets paths
function MergeArrayOfPathsEx(const pa: TArrayOfPathsD; dx, dy: double): TPathsD;
var
  i, j: integer;
  resultCount: integer;
begin
  Result := nil;

  // Preallocate the Result-Array
  resultCount := 0;
  for i := 0 to High(pa) do
    inc(resultCount, Length(pa[i]));
  SetLength(Result, resultCount);

  resultCount := 0;
  for i := 0 to High(pa) do
  begin
    for j := 0 to High(pa[i]) do
    begin
      Result[resultCount] := TranslatePath(pa[i][j], dx, dy);
      inc(resultCount);
    end;
  end;
end;
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
  r: array[0..3] of byte absolute Result; // warning: do not inline
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
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  stream.Read(value, 1);
end;
//------------------------------------------------------------------------------

procedure GetShortInt(stream: TStream; out value: ShortInt);
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  stream.Read(value, 1);
end;
//------------------------------------------------------------------------------

function GetWord(stream: TStream; out value: WORD): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  if not Result then Exit;
  stream.Read(value, SizeOf(value));
  value := WordSwap(value);
end;
//------------------------------------------------------------------------------

function GetInt16(stream: TStream; out value: Int16): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  if not Result then Exit;
  stream.Read(value, SizeOf(value));
  value := Int16Swap(value);
end;
//------------------------------------------------------------------------------

function GetCardinal(stream: TStream; out value: Cardinal): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  if not Result then Exit;
  stream.Read(value, SizeOf(value));
  value := Cardinal(Int32Swap(Integer(value)));
end;
//------------------------------------------------------------------------------

function GetInt(stream: TStream; out value: integer): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  if not Result then Exit;
  stream.Read(value, SizeOf(value));
  value := Int32Swap(value);
end;
//------------------------------------------------------------------------------

function GetUInt64(stream: TStream; out value: UInt64): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  if not Result then Exit;
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

function GetWideString(stream: TStream; len: integer): Utf8String;
var
  i: integer;
  w: WORD;
  s: UnicodeString;
begin
  len := len div 2;
  setLength(s, len);
  for i := 1 to len do
  begin
    GetWord(stream, w);
    if w = 0 then
    begin
      SetLength(s, i -1);
      break;
    end;
    s[i] := WideChar(w);
   end;
   Result := Utf8String(s);
end;
//------------------------------------------------------------------------------

function GetUtf8String(stream: TStream; len: integer): Utf8String;
var
  i: integer;
begin
  setLength(Result, len+1);
  Result[len+1] := #0;
  stream.Read(Result[1], len);
  for i := 1 to length(Result) do
    if Result[i] = #0 then
    begin
      SetLength(Result, i -1);
      break;
    end;
end;
//------------------------------------------------------------------------------

function SameText(const text1, text2: Utf8String): Boolean; overload;
var
  len: integer;
begin
  len := Length(text1);
  Result := (Length(text2) = len) and
    ((len = 0) or CompareMem(@text1[1], @text2[1], len));
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
  fFormat4CodeMap       := nil;
  fFormat12CodeMap      := nil;
  fKernTable            := nil;
  FillChar(fTbl_post, SizeOf(fTbl_post), 0);
  fFontInfo.fontFormat  := ffInvalid;
  fFontInfo.family    := tfUnknown;
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
      // try .. except block because when TFontReader is destroyed in a
      // finalization section, it's possible for recipients to have been
      // destroyed without calling their destructors.
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
    if SelectObject(memDc, hdl) = 0 then Exit;
    // get the required size of the font data (file)
    cnt := Windows.GetFontData(memDc, 0, 0, nil, 0);
    result := cnt <> $FFFFFFFF;
    if not Result then Exit;
    // copy the font data into the memory stream
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

function TFontReader.Load(const FontName: string): Boolean;
var
  lf: TLogFont;
begin
  Result := false;
  if fontname = '' then Exit;
  FillChar(lf, sizeof(TLogFont), 0);
  lf.lfCharSet := DEFAULT_CHARSET;
  Move(fontname[1], lf.lfFaceName[0], Length(fontname) * SizeOf(Char));
  Result := Load(lf);
end;
//------------------------------------------------------------------------------

function TFontReader.Load(const logFont: TLogFont): Boolean;
var
  hdl: HFont;
begin
  Result := false;
  hdl := CreateFontIndirect({$IFDEF FPC}@{$ENDIF}logfont);
  if hdl > 0 then
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

function TFontReader.IsValidFontTable(const tbl : TFontTable): Boolean;
begin
  Result := (fStream.Size >= tbl.offset + tbl.length);
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
    (fTblIdxes[tblLoca] >= 0) and GetTable_loca and // loca must follow maxp
    (fTblIdxes[tblCmap] >= 0) and GetTable_cmap and
    (fTblIdxes[tblHmtx] >= 0) and IsValidFontTable(fTables[fTblIdxes[tblHmtx]]);

  if not Result then Exit;
  if (fTblIdxes[tblKern] >= 0) then GetTable_kern;
  if (fTblIdxes[tblPost] >= 0) then GetTable_post;

  GetFontFamily;
end;
//------------------------------------------------------------------------------

function TFontReader.GetTable_cmap: Boolean;
var
  i,j         : integer;
  segCount    : integer;
  format      : WORD;
  reserved    : WORD;
  format4Rec  : TCmapFormat4;
  format12Rec : TCmapFormat12;
  cmapTbl     : TFontTable;
  cmapTblRecs : array of TCmapTblRec;
label
  format4Error;
begin
  Result := false;
  cmapTbl := fTables[fTblIdxes[tblCmap]];
  if (fStream.Size < cmapTbl.offset + cmapTbl.length) then Exit;

  fStream.Position := cmapTbl.offset;
  GetWord(fStream, fTbl_cmap.version);
  GetWord(fStream, fTbl_cmap.numTables);

  // only use the unicode table (0: always first)
  SetLength(cmapTblRecs, fTbl_cmap.numTables);
  for i := 0 to fTbl_cmap.numTables -1 do
  begin
    GetWord(fStream, cmapTblRecs[i].platformID);
    GetWord(fStream, cmapTblRecs[i].encodingID);
    GetCardinal(fStream, cmapTblRecs[i].offset);
  end;

  for i := 0 to fTbl_cmap.numTables -1 do
  begin
    with cmapTblRecs[i] do
      if (platformID = 0) or (platformID = 3) then
        fStream.Position := cmapTbl.offset + offset
      else
        Continue;
    GetWord(fStream, format);

    case format of
      0:
        begin
          if Assigned(fFormat0CodeMap) then Continue;
          GetWord(fStream, format4Rec.length);
          GetWord(fStream, format4Rec.language);
          SetLength(fFormat0CodeMap, 256);
          for j := 0 to 255 do
            GetByte(fStream, fFormat0CodeMap[j]);
          fFontInfo.glyphCount := 255;
        end;
      4: // USC-2
        begin
          if Assigned(fFormat4CodeMap) then Continue;
          GetWord(fStream, format4Rec.length);
          GetWord(fStream, format4Rec.language);

          fFontInfo.glyphCount := 0;
          GetWord(fStream, format4Rec.segCountX2);
          segCount := format4Rec.segCountX2 shr 1;
          GetWord(fStream, format4Rec.searchRange);
          GetWord(fStream, format4Rec.entrySelector);
          GetWord(fStream, format4Rec.rangeShift);
          SetLength(fFormat4CodeMap, segCount);
          for j := 0 to segCount -1 do
            GetWord(fStream, fFormat4CodeMap[j].endCode);
          if fFormat4CodeMap[segCount-1].endCode <> $FFFF then
            GoTo format4Error;
          GetWord(fStream, reserved);
          if reserved <> 0 then
            GoTo format4Error;
          for j := 0 to segCount -1 do
            GetWord(fStream, fFormat4CodeMap[j].startCode);
          if fFormat4CodeMap[segCount-1].startCode <> $FFFF then
            GoTo format4Error;
          for j := 0 to segCount -1 do
            GetWord(fStream, fFormat4CodeMap[j].idDelta);

          fFormat4Offset := fStream.Position;
          for j := 0 to segCount -1 do
            GetWord(fStream, fFormat4CodeMap[j].rangeOffset);
          if Assigned(fFormat12CodeMap) then Break
          else Continue;

          format4Error:
          fFormat4CodeMap := nil;
        end;
      12: // USC-4
        begin
          if Assigned(fFormat12CodeMap) then Continue;
          GetWord(fStream, reserved);
          GetCardinal(fStream, format12Rec.length);
          GetCardinal(fStream, format12Rec.language);
          GetCardinal(fStream, format12Rec.nGroups);
          SetLength(fFormat12CodeMap, format12Rec.nGroups);
          for j := 0 to format12Rec.nGroups -1 do
            with fFormat12CodeMap[j] do
            begin
              GetCardinal(fStream, startCharCode);
              GetCardinal(fStream, endCharCode);
              GetCardinal(fStream, startGlyphCode);
            end;
          if Assigned(fFormat4CodeMap) then Break;
        end;
    end;
  end;
  Result := Assigned(fFormat4CodeMap) or Assigned(fFormat12CodeMap);
end;
//------------------------------------------------------------------------------

function TFontReader.GetGlyphIdxUsingCmap(codePoint: Cardinal): WORD;
var
  i: integer;
  w: WORD;
begin
  result := 0; // default to the 'missing' glyph
  if (codePoint < 256) and Assigned(fFormat0CodeMap) then
    Result := fFormat0CodeMap[codePoint]
  else if Assigned(fFormat12CodeMap) then
  begin
    for i := 0 to High(fFormat12CodeMap) do
      with fFormat12CodeMap[i] do
        if codePoint <= endCharCode then
        begin
          if codePoint < startCharCode then Break;
          result := (startGlyphCode + WORD(codePoint - startCharCode));
          Break;
        end;
  end
  else if (codePoint < $FFFF) and Assigned(fFormat4CodeMap) then
  begin
    for i := 0 to High(fFormat4CodeMap) do
      with fFormat4CodeMap[i] do
        if codePoint <= endCode then
        begin
          if codePoint < startCode then Break;
          if rangeOffset > 0 then
          begin
            fStream.Position := fFormat4Offset +
              rangeOffset + 2 * (i + WORD(codePoint - startCode));
            GetWord(fStream, w);
            if w < fTbl_maxp.numGlyphs then Result := w;
          end else
            result := (idDelta + codePoint) and $FFFF;
          Break;
        end;
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


function IsUnicode(platformID: WORD): Boolean;
begin
  Result := (platformID = 0) or (platformID = 3);
end;
//------------------------------------------------------------------------------

function GetNameRecString(stream: TStream;
  const nameRec: TNameRec; offset: cardinal): Utf8String;
var
  sPos, len: integer;
begin
  sPos := stream.Position;
  stream.Position := offset + nameRec.offset;
  if IsUnicode(nameRec.platformID) then
    Result := GetWideString(stream, nameRec.length) else
    Result := GetUtf8String(stream, nameRec.length);
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
  fFontInfo.fullFaceName := '';
  fFontInfo.style   := '';
  nameTbl := fTables[fTblIdxes[tblName]];
  Result := IsValidFontTable(nameTbl) and
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
      3: continue;
      4: fFontInfo.fullFaceName := GetNameRecString(fStream, nameRec, offset);
      5..7: continue;
      8: fFontInfo.manufacturer := GetNameRecString(fStream, nameRec, offset);
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
  Result := IsValidFontTable(headTbl) and (headTbl.length >= 54);
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
  Result := IsValidFontTable(hheaTbl) and (hheaTbl.length >= 36);
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

function TFontReader.GetGlyphHorzMetrics(glyphIdx: WORD): TFontTable_Hmtx;
var
  tbl : TFontTable;
begin
  tbl := fTables[fTblIdxes[tblHmtx]];
  if glyphIdx < fTbl_hhea.numLongHorMets then
  begin
    fStream.Position := Integer(tbl.offset) + glyphIdx * 4;
    GetWord(fStream, Result.advanceWidth);
    GetInt16(fStream, Result.leftSideBearing);
  end else
  begin
    fStream.Position := Integer(tbl.offset) +
      Integer(fTbl_hhea.numLongHorMets -1) * 4;
    GetWord(fStream, Result.advanceWidth);
    fStream.Position := Integer(tbl.offset +
      fTbl_hhea.numLongHorMets * 4) +
      2 * (glyphIdx - Integer(fTbl_hhea.numLongHorMets));
    GetInt16(fStream, Result.leftSideBearing);
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
  if not IsValidFontTable(tbl) then Exit;
  fStream.Position := Integer(tbl.offset);

  GetWord(fStream, tbl_kern.version);
  GetWord(fStream, tbl_kern.numTables);
  if tbl_kern.numTables = 0 then Exit;
  // assume there's only one kern table

  GetWord(fStream, kernSub.version);
  GetWord(fStream, kernSub.length);
  GetWord(fStream, kernSub.coverage);
  // we're currently only interested in Format0 horizontal kerning
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
  if not IsValidFontTable(tbl) then Exit;
  fStream.Position := Integer(tbl.offset);

  GetWord(fStream,      fTbl_post.majorVersion);
  GetWord(fStream,      fTbl_post.minorVersion);
  GetFixed(fStream,     fTbl_post.italicAngle);
  GetInt16(fStream,     fTbl_post.underlinePos);
  GetInt16(fStream,     fTbl_post.underlineWidth);
  GetCardinal(fStream,  fTbl_post.isFixedPitch);
end;
//------------------------------------------------------------------------------

function FindKernInTable(glyphIdx: WORD; const kernTable: TArrayOfKernRecs): integer;
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
        // found a match! Now find the very first one ...
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

function TFontReader.GetGlyphKernList(glyphIdx: WORD): TArrayOfTKern;
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

function TFontReader.GetGlyphPaths(glyphIdx: WORD;
  var tbl_hmtx: TFontTable_Hmtx; out tbl_glyf: TFontTable_Glyf): TPathsEx;
var
  offset: cardinal;
  glyfTbl: TFontTable;
begin
  result := nil;
  if fTbl_head.indexToLocFmt = 0 then
  begin
    offset := fTbl_loca2[glyphIdx] *2;
    if offset = fTbl_loca2[glyphIdx+1] *2 then Exit;  // no contours
  end else
  begin
    offset := fTbl_loca4[glyphIdx];
    if offset = fTbl_loca4[glyphIdx+1] then Exit;     // no contours
  end;
  glyfTbl := fTables[fTblIdxes[tblGlyf]];
  if offset >= glyfTbl.length then Exit;
  inc(offset, glyfTbl.offset);

  fStream.Position := offset;
  GetInt16(fStream, tbl_glyf.numContours);
  GetInt16(fStream, tbl_glyf.xMin);
  GetInt16(fStream, tbl_glyf.yMin);
  GetInt16(fStream, tbl_glyf.xMax);
  GetInt16(fStream, tbl_glyf.yMax);

  if tbl_glyf.numContours < 0 then
    result := GetCompositeGlyph(tbl_glyf, tbl_hmtx) else
    result := GetSimpleGlyph(tbl_glyf);
end;
//------------------------------------------------------------------------------

const
  // glyf flags - simple
  ON_CURVE                  = $1;
  X_SHORT_VECTOR            = $2;
  Y_SHORT_VECTOR            = $4;
  REPEAT_FLAG               = $8;
  X_DELTA                   = $10;
  Y_DELTA                   = $20;
//------------------------------------------------------------------------------

function TFontReader.GetSimpleGlyph(tbl_glyf: TFontTable_Glyf): TPathsEx;
var
  i,j, len: integer;
  instructLen: WORD;
  flag, repeats: byte;
  contourEnds: TArrayOfWord;
begin
  SetLength(contourEnds, tbl_glyf.numContours);
  for i := 0 to High(contourEnds) do
    GetWord(fStream, contourEnds[i]);

  // hints are currently ignored
  GetWord(fStream, instructLen);
  fStream.Position := fStream.Position + instructLen;

  setLength(result, tbl_glyf.numContours);
  repeats := 0;
  flag := 0; // help the compiler with "flag isn't initialized"
  for i := 0 to High(result) do
  begin
    if i = 0 then len := contourEnds[0] +1
    else len := contourEnds[i] - contourEnds[i-1];
    setLength(result[i], len);

    for j := 0 to len -1 do
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
  if tbl_glyf.numContours > 0 then
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
  // get X coords
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
          if GetInt16(fStream, xi) then
            pt.X := pt.X + xi;
        end;
      end;
      paths[i][j].pt.X := pt.X;
    end;
  end;

  // get Y coords
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
          if GetInt16(fStream, yi) then
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
var
  i,j: integer;
  mat: TMatrixD;
begin
  // https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6glyf.html
  if ((a = 0) and (b = 0)) or ((c = 0) and (d = 0)) then
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
    mat[0,0] := a;
    mat[0,1] := b;
    mat[1,0] := c;
    mat[1,1] := d;
    mat[2][0] := e;
    mat[2][1] := f;

    for i := 0 to High(pathsEx) do
      for j := 0 to High(pathsEx[i]) do
        MatrixApply(mat, pathsEx[i][j].pt);
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.GetCompositeGlyph(var tbl_glyf: TFontTable_Glyf;
  var tbl_hmtx: TFontTable_Hmtx): TPathsEx;
var
  streamPos: integer;
  flag, glyphIndex: WORD;
  arg1_i8, arg2_i8: ShortInt;
  arg1_i16, arg2_i16: Int16;
  tmp_single: single;
  a,b,c,d,e,f: double;
  componentPaths: TPathsEx;
  component_tbl_glyf: TFontTable_Glyf;
  component_tbl_hmtx: TFontTable_Hmtx;
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
      GetInt16(fStream, arg1_i16);
      GetInt16(fStream, arg2_i16);
      if (flag and ARGS_ARE_XY_VALUES <> 0) then
      begin
        e := arg1_i16;
        f := arg2_i16;
      end;
    end else
    begin
      GetShortInt(fStream, arg1_i8);
      GetShortInt(fStream, arg2_i8);
      if (flag and ARGS_ARE_XY_VALUES <> 0) then
      begin
        e := arg1_i8;
        f := arg2_i8;
      end;
    end;

    if (flag and WE_HAVE_A_SCALE <> 0) then
    begin
      Get2Dot14(fStream, tmp_single);
      a := tmp_single; d := tmp_single;
    end
    else if (flag and WE_HAVE_AN_X_AND_Y_SCALE <> 0) then
    begin
      Get2Dot14(fStream, tmp_single); a := tmp_single;
      Get2Dot14(fStream, tmp_single); d := tmp_single;
    end
    else if (flag and WE_HAVE_A_TWO_BY_TWO <> 0) then
    begin
      Get2Dot14(fStream, tmp_single); a := tmp_single;
      Get2Dot14(fStream, tmp_single); b := tmp_single;
      Get2Dot14(fStream, tmp_single); c := tmp_single;
      Get2Dot14(fStream, tmp_single); d := tmp_single;
    end;

    component_tbl_hmtx := tbl_hmtx;

    // GetGlyphPaths() will change the stream position, so save it.
    streamPos := fStream.Position;
    componentPaths := GetGlyphPaths(glyphIndex, component_tbl_hmtx, component_tbl_glyf);
    // return to saved stream position
    fStream.Position := streamPos;

    if (flag and ARGS_ARE_XY_VALUES <> 0) then
      AffineTransform(a,b,c,d,e,f, componentPaths); // (#131)

    if (flag and USE_MY_METRICS <> 0) then
      tbl_hmtx := component_tbl_hmtx;               // (#24)

    if component_tbl_glyf.numContours > 0 then
    begin
      if tbl_glyf.numContours < 0 then tbl_glyf.numContours := 0;
      inc(tbl_glyf.numContours, component_tbl_glyf.numContours);
      tbl_glyf.xMin := Min(tbl_glyf.xMin, component_tbl_glyf.xMin);
      tbl_glyf.xMax := Max(tbl_glyf.xMax, component_tbl_glyf.xMax);
      tbl_glyf.yMin := Min(tbl_glyf.yMin, component_tbl_glyf.yMin);
      tbl_glyf.yMax := Max(tbl_glyf.yMax, component_tbl_glyf.yMax);
    end;
    AppendPathsEx(result, componentPaths);
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.HasGlyph(codepoint: Cardinal): Boolean;
begin
  Result := GetGlyphIdxUsingCmap(codepoint) > 0;
end;
//------------------------------------------------------------------------------

function FlattenPathExBeziers(pathsEx: TPathsEx): TPathsD;
var
  i,j : integer;
  pt2: TPointEx;
  bez: TPathD;
begin
  setLength(Result, length(pathsEx));
  for i := 0 to High(pathsEx) do
  begin
    SetLength(Result[i],1);
    Result[i][0] := pathsEx[i][0].pt;
    for j := 1 to High(pathsEx[i]) do
    begin
      if OnCurve(pathsEx[i][j].flag) then
      begin
        AppendPoint(Result[i], pathsEx[i][j].pt);
      end else
      begin
        if j = High(pathsEx[i]) then
          pt2 := pathsEx[i][0] else
          pt2 := pathsEx[i][j+1];
        bez := FlattenQBezier(pathsEx[i][j-1].pt, pathsEx[i][j].pt, pt2.pt);
        ConcatPaths(Result[i], bez);
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TFontReader.GetGlyphInfo(codepoint: Cardinal;
  out nextX: integer; out glyphInfo: TGlyphInfo): Boolean;
var
  glyphIdx: WORD;
begin
  Result := IsValidFontFormat;
  if not Result then Exit;
  glyphIdx := GetGlyphIdxUsingCmap(codepoint);
  glyphInfo := GetGlyphInfoInternal(glyphIdx);
  glyphInfo.hmtx := GetGlyphHorzMetrics(glyphIdx);
  nextX   := glyphInfo.hmtx.advanceWidth;
  glyphInfo.codepoint := codepoint;
end;
//------------------------------------------------------------------------------

function TFontReader.GetFontInfo: TFontInfo;
begin
  if not IsValidFontFormat then
  begin
    FillChar(Result, SizeOf(Result), 0);
    Exit;
  end;

  result := fFontInfo;
  if result.unitsPerEm > 0 then Exit;

  // and updated the record with everything except the strings
  result.unitsPerEm  := fTbl_head.unitsPerEm;
  result.xMin        := fTbl_head.xMin;
  result.xMax        := fTbl_head.xMax;
  result.yMin        := fTbl_head.yMin;
  result.yMax        := fTbl_head.yMax;

  // note: the following three fields "represent the design
  // intentions of the font's creator rather than any computed value"
  // https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6hhea.html

  result.ascent      := fTbl_hhea.ascent;
  result.descent     := abs(fTbl_hhea.descent);
  result.lineGap     := fTbl_hhea.lineGap;
  result.advWidthMax := fTbl_hhea.advWidthMax;
  result.minLSB      := fTbl_hhea.minLSB;
  result.minRSB      := fTbl_hhea.minRSB;
  result.xMaxExtent  := fTbl_hhea.xMaxExtent;
end;
//------------------------------------------------------------------------------

function TFontReader.GetGlyphInfoInternal(glyphIdx: WORD): TGlyphInfo;
var
  pathsEx: TPathsEx;
begin
  FillChar(result, sizeOf(Result), 0);
  if not IsValidFontFormat then Exit;
  result.glyphIdx := glyphIdx;
  result.unitsPerEm  := fTbl_head.unitsPerEm;
  // get raw splines
  pathsEx := GetGlyphPaths(glyphIdx, result.hmtx, result.glyf);
  if Assigned(pathsEx) then
  begin
    pathsEx := ConvertSplinesToBeziers(pathsEx);
    result.paths := FlattenPathExBeziers(PathsEx);
  end;
  Result.kernList := GetGlyphKernList(glyphIdx);
end;
//------------------------------------------------------------------------------

function TFontReader.GetWeight: integer;
var
  i, dummy: integer;
  accum: Cardinal;
  gm: TGlyphInfo;
  rec: TRectD;
  img: TImage32;
  p: PARGB;
const
  imgSize = 16;
  k = 5; // an empirical constant
begin
  // get an empirical weight based on the character 'G'
  result := 0;
  if not IsValidFontFormat then Exit;
  if fFontWeight > 0 then
  begin
    Result := fFontWeight;
    Exit;
  end;
  GetGlyphInfo(Ord('G'),dummy, gm);
  rec := GetBoundsD(gm.paths);
  gm.paths := Img32.Vector.TranslatePath(gm.paths, -rec.Left, -rec.Top);
  gm.paths := Img32.Vector.ScalePath(gm.paths, imgSize/rec.Width, imgSize/rec.Height);
  img := TImage32.Create(imgSize,imgSize);
  try
    DrawPolygon(img, gm.paths, frEvenOdd, clBlack32);
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

procedure TFontReader.GetFontFamily;
var
  giT, giI, giM: integer;
  gmT: TGlyphInfo;
  hmtxI, hmtxM: TFontTable_Hmtx;
begin
  fFontInfo.family := tfUnknown;

  if (fTbl_post.majorVersion > 0) and
    (fTbl_post.isFixedPitch <> 0) then
  begin
    fFontInfo.family := tfMonospace;
    Exit;
  end;

  // use glyph metrics for 'T', 'i' & 'm' to determine the font family
  // if the widths of 'i' & 'm' are equal, then assume a monospace font
  // else if the number of vertices used to draw 'T' is greater than 10
  // then assume a serif font otherwise assume a sans serif font.

  giT := GetGlyphIdxUsingCmap(Ord('T'));
  giI := GetGlyphIdxUsingCmap(Ord('i'));
  giM := GetGlyphIdxUsingCmap(Ord('m'));
  if (giT = 0) or (giI = 0) or (giM = 0) then Exit;

  hmtxI := GetGlyphHorzMetrics(giI);
  hmtxM := GetGlyphHorzMetrics(giM);
  if hmtxI.advanceWidth = hmtxM.advanceWidth then
  begin
    fFontInfo.family := tfMonospace;
    Exit;
  end;

  gmT := GetGlyphInfoInternal(giT);
  if Assigned(gmT.paths) and (Length(gmT.paths[0]) > 10) then
    fFontInfo.family := tfSerif else
    fFontInfo.family := tfSansSerif;
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
      // try .. except block because when TFontCache is destroyed in a
      // finalization section, it's possible for recipients to have been
      // destroyed without calling their destructors.
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
function FindInSortedList(charOrdinal: Cardinal; glyphList: TList<PGlyphInfo>): integer;
{$ELSE}
function FindInSortedList(charOrdinal: Cardinal; glyphList: TList): integer;
{$ENDIF}
var
  i,l,r: integer;
begin
  // binary search the sorted list ...
  l := 0;
  r := glyphList.Count -1;
  while l <= r do
  begin
    Result := (l + r) shr 1;
    i := integer(PGlyphInfo(glyphList[Result]).codepoint) - integer(charOrdinal);
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

function TFontCache.FoundInList(charOrdinal: Cardinal): Boolean;
begin
  if not fSorted then Sort;
  result := FindInSortedList(charOrdinal, fGlyphInfoList) >= 0;
end;
//------------------------------------------------------------------------------

procedure TFontCache.GetMissingGlyphs(const ordinals: TArrayOfCardinal);
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
  if not IsValidFont then Result := 0
  else with fFontReader.FontInfo do
    Result := Max(ascent, yMax) * fScale;
end;
//------------------------------------------------------------------------------

function TFontCache.GetDescent: double;
begin
  if not IsValidFont then Result := 0
  else with fFontReader.FontInfo do
    Result := Max(descent, -yMin) * fScale;
end;
//------------------------------------------------------------------------------

function TFontCache.GetGap: double;
begin
  if not IsValidFont then Result := 0
  else Result := fFontReader.FontInfo.lineGap * fScale;
end;
//------------------------------------------------------------------------------

function TFontCache.GetLineHeight: double;
begin
  if not IsValidFont then Result := 0
  else Result := Ascent + Descent + LineGap;
end;
//------------------------------------------------------------------------------

function TFontCache.GetYyHeight: double;
var
  minY, maxY: double;
begin
  // nb: non-inverted Y coordinates.
  maxY := GetGlyphInfo(ord('Y')).glyf.yMax;
  minY := GetGlyphInfo(ord('y')).glyf.yMin;
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

function FindInKernList(glyphIdx: WORD; const kernList: TArrayOfTKern): integer;
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
      if i = 0 then Exit; // found!
      r := Result -1;
    end;
  end;
  Result := -1;
end;
//------------------------------------------------------------------------------

function TFontCache.GetGlyphInfo(codepoint: Cardinal): PGlyphInfo;
var
  listIdx: integer;
begin
  Result := nil;
  if not IsValidFont then Exit;
  if not fSorted then Sort;
  listIdx := FindInSortedList(codepoint, fGlyphInfoList);
  if listIdx < 0 then
    Result := AddGlyph(codepoint) else
    Result := PGlyphInfo(fGlyphInfoList[listIdx]);
end;
//------------------------------------------------------------------------------

function IsSurrogate(c: WideChar): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := (c >= #$D800) and (c <= #$DFFF);
end;
//------------------------------------------------------------------------------

function ConvertSurrogatePair(hiSurrogate, loSurrogate: Cardinal): Cardinal;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := ((hiSurrogate - $D800) shl 10) + (loSurrogate - $DC00) + $10000;
end;
//------------------------------------------------------------------------------

function TFontCache.GetTextCodePoints(const text: UnicodeString): TArrayOfCardinal;
var
  i,j, len: integer;
  inSurrogate: Boolean;
begin
  len := Length(text);
  setLength(Result, len);
  inSurrogate := false;
  j := 0;
  for i := 1 to len do
  begin
    if inSurrogate then
    begin
      Result[j] := ConvertSurrogatePair(Ord(text[i -1]), Ord(text[i]));
      inSurrogate := false;
    end
    else if IsSurrogate(text[i]) then
    begin
      inSurrogate := true;
      Continue;
    end
    else
      Result[j] := Ord(WideChar(text[i]));
    inc(j);
  end;
  setLength(Result, j);
end;
//------------------------------------------------------------------------------

function TFontCache.GetGlyphOffsets(const text: UnicodeString;
 interCharSpace: double): TArrayOfDouble;
var
  i,j, len: integer;
  codePoints: TArrayOfCardinal;
  glyphInfo: PGlyphInfo;
  thisX: double;
  prevGlyphKernList: TArrayOfTKern;
begin
  codePoints := GetTextCodePoints(text);
  len := Length(codePoints);
  SetLength(Result, len +1);
  Result[0] := 0;
  if len = 0 then Exit;
  GetMissingGlyphs(codePoints);

  thisX := 0;
  prevGlyphKernList := nil;
  for i := 0 to High(codePoints) do
  begin
    glyphInfo := GetGlyphInfo(codePoints[i]);
    if not assigned(glyphInfo) then Break;
    if fUseKerning and assigned(prevGlyphKernList) then
    begin
      j := FindInKernList(glyphInfo.glyphIdx, prevGlyphKernList);
      if (j >= 0) then
        thisX := thisX + prevGlyphKernList[j].kernValue*fScale;
    end;
    Result[i] := thisX;
    thisX := thisX + glyphInfo.hmtx.advanceWidth*fScale +interCharSpace;
    prevGlyphKernList := glyphInfo.kernList;
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
  offsets := GetGlyphOffsets(text);
  Result := offsets[high(offsets)];
end;
//------------------------------------------------------------------------------

function TFontCache.CountCharsThatFit(const text: UnicodeString;
  maxWidth: double): integer;
var
  offsets: TArrayOfDouble;
begin
  Result := 0;
  if not IsValidFont then Exit;
  offsets := GetGlyphOffsets(text);
  Result := Length(offsets);
  while offsets[Result -1] > maxWidth do
    Dec(Result);
end;
//------------------------------------------------------------------------------

function TFontCache.GetSpaceWidth: double;
begin
  Result := GetGlyphInfo(32).hmtx.advanceWidth * fScale;
end;
//------------------------------------------------------------------------------

function TFontCache.GetTextOutline(x, y: double; const text: UnicodeString): TPathsD;
var
  dummy: double;
begin
  Result := GetTextOutline(x, y, text, dummy);
end;
//------------------------------------------------------------------------------

function TFontCache.GetTextOutline(x, y: double; const text: UnicodeString;
  out nextX: double; underlineIdx: integer): TPathsD;
var
  arrayOfGlyphs: TArrayOfPathsD;
  dummy: TArrayOfDouble;
begin
  Result := nil;
  if not GetTextOutlineInternal(x, y,
    text, underlineIdx, arrayOfGlyphs, dummy, nextX) then Exit;
  Result := MergeArrayOfPaths(arrayOfGlyphs);
end;
//------------------------------------------------------------------------------

function TFontCache.GetTextOutline(const rec: TRectD; const text: UnicodeString;
  ta: TTextAlign; tav: TTextVAlign; underlineIdx: integer): TPathsD;
var
  dummy2, dx, dy: double;
  arrayOfGlyphs: TArrayOfPathsD;
  dummy1: TArrayOfDouble;
  rec2: TRectD;
begin
  Result := nil;
  if not GetTextOutlineInternal(0, 0, text, underlineIdx,
    arrayOfGlyphs, dummy1, dummy2) or (arrayOfGlyphs = nil) then Exit;

  rec2 := GetBoundsD(arrayOfGlyphs);
  case ta of
    taRight: dx := rec.Right - rec2.Width;
    taCenter: dx := rec.Left + (rec.Width - rec2.Width)/ 2;
    else dx := rec.Left;
  end;

  case tav of
    tvaMiddle: dy := rec.Top - rec2.Top + (rec.Height - rec2.Height)/ 2;
    tvaBottom: dy := rec.Bottom - Descent;
    else dy := rec.Top + Ascent;
  end;

  Result := MergeArrayOfPathsEx(arrayOfGlyphs, dx, dy);
end;
//------------------------------------------------------------------------------

function TFontCache.GetUnderlineOutline(leftX, rightX, y: double;
  dy: double; wavy: Boolean; strokeWidth: double): TPathD;
var
  i, cnt: integer;
  dx: double;
  wavyPath: TPathD;
begin

  if strokeWidth <= 0 then
    strokeWidth := LineHeight * lineFrac;
  if dy = InvalidD then
    y := y + 1.5 * (1 + strokeWidth) else
    y := y + dy;

  if wavy then
  begin
    Result := nil;
    cnt := Ceil((rightX - leftX) / (strokeWidth *4));
    if cnt < 2 then Exit;
    dx := (rightX - leftX)/ cnt;
    SetLength(wavyPath, cnt +2);
    wavyPath[0] := PointD(leftX, y + strokeWidth/2);
    wavyPath[1] := PointD(leftX + dx/2, y-(strokeWidth *2));

    for i := 1 to cnt do
      wavyPath[i+1] := PointD(leftX + dx * i, y + strokeWidth/2);
    Result := FlattenQSpline(wavyPath);
    wavyPath := ReversePath(Result);
    wavyPath := TranslatePath(wavyPath, 0, strokeWidth *1.5);
    ConcatPaths(Result, wavyPath);
  end else
    Result := Rectangle(leftX, y, rightX, y + strokeWidth);
end;
//------------------------------------------------------------------------------

function TFontCache.GetVerticalTextOutline(x, y: double;
  const text: UnicodeString; lineHeight: double): TPathsD;
var
  i, cnt, xxMax: integer;
  glyphInfo: PGlyphInfo;
  dx: double;
  codePoints: TArrayOfCardinal;
  glyphInfos: array of PGlyphInfo;
begin
  Result := nil;
  if not IsValidFont then Exit;

  codePoints := GetTextCodePoints(text);
  xxMax := 0;
  cnt := Length(codePoints);
  SetLength(glyphInfos, cnt);
  for i := 0 to cnt -1 do
  begin
    glyphInfos[i] := GetGlyphInfo(codePoints[i]);
    if not assigned(glyphInfos[i]) then Exit;
    with glyphInfos[i].glyf do
      if xMax > xxMax then
         xxMax := xMax;
  end;

  if lineHeight = 0.0 then
    lineHeight := self.LineHeight;

  for i := 0 to cnt -1 do
  begin
    glyphInfo := glyphInfos[i];
    with glyphInfo.glyf do
      dx :=  (xxMax - xMax) * 0.5 * scale;
    AppendPath(Result, TranslatePath(glyphInfo.paths, x + dx, y));
    y := y + lineHeight;
  end;
  UpdateFontReaderLastUsedTime;
end;
//------------------------------------------------------------------------------

function TFontCache.GetTextOutlineInternal(x, y: double;
  const text: UnicodeString; underlineIdx: integer; out glyphs: TArrayOfPathsD;
  out offsets: TArrayOfDouble; out nextX: double): Boolean;
var
  i,j, len    : integer;
  dx,y2,w     : double;
  codepoints  : TArrayOfCardinal;
  glyphInfo   : PGlyphInfo;
  currGlyph   : TPathsD;
  prevGlyphKernList: TArrayOfTKern;
begin
  Result := true;
  codePoints := GetTextCodePoints(text);
  len := Length(codepoints);
  GetMissingGlyphs(codepoints);

  SetLength(offsets, len);
  nextX := x;
  prevGlyphKernList := nil;
  for i := 0 to len -1 do
  begin
    offsets[i] := nextX;
    glyphInfo := GetGlyphInfo(codepoints[i]);
    if not assigned(glyphInfo) then Break;
    if fUseKerning and assigned(prevGlyphKernList) then
    begin
      j := FindInKernList(glyphInfo.glyphIdx, prevGlyphKernList);
      if (j >= 0) then
        nextX := nextX + prevGlyphKernList[j].kernValue * fScale;
    end;

    currGlyph := TranslatePath(glyphInfo.paths, nextX, y);
    dx := glyphInfo.hmtx.advanceWidth * fScale;
    AppendPath(glyphs, currGlyph);

    if not fUnderlined and (underlineIdx -1 = i) then
    begin
      w := LineHeight * lineFrac;
      y2 := y + 1.5 * (1 + w);
      SetLength(currGlyph, 1);
      currGlyph[0] := Rectangle(nextX, y2, nextX +dx, y2 + w);
      AppendPath(glyphs, currGlyph);
    end;

    nextX := nextX + dx;
    prevGlyphKernList := glyphInfo.kernList;
  end;

  if fUnderlined then
  begin
    w := LineHeight * lineFrac;
    y2 := y + 1.5 * (1 + w);
    SetLength(currGlyph, 1);
    currGlyph[0] := Rectangle(x, y2, nextX, y2 + w);
    AppendPath(glyphs, currGlyph);
  end;

  if fStrikeOut then
  begin
    w := LineHeight * lineFrac;
    y2 := y - LineHeight * 0.22;
    SetLength(currGlyph, 1);
    currGlyph[0] := Rectangle(x, y2, nextX, y2 + w);
    AppendPath(glyphs, currGlyph);
  end;

  UpdateFontReaderLastUsedTime;
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
  UpdateFontReaderLastUsedTime;
end;
//------------------------------------------------------------------------------

procedure TFontCache.UpdateFontReaderLastUsedTime;
begin
  if Assigned(fFontReader) then
    fFontReader.LastUsedTime := now;
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
    fScale := fFontHeight / fFontReader.FontInfo.unitsPerEm else
    fScale := 1;
  NotifyRecipients(inStateChange);
end;
//------------------------------------------------------------------------------

procedure TFontCache.SetFontHeight(newHeight: double);
begin
  newHeight := abs(newHeight); // manage point - pixel conversions externally
  if fFontHeight = newHeight then Exit;
  fFontHeight := newHeight;
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
     FlipVert(glyphInfo.paths);
  end;
  fFlipVert := value;
end;
//------------------------------------------------------------------------------

function GlyphSorter(glyph1, glyph2: pointer): integer;
begin
  Result := PGlyphInfo(glyph1).codepoint - PGlyphInfo(glyph2).codepoint;
end;
//------------------------------------------------------------------------------

procedure TFontCache.Sort;
begin
{$IFDEF XPLAT_GENERICS}
  fGlyphInfoList.Sort(TComparer<PGlyphInfo>.Construct(
    function (const glyph1, glyph2: PGlyphInfo): integer
    begin
      Result := glyph1.codepoint - glyph2.codepoint;
    end));
{$ELSE}
  fGlyphInfoList.Sort(GlyphSorter);
{$ENDIF}
  fSorted := true;
end;
//------------------------------------------------------------------------------

function TFontCache.AddGlyph(codepoint: Cardinal): PGlyphInfo;
var
  dummy: integer;
  altFontReader: TFontReader;
  glyphIdx: WORD;
  scale: double;
const
  minLength = 0.1;
begin

  New(Result);
  Result.codepoint := codepoint;
  if not fFontReader.GetGlyphInfo(codepoint, dummy, Result^) or
    (Result.glyphIdx = 0) then
  begin
    // to get here the unicode char is not supported by fFontReader
    altFontReader :=
      aFontManager.FindReaderContainingGlyph(codepoint, tfUnknown, glyphIdx);
    if Assigned(altFontReader) then
    begin
      altFontReader.GetGlyphInfo(codepoint, dummy, Result^);
      altFontReader.LastUsedTime := now;
      scale := fFontReader.FontInfo.unitsPerEm / altFontReader.FontInfo.unitsPerEm;
      if scale <> 1.0 then
        Result.paths := ScalePath(Result.paths, scale);
    end;
  end;

  fGlyphInfoList.Add(Result);
  if fFontHeight > 0 then
  begin
    Result.paths := ScalePath(Result.paths, fScale);
    // text rendering is about twice as fast when excess detail is removed
    Result.paths := StripNearDuplicates(Result.paths, minLength, true);
  end;

  if fFlipVert then VerticalFlip(Result.paths);
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

procedure FontHeightToFontSize(var logFontHeight: integer);
const
  _72Div96 = 72/96;
begin
  if logFontHeight < 0 then
    logFontHeight := -Round(logFontHeight * _72Div96 / dpiAware1);
end;
//------------------------------------------------------------------------------

procedure FontSizeToFontHeight(var logFontHeight: integer);
const
  _96Div72 = 96/72;
begin
  if logFontHeight > 0 then
    logFontHeight := -Round(DpiAware(logFontHeight * _96Div72));
end;
//------------------------------------------------------------------------------

function GetFontPixelHeight(logFontHeight: integer): double;
const
  _96Div72 = 96/72;
begin
  if logFontHeight > 0 then
    Result := DPIAware(logFontHeight * _96Div72) else
    Result := DPIAware(-logFontHeight);
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
//------------------------------------------------------------------------------

function EnumFontProc(LogFont: PEnumLogFontEx; TextMetric: PNewTextMetric;
  FontType: DWORD; userDefined: LPARAM): Integer; stdcall;
var
  len: integer;
  alf: PArrayOfEnumLogFontEx absolute userDefined;
begin
  if (FontType = TRUETYPE_FONTTYPE) then
  begin
    len := Length(alf^);
    SetLength(alf^, len +1);
    Move(LogFont^, alf^[len], SizeOf(TEnumLogFontEx));
  end;
  Result := 1;
end;
//------------------------------------------------------------------------------

function GetLogFonts(const faceName: string; charSet: byte): TArrayOfEnumLogFontEx;
var
  lf: TLogFont;
  dc: HDC;
begin
  Result := nil;
  if faceName = '' then Exit;
  FillChar(lf, sizeof(lf), 0);
  lf.lfCharSet := charSet;
  Move(faceName[1], lf.lfFaceName[0], Length(faceName) * SizeOf(Char));
  dc := CreateCompatibleDC(0);
  try
    EnumFontFamiliesEx(dc, lf, @EnumFontProc, LParam(@Result), 0);
  finally
    DeleteDC(dc);
  end;
end;
//------------------------------------------------------------------------------

function GetLogFontFromEnumThatMatchesStyles(LogFonts: TArrayOfEnumLogFontEx;
  styles: TMacStyles; out logFont: TLogFont): Boolean;
var
  i: integer;
  styles2: TMacStyles;
begin
  Result := False;
  if not Assigned(LogFonts) then Exit;
  for i := 0 to High(LogFonts) do
  begin
    styles2 := [];
    if LogFonts[i].elfLogFont.lfWeight > 500 then Include(styles2, msBold);
    if LogFonts[i].elfLogFont.lfItalic <> 0 then Include(styles2, msItalic);
    if styles <> styles2 then Continue;
    logFont := LogFonts[i].elfLogFont;
    Result := true;
    Exit;
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function DrawText(image: TImage32; x, y: double; const text: UnicodeString;
  font: TFontCache; textColor: TColor32 = clBlack32): double;
var
  glyphs: TPathsD;
begin
  Result := 0;
  if (text = '') or not assigned(font) or not font.IsValidFont then Exit;
  glyphs := font.GetTextOutline(x,y, text, Result);
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

procedure DrawText(image: TImage32; const rec: TRectD;
  const text: UnicodeString; font: TFontCache;
  textColor: TColor32 = clBlack32; align: TTextAlign = taCenter;
  valign: TTextVAlign = tvaMiddle);
var
  glyphs: TPathsD;
  dx,dy: double;
  rec2: TRectD;
  chunkedText: TChunkedText;
begin
  if (text = '') or not assigned(font) or not font.IsValidFont then Exit;
  if align = taJustify then
  begin
    chunkedText := TChunkedText.Create(text, font, textColor);
    try
      chunkedText.DrawText( image, Rect(rec), taJustify, valign, 0);
    finally
      chunkedText.Free;
    end;
    Exit;
  end;

  glyphs := font.GetTextOutline(0,0, text);
  rec2 := GetBoundsD(glyphs);
  case align of
    taRight: dx := rec.Right - rec2.Right;
    taCenter: dx := (rec.Left + rec.Right - rec2.Right) * 0.5;
    else dx := rec.Left;
  end;
  case valign of
    tvaMiddle: dy := (rec.Top + rec.Bottom - rec2.Top) * 0.5;
    tvaBottom: dy := rec.Bottom - rec2.Bottom;
    else dy := rec.Top + font.Ascent;
  end;
  glyphs := TranslatePath(glyphs, dx, dy);
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
  if not assigned(font) or not font.IsValidFont then
  begin
    Result := NullPointD;
    Exit;
  end;
  glyphs := font.GetAngledTextGlyphs(x, y,
    text, angleRadians, rotatePt, Result);
  DrawPolygon(image, glyphs, frNonZero, textColor);
end;
//------------------------------------------------------------------------------

procedure DrawVerticalText(image: TImage32; x, y: double;
  const text: UnicodeString; font: TFontCache;
  lineHeight: double; textColor: TColor32);
var
  glyphs: TPathsD;
  cr: TCustomRenderer;
begin
  if not assigned(font) or not font.IsValidFont or (text = '') then Exit;
  glyphs := font.GetVerticalTextOutline(x,y, text, lineHeight);
  if image.AntiAliased then
    cr := TColorRenderer.Create(textColor) else
    cr := TAliasedColorRenderer.Create(textColor);
  try
    DrawPolygon(image, glyphs, frNonZero, cr);
  finally
    cr.Free;
  end;
end;
//------------------------------------------------------------------------------

function FindLastSpace(const text: string; StartAt: integer): integer;
begin
  Result := StartAt;
  while (Result > 0) and (text[Result] <> SPACE) do Dec(Result);
end;
//------------------------------------------------------------------------------

function GetTextOutlineOnPath(const text: UnicodeString;
  const path: TPathD; font: TFontCache; textAlign: TTextAlign;
  x, y: double; charSpacing: double;
  out charsThatFit: integer; out outX: double): TPathsD;
var
  pathLen, pathLenMin1: integer;
  cummDists: TArrayOfDouble; // cummulative distances
  i, currentPathIdx: integer;
  textWidth, glyphCenterX, glyphCenterOnPath, dist, dx: double;
  glyph: PGlyphInfo;
  CharOffsets: TArrayOfDouble;
  unitVector: TPointD;
  tmpPaths: TPathsD;
begin
  Result := nil;
  pathLen := Length(path);
  pathLenMin1 := pathLen -1;
  charsThatFit := Length(text);
  if (pathLen < 2) or (charsThatFit = 0) then Exit;

  CharOffsets := font.GetGlyphOffsets(text, charSpacing);
  textWidth := CharOffsets[charsThatFit];
  setLength(cummDists, pathLen +1);
  cummDists[0] := 0;
  dist := 0;
  for i:= 1 to pathLen -1 do
  begin
    dist := dist + Distance(path[i-1], path[i]);
    cummDists[i] := dist;
  end;

  // truncate text that doesn't fit ...
  if textWidth > dist then
  begin
    Dec(charsThatFit);
    while CharOffsets[charsThatFit] > dist do Dec(charsThatFit);
    // if possible, break text at a SPACE char
    i := FindLastSpace(text, charsThatFit);
    if i > 0 then charsThatFit := i;
  end;

  case textAlign of
    taCenter: x := (dist - textWidth) * 0.5;
    taRight : x := dist - textWidth;
    // else use user defined starting x
  end;

  Result := nil;
  currentPathIdx := 0;
  for i := 1 to charsThatFit do
  begin
    glyph :=  font.GetGlyphInfo(Ord(text[i]));
    with glyph^ do
      glyphCenterX := (glyf.xMax - glyf.xMin) * font.Scale * 0.5;
    glyphCenterOnPath := x + glyphCenterX;
    while (currentPathIdx < pathLenMin1) and
      (cummDists[currentPathIdx +1] < glyphCenterOnPath) do
        inc(currentPathIdx);
    if currentPathIdx = pathLenMin1 then
    begin
      charsThatFit := i; // nb 1 base vs 0 base :)
      Break;
    end;
    x := x + glyph.hmtx.advanceWidth * font.Scale + charSpacing;
    unitVector := GetUnitVector(path[currentPathIdx], path[currentPathIdx +1]);
    tmpPaths := RotatePath(glyph.paths,
      PointD(glyphCenterX, -y), GetAngle(NullPointD, unitVector));
    dx := glyphCenterOnPath - cummDists[currentPathIdx];
    tmpPaths := TranslatePath(tmpPaths,
      path[currentPathIdx].X + unitVector.X * dx - glyphCenterX,
      path[currentPathIdx].Y + unitVector.Y * dx + y);
    AppendPath(Result, tmpPaths);
  end;
  outX := x;
end;
//------------------------------------------------------------------------------

function GetTextOutlineOnPath(const text: UnicodeString;
  const path: TPathD; font: TFontCache; textAlign: TTextAlign;
  perpendicOffset: integer; charSpacing: double;
  out charsThatFit: integer): TPathsD;
var
  dummy: double;
begin
  Result := GetTextOutlineOnPath(text, path, font, textAlign,
    0, perpendicOffset, charSpacing, charsThatFit, dummy);
end;
//------------------------------------------------------------------------------

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
  const path: TPathD; font: TFontCache; x, y: integer;
  charSpacing: double; out outX: double): TPathsD;
var
  dummy: integer;
begin
  Result := GetTextOutlineOnPath(text, path, font, taLeft,
    x, y, charSpacing, dummy, outX);
end;

//------------------------------------------------------------------------------
// TTextChunk class
//------------------------------------------------------------------------------

constructor TTextChunk.Create(owner: TChunkedText; const chunk: UnicodeString;
  index: integer; fontCache: TFontCache; fontColor, backColor: TColor32);
var
  i, listCnt: integer;
begin
  Self.owner := owner;
  listCnt := owner.fList.Count;
  if index < 0 then index := 0
  else if index > listCnt then index := listCnt;

  self.index := index;
  self.text  := chunk;
  self.fontColor := fontColor;
  self.backColor := backColor;

  if Assigned(fontCache) then
  begin
    fontCache.GetTextOutlineInternal(0,0,
      chunk, 0, self.arrayOfPaths, self.glyphOffsets, self.width);
    self.height := fontCache.LineHeight;
    self.ascent := fontCache.Ascent;
  end else
  begin
    self.arrayOfPaths := nil;
    SetLength(self.glyphOffsets, 1);
    self.glyphOffsets[0] := 0;
    self.width := 0;
    self.height := 0;
    self.ascent := 0;
  end;

  owner.fList.Insert(index, self);
  // reindex any trailing chunks
  if index < listCnt then
    for i := index +1 to listCnt do
      TTextChunk(owner.fList[i]).index := i;
end;

//------------------------------------------------------------------------------
// TChunkedText
//------------------------------------------------------------------------------

constructor TChunkedText.Create;
begin
  inherited;
{$IFDEF XPLAT_GENERICS}
  fList := TList<TTextChunk>.Create;
{$ELSE}
  fList := TList.Create;
{$ENDIF}
end;
//------------------------------------------------------------------------------

constructor TChunkedText.Create(const text: string; font: TFontCache;
  fontColor: TColor32; backColor: TColor32);
begin
  Create;
  SetText(text, font, fontColor, backColor);
end;
//------------------------------------------------------------------------------

destructor TChunkedText.Destroy;
begin
  Clear;
  fList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TChunkedText.GetChunk(index: integer): TTextChunk;
begin
  if (index < 0) or (index >= fList.Count) then
    raise Exception.Create(rsChunkedTextRangeError);
  Result :=  TTextChunk(fList.Items[index]);
end;
//------------------------------------------------------------------------------

function TChunkedText.GetText: UnicodeString;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Count -1 do
    Result := Result + TTextChunk(fList.Items[i]).text;
end;
//------------------------------------------------------------------------------

procedure TChunkedText.AddNewline(font: TFontCache);
var
  nlChunk: TTextChunk;
begin
  if not Assigned(font) or not font.IsValidFont then
    raise Exception.Create(rsChunkedTextFontError);
  if (fLastFont = font) then
  begin
    // this is much faster as it bypasses font.GetTextOutlineInternal
    nlChunk := InsertTextChunk(nil, MaxInt, #10, clNone32);
    nlChunk.height := fLastFont.LineHeight;
    nlChunk.ascent := fLastFont.Ascent;
  end else
  begin
    nlChunk := InsertTextChunk(font, MaxInt, SPACE, clNone32);
    nlChunk.text := #10;
    fSpaceWidth := nlChunk.width;
    fLastFont := font;
  end;
end;
//------------------------------------------------------------------------------

procedure TChunkedText.AddSpace(font: TFontCache);
var
  spaceChunk: TTextChunk;
begin
  if not Assigned(font) or not font.IsValidFont then
    raise Exception.Create(rsChunkedTextFontError);
  if (fLastFont = font) then
  begin
    // this is much faster as it bypasses font.GetTextOutlineInternal
    spaceChunk := InsertTextChunk(nil, MaxInt, SPACE, clNone32);
    spaceChunk.width := fSpaceWidth;
    spaceChunk.height := fLastFont.LineHeight;
    spaceChunk.ascent := fLastFont.Ascent;
  end else
  begin
    spaceChunk := InsertTextChunk(font, MaxInt, SPACE, clNone32);
    fLastFont := font;
    fSpaceWidth := spaceChunk.width;
  end;
end;
//------------------------------------------------------------------------------

function TChunkedText.AddTextChunk(font: TFontCache; const chunk: UnicodeString;
  fontColor: TColor32; backColor: TColor32): TTextChunk;
begin
  Result := InsertTextChunk(font, MaxInt, chunk, fontColor, backColor);
end;
//------------------------------------------------------------------------------

function TChunkedText.InsertTextChunk(font: TFontCache; index: integer;
  const chunk: UnicodeString; fontColor: TColor32;
  backColor: TColor32): TTextChunk;
begin
  Result := TTextChunk.Create(self, chunk, index, font, fontColor, backColor);
end;
//------------------------------------------------------------------------------

function TChunkedText.GetCount: integer;
begin
  Result := fList.Count;
end;
//------------------------------------------------------------------------------

procedure TChunkedText.Clear;
var
  i: integer;
begin
  for i := 0 to fList.Count -1 do
      TTextChunk(fList.Items[i]).Free;
  fList.Clear;
end;
//------------------------------------------------------------------------------

procedure TChunkedText.DeleteChunk(Index: Integer);
var
  i: integer;
begin
  if (index < 0) or (index >= fList.Count) then
    raise Exception.Create(rsChunkedTextRangeError);
  TTextChunk(fList.Items[index]).Free;
  fList.Delete(index);
  // reindex
  for i := Index to fList.Count -1 do
    dec(TTextChunk(fList.Items[i]).index);
end;
//------------------------------------------------------------------------------

procedure TChunkedText.DeleteChunkRange(startIdx, endIdx: Integer);
var
  i, cnt: Integer;
begin
  cnt := endIdx - startIdx +1;
  if (startIdx < 0) or (endIdx >= fList.Count) or (cnt <= 0) then
    raise Exception.Create(rsChunkedTextRangeError);

  for i := startIdx to endIdx do
    TTextChunk(fList.Items[i]).Free;
  // reindex
  for i := startIdx to fList.Count -1 do
    dec(TTextChunk(fList.Items[i]).index, cnt);
end;
//------------------------------------------------------------------------------

procedure TChunkedText.SetText(const text: UnicodeString;
  font: TFontCache; fontColor: TColor32; backColor: TColor32);
var
  len: integer;
  p, p2, pEnd: PWideChar;
  s: UnicodeString;
begin
  if not Assigned(font) then Exit;

  Clear;
  p := PWideChar(text);
  pEnd := p;
  Inc(pEnd, Length(text));
  while p < pEnd do
  begin
    if (p^ <= SPACE) then
    begin
      if (p^ = SPACE) then AddSpace(font)
      else if (p^ = #10) then AddNewline(font);
      inc(p);
    end else
    begin
      p2 := p;
      inc(p);
      while (p < pEnd) and (p^ > SPACE) do inc(p);
      len := p - p2;
      SetLength(s, len);
      Move(p2^, s[1], len * SizeOf(Char));
      AddTextChunk(font, s, fontColor, backColor);
    end;
  end;
end;
//------------------------------------------------------------------------------

function TChunkedText.GetPageMetrics(const rec: TRect; lineHeight: double;
  startingChunkIdx: integer): TPageTextMetrics;
var
  pageWidth, pageHeight : integer;
  lh, priorSplitWidth   : double;
  currentX              : double;
  arrayCnt, arrayCap    : integer;
  chunkIdxAtStartOfLine : integer;
  currentChunkIdx       : integer;
  linesFinished         : Boolean;

  procedure SetResultLength(len: integer);
  begin
    SetLength(Result.startOfLineIdx, len);
    SetLength(Result.justifyDeltas, len);
    SetLength(Result.lineWidths, len);
  end;

  procedure CheckArrayCap;
  begin
    if arrayCnt < arrayCap then Exit;
    inc(arrayCap, 16);
    SetResultLength(arrayCap);
  end;

  function IsRoomForCurrentLine: Boolean;
  begin
    Result := (arrayCnt + 1) * lh <= pageHeight;
  end;

  function CheckLineHeight(currentChunk: TTextChunk): Boolean;
  begin
    // unless a user-defined lineHeight has been assigned (lineHeight > 0),
    // get the largest lineHeight of all displayed chunks and use that
    // lineHeight for *every* line that's being displayed ...
    if (lineHeight = 0) and (currentChunk.height > lh) then
    begin
      // first make sure that this chunk will fit
      Result := (arrayCnt + 1) * currentChunk.height <= pageHeight;
      if Result then lh := currentChunk.height;
    end else
      Result := IsRoomForCurrentLine;
  end;

  procedure AddLine;
  var
    i, spcCnt, ChunkIdxAtEndOfLine: integer;
    x: double;
    chnk: TTextChunk;
  begin
    CheckArrayCap;
    ChunkIdxAtEndOfLine := currentChunkIdx -1;
    // ignore spaces at the end of lines
    while (ChunkIdxAtEndOfLine > chunkIdxAtStartOfLine) and
      (Chunk[ChunkIdxAtEndOfLine].text = SPACE) do
        Dec(ChunkIdxAtEndOfLine);

    x := -priorSplitWidth; spcCnt := 0;
    for i := chunkIdxAtStartOfLine to ChunkIdxAtEndOfLine do
    begin
      chnk := Chunk[i];
      if chnk.text = SPACE then inc(spcCnt);
      x := x + chnk.width;
    end;
    Result.lineWidths[arrayCnt] := x;
    Result.lineHeight := lh;
    Result.startOfLineIdx[arrayCnt] := chunkIdxAtStartOfLine;
    if spcCnt = 0 then
      Result.justifyDeltas[arrayCnt] := 0 else
      Result.justifyDeltas[arrayCnt] := (pageWidth - x)/spcCnt;
    inc(arrayCnt);
    chunkIdxAtStartOfLine := currentChunkIdx;
    currentX := 0;
    priorSplitWidth := 0;
  end;

  procedure AddSplitChunkLines(glyphOffset: integer);
  var
    highI: integer;
    residualWidth: double;
    chnk: TTextChunk;
  begin
    chnk := Chunk[chunkIdxAtStartOfLine];
    priorSplitWidth := chnk.glyphOffsets[glyphOffset];
    highI := High(chnk.glyphOffsets);
    residualWidth := chnk.width - priorSplitWidth;
    while (highI >= glyphOffset) and (residualWidth > pageWidth) do
    begin
      residualWidth := chnk.glyphOffsets[highI] - priorSplitWidth;
      Dec(highI);
    end;

    if highI < glyphOffset then
    begin
      // oops, even a single character won't fit !!
      linesFinished := true;
      currentChunkIdx := chunkIdxAtStartOfLine;
    end
    else if not IsRoomForCurrentLine then
    begin
      linesFinished := true;
      currentChunkIdx := chunkIdxAtStartOfLine;
    end
    else
    begin
      CheckArrayCap;
      Result.lineWidths[arrayCnt] := residualWidth;
      Result.lineHeight := lh;
      Result.startOfLineIdx[arrayCnt] := chunkIdxAtStartOfLine;
      Result.justifyDeltas[arrayCnt] := 0;
      if (highI = High(chnk.glyphOffsets)) then
      begin
        currentX := residualWidth;
        inc(currentChunkIdx);
      end else
      begin
        inc(arrayCnt);
        AddSplitChunkLines(highI +1); // note recursion
      end;
    end;
  end;

var
  chnk: TTextChunk;
begin
  FillChar(Result, SizeOf(Result), 0);
  arrayCnt := 0; arrayCap := 0;
  if (startingChunkIdx < 0) then startingChunkIdx := 0;
  if (Count = 0) or (startingChunkIdx >= Count) then Exit;

  lh := lineHeight;
  RectWidthHeight(rec, pageWidth, pageHeight);
  currentChunkIdx := startingChunkIdx;
  chunkIdxAtStartOfLine := startingChunkIdx;
  currentX := 0;
  priorSplitWidth := 0;
  linesFinished := false;
  while (currentChunkIdx < Count) do
  begin
    chnk := Chunk[currentChunkIdx];
    if not CheckLineHeight(chnk) then break;

    if (chnk.text = #10) then
    begin
      AddLine;
      if arrayCnt > 0 then
        Result.justifyDeltas[arrayCnt-1] := 0;
      inc(currentChunkIdx);
      chunkIdxAtStartOfLine := currentChunkIdx;
    end
    else if (currentX + chnk.width > pageWidth) then
    begin
      if (currentChunkIdx = chunkIdxAtStartOfLine) then
      begin
        // a single chunk is too wide for 'pageWidth'
        AddSplitChunkLines(0);
        if linesFinished or (currentChunkIdx = Count) then Break;
      end else
      begin
        AddLine;
        // don't allow spaces to wrap to the front of the following line
        while (currentChunkIdx < Count) and
          (self.chunk[currentChunkIdx].text = SPACE) do
            inc(currentChunkIdx);
        chunkIdxAtStartOfLine := currentChunkIdx;
      end;
    end else
    begin
      currentX := currentX + chnk.width;
      inc(currentChunkIdx);
    end;
  end;

  if not linesFinished and
    (currentChunkIdx > chunkIdxAtStartOfLine) then AddLine;
  Result.lineCount := arrayCnt;
  SetResultLength(arrayCnt);
  Result.nextChuckIdx := currentChunkIdx;
  if (arrayCnt > 0) and (Result.nextChuckIdx = Count) then
    Result.justifyDeltas[arrayCnt-1] := 0;
end;
//------------------------------------------------------------------------------

function TChunkedText.GetChunkAndGlyphOffsetAtPt(const ptm: TPageTextMetrics;
  const pt: TPoint; out glyphIdx, chunkChrOff: integer): Boolean;
var
  x,y, maxY, maxIdx: integer;
  x2  : Double;
  chnk: TTextChunk;
begin
  Result := false;
  x := pt.X - ptm.bounds.Left;
  y := Trunc((pt.Y - ptm.bounds.Top - ptm.topLinePxOffset) / ptm.lineHeight);
  maxY := ptm.lineCount -1;

  if (x < 0) or (x > ptm.bounds.right - ptm.bounds.Left) or
    (y < 0) or (y > maxY) then Exit;

  if y = maxY then
    maxIdx := ptm.nextChuckIdx -1 else
    maxIdx := ptm.startOfLineIdx[y +1] -1;

  glyphIdx := ptm.startOfLineIdx[y];
  chunkChrOff := 0;
  x2 := x;

  // get chunkIdx within line 'y' ...
  while (glyphIdx < maxIdx) do
  begin
    if Chunk[glyphIdx].text = space then
    begin
      if x2 < Chunk[glyphIdx].width + ptm.justifyDeltas[y] then Break;
      x2 := x2 - Chunk[glyphIdx].width - ptm.justifyDeltas[y];
    end else
    begin
      if x2 < Chunk[glyphIdx].width then Break;
      x2 := x2 - Chunk[glyphIdx].width;
    end;
    inc(glyphIdx);
  end;

  // get chunkChrOffset within Chunk[chunkIdx] ...
  chnk := Chunk[glyphIdx];
  while x2 >= chnk.glyphOffsets[chunkChrOff +1] do Inc(chunkChrOff);
  Result := true;
end;
//------------------------------------------------------------------------------

function TChunkedText.GetGlyphsOrDrawInternal(image: TImage32; const rec: TRect;
  textAlign: TTextAlign; textAlignV: TTextVAlign; startChunk: integer;
  lineHeight: double; out paths: TPathsD): TPageTextMetrics;
var
  i,j, highJ,k, recWidth, recHeight: integer;
  a,b, chrIdx, lastLine: integer;
  x,y, totalHeight, lineWidth, spcDx: double;
  consumedWidth: double;
  pp: TPathsD;
  top: double;
  chnk: TTextChunk;
begin
  paths := nil;
  FillChar(Result, SizeOf(Result), 0);
  Result.nextChuckIdx := startChunk;
  if Count = 0 then Exit;

  RectWidthHeight(rec, recWidth, recHeight);

  // LINE HEIGHTS ...............
  // Getting lineheights based on a given font's ascent and descent values
  // works well only when a single font is used. Unfortunately, when using
  // multiple fonts, line spacing becomes uneven and looks ugly.
  // An alternative approach is to measure the highest and lowest bounds of all
  // the glyphs in a line, and use these and a fixed inter line space
  // to derive variable line heights. But this approach also has problems,
  // especially when lines contain no glyphs, or when they only contain glyphs
  // with minimal heights (----------). So this too can look ugly.
  // A third approach, is to get the maximum of every lines' height and use
  // that value for every line. But this approach tends to produce undesirably
  // large line heights.
  // A fourth approach is to use the height of the very first text chunk.
  // And a final approach ia simply to use a user defined line height

  if lineHeight = 0 then
    lineHeight := Chunk[0].height;
  Result := GetPageMetrics(rec, lineHeight, startChunk);
  if (Result.lineCount = 0) or (lineHeight > recHeight) then Exit;

  // only return glyphs for visible lines
  totalHeight := lineHeight * Result.lineCount;

  i := Result.startOfLineIdx[0];
  top := rec.Top + Chunk[i].ascent;

  case textAlignV of
    tvaMiddle: y := top + (RecHeight - totalHeight) /2 -1;
    tvaBottom: y := rec.bottom - totalHeight + Chunk[i].ascent;
    else y := top;
  end;

  Result.bounds := rec;
  Result.topLinePxOffset := Round(y - top);
  chrIdx := 0;
  lastLine := Result.lineCount -1;
  for i := 0 to lastLine do
  begin
    a := Result.startOfLineIdx[i];
    if i = lastLine then
    begin
      if (chunk[a].width - chunk[a].glyphOffsets[chrIdx] > recWidth) then
        b := a -1 // flag getting glyphs for a partial chunk
      else if Result.nextChuckIdx = 0 then
        b := Count -1
      else
        b := Result.nextChuckIdx -1;
    end else
      b := Result.startOfLineIdx[i+1] -1;

    if textAlign = taJustify then
      spcDx := Result.justifyDeltas[i] else
      spcDx := 0;
    lineWidth := Result.lineWidths[i];

    if (b < a) then
    begin
      // chunk[a] width exceeds recWidth
      chnk := chunk[a];
      consumedWidth := chnk.glyphOffsets[chrIdx];
      highJ := High(chnk.glyphOffsets);
      j := chrIdx;
      while (j < highJ) and
        (chnk.glyphOffsets[j+1] -consumedWidth < lineWidth) do inc(j);
      pp := nil;
      for k := chrIdx to j do
        AppendPath(pp, chnk.arrayOfPaths[k]);
      pp := TranslatePath(pp, rec.Left - consumedWidth, y);
      chnk.left := rec.Left;
      chnk.top := y - chnk.ascent;

      if Assigned(image) then
      begin
        if Assigned(fDrawChunkEvent) then
          fDrawChunkEvent(chnk, RectD(rec.Left, chnk.top,
            rec.Left + consumedWidth, chnk.top + chnk.height));

        DrawPolygon(image, pp, frNonZero, chnk.fontColor);
      end else
        AppendPath(paths, pp);

      y := y + lineHeight;
      chrIdx := j +1;
      Continue;
    end
    else if chrIdx > 0 then
    begin
      // finish the partially processed chunk before continuing to next one
      chnk := chunk[a];
      highJ := High(chnk.glyphOffsets);
      consumedWidth := chnk.glyphOffsets[chrIdx];
      j := chrIdx;
      while (j < highJ) and
        (chnk.glyphOffsets[j+1] -consumedWidth < lineWidth) do inc(j);
      pp := nil;
      for k := chrIdx to j do
        AppendPath(pp, chnk.arrayOfPaths[k]);
      pp := TranslatePath(pp, rec.Left - consumedWidth, y);

      if Assigned(image) then
        DrawPolygon(image, pp, frNonZero, chnk.fontColor) else
        AppendPath(paths, pp);

      if (j = chrIdx) and (j < highJ) then
        break // oops, even a character is too wide for 'rec' !
      else if j < HighJ then
      begin
        chrIdx := j;
        Continue;
      end else
      begin
        chrIdx := 0;
        x := rec.Left + chnk.width - consumedWidth;
        inc(a);
      end;
    end else
    begin
      case textAlign of
        taRight   : x := rec.Left + (recWidth - lineWidth);
        taCenter  : x := rec.Left + (recWidth - lineWidth) / 2;
        else        x := rec.Left;
      end;
    end;

    // ignore trailing spaces
    while (b >= a) do
      if Chunk[b].text <= SPACE then
        dec(b) else
        break;

    for j := a to b do
    begin
      chnk := GetChunk(j);
      chnk.left := x;
      chnk.top := y - chnk.ascent;

      if chnk.text > SPACE then
      begin
        pp := MergeArrayOfPathsEx(chnk.arrayOfPaths, x, y);

        if Assigned(image) then
        begin
          if (GetAlpha(chnk.backColor) > 0) then
            image.FillRect(Img32.Vector.Rect(RectD(x, chnk.top,
                x + chnk.width, chnk.top + chnk.height)), chnk.backColor);

          if Assigned(fDrawChunkEvent) then
            fDrawChunkEvent(chnk, RectD(x, chnk.top,
              x + chnk.width, chnk.top + chnk.height));

          DrawPolygon(image, pp, frNonZero, chnk.fontColor);
        end else
          AppendPath(paths, pp);

        x := x + chnk.width;
      end else
      begin
        if (GetAlpha(chnk.backColor) > 0) then
          image.FillRect(Img32.Vector.Rect(RectD(x, chnk.top,
              x + chnk.width + spcDx, chnk.top + chnk.height)),
              chnk.backColor);

        if Assigned(image) and Assigned(fDrawChunkEvent) then
          fDrawChunkEvent(chnk, RectD(x, chnk.top,
            x + chnk.width + spcDx, chnk.top + chnk.height));

        x := x + chnk.width + spcDx;
      end;
    end;
    y := y + lineHeight;
  end;
end;
//------------------------------------------------------------------------------

function TChunkedText.DrawText(image: TImage32; const rec: TRect;
  textAlign: TTextAlign; textAlignV: TTextVAlign;
  startChunk: integer; lineHeight: double): TPageTextMetrics;
var
  dummy: TPathsD;
begin
  Result := GetGlyphsOrDrawInternal(image,
    rec, textAlign, textAlignV, startChunk, lineHeight, dummy);
end;
//------------------------------------------------------------------------------

function TChunkedText.GetTextGlyphs(const rec: TRect;
  textAlign: TTextAlign; textAlignV: TTextVAlign; startChunk: integer;
  lineHeight: double = 0.0): TPathsD;
begin
  GetGlyphsOrDrawInternal(nil, rec, textAlign, textAlignV,
    startChunk, lineHeight, Result);
end;
//------------------------------------------------------------------------------

procedure TChunkedText.ApplyNewFont(font: TFontCache);
var
  i: integer;
begin
  if not Assigned(font) then Exit;
  for i := 0 to Count -1 do
    with Chunk[i] do
    begin
      font.GetTextOutlineInternal(0,0,
        text, 0, arrayOfPaths, glyphOffsets, width);
      height := font.LineHeight;
      ascent := font.Ascent;
    end;
end;

//------------------------------------------------------------------------------
// TFontManager
//------------------------------------------------------------------------------

constructor TFontManager.Create;
begin
  fMaxFonts := 32;
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

function TFontManager.FindDuplicate(fr: TFontReader): integer;
var
  fi, fi2: TFontInfo;
begin
  fi := fr.FontInfo;
  for Result := 0 to fFontList.Count -1 do
  begin
    fi2 := TFontReader(fFontList[Result]).FontInfo;
    if SameText(fi.fullFaceName, fi2.fullFaceName) and
      (fi.macStyles = fi2.macStyles) then Exit;
    end;
  Result := -1;
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function TFontManager.LoadFontReaderFamily(const fontFamily: string): TLoadFontResult;
var
  frf: TFontReaderFamily;
begin
  Result := LoadFontReaderFamily(fontFamily, frf);
end;
//------------------------------------------------------------------------------

function TFontManager.LoadFontReaderFamily(const fontFamily: string;
  out fontReaderFamily: TFontReaderFamily): TLoadFontResult;
var
  arrayEnumLogFont: TArrayOfEnumLogFontEx;
  lf: TLogFont;
  fontInfo: TFontInfo;

  function FontInfoNamesAndSytlesMatch(const fontInfo1, fontInfo2: TFontInfo): Boolean;
  begin
    Result := (fontInfo1.faceName = fontInfo2.faceName) and
      (fontInfo1.macStyles = fontInfo2.macStyles);
  end;

begin
  Result := lfrInvalid;
  fontReaderFamily.regularFR := nil;
  fontReaderFamily.boldFR := nil;
  fontReaderFamily.italicFR := nil;
  fontReaderFamily.boldItalicFR := nil;

  if (fontFamily = '') or (Length(fontFamily) > LF_FACESIZE) then Exit;
  arrayEnumLogFont := GetLogFonts(fontFamily, DEFAULT_CHARSET); //ANSI_CHARSET);

  FillChar(lf, SizeOf(TLogFont), 0);
  Move(fontFamily[1], lf.lfFaceName[0], Length(fontFamily) * SizeOf(Char));
  if not GetLogFontFromEnumThatMatchesStyles(arrayEnumLogFont, [], lf) then Exit;

  // make room for 4 new fontreaders
  while fFontList.Count > fMaxFonts - 4 do DeleteOldestFont;

  fontReaderFamily.regularFR := TFontReader.Create;
  fontReaderFamily.regularFR.Load(lf);

  Result := ValidateFontLoad(fontReaderFamily.regularFR);
  case Result of
    lfrInvalid: Exit;
    lfrDuplicate:
      begin
        fontInfo := fontReaderFamily.regularFR.FontInfo;

        fontInfo.macStyles := [msBold];
        fontReaderFamily.boldFR := GetBestMatchFont(fontInfo);
        if not FontInfoNamesAndSytlesMatch(FontInfo,
          fontReaderFamily.boldFR.FontInfo) then
            fontReaderFamily.boldFR := nil;

        fontInfo.macStyles := [msItalic];
        fontReaderFamily.italicFR := GetBestMatchFont(fontInfo);
        if not FontInfoNamesAndSytlesMatch(FontInfo,
          fontReaderFamily.italicFR.FontInfo) then
            fontReaderFamily.italicFR := nil;

        fontInfo.macStyles := [msBold, msItalic];
        fontReaderFamily.boldItalicFR := GetBestMatchFont(fontInfo);
        if not FontInfoNamesAndSytlesMatch(FontInfo,
          fontReaderFamily.boldItalicFR.FontInfo) then
            fontReaderFamily.boldItalicFR := nil;
      end;
    else
      begin
        if GetLogFontFromEnumThatMatchesStyles(arrayEnumLogFont, [msBold], lf) then
        begin
          fontReaderFamily.boldFR := TFontReader.Create;
          fontReaderFamily.boldFR.Load(lf);
          ValidateFontLoad(fontReaderFamily.boldFR);
        end;
        if GetLogFontFromEnumThatMatchesStyles(arrayEnumLogFont, [msItalic], lf) then
        begin
          fontReaderFamily.italicFR := TFontReader.Create;
          fontReaderFamily.italicFR.Load(lf);
          ValidateFontLoad(fontReaderFamily.italicFR);
        end;

        if GetLogFontFromEnumThatMatchesStyles(arrayEnumLogFont, [msBold, msItalic], lf) then
        begin
          fontReaderFamily.boldItalicFR := TFontReader.Create;
          fontReaderFamily.boldItalicFR.Load(lf);
          ValidateFontLoad(fontReaderFamily.boldItalicFR);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

function TFontManager.LoadFontReader(const fontName: string): TFontReader;
begin
  Result := nil;
  if (fontName = '') or (Length(fontName) > LF_FACESIZE) then Exit;
  if fFontList.Count >= fMaxFonts then DeleteOldestFont;
  Result := TFontReader.Create(fontName);
  ValidateFontLoad(Result);
end;
//------------------------------------------------------------------------------
{$ENDIF}

function TFontManager.LoadFromStream(stream: TStream): TFontReader;
begin
  if fFontList.Count >= fMaxFonts then DeleteOldestFont;

  Result := TFontReader.Create;
  try
    if not Result.LoadFromStream(stream) then FreeAndNil(Result)
    else ValidateFontLoad(Result);
  except
    FreeAndNil(Result);
  end;
end;
//------------------------------------------------------------------------------

function TFontManager.LoadFromResource(const resName: string; resType: PChar): TFontReader;
begin
  if fFontList.Count >= fMaxFonts then DeleteOldestFont;

  Result := TFontReader.Create;
  try
    if not Result.LoadFromResource(resName, resType) then FreeAndNil(Result)
    else ValidateFontLoad(Result);
  except
    FreeAndNil(Result);
  end;
end;
//------------------------------------------------------------------------------

function TFontManager.LoadFromFile(const filename: string): TFontReader;
begin
  if fFontList.Count >= fMaxFonts then DeleteOldestFont;

  Result := TFontReader.Create;
  try
    if not Result.LoadFromFile(filename) then FreeAndNil(Result)
    else ValidateFontLoad(Result);
  except
    FreeAndNil(Result);
  end;
end;
//------------------------------------------------------------------------------

function TFontManager.ValidateFontLoad(var fr: TFontReader): TLoadFontResult;
var
  dupIdx: integer;
begin
  if not fr.IsValidFontFormat then
  begin
    FreeAndNil(fr);
    result := lfrInvalid;
    Exit;
  end;
  dupIdx := FindDuplicate(fr);
  if dupIdx >= 0 then
  begin
    FreeAndNil(fr);
    result := lfrDuplicate;
    fr := fFontList[dupIdx];
  end else
  begin
    Result := lfrSuccess;
    fFontList.Add(fr);
    fr.fFontManager := self;
  end;
end;
//------------------------------------------------------------------------------

function TFontManager.Delete(fontReader: TFontReader): Boolean;
var
  i: integer;
begin
  for i := 0 to fFontList.Count -1 do
    if TFontReader(fFontList[i]) = fontReader then
    begin
      // make sure the FontReader object isn't destroying itself externally
      if not fontReader.fDestroying then fontReader.Free;
      fFontList.Delete(i);
      Result := true;
      Exit;
    end;
  Result := false;
end;
//------------------------------------------------------------------------------

function StylesToInt(macstyles: TMacStyles): integer;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  if msBold in macStyles then
    Result := 1 else Result := 0;
  if msItalic in macStyles then inc(Result, 2);
end;
//------------------------------------------------------------------------------

function FontFamilyToInt(family: TFontFamily): integer;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := Ord(family) +1;
end;
//------------------------------------------------------------------------------

function TFontManager.GetBestMatchFont(const fontInfo: TFontInfo): TFontReader;

  function GetStyleDiff(const macstyles1, macstyles2: TMacStyles): integer;
    {$IFDEF INLINE} inline; {$ENDIF}
  begin
    // top priority
    Result := (((Byte(macstyles1) xor $FF) or
      (Byte(macstyles2) xor $FF)) and $3) * 256;
  end;

  function GetFontFamilyDiff(const family1, family2: TFontFamily): integer;
    {$IFDEF INLINE} inline; {$ENDIF}
  begin
    // second priority
    if family1 = tfUnknown then
      Result := 0 else
      Result := Abs(FontFamilyToInt(family1) - FontFamilyToInt(family2)) * 8;
  end;

  function GetShortNameDiff(const name1, name2: Utf8String): integer;
    {$IFDEF INLINE} inline; {$ENDIF}
  begin
    // third priority (shl 3)
    if name1 = '' then
      Result := 0 else
    if SameText(name1, name2) then Result := 0 else Result := 4;
  end;

  function GetFullNameDiff(const fiToMatch: TFontInfo;
    const candidateName: Utf8String): integer;
  var
    i: integer;
  begin
    // lowest priority
    Result := 0;
    if Assigned(fiToMatch.familyNames) then
    begin
      for i := 0 to High(fiToMatch.familyNames) do
        if SameText(fiToMatch.familyNames[i], candidateName) then Exit;
    end
    else if SameText(fiToMatch.faceName, candidateName) then Exit;
    Result := 2;
  end;

  function CompareFontInfos(const fiToMatch, fiCandidate: TFontInfo): integer;
  begin
    Result :=
      GetStyleDiff(fiToMatch.macStyles, fiCandidate.macStyles) +
      GetFontFamilyDiff(fiToMatch.family, fiCandidate.family) +
      GetShortNameDiff(fiToMatch.faceName, fiCandidate.faceName) +
      GetFullNameDiff(fiToMatch, fiCandidate.fullFaceName);
  end;

var
  i, bestDiff, currDiff: integer;
  fr: TFontReader;
begin
  Result := nil;
  bestDiff := MaxInt;
  for i := 0 to fFontList.Count -1 do
  begin
    fr := TFontReader(fFontList[i]);
    currDiff := CompareFontInfos(fontInfo, fr.fFontInfo);
    if (currDiff < bestDiff) then
    begin
      Result := fr;
      if currDiff = 0 then Break; // can't do better :)
      bestDiff := currDiff;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TFontManager.GetBestMatchFont(const styles: TMacStyles): TFontReader;
var
  i, bestDiff, currDiff: integer;
  fr: TFontReader;
begin
  Result := nil;
  bestDiff := MaxInt;
  for i := 0 to fFontList.Count -1 do
  begin
    fr := TFontReader(fFontList[i]);
    currDiff := (((Byte(styles) xor $FF) or (Byte(fr.fFontInfo.macStyles) xor $FF)) and $3);
    if (currDiff < bestDiff) then
    begin
      Result := fr;
      if currDiff = 0 then Break; // can't do any better :)
      bestDiff := currDiff;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TFontManager.FindReaderContainingGlyph(codepoint: Cardinal;
  fntFamily: TFontFamily; out glyphIdx: WORD): TFontReader;
var
  i: integer;
  reader: TFontReader;
begin
  result := nil;
  for i := 0 to fFontList.Count -1 do
  begin
    reader := TFontReader(fFontList[i]);
    glyphIdx := reader.GetGlyphIdxUsingCmap(codepoint);
    // if a font family is specified, then only return true
    // when finding the glyph within that font family
    if (glyphIdx > 0) and ((fntFamily = tfUnknown) or
      (reader.FontFamily = tfUnknown) or (fntFamily = reader.FontFamily)) then
    begin
      Result := reader;
      Exit;
    end;
  end;
  glyphIdx := 0;
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

function FontSorterProc(fontreader1, fontreader2: Pointer): integer;
var
  fr1: TFontReader absolute fontreader1;
  fr2: TFontReader absolute fontreader2;
begin
  if fr1.fLastUsedTime > fr2.fLastUsedTime then Result := -1
  else if fr1.fLastUsedTime < fr2.fLastUsedTime then Result := 1
  else Result := 0;
end;
//------------------------------------------------------------------------------

procedure TFontManager.SortFontListOnLastUse;
begin
{$IFDEF XPLAT_GENERICS}
  fFontList.Sort(TComparer<TFontReader>.Construct(
    function (const fr1, fr2: TFontReader): integer
    begin
      if fr1.fLastUsedTime > fr2.fLastUsedTime then Result := -1
      else if fr1.fLastUsedTime < fr2.fLastUsedTime then Result := 1
      else Result := 0;
    end));
{$ELSE}
  fFontList.Sort(FontSorterProc);
{$ENDIF}
end;
//------------------------------------------------------------------------------

procedure TFontManager.DeleteOldestFont;
var
  cnt: integer;
begin
  cnt := fFontList.Count;
  if cnt = 0 then Exit;
  SortFontListOnLastUse;
  TFontReader(fFontList[cnt -1]).Free;
end;
//------------------------------------------------------------------------------

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
