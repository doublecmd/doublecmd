unit Img32.SVG.Core;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.7                                                             *
* Date      :  12 January 2025                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
*                                                                              *
* Purpose   :  Essential structures and functions to read SVG files            *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Types, Math, StrUtils,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Img32, Img32.Vector, Img32.Text, Img32.Transform;

{$IFDEF ZEROBASEDSTR}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

type
  TSvgEncoding = (eUnknown, eUtf8, eUnicodeLE, eUnicodeBE);

  TUnitType = (utUnknown, utNumber, utPercent, utEm, utEx, utPixel,
    utCm, utMm, utInch, utPt, utPica, utDegree, utRadian);

  //////////////////////////////////////////////////////////////////////
  // TValue - Structure to store numerics with measurement units.
  // See https://www.w3.org/TR/SVG/types.html#InterfaceSVGLength
  // and https://www.w3.org/TR/SVG/types.html#InterfaceSVGAngle
  //////////////////////////////////////////////////////////////////////

  //Unfortunately unit-less values can exhibit ambiguity, especially when their
  //values are small (eg < 1.0). These values can be either absolute values or
  //relative values (ie relative to the supplied dimension size).
  //The 'assumeRelValBelow' parameter (see below) attempts to address this
  //ambiguity, such that unit-less values will be assumed to be 'relative' when
  //'rawVal' is less than the supplied 'assumeRelValBelow' value.

  TValue = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    rawVal    : double;
    unitType  : TUnitType;
    procedure Init;
    procedure SetValue(val: double; unitTyp: TUnitType = utNumber);
    function  GetValue(relSize: double; assumeRelValBelow: Double): double;
    function  GetValueXY(const relSize: TRectD; assumeRelValBelow: Double): double;
    function  IsValid: Boolean;
    function  IsRelativeValue(assumeRelValBelow: double): Boolean;
      {$IFDEF INLINE} inline; {$ENDIF}
    function  HasFontUnits: Boolean;
    function  HasAngleUnits: Boolean;
  end;

  TValuePt = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    X, Y    : TValue;
    procedure Init;
    function  GetPoint(const relSize: double; assumeRelValBelow: Double): TPointD; overload;
    function  GetPoint(const relSize: TRectD; assumeRelValBelow: Double): TPointD; overload;
    function  IsValid: Boolean;
  end;

  TValueRecWH = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    left    : TValue;
    top     : TValue;
    width   : TValue;
    height  : TValue;
    procedure Init;
    function  GetRectD(const relSize: TRectD; assumeRelValBelow: Double): TRectD; overload;
    function  GetRectD(relSize: double; assumeRelValBelow: Double): TRectD; overload;
    function  GetRectD(relSizeX, relSizeY: double; assumeRelValBelow: Double): TRectD; overload;
    function  GetRectWH(const relSize: TRectD; assumeRelValBelow: Double): TRectWH;
    function  IsValid: Boolean;
    function  IsEmpty: Boolean;
  end;

  {$IFNDEF UNICODE}
  UTF8Char  = Char;
  PUTF8Char = PChar;
  {$ELSE}
  {$IF COMPILERVERSION < 31}
  UTF8Char = AnsiChar;
  PUTF8Char = PAnsiChar;
  {$IFEND}
  {$ENDIF}

  TSvgItalicSyle  = (sfsUndefined, sfsNone, sfsItalic);
  TFontDecoration = (fdUndefined, fdNone, fdUnderline, fdStrikeThrough);
  TSvgTextAlign = (staUndefined, staLeft, staCenter, staRight, staJustify);
  TSpacesInText = (sitUndefined, sitIgnore, sitPreserve);

  UTF8Strings = array of UTF8String;

  TSVGFontInfo = record
    family        : TFontFamily;
    familyNames   : UTF8Strings;
    size          : double;
    spacing       : double;
    spacesInText  : TSpacesInText;
    textLength    : double;
    italic        : TSvgItalicSyle;
    weight        : Integer;
    align         : TSvgTextAlign;
    decoration    : TFontDecoration;
    baseShift     : TValue;
  end;

  //////////////////////////////////////////////////////////////////////
  // TClassStylesList: Map that stores CSS selectors with their styles
  //////////////////////////////////////////////////////////////////////

  PClassStyleListItem = ^TClassStyleListItem;
  TClassStyleListItem = record //used internally by TClassStylesList
    Hash  : Cardinal;
    Next  : Integer;
    Name  : UTF8String;
    Style : UTF8String;
  end;

  TClassStylesList = class
  private
    FNameHash: Cardinal;
    FItems: array of TClassStyleListItem;
    FBuckets: TArrayOfInteger;
    FCount: Integer;
    FMod: Cardinal;
    procedure Grow(NewCapacity: Integer = -1);
    function FindItemIndex(const Name: UTF8String): Integer;
  public
    procedure Preallocate(AdditionalItemCount: Integer);
    procedure AddAppendStyle(const Name, Style: UTF8String);
    function GetStyle(const Name: UTF8String): UTF8String;
    procedure Clear;
  end;

  //////////////////////////////////////////////////////////////////////
  // TSvgParser and associated classes - a simple parser for SVG xml
  //////////////////////////////////////////////////////////////////////

  PSvgAttrib = ^TSvgAttrib;   //element attribute
  TSvgAttrib = record
    hash      : Cardinal;     //hashed name
    name      : UTF8String;
    value     : UTF8String;
  end;

  TSvgParser = class;

  TXmlEl = class              //base element class
  private
    {$IFDEF XPLAT_GENERICS}
    attribs     : TList <PSvgAttrib>;
    {$ELSE}
    attribs     : TList;
    {$ENDIF}
    function GetAttrib(index: integer): PSvgAttrib;
    function GetAttribCount: integer;
  public
    {$IFDEF XPLAT_GENERICS}
    childs      : TList<TXmlEl>;
    {$ELSE}
    childs      : TList;
    {$ENDIF}
    name        : UTF8String;
    owner       : TSvgParser;
    hash        : Cardinal;
    text        : UTF8String;
    selfClosed  : Boolean;
    constructor Create(owner: TSvgParser); virtual;
    destructor  Destroy; override;
    procedure   Clear; virtual;
    function    ParseHeader(var c: PUTF8Char; endC: PUTF8Char): Boolean; virtual;
    function    ParseContent(var c: PUTF8Char; endC: PUTF8Char): Boolean; virtual;
    class function ParseAttribName(c, endC: PUTF8Char; attrib: PSvgAttrib): PUTF8Char; {$IFDEF CLASS_STATIC}static;{$ENDIF}
    class function ParseAttribValue(c, endC: PUTF8Char; attrib: PSvgAttrib): PUTF8Char; {$IFDEF CLASS_STATIC}static;{$ENDIF}
    class function ParseAttribNameAndValue(c, endC: PUTF8Char; attrib: PSvgAttrib): PUTF8Char; {$IFDEF CLASS_STATIC}static;{$ENDIF}
    function    ParseAttributes(var c: PUTF8Char; endC: PUTF8Char): Boolean; virtual;
    procedure   ParseStyleAttribute(const style: UTF8String);
    property    Attrib[index: integer]: PSvgAttrib read GetAttrib;
    property    AttribCount: integer read GetAttribCount;
  end;

  TDocTypeEl = class(TXmlEl)
  private
    function    SkipWord(c, endC: PUTF8Char): PUTF8Char;
    function    ParseEntities(var c, endC: PUTF8Char): Boolean;
  public
    function    ParseAttributes(var c: PUTF8Char; endC: PUTF8Char): Boolean; override;
  end;

  TSvgXmlEl = class(TXmlEl)
  public
    constructor Create(owner: TSvgParser); override;
    procedure   Clear; override;
    function    ParseHeader(var c: PUTF8Char; endC: PUTF8Char): Boolean; override;
  end;

  TSvgParser = class
  private
    svgStream : TMemoryStream;
    procedure ParseUtf8Stream;
  public
    classStyles : TClassStylesList;
    xmlHeader   : TXmlEl;
    docType     : TDocTypeEl;
    svgTree     : TSvgXmlEl;
    constructor Create;
    destructor  Destroy; override;
    procedure Clear;
    function  FindEntity(hash: Cardinal): PSvgAttrib;
    function  LoadFromFile(const filename: string): Boolean;
    function  LoadFromStream(stream: TStream): Boolean;
    function  LoadFromString(const str: string): Boolean;
  end;

  //////////////////////////////////////////////////////////////////////
  // Miscellaneous SVG functions
  //////////////////////////////////////////////////////////////////////

  //general parsing functions //////////////////////////////////////////
  function ParseNextWord(var c: PUTF8Char; endC: PUTF8Char;
    out word: UTF8String): Boolean;
  function ParseNextWordHash(var c: PUTF8Char; endC: PUTF8Char;
    out hash: cardinal): Boolean; overload;
  function ParseNextWordHash(c, endC: PUTF8Char): cardinal; overload;
  function ParseNextWordExHash(var c: PUTF8Char; endC: PUTF8Char;
    out hash: cardinal): Boolean;
  function ParseNextNum(var c: PUTF8Char; endC: PUTF8Char;
    skipComma: Boolean; out val: double): Boolean;
  function ParseNextNumEx(var c: PUTF8Char; endC: PUTF8Char; skipComma: Boolean;
    out val: double; out unitType: TUnitType): Boolean;
  function GetHash(c: PUTF8Char; len: nativeint): cardinal; overload;
  function GetHash(const name: UTF8String): cardinal; overload; {$IFDEF INLINE} inline; {$ENDIF}
  function GetHashCaseSensitive(name: PUTF8Char; nameLen: integer): cardinal;
  function ExtractRef(const href: UTF8String): UTF8String;
  function IsNumPending(var c: PUTF8Char;
    endC: PUTF8Char; ignoreComma: Boolean): Boolean;
  function UTF8StringToColor32(const value: UTF8String; var color: TColor32): Boolean;
  function ScaleDashArray(const dblArray: TArrayOfDouble; scale: double): TArrayOfDouble;
  function Match(c: PUTF8Char; const compare: UTF8String): Boolean; overload;
  function Match(const compare1, compare2: UTF8String): Boolean; overload;
  function PosEx(const subStr: utf8String; const text: Utf8String; startIdx: integer = 1): integer;
  procedure ToUTF8String(c, endC: PUTF8Char; var S: UTF8String;
    spacesInText: TSpacesInText = sitUndefined);
  function TrimMultiSpacesUtf8(const text: Utf8String): Utf8String;
  function TrimMultiSpacesUnicode(const text: UnicodeString): UnicodeString;
  function ConvertNewlines(const s: UTF8String): UTF8String; overload;
  function ConvertNewlines(const s: UnicodeString): UnicodeString; overload;
  function StripNewlines(const s: UTF8String): UTF8String; overload;
  function StripNewlines(const s: UnicodeString): UnicodeString; overload;
  procedure ToAsciiLowerUTF8String(c, endC: PUTF8Char; var S: UTF8String);
  procedure ToTrimmedUTF8String(c, endC: PUTF8Char; var S: UTF8String);
  function IsSameUTF8String(const S1, S2: UTF8String): Boolean;

  //special parsing functions //////////////////////////////////////////
  procedure ParseStyleElementContent(const value: UTF8String; stylesList: TClassStylesList);
  function ParseTransform(const transform: UTF8String): TMatrixD;

  procedure GetSvgFontInfo(const value: UTF8String; var fontInfo: TSVGFontInfo);
  function HtmlDecode(const html: UTF8String): UTF8String;

  function GetXmlEncoding(memory: Pointer; len: integer): TSvgEncoding;
  function ClampRange(val, min, max: double): double;

  function SkipBlanks(var c: PUTF8Char; endC: PUTF8Char): Boolean;
  function SkipBlanksEx(c: PUTF8Char; endC: PUTF8Char): PUTF8Char; {$IFDEF INLINE} inline; {$ENDIF}
  function SkipBlanksAndComma(c, endC: PUTF8Char): PUTF8Char; {$IFDEF INLINE} inline; {$ENDIF}

  function GetCommaSeparatedArray(const str: UTF8String): UTF8Strings;
  function TrimQuotes(const str: UTF8String): UTF8String;

  procedure ConvertUnicodeToUtf8(memStream: TMemoryStream);

  function GetScale(src, dst: double): double;
  function GetScaleForBestFit(srcW, srcH, dstW, dstH: double): double;

  function Base64Decode(const str: PAnsiChar; len: integer; memStream: TMemoryStream): Boolean;

type
  TSetOfUTF8Char = set of UTF8Char;

function CharInSet(chr: UTF8Char; const chrs: TSetOfUTF8Char): Boolean;
function DecodeUtf8ToUnicode(const utf8: UTF8String): UnicodeString;

const
  clInvalid   = $00010001;
  clCurrent   = $00010002;
  sqrt2       = 1.4142135623731;
  quote       = '''';
  dquote      = '"';
  space       = #32;
  comma       = ',';
  SvgDecimalSeparator = '.'; //do not localize

  {$I Img32.SVG.HashConsts.inc}

var
  LowerCaseTable : array[#0..#$FF] of UTF8Char;

implementation

//------------------------------------------------------------------------------
// Color Constant HashMap
//------------------------------------------------------------------------------

type
  PColorConst = ^TColorConst;
  TColorConst = record
    ColorName : UTF8String;
    ColorValue: TColor32;
  end;

  PPColorConstMapItem = ^PColorConstMapItem;
  PColorConstMapItem = ^TColorConstMapItem;
  TColorConstMapItem = record
    Hash: Cardinal;
    Next: PColorConstMapItem;
    Data: PColorConst;
  end;

  PColorConstMapItemArray = ^TColorConstMapItemArray;
  TColorConstMapItemArray = array[0..MaxInt div SizeOf(TColorConstMapItem) - 1] of TColorConstMapItem;

  TColorConstList = class(TObject)
  private
    FItems: array of TColorConstMapItem;
    FBuckets: array of PColorConstMapItem;
    FCount: Integer;
    FMod: Cardinal;
  public
    constructor Create(Colors: PColorConst; Count: Integer);
    function GetColorValue(const ColorName: UTF8String; var Color: TColor32): Boolean;
  end;

var
  ColorConstList : TColorConstList;

const
  buffSize    = 8;

  //include hashed html entity constants
  {$I Img32.SVG.HtmlHashConsts.inc}

//------------------------------------------------------------------------------
// Base64 (MIME) Encode & Decode and other encoding functions ...
//------------------------------------------------------------------------------

type
  PFourChars = ^TFourChars;
  TFourChars = record
    c1: ansichar;
    c2: ansichar;
    c3: ansichar;
    c4: ansichar;
  end;

function Chr64ToVal(c: ansiChar): integer; {$IFDEF INLINE} inline; {$ENDIF}
begin
  case c of
    '+': result := 62;
    '/': result := 63;
    '0'..'9': result := ord(c) + 4;
    'A'..'Z': result := ord(c) -65;
    'a'..'z': result := ord(c) -71;
    else Raise Exception.Create('Corrupted MIME encoded text');
  end;
end;
//------------------------------------------------------------------------------

function FrstChr(c: PFourChars): ansichar; {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := ansichar(Chr64ToVal(c.c1) shl 2 or Chr64ToVal(c.c2) shr 4);
end;
//------------------------------------------------------------------------------

function ScndChr(c: PFourChars): ansichar; {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := ansichar(Chr64ToVal(c.c2) shl 4 or Chr64ToVal(c.c3) shr 2);
end;
//------------------------------------------------------------------------------

function ThrdChr(c: PFourChars): ansichar; {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := ansichar( Chr64ToVal(c.c3) shl 6 or Chr64ToVal(c.c4) );
end;
//------------------------------------------------------------------------------

function Base64Decode(const str: PAnsiChar; len: integer; memStream: TMemoryStream): Boolean;
var
  i, j, extra: integer;
  Chars4: PFourChars;
  dst: PAnsiChar;
begin
  result := false;
  if (len = 0) or (len mod 4 > 0) or not Assigned(memStream) then exit;
  if str[len-2] = '=' then extra := 2
  else if str[len-1] = '=' then extra := 1
  else extra := 0;
  memStream.SetSize(LongInt((len div 4 * 3) - extra));
  dst := memStream.Memory;
  Chars4 := @str[0];
  i := 0;
  try
    for j := 1 to (len div 4) -1 do
    begin
      dst[i] := FrstChr(Chars4);
      dst[i+1] := ScndChr(Chars4);
      dst[i+2] := ThrdChr(Chars4);
      inc(pbyte(Chars4),4);
      inc(i,3);
    end;
    dst[i] := FrstChr(Chars4);
    if extra < 2  then dst[i+1] := ScndChr(Chars4);
    if extra < 1 then dst[i+2] := ThrdChr(Chars4);
  except
    Exit;
  end;
  Result := true;
end;

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

function NewSvgAttrib(): PSvgAttrib; {$IFDEF INLINE} inline; {$ENDIF}
begin
  // New(Result) uses RTTI to initialize the UTF8String fields to nil.
  // By allocating zero'ed memory we can achieve that much faster.
  Result := AllocMem(SizeOf(TSvgAttrib));
end;
//------------------------------------------------------------------------------

procedure DisposeSvgAttrib(attrib: PSvgAttrib); {$IFDEF INLINE} inline; {$ENDIF}
begin
  // Dispose(Result) uses RTTI to set the UTF8String fields to nil.
  // By clearing them outself we can achieve that much faster.
  attrib.name := '';
  attrib.value := '';
  FreeMem(attrib);
end;
//------------------------------------------------------------------------------

function GetScale(src, dst: double): double;
begin
  Result := dst / src;
  if (SameValue(Result, 1, 0.00001)) then Result := 1;
end;
//------------------------------------------------------------------------------

function GetScaleForBestFit(srcW, srcH, dstW, dstH: double): double;
var
  sx,sy: double;
begin
  sx := dstW / srcW;
  sy := dstH / srcH;
  if sy < sx then sx := sy;
  if (SameValue(sx, 1, 0.00001)) then
    Result := 1 else
    Result := sx;
end;
//------------------------------------------------------------------------------

function ClampRange(val, min, max: double): double;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  if val <= min then Result := min
  else if val >= max then Result := max
  else Result := val;
end;
//------------------------------------------------------------------------------

function IsSameAsciiUTF8String(const S1, S2: UTF8String): Boolean;
var
  Len: Integer;
  I: Integer;
  Ch1, Ch2: UTF8Char;
begin
  Len := Length(S1);
  Result := Len = Length(S2);
  if Result then
  begin
    Result := False;
    I := 1;
    while True do
    begin
      if I > Len then
        Break;

      Ch1 := S1[I];
      Ch2 := S2[I];
      if Ch1 = Ch2 then
      begin
        Inc(I);
        Continue;
      end;

      case Ch1 of
        'A'..'Z', 'a'..'z':
          ch1 := UTF8Char(Ord(ch1) xor $20); // toggle upper/lower
      end;

      if Ch1 <> Ch2 then
        Exit;
      Inc(I);
    end;
    Result := True;
  end;
end;
//------------------------------------------------------------------------------

function IsSameUTF8StringSlow(const S1, S2: UTF8String): Boolean;
begin
  Result := AnsiSameText(string(S1), string(S2));
end;
//------------------------------------------------------------------------------

function IsSameUTF8String(const S1, S2: UTF8String): Boolean;
var
  Len: Integer;
  I: Integer;
  Ch1, Ch2: UTF8Char;
begin
  Len := Length(S1);
  Result := Len = Length(S2);
  if Result then
  begin
    Result := False;
    I := 1;
    Ch1 := #0;
    Ch2 := #0;
    while True do
    begin
      if I > Len then
        Break;

      Ch1 := S1[I];
      Ch2 := S2[I];
      if Ch1 = Ch2 then
      begin
        Inc(I);
        Continue;
      end;

      case Ch1 of
        'A'..'Z', 'a'..'z':
          ch1 := UTF8Char(Ord(ch1) xor $20); // toggle upper/lower
      end;

      if Ch1 <> Ch2 then
        Break;
      Inc(I);
    end;
    if Ch1 = Ch2 then
      Result := True
    else if (Ord(Ch1) or Ord(Ch2)) and $80 <> 0 then // we found non-matching, non-ASCII characters
      Result := IsSameUTF8StringSlow(S1, S2);
  end;
end;
//------------------------------------------------------------------------------

function CharInSet(chr: UTF8Char; const chrs: TSetOfUTF8Char): Boolean;
begin
  Result := chr in chrs;
end;
//------------------------------------------------------------------------------

function Match(c: PUTF8Char; const compare: UTF8String): Boolean;
var
  i: integer;
begin
  Result := false;
  for i := 1 to Length(compare) do
  begin
    if LowerCaseTable[c[i - 1]] <> compare[i] then Exit;
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

function Match(const compare1, compare2: UTF8String): Boolean;
var
  i, len: integer;
  c1, c2: PUTF8Char;
begin
  Result := false;
  len := Length(compare1);
  if len <> Length(compare2) then Exit;
  c1 := @compare1[1]; c2 := @compare2[1];
  for i := 1 to len do
  begin
    if LowerCaseTable[c1[i - 1]] <> LowerCaseTable[c2[i - 1]] then Exit;
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

function Split(const str: UTF8String): UTF8Strings;
var
  i,j,k, spcCnt, len: integer;
begin
  spcCnt := 0;
  i := 1;
  len := Length(str);
  while (len > 0) and (str[len] <= space) do dec(len);
  while (i <= len) and (str[i] <= space) do inc(i);
  for j := i + 1 to len do
    if (str[j] <= space) and (str[j -1] > space) then inc(spcCnt);
  SetLength(Result, spcCnt +1);
  for k := 0 to spcCnt do
  begin
    j := i;
    while (j <= len) and (str[j] > space) do inc(j);
    SetLength(Result[k], j -i);
    if j > i then
      Move(str[i], Result[k][1], j -i);
    while (j <= len) and (str[j] <= space) do inc(j);
    i := j;
  end;
end;
//------------------------------------------------------------------------------

function TrimQuotes(const str: UTF8String): UTF8String;
var
  i, len: integer;
  savedQuote: UTF8Char;
begin
  len := Length(str);
  i := 1;
  while (i < len) and (str[i] <= space) do inc(i);
  if (i < len) and (str[i] in [quote, dquote]) then
  begin
    savedQuote := str[i];
    inc(i);
    while (len > i) and (str[len] <= space) do dec(len);
    if (len = i) or (str[len] <> savedQuote) then
      Result := str else  // oops!
      Result := Copy(str, i, len - i);
  end
  else
    Result := str
end;
//------------------------------------------------------------------------------

function GetCommaSeparatedArray(const str: UTF8String): UTF8Strings;
var
  i,j,k, cnt, len: integer;
begin
  // precondition: commas CANNOT be embedded
  len := Length(str);
  cnt := 1;
  for i := 1 to len do
    if (str[i] = comma) then inc(cnt);
  SetLength(Result, cnt);
  j := 0;
  k := 1;
  for i := 1 to len do
  begin
    if (str[i] <> comma) then Continue;
    Result[j] := TrimQuotes(Copy(str, k, i-k));
    inc(j);
    k := i + 1;
  end;
  if len >= k then
    Result[j] := TrimQuotes(Copy(str, k, len-k +1));
end;
//------------------------------------------------------------------------------

function GetXmlEncoding(memory: Pointer; len: integer): TSvgEncoding;
var
  p, p1: PUTF8Char;
begin
  Result := eUnknown;
  if (len < 4) or not Assigned(memory) then Exit;
  p := PUTF8Char(memory);
  p1 := (p + 1);
  case p^ of
    #$EF: if (p1^ = #$BB) then
      if ((p +2)^ = #$BF) then
        Result := eUtf8 else
        Exit;
    #$FF: if (p1^ = #$FE) or (p1^ = #0) then
      Result := eUnicodeLE;
    #$FE: if (p1^ = #$FF) then
      Result := eUnicodeBE;
  end;
end;
//------------------------------------------------------------------------------

function SkipBlanks(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  cc: PUTF8Char;
begin
  cc := c;
  if (cc < endC) and (cc^ <= space) then
  begin
    inc(cc);
    while (cc < endC) and (cc^ <= space) do inc(cc);
    c := cc;
  end;
  Result := (cc < endC);
end;
//------------------------------------------------------------------------------

function SkipBlanksEx(c: PUTF8Char; endC: PUTF8Char): PUTF8Char;
begin
  while (c < endC) and (c^ <= space) do inc(c);
  Result := c;
end;
//------------------------------------------------------------------------------

function SkipBlanksAndComma(c, endC: PUTF8Char): PUTF8Char;
begin
  Result := SkipBlanksEx(c, endC);
  if (Result >= endC) or (Result^ <> ',') then Exit;
  Result := SkipBlanksEx(Result + 1, endC);
end;
//------------------------------------------------------------------------------

function SkipStyleBlanks(c, endC: PUTF8Char): PUTF8Char;
var
  inComment: Boolean;
  ch: UTF8Char;
begin
  //style content may include multi-line comment blocks
  inComment := false;
  while (c < endC) do
  begin
    ch := c^;
    if inComment then
    begin
      if (ch = '*') and ((c +1)^ = '/')  then
      begin
        inComment := false;
        inc(c);
      end;
    end
    else if (ch > space) then
    begin
      inComment := (ch = '/') and ((c +1)^ = '*');
      if not inComment then break;
      inc(c);
    end;
    inc(c);
  end;
  Result := c;
end;
//------------------------------------------------------------------------------

function IsDigit(c: UTF8Char): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
begin
  case c of
    '0'..'9': Result := True;
    else Result := False;
  end;
end;
//------------------------------------------------------------------------------

function IsQuoteChar(c: UTF8Char): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := (c = quote) or (c = dquote);
end;
//------------------------------------------------------------------------------

function IsAlpha(c: UTF8Char): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
begin
  case c of
    'A'..'Z', 'a'..'z': Result := True;
    else Result := False;
  end;
end;
//------------------------------------------------------------------------------

function ParseStyleNameLen(c, endC: PUTF8Char): PUTF8Char;
var
  c2: PUTF8Char;
begin
  Result := c;
  //nb: style names may start with a hyphen
  c2 := Result;
  if (c2^ = '-') then inc(c2);
  if not IsAlpha(c2^) then Exit;

  Result := c2 + 1;
  while Result < endC do
  begin
    case Result^ of
      '0'..'9', 'A'..'Z', 'a'..'z', '-': inc(Result);
      else break;
    end;
  end;
end;
//------------------------------------------------------------------------------

function ParseNextWord(var c: PUTF8Char; endC: PUTF8Char; out word: UTF8String): Boolean;
var
  c2, cc: PUTF8Char;
begin
  cc := SkipBlanksAndComma(c, endC);
  if cc >= endC then
  begin
    c := cc;
    Result := False;
    Exit;
  end;

  c2 := cc;
  while cc < endC do
  begin
    case cc^ of
      'A'..'Z', 'a'..'z': inc(cc);
      else break;
    end;
  end;
  c := cc;
  ToUTF8String(c2, cc, word);
  Result := True;
end;
//------------------------------------------------------------------------------

function ParseNextWordHash(var c: PUTF8Char; endC: PUTF8Char; out hash: cardinal): Boolean;
var
  c2, cc: PUTF8Char;
begin
  cc := SkipBlanksAndComma(c, endC);
  if cc >= endC then
  begin
    c := cc;
    hash := 0;
    Result := False;
    Exit;
  end;

  c2 := cc;
  while cc < endC do
  begin
    case cc^ of
      'A'..'Z', 'a'..'z': inc(cc);
      else break;
    end;
  end;
  c := cc;
  hash := GetHash(c2, cc - c2);
  Result := True;
end;
//------------------------------------------------------------------------------

function ParseNextWordHash(c, endC: PUTF8Char): cardinal;
var
  c2: PUTF8Char;
begin
  c := SkipBlanksAndComma(c, endC);
  if c >= endC then
  begin
    Result := 0;
    Exit;
  end;

  c2 := c;
  while c < endC do
  begin
    case c^ of
      'A'..'Z', 'a'..'z': inc(c);
      else break;
    end;
  end;
  Result := GetHash(c2, c - c2);
end;
//------------------------------------------------------------------------------

function ParseNextWordExHash(var c: PUTF8Char; endC: PUTF8Char;
  out hash: cardinal): Boolean;
var
  c2, cc: PUTF8Char;
begin
  cc := SkipBlanksAndComma(c, endC);
  if cc >= endC then
  begin
    c := cc;
    hash := 0;
    Result := False;
    Exit;
  end;

  if cc^ = quote then
  begin
    inc(c);
    c2 := cc;
    while (cc < endC) and (cc^ <> quote) do inc(cc);
    hash := GetHash(c2, cc - c2);
    inc(cc);
  end else
  begin
    if not IsAlpha(cc^) then
    begin
      hash := 0;
      Result := False;
      Exit;
    end;
    c2 := cc;
    inc(cc);
    while cc < endC do
      case cc^ of
        'A'..'Z', 'a'..'z', '-', '_': inc(cc);
        else break;
      end;
    hash := GetHash(c2, cc - c2);
  end;
  c := cc;
  Result := True;
end;
//------------------------------------------------------------------------------

function ParseNameLength(c: PUTF8Char; endC: PUTF8Char): PUTF8Char;
begin
  inc(c);
  while c < endC do
  begin
    case c^ of
      '0'..'9', 'A'..'Z', 'a'..'z', '_', ':', '-': inc(c);
      else break;
    end;
  end;
  Result := c;
end;
//------------------------------------------------------------------------------

{$OVERFLOWCHECKS OFF}
function GetHash(c: PUTF8Char; len: nativeint): cardinal;
var
  i: integer;
begin
  //https://en.wikipedia.org/wiki/Jenkins_hash_function
  Result := 0;
  if c = nil then Exit;
  for i := 1 to len do
  begin
    Result := (Result + Ord(LowerCaseTable[c^]));
    Result := Result + (Result shl 10);
    Result := Result xor (Result shr 6);
    inc(c);
  end;
  Result := Result + (Result shl 3);
  Result := Result xor (Result shr 11);
  Result := Result + (Result shl 15);
end;
{$IFDEF OVERFLOWCHECKS_ENABLED}
  {$OVERFLOWCHECKS ON}
{$ENDIF}
//------------------------------------------------------------------------------

function GetHash(const name: UTF8String): cardinal;
begin
  // skip function call by directly casting it to Pointer
  Result := GetHash(PUTF8Char(Pointer(name)), Length(name));
end;
//------------------------------------------------------------------------------

{$OVERFLOWCHECKS OFF}
function GetHashCaseSensitive(name: PUTF8Char; nameLen: integer): cardinal;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to nameLen do
  begin
    Result := (Result + Ord(name^));
    Result := Result + (Result shl 10);
    Result := Result xor (Result shr 6);
    inc(name);
  end;
  Result := Result + (Result shl 3);
  Result := Result xor (Result shr 11);
  Result := Result + (Result shl 15);
end;
{$IFDEF OVERFLOWCHECKS_ENABLED}
  {$OVERFLOWCHECKS ON}
{$ENDIF}
//------------------------------------------------------------------------------

function ParseNextWordHashed(var c: PUTF8Char; endC: PUTF8Char): cardinal;
var
  c2: PUTF8Char;
  len: integer;
begin
  c2 := c;
  c := ParseNameLength(c2, endC);
  len := c - c2;
  if len <= 0 then Result := 0
  else Result := GetHash(c2, len);
end;
//------------------------------------------------------------------------------

function ParseExpDigits(c, endC: PUTF8Char; out val: Integer): PUTF8Char; {$IFDEF INLINE} inline; {$ENDIF}
var
  v32: Cardinal;
  Digit: Integer;
begin
  Result := c;
  v32 := 0;
  while Result < endC do
  begin
    Digit := Integer(Ord(Result^)) - Ord('0');
    if Cardinal(Digit) >= 10 then break;
    {$IFDEF FPC} // Something Delphi can optimize but FPC can't (yet?)
    v32 := (v32 shl 3) + (v32 shl 1) + Cardinal(Digit); // Delphi's code is even better than this
    {$ELSE}
    v32 := v32 * 10 + Cardinal(Digit);
    {$ENDIF FPC}
    inc(Result);
  end;
  val := v32;
end;
//------------------------------------------------------------------------------

function ParseDigitsToDouble(c, endC: PUTF8Char; out val: double): PUTF8Char;
var
  v32: Cardinal;
  v64: Int64;
  Digit: Integer;
  blockEndC: PUTF8Char;
begin
  // skip leading zeros
  while (c < endC) and (c^ = '0') do inc(c);

  // Use Int32 first as it is fast for 64bit and 32bit CPUs
  Result := c;
  v32 := 0;

  blockEndC := c + 9; // log10(2^31) = 9.33
  if blockEndC > endC then
    blockEndC := endC;
  while Result < blockEndC do
  begin
    Digit := Integer(Ord(Result^)) - Ord('0');
    if Cardinal(Digit) >= 10 then break;
    {$IFDEF FPC} // Something Delphi can optimize but FPC can't (yet?)
    v32 := (v32 shl 3) + (v32 shl 1) + Cardinal(Digit);
    {$ELSE}
    v32 := v32 * 10 + Cardinal(Digit);
    {$ENDIF FPC}
    inc(Result);
  end;

  if (Result < endC) and (Result >= blockEndC) then
  begin
    v64 := v32;

    blockEndC := c + 18; // log10(2^63) = 18.96
    if blockEndC > endC then
      blockEndC := endC;
    while Result < blockEndC do
    begin
      Digit := Integer(Ord(Result^)) - Ord('0');
      if Cardinal(Digit) >= 10 then break;
      {$IF (SizeOf(Pointer) = 4) or defined(FPC)} // neither Delphi 32bit nor FPC can optimize this
      v64 := (v64 shl 3) + (v64 shl 1) + Cardinal(Digit);
      {$ELSE}
      v64 := v64 * 10 + Cardinal(Digit);
      {$IFEND}
      inc(Result);
    end;

    val := v64;
    // Use Double for the remaining digits and loose precision (we are beyong 16 digits anyway)
    if (Result < endC) and (Result >= blockEndC) then
    begin
      while Result < endC do
      begin
        Digit := Integer(Ord(Result^)) - Ord('0');
        if Cardinal(Digit) >= 10 then break;
        val := val * 10 + Digit;
        inc(Result);
      end;
    end;
  end
  else
    val := v32;
end;
//------------------------------------------------------------------------------

function ParseNextNumEx(var c: PUTF8Char; endC: PUTF8Char; skipComma: Boolean;
  out val: double; out unitType: TUnitType): Boolean;
const
  Power10: array[0..18] of Double = (
    1E0, 1E1, 1E2, 1E3, 1E4, 1E5, 1E6, 1E7, 1E8, 1E9,
    1E10, 1E11, 1E12, 1E13, 1E14, 1E15, 1E16, 1E17, 1E18
  );
  Power10Reciprocal: array[0..18] of Double = (
    1/1E0, 1/1E1, 1/1E2, 1/1E3, 1/1E4, 1/1E5, 1/1E6, 1/1E7, 1/1E8, 1/1E9,
    1/1E10, 1/1E11, 1/1E12, 1/1E13, 1/1E14, 1/1E15, 1/1E16, 1/1E17, 1/1E18
  );
var
  exp: integer;
  isNeg, expIsNeg: Boolean;
  start, decStart, cc: PUTF8Char;
  decimals: Double;
begin
  Result := false;
  unitType := utNumber;

  cc := c;

  //skip white space +/- single comma
  if skipComma then
  begin
    while (cc < endC) and (cc^ <= space) do inc(cc);
    if (cc^ = ',') then inc(cc);
  end;
  while (cc < endC) and (cc^ <= space) do inc(cc);
  if (cc = endC) then
  begin
    c := cc;
    Exit;
  end;

  exp := Invalid; expIsNeg := false;
  isNeg := cc^ = '-';
  if isNeg then inc(cc);

  start := cc;

  // Use fast parsing
  cc := ParseDigitsToDouble(cc, endC, val);
  if cc < endC then
  begin
    // Decimals
    if Ord(cc^) = Ord(SvgDecimalSeparator) then
    begin
      inc(cc);
      decStart := cc;
      cc := ParseDigitsToDouble(cc, endC, decimals);
      if cc > decStart then
      begin
        if cc - decStart <= 18 then
          val := val + (decimals * Power10Reciprocal[(cc - decStart)])
        else
          val := val + (decimals * Power(10, -(cc - decStart)))
      end;
    end;

    // Exponent
    if (cc < endC) and ((cc^ = 'e') or (cc^ = 'E')) then
    begin
      case (cc+1)^ of
        '-', '0'..'9':
          begin
            inc(cc);
            if cc^ = '-' then
            begin
              expIsNeg := true;
              inc(cc);
            end;
            cc := ParseExpDigits(cc, endC, exp);
          end;
      end;
    end;
  end;
  Result := cc > start;
  if not Result then
  begin
    c := cc;
    Exit;
  end;

  if isNeg then val := -val;
  if IsValid(exp) then
  begin
    if exp <= 18 then
    begin
      if expIsNeg then
        val := val * Power10Reciprocal[exp] else
        val := val * Power10[exp];
    end
    else
    begin
      if expIsNeg then
        val := val * Power(10, -exp) else
        val := val * Power(10, exp);
    end;
  end;

  //https://oreillymedia.github.io/Using_SVG/guide/units.html
  case cc^ of
    '%':
      begin
        inc(cc);
        unitType := utPercent;
      end;
    'c': //convert cm to pixels
      if ((cc+1)^ = 'm') then
      begin
        inc(cc, 2);
        unitType := utCm;
      end;
    'd': //ignore deg
      if ((cc+1)^ = 'e') and ((cc+2)^ = 'g') then
      begin
        inc(cc, 3);
        unitType := utDegree;
      end;
    'e': //convert cm to pixels
      if ((cc+1)^ = 'm') then
      begin
        inc(cc, 2);
        unitType := utEm;
      end
      else if ((cc+1)^ = 'x') then
      begin
        inc(cc, 2);
        unitType := utEx;
      end;
    'i': //convert inchs to pixels
      if ((cc+1)^ = 'n') then
      begin
        inc(cc, 2);
        unitType := utInch;
      end;
    'm': //convert mm to pixels
      if ((cc+1)^ = 'm') then
      begin
        inc(cc, 2);
        unitType := utMm;
      end;
    'p':
      case (cc+1)^ of
        'c':
          begin
            inc(cc, 2);
            unitType := utPica;
          end;
        't':
          begin
            inc(cc, 2);
            unitType := utPt;
          end;
        'x':
          begin
            inc(cc, 2);
            unitType := utPixel;
          end;
      end;
    'r': //convert radian angles to degrees
      if Match(cc, 'rad') then
      begin
        inc(cc, 3);
        unitType := utRadian;
      end;
  end;
  c := cc;
end;
//------------------------------------------------------------------------------

function ParseNextNum(var c: PUTF8Char; endC: PUTF8Char;
  skipComma: Boolean; out val: double): Boolean;
var
  tmp: TValue;
begin
  tmp.Init;
  Result := ParseNextNumEx(c, endC, skipComma, tmp.rawVal, tmp.unitType);
  val := tmp.GetValue(1, 1);
end;
//------------------------------------------------------------------------------

function ExtractRef(const href: UTF8String): UTF8String; {$IFDEF INLINE} inline; {$ENDIF}
var
  c, c2, endC: PUTF8Char;
begin
  c := PUTF8Char(href);
  endC := c + Length(href);
  if Match(c, 'url(') then
  begin
    inc(c, 4);
    dec(endC); // avoid trailing ')'
  end;
  if c^ = '#' then inc(c);
  c2 := c;
  while (c < endC) and (c^ <> ')') do inc(c);
  ToUTF8String(c2, c, Result);
end;
//------------------------------------------------------------------------------

function ParseNextChar(var c: PUTF8Char; endC: PUTF8Char): UTF8Char;
var
  cc: PUTF8Char;
begin
  cc := SkipBlanksEx(c, endC);
  if cc >= endC then
    Result := #0
  else
  begin
    Result := cc^;
    c := cc + 1;
  end;
end;
//------------------------------------------------------------------------------

procedure ToTrimmedUTF8String(c, endC: PUTF8Char; var S: UTF8String);
var
  len: integer;
begin
  // trim left
  while (c < endC) and (c^ <= space) do Inc(c);
  // trim right
  while (endC > c) and (endC[-1] <= space) do Dec(endC);

  len := endC - c;
  SetLength(S, len);
  if len = 0 then Exit;
  Move(c^, PUTF8Char(S)^, len * SizeOf(UTF8Char));
end;
//------------------------------------------------------------------------------

function PosEx(const subStr: UTF8String; const text: Utf8String; startIdx: integer): integer;
var
  i, maxI, len, subStrLen: integer;
begin
  len := Length(Text);
  subStrLen := Length(subStr);
  maxI := len - subStrLen +1;
  for i := Max(1, startIdx) to maxI do
  begin
    if (text[i] <> subStr[1]) or
      not CompareMem(@text[i], @subStr[1], subStrLen) then Continue;
    Result := i;
    Exit;
  end;
  Result := 0;
end;
//------------------------------------------------------------------------------

function ReversePosEx(utf8: utf8Char;
  const text: Utf8String; startIdx: integer): integer; overload;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := Max(0, Min(Length(text), startidx));
  while (Result > 0) and (text[Result] <> utf8) do Dec(Result);
end;
//------------------------------------------------------------------------------

function TrimMultiSpacesUtf8(const text: Utf8String): Utf8String;
var
  i, len: integer;
begin
  Result := text;
  len := Length(Result);
  for i := 1 to len do
    if Result[i] < #32 then Result[i] := #32;
  i := ReversePosEx(space, Result, len);
  while i > 1 do
  begin
    Dec(i);
    while (i > 0) and (Result[i] = space) do
    begin
      Delete(Result, i, 1);
      Dec(i);
    end;
    i := ReversePosEx(space, Result, i);
  end;
end;
//------------------------------------------------------------------------------

function ReversePosEx(c: WideChar;
  const text: UnicodeString; startIdx: integer): integer; overload;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := Max(0, Min(Length(text), startidx));
  while (Result > 0) and (text[Result] <> c) do Dec(Result);
end;
//------------------------------------------------------------------------------

function TrimMultiSpacesUnicode(const text: UnicodeString): UnicodeString;
var
  i, len: integer;
begin
  Result := text;
  len := Length(Result);
  for i := 1 to len do
    if Result[i] < #32 then Result[i] := #32;
  i := ReversePosEx(space, Result, len);
  while i > 1 do
  begin
    Dec(i);
    while (i > 0) and (Result[i] = space) do
    begin
      Delete(Result, i, 1);
      Dec(i);
    end;
    i := ReversePosEx(space, Result, i);
  end;
end;
//------------------------------------------------------------------------------

function StripNewlines(const s: UTF8String): UTF8String;
var
  i: integer;
begin
  Result := s;
  i := Length(Result);
  while i > 0 do
  begin
    if Result[i] < space then Delete(Result, i, 1);
    Dec(i);
  end;
end;
//------------------------------------------------------------------------------

function StripNewlines(const s: UnicodeString): UnicodeString;
var
  i: integer;
begin
  Result := s;
  i := Length(Result);
  while i > 0 do
  begin
    if Result[i] < space then Delete(Result, i, 1);
    Dec(i);
  end;
end;
//------------------------------------------------------------------------------

function ConvertNewlines(const s: UTF8String): UTF8String; overload;
var
  i: integer;
begin
  Result := s;
  i := Length(Result);
  while i > 0 do
  begin
    if Result[i] < space then
    begin
      if Result[i] = #10 then
        Result[i] := space else
        Delete(Result, i, 1);
    end;
    Dec(i);
  end;
end;
//------------------------------------------------------------------------------

function ConvertNewlines(const s: UnicodeString): UnicodeString; overload;
var
  i: integer;
begin
  Result := s;
  i := Length(Result);
  while i > 0 do
  begin
    if Result[i] < space then
    begin
      if Result[i] = #10 then
        Result[i] := space else
        Delete(Result, i, 1);
    end;
    Dec(i);
  end;
end;
//------------------------------------------------------------------------------

procedure ToUTF8String(c, endC: PUTF8Char;
  var S: UTF8String; spacesInText: TSpacesInText);
var
  len: integer;
begin
  len := endC - c;
  SetLength(S, len);
  if len = 0 then Exit;
  Move(c^, PUTF8Char(S)^, len * SizeOf(UTF8Char));
  if spacesInText <> sitPreserve then
    S := TrimMultiSpacesUtf8(S);
  S := ConvertNewlines(S);
end;
//------------------------------------------------------------------------------

procedure ToAsciiLowerUTF8String(c, endC: PUTF8Char; var S: UTF8String);
// Reads a UTF8String and converts all upper case 'A'..'Z' to lower case 'a'..'z'
var
  len: integer;
  p: PUTF8Char;
  ch: UTF8Char;
begin
  len := endC - c;
  SetLength(S, len);
  if len = 0 then Exit;

  // Use a pointer arithmetic trick to run forward by using a negative index
  p := PUTF8Char(S) + len;
  len := -len;
  while len < 0 do
  begin
    ch := endC[len];
    case ch of
      'A'..'Z':
        ch := UTF8Char(Byte(ch) or $20);
    end;
    p[len] := ch;
    inc(len);
  end;
end;
//------------------------------------------------------------------------------

function IsKnownEntity(owner: TSvgParser;
  var c: PUTF8Char; endC: PUTF8Char; out entity: PSvgAttrib): boolean;
var
  c2, c3: PUTF8Char;
begin
  inc(c); //skip ampersand.
  c2 := c; c3 := c;
  c3 := ParseNameLength(c3, endC);
  entity := owner.FindEntity(GetHash(c2, c3 - c2));
  Result := (c3^ = ';') and Assigned(entity);
  //nb: increments 'c' only if the entity is found.
  if Result then c := c3 +1 else dec(c);
end;
//------------------------------------------------------------------------------

function ParseQuotedString(var c: PUTF8Char; endC: PUTF8Char;
  out quotStr: UTF8String): Boolean;
var
  quote: UTF8Char;
  c2: PUTF8Char;
begin
  quote := c^;
  inc(c);
  c2 := c;
  while (c < endC) and (c^ <> quote) do inc(c);
  Result := (c < endC);
  if not Result then Exit;
  ToUTF8String(c2, c, quotStr);
  inc(c);
end;
//------------------------------------------------------------------------------

function IsNumPending(var c: PUTF8Char;
  endC: PUTF8Char; ignoreComma: Boolean): Boolean;
var
  c2: PUTF8Char;
begin
  Result := false;

  //skip white space +/- single comma
  if ignoreComma then
  begin
    while (c < endC) and (c^ <= space) do inc(c);
    if (c^ = ',') then inc(c);
  end;
  while (c < endC) and (c^ <= ' ') do inc(c);
  if (c = endC) then Exit;

  c2 := c;
  if (c2^ = '-') then inc(c2);
  if (c2^ = SvgDecimalSeparator) then inc(c2);
  Result := (c2 < endC) and IsDigit(c2^);
end;
//------------------------------------------------------------------------------

function ParseTransform(const transform: UTF8String): TMatrixD;
var
  i: integer;
  c, endC: PUTF8Char;
  c2: UTF8Char;
  word: UTF8String;
  values: array[0..5] of double;
  mat: TMatrixD;
begin
  c := PUTF8Char(transform);
  endC := c + Length(transform);
  Result := IdentityMatrix; //in case of invalid or referenced value

  while ParseNextWord(c, endC, word) do
  begin
    if Length(word) < 5 then Exit;
    if ParseNextChar(c, endC) <> '(' then Exit; //syntax check
    //reset values variables
    for i := 0 to High(values) do values[i] := InvalidD;
    //and since every transform function requires at least one value
    if not ParseNextNum(c, endC, false, values[0]) then Break;
    //now get additional variables
    i := 1;
    while (i < 6) and IsNumPending(c, endC, true) and
      ParseNextNum(c, endC, true, values[i]) do inc(i);
    if ParseNextChar(c, endC) <> ')' then Exit; //syntax check

    mat := IdentityMatrix;
    //scal(e), matr(i)x, tran(s)late, rota(t)e, skew(X), skew(Y)
    case LowerCaseTable[word[5]] of
      'e' : //scalE
        if not IsValid(values[1]) then
          MatrixScale(mat, values[0]) else
            MatrixScale(mat, values[0], values[1]);
      'i' : //matrIx
        if IsValid(values[5]) then
        begin
          mat[0,0] :=  values[0];
          mat[0,1] :=  values[1];
          mat[1,0] :=  values[2];
          mat[1,1] :=  values[3];
          mat[2,0] :=  values[4];
          mat[2,1] :=  values[5];
        end;
      's' : //tranSlateX, tranSlateY & tranSlate
        if Length(word) =10  then
        begin
          c2 := LowerCaseTable[word[10]];
          if c2 = 'x' then
            MatrixTranslate(mat, values[0], 0)
          else if c2 = 'y' then
            MatrixTranslate(mat, 0, values[0]);
        end
        else if IsValid(values[1]) then
          MatrixTranslate(mat, values[0], values[1])
        else
          MatrixTranslate(mat, values[0], 0);
      't' : //rotaTe
        if IsValid(values[2]) then
          MatrixRotate(mat, PointD(values[1],values[2]), DegToRad(values[0]))
        else
          MatrixRotate(mat, NullPointD, DegToRad(values[0]));
       'x' : //skewX
         begin
            MatrixSkew(mat, DegToRad(values[0]), 0);
         end;
       'y' : //skewY
         begin
            MatrixSkew(mat, 0, DegToRad(values[0]));
         end;
    end;
    MatrixMultiply2(mat, Result);
  end;
end;
//------------------------------------------------------------------------------

procedure GetSvgFontInfo(const value: UTF8String; var fontInfo: TSVGFontInfo);
var
  c, endC: PUTF8Char;
  hash: Cardinal;
begin
  c := PUTF8Char(value);
  endC := c + Length(value);
  while (c < endC) and SkipBlanks(c, endC) do
  begin
    if c = ';' then
      break
    else if IsNumPending(c, endC, true) then
      ParseNextNum(c, endC, true, fontInfo.size)
    else
    begin
      hash := ParseNextWordHashed(c, endC);
      case hash of
        hSans_045_Serif   : fontInfo.family := tfSansSerif;
        hSerif            : fontInfo.family := tfSerif;
        hMonospace        : fontInfo.family := tfMonospace;
        hBold             : fontInfo.weight := 600;
        hItalic           : fontInfo.italic := sfsItalic;
        hNormal           :
          begin
            fontInfo.weight := 400;
            fontInfo.italic := sfsNone;
          end;
        hStart            : fontInfo.align := staLeft;
        hMiddle           : fontInfo.align := staCenter;
        hEnd              : fontInfo.align := staRight;
        hline_045_through : fontInfo.decoration := fdStrikeThrough;
        hUnderline        : fontInfo.decoration := fdUnderline;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function HtmlDecode(const html: UTF8String): UTF8String;
var
  val, len: integer;
  c,ce,endC: PUTF8Char;
  ch: UTF8Char;
begin
  len := Length(html);
  SetLength(Result, len*3);
  c := PUTF8Char(html);
  endC := c + len;
  ce := c;
  len := 1;
  while (ce < endC) and (ce^ <> '&') do
    inc(ce);

  while (ce < endC) do
  begin
    if ce > c then
    begin
      Move(c^, Result[len], ce - c);
      inc(len, ce - c);
    end;
    c := ce; inc(ce);
    while (ce < endC) and (ce^ <> ';') do inc(ce);
    if ce = endC then break;

    val := -1; //assume error
    if (c +1)^ = '#' then
    begin
      val := 0;
      //decode unicode value
      if (c +2)^ = 'x' then
      begin
        inc(c, 3);
        while c < ce do
        begin
          ch := c^;
          case ch of
            'a'..'f':
              val := val * 16 + Ord(ch) - 87;
            'A'..'F':
              val := val * 16 + Ord(ch) - 55;
            '0'..'9':
              val := val * 16 + Ord(ch) - 48;
          else
            val := -1;
            break;
          end;
          inc(c);
        end;
      end else
      begin
        inc(c, 2);
        while c < ce do
        begin
          val := val * 10 + Ord(c^) - 48;
          inc(c);
        end;
      end;
    end else
    begin
      //decode html entity ...
      case GetHashCaseSensitive(c, ce - c) of
        {$I Img32.SVG.HtmlValues.inc}
      end;
    end;

    //convert unicode value to utf8 chars
    //this saves the overhead of multiple UTF8String<-->string conversions.
    case val of
      0 .. $7F:
        begin
          result[len] := UTF8Char(val);
          inc(len);
        end;
      $80 .. $7FF:
        begin
          Result[len] := UTF8Char($C0 or (val shr 6));
          Result[len+1] := UTF8Char($80 or (val and $3f));
          inc(len, 2);
        end;
      $800 .. $7FFF:
        begin
          Result[len] := UTF8Char($E0 or (val shr 12));
          Result[len+1] := UTF8Char($80 or ((val shr 6) and $3f));
          Result[len+2] := UTF8Char($80 or (val and $3f));
          inc(len, 3);
        end;
      $10000 .. $10FFFF:
        begin
          Result[len] := UTF8Char($F0 or (val shr 18));
          Result[len+1] := UTF8Char($80 or ((val shr 12) and $3f));
          Result[len+2] := UTF8Char($80 or ((val shr 6) and $3f));
          Result[len+3] := UTF8Char($80 or (val and $3f));
          inc(len, 4);
        end;
      else
      begin
        //ie: error
        Move(c^, Result[len], ce- c +1);
        inc(len, ce - c +1);
      end;
    end;
    inc(ce);
    c := ce;
    while (ce < endC) and (ce^ <> '&') do inc(ce);
  end;
  if (c < endC) and (ce > c) then
  begin
    Move(c^, Result[len], (ce - c));
    inc(len, ce - c);
  end;
  setLength(Result, len -1);
end;
//------------------------------------------------------------------------------

function HexByteToInt(h: UTF8Char): Cardinal; {$IFDEF INLINE} inline; {$ENDIF}
begin
  case h of
    '0'..'9': Result := Ord(h) - Ord('0');
    'A'..'F': Result := 10 + Ord(h) - Ord('A');
    'a'..'f': Result := 10 + Ord(h) - Ord('a');
    else Result := 0;
  end;
end;
//------------------------------------------------------------------------------

function IsFraction(val: double): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := (val <> 0) and (Abs(val) < 1);
end;
//------------------------------------------------------------------------------

function UTF8StringToColor32(const value: UTF8String; var color: TColor32): Boolean;
var
  i, len  : integer;
  j       : Cardinal;
  clr     : TColor32;
  alpha   : Byte;
  vals    : array[0..3] of double;
  mus     :  array[0..3] of TUnitType;
  c, endC : PUTF8Char;
begin
  Result := false;
  len := Length(value);
  if len < 3 then Exit;
  c := PUTF8Char(value);

  if (color = clInvalid) or (color = clCurrent) or (color = clNone32) then
    alpha := 255 else
    alpha := GetAlpha(color);

  if Match(c, 'rgb') then
  begin
    endC := c + len;
    inc(c, 3);
    if (c^ = 'a') then inc(c);
    if (ParseNextChar(c, endC) <> '(') or
      not ParseNextNumEx(c, endC, false, vals[0], mus[0]) or
      not ParseNextNumEx(c, endC, true, vals[1], mus[1]) or
      not ParseNextNumEx(c, endC, true, vals[2], mus[2]) then Exit;
    for i := 0 to 2 do
      if mus[i] = utPercent then
        vals[i] := vals[i] * 255 / 100;

    if (c < endC) and (c^ <> ')') and ParseNextNumEx(c, endC, true, vals[3], mus[3]) then
      alpha := 255 else //stops further alpha adjustment
      vals[3] := 255;
    if ParseNextChar(c, endC) <> ')' then Exit;
    for i := 0 to 3 do if IsFraction(vals[i]) then
      vals[i] := vals[i] * 255;
    color := ClampByte(Integer(Round(vals[3]))) shl 24 +
      ClampByte(Integer(Round(vals[0]))) shl 16 +
      ClampByte(Integer(Round(vals[1]))) shl 8 +
      ClampByte(Integer(Round(vals[2])));
  end
  else if (c^ = '#') then           //#RRGGBB or #RGB
  begin
    if (len = 9) then
    begin
      clr := $0;
      alpha := $0;
      for i := 1 to 6 do
      begin
        inc(c);
        clr := clr shl 4 + HexByteToInt(c^);
      end;
      for i := 1 to 2 do
      begin
        inc(c);
        alpha := alpha shl 4 + HexByteToInt(c^);
      end;
      clr := clr or alpha shl 24;
    end
    else if (len = 7) then
    begin
      clr := $0;
      for i := 1 to 6 do
      begin
        inc(c);
        clr := clr shl 4 + HexByteToInt(c^);
      end;
      clr := clr or $FF000000;
    end
    else if (len = 5) then
    begin
      clr := $0;
      for i := 1 to 3 do
      begin
        inc(c);
        j := HexByteToInt(c^);
        clr := clr shl 4 + j;
        clr := clr shl 4 + j;
      end;
      inc(c);
      alpha := HexByteToInt(c^);
      alpha := alpha + alpha shl 4;
      clr := clr or alpha shl 24;
    end
    else if (len = 4) then
    begin
      clr := $0;
      for i := 1 to 3 do
      begin
        inc(c);
        j := HexByteToInt(c^);
        clr := clr shl 4 + j;
        clr := clr shl 4 + j;
      end;
      clr := clr or $FF000000;
    end
    else
      Exit;
    color :=  clr;
  end else                          //color name lookup
  begin
    if not ColorConstList.GetColorValue(value, color) then
      Exit;
  end;

  //and in case the opacity has been set before the color
  if (alpha < 255) then
    color := (color and $FFFFFF) or alpha shl 24;
{$IF DEFINED(ANDROID)}
  color := SwapRedBlue(color);
{$IFEND}
  Result := true;
end;
//------------------------------------------------------------------------------

function ScaleDashArray(const dblArray: TArrayOfDouble; scale: double): TArrayOfDouble;
var
  i, len: integer;
begin
  len := Length(dblArray);
  SetLength(Result, len);
  if len = 0 then Exit;

  for i := 0 to len -1 do
    Result[i] := dblArray[i] * scale;

  if Odd(len) then
  begin
    SetLength(Result, len *2);
    Move(Result[0], Result[len], len * SizeOf(double));
  end;
end;
//------------------------------------------------------------------------------

function PeekNextChar(var c: PUTF8Char; endC: PUTF8Char): UTF8Char;
begin
  if not SkipBlanks(c, endC) then
    Result := #0 else
    Result := c^;
end;
//------------------------------------------------------------------------------

procedure ParseStyleElementContent(const value: UTF8String;
  stylesList: TClassStylesList);
var
  len, cap: integer;
  names: array of UTF8String;

  procedure AddName(const name: UTF8String);
  begin
    if len = cap then
    begin
      cap := cap + buffSize;
      SetLength(names, cap);
    end;
    names[len] := name;
    inc(len);
  end;

var
  i: integer;
  aclassName: UTF8String;
  aStyle: UTF8String;
  c, c2, endC: PUTF8Char;
begin
  //https://oreillymedia.github.io/Using_SVG/guide/style.html

  stylesList.Clear;
  if value = '' then Exit;

  len := 0; cap := 0;
  c := @value[1];
  endC := c + Length(value);

  c := SkipBlanksEx(c, endC);
  if c >= endC then Exit;
  
  if Match(c, '<![cdata[') then inc(c, 9);

  while True do
  begin
    c := SkipStyleBlanks(c, endC);
    if c >= endC then Break;

    case c^ of
      SvgDecimalSeparator, '#', 'A'..'Z', 'a'..'z': ;
      else break;
    end;

    //get one or more class names for each pending style
    c2 := c;
    c := ParseNameLength(c, endC);
    ToAsciiLowerUTF8String(c2, c, aclassName);

    AddName(aclassName);

    c := SkipStyleBlanks(c, endC);
    if (c < endC) and (c^ = ',') then
    begin
      inc(c);
      Continue;
    end;
    if len = 0 then break;

    //now get the style
    if (c >= endC) or (c^ <> '{') then Break;
    inc(c);
    c2 := c;
    while (c < endC) and (c^ <> '}') do inc(c);
    if (c = endC) then break;
    ToTrimmedUTF8String(c2, c, aStyle);
    if aStyle <> '' then
    begin
      stylesList.Preallocate(len);
      //finally, for each class name add (or append) this style
      for i := 0 to len - 1 do
        stylesList.AddAppendStyle(names[i], aStyle);
    end;
    // Reset the used names array length, so we can reuse it to reduce the amount of SetLength calls
    len := 0;
    inc(c);
  end;
end;

//------------------------------------------------------------------------------
// TXmlEl classes
//------------------------------------------------------------------------------

constructor TXmlEl.Create(owner: TSvgParser);
begin
{$IFDEF XPLAT_GENERICS}
  attribs := TList<PSvgAttrib>.Create;
  childs := TList<TXmlEl>.Create;
{$ELSE}
  attribs := TList.Create;
  childs := TList.Create;
{$ENDIF}
  selfClosed := true;
  Self.owner := owner;
end;
//------------------------------------------------------------------------------

destructor TXmlEl.Destroy;
begin
  Clear;
  attribs.Free;
  childs.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TXmlEl.Clear;
var
  i: integer;
begin
  for i := 0 to attribs.Count -1 do
    DisposeSvgAttrib(PSvgAttrib(attribs.List[i]));
  attribs.Clear;

  for i := 0 to childs.Count -1 do
    TXmlEl(childs[i]).free;
  childs.Clear;
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseHeader(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  style: UTF8String;
  c2: PUTF8Char;
begin
  c2 := SkipBlanksEx(c, endC);
  c := ParseNameLength(c2, endC);
  ToAsciiLowerUTF8String(c2, c, name);

  //load the class's style (ie undotted style) if found.
  style := owner.classStyles.GetStyle(name);
  if style <> '' then ParseStyleAttribute(style);
  Result := ParseAttributes(c, endC);
end;
//------------------------------------------------------------------------------

class function TXmlEl.ParseAttribName(c: PUTF8Char;
  endC: PUTF8Char; attrib: PSvgAttrib): PUTF8Char;
begin
  Result := SkipBlanksEx(c, endC);
  if Result >= endC then Exit;
  c := Result;
  Result := ParseNameLength(Result, endC);
  ToUTF8String(c, Result, attrib.Name);
  attrib.hash := GetHash(attrib.Name);
end;
//------------------------------------------------------------------------------

class function TXmlEl.ParseAttribValue(c, endC: PUTF8Char;
  attrib: PSvgAttrib): PUTF8Char;
// Parse: [Whitespaces] "=" [Whitespaces] ("'" | "\"") <string> ("'" | "\"")
var
  quoteChar: UTF8Char;
  c2: PUTF8Char;
begin
  Result := endC;

  // ParseNextChar:
  c := SkipBlanksEx(c, endC);
  if (c >= endC) or (c^ <> '=') then Exit;
  inc(c); // '=' parsed

  // ParseQuoteChar:
  c := SkipBlanksEx(c, endC);
  if c >= endC then Exit;
  quoteChar := c^;
  if not (quoteChar in [quote, dquote]) then Exit;
  inc(c); // quote parsed

  //trim leading and trailing spaces in the actual value
  c := SkipBlanksEx(c, endC);
  // find value end
  Result := c;
  while (Result < endC) and (Result^ <> quoteChar) do inc(Result);
  c2 := Result;
  while (c2 > c) and ((c2 -1)^ <= space) do dec(c2);

  ToUTF8String(c, c2, attrib.value, sitPreserve);
  inc(Result); //skip end quote
end;
//------------------------------------------------------------------------------

class function TXmlEl.ParseAttribNameAndValue(c, endC: PUTF8Char; attrib: PSvgAttrib): PUTF8Char;
begin
  Result := ParseAttribName(c, endC, attrib);
  if (Result < endC) then
    Result := ParseAttribValue(Result, endC, attrib);
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseAttributes(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  i: integer;
  attrib, styleAttrib, classAttrib, idAttrib: PSvgAttrib;
  classes: UTF8Strings;
  ansi: UTF8String;
begin
  Result := false;
  styleAttrib := nil;  classAttrib := nil;  idAttrib := nil;

  while SkipBlanks(c, endC) do
  begin
    case c^ of
      '/', '?':
        begin
          inc(c);
          if (c^ <> '>') then Exit; //error
          selfClosed := true;
          inc(c);
          Result := true;
          break;
        end;
      '>':
        begin
          inc(c);
          Result := true;
          break;
        end;
      'x':
        if Match(c, 'xml:') then
        begin
          inc(c, 4); //ignore xml: prefixes
        end;
    end;

    attrib := NewSvgAttrib();
    c := ParseAttribNameAndValue(c, endC, attrib);
    if c >= endC then
    begin
      DisposeSvgAttrib(attrib);
      Exit;
    end;

    attribs.Add(attrib);
    case attrib.hash of
      hId     : idAttrib := attrib;
      hClass  : classAttrib := attrib;
      hStyle  : styleAttrib := attrib;
    end;    
  end;

  if assigned(classAttrib) then
    with classAttrib^ do
    begin
      //get the 'dotted' classname(s)
      classes := Split(value);
      for i := 0 to High(classes) do
      begin
        ansi := SvgDecimalSeparator + classes[i];
        //get the style definition
        ansi := owner.classStyles.GetStyle(ansi);
        if ansi <> '' then ParseStyleAttribute(ansi);
      end;
    end;

  if assigned(styleAttrib) then
    ParseStyleAttribute(styleAttrib.value);

  if assigned(idAttrib) then
  begin
    //get the 'hashed' classname
    ansi := '#' + idAttrib.value;
    //get the style definition
    ansi := owner.classStyles.GetStyle(ansi);
    if ansi <> '' then ParseStyleAttribute(ansi);
  end;
  
end;
//------------------------------------------------------------------------------

procedure TXmlEl.ParseStyleAttribute(const style: UTF8String);
var
  styleName, styleVal: UTF8String;
  c, c2, endC: PUTF8Char;
  attrib: PSvgAttrib;
begin
  //there are 4 ways to load styles (in ascending precedence) -
  //1. a class element style (called during element contruction)
  //2. a non-element class style (called via a class attribute)
  //3. an inline style (called via a style attribute)
  //4. an id specific class style

  c := PUTF8Char(style);
  endC := c + Length(style);
  while True do
  begin
    c := SkipStyleBlanks(c, endC);
    if c >= endC then Break;

    c2 := c;
    c := ParseStyleNameLen(c, endC);
    ToUTF8String(c2, c, styleName);
    if styleName = '' then Break;

    // ParseNextChar
    c := SkipStyleBlanks(c, endC);
    if (c >= endC) or (c^ <> ':') then Break;  //syntax check
    inc(c);

    c := SkipBlanksEx(c, endC);
    if c >= endC then Break;

    c2 := c;
    inc(c);
    while (c < endC) and (c^ <> ';') do inc(c);
    ToTrimmedUTF8String(c2, c, styleVal);
    inc(c);

    attrib := NewSvgAttrib();
    attrib.name := styleName;
    attrib.value := styleVal;
    attrib.hash := GetHash(attrib.name);
    attribs.Add(attrib);
  end;
end;
//------------------------------------------------------------------------------

function TXmlEl.GetAttribCount: integer;
begin
  Result := attribs.Count;
end;
//------------------------------------------------------------------------------

function TXmlEl.GetAttrib(index: integer): PSvgAttrib;
begin
  Result := PSvgAttrib(attribs[index]);
end;
//------------------------------------------------------------------------------

function IsTextAreaTbreak(var c: PUTF8Char; endC: PUTF8Char): Boolean;
const
  // https://www.w3.org/TR/SVGTiny12/text.html#tbreakElement
  tbreak: PUTF8Char = '<tbreak/>';
begin
  Result := (c + 9 < endC) and CompareMem(c, tbreak, 9);
  if Result then inc(c, 8);
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseContent(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  child             : TSvgXmlEl;
  entity            : PSvgAttrib;
  c2, cc            : PUTF8Char;
  tmpC, tmpEndC     : PUTF8Char;
begin
  Result := false;
  // note: don't trim spaces at the start of text content.
  // Text space trimming will be done later IF and when required.
  while (hash = hText) or (hash = hTSpan) or
    (hash = hTextArea) or SkipBlanks(c, endC) do
  begin
    if (c^ = '<') then
    begin
      inc(c);
      case c^ of
        '!':
          begin
            cc := c;
            if Match(cc, '!--') then             //start comment
            begin
              inc(cc, 3);
              while (cc < endC) and ((cc^ <> '-') or
                not Match(cc, '-->')) do inc(cc); //end comment
              inc(cc, 3);
            end else
            begin
              //it's very likely <![CDATA[
              c2 := cc - 1;
              if Match(cc, '![cdata[') then
              begin
                while (cc < endC) and ((cc^ <> ']') or not Match(cc, ']]>')) do
                  inc(cc);
                ToUTF8String(c2, cc, text);
                inc(cc, 3);
                if (hash = hStyle) then
                  ParseStyleElementContent(text, owner.classStyles);
              end else
              begin
                while (cc < endC) and (cc^ <> '<') do inc(cc);
                ToUTF8String(c2, cc, text);
              end;
            end;
            c := cc;
          end;
        '/', '?':
          begin
            //element closing tag
            cc := c;
            inc(cc);
            if Match(cc, name) then
            begin
              inc(cc, Length(name));
              //very rarely there's a space before '>'
              cc := SkipBlanksEx(cc, endC);
              Result := cc^ = '>';
              inc(cc);
            end;
            c := cc;
            Exit;
          end;
        else
        begin
          //starting a new element
          child := TSvgXmlEl.Create(owner);
          childs.Add(child);
          if not child.ParseHeader(c, endC) then break;
          if not child.selfClosed then
              child.ParseContent(c, endC);
        end;
      end;
    end
    else if c^ = '>' then
    begin
      break; //oops! something's wrong
    end
    else if (c^ = '&') and IsKnownEntity(owner, c, endC, entity) then
    begin
      tmpC := PUTF8Char(entity.value);
      tmpEndC := tmpC + Length(entity.value);
      ParseContent(tmpC, tmpEndC);
    end
    else if (hash = hTSpan) or (hash = hText) or (hash = hTextPath) then
    begin
      // assume this is text content, and because text can also be mixed
      // with any number of nested <tspan> elements, always put text
      // content inside a pseudo 'self closed' <tspan> element
      cc := c;
      while (cc < endC) and (cc^ <> '<') do inc(cc);
      child := TSvgXmlEl.Create(owner);
      child.name := 'tspan';
      child.hash := GetHash('tspan');
      child.selfClosed := true; ////////////////////// :)))
      childs.Add(child);
      ToUTF8String(c, cc, child.text, sitPreserve);
      c := cc;
    end
    else if (hash = hTextArea) then
    begin
      // also assume this is text content, but don't create
      // pseudo <tspan> elements inside <textarea> elements
      cc := c;
      while (cc < endC) and
        ((cc^ <> '<') or IsTextAreaTbreak(cc, endC)) do inc(cc);
      ToUTF8String(c, cc, text, sitPreserve);
      c := cc;
    end else
    begin
      cc := c;
      while (cc < endC) and (cc^ <> '<') do inc(cc);
      ToUTF8String(c, cc, text);
      c := cc;
      //if <style> element then load styles into owner.classStyles
      if (hash = hStyle) then
        ParseStyleElementContent(text, owner.classStyles);
    end;
  end;
end;

//------------------------------------------------------------------------------
// TDocTypeEl
//------------------------------------------------------------------------------

function TDocTypeEl.SkipWord(c, endC: PUTF8Char): PUTF8Char;
begin
  while (c < endC) and (c^ > space) do inc(c);
  inc(c);
  Result := c;
end;
//------------------------------------------------------------------------------

function TDocTypeEl.ParseEntities(var c, endC: PUTF8Char): Boolean;
var
  attrib: PSvgAttrib;
begin
  attrib := nil;
  inc(c); //skip opening '['
  while (c < endC) and SkipBlanks(c, endC) do
  begin
    if (c^ = ']') then break
    else if not Match(c, '<!entity') then
    begin
      while c^ > space do inc(c); //skip word.
      Continue;
    end;
    inc(c, 8);
    attrib := NewSvgAttrib();

    c := ParseAttribName(c, endC, attrib);
    if c >= endC then break;

    SkipBlanks(c, endC);
    if not IsQuoteChar(c^) then break;
    if not ParseQuotedString(c, endC, attrib.value) then break;
    attribs.Add(attrib);
    attrib := nil;
    SkipBlanks(c, endC);
    if c^ <> '>' then break;
    inc(c); //skip entity's trailing '>'
  end;
  if Assigned(attrib) then DisposeSvgAttrib(attrib);
  Result := (c < endC) and (c^ = ']');
  inc(c);
end;
//------------------------------------------------------------------------------

function TDocTypeEl.ParseAttributes(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  dummy : UTF8String;
begin
  while SkipBlanks(c, endC) do
  begin
    //we're currently only interested in ENTITY declarations
    case c^ of
      '[': ParseEntities(c, endC);
      '"', '''': ParseQuotedString(c, endC, dummy);
      '>': break;
      else c := SkipWord(c, endC);
    end;
  end;
  Result := (c < endC) and (c^ = '>');
  inc(c);
end;

//------------------------------------------------------------------------------
// TSvgTreeEl
//------------------------------------------------------------------------------

constructor TSvgXmlEl.Create(owner: TSvgParser);
begin
  inherited Create(owner);
  selfClosed := false;
end;
//------------------------------------------------------------------------------

procedure TSvgXmlEl.Clear;
var
  i: integer;
begin
  for i := 0 to childs.Count -1 do
    TSvgXmlEl(childs[i]).free;
  childs.Clear;
  inherited;
end;
//------------------------------------------------------------------------------

function TSvgXmlEl.ParseHeader(var c: PUTF8Char; endC: PUTF8Char): Boolean;
begin
  Result := inherited ParseHeader(c, endC);
  if Result then hash := GetHash(name);
end;
//------------------------------------------------------------------------------

//function TSvgTreeEl.ParseContent(var c: PUTF8Char; endC: PUTF8Char): Boolean;

constructor TSvgParser.Create;
begin
  classStyles := TClassStylesList.Create;
  svgStream   := TMemoryStream.Create;
  xmlHeader   := TXmlEl.Create(Self);
  docType     := TDocTypeEl.Create(Self);
  svgTree     := nil;
end;
//------------------------------------------------------------------------------

destructor TSvgParser.Destroy;
begin
  Clear;
  svgStream.Free;
  xmlHeader.Free;
  docType.Free;
  classStyles.Free;
end;
//------------------------------------------------------------------------------

procedure TSvgParser.Clear;
begin
  classStyles.Clear;
  svgStream.Clear;
  xmlHeader.Clear;
  docType.Clear;
  FreeAndNil(svgTree);
end;
//------------------------------------------------------------------------------

function TSvgParser.FindEntity(hash: Cardinal): PSvgAttrib;
var
  i: integer;
begin
  //there are usually so few, that there seems little point sorting etc.
  for i := 0 to docType.attribs.Count -1 do
  begin
    Result := PSvgAttrib(docType.attribs.List[i]);
    if Result.hash = hash then Exit;
  end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function TSvgParser.LoadFromFile(const filename: string): Boolean;
var
  fs: TFileStream;
begin
  Result := false;
  if not FileExists(filename) then Exit;

  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    Result := LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure ConvertUnicodeToUtf8(memStream: TMemoryStream);
var
  i, len: LongInt;
  encoding: TSvgEncoding;
  s: UnicodeString;
  wc: PWord;
  utf8: UTF8String;
begin
  memStream.Position := 0;
  encoding := GetXmlEncoding(memStream.Memory, memStream.Size);
  case encoding of
    eUnicodeLE, eUnicodeBE: ;
    else Exit;
  end;
  SetLength(s, memStream.Size div 2);
  Move(memStream.Memory^, s[1], memStream.Size);
  if encoding = eUnicodeBE then
  begin
    wc := @s[1];
    for i := 1 to Length(s) do
    begin
      wc^ := Swap(wc^);
      inc(wc);
    end;
  end;
  utf8 := UTF8Encode(s);
  len := Length(utf8);
  memStream.SetSize(len);
  Move(utf8[1], memStream.Memory^, len);
end;
//------------------------------------------------------------------------------

function TSvgParser.LoadFromStream(stream: TStream): Boolean;
begin
  Clear;
  Result := true;
  try
    svgStream.LoadFromStream(stream);
    // very few SVG files are unicode encoded, almost all are Utf8
    // so it's more efficient to parse them all as Utf8 encoded files
    ConvertUnicodeToUtf8(svgStream);
    ParseUtf8Stream;
  except
    Result := false;
  end;
end;
//------------------------------------------------------------------------------

function TSvgParser.LoadFromString(const str: string): Boolean;
var
  ss: TStringStream;
begin
{$IFDEF UNICODE}
  ss := TStringStream.Create(str, TEncoding.UTF8);
{$ELSE}
  ss := TStringStream.Create(UTF8Encode(str));
{$ENDIF}
  try
    Result := LoadFromStream(ss);
  finally
    ss.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgParser.ParseUtf8Stream;
var
  c, endC: PUTF8Char;
begin
  c := svgStream.Memory;
  endC := c + svgStream.Size;
  SkipBlanks(c, endC);
  if Match(c, '<?xml') then
  begin
    inc(c, 2); //todo: accommodate space after '<' eg using sMatchEl function
    if not xmlHeader.ParseHeader(c, endC) then Exit;
    SkipBlanks(c, endC);
  end;
  if Match(c, '<!doctype') then
  begin
    inc(c, 2);
    if not docType.ParseHeader(c, endC) then Exit;
  end;
  while SkipBlanks(c, endC) do
  begin
    if (c^ = '<') and Match(c, '<svg') then
    begin
      inc(c);
      svgTree := TSvgXmlEl.Create(self);
      if svgTree.ParseHeader(c, endC) and
        not svgTree.selfClosed then
          svgTree.ParseContent(c, endC);
      break;
    end;
    inc(c);
  end;
end;

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function DecodeUtf8ToUnicode(const utf8: UTF8String): UnicodeString;
var
  i,j, len: Integer;
  c, cp: Cardinal;
  codePoints: TArrayOfCardinal;
begin
  Result := '';
  if utf8 = '' then Exit;
  len := Length(utf8);

  // first decode utf8String to codepoints
  SetLength(codePoints, len);
  i := 1;
  j := 0;
  while i <= len do
  begin
    c := Ord(utf8[i]);
    if c and $80 = 0 then // c < 128
    begin
      codePoints[j] := c;
      inc(i); inc(j);
    end
    else if c and $E0 = $C0 then
    begin
      if i = len then break;
      codePoints[j] := (c and $1F) shl 6 + (Ord(utf8[i+1]) and $3F);
      inc(i, 2); inc(j);
    end
    else if c and $F0 = $E0 then
    begin
      if i > len - 2 then break;
      codePoints[j] := (c and $F) shl 12 +
        ((Ord(utf8[i+1]) and $3F) shl 6) + ((Ord(utf8[i+2]) and $3F));
      inc(i, 3); inc(j);
    end else
    begin
      if (i > len - 3) or (c shr 3 <> $1E) then break;
      codePoints[j] := (c and $7) shl 18 + ((Ord(utf8[i+1]) and $3F) shl 12) +
        ((Ord(utf8[i+2]) and $3F) shl 6) + (Ord(utf8[i+3]) and $3F);
      inc(i, 4); inc(j);
    end;
  end;
  len := j; // there are now 'j' valid codepoints
  j := 0;

  // make room in the result for surrogate paired chars, and
  // convert codepoints into the result (a Utf16 string)
  SetLength(Result, len *2);
  for i := 0 to len -1 do
  begin
    inc(j);
    cp := codePoints[i];
    if (cp < $D7FF) or (cp = $E000) or (cp = $FFFF) then
    begin
      Result[j] := WideChar(cp);
    end else if (cp > $FFFF) and (cp < $110000) then
    begin
      Dec(cp, $10000);
      Result[j] := WideChar($D800 + (cp shr 10));
      inc(j);
      Result[j] := WideChar($DC00 + (cp and $3FF));
    end;
  end;
  SetLength(Result, j);
end;

//------------------------------------------------------------------------------
// TValue
//------------------------------------------------------------------------------

function ConvertValue(const value: TValue; scale: double): double;
const
  mm  = 96 / 25.4;
  cm  = 96 / 2.54;
  rad = 180 / PI;
  pt  = 4 / 3;
begin
  //https://oreillymedia.github.io/Using_SVG/guide/units.html
  //todo: still lots of units to support (eg times for animation)
  with value do
    {if not IsValid or (rawVal = 0) then  // already checked by TValue.GetValue, the only function calling this code
      Result := 0
    else}
      case value.unitType of
        utNumber:
          Result := rawVal;
        utPercent:
          Result := rawVal * 0.01 * scale;
        utRadian:
          Result := rawVal * rad;
        utInch:
          Result := rawVal * 96;
        utCm:
          Result := rawVal * cm;
        utMm:
          Result := rawVal * mm;
        utEm:
          if scale <= 0 then
            Result := rawVal * 16 else
            Result := rawVal * scale;
        utEx:
          if scale <= 0 then
            Result := rawVal * 8 else
            Result := rawVal * scale * 0.5;
        utPica:
          Result := rawVal * 16;
        utPt:
          Result := rawVal * pt;
        else
          Result := rawVal;
      end;
end;
//------------------------------------------------------------------------------

procedure TValue.Init;
begin
  rawVal   := InvalidD;
  unitType := utNumber;
end;
//------------------------------------------------------------------------------

procedure TValue.SetValue(val: double; unitTyp: TUnitType);
begin
  rawVal  := val;
  unitType      := unitTyp;
end;
//------------------------------------------------------------------------------

function TValue.GetValue(relSize: double; assumeRelValBelow: Double): double;
begin
  if not IsValid or (rawVal = 0) then
    Result := 0
  else if IsRelativeValue(assumeRelValBelow) then
    Result := rawVal * relSize
  else
    Result := ConvertValue(self, relSize);
end;
//------------------------------------------------------------------------------

function TValue.GetValueXY(const relSize: TRectD; assumeRelValBelow: Double): double;
begin
  //https://www.w3.org/TR/SVG11/coords.html#Units
  Result := GetValue(Hypot(relSize.Width, relSize.Height)/sqrt2, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function  TValue.IsRelativeValue(assumeRelValBelow: double): Boolean;
begin
  Result := (unitType = utNumber) and (Abs(rawVal) <= assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValue.IsValid: Boolean;
begin
  Result := (unitType <> utUnknown) and Img32.Vector.IsValid(rawVal);
end;
//------------------------------------------------------------------------------

function TValue.HasFontUnits: Boolean;
begin
  case unitType of
    utEm, utEx: Result := true;
    else Result := False;
  end;
end;

//------------------------------------------------------------------------------

function TValue.HasAngleUnits: Boolean;
begin
  case unitType of
    utDegree, utRadian: Result := true;
    else Result := False;
  end;
end;

//------------------------------------------------------------------------------
// TValuePt
//------------------------------------------------------------------------------

procedure TValuePt.Init;
begin
  X.Init;
  Y.Init;
end;
//------------------------------------------------------------------------------

function TValuePt.GetPoint(const relSize: double; assumeRelValBelow: Double): TPointD;
begin
  Result.X := X.GetValue(relSize, assumeRelValBelow);
  Result.Y := Y.GetValue(relSize, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValuePt.GetPoint(const relSize: TRectD; assumeRelValBelow: Double): TPointD;
begin
  Result.X := X.GetValue(relSize.Width, assumeRelValBelow);
  Result.Y := Y.GetValue(relSize.Height, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValuePt.IsValid: Boolean;
begin
  Result := X.IsValid and Y.IsValid;
end;

//------------------------------------------------------------------------------
// TValueRec
//------------------------------------------------------------------------------

procedure TValueRecWH.Init;
begin
  left.Init;
  top.Init;
  width.Init;
  height.Init;
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectD(const relSize: TRectD; assumeRelValBelow: Double): TRectD;
begin
  with GetRectWH(relSize, assumeRelValBelow) do
  begin
    Result.Left :=Left;
    Result.Top := Top;
    Result.Right := Left + Width;
    Result.Bottom := Top + Height;
  end;
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectD(relSize: double; assumeRelValBelow: Double): TRectD;
begin
  if not left.IsValid then
    Result.Left := 0 else
    Result.Left := left.GetValue(relSize, assumeRelValBelow);

  if not top.IsValid then
    Result.Top := 0 else
    Result.Top := top.GetValue(relSize, assumeRelValBelow);

  Result.Right := Result.Left + width.GetValue(relSize, assumeRelValBelow);
  Result.Bottom := Result.Top + height.GetValue(relSize, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectD(relSizeX, relSizeY: double; assumeRelValBelow: Double): TRectD;
begin
  if not left.IsValid then
    Result.Left := 0 else
    Result.Left := left.GetValue(relSizeX, assumeRelValBelow);

  if not top.IsValid then
    Result.Top := 0 else
    Result.Top := top.GetValue(relSizeY, assumeRelValBelow);

  Result.Right := Result.Left + width.GetValue(relSizeX, assumeRelValBelow);
  Result.Bottom := Result.Top + height.GetValue(relSizeY, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectWH(const relSize: TRectD; assumeRelValBelow: Double): TRectWH;
begin
  if not left.IsValid then
    Result.Left := 0 else
    Result.Left := left.GetValue(relSize.Width, assumeRelValBelow);

  if not top.IsValid then
    Result.Top := 0 else
    Result.Top := top.GetValue(relSize.Height, assumeRelValBelow);

  Result.Width := width.GetValue(relSize.Width, assumeRelValBelow);
  Result.Height := height.GetValue(relSize.Height, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValueRecWH.IsValid: Boolean;
begin
  Result := width.IsValid and height.IsValid;
end;
//------------------------------------------------------------------------------

function TValueRecWH.IsEmpty: Boolean;
begin
  Result := (width.rawVal <= 0) or (height.rawVal <= 0);
end;

//------------------------------------------------------------------------------
// TClassStylesList
//------------------------------------------------------------------------------

procedure TClassStylesList.Grow(NewCapacity: Integer);
var
  Len, I: Integer;
  Index: Integer;
begin
  Len := Length(FItems);
  if NewCapacity < 0 then
  begin
    if Len < 5 then
      Len := 5
    else
      Len := Len * 2;
  end
  else if NewCapacity <= Len then
    Exit
  else
    Len := NewCapacity;

  SetLength(FItems, Len);
  FMod := Cardinal(Len);
  if not Odd(FMod) then
    Inc(FMod);
  SetLengthUninit(FBuckets, FMod);
  FillChar(FBuckets[0], FMod * SizeOf(FBuckets[0]), $FF);

  // Rehash
  for I := 0 to FCount - 1 do
  begin
    Index := (FItems[I].Hash and $7FFFFFFF) mod FMod;
    FItems[I].Next := FBuckets[Index];
    FBuckets[Index] := I;
  end;
end;
//------------------------------------------------------------------------------

procedure TClassStylesList.Preallocate(AdditionalItemCount: Integer);
begin
  if AdditionalItemCount > 2 then
    Grow(Length(FItems) + AdditionalItemCount);
end;
//------------------------------------------------------------------------------

function TClassStylesList.FindItemIndex(const Name: UTF8String): Integer;
begin
  Result := -1;
  FNameHash := GetHash(Name);
  if FMod <> 0 then
  begin
    Result := FBuckets[(FNameHash and $7FFFFFFF) mod FMod];
    while (Result <> -1) and
      ((FItems[Result].Hash <> FNameHash) or
      not IsSameUTF8String(FItems[Result].Name, Name)) do
        Result := FItems[Result].Next;
  end;
end;

//------------------------------------------------------------------------------

procedure TClassStylesList.AddAppendStyle(const Name, Style: UTF8String);
var
  Index: Integer;
  Item: PClassStyleListItem;
  Bucket: PInteger;
begin
  Index := FindItemIndex(Name);
  if Index <> -1 then
  begin
    Item := @FItems[Index];
    if (Item.Style <> '') and (Item.Style[Length(Item.Style)] <> ';') then
      Item.Style := Item.Style + ';' + Style
    else
      Item.Style := Item.Style + Style;
  end
  else
  begin
    if FCount = Length(FItems) then
      Grow;
    Index := FCount;
    Inc(FCount);

    Bucket := @FBuckets[(FNameHash and $7FFFFFFF) mod FMod];
    Item := @FItems[Index];
    Item.Next := Bucket^;
    Item.Hash := FNameHash;
    Item.Name := Name;
    Item.Style := style;
    Bucket^ := Index;
  end;
end;
//------------------------------------------------------------------------------

function TClassStylesList.GetStyle(const Name: UTF8String): UTF8String;
var
  Index: Integer;
begin
  if FCount = 0 then
    Result := ''
  else
  begin
    Index := FindItemIndex(Name);
    if Index <> -1 then
      Result := FItems[Index].Style
    else
      Result := '';
  end;
end;
//------------------------------------------------------------------------------

procedure TClassStylesList.Clear;
begin
  FCount := 0;
  FMod := 0;
  FItems := nil;
  FBuckets := nil;
end;

//------------------------------------------------------------------------------
// TColorConstList
//------------------------------------------------------------------------------

constructor TColorConstList.Create(Colors: PColorConst; Count: Integer);
var
  I: Integer;
  Bucket: PPColorConstMapItem;
  Item: PColorConstMapItem;
begin
  inherited Create;
  FCount := Count;
  SetLength(FItems, FCount);

  FMod := FCount * 2 + 1; // gives us 3 color constants as max. bucket depth
  SetLength(FBuckets, FMod);

  // Initialize FItems[] and fill the buckets
  for I := 0 to Count - 1 do
  begin
    Item := @FItems[I];
    Item.Data := Colors; // link the constant to the ColorConstMapItem
    Inc(Colors);
    Item.Hash := GetHash(Item.Data.ColorName); // case-insensitive
    Bucket := @FBuckets[(Cardinal(Item.Hash) and $7FFFFFFF) mod FMod];
    Item.Next := Bucket^;
    Bucket^ := Item;
  end;
end;
//------------------------------------------------------------------------------

function TColorConstList.GetColorValue(const ColorName: UTF8String; var Color: TColor32): Boolean;
var
  Hash: Cardinal;
  Item: PColorConstMapItem;
begin
  Hash := GetHash(ColorName);
  Item := FBuckets[(Cardinal(Hash) and $7FFFFFFF) mod FMod];
  while (Item <> nil) and
    not IsSameAsciiUTF8String(Item.Data.ColorName, ColorName) do
      Item := Item.Next;
  if Item <> nil then
  begin
    Color := Item.Data.ColorValue;
    Result := True;
  end
  else
    Result := False;
end;

//------------------------------------------------------------------------------
// initialization procedures
//------------------------------------------------------------------------------

procedure MakeLowerCaseTable;
var
  i: UTF8Char;
begin
  for i:= #0 to #$40 do LowerCaseTable[i]:= i;
  for i:= #$41 to #$5A do LowerCaseTable[i]:= UTF8Char(Ord(i) + $20);
  for i:= #$5B to #$FF do LowerCaseTable[i]:= i;
end;
//------------------------------------------------------------------------------

procedure MakeColorConstList;
  {$I Img32.SVG.HtmlColorConsts.inc}
begin
  ColorConstList := TColorConstList.Create(@ColorConsts, Length(ColorConsts));
end;
//------------------------------------------------------------------------------

procedure CleanupColorConstList;
begin
  FreeAndNil(ColorConstList);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  MakeLowerCaseTable;
  MakeColorConstList;

finalization
  CleanupColorConstList;
end.
