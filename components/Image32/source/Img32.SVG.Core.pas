unit Img32.SVG.Core;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.3                                                             *
* Date      :  27 September 2022                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2022                                         *
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
  SysUtils, Classes, Types, Math,
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
  TSvgTextAlign = (staUndefined, staLeft, staCenter, staRight);

  TSVGFontInfo = record
    family      : TTtfFontFamily;
    size        : double;
    spacing     : double;
    textLength  : double;
    italic      : TSvgItalicSyle;
    weight      : Integer;
    align       : TSvgTextAlign;
    decoration  : TFontDecoration;
    baseShift   : TValue;
  end;

  //////////////////////////////////////////////////////////////////////
  // TClassStylesList: custom TStringList that stores ansistring objects
  //////////////////////////////////////////////////////////////////////

  PAnsStringiRec = ^TAnsiStringRec;   //used internally by TClassStylesList
  TAnsiStringRec = record
    ansi  : UTF8String;
  end;

  TClassStylesList = class
  private
    fList : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function  AddAppendStyle(const classname: string; const ansi: UTF8String): integer;
    function  GetStyle(const classname: UTF8String): UTF8String;
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
    function    ParseAttribName(var c: PUTF8Char; endC: PUTF8Char; attrib: PSvgAttrib): Boolean;
    function    ParseAttribValue(var c: PUTF8Char; endC: PUTF8Char; attrib: PSvgAttrib): Boolean;
    function    ParseAttributes(var c: PUTF8Char; endC: PUTF8Char): Boolean; virtual;
    procedure   ParseStyleAttribute(const style: UTF8String);
    property    Attrib[index: integer]: PSvgAttrib read GetAttrib;
    property    AttribCount: integer read GetAttribCount;
  end;

  TDocTypeEl = class(TXmlEl)
  private
    procedure   SkipWord(var c, endC: PUTF8Char);
    function    ParseEntities(var c, endC: PUTF8Char): Boolean;
  public
    function    ParseAttributes(var c: PUTF8Char; endC: PUTF8Char): Boolean; override;
  end;

  TSvgTreeEl = class(TXmlEl)
  public
    constructor Create(owner: TSvgParser); override;
    procedure   Clear; override;
    function    ParseHeader(var c: PUTF8Char; endC: PUTF8Char): Boolean; override;
  end;

  TSvgParser = class
  private
    svgStream : TMemoryStream;
    procedure ParseStream;
  public
    classStyles :TClassStylesList;
    xmlHeader   : TXmlEl;
    docType     : TDocTypeEl;
    svgTree     : TSvgTreeEl;
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
  function ParseNextWordEx(var c: PUTF8Char; endC: PUTF8Char;
    out word: UTF8String): Boolean;
  function ParseNextNum(var c: PUTF8Char; endC: PUTF8Char;
    skipComma: Boolean; out val: double): Boolean;
  function ParseNextNumEx(var c: PUTF8Char; endC: PUTF8Char; skipComma: Boolean;
    out val: double; out unitType: TUnitType): Boolean;
  function GetHash(const name: UTF8String): cardinal;
  function GetHashCaseSensitive(name: PUTF8Char; nameLen: integer): cardinal;
  function ExtractRef(const href: UTF8String): UTF8String;
  function IsNumPending(var c: PUTF8Char;
    endC: PUTF8Char; ignoreComma: Boolean): Boolean;
  function UTF8StringToColor32(const value: UTF8String; var color: TColor32): Boolean;
  function MakeDashArray(const dblArray: TArrayOfDouble; scale: double): TArrayOfInteger;
  function Match(c: PUTF8Char; const compare: UTF8String): Boolean; overload;
  function Match(const compare1, compare2: UTF8String): Boolean; overload;
  function ToUTF8String(var c: PUTF8Char; endC: PUTF8Char): UTF8String;

  //special parsing functions //////////////////////////////////////////
  procedure ParseStyleElementContent(const value: UTF8String; stylesList: TClassStylesList);
  function ParseTransform(const transform: UTF8String): TMatrixD;

  procedure GetSvgFontInfo(const value: UTF8String; var fontInfo: TSVGFontInfo);
  function HtmlDecode(const html: UTF8String): UTF8String;

  function GetXmlEncoding(memory: Pointer; len: integer): TSvgEncoding;
  function ClampRange(val, min, max: double): double;

  function SkipBlanks(var c: PUTF8Char; endC: PUTF8Char): Boolean;
  function SkipBlanksAndComma(var current: PUTF8Char; currentEnd: PUTF8Char): Boolean;

type
  TSetOfUTF8Char = set of UTF8Char;
  UTF8Strings = array of UTF8String;

function CharInSet(chr: UTF8Char; chrs: TSetOfUTF8Char): Boolean;

const
  clInvalid   = $00010001;
  clCurrent   = $00010002;
  sqrt2       = 1.4142135623731;
  quote       = '''';
  dquote      = '"';
  space       = #32;
  SvgDecimalSeparator = '.'; //do not localize

  {$I Img32.SVG.HashConsts.inc}

var
  LowerCaseTable : array[#0..#255] of UTF8Char;
  ColorConstList : TStringList;

implementation


type
  TColorConst = record
    ColorName : string;
    ColorValue: Cardinal;
  end;

  TColorObj = class
    cc: TColorConst;
  end;

const
  buffSize    = 8;

  //include hashed html entity constants
  {$I Img32.SVG.HtmlHashConsts.inc}

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

function ClampRange(val, min, max: double): double;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  if val <= min then Result := min
  else if val >= max then Result := max
  else Result := val;
end;
//------------------------------------------------------------------------------

function CharInSet(chr: UTF8Char; chrs: TSetOfUTF8Char): Boolean;
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
    if LowerCaseTable[c^] <> compare[i] then Exit;
    inc(c);
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
    if LowerCaseTable[c1^] <> LowerCaseTable[c2^] then Exit;
    inc(c1); inc(c2);
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
  while (len > 0) and (str[len] <= #32) do dec(len);
  while (i <= len) and (str[i] <= #32) do inc(i);
  for j := i + 1 to len do
    if (str[j] <= #32) and (str[j -1] > #32) then inc(spcCnt);
  SetLength(Result, spcCnt +1);
  for k := 0 to spcCnt do
  begin
    j := i;
    while (j <= len) and (str[j] > #32) do inc(j);
    SetLength(Result[k], j -i);
    Move(str[i], Result[k][1], j -i);
    while (j <= len) and (str[j] <= #32) do inc(j);
    i := j;
  end;
end;
//------------------------------------------------------------------------------

function GetXmlEncoding(memory: Pointer; len: integer): TSvgEncoding;
var
  p: PUTF8Char;
begin
  Result := eUnknown;
  if (len < 4) or not Assigned(memory) then Exit;
  p := PUTF8Char(memory);
  case p^ of
    #$EF: if ((p +1)^ = #$BB) and ((p +2)^ = #$BF) then Result := eUtf8;
    #$FF: if ((p +1)^ = #$FE) then Result := eUnicodeLE;
    #$FE: if ((p +1)^ = #$FF) then Result := eUnicodeBE;
  end;
end;
//------------------------------------------------------------------------------

function SkipBlanks(var c: PUTF8Char; endC: PUTF8Char): Boolean;
begin
  while (c < endC) and (c^ <= space) do inc(c);
  Result := (c < endC);
end;
//------------------------------------------------------------------------------

function SkipBlanksAndComma(var current: PUTF8Char; currentEnd: PUTF8Char): Boolean;
begin
  Result := SkipBlanks(current, currentEnd);
  if not Result or (current^ <> ',') then Exit;
  inc(current);
  Result := SkipBlanks(current, currentEnd);
end;
//------------------------------------------------------------------------------

function SkipStyleBlanks(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  inComment: Boolean;
begin
  //style content may include multi-line comment blocks
  inComment := false;
  while (c < endC) do
  begin
    if inComment then
    begin
      if (c^ = '*') and ((c +1)^ = '/')  then
      begin
        inComment := false;
        inc(c);
      end;
    end
    else if (c^ > space) then
    begin
      inComment := (c^ = '/') and ((c +1)^ = '*');
      if not inComment then break;
    end;
    inc(c);
  end;
  Result := (c < endC);
end;
//------------------------------------------------------------------------------

function IsAlpha(c: UTF8Char): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := CharInSet(c, ['A'..'Z','a'..'z']);
end;
//------------------------------------------------------------------------------

function ParseStyleNameLen(var c: PUTF8Char; endC: PUTF8Char): integer;
var
  c2: PUTF8Char;
const
  validNonFirstChars =  ['0'..'9','A'..'Z','a'..'z','-'];
begin
  Result := 0;
  //nb: style names may start with a hyphen
  if (c^ = '-') then
  begin
    if not IsAlpha((c+1)^) then Exit;
  end
  else if not IsAlpha(c^) then Exit;

  c2 := c; inc(c);
  while (c < endC) and CharInSet(c^, validNonFirstChars) do inc(c);
  Result := c - c2;
end;
//------------------------------------------------------------------------------

function ParseNextWord(var c: PUTF8Char; endC: PUTF8Char; out word: UTF8String): Boolean;
var
  c2: PUTF8Char;
begin
  Result := SkipBlanksAndComma(c, endC);
  if not Result then Exit;
  c2 := c;
  while (c < endC) and
    (LowerCaseTable[c^] >= 'a') and (LowerCaseTable[c^] <= 'z') do
      inc(c);
  word := ToUTF8String(c2, c);
end;
//------------------------------------------------------------------------------

function ParseNextWordEx(var c: PUTF8Char; endC: PUTF8Char;
  out word: UTF8String): Boolean;
var
  isQuoted: Boolean;
  c2: PUTF8Char;
begin
  Result := SkipBlanksAndComma(c, endC);
  if not Result then Exit;
  isQuoted := (c^) = quote;
  if isQuoted then
  begin
    inc(c);
    c2 := c;
    while (c < endC) and (c^ <> quote) do inc(c);
    word := ToUTF8String(c2, c);
    inc(c);
  end else
  begin
    Result := CharInSet(LowerCaseTable[c^], ['A'..'Z', 'a'..'z']);
    if not Result then Exit;
    c2 := c;
    inc(c);
    while (c < endC) and
      CharInSet(LowerCaseTable[c^], ['A'..'Z', 'a'..'z', '-', '_']) do inc(c);
    word := ToUTF8String(c2, c);
  end;
end;
//------------------------------------------------------------------------------

function ParseNameLength(var c: PUTF8Char; endC: PUTF8Char): integer; overload;
var
  c2: PUTF8Char;
const
  validNonFirstChars =  ['0'..'9','A'..'Z','a'..'z','_',':','-'];
begin
  c2 := c;
  inc(c);
  while (c < endC) and CharInSet(c^, validNonFirstChars) do inc(c);
  Result := c - c2;
end;
//------------------------------------------------------------------------------

{$OVERFLOWCHECKS OFF}
function GetHash(const name: UTF8String): cardinal;
var
  i: integer;
  c: PUTF8Char;
begin
  //https://en.wikipedia.org/wiki/Jenkins_hash_function
  c := PUTF8Char(name);
  Result := 0;
  if c = nil then Exit;
  for i := 1 to Length(name) do
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
{$OVERFLOWCHECKS ON}
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
{$OVERFLOWCHECKS ON}
//------------------------------------------------------------------------------

function ParseNextWordHashed(var c: PUTF8Char; endC: PUTF8Char): cardinal;
var
  c2: PUTF8Char;
  name: UTF8String;
begin
  c2 := c;
  ParseNameLength(c, endC);
  name := ToUTF8String(c2, c);
  if name = '' then Result := 0
  else Result := GetHash(name);
end;
//------------------------------------------------------------------------------

function ParseNextNumEx(var c: PUTF8Char; endC: PUTF8Char; skipComma: Boolean;
  out val: double; out unitType: TUnitType): Boolean;
var
  decPos,exp: integer;
  isNeg, expIsNeg: Boolean;
  start: PUTF8Char;
begin
  Result := false;
  unitType := utNumber;

  //skip white space +/- single comma
  if skipComma then
  begin
    while (c < endC) and (c^ <= space) do inc(c);
    if (c^ = ',') then inc(c);
  end;
  while (c < endC) and (c^ <= space) do inc(c);
  if (c = endC) then Exit;

  decPos := -1; exp := Invalid; expIsNeg := false;
  isNeg := c^ = '-';
  if isNeg then inc(c);

  val := 0;
  start := c;
  while c < endC do
  begin
    if Ord(c^) = Ord(SvgDecimalSeparator) then
    begin
      if decPos >= 0 then break;
      decPos := 0;
    end
    else if (LowerCaseTable[c^] = 'e') and
      (CharInSet((c+1)^, ['-','0'..'9'])) then
    begin
      if (c +1)^ = '-' then expIsNeg := true;
      inc(c);
      exp := 0;
    end
    else if (c^ < '0') or (c^ > '9') then
      break
    else if IsValid(exp) then
    begin
      exp := exp * 10 + (Ord(c^) - Ord('0'))
    end else
    begin
      val := val *10 + Ord(c^) - Ord('0');
      if decPos >= 0 then inc(decPos);
    end;
    inc(c);
  end;
  Result := c > start;
  if not Result then Exit;

  if decPos > 0 then val := val * Power(10, -decPos);
  if isNeg then val := -val;
  if IsValid(exp) then
  begin
    if expIsNeg then
      val := val * Power(10, -exp) else
      val := val * Power(10, exp);
  end;

  //https://oreillymedia.github.io/Using_SVG/guide/units.html
  case c^ of
    '%':
      begin
        inc(c);
        unitType := utPercent;
      end;
    'c': //convert cm to pixels
      if ((c+1)^ = 'm') then
      begin
        inc(c, 2);
        unitType := utCm;
      end;
    'd': //ignore deg
      if ((c+1)^ = 'e') and ((c+2)^ = 'g') then
      begin
        inc(c, 3);
        unitType := utDegree;
      end;
    'e': //convert cm to pixels
      if ((c+1)^ = 'm') then
      begin
        inc(c, 2);
        unitType := utEm;
      end
      else if ((c+1)^ = 'x') then
      begin
        inc(c, 2);
        unitType := utEx;
      end;
    'i': //convert inchs to pixels
      if ((c+1)^ = 'n') then
      begin
        inc(c, 2);
        unitType := utInch;
      end;
    'm': //convert mm to pixels
      if ((c+1)^ = 'm') then
      begin
        inc(c, 2);
        unitType := utMm;
      end;
    'p':
      case (c+1)^ of
        'c':
          begin
            inc(c, 2);
            unitType := utPica;
          end;
        't':
          begin
            inc(c, 2);
            unitType := utPt;
          end;
        'x':
          begin
            inc(c, 2);
            unitType := utPixel;
          end;
      end;
    'r': //convert radian angles to degrees
      if Match(c, 'rad') then
      begin
        inc(c, 3);
        unitType := utRadian;
      end;
  end;
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
  Result := ToUTF8String(c2, c);
end;
//------------------------------------------------------------------------------

function ParseNextChar(var c: PUTF8Char; endC: PUTF8Char): UTF8Char;
begin
  Result := #0;
  if not SkipBlanks(c, endC) then Exit;
  Result := c^;
  inc(c);
end;
//------------------------------------------------------------------------------

function ParseQuoteChar(var c: PUTF8Char; endC: PUTF8Char): UTF8Char;
begin
  if SkipBlanks(c, endC) and (c^ in [quote, dquote]) then
  begin
    Result := c^;
    inc(c);
  end else
    Result := #0;
end;
//------------------------------------------------------------------------------

function AllTrim(var name: UTF8String): Boolean;
var
  i, len: integer;
begin
  len := Length(name);
  i := 0;
  while (len > 0) and (name[1] <= space) do
  begin
    inc(i); dec(len);
  end;
  if i > 0 then Delete(name, 1, i);
  Result := len > 0;
  if not Result then Exit;
  while name[len] <= space do dec(len);
  SetLength(name, len);
end;
//------------------------------------------------------------------------------

function ToUTF8String(var c: PUTF8Char; endC: PUTF8Char): UTF8String;
var
  len: integer;
begin
  len := endC - c;
  SetLength(Result, len);
  if len = 0 then Exit;
  Move(c^, Result[1], len * SizeOf(UTF8Char));
  c := endC;
end;
//------------------------------------------------------------------------------

function IsKnownEntity(owner: TSvgParser;
  var c: PUTF8Char; endC: PUTF8Char; out entity: PSvgAttrib): boolean;
var
  c2, c3: PUTF8Char;
  entityName: UTF8String;
begin
  inc(c); //skip ampersand.
  c2 := c; c3 := c;
  ParseNameLength(c3, endC);
  entityName := ToUTF8String(c2, c3);
  entity := owner.FindEntity(GetHash(entityName));
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
  quotStr := ToUTF8String(c2, c);
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
  Result := (c2 < endC) and (c2^ >= '0') and (c2^ <= '9');
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
    Result := MatrixMultiply(Result, mat);
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
        hSans_045_Serif   : fontInfo.family := ttfSansSerif;
        hSerif            : fontInfo.family := ttfSerif;
        hMonospace        : fontInfo.family := ttfMonospace;
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
          if (c^ >= 'a') and (c^ <= 'f') then
            val := val * 16 + Ord(c^) - 87
          else if (c^ >= 'A') and (c^ <= 'F') then
            val := val * 16 + Ord(c^) - 55
          else if (c^ >= '0') and (c^ <= '9') then
            val := val * 16 + Ord(c^) - 48
          else
          begin
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

    if ParseNextNumEx(c, endC, true, vals[3], mus[3]) then
      alpha := 255 else //stops further alpha adjustment
      vals[3] := 255;
    if ParseNextChar(c, endC) <> ')' then Exit;
    for i := 0 to 3 do if IsFraction(vals[i]) then
      vals[i] := vals[i] * 255;
    color := ClampByte(Round(vals[3])) shl 24 +
      ClampByte(Round(vals[0])) shl 16 +
      ClampByte(Round(vals[1])) shl 8 +
      ClampByte(Round(vals[2]));
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
  end else                                        //color name lookup
  begin
    i := ColorConstList.IndexOf(string(value));
    if i < 0 then Exit;
    color := TColorObj(ColorConstList.Objects[i]).cc.ColorValue;
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

function MakeDashArray(const dblArray: TArrayOfDouble; scale: double): TArrayOfInteger;
var
  i, len: integer;
  dist: double;
begin
  dist := 0;
  len := Length(dblArray);
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i] := Ceil(dblArray[i] * scale);
    dist := Result[i] + dist;
  end;

  if dist = 0 then
  begin
    Result := nil;
  end
  else if Odd(len) then
  begin
    SetLength(Result, len *2);
    Move(Result[0], Result[len], len * SizeOf(integer));
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
  names: array of string;

  procedure AddName(const name: string);
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

  SkipBlanks(c, endC);
  if Match(c, '<![cdata[') then inc(c, 9);

  while SkipStyleBlanks(c, endC) and
    CharInSet(LowerCaseTable[PeekNextChar(c, endC)],
      [SvgDecimalSeparator, '#', 'a'..'z']) do
  begin
    //get one or more class names for each pending style
    c2 := c;
    ParseNameLength(c, endC);
    aclassName := ToUTF8String(c2, c);

    AddName(Lowercase(String(aclassName)));
    if PeekNextChar(c, endC) = ',' then
    begin
      inc(c);
      Continue;
    end;
    if len = 0 then break;
    SetLength(names, len); //ie no more comma separated names

    //now get the style
    if PeekNextChar(c, endC) <> '{' then Break;
    inc(c);
    c2 := c;
    while (c < endC) and (c^ <> '}') do inc(c);
    if (c = endC) then break;
    aStyle := ToUTF8String(c2, c);

    //finally, for each class name add (or append) this style
    for i := 0 to High(names) do
      stylesList.AddAppendStyle(names[i], aStyle);
    names := nil;
    len := 0; cap := 0;
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
    Dispose(PSvgAttrib(attribs[i]));
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
  SkipBlanks(c, endC);
  c2 := c;;
  ParseNameLength(c, endC);
  name := ToUTF8String(c2, c);

  //load the class's style (ie undotted style) if found.
  style := owner.classStyles.GetStyle(name);
  if style <> '' then ParseStyleAttribute(style);

  Result := ParseAttributes(c, endC);
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseAttribName(var c: PUTF8Char;
  endC: PUTF8Char; attrib: PSvgAttrib): Boolean;
var
  c2: PUTF8Char;
  //attribName: UTF8String;
begin
  Result := SkipBlanks(c, endC);
  if not Result then Exit;
  c2 := c;
  ParseNameLength(c, endC);
  attrib.Name := ToUTF8String(c2, c);
  attrib.hash := GetHash(attrib.Name);
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseAttribValue(var c: PUTF8Char;
  endC: PUTF8Char; attrib: PSvgAttrib): Boolean;
var
  quoteChar : UTF8Char;
  c2, c3: PUTF8Char;
begin
  Result := ParseNextChar(c, endC) = '=';
  if not Result then Exit;
  quoteChar := ParseQuoteChar(c, endC);
  if quoteChar = #0 then Exit;
  //trim leading and trailing spaces
  while (c < endC) and (c^ <= space) do inc(c);
  c2 := c;
  while (c < endC) and (c^ <> quoteChar) do inc(c);
  c3 := c;
  while (c3 > c2) and ((c3 -1)^ <= space) do 
    dec(c3);
  attrib.value := ToUTF8String(c2, c3);
  inc(c); //skip end quote
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
    if CharInSet(c^, ['/', '?', '>']) then
    begin
      if (c^ <> '>') then
      begin
        inc(c);
        if (c^ <> '>') then Exit; //error
        selfClosed := true;
      end;
      inc(c);
      Result := true;
      break;
    end
    else if (c^ = 'x') and Match(c, 'xml:') then
    begin
      inc(c, 4); //ignore xml: prefixes
    end;

    New(attrib);
    if not ParseAttribName(c, endC, attrib) or
      not ParseAttribValue(c, endC, attrib) then
    begin
      Dispose(attrib);
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
  while SkipStyleBlanks(c, endC) do
  begin
    c2 := c;
    ParseStyleNameLen(c, endC);
    styleName := ToUTF8String(c2, c);
    if styleName = '' then Break;

    if (ParseNextChar(c, endC) <> ':') or  //syntax check
      not SkipBlanks(c,endC) then Break;

    c2 := c;
    inc(c);
    while (c < endC) and (c^ <> ';') do inc(c);
    styleVal := ToUTF8String(c2, c);
    AllTrim(styleVal);
    inc(c);

    new(attrib);
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

function TXmlEl.ParseContent(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  child: TSvgTreeEl;
  entity: PSvgAttrib;
  c2, tmpC, tmpEndC: PUTF8Char;
begin
  Result := false;
  while SkipBlanks(c, endC) do
  begin
    if (c^ = '<') then
    begin
      inc(c);
      case c^ of
        '!':
          begin
            if Match(c, '!--') then             //start comment
            begin
              inc(c, 3);
              while (c < endC) and ((c^ <> '-') or
                not Match(c, '-->')) do inc(c); //end comment
              inc(c, 3);
            end else
            begin
              //it's very likely <![CDATA[
              c2 := c - 1;
              if Match(c, '![cdata[') then
              begin
                while (c < endC) and ((c^ <> ']') or not Match(c, ']]>')) do
                  inc(c);
                text := ToUTF8String(c2, c);
                inc(c, 3);
                if (hash = hStyle) then
                  ParseStyleElementContent(text, owner.classStyles);
              end else
              begin
                while (c < endC) and (c^ <> '<') do inc(c);
                text := ToUTF8String(c2, c);
              end;
            end;
          end;
        '/', '?':
          begin
            //element closing tag
            inc(c);
            if Match(c, name) then
            begin
              inc(c, Length(name));
              //very rarely there's a space before '>'
              SkipBlanks(c, endC);
              Result := c^ = '>';
              inc(c);
            end;
            Exit;
          end;
        else
        begin
          //starting a new element
          child := TSvgTreeEl.Create(owner);
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
      //text content: and because text can be mixed with one or more
      //<tspan> elements we need to create sub-elements for each text block.
      //And <tspan> elements can even have <tspan> sub-elements.
      tmpC := c;
      //preserve a leading space
      if (tmpC -1)^ = space then dec(tmpC);
      while (c < endC) and (c^ <> '<') do inc(c);
      if (hash = hTextPath) then
      begin
        text := ToUTF8String(tmpC, c);
      end else
      begin
        child := TSvgTreeEl.Create(owner);
        childs.Add(child);
        child.text := ToUTF8String(tmpC, c);
      end;
    end else
    begin
      tmpC := c;
      while (c < endC) and (c^ <> '<') do inc(c);
      text := ToUTF8String(tmpC, c);

      //if <style> element then load styles into owner.classStyles
      if (hash = hStyle) then
        ParseStyleElementContent(text, owner.classStyles);
    end;
  end;
end;

//------------------------------------------------------------------------------
// TDocTypeEl
//------------------------------------------------------------------------------

procedure TDocTypeEl.SkipWord(var c, endC: PUTF8Char);
begin
  while (c < endC) and (c^ > space) do inc(c);
  inc(c);
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
    new(attrib);
    if not ParseAttribName(c, endC, attrib) then break;
    SkipBlanks(c, endC);
    if not (c^ in [quote, dquote]) then break;
    if not ParseQuotedString(c, endC, attrib.value) then break;
    attribs.Add(attrib);
    attrib := nil;
    SkipBlanks(c, endC);
    if c^ <> '>' then break;
    inc(c); //skip entity's trailing '>'
  end;
  if Assigned(attrib) then Dispose(attrib);
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
      else SkipWord(c, endC);
    end;
  end;
  Result := (c < endC) and (c^ = '>');
  inc(c);
end;

//------------------------------------------------------------------------------
// TSvgTreeEl
//------------------------------------------------------------------------------

constructor TSvgTreeEl.Create(owner: TSvgParser);
begin
  inherited Create(owner);
  selfClosed := false;
end;
//------------------------------------------------------------------------------

procedure TSvgTreeEl.Clear;
var
  i: integer;
begin
  for i := 0 to childs.Count -1 do
    TSvgTreeEl(childs[i]).free;
  childs.Clear;
  inherited;
end;
//------------------------------------------------------------------------------

function TSvgTreeEl.ParseHeader(var c: PUTF8Char; endC: PUTF8Char): Boolean;
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
    if PSvgAttrib(docType.attribs[i]).hash = hash then
    begin
      Result := PSvgAttrib(docType.attribs[i]);
      Exit;
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

function TSvgParser.LoadFromStream(stream: TStream): Boolean;
var
  i, len: LongInt;
  encoding: TSvgEncoding;
  s: UnicodeString;
  wc: PWord;
  utf8: UTF8String;
begin
  Clear;
  Result := true;
  try
    svgStream.LoadFromStream(stream);

    //check encoding and set to UTF-8 if necessary
    encoding := GetXmlEncoding(svgStream.Memory, svgStream.Size);
    case encoding of
      eUnicodeLE, eUnicodeBE:
        begin
          SetLength(s, svgStream.Size div 2);
          Move(svgStream.Memory^, s[1], svgStream.Size);
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
          svgStream.SetSize(len);
          Move(utf8[1], svgStream.Memory^, len);
        end;
    end;
    ParseStream;
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

procedure TSvgParser.ParseStream;
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
      svgTree := TSvgTreeEl.Create(self);
      if svgTree.ParseHeader(c, endC) and
        not svgTree.selfClosed then
          svgTree.ParseContent(c, endC);
      break;
    end;
    inc(c);
  end;
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
    if not IsValid or (rawVal = 0) then
      Result := 0
    else
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
  rawVal      := InvalidD;
  unitType          := utNumber;
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

constructor TClassStylesList.Create;
begin
  fList := TStringList.Create;
  fList.Duplicates := dupIgnore;
  fList.CaseSensitive := false;
  fList.Sorted := True;
end;
//------------------------------------------------------------------------------

destructor TClassStylesList.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------

function TClassStylesList.AddAppendStyle(const classname: string; const ansi: UTF8String): integer;
var
  i: integer;
  sr: PAnsStringiRec;
begin
  Result := fList.IndexOf(classname);
  if (Result >= 0) then
  begin
    sr := PAnsStringiRec(fList.Objects[Result]);
    i := Length(sr.ansi);
    if sr.ansi[i] <> ';' then
      sr.ansi := sr.ansi + ';' + ansi else
      sr.ansi := sr.ansi + ansi;
  end else
  begin
    new(sr);
    sr.ansi := ansi;
    Result := fList.AddObject(classname, Pointer(sr));
  end;
end;
//------------------------------------------------------------------------------

function TClassStylesList.GetStyle(const classname: UTF8String): UTF8String;
var
  i: integer;
begin
  SetLength(Result, 0);
  i := fList.IndexOf(string(className));
  if i >= 0 then
    Result := PAnsStringiRec(fList.objects[i]).ansi;
end;
//------------------------------------------------------------------------------

procedure TClassStylesList.Clear;
var
  i: integer;
begin
  for i := 0 to fList.Count -1 do
    Dispose(PAnsStringiRec(fList.Objects[i]));
  fList.Clear;
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
var
  i   : integer;
  co  : TColorObj;
  {$I Img32.SVG.HtmlColorConsts.inc}
begin
  ColorConstList := TStringList.Create;
  ColorConstList.CaseSensitive := false;
  //ColorConstList.OwnsObjects := true; //not all versions of Delphi
  ColorConstList.Capacity := Length(ColorConsts);
  for i := 0 to High(ColorConsts) do
  begin
    co := TColorObj.Create;
    co.cc := ColorConsts[i];
    ColorConstList.AddObject(co.cc.ColorName, co);
  end;
  ColorConstList.Sorted := true;
end;
//------------------------------------------------------------------------------

procedure CleanupColorConstList;
var
  i   : integer;
begin
  for i := 0 to ColorConstList.Count -1 do
    TColorObj(ColorConstList.Objects[i]).Free;
  ColorConstList.Free;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  MakeLowerCaseTable;
  MakeColorConstList;

finalization
  CleanupColorConstList;
end.
