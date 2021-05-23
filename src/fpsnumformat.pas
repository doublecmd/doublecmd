{@@ ----------------------------------------------------------------------------
  Unit @bold(fpsNumFormat) contains classes and procedures to analyze and process
  @bold(number formats).

  AUTHORS: Werner Pamler

  LICENSE: See the file COPYING.modifiedLGPL.txt, included in the Lazarus
            distribution, for details about the license.
-------------------------------------------------------------------------------}
unit fpsNumFormat;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  fpscommon;

const
  psOK = 0;
  psErrNoValidColorIndex = 1;
  psErrNoValidCompareNumber = 2;
  psErrUnknownInfoInBrackets = 3;
  psErrConditionalFormattingNotSupported = 4;
  psErrNoUsableFormat = 5;
  psErrNoValidNumberFormat = 6;
  psErrNoValidDateTimeFormat = 7;
  psErrQuoteExpected = 8;
  psErrMultipleCurrSymbols = 9;
  psErrMultipleFracSymbols = 10;
  psErrMultipleExpChars = 11;
  psErrGeneralExpected = 12;
  psAmbiguousSymbol = 13;
  psErrNoValidTextFormat = 14;

type
  {@@ Set of characters }
  TsDecsChars = set of char;

  {@@ Tokens used by the elements of the number format parser. If, e.g. a
    format string is "0.000" then the number format parser detects the following
    three tokens

      - nftIntZeroDigit with integer value 1  (i.e. 1 zero-digit for the integer part)
      - nftDecSep (i.e. decimal separator)
      - ntZeroDecs with integer value 2 (i.e. 3 decimal places. }
  TsNumFormatToken = (
    nftGeneral,            // token for "general" number format
    nftText,               // must be quoted, stored in TextValue
    nftThSep,              // ',', replaced by FormatSettings.ThousandSeparator
    nftDecSep,             // '.', replaced by FormatSettings.DecimalSeparator
    nftYear,               // 'y' or 'Y', count stored in IntValue
    nftMonth,              // 'm' or 'M', count stored in IntValue
    nftDay,                // 'd' or 'D', count stored in IntValue
    nftHour,               // 'h' or 'H', count stored in IntValue
    nftMinute,             // 'n' or 'N' (or 'm'/'M'), count stored in IntValue
    nftSecond,             // 's' or 'S', count stored in IntValue
    nftMilliseconds,       // 'z', 'Z', '0', count stored in IntValue
    nftAMPM,               //
    nftMonthMinute,        // 'm'/'M' or 'n'/'N', meaning depending on context
    nftDateTimeSep,        // '/' or ':', replaced by value from FormatSettings, stored in TextValue
    nftSign,               // '+' or '-', stored in TextValue
    nftSignBracket,        // '(' or ')' for negative values, stored in TextValue
    nftIntOptDigit,        // '#', count stored in IntValue
    nftIntZeroDigit,       // '0', count stored in IntValue
    nftIntSpaceDigit,      // '?', count stored in IntValue
    nftIntTh,              // '#,##0' sequence for nfFixed, count of 0 stored in IntValue
    nftZeroDecs,           // '0' after dec sep, count stored in IntValue
    nftOptDecs,            // '#' after dec sep, count stored in IntValue
    nftSpaceDecs,          // '?' after dec sep, count stored in IntValue
    nftExpChar,            // 'e' or 'E', stored in TextValue
    nftExpSign,            // '+' or '-' in exponent
    nftExpDigits,          // '0' digits in exponent, count stored in IntValue
    nftPercent,            // '%' percent symbol
    nftFactor,             // thousand separators at end of format string, each one divides value by 1000
    nftFracSymbol,         // '/' fraction symbol
    nftFracNumOptDigit,    // '#' in numerator, count stored in IntValue
    nftFracNumSpaceDigit,  // '?' in numerator, count stored in IntValue
    nftFracNumZeroDigit,   // '0' in numerator, count stored in IntValue
    nftFracDenomOptDigit,  // '#' in denominator, count stored in IntValue
    nftFracDenomSpaceDigit,// '?' in denominator, count stored in IntValue
    nftFracDenomZeroDigit, // '0' in denominator, count stored in IntValue
    nftFracDenom,          // specified denominator, value stored in IntValue
    nftCurrSymbol,         // e.g., '"€"' or '[$€]', stored in TextValue
    nftCountry,
    nftColor,              // e.g., '[red]', Color in IntValue
    nftCompareOp,
    nftCompareValue,
    nftSpace,
    nftEscaped,            // '\'
    nftRepeat,
    nftEmptyCharWidth,
    nftTextFormat          // '@'
  );

  {@@ Element of the parsed number format sequence. Each element is identified
    by a token and has optional parameters stored as integer, float, and/or text. 
    
    @member Token     Identifies the number format element
    @member IntValue  Integer value associated with the number format element 
    @member FloatValue Floating point value associated with the number format element
    @member TextValue  String value associated with the number format element }
  TsNumFormatElement = record
    Token: TsNumFormatToken;
    IntValue: Integer;
    FloatValue: Double;
    TextValue: String;
  end;

  {@@ Array of parsed number format elements }
  TsNumFormatElements = array of TsNumFormatElement;

  {@@ Summary information classifying a number format section }
  TsNumFormatKind = (nfkPercent, nfkExp, nfkCurrency, nfkFraction,
    nfkDate, nfkTime, nfkTimeInterval, nfkText,
    nfkHasColor, nfkHasThSep, nfkHasFactor);

  {@@ Set of summary elements classifying and describing a number format section }
  TsNumFormatKinds = set of TsNumFormatKind;

  {@@ Number format string can be composed of several parts separated by a
    semicolon. The number format parser extracts the format information into
    individual sections for each part }
  TsNumFormatSection = record
    {@@ Parser number format elements used by this section }
    Elements: TsNumFormatElements;
    {@@ Summary information describing the section }
    Kind: TsNumFormatKinds;
    {@@ Reconstructed number format identifier for the built-in fps formats }
    NumFormat: TsNumberFormat;
    {@@ Number of decimal places used by the format string }
    Decimals: Byte;
    {@@ Minimum number of digits before the decimal separator }
    MinIntDigits: Byte;
    {@@ Factor by which a number will be multiplied before converting to string }
    Factor: Double;
    {@@ Digits to be used for the integer part of a fraction }
    FracInt: Integer;
    {@@ Digits to be used for the numerator part of a fraction }
    FracNumerator: Integer;
    {@@ Digits to be used for the denominator part of a fraction }
    FracDenominator: Integer;
    {@@ Currency string to be used in case of currency/accounting formats }
    CurrencySymbol: String;
    {@@ Color to be used when displaying the converted string }
    Color: TsColor;
  end;

  {@@ Pointer to a parsed number format section }
  PsNumFormatSection = ^TsNumFormatSection;

  {@@ Array of parsed number format sections }
  TsNumFormatSections = array of TsNumFormatSection;

  { TsNumFormatParams }

  {@@ Describes a parsed number format and provides all the information to
    convert a number value to a number or date/time string. These data are created
    by the number format parser from a format string. }
  TsNumFormatParams = class(TObject)
  private
    FAllowLocalizedAMPM: Boolean;
  protected
    function GetNumFormat: TsNumberFormat; virtual;
    function GetNumFormatStr: String; virtual;
  public
    {@@ Array of the format sections }
    Sections: TsNumFormatSections;
    constructor Create;
    procedure DeleteElement(ASectionIndex, AElementIndex: Integer);
    procedure InsertElement(ASectionIndex, AElementIndex: Integer;
      AToken: TsNumFormatToken);
    function SectionsEqualTo(ASections: TsNumFormatSections): Boolean;
    procedure SetCurrSymbol(AValue: String);
    procedure SetDecimals(AValue: Byte);
    procedure SetNegativeRed(AEnable: Boolean);
    procedure SetThousandSep(AEnable: Boolean);
    property AllowLocalizedAMPM: boolean read FAllowLocalizedAMPM write FAllowLocalizedAMPM;
    property NumFormat: TsNumberFormat read GetNumFormat;
    property NumFormatStr: String read GetNumFormatStr;
  end;


  { TsNumFormatList }

  {@@ Class of number format parameters }
  TsNumFormatParamsClass = class of TsNumFormatParams;

  {@@ List containing parsed number format parameters }
  TsNumFormatList = class(TFPList)
  { private }
    FOwnsData: Boolean;
    function GetItem(AIndex: Integer): TsNumFormatParams;
    procedure SetItem(AIndex: Integer; const AValue: TsNumFormatParams);
  protected
    FFormatSettings: TFormatSettings;
    FClass: TsNumFormatParamsClass;
    procedure AddBuiltinFormats; virtual;
  public
    constructor Create(AFormatSettings: TFormatSettings; AOwnsData: Boolean);
    destructor Destroy; override;
    function AddFormat(ASections: TsNumFormatSections): Integer; overload;
    function AddFormat(AFormatStr: String): Integer; overload;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    function Find(ASections: TsNumFormatSections): Integer; overload;
    function Find(AFormatstr: String): Integer; overload;
    property Items[AIndex: Integer]: TsNumFormatParams read GetItem write SetItem; default;
  end;


  { TsNumFormatParser }

  TsNumFormatParser = class
  private
    FToken: Char;
    FCurrent: PChar;
    FStart: PChar;
    FEnd: PChar;
    FCurrSection: Integer;
    FStatus: Integer;
    function GetCurrencySymbol: String;
    function GetDecimals: byte;
    function GetFracDenominator: Integer;
    function GetFracInt: Integer;
    function GetFracNumerator: Integer;
    function GetFormatString: String;
    function GetNumFormat: TsNumberFormat;
    function GetParsedSectionCount: Integer;
    function GetParsedSections(AIndex: Integer): TsNumFormatSection;
    procedure SetDecimals(AValue: Byte);

  protected
    FFormatSettings: TFormatSettings;
    FSections: TsNumFormatSections;

    { Administration while scanning }
    procedure AddElement(AToken: TsNumFormatToken; AText: String); overload;
    procedure AddElement(AToken: TsNumFormatToken; AIntValue: Integer=0; AText: String = ''); overload;
    procedure AddElement(AToken: TsNumFormatToken; AFloatValue: Double); overload;
    procedure AddSection;
    procedure DeleteElement(ASection, AIndex: Integer);
    procedure InsertElement(ASection, AIndex: Integer; AToken: TsNumFormatToken; AText: String); overload;
    procedure InsertElement(ASection, AIndex: Integer; AToken: TsNumFormatToken; AIntValue: Integer); overload;
    procedure InsertElement(ASection, AIndex: Integer; AToken: TsNumFormatToken; AFloatValue: Double); overload;
    function NextToken: Char;
    function PrevToken: Char;

    { Scanning/parsing }
    procedure ScanAMPM;
    procedure ScanAndCount(ATestChar: Char; out ACount: Integer);
    procedure ScanBrackets;
    procedure ScanCondition(AFirstChar: Char);
    procedure ScanCurrSymbol;
    procedure ScanDateTime;
    procedure ScanFormat;
    procedure ScanGeneral;
    procedure ScanNumber;
    procedure ScanQuotedText;
    // Main scanner
    procedure Parse(const AFormatString: String);

    { Analysis while scanning }
    procedure AnalyzeColor(AValue: String);
    function AnalyzeCurrency(const AValue: String): Boolean;

    { Analysis after scanning }
    // General
    procedure CheckSections;
    procedure CheckSection(ASection: Integer);
    procedure FixMonthMinuteToken(var ASection: TsNumFormatSection);
    // Format string
    function BuildFormatString: String; virtual;

  public
    constructor Create(const AFormatString: String;
      const AFormatSettings: TFormatSettings);
    destructor Destroy; override;
    procedure ClearAll;
    function GetDateTimeCode(ASection: Integer): String;
    function IsDateTimeFormat: Boolean;
    function IsTimeFormat: Boolean;
    procedure LimitDecimals;

    property CurrencySymbol: String read GetCurrencySymbol;
    property Decimals: Byte read GetDecimals write SetDecimals;
    property FormatString: String read GetFormatString;
    property FracDenominator: Integer read GetFracDenominator;
    property FracInt: Integer read GetFracInt;
    property FracNumerator: Integer read GetFracNumerator;
    property NumFormat: TsNumberFormat read GetNumFormat;
    property ParsedSectionCount: Integer read GetParsedSectionCount;
    property ParsedSections[AIndex: Integer]: TsNumFormatSection read GetParsedSections;
    property Status: Integer read FStatus;
  end;


{ Utility functions }

function AddAMPM(const ATimeFormatString: String;
  const AFormatSettings: TFormatSettings): String;
function AddIntervalBrackets(AFormatString: String): String;

procedure BuildCurrencyFormatList(AList: TStrings;
  APositive: Boolean; AValue: Double; const ACurrencySymbol: String);

function BuildCurrencyFormatString(ANumberFormat: TsNumberFormat;
  const AFormatSettings: TFormatSettings; ADecimals, APosCurrFmt, ANegCurrFmt: Integer;
  ACurrencySymbol: String; Accounting: Boolean = false): String;
function BuildDateTimeFormatString(ANumberFormat: TsNumberFormat;
  const AFormatSettings: TFormatSettings; AFormatString: String = ''): String;
function BuildFractionFormatString(AMixedFraction: Boolean;
  ANumeratorDigits, ADenominatorDigits: Integer): String;
function BuildNumberFormatString(ANumberFormat: TsNumberFormat;
  const AFormatSettings: TFormatSettings; ADecimals: Integer = -1;
  AMinIntDigits: Integer = 1): String;

function BuildFormatStringFromSection(const ASection: TsNumFormatSection;
  AllowLocalizedAMPM: Boolean = true): String;

function ApplyTextFormat(AText: String; AParams: TsNumFormatParams): String;
function ConvertFloatToStr(AValue: Double; AParams: TsNumFormatParams;
  AFormatSettings: TFormatSettings): String;
function CountDecs(AFormatString: String; ADecChars: TsDecsChars = ['0']): Byte;

function GeneralFormatFloat(AValue: Double;
  AFormatSettings: TFormatSettings): String; inline;

function IsBoolValue(const AText, ATrueText, AFalseText: String;
  out AValue: Boolean): Boolean;

function IsCurrencyFormat(AFormat: TsNumberFormat): Boolean; overload;
function IsCurrencyFormat(ANumFormat: TsNumFormatParams): Boolean; overload;

function IsDateTimeFormat(AFormat: TsNumberFormat): Boolean; overload;
function IsDateTimeFormat(AFormatStr: String): Boolean; overload;
function IsDateTimeFormat(ANumFormat: TsNumFormatParams): Boolean; overload;

function IsDateTimeValue(AText: String; const AFormatSettings: TFormatSettings;
  out ADateTime: TDateTime; out ANumFormat: TsNumberFormat): Boolean;

function IsDateFormat(ANumFormat: TsNumFormatParams): Boolean;

function IsTimeFormat(AFormat: TsNumberFormat): Boolean; overload;
function IsTimeFormat(AFormatStr: String): Boolean; overload;
function IsTimeFormat(ANumFormat: TsNumFormatParams): Boolean; overload;
function IsLongTimeFormat(AFormatStr: String; ATimeSeparator: char): Boolean; overload;

function IsNumberValue(AText: String; AutoDetectNumberFormat: Boolean;
  const AFormatSettings: TFormatSettings; out ANumber: Double;
  out ANumFormat: TsNumberFormat; out ADecimals: Integer;
  out ACurrencySymbol, AWarning: String): Boolean;

function IsTimeIntervalFormat(ANumFormat: TsNumFormatParams): Boolean;

function IsTextFormat(ANumFormat: TsNumFormatParams): Boolean;

function MakeLongDateFormat(ADateFormat: String): String;
function MakeShortDateFormat(ADateFormat: String): String;
procedure MakeTimeIntervalMask(Src: String; var Dest: String);
function StripAMPM(const ATimeFormatString: String): String;

procedure InitFormatSettings(out AFormatSettings: TFormatSettings);
procedure ReplaceFormatSettings(var AFormatSettings: TFormatSettings;
  const ADefaultFormats: TFormatSettings);

function CreateNumFormatParams(ANumFormatStr: String;
  const AFormatSettings: TFormatSettings): TsNumFormatParams;

function ParamsOfNumFormatStr(ANumFormatStr: String;
  const AFormatSettings: TFormatSettings; var AResult: TsNumFormatParams): Integer;


implementation

uses
  StrUtils, Math, LazUTF8;

const
  { Array of format strings identifying the order of number and
    currency symbol of a positive currency value. The number is expected at
    index 0, the currency symbol at index 1 of the parameter array used by the
    fpc Format() function. }
  POS_CURR_FMT: array[0..3] of string = (
    ('%1:s%0:s'),        // 0: $1
    ('%0:s%1:s'),        // 1: 1$
    ('%1:s %0:s'),       // 2: $ 1
    ('%0:s %1:s')        // 3: 1 $
  );
  { Array of format strings identifying the order of number and
    currency symbol of a negative currency value. The sign is shown
    as a dash character ("-") or by means of brackets. The number
    is expected at index 0, the currency symbol at index 1 of the
    parameter array for the fpc Format() function. }
  NEG_CURR_FMT: array[0..15] of string = (
    ('(%1:s%0:s)'),      //  0: ($1)
    ('-%1:s%0:s'),       //  1: -$1
    ('%1:s-%0:s'),       //  2: $-1
    ('%1:s%0:s-'),       //  3: $1-
    ('(%0:s%1:s)'),      //  4: (1$)
    ('-%0:s%1:s'),       //  5: -1$
    ('%0:s-%1:s'),       //  6: 1-$
    ('%0:s%1:s-'),       //  7: 1$-
    ('-%0:s %1:s'),      //  8: -1 $
    ('-%1:s %0:s'),      //  9: -$ 1
    ('%0:s %1:s-'),      // 10: 1 $-
    ('%1:s %0:s-'),      // 11: $ 1-
    ('%1:s -%0:s'),      // 12: $ -1
    ('%0:s- %1:s'),      // 13: 1- $
    ('(%1:s %0:s)'),     // 14: ($ 1)
    ('(%0:s %1:s)')      // 15: (1 $)
  );

{==============================================================================}
{                         Float-to-string conversion                           }
{==============================================================================}

type
  { Set of parsed number format tokens }
  TsNumFormatTokenSet = set of TsNumFormatToken;

const
  { Set of tokens which terminate number information in a format string }
  TERMINATING_TOKENS: TsNumFormatTokenSet =
    [nftSpace, nftText, nftEscaped, nftPercent, nftCurrSymbol, nftSign, nftSignBracket];
  { Set of tokens which describe the integer part of a number format }
  INT_TOKENS: TsNumFormatTokenSet =
    [nftIntOptDigit, nftIntZeroDigit, nftIntSpaceDigit];
  { Set of tokens which describe the decimals of a number format }
  DECS_TOKENS: TsNumFormatTokenSet =
    [nftZeroDecs, nftOptDecs, nftSpaceDecs];
  { Set of tokens which describe the numerator of a fraction format }
  FRACNUM_TOKENS: TsNumFormatTokenSet =
    [nftFracNumOptDigit, nftFracNumZeroDigit, nftFracNumSpaceDigit];
  { Set of tokens which describe the denominator of a fraction format }
  FRACDENOM_TOKENS: TsNumFormatTokenSet =
    [nftFracDenomOptDigit, nftFracDenomZeroDigit, nftFracDenomSpaceDigit, nftFracDenom];
  { Set of tokens which describe the exponent in exponential formatting of a number }
  EXP_TOKENS: TsNumFormatTokenSet =
    [nftExpDigits];   // todo: expand by optional digits (0.00E+#)

{ Helper function which checks whether a sequence of format tokens for
  exponential formatting begins at the specified index in the format elements }
function CheckExp(const AElements: TsNumFormatElements; AIndex: Integer): Boolean;
var
  numEl: Integer;
  i: Integer;
begin
  numEl := Length(AElements);

  Result := (AIndex < numEl) and (AElements[AIndex].Token in INT_TOKENS);
  if not Result then
    exit;

  numEl := Length(AElements);
  i := AIndex + 1;
  while (i < numEl) and (AElements[i].Token in INT_TOKENS) do inc(i);

  // no decimal places
  if (i+2 < numEl) and
     (AElements[i].Token = nftExpChar) and
     (AElements[i+1].Token = nftExpSign) and
     (AElements[i+2].Token in EXP_TOKENS)
  then begin
    Result := true;
    exit;
  end;

  // with decimal places
  if (i < numEl) and (AElements[i].Token = nftDecSep) //and (AElements[i+1].Token in DECS_TOKENS)
  then begin
    inc(i);
    while (i < numEl) and (AElements[i].Token in DECS_TOKENS) do inc(i);
    if (i + 2 < numEl) and
       (AElements[i].Token = nftExpChar) and
       (AElements[i+1].Token = nftExpSign) and
       (AElements[i+2].Token in EXP_TOKENS)
    then begin
      Result := true;
      exit;
    end;
  end;

  Result := false;
end;

{ Helper function which checks whether a sequence of format tokens for
  fraction formatting begins at the specified index in the format elements }
function CheckFraction(const AElements: TsNumFormatElements; AIndex: Integer;
  out digits: Integer): Boolean;
var
  numEl: Integer;
  i: Integer;
begin
  digits := 0;
  numEl := Length(AElements);

  Result := (AIndex < numEl);
  if not Result then
    exit;

  i := AIndex;
  // Check for mixed fraction (integer split off, sample format "# ??/??"
  if (AElements[i].Token in (INT_TOKENS + [nftIntTh])) then
  begin
    inc(i);
    while (i < numEl) and (AElements[i].Token in (INT_TOKENS + [nftIntTh])) do inc(i);
    while (i < numEl) and (AElements[i].Token in TERMINATING_TOKENS) do inc(i);
  end;

  if (i = numEl) or not (AElements[i].Token in FRACNUM_TOKENS) then
    exit(false);

  // Here follows the ordinary fraction (no integer split off); sample format "??/??"
  while (i < numEl) and (AElements[i].Token in FRACNUM_TOKENS) do inc(i);
  while (i < numEl) and (AElements[i].Token in TERMINATING_TOKENS) do inc(i);
  if (i = numEl) or (AElements[i].Token <> nftFracSymbol) then
    exit(False);

  inc(i);
  while (i < numEl) and (AElements[i].Token in TERMINATING_TOKENS) do inc(i);
  if (i = numEl) or (not (AElements[i].Token in FRACDENOM_TOKENS)) then
    exit(false);

  while (i < numEL) and (AElements[i].Token in FRACDENOM_TOKENS) do
  begin
    case AElements[i].Token of
      nftFracDenomZeroDigit : inc(digits, AElements[i].IntValue);
      nftFracDenomSpaceDigit: inc(digits, AElements[i].IntValue);
      nftFracDenomOptDigit  : inc(digits, AElements[i].IntValue);
      nftFracDenom          : digits := -AElements[i].IntValue;  // "-" indicates a literal denominator value!
    end;
    inc(i);
  end;
  Result := true;
end;

{ Processes a sequence of #, 0, and ? tokens.
  Adds leading (GrowRight=false) or trailing (GrowRight=true) zeros and/or
  spaces as specified by the format elements to the number value string.
  On exit AIndex points to the first non-integer token. }
function ProcessIntegerFormat(AValue: String; AFormatSettings: TFormatSettings;
  const AElements: TsNumFormatElements; var AIndex: Integer;
  ATokens: TsNumFormatTokenSet; GrowRight, UseThSep: Boolean): String;
const
  OptTokens = [nftIntOptDigit, nftFracNumOptDigit, nftFracDenomOptDigit, nftOptDecs];
  ZeroTokens = [nftIntZeroDigit, nftFracNumZeroDigit, nftFracDenomZeroDigit, nftZeroDecs, nftIntTh];
  SpaceTokens = [nftIntSpaceDigit, nftFracNumSpaceDigit, nftFracDenomSpaceDigit, nftSpaceDecs];
  AllOptTokens = OptTokens + SpaceTokens;
var
  fs: TFormatSettings absolute AFormatSettings;
  i, j, L: Integer;
  numEl: Integer;
begin
  Result := AValue;
  numEl := Length(AElements);
  if GrowRight then
  begin
    // This branch is intended for decimal places, i.e. there may be trailing zeros.
    i := AIndex;
    if (AValue = '0') and (AElements[i].Token in AllOptTokens) then
      Result := '';
    // Remove trailing zeros
    while (Result <> '') and (Result[Length(Result)] = '0')
      do Delete(Result, Length(Result), 1);
    // Add trailing zeros or spaces as required by the elements.
    i := AIndex;
    L := 0;
    while (i < numEl) and (AElements[i].Token in ATokens) do
    begin
      if AElements[i].Token in ZeroTokens then
      begin
        inc(L, AElements[i].IntValue);
        while Length(Result) < L do Result := Result + '0'
      end else
      if AElements[i].Token in SpaceTokens then
      begin
        inc(L, AElements[i].IntValue);
        while Length(Result) < L do Result := Result + ' ';
      end;
      inc(i);
    end;
    if UseThSep then begin
      j := 2;
      while (j < Length(Result)) and (Result[j-1] <> ' ') and (Result[j] <> ' ') do
      begin
        Insert(fs.ThousandSeparator, Result, 1);
        inc(j, 3);
      end;
    end;
    AIndex := i;
  end else
  begin
    // This branch is intended for digits (or integer and numerator parts of fractions)
    // --> There are no leading zeros.
    // Find last digit token of the sequence
    i := AIndex;
    while (i < numEl) and (AElements[i].Token in ATokens) do
      inc(i);
    j := i;
    if i > 0 then dec(i);
    if (AValue = '0') and (AElements[i].Token in AllOptTokens) and (i = AIndex) then
      Result := '';
    // From the end of the sequence, going backward, add leading zeros or spaces
    // as required by the elements of the format.
    L := 0;
    while (i >= AIndex) do begin
      if AElements[i].Token in ZeroTokens then
      begin
        inc(L, AElements[i].IntValue);
        while Length(Result) < L do Result := '0' + Result;
      end else
      if AElements[i].Token in SpaceTokens then
      begin
        inc(L, AElements[i].IntValue);
        while Length(Result) < L do Result := ' ' + Result;
      end;
      dec(i);
    end;
    AIndex := j;
    if UseThSep then
    begin
     // AIndex := j + 1;
      j := Length(Result) - 2;
      while (j > 1) and (Result[j-1] <> ' ') and (Result[j] <> ' ') do
      begin
        Insert(fs.ThousandSeparator, Result, j);
        dec(j, 3);
      end;
    end;
  end;
end;

{ Converts the floating point number to an exponential number string according
  to the format specification in AElements.
  It must have been verified before, that the elements in fact are valid for
  an exponential format. }
function ProcessExpFormat(AValue: Double; AFormatSettings: TFormatSettings;
  const AElements: TsNumFormatElements; var AIndex: Integer): String;
var
  fs: TFormatSettings absolute AFormatSettings;
  expchar: String;
  expSign: String;
  se, si, sd: String;
  decs, expDigits: Integer;
  intZeroDigits, intOptDigits, intSpaceDigits: Integer;
  numStr: String;
  i, id, p: Integer;
  numEl: Integer;
begin
  Result := '';
  numEl := Length(AElements);

  // Determine digits of integer part of mantissa
  intZeroDigits := 0;
  intOptDigits := 0;
  intSpaceDigits := 0;
  i := AIndex;
  while (AElements[i].Token in INT_TOKENS) do begin
    case AElements[i].Token of
      nftIntZeroDigit : inc(intZeroDigits, AElements[i].IntValue);
      nftIntSpaceDigit: inc(intSpaceDigits, AElements[i].IntValue);
      nftIntOptDigit  : inc(intOptDigits, AElements[i].IntValue);
    end;
    inc(i);
  end;

  // No decimal places
  if (i + 2 < numEl) and (AElements[i].Token = nftExpChar) then
  begin
    expChar := AElements[i].TextValue;
    expSign := AElements[i+1].TextValue;
    expDigits := 0;
    i := i+2;
    while (i < numEl) and (AElements[i].Token in EXP_TOKENS) do
    begin
      inc(expDigits, AElements[i].IntValue);  // not exactly what Excel does... Rather exotic case...
      inc(i);
    end;
    numstr := FormatFloat('0'+expChar+expSign+DupeString('0', expDigits), AValue, fs);
    p := pos('e', Lowercase(numStr));
    se := copy(numStr, p, Length(numStr));   // exp part of the number string, incl "E"
    numStr := copy(numstr, 1, p-1);          // mantissa of the number string
    numStr := ProcessIntegerFormat(numStr, fs, AElements, AIndex, INT_TOKENS, false, false);
    Result := numStr + se;
    AIndex := i;
  end
  else
  // With decimal places
  if (i + 1 < numEl) and (AElements[i].Token = nftDecSep) then
  begin
    inc(i);
    id := i;     // index of decimal elements
    decs := 0;
    while (i < numEl) and (AElements[i].Token in DECS_TOKENS) do
    begin
      case AElements[i].Token of
        nftZeroDecs,
        nftSpaceDecs: inc(decs, AElements[i].IntValue);
      end;
      inc(i);
    end;
    expChar := AElements[i].TextValue;
    expSign := AElements[i+1].TextValue;
    expDigits := 0;
    inc(i, 2);
    while (i < numEl) and (AElements[i].Token in EXP_TOKENS) do
    begin
      inc(expDigits, AElements[i].IntValue);
      inc(i);
    end;
    if decs=0 then
      numstr := FormatFloat('0'+expChar+expSign+DupeString('0', expDigits), AValue, fs)
    else
      numStr := FloatToStrF(AValue, ffExponent, decs+1, expDigits, fs);
    if (abs(AValue) >= 1.0) and (expSign = '-') then
      Delete(numStr, pos('+', numStr), 1);
    p := pos('e', Lowercase(numStr));
    se := copy(numStr, p, Length(numStr));    // exp part of the number string, incl "E"
    numStr := copy(numStr, 1, p-1);           // mantissa of the number string
    p := pos(fs.DecimalSeparator, numStr);
    if p = 0 then
    begin
      si := numstr;
      sd := '';
    end else
    begin
      si := ProcessIntegerFormat(copy(numStr, 1, p-1), fs, AElements, AIndex, INT_TOKENS, false, false);  // integer part of the mantissa
      sd := ProcessIntegerFormat(copy(numStr, p+1, Length(numStr)), fs, AElements, id, DECS_TOKENS, true, false);  // fractional part of the mantissa
    end;
    // Put all parts together...
    Result := si + fs.DecimalSeparator + sd + se;
    AIndex := i;
  end;
end;

function ProcessFracFormat(AValue: Double; const AFormatSettings: TFormatSettings;
  ADigits: Integer; const AElements: TsNumFormatElements;
  var AIndex: Integer): String;
var
  fs: TFormatSettings absolute AFormatSettings;
  frint, frnum, frdenom, maxdenom: Int64;
  sfrint, sfrnum, sfrdenom: String;
  sfrsym, sintnumspace, snumsymspace, ssymdenomspace: String;
  i, numEl: Integer;
begin
  sintnumspace := '';
  snumsymspace := '';
  ssymdenomspace := '';
  sfrsym := '/';
  if ADigits >= 0 then
    maxDenom := Round(IntPower(10, ADigits));
  numEl := Length(AElements);

  i := AIndex;
  if AElements[i].Token in (INT_TOKENS + [nftIntTh]) then begin
    // Split-off integer
    if (AValue >= 1) then
    begin
      frint := trunc(AValue);
      AValue := frac(AValue);
    end else
      frint := 0;
    if ADigits >= 0 then
      FloatToFraction(AValue, maxdenom, frnum, frdenom)
    else
    begin
      frdenom := -ADigits;
      frnum := round(AValue*frdenom);
    end;
    sfrint := ProcessIntegerFormat(IntToStr(frint), fs, AElements, i,
      INT_TOKENS + [nftIntTh], false, (AElements[i].Token = nftIntTh));
    while (i < numEl) and (AElements[i].Token in TERMINATING_TOKENS) do
    begin
      sintnumspace := sintnumspace + AElements[i].TextValue;
      inc(i);
    end;
  end else
  begin
    // "normal" fraction
    sfrint := '';
    if ADigits > 0 then
      FloatToFraction(AValue, maxdenom, frnum, frdenom)
    else
    begin
      frdenom := -ADigits;
      frnum := round(AValue*frdenom);
    end;
    sintnumspace := '';
  end;

  // numerator and denominator
  sfrnum := ProcessIntegerFormat(IntToStr(frnum), fs, AElements, i,
    FRACNUM_TOKENS, false, false);
  while (i < numEl) and (AElements[i].Token in TERMINATING_TOKENS) do
  begin
    snumsymspace := snumsymspace + AElements[i].TextValue;
    inc(i);
  end;
  inc(i);  // fraction symbol
  while (i < numEl) and (AElements[i].Token in TERMINATING_TOKENS) do
  begin
    ssymdenomspace := ssymdenomspace + AElements[i].TextValue;
    inc(i);
  end;

  sfrdenom := ProcessIntegerFormat(IntToStr(frdenom), fs, AElements, i,
    FRACDENOM_TOKENS, false, false);
  AIndex := i+1;

  // Special cases
  if (frnum = 0) then
  begin
    if sfrnum = '' then begin
      sintnumspace := '';
      snumsymspace := '';
      ssymdenomspace := '';
      sfrdenom := '';
      sfrsym := '';
    end else
    if trim(sfrnum) = '' then begin
      sfrdenom := DupeString(' ', Length(sfrdenom));
      sfrsym := ' ';
    end;
  end;
  if sfrint = '' then sintnumspace := '';

  // Compose result string
  Result := sfrnum + snumsymspace + sfrsym + ssymdenomspace + sfrdenom;
  if (Trim(Result) = '') and (sfrint = '') then
    sfrint := '0';
  if sfrint <> '' then
    Result := sfrint + sintnumSpace + result;
end;

function ProcessFloatFormat(AValue: Double; AFormatSettings: TFormatSettings;
  const AElements: TsNumFormatElements; var AIndex: Integer): String;
var
  fs: TFormatSettings absolute AFormatSettings; // just to ease typing...
  numEl: Integer;
  numStr, s: String;
  p, i: Integer;
  decs: Integer;
  useThSep: Boolean;
  decsIndex: Integer;
begin
  Result := '';
  numEl := Length(AElements);
  useThSep := AElements[AIndex].Token = nftIntTh;

  // Find the element index of the decimal separator
  i := AIndex;
  while (i < numEl) and (AElements[i].Token <> nftDecSep) do
    inc(i);

  // No decimal separator --> format as integer
  if i >= numEl then begin
    // fpsUtils.Round() avoids Banker's rounding
    Result := ProcessIntegerFormat(IntToStr(fpsCommon.Round(AValue)), fs,
      AElements, AIndex, (INT_TOKENS + [nftIntTh]), false, useThSep);
    exit;
  end;

  // There is a decimal separator. Get the count of decimal places.
  decs := 0;
  inc(i);
  decsIndex := i;
  while (i < numEl) and (AElements[i].Token in DECS_TOKENS) do begin
    inc(decs, AElements[i].IntValue);
    inc(i);
  end;

  // Convert value to string; this will do some rounding if required.
  numstr := FloatToStrF(AValue, ffFixed, MaxInt, decs, fs);

  // Process the integer part of the rounded number string
  p := pos(fs.DecimalSeparator, numstr);
  if p > 0 then s := copy(numstr, 1, p-1) else s := numstr;
  Result := ProcessIntegerFormat(s, fs, AElements, AIndex,
    (INT_TOKENS + [nftIntTh]), false, UseThSep);

  // Process the fractional part of the rounded number string
  if p > 0 then begin
    s := Copy(numstr, p+1, Length(numstr));
    AIndex := decsIndex;
    s := ProcessIntegerFormat(s, fs, AElements, AIndex, DECS_TOKENS, true, false);
    if s <> '' then
      Result := Result + fs.DecimalSeparator + s;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Converts a floating point number to a string as determined by the specified
  number format parameters

  @param   AValue           Value to be converted to a string
  @param   AParams          Number format parameters which will be applied in the conversion. The number format params are obtained by the number format parser from the number format string.
  @param   AFormatSettings  Format settings needed by the number format parser for the conversion
  @returns Converted string
-------------------------------------------------------------------------------}
function ConvertFloatToStr(AValue: Double; AParams: TsNumFormatParams;
  AFormatSettings: TFormatSettings): String;

  { Returns true if s represent the value 0; it can be written in various
    ways: '0', '0.00', '0,000.0', '0.00E+10' etc. }
  function IsZeroStr(s: String): Boolean;
  var
    i: Integer;
  begin
    Result := false;
    for i:=1 to Length(s) do
      case s[i] of
        'e', 'E': break;
        '1'..'9': exit;
      end;
    Result := true;
  end;

var
  fs: TFormatSettings absolute AFormatSettings;
  sidx: Integer;
  section: TsNumFormatSection;
  i, el, numEl: Integer;
  isNeg: Boolean;
  yr, mon, day, hr, min, sec, ms: Word;
  s: String;
  digits: Integer;
begin
  Result := '';
  if IsNaN(AValue) then
    exit;

  if AParams = nil then
  begin
    Result := GeneralFormatFloat(AValue, fs);
    exit;
  end;

  sidx := 0;
  if (AValue < 0) and (Length(AParams.Sections) > 1) then
    sidx := 1;
  if (AValue = 0) and (Length(AParams.Sections) > 2) then
    sidx := 2;
  isNeg := (AValue < 0);
  AValue := abs(AValue);   // section 0 adds the sign back, section 1 has the sign in the elements
  section := AParams.Sections[sidx];
  numEl := Length(section.Elements);

  if nfkPercent in section.Kind then
    AValue := AValue * 100.0;
  if nfkHasFactor in section.Kind then
    AValue := AValue * section.Factor;
  if nfkTime in section.Kind then
    DecodeTime(AValue, hr, min, sec, ms);
  if nfkDate in section.Kind then
    DecodeDate(AValue, yr, mon, day);

  el := 0;
  while (el < numEl) do begin
    if section.Elements[el].Token = nftGeneral then
    begin
      s := GeneralFormatFloat(AValue, fs);
      if (sidx=0) and isNeg then s := '-' + s;
      Result := Result + s;
    end
    else
    // Integer token: can be the start of a number, exp, or mixed fraction format
    // Cases with thousand separator are handled here as well.
    if section.Elements[el].Token in (INT_TOKENS + [nftIntTh]) then begin
      // Check for exponential format
      if CheckExp(section.Elements, el) then
        s := ProcessExpFormat(AValue, fs, section.Elements, el)
      else
      // Check for fraction format
      if CheckFraction(section.Elements, el, digits) then
        s := ProcessFracFormat(AValue, fs, digits, section.Elements, el)
      else
      // Floating-point or integer
        s := ProcessFloatFormat(AValue, fs, section.Elements, el);
      if (sidx = 0) and isNeg and not IsZeroStr(s) then s := '-' + s;
      Result := Result + s;
      Continue;
    end
    else
    // Regular fraction (without integer being split off)
    if (section.Elements[el].Token in FRACNUM_TOKENS) and
       CheckFraction(section.Elements, el, digits) then
    begin
      s := ProcessFracFormat(AValue, fs, digits, section.Elements, el);
      if (sidx = 0) and isNeg then s := '-' + s;
      Result := Result + s;
      Continue;
    end
    else
      case section.Elements[el].Token of
        nftSpace, nftText, nftEscaped, nftCurrSymbol,
        nftSign, nftSignBracket, nftPercent:
          Result := Result + section.Elements[el].TextValue;

        nftEmptyCharWidth:
          Result := Result + ' ';

        nftDateTimeSep:
          case section.Elements[el].TextValue of
            '/': Result := Result + fs.DateSeparator;
            ':': Result := Result + fs.TimeSeparator;
            else Result := Result + section.Elements[el].TextValue;
          end;

        nftDecSep:
          Result := Result + fs.DecimalSeparator;

        nftThSep:
          Result := Result + fs.ThousandSeparator;

        nftYear:
          case section.Elements[el].IntValue of
            1,
            2: Result := Result + IfThen(yr mod 100 < 10, '0'+IntToStr(yr mod 100), IntToStr(yr mod 100));
            4: Result := Result + IntToStr(yr);
          end;

        nftMonth:
          case section.Elements[el].IntValue of
            1: Result := Result + IntToStr(mon);
            2: Result := Result + IfThen(mon < 10, '0'+IntToStr(mon), IntToStr(mon));
            3: Result := Result + fs.ShortMonthNames[mon];
            4: Result := Result + fs.LongMonthNames[mon];
          end;

        nftDay:
          case section.Elements[el].IntValue of
            1: result := result + IntToStr(day);
            2: result := Result + IfThen(day < 10, '0'+IntToStr(day), IntToStr(day));
            3: Result := Result + fs.ShortDayNames[DayOfWeek(AValue)];
            4: Result := Result + fs.LongDayNames[DayOfWeek(AValue)];
          end;

        nftHour:
          begin
            if section.Elements[el].IntValue < 0 then  // This case is for nfTimeInterval
              s := IntToStr(Int64(hr) + trunc(AValue) * 24)
            else
            if section.Elements[el].TextValue = 'AM' then  // This tag is set in case of AM/FM format
            begin
              hr := hr mod 12;
              if hr = 0 then hr := 12;
              s := IntToStr(hr)
            end else
              s := IntToStr(hr);
            if (abs(section.Elements[el].IntValue) = 2) and (Length(s) = 1) then
              s := '0' + s;
            Result := Result + s;
          end;

        nftMinute:
          begin
            if section.Elements[el].IntValue < 0 then  // case for nfTimeInterval
              s := IntToStr(int64(min) + trunc(AValue) * 24 * 60)
            else
              s := IntToStr(min);
            if (abs(section.Elements[el].IntValue) = 2) and (Length(s) = 1) then
              s := '0' + s;
            Result := Result + s;
          end;

       nftSecond:
          begin
            if section.Elements[el].IntValue < 0 then  // case for nfTimeInterval
              s := IntToStr(Int64(sec) + trunc(AValue) * 24 * 60 * 60)
            else
              s := IntToStr(sec);
            if (abs(section.Elements[el].IntValue) = 2) and (Length(s) = 1) then
              s := '0' + s;
            Result := Result + s;
          end;

        nftMilliseconds:
          case section.Elements[el].IntValue of
            1: Result := Result + IntToStr(round(ms/100));
            2: Result := Result + Format('%.2d', [round(ms/10)]);
            3: Result := Result + Format('%.3d', [ms]);
          end;

        nftAMPM:
          begin
            s := section.Elements[el].TextValue;
            if lowercase(s) = 'ampm' then
              s := IfThen(frac(AValue) < 0.5, fs.TimeAMString, fs.TimePMString)
            else
            begin
              i := pos('/', s);
              if i > 0 then
                s := IfThen(frac(AValue) < 0.5, copy(s, 1, i-1), copy(s, i+1, Length(s)))
              else
                s := IfThen(frac(AValue) < 0.5, 'AM', 'PM');
            end;
            Result := Result + s;
          end;
      end;  // case
    inc(el);
  end;  // while
end;

function GeneralFormatFloat(AValue: Double;
  AFormatSettings: TFormatSettings): String;
begin
  Result := FloatToStrF(AValue, ffGeneral, 15, 15, AFormatSettings);
  // 15 is for best rounding results.
  // Note: Still more than Excel whichrounds pi to 9 digits only.
end;


{==============================================================================}
{                           Utility functions                                  }
{==============================================================================}

{@@ ----------------------------------------------------------------------------
  Adds an AM/PM format code to a pre-built time formatting string.

  Example:  ATimeFormatString = 'hh:nn' ==> 'hh:nn AM/PM'

  @param    ATimeFormatString  String of time formatting codes (such as 'hh:nn')
  @param    AFormatSettings    FormatSettings for locale-dependent information
  @returns  Formatting string with AM/PM option activated.
-------------------------------------------------------------------------------}
function AddAMPM(const ATimeFormatString: String;
  const AFormatSettings: TFormatSettings): String;
begin
  Result := Format('%s AM/PM', [StripAMPM(ATimeFormatString)]);
end;

{@@ ----------------------------------------------------------------------------
  The given format string is assumed to represent a time interval, i.e. its
  first time symbol must be enclosed by square brackets. Checks if this is true,
  and adds the brackes if not.

  @param    AFormatString   String with time formatting codes
  @returns  Unchanged format string if its first time code is in square brackets (as in '[h]:nn:ss'). If not, the first time code is enclosed in square brackets.
-------------------------------------------------------------------------------}
function AddIntervalBrackets(AFormatString: String): String;
var
  p: Integer;
  s1, s2: String;
begin
  if AFormatString[1] = '[' then
    Result := AFormatString
  else begin
    p := pos(':', AFormatString);
    if p <> 0 then begin
      s1 := copy(AFormatString, 1, p-1);
      s2 := copy(AFormatString, p, Length(AFormatString));
      Result := Format('[%s]%s', [s1, s2]);
    end else
      Result := Format('[%s]', [AFormatString]);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Builds a string list with samples of the predefined currency formats

  @param  AList            String list in which the format samples are stored
  @param  APositive        If @true, samples are built for positive currency values, otherwise for negative values
  @param  AValue           Currency value to be used when calculating the sample strings
  @param  ACurrencySymbol  Currency symbol string to be used in the samples
-------------------------------------------------------------------------------}
procedure BuildCurrencyFormatList(AList: TStrings;
  APositive: Boolean; AValue: Double; const ACurrencySymbol: String);
var
  valueStr: String;
  i: Integer;
begin
  valueStr := Format('%.0n', [AValue]);
  AList.BeginUpdate;
  try
    if AList.Count = 0 then
    begin
      if APositive then
        for i:=0 to High(POS_CURR_FMT) do
          AList.Add(Format(POS_CURR_FMT[i], [valueStr, ACurrencySymbol]))
      else
        for i:=0 to High(NEG_CURR_FMT) do
          AList.Add(Format(NEG_CURR_FMT[i], [valueStr, ACurrencySymbol]));
    end else
    begin
      if APositive then
        for i:=0 to High(POS_CURR_FMT) do
          AList[i] := Format(POS_CURR_FMT[i], [valueStr, ACurrencySymbol])
      else
        for i:=0 to High(NEG_CURR_FMT) do
          AList[i] := Format(NEG_CURR_FMT[i], [valueStr, ACurrencySymbol]);
    end;
  finally
    AList.EndUpdate;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Builds a currency format string. The presentation of negative values (brackets,
  or minus signs) is taken from the provided format settings. The format string
  consists of three sections, separated by semicolons.

  Example: '"$"#,##0.00;("$"#,##0.00);"$"0.00'

  @param  ANumberFormat   Identifier of the built-in number format for which the format string is to be generated.
  @param  AFormatSettings FormatSettings to be applied (used to extract default values for the parameters following)
  @param  ADecimals       number of decimal places. If < 0, the CurrencyDecimals of the FormatSettings is used.
  @param  APosCurrFmt     Identifier for the order of currency symbol, value and spaces of positive values - see pcfXXXX constants in fpsTypes.pas. If < 0, the CurrencyFormat of the FormatSettings is used.
  @param  ANegCurrFmt     Identifier for the order of currency symbol, value and spaces of negative values. Specifies also usage of (). - see ncfXXXX constants in fpsTypes.pas. If < 0, the NegCurrFormat of the FormatSettings is used.
  @param  ACurrencySymbol String to identify the currency, like $ or USD. If ? the CurrencyString of the FormatSettings is used.
  @param  Accounting      If true, adds spaces for alignment of decimals

  @returns                String of formatting codes
-------------------------------------------------------------------------------}
function BuildCurrencyFormatString(ANumberFormat: TsNumberFormat;
  const AFormatSettings: TFormatSettings;
  ADecimals, APosCurrFmt, ANegCurrFmt: Integer; ACurrencySymbol: String;
  Accounting: Boolean = false): String;
var
  decs: String;
  pcf, ncf: Byte;
  p, n: String;
  negRed: Boolean;
begin
  pcf := IfThen(APosCurrFmt < 0, AFormatSettings.CurrencyFormat, APosCurrFmt);
  ncf := IfThen(ANegCurrFmt < 0, AFormatSettings.NegCurrFormat, ANegCurrFmt);
  if (ADecimals < 0) then
    ADecimals := AFormatSettings.CurrencyDecimals;
  if ACurrencySymbol = '?' then
    ACurrencySymbol := AFormatSettings.CurrencyString;
  if ACurrencySymbol <> '' then
    ACurrencySymbol := '[$' + ACurrencySymbol + ']';
  //    ACurrencySymbol := '"' + ACurrencySymbol + '"';  // <-- not good for biff2
  decs := DupeString('0', ADecimals);
  if ADecimals > 0 then decs := '.' + decs;

  negRed := (ANumberFormat = nfCurrencyRed);
  p := POS_CURR_FMT[pcf];   // Format mask for positive values
  n := NEG_CURR_FMT[ncf];   // Format mask for negative values

  // add extra space for the sign of the number for perfect alignment in Excel
  if Accounting then
    case ncf of
      0, 14: p := p + '_)';
      3, 11: p := p + '_-';
      4, 15: p := '_(' + p;
      5, 8 : p := '_-' + p;
    end;

  if ACurrencySymbol <> '' then begin
    Result := Format(p, ['#,##0' + decs, ACurrencySymbol]) + ';'
            + IfThen(negRed, '[red]', '')
            + Format(n, ['#,##0' + decs, ACurrencySymbol]) + ';'
            + Format(p, ['0'+decs, ACurrencySymbol]);
  end
  else begin
    Result := '#,##0' + decs;
    if negRed then
      Result := Result +';[red]'
    else
      Result := Result +';';
    case ncf of
      0, 14, 15           : Result := Result + '(#,##0' + decs + ')';
      1, 2, 5, 6, 8, 9, 12: Result := Result + '-#,##0' + decs;
      else                  Result := Result + '#,##0' + decs + '-';
    end;
    Result := Result + ';0' + decs;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Builds a date/time format string from the number format code.

  @param    ANumberFormat    Built-in number format identifier
  @param    AFormatSettings  Format settings from which locale-dependent information like day-month-year order is taken.
  @param    AFormatString    Optional pre-built formatting string. It is used only for the format nfInterval where square brackets are added to the first time code field.
  @returns  String of date/time formatting code constructed from the built-in format information.
-------------------------------------------------------------------------------}
function BuildDateTimeFormatString(ANumberFormat: TsNumberFormat;
  const AFormatSettings: TFormatSettings; AFormatString: String = '') : string;
var
  i, j: Integer;
  Unwanted: set of ansichar;
begin
  case ANumberFormat of
    nfShortDateTime:
      Result := AFormatSettings.ShortDateFormat + ' ' + AFormatSettings.ShortTimeFormat;
      // In the DefaultFormatSettings this is: d/m/y hh:nn
    nfShortDate:
      Result := AFormatSettings.ShortDateFormat;   // --> d/m/y
    nfLongDate:
      Result := AFormatSettings.LongDateFormat;    // --> dd mm yyyy
    nfShortTime:
      Result := StripAMPM(AFormatSettings.ShortTimeFormat);    // --> hh:nn
    nfLongTime:
      Result := StripAMPM(AFormatSettings.LongTimeFormat);     // --> hh:nn:ss
    nfShortTimeAM:
      begin                                       // --> hh:nn AM/PM
        Result := AFormatSettings.ShortTimeFormat;
        if (pos('a', lowercase(AFormatSettings.ShortTimeFormat)) = 0) then
          Result := AddAMPM(Result, AFormatSettings);
      end;
    nfLongTimeAM:                                 // --> hh:nn:ss AM/PM
      begin
        Result := AFormatSettings.LongTimeFormat;
        if pos('a', lowercase(AFormatSettings.LongTimeFormat)) = 0 then
          Result := AddAMPM(Result, AFormatSettings);
      end;
    nfDayMonth,                                  // --> dd/mmm
    nfMonthYear:                                 // --> mmm/yy
      begin
        Result := AFormatSettings.ShortDateFormat;
        case ANumberFormat of
          nfDayMonth:
            unwanted := ['y', 'Y'];
          nfMonthYear:
            unwanted := ['d', 'D'];
        end;
        for i:=Length(Result) downto 1 do
          if Result[i] in unwanted then Delete(Result, i, 1);
        while not (Result[1] in (['m', 'M', 'd', 'D', 'y', 'Y'] - unwanted)) do
          Delete(Result, 1, 1);
        while not (Result[Length(Result)] in (['m', 'M', 'd', 'D', 'y', 'Y'] - unwanted)) do
          Delete(Result, Length(Result), 1);
        i := 1;
        while not (Result[i] in ['m', 'M']) do inc(i);
        j := i;
        while (j <= Length(Result)) and (Result[j] in ['m', 'M']) do inc(j);
        while (j - i < 3) do begin
          Insert(Result[i], Result, j);
          inc(j);
        end;
      end;
    nfTimeInterval:                               // --> [h]:nn:ss
      if AFormatString = '' then
        Result := '[h]:nn:ss'
      else
        Result := AddIntervalBrackets(AFormatString);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Builds a number format string for fraction formatting from the number format
  code and the count of numerator and denominator digits.

  @param   AMixedFraction     If @TRUE, fraction is presented as mixed fraction
  @param   ANumeratorDigits   Count of numerator digits
  @param   ADenominatorDigits Count of denominator digits. If the value is negative then its absolute value is inserted literally as as denominator.

  @returns String of formatting code, here something like: '##/##' or '# ##/##'
-------------------------------------------------------------------------------}
function BuildFractionFormatString(AMixedFraction: Boolean;
  ANumeratorDigits, ADenominatorDigits: Integer): String;
begin
  if ADenominatorDigits < 0 then  // a negative value indicates a fixed denominator value
    Result := Format('%s/%d', [
      DupeString('?', ANumeratorDigits), -ADenominatorDigits
    ])
  else
    Result := Format('%s/%s', [
      DupeString('?', ANumeratorDigits), DupeString('?', ADenominatorDigits)
    ]);
  if AMixedFraction then
    Result := '# ' + Result;
end;

{@@ ----------------------------------------------------------------------------
  Builds a number format string from the number format code and the count of
  decimal places.

  Example:  ANumberFormat = nfFixedTh, ADecimals = 2 --> '#,##0.00'

  @param  ANumberFormat   Identifier of the built-in numberformat for which a format string is to be generated
  @param  AFormatSettings FormatSettings for default parameters
  @param  ADecimals       Number of decimal places. If < 0 the CurrencyDecimals value of the FormatSettings is used. In case of a fraction format "ADecimals" refers to the maximum count digits of the denominator.
  @param  AMinIntDigits   Minimum count of integer digits, i.e. count of '0' in the format string before the decimal separator

  @returns String of formatting codes

-------------------------------------------------------------------------------}
function BuildNumberFormatString(ANumberFormat: TsNumberFormat;
  const AFormatSettings: TFormatSettings; ADecimals: Integer = -1;
  AMinIntDigits: Integer = 1): String;
var
  decdigits: String;
  intdigits: String;
begin
  Result := '';
  if AMinIntDigits > 0 then
    intdigits := DupeString('0', AMinIntDigits)
  else
    intdigits := '#';
  if ADecimals = -1 then
    ADecimals := AFormatSettings.CurrencyDecimals;
  if ADecimals > 0 then
    decdigits := '.' + DupeString('0', ADecimals)
  else
    decdigits := '';
  case ANumberFormat of
    nfText:
      Result := '@';
    nfFixed:
      Result := intdigits + decdigits;
    nfFixedTh:
      begin
        while Length(IntDigits) < 4 do intDigits := '#' + intdigits;
        System.Insert(',', intdigits, Length(intdigits)-2);
        Result := intdigits + decdigits;
      end;
    nfExp:
      Result := intdigits + decdigits + 'E+00';
    nfPercentage:
      Result := intdigits + decdigits + '%';
    nfFraction:
      if ADecimals = 0 then    // "ADecimals" has a different meaning here...
        Result := '# ??/??'    // This is the default fraction format
      else
      begin
        decdigits := DupeString('?', ADecimals);
        Result := '# ' + decdigits + '/' + decdigits;
      end;
    nfCurrency, nfCurrencyRed:
      Result := BuildCurrencyFormatString(ANumberFormat, AFormatSettings,
        ADecimals, AFormatSettings.CurrencyFormat, AFormatSettings.NegCurrFormat,
        AFormatSettings.CurrencyString);
    nfShortDateTime, nfShortDate, nfLongDate, nfShortTime, nfLongTime,
    nfShortTimeAM, nfLongTimeAM, nfDayMonth, nfMonthYear, nfTimeInterval:
      raise EFPSpreadsheet.Create('BuildNumberFormatString: Use BuildDateTimeFormatSstring '+
        'to create a format string for date/time values.');
  end;
end;

{@@ ----------------------------------------------------------------------------
  Creates a format string for the specified parsed number format section.
  The format string is created according to Excel convention (which is understood
  by ODS as well).

  @param   ASection            Parsed section of number format elements as created by the number format parser
  @param   AllowLocalizedAMPM  Replaces "AMPM" in a time format string by "AM/PM". "AMPM" is allowed by FPS, but not by Excel. When converting a time to string it is replaced by the localized strings FormatSettings.TimeAMString/.TimePMString.

  @returns Excel-compatible format string
-------------------------------------------------------------------------------}
function BuildFormatStringFromSection(const ASection: TsNumFormatSection;
  AllowLocalizedAMPM: Boolean = true): String;
var
  element: TsNumFormatElement;
  i, n: Integer;
begin
  Result := '';

  for i := 0 to High(ASection.Elements)  do begin
    element := ASection.Elements[i];
    case element.Token of
      nftGeneral:
        Result := Result + 'General';
      nftIntOptDigit, nftOptDecs, nftFracNumOptDigit, nftFracDenomOptDigit:
        if element.IntValue > 0 then
          Result := Result + DupeString('#', element.IntValue);
      nftIntZeroDigit, nftZeroDecs, nftFracNumZeroDigit, nftFracDenomZeroDigit, nftExpDigits:
        if element.IntValue > 0 then
          Result := result + DupeString('0', element.IntValue);
      nftIntSpaceDigit, nftSpaceDecs, nftFracNumSpaceDigit, nftFracDenomSpaceDigit:
        if element.Intvalue > 0 then
          Result := result + DupeString('?', element.IntValue);
      nftFracDenom:
        Result := Result + IntToStr(element.IntValue);
      nftIntTh:
        case element.Intvalue of
          0: Result := Result + '#,###';
          1: Result := Result + '#,##0';
          2: Result := Result + '#,#00';
          3: Result := Result + '#,000';
        end;
      nftDecSep, nftThSep:
        Result := Result + element.TextValue;
      nftFracSymbol:
        Result := Result + '/';
      nftPercent:
        Result := Result + '%';
      nftFactor:
        if element.IntValue <> 0 then
        begin
          n := element.IntValue;
          while (n > 0) do
          begin
            Result := Result + element.TextValue;
            dec(n);
          end;
        end;
      nftSpace:
        Result := Result + ' ';
      nftText:
        if element.TextValue <> '' then result := Result + '"' + element.TextValue + '"';
      nftYear:
        Result := Result + DupeString('y', element.IntValue);
      nftMonth:
        Result := Result + DupeString('m', element.IntValue);
      nftDay:
        Result := Result + DupeString('d', element.IntValue);
      nftHour:
        if element.IntValue < 0
          then Result := Result + '[' + DupeString('h', -element.IntValue) + ']'
          else Result := Result + DupeString('h', element.IntValue);
      nftMinute:
        if element.IntValue < 0
          then Result := result + '[' + DupeString('m', -element.IntValue) + ']'
          else Result := Result + DupeString('m', element.IntValue);
      nftSecond:
        if element.IntValue < 0
          then Result := Result + '[' + DupeString('s', -element.IntValue) + ']'
          else Result := Result + DupeString('s', element.IntValue);
      nftMilliseconds:
        Result := Result + DupeString('0', element.IntValue);
      nftAMPM:
        if Lowercase(element.TextValue) = 'ampm' then
          Result := Result + 'AM/PM'
        else if element.TextValue <> '' then
          Result := Result + element.TextValue;
      nftSign, nftSignBracket, nftExpChar, nftExpSign, nftDateTimeSep:
        if element.TextValue <> '' then Result := Result + element.TextValue;
      nftCurrSymbol:
        if element.TextValue <> '' then
          Result := Result + '[$' + element.TextValue + ']';
      nftEscaped:
        if element.TextValue <> '' then
          Result := Result + '\' + element.TextValue;
      nftRepeat:
        if element.TextValue <> '' then Result := Result + '*' + element.TextValue;
      nftColor:
        case element.IntValue of
          scBlack  : Result := '[black]';
          scWhite  : Result := '[white]';
          scRed    : Result := '[red]';
          scBlue   : Result := '[blue]';
          scGreen  : Result := '[green]';
          scYellow : Result := '[yellow]';
          scMagenta: Result := '[magenta]';
          scCyan   : Result := '[cyan]';
          else       Result := Format('[Color%d]', [element.IntValue]);
        end;
      nftTextFormat:
        Result := '@';
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Counts how many decimal places are coded into a given number format string.

  @param    AFormatString   String with number format codes, such as '0.000'
  @param    ADecChars       Characters which are considered as symbols for decimals. For the fixed decimals, this is the '0'. Optional decimals are encoced as '#'.
  @returns  Count of decimal places found
-------------------------------------------------------------------------------}
function CountDecs(AFormatString: String; ADecChars: TsDecsChars = ['0']): Byte;
var
  i: Integer;
begin
  Result := 0;
  i := 1;
  while (i <= Length(AFormatString)) do begin
    if AFormatString[i] = '.' then begin
      inc(i);
      while (i <= Length(AFormatString)) and (AFormatString[i] in ADecChars) do begin
        inc(i);
        inc(Result);
      end;
      exit;
    end else
      inc(i);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Applies a text format to a text. The text placeholder is @. Supports
  appending and prepending text.
-------------------------------------------------------------------------------}
function ApplyTextFormat(AText: String; AParams: TsNumFormatParams): String;
var
  sct: TsNumFormatSection;
  element: TsNumFormatElement;
  i: Integer;
begin
  Result := '';
  for sct in AParams.Sections do
    for i := 0 to High(sct.Elements)  do begin
      element := sct.Elements[i];
      case element.Token of
        nftTextFormat:
          Result := Result + AText;
        nftText:
          Result := Result + element.TextValue;
      end;
    end;
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the specified text corresponds to a boolean value. For this,
  it must match the specified @TRUE and @FALSE text phrases.
-------------------------------------------------------------------------------}
function IsBoolValue(const AText, ATrueText, AFalseText: String;
  out AValue: Boolean): Boolean;
begin
  if SameText(AText, ATrueText) then
  begin
    AValue := true;
    Result := true;
  end else
  if SameText(AText, AFalseText) then
  begin
    AValue := false;
    Result := true;
  end else
    Result := false;
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the given number format code is for currency,
  i.e. requires a currency symbol.

  @param    AFormat   Built-in number format identifier to be checked
  @returns  @True if AFormat is nfCurrency or nfCurrencyRed, @false otherwise.
-------------------------------------------------------------------------------}
function IsCurrencyFormat(AFormat: TsNumberFormat): Boolean;
begin
  Result := AFormat in [nfCurrency, nfCurrencyRed];
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the specified number format parameters apply to currency values.

  @param    ANumFormat   Number format parameters
  @returns  @True if Kind of the 1st format parameter section contains the nfkCurrency elements; @false otherwise
-------------------------------------------------------------------------------}
function IsCurrencyFormat(ANumFormat: TsNumFormatParams): Boolean;
begin
  Result := (ANumFormat <> nil) and
            (ANumFormat.Sections[0].Kind * [nfkCurrency] <> []);
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the given number format code is for date/time values.

  @param    AFormat  Built-in number format identifier to be checked
  @returns  @True if AFormat is a date/time format (such as nfShortTime), @false otherwise
-------------------------------------------------------------------------------}
function IsDateTimeFormat(AFormat: TsNumberFormat): Boolean;
begin
  Result := AFormat in [nfShortDateTime, nfShortDate, nfLongDate,
    nfShortTime, nfLongTime, nfShortTimeAM, nfLongTimeAM,
    nfDayMonth, nfMonthYear, nfTimeInterval];
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the given string with formatting codes is for date/time values.

  @param    AFormatStr   String with formatting codes to be checked.
  @returns  @True if AFormatStr is a date/time format string (such as 'hh:nn'), @false otherwise
-------------------------------------------------------------------------------}
function IsDateTimeFormat(AFormatStr: string): Boolean;
var
  parser: TsNumFormatParser;
begin
  parser := TsNumFormatParser.Create(AFormatStr, DefaultFormatSettings);
  try
    Result := parser.IsDateTimeFormat;
  finally
    parser.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the specified number format parameters apply to date/time values.

  @param    ANumFormat   Number format parameters
  @returns  @True if Kind of the 1st format parameter section contains the nfkDate or nfkTime elements; @false otherwise
-------------------------------------------------------------------------------}
function IsDateTimeFormat(ANumFormat: TsNumFormatParams): Boolean;
begin
  Result := (ANumFormat <> nil) and
            (ANumFormat.Sections[0].Kind * [nfkDate, nfkTime] <> []);
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the specified text corresponds to a date/time value and returns
  @true, its numerical value and its built-in numberformat if it is.
-------------------------------------------------------------------------------}
function IsDateTimeValue(AText: String; const AFormatSettings: TFormatSettings;
  out ADateTime: TDateTime; out ANumFormat: TsNumberFormat): Boolean;

  { Test whether the text is formatted according to a built-in date/time format.
    Converts the obtained date/time value back to a string and compares. }
  function TestFormat(lNumFmt: TsNumberFormat): Boolean;
  var
    fmt: string;
  begin
    fmt := BuildDateTimeFormatString(lNumFmt, AFormatSettings);
    Result := FormatDateTime(fmt, ADateTime, AFormatSettings) = AText;
    if Result then ANumFormat := lNumFmt;
  end;

begin
  Result := TryStrToDateTime(AText, ADateTime, AFormatSettings);
  if Result then
  begin
    ANumFormat := nfCustom;
    if abs(ADateTime) > 1 then      // this is most probably a date
    begin
      if TestFormat(nfShortDateTime) then
        exit;
      if TestFormat(nfLongDate) then
        exit;
      if TestFormat(nfShortDate) then
        exit;
      if TestFormat(nfMonthYear) then
        exit;
      if TestFormat(nfDayMonth) then
        exit;
    end else
    begin                           // this case is time-only
      if TestFormat(nfLongTimeAM) then
        exit;
      if TestFormat(nfLongTime) then
        exit;
      if TestFormat(nfShortTimeAM) then
        exit;
      if TestFormat(nfShortTime) then
        exit;
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the specified number format parameters apply to a date value.

  @param    ANumFormat   Number format parameters
  @returns  @True if Kind of the 1st format parameter section contains the nfkDate, but no nfkTime tags; @false otherwise
-------------------------------------------------------------------------------}
function IsDateFormat(ANumFormat: TsNumFormatParams): Boolean;
begin
  Result := (ANumFormat <> nil) and
            (ANumFormat.Sections[0].Kind * [nfkDate, nfkTime] = [nfkDate]);
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the given built-in number format code is for time values.

  @param    AFormat  Built-in number format identifier to be checked
  @returns  @True if AFormat represents to a time-format, @false otherwise
-------------------------------------------------------------------------------}
function IsTimeFormat(AFormat: TsNumberFormat): boolean;
begin
  Result := AFormat in [nfShortTime, nfLongTime, nfShortTimeAM, nfLongTimeAM,
    nfTimeInterval];
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the given string with formatting codes is for time values.

  @param   AFormatStr   String with formatting codes to be checked
  @return  True if AFormatStr represents a time-format, false otherwise
-------------------------------------------------------------------------------}
function IsTimeFormat(AFormatStr: String): Boolean;
var
  parser: TsNumFormatParser;
begin
  parser := TsNumFormatParser.Create(AFormatStr, DefaultFormatSettings);
  try
    Result := parser.IsTimeFormat;
  finally
    parser.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the specified number format parameters apply to time values.

  @param    ANumFormat   Number format parameters
  @returns  @True if Kind of the 1st format parameter section contains the nfkTime, but no nfkDate elements; @false otherwise
-------------------------------------------------------------------------------}
function IsTimeFormat(ANumFormat: TsNumFormatParams): Boolean;
begin
  Result := (ANumFormat <> nil) and
            (ANumFormat.Sections[0].Kind * [nfkTime, nfkDate] = [nfkTime]);
end;

{@@ ----------------------------------------------------------------------------
  Returns @TRUE if the specified format string represents a long time format, 
  i.e. it contains two TimeSeparators.
-------------------------------------------------------------------------------}
function IsLongTimeFormat(AFormatStr: String; ATimeSeparator: Char): Boolean;
var
  i, n: Integer;
begin
  n := 0;
  for i:=1 to Length(AFormatStr) do
    if AFormatStr[i] = ATimeSeparator then inc(n);
  Result := (n=2);
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the specified text corresponds to a numerical value. If it is
  then the function result is @TRUE, and the number value and its formatting
  parameters are returned.
-------------------------------------------------------------------------------}
function IsNumberValue(AText: String; AutoDetectNumberFormat: Boolean;
  const AFormatSettings: TFormatSettings;
  out ANumber: Double; out ANumFormat: TsNumberFormat; out ADecimals: Integer;
  out ACurrencySymbol, AWarning: String): Boolean;
var
  p: Integer;
  DecSep, ThousSep: Char;
begin
  Result := false;
  AWarning := '';

  // To detect whether the text is a currency value we look for the currency
  // string. If we find it, we delete it and convert the remaining string to
  // a number.
  ACurrencySymbol := AFormatSettings.CurrencyString;
  if RemoveCurrencySymbol(ACurrencySymbol, AText) then
  begin
    if IsNegative(AText) then
    begin
      if AText = '' then
        exit;
      AText := '-' + AText;
    end;
  end else
    ACurrencySymbol := '';

  if AutoDetectNumberFormat then
    Result := TryStrToFloatAuto(AText, ANumber, DecSep, ThousSep, AWarning)
  else begin
    Result := TryStrToFloat(AText, ANumber, AFormatSettings);
    if Result then
    begin
      if pos(AFormatSettings.DecimalSeparator, AText) = 0
        then DecSep := #0
        else DecSep := AFormatSettings.DecimalSeparator;
      if pos(AFormatSettings.ThousandSeparator, AText) = 0
        then ThousSep := #0
        else ThousSep := AFormatSettings.ThousandSeparator;
    end;
  end;

  // Try to determine the number format
  if Result then
  begin
    if ThousSep <> #0 then
      ANumFormat := nfFixedTh
    else
      ANumFormat := nfGeneral;
    // count number of decimal places and try to catch special formats
    ADecimals := 0;
    if DecSep <> #0 then
    begin
      // Go to the decimal separator and search towards the end of the string
      p := pos(DecSep, AText) + 1;
      while (p <= Length(AText)) do begin
        // exponential format
        if AText[p] in ['+', '-', 'E', 'e'] then
        begin
          ANumFormat := nfExp;
          break;
        end else
        // percent format
        if AText[p] = '%' then
        begin
          ANumFormat := nfPercentage;
          break;
        end else
        begin
          inc(p);
          inc(ADecimals);
        end;
      end;
      if (ADecimals > 0) and (ADecimals < 9) and (ANumFormat = nfGeneral) then
        // "no formatting" assumed if there are "many" decimals
        ANumFormat := nfFixed;
    end else
    begin
      p := Length(AText);
      while (p > 0) do begin
        case AText[p] of
          '%'     : ANumFormat := nfPercentage;
          'e', 'E': ANumFormat := nfExp;
          else      dec(p);
        end;
        break;
      end;
    end;
  end else
    ACurrencySymbol := '';
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the specified number format parameters is a time interval
  format.

  @param    ANumFormat   Number format parameters
  @returns  @True if Kind of the 1st format parameter section contains the nfkTimeInterval elements; @false otherwise
-------------------------------------------------------------------------------}
function IsTimeIntervalFormat(ANumFormat: TsNumFormatParams): Boolean;
begin
  Result := (ANumFormat <> nil) and
            (ANumFormat.Sections[0].Kind * [nfkTimeInterval] <> []);
end;

function IsTextFormat(ANumFormat: TsNumFormatParams): Boolean;
begin
  Result := (ANumFormat <> nil) and
            (ANumFormat.Sections[0].Kind = [nfkText]);
end;

{@@ ----------------------------------------------------------------------------
  Creates a long date format string out of a short date format string.
  Retains the order of year-month-day and the separators, but uses 4 digits
  for year and 3 digits of month.

  @param   ADateFormat   String with date formatting code representing a "short" date, such as 'dd/mm/yy'
  @returns Format string modified to represent a "long" date, such as 'dd/mmm/yyyy'
-------------------------------------------------------------------------------}
function MakeLongDateFormat(ADateFormat: String): String;
var
  i: Integer;
begin
  Result := '';
  i := 1;
  while i < Length(ADateFormat) do begin
    case ADateFormat[i] of
      'y', 'Y':
        begin
          Result := Result + DupeString(ADateFormat[i], 4);
          while (i < Length(ADateFormat)) and (ADateFormat[i] in ['y','Y']) do
            inc(i);
        end;
      'm', 'M':
        begin
          result := Result + DupeString(ADateFormat[i], 3);
          while (i < Length(ADateFormat)) and (ADateFormat[i] in ['m','M']) do
            inc(i);
        end;
      else
        Result := Result + ADateFormat[i];
        inc(i);
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Modifies the short date format such that it has a two-digit year and a two-digit
  month. Retains the order of year-month-day and the separators.

  @param    ADateFormat   String with date formatting codes representing a "long" date, such as 'dd/mmm/yyyy'
  @returns  Format string modified to represent a "short" date, such as 'dd/mm/yy'
-------------------------------------------------------------------------------}
function MakeShortDateFormat(ADateFormat: String): String;
var
  i: Integer;
begin
  Result := '';
  i := 1;
  while i < Length(ADateFormat) do begin
    case ADateFormat[i] of
      'y', 'Y':
        begin
          Result := Result + DupeString(ADateFormat[i], 2);
          while (i < Length(ADateFormat)) and (ADateFormat[i] in ['y','Y']) do
            inc(i);
        end;
      'm', 'M':
        begin
          result := Result + DupeString(ADateFormat[i], 2);
          while (i < Length(ADateFormat)) and (ADateFormat[i] in ['m','M']) do
            inc(i);
        end;
      else
        Result := Result + ADateFormat[i];
        inc(i);
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Creates a "time interval" format string having the first time code identifier
  in square brackets.

  @param  Src   Source format string, must be a time format string, like 'hh:nn'
  @param  Dest  Destination format string, will have the first time code element of the src format string in square brackets, like '[hh]:nn'.
-------------------------------------------------------------------------------}
procedure MakeTimeIntervalMask(Src: String; var Dest: String);
var
  L: TStrings;
begin
  L := TStringList.Create;
  try
    L.StrictDelimiter := true;
    L.Delimiter := ':';
    L.DelimitedText := Src;
    if L[0][1] <> '[' then L[0] := '[' + L[0];
    if L[0][Length(L[0])] <> ']' then L[0] := L[0] + ']';
    Dest := L.DelimitedText;
  finally
    L.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Removes an AM/PM formatting code from a given time formatting string. Variants
  of "AM/PM" are considered as well. The string is left unchanged if it does not
  contain AM/PM codes.

  @param    ATimeFormatString  String of time formatting codes (such as 'hh:nn AM/PM')
  @returns  Formatting string with AM/PM being removed (--> 'hh:nn')
-------------------------------------------------------------------------------}
function StripAMPM(const ATimeFormatString: String): String;
var
  i: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(ATimeFormatString) do begin
    if ATimeFormatString[i] in ['a', 'A'] then begin
      inc(i);
      while (i <= Length(ATimeFormatString)) and (ATimeFormatString[i] in ['p', 'P', 'm', 'M', '/'])  do
        inc(i);
    end else
      Result := Result + ATimeFormatString[i];
    inc(i);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Initializes the FormatSettings of file a import/export parameters record to
  default values which can be replaced by the FormatSettings of the
  workbook's FormatSettings
-------------------------------------------------------------------------------}
procedure InitFormatSettings(out AFormatSettings: TFormatSettings);
var
  i: Integer;
begin
  with AFormatSettings do
  begin
    CurrencyFormat := Byte(-1);
    NegCurrFormat := Byte(-1);
    ThousandSeparator := #0;
    DecimalSeparator := #0;
    CurrencyDecimals := Byte(-1);
    DateSeparator := #0;
    TimeSeparator := #0;
    ListSeparator := #0;
    CurrencyString := '';
    ShortDateFormat := '';
    LongDateFormat := '';
    TimeAMString := '';
    TimePMString := '';
    ShortTimeFormat := '';
    LongTimeFormat := '';
    for i:=1 to 12 do
    begin
      ShortMonthNames[i] := '';
      LongMonthNames[i] := '';
    end;
    for i:=1 to 7 do
    begin
      ShortDayNames[i] := '';
      LongDayNames[i] := '';
    end;
    TwoDigitYearCenturyWindow := Word(-1);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Replaces in AFormatSettings all members marked as having default values (#0,
  -1, '') by the corresponding values of the ADefaultFormats record
-------------------------------------------------------------------------------}
procedure ReplaceFormatSettings(var AFormatSettings: TFormatSettings;
  const ADefaultFormats: TFormatSettings);
var
  i: Integer;
begin
  if AFormatSettings.CurrencyFormat = Byte(-1) then
    AFormatSettings.CurrencyFormat := ADefaultFormats.CurrencyFormat;
  if AFormatSettings.NegCurrFormat = Byte(-1) then
    AFormatSettings.NegCurrFormat := ADefaultFormats.NegCurrFormat;
  if AFormatSettings.ThousandSeparator = #0 then
    AFormatSettings.ThousandSeparator := ADefaultFormats.ThousandSeparator;
  if AFormatSettings.DecimalSeparator = #0 then
    AFormatSettings.DecimalSeparator := ADefaultFormats.DecimalSeparator;
  if AFormatSettings.CurrencyDecimals = Byte(-1) then
    AFormatSettings.CurrencyDecimals := ADefaultFormats.CurrencyDecimals;
  if AFormatSettings.DateSeparator = #0 then
    AFormatSettings.DateSeparator := ADefaultFormats.DateSeparator;
  if AFormatSettings.TimeSeparator = #0 then
    AFormatSettings.TimeSeparator := ADefaultFormats.TimeSeparator;
  if AFormatSettings.ListSeparator = #0 then
    AFormatSettings.ListSeparator := ADefaultFormats.ListSeparator;
  if AFormatSettings.CurrencyString = '' then
    AFormatSettings.CurrencyString := ADefaultFormats.CurrencyString;
  if AFormatSettings.ShortDateFormat = '' then
    AFormatSettings.ShortDateFormat := ADefaultFormats.ShortDateFormat;
  if AFormatSettings.LongDateFormat = '' then
    AFormatSettings.LongDateFormat := ADefaultFormats.LongDateFormat;
  if AFormatSettings.ShortTimeFormat = '' then
    AFormatSettings.ShortTimeFormat := ADefaultFormats.ShortTimeFormat;
  if AFormatSettings.LongTimeFormat = '' then
    AFormatSettings.LongTimeFormat := ADefaultFormats.LongTimeFormat;
  for i:=1 to 12 do
  begin
    if AFormatSettings.ShortMonthNames[i] = '' then
      AFormatSettings.ShortMonthNames[i] := ADefaultFormats.ShortMonthNames[i];
    if AFormatSettings.LongMonthNames[i] = '' then
      AFormatSettings.LongMonthNames[i] := ADefaultFormats.LongMonthNames[i];
  end;
  for i:=1 to 7 do
  begin
    if AFormatSettings.ShortDayNames[i] = '' then
      AFormatSettings.ShortDayNames[i] := ADefaultFormats.ShortDayNames[i];
    if AFormatSettings.LongDayNames[i] = '' then
      AFormatSettings.LongDayNames[i] := ADefaultFormats.LongDayNames[i];
  end;
  if AFormatSettings.TwoDigitYearCenturyWindow = Word(-1) then
    AFormatSettings.TwoDigitYearCenturyWindow := ADefaultFormats.TwoDigitYearCenturyWindow;
end;

function CreateNumFormatParams(ANumFormatStr: String;
  const AFormatSettings: TFormatSettings): TsNumFormatParams;
begin
  Result := TsNumFormatParams.Create;
  ParamsOfNumFormatStr(ANumFormatStr, AFormatSettings, result);
end;

function ParamsOfNumFormatStr(ANumFormatStr: String;
  const AFormatSettings: TFormatSettings; var AResult: TsNumFormatParams): Integer;
var
  parser: TsNumFormatParser;
begin
  Assert(AResult <> nil);
  if ANumFormatstr = 'General' then ANumFormatStr := '';
  parser := TsNumFormatParser.Create(ANumFormatStr, AFormatSettings);
  try
    Result := parser.Status;
    AResult.Sections := parser.FSections;
  finally
    parser.Free;
  end;
end;


{==============================================================================}
{                             TsNumFormatParams                                }
{==============================================================================}

constructor TsNumFormatParams.Create;
begin
  inherited;
  FAllowLocalizedAMPM := true;
end;

{@@ ----------------------------------------------------------------------------
  Deletes a parsed number format element from the specified format section.

  @param  ASectionIndex  Index of the format section containing the element to be deleted
  @param  AElementIndex  Index of the format element to be deleted
-------------------------------------------------------------------------------}
procedure TsNumFormatParams.DeleteElement(ASectionIndex, AElementIndex: Integer);
var
  i, n: Integer;
begin
  with Sections[ASectionIndex] do
  begin
    n := Length(Elements);
    for i := AElementIndex+1 to n-1 do
      Elements[i-1] := Elements[i];
    SetLength(Elements, n-1);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Creates the built-in number format identifier from the parsed number format
  sections and elements

  @returns  Built-in number format identifer if the format is built into fpspreadsheet, or nfCustom otherwise
  @seeAlso  TsNumberFormat
-------------------------------------------------------------------------------}
function TsNumFormatParams.GetNumFormat: TsNumberFormat;
begin
  Result := nfCustom;
  case Length(Sections) of
    0: Result := nfGeneral;
    1: Result := Sections[0].NumFormat;
    2: if (Sections[0].NumFormat = Sections[1].NumFormat) and
          (Sections[0].NumFormat in [nfCurrency, nfCurrencyRed])
       then
         Result := Sections[0].NumFormat;
    3: if (Sections[0].NumFormat = Sections[1].NumFormat) and
          (Sections[1].NumFormat = Sections[2].NumFormat) and
          (Sections[0].NumFormat in [nfCurrency, nfCurrencyRed])
       then
         Result := Sections[0].NumFormat;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Constructs the number format string from the parsed sections and elements.
  The format symbols are selected according to Excel syntax.

  @returns  Excel-compatible number format string.
-------------------------------------------------------------------------------}
function TsNumFormatParams.GetNumFormatStr: String;
var
  i: Integer;
begin
  if Length(Sections) > 0 then begin
    Result := BuildFormatStringFromSection(Sections[0]);
    for i := 1 to High(Sections) do
      Result := Result + ';' + BuildFormatStringFromSection(Sections[i], FAllowLocalizedAMPM);
  end else
    Result := '';
end;

{@@ ----------------------------------------------------------------------------
  Inserts a parsed format token into the specified format section before the
  specified element.

  @param   ASectionIndex   Index of the parsed format section into which the token is to be inserted
  @param   AElementIndex   Index of the format element before which the token is to be inserted
  @param   AToken          Parsed format token to be inserted

  @seeAlso TsNumFormatToken
-------------------------------------------------------------------------------}
procedure TsNumFormatParams.InsertElement(ASectionIndex, AElementIndex: Integer;
  AToken: TsNumFormatToken);
var
  i, n: Integer;
begin
  with Sections[ASectionIndex] do
  begin
    n := Length(Elements);
    SetLength(Elements, n+1);
    for i:=n-1 downto AElementIndex do
      Elements[i+1] := Elements[i];
    Elements[AElementIndex].Token := AToken;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Checks whether the parsed format sections passed as a parameter are identical
  to the interal section array.

  @param  ASections  Array of parsed format sections to be compared with the internal format sections
-------------------------------------------------------------------------------}
function TsNumFormatParams.SectionsEqualTo(ASections: TsNumFormatSections): Boolean;
var
  i, j: Integer;
begin
  Result := false;
  if Length(ASections) <> Length(Sections) then
    exit;
  for i := 0 to High(Sections) do begin
    if Length(Sections[i].Elements) <> Length(ASections[i].Elements) then
      exit;

    for j:=0 to High(Sections[i].Elements) do
    begin
      if Sections[i].Elements[j].Token <> ASections[i].Elements[j].Token then
        exit;

      if Sections[i].NumFormat <> ASections[i].NumFormat then
        exit;
      if Sections[i].Decimals <> ASections[i].Decimals then
        exit;
      {
      if Sections[i].Factor <> ASections[i].Factor then
        exit;
        }
      if Sections[i].FracInt <> ASections[i].FracInt then
        exit;
      if Sections[i].FracNumerator <> ASections[i].FracNumerator then
        exit;
      if Sections[i].FracDenominator <> ASections[i].FracDenominator then
        exit;
      if Sections[i].CurrencySymbol <> ASections[i].CurrencySymbol then
        exit;
      if Sections[i].Color <> ASections[i].Color then
        exit;

      case Sections[i].Elements[j].Token of
        nftText, nftThSep, nftDecSep, nftDateTimeSep,
        nftAMPM, nftSign, nftSignBracket,
        nftExpChar, nftExpSign, nftPercent, nftFracSymbol, nftCurrSymbol,
        nftCountry, nftSpace, nftEscaped, nftRepeat, nftEmptyCharWidth,
        nftTextFormat:
          if Sections[i].Elements[j].TextValue <> ASections[i].Elements[j].TextValue
            then exit;

        nftYear, nftMonth, nftDay,
        nftHour, nftMinute, nftSecond, nftMilliseconds,
        nftMonthMinute,
        nftIntOptDigit, nftIntZeroDigit, nftIntSpaceDigit, nftIntTh,
        nftZeroDecs, nftOptDecs, nftSpaceDecs, nftExpDigits, nftFactor,
        nftFracNumOptDigit, nftFracNumSpaceDigit, nftFracNumZeroDigit,
        nftFracDenomOptDigit, nftFracDenomSpaceDigit, nftFracDenomZeroDigit,
        nftColor:
          if Sections[i].Elements[j].IntValue <> ASections[i].Elements[j].IntValue
            then exit;

        nftCompareOp, nftCompareValue:
          if Sections[i].Elements[j].FloatValue <> ASections[i].Elements[j].FloatValue
            then exit;
      end;
    end;
  end;
  Result := true;
end;

{@@ ----------------------------------------------------------------------------
  Defines the currency symbol used in the format params sequence

  @param  AValue  String containing the currency symbol to be used in the converted numbers
-------------------------------------------------------------------------------}
procedure TsNumFormatParams.SetCurrSymbol(AValue: String);
var
  section: TsNumFormatSection;
  s, el: Integer;
begin
  for s:=0 to High(Sections) do
  begin
    section := Sections[s];
    if (nfkCurrency in section.Kind) then
    begin
      section.CurrencySymbol := AValue;
      for el := 0 to High(section.Elements) do
        if section.Elements[el].Token = nftCurrSymbol then
          section.Elements[el].Textvalue := AValue;
    end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Adds or modifies parsed format tokens such that the specified number of
  decimal places is displayed

  @param  AValue  Number of decimal places to be shown
-------------------------------------------------------------------------------}
procedure TsNumFormatParams.SetDecimals(AValue: byte);
var
  section: TsNumFormatSection;
  s, el: Integer;
begin
  for s := 0 to High(Sections) do
  begin
    section := Sections[s];
    if section.Kind * [nfkFraction, nfkDate, nfkTime] <> [] then
      Continue;
    section.Decimals := AValue;
    for el := High(section.Elements) downto 0 do
      case section.Elements[el].Token of
        nftZeroDecs:
          section.Elements[el].Intvalue := AValue;
        nftOptDecs, nftSpaceDecs:
          DeleteElement(s, el);
      end;
  end;
end;

{@@ ----------------------------------------------------------------------------
  If AEnable is true a format section for negative numbers is added (or an
  existing one is modified) such that negative numbers are displayed in red.
  If AEnable is false the format tokens are modified such that negative values
  are displayed in default color.

  @param  AEnable  The format tokens are modified such as to display negative values in red if AEnable is true.
-------------------------------------------------------------------------------}
procedure TsNumFormatParams.SetNegativeRed(AEnable: Boolean);
var
  el: Integer;
begin
  // Enable negative-value color
  if AEnable then
  begin
    if Length(Sections) = 1 then begin
      SetLength(Sections, 2);
      Sections[1] := Sections[0];
      InsertElement(1, 0, nftColor);
      Sections[1].Elements[0].Intvalue := scRed;
      InsertElement(1, 1, nftSign);
      Sections[1].Elements[1].TextValue := '-';
    end else
    begin
      if not (nfkHasColor in Sections[1].Kind) then
        InsertElement(1, 0, nftColor);
      for el := 0 to High(Sections[1].Elements) do
        if Sections[1].Elements[el].Token = nftColor then
          Sections[1].Elements[el].IntValue := scRed;
    end;
    Sections[1].Kind := Sections[1].Kind + [nfkHasColor];
    Sections[1].Color := scRed;
  end else
  // Disable negative-value color
  if Length(Sections) >= 2 then
  begin
    Sections[1].Kind := Sections[1].Kind - [nfkHasColor];
    Sections[1].Color := scBlack;
    for el := High(Sections[1].Elements) downto 0 do
      if Sections[1].Elements[el].Token = nftColor then
        DeleteElement(1, el);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Inserts a thousand separator token into the format elements at the
  appropriate position, or removes it

  @param  AEnable   A thousand separator is inserted if AEnable is @true, or else deleted.
-------------------------------------------------------------------------------}
procedure TsNumFormatParams.SetThousandSep(AEnable: Boolean);
var
  section: TsNumFormatSection;
  s, el: Integer;
  replaced: Boolean;
begin
  for s := 0 to High(Sections) do
  begin
    section := Sections[s];
    replaced := false;
    for el := High(section.Elements) downto 0 do
    begin
      if AEnable then
      begin
        if section.Elements[el].Token in [nftIntOptDigit, nftIntSpaceDigit, nftIntZeroDigit] then
        begin
          if replaced then
            DeleteElement(s, el)
          else begin
            section.Elements[el].Token := nftIntTh;
            Include(section.Kind, nfkHasThSep);
            replaced := true;
          end;
        end;
      end else
      begin
        if section.Elements[el].Token = nftIntTh then begin
          section.Elements[el].Token := nftIntZeroDigit;
          Exclude(section.Kind, nfkHasThSep);
          break;
        end;
      end;
    end;
  end;
end;


{==============================================================================}
{                           TsNumFormatList                                    }
{==============================================================================}

{@@ ----------------------------------------------------------------------------
  Constructor of the number format list class.

  @param  AFormatSettings   Format settings needed internally by the number format parser (currency symbol, etc.)
  @param  AOwnsData         If @true then the list is responsible to destroy the list items
-------------------------------------------------------------------------------}
constructor TsNumFormatList.Create(AFormatSettings: TFormatSettings;
  AOwnsData: Boolean);
begin
  inherited Create;
  FClass := TsNumFormatParams;
  FFormatSettings := AFormatSettings;
  FOwnsData := AOwnsData;
end;

{@@ ----------------------------------------------------------------------------
  Destructor of the number format list class.

  Clears the list items if the list "owns" the data.
-------------------------------------------------------------------------------}
destructor TsNumFormatList.Destroy;
begin
  Clear;
  inherited;
end;

{@@ ----------------------------------------------------------------------------
  Adds the specified sections of a parsed number format to the list.
  Duplicates are not checked before adding the format item.

  @param   ASections   Array of number format sections as obtained by the number format parser for a given format string
  @returns Index of the format item in the list.
-------------------------------------------------------------------------------}
function TsNumFormatList.AddFormat(ASections: TsNumFormatSections): Integer;
var
  nfp: TsNumFormatParams;
begin
  Result := Find(ASections);
  if Result = -1 then begin
    nfp := FClass.Create;
    nfp.Sections := ASections;
    Result := inherited Add(nfp);
  end;
end;

{@@ ----------------------------------------------------------------------------
  Adds a number format as specified by a format string to the list
  Uses the number format parser to convert the format string to format sections
  and elements.

  Duplicates are not checked before adding the format item.

  @param    AFormatStr  Excel-like format string describing the format to be added
  @returns  Index of the format item in the list
-------------------------------------------------------------------------------}
function TsNumFormatList.AddFormat(AFormatStr: String): Integer;
var
  parser: TsNumFormatParser;
  newSections: TsNumFormatSections = nil;
  i: Integer;
begin
  parser := TsNumFormatParser.Create(AFormatStr, FFormatSettings);
  try
    SetLength(newSections, parser.ParsedSectionCount);
    for i:=0 to High(newSections) do
      newSections[i] := parser.ParsedSections[i];
    Result := AddFormat(newSections);
  finally
    parser.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  Adds the number formats to the list which are built into the file format.

  Does nothing here. Must be overridden by derived classes for each file format.
-------------------------------------------------------------------------------}
procedure TsNumFormatList.AddBuiltinFormats;
begin
end;

{@@ ----------------------------------------------------------------------------
  Clears the list.
  If the list "owns" the format items they are destroyed.

  @seeAlso  TsNumFormatList.Create
-------------------------------------------------------------------------------}
procedure TsNumFormatList.Clear;
var
  i: Integer;
begin
  for i := Count-1 downto 0 do Delete(i);
  inherited;
end;

{@@ ----------------------------------------------------------------------------
  Deletes the number format item having the specified index in the list.
  If the list "owns" the format items, the item is destroyed.

  @param  AIndex  Index of the format item to be deleted
  @seeAlso TsNumformatList.Create
-------------------------------------------------------------------------------}
procedure TsNumFormatList.Delete(AIndex: Integer);
var
  p: TsNumFormatParams;
begin
  if FOwnsData then
  begin
    p := GetItem(AIndex);
    if p <> nil then p.Free;
  end;
  inherited Delete(AIndex);
end;

{@@ ----------------------------------------------------------------------------
  Checks whether a parsed format item having the specified format sections is
  contained in the list and returns its index if found, or -1 if not found.

  @param   ASections   Array of number format sections as obtained by the number format parser for a given format string
  @returns Index of the found format item, or -1 if not found
-------------------------------------------------------------------------------}
function TsNumFormatList.Find(ASections: TsNumFormatSections): Integer;
var
  nfp: TsNumFormatParams;
begin
  for Result := 0 to Count-1 do begin
    nfp := GetItem(Result);
    if nfp.SectionsEqualTo(ASections) then
      exit;
  end;
  Result := -1;
end;

{@@ ----------------------------------------------------------------------------
  Checks whether a format item corresponding to the specified format string is
  contained in the list and returns its index if found, or -1 if not.

  Should be called before adding a format to the list to avoid duplicates.

  @param    AFormatStr  Number format string of the format item which is seeked
  @returns  Index of the found format item, or -1 if not found
  @seeAlso  TsNumFormatList.AddFormat
-------------------------------------------------------------------------------}
function TsNumFormatList.Find(AFormatStr: String): Integer;
var
  nfp: TsNumFormatParams;
begin
  nfp := CreateNumFormatParams(AFormatStr, FFormatSettings);
  if nfp = nil then
    Result := -1
  else
    Result := Find(nfp.Sections);
end;

{@@ ----------------------------------------------------------------------------
  Getter function returning the correct type of the list items
  (i.e., @link(TsNumFormatParams) which are parsed format descriptions).

  @param   AIndex   Index of the format item
  @returns Pointer to the list item at the specified index, cast to the type @link(TsNumFormatParams)
-------------------------------------------------------------------------------}
function TsNumFormatList.GetItem(AIndex: Integer): TsNumFormatParams;
begin
  Result := TsNumFormatParams(inherited Items[AIndex]);
end;

{@@ ----------------------------------------------------------------------------
  Setter function for the list items

  @param  AIndex  Index of the format item
  @param  AValue  Pointer to the parsed format description to be stored in the list at the specified index.
-------------------------------------------------------------------------------}
procedure TsNumFormatList.SetItem(AIndex: Integer;
  const AValue: TsNumFormatParams);
begin
  inherited Items[AIndex] := AValue;
end;


{==============================================================================}
{                              TsNumFormatParser                               }
{==============================================================================}

{@@ ----------------------------------------------------------------------------
  Creates a number format parser for analyzing a formatstring that has been
  read from a spreadsheet file.

  If ALocalized is true then the formatstring contains localized decimal
  separator etc.
-------------------------------------------------------------------------------}
constructor TsNumFormatParser.Create(const AFormatString: String;
  const AFormatSettings: TFormatSettings);
begin
  inherited Create;
  FFormatSettings := AFormatSettings;
  Parse(AFormatString);
  CheckSections;
  if AFormatString = '' then FSections[0].NumFormat := nfGeneral;
end;

destructor TsNumFormatParser.Destroy;
begin
  FSections := nil;
  inherited Destroy;
end;

procedure TsNumFormatParser.AddElement(AToken: TsNumFormatToken; AText: String);
var
  n: Integer;
begin
  n := Length(FSections[FCurrSection].Elements);
  SetLength(FSections[FCurrSection].Elements, n+1);
  FSections[FCurrSection].Elements[n].Token := AToken;
  FSections[FCurrSection].Elements[n].TextValue := AText;
end;

procedure TsNumFormatParser.AddElement(AToken: TsNumFormatToken;
  AIntValue: Integer=0; AText: String = '');
var
  n: Integer;
begin
  n := Length(FSections[FCurrSection].Elements);
  SetLength(FSections[FCurrSection].Elements, n+1);
  FSections[FCurrSection].Elements[n].Token := AToken;
  FSections[FCurrSection].Elements[n].IntValue := AIntValue;
  FSections[FCurrSection].Elements[n].TextValue := AText;
end;

procedure TsNumFormatParser.AddElement(AToken: TsNumFormatToken; AFloatValue: Double); overload;
var
  n: Integer;
begin
  n := Length(FSections[FCurrSection].Elements);
  SetLength(FSections[FCurrSection].Elements, n+1);
  FSections[FCurrSection].Elements[n].Token := AToken;
  FSections[FCurrSection].Elements[n].FloatValue := AFloatValue;
end;

procedure TsNumFormatParser.AddSection;
begin
  FCurrSection := Length(FSections);
  SetLength(FSections, FCurrSection + 1);
  with FSections[FCurrSection] do
    SetLength(Elements, 0);
end;

procedure TsNumFormatParser.AnalyzeColor(AValue: String);
var
  n: Integer;
begin
  AValue := lowercase(AValue);
  // Colors
  if AValue = 'red' then
    AddElement(nftColor, ord(scRed))
  else
  if AValue = 'black' then
    AddElement(nftColor, ord(scBlack))
  else
  if AValue = 'blue' then
    AddElement(nftColor, ord(scBlue))
  else
  if AValue = 'white' then
    AddElement(nftColor, ord(scWhite))
  else
  if AValue = 'green' then
    AddElement(nftColor, ord(scGreen))
  else
  if AValue = 'cyan' then
    AddElement(nftColor, ord(scCyan))
  else
  if AValue = 'magenta' then
    AddElement(nftColor, ord(scMagenta))
  else
  if copy(AValue, 1, 5) = 'color' then begin
    AValue := copy(AValue, 6, Length(AValue));
    if not TryStrToInt(trim(AValue), n) then begin
      FStatus := psErrNoValidColorIndex;
      exit;
    end;
    AddElement(nftColor, n);
  end else
    FStatus := psErrUnknownInfoInBrackets;
end;

function TsNumFormatParser.AnalyzeCurrency(const AValue: String): Boolean;
begin
  if (FFormatSettings.CurrencyString = '') then
    Result := false
  else
    Result := CurrencyRegistered(AValue);
end;

{@@ Creates a formatstring for all sections.

  @Note This implementation is only valid for the fpc and Excel dialects of format string. }
function TsNumFormatParser.BuildFormatString: String;
var
  i: Integer;
begin
  if Length(FSections) > 0 then begin
    Result := BuildFormatStringFromSection(FSections[0]);
    for i:=1 to High(FSections) do
      Result := Result + ';' + BuildFormatStringFromSection(FSections[i]);
  end;
end;

procedure TsNumFormatParser.CheckSections;
var
  i: Integer;
begin
  for i:=0 to High(FSections) do
    CheckSection(i);

  if (Length(FSections) > 1) and (FSections[1].NumFormat = nfCurrencyRed) then
    for i:=0 to High(FSections) do
      if FSections[i].NumFormat = nfCurrency then
        FSections[i].NumFormat := nfCurrencyRed;
end;

procedure TsNumFormatParser.CheckSection(ASection: Integer);
var
  el, i: Integer;
  section: PsNumFormatSection;
  nfs, nfsTest: String;
  nf: TsNumberFormat;
  formats: set of TsNumberFormat;
  isMonthMinute: Boolean;
begin
  if FStatus <> psOK then
    exit;

  section := @FSections[ASection];
  section^.Kind := [];

  if (ASection = 0) and (Length(FSections) = 1) and (Length(section^.Elements) = 1)
    and (section^.Elements[0].Token = nftGeneral)
  then begin
    section^.NumFormat := nfGeneral;
    exit;
  end;

  i := 0;
  isMonthMinute := false;

  for el := 0 to High(section^.Elements) do
  begin
    case section^.Elements[el].Token of
      nftZeroDecs:
        section^.Decimals := section^.Elements[el].IntValue;
      nftIntZeroDigit:
        begin
          section^.MinIntDigits := section^.Elements[el].IntValue;
          i := section^.Elements[el].IntValue;
        end;
      nftIntOptDigit, nftIntSpaceDigit:
        i := section^.Elements[el].IntValue;
      nftFracNumSpaceDigit, nftFracNumZeroDigit:
        section^.FracNumerator := section^.Elements[el].IntValue;
      nftFracDenomSpaceDigit, nftFracDenomZeroDigit:
        section^.FracDenominator := section^.Elements[el].IntValue;
      nftFracDenom:
        section^.FracDenominator := -section^.Elements[el].IntValue;
      nftPercent:
        section^.Kind := section^.Kind + [nfkPercent];
      nftExpChar:
        if (nfkExp in section^.Kind) then
          FStatus := psErrMultipleExpChars
        else
          section^.Kind := section^.Kind + [nfkExp];
      nftFactor:
        if section^.Elements[el].IntValue <> 0 then
        begin
          section^.Elements[el].FloatValue := IntPower(10, -3*section^.Elements[el].IntValue);
          section^.Factor := section^.Elements[el].FloatValue;
          section^.Kind := section^.Kind + [nfkHasFactor];
        end;
      nftFracSymbol:
        if (nfkFraction in section^.Kind) then
          FStatus := psErrMultipleFracSymbols
        else
        begin
          section^.Kind := section^.Kind + [nfkFraction];
          section^.FracInt := i;
        end;
      nftCurrSymbol:
        begin
          if (nfkCurrency in section^.Kind) then
            FStatus := psErrMultipleCurrSymbols
          else begin
            section^.Kind := section^.Kind + [nfkCurrency];
            section^.CurrencySymbol := section^.Elements[el].TextValue;
          end;
        end;
      nftYear, nftMonth, nftDay:
        section^.Kind := section^.Kind + [nfkDate];
      nftHour, nftMinute, nftSecond, nftMilliseconds:
        begin
          section^.Kind := section^.Kind + [nfkTime];
          if section^.Elements[el].IntValue < 0 then
            section^.Kind := section^.Kind + [nfkTimeInterval];
          if section^.Elements[el].Token = nftMilliseconds then
            section^.Decimals := section^.Elements[el].IntValue
          else
            section^.Decimals := 0;
        end;
      nftMonthMinute:
        isMonthMinute := true;
      nftColor:
        begin
          section^.Kind := section^.Kind + [nfkHasColor];
          section^.Color := section^.Elements[el].IntValue;
        end;
      nftIntTh:
        section^.Kind := section^.Kind + [nfkHasThSep];
      nftTextFormat:
        section^.Kind := section^.Kind + [nfkText];
    end;
  end; // for

  if FStatus <> psOK then
    exit;

  if (section^.Kind * [nfkDate, nfkTime] <> []) and
     (section^.Kind * [nfkPercent, nfkExp, nfkCurrency, nfkFraction] <> []) then
  begin
    FStatus := psErrNoValidDateTimeFormat;
    exit;
  end;

  if (Length(FSections) = 1) and (section^.Kind = [nfkText]) then begin
    section^.NumFormat := nfText;
    exit;
  end;

  section^.NumFormat := nfCustom;

  if (section^.Kind * [nfkDate, nfkTime] <> []) or isMonthMinute then
  begin
    FixMonthMinuteToken(section^);
    nfs := GetFormatString;
    if (nfkTimeInterval in section^.Kind) then
      section^.NumFormat := nfTimeInterval
    else
    begin
      formats := [nfShortDateTime, nfLongDate, nfShortDate, nfLongTime,
        nfShortTime, nfLongTimeAM, nfShortTimeAM, nfDayMonth, nfMonthYear];
      for nf in formats do
      begin
        nfsTest := BuildDateTimeFormatString(nf, FFormatSettings);
        if Length(nfsTest) = Length(nfs) then
        begin
          if SameText(nfs, nfsTest) then
          begin
            section^.NumFormat := nf;
            break;
          end;
          for i := 1 to Length(nfsTest) do
            case nfsTest[i] of
              '/': if not (nf in [nfLongTimeAM, nfShortTimeAM]) then
                     nfsTest[i] := FFormatSettings.DateSeparator;
              ':': nfsTest[i] := FFormatSettings.TimeSeparator;
              'n': nfsTest[i] := 'm';
            end;
          if SameText(nfs, nfsTest) then
          begin
            section^.NumFormat := nf;
            break;
          end;
        end;
      end;
    end;
  end else
  begin
    nfs := GetFormatString;
    nfsTest := BuildFractionFormatString(section^.FracInt > 0, section^.FracNumerator, section^.FracDenominator);
    if sameText(nfs, nfsTest) then
      section^.NumFormat := nfFraction
    else
    begin
      formats := [nfFixed, nfFixedTh, nfPercentage, nfExp];
      for nf in formats do begin
        nfsTest := BuildNumberFormatString(nf, FFormatSettings, section^.Decimals);
        if SameText(nfs, nfsTest) then
        begin
          section^.NumFormat := nf;
          break;
        end;
      end;
    end;
    if (section^.NumFormat = nfCustom) and (nfkCurrency in section^.Kind) then
    begin
      section^.NumFormat := nfCurrency;
      if section^.Color = scRed then
        section^.NumFormat := nfCurrencyRed;
    end;
  end;
end;

procedure TsNumFormatParser.ClearAll;
var
  i, j: Integer;
begin
  for i:=0 to Length(FSections)-1 do begin
    for j:=0 to Length(FSections[i].Elements) do
      if FSections[i].Elements <> nil then
        FSections[i].Elements[j].TextValue := '';
    FSections[i].Elements := nil;
    FSections[i].CurrencySymbol := '';
  end;
  FSections := nil;
end;

procedure TsNumFormatParser.DeleteElement(ASection, AIndex: Integer);
var
  i, n: Integer;
begin
  n := Length(FSections[ASection].Elements);
  for i:= AIndex+1 to n-1 do
    FSections[ASection].Elements[i-1] := FSections[ASection].Elements[i];
  SetLength(FSections[ASection].Elements, n-1);
end;

{@@ Identify the ambiguous "m" token ("month" or "minute") }
procedure TsNumFormatParser.FixMonthMinuteToken(var ASection: TsNumFormatSection);
var
  i, j: Integer;

  // Finds the previous date/time element skipping spaces, date/time sep etc.
  function PrevDateTimeElement(j: Integer): Integer;
  begin
    Result := -1;
    dec(j);
    while (j >= 0) do begin
      with ASection.Elements[j] do
        if Token in [nftYear, nftMonth, nftDay, nftHour, nftMinute, nftSecond] then
        begin
          Result := j;
          exit;
        end;
      dec(j);
    end;
  end;

  // Finds the next date/time element skipping spaces, date/time sep etc.
  function NextDateTimeElement(j: Integer): Integer;
  begin
    Result := -1;
    inc(j);
    while (j < Length(ASection.Elements)) do begin
      with ASection.Elements[j] do
        if Token in [nftYear, nftMonth, nftDay, nftHour, nftMinute, nftSecond] then
        begin
          Result := j;
          exit;
        end;
      inc(j);
    end;
  end;

begin
  for i:=0 to High(ASection.Elements) do
  begin
    // Find index of nftMonthMinute token...
    if ASection.Elements[i].Token = nftMonthMinute then begin
      // ... and, using its neighbors, decide whether it is a month or a minute.
      j := NextDateTimeElement(i);
      if j <> -1 then
        case ASection.Elements[j].Token of
          nftDay, nftYear:
            begin
              ASection.Elements[i].Token := nftMonth;
              Continue;
            end;
          nftSecond:
            begin
              ASection.Elements[i].Token := nftMinute;
              Continue;
            end;
        end;
      j := PrevDateTimeElement(i);
      if j <> -1 then
        case ASection.Elements[j].Token of
          nftDay, nftYear:
            begin
              ASection.Elements[i].Token := nftMonth;
              Continue;
            end;
          nftHour:
            begin
              ASection.Elements[i].Token := nftMinute;
              Continue;
            end;
        end;

      // If we get here the token is isolated. In this case we assume
      // that it is a month - that's the way Excel does it when reading files
      // (for editing of a worksheet, however, Excel distinguishes between
      // uppercase "M" for "month" and lowercase "m" for "minute".)
      ASection.Elements[i].Token := nftMonth;
      Include(ASection.Kind, nfkDate);
    end;
  end;
end;

procedure TsNumFormatParser.InsertElement(ASection, AIndex: Integer;
  AToken: TsNumFormatToken; AText: String);
var
  i, n: Integer;
begin
  n := Length(FSections[ASection].Elements);
  SetLength(FSections[ASection].Elements, n+1);
  for i:= n-1 downto AIndex+1 do
    FSections[ASection].Elements[i+1] := FSections[ASection].Elements[i];
  FSections[ASection].Elements[AIndex+1].Token := AToken;
  FSections[ASection].Elements[AIndex+1].TextValue := AText;
end;

procedure TsNumFormatParser.InsertElement(ASection, AIndex: Integer;
  AToken: TsNumFormatToken; AIntValue: Integer);
var
  i, n: Integer;
begin
  n := Length(FSections[ASection].Elements);
  SetLength(FSections[ASection].Elements, n+1);
  for i:= n-1 downto AIndex+1 do
    FSections[ASection].Elements[i+1] := FSections[ASection].Elements[i];
  FSections[ASection].Elements[AIndex+1].Token := AToken;
  FSections[ASection].Elements[AIndex+1].IntValue := AIntValue;
end;

procedure TsNumFormatParser.InsertElement(ASection, AIndex: Integer;
  AToken: TsNumFormatToken; AFloatValue: Double);
var
  i, n: Integer;
begin
  n := Length(FSections[ASection].Elements);
  SetLength(FSections[ASection].Elements, n+1);
  for i:= n-1 downto AIndex+1 do
    FSections[ASection].Elements[i+1] := FSections[ASection].Elements[i];
  FSections[ASection].Elements[AIndex+1].Token := AToken;
  FSections[ASection].Elements[AIndex+1].FloatValue := AFloatValue;
end;

function TsNumFormatParser.GetFormatString: String;
begin
  Result := BuildFormatString;
end;

{@@ Extracts the currency symbol form the formatting sections. It is assumed that
  all two or three sections of the currency/accounting format use the same
  currency symbol, otherwise it would be custom format anyway which ignores
  the currencysymbol value. }
function TsNumFormatParser.GetCurrencySymbol: String;
begin
  if Length(FSections) > 0 then
    Result := FSections[0].CurrencySymbol
  else
    Result := '';
end;

{@@ Creates a string which summarizes the date/time formats in the given section.
  The string contains a 'y' for a nftYear, a 'm' for a nftMonth, a
  'd' for a nftDay, a 'h' for a nftHour, a 'n' for a nftMinute, a 's' for a
  nftSeconds, and a 'z' for a nftMilliseconds token. The order is retained.
  Needed for biff2 }
function TsNumFormatParser.GetDateTimeCode(ASection: Integer): String;
var
  i: Integer;
begin
  Result := '';
  if ASection < Length(FSections) then
    with FSections[ASection] do begin
      i := 0;
      while i < Length(Elements) do begin
        case Elements[i].Token of
          nftYear        : Result := Result + 'y';
          nftMonth       : Result := Result + 'm';
          nftDay         : Result := Result + 'd';
          nftHour        : Result := Result + 'h';
          nftMinute      : Result := Result + 'n';
          nftSecond      : Result := Result + 's';
          nftMilliSeconds: Result := Result + 'z';
        end;
        inc(i);
      end;
    end;
end;

{@@ Extracts the number of decimals from the sections. Since they are needed only
  for default formats having only a single section, only the first section is
  considered. In case of currency/accounting having two or three sections, it is
  assumed that all sections have the same decimals count, otherwise it would not
  be a standard format. }
function TsNumFormatParser.GetDecimals: Byte;
begin
  if Length(FSections) > 0 then
    Result := FSections[0].Decimals
  else
    Result := 0;
end;

function TsNumFormatParser.GetFracDenominator: Integer;
begin
  if Length(FSections) > 0 then
    Result := FSections[0].FracDenominator
  else
    Result := 0;
end;

function TsNumFormatParser.GetFracInt: Integer;
begin
  if Length(FSections) > 0 then
    Result := FSections[0].FracInt
  else
    Result := 0;
end;

function TsNumFormatParser.GetFracNumerator: Integer;
begin
  if Length(FSections) > 0 then
    Result := FSections[0].FracNumerator
  else
    Result := 0;
end;

{@@ Tries to extract a common built-in number format from the sections. If there
  are multiple sections, it is always a custom format, except for Currency and
  Accounting. }
function TsNumFormatParser.GetNumFormat: TsNumberFormat;
begin
  if Length(FSections) = 0 then
    result := nfGeneral
  else begin
    Result := FSections[0].NumFormat;
    if (Result = nfCurrency) then begin
      if Length(FSections) = 2 then begin
        Result := FSections[1].NumFormat;
        if FSections[1].CurrencySymbol <> FSections[0].CurrencySymbol then begin
          Result := nfCustom;
          exit;
        end;
        if (FSections[0].NumFormat in [nfCurrency, nfCurrencyRed]) and
           (FSections[1].NumFormat in [nfCurrency, nfCurrencyRed])
        then
          exit;
      end else
      if Length(FSections) = 3 then begin
        Result := FSections[1].NumFormat;
        if (FSections[0].CurrencySymbol <> FSections[1].CurrencySymbol) or
           (FSections[1].CurrencySymbol <> FSections[2].CurrencySymbol)
        then begin
          Result := nfCustom;
          exit;
        end;
        if (FSections[0].NumFormat in [nfCurrency, nfCurrencyRed]) and
           (FSections[1].NumFormat in [nfCurrency, nfCurrencyRed]) and
           (FSections[2].NumFormat in [nfCurrency, nfCurrencyRed])
        then
          exit;
      end;
      Result := nfCustom;
      exit;
    end;
    if Length(FSections) > 1 then
      Result := nfCustom;
  end;
end;

function TsNumFormatParser.GetParsedSectionCount: Integer;
begin
  Result := Length(FSections);
end;

function TsNumFormatParser.GetParsedSections(AIndex: Integer): TsNumFormatSection;
begin
  Result := FSections[AIndex];
end;
  {
function TsNumFormatParser.GetTokenIntValueAt(AToken: TsNumFormatToken;
  ASection, AIndex: Integer): Integer;
begin
  if IsTokenAt(AToken, ASection, AIndex) then
    Result := FSections[ASection].Elements[AIndex].IntValue
  else
    Result := -1;
end;
   }
{ Returns true if the format elements contain at least one date/time token }
function TsNumFormatParser.IsDateTimeFormat: Boolean;
var
  section: TsNumFormatSection;
begin
  for section in FSections do
    if section.Kind * [nfkDate, nfkTime] <> [] then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;
  {
function TsNumFormatParser.IsNumberAt(ASection, AIndex: Integer;
  out ANumFormat: TsNumberFormat; out ADecimals: Byte;
  out ANextIndex: Integer): Boolean;
var
  token: TsNumFormatToken;
begin
  if (ASection > High(FSections)) or (AIndex > High(FSections[ASection].Elements))
  then begin
    Result := false;
    ANextIndex := AIndex;
    exit;
  end;

  Result := true;
  ANumFormat := nfCustom;
  ADecimals := 0;
  token := FSections[ASection].Elements[AIndex].Token;

  if token in [nftFracNumOptDigit, nftFracNumZeroDigit, nftFracNumSpaceDigit,
    nftFracDenomOptDigit, nftFracDenomZeroDigit, nftFracDenomSpaceDigit] then
  begin
    ANumFormat := nfFraction;
    ANextIndex := AIndex + 1;
    exit;
  end;

  if (token = nftIntTh) and (FSections[ASection].Elements[AIndex].IntValue = 1) then   // '#,##0'
    ANumFormat := nfFixedTh
  else
  if (token = nftIntZeroDigit) and (FSections[ASection].Elements[AIndex].IntValue = 1) then // '0'
    ANumFormat := nfFixed;

  if (token in [nftIntTh, nftIntZeroDigit, nftIntOptDigit, nftIntSpaceDigit]) then
  begin
    if IsTokenAt(nftDecSep, ASection, AIndex+1) then
    begin
      if AIndex + 2 < Length(FSections[ASection].Elements) then
      begin
        token := FSections[ASection].Elements[AIndex+2].Token;
        if (token in [nftZeroDecs, nftOptDecs, nftSpaceDecs]) then
        begin
          ANextIndex := AIndex + 3;
          ADecimals := FSections[ASection].Elements[AIndex+2].IntValue;
          if (token <> nftZeroDecs) then
            ANumFormat := nfCustom;
          exit;
        end;
      end;
    end else
    if IsTokenAt(nftSpace, ASection, AIndex+1) then
    begin
      ANumFormat := nfFraction;
      ANextIndex := AIndex + 1;
      exit;
    end else
    begin
      ANextIndex := AIndex + 1;
      exit;
    end;
  end;

  ANextIndex := AIndex;
  Result := false;
end;

function TsNumFormatParser.IsTextAt(AText: String; ASection, AIndex: Integer): Boolean;
begin
  Result := IsTokenAt(nftText, ASection, AIndex) and
    (FSections[ASection].Elements[AIndex].TextValue = AText);
end;
   }
   
{@@ Returns @true if the format elements contain only time, no date tokens. }
function TsNumFormatParser.IsTimeFormat: Boolean;
var
  section: TsNumFormatSection;
begin
  for section in FSections do
    if (nfkTime in section.Kind) then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

  {
function TsNumFormatParser.IsTokenAt(AToken: TsNumFormatToken;
  ASection, AIndex: Integer): Boolean;
begin
  Result := (ASection < Length(FSections)) and
            (AIndex < Length(FSections[ASection].Elements)) and
            (FSections[ASection].Elements[AIndex].Token = AToken);
end;
   }
   
{@@ Limits the decimals to 0 or 2, as required by Excel2. }
procedure TsNumFormatParser.LimitDecimals;
var
  i, j: Integer;
begin
  for j:=0 to High(FSections) do
    for i:=0 to High(FSections[j].Elements) do
      if FSections[j].Elements[i].Token = nftZeroDecs then
        if FSections[j].Elements[i].IntValue > 0 then
          FSections[j].Elements[i].IntValue := 2;
end;

function TsNumFormatParser.NextToken: Char;
begin
  if FCurrent < FEnd then begin
    inc(FCurrent);
    Result := FCurrent^;
  end else
    Result := #0;
end;

function TsNumFormatParser.PrevToken: Char;
begin
  if FCurrent > nil then begin
    dec(FCurrent);
    Result := FCurrent^;
  end else
    Result := #0;
end;

procedure TsNumFormatParser.Parse(const AFormatString: String);
begin
  FStatus := psOK;

  AddSection;
  if (AFormatString = '') then
  begin
    AddElement(nftGeneral);
    exit;
  end;

  FStart := @AFormatString[1];
  FEnd := FStart + Length(AFormatString);
  FCurrent := FStart;
  FToken := FCurrent^;
  while (FCurrent < FEnd) and (FStatus = psOK) do begin
    case FToken of
      'G','g': ScanGeneral;
      '[': ScanBrackets;
      '"': ScanQuotedText;
      ':': AddElement(nftDateTimeSep, ':');
      ';': AddSection;
      else ScanFormat;
    end;
    FToken := NextToken;
  end;
end;

{@@ Scans an AM/PM sequence (or AMPM or A/P).
  At exit, cursor is a next character }
procedure TsNumFormatParser.ScanAMPM;
var
  s: String;
  el: Integer;
begin
  s := '';
  while (FCurrent < FEnd) do begin
    if (FToken in ['A', 'a', 'P', 'p', 'm', 'M', '/']) then
      s := s + FToken
    else
      break;
    FToken := NextToken;
  end;
  if s <> '' then
  begin
    AddElement(nftAMPM, s);
    // Tag the hour element for AM/PM format needed
    el := High(FSections[FCurrSection].Elements)-1;
    for el := High(FSections[FCurrSection].Elements)-1 downto 0 do
      if FSections[FCurrSection].Elements[el].Token = nftHour then
      begin
        FSections[FCurrSection].Elements[el].TextValue := 'AM';
        break;
      end;
  end;
end;

{@@ Counts the number of characters equal to ATestChar. Stops at the next
  different character. This is also where the cursor is at exit. }
procedure TsNumFormatParser.ScanAndCount(ATestChar: Char; out ACount: Integer);
begin
  ACount := 0;
  if FToken <> ATestChar then
    exit;
  repeat
    inc(ACount);
    FToken := NextToken;
  until (FToken <> ATestChar) or (FCurrent >= FEnd);
end;

{@@ Extracts the text between square brackets. This can be
  - a time duration like [hh]
  - a condition, like [>= 2.0]
  - a currency symbol like [$€-409]
  - a color like [red] or [color25]
  The procedure is left with the cursor at ']' }
procedure TsNumFormatParser.ScanBrackets;
var
  s: String;
  n: Integer;
  prevtok: Char;
  isText: Boolean;
begin
  s := '';
  isText := false;
  FToken := NextToken;   // Cursor was at '['
  while (FCurrent < FEnd) and (FStatus = psOK) do begin
    case FToken of
      'h', 'H', 'm', 'M', 'n', 'N', 's', 'S':
        if isText then
          s := s + FToken
        else
        begin
          prevtok := FToken;
          ScanAndCount(FToken, n);
          if (FToken in [']', #0]) then begin
            case prevtok of
              'h', 'H'          : AddElement(nftHour, -n);
              'm', 'M', 'n', 'N': AddElement(nftMinute, -n);
              's', 'S'          : AddElement(nftSecond, -n);
            end;
            break;
          end else
           FStatus := psErrUnknownInfoInBrackets;
        end;

      '<', '>', '=':
        begin
          ScanCondition(FToken);
          if FToken = ']' then
            break
          else
           FStatus := psErrUnknownInfoInBrackets;
        end;

      '$':
        begin
          ScanCurrSymbol;
          if FToken = ']' then
            break
          else
           FStatus := psErrUnknownInfoInBrackets;
        end;

      ']':
        begin
          AnalyzeColor(s);
          break;
        end;

      else
        s := s + FToken;
        isText := true;
    end;
    FToken := NextToken;
  end;
end;

{@@ Scans a condition like [>=2.0]. Starts after the "[" and ends before at "]".
  Returns first character after the number (spaces allowed). }
procedure TsNumFormatParser.ScanCondition(AFirstChar: Char);
var
  s: String;
//  op: TsCompareOperation;
  value: Double;
  res: Integer;
begin
  s := AFirstChar;
  FToken := NextToken;
  if FToken in ['>', '<', '='] then s := s + FToken else FToken := PrevToken;
  {
  if s = '=' then op := coEqual else
  if s = '<>' then op := coNotEqual else
  if s = '<' then op := coLess else
  if s = '>' then op := coGreater else
  if s = '<=' then op := coLessEqual else
  if s = '>=' then op := coGreaterEqual
  else begin
    FStatus := psErrUnknownInfoInBrackets;
    FToken := #0;
    exit;
  end;
    }
  while (FToken = ' ') and (FCurrent < FEnd) do
    FToken := NextToken;

  if FCurrent >= FEnd then begin
    FStatus := psErrUnknownInfoInBrackets;
    FToken := #0;
    exit;
  end;

  s := FToken;
  while (FCurrent < FEnd) and (FToken in ['+', '-', '.', '0'..'9']) do begin
    FToken := NextToken;
    s := s + FToken;
  end;
  val(s, value, res);
  if res <> 0 then begin
    FStatus := psErrUnknownInfoInBrackets;
    FToken := #0;
    exit;
  end;

  while (FCurrent < FEnd) and (FToken = ' ') do
    FToken := NextToken;
  if FToken = ']' then
    AddElement(nftCompareOp, value)
  else begin
    FStatus := psErrUnknownInfoInBrackets;
    FToken := #0;
  end;
end;

{@@ Scans to end of a symbol like [$EUR-409], starting after the $ and ending at
  the "]".
  After the "$" follows the currency symbol, after the "-" country information }
procedure TsNumFormatParser.ScanCurrSymbol;
var
  s: String;
begin
  s := '';
  FToken := NextToken;
  while (FCurrent < FEnd) and not (FToken in ['-', ']']) do begin
    s := s + FToken;
    FToken := NextToken;
  end;
  if s <> '' then
    AddElement(nftCurrSymbol, s);
  if FToken <> ']' then begin
    FToken := NextToken;
    while (FCurrent < FEnd) and (FToken <> ']') do begin
      s := s + FToken;
      FToken := NextToken;
    end;
    if s <> '' then
      AddElement(nftCountry, s);
  end;
end;

{@@ Scans a date/time format. Procedure is left with the cursor at the last char
  of the date/time format. }
procedure TsNumFormatParser.ScanDateTime;
var
  n: Integer;
  token: Char;
begin
  while (FCurrent < FEnd) and (FStatus = psOK) do begin
    case FToken of
      '\':  // means that the next character is taken literally
        begin
          FToken := NextToken;     // skip the "\"...
          AddElement(nftEscaped, FToken);
          FToken := NextToken;
        end;
      'Y', 'y':
        begin
          ScanAndCount(FToken, n);
          AddElement(nftYear, n);
        end;
      'm', 'M', 'n', 'N':
        begin
          token := FToken;
          ScanAndCount(FToken, n);
          AddElement(nftMonthMinute, n, token);  // Decide on minute or month later
        end;
      'D', 'd':
        begin
          ScanAndCount(FToken, n);
          AddElement(nftDay, n);
        end;
      'H', 'h':
        begin
          ScanAndCount(FToken, n);
          AddElement(nftHour, n);
        end;
      'S', 's':
        begin
          ScanAndCount(FToken, n);
          AddElement(nftSecond, n);
        end;
      '/', ':':
        begin
          AddElement(nftDateTimeSep, FToken);
          FToken := NextToken;
        end;
      '.':
        begin
          {
          AddElement(nftDecSep, FToken);
          FToken := NextToken;
          if FToken in ['z', 'Z', '0'] then
          begin
            ScanAndCount(FToken, n);
            AddElement(nftMilliseconds, n);
          end;
          }

          token := NextToken;
          if token in ['z', 'Z', '0'] then begin
            AddElement(nftDecSep, FToken);
            FToken := NextToken;
            if FToken in ['z', 'Z', '0'] then
              ScanAndCount(FToken, n)
            else
              n := 0;
            AddElement(nftMilliseconds, n+1);
          end else begin
            AddElement(nftDateTimeSep, FToken);
            FToken := token;
          end;
        end;
      '[':
        begin
          ScanBrackets;
          FToken := NextToken;
        end;
      'A', 'a':
        ScanAMPM;
      ',', '-':
        begin
          AddElement(nftText, FToken);
          FToken := NextToken;
        end
      else
        // char pointer must be at end of date/time mask.
        FToken := PrevToken;
        Exit;
    end;
  end;
end;

procedure TsNumFormatParser.ScanFormat;
var
  done: Boolean;
  n: Integer;
  uch: Cardinal;
begin
  done := false;
  while (FCurrent < FEnd) and (FStatus = psOK) and (not done) do begin
    case FToken of
      '\': // Excel: add next character literally
        begin
          FToken := NextToken;
          AddElement(nftText, FToken);
        end;
      '*':  // Excel: repeat next character to fill cell. For accounting format.
        begin
          FToken := NextToken;
          AddElement(nftRepeat, FToken);
        end;
      '_':  // Excel: Leave width of next character empty
        begin
          FToken := NextToken;
          uch := UTF8CharacterToUnicode(FCurrent, n);                           // wp: Why Unicode ???
          if n > 1 then
          begin
            AddElement(nftEmptyCharWidth, UnicodeToUTF8(uch));
            inc(FCurrent, n-1);
            FToken := NextToken;
            Continue;
          end else
            AddElement(nftEmptyCharWidth, FToken);
        end;
      '@':  // Excel: Indicates text format
        begin
          AddElement(nftTextFormat, FToken);
        end;
      '"':
        ScanQuotedText;
      '(', ')':
        AddElement(nftSignBracket, FToken);
      '0', '#', '?', '.', ',', '-':
        ScanNumber;
      'y', 'Y', 'm', 'M',  'd', 'D', 'h', 'H', 'N', 'n', 's':
        ScanDateTime;
      '[':
        ScanBrackets;
      '%':
        AddElement(nftPercent, FToken);
      ' ':
        AddElement(nftSpace, FToken);
      'A', 'a':
        begin
          ScanAMPM;
          FToken := PrevToken;
        end;
      'G', 'g':
        ScanGeneral;
      ';':  // End of the section. Important: Cursor must stay on ';'
        begin
          AddSection;
          Exit;
        end;
      else
        uch := UTF8CharacterToUnicode(FCurrent, n);
        if n > 1 then
        begin
          AddElement(nftText, UnicodeToUTF8(uch));
          inc(FCurrent, n-1);
        end else
          AddElement(nftText, FToken);
    end;
    FToken := NextToken;
  end;
end;

{@@ Scans for the word "General", it may be used like other tokens }
procedure TsNumFormatParser.ScanGeneral;
begin
  FStatus := psErrGeneralExpected;
  FToken := NextToken;
  if not (FToken in ['e', 'E']) then exit;
  FToken := NextToken;
  if not (FToken in ['n', 'N']) then exit;
  FToken := NextToken;
  if not (FToken in ['e', 'E']) then exit;
  FToken := NextToken;
  if not (FToken in ['r', 'R']) then exit;
  FToken := NextToken;
  if not (FToken in ['a', 'A']) then exit;
  FToken := NextToken;
  if not (FToken in ['l', 'L']) then exit;
  AddElement(nftGeneral);
  FStatus := psOK;
end;

{@@ Scans a floating point format. Procedure is left with the cursor at the last
  character of the format. }
procedure TsNumFormatParser.ScanNumber;
var
  hasDecSep: Boolean;
  isFrac: Boolean;
  n, m: Integer;
  el: Integer;
  savedCurrent: PChar;
  thSep: Char;
begin
  hasDecSep := false;
  isFrac := false;
  thSep := ',';
  while (FCurrent < FEnd) and (FStatus = psOK) do begin
    case FToken of
      ',': AddElement(nftThSep, ',');
      '.': begin
             AddElement(nftDecSep, '.');
             hasDecSep := true;
           end;
      '#': begin
             ScanAndCount('#', n);
             savedCurrent := FCurrent;
             if not (hasDecSep or isFrac) and (n = 1) and (FToken = thSep) then
             begin
               m := 0;
               FToken := NextToken;
               ScanAndCount('#', n);
               case n of
                 0: begin
                      ScanAndCount('0', n);
                      ScanAndCount(thSep, m);
                      FToken := prevToken;
                      if n = 3 then
                        AddElement(nftIntTh, 3, ',')
                      else
                        FCurrent := savedCurrent;
                    end;
                 1: begin
                      ScanAndCount('0', n);
                      ScanAndCount(thSep, m);
                      FToken := prevToken;
                      if n = 2 then
                        AddElement(nftIntTh, 2, ',')
                      else
                        FCurrent := savedCurrent;
                    end;
                 2: begin
                      ScanAndCount('0', n);
                      ScanAndCount(thSep, m);
                      FToken := prevToken;
                      if (n = 1) then
                        AddElement(nftIntTh, 1, ',')
                      else
                        FCurrent := savedCurrent;
                    end;
               end;
               if m > 0 then
                 AddElement(nftFactor, m, thSep);
             end else
             begin
               FToken := PrevToken;
               if isFrac then
                 AddElement(nftFracDenomOptDigit, n)
               else
               if hasDecSep then
                 AddElement(nftOptDecs, n)
               else
                 AddElement(nftIntOptDigit, n);
             end;
           end;
      '0': begin
             ScanAndCount('0', n);
             ScanAndCount(thSep, m);
             FToken := PrevToken;
             if hasDecSep then
               AddElement(nftZeroDecs, n)
             else
             if isFrac then
               AddElement(nftFracDenomZeroDigit, n)
             else
               AddElement(nftIntZeroDigit, n);
             if m > 0 then
               AddElement(nftFactor, m, thSep);
           end;
      '1'..'9':
           begin
             if isFrac then
             begin
               n := 0;
               while (FToken in ['1'..'9','0']) do
               begin
                 n := n*10 + StrToInt(FToken);
                 FToken := nextToken;
               end;
               AddElement(nftFracDenom, n);
             end else
               AddElement(nftText, FToken);
           end;
      '?': begin
             ScanAndCount('?', n);
             FToken := PrevToken;
             if hasDecSep then
               AddElement(nftSpaceDecs, n)
             else
             if isFrac then
               AddElement(nftFracDenomSpaceDigit, n)
             else
               AddElement(nftIntSpaceDigit, n);
           end;
      'E', 'e':
           begin
             AddElement(nftExpChar, FToken);
             FToken := NextToken;
             if FToken in ['+', '-'] then
               AddElement(nftExpSign, FToken);
             FToken := NextToken;
             if FToken = '0' then begin
               ScanAndCount('0', n);
               FToken := PrevToken;
               AddElement(nftExpDigits, n);
             end;
           end;
      '+', '-':
           AddElement(nftSign, FToken);
      '%': AddElement(nftPercent, FToken);
      '/': begin
             isFrac := true;
             AddElement(nftFracSymbol, FToken);
             // go back and replace correct token for numerator
             el := High(FSections[FCurrSection].Elements);
             while el > 0 do begin
               dec(el);
               case FSections[FCurrSection].Elements[el].Token of
                 nftIntOptDigit:
                   begin
                     FSections[FCurrSection].Elements[el].Token := nftFracNumOptDigit;
                     break;
                   end;
                 nftIntSpaceDigit:
                   begin
                     FSections[FCurrSection].Elements[el].Token := nftFracNumSpaceDigit;
                     break;
                   end;
                 nftIntZeroDigit:
                   begin
                     FSections[FCurrSection].Elements[el].Token := nftFracNumZeroDigit;
                     break;
                   end;
               end;
             end;
           end;
      'G', 'g':
           ScanGeneral;
      else
           FToken := PrevToken;
           Exit;
    end;
    FToken := NextToken;
  end;
end;

{@@ Scans a text in quotation marks. Tries to interpret the text as a currency
  symbol (--> AnalyzeText).
  The procedure is entered and left with the cursor at a quotation mark. }
procedure TsNumFormatParser.ScanQuotedText;
var
  s: String;
begin
  s := '';
  FToken := NextToken;   // Cursor war at '"'
  while (FCurrent < FEnd) and (FStatus = psOK) do begin
    if FToken = '"' then begin
      if AnalyzeCurrency(s) then
        AddElement(nftCurrSymbol, s)
      else
        AddElement(nftText, s);
      exit;
    end else begin
      s := s + FToken;
      FToken := NextToken;
    end;
  end;
  // When the procedure gets here the final quotation mark is missing
  FStatus := psErrQuoteExpected;
end;

procedure TsNumFormatParser.SetDecimals(AValue: Byte);
var
  i, j, n: Integer;
  foundDecs: Boolean;
begin
  foundDecs := false;
  for j := 0 to High(FSections) do begin
    n := Length(FSections[j].Elements);
    i := n-1;
    while (i > -1) do begin
      case FSections[j].Elements[i].Token of
        nftDecSep:                  // this happens, e.g., for "0.E+00"
          if (AValue > 0) and not foundDecs then begin
            InsertElement(j, i, nftZeroDecs, AValue);
            break;
          end;
        nftIntOptDigit, nftIntZeroDigit, nftIntSpaceDigit, nftIntTh:
          // no decimals so far --> add decimal separator and decimals element
          if (AValue > 0) then begin
            // Don't use "AddElements" because nfCurrency etc have elements after the number.
            InsertElement(j, i, nftDecSep, '.');
            InsertElement(j, i+1, nftZeroDecs, AValue);
            break;
          end;
        nftZeroDecs, nftOptDecs, nftSpaceDecs:
          begin
            foundDecs := true;
            if AValue > 0 then begin
              // decimals are already used, just replace value of decimal places
              FSections[j].Elements[i].IntValue := AValue;
              FSections[j].Elements[i].Token := nftZeroDecs;
              break;
            end else begin
              // No decimals any more: delete decs and decsep elements
              DeleteElement(j, i);
              DeleteElement(j, i-1);
              break;
            end;
          end;
      end;
      dec(i);
    end;
  end;
end;

end.
